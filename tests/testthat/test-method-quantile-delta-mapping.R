# Full-annual seasonal pool and compact minimum sample overrides shared by tests.
qdm_test__defaults <- list(
    seasonal_window_days = 365L,
    future_window_years = 31L,
    target_year_days = 365L,
    min_samples = 2L
)

test_that("QDM transfers absolute and relative quantile changes", {
    absolute <- qdm__map_value(
        observed = c(10, 20, 30),
        historical = c(0, 10, 20),
        future_sample = c(5, 15, 25),
        future_value = 15,
        trend_preservation = "absolute"
    )
    relative <- qdm__map_value(
        observed = c(2, 4, 8),
        historical = c(1, 2, 4),
        future_sample = c(2, 4, 8),
        future_value = 4,
        trend_preservation = "relative"
    )

    expect_equal(absolute$probability, 0.5)
    expect_equal(absolute$change, 5)
    expect_equal(absolute$value, 25)
    expect_equal(relative$probability, 0.5)
    expect_equal(relative$change, 2)
    expect_equal(relative$value, 8)
})

test_that("QDM returns a typed future-backbone daily series", {
    observed <- signal_test__series(
        "tas",
        2001L,
        c(10, 20, 30, 40)
    )
    historical <- signal_test__series(
        "tas",
        1991L,
        c(0, 10, 20, 30)
    )
    future <- signal_test__series(
        "tas",
        2061L,
        c(5, 15, 25, 35)
    )

    execution <- signal_test__execute(
        qdm__component(),
        "tas",
        observed,
        historical,
        future,
        default_overrides = qdm_test__defaults
    )
    adjusted <- execution@values[[1L]]

    expect_true(S7::S7_inherits(execution, SignalExecutionResult))
    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_equal(adjusted@data$value, c(15, 25, 35, 45))
    expect_identical(
        adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
        future[BIAS_DAILY_SERIES_COLUMNS[-2L]]
    )
    expect_identical(adjusted@output_role, "model_future")
    expect_identical(
        adjusted@transformation,
        "quantile_delta_mapping"
    )
    expect_identical(
        adjusted@provenance$output_backbone,
        "model_future"
    )
    expect_identical(adjusted@provenance$mapping_type, "nonparametric")
    expect_identical(adjusted@provenance$zero_denominator_policy, "error")
    expect_identical(
        adjusted@provenance$empirical_conventions$tails,
        "future_window_support"
    )
    expect_identical(
        adjusted@provenance$diagnostics$future_lower_tail_values,
        0L
    )
    expect_identical(
        adjusted@provenance$diagnostics$future_upper_tail_values,
        0L
    )
    expect_identical(execution@diagnostics$status, "ok")
})

test_that("QDM preserves modeled quantile deltas that QM changes", {
    observed <- signal_test__series("tas", 2001L, c(0, 20, 40))
    historical <- signal_test__series("tas", 1991L, c(0, 10, 20))
    future <- signal_test__series("tas", 2061L, c(10, 20, 30))
    qdm <- signal_test__execute(
        qdm__component(),
        "tas",
        observed,
        historical,
        future,
        default_overrides = qdm_test__defaults
    )@values[[1L]]@data$value
    qm <- signal_test__execute(
        qm__component(),
        "tas",
        observed,
        historical,
        future,
        default_overrides = list(
            seasonal_window_days = 365L,
            target_year_days = 365L,
            min_samples = 2L
        )
    )@values[[1L]]@data$value

    expect_equal(future$value - historical$value, rep.int(10, 3L))
    expect_equal(qdm - observed$value, rep.int(10, 3L))
    expect_false(isTRUE(all.equal(qm - observed$value, rep.int(10, 3L))))
})

test_that("QDM preserves identity across native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    values <- c(2, 2, 4, 7, 9)

    for (calendar in calendars) {
        observed <- signal_test__series(
            "tas",
            2000L,
            values,
            calendar
        )
        historical <- signal_test__series(
            "tas",
            1990L,
            values,
            calendar
        )
        future <- signal_test__series(
            "tas",
            2060L,
            values,
            calendar
        )
        execution <- signal_test__execute(
            qdm__component(),
            "tas",
            observed,
            historical,
            future,
            default_overrides = qdm_test__defaults
        )

        expect_equal(
            execution@values[[1L]]@data$value,
            values,
            info = calendar
        )
        expect_identical(
            execution@values[[1L]]@data$cf_calendar,
            rep.int(calendar, length(values)),
            info = calendar
        )
    }
})

test_that("QDM circular windows bridge the annual-phase boundary", {
    observed <- signal_test__series(
        "tas",
        2001L,
        seq_len(365L) + 10
    )
    historical <- signal_test__series(
        "tas",
        1991L,
        seq_len(365L)
    )
    future <- signal_test__series(
        "tas",
        2061L,
        seq_len(365L)
    )
    boundary <- signal_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- signal_test__profile_settings(qdm__profiles(), "tas")
    settings$seasonal_window_days <- 3L
    settings$min_samples <- 3L

    adjusted <- component__execute(
        qdm__component(),
        "apply_group",
        inputs = boundary$group@inputs,
        settings = list(tas = settings),
        key = boundary$group@key
    )

    expect_equal(adjusted@data$value[c(1L, 365L)], c(11, 375))
    expect_equal(
        adjusted@provenance$diagnostics$future_window_samples,
        c(minimum = 3, median = 3, maximum = 3)
    )
})

test_that("QDM future windows are centered and truncate at series edges", {
    years <- 2040:2070

    expect_true(all(qdm__future_year_window(years, 2055L, 31L)))
    expect_identical(
        years[qdm__future_year_window(years, 2040L, 31L)],
        2040:2055
    )
})

test_that("relative QDM preserves positive precipitation quantile ratios", {
    observed <- signal_test__series("pr", 2001L, c(2, 4, 8, 16))
    historical <- signal_test__series("pr", 1991L, c(1, 2, 4, 8))
    future <- signal_test__series("pr", 2061L, c(2, 4, 8, 16))

    adjusted <- signal_test__execute(
        qdm__component(),
        "pr",
        observed,
        historical,
        future,
        default_overrides = qdm_test__defaults
    )@values[[1L]]

    expect_equal(adjusted@data$value, c(4, 8, 16, 32))
    expect_equal(
        adjusted@data$value / observed$value,
        future$value / historical$value
    )
    expect_identical(
        adjusted@settings$trend_preservation,
        "relative"
    )
})

test_that("precipitation censoring is deterministic and RNG-independent", {
    count <- 40L
    observed <- signal_test__series(
        "pr",
        2001L,
        c(rep.int(0, 8L), seq_len(32L))
    )
    historical <- signal_test__series(
        "pr",
        1991L,
        c(rep.int(0, 20L), seq_len(20L))
    )
    future <- signal_test__series(
        "pr",
        2061L,
        rep.int(0, count)
    )
    set.seed(2026)
    seed_before <- .Random.seed
    overrides <- list(dry_threshold = 0.5, random_seed = 99L)

    first <- signal_test__execute(
        qdm__component(),
        "pr",
        observed,
        historical,
        future,
        overrides = overrides,
        default_overrides = qdm_test__defaults
    )
    second <- signal_test__execute(
        qdm__component(),
        "pr",
        observed,
        historical,
        future,
        overrides = overrides,
        default_overrides = qdm_test__defaults
    )
    different <- signal_test__execute(
        qdm__component(),
        "pr",
        observed,
        historical,
        future,
        overrides = list(dry_threshold = 0.5, random_seed = 100L),
        default_overrides = qdm_test__defaults
    )

    first_values <- first@values[[1L]]@data$value
    expect_identical(.Random.seed, seed_before)
    expect_identical(first_values, second@values[[1L]]@data$value)
    expect_false(identical(
        first_values,
        different@values[[1L]]@data$value
    ))
    expect_true(any(first_values == 0))
    expect_true(any(first_values > 0))
    precipitation <- first@values[[1L]]@provenance$diagnostics$precipitation
    expect_identical(
        precipitation$input_censored_values,
        c(
            observed_reference = 8L,
            model_historical = 20L,
            model_future = count
        )
    )
    expect_true(precipitation$output_censored_values > 0L)
    expect_identical(precipitation$random_seed, 99L)
    expect_identical(
        precipitation$random_generator,
        "park_miller_16807"
    )
})

test_that("QDM rejects invalid window, zero quantile, and zero dry threshold", {
    observed <- signal_test__series("tas", 2001L, c(1, 2, 3, 4))
    historical <- signal_test__series("tas", 1991L, c(1, 2, 3, 4))
    future <- signal_test__series("tas", 2061L, c(1, 2, 3, 4))
    boundary <- signal_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- signal_test__profile_settings(qdm__profiles(), "tas")

    even_window <- settings
    even_window$future_window_years <- 30L
    expect_error(
        component__execute(
            qdm__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(tas = even_window),
            key = list()
        ),
        "requires an odd"
    )

    expect_error(
        qdm__map_value(
            observed = c(1, 2),
            historical = c(0, 0),
            future_sample = c(1, 2),
            future_value = 1,
            trend_preservation = "relative"
        ),
        "zero historical-model quantile"
    )

    pr_settings <- signal_test__profile_settings(qdm__profiles(), "pr")
    pr_settings$dry_threshold <- 0
    expect_error(
        qdm__settings(list(pr = pr_settings)),
        "positive `dry_threshold`"
    )
})

# vim: fdm=marker :
