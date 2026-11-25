# Full-cycle compact window overrides shared by unit fixtures.
qm_test__defaults <- list(
    seasonal_window_days = 365L,
    target_year_days = 365L,
    min_samples = 2L
)

test_that("empirical CDF conventions make ties and tails explicit", {
    anchors <- quantile__cdf_anchors(c(1, 1, 2, 4))

    expect_equal(anchors$value, c(1, 2, 4))
    expect_equal(anchors$probability, c(1 / 6, 2 / 3, 1))

    mapped <- qm__map_continuous(
        historical = c(1, 1, 2, 4),
        observed = c(10, 20, 30, 40),
        future = c(-1, 1, 2, 4, 6)
    )
    expect_equal(mapped$value, c(10, 15, 30, 40, 40))
    expect_identical(
        mapped$lower_tail,
        c(TRUE, FALSE, FALSE, FALSE, FALSE)
    )
    expect_identical(
        mapped$upper_tail,
        c(FALSE, FALSE, FALSE, FALSE, TRUE)
    )
    expect_identical(mapped$tied_historical_values, 1L)
    expect_identical(mapped$tied_observed_values, 0L)
})

test_that("Quantile Mapping returns a typed future-backbone daily series", {
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
        c(5, 15, -10, 40)
    )

    execution <- signal_test__execute(
        qm__component(),
        "tas",
        observed,
        historical,
        future,
        default_overrides = qm_test__defaults
    )
    adjusted <- execution@values[[1L]]

    expect_true(S7::S7_inherits(execution, SignalExecutionResult))
    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_equal(adjusted@data$value, c(15, 25, 10, 40))
    expect_identical(
        adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
        future[BIAS_DAILY_SERIES_COLUMNS[-2L]]
    )
    expect_identical(adjusted@output_role, "model_future")
    expect_identical(adjusted@transformation, "quantile_mapping")
    expect_identical(adjusted@provenance$output_backbone, "model_future")
    expect_identical(adjusted@provenance$mapping_type, "nonparametric")
    expect_identical(adjusted@provenance$detrending, "none")
    expect_identical(
        adjusted@provenance$empirical_conventions$cdf,
        "linear_interpolation"
    )
    expect_identical(
        adjusted@provenance$diagnostics$lower_tail_values,
        1L
    )
    expect_identical(
        adjusted@provenance$diagnostics$upper_tail_values,
        1L
    )
    expect_identical(execution@diagnostics$status, "ok")
})

test_that("Quantile Mapping preserves identity across native CF calendars", {
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
            qm__component(),
            "tas",
            observed,
            historical,
            future,
            default_overrides = qm_test__defaults
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

test_that("circular windows bridge the annual-phase boundary", {
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
    settings <- signal_test__profile_settings(qm__profiles(), "tas")
    settings$seasonal_window_days <- 3L
    settings$min_samples <- 3L

    adjusted <- component__execute(
        qm__component(),
        "apply_group",
        inputs = boundary$group@inputs,
        settings = list(tas = settings),
        key = boundary$group@key
    )

    expect_equal(adjusted@data$value[c(1L, 365L)], c(11, 375))
    expect_equal(
        adjusted@provenance$diagnostics$observed_window_samples,
        c(minimum = 3, median = 3, maximum = 3)
    )
})

test_that("bounds clip mapped values and are retained in diagnostics", {
    observed <- signal_test__series("hurs", 2001L, c(-10, 25, 75, 120))
    historical <- signal_test__series("hurs", 1991L, c(0, 1, 2, 3))
    future <- signal_test__series("hurs", 2061L, c(0, 1, 2, 3))

    execution <- signal_test__execute(
        qm__component(),
        "hurs",
        observed,
        historical,
        future,
        default_overrides = qm_test__defaults
    )
    adjusted <- execution@values[[1L]]

    expect_equal(adjusted@data$value, c(0, 25, 75, 100))
    expect_identical(
        adjusted@provenance$diagnostics$clipped_values,
        2L
    )
    expect_identical(adjusted@settings$bounds, c(0, 100))
    expect_identical(
        execution@profiles$hurs$metadata$default_source,
        "package_implementation"
    )
})

test_that("precipitation hurdle mapping has deterministic dry-day control", {
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

    first <- signal_test__execute(
        qm__component(),
        "pr",
        observed,
        historical,
        future,
        overrides = list(dry_threshold = 0, random_seed = 99L),
        default_overrides = qm_test__defaults
    )
    second <- signal_test__execute(
        qm__component(),
        "pr",
        observed,
        historical,
        future,
        overrides = list(dry_threshold = 0, random_seed = 99L),
        default_overrides = qm_test__defaults
    )
    different <- signal_test__execute(
        qm__component(),
        "pr",
        observed,
        historical,
        future,
        overrides = list(dry_threshold = 0, random_seed = 100L),
        default_overrides = qm_test__defaults
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
    expect_identical(precipitation$input_dry_values, count)
    expect_identical(precipitation$randomized_dry_values, count)
    expect_true(precipitation$output_dry_values < count)
    expect_identical(precipitation$random_seed, 99L)
    expect_identical(
        precipitation$random_generator,
        "park_miller_16807"
    )
})

test_that("Quantile Mapping rejects insufficient circular-window samples", {
    observed <- signal_test__series("tas", 2001L, c(1, 2, 3, 4))
    historical <- signal_test__series("tas", 1991L, c(1, 2, 3, 4))
    future <- signal_test__series("tas", 2061L, c(1, 2, 3, 4))
    boundary <- signal_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- signal_test__profile_settings(qm__profiles(), "tas")

    expect_error(
        component__execute(
            qm__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(tas = settings),
            key = list()
        ),
        "fewer than 10"
    )
})

# vim: fdm=marker :
