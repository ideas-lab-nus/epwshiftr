# Use the shared native-calendar series and signal-boundary fixtures while
# retaining the QDM names used throughout this test module.
qdm_test__series <- signal_test__series
qdm_test__execution_inputs <- signal_test__execution_inputs

# Execute compact fixtures through the common signal lifecycle with a
# full-annual seasonal pool and an explicit minimum sample count.
qdm_test__execute <- function(
    variable,
    observed,
    historical,
    future,
    overrides = list(),
    key = list(site = "A"),
    warn_experimental = FALSE
) {
    boundary <- qdm_test__execution_inputs(
        observed,
        historical,
        future,
        key
    )
    settings <- utils::modifyList(
        list(
            seasonal_window_days = 365L,
            future_window_years = 31L,
            target_year_days = 365L,
            min_samples = 2L
        ),
        overrides
    )
    component__execute(
        qdm__component(),
        "apply",
        inputs = boundary$inputs,
        groups = list(boundary$group),
        overrides = stats::setNames(list(settings), variable),
        warn_experimental = warn_experimental
    )
}

# Retrieve one default profile by variable for direct kernel validation tests.
qdm_test__settings <- function(variable) {
    profiles <- qdm__profiles()
    index <- which(vapply(
        profiles,
        function(profile) identical(profile@variable_id, variable),
        logical(1L)
    ))
    profiles[[index]]@settings
}

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
    observed <- qdm_test__series(
        "tas",
        2001L,
        c(10, 20, 30, 40)
    )
    historical <- qdm_test__series(
        "tas",
        1991L,
        c(0, 10, 20, 30)
    )
    future <- qdm_test__series(
        "tas",
        2061L,
        c(5, 15, 25, 35)
    )

    execution <- qdm_test__execute(
        "tas",
        observed,
        historical,
        future
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
    observed <- qdm_test__series("tas", 2001L, c(0, 20, 40))
    historical <- qdm_test__series("tas", 1991L, c(0, 10, 20))
    future <- qdm_test__series("tas", 2061L, c(10, 20, 30))
    qdm <- qdm_test__execute(
        "tas",
        observed,
        historical,
        future
    )@values[[1L]]@data$value
    boundary <- qdm_test__execution_inputs(
        observed,
        historical,
        future
    )
    qm <- component__execute(
        qm__component(),
        "apply",
        inputs = boundary$inputs,
        groups = list(boundary$group),
        overrides = list(tas = list(
            seasonal_window_days = 365L,
            target_year_days = 365L,
            min_samples = 2L
        )),
        warn_experimental = FALSE
    )@values[[1L]]@data$value

    expect_equal(future$value - historical$value, rep.int(10, 3L))
    expect_equal(qdm - observed$value, rep.int(10, 3L))
    expect_false(isTRUE(all.equal(qm - observed$value, rep.int(10, 3L))))
})

test_that("QDM preserves identity across native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    values <- c(2, 2, 4, 7, 9)

    for (calendar in calendars) {
        observed <- qdm_test__series(
            "tas",
            2000L,
            values,
            calendar
        )
        historical <- qdm_test__series(
            "tas",
            1990L,
            values,
            calendar
        )
        future <- qdm_test__series(
            "tas",
            2060L,
            values,
            calendar
        )
        execution <- qdm_test__execute(
            "tas",
            observed,
            historical,
            future
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
    observed <- qdm_test__series(
        "tas",
        2001L,
        seq_len(365L) + 10
    )
    historical <- qdm_test__series(
        "tas",
        1991L,
        seq_len(365L)
    )
    future <- qdm_test__series(
        "tas",
        2061L,
        seq_len(365L)
    )
    boundary <- qdm_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- qdm_test__settings("tas")
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
    observed <- qdm_test__series("pr", 2001L, c(2, 4, 8, 16))
    historical <- qdm_test__series("pr", 1991L, c(1, 2, 4, 8))
    future <- qdm_test__series("pr", 2061L, c(2, 4, 8, 16))

    adjusted <- qdm_test__execute(
        "pr",
        observed,
        historical,
        future
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
    observed <- qdm_test__series(
        "pr",
        2001L,
        c(rep.int(0, 8L), seq_len(32L))
    )
    historical <- qdm_test__series(
        "pr",
        1991L,
        c(rep.int(0, 20L), seq_len(20L))
    )
    future <- qdm_test__series(
        "pr",
        2061L,
        rep.int(0, count)
    )
    set.seed(2026)
    seed_before <- .Random.seed
    overrides <- list(dry_threshold = 0.5, random_seed = 99L)

    first <- qdm_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = overrides
    )
    second <- qdm_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = overrides
    )
    different <- qdm_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = list(dry_threshold = 0.5, random_seed = 100L)
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
    precipitation <- (
        first@values[[1L]]@provenance$diagnostics$precipitation
    )
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

test_that("QDM rejects unsupported, insufficient, or invalid settings", {
    observed <- qdm_test__series("tas", 2001L, c(1, 2, 3, 4))
    historical <- qdm_test__series("tas", 1991L, c(1, 2, 3, 4))
    future <- qdm_test__series("tas", 2061L, c(1, 2, 3, 4))
    boundary <- qdm_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- qdm_test__settings("tas")

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
        component__execute(
            qdm__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(tas = settings),
            key = list()
        ),
        "fewer than 10"
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
})

test_that("QDM validates precipitation thresholds and non-negative inputs", {
    observed <- qdm_test__series("pr", 2001L, c(0, 1, 2, 3))
    historical <- qdm_test__series("pr", 1991L, c(0, 1, 2, 3))
    future <- qdm_test__series("pr", 2061L, c(0, 1, 2, 3))
    boundary <- qdm_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- qdm_test__settings("pr")
    settings$seasonal_window_days <- 365L
    settings$min_samples <- 2L

    zero_threshold <- settings
    zero_threshold$dry_threshold <- 0
    expect_error(
        component__execute(
            qdm__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(pr = zero_threshold),
            key = list()
        ),
        "positive `dry_threshold`"
    )

    negative <- boundary$group@inputs
    negative$model_future$value[[1L]] <- -1
    expect_error(
        component__execute(
            qdm__component(),
            "apply_group",
            inputs = negative,
            settings = list(pr = settings),
            key = list()
        ),
        "non-negative"
    )
})

test_that("QDM profiles retain evidence and component registration", {
    qdm__register_component()
    component <- component__get(
        "signal",
        "quantile_delta_mapping_daily"
    )
    profiles <- component@metadata$signal_profiles

    expect_true(S7::S7_inherits(component, WeatherComponentSpec))
    expect_identical(component@stage, "signal")
    expect_identical(
        component@input_kinds,
        "calendar_indexed_daily_series"
    )
    expect_identical(component@output_kinds, "daily_adjusted_series")
    expect_identical(component@scopes, "univariate")
    expect_true(component@stochastic)
    expect_identical(
        sort(names(profiles)),
        sort(c(
            QDM_PUBLISHED_VARIABLES,
            QDM_EXPERIMENTAL_VARIABLES
        ))
    )
    expect_identical(profiles$tas$evidence, "published")
    expect_identical(profiles$pr$evidence, "published")
    expect_identical(profiles$hurs$evidence, "experimental")
    expect_identical(
        profiles$hurs$metadata$default_source,
        "package_implementation"
    )

    calendar <- component__spec(
        name = "qdm_calendar_test",
        stage = "calendar",
        input_kinds = "preprocessed_daily_series",
        output_kinds = "calendar_indexed_daily_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "qdm_sequence_test",
        stage = "sequence",
        input_kinds = "daily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
