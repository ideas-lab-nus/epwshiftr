# Use the shared native-calendar series and signal-boundary fixtures while
# retaining the SDM names used throughout this test module.
sdm_test__series <- signal_test__series
sdm_test__execution_inputs <- signal_test__execution_inputs

# Execute compact fixtures through the common signal lifecycle with one
# retained year and explicit sample requirements.
sdm_test__execute <- function(
  variable,
  observed,
  historical,
  future,
  overrides = list(),
  key = list(site = "A"),
  warn_experimental = FALSE
) {
    boundary <- sdm_test__execution_inputs(
        observed,
        historical,
        future,
        key
    )
    settings <- utils::modifyList(
        list(
            future_window_years = 1L,
            output_block_years = 1L,
            min_samples = 2L
        ),
        overrides
    )
    component__execute(
        sdm__component(),
        "apply",
        inputs = boundary$inputs,
        groups = list(boundary$group),
        overrides = stats::setNames(list(settings), variable),
        warn_experimental = warn_experimental
    )
}

# Retrieve one default profile by variable for direct settings validation.
sdm_test__settings <- function(variable) {
    profiles <- sdm__profiles()
    index <- which(vapply(
        profiles,
        function(profile) identical(profile@variable_id, variable),
        logical(1L)
    ))
    profiles[[index]]@settings
}

test_that("parametric distribution helpers fit and invert supported families", {
    normal_values <- c(-2, -1, 0, 1, 2)
    normal <- distribution__fit(normal_values, "normal")
    gamma <- distribution__fit(c(0.5, 1, 2, 3, 5, 8), "gamma")
    probabilities <- c(0.1, 0.5, 0.9)

    expect_identical(normal$family, "normal")
    expect_equal(normal$parameters$location, 0)
    expect_equal(
        normal$parameters$scale,
        sqrt(mean(normal_values^2))
    )
    expect_identical(gamma$family, "gamma")
    expect_gt(gamma$parameters$shape, 0)
    expect_gt(gamma$parameters$scale, 0)
    expect_equal(
        gamma$parameters$shape * gamma$parameters$scale,
        mean(c(0.5, 1, 2, 3, 5, 8))
    )
    expect_equal(
        log(gamma$parameters$shape) -
            digamma(gamma$parameters$shape),
        log(mean(c(0.5, 1, 2, 3, 5, 8))) -
            mean(log(c(0.5, 1, 2, 3, 5, 8))),
        tolerance = 1e-8
    )
    expect_equal(
        distribution__cdf(
            normal,
            distribution__quantile(normal, probabilities)
        ),
        probabilities
    )
    expect_equal(
        distribution__cdf(
            gamma,
            distribution__quantile(gamma, probabilities)
        ),
        probabilities
    )
    expect_error(
        distribution__fit_gamma(c(1, 1, 1)),
        "constant or numerically degenerate"
    )
})

test_that("SDM reproduces the published wet-day and recurrence equations", {
    wet_days <- sdm__expected_wet_days(
        future_wet = 593L,
        future_total = 900L,
        observed_wet = 434L,
        observed_total = 900L,
        historical_wet = 525L,
        historical_total = 900L
    )
    scaled <- sdm__scaled_probability(
        observed_probability = 1 - 1 / 1667,
        historical_probability = 1 - 1 / 2000,
        future_probability = 1 - 1 / 385,
        tails = "one",
        epsilon = 1e-9
    )

    expect_identical(wet_days$requested, 490L)
    expect_identical(wet_days$retained, 490L)
    expect_false(wet_days$increase_not_supported)
    expect_equal(
        scaled$recurrence_interval,
        1667 * 385 / 2000
    )
    expect_equal(
        scaled$probability,
        1 - 1 / (1667 * 385 / 2000)
    )
    expect_error(
        sdm__expected_wet_days(
            future_wet = 2L,
            future_total = 1L,
            observed_wet = 1L,
            observed_total = 1L,
            historical_wet = 1L,
            historical_total = 1L
        ),
        "cannot exceed"
    )
})

test_that("SDM uses published future windows and retained blocks", {
    blocks <- sdm__future_blocks(
        2001:2100,
        future_window_years = 30L,
        output_block_years = 10L
    )

    expect_identical(blocks[[1L]]$output_years, 2001:2010)
    expect_identical(blocks[[1L]]$window_years, 2001:2020)
    expect_true(blocks[[1L]]$truncated_left)
    expect_identical(blocks[[2L]]$output_years, 2011:2020)
    expect_identical(blocks[[2L]]$window_years, 2001:2030)
    expect_false(blocks[[2L]]$truncated_left)
    expect_false(blocks[[2L]]$truncated_right)
    expect_identical(blocks[[10L]]$output_years, 2091:2100)
    expect_identical(blocks[[10L]]$window_years, 2081:2100)
    expect_true(blocks[[10L]]$truncated_right)
})

test_that("temperature SDM returns a typed future-backbone daily series", {
    pattern <- c(
        0, 3, -1, 4, -2, 2, -3, 1, 5, -4,
        2, -1, 3, -2, 4, 0, -3, 5, 1, -4
    )
    observed <- sdm_test__series("tas", 2001L, 280 + pattern)
    historical <- sdm_test__series("tas", 1991L, 284 + 2 * pattern)
    future <- sdm_test__series("tas", 2061L, 290 + 4 * pattern)

    execution <- sdm_test__execute(
        "tas",
        observed,
        historical,
        future,
        overrides = list(min_samples = 10L)
    )
    adjusted <- execution@values[[1L]]
    adjusted_detrended <- sdm__detrend(
        adjusted@data$value,
        sdm__time_coordinate(adjusted@data)
    )
    observed_detrended <- sdm__detrend(
        observed$value,
        sdm__time_coordinate(observed)
    )
    future_detrended <- sdm__detrend(
        future$value,
        sdm__time_coordinate(future)
    )

    expect_true(S7::S7_inherits(execution, SignalExecutionResult))
    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_equal(
        mean(adjusted@data$value),
        mean(observed$value) +
            mean(future$value) -
            mean(historical$value)
    )
    expect_equal(
        distribution__fit_normal(
            adjusted_detrended$residual
        )$parameters$scale /
            distribution__fit_normal(
                observed_detrended$residual
            )$parameters$scale,
        2
    )
    expect_equal(
        adjusted_detrended$coefficients[["slope"]],
        future_detrended$coefficients[["slope"]]
    )
    expect_identical(
        adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
        future[BIAS_DAILY_SERIES_COLUMNS[-2L]]
    )
    expect_identical(adjusted@output_role, "model_future")
    expect_identical(
        adjusted@transformation,
        "scaled_distribution_mapping"
    )
    expect_identical(
        adjusted@provenance$output_backbone,
        "model_future"
    )
    expect_identical(
        adjusted@provenance$temporal_policy$source,
        "user_override"
    )
    expect_identical(execution@diagnostics$status, "ok")
})

test_that("temperature SDM preserves identity across native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    values <- 280 + c(
        0, 3, -1, 4, -2, 2, -3, 1, 5, -4,
        2, -1, 3, -2, 4, 0, -3, 5, 1, -4
    )

    for (calendar in calendars) {
        observed <- sdm_test__series(
            "tas",
            2001L,
            values,
            calendar
        )
        historical <- sdm_test__series(
            "tas",
            1991L,
            values,
            calendar
        )
        future <- sdm_test__series(
            "tas",
            2061L,
            values,
            calendar
        )
        execution <- sdm_test__execute(
            "tas",
            observed,
            historical,
            future,
            overrides = list(min_samples = 10L)
        )

        expect_equal(
            execution@values[[1L]]@data$value,
            future$value,
            tolerance = 1e-10
        )
        expect_identical(
            execution@values[[1L]]@data$cf_calendar,
            future$cf_calendar
        )
    }
})

test_that("precipitation SDM adjusts wet frequency on future ranks", {
    mm_per_day <- function(value) value / 86400
    observed <- sdm_test__series(
        "pr",
        2001L,
        mm_per_day(c(rep.int(0, 10L), seq_len(10L)))
    )
    historical <- sdm_test__series(
        "pr",
        1991L,
        mm_per_day(c(rep.int(0, 5L), seq_len(15L)))
    )
    future <- sdm_test__series(
        "pr",
        2061L,
        mm_per_day(c(rep.int(0, 8L), seq_len(12L)))
    )
    set.seed(2026)
    seed_before <- .Random.seed

    execution <- sdm_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = list(min_samples = 5L)
    )
    adjusted <- execution@values[[1L]]
    threshold <- SDM_PR_DRY_THRESHOLD
    window <- adjusted@provenance$diagnostics$windows[[1L]]

    expect_identical(.Random.seed, seed_before)
    expect_identical(sum(adjusted@data$value >= threshold), 8L)
    expect_true(all(adjusted@data$value[seq_len(12L)] < threshold))
    expect_true(all(adjusted@data$value[13:20] >= threshold))
    expect_identical(window$expected_wet_days$requested, 8L)
    expect_identical(window$expected_wet_days$retained, 8L)
    expect_false(window$expected_wet_days$increase_not_supported)
    expect_identical(window$adjusted_wet_days, 8L)
    expect_identical(
        window$adjusted_positive_below_threshold_days,
        0L
    )
    expect_identical(
        adjusted@provenance$diagnostics$
            precipitation$wet_day_increase_not_supported_windows,
        0L
    )
})

test_that("precipitation SDM records unsupported wet-day increases", {
    mm_per_day <- function(value) value / 86400
    observed <- sdm_test__series(
        "pr",
        2001L,
        mm_per_day(c(rep.int(0, 5L), seq_len(15L)))
    )
    historical <- sdm_test__series(
        "pr",
        1991L,
        mm_per_day(c(rep.int(0, 15L), seq_len(5L)))
    )
    future <- sdm_test__series(
        "pr",
        2061L,
        mm_per_day(c(rep.int(0, 10L), seq_len(10L)))
    )

    adjusted <- sdm_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = list(min_samples = 5L)
    )@values[[1L]]
    window <- adjusted@provenance$diagnostics$windows[[1L]]

    expect_identical(window$expected_wet_days$requested, 30L)
    expect_identical(window$expected_wet_days$retained, 10L)
    expect_true(window$expected_wet_days$increase_not_supported)
    expect_identical(
        adjusted@provenance$diagnostics$
            precipitation$wet_day_increase_not_supported_windows,
        1L
    )
})

test_that("SDM rejects incompatible settings and invalid inputs", {
    observed <- sdm_test__series(
        "tas",
        2001L,
        c(1, 3, 2, 5, 4)
    )
    historical <- sdm_test__series(
        "tas",
        1991L,
        c(1, 3, 2, 5, 4)
    )
    future <- sdm_test__series(
        "tas",
        2061L,
        c(1, 3, 2, 5, 4)
    )
    boundary <- sdm_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- sdm_test__settings("tas")

    uneven <- settings
    uneven$future_window_years <- 4L
    uneven$output_block_years <- 1L
    expect_error(
        component__execute(
            sdm__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(tas = uneven),
            key = list()
        ),
        "even, non-negative"
    )

    expect_error(
        component__execute(
            sdm__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(tas = settings),
            key = list()
        ),
        "fewer than 10"
    )

    precipitation <- lapply(
        boundary$group@inputs,
        function(data) {
            data$variable_id <- "pr"
            data$units <- "kg m-2 s-1"
            data$value <- abs(data$value) / 86400
            data
        }
    )
    precipitation$model_future$value[[1L]] <- -1
    precipitation_settings <- sdm_test__settings("pr")
    precipitation_settings$future_window_years <- 1L
    precipitation_settings$output_block_years <- 1L
    precipitation_settings$min_samples <- 2L
    expect_error(
        component__execute(
            sdm__component(),
            "apply_group",
            inputs = precipitation,
            settings = list(pr = precipitation_settings),
            key = list()
        ),
        "non-negative"
    )
})

test_that("SDM profiles retain evidence and component registration", {
    sdm__register_component()
    component <- component__get(
        "signal",
        "scaled_distribution_mapping_daily"
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
    expect_false(component@stochastic)
    expect_identical(
        sort(names(profiles)),
        sort(c(
            SDM_PUBLISHED_VARIABLES,
            SDM_EXPERIMENTAL_VARIABLES
        ))
    )
    expect_identical(profiles$tas$evidence, "published")
    expect_identical(profiles$pr$evidence, "published")
    expect_identical(profiles$tasmin$evidence, "experimental")
    expect_identical(
        profiles$tasmin$metadata$default_source,
        "package_implementation"
    )

    calendar <- component__spec(
        name = "sdm_calendar_test",
        stage = "calendar",
        input_kinds = "preprocessed_daily_series",
        output_kinds = "calendar_indexed_daily_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "sdm_sequence_test",
        stage = "sequence",
        input_kinds = "daily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
