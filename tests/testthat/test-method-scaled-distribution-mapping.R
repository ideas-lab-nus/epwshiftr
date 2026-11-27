# Execute compact SDM fixtures through the shared signal test harness with one
# retained year and explicit sample requirements.
# sdm_test__execute {{{
sdm_test__execute <- function(
    variable,
    observed,
    historical,
    future,
    overrides = list(),
    key = list(site = "A"),
    warn_experimental = FALSE
) {
    signal_test__execute(
        sdm__component(),
        variable = variable,
        observed = observed,
        historical = historical,
        future = future,
        overrides = overrides,
        default_overrides = list(
            future_window_years = 1L,
            output_block_years = 1L,
            min_samples = 2L
        ),
        key = key,
        warn_experimental = warn_experimental
    )
}
# }}}

test_that("parametric distribution helpers fit and invert supported families", {
    normal_values <- c(-2, -1, 0, 1, 2)
    normal <- distribution__fit_normal(normal_values)
    gamma <- distribution__fit_gamma(c(0.5, 1, 2, 3, 5, 8))
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

test_that("temperature SDM returns a typed future-backbone daily series", {
    pattern <- c(
        0,
        3,
        -1,
        4,
        -2,
        2,
        -3,
        1,
        5,
        -4,
        2,
        -1,
        3,
        -2,
        4,
        0,
        -3,
        5,
        1,
        -4
    )
    observed <- signal_test__series("tas", 2001L, 280 + pattern)
    historical <- signal_test__series("tas", 1991L, 284 + 2 * pattern)
    future <- signal_test__series("tas", 2061L, 290 + 4 * pattern)

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
    expect_identical(
        adjusted@provenance$temporal_policy$edge_policy,
        "truncate"
    )
    expect_identical(
        adjusted@provenance$method_conventions$rank_interpolation,
        "linear_normalized_rank"
    )
    expect_identical(execution@diagnostics$status, "ok")
})

test_that("temperature SDM preserves identity across native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    values <- 280 +
        c(
            0,
            3,
            -1,
            4,
            -2,
            2,
            -3,
            1,
            5,
            -4,
            2,
            -1,
            3,
            -2,
            4,
            0,
            -3,
            5,
            1,
            -4
        )

    for (calendar in calendars) {
        observed <- signal_test__series(
            "tas",
            2001L,
            values,
            calendar
        )
        historical <- signal_test__series(
            "tas",
            1991L,
            values,
            calendar
        )
        future <- signal_test__series(
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
    observed <- signal_test__series(
        "pr",
        2001L,
        mm_per_day(c(rep.int(0, 10L), seq_len(10L)))
    )
    historical <- signal_test__series(
        "pr",
        1991L,
        mm_per_day(c(rep.int(0, 5L), seq_len(15L)))
    )
    future <- signal_test__series(
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
        adjusted@provenance$diagnostics$precipitation$wet_day_increase_not_supported_windows,
        0L
    )
})

test_that("precipitation SDM records unsupported wet-day increases", {
    mm_per_day <- function(value) value / 86400
    observed <- signal_test__series(
        "pr",
        2001L,
        mm_per_day(c(rep.int(0, 5L), seq_len(15L)))
    )
    historical <- signal_test__series(
        "pr",
        1991L,
        mm_per_day(c(rep.int(0, 15L), seq_len(5L)))
    )
    future <- signal_test__series(
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
        adjusted@provenance$diagnostics$precipitation$wet_day_increase_not_supported_windows,
        1L
    )
})

test_that("SDM rejects incompatible settings and retains profile metadata", {
    observed <- signal_test__series("tas", 2001L, c(1, 3, 2, 5, 4))
    historical <- signal_test__series("tas", 1991L, c(1, 3, 2, 5, 4))
    future <- signal_test__series("tas", 2061L, c(1, 3, 2, 5, 4))
    boundary <- signal_test__execution_inputs(observed, historical, future)
    settings <- signal_test__profile_settings(sdm__profiles(), "tas")

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

    mismatched_branch <- settings
    mismatched_branch$distribution <- "gamma"
    expect_error(
        sdm__settings(list(tas = mismatched_branch)),
        "Normal distribution and linear detrending"
    )

    profiles <- sdm__component()@metadata$signal_profiles
    expect_identical(
        profiles$tasmin$metadata$default_source,
        "package_implementation"
    )
    expect_identical(
        profiles$pr$metadata$gamma_fit_method,
        "maximum_likelihood_zero_location"
    )
})

# vim: fdm=marker :
