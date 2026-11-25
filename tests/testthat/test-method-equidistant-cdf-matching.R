# Compact sample-threshold overrides shared by EDCDFm unit fixtures.
edcdf_test__defaults <- list(
    min_samples = 2L,
    min_positive_samples = 2L
)

test_that("four-parameter Beta fit uses the Li range convention", {
    values <- c(1, 2, 3, 4, 6, 9)
    fit <- edcdf__fit_beta4(
        values,
        range_extension_sd = 0.5,
        tolerance = 1e-10,
        max_iterations = 1000L
    )

    expect_identical(fit$family, "beta_four_parameter")
    expect_equal(
        fit$parameters$lower,
        min(values) - 0.5 * stats::sd(values)
    )
    expect_equal(
        fit$parameters$upper,
        max(values) + 0.5 * stats::sd(values)
    )
    expect_gt(fit$parameters$shape1, 0)
    expect_gt(fit$parameters$shape2, 0)
    probabilities <- c(0.1, 0.5, 0.9)
    expect_equal(
        edcdf__cdf(
            fit,
            edcdf__quantile(fit, probabilities)
        ),
        probabilities,
        tolerance = 1e-8
    )
    expect_error(
        edcdf__fit_beta4(
            rep(2, 5),
            0.5,
            1e-10,
            1000L
        ),
        "constant"
    )
})

test_that("mixed Gamma fit retains dry mass and positive amounts", {
    values <- c(0, 0, 1, 2, 4, 8)
    fit <- edcdf__fit_mixed_gamma(
        values,
        dry_threshold = 0,
        min_positive_samples = 2L,
        tolerance = 1e-10,
        max_iterations = 1000L
    )

    expect_identical(fit$family, "mixed_gamma")
    expect_equal(fit$parameters$dry_probability, 2 / 6)
    expect_equal(fit$parameters$wet_probability, 4 / 6)
    expect_identical(fit$positive_sample_size, 4L)
    expect_equal(edcdf__quantile(fit, 0.2), 0)
    expect_gt(edcdf__quantile(fit, 0.8), 0)
    positive_probabilities <- c(0.5, 0.7, 0.9)
    expect_equal(
        edcdf__cdf(
            fit,
            edcdf__quantile(fit, positive_probabilities)
        ),
        positive_probabilities,
        tolerance = 1e-8
    )
    expect_error(
        edcdf__fit_mixed_gamma(
            c(0, 0, 1),
            0,
            2L,
            1e-10,
            1000L
        ),
        "at least 2 positive"
    )
})

test_that("Equidistant CDF Matching applies the published additive equation", {
    observed <- c(1, 2, 4, 7, 11, 16)
    historical <- observed + 5
    future <- observed + 12
    resolved <- signal_test__profile_settings(edcdf__profiles(), "tas")
    resolved$min_samples <- 2L
    mapped <- edcdf__map_values(
        observed,
        historical,
        future,
        resolved
    )
    probability <- distribution__clamp_probability(
        edcdf__cdf(mapped$fits$model_future, future),
        resolved$cdf_epsilon
    )
    expected <- future +
        edcdf__quantile(
            mapped$fits$observed_reference,
            probability
        ) -
        edcdf__quantile(
            mapped$fits$model_historical,
            probability
        )

    expect_equal(mapped$value, expected)
    expect_equal(mapped$value, future - 5, tolerance = 1e-6)
    expect_equal(mapped$diagnostics$correction_range, c(-5, -5))
})

test_that("daily adaptation pools native calendar months separately", {
    base <- seq_len(59) + sin(seq_len(59))
    historical_offset <- c(rep(5, 31), rep(10, 28))
    future_offset <- c(rep(15, 31), rep(30, 28))
    observed <- signal_test__series("tas", 2001L, base)
    historical <- signal_test__series(
        "tas",
        1991L,
        base + historical_offset
    )
    future <- signal_test__series(
        "tas",
        2061L,
        base + future_offset
    )
    execution <- signal_test__execute(
        edcdf__component(),
        "tas",
        observed,
        historical,
        future,
        default_overrides = edcdf_test__defaults
    )
    adjusted <- execution@values[[1L]]

    expect_equal(
        adjusted@data$value,
        future$value - historical_offset,
        tolerance = 1e-5
    )
    expect_identical(
        adjusted@provenance$diagnostics$month_count,
        2L
    )
    expect_identical(
        adjusted@provenance$frequency_source,
        "epwshiftr_daily_adaptation"
    )
    expect_identical(
        adjusted@provenance$equation_equivalence,
        "absolute_quantile_delta_mapping"
    )
    expect_identical(
        execution@profiles$tas$metadata$method_variable_source,
        "li_2010_monthly"
    )
})

test_that("temperature identity is preserved on native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    observed_values <- c(1, 3, 2, 5, 4, 8, 7, 6, 10, 9, 12, 11)
    future_values <- c(11, 13, 12, 15, 14, 18, 17, 16, 20, 19, 22, 21)

    for (calendar in calendars) {
        observed <- signal_test__series(
            "tas",
            2001L,
            observed_values,
            calendar
        )
        historical <- signal_test__series(
            "tas",
            1991L,
            observed_values,
            calendar
        )
        future <- signal_test__series(
            "tas",
            2061L,
            future_values,
            calendar
        )
        execution <- signal_test__execute(
            edcdf__component(),
            "tas",
            observed,
            historical,
            future,
            default_overrides = edcdf_test__defaults
        )
        adjusted <- execution@values[[1L]]

        expect_equal(adjusted@data$value, future_values)
        expect_identical(
            adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
            future[BIAS_DAILY_SERIES_COLUMNS[-2L]]
        )
        expect_identical(
            unique(adjusted@data$cf_calendar),
            calendar
        )
    }
})

test_that("mixed Gamma precipitation preserves identity and dry values", {
    observed_values <- c(0, 0, 1, 2, 3, 5, 8, 13)
    future_values <- c(0, 0, 0, 2, 4, 6, 9, 15)
    observed <- signal_test__series(
        "pr",
        2001L,
        observed_values
    )
    historical <- signal_test__series(
        "pr",
        1991L,
        observed_values
    )
    future <- signal_test__series(
        "pr",
        2061L,
        future_values
    )
    execution <- signal_test__execute(
        edcdf__component(),
        "pr",
        observed,
        historical,
        future,
        default_overrides = edcdf_test__defaults
    )
    adjusted <- execution@values[[1L]]
    diagnostics <- adjusted@provenance$diagnostics$precipitation

    expect_equal(adjusted@data$value, future_values)
    expect_identical(
        diagnostics$input_dry_values,
        c(
            observed_reference = 2L,
            model_historical = 2L,
            model_future = 3L
        )
    )
    expect_identical(diagnostics$output_dry_values, 3L)
    expect_identical(
        adjusted@provenance$negative_precipitation_policy,
        "clip_zero"
    )
})

test_that("negative additive precipitation corrections are explicit", {
    observed <- signal_test__series(
        "pr",
        2001L,
        c(0, 0, 1, 2, 3, 5, 8, 13)
    )
    historical <- signal_test__series(
        "pr",
        1991L,
        c(0, 0, 10, 20, 30, 50, 80, 130)
    )
    future <- signal_test__series(
        "pr",
        2061L,
        c(0, 0, 1, 2, 3, 5, 8, 13)
    )
    execution <- signal_test__execute(
        edcdf__component(),
        "pr",
        observed,
        historical,
        future,
        default_overrides = edcdf_test__defaults
    )
    adjusted <- execution@values[[1L]]
    diagnostics <- adjusted@provenance$diagnostics$precipitation

    expect_true(all(adjusted@data$value >= 0))
    expect_gt(diagnostics$negative_before_clipping, 0L)
    expect_gt(diagnostics$output_dry_values, 2L)
})

test_that("settings validation rejects non-positive extension and unknown keys", {
    settings <- signal_test__profile_settings(edcdf__profiles(), "tas")
    invalid <- settings
    invalid$range_extension_sd <- 0
    expect_error(
        edcdf__settings(list(tas = invalid)),
        "must be positive"
    )

    invalid <- settings
    invalid$unexpected <- TRUE
    expect_error(
        edcdf__settings(list(tas = invalid)),
        "Unexpected"
    )
})

test_that("experimental daily profile warning is retained", {
    values <- c(1, 3, 2, 5, 4, 8, 7, 6, 10, 9, 12, 11)
    observed <- signal_test__series("tas", 2001L, values)
    historical <- signal_test__series("tas", 1991L, values)
    future <- signal_test__series("tas", 2061L, values + 5)

    expect_warning(
        signal_test__execute(
            edcdf__component(),
            "tas",
            observed,
            historical,
            future,
            default_overrides = edcdf_test__defaults,
            warn_experimental = TRUE
        ),
        "experimental"
    )

    component <- edcdf__component()
    expect_identical(component@metadata$published_frequency, "mon")
    expect_identical(component@metadata$adapted_frequency, "day")
})

# vim: fdm=marker :
