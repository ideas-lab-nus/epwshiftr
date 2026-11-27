test_that("Linear Scaling applies monthly additive temperature corrections", {
    observed <- bias_adjustment_test__series(
        "tas",
        2001L,
        c(10, 14, 20, 24)
    )
    historical <- bias_adjustment_test__series(
        "tas",
        1991L,
        c(8, 10, 17, 19)
    )
    future <- bias_adjustment_test__series(
        "tas",
        2061L,
        c(18, 21, 30, 35)
    )
    execution <- signal_test__execute(
        bias__linear_scaling_component(),
        "tas",
        observed,
        historical,
        future,
        overrides = list(bounds = c(-Inf, 40))
    )
    adjusted <- execution@values[[1L]]

    expect_true(S7::S7_inherits(execution, SignalExecutionResult))
    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_equal(adjusted@data$value, c(21, 24, 34, 39))
    expect_identical(
        adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
        future[BIAS_DAILY_SERIES_COLUMNS[-2L]]
    )
    expect_identical(adjusted@transformation, "additive")
    expect_identical(adjusted@output_role, "model_future")
    expect_identical(execution@diagnostics$status, "ok")
    expect_equal(
        adjusted@provenance$monthly_corrections$correction,
        c(3, 4)
    )
    expect_identical(adjusted@provenance$grouping, "calendar_month")
    expect_identical(adjusted@provenance$statistic, "mean")
    expect_identical(adjusted@provenance$group_key, list(site = "A"))
    expect_identical(
        names(adjusted@settings),
        c("transformation", "bounds", "zero_tolerance")
    )
    expect_identical(adjusted@settings$bounds, c(-Inf, 40))
    expect_identical(execution@profiles$tas$settings$bounds, c(-Inf, 40))
    expect_identical(
        execution@profiles$tas$metadata$grouping,
        "calendar_month"
    )
    expect_identical(execution@profiles$tas$metadata$statistic, "mean")
})

test_that("Linear Scaling applies multiplicative precipitation corrections", {
    observed <- bias_adjustment_test__series(
        "pr",
        2001L,
        c(2, 4, 3, 5)
    )
    historical <- bias_adjustment_test__series(
        "pr",
        1991L,
        c(1, 2, 2, 2)
    )
    future <- bias_adjustment_test__series(
        "pr",
        2061L,
        c(3, 6, 4, 8)
    )
    component <- bias__linear_scaling_component()
    result <- component__execute(
        component,
        "apply_group",
        inputs = list(
            observed_reference = observed,
            model_historical = historical,
            model_future = future
        ),
        settings = list(
            pr = signal_test__profile_settings(
                bias__linear_scaling_profiles(),
                "pr"
            )
        ),
        key = list(site = "A")
    )

    expect_true(S7::S7_inherits(result, DailyAdjustedSeries))
    expect_equal(result@data$value, c(6, 12, 8, 16))
    expect_equal(
        result@provenance$monthly_corrections$correction,
        c(2, 2)
    )
    expect_identical(result@settings$bounds, c(0, Inf))
    expect_identical(result@provenance$clipped_values, 0L)
})

test_that("Linear Scaling handles undefined ratios and bounds explicitly", {
    observed <- bias_adjustment_test__series(
        "pr",
        2001L,
        c(1, 1, 1, 1)
    )
    historical <- bias_adjustment_test__series(
        "pr",
        1991L,
        c(0, 0, 1, 1)
    )
    future <- bias_adjustment_test__series(
        "pr",
        2061L,
        c(2, 2, 2, 2)
    )
    component <- bias__linear_scaling_component()
    settings <- signal_test__profile_settings(
        bias__linear_scaling_profiles(),
        "pr"
    )

    expect_error(
        component__execute(
            component,
            "apply_group",
            inputs = list(
                observed_reference = observed,
                model_historical = historical,
                model_future = future
            ),
            settings = list(pr = settings),
            key = list()
        ),
        "historical monthly mean is zero"
    )
    collected <- signal_test__execute(
        component,
        "pr",
        observed,
        historical,
        future,
        key = list(),
        error_policy = "collect"
    )
    expect_null(collected@values[[1L]])
    expect_identical(collected@diagnostics$status, "error")
    expect_match(
        collected@diagnostics$message,
        "historical monthly mean is zero"
    )

    historical$value <- 1
    bounded <- component__execute(
        component,
        "apply_group",
        inputs = list(
            observed_reference = observed,
            model_historical = historical,
            model_future = future
        ),
        settings = list(
            pr = utils::modifyList(
                settings,
                list(bounds = c(0, 1.5))
            )
        ),
        key = list()
    )
    expect_equal(bounded@data$value, rep(1.5, 4L))
    expect_identical(bounded@provenance$clipped_values, 4L)
})

# vim: fdm=marker :
