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
    component <- bias__linear_scaling_component()
    inputs <- bias_adjustment_test__inputs(
        observed,
        historical,
        future
    )
    group <- signal__group(
        key = list(site = "A"),
        inputs = list(
            observed_reference = observed,
            model_historical = historical,
            model_future = future
        ),
        variables = "tas"
    )

    execution <- component__execute(
        component,
        "apply",
        inputs = inputs,
        groups = list(group),
        overrides = list(tas = list(bounds = c(-Inf, 40)))
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
    expect_identical(adjusted@provenance$group_key, list(site = "A"))
    expect_identical(adjusted@settings$bounds, c(-Inf, 40))
    expect_identical(execution@profiles$tas$settings$bounds, c(-Inf, 40))
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
            pr = component@metadata$signal_profiles$pr$settings
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
    settings <- component@metadata$signal_profiles$pr$settings

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
    inputs <- bias_adjustment_test__inputs(
        observed,
        historical,
        future
    )
    group <- signal__group(
        inputs = list(
            observed_reference = observed,
            model_historical = historical,
            model_future = future
        ),
        variables = "pr"
    )
    collected <- component__execute(
        component,
        "apply",
        inputs = inputs,
        groups = list(group),
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

    future$value[[1L]] <- -1
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
        "non-negative"
    )
})

test_that("Linear Scaling is a registered package-native signal component", {
    bias__register_linear_scaling_component()
    component <- component__get("signal", "linear_scaling_daily")

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
        sort(names(component@required_inputs)),
        sort(c(
            "observed_reference",
            "model_historical",
            "model_future"
        ))
    )
    expect_identical(
        component@metadata$output_contract,
        "daily_adjusted_series"
    )

    calendar <- component__spec(
        name = "daily_calendar_test",
        stage = "calendar",
        input_kinds = "preprocessed_daily_series",
        output_kinds = "calendar_indexed_daily_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "adjusted_sequence_test",
        stage = "sequence",
        input_kinds = "daily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
