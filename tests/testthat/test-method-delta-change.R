test_that("Delta Change transfers additive changes onto observed daily data", {
    observed <- bias_adjustment_test__series(
        "tas",
        2001L,
        c(10, 14, 20, 24),
        calendar = "360_day"
    )
    historical <- bias_adjustment_test__series(
        "tas",
        1991L,
        c(8, 10, 17, 19),
        calendar = "noleap"
    )
    future <- bias_adjustment_test__series(
        "tas",
        2061L,
        c(18, 22, 30, 34),
        calendar = "all_leap"
    )
    component <- bias__delta_change_component()
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
        groups = list(group)
    )
    adjusted <- execution@values[[1L]]

    expect_true(S7::S7_inherits(execution, SignalExecutionResult))
    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_equal(adjusted@data$value, c(21, 25, 34, 38))
    expect_identical(
        adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
        observed[BIAS_DAILY_SERIES_COLUMNS[-2L]]
    )
    expect_equal(
        diff(adjusted@data$value[c(1L, 2L)]),
        diff(observed$value[c(1L, 2L)])
    )
    expect_equal(
        diff(adjusted@data$value[c(3L, 4L)]),
        diff(observed$value[c(3L, 4L)])
    )
    expect_identical(adjusted@output_role, "observed_reference")
    expect_identical(adjusted@transformation, "additive")
    expect_identical(adjusted@provenance$method, "delta_change")
    expect_equal(
        adjusted@provenance$monthly_changes$change,
        c(11, 14)
    )
    expect_identical(adjusted@provenance$group_key, list(site = "A"))
    expect_identical(execution@diagnostics$status, "ok")
    expect_identical(
        execution@profiles$tas$metadata$output_role,
        "observed_reference"
    )
})

test_that("Delta Change has zero-change identity for every CF calendar", {
    component <- bias__delta_change_component()
    settings <- component@metadata$signal_profiles$tas$settings

    for (calendar in CF_TIME_CALENDARS) {
        observed <- bias_adjustment_test__series(
            "tas",
            2000L,
            c(10, 14, 20, 24),
            calendar = calendar
        )
        historical <- bias_adjustment_test__series(
            "tas",
            1990L,
            c(8, 10, 17, 19),
            calendar = calendar
        )
        future <- historical
        future$cf_year <- rep.int(2060L, nrow(future))
        coordinates <- cf_time__coordinates(
            data.frame(
                year = future$cf_year,
                month = future$cf_month,
                day = future$cf_day,
                hour = rep.int(12, nrow(future)),
                minute = rep.int(0, nrow(future)),
                second = rep.int(0, nrow(future))
            ),
            calendar
        )
        future[names(coordinates)] <- coordinates

        result <- component__execute(
            component,
            "apply_group",
            inputs = list(
                observed_reference = observed,
                model_historical = historical,
                model_future = future
            ),
            settings = list(tas = settings),
            key = list(calendar = calendar)
        )

        expect_identical(result@data$value, observed$value)
        expect_identical(result@data$cf_calendar, observed$cf_calendar)
        expect_equal(result@provenance$monthly_changes$change, c(0, 0))
    }
})

test_that("Delta Change transfers multiplicative precipitation changes", {
    observed <- bias_adjustment_test__series(
        "pr",
        2001L,
        c(0, 4, 2, 6)
    )
    historical <- bias_adjustment_test__series(
        "pr",
        1991L,
        c(1, 3, 2, 2)
    )
    future <- bias_adjustment_test__series(
        "pr",
        2061L,
        c(2, 6, 6, 2)
    )
    component <- bias__delta_change_component()
    settings <- component@metadata$signal_profiles$pr$settings
    result <- component__execute(
        component,
        "apply_group",
        inputs = list(
            observed_reference = observed,
            model_historical = historical,
            model_future = future
        ),
        settings = list(pr = settings),
        key = list(site = "A")
    )

    expect_equal(result@data$value, c(0, 8, 4, 12))
    expect_identical(
        result@data$value == 0,
        observed$value == 0
    )
    expect_equal(
        result@provenance$monthly_changes$change,
        c(2, 2)
    )
    expect_identical(result@settings$bounds, c(0, Inf))
    expect_identical(result@provenance$clipped_values, 0L)
})

test_that("Delta Change rejects undefined inputs and records clipping", {
    observed <- bias_adjustment_test__series(
        "pr",
        2001L,
        c(1, 2, 3, 4)
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
    component <- bias__delta_change_component()
    settings <- component@metadata$signal_profiles$pr$settings
    roles <- list(
        observed_reference = observed,
        model_historical = historical,
        model_future = future
    )

    expect_error(
        component__execute(
            component,
            "apply_group",
            inputs = roles,
            settings = list(pr = settings),
            key = list()
        ),
        "historical monthly mean is zero"
    )

    roles$model_historical$value <- rep.int(1, 4L)
    incompatible_units <- roles
    incompatible_units$model_future$units <- rep.int("mm day-1", 4L)
    expect_error(
        component__execute(
            component,
            "apply_group",
            inputs = incompatible_units,
            settings = list(pr = settings),
            key = list()
        ),
        "identical units"
    )

    negative <- roles
    negative$model_future$value[[1L]] <- -1
    expect_error(
        component__execute(
            component,
            "apply_group",
            inputs = negative,
            settings = list(pr = settings),
            key = list()
        ),
        "non-negative"
    )

    bounded <- component__execute(
        component,
        "apply_group",
        inputs = roles,
        settings = list(
            pr = utils::modifyList(
                settings,
                list(bounds = c(0, 5))
            )
        ),
        key = list()
    )
    expect_equal(bounded@data$value, c(2, 4, 5, 5))
    expect_identical(bounded@provenance$clipped_values, 2L)
})

test_that("Delta Change is a registered package-native signal component", {
    bias__register_delta_change_component()
    component <- component__get("signal", "delta_change_daily")

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
        sort(names(component@metadata$signal_profiles)),
        sort(c("tas", "tasmin", "tasmax", "pr"))
    )
    expect_identical(
        component@metadata$signal_profiles$pr$evidence,
        "published"
    )

    calendar <- component__spec(
        name = "delta_calendar_test",
        stage = "calendar",
        input_kinds = "preprocessed_daily_series",
        output_kinds = "calendar_indexed_daily_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "delta_sequence_test",
        stage = "sequence",
        input_kinds = "daily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
