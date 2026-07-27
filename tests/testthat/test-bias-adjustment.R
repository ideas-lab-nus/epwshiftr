# Build compact canonical daily rows for package-native signal contract tests.
bias_adjustment_test__series <- function(
    variable_id,
    year,
    values,
    months = c(1L, 1L, 2L, 2L),
    days = c(1L, 2L, 1L, 2L),
    calendar = "noleap",
    units = if (identical(variable_id, "pr")) {
        "kg m-2 s-1"
    } else {
        "K"
    }
) {
    fields <- data.frame(
        year = rep.int(as.integer(year), length(values)),
        month = as.integer(months),
        day = as.integer(days),
        hour = rep.int(12, length(values)),
        minute = rep.int(0, length(values)),
        second = rep.int(0, length(values))
    )
    coordinates <- cf_time__coordinates(fields, calendar)
    data.frame(
        variable_id = rep.int(variable_id, length(values)),
        value = as.numeric(values),
        units = rep.int(units, length(values)),
        frequency = rep.int("day", length(values)),
        coordinates,
        stringsAsFactors = FALSE
    )
}

# Build all three role-labelled WeatherInput objects required by monthly
# mean-change signals while retaining the same tables in the aligned group.
bias_adjustment_test__inputs <- function(observed, historical, future) {
    weather__new_inputs(
        observed_reference = weather__new_input(
            "observed_reference",
            observed
        ),
        model_historical = weather__new_input(
            "model_historical",
            historical
        ),
        model_future = weather__new_input(
            "model_future",
            future
        )
    )
}

test_that("daily adjusted series enforces canonical calendar-native data", {
    source <- bias_adjustment_test__series(
        "tas",
        2061L,
        c(280, 282, 284, 286)
    )
    result <- bias__daily_adjusted_series(
        source,
        output_role = "model_future",
        transformation = "additive",
        settings = list(grouping = "calendar_month"),
        provenance = list(method = "test")
    )

    expect_true(S7::S7_inherits(result, DailyAdjustedSeries))
    expect_identical(result@data, source)
    expect_identical(result@output_role, "model_future")
    expect_identical(result@variable_metadata$tas$units, "K")
    expect_identical(result@variable_metadata$tas$frequency, "day")
    expect_identical(result@variable_metadata$tas$calendars, "noleap")

    for (calendar in CF_TIME_CALENDARS) {
        calendar_data <- bias_adjustment_test__series(
            "tas",
            2000L,
            c(280, 282, 284, 286),
            calendar = calendar
        )
        expect_no_error(
            bias__daily_adjusted_series(
                calendar_data,
                "model_future",
                "additive"
            )
        )
    }

    duplicate <- rbind(source, source[1L, ])
    expect_error(
        bias__daily_adjusted_series(
            duplicate,
            "model_future",
            "additive"
        ),
        "unique variable-calendar-year-month-day"
    )
    invalid_date <- source
    invalid_date$cf_day[[1L]] <- 31L
    invalid_date$cf_month[[1L]] <- 2L
    expect_error(
        bias__daily_adjusted_series(
            invalid_date,
            "model_future",
            "additive"
        ),
        "invalid date"
    )
    invalid_phase <- source
    invalid_phase$annual_phase[[1L]] <- 0.9
    expect_error(
        bias__daily_adjusted_series(
            invalid_phase,
            "model_future",
            "additive"
        ),
        "annual_phase"
    )
})

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
