test_that("all seven future-weather component stages have executable contracts", {
    specs <- lapply(WEATHER_COMPONENT_STAGES, function(stage) {
        primary <- WEATHER_COMPONENT_PRIMARY_OPERATIONS[[stage]]
        operations <- stats::setNames(
            list(function(value) value),
            primary
        )
        component__spec(
            name = paste0(stage, "_test"),
            stage = stage,
            input_kinds = paste0(stage, "_input"),
            output_kinds = paste0(stage, "_output"),
            operations = operations
        )
    })

    expect_identical(
        vapply(specs, function(spec) spec@stage, character(1L)),
        WEATHER_COMPONENT_STAGES
    )
    for (spec in specs) {
        primary <- WEATHER_COMPONENT_PRIMARY_OPERATIONS[[spec@stage]]
        expect_identical(
            component__execute(spec, primary, "value"),
            "value"
        )
    }
})

test_that("component registries retain metadata but not serialized operations", {
    registry <- new.env(parent = emptyenv())
    signal <- component__spec(
        name = "daily_delta",
        stage = "signal",
        label = "Daily delta",
        input_kinds = "calendar_indexed",
        output_kinds = "seasonal_change",
        operations = list(apply = function(value) value)
    )

    expect_identical(
        component__register(signal, registry = registry),
        signal
    )
    expect_identical(
        component__get("signal", "daily_delta", registry),
        signal
    )
    listed <- component__list(registry = registry)
    expect_named(
        listed,
        c(
            "stage", "name", "label", "input_kinds", "output_kinds",
            "scopes", "stochastic"
        )
    )
    expect_identical(listed$name, "daily_delta")
    expect_false("operations" %in% names(listed))
    expect_error(
        component__register(signal, registry = registry),
        "already registered"
    )
})

test_that("component compatibility uses stage order and intermediate kinds", {
    calendar <- component__spec(
        name = "annual_phase",
        stage = "calendar",
        input_kinds = "prepared_inputs",
        output_kinds = "calendar_indexed",
        operations = list(apply = function(value) value)
    )
    signal <- component__spec(
        name = "qdm",
        stage = "signal",
        input_kinds = "calendar_indexed",
        output_kinds = "corrected_series",
        operations = list(apply = function(value) value)
    )
    incompatible <- component__spec(
        name = "monthly_only",
        stage = "signal",
        input_kinds = "monthly_summary",
        output_kinds = "seasonal_change",
        operations = list(apply = function(value) value)
    )

    expect_true(component__compatible(calendar, signal))
    expect_false(component__compatible(signal, calendar))
    expect_false(component__compatible(calendar, incompatible))
    expect_invisible(component__assert_compatible(calendar, signal))
    expect_error(
        component__assert_compatible(signal, calendar),
        "must follow"
    )
    expect_error(
        component__assert_compatible(calendar, incompatible),
        "cannot feed"
    )
})

test_that("component input requirements keep inner AND and outer OR semantics", {
    temperature <- component__input_requirement(
        "model_future",
        representations = "series",
        frequencies = "day",
        calendars = c("noleap", "360_day"),
        variable_sets = list(
            c("huss", "tas", "ps"),
            c("hurs", "tas", "ps")
        )
    )
    signal <- component__spec(
        name = "humidity_signal",
        stage = "signal",
        required_inputs = list(model_future = temperature),
        input_kinds = "calendar_indexed",
        output_kinds = "corrected_series",
        scopes = "multivariate",
        operations = list(apply = function(value) value)
    )
    valid <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            data.frame(
                variable_id = c("hurs", "tas", "ps"),
                frequency = "day",
                cf_calendar = "360_day"
            )
        )
    )
    missing_pressure <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            data.frame(
                variable_id = c("hurs", "tas"),
                frequency = "day",
                cf_calendar = "360_day"
            )
        )
    )

    expect_identical(component__input_errors(signal, valid), character())
    expect_invisible(component__validate_inputs(signal, valid))
    expect_match(
        component__input_errors(signal, missing_pressure),
        "huss \\+ tas \\+ ps.*or.*hurs \\+ tas \\+ ps"
    )
    expect_error(
        component__validate_inputs(signal, missing_pressure),
        "input requirements are not satisfied"
    )
})

test_that("component input validation distinguishes required and optional roles", {
    observed <- component__input_requirement(
        "observed_reference",
        representations = "series",
        frequencies = "day"
    )
    historical <- component__input_requirement(
        "model_historical",
        representations = "series",
        frequencies = "day",
        variable_sets = "tas"
    )
    signal <- component__spec(
        name = "quantile_mapping",
        stage = "signal",
        required_inputs = list(
            observed_reference = observed,
            model_historical = historical
        ),
        optional_inputs = list(
            model_future = component__input_requirement(
                "model_future",
                frequencies = "day"
            )
        ),
        input_kinds = "calendar_indexed",
        output_kinds = "corrected_series",
        operations = list(
            fit = function(value) value,
            apply = function(value) value
        )
    )
    inputs <- weather__new_inputs(
        model_historical = weather__new_input(
            "model_historical",
            data.frame(variable_id = "tas", frequency = "day")
        )
    )

    errors <- component__input_errors(signal, inputs)
    expect_match(errors, "required role `observed_reference` is missing")
    expect_length(errors, 1L)
})

test_that("component specs reject missing or stage-inappropriate operations", {
    expect_error(
        component__spec(
            "bad_signal",
            "signal",
            input_kinds = "calendar_indexed",
            output_kinds = "corrected_series",
            operations = list(fit = function(value) value)
        ),
        "require a named `apply` operation"
    )
    expect_error(
        component__spec(
            "bad_output",
            "output",
            input_kinds = "physical_weather",
            output_kinds = "weather_artifact",
            operations = list(apply = function(value) value)
        ),
        "Unknown `output` operation"
    )
})
