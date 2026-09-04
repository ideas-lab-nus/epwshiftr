# Build a minimal valid pipeline plan for testing stage argument dispatch
# without registering test-only components in the package-wide registry.
pipeline_test__plan <- function(signal) {
    stages <- WEATHER_COMPONENT_STAGES
    components <- lapply(stages, function(stage) {
        if (identical(stage, "signal")) {
            return(signal)
        }
        operation <- WEATHER_COMPONENT_PRIMARY_OPERATIONS[[stage]]
        component__spec(
            name = paste0("pipeline_", stage, "_test"),
            stage = stage,
            input_kinds = paste0(stage, "_input"),
            output_kinds = paste0(stage, "_output"),
            operations = stats::setNames(
                list(function(...) NULL),
                operation
            )
        )
    })
    names(components) <- stages
    records <- stats::setNames(
        lapply(components, function(component) component@name),
        stages
    )
    inputs <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            data.frame(variable_id = "tas", frequency = "day")
        )
    )
    WeatherPipelinePlan(
        spec = pipeline__spec(records),
        inputs = inputs,
        components = components
    )
}

test_that("pipeline signal options reach the selected component", {
    requirement <- component__input_requirement(
        "model_future",
        frequencies = "day",
        variable_sets = "tas"
    )
    signal <- signal__component(
        "pipeline_signal_test",
        required_inputs = list(model_future = requirement),
        input_kinds = "calendar_output",
        output_kinds = "signal_output",
        profiles = list(signal__variable_profile(
            "tas",
            settings = list(offset = 1),
            evidence = "published",
            references = "doi:10.1000/pipeline-test"
        )),
        apply_group = function(inputs, settings, key) {
            inputs$model_future + settings$tas$offset
        }
    )
    plan <- pipeline_test__plan(signal)
    groups <- list(signal__group(
        inputs = list(model_future = 1:2),
        variables = "tas"
    ))
    previous <- WeatherStageResult(
        stage = "calendar",
        component = "pipeline_calendar_test",
        kind = "calendar_output",
        value = groups
    )
    overrides <- list(tas = list(offset = 5))

    args <- pipeline__operation_args(
        component = signal,
        plan = plan,
        previous = previous,
        context = NULL,
        options = list(signal_overrides = overrides),
        stages = list()
    )
    result <- do.call(component__operation(signal, "apply"), args)

    expect_identical(args$overrides, overrides)
    expect_identical(result@values[[1L]], c(6, 7))
    expect_identical(result@profiles$tas$settings$offset, 5)

    default_args <- pipeline__operation_args(
        component = signal,
        plan = plan,
        previous = previous,
        context = NULL,
        options = list(),
        stages = list()
    )
    expect_identical(default_args$overrides, list())
    expect_error(
        pipeline__signal_overrides(list(signal_overrides = "invalid")),
        "list"
    )
})

test_that("daily hourly projection ignores signal-owned options", {
    options <- c(
        EPW_MORPH_DAILY_TEMPERATURE_OPTIONS,
        list(signal_overrides = list(tas = list(offset = 5)))
    )

    projection <- temperature__projection_options(options)

    expect_named(
        projection,
        names(EPW_MORPH_TEMPERATURE_PROJECTION_OPTIONS)
    )
    expect_false("signal_overrides" %in% names(projection))
})
