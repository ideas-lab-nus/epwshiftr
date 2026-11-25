# Build a minimal valid pipeline plan for testing stage argument dispatch
# without registering test-only components in the package-wide registry.
# pipeline_test__plan {{{
pipeline_test__plan <- function(signal) {
    stages <- WEATHER_COMPONENT_STAGES
    components <- stats::setNames(
        lapply(stages, function(stage) {
            if (identical(stage, "signal")) {
                return(signal)
            }
            component__spec(
                name = paste0("pipeline_", stage, "_test"),
                stage = stage,
                input_kinds = paste0(stage, "_input"),
                output_kinds = paste0(stage, "_output"),
                operations = stats::setNames(
                    list(function(...) NULL),
                    WEATHER_COMPONENT_PRIMARY_OPERATIONS[[stage]]
                )
            )
        }),
        stages
    )
    WeatherPipelinePlan(
        spec = pipeline__spec(lapply(components, function(c) c@name)),
        inputs = weather__new_inputs(
            model_future = weather__new_input(
                "model_future",
                data.frame(variable_id = "tas", frequency = "day")
            )
        ),
        components = components
    )
}
# }}}

test_that("pipeline routes signal overrides and scoped options by component declaration", {
    signal <- signal__component(
        "pipeline_signal_test",
        required_inputs = list(
            model_future = component__input_requirement(
                "model_future",
                frequencies = "day",
                variable_sets = "tas"
            )
        ),
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
    plan@components$preprocess@metadata$scoped_options <- "input_setting"
    plan@components$hourly@metadata$scoped_options <- "hourly_setting"

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
    expect_identical(
        pipeline__operation_args(
            signal,
            plan,
            previous,
            NULL,
            list(),
            list()
        )$overrides,
        list()
    )
    expect_error(
        pipeline__signal_overrides(list(signal_overrides = "invalid")),
        "list"
    )

    # Scoped options route only to owning stages while shared options reach all stages
    args_for <- function(stage) {
        pipeline__operation_args(
            plan@components[[stage]],
            plan,
            previous,
            context = list(recipe = list(backend = "unrelated_backend")),
            options = list(
                signal_overrides = overrides,
                input_setting = 1L,
                hourly_setting = 2L,
                shared = 3L
            ),
            stages = list()
        )
    }
    expect_identical(
        args_for("preprocess")$options,
        list(input_setting = 1L, shared = 3L)
    )
    expect_identical(
        args_for("hourly")$options,
        list(hourly_setting = 2L, shared = 3L)
    )
    expect_identical(args_for("calendar")$options, list(shared = 3L))
    expect_identical(args_for("output")$options, list(shared = 3L))
    expect_setequal(
        hourly_kqdm_input__component()@metadata$scoped_options,
        c("include_longwave", "model_utc_offset_hours")
    )

    # Daily hourly projection strips signal-owned options
    projection <- temperature__projection_options(c(
        EPW_MORPH_DAILY_TEMPERATURE_OPTIONS,
        list(signal_overrides = overrides)
    ))
    expect_named(projection, names(EPW_MORPH_TEMPERATURE_PROJECTION_OPTIONS))
    expect_false("signal_overrides" %in% names(projection))
})

test_that("generic interpolation rejects method-specific options and invalid flags", {
    inputs <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            data.frame(variable_id = "tas", frequency = "hour")
        )
    )
    expect_error(
        weather_interp__apply_core(
            inputs,
            context = list(),
            options = list(include_longwave = TRUE),
            component_name = "hourly_kernel_qdm_input_preparation"
        ),
        "does not accept"
    )
    expect_error(
        hourly_kqdm_input__apply(
            inputs,
            context = list(),
            options = list(include_longwave = "yes")
        ),
        "flag"
    )
})

# vim: fdm=marker :
