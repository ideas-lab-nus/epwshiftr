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

test_that("weather inputs enforce four semantic roles, mixed frequencies, and climatology rejection", {
    future <- data.frame(
        variable_id = c("tas", "rsds", "tasmin"),
        frequency = c("3hrPt", "3hr", "day"),
        cf_calendar = "360_day"
    )
    historical <- transform(future, cf_calendar = "noleap")
    observed <- data.frame(
        variable_id = "tas",
        frequency = "day",
        calendar = "gregorian"
    )

    inputs <- weather__new_inputs(
        weather_template = weather__new_input(
            "weather_template",
            "baseline.epw"
        ),
        observed_reference = weather__new_input(
            "observed_reference",
            observed
        ),
        model_historical = weather__new_input(
            "model_historical",
            historical
        ),
        model_future = weather__new_input("model_future", future)
    )

    expect_true(S7::S7_inherits(inputs, WeatherInputs))
    expect_identical(
        weather__get_input(inputs, "weather_template")@representation,
        "epw"
    )
    expect_identical(
        weather__get_input(inputs, "observed_reference")@calendars,
        "gregorian"
    )
    expect_identical(
        weather__get_input(inputs, "model_historical")@calendars,
        "noleap"
    )
    expect_identical(
        weather__get_input(inputs, "model_future")@frequencies,
        c("3hrPt", "3hr", "day")
    )
    expect_identical(
        weather__get_input(inputs, "model_future")@variable_frequencies,
        list(tas = "3hrPt", rsds = "3hr", tasmin = "day")
    )

    expect_error(weather__new_inputs(), "At least one future-weather input")
    expect_error(
        weather__new_inputs(
            model_historical = weather__get_input(inputs, "model_future")
        ),
        "contains input role `model_future`"
    )
    expect_error(
        weather__new_input(
            "model_future",
            data.frame(variable_id = "tas"),
            representation = "unknown"
        ),
        "Must be element of set"
    )

    # Climatology aggregates cannot be relabelled as ordinary series or grids
    clim <- data.frame(
        variable_id = "tas",
        frequency = "day",
        cf_calendar = "standard",
        time_kind = "climatology"
    )
    expect_error(weather__new_input("model_future", clim), "climatolog")
    expect_error(
        weather__new_input("model_future", clim, representation = "grid"),
        "climatolog"
    )

    # morpher__context() preserves legacy fields alongside explicit WeatherInputs
    epw <- epw_file_read(get_cache_epw())
    row <- data.table::data.table(
        variable_id = "tas",
        time = as.POSIXct("2050-01-01", tz = "UTC"),
        period = "future",
        year = 2050L,
        lon = 103.8,
        lat = 1.3,
        units = "K",
        value = 301,
        frequency = "day",
        cf_calendar = "360_day"
    )
    ctx <- morpher__context(
        epw,
        row,
        reference_climate = data.table::copy(row)[, cf_calendar := "noleap"],
        observed_reference = data.table::copy(row)[, cf_calendar := "standard"]
    )
    expect_true(S7::S7_inherits(ctx$inputs, WeatherInputs))
    expect_identical(ctx$inputs@weather_template@source, ctx$epw)
    expect_identical(ctx$inputs@model_future@source, ctx$climate)
    expect_identical(ctx$inputs@model_historical@source, ctx$reference_climate)
    expect_identical(
        ctx$inputs@observed_reference@source,
        ctx$observed_reference
    )
})

test_that("component contracts enforce stage compatibility, AND/OR variables, and variable frequencies", {
    registry <- new.env(parent = emptyenv())
    calendar <- component__spec(
        name = "annual_phase",
        stage = "calendar",
        input_kinds = "prepared_inputs",
        output_kinds = "calendar_indexed",
        operations = list(apply = function(value) value)
    )
    signal <- component__spec(
        name = "humidity_signal",
        stage = "signal",
        required_inputs = list(
            observed_reference = component__input_requirement(
                "observed_reference",
                representations = "series",
                frequencies = "day"
            ),
            model_future = component__input_requirement(
                "model_future",
                representations = "series",
                frequencies = c("3hrPt", "3hr"),
                variable_frequencies = list(tas = "3hrPt", rsds = "3hr"),
                calendars = c("noleap", "360_day"),
                variable_sets = list(
                    c("huss", "tas", "ps"),
                    c("hurs", "tas", "rsds")
                )
            )
        ),
        input_kinds = "calendar_indexed",
        output_kinds = "corrected_series",
        scopes = "multivariate",
        operations = list(apply = function(value) value)
    )
    incompatible <- component__spec(
        name = "monthly_only",
        stage = "signal",
        input_kinds = "monthly_summary",
        output_kinds = "seasonal_change",
        operations = list(apply = function(value) value)
    )

    expect_identical(component__execute(signal, "apply", "ok"), "ok")
    expect_invisible(component__assert_compatible(calendar, signal))
    expect_error(component__assert_compatible(signal, calendar), "must follow")
    expect_error(
        component__assert_compatible(calendar, incompatible),
        "cannot feed"
    )

    # Built-in registration preserves existing process-local extensions
    extension <- calendar
    extension@label <- "Extension implementation"
    component__register(extension, registry = registry)
    expect_error(
        component__register(extension, registry = registry),
        "already registered"
    )
    component__register_builtins(list(calendar, signal), registry = registry)
    expect_identical(
        component__get("calendar", "annual_phase", registry),
        extension
    )
    expect_identical(
        component__get("signal", "humidity_signal", registry),
        signal
    )

    obs <- weather__new_input(
        "observed_reference",
        data.frame(variable_id = "tas", frequency = "day")
    )
    valid <- weather__new_inputs(
        observed_reference = obs,
        model_future = weather__new_input(
            "model_future",
            data.frame(
                variable_id = c("hurs", "tas", "rsds"),
                frequency = c("3hrPt", "3hrPt", "3hr"),
                cf_calendar = "360_day"
            )
        )
    )
    expect_identical(component__input_errors(signal, valid), character())
    expect_invisible(component__validate_inputs(signal, valid))

    bad_inputs <- weather__new_inputs(
        model_future = weather__new_input(
            "model_future",
            data.frame(
                variable_id = c("hurs", "tas", "rsds"),
                frequency = c("3hr", "3hr", "3hrPt"),
                cf_calendar = "360_day"
            )
        )
    )
    errors <- component__input_errors(signal, bad_inputs)
    expect_true(any(grepl(
        "required role `observed_reference` is missing",
        errors
    )))
    expect_true(any(grepl(
        "variable `tas` frequencies `3hr`",
        errors,
        fixed = TRUE
    )))

    missing_var <- weather__new_inputs(
        observed_reference = obs,
        model_future = weather__new_input(
            "model_future",
            data.frame(
                variable_id = c("hurs", "tas"),
                frequency = "3hrPt",
                cf_calendar = "360_day"
            )
        )
    )
    expect_match(
        component__input_errors(signal, missing_var),
        "huss \\+ tas \\+ ps.*or.*hurs \\+ tas \\+ rsds"
    )
    expect_error(
        signal@optional_inputs <- signal@required_inputs,
        "both required and optional"
    )
    expect_error(
        component__spec(
            "bad_signal",
            "signal",
            input_kinds = "calendar_indexed",
            output_kinds = "corrected_series",
            operations = list(fit = identity)
        ),
        "require a named `apply` operation"
    )
    expect_error(
        component__spec(
            "bad_output",
            "output",
            input_kinds = "physical_weather",
            output_kinds = "weather_artifact",
            operations = list(apply = identity)
        ),
        "Unknown `output` operation"
    )
})

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
