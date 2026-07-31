#' @include weather-signal.R
NULL

# WeatherStageResult is the runtime envelope exchanged between component
# stages. It keeps the data kind explicit and retains stage-local diagnostics
# and provenance without imposing one physical table shape on every method.
WeatherStageResult <- S7::new_class(
    "WeatherStageResult",
    properties = list(
        stage = S7::new_property(S7::class_character),
        component = S7::new_property(S7::class_character),
        kind = S7::new_property(S7::class_character),
        value = S7::new_property(S7::class_any),
        diagnostics = S7::new_property(S7::class_list, default = list()),
        provenance = S7::new_property(S7::class_list, default = list()),
        metadata = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (length(self@stage) != 1L ||
            is.na(self@stage) ||
            !self@stage %in% WEATHER_COMPONENT_STAGES) {
            return("`stage` must identify one future-weather component stage.")
        }
        if (length(self@component) != 1L ||
            is.na(self@component) ||
            !grepl("^[a-z][a-z0-9_]*$", self@component)) {
            return("`component` must be one lower snake_case component name.")
        }
        if (length(self@kind) != 1L ||
            is.na(self@kind) ||
            !nzchar(self@kind)) {
            return("`kind` must be one non-empty intermediate data kind.")
        }
        if (is.null(self@value)) {
            return("`value` cannot be NULL.")
        }
        NULL
    }
)

# WeatherPipelineSpec stores only stable component names. Executable functions
# remain in the process-local registry and are resolved when a plan is compiled.
WeatherPipelineSpec <- S7::new_class(
    "WeatherPipelineSpec",
    properties = list(
        components = S7::new_property(S7::class_list),
        metadata = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (!identical(names(self@components), WEATHER_COMPONENT_STAGES)) {
            return(
                "`components` must contain one entry for every ordered component stage."
            )
        }
        valid <- vapply(self@components, function(component) {
            is.character(component) &&
                length(component) == 1L &&
                !is.na(component) &&
                grepl("^[a-z][a-z0-9_]*$", component)
        }, logical(1L))
        if (!all(valid)) {
            return(
                "Every pipeline component must be one lower snake_case name."
            )
        }
        NULL
    }
)

# WeatherPipelinePlan retains the resolved component specifications and the
# role-addressable inputs that were validated before any stage executed.
WeatherPipelinePlan <- S7::new_class(
    "WeatherPipelinePlan",
    properties = list(
        spec = S7::new_property(S7::class_any),
        inputs = S7::new_property(S7::class_any),
        components = S7::new_property(S7::class_list)
    ),
    validator = function(self) {
        if (!S7::S7_inherits(self@spec, WeatherPipelineSpec)) {
            return("`spec` must be a WeatherPipelineSpec object.")
        }
        if (!S7::S7_inherits(self@inputs, WeatherInputs)) {
            return("`inputs` must be a WeatherInputs object.")
        }
        if (!identical(names(self@components), WEATHER_COMPONENT_STAGES) ||
            !all(vapply(
                self@components,
                S7::S7_inherits,
                logical(1L),
                class = WeatherComponentSpec
            ))) {
            return(
                "`components` must contain resolved specifications for every stage."
            )
        }
        NULL
    }
)

# WeatherPipelineExecution exposes every stage envelope for diagnostics while
# retaining the final method result consumed by EpwMorphBackend.
WeatherPipelineExecution <- S7::new_class(
    "WeatherPipelineExecution",
    properties = list(
        plan = S7::new_property(S7::class_any),
        stages = S7::new_property(S7::class_list),
        result = S7::new_property(S7::class_any)
    ),
    validator = function(self) {
        if (!S7::S7_inherits(self@plan, WeatherPipelinePlan)) {
            return("`plan` must be a WeatherPipelinePlan object.")
        }
        if (!identical(names(self@stages), WEATHER_COMPONENT_STAGES) ||
            !all(vapply(
                self@stages,
                S7::S7_inherits,
                logical(1L),
                class = WeatherStageResult
            ))) {
            return(
                "`stages` must contain one WeatherStageResult for every stage."
            )
        }
        if (is.null(self@result)) {
            return("`result` cannot be NULL.")
        }
        NULL
    }
)

# Construct one complete linear pipeline from stable registered component
# names. Identity components make intentionally unchanged stages explicit.
pipeline__spec <- function(components, metadata = list()) {
    checkmate::assert_list(components, names = "unique")
    missing <- setdiff(WEATHER_COMPONENT_STAGES, names(components))
    unknown <- setdiff(names(components), WEATHER_COMPONENT_STAGES)
    if (length(missing) || length(unknown)) {
        cli::cli_abort(c(
            "A weather pipeline must name every ordered component stage.",
            "x" = if (length(missing)) {
                "Missing stage(s): {.val {missing}}."
            },
            "x" = if (length(unknown)) {
                "Unknown stage(s): {.val {unknown}}."
            }
        ))
    }
    components <- components[WEATHER_COMPONENT_STAGES]
    checkmate::assert_list(metadata, names = "unique")
    WeatherPipelineSpec(components = components, metadata = metadata)
}

# Return the serializable stage-to-component mapping stored with a recipe.
pipeline__records <- function(spec) {
    if (!S7::S7_inherits(spec, WeatherPipelineSpec)) {
        cli::cli_abort("{.arg spec} must be a WeatherPipelineSpec object.")
    }
    lapply(spec@components, identity)
}

# Reconstruct a pipeline specification from persisted stage-to-component
# records before resolving executable operations from the registry.
pipeline__from_records <- function(records) {
    if (is.null(records)) {
        return(NULL)
    }
    if (is.character(records) && !is.null(names(records))) {
        records <- as.list(records)
    }
    pipeline__spec(records)
}

# Resolve and validate every component before execution, including role
# requirements and the intermediate kind exchanged across adjacent stages.
pipeline__compile <- function(spec, inputs) {
    if (!S7::S7_inherits(spec, WeatherPipelineSpec)) {
        cli::cli_abort("{.arg spec} must be a WeatherPipelineSpec object.")
    }
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    components <- lapply(WEATHER_COMPONENT_STAGES, function(stage) {
        component <- component__get(stage, spec@components[[stage]])
        component__validate_inputs(component, inputs)
        component
    })
    names(components) <- WEATHER_COMPONENT_STAGES
    for (index in seq_len(length(components) - 1L)) {
        component__assert_compatible(
            components[[index]],
            components[[index + 1L]]
        )
    }
    WeatherPipelinePlan(
        spec = spec,
        inputs = inputs,
        components = components
    )
}

# Find the allowed frequency intersection declared for one or more semantic
# input roles. NULL means that no component constrains those roles.
pipeline__frequency_choices <- function(
    spec,
    roles = c("model_historical", "model_future")
) {
    if (!S7::S7_inherits(spec, WeatherPipelineSpec)) {
        cli::cli_abort("{.arg spec} must be a WeatherPipelineSpec object.")
    }
    checkmate::assert_subset(roles, WEATHER_INPUT_ROLES)
    choices <- list()
    for (stage in WEATHER_COMPONENT_STAGES) {
        component <- component__get(stage, spec@components[[stage]])
        requirements <- c(
            component@required_inputs,
            component@optional_inputs
        )
        for (role in intersect(roles, names(requirements))) {
            frequencies <- requirements[[role]]@frequencies
            if (length(frequencies)) {
                choices[[length(choices) + 1L]] <- frequencies
            }
        }
    }
    if (!length(choices)) {
        return(NULL)
    }
    allowed <- Reduce(intersect, choices)
    if (!length(allowed)) {
        cli::cli_abort(
            "Pipeline components declare incompatible CMIP frequency requirements."
        )
    }
    allowed
}

# Convert either a component-provided envelope or its raw return value into the
# single runtime representation consumed by the next stage.
pipeline__stage_result <- function(component, value) {
    if (S7::S7_inherits(value, WeatherStageResult)) {
        if (!identical(value@stage, component@stage) ||
            !identical(value@component, component@name)) {
            cli::cli_abort(
                "Component {.val {component@stage}::{component@name}} returned an envelope for another component."
            )
        }
        if (!value@kind %in% component@output_kinds) {
            cli::cli_abort(
                "Component {.val {component@stage}::{component@name}} returned undeclared kind {.val {value@kind}}."
            )
        }
        return(value)
    }
    if (length(component@output_kinds) != 1L) {
        cli::cli_abort(
            "Component {.val {component@stage}::{component@name}} must return a WeatherStageResult when it declares multiple output kinds."
        )
    }
    diagnostics <- list()
    provenance <- list()
    if (S7::S7_inherits(value, SignalExecutionResult)) {
        diagnostics$signal <- value@diagnostics
        provenance$signal_profiles <- value@profiles
    }
    WeatherStageResult(
        stage = component@stage,
        component = component@name,
        kind = component@output_kinds[[1L]],
        value = value,
        diagnostics = diagnostics,
        provenance = provenance
    )
}

# Extract optional signal-setting overrides from backend options while keeping
# pipelines without configurable signal profiles on the empty-list default.
pipeline__signal_overrides <- function(options) {
    overrides <- options$signal_overrides
    if (is.null(overrides)) {
        return(list())
    }
    checkmate::assert_list(overrides, names = "unique")
    overrides
}

# Build the stage-specific operation arguments while retaining one generic
# executor. Signal components receive their shared group lifecycle contract;
# every other stage receives the previous typed value.
pipeline__operation_args <- function(
    component,
    plan,
    previous,
    context,
    options,
    stages
) {
    common <- list(
        inputs = plan@inputs,
        context = context,
        options = options
    )
    switch(
        component@stage,
        preprocess = common,
        calendar = c(list(data = previous@value), common),
        signal = list(
            inputs = plan@inputs,
            groups = previous@value,
            overrides = pipeline__signal_overrides(options),
            error_policy = "abort",
            warn_experimental = TRUE
        ),
        sequence = c(list(data = previous@value), common),
        hourly = c(list(data = previous@value), common),
        physics = c(list(data = previous@value), common),
        output = c(
            list(data = previous@value),
            common,
            list(stages = stages)
        )
    )
}

# Render one compact execution table suitable for retaining with a backend
# result without serializing process-local component functions.
pipeline__stage_table <- function(stages) {
    data.table::rbindlist(lapply(stages, function(result) {
        status <- "ok"
        message <- NA_character_
        signal <- result@diagnostics$signal
        if (is.data.frame(signal) && nrow(signal) &&
            any(signal[["status"]] == "error")) {
            status <- "error"
            message <- paste(
                signal[["message"]][signal[["status"]] == "error"],
                collapse = "; "
            )
        }
        data.table::data.table(
            stage = result@stage,
            component = result@component,
            kind = result@kind,
            status = status,
            message = message
        )
    }), use.names = TRUE, fill = TRUE)
}

# Execute one compiled pipeline in stage order and return both inspectable stage
# envelopes and the final backend result.
pipeline__execute <- function(plan, context, options = list()) {
    if (!S7::S7_inherits(plan, WeatherPipelinePlan)) {
        cli::cli_abort("{.arg plan} must be a WeatherPipelinePlan object.")
    }
    checkmate::assert_list(options, names = "unique")
    previous <- NULL
    stages <- list()
    for (stage in WEATHER_COMPONENT_STAGES) {
        component <- plan@components[[stage]]
        operation <- WEATHER_COMPONENT_PRIMARY_OPERATIONS[[stage]]
        args <- pipeline__operation_args(
            component,
            plan,
            previous,
            context,
            options,
            stages
        )
        value <- do.call(component__operation(component, operation), args)
        current <- pipeline__stage_result(component, value)
        stages[[stage]] <- current
        previous <- current
    }
    final <- previous@value
    if (inherits(final, "epw_morph_result")) {
        final$parts$component_pipeline <- pipeline__stage_table(stages)
    } else if (S7::S7_inherits(final, WeatherSequenceResult)) {
        # Sequence outputs retain the same inspectable component provenance as
        # representative-year results without flattening their member data.
        final@parts$component_pipeline <- pipeline__stage_table(stages)
    }
    WeatherPipelineExecution(
        plan = plan,
        stages = stages,
        result = final
    )
}

# Compile and execute a pipeline directly from the canonical backend context.
pipeline__run <- function(spec, context) {
    checkmate::assert_class(context, "morpher__context")
    plan <- pipeline__compile(spec, context$inputs)
    execution <- pipeline__execute(
        plan,
        context,
        options = context$recipe$options
    )
    execution@result
}
