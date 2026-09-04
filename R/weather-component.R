#' @include weather-input.R
NULL

# Component stages form the stable execution vocabulary shared by paper-
# faithful recipes, harmonized recipes, and external method adapters.
WEATHER_COMPONENT_STAGES <- c(
    "preprocess",
    "calendar",
    "signal",
    "sequence",
    "hourly",
    "physics",
    "output"
)

# Component scopes distinguish ordinary variable-wise methods from algorithms
# that must receive multiple variables or a spatial field together.
WEATHER_COMPONENT_SCOPES <- c(
    "univariate",
    "multivariate",
    "spatial"
)

# Each stage has one operation that makes a registered component executable;
# fitting, option validation, and diagnostics remain optional operations.
WEATHER_COMPONENT_PRIMARY_OPERATIONS <- c(
    preprocess = "apply",
    calendar = "apply",
    signal = "apply",
    sequence = "generate",
    hourly = "reconstruct",
    physics = "apply",
    output = "write"
)

# Allowed operation names describe stage semantics without imposing one common
# run signature on statistically different algorithms.
WEATHER_COMPONENT_ALLOWED_OPERATIONS <- list(
    preprocess = c("validate_options", "fit", "apply", "diagnose"),
    calendar = c("validate_options", "apply", "diagnose"),
    signal = c(
        "validate_options", "fit", "apply", "apply_group",
        "validate_result", "diagnose"
    ),
    sequence = c("validate_options", "fit", "generate", "diagnose"),
    hourly = c("validate_options", "fit", "reconstruct", "diagnose"),
    physics = c("validate_options", "apply", "diagnose"),
    output = c("validate_options", "write", "diagnose")
)

# Registered implementations live outside serialized recipes. Persisted
# workflows will retain stable component names and options, then resolve the
# executable functions from this process-local registry.
WEATHER_COMPONENT_REGISTRY <- new.env(parent = emptyenv())

# WeatherInputRequirement declares one role-specific component dependency.
# Alternative variable sets use outer OR and inner AND semantics.
WeatherInputRequirement <- S7::new_class(
    "WeatherInputRequirement",
    properties = list(
        role = S7::new_property(S7::class_character),
        representations = S7::new_property(
            S7::class_character,
            default = character()
        ),
        frequencies = S7::new_property(
            S7::class_character,
            default = character()
        ),
        calendars = S7::new_property(
            S7::class_character,
            default = character()
        ),
        variable_sets = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (length(self@role) != 1L ||
            is.na(self@role) ||
            !self@role %in% WEATHER_INPUT_ROLES) {
            return("`role` must identify one future-weather input role.")
        }
        for (property in c(
            "representations",
            "frequencies",
            "calendars"
        )) {
            value <- S7::prop(self, property)
            if (anyNA(value) || any(!nzchar(value)) || anyDuplicated(value)) {
                return(sprintf(
                    "`%s` must contain unique, non-missing, non-empty values.",
                    property
                ))
            }
        }
        if (length(self@representations) &&
            !all(self@representations %in% WEATHER_INPUT_REPRESENTATIONS)) {
            return("`representations` contains an unknown input representation.")
        }
        for (variable_set in self@variable_sets) {
            if (!is.character(variable_set) ||
                !length(variable_set) ||
                anyNA(variable_set) ||
                any(!nzchar(variable_set)) ||
                anyDuplicated(variable_set)) {
                return(paste(
                    "Every `variable_sets` entry must contain unique,",
                    "non-missing, non-empty variable IDs."
                ))
            }
        }
        NULL
    }
)

# Normalize alternative variable requirements once. One character vector is
# one required AND-set; the list of vectors represents alternative OR-sets.
component__variable_sets <- function(variable_sets) {
    if (is.null(variable_sets)) {
        return(list())
    }
    if (is.character(variable_sets)) {
        variable_sets <- list(variable_sets)
    }
    checkmate::assert_list(variable_sets)
    lapply(variable_sets, function(variable_set) {
        checkmate::assert_character(
            variable_set,
            any.missing = FALSE,
            min.len = 1L,
            unique = TRUE
        )
        if (any(!nzchar(variable_set))) {
            cli::cli_abort(
                "Variable requirement sets cannot contain empty IDs."
            )
        }
        as.character(variable_set)
    })
}

# Construct one role-specific requirement consumed by a component spec.
component__input_requirement <- function(
    role,
    representations = character(),
    frequencies = character(),
    calendars = character(),
    variable_sets = list()
) {
    checkmate::assert_choice(role, WEATHER_INPUT_ROLES)
    representations <- weather__descriptor_values(
        representations,
        "representations"
    )
    if (length(representations)) {
        checkmate::assert_subset(
            representations,
            WEATHER_INPUT_REPRESENTATIONS
        )
    }
    WeatherInputRequirement(
        role = role,
        representations = representations,
        frequencies = weather__descriptor_values(
            frequencies,
            "frequencies"
        ),
        calendars = weather__descriptor_values(calendars, "calendars"),
        variable_sets = component__variable_sets(variable_sets)
    )
}

# Validate and preserve named role requirements for component construction.
component__requirements <- function(requirements, name) {
    if (is.null(requirements)) {
        return(list())
    }
    checkmate::assert_list(requirements, names = "unique")
    if (length(requirements) &&
        (is.null(names(requirements)) || any(!nzchar(names(requirements))))) {
        cli::cli_abort("{.arg {name}} must be named by input role.")
    }
    unknown <- setdiff(names(requirements), WEATHER_INPUT_ROLES)
    if (length(unknown)) {
        cli::cli_abort(
            "{.arg {name}} contains unknown input role(s): {.val {unknown}}."
        )
    }
    for (role in names(requirements)) {
        requirement <- requirements[[role]]
        if (!S7::S7_inherits(requirement, WeatherInputRequirement)) {
            cli::cli_abort(
                "{.arg {name}} entry {.val {role}} must be a WeatherInputRequirement."
            )
        }
        if (!identical(requirement@role, role)) {
            cli::cli_abort(
                "{.arg {name}} entry {.val {role}} declares role {.val {requirement@role}}."
            )
        }
    }
    requirements
}

# WeatherComponentSpec describes one executable implementation independently
# of any complete future-weather recipe.
WeatherComponentSpec <- S7::new_class(
    "WeatherComponentSpec",
    properties = list(
        name = S7::new_property(S7::class_character),
        stage = S7::new_property(S7::class_character),
        label = S7::new_property(S7::class_character),
        required_inputs = S7::new_property(
            S7::class_list,
            default = list()
        ),
        optional_inputs = S7::new_property(
            S7::class_list,
            default = list()
        ),
        input_kinds = S7::new_property(S7::class_character),
        output_kinds = S7::new_property(S7::class_character),
        scopes = S7::new_property(S7::class_character),
        stochastic = S7::new_property(S7::class_logical),
        operations = S7::new_property(S7::class_list),
        metadata = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (length(self@name) != 1L ||
            is.na(self@name) ||
            !grepl("^[a-z][a-z0-9_]*$", self@name)) {
            return(
                "`name` must use lower snake_case and start with a letter."
            )
        }
        if (length(self@stage) != 1L ||
            is.na(self@stage) ||
            !self@stage %in% WEATHER_COMPONENT_STAGES) {
            return("`stage` must identify one future-weather component stage.")
        }
        if (length(self@label) != 1L ||
            is.na(self@label) ||
            !nzchar(self@label)) {
            return("`label` must be one non-empty string.")
        }
        overlap <- intersect(
            names(self@required_inputs),
            names(self@optional_inputs)
        )
        if (length(overlap)) {
            return(sprintf(
                "Input role(s) cannot be both required and optional: %s.",
                paste(overlap, collapse = ", ")
            ))
        }
        for (property in c("input_kinds", "output_kinds", "scopes")) {
            value <- S7::prop(self, property)
            if (!length(value) ||
                anyNA(value) ||
                any(!nzchar(value)) ||
                anyDuplicated(value)) {
                return(sprintf(
                    "`%s` must contain unique, non-missing, non-empty values.",
                    property
                ))
            }
        }
        if (!all(self@scopes %in% WEATHER_COMPONENT_SCOPES)) {
            return("`scopes` contains an unknown component scope.")
        }
        if (length(self@stochastic) != 1L || is.na(self@stochastic)) {
            return("`stochastic` must be one non-missing logical value.")
        }
        if (is.null(names(self@operations)) ||
            any(!nzchar(names(self@operations))) ||
            anyDuplicated(names(self@operations)) ||
            !all(vapply(self@operations, is.function, logical(1L)))) {
            return("`operations` must be a uniquely named list of functions.")
        }
        allowed <- WEATHER_COMPONENT_ALLOWED_OPERATIONS[[self@stage]]
        unknown <- setdiff(names(self@operations), allowed)
        if (length(unknown)) {
            return(sprintf(
                "Unknown `%s` operation(s): %s.",
                self@stage,
                paste(unknown, collapse = ", ")
            ))
        }
        primary <- WEATHER_COMPONENT_PRIMARY_OPERATIONS[[self@stage]]
        if (!primary %in% names(self@operations)) {
            return(sprintf(
                "`%s` components require a named `%s` operation.",
                self@stage,
                primary
            ))
        }
        NULL
    }
)

# Construct a component specification after normalizing all inspectable
# capabilities and executable operations.
component__spec <- function(
    name, stage, label = name,
    required_inputs = list(), optional_inputs = list(),
    input_kinds, output_kinds,
    scopes = "univariate", stochastic = FALSE,
    operations, metadata = list()
) {
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_choice(stage, WEATHER_COMPONENT_STAGES)
    checkmate::assert_string(label, min.chars = 1L)
    required_inputs <- component__requirements(
        required_inputs,
        "required_inputs"
    )
    optional_inputs <- component__requirements(
        optional_inputs,
        "optional_inputs"
    )
    overlap <- intersect(names(required_inputs), names(optional_inputs))
    if (length(overlap)) {
        cli::cli_abort(
            "Input role(s) cannot be both required and optional: {.val {overlap}}."
        )
    }
    input_kinds <- weather__descriptor_values(input_kinds, "input_kinds")
    output_kinds <- weather__descriptor_values(output_kinds, "output_kinds")
    scopes <- weather__descriptor_values(scopes, "scopes")
    checkmate::assert_subset(scopes, WEATHER_COMPONENT_SCOPES)
    checkmate::assert_flag(stochastic)
    checkmate::assert_list(operations, names = "unique")
    checkmate::assert_list(metadata, names = "unique")

    WeatherComponentSpec(
        name = name,
        stage = stage,
        label = label,
        required_inputs = required_inputs,
        optional_inputs = optional_inputs,
        input_kinds = input_kinds,
        output_kinds = output_kinds,
        scopes = scopes,
        stochastic = stochastic,
        operations = operations,
        metadata = metadata
    )
}

# Resolve a stable registry key without relying on environment nesting or list
# insertion order.
component__registry_key <- function(stage, name) {
    checkmate::assert_choice(stage, WEATHER_COMPONENT_STAGES)
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    paste(stage, name, sep = "::")
}

# Register one executable component while keeping serialized recipes free from
# process-specific function objects.
component__register <- function(
    component, overwrite = FALSE,
    registry = WEATHER_COMPONENT_REGISTRY
) {
    if (!S7::S7_inherits(component, WeatherComponentSpec)) {
        cli::cli_abort(
            "{.arg component} must be a WeatherComponentSpec object."
        )
    }
    checkmate::assert_flag(overwrite)
    checkmate::assert_environment(registry)
    key <- component__registry_key(component@stage, component@name)
    if (exists(key, envir = registry, inherits = FALSE) &&
        !isTRUE(overwrite)) {
        cli::cli_abort(
            "Weather component {.val {key}} is already registered."
        )
    }
    assign(key, component, envir = registry)
    invisible(component)
}

# Register one package-provided component without replacing a process-local
# extension that already owns the same stable registry key.
component__register_builtin <- function(
    component,
    registry = WEATHER_COMPONENT_REGISTRY
) {
    if (!S7::S7_inherits(component, WeatherComponentSpec)) {
        cli::cli_abort(
            "{.arg component} must be a WeatherComponentSpec object."
        )
    }
    checkmate::assert_environment(registry)
    key <- component__registry_key(component@stage, component@name)

    # Package-load registration is idempotent and must preserve an extension
    # installed earlier in the current R process.
    if (exists(key, envir = registry, inherits = FALSE)) {
        return(invisible(get(key, envir = registry, inherits = FALSE)))
    }
    component__register(component, registry = registry)
}

# Apply the package-provided registration policy consistently to a collection
# of component specifications owned by one backend or shared adapter.
component__register_builtins <- function(
    components,
    registry = WEATHER_COMPONENT_REGISTRY
) {
    checkmate::assert_list(components)
    checkmate::assert_environment(registry)
    for (component in components) {
        if (!S7::S7_inherits(component, WeatherComponentSpec)) {
            cli::cli_abort(
                "Every {.arg components} entry must be a WeatherComponentSpec object."
            )
        }
        component__register_builtin(component, registry = registry)
    }
    invisible(NULL)
}

# Retrieve one registered component by its stage and stable name.
component__get <- function(
    stage, name,
    registry = WEATHER_COMPONENT_REGISTRY
) {
    checkmate::assert_environment(registry)
    key <- component__registry_key(stage, name)
    if (!exists(key, envir = registry, inherits = FALSE)) {
        cli::cli_abort("Unknown weather component: {.val {key}}.")
    }
    get(key, envir = registry, inherits = FALSE)
}

# Return inspectable registry metadata without exposing executable functions.
component__list <- function(
    stage = NULL,
    registry = WEATHER_COMPONENT_REGISTRY
) {
    checkmate::assert_choice(
        stage,
        WEATHER_COMPONENT_STAGES,
        null.ok = TRUE
    )
    checkmate::assert_environment(registry)
    keys <- ls(envir = registry, all.names = TRUE)
    components <- lapply(keys, get, envir = registry, inherits = FALSE)
    if (!is.null(stage)) {
        components <- Filter(
            function(component) identical(component@stage, stage),
            components
        )
    }
    if (!length(components)) {
        return(data.table::data.table(
            stage = character(),
            name = character(),
            label = character(),
            input_kinds = character(),
            output_kinds = character(),
            scopes = character(),
            stochastic = logical()
        ))
    }
    out <- data.table::rbindlist(lapply(components, function(component) {
        data.table::data.table(
            stage = component@stage,
            name = component@name,
            label = component@label,
            input_kinds = paste(component@input_kinds, collapse = ","),
            output_kinds = paste(component@output_kinds, collapse = ","),
            scopes = paste(component@scopes, collapse = ","),
            stochastic = component@stochastic
        )
    }))
    # Use explicit column access so package checks do not treat temporary
    # ordering columns as unresolved symbols.
    data.table::set(
        out,
        j = ".stage_order",
        value = match(out[["stage"]], WEATHER_COMPONENT_STAGES)
    )
    data.table::setorderv(out, c(".stage_order", "name"))
    data.table::set(out, j = ".stage_order", value = NULL)
    out[]
}

# Report whether an upstream component can feed a later component according to
# declared stage order and at least one shared intermediate data kind.
component__compatible <- function(upstream, downstream) {
    if (!S7::S7_inherits(upstream, WeatherComponentSpec) ||
        !S7::S7_inherits(downstream, WeatherComponentSpec)) {
        cli::cli_abort(
            "`upstream` and `downstream` must be WeatherComponentSpec objects."
        )
    }
    upstream_order <- match(upstream@stage, WEATHER_COMPONENT_STAGES)
    downstream_order <- match(downstream@stage, WEATHER_COMPONENT_STAGES)
    upstream_order < downstream_order &&
        length(intersect(
            upstream@output_kinds,
            downstream@input_kinds
        )) > 0L
}

# Fail early with the precise stage or data-kind incompatibility that prevents
# two components from being composed.
component__assert_compatible <- function(upstream, downstream) {
    if (component__compatible(upstream, downstream)) {
        return(invisible(TRUE))
    }
    upstream_order <- match(upstream@stage, WEATHER_COMPONENT_STAGES)
    downstream_order <- match(downstream@stage, WEATHER_COMPONENT_STAGES)
    if (upstream_order >= downstream_order) {
        cli::cli_abort(
            "Component {.val {downstream@stage}::{downstream@name}} must follow {.val {upstream@stage}::{upstream@name}} in stage order."
        )
    }
    cli::cli_abort(c(
        "Component {.val {upstream@stage}::{upstream@name}} cannot feed {.val {downstream@stage}::{downstream@name}}.",
        "x" = "Produced kind(s): {.val {upstream@output_kinds}}.",
        "x" = "Accepted kind(s): {.val {downstream@input_kinds}}."
    ))
}

# Check one concrete input against a role-specific component requirement.
component__requirement_errors <- function(requirement, input) {
    errors <- character()
    if (length(requirement@representations) &&
        !input@representation %in% requirement@representations) {
        errors <- c(
            errors,
            sprintf(
                "role `%s` representation `%s` is unsupported",
                requirement@role,
                input@representation
            )
        )
    }
    for (property in c("frequencies", "calendars")) {
        required <- S7::prop(requirement, property)
        if (!length(required)) {
            next
        }
        available <- S7::prop(input, property)
        if (!length(available) || !all(available %in% required)) {
            shown <- if (length(available)) {
                paste(available, collapse = ", ")
            } else {
                "<missing>"
            }
            errors <- c(
                errors,
                sprintf(
                    "role `%s` %s `%s` do not satisfy `%s`",
                    requirement@role,
                    property,
                    shown,
                    paste(required, collapse = ", ")
                )
            )
        }
    }
    if (length(requirement@variable_sets)) {
        matched <- vapply(
            requirement@variable_sets,
            function(variable_set) all(variable_set %in% input@variables),
            logical(1L)
        )
        if (!any(matched)) {
            alternatives <- vapply(
                requirement@variable_sets,
                paste,
                character(1L),
                collapse = " + "
            )
            errors <- c(
                errors,
                sprintf(
                    "role `%s` lacks variable alternative `%s`",
                    requirement@role,
                    paste(alternatives, collapse = "` or `")
                )
            )
        }
    }
    errors
}

# Validate all required inputs and every supplied optional input before a
# component starts fitting or transforming data.
component__input_errors <- function(component, inputs) {
    if (!S7::S7_inherits(component, WeatherComponentSpec)) {
        cli::cli_abort(
            "{.arg component} must be a WeatherComponentSpec object."
        )
    }
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    errors <- character()
    for (role in names(component@required_inputs)) {
        input <- weather__get_input(inputs, role)
        if (is.null(input)) {
            errors <- c(errors, sprintf("required role `%s` is missing", role))
            next
        }
        errors <- c(
            errors,
            component__requirement_errors(
                component@required_inputs[[role]],
                input
            )
        )
    }
    for (role in names(component@optional_inputs)) {
        input <- weather__get_input(inputs, role)
        if (is.null(input)) {
            next
        }
        errors <- c(
            errors,
            component__requirement_errors(
                component@optional_inputs[[role]],
                input
            )
        )
    }
    unique(errors)
}

# Abort with all input-contract failures together so discovery and workflow
# planning can explain every missing requirement in one pass.
component__validate_inputs <- function(component, inputs) {
    errors <- component__input_errors(component, inputs)
    if (length(errors)) {
        cli::cli_abort(c(
            "Weather component input requirements are not satisfied.",
            "x" = errors
        ))
    }
    invisible(TRUE)
}

# Resolve one executable operation from a registered component specification.
component__operation <- function(component, operation) {
    if (!S7::S7_inherits(component, WeatherComponentSpec)) {
        cli::cli_abort(
            "{.arg component} must be a WeatherComponentSpec object."
        )
    }
    checkmate::assert_string(operation, min.chars = 1L)
    if (!operation %in% names(component@operations)) {
        cli::cli_abort(
            "Component {.val {component@stage}::{component@name}} does not implement operation {.val {operation}}."
        )
    }
    component@operations[[operation]]
}

# Execute one named component operation without embedding function objects in a
# recipe or persisted task specification.
component__execute <- function(component, operation, ...) {
    component__operation(component, operation)(...)
}
