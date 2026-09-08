#' @include weather-recipe.R epw-morph-recipe.R
NULL

# Public transform scales describe where the climate signal is calculated,
# independently of source and delivered weather frequencies.
WEATHER_TRANSFORM_SCALES <- c("monthly", "daily", "hourly")

# WeatherTransformSpec is the reusable public method boundary. It deliberately
# contains no site, climate, reference period, model identity, or output path.
WeatherTransformSpec <- S7::new_class(
    "WeatherTransformSpec",
    properties = list(
        scale = S7::new_property(S7::class_character),
        method = S7::new_property(S7::class_character),
        label = S7::new_property(S7::class_character),
        recipe = S7::new_property(S7::class_character),
        recipe_version = S7::new_property(S7::class_integer),
        reconstruction = S7::new_property(S7::class_character),
        options = S7::new_property(S7::class_list, default = list()),
        required_inputs = S7::new_property(S7::class_list),
        optional_inputs = S7::new_property(
            S7::class_list,
            default = list()
        ),
        source_frequencies = S7::new_property(S7::class_list),
        statistical_grouping = S7::new_property(S7::class_character),
        output_frequency = S7::new_property(S7::class_character),
        output_type = S7::new_property(S7::class_character),
        stochastic = S7::new_property(S7::class_logical),
        stochastic_variables = S7::new_property(
            S7::class_character,
            default = character()
        ),
        evidence = S7::new_property(S7::class_character),
        references = S7::new_property(
            S7::class_character,
            default = character()
        ),
        status = S7::new_property(S7::class_character)
    ),
    validator = function(self) {
        if (length(self@scale) != 1L ||
            is.na(self@scale) ||
            !self@scale %in% WEATHER_TRANSFORM_SCALES) {
            return("`scale` must be `monthly`, `daily`, or `hourly`.")
        }
        for (property in c(
            "method",
            "label",
            "recipe",
            "reconstruction",
            "statistical_grouping",
            "output_frequency",
            "output_type",
            "evidence",
            "status"
        )) {
            value <- S7::prop(self, property)
            if (length(value) != 1L || is.na(value) || !nzchar(value)) {
                return(sprintf("`%s` must be one non-empty string.", property))
            }
        }
        if (length(self@recipe_version) != 1L ||
            is.na(self@recipe_version) ||
            self@recipe_version < 1L) {
            return("`recipe_version` must be one positive integer.")
        }
        if (is.null(names(self@required_inputs)) ||
            any(!nzchar(names(self@required_inputs)))) {
            return("`required_inputs` must be a named role contract.")
        }
        if (length(self@optional_inputs) &&
            (is.null(names(self@optional_inputs)) ||
                any(!nzchar(names(self@optional_inputs))))) {
            return("`optional_inputs` must be a named role contract.")
        }
        if (length(self@stochastic) != 1L || is.na(self@stochastic)) {
            return("`stochastic` must be one non-missing logical value.")
        }
        if (anyNA(self@stochastic_variables) ||
            any(!nzchar(self@stochastic_variables)) ||
            anyDuplicated(self@stochastic_variables)) {
            return(
                "`stochastic_variables` must contain unique, non-empty variable IDs."
            )
        }
        if (!identical(
            self@stochastic,
            length(self@stochastic_variables) > 0L
        )) {
            return(paste(
                "`stochastic` must agree with whether",
                "`stochastic_variables` is non-empty."
            ))
        }
        NULL
    }
)

# Declare every public selection tuple in one place. Internal recipe keys stay
# stable while concise method keys and scientific scale remain user-facing.
transform__records <- function() {
    list(
        list(
            scale = "monthly",
            method = "belcher",
            recipe = "belcher_monthly",
            reconstructions = "belcher_field_equations",
            default_reconstruction = "belcher_field_equations",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "monthly",
            method = "epwshiftr",
            recipe = "epwshiftr_monthly",
            reconstructions = "enhanced_field_equations",
            default_reconstruction = "enhanced_field_equations",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "monthly",
            method = "eames",
            recipe = "eames_monthly_temperature",
            reconstructions = "btws",
            default_reconstruction = "btws",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "epwshiftr",
            recipe = c(
                power = "epwshiftr_daily_power",
                btws = "epwshiftr_daily_btws"
            ),
            reconstructions = c("power", "btws"),
            default_reconstruction = "power",
            statistical_grouping = "circular_daily_climatology"
        ),
        list(
            scale = "daily",
            method = "ek",
            recipe = "ek_daily_factors",
            reconstructions = "ek_hourly_transform",
            default_reconstruction = "ek_hourly_transform",
            statistical_grouping = "calendar_neutral_day"
        ),
        list(
            scale = "daily",
            method = "arima",
            recipe = "monthly_percentile_temperature",
            reconstructions = "daily_additive_application",
            default_reconstruction = "daily_additive_application",
            statistical_grouping = "calendar_month_distribution"
        ),
        list(
            scale = "daily",
            method = "sobie_curry",
            recipe = "sobie_curry_daily",
            reconstructions = "sobie_curry_field_transforms",
            default_reconstruction = "sobie_curry_field_transforms",
            statistical_grouping = "circular_daily_window"
        ),
        list(
            scale = "daily",
            method = "linear_scaling",
            recipe = "linear_scaling_daily_temperature",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "delta_change",
            recipe = "delta_change_daily_temperature",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "qm",
            recipe = "quantile_mapping_daily_temperature",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "circular_daily_window"
        ),
        list(
            scale = "daily",
            method = "qdm",
            recipe = "quantile_delta_mapping_daily_temperature",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "circular_daily_window"
        ),
        list(
            scale = "daily",
            method = "sdm",
            recipe = "scaled_distribution_mapping_daily_temperature",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "cdf_t",
            recipe = "cdf_transform_daily_temperature",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "edcdfm",
            recipe = "equidistant_cdf_matching_daily_temperature",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "isimip3basd",
            recipe = "isimip3basd_daily_temperature",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "circular_daily_window"
        ),
        list(
            scale = "hourly",
            method = "kernel_qdm",
            recipe = "hourly_kernel_qdm",
            reconstructions = "direct_model_hourly_mapping",
            default_reconstruction = "direct_model_hourly_mapping",
            statistical_grouping = "centered_three_month_window"
        )
    )
}

# Resolve one public scale/method/reconstruction tuple and reject fixed-method
# reconstruction arguments before constructing any internal recipe.
transform__record <- function(scale, method, reconstruction = NULL) {
    checkmate::assert_choice(scale, WEATHER_TRANSFORM_SCALES)
    checkmate::assert_string(method, min.chars = 1L)
    checkmate::assert_string(
        reconstruction,
        min.chars = 1L,
        null.ok = TRUE
    )
    method <- tolower(method)
    records <- Filter(
        function(record) {
            identical(record$scale, scale) &&
                identical(record$method, method)
        },
        transform__records()
    )
    if (!length(records)) {
        available <- unique(vapply(
            Filter(
                function(record) identical(record$scale, scale),
                transform__records()
            ),
            `[[`,
            character(1L),
            "method"
        ))
        cli::cli_abort(c(
            "Unknown {scale} weather transformation {.val {method}}.",
            "i" = "Available methods: {.val {available}}."
        ))
    }
    record <- records[[1L]]
    choices <- record$reconstructions
    if (length(choices) == 1L && !is.null(reconstruction)) {
        cli::cli_abort(c(
            "Weather transformation {.val {method}} has a fixed hourly reconstruction.",
            "i" = "Omit {.arg reconstruction}; it resolves to {.val {choices}}."
        ))
    }
    if (is.null(reconstruction)) {
        reconstruction <- record$default_reconstruction
    } else {
        reconstruction <- tolower(reconstruction)
        checkmate::assert_choice(reconstruction, choices)
    }
    recipe <- record$recipe
    if (length(recipe) > 1L) {
        recipe <- unname(recipe[[reconstruction]])
    }
    record$recipe <- recipe
    record$reconstruction <- reconstruction
    record
}

# Resolve variable-level signal settings against the selected method schema so
# unknown public options fail before data access or workflow persistence.
transform__signal_options <- function(method, options) {
    if (!length(options)) {
        return(list())
    }
    parameters <- method@parameters
    if (identical(method@output_variables, "tas")) {
        allowed <- names(parameters$tas)
        unknown <- setdiff(names(options), allowed)
        if (length(unknown)) {
            cli::cli_abort(
                "Unknown transformation option(s): {.val {unknown}}."
            )
        }
        return(list(signal_overrides = list(tas = options)))
    }
    unknown <- setdiff(names(options), names(parameters))
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown transformation variable option(s): {.val {unknown}}."
        )
    }
    for (variable in names(options)) {
        checkmate::assert_list(options[[variable]], names = "unique")
        invalid <- setdiff(
            names(options[[variable]]),
            names(parameters[[variable]])
        )
        if (length(invalid)) {
            cli::cli_abort(
                "Unknown {.val {variable}} transformation option(s): {.val {invalid}}."
            )
        }
    }
    list(signal_overrides = options)
}

# Convert public scientific options to the existing canonical recipe option
# contract, retaining the current numerical defaults and validators.
transform__recipe_options <- function(record, options) {
    checkmate::assert_list(options, names = "unique")
    recipe_spec <- recipe__get(record$recipe)
    method <- method__get(recipe_spec@method)
    if (startsWith(recipe_spec@backend, "daily_adjustment_") ||
        identical(recipe_spec@backend, "hourly_kernel_qdm")) {
        return(transform__signal_options(method, options))
    }
    defaults <- epw_morph_recipe(record$recipe)$options
    unknown <- setdiff(names(options), names(defaults))
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown transformation option(s): {.val {unknown}}."
        )
    }
    utils::modifyList(defaults, options)
}

# Expand canonical variable alternatives into complete source-variable sets.
# Each returned vector is one valid AND-set and the list retains OR semantics.
transform__variable_sets <- function(requirements) {
    variable_sets <- list(character())
    for (alternatives in unname(requirements)) {
        variable_sets <- unlist(lapply(variable_sets, function(current) {
            lapply(alternatives, function(alternative) {
                unique(c(current, as.character(alternative)))
            })
        }), recursive = FALSE)
    }
    unname(variable_sets)
}

# Rebuild one input requirement while preserving every descriptor except the
# option-dependent variable alternatives supplied by the resolved recipe.
transform__with_variable_sets <- function(requirement, variable_sets) {
    component__input_requirement(
        role = requirement@role,
        representations = requirement@representations,
        frequencies = requirement@frequencies,
        variable_frequencies = requirement@variable_frequencies,
        calendars = requirement@calendars,
        variable_sets = variable_sets
    )
}

# Resolve option-dependent Belcher source variables after recipe construction.
# Other recipes already declare fixed role contracts in their canonical specs.
transform__input_contracts <- function(recipe_spec, recipe) {
    required_inputs <- recipe_spec@required_inputs
    optional_inputs <- recipe_spec@optional_inputs
    if (!recipe$backend %in% c("belcher", "belcher_absolute")) {
        return(list(
            required_inputs = required_inputs,
            optional_inputs = optional_inputs
        ))
    }

    variable_sets <- transform__variable_sets(
        morpher__variable_requirements(recipe)
    )
    model_roles <- c("model_historical", "model_future")
    for (role in intersect(model_roles, names(required_inputs))) {
        required_inputs[[role]] <- transform__with_variable_sets(
            required_inputs[[role]],
            variable_sets
        )
    }
    for (role in intersect(model_roles, names(optional_inputs))) {
        optional_inputs[[role]] <- transform__with_variable_sets(
            optional_inputs[[role]],
            variable_sets
        )
    }
    list(
        required_inputs = required_inputs,
        optional_inputs = optional_inputs
    )
}

# Return the source-frequency contract by semantic role without collapsing
# mixed-frequency variables into an ambiguous scalar.
transform__source_frequencies <- function(required_inputs, optional_inputs) {
    requirements <- c(
        required_inputs,
        optional_inputs
    )
    requirements <- requirements[
        setdiff(names(requirements), "weather_template")
    ]
    lapply(requirements, function(requirement) {
        if (length(requirement@variable_frequencies)) {
            return(requirement@variable_frequencies)
        }
        requirement@frequencies
    })
}

# Build the public immutable object from one already validated canonical recipe
# so new construction and persisted-plan restoration share the same boundary.
transform__from_recipe <- function(record, recipe) {
    recipe_spec <- recipe__get(
        recipe$recipe_spec,
        version = recipe$recipe_version
    )
    method_spec <- method__get(recipe_spec@method)
    inputs <- transform__input_contracts(recipe_spec, recipe)
    stochastic_variables <- if (isTRUE(recipe_spec@stochastic)) {
        intersect(
            method_spec@stochastic_variables,
            epw_morph_variables(recipe)
        )
    } else {
        character()
    }
    WeatherTransformSpec(
        scale = record$scale,
        method = record$method,
        label = recipe_spec@label,
        recipe = recipe_spec@name,
        recipe_version = recipe_spec@version,
        reconstruction = record$reconstruction,
        options = recipe$options,
        required_inputs = inputs$required_inputs,
        optional_inputs = inputs$optional_inputs,
        source_frequencies = transform__source_frequencies(
            inputs$required_inputs,
            inputs$optional_inputs
        ),
        statistical_grouping = record$statistical_grouping,
        output_frequency = "hour",
        output_type = recipe_spec@output_type,
        stochastic = recipe_spec@stochastic,
        stochastic_variables = stochastic_variables,
        evidence = method_spec@evidence,
        references = method_spec@references,
        status = recipe_spec@status
    )
}

# Resolve an existing stored recipe to its unique public tuple. This supports
# store-backed output and retry operations without exposing recipe identifiers
# in current user-facing commands.
transform__from_recipe_object <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("Cannot resolve a weather transform from an invalid recipe.")
    }
    recipe_spec <- morpher__recipe_spec(recipe)
    recipe_name <- if (is.null(recipe_spec)) {
        recipe$name
    } else {
        recipe_spec@name
    }
    matches <- list()
    for (record in transform__records()) {
        recipe_names <- unname(record$recipe)
        index <- match(recipe_name, recipe_names)
        if (is.na(index)) {
            next
        }
        selected_reconstruction <- record$reconstructions[[index]]
        record$recipe <- recipe_name
        record$reconstruction <- selected_reconstruction
        matches[[length(matches) + 1L]] <- record
    }
    if (length(matches) != 1L) {
        cli::cli_abort(c(
            "Stored recipe {.val {recipe_name}} does not resolve to one current weather transformation.",
            "i" = "Create a new run with a transform returned by a scale-specific constructor."
        ))
    }
    transform__from_recipe(matches[[1L]], recipe)
}

# Construct one validated reusable transform from the public tuple and options.
transform__new <- function(scale, method, reconstruction = NULL, options = list()) {
    record <- transform__record(scale, method, reconstruction)
    recipe_options <- transform__recipe_options(record, options)
    recipe <- epw_morph_recipe(
        record$recipe,
        options = recipe_options
    )
    transform__from_recipe(record, recipe)
}

# Restore the internal executable recipe without exposing its backend, profile,
# policy, or component identifiers through the public constructor surface.
transform__recipe <- function(transform) {
    if (!S7::S7_inherits(transform, WeatherTransformSpec)) {
        cli::cli_abort(
            "{.arg transform} must be a {.cls WeatherTransformSpec}."
        )
    }
    epw_morph_recipe(
        transform@recipe,
        version = transform@recipe_version,
        options = transform@options
    )
}

# Serialize only reusable transform intent; execution data and internal recipe
# implementation details remain separate plan records.
transform__spec_value <- function(transform) {
    if (!S7::S7_inherits(transform, WeatherTransformSpec)) {
        cli::cli_abort(
            "{.arg transform} must be a {.cls WeatherTransformSpec}."
        )
    }
    list(
        scale = transform@scale,
        method = transform@method,
        recipe = transform@recipe,
        recipe_version = transform@recipe_version,
        reconstruction = transform@reconstruction,
        options = unclass(transform@options)
    )
}

# Restore a persisted transform only when its public tuple still resolves to
# the recorded canonical recipe and version.
transform__from_spec <- function(spec) {
    if (!is.list(spec)) {
        cli::cli_abort("Persisted weather transform specification is invalid.")
    }
    required <- c(
        "scale",
        "method",
        "recipe",
        "recipe_version",
        "reconstruction",
        "options"
    )
    missing <- setdiff(required, names(spec))
    if (length(missing)) {
        cli::cli_abort(
            "Persisted weather transform is missing field(s): {.val {missing}}."
        )
    }
    base_record <- transform__record(
        as.character(spec$scale),
        as.character(spec$method)
    )
    record <- transform__record(
        as.character(spec$scale),
        as.character(spec$method),
        if (length(base_record$reconstructions) > 1L) {
            as.character(spec$reconstruction)
        } else {
            NULL
        }
    )
    if (!identical(record$recipe, as.character(spec$recipe))) {
        cli::cli_abort(
            "Persisted weather transform no longer resolves to its recorded canonical recipe."
        )
    }
    recipe <- epw_morph_recipe(
        record$recipe,
        version = as.integer(spec$recipe_version),
        options = shift_coalesce(spec$options, list())
    )
    transform__from_recipe(record, recipe)
}

# Test whether a transform declares one semantic role in its complete recipe
# contract, including optional inputs where relevant.
transform__accepts_input <- function(transform, role) {
    role %in% c(
        names(transform@required_inputs),
        names(transform@optional_inputs)
    )
}

# Test whether execution must supply one semantic role before any catalog or
# source-data access starts.
transform__requires_input <- function(transform, role) {
    role %in% names(transform@required_inputs)
}

# Validate execution-owned reference roles without allowing model output to be
# substituted silently for observed weather.
transform__validate_execution_inputs <- function(
    transform,
    reference = NULL,
    observed_reference = NULL
) {
    if (!S7::S7_inherits(transform, WeatherTransformSpec)) {
        cli::cli_abort(
            "{.arg transform} must be a {.cls WeatherTransformSpec}."
        )
    }
    values <- list(
        reference = reference,
        observed_reference = observed_reference
    )
    roles <- c(
        reference = "model_historical",
        observed_reference = "observed_reference"
    )
    for (name in names(values)) {
        value <- values[[name]]
        role <- roles[[name]]
        if (is.null(value)) {
            if (transform__requires_input(transform, role)) {
                cli::cli_abort(
                    "Weather transformation {.val {transform@method}} requires {.arg {name}}."
                )
            }
            next
        }
        if (!transform__accepts_input(transform, role)) {
            cli::cli_abort(
                "Weather transformation {.val {transform@method}} does not use {.arg {name}}."
            )
        }
        if (!S7::S7_inherits(value, ShiftReferenceSpec) &&
            !S7::S7_inherits(value, ShiftClimate)) {
            cli::cli_abort(
                "{.arg {name}} must be a {.cls ShiftReferenceSpec} or a {.cls ShiftClimate} stage."
            )
        }
    }
    if (S7::S7_inherits(observed_reference, ShiftReferenceSpec) &&
        !identical(observed_reference@mode, "plan")) {
        cli::cli_abort(
            paste(
                "{.arg observed_reference} must use a plan-backed reference;",
                "automatic historical CMIP resolution produces model output,",
                "not observations."
            )
        )
    }
    invisible(TRUE)
}

#' Configure a built-in future-weather transformation
#'
#' `monthly_transform()`, `daily_transform()`, and `hourly_transform()` select
#' a scientific method on its transformation scale. The resulting
#' `WeatherTransformSpec` contains no climate data, EPW template, period, site,
#' or output path and can therefore be reused across runs.
#'
#' @param method A method key returned by [weather_transforms()].
#' @param reconstruction An hourly reconstruction choice. Supply this only for
#'   methods that list more than one permitted reconstruction.
#' @param ... Method-specific scientific settings. Unknown settings are errors.
#'
#' @return A reusable `WeatherTransformSpec`.
#'
#' @seealso [weather_transforms()], [shift_future_epw()]
#' @export
monthly_transform <- function(method, reconstruction = NULL, ...) {
    transform__new(
        "monthly",
        method,
        reconstruction,
        options = list(...)
    )
}

#' @rdname monthly_transform
#' @export
daily_transform <- function(method, reconstruction = NULL, ...) {
    transform__new(
        "daily",
        method,
        reconstruction,
        options = list(...)
    )
}

#' @rdname monthly_transform
#' @export
hourly_transform <- function(method, reconstruction = NULL, ...) {
    transform__new(
        "hourly",
        method,
        reconstruction,
        options = list(...)
    )
}

#' List built-in future-weather transformations
#'
#' @description
#' `weather_transforms()` lists the same public scale, method, reconstruction,
#' source-frequency, input-role, evidence, and output contracts used by the
#' three transform constructors.
#'
#' @return A data table with one row per selectable configuration.
#'
#' @seealso [monthly_transform()]
#' @export
weather_transforms <- function() {
    rows <- lapply(transform__records(), function(record) {
        lapply(record$reconstructions, function(reconstruction) {
            selected <- transform__record(
                record$scale,
                record$method,
                if (length(record$reconstructions) > 1L) {
                    reconstruction
                } else {
                    NULL
                }
            )
            transform <- transform__new(
                selected$scale,
                selected$method,
                if (length(record$reconstructions) > 1L) {
                    selected$reconstruction
                } else {
                    NULL
                }
            )
            data.table::data.table(
                scale = transform@scale,
                method = transform@method,
                label = transform@label,
                reconstruction = transform@reconstruction,
                default_reconstruction = identical(
                    transform@reconstruction,
                    record$default_reconstruction
                ),
                required_inputs = list(transform@required_inputs),
                optional_inputs = list(transform@optional_inputs),
                source_frequencies = list(transform@source_frequencies),
                statistical_grouping = transform@statistical_grouping,
                output_frequency = transform@output_frequency,
                output_type = transform@output_type,
                stochastic = transform@stochastic,
                stochastic_variables = list(
                    transform@stochastic_variables
                ),
                evidence = transform@evidence,
                references = list(transform@references),
                status = transform@status
            )
        })
    })
    data.table::rbindlist(
        unlist(rows, recursive = FALSE),
        use.names = TRUE,
        fill = TRUE
    )[]
}

# Format one role's alternative variable sets without leaking internal recipe
# or component identifiers into the public transform print method.
transform__format_variable_sets <- function(requirements) {
    requirements <- Filter(
        function(requirement) length(requirement@variable_sets),
        requirements
    )
    if (!length(requirements)) {
        return("none")
    }
    values <- vapply(names(requirements), function(role) {
        sets <- vapply(requirements[[role]]@variable_sets, function(variables) {
            paste(variables, collapse = " + ")
        }, character(1L))
        sprintf("%s: %s", role, paste(sets, collapse = " or "))
    }, character(1L))
    paste(values, collapse = "; ")
}

# Format scalar and variable-specific frequencies by semantic input role so a
# mixed-frequency transform remains fully inspectable at the console.
transform__format_source_frequencies <- function(source_frequencies) {
    if (!length(source_frequencies)) {
        return("none")
    }
    values <- vapply(names(source_frequencies), function(role) {
        frequency <- source_frequencies[[role]]
        if (length(frequency) && !is.null(names(frequency)) &&
            all(nzchar(names(frequency)))) {
            frequency <- paste(
                sprintf(
                    "%s=%s",
                    names(frequency),
                    vapply(frequency, function(value) {
                        paste(value, collapse = "/")
                    }, character(1L))
                ),
                collapse = ", "
            )
        } else {
            frequency <- paste(unlist(frequency, use.names = FALSE),
                collapse = "/")
        }
        sprintf("%s: %s", role, frequency)
    }, character(1L))
    paste(values, collapse = "; ")
}

# Present the five distinct temporal concepts and the required input roles
# without exposing internal backend, policy, profile, or component identifiers.
S7::method(print, WeatherTransformSpec) <- function(x, ...) {
    esg__print_header("Weather Transform")
    esg__print_facts(list(
        "Method" = x@label,
        "Transformation scale" = x@scale,
        "Required source frequency" = transform__format_source_frequencies(
            x@source_frequencies
        ),
        "Statistical grouping" = x@statistical_grouping,
        "Hourly reconstruction" = x@reconstruction,
        "Output frequency" = x@output_frequency,
        "Output type" = x@output_type,
        "Required inputs" = paste(names(x@required_inputs), collapse = ", "),
        "Required variables" = transform__format_variable_sets(
            x@required_inputs
        ),
        "Optional inputs" = paste(names(x@optional_inputs), collapse = ", "),
        "Optional variables" = transform__format_variable_sets(
            x@optional_inputs
        ),
        "Stochastic variables" = if (length(x@stochastic_variables)) {
            paste(x@stochastic_variables, collapse = ", ")
        } else {
            "none"
        },
        "Evidence" = x@evidence,
        "References" = paste(x@references, collapse = ", "),
        "Status" = x@status
    ))
    invisible(x)
}
