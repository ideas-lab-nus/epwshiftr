#' @include weather-recipe.R epw-morph-recipe.R
NULL

# Public transform scales describe where the climate signal is calculated,
# independently of source and delivered weather frequencies.
WEATHER_TRANSFORM_SCALES <- c("monthly", "daily", "hourly")

# Evidence labels form part of the public transformation catalog and persisted
# provenance, so additions must use one documented scientific-status category.
WEATHER_TRANSFORM_EVIDENCE <- c(
    "published",
    "package_method",
    "adapted_publication",
    "reconstructed_publication",
    "experimental"
)

# Cache only successful validation of the package's immutable built-in mapping;
# direct validator calls remain available to tests and registry maintenance.
WEATHER_TRANSFORM_REGISTRY_STATE <- new.env(parent = emptyenv())

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
        reconstruction_label = S7::new_property(S7::class_character),
        options = S7::new_property(S7::class_list, default = list()),
        required_inputs = S7::new_property(S7::class_list),
        optional_inputs = S7::new_property(
            S7::class_list,
            default = list()
        ),
        source_frequencies = S7::new_property(S7::class_list),
        optional_source_frequencies = S7::new_property(
            S7::class_list,
            default = list()
        ),
        optional_variables = S7::new_property(
            S7::class_list,
            default = list()
        ),
        optional_variable_frequencies = S7::new_property(
            S7::class_list,
            default = list()
        ),
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
            "reconstruction_label",
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
        if (!self@evidence %in% WEATHER_TRANSFORM_EVIDENCE) {
            return("`evidence` must use one supported evidence category.")
        }
        if (!self@status %in% WEATHER_RECIPE_STATUSES) {
            return("`status` must use one supported recipe status.")
        }
        if (!length(self@references) || anyNA(self@references) ||
            any(!nzchar(self@references)) || anyDuplicated(self@references)) {
            return(
                "`references` must contain unique, non-empty scientific references."
            )
        }
        if (length(self@options) &&
            (is.null(names(self@options)) || any(!nzchar(names(self@options))) ||
                anyDuplicated(names(self@options)))) {
            return("`options` must be uniquely named scientific settings.")
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
        for (contract_name in c("required_inputs", "optional_inputs")) {
            contract <- S7::prop(self, contract_name)
            if (anyDuplicated(names(contract))) {
                return(sprintf(
                    "`%s` must use unique input roles.",
                    contract_name
                ))
            }
            for (role in names(contract)) {
                requirement <- contract[[role]]
                if (!S7::S7_inherits(
                    requirement,
                    WeatherInputRequirement
                ) ||
                    !identical(requirement@role, role)) {
                    return(sprintf(
                        "`%s` role `%s` must contain a matching WeatherInputRequirement.",
                        contract_name,
                        role
                    ))
                }
            }
        }
        overlap <- intersect(
            names(self@required_inputs),
            names(self@optional_inputs)
        )
        if (length(overlap)) {
            return("An input role cannot be both required and optional.")
        }
        for (property in c(
            "source_frequencies",
            "optional_source_frequencies",
            "optional_variables",
            "optional_variable_frequencies"
        )) {
            value <- S7::prop(self, property)
            if (length(value) &&
                (is.null(names(value)) || any(!nzchar(names(value))) ||
                    anyDuplicated(names(value)))) {
                return(sprintf(
                    "`%s` must be uniquely named by input role.",
                    property
                ))
            }
        }
        required_frequency_roles <- setdiff(
            names(self@required_inputs),
            "weather_template"
        )
        optional_frequency_roles <- setdiff(
            names(self@optional_inputs),
            "weather_template"
        )
        if (!setequal(
            names(self@source_frequencies),
            required_frequency_roles
        )) {
            return(
                "`source_frequencies` must match required climate input roles."
            )
        }
        if (!setequal(
            names(self@optional_source_frequencies),
            optional_frequency_roles
        )) {
            return(
                "`optional_source_frequencies` must match optional climate input roles."
            )
        }
        known_roles <- union(
            required_frequency_roles,
            optional_frequency_roles
        )
        if (length(setdiff(names(self@optional_variables), known_roles)) ||
            !setequal(
                names(self@optional_variable_frequencies),
                names(self@optional_variables)
            )) {
            return(
                "Optional variable metadata must use declared climate input roles."
            )
        }
        for (role in names(self@optional_variables)) {
            variables <- self@optional_variables[[role]]
            if (!is.character(variables) || !length(variables) ||
                anyNA(variables) || any(!nzchar(variables)) ||
                anyDuplicated(variables)) {
                return(
                    "Optional source variables must be unique, non-empty variable IDs."
                )
            }
            frequencies <- self@optional_variable_frequencies[[role]]
            if (is.null(names(frequencies)) ||
                !setequal(names(frequencies), variables)) {
                return(
                    "Every optional source variable must declare its frequency."
                )
            }
        }
        for (property in c(
            "source_frequencies",
            "optional_source_frequencies",
            "optional_variable_frequencies"
        )) {
            contract <- S7::prop(self, property)
            for (role in names(contract)) {
                frequencies <- unlist(contract[[role]], use.names = FALSE)
                if (!is.character(frequencies) || !length(frequencies) ||
                    anyNA(frequencies) || any(!nzchar(frequencies))) {
                    return(sprintf(
                        "`%s` must contain non-empty source frequencies.",
                        property
                    ))
                }
            }
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
    reconstruction_labels <- c(
        original_morphing_field_equations = "Original morphing field equations",
        enhanced_field_equations = "Enhanced monthly field equations",
        btws = "BTWS",
        power = "POWER",
        ek_hourly_transform = "Ek hourly temperature transform",
        daily_additive_application = "Daily additive temperature application",
        sobie_curry_field_transforms = "Sobie-Curry field transforms",
        daily_temperature_projection = "Daily temperature projection",
        direct_model_hourly_mapping = "Direct-model hourly mapping"
    )
    records <- list(
        list(
            scale = "monthly",
            method = "original_morphing",
            recipe = "original_morphing_monthly",
            method_definition = "original_morphing_monthly",
            reconstructions = "original_morphing_field_equations",
            default_reconstruction = "original_morphing_field_equations",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "monthly",
            method = "epwshiftr",
            recipe = "epwshiftr_monthly",
            method_definition = "epwshiftr_monthly",
            reconstructions = "enhanced_field_equations",
            default_reconstruction = "enhanced_field_equations",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "monthly",
            method = "btws",
            recipe = "btws_monthly_temperature",
            method_definition = "btws_monthly_temperature",
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
            method_definition = "daily_temperature_delta",
            reconstructions = c("power", "btws"),
            default_reconstruction = "power",
            statistical_grouping = "circular_daily_climatology"
        ),
        list(
            scale = "daily",
            method = "ek",
            recipe = "ek_daily_factors",
            method_definition = "ek_daily_factors",
            reconstructions = "ek_hourly_transform",
            default_reconstruction = "ek_hourly_transform",
            statistical_grouping = "calendar_neutral_day"
        ),
        list(
            scale = "daily",
            method = "qm_morphing",
            recipe = "quantile_mapping_morphing_daily",
            method_definition = "quantile_mapping_morphing_daily",
            reconstructions = "daily_additive_application",
            default_reconstruction = "daily_additive_application",
            statistical_grouping = "calendar_month_distribution"
        ),
        list(
            scale = "daily",
            method = "sobie_curry",
            recipe = "sobie_curry_daily",
            method_definition = "sobie_curry_daily",
            reconstructions = "sobie_curry_field_transforms",
            default_reconstruction = "sobie_curry_field_transforms",
            statistical_grouping = "circular_daily_window"
        ),
        list(
            scale = "daily",
            method = "linear_scaling",
            recipe = "linear_scaling_daily_temperature",
            method_definition = "linear_scaling_daily",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "delta_change",
            recipe = "delta_change_daily_temperature",
            method_definition = "delta_change_daily",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "qm",
            recipe = "quantile_mapping_daily_temperature",
            method_definition = "quantile_mapping_daily",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "circular_daily_window"
        ),
        list(
            scale = "daily",
            method = "qdm",
            recipe = "quantile_delta_mapping_daily_temperature",
            method_definition = "quantile_delta_mapping_daily",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "circular_daily_window"
        ),
        list(
            scale = "daily",
            method = "sdm",
            recipe = "scaled_distribution_mapping_daily_temperature",
            method_definition = "scaled_distribution_mapping_daily",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "cdf_t",
            recipe = "cdf_transform_daily_temperature",
            method_definition = "cdf_transform_daily",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "edcdfm",
            recipe = "equidistant_cdf_matching_daily_temperature",
            method_definition = "equidistant_cdf_matching_daily",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "calendar_month"
        ),
        list(
            scale = "daily",
            method = "isimip3basd",
            recipe = "isimip3basd_daily_temperature",
            method_definition = "isimip3basd_daily",
            reconstructions = "daily_temperature_projection",
            default_reconstruction = "daily_temperature_projection",
            statistical_grouping = "circular_daily_window"
        ),
        list(
            scale = "hourly",
            method = "kernel_qdm",
            recipe = "hourly_kernel_qdm",
            method_definition = "kernel_quantile_delta_mapping_hourly",
            reconstructions = "direct_model_hourly_mapping",
            default_reconstruction = "direct_model_hourly_mapping",
            statistical_grouping = "centered_three_month_window"
        )
    )
    # Attach user-facing labels in the same registry that owns the selectable
    # reconstruction keys, so printing never exposes implementation-style IDs.
    records <- lapply(records, function(record) {
        keys <- record$reconstructions
        labels <- unname(reconstruction_labels[keys])
        record$reconstruction_labels <- stats::setNames(labels, keys)
        record
    })
    if (!isTRUE(WEATHER_TRANSFORM_REGISTRY_STATE$validated)) {
        transform__validate_records(records)
        WEATHER_TRANSFORM_REGISTRY_STATE$validated <- TRUE
    }
    records
}

# Validate the complete public-to-internal mapping as one graph before any
# constructor or discovery call uses it. This catches drift between method,
# recipe, backend, component, and input-role registries at package runtime.
transform__validate_records <- function(records) {
    checkmate::assert_list(records, min.len = 1L)
    required_fields <- c(
        "scale", "method", "recipe", "method_definition", "reconstructions",
        "reconstruction_labels", "default_reconstruction",
        "statistical_grouping"
    )
    method_keys <- character()
    tuples <- character()
    recipe_names <- character()
    method_definitions <- character()

    for (index in seq_along(records)) {
        record <- records[[index]]
        checkmate::assert_list(record, names = "unique")
        missing <- setdiff(required_fields, names(record))
        if (length(missing)) {
            cli::cli_abort(
                "Weather transform record {index} is missing field(s): {.val {missing}}."
            )
        }
        checkmate::assert_choice(record$scale, WEATHER_TRANSFORM_SCALES)
        checkmate::assert_string(
            record$method,
            pattern = "^[a-z][a-z0-9_]*$"
        )
        checkmate::assert_string(
            record$method_definition,
            pattern = "^[a-z][a-z0-9_]*$"
        )
        checkmate::assert_character(
            record$reconstructions,
            min.len = 1L,
            any.missing = FALSE,
            unique = TRUE
        )
        checkmate::assert_choice(
            record$default_reconstruction,
            record$reconstructions
        )
        checkmate::assert_character(
            record$reconstruction_labels,
            min.len = 1L,
            any.missing = FALSE,
            unique = TRUE,
            names = "unique"
        )
        if (!identical(
            names(record$reconstruction_labels),
            record$reconstructions
        )) {
            cli::cli_abort(
                "Weather transformation {.val {record$method}} must label every reconstruction."
            )
        }
        checkmate::assert_string(
            record$statistical_grouping,
            pattern = "^[a-z][a-z0-9_]*$"
        )

        # One method record owns all of its reconstruction choices. Splitting
        # them across records would make transform__record() resolve only the
        # first record even when the individual selection tuples are unique.
        method_keys <- c(
            method_keys,
            paste(record$scale, record$method, sep = "/")
        )

        recipes <- as.character(record$recipe)
        if (length(recipes) != length(record$reconstructions)) {
            cli::cli_abort(
                "Weather transformation {.val {record$method}} must map each reconstruction to one recipe."
            )
        }
        if (length(recipes) > 1L &&
            !identical(names(record$recipe), record$reconstructions)) {
            cli::cli_abort(
                "Weather transformation {.val {record$method}} must name recipes by reconstruction."
            )
        }

        tuples <- c(
            tuples,
            paste(
                record$scale,
                record$method,
                record$reconstructions,
                sep = "/"
            )
        )
        recipe_names <- c(recipe_names, unname(recipes))
        method_definitions <- c(
            method_definitions,
            rep(record$method_definition, length(recipes))
        )
    }

    if (anyDuplicated(method_keys)) {
        cli::cli_abort(
            "Weather transform registry contains duplicate scale/method records."
        )
    }
    if (anyDuplicated(tuples)) {
        cli::cli_abort(
            "Weather transform registry contains duplicate public selection tuples."
        )
    }
    if (anyDuplicated(recipe_names) ||
        !setequal(recipe_names, WEATHER_RECIPE_DEFAULTS)) {
        cli::cli_abort(
            paste(
                "Every built-in canonical recipe must be reachable through",
                "exactly one public weather transformation."
            )
        )
    }

    for (recipe_index in seq_along(recipe_names)) {
        recipe_name <- recipe_names[[recipe_index]]
        recipe <- recipe__get(recipe_name)
        expected_method <- method_definitions[[recipe_index]]
        if (!identical(recipe@method, expected_method)) {
            cli::cli_abort(
                "Recipe method identity disagrees for {.val {recipe_name}}."
            )
        }
        method <- method__get(recipe@method)
        recipe_roles <- setdiff(
            c(names(recipe@required_inputs), names(recipe@optional_inputs)),
            "weather_template"
        )
        if (!setequal(method@input_roles, recipe_roles)) {
            cli::cli_abort(
                "Method and recipe input roles disagree for {.val {recipe_name}}."
            )
        }

        if (identical(method@implementation, "backend")) {
            if (!identical(method@implementation_key, recipe@backend)) {
                cli::cli_abort(
                    "Method and backend identity disagree for {.val {recipe_name}}."
                )
            }
            epw_morph_backend(recipe@backend)
            next
        }

        if (!identical(
            recipe@components$signal,
            method@implementation_key
        )) {
            cli::cli_abort(
                "Method and signal component identity disagree for {.val {recipe_name}}."
            )
        }
        components <- lapply(WEATHER_COMPONENT_STAGES, function(stage) {
            component__get(stage, recipe@components[[stage]])
        })
        for (component_index in seq_len(length(components) - 1L)) {
            component__assert_compatible(
                components[[component_index]],
                components[[component_index + 1L]]
            )
        }
    }
    invisible(TRUE)
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
    registry <- transform__records()
    records <- Filter(
        function(record) {
            identical(record$scale, scale) &&
                identical(record$method, method)
        },
        registry
    )
    if (!length(records)) {
        available <- unique(vapply(
            Filter(
                function(record) identical(record$scale, scale),
                registry
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
            "i" = "Omit {.arg reconstruction}; it uses {record$reconstruction_labels[[choices]]}."
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
    record$reconstruction_label <- unname(
        record$reconstruction_labels[[reconstruction]]
    )
    record
}

# Resolve complete signal settings through the selected component's own
# validator so construction and execution cannot apply different contracts.
transform__validated_signal_settings <- function(
    method,
    variables,
    overrides = list()
) {
    variables <- weather__descriptor_values(variables, "variables")
    checkmate::assert_list(overrides, names = "unique")
    unknown <- setdiff(names(overrides), variables)
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown transformation variable option(s): {.val {unknown}}."
        )
    }
    component <- component__get("signal", method@implementation_key)
    validator <- component@operations$validate_options
    if (!is.function(validator)) {
        cli::cli_abort(
            "Signal method {.val {method@name}} does not expose its settings validator."
        )
    }

    resolved <- lapply(variables, function(variable) {
        defaults <- method@parameters[[variable]]
        if (is.null(defaults)) {
            cli::cli_abort(
                "Signal method {.val {method@name}} has no settings profile for {.val {variable}}."
            )
        }
        override <- shift_coalesce(overrides[[variable]], list())
        checkmate::assert_list(override, names = "unique")
        invalid <- setdiff(names(override), names(defaults))
        if (length(invalid)) {
            cli::cli_abort(
                "Unknown {.val {variable}} transformation option(s): {.val {invalid}}."
            )
        }
        complete <- utils::modifyList(defaults, override, keep.null = TRUE)
        validator(stats::setNames(list(complete), variable))
    })
    stats::setNames(resolved, variables)
}

# Split adapter controls from the selected signal method's scientific settings
# and return the complete validated recipe option envelope.
transform__signal_options <- function(recipe_spec, method, options) {
    if (startsWith(recipe_spec@backend, "daily_adjustment_")) {
        adapter_defaults <- daily_adjustment__options(NULL)
        adapter_names <- setdiff(names(adapter_defaults), "signal_overrides")
        setting_names <- names(method@parameters$tas)
        nested <- "tas" %in% names(options)
        if (nested && any(setting_names %in% names(options))) {
            cli::cli_abort(
                "Supply daily signal settings either directly or inside {.arg tas}, not both."
            )
        }
        allowed <- c(adapter_names, setting_names, "tas")
        unknown <- setdiff(names(options), allowed)
        if (length(unknown)) {
            cli::cli_abort(
                "Unknown transformation option(s): {.val {unknown}}."
            )
        }
        overrides <- if (nested) {
            list(tas = options$tas)
        } else {
            list(tas = options[intersect(names(options), setting_names)])
        }
        signal_settings <- transform__validated_signal_settings(
            method,
            "tas",
            overrides
        )
        adapter <- options[intersect(names(options), adapter_names)]
        adapter$signal_overrides <- signal_settings
        return(daily_adjustment__options(adapter))
    }

    variables <- method@output_variables
    unknown <- setdiff(names(options), variables)
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown transformation variable option(s): {.val {unknown}}."
        )
    }
    signal_settings <- transform__validated_signal_settings(
        method,
        variables,
        options
    )
    hourly_kqdm__options(list(signal_overrides = signal_settings))
}

# Convert public scientific options to the existing canonical recipe option
# contract, retaining the current numerical defaults and validators.
transform__recipe_options <- function(record, options) {
    checkmate::assert_list(options, names = "unique")
    recipe_spec <- recipe__get(record$recipe)
    method <- method__get(recipe_spec@method)
    if (startsWith(recipe_spec@backend, "daily_adjustment_") ||
        identical(recipe_spec@backend, "hourly_kernel_qdm")) {
        return(transform__signal_options(recipe_spec, method, options))
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

# Resolve option-dependent original-morphing variables after recipe construction.
# Other recipes already declare fixed role contracts in their canonical specs.
transform__input_contracts <- function(recipe_spec, recipe) {
    required_inputs <- recipe_spec@required_inputs
    optional_inputs <- recipe_spec@optional_inputs
    if (!recipe$backend %in% c("original_morphing", "original_morphing_absolute")) {
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
transform__source_frequencies <- function(requirements) {
    requirements <- requirements[
        setdiff(names(requirements), "weather_template")
    ]
    lapply(requirements, function(requirement) {
        if (length(requirement@variable_frequencies)) {
            required_variables <- unique(unlist(
                requirement@variable_sets,
                use.names = FALSE
            ))
            selected <- intersect(
                names(requirement@variable_frequencies),
                required_variables
            )
            if (length(selected)) {
                return(requirement@variable_frequencies[selected])
            }
        }
        requirement@frequencies
    })
}

# Surface model variables queried opportunistically by a recipe even though
# no method contract requires them for a successful transformation.
transform__optional_variables <- function(recipe, inputs) {
    all_variables <- morpher__input_variables(recipe)
    model_roles <- intersect(
        c("model_historical", "model_future"),
        c(names(inputs$required_inputs), names(inputs$optional_inputs))
    )
    values <- lapply(model_roles, function(role) {
        requirement <- shift_coalesce(
            inputs$required_inputs[[role]],
            inputs$optional_inputs[[role]]
        )
        required <- unique(unlist(
            requirement@variable_sets,
            use.names = FALSE
        ))
        setdiff(all_variables, required)
    })
    values <- stats::setNames(values, model_roles)
    Filter(length, values)
}

# Resolve the source frequency of every optional model variable from the
# role-specific mapping first and the recipe-wide frequency contract second.
transform__optional_variable_frequencies <- function(
    recipe,
    inputs,
    optional_variables
) {
    if (!length(optional_variables)) {
        return(list())
    }
    recipe_frequencies <- morpher__recipe_required_frequency(recipe)
    values <- lapply(names(optional_variables), function(role) {
        requirement <- shift_coalesce(
            inputs$required_inputs[[role]],
            inputs$optional_inputs[[role]]
        )
        variables <- optional_variables[[role]]
        values <- lapply(variables, function(variable) {
            mapped <- requirement@variable_frequencies[[variable]]
            if (!is.null(mapped)) {
                return(mapped)
            }
            if (!is.null(names(recipe_frequencies)) &&
                variable %in% names(recipe_frequencies)) {
                return(unname(recipe_frequencies[[variable]]))
            }
            as.character(recipe_frequencies)
        })
        stats::setNames(values, variables)
    })
    stats::setNames(values, names(optional_variables))
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
    optional_variables <- transform__optional_variables(recipe, inputs)
    stochastic_variables <- if (isTRUE(recipe_spec@stochastic)) {
        intersect(
            method_spec@stochastic_variables,
            epw_morph_variables(recipe)
        )
    } else {
        character()
    }
    source_references <- recipe_spec@source$references
    if (is.null(source_references) || !length(source_references)) {
        source_references <- method_spec@references
    }
    WeatherTransformSpec(
        scale = record$scale,
        method = record$method,
        label = recipe_spec@label,
        recipe = recipe_spec@name,
        recipe_version = recipe_spec@version,
        reconstruction = record$reconstruction,
        reconstruction_label = record$reconstruction_label,
        options = recipe$options,
        required_inputs = inputs$required_inputs,
        optional_inputs = inputs$optional_inputs,
        source_frequencies = transform__source_frequencies(
            inputs$required_inputs
        ),
        optional_source_frequencies = transform__source_frequencies(
            inputs$optional_inputs
        ),
        optional_variables = optional_variables,
        optional_variable_frequencies =
            transform__optional_variable_frequencies(
                recipe,
                inputs,
                optional_variables
            ),
        statistical_grouping = record$statistical_grouping,
        output_frequency = "hour",
        output_type = recipe_spec@output_type,
        stochastic = recipe_spec@stochastic,
        stochastic_variables = stochastic_variables,
        evidence = recipe_spec@source$type,
        references = source_references,
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
        record$reconstruction_label <- unname(
            record$reconstruction_labels[[selected_reconstruction]]
        )
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

# Convert one role requirement to the data-only form persisted with a transform.
transform__input_requirement_value <- function(requirement) {
    list(
        role = requirement@role,
        representations = requirement@representations,
        frequencies = requirement@frequencies,
        variable_frequencies = requirement@variable_frequencies,
        calendars = requirement@calendars,
        variable_sets = requirement@variable_sets
    )
}

# Convert role contracts into data-only records that remain readable in a
# persisted plan and comparable after a JSON round trip.
transform__input_contract_value <- function(contract) {
    lapply(contract, transform__input_requirement_value)
}

# Record the scientific catalog identity needed to detect registry drift when
# a stored workflow is resumed in a later package session.
transform__persistence_snapshot <- function(transform) {
    recipe <- recipe__get(
        transform@recipe,
        version = transform@recipe_version
    )
    method <- method__get(recipe@method)
    list(
        method_version = method@version,
        required_inputs = transform__input_contract_value(
            transform@required_inputs
        ),
        optional_inputs = transform__input_contract_value(
            transform@optional_inputs
        ),
        provenance = list(
            evidence = transform@evidence,
            references = transform@references,
            status = transform@status
        )
    )
}

# Normalize alternative variable sets after jsonlite has simplified one-row
# arrays to vectors or equal-width alternatives to matrices.
transform__variable_sets_from_value <- function(variable_sets) {
    if (is.null(variable_sets) || !length(variable_sets)) {
        return(list())
    }
    if (is.matrix(variable_sets)) {
        return(lapply(seq_len(nrow(variable_sets)), function(index) {
            as.character(variable_sets[index, , drop = TRUE])
        }))
    }
    if (is.character(variable_sets)) {
        return(list(as.character(variable_sets)))
    }
    lapply(variable_sets, function(variable_set) {
        as.character(unlist(variable_set, use.names = FALSE))
    })
}

# Normalize a persisted role contract without weakening any scientific field
# before comparing it with the current registry definition.
transform__input_contract_from_value <- function(contract) {
    if (is.null(contract) || !length(contract)) {
        return(list())
    }
    lapply(contract, function(requirement) {
        frequencies <- requirement$variable_frequencies
        if (is.atomic(frequencies) && length(frequencies)) {
            frequencies <- as.list(frequencies)
        }
        frequencies <- lapply(frequencies, function(value) {
            as.character(unlist(value, use.names = FALSE))
        })
        list(
            role = as.character(requirement$role),
            representations = as.character(unlist(
                requirement$representations,
                use.names = FALSE
            )),
            frequencies = as.character(unlist(
                requirement$frequencies,
                use.names = FALSE
            )),
            variable_frequencies = frequencies,
            calendars = as.character(unlist(
                requirement$calendars,
                use.names = FALSE
            )),
            variable_sets = transform__variable_sets_from_value(
                requirement$variable_sets
            )
        )
    })
}

# Canonicalize the persisted scientific snapshot after JSON simplification so
# semantic equality does not depend on scalar-versus-one-element-list shapes.
transform__snapshot_from_value <- function(snapshot) {
    provenance <- snapshot$provenance
    list(
        method_version = as.integer(snapshot$method_version),
        required_inputs = transform__input_contract_from_value(
            snapshot$required_inputs
        ),
        optional_inputs = transform__input_contract_from_value(
            snapshot$optional_inputs
        ),
        provenance = list(
            evidence = as.character(provenance$evidence),
            references = as.character(unlist(
                provenance$references,
                use.names = FALSE
            )),
            status = as.character(provenance$status)
        )
    )
}

# Encode a transform snapshot canonically so list simplification during JSON
# restoration cannot disguise a changed input or provenance contract.
transform__snapshot_json <- function(snapshot) {
    as.character(jsonlite::toJSON(
        transform__snapshot_from_value(snapshot),
        auto_unbox = TRUE,
        null = "null",
        na = "null"
    ))
}

# Encode non-finite numeric settings explicitly because workflow JSON maps
# infinities to null and would otherwise corrupt method bounds during resume.
transform__options_to_spec <- function(value) {
    if (is.list(value)) {
        return(lapply(value, transform__options_to_spec))
    }
    if (!is.numeric(value) || !any(!is.finite(value))) {
        return(value)
    }
    encoded <- as.list(value)
    encoded <- lapply(encoded, function(element) {
        if (is.infinite(element) && element > 0) {
            return("__epwshiftr_positive_infinity__")
        }
        if (is.infinite(element) && element < 0) {
            return("__epwshiftr_negative_infinity__")
        }
        element
    })
    unlist(encoded, use.names = FALSE)
}

# Decode transform-specific non-finite sentinels before the canonical recipe
# validates and normalizes restored scientific settings.
transform__options_from_spec <- function(value) {
    if (is.list(value)) {
        return(lapply(value, transform__options_from_spec))
    }
    if (!is.character(value)) {
        return(value)
    }
    positive <- value == "__epwshiftr_positive_infinity__"
    negative <- value == "__epwshiftr_negative_infinity__"
    if (!any(positive | negative)) {
        return(value)
    }
    decoded <- suppressWarnings(as.numeric(value))
    decoded[positive] <- Inf
    decoded[negative] <- -Inf
    decoded
}

# Serialize only reusable transform intent; execution data and internal recipe
# implementation details remain separate plan records.
transform__spec_value <- function(transform) {
    if (!S7::S7_inherits(transform, WeatherTransformSpec)) {
        cli::cli_abort(
            "{.arg transform} must be a {.cls WeatherTransformSpec}."
        )
    }
    c(list(
        scale = transform@scale,
        method = transform@method,
        recipe = transform@recipe,
        recipe_version = transform@recipe_version,
        reconstruction = transform@reconstruction,
        options = transform__options_to_spec(unclass(transform@options))
    ), transform__persistence_snapshot(transform))
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
        "options",
        "method_version",
        "required_inputs",
        "optional_inputs",
        "provenance"
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
    recorded_reconstruction <- as.character(spec$reconstruction)
    if (length(base_record$reconstructions) == 1L &&
        !identical(
            base_record$reconstruction,
            recorded_reconstruction
        )) {
        cli::cli_abort(
            "Persisted weather transform no longer matches its fixed hourly reconstruction."
        )
    }
    record <- if (length(base_record$reconstructions) > 1L) {
        transform__record(
            as.character(spec$scale),
            as.character(spec$method),
            recorded_reconstruction
        )
    } else {
        base_record
    }
    if (!identical(record$recipe, as.character(spec$recipe))) {
        cli::cli_abort(
            "Persisted weather transform no longer resolves to its recorded canonical recipe."
        )
    }
    recipe <- epw_morph_recipe(
        record$recipe,
        version = as.integer(spec$recipe_version),
        options = transform__options_from_spec(
            shift_coalesce(spec$options, list())
        )
    )
    transform <- transform__from_recipe(record, recipe)
    expected <- transform__persistence_snapshot(transform)
    actual <- spec[names(expected)]
    if (!identical(
        transform__snapshot_json(actual),
        transform__snapshot_json(expected)
    )) {
        cli::cli_abort(c(
            "Persisted weather transform no longer matches its recorded scientific contract.",
            "i" = "Create a new plan with the current weather transform registry."
        ))
    }
    transform
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
        if (S7::S7_inherits(value, ShiftReferenceSpec) &&
            !identical(value@role, role)) {
            cli::cli_abort(
                "{.arg {name}} declares role {.val {value@role}} but {.val {role}} is required."
            )
        }
        # A ShiftClimate stage has no intrinsic observational/model tag. Its
        # semantic role is declared by the argument receiving it and is written
        # explicitly when the execution plan is persisted.
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
                reconstruction = if (length(record$reconstructions) > 1L) {
                    transform@reconstruction
                } else {
                    NA_character_
                },
                reconstruction_label = transform@reconstruction_label,
                default_reconstruction = if (
                    length(record$reconstructions) > 1L
                ) {
                    identical(
                        transform@reconstruction,
                        record$default_reconstruction
                    )
                } else {
                    NA
                },
                required_inputs = list(transform@required_inputs),
                optional_inputs = list(transform@optional_inputs),
                source_frequencies = list(transform@source_frequencies),
                optional_source_frequencies = list(
                    transform@optional_source_frequencies
                ),
                optional_variables = list(transform@optional_variables),
                optional_variable_frequencies = list(
                    transform@optional_variable_frequencies
                ),
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
        "Optional source frequency" = transform__format_source_frequencies(
            x@optional_source_frequencies
        ),
        "Statistical grouping" = x@statistical_grouping,
        "Hourly reconstruction" = x@reconstruction_label,
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
        "Optional source variables" = if (length(x@optional_variables)) {
            paste(vapply(names(x@optional_variables), function(role) {
                sprintf(
                    "%s: %s",
                    role,
                    paste(x@optional_variables[[role]], collapse = " + ")
                )
            }, character(1L)), collapse = "; ")
        } else {
            "none"
        },
        "Optional variable frequency" =
            transform__format_source_frequencies(
                x@optional_variable_frequencies
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
