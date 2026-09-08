#' @include weather-method.R
NULL

# Stable protocol identifiers define the four comparison strata requested by
# the package architecture. Each protocol owns conditions shared across methods
# and never owns a statistical kernel.
WEATHER_PROTOCOL_DEFAULTS <- c(
    "monthly_morphing_comparison",
    "daily_temperature_comparison",
    "daily_bias_adjustment_comparison",
    "hourly_direct_model_comparison"
)

# Comparison protocols use one common set of model identity fields so a result
# cannot silently combine different sources, members, or grids.
WEATHER_PROTOCOL_SOURCE_FIELDS <- c(
    "source_id",
    "variant_label",
    "grid_label"
)

# Period roles are explicit because observed, historical-model, and
# future-model windows have different statistical purposes.
WEATHER_PROTOCOL_PERIOD_ROLES <- c(
    "observed_reference",
    "model_historical",
    "model_future"
)

# Protocol output types mirror the established recipe result contracts without
# depending on the recipe registry during package collation.
WEATHER_PROTOCOL_OUTPUT_TYPES <- c(
    "representative_year",
    "future_year",
    "multi_year"
)

# Protocol definitions are registered independently of recipes and study
# presets so one comparison boundary can be reused by many methods.
WEATHER_PROTOCOL_REGISTRY <- new.env(parent = emptyenv())

# WeatherProtocolSpec describes only conditions shared across a comparison:
# model identity, periods, template, source-calendar interpretation, target
# calendar, reconstruction, physics, output, stochastic execution, diagnostics,
# and evaluation metrics. Method-owned aggregation may still use months,
# annual phase, or another published statistic inside this boundary.
WeatherProtocolSpec <- S7::new_class(
    "WeatherProtocolSpec",
    properties = list(
        name = S7::new_property(S7::class_character),
        version = S7::new_property(S7::class_integer),
        label = S7::new_property(S7::class_character),
        domain = S7::new_property(S7::class_character),
        data_source = S7::new_property(S7::class_list),
        periods = S7::new_property(S7::class_list),
        weather_template = S7::new_property(S7::class_list),
        frequencies = S7::new_property(S7::class_character),
        comparison_variables = S7::new_property(S7::class_character),
        input_calendar_semantics = S7::new_property(S7::class_character),
        target_calendar = S7::new_property(S7::class_character),
        hourly_reconstruction = S7::new_property(
            S7::class_character,
            default = character()
        ),
        physical_policy = S7::new_property(S7::class_character),
        output_type = S7::new_property(S7::class_character),
        random_seed = S7::new_property(S7::class_integer),
        replicates = S7::new_property(S7::class_integer),
        diagnostics = S7::new_property(S7::class_character),
        metrics = S7::new_property(S7::class_character)
    ),
    validator = function(self) {
        if (length(self@name) != 1L ||
            is.na(self@name) ||
            !grepl("^[a-z][a-z0-9_]*$", self@name)) {
            return("`name` must be one lower snake_case protocol identifier.")
        }
        if (length(self@version) != 1L ||
            is.na(self@version) ||
            self@version < 1L) {
            return("`version` must be one positive integer.")
        }
        for (property in c(
            "label", "input_calendar_semantics", "target_calendar",
            "physical_policy", "output_type"
        )) {
            value <- S7::prop(self, property)
            if (length(value) != 1L || is.na(value) || !nzchar(value)) {
                return(sprintf("`%s` must be one non-empty string.", property))
            }
        }
        if (length(self@hourly_reconstruction) > 1L ||
            anyNA(self@hourly_reconstruction) ||
            any(!nzchar(self@hourly_reconstruction))) {
            return(
                "`hourly_reconstruction` must be empty or one non-empty string."
            )
        }
        if (length(self@domain) != 1L ||
            is.na(self@domain) ||
            !self@domain %in% WEATHER_METHOD_DOMAINS) {
            return("`domain` must identify one weather-method comparison stratum.")
        }
        for (property in c("data_source", "periods", "weather_template")) {
            value <- S7::prop(self, property)
            if (!length(value) || is.null(names(value)) ||
                any(!nzchar(names(value))) || anyDuplicated(names(value))) {
                return(sprintf("`%s` must be a non-empty named list.", property))
            }
        }
        source_fields <- self@data_source$identity_fields
        if (!is.character(source_fields) ||
            !identical(source_fields, WEATHER_PROTOCOL_SOURCE_FIELDS) ||
            !identical(self@data_source$comparison, "identical")) {
            return(
                "`data_source` must require identical source, member, and grid fields."
            )
        }
        shared_roles <- self@periods$shared_roles
        optional_shared_roles <- self@periods$optional_shared_roles
        if (!is.character(shared_roles) || !length(shared_roles) ||
            anyNA(shared_roles) || any(!nzchar(shared_roles)) ||
            anyDuplicated(shared_roles) ||
            !all(shared_roles %in% WEATHER_PROTOCOL_PERIOD_ROLES) ||
            !all(c("model_historical", "model_future") %in% shared_roles) ||
            !identical(self@periods$comparison, "identical")) {
            return(
                "`periods` must identify unique shared roles including historical and future model periods."
            )
        }
        if (!is.character(optional_shared_roles) ||
            anyNA(optional_shared_roles) ||
            any(!nzchar(optional_shared_roles)) ||
            anyDuplicated(optional_shared_roles) ||
            !all(optional_shared_roles %in% WEATHER_PROTOCOL_PERIOD_ROLES) ||
            length(intersect(shared_roles, optional_shared_roles))) {
            return(
                "`periods` optional shared roles must be unique, recognized, and distinct from required shared roles."
            )
        }
        if (!identical(self@weather_template$comparison, "identical")) {
            return("`weather_template` must require one identical EPW template.")
        }
        for (property in c(
            "frequencies", "comparison_variables", "diagnostics", "metrics"
        )) {
            value <- S7::prop(self, property)
            if (!length(value) || anyNA(value) ||
                any(!nzchar(value)) || anyDuplicated(value)) {
                return(sprintf(
                    "`%s` must contain unique, non-empty values.",
                    property
                ))
            }
        }
        if (!self@output_type %in% WEATHER_PROTOCOL_OUTPUT_TYPES) {
            return("`output_type` contains an unknown weather output type.")
        }
        if (length(self@random_seed) != 1L ||
            is.na(self@random_seed) ||
            self@random_seed < 0L) {
            return("`random_seed` must be one non-negative integer.")
        }
        if (length(self@replicates) != 1L ||
            is.na(self@replicates) ||
            self@replicates < 1L) {
            return("`replicates` must be one positive integer.")
        }
        NULL
    }
)

# Construct one normalized comparison protocol while fixing equality rules for
# model identity, period roles, and the EPW template across all methods.
protocol__spec <- function(
    name,
    label,
    domain,
    frequencies,
    comparison_variables,
    shared_roles = c("model_historical", "model_future"),
    optional_shared_roles = character(),
    input_calendar_semantics,
    target_calendar,
    hourly_reconstruction = character(),
    physical_policy,
    output_type,
    random_seed = 1L,
    replicates = 1L,
    diagnostics,
    metrics,
    version = 1L
) {
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_count(version, positive = TRUE)
    checkmate::assert_string(label, min.chars = 1L)
    checkmate::assert_choice(domain, WEATHER_METHOD_DOMAINS)
    frequencies <- weather__descriptor_values(frequencies, "frequencies")
    comparison_variables <- weather__descriptor_values(
        comparison_variables,
        "comparison_variables"
    )
    shared_roles <- weather__descriptor_values(
        shared_roles,
        "shared_roles"
    )
    checkmate::assert_subset(
        shared_roles,
        WEATHER_PROTOCOL_PERIOD_ROLES
    )
    if (!all(c("model_historical", "model_future") %in% shared_roles)) {
        cli::cli_abort(
            "{.arg shared_roles} must include historical and future model periods."
        )
    }
    optional_shared_roles <- weather__descriptor_values(
        optional_shared_roles,
        "optional_shared_roles"
    )
    checkmate::assert_subset(
        optional_shared_roles,
        setdiff(WEATHER_PROTOCOL_PERIOD_ROLES, shared_roles)
    )
    checkmate::assert_string(
        input_calendar_semantics,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    checkmate::assert_string(
        target_calendar,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    checkmate::assert_character(
        hourly_reconstruction,
        max.len = 1L,
        any.missing = FALSE
    )
    if (length(hourly_reconstruction)) {
        checkmate::assert_string(
            hourly_reconstruction,
            pattern = "^[a-z][a-z0-9_]*$"
        )
    }
    checkmate::assert_string(
        physical_policy,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    checkmate::assert_choice(output_type, WEATHER_PROTOCOL_OUTPUT_TYPES)
    checkmate::assert_int(
        random_seed,
        lower = 0L,
        upper = .Machine$integer.max - 1L
    )
    checkmate::assert_count(replicates, positive = TRUE)
    diagnostics <- weather__descriptor_values(diagnostics, "diagnostics")
    metrics <- weather__descriptor_values(metrics, "metrics")

    WeatherProtocolSpec(
        name = name,
        version = as.integer(version),
        label = label,
        domain = domain,
        data_source = list(
            comparison = "identical",
            identity_fields = WEATHER_PROTOCOL_SOURCE_FIELDS
        ),
        periods = list(
            comparison = "identical",
            shared_roles = shared_roles,
            optional_shared_roles = optional_shared_roles
        ),
        weather_template = list(comparison = "identical"),
        frequencies = frequencies,
        comparison_variables = comparison_variables,
        input_calendar_semantics = input_calendar_semantics,
        target_calendar = target_calendar,
        hourly_reconstruction = hourly_reconstruction,
        physical_policy = physical_policy,
        output_type = output_type,
        random_seed = as.integer(random_seed),
        replicates = as.integer(replicates),
        diagnostics = diagnostics,
        metrics = metrics
    )
}

# Define common diagnostics and metrics once so every comparison stratum uses
# the same evaluation vocabulary where its output variables overlap.
protocol__common_evaluation <- function() {
    list(
        diagnostics = c(
            "input_identity",
            "period_coverage",
            "calendar_mapping",
            "physical_closure",
            "stochastic_realization"
        ),
        metrics = c(
            "mean_bias",
            "root_mean_square_error",
            "quantile_error",
            "daily_extrema_error",
            "spell_length_error",
            "physical_closure_error"
        )
    )
}

# Build the four protocol strata without embedding concrete publication data
# selections or method-owned numerical settings.
protocol__default_specs <- function() {
    evaluation <- protocol__common_evaluation()
    list(
        monthly_morphing_comparison = protocol__spec(
            name = "monthly_morphing_comparison",
            label = "Monthly morphing comparison",
            domain = "monthly_morphing",
            frequencies = "mon",
            comparison_variables = c(
                "tas", "hurs", "psl", "pr", "sfcWind", "rsds", "rlds"
            ),
            input_calendar_semantics = "native_calendar_semantics",
            target_calendar = "epw_365_day",
            physical_policy = "monthly_harmonized",
            output_type = "representative_year",
            diagnostics = evaluation$diagnostics,
            metrics = evaluation$metrics
        ),
        daily_temperature_comparison = protocol__spec(
            name = "daily_temperature_comparison",
            label = "Daily temperature comparison",
            domain = "daily_temperature",
            frequencies = "day",
            comparison_variables = "tas",
            optional_shared_roles = "observed_reference",
            input_calendar_semantics = "native_cf_calendar",
            target_calendar = "epw_365_day",
            hourly_reconstruction = "constrained_daily_temperature",
            physical_policy = "preserve_specific_humidity",
            output_type = "representative_year",
            diagnostics = evaluation$diagnostics,
            metrics = evaluation$metrics
        ),
        daily_bias_adjustment_comparison = protocol__spec(
            name = "daily_bias_adjustment_comparison",
            label = "Daily bias-adjustment comparison",
            domain = "daily_bias_adjustment",
            frequencies = "day",
            comparison_variables = "tas",
            shared_roles = WEATHER_PROTOCOL_PERIOD_ROLES,
            input_calendar_semantics = "native_cf_calendar",
            target_calendar = "epw_365_day",
            hourly_reconstruction = "constrained_daily_temperature",
            physical_policy = "preserve_specific_humidity",
            output_type = "representative_year",
            diagnostics = evaluation$diagnostics,
            metrics = evaluation$metrics
        ),
        hourly_direct_model_comparison = protocol__spec(
            name = "hourly_direct_model_comparison",
            label = "Hourly direct-model comparison",
            domain = "hourly_direct_model",
            frequencies = "hour",
            comparison_variables = c(
                "tas", "ps", "hurs", "sfcWind", "rsds", "rsdsdiff"
            ),
            shared_roles = WEATHER_PROTOCOL_PERIOD_ROLES,
            input_calendar_semantics = "native_cf_calendar",
            target_calendar = "epw_365_day",
            hourly_reconstruction = "direct_model_epw_calendar_mapping",
            physical_policy = "absolute_model_fields",
            output_type = "multi_year",
            diagnostics = evaluation$diagnostics,
            metrics = evaluation$metrics
        )
    )
}

# Register one protocol without allowing silent replacement of a stable
# comparison identifier.
protocol__register <- function(
    spec,
    overwrite = FALSE,
    registry = WEATHER_PROTOCOL_REGISTRY
) {
    if (!S7::S7_inherits(spec, WeatherProtocolSpec)) {
        cli::cli_abort("{.arg spec} must be a WeatherProtocolSpec object.")
    }
    checkmate::assert_flag(overwrite)
    checkmate::assert_environment(registry)
    if (exists(spec@name, envir = registry, inherits = FALSE) &&
        !isTRUE(overwrite)) {
        cli::cli_abort(
            "Future-weather protocol {.val {spec@name}} is already registered."
        )
    }
    assign(spec@name, spec, envir = registry)
    invisible(spec)
}

# Populate all built-in comparison protocols exactly once.
protocol__register_defaults <- function() {
    registered <- ls(envir = WEATHER_PROTOCOL_REGISTRY, all.names = FALSE)
    if (all(WEATHER_PROTOCOL_DEFAULTS %in% registered)) {
        return(invisible(NULL))
    }
    for (spec in protocol__default_specs()) {
        if (!exists(
            spec@name,
            envir = WEATHER_PROTOCOL_REGISTRY,
            inherits = FALSE
        )) {
            protocol__register(spec)
        }
    }
    invisible(NULL)
}

# Retrieve one comparison protocol and optionally enforce its definition
# version for persisted benchmark plans.
protocol__get <- function(
    name,
    version = NULL,
    registry = WEATHER_PROTOCOL_REGISTRY
) {
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_environment(registry)
    if (identical(registry, WEATHER_PROTOCOL_REGISTRY)) {
        protocol__register_defaults()
    }
    name <- tolower(name)
    if (!exists(name, envir = registry, inherits = FALSE)) {
        cli::cli_abort("Unknown future-weather protocol: {.val {name}}.")
    }
    spec <- get(name, envir = registry, inherits = FALSE)
    if (!is.null(version)) {
        checkmate::assert_count(version, positive = TRUE)
        if (!identical(spec@version, as.integer(version))) {
            cli::cli_abort(
                "Future-weather protocol {.val {name}} requires definition version {spec@version}; persisted version is {as.integer(version)}."
            )
        }
    }
    spec
}

# Return inspectable protocol records without executable functions or concrete
# study data selections.
protocol__list <- function(registry = WEATHER_PROTOCOL_REGISTRY) {
    checkmate::assert_environment(registry)
    if (identical(registry, WEATHER_PROTOCOL_REGISTRY)) {
        protocol__register_defaults()
    }
    names <- sort(ls(envir = registry, all.names = FALSE))
    data.table::rbindlist(lapply(names, function(name) {
        spec <- get(name, envir = registry, inherits = FALSE)
        data.table::data.table(
            name = spec@name,
            version = spec@version,
            label = spec@label,
            domain = spec@domain,
            data_source = list(spec@data_source),
            periods = list(spec@periods),
            weather_template = list(spec@weather_template),
            frequencies = list(spec@frequencies),
            comparison_variables = list(spec@comparison_variables),
            input_calendar_semantics = spec@input_calendar_semantics,
            target_calendar = spec@target_calendar,
            hourly_reconstruction = if (length(
                spec@hourly_reconstruction
            )) {
                spec@hourly_reconstruction
            } else {
                NA_character_
            },
            physical_policy = spec@physical_policy,
            output_type = spec@output_type,
            random_seed = spec@random_seed,
            replicates = spec@replicates,
            diagnostics = list(spec@diagnostics),
            metrics = list(spec@metrics)
        )
    }), use.names = TRUE, fill = TRUE)
}

# Explain whether one method can participate in one protocol using domain,
# frequency, and comparable-output-variable contracts.
protocol__method_compatibility <- function(method, protocol) {
    if (!S7::S7_inherits(method, WeatherMethodSpec)) {
        cli::cli_abort("{.arg method} must be a WeatherMethodSpec object.")
    }
    if (!S7::S7_inherits(protocol, WeatherProtocolSpec)) {
        cli::cli_abort("{.arg protocol} must be a WeatherProtocolSpec object.")
    }
    reasons <- character()
    if (!identical(method@domain, protocol@domain)) {
        reasons <- c(reasons, "comparison domain differs")
    }
    if (!length(intersect(method@frequencies, protocol@frequencies))) {
        reasons <- c(reasons, "temporal frequency differs")
    }
    unsupported <- setdiff(
        protocol@comparison_variables,
        method@output_variables
    )
    if (length(unsupported)) {
        reasons <- c(
            reasons,
            sprintf(
                "does not produce comparison variable(s): %s",
                paste(unsupported, collapse = ", ")
            )
        )
    }
    list(
        compatible = !length(reasons),
        reason = if (length(reasons)) {
            paste(reasons, collapse = "; ")
        } else {
            "compatible"
        },
        common_variables = intersect(
            method@output_variables,
            protocol@comparison_variables
        )
    )
}

# Build the complete method-by-protocol matrix so incompatible comparisons are
# represented explicitly instead of disappearing from listings.
protocol__compatibility_matrix <- function(
    methods = NULL,
    protocols = NULL
) {
    method__register_defaults()
    protocol__register_defaults()
    if (is.null(methods)) {
        methods <- sort(ls(
            envir = WEATHER_METHOD_REGISTRY,
            all.names = FALSE
        ))
    }
    if (is.null(protocols)) {
        protocols <- WEATHER_PROTOCOL_DEFAULTS
    }
    checkmate::assert_character(
        methods,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    checkmate::assert_character(
        protocols,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    rows <- list()
    for (method_name in methods) {
        method <- method__get(method_name)
        for (protocol_name in protocols) {
            protocol <- protocol__get(protocol_name)
            compatibility <- protocol__method_compatibility(
                method,
                protocol
            )
            rows[[length(rows) + 1L]] <- data.table::data.table(
                method = method@name,
                method_label = method@label,
                method_domain = method@domain,
                protocol = protocol@name,
                protocol_label = protocol@label,
                protocol_domain = protocol@domain,
                compatible = compatibility$compatible,
                reason = compatibility$reason,
                input_roles = list(method@input_roles),
                required_variable_sets = list(method@variable_sets),
                common_variables = list(compatibility$common_variables),
                stochastic = method@stochastic,
                random_seed = protocol@random_seed,
                replicates = protocol@replicates
            )
        }
    }
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Produce deterministic period, calendar, frequency, and per-variable content
# signatures for one role table. Different methods may require different
# variables, so content equality is checked only where two methods share a
# variable rather than forcing every method to consume the same columns.
protocol__role_signature <- function(input, role) {
    if (!S7::S7_inherits(input, WeatherInput) ||
        !is.data.frame(input@source)) {
        cli::cli_abort(
            "Comparison role {.val {role}} must contain a materialized series WeatherInput."
        )
    }
    data <- data.table::as.data.table(data.table::copy(input@source))
    if (!nrow(data) || !"variable_id" %in% names(data)) {
        cli::cli_abort(
            "Comparison role {.val {role}} must contain variable-labelled rows."
        )
    }
    year_column <- intersect(c("cf_year", "year"), names(data))
    if (!length(year_column)) {
        cli::cli_abort(
            "Comparison role {.val {role}} lacks a calendar year column."
        )
    }
    order_columns <- intersect(c(
        "source_id", "variant_label", "grid_label", "variable_id",
        "cf_year", "cf_day_of_year", "cf_second_of_day", "time", "year"
    ), names(data))
    if (length(order_columns)) {
        data.table::setorderv(data, order_columns)
    }
    stable_columns <- intersect(c(
        "source_id", "variant_label", "grid_label", "experiment_id",
        "variable_id", "units", "frequency", "cf_calendar", "cf_year",
        "cf_month", "cf_day", "cf_day_of_year", "cf_year_days",
        "cf_second_of_day", "annual_phase", "value", "year"
    ), names(data))
    variables <- sort(unique(as.character(data[["variable_id"]])))
    variable_hashes <- stats::setNames(lapply(variables, function(variable) {
        rows <- data[data[["variable_id"]] == variable]
        store__hash(rows[, stable_columns, with = FALSE])
    }), variables)
    list(
        years = sort(unique(as.integer(data[[year_column[[1L]]]]))),
        calendars = sort(unique(input@calendars)),
        frequencies = sort(unique(input@frequencies)),
        variable_hashes = variable_hashes
    )
}

# Extract one model source/member/grid identity and reject mixed identities
# before comparisons are allowed to proceed.
protocol__model_identity <- function(input, role) {
    if (!S7::S7_inherits(input, WeatherInput) ||
        !is.data.frame(input@source)) {
        cli::cli_abort(
            "Comparison role {.val {role}} must contain a materialized series WeatherInput."
        )
    }
    missing <- setdiff(
        WEATHER_PROTOCOL_SOURCE_FIELDS,
        names(input@source)
    )
    if (length(missing)) {
        cli::cli_abort(
            "Comparison role {.val {role}} lacks model identity field(s): {.val {missing}}."
        )
    }
    identity <- unique(data.table::as.data.table(input@source)[,
        WEATHER_PROTOCOL_SOURCE_FIELDS,
        with = FALSE
    ])
    if (nrow(identity) != 1L || anyNA(identity)) {
        cli::cli_abort(
            "Comparison role {.val {role}} must contain one complete model, member, and grid identity."
        )
    }
    as.list(identity[1L])
}

# Build the complete shared-input signature owned by a comparison protocol,
# including model identity, role periods and values, and the EPW template.
protocol__input_signature <- function(protocol, inputs) {
    if (!S7::S7_inherits(protocol, WeatherProtocolSpec)) {
        cli::cli_abort("{.arg protocol} must be a WeatherProtocolSpec object.")
    }
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    shared_roles <- protocol@periods$shared_roles
    optional_roles <- protocol@periods$optional_shared_roles
    roles <- c(shared_roles, optional_roles)
    role_inputs <- lapply(
        roles,
        function(role) weather__get_input(inputs, role)
    )
    names(role_inputs) <- roles
    missing <- shared_roles[vapply(
        role_inputs[shared_roles],
        is.null,
        logical(1L)
    )]
    if (length(missing)) {
        cli::cli_abort(
            "Comparison inputs are missing role(s): {.val {missing}}."
        )
    }
    present <- !vapply(role_inputs, is.null, logical(1L))
    role_inputs <- role_inputs[present]
    role_signatures <- lapply(names(role_inputs), function(role) {
        protocol__role_signature(
            role_inputs[[role]],
            role
        )
    })
    names(role_signatures) <- names(role_inputs)
    required_signatures <- role_signatures[shared_roles]
    optional_signatures <- role_signatures[intersect(
        optional_roles,
        names(role_signatures)
    )]
    historical_identity <- protocol__model_identity(
        role_inputs$model_historical,
        "model_historical"
    )
    future_identity <- protocol__model_identity(
        role_inputs$model_future,
        "model_future"
    )
    if (!identical(historical_identity, future_identity)) {
        cli::cli_abort(
            "Historical and future model inputs must use the same source, member, and grid."
        )
    }
    template <- weather__get_input(inputs, "weather_template")
    if (!S7::S7_inherits(template, WeatherInput) ||
        !inherits(template@source, "EpwFile")) {
        cli::cli_abort(
            "Comparison inputs require one materialized EPW weather template."
        )
    }
    list(
        model_identity = historical_identity,
        periods = lapply(required_signatures, `[[`, "years"),
        calendars = lapply(required_signatures, `[[`, "calendars"),
        frequencies = lapply(required_signatures, `[[`, "frequencies"),
        variable_hashes = lapply(
            required_signatures,
            `[[`,
            "variable_hashes"
        ),
        optional_roles = optional_signatures,
        weather_template = store__hash(
            template@source$location(),
            template@source$data()
        ),
        input_calendar_semantics = protocol@input_calendar_semantics,
        target_calendar = protocol@target_calendar,
        hourly_reconstruction = protocol@hourly_reconstruction,
        physical_policy = protocol@physical_policy,
        output_type = protocol@output_type,
        random_seed = protocol@random_seed,
        replicates = protocol@replicates,
        diagnostics = protocol@diagnostics,
        metrics = protocol@metrics
    )
}

# Compare content hashes only for variables shared by multiple method inputs.
# Method-specific variables stay outside the protocol equality boundary.
protocol__validate_common_variables <- function(signatures, roles) {
    for (role in roles) {
        variables <- unique(unlist(lapply(signatures, function(signature) {
            hashes <- signature$variable_hashes[[role]]
            if (is.null(hashes)) character() else names(hashes)
        }), use.names = FALSE))
        for (variable in variables) {
            hashes <- unlist(lapply(signatures, function(signature) {
                signature$variable_hashes[[role]][[variable]]
            }), use.names = FALSE)
            if (length(hashes) > 1L && length(unique(hashes)) != 1L) {
                cli::cli_abort(c(
                    "Comparison methods do not share identical values for a common input variable.",
                    "x" = "Role {.val {role}}, variable {.val {variable}} differs between methods."
                ))
            }
        }
    }
    invisible(NULL)
}

# Compare an optional shared role only among methods that actually require it.
# Its absence from another method never creates an artificial input requirement.
protocol__validate_optional_roles <- function(protocol, signatures) {
    for (role in protocol@periods$optional_shared_roles) {
        present <- lapply(signatures, function(signature) {
            signature$optional_roles[[role]]
        })
        present <- present[!vapply(present, is.null, logical(1L))]
        if (length(present) < 2L) {
            next
        }
        boundaries <- lapply(present, function(signature) {
            signature$variable_hashes <- NULL
            signature
        })
        reference <- boundaries[[1L]]
        if (!all(vapply(boundaries, identical, logical(1L), reference))) {
            cli::cli_abort(
                "Comparison methods that use optional role {.val {role}} do not share its period, calendar, and frequency boundary."
            )
        }
        wrapped <- lapply(present, function(signature) {
            list(variable_hashes = stats::setNames(
                list(signature$variable_hashes),
                role
            ))
        })
        protocol__validate_common_variables(wrapped, role)
    }
    invisible(NULL)
}

# Validate that every method run uses exactly the same protocol-owned inputs
# and settings while leaving method-owned variables and parameters untouched.
protocol__validate_shared_inputs <- function(protocol, inputs) {
    if (is.character(protocol)) {
        checkmate::assert_string(protocol, min.chars = 1L)
        protocol <- protocol__get(protocol)
    }
    checkmate::assert_list(inputs, min.len = 1L, names = "unique")
    if (is.null(names(inputs)) || any(!nzchar(names(inputs)))) {
        cli::cli_abort("{.arg inputs} must be named by method.")
    }
    signatures <- lapply(inputs, protocol__input_signature,
        protocol = protocol
    )
    # Method-owned variable sets can differ. Compare the protocol-owned
    # boundary first, then compare values only for variables shared by two or
    # more method inputs.
    boundaries <- lapply(signatures, function(signature) {
        signature$variable_hashes <- NULL
        signature$optional_roles <- NULL
        signature
    })
    reference <- boundaries[[1L]]
    mismatched <- names(boundaries)[!vapply(
        boundaries,
        identical,
        logical(1L),
        reference
    )]
    if (length(mismatched)) {
        cli::cli_abort(c(
            "Comparison methods do not share one protocol input boundary.",
            "x" = "Mismatched method input(s): {.val {mismatched}}."
        ))
    }
    protocol__validate_common_variables(
        signatures,
        protocol@periods$shared_roles
    )
    protocol__validate_optional_roles(protocol, signatures)
    invisible(signatures)
}

#' Inspect registered future-weather comparison protocols
#'
#' @return A data table with one row per shared comparison protocol.
#'
#' @seealso [epw_morph_methods()], [epw_morph_compatibility()],
#'   [epw_morph_study_presets()]
#' @export
epw_morph_protocols <- function() {
    protocol__list()
}

#' Get a registered future-weather comparison protocol
#'
#' @param name Stable protocol name returned by [epw_morph_protocols()].
#'
#' @return A `WeatherProtocolSpec` object.
#'
#' @seealso [epw_morph_protocols()], [epw_morph_method_spec()]
#' @export
epw_morph_protocol_spec <- function(name) {
    protocol__get(name)
}

#' Compare method and protocol compatibility
#'
#' @param methods Optional method names returned by [epw_morph_methods()].
#' @param protocols Optional protocol names returned by
#'   [epw_morph_protocols()].
#'
#' @return A data table containing every requested method-protocol pair and an
#'   explicit compatibility result.
#'
#' @seealso [epw_morph_methods()], [epw_morph_protocols()]
#' @export
epw_morph_compatibility <- function(methods = NULL, protocols = NULL) {
    protocol__compatibility_matrix(methods, protocols)
}
