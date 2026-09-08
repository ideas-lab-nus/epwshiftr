#' @include weather-signal.R
NULL

# Method domains group algorithms by the temporal scale and weather-generation
# role of their climate signal.
WEATHER_METHOD_DOMAINS <- c(
    "monthly_morphing",
    "daily_temperature",
    "daily_bias_adjustment",
    "hourly_direct_model"
)

# Method implementations identify the executable registry boundary without
# retaining a complete workflow or any publication-specific experiment setup.
WEATHER_METHOD_IMPLEMENTATIONS <- c("backend", "signal_component")

# Stable method identifiers are independent of complete recipes and study
# presets. BTWS and POWER are hourly reconstruction choices, so the shared
# daily temperature signal appears only once in this catalog.
WEATHER_METHOD_DEFAULTS <- c(
    "belcher_monthly",
    "epwshiftr_monthly",
    "daily_temperature_delta",
    "eames_monthly_temperature",
    "ek_daily_factors",
    "monthly_percentile_temperature",
    "sobie_curry_daily",
    "kernel_quantile_delta_mapping_hourly",
    "linear_scaling_daily",
    "delta_change_daily",
    "quantile_mapping_daily",
    "quantile_delta_mapping_daily",
    "scaled_distribution_mapping_daily",
    "cdf_transform_daily",
    "equidistant_cdf_matching_daily",
    "isimip3basd_daily"
)

# Method definitions are process-local catalog records. Executable functions
# remain owned by backend and component registries.
WEATHER_METHOD_REGISTRY <- new.env(parent = emptyenv())

# WeatherMethodSpec describes only method-owned behavior: the algorithm,
# supported temporal lattice, semantic inputs, variables, output backbone,
# settings, stochastic status, evidence, and references.
WeatherMethodSpec <- S7::new_class(
    "WeatherMethodSpec",
    properties = list(
        name = S7::new_property(S7::class_character),
        version = S7::new_property(S7::class_integer),
        label = S7::new_property(S7::class_character),
        domain = S7::new_property(S7::class_character),
        implementation = S7::new_property(S7::class_character),
        implementation_key = S7::new_property(S7::class_character),
        frequencies = S7::new_property(S7::class_character),
        input_roles = S7::new_property(S7::class_character),
        variable_sets = S7::new_property(S7::class_list),
        output_variables = S7::new_property(S7::class_character),
        output_role = S7::new_property(S7::class_character),
        parameters = S7::new_property(S7::class_list, default = list()),
        stochastic = S7::new_property(S7::class_logical),
        stochastic_variables = S7::new_property(
            S7::class_character,
            default = character()
        ),
        evidence = S7::new_property(S7::class_character),
        references = S7::new_property(
            S7::class_character,
            default = character()
        )
    ),
    validator = function(self) {
        if (length(self@name) != 1L ||
            is.na(self@name) ||
            !grepl("^[a-z][a-z0-9_]*$", self@name)) {
            return("`name` must be one lower snake_case method identifier.")
        }
        if (length(self@version) != 1L ||
            is.na(self@version) ||
            self@version < 1L) {
            return("`version` must be one positive integer.")
        }
        for (property in c("label", "implementation_key", "evidence")) {
            value <- S7::prop(self, property)
            if (length(value) != 1L || is.na(value) || !nzchar(value)) {
                return(sprintf("`%s` must be one non-empty string.", property))
            }
        }
        if (length(self@domain) != 1L ||
            is.na(self@domain) ||
            !self@domain %in% WEATHER_METHOD_DOMAINS) {
            return("`domain` must identify one weather-method family.")
        }
        if (length(self@implementation) != 1L ||
            is.na(self@implementation) ||
            !self@implementation %in% WEATHER_METHOD_IMPLEMENTATIONS) {
            return("`implementation` must be `backend` or `signal_component`.")
        }
        for (property in c("frequencies", "input_roles", "references")) {
            value <- S7::prop(self, property)
            if (!length(value) || anyNA(value) ||
                any(!nzchar(value)) || anyDuplicated(value)) {
                return(sprintf(
                    "`%s` must contain unique, non-empty values.",
                    property
                ))
            }
        }
        if (!all(self@input_roles %in% WEATHER_INPUT_ROLES)) {
            return("`input_roles` contains an unknown semantic input role.")
        }
        if (!length(self@variable_sets)) {
            return("`variable_sets` must contain at least one supported set.")
        }
        for (variable_set in self@variable_sets) {
            if (!is.character(variable_set) || !length(variable_set) ||
                anyNA(variable_set) || any(!nzchar(variable_set)) ||
                anyDuplicated(variable_set)) {
                return(
                    "Every `variable_sets` entry must contain unique variable IDs."
                )
            }
        }
        if (!length(self@output_variables) ||
            anyNA(self@output_variables) ||
            any(!nzchar(self@output_variables)) ||
            anyDuplicated(self@output_variables)) {
            return("`output_variables` must contain unique variable IDs.")
        }
        if (length(self@output_role) != 1L ||
            is.na(self@output_role) ||
            !self@output_role %in% WEATHER_INPUT_ROLES) {
            return("`output_role` must identify one semantic output backbone.")
        }
        if (length(self@parameters) &&
            (is.null(names(self@parameters)) ||
                any(!nzchar(names(self@parameters))) ||
                anyDuplicated(names(self@parameters)))) {
            return("`parameters` must be a uniquely named list.")
        }
        if (length(self@stochastic) != 1L || is.na(self@stochastic)) {
            return("`stochastic` must be one non-missing logical value.")
        }
        if (anyNA(self@stochastic_variables) ||
            any(!nzchar(self@stochastic_variables)) ||
            anyDuplicated(self@stochastic_variables) ||
            !all(self@stochastic_variables %in% self@output_variables)) {
            return(
                "`stochastic_variables` must contain unique output variable IDs."
            )
        }
        if (!identical(self@stochastic, length(self@stochastic_variables) > 0L)) {
            return(
                "`stochastic` must agree with whether `stochastic_variables` is non-empty."
            )
        }
        NULL
    }
)

# Construct a normalized method record without accepting any data-source,
# period, calendar, physical-policy, or output-workflow setting.
method__spec <- function(
    name,
    label,
    domain,
    implementation,
    implementation_key,
    frequencies,
    input_roles,
    variable_sets,
    output_variables = unique(unlist(variable_sets, use.names = FALSE)),
    output_role,
    parameters = list(),
    stochastic_variables = character(),
    evidence = "published",
    references,
    version = 1L
) {
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_count(version, positive = TRUE)
    checkmate::assert_string(label, min.chars = 1L)
    checkmate::assert_choice(domain, WEATHER_METHOD_DOMAINS)
    checkmate::assert_choice(
        implementation,
        WEATHER_METHOD_IMPLEMENTATIONS
    )
    checkmate::assert_string(
        implementation_key,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    frequencies <- weather__descriptor_values(frequencies, "frequencies")
    input_roles <- weather__descriptor_values(input_roles, "input_roles")
    checkmate::assert_subset(input_roles, WEATHER_INPUT_ROLES)
    variable_sets <- component__variable_sets(variable_sets)
    output_variables <- weather__descriptor_values(
        output_variables,
        "output_variables"
    )
    checkmate::assert_choice(output_role, WEATHER_INPUT_ROLES)
    checkmate::assert_list(parameters, names = "unique")
    stochastic_variables <- weather__descriptor_values(
        stochastic_variables,
        "stochastic_variables"
    )
    checkmate::assert_subset(stochastic_variables, output_variables)
    checkmate::assert_string(evidence, min.chars = 1L)
    references <- weather__descriptor_values(references, "references")

    WeatherMethodSpec(
        name = name,
        version = as.integer(version),
        label = label,
        domain = domain,
        implementation = implementation,
        implementation_key = implementation_key,
        frequencies = frequencies,
        input_roles = input_roles,
        variable_sets = variable_sets,
        output_variables = output_variables,
        output_role = output_role,
        parameters = parameters,
        stochastic = length(stochastic_variables) > 0L,
        stochastic_variables = stochastic_variables,
        evidence = evidence,
        references = references
    )
}

# Convert an existing signal component into a method-only specification while
# allowing multivariate methods to override their required variable set.
method__from_signal_component <- function(
    name,
    label,
    domain,
    component,
    frequencies,
    input_roles = NULL,
    variable_sets = NULL,
    output_variables = NULL,
    output_role = NULL,
    stochastic_variables = NULL,
    evidence = NULL,
    references = NULL,
    version = 1L
) {
    signal <- component__get("signal", component)
    profiles <- signal@metadata$signal_profiles
    if (is.null(variable_sets)) {
        variable_sets <- as.list(names(profiles))
    }
    if (is.null(output_variables)) {
        output_variables <- unique(unlist(variable_sets, use.names = FALSE))
    }
    if (is.null(input_roles)) {
        input_roles <- setdiff(
            names(signal@required_inputs),
            "weather_template"
        )
    }
    profile_output_roles <- unique(unlist(lapply(
        profiles,
        function(profile) profile$metadata$output_role
    ), use.names = FALSE))
    if (is.null(output_role)) {
        if (length(profile_output_roles) != 1L) {
            cli::cli_abort(
                "Method {.val {name}} must declare one output role explicitly."
            )
        }
        output_role <- profile_output_roles[[1L]]
    }
    if (is.null(stochastic_variables)) {
        stochastic_variables <- if (isTRUE(signal@stochastic)) {
            names(profiles)
        } else {
            character()
        }
    }
    if (is.null(evidence)) {
        profile_evidence <- unique(vapply(
            profiles,
            `[[`,
            character(1L),
            "evidence"
        ))
        evidence <- if (length(profile_evidence) == 1L) {
            profile_evidence
        } else {
            "mixed"
        }
    }
    if (is.null(references)) {
        references <- unique(c(
            signal@metadata$references,
            unlist(lapply(profiles, `[[`, "references"), use.names = FALSE)
        ))
    }
    parameters <- lapply(profiles, `[[`, "settings")

    method__spec(
        name = name,
        label = label,
        domain = domain,
        implementation = "signal_component",
        implementation_key = component,
        frequencies = frequencies,
        input_roles = input_roles,
        variable_sets = variable_sets,
        output_variables = output_variables,
        output_role = output_role,
        parameters = parameters,
        stochastic_variables = stochastic_variables,
        evidence = evidence,
        references = references,
        version = version
    )
}

# Ensure every signal implementation referenced by a built-in method has been
# registered before method records are derived from component metadata.
method__register_components <- function() {
    bias__register_linear_scaling_component()
    bias__register_delta_change_component()
    qm__register_component()
    qdm__register_component()
    sdm__register_component()
    cdft__register_component()
    edcdf__register_component()
    isimip__register_component()
    daily__register_temperature_components()
    eames__register_monthly_temperature_components()
    ek__register_components()
    arima__register_components()
    sobie__register_components()
    hourly_kqdm__register_components()
    invisible(NULL)
}

# Build the method catalog independently of complete recipe defaults. Source
# selections and study periods remain caller-owned inputs.
method__default_specs <- function() {
    method__register_components()
    list(
        belcher_monthly = method__spec(
            name = "belcher_monthly",
            label = "Belcher monthly morphing",
            domain = "monthly_morphing",
            implementation = "backend",
            implementation_key = "belcher",
            frequencies = "mon",
            input_roles = c("model_historical", "model_future"),
            variable_sets = c(
                "tas", "psl", "rlds", "rsds", "sfcWind", "clt", "pr",
                "hurs"
            ),
            output_role = "weather_template",
            parameters = EPW_MORPH_BELCHER_PROFILE_METHODS$legacy,
            references = "https://doi.org/10.1191/0143624405bt112oa"
        ),
        epwshiftr_monthly = method__spec(
            name = "epwshiftr_monthly",
            label = "epwshiftr monthly morphing",
            domain = "monthly_morphing",
            implementation = "backend",
            implementation_key = "belcher",
            frequencies = "mon",
            input_roles = c("model_historical", "model_future"),
            variable_sets = list(
                c(
                    "tas", "psl", "rlds", "rsds", "sfcWind", "clt", "pr",
                    "hurs"
                ),
                c(
                    "tas", "psl", "rlds", "rsds", "sfcWind", "clt", "pr",
                    "huss", "ps"
                )
            ),
            output_role = "weather_template",
            parameters = EPW_MORPH_BELCHER_PROFILE_METHODS$enhanced,
            evidence = "package_method",
            references = c(
                "https://doi.org/10.1191/0143624405bt112oa",
                "https://github.com/ideas-lab-nus/epwshiftr/pull/126"
            )
        ),
        daily_temperature_delta = method__from_signal_component(
            name = "daily_temperature_delta",
            label = "Daily temperature delta",
            domain = "daily_temperature",
            component = "daily_temperature_delta",
            frequencies = "day",
            input_roles = c("model_historical", "model_future"),
            variable_sets = "tas",
            output_variables = "tas",
            output_role = "weather_template"
        ),
        eames_monthly_temperature = method__from_signal_component(
            name = "eames_monthly_temperature",
            label = "Eames monthly temperature changes",
            domain = "daily_temperature",
            component = "monthly_mean_extrema_changes",
            frequencies = "day",
            input_roles = c("model_historical", "model_future"),
            variable_sets = c("tas", "tasmin", "tasmax"),
            output_variables = "tas",
            output_role = "weather_template",
            evidence = "adapted_publication"
        ),
        ek_daily_factors = method__from_signal_component(
            name = "ek_daily_factors",
            label = "Ek daily mean and DTR factors",
            domain = "daily_temperature",
            component = "daily_mean_dtr_change_factors",
            frequencies = "day",
            input_roles = c("model_historical", "model_future"),
            variable_sets = c("tasmin", "tasmax"),
            output_variables = "tas",
            output_role = "weather_template",
            evidence = "reconstructed_publication"
        ),
        monthly_percentile_temperature = method__from_signal_component(
            name = "monthly_percentile_temperature",
            label = "Monthly percentile temperature change",
            domain = "daily_temperature",
            component = "percentile_temperature_change_function",
            frequencies = "day",
            input_roles = SIGNAL_THREE_INPUT_ROLES,
            variable_sets = "tas",
            output_variables = "tas",
            output_role = "weather_template"
        ),
        sobie_curry_daily = method__from_signal_component(
            name = "sobie_curry_daily",
            label = "Sobie-Curry daily thermodynamic factors",
            domain = "daily_temperature",
            component = "daily_thermodynamic_change_factors",
            frequencies = "day",
            input_roles = c("model_historical", "model_future"),
            variable_sets = c("tas", "tasmin", "tasmax", "huss", "ps"),
            output_variables = c("tas", "huss", "ps"),
            output_role = "weather_template"
        ),
        kernel_quantile_delta_mapping_hourly =
            method__from_signal_component(
                name = "kernel_quantile_delta_mapping_hourly",
                label = "Hourly kernel quantile delta mapping",
                domain = "hourly_direct_model",
                component = "kernel_quantile_delta_mapping_hourly",
                frequencies = "hour",
                input_roles = SIGNAL_THREE_INPUT_ROLES,
                variable_sets = c(
                    "tas", "ps", "hurs", "sfcWind", "rsds", "rsdsdiff"
                ),
                output_variables = c(
                    "tas", "ps", "hurs", "sfcWind", "rsds", "rsdsdiff"
                ),
                output_role = "model_future",
                evidence = "adapted_publication"
            ),
        linear_scaling_daily = method__from_signal_component(
            "linear_scaling_daily",
            "Daily Linear Scaling",
            "daily_bias_adjustment",
            "linear_scaling_daily",
            "day"
        ),
        delta_change_daily = method__from_signal_component(
            "delta_change_daily",
            "Daily Delta Change",
            "daily_bias_adjustment",
            "delta_change_daily",
            "day"
        ),
        quantile_mapping_daily = method__from_signal_component(
            "quantile_mapping_daily",
            "Daily Quantile Mapping",
            "daily_bias_adjustment",
            "quantile_mapping_daily",
            "day",
            stochastic_variables = "pr"
        ),
        quantile_delta_mapping_daily = method__from_signal_component(
            "quantile_delta_mapping_daily",
            "Daily Quantile Delta Mapping",
            "daily_bias_adjustment",
            "quantile_delta_mapping_daily",
            "day",
            stochastic_variables = "pr"
        ),
        scaled_distribution_mapping_daily = method__from_signal_component(
            "scaled_distribution_mapping_daily",
            "Daily Scaled Distribution Mapping",
            "daily_bias_adjustment",
            "scaled_distribution_mapping_daily",
            "day"
        ),
        cdf_transform_daily = method__from_signal_component(
            "cdf_transform_daily",
            "Daily CDF-t",
            "daily_bias_adjustment",
            "cdf_transform_daily",
            "day",
            stochastic_variables = "pr"
        ),
        equidistant_cdf_matching_daily = method__from_signal_component(
            "equidistant_cdf_matching_daily",
            "Daily Equidistant CDF Matching",
            "daily_bias_adjustment",
            "equidistant_cdf_matching_daily",
            "day"
        ),
        isimip3basd_daily = method__from_signal_component(
            "isimip3basd_daily",
            "Daily ISIMIP3BASD",
            "daily_bias_adjustment",
            "isimip3basd_daily",
            "day",
            stochastic_variables = "pr"
        )
    )
}

# Register a method definition without allowing silent replacement of one
# stable method identifier.
method__register <- function(
    spec,
    overwrite = FALSE,
    registry = WEATHER_METHOD_REGISTRY
) {
    if (!S7::S7_inherits(spec, WeatherMethodSpec)) {
        cli::cli_abort("{.arg spec} must be a WeatherMethodSpec object.")
    }
    checkmate::assert_flag(overwrite)
    checkmate::assert_environment(registry)
    if (exists(spec@name, envir = registry, inherits = FALSE) &&
        !isTRUE(overwrite)) {
        cli::cli_abort(
            "Future-weather method {.val {spec@name}} is already registered."
        )
    }
    assign(spec@name, spec, envir = registry)
    invisible(spec)
}

# Populate the built-in method catalog once while preserving explicit
# process-local replacements.
method__register_defaults <- function() {
    registered <- ls(envir = WEATHER_METHOD_REGISTRY, all.names = FALSE)
    if (all(WEATHER_METHOD_DEFAULTS %in% registered)) {
        return(invisible(NULL))
    }
    for (spec in method__default_specs()) {
        if (!exists(
            spec@name,
            envir = WEATHER_METHOD_REGISTRY,
            inherits = FALSE
        )) {
            method__register(spec)
        }
    }
    invisible(NULL)
}

# Retrieve one stable method definition and optionally enforce its persisted
# catalog version.
method__get <- function(
    name,
    version = NULL,
    registry = WEATHER_METHOD_REGISTRY
) {
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_environment(registry)
    if (identical(registry, WEATHER_METHOD_REGISTRY)) {
        method__register_defaults()
    }
    name <- tolower(name)
    if (!exists(name, envir = registry, inherits = FALSE)) {
        cli::cli_abort("Unknown future-weather method: {.val {name}}.")
    }
    spec <- get(name, envir = registry, inherits = FALSE)
    if (!is.null(version)) {
        checkmate::assert_count(version, positive = TRUE)
        if (!identical(spec@version, as.integer(version))) {
            cli::cli_abort(
                "Future-weather method {.val {name}} requires definition version {spec@version}; persisted version is {as.integer(version)}."
            )
        }
    }
    spec
}

# Flatten method capabilities for user inspection without exposing executable
# component or backend objects.
method__list <- function(registry = WEATHER_METHOD_REGISTRY) {
    checkmate::assert_environment(registry)
    if (identical(registry, WEATHER_METHOD_REGISTRY)) {
        method__register_defaults()
    }
    names <- sort(ls(envir = registry, all.names = FALSE))
    data.table::rbindlist(lapply(names, function(name) {
        spec <- get(name, envir = registry, inherits = FALSE)
        data.table::data.table(
            name = spec@name,
            version = spec@version,
            label = spec@label,
            domain = spec@domain,
            implementation = spec@implementation,
            implementation_key = spec@implementation_key,
            frequencies = list(spec@frequencies),
            input_roles = list(spec@input_roles),
            variable_sets = list(spec@variable_sets),
            variables = list(unique(unlist(
                spec@variable_sets,
                use.names = FALSE
            ))),
            output_variables = list(spec@output_variables),
            output_role = spec@output_role,
            parameters = list(spec@parameters),
            stochastic = spec@stochastic,
            stochastic_variables = list(spec@stochastic_variables),
            evidence = spec@evidence,
            references = list(spec@references)
        )
    }), use.names = TRUE, fill = TRUE)
}

#' Inspect registered future-weather methods
#'
#' `epw_morph_methods()` lists method-owned algorithm contracts independently
#' of data sources, study periods, calendars, physical policies, and output
#' workflows.
#'
#' @return A data table with one row per registered method.
#'
#' @seealso [epw_morph_recipes()], [epw_morph_recipe_spec()]
#' @export
epw_morph_methods <- function() {
    method__list()
}

#' Get a registered future-weather method specification
#'
#' @param name Stable method name returned by [epw_morph_methods()].
#'
#' @return A `WeatherMethodSpec` object.
#'
#' @seealso [epw_morph_methods()], [epw_morph_recipe_spec()]
#' @export
epw_morph_method_spec <- function(name) {
    method__get(name)
}
