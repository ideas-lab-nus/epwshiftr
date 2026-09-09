#' @include weather-pipeline.R
NULL

# Complete-recipe policies distinguish a published method's field treatment
# from epwshiftr's harmonized physical closure and output controls.
WEATHER_RECIPE_POLICIES <- c("paper_faithful", "harmonized")

# Output types keep representative weather years distinct from continuous
# future realizations and multi-year ensembles.
WEATHER_RECIPE_OUTPUT_TYPES <- c(
    "representative_year",
    "future_year",
    "multi_year"
)

# Existing monolithic backends remain explicit adapters while componentized
# methods can be checked against the executable seven-stage registry.
WEATHER_RECIPE_IMPLEMENTATIONS <- c("backend", "pipeline")

# Recipe status records implementation maturity independently of method type.
WEATHER_RECIPE_STATUSES <- c("production", "experimental")

# Built-in keys allow idempotent registration to return without rebuilding
# component and input specifications on every recipe construction.
WEATHER_RECIPE_DEFAULTS <- c(
    "original_morphing_monthly",
    "epwshiftr_monthly",
    "epwshiftr_daily_power",
    "epwshiftr_daily_btws",
    "bws_btws_monthly",
    "ek_daily_factors",
    "quantile_mapping_morphing_daily",
    "sobie_curry_daily",
    "hourly_kernel_qdm",
    "linear_scaling_daily_temperature",
    "delta_change_daily_temperature",
    "quantile_mapping_daily_temperature",
    "quantile_delta_mapping_daily_temperature",
    "scaled_distribution_mapping_daily_temperature",
    "cdf_transform_daily_temperature",
    "equidistant_cdf_matching_daily_temperature",
    "isimip3basd_daily_temperature"
)

# Recipe definitions contain only stable metadata. Executable functions remain
# in the backend and component registries.
WEATHER_RECIPE_REGISTRY <- new.env(parent = emptyenv())

# WeatherRecipeSpec describes one complete future-weather method independently
# of a configured run. Its identifiers are safe to persist and resolve later.
WeatherRecipeSpec <- S7::new_class(
    "WeatherRecipeSpec",
    properties = list(
        name = S7::new_property(S7::class_character),
        version = S7::new_property(S7::class_integer),
        label = S7::new_property(S7::class_character),
        method = S7::new_property(S7::class_character),
        backend = S7::new_property(S7::class_character),
        implementation = S7::new_property(S7::class_character),
        source = S7::new_property(S7::class_list),
        required_inputs = S7::new_property(
            S7::class_list,
            default = list()
        ),
        optional_inputs = S7::new_property(
            S7::class_list,
            default = list()
        ),
        calendar_policy = S7::new_property(S7::class_character),
        target_calendar = S7::new_property(
            S7::class_character,
            default = character()
        ),
        components = S7::new_property(S7::class_list),
        policy_profiles = S7::new_property(S7::class_character),
        physical_policies = S7::new_property(S7::class_character),
        default_policy = S7::new_property(S7::class_character),
        output_type = S7::new_property(S7::class_character),
        stochastic = S7::new_property(S7::class_logical),
        diagnostics = S7::new_property(
            S7::class_character,
            default = character()
        ),
        provenance = S7::new_property(
            S7::class_character,
            default = character()
        ),
        status = S7::new_property(S7::class_character)
    ),
    validator = function(self) {
        if (length(self@name) != 1L ||
            is.na(self@name) ||
            !grepl("^[a-z][a-z0-9_]*$", self@name)) {
            return("`name` must be one lower snake_case recipe identifier.")
        }
        if (length(self@version) != 1L ||
            is.na(self@version) ||
            self@version < 1L) {
            return("`version` must be one positive integer.")
        }
        for (property in c(
            "label",
            "method",
            "backend",
            "calendar_policy",
            "default_policy",
            "output_type",
            "status"
        )) {
            value <- S7::prop(self, property)
            if (length(value) != 1L || is.na(value) || !nzchar(value)) {
                return(sprintf("`%s` must be one non-empty string.", property))
            }
        }
        if (length(self@implementation) != 1L ||
            is.na(self@implementation) ||
            !self@implementation %in% WEATHER_RECIPE_IMPLEMENTATIONS) {
            return("`implementation` must be `backend` or `pipeline`.")
        }
        if (length(self@target_calendar) > 1L ||
            anyNA(self@target_calendar) ||
            any(!grepl(
                "^[a-z][a-z0-9_]*$",
                self@target_calendar
            ))) {
            return(
                "`target_calendar` must be empty or one lower snake_case identifier."
            )
        }
        if (!identical(names(self@components), WEATHER_COMPONENT_STAGES)) {
            return(
                "`components` must name every ordered future-weather stage."
            )
        }
        valid_components <- vapply(
            self@components,
            function(component) {
                is.character(component) &&
                    length(component) == 1L &&
                    !is.na(component) &&
                    grepl("^[a-z][a-z0-9_]*$", component)
            },
            logical(1L)
        )
        if (!all(valid_components)) {
            return(
                "Every recipe component must be one lower snake_case name."
            )
        }
        if (is.null(names(self@policy_profiles)) ||
            any(!nzchar(names(self@policy_profiles))) ||
            anyDuplicated(names(self@policy_profiles)) ||
            anyNA(self@policy_profiles) ||
            any(!nzchar(self@policy_profiles))) {
            return(
                "`policy_profiles` must be a uniquely named, non-empty character vector."
            )
        }
        if (!all(names(self@policy_profiles) %in%
            WEATHER_RECIPE_POLICIES)) {
            return("`policy_profiles` contains an unknown execution policy.")
        }
        if (!self@default_policy %in% names(self@policy_profiles)) {
            return(
                "`default_policy` must identify one supported execution policy."
            )
        }
        if (!identical(
            names(self@physical_policies),
            names(self@policy_profiles)
        ) ||
            anyNA(self@physical_policies) ||
            any(!nzchar(self@physical_policies)) ||
            any(!self@physical_policies %in% names(EPW_PHYS_POLICY_SPECS))) {
            return(
                "`physical_policies` must map every execution policy to one built-in EPW physical policy."
            )
        }
        if (!self@output_type %in% WEATHER_RECIPE_OUTPUT_TYPES) {
            return("`output_type` contains an unknown weather output type.")
        }
        if (length(self@stochastic) != 1L || is.na(self@stochastic)) {
            return("`stochastic` must be one non-missing logical value.")
        }
        if (!self@status %in% WEATHER_RECIPE_STATUSES) {
            return("`status` contains an unknown recipe status.")
        }
        if (is.null(names(self@source)) ||
            any(!nzchar(names(self@source))) ||
            anyDuplicated(names(self@source)) ||
            !all(c("type", "citation") %in% names(self@source))) {
            return(
                "`source` must be a named list containing `type` and `citation`."
            )
        }
        for (property in c("type", "citation")) {
            value <- self@source[[property]]
            if (!is.character(value) ||
                !length(value) ||
                anyNA(value) ||
                any(!nzchar(value))) {
                return(sprintf(
                    "`source$%s` must contain non-empty text.",
                    property
                ))
            }
        }
        for (requirements in list(
            self@required_inputs,
            self@optional_inputs
        )) {
            if (length(requirements) &&
                (is.null(names(requirements)) ||
                    any(!nzchar(names(requirements))) ||
                    anyDuplicated(names(requirements)))) {
                return("Recipe input requirements must be uniquely role-named.")
            }
            for (role in names(requirements)) {
                requirement <- requirements[[role]]
                if (!S7::S7_inherits(
                    requirement,
                    WeatherInputRequirement
                ) ||
                    !identical(requirement@role, role)) {
                    return(sprintf(
                        "Recipe input `%s` must contain a matching WeatherInputRequirement.",
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
            return(
                "An input role cannot be both required and optional."
            )
        }
        for (property in c("diagnostics", "provenance")) {
            value <- S7::prop(self, property)
            if (anyNA(value) ||
                any(!nzchar(value)) ||
                anyDuplicated(value)) {
                return(sprintf(
                    "`%s` must contain unique, non-empty identifiers.",
                    property
                ))
            }
        }
        NULL
    }
)

# Construct and normalize one serializable recipe specification before it
# enters the process-local registry.
recipe__spec <- function(
    name,
    label,
    method,
    backend,
    implementation,
    source,
    required_inputs,
    optional_inputs = list(),
    calendar_policy,
    target_calendar = character(),
    components,
    policy_profiles,
    physical_policies,
    default_policy,
    output_type = "representative_year",
    stochastic = FALSE,
    diagnostics = character(),
    provenance = character(),
    status = "experimental",
    version = 1L
) {
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_count(version, positive = TRUE)
    checkmate::assert_string(label, min.chars = 1L)
    checkmate::assert_string(method, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_string(backend, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_choice(
        implementation,
        WEATHER_RECIPE_IMPLEMENTATIONS
    )
    checkmate::assert_list(source, names = "unique")
    if (is.null(names(source)) || any(!nzchar(names(source)))) {
        cli::cli_abort("{.arg source} must be a named list.")
    }
    if (!all(c("type", "citation") %in% names(source))) {
        cli::cli_abort(
            "{.arg source} must contain {.field type} and {.field citation}."
        )
    }
    source <- lapply(source, function(value) {
        checkmate::assert_character(value, any.missing = FALSE)
        as.character(value)
    })
    for (property in c("type", "citation")) {
        checkmate::assert_character(
            source[[property]],
            any.missing = FALSE,
            min.len = 1L
        )
        if (any(!nzchar(source[[property]]))) {
            cli::cli_abort(
                "{.arg source} field {.field {property}} cannot contain empty text."
            )
        }
    }
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
            "Recipe input role(s) cannot be both required and optional: {.val {overlap}}."
        )
    }
    checkmate::assert_string(
        calendar_policy,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    checkmate::assert_character(
        target_calendar,
        max.len = 1L,
        any.missing = FALSE
    )
    if (length(target_calendar)) {
        checkmate::assert_string(
            target_calendar,
            pattern = "^[a-z][a-z0-9_]*$"
        )
    }
    if (is.character(components) && !is.null(names(components))) {
        components <- as.list(components)
    }
    checkmate::assert_list(components, names = "unique")
    missing_stages <- setdiff(WEATHER_COMPONENT_STAGES, names(components))
    unknown_stages <- setdiff(names(components), WEATHER_COMPONENT_STAGES)
    if (length(missing_stages) || length(unknown_stages)) {
        cli::cli_abort(c(
            "A recipe must identify all seven future-weather stages.",
            "x" = if (length(missing_stages)) {
                "Missing stage(s): {.val {missing_stages}}."
            },
            "x" = if (length(unknown_stages)) {
                "Unknown stage(s): {.val {unknown_stages}}."
            }
        ))
    }
    components <- components[WEATHER_COMPONENT_STAGES]
    checkmate::assert_character(
        policy_profiles,
        any.missing = FALSE,
        min.len = 1L,
        names = "named"
    )
    if (any(!nzchar(names(policy_profiles))) ||
        any(!nzchar(policy_profiles))) {
        cli::cli_abort(
            "{.arg policy_profiles} cannot contain empty policy or profile names."
        )
    }
    checkmate::assert_subset(
        names(policy_profiles),
        WEATHER_RECIPE_POLICIES
    )
    checkmate::assert_character(
        physical_policies,
        any.missing = FALSE,
        min.len = 1L,
        names = "named"
    )
    if (!identical(names(physical_policies), names(policy_profiles))) {
        cli::cli_abort(
            "{.arg physical_policies} must map every declared execution policy in the same order."
        )
    }
    checkmate::assert_subset(
        unname(physical_policies),
        names(EPW_PHYS_POLICY_SPECS)
    )
    checkmate::assert_choice(default_policy, names(policy_profiles))
    checkmate::assert_choice(output_type, WEATHER_RECIPE_OUTPUT_TYPES)
    checkmate::assert_flag(stochastic)
    diagnostics <- weather__descriptor_values(
        diagnostics,
        "diagnostics"
    )
    provenance <- weather__descriptor_values(
        provenance,
        "provenance"
    )
    checkmate::assert_choice(status, WEATHER_RECIPE_STATUSES)

    WeatherRecipeSpec(
        name = name,
        version = as.integer(version),
        label = label,
        method = method,
        backend = backend,
        implementation = implementation,
        source = source,
        required_inputs = required_inputs,
        optional_inputs = optional_inputs,
        calendar_policy = calendar_policy,
        target_calendar = target_calendar,
        components = components,
        policy_profiles = policy_profiles,
        physical_policies = physical_policies,
        default_policy = default_policy,
        output_type = output_type,
        stochastic = stochastic,
        diagnostics = diagnostics,
        provenance = provenance,
        status = status
    )
}

# Return the stable conceptual stages used by the two existing monthly
# backends. They remain backend adapters until their monolithic runner is split.
recipe__monthly_components <- function(enhanced = FALSE) {
    checkmate::assert_flag(enhanced)
    list(
        preprocess = "monthly_climate_summary",
        calendar = "gregorian_months",
        signal = if (enhanced) {
            "epwshiftr_change_factors"
        } else {
            "original_morphing_change_factors"
        },
        sequence = "preserve_epw_sequence",
        hourly = if (enhanced) {
            "epwshiftr_monthly_transform"
        } else {
            "original_morphing_monthly_transform"
        },
        physics = if (enhanced) {
            "enhanced_weather_closure"
        } else {
            "legacy_epw_field_closure"
        },
        output = "epw_result"
    )
}

# Declare the monthly Belcher climate inputs once. The publication-defined
# recipe requires temperature mean and average-daily-extrema changes, while
# the enhanced package method may fall back when extrema are unavailable.
recipe__monthly_inputs <- function(enhanced = FALSE) {
    checkmate::assert_flag(enhanced)
    common <- c(
        "tas", "psl", "rlds", "rsds", "sfcWind", "clt", "pr"
    )
    if (!enhanced) {
        common <- c("tas", "tasmax", "tasmin", setdiff(common, "tas"))
    }
    variables <- if (enhanced) {
        list(
            c(common, "hurs"),
            c(common, "huss", "ps")
        )
    } else {
        c(common, "hurs")
    }
    list(
        weather_template = component__input_requirement(
            "weather_template",
            representations = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        ),
        model_historical = component__input_requirement(
            "model_historical",
            representations = "series",
            frequencies = "mon",
            variable_sets = variables
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = "mon",
            variable_sets = variables
        )
    )
}

# Declare the shared temperature input contract once so all eight daily
# adjustment recipes receive the same semantic roles and frequencies.
recipe__daily_adjustment_inputs <- function() {
    c(
        list(
            weather_template = component__input_requirement(
                "weather_template",
                representations = "epw",
                frequencies = "hour",
                calendars = "gregorian"
            )
        ),
        signal__three_role_requirements(
            "tas",
            frequencies = "day",
            calendars = CF_TIME_CALENDARS
        )
    )
}

# Build complete temperature recipes from method records. Only the signal
# component and backend identifier vary; the reusable EPW adapter stays fixed.
recipe__daily_adjustment_specs <- function() {
    inputs <- recipe__daily_adjustment_inputs()
    specs <- lapply(names(DAILY_ADJUSTMENT_METHOD_COMPONENTS), function(key) {
        method_name <- DAILY_ADJUSTMENT_METHOD_COMPONENTS[[key]]
        method <- method__get(method_name)
        signal <- component__get("signal", method@implementation_key)
        profile <- signal@metadata$signal_profiles$tas
        pipeline <- daily_adjustment__pipeline(key)
        # The published signal kernel is combined with epwshiftr's common
        # daily-to-hourly EPW adapter, so the complete transform is an adapted
        # publication even when the signal itself is published unchanged.
        transform_evidence <- if (identical(
            profile$evidence,
            "experimental"
        )) {
            "experimental"
        } else {
            "adapted_publication"
        }
        recipe__spec(
            name = paste0(method_name, "_temperature"),
            label = paste(method@label, "temperature EPW"),
            method = method@name,
            backend = DAILY_ADJUSTMENT_BACKENDS[[key]],
            implementation = "pipeline",
            source = list(
                type = transform_evidence,
                citation = method@label,
                signal_evidence = profile$evidence,
                references = profile$references
            ),
            required_inputs = inputs,
            calendar_policy = "cf_annual_phase_365",
            components = pipeline__records(pipeline),
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(
                harmonized = "preserve_specific_humidity"
            ),
            default_policy = "harmonized",
            output_type = "representative_year",
            stochastic = "tas" %in% method@stochastic_variables,
            diagnostics = c(
                "daily_target_closure",
                "humidity_closure",
                "physical_bounds"
            ),
            provenance = c(
                "weather_method",
                "input_periods",
                "calendar_mapping",
                "signal_settings",
                "physical_policy"
            ),
            status = "experimental"
        )
    })
    names(specs) <- vapply(specs, function(spec) spec@name, character(1L))
    specs
}

# Build the complete built-in recipes without embedding backend runners or
# component functions in their definitions.
recipe__default_specs <- function() {
    faithful_inputs <- recipe__monthly_inputs(enhanced = FALSE)
    enhanced_inputs <- recipe__monthly_inputs(enhanced = TRUE)
    daily_pipeline <- daily__temperature_pipeline()
    daily_btws_pipeline <- daily__temperature_pipeline("btws")
    bws_btws_pipeline <- bws_btws__pipeline()
    ek_pipeline <- ek__pipeline()
    quantile_mapping_pipeline <- quantile_mapping_morphing__pipeline()
    sobie_pipeline <- sobie__pipeline()
    hourly_kqdm_pipeline <- hourly_kqdm__pipeline()
    daily_inputs <- list(
        weather_template = component__input_requirement(
            "weather_template",
            representations = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        ),
        model_historical = component__input_requirement(
            "model_historical",
            representations = "series",
            frequencies = "day",
            variable_sets = "tas"
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = "day",
            variable_sets = "tas"
        )
    )
    daily_btws_inputs <- list(
        weather_template = daily_inputs$weather_template,
        model_historical = component__input_requirement(
            "model_historical",
            representations = "series",
            frequencies = "day",
            variable_sets = c("tas", "tasmin", "tasmax")
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = "day",
            variable_sets = c("tas", "tasmin", "tasmax")
        )
    )
    bws_btws_inputs <- bws_btws__inputs()
    ek_inputs <- ek__daily_temperature_inputs()
    quantile_mapping_inputs <- quantile_mapping_morphing__temperature_inputs()
    sobie_inputs <- list(
        weather_template = component__input_requirement(
            "weather_template",
            representations = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        ),
        model_historical = component__input_requirement(
            "model_historical",
            representations = "series",
            frequencies = "day",
            variable_sets = c("tas", "tasmin", "tasmax", "huss", "ps")
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = "day",
            variable_sets = c("tas", "tasmin", "tasmax", "huss", "ps")
        )
    )
    hourly_kqdm_inputs <- list(
        weather_template = component__input_requirement(
            "weather_template",
            representations = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        ),
        observed_reference = component__input_requirement(
            "observed_reference",
            representations = "series",
            frequencies = "hour",
            calendars = CF_TIME_CALENDARS,
            variable_sets = EPW_MORPH_HOURLY_KQDM_VARIABLES
        ),
        model_historical = component__input_requirement(
            "model_historical",
            representations = "series",
            frequencies = unique(unname(HOURLY_KQDM_MODEL_FREQUENCIES)),
            variable_frequencies = as.list(
                HOURLY_KQDM_MODEL_FREQUENCIES
            ),
            calendars = CF_TIME_CALENDARS,
            variable_sets = EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = unique(unname(HOURLY_KQDM_MODEL_FREQUENCIES)),
            variable_frequencies = as.list(
                HOURLY_KQDM_MODEL_FREQUENCIES
            ),
            calendars = CF_TIME_CALENDARS,
            variable_sets = EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES
        )
    )

    builtins <- list(
        original_morphing_monthly = recipe__spec(
            name = "original_morphing_monthly",
            label = "Original monthly morphing",
            method = "original_morphing_monthly",
            backend = "original_morphing",
            implementation = "backend",
            source = list(
                type = "published",
                citation = paste(
                    "Belcher, Hacker, and Powell (2005),",
                    "Constructing design weather data for future climates"
                ),
                references = "https://doi.org/10.1191/0143624405bt112oa",
                equation_note = paste(
                    "Dry-bulb temperature uses the published combined",
                    "transformation: monthly mean change plus the change",
                    "in average daily temperature range derived from tasmax",
                    "and tasmin. The EPW denominator is its monthly average",
                    "daily maximum minus average daily minimum."
                )
            ),
            required_inputs = faithful_inputs,
            calendar_policy = "monthly_gregorian",
            target_calendar = "epw_365_day",
            components = recipe__monthly_components(),
            policy_profiles = c(paper_faithful = "legacy"),
            physical_policies = c(
                paper_faithful = "legacy_independent_fields"
            ),
            default_policy = "paper_faithful",
            diagnostics = c(
                "monthly_target_closure",
                "physical_bounds"
            ),
            provenance = c(
                "source_method",
                "backend_profile",
                "input_periods",
                "component_names"
            ),
            status = "production",
            version = 2L
        ),
        epwshiftr_monthly = recipe__spec(
            name = "epwshiftr_monthly",
            label = "Enhanced epwshiftr monthly morphing",
            method = "epwshiftr_monthly",
            backend = "original_morphing",
            implementation = "backend",
            source = list(
                type = "package_method",
                citation = "epwshiftr enhanced Belcher workflow",
                references = c(
                    "https://doi.org/10.1191/0143624405bt112oa",
                    "https://github.com/ideas-lab-nus/epwshiftr/pull/126"
                )
            ),
            required_inputs = enhanced_inputs[
                c("weather_template", "model_future")
            ],
            optional_inputs = enhanced_inputs["model_historical"],
            calendar_policy = "monthly_gregorian",
            target_calendar = "epw_365_day",
            components = recipe__monthly_components(enhanced = TRUE),
            policy_profiles = c(harmonized = "enhanced"),
            physical_policies = c(
                harmonized = "monthly_harmonized"
            ),
            default_policy = "harmonized",
            diagnostics = c(
                "monthly_target_closure",
                "humidity_closure",
                "radiation_closure",
                "physical_bounds"
            ),
            provenance = c(
                "source_method",
                "backend_profile",
                "input_periods",
                "component_names",
                "physical_policies"
            ),
            status = "production"
        ),
        epwshiftr_daily_power = recipe__spec(
            name = "epwshiftr_daily_power",
            label = "Daily power-constrained temperature projection",
            method = "daily_temperature_delta",
            backend = "daily_temperature",
            implementation = "pipeline",
            source = list(
                type = "package_method",
                citation = paste(
                    "Sobie-Curry-style daily climatological signals with",
                    "a monotone bounded power transfer"
                ),
                references = c(
                    "https://doi.org/10.1016/j.dib.2025.111667",
                    "https://github.com/ideas-lab-nus/epwshiftr/pull/141"
                )
            ),
            required_inputs = daily_inputs,
            calendar_policy = "cf_annual_phase_365",
            components = pipeline__records(daily_pipeline),
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(
                harmonized = "preserve_specific_humidity"
            ),
            default_policy = "harmonized",
            diagnostics = c(
                "daily_target_closure",
                "daily_extrema_closure",
                "humidity_closure",
                "day_boundary_jump",
                "fallback_status"
            ),
            provenance = c(
                "source_method",
                "backend_profile",
                "input_periods",
                "calendar_mapping",
                "component_names",
                "physical_policies"
            ),
            status = "experimental"
        ),
        epwshiftr_daily_btws = recipe__spec(
            name = "epwshiftr_daily_btws",
            label = "Daily CMIP6 signal with BTWS projection",
            method = "daily_temperature_delta",
            backend = "daily_temperature_btws",
            implementation = "pipeline",
            source = list(
                type = "package_method",
                citation = paste(
                    "epwshiftr daily CMIP6 temperature targets combined with",
                    "the hourly bounded temperature weighted stretch from",
                    "Eames et al. (2024)"
                ),
                references = c(
                    "https://github.com/ideas-lab-nus/epwshiftr/pull/141",
                    "https://doi.org/10.1177/01436244231218861"
                ),
                equation_note = paste(
                    "Equations (7)-(16) are used for the hourly projection.",
                    "Where the paper does not publish solver code, epwshiftr",
                    "uses deterministic bisection to retain the largest",
                    "admissible m or n in [0, 1]."
                ),
                signal_note = paste(
                    "Eames et al. use monthly UKCP18 change factors, not daily",
                    "CMIP6 series. This recipe supplies epwshiftr daily CMIP6",
                    "targets to the published hourly reconstruction component."
                )
            ),
            required_inputs = daily_btws_inputs,
            calendar_policy = "cf_annual_phase_365",
            components = pipeline__records(daily_btws_pipeline),
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(
                harmonized = "preserve_specific_humidity"
            ),
            default_policy = "harmonized",
            diagnostics = c(
                "daily_target_closure",
                "daily_extrema_closure",
                "btws_scale_and_exponents",
                "mean_shift_fallback",
                "humidity_closure",
                "day_boundary_jump"
            ),
            provenance = c(
                "source_methods",
                "backend_profile",
                "input_periods",
                "calendar_mapping",
                "component_names",
                "equation_interpretation",
                "physical_policies"
            ),
            status = "experimental"
        ),
        bws_btws_monthly = recipe__spec(
            name = "bws_btws_monthly",
            label = "BWS and BTWS monthly weather morphing",
            method = "bws_btws_monthly",
            backend = "bws_btws_monthly",
            implementation = "pipeline",
            source = list(
                type = "adapted_publication",
                citation = paste(
                    "Eames et al. (2024) bounded weather stretch for cloud",
                    "cover and global solar radiation plus bounded",
                    "temperature weighted stretch using monthly UKCP18",
                    "change factors"
                ),
                references = c(
                    "https://doi.org/10.1177/01436244231218861",
                    "https://github.com/ideas-lab-nus/epwshiftr/issues/152"
                ),
                equation_note = paste(
                    "The published BWS equations (7)-(8) transform cloud",
                    "cover and global solar radiation; equations (9)-(16)",
                    "transform daily temperature mean and extrema.",
                    "Where the paper does not publish solver code, epwshiftr",
                    "uses deterministic bisection to retain the largest",
                    "admissible m or n in [0, 1]."
                ),
                signal_note = paste(
                    "The paper obtains monthly factors from UKCP18.",
                    "epwshiftr derives matching change factors from monthly",
                    "CMIP6 tas, tasmin, tasmax, rsds, and clt;",
                    "it does not apply daily-varying change factors."
                ),
                implementation_note = paste(
                    "The BWS kernel retains zero and upper-bound states.",
                    "For EPW cloud cover, the paper's normalized bounded",
                    "equation is applied on the equivalent 0-10 tenths scale.",
                    "The unified physical layer closes humidity, diffuse and",
                    "direct radiation after all method candidates are formed."
                )
            ),
            required_inputs = bws_btws_inputs,
            calendar_policy = "cf_calendar_month_to_epw_365",
            components = pipeline__records(bws_btws_pipeline),
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(
                harmonized = "bws_btws_weather"
            ),
            default_policy = "harmonized",
            diagnostics = c(
                "monthly_temperature_radiation_cloud_changes",
                "daily_extrema_closure",
                "btws_scale_and_exponents",
                "bws_scale_and_exponents",
                "mean_shift_fallback",
                "humidity_closure",
                "shortwave_closure",
                "day_boundary_jump"
            ),
            provenance = c(
                "source_method",
                "input_periods",
                "calendar_mapping",
                "component_names",
                "equation_interpretation",
                "adaptation_boundary",
                "physical_policies"
            ),
            status = "experimental"
        ),
        ek_daily_factors = recipe__spec(
            name = "ek_daily_factors",
            label = "Ek daily temperature change factors",
            method = "ek_daily_factors",
            backend = "ek_daily_temperature",
            implementation = "pipeline",
            source = list(
                type = "reconstructed_publication",
                citation = paste(
                    "Ek et al. (2018), Future weather files to support",
                    "climate resilient building design in Vancouver"
                ),
                references = paste0(
                    "https://dspace.library.uvic.ca/items/",
                    "5e8e6684-c704-4d2e-8480-2c81bdbafde9"
                ),
                equation_note = paste(
                    "Temperature mean is reconstructed as",
                    "(tasmin + tasmax) / 2. The anomaly multiplier is the",
                    "relative DTR change so zero climate change is an",
                    "identity and equation (5) closes on the stated daily",
                    "mean and variance behavior."
                ),
                ambiguity_note = paste(
                    "Equation (2), equation (5), and the accompanying",
                    "variance text are not fully self-consistent, and the",
                    "original Matlab implementation is unavailable.",
                    "The selected interpretation follows the temperature",
                    "inputs in Table 2 and the Belcher combined transform."
                ),
                implementation_note = paste(
                    "This recipe implements dry-bulb temperature only.",
                    "The publication's wind and cloud prose conflicts with",
                    "Table 2, so unsupported variables are not invented."
                )
            ),
            required_inputs = ek_inputs,
            calendar_policy = "cf_yearly_linear_to_epw_365",
            target_calendar = "epw_365_day",
            components = pipeline__records(ek_pipeline),
            policy_profiles = c(
                paper_faithful = "default",
                harmonized = "default"
            ),
            physical_policies = c(
                paper_faithful = "preserve_humidity_fields",
                harmonized = "preserve_specific_humidity"
            ),
            default_policy = "paper_faithful",
            diagnostics = c(
                "daily_mean_closure",
                "daily_dtr_closure",
                "zero_historical_dtr_fallback",
                "day_boundary_jump",
                "temperature_humidity_consistency"
            ),
            provenance = c(
                "source_method",
                "input_periods",
                "calendar_mapping",
                "component_names",
                "equation_interpretation",
                "unsupported_variables",
                "physical_policy"
            ),
            status = "experimental"
        ),
        quantile_mapping_morphing_daily = recipe__spec(
            name = "quantile_mapping_morphing_daily",
            label = "Quantile-mapping morphing for daily temperature",
            method = "quantile_mapping_morphing_daily",
            backend = "quantile_mapping_morphing",
            implementation = "pipeline",
            source = list(
                type = "published",
                citation = paste(
                    "Arima et al. (2024), Development of Future Weather",
                    "Data Using the Quantile Mapping Technique and its",
                    "Application in Japan"
                ),
                references = c(
                    "https://doi.org/10.69357/asim2024.1178",
                    paste0(
                        "https://doi.org/10.18948/",
                        "shasetaikai.2024.5.0_85"
                    )
                ),
                implementation_note = paste(
                    "This recipe implements additive dry-bulb temperature",
                    "for one model-specific case. Multi-model percentile",
                    "averaging and non-temperature variables are separate",
                    "method extensions."
                ),
                empirical_cdf_note = paste(
                    "The publications do not identify plotting positions,",
                    "quantile interpolation, or endpoint evaluation.",
                    "epwshiftr uses midpoint ranks, R quantile type 7,",
                    "linear factor interpolation, and endpoint clamping."
                )
            ),
            required_inputs = quantile_mapping_inputs,
            calendar_policy = "native_calendar_month_distributions",
            target_calendar = "epw_365_day",
            components = pipeline__records(quantile_mapping_pipeline),
            policy_profiles = c(
                paper_faithful = "default",
                harmonized = "default"
            ),
            physical_policies = c(
                paper_faithful = "preserve_humidity_fields",
                harmonized = "preserve_specific_humidity"
            ),
            default_policy = "paper_faithful",
            diagnostics = c(
                "monthly_change_function",
                "observed_percentile",
                "percentile_endpoint_clamping",
                "temperature_humidity_consistency"
            ),
            provenance = c(
                "source_method",
                "input_periods",
                "monthly_calendar_grouping",
                "component_names",
                "empirical_cdf_conventions",
                "smoothing",
                "physical_policy"
            ),
            status = "experimental"
        ),
        hourly_kernel_qdm = recipe__spec(
            name = "hourly_kernel_qdm",
            label = "Hourly kernel QDM multi-year future weather",
            method = "kernel_quantile_delta_mapping_hourly",
            backend = "hourly_kernel_qdm",
            implementation = "pipeline",
            source = list(
                type = "adapted_publication",
                citation = paste(
                    "Wang et al. (2023), Climate data for building",
                    "simulations in EnergyPlus"
                ),
                references = c(
                    "https://doi.org/10.1038/s41467-023-41458-5",
                    "https://doi.org/10.1175/JCLI-D-14-00754.1"
                ),
                implementation_note = paste(
                    "CMIP6 3hrPt states and 3hr radiation means are",
                    "reconstructed to an hourly lattice before kernel-density QDM. Numerical KDE",
                    "and tail defaults not stated by the publication remain",
                    "explicit experimental settings."
                )
            ),
            required_inputs = hourly_kqdm_inputs,
            calendar_policy = "native_cf_hourly_to_epw_365",
            components = pipeline__records(hourly_kqdm_pipeline),
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(
                harmonized = "absolute_model_fields"
            ),
            default_policy = "harmonized",
            output_type = "multi_year",
            diagnostics = c(
                "hourly_interpolation",
                "calendar_completeness",
                "kernel_qdm_distribution",
                "physical_bounds",
                "humidity_closure",
                "radiation_closure"
            ),
            provenance = c(
                "source_method",
                "input_periods",
                "temporal_interpolation",
                "calendar_mapping",
                "signal_settings",
                "component_names",
                "physical_policies",
                "source_weather_years"
            ),
            status = "experimental"
        ),
        sobie_curry_daily = recipe__spec(
            name = "sobie_curry_daily",
            label = "Sobie-Curry daily morphing",
            method = "sobie_curry_daily",
            backend = "sobie_curry_daily",
            implementation = "pipeline",
            source = list(
                type = "published",
                citation = paste(
                    "Sobie and Curry (2025), Dataset of future-shifted",
                    "weather files for Canada using climate projections",
                    "from CMIP6"
                ),
                references = "https://doi.org/10.1016/j.dib.2025.111667",
                equation_note = paste(
                    "Dew-point alpha is implemented as sigma_future /",
                    "sigma_historical - 1 so zero change is an identity,",
                    "consistent with the paper's difference wording."
                )
            ),
            required_inputs = sobie_inputs,
            calendar_policy = "cf_annual_phase_365",
            target_calendar = "epw_365_day",
            components = pipeline__records(sobie_pipeline),
            policy_profiles = c(
                paper_faithful = "default",
                harmonized = "default"
            ),
            physical_policies = c(
                paper_faithful = "independent_thermodynamic_fields",
                harmonized = "specific_humidity_delta"
            ),
            default_policy = "paper_faithful",
            diagnostics = c(
                "daily_mean_dtr_closure",
                "dew_point_mean_closure",
                "zero_denominator_fallback",
                "independent_thermodynamic_state",
                "specific_humidity_closure"
            ),
            provenance = c(
                "source_method",
                "backend_profile",
                "input_periods",
                "calendar_mapping",
                "component_names",
                "equation_interpretation",
                "physical_policy"
            ),
            status = "experimental",
            version = 2L
        )
    )
    c(builtins, recipe__daily_adjustment_specs())
}

# Resolve the algorithm actually used at the hourly stage. Adapter components
# may expose a shared reconstruction key while keeping their own stable name.
recipe__hourly_reconstruction <- function(spec) {
    if (!identical(spec@implementation, "pipeline")) {
        return(character())
    }
    component <- component__get("hourly", spec@components$hourly)
    reconstruction <- component@metadata$reconstruction
    if (is.null(reconstruction)) {
        reconstruction <- component@name
    }
    checkmate::assert_string(
        reconstruction,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    reconstruction
}

# Resolve the output calendar declared by a recipe. Backend recipes state this
# contract directly, while componentized recipes inherit it from their output
# component unless an explicit recipe-level contract takes precedence.
recipe__target_calendar <- function(spec) {
    if (length(spec@target_calendar)) {
        return(spec@target_calendar)
    }
    if (!identical(spec@implementation, "pipeline")) {
        return(character())
    }
    component <- component__get("output", spec@components$output)
    target_calendar <- component@metadata$target_calendar
    if (is.null(target_calendar)) {
        return(character())
    }
    checkmate::assert_string(
        target_calendar,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    target_calendar
}

# Verify that a catalog entry resolves to an available backend and, for a
# pipeline method, to the same executable component sequence as that backend.
recipe__validate_registration <- function(spec) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort(
            "{.arg spec} must be a WeatherRecipeSpec object."
        )
    }
    method__get(spec@method)
    backend <- epw_morph_backend(spec@backend)
    profiles <- unname(spec@policy_profiles)
    if (spec@backend %in% c("original_morphing", "original_morphing_absolute")) {
        invalid <- setdiff(profiles, EPW_MORPH_ORIGINAL_PROFILES)
    } else {
        invalid <- setdiff(profiles, "default")
    }
    if (length(invalid)) {
        cli::cli_abort(
            "Recipe {.val {spec@name}} uses unsupported backend profile(s): {.val {invalid}}."
        )
    }
    if (identical(spec@implementation, "pipeline")) {
        pipeline <- backend$component_pipeline()
        if (is.null(pipeline)) {
            cli::cli_abort(
                "Recipe {.val {spec@name}} declares a component pipeline but backend {.val {spec@backend}} does not provide one."
            )
        }
        if (!identical(pipeline__records(pipeline), spec@components)) {
            cli::cli_abort(
                "Recipe {.val {spec@name}} components do not match backend {.val {spec@backend}}."
            )
        }
        components <- lapply(WEATHER_COMPONENT_STAGES, function(stage) {
            component__get(stage, spec@components[[stage]])
        })
        for (index in seq_len(length(components) - 1L)) {
            component__assert_compatible(
                components[[index]],
                components[[index + 1L]]
            )
        }
    }
    invisible(spec)
}

# Register one complete recipe while preventing accidental replacement of a
# stable method identifier.
recipe__register <- function(
    spec,
    overwrite = FALSE,
    registry = WEATHER_RECIPE_REGISTRY
) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort(
            "{.arg spec} must be a WeatherRecipeSpec object."
        )
    }
    checkmate::assert_flag(overwrite)
    checkmate::assert_environment(registry)
    recipe__validate_registration(spec)
    if (exists(spec@name, envir = registry, inherits = FALSE) &&
        !isTRUE(overwrite)) {
        cli::cli_abort(
            "Future-weather recipe {.val {spec@name}} is already registered."
        )
    }
    assign(spec@name, spec, envir = registry)
    invisible(spec)
}

# Populate the built-in catalog once while retaining any explicitly replaced
# process-local entries.
recipe__register_defaults <- function() {
    registered <- ls(
        envir = WEATHER_RECIPE_REGISTRY,
        all.names = FALSE
    )
    if (all(WEATHER_RECIPE_DEFAULTS %in% registered)) {
        return(invisible(NULL))
    }
    specs <- recipe__default_specs()
    for (name in names(specs)) {
        if (!exists(
            name,
            envir = WEATHER_RECIPE_REGISTRY,
            inherits = FALSE
        )) {
            recipe__register(specs[[name]])
        }
    }
    invisible(NULL)
}

# Retrieve one registered recipe and optionally enforce its persisted
# definition version.
recipe__get <- function(
    name,
    version = NULL,
    registry = WEATHER_RECIPE_REGISTRY
) {
    checkmate::assert_environment(registry)
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    if (identical(registry, WEATHER_RECIPE_REGISTRY)) {
        recipe__register_defaults()
    }
    name <- tolower(name)
    if (!exists(name, envir = registry, inherits = FALSE)) {
        cli::cli_abort(
            "Unknown future-weather recipe: {.val {name}}."
        )
    }
    spec <- get(name, envir = registry, inherits = FALSE)
    if (!is.null(version)) {
        checkmate::assert_count(version, positive = TRUE)
        if (!identical(spec@version, as.integer(version))) {
            cli::cli_abort(
                "Future-weather recipe {.val {name}} requires definition version {spec@version}; persisted version is {as.integer(version)}."
            )
        }
    }
    spec
}

# Convert one role requirement into a function-free record suitable for
# listings, JSON diagnostics, and user inspection.
recipe__requirement_record <- function(requirement) {
    list(
        role = requirement@role,
        representations = requirement@representations,
        frequencies = requirement@frequencies,
        variable_frequencies = requirement@variable_frequencies,
        calendars = requirement@calendars,
        variable_sets = requirement@variable_sets
    )
}

# Return inspectable catalog metadata without exposing backend runners or
# component functions.
recipe__list <- function(registry = WEATHER_RECIPE_REGISTRY) {
    checkmate::assert_environment(registry)
    if (identical(registry, WEATHER_RECIPE_REGISTRY)) {
        recipe__register_defaults()
    }
    names <- sort(ls(envir = registry, all.names = FALSE))
    if (!length(names)) {
        return(data.table::data.table(
            name = character(),
            version = integer(),
            label = character(),
            method = character(),
            backend = character(),
            implementation = character(),
            default_policy = character(),
            policies = list(),
            physical_policies = list(),
            calendar_policy = character(),
            target_calendar = character(),
            output_type = character(),
            stochastic = logical(),
            status = character(),
            source = list(),
            required_inputs = list(),
            optional_inputs = list(),
            components = list(),
            diagnostics = list(),
            provenance = list()
        ))
    }
    data.table::rbindlist(lapply(names, function(name) {
        spec <- get(name, envir = registry, inherits = FALSE)
        target_calendar <- recipe__target_calendar(spec)
        data.table::data.table(
            name = spec@name,
            version = spec@version,
            label = spec@label,
            method = spec@method,
            backend = spec@backend,
            implementation = spec@implementation,
            default_policy = spec@default_policy,
            policies = list(names(spec@policy_profiles)),
            physical_policies = list(spec@physical_policies),
            calendar_policy = spec@calendar_policy,
            target_calendar = if (length(target_calendar)) {
                target_calendar
            } else {
                NA_character_
            },
            output_type = spec@output_type,
            stochastic = spec@stochastic,
            status = spec@status,
            source = list(spec@source),
            required_inputs = list(lapply(
                spec@required_inputs,
                recipe__requirement_record
            )),
            optional_inputs = list(lapply(
                spec@optional_inputs,
                recipe__requirement_record
            )),
            components = list(spec@components),
            diagnostics = list(spec@diagnostics),
            provenance = list(spec@provenance)
        )
    }), use.names = TRUE, fill = TRUE)
}

# Resolve a registered method and its explicit execution policy without
# changing unregistered custom-backend recipe behavior.
recipe__resolve <- function(name, policy = NULL, version = NULL) {
    checkmate::assert_string(name, min.chars = 1L)
    recipe__register_defaults()
    key <- tolower(name)
    if (!exists(
        key,
        envir = WEATHER_RECIPE_REGISTRY,
        inherits = FALSE
    )) {
        return(NULL)
    }
    spec <- recipe__get(key, version = version)
    if (is.null(policy)) {
        policy <- spec@default_policy
    }
    checkmate::assert_choice(policy, names(spec@policy_profiles))
    list(
        spec = spec,
        policy = policy,
        profile = unname(spec@policy_profiles[[policy]])
    )
}

# Report the frequency intersection declared by complete recipe inputs rather
# than inferring it from backend names.
recipe__frequency_choices <- function(
    spec,
    roles = c("model_historical", "model_future")
) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort(
            "{.arg spec} must be a WeatherRecipeSpec object."
        )
    }
    checkmate::assert_subset(roles, WEATHER_INPUT_ROLES)
    requirements <- c(spec@required_inputs, spec@optional_inputs)
    choices <- lapply(
        intersect(roles, names(requirements)),
        function(role) requirements[[role]]@frequencies
    )
    choices <- Filter(length, choices)
    if (!length(choices)) {
        return(NULL)
    }
    allowed <- Reduce(intersect, choices)
    if (!length(allowed)) {
        cli::cli_abort(
            "Recipe {.val {spec@name}} declares incompatible climate frequencies."
        )
    }
    allowed
}

# Resolve the per-variable frequency contract shared by historical and future
# model roles while keeping scalar recipe frequencies backward compatible.
recipe__variable_frequencies <- function(
    spec,
    roles = c("model_historical", "model_future")
) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort(
            "{.arg spec} must be a WeatherRecipeSpec object."
        )
    }
    checkmate::assert_subset(roles, WEATHER_INPUT_ROLES)
    requirements <- c(spec@required_inputs, spec@optional_inputs)
    mappings <- lapply(
        intersect(roles, names(requirements)),
        function(role) requirements[[role]]@variable_frequencies
    )
    weather__combine_variable_frequencies(
        mappings,
        sprintf("Recipe %s", spec@name)
    )
}

# Return all role-level input failures before a registered recipe starts its
# backend or component implementation.
recipe__input_errors <- function(spec, inputs) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort(
            "{.arg spec} must be a WeatherRecipeSpec object."
        )
    }
    weather__input_requirement_errors(
        spec@required_inputs,
        spec@optional_inputs,
        inputs
    )
}

# Abort with the complete role diagnostics so queued and foreground execution
# enforce the same registered recipe contract.
recipe__validate_inputs <- function(spec, inputs) {
    errors <- recipe__input_errors(spec, inputs)
    if (length(errors)) {
        cli::cli_abort(c(
            "Future-weather recipe {.val {spec@name}} input requirements are not satisfied.",
            stats::setNames(errors, rep("x", length(errors)))
        ))
    }
    invisible(inputs)
}

# Return internal executable recipe records for registry validation.
#' @noRd
epw_morph_recipes <- function() {
    recipe__list()
}

# Return one internal complete-recipe contract by its registered key.
#' @noRd
epw_morph_recipe_spec <- function(name) {
    recipe__get(name)
}
