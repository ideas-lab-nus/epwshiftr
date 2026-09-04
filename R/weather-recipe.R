#' @include weather-pipeline.R
NULL

# Complete-recipe policies distinguish reproduction of a published method from
# comparisons that share epwshiftr's physical closure and output controls.
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

# Comparison status is metadata, not a claim that every registered recipe is
# recommended for production use.
WEATHER_RECIPE_STATUSES <- c(
    "production",
    "comparison",
    "experimental"
)

# Built-in keys allow idempotent registration to return without rebuilding
# component and input specifications on every recipe construction.
WEATHER_RECIPE_DEFAULTS <- c(
    "belcher_monthly",
    "epwshiftr_monthly",
    "epwshiftr_daily_power",
    "epwshiftr_daily_btws",
    "eames_monthly_temperature",
    "ek_daily_factors",
    "monthly_percentile_temperature",
    "sobie_curry_daily"
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
        components = S7::new_property(S7::class_list),
        policy_profiles = S7::new_property(S7::class_character),
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
        if (!self@output_type %in% WEATHER_RECIPE_OUTPUT_TYPES) {
            return("`output_type` contains an unknown weather output type.")
        }
        if (length(self@stochastic) != 1L || is.na(self@stochastic)) {
            return("`stochastic` must be one non-missing logical value.")
        }
        if (!self@status %in% WEATHER_RECIPE_STATUSES) {
            return("`status` contains an unknown comparison status.")
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
    backend,
    implementation,
    source,
    required_inputs,
    optional_inputs = list(),
    calendar_policy,
    components,
    policy_profiles,
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
        backend = backend,
        implementation = implementation,
        source = source,
        required_inputs = required_inputs,
        optional_inputs = optional_inputs,
        calendar_policy = calendar_policy,
        components = components,
        policy_profiles = policy_profiles,
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
            "enhanced_belcher_change_factors"
        } else {
            "belcher_change_factors"
        },
        sequence = "preserve_epw_sequence",
        hourly = if (enhanced) {
            "enhanced_belcher_transform"
        } else {
            "belcher_monthly_transform"
        },
        physics = if (enhanced) {
            "enhanced_weather_closure"
        } else {
            "legacy_epw_field_closure"
        },
        output = "epw_result"
    )
}

# Declare the monthly Belcher climate inputs once so the paper-faithful and
# enhanced catalog entries differ only where their reference policy requires.
recipe__monthly_inputs <- function(enhanced = FALSE) {
    checkmate::assert_flag(enhanced)
    common <- c(
        "tas", "psl", "rlds", "rsds", "sfcWind", "clt", "pr"
    )
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

# Build the complete built-in recipes without embedding backend runners or
# component functions in their definitions.
recipe__default_specs <- function() {
    faithful_inputs <- recipe__monthly_inputs(enhanced = FALSE)
    enhanced_inputs <- recipe__monthly_inputs(enhanced = TRUE)
    daily_pipeline <- daily__temperature_pipeline()
    btws_pipeline <- daily__temperature_pipeline("btws")
    eames_pipeline <- eames__monthly_temperature_pipeline()
    ek_pipeline <- ek__pipeline()
    arima_pipeline <- arima__pipeline()
    sobie_pipeline <- sobie__pipeline()
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
    btws_inputs <- list(
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
    eames_inputs <- eames__monthly_temperature_inputs()
    ek_inputs <- ek__daily_temperature_inputs()
    arima_inputs <- arima__temperature_inputs()
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

    list(
        belcher_monthly = recipe__spec(
            name = "belcher_monthly",
            label = "Belcher monthly morphing",
            backend = "belcher",
            implementation = "backend",
            source = list(
                type = "publication",
                citation = paste(
                    "Belcher, Hacker, and Powell (2005),",
                    "Constructing design weather data for future climates"
                ),
                references = "https://doi.org/10.1191/0143624405bt112oa"
            ),
            required_inputs = faithful_inputs,
            calendar_policy = "monthly_gregorian",
            components = recipe__monthly_components(),
            policy_profiles = c(paper_faithful = "legacy"),
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
            status = "comparison"
        ),
        epwshiftr_monthly = recipe__spec(
            name = "epwshiftr_monthly",
            label = "Enhanced epwshiftr monthly morphing",
            backend = "belcher",
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
            components = recipe__monthly_components(enhanced = TRUE),
            policy_profiles = c(harmonized = "enhanced"),
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
            backend = "daily_temperature",
            implementation = "pipeline",
            source = list(
                type = "combined_prior_methods",
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
            backend = "daily_temperature_btws",
            implementation = "pipeline",
            source = list(
                type = "combined_prior_methods",
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
            required_inputs = btws_inputs,
            calendar_policy = "cf_annual_phase_365",
            components = pipeline__records(btws_pipeline),
            policy_profiles = c(harmonized = "default"),
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
            status = "comparison"
        ),
        eames_monthly_temperature = recipe__spec(
            name = "eames_monthly_temperature",
            label = "Eames monthly temperature with BTWS projection",
            backend = "eames_monthly_temperature",
            implementation = "pipeline",
            source = list(
                type = "adapted_publication",
                citation = paste(
                    "Eames et al. (2024) monthly temperature changes and",
                    "bounded temperature weighted stretch, with the monthly",
                    "statistics derived from daily CMIP6 inputs"
                ),
                references = c(
                    "https://doi.org/10.1177/01436244231218861",
                    "https://github.com/ideas-lab-nus/epwshiftr/issues/152"
                ),
                equation_note = paste(
                    "The published monthly mean, average daily minimum, and",
                    "average daily maximum changes feed equations (7)-(16).",
                    "Where the paper does not publish solver code, epwshiftr",
                    "uses deterministic bisection to retain the largest",
                    "admissible m or n in [0, 1]."
                ),
                signal_note = paste(
                    "The paper obtains monthly factors from UKCP18.",
                    "epwshiftr instead aggregates matching daily CMIP6 tas,",
                    "tasmin, and tasmax to the same three monthly statistics;",
                    "it does not apply daily-varying change factors."
                ),
                implementation_note = paste(
                    "This temperature-only comparison reuses epwshiftr's",
                    "specific-humidity closure and EPW output policy.",
                    "The paper's non-temperature transformations are not",
                    "implemented by this recipe."
                )
            ),
            required_inputs = eames_inputs,
            calendar_policy = "cf_calendar_month_to_epw_365",
            components = pipeline__records(eames_pipeline),
            policy_profiles = c(harmonized = "default"),
            default_policy = "harmonized",
            diagnostics = c(
                "monthly_temperature_changes",
                "daily_extrema_closure",
                "btws_scale_and_exponents",
                "mean_shift_fallback",
                "humidity_closure",
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
            status = "comparison"
        ),
        ek_daily_factors = recipe__spec(
            name = "ek_daily_factors",
            label = "Ek daily temperature change factors",
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
            components = pipeline__records(ek_pipeline),
            policy_profiles = c(
                paper_faithful = "default",
                harmonized = "default"
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
            status = "comparison"
        ),
        monthly_percentile_temperature = recipe__spec(
            name = "monthly_percentile_temperature",
            label = "Monthly percentile-dependent temperature change",
            backend = "arima_temperature",
            implementation = "pipeline",
            source = list(
                type = "publication",
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
            required_inputs = arima_inputs,
            calendar_policy = "native_calendar_month_distributions",
            components = pipeline__records(arima_pipeline),
            policy_profiles = c(
                paper_faithful = "default",
                harmonized = "default"
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
            status = "comparison"
        ),
        sobie_curry_daily = recipe__spec(
            name = "sobie_curry_daily",
            label = "Sobie-Curry daily morphing",
            backend = "sobie_curry_daily",
            implementation = "pipeline",
            source = list(
                type = "publication",
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
            components = pipeline__records(sobie_pipeline),
            policy_profiles = c(
                paper_faithful = "default",
                harmonized = "default"
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
            status = "comparison",
            version = 2L
        )
    )
}

# Verify that a catalog entry resolves to an available backend and, for a
# pipeline method, to the same executable component sequence as that backend.
recipe__validate_registration <- function(spec) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort(
            "{.arg spec} must be a WeatherRecipeSpec object."
        )
    }
    backend <- epw_morph_backend(spec@backend)
    profiles <- unname(spec@policy_profiles)
    if (spec@backend %in% c("belcher", "belcher_absolute")) {
        invalid <- setdiff(profiles, EPW_MORPH_BELCHER_PROFILES)
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
            backend = character(),
            implementation = character(),
            default_policy = character(),
            policies = list(),
            calendar_policy = character(),
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
        data.table::data.table(
            name = spec@name,
            version = spec@version,
            label = spec@label,
            backend = spec@backend,
            implementation = spec@implementation,
            default_policy = spec@default_policy,
            policies = list(names(spec@policy_profiles)),
            calendar_policy = spec@calendar_policy,
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

#' Inspect registered future-weather recipes
#'
#' `epw_morph_recipes()` lists complete built-in future-weather methods rather
#' than low-level statistical backends. The returned metadata includes source,
#' input-role, calendar, component, execution-policy, output, diagnostic, and
#' provenance declarations without executing the method.
#'
#' @return A data table with one row per registered complete recipe. Structured
#'   metadata is retained in list columns.
#'
#' @seealso [epw_morph_recipe_spec()], [epw_morph_recipe()],
#'   [epw_morph_backends()]
#' @export
epw_morph_recipes <- function() {
    recipe__list()
}

#' Get a registered future-weather recipe specification
#'
#' @param name Stable complete-recipe name returned by
#'   [epw_morph_recipes()].
#'
#' @return A `WeatherRecipeSpec` object containing only inspectable,
#'   serializable method metadata.
#'
#' @seealso [epw_morph_recipes()], [epw_morph_recipe()]
#' @export
epw_morph_recipe_spec <- function(name) {
    recipe__get(name)
}
