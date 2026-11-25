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

# Direct monolithic backends remain explicit adapters while componentized
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
# WeatherRecipeSpec {{{
WeatherRecipeSpec <- S7::new_class(
    "WeatherRecipeSpec",
    properties = list(
        name = S7::new_property(S7::class_character),
        version = S7::new_property(S7::class_integer),
        label = S7::new_property(S7::class_character),
        method = S7::new_property(S7::class_character),
        method_version = S7::new_property(S7::class_integer, default = 1L),
        signal_component = S7::new_property(S7::class_any, default = NULL),
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
        components = S7::new_property(S7::class_list, default = list()),
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
    # validator {{{
    validator = function(self) {
        if (
            length(self@name) != 1L ||
                is.na(self@name) ||
                !grepl("^[a-z][a-z0-9_]*$", self@name)
        ) {
            return("`name` must be one lower snake_case recipe identifier.")
        }
        for (property in c("version", "method_version")) {
            value <- S7::prop(self, property)
            if (length(value) != 1L || is.na(value) || value < 1L) {
                return(sprintf("`%s` must be one positive integer.", property))
            }
        }
        if (
            !is.null(self@signal_component) &&
                (!is.character(self@signal_component) ||
                    length(self@signal_component) != 1L ||
                    is.na(self@signal_component) ||
                    !grepl("^[a-z][a-z0-9_]*$", self@signal_component))
        ) {
            return(
                "`signal_component` must be NULL or one lower snake_case component name."
            )
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
            if (
                !checkmate::test_string(
                    S7::prop(self, property),
                    min.chars = 1L
                )
            ) {
                return(sprintf("`%s` must be one non-empty string.", property))
            }
        }
        if (
            length(self@implementation) != 1L ||
                is.na(self@implementation) ||
                !self@implementation %in% WEATHER_RECIPE_IMPLEMENTATIONS
        ) {
            return("`implementation` must be `backend` or `pipeline`.")
        }
        if (
            length(self@target_calendar) > 1L ||
                anyNA(self@target_calendar) ||
                any(!grepl("^[a-z][a-z0-9_]*$", self@target_calendar))
        ) {
            return(
                "`target_calendar` must be empty or one lower snake_case identifier."
            )
        }
        if (identical(self@implementation, "backend")) {
            if (length(self@components)) {
                return(
                    "`components` must be empty for `backend` implementations."
                )
            }
        } else {
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
        }
        if (
            !is.null(weather__check_named_list(
                self@policy_profiles,
                "policy_profiles",
                allow_empty = FALSE
            )) ||
                anyNA(self@policy_profiles) ||
                any(!nzchar(self@policy_profiles))
        ) {
            return(
                "`policy_profiles` must be a uniquely named, non-empty character vector."
            )
        }
        if (!all(names(self@policy_profiles) %in% WEATHER_RECIPE_POLICIES)) {
            return("`policy_profiles` contains an unknown execution policy.")
        }
        if (!self@default_policy %in% names(self@policy_profiles)) {
            return(
                "`default_policy` must identify one supported execution policy."
            )
        }
        if (
            !identical(
                names(self@physical_policies),
                names(self@policy_profiles)
            ) ||
                anyNA(self@physical_policies) ||
                any(!nzchar(self@physical_policies)) ||
                any(!self@physical_policies %in% names(EPW_PHYS_POLICY_SPECS))
        ) {
            return(
                "`physical_policies` must map every execution policy to one built-in EPW physical policy."
            )
        }
        if (!self@output_type %in% WEATHER_RECIPE_OUTPUT_TYPES) {
            return("`output_type` contains an unknown weather output type.")
        }
        if (!checkmate::test_flag(self@stochastic)) {
            return("`stochastic` must be one non-missing logical value.")
        }
        if (!self@status %in% WEATHER_RECIPE_STATUSES) {
            return("`status` contains an unknown recipe status.")
        }
        if (
            !is.null(weather__check_named_list(
                self@source,
                "source",
                allow_empty = FALSE
            )) ||
                !all(c("type", "citation") %in% names(self@source))
        ) {
            return(
                "`source` must be a named list containing `type` and `citation`."
            )
        }
        for (property in c("type", "citation")) {
            value <- self@source[[property]]
            if (
                !is.character(value) ||
                    !length(value) ||
                    anyNA(value) ||
                    any(!nzchar(value))
            ) {
                return(sprintf(
                    "`source$%s` must contain non-empty text.",
                    property
                ))
            }
        }
        err <- weather__check_role_requirements(
            self@required_inputs,
            self@optional_inputs
        )
        if (!is.null(err)) {
            return(err)
        }
        for (property in c("diagnostics", "provenance")) {
            err <- weather__check_unique_strings(
                S7::prop(self, property),
                property
            )
            if (!is.null(err)) {
                return(err)
            }
        }
        NULL
    }
    # }}}
)
# }}}

# Construct and normalize one serializable recipe specification before it
# enters the process-local registry.
# recipe__spec {{{
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
    components = list(),
    policy_profiles,
    physical_policies,
    default_policy,
    output_type = "representative_year",
    stochastic = FALSE,
    diagnostics = character(),
    provenance = character(),
    status = "experimental",
    version = 1L,
    method_version = 1L,
    signal_component = NULL
) {
    checkmate::assert_count(version, positive = TRUE)
    checkmate::assert_count(method_version, positive = TRUE)
    checkmate::assert_string(method, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_string(backend, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_string(
        signal_component,
        pattern = "^[a-z][a-z0-9_]*$",
        null.ok = TRUE
    )
    checkmate::assert_list(source, names = "unique")
    source <- lapply(source, function(value) {
        checkmate::assert_character(value, any.missing = FALSE)
        as.character(value)
    })
    required_inputs <- component__requirements(
        required_inputs,
        "required_inputs"
    )
    optional_inputs <- component__requirements(
        optional_inputs,
        "optional_inputs"
    )
    checkmate::assert_string(
        calendar_policy,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    if (identical(implementation, "pipeline")) {
        if (is.character(components) && !is.null(names(components))) {
            components <- as.list(components)
        }
        checkmate::assert_list(components, names = "unique")
        missing_stages <- setdiff(WEATHER_COMPONENT_STAGES, names(components))
        unknown_stages <- setdiff(names(components), WEATHER_COMPONENT_STAGES)
        if (length(missing_stages) || length(unknown_stages)) {
            cli::cli_abort(c(
                "A pipeline recipe must identify all seven future-weather stages.",
                "x" = if (length(missing_stages)) {
                    "Missing stage(s): {.val {missing_stages}}."
                },
                "x" = if (length(unknown_stages)) {
                    "Unknown stage(s): {.val {unknown_stages}}."
                }
            ))
        }
        components <- components[WEATHER_COMPONENT_STAGES]
    } else {
        checkmate::assert_list(components)
    }

    WeatherRecipeSpec(
        name = name,
        version = as.integer(version),
        label = label,
        method = method,
        method_version = as.integer(method_version),
        signal_component = signal_component,
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
        diagnostics = weather__descriptor_values(diagnostics, "diagnostics"),
        provenance = weather__descriptor_values(provenance, "provenance"),
        status = status
    )
}
# }}}

# Ensure all active signal and pipeline components referenced by built-in
# recipes are registered in the process-local component registry.
# recipe__register_components {{{
recipe__register_components <- function() {
    bias__register_linear_scaling_component()
    bias__register_delta_change_component()
    qm__register_component()
    qdm__register_component()
    kqdm__register_component()
    sdm__register_component()
    cdft__register_component()
    edcdf__register_component()
    isimip__register_component()
    daily__register_temperature_components()
    btws__register_hourly_component()
    daily_adjustment__register_components()
    hourly_kqdm__register_components()
    invisible(NULL)
}
# }}}

# Build a standard weather_template + climate-role requirement list concisely.
# recipe__role_inputs {{{
recipe__role_inputs <- function(
    frequencies,
    variable_sets,
    roles = c("model_historical", "model_future"),
    calendars = character(),
    variable_frequencies = list()
) {
    role_reqs <- stats::setNames(
        lapply(roles, function(role) {
            component__input_requirement(
                role,
                representations = "series",
                frequencies = frequencies,
                variable_frequencies = variable_frequencies,
                calendars = calendars,
                variable_sets = variable_sets
            )
        }),
        roles
    )
    c(list(weather_template = weather__epw_template_requirement()), role_reqs)
}
# }}}

# Declare the monthly Belcher climate inputs once. The publication-defined
# recipe requires temperature mean and average-daily-extrema changes, while
# the enhanced package method may fall back when extrema are unavailable.
# recipe__monthly_inputs {{{
recipe__monthly_inputs <- function(enhanced = FALSE) {
    checkmate::assert_flag(enhanced)
    common <- c("tas", "psl", "rlds", "rsds", "sfcWind", "clt", "pr")
    if (!enhanced) {
        common <- c("tas", "tasmax", "tasmin", setdiff(common, "tas"))
    }
    variables <- if (enhanced) {
        list(c(common, "hurs"), c(common, "huss", "ps"))
    } else {
        c(common, "hurs")
    }
    recipe__role_inputs("mon", variables)
}
# }}}

# Build complete temperature recipes from registered daily signal components.
# Only the signal component and backend identifier vary; the EPW adapter stays fixed.
# recipe__daily_adjustment_specs {{{
recipe__daily_adjustment_specs <- function() {
    inputs <- c(
        list(weather_template = weather__epw_template_requirement()),
        signal__three_role_requirements(
            "tas",
            frequencies = "day",
            calendars = CF_TIME_CALENDARS
        )
    )
    specs <- lapply(names(DAILY_ADJUSTMENT_METHOD_COMPONENTS), function(key) {
        method_name <- DAILY_ADJUSTMENT_METHOD_COMPONENTS[[key]]
        signal <- component__get("signal", method_name)
        profile <- signal@metadata$signal_profiles$tas
        pipeline <- daily_adjustment__pipeline(key)
        transform_evidence <- if (identical(profile$evidence, "experimental")) {
            "experimental"
        } else {
            "adapted_publication"
        }
        recipe__spec(
            name = paste0(method_name, "_temperature"),
            label = paste(signal@label, "temperature EPW"),
            method = method_name,
            signal_component = method_name,
            backend = DAILY_ADJUSTMENT_BACKENDS[[key]],
            implementation = "pipeline",
            source = list(
                type = transform_evidence,
                citation = signal@label,
                signal_evidence = profile$evidence,
                references = profile$references
            ),
            required_inputs = inputs,
            calendar_policy = "cf_annual_phase_365",
            components = pipeline__records(pipeline),
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(harmonized = "preserve_specific_humidity"),
            default_policy = "harmonized",
            output_type = "representative_year",
            stochastic = FALSE,
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
            status = "experimental",
            # Version 2 rejects plans created before per-plan observation isolation.
            version = 2L
        )
    })
    names(specs) <- vapply(specs, function(spec) spec@name, character(1L))
    specs
}
# }}}

# Build the complete built-in recipes without embedding backend runners or
# component functions in their definitions.
# recipe__default_specs {{{
recipe__default_specs <- function() {
    recipe__register_components()
    faithful_inputs <- recipe__monthly_inputs(enhanced = FALSE)
    enhanced_inputs <- recipe__monthly_inputs(enhanced = TRUE)
    daily_pipeline <- daily__temperature_pipeline()
    daily_btws_pipeline <- daily__temperature_pipeline("btws")
    hourly_kqdm_pipeline <- hourly_kqdm__pipeline()
    hourly_kqdm_inputs <- c(
        list(
            weather_template = weather__epw_template_requirement(),
            observed_reference = component__input_requirement(
                "observed_reference",
                representations = "series",
                frequencies = "hour",
                calendars = CF_TIME_CALENDARS,
                variable_sets = EPW_MORPH_HOURLY_KQDM_VARIABLES
            )
        ),
        recipe__role_inputs(
            frequencies = unique(unname(HOURLY_KQDM_MODEL_FREQUENCIES)),
            variable_sets = EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES,
            calendars = CF_TIME_CALENDARS,
            variable_frequencies = as.list(HOURLY_KQDM_MODEL_FREQUENCIES)
        )[c("model_historical", "model_future")]
    )

    builtins <- list(
        original_morphing_monthly = recipe__spec(
            name = "original_morphing_monthly",
            label = "Original monthly morphing",
            method = "original_morphing_monthly",
            method_version = 2L,
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
            policy_profiles = c(paper_faithful = "legacy"),
            physical_policies = c(
                paper_faithful = "legacy_independent_fields"
            ),
            default_policy = "paper_faithful",
            diagnostics = c("monthly_target_closure", "physical_bounds"),
            provenance = c(
                "source_method",
                "backend_profile",
                "input_periods"
            ),
            status = "production",
            version = 2L
        ),
        epwshiftr_monthly = recipe__spec(
            name = "epwshiftr_monthly",
            label = "Enhanced epwshiftr monthly morphing",
            method = "epwshiftr_monthly",
            method_version = 3L,
            backend = "original_morphing",
            implementation = "backend",
            source = list(
                type = "package_method",
                citation = "epwshiftr enhanced Belcher workflow",
                references = c(
                    "https://doi.org/10.1191/0143624405bt112oa",
                    "https://github.com/ideas-lab-nus/epwshiftr/pull/126"
                ),
                equation_note = paste(
                    "Invalid stretch factors use the recorded additive shift.",
                    "Neutral multipliers support cyclic interpolation next to",
                    "fallback months; fallback rows retain shift arithmetic.",
                    "Cyclic factors follow the representative month/day/hour",
                    "calendar independently of historical source-year labels."
                )
            ),
            required_inputs = enhanced_inputs[
                c("weather_template", "model_future")
            ],
            optional_inputs = enhanced_inputs["model_historical"],
            calendar_policy = "monthly_gregorian",
            target_calendar = "epw_365_day",
            policy_profiles = c(harmonized = "enhanced"),
            physical_policies = c(harmonized = "monthly_harmonized"),
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
                "physical_policies"
            ),
            status = "production",
            version = 3L
        ),
        epwshiftr_daily_power = recipe__spec(
            name = "epwshiftr_daily_power",
            label = "Daily power-constrained temperature projection",
            method = "daily_temperature_delta",
            signal_component = "daily_temperature_delta",
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
            required_inputs = recipe__role_inputs("day", "tas"),
            calendar_policy = "cf_annual_phase_365",
            components = pipeline__records(daily_pipeline),
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(harmonized = "preserve_specific_humidity"),
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
            signal_component = "daily_temperature_delta",
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
            required_inputs = recipe__role_inputs(
                "day",
                c("tas", "tasmin", "tasmax")
            ),
            calendar_policy = "cf_annual_phase_365",
            components = pipeline__records(daily_btws_pipeline),
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(harmonized = "preserve_specific_humidity"),
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
            implementation = "backend",
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
                    "If a CMIP6 change requests a mean outside the interval",
                    "attainable while preserving those endpoints, epwshiftr",
                    "uses the nearest attainable mean and records both targets,",
                    "the bounds, source means, and adjustment reason.",
                    "The unified physical layer closes humidity, diffuse and",
                    "direct radiation after all method candidates are formed."
                )
            ),
            required_inputs = recipe__role_inputs(
                "mon",
                c("tas", "tasmin", "tasmax", "rsds", "clt")
            ),
            calendar_policy = "cf_calendar_month_to_epw_365",
            target_calendar = "epw_365_day",
            policy_profiles = c(harmonized = "default"),
            physical_policies = c(harmonized = "bws_btws_weather"),
            default_policy = "harmonized",
            diagnostics = c(
                "monthly_temperature_radiation_cloud_changes",
                "daily_extrema_closure",
                "btws_scale_and_exponents",
                "bws_scale_and_exponents",
                "bws_target_adjustment",
                "mean_shift_fallback",
                "humidity_closure",
                "shortwave_closure",
                "day_boundary_jump"
            ),
            provenance = c(
                "source_method",
                "input_periods",
                "calendar_mapping",
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
            implementation = "backend",
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
            required_inputs = recipe__role_inputs("day", c("tasmin", "tasmax")),
            calendar_policy = "cf_yearly_linear_to_epw_365",
            target_calendar = "epw_365_day",
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
            implementation = "backend",
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
            required_inputs = recipe__role_inputs(
                "day",
                "tas",
                roles = SIGNAL_THREE_INPUT_ROLES
            ),
            calendar_policy = "native_calendar_month_distributions",
            target_calendar = "epw_365_day",
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
                "empirical_cdf_conventions",
                "smoothing",
                "physical_policy"
            ),
            status = "experimental",
            # Version 2 rejects plans created before per-plan observation isolation.
            version = 2L
        ),
        hourly_kernel_qdm = recipe__spec(
            name = "hourly_kernel_qdm",
            label = "Hourly kernel QDM multi-year future weather",
            method = "kernel_quantile_delta_mapping_hourly",
            signal_component = "kernel_quantile_delta_mapping_hourly",
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
            physical_policies = c(harmonized = "absolute_model_fields"),
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
            status = "experimental",
            # Version 2 rejects plans created before per-plan observation isolation.
            version = 2L
        ),
        sobie_curry_daily = recipe__spec(
            name = "sobie_curry_daily",
            label = "Sobie-Curry daily morphing",
            method = "sobie_curry_daily",
            backend = "sobie_curry_daily",
            implementation = "backend",
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
            required_inputs = recipe__role_inputs(
                "day",
                c("tas", "tasmin", "tasmax", "huss", "ps")
            ),
            calendar_policy = "cf_annual_phase_365",
            target_calendar = "epw_365_day",
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
                "equation_interpretation",
                "physical_policy"
            ),
            status = "experimental",
            version = 2L
        )
    )
    c(builtins, recipe__daily_adjustment_specs())
}
# }}}

# Resolve the output calendar declared by a recipe. Backend recipes state this
# contract directly, while componentized recipes inherit it from their output
# component unless an explicit recipe-level contract takes precedence.
# recipe__target_calendar {{{
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
    checkmate::assert_string(target_calendar, pattern = "^[a-z][a-z0-9_]*$")
    target_calendar
}
# }}}

# Verify that a catalog entry resolves to an available backend and, for a
# pipeline method, to the same executable component sequence as that backend.
# recipe__validate_registration {{{
recipe__validate_registration <- function(spec) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort("{.arg spec} must be a WeatherRecipeSpec object.")
    }
    if (!is.null(spec@signal_component)) {
        component__get("signal", spec@signal_component)
    }
    backend <- epw_morph_backend(spec@backend)
    allowed_profiles <- if (
        spec@backend %in% c("original_morphing", "original_morphing_absolute")
    ) {
        EPW_MORPH_ORIGINAL_PROFILES
    } else {
        "default"
    }
    invalid <- setdiff(unname(spec@policy_profiles), allowed_profiles)
    if (length(invalid)) {
        cli::cli_abort(
            "Recipe {.val {spec@name}} uses unsupported backend profile(s): {.val {invalid}}."
        )
    }
    pipeline <- backend$component_pipeline()
    if (identical(spec@implementation, "pipeline")) {
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
    } else if (!is.null(pipeline)) {
        cli::cli_abort(
            "Recipe {.val {spec@name}} uses implementation `backend` but backend {.val {spec@backend}} provides a component pipeline."
        )
    }
    invisible(spec)
}
# }}}

# Register one complete recipe while preventing accidental replacement of a
# stable method identifier.
# recipe__register {{{
recipe__register <- function(
    spec,
    registry = WEATHER_RECIPE_REGISTRY
) {
    checkmate::assert_environment(registry)
    recipe__validate_registration(spec)
    if (exists(spec@name, envir = registry, inherits = FALSE)) {
        cli::cli_abort(
            "Future-weather recipe {.val {spec@name}} is already registered."
        )
    }
    assign(spec@name, spec, envir = registry)
    invisible(spec)
}
# }}}

# Populate the built-in catalog once while retaining existing process-local entries.
# recipe__register_defaults {{{
recipe__register_defaults <- function() {
    registered <- ls(envir = WEATHER_RECIPE_REGISTRY, all.names = FALSE)
    if (all(WEATHER_RECIPE_DEFAULTS %in% registered)) {
        return(invisible(NULL))
    }
    specs <- recipe__default_specs()
    for (name in names(specs)) {
        if (!exists(name, envir = WEATHER_RECIPE_REGISTRY, inherits = FALSE)) {
            recipe__register(specs[[name]])
        }
    }
    invisible(NULL)
}
# }}}

# Retrieve one registered recipe and optionally enforce its persisted
# definition version.
# recipe__get {{{
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
    if (!exists(name, envir = registry, inherits = FALSE)) {
        cli::cli_abort("Unknown future-weather recipe: {.val {name}}.")
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
# }}}

# Return inspectable catalog metadata without exposing backend runners or
# component functions.
# recipe__list {{{
recipe__list <- function(registry = WEATHER_RECIPE_REGISTRY) {
    checkmate::assert_environment(registry)
    if (identical(registry, WEATHER_RECIPE_REGISTRY)) {
        recipe__register_defaults()
    }
    names <- sort(ls(envir = registry, all.names = FALSE))
    specs <- lapply(names, get, envir = registry, inherits = FALSE)
    target_calendars <- vapply(
        specs,
        function(spec) {
            cal <- recipe__target_calendar(spec)
            if (length(cal)) cal else NA_character_
        },
        character(1L)
    )
    data.table::data.table(
        name = vapply(specs, function(s) s@name, character(1L)),
        version = vapply(specs, function(s) s@version, integer(1L)),
        label = vapply(specs, function(s) s@label, character(1L)),
        method = vapply(specs, function(s) s@method, character(1L)),
        backend = vapply(specs, function(s) s@backend, character(1L)),
        implementation = vapply(
            specs,
            function(s) s@implementation,
            character(1L)
        ),
        default_policy = vapply(
            specs,
            function(s) s@default_policy,
            character(1L)
        ),
        policies = lapply(specs, function(s) names(s@policy_profiles)),
        physical_policies = lapply(specs, function(s) s@physical_policies),
        calendar_policy = vapply(
            specs,
            function(s) s@calendar_policy,
            character(1L)
        ),
        target_calendar = target_calendars,
        output_type = vapply(specs, function(s) s@output_type, character(1L)),
        stochastic = vapply(specs, function(s) s@stochastic, logical(1L)),
        status = vapply(specs, function(s) s@status, character(1L)),
        source = lapply(specs, function(s) s@source),
        required_inputs = lapply(specs, function(s) {
            lapply(s@required_inputs, component__input_requirement_record)
        }),
        optional_inputs = lapply(specs, function(s) {
            lapply(s@optional_inputs, component__input_requirement_record)
        }),
        components = lapply(specs, function(s) s@components),
        diagnostics = lapply(specs, function(s) s@diagnostics),
        provenance = lapply(specs, function(s) s@provenance)
    )
}
# }}}

# Resolve a registered method and its explicit execution policy without
# changing unregistered custom-backend recipe behavior.
# recipe__resolve {{{
recipe__resolve <- function(name, policy = NULL, version = NULL) {
    checkmate::assert_string(name, min.chars = 1L)
    recipe__register_defaults()
    key <- tolower(name)
    if (!exists(key, envir = WEATHER_RECIPE_REGISTRY, inherits = FALSE)) {
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
# }}}

# Report the frequency intersection declared by complete recipe inputs rather
# than inferring it from backend names.
# recipe__frequency_choices {{{
recipe__frequency_choices <- function(
    spec,
    roles = c("model_historical", "model_future")
) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort("{.arg spec} must be a WeatherRecipeSpec object.")
    }
    checkmate::assert_subset(roles, WEATHER_INPUT_ROLES)
    requirements <- c(spec@required_inputs, spec@optional_inputs)
    choices <- Filter(
        length,
        lapply(
            intersect(roles, names(requirements)),
            function(role) requirements[[role]]@frequencies
        )
    )
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
# }}}

# Resolve the per-variable frequency contract shared by historical and future
# model roles while keeping scalar recipe frequencies backward compatible.
# recipe__variable_frequencies {{{
recipe__variable_frequencies <- function(
    spec,
    roles = c("model_historical", "model_future")
) {
    if (!S7::S7_inherits(spec, WeatherRecipeSpec)) {
        cli::cli_abort("{.arg spec} must be a WeatherRecipeSpec object.")
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
# }}}

# Abort with the complete role diagnostics so queued and foreground execution
# enforce the same registered recipe contract.
# recipe__validate_inputs {{{
recipe__validate_inputs <- function(
    spec,
    inputs,
    required_inputs = spec@required_inputs,
    optional_inputs = spec@optional_inputs
) {
    errors <- weather__input_requirement_errors(
        required_inputs,
        optional_inputs,
        inputs
    )
    if (length(errors)) {
        cli::cli_abort(c(
            "Future-weather recipe {.val {spec@name}} input requirements are not satisfied.",
            stats::setNames(errors, rep("x", length(errors)))
        ))
    }
    invisible(inputs)
}
# }}}

# Return internal executable recipe records for registry validation.
#' @noRd
# epw_morph_recipes {{{
epw_morph_recipes <- function() {
    recipe__list()
}
# }}}

# vim: fdm=marker :
