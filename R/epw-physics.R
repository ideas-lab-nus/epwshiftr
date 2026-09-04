#' @include weather-solar.R
NULL

# The shared EPW physical layer separates method-defined target states from
# field derivation, closure, bounding, and diagnostics applied to those states.

EPW_PHYS_HUMIDITY_MODES <- c(
    "preserve_fields",
    "independent_fields",
    "preserve_specific_humidity",
    "specific_humidity_target",
    "absolute"
)
EPW_PHYS_WIND_MODES <- c("preserve_fields", "independent_fields", "absolute")
EPW_PHYS_SHORTWAVE_MODES <- c(
    "preserve_fields",
    "independent_fields",
    "absolute"
)
EPW_PHYS_MISSING_ACTIONS <- c("preserve", "error")

# Built-in policies describe physical treatment without embedding a method
# implementation or executable function in a persisted weather recipe.
EPW_PHYS_POLICY_SPECS <- list(
    legacy_independent_fields = list(
        humidity = "independent_fields",
        wind = "independent_fields",
        shortwave = "independent_fields",
        bounded_fields = character(),
        missing_action = "preserve",
        diagnose_inconsistency = TRUE
    ),
    monthly_harmonized = list(
        humidity = "independent_fields",
        wind = "independent_fields",
        shortwave = "independent_fields",
        bounded_fields = character(),
        missing_action = "preserve",
        diagnose_inconsistency = TRUE
    ),
    preserve_specific_humidity = list(
        humidity = "preserve_specific_humidity",
        wind = "preserve_fields",
        shortwave = "preserve_fields",
        bounded_fields = character(),
        missing_action = "preserve",
        diagnose_inconsistency = TRUE
    ),
    preserve_humidity_fields = list(
        humidity = "preserve_fields",
        wind = "preserve_fields",
        shortwave = "preserve_fields",
        bounded_fields = character(),
        missing_action = "preserve",
        diagnose_inconsistency = TRUE
    ),
    independent_thermodynamic_fields = list(
        humidity = "independent_fields",
        wind = "preserve_fields",
        shortwave = "preserve_fields",
        bounded_fields = character(),
        missing_action = "error",
        diagnose_inconsistency = TRUE
    ),
    specific_humidity_delta = list(
        humidity = "specific_humidity_target",
        wind = "preserve_fields",
        shortwave = "preserve_fields",
        bounded_fields = character(),
        missing_action = "error",
        diagnose_inconsistency = TRUE
    ),
    absolute_model_fields = list(
        humidity = "absolute",
        wind = "absolute",
        shortwave = "absolute",
        bounded_fields = c(
            "dry_bulb_temperature",
            "atmospheric_pressure",
            "horizontal_infrared_radiation_intensity_from_sky"
        ),
        missing_action = "error",
        diagnose_inconsistency = TRUE
    )
)

# Validate one data-only physical policy before it is passed to the shared
# executor by a method-specific adapter.
epwphys__policy_error <- function(self) {
    if (length(self@name) != 1L ||
        is.na(self@name) ||
        !self@name %in% names(EPW_PHYS_POLICY_SPECS)) {
        return("`name` must identify one built-in EPW physical policy.")
    }
    if (length(self@humidity) != 1L ||
        is.na(self@humidity) ||
        !self@humidity %in% EPW_PHYS_HUMIDITY_MODES) {
        return("`humidity` must identify one humidity treatment.")
    }
    if (length(self@wind) != 1L ||
        is.na(self@wind) ||
        !self@wind %in% EPW_PHYS_WIND_MODES) {
        return("`wind` must identify one wind treatment.")
    }
    if (length(self@shortwave) != 1L ||
        is.na(self@shortwave) ||
        !self@shortwave %in% EPW_PHYS_SHORTWAVE_MODES) {
        return("`shortwave` must identify one shortwave treatment.")
    }
    if (anyNA(self@bounded_fields) ||
        anyDuplicated(self@bounded_fields) ||
        any(!self@bounded_fields %in% names(EPW_FILE_FIELD_SPECS))) {
        return("`bounded_fields` must contain unique EPW fields with specifications.")
    }
    if (length(self@missing_action) != 1L ||
        is.na(self@missing_action) ||
        !self@missing_action %in% EPW_PHYS_MISSING_ACTIONS) {
        return("`missing_action` must be `preserve` or `error`.")
    }
    if (length(self@diagnose_inconsistency) != 1L ||
        is.na(self@diagnose_inconsistency)) {
        return("`diagnose_inconsistency` must be one non-missing logical value.")
    }
    NULL
}

# EpwPhysicalPolicy is the internal, serializable description of how a method's
# projected state is interpreted at the common EPW physical boundary.
EpwPhysicalPolicy <- S7::new_class(
    "EpwPhysicalPolicy",
    properties = list(
        name = S7::new_property(S7::class_character),
        humidity = S7::new_property(S7::class_character),
        wind = S7::new_property(S7::class_character),
        shortwave = S7::new_property(S7::class_character),
        bounded_fields = S7::new_property(
            S7::class_character,
            default = character()
        ),
        missing_action = S7::new_property(S7::class_character),
        diagnose_inconsistency = S7::new_property(S7::class_logical)
    ),
    validator = epwphys__policy_error
)

# Validate role-specific candidate vectors against the number of rows in the
# template without imposing one common statistical representation upstream.
epwphys__candidate_error <- function(values, allowed, rows, label) {
    if (!is.list(values) ||
        (length(values) &&
            (is.null(names(values)) ||
                any(!nzchar(names(values))) ||
                anyDuplicated(names(values))))) {
        return(sprintf("`%s` must be a uniquely named list.", label))
    }
    unknown <- setdiff(names(values), allowed)
    if (length(unknown)) {
        return(sprintf(
            "`%s` contains unsupported candidate(s): %s.",
            label,
            paste(unknown, collapse = ", ")
        ))
    }
    invalid <- names(values)[vapply(
        values,
        function(value) length(value) != rows,
        logical(1L)
    )]
    if (length(invalid)) {
        return(sprintf(
            "`%s` candidate(s) must contain %d values: %s.",
            label,
            rows,
            paste(invalid, collapse = ", ")
        ))
    }
    NULL
}

# Validate one method-neutral physical request. Candidate lists remain grouped
# by physical role so mutually exclusive humidity and wind paths stay explicit.
epwphys__request_error <- function(self) {
    if (!is.data.frame(self@template) || !nrow(self@template)) {
        return("`template` must be a non-empty weather data frame.")
    }
    rows <- nrow(self@template)
    checks <- list(
        epwphys__candidate_error(
            self@fields,
            EPW_FILE_COLUMNS,
            rows,
            "fields"
        ),
        epwphys__candidate_error(
            self@humidity,
            c("relative_humidity", "target_specific_humidity"),
            rows,
            "humidity"
        ),
        epwphys__candidate_error(
            self@wind,
            c("speed", "eastward", "northward"),
            rows,
            "wind"
        ),
        epwphys__candidate_error(
            self@shortwave,
            c("global_horizontal", "diffuse_horizontal"),
            rows,
            "shortwave"
        )
    )
    failed <- Filter(Negate(is.null), checks)
    if (length(failed)) {
        return(failed[[1L]])
    }
    if (!is.null(self@geometry) &&
        (!is.data.frame(self@geometry) || nrow(self@geometry) != rows)) {
        return("`geometry` must be NULL or match the weather row count.")
    }
    if (length(self@provenance) &&
        (is.null(names(self@provenance)) ||
            any(!nzchar(names(self@provenance))) ||
            anyDuplicated(names(self@provenance)))) {
        return("`provenance` must be a uniquely named list.")
    }
    NULL
}

# EpwPhysicalRequest carries a template plus only the candidate states produced
# by an upstream weather method; it never parses or writes an EPW file.
EpwPhysicalRequest <- S7::new_class(
    "EpwPhysicalRequest",
    properties = list(
        template = S7::new_property(S7::class_any),
        fields = S7::new_property(S7::class_list, default = list()),
        humidity = S7::new_property(S7::class_list, default = list()),
        wind = S7::new_property(S7::class_list, default = list()),
        shortwave = S7::new_property(S7::class_list, default = list()),
        geometry = S7::new_property(S7::class_any, default = NULL),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = epwphys__request_error
)

# Validate the shared executor result before method-specific adapters translate
# its state and corrections back to their existing result contracts.
epwphys__result_error <- function(self) {
    if (!is.data.frame(self@weather) || !nrow(self@weather)) {
        return("`weather` must be a non-empty weather data frame.")
    }
    if (!S7::S7_inherits(self@policy, EpwPhysicalPolicy)) {
        return("`policy` must be an EpwPhysicalPolicy object.")
    }
    for (property in c("state", "corrections", "provenance")) {
        value <- S7::prop(self, property)
        if (length(value) &&
            (is.null(names(value)) ||
                any(!nzchar(names(value))) ||
                anyDuplicated(names(value)))) {
            return(sprintf("`%s` must be a uniquely named list.", property))
        }
    }
    NULL
}

# EpwPhysicalResult retains the complete weather state together with generic
# derived quantities and correction counts used by existing diagnostics.
EpwPhysicalResult <- S7::new_class(
    "EpwPhysicalResult",
    properties = list(
        weather = S7::new_property(S7::class_any),
        policy = S7::new_property(S7::class_any),
        state = S7::new_property(S7::class_list, default = list()),
        corrections = S7::new_property(S7::class_list, default = list()),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = epwphys__result_error
)

# Construct one immutable policy from the package-owned behavioral catalog.
epwphys__policy <- function(name) {
    checkmate::assert_choice(name, names(EPW_PHYS_POLICY_SPECS))
    spec <- EPW_PHYS_POLICY_SPECS[[name]]
    EpwPhysicalPolicy(
        name = name,
        humidity = spec$humidity,
        wind = spec$wind,
        shortwave = spec$shortwave,
        bounded_fields = spec$bounded_fields,
        missing_action = spec$missing_action,
        diagnose_inconsistency = spec$diagnose_inconsistency
    )
}

# Resolve every built-in complete recipe to its physical behavior while leaving
# unregistered custom backends responsible for their own physical contract.
epwphys__recipe_policy <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    spec <- recipe$recipe_spec
    if (is.null(spec)) {
        if (recipe$backend %in% c("belcher", "belcher_absolute")) {
            return(epwphys__policy(if (
                identical(recipe$profile, "enhanced")
            ) {
                "monthly_harmonized"
            } else {
                "legacy_independent_fields"
            }))
        }
        return(NULL)
    }
    name <- switch(
        spec,
        belcher_monthly = "legacy_independent_fields",
        epwshiftr_monthly = "monthly_harmonized",
        epwshiftr_daily_power = "preserve_specific_humidity",
        epwshiftr_daily_btws = "preserve_specific_humidity",
        eames_monthly_temperature = "preserve_specific_humidity",
        ek_daily_factors = if (identical(recipe$policy, "harmonized")) {
            "preserve_specific_humidity"
        } else {
            "preserve_humidity_fields"
        },
        monthly_percentile_temperature = if (
            identical(recipe$policy, "harmonized")
        ) {
            "preserve_specific_humidity"
        } else {
            "preserve_humidity_fields"
        },
        sobie_curry_daily = if (identical(recipe$policy, "harmonized")) {
            "specific_humidity_delta"
        } else {
            "independent_thermodynamic_fields"
        },
        NULL
    )
    if (is.null(name)) {
        cli::cli_abort(
            "Registered recipe {.val {spec}} has no EPW physical policy."
        )
    }
    epwphys__policy(name)
}

# Evaluate the ASHRAE saturation-vapour-pressure correlation in logarithmic
# form. Separate ice and liquid-water coefficients meet at the triple point.
epwphys__psychro_ln_pws <- function(t_c) {
    t_k <- as.numeric(t_c) + 273.15
    ice <- t_k <= 273.16
    out <- numeric(length(t_k))
    out[ice] <- -5.6745359e3 / t_k[ice] +
        6.3925247 -
        9.677843e-3 * t_k[ice] +
        6.2215701e-7 * t_k[ice]^2 +
        2.0747825e-9 * t_k[ice]^3 -
        9.484024e-13 * t_k[ice]^4 +
        4.1635019 * log(t_k[ice])
    out[!ice] <- -5.8002206e3 / t_k[!ice] +
        1.3914993 -
        4.8640239e-2 * t_k[!ice] +
        4.1764768e-5 * t_k[!ice]^2 -
        1.4452093e-8 * t_k[!ice]^3 +
        6.5459673 * log(t_k[!ice])
    out
}

# Differentiate the ASHRAE saturation-pressure equation for the vectorised
# Newton iteration used by the dew-point inverse.
epwphys__psychro_d_ln_pws <- function(t_c) {
    t_k <- as.numeric(t_c) + 273.15
    ice <- t_k <= 273.16
    out <- numeric(length(t_k))
    out[ice] <- 5.6745359e3 / t_k[ice]^2 -
        9.677843e-3 +
        2 * 6.2215701e-7 * t_k[ice] +
        3 * 2.0747825e-9 * t_k[ice]^2 -
        4 * 9.484024e-13 * t_k[ice]^3 +
        4.1635019 / t_k[ice]
    out[!ice] <- 5.8002206e3 / t_k[!ice]^2 -
        4.8640239e-2 +
        2 * 4.1764768e-5 * t_k[!ice] -
        3 * 1.4452093e-8 * t_k[!ice]^2 +
        6.5459673 / t_k[!ice]
    out
}

# Derive relative humidity from specific humidity, air temperature in kelvin,
# and station pressure through the exact moist-air vapour-pressure relation.
epwphys__hurs_from_huss_si <- function(huss, tas, ps) {
    huss <- as.numeric(huss)
    tas <- as.numeric(tas)
    ps <- as.numeric(ps)
    valid <- is.na(huss) | (is.finite(huss) & huss >= 0 & huss < 1)
    valid <- valid &
        (is.na(tas) | (is.finite(tas) & tas >= 173.15 & tas <= 473.15))
    valid <- valid & (is.na(ps) | (is.finite(ps) & ps > 0))
    if (!all(valid)) {
        cli::cli_abort(
            paste(
                "Cannot derive hurs because huss, tas, or ps contains values",
                "outside the supported physical range."
            ),
            class = "epwshiftr_hurs_derivation_error"
        )
    }

    # Epsilon is the molecular-weight ratio of dry air to water vapour used by
    # ASHRAE psychrometric relations; sea-level pressure is invalid here.
    epsilon <- 0.621945
    vapour_pressure <- huss * ps / (epsilon + (1 - epsilon) * huss)
    saturation_pressure <- exp(epwphys__psychro_ln_pws(tas - 273.15))
    100 * vapour_pressure / saturation_pressure
}

# Convert dry-bulb temperature, relative humidity, and station pressure to
# specific humidity using the same exact moist-air relation.
epwphys__huss_from_rh_si <- function(t_c, rh, ps) {
    t_c <- as.numeric(t_c)
    rh <- pmin(100, pmax(0, as.numeric(rh))) / 100
    ps <- as.numeric(ps)
    saturation_pressure <- exp(epwphys__psychro_ln_pws(t_c))
    vapour_pressure <- pmin(ps * (1 - 1e-12), rh * saturation_pressure)
    epsilon <- 0.621945
    epsilon * vapour_pressure / (ps - (1 - epsilon) * vapour_pressure)
}

# Evaluate saturation specific humidity at the projected temperature and
# station pressure so harmonized policies can cap supersaturated targets.
epwphys__saturation_huss_si <- function(t_c, ps) {
    t_c <- as.numeric(t_c)
    ps <- as.numeric(ps)
    saturation_pressure <- pmin(
        ps * (1 - 1e-12),
        exp(epwphys__psychro_ln_pws(t_c))
    )
    epsilon <- 0.621945
    epsilon * saturation_pressure /
        (ps - (1 - epsilon) * saturation_pressure)
}

# Solve vapour pressure = RH * saturation pressure for dew point using a
# bounded Newton iteration over the ASHRAE correlation validity range.
epwphys__dew_point_from_rh <- function(t_c, rh) {
    t_c <- as.numeric(t_c)
    rh <- pmin(1, pmax(as.numeric(rh), .Machine$double.eps))
    target <- log(rh) + epwphys__psychro_ln_pws(t_c)
    dew <- pmin(t_c, 100)
    for (i in seq_len(20L)) {
        step <- (epwphys__psychro_ln_pws(dew) - target) /
            epwphys__psychro_d_ln_pws(dew)
        updated <- pmin(t_c, pmax(-100, pmin(200, dew - step)))
        if (all(abs(updated - dew) <= 1e-9)) {
            dew <- updated
            break
        }
        dew <- updated
    }
    dew
}

# Clip a finite vector with the package-wide EPW field specification and
# report how many values changed under the selected policy.
epwphys__bound_field <- function(value, field, upper = NULL) {
    spec <- EPW_FILE_FIELD_SPECS[[field]]
    if (is.null(spec)) {
        cli::cli_abort("EPW field {.val {field}} has no physical specification.")
    }
    lower <- spec[[1L]]
    if (is.null(upper)) {
        upper <- spec[[2L]]
    }
    value <- as.numeric(value)
    bounded <- pmin(upper, pmax(lower, value))
    list(
        value = bounded,
        clipped = as.integer(sum(
            abs(bounded - value) > sqrt(.Machine$double.eps),
            na.rm = TRUE
        ))
    )
}

# Close a method-defined specific-humidity target and retain the unclipped,
# saturation, closed, RH, dew-point, and status states for method diagnostics.
epwphys__close_specific_humidity <- function(
    temperature,
    pressure,
    target_specific_humidity,
    bound_dew_point = FALSE
) {
    checkmate::assert_flag(bound_dew_point)
    temperature <- as.numeric(temperature)
    pressure <- as.numeric(pressure)
    target <- as.numeric(target_specific_humidity)
    if (length(unique(c(
        length(temperature),
        length(pressure),
        length(target)
    ))) != 1L) {
        cli::cli_abort(
            "Temperature, pressure, and target specific humidity must have equal lengths."
        )
    }
    valid <- is.finite(temperature) &
        is.finite(pressure) & pressure > 0 &
        is.finite(target)
    saturation <- rep.int(NA_real_, length(target))
    specific <- rep.int(NA_real_, length(target))
    relative <- rep.int(NA_real_, length(target))
    dew <- rep.int(NA_real_, length(target))
    status <- rep.int("missing", length(target))
    if (any(valid)) {
        saturation[valid] <- epwphys__saturation_huss_si(
            temperature[valid],
            pressure[valid]
        )
        specific[valid] <- pmin(
            saturation[valid],
            pmax(0, target[valid])
        )
        status[valid] <- "ok"
        status[valid & target < 0] <- "zero_clipped"
        status[valid & target > saturation] <- "saturation_clipped"
        relative[valid] <- epwphys__hurs_from_huss_si(
            specific[valid],
            temperature[valid] + 273.15,
            pressure[valid]
        )
        relative[valid] <- pmin(100, pmax(0, relative[valid]))
        dew[valid] <- epwphys__dew_point_from_rh(
            temperature[valid],
            pmax(relative[valid], .Machine$double.eps) / 100
        )
        dew[valid] <- pmin(dew[valid], temperature[valid])
    }
    dew_raw <- dew
    dew_clipped <- 0L
    if (isTRUE(bound_dew_point)) {
        bounded <- epwphys__bound_field(dew, "dew_point_temperature")
        dew <- pmin(temperature, bounded$value)
        dew_clipped <- as.integer(sum(
            abs(dew - dew_raw) > 1e-9,
            na.rm = TRUE
        ))
    }
    list(
        relative_humidity = relative,
        dew_point_temperature = dew,
        target_specific_humidity = target,
        specific_humidity = specific,
        saturation_specific_humidity = saturation,
        status = status,
        zero_clipped = as.integer(sum(status == "zero_clipped")),
        saturation_clipped = as.integer(sum(
            status == "saturation_clipped"
        )),
        dew_point_clipped = dew_clipped
    )
}

# Preserve baseline specific humidity across a projected temperature/pressure
# state, leaving invalid baseline rows unchanged for legacy EPW compatibility.
epwphys__preserve_specific_humidity <- function(template, weather) {
    required <- c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature",
        "atmospheric_pressure"
    )
    missing <- setdiff(required, intersect(names(template), names(weather)))
    if (length(missing)) {
        cli::cli_abort(
            "Specific-humidity preservation requires EPW field(s): {.val {missing}}."
        )
    }
    baseline_temperature <- as.numeric(template[["dry_bulb_temperature"]])
    baseline_rh <- as.numeric(template[["relative_humidity"]])
    baseline_pressure <- as.numeric(template[["atmospheric_pressure"]])
    temperature <- as.numeric(weather[["dry_bulb_temperature"]])
    pressure <- as.numeric(weather[["atmospheric_pressure"]])
    valid <- is.finite(baseline_temperature) &
        is.finite(baseline_rh) & baseline_rh >= 0 & baseline_rh <= 100 &
        is.finite(baseline_pressure) & baseline_pressure > 0 &
        is.finite(temperature) &
        is.finite(pressure) & pressure > 0
    baseline_huss <- rep.int(NA_real_, nrow(template))
    baseline_huss[valid] <- epwphys__huss_from_rh_si(
        baseline_temperature[valid],
        baseline_rh[valid],
        baseline_pressure[valid]
    )
    closed <- epwphys__close_specific_humidity(
        temperature,
        pressure,
        baseline_huss
    )
    closed$baseline_specific_humidity <- baseline_huss
    closed$status[valid & closed$status == "ok"] <- "inherited"
    closed$status[!valid] <- "missing_baseline_state"
    closed
}

# Close a relative-humidity target against projected dry-bulb temperature and
# optionally apply the strict bounds used by absolute model fields.
epwphys__close_relative_humidity <- function(
    temperature,
    relative_humidity,
    clip_relative = FALSE,
    bound_dew_point = FALSE
) {
    checkmate::assert_flag(clip_relative)
    checkmate::assert_flag(bound_dew_point)
    temperature <- as.numeric(temperature)
    relative_raw <- as.numeric(relative_humidity)
    relative <- if (isTRUE(clip_relative)) {
        epwphys__bound_field(
            relative_raw,
            "relative_humidity",
            upper = 100
        )
    } else {
        list(value = relative_raw, clipped = 0L)
    }
    dew_raw <- epwphys__dew_point_from_rh(
        temperature,
        pmax(relative$value, .Machine$double.eps) / 100
    )
    dew <- pmin(dew_raw, temperature)
    if (isTRUE(bound_dew_point)) {
        bounded <- epwphys__bound_field(dew, "dew_point_temperature")
        dew <- pmin(temperature, bounded$value)
    }
    list(
        relative_humidity = relative$value,
        dew_point_temperature = dew,
        relative_humidity_clipped = relative$clipped,
        dew_point_clipped = as.integer(sum(
            abs(dew - dew_raw) > 1e-9,
            na.rm = TRUE
        ))
    )
}

# Enforce the EPW shortwave identity jointly. Optional night-time zeroing is
# selected by policy because legacy Belcher behavior must remain reproducible.
epwphys__close_shortwave <- function(
    global_horizontal,
    diffuse_horizontal,
    geometry,
    zero_night = FALSE
) {
    checkmate::assert_flag(zero_night)
    global_raw <- as.numeric(global_horizontal)
    diffuse_raw <- as.numeric(diffuse_horizontal)
    global <- pmax(0, global_raw)
    diffuse <- pmax(0, diffuse_raw)
    projection <- as.numeric(geometry[["effective_solar_projection"]])
    ext_direct <- pmax(
        0,
        as.numeric(geometry[["extraterrestrial_direct_normal_radiation"]])
    )
    daylight <- is.finite(projection) & projection > 1e-8
    night_changed <- 0L
    if (isTRUE(zero_night)) {
        night_changed <- as.integer(sum(
            !daylight & (global > 0 | diffuse > 0),
            na.rm = TRUE
        ))
        global[!daylight] <- 0
        diffuse[!daylight] <- 0
    }
    diffuse_above_global <- daylight & diffuse > global
    constrained_diffuse <- pmin(global, diffuse)

    # Beam energy is limited before deriving DNI so the horizontal identity
    # remains exact after the extraterrestrial-direct constraint is applied.
    beam_horizontal <- pmax(0, global - constrained_diffuse)
    max_beam_horizontal <- projection * ext_direct
    beam_horizontal <- pmin(beam_horizontal, max_beam_horizontal)
    no_projection <- !is.finite(projection) |
        projection <= .Machine$double.eps
    beam_horizontal[no_projection] <- 0
    closed_diffuse <- global - beam_horizontal
    direct <- ifelse(
        no_projection,
        0,
        beam_horizontal / projection
    )
    direct <- pmin(ext_direct, pmax(0, direct))
    closure_error <- global - (
        closed_diffuse + direct * pmax(projection, 0)
    )
    list(
        ghi = global,
        dhi = closed_diffuse,
        dni = direct,
        night_values_zeroed = night_changed,
        negative_global_clipped = as.integer(sum(global_raw < 0, na.rm = TRUE)),
        negative_diffuse_clipped = as.integer(sum(diffuse_raw < 0, na.rm = TRUE)),
        diffuse_above_global_clipped = as.integer(sum(
            diffuse_above_global,
            na.rm = TRUE
        )),
        excess_beam_reallocated = as.integer(sum(
            closed_diffuse - constrained_diffuse > 1e-9,
            na.rm = TRUE
        )),
        maximum_closure_error = max(abs(closure_error), na.rm = TRUE)
    )
}

# Overlay method-provided EPW-native fields while preserving the template's
# row order, column types, and undeclared weather fields.
epwphys__overlay_fields <- function(template, fields) {
    weather <- data.table::as.data.table(data.table::copy(template))
    for (field in names(fields)) {
        data.table::set(weather, j = field, value = fields[[field]])
    }
    weather
}

# Identify row-level humidity and pressure inconsistencies without correcting a
# paper-faithful result. Retaining the masks lets method adapters combine the
# shared checks with method-specific diagnostics without repeating equations.
epwphys__inconsistency_state <- function(weather) {
    rows <- nrow(weather)
    required <- c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature"
    )
    humidity <- rep.int(FALSE, rows)
    if (all(required %in% names(weather))) {
        humidity <- weather[["relative_humidity"]] < 0 |
            weather[["relative_humidity"]] > 100 |
            weather[["dew_point_temperature"]] >
                weather[["dry_bulb_temperature"]]
        humidity[is.na(humidity)] <- TRUE
    }

    pressure <- rep.int(FALSE, rows)
    if ("atmospheric_pressure" %in% names(weather)) {
        pressure <- weather[["atmospheric_pressure"]] <= 0
        pressure[is.na(pressure)] <- TRUE
    }

    list(
        humidity = humidity,
        pressure = pressure,
        thermodynamic = humidity | pressure
    )
}

# Count humidity inconsistencies through the shared row-level diagnostic so
# existing callers retain the established scalar correction contract.
epwphys__humidity_inconsistent <- function(weather) {
    state <- epwphys__inconsistency_state(weather)
    as.integer(sum(state$humidity))
}

# Apply one validated policy to a method-neutral request. Method adapters own
# statistical transforms; this executor owns only EPW physical interpretation.
epwphys__apply <- function(request, policy) {
    if (!S7::S7_inherits(request, EpwPhysicalRequest)) {
        cli::cli_abort("`request` must be an EpwPhysicalRequest object.")
    }
    if (!S7::S7_inherits(policy, EpwPhysicalPolicy)) {
        cli::cli_abort("`policy` must be an EpwPhysicalPolicy object.")
    }
    weather <- epwphys__overlay_fields(request@template, request@fields)
    corrections <- list(
        temperature_clipped = 0L,
        pressure_clipped = 0L,
        humidity_saturation_clipped = 0L,
        specific_humidity_clipped = 0L,
        dew_point_clipped = 0L,
        wind_speed_clipped = 0L,
        radiation_night_values_zeroed = 0L,
        radiation_negative_global_clipped = 0L,
        radiation_negative_diffuse_clipped = 0L,
        radiation_diffuse_above_global_clipped = 0L,
        radiation_excess_beam_reallocated = 0L,
        radiation_maximum_closure_error = 0,
        infrared_negative_clipped = 0L,
        humidity_inconsistent = 0L
    )
    for (field in intersect(policy@bounded_fields, names(request@fields))) {
        bounded <- epwphys__bound_field(weather[[field]], field)
        data.table::set(weather, j = field, value = bounded$value)
        correction <- switch(
            field,
            dry_bulb_temperature = "temperature_clipped",
            atmospheric_pressure = "pressure_clipped",
            horizontal_infrared_radiation_intensity_from_sky =
                "infrared_negative_clipped",
            NULL
        )
        if (!is.null(correction)) {
            corrections[[correction]] <- bounded$clipped
        }
    }

    state <- list()
    if (identical(policy@humidity, "preserve_specific_humidity")) {
        humidity <- epwphys__preserve_specific_humidity(
            request@template,
            weather
        )
        valid <- humidity$status != "missing_baseline_state"
        data.table::set(
            weather,
            i = which(valid),
            j = "relative_humidity",
            value = humidity$relative_humidity[valid]
        )
        data.table::set(
            weather,
            i = which(valid),
            j = "dew_point_temperature",
            value = humidity$dew_point_temperature[valid]
        )
        corrections$humidity_saturation_clipped <-
            humidity$saturation_clipped
        corrections$specific_humidity_clipped <-
            humidity$saturation_clipped
        state$humidity <- humidity
    } else if (identical(policy@humidity, "specific_humidity_target")) {
        target <- request@humidity$target_specific_humidity
        humidity <- epwphys__close_specific_humidity(
            weather[["dry_bulb_temperature"]],
            weather[["atmospheric_pressure"]],
            target
        )
        if (identical(policy@missing_action, "error") &&
            any(humidity$status == "missing")) {
            cli::cli_abort(
                "Specific-humidity closure requires finite temperature, pressure, and target values."
            )
        }
        data.table::set(
            weather,
            j = "relative_humidity",
            value = humidity$relative_humidity
        )
        data.table::set(
            weather,
            j = "dew_point_temperature",
            value = humidity$dew_point_temperature
        )
        corrections$humidity_saturation_clipped <-
            humidity$saturation_clipped
        corrections$specific_humidity_clipped <-
            humidity$zero_clipped + humidity$saturation_clipped
        state$humidity <- humidity
    } else if (identical(policy@humidity, "absolute")) {
        sources <- intersect(
            c("relative_humidity", "target_specific_humidity"),
            names(request@humidity)
        )
        if (length(sources) != 1L) {
            cli::cli_abort(
                "Absolute EPW physics requires exactly one humidity source."
            )
        }
        if (identical(sources, "relative_humidity")) {
            humidity <- epwphys__close_relative_humidity(
                weather[["dry_bulb_temperature"]],
                request@humidity$relative_humidity,
                clip_relative = TRUE,
                bound_dew_point = TRUE
            )
            corrections$humidity_saturation_clipped <-
                humidity$relative_humidity_clipped
        } else {
            target <- as.numeric(request@humidity$target_specific_humidity)
            if (any(!is.finite(target) | target < 0 | target >= 1)) {
                cli::cli_abort(
                    "Mapped `huss` must stay in the physical interval [0, 1)."
                )
            }
            humidity <- epwphys__close_specific_humidity(
                weather[["dry_bulb_temperature"]],
                weather[["atmospheric_pressure"]],
                target,
                bound_dew_point = TRUE
            )
            corrections$humidity_saturation_clipped <-
                humidity$saturation_clipped
            corrections$specific_humidity_clipped <-
                humidity$saturation_clipped
        }
        data.table::set(
            weather,
            j = "relative_humidity",
            value = humidity$relative_humidity
        )
        data.table::set(
            weather,
            j = "dew_point_temperature",
            value = humidity$dew_point_temperature
        )
        corrections$dew_point_clipped <- humidity$dew_point_clipped
        state$humidity <- humidity
    }

    if (identical(policy@wind, "absolute")) {
        has_speed <- identical(names(request@wind), "speed")
        has_vector <- setequal(names(request@wind), c("eastward", "northward"))
        if (identical(has_speed, has_vector)) {
            cli::cli_abort(
                "Absolute EPW physics requires scalar speed or paired vector wind."
            )
        }
        if (has_speed) {
            speed_raw <- request@wind$speed
            direction <- as.numeric(request@template[["wind_direction"]])
            direction_policy <- "inherit_epw_template"
        } else {
            eastward <- as.numeric(request@wind$eastward)
            northward <- as.numeric(request@wind$northward)
            speed_raw <- sqrt(eastward^2 + northward^2)
            direction <- (atan2(-eastward, -northward) * 180 / pi) %% 360
            direction[speed_raw <= sqrt(.Machine$double.eps)] <- 0
            direction_policy <- "derive_from_uas_vas"
        }
        speed <- epwphys__bound_field(speed_raw, "wind_speed")
        data.table::set(weather, j = "wind_speed", value = speed$value)
        data.table::set(weather, j = "wind_direction", value = direction)
        corrections$wind_speed_clipped <- speed$clipped
        state$wind <- list(
            speed = speed$value,
            direction = direction,
            direction_policy = direction_policy
        )
    }

    if (identical(policy@shortwave, "absolute")) {
        if (!setequal(
            names(request@shortwave),
            c("global_horizontal", "diffuse_horizontal")
        ) || is.null(request@geometry)) {
            cli::cli_abort(
                "Absolute EPW physics requires GHI, DHI, and solar geometry."
            )
        }
        radiation <- epwphys__close_shortwave(
            request@shortwave$global_horizontal,
            request@shortwave$diffuse_horizontal,
            request@geometry,
            zero_night = TRUE
        )
        data.table::set(
            weather,
            j = "global_horizontal_radiation",
            value = radiation$ghi
        )
        data.table::set(
            weather,
            j = "diffuse_horizontal_radiation",
            value = radiation$dhi
        )
        data.table::set(
            weather,
            j = "direct_normal_radiation",
            value = radiation$dni
        )
        corrections$radiation_night_values_zeroed <-
            radiation$night_values_zeroed
        corrections$radiation_negative_global_clipped <-
            radiation$negative_global_clipped
        corrections$radiation_negative_diffuse_clipped <-
            radiation$negative_diffuse_clipped
        corrections$radiation_diffuse_above_global_clipped <-
            radiation$diffuse_above_global_clipped
        corrections$radiation_excess_beam_reallocated <-
            radiation$excess_beam_reallocated
        corrections$radiation_maximum_closure_error <-
            radiation$maximum_closure_error
        state$radiation <- radiation
    }

    if (isTRUE(policy@diagnose_inconsistency)) {
        inconsistency <- epwphys__inconsistency_state(weather)
        corrections$humidity_inconsistent <-
            as.integer(sum(inconsistency$humidity))
        state$inconsistency <- inconsistency
    }
    EpwPhysicalResult(
        weather = weather[],
        policy = policy,
        state = state,
        corrections = corrections,
        provenance = c(
            list(physical_policy = policy@name),
            request@provenance
        )
    )
}

# Apply one physical policy independently to every complete weather case while
# preserving the original cross-case row order used by Belcher backends.
epwphys__apply_groups <- function(
    weather,
    policy,
    group_columns = character(),
    expected_rows = NULL
) {
    weather <- data.table::as.data.table(data.table::copy(weather))
    checkmate::assert_subset(group_columns, names(weather))
    checkmate::assert_count(expected_rows, positive = TRUE, null.ok = TRUE)
    data.table::set(weather, j = ".epwphys_order", value = seq_len(nrow(weather)))
    indices <- if (length(group_columns)) {
        # Use an explicit character-column selection so package checks do not
        # depend on data.table's `..` lookup in this internal adapter.
        interaction(
            lapply(weather[, group_columns, with = FALSE], function(value) {
                value <- as.character(value)
                value[is.na(value)] <- "<NA>"
                value
            }),
            drop = TRUE,
            lex.order = TRUE
        )
    } else {
        factor(rep.int("case", nrow(weather)))
    }
    rows <- split(seq_len(nrow(weather)), indices)
    results <- lapply(rows, function(index) {
        if (!is.null(expected_rows) && length(index) != expected_rows) {
            cli::cli_abort(
                "EPW physical case contains {length(index)} rows; expected {expected_rows}."
            )
        }
        epwphys__apply(
            EpwPhysicalRequest(template = weather[index]),
            policy
        )
    })
    output <- data.table::rbindlist(lapply(results, function(result) {
        result@weather
    }), use.names = TRUE, fill = TRUE)
    data.table::setorderv(output, ".epwphys_order")
    data.table::set(output, j = ".epwphys_order", value = NULL)
    list(weather = output[], results = results)
}

# Shared EPW location and interval-solar helpers {{{
morpher__epw_location_numeric <- function(epw, names, default = NA_real_) {
    loc <- tryCatch(epw$location(), error = function(e) NULL)
    if (is.null(loc)) {
        return(default)
    }
    for (name in names) {
        if (name %in% names(loc)) {
            value <- suppressWarnings(as.numeric(loc[[name]][[1L]]))
            if (length(value) && !is.na(value) && is.finite(value)) {
                return(value)
            }
        }
    }
    default
}

solar__epw_interval_geometry <- function(data, latitude, longitude, timezone,
                                          solar_constant = 1367) {
    n <- nrow(data)
    if (!n) {
        return(data.table::data.table())
    }
    latitude <- as.numeric(latitude)
    longitude <- as.numeric(longitude)
    timezone <- as.numeric(timezone)
    if (!is.finite(latitude) || !is.finite(longitude) || !is.finite(timezone)) {
        cli::cli_abort("EPW LOCATION must provide finite latitude, longitude, and time zone for enhanced solar geometry.")
    }

    minute_midpoint <- (seq_len(60L) - 0.5) / 60
    clock_hour <- outer(as.numeric(data$hour) - 1, rep(1, 60L)) +
        matrix(rep(minute_midpoint, each = n), nrow = n)
    day_of_year <- as.integer(format(
        as.Date(sprintf(
            "%04d-%02d-%02d",
            as.integer(data$year), as.integer(data$month), as.integer(data$day)
        )),
        "%j"
    ))
    day_matrix <- matrix(rep(day_of_year, 60L), nrow = n)

    # Spencer's Fourier series supplies Earth-Sun distance, declination, and
    # equation of time at every minute midpoint without external dependencies.
    gamma <- 2 * pi / 365 * (day_matrix - 1 + (clock_hour - 12) / 24)
    eccentricity <- 1.000110 +
        0.034221 * cos(gamma) +
        0.001280 * sin(gamma) +
        0.000719 * cos(2 * gamma) +
        0.000077 * sin(2 * gamma)
    declination <- solar__spencer_declination(gamma)
    equation_of_time <- solar__spencer_equation_of_time(gamma)
    apparent_solar_minutes <- clock_hour * 60 +
        4 * (longitude - 15 * timezone) + equation_of_time
    hour_angle <- solar__radians(apparent_solar_minutes / 4 - 180)
    latitude_radian <- solar__radians(latitude)
    cos_zenith <- solar__cos_zenith(
        latitude_radian,
        declination,
        hour_angle
    )
    daylight <- cos_zenith > 0
    extraterrestrial_direct <- solar_constant * eccentricity

    horizontal <- rowSums(extraterrestrial_direct * pmax(cos_zenith, 0)) / 60
    direct_normal <- rowSums(extraterrestrial_direct * daylight) / 60
    projection <- ifelse(direct_normal > .Machine$double.eps,
        horizontal / direct_normal, 0)
    apparent_hour <- rowMeans(apparent_solar_minutes) / 60
    apparent_hour <- apparent_hour %% 24
    data.table::data.table(
        extraterrestrial_horizontal_radiation = pmax(0, horizontal),
        extraterrestrial_direct_normal_radiation = pmax(0, direct_normal),
        effective_solar_projection = pmin(1, pmax(0, projection)),
        solar_zenith = acos(pmin(1, pmax(-1, projection))),
        solar_altitude = asin(pmin(1, pmax(-1, projection))) * 180 / pi,
        apparent_solar_time = apparent_hour
    )
}

# Compute relative optical air mass using the Kasten expression used with the
# Perez daylight parameterization; values at and below the horizon are absent.
solar__relative_air_mass <- function(zenith_radian) {
    zenith_degree <- as.numeric(zenith_radian) * 180 / pi
    out <- rep(NA_real_, length(zenith_degree))
    daylight <- is.finite(zenith_degree) & zenith_degree < 90
    out[daylight] <- 1 / (
        cos(zenith_radian[daylight]) +
            0.15 * (93.885 - zenith_degree[daylight])^(-1.253)
    )
    out
}
# }}}
