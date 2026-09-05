#' @include backend-registry.R epw-morph-context.R epw-physics.R weather-solar.R
NULL

# Belcher backends {{{

EPW_MORPH_BELCHER_METHOD_DEFAULTS <- c(
    tdb = "stretch",
    rh = "stretch",
    p = "stretch",
    hor_ir = "stretch",
    glob_rad = "stretch",
    wind = "stretch"
)

EPW_MORPH_BELCHER_CHANGE_FACTOR_METHOD_DEFAULTS <- c(
    tdb = "shift",
    rh = "shift",
    p = "shift",
    hor_ir = "stretch",
    glob_rad = "stretch",
    wind = "stretch"
)

EPW_MORPH_BELCHER_METHOD_CHOICES <- c("shift", "stretch", "combined")

# Profiles make the numerical compatibility boundary explicit. The legacy
# defaults reproduce the historical calculation path, while enhanced enables
# the guarded temperature method and the standards-based post-process.
EPW_MORPH_BELCHER_PROFILES <- c("enhanced", "legacy")
EPW_MORPH_BELCHER_PROFILE_METHODS <- list(
    enhanced = utils::modifyList(
        as.list(EPW_MORPH_BELCHER_CHANGE_FACTOR_METHOD_DEFAULTS),
        list(tdb = "auto")
    ),
    legacy = as.list(EPW_MORPH_BELCHER_CHANGE_FACTOR_METHOD_DEFAULTS)
)
EPW_MORPH_BELCHER_ABSOLUTE_PROFILE_METHODS <- list(
    enhanced = utils::modifyList(
        as.list(EPW_MORPH_BELCHER_METHOD_DEFAULTS),
        list(tdb = "auto")
    ),
    legacy = as.list(EPW_MORPH_BELCHER_METHOD_DEFAULTS)
)

# Each profile owns a complete option set so recipe JSON never depends on
# process-global defaults when a queued or resumed task is reconstructed.
EPW_MORPH_BELCHER_PROFILE_OPTIONS <- list(
    enhanced = list(
        transition_hours = 72L,
        humidity_source = "auto",
        diffuse_model = "rbl_2010",
        illuminance_model = "perez_1990",
        snow_depth = "auto",
        ground_temperatures = "recalculate",
        typical_extreme_periods = "recalculate",
        design_conditions = "drop"
    ),
    legacy = list(
        transition_hours = 0L,
        humidity_source = "hurs",
        diffuse_model = "preserve_fraction",
        illuminance_model = "preserve",
        snow_depth = "off",
        ground_temperatures = "preserve",
        typical_extreme_periods = "preserve",
        design_conditions = "preserve"
    )
)

EPW_MORPH_BELCHER_OPTION_CHOICES <- list(
    humidity_source = c("auto", "huss", "hurs"),
    diffuse_model = c("rbl_2010", "preserve_fraction"),
    illuminance_model = c("perez_1990", "preserve"),
    snow_depth = c("auto", "required", "off"),
    ground_temperatures = c("recalculate", "preserve"),
    typical_extreme_periods = c("recalculate", "preserve"),
    design_conditions = c("drop", "preserve")
)

EPW_MORPH_BELCHER_RULES <- data.table::data.table(
    step = c(
        "tdb",
        "rh",
        "p",
        "hor_ir",
        "glob_rad",
        "wind",
        "total_cover",
        "precip",
        "tdew",
        "diff_rad",
        "norm_rad",
        "opaque_cover",
        "precip_rate"
    ),
    epw_field = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "atmospheric_pressure",
        "horizontal_infrared_radiation_intensity_from_sky",
        "global_horizontal_radiation",
        "wind_speed",
        "total_sky_cover",
        "liquid_precip_depth",
        "dew_point_temperature",
        "diffuse_horizontal_radiation",
        "direct_normal_radiation",
        "opaque_sky_cover",
        "liquid_precip_rate"
    ),
    variable_id = c("tas", "hurs", "psl", "rlds", "rsds", "sfcWind", "clt", "pr", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    optional_variable_id = c("tasmax,tasmin", "hursmax,hursmin", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    method = c(EPW_MORPH_BELCHER_METHOD_DEFAULTS, "sky_cover", "precipitation", "derived", "derived", "derived", "derived", "derived"),
    required = c(rep(TRUE, 8L), rep(FALSE, 5L)),
    derived = c(rep(FALSE, 8L), rep(TRUE, 5L))
)

# Temperature alone accepts the automatic combined-to-shift fallback. Other
# user-selectable fields retain the three original Belcher methods.
EPW_MORPH_BELCHER_RULES[, method_choices := lapply(step, function(step_name) {
    if (identical(step_name, "tdb")) {
        c("auto", EPW_MORPH_BELCHER_METHOD_CHOICES)
    } else if (step_name %in% names(EPW_MORPH_BELCHER_CHANGE_FACTOR_METHOD_DEFAULTS)) {
        EPW_MORPH_BELCHER_METHOD_CHOICES
    } else {
        method[step == step_name]
    }
})]

# Snow depth is an optional state variable rather than a required atmospheric
# input. Recipe options decide whether it is queried, required, or disabled.
EPW_MORPH_BELCHER_RULES <- data.table::rbindlist(list(
    EPW_MORPH_BELCHER_RULES[seq_len(8L)],
    data.table::data.table(
        step = "snow_depth",
        epw_field = "snow_depth",
        variable_id = "snd",
        optional_variable_id = NA_character_,
        method = "ratio",
        required = FALSE,
        derived = FALSE,
        method_choices = list("ratio")
    ),
    EPW_MORPH_BELCHER_RULES[-seq_len(8L)]
), use.names = TRUE, fill = TRUE)

# Validate one complete Belcher option list before it enters a recipe. Keeping
# this check at construction time prevents workers from interpreting malformed
# task JSON differently after a resume.
morpher__belcher_validate_options <- function(options) {
    if (!is.list(options) || is.null(names(options)) || any(!nzchar(names(options)))) {
        cli::cli_abort("Belcher `options` must be a named list or the result of {.fn belcher_options}.")
    }
    unknown <- setdiff(names(options), names(EPW_MORPH_BELCHER_PROFILE_OPTIONS$enhanced))
    if (length(unknown)) {
        cli::cli_abort("Unknown Belcher option(s): {.val {unknown}}.")
    }
    transition_hours <- options$transition_hours
    checkmate::assert_count(transition_hours, na.ok = FALSE)
    if (transition_hours > 336L) {
        cli::cli_abort("`transition_hours` must be between 0 and 336.")
    }
    options$transition_hours <- as.integer(transition_hours)
    for (name in names(EPW_MORPH_BELCHER_OPTION_CHOICES)) {
        value <- options[[name]]
        checkmate::assert_string(value, min.chars = 1L)
        value <- tolower(value)
        allowed <- EPW_MORPH_BELCHER_OPTION_CHOICES[[name]]
        if (!value %in% allowed) {
            cli::cli_abort(
                "Unsupported Belcher option value {.val {value}} for {.field {name}}. Allowed value(s): {.val {allowed}}."
            )
        }
        options[[name]] <- value
    }
    class(options) <- unique(c("belcher_options", class(options)))
    options
}

# Resolve partial user options against the selected profile. This function is
# also the single compatibility boundary used when old serialized recipes are
# reconstructed explicitly with `profile = "legacy"`.
morpher__belcher_resolve_options <- function(profile, options = NULL) {
    defaults <- EPW_MORPH_BELCHER_PROFILE_OPTIONS[[profile]]
    if (is.null(options)) {
        return(morpher__belcher_validate_options(defaults))
    }
    if (!is.list(options)) {
        cli::cli_abort("Belcher `options` must be a named list or the result of {.fn belcher_options}.")
    }
    unknown <- setdiff(names(options), names(defaults))
    if (length(unknown)) {
        cli::cli_abort("Unknown Belcher option(s): {.val {unknown}}.")
    }
    morpher__belcher_validate_options(utils::modifyList(defaults, unclass(options)))
}

# Resolve the profile-specific method baseline independently of the backend's
# registry default so legacy recipes retain their historical methods.
morpher__belcher_profile_methods <- function(backend, profile) {
    methods <- if (identical(backend$name, "belcher_absolute")) {
        EPW_MORPH_BELCHER_ABSOLUTE_PROFILE_METHODS[[profile]]
    } else {
        EPW_MORPH_BELCHER_PROFILE_METHODS[[profile]]
    }
    unlist(methods, use.names = TRUE)
}

#' Configure enhanced Belcher morphing
#'
#' @param transition_hours Total width in hours of each cyclic transition
#'   centered on a month boundary. Must be between 0 and 336; `0` disables
#'   smoothing.
#' @param humidity_source Humidity state input: `"auto"`, `"huss"`, or
#'   `"hurs"`.
#' @param diffuse_model Diffuse-radiation model: `"rbl_2010"` or
#'   `"preserve_fraction"`.
#' @param illuminance_model Illuminance model: `"perez_1990"` or `"preserve"`.
#' @param snow_depth Snow-depth policy: `"auto"`, `"required"`, or `"off"`.
#' @param ground_temperatures Ground-temperature header policy:
#'   `"recalculate"` applies the Kusuda--Achenbach model to the morphed year,
#'   while `"preserve"` retains the baseline EPW header.
#' @param typical_extreme_periods Typical/extreme-period header policy:
#'   `"recalculate"` finds six hemisphere-aware periods from the morphed year,
#'   while `"preserve"` retains the baseline EPW header.
#' @param design_conditions Design-condition header policy. `"drop"` writes
#'   `DESIGN CONDITIONS,0` because one morphed year cannot support a new ASHRAE
#'   design-condition calculation; `"preserve"` retains the baseline header.
#'
#' @return A validated `belcher_options` list.
#'
#' @references
#' Ridley B, Boland J, Lauret P (2010), "Modelling of diffuse solar fraction
#' with multiple predictors", *Renewable Energy*.
#' \doi{10.1016/j.renene.2009.07.018}
#'
#' Perez R, Ineichen P, Seals R, Michalsky J, Stewart R (1990), "Modeling
#' daylight availability and irradiance components from direct and global
#' irradiance", *Solar Energy*.
#'
#' EnergyPlus Weather File Data Dictionary:
#' <https://bigladdersoftware.com/epx/docs/22-2/auxiliary-programs/energyplus-weather-file-epw-data-dictionary.html>
#' @export
belcher_options <- function(
    transition_hours = 72L,
    humidity_source = "auto",
    diffuse_model = "rbl_2010",
    illuminance_model = "perez_1990",
    snow_depth = "auto",
    ground_temperatures = "recalculate",
    typical_extreme_periods = "recalculate",
    design_conditions = "drop"
) {
    morpher__belcher_validate_options(list(
        transition_hours = transition_hours,
        humidity_source = humidity_source,
        diffuse_model = diffuse_model,
        illuminance_model = illuminance_model,
        snow_depth = snow_depth,
        ground_temperatures = ground_temperatures,
        typical_extreme_periods = typical_extreme_periods,
        design_conditions = design_conditions
    ))
}

morpher__belcher_monthly_variable <- function(context, variable_id) {
    data <- morpher__context_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    year_labels <- morpher__context_year_labels(context)
    morpher__monthly_climate(
        data,
        years = year_labels$years,
        labels = year_labels$labels,
        warning = context$warning
    )
}

morpher__belcher_monthly_reference_variable <- function(context, variable_id) {
    data <- morpher__context_reference_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    morpher__monthly_climate(
        data,
        years = context$reference_years,
        labels = context$reference_labels,
        warning = context$warning
    )
}

morpher__belcher_epw_monthly <- function(data_epw, var, keep_units = TRUE) {
    monthly <- data_epw[,
        list(val_mean = mean(get(var)), val_max = max(get(var)), val_min = min(get(var))),
        by = "month"
    ]

    monthly
}

# Compute the EPW diurnal range from daily extrema, not from the single most
# extreme hours in a month. This is the denominator used by enhanced combined
# temperature morphing and is intentionally independent of CMIP sampling.
morpher__belcher_epw_monthly_dtr <- function(data_epw, var) {
    values <- morpher__drop_units(data_epw[[var]])
    daily <- data.table::data.table(
        year = as.integer(data_epw$year),
        month = as.integer(data_epw$month),
        day = as.integer(data_epw$day),
        value = as.numeric(values)
    )[, .(
        daily_max = if (all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
        daily_min = if (all(is.na(value))) NA_real_ else min(value, na.rm = TRUE)
    ), by = .(year, month, day)]
    monthly <- daily[, .(
        val_daily_max = if (all(is.na(daily_max))) NA_real_ else mean(daily_max, na.rm = TRUE),
        val_daily_min = if (all(is.na(daily_min))) NA_real_ else mean(daily_min, na.rm = TRUE)
    ), by = "month"]
    monthly[, val_dtr := val_daily_max - val_daily_min]
    mean_monthly <- data.table::data.table(
        month = as.integer(data_epw$month),
        value = as.numeric(values)
    )[, .(
        val_mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE)
    ), by = "month"]
    mean_monthly[monthly, on = "month", `:=`(
        val_daily_max = i.val_daily_max,
        val_daily_min = i.val_daily_min,
        val_dtr = i.val_dtr
    )][]
}

# Convert climate values from their declared source units into the explicit EPW
# field unit before monthly morphing factors are calculated.
morpher__belcher_align_units <- function(data, target_units) {
    converted <- lapply(seq_len(nrow(data)), function(i) {
        morpher__convert_value_checked(data$value[[i]], data$units[[i]], target_units)
    })
    ok <- vapply(converted, `[[`, logical(1L), "ok")
    if (any(!ok)) {
        messages <- unique(vapply(converted[!ok], `[[`, character(1L), "message"))
        cli::cli_abort(c(
            "Climate values cannot be converted to the EPW field unit {.val {target_units}}.",
            "x" = messages
        ))
    }
    data.table::set(data, NULL, "value", vapply(converted, `[[`, numeric(1L), "value"))
    data[, units := target_units]
    data
}

morpher__belcher_drop_units <- function(data, vars) {
    for (var in c(vars, "delta", "alpha")) {
        if (var %in% names(data)) {
            data.table::set(data, NULL, var, as.numeric(data[[var]]))
        }
    }
    data
}

morpher__belcher_day_angle <- function(day_of_year) {
    2.0 * pi * (day_of_year - 1.0) / 365.0
}

morpher__belcher_equation_of_time <- function(day_of_year) {
    d <- morpher__belcher_day_angle(day_of_year)
    (-7.659 * sin(d) + 9.863 * sin(2.0 * d + 3.5932)) / 60.0
}

morpher__belcher_solar_time <- function(longitude, day_of_year, hour, timezone) {
    local_standard_time <- (hour - 0.5) %% 24.0
    local_standard_time + (longitude - timezone * 15.0) / 15.0 +
        morpher__belcher_equation_of_time(day_of_year)
}

morpher__belcher_hour_angle <- function(longitude, day_of_year, hour, timezone) {
    solar_time <- morpher__belcher_solar_time(longitude, day_of_year, hour, timezone)
    360 / 24 * (solar_time - 12)
}

morpher__belcher_declination <- function(day_of_year) {
    d <- morpher__belcher_day_angle(day_of_year)
    solar__spencer_declination(d)
}

morpher__belcher_solar_angle <- function(latitude, longitude, day_of_year, hour, timezone) {
    declination <- morpher__belcher_declination(day_of_year)
    hour_angle <- morpher__belcher_hour_angle(longitude, day_of_year, hour, timezone)
    solar__cos_zenith(
        solar__radians(latitude),
        declination,
        solar__radians(hour_angle)
    )
}

# Check completeness at the month/case level. The selector is deliberately
# case-wide: future and historical periods may not switch humidity source in
# individual months.
morpher__humidity_variable_complete <- function(context, variable_id,
                                                  reference = FALSE) {
    data <- if (isTRUE(reference)) {
        morpher__belcher_monthly_reference_variable(context, variable_id)
    } else {
        morpher__belcher_monthly_variable(context, variable_id)
    }
    if (!nrow(data) || !all(1:12 %in% unique(data$month))) {
        return(FALSE)
    }
    values <- as.numeric(data$value)
    all(is.finite(values))
}

# Select one humidity source for the complete case. Enhanced auto mode prefers
# HUSS only when huss, tas, and ps are complete in both future and reference;
# non-shift RH methods stay on HURS because they explicitly override that path.
morpher__belcher_humidity_source <- function(context) {
    source <- context$recipe$options$humidity_source
    if (!identical(context$recipe$profile, "enhanced")) {
        return("hurs")
    }
    if (identical(source, "auto") && !identical(context$recipe$methods[["rh"]], "shift")) {
        source <- "hurs"
    }
    has_reference <- !is.null(context$reference_climate)
    complete <- function(variable_id) {
        morpher__humidity_variable_complete(context, variable_id) &&
            (!has_reference || morpher__humidity_variable_complete(
                context, variable_id, reference = TRUE
            ))
    }
    huss_complete <- all(vapply(c("huss", "tas", "ps"), complete, logical(1L)))
    hurs_complete <- complete("hurs")
    if (identical(source, "huss")) {
        if (!huss_complete) {
            cli::cli_abort(
                "Belcher humidity_source = 'huss' requires complete huss + tas + ps data for both future and reference periods.",
                class = "epwshiftr_huss_required_error"
            )
        }
        return("huss")
    }
    if (identical(source, "hurs")) {
        return("hurs")
    }
    if (huss_complete) {
        return("huss")
    }
    if (hurs_complete) {
        return("hurs")
    }
    if (isTRUE(context$strict)) {
        cli::cli_abort(
            "Enhanced Belcher humidity requires either complete huss + tas + ps or complete hurs data."
        )
    }
    "hurs"
}

# Normalize monthly HUSS summaries to kg/kg before calculating a state change.
morpher__belcher_monthly_huss <- function(context, reference = FALSE) {
    data <- if (isTRUE(reference)) {
        morpher__belcher_monthly_reference_variable(context, "huss")
    } else {
        morpher__belcher_monthly_variable(context, "huss")
    }
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    data <- data.table::copy(data)
    data[, value := morpher__humidity_input_si(value, units, "huss")]
    data[, units := "kg/kg"]
    data[]
}

# Apply the monthly HUSS state change to baseline EPW specific humidity, smooth
# it cyclically, cap at saturation, and invert the future state to RH and dew
# point using morphed temperature and station pressure.
morpher__belcher_huss_state <- function(data_epw, context, tdb, pressure) {
    if (!nrow(tdb)) {
        return(list(rh = data.table::data.table(), tdew = data.table::data.table()))
    }
    future <- morpher__belcher_monthly_huss(context)
    if (!nrow(future)) {
        return(list(rh = data.table::data.table(), tdew = data.table::data.table()))
    }
    external_reference <- !is.null(context$reference_climate)
    if (external_reference) {
        reference <- morpher__belcher_monthly_huss(context, reference = TRUE)
        future <- morpher__belcher_attach_reference(future, reference, "reference_value")
        future <- morpher__belcher_handle_missing_reference(
            future, "huss", strict = context$strict
        )
        future[, huss_target := as.numeric(value - reference_value)]
    } else {
        future[, huss_target := as.numeric(value)]
    }

    hourly <- data.table::copy(tdb)
    baseline <- data_epw[, .SD, .SDcols = c(
        "datetime", "year", "month", "day", "hour", "minute",
        "dry_bulb_temperature", "relative_humidity", "atmospheric_pressure"
    )]
    hourly[baseline, on = c("datetime", "year", "month", "day", "hour", "minute"), `:=`(
        baseline_tdb = i.dry_bulb_temperature,
        baseline_rh = i.relative_humidity,
        baseline_pressure = i.atmospheric_pressure
    )]
    hourly[, future_pressure := NA_real_]
    if (nrow(pressure)) {
        pressure_join <- intersect(
            c("source_id", "experiment_id", "member_id", "interval",
              "datetime", "year", "month", "day", "hour", "minute"),
            intersect(names(hourly), names(pressure))
        )
        hourly[pressure, on = pressure_join,
            future_pressure := i.atmospheric_pressure]
    }
    hourly[is.na(future_pressure), future_pressure := baseline_pressure]
    hourly[, baseline_huss := epwphys__huss_from_rh_si(
        baseline_tdb, baseline_rh, baseline_pressure
    )]

    join_cols <- intersect(
        c("activity_drs", "institution_id", "source_id", "experiment_id",
          "member_id", "interval", "month"),
        intersect(names(hourly), names(future))
    )
    hourly[future, on = join_cols, climate_huss_target := i.huss_target]
    case_cols <- morpher__factor_case_columns(hourly)
    hourly[, .humidity_order := .I]
    groups <- if (length(case_cols)) unique(hourly[, ..case_cols]) else data.table::data.table(.case = 1L)
    pieces <- vector("list", nrow(groups))
    for (i in seq_len(nrow(groups))) {
        rows <- if (length(case_cols)) {
            keep <- rep(TRUE, nrow(hourly))
            for (name in case_cols) {
                value <- groups[[name]][[i]]
                keep <- keep & if (is.na(value)) is.na(hourly[[name]]) else hourly[[name]] == value
            }
            hourly[keep]
        } else {
            data.table::copy(hourly)
        }
        data.table::setorder(rows, datetime)
        if (external_reference) {
            delta_target <- morpher__monthly_target_vector(rows, "climate_huss_target")
        } else {
            baseline_monthly <- rows[, .(
                baseline_huss_mean = mean(baseline_huss, na.rm = TRUE),
                target = climate_huss_target[[1L]]
            ), by = "month"]
            delta_target <- rep(NA_real_, 12L)
            delta_target[baseline_monthly$month] <-
                baseline_monthly$target - baseline_monthly$baseline_huss_mean
        }
        delta <- morpher__constrained_month_series(
            rows$month, delta_target,
            context$recipe$options$transition_hours
        )
        rows[, huss_delta := delta]
        pieces[[i]] <- rows
    }
    hourly <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
    data.table::setorder(hourly, .humidity_order)
    hourly[, .humidity_order := NULL]

    hourly[, huss_unclipped := baseline_huss + huss_delta]
    humidity <- epwphys__close_specific_humidity(
        hourly[["dry_bulb_temperature"]],
        hourly[["future_pressure"]],
        hourly[["huss_unclipped"]]
    )
    hourly[, saturation_huss := humidity$saturation_specific_humidity]
    hourly[, future_huss := humidity$specific_humidity]
    hourly[, relative_humidity := humidity$relative_humidity]
    hourly[, dew_point_temperature := humidity$dew_point_temperature]
    hourly[, `:=`(
        delta = huss_delta,
        alpha = NA_real_,
        factor_status = data.table::fifelse(
            humidity$status == "missing",
            NA_character_,
            data.table::fifelse(
                humidity$status == "ok",
                "ok",
                "saturation_clipped"
            )
        )
    )]

    identity <- c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval", "datetime", "year", "month",
        "day", "hour", "minute"
    )
    rh_keep <- intersect(c(identity, "relative_humidity", "delta", "alpha", "factor_status"), names(hourly))
    dew_keep <- intersect(c(identity, "dew_point_temperature", "delta", "alpha", "factor_status"), names(hourly))
    list(
        rh = hourly[, .SD, .SDcols = rh_keep],
        tdew = hourly[, .SD, .SDcols = dew_keep]
    )
}

# Normalize the narrowly supported CF units needed by the hurs derivation.
# Rejecting unknown units is safer than silently treating scaled humidity or
# pressure as SI input.
morpher__belcher_tdew <- function(tdb, rh) {
    # Join only on scientific case identity and EPW time. Enhanced factor
    # diagnostics legitimately differ between temperature and humidity and
    # must not become accidental equality keys.
    join_cols <- intersect(
        c("activity_drs", "institution_id", "source_id", "experiment_id",
          "member_id", "lon", "lat", "interval", "datetime", "year",
          "month", "day", "hour", "minute"),
        intersect(names(tdb), names(rh))
    )
    if (!"datetime" %in% join_cols) {
        cli::cli_abort("Cannot derive dew point without a shared datetime column.")
    }
    tdew <- data.table::copy(tdb)[
        rh, on = join_cols,
        relative_humidity := i.relative_humidity
    ]

    tdew[!is.na(dry_bulb_temperature) & !is.na(relative_humidity),
        dew_point_temperature := epwphys__dew_point_from_rh(
            dry_bulb_temperature,
            relative_humidity / 100
        )
    ]

    data.table::set(tdew, NULL, c("delta", "alpha"), NA_real_)
    data.table::set(tdew, NULL, c("dry_bulb_temperature", "relative_humidity"), NULL)

    data.table::setcolorder(tdew,
        c(setdiff(names(tdew), c("dew_point_temperature", "delta", "alpha")),
          "dew_point_temperature", "delta", "alpha")
    )

    tdew
}

morpher__belcher_diffuse_radiation <- function(data_epw, glob_rad) {
    diff_rad <- data.table::copy(glob_rad)
    if (!nrow(diff_rad)) {
        return(data.table::data.table())
    }
    diff_rad[data_epw[, .SD, .SDcols = c("month", "day", "hour", "diffuse_horizontal_radiation")],
        on = c("month", "day", "hour"),
        diffuse_horizontal_radiation := i.diffuse_horizontal_radiation * alpha
    ]
    diff_rad[, global_horizontal_radiation := NULL]
    diff_rad[, diffuse_horizontal_radiation := as.numeric(diffuse_horizontal_radiation)][]
}

morpher__belcher_direct_normal_radiation <- function(glob_rad, diff_rad, latitude = NULL,
                                                      longitude = NULL, timezone = NULL) {
    norm_rad <- data.table::copy(glob_rad)
    if (!nrow(glob_rad) || !nrow(diff_rad)) {
        return(data.table::data.table())
    }
    norm_rad[, diffuse_horizontal_radiation := diff_rad$diffuse_horizontal_radiation]
    norm_rad[, day_of_year := data.table::yday(datetime)]
    if (!is.null(latitude) && !is.na(latitude)) {
        norm_rad[, lat_calc := latitude]
    } else {
        norm_rad[, lat_calc := lat]
    }
    if (!is.null(longitude) && !is.na(longitude)) {
        norm_rad[, lon_calc := longitude]
    } else {
        norm_rad[, lon_calc := lon]
    }
    if (is.null(timezone) || is.na(timezone)) {
        timezone <- 0
    }
    norm_rad[, solar_angle := morpher__belcher_solar_angle(lat_calc, lon_calc, day_of_year, hour, timezone)]
    ghi <- morpher__drop_units(norm_rad$global_horizontal_radiation)
    dhi <- morpher__drop_units(norm_rad$diffuse_horizontal_radiation)
    sin_altitude <- norm_rad$solar_angle
    dni <- ifelse(
        is.finite(sin_altitude) & sin_altitude > 0 & is.finite(ghi) & ghi > 0,
        pmax(0, (ghi - dhi) / sin_altitude),
        0
    )
    dni <- pmin(dni, pmax(0, ghi) * 3)
    norm_rad[, direct_normal_radiation := as.numeric(dni)]
    norm_rad[, c("global_horizontal_radiation", "diffuse_horizontal_radiation", "day_of_year", "solar_angle") := NULL]
    norm_rad[, c("lat_calc", "lon_calc") := NULL]
}

# Integrate solar geometry at one-minute midpoints over the EPW interval that
# precedes each record time. Irradiance multiplied by 1/60 hour yields the
# Wh/m2 values required by EPW N10 and N11.

# Ridley-Boland-Lauret (2010) expresses hourly diffuse fraction as a logistic
# function of hourly/daily clearness, apparent solar time, solar altitude, and
# persistence. The output is clamped to the physically admissible [0, 1].
radiation__rbl_2010_diffuse <- function(ghi, geometry, day_key,
                                        case_key = rep("case", length(ghi))) {
    ghi <- pmax(0, as.numeric(ghi))
    ext_horizontal <- as.numeric(geometry$extraterrestrial_horizontal_radiation)
    kt <- ifelse(ext_horizontal > .Machine$double.eps,
        ghi / ext_horizontal, 0)
    state <- data.table::data.table(
        row = seq_along(ghi),
        case_key = as.character(case_key),
        day_key = as.character(day_key),
        ghi = ghi,
        ext = ext_horizontal,
        kt = kt
    )
    daily <- state[, .(
        daily_kt = if (sum(ext, na.rm = TRUE) > .Machine$double.eps) {
            sum(ghi, na.rm = TRUE) / sum(ext, na.rm = TRUE)
        } else {
            0
        }
    ), by = c("case_key", "day_key")]
    state[daily, on = c("case_key", "day_key"), daily_kt := i.daily_kt]
    state[, persistence := {
        previous <- data.table::shift(kt, 1L, type = "lag")
        following <- data.table::shift(kt, 1L, type = "lead")
        value <- rowMeans(cbind(previous, following), na.rm = TRUE)
        value[!is.finite(value)] <- kt[!is.finite(value)]
        value
    }, by = "case_key"]
    data.table::setorder(state, row)

    linear_predictor <- -5.38 +
        6.63 * kt +
        0.006 * as.numeric(geometry$apparent_solar_time) +
        -0.007 * as.numeric(geometry$solar_altitude) +
        1.75 * state$daily_kt +
        1.31 * state$persistence
    diffuse_fraction <- 1 / (1 + exp(linear_predictor))
    diffuse_fraction <- pmin(1, pmax(0, diffuse_fraction))
    diffuse <- ghi * diffuse_fraction
    diffuse[ext_horizontal <= .Machine$double.eps] <- ghi[ext_horizontal <= .Machine$double.eps]
    pmin(ghi, pmax(0, diffuse))
}

# Preserve the baseline diffuse fraction as an explicit compatibility option.
# Zero-GHI hours use a fully diffuse fraction so no beam is synthesized.
radiation__preserved_diffuse <- function(data_epw, ghi) {
    baseline_ghi <- pmax(0, as.numeric(data_epw$global_horizontal_radiation))
    baseline_dhi <- pmax(0, as.numeric(data_epw$diffuse_horizontal_radiation))
    fraction <- ifelse(
        baseline_ghi > .Machine$double.eps,
        pmin(1, baseline_dhi / baseline_ghi),
        1
    )
    pmin(ghi, pmax(0, as.numeric(ghi) * fraction))
}

# Perez et al. (1990), Table 4. Rows correspond to the eight sky-clearness
# bins; columns are the four coefficients in each published transfer equation.
ILLUMINANCE__PEREZ_GLOBAL <- rbind(
    c(96.63, -0.47, 11.50, -9.16),
    c(107.54, 0.79, 1.79, -1.19),
    c(98.73, 0.70, 4.40, -6.95),
    c(92.72, 0.56, 8.36, -8.31),
    c(86.73, 0.98, 7.10, -10.94),
    c(88.34, 1.39, 6.06, -7.60),
    c(78.63, 1.47, 4.93, -11.37),
    c(99.65, 1.86, -4.46, -3.15)
)
ILLUMINANCE__PEREZ_DIFFUSE <- rbind(
    c(97.24, -0.46, 12.00, -8.91),
    c(107.22, 1.15, 0.59, -3.95),
    c(104.97, 2.96, -5.53, -8.77),
    c(102.39, 5.59, -13.95, -13.90),
    c(100.71, 5.94, -22.75, -23.74),
    c(106.42, 3.83, -36.15, -28.83),
    c(141.88, 1.90, -53.24, -14.03),
    c(152.23, 0.35, -45.27, -7.98)
)
ILLUMINANCE__PEREZ_DIRECT <- rbind(
    c(57.20, -4.55, -2.98, 117.12),
    c(98.99, -3.46, -1.21, 12.38),
    c(109.83, -4.90, -1.71, -8.81),
    c(110.34, -5.84, -1.99, -4.56),
    c(106.36, -3.97, -1.75, -6.16),
    c(107.19, -1.25, -1.51, -26.73),
    c(105.75, 0.77, -1.26, -34.44),
    c(101.18, 1.58, -1.10, -8.29)
)
ILLUMINANCE__PEREZ_ZENITH <- rbind(
    c(40.86, 26.77, -29.59, -45.75),
    c(26.58, 14.73, 58.46, -21.25),
    c(19.34, 2.28, 100.00, 0.25),
    c(13.25, -1.39, 124.79, 15.66),
    c(14.47, -5.09, 160.09, 9.13),
    c(19.76, -3.88, 154.61, -19.21),
    c(28.39, -9.67, 151.58, -69.39),
    c(42.91, -19.62, 130.80, -164.08)
)

# Map Perez sky clearness to the eight published bins.
illuminance__perez_bin <- function(clearness) {
    findInterval(
        as.numeric(clearness),
        c(1.065, 1.230, 1.500, 1.950, 2.800, 4.500, 6.200),
        left.open = FALSE
    ) + 1L
}

# Recalculate EPW N16-N19 with the Perez 1990 luminous-efficacy and zenith
# luminance equations. Nighttime values are zero; invalid daytime inputs remain
# NA so EpwFile can serialize the field-specific missing sentinel.
illuminance__perez_1990 <- function(ghi, dhi, dni, geometry, dew_point) {
    ghi <- as.numeric(ghi)
    dhi <- as.numeric(dhi)
    dni <- as.numeric(dni)
    zenith <- as.numeric(geometry$solar_zenith)
    ext_direct <- as.numeric(geometry$extraterrestrial_direct_normal_radiation)
    projection <- as.numeric(geometry$effective_solar_projection)
    daylight <- projection > .Machine$double.eps & ext_direct > .Machine$double.eps
    air_mass <- solar__relative_air_mass(zenith)
    brightness <- dhi * air_mass / pmax(ext_direct, .Machine$double.eps)
    brightness <- pmax(brightness, 0.01)
    clearness <- ((dhi + dni) / pmax(dhi, .Machine$double.eps) +
        1.041 * zenith^3) / (1 + 1.041 * zenith^3)
    clearness[!is.finite(clearness)] <- 6.201
    bin <- illuminance__perez_bin(clearness)
    water <- exp(0.07 * as.numeric(dew_point) - 0.075)
    coefficient <- function(table) table[bin, , drop = FALSE]
    global_coef <- coefficient(ILLUMINANCE__PEREZ_GLOBAL)
    diffuse_coef <- coefficient(ILLUMINANCE__PEREZ_DIFFUSE)
    direct_coef <- coefficient(ILLUMINANCE__PEREZ_DIRECT)
    zenith_coef <- coefficient(ILLUMINANCE__PEREZ_ZENITH)

    global_efficacy <- global_coef[, 1L] + global_coef[, 2L] * water +
        global_coef[, 3L] * cos(zenith) + global_coef[, 4L] * log(brightness)
    diffuse_efficacy <- diffuse_coef[, 1L] + diffuse_coef[, 2L] * water +
        diffuse_coef[, 3L] * cos(zenith) + diffuse_coef[, 4L] * log(brightness)
    direct_efficacy <- direct_coef[, 1L] + direct_coef[, 2L] * water +
        direct_coef[, 3L] * exp(5.73 * zenith - 5) + direct_coef[, 4L] * brightness
    zenith_for_model <- ifelse(bin == 1L, pmax(zenith, 0.6), zenith)
    zenith_efficacy <- zenith_coef[, 1L] + zenith_coef[, 2L] * cos(zenith_for_model) +
        zenith_coef[, 3L] * exp(-3 * zenith_for_model) + zenith_coef[, 4L] * brightness

    output <- data.table::data.table(
        global_horizontal_illuminance = pmax(0, ghi * global_efficacy),
        direct_normal_illuminance = pmax(0, dni * direct_efficacy),
        diffuse_horizontal_illuminance = pmax(0, dhi * diffuse_efficacy),
        zenith_luminance = pmax(0, dhi * zenith_efficacy)
    )
    for (field in names(output)) {
        output[!daylight, (field) := 0]
        output[daylight & !is.finite(get(field)), (field) := NA_real_]
    }
    output[]
}

# Execute the enhanced radiation chain once so N10-N19 share the same integrated
# solar geometry and the GHI/DHI/DNI closure is exact by construction.
radiation__enhanced_chain <- function(data_epw, glob_rad, epw, tdew,
                                       diffuse_model = "rbl_2010",
                                       illuminance_model = "perez_1990") {
    if (!nrow(glob_rad)) {
        empty <- data.table::data.table()
        return(list(solar = empty, glob_rad = empty, diff_rad = empty,
            norm_rad = empty, illuminance = empty))
    }
    latitude <- morpher__epw_location_numeric(epw, c("latitude", "lat", "N2_latitude"))
    longitude <- morpher__epw_location_numeric(epw, c("longitude", "lon", "N3_longitude"))
    timezone <- morpher__epw_location_numeric(epw, c("time_zone", "timezone", "N4_time_zone"), default = 0)
    geometry <- solar__epw_interval_geometry(
        glob_rad, latitude = latitude, longitude = longitude, timezone = timezone
    )
    ghi <- pmax(0, as.numeric(glob_rad$global_horizontal_radiation))
    if (identical(diffuse_model, "rbl_2010")) {
        day_key <- sprintf("%04d-%02d-%02d", glob_rad$year, glob_rad$month, glob_rad$day)
        case_columns <- intersect(
            c("source_id", "experiment_id", "member_id", "interval"),
            names(glob_rad)
        )
        case_key <- if (length(case_columns)) {
            do.call(paste, c(glob_rad[, ..case_columns], sep = "\r"))
        } else {
            rep("case", nrow(glob_rad))
        }
        dhi <- radiation__rbl_2010_diffuse(ghi, geometry, day_key, case_key)
    } else {
        baseline_match <- data_epw[glob_rad,
            on = c("datetime", "year", "month", "day", "hour", "minute")]
        dhi <- radiation__preserved_diffuse(baseline_match, ghi)
    }
    closed <- epwphys__close_shortwave(ghi, dhi, geometry)
    glob_rad[, global_horizontal_radiation := closed$ghi]

    identity <- c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval", "datetime", "year", "month",
        "day", "hour", "minute", "delta", "alpha"
    )
    solar <- data.table::copy(glob_rad)[, .SD, .SDcols = intersect(identity, names(glob_rad))]
    solar[, `:=`(
        extraterrestrial_horizontal_radiation = geometry$extraterrestrial_horizontal_radiation,
        extraterrestrial_direct_normal_radiation = geometry$extraterrestrial_direct_normal_radiation
    )]
    diff_rad <- data.table::copy(glob_rad)[, .SD, .SDcols = intersect(identity, names(glob_rad))]
    diff_rad[, diffuse_horizontal_radiation := closed$dhi]
    norm_rad <- data.table::copy(glob_rad)[, .SD, .SDcols = intersect(identity, names(glob_rad))]
    norm_rad[, direct_normal_radiation := closed$dni]

    illuminance <- data.table::data.table()
    if (identical(illuminance_model, "perez_1990")) {
        dew_point <- rep(NA_real_, nrow(glob_rad))
        if (nrow(tdew)) {
            join_cols <- intersect(
                c("source_id", "experiment_id", "member_id", "interval",
                  "datetime", "year", "month", "day", "hour", "minute"),
                intersect(names(glob_rad), names(tdew))
            )
            dew_rows <- data.table::copy(glob_rad)[, .SD,
                .SDcols = intersect(identity, names(glob_rad))]
            dew_rows[tdew, on = join_cols, dew_point := i.dew_point_temperature]
            dew_point <- dew_rows$dew_point
        }
        missing_dew <- !is.finite(dew_point)
        if (any(missing_dew)) {
            baseline_match <- data_epw[glob_rad,
                on = c("datetime", "year", "month", "day", "hour", "minute")]
            dew_point[missing_dew] <- baseline_match$dew_point_temperature[missing_dew]
        }
        values <- illuminance__perez_1990(
            closed$ghi, closed$dhi, closed$dni, geometry, dew_point
        )
        illuminance <- data.table::copy(glob_rad)[, .SD,
            .SDcols = intersect(identity, names(glob_rad))]
        for (field in names(values)) {
            illuminance[, (field) := values[[field]]]
        }
    }
    list(
        solar = solar,
        glob_rad = glob_rad,
        diff_rad = diff_rad,
        norm_rad = norm_rad,
        illuminance = illuminance
    )
}

morpher__belcher_opaque_sky_cover <- function(data_epw, total_sky_cover) {
    if (!nrow(total_sky_cover)) {
        return(data.table::data.table())
    }
    data <- data.table::copy(total_sky_cover)[
        data_epw[, .SD, .SDcols = c("month", "day", "hour", "opaque_sky_cover", "total_sky_cover")],
        on = c("month", "day", "hour"),
        `:=`(
            baseline_opaque_sky_cover = i.opaque_sky_cover,
            baseline_total_sky_cover = i.total_sky_cover
        )
    ]
    data[, opaque_sky_cover := ifelse(
        baseline_total_sky_cover == 0,
        as.integer(round(total_sky_cover / 2.0)),
        as.integer(round(total_sky_cover * baseline_opaque_sky_cover / baseline_total_sky_cover))
    )]
    data[opaque_sky_cover > total_sky_cover, opaque_sky_cover := total_sky_cover]
    data[opaque_sky_cover < 0, opaque_sky_cover := 0L]
    data[, c("total_sky_cover", "baseline_opaque_sky_cover", "baseline_total_sky_cover") := NULL]

    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        "opaque_sky_cover", "delta", "alpha"
    )]
}

morpher__belcher_from_monthly <- function(var, data_epw, data_mean, data_max = NULL, data_min = NULL,
                                           type = c("shift", "stretch", "combined")) {
    type <- match.arg(type)
    if (!nrow(data_mean)) {
        return(data.table::data.table())
    }

    monthly <- morpher__belcher_epw_monthly(data_epw, var)
    u <- morpher__default_epw_units(var)
    data_mean <- morpher__belcher_align_units(data.table::copy(data_mean), u)

    case_fallback <- data.table::data.table()
    if (identical(type, "combined") && !is.null(data_max) && !is.null(data_min)) {
        data_max <- morpher__belcher_align_units(data.table::copy(data_max), u)
        data_min <- morpher__belcher_align_units(data.table::copy(data_min), u)
        join_cols <- c(
            "activity_drs", "institution_id", "source_id", "experiment_id",
            "member_id", "table_id", "lat", "lon", "units", "month",
            "interval"
        )
        data_mean[data_max, on = join_cols, value_max := i.value]
        data_mean[data_min, on = join_cols, value_min := i.value]

        i_max <- data_mean[J(NA_real_), on = "value_max", which = TRUE, nomatch = NULL]
        i_min <- data_mean[J(NA_real_), on = "value_min", which = TRUE, nomatch = NULL]
        i <- unique(c(i_min, i_max))
        if (length(i)) {
            cols <- c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id")
            case_fallback <- unique(data_mean[i], by = cols)
            data.table::set(case_fallback, NULL, setdiff(names(case_fallback), cols), NULL)
            cases <- case_fallback[, unique(sprintf(
                "CMIP6.%s.%s.%s.%s.%s.%s",
                activity_drs, institution_id, source_id, experiment_id, member_id, table_id
            ))]
            cases <- sprintf("[%i] '%s'", seq_along(cases), sort(cases))
            warning(sprintf(
                "Case(s) below contains missing values of max or min of '%s' data. ",
                gsub("_", " ", var)
            ),
            "'Shift' method will be used for it.\n", paste0(cases, collapse = "\n"),
            call. = FALSE
            )
        }
    }

    data_mean[monthly, on = "month", `:=`(
        delta = value - val_mean,
        alpha = value / val_mean,
        epw_mean = i.val_mean,
        epw_max = i.val_max,
        epw_min = i.val_min
    )]

    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE
    ]

    if (identical(type, "combined") && all(c("value_min", "value_max") %in% names(data))) {
        data[, alpha := ((value_max - epw_max) - (value_min - epw_min)) / (epw_max - epw_min)]
        if (nrow(case_fallback)) {
            data[case_fallback, on = names(case_fallback), alpha := 0.0]
        }
    } else {
        data[, alpha := value / epw_mean]
    }

    thres_alpha <- getOption("epwshiftr.threshold_alpha")
    if (!checkmate::test_number(thres_alpha, lower = 0)) {
        warning(paste0(
            "The threshold value for the monthly-mean fractional change (Alpha) ",
            "should be a positive number, but '",
            if (is.null(thres_alpha)) "NULL" else thres_alpha,
            "' is found."
        ))
    }
    if (type %in% c("stretch", "combined") && nrow(abnorm_alpha <- data_mean[abs(morpher__drop_units(alpha)) > thres_alpha])) {
        warning(sprintf(
            paste(
                "The absolute values of monthly-mean fractional change (Alpha) below",
                "for '%s' has exceeded the threshold (%s) set by the option",
                "'epwshiftr.threshold_alpha'. 'Shift' morphing method will be utilized",
                "instead of '%s' method to avoid unrealistic values. It is highly",
                "suggested to further investigate the input data.\n%s",
                collapse = " "
            ),
            gsub("_", " ", var, fixed = TRUE), thres_alpha, type,
            paste0(sprintf(
                "Month = %s | Monthly-mean: EPW = %s, GCM = %s --> Alpha = %s",
                format(abnorm_alpha$month),
                format(abnorm_alpha$epw_mean, digits = 3),
                format(abnorm_alpha$value, digits = 3),
                format(morpher__drop_units(abnorm_alpha$alpha), digits = 3)
            ), collapse = "\n")
        ))
        type <- "shift"
    }

    if (identical(type, "shift")) {
        data[, c(var) := get(var) + delta]
    } else if (identical(type, "stretch")) {
        data[, c(var) := get(var) * alpha]
    } else if (identical(type, "combined")) {
        if (all(c("value_min", "value_max") %in% names(data))) {
            data[, c(var) := get(var) + delta + alpha * (get(var) - epw_mean)]
        } else {
            data[, c(var) := get(var) + delta + alpha * get(var)]
        }
    }

    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        var, "delta", "alpha"
    )]
}

# The cubic smoothstep has zero slope at both ends, so adjacent monthly factor
# plateaus meet without a value or first-derivative jump.
morpher__smoothstep <- function(x) {
    x <- pmin(1, pmax(0, as.numeric(x)))
    x * x * (3 - 2 * x)
}

# Build twelve cyclic basis functions over the EPW year. Each boundary blends
# only its preceding and following month over a centered window; modulo row
# indexing makes the December-January boundary identical to the other eleven.
morpher__cyclic_month_basis <- function(month, transition_hours) {
    month <- as.integer(month)
    transition_hours <- as.integer(transition_hours)
    n <- length(month)
    basis <- matrix(0, nrow = n, ncol = 12L)
    basis[cbind(seq_len(n), month)] <- 1
    if (!n || transition_hours == 0L) {
        return(basis)
    }
    if (transition_hours < 0L || transition_hours > 336L) {
        cli::cli_abort("`transition_hours` must be between 0 and 336.")
    }
    missing_months <- setdiff(1:12, unique(month))
    if (length(missing_months)) {
        cli::cli_abort("Cyclic monthly smoothing requires all twelve EPW months.")
    }

    left <- floor(transition_hours / 2)
    offsets <- seq.int(-left, length.out = transition_hours)
    blend <- morpher__smoothstep((seq_len(transition_hours) - 0.5) / transition_hours)
    for (next_month in 1:12) {
        boundary <- which(month == next_month)[[1L]]
        rows <- ((boundary - 1L + offsets) %% n) + 1L
        previous_month <- if (next_month == 1L) 12L else next_month - 1L
        basis[rows, ] <- 0
        basis[cbind(rows, previous_month)] <- 1 - blend
        basis[cbind(rows, next_month)] <- blend
    }
    basis
}

# Solve the 12 by 12 monthly-mean constraint system. The unknown plateau
# coefficients differ slightly from the requested factors near boundaries, but
# the resulting hourly series has the requested arithmetic mean in every month.
morpher__constrained_month_series <- function(month, target, transition_hours) {
    month <- as.integer(month)
    if (length(target) != 12L || any(!is.finite(target))) {
        cli::cli_abort("Monthly smoothing targets must contain twelve finite values.")
    }
    basis <- morpher__cyclic_month_basis(month, transition_hours)
    constraint <- vapply(1:12, function(target_month) {
        colMeans(basis[month == target_month, , drop = FALSE])
    }, numeric(12L))
    constraint <- t(constraint)
    coefficients <- tryCatch(
        solve(constraint, as.numeric(target)),
        error = function(e) qr.solve(constraint, as.numeric(target), tol = 1e-12)
    )
    as.numeric(basis %*% coefficients)
}

# Read one value per month from rows in a single model/member/period case.
morpher__monthly_target_vector <- function(data, column) {
    target <- rep(NA_real_, 12L)
    for (target_month in 1:12) {
        values <- as.numeric(data[data[["month"]] == target_month, get(column)])
        values <- values[is.finite(values)]
        if (length(values)) {
            target[[target_month]] <- values[[1L]]
        }
    }
    target
}

# Identify only stable scientific case columns. Variable-specific table IDs and
# floating-point site coordinates are metadata: including either would split a
# single model/member/period into false monthly cases after spatial averaging.
BELCHER_PROJECTED_EXTREME_IDENTITY_COLUMNS <- c(
    "activity_drs", "institution_id", "source_id", "experiment_id",
    "member_id", "interval", "month"
)

BELCHER_REFERENCE_EXTREME_IDENTITY_COLUMNS <- c(
    "activity_drs", "institution_id", "source_id", "member_id", "month"
)

# Aggregate and attach one monthly-extreme field using an explicitly supplied
# scientific identity. Callers retain ownership of the projected-versus-
# historical identity and its user-facing alignment diagnostic.
morpher__attach_monthly_extreme <- function(target, extreme, value_name,
                                             identity_columns,
                                             missing_month_message) {
    target <- data.table::copy(target)
    if (is.null(extreme) || !nrow(extreme)) {
        target[, (value_name) := NA_real_]
        return(target)
    }

    # Restrict the join to identity columns represented by both inputs. Month
    # remains mandatory because the values are monthly extrema.
    join_cols <- intersect(
        identity_columns,
        intersect(names(target), names(extreme))
    )
    if (!"month" %in% join_cols) {
        cli::cli_abort(missing_month_message)
    }

    # Multiple table or spatial rows for one scientific case represent one
    # monthly value and retain the established all-missing behavior.
    extreme <- data.table::copy(extreme)
    extreme[, .extreme_value := as.numeric(value)]
    extreme <- extreme[, .(
        .extreme_value = if (all(is.na(.extreme_value))) NA_real_ else mean(.extreme_value, na.rm = TRUE)
    ), by = join_cols]
    target[extreme, on = join_cols, (value_name) := i..extreme_value]
    target[]
}

# Attach monthly extrema using model/member/period identity rather than table
# identity. This supports tas in Amon and tasmax/tasmin in a different CMIP table.
morpher__attach_extreme_value <- function(target, extreme, value_name) {
    morpher__attach_monthly_extreme(
        target,
        extreme,
        value_name,
        identity_columns = BELCHER_PROJECTED_EXTREME_IDENTITY_COLUMNS,
        missing_month_message = "Cannot align monthly extrema without a month column."
    )
}

# Smooth delta and alpha independently, then compensate the combined method for
# mean(alpha * baseline anomaly). This covariance term is the only way a
# time-varying alpha can otherwise move the requested monthly mean temperature.
morpher__smooth_enhanced_factors <- function(data, var, transform,
                                              transition_hours) {
    data <- data.table::copy(data)
    data[, .factor_order := .I]
    case_cols <- morpher__factor_case_columns(data)
    groups <- if (length(case_cols)) unique(data[, ..case_cols]) else data.table::data.table(.case = 1L)
    pieces <- vector("list", nrow(groups))
    for (i in seq_len(nrow(groups))) {
        rows <- if (length(case_cols)) {
            filter <- rep(TRUE, nrow(data))
            for (name in case_cols) {
                value <- groups[[name]][[i]]
                filter <- filter & if (is.na(value)) is.na(data[[name]]) else data[[name]] == value
            }
            data[filter]
        } else {
            data.table::copy(data)
        }
        data.table::setorder(rows, datetime)
        month <- rows$month
        # Relaxed workflows may intentionally contain only a subset of months.
        # Without all twelve constraints, retain piecewise factors for the
        # available months instead of inventing values for missing climate.
        if (!all(1:12 %in% unique(month))) {
            rows[, `:=`(
                delta = as.numeric(delta_target),
                alpha = if (identical(transform, "shift")) 0 else as.numeric(alpha_target)
            )]
            pieces[[i]] <- rows
            next
        }
        delta_target <- morpher__monthly_target_vector(rows, "delta_target")
        alpha_target <- morpher__monthly_target_vector(rows, "alpha_target")
        alpha <- if (identical(transform, "shift")) {
            rep(0, nrow(rows))
        } else {
            morpher__constrained_month_series(month, alpha_target, transition_hours)
        }

        if (transform %in% c("combined", "auto")) {
            anomaly <- as.numeric(rows[[var]]) - as.numeric(rows$epw_mean)
            covariance <- vapply(1:12, function(target_month) {
                mean(alpha[month == target_month] * anomaly[month == target_month], na.rm = TRUE)
            }, numeric(1L))
            delta_target <- delta_target - covariance
        }
        delta <- if (identical(transform, "stretch")) {
            as.numeric(rows$delta_target)
        } else {
            morpher__constrained_month_series(month, delta_target, transition_hours)
        }
        rows[, `:=`(delta = delta, alpha = alpha)]
        pieces[[i]] <- rows
    }
    out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
    data.table::setorder(out, .factor_order)
    out[, .factor_order := NULL]
    out[]
}

# Enhanced absolute-target morphing uses mean daily extrema for the EPW DTR.
# For combined temperature, alpha = (R_future - R_epw) / R_epw; invalid or
# nearly flat EPW ranges are represented explicitly as shift fallbacks.
morpher__belcher_from_monthly_enhanced <- function(
    var, data_epw, data_mean, data_max = NULL, data_min = NULL,
    type = c("shift", "stretch", "combined", "auto"), transition_hours = 72L
) {
    type <- match.arg(type)
    if (!nrow(data_mean)) {
        return(data.table::data.table())
    }
    units <- morpher__default_epw_units(var)
    data_mean <- morpher__belcher_align_units(data.table::copy(data_mean), units)
    if (!is.null(data_max) && nrow(data_max)) {
        data_max <- morpher__belcher_align_units(data.table::copy(data_max), units)
    }
    if (!is.null(data_min) && nrow(data_min)) {
        data_min <- morpher__belcher_align_units(data.table::copy(data_min), units)
    }
    data_mean <- morpher__attach_extreme_value(data_mean, data_max, "value_max")
    data_mean <- morpher__attach_extreme_value(data_mean, data_min, "value_min")
    monthly <- morpher__belcher_epw_monthly_dtr(data_epw, var)
    data_mean[monthly, on = "month", `:=`(
        epw_mean = i.val_mean,
        epw_dtr = i.val_dtr
    )]
    data_mean[, `:=`(
        delta_target = as.numeric(value - epw_mean),
        alpha_target = 0,
        method_applied = "shift",
        factor_status = "ok"
    )]

    if (identical(type, "stretch")) {
        valid <- is.finite(data_mean$epw_mean) & abs(data_mean$epw_mean) > .Machine$double.eps
        data_mean[valid, `:=`(
            alpha_target = as.numeric(value / epw_mean),
            method_applied = "stretch"
        )]
        data_mean[!valid, factor_status := "fallback_shift_zero_epw_mean"]
    } else if (type %in% c("combined", "auto")) {
        future_dtr <- as.numeric(data_mean$value_max - data_mean$value_min)
        valid_extreme <- is.finite(future_dtr)
        valid_denominator <- is.finite(data_mean$epw_dtr) & data_mean$epw_dtr > 0.1
        valid <- valid_extreme & valid_denominator
        data_mean[valid, `:=`(
            alpha_target = (future_dtr[valid] - epw_dtr) / epw_dtr,
            method_applied = "combined"
        )]
        data_mean[!valid_extreme, factor_status := "fallback_shift_missing_extremes"]
        data_mean[valid_extreme & !valid_denominator, factor_status := "fallback_shift_flat_epw_dtr"]
    }

    threshold <- getOption("epwshiftr.threshold_alpha", 3)
    if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold) || threshold < 0) {
        threshold <- Inf
    }
    too_large <- abs(data_mean$alpha_target) > threshold
    data_mean[too_large, `:=`(
        alpha_target = 0,
        method_applied = "shift",
        factor_status = "fallback_shift_alpha_threshold"
    )]

    hourly <- data_epw[, .SD, .SDcols = c(
        "datetime", "year", "month", "day", "hour", "minute", var
    )][data_mean, on = "month", allow.cartesian = TRUE]
    hourly <- morpher__smooth_enhanced_factors(
        hourly, var, transform = type, transition_hours = transition_hours
    )
    if (identical(type, "stretch")) {
        hourly[, (var) := as.numeric(get(var)) * alpha]
    } else if (type %in% c("combined", "auto")) {
        hourly[, (var) := as.numeric(get(var)) + delta + alpha * (as.numeric(get(var)) - epw_mean)]
    } else {
        hourly[, (var) := as.numeric(get(var)) + delta]
    }

    keep <- c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval", "datetime", "year", "month",
        "day", "hour", "minute", var, "delta", "alpha", "method_applied",
        "factor_status"
    )
    hourly[, .SD, .SDcols = intersect(keep, names(hourly))]
}

morpher__belcher_reference_join_cols <- function(target, reference) {
    cols <- c("institution_id", "source_id", "member_id", "table_id", "month")
    cols <- intersect(cols, intersect(names(target), names(reference)))
    if (!"month" %in% cols && "month" %in% names(target) && "month" %in% names(reference)) {
        cols <- c(cols, "month")
    }
    cols
}

morpher__belcher_attach_reference <- function(target, reference, value_name = "reference_value") {
    if (!nrow(target) || !nrow(reference)) {
        target[, (value_name) := NA_real_]
        return(target)
    }
    reference <- data.table::copy(reference)
    data.table::setnames(reference, "value", value_name)
    join_cols <- morpher__belcher_reference_join_cols(target, reference)
    if (!length(join_cols)) {
        cli::cli_abort("Cannot align target and reference climate data without shared identity columns.")
    }
    reference[, value_reference_tmp := get(value_name)]
    reference[, (value_name) := NULL]
    reference <- reference[, .(
        value_reference_tmp = mean(value_reference_tmp, na.rm = TRUE)
    ), by = join_cols]
    target[reference, on = join_cols, (value_name) := i.value_reference_tmp]
    target[]
}

morpher__belcher_handle_missing_reference <- function(data, var, strict = TRUE) {
    missing <- data[is.na(reference_value)]
    if (!nrow(missing)) {
        return(data)
    }
    message <- sprintf("Reference climate data are missing for %s in one or more morphing months.", var)
    if (isTRUE(strict)) {
        cli::cli_abort(message)
    }
    warning(message, call. = FALSE)
    data[is.na(reference_value), reference_value := value]
    data
}

morpher__belcher_from_monthly_change <- function(var, data_epw, data_mean, reference_mean,
                                                  data_max = NULL, data_min = NULL,
                                                  reference_max = NULL, reference_min = NULL,
                                                  type = c("shift", "stretch", "combined"),
                                                  strict = TRUE) {
    type <- match.arg(type)
    if (!nrow(data_mean)) {
        return(data.table::data.table())
    }
    if (!nrow(reference_mean)) {
        if (isTRUE(strict)) {
            cli::cli_abort("Change-factor morphing requires reference climate data for {.val {var}}.")
        }
        return(data.table::data.table())
    }

    monthly <- morpher__belcher_epw_monthly(data_epw, var)
    u <- morpher__default_epw_units(var)
    data_mean <- morpher__belcher_align_units(data.table::copy(data_mean), u)
    reference_mean <- morpher__belcher_align_units(data.table::copy(reference_mean), u)
    data_mean <- morpher__belcher_attach_reference(data_mean, reference_mean, "reference_value")
    data_mean <- morpher__belcher_handle_missing_reference(data_mean, var, strict = strict)

    case_fallback <- data.table::data.table()
    if (identical(type, "combined") && !is.null(data_max) && !is.null(data_min) &&
        !is.null(reference_max) && !is.null(reference_min)) {
        data_max <- morpher__belcher_align_units(data.table::copy(data_max), u)
        data_min <- morpher__belcher_align_units(data.table::copy(data_min), u)
        reference_max <- morpher__belcher_align_units(data.table::copy(reference_max), u)
        reference_min <- morpher__belcher_align_units(data.table::copy(reference_min), u)
        join_cols <- intersect(c(
            "activity_drs", "institution_id", "source_id", "experiment_id",
            "member_id", "table_id", "lat", "lon", "units", "month",
            "interval"
        ), names(data_mean))
        data_mean[data_max, on = join_cols, value_max := i.value]
        data_mean[data_min, on = join_cols, value_min := i.value]
        data_mean <- morpher__belcher_attach_reference(data_mean, reference_max, "reference_max")
        data_mean <- morpher__belcher_attach_reference(data_mean, reference_min, "reference_min")

        missing_extreme <- data_mean[
            is.na(value_max) | is.na(value_min) | is.na(reference_max) | is.na(reference_min)
        ]
        if (nrow(missing_extreme)) {
            cols <- c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id")
            case_fallback <- unique(missing_extreme[, ..cols], by = cols)
            cases <- case_fallback[, unique(sprintf(
                "CMIP6.%s.%s.%s.%s.%s.%s",
                activity_drs, institution_id, source_id, experiment_id, member_id, table_id
            ))]
            cases <- sprintf("[%i] '%s'", seq_along(cases), sort(cases))
            warning(sprintf(
                "Case(s) below contains missing target or reference max/min values of '%s'. ",
                gsub("_", " ", var)
            ),
            "'Shift' method will be used for it.\n", paste0(cases, collapse = "\n"),
            call. = FALSE
            )
        }
    }

    data_mean[monthly, on = "month", `:=`(
        epw_mean = i.val_mean,
        epw_max = i.val_max,
        epw_min = i.val_min
    )]
    data_mean[, delta := value - reference_value]

    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE
    ]

    if (identical(type, "combined") && all(c("value_min", "value_max", "reference_min", "reference_max") %in% names(data))) {
        data[, alpha := ((value_max - reference_max) - (value_min - reference_min)) / (epw_max - epw_min)]
        if (nrow(case_fallback)) {
            data[case_fallback, on = names(case_fallback), alpha := 0.0]
        }
        data[is.na(alpha) | !is.finite(morpher__drop_units(alpha)), alpha := 0.0]
    } else if (identical(type, "stretch")) {
        ref <- morpher__drop_units(data$reference_value)
        val <- morpher__drop_units(data$value)
        data[, alpha := ifelse(is.finite(ref) & abs(ref) > .Machine$double.eps, val / ref, NA_real_)]
        missing_alpha <- data[is.na(alpha) | !is.finite(alpha)]
        if (nrow(missing_alpha)) {
            message <- sprintf("Reference climate data include zero values for %s; cannot compute stretch factors.", var)
            if (isTRUE(strict)) {
                cli::cli_abort(message)
            }
            warning(message, call. = FALSE)
            data[is.na(alpha) | !is.finite(alpha), alpha := 1.0]
        }
    } else {
        data[, alpha := 0.0]
    }

    thres_alpha <- getOption("epwshiftr.threshold_alpha")
    if (!checkmate::test_number(thres_alpha, lower = 0)) {
        warning(paste0(
            "The threshold value for the monthly-mean fractional change (Alpha) ",
            "should be a positive number, but '",
            if (is.null(thres_alpha)) "NULL" else thres_alpha,
            "' is found."
        ))
    }
    if (type %in% c("stretch", "combined") && nrow(abnorm_alpha <- data[abs(morpher__drop_units(alpha)) > thres_alpha])) {
        warning(sprintf(
            paste(
                "The absolute values of monthly-mean fractional change (Alpha) below",
                "for '%s' has exceeded the threshold (%s) set by the option",
                "'epwshiftr.threshold_alpha'. 'Shift' morphing method will be utilized",
                "instead of '%s' method to avoid unrealistic values. It is highly",
                "suggested to further investigate the input data.\n%s",
                collapse = " "
            ),
            gsub("_", " ", var, fixed = TRUE), thres_alpha, type,
            paste0(sprintf(
                "Month = %s | Future = %s, Reference = %s --> Alpha = %s",
                format(abnorm_alpha$month),
                format(abnorm_alpha$value, digits = 3),
                format(abnorm_alpha$reference_value, digits = 3),
                format(morpher__drop_units(abnorm_alpha$alpha), digits = 3)
            ), collapse = "\n")
        ))
        type <- "shift"
    }

    if (identical(type, "shift")) {
        data[, c(var) := get(var) + delta]
    } else if (identical(type, "stretch")) {
        data[, c(var) := get(var) * alpha]
    } else if (identical(type, "combined")) {
        data[, c(var) := get(var) + delta + alpha * (get(var) - epw_mean)]
    }

    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        var, "delta", "alpha"
    )]
}

# Attach historical extrema across experiments and variable-specific tables,
# retaining model/member/month as the scientific case identity.
morpher__attach_reference_extreme <- function(target, reference, value_name) {
    morpher__attach_monthly_extreme(
        target,
        reference,
        value_name,
        identity_columns = BELCHER_REFERENCE_EXTREME_IDENTITY_COLUMNS,
        missing_month_message = "Cannot align historical monthly extrema without a month column."
    )
}

# Enhanced change-factor morphing applies
# alpha = (R_future - R_reference) / R_epw to the EPW anomaly. The same guarded
# per-month fallback and covariance-compensated smoothing used by the absolute
# path keeps the target monthly mean exact across month boundaries.
morpher__belcher_from_monthly_change_enhanced <- function(
    var, data_epw, data_mean, reference_mean,
    data_max = NULL, data_min = NULL,
    reference_max = NULL, reference_min = NULL,
    type = c("shift", "stretch", "combined", "auto"), strict = TRUE,
    transition_hours = 72L
) {
    type <- match.arg(type)
    if (!nrow(data_mean)) {
        return(data.table::data.table())
    }
    if (!nrow(reference_mean)) {
        if (isTRUE(strict)) {
            cli::cli_abort("Change-factor morphing requires reference climate data for {.val {var}}.")
        }
        return(data.table::data.table())
    }

    units <- morpher__default_epw_units(var)
    data_mean <- morpher__belcher_align_units(data.table::copy(data_mean), units)
    reference_mean <- morpher__belcher_align_units(data.table::copy(reference_mean), units)
    data_mean <- morpher__belcher_attach_reference(data_mean, reference_mean, "reference_value")
    data_mean <- morpher__belcher_handle_missing_reference(data_mean, var, strict = strict)

    align_optional <- function(data) {
        if (is.null(data) || !nrow(data)) return(NULL)
        morpher__belcher_align_units(data.table::copy(data), units)
    }
    data_mean <- morpher__attach_extreme_value(data_mean, align_optional(data_max), "value_max")
    data_mean <- morpher__attach_extreme_value(data_mean, align_optional(data_min), "value_min")
    data_mean <- morpher__attach_reference_extreme(data_mean, align_optional(reference_max), "reference_max")
    data_mean <- morpher__attach_reference_extreme(data_mean, align_optional(reference_min), "reference_min")

    monthly <- morpher__belcher_epw_monthly_dtr(data_epw, var)
    data_mean[monthly, on = "month", `:=`(
        epw_mean = i.val_mean,
        epw_dtr = i.val_dtr
    )]
    data_mean[, `:=`(
        delta_target = as.numeric(value - reference_value),
        alpha_target = 0,
        method_applied = "shift",
        factor_status = "ok"
    )]

    if (identical(type, "stretch")) {
        reference_numeric <- as.numeric(data_mean$reference_value)
        valid <- is.finite(reference_numeric) & abs(reference_numeric) > .Machine$double.eps
        data_mean[valid, `:=`(
            alpha_target = as.numeric(value / reference_value),
            method_applied = "stretch"
        )]
        data_mean[!valid, factor_status := "fallback_shift_zero_reference"]
    } else if (type %in% c("combined", "auto")) {
        future_dtr <- as.numeric(data_mean$value_max - data_mean$value_min)
        reference_dtr <- as.numeric(data_mean$reference_max - data_mean$reference_min)
        valid_extreme <- is.finite(future_dtr) & is.finite(reference_dtr)
        valid_denominator <- is.finite(data_mean$epw_dtr) & data_mean$epw_dtr > 0.1
        valid <- valid_extreme & valid_denominator
        data_mean[valid, `:=`(
            alpha_target = (future_dtr[valid] - reference_dtr[valid]) / epw_dtr,
            method_applied = "combined"
        )]
        data_mean[!valid_extreme, factor_status := "fallback_shift_missing_extremes"]
        data_mean[valid_extreme & !valid_denominator, factor_status := "fallback_shift_flat_epw_dtr"]
    }

    threshold <- getOption("epwshiftr.threshold_alpha", 3)
    if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold) || threshold < 0) {
        threshold <- Inf
    }
    too_large <- abs(data_mean$alpha_target) > threshold
    data_mean[too_large, `:=`(
        alpha_target = 0,
        method_applied = "shift",
        factor_status = "fallback_shift_alpha_threshold"
    )]

    hourly <- data_epw[, .SD, .SDcols = c(
        "datetime", "year", "month", "day", "hour", "minute", var
    )][data_mean, on = "month", allow.cartesian = TRUE]
    hourly <- morpher__smooth_enhanced_factors(
        hourly, var, transform = type, transition_hours = transition_hours
    )
    if (identical(type, "stretch")) {
        hourly[, (var) := as.numeric(get(var)) * alpha]
    } else if (type %in% c("combined", "auto")) {
        hourly[, (var) := as.numeric(get(var)) + delta + alpha * (as.numeric(get(var)) - epw_mean)]
    } else {
        hourly[, (var) := as.numeric(get(var)) + delta]
    }

    keep <- c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval", "datetime", "year", "month",
        "day", "hour", "minute", var, "delta", "alpha", "method_applied",
        "factor_status"
    )
    hourly[, .SD, .SDcols = intersect(keep, names(hourly))]
}

morpher__belcher_tdb <- function(data_epw, context, type) {
    tas <- morpher__belcher_monthly_variable(context, "tas")
    if (!nrow(tas)) {
        return(data.table::data.table())
    }
    tasmax <- morpher__belcher_monthly_variable(context, "tasmax")
    tasmin <- morpher__belcher_monthly_variable(context, "tasmin")
    if (identical(context$recipe$profile, "enhanced")) {
        return(morpher__belcher_from_monthly_enhanced(
            "dry_bulb_temperature", data_epw, tas,
            if (nrow(tasmax)) tasmax else NULL,
            if (nrow(tasmin)) tasmin else NULL,
            type = type,
            transition_hours = context$recipe$options$transition_hours
        ))
    }
    morpher__belcher_from_monthly(
        "dry_bulb_temperature", data_epw, tas,
        if (nrow(tasmax)) tasmax else NULL,
        if (nrow(tasmin)) tasmin else NULL,
        type = type
    )
}

morpher__belcher_rh <- function(data_epw, context, type) {
    hurs <- morpher__belcher_monthly_variable(context, "hurs")
    if (!nrow(hurs)) {
        return(data.table::data.table())
    }
    hursmax <- morpher__belcher_monthly_variable(context, "hursmax")
    hursmin <- morpher__belcher_monthly_variable(context, "hursmin")
    rh <- if (identical(context$recipe$profile, "enhanced")) {
        morpher__belcher_from_monthly_enhanced(
            "relative_humidity", data_epw, hurs,
            if (nrow(hursmax)) hursmax else NULL,
            if (nrow(hursmin)) hursmin else NULL,
            type = type,
            transition_hours = context$recipe$options$transition_hours
        )
    } else {
        morpher__belcher_from_monthly(
            "relative_humidity", data_epw, hurs,
            if (nrow(hursmax)) hursmax else NULL,
            if (nrow(hursmin)) hursmin else NULL,
            type = type
        )
    }
    rh[relative_humidity > 100, relative_humidity := 100]
    rh[relative_humidity < 0, relative_humidity := 0]
    rh
}

morpher__belcher_change_tdb <- function(data_epw, context, type) {
    tas <- morpher__belcher_monthly_variable(context, "tas")
    tas_ref <- morpher__belcher_monthly_reference_variable(context, "tas")
    if (!nrow(tas)) {
        return(data.table::data.table())
    }
    tasmax <- morpher__belcher_monthly_variable(context, "tasmax")
    tasmin <- morpher__belcher_monthly_variable(context, "tasmin")
    tasmax_ref <- morpher__belcher_monthly_reference_variable(context, "tasmax")
    tasmin_ref <- morpher__belcher_monthly_reference_variable(context, "tasmin")
    if (identical(context$recipe$profile, "enhanced")) {
        return(morpher__belcher_from_monthly_change_enhanced(
            "dry_bulb_temperature", data_epw, tas, tas_ref,
            if (nrow(tasmax)) tasmax else NULL,
            if (nrow(tasmin)) tasmin else NULL,
            if (nrow(tasmax_ref)) tasmax_ref else NULL,
            if (nrow(tasmin_ref)) tasmin_ref else NULL,
            type = type,
            strict = context$strict,
            transition_hours = context$recipe$options$transition_hours
        ))
    }
    morpher__belcher_from_monthly_change(
        "dry_bulb_temperature", data_epw, tas, tas_ref,
        if (nrow(tasmax)) tasmax else NULL,
        if (nrow(tasmin)) tasmin else NULL,
        if (nrow(tasmax_ref)) tasmax_ref else NULL,
        if (nrow(tasmin_ref)) tasmin_ref else NULL,
        type = type,
        strict = context$strict
    )
}

morpher__belcher_change_rh <- function(data_epw, context, type) {
    hurs <- morpher__belcher_monthly_variable(context, "hurs")
    hurs_ref <- morpher__belcher_monthly_reference_variable(context, "hurs")
    if (!nrow(hurs)) {
        return(data.table::data.table())
    }
    hursmax <- morpher__belcher_monthly_variable(context, "hursmax")
    hursmin <- morpher__belcher_monthly_variable(context, "hursmin")
    hursmax_ref <- morpher__belcher_monthly_reference_variable(context, "hursmax")
    hursmin_ref <- morpher__belcher_monthly_reference_variable(context, "hursmin")
    rh <- if (identical(context$recipe$profile, "enhanced")) {
        morpher__belcher_from_monthly_change_enhanced(
            "relative_humidity", data_epw, hurs, hurs_ref,
            if (nrow(hursmax)) hursmax else NULL,
            if (nrow(hursmin)) hursmin else NULL,
            if (nrow(hursmax_ref)) hursmax_ref else NULL,
            if (nrow(hursmin_ref)) hursmin_ref else NULL,
            type = type,
            strict = context$strict,
            transition_hours = context$recipe$options$transition_hours
        )
    } else {
        morpher__belcher_from_monthly_change(
            "relative_humidity", data_epw, hurs, hurs_ref,
            if (nrow(hursmax)) hursmax else NULL,
            if (nrow(hursmin)) hursmin else NULL,
            if (nrow(hursmax_ref)) hursmax_ref else NULL,
            if (nrow(hursmin_ref)) hursmin_ref else NULL,
            type = type,
            strict = context$strict
        )
    }
    rh[relative_humidity > 100, relative_humidity := 100]
    rh[relative_humidity < 0, relative_humidity := 0]
    rh
}

morpher__belcher_monthly_field <- function(data_epw, context, variable_id, epw_field, type) {
    data <- morpher__belcher_monthly_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    if (identical(context$recipe$profile, "enhanced")) {
        return(morpher__belcher_from_monthly_enhanced(
            epw_field, data_epw, data, type = type,
            transition_hours = context$recipe$options$transition_hours
        ))
    }
    morpher__belcher_from_monthly(epw_field, data_epw, data, type = type)
}

morpher__belcher_change_monthly_field <- function(data_epw, context, variable_id, epw_field, type) {
    data <- morpher__belcher_monthly_variable(context, variable_id)
    reference <- morpher__belcher_monthly_reference_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    if (identical(context$recipe$profile, "enhanced")) {
        return(morpher__belcher_from_monthly_change_enhanced(
            epw_field,
            data_epw,
            data,
            reference,
            type = type,
            strict = context$strict,
            transition_hours = context$recipe$options$transition_hours
        ))
    }
    morpher__belcher_from_monthly_change(
        epw_field,
        data_epw,
        data,
        reference,
        type = type,
        strict = context$strict
    )
}

morpher__belcher_monthly_change_variable <- function(context, variable_id, target_units = NULL) {
    data <- morpher__belcher_monthly_variable(context, variable_id)
    reference <- morpher__belcher_monthly_reference_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    if (!nrow(reference)) {
        if (isTRUE(context$strict)) {
            cli::cli_abort("Change-factor morphing requires reference climate data for {.val {variable_id}}.")
        }
        return(data.table::data.table())
    }
    if (!is.null(target_units)) {
        data <- morpher__belcher_align_units(data.table::copy(data), target_units)
        reference <- morpher__belcher_align_units(data.table::copy(reference), target_units)
    }
    data <- morpher__belcher_attach_reference(data, reference, "reference_value")
    data <- morpher__belcher_handle_missing_reference(data, variable_id, strict = context$strict)
    data[, value := value - reference_value]
    data[, reference_value := NULL]
    data[]
}

# Morph cloud cover as a smoothed additive factor while retaining the baseline
# hourly cloud sequence. Values are rounded only after the constrained factor
# series is applied because EPW stores sky cover in tenths.
morpher__belcher_total_sky_cover_enhanced <- function(
    data_epw, context, data_mean, change_factor = FALSE
) {
    baseline_monthly <- data_epw[, .(
        epw_mean = mean(as.numeric(total_sky_cover), na.rm = TRUE)
    ), by = "month"]
    data_mean <- data.table::copy(data_mean)
    data_mean[baseline_monthly, on = "month", epw_mean := i.epw_mean]
    data_mean[, target_mean := if (isTRUE(change_factor)) {
        epw_mean + as.numeric(value) / 10
    } else {
        as.numeric(value) / 10
    }]
    data_mean[, `:=`(
        delta_target = target_mean - epw_mean,
        alpha_target = 0,
        method_applied = "shift",
        factor_status = "ok"
    )]
    hourly <- data_epw[, .SD, .SDcols = c(
        "datetime", "year", "month", "day", "hour", "minute",
        "total_sky_cover"
    )][data_mean, on = "month", allow.cartesian = TRUE]
    hourly <- morpher__smooth_enhanced_factors(
        hourly,
        "total_sky_cover",
        transform = "shift",
        transition_hours = context$recipe$options$transition_hours
    )
    hourly[, total_sky_cover := as.integer(round(pmax(
        0, pmin(10, as.numeric(total_sky_cover) + delta)
    )))]
    hourly[, alpha := NA_real_]
    keep <- c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval", "datetime", "year", "month",
        "day", "hour", "minute", "total_sky_cover", "delta", "alpha",
        "factor_status"
    )
    hourly[, .SD, .SDcols = intersect(keep, names(hourly))]
}

morpher__belcher_total_sky_cover <- function(data_epw, context, data_mean = NULL, change_factor = FALSE) {
    var <- "total_sky_cover"
    if (is.null(data_mean)) {
        data_mean <- morpher__belcher_monthly_variable(context, "clt")
    }
    if (!nrow(data_mean)) {
        return(data.table::data.table())
    }
    if (!is.null(context) && identical(context$recipe$profile, "enhanced")) {
        return(morpher__belcher_total_sky_cover_enhanced(
            data_epw, context, data_mean,
            change_factor = change_factor
        ))
    }
    monthly <- unique(data_epw[, .SD, .SDcols = "month"])
    data_mean <- data_mean[monthly, on = "month"]
    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE
    ]
    data.table::set(data, NULL, "value", morpher__drop_units(data$value))
    if (isTRUE(change_factor)) {
        data[, target_total_sky_cover := as.integer(round(pmax(0, pmin(10, total_sky_cover + value / 10))))]
    } else {
        data[, target_total_sky_cover := as.integer(round(pmax(0, pmin(10, value / 10))))]
    }
    data[, `:=`(
        delta = target_total_sky_cover - total_sky_cover,
        alpha = ifelse(total_sky_cover == 0, NA_real_, target_total_sky_cover / total_sky_cover),
        total_sky_cover = target_total_sky_cover
    )]
    data[, target_total_sky_cover := NULL]
    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        var, "delta", "alpha"
    )]
}

morpher__belcher_change_total_sky_cover <- function(data_epw, context) {
    data_mean <- morpher__belcher_monthly_change_variable(context, "clt", target_units = "%")
    morpher__belcher_total_sky_cover(data_epw, context, data_mean = data_mean, change_factor = TRUE)
}

# Return a non-blocking diagnostic when optional snow data cannot form the
# required future/reference pair; the required policy promotes the same state
# to an error before any hourly values are changed.
morpher__snow_unavailable <- function(context, message) {
    if (identical(context$recipe$options$snow_depth, "required")) {
        cli::cli_abort(message, class = "epwshiftr_snow_required_error")
    }
    list(
        data = data.table::data.table(),
        diagnostics = morpher__diagnostic(
            stage = "runtime",
            severity = "warning",
            code = "optional_snd_unavailable",
            message = message,
            variable_id = "snd",
            epw_field = "snow_depth",
            action = "Provide matching future and historical snd from CMIP6 LImon, or set snow_depth = 'off'."
        )
    )
}

# Scale existing EPW snow events by the monthly future/reference SND ratio.
# CMIP SND is converted from metres to EPW centimetres; zero reference or a
# snow-free EPW month never synthesizes new event timing.
morpher__belcher_snow_depth <- function(data_epw, context) {
    policy <- context$recipe$options$snow_depth
    if (!identical(context$recipe$profile, "enhanced") || identical(policy, "off")) {
        return(list(data = data.table::data.table(), diagnostics = morpher__empty_diagnostics()))
    }
    if (is.null(context$reference_climate)) {
        return(morpher__snow_unavailable(
            context,
            "Snow-depth morphing requires an explicit historical climate reference containing snd."
        ))
    }
    future <- morpher__belcher_monthly_variable(context, "snd")
    reference <- morpher__belcher_monthly_reference_variable(context, "snd")
    future_complete <- nrow(future) && all(1:12 %in% unique(future$month)) &&
        all(is.finite(as.numeric(future$value)))
    reference_complete <- nrow(reference) && all(1:12 %in% unique(reference$month)) &&
        all(is.finite(as.numeric(reference$value)))
    if (!future_complete || !reference_complete) {
        return(morpher__snow_unavailable(
            context,
            "Snow-depth morphing was skipped because future and historical snd are not both complete."
        ))
    }
    future <- morpher__belcher_align_units(data.table::copy(future), "cm")
    reference <- morpher__belcher_align_units(data.table::copy(reference), "cm")
    future <- morpher__belcher_attach_reference(future, reference, "reference_value")
    future <- morpher__belcher_handle_missing_reference(
        future, "snd", strict = identical(policy, "required")
    )
    future[, `:=`(
        delta_target = as.numeric(value - reference_value),
        alpha_target = ifelse(
            is.finite(reference_value) & reference_value > .Machine$double.eps,
            as.numeric(value / reference_value),
            ifelse(is.finite(value) & value <= .Machine$double.eps, 0, 1)
        ),
        factor_status = ifelse(
            reference_value <= .Machine$double.eps & value > .Machine$double.eps,
            "zero_reference_snow_preserved",
            "ok"
        )
    )]
    hourly <- data_epw[, .SD, .SDcols = c(
        "datetime", "year", "month", "day", "hour", "minute", "snow_depth"
    )][future, on = "month", allow.cartesian = TRUE]
    hourly[, .snow_order := .I]
    case_cols <- morpher__factor_case_columns(hourly)
    groups <- if (length(case_cols)) unique(hourly[, ..case_cols]) else data.table::data.table(.case = 1L)
    pieces <- vector("list", nrow(groups))
    for (i in seq_len(nrow(groups))) {
        rows <- if (length(case_cols)) {
            keep <- rep(TRUE, nrow(hourly))
            for (name in case_cols) {
                value <- groups[[name]][[i]]
                keep <- keep & if (is.na(value)) is.na(hourly[[name]]) else hourly[[name]] == value
            }
            hourly[keep]
        } else {
            data.table::copy(hourly)
        }
        data.table::setorder(rows, datetime)
        alpha_target <- morpher__monthly_target_vector(rows, "alpha_target")
        alpha <- morpher__constrained_month_series(
            rows$month, alpha_target,
            context$recipe$options$transition_hours
        )
        rows[, alpha := alpha]
        pieces[[i]] <- rows
    }
    hourly <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
    data.table::setorder(hourly, .snow_order)
    hourly[, .snow_order := NULL]
    baseline_snow <- as.numeric(hourly$snow_depth)
    valid_baseline <- is.finite(baseline_snow) & baseline_snow >= 0 & baseline_snow < 999
    scaled <- baseline_snow
    scaled[valid_baseline] <- baseline_snow[valid_baseline] * pmax(0, hourly$alpha[valid_baseline])
    # Multiplication cannot create snow where the baseline event state is zero.
    scaled[valid_baseline & baseline_snow <= .Machine$double.eps] <- 0
    hourly[, `:=`(
        snow_depth = scaled,
        delta = delta_target,
        method_applied = "ratio"
    )]
    keep <- c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval", "datetime", "year", "month",
        "day", "hour", "minute", "snow_depth", "delta", "alpha",
        "method_applied", "factor_status"
    )
    list(
        data = hourly[, .SD, .SDcols = intersect(keep, names(hourly))],
        diagnostics = morpher__empty_diagnostics()
    )
}

# Normalize precipitation units before manually converting fluxes to monthly
# water-equivalent depth; udunits does not know the density convention.
morpher__precip_unit_kind <- function(units) {
    units <- morpher__unit_alias(units)
    if (is.na(units) || !nzchar(units)) {
        return(NA_character_)
    }
    key <- tolower(trimws(units))
    key <- gsub("\\s+", "", key)
    key <- gsub("\\^", "", key)
    switch(
        key,
        "kgm-2s-1" = "kg_m2_s",
        "kgm**-2s**-1" = "kg_m2_s",
        "kg/m2/s" = "kg_m2_s",
        "kgm-2sec-1" = "kg_m2_s",
        "mmday-1" = "mm_day",
        "mm/day" = "mm_day",
        "mmd-1" = "mm_day",
        "mm/d" = "mm_day",
        "mm" = "mm",
        NA_character_
    )
}

# Return Gregorian month lengths for the period years used by morphing.
morpher__precip_month_days <- function(year, month) {
    year <- as.integer(year)
    month <- as.integer(month)
    if (!length(year) || !length(month)) {
        return(integer())
    }
    mapply(function(y, m) {
        start <- as.Date(sprintf("%04d-%02d-01", y, m))
        next_year <- y + as.integer(m == 12L)
        next_month <- if (m == 12L) 1L else m + 1L
        as.integer(as.Date(sprintf("%04d-%02d-01", next_year, next_month)) - start)
    }, year, month)
}

# Convert a precipitation rate or monthly depth into water-equivalent millimetres.
morpher__precip_depth_checked <- function(value, units, seconds) {
    kind <- morpher__precip_unit_kind(units)
    value <- morpher__drop_units(value)
    if (!length(value) || is.na(value[[1L]]) || is.na(kind)) {
        return(list(value = as.numeric(value), ok = !is.na(kind), message = "Unsupported precipitation units."))
    }
    value <- as.numeric(value[[1L]])
    seconds <- as.numeric(seconds[[1L]])
    if (!is.finite(value) || !is.finite(seconds)) {
        return(list(value = value, ok = TRUE, message = NA_character_))
    }
    out <- switch(
        kind,
        kg_m2_s = value * seconds,
        mm_day = value * seconds / 86400,
        mm = value
    )
    list(value = out, ok = TRUE, message = NA_character_)
}

# Convert climate summary rows for `pr` from monthly mean rate to monthly depth.
morpher__precip_summary_depth_checked <- function(value, units, years_json, month) {
    years <- tryCatch(morpher__json_int_vector(years_json), error = function(e) integer())
    if (!length(years)) {
        years <- 2001L
    }
    days <- morpher__precip_month_days(years, month)
    morpher__precip_depth_checked(value, units, mean(days, na.rm = TRUE) * 86400)
}

# Convert a baseline EPW monthly mean precipitation depth into a monthly total.
morpher__baseline_precip_depth_checked <- function(value, units, month) {
    if (is.na(units) || !nzchar(units)) {
        units <- "mm"
    }
    converted <- morpher__convert_value_checked(value, units, "mm")
    converted$value <- converted$value * morpher__precip_month_days(2001L, month) * 24
    converted
}

# Summarise raw `pr` climate data to monthly water-equivalent depths.
morpher__belcher_monthly_precip_variable <- function(context, variable_id, reference = FALSE) {
    data <- if (isTRUE(reference)) {
        morpher__context_reference_variable(context, variable_id)
    } else {
        morpher__context_variable(context, variable_id)
    }
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    data <- data.table::as.data.table(data.table::copy(data))
    data <- morpher__resolve_calendar_columns(data, month = TRUE, day = TRUE)
    data[, year := as.integer(year)]
    data <- data[!(month == 2L & day == 29L)]

    identity <- morpher__context_identity_rows(data)
    data <- data.table::data.table(
        identity,
        units = as.character(data$units),
        value = as.numeric(data$value),
        year = as.integer(data$year),
        month = as.integer(data$month),
        interval = as.factor(data$period)
    )
    group_cols <- c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id", "units", "month", "interval")
    out <- data[, .(
        lon = mean(lon, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        value = mean(value, na.rm = TRUE),
        years = list(sort(unique(year)))
    ), by = group_cols]

    values <- vapply(seq_len(nrow(out)), function(i) {
        days <- morpher__precip_month_days(out$years[[i]], out$month[[i]])
        converted <- morpher__precip_depth_checked(out$value[[i]], out$units[[i]], mean(days, na.rm = TRUE) * 86400)
        if (!isTRUE(converted$ok)) {
            cli::cli_abort("Unsupported precipitation units for {.val {variable_id}}: {.val {out$units[[i]]}}.")
        }
        converted$value
    }, numeric(1L))
    out[, `:=`(
        value = as.numeric(values),
        units = "mm",
        years = NULL
    )]
    data.table::setcolorder(out, c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id", "lon", "lat", "units", "value", "month", "interval"))
    out[]
}

# Report conservative precipitation fallbacks consistently across strict modes.
morpher__belcher_precip_guard <- function(rows, message, strict = TRUE) {
    if (!nrow(rows)) {
        return(invisible(NULL))
    }
    months <- paste(sort(unique(rows$month)), collapse = ", ")
    message <- sprintf("%s Month(s): %s.", message, months)
    if (isTRUE(strict)) {
        cli::cli_abort(message)
    }
    warning(message, call. = FALSE)
    invisible(NULL)
}

# Apply monthly precipitation targets while preserving baseline wet-hour timing.
morpher__belcher_precip_from_monthly <- function(data_epw, data_mean, strict = TRUE,
                                                 change_factor = FALSE) {
    if (!nrow(data_mean) || !"liquid_precip_depth" %in% names(data_epw)) {
        return(data.table::data.table())
    }
    rate_col <- "liquid_precip_rate"
    keep <- c("datetime", "year", "month", "day", "hour", "minute", "liquid_precip_depth")
    if (rate_col %in% names(data_epw)) {
        keep <- c(keep, rate_col)
    }
    baseline <- data_epw[, .SD, .SDcols = keep]
    if (!rate_col %in% names(baseline)) {
        baseline[, (rate_col) := 0]
    }
    baseline[, .baseline_precip_depth := pmax(0, morpher__drop_units(liquid_precip_depth))]
    monthly <- baseline[, .(baseline_total = sum(.baseline_precip_depth, na.rm = TRUE)), by = "month"]

    data_mean <- data.table::copy(data_mean)
    data_mean[monthly, on = "month", baseline_total := i.baseline_total]
    data_mean[is.na(baseline_total), baseline_total := 0]
    data_mean[, future_total := morpher__drop_units(value)]

    if (isTRUE(change_factor)) {
        data_mean[, reference_total := morpher__drop_units(reference_value)]
        zero_reference <- data_mean[reference_total <= .Machine$double.eps & future_total > .Machine$double.eps]
        morpher__belcher_precip_guard(
            zero_reference,
            "Reference climate precipitation is zero while future precipitation is positive; preserving baseline precipitation in relaxed mode.",
            strict = strict
        )
        # Relaxed mode cannot infer new storm frequency from zero reference rain,
        # so it keeps the baseline precipitation magnitude unchanged.
        data_mean[, alpha := data.table::fifelse(
            reference_total > .Machine$double.eps,
            future_total / reference_total,
            data.table::fifelse(future_total <= .Machine$double.eps, 0, 1)
        )]
        data_mean[, target_total := baseline_total * alpha]
        data_mean[, delta := future_total - reference_total]
    } else {
        data_mean[, target_total := future_total]
        data_mean[, alpha := data.table::fifelse(
            baseline_total > .Machine$double.eps,
            target_total / baseline_total,
            NA_real_
        )]
        data_mean[, delta := target_total - baseline_total]
    }

    dry_target <- data_mean[baseline_total <= .Machine$double.eps & future_total > .Machine$double.eps]
    morpher__belcher_precip_guard(
        dry_target,
        "Baseline EPW has no wet hours for positive target precipitation; keeping the month dry in relaxed mode.",
        strict = strict
    )
    # Without baseline wet hours, v1 deliberately refuses to synthesize event
    # timing and therefore leaves precipitation at zero for that month.
    data_mean[baseline_total <= .Machine$double.eps, `:=`(
        target_total = 0,
        alpha = NA_real_
    )]

    data <- baseline[data_mean, on = "month", allow.cartesian = TRUE]
    scale <- ifelse(is.na(data$alpha), 0, data$alpha)
    # CMIP6 `pr` only supplies precipitation amount after time integration; the
    # EPW liquid precipitation duration/rate field is derived from wet hours.
    depth <- data$.baseline_precip_depth * scale
    data[, liquid_precip_depth := as.numeric(depth)]
    data[, liquid_precip_rate := as.numeric(depth > .Machine$double.eps)]
    data[, .baseline_precip_depth := NULL]
    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        "liquid_precip_depth", "liquid_precip_rate", "delta", "alpha"
    )]
}

# Build absolute-target Belcher precipitation from future climate monthly totals.
morpher__belcher_precip <- function(data_epw, context) {
    pr <- morpher__belcher_monthly_precip_variable(context, "pr")
    morpher__belcher_precip_from_monthly(data_epw, pr, strict = context$strict)
}

# Build change-factor Belcher precipitation from future/reference monthly totals.
morpher__belcher_change_precip <- function(data_epw, context) {
    pr <- morpher__belcher_monthly_precip_variable(context, "pr")
    pr_ref <- morpher__belcher_monthly_precip_variable(context, "pr", reference = TRUE)
    if (!nrow(pr)) {
        return(data.table::data.table())
    }
    if (!nrow(pr_ref)) {
        if (isTRUE(context$strict)) {
            cli::cli_abort("Change-factor morphing requires reference climate data for {.val pr}.")
        }
        warning("Reference climate data are missing for pr; precipitation is left unchanged.", call. = FALSE)
        return(data.table::data.table())
    }
    pr <- morpher__belcher_attach_reference(pr, pr_ref, "reference_value")
    pr <- morpher__belcher_handle_missing_reference(pr, "pr", strict = context$strict)
    morpher__belcher_precip_from_monthly(data_epw, pr, strict = context$strict, change_factor = TRUE)
}

# Summarise runtime fallback and clipping states into inspectable factor rows
# and compact diagnostics without emitting one message per EPW hour.
morpher__enhanced_factor_metadata <- function(context, parts) {
    rows <- list()
    for (part_name in names(parts)) {
        part <- parts[[part_name]]
        if (!nrow(part) || !"factor_status" %in% names(part)) {
            next
        }
        by <- intersect(
            c("source_id", "experiment_id", "member_id", "interval", "month",
              "factor_status", "method_applied"),
            names(part)
        )
        summary <- part[, .(
            delta = if ("delta" %in% names(part)) mean(as.numeric(delta), na.rm = TRUE) else NA_real_,
            alpha = if ("alpha" %in% names(part)) mean(as.numeric(alpha), na.rm = TRUE) else NA_real_
        ), by = by]
        summary[, step := part_name]
        rows[[length(rows) + 1L]] <- summary
    }
    factors <- if (length(rows)) {
        data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    } else {
        data.table::data.table()
    }
    bad <- if (nrow(factors)) factors[factor_status != "ok"] else factors
    diagnostics <- lapply(seq_len(nrow(bad)), function(i) {
        status <- bad$factor_status[[i]]
        message <- switch(
            status,
            fallback_shift_missing_extremes = "Combined morphing fell back to shift because monthly extrema were incomplete or non-finite.",
            fallback_shift_flat_epw_dtr = "Combined morphing fell back to shift because the EPW mean daily range was not greater than 0.1 C.",
            fallback_shift_alpha_threshold = "Morphing fell back to shift because alpha exceeded epwshiftr.threshold_alpha.",
            fallback_shift_zero_reference = "Stretch morphing fell back to shift because the reference value was zero or non-finite.",
            fallback_shift_zero_epw_mean = "Stretch morphing fell back to shift because the EPW monthly mean was zero or non-finite.",
            saturation_clipped = "Specific humidity was clipped to the saturation state before RH and dew point were calculated.",
            sprintf("Enhanced morphing reported factor state %s.", status)
        )
        morpher__diagnostic(
            stage = "runtime",
            severity = "warning",
            code = status,
            message = message,
            variable_id = if (identical(bad$step[[i]], "tdb")) "tas" else if (identical(bad$step[[i]], "rh")) "huss" else NA_character_,
            epw_field = if (identical(bad$step[[i]], "tdb")) "dry_bulb_temperature" else if (identical(bad$step[[i]], "rh")) "relative_humidity" else NA_character_,
            period = if ("interval" %in% names(bad)) bad$interval[[i]] else NA_character_,
            month = if ("month" %in% names(bad)) bad$month[[i]] else NA_integer_,
            action = "Inspect the persisted factor status and input coverage for this month."
        )
    })
    list(
        factors = factors,
        diagnostics = morpher__bind_diagnostics(diagnostics)
    )
}

# Select the five builders that differ between absolute-target and
# change-factor Belcher execution while leaving their equations independent.
morpher__belcher_execution_steps <- function(change_factor = FALSE) {
    if (isTRUE(change_factor)) {
        return(list(
            tdb = morpher__belcher_change_tdb,
            monthly_field = morpher__belcher_change_monthly_field,
            rh = morpher__belcher_change_rh,
            total_cover = morpher__belcher_change_total_sky_cover,
            precip = morpher__belcher_change_precip
        ))
    }

    list(
        tdb = morpher__belcher_tdb,
        monthly_field = morpher__belcher_monthly_field,
        rh = morpher__belcher_rh,
        total_cover = morpher__belcher_total_sky_cover,
        precip = morpher__belcher_precip
    )
}

# Execute the common Belcher EPW assembly after the runner wrapper has chosen
# whether fields come from absolute targets or future-minus-reference changes.
morpher__belcher_execute <- function(context, change_factor = FALSE) {
    steps <- morpher__belcher_execution_steps(change_factor)
    methods <- context$recipe$methods
    epw <- context$epw$clone()
    data_epw <- suppressMessages(epw$add_unit()$data())

    tdb <- steps$tdb(data_epw, context, methods[["tdb"]])
    p <- steps$monthly_field(
        data_epw,
        context,
        "psl",
        "atmospheric_pressure",
        methods[["p"]]
    )

    # Keep the profile-specific humidity source decision in the shared flow so
    # both execution modes apply identical thermodynamic closure behavior.
    humidity_source <- morpher__belcher_humidity_source(context)
    if (identical(humidity_source, "huss")) {
        humidity <- morpher__belcher_huss_state(data_epw, context, tdb, p)
        rh <- humidity$rh
        tdew <- humidity$tdew
    } else {
        rh <- steps$rh(data_epw, context, methods[["rh"]])
        tdew <- if (!nrow(tdb) || !nrow(rh)) {
            data.table::data.table()
        } else {
            morpher__belcher_tdew(tdb, rh)
        }
    }

    data_epw[, horizontal_infrared_radiation_intensity_from_sky :=
        as.numeric(horizontal_infrared_radiation_intensity_from_sky)]
    hor_ir <- steps$monthly_field(
        data_epw,
        context,
        "rlds",
        "horizontal_infrared_radiation_intensity_from_sky",
        methods[["hor_ir"]]
    )

    data_epw[, global_horizontal_radiation :=
        as.numeric(global_horizontal_radiation)]
    glob_rad <- steps$monthly_field(
        data_epw,
        context,
        "rsds",
        "global_horizontal_radiation",
        methods[["glob_rad"]]
    )

    # Preserve the enhanced closure and published legacy radiation paths as
    # separate definitions after their shared monthly field has been built.
    if (identical(context$recipe$profile, "enhanced")) {
        radiation <- radiation__enhanced_chain(
            data_epw, glob_rad, epw, tdew,
            diffuse_model = context$recipe$options$diffuse_model,
            illuminance_model = context$recipe$options$illuminance_model
        )
        solar <- radiation$solar
        glob_rad <- radiation$glob_rad
        diff_rad <- radiation$diff_rad
        norm_rad <- radiation$norm_rad
        illuminance <- radiation$illuminance
    } else {
        solar <- data.table::data.table()
        illuminance <- data.table::data.table()
        diff_rad <- if (!nrow(glob_rad)) {
            data.table::data.table()
        } else {
            morpher__belcher_diffuse_radiation(data_epw, glob_rad)
        }
        epw_lat <- morpher__epw_location_numeric(
            epw,
            c("latitude", "lat", "N2_latitude")
        )
        epw_lon <- morpher__epw_location_numeric(
            epw,
            c("longitude", "lon", "N3_longitude")
        )
        epw_tz <- morpher__epw_location_numeric(
            epw,
            c("time_zone", "timezone", "N4_time_zone"),
            default = 0
        )
        norm_rad <- if (!nrow(glob_rad) || !nrow(diff_rad)) {
            data.table::data.table()
        } else {
            morpher__belcher_direct_normal_radiation(
                glob_rad,
                diff_rad,
                latitude = epw_lat,
                longitude = epw_lon,
                timezone = epw_tz
            )
        }
    }

    wind <- steps$monthly_field(
        data_epw,
        context,
        "sfcWind",
        "wind_speed",
        methods[["wind"]]
    )
    total_cover <- steps$total_cover(data_epw, context)
    opaque_cover <- if (!nrow(total_cover)) {
        data.table::data.table()
    } else {
        morpher__belcher_opaque_sky_cover(data_epw, total_cover)
    }
    precip <- steps$precip(data_epw, context)
    snow <- morpher__belcher_snow_depth(data_epw, context)

    # Keep the established part order because it controls both field overlay
    # precedence and the persisted result contract fixed by the snapshots.
    parts <- list(
        tdb = tdb,
        tdew = tdew,
        rh = rh,
        p = p,
        hor_ir = hor_ir,
        solar = solar,
        glob_rad = glob_rad,
        norm_rad = norm_rad,
        diff_rad = diff_rad,
        illuminance = illuminance,
        wind = wind,
        total_cover = total_cover,
        opaque_cover = opaque_cover,
        snow_depth = snow$data,
        precip = precip
    )
    suppressMessages(epw$drop_unit())
    for (name in names(parts)) {
        parts[[name]] <- morpher__belcher_drop_units(
            parts[[name]],
            intersect(names(parts[[name]]), names(data_epw))
        )
    }
    metadata <- morpher__enhanced_factor_metadata(context, parts)
    morpher__engine_output(
        context, epw, parts,
        diagnostics = morpher__bind_diagnostics(
            metadata$diagnostics,
            snow$diagnostics
        ),
        factors = metadata$factors
    )
}

# Retain the registered absolute-target runner while delegating its common EPW
# assembly to the shared Belcher executor.
morpher__belcher_absolute_run <- function(context, backend = NULL) {
    morpher__belcher_execute(context, change_factor = FALSE)
}

# Retain the registered change-factor runner and its no-reference fallback while
# delegating identified change cases to the shared Belcher executor.
morpher__belcher_run <- function(context, backend = NULL) {
    if (is.null(context$reference_climate)) {
        # Without external historical climate, the EPW monthly climatology is
        # the reference. Applying future-minus-EPW changes is equivalent to the
        # absolute-target implementation, including precipitation scaling.
        return(morpher__belcher_absolute_run(context, backend))
    }

    morpher__belcher_execute(context, change_factor = TRUE)
}
# }}}
