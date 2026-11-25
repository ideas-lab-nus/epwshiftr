#' @include weather-temperature.R
NULL

# Sobie-Curry daily backend
# The backend adjusts only the four thermodynamic EPW fields described by Sobie
# and Curry (2025). Each rule declares the daily CMIP variables needed by both
# the paper-faithful transformation and the harmonized humidity closure.
EPW_MORPH_SOBIE_CURRY_METHODS <- c(
    tdb = "mean_dtr_anomaly",
    tdew = "mean_sd_anomaly",
    rh = "multiplicative",
    pressure = "additive"
)

EPW_MORPH_SOBIE_CURRY_RULES <- data.table::data.table(
    step = c("tdb", "tdew", "rh", "pressure"),
    epw_field = c(
        "dry_bulb_temperature",
        "dew_point_temperature",
        "relative_humidity",
        "atmospheric_pressure"
    ),
    variable_id = c(
        "tasmin,tasmax",
        "tas,huss,ps",
        "tas,huss,ps",
        "ps"
    ),
    optional_variable_id = NA_character_,
    method = unname(EPW_MORPH_SOBIE_CURRY_METHODS),
    required = TRUE,
    derived = FALSE,
    method_choices = as.list(unname(EPW_MORPH_SOBIE_CURRY_METHODS))
)

# The published method uses a 21-day moving window. The tolerance controls only
# explicit zero-denominator fallbacks and never changes the method equations.
EPW_MORPH_SOBIE_CURRY_OPTIONS <- list(
    window_days = 21L,
    tolerance = 1e-8
)

# Validate JSON-safe Sobie-Curry settings before either recipe persistence or
# pipeline execution so resumed workflows cannot reinterpret their options.
# sobie__backend_options {{{
sobie__backend_options <- function(options = NULL) {
    temperature__backend_options(
        options,
        defaults = EPW_MORPH_SOBIE_CURRY_OPTIONS,
        label = "Sobie-Curry"
    )
}
# }}}

# Extract one unit-normalized CMIP variable while rejecting duplicate,
# conflicting timestamps before wide alignment.
# sobie__variable_rows {{{
sobie__variable_rows <- function(data, variable_id) {
    target_variable <- variable_id
    rows <- data[variable_id == target_variable]
    key <- c("time", "annual_phase")
    conflicts <- rows[,
        .(values = data.table::uniqueN(.SD[["value"]])),
        by = key
    ]
    if (any(conflicts[["values"]] > 1L)) {
        cli::cli_abort(
            "Sobie-Curry variable {.val {variable_id}} has conflicting values at aligned timestamps."
        )
    }
    rows <- rows[!duplicated(rows, by = key)]
    out <- rows[, c(key, "value"), with = FALSE]
    data.table::setnames(out, "value", variable_id)
    out[]
}
# }}}

# Normalize daily CMIP rows and derive daily relative humidity and dew point
# from aligned tas, huss, and surface pressure before factor estimation.
# sobie__climate {{{
sobie__climate <- function(data, name) {
    variables <- c("tas", "tasmin", "tasmax", "huss", "ps")
    normalized <- weather__normalize_climate_units(
        data,
        name = name,
        expected_frequency = "day",
        target_units = c(
            tas = "K",
            tasmin = "K",
            tasmax = "K",
            huss = "1",
            ps = "Pa"
        ),
        required_cols = c(
            "variable_id",
            "time",
            "annual_phase",
            "value",
            "units",
            "frequency"
        ),
        required_variables = variables
    )
    daily__check_phase(
        normalized[["annual_phase"]],
        sprintf("%s[['annual_phase']]", name)
    )

    wide <- Reduce(
        function(left, right) {
            merge(
                left,
                right,
                by = c("time", "annual_phase"),
                all = TRUE,
                sort = FALSE
            )
        },
        lapply(variables, function(variable_id) {
            sobie__variable_rows(normalized, variable_id)
        })
    )
    incomplete <- !stats::complete.cases(wide[, variables, with = FALSE])
    if (any(incomplete)) {
        cli::cli_abort(
            "{.arg {name}} requires aligned daily tas, tasmin, tasmax, huss, and ps values."
        )
    }
    if (any(wide[["tasmax"]] < wide[["tasmin"]])) {
        cli::cli_abort(
            "{.arg {name}} contains daily tasmax values below tasmin."
        )
    }

    relative_humidity <- epwphys__hurs_from_huss_si(
        wide[["huss"]],
        wide[["tas"]],
        wide[["ps"]]
    )
    dry_mean <- morpher__convert_value(
        (wide[["tasmin"]] + wide[["tasmax"]]) / 2,
        "K",
        "degC"
    )
    dry_minimum <- morpher__convert_value(
        wide[["tasmin"]],
        "K",
        "degC"
    )
    dry_maximum <- morpher__convert_value(
        wide[["tasmax"]],
        "K",
        "degC"
    )
    dry_temperature <- morpher__convert_value(
        wide[["tas"]],
        "K",
        "degC"
    )
    dew_point <- epwphys__dew_point_from_rh(
        dry_temperature,
        relative_humidity / 100
    )
    data.table::data.table(
        time = wide[["time"]],
        annual_phase = wide[["annual_phase"]],
        dry_mean = dry_mean,
        dry_minimum = dry_minimum,
        dry_maximum = dry_maximum,
        specific_humidity = wide[["huss"]],
        dew_point = dew_point,
        relative_humidity = relative_humidity,
        pressure = wide[["ps"]]
    )
}
# }}}

# Estimate one unsmoothed statistic for every calendar-neutral target day.
# Native 360-, 365-, and 366-day phases are mapped to their nearest containing
# interval on the shared 365-day grid before interannual statistics are taken.
# sobie__daily_statistics {{{
sobie__daily_statistics <- function(
    data,
    value,
    target_year_days = 365L
) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(value, min.chars = 1L)
    if (!value %in% names(data)) {
        cli::cli_abort(
            "Sobie-Curry climate input lacks statistic column {.val {value}}."
        )
    }
    phase <- daily__check_phase(data[["annual_phase"]])
    values <- as.numeric(data[[value]])
    target_day <- daily__target_day(phase, target_year_days)
    working <- data.table::data.table(
        target_day = as.integer(target_day),
        value = values
    )
    statistics <- working[,
        {
            valid <- is.finite(.SD[["value"]])
            count <- sum(valid)
            list(
                mean = if (count) mean(.SD[["value"]][valid]) else NA_real_,
                standard_deviation = if (count >= 2L) {
                    stats::sd(.SD[["value"]][valid])
                } else {
                    NA_real_
                },
                n = count
            )
        },
        by = "target_day"
    ]
    grid <- data.table::data.table(
        target_day = seq_len(target_year_days),
        annual_phase = daily__phase_grid(target_year_days)
    )
    out <- merge(
        grid,
        statistics,
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setorderv(out, "target_day")
    out[]
}
# }}}

# Calculate every unsmoothed, calendar-aligned thermodynamic statistic consumed
# by the Sobie-Curry signal equations.
# sobie__daily_statistics_set {{{
sobie__daily_statistics_set <- function(data) {
    variables <- c(
        "dry_mean",
        "dry_minimum",
        "dry_maximum",
        "specific_humidity",
        "dew_point",
        "relative_humidity",
        "pressure"
    )
    stats::setNames(
        lapply(variables, function(variable) {
            sobie__daily_statistics(
                data,
                value = variable
            )
        }),
        variables
    )
}
# }}}

# Precompute the 21 target-day indices belonging to every circular smoothing
# window so the signal kernel receives alignment, not calendar semantics.
# sobie__smoothing_windows {{{
sobie__smoothing_windows <- function(
    window_days,
    target_year_days = 365L
) {
    phase <- daily__phase_grid(target_year_days)
    spec <- daily__window_spec(window_days, target_year_days)
    lapply(phase, function(center) {
        which(
            daily__phase_distance(phase, center) <=
                spec$half_width + 8 * .Machine$double.eps
        )
    })
}
# }}}

# Apply pre-aligned circular moving averages to one raw factor vector and fail
# explicitly if a target window contains no estimable daily factor.
# sobie__smooth_factor {{{
sobie__smooth_factor <- function(value, windows, name) {
    checkmate::assert_numeric(value, any.missing = TRUE)
    checkmate::assert_list(windows, min.len = 1L)
    checkmate::assert_string(name, min.chars = 1L)
    out <- vapply(
        windows,
        function(index) {
            values <- value[index]
            values <- values[is.finite(values)]
            if (!length(values)) {
                return(NA_real_)
            }
            mean(values)
        },
        numeric(1L)
    )
    if (any(!is.finite(out))) {
        cli::cli_abort(
            "Sobie-Curry factor {.val {name}} cannot be estimated for every 21-day window."
        )
    }
    out
}
# }}}

# Reduce raw zero-denominator states over each circular smoothing window while
# keeping a fallback visible whenever any contributing daily factor used it.
# sobie__smooth_status {{{
sobie__smooth_status <- function(status, windows) {
    vapply(
        windows,
        function(index) {
            values <- status[index]
            values <- values[!is.na(values) & values != "missing_alignment"]
            if (!length(values)) {
                return("missing_alignment")
            }
            inherited <- values[grepl("^inherited_", values)]
            if (length(inherited)) {
                return(inherited[[1L]])
            }
            if (all(grepl("^identity_", values))) {
                return(values[[1L]])
            }
            "ok"
        },
        character(1L)
    )
}
# }}}

# Form a multiplicative ratio with an explicit identity fallback when the
# historical denominator is zero and the published equation is undefined.
# sobie__ratio {{{
sobie__ratio <- function(future, historical, tolerance) {
    future <- as.numeric(future)
    historical <- as.numeric(historical)
    ratio <- rep.int(NA_real_, length(historical))
    status <- rep.int("missing_alignment", length(historical))
    finite <- is.finite(future) & is.finite(historical)
    regular <- finite & abs(historical) > tolerance
    ratio[regular] <- future[regular] / historical[regular]
    both_zero <- finite & !regular & abs(future) <= tolerance
    ratio[both_zero] <- 1
    status[both_zero] <- "identity_zero_historical"
    inherited <- finite & !regular & !both_zero
    ratio[inherited] <- 1
    status[inherited] <- "inherited_zero_historical"
    status[regular] <- "ok"
    list(value = ratio, status = status)
}
# }}}

# Apply the published daily mean, DTR, humidity, pressure, and dew-point signal
# definitions after both climate periods share one 365-day phase grid.
# sobie__signal_factors {{{
sobie__signal_factors <- function(
    future,
    historical,
    windows,
    tolerance
) {
    metrics <- names(future)
    if (!identical(metrics, names(historical))) {
        cli::cli_abort(
            "Sobie-Curry future and historical climatologies are not aligned."
        )
    }
    target_day <- future$dry_mean[["target_day"]]
    annual_phase <- future$dry_mean[["annual_phase"]]
    aligned <- vapply(
        metrics,
        function(metric) {
            identical(future[[metric]][["target_day"]], target_day) &&
                identical(historical[[metric]][["target_day"]], target_day)
        },
        logical(1L)
    )
    if (!all(aligned)) {
        cli::cli_abort(
            "Sobie-Curry climatology statistics do not share one target grid."
        )
    }

    raw_humidity_ratio <- sobie__ratio(
        future$relative_humidity[["mean"]],
        historical$relative_humidity[["mean"]],
        tolerance
    )
    raw_dew_sd_ratio <- sobie__ratio(
        future$dew_point[["standard_deviation"]],
        historical$dew_point[["standard_deviation"]],
        tolerance
    )

    # Equation (3) uses the future-minus-historical DTR change. The hourly
    # denominator remains EPW-day-specific and is therefore applied later.
    future_dtr <- future$dry_maximum[["mean"]] -
        future$dry_minimum[["mean"]]
    historical_dtr <- historical$dry_maximum[["mean"]] -
        historical$dry_minimum[["mean"]]

    raw_temperature_mean_delta <- future$dry_mean[["mean"]] -
        historical$dry_mean[["mean"]]
    raw_temperature_dtr_delta <- future_dtr - historical_dtr
    raw_dew_point_mean_delta <- future$dew_point[["mean"]] -
        historical$dew_point[["mean"]]
    raw_specific_humidity_delta <- future$specific_humidity[["mean"]] -
        historical$specific_humidity[["mean"]]
    raw_pressure_delta <- future$pressure[["mean"]] -
        historical$pressure[["mean"]]

    # The paper calculates daily factors first and then smooths those factors;
    # applying the window to source variables first would change the nonlinear
    # humidity and standard-deviation ratios.
    data.table::data.table(
        target_day = target_day,
        annual_phase = annual_phase,
        temperature_mean_delta = sobie__smooth_factor(
            raw_temperature_mean_delta,
            windows,
            "temperature_mean_delta"
        ),
        temperature_dtr_delta = sobie__smooth_factor(
            raw_temperature_dtr_delta,
            windows,
            "temperature_dtr_delta"
        ),
        dew_point_mean_delta = sobie__smooth_factor(
            raw_dew_point_mean_delta,
            windows,
            "dew_point_mean_delta"
        ),
        # Equations (5)-(6) are interpreted as a relative *change* in standard
        # deviation. Subtracting one makes zero climate change an identity
        # transform and matches the paper's past-future difference wording.
        dew_point_sd_relative_change = sobie__smooth_factor(
            raw_dew_sd_ratio$value - 1,
            windows,
            "dew_point_sd_relative_change"
        ),
        dew_point_sd_status = sobie__smooth_status(
            raw_dew_sd_ratio$status,
            windows
        ),
        relative_humidity_ratio = sobie__smooth_factor(
            raw_humidity_ratio$value,
            windows,
            "relative_humidity_ratio"
        ),
        relative_humidity_status = sobie__smooth_status(
            raw_humidity_ratio$status,
            windows
        ),
        # Harmonized execution uses the same additive HUSS state-change
        # convention as epwshiftr's shared humidity closure.
        specific_humidity_delta = sobie__smooth_factor(
            raw_specific_humidity_delta,
            windows,
            "specific_humidity_delta"
        ),
        pressure_delta = sobie__smooth_factor(
            raw_pressure_delta,
            windows,
            "pressure_delta"
        ),
        n_future = vapply(
            windows,
            function(index) {
                sum(future$dry_mean[["n"]][index], na.rm = TRUE)
            },
            integer(1L)
        ),
        n_historical = vapply(
            windows,
            function(index) {
                sum(historical$dry_mean[["n"]][index], na.rm = TRUE)
            },
            integer(1L)
        )
    )
}
# }}}

# Resolve an EPW daily-range denominator with a traceable identity fallback for
# a flat baseline day, where the published temperature equation is undefined.
# sobie__temperature_alpha {{{
sobie__temperature_alpha <- function(dtr_delta, baseline_dtr, tolerance) {
    dtr_delta <- as.numeric(dtr_delta)
    baseline_dtr <- as.numeric(baseline_dtr)
    alpha <- numeric(length(baseline_dtr))
    status <- rep.int("ok", length(baseline_dtr))
    regular <- baseline_dtr > tolerance
    alpha[regular] <- dtr_delta[regular] / baseline_dtr[regular]
    unchanged <- !regular & abs(dtr_delta) <= tolerance
    status[unchanged] <- "identity_flat_baseline"
    status[!regular & !unchanged] <- "inherited_flat_baseline"
    list(value = alpha, status = status)
}
# }}}

# Apply equations (1)-(6) independently to each preserved EPW day. Temperature
# and dew-point anomaly changes are centered on their own baseline daily means.
# sobie__hourly_reconstruct {{{
sobie__hourly_reconstruct <- function(data) {
    baseline <- data$baseline
    factors <- data.table::copy(data$factors)
    tolerance <- data$options$tolerance
    template <- data.table::copy(baseline$template)
    weather <- data.table::copy(baseline$weather)
    data.table::set(template, j = ".sobie_row", value = seq_len(nrow(template)))
    data.table::set(
        template,
        j = "dew_point_temperature",
        value = as.numeric(weather[["dew_point_temperature"]])
    )
    data.table::set(
        template,
        j = "relative_humidity",
        value = as.numeric(weather[["relative_humidity"]])
    )
    data.table::set(
        template,
        j = "atmospheric_pressure",
        value = as.numeric(weather[["atmospheric_pressure"]])
    )

    baseline_days <- template[,
        .(
            baseline_temperature_mean = mean(
                .SD[["dry_bulb_temperature"]]
            ),
            baseline_temperature_minimum = min(
                .SD[["dry_bulb_temperature"]]
            ),
            baseline_temperature_maximum = max(
                .SD[["dry_bulb_temperature"]]
            ),
            baseline_dew_point_mean = mean(
                .SD[["dew_point_temperature"]]
            )
        ),
        by = "target_day"
    ]
    data.table::set(
        baseline_days,
        j = "baseline_temperature_dtr",
        value = baseline_days[["baseline_temperature_maximum"]] -
            baseline_days[["baseline_temperature_minimum"]]
    )
    factors <- merge(
        factors,
        baseline_days,
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    temperature_alpha <- sobie__temperature_alpha(
        factors[["temperature_dtr_delta"]],
        factors[["baseline_temperature_dtr"]],
        tolerance
    )
    data.table::set(
        factors,
        j = "temperature_anomaly_relative_change",
        value = temperature_alpha$value
    )
    data.table::set(
        factors,
        j = "temperature_dtr_status",
        value = temperature_alpha$status
    )

    hourly <- merge(
        template,
        factors,
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setorderv(hourly, ".sobie_row")

    # Equations (3)-(4): add the daily-mean change, then change each anomaly by
    # the projected DTR change divided by the observed EPW daily range.
    temperature_projected <- hourly[["dry_bulb_temperature"]] +
        hourly[["temperature_mean_delta"]] +
        hourly[["temperature_anomaly_relative_change"]] *
            (hourly[["dry_bulb_temperature"]] -
                hourly[["baseline_temperature_mean"]])

    # Equations (5)-(6): the implemented alpha is sigma_f / sigma_o - 1 so an
    # unchanged modeled standard deviation leaves the EPW anomalies unchanged.
    dew_point_projected <- hourly[["dew_point_temperature"]] +
        hourly[["dew_point_mean_delta"]] +
        hourly[["dew_point_sd_relative_change"]] *
            (hourly[["dew_point_temperature"]] -
                hourly[["baseline_dew_point_mean"]])
    relative_humidity_projected <- hourly[["relative_humidity"]] *
        hourly[["relative_humidity_ratio"]]
    pressure_projected <- hourly[["atmospheric_pressure"]] +
        hourly[["pressure_delta"]]

    data.table::set(
        hourly,
        j = "temperature_projected",
        value = temperature_projected
    )
    data.table::set(
        hourly,
        j = "dew_point_projected",
        value = dew_point_projected
    )
    data.table::set(
        hourly,
        j = "relative_humidity_projected",
        value = relative_humidity_projected
    )
    data.table::set(
        hourly,
        j = "pressure_projected",
        value = pressure_projected
    )

    achieved <- hourly[,
        .(
            projected_temperature_mean = mean(
                .SD[["temperature_projected"]]
            ),
            projected_temperature_minimum = min(
                .SD[["temperature_projected"]]
            ),
            projected_temperature_maximum = max(
                .SD[["temperature_projected"]]
            ),
            projected_dew_point_mean = mean(
                .SD[["dew_point_projected"]]
            )
        ),
        by = "target_day"
    ]
    factors <- merge(
        factors,
        achieved,
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::set(
        factors,
        j = "temperature_mean_closure_error",
        value = factors[["projected_temperature_mean"]] -
            (factors[["baseline_temperature_mean"]] +
                factors[["temperature_mean_delta"]])
    )
    data.table::set(
        factors,
        j = "temperature_dtr_closure_error",
        value = (factors[["projected_temperature_maximum"]] -
            factors[["projected_temperature_minimum"]]) -
            (factors[["baseline_temperature_dtr"]] +
                factors[["temperature_dtr_delta"]])
    )
    data.table::set(
        factors,
        j = "dew_point_mean_closure_error",
        value = factors[["projected_dew_point_mean"]] -
            (factors[["baseline_dew_point_mean"]] +
                factors[["dew_point_mean_delta"]])
    )
    data.table::setorderv(factors, "target_day")

    list(
        baseline = baseline,
        hourly = hourly,
        factors = factors,
        options = data$options
    )
}
# }}}

# Calculate the Sobie-Curry target HUSS independently of its physical closure
# so the published climate signal remains owned by the method adapter.
# sobie__specific_humidity_target {{{
sobie__specific_humidity_target <- function(hourly) {
    temperature <- as.numeric(hourly[["temperature_projected"]])
    pressure <- as.numeric(hourly[["pressure_projected"]])
    baseline_temperature <- as.numeric(hourly[["dry_bulb_temperature"]])
    baseline_humidity <- as.numeric(hourly[["relative_humidity"]])
    baseline_pressure <- as.numeric(hourly[["atmospheric_pressure"]])
    delta <- as.numeric(hourly[["specific_humidity_delta"]])
    valid <- is.finite(temperature) &
        is.finite(pressure) &
        pressure > 0 &
        is.finite(baseline_temperature) &
        is.finite(baseline_humidity) &
        baseline_humidity >= 0 &
        baseline_humidity <= 100 &
        is.finite(baseline_pressure) &
        baseline_pressure > 0 &
        is.finite(delta)
    if (!all(valid)) {
        cli::cli_abort(
            paste(
                "Sobie-Curry harmonized humidity closure requires finite",
                "temperature, pressure, humidity, and HUSS-delta values."
            )
        )
    }

    baseline_huss <- epwphys__huss_from_rh_si(
        baseline_temperature,
        baseline_humidity,
        baseline_pressure
    )
    list(
        baseline_specific_humidity = baseline_huss,
        target_specific_humidity = baseline_huss + delta
    )
}
# }}}

# Select the paper-faithful independent thermodynamic transforms or the
# harmonized HUSS-state closure without changing the preceding Sobie-Curry
# climate signal, sequence, or hourly temperature stages.
# sobie__physics_apply {{{
sobie__physics_apply <- function(data, context) {
    baseline <- data$baseline
    hourly <- data$hourly
    factors <- data$factors
    policy <- context$recipe$policy
    checkmate::assert_choice(
        policy,
        c("paper_faithful", "harmonized")
    )
    fields <- list(
        dry_bulb_temperature = hourly[["temperature_projected"]],
        atmospheric_pressure = hourly[["pressure_projected"]]
    )
    humidity <- NULL
    if (identical(policy, "paper_faithful")) {
        fields$dew_point_temperature <- hourly[["dew_point_projected"]]
        fields$relative_humidity <- hourly[["relative_humidity_projected"]]
        humidity_request <- list()
    } else {
        target <- sobie__specific_humidity_target(hourly)
        humidity_request <- list(
            target_specific_humidity = target$target_specific_humidity
        )
    }
    # Both policy variants use the common executor; the faithful policy overlays
    # independent published fields and the harmonized policy closes target HUSS.
    physical <- epwphys__apply(
        EpwPhysicalRequest(
            template = baseline$weather,
            fields = fields,
            humidity = humidity_request,
            provenance = list(adapter = "sobie_curry")
        ),
        epwphys__recipe_policy(context$recipe)
    )
    weather <- data.table::copy(physical@weather)
    if (identical(policy, "harmonized")) {
        humidity <- physical@state$humidity
        humidity$baseline_specific_humidity <-
            target$baseline_specific_humidity
        humidity$target_specific_humidity <-
            target$target_specific_humidity
    }

    diagnostic_values <- list(
        sobie_curry_target_day = hourly[["target_day"]],
        sobie_curry_annual_phase = hourly[["annual_phase"]],
        sobie_curry_temperature_mean_delta = hourly[["temperature_mean_delta"]],
        sobie_curry_temperature_dtr_delta = hourly[["temperature_dtr_delta"]],
        sobie_curry_temperature_anomaly_relative_change = hourly[[
            "temperature_anomaly_relative_change"
        ]],
        sobie_curry_temperature_dtr_status = hourly[["temperature_dtr_status"]],
        sobie_curry_dew_point_mean_delta = hourly[["dew_point_mean_delta"]],
        sobie_curry_dew_point_sd_relative_change = hourly[[
            "dew_point_sd_relative_change"
        ]],
        sobie_curry_dew_point_sd_status = hourly[["dew_point_sd_status"]],
        sobie_curry_relative_humidity_ratio = hourly[[
            "relative_humidity_ratio"
        ]],
        sobie_curry_relative_humidity_status = hourly[[
            "relative_humidity_status"
        ]],
        sobie_curry_pressure_delta = hourly[["pressure_delta"]]
    )
    if (identical(policy, "harmonized")) {
        diagnostic_values <- c(
            diagnostic_values,
            list(
                sobie_curry_specific_humidity_delta = hourly[[
                    "specific_humidity_delta"
                ]],
                sobie_curry_baseline_specific_humidity = humidity$baseline_specific_humidity,
                sobie_curry_target_specific_humidity = humidity$target_specific_humidity,
                sobie_curry_specific_humidity = humidity$specific_humidity,
                sobie_curry_saturation_specific_humidity = humidity$saturation_specific_humidity,
                sobie_curry_humidity_closure_status = humidity$status
            )
        )
    }
    data.table::set(
        weather,
        j = names(diagnostic_values),
        value = diagnostic_values
    )

    diagnostics <- list()
    fallback <- factors[["temperature_dtr_status"]] == "inherited_flat_baseline"
    if (any(fallback)) {
        diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "warning",
            code = "sobie_curry_flat_temperature_day",
            message = sprintf(
                "Temperature DTR change was inherited for %d flat EPW day(s).",
                sum(fallback)
            ),
            variable_id = "tasmin,tasmax",
            epw_field = "dry_bulb_temperature",
            action = "Inspect sobie_curry_temperature_dtr_status."
        )
    }
    if (identical(policy, "paper_faithful")) {
        dew_fallback <- factors[["dew_point_sd_status"]] ==
            "inherited_zero_historical"
        if (any(dew_fallback)) {
            diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                stage = "runtime",
                severity = "warning",
                code = "sobie_curry_zero_historical_dew_sd",
                message = sprintf(
                    "Dew-point anomaly change was inherited for %d target day(s) with zero historical variability.",
                    sum(dew_fallback)
                ),
                variable_id = "tas,huss,ps",
                epw_field = "dew_point_temperature",
                action = "Inspect sobie_curry_dew_point_sd_status."
            )
        }

        # The shared executor owns the row-level thermodynamic checks. Sobie-
        # Curry keeps its paper-specific warning while consuming their union.
        invalid <- sum(
            physical@state$inconsistency$thermodynamic
        )
        if (invalid > 0L) {
            diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                stage = "runtime",
                severity = "warning",
                code = "sobie_curry_independent_state_not_closed",
                message = sprintf(
                    "Published independent transforms produced %d hourly row(s) outside shared thermodynamic closure.",
                    invalid
                ),
                epw_field = paste(
                    "dry_bulb_temperature,dew_point_temperature,",
                    "relative_humidity,atmospheric_pressure",
                    sep = ""
                ),
                action = paste(
                    "Treat this result as paper-faithful output;",
                    "a harmonized closure policy is not applied."
                )
            )
        }
    } else {
        clipped <- physical@corrections$specific_humidity_clipped
        if (clipped > 0L) {
            diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                stage = "runtime",
                severity = "warning",
                code = "sobie_curry_humidity_clipped",
                message = sprintf(
                    "Harmonized HUSS closure clipped %d hourly target(s) to a physical bound.",
                    clipped
                ),
                variable_id = "huss",
                epw_field = "dew_point_temperature,relative_humidity",
                action = "Inspect sobie_curry_humidity_closure_status."
            )
        }
    }

    settings <- list(
        window_days = data$options$window_days,
        dew_point_sd_factor = "sigma_future / sigma_historical - 1",
        physical_policy = if (identical(policy, "paper_faithful")) {
            "independent_paper_transforms"
        } else {
            "specific_humidity_delta_closure"
        }
    )
    if (identical(policy, "harmonized")) {
        settings$humidity_signal <-
            "smoothed future - historical daily specific humidity"
    }

    list(
        epw = baseline$epw,
        weather = weather,
        factors = factors,
        diagnostics = morpher__bind_diagnostics(diagnostics),
        settings = settings
    )
}
# }}}

# Execute the Sobie-Curry daily thermodynamic workflow directly on a canonical
# morphing context and return a standard epw_morph_result.
# sobie__run {{{
sobie__run <- function(context, backend = NULL) {
    morpher__validate_context(context)
    options <- sobie__backend_options(context$recipe$options)
    inputs <- context$inputs
    baseline <- temperature__epw_template(
        weather__get_input(inputs, "weather_template")@source
    )
    future <- sobie__daily_statistics_set(
        sobie__climate(
            weather__get_input(inputs, "model_future")@source,
            "future climate"
        )
    )
    historical <- sobie__daily_statistics_set(
        sobie__climate(
            weather__get_input(inputs, "model_historical")@source,
            "historical climate"
        )
    )
    factors <- sobie__signal_factors(
        future,
        historical,
        sobie__smoothing_windows(options$window_days),
        options$tolerance
    )
    hourly <- sobie__hourly_reconstruct(
        list(baseline = baseline, factors = factors, options = options)
    )
    closed <- sobie__physics_apply(hourly, context = context)
    epw_morph_result(
        context,
        epw = closed$epw,
        data = closed$weather,
        parts = list(
            factors = closed$factors,
            settings = closed$settings
        ),
        diagnostics = closed$diagnostics,
        factors = closed$factors
    )
}
# }}}

# vim: fdm=marker :
