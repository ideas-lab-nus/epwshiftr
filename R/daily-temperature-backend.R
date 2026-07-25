# Daily temperature backend {{{

# The backend owns only temperature and its humidity-state post-process. Other
# EPW fields remain on the baseline hourly sequence until their daily methods
# are implemented independently.
EPW_MORPH_DAILY_TEMPERATURE_METHODS <- c(tdb = "constrained")

# A required tas series defines daily mean changes. Paired tasmin and tasmax are
# optional inputs that activate the constrained daily-range projection.
EPW_MORPH_DAILY_TEMPERATURE_RULES <- data.table::data.table(
    step = c("tdb", "rh", "tdew"),
    epw_field = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature"
    ),
    variable_id = c("tas", NA_character_, NA_character_),
    optional_variable_id = c(
        "tasmin,tasmax",
        NA_character_,
        NA_character_
    ),
    method = c("constrained", "derived", "derived"),
    required = c(TRUE, FALSE, FALSE),
    derived = c(FALSE, TRUE, TRUE),
    method_choices = list("constrained", "derived", "derived")
)

# These defaults keep the numerical projection reproducible and reuse the
# existing EPW header post-process after the hourly temperature year is built.
EPW_MORPH_DAILY_TEMPERATURE_OPTIONS <- list(
    window_days = 31L,
    tolerance = 1e-8,
    ground_temperatures = "recalculate",
    typical_extreme_periods = "recalculate",
    design_conditions = "drop"
)

# Validate and complete the JSON-safe options used by foreground, background,
# and resumed daily temperature recipes.
daily__temperature_backend_options <- function(options = NULL) {
    defaults <- EPW_MORPH_DAILY_TEMPERATURE_OPTIONS
    if (is.null(options)) {
        options <- defaults
    } else {
        if (!is.list(options) || is.null(names(options)) ||
            any(!nzchar(names(options)))) {
            cli::cli_abort(
                "Daily temperature `options` must be a named list."
            )
        }
        unknown <- setdiff(names(options), names(defaults))
        if (length(unknown)) {
            cli::cli_abort(
                "Unknown daily temperature option(s): {.val {unknown}}."
            )
        }
        options <- utils::modifyList(defaults, options)
    }

    # Reuse the climatology window validator so recipe construction and runtime
    # projection cannot interpret an even or oversized window differently.
    window <- daily__window_spec(
        options$window_days,
        target_year_days = 365L
    )
    options$window_days <- window$window_days
    checkmate::assert_number(
        options$tolerance,
        lower = 0,
        finite = TRUE
    )
    options$tolerance <- as.numeric(options$tolerance)

    policies <- list(
        ground_temperatures = c("recalculate", "preserve"),
        typical_extreme_periods = c("recalculate", "preserve"),
        design_conditions = c("drop", "preserve")
    )
    for (name in names(policies)) {
        value <- options[[name]]
        checkmate::assert_string(value, min.chars = 1L)
        value <- tolower(value)
        if (!value %in% policies[[name]]) {
            cli::cli_abort(
                "Unsupported daily temperature option value {.val {value}} for {.field {name}}."
            )
        }
        options[[name]] <- value
    }

    options
}

# Convert extracted daily temperature rows to degrees Celsius and reject inputs
# that cannot satisfy the backend's explicit daily-frequency contract.
daily__temperature_backend_climate <- function(data, name) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(name, min.chars = 1L)
    required <- c("variable_id", "annual_phase", "value", "units", "frequency")
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg {name}} is missing daily temperature column{?s}: {.val {missing}}."
        )
    }

    frequencies <- unique(tolower(as.character(data[["frequency"]])))
    frequencies <- frequencies[
        !is.na(frequencies) & nzchar(frequencies)
    ]
    if (!identical(frequencies, "day")) {
        shown <- if (length(frequencies)) frequencies else "<missing>"
        cli::cli_abort(
            "{.arg {name}} must use CMIP frequency {.val day}; found {.val {shown}}."
        )
    }

    keep <- data[["variable_id"]] %in% c("tas", "tasmin", "tasmax")
    out <- data.table::as.data.table(data.table::copy(data))[keep]
    if (!nrow(out)) {
        cli::cli_abort(
            "{.arg {name}} does not contain daily temperature variables."
        )
    }

    units <- vapply(out[["units"]], morpher__unit_alias, character(1L))
    unknown <- unique(units[is.na(units) | !units %in% c("K", "degC")])
    if (length(unknown)) {
        cli::cli_abort(
            "{.arg {name}} contains unsupported temperature unit(s): {.val {unknown}}."
        )
    }
    value <- as.numeric(out[["value"]])
    value[units == "K"] <- value[units == "K"] - 273.15
    data.table::set(out, j = "value", value = value)
    data.table::set(out, j = "units", value = rep.int("degC", nrow(out)))
    out[]
}

# Map a complete non-leap EPW year to the backend's 365-day target grid while
# retaining a stable row index for reconstruction after grouped projection.
daily__temperature_epw_template <- function(epw) {
    if (!inherits(epw, "EpwFile")) {
        cli::cli_abort("`epw` must be an internal {.cls EpwFile} object.")
    }
    epw <- epw$clone()
    suppressMessages(epw$drop_unit())
    weather <- data.table::as.data.table(data.table::copy(epw$data()))
    required <- c(
        "month", "day", "hour", "dry_bulb_temperature",
        "relative_humidity", "dew_point_temperature",
        "atmospheric_pressure"
    )
    missing <- setdiff(required, names(weather))
    if (length(missing)) {
        cli::cli_abort(
            "Baseline EPW is missing daily temperature field{?s}: {.val {missing}}."
        )
    }
    if (nrow(weather) != 8760L) {
        cli::cli_abort(
            "Daily temperature projection requires a complete 8760-hour EPW year."
        )
    }

    month_days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    month <- as.integer(weather[["month"]])
    day <- as.integer(weather[["day"]])
    valid_date <- !is.na(month) & month >= 1L & month <= 12L
    valid_date <- valid_date & !is.na(day)
    valid_date[valid_date] <- day[valid_date] >= 1L &
        day[valid_date] <= month_days[month[valid_date]]
    if (!all(valid_date)) {
        cli::cli_abort(
            "Daily temperature projection requires valid non-leap EPW month/day fields."
        )
    }

    month_start <- c(0L, cumsum(month_days)[-length(month_days)])
    target_day <- month_start[month] + day
    template <- data.table::data.table(
        target_day = as.integer(target_day),
        hour = as.numeric(weather[["hour"]]),
        dry_bulb_temperature = as.numeric(
            weather[["dry_bulb_temperature"]]
        )
    )
    shape <- template[, .(
        rows = .N,
        unique_hours = data.table::uniqueN(hour)
    ), by = "target_day"]
    if (nrow(shape) != 365L ||
        any(shape[["rows"]] != 24L | shape[["unique_hours"]] != 24L)) {
        cli::cli_abort(
            "Daily temperature projection requires 365 days with exactly 24 unique hourly rows each."
        )
    }
    if (any(!is.finite(template[["dry_bulb_temperature"]]))) {
        cli::cli_abort(
            "Baseline EPW dry-bulb temperature must be finite for all 8760 hours."
        )
    }

    list(epw = epw, weather = weather, template = template)
}

# Preserve baseline specific humidity after changing dry-bulb temperature,
# clipping only when the inherited moisture would exceed the new saturation
# state, then derive physically consistent RH and dew point.
daily__temperature_moisture <- function(weather, temperature) {
    temperature <- as.numeric(temperature)
    baseline_temperature <- as.numeric(weather[["dry_bulb_temperature"]])
    baseline_rh <- as.numeric(weather[["relative_humidity"]])
    pressure <- as.numeric(weather[["atmospheric_pressure"]])
    valid <- is.finite(baseline_temperature) &
        is.finite(baseline_rh) & baseline_rh >= 0 & baseline_rh <= 100 &
        is.finite(pressure) & pressure > 0 &
        is.finite(temperature)

    baseline_huss <- rep.int(NA_real_, nrow(weather))
    future_huss <- rep.int(NA_real_, nrow(weather))
    future_rh <- as.numeric(weather[["relative_humidity"]])
    future_dew <- as.numeric(weather[["dew_point_temperature"]])
    status <- rep.int("missing_baseline_state", nrow(weather))
    if (any(valid)) {
        baseline_huss[valid] <- morpher__huss_from_rh_si(
            baseline_temperature[valid],
            baseline_rh[valid],
            pressure[valid]
        )
        saturation_huss <- morpher__saturation_huss_si(
            temperature[valid],
            pressure[valid]
        )
        future_huss[valid] <- pmin(
            baseline_huss[valid],
            saturation_huss
        )
        future_rh[valid] <- morpher__hurs_from_huss_si(
            future_huss[valid],
            temperature[valid] + 273.15,
            pressure[valid]
        )
        future_rh[valid] <- pmin(100, pmax(0, future_rh[valid]))
        future_dew[valid] <- morpher__dew_point_from_rh(
            temperature[valid],
            pmax(future_rh[valid], .Machine$double.eps) / 100
        )
        future_dew[valid] <- pmin(future_dew[valid], temperature[valid])
        clipped <- baseline_huss[valid] - future_huss[valid] > 1e-12
        status[valid] <- ifelse(
            clipped,
            "saturation_clipped",
            "inherited"
        )
    }

    list(
        relative_humidity = future_rh,
        dew_point_temperature = future_dew,
        baseline_specific_humidity = baseline_huss,
        specific_humidity = future_huss,
        status = status
    )
}

# Reduce hourly projection output to one auditable row per target day, including
# numerical closure and cyclic boundary changes.
daily__temperature_factor_rows <- function(targets, projected) {
    projection_columns <- c(
        "dry_bulb_temperature", "target_mean", "target_minimum",
        "target_maximum", "projected_mean", "projected_minimum",
        "projected_maximum", "dtr_status", "projection_status",
        "shape_exponent", "boundary_jump", "boundary_jump_change"
    )
    # Explicit .SD access keeps package checks free from data.table NSE notes
    # while preserving one diagnostic value for every projected target day.
    daily_projection <- projected[, {
        list(
            baseline_mean = mean(.SD[["dry_bulb_temperature"]]),
            baseline_minimum = min(.SD[["dry_bulb_temperature"]]),
            baseline_maximum = max(.SD[["dry_bulb_temperature"]]),
            target_mean = unique(.SD[["target_mean"]]),
            target_minimum = unique(.SD[["target_minimum"]]),
            target_maximum = unique(.SD[["target_maximum"]]),
            projected_mean = unique(.SD[["projected_mean"]]),
            projected_minimum = unique(.SD[["projected_minimum"]]),
            projected_maximum = unique(.SD[["projected_maximum"]]),
            dtr_status = unique(.SD[["dtr_status"]]),
            projection_status = unique(.SD[["projection_status"]]),
            shape_exponent = unique(.SD[["shape_exponent"]]),
            boundary_jump = unique(.SD[["boundary_jump"]]),
            boundary_jump_change = unique(.SD[["boundary_jump_change"]])
        )
    }, by = "target_day", .SDcols = projection_columns]
    factors <- merge(
        data.table::copy(targets),
        daily_projection,
        by = c("target_day", "dtr_status"),
        all.x = TRUE,
        sort = FALSE
    )
    data.table::set(
        factors,
        j = "mean_closure_error",
        value = factors[["projected_mean"]] - factors[["target_mean"]]
    )
    data.table::set(
        factors,
        j = "minimum_closure_error",
        value = factors[["projected_minimum"]] - factors[["target_minimum"]]
    )
    data.table::set(
        factors,
        j = "maximum_closure_error",
        value = factors[["projected_maximum"]] - factors[["target_maximum"]]
    )
    data.table::setorderv(factors, "target_day")
    factors[]
}

# Normalize the three role-addressable sources before calendar mapping. This
# stage is the only daily-temperature component that interprets raw source
# representations and units.
daily__temperature_preprocess_apply <- function(
    inputs,
    context,
    options
) {
    morpher__validate_context(context)
    options <- daily__temperature_backend_options(options)
    future <- weather__get_input(inputs, "model_future")
    historical <- weather__get_input(inputs, "model_historical")
    template <- weather__get_input(inputs, "weather_template")
    list(
        baseline = daily__temperature_epw_template(template@source),
        future = daily__temperature_backend_climate(
            future@source,
            "future climate"
        ),
        historical = daily__temperature_backend_climate(
            historical@source,
            "historical climate"
        ),
        options = options
    )
}

# Map future and historical daily sources onto the common 365-day phase grid,
# then build the aligned role payload consumed by the signal kernel.
daily__temperature_calendar_apply <- function(
    data,
    inputs,
    context,
    options
) {
    future <- daily__temperature_source(
        data$future,
        "future climate",
        character()
    )
    historical <- daily__temperature_source(
        data$historical,
        "historical climate",
        character()
    )
    future_climatology <- daily__temperature_climatology(
        future,
        character(),
        data$options$window_days,
        365L
    )
    historical_climatology <- daily__temperature_climatology(
        historical,
        character(),
        data$options$window_days,
        365L
    )
    list(signal__group(
        inputs = list(
            weather_template = data$baseline,
            model_historical = historical_climatology,
            model_future = future_climatology
        ),
        variables = "tas"
    ))
}

# Calculate future-minus-historical daily mean and range changes from calendar-
# aligned climatologies. Calendar interpretation is intentionally absent here.
daily__temperature_signal_apply_group <- function(
    inputs,
    settings,
    key
) {
    list(
        baseline = inputs$weather_template,
        targets = daily__temperature_target_changes(
            inputs$model_future,
            inputs$model_historical
        )
    )
}

# Preserve the baseline EPW day order explicitly. Future sequence-generation
# methods can replace this component without changing the signal or hourly code.
daily__temperature_sequence_generate <- function(
    data,
    inputs,
    context,
    options
) {
    if (!S7::S7_inherits(data, SignalExecutionResult) ||
        length(data@values) != 1L ||
        is.null(data@values[[1L]])) {
        cli::cli_abort(
            "Daily temperature sequence input must contain one successful signal group."
        )
    }
    data@values[[1L]]
}

# Apply the constrained 24-hour projection to the preserved EPW sequence and
# retain hourly and daily closure values for the later physics stage.
daily__temperature_hourly_reconstruct <- function(
    data,
    inputs,
    context,
    options
) {
    options <- daily__temperature_backend_options(options)
    baseline <- data$baseline
    targets <- data$targets
    projected <- daily__project_temperature(
        baseline$template,
        targets,
        value = "dry_bulb_temperature",
        day = "target_day",
        hour = "hour",
        tolerance = options$tolerance
    )
    # The kernel guarantees caller-order output, so this private row key can be
    # added after projection without colliding with its own working column.
    data.table::set(
        projected,
        j = ".daily_row",
        value = seq_len(nrow(projected))
    )
    factors <- daily__temperature_factor_rows(targets, projected)

    # Join target deltas back to every hourly row before physical closure.
    target_columns <- c(
        "target_day", "annual_phase", "mean_delta", "minimum_delta",
        "maximum_delta", "dtr_delta"
    )
    hourly <- merge(
        projected,
        targets[, target_columns, with = FALSE],
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setorderv(hourly, ".daily_row")
    list(
        baseline = baseline,
        targets = targets,
        projected = projected,
        factors = factors,
        hourly = hourly
    )
}

# Close relative humidity and dew point against the projected dry-bulb
# temperature, retaining the existing EPW moisture state when it is feasible.
daily__temperature_physics_apply <- function(
    data,
    inputs,
    context,
    options
) {
    baseline <- data$baseline
    hourly <- data$hourly
    factors <- data$factors
    moisture <- daily__temperature_moisture(
        baseline$weather,
        hourly[["temperature_projected"]]
    )

    weather <- data.table::copy(baseline$weather)
    data.table::set(
        weather,
        j = "dry_bulb_temperature",
        value = hourly[["temperature_projected"]]
    )
    data.table::set(
        weather,
        j = "relative_humidity",
        value = moisture$relative_humidity
    )
    data.table::set(
        weather,
        j = "dew_point_temperature",
        value = moisture$dew_point_temperature
    )

    diagnostic_values <- list(
        daily_target_day = hourly[["target_day"]],
        daily_annual_phase = hourly[["annual_phase"]],
        daily_temperature_mean_delta = hourly[["mean_delta"]],
        daily_temperature_minimum_delta = hourly[["minimum_delta"]],
        daily_temperature_maximum_delta = hourly[["maximum_delta"]],
        daily_temperature_dtr_delta = hourly[["dtr_delta"]],
        daily_temperature_dtr_status = hourly[["dtr_status"]],
        daily_temperature_projection_status = hourly[["projection_status"]],
        daily_temperature_shape_exponent = hourly[["shape_exponent"]],
        daily_temperature_target_mean = hourly[["target_mean"]],
        daily_temperature_target_minimum = hourly[["target_minimum"]],
        daily_temperature_target_maximum = hourly[["target_maximum"]],
        daily_temperature_projected_mean = hourly[["projected_mean"]],
        daily_temperature_projected_minimum = hourly[["projected_minimum"]],
        daily_temperature_projected_maximum = hourly[["projected_maximum"]],
        daily_temperature_boundary_jump = hourly[["boundary_jump"]],
        daily_temperature_boundary_jump_change =
            hourly[["boundary_jump_change"]],
        daily_temperature_baseline_specific_humidity =
            moisture$baseline_specific_humidity,
        daily_temperature_specific_humidity = moisture$specific_humidity,
        daily_temperature_moisture_status = moisture$status
    )
    for (name in names(diagnostic_values)) {
        data.table::set(weather, j = name, value = diagnostic_values[[name]])
    }

    diagnostics <- list()
    if (any(factors[["dtr_status"]] == "inherited_missing_extremes")) {
        diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "warning",
            code = "daily_temperature_dtr_inherited",
            message = paste(
                "Daily temperature range was inherited from the baseline EPW",
                "because paired future and historical tasmin/tasmax were unavailable."
            ),
            variable_id = "tasmin,tasmax",
            epw_field = "dry_bulb_temperature",
            action = paste(
                "Provide paired daily tasmin and tasmax for both future and",
                "historical periods to adjust the daily temperature range."
            )
        )
    }
    clipped <- sum(moisture$status == "saturation_clipped")
    if (clipped > 0L) {
        diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "info",
            code = "daily_temperature_moisture_saturation_clipped",
            message = sprintf(
                "Inherited baseline moisture was clipped to saturation for %d hourly row(s).",
                clipped
            ),
            epw_field = "relative_humidity",
            action = "Inspect daily_temperature_moisture_status in the morphed data artifact."
        )
    }

    list(
        epw = baseline$epw,
        weather = weather,
        projected = data$projected,
        factors = factors,
        diagnostics = morpher__bind_diagnostics(diagnostics)
    )
}

# Assemble the physics-closed hourly data into the existing backend result
# contract. EpwMorpher remains responsible for Parquet and EPW file writes.
daily__temperature_output_write <- function(
    data,
    inputs,
    context,
    options,
    stages
) {
    epw_morph_result(
        context,
        epw = data$epw,
        data = data$weather,
        parts = list(
            temperature = data$projected,
            daily_targets = data$factors
        ),
        diagnostics = data$diagnostics,
        factors = data$factors
    )
}

# Build all seven executable component specifications for the daily temperature
# method. Stable names are persisted separately from these process-local
# functions.
daily__temperature_component_specs <- function() {
    template <- component__input_requirement(
        "weather_template",
        representations = "epw",
        frequencies = "hour",
        calendars = "gregorian"
    )
    historical <- component__input_requirement(
        "model_historical",
        representations = "series",
        frequencies = "day",
        variable_sets = "tas"
    )
    future <- component__input_requirement(
        "model_future",
        representations = "series",
        frequencies = "day",
        variable_sets = "tas"
    )
    complete_inputs <- list(
        weather_template = template,
        model_historical = historical,
        model_future = future
    )

    list(
        preprocess = component__spec(
            name = "daily_temperature_inputs",
            stage = "preprocess",
            label = "Daily temperature input normalization",
            required_inputs = complete_inputs,
            input_kinds = "role_inputs",
            output_kinds = "daily_temperature_preprocessed",
            scopes = "multivariate",
            operations = list(
                apply = daily__temperature_preprocess_apply
            )
        ),
        calendar = component__spec(
            name = "daily_temperature_calendar",
            stage = "calendar",
            label = "Calendar-neutral daily temperature climatology",
            required_inputs = complete_inputs,
            input_kinds = "daily_temperature_preprocessed",
            output_kinds = "calendar_indexed_temperature",
            scopes = "multivariate",
            operations = list(
                apply = daily__temperature_calendar_apply
            )
        ),
        signal = signal__component(
            name = "daily_temperature_delta",
            label = "Daily temperature delta change",
            required_inputs = complete_inputs,
            input_kinds = "calendar_indexed_temperature",
            output_kinds = "daily_temperature_targets",
            scopes = "multivariate",
            profiles = list(signal__variable_profile(
                "tas",
                evidence = "published",
                references = paste(
                    "Belcher, Hacker, and Powell (2005),",
                    "Constructing design weather data for future climates"
                )
            )),
            apply_group = daily__temperature_signal_apply_group
        ),
        sequence = component__spec(
            name = "preserve_epw_sequence",
            stage = "sequence",
            label = "Preserve baseline EPW day sequence",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_temperature_targets",
            output_kinds = "daily_temperature_sequence",
            scopes = "multivariate",
            operations = list(
                generate = daily__temperature_sequence_generate
            )
        ),
        hourly = component__spec(
            name = "constrained_daily_temperature",
            stage = "hourly",
            label = "Constrained 24-hour temperature reconstruction",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_temperature_sequence",
            output_kinds = "hourly_temperature_projected",
            scopes = "multivariate",
            operations = list(
                reconstruct = daily__temperature_hourly_reconstruct
            )
        ),
        physics = component__spec(
            name = "specific_humidity_closure",
            stage = "physics",
            label = "Specific-humidity temperature closure",
            required_inputs = list(weather_template = template),
            input_kinds = "hourly_temperature_projected",
            output_kinds = "hourly_weather_closed",
            scopes = "multivariate",
            operations = list(
                apply = daily__temperature_physics_apply
            )
        ),
        output = component__spec(
            name = "daily_temperature_epw_result",
            stage = "output",
            label = "Daily temperature EPW result",
            required_inputs = list(weather_template = template),
            input_kinds = "hourly_weather_closed",
            output_kinds = "epw_morph_result",
            scopes = "multivariate",
            operations = list(
                write = daily__temperature_output_write
            )
        )
    )
}

# Register built-in daily temperature components once without replacing an
# existing implementation under the same stable registry key.
daily__register_temperature_components <- function() {
    components <- daily__temperature_component_specs()
    for (stage in names(components)) {
        key <- component__registry_key(stage, components[[stage]]@name)
        if (!exists(
            key,
            envir = WEATHER_COMPONENT_REGISTRY,
            inherits = FALSE
        )) {
            component__register(components[[stage]])
        }
    }
    invisible(NULL)
}

# Return the stable seven-stage pipeline used by the built-in daily temperature
# backend and serialized with each recipe.
daily__temperature_pipeline <- function() {
    daily__register_temperature_components()
    pipeline__spec(list(
        preprocess = "daily_temperature_inputs",
        calendar = "daily_temperature_calendar",
        signal = "daily_temperature_delta",
        sequence = "preserve_epw_sequence",
        hourly = "constrained_daily_temperature",
        physics = "specific_humidity_closure",
        output = "daily_temperature_epw_result"
    ))
}

# }}}
