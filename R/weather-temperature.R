# Shared temperature workflow support {{{

# Common numerical and EPW-header controls are owned by the temperature
# workflow boundary rather than by any one daily or monthly backend.
EPW_MORPH_TEMPERATURE_OPTIONS <- list(
    tolerance = 1e-8,
    ground_temperatures = "recalculate",
    typical_extreme_periods = "recalculate",
    design_conditions = "drop"
)

# Validate and complete one backend's JSON-safe temperature options while
# allowing the backend to supply only the settings its method actually owns.
temperature__backend_options <- function(
    options,
    defaults,
    label,
    unknown_label = label
) {
    checkmate::assert_list(defaults, names = "unique")
    checkmate::assert_string(label, min.chars = 1L)
    checkmate::assert_string(unknown_label, min.chars = 1L)
    if (is.null(options)) {
        options <- defaults
    } else {
        if (!is.list(options) || is.null(names(options)) ||
            any(!nzchar(names(options)))) {
            cli::cli_abort("{label} `options` must be a named list.")
        }
        unknown <- setdiff(names(options), names(defaults))
        if (length(unknown)) {
            cli::cli_abort(
                "Unknown {unknown_label} option(s): {.val {unknown}}."
            )
        }
        options <- utils::modifyList(defaults, options)
    }

    # A backend opts into the common circular-window contract by declaring the
    # setting in its own defaults; monthly methods do not receive a fake value.
    if ("window_days" %in% names(defaults)) {
        window <- daily__window_spec(
            options$window_days,
            target_year_days = 365L
        )
        options$window_days <- window$window_days
    }
    if ("tolerance" %in% names(defaults)) {
        checkmate::assert_number(
            options$tolerance,
            lower = 0,
            finite = TRUE
        )
        options$tolerance <- as.numeric(options$tolerance)
    }

    policies <- list(
        ground_temperatures = c("recalculate", "preserve"),
        typical_extreme_periods = c("recalculate", "preserve"),
        design_conditions = c("drop", "preserve")
    )
    for (name in intersect(names(policies), names(defaults))) {
        value <- options[[name]]
        checkmate::assert_string(value, min.chars = 1L)
        value <- tolower(value)
        if (!value %in% policies[[name]]) {
            cli::cli_abort(
                "Unsupported {unknown_label} option value {.val {value}} for {.field {name}}."
            )
        }
        options[[name]] <- value
    }

    options
}

# Normalize the three role-addressable inputs shared by daily-source
# temperature backends after each method has resolved its own option contract.
temperature__preprocess_inputs <- function(inputs, options) {
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    future <- weather__get_input(inputs, "model_future")
    historical <- weather__get_input(inputs, "model_historical")
    template <- weather__get_input(inputs, "weather_template")

    # Component and recipe contracts guarantee the roles exist; keeping all
    # normalization here gives every backend the same table and unit boundary.
    list(
        baseline = temperature__epw_template(template@source),
        future = temperature__daily_climate(
            future@source,
            "future climate"
        ),
        historical = temperature__daily_climate(
            historical@source,
            "historical climate"
        ),
        options = options
    )
}

# Convert mixed supported temperature units to degrees Celsius through the
# package-wide checked unit converter after a caller validates source metadata.
temperature__to_celsius <- function(value, units) {
    value <- as.numeric(value)
    if (length(units) == 1L) {
        units <- rep.int(units, length(value))
    }
    checkmate::assert_character(
        units,
        len = length(value),
        any.missing = FALSE
    )
    aliases <- vapply(units, morpher__unit_alias, character(1L))
    converted <- value
    for (unit in unique(aliases)) {
        index <- aliases == unit
        result <- morpher__convert_value_checked(
            value[index],
            unit,
            "degC"
        )
        if (!isTRUE(result$ok)) {
            cli::cli_abort(result$message)
        }
        converted[index] <- result$value
    }
    converted
}

# Convert extracted daily temperature rows to degrees Celsius and reject inputs
# that cannot satisfy the shared daily-frequency temperature contract.
temperature__daily_climate <- function(data, name) {
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
    value <- temperature__to_celsius(out[["value"]], units)
    data.table::set(out, j = "value", value = value)
    data.table::set(out, j = "units", value = rep.int("degC", nrow(out)))
    out[]
}

# Map a complete non-leap EPW year to the shared 365-day temperature grid while
# retaining a stable row index for reconstruction after grouped projection.
temperature__epw_template <- function(epw) {
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

    month_days <- c(
        31L, 28L, 31L, 30L, 31L, 30L,
        31L, 31L, 30L, 31L, 30L, 31L
    )
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

# }}}
