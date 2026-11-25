# Shared temperature workflow support
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
# temperature__backend_options {{{
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
        if (
            !is.list(options) ||
                is.null(names(options)) ||
                any(!nzchar(names(options)))
        ) {
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
# }}}

# Normalize the three role-addressable inputs shared by daily-source
# temperature backends after each method has resolved its own option contract.
# temperature__preprocess_inputs {{{
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
# }}}

# Convert mixed supported temperature units to degrees Celsius through the
# package-wide checked unit converter after a caller validates source metadata.
# temperature__to_celsius {{{
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
# }}}

# Validate required columns, CMIP frequency, required variables, and units on a
# daily or monthly climate source, converting each variable to its target unit.
# weather__normalize_climate_units {{{
weather__normalize_climate_units <- function(
    data,
    name,
    expected_frequency,
    target_units,
    required_cols = c("variable_id", "value", "units", "frequency"),
    required_variables = character(),
    single_unit = FALSE
) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_string(expected_frequency, min.chars = 1L)
    checkmate::assert_character(
        target_units,
        min.len = 1L,
        any.missing = FALSE,
        names = "unique"
    )
    checkmate::assert_character(
        required_cols,
        min.len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_character(required_variables, any.missing = FALSE)
    checkmate::assert_flag(single_unit)

    missing <- setdiff(required_cols, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg {name}} is missing climate column{?s}: {.val {missing}}."
        )
    }

    frequencies <- unique(tolower(as.character(data[["frequency"]])))
    frequencies <- frequencies[!is.na(frequencies) & nzchar(frequencies)]
    if (!identical(frequencies, tolower(expected_frequency))) {
        shown <- if (length(frequencies)) frequencies else "<missing>"
        cli::cli_abort(
            "{.arg {name}} must use CMIP frequency {.val {expected_frequency}}; found {.val {shown}}."
        )
    }

    variables <- names(target_units)
    out <- data.table::as.data.table(data.table::copy(data))[
        variable_id %in% variables
    ]
    present <- unique(as.character(out[["variable_id"]]))
    missing_variables <- setdiff(required_variables, present)
    if (length(missing_variables)) {
        cli::cli_abort(
            "{.arg {name}} is missing required variable{?s}: {.val {missing_variables}}."
        )
    }
    if (!nrow(out)) {
        cli::cli_abort(
            "{.arg {name}} does not contain required climate variables."
        )
    }

    data.table::set(out, j = "value", value = as.numeric(out[["value"]]))
    data.table::set(out, j = "units", value = as.character(out[["units"]]))
    for (variable in intersect(variables, present)) {
        rows <- which(out[["variable_id"]] == variable)
        aliases <- vapply(
            out[["units"]][rows],
            morpher__unit_alias,
            character(1L)
        )
        unique_aliases <- unique(aliases)
        if (
            isTRUE(single_unit) &&
                (length(unique_aliases) != 1L || anyNA(unique_aliases))
        ) {
            cli::cli_abort(
                "{.arg {name}} variable {.val {variable}} must use one supported unit."
            )
        }
        if (anyNA(unique_aliases)) {
            cli::cli_abort(
                "{.arg {name}} variable {.val {variable}} contains an unsupported unit."
            )
        }
        target <- target_units[[variable]]
        values <- as.numeric(out[["value"]][rows])
        for (unit in unique_aliases) {
            idx <- which(aliases == unit)
            if (
                identical(variable, "huss") &&
                    unit %in% c("1", "kg/kg") &&
                    target %in% c("1", "kg/kg")
            ) {
                next
            }
            converted <- morpher__convert_value_checked(
                values[idx],
                unit,
                target
            )
            if (!isTRUE(converted$ok)) {
                cli::cli_abort(converted$message)
            }
            values[idx] <- converted$value
        }
        data.table::set(out, i = rows, j = "value", value = values)
        data.table::set(
            out,
            i = rows,
            j = "units",
            value = rep.int(target, length(rows))
        )
    }
    out[]
}
# }}}

# Convert extracted daily temperature rows to degrees Celsius and reject inputs
# that cannot satisfy the shared daily-frequency temperature contract.
# temperature__daily_climate {{{
temperature__daily_climate <- function(data, name) {
    weather__normalize_climate_units(
        data,
        name = name,
        expected_frequency = "day",
        target_units = c(tas = "degC", tasmin = "degC", tasmax = "degC"),
        required_cols = c(
            "variable_id",
            "annual_phase",
            "value",
            "units",
            "frequency"
        )
    )
}
# }}}

# Apply either paper-faithful humidity preservation or shared specific-humidity
# closure to a projected temperature series, attaching method-prefixed columns
# and policy diagnostics shared by temperature-only methods.
# temperature__policy_physics_apply {{{
temperature__policy_physics_apply <- function(
    baseline,
    temperature,
    policy_recipe,
    adapter,
    prefix,
    label,
    hourly_cols,
    inconsistent_code,
    inconsistent_message,
    extra_diagnostics = list()
) {
    policy <- policy_recipe$policy
    checkmate::assert_choice(policy, c("paper_faithful", "harmonized"))
    physical <- epwphys__apply_temperature(
        template = baseline$weather,
        temperature = temperature,
        policy = epwphys__recipe_policy(policy_recipe),
        adapter = adapter
    )
    weather <- data.table::copy(physical@weather)
    moisture <- physical@state$humidity

    diagnostic_values <- hourly_cols
    if (identical(policy, "harmonized")) {
        harmonized_cols <- list(
            moisture$baseline_specific_humidity,
            moisture$specific_humidity,
            moisture$status
        )
        names(harmonized_cols) <- paste0(
            prefix,
            "_",
            c(
                "baseline_specific_humidity",
                "specific_humidity",
                "humidity_closure_status"
            )
        )
        diagnostic_values <- c(diagnostic_values, harmonized_cols)
    }
    data.table::set(
        weather,
        j = names(diagnostic_values),
        value = diagnostic_values
    )

    diagnostics <- extra_diagnostics
    if (identical(policy, "paper_faithful")) {
        invalid <- physical@corrections$humidity_inconsistent
        if (invalid > 0L) {
            diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                stage = "runtime",
                severity = "warning",
                code = inconsistent_code,
                message = sprintf(inconsistent_message, invalid),
                epw_field = paste(
                    "dry_bulb_temperature,dew_point_temperature,",
                    "relative_humidity",
                    sep = ""
                ),
                action = paste(
                    "Treat this as paper-faithful temperature-only",
                    "output or select the harmonized policy."
                )
            )
        }
    } else {
        clipped <- physical@corrections$humidity_saturation_clipped
        if (clipped > 0L) {
            diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                stage = "runtime",
                severity = "info",
                code = paste0(prefix, "_humidity_saturation_clipped"),
                message = sprintf(
                    "%s harmonized closure clipped %d hourly moisture state(s) to saturation.",
                    label,
                    clipped
                ),
                epw_field = "dew_point_temperature,relative_humidity",
                action = sprintf(
                    "Inspect %s_humidity_closure_status.",
                    prefix
                )
            )
        }
    }

    list(
        epw = baseline$epw,
        weather = weather,
        diagnostics = morpher__bind_diagnostics(diagnostics),
        physical_policy = if (identical(policy, "paper_faithful")) {
            "preserve_baseline_humidity_fields"
        } else {
            "specific_humidity_closure"
        }
    )
}
# }}}

# Map a complete non-leap EPW year to the shared 365-day temperature grid while
# retaining a stable row index for reconstruction after grouped projection.
# temperature__epw_template {{{
temperature__epw_template <- function(epw) {
    if (!inherits(epw, "EpwFile")) {
        cli::cli_abort("`epw` must be an internal {.cls EpwFile} object.")
    }
    epw <- epw$clone()
    suppressMessages(epw$drop_unit())
    # Daily, bounded, and distribution-based backends share this template;
    # none may interpret an EPW numeric missing sentinel as observed weather.
    weather <- epw_file__calculation_weather(epw$data())
    required <- c(
        "month",
        "day",
        "hour",
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature",
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
        31L,
        28L,
        31L,
        30L,
        31L,
        30L,
        31L,
        31L,
        30L,
        31L,
        30L,
        31L
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
    shape <- template[,
        .(
            rows = .N,
            unique_hours = data.table::uniqueN(hour)
        ),
        by = "target_day"
    ]
    if (
        nrow(shape) != 365L ||
            any(shape[["rows"]] != 24L | shape[["unique_hours"]] != 24L)
    ) {
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

# vim: fdm=marker :
