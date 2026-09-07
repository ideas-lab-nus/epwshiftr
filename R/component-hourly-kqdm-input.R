#' @include component-hourly-weather-interpolation.R epw-morph-context.R
NULL

# The published workflow bias-adjusts six canonical signals after reconstructing
# seven raw CMIP model variables on a common hourly lattice.
HOURLY_KQDM_SIGNAL_VARIABLES <- c(
    "tas",
    "ps",
    "hurs",
    "sfcWind",
    "rsds",
    "rsdsdiff"
)

HOURLY_KQDM_MODEL_VARIABLES <- c(
    "tas",
    "ps",
    "huss",
    "uas",
    "vas",
    "rsds",
    "rsdsdiff"
)

# CMIP6 stores instantaneous state and wind fields under `3hrPt`, averaged
# radiation fluxes under `3hr`, and optional temperature extrema under `day`.
HOURLY_KQDM_MODEL_FREQUENCIES <- c(
    tas = "3hrPt",
    ps = "3hrPt",
    huss = "3hrPt",
    uas = "3hrPt",
    vas = "3hrPt",
    rsds = "3hr",
    rsdsdiff = "3hr",
    tasmin = "day",
    tasmax = "day"
)

HOURLY_KQDM_CANONICAL_UNITS <- c(
    tas = "K",
    ps = "Pa",
    huss = "1",
    hurs = "%",
    uas = "m/s",
    vas = "m/s",
    sfcWind = "m/s",
    rsds = "W/m^2",
    rsdsdiff = "W/m^2"
)

# Convert every signal or raw-model variable to one method-owned unit before
# roles are aligned, avoiding false mismatches between equivalent unit labels.
hourly_kqdm_input__canonical_units <- function(data, role, expected) {
    data <- data.table::as.data.table(data.table::copy(data))
    present <- sort(unique(as.character(data[["variable_id"]])))
    if (!setequal(present, expected)) {
        missing <- setdiff(expected, present)
        unexpected <- setdiff(present, expected)
        cli::cli_abort(c(
            "Role {.val {role}} does not match the hourly kernel QDM input contract.",
            "i" = "Missing variable(s): {.val {missing}}.",
            "i" = "Unexpected variable(s): {.val {unexpected}}."
        ))
    }
    for (variable in expected) {
        rows <- which(data[["variable_id"]] == variable)
        aliases <- unique(vapply(
            data[["units"]][rows],
            morpher__unit_alias,
            character(1L)
        ))
        if (length(aliases) != 1L || is.na(aliases)) {
            cli::cli_abort(
                "Role {.val {role}} variable {.val {variable}} must use one supported unit."
            )
        }
        target <- HOURLY_KQDM_CANONICAL_UNITS[[variable]]
        if (identical(variable, "huss") && aliases %in% c("1", "kg/kg")) {
            converted <- list(value = data[["value"]][rows], ok = TRUE)
        } else {
            converted <- morpher__convert_value_checked(
                data[["value"]][rows],
                aliases,
                target
            )
        }
        if (!isTRUE(converted$ok)) {
            cli::cli_abort(converted$message)
        }
        data.table::set(data, i = rows, j = "value", value = converted$value)
        data.table::set(data, i = rows, j = "units", value = target)
    }
    data[]
}

# Derive scalar wind and meteorological direction from aligned reconstructed
# eastward and northward model components while preserving source metadata.
hourly_kqdm_input__wind_rows <- function(data, role) {
    required <- c("variable_id", "time", "value", "units")
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "Role {.val {role}} cannot derive wind because column(s) are missing: {.val {missing}}."
        )
    }
    key <- intersect(
        c(
            "source_id", "experiment_id", "variant_label", "frequency",
            "table_id", "grid_label", "site_id", "time"
        ),
        names(data)
    )
    if (!"time" %in% key) {
        cli::cli_abort(
            "Role {.val {role}} cannot derive wind without aligned times."
        )
    }

    # Duplicate rows from overlapping source files are harmless only when
    # their component values agree exactly at the same identity and time.
    prepare <- function(variable) {
        rows <- data[data[["variable_id"]] == variable]
        conflicts <- rows[, list(values = data.table::uniqueN(get("value"))),
            by = key
        ][get("values") > 1L]
        if (nrow(conflicts)) {
            cli::cli_abort(
                "Role {.val {role}} variable {.val {variable}} has conflicting aligned wind values."
            )
        }
        rows[!duplicated(rows, by = key)]
    }
    eastward <- prepare("uas")
    northward <- prepare("vas")[, c(key, "value"), with = FALSE]
    data.table::setnames(northward, "value", ".northward")
    out <- merge(
        eastward,
        northward,
        by = key,
        all.x = TRUE,
        sort = FALSE
    )
    if (nrow(out) != nrow(eastward) || anyNA(out[[".northward"]])) {
        cli::cli_abort(
            "Role {.val {role}} requires fully aligned `uas` and `vas` timestamps."
        )
    }
    vector <- epwphys__wind_from_components(
        out[["value"]],
        out[[".northward"]]
    )
    data.table::set(out, j = "variable_id", value = "sfcWind")
    if ("variable" %in% names(out)) {
        data.table::set(out, j = "variable", value = "sfcWind")
    }
    data.table::set(out, j = "value", value = vector$speed)
    data.table::set(out, j = "units", value = "m/s")
    data.table::set(out, j = "wind_direction", value = vector$direction)
    data.table::set(out, j = "derived_from", value = "uas,vas")
    data.table::set(
        out,
        j = "derivation",
        value = "vector magnitude and meteorological direction"
    )
    out[, ".northward" := NULL]
    out[]
}

# Replace reconstructed model humidity and vector wind inputs with the six
# canonical variables consumed by the published univariate KQDM stage.
hourly_kqdm_input__model_role <- function(input, role) {
    data <- hourly_kqdm_input__canonical_units(
        input@source,
        role,
        HOURLY_KQDM_MODEL_VARIABLES
    )
    humidity <- morpher__derive_hurs_rows(data)
    wind <- hourly_kqdm_input__wind_rows(data, role)
    humidity_raw <- as.numeric(humidity[["value"]])
    humidity_bounded <- pmin(100, pmax(0, humidity_raw))
    data.table::set(humidity, j = "value", value = humidity_bounded)
    retained <- data[
        get("variable_id") %in% c("tas", "ps", "rsds", "rsdsdiff")
    ]
    output <- data.table::rbindlist(
        list(retained, humidity, wind),
        use.names = TRUE,
        fill = TRUE
    )
    order_columns <- unique(c(
        intersect(TEMPORAL_ID_COLUMNS, names(output)),
        "cf_calendar",
        "cf_year",
        "cf_day_of_year",
        "cf_second_of_day"
    ))
    data.table::setorderv(output, order_columns)
    transformed <- weather__new_input(
        role,
        as.data.frame(output, stringsAsFactors = FALSE),
        representation = "series",
        variables = HOURLY_KQDM_SIGNAL_VARIABLES,
        frequencies = "hour",
        calendars = unique(as.character(output[["cf_calendar"]])),
        provenance = utils::modifyList(
            input@provenance,
            list(
                hourly_kqdm_input = list(
                    humidity = "huss_tas_ps_to_hurs",
                    wind = "uas_vas_to_speed_direction"
                )
            )
        ),
        metadata = input@metadata
    )
    list(
        input = transformed,
        diagnostics = data.frame(
            role = role,
            source_variables = paste(HOURLY_KQDM_MODEL_VARIABLES,
                collapse = ","
            ),
            output_variables = paste(HOURLY_KQDM_SIGNAL_VARIABLES,
                collapse = ","
            ),
            humidity_values_bounded = sum(humidity_bounded != humidity_raw),
            wind_directions_derived = nrow(wind),
            stringsAsFactors = FALSE
        )
    )
}

# Normalize the already-hourly observed role to the same canonical signal
# units without applying any model-derived transformation.
hourly_kqdm_input__observed_role <- function(input, role) {
    data <- hourly_kqdm_input__canonical_units(
        input@source,
        role,
        HOURLY_KQDM_SIGNAL_VARIABLES
    )
    list(
        input = weather__new_input(
            role,
            as.data.frame(data, stringsAsFactors = FALSE),
            representation = "series",
            variables = HOURLY_KQDM_SIGNAL_VARIABLES,
            frequencies = "hour",
            calendars = unique(as.character(data[["cf_calendar"]])),
            provenance = input@provenance,
            metadata = input@metadata
        ),
        diagnostics = data.frame(
            role = role,
            source_variables = paste(HOURLY_KQDM_SIGNAL_VARIABLES,
                collapse = ","
            ),
            output_variables = paste(HOURLY_KQDM_SIGNAL_VARIABLES,
                collapse = ","
            ),
            humidity_values_bounded = 0L,
            wind_directions_derived = 0L,
            stringsAsFactors = FALSE
        )
    )
}

# Execute raw-model reconstruction and canonical signal preparation through
# the shared hourly interpolation implementation.
hourly_kqdm_input__apply <- function(inputs, context, options) {
    weather_interp__apply_core(
        inputs,
        context,
        options,
        model_transform = hourly_kqdm_input__model_role,
        observed_transform = hourly_kqdm_input__observed_role,
        component_name = "hourly_kernel_qdm_input_preparation",
        method = "hourly_kernel_qdm_input_preparation",
        extra_provenance = list(
            raw_model_variables = HOURLY_KQDM_MODEL_VARIABLES,
            signal_variables = HOURLY_KQDM_SIGNAL_VARIABLES,
            humidity_derivation = "huss_tas_ps_to_hurs",
            wind_derivation = "uas_vas_to_speed_direction"
        ),
        extra_metadata = list(
            raw_to_signal_contract = TRUE,
            model_wind_direction_retained = TRUE
        )
    )
}

# Describe the method-specific raw-to-signal adapter while reusing the generic
# variable-aware interpolation stage for all temporal reconstruction.
hourly_kqdm_input__component <- function() {
    component__spec(
        name = "hourly_kernel_qdm_input_preparation",
        stage = "preprocess",
        label = "Hourly kernel QDM input preparation",
        required_inputs = list(
            observed_reference = component__input_requirement(
                "observed_reference",
                representations = "series",
                frequencies = "hour",
                calendars = CF_TIME_CALENDARS,
                variable_sets = HOURLY_KQDM_SIGNAL_VARIABLES
            ),
            model_historical = component__input_requirement(
                "model_historical",
                representations = "series",
                frequencies = unique(unname(
                    HOURLY_KQDM_MODEL_FREQUENCIES
                )),
                variable_frequencies = as.list(
                    HOURLY_KQDM_MODEL_FREQUENCIES
                ),
                calendars = CF_TIME_CALENDARS,
                variable_sets = HOURLY_KQDM_MODEL_VARIABLES
            ),
            model_future = component__input_requirement(
                "model_future",
                representations = "series",
                frequencies = unique(unname(
                    HOURLY_KQDM_MODEL_FREQUENCIES
                )),
                variable_frequencies = as.list(
                    HOURLY_KQDM_MODEL_FREQUENCIES
                ),
                calendars = CF_TIME_CALENDARS,
                variable_sets = HOURLY_KQDM_MODEL_VARIABLES
            )
        ),
        input_kinds = "role_inputs",
        output_kinds = "hourly_role_inputs",
        scopes = "multivariate",
        stochastic = FALSE,
        operations = list(apply = hourly_kqdm_input__apply),
        metadata = list(
            algorithm = "raw_model_to_hourly_kqdm_signals",
            references = HOURLY_WEATHER_REFERENCES,
            raw_model_variables = HOURLY_KQDM_MODEL_VARIABLES,
            variable_frequencies = HOURLY_KQDM_MODEL_FREQUENCIES,
            signal_variables = HOURLY_KQDM_SIGNAL_VARIABLES,
            target_frequency = "hour"
        )
    )
}

# Register the method-specific adapter once while preserving process-local
# replacements under the same component key.
hourly_kqdm_input__register_component <- function() {
    component__register_builtin(hourly_kqdm_input__component())
    invisible(NULL)
}
