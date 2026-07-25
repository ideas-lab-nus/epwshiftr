# Eames BTWS hourly backend {{{

# The composite backend changes dry-bulb temperature with the published Eames
# hourly transfer and derives the coupled humidity fields afterward.
EPW_MORPH_DAILY_BTWS_METHODS <- c(tdb = "eames_btws")

# BTWS needs all three daily temperature statistics in both historical and
# future model inputs; none of the extrema are optional for this comparison.
EPW_MORPH_DAILY_BTWS_RULES <- data.table::data.table(
    step = c("tdb", "rh", "tdew"),
    epw_field = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature"
    ),
    variable_id = c(
        "tas,tasmin,tasmax",
        NA_character_,
        NA_character_
    ),
    optional_variable_id = NA_character_,
    method = c("eames_btws", "derived", "derived"),
    required = c(TRUE, FALSE, FALSE),
    derived = c(FALSE, TRUE, TRUE),
    method_choices = list("eames_btws", "derived", "derived")
)

# Reconstruct every EPW day with the BTWS projector while retaining the shared
# target, boundary, and physical-closure payload used by daily temperature.
btws__hourly_reconstruct <- function(
    data,
    inputs,
    context,
    options
) {
    daily__temperature_hourly_result(
        data,
        options,
        btws__project_temperature
    )
}

# Define the one stage that differs from the existing daily-power pipeline.
# Its kind contract deliberately matches the shared physics component.
btws__hourly_component <- function() {
    template <- component__input_requirement(
        "weather_template",
        representations = "epw",
        frequencies = "hour",
        calendars = "gregorian"
    )
    component__spec(
        name = "eames_btws_temperature",
        stage = "hourly",
        label = "Eames bounded temperature weighted stretch",
        required_inputs = list(weather_template = template),
        input_kinds = "daily_temperature_sequence",
        output_kinds = "hourly_temperature_projected",
        scopes = "multivariate",
        operations = list(
            reconstruct = btws__hourly_reconstruct
        )
    )
}

# Register the BTWS hourly component once without replacing another process-
# local implementation under the same stable registry key.
btws__register_hourly_component <- function() {
    component <- btws__hourly_component()
    key <- component__registry_key(component@stage, component@name)
    if (!exists(
        key,
        envir = WEATHER_COMPONENT_REGISTRY,
        inherits = FALSE
    )) {
        component__register(component)
    }
    invisible(NULL)
}

# Assemble the comparison pipeline from six shared daily-temperature stages
# and the paper-specific Eames hourly reconstruction component.
btws__pipeline <- function() {
    daily__register_temperature_components()
    btws__register_hourly_component()
    pipeline__spec(list(
        preprocess = "daily_temperature_inputs",
        calendar = "daily_temperature_calendar",
        signal = "daily_temperature_delta",
        sequence = "preserve_epw_sequence",
        hourly = "eames_btws_temperature",
        physics = "specific_humidity_closure",
        output = "daily_temperature_epw_result"
    ))
}

# }}}
