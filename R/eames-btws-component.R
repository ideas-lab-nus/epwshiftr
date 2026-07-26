# Eames BTWS hourly component {{{

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

# }}}
