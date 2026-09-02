# BTWS hourly temperature component {{{

# Reconstruct every EPW day with the BTWS projector while retaining the shared
# target, boundary, and physical-closure payload used by daily temperature.
btws__hourly_reconstruct <- function(
    data,
    inputs,
    context,
    options
) {
    temperature__hourly_result(
        data,
        options,
        btws__project_temperature
    )
}

# Define the reusable BTWS projection stage independently of the paper or
# complete recipe that supplies its daily temperature targets.
btws__hourly_component <- function() {
    template <- component__input_requirement(
        "weather_template",
        representations = "epw",
        frequencies = "hour",
        calendars = "gregorian"
    )
    component__spec(
        name = "btws_temperature_projection",
        stage = "hourly",
        label = "Bounded temperature weighted stretch",
        required_inputs = list(weather_template = template),
        input_kinds = "daily_temperature_sequence",
        output_kinds = "hourly_temperature_projected",
        scopes = "multivariate",
        operations = list(
            reconstruct = btws__hourly_reconstruct
        )
    )
}

# Register the method-neutral BTWS component once without replacing another
# process-local implementation under the same stable registry key.
btws__register_hourly_component <- function() {
    component__register_builtin(btws__hourly_component())
    invisible(NULL)
}

# }}}
