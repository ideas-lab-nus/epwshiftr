#' @include weather-temperature.R component-temperature-epw.R
NULL

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

# The BTWS composition requires all three temperature statistics because the
# hourly projection preserves separate mean, minimum, and maximum targets
# rather than inheriting a missing daily range.
EPW_MORPH_DAILY_TEMPERATURE_BTWS_METHODS <- c(tdb = "btws")

# Describe the stricter climate-variable contract selected when the shared
# daily temperature signal is paired with the BTWS hourly component.
EPW_MORPH_DAILY_TEMPERATURE_BTWS_RULES <- data.table::data.table(
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
    method = c("btws", "derived", "derived"),
    required = c(TRUE, FALSE, FALSE),
    derived = c(FALSE, TRUE, TRUE),
    method_choices = list("btws", "derived", "derived")
)

# The daily POWER and BTWS recipes add their circular-window setting to the
# method-neutral temperature output controls.
EPW_MORPH_DAILY_TEMPERATURE_OPTIONS <- c(
    list(window_days = 31L),
    EPW_MORPH_TEMPERATURE_OPTIONS
)

# Validate and complete the JSON-safe options used by foreground, background,
# and resumed daily temperature recipes.
daily__temperature_backend_options <- function(options = NULL) {
    temperature__backend_options(
        options,
        defaults = EPW_MORPH_DAILY_TEMPERATURE_OPTIONS,
        label = "Daily temperature",
        unknown_label = "daily temperature"
    )
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
    temperature__preprocess_inputs(inputs, options)
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

# Apply the constrained 24-hour projection to the preserved EPW sequence and
# retain hourly and daily closure values for the later physics stage.
daily__temperature_hourly_reconstruct <- function(
    data,
    inputs,
    context,
    options
) {
    temperature__hourly_result(
        data,
        options,
        daily__project_temperature
    )
}

# Build only the components that implement the daily signal and POWER hourly
# projection. Shared sequence, physics, and output components live separately.
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
        )
    )
}

# Register built-in daily temperature components once without replacing an
# existing implementation under the same stable registry key.
daily__register_temperature_components <- function() {
    temperature__register_components()
    component__register_builtins(daily__temperature_component_specs())
}

# Return a seven-stage daily temperature pipeline with an explicitly selected
# hourly reconstruction component. The climate signal and all other stages stay
# identical so comparisons isolate the hourly projection algorithm.
daily__temperature_pipeline <- function(
    reconstruction = c("power", "btws")
) {
    reconstruction <- match.arg(reconstruction)
    daily__register_temperature_components()
    hourly <- "constrained_daily_temperature"
    if (identical(reconstruction, "btws")) {
        btws__register_hourly_component()
        hourly <- "btws_temperature_projection"
    }
    pipeline__spec(list(
        preprocess = "daily_temperature_inputs",
        calendar = "daily_temperature_calendar",
        signal = "daily_temperature_delta",
        sequence = "preserve_epw_sequence",
        hourly = hourly,
        physics = "specific_humidity_closure",
        output = "daily_temperature_epw_result"
    ))
}

# }}}
