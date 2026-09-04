# Build compact canonical daily rows for package-native signal contract tests.
bias_adjustment_test__series <- function(
    variable_id,
    year,
    values,
    months = c(1L, 1L, 2L, 2L),
    days = c(1L, 2L, 1L, 2L),
    calendar = "noleap",
    units = if (identical(variable_id, "pr")) {
        "kg m-2 s-1"
    } else {
        "K"
    }
) {
    fields <- data.frame(
        year = rep.int(as.integer(year), length(values)),
        month = as.integer(months),
        day = as.integer(days),
        hour = rep.int(12, length(values)),
        minute = rep.int(0, length(values)),
        second = rep.int(0, length(values))
    )
    coordinates <- cf_time__coordinates(fields, calendar)
    data.frame(
        variable_id = rep.int(variable_id, length(values)),
        value = as.numeric(values),
        units = rep.int(units, length(values)),
        frequency = rep.int("day", length(values)),
        coordinates,
        stringsAsFactors = FALSE
    )
}

# Build all three role-labelled WeatherInput objects required by monthly
# mean-change signals while retaining the same tables in the aligned group.
bias_adjustment_test__inputs <- function(observed, historical, future) {
    weather__new_inputs(
        observed_reference = weather__new_input(
            "observed_reference",
            observed
        ),
        model_historical = weather__new_input(
            "model_historical",
            historical
        ),
        model_future = weather__new_input(
            "model_future",
            future
        )
    )
}
