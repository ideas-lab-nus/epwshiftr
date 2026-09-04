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

# Build consecutive native-calendar daily rows shared by statistical signal
# method tests without coercing 360-, 365-, or 366-day dates through base Date.
signal_test__series <- function(
    variable_id,
    year,
    values,
    calendar = "noleap",
    units = if (identical(variable_id, "pr")) {
        "kg m-2 s-1"
    } else if (identical(variable_id, "hurs")) {
        "%"
    } else {
        "K"
    }
) {
    origin <- data.frame(
        year = as.integer(year),
        month = 1L,
        day = 1L
    )
    fields <- cf_time_offset2date(
        seq.int(0L, length(values) - 1L),
        origin,
        calendar
    )
    fields$hour <- 12L
    fields$minute <- 0L
    fields$second <- 0
    data.frame(
        variable_id = rep.int(variable_id, length(values)),
        value = as.numeric(values),
        units = rep.int(units, length(values)),
        frequency = rep.int("day", length(values)),
        cf_time__coordinates(fields, calendar),
        stringsAsFactors = FALSE
    )
}

# Construct the role metadata and aligned group shared by signal component
# execution tests so every method exercises the same package boundary.
signal_test__execution_inputs <- function(
    observed,
    historical,
    future,
    key = list(site = "A")
) {
    list(
        inputs = weather__new_inputs(
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
        ),
        group = signal__group(
            key = key,
            inputs = list(
                observed_reference = observed,
                model_historical = historical,
                model_future = future
            ),
            variables = unique(future$variable_id)
        )
    )
}

# Build all three role-labelled WeatherInput objects required by monthly
# mean-change signals while retaining the same tables in the aligned group.
bias_adjustment_test__inputs <- function(observed, historical, future) {
    signal_test__execution_inputs(observed, historical, future)$inputs
}
