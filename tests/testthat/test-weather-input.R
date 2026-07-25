test_that("future-weather inputs preserve four distinct semantic roles", {
    future <- data.frame(
        variable_id = c("tas", "tasmin", "tasmax"),
        frequency = "day",
        cf_calendar = "360_day"
    )
    historical <- transform(future, cf_calendar = "noleap")
    observed <- data.frame(
        variable_id = "tas",
        frequency = "day",
        calendar = "gregorian"
    )

    inputs <- weather__new_inputs(
        weather_template = weather__new_input(
            "weather_template",
            "baseline.epw"
        ),
        observed_reference = weather__new_input(
            "observed_reference",
            observed
        ),
        model_historical = weather__new_input(
            "model_historical",
            historical
        ),
        model_future = weather__new_input("model_future", future)
    )

    expect_true(S7::S7_inherits(inputs, WeatherInputs))
    expect_identical(
        weather__get_input(inputs, "weather_template")@representation,
        "epw"
    )
    expect_identical(
        weather__get_input(inputs, "observed_reference")@calendars,
        "gregorian"
    )
    expect_identical(
        weather__get_input(inputs, "model_historical")@calendars,
        "noleap"
    )
    expect_identical(
        weather__get_input(inputs, "model_future")@variables,
        c("tas", "tasmin", "tasmax")
    )
    expect_identical(
        weather__get_input(inputs, "model_future")@frequencies,
        "day"
    )
    expect_identical(
        weather__get_input(inputs, "model_future")@calendars,
        "360_day"
    )
})

test_that("future-weather input sets reject missing and mislabelled roles", {
    future <- weather__new_input(
        "model_future",
        data.frame(variable_id = "tas", frequency = "day")
    )

    expect_error(
        weather__new_inputs(),
        "At least one future-weather input"
    )
    expect_error(
        weather__new_inputs(model_historical = future),
        "contains input role `model_future`"
    )
    expect_error(
        weather__new_input(
            "model_future",
            data.frame(variable_id = "tas"),
            representation = "unknown"
        ),
        "Must be element of set"
    )
})

test_that("morphing contexts expose explicit inputs and preserve legacy fields", {
    epw <- epw_file_read(get_cache_epw())
    future <- data.table::data.table(
        variable_id = "tas",
        time = as.POSIXct("2050-01-01", tz = "UTC"),
        period = "future",
        year = 2050L,
        lon = 103.8,
        lat = 1.3,
        units = "K",
        value = 301,
        frequency = "day",
        cf_calendar = "360_day"
    )
    historical <- data.table::copy(future)
    historical[, `:=`(
        time = as.POSIXct("2000-01-01", tz = "UTC"),
        period = "historical",
        year = 2000L,
        value = 299,
        cf_calendar = "noleap"
    )]
    observed <- data.table::copy(historical)
    observed[, `:=`(
        period = "observed",
        value = 298,
        cf_calendar = "standard"
    )]

    context <- morpher__context(
        epw,
        future,
        reference_climate = historical,
        observed_reference = observed
    )

    expect_true(S7::S7_inherits(context$inputs, WeatherInputs))
    expect_identical(context$inputs@weather_template@source, context$epw)
    expect_identical(context$inputs@model_future@source, context$climate)
    expect_identical(
        context$inputs@model_historical@source,
        context$reference_climate
    )
    expect_identical(
        context$inputs@observed_reference@source,
        context$observed_reference
    )
    expect_identical(context$inputs@weather_template@frequencies, "hour")
    expect_identical(context$inputs@model_future@frequencies, "day")
    expect_identical(context$inputs@model_future@calendars, "360_day")
    expect_identical(
        context$inputs@observed_reference@role,
        "observed_reference"
    )
})
