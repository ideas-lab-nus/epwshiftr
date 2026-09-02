# Convert offsets from a native-calendar year start into canonical CF fields
# used by all hourly-weather interpolation fixtures.
weather_interp_test__coordinates <- function(offsets, calendar = "noleap") {
    origin <- data.frame(year = 2061L, month = 1L, day = 1L)
    fields <- cf_time_offset2date(offsets %/% 86400, origin, calendar)
    seconds <- offsets %% 86400
    fields$hour <- seconds %/% 3600
    fields$minute <- (seconds %% 3600) %/% 60
    fields$second <- seconds %% 60
    list(
        fields = fields,
        coordinates = cf_time__coordinates(fields, calendar),
        seconds = seconds
    )
}

# Build one regular three-hourly temperature series whose extrema fall between
# source samples at the observed modal hours used by the anchor tests.
weather_interp_test__tas <- function(value_offset = 0) {
    offsets <- 79 * 86400 + seq.int(21600, 64800, by = 10800)
    coordinates <- weather_interp_test__coordinates(offsets)
    data.frame(
        site_id = "A",
        source_id = "example-model",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        table_id = "3hr",
        period = "2060s",
        variable_id = "tas",
        value = c(290, 295, 300, 296, 292) + value_offset,
        units = "K",
        frequency = "3hr",
        time = as.POSIXct("2061-01-01", tz = "UTC") + offsets,
        coordinates$coordinates,
        stringsAsFactors = FALSE
    )
}

# Build paired daily extrema on the same native-calendar day as the three-
# hourly temperature samples.
weather_interp_test__extrema <- function(value_offset = 0) {
    offsets <- rep.int(79 * 86400, 2L)
    coordinates <- weather_interp_test__coordinates(offsets)
    data.frame(
        site_id = "A",
        source_id = "example-model",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        table_id = "day",
        period = "2060s",
        variable_id = c("tasmin", "tasmax"),
        value = c(288, 304) + value_offset,
        units = "K",
        frequency = "day",
        time = as.POSIXct("2061-01-01", tz = "UTC") + offsets,
        coordinates$coordinates,
        stringsAsFactors = FALSE
    )
}

# Build bounded interval-mean shortwave radiation for the same site and model
# identity as the point-state fixture.
weather_interp_test__radiation <- function(value_offset = 0) {
    interval_starts <- 79 * 86400 + seq.int(21600, 54000, by = 10800)
    source_step <- 10800
    sample_offsets <- interval_starts + source_step / 2
    coordinates <- weather_interp_test__coordinates(sample_offsets)
    origin <- as.POSIXct("2061-01-01", tz = "UTC")
    data.frame(
        site_id = "A",
        source_id = "example-model",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        table_id = "3hr",
        period = "2060s",
        variable_id = "rsds",
        value = c(200, 500, 600, 300) + value_offset,
        units = "W m-2",
        frequency = "3hr",
        time = origin + sample_offsets,
        time_bound_start = origin + interval_starts,
        time_bound_end = origin + interval_starts + source_step,
        lon = 0,
        lat = 0,
        coordinates$coordinates,
        stringsAsFactors = FALSE
    )
}

# Build an hourly observed reference whose daily temperature extrema occur at
# 07:00 and 14:00, plus the matching radiation variable required downstream.
weather_interp_test__observed <- function() {
    offsets <- 79 * 86400 + seq.int(0, 23 * 3600, by = 3600)
    coordinates <- weather_interp_test__coordinates(offsets)
    temperature <- 295 + 5 * cos((seq_len(24L) - 15) * pi / 12)
    temperature[[8L]] <- min(temperature) - 1
    temperature[[15L]] <- max(temperature) + 1
    one_variable <- function(variable, value, units) {
        data.frame(
            site_id = "A",
            variable_id = variable,
            value = value,
            units = units,
            frequency = "hour",
            time = as.POSIXct("2061-01-01", tz = "UTC") + offsets,
            coordinates$coordinates,
            stringsAsFactors = FALSE
        )
    }
    data.table::rbindlist(list(
        one_variable("tas", temperature, "K"),
        one_variable("rsds", pmax(0, 500 * sin(seq(0, pi, length.out = 24))),
            "W m-2")
    ), use.names = TRUE, fill = TRUE)
}

# Assemble all three semantic roles with mixed point, daily-extrema, and
# interval-mean rows in each model input.
weather_interp_test__inputs <- function(include_extrema = TRUE) {
    model <- function(value_offset, radiation_offset) {
        pieces <- list(
            weather_interp_test__tas(value_offset),
            weather_interp_test__radiation(radiation_offset)
        )
        if (include_extrema) {
            pieces <- append(
                pieces,
                list(weather_interp_test__extrema(value_offset)),
                after = 1L
            )
        }
        data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
    }
    weather__new_inputs(
        observed_reference = weather__new_input(
            "observed_reference",
            weather_interp_test__observed()
        ),
        model_historical = weather__new_input(
            "model_historical",
            model(0, 0)
        ),
        model_future = weather__new_input(
            "model_future",
            model(2, 20)
        )
    )
}

test_that("hourly weather interpolation dispatches variables and applies extrema anchors", {
    result <- weather_interp__apply(
        weather_interp_test__inputs(),
        context = NULL,
        options = list()
    )
    future <- data.table::as.data.table(result@value@model_future@source)
    temperature <- future[get("variable_id") == "tas"]

    expect_s7_class(result, WeatherStageResult)
    expect_identical(result@component, "hourly_weather_interpolation")
    expect_identical(result@kind, "hourly_role_inputs")
    expect_identical(
        sort(result@value@model_future@variables),
        c("rsds", "tas")
    )
    expect_equal(
        temperature[get("cf_second_of_day") == 7 * 3600][["value"]],
        290
    )
    expect_equal(
        temperature[get("cf_second_of_day") == 14 * 3600][["value"]],
        306
    )
    expect_match(
        temperature[get("cf_second_of_day") == 7 * 3600][["source_kind_left"]],
        "tasmin"
    )
    expect_match(
        temperature[get("cf_second_of_day") == 14 * 3600][["source_kind_left"]],
        "tasmax"
    )
    expect_true(result@provenance$daily_extrema_anchors)
    expect_setequal(
        result@diagnostics$hourly_weather_interpolation$family,
        c("point_state", "solar_radiation")
    )
    future_coordinates <- result@diagnostics$hourly_weather_coordinates[
        role == "model_future"
    ]
    expect_identical(
        future_coordinates[variable_id == "tas"]$hour_phase_seconds,
        0
    )
    expect_identical(
        future_coordinates[variable_id == "rsds"]$hour_phase_seconds,
        1800
    )
    point <- result@diagnostics$hourly_weather_interpolation[
        family == "point_state"
    ]
    expect_identical(point$anchor_samples, c(2L, 2L))
    expect_false(any(result@value@model_future@variables %in%
        HOURLY_WEATHER_EXTREMA_VARIABLES))
})

test_that("hourly weather interpolation keeps the unanchored linear fallback explicit", {
    result <- weather_interp__apply(
        weather_interp_test__inputs(include_extrema = FALSE),
        context = NULL,
        options = list()
    )
    future <- data.table::as.data.table(result@value@model_future@source)
    temperature <- future[get("variable_id") == "tas"]

    expect_false(result@provenance$daily_extrema_anchors)
    expect_equal(
        temperature[get("cf_second_of_day") == 7 * 3600][["value"]],
        292 + 5 / 3
    )
    expect_true(all(
        result@diagnostics$hourly_weather_interpolation[
            family == "point_state"
        ]$anchor_samples == 0L
    ))
})

test_that("hourly weather interpolation rejects incomplete extrema and registers its contract", {
    inputs <- weather_interp_test__inputs()
    historical <- inputs@model_historical@source
    historical <- historical[historical$variable_id != "tasmax", ]
    inputs@model_historical <- weather__new_input(
        "model_historical",
        historical
    )
    expect_error(
        weather_interp__apply(inputs, NULL, list()),
        "tasmin.*tasmax.*together"
    )

    weather_interp__register_component()
    component <- component__get(
        "preprocess",
        "hourly_weather_interpolation"
    )
    expect_identical(component@scopes, "multivariate")
    expect_identical(component@output_kinds, "hourly_role_inputs")
    expect_identical(
        component@required_inputs$observed_reference@frequencies,
        "hour"
    )
})
