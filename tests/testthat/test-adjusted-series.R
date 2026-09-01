# Build a regular calendar-native sub-daily table while exposing the exact
# time-of-day coordinate and one untouched source timestamp column.
adjusted_series_test__subdaily <- function(
    variable = "tas",
    year = 2061L,
    days = 2L,
    calendar = "noleap",
    frequency = "3hr",
    time_step_seconds = 10800,
    time_offset_seconds = 0
) {
    offsets <- seq.int(
        from = time_offset_seconds,
        to = days * 86400 - time_step_seconds + time_offset_seconds,
        by = time_step_seconds
    )
    day_offsets <- offsets %/% 86400
    seconds <- offsets %% 86400
    fields <- cf_time_offset2date(
        day_offsets,
        data.frame(year = year, month = 1L, day = 1L),
        calendar
    )
    fields$hour <- seconds %/% 3600
    fields$minute <- (seconds %% 3600) %/% 60
    fields$second <- seconds %% 60
    coordinates <- cf_time__coordinates(fields, calendar)
    data.frame(
        variable_id = rep.int(variable, length(offsets)),
        value = seq_along(offsets),
        units = rep.int("K", length(offsets)),
        frequency = rep.int(frequency, length(offsets)),
        coordinates,
        cf_second_of_day = as.numeric(seconds),
        source_time = sprintf(
            "%s:%05d",
            calendar,
            as.integer(offsets)
        ),
        stringsAsFactors = FALSE
    )
}

# Build a compact daily table without depending on helpers from another test
# file or assigning Gregorian dates to non-Gregorian calendars.
adjusted_series_test__daily <- function() {
    fields <- cf_time_offset2date(
        0:3,
        data.frame(year = 2061L, month = 1L, day = 1L),
        "noleap"
    )
    fields$hour <- 12L
    fields$minute <- 0L
    fields$second <- 0
    data.frame(
        variable_id = rep.int("tas", 4L),
        value = c(280, 282, 284, 286),
        units = rep.int("K", 4L),
        frequency = rep.int("day", 4L),
        cf_time__coordinates(fields, "noleap"),
        stringsAsFactors = FALSE
    )
}

test_that("daily adjusted series remains a strict specialization", {
    data <- adjusted_series_test__daily()
    adjusted <- bias__daily_adjusted_series(
        data,
        output_role = "model_future",
        transformation = "test_adjustment"
    )

    expect_s7_class(adjusted, DailyAdjustedSeries)
    expect_true(S7::S7_inherits(adjusted, AdjustedWeatherSeries))
    expect_identical(adjusted@frequency, "day")
    expect_identical(as.numeric(adjusted@time_step_seconds), 86400)
    expect_identical(adjusted@data, data)
})

test_that("sub-daily adjusted series retains its regular CF time lattice", {
    for (calendar in CF_TIME_CALENDARS) {
        data <- adjusted_series_test__subdaily(calendar = calendar)
        adjusted <- bias__subdaily_adjusted_series(
            data,
            frequency = "3hr",
            time_step_seconds = 10800,
            output_role = "model_future",
            transformation = "test_adjustment",
            settings = list(window_months = 3L),
            provenance = list(method = "test_adjustment")
        )

        expect_s7_class(adjusted, SubdailyAdjustedSeries)
        expect_true(
            S7::S7_inherits(adjusted, AdjustedWeatherSeries),
            info = calendar
        )
        expect_identical(adjusted@frequency, "3hr", info = calendar)
        expect_identical(
            as.numeric(adjusted@time_step_seconds),
            10800,
            info = calendar
        )
        expect_identical(
            adjusted@data$source_time,
            data$source_time,
            info = calendar
        )
        expect_identical(
            adjusted@variable_metadata$tas$frequency,
            "3hr",
            info = calendar
        )
    }

    offset_data <- adjusted_series_test__subdaily(
        time_offset_seconds = 5400
    )
    expect_no_error(
        bias__subdaily_adjusted_series(
            offset_data,
            "3hr",
            10800,
            "model_future",
            "test_adjustment"
        )
    )
})

test_that("sub-daily adjusted series rejects ambiguous temporal metadata", {
    data <- adjusted_series_test__subdaily()

    duplicate <- rbind(data, data[1L, ])
    expect_error(
        bias__subdaily_adjusted_series(
            duplicate,
            "3hr",
            10800,
            "model_future",
            "test_adjustment"
        ),
        "unique variable-calendar-year-month-day-second"
    )

    inconsistent_phase <- data
    inconsistent_phase$annual_phase[[2L]] <-
        inconsistent_phase$annual_phase[[2L]] + 0.00001
    expect_error(
        bias__subdaily_adjusted_series(
            inconsistent_phase,
            "3hr",
            10800,
            "model_future",
            "test_adjustment"
        ),
        "annual_phase.*cf_second_of_day"
    )

    irregular <- data
    irregular$cf_second_of_day[[2L]] <- 7200
    irregular$annual_phase[[2L]] <-
        irregular$cf_second_of_day[[2L]] /
        86400 / irregular$cf_year_days[[2L]]
    expect_error(
        bias__subdaily_adjusted_series(
            irregular,
            "3hr",
            10800,
            "model_future",
            "test_adjustment"
        ),
        "regular timestep lattice"
    )

    expect_error(
        bias__subdaily_adjusted_series(
            data,
            "3hr",
            0,
            "model_future",
            "test_adjustment"
        ),
        "positive number"
    )
    expect_error(
        bias__subdaily_adjusted_series(
            data,
            "3hr",
            10000,
            "model_future",
            "test_adjustment"
        ),
        "divide one 86400-second day"
    )
    expect_error(
        bias__subdaily_adjusted_series(
            data,
            "1hr",
            3600,
            "model_future",
            "test_adjustment"
        ),
        "declared `frequency`"
    )
})
