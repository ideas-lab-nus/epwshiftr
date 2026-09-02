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

test_that("shared signal requirements preserve the three-role contract", {
    requirements <- signal__three_role_requirements(
        as.list(c("tas", "pr")),
        frequencies = "day"
    )

    expect_named(requirements, SIGNAL_THREE_INPUT_ROLES)
    for (role in SIGNAL_THREE_INPUT_ROLES) {
        requirement <- requirements[[role]]
        expect_s7_class(requirement, WeatherInputRequirement)
        expect_identical(requirement@role, role)
        expect_identical(requirement@representations, "series")
        expect_identical(requirement@frequencies, "day")
        expect_identical(requirement@variable_sets, list("tas", "pr"))
    }
})

test_that("shared adjusted result validation retains diagnostic contracts", {
    daily <- bias__daily_adjusted_series(
        adjusted_series_test__daily(),
        output_role = "model_future",
        transformation = "test_adjustment"
    )
    validate_daily <- function(value, output_role = "model_future") {
        signal__validate_adjusted_result(
            value,
            DailyAdjustedSeries,
            "DailyAdjustedSeries",
            output_role,
            "Test method"
        )
    }

    expect_true(validate_daily(daily))
    expect_identical(
        validate_daily(1),
        "Test method must return a DailyAdjustedSeries object."
    )
    expect_identical(
        validate_daily(daily, "observed_reference"),
        "Test method output must retain the `observed_reference` role."
    )

    subdaily <- bias__subdaily_adjusted_series(
        adjusted_series_test__subdaily(),
        frequency = "3hr",
        time_step_seconds = 10800,
        output_role = "model_future",
        transformation = "test_adjustment"
    )
    expect_identical(
        signal__validate_adjusted_result(
            subdaily,
            SubdailyAdjustedSeries,
            "SubdailyAdjustedSeries",
            "model_future",
            "Test method",
            frequency = "hour",
            time_step_seconds = 3600,
            temporal_message = "Test method must return hourly values."
        ),
        "Test method must return hourly values."
    )
})

test_that("shared future blocks preserve fitting-window edge diagnostics", {
    blocks <- signal__future_blocks(
        2041:2050,
        future_window_years = 7L,
        output_block_years = 3L,
        method_label = "Test method"
    )

    expect_length(blocks, 4L)
    expect_identical(blocks[[1L]]$output_years, 2041:2043)
    expect_identical(blocks[[1L]]$window_years, 2041:2045)
    expect_true(blocks[[1L]]$truncated_left)
    expect_false(blocks[[1L]]$truncated_right)
    expect_identical(blocks[[4L]]$output_years, 2050L)
    expect_true(blocks[[4L]]$truncated_right)
    expect_error(
        signal__future_blocks(2041:2043 * 2L, 3L, 1L, "Test method"),
        "Test method requires contiguous future model years"
    )
})

test_that("shared signal bounds return values and exact clipping counts", {
    bounded <- signal__bound_values(
        c(-1, 0, 0.5, 1, 2),
        c(0, 1)
    )

    expect_identical(bounded$value, c(0, 0, 0.5, 1, 1))
    expect_identical(bounded$clipped, 2L)
    expect_identical(
        signal__bound_values(c(-1, 2), c(-Inf, Inf))$value,
        c(-1, 2)
    )
    expect_identical(
        signal__bound_values(c(-Inf, Inf), c(0, 1)),
        list(value = c(0, 1), clipped = 2L)
    )
})

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

test_that("daily adjusted series enforces canonical calendar-native data", {
    source <- bias_adjustment_test__series(
        "tas",
        2061L,
        c(280, 282, 284, 286)
    )
    result <- bias__daily_adjusted_series(
        source,
        output_role = "model_future",
        transformation = "additive",
        settings = list(grouping = "calendar_month"),
        provenance = list(method = "test")
    )

    expect_true(S7::S7_inherits(result, DailyAdjustedSeries))
    expect_identical(result@data, source)
    expect_identical(result@output_role, "model_future")
    expect_identical(result@variable_metadata$tas$units, "K")
    expect_identical(result@variable_metadata$tas$frequency, "day")
    expect_identical(result@variable_metadata$tas$calendars, "noleap")

    for (calendar in CF_TIME_CALENDARS) {
        calendar_data <- bias_adjustment_test__series(
            "tas",
            2000L,
            c(280, 282, 284, 286),
            calendar = calendar
        )
        expect_no_error(
            bias__daily_adjusted_series(
                calendar_data,
                "model_future",
                "additive"
            )
        )
    }

    duplicate <- rbind(source, source[1L, ])
    expect_error(
        bias__daily_adjusted_series(
            duplicate,
            "model_future",
            "additive"
        ),
        "unique variable-calendar-year-month-day"
    )
    invalid_date <- source
    invalid_date$cf_day[[1L]] <- 31L
    invalid_date$cf_month[[1L]] <- 2L
    expect_error(
        bias__daily_adjusted_series(
            invalid_date,
            "model_future",
            "additive"
        ),
        "invalid date"
    )
    invalid_phase <- source
    invalid_phase$annual_phase[[1L]] <- 0.9
    expect_error(
        bias__daily_adjusted_series(
            invalid_phase,
            "model_future",
            "additive"
        ),
        "annual_phase"
    )
})
