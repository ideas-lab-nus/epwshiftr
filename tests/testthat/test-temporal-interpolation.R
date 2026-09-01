# Build one regular sub-daily state-variable series with native CF coordinates
# and a POSIX surrogate whose elapsed seconds match the native calendar.
temporal_test__series <- function(
    calendar = "noleap",
    frequency = "3hr",
    variable = "tas",
    offsets = seq.int(0, 21600, by = 10800),
    site_id = "A",
    value_offset = 0,
    include_second = FALSE
) {
    origin <- data.frame(year = 2061L, month = 1L, day = 1L)
    day_offsets <- offsets %/% 86400
    seconds <- offsets %% 86400
    fields <- cf_time_offset2date(day_offsets, origin, calendar)
    fields$hour <- seconds %/% 3600
    fields$minute <- (seconds %% 3600) %/% 60
    fields$second <- seconds %% 60
    coordinates <- cf_time__coordinates(fields, calendar)
    data <- data.frame(
        site_id = rep.int(site_id, length(offsets)),
        source_id = rep.int("example-model", length(offsets)),
        experiment_id = rep.int("ssp585", length(offsets)),
        variant_label = rep.int("r1i1p1f1", length(offsets)),
        table_id = rep.int(frequency, length(offsets)),
        period = rep.int("2060s", length(offsets)),
        variable_id = rep.int(variable, length(offsets)),
        value = value_offset + offsets / 3600,
        units = rep.int(if (identical(variable, "tas")) "K" else "1", length(offsets)),
        frequency = rep.int(frequency, length(offsets)),
        time = as.POSIXct("2061-01-01", tz = "UTC") + offsets,
        year = as.integer(fields$year),
        month = as.integer(fields$month),
        day = as.integer(fields$day),
        hour = as.integer(fields$hour),
        coordinates,
        stringsAsFactors = FALSE
    )
    if (isTRUE(include_second)) {
        data$cf_second_of_day <- as.numeric(seconds)
    }
    data
}

# Assemble the two model roles required by the standalone preprocess component.
temporal_test__inputs <- function(
    historical = temporal_test__series(),
    future = temporal_test__series(value_offset = 100),
    observed_reference = NULL
) {
    weather__new_inputs(
        observed_reference = observed_reference,
        model_historical = weather__new_input(
            "model_historical",
            historical
        ),
        model_future = weather__new_input("model_future", future)
    )
}

test_that("linear temporal interpolation preserves source hours and weights", {
    source_order <- c(3L, 1L, 2L)
    result <- temporal__linear_apply(
        temporal_test__inputs(
            temporal_test__series()[source_order, ],
            temporal_test__series(value_offset = 100)[source_order, ]
        ),
        context = NULL,
        options = list()
    )
    future <- result@value@model_future@source

    expect_s7_class(result, WeatherStageResult)
    expect_identical(result@stage, "preprocess")
    expect_identical(result@kind, "hourly_role_inputs")
    expect_identical(result@value@model_future@frequencies, "hour")
    expect_identical(future$value, 100 + 0:6)
    expect_identical(future$cf_second_of_day, seq.int(0, 21600, by = 3600))
    expect_identical(
        future$interpolation_weight_right,
        c(0, 1 / 3, 2 / 3, 0, 1 / 3, 2 / 3, 0)
    )
    source_rows <- future$cf_second_of_day %% 10800 == 0
    expect_identical(
        future$source_time_left[source_rows],
        future$source_time_right[source_rows]
    )
    expect_identical(
        future$source_row_left[source_rows],
        c(2L, 3L, 1L)
    )
    expect_identical(
        result@provenance$boundary_policy,
        "bounded_by_source"
    )
    expect_identical(
        result@diagnostics$temporal_interpolation$interpolated_samples,
        c(4L, 4L)
    )
})

test_that("linear temporal interpolation supports every CF calendar boundary", {
    for (calendar in CF_TIME_CALENDARS) {
        year_days <- cf_time__year_days(2061L, calendar)[[1L]]
        offsets <- c(
            year_days * 86400 - 10800,
            year_days * 86400
        )
        historical <- temporal_test__series(
            calendar = calendar,
            offsets = offsets,
            include_second = TRUE
        )
        future <- temporal_test__series(
            calendar = calendar,
            offsets = offsets,
            value_offset = 100,
            include_second = TRUE
        )
        result <- temporal__linear_apply(
            temporal_test__inputs(historical, future),
            NULL,
            list()
        )
        data <- result@value@model_future@source

        expect_identical(nrow(data), 4L, info = calendar)
        expect_identical(
            data$cf_second_of_day,
            c(75600, 79200, 82800, 0),
            info = calendar
        )
        expect_identical(
            data$cf_year,
            c(2061L, 2061L, 2061L, 2062L),
            info = calendar
        )
        expect_identical(
            data$value,
            100 + offsets[[1L]] / 3600 + 0:3,
            info = calendar
        )
    }
})

test_that("linear temporal interpolation handles a non-zero source phase", {
    offsets <- seq.int(5400, 27000, by = 10800)
    result <- temporal__linear_apply(
        temporal_test__inputs(
            temporal_test__series(offsets = offsets),
            temporal_test__series(offsets = offsets, value_offset = 100)
        ),
        NULL,
        list()
    )
    data <- result@value@model_future@source

    expect_identical(data$cf_second_of_day, seq.int(7200, 25200, by = 3600))
    expect_equal(data$value, 102:107)
    expect_equal(
        data$interpolation_weight_right,
        c(1 / 6, 1 / 2, 5 / 6, 1 / 6, 1 / 2, 5 / 6)
    )
})

test_that("linear temporal interpolation keeps independent groups isolated", {
    historical <- rbind(
        temporal_test__series(site_id = "A"),
        temporal_test__series(site_id = "B", value_offset = 1000)
    )
    future <- rbind(
        temporal_test__series(site_id = "A", value_offset = 100),
        temporal_test__series(site_id = "B", value_offset = 1100)
    )
    observed <- weather__new_input(
        "observed_reference",
        data.frame(value = 1),
        representation = "series"
    )
    inputs <- temporal_test__inputs(historical, future, observed)
    result <- temporal__linear_apply(inputs, NULL, list())
    data <- result@value@model_future@source

    expect_identical(
        data[site_id == "B"]$value - data[site_id == "A"]$value,
        rep.int(1000, 7L)
    )
    expect_identical(
        result@value@observed_reference,
        observed
    )
    expect_identical(
        result@value@model_future@metadata$time_step_seconds,
        3600
    )
    expect_identical(
        result@value@model_future@provenance$temporal_interpolation$method,
        "linear_temporal_interpolation"
    )
})

test_that("linear temporal interpolation supports mixed source frequencies", {
    three_hourly <- temporal_test__series()
    six_hourly <- temporal_test__series(
        frequency = "6hr",
        variable = "hurs",
        offsets = seq.int(0, 43200, by = 21600),
        value_offset = 40
    )
    historical <- rbind(three_hourly, six_hourly)
    future <- rbind(
        temporal_test__series(value_offset = 100),
        temporal_test__series(
            frequency = "6hr",
            variable = "hurs",
            offsets = seq.int(0, 43200, by = 21600),
            value_offset = 50
        )
    )
    result <- temporal__linear_apply(
        temporal_test__inputs(historical, future),
        NULL,
        list()
    )
    data <- result@value@model_future@source
    provenance <- result@value@model_future@provenance[[
        "temporal_interpolation"
    ]]

    expect_identical(data[variable_id == "tas", .N], 7L)
    expect_identical(data[variable_id == "hurs", .N], 13L)
    expect_identical(
        provenance$source_frequencies,
        c("3hr", "6hr")
    )
    expect_identical(
        provenance$source_step_seconds,
        c(10800, 21600)
    )
    expect_true(
        "source_frequency" %in%
            result@value@model_future@metadata$group_columns
    )
})

test_that("linear temporal interpolation rejects unsafe source semantics", {
    gap <- temporal_test__series()
    gap <- gap[-2L, ]
    expect_error(
        temporal__linear_apply(
            temporal_test__inputs(gap, temporal_test__series()),
            NULL,
            list()
        ),
        "gap or irregular source timestep"
    )

    duplicate <- rbind(
        temporal_test__series(),
        temporal_test__series()[1L, ]
    )
    expect_error(
        temporal__linear_apply(
            temporal_test__inputs(duplicate, temporal_test__series()),
            NULL,
            list()
        ),
        "unique variable-calendar-year-month-day-second"
    )

    radiation <- temporal_test__series(variable = "rsds")
    expect_error(
        temporal__linear_apply(
            temporal_test__inputs(radiation, radiation),
            NULL,
            list()
        ),
        "without linear point-state semantics"
    )

    inconsistent_time <- temporal_test__series()
    inconsistent_time$time[[2L]] <- inconsistent_time$time[[2L]] + 60
    expect_error(
        temporal__linear_apply(
            temporal_test__inputs(
                inconsistent_time,
                temporal_test__series()
            ),
            NULL,
            list()
        ),
        "inconsistent with its native CF chronology"
    )

    expect_error(
        temporal__linear_apply(
            temporal_test__inputs(),
            NULL,
            list(extrapolate = TRUE)
        ),
        "does not accept component options"
    )

    empty <- temporal_test__series()[0L, ]
    expect_error(
        temporal__linear_apply(
            temporal_test__inputs(empty, temporal_test__series()),
            NULL,
            list()
        ),
        "must contain sub-daily source samples"
    )
})

test_that("linear temporal interpolation is registered and compatible", {
    temporal__register_linear_component()
    component <- component__get(
        "preprocess",
        "linear_temporal_interpolation"
    )
    calendar <- component__spec(
        name = "hourly_calendar_test",
        stage = "calendar",
        input_kinds = "hourly_role_inputs",
        output_kinds = "calendar_indexed_hourly",
        scopes = "multivariate",
        operations = list(apply = identity)
    )
    inputs <- temporal_test__inputs()

    expect_s7_class(component, WeatherComponentSpec)
    expect_identical(component@stage, "preprocess")
    expect_identical(component@output_kinds, "hourly_role_inputs")
    expect_identical(component@scopes, "univariate")
    expect_identical(component@metadata$target_step_seconds, 3600)
    expect_identical(
        component@metadata$radiation_policy,
        "dedicated_solar_interpolation_required"
    )
    expect_invisible(component__validate_inputs(component, inputs))
    expect_true(component__compatible(component, calendar))

    invalid_frequency <- temporal_test__series()
    invalid_frequency$frequency <- "day"
    invalid_inputs <- temporal_test__inputs(
        invalid_frequency,
        invalid_frequency
    )
    expect_error(
        component__validate_inputs(component, invalid_inputs),
        "frequencies `day`"
    )
})
