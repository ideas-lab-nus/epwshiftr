# Build interval-mean shortwave series with exact native CF coordinates and
# explicit POSIX surrogates for both interval bounds.
solar_test__series <- function(
    calendar = "noleap",
    frequency = "3hr",
    variable = "rsds",
    interval_starts = 79 * 86400 + seq.int(21600, 43200, by = 10800),
    coordinate_position = 0.5,
    values = seq.int(300, by = 150, length.out = length(interval_starts)),
    value_offset = 0,
    longitude = 0,
    latitude = 0,
    site_id = "A"
) {
    step <- unname(TEMPORAL_SOURCE_STEPS[[frequency]])
    stopifnot(length(step) == 1L, !is.na(step))
    year_start <- data.frame(year = 2061L, month = 1L, day = 1L)
    absolute_start <- as.numeric(cf_time_date2offset(
        year_start,
        data.frame(year = 1L, month = 1L, day = 1L),
        calendar
    )) * 86400 + interval_starts
    absolute_end <- absolute_start + step
    absolute_sample <- absolute_start + coordinate_position * step
    target <- temporal__target_coordinates(absolute_sample, calendar)
    surrogate_origin <- as.POSIXct("2061-01-01", tz = "UTC")
    time <- surrogate_origin + interval_starts + coordinate_position * step
    time_bound_start <- surrogate_origin + interval_starts
    time_bound_end <- time_bound_start + step

    data.frame(
        site_id = rep.int(site_id, length(interval_starts)),
        source_id = rep.int("example-model", length(interval_starts)),
        experiment_id = rep.int("ssp585", length(interval_starts)),
        variant_label = rep.int("r1i1p1f1", length(interval_starts)),
        table_id = rep.int(frequency, length(interval_starts)),
        period = rep.int("2060s", length(interval_starts)),
        variable_id = rep.int(variable, length(interval_starts)),
        value = as.numeric(values) + value_offset,
        units = rep.int("W m-2", length(interval_starts)),
        frequency = rep.int(frequency, length(interval_starts)),
        time = time,
        time_bound_start = time_bound_start,
        time_bound_end = time_bound_end,
        lon = rep.int(longitude, length(interval_starts)),
        lat = rep.int(latitude, length(interval_starts)),
        target$coordinates,
        stringsAsFactors = FALSE
    )
}

# Assemble the two model roles required by the standalone radiation component.
solar_test__inputs <- function(
    historical = solar_test__series(),
    future = solar_test__series(value_offset = 100),
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

test_that("solar radiation interpolation conserves every source interval", {
    source <- solar_test__series(value_offset = 100)
    result <- solar__apply(
        solar_test__inputs(future = source),
        context = NULL,
        options = list()
    )
    future <- result@value@model_future@source
    reconstructed <- future[, .(value = mean(value)), by = source_row]

    expect_s7_class(result, WeatherStageResult)
    expect_identical(result@stage, "preprocess")
    expect_identical(result@kind, "hourly_role_inputs")
    expect_identical(result@value@model_future@frequencies, "hour")
    expect_equal(reconstructed$value, source$value, tolerance = 1e-12)
    expect_equal(
        future[, mean(solar_weight), by = source_row]$V1,
        rep.int(1, nrow(source)),
        tolerance = 1e-12
    )
    expect_true(all(future$value >= 0))
    expect_true(all(future$solar_projection >= 0))
    expect_equal(
        as.numeric(
            future$time_bound_end - future$time_bound_start,
            units = "hours"
        ),
        rep.int(1, nrow(future))
    )
    expect_identical(
        future$cf_second_of_day,
        seq.int(23400, 52200, by = 3600)
    )
    expect_true(all(diff(future[source_row == 1L]$solar_projection) > 0))
    expect_lt(
        max(result@diagnostics$solar_radiation_interpolation[[
            "maximum_conservation_error"
        ]]),
        1e-10
    )
    expect_identical(
        result@provenance$conservation_policy,
        "source_interval_mean"
    )
    expect_identical(result@provenance$published_source_frequency, "3hr")
    expect_identical(result@provenance$adapted_source_frequencies, "6hr")
})

test_that("solar radiation interpolation supports all CF calendar boundaries", {
    for (calendar in CF_TIME_CALENDARS) {
        year_days <- cf_time__year_days(2061L, calendar)[[1L]]
        starts <- c(
            year_days * 86400 - 10800,
            year_days * 86400
        )
        source <- solar_test__series(
            calendar = calendar,
            interval_starts = starts,
            values = c(0, 0)
        )
        result <- solar__apply(
            solar_test__inputs(source, source),
            NULL,
            list()
        )
        data <- result@value@model_future@source

        expect_identical(nrow(data), 6L, info = calendar)
        expect_identical(
            data$cf_year,
            c(rep.int(2061L, 3L), rep.int(2062L, 3L)),
            info = calendar
        )
        expect_identical(
            data$cf_second_of_day,
            c(77400, 81000, 84600, 1800, 5400, 9000),
            info = calendar
        )
        expect_identical(data$value, rep.int(0, 6L), info = calendar)
    }
})

test_that("solar radiation interpolation supports six-hourly diffuse flux", {
    starts <- 79 * 86400 + c(21600, 43200)
    source <- solar_test__series(
        frequency = "6hr",
        variable = "rsdsdiff",
        interval_starts = starts,
        coordinate_position = 1,
        values = c(120, 80)
    )
    result <- solar__apply(
        solar_test__inputs(source, source),
        NULL,
        list()
    )
    data <- result@value@model_future@source

    expect_identical(nrow(data), 12L)
    expect_identical(data$variable_id, rep.int("rsdsdiff", 12L))
    expect_equal(data[, mean(value), by = source_row]$V1, source$value)
    expect_identical(
        data$cf_second_of_day,
        seq.int(25200, 64800, by = 3600)
    )
    expect_identical(
        result@value@model_future@metadata$interval_bounds,
        c("time_bound_start", "time_bound_end")
    )
})

test_that("solar radiation interpolation rejects ambiguous interval semantics", {
    missing_bounds <- solar_test__series()
    missing_bounds$time_bound_start <- NULL
    expect_error(
        solar__apply(
            solar_test__inputs(missing_bounds, solar_test__series()),
            NULL,
            list()
        ),
        "missing bounded-radiation"
    )

    negative <- solar_test__series()
    negative$value[[1L]] <- -1
    expect_error(
        solar__apply(
            solar_test__inputs(negative, solar_test__series()),
            NULL,
            list()
        ),
        "finite and non-negative"
    )

    night <- solar_test__series(
        interval_starts = 79 * 86400,
        values = 10
    )
    expect_error(
        solar__apply(solar_test__inputs(night, night), NULL, list()),
        "zero solar projection"
    )

    gap <- solar_test__series()
    gap$time_bound_start[[2L]] <- gap$time_bound_start[[2L]] + 3600
    expect_error(
        solar__apply(
            solar_test__inputs(gap, solar_test__series()),
            NULL,
            list()
        ),
        "bounds do not match|gapped or overlapping"
    )

    shifted <- solar_test__series()
    shifted$time[[2L]] <- shifted$time[[2L]] + 60
    expect_error(
        solar__apply(
            solar_test__inputs(shifted, solar_test__series()),
            NULL,
            list()
        ),
        "gapped or overlapping|coordinate position|time coordinates inconsistent"
    )

    state <- solar_test__series(variable = "tas")
    expect_error(
        solar__apply(solar_test__inputs(state, state), NULL, list()),
        "without supported shortwave"
    )

    expect_error(
        solar__apply(
            solar_test__inputs(),
            NULL,
            list(extrapolate = TRUE)
        ),
        "does not accept component options"
    )
})

test_that("solar radiation interpolation is registered and compatible", {
    solar__register_component()
    component <- component__get(
        "preprocess",
        "solar_radiation_interpolation"
    )
    calendar <- component__spec(
        name = "hourly_solar_calendar_test",
        stage = "calendar",
        input_kinds = "hourly_role_inputs",
        output_kinds = "calendar_indexed_hourly",
        scopes = "multivariate",
        operations = list(apply = identity)
    )

    expect_s7_class(component, WeatherComponentSpec)
    expect_identical(component@stage, "preprocess")
    expect_identical(component@output_kinds, "hourly_role_inputs")
    expect_identical(component@scopes, "univariate")
    expect_identical(
        component@metadata$interval_policy,
        "cf_time_bounds"
    )
    expect_identical(
        component@metadata$supported_variables,
        c("rsds", "rsdsdiff")
    )
    expect_invisible(
        component__validate_inputs(component, solar_test__inputs())
    )
    expect_true(component__compatible(component, calendar))
})
