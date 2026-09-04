# Build one complete hourly native-calendar series with values defined by the
# supplied annual-phase function so calendar remapping remains directly testable.
hourmap_test__adjusted <- function(
    variable,
    year,
    calendar = "noleap",
    value = function(phase) phase,
    units = NULL
) {
    year_days <- cf_time__year_days(year, calendar)[[1L]]
    offsets <- seq.int(0, year_days * 86400 - 3600, by = 3600)
    fields <- cf_time_offset2date(
        offsets %/% 86400,
        data.frame(year = year, month = 1L, day = 1L),
        calendar
    )
    fields$hour <- (offsets %% 86400) %/% 3600
    fields$minute <- 0L
    fields$second <- 0
    coordinates <- cf_time__coordinates(fields, calendar)
    coordinates$cf_second_of_day <- as.numeric(offsets %% 86400)
    if (is.null(units)) {
        units <- if (variable %in% c("rsds", "rsdsdiff", "rlds")) {
            "W m-2"
        } else {
            "K"
        }
    }
    values <- if (length(formals(value)) >= 2L) {
        value(coordinates$annual_phase, coordinates$cf_second_of_day)
    } else {
        value(coordinates$annual_phase)
    }
    data <- data.frame(
        variable_id = rep.int(variable, length(offsets)),
        value = as.numeric(values),
        units = rep.int(units, length(offsets)),
        frequency = rep.int("hour", length(offsets)),
        coordinates,
        stringsAsFactors = FALSE
    )
    bias__subdaily_adjusted_series(
        data,
        frequency = "hour",
        time_step_seconds = 3600,
        output_role = "model_future",
        transformation = "test_hourly_adjustment",
        settings = list(window_months = 3L),
        provenance = list(source = "synthetic")
    )
}

# Assemble the signal envelope expected by direct_model_realization while
# allowing repeated variables to use distinct group keys in rejection tests.
hourmap_test__execution <- function(values) {
    groups <- lapply(seq_along(values), function(index) {
        signal__group(
            key = list(group = paste0("group-", index)),
            inputs = list(model_future = values[[index]]@data),
            variables = unique(values[[index]]@data[["variable_id"]])
        )
    })
    variables <- unique(unlist(lapply(
        groups,
        function(group) group@variables
    ), use.names = FALSE))
    SignalExecutionResult(
        groups = groups,
        values = values,
        profiles = stats::setNames(lapply(variables, function(variable) {
            list(variable_id = variable)
        }), variables),
        diagnostics = data.frame(
            method = rep.int("test_signal", length(groups)),
            group = paste0("group-", seq_along(groups)),
            status = rep.int("ok", length(groups)),
            variables = vapply(
                groups,
                function(group) paste(group@variables, collapse = ","),
                character(1L)
            ),
            evidence = rep.int("published", length(groups)),
            message = rep.int(NA_character_, length(groups)),
            stringsAsFactors = FALSE
        )
    )
}

# Return the complete role-addressable input set required by the standalone
# hourly component without constructing an otherwise incomplete recipe.
hourmap_test__inputs <- function() {
    epw <- epw_file_read(get_cache_epw())
    weather__new_inputs(
        weather_template = weather__new_input(
            "weather_template",
            epw,
            representation = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        )
    )
}

test_that("365-day direct-model hours map exactly onto EPW rows", {
    adjusted <- hourmap_test__adjusted(
        "tas",
        2061L,
        value = function(phase) seq_along(phase)
    )
    sequence <- sequence__direct_model_generate(
        hourmap_test__execution(list(adjusted)),
        NULL,
        NULL,
        list()
    )
    result <- hourmap__reconstruct(
        sequence,
        hourmap_test__inputs(),
        NULL,
        list()
    )

    expect_s7_class(result, MappedHourlyClimateSequence)
    expect_identical(result@target_calendar, "epw_365_day")
    expect_identical(result@frequency, "hour")
    expect_identical(as.numeric(result@time_step_seconds), 3600)
    expect_length(result@members, 1L)
    member <- result@members[[1L]]
    expect_s7_class(member, MappedHourlyClimateMember)
    expect_identical(member@weather_year, 2061L)
    expect_identical(member@source_calendar, "noleap")
    series <- member@series[[1L]]
    expect_s7_class(series, MappedHourlyClimateSeries)
    expect_identical(
        series@data$value,
        as.numeric(seq_len(HOURMAP_TARGET_HOURS))
    )
    expect_identical(series@data$epw_row, seq_len(HOURMAP_TARGET_HOURS))
    expect_identical(series@data$month[1:24], rep.int(1L, 24L))
    expect_identical(series@data$hour[1:24], seq_len(24L))
    expect_identical(
        unique(series@data$mapping_method),
        "identity_365_day"
    )
    expect_identical(result@provenance$physical_conversion, "deferred")
    expect_error(
        MappedHourlyClimateSequence(
            members = list(member, member),
            frequency = result@frequency,
            time_step_seconds = result@time_step_seconds,
            target_calendar = result@target_calendar,
            provenance = result@provenance
        ),
        "unique ascending weather years"
    )
})

test_that("daily slot traversal shares identity and mapped placement", {
    identity_data <- hourmap_test__adjusted(
        "tas",
        2061L,
        value = function(phase) seq_along(phase)
    )@data
    identity <- hourmap__map_daily_slots(
        identity_data,
        HOURMAP_TARGET_DAYS,
        mapper = function(source_rows, target_phase) {
            stop("identity mapping should bypass the numerical kernel")
        }
    )

    expect_identical(identity$value, as.numeric(identity_data[["value"]]))
    expect_identical(
        identity$target_phase,
        as.numeric(identity_data[["annual_phase"]])
    )
    expect_identical(
        identity$source_second_of_day,
        as.numeric(identity_data[["cf_second_of_day"]])
    )
    expect_identical(identity$hour_phase_seconds, 0)

    data <- hourmap_test__adjusted(
        "tas",
        2061L,
        "360_day",
        value = function(phase, second_of_day) second_of_day / 3600
    )@data
    source_lengths <- integer()
    target_lengths <- integer()
    mapped <- hourmap__map_daily_slots(
        data,
        HOURMAP_TARGET_DAYS,
        mapper = function(source_rows, target_phase) {
            source_lengths <<- c(source_lengths, length(source_rows))
            target_lengths <<- c(target_lengths, length(target_phase))
            rep.int(
                data[["cf_second_of_day"]][source_rows[[1L]]] / 3600,
                length(target_phase)
            )
        }
    )

    expect_identical(source_lengths, rep.int(360L, 24L))
    expect_identical(target_lengths, rep.int(HOURMAP_TARGET_DAYS, 24L))
    expect_identical(
        mapped$value,
        rep(as.numeric(0:23), HOURMAP_TARGET_DAYS)
    )
    expect_equal(
        mapped$target_phase,
        seq.int(0L, HOURMAP_TARGET_HOURS - 1L) / HOURMAP_TARGET_HOURS,
        tolerance = 1e-12
    )
    expect_identical(
        mapped$source_second_of_day,
        rep(as.numeric(0:23) * 3600, HOURMAP_TARGET_DAYS)
    )
    expect_identical(mapped$hour_phase_seconds, 0)
})

test_that("point variables use circular annual-phase interpolation", {
    calendars <- CF_TIME_CALENDARS
    years <- ifelse(
        calendars %in% c(
            "standard",
            "gregorian",
            "proleptic_gregorian"
        ),
        2064L,
        2061L
    )
    for (index in seq_along(calendars)) {
        adjusted <- hourmap_test__adjusted(
            "tas",
            years[[index]],
            calendars[[index]],
            value = function(phase) 280 + 5 * sin(2 * pi * phase)
        )
        sequence <- sequence__direct_model_generate(
            hourmap_test__execution(list(adjusted)),
            NULL,
            NULL,
            list()
        )
        result <- hourmap__reconstruct(
            sequence,
            hourmap_test__inputs(),
            NULL,
            list()
        )
        series <- result@members[[1L]]@series[[1L]]
        expected <- 280 + 5 * sin(
            2 * pi * series@data$target_annual_phase
        )

        expect_equal(
            series@data$value,
            expected,
            tolerance = 1e-6,
            info = calendars[[index]]
        )
        expected_method <- if (
            cf_time__year_days(years[[index]], calendars[[index]])[[1L]] ==
                HOURMAP_TARGET_DAYS
        ) {
            "identity_365_day"
        } else {
            "circular_linear_annual_phase"
        }
        expect_identical(
            unique(series@data$mapping_method),
            expected_method,
            info = calendars[[index]]
        )
        expect_identical(nrow(series@data), HOURMAP_TARGET_HOURS)
    }
})

test_that("calendar mapping preserves every source time-of-day position", {
    for (variable in c("tas", "rsds")) {
        adjusted <- hourmap_test__adjusted(
            variable,
            2061L,
            "360_day",
            value = function(phase, second_of_day) {
                second_of_day / 3600
            }
        )
        sequence <- sequence__direct_model_generate(
            hourmap_test__execution(list(adjusted)),
            NULL,
            NULL,
            list()
        )
        result <- hourmap__reconstruct(
            sequence,
            hourmap_test__inputs(),
            NULL,
            list()
        )
        data <- result@members[[1L]]@series[[1L]]@data

        expect_equal(
            data$value,
            rep(as.numeric(0:23), HOURMAP_TARGET_DAYS),
            tolerance = 1e-12,
            info = variable
        )
        expect_identical(
            data$source_second_of_day,
            rep(as.numeric(0:23) * 3600, HOURMAP_TARGET_DAYS),
            info = variable
        )
    }
})

test_that("interval-mean variables conserve their normalized annual mean", {
    adjusted <- hourmap_test__adjusted(
        "rsds",
        2061L,
        "360_day",
        value = function(phase) 200 + 100 * pmax(0, sin(2 * pi * phase))
    )
    sequence <- sequence__direct_model_generate(
        hourmap_test__execution(list(adjusted)),
        NULL,
        NULL,
        list()
    )
    result <- hourmap__reconstruct(
        sequence,
        hourmap_test__inputs(),
        NULL,
        list()
    )
    series <- result@members[[1L]]@series[[1L]]

    expect_equal(
        mean(series@data$value),
        mean(adjusted@data$value),
        tolerance = 1e-10
    )
    expect_equal(
        series@diagnostics$annual_mean_error,
        0,
        tolerance = 1e-10
    )
    expect_identical(
        unique(series@data$mapping_method),
        "conservative_normalized_interval"
    )
})

test_that("hourly calendar mapping is registered and contract-compatible", {
    hourmap__register_component()
    component <- component__get(
        "hourly",
        "direct_model_epw_calendar_mapping"
    )
    upstream <- sequence__direct_model_component()

    expect_s7_class(component, WeatherComponentSpec)
    expect_identical(component@input_kinds, "direct_model_sequence")
    expect_identical(
        component@output_kinds,
        "epw_hourly_climate_sequence"
    )
    expect_false(component@stochastic)
    expect_identical(component@metadata$target_days, HOURMAP_TARGET_DAYS)
    expect_true(component__compatible(upstream, component))
    expect_invisible(component__validate_inputs(
        component,
        hourmap_test__inputs()
    ))
})

test_that("hourly calendar mapping rejects ambiguous source contracts", {
    daily_fields <- cf_time_offset2date(
        0:364,
        data.frame(year = 2061L, month = 1L, day = 1L),
        "noleap"
    )
    daily_fields$hour <- 12L
    daily_fields$minute <- 0L
    daily_fields$second <- 0
    daily <- data.frame(
        variable_id = rep.int("tas", 365L),
        value = seq_len(365L),
        units = rep.int("K", 365L),
        frequency = rep.int("day", 365L),
        cf_time__coordinates(daily_fields, "noleap"),
        stringsAsFactors = FALSE
    )
    daily_adjusted <- bias__daily_adjusted_series(
        daily,
        output_role = "model_future",
        transformation = "test_daily_adjustment"
    )
    daily_sequence <- sequence__direct_model_generate(
        hourmap_test__execution(list(daily_adjusted)),
        NULL,
        NULL,
        list()
    )
    expect_error(
        hourmap__reconstruct(
            daily_sequence,
            hourmap_test__inputs(),
            NULL,
            list()
        ),
        "requires an hourly direct-model sequence"
    )

    first <- hourmap_test__adjusted("tas", 2061L)
    second <- hourmap_test__adjusted(
        "tas",
        2061L,
        value = function(phase) phase + 1
    )
    duplicate_sequence <- sequence__direct_model_generate(
        hourmap_test__execution(list(first, second)),
        NULL,
        NULL,
        list()
    )
    expect_error(
        hourmap__reconstruct(
            duplicate_sequence,
            hourmap_test__inputs(),
            NULL,
            list()
        ),
        "duplicate direct-model variable groups"
    )

    unsupported <- hourmap_test__adjusted("pr", 2061L)
    unsupported_sequence <- sequence__direct_model_generate(
        hourmap_test__execution(list(unsupported)),
        NULL,
        NULL,
        list()
    )
    expect_error(
        hourmap__reconstruct(
            unsupported_sequence,
            hourmap_test__inputs(),
            NULL,
            list()
        ),
        "no declared native-calendar-to-EPW hourly mapping semantics"
    )
    expect_error(
        hourmap__target_grid("not-an-epw"),
        "must contain an internal.*EpwFile"
    )
})
