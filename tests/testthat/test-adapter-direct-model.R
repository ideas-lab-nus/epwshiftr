# Build one complete hourly native-calendar series with values defined by the
# supplied annual-phase function so calendar remapping remains directly testable.
# hourmap_test__adjusted {{{
hourmap_test__adjusted <- function(
    variable,
    year,
    calendar = "noleap",
    value = function(phase) phase,
    units = NULL,
    wind_direction = NULL
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
    if (!is.null(wind_direction)) {
        data$wind_direction <- as.numeric(wind_direction(
            coordinates$annual_phase
        ))
    }
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
# }}}

# Assemble the signal envelope expected by direct_model_realization while
# allowing repeated variables to use distinct group keys in rejection tests.
# hourmap_test__execution {{{
hourmap_test__execution <- function(values) {
    groups <- lapply(seq_along(values), function(index) {
        signal__group(
            key = list(group = paste0("group-", index)),
            inputs = list(model_future = values[[index]]@data),
            variables = unique(values[[index]]@data[["variable_id"]])
        )
    })
    variables <- unique(unlist(
        lapply(
            groups,
            function(group) group@variables
        ),
        use.names = FALSE
    ))
    SignalExecutionResult(
        groups = groups,
        values = values,
        profiles = stats::setNames(
            lapply(variables, function(variable) {
                list(variable_id = variable)
            }),
            variables
        ),
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
# }}}

# Return the complete role-addressable input set required by the standalone
# hourly component without constructing an otherwise incomplete recipe.
# hourmap_test__inputs {{{
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
# }}}

# Run direct-model sequence generation and EPW calendar mapping in one call.
# hourmap_test__run {{{
hourmap_test__run <- function(values) {
    sequence <- sequence__direct_model_generate(
        hourmap_test__execution(values),
        NULL,
        NULL,
        list()
    )
    hourmap__reconstruct(sequence, hourmap_test__inputs(), NULL, list())
}
# }}}

test_that("365-day direct-model hours map exactly onto EPW rows", {
    adjusted <- hourmap_test__adjusted(
        "tas",
        2061L,
        value = function(phase) seq_along(phase)
    )
    result <- hourmap_test__run(list(adjusted))

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
    expect_identical(unique(series@data$mapping_method), "identity_365_day")
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
        calendars %in%
            c(
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
        result <- hourmap_test__run(list(adjusted))
        series <- result@members[[1L]]@series[[1L]]
        expected <- 280 + 5 * sin(2 * pi * series@data$target_annual_phase)

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

test_that("scalar wind direction uses circular vector interpolation", {
    adjusted <- hourmap_test__adjusted(
        "sfcWind",
        2061L,
        "360_day",
        value = function(phase) rep.int(3, length(phase)),
        units = "m/s",
        wind_direction = function(phase) {
            (350 + 20 * sin(2 * pi * phase)) %% 360
        }
    )
    series <- hourmap_test__run(list(adjusted))@members[[1L]]@series[[1L]]
    expected <- (350 + 20 * sin(2 * pi * series@data$target_annual_phase)) %%
        360
    angular_error <- abs(
        ((series@data$wind_direction - expected + 180) %% 360) - 180
    )

    expect_lt(max(angular_error), 1e-3)
    expect_identical(
        series@diagnostics$wind_direction_mapping,
        "circular_vector_interpolation"
    )
})

test_that("calendar mapping preserves every source time-of-day position", {
    for (variable in c("tas", "rsds")) {
        adjusted <- hourmap_test__adjusted(
            variable,
            2061L,
            "360_day",
            value = function(phase, second_of_day) second_of_day / 3600
        )
        data <- hourmap_test__run(list(adjusted))@members[[1L]]@series[[
            1L
        ]]@data

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
    series <- hourmap_test__run(list(adjusted))@members[[1L]]@series[[1L]]

    expect_equal(
        mean(series@data$value),
        mean(adjusted@data$value),
        tolerance = 1e-10
    )
    expect_equal(series@diagnostics$annual_mean_error, 0, tolerance = 1e-10)
    expect_identical(
        unique(series@data$mapping_method),
        "conservative_normalized_interval"
    )
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
    expect_error(
        hourmap_test__run(list(daily_adjusted)),
        "requires an hourly direct-model sequence"
    )

    first <- hourmap_test__adjusted("tas", 2061L)
    second <- hourmap_test__adjusted(
        "tas",
        2061L,
        value = function(phase) phase + 1
    )
    expect_error(
        hourmap_test__run(list(first, second)),
        "duplicate direct-model variable groups"
    )
    expect_error(
        hourmap_test__run(list(hourmap_test__adjusted("pr", 2061L))),
        "no declared native-calendar-to-EPW hourly mapping semantics"
    )
    expect_error(
        hourmap__target_grid("not-an-epw"),
        "must contain an internal.*EpwFile"
    )
})

# Return the EPW role-addressable input used by the standalone physics stage.
# epwphys_test__inputs {{{
epwphys_test__inputs <- function() {
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
# }}}

# Construct one already-mapped hourly variable so tests can isolate physical
# closure from the preceding signal, sequence, and calendar components.
# epwphys_test__series {{{
epwphys_test__series <- function(
    variable,
    value,
    unit,
    year = 2064L,
    calendar = "noleap",
    wind_direction = NULL
) {
    target <- hourmap__target_grid(
        weather__get_input(
            epwphys_test__inputs(),
            "weather_template"
        )@source
    )
    if (length(value) == 1L) {
        value <- rep.int(value, HOURMAP_TARGET_HOURS)
    }
    stopifnot(length(value) == HOURMAP_TARGET_HOURS)
    data <- data.frame(
        target,
        year = rep.int(as.integer(year), HOURMAP_TARGET_HOURS),
        target_annual_phase = ((target$epw_day - 1) + (target$hour - 1) / 24) /
            HOURMAP_TARGET_DAYS,
        variable_id = rep.int(variable, HOURMAP_TARGET_HOURS),
        value = as.numeric(value),
        units = rep.int(unit, HOURMAP_TARGET_HOURS),
        mapping_method = rep.int(
            "identity_365_day",
            HOURMAP_TARGET_HOURS
        ),
        source_calendar = rep.int(calendar, HOURMAP_TARGET_HOURS),
        source_year = rep.int(as.integer(year), HOURMAP_TARGET_HOURS),
        source_second_of_day = rep.int(
            seq.int(0, 23 * 3600, by = 3600),
            HOURMAP_TARGET_DAYS
        ),
        source_hour_phase_seconds = rep.int(0, HOURMAP_TARGET_HOURS),
        stringsAsFactors = FALSE
    )
    if (!is.null(wind_direction)) {
        if (length(wind_direction) == 1L) {
            wind_direction <- rep.int(wind_direction, HOURMAP_TARGET_HOURS)
        }
        data$wind_direction <- as.numeric(wind_direction)
    }
    MappedHourlyClimateSeries(
        group_id = gsub("_", "-", tolower(variable)),
        key = list(variable_id = variable),
        variables = variable,
        data = data,
        diagnostics = data.frame(
            variable_id = variable,
            status = "ok",
            stringsAsFactors = FALSE
        ),
        provenance = list(source = "synthetic")
    )
}
# }}}

# Assemble a one-year mapped sequence from named variable specifications.
# epwphys_test__sequence {{{
epwphys_test__sequence <- function(
    values,
    units,
    year = 2064L,
    calendar = "noleap",
    wind_direction = NULL
) {
    stopifnot(identical(names(values), names(units)))
    series <- Map(
        function(variable, value, unit) {
            epwphys_test__series(
                variable,
                value,
                unit,
                year,
                calendar,
                wind_direction = if (identical(variable, "sfcWind")) {
                    wind_direction
                } else {
                    NULL
                }
            )
        },
        names(values),
        values,
        units
    )
    member <- MappedHourlyClimateMember(
        sequence_id = "test-sequence",
        weather_year = as.integer(year),
        source_calendar = calendar,
        series = unname(series),
        provenance = list(source = "synthetic")
    )
    MappedHourlyClimateSequence(
        members = list(member),
        frequency = "hour",
        time_step_seconds = 3600,
        target_calendar = "epw_365_day",
        provenance = list(source = "synthetic")
    )
}
# }}}

EPWPHYS_TEST_VALUES <- list(
    tas = 293.15,
    ps = 101325,
    hurs = 50,
    sfcWind = 3,
    rsds = 500,
    rsdsdiff = 100
)
EPWPHYS_TEST_UNITS <- list(
    tas = "K",
    ps = "Pa",
    hurs = "%",
    sfcWind = "m s-1",
    rsds = "W m-2",
    rsdsdiff = "W m-2"
)

# Execute the component with the empty option set used by the generic runner.
# epwphys_test__apply {{{
epwphys_test__apply <- function(
    values = EPWPHYS_TEST_VALUES,
    units = EPWPHYS_TEST_UNITS,
    year = 2064L,
    wind_direction = NULL,
    inputs = epwphys_test__inputs()
) {
    direct_epw__apply(
        epwphys_test__sequence(
            values,
            units,
            year = year,
            wind_direction = wind_direction
        ),
        inputs,
        NULL,
        list()
    )
}
# }}}

test_that("relative humidity and scalar wind close an EPW weather year", {
    input <- epwphys_test__inputs()
    template <- weather__get_input(input, "weather_template")@source$data()
    result <- epwphys_test__apply(inputs = input)

    expect_s7_class(result, EpwHourlyWeatherSequence)
    expect_identical(result@target_calendar, "epw_365_day")
    expect_identical(
        result@provenance$physical_policy,
        "absolute_model_fields"
    )
    expect_length(result@members, 1L)
    member <- result@members[[1L]]
    weather <- member@data
    expect_s7_class(member, EpwHourlyWeatherMember)
    expect_identical(nrow(weather), HOURMAP_TARGET_HOURS)
    expect_identical(unique(weather$year), 2064L)
    expect_equal(weather$dry_bulb_temperature, rep.int(20, 8760))
    expect_equal(weather$atmospheric_pressure, rep.int(101325, 8760))
    expect_equal(weather$relative_humidity, rep.int(50, 8760))
    expect_true(all(
        weather$dew_point_temperature <= weather$dry_bulb_temperature
    ))
    expect_equal(weather$wind_speed, rep.int(3, 8760))
    expect_identical(weather$wind_direction, template$wind_direction)
    expect_identical(weather$total_sky_cover, template$total_sky_cover)
    expect_true("wind_direction" %in% member@provenance$inherited_fields)
    expect_false("wind_direction" %in% result@constructed_fields)

    geometry <- direct_epw__solar_geometry(
        template,
        input@weather_template@source
    )
    projection <- geometry$effective_solar_projection
    daylight <- projection > 1e-8
    expect_equal(
        weather$global_horizontal_radiation[daylight],
        weather$diffuse_horizontal_radiation[daylight] +
            weather$direct_normal_radiation[daylight] * projection[daylight],
        tolerance = 1e-8
    )
    expect_true(all(weather$global_horizontal_radiation[!daylight] == 0))
    expect_true(all(weather$diffuse_horizontal_radiation[!daylight] == 0))
    expect_true(all(weather$direct_normal_radiation[!daylight] == 0))
    expect_true(all(
        weather$direct_normal_radiation <=
            geometry$extraterrestrial_direct_normal_radiation + 1e-10
    ))
    expect_identical(member@provenance$solar_calendar, "fixed_365_day")
    expect_identical(
        member@provenance$physical_policy,
        "absolute_model_fields"
    )
    expect_gt(member@diagnostics$radiation_excess_beam_reallocated, 0L)
    expect_lt(member@diagnostics$radiation_maximum_closure_error, 1e-8)
    expect_error(
        EpwHourlyWeatherSequence(
            members = list(member, member),
            target_calendar = result@target_calendar,
            constructed_fields = result@constructed_fields,
            provenance = result@provenance
        ),
        "unique ascending years"
    )
})

test_that("specific humidity and vector wind derive dependent EPW fields", {
    result <- epwphys_test__apply(
        list(
            tas = 300,
            ps = 1000,
            huss = 0.01,
            uas = 3,
            vas = 4,
            rsds = 400,
            rsdsdiff = 80,
            rlds = 300
        ),
        list(
            tas = "K",
            ps = "hPa",
            huss = "kg kg-1",
            uas = "m/s",
            vas = "m/s",
            rsds = "W/m2",
            rsdsdiff = "W/m2",
            rlds = "W/m2"
        )
    )

    member <- result@members[[1L]]
    weather <- member@data
    expect_equal(weather$dry_bulb_temperature, rep.int(26.85, 8760))
    expect_equal(weather$atmospheric_pressure, rep.int(100000, 8760))
    expect_true(all(weather$relative_humidity >= 0))
    expect_true(all(weather$relative_humidity <= 100))
    expect_true(all(
        weather$dew_point_temperature <= weather$dry_bulb_temperature
    ))
    expect_equal(weather$wind_speed, rep.int(5, 8760))
    expect_equal(
        weather$wind_direction,
        rep.int((atan2(-3, -4) * 180 / pi) %% 360, 8760)
    )
    expect_equal(
        weather$horizontal_infrared_radiation_intensity_from_sky,
        rep.int(300, 8760)
    )
    expect_identical(member@diagnostics$humidity_source, "huss")
    expect_identical(member@diagnostics$wind_source, "uas_vas")
    expect_true("wind_direction" %in% result@constructed_fields)
    expect_true(
        "horizontal_infrared_radiation_intensity_from_sky" %in%
            result@constructed_fields
    )
})

test_that("scalar wind retains model direction through physical closure", {
    result <- epwphys_test__apply(wind_direction = 225)
    member <- result@members[[1L]]
    expect_equal(member@data$wind_direction, rep.int(225, 8760))
    expect_identical(
        member@diagnostics$wind_direction_policy,
        "supplied_wind_direction"
    )
    expect_true("wind_direction" %in% result@constructed_fields)
})

test_that("physical bounds and radiation closure report every correction", {
    result <- epwphys_test__apply(
        list(
            tas = 400,
            ps = 200000,
            hurs = 120,
            sfcWind = 50,
            rsds = -10,
            rsdsdiff = 600,
            rlds = -5
        ),
        list(
            tas = "K",
            ps = "Pa",
            hurs = "%",
            sfcWind = "m/s",
            rsds = "W/m^2",
            rsdsdiff = "W/m^2",
            rlds = "W/m^2"
        )
    )

    member <- result@members[[1L]]
    weather <- member@data
    diagnostic <- member@diagnostics
    expect_true(all(weather$dry_bulb_temperature == 70))
    expect_true(all(weather$atmospheric_pressure == 120000))
    expect_true(all(weather$relative_humidity == 100))
    expect_true(all(weather$wind_speed == 40))
    expect_true(all(weather$global_horizontal_radiation == 0))
    expect_true(all(weather$diffuse_horizontal_radiation == 0))
    expect_true(all(weather$direct_normal_radiation == 0))
    expect_true(all(
        weather$horizontal_infrared_radiation_intensity_from_sky == 0
    ))
    expect_identical(diagnostic$temperature_clipped, 8760L)
    expect_identical(diagnostic$pressure_clipped, 8760L)
    expect_identical(diagnostic$humidity_saturation_clipped, 8760L)
    expect_identical(diagnostic$wind_speed_clipped, 8760L)
    expect_identical(diagnostic$radiation_negative_global_clipped, 8760L)
    expect_gt(diagnostic$radiation_diffuse_above_global_clipped, 0L)
    expect_identical(diagnostic$infrared_negative_clipped, 8760L)
})

test_that("ambiguous and incomplete physical input contracts are rejected", {
    skip_on_cran()
    base_values <- EPWPHYS_TEST_VALUES
    base_units <- EPWPHYS_TEST_UNITS

    expect_error(
        epwphys_test__apply(
            append(base_values, list(huss = 0.01)),
            append(base_units, list(huss = "1"))
        ),
        "exactly one humidity path"
    )
    expect_error(
        epwphys_test__apply(
            append(base_values, list(uas = 3)),
            append(base_units, list(uas = "m/s"))
        ),
        "provide `uas` and `vas` together"
    )
    expect_error(
        epwphys_test__apply(
            append(base_values, list(uas = 3, vas = 4)),
            append(base_units, list(uas = "m/s", vas = "m/s"))
        ),
        "exactly one wind path"
    )
    expect_error(
        epwphys_test__apply(
            base_values[names(base_values) != "ps"],
            base_units[names(base_units) != "ps"]
        ),
        "missing required EPW physical variable"
    )
    expect_error(
        epwphys_test__apply(
            append(base_values, list(psl = 101325)),
            append(base_units, list(psl = "Pa"))
        ),
        "unsupported EPW physical variable"
    )
    unsupported_units <- base_units
    unsupported_units$tas <- "degree_Fahrenheit"
    expect_error(
        epwphys_test__apply(base_values, unsupported_units),
        "unsupported unit"
    )
})

test_that("direct-model output retains every physically closed weather year", {
    first <- epwphys_test__apply(year = 2061L)
    second <- epwphys_test__apply(year = 2062L)
    closed <- EpwHourlyWeatherSequence(
        members = list(first@members[[1L]], second@members[[1L]]),
        target_calendar = first@target_calendar,
        constructed_fields = first@constructed_fields,
        provenance = first@provenance
    )
    input <- epwphys_test__inputs()
    context <- morpher__context(
        epw = weather__get_input(input, "weather_template")@source,
        climate = data.table::data.table(
            time = as.POSIXct("2061-01-01", tz = "UTC"),
            variable_id = "tas",
            period = "future",
            year = 2061L,
            lon = 104,
            lat = 1,
            units = "K",
            value = 300
        ),
        recipe = suppressWarnings(epw_morph_recipe(
            "original_morphing_absolute"
        ))
    )
    result <- sequence__epw_output_write(
        closed,
        input,
        context,
        list(),
        list()
    )

    expect_s7_class(result, WeatherSequenceResult)
    expect_identical(result@output_type, "multi_year")
    expect_identical(
        vapply(result@members, function(m) m@weather_year, integer(1L)),
        c(2061L, 2062L)
    )
    expect_identical(result@provenance$method, "direct_model_epw_result")
    expect_identical(nrow(result@diagnostics), 2L)
    expect_true(all(
        result@diagnostics$physical_policy == "absolute_model_fields"
    ))
    expect_true("dew_point_temperature" %in% result@parts$constructed_fields)
    expect_error(
        sequence__epw_output_write(first, input, context, list(), list()),
        "at least two complete weather years"
    )
})

# vim: fdm=marker :
