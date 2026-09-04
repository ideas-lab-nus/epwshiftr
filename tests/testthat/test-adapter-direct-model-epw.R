# Return the EPW role-addressable input used by the standalone physics stage.
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

# Construct one already-mapped hourly variable so tests can isolate physical
# closure from the preceding signal, sequence, and calendar components.
epwphys_test__series <- function(
    variable,
    value,
    unit,
    year = 2064L,
    calendar = "noleap"
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
        target_annual_phase = (
            (target$epw_day - 1) + (target$hour - 1) / 24
        ) / HOURMAP_TARGET_DAYS,
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

# Assemble a one-year mapped sequence from named variable specifications.
epwphys_test__sequence <- function(
    values,
    units,
    year = 2064L,
    calendar = "noleap"
) {
    stopifnot(identical(names(values), names(units)))
    series <- Map(
        function(variable, value, unit) {
            epwphys_test__series(
                variable,
                value,
                unit,
                year,
                calendar
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

# Execute the component with the empty option set used by the generic runner.
epwphys_test__apply <- function(values, units, year = 2064L) {
    direct_epw__apply(
        epwphys_test__sequence(values, units, year),
        epwphys_test__inputs(),
        NULL,
        list()
    )
}

test_that("relative humidity and scalar wind close an EPW weather year", {
    input <- epwphys_test__inputs()
    template <- weather__get_input(input, "weather_template")@source$data()
    result <- direct_epw__apply(
        epwphys_test__sequence(
            list(
                tas = 293.15,
                ps = 101325,
                hurs = 50,
                sfcWind = 3,
                rsds = 500,
                rsdsdiff = 100
            ),
            list(
                tas = "K",
                ps = "Pa",
                hurs = "%",
                sfcWind = "m s-1",
                rsds = "W m-2",
                rsdsdiff = "W m-2"
            )
        ),
        input,
        NULL,
        list()
    )

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
    expect_true(all(weather$dew_point_temperature <=
        weather$dry_bulb_temperature))
    expect_equal(weather$wind_speed, rep.int(3, 8760))
    expect_identical(weather$wind_direction, template$wind_direction)
    expect_identical(weather$total_sky_cover, template$total_sky_cover)
    expect_true("wind_direction" %in% member@provenance$inherited_fields)
    expect_false("wind_direction" %in% result@constructed_fields)

    geometry <- direct_epw__solar_geometry(template, input@weather_template@source)
    projection <- geometry$effective_solar_projection
    daylight <- projection > 1e-8
    expect_equal(
        weather$global_horizontal_radiation[daylight],
        weather$diffuse_horizontal_radiation[daylight] +
            weather$direct_normal_radiation[daylight] *
                projection[daylight],
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
    expect_true(all(weather$dew_point_temperature <=
        weather$dry_bulb_temperature))
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
    base_values <- list(
        tas = 293.15,
        ps = 101325,
        hurs = 50,
        sfcWind = 3,
        rsds = 500,
        rsdsdiff = 100
    )
    base_units <- list(
        tas = "K",
        ps = "Pa",
        hurs = "%",
        sfcWind = "m/s",
        rsds = "W/m^2",
        rsdsdiff = "W/m^2"
    )

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

test_that("the registered physics component follows the hourly contract", {
    direct_epw__register_component()
    component <- component__get(
        "physics",
        "epw_hourly_physical_closure"
    )
    upstream <- hourmap__component()

    expect_s7_class(component, WeatherComponentSpec)
    expect_identical(component@input_kinds, "epw_hourly_climate_sequence")
    expect_identical(component@output_kinds, "epw_hourly_weather_sequence")
    expect_false(component@stochastic)
    expect_true(component__compatible(upstream, component))
    expect_identical(
        component@metadata$humidity_alternatives,
        c("hurs", "huss")
    )
    expect_invisible(component__validate_inputs(
        component,
        epwphys_test__inputs()
    ))
})
