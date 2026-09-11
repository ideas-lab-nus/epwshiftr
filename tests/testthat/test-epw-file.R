test_that("EPW calculation weather recognizes every documented missing code", {
    expected_numeric <- c(
        dry_bulb_temperature = 99.9,
        dew_point_temperature = 99.9,
        relative_humidity = 999,
        atmospheric_pressure = 999999,
        extraterrestrial_horizontal_radiation = 9999,
        extraterrestrial_direct_normal_radiation = 9999,
        horizontal_infrared_radiation_intensity_from_sky = 9999,
        global_horizontal_radiation = 9999,
        direct_normal_radiation = 9999,
        diffuse_horizontal_radiation = 9999,
        global_horizontal_illuminance = 999999,
        direct_normal_illuminance = 999999,
        diffuse_horizontal_illuminance = 999999,
        zenith_luminance = 9999,
        wind_direction = 999,
        wind_speed = 999,
        total_sky_cover = 99,
        opaque_sky_cover = 99,
        visibility = 9999,
        ceiling_height = 99999,
        present_weather_observation = 9,
        precipitable_water = 999,
        aerosol_optical_depth = 0.999,
        snow_depth = 999,
        days_since_last_snow = 99,
        albedo = 999,
        liquid_precip_depth = 999,
        liquid_precip_rate = 99
    )
    actual_numeric <- vapply(
        EPW_FILE_FIELD_SPECS,
        `[[`,
        numeric(1L),
        "missing_value"
    )
    expected_threshold <- c(
        global_horizontal_illuminance = 999900,
        direct_normal_illuminance = 999900,
        diffuse_horizontal_illuminance = 999900,
        zenith_luminance = 9999
    )
    actual_threshold <- vapply(
        EPW_FILE_FIELD_SPECS,
        `[[`,
        numeric(1L),
        "missing_from"
    )

    expect_equal(actual_numeric, expected_numeric)
    expect_equal(
        actual_threshold[is.finite(actual_threshold)],
        expected_threshold
    )
    expect_identical(
        EPW_FILE_TEXT_MISSING_CODES,
        c(present_weather_codes = "999999999")
    )
})

test_that("EPW calculation weather separates missing codes from raw data", {
    weather <- data.table::data.table(
        month = c(1L, 2L),
        liquid_precip_depth = c(1000, 999),
        liquid_precip_rate = c(100, 99),
        global_horizontal_radiation = c(10000, 9999),
        global_horizontal_illuminance = c(100, 999900),
        zenith_luminance = c(100, 10000),
        ceiling_height = c(77777, 99999),
        aerosol_optical_depth = c(0.1, 0.999),
        present_weather_observation = c(0L, 9L),
        present_weather_codes = c("000000000", "999999999")
    )
    raw <- data.table::copy(weather)

    calculation <- epw_file__calculation_weather(weather)

    expect_identical(weather, raw)
    expect_equal(calculation$liquid_precip_depth, c(1000, NA_real_))
    expect_equal(calculation$liquid_precip_rate, c(100, NA_real_))
    expect_equal(calculation$global_horizontal_radiation, c(10000, NA_real_))
    expect_equal(calculation$global_horizontal_illuminance, c(100, NA_real_))
    expect_equal(calculation$zenith_luminance, c(100, NA_real_))
    # 77777 means an unlimited ceiling rather than missing weather data.
    expect_equal(calculation$ceiling_height, c(77777, NA_real_))
    expect_equal(calculation$aerosol_optical_depth, c(0.1, NA_real_))
    expect_equal(calculation$present_weather_observation, c(0L, NA_integer_))
    expect_identical(
        calculation$present_weather_codes,
        c("000000000", NA_character_)
    )

    summary <- epw_file__missing_summary(weather)
    expect_equal(
        summary[epw_field == "liquid_precip_depth", missing_hours],
        c(0L, 1L)
    )
    expect_equal(
        summary[epw_field == "present_weather_codes", missing_hours],
        c(0L, 1L)
    )
})

test_that("EPW serialization restores canonical missing codes", {
    weather <- data.table::data.table(
        dry_bulb_temperature = c(20, 71),
        global_horizontal_illuminance = c(NA_real_, 999950),
        ceiling_height = c(77777, 88888),
        present_weather_observation = c(NA_integer_, 0L),
        present_weather_codes = c(NA_character_, "000000000"),
        liquid_precip_depth = c(NA_real_, 0),
        liquid_precip_rate = c(NA_real_, 1)
    )

    filled <- epw_file_fill_abnormal(weather)

    expect_equal(filled$dry_bulb_temperature, c(20, 99.9))
    expect_equal(
        filled$global_horizontal_illuminance,
        c(999999, 999999)
    )
    expect_equal(filled$ceiling_height, c(77777, 88888))
    expect_identical(filled$present_weather_observation, c(9L, 0L))
    expect_identical(
        filled$present_weather_codes,
        c("999999999", "000000000")
    )
    expect_equal(filled$liquid_precip_depth, c(999, 0))
    expect_equal(filled$liquid_precip_rate, c(99, 1))
})

test_that("shared weather backends cannot treat EPW sentinels as temperature", {
    epw <- epw_file_read(get_cache_epw())
    weather <- epw$data()
    weather[1L, dry_bulb_temperature := 99.9]
    epw$set(weather)

    expect_error(
        temperature__epw_template(epw),
        "dry-bulb temperature must be finite"
    )
})
