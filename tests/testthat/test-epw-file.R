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

# Compare the public timestamp behavior with the established string parser,
# including unusual inputs that remain on the compatibility path.
test_that("EPW numeric timestamps preserve Gregorian dates and UTC attributes", {
    skip_on_cran()
    # The former implementation is an independent oracle for date conversion.
    reference <- function(year, month, day, hour) {
        safe_year <- as.integer(year)
        safe_year[
            is.na(safe_year) | safe_year < 1600L | safe_year > 9999L
        ] <- 2001L
        start <- as.POSIXct(
            sprintf(
                "%04d-%02d-%02d 00:00:00",
                safe_year,
                as.integer(month),
                as.integer(day)
            ),
            tz = "UTC"
        )
        start + as.numeric(hour) * 3600
    }
    # A complete Gregorian leap cycle covers ordinary, century, and 400-year
    # leap years, with every day checked through the original interface.
    dates <- as.POSIXlt(
        seq(as.Date("1800-01-01"), as.Date("2199-12-31"), by = "day"),
        tz = "UTC"
    )
    args <- list(
        dates$year + 1900L,
        dates$mon + 1L,
        dates$mday,
        rep(c(0, 1, 12.5, 24), length.out = length(dates))
    )
    expect_identical(
        do.call(epw_file__datetime, args),
        do.call(reference, args)
    )
    examples <- list(
        list(
            c(1600, 1900, 2000, 2100, 9999),
            rep(12, 5),
            rep(31, 5),
            rep(24, 5)
        ),
        list(
            c(NA, 0, 1599, 10000, 2000),
            rep(2, 5),
            rep(28, 5),
            c(NA, -1, 24, 25, Inf)
        ),
        list(integer(), integer(), integer(), integer()),
        list(c(2000, 2001), c(2, NA), c(29, 1), c(1, 24)),
        list(2000, c(1, 2), c(1, 29), c(1, 24)),
        list(c(2000, 2001), 1, 1, c(0, 24)),
        list(c(2000, 2000), c(2, 2), c(30, 31), c(1, 24)),
        list(c(2000, 2000), c(0, 13), c(1, 1), c(1, 24)),
        list(c(2000, 2000), c(1, 1), c(0, 32), c(1, 24)),
        list(NA_integer_, NA_integer_, NA_integer_, NA_real_),
        list(c(2000.9, 2001.2), c(2.5, 1.5), c(29.5, 1.5), c(1, 24)),
        list(c(a = 2000, b = 2001), c(1, 1), c(1, 1), c(1, 24))
    )
    # Errors are part of the existing behavior when no input date can parse.
    capture <- function(fun, args) {
        tryCatch(do.call(fun, args), error = function(e) {
            list(class = class(e), message = conditionMessage(e))
        })
    }
    for (args in examples) {
        expect_identical(
            capture(epw_file__datetime, args),
            capture(reference, args)
        )
    }
    withr::local_timezone("America/New_York")
    expect_identical(
        epw_file__datetime(2020, 3, 8, 2),
        reference(2020, 3, 8, 2)
    )
})

# vim: fdm=marker :
