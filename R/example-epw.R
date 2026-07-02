#' Create an example EPW file
#'
#' @description
#' `epwshiftr_example_epw()` writes a small deterministic Singapore EPW file
#' that can be used as a baseline for examples, tests, and smoke workflows.
#'
#' @param name Example EPW name. Currently only `"singapore"` is available.
#' @param dir Directory where the EPW file should be written.
#' @param overwrite Whether to replace an existing example file.
#'
#' @return The normalized EPW file path.
#' @export
epwshiftr_example_epw <- function(name = "singapore", dir = tempdir(), overwrite = TRUE) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(overwrite)

    name <- match.arg(tolower(name), "singapore")
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!dir.exists(dir)) {
        cli::cli_abort("Could not create example EPW directory: {.path {dir}}.")
    }

    path <- file.path(dir, "SGP_Singapore.486980_IWEC.epw")
    if (file.exists(path) && !isTRUE(overwrite)) {
        return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }

    switch(
        name,
        singapore = epwshiftr_write_singapore_example_epw(path)
    )
    normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Write a deterministic Singapore EPW with wet hours so precipitation morphing
# examples can preserve event timing without relying on external fixtures.
epwshiftr_write_singapore_example_epw <- function(path) {
    datetime <- seq.POSIXt(
        as.POSIXct("2001-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2001-12-31 23:00:00", tz = "UTC"),
        by = "hour"
    )

    year <- as.integer(format(datetime, "%Y"))
    month <- as.integer(format(datetime, "%m"))
    day <- as.integer(format(datetime, "%d"))
    hour0 <- as.integer(format(datetime, "%H"))
    yday <- as.integer(format(datetime, "%j"))
    daylight <- pmax(0, sin(pi * (hour0 - 6) / 12))
    rain_depth <- ifelse(day %% 7L == 1L & hour0 %in% 3:4, 2.0, 0.0)

    weather <- data.frame(
        year = year,
        month = month,
        day = day,
        hour = hour0 + 1L,
        minute = 60L,
        data_source = "A7A7A7A7*0?0?0?0",
        dry_bulb = sprintf("%.1f", round(27 + 2 * sin(2 * pi * (yday - 81) / 365) + 1.5 * sin(2 * pi * (hour0 - 8) / 24), 1)),
        dew_point = sprintf("%.1f", round(25 + 2 * sin(2 * pi * (yday - 81) / 365) + 1.5 * sin(2 * pi * (hour0 - 8) / 24), 1)),
        relative_humidity = as.integer(round(pmin(pmax(78 - 12 * sin(2 * pi * (hour0 - 8) / 24), 45), 95))),
        atmospheric_pressure = 101325L,
        extraterrestrial_horizontal_radiation = as.integer(round(1000 * daylight)),
        extraterrestrial_direct_normal_radiation = as.integer(round(900 * daylight)),
        horizontal_infrared_radiation_intensity_from_sky = 350L,
        global_horizontal_radiation = as.integer(round(800 * daylight)),
        direct_normal_radiation = as.integer(round(600 * daylight)),
        diffuse_horizontal_radiation = as.integer(round(200 * daylight)),
        global_horizontal_illuminance = as.integer(round(800 * daylight * 120)),
        direct_normal_illuminance = as.integer(round(600 * daylight * 120)),
        diffuse_horizontal_illuminance = as.integer(round(200 * daylight * 120)),
        zenith_luminance = as.integer(round(pmin(9999, 800 * daylight * 12))),
        wind_direction = 180L,
        wind_speed = sprintf("%.1f", round(2.5 + 0.8 * sin(2 * pi * hour0 / 24), 1)),
        total_sky_cover = 5L,
        opaque_sky_cover = 5L,
        visibility = sprintf("%.1f", 20.0),
        ceiling_height = 77777L,
        present_weather_observation = 0L,
        present_weather_codes = "999999999",
        precipitable_water = 35L,
        aerosol_optical_depth = sprintf("%.1f", 0.1),
        snow_depth = 0L,
        days_since_last_snowfall = 88L,
        albedo = sprintf("%.2f", 0.12),
        liquid_precipitation_depth = sprintf("%.1f", rain_depth),
        liquid_precipitation_quantity = sprintf("%.1f", as.numeric(rain_depth > 0)),
        stringsAsFactors = FALSE
    )

    header <- c(
        "LOCATION,Singapore,NA,Singapore,TEST,486980,1.37,103.98,8.0,15.0",
        "DESIGN CONDITIONS,0",
        "TYPICAL/EXTREME PERIODS,0",
        "GROUND TEMPERATURES,0",
        "HOLIDAYS/DAYLIGHT SAVINGS,No,0,0,0",
        "COMMENTS 1,Generated local smoke EPW fixture",
        "COMMENTS 2,Deterministic synthetic data for examples and tests",
        "DATA PERIODS,1,1,Data,Monday,1/1,12/31"
    )

    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(c(header, do.call(paste, c(weather, sep = ","))), path)
    invisible(path)
}
