# Get test data directory path (for EPW/NetCDF files, not DiskCache objects)
#
# Uses EPWSHIFTR_CHECK_CACHE env var in CI, otherwise a fixed subdir of tempdir().
# The directory is created if it doesn't exist.
test_data_dir <- function() {
    dir <- Sys.getenv("EPWSHIFTR_CHECK_CACHE", NA)
    if (is.na(dir)) {
        dir <- file.path(tempdir(), "epwshiftr-test-data")
    }
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    dir
}

write_local_epw_fixture <- function(path) {
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
    # Provide deterministic wet hours so precipitation morphing can preserve
    # event timing without synthesizing new rainfall sequences.
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
        "COMMENTS 2,Deterministic synthetic data for tests",
        "DATA PERIODS,1,1,Data,Monday,1/1,12/31"
    )

    writeLines(c(header, do.call(paste, c(weather, sep = ","))), path)
    invisible(path)
}

get_cache_epw <- function() {
    dir <- test_data_dir()
    file <- "SGP_Singapore.486980_IWEC.epw"
    epw <- file.path(dir, file)
    # The EPW fixture is tiny, so rewrite it to avoid stale CI cache content
    # from older fixture versions without adding schema/version probes.
    write_local_epw_fixture(epw)
    normalizePath(epw)
}

get_cache_nc <- function(reset = FALSE) {
    dir <- test_data_dir()
    paths <- file.path(dir, vapply(local_cmip6_test_years, local_cmip6_nc_file, character(1)))

    if (reset) unlink(paths, force = TRUE)
    unlink(paths[file.exists(paths)], force = TRUE)

    for (i in seq_along(paths)) {
        write_local_cmip6_netcdf_fixture(paths[[i]], local_cmip6_test_years[[i]])
    }

    normalizePath(dir)
}

read_test_parquet <- function(path) {
    conn <- ddb_connect(":memory:")
    on.exit(ddb_disconnect(conn), add = TRUE)

    data.table::as.data.table(ddb_query(conn, sprintf(
        "SELECT * FROM read_parquet(%s)",
        ddb_literal(conn, path)
    )))
}

get_cache_parquet <- function(reset = FALSE) {
    dir <- get_cache_nc(reset = reset)
    path <- file.path(dir, "EC-Earth3.ssp585.tas.parquet")

    if (reset && file.exists(path)) unlink(path)
    write_local_morph_tas_fixture(path)

    normalizePath(path)
}

# Scoped cache mode switch for tests
local_cache_mode <- function(mode, env = parent.frame()) {
    old <- getOption("epwshiftr.cache")
    withr::defer(options(epwshiftr.cache = old), envir = env)
    opt_val <- switch(
        mode,
        "normal" = TRUE,
        "off" = FALSE,
        "offline" = "offline",
        stop(sprintf("Unknown cache mode: '%s'", mode))
    )
    options(epwshiftr.cache = opt_val)
}

# Create a temporary test cache and set it as the package cache
#
# @param scope Character. One of:
#   - "test" (default): ephemeral cache in tempfile(), deleted on exit
#   - "session": persists within the R session (tempdir()-based), not deleted
#   - "persist": persists for the testthat run, then is deleted at teardown
# @param env The environment for withr::defer cleanup
local_test_cache <- function(scope = c("test", "session", "persist"), env = parent.frame()) {
    scope <- match.arg(scope)

    dir <- switch(scope,
        "test" = tempfile("epwshiftr-test-cache-"),
        "session" = file.path(tempdir(), "epwshiftr-test-cache"),
        "persist" = file.path(dirname(tempdir()), "epwshiftr-test-cache")
    )

    cache <- DiskCache$new(dir = dir, max_size = "100 MB", max_age = Inf, max_n = Inf)
    old_cache <- cache__set(cache)
    cleanup_env <- if (identical(scope, "persist")) testthat::teardown_env() else env
    withr::defer(
        {
            cache__set(old_cache)
            if (scope %in% c("test", "persist")) unlink(dir, recursive = TRUE)
        },
        envir = cleanup_env
    )
    cache
}
