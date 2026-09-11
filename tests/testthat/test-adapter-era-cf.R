test_that("ERA NetCDF reading selects the nearest returned grid centre", {
    path <- tempfile(fileext = ".nc")
    time <- 0:2
    values <- array(
        c(280, 290, 281, 291, 282, 292),
        dim = c(2, 1, 3)
    )
    write_test_era_netcdf(path, values = values, time = time)
    site <- shift_site(id = "SFO", lon = -122.49, lat = 37.75,
        epw = get_cache_epw())

    result <- era__read_netcdf(path, "tas", site)

    expect_equal(result$data$grid_lon, rep(-122.5, 3))
    expect_equal(result$data$value, c(280, 281, 282))
    expect_identical(result$units, "K")
})

test_that("ERA normalization closes humidity, wind, flux, and local-day fields", {
    utc_time <- seq(
        as.POSIXct("1999-12-31 00:00:00", tz = "UTC"),
        as.POSIXct("2000-01-03 23:00:00", tz = "UTC"),
        by = "hour"
    )
    row <- function(value) {
        list(
            data = data.table::data.table(
                utc_time = utc_time,
                value = value,
                grid_lat = 37.75,
                grid_lon = -122.5,
                grid_dist_km = 11
            ),
            units = ""
        )
    }
    raw <- list(
        tas = row(273.15 + rep(0:23, length.out = length(utc_time))),
        tdps = row(268.15 + rep(0:23, length.out = length(utc_time))),
        ps = row(rep(101325, length(utc_time))),
        uas = row(rep(3, length(utc_time))),
        vas = row(rep(4, length(utc_time))),
        rsds = row(rep(3600000, length(utc_time))),
        pr = row(rep(0.0036, length(utc_time)))
    )
    raw$tas$units <- raw$tdps$units <- "K"
    raw$ps$units <- "Pa"
    raw$uas$units <- raw$vas$units <- "m s-1"
    raw$rsds$units <- "J m-2"
    raw$pr$units <- "m"
    variables <- c(
        "tas", "tasmin", "tasmax", "hurs", "huss", "sfcWind",
        "rsds", "pr"
    )
    canonical <- era__canonical_hourly(raw, variables)

    expect_equal(canonical$data$sfcWind, rep(5, nrow(canonical$data)))
    expect_true(all(canonical$data$hurs >= 0 & canonical$data$hurs <= 100))
    expect_true(all(canonical$data$huss >= 0))

    rsds <- era__convert_source(raw$rsds, "rsds")
    pr <- era__convert_source(raw$pr, "pr")
    expect_equal(rsds$value, rep(1000, length(utc_time)))
    expect_equal(pr$value, rep(1e-3, length(utc_time)))

    daily <- era__aggregate_variable(
        canonical$data,
        "tasmin",
        "day",
        timezone = -8,
        years = 2000,
        lon = -122.5,
        lat = 37.75
    )
    expect_true(all(daily$cf_year == 2000L))
    expect_equal(daily$value[[1L]], min(
        canonical$data[
            utc_time >= as.POSIXct("2000-01-01 08:00:00", tz = "UTC") &
                utc_time < as.POSIXct("2000-01-02 08:00:00", tz = "UTC"),
            tas
        ]
    ))
    expect_identical(unique(daily$time_basis), "epw_local_standard")
})

test_that("reanalysis materialization persists and reuses ShiftClimate data", {
    store_path <- tempfile("era-store-")
    store <- EsgStore$new(store_path)
    store$close()
    site <- shift_site(epw = get_cache_epw(), id = "SIN")
    stage <- shift_stage_new(
        ShiftFiles,
        "files",
        store_path = store_path
    )
    recipe <- transform__recipe(daily_transform("isimip3basd"))
    source <- shift_era5(1999:2000)
    calls <- 0L
    retrieve <- function(dataset_id, request, target, reporter = NULL, ...) {
        calls <<- calls + 1L
        dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
        writeBin(as.raw(1:10), target)
        list(
            path = target,
            job = list(request_id = sprintf("job-%d", calls)),
            reused = FALSE
        )
    }
    reader <- function(path, source_variable, site) {
        time <- seq(
            as.POSIXct("1998-12-31 00:00:00", tz = "UTC"),
            as.POSIXct("2001-01-02 23:00:00", tz = "UTC"),
            by = "hour"
        )
        list(
            path = path,
            units = "K",
            data = data.table::data.table(
                utc_time = time,
                value = 298 + sin(seq_along(time) / 24),
                grid_lat = site@lat,
                grid_lon = site@lon,
                grid_dist_km = 0
            )
        )
    }
    withr::local_options(list(
        epwshiftr.reanalysis.retrieve = retrieve,
        epwshiftr.reanalysis.read = reader
    ))

    climate <- reanalysis__materialize(
        stage,
        recipe,
        site,
        source
    )
    expect_true(S7::S7_inherits(climate, ShiftClimate))
    expect_true(all(shift_coverage(climate)$complete))
    expect_identical(calls, 1L)
    store <- EsgStore$new(store_path, create = FALSE)
    on.exit(store$close(), add = TRUE)
    expect_true("grid_elevation_m" %in% names(
        priv(store)$read_table("extraction_grid_source")
    ))

    reused <- reanalysis__materialize(stage, recipe, site, source)
    expect_true(reused@meta$reused)
    expect_identical(calls, 1L)
})

test_that("reanalysis identities include the EPW fixed time zone", {
    original <- epw_file_read(get_cache_epw())
    shifted <- epw_file_read(get_cache_epw())
    location <- shifted$header("LOCATION")
    location[[8L]] <- as.character(as.numeric(location[[8L]]) - 1)
    shifted$header("LOCATION", location)
    shifted_path <- tempfile(fileext = ".epw")
    shifted$save(shifted_path)
    source <- shift_era5(1999:2000)
    variables <- "tas"
    frequencies <- c(tas = "day")
    original_site <- shift_site(
        id = "same-site",
        lon = 103.98,
        lat = 1.37,
        epw = original
    )
    shifted_site <- shift_site(
        id = "same-site",
        lon = 103.98,
        lat = 1.37,
        epw = shifted
    )

    original_ids <- reanalysis__identities(
        source,
        original_site,
        variables,
        frequencies,
        "arco"
    )
    shifted_ids <- reanalysis__identities(
        source,
        shifted_site,
        variables,
        frequencies,
        "arco"
    )

    expect_false(identical(original_ids$query_id, shifted_ids$query_id))
})
