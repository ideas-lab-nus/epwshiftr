test_that("extract_location_dict()", {
    skip_on_cran()
    expect_null(extract_location_dict("abcdefghijk"))

    mockery::stub(extract_location_dict, "utils::menu", 1L)
    expect_is(res <- extract_location_dict("Singapore"), class = "data.table")
    expect_equal(names(res),
        c("title", "location", "state_province", "country", "wmo_region",
            "wmo_number", "source_type", "longitude", "latitude", "epw_url",
            "ddy_url", "stat_url", "zip_url", "provider", "index")
    )
    expect_equal(nrow(res), 1L)

    mockery::stub(extract_location_dict, "utils::menu", 1L)
    expect_is(res <- extract_location_dict("China"), class = "data.table")
    expect_equal(names(res),
        c("title", "location", "state_province", "country", "wmo_region",
            "wmo_number", "source_type", "longitude", "latitude", "epw_url",
            "ddy_url", "stat_url", "zip_url", "provider", "index")
    )
    expect_equal(nrow(res), 1L)

    mockery::stub(extract_location_dict, "utils::menu", 0L)
    expect_null(extract_location_dict("Singapore"))

    mockery::stub(extract_location_dict, "utils::menu", 0L)
    expect_null(extract_location_dict("China"))
})

test_that("match_nc_coord()", {
    skip_on_cran()
    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        loc <- eplusr:::WEATHER_DB[grepl("Singapore", title)]

        # can stop if threshold is too small
        expect_error(match_nc_coord(path, loc$latitude, loc$longitude, threshold = list(lat = 0.01, lon = 1.0)))
        expect_error(match_nc_coord(path, loc$latitude, loc$longitude, threshold = list(lat = 1.0, lon = 0.01)))

        # can specify maximum matched number
        expect_is(matched <- match_nc_coord(path, loc$latitude, loc$longitude, max_num = 1L), "data.table")
        expect_equal(names(matched), c("index", "ind_lon", "ind_lat", "lon", "lat", "dist"))
        expect_equal(nrow(matched), 1L)

        # can match multiple values
        expect_is(matched <- match_nc_coord(path, loc$latitude, loc$longitude, threshold = list(lat = 1.0, lon = 1.0)), "data.table")
        expect_equal(names(matched), c("index", "ind_lon", "ind_lat", "lon", "lat", "dist"))
        expect_equal(nrow(matched), 6L)

        # can change EPW longitude to [0, 360] range
        loc <- eplusr:::WEATHER_DB[title == "USA_NY_New.York.City-Central.Park.744860_TMY2"]
        expect_is(matched <- match_nc_coord(path, loc$latitude, loc$longitude, threshold = list(lat = 1, lon = 1), max_num = 1), "data.table")
        expect_equal(names(matched), c("index", "ind_lon", "ind_lat", "lon", "lat", "dist"))
        expect_equal(nrow(matched), 1L)

        # can work with NetCDF object
        con <- RNetCDF::open.nc(path)
        expect_equal(match_nc_coord(con, loc$latitude, loc$longitude, max_num = 1), matched)
        RNetCDF::close.nc(con)
    }
})

test_that("match_location_coord()", {
    skip_on_cran()
    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        loc <- eplusr:::WEATHER_DB[grepl("Singapore", title)]

        expect_is(matched <- match_location_coord(path, as.list(loc)), "data.table")
        expect_equal(names(matched), c("index", "ind_lon", "ind_lat", "lon", "lat", "dist"))
        expect_equal(nrow(matched), 6L)
    }
})

test_that("match_coord()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "SGP_Singapore.486980_IWEC.epw"
    path <- file.path(cache, file)

    if (!file.exists(path)) {
        eplusr::download_weather("SGP_Singapore.486980_IWEC", dir = cache, type = "epw", ask = FALSE, max_match = 1)
    }

    if (file.exists(path)) {
        options(epwshiftr.dir = tempdir())
        summary_database(cache, update = TRUE)

        idx <- load_cmip6_index()
        # can stop if no database is idendified
        idx1 <- copy(idx)[, file_path := NULL]
        set_cmip6_index(idx1)
        expect_error(match_coord(path), "No NetCDF database")

        # test if load_cmip6_index() can fix column types
        idx2 <- copy(idx)
        idx2[, file_mtime := as.character(file_mtime)]
        idx2[, datetime_start := as.character(datetime_start)]
        idx2[, datetime_end := as.character(datetime_end)]
        set_cmip6_index(idx2, TRUE)
        withr::with_options(
            list(datatable.old.fread.datetime.character = TRUE),
            expect_equivalent(load_cmip6_index(TRUE)[, -"file_mtime"], idx[, -"file_mtime"])
        )

        # can work with Epw object
        expect_is(res1 <- match_coord(eplusr::read_epw(path)), "epw_cmip6_coord")
        expect_equal(names(res1), c("epw", "meta", "coord"))
        expect_is(res1$epw, "Epw")
        expect_equal(names(res1$meta), c("city", "state_province", "country", "latitude", "longitude"))
        expect_equal(nrow(res1$coord), 3L)
        expect_equal(names(res1$coord),
            c("file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
              "source_id", "experiment_id", "member_id", "table_id", "frequency",
              "grid_label", "version", "nominal_resolution", "variable_id",
              "variable_long_name", "variable_units", "datetime_start", "datetime_end",
              "file_size", "data_node", "file_url", "dataset_pid", "tracking_id",
              "file_path", "file_realsize", "file_mtime", "time_units", "time_calendar",
              "coord"
            )
        )
        expect_is(res1$coord$coord, "list")
        expect_is(res1$coord$coord[[1]], "data.table")
        expect_equal(names(res1$coord$coord[[1]]), c("index", "ind_lon", "ind_lat", "lon", "lat", "dist"))

        # can work with EPW file path
        expect_is(res2 <- match_coord(path), "epw_cmip6_coord")
        expect_equal(res2$meta, res1$meta)
        expect_equal(res2$coord, res1$coord)

        # can select the location interactively
        mockery::stub(match_coord, "extract_location_dict", eplusr:::WEATHER_DB[grepl("Singapore", title)][1L])
        expect_is(res3 <- match_coord("Singapore"), "epw_cmip6_coord")

        mockery::stub(match_coord, "extract_location_dict", NULL)
        expect_null(match_coord("abcdefghijk"))
    }
})
