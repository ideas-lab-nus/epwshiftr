test_that("get_nc_meta()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20590101-20591231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(meta <- get_nc_meta(path), "list")
        expect_equal(names(meta),
            c("mip_era", "activity_id", "institution_id", "source_id",
              "experiment_id", "variant_label", "table_id", "grid_label",
              "nominal_resolution", "variable_id", "tracking_id",
              "standard_name", "units", "time_units", "time_calendar"
            )
        )

        con <- RNetCDF::open.nc(path)
        expect_equivalent(get_nc_meta(con), meta)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_atts()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(atts <- get_nc_atts(path), "data.table")
        expect_equal(names(atts), c("id", "variable", "attribute", "value"))
        expect_true(all(unique(atts$variable) %in% c("height", "lat", "lon", "NC_GLOBAL", "tas", "time")))
        expect_equal(atts[variable == "NC_GLOBAL", unique(id)], -1L)

        con <- RNetCDF::open.nc(path)
        expect_equivalent(get_nc_atts(con), atts)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_vars()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(vars <- get_nc_vars(path), "data.table")
        expect_equal(names(vars), c("id", "name", "type", "ndims", "natts"))

        con <- RNetCDF::open.nc(path)
        expect_equivalent(get_nc_vars(con), vars)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_dims()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(dims <- get_nc_dims(path), "data.table")
        expect_equal(names(dims), c("id", "name", "length", "unlim"))

        con <- RNetCDF::open.nc(path)
        expect_equivalent(get_nc_dims(con), dims)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_axes()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(axes <- get_nc_axes(path), "data.table")
        expect_equal(names(axes), c("axis", "variable", "dimension"))

        con <- RNetCDF::open.nc(path)
        expect_equivalent(get_nc_axes(con), axes)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_time()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(time <- get_nc_time(path), "POSIXct")
        expect_equal(length(time), 366)

        expect_is(time <- get_nc_time(path, range = TRUE), "POSIXct")
        expect_equal(length(time), 2L)

        con <- RNetCDF::open.nc(path)
        expect_equivalent(get_nc_time(con, range = TRUE), time)
        RNetCDF::close.nc(con)
    }
})

test_that("match_nc_time()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(matched <- match_nc_time(path, 2060), "list")
        expect_equal(names(matched), c("datetime", "which"))
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 366L)
        expect_equal(length(matched$which), 1L)
        expect_equal(matched$which[[1L]], 1:366)

        expect_is(matched <- match_nc_time(path), "list")
        expect_equal(names(matched), c("datetime", "which"))
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 366L)
        expect_equal(length(matched$which), 1L)
        expect_equal(matched$which[[1L]], 1:366)

        expect_is(matched <- match_nc_time(path, 2059), "list")
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 0L)
        expect_equal(length(matched$which), 1L)
        expect_equal(length(matched$which[[1L]]), 0L)

        con <- RNetCDF::open.nc(path)
        expect_equal(match_nc_time(con, 2059), matched)
        RNetCDF::close.nc(con)

        expect_is(matched <- match_nc_time(path, 2059:2061), "list")
        expect_equal(length(matched), 2L)
        expect_equal(length(matched$datetime), 3L)
        expect_equal(length(matched$which), 3L)
        expect_equal(length(matched$datetime[[1]]), 0L)
        expect_equal(length(matched$datetime[[2]]), 366L)
        expect_equal(length(matched$datetime[[3]]), 0L)
        expect_equal(length(matched$which[[1]]), 0L)
        expect_equal(length(matched$which[[2]]), 366L)
        expect_equal(length(matched$which[[3]]), 0L)
    }
})

test_that("summary_database()", {
    skip_on_cran()

    cache <- get_cache()
    file_2059 <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20590101-20591231.nc"
    file_2060 <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    file_2061 <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20610101-20611231.nc"
    paths <- file.path(cache, c(file_2059, file_2060, file_2061))
    names(paths) <- c("2059", "2060", "2061")

    options(epwshiftr.dir = tempdir())
    index <- file.path(tempdir(), "cmip6_index.csv")

    if (all(file.exists(paths)) && file.exists(index)) {
        expect_is(db <- summary_database(cache), "data.table")
        expect_true(all(!is.na(load_cmip6_index()$file_path)))
        expect_true(all(!is.na(load_cmip6_index()$file_realsize)))
        expect_true(all(!is.na(load_cmip6_index()$file_mtime)))
        expect_true(all(!is.na(load_cmip6_index()$time_units)))
        expect_true(all(!is.na(load_cmip6_index()$time_calendar)))
        expect_is(not <- attr(db, "not_matched"), "data.table")
        expect_equal(names(not),
            c("mip_era", "activity_id", "institution_id", "source_id", "experiment_id",
              "variant_label", "table_id", "grid_label", "nominal_resolution",
              "variable_id", "tracking_id", "standard_name", "units", "time_units",
              "time_calendar", "datetime_start", "datetime_end", "file_path",
              "file_realsize", "file_mtime"
            )
        )
        expect_equal(nrow(not), 0L)
        expect_equal(as.integer(db$dl_percent), 100L)
        expect_equal(as.integer(db$dl_num), 3L)

        expect_is(db <- summary_database(cache, append = TRUE), "data.table")

        # can work if no NetCDF files were found
        expect_is(db <- summary_database(tempdir()), "data.table")
        expect_equal(names(db),
            c("activity_drs", "experiment_id", "member_id", "table_id",
              "variable_id", "source_id", "nominal_resolution",
              "datetime_start", "datetime_end", "file_num", "file_size",
              "dl_num", "dl_percent", "dl_size"
            )
        )
        expect_equal(db$dl_num, 0L)
        expect_equal(db$dl_percent, units::set_units(0L, "%"))
        expect_equal(attr(db, "not_matched"), data.table())
        idx <- load_cmip6_index()
        set(idx, NULL, 25:28, NULL)
        set_cmip6_index(idx, TRUE)
        expect_is(db <- summary_database(tempdir(), append = TRUE), "data.table")
        expect_equal(names(db),
            c("activity_drs", "experiment_id", "member_id", "table_id",
              "variable_id", "source_id", "nominal_resolution",
              "datetime_start", "datetime_end", "file_num", "file_size",
              "dl_num", "dl_percent", "dl_size"
            )
        )
        expect_equal(db$dl_num, 0L)
        expect_equal(db$dl_percent, units::set_units(0L, "%"))
        expect_equal(attr(db, "not_matched"), data.table())

        # can update local cmip6_index.csv
        expect_is(db <- summary_database(tempdir(), update = TRUE), "data.table")
        expect_equal(nm <- names(load_cmip6_index(TRUE)),
            c("file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
              "source_id", "experiment_id", "member_id", "table_id", "frequency",
              "grid_label", "version", "nominal_resolution", "variable_id",
              "variable_long_name", "variable_units", "datetime_start", "datetime_end",
              "file_size", "data_node", "file_url", "dataset_pid", "tracking_id",
              "file_path", "file_realsize", "file_mtime", "time_units", "time_calendar"
            )
        )
        expect_true(all(is.na(load_cmip6_index()$file_path)))
        expect_true(all(is.na(load_cmip6_index()$file_realsize)))
        expect_true(all(is.na(load_cmip6_index()$file_mtime)))
        expect_true(all(is.na(load_cmip6_index()$time_units)))
        expect_true(all(is.na(load_cmip6_index()$time_calendar)))

        # can give warnings if duplicates found
        file.copy(paths["2059"], file.path(cache, "dup_2059.nc"), copy.date = TRUE)
        expect_warning(summary_database(cache))
        expect_warning(summary_database(cache, mult = "latest"))
        unlink(file.path(cache, "dup_2059.nc"), force = TRUE)
    }
})

test_that("get_nc_data()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        loc <- eplusr:::WEATHER_DB[grepl("Singapore", title)]
        coord <- match_nc_coord(path, loc$latitude, loc$longitude, max_num = 1L)
        expect_is(d <- get_nc_data(path, coord, years = 2060), "data.table")
        expect_equal(names(d),
            c("index", "activity_drs", "institution_id", "source_id", "experiment_id",
              "member_id", "table_id", "datetime", "lat", "lon", "dist", "variable",
              "description", "units", "value"
            )
        )
        expect_is(d$value, "units")

        expect_is(d <- get_nc_data(path, coord, years = 2000), "data.table")
        expect_equal(names(d),
            c("index", "activity_drs", "institution_id", "source_id", "experiment_id",
              "member_id", "table_id", "datetime", "lat", "lon", "dist", "variable",
              "description", "units", "value"
            )
        )
        expect_equal(nrow(d), 0L)
        expect_is(d$value, "numeric")

        con <- RNetCDF::open.nc(path)
        expect_equivalent(get_nc_data(con, coord, years = 2000), d)
        RNetCDF::close.nc(con)
    }
})

test_that("extract_data()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)
    epw <- file.path(cache, "SGP_Singapore.486980_IWEC.epw")

    if (file.exists(path) && file.exists(epw)) {
        options(epwshiftr.dir = tempdir())
        summary_database(cache)
        idx <- load_cmip6_index()
        set_cmip6_index(idx[2L])
        coord <- match_coord(epw)
        set_cmip6_index(idx)

        expect_is(d <- extract_data(coord, years = 2060), "epw_cmip6_data")
        expect_equal(names(d), c("epw", "meta", "data"))
        expect_is(d$epw, "Epw")
        expect_equal(d$meta, coord$meta)
        expect_is(d$data, "data.table")
        expect_equal(names(d$data),
            c("activity_drs", "institution_id", "source_id", "experiment_id",
              "member_id", "table_id", "datetime", "lat", "lon", "dist", "variable",
              "description", "units", "value"
            )
        )
        expect_equal(nrow(d$data), 2196L)

        expect_is(d1 <- extract_data(coord, out_dir = cache), "epw_cmip6_data")
        expect_equal(d1$data, data.table())
        expect_true(file.exists(file.path(cache, "data.fst")))
        unlink(file.path(cache, "data.fst"))

        expect_is(
            d2 <- extract_data(coord, out_dir = cache,
                by = c("source", "experiment", "variable")
            ),
            "epw_cmip6_data"
        )
        expect_equal(d2$data, data.table())
        expect_true(file.exists(file.path(cache, "EC-Earth3.ssp585.tas.fst")))
        unlink(file.path(cache, "EC-Earth3.ssp585.tas.fst"))

        expect_is(
            d3 <- extract_data(coord, out_dir = cache,
                by = c("source", "experiment", "variable"), keep = TRUE
            ),
            "epw_cmip6_data"
        )
        expect_equal(d3$data, d$data)
        expect_true(file.exists(file.path(cache, "EC-Earth3.ssp585.tas.fst")))
        unlink(file.path(cache, "EC-Earth3.ssp585.tas.fst"))
    }
})
