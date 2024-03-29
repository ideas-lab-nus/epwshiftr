test_that("get_nc_meta()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20590101-20591231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_type(meta <- get_nc_meta(path), "list")
        expect_named(meta,
            c("mip_era", "activity_id", "institution_id", "source_id",
              "experiment_id", "variant_label", "table_id", "grid_label",
              "nominal_resolution", "variable_id", "tracking_id",
              "standard_name", "units", "time_units", "time_calendar"
            )
        )

        con <- RNetCDF::open.nc(path)
        expect_equal(get_nc_meta(con), meta, ignore_attr = TRUE)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_atts()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_s3_class(atts <- get_nc_atts(path), "data.table")
        expect_named(atts, c("id", "variable", "attribute", "value"))
        expect_true(all(unique(atts$variable) %in% c("height", "lat", "lon", "NC_GLOBAL", "tas", "time")))
        expect_identical(atts[variable == "NC_GLOBAL", unique(id)], -1L)

        con <- RNetCDF::open.nc(path)
        expect_equal(get_nc_atts(con), atts, ignore_attr = TRUE)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_vars()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_s3_class(vars <- get_nc_vars(path), "data.table")
        expect_named(vars, c("id", "name", "type", "ndims", "natts"))

        con <- RNetCDF::open.nc(path)
        expect_equal(get_nc_vars(con), vars, ignore_attr = TRUE)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_dims()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_s3_class(dims <- get_nc_dims(path), "data.table")
        expect_named(dims, c("id", "name", "length", "unlim"))

        con <- RNetCDF::open.nc(path)
        expect_equal(get_nc_dims(con), dims, ignore_attr = TRUE)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_axes()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_s3_class(axes <- get_nc_axes(path), "data.table")
        expect_named(axes, c("axis", "variable", "dimension"))

        con <- RNetCDF::open.nc(path)
        expect_equal(get_nc_axes(con), axes, ignore_attr = TRUE)
        RNetCDF::close.nc(con)
    }
})

test_that("get_nc_time()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_s3_class(time <- get_nc_time(path), "POSIXct")
        expect_equal(length(time), 366)

        expect_s3_class(time <- get_nc_time(path, range = TRUE), "POSIXct")
        expect_equal(length(time), 2L)

        con <- RNetCDF::open.nc(path)
        expect_equal(get_nc_time(con, range = TRUE), time, ignore_attr = TRUE)
        RNetCDF::close.nc(con)

        # can stop if invalid calendar found
        mockery::stub(get_nc_time, "get_nc_atts",
            data.table(
                variable = "time",
                attribute = "calendar",
                value = list("invalid")
            )
        )
        expect_error(get_nc_time(path), "Unsupported calendar")

        # can work with only date specification
        mockery::stub(get_nc_time, "get_nc_atts",
            data.table(
                variable = c("time", "time"),
                attribute = c("calendar", "units"),
                value = list("standard", "days since 1850-01-01")
            )
        )
        expect_s3_class(get_nc_time(path, range = TRUE), "POSIXct")

        # can warning if months resolution found
        mockery::stub(get_nc_time, "get_nc_atts",
            data.table(
                variable = c("time", "time"),
                attribute = c("calendar", "units"),
                value = list("standard", "months since 1850-01")
            )
        )
        expect_warning(get_nc_time(path, range = TRUE), "Month time resolution")

        # can stop if invlaid time unit string
        mockery::stub(get_nc_time, "get_nc_atts",
            data.table(
                variable = c("time", "time"),
                attribute = c("calendar", "units"),
                value = list("standard", "months 1850-01")
            )
        )
        expect_error(get_nc_time(path, range = TRUE), "Invalid time units")
    }
})

test_that("match_nc_time()", {
    skip_on_cran()

    cache <- get_cache()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_type(matched <- match_nc_time(path, 2060), "list")
        expect_equal(names(matched), c("datetime", "which"))
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 366L)
        expect_equal(length(matched$which), 1L)
        expect_equal(matched$which[[1L]], 1:366)

        expect_type(matched <- match_nc_time(path), "list")
        expect_equal(names(matched), c("datetime", "which"))
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 366L)
        expect_equal(length(matched$which), 1L)
        expect_equal(matched$which[[1L]], 1:366)

        expect_type(matched <- match_nc_time(path, 2059), "list")
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 0L)
        expect_equal(length(matched$which), 1L)
        expect_equal(length(matched$which[[1L]]), 0L)

        con <- RNetCDF::open.nc(path)
        expect_equal(match_nc_time(con, 2059), matched)
        RNetCDF::close.nc(con)

        expect_type(matched <- match_nc_time(path, 2059:2061), "list")
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
        expect_s3_class(db <- summary_database(cache), "data.table")
        expect_true(all(!is.na(load_cmip6_index()$file_path)))
        expect_true(all(!is.na(load_cmip6_index()$file_realsize)))
        expect_true(all(!is.na(load_cmip6_index()$file_mtime)))
        expect_true(all(!is.na(load_cmip6_index()$time_units)))
        expect_true(all(!is.na(load_cmip6_index()$time_calendar)))
        expect_s3_class(not <- attr(db, "not_matched"), "data.table")
        expect_equal(names(not),
            c("mip_era", "activity_id", "institution_id", "source_id", "experiment_id",
              "variant_label", "table_id", "grid_label", "nominal_resolution",
              "variable_id", "tracking_id", "standard_name", "units", "time_units",
              "time_calendar", "datetime_start", "datetime_end", "file_path",
              "file_realsize", "file_mtime"
            )
        )
        expect_s3_class(mis <- attr(db, "not_found"), "data.table")
        expect_equal(names(mis),
            c("file_id", "dataset_id", "mip_era", "activity_drs",
              "institution_id", "source_id", "experiment_id", "member_id",
              "table_id", "frequency", "grid_label", "version",
              "nominal_resolution", "variable_id", "variable_long_name",
              "variable_units", "datetime_start", "datetime_end", "file_size",
              "data_node", "file_url", "dataset_pid", "tracking_id"
            )
        )
        expect_equal(nrow(mis), 0L)
        expect_equal(as.integer(db$dl_percent), 100L)
        expect_equal(as.integer(db$dl_num), 3L)

        # 'append' can still work for initial summary
        idx <- load_cmip6_index()
        cols <- c("file_path", "file_realsize", "file_mtime", "time_units", "time_calendar")
        if (any(cols %in% names(idx))) set(idx, NULL, cols[cols %in% names(idx)], NULL)
        set_cmip6_index(idx)
        expect_s3_class(db <- summary_database(cache, append = TRUE), "data.table")
        load_cmip6_index()
        expect_equal(as.integer(db$dl_percent), 100L)
        expect_equal(as.integer(db$dl_num), 3L)

        # can append to existing database results
        idx <- load_cmip6_index()
        idx[1L, `:=`(file_path = "ori.nc", file_realsize = 1)]
        idx[2L, `:=`(file_path = NA, file_realsize = 1)]
        set_cmip6_index(idx)
        expect_warning(db <- summary_database(cache, append = TRUE))
        expect_s3_class(db, "data.table")
        idx <- load_cmip6_index()
        expect_equal(idx$file_path[1], "ori.nc")
        expect_equal(idx$file_realsize[1], 1.0)
        expect_true(!is.na(idx$file_path[2]))
        expect_equal(as.integer(db$dl_percent), 100L)
        expect_equal(as.integer(db$dl_num), 3L)
        attr(db, "not_found")

        # can give warnings if missing outputs found
        nc_2059 <- tempfile(fileext = ".nc")
        file.rename(paths["2059"], nc_2059)
        expect_warning(db <- summary_database(cache))
        idx <- load_cmip6_index()
        file.rename(nc_2059, paths["2059"])
        expect_true(is.na(idx$file_path[1]))
        expect_equal(as.integer(db$dl_percent), 66L)
        expect_equal(as.integer(db$dl_num), 2L)

        # can overwrite output metadata if the original file path does not
        # exists
        summary_database(cache)
        idx <- load_cmip6_index()
        idx[1L, `:=`(file_path = "ori.nc")]
        idx[2L, `:=`(file_path = NA)]
        set_cmip6_index(idx)
        suppressWarnings(expect_warning(db <- summary_database(cache, append = TRUE, miss = "overwrite")))

        # can work if no NetCDF files were found
        expect_s3_class(db <- summary_database(tempdir()), "data.table")
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
        expect_s3_class(db <- summary_database(tempdir(), append = TRUE), "data.table")
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
        expect_s3_class(db <- summary_database(tempdir(), update = TRUE), "data.table")
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
        suppressWarnings(expect_warning(summary_database(cache)))
        suppressWarnings(expect_warning(summary_database(cache, mult = "latest")))
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
        expect_s3_class(d <- get_nc_data(path, coord, years = 2060), "data.table")
        expect_equal(names(d),
            c("index", "activity_drs", "institution_id", "source_id", "experiment_id",
              "member_id", "table_id", "lon", "lat", "dist", "datetime", "variable",
              "description", "units", "value"
            )
        )
        expect_s3_class(d$value, "units")

        expect_s3_class(d <- get_nc_data(path, coord, years = 2000), "data.table")
        expect_equal(names(d),
            c("index", "activity_drs", "institution_id", "source_id", "experiment_id",
              "member_id", "table_id", "lon", "lat", "dist", "datetime", "variable",
              "description", "units", "value"
            )
        )
        expect_equal(nrow(d), 0L)
        expect_type(d$value, "double")

        con <- RNetCDF::open.nc(path)
        expect_equal(get_nc_data(con, coord, years = 2000), d, ignore_attr = TRUE)
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

        expect_s3_class(d <- extract_data(coord, years = 2060), "epw_cmip6_data")
        expect_equal(names(d), c("epw", "meta", "data"))
        expect_s3_class(d$epw, "Epw")
        expect_equal(d$meta, coord$meta)
        expect_s3_class(d$data, "data.table")
        expect_named(d$data,
            c("activity_drs", "institution_id", "source_id", "experiment_id",
              "member_id", "table_id", "lon", "lat", "dist", "datetime", "variable",
              "description", "units", "value"
            )
        )
        expect_identical(nrow(d$data), 2196L)

        expect_s3_class(d1 <- extract_data(coord, out_dir = cache), "epw_cmip6_data")
        expect_identical(d1$data, data.table())
        expect_true(file.exists(file.path(cache, "data.fst")))
        unlink(file.path(cache, "data.fst"))

        expect_s3_class(
            d2 <- extract_data(coord, out_dir = cache,
                by = c("source", "experiment", "variable"),
                years = 2060L
            ),
            "epw_cmip6_data"
        )
        expect_equal(d2$data, data.table())
        expect_true(file.exists(file.path(cache, "EC-Earth3.ssp585.tas.fst")))
        unlink(file.path(cache, "EC-Earth3.ssp585.tas.fst"))

        expect_s3_class(
            d3 <- extract_data(coord, out_dir = cache,
                by = c("source", "experiment", "variable"), keep = TRUE,
                years = 2060L
            ),
            "epw_cmip6_data"
        )
        expect_equal(d3$data, d$data)
        expect_true(file.exists(file.path(cache, "EC-Earth3.ssp585.tas.fst")))
    }
})
