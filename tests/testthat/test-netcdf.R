local_nc_time_file <- function(time_vals, units, calendar = "standard") {
    path <- tempfile(fileext = ".nc")
    nc <- RNetCDF::create.nc(path)

    RNetCDF::dim.def.nc(nc, "time", length(time_vals))
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "test")
    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", units)
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", calendar)
    RNetCDF::var.put.nc(nc, "time", time_vals, count = length(time_vals))
    RNetCDF::close.nc(nc)

    path
}

local_summary_database_file <- function(
    time_vals = c(0, 364),
    time_units = "days since 2000-01-01 00:00:00",
    calendar = "standard",
    path = tempfile(fileext = ".nc"),
    tracking_id = "hdl:21.14100/test-file"
) {
    nc <- RNetCDF::create.nc(path)

    RNetCDF::dim.def.nc(nc, "time", length(time_vals))
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::var.def.nc(nc, "tas", "NC_DOUBLE", "time")

    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "mip_era", "NC_CHAR", "CMIP6")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "activity_id", "NC_CHAR", "ScenarioMIP")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "institution_id", "NC_CHAR", "ideas-lab")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "source_id", "NC_CHAR", "TestModel")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "experiment_id", "NC_CHAR", "ssp585")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "variant_label", "NC_CHAR", "r1i1p1f1")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "table_id", "NC_CHAR", "day")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "grid_label", "NC_CHAR", "gr")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "nominal_resolution", "NC_CHAR", "100 km")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "variable_id", "NC_CHAR", "tas")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "tracking_id", "NC_CHAR", tracking_id)
    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", time_units)
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", calendar)
    RNetCDF::att.put.nc(nc, "tas", "standard_name", "NC_CHAR", "air_temperature")
    RNetCDF::att.put.nc(nc, "tas", "units", "NC_CHAR", "K")

    RNetCDF::var.put.nc(nc, "time", time_vals, count = length(time_vals))
    RNetCDF::var.put.nc(nc, "tas", seq_along(time_vals), count = length(time_vals))
    RNetCDF::close.nc(nc)

    path
}

local_summary_database_index <- function(paths) {
    data.table::rbindlist(lapply(seq_along(paths), function(i) {
        path <- paths[[i]]
        meta <- get_nc_meta(path)
        time <- get_nc_time(path, range = TRUE)

        data.table::data.table(
            file_id = sprintf("case|tas|%02i", i),
            dataset_id = sprintf("dataset-%02i", i),
            mip_era = meta$mip_era,
            activity_drs = meta$activity_id,
            institution_id = meta$institution_id,
            source_id = meta$source_id,
            experiment_id = meta$experiment_id,
            member_id = meta$variant_label,
            table_id = meta$table_id,
            frequency = meta$table_id,
            grid_label = meta$grid_label,
            version = "v20240101",
            nominal_resolution = meta$nominal_resolution,
            variable_id = meta$variable_id,
            variable_long_name = meta$standard_name,
            variable_units = meta$units,
            datetime_start = time[[1L]],
            datetime_end = time[[2L]],
            file_size = unname(file.size(path)),
            data_node = "example.test",
            file_url = sprintf("https://example.test/%s", basename(path)),
            dataset_pid = sprintf("hdl:21.14100/test-dataset-%02i", i),
            tracking_id = meta$tracking_id
        )
    }))
}

local_summary_database_fixture <- function(years = 2059:2061, include_unmatched = FALSE) {
    dir <- tempfile("summary-database-")
    dir.create(dir)

    paths <- file.path(dir, sprintf("tas_%s.nc", years))
    for (i in seq_along(paths)) {
        local_summary_database_file(
            time_units = sprintf("days since %s-01-01 00:00:00", years[[i]]),
            path = paths[[i]],
            tracking_id = sprintf("hdl:21.14100/test-file-%s", years[[i]])
        )
    }

    extra_path <- NULL
    if (include_unmatched) {
        extra_path <- file.path(dir, "tas_extra.nc")
        local_summary_database_file(
            time_units = "days since 2100-01-01 00:00:00",
            path = extra_path,
            tracking_id = "hdl:21.14100/test-file-extra"
        )
    }

    list(
        dir = dir,
        paths = paths,
        index = local_summary_database_index(paths),
        extra_path = extra_path
    )
}

skip_summary_database_on_covr <- function() {
    # covr cannot reliably merge coverage traces emitted by mirai worker
    # processes used by summary_database() NetCDF scans.
    skip_on_covr()
}

test_that("parse_cf_time() returns UTC POSIXct with CF metadata", {
    time <- parse_cf_time(c(0, 1.5), "days since 2000-01-01 00:00:00", "standard")

    expect_s3_class(time, "POSIXct")
    expect_equal(as.numeric(time), c(946684800, 946814400))
    expect_identical(attr(time, "tzone"), "UTC")
    expect_identical(attr(time, "cf_units"), "days since 2000-01-01 00:00:00")
    expect_identical(attr(time, "cf_calendar"), "standard")
})

test_that("parse_cf_time() returns POSIXct for common CF calendars", {
    calendars <- c(
        "standard",
        "gregorian",
        "proleptic_gregorian",
        "noleap",
        "365_day",
        "360_day"
    )

    for (calendar in calendars) {
        time <- parse_cf_time(58:62, "days since 2000-01-01 00:00:00", calendar)

        expect_true(inherits(time, "POSIXct"), info = calendar)
        expect_identical(attr(time, "tzone"), "UTC", info = calendar)
        expect_identical(attr(time, "cf_units"), "days since 2000-01-01 00:00:00", info = calendar)
        expect_identical(attr(time, "cf_calendar"), calendar, info = calendar)
        expect_equal(length(time), 5L, info = calendar)
        expect_equal(unname(diff(as.numeric(time))), rep(86400, 4L), info = calendar)
    }
})

test_that("get_nc_time() and EsgDataset$get_time_axis() share CF time parsing", {
    path <- local_nc_time_file(
        time_vals = c(0, 1.5),
        units = "days since 2000-01-01 00:00:00",
        calendar = "standard"
    )
    on.exit(unlink(path), add = TRUE)

    expected <- as.POSIXct(c("2000-01-01 00:00:00", "2000-01-02 12:00:00"), tz = "UTC")

    time <- get_nc_time(path)
    expect_s3_class(time, "POSIXct")
    expect_equal(as.numeric(time), as.numeric(expected))
    expect_identical(attr(time, "cf_units"), "days since 2000-01-01 00:00:00")
    expect_identical(attr(time, "cf_calendar"), "standard")

    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    time_info <- ds$get_time_axis()
    expect_s3_class(time_info$values, "POSIXct")
    expect_equal(time_info$length, 2L)
    expect_identical(time_info$units, "days since 2000-01-01 00:00:00")
    expect_identical(time_info$calendar, "standard")
    expect_equal(as.numeric(time_info$values), as.numeric(expected))
    expect_identical(attr(time_info$values, "cf_units"), time_info$units)
    expect_identical(attr(time_info$values, "cf_calendar"), time_info$calendar)
})

test_that("get_nc_time() and EsgDataset$get_time_axis() return POSIXct for 360_day", {
    path <- local_nc_time_file(
        time_vals = 58:62,
        units = "days since 2000-01-01 00:00:00",
        calendar = "360_day"
    )
    on.exit(unlink(path), add = TRUE)

    time <- get_nc_time(path)
    expect_s3_class(time, "POSIXct")
    expect_identical(attr(time, "cf_units"), "days since 2000-01-01 00:00:00")
    expect_identical(attr(time, "cf_calendar"), "360_day")
    expect_equal(unname(diff(as.numeric(time))), rep(86400, 4L))

    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    time_info <- ds$get_time_axis()
    expect_s3_class(time_info$values, "POSIXct")
    expect_identical(time_info$units, "days since 2000-01-01 00:00:00")
    expect_identical(time_info$calendar, "360_day")
    expect_equal(as.numeric(time_info$values), as.numeric(time))
    expect_identical(attr(time_info$values, "cf_units"), time_info$units)
    expect_identical(attr(time_info$values, "cf_calendar"), time_info$calendar)
})

test_that("get_nc_meta()", {
    skip_on_cran()

    cache <- test_data_dir()
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

    cache <- test_data_dir()
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

    cache <- test_data_dir()
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

    cache <- test_data_dir()
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

    cache <- test_data_dir()
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

    cache <- test_data_dir()
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
        testthat::with_mocked_bindings(
            expect_error(get_nc_time(path), "Invalid calendar specification"),
            get_nc_atts = function(...) {
                data.table(
                    variable = c("time", "time"),
                    attribute = c("calendar", "units"),
                    value = list("invalid", "days since 1850-01-01")
                )
            },
            .package = "epwshiftr"
        )

        # can work with only date specification
        testthat::with_mocked_bindings(
            expect_s3_class(get_nc_time(path, range = TRUE), "POSIXct"),
            get_nc_atts = function(...) {
                data.table(
                    variable = c("time", "time"),
                    attribute = c("calendar", "units"),
                    value = list("standard", "days since 1850-01-01")
                )
            },
            .package = "epwshiftr"
        )

        # can parse months resolution with the internal CF time parser
        testthat::with_mocked_bindings(
            expect_s3_class(get_nc_time(path, range = TRUE), "POSIXct"),
            get_nc_atts = function(...) {
                data.table(
                    variable = c("time", "time"),
                    attribute = c("calendar", "units"),
                    value = list("standard", "months since 1850-01")
                )
            },
            .package = "epwshiftr"
        )

        # can stop if invlaid time unit string
        testthat::with_mocked_bindings(
            expect_error(get_nc_time(path, range = TRUE), "CF-compliant time coordinate"),
            get_nc_atts = function(...) {
                data.table(
                    variable = c("time", "time"),
                    attribute = c("calendar", "units"),
                    value = list("standard", "months 1850-01")
                )
            },
            .package = "epwshiftr"
        )
    }
})

test_that("summary_database_scan_file()", {
    path <- local_summary_database_file()
    on.exit(unlink(path), add = TRUE)

    expected <- data.table::as.data.table(c(
        get_nc_meta(path),
        setNames(as.list(get_nc_time(path, range = TRUE)), c("datetime_start", "datetime_end")),
        list(
            file_path = path,
            file_realsize = file.size(path),
            file_mtime = file.mtime(path)
        )
    ))

    expect_equal(summary_database_scan_file(path), expected, ignore_attr = TRUE)
})

test_that("match_nc_time()", {
    skip_on_cran()

    cache <- test_data_dir()
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

test_that("summary_database() uses single-file scan helper", {
    skip_summary_database_on_covr()

    path <- local_summary_database_file()
    on.exit(unlink(path), add = TRUE)

    dir <- tempfile("summary-database-single-")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)

    single <- file.path(dir, basename(path))
    file.copy(path, single)

    meta <- get_nc_meta(path)
    time <- get_nc_time(path, range = TRUE)
    idx <- data.table::data.table(
        file_id = "case|tas",
        activity_drs = meta$activity_id,
        experiment_id = meta$experiment_id,
        member_id = meta$variant_label,
        table_id = meta$table_id,
        variable_id = meta$variable_id,
        source_id = meta$source_id,
        nominal_resolution = meta$nominal_resolution,
        datetime_start = time[[1L]],
        datetime_end = time[[2L]],
        file_size = unname(file.size(path)),
        tracking_id = meta$tracking_id
    )

    stubbed_scan <- data.table::copy(summary_database_scan_file(single))
    stubbed_scan[, tracking_id := "stubbed-tracking-id"]

    ns <- asNamespace("epwshiftr")
    old_load <- get("load_cmip6_index", envir = ns)
    old_scan <- get("summary_database_scan_file", envir = ns)
    on.exit({
        unlockBinding("load_cmip6_index", ns)
        assign("load_cmip6_index", old_load, envir = ns)
        lockBinding("load_cmip6_index", ns)
        unlockBinding("summary_database_scan_file", ns)
        assign("summary_database_scan_file", old_scan, envir = ns)
        lockBinding("summary_database_scan_file", ns)
    }, add = TRUE)

    unlockBinding("load_cmip6_index", ns)
    assign("load_cmip6_index", function(force = FALSE) data.table::copy(idx), envir = ns)
    lockBinding("load_cmip6_index", ns)

    unlockBinding("summary_database_scan_file", ns)
    assign("summary_database_scan_file", function(file) data.table::copy(stubbed_scan), envir = ns)
    lockBinding("summary_database_scan_file", ns)

    db <- summary_database(dir, warning = FALSE)

    expect_equal(db$dl_num, 0L)
    expect_equal(attr(db, "not_matched")$tracking_id, "stubbed-tracking-id")
})

test_that("summary_database() matches temporary NetCDF files self-contained", {
    skip_summary_database_on_covr()

    fixture <- local_summary_database_fixture(include_unmatched = TRUE)
    on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

    data_dir <- tempfile("summary-database-data-")
    dir.create(data_dir)
    on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

    withr::local_options(list(epwshiftr.dir_store = data_dir, epwshiftr.verbose = FALSE))
    set_cmip6_index(fixture$index)

    expect_s3_class(db <- summary_database(fixture$dir), "data.table")

    idx <- load_cmip6_index()
    expect_true(all(!is.na(idx$file_path)))
    expect_true(all(!is.na(idx$file_realsize)))
    expect_true(all(!is.na(idx$file_mtime)))
    expect_true(all(!is.na(idx$time_units)))
    expect_true(all(!is.na(idx$time_calendar)))
    expect_equal(sort(normalizePath(idx$file_path)), sort(normalizePath(fixture$paths)))
    expect_equal(as.integer(db$dl_percent), 100L)
    expect_equal(as.integer(db$dl_num), 3L)
    expect_equal(nrow(attr(db, "not_found")), 0L)
    expect_equal(attr(db, "not_matched")$tracking_id, "hdl:21.14100/test-file-extra")
})

test_that("summary_database() append mode works with temporary NetCDF files", {
    skip_summary_database_on_covr()

    fixture <- local_summary_database_fixture()
    on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

    data_dir <- tempfile("summary-database-data-")
    dir.create(data_dir)
    on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

    withr::local_options(list(epwshiftr.dir_store = data_dir, epwshiftr.verbose = FALSE))
    set_cmip6_index(fixture$index)

    expect_s3_class(db <- summary_database(fixture$dir, append = TRUE), "data.table")

    idx <- load_cmip6_index()
    expect_true(all(c("file_path", "file_realsize", "file_mtime", "time_units", "time_calendar") %in% names(idx)))
    expect_equal(as.integer(db$dl_percent), 100L)
    expect_equal(as.integer(db$dl_num), 3L)
})

test_that("summary_database() handles lost and missing temporary NetCDF files", {
    skip_summary_database_on_covr()

    fixture <- local_summary_database_fixture()
    on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

    data_dir <- tempfile("summary-database-data-")
    dir.create(data_dir)
    on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

    withr::local_options(list(epwshiftr.dir_store = data_dir, epwshiftr.verbose = FALSE))
    set_cmip6_index(fixture$index)
    summary_database(fixture$dir)

    idx <- load_cmip6_index()
    idx[1L, `:=`(file_path = "ori.nc", file_realsize = 1)]
    idx[2L, `:=`(file_path = NA_character_, file_realsize = 1)]
    set_cmip6_index(idx)

    expect_warning(db_keep <- summary_database(fixture$dir, append = TRUE))
    idx_keep <- load_cmip6_index()
    expect_equal(idx_keep$file_path[1], "ori.nc")
    expect_equal(idx_keep$file_realsize[1], 1)
    expect_true(!is.na(idx_keep$file_path[2]))
    expect_equal(as.integer(db_keep$dl_num), 3L)
    expect_equal(nrow(attr(db_keep, "not_found")), 1L)

    idx_keep[1L, file_path := "ori.nc"]
    idx_keep[2L, file_path := NA_character_]
    set_cmip6_index(idx_keep)

    suppressWarnings(expect_warning(db_overwrite <- summary_database(fixture$dir, append = TRUE, miss = "overwrite")))
    idx_overwrite <- load_cmip6_index()
    expect_true(is.na(idx_overwrite$file_path[1]))
    expect_true(!is.na(idx_overwrite$file_path[2]))
    expect_equal(as.integer(db_overwrite$dl_num), 2L)

    unlink(fixture$paths[1])
    expect_warning(db_missing <- summary_database(fixture$dir))
    idx_missing <- load_cmip6_index()
    expect_true(is.na(idx_missing$file_path[1]))
    expect_equal(as.integer(db_missing$dl_percent), 66L)
    expect_equal(as.integer(db_missing$dl_num), 2L)
    expect_equal(nrow(attr(db_missing, "not_found")), 1L)
})

test_that("summary_database() handles empty directories and update self-contained", {
    fixture <- local_summary_database_fixture()
    on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

    data_dir <- tempfile("summary-database-data-")
    empty_dir <- tempfile("summary-database-empty-")
    dir.create(data_dir)
    dir.create(empty_dir)
    on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
    on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

    withr::local_options(list(epwshiftr.dir_store = data_dir, epwshiftr.verbose = FALSE))
    set_cmip6_index(fixture$index)

    expect_s3_class(db <- summary_database(empty_dir), "data.table")
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
    expect_true(all(is.na(idx$file_path)))
    expect_true(all(is.na(idx$file_realsize)))
    expect_true(all(is.na(idx$file_mtime)))
    expect_true(all(is.na(idx$time_units)))
    expect_true(all(is.na(idx$time_calendar)))

    set_cmip6_index(fixture$index)
    suppressMessages(trace(summary_database, tracer = quote(verbose <- function(...) NULL), print = FALSE))
    on.exit(suppressMessages(untrace(summary_database)), add = TRUE)

    expect_s3_class(db <- summary_database(empty_dir, update = TRUE), "data.table")
    index_path <- store_cmip6_index_active_path()
    expect_true(file.exists(index_path))
    expect_equal(names(data.table::fread(index_path)),
        c("file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
          "source_id", "experiment_id", "member_id", "table_id", "frequency",
          "grid_label", "version", "nominal_resolution", "variable_id",
          "variable_long_name", "variable_units", "datetime_start", "datetime_end",
          "file_size", "data_node", "file_url", "dataset_pid", "tracking_id",
          "file_path", "file_realsize", "file_mtime", "time_units", "time_calendar"
        )
    )
})

test_that("summary_database() handles duplicate temporary NetCDF matches", {
    skip_summary_database_on_covr()

    fixture <- local_summary_database_fixture()
    on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

    data_dir <- tempfile("summary-database-data-")
    dir.create(data_dir)
    on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

    withr::local_options(list(epwshiftr.dir_store = data_dir, epwshiftr.verbose = FALSE))
    dup <- file.path(fixture$dir, "tas_2059_dup.nc")
    file.copy(fixture$paths[1], dup)
    Sys.setFileTime(dup, file.info(fixture$paths[1])$mtime + 60)

    set_cmip6_index(fixture$index)
    suppressWarnings(expect_warning(db_skip <- summary_database(fixture$dir)))
    idx_skip <- load_cmip6_index()
    expect_true(is.na(idx_skip$file_path[1]))
    expect_equal(as.integer(db_skip$dl_num), 2L)

    set_cmip6_index(fixture$index)
    suppressWarnings(expect_warning(db_latest <- summary_database(fixture$dir, mult = "latest")))
    idx_latest <- load_cmip6_index()
    expect_identical(normalizePath(idx_latest$file_path[1]), normalizePath(dup))
    expect_equal(as.integer(db_latest$dl_num), 3L)
})

test_that("get_nc_data()", {
    skip_on_cran()

    cache <- test_data_dir()
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

    cache <- test_data_dir()
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)
    epw <- file.path(cache, "SGP_Singapore.486980_IWEC.epw")

    if (file.exists(path) && file.exists(epw)) {
        options(epwshiftr.dir_store = tempdir())
        get_cache_nc()
        summary_database(cache)
        idx <- load_cmip6_index()
        target_idx <- idx[!is.na(file_path) & basename(file_path) == file]
        expect_identical(nrow(target_idx), 1L)
        set_cmip6_index(target_idx)
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
        parquet <- file.path(cache, "data.parquet")
        expect_true(file.exists(parquet))
        expect_equal(nrow(read_test_parquet(parquet)), nrow(d$data))
        unlink(parquet)

        expect_s3_class(
            d2 <- extract_data(coord, out_dir = cache,
                by = c("source", "experiment", "variable"),
                years = 2060L
            ),
            "epw_cmip6_data"
        )
        expect_equal(d2$data, data.table())
        parquet <- file.path(cache, "EC-Earth3.ssp585.tas.parquet")
        expect_true(file.exists(parquet))
        expect_equal(nrow(read_test_parquet(parquet)), 2196L)
        unlink(parquet)

        expect_s3_class(
            d3 <- extract_data(coord, out_dir = cache,
                by = c("source", "experiment", "variable"), keep = TRUE,
                years = 2060L
            ),
            "epw_cmip6_data"
        )
        expect_equal(d3$data, d$data)
        expect_true(file.exists(file.path(cache, "EC-Earth3.ssp585.tas.parquet")))
    }
})
