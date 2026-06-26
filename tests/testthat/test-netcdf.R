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
