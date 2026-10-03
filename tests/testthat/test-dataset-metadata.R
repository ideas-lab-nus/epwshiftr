# Check real native reads, including bounds-first and time-first CF layouts.
test_that("selected CF bounds preserve native indices and dimension order", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    withr::defer(unlink(path))
    nc <- RNetCDF::open.nc(path, write = TRUE)
    raw <- RNetCDF::var.get.nc(nc, "time_bnds", collapse = FALSE)
    RNetCDF::var.def.nc(nc, "alternate_bounds", "NC_DOUBLE", c("time", "bnds"))
    RNetCDF::var.put.nc(nc, "alternate_bounds", t(raw), count = rev(dim(raw)))
    RNetCDF::close.nc(nc)
    ds <- EsgDataset$new(path)
    ds$open()
    withr::defer(ds$close())
    full <- ds$get_time_axis()
    selected <- c(1L, 2L, 30L, 60L, 360L)
    subset <- dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        selected
    )
    expect_equal(subset$start[seq_along(selected)], full$bounds$start[selected])
    expect_equal(subset$end[seq_along(selected)], full$bounds$end[selected])
    expect_equal(
        data.table::as.data.table(subset$start_coordinates),
        data.table::as.data.table(full$bounds$start_coordinates[selected, ])
    )
    expect_null(dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        integer()
    ))
    expect_error(dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        c(1L, 1L)
    ))
    expect_error(dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        NA_integer_
    ))
    expect_error(dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        361L
    ))
    ds$close()
    nc <- RNetCDF::open.nc(path, write = TRUE)
    RNetCDF::att.put.nc(nc, "time", "bounds", "NC_CHAR", "alternate_bounds")
    RNetCDF::close.nc(nc)
    ds$open()
    reordered <- dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        selected
    )
    expect_equal(reordered$start, subset$start)
    expect_equal(reordered$end, subset$end)
})

# Instrument the NetCDF wrapper to ensure the optimized path requests only
# selected bounds; the public full-axis method still reads all intervals.
test_that("multi-site reads fetch only selected CF bounds", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "365_day")
    withr::defer(unlink(path))
    ds <- EsgDataset$new(path)
    ds$open()
    withr::defer(ds$close())
    original <- RNetCDF::var.get.nc
    bounds_counts <- list()
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.get.nc = function(ncfile, variable, ...) {
            arguments <- list(...)
            if (identical(variable, "time_bnds")) {
                bounds_counts[[length(bounds_counts) + 1L]] <<- arguments$count
            }
            original(ncfile, variable, ...)
        }
    )
    sites <- data.table::data.table(
        site_id = c("first", "last"),
        lon = c(104, 254),
        lat = c(1, 41),
        method = "nearest",
        time_start = as.POSIXct(c("2060-01-01", "2060-12-31"), tz = "UTC"),
        time_stop = as.POSIXct(
            c("2060-01-01 23:59:59", "2060-12-31 23:59:59"),
            tz = "UTC"
        )
    )
    result <- dataset__read_regions(ds, "tas", sites)
    expect_equal(nrow(result), 2L)
    expect_equal(bounds_counts, list(c(2L, 1L), c(2L, 1L)))
    full <- ds$get_time_axis()
    expect_equal(length(full$bounds$start), 365L)
    expect_equal(tail(bounds_counts, 1L), list(c(2L, 365L)))
    expect_equal(result$time_bound_start, full$bounds$start[c(1L, 365L)])
    empty <- dataset__read_regions(
        ds,
        "tas",
        sites[, c("site_id", "lon", "lat", "method"), with = FALSE],
        time = c("2059-01-01", "2059-01-31")
    )
    expect_equal(nrow(empty), 0L)
    expect_length(bounds_counts, 3L)
})

# Coordinate reuse belongs to the opened dataset object. A separate object
# reads its own native coordinates without publishing a cross-batch cache.
test_that("native coordinates are reused only within a dataset object", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    withr::defer(unlink(path))
    root <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_cache = root))
    original <- RNetCDF::var.get.nc
    reads <- character()
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.get.nc = function(ncfile, variable, ...) {
            reads <<- c(reads, variable)
            original(ncfile, variable, ...)
        }
    )
    ds <- EsgDataset$new(path)
    ds$open()
    withr::defer(ds$close())
    axis <- dataset__time_axis(ds)
    grid <- ds$get_spatial_grid()
    expect_equal(reads, c("time", "lat", "lon"))
    expect_null(axis$bounds)
    reads <- character()
    expect_identical(dataset__time_axis(ds), axis)
    expect_identical(ds$get_spatial_grid(), grid)
    expect_length(reads, 0L)

    other <- EsgDataset$new(path)
    other$open()
    withr::defer(other$close())
    expect_identical(dataset__time_axis(other), axis)
    expect_identical(other$get_spatial_grid(), grid)
    expect_equal(reads, c("time", "lat", "lon"))
    expect_false(dir.exists(file.path(root, "source-metadata")))
})

# Bound metadata is shared across the acquisition's value windows, so a long
# period does not add one metadata round trip per window.
test_that("batch prefetch shares selected bounds across value windows", {
    cache <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_cache = cache))
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(
        path,
        2060L,
        calendar = "365_day",
        n_years = 24L
    )
    withr::defer(unlink(path))
    acquisition <- data.table::data.table(
        acquisition_id = "twenty-four-years",
        physical_file_id = "twenty-four-years",
        filename = basename(path),
        variable_id = "tas",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        grid_label = "gr",
        frequency = "day",
        table_id = "day",
        version = "v1",
        tracking_id = "fixture",
        checksum = store_hash_file(path, "sha256"),
        checksum_type = "sha256",
        url_opendap = path,
        url_download = path,
        time_start = as.POSIXct("2060-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2083-12-31 23:59:59", tz = "UTC")
    )
    consumer <- data.table::data.table(
        acquisition_id = "twenty-four-years",
        demand_id = "site",
        child_key = "site",
        site_id = "site",
        role = "future",
        variable_id = "tas",
        lon = 104,
        lat = 1,
        spatial_method = "nearest",
        time_start = acquisition$time_start,
        time_stop = acquisition$time_stop,
        requested_start = acquisition$time_start,
        requested_stop = acquisition$time_stop
    )
    original <- RNetCDF::var.get.nc
    bounds_counts <- list()
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.get.nc = function(ncfile, variable, ...) {
            if (identical(variable, "time_bnds")) {
                bounds_counts[[length(bounds_counts) + 1L]] <<- list(...)$count
            }
            original(ncfile, variable, ...)
        }
    )
    expect_equal(
        shift_batch_window__prefetch_acquisition(
            withr::local_tempdir(),
            acquisition,
            consumer
        ),
        2L
    )
    expect_equal(bounds_counts, list(c(2L, 4096L), c(2L, 4096L), c(2L, 568L)))

    # Retuning the common limit changes both acquisition windows and bounds
    # slices; fresh receipts and extraction caches force the new schedule.
    local_mocked_bindings(DATASET_REQUEST_MAX_VALUES = 4096L)
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())
    bounds_counts <- list()
    expect_equal(
        shift_batch_window__prefetch_acquisition(
            withr::local_tempdir(),
            acquisition,
            consumer
        ),
        3L
    )
    expect_equal(
        bounds_counts,
        c(rep(list(c(2L, 2048L)), 4L), list(c(2L, 568L)))
    )
    expect_false(dir.exists(file.path(cache, "source-metadata")))
})

# Ordinary point reads must retain real, irregular intervals while avoiding
# bounds outside the requested window. Repeated cities reuse the same subset.
test_that("point reads reuse selected actual bounds without loading the full axis", {
    for (calendar in c("365_day", "360_day", "proleptic_gregorian")) {
        path <- tempfile(fileext = ".nc")
        write_local_cmip6_netcdf_fixture(path, 2060L, calendar = calendar)
        withr::defer(unlink(path))
        nc <- RNetCDF::open.nc(path, write = TRUE)
        raw <- RNetCDF::var.get.nc(nc, "time_bnds", collapse = FALSE)
        # Deliberately nonuniform intervals rule out reconstruction from spacing.
        raw[1L, 2:3] <- raw[1L, 2:3] + c(0.1, 0.2)
        RNetCDF::var.put.nc(nc, "time_bnds", raw)
        RNetCDF::close.nc(nc)
        ds <- EsgDataset$new(path)
        ds$open()
        withr::defer(ds$close())
        original <- RNetCDF::var.get.nc
        counts <- list()
        testthat::local_mocked_bindings(
            .package = "RNetCDF",
            var.get.nc = function(ncfile, variable, ...) {
                if (identical(variable, "time_bnds")) {
                    counts[[length(counts) + 1L]] <<- list(...)$count
                }
                original(ncfile, variable, ...)
            }
        )
        window <- c("2060-01-02", "2060-01-03 23:59:59")
        actual <- ds$read_region("tas", lon = 104, lat = 1, time = window)
        expect_equal(counts, list(c(2L, 2L)))
        expect_equal(nrow(actual), 2L)
        again <- ds$read_region("tas", lon = 254, lat = 41, time = window)
        expect_equal(again$time_bound_start, actual$time_bound_start)
        expect_length(counts, 1L)
        ds$read_region(
            "tas",
            lon = 104,
            lat = 1,
            time = c("2060-01-03", "2060-01-03 23:59:59")
        )
        expect_length(counts, 1L)
        empty <- ds$read_region(
            "tas",
            lon = 104,
            lat = 1,
            time = c("2059-01-01", "2059-01-02")
        )
        expect_equal(nrow(empty), 0L)
        expect_length(counts, 1L)
        full <- ds$get_time_axis()
        expect_equal(tail(counts, 1L), list(c(2L, ncol(raw))))
        expected <- ds$read_region("tas", lon = 104, lat = 1, time = window)
        expect_identical(actual, expected)
        expect_equal(actual$time_bound_start, full$bounds$start[2:3])
        expect_length(counts, 2L)
        ds$close()
        nc <- RNetCDF::open.nc(path, write = TRUE)
        raw[1L, 2L] <- raw[1L, 2L] + 0.1
        RNetCDF::var.put.nc(nc, "time_bnds", raw)
        RNetCDF::close.nc(nc)
        ds$open()
        reopened <- ds$read_region("tas", lon = 104, lat = 1, time = window)
        expect_false(identical(
            reopened$time_bound_start,
            actual$time_bound_start
        ))
        expect_equal(tail(counts, 1L), list(c(2L, 2L)))
        expect_length(counts, 3L)
        ds$close()
        # Restore between calendars; do not stack wrappers through loop bindings.
        testthat::local_mocked_bindings(
            .package = "RNetCDF",
            var.get.nc = original
        )
    }
})

# Independent Dataset objects must not reset each other's compute profiles.
test_that("concurrent dataset tasks in one process retain separate backends", {
    skip_if_not_installed("mirai")
    datasets <- lapply(seq_len(2L), function(index) EsgDataset$new("unused.nc"))
    on.exit(lapply(datasets, function(dataset) dataset$close()), add = TRUE)
    tasks <- lapply(seq_along(datasets), function(index) {
        private <- datasets[[index]]$.__enclos_env__$private
        # No remote I/O is needed to exercise the task ownership boundary.
        private$urls <- character()
        private$start_async_operation(
            "return task identity",
            function(urls, nc_handles, value) {
                Sys.sleep(0.1)
                value
            },
            handler_args = list(value = index),
            timeout = 10
        )
    })
    expect_false(identical(
        tasks[[1L]]$compute_profile,
        tasks[[2L]]$compute_profile
    ))
    expect_identical(tasks[[2L]]$collect(), 2L)
    expect_identical(tasks[[1L]]$collect(), 1L)
    expect_true(all(vapply(
        tasks,
        function(task) task$backend_released,
        logical(1L)
    )))
})
