# Compare real native reads, worker payloads and subsequent offline cache reads.

test_that("native worker payloads preserve actual CF bounds and site results", {
    withr::local_options(epwshiftr.mirai_workers = 2L, epwshiftr.cache = FALSE)
    paths <- c(tempfile(fileext = ".nc"), tempfile(fileext = ".nc"))
    on.exit(unlink(paths), add = TRUE)
    jobs <- lapply(seq_along(paths), function(index) {
        path <- paths[[index]]
        write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
        list(
            indices = index,
            file = data.table::data.table(
                file_key = basename(path),
                filename = basename(path),
                url_opendap = path,
                variable_id = "tas",
                experiment_id = "ssp245"
            ),
            plans = data.table::data.table(
                variable_id = "tas",
                lon = c(103.98, -106),
                lat = c(1.37, 41),
                method = "nearest",
                time_start = as.POSIXct("2060-01-02", tz = "UTC"),
                time_stop = as.POSIXct("2060-01-03 23:59:59", tz = "UTC")
            )
        )
    })
    # Read the tiny native files directly, independently of the extraction and
    # worker implementations. This catches errors shared by serial and parallel
    # paths; the fixture's two requested sites resolve to cells (2, 1) and (4, 3).
    native <- lapply(paths, function(path) {
        nc <- RNetCDF::open.nc(path)
        on.exit(RNetCDF::close.nc(nc))
        list(
            values = RNetCDF::var.get.nc(nc, "tas"),
            time = RNetCDF::var.get.nc(nc, "time"),
            bounds = RNetCDF::var.get.nc(nc, "time_bnds"),
            units = RNetCDF::att.get.nc(nc, "tas", "units")
        )
    })
    expected <- lapply(jobs, store__read_task)
    actual <- vector("list", 2L)
    source__apply(jobs, store__read_task, function(job, value) {
        actual[[job$indices]] <<- value
    })
    for (index in seq_along(jobs)) {
        for (site in 1:2) {
            expect_equal(
                actual[[index]]$results[[site]]$payload,
                expected[[index]]$results[[site]]$payload,
                tolerance = 0
            )
            expect_identical(
                unique(
                    actual[[index]]$results[[site]]$payload$data$cf_calendar
                ),
                "360_day"
            )
            payload <- actual[[index]]$results[[site]]$payload
            source <- native[[index]]
            lon_index <- c(2L, 4L)[[site]]
            lat_index <- c(1L, 3L)[[site]]
            expect_equal(
                payload$data$value,
                source$values[lon_index, lat_index, 2:3],
                tolerance = 0
            )
            expect_identical(unique(payload$data$units), source$units)
            expect_identical(payload$data$cf_day, 2:3)
            expect_equal(payload$grid_sources$grid_lon, c(104, 254)[[site]])
            expect_equal(payload$grid_sources$grid_lat, c(1, 41)[[site]])
            origin <- as.POSIXct("2060-01-01", tz = "UTC")
            expect_equal(
                payload$data$time,
                origin + as.numeric(source$time[2:3]) * 86400
            )
            expect_equal(
                payload$data$time_bound_start,
                origin + source$bounds[1L, 2:3] * 86400
            )
            expect_equal(
                payload$data$time_bound_end,
                origin + source$bounds[2L, 2:3] * 86400
            )
        }
    }
    # Cache references cross the process boundary; a later offline read must
    # succeed even after the native sources have been removed.
    cache <- tempfile("source-cache-")
    withr::local_options(epwshiftr.cache = TRUE, epwshiftr.dir_cache = cache)
    on.exit(unlink(cache, recursive = TRUE), add = TRUE)
    source__apply(jobs, store__read_task, function(job, value) {
        for (site in seq_along(value$results)) {
            resolved <- value$results[[site]]
            expect_null(resolved$payload)
            expect_equal(
                store__extract_cache_read(resolved$cache_path),
                expected[[job$indices]]$results[[site]]$payload,
                tolerance = 0
            )
        }
    })
    unlink(paths)
    withr::local_options(epwshiftr.cache = "offline")
    source__apply(jobs, store__read_task, function(job, value) {
        expect_true(all(vapply(
            value$results,
            function(x) isTRUE(x$cache_reused),
            logical(1L)
        )))
    })
})

# vim: fdm=marker :
