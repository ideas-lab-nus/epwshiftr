# Keep source-pool tests independent of machine defaults and global daemons.
test_that("source workers overlap tasks and collect in the owning process", {
    withr::local_options(epwshiftr.mirai_workers = 2L)
    owner <- Sys.getpid()
    result <- vector("list", 3L)
    source__apply(
        as.list(1:3),
        function(job) {
            start <- as.numeric(Sys.time())
            Sys.sleep(0.15)
            list(
                pid = Sys.getpid(),
                start = start,
                stop = as.numeric(Sys.time())
            )
        },
        function(job, value) {
            expect_identical(Sys.getpid(), owner)
            result[[job]] <<- value
        }
    )
    expect_length(unique(vapply(result, `[[`, integer(1L), "pid")), 2L)
    expect_true(all(vapply(result, `[[`, integer(1L), "pid") != owner))
    expect_lt(
        max(result[[1L]]$start, result[[2L]]$start),
        min(result[[1L]]$stop, result[[2L]]$stop)
    )
    withr::local_options(epwshiftr.mirai_workers = 0L)
    expect_error(
        source__apply(list(1), identity, function(...) NULL),
        "positive|>=|greater"
    )
})

test_that("fatal source failures drain active work without dispatching more", {
    withr::local_options(epwshiftr.mirai_workers = 2L)
    paths <- file.path(
        tempdir(),
        paste0("source-pool-", Sys.getpid(), "-", 1:3)
    )
    on.exit(unlink(paths), add = TRUE)
    jobs <- Map(
        function(index, path) list(index = index, path = path),
        1:3,
        paths
    )
    expect_error(
        source__apply(
            jobs,
            function(job) {
                if (job$index == 1L) {
                    stop("source failed")
                }
                Sys.sleep(0.1)
                file.create(job$path)
            },
            function(job, value) NULL
        ),
        "source failed"
    )
    expect_true(file.exists(paths[[2L]]))
    expect_false(file.exists(paths[[3L]]))
    # A separate pool must still be usable after failure cleanup.
    value <- NULL
    source__apply(list(1), identity, function(job, result) value <<- result)
    expect_identical(value, 1)
})

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


# A one-file interactive workflow must retain cancellation and heartbeat polling.
test_that("a single visible source read leaves the reporter responsive", {
    withr::local_options(epwshiftr.mirai_workers = 1L)
    owner <- Sys.getpid()
    heartbeats <- 0L
    pid <- NULL
    reporter <- list(
        check_cancel = function() invisible(NULL),
        heartbeat = function(...) heartbeats <<- heartbeats + 1L
    )
    source__apply(
        list(1L),
        function(job) {
            Sys.sleep(0.1)
            Sys.getpid()
        },
        function(job, value) pid <<- value,
        reporter = reporter
    )
    expect_false(identical(pid, owner))
    expect_gt(heartbeats, 0L)
})

# A persistence error belongs to the owning process, but must not destroy the
# other reader's already started work or dispatch an additional source.
test_that("collector failures drain readers and stop dispatching", {
    withr::local_options(epwshiftr.mirai_workers = 2L)
    root <- withr::local_tempdir()
    jobs <- lapply(1:3, function(index) list(index = index, root = root))
    expect_error(
        source__apply(
            jobs,
            function(job) {
                if (job$index == 1L) {
                    deadline <- Sys.time() + 10
                    while (
                        !file.exists(file.path(job$root, "started-2")) &&
                            Sys.time() < deadline
                    ) {
                        Sys.sleep(0.01)
                    }
                }
                file.create(file.path(job$root, paste0("started-", job$index)))
                if (job$index == 2L) {
                    Sys.sleep(0.3)
                }
                file.create(file.path(job$root, paste0("done-", job$index)))
            },
            function(job, result) {
                stop("persist conflict")
            }
        ),
        "persist conflict"
    )
    expect_true(file.exists(file.path(root, "done-2")))
    expect_false(file.exists(file.path(root, "started-3")))
})

test_that("file isolation callbacks allow independent sources to finish", {
    for (workers in c(1L, 2L)) {
        withr::local_options(epwshiftr.mirai_workers = workers)
        done <- integer()
        failed <- integer()
        source__apply(
            as.list(1:3),
            function(job) {
                if (job == 1L) {
                    stop("unavailable source")
                }
                job
            },
            function(job, value) done <<- c(done, job),
            on_error = function(job, error) failed <<- c(failed, job)
        )
        expect_setequal(done, 2:3)
        expect_identical(failed, 1L)
    }
})
