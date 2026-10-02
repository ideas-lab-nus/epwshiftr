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
