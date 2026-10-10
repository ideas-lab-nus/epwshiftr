# One real pool checks overlapping readers, owning-process callbacks, isolated
# source failures and the settings inherited by both persistent workers.
test_that("source workers overlap, isolate failures and inherit execution settings", {
    withr::local_options(
        epwshiftr.mirai_workers = 2L,
        epwshiftr.query.timeout = 47,
        epwshiftr.cache_max_age = 123,
        epwshiftr.cache_max_size = 4096,
        epwshiftr.cache_max_n = 7
    )
    owner <- Sys.getpid()
    result <- vector("list", 5L)
    done <- integer()
    failed <- integer()
    source__apply(
        as.list(1:5),
        function(job) {
            if (job == 3L) {
                stop("unavailable source")
            }
            start <- as.numeric(Sys.time())
            Sys.sleep(0.15)
            list(
                pid = Sys.getpid(),
                start = start,
                stop = as.numeric(Sys.time()),
                settings = options()[c(
                    "epwshiftr.query.timeout",
                    "epwshiftr.cache_max_age",
                    "epwshiftr.cache_max_size",
                    "epwshiftr.cache_max_n",
                    "epwshiftr.mirai_workers"
                )]
            )
        },
        function(job, value) {
            expect_identical(Sys.getpid(), owner)
            done <<- c(done, job)
            result[[job]] <<- value
        },
        on_error = function(job, error) {
            expect_identical(Sys.getpid(), owner)
            expect_match(
                conditionMessage(error),
                "unavailable source",
                fixed = TRUE
            )
            failed <<- c(failed, job)
        }
    )
    # More jobs than initial slots verify that an isolated error does not
    # prevent the other sources completing; dispatch order remains unconstrained.
    expect_setequal(done, c(1L, 2L, 4L, 5L))
    expect_identical(failed, 3L)
    successful <- result[c(1L, 2L, 4L, 5L)]
    expect_length(unique(vapply(successful, `[[`, integer(1L), "pid")), 2L)
    expect_true(all(vapply(successful, `[[`, integer(1L), "pid") != owner))
    expect_lt(
        max(result[[1L]]$start, result[[2L]]$start),
        min(result[[1L]]$stop, result[[2L]]$stop)
    )
    expected <- list(
        epwshiftr.query.timeout = 47,
        epwshiftr.cache_max_age = 123,
        epwshiftr.cache_max_size = 4096,
        epwshiftr.cache_max_n = 7,
        epwshiftr.mirai_workers = 1L
    )
    expect_identical(
        lapply(successful, `[[`, "settings"),
        rep(list(expected), 4L)
    )
    expect_identical(getOption("epwshiftr.mirai_workers"), 2L)
    withr::local_options(epwshiftr.mirai_workers = 0L)
    expect_error(
        source__apply(list(1), identity, function(...) NULL),
        "positive|>=|greater"
    )
})

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

# vim: fdm=marker :
