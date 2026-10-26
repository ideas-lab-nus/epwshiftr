# Pool lifecycle checks retain real concurrent execution and exercise shutdown
# immediately after cancellation or timeout, where native IPC waits occurred.
test_that("owned mirai pools preserve concurrency and survive interrupted tasks", {
    skip_on_cran()
    profile <- paste0("epwshiftr-lifecycle-", Sys.getpid())
    on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
    mirai__start_pool(2L, .compute = profile)
    jobs <- lapply(seq_len(8L), function(i) {
        mirai::mirai(
            {
                Sys.sleep(0.03)
                Sys.getpid()
            },
            .compute = profile
        )
    })
    expect_length(unique(vapply(jobs, mirai::collect_mirai, integer(1L))), 2L)
    mirai::daemons(0L, .compute = profile)

    for (iteration in seq_len(20L)) {
        mirai__start_pool(1L, .compute = profile)
        job <- if (iteration %% 2L) {
            mirai::mirai(Sys.sleep(5), .timeout = 25, .compute = profile)
        } else {
            job <- mirai::mirai(Sys.sleep(5), .compute = profile)
            mirai::stop_mirai(job)
            job
        }
        expect_true(mirai::is_error_value(mirai::collect_mirai(job)))
        mirai::daemons(0L, .compute = profile)
        expect_identical(mirai::status(.compute = profile)$connections, 0L)
    }
})

# The copyable module's pool helper must run without the package namespace.
test_that("standalone worker startup has no host-package dependencies", {
    skip_on_cran()
    start <- downloader__start_pool
    environment(start) <- baseenv()
    profile <- paste0("standalone-lifecycle-", Sys.getpid())
    on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
    start(1L, .compute = profile)
    job <- mirai::mirai(42L, .compute = profile)
    expect_identical(mirai::collect_mirai(job), 42L)
})

# A persistent session retains its healthy slot when a different owned reader times out.
test_that("subset sessions reject busy reuse and replace only timed out readers", {
    skip_on_cran()
    session <- opendap__session(2L, .15)
    on.exit(session$close(), add = TRUE)
    pool <- session$acquire(2L, .15)
    identities <- vapply(pool$status(), function(s) s$pid, integer(1L))
    timed_out <- 0L
    ticket <- pool$submit(
        list(
            list(source_key = "slow", delay = 3),
            list(source_key = "fast", delay = 0)
        ),
        read = function(job) {
            Sys.sleep(job$delay)
            Sys.getpid()
        },
        collect = function(job, value) NULL,
        on_error = function(job, error) {
            stopifnot(inherits(error, "epwshiftr_source_timeout"))
            timed_out <<- timed_out + 1L
        }
    )
    expect_error(session$acquire(2L, .15))
    deadline <- proc.time()[["elapsed"]] + 10
    while (ticket$status == "pending") {
        stopifnot(proc.time()[["elapsed"]] < deadline)
        pool$poll()
        Sys.sleep(.01)
    }
    expect_identical(timed_out, 1L)
    expect_identical(ticket$status, "done")
    expect_true(pool$idle())
    after <- vapply(pool$status(), function(s) s$pid, integer(1L))
    expect_false(identical(identities[[1L]], after[[1L]]))
    expect_identical(identities[[2L]], after[[2L]])
    expect_identical(session$acquire(2L, .15), pool)
})
