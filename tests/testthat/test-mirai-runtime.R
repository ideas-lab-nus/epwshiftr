# Pool lifecycle checks retain real concurrent execution and exercise shutdown
# immediately after cancellation or timeout, where native IPC waits occurred.
test_that("owned mirai pools preserve concurrency and survive interrupted tasks", {
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
