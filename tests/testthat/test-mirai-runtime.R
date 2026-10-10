# Pool lifecycle checks retain real concurrent execution and exercise shutdown
# immediately after cancellation or timeout, where native IPC waits occurred.
test_that("owned mirai pools preserve concurrency and survive interrupted tasks", {
    profile <- paste0("epwshiftr-lifecycle-", Sys.getpid())
    on.exit(mirai::daemons(NULL, .compute = profile), add = TRUE)
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
    # Reusing an owned profile must release the old pool before replacement.
    mirai__start_pool(1L, .compute = profile)
    expect_identical(mirai::status(.compute = profile)$connections, 1L)
    expect_identical(
        mirai::collect_mirai(mirai::mirai(42L, .compute = profile)),
        42L
    )
    mirai::daemons(NULL, .compute = profile)

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
        mirai::daemons(NULL, .compute = profile)
        expect_identical(mirai::status(.compute = profile)$connections, 0L)
    }
})

# The copyable module's pool helper must run without the package namespace.
test_that("standalone worker startup has no host-package dependencies", {
    start <- downloader__start_pool
    environment(start) <- baseenv()
    profile <- paste0("standalone-lifecycle-", Sys.getpid())
    on.exit(mirai::daemons(NULL, .compute = profile), add = TRUE)
    start(1L, .compute = profile)
    job <- mirai::mirai(42L, .compute = profile)
    expect_identical(mirai::collect_mirai(job), 42L)
})
