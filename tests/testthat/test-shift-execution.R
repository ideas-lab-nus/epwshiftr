# Configuration crosses JSON and process boundaries without losing unlimited
# cache settings, and errors restore the caller's original options.
test_that("execution snapshots preserve settings and restore caller state", {
    withr::local_options(
        epwshiftr.query.timeout = 37,
        epwshiftr.cache_max_n = Inf,
        epwshiftr.mirai_workers = 3L
    )
    root <- withr::local_tempdir()
    job <- list(id = "execution-test", options = execution__options())
    store_write_json_atomic(job, file.path(root, "batch-job.json"))
    saved <- jsonlite::read_json(
        file.path(root, "batch-job.json"),
        simplifyVector = TRUE
    )
    context <- execution__context(root, saved)
    options(epwshiftr.query.timeout = 91)
    expect_error(
        execution__run(context, {
            expect_identical(getOption("epwshiftr.query.timeout"), 37L)
            expect_identical(getOption("epwshiftr.cache_max_n"), Inf)
            stop("execution failure")
        }),
        "execution failure"
    )
    expect_equal(getOption("epwshiftr.query.timeout"), 91)
    expect_identical(shift_batch__job_read(root)$status, "failed")
    expect_identical(shift_batch__job_read(root)$message, "execution failure")
})

# Cancellation is tied to an attempt and passed explicitly to its reporter.
test_that("independent reporters cannot inherit another execution's cancellation", {
    root <- withr::local_tempdir()
    context <- execution__context(
        root,
        list(id = "cancelled-owner", options = execution__options())
    )
    file.create(file.path(root, "cancelled-owner.cancel.json"))
    attached <- shift__reporter(shift_ui("none"), execution = context)
    unrelated <- shift__reporter(shift_ui("none"))
    on.exit(attached$close(), add = TRUE)
    on.exit(unrelated$close(), add = TRUE)
    expect_error(attached$check_cancel(), class = "epwshiftr_shift_cancelled")
    expect_no_error(unrelated$check_cancel())
    expect_error(
        execution__run(context, stop("must not run")),
        class = "epwshiftr_shift_cancelled"
    )
    expect_identical(shift_batch__job_read(root)$status, "cancelled")
})

# Real installed workers inherit effective user settings and never start nested
# pools. This is local IPC only and makes no remote data requests.
test_that("source workers share the execution settings snapshot", {
    withr::local_options(
        epwshiftr.mirai_workers = 2L,
        epwshiftr.query.timeout = 47,
        epwshiftr.cache_max_age = 123,
        epwshiftr.cache_max_size = 4096,
        epwshiftr.cache_max_n = 7
    )
    actual <- vector("list", 2L)
    source__apply(
        as.list(1:2),
        function(job) {
            options()[c(
                "epwshiftr.query.timeout",
                "epwshiftr.cache_max_age",
                "epwshiftr.cache_max_size",
                "epwshiftr.cache_max_n",
                "epwshiftr.mirai_workers"
            )]
        },
        function(job, value) actual[[job]] <<- value
    )
    expected <- list(
        epwshiftr.query.timeout = 47,
        epwshiftr.cache_max_age = 123,
        epwshiftr.cache_max_size = 4096,
        epwshiftr.cache_max_n = 7,
        epwshiftr.mirai_workers = 1L
    )
    expect_identical(actual, list(expected, expected))
    expect_identical(getOption("epwshiftr.mirai_workers"), 2L)
})

# Cancellation before a stage starts must finish both durable records without
# evaluating the source operation or leaving a running step behind.
test_that("early standalone cancellation updates the run and step", {
    store <- shift_store(withr::local_tempdir(), create = TRUE)
    on.exit(store$close(), add = TRUE)
    create <- shift__job_create
    local_mocked_bindings(shift__job_create = function(...) {
        job <- create(...)
        shift__cancel_request_write(
            store$path,
            job$run_id[[1L]],
            job$job_id[[1L]]
        )
        job
    })
    called <- FALSE
    expect_error(
        shift__task_execute(
            "collect",
            shift_request(),
            code = function(...) {
                called <<- TRUE
                stop("unexpected read")
            },
            store = store,
            ui = shift_ui("none")
        ),
        class = "epwshiftr_shift_cancelled"
    )
    expect_false(called)
    private <- morpher__private_store(store)
    expect_identical(private$read_table("shift_run")$status, "cancelled")
    expect_identical(private$read_table("shift_run_step")$status, "cancelled")
    expect_identical(private$read_table("shift_run_job")$status, "cancelled")
})

# A foreground result exposes its final attempt without requiring a second
# read through shift_refresh().
test_that("execution returns the final attempt snapshot", {
    store <- shift_store(withr::local_tempdir(), create = TRUE)
    on.exit(store$close(), add = TRUE)
    run_id <- shift__task_run_register(store, "collect", spec = list())
    job <- shift__job_create(
        store,
        run_id,
        mode = "foreground",
        ui = shift_ui("none")
    )
    result <- execution__run(execution__context(store$path, job, store), {
        shift__run_finish(store, run_id, "completed", current_stage = "collect")
        shift__run_handle(store, run_id)
    })
    expect_identical(result@meta$jobs$status, "completed")
    expect_false(anyNA(result@meta$jobs$completed_at))
})

# Animation callbacks must not turn one durable liveness check into repeated
# DuckDB reads. Explicit checks still bypass the heartbeat throttle.
test_that("standalone progress checks cancellation once per heartbeat", {
    store <- shift_store(withr::local_tempdir(), create = TRUE)
    on.exit(store$close(), add = TRUE)
    run_id <- shift__task_run_register(store, "collect", spec = list())
    job <- shift__job_create(
        store,
        run_id,
        mode = "foreground",
        ui = shift_ui("none")
    )
    context <- execution__context(store$path, job, store)
    reporter <- shift__reporter(
        shift_ui("none", heartbeat = 60),
        execution = context
    )
    on.exit(reporter$close(), add = TRUE)
    checks <- 0L
    local_mocked_bindings(shift__job_check_cancel = function(...) {
        checks <<- checks + 1L
    })
    reporter$heartbeat(force = TRUE)
    reporter$heartbeat()
    reporter$heartbeat()
    expect_identical(checks, 1L)
    reporter$check_cancel()
    expect_identical(checks, 2L)
    reporter$heartbeat(force = TRUE)
    expect_identical(checks, 3L)
})
