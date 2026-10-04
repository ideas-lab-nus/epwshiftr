# Configuration crosses JSON and process boundaries without losing unlimited
# cache settings, and errors restore the caller's original options.
test_that("execution snapshots preserve settings and restore caller state", {
    withr::local_options(
        epwshiftr.query.timeout = 37,
        epwshiftr.cache_max_n = Inf,
        epwshiftr.mirai_workers = 3L
    )
    root <- withr::local_tempdir()
    job <- list(id = "execution-test", options = shift_execution__options())
    store_write_json_atomic(job, file.path(root, "batch-job.json"))
    saved <- jsonlite::read_json(
        file.path(root, "batch-job.json"),
        simplifyVector = TRUE
    )
    context <- shift_execution__context(root, saved)
    options(epwshiftr.query.timeout = 91)
    expect_error(
        shift_execution__run(context, {
            expect_identical(getOption("epwshiftr.query.timeout"), 37L)
            expect_identical(getOption("epwshiftr.cache_max_n"), Inf)
            stop("execution failure")
        }),
        "execution failure"
    )
    expect_equal(getOption("epwshiftr.query.timeout"), 91)
    expect_identical(shift_batch_execution__job_read(root)$status, "failed")
    expect_identical(
        shift_batch_execution__job_read(root)$message,
        "execution failure"
    )
})

# Cancellation is tied to an attempt and passed explicitly to its reporter.
test_that("independent reporters cannot inherit another execution's cancellation", {
    root <- withr::local_tempdir()
    context <- shift_execution__context(
        root,
        list(id = "cancelled-owner", options = shift_execution__options())
    )
    file.create(file.path(root, "cancelled-owner.cancel.json"))
    attached <- shift_reporter__reporter(shift_ui("none"), execution = context)
    unrelated <- shift_reporter__reporter(shift_ui("none"))
    on.exit(attached$close(), add = TRUE)
    on.exit(unrelated$close(), add = TRUE)
    expect_error(attached$check_cancel(), class = "epwshiftr_shift_cancelled")
    expect_no_error(unrelated$check_cancel())
    expect_error(
        shift_execution__run(context, stop("must not run")),
        class = "epwshiftr_shift_cancelled"
    )
    expect_identical(shift_batch_execution__job_read(root)$status, "cancelled")
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
    create <- shift_job__job_create
    local_mocked_bindings(shift_job__job_create = function(...) {
        job <- create(...)
        shift_job__cancel_request_write(
            store$path,
            job$run_id[[1L]],
            job$job_id[[1L]]
        )
        job
    })
    called <- FALSE
    expect_error(
        shift_run__task_execute(
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
    run_id <- shift_job__task_run_register(store, "collect", spec = list())
    job <- shift_job__job_create(
        store,
        run_id,
        mode = "foreground",
        ui = shift_ui("none")
    )
    result <- shift_execution__run(
        shift_execution__context(store$path, job, store),
        {
            shift_job__run_finish(
                store,
                run_id,
                "completed",
                current_stage = "collect"
            )
            shift_job__run_handle(store, run_id)
        }
    )
    expect_identical(result@meta$jobs$status, "completed")
    expect_false(anyNA(result@meta$jobs$completed_at))
})

# Animation callbacks must not turn one durable liveness check into repeated
# DuckDB reads. Explicit checks still bypass the heartbeat throttle.
test_that("standalone progress checks cancellation once per heartbeat", {
    store <- shift_store(withr::local_tempdir(), create = TRUE)
    on.exit(store$close(), add = TRUE)
    run_id <- shift_job__task_run_register(store, "collect", spec = list())
    job <- shift_job__job_create(
        store,
        run_id,
        mode = "foreground",
        ui = shift_ui("none")
    )
    context <- shift_execution__context(store$path, job, store)
    reporter <- shift_reporter__reporter(
        shift_ui("none", heartbeat = 60),
        execution = context
    )
    on.exit(reporter$close(), add = TRUE)
    checks <- 0L
    local_mocked_bindings(shift_job__job_check_cancel = function(...) {
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

# Snapshots retain NULL defaults and explicit FALSE/zero values without copying
# options owned by other packages or changing the current session.
test_that("execution snapshots read only supported options", {
    withr::local_options(
        epwshiftr.query.timeout = NULL,
        epwshiftr.query.connect_timeout = 11,
        epwshiftr.ui_height = NULL,
        epwshiftr.cache = FALSE,
        epwshiftr.cache_max_n = 0,
        unrelated_execution_setting = "keep"
    )
    actual <- shift_execution__options()
    expect_identical(actual$epwshiftr.query.timeout, 300)
    expect_identical(actual$epwshiftr.query.connect_timeout, 11)
    expect_true("epwshiftr.ui_height" %in% names(actual))
    expect_null(actual$epwshiftr.ui_height)
    expect_identical(actual$epwshiftr.cache, FALSE)
    expect_identical(actual$epwshiftr.cache_max_n, 0)
    expect_false("unrelated_execution_setting" %in% names(actual))
    expect_null(getOption("epwshiftr.query.timeout"))
    expect_identical(getOption("unrelated_execution_setting"), "keep")
})

# Arguments cross an R expression boundary before the shell boundary. Test
# round trips and rejection rather than assuming every value is a path string.
test_that("background string literals round trip without coercion", {
    values <- c(
        "",
        "two cities",
        'a"b',
        "C:\\weather\\file",
        "line\nnext",
        "广州"
    )
    for (value in values) {
        expect_identical(
            eval(parse(text = shift_execution__string_literal(value))),
            value
        )
    }
    expect_identical(shift_execution__string_literal(NULL), "NULL")
    for (value in list(
        character(),
        NA_character_,
        c("a", "b"),
        1,
        FALSE,
        list("a")
    )) {
        expect_error(shift_execution__string_literal(value))
    }
})

# Parse the complete generated expression to verify that library paths and job
# arguments survive quoting and that launch failures reach the caller.
test_that("background launch preserves strings and reports launch failure", {
    libraries <- c('/tmp/library "one"', "C:\\R libs")
    arguments <- list(root = "广州 / weather", id = 'job"1', optional = NULL)
    captured <- NULL
    status <- 0L
    local_mocked_bindings(
        shift_execution__library_paths = function() libraries
    )
    local_mocked_bindings(
        .package = "base",
        system2 = function(command, args, stdout, stderr, wait) {
            captured <<- list(
                args = args,
                stdout = stdout,
                stderr = stderr,
                wait = wait
            )
            status
        },
        shQuote = identity
    )
    expect_identical(
        shift_execution__launch(
            "shift_batch_execution__job_main",
            arguments,
            "job.log"
        ),
        0L
    )
    expressions <- parse(text = captured$args[[3L]])
    expect_identical(eval(expressions[[1L]][[2L]]), libraries)
    actual <- lapply(as.list(expressions[[3L]])[-1L], eval)
    expect_identical(actual, arguments)
    expect_identical(captured$stdout, "job.log")
    expect_identical(captured$stderr, "job.log")
    expect_false(captured$wait)
    status <- 1L
    expect_error(
        shift_execution__launch(
            "shift_batch_execution__job_main",
            arguments,
            "job.log"
        ),
        "Could not launch"
    )
})

# Refreshing coordinator status must leave a real child alive on every OS.
# A second round trip detects accidental termination by the first PID check.
test_that("process liveness checks do not terminate a worker", {
    profile <- paste0("pid-check-", basename(tempfile()))
    mirai::daemons(1L, dispatcher = FALSE, .compute = profile)
    on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
    first <- mirai::collect_mirai(mirai::mirai(
        Sys.getpid(),
        .compute = profile,
        .timeout = 60000L
    ))
    expect_type(first, "integer")
    expect_false(identical(first, Sys.getpid()))
    expect_true(downloader__pid_alive(first))
    second <- mirai::collect_mirai(mirai::mirai(
        Sys.getpid(),
        .compute = profile,
        .timeout = 60000L
    ))
    expect_identical(second, first)
    expect_true(downloader__pid_alive(first))
    expect_false(downloader__pid_alive(NA_integer_))
    expect_false(downloader__pid_alive(0L))
})

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
