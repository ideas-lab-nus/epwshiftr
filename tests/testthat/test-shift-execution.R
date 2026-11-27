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
    store <- shift_inspect__store(withr::local_tempdir(), create = TRUE)
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
# read through shift_inspect__refresh().
test_that("execution returns the final attempt snapshot", {
    store <- shift_inspect__store(withr::local_tempdir(), create = TRUE)
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
    store <- shift_inspect__store(withr::local_tempdir(), create = TRUE)
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
    skip_on_cran()
    profile <- paste0("pid-check-", basename(tempfile()))
    mirai__start_pool(1L, dispatcher = FALSE, .compute = profile)
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

# Use a real detached process to exercise the receipt handoff, source pool and
# child stores without any ESGF requests. Short fixtures may fail the weather
# method's coverage check; their native source reading must still finish.
test_that("background batches share reads through one coordinator", {
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    shift_test__mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    withr::local_options(epwshiftr.mirai_workers = 2L)
    background <- shift_run(batch, background = TRUE, ui = shift_ui("none"))
    original <- shift_batch_execution__job_read(batch@store_path)
    expect_equal(original$options$epwshiftr.mirai_workers, 2L)
    shift_resume(background, background = TRUE, ui = shift_ui("none"))
    expect_identical(
        shift_batch_execution__job_read(batch@store_path)$id,
        original$id
    )
    # Include instrumented package loading in the coordinator and source
    # workers. This checks lifecycle behavior, not a wall-clock performance SLA.
    deadline <- Sys.time() + 300
    observed <- FALSE
    # Windows keeps redirected stdout exclusively open until the process exits,
    # which may follow its terminal receipt. Diagnostics must not mask that
    # receipt or turn a successful task into a log-file access failure.
    failure_info <- function(job) {
        lines <- tryCatch(
            suppressWarnings(readLines(
                file.path(batch@store_path, paste0(job$id, ".log")),
                warn = FALSE
            )),
            error = function(error) {
                paste("Log unavailable:", conditionMessage(error))
            }
        )
        paste(c(job$status, job$message, lines), collapse = "\n")
    }
    repeat {
        job <- shift_batch_execution__job_read(batch@store_path)
        if (!job$status %in% c("queued", "running", "stopping")) {
            break
        }
        if (Sys.time() > deadline) {
            stop(paste(
                "Background batch did not finish",
                failure_info(job),
                sep = "\n"
            ))
        }
        current <- shift_inspect__refresh(background)
        runs <- Filter(
            function(child) S7::S7_inherits(child, ShiftRun),
            current@meta$children
        )
        if (length(runs)) {
            observed <- TRUE
        }
        Sys.sleep(0.1)
    }
    expect_identical(
        job$status,
        "finished",
        info = if (!identical(job$status, "finished")) failure_info(job)
    )
    # Completion is published before R releases handles and removes temporary
    # launch files. Observe normal process exit before disposing this fixture.
    while (downloader__pid_alive(job$pid) && Sys.time() < deadline) {
        Sys.sleep(0.1)
    }
    expect_false(downloader__pid_alive(job$pid))
    expect_null(job$progress)
    expect_false(identical(job$pid, Sys.getpid()))
    expect_true(observed)
    expect_true(
        length(list.files(
            file.path(batch@store_path, "shared-acquisitions"),
            pattern = "[.]json$",
            recursive = TRUE
        )) >
            0L
    )
    restored <- shift_inspect__refresh(background)
    expect_true(all(vapply(
        restored@meta$children,
        function(child) S7::S7_inherits(child, ShiftRun),
        logical(1L)
    )))
})

test_that("queued batch cancellation starts no source requests or children", {
    fixture <- shared_inputs_test__fixture()
    batch <- fixture$batch
    launched <- NULL
    local_mocked_bindings(shift_batch_execution__launch = function(root, job) {
        launched <<- job
    })
    background <- shift_run(batch, background = TRUE, ui = shift_ui("none"))
    stopped <- shift_cancel(background)
    expect_identical(shift_status(stopped), "stopping")
    local_mocked_bindings(shift_batch_plan__resolve_inputs = function(
        x,
        reporter
    ) {
        reporter$check_cancel()
        stop("unexpected input query")
    })
    expect_error(
        shift_batch_execution__job_main(batch@store_path, launched$id),
        class = "epwshiftr_shift_cancelled"
    )
    expect_identical(shift_status(background), "cancelled")
    expect_false(dir.exists(file.path(batch@store_path, "shared-acquisitions")))
    # A new explicit resume gets a different cancellation boundary.
    resumed <- shift_resume(
        background,
        background = TRUE,
        ui = shift_ui("none")
    )
    expect_false(identical(
        shift_batch_execution__job_read(batch@store_path)$id,
        basename(sub(
            "[.]cancel[.]json$",
            "",
            list.files(batch@store_path, pattern = "[.]cancel[.]json$")
        ))
    ))
    expect_identical(shift_status(resumed), "queued")
})

test_that("shared read progress is inspectable before child registration", {
    fixture <- shared_inputs_test__fixture()
    batch <- fixture$batch
    local_mocked_bindings(shift_batch__execute = function(
        x,
        ui,
        reporter,
        execution
    ) {
        reporter$heartbeat(
            details = list(
                unit_type = "source_reads",
                current = 1L,
                total = 3L,
                active = 2L
            ),
            force = TRUE
        )
        job <- shift_batch_execution__job_read(x@store_path)
        expect_equal(job$progress$current, 1L)
        expect_equal(job$progress$active, 2L)
        snapshot <- shift_batch_ui__snapshot(x, refresh = FALSE)
        expect_equal(snapshot$source_progress$total, 3L)
        expect_true(any(grepl(
            "1/3 files",
            shift_batch_ui__view(snapshot)$lines
        )))
        x
    })
    shift_run(batch, ui = shift_ui("none"))
    expect_false("epwshiftr.batch.context" %in% names(options()))
    expect_null(shift_batch_execution__job_read(batch@store_path)$progress)
})

test_that("CLI watch follows shared reads before any child is registered", {
    snapshot <- list(
        batch = list(status = "running"),
        children = data.table::data.table(status = "planned")
    )
    expect_true(epwshiftr_cli_shift_watch_active(snapshot))
    snapshot$batch$status <- "queued"
    expect_true(epwshiftr_cli_shift_watch_active(snapshot))
    snapshot$batch$status <- "cancelled"
    expect_false(epwshiftr_cli_shift_watch_active(snapshot))
    snapshot$children$status <- "running"
    expect_true(epwshiftr_cli_shift_watch_active(snapshot))
})

# Force the non-atomic copy fallback and try reading while its destination is
# incomplete. The reader must respect publication ownership, not parse or retry
# the partial file. Zero lock timeout makes this contention check deterministic.
test_that("batch readers cannot enter an incomplete receipt publication", {
    root <- withr::local_tempdir()
    expected <- list(id = "receipt-test", status = "finished", pid = 123L)
    shift_batch_execution__job_write(root, expected)
    with_lock <- manifest_with_lock
    local_mocked_bindings(manifest_with_lock = function(path, expr, ...) {
        with_lock(path, expr, timeout = 0)
    })
    copy_file <- base::file.copy
    copied <- FALSE
    local_mocked_bindings(
        file.rename = function(...) FALSE,
        file.copy = function(from, to, ...) {
            writeLines('{"status":', to)
            expect_error(
                shift_batch_execution__job_read(root),
                "Manifest is locked"
            )
            copied <<- TRUE
            copy_file(from, to, ...)
        },
        .package = "base"
    )
    shift_batch_execution__job_write(root, expected)
    expect_true(copied)
    expect_identical(shift_batch_execution__job_read(root), expected)
    expect_false(dir.exists(manifest_lock_path(file.path(
        root,
        "batch-job.json"
    ))))
})

# Failed replacement must be explicit and release the receipt lock. A surviving
# previous receipt must remain readable, with no leaked temporary JSON files.
test_that("failed receipt publication reports failure and releases ownership", {
    root <- withr::local_tempdir()
    previous <- list(id = "previous", status = "finished")
    shift_batch_execution__job_write(root, previous)
    local_mocked_bindings(
        file.rename = function(...) FALSE,
        file.copy = function(...) FALSE,
        .package = "base"
    )
    expect_error(
        shift_batch_execution__job_write(
            root,
            list(id = "next", status = "queued")
        ),
        "Could not publish batch receipt"
    )
    expect_identical(shift_batch_execution__job_read(root), previous)
    expect_identical(list.files(root, all.files = FALSE), "batch-job.json")
})

# Missing state remains distinct from unreadable or malformed state. Parsing
# failures propagate immediately and must not leave ownership behind.
test_that("missing and corrupt batch receipts remain distinguishable", {
    root <- withr::local_tempdir()
    expect_null(shift_batch_execution__job_read(file.path(root, "absent")))
    expect_null(shift_batch_execution__job_read(root))
    writeLines('{"status":', file.path(root, "batch-job.json"))
    expect_error(shift_batch_execution__job_read(root), "parse error")
    expect_false(dir.exists(manifest_lock_path(file.path(
        root,
        "batch-job.json"
    ))))
})

# vim: fdm=marker :
