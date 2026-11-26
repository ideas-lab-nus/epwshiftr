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
