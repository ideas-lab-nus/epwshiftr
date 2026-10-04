# Use a real detached process to exercise the receipt handoff, source pool and
# child stores without any ESGF requests. Short fixtures may fail the weather
# method's coverage check; their native source reading must still finish.
test_that("background batches share reads through one coordinator", {
    fixture <- shared_inputs_test__fixture()
    cli_shift_test_mock_collect(fixture$docs)
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
    repeat {
        job <- shift_batch_execution__job_read(batch@store_path)
        if (!job$status %in% c("queued", "running", "stopping")) {
            break
        }
        if (Sys.time() > deadline) {
            log <- readLines(
                file.path(batch@store_path, paste0(job$id, ".log")),
                warn = FALSE
            )
            stop(paste(
                c("Background batch did not finish", job$message, log),
                collapse = "\n"
            ))
        }
        current <- shift_refresh(background)
        runs <- Filter(
            function(child) S7::S7_inherits(child, ShiftRun),
            current@meta$children
        )
        if (length(runs)) {
            observed <- TRUE
        }
        Sys.sleep(0.1)
    }
    log <- readLines(
        file.path(batch@store_path, paste0(job$id, ".log")),
        warn = FALSE
    )
    expect_identical(
        job$status,
        "finished",
        info = paste(c(job$message, log), collapse = "\n")
    )
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
    restored <- shift_refresh(background)
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
