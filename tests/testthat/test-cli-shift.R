# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("summary plan IDs prefer normalized lineage and support legacy rows", {
    skip_if_not_installed("duckdb")

    store <- EsgStore$new(tempfile("summary-plan-store-"))
    on.exit(store$close(), add = TRUE)
    lineage <- data.frame(
        summary_plan_id = c("source-a", "source-b"),
        summary_id = "summary-current",
        plan_id = c("plan-a", "plan-b"),
        created_at = morpher__now(),
        stringsAsFactors = FALSE
    )
    morpher__replace_rows(
        store,
        "epw_climate_summary_plan",
        lineage,
        "summary_plan_id"
    )

    current <- data.table::data.table(
        summary_id = "summary-current",
        plan_id = NA_character_
    )
    expect_setequal(
        morpher__summary_plan_ids(store, current),
        c("plan-a", "plan-b")
    )

    legacy <- data.table::data.table(
        summary_id = "summary-legacy",
        plan_id = c("plan-c", "plan-d")
    )
    expect_setequal(
        morpher__summary_plan_ids(store, legacy),
        c("plan-c", "plan-d")
    )
})

test_that("shift run validates the task-oriented JSON config", {
    skip_if_not_installed("duckdb")

    store <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)

    dry_run <- suppressWarnings(epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "run",
        "--config",
        config,
        "--dry-run"
    )))
    expect_equal(dry_run$status, 0L)
    expect_equal(dry_run$result$status, "dry_run")
    expect_equal(nrow(dry_run$result$cases), 1L)
    expect_true(all(
        c("transform", "reference", "cases", "output") %in%
            dry_run$result$explain$step
    ))

    validate <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "config",
        "validate",
        "--config",
        config
    ))
    expect_equal(validate$status, 0L)
    expect_equal(validate$result$status, "valid")

    example <- tempfile(fileext = ".json")
    written <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "config",
        "example",
        "--output",
        example
    ))
    expect_equal(written$status, 0L)
    expect_true(file.exists(example))
    expect_equal(
        epwshiftr_cli(c(
            "--quiet",
            "--store",
            store,
            "shift",
            "config",
            "validate",
            "--config",
            example
        ))$status,
        0L
    )

    missing_epw <- tempfile(fileext = ".json")
    missing_baseline <- jsonlite::read_json(
        config,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE
    )
    missing_baseline$sites[[1L]]$epw <- NULL
    jsonlite::write_json(
        missing_baseline,
        missing_epw,
        auto_unbox = TRUE,
        null = "null"
    )
    invalid <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "run",
        "--config",
        missing_epw,
        "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "epw")

    unknown_field <- tempfile(fileext = ".json")
    payload <- jsonlite::read_json(
        config,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE
    )
    payload$surprise <- TRUE
    jsonlite::write_json(
        payload,
        unknown_field,
        auto_unbox = TRUE,
        null = "null"
    )
    invalid <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "run",
        "--config",
        unknown_field,
        "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "surprise")

    # The previous split model/scenarios/cmip6 shape is not accepted.
    legacy_climate <- tempfile(fileext = ".json")
    payload <- jsonlite::read_json(
        config,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE
    )
    payload$model <- payload$climate$model
    payload$scenarios <- payload$climate$scenarios
    payload$cmip6 <- payload$climate[setdiff(
        names(payload$climate),
        c("provider", "model", "scenarios")
    )]
    payload$climate <- NULL
    jsonlite::write_json(
        payload,
        legacy_climate,
        auto_unbox = TRUE,
        null = "null"
    )
    invalid <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "run",
        "--config",
        legacy_climate,
        "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "climate")

    invalid_period <- tempfile(fileext = ".json")
    payload <- jsonlite::read_json(
        config,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE
    )
    payload$periods <- list(`2060s` = "not-a-year")
    jsonlite::write_json(
        payload,
        invalid_period,
        auto_unbox = TRUE,
        null = "null"
    )
    invalid <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "run",
        "--config",
        invalid_period,
        "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "invalid year")

    # A null optional model reference stays null and never receives a
    # parser-supplied historical default.
    missing_reference <- tempfile(fileext = ".json")
    payload <- jsonlite::read_json(
        config,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE
    )
    payload["reference"] <- list(NULL)
    jsonlite::write_json(
        payload,
        missing_reference,
        auto_unbox = TRUE,
        null = "null"
    )
    baseline_reference <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "run",
        "--config",
        missing_reference,
        "--dry-run"
    ))
    expect_equal(baseline_reference$status, 0L)
    expect_equal(
        baseline_reference$result$explain[step == "reference", detail],
        "none"
    )

    historical <- tempfile(fileext = ".json")
    payload$reference <- list(
        mode = "historical",
        periods = list(reference = "1995:2014")
    )
    jsonlite::write_json(payload, historical, auto_unbox = TRUE)
    expect_equal(
        epwshiftr_cli(c(
            "--quiet",
            "--store",
            store,
            "shift",
            "run",
            "--config",
            historical,
            "--dry-run"
        ))$status,
        0L
    )

    observed_historical <- tempfile(fileext = ".json")
    payload$observed_reference <- list(
        mode = "historical",
        periods = list(reference = "1995:2014")
    )
    jsonlite::write_json(
        payload,
        observed_historical,
        auto_unbox = TRUE
    )
    invalid <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "config",
        "validate",
        "--config",
        observed_historical
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "observed_reference")
    payload$observed_reference <- NULL

    manual <- tempfile(fileext = ".json")
    payload$reference <- list(
        mode = "plan",
        plan_id = "REFERENCE_PLAN_ID",
        periods = list(reference = 1995L)
    )
    jsonlite::write_json(payload, manual, auto_unbox = TRUE)
    expect_equal(
        epwshiftr_cli(c(
            "--quiet",
            "--store",
            store,
            "shift",
            "run",
            "--config",
            manual,
            "--dry-run"
        ))$status,
        1L
    )

    # Removed request/site/stage-list configs are intentionally rejected.
    legacy <- tempfile(fileext = ".json")
    jsonlite::write_json(
        list(request = list(), site = list()),
        legacy,
        auto_unbox = TRUE
    )
    invalid <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "run",
        "--config",
        legacy,
        "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
})

test_that("shift watch JSONL follow emits typed event deltas", {
    first_events <- data.table::data.table(
        event_id = "event-1",
        run_id = "run-jsonl",
        stage = "resolve",
        status = "running",
        message = "Resolving",
        details_json = NA_character_,
        created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    )
    second_events <- data.table::rbindlist(list(
        first_events,
        data.table::data.table(
            event_id = "event-2",
            run_id = "run-jsonl",
            stage = "resolve",
            status = "completed",
            message = "Resolved",
            details_json = NA_character_,
            created_at = as.POSIXct("2026-01-01 00:00:01", tz = "UTC")
        )
    ))
    snapshots <- list(
        list(
            run = data.table::data.table(
                run_id = "run-jsonl",
                status = "running"
            ),
            cases = data.table::data.table(),
            outputs = data.table::data.table(),
            diagnostics = data.table::data.table(),
            events = first_events
        ),
        list(
            run = data.table::data.table(
                run_id = "run-jsonl",
                status = "completed"
            ),
            cases = data.table::data.table(),
            outputs = data.table::data.table(),
            diagnostics = data.table::data.table(),
            events = second_events
        )
    )
    index <- 0L
    testthat::local_mocked_bindings(
        epwshiftr_cli_shift_watch_snapshot = function(...) {
            index <<- index + 1L
            snapshot <- snapshots[[index]]
            attr(snapshot, "shift_ui_events") <- snapshot$events
            snapshot
        },
        .package = "epwshiftr"
    )

    output <- capture.output(
        result <- epwshiftr_cli_shift_watch_follow(
            store = "unused",
            run_id = "run-jsonl",
            event_count = 10L,
            interval = 0,
            count = 2L,
            jsonl = TRUE,
            quiet = FALSE,
            progress = "none"
        )
    )
    records <- lapply(output, jsonlite::fromJSON)

    expect_equal(
        vapply(records, `[[`, character(1L), "type"),
        c("snapshot", "event", "terminal")
    )
    expect_equal(records[[2L]]$event$event_id, "event-2")
    expect_equal(records[[3L]]$snapshot$run$run_id, "run-jsonl")
})

test_that("shift CLI maps reduced motion independently from detail", {
    parsed <- list(
        flags = list(
            "--reduced-motion" = TRUE,
            "--verbose" = FALSE,
            "--debug" = FALSE
        )
    )
    expect_identical(epwshiftr_cli_shift_motion(parsed), "reduced")
    expect_identical(epwshiftr_cli_shift_detail(parsed), "normal")
    parsed$flags[["--reduced-motion"]] <- FALSE
    expect_identical(epwshiftr_cli_shift_motion(parsed), "auto")
})


test_that("shift CLI registers, inspects, and cancels background batches", {
    skip_if_not_installed("duckdb")
    store <- tempfile("esg-background-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)
    launched <- NULL
    testthat::local_mocked_bindings(shift_batch_execution__launch = function(
        root,
        job
    ) {
        launched <<- list(root = root, job = job)
    })
    queued <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "run",
        "--config",
        config,
        "--background"
    ))
    expect_equal(queued$status, 0L)
    expect_equal(queued$result$status, "queued")
    batch_id <- queued$result$batch_id
    expect_identical(launched$job$batch_id, batch_id)
    expect_true(all(is.na(queued$result$children$run_id)))
    expect_true(all(
        c("watch", "cancel", "logs") %in% queued$result$next_steps$step
    ))
    logs <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "logs",
        "--batch",
        batch_id
    ))
    expect_equal(logs$status, 0L)
    expect_equal(nrow(logs$result), 0L)
    writeLines(
        c("shared source reading", "child started"),
        file.path(launched$root, paste0(launched$job$id, ".log"))
    )
    logs <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "logs",
        "--batch",
        batch_id,
        "--tail",
        "1"
    ))
    expect_identical(logs$result$message, "child started")
    cancelled <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "cancel",
        "--batch",
        batch_id
    ))
    expect_equal(cancelled$status, 0L)
    expect_equal(cancelled$result$status, "stopping")
    expect_error(
        shift_batch_execution__job_main(launched$root, launched$job$id),
        class = "epwshiftr_shift_cancelled"
    )
    status <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "status",
        "--batch",
        batch_id
    ))
    expect_identical(status$result$batch$status, "cancelled")
    conflict <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        tempfile(),
        "shift",
        "run",
        "--config",
        config,
        "--dry-run",
        "--background"
    ))
    expect_equal(conflict$status, 2L)
    expect_match(conflict$error, "cannot be used together")
})


test_that("shift CLI reads live sidecars while a worker owns DuckDB", {
    skip_if_not_installed("duckdb")
    skip_on_os("windows")

    store <- tempfile("esg-background-lock-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)
    test_local_dependencies(list(
        shift_job__launch_job = function(...) invisible(0L)
    ))
    # This test targets a standalone run's locked-store inspection. Batch
    # launch ownership is covered separately above.
    plan <- epwshiftr_cli_config_plan(
        epwshiftr_cli_read_shift_config(config),
        store = store,
        ui = shift_ui("none")
    )
    run <- shift_run(
        plan@meta$children[[1L]],
        background = TRUE,
        ui = shift_ui("none")
    )
    run_id <- run@ids$run_id
    store <- run@store_path

    ready <- tempfile("cli-shift-lock-ready-")
    done <- tempfile("cli-shift-lock-done-")
    # A separate raw DuckDB process reproduces the exclusive manifest lock
    # held by the real background worker without starting remote ESGF work.
    child_code <- paste(
        "library(duckdb)",
        "args <- commandArgs(TRUE)",
        "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
        "file.create(args[[2L]])",
        "Sys.sleep(1)",
        "dbDisconnect(conn, shutdown = TRUE)",
        "file.create(args[[3L]])",
        sep = "; "
    )
    system2(
        file.path(R.home("bin"), "Rscript"),
        c(
            "-e",
            shQuote(child_code),
            shQuote(file.path(store, "manifest.duckdb")),
            shQuote(ready),
            shQuote(done)
        ),
        wait = FALSE,
        stdout = FALSE,
        stderr = FALSE
    )
    for (i in seq_len(50L)) {
        if (file.exists(ready)) {
            break
        }
        Sys.sleep(0.05)
    }
    expect_true(file.exists(ready))

    status <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "status",
        "--run",
        run_id
    ))
    expect_equal(status$status, 0L)
    expect_equal(status$result$status, "queued")
    watch <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "watch",
        "--run",
        run_id
    ))
    expect_equal(watch$status, 0L)
    expect_equal(watch$result$run$run_id, run_id)

    for (i in seq_len(50L)) {
        if (file.exists(done)) {
            break
        }
        Sys.sleep(0.05)
    }
    expect_true(file.exists(done))
    cancelled <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "cancel",
        "--run",
        run_id
    ))
    expect_equal(cancelled$result$status, "cancelled")
})


# Planned receipts take the real CLI/public resume path up to execution. The
# full scientific execution and repair are exercised once by the batch case.
test_that("shift CLI resumes a planned receipt with the saved identity and UI", {
    skip_if_not_installed("duckdb")

    root <- withr::local_tempdir()
    config <- tempfile(fileext = ".json")
    withr::defer(unlink(config))
    cli_shift_test_config(config)
    base <- c("--quiet", "--store", root, "shift")
    planned <- epwshiftr_cli(c(
        base,
        "run",
        "--config",
        config,
        "--dry-run"
    ))
    expect_equal(planned$status, 0L, info = planned$error)

    received <- NULL
    # Preserve actual target restoration, resume dispatch and queued job writes;
    # stop only at the shared execution seam before native data are acquired.
    testthat::local_mocked_bindings(
        shift_batch_execution__run_execution = function(x, job, ui) {
            received <<- list(batch = x, job = job, ui = ui)
            x
        },
        .package = "epwshiftr"
    )
    resumed <- epwshiftr_cli(c(
        base,
        "resume",
        "--batch",
        planned$result$batch_id
    ))
    expect_equal(resumed$status, 0L, info = resumed$error)
    expect_s7_class(received$batch, ShiftBatch)
    expect_identical(received$batch@ids$batch_id, planned$result$batch_id)
    expect_true(all(vapply(
        received$batch@meta$children,
        function(child) S7::S7_inherits(child, ShiftPlan),
        logical(1L)
    )))
    expect_identical(received$job$batch_id, planned$result$batch_id)
    expect_identical(received$job$status, "queued")
    expect_false(received$job$background)
    expect_identical(received$ui@progress, "none")
    expect_false(received$ui@batch_receipt)
    saved <- shift_batch_execution__job_read(received$batch@store_path)
    expect_identical(saved$id, received$job$id)
    expect_identical(saved$batch_id, received$job$batch_id)
})

# vim: fdm=marker :
