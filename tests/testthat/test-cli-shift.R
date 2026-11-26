# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("summary plan IDs prefer normalized lineage and support legacy rows", {
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
    store <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)

    run_mutated_config <- function(
        mutator,
        args = c("shift", "run", "--dry-run")
    ) {
        payload <- jsonlite::read_json(
            config,
            simplifyVector = TRUE,
            simplifyDataFrame = FALSE
        )
        payload <- mutator(payload)
        path <- cli_shift_test_write_config(payload)
        cli_run(store, args, "--config", path)
    }

    dry_run <- suppressWarnings(cli_run(
        store,
        "shift",
        "run",
        "--config",
        config,
        "--dry-run"
    ))
    expect_equal(dry_run$status, 0L)
    expect_equal(dry_run$result$status, "dry_run")
    expect_equal(nrow(dry_run$result$cases), 1L)
    expect_true(all(
        c("transform", "reference", "cases", "output") %in%
            dry_run$result$explain$step
    ))

    validate <- cli_run(
        store,
        "shift",
        "config",
        "validate",
        "--config",
        config
    )
    expect_equal(validate$status, 0L)
    expect_equal(validate$result$status, "valid")

    example <- tempfile(fileext = ".json")
    written <- cli_run(
        store,
        "shift",
        "config",
        "example",
        "--output",
        example
    )
    expect_equal(written$status, 0L)
    expect_true(file.exists(example))
    expect_equal(
        cli_run(
            store,
            "shift",
            "config",
            "validate",
            "--config",
            example
        )$status,
        0L
    )

    invalid <- run_mutated_config(function(p) {
        p$sites[[1L]]$epw <- NULL
        p
    })
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "epw")

    invalid <- run_mutated_config(function(p) {
        p$surprise <- TRUE
        p
    })
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "surprise")

    # The previous split model/scenarios/cmip6 shape is not accepted.
    invalid <- run_mutated_config(function(p) {
        p$model <- p$climate$model
        p$scenarios <- p$climate$scenarios
        p$cmip6 <- p$climate[setdiff(
            names(p$climate),
            c("provider", "model", "scenarios")
        )]
        p$climate <- NULL
        p
    })
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "climate")

    invalid <- run_mutated_config(function(p) {
        p$periods <- list(`2060s` = "not-a-year")
        p
    })
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "invalid year")

    # A null optional model reference stays null and never receives a
    # parser-supplied historical default.
    baseline_reference <- run_mutated_config(function(p) {
        p["reference"] <- list(NULL)
        p
    })
    expect_equal(baseline_reference$status, 0L)
    expect_equal(
        baseline_reference$result$explain[step == "reference", detail],
        "none"
    )

    historical_ref <- list(
        mode = "historical",
        periods = list(reference = "1995:2014")
    )
    expect_equal(
        run_mutated_config(function(p) {
            p$reference <- historical_ref
            p
        })$status,
        0L
    )

    invalid <- run_mutated_config(
        function(p) {
            p$reference <- historical_ref
            p$observed_reference <- historical_ref
            p
        },
        args = c("shift", "config", "validate")
    )
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "observed_reference")

    expect_equal(
        run_mutated_config(function(p) {
            p$reference <- list(
                mode = "plan",
                plan_id = "REFERENCE_PLAN_ID",
                periods = list(reference = 1995L)
            )
            p
        })$status,
        1L
    )

    # Removed request/site/stage-list configs are intentionally rejected.
    invalid <- run_mutated_config(function(p) {
        list(request = list(), site = list())
    })
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
    queued <- cli_run(
        store,
        "shift",
        "run",
        "--config",
        config,
        "--background"
    )
    expect_equal(queued$status, 0L)
    expect_equal(queued$result$status, "queued")
    batch_id <- queued$result$batch_id
    expect_identical(launched$job$batch_id, batch_id)
    expect_true(all(is.na(queued$result$children$run_id)))
    expect_true(all(
        c("watch", "cancel", "logs") %in% queued$result$next_steps$step
    ))
    logs <- cli_run(store, "shift", "logs", "--batch", batch_id)
    expect_equal(logs$status, 0L)
    expect_equal(nrow(logs$result), 0L)
    writeLines(
        c("shared source reading", "child started"),
        file.path(launched$root, paste0(launched$job$id, ".log"))
    )
    logs <- cli_run(store, "shift", "logs", "--batch", batch_id, "--tail", "1")
    expect_identical(logs$result$message, "child started")
    cancelled <- cli_run(store, "shift", "cancel", "--batch", batch_id)
    expect_equal(cancelled$status, 0L)
    expect_equal(cancelled$result$status, "stopping")
    expect_error(
        shift_batch_execution__job_main(launched$root, launched$job$id),
        class = "epwshiftr_shift_cancelled"
    )
    status <- cli_run(store, "shift", "status", "--batch", batch_id)
    expect_identical(status$result$batch$status, "cancelled")
    conflict <- cli_run(
        tempfile(),
        "shift",
        "run",
        "--config",
        config,
        "--dry-run",
        "--background"
    )
    expect_equal(conflict$status, 2L)
    expect_match(conflict$error, "cannot be used together")
})

test_that("shift CLI reads live sidecars while a worker owns DuckDB", {
    skip_on_cran()
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

    status <- cli_run(store, "shift", "status", "--run", run_id)
    expect_equal(status$status, 0L)
    expect_equal(status$result$status, "queued")
    watch <- cli_run(store, "shift", "watch", "--run", run_id)
    expect_equal(watch$status, 0L)
    expect_equal(watch$result$run$run_id, run_id)

    for (i in seq_len(50L)) {
        if (file.exists(done)) {
            break
        }
        Sys.sleep(0.05)
    }
    expect_true(file.exists(done))
    cancelled <- cli_run(store, "shift", "cancel", "--run", run_id)
    expect_equal(cancelled$result$status, "cancelled")
})

test_that("shift CLI executes and inspects one persisted workflow run", {
    skip_on_cran()

    calls <- cli_shift_test_mock_monthly_fixtures()
    store <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)
    export_dir <- tempfile("cli-shift-export-")
    payload <- jsonlite::read_json(
        config,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE
    )
    payload$dir <- export_dir
    cli_shift_test_write_config(payload, config)

    result <- cli_run(store, "shift", "run", "--config", config)
    expect_equal(result$status, 0L, info = result$error)
    expect_equal(result$result$status, "completed")
    expect_length(result$result$batch_id, 1L)
    expect_length(result$result$children$run_id, 1L)
    expect_equal(nrow(result$result$outputs), 1L)
    expect_true(all(file.exists(result$result$outputs$export_path)))
    expect_true(all(result$result$cases$status == "completed"))
    expect_true("File" %in% calls$types)

    run_id <- result$result$children$run_id
    store <- result$result$children$store
    status <- cli_run(store, "shift", "status", "--run", run_id)
    expect_equal(status$status, 0L)
    expect_equal(status$result$run_id, run_id)
    expect_equal(status$result$status, "completed")

    show <- cli_run(store, "shift", "show", "--run", run_id)
    expect_equal(show$status, 0L)
    expect_named(
        show$result,
        c("run", "cases", "events", "outputs", "diagnostics", "explain")
    )
    expect_equal(show$result$run$run_id, run_id)
    expect_equal(nrow(show$result$outputs), 1L)

    watch <- cli_run(store, "shift", "watch", "--run", run_id, "--events", "2")
    expect_equal(watch$status, 0L)
    expect_named(
        watch$result,
        c("run", "cases", "outputs", "diagnostics", "events")
    )

    jsonl_text <- capture.output(
        jsonl_watch <- cli_run(
            store,
            "--jsonl",
            "shift",
            "watch",
            "--run",
            run_id,
            "--follow",
            "--count",
            "1",
            "--events",
            "1",
            quiet = FALSE
        )
    )
    expect_equal(jsonl_watch$status, 0L)
    jsonl_records <- lapply(jsonl_text, jsonlite::fromJSON)
    expect_equal(
        vapply(jsonl_records, `[[`, character(1L), "type"),
        c("snapshot", "terminal")
    )
    expect_equal(jsonl_records[[1L]]$snapshot$run$run_id, run_id)

    json_text <- capture.output(
        json_watch <- cli_run(
            store,
            "--json",
            "shift",
            "watch",
            "--run",
            run_id,
            "--follow",
            "--count",
            "1",
            "--events",
            "1",
            quiet = FALSE
        )
    )
    expect_equal(json_watch$status, 0L)
    # A single valid JSON document proves human watch snapshots were not mixed
    # into the machine-readable stdout contract.
    json_snapshot <- jsonlite::fromJSON(paste(json_text, collapse = "\n"))
    expect_equal(json_snapshot$run$run_id, run_id)

    diagnostics <- cli_run(store, "shift", "diagnostics", "--run", run_id)
    expect_equal(diagnostics$status, 0L)
    expect_false("error" %in% diagnostics$result$severity)

    outputs <- cli_run(store, "shift", "outputs", "--run", run_id)
    expect_equal(outputs$status, 0L)
    expect_equal(nrow(outputs$result), 1L)

    data <- cli_run(
        store,
        "shift",
        "data",
        "--run",
        run_id,
        "--columns",
        "case_id,period,dry_bulb_temperature",
        "--limit",
        "2"
    )
    expect_equal(data$status, 0L)
    expect_equal(nrow(data$result), 2L)

    resumed <- cli_run(store, "shift", "resume", "--run", run_id)
    expect_equal(resumed$status, 0L)
    expect_equal(resumed$result$status, "completed")
    expect_equal(resumed$result$run_id, run_id)

    rendered <- capture.output(
        rendered_show <- cli_run(
            store,
            "shift",
            "show",
            "--run",
            run_id,
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(rendered_show$status, 0L)
    expect_true(any(grepl("Shift workflow run", rendered)))
})

# Invalid inputs must retain their intended diagnostic in installations that
# contain runtime dependencies only, rather than relying on test dependencies.
test_that("CLI validation retains the original source error condition", {
    expect_error(
        weather__new_input(WEATHER_INPUT_ROLES[[1L]], source = NULL),
        "source.*cannot be NULL",
        class = "rlang_error"
    )
})

# vim: fdm=marker :
