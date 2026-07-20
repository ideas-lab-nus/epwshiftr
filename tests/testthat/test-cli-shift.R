test_that("shift run validates the task-oriented JSON config", {
    skip_if_not_installed("duckdb")

    store <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)

    dry_run <- suppressWarnings(epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", config, "--dry-run"
    )))
    expect_equal(dry_run$status, 0L)
    expect_equal(dry_run$result$status, "dry_run")
    expect_equal(nrow(dry_run$result$cases), 1L)
    expect_true(all(c("method", "reference", "cases", "output") %in% dry_run$result$explain$step))

    validate <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "config", "validate", "--config", config
    ))
    expect_equal(validate$status, 0L)
    expect_equal(validate$result$status, "valid")

    example <- tempfile(fileext = ".json")
    written <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "config", "example", "--output", example
    ))
    expect_equal(written$status, 0L)
    expect_true(file.exists(example))
    expect_equal(
        epwshiftr_cli(c("--quiet", "--store", store, "shift", "config", "validate", "--config", example))$status,
        0L
    )

    missing_epw <- tempfile(fileext = ".json")
    jsonlite::write_json(list(version = 1L), missing_epw, auto_unbox = TRUE)
    invalid <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", missing_epw, "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "epw")

    unknown_field <- tempfile(fileext = ".json")
    payload <- jsonlite::read_json(config, simplifyVector = TRUE)
    payload$surprise <- TRUE
    jsonlite::write_json(payload, unknown_field, auto_unbox = TRUE)
    invalid <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", unknown_field, "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "surprise")

    # The previous split model/scenarios/cmip6 shape is not accepted.
    legacy_climate <- tempfile(fileext = ".json")
    payload <- jsonlite::read_json(config, simplifyVector = TRUE)
    payload$model <- payload$climate$model
    payload$scenarios <- payload$climate$scenarios
    payload$cmip6 <- payload$climate[setdiff(names(payload$climate), c("provider", "model", "scenarios"))]
    payload$climate <- NULL
    jsonlite::write_json(payload, legacy_climate, auto_unbox = TRUE, null = "null")
    invalid <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", legacy_climate, "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "climate")

    invalid_period <- tempfile(fileext = ".json")
    payload <- jsonlite::read_json(config, simplifyVector = TRUE)
    payload$periods <- list(`2060s` = "not-a-year")
    jsonlite::write_json(payload, invalid_period, auto_unbox = TRUE)
    invalid <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", invalid_period, "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
    expect_match(invalid$error, "Invalid year")

    # Missing Belcher reference explicitly selects the baseline EPW and never
    # receives a parser-supplied historical default.
    missing_reference <- tempfile(fileext = ".json")
    payload <- jsonlite::read_json(config, simplifyVector = TRUE)
    payload$method <- list(name = "belcher")
    jsonlite::write_json(payload, missing_reference, auto_unbox = TRUE, null = "null")
    baseline_reference <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", missing_reference, "--dry-run"
    ))
    expect_equal(baseline_reference$status, 0L)
    expect_equal(
        baseline_reference$result$explain[step == "reference", detail],
        "baseline EPW"
    )

    historical <- tempfile(fileext = ".json")
    payload$method$reference <- list(
        mode = "historical",
        periods = list(reference = "1995:2014")
    )
    jsonlite::write_json(payload, historical, auto_unbox = TRUE)
    expect_equal(epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", historical, "--dry-run"
    ))$status, 0L)

    manual <- tempfile(fileext = ".json")
    payload$method$reference <- list(
        mode = "plan",
        plan_id = "REFERENCE_PLAN_ID",
        periods = list(reference = 1995L)
    )
    jsonlite::write_json(payload, manual, auto_unbox = TRUE)
    expect_equal(epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", manual, "--dry-run"
    ))$status, 0L)

    # Removed request/site/stage-list configs are intentionally rejected.
    legacy <- tempfile(fileext = ".json")
    jsonlite::write_json(list(request = list(), site = list()), legacy, auto_unbox = TRUE)
    invalid <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", legacy, "--dry-run"
    ))
    expect_equal(invalid$status, 2L)
})


test_that("shift CLI registers, inspects, and cancels background jobs", {
    skip_if_not_installed("duckdb")

    store <- tempfile("esg-background-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)
    launched <- new.env(parent = emptyenv())
    withr::local_options(list(epwshiftr.shift.launcher = function(store_path, run_id, job_id, log_path) {
        launched$args <- list(store_path = store_path, run_id = run_id,
            job_id = job_id, log_path = log_path)
        invisible(0L)
    }))

    queued <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", config,
        "--background"
    ))
    expect_equal(queued$status, 0L)
    expect_equal(queued$result$status, "queued")
    expect_equal(launched$args$run_id, queued$result$run_id)
    expect_true(all(c("watch", "cancel", "logs") %in% queued$result$next_steps$step))

    logs <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "logs", "--run", queued$result$run_id
    ))
    expect_equal(logs$status, 0L)
    expect_equal(nrow(logs$result), 0L)

    cancelled <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "cancel", "--run", queued$result$run_id
    ))
    expect_equal(cancelled$status, 0L)
    expect_equal(cancelled$result$status, "cancelled")

    conflict <- epwshiftr_cli(c(
        "--quiet", "--store", tempfile("esg-background-conflict-"),
        "shift", "run", "--config", config, "--dry-run", "--background"
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
    withr::local_options(list(
        epwshiftr.shift.launcher = function(...) invisible(0L)
    ))
    queued <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "run", "--config", config,
        "--background"
    ))
    run_id <- queued$result$run_id

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
        c("-e", shQuote(child_code),
          shQuote(file.path(store, "manifest.duckdb")),
          shQuote(ready), shQuote(done)),
        wait = FALSE, stdout = FALSE, stderr = FALSE
    )
    for (i in seq_len(50L)) {
        if (file.exists(ready)) break
        Sys.sleep(0.05)
    }
    expect_true(file.exists(ready))

    status <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "status", "--run", run_id
    ))
    expect_equal(status$status, 0L)
    expect_equal(status$result$status, "queued")
    watch <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "watch", "--run", run_id
    ))
    expect_equal(watch$status, 0L)
    expect_equal(watch$result$run$run_id, run_id)

    for (i in seq_len(50L)) {
        if (file.exists(done)) break
        Sys.sleep(0.05)
    }
    expect_true(file.exists(done))
    cancelled <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "cancel", "--run", run_id
    ))
    expect_equal(cancelled$result$status, "cancelled")
})


test_that("shift CLI executes and inspects one persisted workflow run", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    variables <- epw_morph_variables("recommended")
    nc <- stats::setNames(vapply(variables, function(variable_id) {
        path <- tempfile(fileext = ".nc")
        write_local_cmip6_netcdf_fixture(path, 2060L, variable_id = variable_id)
        path
    }, character(1L)), variables)
    on.exit(unlink(nc), add = TRUE)

    docs <- data.table::rbindlist(lapply(variables, function(variable_id) {
        cli_shift_test_file_docs(
            basename(nc[[variable_id]]),
            opendap_url = nc[[variable_id]],
            download_url = nc[[variable_id]],
            variable_id = variable_id
        )
    }), fill = TRUE)
    # Each synthetic variable represents a separate CMIP6 File identity.
    docs[, `:=`(
        dataset_id = paste0("future-", variable_id),
        master_id = paste0("future-", variable_id),
        instance_id = paste0("future-", variable_id, ".v20260101"),
        tracking_id = paste0("hdl:21.14100/future-", variable_id),
        id = paste0(title, "|future-", variable_id)
    )]
    calls <- cli_shift_test_mock_collect(docs)

    store <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)
    export_dir <- tempfile("cli-shift-export-")
    payload <- jsonlite::read_json(config, simplifyVector = TRUE)
    payload$dir <- export_dir
    jsonlite::write_json(payload, config, auto_unbox = TRUE, pretty = TRUE)

    result <- epwshiftr_cli(c("--quiet", "--store", store, "shift", "run", "--config", config))
    expect_equal(result$status, 0L)
    expect_equal(result$result$status, "completed")
    expect_length(result$result$run_id, 1L)
    expect_length(result$result$query_id, 1L)
    expect_length(result$result$morph_id, 1L)
    expect_equal(nrow(result$result$outputs), 1L)
    expect_true(all(file.exists(result$result$outputs$export_path)))
    expect_equal(nrow(result$result$missing), 0L)
    expect_true("File" %in% calls$types)

    run_id <- result$result$run_id
    status <- epwshiftr_cli(c("--quiet", "--store", store, "shift", "status", "--run", run_id))
    expect_equal(status$status, 0L)
    expect_equal(status$result$run_id, run_id)
    expect_equal(status$result$status, "completed")

    show <- epwshiftr_cli(c("--quiet", "--store", store, "shift", "show", "--run", run_id))
    expect_equal(show$status, 0L)
    expect_named(show$result, c("run", "cases", "events", "outputs", "diagnostics", "explain"))
    expect_equal(show$result$run$run_id, run_id)
    expect_equal(nrow(show$result$outputs), 1L)

    watch <- epwshiftr_cli(c("--quiet", "--store", store, "shift", "watch", "--run", run_id, "--events", "2"))
    expect_equal(watch$status, 0L)
    expect_named(watch$result, c("run", "cases", "outputs", "diagnostics", "events"))

    jsonl_text <- capture.output(
        jsonl_watch <- epwshiftr_cli(c(
            "--store", store, "--jsonl", "shift", "watch", "--run", run_id,
            "--follow", "--count", "1", "--events", "1"
        ))
    )
    expect_equal(jsonl_watch$status, 0L)
    expect_equal(jsonlite::fromJSON(jsonl_text[[1L]])$run$run_id, run_id)

    json_text <- capture.output(
        json_watch <- epwshiftr_cli(c(
            "--store", store, "--json", "shift", "watch", "--run", run_id,
            "--follow", "--count", "1", "--events", "1"
        ))
    )
    expect_equal(json_watch$status, 0L)
    # A single valid JSON document proves human watch snapshots were not mixed
    # into the machine-readable stdout contract.
    json_snapshot <- jsonlite::fromJSON(paste(json_text, collapse = "\n"))
    expect_equal(json_snapshot$run$run_id, run_id)

    diagnostics <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "diagnostics", "--run", run_id
    ))
    expect_equal(diagnostics$status, 0L)
    expect_named(diagnostics$result, shift_diagnostic_columns())

    outputs <- epwshiftr_cli(c("--quiet", "--store", store, "shift", "outputs", "--run", run_id))
    expect_equal(outputs$status, 0L)
    expect_equal(nrow(outputs$result), 1L)

    data <- epwshiftr_cli(c(
        "--quiet", "--store", store, "shift", "data", "--run", run_id,
        "--columns", "case_id,period,dry_bulb_temperature", "--limit", "2"
    ))
    expect_equal(data$status, 0L)
    expect_equal(nrow(data$result), 2L)

    resumed <- epwshiftr_cli(c("--quiet", "--store", store, "shift", "resume", "--run", run_id))
    expect_equal(resumed$status, 0L)
    expect_equal(resumed$result$status, "completed")
    expect_equal(resumed$result$run_id, run_id)

    rendered <- capture.output(
        rendered_show <- epwshiftr_cli(c("--store", store, "shift", "show", "--run", run_id)),
        type = "message"
    )
    expect_equal(rendered_show$status, 0L)
    expect_true(any(grepl("Shift workflow run", rendered)))
})
