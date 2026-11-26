# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

cli_shift_test__file_env <- environment()

# Reuse one immutable monthly fixture set and isolated extract cache across
# CLI workflow tests so repeated CLI runs reuse warm source slices without
# rebuilding NetCDF inputs or spawning duplicate source-reader daemons.
# cli_shift_test__mock_monthly_shared {{{
cli_shift_test__mock_monthly_shared <- local({
    docs <- NULL
    cache_dir <- NULL
    function(env = parent.frame()) {
        if (is.null(docs)) {
            variables <- epw_morph_variables(
                transform__recipe(monthly_transform("epwshiftr"))
            )
            dir <- withr::local_tempdir(
                pattern = "cli-shift-nc-",
                .local_envir = cli_shift_test__file_env
            )
            files <- stats::setNames(
                vapply(
                    variables,
                    function(variable_id) {
                        path <- file.path(
                            dir,
                            sprintf(
                                "%s_Amon_EC-Earth3_ssp585_2060.nc",
                                variable_id
                            )
                        )
                        write_local_cmip6_netcdf_fixture(
                            path,
                            2060L,
                            variable_id = variable_id,
                            frequency = "mon"
                        )
                        path
                    },
                    character(1L)
                ),
                variables
            )
            rows <- data.table::rbindlist(
                lapply(variables, function(variable_id) {
                    esgf_test__file_docs(
                        basename(files[[variable_id]]),
                        opendap_url = files[[variable_id]],
                        download_url = files[[variable_id]],
                        variable_id = variable_id,
                        frequency = "mon",
                        table_id = "Amon"
                    )
                }),
                fill = TRUE
            )
            future_id <- paste0("future-", rows$variable_id)
            data.table::set(rows, j = "dataset_id", value = future_id)
            data.table::set(rows, j = "master_id", value = future_id)
            data.table::set(
                rows,
                j = "instance_id",
                value = paste0(future_id, ".v20260101")
            )
            data.table::set(
                rows,
                j = "tracking_id",
                value = paste0("hdl:21.14100/", future_id)
            )
            data.table::set(
                rows,
                j = "id",
                value = paste0(rows$title, "|", future_id)
            )
            data.table::set(
                rows,
                j = "checksum",
                value = vapply(
                    files[rows$variable_id],
                    checksum_file,
                    character(1L),
                    algo = "sha256"
                )
            )
            docs <<- rows
            cache_dir <<- withr::local_tempdir(
                pattern = "cli-shift-cache-",
                .local_envir = cli_shift_test__file_env
            )
        }
        withr::local_options(
            list(epwshiftr.dir_cache = cache_dir),
            .local_envir = env
        )
        shift_test__mock_collect(data.table::copy(docs), env = env)
    }
})
# }}}

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
    release <- tempfile("cli-shift-lock-release-")
    done <- tempfile("cli-shift-lock-done-")
    # A separate raw DuckDB process reproduces the exclusive manifest lock
    # held by the real background worker without starting remote ESGF work.
    child_code <- paste(
        "library(duckdb)",
        "args <- commandArgs(TRUE)",
        "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
        "file.create(args[[2L]])",
        "deadline <- Sys.time() + 10",
        "while (!file.exists(args[[3L]]) && Sys.time() < deadline) Sys.sleep(0.02)",
        "dbDisconnect(conn, shutdown = TRUE)",
        "file.create(args[[4L]])",
        sep = "; "
    )
    system2(
        file.path(R.home("bin"), "Rscript"),
        c(
            "-e",
            shQuote(child_code),
            shQuote(file.path(store, "manifest.duckdb")),
            shQuote(ready),
            shQuote(release),
            shQuote(done)
        ),
        wait = FALSE,
        stdout = FALSE,
        stderr = FALSE
    )
    withr::defer({
        file.create(release)
        unlink(c(ready, release, done))
    })
    for (i in seq_len(500L)) {
        if (file.exists(ready)) {
            break
        }
        Sys.sleep(0.02)
    }
    expect_true(file.exists(ready))

    status <- cli_run(store, "shift", "status", "--run", run_id)
    expect_equal(status$status, 0L)
    expect_equal(status$result$status, "queued")
    watch <- cli_run(store, "shift", "watch", "--run", run_id)
    expect_equal(watch$status, 0L)
    expect_equal(watch$result$run$run_id, run_id)

    file.create(release)
    for (i in seq_len(500L)) {
        if (file.exists(done)) {
            break
        }
        Sys.sleep(0.02)
    }
    expect_true(file.exists(done))
    cancelled <- cli_run(store, "shift", "cancel", "--run", run_id)
    expect_equal(cancelled$result$status, "cancelled")
})

test_that("shift CLI executes and inspects one persisted workflow run", {
    skip_on_cran()

    calls <- cli_shift_test__mock_monthly_shared()
    store <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)

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

test_that("every catalog configuration can be described and generated offline", {
    testthat::local_mocked_bindings(
        EsgStore = list(new = function(...) stop("Unexpected store access")),
        .package = "epwshiftr"
    )
    catalog <- cli_run(NULL, "morph", "transforms")$result
    expect_equal(nrow(catalog), nrow(weather_transforms()))
    expect_true(all(
        c("required_inputs", "status", "output_type") %in% names(catalog)
    ))
    for (index in seq_len(nrow(catalog))) {
        row <- catalog[index]
        args <- c("--scale", row$scale, "--method", row$method)
        if (!is.na(row$reconstruction)) {
            args <- c(args, "--reconstruction", row$reconstruction)
        }
        described <- cli_run(NULL, "morph", "describe", args)
        expect_equal(described$status, 0L, info = paste(args, collapse = " "))
        expect_identical(described$result$status, row$status)
        expect_identical(described$result$output_type, row$output_type)
        expect_silent(jsonlite::toJSON(described$result, auto_unbox = TRUE))
        example <- epwshiftr_cli_shift_config_example(args)$config
        expect_silent(epwshiftr_cli_read_shift_config(
            cli_shift_test_write_config(example)
        ))
    }
    filtered <- cli_run(
        NULL,
        "morph",
        "transforms",
        "--scale",
        "hourly",
        "--status",
        "experimental"
    )$result
    expect_identical(filtered$method, "kernel_qdm")
})

test_that("CLI options preserve JSON vectors and enforce constructor constraints", {
    args <- c(
        "--quiet",
        "morph",
        "describe",
        "--scale",
        "daily",
        "--method",
        "qdm",
        "--option",
        "tas.bounds=[-40,60]"
    )
    result <- epwshiftr_cli(args)
    expect_equal(result$status, 0L)
    expect_equal(
        cli_morph__parse_options("tas.bounds=[-40,60]")$tas$bounds,
        c(-40, 60)
    )
    expect_match(result$result$options[option == "tas.bounds", value], "-40")
    expect_equal(
        epwshiftr_cli(c(args, "--option", "tas.bounds=[0,1]"))$status,
        2L
    )
    expect_equal(
        epwshiftr_cli(c(args[-length(args)], "tas.bounds=[-40,"))$status,
        2L
    )
    expect_error(cli_morph__parse_options("=1"), "Empty")
    expect_error(cli_morph__parse_options("missing"), "KEY=VALUE")
    expect_error(cli_morph__describe(c(
        "--scale",
        "daily",
        "--method",
        "qdm",
        "--option",
        "tas.bounds=[60,-40]"
    )))
})

test_that("human method details retain input alternatives and full option names", {
    withr::local_options(list(cli.width = 60L))
    output <- capture.output(
        result <- cli_run(
            NULL,
            "morph",
            "describe",
            "--scale",
            "monthly",
            "--method",
            "epwshiftr",
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(result$status, 0L)
    text <- paste(cli::ansi_strip(output), collapse = " ")
    expect_match(text, "Variables:.*tas.*OR.*tas")
    output <- capture.output(
        result <- cli_run(
            NULL,
            "morph",
            "describe",
            "--scale",
            "daily",
            "--method",
            "qdm",
            "--option",
            "tas.bounds=[-40,60]",
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(result$status, 0L)
    text <- paste(cli::ansi_strip(output), collapse = " ")
    expect_match(text, "tas.bounds \\(integer\\): default")
    expect_match(text, "selected[[:space:]]+c\\(-40L,")
})

test_that("config supports method matrices, typed models, ERA5, and explicit transforms", {
    config <- epwshiftr_cli_shift_config_example(c(
        "--methods",
        "original_morphing,qdm",
        "--model",
        "2"
    ))$config
    expect_identical(config$climate$model, 2L)
    expect_identical(config$calibration$dataset, "era5")
    config$control$refresh <- TRUE
    parsed <- epwshiftr_cli_read_shift_config(
        cli_shift_test_write_config(config)
    )
    climate <- epwshiftr_cli_config_climate(parsed$climate)
    expect_identical(climate@n_models, 2L)
    expect_null(climate@frequency)
    expect_true(epwshiftr_cli_config_control(parsed$control)@refresh)
    observed <- cli_shift__config_reference(parsed$calibration, "calibration")
    expect_true(S7::S7_inherits(observed, ShiftReanalysisSpec))
    expect_equal(observed@years, 1995:2014)

    config$methods <- NULL
    config$transform <- list(
        list(scale = "daily", method = "epwshiftr", reconstruction = "power"),
        list(scale = "daily", method = "epwshiftr", reconstruction = "btws")
    )
    config$climate["model"] <- list(NULL)
    parsed <- epwshiftr_cli_read_shift_config(
        cli_shift_test_write_config(config)
    )
    transforms <- cli_shift__config_transform(parsed$transform)
    expect_length(transforms, 2L)
    expect_identical(
        vapply(transforms, function(x) x@reconstruction, character(1L)),
        c("power", "btws")
    )
    expect_null(epwshiftr_cli_config_climate(parsed$climate)@n_models)

    config$methods <- "qdm"
    expect_error(
        epwshiftr_cli_read_shift_config(cli_shift_test_write_config(config)),
        "methods.*transform|transform.*methods"
    )
    config$methods <- NULL
    config$observed_reference <- config$calibration
    expect_error(
        epwshiftr_cli_read_shift_config(cli_shift_test_write_config(config)),
        "either calibration"
    )
    config$observed_reference <- NULL
    config$calibration$key <- "must-not-be-stored"
    expect_error(
        epwshiftr_cli_read_shift_config(cli_shift_test_write_config(config)),
        "key"
    )
})

test_that("local batch validation never discovers remote coverage", {
    testthat::local_mocked_bindings(
        shift_batch_ui__discover_models = function(...) {
            stop("Unexpected discovery")
        },
        .package = "epwshiftr"
    )
    config <- epwshiftr_cli_shift_config_example(c(
        "--methods",
        "original_morphing,bws_btws",
        "--model",
        "all"
    ))$config
    path <- cli_shift_test_write_config(config)
    result <- cli_run(
        tempfile(),
        "shift",
        "config",
        "validate",
        "--config",
        path
    )
    expect_equal(result$status, 0L)
    expect_identical(result$result$validation, "local")
    expect_equal(nrow(result$result$selected_models), 0L)
    expect_match(
        result$result$explain[step == "discovery", detail],
        "Not checked locally"
    )
})

test_that("network validation and doctor share reanalysis readiness checks", {
    network_flags <- logical()
    testthat::local_mocked_bindings(
        shift_check = function(x, network = FALSE, ...) {
            network_flags <<- c(network_flags, network)
            data.table::data.table(
                severity = "error",
                code = "cds_credentials",
                message = "Configure CDS access",
                action = "Set the CDS environment"
            )
        },
        .package = "epwshiftr"
    )
    config <- epwshiftr_cli_shift_config_example(c(
        "--methods",
        "qdm",
        "--model",
        "1"
    ))$config
    path <- cli_shift_test_write_config(config)
    root <- tempfile()
    local <- cli_run(root, "shift", "config", "validate", "--config", path)
    expect_equal(local$status, 1L)
    expect_identical(local$result$status, "valid")
    expect_identical(local$result$readiness, "blocked")
    remote <- cli_run(
        root,
        "shift",
        "config",
        "validate",
        "--config",
        path,
        "--network"
    )
    expect_identical(remote$result$validation, "network")
    expect_equal(nrow(remote$result$selected_models), 1L)
    doctor <- cli_run(root, "doctor", "--config", path)
    expect_true(any(doctor$result$checks$check == "cds_credentials"))
    expect_identical(network_flags, c(FALSE, TRUE, FALSE))
})

test_that("dry-run batch receipts reopen offline through CLI and R", {
    skip_on_cran()
    config <- epwshiftr_cli_shift_config_example(c(
        "--methods",
        "original_morphing,bws_btws",
        "--model",
        "2"
    ))$config
    config$sites[[1L]]$epw <- get_cache_epw()
    config$dir <- tempfile("cli-batch-output-")
    config$periods <- list(future = 2049:2050)
    root <- tempfile("cli-batch-store-")
    path <- cli_shift_test_write_config(config)
    text <- capture.output(
        result <- cli_run(
            root,
            "--json",
            "shift",
            "run",
            "--config",
            path,
            "--dry-run",
            quiet = FALSE
        )
    )
    expect_equal(result$status, 0L)
    expect_true(jsonlite::validate(paste(text, collapse = "\n")))
    id <- result$result$batch_id
    # Reopening uses persisted child plans, even if discovery is unavailable.
    testthat::local_mocked_bindings(
        shift_batch_ui__discover_models = function(...) {
            stop("Unexpected discovery")
        },
        .package = "epwshiftr"
    )
    batch <- shift_batch_get(id, root)
    expect_equal(nrow(shift_cases(batch)), 8L)
    expect_identical(shift_status(batch), "planned")
    for (command in c(
        "show",
        "status",
        "diagnostics",
        "outputs",
        "logs",
        "data",
        "cancel"
    )) {
        out <- cli_run(root, "shift", command, "--batch", id)
        expect_equal(out$status, 0L, info = command)
    }
    expect_silent(shift_watch(batch, follow = FALSE, ui = shift_ui("none")))
    snapshot <- shift_batch_ui__snapshot(batch, refresh = FALSE)
    for (width in c(48L, 59L, 60L, 80L, 112L, 120L)) {
        view <- shift_batch_ui__view(snapshot, width = width)
        expect_lte(max(cli::ansi_nchar(view$lines, type = "width")), width)
        expect_match(paste(view$lines, collapse = " "), "Model-A")
        expect_match(paste(view$lines, collapse = " "), "original_morphing")
        plain <- cli::ansi_strip(view$lines)
        if (width >= 60L) {
            expect_match(plain[[1L]], "^╭─ Future EPW Batch")
            expect_true(any(grepl("^├─ Workflows", plain)))
            expect_true(any(grepl("^├─ Results", plain)))
            expect_match(utils::tail(plain, 1L), "^╰─")
            expect_true(all(
                cli::ansi_nchar(view$lines, type = "width") == width - 1L
            ))
        } else {
            expect_false(any(grepl("^[╭├╰│]", plain)))
        }
    }
    # Borders must not truncate persisted identifiers, Unicode paths, or
    # diagnostics after the content budget becomes four columns narrower.
    snapshot$batch$batch_id <- paste0("batch_", strrep("abcdef", 18L))
    snapshot$batch$output_dir <- paste0("/天气/", strrep("future-output/", 12L))
    snapshot$diagnostics <- data.table::data.table(
        method = "original_morphing",
        model = "Model-A",
        severity = "warning",
        message = paste0("Check input ", strrep("reference-variable-", 8L))
    )
    for (width in c(48L, 60L, 80L)) {
        view <- shift_batch_ui__view(snapshot, width = width, detail = "detail")
        expect_true(all(
            cli::ansi_nchar(view$lines, type = "width") <= width - 1L
        ))
        content <- paste(
            gsub("^│ | │$", "", cli::ansi_strip(view$lines)),
            collapse = ""
        )
        content <- gsub("[[:space:]]", "", content)
        expect_match(content, snapshot$batch$batch_id, fixed = TRUE)
        expect_match(content, snapshot$batch$output_dir, fixed = TRUE)
        expect_match(
            content,
            gsub("[[:space:]]", "", snapshot$diagnostics$message),
            fixed = TRUE
        )
        expect_match(content, "original_morphing_field_equations", fixed = TRUE)
    }
    expect_error(
        cli_shift__target(
            epwshiftr_cli_parse_command(
                c("--run", "a", "--batch", "b"),
                options = c("--run", "--batch")
            ),
            root
        ),
        "exactly one"
    )
})

test_that("batch watch follows remaining children after an independent failure", {
    snapshots <- lapply(c("running", "completed"), function(status) {
        list(
            batch = data.table::data.table(
                status = "failed",
                active = as.integer(status == "running")
            ),
            children = data.table::data.table(status = c("failed", status)),
            events = data.table::data.table()
        )
    })
    index <- 0L
    testthat::local_mocked_bindings(
        shift_batch_get = function(...) NULL,
        shift_batch_ui__snapshot = function(...) {
            index <<- index + 1L
            snapshots[[index]]
        },
        .package = "epwshiftr"
    )
    output <- capture.output(
        result <- epwshiftr_cli_shift_watch_follow(
            NULL,
            "batch-test",
            batch_id = "batch-test",
            interval = 0,
            jsonl = TRUE,
            progress = "none"
        )
    )
    rows <- lapply(output, jsonlite::fromJSON)
    expect_identical(
        vapply(rows, `[[`, character(1L), "type"),
        c("snapshot", "terminal")
    )
    expect_equal(index, 2L)
})

test_that("completion facts distinguish cases, files, warnings, and field roles", {
    cases <- data.table::data.table(status = "completed")
    roles <- list(
        transformed_fields = "dry_bulb_temperature",
        derived_fields = "dew_point_temperature",
        physically_closed_fields = character(),
        inherited_fields = c("wind_direction", "present_weather_codes")
    )
    outputs <- data.table::data.table(
        weather_year = 2049:2050,
        provenance_json = jsonlite::toJSON(
            list(weather_field_roles = roles),
            auto_unbox = TRUE
        )
    )
    diagnostics <- data.table::data.table(
        severity = "warning",
        message = "Experimental method"
    )
    result <- shift_inspect__completion(cases, outputs, diagnostics)
    expect_identical(
        result$result_summary,
        "1/1 cases completed · 2 EPW files · 1 warnings"
    )
    expect_match(result$field_summary, "1 transformed.*1 derived.*2 inherited")
    expect_identical(result$warning_messages, "Experimental method")
    metadata <- cli_shift__output_metadata(outputs)
    expect_identical(metadata$inherited_fields[[1L]], roles$inherited_fields)
})

test_that("failed batch execution returns a nonzero CLI status with its receipt", {
    testthat::local_mocked_bindings(
        epwshiftr_cli_dispatch = function(parsed) {
            list(status = "partial", batch_id = "batch-partial")
        },
        .package = "epwshiftr"
    )
    result <- cli_run(NULL, "shift", "run")
    expect_equal(result$status, 1L)
    expect_identical(result$result$batch_id, "batch-partial")
    expect_null(result$error)
})

test_that("persisted batch plans execute, reuse artifacts, and repair missing exports", {
    skip_on_cran()
    test_local_dependencies(list(
        availability = function(...) {
            args <- list(...)
            args$source <- "EC-Earth3"
            do.call(test_cmip6_availability, args)
        }
    ))
    calls <- cli_shift_test__mock_monthly_shared()
    config_path <- tempfile(fileext = ".json")
    cli_shift_test_config(config_path)
    config <- epwshiftr_cli_read_shift_config(config_path)
    config$climate$model <- 1L
    config$control$overwrite <- FALSE
    config_path <- cli_shift_test_write_config(config)
    root <- tempfile("cli-batch-integration-")
    planned <- cli_run(
        root,
        "shift",
        "run",
        "--config",
        config_path,
        "--dry-run"
    )
    expect_equal(planned$status, 0L, info = planned$error)
    id <- planned$result$batch_id
    completed <- cli_run(root, "shift", "resume", "--batch", id)
    expect_equal(completed$status, 0L, info = completed$error)
    expect_identical(completed$result$status, "completed")
    output <- completed$result$outputs$export_path
    expect_length(output, 1L)
    expect_true(all(file.exists(output)))
    hash <- tools::md5sum(output)
    collected <- length(calls$types)
    child_ids <- completed$result$children$run_id
    receipt_path <- shift_batch__receipt_path(file.path(root, "batches", id))
    receipt_hash <- tools::md5sum(receipt_path)

    reused <- cli_run(root, "shift", "run", "--config", config_path)
    expect_equal(reused$status, 0L, info = reused$error)
    expect_identical(reused$result$children$run_id, child_ids)
    expect_identical(reused$result$execution$action, "reused")
    expect_identical(tools::md5sum(output), hash)
    expect_identical(tools::md5sum(receipt_path), receipt_hash)
    expect_equal(length(calls$types), collected)
    human <- capture.output(
        cli_run(root, "shift", "run", "--config", config_path, quiet = FALSE),
        type = "message"
    )
    expect_equal(
        sum(grepl("Future EPW Batch", cli::ansi_strip(human), fixed = TRUE)),
        1L
    )
    snapshot <- shift_batch_ui__snapshot(
        shift_batch_get(id, root),
        refresh = FALSE
    )
    expect_equal(snapshot$batch$completed, 1L)
    expect_equal(snapshot$batch$epw_files, 1L)
    data <- cli_run(
        root,
        "shift",
        "data",
        "--batch",
        id,
        "--limit",
        "2",
        "--columns",
        "method,model,dry_bulb_temperature"
    )
    expect_equal(data$status, 0L, info = data$error)
    expect_equal(nrow(data$result), 2L)
    expect_true(all(data$result$method == "epwshiftr"))
    expect_true(all(data$result$model == "EC-Earth3"))
    expect_true(all(is.finite(data$result$dry_bulb_temperature)))
    summary <- cli_run(root, "shift", "summary", "--batch", id, "--weather")
    expect_equal(summary$status, 0L, info = summary$error)
    expect_equal(summary$result$epw_files, 1L)
    expect_true(all(is.finite(summary$result$mean_temperature_c)))
    expect_equal(summary$result$weather_hours, 8760L)
    history <- cli_run(root, "shift", "list", "--type", "batch")
    expect_equal(history$status, 0L, info = history$error)
    expect_identical(history$result$status, "completed")
    reopened <- shift_batch_get(id, root)
    # DuckDB may emit its own first-open notice; the null UI itself must not
    # render progress or attempt to close a nonexistent frame renderer.
    expect_no_error(shift_watch(
        reopened,
        follow = FALSE,
        ui = shift_ui("none")
    ))

    # Remove only this test's exported file. The persisted plan and cached
    # scientific artifacts must be sufficient to reconstruct the delivery.
    unlink(output)
    repaired <- cli_run(root, "shift", "resume", "--batch", id)
    expect_equal(repaired$status, 0L, info = repaired$error)
    expect_identical(repaired$result$status, "completed")
    expect_true(all(file.exists(repaired$result$outputs$export_path)))
    expect_identical(tools::md5sum(output), hash)
})

# vim: fdm=marker :
