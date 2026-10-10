# Preserve inspectable CLI failures before temporary stores disappear. Capture
# raw returns first so a failing diagnostic reader cannot erase the root cause.
# cli_batch__failure_evidence {{{
cli_batch__failure_evidence <- function(
    planned,
    completed,
    root,
    config_path,
    docs,
    files,
    calls
) {
    if (
        identical(planned$status, 0L) &&
            identical(completed$status, 0L) &&
            identical(completed$result$status, "completed")
    ) {
        return(completed$error)
    }
    reason <- paste(
        c(
            completed$error,
            completed$result$status,
            completed$result$diagnostics$message
        ),
        collapse = "; "
    )
    tryCatch(
        {
            parent <- Sys.getenv(
                "EPWSHIFTR_TEST_EVIDENCE_DIR",
                file.path(getwd(), "failure-evidence")
            )
            dir.create(parent, recursive = TRUE, showWarnings = FALSE)
            directory <- tempfile(
                paste0("cli-batch-", Sys.getpid(), "-"),
                parent
            )
            dir.create(directory)
            saveRDS(
                list(
                    planned = planned,
                    completed = completed,
                    store = root,
                    config_path = config_path,
                    docs = docs,
                    files = file.info(files),
                    calls = as.list(calls),
                    options = options()[startsWith(
                        names(options()),
                        "epwshiftr."
                    )]
                ),
                file.path(directory, "cli-results.rds")
            )
            file.copy(config_path, file.path(directory, "config.json"))
            # Preserve original receipt bytes before inspection can refresh a run.
            receipts <- list.files(root, pattern = "[.]json$", recursive = TRUE)
            for (relative in receipts) {
                target <- file.path(directory, "receipts", relative)
                dir.create(
                    dirname(target),
                    recursive = TRUE,
                    showWarnings = FALSE
                )
                file.copy(file.path(root, relative), target)
            }
            # Each read is independent: one unreadable manifest must not hide peers.
            capture <- function(expr) {
                tryCatch(force(expr), error = function(error) {
                    list(error = conditionMessage(error), class = class(error))
                })
            }
            id <- completed$result$batch_id
            if (is.null(id)) {
                id <- planned$result$batch_id
            }
            batch <- capture(shift_batch_get(id, root))
            state <- list(batch = batch)
            if (S7::S7_inherits(batch, ShiftBatch)) {
                state$diagnostics <- capture(shift_diagnostics(
                    batch,
                    refresh = FALSE
                ))
                state$children <- lapply(batch@meta$children, function(child) {
                    list(
                        ids = child@ids,
                        meta = child@meta,
                        diagnostics = capture(shift_diagnostics(
                            child,
                            refresh = FALSE
                        )),
                        logs = if (S7::S7_inherits(child, ShiftRun)) {
                            capture(shift_logs(
                                child,
                                tail = .Machine$integer.max
                            ))
                        } else {
                            NULL
                        }
                    )
                })
            }
            saveRDS(state, file.path(directory, "workflow-diagnostics.rds"))
            paste(
                reason,
                "Failure evidence:",
                normalizePath(directory, winslash = "/")
            )
        },
        error = function(error) {
            paste(
                reason,
                "Failure evidence capture error:",
                conditionMessage(error)
            )
        }
    )
}
# }}}

# Write through JSON so tests exercise the actual command-line configuration
# boundary, including nulls and arrays of transform objects.
# cli_batch__write_config {{{
cli_batch__write_config <- function(config) {
    path <- tempfile(fileext = ".json")
    jsonlite::write_json(config, path, auto_unbox = TRUE, null = "null")
    path
}
# }}}

# Resolve deterministic local model coverage while preserving real planning,
# receipt persistence, and inspection code.
# cli_batch__local_catalog {{{
cli_batch__local_catalog <- function() {
    list(
        availability = test_cmip6_availability,
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    )
}
# }}}

test_that("every catalog configuration can be described and generated offline", {
    testthat::local_mocked_bindings(
        EsgStore = list(new = function(...) stop("Unexpected store access")),
        .package = "epwshiftr"
    )
    catalog <- epwshiftr_cli(c("--quiet", "morph", "transforms"))$result
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
        described <- epwshiftr_cli(c("--quiet", "morph", "describe", args))
        expect_equal(described$status, 0L, info = paste(args, collapse = " "))
        expect_identical(described$result$status, row$status)
        expect_identical(described$result$output_type, row$output_type)
        expect_silent(jsonlite::toJSON(described$result, auto_unbox = TRUE))
        example <- epwshiftr_cli_shift_config_example(args)$config
        expect_silent(epwshiftr_cli_read_shift_config(cli_batch__write_config(
            example
        )))
    }
    filtered <- epwshiftr_cli(c(
        "--quiet",
        "morph",
        "transforms",
        "--scale",
        "hourly",
        "--status",
        "experimental"
    ))$result
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
        result <- epwshiftr_cli(c(
            "morph",
            "describe",
            "--scale",
            "monthly",
            "--method",
            "epwshiftr"
        )),
        type = "message"
    )
    expect_equal(result$status, 0L)
    text <- paste(cli::ansi_strip(output), collapse = " ")
    expect_match(text, "Variables:.*tas.*OR.*tas")
    output <- capture.output(
        result <- epwshiftr_cli(c(
            "morph",
            "describe",
            "--scale",
            "daily",
            "--method",
            "qdm",
            "--option",
            "tas.bounds=[-40,60]"
        )),
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
    parsed <- epwshiftr_cli_read_shift_config(cli_batch__write_config(config))
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
    parsed <- epwshiftr_cli_read_shift_config(cli_batch__write_config(config))
    transforms <- cli_shift__config_transform(parsed$transform)
    expect_length(transforms, 2L)
    expect_identical(
        vapply(transforms, function(x) x@reconstruction, character(1L)),
        c("power", "btws")
    )
    expect_null(epwshiftr_cli_config_climate(parsed$climate)@n_models)

    config$methods <- "qdm"
    expect_error(
        epwshiftr_cli_read_shift_config(cli_batch__write_config(config)),
        "methods.*transform|transform.*methods"
    )
    config$methods <- NULL
    config$observed_reference <- config$calibration
    expect_error(
        epwshiftr_cli_read_shift_config(cli_batch__write_config(config)),
        "either calibration"
    )
    config$observed_reference <- NULL
    config$calibration$key <- "must-not-be-stored"
    expect_error(
        epwshiftr_cli_read_shift_config(cli_batch__write_config(config)),
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
    path <- cli_batch__write_config(config)
    result <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        tempfile(),
        "shift",
        "config",
        "validate",
        "--config",
        path
    ))
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
    test_local_dependencies(cli_batch__local_catalog())
    config <- epwshiftr_cli_shift_config_example(c(
        "--methods",
        "qdm",
        "--model",
        "1"
    ))$config
    path <- cli_batch__write_config(config)
    root <- tempfile()
    local <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        root,
        "shift",
        "config",
        "validate",
        "--config",
        path
    ))
    expect_equal(local$status, 1L)
    expect_identical(local$result$status, "valid")
    expect_identical(local$result$readiness, "blocked")
    remote <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        root,
        "shift",
        "config",
        "validate",
        "--config",
        path,
        "--network"
    ))
    expect_identical(remote$result$validation, "network")
    expect_equal(nrow(remote$result$selected_models), 1L)
    doctor <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        root,
        "doctor",
        "--config",
        path
    ))
    expect_true(any(doctor$result$checks$check == "cds_credentials"))
    expect_identical(network_flags, c(FALSE, TRUE, FALSE))
})

test_that("dry-run batch receipts reopen offline through CLI and R", {
    test_local_dependencies(cli_batch__local_catalog())
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
    path <- cli_batch__write_config(config)
    text <- capture.output(
        result <- epwshiftr_cli(c(
            "--json",
            "--store",
            root,
            "shift",
            "run",
            "--config",
            path,
            "--dry-run"
        ))
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
        out <- epwshiftr_cli(c(
            "--quiet",
            "--store",
            root,
            "shift",
            command,
            "--batch",
            id
        ))
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
    result <- epwshiftr_cli(c("--quiet", "shift", "run"))
    expect_equal(result$status, 1L)
    expect_identical(result$result$batch_id, "batch-partial")
    expect_null(result$error)
})

test_that("persisted batch plans execute, support CLI inspection, reuse and repair exports", {
    skip_if_not_installed("RNetCDF")
    skip_if_not_installed("duckdb")
    test_local_dependencies(list(
        availability = function(...) {
            args <- list(...)
            args$source <- "EC-Earth3"
            do.call(test_cmip6_availability, args)
        },
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    variables <- epw_morph_variables(transform__recipe(monthly_transform(
        "epwshiftr"
    )))
    files <- stats::setNames(
        vapply(
            variables,
            function(variable) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    2060L,
                    variable_id = variable,
                    frequency = "mon"
                )
                path
            },
            character(1L)
        ),
        variables
    )
    withr::defer(unlink(files))
    docs <- data.table::rbindlist(
        lapply(variables, function(variable) {
            esgf_test__file_docs(
                basename(files[[variable]]),
                opendap_url = files[[variable]],
                download_url = files[[variable]],
                variable_id = variable,
                frequency = "mon",
                table_id = "Amon"
            )
        }),
        fill = TRUE
    )
    docs[, `:=`(
        dataset_id = paste0("future-", variable_id),
        master_id = paste0("future-", variable_id),
        instance_id = paste0("future-", variable_id, ".v20260101"),
        tracking_id = paste0("hdl:21.14100/future-", variable_id),
        id = paste0(title, "|future-", variable_id)
    )]
    data.table::set(
        docs,
        j = "checksum",
        value = vapply(
            files[docs$variable_id],
            checksum_file,
            character(1L),
            algo = "sha256"
        )
    )
    calls <- cli_shift_test_mock_collect(docs)
    config_path <- tempfile(fileext = ".json")
    cli_shift_test_config(config_path)
    config <- epwshiftr_cli_read_shift_config(config_path)
    config$climate$model <- 1L
    config$control$overwrite <- FALSE
    config_path <- cli_batch__write_config(config)
    root <- tempfile("cli-batch-integration-")
    base <- c("--quiet", "--store", root, "shift")
    planned <- epwshiftr_cli(c(
        base,
        "run",
        "--config",
        config_path,
        "--dry-run"
    ))
    expect_equal(planned$status, 0L, info = planned$error)
    id <- planned$result$batch_id
    completed <- epwshiftr_cli(c(base, "run", "--config", config_path))
    failure_info <- cli_batch__failure_evidence(
        planned,
        completed,
        root,
        config_path,
        docs,
        files,
        calls
    )
    expect_equal(completed$status, 0L, info = failure_info)
    expect_identical(completed$result$status, "completed")
    expect_identical(completed$result$batch_id, id)
    output <- completed$result$outputs$export_path
    expect_length(output, 1L)
    expect_true(all(file.exists(output)))
    hash <- tools::md5sum(output)
    collected <- length(calls$types)
    child_ids <- completed$result$children$run_id
    receipt_path <- shift_batch__receipt_path(file.path(root, "batches", id))
    receipt_hash <- tools::md5sum(receipt_path)

    reused <- epwshiftr_cli(c(base, "run", "--config", config_path))
    expect_equal(reused$status, 0L, info = reused$error)
    expect_identical(reused$result$children$run_id, child_ids)
    expect_identical(reused$result$execution$action, "reused")
    expect_identical(tools::md5sum(output), hash)
    expect_identical(tools::md5sum(receipt_path), receipt_hash)
    expect_equal(length(calls$types), collected)
    human <- capture.output(
        epwshiftr_cli(c(
            "--store",
            root,
            "shift",
            "run",
            "--config",
            config_path
        )),
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
    data <- epwshiftr_cli(c(
        base,
        "data",
        "--batch",
        id,
        "--limit",
        "2",
        "--columns",
        "method,model,dry_bulb_temperature"
    ))
    expect_equal(data$status, 0L, info = data$error)
    expect_equal(nrow(data$result), 2L)
    expect_true(all(data$result$method == "epwshiftr"))
    expect_true(all(data$result$model == "EC-Earth3"))
    expect_true(all(is.finite(data$result$dry_bulb_temperature)))
    summary <- epwshiftr_cli(c(base, "summary", "--batch", id, "--weather"))
    expect_equal(summary$status, 0L, info = summary$error)
    expect_equal(summary$result$epw_files, 1L)
    expect_true(all(is.finite(summary$result$mean_temperature_c)))
    expect_equal(summary$result$weather_hours, 8760L)
    history <- epwshiftr_cli(c(base, "list", "--type", "batch"))
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

    # Reuse this real child for run-scoped command and output-format contracts.
    expect_length(completed$result$batch_id, 1L)
    expect_length(completed$result$children$run_id, 1L)
    expect_true(all(completed$result$cases$status == "completed"))
    expect_true("File" %in% calls$types)

    run_id <- completed$result$children$run_id
    store <- completed$result$children$store
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
    expect_equal(status$result$run_id, run_id)
    expect_equal(status$result$status, "completed")

    show <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "show",
        "--run",
        run_id
    ))
    expect_equal(show$status, 0L)
    expect_named(
        show$result,
        c("run", "cases", "events", "outputs", "diagnostics", "explain")
    )
    expect_equal(show$result$run$run_id, run_id)
    expect_equal(nrow(show$result$outputs), 1L)

    watch <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "watch",
        "--run",
        run_id,
        "--events",
        "2"
    ))
    expect_equal(watch$status, 0L)
    expect_named(
        watch$result,
        c("run", "cases", "outputs", "diagnostics", "events")
    )

    jsonl_text <- capture.output(
        jsonl_watch <- epwshiftr_cli(c(
            "--store",
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
            "1"
        ))
    )
    expect_equal(jsonl_watch$status, 0L)
    jsonl_records <- lapply(jsonl_text, jsonlite::fromJSON)
    expect_equal(
        vapply(jsonl_records, `[[`, character(1L), "type"),
        c("snapshot", "terminal")
    )
    expect_equal(jsonl_records[[1L]]$snapshot$run$run_id, run_id)

    json_text <- capture.output(
        json_watch <- epwshiftr_cli(c(
            "--store",
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
            "1"
        ))
    )
    expect_equal(json_watch$status, 0L)
    # A single valid JSON document proves human watch snapshots were not mixed
    # into the machine-readable stdout contract.
    json_snapshot <- jsonlite::fromJSON(paste(json_text, collapse = "\n"))
    expect_equal(json_snapshot$run$run_id, run_id)

    diagnostics <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "diagnostics",
        "--run",
        run_id
    ))
    expect_equal(diagnostics$status, 0L)
    expect_named(diagnostics$result, shift_stage__diagnostic_columns())

    outputs <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "outputs",
        "--run",
        run_id
    ))
    expect_equal(outputs$status, 0L)
    expect_equal(nrow(outputs$result), 1L)

    data <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "data",
        "--run",
        run_id,
        "--columns",
        "case_id,period,dry_bulb_temperature",
        "--limit",
        "2"
    ))
    expect_equal(data$status, 0L)
    expect_equal(nrow(data$result), 2L)

    resumed <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        store,
        "shift",
        "resume",
        "--run",
        run_id
    ))
    expect_equal(resumed$status, 0L)
    expect_equal(resumed$result$status, "completed")
    expect_equal(resumed$result$run_id, run_id)

    rendered <- capture.output(
        rendered_show <- epwshiftr_cli(c(
            "--store",
            store,
            "shift",
            "show",
            "--run",
            run_id
        )),
        type = "message"
    )
    expect_equal(rendered_show$status, 0L)
    expect_true(any(grepl("Shift workflow run", rendered)))

    # Remove only this test's exported file. The persisted plan and cached
    # scientific artifacts must be sufficient to reconstruct the delivery.
    unlink(output)
    repaired <- epwshiftr_cli(c(base, "resume", "--batch", id))
    expect_equal(repaired$status, 0L, info = repaired$error)
    expect_identical(repaired$result$status, "completed")
    expect_true(all(file.exists(repaired$result$outputs$export_path)))
    expect_identical(tools::md5sum(output), hash)
})

# vim: fdm=marker :
