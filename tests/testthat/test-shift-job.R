# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("failed standalone steps expose recovery identity and resume in place", {
    skip_if_not_installed("duckdb")

    attempts <- 0L
    file_docs <- esgf_test__file_docs("tas_day.nc")
    testthat::local_mocked_bindings(
        query__collect = function(
            index_node,
            params,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = TRUE,
            dict_check = FALSE,
            progress_callback = NULL
        ) {
            attempts <<- attempts + 1L
            if (attempts == 1L) {
                stop("temporary catalog failure")
            }
            type <- query_param__value(params$type())
            docs <- if (identical(type, "Dataset")) {
                esgf_test__dataset_docs()
            } else {
                file_docs
            }
            fields <- query_param__value(params$fields())
            if (is.null(fields) || identical(fields, "*")) {
                fields <- names(docs)
            }
            params$fields(unique(c(fields, required_fields)))
            response <- esgf_test__response(docs)
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr"
    )
    store_path <- tempfile("shift-resume-stage-store-")
    request <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    failure <- tryCatch(
        shift_collect(request, store = store_path, ui = shift_ui("none")),
        epwshiftr_shift_error = identity
    )

    expect_s3_class(failure, "epwshiftr_shift_error")
    expect_match(failure$run_id, "^run_")
    expect_match(failure$step_id, "^step_")
    expect_identical(
        failure$store,
        normalizePath(store_path, winslash = "/", mustWork = TRUE)
    )
    expect_equal(
        shift_status(shift_run_get(failure$run_id, store = store_path)),
        "failed"
    )

    resumed <- shift_resume(
        failure$run_id,
        store = store_path,
        ui = shift_ui("none")
    )
    expect_s7_class(resumed, ShiftFiles)
    expect_identical(shift_ids(resumed)$run_id, failure$run_id)
    expect_equal(shift_status(shift_run_get(resumed)), "waiting")
})

test_that("foreground interrupts persist one meaningful cancelled state", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-interrupt-store-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-interrupt-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    testthat::local_mocked_bindings(
        shift_resolve__collect_resolved_inputs = function(...) {
            stop(structure(
                list(message = "", call = NULL),
                class = c("interrupt", "condition")
            ))
        },
        .package = "epwshiftr"
    )

    interrupted <- tryCatch(
        shift_run(plan, ui = shift_ui("none")),
        interrupt = function(e) e
    )
    expect_s3_class(interrupted, "epwshiftr_shift_cancelled")
    expect_equal(conditionMessage(interrupted), "Interrupted by user.")

    run <- shift_run_get(interrupted$run_id, store = store_path)
    expect_equal(shift_status(run), "cancelled")
    expect_false(is.na(run@meta$run$completed_at[[1L]]))
    expect_equal(run@meta$run$last_error[[1L]], "Interrupted by user.")
    logs <- shift_logs(run)
    expect_gt(nrow(logs), 0L)
    expect_true(all(logs$source == "event"))
    terminal <- run@meta$events[status %in% c("cancelled", "failed")]
    expect_equal(terminal$status, "cancelled")
    expect_equal(terminal$message, "Interrupted by user.")
})

test_that("background live sidecars carry transient reporter state without events", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-live-ui-store-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-live-ui-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift_job__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    job <- shift_job__job_create(
        store,
        run_id,
        mode = "foreground",
        ui = shift_ui("none", heartbeat = 0)
    )
    initial_events <- nrow(morpher__private_store(store)$read_table(
        "shift_run_event"
    ))
    reporter <- shift_reporter__reporter(
        shift_ui("none", heartbeat = 0),
        store = store,
        run_id = run_id,
        job_id = job$job_id[[1L]]
    )
    reporter$heartbeat(
        "Reading tas",
        details = list(
            stage = "extract_future",
            unit_type = "extraction_plan",
            scenario = "ssp585",
            variable = "tas",
            access_method = "OPeNDAP",
            transfer_state = "waiting"
        ),
        force = TRUE
    )

    live <- shift_job__live_run_get(run_id, store_path)
    expect_s7_class(live, ShiftRun)
    expect_identical(live@meta$ui_state$current_details$variable, "tas")
    expect_identical(
        live@meta$ui_state$current_details$access_method,
        "OPeNDAP"
    )
    expect_equal(
        nrow(morpher__private_store(store)$read_table(
            "shift_run_event"
        )),
        initial_events
    )
})

test_that("successful-run scientific diagnostics survive refresh", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-scientific-diagnostic-store-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6("Model-A", "ssp585"),
        periods = list(`2050` = 2050L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-scientific-diagnostic-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift_job__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    diagnostic <- shift_stage__diagnostic(
        "plan",
        "warning",
        "dry_baseline_precip",
        "Baseline month contains no wet hours.",
        variable_id = "pr",
        epw_field = "liquid_precip_depth",
        period = "2050",
        month = 6L,
        action = "Keep the dry baseline month."
    )

    shift_job__run_diagnostics_record(store, run_id, diagnostic)
    shift_job__run_diagnostics_record(store, run_id, diagnostic)
    refreshed <- shift_job__run_handle(store, run_id)
    actual <- shift_diagnostics(refreshed, refresh = FALSE)

    expect_equal(nrow(actual), 1L)
    expect_identical(actual$code, "dry_baseline_precip")
    expect_identical(actual$severity, "warning")
    expect_identical(actual$variable_id, "pr")
    expect_equal(
        nrow(refreshed@meta$events[status == "diagnostic"]),
        1L
    )
})

test_that("background runs register live jobs before launching workers", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-background-store-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-background-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    launched <- new.env(parent = emptyenv())
    test_local_dependencies(list(shift_job__launch_job = function(
        store_path,
        run_id,
        job_id,
        log_path
    ) {
        launched$args <- list(
            store_path = store_path,
            run_id = run_id,
            job_id = job_id,
            log_path = log_path
        )
        invisible(0L)
    }))
    run <- shift_run(
        plan,
        background = TRUE,
        ui = shift_ui("none", motion = "reduced", refresh = 0.25, heartbeat = 7)
    )
    expect_equal(shift_status(run), "queued")
    expect_equal(launched$args$run_id, shift_ids(run)$run_id)
    expect_true(startsWith(
        launched$args$log_path,
        normalizePath(store_path, winslash = "/")
    ))
    expect_equal(run@meta$jobs$mode, "process")
    expect_equal(run@meta$jobs$status, "queued")
    ui_spec <- jsonlite::fromJSON(run@meta$jobs$ui_json[[1L]])
    expect_identical(ui_spec$motion, "reduced")
    expect_equal(ui_spec$refresh, 0.25)
    expect_equal(ui_spec$heartbeat, 7)
    expect_equal(nrow(shift_logs(run)), 0L)

    cancelled <- shift_cancel(run)
    expect_equal(shift_status(cancelled), "cancelled")
    expect_equal(cancelled@meta$jobs$status, "cancelled")
})

test_that("live sidecars keep background handles readable while DuckDB is locked", {
    skip_if_not_installed("duckdb")
    skip_on_os("windows")

    store_path <- tempfile("shift-live-lock-store-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-live-lock-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    test_local_dependencies(list(shift_job__launch_job = function(...) {
        invisible(0L)
    }))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))

    ready <- tempfile("shift-live-lock-ready-")
    child_code <- paste(
        "library(duckdb)",
        "args <- commandArgs(TRUE)",
        "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
        "file.create(args[[2L]])",
        "Sys.sleep(2)",
        "dbDisconnect(conn, shutdown = TRUE)",
        sep = "; "
    )
    system2(
        file.path(R.home("bin"), "Rscript"),
        c(
            "-e",
            shQuote(child_code),
            shQuote(file.path(store_path, "manifest.duckdb")),
            shQuote(ready)
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
    expect_equal(shift_status(run), "queued")

    cancelled <- shift_cancel(run)
    expect_equal(shift_status(cancelled), "stopping")
    expect_true(file.exists(shift_job__live_path(
        store_path,
        shift_ids(run, refresh = FALSE)$run_id,
        "cancel.json"
    )))
})

# Replay the Windows CI sharing violation through the public reader on every
# platform. Permission and corruption failures must never return a live snapshot.
test_that("run readers distinguish Windows sharing violations from IO errors", {
    fixture <- shared_inputs_test__fixture()
    plan <- fixture$batch@meta$children[[1L]]
    test_local_dependencies(list(shift_job__launch_job = function(...) {
        invisible(0L)
    }))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))
    run_id <- shift_ids(run, refresh = FALSE)$run_id
    failure <- simpleError(paste(
        'IO Error: Cannot open file "C:\\store\\manifest.duckdb":',
        "The process cannot access the file because it is being used by another process.",
        "\n\nFile is already open in \nC:\\R\\bin\\x64\\Rscript.exe (PID 6724)"
    ))
    # Force the manifest-open path, as when a batch child has no active process
    # job of its own. A standalone queued job otherwise uses its startup grace.
    local_mocked_bindings(
        shift_job__live_process_is_active = function(...) FALSE,
        shift_store = function(...) stop(failure)
    )
    restored <- shift_run_get(run_id, store = run@store_path)
    expect_s7_class(restored, ShiftRun)
    expect_identical(restored@ids$run_id, run_id)
    expect_identical(restored@meta$run$status, "queued")
    expect_error(
        shift_run_get("missing-run", store = run@store_path),
        "File is already open in"
    )

    # Keep both real Windows variants: current CI omits the owner paragraph,
    # while other builds include it after a potentially localized system error.
    for (message in c(
        "IO Error: Cannot open file 'manifest.duckdb': The process cannot access the file because it is being used by another process.",
        "IO Error: Cannot open file 'manifest.duckdb': localized system error\nFile is already open in Rscript.exe (PID 6724)",
        "IO Error: Could not set lock on file 'manifest.duckdb': Conflicting lock is held"
    )) {
        failure <- simpleError(message)
        expect_identical(
            shift_run_get(run_id, store = run@store_path)@ids$run_id,
            run_id
        )
    }
    for (message in c(
        "IO Error: Cannot open file 'manifest.duckdb': Permission denied",
        "IO Error: Cannot open file 'manifest.duckdb': No such file or directory",
        "IO Error: The file exists, but it is not a valid DuckDB database file"
    )) {
        failure <- simpleError(message)
        expect_identical(
            tryCatch(
                shift_run_get(run_id, store = run@store_path),
                error = identity
            ),
            failure
        )
    }
})

test_that("background workers retry transient DuckDB launch locks", {
    skip_if_not_installed("duckdb")
    skip_on_os("windows")

    store_path <- tempfile("shift-worker-open-store-")
    store <- EsgStore$new(store_path)
    store$close()
    handshake <- withr::local_tempdir(pattern = "shift-worker-open-")
    ready <- file.path(handshake, "ready")
    release <- file.path(handshake, "release")
    stdout <- file.path(handshake, "stdout.log")
    stderr <- file.path(handshake, "stderr.log")
    # Keep the real connection locked until the owning test has observed a
    # retryable launch error; process startup has its own bounded deadline.
    child <- callr::r_bg(
        function(database, ready, release) {
            conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = database)
            on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
            stopifnot(file.create(ready))
            deadline <- Sys.time() + 30
            while (!file.exists(release)) {
                if (Sys.time() >= deadline) {
                    stop("Timed out waiting for the parent to release the lock")
                }
                Sys.sleep(0.01)
            }
            Sys.sleep(0.5)
            TRUE
        },
        args = list(
            database = file.path(store_path, "manifest.duckdb"),
            ready = ready,
            release = release
        ),
        stdout = stdout,
        stderr = stderr,
        supervise = TRUE
    )
    # Only this test's supervised child is eligible for forced cleanup.
    on.exit(
        {
            if (child$is_alive()) {
                child$kill_tree()
            }
            child$wait(timeout = 1000)
        },
        add = TRUE
    )
    # Include both native stderr and the structured R error in failed setup.
    child_diagnostics <- function() {
        failure <- if (child$is_alive()) {
            "Child is still running"
        } else {
            tryCatch(
                paste("Child result:", child$get_result()),
                error = function(error) conditionMessage(error)
            )
        }
        logs <- unlist(lapply(c(stdout, stderr), function(path) {
            if (file.exists(path)) {
                readLines(path, warn = FALSE)
            } else {
                character()
            }
        }))
        paste(c(failure, logs), collapse = "\n")
    }
    deadline <- Sys.time() + 30
    while (!file.exists(ready) && child$is_alive() && Sys.time() < deadline) {
        Sys.sleep(0.05)
    }
    expect_true(file.exists(ready), info = child_diagnostics())
    if (!file.exists(ready)) {
        stop(child_diagnostics(), call. = FALSE)
    }

    locked_attempts <- 0L
    manifest_locked <- shift_job__manifest_locked
    # Observe genuine lock errors while preserving the production classification.
    testthat::local_mocked_bindings(
        shift_job__manifest_locked = function(error) {
            locked <- manifest_locked(error)
            if (locked) {
                locked_attempts <<- locked_attempts + 1L
                # Start the original half-second hold only after the production
                # opener has encountered the child's actual DuckDB lock.
                if (locked_attempts == 1L) {
                    stopifnot(file.create(release))
                }
            }
            locked
        },
        .package = "epwshiftr"
    )
    # This call represents the detached worker starting while a short-lived
    # status reader still owns the manifest.
    worker_store <- shift_job__job_store_open(
        store_path,
        timeout = 3,
        interval = 0.05
    )
    on.exit(worker_store$close(), add = TRUE)
    expect_true(inherits(worker_store, "EsgStore"))
    expect_gt(locked_attempts, 0L)
    child$wait(timeout = 5000)
    expect_false(child$is_alive(), info = child_diagnostics())
    if (!child$is_alive()) {
        expect_true(child$get_result())
    }
})

# Exercise persisted updates against DuckDB, including SQL quoting and NULLs,
# so an unrelated run cannot be changed by a lifecycle state transition.
test_that("run updates preserve other runs and explicit nullable fields", {
    store <- shift_store(tempfile("run-update-"), create = TRUE)
    on.exit(store$close(), add = TRUE)
    first <- shift_job__task_run_register(store, "collect")
    second <- shift_job__task_run_register(store, "collect")
    before <- shift_runs(store)
    shift_job__run_update(
        store,
        first,
        status = "failed",
        last_error = "can't read 'tas'"
    )
    after <- shift_runs(store)
    expect_identical(
        after[after$run_id == second],
        before[before$run_id == second]
    )
    expect_identical(
        after$last_error[after$run_id == first],
        "can't read 'tas'"
    )
    shift_job__run_update(store, first, last_error = NA_character_)
    selected <- shift_inspect__runs(store, first)
    expect_identical(selected$run_id, first)
    expect_true(is.na(selected$last_error))
    expect_identical(nrow(shift_inspect__runs(store, "missing")), 0L)
    shift_job__run_update(store, first, started_at = as.POSIXct(NA, tz = "UTC"))
    expect_identical(shift_runs(store)$run_id, c(second, first))
})

# vim: fdm=marker :
