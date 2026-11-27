# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

job_test__plan <- function(
    store_path,
    climate = shift_cmip6(
        "EC-Earth3",
        "ssp585",
        member = "r1i1p1f1",
        grid = "gr",
        frequency = "mon",
        table = "Amon"
    ),
    periods = list(`2060s` = 2060L)
) {
    shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = climate,
        periods = periods,
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-job-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
}

job_test__spawn_lock <- function(
    db_path,
    sleep = NULL,
    env = parent.frame()
) {
    ready <- tempfile("shift-lock-ready-")
    if (is.null(sleep)) {
        release <- tempfile("shift-lock-release-")
        done <- tempfile("shift-lock-done-")
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
                shQuote(db_path),
                shQuote(ready),
                shQuote(release),
                shQuote(done)
            ),
            wait = FALSE,
            stdout = FALSE,
            stderr = FALSE
        )
        withr::defer(
            {
                file.create(release)
                for (i in seq_len(100L)) {
                    if (file.exists(done)) {
                        break
                    }
                    Sys.sleep(0.02)
                }
                unlink(c(ready, release, done))
            },
            envir = env
        )
    } else {
        child_code <- paste(
            "library(duckdb)",
            "args <- commandArgs(TRUE)",
            "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
            "file.create(args[[2L]])",
            sprintf("Sys.sleep(%s)", sleep),
            "dbDisconnect(conn, shutdown = TRUE)",
            sep = "; "
        )
        system2(
            file.path(R.home("bin"), "Rscript"),
            c("-e", shQuote(child_code), shQuote(db_path), shQuote(ready)),
            wait = FALSE,
            stdout = FALSE,
            stderr = FALSE
        )
        withr::defer(unlink(ready), envir = env)
    }
    for (i in seq_len(500L)) {
        if (file.exists(ready)) {
            break
        }
        Sys.sleep(0.02)
    }
    expect_true(file.exists(ready))
}

test_that("failed standalone steps expose recovery identity and resume in place", {
    store_path <- tempfile("shift-resume-stage-store-")
    request <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    failure <- testthat::with_mocked_bindings(
        tryCatch(
            shift_collect(request, store = store_path, ui = shift_ui("none")),
            epwshiftr_shift_error = identity
        ),
        query__collect = function(...) stop("temporary catalog failure"),
        .package = "epwshiftr"
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

    shift_test__mock_collect(esgf_test__file_docs("tas_day.nc"))
    resumed <- shift_resume(
        failure$run_id,
        store = store_path,
        ui = shift_ui("none")
    )
    expect_s7_class(resumed, ShiftFiles)
    expect_identical(shift_inspect__ids(resumed)$run_id, failure$run_id)
    expect_equal(shift_status(shift_run_get(resumed)), "waiting")
})

test_that("foreground interrupts persist one meaningful cancelled state", {
    plan <- job_test__plan(tempfile("shift-interrupt-store-"))
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
    plan <- job_test__plan(tempfile("shift-live-ui-store-"))
    store_path <- plan@store_path
    run_id <- shift_job__run_register(plan)
    store <- shift_inspect__store(plan)
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
        nrow(morpher__private_store(store)$read_table("shift_run_event")),
        initial_events
    )
})

test_that("durable and live run reads agree on recorded diagnostics", {
    plan <- job_test__plan(
        tempfile("shift-scientific-diagnostic-store-"),
        climate = shift_cmip6("Model-A", "ssp585"),
        periods = list(`2050` = 2050L)
    )
    store_path <- plan@store_path
    run_id <- shift_job__run_register(plan)
    store <- shift_inspect__store(plan)
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
    refreshed <- shift_run_get(run_id, store = store)
    actual <- shift_diagnostics(refreshed, refresh = FALSE)

    expect_identical(actual$code, "dry_baseline_precip")
    expect_identical(actual$severity, "warning")
    expect_identical(actual$variable_id, "pr")
    expect_equal(nrow(refreshed@meta$events[status == "diagnostic"]), 1L)

    shift_job__run_event(store, run_id, "extract", "running", "Working")
    shift_job__run_event(store, run_id, "extract", "failed", "Read failed")
    durable <- shift_diagnostics(
        shift_run_get(run_id, store = store),
        refresh = FALSE
    )
    expect_identical(durable$code, c("dry_baseline_precip", "shift_run_error"))

    # Force the public reader down its lock fallback using a real live snapshot.
    shift_job__live_snapshot(store, run_id, urgent = TRUE)
    local_mocked_bindings(shift_inspect__store = function(...) {
        stop("IO Error: Could not set lock on file 'manifest.duckdb'.")
    })
    live <- shift_run_get(run_id, store = store_path)
    expect_true(live@meta$live)
    expect_identical(shift_diagnostics(live, refresh = FALSE), durable)
})

test_that("background runs register live jobs before launching workers", {
    plan <- job_test__plan(tempfile("shift-background-store-"))
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
    expect_equal(launched$args$run_id, shift_inspect__ids(run)$run_id)
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
    skip_on_cran()
    skip_on_os("windows")

    plan <- job_test__plan(tempfile("shift-live-lock-store-"))
    store_path <- plan@store_path
    test_local_dependencies(list(shift_job__launch_job = function(...) {
        invisible(0L)
    }))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))

    job_test__spawn_lock(file.path(store_path, "manifest.duckdb"))
    expect_equal(shift_status(run), "queued")

    cancelled <- shift_cancel(run)
    expect_equal(shift_status(cancelled), "stopping")
    expect_true(file.exists(shift_job__live_path(
        store_path,
        shift_inspect__ids(run, refresh = FALSE)$run_id,
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
    run_id <- shift_inspect__ids(run, refresh = FALSE)$run_id
    failure <- simpleError(paste(
        'IO Error: Cannot open file "C:\\store\\manifest.duckdb":',
        "The process cannot access the file because it is being used by another process.",
        "\n\nFile is already open in \nC:\\R\\bin\\x64\\Rscript.exe (PID 6724)"
    ))
    # Force the manifest-open path, as when a batch child has no active process
    # job of its own. A standalone queued job otherwise uses its startup grace.
    local_mocked_bindings(
        shift_job__live_process_is_active = function(...) FALSE,
        shift_inspect__store = function(...) stop(failure)
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
    skip_on_cran()
    skip_on_os("windows")

    store_path <- tempfile("shift-worker-open-store-")
    store <- EsgStore$new(store_path)
    store$close()
    job_test__spawn_lock(file.path(store_path, "manifest.duckdb"), sleep = 0.25)

    # This call represents the detached worker starting while a short-lived
    # status reader still owns the manifest.
    worker_store <- shift_job__job_store_open(
        store_path,
        timeout = 3,
        interval = 0.02
    )
    on.exit(worker_store$close(), add = TRUE)
    expect_true(inherits(worker_store, "EsgStore"))
})

# Exercise persisted updates against DuckDB, including SQL quoting and NULLs,
# so an unrelated run cannot be changed by a lifecycle state transition.
test_that("run updates preserve other runs and explicit nullable fields", {
    store <- shift_inspect__store(tempfile("run-update-"), create = TRUE)
    on.exit(store$close(), add = TRUE)
    first <- shift_job__task_run_register(store, "collect")
    second <- shift_job__task_run_register(store, "collect")
    before <- shift_inspect__runs(store)
    shift_job__run_update(
        store,
        first,
        status = "failed",
        last_error = "can't read 'tas'"
    )
    after <- shift_inspect__runs(store)
    expect_identical(
        after[after$run_id == second],
        before[before$run_id == second]
    )
    expect_identical(
        after$last_error[after$run_id == first],
        "can't read 'tas'"
    )
    shift_job__run_update(store, first, last_error = NA_character_)
    cleared <- shift_inspect__runs(store)
    expect_true(is.na(cleared$last_error[cleared$run_id == first]))
})

# Recreate the on-disk evidence left by a process that exited without releasing
# its lock. A real short-lived R child provides a PID known to have exited.
shift_download_test__exited_owner <- function(directory) {
    pid_path <- file.path(directory, "owner.pid")
    code <- sprintf(
        "cat(Sys.getpid(), file = %s)",
        encodeString(pid_path, quote = '"')
    )
    status <- system2(
        file.path(R.home("bin"), "Rscript"),
        c("--vanilla", "-e", shQuote(code)),
        stdout = FALSE,
        stderr = FALSE
    )
    stopifnot(status == 0L)
    list(
        pid = as.integer(readLines(pid_path, warn = FALSE)[[1L]]),
        hostname = unname(Sys.info()[["nodename"]])
    )
}

# Foreground staged operations persist a PID just like detached operations.
# Reopening a run must reconcile a dead owner before public resume tests status.
test_that("foreground owner death is reconciled without touching live or remote runs", {
    root <- withr::local_tempdir()
    owner <- shift_download_test__exited_owner(root)
    for (kind in c("dead", "live", "remote")) {
        path <- file.path(root, kind)
        store <- shift_inspect__store(path, create = TRUE)
        run_id <- shift_job__task_run_register(
            store,
            "collect",
            spec = list(),
            status = "queued"
        )
        shift_job__run_update(store, run_id, status = "running")
        job <- shift_job__job_create(
            store,
            run_id,
            mode = "foreground",
            ui = shift_ui("none")
        )
        shift_job__job_update(
            store,
            job$job_id,
            pid = if (kind == "live") Sys.getpid() else owner$pid,
            hostname = if (kind == "remote") {
                paste0(owner$hostname, "-remote")
            } else {
                owner$hostname
            }
        )
        store$close()
        run <- shift_run_get(run_id, store = path)
        expect_identical(
            shift_status(run, refresh = FALSE),
            if (kind == "dead") "failed" else "running"
        )
        if (kind == "dead") {
            expect_match(run@meta$run$last_error, "PID .* is not running")
        }
    }
})

# Resume decodes JSON without automatic vector simplification. Preserve bounded
# time and multi-valued scientific identities when reconstructing a request.
test_that("saved staged requests restore vectors before public validation", {
    roundtrip <- function(req) {
        ref <- jsonlite::fromJSON(
            shift_persist__spec_json(shift_persist__stage_ref(req)),
            simplifyVector = FALSE
        )
        shift_persist__stage_from_ref(ref)
    }
    request <- shift_request(
        project = "CMIP6",
        source = c("Model-A", "Model-B"),
        experiment = c("ssp245", "ssp585"),
        variant = c("r1i1p1f1", "r2i1p1f1"),
        variables = c("tas", "psl"),
        frequency = c("mon", "day"),
        time = c(2041L, 2060L)
    )
    expect_identical(roundtrip(request)@meta, request@meta)
    empty <- shift_request()
    expect_identical(roundtrip(empty)@meta, empty@meta)
})

# Kill only the fixture-owned R process tree after a real GET has written its
# uncheckpointed prefix. Restart keeps its IDs and history, then obtains a
# complete representation from zero rather than certifying the hard-kill tail.
test_that("shift_resume reopens a stopped process's original download session", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    root <- withr::local_tempdir()
    bytes <- downloader_http_bytes(32768L)
    app <- webfakes::new_app()
    app$locals$bytes <- bytes
    app$locals$offset <- NULL
    app$locals$gets <- 0L
    app$locals$requests <- character()
    app$get("/offset", function(req, res, locals) {
        res$send_json(locals$offset, null = "null", auto_unbox = TRUE)
    })
    app$get("/requests", function(req, res, locals) {
        res$send_json(locals$requests)
    })
    app$head("/source.bin", function(req, res, locals) {
        start <- as.numeric(gsub("[^0-9]", "", req$get_header("Range")))
        end <- length(locals$bytes) - 1L
        res$set_status(206L)$set_header(
            "Content-Range",
            sprintf("bytes %.0f-%d/%d", start, end, end + 1L)
        )$send(raw())
    })
    app$get("/source.bin", function(req, res, locals) {
        range <- req$get_header("Range")
        locals$gets <- locals$gets + 1L
        locals$requests <- c(
            locals$requests,
            if (is.null(range)) "full" else range
        )
        if (is.null(range)) {
            if (locals$gets == 1L) {
                res$send_chunk(locals$bytes[1:8192])
                Sys.sleep(4)
                return(invisible(NULL))
            }
            return(res$set_header("Content-Length", length(locals$bytes))$send(
                locals$bytes
            ))
        }
        start <- as.numeric(gsub("[^0-9]", "", range))
        end <- length(locals$bytes) - 1L
        locals$offset <- start
        res$set_status(206L)$set_header(
            "Content-Range",
            sprintf("bytes %.0f-%d/%d", start, end, end + 1L)
        )$send(locals$bytes[seq.int(start + 1L, end + 1L)])
    })
    server <- webfakes::new_app_process(app)
    on.exit(server$stop(), add = TRUE)
    docs <- esgf_test__file_docs(
        "source.bin",
        download_url = server$url("/source.bin")
    )
    data.table::set(docs, j = "size", value = length(bytes))
    data.table::set(
        docs,
        j = "checksum",
        value = checksum_bytes(bytes, "sha256")
    )
    store <- EsgStore$new(file.path(root, "store"))
    query <- store$add_files(esgf_test__file_result(docs))
    store_path <- store$path
    temp <- priv(store)$tmp_download_dir
    store$close()
    child <- file.path(root, "child.R")
    writeLines(
        c(
            "library(epwshiftr)",
            "args <- commandArgs(TRUE)",
            "ns <- asNamespace('epwshiftr')",
            "files <- ns$shift_stage__new(ns$ShiftFiles, 'files', store_path=args[[1L]], ids=list(query_id=args[[2L]]), meta=list(variables='tas'))",
            "shift_download(files, ui=shift_ui('none'))"
        ),
        child
    )
    process <- processx::process$new(
        file.path(R.home("bin"), "Rscript"),
        c("--vanilla", child, store_path, query),
        env = c(
            R_LIBS_USER = paste(
                shift_execution__library_paths(),
                collapse = .Platform$path.sep
            )
        ),
        stdout = file.path(root, "stdout.log"),
        stderr = file.path(root, "stderr.log"),
        cleanup_tree = TRUE
    )
    on.exit(if (process$is_alive()) process$kill_tree(), add = TRUE)
    deadline <- Sys.time() + 20
    repeat {
        partial <- list.files(
            temp,
            pattern = "[.]part$",
            all.files = TRUE,
            full.names = TRUE
        )
        size <- file.info(partial)$size
        if (length(size) == 1L && is.finite(size) && size > 0L) {
            break
        }
        if (!process$is_alive() || Sys.time() >= deadline) {
            stop(
                "Fixture never wrote a partial: ",
                paste(readLines(file.path(root, "stderr.log")), collapse = "\n")
            )
        }
        Sys.sleep(.02)
    }
    expect_true(process$is_alive())
    process$kill_tree()
    process$wait(timeout = 5000)
    expect_false(process$is_alive())
    size <- file.info(partial)$size
    expect_gt(size, 0L)
    expect_lt(size, length(bytes))
    prefix <- readBin(partial, "raw", n = size)
    expect_identical(prefix, head(bytes, size))
    store <- EsgStore$new(store_path)
    on.exit(store$close(), add = TRUE)
    downloader <- store$downloader(n_workers = 0L)
    sessions <- downloader$sessions()
    expect_equal(nrow(sessions), 1L)
    tasks <- downloader$tasks(session_id = sessions$session_id)
    expect_equal(nrow(tasks), 1L)
    expect_identical(basename(partial), paste0(tasks$task_id, ".part"))
    run_id <- store$query(
        "SELECT run_id FROM shift_run WHERE task = 'download'"
    )$run_id
    expect_length(run_id, 1L)
    store$close()
    reopened <- shift_resume(run_id, store = store_path, ui = shift_ui("none"))
    expect_identical(
        shift_inspect__ids(reopened)$session_id,
        sessions$session_id
    )
    store <- EsgStore$new(store_path)
    downloader <- store$downloader(n_workers = 0L)
    expect_identical(downloader$sessions()$session_id, sessions$session_id)
    actual <- downloader$tasks(session_id = sessions$session_id)
    expect_identical(actual$task_id, tasks$task_id)
    expect_identical(actual$status, "done", info = actual$last_error)
    expect_equal(
        nrow(store$query("SELECT * FROM artifact WHERE role='download'")),
        1L
    )
    expect_identical(actual$selected_url, server$url("/source.bin"))
    expect_identical(
        checksum_file(actual$target_path),
        checksum_bytes(bytes, "sha256")
    )
    expect_false(file.exists(partial))
    retained <- list.files(temp, "[.]part[.]retained-", full.names = TRUE)
    expect_length(retained, 1L)
    expect_identical(readBin(retained, "raw", n = size), prefix)
    offset <- curl::curl_fetch_memory(server$url("/offset"))$content
    expect_null(jsonlite::fromJSON(rawToChar(offset)))
    requests <- jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url("/requests"))$content
    ))
    expect_identical(requests, c("full", "full"))
})

# A stale checkpoint must not silently create a replacement session.
test_that("a missing saved session reports the original session ID", {
    store <- EsgStore$new(withr::local_tempdir())
    on.exit(store$close(), add = TRUE)
    files <- shift_stage__new(
        ShiftFiles,
        "files",
        store_path = store$path,
        ids = list(
            query_id = "query-fixture",
            session_id = "missing-fixture-session"
        )
    )
    failure <- tryCatch(
        shift_download(files, ui = shift_ui("none")),
        epwshiftr_shift_error = identity
    )
    expect_s3_class(failure, "epwshiftr_shift_error")
    expect_match(conditionMessage(failure), "missing-fixture-session")
    expect_identical(
        shift_status(shift_run_get(failure$run_id, store = store$path)),
        "failed"
    )
    expect_equal(nrow(store$downloader(n_workers = 0L)$sessions()), 0L)
})

shift_job_test__file_env <- environment()
shift_job_test__http_server <- local({
    server <- NULL
    function() {
        skip_if_not_installed("webfakes")
        if (is.null(server)) {
            server <<- local_downloader_http_server(shift_job_test__file_env)
        }
        server
    }
})

# Prepare a real local download while leaving the workflow's background step
# running, as it would be immediately after the Downloader process exits.
shift_job_test__finished_background_download <- function(env = parent.frame()) {
    skip_on_cran()
    server <- shift_job_test__http_server()
    bytes <- downloader_http_bytes()
    docs <- esgf_test__file_docs(
        "source.bin",
        download_url = server$url("/files/ok.bin")
    )
    data.table::set(docs, j = "size", value = length(bytes))
    hash <- checksum_bytes(bytes, "sha256")
    data.table::set(docs, j = "checksum", value = hash)
    store <- shift_inspect__store(
        file.path(withr::local_tempdir(.local_envir = env), "store"),
        create = TRUE
    )
    on.exit(store$close(), add = TRUE)
    path <- store$path
    query <- store$add_files(esgf_test__file_result(docs))
    files <- shift_stage__new(
        ShiftFiles,
        "files",
        store_path = path,
        ids = list(query_id = query),
        meta = list(variables = "tas")
    )
    downloader <- store$downloader(n_workers = 0L)
    stage <- shift_download(
        files,
        downloader = downloader,
        run = FALSE,
        background = TRUE,
        ui = shift_ui("none")
    )
    ids <- shift_inspect__ids(stage)
    store$close()

    # Complete the queued session directly in the Downloader's own manifest.
    # This models an exited worker without involving process startup or polling.
    task <- downloader$run(session_id = ids$session_id, progress = FALSE)
    expect_identical(task$status, "done")
    expect_identical(checksum_file(task$target_path), hash)
    list(
        path = path,
        ids = ids,
        downloader = downloader,
        task_id = task$task_id
    )
}

# Use a real local transfer. Inject the interruption exactly after the
# completed step write and before the matching run status write.
test_that("an interrupted background download publication reopens the original result", {
    fixture <- shift_job_test__finished_background_download()
    ids <- fixture$ids
    path <- fixture$path
    write_snapshot <- shift_job__live_snapshot_write
    snapshots <- 0L

    update_row <- shift_job__update_row
    interrupted <- FALSE
    testthat::local_mocked_bindings(
        shift_job__update_row = function(store, table, key, row, fields) {
            if (
                !interrupted &&
                    identical(table, "shift_run") &&
                    identical(row$status, "waiting")
            ) {
                interrupted <<- TRUE
                stop("fixture interruption before run update", call. = FALSE)
            }
            update_row(store, table, key, row, fields)
        },
        shift_job__live_snapshot_write = function(store, run_id, ...) {
            snapshots <<- snapshots + 1L
            write_snapshot(store, run_id, ...)
        },
        .package = "epwshiftr"
    )
    expect_error(
        shift_run_get(ids$run_id, store = path),
        "fixture interruption before run update"
    )
    expect_true(interrupted)
    expect_identical(snapshots, 0L)
    store <- shift_inspect__store(path)
    on.exit(store$close(), add = TRUE)
    expect_identical(
        shift_job__latest_step(store, ids$run_id)$status,
        "running"
    )
    store$close()
    reopened <- shift_run_get(ids$run_id, store = path)
    expect_identical(shift_status(reopened, refresh = FALSE), "waiting")
    expect_identical(snapshots, 1L)
    expect_equal(
        sum(
            reopened@meta$events$message %in%
                "Background download completed; ready for the next stage."
        ),
        1L
    )
    expect_error(
        shift_resume(ids$run_id, store = path, ui = shift_ui("none")),
        "waiting for its next stage"
    )
    expect_identical(
        shift_inspect__ids(shift_inspect__result(reopened))$session_id,
        ids$session_id
    )
    expect_identical(fixture$downloader$sessions()$session_id, ids$session_id)
    expect_identical(
        fixture$downloader$tasks(session_id = ids$session_id)$task_id,
        fixture$task_id
    )
})

# Emulate a previously interrupted store after the old step write committed.
# A completed Downloader session and a terminal workflow owner justify repair.
test_that("reopening a legacy download commit repairs the waiting run once", {
    fixture <- shift_job_test__finished_background_download()
    ids <- fixture$ids
    store <- shift_inspect__store(fixture$path)
    on.exit(store$close(), add = TRUE)
    shift_job__step_finish(store, ids$step_id, "completed")
    step <- shift_job__latest_step(store, ids$run_id)
    expect_identical(step$status, "completed")
    store$close()

    run <- shift_run_get(ids$run_id, store = fixture$path)
    expect_identical(shift_status(run, refresh = FALSE), "waiting")
    expect_identical(
        shift_inspect__ids(shift_inspect__result(run))$session_id,
        ids$session_id
    )
    again <- shift_run_get(ids$run_id, store = fixture$path)
    expect_equal(
        sum(
            again@meta$events$message %in%
                "Background download completed; ready for the next stage."
        ),
        1L
    )
    store <- shift_inspect__store(fixture$path)
    expect_identical(
        shift_job__latest_step(store, ids$run_id)$completed_at,
        step$completed_at
    )
})

# The live snapshot is a durable read model of a run. Reading it through one
# connection must produce the same payload as reading every table through
# store$query().
# job_test__live_snapshot_run {{{
job_test__live_snapshot_run <- function(env = parent.frame()) {
    plan <- job_test__plan(tempfile("shift-snapshot-store-"))
    run_id <- shift_job__run_register(plan)
    store <- shift_inspect__store(plan)
    withr::defer(store$close(), envir = env)
    shift_job__job_create(
        store,
        run_id,
        mode = "foreground",
        ui = shift_ui("none", heartbeat = 0)
    )
    for (i in 1:3) {
        shift_job__run_event(
            store,
            run_id,
            stage = "extract",
            status = "running",
            message = paste("event", i),
            snapshot = FALSE
        )
    }
    list(store = store, run_id = run_id)
}
# }}}

# Rebuild the payload with the earlier per-table store$query() reads.
# job_test__live_snapshot_oracle {{{
job_test__live_snapshot_oracle <- function(store, run_id, event_limit) {
    by <- function(table, key, id) {
        store$query(sprintf(
            "SELECT * FROM %s WHERE %s IN (%s)",
            table,
            key,
            shift_stage__query_ids(id)
        ))
    }
    run <- by("shift_run", "run_id", run_id)
    events <- store$query(sprintf(
        paste(
            "SELECT * FROM (SELECT * FROM shift_run_event WHERE run_id IN (%s)",
            "ORDER BY created_at DESC LIMIT %d) ORDER BY created_at"
        ),
        shift_stage__query_ids(run_id),
        as.integer(event_limit)
    ))
    jobs <- by("shift_run_job", "run_id", run_id)
    steps <- by("shift_run_step", "run_id", run_id)
    list(
        run = run,
        cases = by("shift_run_case", "run_id", run_id),
        events = events,
        jobs = jobs[order(jobs[["attempt"]])],
        steps = steps[order(steps[["ordinal"]])]
    )
}
# }}}

# Coalescing: bursts of changes inside one interval write once, while urgent
# changes always write, and a deferred write is delivered later.
# job_test__live_snapshot_coalesce {{{
job_test__live_snapshot_coalesce <- function(
    interval = 3600,
    record = TRUE,
    env = parent.frame()
) {
    withr::defer(
        rm(
            list = ls(shift_job__snapshot_state, all.names = TRUE),
            envir = shift_job__snapshot_state
        ),
        envir = env
    )
    interval_fn <- if (is.function(interval)) interval else function() interval
    bindings <- list(shift_job__snapshot_interval = interval_fn)
    writes <- NULL
    if (isTRUE(record)) {
        writes <- new.env(parent = emptyenv())
        writes$ui <- list()
        bindings$shift_job__live_snapshot_write <- function(
            store,
            run_id,
            event_limit = 200L,
            ui_state = NULL
        ) {
            # Wrap the value: assigning NULL into a list element is a no-op, so
            # a bare NULL ui_state would leave the call unrecorded.
            writes$ui[[length(writes$ui) + 1L]] <- list(ui_state)
            invisible(NULL)
        }
    }
    do.call(
        local_mocked_bindings,
        c(bindings, list(.package = "epwshiftr", .env = env))
    )
    writes
}
# }}}

test_that("live snapshot payload matches per-table reads", {
    fixture <- job_test__live_snapshot_run()
    store <- fixture$store
    run_id <- fixture$run_id

    for (limit in c(200L, 2L, 0L)) {
        expected <- job_test__live_snapshot_oracle(store, run_id, limit)
        payload <- shift_job__live_snapshot_write(
            store,
            run_id,
            event_limit = limit
        )
        for (field in names(expected)) {
            expect_identical(payload[[field]], expected[[field]], info = field)
        }
        expect_identical(payload$run_id, run_id)
        expect_identical(payload$store_path, store$path)
    }
    # The event tail is bounded by event_limit and keeps the newest rows.
    all_events <- job_test__live_snapshot_oracle(store, run_id, 1000L)$events
    expect_gte(nrow(all_events), 3L)
    tail_events <- shift_job__live_snapshot_write(
        store,
        run_id,
        event_limit = 2L
    )$events
    expect_identical(nrow(tail_events), 2L)
    expect_identical(
        tail_events$event_id,
        tail(all_events$event_id, 2L)
    )
})

test_that("live snapshot of an unknown run writes nothing and a closed store errors", {
    fixture <- job_test__live_snapshot_run()
    store <- fixture$store
    expect_null(shift_job__live_snapshot_write(store, "no-such-run"))
    expect_false(file.exists(shift_job__live_path(store$path, "no-such-run")))

    closed <- EsgStore$new(tempfile("shift-snapshot-closed-"))
    closed$close()
    expect_error(
        shift_job__live_snapshot_write(closed, fixture$run_id),
        "closed"
    )
})

test_that("non-urgent snapshots coalesce inside the interval and urgent ones do not", {
    store <- list(path = tempfile("coalesce-"))
    writes <- job_test__live_snapshot_coalesce()
    key <- "run-coalesce"

    expect_true(shift_job__live_snapshot(store, key))
    # Inside the interval a burst of ordinary changes is deferred.
    for (i in 1:5) {
        expect_false(shift_job__live_snapshot(store, key))
    }
    expect_length(writes$ui, 1L)
    # An urgent change writes immediately, even inside the interval.
    expect_true(shift_job__live_snapshot(store, key, urgent = TRUE))
    expect_length(writes$ui, 2L)
    expect_true(shift_job__live_snapshot(store, key, urgent = TRUE))
    expect_length(writes$ui, 3L)
    # Different runs and different stores never share an interval.
    expect_true(shift_job__live_snapshot(store, "other-run"))
    expect_true(shift_job__live_snapshot(list(path = "elsewhere"), key))
    expect_length(writes$ui, 5L)
})

test_that("deferred snapshots are delivered once the interval has passed", {
    store <- list(path = tempfile("flush-"))
    interval <- 3600
    writes <- job_test__live_snapshot_coalesce(interval = function() interval)
    key <- "run-flush"

    # Nothing is pending before the first write, and an unknown run is a no-op.
    expect_false(shift_job__live_snapshot_flush(store, key))
    shift_job__live_snapshot(store, key)
    expect_false(shift_job__live_snapshot_flush(store, key))
    shift_job__live_snapshot(store, key)
    # Pending, but the interval has not passed yet.
    expect_false(shift_job__live_snapshot_flush(store, key))
    expect_length(writes$ui, 1L)
    # Once the interval passes, flush writes exactly once and clears the flag.
    interval <- 0
    expect_true(shift_job__live_snapshot_flush(store, key))
    expect_length(writes$ui, 2L)
    expect_false(shift_job__live_snapshot_flush(store, key))
    expect_length(writes$ui, 2L)
})

test_that("a deferred snapshot keeps the last reporter state and a failed write stays pending", {
    store <- list(path = tempfile("uistate-"))
    writes <- job_test__live_snapshot_coalesce()
    key <- "run-ui"

    shift_job__live_snapshot(store, key, ui_state = list(unit = "first"))
    # A later change that carries no reporter state keeps the last one.
    shift_job__live_snapshot(store, key, ui_state = list(unit = "second"))
    shift_job__live_snapshot(store, key, urgent = TRUE)
    expect_identical(writes$ui[[1L]][[1L]], list(unit = "first"))
    expect_identical(writes$ui[[2L]][[1L]], list(unit = "second"))

    # A write that fails is not recorded, so the next call tries again.
    attempts <- 0L
    local_mocked_bindings(
        shift_job__live_snapshot_write = function(store, run_id, ...) {
            attempts <<- attempts + 1L
            if (attempts == 1L) {
                stop("disk full")
            }
            invisible(NULL)
        },
        .package = "epwshiftr"
    )
    expect_error(
        shift_job__live_snapshot(store, "run-fails", urgent = TRUE),
        "disk full"
    )
    expect_true(shift_job__live_snapshot(store, "run-fails"))
    expect_identical(attempts, 2L)
})

test_that("resetting a run forgets its interval and its reporter state", {
    store <- list(path = tempfile("reset-"))
    writes <- job_test__live_snapshot_coalesce()
    key <- "run-reset"

    shift_job__live_snapshot(store, key, ui_state = list(unit = "old"))
    expect_false(shift_job__live_snapshot(store, key))
    shift_job__snapshot_reset(store, key)
    # The next write is immediate again and carries no stale reporter state.
    expect_true(shift_job__live_snapshot(store, key))
    expect_null(writes$ui[[2L]][[1L]])
    # Resetting an unknown run is harmless.
    expect_silent(shift_job__snapshot_reset(store, "never-written"))
})

test_that("terminal changes write a snapshot immediately and ordinary ones coalesce", {
    fixture <- job_test__live_snapshot_run()
    store <- fixture$store
    run_id <- fixture$run_id
    job_test__live_snapshot_coalesce(record = FALSE)
    path <- shift_job__live_path(store$path, run_id)
    live_status <- function() {
        shift_job__live_run_get(run_id, store$path)@meta$run$status
    }

    shift_job__run_event(store, run_id, "extract", "running", "go")
    first <- file.mtime(path)
    expect_false(is.na(first))
    # Ordinary events inside the interval leave the sidecar as it was.
    for (i in 1:3) {
        shift_job__run_event(store, run_id, "extract", "running", "again")
    }
    expect_identical(file.mtime(path), first)
    # A terminal run update is written at once and shows the new status.
    shift_job__run_finish(store, run_id, "failed", last_error = "boom")
    expect_identical(live_status(), "failed")
    # The terminal snapshot carries the events deferred before it.
    events <- shift_job__live_run_get(run_id, store$path)@meta$events
    expect_gte(sum(events$message == "again"), 3L)
})

# vim: fdm=marker :
