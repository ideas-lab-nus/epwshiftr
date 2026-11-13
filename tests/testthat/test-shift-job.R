# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("failed standalone steps expose recovery identity and resume in place", {
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
    skip_on_cran()
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
    skip_on_cran()
    skip_on_os("windows")

    store_path <- tempfile("shift-worker-open-store-")
    store <- EsgStore$new(store_path)
    store$close()
    ready <- tempfile("shift-worker-open-ready-")
    child_code <- paste(
        "library(duckdb)",
        "args <- commandArgs(TRUE)",
        "conn <- dbConnect(duckdb(), dbdir = args[[1L]])",
        "file.create(args[[2L]])",
        "Sys.sleep(0.5)",
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

    # This call represents the detached worker starting while a short-lived
    # status reader still owns the manifest.
    worker_store <- shift_job__job_store_open(
        store_path,
        timeout = 3,
        interval = 0.05
    )
    on.exit(worker_store$close(), add = TRUE)
    expect_true(inherits(worker_store, "EsgStore"))
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

# Recreate the on-disk evidence left by a process that exited without releasing
# its lock. A real short-lived R child provides a PID known to have exited.
shift_download_test__exited_owner <- function(directory) {
    code <- sprintf(
        'jsonlite::write_json(list(pid=Sys.getpid(), hostname=unname(Sys.info()[["nodename"]])), %s, auto_unbox=TRUE)',
        encodeString(file.path(directory, 'owner.json'), quote = '"')
    )
    status <- system2(
        file.path(R.home('bin'), 'Rscript'),
        c('--vanilla', '-e', shQuote(code)),
        stdout = FALSE,
        stderr = FALSE
    )
    stopifnot(status == 0L)
    jsonlite::read_json(file.path(directory, 'owner.json'))
}

# Foreground staged operations persist a PID just like detached operations.
# Reopening a run must reconcile a dead owner before public resume tests status.
test_that('foreground owner death is reconciled without touching live or remote runs', {
    root <- withr::local_tempdir()
    owner <- shift_download_test__exited_owner(root)
    for (kind in c('dead', 'live', 'remote')) {
        path <- file.path(root, kind)
        store <- shift_store(path, create = TRUE)
        run_id <- shift_job__task_run_register(
            store,
            'collect',
            spec = list(),
            status = 'queued'
        )
        shift_job__run_update(store, run_id, status = 'running')
        job <- shift_job__job_create(
            store,
            run_id,
            mode = 'foreground',
            ui = shift_ui('none')
        )
        shift_job__job_update(
            store,
            job$job_id,
            pid = if (kind == 'live') Sys.getpid() else owner$pid,
            hostname = if (kind == 'remote') {
                paste0(owner$hostname, '-remote')
            } else {
                owner$hostname
            }
        )
        store$close()
        run <- shift_run_get(run_id, store = path)
        expect_identical(
            shift_status(run, refresh = FALSE),
            if (kind == 'dead') 'failed' else 'running'
        )
        if (kind == 'dead') {
            expect_match(run@meta$run$last_error, 'PID .* is not running')
        }
    }
})

# Resume decodes JSON without automatic vector simplification. Preserve bounded
# time and multi-valued scientific identities when reconstructing a request.
test_that('saved staged requests restore vectors before public validation', {
    request <- shift_request(
        project = 'CMIP6',
        source = c('Model-A', 'Model-B'),
        experiment = c('ssp245', 'ssp585'),
        variant = c('r1i1p1f1', 'r2i1p1f1'),
        variables = c('tas', 'psl'),
        frequency = c('mon', 'day'),
        time = c(2041L, 2060L)
    )
    ref <- jsonlite::fromJSON(
        shift_persist__spec_json(shift_persist__stage_ref(request)),
        simplifyVector = FALSE
    )
    restored <- shift_persist__stage_from_ref(ref)
    expect_identical(restored@meta, request@meta)
    empty <- shift_request()
    restored <- shift_persist__stage_from_ref(jsonlite::fromJSON(
        shift_persist__spec_json(shift_persist__stage_ref(empty)),
        simplifyVector = FALSE
    ))
    expect_identical(restored@meta, empty@meta)
})

# Kill only the fixture-owned R process tree after a real GET has written its
# prefix. The new process must request a Range for the retained file's length.
test_that("shift_resume reopens a stopped process's original download session", {
    skip_on_cran()
    skip_if_not_installed("webfakes")
    root <- withr::local_tempdir()
    bytes <- downloader_http_bytes(32768L)
    app <- webfakes::new_app()
    app$locals$bytes <- bytes
    app$locals$offset <- NULL
    app$get("/offset", function(req, res, locals) res$send_json(locals$offset))
    app$head("/source.bin", function(req, res, locals) {
        range <- req$get_header("Range")
        start <- as.numeric(gsub("[^0-9]", "", range))
        end <- length(locals$bytes) - 1L
        res$set_status(206L)$set_header(
            "Content-Range",
            sprintf("bytes %.0f-%d/%d", start, end, end + 1L)
        )$send(raw())
    })
    app$get("/source.bin", function(req, res, locals) {
        range <- req$get_header("Range")
        if (is.null(range)) {
            res$send_chunk(locals$bytes[1:8192])
            Sys.sleep(4)
            return(invisible(NULL))
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
    expect_identical(readBin(partial, "raw", n = size), head(bytes, size))
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
    expect_identical(shift_ids(reopened)$session_id, sessions$session_id)
    store <- EsgStore$new(store_path)
    downloader <- store$downloader(n_workers = 0L)
    expect_identical(downloader$sessions()$session_id, sessions$session_id)
    actual <- downloader$tasks(session_id = sessions$session_id)
    expect_identical(actual$task_id, tasks$task_id)
    expect_identical(actual$status, "done")
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
    offset <- curl::curl_fetch_memory(server$url("/offset"))$content
    expect_equal(jsonlite::fromJSON(rawToChar(offset)), size)
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

# vim: fdm=marker :
