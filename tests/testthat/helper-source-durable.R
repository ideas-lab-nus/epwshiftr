# Start a real installed-package R process with only explicit on-disk inputs.
# The helper is sourced in the child; no test environment or Store is serialized.
durable_test__launch <- function(cases, cache, mode = "offline", wait = TRUE) {
    input <- tempfile("durable-input-", fileext = ".rds")
    output <- tempfile("durable-output-", fileext = ".rds")
    config <- list(
        cases = cases,
        cache = cache,
        mode = mode,
        libraries = .libPaths(),
        helper = normalizePath(testthat::test_path("helper-source-durable.R")),
        parent_pid = Sys.getpid(),
        output = output
    )
    saveRDS(config, input)
    script <- paste(
        "cfg <- readRDS(commandArgs(TRUE)[[1L]])",
        ".libPaths(cfg$libraries)",
        "source(cfg$helper)",
        "durable_test__worker(cfg)",
        sep = "; "
    )
    stdout <- tempfile("durable-stdout-")
    stderr <- tempfile("durable-stderr-")
    process <- processx::process$new(
        file.path(R.home("bin"), "Rscript"),
        c("--vanilla", "-e", script, input),
        stdout = stdout,
        stderr = stderr,
        cleanup_tree = TRUE,
        env = c(
            "current",
            R_LIBS_USER = paste(.libPaths(), collapse = .Platform$path.sep)
        )
    )
    state <- list(
        process = process,
        input = input,
        output = output,
        stdout = stdout,
        stderr = stderr
    )
    if (!wait) {
        return(state)
    }
    durable_test__collect(state)
}

# Bound child execution at the OS-process level, including its owned daemons.
# An incomplete or killed invocation never supplies a successful readback result.
durable_test__collect <- function(state, timeout = 90) {
    on.exit(if (state$process$is_alive()) state$process$kill_tree(), add = TRUE)
    state$process$wait(timeout * 1000)
    stdout <- readLines(state$stdout, warn = FALSE)
    stderr <- readLines(state$stderr, warn = FALSE)
    if (
        state$process$is_alive() ||
            !identical(state$process$get_exit_status(), 0L)
    ) {
        stop(paste("Fresh R readback failed:", stdout, stderr), call. = FALSE)
    }
    result <- readRDS(state$output)
    # Optional external evidence stores exact process inputs and readback outputs.
    evidence <- Sys.getenv("EPWSHIFTR_DURABLE_EVIDENCE", "")
    if (nzchar(evidence)) {
        dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
        label <- basename(state$input)
        file.copy(state$input, file.path(evidence, label))
        file.copy(
            state$output,
            file.path(evidence, sub("input", "output", label))
        )
        writeLines(
            c(stdout, stderr),
            file.path(evidence, paste0(label, ".log"))
        )
    }
    result
}

# Consume only public Store and pool interfaces in the new process. Namespace
# qualification keeps this callback independent of any inherited test closure.
durable_test__worker <- function(config) {
    options(
        epwshiftr.cache = config$mode,
        epwshiftr.dir_cache = config$cache,
        epwshiftr.progress = FALSE
    )
    answers <- vector("list", length(config$cases))
    for (index in seq_along(config$cases)) {
        case <- config$cases[[index]]
        store <- epwshiftr::EsgStore$new(case$path, create = FALSE)
        pool <- if (identical(case$mode, "pooled")) {
            epwshiftr::source_pool(1L, timeout = 15)
        } else {
            NULL
        }
        case$readers <- if (is.null(pool)) {
            integer()
        } else {
            vapply(pool$status(), `[[`, integer(1L), "pid")
        }
        # Each invocation owns its handles; close them even after a rejected plan.
        answers[[index]] <- tryCatch(
            {
                result <- store$extract(
                    plan_id = case$plans,
                    pool = pool,
                    fallback = if (is.null(case$fallback)) {
                        "auto"
                    } else {
                        case$fallback
                    },
                    reporter = if (is.null(case$boundary)) {
                        NULL
                    } else {
                        durable_test__reporter(case)
                    }
                )
                result <- durable_test__drain(pool, result)
                conflict <- NULL
                replacement <- NULL
                if (isTRUE(case$options)) {
                    conflict <- tryCatch(
                        {
                            value <- store$extract(
                                plan_id = case$plans,
                                resume = FALSE,
                                pool = pool
                            )
                            durable_test__drain(pool, value)
                            NULL
                        },
                        error = function(error) class(error)
                    )
                    value <- store$extract(
                        plan_id = case$plans,
                        resume = FALSE,
                        overwrite = TRUE,
                        pool = pool
                    )
                    replacement <- durable_test__drain(pool, value)
                }
                list(
                    result = result,
                    conflict = conflict,
                    replacement = replacement,
                    snapshot = durable_test__snapshot(store)
                )
            },
            finally = {
                if (!is.null(pool)) {
                    pool$close(force = TRUE)
                }
                store$close()
            }
        )
    }
    saveRDS(
        list(
            pid = Sys.getpid(),
            parent_pid = ps::ps_ppid(ps::ps_handle()),
            library = find.package("epwshiftr"),
            answers = answers
        ),
        config$output
    )
}

# Poll persistent tickets under a deadline without altering reader timeouts.
durable_test__drain <- function(pool, result) {
    if (is.data.frame(result)) {
        return(result)
    }
    deadline <- as.double(Sys.time()) + 45
    while (identical(result$status, "pending")) {
        pool$poll()
        if (as.double(Sys.time()) > deadline) {
            stop("Persistent extraction exceeded deadline")
        }
        Sys.sleep(0.01)
    }
    if (!identical(result$status, "done")) {
        stop(result$error)
    }
    result$value
}

# Read registered outputs and their acquisition records through public readback.
# MD5 here is an independent byte-preservation check; saved receipts retain SHA256.
durable_test__snapshot <- function(store) {
    artifacts <- store$query(
        "SELECT * FROM artifact WHERE kind = 'extract' ORDER BY artifact_id"
    )
    results <- store$query("SELECT * FROM extraction_result ORDER BY plan_id")
    paths <- vapply(artifacts$artifact_id, store$artifact_path, character(1L))
    references <- list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )
    values <- lapply(results$output_path, function(path) {
        store$query(paste0(
            "SELECT value FROM read_parquet('",
            file.path(store$path, path),
            "') ORDER BY cf_year,cf_month,cf_day,cf_second_of_day"
        ))$value
    })
    list(
        artifacts = artifacts,
        results = results,
        values = values,
        output_md5 = unname(tools::md5sum(paths)),
        reference_md5 = unname(tools::md5sum(references)),
        references = lapply(references, jsonlite::read_json)
    )
}

# Prepare an ordinary or pooled producer through real local HTTP/NetCDF input.
# Two query identities keep the healthy source independently addressable.
durable_test__producer <- function(root, mode, servers, paths, acquire = TRUE) {
    store <- EsgStore$new(root)
    plans <- character(length(paths))
    for (index in seq_along(paths)) {
        files <- recovery_test__files(
            paths[[index]],
            servers[[index]],
            record = paste0("durable-", index)
        )
        query <- store$add_files(files)
        plan <- store$plan_region(
            query,
            104 + (index - 1L) * 0.5,
            1.37,
            c("2060-01-01", "2060-01-03")
        )
        plans[[index]] <- plan$plan_id
    }
    if (!acquire) {
        return(list(store = store, plans = plans, mode = mode))
    }
    pool <- if (identical(mode, "pooled")) {
        source_pool(1L, timeout = 15)
    } else {
        NULL
    }
    on.exit(if (!is.null(pool)) pool$close(force = TRUE), add = TRUE)
    result <- durable_test__drain(
        pool,
        store$extract(plan_id = plans, pool = pool)
    )
    stopifnot(all(result$status == "done"))
    list(store = store, plans = plans, mode = mode)
}

# Remove every acquired native file, leaving only saved output/cache evidence.
durable_test__remove_native <- function(store) {
    references <- list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )
    for (path in references) {
        unlink(jsonlite::read_json(path)$cache_path)
    }
    invisible(NULL)
}

# Pause at the existing public reporter boundary, after acquisition registration
# or after result commit. Only filesystem paths enter this owner-only callback;
# the supervising process performs the actual OS termination independently.
durable_test__reporter <- function(case) {
    state <- new.env(parent = baseenv())
    state$path <- case$path
    state$marker <- case$marker
    state$release <- case$release
    state$boundary <- case$boundary
    state$readers <- case$readers
    gate <- function(point) {
        if (!identical(point, boundary)) {
            return(invisible(NULL))
        }
        references <- list.files(
            file.path(path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )
        outputs <- list.files(
            file.path(path, "extracts"),
            "[.]parquet$",
            full.names = TRUE,
            recursive = TRUE
        )
        stopifnot(length(references) == 1L)
        saveRDS(
            list(
                pid = Sys.getpid(),
                boundary = point,
                readers = readers,
                references = references,
                reference_md5 = unname(tools::md5sum(references)),
                outputs = outputs,
                output_md5 = unname(tools::md5sum(outputs))
            ),
            marker
        )
        deadline <- as.double(Sys.time()) + 45
        while (!file.exists(release)) {
            if (as.double(Sys.time()) > deadline) {
                stop("External reporter release exceeded deadline")
            }
            Sys.sleep(0.02)
        }
        invisible(NULL)
    }
    environment(gate) <- state
    callbacks <- new.env(parent = baseenv())
    callbacks$gate <- gate
    started <- function(...) gate("source")
    completed <- function(...) gate("committed")
    environment(started) <- environment(completed) <- callbacks
    noop <- function(...) invisible(NULL)
    environment(noop) <- baseenv()
    ui <- function() epwshiftr::shift_ui(progress = "none")
    environment(ui) <- baseenv()
    list(
        unit_started = started,
        unit_completed = completed,
        unit_updated = noop,
        heartbeat = noop,
        check_cancel = noop,
        detail = noop,
        notice = noop,
        ui = ui
    )
}

# Exercise either public consumption mode with unchanged reader policies.
durable_test__consume <- function(case, ...) {
    pool <- if (identical(case$mode, "pooled")) {
        source_pool(1L, timeout = 15)
    } else {
        NULL
    }
    on.exit(if (!is.null(pool)) pool$close(force = TRUE), add = TRUE)
    durable_test__drain(
        pool,
        case$store$extract(plan_id = case$plans, pool = pool, ...)
    )
}

# Persist actual requests outside the server process so a stopped server still
# leaves evidence that offline/cache consumers made no additional request.
durable_test__app <- function(bytes, log) {
    app <- webfakes::new_app()
    app$locals$bytes <- bytes
    app$locals$log <- log
    app$all("/dods/source.nc*", function(req, res, locals) {
        cat(req$method, req$path, "\n", file = locals$log, append = TRUE)
        res$set_status(504L)$send("DAP unavailable")
    })
    app$all("/files/source.nc", function(req, res, locals) {
        cat(req$method, req$path, "\n", file = locals$log, append = TRUE)
        res$set_header("Content-Type", "application/x-netcdf")$set_header(
            "Content-Length",
            length(locals$bytes)
        )$set_header("ETag", '"durable-v1"')
        if (req$method == "HEAD") {
            return(res$send(raw()))
        }
        res$send(locals$bytes)
    })
    app
}
environment(durable_test__app) <- baseenv()

# Read only completed append-only request lines; no live server connection is
# needed, and the file bytes can be retained with the independent climate input.
durable_test__requests <- function(logs) {
    lapply(logs, function(path) {
        if (file.exists(path)) readLines(path, warn = FALSE) else character()
    })
}

# Keep compact source, request and process facts when the supervised acceptance
# runner asks for external evidence. Regular package tests remain self-contained.
durable_test__record <- function(label, facts, files = character()) {
    evidence <- Sys.getenv("EPWSHIFTR_DURABLE_EVIDENCE", "")
    if (!nzchar(evidence)) {
        return(invisible(NULL))
    }
    target <- file.path(evidence, label)
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    saveRDS(facts, file.path(target, "facts.rds"))
    for (index in seq_along(files)) {
        if (file.exists(files[[index]])) {
            file.copy(
                files[[index]],
                file.path(target, paste0(index, "-", basename(files[[index]])))
            )
        }
    }
    invisible(NULL)
}

# vim: fdm=marker :
