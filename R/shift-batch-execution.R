# Read coordinator state independently of child DuckDB locks. A dead owner or
# an expired launch is recoverable; a cancellation belongs to one attempt only.
shift_batch__job_read <- function(root) {
    path <- file.path(root, "batch-job.json")
    if (!file.exists(path)) {
        return(NULL)
    }
    job <- jsonlite::read_json(path, simplifyVector = TRUE)
    if (job$status %in% c("queued", "running", "stopping")) {
        alive <- if (is.null(job$pid) || is.na(job$pid)) {
            as.numeric(Sys.time()) - job$started < 60
        } else {
            downloader__pid_alive(job$pid)
        }
        if (!alive) {
            job$status <- "failed"
        } else if (
            file.exists(file.path(root, paste0(job$id, ".cancel.json")))
        ) {
            job$status <- "stopping"
        }
    }
    job
}

# Share the existing reporter's cancellation boundary with the batch owner,
# including the period before any child workflow has been registered.
shift_batch__check_cancel <- function() {
    context <- getOption("epwshiftr.batch.context")
    if (is.null(context)) {
        return(invisible(NULL))
    }
    if (
        file.exists(file.path(context$root, paste0(context$id, ".cancel.json")))
    ) {
        cli::cli_abort(
            "Future-weather batch was cancelled.",
            class = "epwshiftr_shift_cancelled"
        )
    }
    invisible(NULL)
}

# Persist at most one heartbeat per second. Only the coordinator writes state;
# readers and cancellation requests never compete to overwrite its job record.
shift_batch__checkpoint <- function(details = list()) {
    context <- getOption("epwshiftr.batch.context")
    if (is.null(context)) {
        return(invisible(NULL))
    }
    shift_batch__check_cancel()
    now <- as.numeric(Sys.time())
    if (now - context$heartbeat < 1) {
        return(invisible(NULL))
    }
    context$heartbeat <- now
    context$job$heartbeat <- now
    context$job$progress <- details
    store_write_json_atomic(
        context$job,
        file.path(context$root, "batch-job.json")
    )
    invisible(NULL)
}

# Execute the same bounded source pool and sequential child persistence in the
# foreground or one detached coordinator. The option is scoped to this call.
shift_batch__run_execution <- function(x, job, ui) {
    job$status <- "running"
    job$pid <- Sys.getpid()
    context <- new.env(parent = emptyenv())
    context$root <- x@store_path
    context$id <- job$id
    context$job <- job
    context$heartbeat <- -Inf
    old <- options(epwshiftr.batch.context = context)
    on.exit(options(old), add = TRUE)
    path <- file.path(x@store_path, "batch-job.json")
    store_write_json_atomic(job, path)
    reporter <- shift__reporter(ui)
    on.exit(reporter$close(), add = TRUE)
    reporter$operation_started("collect", "Read shared batch sources")
    terminal <- "failed"
    on.exit(
        {
            job <- context$job
            job$status <- terminal
            job$finished <- as.numeric(Sys.time())
            if (identical(job$progress$unit_type, "source_reads")) {
                job$progress$active <- 0L
            }
            store_write_json_atomic(job, path)
        },
        add = TRUE
    )
    result <- tryCatch(
        shift_batch__execute(x, ui, reporter),
        epwshiftr_shift_cancelled = function(error) {
            terminal <<- "cancelled"
            reporter$operation_failed(conditionMessage(error), cancelled = TRUE)
            stop(error)
        },
        interrupt = function(error) {
            terminal <<- "cancelled"
            reporter$operation_failed("Batch interrupted.", cancelled = TRUE)
            stop(error)
        },
        error = function(error) {
            context$job$message <- conditionMessage(error)
            stop(error)
        }
    )
    terminal <- "finished"
    context$job$status <- terminal
    context$job$progress <- NULL
    store_write_json_atomic(context$job, path)
    shift_batch__report(result, ui)
    result
}

# Launch a single R process with the same package and library paths. Source
# workers run inside it, so adding cities never multiplies the worker limit.
shift_batch__launch <- function(root, job) {
    package <- getNamespaceInfo(asNamespace("epwshiftr"), "path")
    load <- if (file.exists(file.path(package, "Meta", "package.rds"))) {
        "library(epwshiftr)"
    } else {
        sprintf(
            "pkgload::load_all(%s, quiet=TRUE)",
            downloader__r_literal(package)
        )
    }
    libraries <- paste(
        vapply(.libPaths(), downloader__r_literal, character(1L)),
        collapse = ","
    )
    expr <- sprintf(
        ".libPaths(c(%s)); %s; epwshiftr:::shift_batch__job_main(%s, %s)",
        libraries,
        load,
        downloader__r_literal(root),
        downloader__r_literal(job$id)
    )
    status <- system2(
        downloader__rscript(),
        c("--vanilla", "-e", shQuote(expr)),
        stdout = file.path(root, paste0(job$id, ".log")),
        stderr = file.path(root, paste0(job$id, ".log")),
        wait = FALSE
    )
    if (!identical(as.integer(status), 0L)) {
        cli::cli_abort("Could not launch the batch coordinator.")
    }
    invisible(NULL)
}

# A coordinator owns the batch execution lock for its whole lifetime. Its ID
# prevents a delayed process from taking over a later explicitly resumed job.
shift_batch__job_main <- function(root, id) {
    manifest_with_lock(
        file.path(root, "batch-execution"),
        {
            job <- shift_batch__job_read(root)
            if (
                is.null(job) ||
                    !identical(job$id, id) ||
                    !job$status %in% c("queued", "stopping")
            ) {
                cli::cli_abort("The batch launch is no longer current.")
            }
            options(job$options)
            ui <- do.call(shift_ui, job$ui)
            x <- shift_batch_get(job$batch_id, store = root)
            shift_batch__run_execution(x, job, ui)
        },
        timeout = 60
    )
}

# Claim an execution attempt before doing catalog work. Both public execution
# modes return the same batch identity and expose pre-read progress/cancellation.
shift_batch__resume <- function(x, background = FALSE, ui = shift_ui()) {
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    workers <- getOption("epwshiftr.mirai_workers", 4L)
    checkmate::assert_count(workers, positive = TRUE)
    job <- shift_batch__job_read(x@store_path)
    if (!is.null(job) && job$status %in% c("queued", "running", "stopping")) {
        return(shift_batch__refresh(x))
    }
    dir.create(x@store_path, recursive = TRUE, showWarnings = FALSE)
    manifest_with_lock(
        file.path(x@store_path, "batch-execution"),
        {
            # Recheck after claiming the lock to close simultaneous-launch races.
            job <- shift_batch__job_read(x@store_path)
            if (
                !is.null(job) &&
                    job$status %in% c("queued", "running", "stopping")
            ) {
                return(shift_batch__refresh(x))
            }
            x <- shift_batch__refresh(x)
            selected_options <- options()[intersect(
                names(options()),
                c(
                    "epwshiftr.cache",
                    "epwshiftr.dir_cache",
                    "epwshiftr.dir_store",
                    "epwshiftr.threshold_alpha"
                )
            )]
            selected_options$epwshiftr.mirai_workers <- workers
            job <- list(
                id = paste0("batch-job-", basename(tempfile())),
                batch_id = x@ids$batch_id,
                status = "queued",
                pid = NA_integer_,
                started = as.numeric(Sys.time()),
                background = background,
                options = selected_options,
                ui = list(
                    progress = if (background && ui@progress != "none") {
                        "log"
                    } else {
                        ui@progress
                    },
                    detail = ui@detail,
                    motion = ui@motion,
                    refresh = ui@refresh,
                    heartbeat = ui@heartbeat
                )
            )
            shift_batch__receipt_write(x)
            path <- file.path(x@store_path, "batch-job.json")
            store_write_json_atomic(job, path)
            if (background) {
                tryCatch(
                    shift_batch__launch(x@store_path, job),
                    error = function(error) {
                        job$status <- "failed"
                        job$message <- conditionMessage(error)
                        store_write_json_atomic(job, path)
                        stop(error)
                    }
                )
                x
            } else {
                shift_batch__run_execution(x, job, ui)
            }
        },
        timeout = 0
    )
}

# Publish the current child's registered run before it acquires long-lived
# store ownership. External batch watchers then use ordinary live snapshots.
shift_batch__register_child <- function(store, run_id) {
    context <- getOption("epwshiftr.batch.context")
    if (is.null(context$child_key)) {
        return(invisible(NULL))
    }
    if (isTRUE(context$job$background)) {
        job <- shift__latest_job(store, run_id)
        shift__job_update(
            store,
            job$job_id[[1L]],
            mode = "process",
            pid = as.integer(Sys.getpid()),
            status = "running",
            log_path = file.path(context$root, paste0(context$id, ".log"))
        )
    }
    child <- shift__run_handle(store, run_id)
    child@meta$shared_inputs <- context$batch@meta$children[[
        context$child_key
    ]]@meta$shared_inputs
    context$batch@meta$children[[context$child_key]] <- child
    shift_batch__receipt_write(context$batch)
    invisible(NULL)
}
