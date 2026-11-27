# Resolve user settings once at an execution boundary. The same snapshot is
# persisted for detached jobs and copied to source workers; runtime objects and
# testing dependencies never enter this configuration.
# shift_execution__options {{{
shift_execution__options <- function() {
    defaults <- list(
        epwshiftr.verbose = FALSE,
        epwshiftr.progress = interactive(),
        epwshiftr.threshold_alpha = 3,
        epwshiftr.cache = TRUE,
        epwshiftr.dir_store = tools::R_user_dir("epwshiftr", "data"),
        epwshiftr.dir_cache = tools::R_user_dir("epwshiftr", "cache"),
        epwshiftr.ui_height = NULL,
        epwshiftr.mirai_workers = 4L,
        epwshiftr.cache_max_size = 1024^3,
        epwshiftr.cache_max_age = 30 * 60,
        epwshiftr.cache_max_n = Inf,
        epwshiftr.query.timeout = 300,
        epwshiftr.query.connect_timeout = 30
    )
    # Read only supported settings; unrelated session options stay outside
    # the persisted configuration, including when a default is NULL.
    defaults[] <- lapply(names(defaults), function(name) {
        getOption(name, defaults[[name]])
    })
    checkmate::assert_count(defaults$epwshiftr.mirai_workers, positive = TRUE)
    for (name in c(
        "epwshiftr.cache_max_size",
        "epwshiftr.cache_max_age",
        "epwshiftr.cache_max_n"
    )) {
        checkmate::assert_number(defaults[[name]], lower = 0)
    }
    defaults
}
# }}}

# Worker processes load the installed package that owns this namespace. A
# source checkout must be installed by the development/test harness first.
# shift_execution__library_paths {{{
shift_execution__library_paths <- function() {
    path <- getNamespaceInfo(asNamespace("epwshiftr"), "path")
    if (!file.exists(file.path(path, "Meta", "package.rds"))) {
        cli::cli_abort(c(
            "Worker execution requires an installed epwshiftr package.",
            "i" = "Install the current source into a test library before running workers."
        ))
    }
    unique(c(dirname(path), .libPaths()))
}
# }}}

# Quote only scalar strings accepted by background entry points. Reject other
# types and lengths instead of silently coercing or discarding arguments.
# shift_execution__string_literal {{{
shift_execution__string_literal <- function(x) {
    checkmate::assert_string(x, null.ok = TRUE)
    if (is.null(x)) {
        return("NULL")
    }
    encodeString(x, quote = '"')
}
# }}}

# Start one detached entry point through the same installed library and quoting
# rules for standalone Shift workflows and batches. Downloader owns its launcher.
# shift_execution__launch {{{
shift_execution__launch <- function(entry, args, log_path) {
    libraries <- paste(
        vapply(
            shift_execution__library_paths(),
            shift_execution__string_literal,
            character(1L)
        ),
        collapse = ","
    )
    arguments <- paste(
        paste0(
            names(args),
            " = ",
            vapply(args, shift_execution__string_literal, character(1L))
        ),
        collapse = ","
    )
    expr <- sprintf(
        ".libPaths(c(%s)); library(epwshiftr); epwshiftr:::%s(%s)",
        libraries,
        entry,
        arguments
    )
    status <- system2(
        downloader__rscript(),
        c("--vanilla", "-e", shQuote(expr)),
        stdout = log_path,
        stderr = log_path,
        wait = FALSE
    )
    if (!identical(as.integer(status), 0L)) {
        cli::cli_abort("Could not launch the background execution process.")
    }
    invisible(status)
}
# }}}

# Bind a durable attempt to an explicit, call-owned runtime context. Child
# contexts refer to their batch owner without modifying session-global state.
# shift_execution__context {{{
shift_execution__context <- function(root, job, store = NULL, parent = NULL) {
    context <- new.env(parent = emptyenv())
    context$root <- root
    context$store <- store
    context$parent <- parent
    context$job <- job
    context$batch <- is.null(store)
    context$id <- if (context$batch) job$id else job$job_id[[1L]]
    context$heartbeat <- -Inf
    context$options <- if (context$batch) {
        job$options
    } else {
        jsonlite::fromJSON(job$ui_json[[1L]], simplifyVector = TRUE)$execution
    }
    for (name in c(
        "epwshiftr.cache_max_size",
        "epwshiftr.cache_max_age",
        "epwshiftr.cache_max_n"
    )) {
        if (is.null(context$options[[name]])) context$options[[name]] <- Inf
    }
    context
}
# }}}

# Keep storage-specific representation at one boundary. Batch receipts stay
# readable while child DuckDB stores are busy; both use the same lifecycle.
# shift_execution__update {{{
shift_execution__update <- function(context, status, message = NULL) {
    now <- store__now()
    terminal <- status %in% c("completed", "partial", "cancelled", "failed")
    if (context$batch) {
        context$job$status <- if (status == "completed") "finished" else status
        context$job$pid <- Sys.getpid()
        context$job$heartbeat <- as.numeric(now)
        if (terminal) {
            context$job$finished <- as.numeric(now)
            context$job$progress <- NULL
        }
        if (!is.null(message)) {
            context$job$message <- message
        }
        shift_batch_execution__job_write(context$root, context$job)
    } else {
        values <- list(
            status = status,
            pid = as.integer(Sys.getpid()),
            heartbeat_at = now
        )
        if (terminal) {
            values$completed_at <- now
            values$exit_code <- switch(
                status,
                failed = 1L,
                cancelled = 130L,
                0L
            )
            values$last_error <- if (is.null(message)) {
                NA_character_
            } else {
                message
            }
        } else {
            values$started_at <- now
        }
        do.call(
            shift_job__job_update,
            c(list(context$store, context$id), values)
        )
    }
    invisible(NULL)
}
# }}}

# Observe both the task and its owning batch at cooperative cancellation
# boundaries, including shared reads before any child run is registered.
# shift_execution__check_cancel {{{
shift_execution__check_cancel <- function(context, stage = "working") {
    if (is.null(context)) {
        return(invisible(NULL))
    }
    shift_execution__check_cancel(context$parent, stage)
    if (context$batch) {
        if (
            file.exists(file.path(
                context$root,
                paste0(context$id, ".cancel.json")
            ))
        ) {
            cli::cli_abort(
                "Future-weather batch was cancelled.",
                class = "epwshiftr_shift_cancelled"
            )
        }
    } else {
        shift_job__job_check_cancel(
            context$store,
            context$job$run_id[[1L]],
            context$id,
            stage
        )
    }
    invisible(NULL)
}
# }}}

# Batch source progress is written at most once per second; ordinary run
# snapshots remain owned by the reporter attached to their DuckDB store.
# shift_execution__checkpoint {{{
shift_execution__checkpoint <- function(context, details = list()) {
    if (is.null(context)) {
        return(invisible(NULL))
    }
    shift_execution__checkpoint(context$parent, details)
    if (!context$batch) {
        return(invisible(NULL))
    }
    shift_execution__check_cancel(context)
    now <- as.numeric(Sys.time())
    if (now - context$heartbeat < 1) {
        return(invisible(NULL))
    }
    context$heartbeat <- now
    context$job$heartbeat <- now
    context$job$progress <- details
    shift_batch_execution__job_write(context$root, context$job)
    invisible(NULL)
}
# }}}

# Own attempt transitions and option restoration for every execution mode.
# Scientific run results remain separate from the process attempt status.
# shift_execution__run {{{
shift_execution__run <- function(context, code) {
    old <- options(context$options)
    on.exit(options(old), add = TRUE)
    shift_execution__update(context, "running")
    value <- tryCatch(
        {
            # Standalone stages check cancellation inside their own scientific
            # result handler so the step and run receive the same terminal state.
            if (context$batch) {
                shift_execution__check_cancel(context)
            }
            force(code)
        },
        interrupt = function(error) {
            try(
                shift_execution__update(
                    context,
                    "cancelled",
                    conditionMessage(error)
                ),
                silent = TRUE
            )
            stop(error)
        },
        error = function(error) {
            status <- if (inherits(error, "epwshiftr_shift_cancelled")) {
                "cancelled"
            } else {
                "failed"
            }
            # Preserve the original error if its persistence also fails.
            try(
                shift_execution__update(
                    context,
                    status,
                    conditionMessage(error)
                ),
                silent = TRUE
            )
            stop(error)
        }
    )
    outcome <- if (
        S7::S7_inherits(value, ShiftRun) &&
            identical(value@meta$run$status[[1L]], "partial")
    ) {
        "partial"
    } else {
        "completed"
    }
    shift_execution__update(context, outcome)
    if (S7::S7_inherits(value, ShiftRun)) {
        # Return the completed attempt, including for refresh = FALSE callers.
        jobs <- morpher__private_store(context$store)$read_table(
            "shift_run_job"
        )
        jobs <- jobs[jobs[["run_id"]] == value@ids$run_id]
        value@meta$jobs <- jobs[order(jobs[["attempt"]])]
    }
    value
}
# }}}

# Read coordinator state independently of child DuckDB locks. A dead owner or
# an expired launch is recoverable; a cancellation belongs to one attempt only.
# shift_batch_execution__job_read {{{
shift_batch_execution__job_read <- function(root) {
    path <- file.path(root, "batch-job.json")
    # Share a short receipt lock with every publisher. In particular, Windows
    # copy-overwrite fallback must finish before any reader opens the JSON file.
    job <- shift_batch__locked_read(
        root,
        path,
        function(file) jsonlite::read_json(file, simplifyVector = TRUE),
        timeout = 5
    )
    if (is.null(job)) {
        return(NULL)
    }
    if (job$status %in% SHIFT_BATCH_ACTIVE_STATUSES) {
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
# }}}

# Serialize receipt publication with readers, independently of the long-lived
# execution lock. Check both publication operations so an unwritable receipt
# cannot be reported as a successful state transition.
# shift_batch_execution__job_write {{{
shift_batch_execution__job_write <- function(root, job) {
    path <- file.path(root, "batch-job.json")
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    manifest_with_lock(
        path,
        {
            tmp <- tempfile(pattern = "batch-job-", tmpdir = root)
            on.exit(unlink(tmp, force = TRUE), add = TRUE)
            jsonlite::write_json(job, tmp)
            # Renaming is preferred. The copy fallback is safe for readers only
            # because they acquire the same lock before checking or opening it.
            if (!suppressWarnings(file.rename(tmp, path))) {
                if (!file.copy(tmp, path, overwrite = TRUE)) {
                    cli::cli_abort(
                        "Could not publish batch receipt: {.path {path}}."
                    )
                }
            }
            path
        },
        timeout = 5
    )
}
# }}}

# Run a batch with the same attempt lifecycle used by a standalone plan.
# shift_batch_execution__run_execution {{{
shift_batch_execution__run_execution <- function(x, job, ui) {
    context <- shift_execution__context(x@store_path, job)
    reporter <- shift_reporter__reporter(ui, execution = context)
    on.exit(reporter$close(), add = TRUE)
    result <- shift_execution__run(context, {
        reporter$operation_started("collect", "Read shared batch sources")
        shift_batch__execute(x, ui, reporter, context)
    })
    shift_batch_ui__report(result, ui)
    result
}
# }}}

# Launch a single R process with the same package and library paths. Source
# workers run inside it, so adding cities never multiplies the worker limit.
# shift_batch_execution__launch {{{
shift_batch_execution__launch <- function(root, job) {
    shift_execution__launch(
        "shift_batch_execution__job_main",
        list(root = root, id = job$id),
        file.path(root, paste0(job$id, ".log"))
    )
}
# }}}

# A coordinator owns the batch execution lock for its whole lifetime. Its ID
# prevents a delayed process from taking over a later explicitly resumed job.
# shift_batch_execution__job_main {{{
shift_batch_execution__job_main <- function(root, id) {
    manifest_with_lock(
        file.path(root, "batch-execution"),
        {
            job <- shift_batch_execution__job_read(root)
            if (
                is.null(job) ||
                    !identical(job$id, id) ||
                    !job$status %in% c("queued", "stopping")
            ) {
                cli::cli_abort("The batch launch is no longer current.")
            }
            ui <- do.call(shift_ui, job$ui)
            x <- shift_batch_get(job$batch_id, store = root)
            shift_batch_execution__run_execution(x, job, ui)
        },
        timeout = 60
    )
}
# }}}

# Claim an execution attempt before doing catalog work. Both public execution
# modes return the same batch identity and expose pre-read progress/cancellation.
# shift_batch_execution__resume {{{
shift_batch_execution__resume <- function(
    x,
    background = FALSE,
    ui = shift_ui()
) {
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    selected_options <- shift_execution__options()
    if (!is.null(shift_batch__active_job(x@store_path))) {
        return(shift_batch__refresh(x))
    }
    dir.create(x@store_path, recursive = TRUE, showWarnings = FALSE)
    manifest_with_lock(
        file.path(x@store_path, "batch-execution"),
        {
            # Recheck after claiming the lock to close simultaneous-launch races.
            if (!is.null(shift_batch__active_job(x@store_path))) {
                return(shift_batch__refresh(x))
            }
            x <- shift_batch__refresh(x)
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
            shift_batch_execution__job_write(x@store_path, job)
            if (background) {
                tryCatch(
                    shift_batch_execution__launch(x@store_path, job),
                    error = function(error) {
                        job$status <- "failed"
                        job$message <- conditionMessage(error)
                        shift_batch_execution__job_write(x@store_path, job)
                        stop(error)
                    }
                )
                x
            } else {
                shift_batch_execution__run_execution(x, job, ui)
            }
        },
        timeout = 0
    )
}
# }}}

# Publish the current child's registered run before it acquires long-lived
# store ownership. External batch watchers then use ordinary live snapshots.
# shift_batch_execution__register_child {{{
shift_batch_execution__register_child <- function(
    store,
    run_id,
    context = NULL
) {
    if (is.null(context$child_key)) {
        return(invisible(NULL))
    }
    if (isTRUE(context$job$background)) {
        job <- shift_job__latest_job(store, run_id)
        shift_job__job_update(
            store,
            job$job_id[[1L]],
            mode = "process",
            pid = as.integer(Sys.getpid()),
            status = "running",
            log_path = file.path(context$root, paste0(context$id, ".log"))
        )
    }
    child <- shift_job__run_handle(store, run_id)
    child@meta$shared_inputs <- context$owner@meta$children[[
        context$child_key
    ]]@meta$shared_inputs
    context$owner@meta$children[[context$child_key]] <- child
    shift_batch__receipt_write(context$owner)
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
