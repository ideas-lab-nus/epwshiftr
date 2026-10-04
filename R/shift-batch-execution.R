# Read coordinator state independently of child DuckDB locks. A dead owner or
# an expired launch is recoverable; a cancellation belongs to one attempt only.
# shift_batch_execution__job_read {{{
shift_batch_execution__job_read <- function(root) {
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
    job <- shift_batch_execution__job_read(x@store_path)
    if (!is.null(job) && job$status %in% c("queued", "running", "stopping")) {
        return(shift_batch__refresh(x))
    }
    dir.create(x@store_path, recursive = TRUE, showWarnings = FALSE)
    manifest_with_lock(
        file.path(x@store_path, "batch-execution"),
        {
            # Recheck after claiming the lock to close simultaneous-launch races.
            job <- shift_batch_execution__job_read(x@store_path)
            if (
                !is.null(job) &&
                    job$status %in% c("queued", "running", "stopping")
            ) {
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
            path <- file.path(x@store_path, "batch-job.json")
            store_write_json_atomic(job, path)
            if (background) {
                tryCatch(
                    shift_batch_execution__launch(x@store_path, job),
                    # error {{{
                    error = function(error) {
                        job$status <- "failed"
                        job$message <- conditionMessage(error)
                        store_write_json_atomic(job, path)
                        stop(error)
                    }
                    # }}}
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

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
