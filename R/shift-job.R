#' @include shift-stage.R
NULL

# Manage persisted jobs and their background execution, observation and recovery.

# Rebuild one failed, cancelled, or partial standalone step from its immutable
# input and scientific spec. UI choices are supplied by the new attempt and are
# intentionally absent from the persisted step hash.
# shift_job__resume_generic_task {{{
shift_job__resume_generic_task <- function(run, step, ui, background = FALSE) {
    if (!isTRUE(step$resumable[[1L]])) {
        reason <- store__chr1(step$nonresumable_reason[[1L]])
        cli::cli_abort(c(
            "Shift step {.val {step$step_id[[1L]]}} cannot be resumed across sessions.",
            "x" = if (is.na(reason)) {
                "Its original input is session-local."
            } else {
                reason
            }
        ))
    }
    if (
        is.na(step$input_stage_json[[1L]]) ||
            !nzchar(step$input_stage_json[[1L]])
    ) {
        cli::cli_abort(
            "Shift step {.val {step$step_id[[1L]]}} has no reconstructible input stage."
        )
    }
    input_ref <- jsonlite::fromJSON(
        step$input_stage_json[[1L]],
        simplifyVector = FALSE
    )
    input <- shift_persist__stage_from_ref(input_ref)
    spec <- jsonlite::fromJSON(step$spec_json[[1L]], simplifyVector = TRUE)
    task <- as.character(step$task[[1L]])
    if (isTRUE(background) && !identical(task, "download")) {
        cli::cli_abort(
            "Background resume is currently supported only for standalone download steps."
        )
    }

    call <- switch(
        task,
        datasets = list(
            what = shift_datasets,
            args = list(
                input,
                store = run@store_path,
                all = isTRUE(spec$all),
                limit = spec$limit,
                ui = ui
            )
        ),
        collect = list(
            what = shift_collect,
            args = c(
                list(
                    input,
                    store = run@store_path,
                    fields = as.character(spec$fields),
                    all = isTRUE(spec$all),
                    limit = spec$limit,
                    label = store__chr1(spec$label),
                    ui = ui
                ),
                shift_stage__coalesce(spec$options, list())
            )
        ),
        download = list(
            what = shift_download,
            args = c(
                list(
                    input,
                    run = isTRUE(spec$run),
                    background = isTRUE(background) || isTRUE(spec$background),
                    resume = TRUE,
                    overwrite = isTRUE(spec$overwrite),
                    session_label = store__chr1(spec$session_label),
                    ui = ui
                ),
                shift_stage__coalesce(spec$options, list())
            )
        ),
        extract = list(
            what = shift_extract,
            args = list(
                input,
                site = shift_persist__site_from_ref(spec$site),
                periods = shift_spec__periods_from_input(spec$periods),
                variables = if (is.null(spec$variables)) {
                    NULL
                } else {
                    as.character(spec$variables)
                },
                time = spec$time,
                filters = shift_stage__coalesce(spec$filters, list()),
                method = as.character(spec$method),
                fallback = as.character(spec$fallback),
                overwrite = isTRUE(spec$overwrite),
                resume = TRUE,
                ui = ui
            )
        ),
        morph = list(
            what = shift_morph,
            args = list(
                input,
                baseline = if (is.null(spec$baseline)) {
                    NULL
                } else {
                    as.character(spec$baseline)
                },
                transform = transform__from_spec(spec$transform),
                reference = shift_persist__reference_from_spec(spec$reference),
                observed_reference = shift_persist__reference_from_spec(
                    spec$observed_reference
                ),
                strict = isTRUE(spec$strict),
                complete_only = isTRUE(spec$complete_only),
                by = as.character(spec$by),
                overwrite = isTRUE(spec$overwrite),
                resume = TRUE,
                ui = ui
            )
        ),
        write_epw = list(
            what = shift_epw,
            args = list(
                input,
                dir = store__chr1(spec$dir),
                separate = isTRUE(spec$separate),
                export_dir = store__chr1(spec$export_dir),
                overwrite = isTRUE(spec$overwrite),
                resume = TRUE,
                ui = ui
            )
        ),
        export_epw = list(
            what = shift_export_epw,
            args = list(
                input,
                dir = as.character(spec$dir),
                separate = isTRUE(spec$separate),
                overwrite = isTRUE(spec$overwrite),
                resume = TRUE,
                ui = ui
            )
        ),
        cli::cli_abort("Unsupported standalone shift task: {.val {task}}.")
    )
    # Remove JSON nulls restored as NA scalar strings before public validation.
    call$args <- lapply(call$args, function(value) {
        if (is.character(value) && length(value) == 1L && is.na(value)) {
            NULL
        } else {
            value
        }
    })
    shift_run__with_run_override(run@ids$run_id, do.call(call$what, call$args))
}
# }}}

#' @rdname shift_api
#' @export
# shift_resume {{{
shift_resume <- function(x, store = NULL, background = FALSE, ui = shift_ui()) {
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch_execution__resume(
            x,
            background = background,
            ui = ui
        ))
    }
    shift_job__resume_one(x, store, background, ui)
}
# }}}

# Resume one durable task with the same execution context as a fresh task.
# shift_job__resume_one {{{
shift_job__resume_one <- function(
    x,
    store = NULL,
    background = FALSE,
    ui = shift_ui(),
    execution = NULL
) {
    run <- if (S7::S7_inherits(x, ShiftRun)) {
        shift_refresh(x)
    } else if (S7::S7_inherits(x, ShiftStage)) {
        shift_run_get(x, store = store)
    } else {
        checkmate::assert_string(x, min.chars = 1L)
        shift_run_get(x, store = store)
    }
    status <- shift_status(run, refresh = FALSE)
    if (status %in% "completed") {
        return(run)
    }
    if (status %in% c("queued", "running", "stopping")) {
        cli::cli_abort(
            "Shift run {.val {run@ids$run_id}} is already active with status {.val {status}}."
        )
    }
    row <- run@meta$run
    task <- as.character(row$task[[1L]])
    if (identical(status, "waiting")) {
        cli::cli_abort(c(
            "Shift run {.val {run@ids$run_id}} is waiting for its next stage, not interrupted.",
            "i" = "Pass the latest stage object to the next {.fn shift_*} function, or call {.fn shift_complete}."
        ))
    }
    if (!identical(task, "future_epw")) {
        run_store <- shift_store(run)
        on.exit(try(run_store$close(), silent = TRUE), add = TRUE)
        step <- shift_job__latest_step(run_store, run@ids$run_id)
        if (!nrow(step)) {
            cli::cli_abort(
                "Shift run {.val {run@ids$run_id}} has no resumable step."
            )
        }
        if (!isTRUE(step$resumable[[1L]])) {
            reason <- store__chr1(step$nonresumable_reason[[1L]])
            cli::cli_abort(c(
                "Shift step {.val {step$step_id[[1L]]}} cannot be resumed across sessions.",
                "x" = if (is.na(reason)) {
                    "Its original input is session-local."
                } else {
                    reason
                }
            ))
        }
        if (
            is.na(step$input_stage_json[[1L]]) ||
                !nzchar(step$input_stage_json[[1L]])
        ) {
            cli::cli_abort(
                "Shift step {.val {step$step_id[[1L]]}} has no reconstructible input stage."
            )
        }
        shift_job__run_update(
            run_store,
            run@ids$run_id,
            status = "waiting",
            current_stage = step$task[[1L]],
            completed_at = as.POSIXct(NA, tz = "UTC"),
            last_error = NA_character_
        )
        shift_job__run_event(
            run_store,
            run@ids$run_id,
            "resume",
            "waiting",
            sprintf("Resume requested for %s.", step$task[[1L]]),
            details = list(step_id = step$step_id[[1L]]),
            step_id = step$step_id[[1L]]
        )
        refreshed <- shift_job__run_handle(run_store, run@ids$run_id)
        return(tryCatch(
            shift_job__resume_generic_task(
                refreshed,
                step,
                ui = ui,
                background = background
            ),
            error = function(e) {
                latest_run <- shift_job__run_handle(run_store, run@ids$run_id)
                if (
                    identical(
                        shift_status(latest_run, refresh = FALSE),
                        "waiting"
                    )
                ) {
                    shift_job__run_finish(
                        run_store,
                        run@ids$run_id,
                        "failed",
                        current_stage = step$task[[1L]],
                        last_error = conditionMessage(e)
                    )
                }
                stop(e)
            }
        ))
    }
    spec <- jsonlite::fromJSON(row$spec_json[[1L]], simplifyVector = TRUE)
    plan <- shift_persist__plan_from_spec(spec, store = run@store_path)
    if (!is.null(run@meta$shared_inputs)) {
        plan@meta$shared_inputs <- run@meta$shared_inputs
    }
    resolved <- row$resolved_spec_json[[1L]]
    if (!is.na(resolved) && nzchar(resolved)) {
        # Resolved member/grid/node choices are immutable across resume.
        plan@meta$resolved <- jsonlite::fromJSON(
            resolved,
            simplifyVector = TRUE
        )
    }
    if (isTRUE(background)) {
        shift_job__validate_background_plan(plan)
    }
    run_store <- shift_store(run)
    on.exit(try(run_store$close(), silent = TRUE), add = TRUE)
    shift_job__run_event(
        run_store,
        run@ids$run_id,
        "resume",
        "running",
        "Workflow resume requested."
    )
    shift_run__start_plan(
        plan,
        run_store,
        run@ids$run_id,
        background,
        ui,
        execution,
        resume_existing = TRUE
    )
}
# }}}

# Resolve either a ShiftRun handle or a run ID to a fresh persisted snapshot.
# shift_job__as_run {{{
shift_job__as_run <- function(x, store = NULL) {
    if (S7::S7_inherits(x, ShiftRun)) {
        return(shift_refresh(x))
    }
    if (S7::S7_inherits(x, ShiftStage)) {
        return(shift_run_get(x, store = store))
    }
    checkmate::assert_string(x, min.chars = 1L)
    shift_run_get(x, store = store)
}
# }}}

# Isolate watch-loop wall-clock reads so cadence tests can advance a deterministic
# clock without depending on runner speed or covr instrumentation overhead.
# shift_job__watch_now {{{
shift_job__watch_now <- function() {
    Sys.time()
}
# }}}

# Isolate frame waiting for the same deterministic watch-loop tests while the
# production path continues to yield normally between dashboard updates.
# shift_job__watch_sleep {{{
shift_job__watch_sleep <- function(seconds) {
    Sys.sleep(seconds)
}
# }}}

#' @rdname shift_api
#' @param follow Whether to continue watching until the run reaches a terminal
#'   status.
#' @param interval Polling interval in seconds.
#' @param events Number of recent events to display or return.
#' @param ui Runtime presentation options from [shift_ui()].
#' @export
# shift_watch {{{
shift_watch <- function(
    x,
    store = NULL,
    follow = TRUE,
    interval = 1,
    events = 10L,
    ui = shift_ui()
) {
    checkmate::assert_flag(follow)
    checkmate::assert_number(interval, lower = 0.1, finite = TRUE)
    checkmate::assert_count(events, positive = FALSE)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch_ui__watch(x, follow, interval, events, ui))
    }
    run <- shift_job__as_run(x, store = store)
    run_id <- run@ids$run_id
    store_path <- run@store_path
    mode <- shift_ui__ui_mode(ui)
    motion <- shift_ui__ui_motion(ui, mode)
    terminal <- c("waiting", "completed", "partial", "failed", "cancelled")
    renderer <- tryCatch(shift_tui__ui_renderer(mode), error = function(e) NULL)
    if (identical(mode, "dynamic") && is.null(renderer)) {
        mode <- "log"
        motion <- "none"
    }
    frame <- 0L
    last_event_id <- NA_character_
    event_cursor_initialized <- FALSE
    # Keep one atomic framebuffer alive for the same dashboard used by
    # foreground runs; constrained IDE consoles receive its compact form.
    update_dynamic <- function(view) {
        ok <- !is.null(renderer) &&
            isTRUE(renderer$draw(view$lines, compact = view$compact))
        if (!isTRUE(ok)) {
            if (!is.null(renderer)) {
                renderer$close(result = "failed")
            }
            renderer <<- NULL
            mode <<- "log"
            motion <<- "none"
        }
        ok
    }
    close_dynamic <- function(result = "done") {
        if (!is.null(renderer)) {
            renderer$close(result = result)
        }
        invisible(NULL)
    }
    on.exit(close_dynamic(), add = TRUE)
    emit_snapshot <- function(snapshot, initial = FALSE, final = FALSE) {
        view <- shift_ui_view__ui_run_view(
            snapshot,
            width = shift_ui__ui_width(),
            detail = ui@detail,
            motion = motion,
            frame = frame
        )
        if (identical(mode, "dynamic") && !isTRUE(final)) {
            if (!isTRUE(update_dynamic(view))) {
                shift_ui_view__ui_print_view(view, include_tables = FALSE)
            }
        } else if (identical(mode, "dynamic") && isTRUE(final)) {
            close_dynamic(result = "done")
            shift_ui_view__ui_print_view(view, include_tables = TRUE)
        } else if (identical(mode, "log")) {
            delta <- shift_ui_state__ui_event_delta(
                snapshot@meta$events,
                last_event_id = last_event_id,
                initial_limit = events,
                initial = !event_cursor_initialized
            )
            rows <- delta$rows
            if (isTRUE(initial)) {
                shift_ui_view__ui_print_view(view, include_tables = TRUE)
            } else {
                if (isTRUE(delta$gap)) {
                    cli::cli_alert_info(paste(
                        "Some older workflow events are no longer available",
                        "in the live buffer; continuing from its oldest event."
                    ))
                }
                for (i in seq_len(nrow(rows))) {
                    cli::cli_text(
                        "{shift_ui_view__ui_persisted_event_line(rows[i], detail = ui@detail)}"
                    )
                }
            }
            last_event_id <<- delta$cursor
            event_cursor_initialized <<- TRUE
            if (isTRUE(final) && !isTRUE(initial)) {
                shift_ui_view__ui_print_view(view, include_tables = TRUE)
            }
        }
        invisible(snapshot)
    }
    if (!isTRUE(follow)) {
        if (!identical(mode, "none")) {
            shift_ui_view__ui_print_view(
                shift_ui_view__ui_run_view(
                    run,
                    detail = ui@detail,
                    motion = "none"
                ),
                include_tables = TRUE
            )
        }
        return(run)
    }
    tryCatch(
        {
            first <- TRUE
            last_poll <- as.POSIXct(NA)
            frame_interval <- if (identical(motion, "full")) {
                ui@refresh
            } else if (identical(motion, "reduced")) {
                max(1, ui@refresh)
            } else {
                interval
            }
            repeat {
                now <- shift_job__watch_now()
                poll_due <- isTRUE(first) ||
                    is.na(last_poll) ||
                    as.numeric(difftime(now, last_poll, units = "secs")) >=
                        interval
                if (isTRUE(poll_due)) {
                    # Poll durable/live state at the requested interval while the
                    # cached snapshot is animated independently between polls.
                    run <- shift_run_get(run_id, store = store_path)
                    last_poll <- now
                }
                done <- shift_status(run, refresh = FALSE) %in% terminal
                if (isTRUE(poll_due) || identical(mode, "dynamic")) {
                    frame <- frame + 1L
                    emit_snapshot(run, initial = first, final = done)
                }
                first <- FALSE
                if (done) {
                    break
                }
                shift_job__watch_sleep(frame_interval)
            }
        },
        interrupt = function(e) {
            close_dynamic(result = "cancelled")
            if (!identical(mode, "none")) {
                cli::cli_alert_info(
                    "Stopped watching {.val {run_id}}; the workflow continues. Use {.fn shift_cancel} to cancel it."
                )
            }
        }
    )
    shift_run_get(run_id, store = store_path)
}
# }}}

#' @rdname shift_api
#' @param force If `FALSE`, request cancellation at the next safe workflow
#'   boundary. If `TRUE`, persist the request and then terminate the recorded
#'   background worker process immediately. For coordinated batches, send an
#'   interrupt to the owner so it can close its source workers; cooperative
#'   cancellation remains requested if the platform cannot deliver the interrupt.
#' @export
# shift_cancel {{{
shift_cancel <- function(x, store = NULL, force = FALSE) {
    checkmate::assert_flag(force)
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch__cancel(x, force = force))
    }
    run <- shift_job__as_run(x, store = store)
    status <- shift_status(run, refresh = FALSE)
    if (status %in% c("completed", "partial", "failed", "cancelled")) {
        return(run)
    }
    if (status %in% c("running", "stopping")) {
        download_store <- shift_store(run)
        download_context <- shift_job__background_download_context(
            download_store,
            run@ids$run_id
        )
        if (!is.null(download_context)) {
            on.exit(try(download_store$close(), silent = TRUE), add = TRUE)
            downloader_job_id <- if (nrow(download_context$jobs)) {
                as.character(download_context$jobs$job_id[[
                    nrow(download_context$jobs)
                ]])
            } else {
                NA_character_
            }
            # Stop the owning Downloader job first, then mark any queued or
            # active tasks so its session reaches a deterministic terminal
            # state that the shared run reconciler can observe.
            if (!is.na(downloader_job_id) && nzchar(downloader_job_id)) {
                download_context$downloader$stop_job(
                    downloader_job_id,
                    force = force
                )
            }
            download_context$downloader$cancel(
                session_id = download_context$session_id
            )
            shift_job__run_update(
                download_store,
                run@ids$run_id,
                status = "stopping",
                last_error = "Cancelled by user."
            )
            shift_job__run_event(
                download_store,
                run@ids$run_id,
                "download",
                "stopping",
                "Background download cancellation requested.",
                details = list(
                    step_id = download_context$step$step_id[[1L]],
                    session_id = download_context$session_id,
                    downloader_job_id = downloader_job_id,
                    force = force
                ),
                step_id = download_context$step$step_id[[1L]]
            )
            shift_job__reconcile_background_download(
                download_store,
                run@ids$run_id
            )
            return(shift_job__run_handle(download_store, run@ids$run_id))
        }
        try(download_store$close(), silent = TRUE)
    }
    if (identical(status, "waiting")) {
        # No process is active between object-carried stages. Cancelling here
        # closes the resumable run immediately instead of creating a stopping
        # state that no worker could ever acknowledge.
        run_store <- shift_store(run)
        on.exit(try(run_store$close(), silent = TRUE), add = TRUE)
        shift_job__run_finish(
            run_store,
            run@ids$run_id,
            "cancelled",
            current_stage = run@meta$run$current_stage[[1L]],
            last_error = "Cancelled by user while waiting for the next stage."
        )
        shift_job__run_event(
            run_store,
            run@ids$run_id,
            run@meta$run$current_stage[[1L]],
            "cancelled",
            "Waiting shift run cancelled by user."
        )
        return(shift_job__run_handle(run_store, run@ids$run_id))
    }
    job <- data.table::as.data.table(run@meta$jobs)
    if (nrow(job)) {
        job <- job[which.max(job[["attempt"]])]
    }
    if (!nrow(job)) {
        cli::cli_abort(
            "Shift run {.val {run@ids$run_id}} has no execution job to cancel."
        )
    }
    now <- store__now()
    pid <- suppressWarnings(as.integer(job$pid[[1L]]))
    shift_job__cancel_request_write(
        run@store_path,
        run@ids$run_id,
        job$job_id[[1L]],
        force = force
    )

    # A detached worker owns DuckDB's write lock for the duration of an active
    # stage. If the manifest cannot be opened, the sidecar marker is the
    # cooperative signal and the live handle is updated in memory immediately.
    run_store <- tryCatch(
        EsgStore$new(run@store_path, create = FALSE),
        error = function(e) e
    )
    if (inherits(run_store, "error")) {
        if (!shift_job__manifest_locked(run_store)) {
            stop(run_store)
        }
        live_status <- if (isTRUE(force)) "cancelled" else "stopping"
        marked <- shift_job__live_cancel_mark(
            run@store_path,
            run@ids$run_id,
            job$job_id[[1L]],
            live_status
        )
        if (!is.null(marked)) {
            run <- marked
        }
        if (isTRUE(force) && !is.na(pid)) {
            downloader__pid_kill(pid)
            # Wait briefly for DuckDB to release its process lock, then let the
            # normal stale reconciliation record a cancelled terminal state.
            for (i in seq_len(40L)) {
                if (!downloader__pid_alive(pid)) {
                    break
                }
                Sys.sleep(0.05)
            }
            refreshed <- tryCatch(
                shift_run_get(run@ids$run_id, run@store_path),
                error = function(e) NULL
            )
            if (!is.null(refreshed)) {
                return(refreshed)
            }
        }
        return(run)
    }
    on.exit(try(run_store$close(), silent = TRUE), add = TRUE)
    job <- shift_job__latest_job(run_store, run@ids$run_id)
    immediate <- identical(job$status[[1L]], "queued") && is.na(pid)
    job_status <- if (isTRUE(immediate) || isTRUE(force)) {
        "cancelled"
    } else {
        "stopping"
    }
    shift_job__job_update(
        run_store,
        job$job_id[[1L]],
        status = job_status,
        cancel_requested_at = now,
        completed_at = if (identical(job_status, "cancelled")) {
            now
        } else {
            job$completed_at
        },
        exit_code = if (identical(job_status, "cancelled")) {
            130L
        } else {
            job$exit_code
        },
        last_error = "Cancelled by user."
    )
    if (identical(job_status, "cancelled")) {
        shift_job__run_finish(
            run_store,
            run@ids$run_id,
            status = job_status,
            last_error = "Cancelled by user."
        )
    } else {
        shift_job__run_update(
            run_store,
            run@ids$run_id,
            status = job_status,
            last_error = "Cancelled by user."
        )
    }
    shift_job__run_event(
        run_store,
        run@ids$run_id,
        run@meta$run$current_stage[[1L]],
        job_status,
        "Cancellation requested by user.",
        details = list(job_id = job$job_id[[1L]], force = force, pid = pid)
    )
    if (isTRUE(force) && !is.na(pid)) {
        downloader__pid_kill(pid)
    }
    shift_job__run_handle(run_store, run@ids$run_id)
}
# }}}

# Resolve the Downloader session that owns an open standalone download step.
# The returned context joins the shift run identity to the existing persistent
# downloader manifest without duplicating its task or process tables.
# shift_job__background_download_context {{{
shift_job__background_download_context <- function(
    store,
    run_id,
    active_only = TRUE
) {
    checkmate::assert_flag(active_only)
    step <- shift_job__latest_step(store, run_id)
    if (
        !nrow(step) ||
            !identical(step$task[[1L]], "download") ||
            (isTRUE(active_only) && !identical(step$status[[1L]], "running")) ||
            is.na(step$output_stage_json[[1L]]) ||
            !nzchar(step$output_stage_json[[1L]])
    ) {
        return(NULL)
    }
    ref <- tryCatch(
        jsonlite::fromJSON(
            step$output_stage_json[[1L]],
            simplifyVector = FALSE
        ),
        error = function(e) NULL
    )
    if (is.null(ref)) {
        return(NULL)
    }
    stage <- shift_persist__stage_from_ref(ref)
    session_id <- store__chr1(stage@ids$session_id)
    if (is.na(session_id) || !nzchar(session_id)) {
        return(NULL)
    }

    downloader <- store$downloader()
    sessions <- data.table::as.data.table(downloader$sessions())
    wanted_session_id <- session_id
    session <- sessions[sessions[["session_id"]] == wanted_session_id]
    jobs <- data.table::as.data.table(downloader$jobs())
    if ("session_id" %in% names(jobs)) {
        jobs <- jobs[jobs[["session_id"]] == wanted_session_id]
    } else {
        jobs <- jobs[0]
    }
    if (nrow(jobs) && "created_at" %in% names(jobs)) {
        jobs <- jobs[order(jobs[["created_at"]])]
    }
    list(
        step = step,
        stage = stage,
        session_id = session_id,
        session = session,
        jobs = jobs,
        downloader = downloader
    )
}
# }}}

# Reconcile an existing Downloader process into the shared ShiftRun lifecycle.
# Polling is read-only while work is active; only terminal downloader states
# create shift step/run events, so watch refreshes do not become heartbeat spam.
# shift_job__reconcile_background_download {{{
shift_job__reconcile_background_download <- function(store, run_id) {
    context <- shift_job__background_download_context(store, run_id)
    if (is.null(context)) {
        return(invisible(NULL))
    }
    session_status <- if (nrow(context$session)) {
        as.character(context$session$status[[nrow(context$session)]])
    } else {
        NA_character_
    }
    job_status <- if (nrow(context$jobs)) {
        as.character(context$jobs$status[[nrow(context$jobs)]])
    } else {
        NA_character_
    }
    status <- if (job_status %in% c("error", "cancelled", "stale")) {
        job_status
    } else if (!is.na(session_status) && nzchar(session_status)) {
        session_status
    } else {
        job_status
    }
    if (
        is.na(status) ||
            status %in% c("queued", "running", "downloading", "stopping")
    ) {
        return(invisible(context))
    }

    step_id <- context$step$step_id[[1L]]
    details <- list(
        phase = "operation",
        stage = "download",
        step_id = step_id,
        session_id = context$session_id,
        downloader_status = status
    )
    if (identical(status, "done")) {
        # The detached downloader has released its manifest and output files;
        # synchronize those files before exposing the next-stage boundary.
        store$sync_downloads(context$downloader)
        shift_job__step_finish(
            store,
            step_id,
            "completed",
            output_stage = context$stage
        )
        shift_job__run_update(
            store,
            run_id,
            status = "waiting",
            current_stage = "download",
            last_error = NA_character_
        )
        shift_job__run_event(
            store,
            run_id,
            "download",
            "waiting",
            "Background download completed; ready for the next stage.",
            details = c(details, list(outcome = "completed")),
            step_id = step_id
        )
        return(invisible(context))
    }

    terminal_status <- if (identical(status, "cancelled")) {
        "cancelled"
    } else {
        "failed"
    }
    message <- if (identical(terminal_status, "cancelled")) {
        "Background download was cancelled."
    } else {
        sprintf("Background download failed with status %s.", status)
    }
    shift_job__step_finish(
        store,
        step_id,
        terminal_status,
        last_error = message
    )
    shift_job__run_finish(
        store,
        run_id,
        terminal_status,
        current_stage = "download",
        last_error = message
    )
    shift_job__run_event(
        store,
        run_id,
        "download",
        terminal_status,
        message,
        details = c(details, list(outcome = terminal_status)),
        step_id = step_id
    )
    invisible(context)
}
# }}}

# Append one immutable run event for status displays and recovery diagnostics.
# Reporter callers may defer the sidecar snapshot until their paired heartbeat
# update so a single milestone does not rewrite the same live state twice.
# shift_job__run_event {{{
shift_job__run_event <- function(
    store,
    run_id,
    stage,
    status,
    message = NA_character_,
    details = NULL,
    snapshot = TRUE,
    step_id = NULL,
    event_id = NULL
) {
    now <- store__now()
    event_id <- shift_stage__coalesce(
        event_id,
        store__hash(run_id, stage, status, now, stats::runif(1L))
    )
    row <- data.table::data.table(
        event_id = event_id,
        run_id = run_id,
        step_id = store__chr1(step_id),
        stage = stage,
        status = status,
        message = store__chr1(message),
        details_json = if (is.null(details)) {
            NA_character_
        } else {
            shift_persist__spec_json(details)
        },
        created_at = now
    )
    morpher__private_store(store)$append_new_rows(
        "shift_run_event",
        row,
        "event_id"
    )
    if (isTRUE(snapshot)) {
        shift_job__live_snapshot_write(store, run_id)
    }
    invisible(row)
}
# }}}

# Persist stage diagnostics as idempotent run events so warnings and
# informational scientific decisions survive refresh and cross-session resume.
# shift_job__run_diagnostics_record {{{
shift_job__run_diagnostics_record <- function(store, run_id, diagnostics) {
    diagnostics <- shift_stage__diagnostics_normalize(diagnostics)
    if (!nrow(diagnostics)) {
        return(invisible(diagnostics))
    }
    for (i in seq_len(nrow(diagnostics))) {
        row <- diagnostics[i]
        details <- c(
            list(kind = "scientific_diagnostic"),
            as.list(row)
        )
        shift_job__run_event(
            store,
            run_id,
            stage = row$stage[[1L]],
            status = "diagnostic",
            message = row$message[[1L]],
            details = details,
            snapshot = i == nrow(diagnostics),
            event_id = store__hash(
                run_id,
                "scientific-diagnostic",
                as.list(row)
            )
        )
    }
    invisible(diagnostics)
}
# }}}

# Create one durable execution attempt for a run. Foreground attempts use the
# current PID; background attempts fill their PID when the worker starts.
# shift_job__job_create {{{
shift_job__job_create <- function(
    store,
    run_id,
    mode = c("foreground", "process"),
    ui = shift_ui(),
    step_id = NULL
) {
    mode <- match.arg(mode)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    attempts <- shift_inspect__rows(
        store,
        "shift_run_job",
        "run_id",
        wanted_run_id
    )$attempt
    attempt <- if (length(attempts)) max(attempts, na.rm = TRUE) + 1L else 1L
    now <- store__now()
    job_id <- paste0(
        "shift-job-",
        substr(store__hash(run_id, attempt, now, stats::runif(1L)), 1L, 20L)
    )
    log_dir <- file.path(store$path, "logs", "shift")
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    row <- data.table::data.table(
        job_id = job_id,
        run_id = run_id,
        step_id = store__chr1(step_id),
        attempt = as.integer(attempt),
        mode = mode,
        status = if (identical(mode, "process")) "queued" else "running",
        pid = if (identical(mode, "foreground")) {
            as.integer(Sys.getpid())
        } else {
            NA_integer_
        },
        hostname = unname(shift_stage__coalesce(
            Sys.info()[["nodename"]],
            "localhost"
        )),
        log_path = if (identical(mode, "process")) {
            file.path(log_dir, paste0(job_id, ".log"))
        } else {
            NA_character_
        },
        ui_json = shift_persist__spec_json(list(
            progress = ui@progress,
            detail = ui@detail,
            motion = ui@motion,
            refresh = ui@refresh,
            heartbeat = ui@heartbeat,
            execution = shift_execution__options()
        )),
        cancel_requested_at = as.POSIXct(NA, tz = "UTC"),
        started_at = if (identical(mode, "foreground")) {
            now
        } else {
            as.POSIXct(NA, tz = "UTC")
        },
        heartbeat_at = if (identical(mode, "foreground")) {
            now
        } else {
            as.POSIXct(NA, tz = "UTC")
        },
        completed_at = as.POSIXct(NA, tz = "UTC"),
        exit_code = NA_integer_,
        last_error = NA_character_,
        created_at = now,
        updated_at = now
    )
    # A resumed attempt owns a new cancellation boundary; remove any marker
    # left by the preceding failed or cancelled attempt before registering it.
    unlink(
        shift_job__live_path(store$path, run_id, suffix = "cancel.json"),
        force = TRUE
    )
    private$append_new_rows("shift_run_job", row, "job_id")
    shift_job__live_snapshot_write(store, run_id)
    row
}
# }}}

# Update a job row after a process/status transition while preserving the
# immutable run, attempt, and job identities.
# shift_job__job_update {{{
shift_job__job_update <- function(
    store,
    job_id,
    ...,
    .snapshot = TRUE,
    .ui_state = NULL
) {
    wanted_job_id <- job_id
    row <- shift_inspect__rows(store, "shift_run_job", "job_id", wanted_job_id)
    if (!nrow(row)) {
        cli::cli_abort("Shift job {.val {job_id}} was not found.")
    }
    updates <- list(...)
    unknown <- setdiff(names(updates), names(row))
    if (length(unknown)) {
        cli::cli_abort("Unknown shift job field(s): {.field {unknown}}.")
    }
    for (name in names(updates)) {
        data.table::set(row, j = name, value = updates[[name]])
    }
    data.table::set(row, j = "updated_at", value = store__now())
    shift_job__update_row(
        store,
        "shift_run_job",
        "job_id",
        row,
        unique(c(names(updates), "updated_at"))
    )
    if (isTRUE(.snapshot)) {
        shift_job__live_snapshot_write(
            store,
            row$run_id[[1L]],
            ui_state = .ui_state
        )
    }
    invisible(row)
}
# }}}

# Update the worker heartbeat only at reporter callbacks and workflow
# boundaries; this is deliberately separate from transient Console animation.
# shift_job__job_touch {{{
shift_job__job_touch <- function(store, job_id, ui_state = NULL) {
    shift_job__job_update(
        store,
        job_id,
        heartbeat_at = store__now(),
        .ui_state = ui_state
    )
}
# }}}

# Return all attempts for a run in deterministic attempt order.
# shift_job__run_jobs {{{
shift_job__run_jobs <- function(store, run_id) {
    wanted_run_id <- run_id
    jobs <- shift_inspect__rows(store, "shift_run_job", "run_id", wanted_run_id)
    jobs[order(jobs[["attempt"]])]
}
# }}}

# Read the most recent attempt, which is authoritative for cancellation,
# logging, and stale-process reconciliation.
# shift_job__latest_job {{{
shift_job__latest_job <- function(store, run_id) {
    jobs <- shift_job__run_jobs(store, run_id)
    if (!nrow(jobs)) jobs else jobs[which.max(jobs[["attempt"]])]
}
# }}}

# Reconcile detached jobs when a worker exits before it can persist a terminal
# state, preventing background runs from appearing active forever.
# shift_job__reconcile_run_job {{{
shift_job__reconcile_run_job <- function(store, run_id, startup_grace = 60) {
    job <- shift_job__latest_job(store, run_id)
    if (
        !nrow(job) || !job$status[[1L]] %in% c("queued", "running", "stopping")
    ) {
        return(invisible(job))
    }
    now <- store__now()
    pid <- suppressWarnings(as.integer(job$pid[[1L]]))
    stale <- FALSE
    reason <- NA_character_
    if (is.na(pid)) {
        age <- as.numeric(difftime(now, job$created_at[[1L]], units = "secs"))
        stale <- identical(job$mode[[1L]], "process") &&
            is.finite(age) &&
            age > startup_grace
        if (stale) {
            reason <- sprintf(
                "Background worker did not report a PID within %d seconds.",
                as.integer(startup_grace)
            )
        }
    } else if (
        identical(job$mode[[1L]], "process") && !downloader__pid_alive(pid)
    ) {
        stale <- TRUE
        reason <- sprintf("Background worker PID %d is not running.", pid)
    }
    if (!isTRUE(stale)) {
        return(invisible(job))
    }
    cancelled <- shift_job__cancel_request_exists(
        store$path,
        run_id,
        job$job_id[[1L]]
    )
    if (isTRUE(cancelled)) {
        reason <- "Background worker stopped after cancellation was requested."
        shift_job__job_update(
            store,
            job$job_id[[1L]],
            status = "cancelled",
            completed_at = now,
            exit_code = 130L,
            last_error = reason
        )
        shift_job__run_finish(
            store,
            run_id,
            status = "cancelled",
            last_error = reason
        )
        shift_job__run_event(
            store,
            run_id,
            "worker",
            "cancelled",
            reason,
            details = list(
                job_id = job$job_id[[1L]],
                pid = pid,
                outcome = "cancelled"
            )
        )
    } else {
        shift_job__job_update(
            store,
            job$job_id[[1L]],
            status = "stale",
            completed_at = now,
            exit_code = 1L,
            last_error = reason
        )
        shift_job__run_finish(
            store,
            run_id,
            status = "failed",
            last_error = reason
        )
        shift_job__run_event(
            store,
            run_id,
            "worker",
            "failed",
            reason,
            details = list(
                job_id = job$job_id[[1L]],
                pid = pid,
                outcome = "stale"
            )
        )
    }
    invisible(shift_job__latest_job(store, run_id))
}
# }}}

# Cooperative cancellation is checked at every stage and business-unit
# boundary so partial artifacts remain resumable and manifest-consistent.
# shift_job__job_cancel_requested {{{
shift_job__job_cancel_requested <- function(store, job_id) {
    wanted_job_id <- job_id
    row <- shift_inspect__rows(store, "shift_run_job", "job_id", wanted_job_id)
    nrow(row) &&
        (!is.na(row$cancel_requested_at[[1L]]) ||
            row$status[[1L]] %in% c("stopping", "cancelled") ||
            shift_job__cancel_request_exists(
                store$path,
                row$run_id[[1L]],
                job_id
            ))
}
# }}}

# Abort with a dedicated condition after persisting a user cancellation request.
# shift_job__job_check_cancel {{{
shift_job__job_check_cancel <- function(store, run_id, job_id, stage) {
    if (!is.null(job_id) && shift_job__job_cancel_requested(store, job_id)) {
        cli::cli_abort(
            "Future EPW workflow run {.val {run_id}} was cancelled during {.val {stage}}.",
            class = "epwshiftr_shift_cancelled",
            run_id = run_id,
            job_id = job_id,
            stage = stage
        )
    }
    invisible(FALSE)
}
# }}}

# Background workers require a JSON-safe public transform and plan; validate
# both by reconstructing the exact persisted specification before registration.
# shift_job__validate_background_plan {{{
shift_job__validate_background_plan <- function(plan) {
    spec <- shift_persist__plan_spec(plan)
    tryCatch(
        shift_persist__plan_from_spec(spec, store = plan@store_path),
        error = function(e) {
            cli::cli_abort(
                c(
                    "The shift plan cannot be reconstructed for background execution.",
                    "x" = conditionMessage(e),
                    "i" = "Run with {.code background = FALSE} for session-local inputs."
                ),
                parent = e
            )
        }
    )
    invisible(TRUE)
}
# }}}

# Build the detached Rscript command without serializing live R objects into
# the child process; the durable run and job IDs are its only inputs.
# shift_job__launch_job {{{
shift_job__launch_job <- function(store_path, run_id, job_id, log_path) {
    status <- tryCatch(
        shift_execution__launch(
            "shift_job__job_main",
            list(
                store_path = store_path,
                run_id = run_id,
                job_id = job_id
            ),
            log_path
        ),
        error = function(e) e
    )
    if (inherits(status, "error")) {
        failed_store <- EsgStore$new(store_path, create = FALSE)
        on.exit(try(failed_store$close(), silent = TRUE), add = TRUE)
        shift_job__job_update(
            failed_store,
            job_id,
            status = "failed",
            completed_at = store__now(),
            exit_code = 1L,
            last_error = conditionMessage(status)
        )
        shift_job__run_finish(
            failed_store,
            run_id,
            status = "failed",
            last_error = conditionMessage(status)
        )
        cli::cli_abort(
            "Failed to launch background shift job: {conditionMessage(status)}"
        )
    }
    invisible(status)
}
# }}}

# Open the worker manifest with bounded retries because an immediate status or
# cancel call may briefly own DuckDB between process launch and worker startup.
# shift_job__job_store_open {{{
shift_job__job_store_open <- function(
    store_path,
    timeout = 60,
    interval = 0.1
) {
    checkmate::assert_number(timeout, lower = 0, finite = TRUE)
    checkmate::assert_number(interval, lower = 0.01, finite = TRUE)
    started <- Sys.time()
    repeat {
        store <- tryCatch(
            EsgStore$new(store_path, create = FALSE),
            error = function(e) e
        )
        if (!inherits(store, "error")) {
            return(store)
        }
        elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
        if (!shift_job__manifest_locked(store) || elapsed >= timeout) {
            stop(store)
        }
        Sys.sleep(interval)
    }
}
# }}}

# Execute one detached workflow attempt from persisted intent. The worker uses
# log mode because its stdout/stderr are redirected to the job log.
# shift_job__job_main {{{
shift_job__job_main <- function(store_path, run_id, job_id) {
    store <- shift_job__job_store_open(store_path)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    wanted_run_id <- run_id
    wanted_job_id <- job_id
    jobs <- shift_inspect__rows(store, "shift_run_job", "job_id", wanted_job_id)
    job <- jobs[
        jobs[["job_id"]] == wanted_job_id &
            jobs[["run_id"]] == wanted_run_id
    ]
    if (!nrow(job)) {
        cli::cli_abort("Background shift job {.val {job_id}} was not found.")
    }
    ui_spec <- jsonlite::fromJSON(job$ui_json[[1L]], simplifyVector = TRUE)
    ui <- shift_ui(
        # Detached workers use stable logs for every visible mode, while an
        # explicit none setting remains completely quiet.
        progress = if (identical(as.character(ui_spec$progress), "none")) {
            "none"
        } else {
            "log"
        },
        detail = as.character(ui_spec$detail),
        motion = as.character(ui_spec$motion),
        refresh = as.numeric(ui_spec$refresh),
        heartbeat = as.numeric(ui_spec$heartbeat)
    )
    shift_job__run_update(
        store,
        run_id,
        status = "running",
        completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_
    )

    row <- shift_inspect__rows(store, "shift_run", "run_id", wanted_run_id)
    if (!nrow(row)) {
        cli::cli_abort("Background shift run {.val {run_id}} was not found.")
    }
    spec <- jsonlite::fromJSON(row$spec_json[[1L]], simplifyVector = TRUE)
    plan <- shift_persist__plan_from_spec(spec, store = store_path)
    resolved <- row$resolved_spec_json[[1L]]
    if (!is.na(resolved) && nzchar(resolved)) {
        # Resume always reuses the first successful node/member/grid selection.
        plan@meta$resolved <- jsonlite::fromJSON(
            resolved,
            simplifyVector = TRUE
        )
    }
    context <- shift_execution__context(store_path, job, store)
    reporter <- shift_reporter__reporter(
        ui,
        store = store,
        run_id = run_id,
        job_id = job_id,
        background = TRUE,
        execution = context
    )
    on.exit(reporter$close(), add = TRUE)
    reporter$run_started(plan, run_id, background = TRUE)
    shift_execution__run(
        context,
        shift_run__plan_run(
            plan,
            run_id = run_id,
            job_id = job_id,
            reporter = reporter,
            resume_existing = job$attempt[[1L]] > 1L
        )
    )
    invisible(TRUE)
}
# }}}

# Update a run row after a state transition while leaving the original spec
# and unique run identity unchanged.
# shift_job__run_update {{{
shift_job__run_update <- function(store, run_id, ...) {
    wanted_run_id <- run_id
    row <- shift_inspect__rows(store, "shift_run", "run_id", wanted_run_id)
    if (!nrow(row)) {
        cli::cli_abort("Shift run {.val {run_id}} was not found.")
    }
    updates <- list(...)
    unknown <- setdiff(names(updates), names(row))
    if (length(unknown)) {
        cli::cli_abort("Unknown shift run field(s): {.field {unknown}}.")
    }
    for (name in names(updates)) {
        data.table::set(row, j = name, value = updates[[name]])
    }
    data.table::set(row, j = "updated_at", value = store__now())
    shift_job__update_row(
        store,
        "shift_run",
        "run_id",
        row,
        unique(c(names(updates), "updated_at"))
    )
    shift_job__live_snapshot_write(store, run_id)
    invisible(row)
}
# }}}

# Finish one run through a single terminal-state boundary so every completed,
# partial, failed, or cancelled row freezes its elapsed time consistently.
# shift_job__run_finish {{{
shift_job__run_finish <- function(store, run_id, status, ...) {
    checkmate::assert_choice(
        status,
        c("completed", "partial", "failed", "cancelled")
    )
    updates <- list(...)
    if ("completed_at" %in% names(updates)) {
        cli::cli_abort("`completed_at` is owned by `shift_job__run_finish()`.")
    }
    do.call(
        shift_job__run_update,
        c(
            list(
                store = store,
                run_id = run_id,
                status = status,
                completed_at = store__now()
            ),
            updates
        )
    )
}
# }}}

# Persist the current case matrix as the authoritative fulfilment contract for
# this run. Each run owns independent rows even when its spec hash is reused.
# shift_job__run_cases_write {{{
shift_job__run_cases_write <- function(store, run_id, cases) {
    private <- morpher__private_store(store)
    cases <- data.table::as.data.table(data.table::copy(cases))
    rows <- data.table::data.table(
        run_case_id = vapply(
            cases$case_id,
            function(value) store__hash(run_id, value),
            character(1L)
        ),
        run_id = run_id,
        case_id = cases$case_id,
        source_id = cases$source_id,
        experiment_id = cases$experiment_id,
        variant_label = cases$variant_label,
        grid_label = cases$grid_label,
        period = cases$period,
        years_json = vapply(
            cases$years,
            function(value) shift_persist__spec_json(as.integer(value)),
            character(1L)
        ),
        required = as.logical(cases$required),
        status = cases$status,
        output_id = cases$output_id,
        export_path = cases$export_path,
        missing_reason = cases$missing_reason,
        updated_at = store__now()
    )
    private$delete_by_key("shift_run_case", "run_id", run_id)
    private$append_new_rows("shift_run_case", rows, "run_case_id")
    shift_job__live_snapshot_write(store, run_id)
    invisible(cases)
}
# }}}

# Register workflow intent through the shared run writer, then save its cases.
# shift_job__run_register {{{
shift_job__run_register <- function(plan) {
    store <- shift_store(plan, create = TRUE)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    run_id <- shift_job__task_run_register(
        store,
        "future_epw",
        shift_persist__plan_spec(plan)
    )
    shift_job__run_cases_write(store, run_id, plan@meta$expected_cases)
    run_id
}
# }}}

# Register a generic stage run before its first side effect. UI preferences are
# intentionally absent from the spec so changing presentation never alters
# deterministic task identity.
# shift_job__task_run_register {{{
shift_job__task_run_register <- function(
    store,
    task,
    spec = list(),
    status = c("queued", "waiting")
) {
    status <- match.arg(status)
    checkmate::assert_string(task, min.chars = 1L)
    checkmate::assert_list(spec)
    # Workflow specs already carry a canonical schema, including explicit NULLs.
    # Keep their serialization byte-identical for persisted run reuse.
    if (task != "future_epw") {
        spec <- utils::modifyList(list(version = 1L, task = task), spec)
    }
    spec_json <- shift_persist__spec_json(spec)
    spec_hash <- store__hash(spec_json)
    now <- store__now()
    run_id <- paste0(
        "run_",
        substr(
            store__hash(
                spec_hash,
                now,
                stats::runif(1L)
            ),
            1L,
            24L
        )
    )
    row <- data.table::data.table(
        run_id = run_id,
        task = task,
        spec_hash = spec_hash,
        spec_json = spec_json,
        resolved_spec_json = NA_character_,
        status = status,
        current_stage = if (identical(status, "waiting")) {
            "waiting"
        } else {
            "planned"
        },
        query_id = NA_character_,
        reference_query_id = NA_character_,
        plan_ids_json = NA_character_,
        reference_plan_ids_json = NA_character_,
        morph_id = NA_character_,
        output_dir = store__chr1(
            if (task == "future_epw") {
                spec$stages$epw$export_dir
            } else {
                spec$output_dir
            }
        ),
        package_version = as.character(utils::packageVersion("epwshiftr")),
        started_at = now,
        updated_at = now,
        completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_
    )
    morpher__private_store(store)$append_new_rows("shift_run", row, "run_id")
    shift_job__run_event(
        store,
        run_id,
        row$current_stage[[1L]],
        status,
        if (task == "future_epw") {
            "Workflow run registered."
        } else {
            sprintf("%s task registered.", task)
        }
    )
    run_id
}
# }}}

# Create one ordered task step under a run. The immutable input/spec fields are
# written before execution so even an early interrupt remains diagnosable.
# shift_job__step_create {{{
shift_job__step_create <- function(
    store,
    run_id,
    task,
    spec,
    input_stage = NULL,
    resumable = TRUE,
    nonresumable_reason = NULL
) {
    checkmate::assert_string(task, min.chars = 1L)
    checkmate::assert_flag(resumable)
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    previous <- shift_inspect__rows(
        store,
        "shift_run_step",
        "run_id",
        wanted_run_id
    )$ordinal
    ordinal <- if (length(previous)) max(previous, na.rm = TRUE) + 1L else 1L
    spec_json <- shift_persist__spec_json(spec)
    now <- store__now()
    step_id <- paste0(
        "step_",
        substr(
            store__hash(
                run_id,
                ordinal,
                spec_json
            ),
            1L,
            24L
        )
    )
    row <- data.table::data.table(
        step_id = step_id,
        run_id = run_id,
        ordinal = as.integer(ordinal),
        task = task,
        spec_hash = store__hash(spec_json),
        spec_json = spec_json,
        input_stage_json = if (is.null(input_stage)) {
            NA_character_
        } else {
            shift_persist__spec_json(shift_persist__stage_ref(input_stage))
        },
        output_stage_json = NA_character_,
        status = "running",
        resumable = resumable,
        nonresumable_reason = store__chr1(nonresumable_reason),
        started_at = now,
        updated_at = now,
        completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_
    )
    private$append_new_rows("shift_run_step", row, "step_id")
    row
}
# }}}

# Update mutable step state while preserving its stable task specification.
# shift_job__step_update {{{
shift_job__step_update <- function(store, step_id, ...) {
    wanted_step_id <- step_id
    row <- shift_inspect__rows(
        store,
        "shift_run_step",
        "step_id",
        wanted_step_id
    )
    if (!nrow(row)) {
        cli::cli_abort("Shift step {.val {step_id}} was not found.")
    }
    updates <- list(...)
    unknown <- setdiff(names(updates), names(row))
    if (length(unknown)) {
        cli::cli_abort("Unknown shift step field(s): {.field {unknown}}.")
    }
    for (name in names(updates)) {
        data.table::set(row, j = name, value = updates[[name]])
    }
    data.table::set(row, j = "updated_at", value = store__now())
    shift_job__update_row(
        store,
        "shift_run_step",
        "step_id",
        row,
        unique(c(names(updates), "updated_at"))
    )
    shift_job__live_snapshot_write(store, row$run_id[[1L]])
    invisible(row)
}
# }}}

# Close one step independently from its object-carried workflow run.
# shift_job__step_finish {{{
shift_job__step_finish <- function(
    store,
    step_id,
    status,
    output_stage = NULL,
    last_error = NULL
) {
    checkmate::assert_choice(
        status,
        c("completed", "partial", "failed", "cancelled")
    )
    shift_job__step_update(
        store,
        step_id,
        status = status,
        output_stage_json = if (is.null(output_stage)) {
            NA_character_
        } else {
            shift_persist__spec_json(shift_persist__stage_ref(output_stage))
        },
        completed_at = store__now(),
        last_error = store__chr1(last_error)
    )
}
# }}}

# Return the latest step for resume, result reconstruction, and task-aware
# inspectors without assuming that every run is a Future EPW workflow.
# shift_job__latest_step {{{
shift_job__latest_step <- function(store, run_id, completed = FALSE) {
    wanted_run_id <- run_id
    steps <- shift_inspect__rows(
        store,
        "shift_run_step",
        "run_id",
        wanted_run_id
    )
    if (isTRUE(completed)) {
        steps <- steps[
            steps[["status"]] %in%
                c("completed", "partial") &
                !is.na(steps[["output_stage_json"]])
        ]
    }
    if (!nrow(steps)) steps else steps[which.max(steps[["ordinal"]])]
}
# }}}

# Derive the terminal run outcome from every durable step rather than only the
# last artifact. A later successful morph or export must not hide an upstream
# partial extraction or download.
# shift_job__run_completion_status {{{
shift_job__run_completion_status <- function(store, run_id) {
    wanted_run_id <- run_id
    steps <- shift_inspect__rows(
        store,
        "shift_run_step",
        "run_id",
        wanted_run_id
    )
    if (nrow(steps) && any(steps[["status"]] == "partial")) {
        "partial"
    } else {
        "completed"
    }
}
# }}}

# Rebuild one actionable ShiftRun diagnostic from its persisted terminal event.
# Resolver coverage failures recommend changing intent, while transient and
# later-stage errors retain resume as the recovery action.
# shift_job__run_event_diagnostic {{{
shift_job__run_event_diagnostic <- function(event, run_id, store_path) {
    details <- if (
        !is.null(event$details_json) &&
            length(event$details_json) &&
            !is.na(event$details_json[[1L]]) &&
            nzchar(event$details_json[[1L]])
    ) {
        tryCatch(
            jsonlite::fromJSON(event$details_json[[1L]], simplifyVector = TRUE),
            error = function(e) list()
        )
    } else {
        list()
    }
    if (identical(as.character(details$kind), "scientific_diagnostic")) {
        field <- function(name) store__chr1(details[[name]])
        return(shift_stage__diagnostic(
            field("stage"),
            field("severity"),
            field("code"),
            field("message"),
            query_id = field("query_id"),
            session_id = field("session_id"),
            plan_id = field("plan_id"),
            summary_id = field("summary_id"),
            baseline_id = field("baseline_id"),
            morph_id = field("morph_id"),
            case_id = field("case_id"),
            variable_id = field("variable_id"),
            epw_field = field("epw_field"),
            period = field("period"),
            month = field("month"),
            action = field("action")
        ))
    }
    missing <- as.character(shift_stage__coalesce(details$missing, character()))
    missing <- missing[!is.na(missing) & nzchar(missing)]
    message <- as.character(shift_stage__coalesce(
        details$cause,
        shift_stage__coalesce(
            details$error_summary,
            shift_print__error_summary(event$message[[1L]])
        )
    ))[[1L]]
    if (length(missing)) {
        message <- paste0(
            message,
            " First missing requirement: ",
            missing[[1L]],
            "."
        )
    }
    recovery <- as.character(shift_stage__coalesce(details$recovery, "retry"))[[
        1L
    ]]
    action <- switch(
        recovery,
        change_request = paste(
            "Adjust the CMIP6 selection or reference before retrying;",
            "resuming unchanged will repeat this coverage failure."
        ),
        inspect = paste(
            "Inspect the per-node diagnostics; retry only after confirming",
            "that a transient node failure could change the result."
        ),
        sprintf(
            "Run %s.",
            shift_print__run_command(
                "shift_resume",
                run_id,
                store_path
            )
        )
    )
    shift_stage__diagnostic(
        event$stage[[1L]],
        "error",
        if (identical(details$kind, "resolver_exhausted")) {
            "shift_resolver_exhausted"
        } else {
            "shift_run_error"
        },
        message,
        action = action
    )
}
# }}}

# Materialize a lightweight ShiftRun handle from persisted tables.
# shift_job__run_handle {{{
shift_job__run_handle <- function(
    store,
    run_id,
    output_stage = NULL,
    plan = NULL
) {
    wanted_run_id <- run_id
    row <- shift_inspect__rows(store, "shift_run", "run_id", wanted_run_id)
    if (!nrow(row)) {
        cli::cli_abort(
            "Shift run {.val {run_id}} was not found in {.path {store$path}}."
        )
    }
    cases <- shift_inspect__rows(
        store,
        "shift_run_case",
        "run_id",
        wanted_run_id
    )
    if (nrow(cases)) {
        cases[,
            years := lapply(years_json, function(value) {
                as.integer(jsonlite::fromJSON(value, simplifyVector = TRUE))
            })
        ]
    }
    events <- shift_inspect__rows(
        store,
        "shift_run_event",
        "run_id",
        wanted_run_id
    )[order(created_at)]
    jobs <- shift_inspect__rows(store, "shift_run_job", "run_id", wanted_run_id)
    jobs <- jobs[order(jobs[["attempt"]])]
    steps <- shift_inspect__rows(
        store,
        "shift_run_step",
        "run_id",
        wanted_run_id
    )
    steps <- steps[order(steps[["ordinal"]])]
    diagnostic_events <- events[
        status %in% c("failed", "error", "diagnostic")
    ]
    diagnostics <- if (!nrow(diagnostic_events)) {
        shift_stage__diagnostics_empty()
    } else {
        do.call(
            shift_stage__bind_diagnostics,
            lapply(
                seq_len(nrow(diagnostic_events)),
                function(i) {
                    shift_job__run_event_diagnostic(
                        diagnostic_events[i],
                        run_id,
                        store$path
                    )
                }
            )
        )
    }
    shift_stage__new(
        ShiftRun,
        "run",
        store_path = store$path,
        ids = list(
            run_id = run_id,
            query_id = store__chr1(row$query_id[[1L]]),
            reference_query_id = store__chr1(row$reference_query_id[[1L]]),
            morph_id = store__chr1(row$morph_id[[1L]])
        ),
        meta = list(
            run = row[1L],
            cases = cases,
            events = events,
            jobs = jobs,
            steps = steps,
            output_stage = output_stage,
            plan = plan
        ),
        diagnostics = diagnostics
    )
}
# }}}

# Use atomic sidecar snapshots as the live read channel while a detached worker
# owns DuckDB's cross-process write lock. DuckDB remains the durable authority.
# shift_job__live_path {{{
shift_job__live_path <- function(store_path, run_id, suffix = "live.json") {
    file.path(store_path, "logs", "shift", sprintf("%s.%s", run_id, suffix))
}
# }}}

# Sidecar fallback is only valid for DuckDB's expected cross-process lock
# conflict; schema, corruption, and path errors must remain visible.
# shift_job__manifest_locked {{{
shift_job__manifest_locked <- function(error) {
    inherits(error, "error") &&
        grepl(
            # Windows reports sharing violations while opening the file, before
            # reaching the POSIX lock path. Some DuckDB builds omit the owner
            # diagnostic, so also recognize Windows' explicit sharing error.
            # Generic file-open, permission and corruption errors stay visible.
            paste0(
                "Could not set lock|Conflicting lock|",
                "Cannot open file[\\s\\S]*(?:File is already open in|",
                "The process cannot access the file because it is being used by another process\\.)"
            ),
            conditionMessage(error),
            ignore.case = TRUE,
            perl = TRUE
        )
}
# }}}

# Serialize the latest run tables after each durable milestone. Keeping only a
# bounded event tail prevents frequent progress snapshots from growing without
# bound during large workflows.
# shift_job__live_snapshot_write {{{
shift_job__live_snapshot_write <- function(
    store,
    run_id,
    event_limit = 200L,
    ui_state = NULL
) {
    wanted_run_id <- run_id
    run <- shift_inspect__rows(store, "shift_run", "run_id", wanted_run_id)
    if (!nrow(run)) {
        return(invisible(NULL))
    }
    cases <- shift_inspect__rows(
        store,
        "shift_run_case",
        "run_id",
        wanted_run_id
    )
    checkmate::assert_count(event_limit, positive = FALSE)
    events <- store$query(sprintf(
        paste(
            "SELECT * FROM (SELECT * FROM shift_run_event WHERE run_id IN (%s)",
            "ORDER BY created_at DESC LIMIT %d) ORDER BY created_at"
        ),
        shift_stage__query_ids(wanted_run_id),
        as.integer(event_limit)
    ))
    jobs <- shift_inspect__rows(store, "shift_run_job", "run_id", wanted_run_id)
    jobs <- jobs[order(jobs[["attempt"]])]
    steps <- shift_inspect__rows(
        store,
        "shift_run_step",
        "run_id",
        wanted_run_id
    )
    steps <- steps[order(steps[["ordinal"]])]
    outputs <- data.table::data.table()
    morph_id <- store__chr1(run$morph_id[[1L]])
    if (!is.na(morph_id) && nzchar(morph_id)) {
        outputs <- shift_inspect__rows(
            store,
            "epw_output",
            "morph_id",
            morph_id
        )
    }
    payload <- list(
        version = 1L,
        run_id = run_id,
        store_path = store$path,
        written_at = store__now(),
        run = run,
        cases = cases,
        events = events,
        jobs = jobs,
        steps = steps,
        outputs = outputs,
        ui_state = ui_state
    )
    store_write_json_atomic(
        payload,
        shift_job__live_path(store$path, run_id),
        auto_unbox = TRUE,
        dataframe = "rows",
        null = "null",
        na = "null",
        POSIXt = "ISO8601",
        digits = 15
    )
    invisible(payload)
}
# }}}

# Parse the ISO timestamps written by jsonlite without allowing base R to
# accept only the date prefix. Keep fractional seconds and explicit offsets.
# shift_job__live_time {{{
shift_job__live_time <- function(x) {
    if (inherits(x, "POSIXt") || inherits(x, "Date")) {
        return(as.POSIXct(x, tz = "UTC"))
    }
    if (is.numeric(x) || is.logical(x)) {
        return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    }
    value <- gsub("T", " ", trimws(as.character(x)), fixed = TRUE)
    value <- sub("Z$", "+0000", value)
    value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value)
    offset <- !is.na(value) & grepl("[+-][0-9]{4}$", value)
    date_only <- !is.na(value) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)
    out <- as.POSIXct(value, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    # Parse offsets separately so mixed offset/no-offset columns stay valid.
    out[offset] <- as.POSIXct(
        value[offset],
        format = "%Y-%m-%d %H:%M:%OS%z",
        tz = "UTC"
    )
    out[date_only] <- as.POSIXct(
        value[date_only],
        format = "%Y-%m-%d",
        tz = "UTC"
    )
    out
}
# }}}

# Normalize JSON rows back to data.table form and restore timestamp columns
# needed by status age calculations and watch rendering.
# shift_job__live_table {{{
shift_job__live_table <- function(x) {
    if (is.null(x) || !length(x)) {
        return(data.table::data.table())
    }
    out <- data.table::as.data.table(x)
    time_columns <- intersect(
        c(
            "started_at",
            "updated_at",
            "completed_at",
            "created_at",
            "heartbeat_at",
            "cancel_requested_at"
        ),
        names(out)
    )
    for (name in time_columns) {
        out[[name]] <- shift_job__live_time(out[[name]])
    }
    out
}
# }}}

# Rebuild the same lightweight ShiftRun shape from a live sidecar when opening
# the manifest fails specifically because the background worker owns its lock.
# shift_job__live_run_get {{{
shift_job__live_run_get <- function(run_id, store_path) {
    path <- shift_job__live_path(store_path, run_id)
    if (!file.exists(path)) {
        return(NULL)
    }
    snapshot <- tryCatch(
        jsonlite::fromJSON(
            path,
            simplifyVector = TRUE,
            simplifyDataFrame = TRUE
        ),
        error = function(e) NULL
    )
    if (
        is.null(snapshot) || !identical(as.character(snapshot$run_id), run_id)
    ) {
        return(NULL)
    }
    row <- shift_job__live_table(snapshot$run)
    cases <- shift_job__live_table(snapshot$cases)
    events <- shift_job__live_table(snapshot$events)
    jobs <- shift_job__live_table(snapshot$jobs)
    steps <- shift_job__live_table(snapshot$steps)
    outputs <- shift_job__live_table(snapshot$outputs)
    ui_state <- shift_stage__coalesce(snapshot$ui_state, list())
    if (!nrow(row)) {
        return(NULL)
    }
    if (nrow(cases) && "years_json" %in% names(cases)) {
        years <- lapply(cases$years_json, function(value) {
            as.integer(jsonlite::fromJSON(value, simplifyVector = TRUE))
        })
        data.table::set(cases, j = "years", value = years)
    }
    diagnostic_events <- events[
        status %in% c("failed", "error", "diagnostic")
    ]
    diagnostics <- if (!nrow(diagnostic_events)) {
        shift_stage__diagnostics_empty()
    } else {
        do.call(
            shift_stage__bind_diagnostics,
            lapply(
                seq_len(nrow(diagnostic_events)),
                function(i) {
                    shift_job__run_event_diagnostic(
                        diagnostic_events[i],
                        run_id,
                        store_path
                    )
                }
            )
        )
    }
    shift_stage__new(
        ShiftRun,
        "run",
        store_path = store_path,
        ids = list(
            run_id = run_id,
            query_id = store__chr1(row$query_id[[1L]]),
            reference_query_id = store__chr1(row$reference_query_id[[1L]]),
            morph_id = store__chr1(row$morph_id[[1L]])
        ),
        meta = list(
            run = row[1L],
            cases = cases,
            events = events,
            jobs = jobs,
            steps = steps,
            outputs = outputs,
            ui_state = ui_state,
            live = TRUE
        ),
        diagnostics = diagnostics
    )
}
# }}}

# Decide whether an atomic live snapshot is safe to serve without opening
# DuckDB. Dead PIDs and launch attempts older than the grace period fall back
# to manifest reconciliation so stale runs still become failed.
# shift_job__live_process_is_active {{{
shift_job__live_process_is_active <- function(run, startup_grace = 60) {
    if (is.null(run) || !S7::S7_inherits(run, ShiftRun)) {
        return(FALSE)
    }
    status <- shift_status(run, refresh = FALSE)
    if (!status %in% c("queued", "running", "stopping")) {
        return(FALSE)
    }
    jobs <- data.table::as.data.table(run@meta$jobs)
    if (!nrow(jobs)) {
        return(FALSE)
    }
    job <- jobs[which.max(jobs[["attempt"]])]
    if (!identical(job$mode[[1L]], "process")) {
        return(FALSE)
    }
    pid <- suppressWarnings(as.integer(job$pid[[1L]]))
    if (!is.na(pid)) {
        return(downloader__pid_alive(pid))
    }
    age <- as.numeric(difftime(
        Sys.time(),
        job$created_at[[1L]],
        units = "secs"
    ))
    is.finite(age) && age <= startup_grace
}
# }}}

# Persist a cooperative cancellation request outside DuckDB so a watcher can
# signal a worker even while the manifest is exclusively locked.
# shift_job__cancel_request_write {{{
shift_job__cancel_request_write <- function(
    store_path,
    run_id,
    job_id,
    force = FALSE
) {
    store_write_json_atomic(
        list(
            run_id = run_id,
            job_id = job_id,
            force = force,
            requested_at = format(
                store__now(),
                "%Y-%m-%dT%H:%M:%OSZ",
                tz = "UTC"
            )
        ),
        shift_job__live_path(store_path, run_id, suffix = "cancel.json"),
        auto_unbox = TRUE,
        null = "null"
    )
}
# }}}

# Reflect cancellation in the lock-free snapshot immediately; the worker will
# subsequently persist the authoritative terminal state in DuckDB.
# shift_job__live_cancel_mark {{{
shift_job__live_cancel_mark <- function(store_path, run_id, job_id, status) {
    path <- shift_job__live_path(store_path, run_id)
    snapshot <- tryCatch(
        jsonlite::fromJSON(path, simplifyDataFrame = TRUE),
        error = function(e) NULL
    )
    if (is.null(snapshot)) {
        return(NULL)
    }
    snapshot$run$status[[1L]] <- status
    snapshot$run$last_error[[1L]] <- "Cancellation requested by user."
    if (length(snapshot$ui_state)) {
        snapshot$ui_state$status <- status
    }
    if (!is.null(snapshot$jobs) && nrow(snapshot$jobs)) {
        hit <- which(snapshot$jobs$job_id %in% job_id)
        if (length(hit)) {
            snapshot$jobs$status[hit] <- status
            snapshot$jobs$last_error[hit] <- "Cancellation requested by user."
            snapshot$jobs$cancel_requested_at[hit] <- format(
                store__now(),
                "%Y-%m-%dT%H:%M:%OSZ",
                tz = "UTC"
            )
        }
    }
    snapshot$written_at <- format(
        store__now(),
        "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
    )
    store_write_json_atomic(
        snapshot,
        path,
        auto_unbox = TRUE,
        dataframe = "rows",
        null = "null",
        na = "null",
        POSIXt = "ISO8601",
        digits = 15
    )
    shift_job__live_run_get(run_id, store_path)
}
# }}}

# Read only cancellation requests for the current attempt; stale markers from
# a previous attempt cannot cancel a resumed job.
# shift_job__cancel_request_exists {{{
shift_job__cancel_request_exists <- function(store_path, run_id, job_id) {
    path <- shift_job__live_path(store_path, run_id, suffix = "cancel.json")
    if (!file.exists(path)) {
        return(FALSE)
    }
    request <- tryCatch(
        jsonlite::fromJSON(path, simplifyVector = TRUE),
        error = function(e) NULL
    )
    !is.null(request) &&
        identical(as.character(request$job_id), as.character(job_id))
}
# }}}

# Update a validated record in place, quoting identifiers and scalar values
# through DuckDB so timestamps, missing values and strings retain their types.
# shift_job__update_row {{{
shift_job__update_row <- function(store, table, key, row, fields) {
    conn <- morpher__private_store(store)$conn
    values <- vapply(
        fields,
        function(field) ddb_literal(conn, row[[field]]),
        character(1L)
    )
    sql <- sprintf(
        "UPDATE %s SET %s WHERE %s = %s",
        ddb_ident(conn, table),
        paste(
            paste(ddb_ident(conn, fields), values, sep = " = "),
            collapse = ", "
        ),
        ddb_ident(conn, key),
        ddb_literal(conn, row[[key]])
    )
    ddb_exec(conn, sql)
    invisible(row)
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
