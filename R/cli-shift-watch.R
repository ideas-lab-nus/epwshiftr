# Preserve the requested detail level as presentation metadata for both target
# types; attributes do not change the machine-readable snapshot fields.
epwshiftr_cli_shift_show <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = c("--run", "--batch"),
        flags = c("--verbose", "--debug"))
    epwshiftr_cli_assert_no_positionals(parsed)
    run <- cli_shift__target(parsed, store)
    if (S7::S7_inherits(run, ShiftBatch)) {
        snapshot <- shift_batch__snapshot(run, event_count = Inf, refresh = FALSE)
        snapshot$explain <- shift_explain(run)
        attr(snapshot, "shift_ui_detail") <- epwshiftr_cli_shift_detail(parsed)
        return(snapshot)
    }
    snapshot <- list(
        run = run@meta$run,
        cases = shift_cases(run),
        events = run@meta$events,
        outputs = shift_outputs(run),
        diagnostics = shift_diagnostics(run),
        explain = shift_explain(run)
    )
    attr(snapshot, "shift_ui_detail") <- epwshiftr_cli_shift_detail(parsed)
    snapshot
}


# Route watch presentation so JSON emits one final snapshot, JSONL emits the
# event stream, and neither mode is contaminated by human-readable rendering.
epwshiftr_cli_shift_watch <- function(store, args, json = FALSE,
                                      jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--follow", "--no-progress", "--reduced-motion",
            "--verbose", "--debug"),
        options = c("--run", "--batch", "--interval", "--count", "--events")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    batch_id <- parsed$options[["--batch"]]
    if (is.null(batch_id) == is.null(parsed$options[["--run"]])) {
        epwshiftr_cli_usage_abort("Supply exactly one of --run or --batch.")
    }
    run_id <- if (is.null(batch_id)) {
        epwshiftr_cli_required_single_id(parsed, "--run")
    } else {
        epwshiftr_cli_required_single_id(parsed, "--batch")
    }
    detail <- epwshiftr_cli_shift_detail(parsed)
    progress <- if (isTRUE(parsed$flags[["--no-progress"]])) {
        "none"
    } else if (isTRUE(cli::is_dynamic_tty())) {
        "dynamic"
    } else {
        "log"
    }
    event_count <- epwshiftr_cli_count_or_default(parsed$options[["--events"]], "--events", 10L, positive = FALSE)
    if (isTRUE(parsed$flags[["--follow"]])) {
        snapshot <- epwshiftr_cli_shift_watch_follow(
            store,
            run_id = run_id,
            event_count = event_count,
            interval = epwshiftr_cli_download_interval(parsed$options[["--interval"]], 1),
            count = epwshiftr_cli_count_or_default(parsed$options[["--count"]], "--count", Inf, positive = FALSE),
            jsonl = jsonl,
            quiet = isTRUE(quiet) || isTRUE(json),
            progress = progress,
            detail = detail,
            motion = epwshiftr_cli_shift_motion(parsed),
            batch_id = batch_id
        )
        if (isTRUE(json)) {
            # JSON follow suppresses intermediate snapshots and lets the
            # top-level emitter serialize exactly one terminal document.
            class(snapshot) <- setdiff(class(snapshot), "epwshiftr_cli_emitted")
        }
        return(snapshot)
    }
    snapshot <- if (is.null(batch_id)) {
        epwshiftr_cli_shift_watch_snapshot(
            store, run_id = run_id, event_count = event_count)
    } else {
        shift_batch__snapshot(shift_batch_get(batch_id, store),
            event_count = event_count, refresh = FALSE)
    }
    attr(snapshot, "shift_ui_detail") <- detail
    snapshot
}


# Read one durable run snapshot without reconstructing state from unrelated
# query, extraction, or morph tables.
epwshiftr_cli_shift_watch_snapshot <- function(store, run_id, event_count = 10L) {
    run <- shift_run_get(run_id, store)
    all_events <- run@meta$events
    events <- all_events
    if (nrow(events) > event_count) {
        events <- utils::tail(events, event_count)
    }
    snapshot <- list(
        run = run@meta$run,
        cases = shift_cases(run),
        outputs = shift_outputs(run),
        diagnostics = shift_diagnostics(run),
        events = events
    )
    # Preserve the full event history only as renderer metadata; JSON/JSONL
    # contracts continue to expose the requested recent-event count.
    attr(snapshot, "shift_ui_events") <- all_events
    attr(snapshot, "shift_ui_state") <- run@meta$ui_state
    snapshot
}


# Emit one typed JSONL record for workflow automation. Snapshot, event, gap, and
# terminal records remain self-describing and never contain human progress text.
epwshiftr_cli_shift_jsonl_record <- function(type, ...) {
    epwshiftr_cli_emit_jsonl(c(list(
        type = type,
        emitted_at = store__now()
    ), list(...)))
}


# Select the correct event cursor for single-run and independently polled
# batch histories before any public recent-event limit is applied.
cli_shift__watch_event_delta <- function(snapshot, cursor, initial_limit, initial) {
    events <- shift_coalesce(attr(snapshot, "shift_ui_events"), snapshot$events)
    if (!is.null(snapshot$batch)) {
        shift_batch__event_delta(events, cursor, initial_limit, initial)
    } else {
        shift__ui_event_delta(events, cursor, initial_limit, initial)
    }
}

# Follow persisted snapshots with shared R/CLI views and lossless event deltas.
epwshiftr_cli_shift_watch_follow <- function(store, run_id,
                                             event_count = 10L, interval = 1, count = Inf,
                                             jsonl = FALSE, quiet = FALSE,
                                             progress = c("dynamic", "log", "none"),
                                             detail = "normal",
                                             motion = c("auto", "full", "reduced", "none"),
                                             batch_id = NULL) {
    progress <- match.arg(progress)
    motion <- match.arg(motion)
    ui <- shift_ui(progress = progress, detail = detail, motion = motion)
    motion <- shift__ui_motion(ui, progress)
    i <- 0L
    frame <- 0L
    renderer <- tryCatch(shift__ui_renderer(progress), error = function(e) NULL)
    if (identical(progress, "dynamic") && is.null(renderer)) {
        progress <- "log"
        motion <- "none"
    }
    event_cursor <- if (is.null(batch_id)) NA_character_ else list()
    event_cursor_initialized <- FALSE
    update_dynamic <- function(snapshot) {
        if (!is.null(snapshot$batch)) {
            view <- shift_batch__view(snapshot, detail = detail,
                motion = motion, frame = frame, height = shift__ui_height())
        } else {
            view_events <- shift_coalesce(attr(snapshot, "shift_ui_events"),
                snapshot$events)
            view <- shift__ui_table_view(snapshot$run, snapshot$cases,
                view_events, detail = detail, motion = motion, frame = frame,
                outputs = snapshot$outputs, diagnostics = snapshot$diagnostics,
                ui_state = attr(snapshot, "shift_ui_state"))
        }
        ok <- !is.null(renderer) &&
            isTRUE(renderer$draw(view$lines, compact = view$compact))
        if (!isTRUE(ok)) {
            if (!is.null(renderer)) renderer$close(result = "failed")
            renderer <<- NULL
            progress <<- "log"
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
    repeat {
        i <- i + 1L
        frame <- frame + 1L
        snapshot <- if (is.null(batch_id)) {
            epwshiftr_cli_shift_watch_snapshot(store, run_id = run_id,
                event_count = event_count)
        } else {
            shift_batch__snapshot(shift_batch_get(batch_id, store),
                event_count = event_count, refresh = FALSE)
        }
        active <- epwshiftr_cli_shift_watch_active(snapshot)
        if (isTRUE(quiet)) {
            # no output
        } else if (isTRUE(jsonl)) {
            delta <- cli_shift__watch_event_delta(
                snapshot,
                cursor = event_cursor,
                initial_limit = event_count,
                initial = !event_cursor_initialized
            )
            if (!isTRUE(event_cursor_initialized)) {
                epwshiftr_cli_shift_jsonl_record("snapshot", snapshot = snapshot)
            } else {
                if (isTRUE(delta$gap)) {
                    epwshiftr_cli_shift_jsonl_record("gap",
                        message = "Older workflow events are no longer available in the live buffer.")
                }
                for (j in seq_len(nrow(delta$rows))) {
                    epwshiftr_cli_shift_jsonl_record("event",
                        event = epwshiftr_cli_row_object(delta$rows, j))
                }
            }
            event_cursor <- delta$cursor
            event_cursor_initialized <- TRUE
            if (!isTRUE(active)) {
                epwshiftr_cli_shift_jsonl_record("terminal", snapshot = snapshot)
            }
        } else if (identical(progress, "dynamic")) {
            if (!isTRUE(update_dynamic(snapshot))) {
                epwshiftr_cli_render_shift_watch(snapshot, detail = detail)
            }
        } else {
            # Cursor against the complete available history before applying
            # the public tail limit; otherwise a busy interval can silently
            # discard events that arrived between two watch polls.
            delta <- cli_shift__watch_event_delta(
                snapshot,
                cursor = event_cursor,
                initial_limit = event_count,
                initial = !event_cursor_initialized
            )
            rows <- delta$rows
            if (i == 1L) {
                epwshiftr_cli_render_shift_watch(snapshot, detail = detail)
            } else {
                if (isTRUE(delta$gap)) {
                    cli::cli_alert_info(paste(
                        "Some older workflow events are no longer available",
                        "in the live buffer; continuing from its oldest event."
                    ))
                }
                for (j in seq_len(nrow(rows))) {
                    cli::cli_text("{shift__ui_persisted_event_line(rows[j], detail = detail)}")
                }
                if (!isTRUE(active)) {
                    epwshiftr_cli_render_shift_watch(snapshot, detail = detail)
                }
            }
            event_cursor <- delta$cursor
            event_cursor_initialized <- TRUE
        }
        if ((!is.infinite(count) && i >= count) || !isTRUE(active)) {
            if (identical(progress, "dynamic") && !isTRUE(quiet) && !isTRUE(jsonl)) {
                close_dynamic()
                epwshiftr_cli_render_shift_watch(snapshot, detail = detail)
            }
            break
        }
        Sys.sleep(interval)
    }
    attr(snapshot, "shift_ui_detail") <- detail
    structure(snapshot, class = c("epwshiftr_cli_emitted", class(snapshot)))
}


epwshiftr_cli_shift_watch_active <- function(snapshot) {
    if (!is.null(snapshot$batch)) {
        return(any(snapshot$children$status %in% c("queued", "running", "stopping")))
    }
    nrow(snapshot$run) && snapshot$run$status[[1L]] %in% c("queued", "running", "stopping")
}
