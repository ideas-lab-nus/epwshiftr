epwshiftr_cli_shift_show <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(args, options = "--run")
    epwshiftr_cli_assert_no_positionals(parsed)
    run <- shift_run_get(epwshiftr_cli_required_single_id(parsed, "--run"), store)
    list(
        run = run@meta$run,
        cases = shift_cases(run),
        events = run@meta$events,
        outputs = shift_outputs(run),
        diagnostics = shift_diagnostics(run),
        explain = shift_explain(run)
    )
}


# Route watch presentation so JSON emits one final snapshot, JSONL emits the
# event stream, and neither mode is contaminated by human-readable rendering.
epwshiftr_cli_shift_watch <- function(store, args, json = FALSE,
                                      jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--follow", "--no-progress", "--verbose", "--debug"),
        options = c("--run", "--interval", "--count", "--events")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    run_id <- epwshiftr_cli_required_single_id(parsed, "--run")
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
            detail = detail
        )
        if (isTRUE(json)) {
            # JSON follow suppresses intermediate snapshots and lets the
            # top-level emitter serialize exactly one terminal document.
            class(snapshot) <- setdiff(class(snapshot), "epwshiftr_cli_emitted")
        }
        return(snapshot)
    }
    snapshot <- epwshiftr_cli_shift_watch_snapshot(
        store, run_id = run_id, event_count = event_count)
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
    snapshot
}


epwshiftr_cli_shift_watch_follow <- function(store, run_id,
                                             event_count = 10L, interval = 1, count = Inf,
                                             jsonl = FALSE, quiet = FALSE,
                                             progress = c("dynamic", "log", "none"),
                                             detail = "normal") {
    progress <- match.arg(progress)
    i <- 0L
    bar_id <- NULL
    last_event_id <- NA_character_
    event_cursor_initialized <- FALSE
    update_dynamic <- function(snapshot) {
        view_events <- shift_coalesce(attr(snapshot, "shift_ui_events"),
            snapshot$events)
        view <- shift__ui_table_view(snapshot$run, snapshot$cases,
            view_events, detail = detail)
        refreshed <- shift__ui_progress_refresh(bar_id, view$lines)
        bar_id <<- refreshed$ids
        refreshed$ok
    }
    close_dynamic <- function(result = "done") {
        if (length(bar_id)) {
            shift__ui_progress_close(bar_id, result = result)
            bar_id <<- character()
        }
        invisible(NULL)
    }
    on.exit(close_dynamic(), add = TRUE)
    repeat {
        i <- i + 1L
        snapshot <- epwshiftr_cli_shift_watch_snapshot(store, run_id = run_id, event_count = event_count)
        if (isTRUE(quiet)) {
            # no output
        } else if (isTRUE(jsonl)) {
            epwshiftr_cli_emit_jsonl(snapshot)
        } else if (identical(progress, "dynamic")) {
            if (!isTRUE(update_dynamic(snapshot))) {
                epwshiftr_cli_render_shift_watch(snapshot, detail = detail)
            }
        } else {
            # Cursor against the complete available history before applying
            # the public tail limit; otherwise a busy interval can silently
            # discard events that arrived between two watch polls.
            all_rows <- shift_coalesce(attr(snapshot, "shift_ui_events"),
                snapshot$events)
            delta <- shift__ui_event_delta(
                all_rows,
                last_event_id = last_event_id,
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
            }
            last_event_id <- delta$cursor
            event_cursor_initialized <- TRUE
        }
        if (!is.infinite(count) && i >= count) {
            break
        }
        if (!epwshiftr_cli_shift_watch_active(snapshot)) {
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
    nrow(snapshot$run) && snapshot$run$status[[1L]] %in% c("queued", "running", "stopping")
}
