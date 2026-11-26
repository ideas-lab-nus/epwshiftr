# Preserve the requested detail level as presentation metadata for both target
# types; attributes do not change the machine-readable snapshot fields.
# epwshiftr_cli_shift_show {{{
epwshiftr_cli_shift_show <- function(store, args) {
    req <- cli_shift__parse_target(
        store,
        args,
        flags = c("--verbose", "--debug")
    )
    run <- req$target
    snapshot <- if (S7::S7_inherits(run, ShiftBatch)) {
        out <- shift_batch_ui__snapshot(
            run,
            event_count = Inf,
            refresh = FALSE
        )
        out$explain <- shift_explain(run)
        out
    } else {
        list(
            run = run@meta$run,
            cases = shift_cases(run),
            events = run@meta$events,
            outputs = shift_outputs(run),
            diagnostics = shift_diagnostics(run),
            explain = shift_explain(run)
        )
    }
    attr(snapshot, "shift_ui_detail") <- epwshiftr_cli_shift_detail(req$parsed)
    snapshot
}
# }}}

# cli_shift__read_watch_snapshot {{{
cli_shift__read_watch_snapshot <- function(
    store,
    run_id,
    batch_id = NULL,
    event_count = 10L
) {
    if (is.null(batch_id)) {
        return(epwshiftr_cli_shift_watch_snapshot(
            store,
            run_id = run_id,
            event_count = event_count
        ))
    }
    shift_batch_ui__snapshot(
        shift_batch_get(batch_id, store),
        event_count = event_count,
        refresh = FALSE
    )
}
# }}}

# Route watch presentation so JSON emits one final snapshot, JSONL emits the
# event stream, and neither mode is contaminated by human-readable rendering.
# epwshiftr_cli_shift_watch {{{
epwshiftr_cli_shift_watch <- function(
    store,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--follow", CLI_UI_FLAGS),
        options = c("--run", "--batch", "--interval", "--count", "--events"),
        no_positionals = TRUE
    )
    batch_id <- parsed$options[["--batch"]]
    if (is.null(batch_id) == is.null(parsed$options[["--run"]])) {
        epwshiftr_cli_usage_abort("Supply exactly one of --run or --batch.")
    }
    run_id <- epwshiftr_cli_required_single_id(
        parsed,
        if (is.null(batch_id)) "--run" else "--batch"
    )
    detail <- epwshiftr_cli_shift_detail(parsed)
    progress <- if (isTRUE(parsed$flags[["--no-progress"]])) {
        "none"
    } else if (isTRUE(cli::is_dynamic_tty())) {
        "dynamic"
    } else {
        "log"
    }
    event_count <- epwshiftr_cli_count_or_default(
        parsed$options[["--events"]],
        "--events",
        10L,
        positive = FALSE
    )
    if (isTRUE(parsed$flags[["--follow"]])) {
        snapshot <- epwshiftr_cli_shift_watch_follow(
            store,
            run_id = run_id,
            event_count = event_count,
            interval = epwshiftr_cli_download_interval(
                parsed$options[["--interval"]],
                1
            ),
            count = epwshiftr_cli_count_or_default(
                parsed$options[["--count"]],
                "--count",
                Inf,
                positive = FALSE
            ),
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
    snapshot <- cli_shift__read_watch_snapshot(
        store,
        run_id = run_id,
        batch_id = batch_id,
        event_count = event_count
    )
    attr(snapshot, "shift_ui_detail") <- detail
    snapshot
}
# }}}

# Read one durable run snapshot without reconstructing state from unrelated
# query, extraction, or morph tables.
# epwshiftr_cli_shift_watch_snapshot {{{
epwshiftr_cli_shift_watch_snapshot <- function(
    store,
    run_id,
    event_count = 10L
) {
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
# }}}

# Emit one typed JSONL record for workflow automation. Snapshot, event, gap, and
# terminal records remain self-describing and never contain human progress text.
# epwshiftr_cli_shift_jsonl_record {{{
epwshiftr_cli_shift_jsonl_record <- function(type, ...) {
    epwshiftr_cli_emit_jsonl(c(
        list(
            type = type,
            emitted_at = store__now()
        ),
        list(...)
    ))
}
# }}}

# Select the correct event cursor for single-run and independently polled
# batch histories before any public recent-event limit is applied.
# cli_shift__watch_event_delta {{{
cli_shift__watch_event_delta <- function(
    snapshot,
    cursor,
    initial_limit,
    initial
) {
    events <- shift_stage__coalesce(
        attr(snapshot, "shift_ui_events"),
        snapshot$events
    )
    if (!is.null(snapshot$batch)) {
        shift_batch_ui__event_delta(events, cursor, initial_limit, initial)
    } else {
        shift_ui_state__ui_event_delta(events, cursor, initial_limit, initial)
    }
}
# }}}

# Follow persisted snapshots with shared R/CLI views and lossless event deltas.
# epwshiftr_cli_shift_watch_follow {{{
epwshiftr_cli_shift_watch_follow <- function(
    store,
    run_id,
    event_count = 10L,
    interval = 1,
    count = Inf,
    jsonl = FALSE,
    quiet = FALSE,
    progress = c("dynamic", "log", "none"),
    detail = "normal",
    motion = c("auto", "full", "reduced", "none"),
    batch_id = NULL
) {
    progress <- match.arg(progress)
    motion <- match.arg(motion)
    effective_progress <- if (isTRUE(quiet) || isTRUE(jsonl)) {
        "none"
    } else if (identical(progress, "dynamic")) {
        "dynamic"
    } else {
        "log"
    }
    ui <- shift_ui(
        progress = effective_progress,
        detail = detail,
        motion = motion
    )
    initial_cursor <- if (is.null(batch_id)) NA_character_ else list()
    jsonl_cursor <- initial_cursor
    on_poll <- if (!isTRUE(quiet) && isTRUE(jsonl)) {
        function(snapshot, first, done) {
            delta <- cli_shift__watch_event_delta(
                snapshot,
                cursor = jsonl_cursor,
                initial_limit = event_count,
                initial = isTRUE(first)
            )
            if (isTRUE(first)) {
                epwshiftr_cli_shift_jsonl_record(
                    "snapshot",
                    snapshot = snapshot
                )
            } else {
                if (isTRUE(delta$gap)) {
                    epwshiftr_cli_shift_jsonl_record(
                        "gap",
                        message = "Older workflow events are no longer available in the live buffer."
                    )
                }
                for (j in seq_len(nrow(delta$rows))) {
                    epwshiftr_cli_shift_jsonl_record(
                        "event",
                        event = epwshiftr_cli_row_object(delta$rows, j)
                    )
                }
            }
            jsonl_cursor <<- delta$cursor
            if (isTRUE(done)) {
                epwshiftr_cli_shift_jsonl_record(
                    "terminal",
                    snapshot = snapshot
                )
            }
        }
    }
    tick <- 0
    snapshot <- shift_tui__watch_loop(
        poll = function(polls) {
            cli_shift__read_watch_snapshot(
                store,
                run_id = run_id,
                batch_id = batch_id,
                event_count = event_count
            )
        },
        is_done = function(snapshot, polls) {
            !isTRUE(epwshiftr_cli_shift_watch_active(snapshot))
        },
        render_view = function(snapshot, motion, frame) {
            if (!is.null(snapshot$batch)) {
                return(shift_batch_ui__view(
                    snapshot,
                    detail = detail,
                    motion = motion,
                    frame = frame,
                    height = shift_ui__ui_height()
                ))
            }
            view_events <- shift_stage__coalesce(
                attr(snapshot, "shift_ui_events"),
                snapshot$events
            )
            shift_ui_view__ui_table_view(
                snapshot$run,
                snapshot$cases,
                view_events,
                detail = detail,
                motion = motion,
                frame = frame,
                outputs = snapshot$outputs,
                diagnostics = snapshot$diagnostics,
                ui_state = attr(snapshot, "shift_ui_state")
            )
        },
        print_view = function(snapshot, final = FALSE) {
            epwshiftr_cli_render_shift_watch(snapshot, detail = detail)
        },
        ui = ui,
        interval = interval,
        event_delta = function(snapshot, cursor, initial) {
            cli_shift__watch_event_delta(
                snapshot,
                cursor = cursor,
                initial_limit = event_count,
                initial = initial
            )
        },
        initial_cursor = initial_cursor,
        on_poll = on_poll,
        max_polls = count,
        animate = FALSE,
        now_fn = function() {
            tick <<- tick + max(interval, 1)
            as.POSIXct(tick, origin = "1970-01-01", tz = "UTC")
        }
    )
    if (is.null(snapshot)) {
        return(invisible(NULL))
    }
    attr(snapshot, "shift_ui_detail") <- detail
    structure(snapshot, class = c("epwshiftr_cli_emitted", class(snapshot)))
}
# }}}

# Keep following the coordinator while source work precedes child registration.
# epwshiftr_cli_shift_watch_active {{{
epwshiftr_cli_shift_watch_active <- function(snapshot) {
    active <- c("queued", "running", "stopping")
    if (!is.null(snapshot$batch)) {
        return(
            snapshot$batch$status %in%
                active ||
                any(snapshot$children$status %in% active)
        )
    }
    nrow(snapshot$run) && snapshot$run$status[[1L]] %in% active
}
# }}}

# vim: fdm=marker :
