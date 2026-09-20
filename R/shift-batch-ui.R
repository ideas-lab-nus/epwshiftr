# Batch presentation ---------------------------------------------------------

# Own a single live region for the complete model-discovery operation. Nested
# public catalog calls inherit this reporter and therefore do not create or
# commit separate standalone operation panels.
shift_batch__discover_models <- function(climate, transforms, periods,
                                        references, store, ui, site = NULL) {
    target <- if (length(climate@model)) length(climate@model) else climate@n_models
    ui@batch_context <- list(kind = "discovery", total = length(transforms),
        target_models = target, site = site, scenarios = climate@scenarios,
        periods = shift__ui_periods(periods))
    reporter <- shift__reporter(ui)
    on.exit(reporter$close(), add = TRUE)
    reporter$operation_started("discovery", "Discover CMIP6 models",
        context = list(items = c(site,
            if (is.null(target)) "all compatible GCMs" else paste(target, "GCMs requested"),
            paste(climate@scenarios, collapse = " + "), shift__ui_periods(periods))))
    tryCatch({
        result <- shift__with_reporter(reporter,
            shift_batch__discover_candidates(climate, transforms, periods,
                references, store, ui))
        reporter$discovery_updated(list(selected_models = result$identities$source_id))
        reporter$operation_completed(sprintf(
            "%d GCM(s) selected with complete coverage across all %d method(s)",
            nrow(result$identities), length(transforms)))
        result
    }, error = function(error) {
        reporter$operation_failed(conditionMessage(error))
        stop(error)
    }, interrupt = function(error) {
        reporter$operation_failed("Model discovery interrupted.", cancelled = TRUE)
        stop(error)
    })
}

# Update only a discovery-owned reporter, leaving ordinary single-run resolver
# presentation untouched when the same coverage helpers are reused elsewhere.
shift_batch__discovery_update <- function(context, reset = FALSE) {
    reporter <- shift__current_reporter()
    if (!is.null(reporter) &&
        identical(reporter$ui()@batch_context$kind, "discovery")) {
        reporter$discovery_updated(context, reset = reset)
    }
    invisible(NULL)
}

# Keep completed searches and real failover reasons in the bounded activity
# feed, with durable one-line milestones for append-only consoles.
shift_batch__discovery_notice <- function(message, outcome = "completed") {
    reporter <- shift__current_reporter()
    if (!is.null(reporter) &&
        identical(reporter$ui()@batch_context$kind, "discovery")) {
        reporter$notice(message, outcome = outcome)
    }
    invisible(NULL)
}

# Name humidity alternatives in user terms; the full input contract belongs to
# detail/debug views, while other recipes retain an honest variable count.
shift_batch__discovery_variables <- function(variables, detail = "normal") {
    if (!length(variables)) return(NULL)
    if (!identical(detail, "normal")) return(paste(variables, collapse = ", "))
    humidity <- if ("hurs" %in% variables) {
        "relative humidity (hurs)"
    } else if (all(c("huss", "ps") %in% variables)) {
        "specific humidity + pressure (huss + ps)"
    } else NULL
    paste(c(paste(length(variables), "variables"), humidity), collapse = " \u00b7 ")
}

# Render discovery around the requested matrix and current search scope. Method
# and alternative ordinals are deliberately never converted to percentages.
shift_batch__discovery_lines <- function(state, width, motion, frame) {
    batch <- state$batch_context
    status <- shift_coalesce(state$status, "running")
    terminal <- status %in% c("completed", "failed", "cancelled")
    panel <- width >= 60L
    outer <- shift__ui_dashboard_width(width)
    inner <- if (panel) outer - 4L else outer
    # Wrap facts with the shared display-width-aware label formatter.
    row <- function(label, value) {
        if (!length(value)) return(character())
        shift__ui_labeled_lines(label, paste(value, collapse = " \u00b7 "), inner)
    }
    title <- paste(cli::style_bold("Discover CMIP6 models"),
        shift__ui_status_style(status),
        cli::style_dim(shift__format_elapsed(state$elapsed_seconds)))
    plan <- c(row("Site", batch$site), row("Target", c(
        if (is.null(batch$target_models)) "all compatible GCMs" else
            paste(batch$target_models, "GCMs"),
        paste(length(batch$scenarios), "scenarios"),
        paste(batch$total, "methods"))),
        row("Scenarios", batch$scenarios), row("Periods", batch$periods))
    current <- character()
    if (!terminal) {
        current <- c(row("Method", if (!is.null(batch$current)) sprintf(
            "%d/%d: %s", batch$current, batch$total, batch$method_label)),
            row("Inputs", c(if (!is.null(batch$alternative)) sprintf(
                "combination %d/%d", batch$alternative, batch$alternatives),
                shift_batch__discovery_variables(batch$variables,
                    shift_coalesce(state$detail, "normal")))),
            row("Check", c(batch$scope, batch$scope_periods)),
            row("Node", if (!is.null(batch$node)) c(shift__node_label(batch$node),
                if (isTRUE(batch$node_index > 1L)) sprintf(
                    "fallback node %d/%d", batch$node_index, batch$node_total))),
            row("Now", c(shift__ui_state_symbol("running", motion, frame),
                shift_coalesce(state$unit_label, state$stage_message))),
            shift__ui_query_lines(state, inner),
            row("Cancel", if (interactive()) "Interrupt R to cancel" else "Ctrl+C to cancel"))
    }
    result <- if (identical(status, "completed")) {
        c(row("Summary", state$result_summary),
            row("Models", batch$selected_models))
    } else if (terminal) {
        shift__ui_failure_lines(state, inner)
    } else {
        c(row("Common", if (is.null(batch$common_models))
            "Pending checks across all methods" else paste(batch$common_models, "compatible GCMs")),
            shift__ui_recent_lines(state, inner))
    }
    # On small consoles reserve the viewport for the live query. Older
    # milestones yield to the newest check; failure receipts remain complete.
    chrome <- if (panel) 4L else 1L
    if (!terminal && length(plan) + length(current) + length(result) + chrome > shift__ui_height()) {
        recent <- state
        recent$recent_events <- utils::tail(state$recent_events, 1L)
        recent$recent_outcomes <- utils::tail(state$recent_outcomes, 1L)
        result <- c(row("Common", if (is.null(batch$common_models))
            "Pending checks across all methods" else paste(batch$common_models, "compatible GCMs")),
            shift__ui_recent_lines(recent, inner))
    }
    if (!panel) return(c(shift__ui_fit(title, outer), plan, current, result))
    c(shift__ui_panel_rule(title, outer, "top"),
        vapply(plan, shift__ui_panel_line, character(1L), width = outer),
        if (length(current)) c(
            shift__ui_panel_rule(cli::style_bold("Current search"), outer, "middle"),
            vapply(current, shift__ui_panel_line, character(1L), width = outer)),
        shift__ui_panel_rule(cli::style_bold(if (terminal) "Results" else "Recent checks"),
            outer, "middle"),
        vapply(result, shift__ui_panel_line, character(1L), width = outer),
        shift__ui_panel_rule(width = outer, kind = "bottom"))
}

# Read each child once per snapshot and retain its identity in every table.
# Case counts and physical EPW file counts remain separate for multi-year output.
shift_batch__snapshot <- function(x, event_count = 10L, refresh = TRUE) {
    if (isTRUE(refresh)) {
        x <- shift_batch__refresh(x)
    }
    children <- data.table::copy(x@meta$manifest)
    statuses <- vapply(x@meta$children, function(child) {
        shift_status(child, refresh = FALSE)
    }, character(1L))
    children[, status := statuses]
    data.table::set(children, j = "run_id", value = vapply(x@meta$children, function(child) {
        store__chr1(child@ids$run_id)
    }, character(1L)))
    data.table::set(children, j = "current_stage", value = vapply(x@meta$children, function(child) {
        store__chr1(child@meta$run$current_stage)
    }, character(1L)))
    # Reuse the same live state as single-run watch. Older stores can still
    # reconstruct it from durable events; planned children have no activity yet.
    activity <- lapply(x@meta$children, function(child) {
        if (!S7::S7_inherits(child, ShiftRun)) return(list())
        state <- shift__ui_live_state(child@meta$run,
            shift__ui_table_state(child@meta$run, child@meta$events,
                child@meta$cases), child@meta$ui_state)
        stamps <- c(child@meta$run$updated_at, child@meta$jobs$heartbeat_at)
        stamps <- stamps[!is.na(stamps)]
        state$updated_at <- if (length(stamps)) as.character(max(stamps)) else NA_character_
        state
    })
    names(activity) <- children$child_key
    cases <- shift_cases(x, refresh = FALSE)
    outputs <- shift_outputs(x, refresh = FALSE)
    diagnostics <- shift_diagnostics(x, refresh = FALSE)
    data.table::set(children, j = "method_status", value = vapply(seq_len(nrow(children)), function(index) {
        record <- transform__record(children$scale[[index]], children$method[[index]])
        recipe__get(record$recipe)@status
    }, character(1L)))
    events <- shift_batch__inspect(x@meta$children, x@meta$manifest,
        function(child) {
            data.table::as.data.table(shift_coalesce(
                child@meta$events, data.table::data.table()))
        })
    if (nrow(events)) {
        data.table::setorderv(events,
            intersect(c("created_at", "event_id"), names(events)))
    }
    starts <- vapply(x@meta$children, function(child) {
        value <- child@meta$run$started_at
        if (length(value)) as.numeric(value[[1L]]) else NA_real_
    }, numeric(1L))
    ends <- vapply(x@meta$children, function(child) {
        value <- child@meta$run$completed_at
        if (!length(value) || is.na(value[[1L]])) value <- child@meta$run$updated_at
        if (length(value)) as.numeric(value[[1L]]) else NA_real_
    }, numeric(1L))
    active <- any(statuses %in% c("queued", "running", "stopping"))
    elapsed <- if (any(is.finite(starts)) &&
        (active || any(is.finite(ends)))) {
        end <- if (active) as.numeric(Sys.time()) else max(ends, na.rm = TRUE)
        max(0, end - min(starts, na.rm = TRUE))
    } else {
        0
    }
    summary <- data.table::data.table(
        batch_id = x@ids$batch_id,
        status = shift_status(x, refresh = FALSE),
        methods = data.table::uniqueN(children$method),
        configurations = data.table::uniqueN(children[, c("method", "scale", "reconstruction"), with = FALSE]),
        models = data.table::uniqueN(children$model),
        children = nrow(children),
        completed = sum(statuses == "completed"),
        active = sum(statuses %in% c("queued", "running", "stopping")),
        failed = sum(statuses %in% c("failed", "blocked")),
        partial = sum(statuses == "partial"),
        waiting = sum(statuses %in% c("planned", "waiting")),
        cancelled = sum(statuses == "cancelled"),
        cases = nrow(cases),
        epw_files = nrow(outputs),
        warnings = sum(diagnostics$severity == "warning"),
        elapsed_seconds = elapsed,
        output_dir = x@meta$output_dir
    )
    result <- list(batch = summary, children = children, cases = cases,
        outputs = outputs, diagnostics = diagnostics,
        events = if (is.infinite(event_count)) events else utils::tail(events, event_count),
        call_elapsed_seconds = x@meta$call_elapsed_seconds,
        execution = shift_coalesce(x@meta$execution, data.table::data.table()),
        activity = activity)
    attr(result, "shift_ui_events") <- events
    result
}

# Render errors before warnings, preserving recovery actions and reporting
# omissions. Static detail views retain every diagnostic without truncation.
shift_batch__diagnostic_lines <- function(snapshot, width, limit = 3L, compact = FALSE) {
    rows <- data.table::copy(snapshot$diagnostics)
    rows <- rows[rows$severity %in% c("error", "warning")]
    if (!nrow(rows)) return(character())
    rows <- rows[order(match(rows$severity, c("error", "warning")))]
    shown <- min(nrow(rows), limit)
    # Compact fields consume exactly one row for deterministic height budgets.
    field <- function(label, value) {
        if (compact) shift__ui_fit(shift__ui_labeled_line(label, value), width) else
            shift__ui_labeled_lines(label, value, width)
    }
    lines <- character()
    for (index in seq_len(shown)) {
        row <- rows[index]
        lines <- c(lines, field(if (identical(row$severity, "error")) "Failure" else "Warning",
            paste(c(row$method, row$model, row$message), collapse = " \u00b7 ")))
        action <- store__chr1(row$action)
        if (!is.na(action) && nzchar(action)) lines <- c(lines, field("Action", action))
    }
    if (shown < nrow(rows)) lines <- c(lines, field("More", sprintf(
        "%d more diagnostics; use shift diagnostics", nrow(rows) - shown)))
    lines
}

# Build a child's identity and activity from the same live state as single-run
# watch. Full detail includes its latest event, timestamps, and durable run ID.
shift_batch__child_lines <- function(child, index, state, width, detail,
                                     compact, motion, frame) {
    # Compact fields do not wrap beyond the dynamic viewport budget.
    field <- function(label, value) {
        if (compact) shift__ui_fit(shift__ui_labeled_line(label, value), width) else
            shift__ui_labeled_lines(label, value, width)
    }
    identity <- c(child$method, child$model)
    identity <- identity[!is.na(identity) & nzchar(identity)]
    label <- sprintf("%s %s [%s] \u00b7 %s",
        shift__ui_state_symbol(child$status, motion, frame),
        cli::style_bold(paste(identity, collapse = " / ")),
        child$method_status, child$status)
    configuration <- c(child$scale, child$reconstruction)
    if (!is.na(child$current_stage)) configuration <- c(configuration,
        shift__ui_stage_label(child$current_stage))
    configuration <- configuration[!is.na(configuration) & nzchar(configuration)]
    lines <- c(field(sprintf("#%d", index), label),
        field("", cli::style_dim(paste(configuration, collapse = " \u00b7 "))))
    active <- child$status %in% c("queued", "running", "stopping")
    if (length(state) && (active || !identical(detail, "normal"))) {
        current <- shift_coalesce(state$unit_label, state$stage_message)
        if (!is.null(current)) lines <- c(lines, field(if (active) "Now" else "Last", current))
        metric <- shift__ui_metric_line(state, width)
        lines <- c(lines, if (compact) utils::head(metric, 1L) else metric)
        if (!is.null(state$last_event) && !identical(detail, "normal")) {
            lines <- c(lines, field("Event", state$last_event))
        }
        updated <- store__chr1(state$updated_at)
        lines <- c(lines, field("Updated", paste(c(
            if (!is.na(updated)) updated,
            paste(shift__format_elapsed(shift_coalesce(state$elapsed_seconds, 0)),
                "elapsed")), collapse = " \u00b7 ")))
    }
    if (!identical(detail, "normal")) {
        values <- c(child$member, child$grid, child$run_id)
        values <- values[!is.na(values) & nzchar(values)]
        lines <- c(lines, field("Identity", paste(values, collapse = " \u00b7 ")))
    }
    lines
}

# Keep the shared boxes in static receipts and dynamic viewports. Only dynamic
# callers supply a finite height; static detail always preserves complete data.
shift_batch__view <- function(snapshot, width = shift__ui_width(),
                              detail = "normal", motion = "none", frame = 0L,
                              height = Inf) {
    summary <- snapshot$batch
    terminal_width <- shift__ui_width(width)
    width <- shift__ui_dashboard_width(terminal_width)
    panel <- terminal_width >= 60L
    compact <- is.finite(height)
    content_width <- if (panel) max(1L, width - 4L) else width
    # Preserve long paths and identifiers outside the finite dynamic viewport.
    field <- function(label, value) {
        if (compact) shift__ui_fit(shift__ui_labeled_line(label, value), content_width) else
            shift__ui_labeled_lines(label, value, content_width)
    }
    header <- paste(cli::style_bold("Future EPW Batch"),
        shift__ui_status_style(summary$status), sep = "  ")
    counts <- c(completed = summary$completed, active = summary$active,
        failed = summary$failed, partial = summary$partial, waiting = summary$waiting,
        cancelled = shift_coalesce(summary$cancelled, 0L))
    if (compact) counts <- counts[counts > 0 | names(counts) %in% c("completed", "active")]
    overview <- c(field("Batch", summary$batch_id),
        field("Matrix", sprintf("%d configurations \u00b7 %d models \u00b7 %d children \u00b7 %d cases",
            summary$configurations, summary$models, summary$children, summary$cases)),
        field("Status", paste(sprintf("%d %s", counts, names(counts)), collapse = " \u00b7 ")))
    children <- snapshot$children
    groups <- lapply(seq_len(nrow(children)), function(index) {
        child <- children[index]
        rows <- shift_batch__child_lines(child, index, snapshot$activity[[child$child_key]],
            content_width, detail, compact, motion, frame)
        if (!identical(detail, "normal") && nrow(snapshot$outputs)) {
            completion <- shift__ui_completion(data.table::data.table(),
                snapshot$outputs[snapshot$outputs$child_key == child$child_key],
                shift_diagnostics_empty())
            if (!is.null(completion$field_summary)) rows <- c(rows,
                field("Fields", completion$field_summary))
        }
        rows
    })
    results <- c(field("Summary", sprintf("%d EPW files \u00b7 %d warnings",
        summary$epw_files, summary$warnings)),
        shift_batch__diagnostic_lines(snapshot, content_width,
            limit = if (compact) 1L else if (identical(detail, "normal")) 3L else Inf,
            compact = compact))
    execution <- snapshot$execution
    if (nrow(execution)) results <- c(results, field("Call", sprintf(
        "%d children reused \u00b7 %d started/resumed \u00b7 %s",
        sum(execution$action == "reused"), sum(execution$action != "reused"),
        shift__format_elapsed(shift_coalesce(snapshot$call_elapsed_seconds,
            sum(execution$elapsed_seconds, na.rm = TRUE))))))
    results <- c(results, field("Output", summary$output_dir))
    omitted <- 0L
    shortened <- FALSE
    if (compact) {
        # Failed and active work stays visible as completed children accumulate.
        priority <- match(children$status, c("failed", "blocked", "running",
            "stopping", "queued", "partial", "cancelled", "planned", "waiting", "completed"))
        indices <- order(priority, seq_len(nrow(children)), na.last = TRUE)
        # Reserve visibility for both categories even when many failures exist.
        focus <- c(utils::head(which(children$status %in% c("failed", "blocked")), 1L),
            utils::head(which(children$status %in% c("running", "stopping", "queued")), 1L))
        indices <- unique(c(focus, indices))
        overhead <- (if (panel) 4L else 1L) + length(overview) + length(results)
        remaining <- max(0L, height - overhead - 1L)
        selected <- list()
        # Reserve identity/configuration rows for both focus categories before
        # letting the first child's detail consume the available viewport.
        focus_rows <- if (remaining >= 2L * length(focus)) 2L else 1L
        for (position in seq_along(indices)) {
            index <- indices[[position]]
            reserved <- sum(match(focus, indices) > position) * focus_rows
            minimum <- if (index %in% focus) focus_rows else 2L
            if (remaining - reserved < minimum) break
            keep <- min(length(groups[[index]]), remaining - reserved)
            shortened <- shortened || keep < length(groups[[index]])
            selected[[length(selected) + 1L]] <- utils::head(groups[[index]], keep)
            remaining <- remaining - keep
        }
        omitted <- length(groups) - length(selected)
        groups <- selected
    }
    workflows <- unlist(groups, use.names = FALSE)
    if (omitted > 0L) workflows <- c(workflows, field("More", sprintf(
        "%d children hidden; use shift show --verbose", omitted)))
    if (!omitted && shortened) workflows <- c(workflows,
        field("More", "Activity shortened; use shift show --verbose"))
    diagnosis <- summary$status %in% c("failed", "blocked", "cancelled", "partial")
    lines <- if (panel) c(
        shift__ui_panel_rule(header, width, "top"),
        vapply(overview, shift__ui_panel_line, character(1L), width = width),
        shift__ui_panel_rule(cli::style_bold("Workflows"), width, "middle"),
        vapply(workflows, shift__ui_panel_line, character(1L), width = width),
        shift__ui_panel_rule(cli::style_bold(if (diagnosis) "Diagnosis" else "Results"), width, "middle"),
        vapply(results, shift__ui_panel_line, character(1L), width = width),
        shift__ui_panel_rule(width = width, kind = "bottom")
    ) else c(shift__ui_wrap_lines(header, width), overview, workflows, results)
    compact_line <- shift__ui_fit(sprintf(
        "Batch %s \u00b7 %d/%d children complete \u00b7 %d EPW files \u00b7 %d warnings",
        summary$status, summary$completed, summary$children,
        summary$epw_files, summary$warnings), width)
    # Tiny panes keep status and the most severe diagnosis without unsafe
    # cursor movement; the full static receipt remains available on exit.
    if (compact && length(lines) > height) {
        body <- c(compact_line, shift_batch__diagnostic_lines(snapshot,
            content_width, limit = 1L, compact = TRUE),
            field("More", "Full details: shift show --verbose"))
        if (panel && height >= 4L) {
            lines <- c(shift__ui_panel_rule(header, width, "top"),
                vapply(utils::head(body, height - 2L), shift__ui_panel_line,
                    character(1L), width = width),
                shift__ui_panel_rule(width = width, kind = "bottom"))
        } else lines <- utils::head(body, height)
    }
    list(lines = lines, compact = compact_line, nodes = character(),
        cases = if (!identical(detail, "normal")) {
            shift__ui_case_table(snapshot$cases, width, detail)
        } else character())
}

# Emit a durable receipt in human modes while leaving JSON and quiet callers
# free of reporter text. Foreground child frames carry the live batch context.
shift_batch__report <- function(x, ui) {
    if (isTRUE(ui@batch_receipt) && !identical(shift__ui_mode(ui), "none")) {
        shift__ui_print_view(shift_batch__view(
            shift_batch__snapshot(x, refresh = FALSE), detail = ui@detail))
    }
    invisible(x)
}

# Track observed IDs per child run, because polling independent workers can
# insert an older event anywhere in the merged chronological display order.
shift_batch__event_delta <- function(events, cursor = list(),
                                     initial_limit = 10L, initial = FALSE) {
    events <- data.table::as.data.table(events)
    checkmate::assert_count(initial_limit, positive = FALSE)
    checkmate::assert_flag(initial)
    fresh <- rep(FALSE, nrow(events))
    gap <- FALSE
    keys <- shift_coalesce(events$run_id, shift_coalesce(events$child_key,
        rep("batch", nrow(events))))
    keys <- as.character(keys)
    keys[is.na(keys) | !nzchar(keys)] <- "batch"
    for (key in unique(keys)) {
        positions <- which(keys %in% key)
        ids <- as.character(events$event_id[positions])
        seen <- cursor[[key]]
        fresh[positions] <- !ids %in% seen
        # No overlap in an already observed child indicates that its bounded
        # live buffer discarded events. New children are not history gaps.
        if (!initial && length(seen) && !any(ids %in% seen)) gap <- TRUE
        cursor[[key]] <- union(seen, ids)
    }
    rows <- if (initial) {
        if (initial_limit == 0L) events[0] else utils::tail(events, initial_limit)
    } else events[fresh]
    list(rows = rows, cursor = cursor, gap = gap)
}

# Watch all independently running children; one failure never ends observation
# while another child is queued, running, or stopping.
shift_batch__watch <- function(x, follow, interval, events, ui) {
    mode <- shift__ui_mode(ui)
    motion <- shift__ui_motion(ui, mode)
    renderer <- tryCatch(shift__ui_renderer(mode), error = function(error) NULL)
    if (identical(mode, "dynamic") && is.null(renderer)) mode <- "log"
    on.exit(if (!is.null(renderer)) renderer$close(), add = TRUE)
    frame <- 0L
    first <- TRUE
    last_poll <- as.POSIXct(NA)
    cursor <- list()
    snapshot <- NULL
    frame_interval <- if (identical(mode, "dynamic") && identical(motion, "full")) {
        ui@refresh
    } else if (identical(mode, "dynamic") && identical(motion, "reduced")) {
        max(1, ui@refresh)
    } else interval
    tryCatch(repeat {
        now <- shift__watch_now()
        poll_due <- first || is.na(last_poll) ||
            as.numeric(difftime(now, last_poll, units = "secs")) >= interval
        if (poll_due) {
            x <- shift_batch__refresh(x)
            snapshot <- shift_batch__snapshot(x, event_count = events, refresh = FALSE)
            last_poll <- now
        }
        done <- !follow || snapshot$batch$active == 0L
        if (identical(mode, "log") && poll_due) {
            delta <- shift_batch__event_delta(shift_coalesce(
                attr(snapshot, "shift_ui_events"), snapshot$events),
                cursor = cursor, initial_limit = events, initial = first)
            # Emit newly observed milestones even on the terminal poll, whose
            # final receipt may include only a short tail of the event history.
            if (!first) {
                if (delta$gap) cli::cli_alert_info("Older batch events are no longer available.")
                for (index in seq_len(nrow(delta$rows))) cli::cli_text(
                    "{shift__ui_persisted_event_line(delta$rows[index], detail = ui@detail)}")
            }
            cursor <- delta$cursor
        }
        if (done) {
            # Clear the bounded live view, then leave a complete static receipt
            # in scrollback, including when the caller assigns the return value.
            if (!is.null(renderer)) renderer$close()
            if (!identical(mode, "none")) shift__ui_print_view(
                shift_batch__view(snapshot, detail = ui@detail), include_tables = TRUE)
            break
        }
        frame <- frame + 1L
        if (identical(mode, "dynamic")) {
            view <- shift_batch__view(snapshot, detail = ui@detail,
                motion = motion, frame = frame, height = shift__ui_height())
            if (!isTRUE(renderer$draw(view$lines, compact = view$compact))) {
                renderer$close(result = "failed")
                renderer <- NULL
                mode <- "log"
                first <- TRUE
            }
        }
        if (identical(mode, "log") && poll_due) {
            if (first) {
                shift__ui_print_view(shift_batch__view(snapshot, detail = ui@detail))
                # A failed dynamic renderer may have switched to log mode
                # after this poll's cursor step. Seed it from the shown frame.
                cursor <- shift_batch__event_delta(shift_coalesce(
                    attr(snapshot, "shift_ui_events"), snapshot$events),
                    cursor = cursor, initial_limit = events, initial = TRUE)$cursor
            }
        }
        first <- FALSE
        shift__watch_sleep(frame_interval)
    }, interrupt = function(error) {
        if (!is.null(renderer)) renderer$close(result = "cancelled")
        if (!identical(mode, "none")) cli::cli_alert_info(
            "Stopped watching; batch children continue. Use shift_cancel() to cancel them.")
    })
    x
}
