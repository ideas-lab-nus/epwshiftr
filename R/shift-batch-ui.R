# Batch presentation ---------------------------------------------------------

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
        execution = shift_coalesce(x@meta$execution, data.table::data.table()))
    attr(result, "shift_ui_events") <- events
    result
}

# Render batch identity, child workflows, and results inside the same panel
# rules as single runs. Narrow terminals retain the content without borders.
shift_batch__view <- function(snapshot, width = shift__ui_width(),
                              detail = "normal", motion = "none", frame = 0L) {
    summary <- snapshot$batch
    terminal_width <- shift__ui_width(width)
    width <- shift__ui_dashboard_width(terminal_width)
    panel <- terminal_width >= 60L
    # Reserve the real border/padding width before wrapping any value. Long
    # batch IDs, model names, and paths must remain complete inside the box.
    content_width <- if (isTRUE(panel)) max(1L, width - 4L) else width
    header <- paste(cli::style_bold("Future EPW Batch"),
        shift__ui_status_style(summary$status), sep = "  ")
    overview <- c(
        shift__ui_labeled_lines("Batch", summary$batch_id, content_width),
        shift__ui_labeled_lines("Matrix", sprintf(
            "%d configurations \u00b7 %d models \u00b7 %d children \u00b7 %d cases",
            summary$configurations, summary$models, summary$children, summary$cases), content_width),
        shift__ui_labeled_lines("Status", sprintf(
            "%d completed \u00b7 %d active \u00b7 %d failed \u00b7 %d partial \u00b7 %d waiting",
            summary$completed, summary$active, summary$failed,
            summary$partial, summary$waiting), content_width)
    )
    children <- snapshot$children
    workflows <- character()
    for (index in seq_len(nrow(children))) {
        child <- children[index]
        identity <- c(child$method, child$model)
        identity <- identity[!is.na(identity) & nzchar(identity)]
        label <- sprintf("%s %s [%s] \u00b7 %s",
            shift__ui_state_symbol(child$status, motion, frame),
            cli::style_bold(paste(identity, collapse = " / ")),
            child$method_status, child$status)
        configuration <- c(child$scale, child$reconstruction)
        if (!is.na(child$current_stage)) {
            configuration <- c(configuration, shift__ui_stage_label(child$current_stage))
        }
        configuration <- configuration[!is.na(configuration) & nzchar(configuration)]
        # Put the method/model on the first row of each child. Its scientific
        # configuration and current stage stay aligned on the quieter row below.
        workflows <- c(workflows,
            shift__ui_labeled_lines(sprintf("#%d", index), label, content_width),
            shift__ui_labeled_lines("", cli::style_dim(
                paste(configuration, collapse = " \u00b7 ")), content_width))
        if (!identical(detail, "normal")) {
            workflows <- c(workflows, shift__ui_labeled_lines("",
                sprintf("%s \u00b7 %s \u00b7 %s", child$member, child$grid,
                    shift_coalesce(child$run_id, "planned")), content_width))
            outputs <- snapshot$outputs
            if (nrow(outputs)) {
                completion <- shift__ui_completion(data.table::data.table(),
                    outputs[outputs$child_key == child$child_key],
                    shift_diagnostics_empty())
                if (!is.null(completion$field_summary)) {
                    workflows <- c(workflows, shift__ui_labeled_lines("Fields",
                        completion$field_summary, content_width))
                }
            }
        }
    }
    results <- shift__ui_labeled_lines("Summary", sprintf("%d EPW files \u00b7 %d warnings",
        summary$epw_files, summary$warnings), content_width)
    warnings <- snapshot$diagnostics
    warnings <- warnings[warnings$severity %in% c("warning", "error")]
    if (nrow(warnings)) {
        for (index in seq_len(min(nrow(warnings), 3L))) {
            row <- warnings[index]
            results <- c(results, shift__ui_labeled_lines("Notice",
                paste(row$method, row$model, row$message, sep = " \u00b7 "), content_width))
        }
    }
    execution <- snapshot$execution
    if (nrow(execution)) {
        results <- c(results, shift__ui_labeled_lines("Call", sprintf(
            "%d children reused \u00b7 %d started/resumed \u00b7 %s",
            sum(execution$action == "reused"),
            sum(execution$action != "reused"),
            shift__format_elapsed(shift_coalesce(snapshot$call_elapsed_seconds,
                sum(execution$elapsed_seconds, na.rm = TRUE)))), content_width))
    }
    results <- c(results, shift__ui_labeled_lines("Output", summary$output_dir, content_width))
    lines <- if (isTRUE(panel)) {
        c(
            shift__ui_panel_rule(header, width, "top"),
            vapply(overview, shift__ui_panel_line, character(1L), width = width),
            shift__ui_panel_rule(cli::style_bold("Workflows"), width, "middle"),
            vapply(workflows, shift__ui_panel_line, character(1L), width = width),
            shift__ui_panel_rule(cli::style_bold("Results"), width, "middle"),
            vapply(results, shift__ui_panel_line, character(1L), width = width),
            shift__ui_panel_rule(width = width, kind = "bottom")
        )
    } else {
        c(shift__ui_wrap_lines(header, width), overview, workflows, results)
    }
    list(lines = lines, compact = shift__ui_fit(sprintf(
        "Batch %s \u00b7 %d/%d children complete \u00b7 %d EPW files \u00b7 %d warnings",
        summary$status, summary$completed, summary$children,
        summary$epw_files, summary$warnings), width), nodes = character(),
        cases = if (!identical(detail, "normal")) {
            shift__ui_case_table(snapshot$cases, width, detail)
        } else {
            character()
        })
}

# Emit a durable receipt in human modes while leaving JSON and quiet callers
# free of reporter text. Foreground child frames carry the live batch context.
shift_batch__report <- function(x, ui) {
    if (!identical(shift__ui_mode(ui), "none")) {
        shift__ui_print_view(shift_batch__view(
            shift_batch__snapshot(x, refresh = FALSE), detail = ui@detail))
    }
    invisible(x)
}

# Watch all independently running children; one failure never ends observation
# while another child is queued, running, or stopping.
shift_batch__watch <- function(x, follow, interval, events, ui) {
    mode <- shift__ui_mode(ui)
    renderer <- tryCatch(shift__ui_renderer(mode), error = function(error) NULL)
    if (identical(mode, "dynamic") && is.null(renderer)) mode <- "log"
    on.exit(if (!is.null(renderer)) renderer$close(), add = TRUE)
    frame <- 0L
    previous <- NULL
    tryCatch(repeat {
        x <- shift_batch__refresh(x)
        snapshot <- shift_batch__snapshot(x, event_count = events, refresh = FALSE)
        frame <- frame + 1L
        view <- shift_batch__view(snapshot, detail = ui@detail,
            motion = shift__ui_motion(ui), frame = frame)
        if (identical(mode, "dynamic")) {
            if (!isTRUE(renderer$draw(view$lines, compact = view$compact))) {
                renderer$close(result = "failed")
                renderer <- NULL
                mode <- "log"
            }
        }
        if (identical(mode, "log") && !identical(view$lines, previous)) {
            shift__ui_print_view(view)
        }
        previous <- view$lines
        if (!isTRUE(follow) || snapshot$batch$active == 0L) {
            break
        }
        shift__watch_sleep(interval)
    }, interrupt = function(error) {
        if (!identical(mode, "none")) {
            cli::cli_alert_info("Stopped watching; batch children continue. Use shift_cancel() to cancel them.")
        }
    })
    x
}
