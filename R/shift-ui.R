# ShiftUiOptions keeps runtime presentation preferences separate from the
# scientific ShiftPlan so display choices never alter deterministic artifacts.
ShiftUiOptions <- S7::new_class(
    "ShiftUiOptions",
    properties = list(
        progress = shift_prop_string(min.chars = 1L),
        detail = shift_prop_string(min.chars = 1L),
        heartbeat = S7::new_property(S7::class_numeric)
    )
)

# Detail levels are ordered so every renderer applies the same visibility
# contract to foreground runs, background logs, and watch snapshots.
SHIFT_UI_DETAIL_LEVELS <- c("normal", "detail", "debug")

#' @rdname shift_api
#' @param progress In [shift_ui()], workflow presentation mode: `"auto"`,
#'   `"dynamic"`, `"log"`, or `"none"`. In low-level collect helpers, a
#'   logical controlling their native query progress display.
#' @param detail Presentation detail level. `"normal"` shows task progress,
#'   `"detail"` adds selection, reuse, and fallback decisions, and `"debug"`
#'   also shows full URLs, paths, and low-level transfer context.
#' @param heartbeat Minimum seconds between transient heartbeat updates.
#' @export
shift_ui <- function(progress = c("auto", "dynamic", "log", "none"),
                     detail = c("normal", "detail", "debug"), heartbeat = 10) {
    progress <- match.arg(progress)
    detail <- match.arg(detail)
    checkmate::assert_number(heartbeat, lower = 0, finite = TRUE)
    ShiftUiOptions(
        progress = progress,
        detail = detail,
        heartbeat = as.numeric(heartbeat)
    )
}

# Resolve auto mode once per reporter so a run does not switch presentation
# when its surrounding output device changes halfway through execution.
shift__ui_mode <- function(ui) {
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (!identical(ui@progress, "auto")) {
        return(ui@progress)
    }
    # Rscript is non-interactive even when a human is watching it in a real
    # terminal. TTY capability, not `interactive()`, is the useful boundary.
    if (isTRUE(cli::is_dynamic_tty())) "dynamic" else "log"
}

# Compare one requested detail level against the immutable UI configuration.
shift__ui_at_least <- function(ui, level = c("normal", "detail", "debug")) {
    level <- match.arg(level)
    match(ui@detail, SHIFT_UI_DETAIL_LEVELS) >= match(level, SHIFT_UI_DETAIL_LEVELS)
}

# Resolve a bounded console width once per rendering operation. Keeping a
# 40-column floor prevents the fixed status labels from consuming the view.
shift__ui_width <- function(width = getOption("width", 80L)) {
    width <- suppressWarnings(as.integer(width[[1L]]))
    if (!length(width) || is.na(width) || width < 40L) 40L else width
}

# Fit plain user-facing text into one terminal row without relying on colour or
# terminal-specific clipping for essential status information.
shift__ui_fit <- function(x, width = shift__ui_width()) {
    x <- gsub("[\r\n]+", " ", as.character(shift_coalesce(x, "")))
    width <- shift__ui_width(width)
    if (nchar(x, type = "width") <= width) {
        return(x)
    }
    if (width <= 1L) {
        return(substr(x, 1L, width))
    }
    paste0(substr(x, 1L, width - 1L), "\u2026")
}

# Format named workflow periods compactly for the three-line startup summary.
shift__ui_periods <- function(periods) {
    periods <- data.table::as.data.table(periods)
    if (!nrow(periods) || !all(c("period", "year") %in% names(periods))) {
        return("no periods")
    }
    labels <- unique(as.character(periods$period))
    paste(vapply(labels, function(label) {
        years <- periods[period == label]$year
        if (length(unique(years)) == 1L) {
            sprintf("%s (%d)", label, years[[1L]])
        } else {
            sprintf("%s (%d\u2013%d)", label, min(years), max(years))
        }
    }, character(1L)), collapse = ", ")
}

# Describe the reference input without exposing matching fields or plan IDs in
# the normal startup view; those remain available through shift_explain().
shift__ui_reference <- function(method) {
    reference <- method@reference
    if (is.null(reference)) {
        return(if (isTRUE(morpher__recipe_accepts_reference(method@recipe))) {
            "baseline EPW"
        } else {
            "no reference"
        })
    }
    if (S7::S7_inherits(reference, ShiftReferenceSpec)) {
        periods_table <- data.table::as.data.table(reference@periods)
        period_names <- unique(periods_table$period)
        periods <- if (length(period_names) == 1L) {
            years <- periods_table$year
            if (length(unique(years)) == 1L) {
                as.character(years[[1L]])
            } else {
                sprintf("%d\u2013%d", min(years), max(years))
            }
        } else {
            shift__ui_periods(periods_table)
        }
        return(sprintf("%s %s", reference@mode, periods))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return("supplied climate reference")
    }
    "reference supplied"
}

# Build the deliberately compact three-line summary shown before any network
# request. Each line is independently width-bounded for 80-column consoles.
shift__ui_plan_summary <- function(plan, run_id, background = FALSE,
                                   width = shift__ui_width()) {
    request <- plan@meta$request@meta
    model <- shift_coalesce(shift_display_values(request$source), "<model>")
    scenarios <- shift_coalesce(shift_display_values(request$experiment), "<scenario>")
    status <- if (isTRUE(background)) "QUEUED" else "STARTING"
    c(
        shift__ui_fit(sprintf("Future EPW \u00b7 %s \u00b7 %s", run_id, status), width),
        shift__ui_fit(sprintf("%s \u00b7 %s \u00b7 %s",
            model, scenarios, shift__ui_periods(plan@meta$periods)), width),
        shift__ui_fit(sprintf("%s \u00b7 %s \u00b7 %d expected output(s)",
            plan@meta$method@name, shift__ui_reference(plan@meta$method),
            nrow(plan@meta$expected_cases)), width)
    )
}

# Map internal stage identifiers onto short labels that remain readable in the
# fixed status region and in redirected logs.
shift__ui_stage_label <- function(stage) {
    labels <- c(
        planned = "Plan", resolve = "Resolve", download = "Download",
        extract_future = "Extract future",
        extract_reference = "Extract reference", coverage = "Coverage",
        morph = "Morph", write_epw = "Write EPW", completed = "Completed",
        resume = "Resume"
    )
    value <- labels[[as.character(shift_coalesce(stage, "planned"))]]
    shift_coalesce(value, gsub("_", " ", as.character(stage), fixed = TRUE))
}

# Render the shared four-row live view used by foreground reporters and
# shift_watch(). The caller owns terminal animation; this helper owns meaning.
shift__ui_status_lines <- function(state, width = shift__ui_width()) {
    width <- shift__ui_width(width)
    stage_position <- if (!is.null(state$stage_current) && !is.null(state$stage_total)) {
        sprintf("[%d/%d] ", state$stage_current, state$stage_total)
    } else {
        ""
    }
    elapsed <- shift__format_elapsed(shift_coalesce(state$elapsed_seconds, 0))
    stage <- sprintf("Stage   %s%s \u00b7 %s \u00b7 %s",
        stage_position, shift__ui_stage_label(state$stage),
        toupper(shift_coalesce(state$status, "running")), elapsed)
    unit_position <- if (!is.null(state$unit_current) && !is.null(state$unit_total)) {
        sprintf("[%d/%d] ", state$unit_current, state$unit_total)
    } else {
        ""
    }
    current <- sprintf("Current %s%s", unit_position,
        shift_coalesce(state$unit_label,
            shift_coalesce(state$stage_message, "Waiting")))
    cases <- sprintf("Cases   ready %d/%d \u00b7 outputs %d/%d",
        as.integer(shift_coalesce(state$cases_ready, 0L)),
        as.integer(shift_coalesce(state$cases_total, 0L)),
        as.integer(shift_coalesce(state$outputs_completed, 0L)),
        as.integer(shift_coalesce(state$cases_total, 0L)))
    last <- sprintf("Last    %s", shift_coalesce(state$last_event, "No completed event yet"))
    vapply(c(stage, current, cases, last), shift__ui_fit,
        character(1L), width = width)
}

# Format byte counts locally so workflow UI does not depend on units objects or
# on the downloader's table renderer.
shift__ui_bytes <- function(bytes) {
    if (is.null(bytes) || !length(bytes)) {
        return("?")
    }
    bytes <- suppressWarnings(as.numeric(bytes[[1L]]))
    if (!length(bytes) || is.na(bytes) || !is.finite(bytes)) {
        return("?")
    }
    labels <- c("B", "KiB", "MiB", "GiB", "TiB")
    power <- if (bytes <= 0) 0L else min(floor(log(bytes, 1024)), length(labels) - 1L)
    value <- bytes / (1024^power)
    sprintf(if (power == 0L) "%.0f %s" else "%.1f %s", value, labels[[power + 1L]])
}

# Convert an index-node URL into the stable short name used in every normal and
# detail view. Unknown nodes fall back to their host name.
shift__node_label <- function(node) {
    node <- as.character(shift_coalesce(node, "unknown"))[[1L]]
    normalized <- tryCatch(query__normalize_node(node), error = function(e) node)
    known <- vapply(INDEX_NODES, function(value) {
        identical(tryCatch(query__normalize_node(value), error = function(e) value),
            normalized)
    }, logical(1L))
    if (any(known)) {
        return(names(INDEX_NODES)[which(known)[[1L]]])
    }
    parsed <- tryCatch(curl::curl_parse_url(normalized), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$host) || !nzchar(parsed$host)) {
        normalized
    } else {
        parsed$host
    }
}

# Format resolver attempts as an 80-column-safe table. The result column is
# elastic because failure explanations carry more value than padded counters.
shift__ui_node_table <- function(rows, width = shift__ui_width()) {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows)) {
        return(character())
    }
    width <- shift__ui_width(width)
    node_width <- min(12L, max(4L, nchar(c("Node", rows$node), type = "width")))
    future_width <- 7L
    reference_width <- 9L
    result_width <- max(10L, width - node_width - future_width - reference_width - 8L)
    cell <- function(x, size) {
        x <- shift__ui_fit(ifelse(is.na(x), "\u2014", as.character(x)), size)
        sprintf(paste0("%-", size, "s"), x)
    }
    lines <- c(
        "Resolver attempts",
        sprintf("  %s  %s  %s  %s",
            cell("Node", node_width), cell("Future", future_width),
            cell("Reference", reference_width), cell("Result", result_width))
    )
    for (i in seq_len(nrow(rows))) {
        lines <- c(lines, sprintf("  %s  %s  %s  %s",
            cell(rows$node[[i]], node_width), cell(rows$future[[i]], future_width),
            cell(rows$reference[[i]], reference_width),
            cell(rows$result[[i]], result_width)))
    }
    vapply(lines, shift__ui_fit, character(1L), width = width)
}

# Format user cases independently from extraction plans. Narrow terminals omit
# the member column before truncating scenario or missing-reason information.
shift__ui_case_table <- function(rows, width = shift__ui_width(),
                                 detail = "normal") {
    rows <- data.table::as.data.table(rows)
    if (!nrow(rows)) {
        return(character())
    }
    width <- shift__ui_width(width)
    scenario <- if ("experiment_id" %in% names(rows)) rows$experiment_id else rep("\u2014", nrow(rows))
    period <- if ("period" %in% names(rows)) rows$period else rep("\u2014", nrow(rows))
    member <- if ("variant_label" %in% names(rows)) rows$variant_label else rep("\u2014", nrow(rows))
    status <- if ("status" %in% names(rows)) rows$status else rep("unknown", nrow(rows))
    include_member <- width >= 68L
    scenario_width <- min(14L, max(8L, nchar(c("Scenario", scenario), type = "width")))
    period_width <- min(12L, max(6L, nchar(c("Period", period), type = "width")))
    member_width <- if (include_member) min(14L, max(6L,
        nchar(c("Member", member), type = "width"))) else 0L
    fixed <- scenario_width + period_width + member_width +
        if (include_member) 10L else 7L
    status_width <- max(10L, width - fixed)
    cell <- function(x, size) {
        x <- shift__ui_fit(ifelse(is.na(x), "\u2014", as.character(x)), size)
        sprintf(paste0("%-", size, "s"), x)
    }
    header <- if (include_member) {
        sprintf("  %s  %s  %s  %s", cell("Scenario", scenario_width),
            cell("Period", period_width), cell("Member", member_width),
            cell("Status", status_width))
    } else {
        sprintf("  %s  %s  %s", cell("Scenario", scenario_width),
            cell("Period", period_width), cell("Status", status_width))
    }
    lines <- c("Cases", header)
    for (i in seq_len(nrow(rows))) {
        value <- status[[i]]
        if (!identical(detail, "normal") && "missing_reason" %in% names(rows) &&
            !is.na(rows$missing_reason[[i]]) && nzchar(rows$missing_reason[[i]])) {
            value <- sprintf("%s \u00b7 %s", value, rows$missing_reason[[i]])
        }
        line <- if (include_member) {
            sprintf("  %s  %s  %s  %s", cell(scenario[[i]], scenario_width),
                cell(period[[i]], period_width), cell(member[[i]], member_width),
                cell(value, status_width))
        } else {
            sprintf("  %s  %s  %s", cell(scenario[[i]], scenario_width),
                cell(period[[i]], period_width), cell(value, status_width))
        }
        lines <- c(lines, line)
    }
    vapply(lines, shift__ui_fit, character(1L), width = width)
}

# Decode persisted event details without allowing a malformed historical event
# to break shift_watch() for the rest of an otherwise readable run.
shift__ui_event_details <- function(events) {
    if (!nrow(events)) {
        return(list())
    }
    if (!"details_json" %in% names(events)) {
        return(rep(list(list()), nrow(events)))
    }
    lapply(events$details_json, function(value) {
        if (is.null(value) || !length(value) || is.na(value) || !nzchar(value)) {
            return(list())
        }
        tryCatch(jsonlite::fromJSON(value, simplifyVector = TRUE),
            error = function(e) list())
    })
}

# Reconstruct the same semantic live state from persisted tables that the
# foreground reporter maintains in memory.
shift__ui_table_state <- function(row, events, cases) {
    row <- data.table::as.data.table(row)
    events <- data.table::as.data.table(events)
    cases <- data.table::as.data.table(cases)
    details <- shift__ui_event_details(events)
    stage <- row$current_stage[[1L]]
    stage_indices <- which(vapply(details, function(x) {
        identical(x$phase, "stage") && identical(x$stage, stage)
    }, logical(1L)))
    stage_index <- if (length(stage_indices)) utils::tail(stage_indices, 1L) else NA_integer_
    unit_indices <- which(vapply(details, function(x) {
        identical(x$phase, "unit") && identical(x$stage, stage)
    }, logical(1L)))
    unit_index <- if (length(unit_indices)) utils::tail(unit_indices, 1L) else NA_integer_
    last_index <- if (nrow(events)) nrow(events) else NA_integer_
    started_at <- row$started_at[[1L]]
    stopped_at <- row$completed_at[[1L]]
    if (is.na(stopped_at)) {
        terminal <- row$status[[1L]] %in%
            c("completed", "partial", "failed", "cancelled")
        # Older or partially written terminal rows may lack completed_at. In
        # that case freeze elapsed time at their last durable activity rather
        # than making a completed or failed run appear to keep executing.
        if (isTRUE(terminal) && "updated_at" %in% names(row) &&
            !is.na(row$updated_at[[1L]])) {
            stopped_at <- row$updated_at[[1L]]
        } else if (isTRUE(terminal) && nrow(events) &&
            "created_at" %in% names(events) &&
            !is.na(events$created_at[[nrow(events)]])) {
            stopped_at <- events$created_at[[nrow(events)]]
        } else {
            stopped_at <- Sys.time()
        }
    }
    elapsed <- if (is.na(started_at)) 0 else as.numeric(difftime(
        stopped_at, started_at, units = "secs"))
    stage_details <- if (is.na(stage_index)) list() else details[[stage_index]]
    unit_details <- if (is.na(unit_index)) list() else details[[unit_index]]
    fallback_stage_message <- switch(row$status[[1L]],
        queued = "Waiting for background worker",
        completed = "Workflow completed",
        partial = "Workflow completed with missing cases",
        failed = "Workflow failed",
        cancelled = "Workflow cancelled",
        stopping = "Waiting for cancellation boundary",
        "Waiting for next workflow event"
    )
    list(
        run_id = row$run_id[[1L]],
        status = row$status[[1L]],
        stage = stage,
        stage_message = if (is.na(stage_index)) {
            fallback_stage_message
        } else {
            events$message[[stage_index]]
        },
        stage_current = stage_details$current,
        stage_total = stage_details$total,
        unit_label = if (is.na(unit_index)) NULL else events$message[[unit_index]],
        unit_current = unit_details$current,
        unit_total = unit_details$total,
        cases_ready = sum(cases$status %in% c("ready", "morphing", "morphed", "completed")),
        cases_total = if (nrow(cases)) nrow(cases) else 0L,
        outputs_completed = sum(cases$status %in% "completed"),
        last_event = if (is.na(last_index)) "No activity" else events$message[[last_index]],
        elapsed_seconds = elapsed
    )
}

# Reconstruct the resolver-attempt table from terminal index-node events.
shift__ui_event_nodes <- function(events) {
    events <- data.table::as.data.table(events)
    details <- shift__ui_event_details(events)
    rows <- lapply(seq_along(details), function(i) {
        value <- details[[i]]
        if (!identical(value$unit_type, "index_node") ||
            !events$status[[i]] %in%
                c("completed", "skipped", "rejected", "failed")) {
            return(NULL)
        }
        data.table::data.table(
            node = shift__node_label(value$node),
            future = shift_coalesce(value$future_files, NA_integer_),
            reference = shift_coalesce(value$reference_files, NA_integer_),
            result = if (events$status[[i]] %in% c("completed", "skipped")) {
                shift_coalesce(value$result, "selected")
            } else {
                shift_coalesce(value$error, events$message[[i]])
            }
        )
    })
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Build the complete watch view once so R and CLI renderers cannot drift in
# stage, case, resolver, or width semantics.
shift__ui_table_view <- function(row, cases, events,
                                 width = shift__ui_width(), detail = "normal") {
    list(
        lines = shift__ui_status_lines(
            shift__ui_table_state(row, events, cases), width = width),
        nodes = shift__ui_node_table(shift__ui_event_nodes(events), width = width),
        cases = shift__ui_case_table(cases,
            width = width, detail = detail)
    )
}

# Adapt a live ShiftRun handle to the table-based view shared with the CLI.
shift__ui_run_view <- function(run, width = shift__ui_width(),
                               detail = "normal") {
    shift__ui_table_view(
        row = run@meta$run,
        cases = shift_cases(run, refresh = FALSE),
        events = run@meta$events,
        width = width,
        detail = detail
    )
}

# Render a complete persisted snapshot once. This is the non-animated fallback
# and the final frame for both R and CLI watch commands.
shift__ui_print_view <- function(view, include_tables = TRUE) {
    for (line in view$lines) {
        cli::cli_verbatim(line)
    }
    if (isTRUE(include_tables)) {
        for (line in c(view$nodes, view$cases)) {
            cli::cli_verbatim(line)
        }
    }
    invisible(view)
}

# Create one cli-owned row per live status line. cli normalizes embedded
# newlines inside a single bar, so a real fixed region requires four bars.
shift__ui_progress_create <- function(lines) {
    ids <- character()
    for (i in seq_along(lines)) {
        id <- tryCatch(cli::cli_progress_bar(
            name = sprintf("Shift status %d", i),
            total = NA,
            status = lines[[i]],
            format = "{cli::pb_status}",
            current = FALSE,
            auto_terminate = FALSE,
            .auto_close = FALSE
        ), error = function(e) NULL)
        if (is.null(id)) {
            shift__ui_progress_close(ids, result = "failed")
            return(character())
        }
        ids <- c(ids, id)
    }
    ids
}

# Update all fixed rows as one best-effort rendering operation.
shift__ui_progress_update <- function(ids, lines) {
    if (length(ids) != length(lines) || !length(ids)) {
        return(FALSE)
    }
    ok <- vapply(seq_along(ids), function(i) {
        tryCatch({
            cli::cli_progress_update(id = ids[[i]], inc = 0L,
                status = lines[[i]], force = TRUE)
            TRUE
        }, error = function(e) FALSE)
    }, logical(1L))
    all(ok)
}

# Close every row exactly once, including partially created fixed regions.
shift__ui_progress_close <- function(ids, result = "done") {
    for (id in ids) {
        try(cli::cli_progress_done(id = id, result = result), silent = TRUE)
    }
    invisible(NULL)
}

# Refresh a fixed status region and recreate all rows when an IDE has removed
# any one of them. Returning the new IDs keeps ownership with the caller.
shift__ui_progress_refresh <- function(ids, lines) {
    if (length(ids) && isTRUE(shift__ui_progress_update(ids, lines))) {
        return(list(ids = ids, ok = TRUE))
    }
    shift__ui_progress_close(ids, result = "failed")
    ids <- shift__ui_progress_create(lines)
    list(ids = ids, ok = length(ids) &&
        isTRUE(shift__ui_progress_update(ids, lines)))
}

# Format one persisted event for append-only watch logs with the same stage,
# node, and catalog-role context used by foreground log reporters.
shift__ui_persisted_event_line <- function(event, detail = "normal",
                                           width = shift__ui_width()) {
    details <- shift__ui_event_details(event)[[1L]]
    context <- c(shift__ui_stage_label(event$stage[[1L]]))
    if (!is.null(details$node) && length(details$node)) {
        context <- c(context, if (identical(detail, "debug")) {
            as.character(details$node[[1L]])
        } else {
            shift__node_label(details$node)
        })
    }
    if (!is.null(details$catalog_role) && length(details$catalog_role)) {
        context <- c(context, as.character(details$catalog_role[[1L]]))
    }
    line <- sprintf("%s [%s] %s",
        format(event$created_at[[1L]], "%F %T"),
        paste(context, collapse = "]["), event$message[[1L]])
    if (is.null(width)) line else shift__ui_fit(line, width)
}

# Select an event delta before applying any presentation limit so a watch
# client cannot silently lose milestones when more than one page arrives
# between polls. A missing cursor is reported separately because bounded live
# sidecars may legitimately have discarded older events.
shift__ui_event_delta <- function(events, last_event_id = NA_character_,
                                  initial_limit = 10L,
                                  initial = is.na(last_event_id)) {
    events <- data.table::as.data.table(events)
    checkmate::assert_count(initial_limit, positive = FALSE)
    checkmate::assert_flag(initial)
    newest <- if (nrow(events)) {
        as.character(events$event_id[[nrow(events)]])
    } else {
        NA_character_
    }
    if (isTRUE(initial)) {
        rows <- if (initial_limit == 0L) events[0] else utils::tail(
            events, initial_limit)
        return(list(rows = rows, cursor = newest, gap = FALSE))
    }
    if (!nrow(events)) {
        return(list(rows = events, cursor = last_event_id, gap = FALSE))
    }
    if (is.na(last_event_id) || !nzchar(last_event_id)) {
        return(list(rows = events, cursor = newest, gap = FALSE))
    }
    position <- match(last_event_id, events$event_id)
    if (is.na(position)) {
        return(list(rows = events, cursor = newest, gap = TRUE))
    }
    rows <- if (position < nrow(events)) {
        events[seq.int(position + 1L, nrow(events))]
    } else {
        events[0]
    }
    list(rows = rows, cursor = newest, gap = FALSE)
}

# Normalize event details to a stable JSON shape shared by Console reporters,
# persisted run events, and CLI/R watch views.
shift__progress_details <- function(stage = NULL, phase = NULL,
                                    unit_type = NULL, unit_label = NULL,
                                    current = NULL, total = NULL, node = NULL,
                                    scenario = NULL, variable = NULL, period = NULL,
                                    access_method = NULL, elapsed_seconds = NULL,
                                    outcome = NULL, ...) {
    values <- c(list(
        stage = stage,
        phase = phase,
        unit_type = unit_type,
        unit_label = unit_label,
        current = current,
        total = total,
        node = node,
        scenario = scenario,
        variable = variable,
        period = period,
        access_method = access_method,
        elapsed_seconds = elapsed_seconds,
        outcome = outcome
    ), list(...))
    values[!vapply(values, is.null, logical(1L))]
}

# ShiftReporter is the single runtime sink for workflow messages and durable
# milestone events. Heartbeats remain transient to avoid frequent store writes.
ShiftReporter <- R6::R6Class(
    "ShiftReporter",
    lock_class = TRUE,
    public = list(
        # Bind one reporter to a stable run/job identity and resolve its
        # presentation mode once for the lifetime of the execution attempt.
        initialize = function(ui = shift_ui(), store = NULL, run_id = NULL,
                              job_id = NULL, background = FALSE) {
            if (!S7::S7_inherits(ui, ShiftUiOptions)) {
                cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
            }
            private$ui_value <- ui
            private$mode_value <- shift__ui_mode(ui)
            private$store <- store
            private$run_id_value <- run_id
            private$job_id_value <- job_id
            private$background <- isTRUE(background)
            private$started_at <- Sys.time()
            private$last_heartbeat <- as.POSIXct(NA)
            private$status <- if (isTRUE(background)) "queued" else "running"
        },

        # Render the scientific plan summary before any remote operation and
        # include control commands when a process job has only been queued.
        run_started = function(plan, run_id, background = FALSE) {
            private$run_id_value <- run_id
            private$background <- isTRUE(background)
            private$status <- if (isTRUE(background)) "queued" else "running"
            private$cases_total <- nrow(plan@meta$expected_cases)
            if (!identical(private$mode_value, "none")) {
                summary <- shift__ui_plan_summary(plan, run_id,
                    background = background, width = private$width())
                private$emit("info", summary[[1L]])
                for (line in summary[-1L]) {
                    private$emit("text", line)
                }
                if (isTRUE(background)) {
                    private$emit("text", shift__ui_fit(sprintf(
                        "Next    shift_watch(\"%s\") \u00b7 shift_cancel(\"%s\")",
                        run_id, run_id), private$width()))
                    if (shift__ui_at_least(private$ui_value, "debug")) {
                        private$emit("text", shift__ui_fit(sprintf(
                            "Store   %s", plan@store_path), private$width()))
                    }
                }
            }
            invisible(self)
        },

        # Start a durable workflow stage and close any dynamic unit left by the
        # preceding stage before emitting its new status.
        stage_started = function(stage, message, current = NULL, total = NULL, details = list()) {
            private$stage <- stage
            private$status <- "running"
            private$stage_message <- message
            private$stage_current <- current
            private$stage_total <- total
            private$stage_started_at <- Sys.time()
            private$current_details <- NULL
            private$last_event <- message
            if (identical(private$mode_value, "log")) {
                private$emit("info", private$format_event(message,
                    current = current, total = total,
                    details = list(stage = stage, phase = "stage")))
            } else {
                private$render_dynamic()
            }
            private$persist(stage, "running", message,
                utils::modifyList(shift__progress_details(stage = stage, phase = "stage", current = current, total = total), details))
            invisible(self)
        },

        # Start a user-meaningful business unit such as a node, variable, or
        # scenario-period case and initialize dynamic progress when available.
        unit_started = function(message, current = NULL, total = NULL, details = list()) {
            private$unit_started_at <- Sys.time()
            private$current_details <- utils::modifyList(
                shift__progress_details(
                    stage = private$stage,
                    phase = "unit",
                    unit_label = message,
                    unit_base_label = message,
                    current = current,
                    total = total
                ),
                details
            )
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic()
            } else {
                private$emit("verbatim", private$format_event(message,
                    current = current, total = total,
                    details = private$current_details))
            }
            private$persist(private$stage, "running", message, private$current_details)
            invisible(self)
        },

        # Complete the current business unit with a structured outcome that can
        # later be reconstructed by watch clients.
        unit_completed = function(message, current = NULL, total = NULL,
                                  outcome = "completed", details = list()) {
            elapsed <- private$elapsed(private$unit_started_at)
            event_details <- utils::modifyList(
                shift_coalesce(private$current_details, shift__progress_details(stage = private$stage)),
                c(details, list(unit_label = message,
                    unit_base_label = message, current = current, total = total,
                    elapsed_seconds = elapsed, outcome = outcome))
            )
            private$current_details <- event_details
            private$last_event <- message
            private$capture_business_result(message, event_details)
            if (identical(private$mode_value, "dynamic")) {
                if (outcome %in% c("failed", "fallback")) {
                    private$emit("warning", private$format_event(message,
                        current = current, total = total, details = event_details))
                }
                private$render_dynamic()
            } else if (shift__ui_at_least(private$ui_value, "detail") ||
                outcome %in% c("failed", "fallback")) {
                event_type <- if (identical(outcome, "failed")) {
                    "warning"
                } else if (outcome %in% c("fallback", "rejected")) {
                    "verbatim"
                } else {
                    "success"
                }
                private$emit(event_type,
                    private$format_event(message, current = current, total = total,
                        details = event_details))
            }
            private$persist(private$stage, outcome, message, event_details)
            invisible(self)
        },

        # Persist a meaningful change to the current business unit without
        # treating transient animation frames as durable workflow events.
        unit_updated = function(message, current = NULL, total = NULL, details = list()) {
            event_details <- utils::modifyList(
                shift_coalesce(private$current_details, shift__progress_details(stage = private$stage)),
                c(details, list(unit_label = message,
                    unit_base_label = message, current = current,
                    total = total, outcome = "updated"))
            )
            private$current_details <- event_details
            private$last_event <- message
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic()
            } else if (shift__ui_at_least(private$ui_value, "detail")) {
                private$emit("verbatim", private$format_event(message,
                    current = current, total = total, details = event_details))
            }
            private$persist(private$stage, "updated", message, event_details)
            invisible(self)
        },

        # Record deterministic resume/reuse outcomes with a dedicated reporter
        # method so callers do not need to encode skipped semantics themselves.
        unit_skipped = function(message, current = NULL, total = NULL, details = list()) {
            self$unit_completed(message, current = current, total = total,
                outcome = "skipped", details = details)
        },

        # Record an operational milestone that is relevant to the current stage
        # but is not itself a countable business unit.
        notice = function(message, outcome = "info", details = list()) {
            event_details <- utils::modifyList(
                shift__progress_details(stage = private$stage, phase = "notice",
                    outcome = outcome),
                details
            )
            private$last_event <- message
            if (identical(private$mode_value, "dynamic")) {
                if (outcome %in% c("failed", "fallback")) {
                    private$emit("warning", private$format_event(message,
                        details = event_details))
                }
                private$render_dynamic()
            } else if (!identical(private$mode_value, "none")) {
                private$emit(if (outcome %in% c("failed", "fallback")) "warning" else "verbatim",
                    private$format_event(message, details = event_details))
            }
            private$persist(private$stage, outcome, message, event_details)
            invisible(self)
        },

        # Update the user-case snapshot after coverage or output transitions.
        # The same rows are later reconstructed from shift_run_case by watch.
        cases_updated = function(cases, show = FALSE) {
            private$case_rows <- data.table::as.data.table(data.table::copy(cases))
            private$cases_total <- nrow(private$case_rows)
            private$cases_ready <- sum(private$case_rows$status %in%
                c("ready", "morphing", "morphed", "completed"))
            private$outputs_completed <- sum(private$case_rows$status %in% "completed")
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic()
            }
            if (isTRUE(show) && !identical(private$mode_value, "none")) {
                private$render_case_table()
            }
            invisible(self)
        },

        # Check cooperative cancellation at explicit workflow boundaries even
        # when no heartbeat or progress output is currently being rendered.
        check_cancel = function(stage = private$stage) {
            if (!is.null(private$store) && !is.null(private$run_id_value) && !is.null(private$job_id_value)) {
                shift__job_check_cancel(private$store, private$run_id_value, private$job_id_value, stage)
            }
            invisible(FALSE)
        },

        # Close the dynamic unit and persist the terminal milestone for the
        # current stage together with its elapsed time.
        stage_completed = function(message, details = list()) {
            elapsed <- private$elapsed(private$stage_started_at)
            private$last_event <- message
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic()
                private$emit("success", private$format_event(message,
                    details = list(stage = private$stage, phase = "stage")))
                if (identical(private$stage, "resolve")) {
                    private$render_node_table()
                }
            } else if (!identical(private$mode_value, "none")) {
                private$emit("success", private$format_event(message,
                    details = list(stage = private$stage, phase = "stage")))
                if (identical(private$stage, "resolve")) {
                    private$render_node_table()
                }
            }
            private$persist(private$stage, "completed", message,
                utils::modifyList(shift__progress_details(
                    stage = private$stage,
                    phase = "stage",
                    elapsed_seconds = elapsed,
                    outcome = "completed"
                ), details))
            invisible(self)
        },

        # Refresh transient liveness and cancellation state without persisting
        # animation-only heartbeat events in the run history.
        heartbeat = function(message = NULL, details = list(), force = FALSE) {
            now <- Sys.time()
            due <- isTRUE(force) || is.na(private$last_heartbeat) ||
                as.numeric(difftime(now, private$last_heartbeat, units = "secs")) >= private$ui_value@heartbeat
            if (!isTRUE(due)) {
                return(invisible(FALSE))
            }
            private$last_heartbeat <- now
            # Cancellation and worker liveness must still be checked when the
            # user selected progress = "none".
            if (!is.null(private$store) && !is.null(private$run_id_value) && !is.null(private$job_id_value)) {
                shift__job_check_cancel(private$store, private$run_id_value,
                    private$job_id_value, shift_coalesce(private$stage, "working"))
            }
            private$touch_job()
            if (identical(private$mode_value, "none")) {
                return(invisible(TRUE))
            }
            # Keep a stable base label separate from the transient elapsed
            # suffix so repeated heartbeats never grow the displayed message.
            label <- shift_coalesce(message, shift_coalesce(
                private$current_details$unit_base_label,
                shift_coalesce(private$current_details$unit_label, "Working")))
            elapsed <- private$elapsed(private$unit_started_at)
            status <- sprintf("%s (%s elapsed)", label, shift__format_elapsed(elapsed))
            if (identical(private$mode_value, "dynamic")) {
                private$current_details$unit_label <- status
                private$render_dynamic()
            } else {
                private$emit("verbatim", private$format_event(status,
                    details = shift_coalesce(private$current_details,
                        list(stage = private$stage))))
            }
            invisible(TRUE)
        },

        # Render one terminal success summary from the refreshed run state and
        # cap output paths unless verbose mode was requested.
        run_completed = function(run, outputs = data.table::data.table()) {
            private$close_bar(result = "done")
            elapsed <- private$elapsed(private$started_at)
            status <- shift_status(run, refresh = FALSE)
            private$status <- status
            private$emit("success", sprintf("Future EPW run %s %s: %d output(s) in %s.",
                private$run_id_value, status, nrow(outputs), shift__format_elapsed(elapsed)))
            if (!identical(private$mode_value, "none") && nrow(outputs)) {
                paths <- shift_coalesce(outputs$export_path, outputs$path)
                paths <- paths[!is.na(paths) & nzchar(paths)]
                limit <- if (shift__ui_at_least(private$ui_value, "detail")) {
                    length(paths)
                } else {
                    min(5L, length(paths))
                }
                for (path in utils::head(paths, limit)) {
                    shown <- if (shift__ui_at_least(private$ui_value, "debug")) {
                        path
                    } else {
                        shift_display_path(path)
                    }
                    private$emit("verbatim", if (shift__ui_at_least(
                        private$ui_value, "debug")) {
                        paste0("  ", shown)
                    } else {
                        shift__ui_fit(paste0("  ", shown), private$width())
                    })
                }
                if (length(paths) > limit) {
                    private$emit("text", sprintf("  ... and %d more", length(paths) - limit))
                }
            }
            invisible(self)
        },

        # Close transient UI resources before showing a terminal failure or
        # cancellation message.
        run_failed = function(message = NULL, cancelled = FALSE) {
            private$close_bar(result = "failed")
            private$status <- if (isTRUE(cancelled)) "cancelled" else "failed"
            # The caller raises the one primary cli condition. Reporter output
            # here is deliberately limited to structured context tables so a
            # failure is never printed once by the reporter and again by rlang.
            if (!isTRUE(cancelled)) {
                private$render_node_table(force = TRUE)
                # Failure tables always expose the concrete missing reason,
                # even when the normal running view intentionally stays terse.
                private$render_case_table(force = TRUE, detail = "detail")
            } else if (!is.null(message)) {
                private$emit("warning", message)
            }
            invisible(self)
        },

        # Keep cancellation rendering distinct at call sites while sharing the
        # same cleanup and warning behavior as other terminal failures.
        run_cancelled = function(message) {
            self$run_failed(message, cancelled = TRUE)
        },

        # Emit low-level paths, URLs, and reuse details only when explicitly
        # requested by the caller.
        detail = function(message, level = c("detail", "debug")) {
            level <- match.arg(level)
            if (shift__ui_at_least(private$ui_value, level)) {
                private$emit("text", message)
            }
            invisible(self)
        },

        # Expose immutable reporter context to workflow adapters without
        # leaking its mutable private state.
        mode = function() private$mode_value,
        # Return the validated UI options used to create this reporter.
        ui = function() private$ui_value,
        # Return the durable run identity associated with persisted events.
        run_id = function() private$run_id_value,
        # Return the current execution-attempt identity used for heartbeats.
        job_id = function() private$job_id_value,
        # Return the current business context for terminal diagnostics without
        # exposing the reporter's mutable private environment.
        context = function() shift_coalesce(private$current_details, list()),
        # Return the semantic view state for unit tests and alternate renderers.
        snapshot = function() private$view_state(),

        # Explicitly release a dynamic progress bar when a caller exits through
        # an unusual but non-error path.
        close = function() {
            private$close_bar(result = "done")
            invisible(self)
        }
    ),
    private = list(
        ui_value = NULL,
        mode_value = NULL,
        store = NULL,
        run_id_value = NULL,
        job_id_value = NULL,
        background = FALSE,
        status = NULL,
        stage = NULL,
        bar_id = NULL,
        started_at = NULL,
        stage_started_at = NULL,
        unit_started_at = NULL,
        last_heartbeat = NULL,
        current_details = NULL,
        stage_message = NULL,
        stage_current = NULL,
        stage_total = NULL,
        last_event = NULL,
        cases_ready = 0L,
        cases_total = 0L,
        outputs_completed = 0L,
        node_rows = NULL,
        case_rows = NULL,

        # Map reporter message kinds onto cli output while respecting the null
        # presentation mode. Console rendering failures are deliberately
        # contained because presentation must never abort scientific work.
        emit = function(type, message) {
            if (identical(private$mode_value, "none")) {
                return(invisible(NULL))
            }
            tryCatch(
                switch(type,
                    success = cli::cli_alert_success("{message}"),
                    warning = cli::cli_alert_warning("{message}"),
                    danger = cli::cli_alert_danger("{message}"),
                    info = cli::cli_alert_info("{message}"),
                    verbatim = cli::cli_verbatim(message),
                    cli::cli_text("{message}")
                ),
                error = function(e) invisible(NULL)
            )
            invisible(NULL)
        },

        # Persist one structured milestone and update job liveness as one
        # reporter-side operation.
        persist = function(stage, status, message, details) {
            if (is.null(private$store) || is.null(private$run_id_value)) {
                return(invisible(NULL))
            }
            # Job heartbeat persistence immediately snapshots the same event;
            # suppress the first snapshot to avoid two full live JSON rewrites
            # for every reporter milestone.
            shift__run_event(private$store, private$run_id_value, stage, status,
                message, details, snapshot = FALSE)
            private$touch_job()
            invisible(NULL)
        },

        # Best-effort heartbeat updates must never replace the workflow error
        # that triggered reporter cleanup.
        touch_job = function() {
            if (!is.null(private$store) && !is.null(private$job_id_value) &&
                exists("shift__job_touch", mode = "function")) {
                try(shift__job_touch(private$store, private$job_id_value), silent = TRUE)
            }
            invisible(NULL)
        },

        # Close and clear the active cli bar exactly once.
        close_bar = function(result = "done") {
            if (length(private$bar_id)) {
                shift__ui_progress_close(private$bar_id, result = result)
                private$bar_id <- character()
            }
            invisible(NULL)
        },

        # Normalize missing timestamps to zero so summaries remain renderable
        # during early launch failures.
        elapsed = function(start) {
            if (is.null(start) || length(start) == 0L || is.na(start)) {
                return(0)
            }
            as.numeric(difftime(Sys.time(), start, units = "secs"))
        },

        # Resolve the output width at render time so tests, IDE resizing, and
        # redirected 80-column logs all share the same clipping behavior.
        width = function() shift__ui_width(),

        # Assemble the semantic state consumed by the shared status formatter.
        view_state = function() {
            details <- shift_coalesce(private$current_details, list())
            list(
                run_id = private$run_id_value,
                status = private$status,
                stage = private$stage,
                stage_message = private$stage_message,
                stage_current = private$stage_current,
                stage_total = private$stage_total,
                unit_label = details$unit_label,
                unit_current = details$current,
                unit_total = details$total,
                cases_ready = private$cases_ready,
                cases_total = private$cases_total,
                outputs_completed = private$outputs_completed,
                last_event = private$last_event,
                elapsed_seconds = private$elapsed(private$started_at)
            )
        },

        # Refresh the fixed four-row dynamic region, recreating it defensively
        # if an IDE has already removed its cli progress frame.
        render_dynamic = function() {
            if (!identical(private$mode_value, "dynamic")) {
                return(invisible(FALSE))
            }
            lines <- shift__ui_status_lines(private$view_state(),
                width = private$width())
            refreshed <- shift__ui_progress_refresh(private$bar_id, lines)
            private$bar_id <- refreshed$ids
            if (!isTRUE(refreshed$ok)) {
                private$fallback_to_log(lines)
                return(invisible(FALSE))
            }
            invisible(TRUE)
        },

        # Degrade a broken dynamic renderer exactly once to durable line logs.
        # Presentation failures must remain visible without aborting or hiding
        # the scientific workflow that is still running underneath them.
        fallback_to_log = function(lines) {
            private$close_bar(result = "failed")
            private$mode_value <- "log"
            private$emit("warning",
                "Dynamic progress is unavailable; switched to line-by-line logs.")
            for (line in lines) {
                private$emit("verbatim", line)
            }
            private$persist(
                shift_coalesce(private$stage, "ui"),
                "warning",
                "Dynamic progress was unavailable; switched to line-by-line logs.",
                shift__progress_details(
                    stage = shift_coalesce(private$stage, "ui"),
                    phase = "notice",
                    unit_type = "ui",
                    outcome = "fallback"
                )
            )
            invisible(NULL)
        },

        # Prefix append-only log events with stable workflow context. Full URLs
        # are restricted to debug mode while normal logs use short node names.
        format_event = function(message, current = NULL, total = NULL,
                                details = list()) {
            stage <- shift__ui_stage_label(shift_coalesce(details$stage, private$stage))
            context <- character()
            node <- details$node
            if (!is.null(node) && length(node) && !is.na(node[[1L]])) {
                node <- as.character(node[[1L]])
                if (!shift__ui_at_least(private$ui_value, "debug")) {
                    node <- shift__node_label(node)
                }
                context <- c(context, node)
            }
            phase <- details$catalog_role
            if (is.null(phase) && !identical(details$phase, "stage") &&
                !identical(details$phase, "unit") && !identical(details$phase, "notice")) {
                phase <- details$phase
            }
            if (!is.null(phase) && length(phase) && !is.na(phase[[1L]])) {
                context <- c(context, as.character(phase[[1L]]))
            }
            prefix <- paste0("[", paste(c(stage, context), collapse = "]["), "]")
            counter <- if (!is.null(current) && !is.null(total)) {
                sprintf(" %d/%d", as.integer(current), as.integer(total))
            } else {
                ""
            }
            shift__ui_fit(sprintf("%s%s %s", prefix, counter, message), private$width())
        },

        # Capture node, case, and output outcomes while keeping their event
        # persistence independent from terminal rendering.
        capture_business_result = function(message, details) {
            if (identical(details$unit_type, "index_node")) {
                row <- data.table::data.table(
                    node = shift__node_label(details$node),
                    future = shift_coalesce(details$future_files, NA_integer_),
                    reference = shift_coalesce(details$reference_files, NA_integer_),
                    result = if (details$outcome %in% c("completed", "skipped")) {
                        shift_coalesce(details$result, "selected")
                    } else {
                        shift_coalesce(details$error, message)
                    }
                )
                private$node_rows <- data.table::rbindlist(
                    list(private$node_rows, row), use.names = TRUE, fill = TRUE)
            }
            if (identical(details$unit_type, "epw_export") &&
                details$outcome %in% c("completed", "skipped")) {
                private$outputs_completed <- max(private$outputs_completed,
                    as.integer(shift_coalesce(details$current, 0L)))
            }
            invisible(NULL)
        },

        # Print a compact resolver-attempt table after resolve or immediately
        # before a resolve failure; result text receives the remaining width.
        render_node_table = function(force = FALSE) {
            rows <- private$node_rows
            if (is.null(rows) || !nrow(rows)) {
                return(invisible(NULL))
            }
            for (line in shift__ui_node_table(rows, width = private$width())) {
                private$emit("verbatim", line)
            }
            invisible(NULL)
        },

        # Print the user-level case matrix rather than exposing extraction-plan
        # rows as the main progress model.
        render_case_table = function(force = FALSE,
                                     detail = private$ui_value@detail) {
            rows <- private$case_rows
            if (is.null(rows) || !nrow(rows) ||
                (!isTRUE(force) && !shift__ui_at_least(private$ui_value, "normal"))) {
                return(invisible(NULL))
            }
            for (line in shift__ui_case_table(rows, width = private$width(),
                detail = detail)) {
                private$emit("verbatim", line)
            }
            invisible(NULL)
        }
    )
)

# Construct a reporter after a run and optional job have durable identities.
shift__reporter <- function(ui = shift_ui(), store = NULL, run_id = NULL,
                            job_id = NULL, background = FALSE) {
    ShiftReporter$new(
        ui = ui,
        store = store,
        run_id = run_id,
        job_id = job_id,
        background = background
    )
}

# Format workflow durations without pretending that remote work has a reliable
# ETA while it is still running.
shift__format_elapsed <- function(seconds) {
    seconds <- max(0, round(as.numeric(seconds)))
    hours <- seconds %/% 3600L
    minutes <- (seconds %% 3600L) %/% 60L
    secs <- seconds %% 60L
    if (hours > 0L) {
        return(sprintf("%dh %02dm %02ds", hours, minutes, secs))
    }
    if (minutes > 0L) {
        return(sprintf("%dm %02ds", minutes, secs))
    }
    sprintf("%ds", secs)
}
