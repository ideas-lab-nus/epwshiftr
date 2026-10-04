#' @include shift-ui.R
NULL

# Reconstruct display state from plans, persisted events and live snapshots.

# Return the exact execution-stage sequence implied by one plan. Reporter stage
# events persist the next stage so a background watch does not need to rebuild
# the scientific plan merely to explain what comes next.
# shift_ui_state__ui_stage_sequence {{{
shift_ui_state__ui_stage_sequence <- function(plan) {
    reference <- plan@meta$reference
    reference_expected <- S7::S7_inherits(reference, ShiftReferenceSpec) &&
        identical(reference@mode, "historical")
    c(
        "resolve",
        if (identical(plan@meta$control@download, "always")) "download",
        "extract_future",
        if (isTRUE(reference_expected)) "extract_reference",
        "coverage",
        "morph",
        "write_epw"
    )
}
# }}}

# Extract the compact scientific context carried inside every foreground frame.
# Keeping this separate from the startup receipt lets dynamic mode replace its
# first frame instead of leaving a duplicated five-line transcript behind.
# shift_ui_state__ui_plan_context {{{
shift_ui_state__ui_plan_context <- function(plan) {
    request <- plan@meta$request@meta
    model <- paste(
        as.character(shift_stage__coalesce(request$source, "<model>")),
        collapse = ", "
    )
    scenarios <- paste(
        as.character(shift_stage__coalesce(
            request$experiment,
            "<scenario>"
        )),
        collapse = " + "
    )
    reference <- shift_ui_view__ui_reference(plan@meta$reference)
    expected <- nrow(plan@meta$expected_cases)
    transform_label <- plan@meta$transform@label
    items <- c(
        model,
        scenarios,
        shift_ui_view__ui_periods(plan@meta$periods),
        sprintf("%s / %s", transform_label, reference),
        if (identical(plan@meta$transform@output_type, "multi_year")) {
            sprintf("%d cases; one EPW per weather year", expected)
        } else {
            sprintf("%d EPW%s", expected, if (expected == 1L) "" else "s")
        },
        if (!is.null(plan@meta$observed_reference)) {
            paste(
                "Observed",
                shift_ui_view__ui_reference(plan@meta$observed_reference)
            )
        }
    )
    list(
        line = paste(items, collapse = " \u00b7 "),
        items = items,
        selection = shift_ui_view__ui_selection(plan),
        output = shift_stage__coalesce(
            plan@meta$epw$export_dir,
            "<output directory>"
        )
    )
}
# }}}

# Select the stage-specific count used by the determinate progress row.
# shift_ui_state__ui_progress_values {{{
shift_ui_state__ui_progress_values <- function(state) {
    details <- shift_stage__coalesce(state$current_details, list())
    current <- shift_ui_view__ui_metric_number(
        details,
        "current",
        shift_stage__coalesce(state$unit_current, NA_real_)
    )
    total <- shift_ui_view__ui_metric_number(
        details,
        "total",
        shift_stage__coalesce(state$unit_total, NA_real_)
    )
    if (identical(state$stage, "coverage")) {
        current <- as.numeric(shift_stage__coalesce(state$cases_ready, 0L))
        total <- as.numeric(shift_stage__coalesce(state$cases_total, 0L))
    } else if (
        !identical(state$stage, "resolve") &&
            identical(details$phase, "unit") &&
            is.null(details$outcome) &&
            !is.na(current)
    ) {
        # Compact backends use the same completed-unit meaning as boxed views.
        current <- max(0L, current - 1L)
    }
    list(current = current, total = total)
}
# }}}

# Decode persisted event details without allowing a malformed historical event
# to break shift_watch() for the rest of an otherwise readable run.
# shift_ui_state__ui_event_details {{{
shift_ui_state__ui_event_details <- function(events) {
    if (!nrow(events)) {
        return(list())
    }
    if (!"details_json" %in% names(events)) {
        return(rep(list(list()), nrow(events)))
    }
    lapply(events$details_json, function(value) {
        if (
            is.null(value) || !length(value) || is.na(value) || !nzchar(value)
        ) {
            return(list())
        }
        tryCatch(
            jsonlite::fromJSON(value, simplifyVector = TRUE),
            error = function(e) list()
        )
    })
}
# }}}

# Rebuild the planned stage route from the persisted scientific specification
# before a queued worker has emitted its first reporter event.
# shift_ui_state__ui_stage_sequence_from_row {{{
shift_ui_state__ui_stage_sequence_from_row <- function(row) {
    row <- data.table::as.data.table(row)
    if (
        !nrow(row) ||
            !"spec_json" %in% names(row) ||
            is.na(row$spec_json[[1L]]) ||
            !nzchar(row$spec_json[[1L]])
    ) {
        return(character())
    }
    spec <- tryCatch(
        jsonlite::fromJSON(row$spec_json[[1L]], simplifyVector = TRUE),
        error = function(e) NULL
    )
    if (is.null(spec)) {
        return(character())
    }
    task <- as.character(shift_stage__coalesce(spec$task, "future_epw"))[[1L]]
    if (!identical(task, "future_epw")) {
        current <- as.character(shift_stage__coalesce(
            row$current_stage[[1L]],
            task
        ))
        return(current)
    }
    reference_mode <- if (is.null(spec$reference)) {
        "none"
    } else {
        as.character(shift_stage__coalesce(spec$reference$mode, "none"))[[1L]]
    }
    observed_reference_mode <- if (is.null(spec$observed_reference)) {
        "none"
    } else {
        as.character(shift_stage__coalesce(
            spec$observed_reference$mode,
            "none"
        ))[[1L]]
    }
    download <- as.character(shift_stage__coalesce(
        spec$control$download,
        "auto"
    ))[[
        1L
    ]]
    c(
        "resolve",
        if (identical(download, "always")) "download",
        "extract_future",
        if (identical(reference_mode, "historical")) "extract_reference",
        if (identical(observed_reference_mode, "historical")) {
            "extract_observed_reference"
        },
        "coverage",
        "morph",
        "write_epw"
    )
}
# }}}

# Format the named period list stored in a canonical workflow specification
# without reconstructing a complete ShiftPlan in watch clients.
# shift_ui_state__ui_periods_from_spec {{{
shift_ui_state__ui_periods_from_spec <- function(periods) {
    if (is.null(periods) || !length(periods)) {
        return("no periods")
    }
    if (is.atomic(periods) && !is.null(names(periods))) {
        periods <- split(as.integer(periods), names(periods))
    }
    labels <- names(periods)
    if (is.null(labels) || !length(labels)) {
        labels <- rep("period", length(periods))
    }
    paste(
        vapply(
            seq_along(periods),
            function(i) {
                years <- suppressWarnings(as.integer(periods[[i]]))
                years <- years[!is.na(years)]
                if (!length(years)) {
                    return(labels[[i]])
                }
                if (length(unique(years)) == 1L) {
                    sprintf("%s (%d)", labels[[i]], years[[1L]])
                } else {
                    sprintf(
                        "%s (%d\u2013%d)",
                        labels[[i]],
                        min(years),
                        max(years)
                    )
                }
            },
            character(1L)
        ),
        collapse = ", "
    )
}
# }}}

# Describe a persisted reference using only explicit values in the run spec;
# this display helper never infers a historical reference from missing data.
# shift_ui_state__ui_reference_from_spec {{{
shift_ui_state__ui_reference_from_spec <- function(reference) {
    mode <- as.character(shift_stage__coalesce(reference$mode, "none"))[[1L]]
    if (identical(mode, "none")) {
        return("no reference")
    }
    if (identical(mode, "reanalysis")) {
        years <- as.integer(unlist(reference$years, use.names = FALSE))
        return(sprintf(
            "%s %s %d\u2013%d (%s)",
            toupper(reference$dataset),
            reference$product,
            min(years),
            max(years),
            reference$access
        ))
    }
    if (identical(mode, "historical")) {
        periods <- shift_ui_state__ui_periods_from_spec(reference$periods)
        periods <- sub("^[^(]+ \\(", "", periods)
        periods <- sub("\\)$", "", periods)
        return(paste("historical", periods))
    }
    "supplied reference"
}
# }}}

# Rebuild the one-line dashboard context from persisted intent so foreground,
# R watch, and CLI watch retain the same visual hierarchy across sessions.
# shift_ui_state__ui_plan_context_from_row {{{
shift_ui_state__ui_plan_context_from_row <- function(row, cases_total = 0L) {
    row <- data.table::as.data.table(row)
    if (
        !nrow(row) ||
            !"spec_json" %in% names(row) ||
            is.na(row$spec_json[[1L]]) ||
            !nzchar(row$spec_json[[1L]])
    ) {
        return(list())
    }
    spec <- tryCatch(
        jsonlite::fromJSON(row$spec_json[[1L]], simplifyVector = TRUE),
        error = function(e) NULL
    )
    if (is.null(spec)) {
        return(list())
    }
    task <- as.character(shift_stage__coalesce(spec$task, "future_epw"))[[1L]]
    if (!identical(task, "future_epw")) {
        current <- as.character(shift_stage__coalesce(
            row$current_stage[[1L]],
            task
        ))
        label <- shift_run__task_label(current)
        return(list(
            title = label,
            line = label,
            items = c(
                label,
                sprintf(
                    "store %s",
                    shift_print__display_path(shift_stage__coalesce(
                        spec$store,
                        "<store>"
                    ))
                )
            ),
            selection = NULL,
            output = if ("output_dir" %in% names(row)) {
                row$output_dir[[1L]]
            } else {
                NULL
            }
        ))
    }
    climate <- shift_stage__coalesce(spec$climate, spec$request)
    model <- paste(
        as.character(shift_stage__coalesce(
            climate$model,
            climate$source
        )),
        collapse = ", "
    )
    scenarios <- paste(
        as.character(shift_stage__coalesce(
            climate$scenarios,
            climate$experiment
        )),
        collapse = " + "
    )
    transform <- shift_stage__coalesce(spec$transform$method, "transform")
    reference <- shift_ui_state__ui_reference_from_spec(spec$reference)
    expected <- as.integer(cases_total)
    line <- c(
        if (nzchar(model)) model,
        if (nzchar(scenarios)) scenarios,
        shift_ui_state__ui_periods_from_spec(spec$periods),
        sprintf("%s / %s", transform, reference),
        if (expected > 0L) sprintf("%d cases", expected),
        if (
            !is.null(spec$observed_reference) &&
                !identical(spec$observed_reference$mode, "none")
        ) {
            paste(
                "Observed",
                shift_ui_state__ui_reference_from_spec(spec$observed_reference)
            )
        }
    )
    member_value <- shift_stage__coalesce(
        spec$climate$member,
        spec$request$variant
    )
    grid_value <- shift_stage__coalesce(
        spec$climate$grid,
        spec$request$filters$grid_label
    )
    table_value <- if (!is.null(spec$climate)) {
        spec$climate$table
    } else {
        spec$request$filters$table_id
    }
    member <- if (is.null(member_value)) {
        "member auto"
    } else {
        sprintf("member %s", paste(member_value, collapse = ", "))
    }
    grid <- if (is.null(grid_value)) {
        "grid auto"
    } else {
        sprintf("grid %s", paste(grid_value, collapse = ", "))
    }
    tables <- sprintf(
        "tables %s",
        shift_print__format_cmip6_tables(table_value)
    )
    list(
        line = paste(line, collapse = " \u00b7 "),
        items = line,
        selection = paste(member, grid, tables, sep = " \u00b7 "),
        output = if ("output_dir" %in% names(row)) {
            row$output_dir[[1L]]
        } else {
            NULL
        }
    )
}
# }}}

# Reconstruct the same semantic live state from persisted tables that the
# foreground reporter maintains in memory.
# shift_ui_state__ui_table_state {{{
shift_ui_state__ui_table_state <- function(row, events, cases) {
    row <- data.table::as.data.table(row)
    events <- data.table::as.data.table(events)
    cases <- data.table::as.data.table(cases)
    details <- shift_ui_state__ui_event_details(events)
    stage <- row$current_stage[[1L]]
    stage_indices <- which(vapply(
        details,
        function(x) {
            isTRUE(x$phase %in% c("stage", "operation")) &&
                identical(x$stage, stage)
        },
        logical(1L)
    ))
    stage_index <- if (length(stage_indices)) {
        utils::tail(stage_indices, 1L)
    } else {
        NA_integer_
    }
    unit_indices <- which(vapply(
        details,
        function(x) {
            identical(x$phase, "unit") && identical(x$stage, stage)
        },
        logical(1L)
    ))
    unit_index <- if (length(unit_indices)) {
        utils::tail(unit_indices, 1L)
    } else {
        NA_integer_
    }
    milestone_indices <- which(
        events$status %in%
            c(
                "completed",
                "skipped",
                "rejected",
                "fallback",
                "failed",
                "cancelled",
                "partial"
            )
    )
    last_index <- if (length(milestone_indices)) {
        utils::tail(milestone_indices, 1L)
    } else {
        NA_integer_
    }
    failure_indices <- which(events$status %in% c("failed", "cancelled"))
    failure_index <- if (length(failure_indices)) {
        utils::tail(failure_indices, 1L)
    } else {
        NA_integer_
    }
    recent_indices <- utils::tail(milestone_indices, 3L)
    completed_stages <- unique(vapply(
        seq_along(details),
        function(i) {
            operation_done <- identical(details[[i]]$phase, "operation") &&
                isTRUE(details[[i]]$outcome %in% c("completed", "partial"))
            if (
                (identical(details[[i]]$phase, "stage") &&
                    identical(events$status[[i]], "completed")) ||
                    operation_done
            ) {
                as.character(details[[i]]$stage)
            } else {
                NA_character_
            }
        },
        character(1L)
    ))
    completed_stages <- completed_stages[!is.na(completed_stages)]
    started_at <- shift_stage__coalesce(
        row$started_at[[1L]],
        as.POSIXct(NA, tz = "UTC")
    )
    stopped_at <- shift_stage__coalesce(
        row$completed_at[[1L]],
        as.POSIXct(NA, tz = "UTC")
    )
    if (is.na(stopped_at)) {
        terminal <- row$status[[1L]] %in%
            c("waiting", "completed", "partial", "failed", "cancelled")
        # Older or partially written terminal rows may lack completed_at. In
        # that case freeze elapsed time at their last durable activity rather
        # than making a completed or failed run appear to keep executing.
        if (
            isTRUE(terminal) &&
                "updated_at" %in% names(row) &&
                !is.na(row$updated_at[[1L]])
        ) {
            stopped_at <- row$updated_at[[1L]]
        } else if (
            isTRUE(terminal) &&
                nrow(events) &&
                "created_at" %in% names(events) &&
                !is.na(events$created_at[[nrow(events)]])
        ) {
            stopped_at <- events$created_at[[nrow(events)]]
        } else {
            stopped_at <- Sys.time()
        }
    }
    elapsed <- if (is.na(started_at)) {
        0
    } else {
        as.numeric(difftime(
            stopped_at,
            started_at,
            units = "secs"
        ))
    }
    stage_details <- if (is.na(stage_index)) list() else details[[stage_index]]
    unit_details <- if (is.na(unit_index)) list() else details[[unit_index]]
    stage_sequence <- as.character(shift_stage__coalesce(
        stage_details$stage_sequence,
        character()
    ))
    if (!length(stage_sequence)) {
        stage_sequence <- shift_ui_state__ui_stage_sequence_from_row(row)
    }
    fallback_stage_message <- switch(
        row$status[[1L]],
        queued = "Waiting for background worker",
        waiting = "Ready for the next shift stage",
        completed = "Workflow completed",
        partial = "Workflow completed with missing cases",
        failed = "Workflow failed",
        cancelled = "Workflow cancelled",
        stopping = "Waiting for cancellation boundary",
        "Waiting for next workflow event"
    )
    plan_context <- shift_ui_state__ui_plan_context_from_row(row, nrow(cases))
    output_paths <- if ("export_path" %in% names(cases)) {
        as.character(cases$export_path)
    } else {
        character()
    }
    # Export outcomes count physical files even when one case spans many years.
    # Older snapshots retain their available path evidence when no such event exists.
    exported <- vapply(
        details,
        function(value) {
            if (
                identical(value$unit_type, "epw_export") &&
                    isTRUE(value$outcome %in% c("completed", "skipped"))
            ) {
                as.integer(shift_stage__coalesce(value$current, 0L))
            } else {
                0L
            }
        },
        integer(1L)
    )
    exported <- max(c(
        0L,
        exported,
        length(unique(output_paths[
            !is.na(output_paths) & nzchar(output_paths)
        ]))
    ))
    list(
        run_id = row$run_id[[1L]],
        task_label = shift_stage__coalesce(plan_context$title, "Future EPW"),
        status = row$status[[1L]],
        stage = stage,
        stage_message = if (is.na(stage_index)) {
            fallback_stage_message
        } else {
            events$message[[stage_index]]
        },
        stage_current = stage_details$current,
        stage_total = stage_details$total,
        unit_label = if (is.na(unit_index)) {
            NULL
        } else {
            events$message[[unit_index]]
        },
        unit_current = unit_details$current,
        unit_total = unit_details$total,
        current_details = unit_details,
        next_stage = stage_details$next_stage,
        stage_sequence = stage_sequence,
        completed_stages = completed_stages,
        plan_context = plan_context,
        cases_ready = sum(
            cases$status %in% c("ready", "morphing", "morphed", "completed")
        ),
        cases_total = if (nrow(cases)) nrow(cases) else 0L,
        outputs_completed = exported,
        output_dir = plan_context$output,
        output_paths = output_paths,
        output_path_limit = 5L,
        result_summary = if (
            row$status[[1L]] %in%
                c("waiting", "completed", "partial") &&
                !is.na(last_index)
        ) {
            events$message[[last_index]]
        } else {
            NULL
        },
        last_event = if (is.na(last_index)) {
            "No completed event yet"
        } else {
            events$message[[last_index]]
        },
        recent_events = if (!length(recent_indices)) {
            character()
        } else {
            as.character(events$message[recent_indices])
        },
        recent_outcomes = if (!length(recent_indices)) {
            character()
        } else {
            as.character(events$status[recent_indices])
        },
        node_rows = shift_ui_state__ui_event_nodes(events),
        failure_details = if (is.na(failure_index)) {
            list()
        } else {
            details[[failure_index]]
        },
        elapsed_seconds = elapsed
    )
}
# }}}

# Reconstruct the resolver-attempt table from terminal index-node events.
# shift_ui_state__ui_event_nodes {{{
shift_ui_state__ui_event_nodes <- function(events) {
    events <- data.table::as.data.table(events)
    details <- shift_ui_state__ui_event_details(events)
    rows <- lapply(seq_along(details), function(i) {
        value <- details[[i]]
        if (
            !identical(value$unit_type, "index_node") ||
                !events$status[[i]] %in%
                    c("completed", "skipped", "rejected", "failed")
        ) {
            return(NULL)
        }
        data.table::data.table(
            node = shift_ui_view__node_label(value$node),
            future = shift_stage__coalesce(value$future_files, NA_integer_),
            reference = shift_stage__coalesce(
                value$reference_files,
                NA_integer_
            ),
            outcome = as.character(events$status[[i]]),
            duration = if (is.null(value$elapsed_seconds)) {
                "\u2014"
            } else {
                shift_ui_view__format_elapsed(value$elapsed_seconds)
            },
            result = if (events$status[[i]] %in% c("completed", "skipped")) {
                shift_stage__coalesce(value$result, "selected")
            } else {
                error <- shift_stage__coalesce(value$error, events$message[[i]])
                kind <- shift_stage__coalesce(
                    value$error_kind,
                    shift_ui_view__ui_error_kind(error)
                )
                sprintf("%s: %s", kind, error)
            }
        )
    })
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
# }}}

# Merge transient progress into a persisted run state. The run status remains
# authoritative when a cancellation arrives after the worker's last frame.
# shift_ui_state__ui_live_state {{{
shift_ui_state__ui_live_state <- function(row, state, ui_state = NULL) {
    if (
        !length(ui_state) ||
            !nrow(row) ||
            !row$status[[1L]] %in% c("queued", "running", "stopping")
    ) {
        return(state)
    }
    state[names(ui_state)] <- ui_state
    state$status <- row$status[[1L]]
    if (length(row$started_at) && !is.na(row$started_at[[1L]])) {
        # Elapsed time keeps advancing between worker heartbeat snapshots.
        state$elapsed_seconds <- max(
            0,
            as.numeric(difftime(
                shift_job__watch_now(),
                row$started_at[[1L]],
                units = "secs"
            ))
        )
    }
    state
}
# }}}

# Select an event delta before applying any presentation limit so a watch
# client cannot silently lose milestones when more than one page arrives
# between polls. A missing cursor is reported separately because bounded live
# sidecars may legitimately have discarded older events.
# shift_ui_state__ui_event_delta {{{
shift_ui_state__ui_event_delta <- function(
    events,
    last_event_id = NA_character_,
    initial_limit = 10L,
    initial = is.na(last_event_id)
) {
    events <- data.table::as.data.table(events)
    checkmate::assert_count(initial_limit, positive = FALSE)
    checkmate::assert_flag(initial)
    newest <- if (nrow(events)) {
        as.character(events$event_id[[nrow(events)]])
    } else {
        NA_character_
    }
    if (isTRUE(initial)) {
        rows <- if (initial_limit == 0L) {
            events[0]
        } else {
            utils::tail(
                events,
                initial_limit
            )
        }
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
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
