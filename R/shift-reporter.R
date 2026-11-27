#' @include shift-ui-state.R shift-ui-render.R
NULL

# Own transient progress state and publish durable workflow milestones.

# Normalize event details to a stable JSON shape shared by reporters, persisted
# run events, and CLI/R watch views.
# shift_reporter__progress_details {{{
shift_reporter__progress_details <- function(
    stage = NULL,
    phase = NULL,
    unit_type = NULL,
    unit_label = NULL,
    current = NULL,
    total = NULL,
    node = NULL,
    scenario = NULL,
    variable = NULL,
    period = NULL,
    access_method = NULL,
    elapsed_seconds = NULL,
    outcome = NULL,
    ...
) {
    vals <- c(
        list(
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
        ),
        list(...)
    )
    vals[!vapply(vals, is.null, logical(1L))]
}
# }}}

# ShiftReporter is the single runtime sink for workflow messages, live DuckDB
# snapshots, and durable milestone events.
# ShiftReporter {{{
ShiftReporter <- R6::R6Class(
    "ShiftReporter",
    lock_class = TRUE,
    public = list(
        initialize = function(
            ui = shift_ui(),
            store = NULL,
            run_id = NULL,
            job_id = NULL,
            background = FALSE,
            step_id = NULL,
            execution = NULL
        ) {
            sess <- shift_ui_render__open_session(ui)
            private$execution <- execution
            private$ui_value <- ui
            private$mode_value <- sess$mode
            private$motion_value <- sess$motion
            private$renderer <- sess$renderer
            private$store <- store
            private$run_id_value <- run_id
            private$job_id_value <- job_id
            private$step_id_value <- step_id
            private$background <- isTRUE(background)
            private$started_at <- Sys.time()
            private$last_heartbeat <- as.POSIXct(NA)
            private$last_liveness <- as.POSIXct(NA)
            private$last_refresh <- as.POSIXct(NA)
            private$status <- if (isTRUE(background)) "queued" else "running"
        },

        operation_started = function(
            task,
            label,
            context = list(),
            stage_sequence = task,
            completed_stages = character()
        ) {
            checkmate::assert_string(task, min.chars = 1L)
            checkmate::assert_string(label, min.chars = 1L)
            checkmate::assert_list(context)
            private$task_label <- label
            private$status <- "running"
            private$stage <- task
            private$stage_sequence <- unique(as.character(stage_sequence))
            private$completed_stages <- unique(as.character(completed_stages))
            private$stage_started_at <- Sys.time()
            private$stage_message <- shift_stage__coalesce(
                context$message,
                paste("Preparing", tolower(label))
            )
            store_str <- shift_print__display_path(shift_stage__coalesce(
                context$store,
                "<store>"
            ))
            private$plan_context <- utils::modifyList(
                list(
                    title = label,
                    items = c(label, paste("store", store_str)),
                    selection = NULL,
                    output = NULL
                ),
                context
            )
            private$next_stage <- private$next_stage_after(task)
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else if (!identical(private$mode_value, "none")) {
                msg <- if (is.null(private$run_id_value)) {
                    paste(label, "started.")
                } else {
                    sprintf("%s run %s started.", label, private$run_id_value)
                }
                private$emit("info", msg)
                b <- private$ui_value@batch_context
                if (identical(b$kind, "discovery") && length(b$message)) {
                    private$emit_lines(shift_ui_render__labeled_lines(
                        "Discovery",
                        b$message,
                        private$width()
                    ))
                }
                private$emit_lines(shift_ui_view__ui_plan_lines(
                    private$plan_context,
                    private$width()
                ))
            }
            private$persist(
                task,
                "running",
                private$stage_message,
                shift_reporter__progress_details(
                    stage = task,
                    phase = "operation",
                    unit_type = "shift_operation",
                    outcome = "running",
                    stage_sequence = private$stage_sequence,
                    next_stage = private$next_stage
                )
            )
            invisible(self)
        },

        discovery_updated = function(context, reset = FALSE) {
            if (identical(context$method, "original_morphing")) {
                context$method_label <- "Belcher original Morphing"
            }
            private$ui_value@batch_context <- utils::modifyList(
                private$ui_value@batch_context,
                context,
                keep.null = TRUE
            )
            if (reset) {
                private$current_details <- NULL
            }
            b <- private$ui_value@batch_context
            private$stage_message <- shift_stage__coalesce(
                b$scope,
                "Preparing model discovery"
            )
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else if (
                identical(private$mode_value, "log") &&
                    !is.null(b$method_label) &&
                    is.null(context$selected_models)
            ) {
                parts <- c(
                    sprintf(
                        "[Discovery][method %d/%d][inputs %d/%d] %s",
                        b$current,
                        b$total,
                        b$alternative,
                        b$alternatives,
                        b$method_label
                    ),
                    b$scope,
                    b$scope_periods,
                    if (!is.null(b$node)) shift_ui_view__node_label(b$node)
                )
                private$emit("verbatim", paste(parts, collapse = " \u00b7 "))
            }
            invisible(self)
        },

        operation_completed = function(
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            private$finish_operation(
                "completed",
                summary,
                output_paths,
                output_dir
            )
        },
        operation_partial = function(
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            private$finish_operation(
                "partial",
                summary,
                output_paths,
                output_dir
            )
        },
        operation_waiting = function(
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            private$finish_operation(
                "waiting",
                summary,
                output_paths,
                output_dir
            )
        },
        operation_detached = function(
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            private$finish_operation(
                "running",
                summary,
                output_paths,
                output_dir
            )
        },
        operation_failed = function(
            message,
            cancelled = FALSE,
            details = list()
        ) {
            self$run_failed(message, cancelled = cancelled, details = details)
        },

        run_started = function(plan, run_id, background = FALSE) {
            private$run_id_value <- run_id
            private$background <- isTRUE(background)
            private$status <- if (isTRUE(background)) "queued" else "running"
            private$task_label <- "Future EPW"
            private$cases_total <- nrow(plan@meta$expected_cases)
            private$stage_sequence <- shift_ui_state__ui_stage_sequence(plan)
            private$plan_context <- shift_ui_state__ui_plan_context(plan)
            if (!identical(private$mode_value, "none")) {
                if (
                    identical(private$mode_value, "dynamic") &&
                        !isTRUE(background)
                ) {
                    private$stage <- if (length(private$stage_sequence)) {
                        private$stage_sequence[[1L]]
                    } else {
                        "planned"
                    }
                    private$next_stage <- private$next_stage_after(
                        private$stage
                    )
                    private$stage_message <- "Preparing resolver"
                    private$render_dynamic(force = TRUE)
                } else {
                    sm <- shift_ui_view__ui_plan_summary(
                        plan,
                        run_id,
                        background = background,
                        width = private$width(),
                        detail = private$ui_value@detail
                    )
                    private$emit("info", sm[[1L]])
                    private$emit_lines(sm[-1L])
                }
                if (isTRUE(background)) {
                    qstore <- encodeString(plan@store_path, quote = '"')
                    cmds <- c(
                        "Watch  " = "shift_watch",
                        "Cancel " = "shift_cancel",
                        "Logs   " = "shift_logs"
                    )
                    for (lbl in names(cmds)) {
                        private$emit(
                            "text",
                            sprintf(
                                '%s %s("%s", store = %s)',
                                lbl,
                                cmds[[lbl]],
                                run_id,
                                qstore
                            )
                        )
                    }
                }
            }
            invisible(self)
        },

        stage_started = function(
            stage,
            message,
            current = NULL,
            total = NULL,
            details = list()
        ) {
            private$stage <- stage
            private$status <- "running"
            private$stage_message <- message
            private$stage_current <- current
            private$stage_total <- total
            private$stage_started_at <- Sys.time()
            private$current_details <- NULL
            private$next_stage <- private$next_stage_after(stage)
            ev <- utils::modifyList(
                shift_reporter__progress_details(
                    stage = stage,
                    phase = "stage",
                    current = current,
                    total = total,
                    next_stage = private$next_stage,
                    stage_sequence = private$stage_sequence
                ),
                details
            )
            private$publish_event(
                stage = stage,
                status = "running",
                message = message,
                details = ev,
                current = current,
                total = total,
                log_type = "info",
                format_details = list(stage = stage, phase = "stage")
            )
            invisible(self)
        },

        unit_started = function(
            message,
            current = NULL,
            total = NULL,
            details = list()
        ) {
            private$unit_started_at <- Sys.time()
            private$current_details <- utils::modifyList(
                shift_reporter__progress_details(
                    stage = private$stage,
                    phase = "unit",
                    unit_label = message,
                    unit_base_label = message,
                    current = current,
                    total = total
                ),
                details
            )
            private$publish_event(
                stage = private$stage,
                status = "running",
                message = message,
                details = private$current_details,
                current = current,
                total = total,
                log_type = "verbatim"
            )
            invisible(self)
        },

        unit_completed = function(
            message,
            current = NULL,
            total = NULL,
            outcome = "completed",
            details = list()
        ) {
            ev <- private$merge_unit_details(
                message,
                current = current,
                total = total,
                outcome = outcome,
                details = details,
                elapsed = private$elapsed(private$unit_started_at)
            )
            private$current_details <- ev
            private$last_event <- message
            b <- private$ui_value@batch_context
            recent <- if (identical(b$kind, "discovery")) {
                paste(b$scope, message, sep = ": ")
            } else {
                message
            }
            private$add_recent(recent, outcome)
            private$capture_business_result(message, ev)
            warn_out <- outcome %in% c("failed", "fallback")
            log_type <- if (identical(outcome, "failed")) {
                "warning"
            } else if (outcome %in% c("fallback", "rejected")) {
                "verbatim"
            } else {
                "success"
            }
            private$publish_event(
                stage = private$stage,
                status = outcome,
                message = message,
                details = ev,
                current = current,
                total = total,
                log_type = log_type,
                emit_log = shift_ui__ui_at_least(private$ui_value, "detail") ||
                    warn_out,
                emit_dynamic_warning = warn_out
            )
            invisible(self)
        },

        unit_updated = function(
            message,
            current = NULL,
            total = NULL,
            details = list()
        ) {
            ev <- private$merge_unit_details(
                message,
                current,
                total,
                "updated",
                details
            )
            private$current_details <- ev
            private$publish_event(
                stage = private$stage,
                status = "updated",
                message = message,
                details = ev,
                current = current,
                total = total,
                log_type = "verbatim",
                emit_log = shift_ui__ui_at_least(private$ui_value, "detail")
            )
            invisible(self)
        },

        unit_skipped = function(
            message,
            current = NULL,
            total = NULL,
            details = list()
        ) {
            self$unit_completed(
                message,
                current,
                total,
                outcome = "skipped",
                details = details
            )
        },

        notice = function(message, outcome = "info", details = list()) {
            ev <- utils::modifyList(
                shift_reporter__progress_details(
                    stage = private$stage,
                    phase = "notice",
                    outcome = outcome
                ),
                details
            )
            if (
                outcome %in%
                    c(
                        "completed",
                        "skipped",
                        "rejected",
                        "fallback",
                        "failed",
                        "cancelled"
                    )
            ) {
                private$last_event <- message
                private$add_recent(message, outcome)
            }
            warn_out <- outcome %in% c("failed", "fallback")
            private$publish_event(
                stage = private$stage,
                status = outcome,
                message = message,
                details = ev,
                log_type = if (warn_out) "warning" else "verbatim",
                emit_dynamic_warning = warn_out
            )
            invisible(self)
        },

        cases_updated = function(cases, show = FALSE) {
            private$case_rows <- data.table::as.data.table(data.table::copy(
                cases
            ))
            private$cases_total <- nrow(private$case_rows)
            private$cases_ready <- sum(
                private$case_rows$status %in%
                    c("ready", "morphing", "morphed", "completed")
            )
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            if (
                isTRUE(show) &&
                    !identical(private$mode_value, "none") &&
                    (!identical(private$mode_value, "dynamic") ||
                        shift_ui__ui_at_least(private$ui_value, "detail"))
            ) {
                private$render_table(
                    private$case_rows,
                    shift_ui_view__ui_case_table
                )
            }
            invisible(self)
        },

        check_cancel = function(stage = private$stage) {
            shift_execution__check_cancel(private$execution, stage)
            if (
                is.null(private$execution) &&
                    !is.null(private$store) &&
                    !is.null(private$run_id_value) &&
                    !is.null(private$job_id_value)
            ) {
                shift_job__job_check_cancel(
                    private$store,
                    private$run_id_value,
                    private$job_id_value,
                    stage
                )
            }
            invisible(FALSE)
        },

        stage_completed = function(message, details = list()) {
            elapsed <- private$elapsed(private$stage_started_at)
            private$last_event <- message
            private$completed_stages <- unique(c(
                private$completed_stages,
                private$stage
            ))
            private$add_recent(message, "completed")
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
                if (
                    identical(private$stage, "resolve") &&
                        shift_ui__ui_at_least(private$ui_value, "detail")
                ) {
                    private$render_table(
                        private$node_rows,
                        shift_ui_view__ui_node_table
                    )
                }
            } else if (!identical(private$mode_value, "none")) {
                private$emit(
                    "success",
                    private$format_event(
                        message,
                        details = list(stage = private$stage, phase = "stage")
                    )
                )
                if (identical(private$stage, "resolve")) {
                    private$render_table(
                        private$node_rows,
                        shift_ui_view__ui_node_table
                    )
                }
            }
            private$persist(
                private$stage,
                "completed",
                message,
                utils::modifyList(
                    shift_reporter__progress_details(
                        stage = private$stage,
                        phase = "stage",
                        elapsed_seconds = elapsed,
                        outcome = "completed"
                    ),
                    details
                )
            )
            invisible(self)
        },

        heartbeat = function(message = NULL, details = list(), force = FALSE) {
            shift_execution__checkpoint(private$execution, details)
            now <- Sys.time()
            private$current_details <- utils::modifyList(
                shift_stage__coalesce(
                    private$current_details,
                    shift_reporter__progress_details(
                        stage = private$stage,
                        phase = "unit"
                    )
                ),
                details
            )
            d <- private$current_details
            label <- shift_stage__coalesce(
                message,
                shift_stage__coalesce(
                    d$unit_base_label,
                    shift_stage__coalesce(d$unit_label, "Working")
                )
            )
            elapsed <- private$elapsed(private$unit_started_at)
            private$current_details$unit_base_label <- shift_stage__coalesce(
                d$unit_base_label,
                label
            )
            private$current_details$unit_label <- label
            private$current_details$elapsed_seconds <- elapsed
            due <- private$is_due(
                private$last_heartbeat,
                max(1, private$ui_value@heartbeat),
                force,
                now
            )
            if (due) {
                private$last_heartbeat <- now
                self$check_cancel(shift_stage__coalesce(
                    private$stage,
                    "working"
                ))
                private$touch_job(force = TRUE, urgent = isTRUE(force))
            } else if (
                !is.null(private$store) && !is.null(private$run_id_value)
            ) {
                try(
                    shift_job__live_snapshot_flush(
                        private$store,
                        private$run_id_value
                    ),
                    silent = TRUE
                )
            }
            if (identical(private$mode_value, "none")) {
                return(invisible(due))
            }
            if (identical(private$mode_value, "dynamic")) {
                return(invisible(
                    isTRUE(private$render_dynamic(force = force)) || due
                ))
            }
            if (due) {
                st <- sprintf(
                    "%s (%s elapsed)",
                    label,
                    shift_ui_view__format_elapsed(elapsed)
                )
                private$emit(
                    "verbatim",
                    private$format_event(st, details = private$current_details)
                )
            }
            invisible(due)
        },

        run_completed = function(run, outputs = data.table::data.table()) {
            elapsed <- private$elapsed(private$started_at)
            status <- shift_status(run, refresh = FALSE)
            private$status <- status
            comp <- shift_inspect__completion(
                shift_cases(run, refresh = FALSE),
                outputs,
                shift_diagnostics(run, refresh = FALSE)
            )
            private$result_summary <- comp$result_summary
            private$warning_messages <- comp$warning_messages
            private$field_summary <- comp$field_summary
            private$outputs_completed <- nrow(outputs)
            if (identical(status, "completed")) {
                private$completed_stages <- private$stage_sequence
            }
            paths <- shift_ui_state__nonempty(shift_stage__coalesce(
                outputs$export_path,
                outputs$path
            ))
            out_dir <- if (
                nrow(run@meta$run) && "output_dir" %in% names(run@meta$run)
            ) {
                shift_ui_state__chr1(run@meta$run$output_dir)
            }
            if (is.null(out_dir) && length(paths)) {
                out_dir <- dirname(paths[[1L]])
            }
            private$output_paths <- paths
            private$output_dir <- out_dir
            private$output_path_limit <- if (
                shift_ui__ui_at_least(private$ui_value, "detail")
            ) {
                Inf
            } else {
                5L
            }
            private$add_recent(
                sprintf("%d EPW output(s) ready", nrow(outputs)),
                status
            )
            if (!isTRUE(private$commit_dynamic_frame("done"))) {
                private$emit(
                    "success",
                    sprintf(
                        "Future EPW run %s %s: %d output(s) in %s.",
                        private$run_id_value,
                        status,
                        nrow(outputs),
                        shift_ui_view__format_elapsed(elapsed)
                    )
                )
                private$emit("text", private$result_summary)
                for (m in utils::head(private$warning_messages, 3L)) {
                    private$emit("warning", m)
                }
                if (!identical(private$mode_value, "none") && nrow(outputs)) {
                    if (!is.null(out_dir)) {
                        private$emit(
                            "text",
                            paste(
                                "Output directory:",
                                shift_print__display_path(out_dir)
                            )
                        )
                    }
                    if (shift_ui__ui_at_least(private$ui_value, "detail")) {
                        for (p in paths) {
                            private$emit("path", p)
                        }
                    }
                }
            }
            invisible(self)
        },

        run_failed = function(
            message = NULL,
            cancelled = FALSE,
            details = list()
        ) {
            private$status <- if (isTRUE(cancelled)) "cancelled" else "failed"
            private$failure_details <- shift_stage__coalesce(details, list())
            term_msg <- shift_stage__coalesce(
                message,
                shift_stage__coalesce(
                    private$failure_details$summary,
                    if (isTRUE(cancelled)) {
                        "Workflow cancelled"
                    } else {
                        "Workflow failed"
                    }
                )
            )
            if (!is.null(term_msg)) {
                private$last_event <- term_msg
                private$add_recent(term_msg, private$status)
                private$current_details <- utils::modifyList(
                    shift_stage__coalesce(private$current_details, list()),
                    list(
                        unit_label = term_msg,
                        unit_base_label = term_msg,
                        outcome = private$status
                    )
                )
            }
            was_dyn <- identical(private$mode_value, "dynamic")
            if (was_dyn) {
                private$render_dynamic(force = TRUE)
            }
            private$close_renderer(
                if (isTRUE(cancelled)) "cancelled" else "failed",
                preserve = TRUE
            )
            if (!isTRUE(cancelled)) {
                if (
                    !was_dyn ||
                        shift_ui__ui_at_least(private$ui_value, "detail")
                ) {
                    private$render_table(
                        private$node_rows,
                        shift_ui_view__ui_node_table
                    )
                    private$render_table(
                        private$case_rows,
                        shift_ui_view__ui_case_table,
                        detail = "detail"
                    )
                }
            } else if (!is.null(term_msg) && !was_dyn) {
                private$emit("warning", term_msg)
            }
            invisible(self)
        },

        detail = function(message, level = c("detail", "debug")) {
            if (shift_ui__ui_at_least(private$ui_value, match.arg(level))) {
                private$emit("text", message)
            }
            invisible(self)
        },

        mode = function() private$mode_value,
        ui = function() private$ui_value,
        run_id = function() private$run_id_value,
        job_id = function() private$job_id_value,
        step_id = function() private$step_id_value,
        context = function() {
            shift_stage__coalesce(private$current_details, list())
        },
        snapshot = function() private$view_state(),
        close = function() {
            private$close_renderer("done")
            invisible(self)
        }
    ),
    private = list(
        execution = NULL,
        ui_value = NULL,
        mode_value = NULL,
        motion_value = NULL,
        store = NULL,
        run_id_value = NULL,
        job_id_value = NULL,
        step_id_value = NULL,
        background = FALSE,
        status = NULL,
        stage = NULL,
        renderer = NULL,
        started_at = NULL,
        stage_started_at = NULL,
        unit_started_at = NULL,
        last_heartbeat = NULL,
        last_liveness = NULL,
        last_refresh = NULL,
        animation_frame = 0L,
        current_details = NULL,
        stage_message = NULL,
        stage_current = NULL,
        stage_total = NULL,
        stage_sequence = character(),
        completed_stages = character(),
        next_stage = NULL,
        last_event = NULL,
        recent_events = character(),
        recent_outcomes = character(),
        cases_ready = 0L,
        cases_total = 0L,
        outputs_completed = 0L,
        node_rows = NULL,
        case_rows = NULL,
        plan_context = NULL,
        failure_details = list(),
        output_dir = NULL,
        output_paths = character(),
        output_path_limit = 5L,
        task_label = "Future EPW",
        result_summary = NULL,
        warning_messages = character(),
        field_summary = NULL,

        emit = function(type, message) {
            if (identical(private$mode_value, "none")) {
                return(invisible(NULL))
            }
            private$with_output(function() {
                tryCatch(
                    switch(
                        type,
                        success = cli::cli_alert_success("{message}"),
                        warning = cli::cli_alert_warning("{message}"),
                        danger = cli::cli_alert_danger("{message}"),
                        info = cli::cli_alert_info("{message}"),
                        verbatim = cli::cli_verbatim(message),
                        path = cli::cli_text("  {.path {message}}"),
                        cli::cli_text("{message}")
                    ),
                    error = function(e) invisible(NULL)
                )
            })
            invisible(NULL)
        },

        emit_lines = function(lines) {
            for (l in lines) {
                private$emit("verbatim", l)
            }
            invisible(NULL)
        },

        with_output = function(code) {
            if (is.null(private$renderer)) {
                code()
            } else {
                private$renderer$suspend(code)
            }
        },

        publish_event = function(
            stage,
            status,
            message,
            details,
            current = NULL,
            total = NULL,
            log_type = "verbatim",
            emit_log = TRUE,
            emit_dynamic_warning = FALSE,
            format_details = details
        ) {
            formatted <- private$format_event(
                message,
                current,
                total,
                format_details
            )
            if (identical(private$mode_value, "dynamic")) {
                if (isTRUE(emit_dynamic_warning)) {
                    private$emit("warning", formatted)
                }
                private$render_dynamic(force = TRUE)
            } else if (
                isTRUE(emit_log) && !identical(private$mode_value, "none")
            ) {
                private$emit(log_type, formatted)
            }
            private$persist(stage, status, message, details)
            invisible(NULL)
        },

        persist = function(stage, status, message, details) {
            if (is.null(private$store) || is.null(private$run_id_value)) {
                return(invisible(NULL))
            }
            shift_job__run_event(
                private$store,
                private$run_id_value,
                stage,
                status,
                message,
                details,
                snapshot = FALSE,
                step_id = private$step_id_value
            )
            private$touch_job(
                force = TRUE,
                urgent = status %in%
                    c("completed", "partial", "failed", "cancelled")
            )
            invisible(NULL)
        },

        is_due = function(last, interval, force = FALSE, now = Sys.time()) {
            isTRUE(force) ||
                is.na(last) ||
                as.numeric(difftime(now, last, units = "secs")) >= interval
        },

        next_stage_after = function(stage) {
            pos <- match(stage, private$stage_sequence)
            if (
                length(pos) &&
                    !is.na(pos) &&
                    pos < length(private$stage_sequence)
            ) {
                private$stage_sequence[[pos + 1L]]
            }
        },

        merge_unit_details = function(
            message,
            current = NULL,
            total = NULL,
            outcome = NULL,
            details = list(),
            elapsed = NULL
        ) {
            fields <- list(
                unit_label = message,
                unit_base_label = message,
                current = current,
                total = total,
                outcome = outcome
            )
            if (!is.null(elapsed)) {
                fields$elapsed_seconds <- elapsed
            }
            utils::modifyList(
                shift_stage__coalesce(
                    private$current_details,
                    shift_reporter__progress_details(stage = private$stage)
                ),
                c(details, fields)
            )
        },

        commit_dynamic_frame = function(result = "done") {
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            backend <- if (!is.null(private$renderer)) {
                tryCatch(private$renderer$backend(), error = function(e) NULL)
            }
            committed <- identical(private$mode_value, "dynamic") &&
                identical(backend, "frame")
            private$close_renderer(result = result, preserve = TRUE)
            committed
        },

        touch_job = function(force = FALSE, urgent = FALSE) {
            now <- Sys.time()
            if (
                !private$is_due(
                    private$last_liveness,
                    max(1, private$ui_value@heartbeat),
                    force,
                    now
                )
            ) {
                return(invisible(FALSE))
            }
            private$last_liveness <- now
            if (
                !is.null(private$store) &&
                    !is.null(private$job_id_value) &&
                    exists("shift_job__job_touch", mode = "function")
            ) {
                try(
                    shift_job__job_touch(
                        private$store,
                        private$job_id_value,
                        ui_state = private$view_state(),
                        urgent = urgent
                    ),
                    silent = TRUE
                )
            }
            invisible(TRUE)
        },

        close_renderer = function(result = "done", preserve = FALSE) {
            if (!is.null(private$renderer)) {
                if (isTRUE(preserve) && is.function(private$renderer$commit)) {
                    private$renderer$commit(result = result)
                } else {
                    private$renderer$close(result = result)
                }
                private$renderer <- NULL
            }
            invisible(NULL)
        },

        elapsed = function(start) {
            if (is.null(start) || !length(start) || is.na(start)) {
                0
            } else {
                as.numeric(difftime(Sys.time(), start, units = "secs"))
            }
        },

        width = function() shift_ui_render__width(),

        add_recent = function(message, outcome) {
            private$recent_events <- utils::tail(
                c(private$recent_events, message),
                3L
            )
            private$recent_outcomes <- utils::tail(
                c(private$recent_outcomes, outcome),
                3L
            )
            invisible(message)
        },

        view_state = function() {
            d <- shift_stage__coalesce(private$current_details, list())
            list(
                run_id = private$run_id_value,
                task_label = private$task_label,
                status = private$status,
                stage = private$stage,
                stage_message = private$stage_message,
                stage_current = private$stage_current,
                stage_total = private$stage_total,
                unit_label = d$unit_label,
                unit_current = d$current,
                unit_total = d$total,
                current_details = d,
                next_stage = private$next_stage,
                stage_sequence = private$stage_sequence,
                completed_stages = private$completed_stages,
                cases_ready = private$cases_ready,
                cases_total = private$cases_total,
                outputs_completed = private$outputs_completed,
                last_event = private$last_event,
                recent_events = private$recent_events,
                recent_outcomes = private$recent_outcomes,
                node_rows = private$node_rows,
                plan_context = private$plan_context,
                failure_details = private$failure_details,
                output_dir = private$output_dir,
                output_paths = private$output_paths,
                output_path_limit = private$output_path_limit,
                result_summary = private$result_summary,
                warning_messages = private$warning_messages,
                field_summary = private$field_summary,
                batch_context = private$ui_value@batch_context,
                detail = private$ui_value@detail,
                elapsed_seconds = private$elapsed(private$started_at)
            )
        },

        finish_operation = function(
            status,
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            checkmate::assert_choice(
                status,
                c("completed", "partial", "waiting", "running")
            )
            checkmate::assert_string(summary, min.chars = 1L)
            private$status <- status
            private$result_summary <- summary
            private$last_event <- summary
            private$completed_stages <- unique(c(
                private$completed_stages,
                private$stage
            ))
            private$output_paths <- as.character(output_paths)
            private$output_dir <- output_dir
            private$add_recent(summary, status)
            private$current_details <- utils::modifyList(
                shift_stage__coalesce(private$current_details, list()),
                list(
                    unit_label = summary,
                    unit_base_label = summary,
                    outcome = status
                )
            )
            if (
                !isTRUE(private$commit_dynamic_frame("done")) &&
                    !identical(private$mode_value, "none")
            ) {
                ds <- if (identical(status, "waiting")) "ready" else status
                private$emit(
                    if (identical(status, "completed")) "success" else "info",
                    sprintf(
                        "%s run %s %s: %s",
                        private$task_label,
                        private$run_id_value,
                        ds,
                        summary
                    )
                )
            }
            invisible(self)
        },

        render_dynamic = function(force = TRUE) {
            if (!identical(private$mode_value, "dynamic")) {
                return(invisible(FALSE))
            }
            now <- Sys.time()
            if (
                !private$is_due(
                    private$last_refresh,
                    private$ui_value@refresh,
                    force,
                    now
                )
            ) {
                return(invisible(FALSE))
            }
            private$last_refresh <- now
            private$animation_frame <- private$animation_frame + 1L
            state <- private$view_state()
            w <- private$width()
            lines <- shift_ui_view__ui_status_lines(
                state,
                w,
                private$motion_value,
                private$animation_frame
            )
            compact <- shift_ui_view__ui_compact_line(
                state,
                w,
                private$motion_value,
                private$animation_frame
            )
            ok <- !is.null(private$renderer) &&
                isTRUE(private$renderer$draw(lines, compact = compact))
            if (!ok) {
                private$fallback_to_log(lines)
                return(invisible(FALSE))
            }
            invisible(TRUE)
        },

        fallback_to_log = function(lines) {
            private$close_renderer("failed")
            private$mode_value <- "log"
            private$motion_value <- "none"
            private$emit(
                "warning",
                "Dynamic progress is unavailable; switched to line-by-line logs."
            )
            private$emit_lines(lines)
            st <- shift_stage__coalesce(private$stage, "ui")
            private$persist(
                st,
                "warning",
                "Dynamic progress was unavailable; switched to line-by-line logs.",
                shift_reporter__progress_details(
                    stage = st,
                    phase = "notice",
                    unit_type = "ui",
                    outcome = "fallback"
                )
            )
            invisible(NULL)
        },

        format_event = function(
            message,
            current = NULL,
            total = NULL,
            details = list()
        ) {
            ctx <- shift_ui_render__event_context(
                shift_stage__coalesce(details$stage, private$stage),
                details = details,
                detail = private$ui_value@detail,
                include_phase = TRUE
            )
            cnt <- if (!is.null(current) && !is.null(total)) {
                sprintf(" %d/%d", as.integer(current), as.integer(total))
            } else {
                ""
            }
            sprintf("[%s]%s %s", paste(ctx, collapse = "]["), cnt, message)
        },

        capture_business_result = function(message, details) {
            if (identical(details$unit_type, "index_node")) {
                row <- shift_ui_state__node_row(
                    details,
                    details$outcome,
                    message,
                    elapsed_default = 0
                )
                private$node_rows <- data.table::rbindlist(
                    list(private$node_rows, row),
                    use.names = TRUE,
                    fill = TRUE
                )
            }
            if (
                identical(details$unit_type, "epw_export") &&
                    details$outcome %in% c("completed", "skipped")
            ) {
                private$outputs_completed <- max(
                    private$outputs_completed,
                    as.integer(shift_stage__coalesce(details$current, 0L))
                )
            }
            invisible(NULL)
        },

        render_table = function(
            rows,
            formatter,
            detail = private$ui_value@detail
        ) {
            if (is.null(rows) || !nrow(rows)) {
                return(invisible(NULL))
            }
            private$with_output(function() {
                private$emit_lines(formatter(
                    rows,
                    width = private$width(),
                    detail = detail
                ))
            })
            invisible(NULL)
        }
    )
)
# }}}

# shift_reporter__reporter {{{
shift_reporter__reporter <- function(
    ui = shift_ui(),
    store = NULL,
    run_id = NULL,
    job_id = NULL,
    background = FALSE,
    step_id = NULL,
    execution = NULL
) {
    ShiftReporter$new(ui, store, run_id, job_id, background, step_id, execution)
}
# }}}

# shift_reporter__with_operation {{{
shift_reporter__with_operation <- function(
    ui,
    task,
    label,
    context,
    code,
    interrupt_message = "Operation interrupted."
) {
    reporter <- shift_reporter__reporter(ui)
    on.exit(reporter$close(), add = TRUE)
    reporter$operation_started(task, label, context = context)
    tryCatch(
        code(reporter),
        error = function(error) {
            reporter$operation_failed(conditionMessage(error))
            stop(error)
        },
        interrupt = function(error) {
            reporter$operation_failed(interrupt_message, cancelled = TRUE)
            stop(error)
        }
    )
}
# }}}

# shift_reporter__ui_check {{{
shift_reporter__ui_check <- function(ui, label, code) {
    shift_reporter__with_operation(
        ui = ui,
        task = "check",
        label = label,
        context = list(items = label, message = paste("Preparing", label)),
        code = code,
        interrupt_message = "Check interrupted."
    )
}
# }}}

# vim: fdm=marker :
