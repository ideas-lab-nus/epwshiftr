#' @include shift-ui.R
NULL

# Own transient progress state and publish durable workflow milestones.

# Normalize event details to a stable JSON shape shared by Console reporters,
# persisted run events, and CLI/R watch views.
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
    values <- c(
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
    values[!vapply(values, is.null, logical(1L))]
}
# }}}

# ShiftReporter is the single runtime sink for workflow messages and durable
# milestone events. Heartbeats remain transient to avoid frequent store writes.
# ShiftReporter {{{
ShiftReporter <- R6::R6Class(
    "ShiftReporter",
    lock_class = TRUE,
    public = list(
        # Bind one reporter to a stable run/job identity and resolve its
        # presentation mode once for the lifetime of the execution attempt.
        # initialize {{{
        initialize = function(
            ui = shift_ui(),
            store = NULL,
            run_id = NULL,
            job_id = NULL,
            background = FALSE,
            step_id = NULL,
            execution = NULL
        ) {
            if (!S7::S7_inherits(ui, ShiftUiOptions)) {
                cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
            }
            private$execution <- execution
            private$ui_value <- ui
            private$mode_value <- shift_ui__ui_mode(ui)
            private$motion_value <- shift_ui__ui_motion(ui, private$mode_value)
            private$renderer <- tryCatch(
                shift_tui__ui_renderer(private$mode_value),
                # error {{{
                error = function(e) NULL
                # }}}
            )
            # An explicitly requested dynamic mode still degrades safely when
            # the current output connection has no live rendering capability.
            if (
                identical(private$mode_value, "dynamic") &&
                    is.null(private$renderer)
            ) {
                private$mode_value <- "log"
                private$motion_value <- "none"
            }
            private$store <- store
            private$run_id_value <- run_id
            private$job_id_value <- job_id
            private$step_id_value <- step_id
            private$background <- isTRUE(background)
            private$started_at <- Sys.time()
            private$last_heartbeat <- as.POSIXct(NA)
            private$last_liveness <- as.POSIXct(NA)
            private$last_refresh <- as.POSIXct(NA)
            private$animation_frame <- 0L
            private$status <- if (isTRUE(background)) "queued" else "running"
        },
        # }}}

        # Start a generic standalone shift operation without requiring a
        # Future EPW plan. The same semantic state feeds foreground frames,
        # persisted events, and later shift_watch() reconstruction.
        # operation_started {{{
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
            private$plan_context <- utils::modifyList(
                list(
                    title = label,
                    items = c(
                        label,
                        sprintf(
                            "store %s",
                            shift_print__display_path(shift_stage__coalesce(
                                context$store,
                                "<store>"
                            ))
                        )
                    ),
                    selection = NULL,
                    output = NULL
                ),
                context
            )
            position <- match(task, private$stage_sequence)
            private$next_stage <- if (
                !is.na(position) &&
                    position < length(private$stage_sequence)
            ) {
                private$stage_sequence[[position + 1L]]
            } else {
                NULL
            }
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else if (!identical(private$mode_value, "none")) {
                private$emit(
                    "info",
                    if (is.null(private$run_id_value)) {
                        paste(label, "started.")
                    } else {
                        sprintf(
                            "%s run %s started.",
                            label,
                            private$run_id_value
                        )
                    }
                )
                # Retain context from older standalone discovery callers. The
                # parent discovery reporter publishes structured updates below.
                batch <- private$ui_value@batch_context
                if (
                    identical(batch$kind, "discovery") && length(batch$message)
                ) {
                    for (line in shift_ui_view__ui_labeled_lines(
                        "Discovery",
                        batch$message,
                        width = private$width()
                    )) {
                        private$emit("verbatim", line)
                    }
                }
                for (line in shift_ui_view__ui_plan_lines(
                    private$plan_context,
                    width = private$width()
                )) {
                    private$emit("verbatim", line)
                }
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
        # }}}

        # Replace transient discovery context without opening another reporter.
        # Reset per-query details at scope boundaries so historical checks never
        # display counters inherited from the preceding future catalog.
        # discovery_updated {{{
        discovery_updated = function(context, reset = FALSE) {
            # Use the recognizable algorithm name in presentation while the
            # scientific transform key and registry label remain unchanged.
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
            batch <- private$ui_value@batch_context
            private$stage_message <- shift_stage__coalesce(
                batch$scope,
                "Preparing model discovery"
            )
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else if (
                identical(private$mode_value, "log") &&
                    !is.null(batch$method_label) &&
                    is.null(context$selected_models)
            ) {
                private$emit(
                    "verbatim",
                    paste(
                        c(
                            sprintf(
                                "[Discovery][method %d/%d][inputs %d/%d] %s",
                                batch$current,
                                batch$total,
                                batch$alternative,
                                batch$alternatives,
                                batch$method_label
                            ),
                            batch$scope,
                            batch$scope_periods,
                            if (!is.null(batch$node)) {
                                shift_ui_view__node_label(batch$node)
                            }
                        ),
                        collapse = " \u00b7 "
                    )
                )
            }
            invisible(self)
        },
        # }}}

        # Commit an operation that produced its final delivery artifact while
        # preserving the dashboard receipt in terminal scrollback.
        # operation_completed {{{
        operation_completed = function(
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            private$finish_operation(
                "completed",
                summary,
                output_paths = output_paths,
                output_dir = output_dir
            )
            invisible(self)
        },
        # }}}

        # Commit a scientifically incomplete stage as a visible terminal
        # receipt instead of presenting it as ready for the next operation.
        # operation_partial {{{
        operation_partial = function(
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            private$finish_operation(
                "partial",
                summary,
                output_paths = output_paths,
                output_dir = output_dir
            )
            invisible(self)
        },
        # }}}

        # Commit one successful intermediate step without terminating its run.
        # The framebuffer closes at the R prompt; the returned stage carries
        # run/step identity into the next invocation.
        # operation_waiting {{{
        operation_waiting = function(
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            private$finish_operation(
                "waiting",
                summary,
                output_paths = output_paths,
                output_dir = output_dir
            )
            invisible(self)
        },
        # }}}

        # Close the caller's framebuffer after handing work to an existing
        # detached subsystem while keeping the durable run in running state.
        # operation_detached {{{
        operation_detached = function(
            summary,
            output_paths = character(),
            output_dir = NULL
        ) {
            private$finish_operation(
                "running",
                summary,
                output_paths = output_paths,
                output_dir = output_dir
            )
            invisible(self)
        },
        # }}}

        # Reuse the established failure receipt for generic operations so
        # Future EPW and standalone stages never print competing error panels.
        # operation_failed {{{
        operation_failed = function(
            message,
            cancelled = FALSE,
            details = list()
        ) {
            self$run_failed(message, cancelled = cancelled, details = details)
        },
        # }}}

        # Render the scientific plan summary before any remote operation and
        # include control commands when a process job has only been queued.
        # run_started {{{
        run_started = function(plan, run_id, background = FALSE) {
            private$run_id_value <- run_id
            private$background <- isTRUE(background)
            private$status <- if (isTRUE(background)) "queued" else "running"
            private$task_label <- "Future EPW"
            private$cases_total <- nrow(plan@meta$expected_cases)
            private$stage_sequence <- shift_ui_state__ui_stage_sequence(plan)
            private$plan_context <- shift_ui_state__ui_plan_context(plan)
            if (!identical(private$mode_value, "none")) {
                # Foreground dynamic runs introduce the plan as a replaceable
                # first frame. Logs and queued jobs retain a permanent receipt.
                if (
                    identical(private$mode_value, "dynamic") &&
                        !isTRUE(background)
                ) {
                    private$stage <- if (length(private$stage_sequence)) {
                        private$stage_sequence[[1L]]
                    } else {
                        "planned"
                    }
                    private$next_stage <- if (
                        length(private$stage_sequence) > 1L
                    ) {
                        private$stage_sequence[[2L]]
                    } else {
                        NULL
                    }
                    private$stage_message <- "Preparing resolver"
                    private$render_dynamic(force = TRUE)
                } else {
                    summary <- shift_ui_view__ui_plan_summary(
                        plan,
                        run_id,
                        background = background,
                        width = private$width(),
                        detail = private$ui_value@detail
                    )
                    private$emit("info", summary[[1L]])
                    for (line in summary[-1L]) {
                        private$emit("verbatim", line)
                    }
                }
                if (isTRUE(background)) {
                    # Background control commands include the exact store path
                    # so they remain valid after the returned R handle is gone.
                    quoted_store <- encodeString(plan@store_path, quote = '"')
                    private$emit(
                        "text",
                        sprintf(
                            "Watch   shift_watch(\"%s\", store = %s)",
                            run_id,
                            quoted_store
                        )
                    )
                    private$emit(
                        "text",
                        sprintf(
                            "Cancel  shift_cancel(\"%s\", store = %s)",
                            run_id,
                            quoted_store
                        )
                    )
                    private$emit(
                        "text",
                        sprintf(
                            "Logs    shift_logs(\"%s\", store = %s)",
                            run_id,
                            quoted_store
                        )
                    )
                }
            }
            invisible(self)
        },
        # }}}

        # Start a durable workflow stage and close any dynamic unit left by the
        # preceding stage before emitting its new status.
        # stage_started {{{
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
            stage_position <- match(stage, private$stage_sequence)
            private$next_stage <- if (
                length(stage_position) &&
                    !is.na(stage_position) &&
                    stage_position < length(private$stage_sequence)
            ) {
                private$stage_sequence[[stage_position + 1L]]
            } else {
                NULL
            }
            if (identical(private$mode_value, "log")) {
                private$emit(
                    "info",
                    private$format_event(
                        message,
                        current = current,
                        total = total,
                        details = list(stage = stage, phase = "stage")
                    )
                )
            } else {
                private$render_dynamic(force = TRUE)
            }
            private$persist(
                stage,
                "running",
                message,
                utils::modifyList(
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
            )
            invisible(self)
        },
        # }}}

        # Start a user-meaningful business unit such as a node, variable, or
        # scenario-period case and initialize dynamic progress when available.
        # unit_started {{{
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
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else {
                private$emit(
                    "verbatim",
                    private$format_event(
                        message,
                        current = current,
                        total = total,
                        details = private$current_details
                    )
                )
            }
            private$persist(
                private$stage,
                "running",
                message,
                private$current_details
            )
            invisible(self)
        },
        # }}}

        # Complete the current business unit with a structured outcome that can
        # later be reconstructed by watch clients.
        # unit_completed {{{
        unit_completed = function(
            message,
            current = NULL,
            total = NULL,
            outcome = "completed",
            details = list()
        ) {
            elapsed <- private$elapsed(private$unit_started_at)
            event_details <- utils::modifyList(
                shift_stage__coalesce(
                    private$current_details,
                    shift_reporter__progress_details(stage = private$stage)
                ),
                c(
                    details,
                    list(
                        unit_label = message,
                        unit_base_label = message,
                        current = current,
                        total = total,
                        elapsed_seconds = elapsed,
                        outcome = outcome
                    )
                )
            )
            private$current_details <- event_details
            private$last_event <- message
            recent <- message
            batch <- private$ui_value@batch_context
            if (identical(batch$kind, "discovery")) {
                recent <- paste(
                    batch$scope,
                    message,
                    sep = ": "
                )
            }
            private$add_recent(recent, outcome)
            private$capture_business_result(message, event_details)
            if (identical(private$mode_value, "dynamic")) {
                if (outcome %in% c("failed", "fallback")) {
                    private$emit(
                        "warning",
                        private$format_event(
                            message,
                            current = current,
                            total = total,
                            details = event_details
                        )
                    )
                }
                private$render_dynamic(force = TRUE)
            } else if (
                shift_ui__ui_at_least(private$ui_value, "detail") ||
                    outcome %in% c("failed", "fallback")
            ) {
                event_type <- if (identical(outcome, "failed")) {
                    "warning"
                } else if (outcome %in% c("fallback", "rejected")) {
                    "verbatim"
                } else {
                    "success"
                }
                private$emit(
                    event_type,
                    private$format_event(
                        message,
                        current = current,
                        total = total,
                        details = event_details
                    )
                )
            }
            private$persist(private$stage, outcome, message, event_details)
            invisible(self)
        },
        # }}}

        # Persist a meaningful change to the current business unit without
        # treating transient animation frames as durable workflow events.
        # unit_updated {{{
        unit_updated = function(
            message,
            current = NULL,
            total = NULL,
            details = list()
        ) {
            event_details <- utils::modifyList(
                shift_stage__coalesce(
                    private$current_details,
                    shift_reporter__progress_details(stage = private$stage)
                ),
                c(
                    details,
                    list(
                        unit_label = message,
                        unit_base_label = message,
                        current = current,
                        total = total,
                        outcome = "updated"
                    )
                )
            )
            private$current_details <- event_details
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            } else if (shift_ui__ui_at_least(private$ui_value, "detail")) {
                private$emit(
                    "verbatim",
                    private$format_event(
                        message,
                        current = current,
                        total = total,
                        details = event_details
                    )
                )
            }
            private$persist(private$stage, "updated", message, event_details)
            invisible(self)
        },
        # }}}

        # Record deterministic resume/reuse outcomes with a dedicated reporter
        # method so callers do not need to encode skipped semantics themselves.
        # unit_skipped {{{
        unit_skipped = function(
            message,
            current = NULL,
            total = NULL,
            details = list()
        ) {
            self$unit_completed(
                message,
                current = current,
                total = total,
                outcome = "skipped",
                details = details
            )
        },
        # }}}

        # Record an operational milestone that is relevant to the current stage
        # but is not itself a countable business unit.
        # notice {{{
        notice = function(message, outcome = "info", details = list()) {
            event_details <- utils::modifyList(
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
            if (identical(private$mode_value, "dynamic")) {
                if (outcome %in% c("failed", "fallback")) {
                    private$emit(
                        "warning",
                        private$format_event(message, details = event_details)
                    )
                }
                private$render_dynamic(force = TRUE)
            } else if (!identical(private$mode_value, "none")) {
                private$emit(
                    if (outcome %in% c("failed", "fallback")) {
                        "warning"
                    } else {
                        "verbatim"
                    },
                    private$format_event(message, details = event_details)
                )
            }
            private$persist(private$stage, outcome, message, event_details)
            invisible(self)
        },
        # }}}

        # Update the user-case snapshot after coverage or output transitions.
        # The same rows are later reconstructed from shift_run_case by watch.
        # cases_updated {{{
        cases_updated = function(cases, show = FALSE) {
            private$case_rows <- data.table::as.data.table(data.table::copy(
                cases
            ))
            private$cases_total <- nrow(private$case_rows)
            private$cases_ready <- sum(
                private$case_rows$status %in%
                    c("ready", "morphing", "morphed", "completed")
            )
            # Case completion must not overwrite the independently measured
            # export count: a single multi-year case can produce many EPWs.
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            if (
                isTRUE(show) &&
                    !identical(private$mode_value, "none") &&
                    (!identical(private$mode_value, "dynamic") ||
                        shift_ui__ui_at_least(private$ui_value, "detail"))
            ) {
                private$render_case_table()
            }
            invisible(self)
        },
        # }}}

        # Check cooperative cancellation at explicit workflow boundaries even
        # when no heartbeat or progress output is currently being rendered.
        # check_cancel {{{
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
        # }}}

        # Close the dynamic unit and persist the terminal milestone for the
        # current stage together with its elapsed time.
        # stage_completed {{{
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
                # The live Recent section already retains this milestone.
                # Only explicit detail mode adds a scrolling resolver table.
                if (
                    identical(private$stage, "resolve") &&
                        shift_ui__ui_at_least(private$ui_value, "detail")
                ) {
                    private$render_node_table()
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
                    private$render_node_table()
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
        # }}}

        # Refresh transient liveness and cancellation state without persisting
        # animation-only heartbeat events in the run history.
        # heartbeat {{{
        heartbeat = function(message = NULL, details = list(), force = FALSE) {
            shift_execution__checkpoint(private$execution, details)
            now <- Sys.time()
            # Keep a stable base label separate from the transient elapsed
            # suffix so repeated heartbeats never grow the displayed message.
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
            label <- shift_stage__coalesce(
                message,
                shift_stage__coalesce(
                    private$current_details$unit_base_label,
                    shift_stage__coalesce(
                        private$current_details$unit_label,
                        "Working"
                    )
                )
            )
            private$current_details$unit_base_label <- shift_stage__coalesce(
                private$current_details$unit_base_label,
                label
            )
            elapsed <- private$elapsed(private$unit_started_at)
            private$current_details$unit_label <- label
            private$current_details$elapsed_seconds <- elapsed
            due_liveness <- isTRUE(force) ||
                is.na(private$last_heartbeat) ||
                as.numeric(difftime(
                    now,
                    private$last_heartbeat,
                    units = "secs"
                )) >=
                    max(1, private$ui_value@heartbeat)
            if (isTRUE(due_liveness)) {
                private$last_heartbeat <- now
                # Cancellation and durable heartbeat checks follow the slower
                # liveness cadence, not the animation frame rate.
                self$check_cancel(shift_stage__coalesce(
                    private$stage,
                    "working"
                ))
                private$touch_job(force = TRUE)
            }
            if (identical(private$mode_value, "none")) {
                return(invisible(due_liveness))
            }
            status <- sprintf(
                "%s (%s elapsed)",
                label,
                shift_ui_view__format_elapsed(elapsed)
            )
            if (identical(private$mode_value, "dynamic")) {
                refreshed <- private$render_dynamic(force = force)
                return(invisible(isTRUE(refreshed) || isTRUE(due_liveness)))
            } else if (isTRUE(due_liveness)) {
                private$emit(
                    "verbatim",
                    private$format_event(
                        status,
                        details = private$current_details
                    )
                )
            }
            invisible(due_liveness)
        },
        # }}}

        # Render one terminal completion receipt from the refreshed run state.
        # Frame terminals commit it to scrollback; compact/log renderers retain
        # the append-only text summary that remains suitable for redirection.
        # run_completed {{{
        run_completed = function(run, outputs = data.table::data.table()) {
            elapsed <- private$elapsed(private$started_at)
            status <- shift_status(run, refresh = FALSE)
            private$status <- status
            completion <- shift_inspect__completion(
                shift_cases(run, refresh = FALSE),
                outputs,
                shift_diagnostics(run, refresh = FALSE)
            )
            private$result_summary <- completion$result_summary
            private$warning_messages <- completion$warning_messages
            private$field_summary <- completion$field_summary
            private$outputs_completed <- nrow(outputs)
            if (identical(status, "completed")) {
                private$completed_stages <- private$stage_sequence
            }
            paths <- shift_stage__coalesce(outputs$export_path, outputs$path)
            paths <- as.character(paths[!is.na(paths) & nzchar(paths)])
            output_dir <- if (
                nrow(run@meta$run) &&
                    "output_dir" %in% names(run@meta$run)
            ) {
                run@meta$run$output_dir[[1L]]
            } else {
                NULL
            }
            if (
                (is.null(output_dir) ||
                    !length(output_dir) ||
                    is.na(output_dir[[1L]]) ||
                    !nzchar(output_dir[[1L]])) &&
                    length(paths)
            ) {
                output_dir <- dirname(paths[[1L]])
            }
            private$output_paths <- paths
            private$output_dir <- output_dir
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
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            renderer_backend <- if (is.null(private$renderer)) {
                NULL
            } else {
                # error {{{
                tryCatch(private$renderer$backend(), error = function(e) NULL)
                # }}}
            }
            committed_frame <- identical(private$mode_value, "dynamic") &&
                identical(renderer_backend, "frame")
            private$close_renderer(result = "done", preserve = TRUE)
            if (!isTRUE(committed_frame)) {
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
                for (message in utils::head(private$warning_messages, 3L)) {
                    private$emit("warning", message)
                }
            }
            if (
                !isTRUE(committed_frame) &&
                    !identical(private$mode_value, "none") &&
                    nrow(outputs)
            ) {
                if (
                    !is.null(output_dir) &&
                        length(output_dir) &&
                        !is.na(output_dir[[1L]]) &&
                        nzchar(output_dir[[1L]])
                ) {
                    private$emit(
                        "text",
                        sprintf(
                            "Output directory: %s",
                            shift_print__display_path(output_dir[[1L]])
                        )
                    )
                }
                if (shift_ui__ui_at_least(private$ui_value, "detail")) {
                    for (path in paths) {
                        private$emit("path", path)
                    }
                }
            }
            invisible(self)
        },
        # }}}

        # Close transient UI resources before showing a terminal failure or
        # cancellation message.
        # run_failed {{{
        run_failed = function(
            message = NULL,
            cancelled = FALSE,
            details = list()
        ) {
            private$status <- if (isTRUE(cancelled)) "cancelled" else "failed"
            private$failure_details <- shift_stage__coalesce(details, list())
            terminal_message <- shift_stage__coalesce(
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
            if (!is.null(terminal_message)) {
                private$last_event <- terminal_message
                private$add_recent(terminal_message, private$status)
                private$current_details <- utils::modifyList(
                    shift_stage__coalesce(private$current_details, list()),
                    list(
                        unit_label = terminal_message,
                        unit_base_label = terminal_message,
                        outcome = private$status
                    )
                )
            }
            was_dynamic <- identical(private$mode_value, "dynamic")
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            private$close_renderer(
                result = if (isTRUE(cancelled)) {
                    "cancelled"
                } else {
                    "failed"
                },
                preserve = TRUE
            )
            # The caller raises the one primary cli condition. Reporter output
            # here is deliberately limited to structured context tables so a
            # failure is never printed once by the reporter and again by rlang.
            if (!isTRUE(cancelled)) {
                # The committed dashboard owns normal dynamic diagnostics.
                # Logs and explicit detail modes retain complete tables.
                if (
                    !isTRUE(was_dynamic) ||
                        shift_ui__ui_at_least(private$ui_value, "detail")
                ) {
                    private$render_node_table(force = TRUE)
                    private$render_case_table(force = TRUE, detail = "detail")
                }
            } else if (!is.null(terminal_message) && !isTRUE(was_dynamic)) {
                private$emit("warning", terminal_message)
            }
            invisible(self)
        },
        # }}}

        # Keep cancellation rendering distinct at call sites while sharing the
        # same cleanup and warning behavior as other terminal failures.
        # run_cancelled {{{
        run_cancelled = function(message) {
            self$run_failed(message, cancelled = TRUE)
        },
        # }}}

        # Emit low-level paths, URLs, and reuse details only when explicitly
        # requested by the caller.
        # detail {{{
        detail = function(message, level = c("detail", "debug")) {
            level <- match.arg(level)
            if (shift_ui__ui_at_least(private$ui_value, level)) {
                private$emit("text", message)
            }
            invisible(self)
        },
        # }}}

        # Expose immutable reporter context to workflow adapters without
        # leaking its mutable private state.
        # mode {{{
        mode = function() private$mode_value,
        # }}}
        # Return the validated UI options used to create this reporter.
        # ui {{{
        ui = function() private$ui_value,
        # }}}
        # Return the durable run identity associated with persisted events.
        # run_id {{{
        run_id = function() private$run_id_value,
        # }}}
        # Return the current execution-attempt identity used for heartbeats.
        # job_id {{{
        job_id = function() private$job_id_value,
        # }}}
        # Return the persisted step currently owning reporter events.
        # step_id {{{
        step_id = function() private$step_id_value,
        # }}}
        # Return the current business context for terminal diagnostics without
        # exposing the reporter's mutable private environment.
        # context {{{
        context = function() {
            shift_stage__coalesce(private$current_details, list())
        },
        # }}}
        # Return the semantic view state for unit tests and alternate renderers.
        # snapshot {{{
        snapshot = function() private$view_state(),
        # }}}

        # Explicitly release the live terminal renderer when a caller exits
        # through an unusual but non-error path.
        # close {{{
        close = function() {
            private$close_renderer(result = "done")
            invisible(self)
        }
        # }}}
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

        # Map reporter message kinds onto cli output while temporarily
        # releasing an active framebuffer. Console rendering failures are
        # contained because presentation must never abort scientific work.
        # emit {{{
        emit = function(type, message) {
            if (identical(private$mode_value, "none")) {
                return(invisible(NULL))
            }
            # emit_one {{{
            emit_one <- function() {
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
                    # error {{{
                    error = function(e) invisible(NULL)
                    # }}}
                )
            }
            # }}}
            private$with_output(emit_one)
            invisible(NULL)
        },
        # }}}

        # Execute a related group of cli emissions under one framebuffer
        # clear/restore cycle so multi-line tables do not flicker row by row.
        # with_output {{{
        with_output = function(code) {
            if (is.null(private$renderer)) {
                return(code())
            }
            private$renderer$suspend(code)
        },
        # }}}

        # Persist one structured milestone and update job liveness as one
        # reporter-side operation.
        # persist {{{
        persist = function(stage, status, message, details) {
            if (is.null(private$store) || is.null(private$run_id_value)) {
                return(invisible(NULL))
            }
            # Job heartbeat persistence immediately snapshots the same event;
            # suppress the first snapshot to avoid two full live JSON rewrites
            # for every reporter milestone.
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
            private$touch_job(force = TRUE)
            invisible(NULL)
        },
        # }}}

        # Best-effort heartbeat updates must never replace the workflow error
        # that triggered reporter cleanup.
        # touch_job {{{
        touch_job = function(force = FALSE) {
            now <- Sys.time()
            due <- isTRUE(force) ||
                is.na(private$last_liveness) ||
                as.numeric(difftime(
                    now,
                    private$last_liveness,
                    units = "secs"
                )) >=
                    max(1, private$ui_value@heartbeat)
            if (!isTRUE(due)) {
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
                        ui_state = private$view_state()
                    ),
                    silent = TRUE
                )
            }
            invisible(TRUE)
        },
        # }}}

        # Release the active framebuffer exactly once. Terminal workflow
        # outcomes commit their final semantic frame; routine cleanup clears
        # transient output.
        # close_renderer {{{
        close_renderer = function(result = "done", preserve = FALSE) {
            if (!is.null(private$renderer)) {
                if (
                    isTRUE(preserve) &&
                        is.function(private$renderer$commit)
                ) {
                    private$renderer$commit(result = result)
                } else {
                    private$renderer$close(result = result)
                }
                private$renderer <- NULL
            }
            invisible(NULL)
        },
        # }}}

        # Normalize missing timestamps to zero so summaries remain renderable
        # during early launch failures.
        # elapsed {{{
        elapsed = function(start) {
            if (is.null(start) || length(start) == 0L || is.na(start)) {
                return(0)
            }
            as.numeric(difftime(Sys.time(), start, units = "secs"))
        },
        # }}}

        # Resolve the output width at render time so tests, IDE resizing, and
        # redirected 80-column logs all share the same clipping behavior.
        # width {{{
        width = function() shift_ui__ui_width(),
        # }}}

        # Keep only user-meaningful terminal milestones in the fixed activity
        # feed. Animation ticks and routine updates never enter this buffer.
        # add_recent {{{
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
        # }}}

        # Assemble the semantic state consumed by the shared status formatter.
        # view_state {{{
        view_state = function() {
            details <- shift_stage__coalesce(private$current_details, list())
            list(
                run_id = private$run_id_value,
                task_label = private$task_label,
                status = private$status,
                stage = private$stage,
                stage_message = private$stage_message,
                stage_current = private$stage_current,
                stage_total = private$stage_total,
                unit_label = details$unit_label,
                unit_current = details$current,
                unit_total = details$total,
                current_details = details,
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
        # }}}

        # Finalize a generic operation in one place so completed and waiting
        # receipts share identical rendering and persistence semantics.
        # finish_operation {{{
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
            if (identical(private$mode_value, "dynamic")) {
                private$render_dynamic(force = TRUE)
            }
            renderer_backend <- if (is.null(private$renderer)) {
                NULL
            } else {
                # error {{{
                tryCatch(private$renderer$backend(), error = function(e) NULL)
                # }}}
            }
            committed_frame <- identical(private$mode_value, "dynamic") &&
                identical(renderer_backend, "frame")
            # Workflow states such as waiting and running are successful UI
            # terminations. The framebuffer owns only terminal outcomes, so
            # translate every non-error operation receipt to its `done` state.
            private$close_renderer(result = "done", preserve = TRUE)
            if (
                !isTRUE(committed_frame) &&
                    !identical(private$mode_value, "none")
            ) {
                display_status <- if (identical(status, "waiting")) {
                    "ready"
                } else {
                    status
                }
                private$emit(
                    if (identical(status, "completed")) "success" else "info",
                    sprintf(
                        "%s run %s %s: %s",
                        private$task_label,
                        private$run_id_value,
                        display_status,
                        summary
                    )
                )
            }
            invisible(NULL)
        },
        # }}}

        # Refresh the complete dashboard as one atomic frame on its own visual
        # cadence; compact terminals receive the matching one-line summary.
        # render_dynamic {{{
        render_dynamic = function(force = TRUE) {
            if (!identical(private$mode_value, "dynamic")) {
                return(invisible(FALSE))
            }
            now <- Sys.time()
            due <- isTRUE(force) ||
                is.na(private$last_refresh) ||
                as.numeric(difftime(
                    now,
                    private$last_refresh,
                    units = "secs"
                )) >=
                    private$ui_value@refresh
            if (!isTRUE(due)) {
                return(invisible(FALSE))
            }
            private$last_refresh <- now
            private$animation_frame <- private$animation_frame + 1L
            state <- private$view_state()
            lines <- shift_ui_view__ui_status_lines(
                state,
                width = private$width(),
                motion = private$motion_value,
                frame = private$animation_frame
            )
            compact <- shift_ui_view__ui_compact_line(
                state,
                width = private$width(),
                motion = private$motion_value,
                frame = private$animation_frame
            )
            refreshed <- !is.null(private$renderer) &&
                isTRUE(private$renderer$draw(lines, compact = compact))
            if (!isTRUE(refreshed)) {
                private$fallback_to_log(lines)
                return(invisible(FALSE))
            }
            invisible(TRUE)
        },
        # }}}

        # Degrade a broken dynamic renderer exactly once to durable line logs.
        # Presentation failures must remain visible without aborting or hiding
        # the scientific workflow that is still running underneath them.
        # fallback_to_log {{{
        fallback_to_log = function(lines) {
            private$close_renderer(result = "failed")
            private$mode_value <- "log"
            private$motion_value <- "none"
            private$emit(
                "warning",
                "Dynamic progress is unavailable; switched to line-by-line logs."
            )
            for (line in lines) {
                private$emit("verbatim", line)
            }
            private$persist(
                shift_stage__coalesce(private$stage, "ui"),
                "warning",
                "Dynamic progress was unavailable; switched to line-by-line logs.",
                shift_reporter__progress_details(
                    stage = shift_stage__coalesce(private$stage, "ui"),
                    phase = "notice",
                    unit_type = "ui",
                    outcome = "fallback"
                )
            )
            invisible(NULL)
        },
        # }}}

        # Prefix append-only log events with stable workflow context. Full URLs
        # are restricted to debug mode while normal logs use short node names.
        # format_event {{{
        format_event = function(
            message,
            current = NULL,
            total = NULL,
            details = list()
        ) {
            stage <- shift_ui_view__ui_stage_label(shift_stage__coalesce(
                details$stage,
                private$stage
            ))
            context <- character()
            node <- details$node
            if (!is.null(node) && length(node) && !is.na(node[[1L]])) {
                node <- as.character(node[[1L]])
                if (!shift_ui__ui_at_least(private$ui_value, "debug")) {
                    node <- shift_ui_view__node_label(node)
                }
                context <- c(context, node)
            }
            phase <- details$catalog_role
            if (
                is.null(phase) &&
                    !identical(details$phase, "stage") &&
                    !identical(details$phase, "unit") &&
                    !identical(details$phase, "notice")
            ) {
                phase <- details$phase
            }
            if (!is.null(phase) && length(phase) && !is.na(phase[[1L]])) {
                context <- c(context, as.character(phase[[1L]]))
            }
            prefix <- paste0(
                "[",
                paste(c(stage, context), collapse = "]["),
                "]"
            )
            counter <- if (!is.null(current) && !is.null(total)) {
                sprintf(" %d/%d", as.integer(current), as.integer(total))
            } else {
                ""
            }
            # Append-only logs must retain the complete message. Width-bounded
            # trimming is reserved for dynamic rows and compact tables.
            sprintf("%s%s %s", prefix, counter, message)
        },
        # }}}

        # Capture node, case, and output outcomes while keeping their event
        # persistence independent from terminal rendering.
        # capture_business_result {{{
        capture_business_result = function(message, details) {
            if (identical(details$unit_type, "index_node")) {
                row <- data.table::data.table(
                    node = shift_ui_view__node_label(details$node),
                    future = shift_stage__coalesce(
                        details$future_files,
                        NA_integer_
                    ),
                    reference = shift_stage__coalesce(
                        details$reference_files,
                        NA_integer_
                    ),
                    outcome = as.character(shift_stage__coalesce(
                        details$outcome,
                        "rejected"
                    )),
                    duration = shift_ui_view__format_elapsed(
                        shift_stage__coalesce(details$elapsed_seconds, 0)
                    ),
                    result = if (
                        details$outcome %in% c("completed", "skipped")
                    ) {
                        shift_stage__coalesce(details$result, "selected")
                    } else {
                        error <- shift_stage__coalesce(details$error, message)
                        kind <- shift_stage__coalesce(
                            details$error_kind,
                            shift_ui_view__ui_error_kind(error)
                        )
                        sprintf("%s: %s", kind, error)
                    }
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
        # }}}

        # Print a compact resolver-attempt table after resolve or immediately
        # before a resolve failure; result text receives the remaining width.
        # render_node_table {{{
        render_node_table = function(force = FALSE) {
            rows <- private$node_rows
            if (is.null(rows) || !nrow(rows)) {
                return(invisible(NULL))
            }
            # private$with_output callback {{{
            private$with_output(function() {
                for (line in shift_ui_view__ui_node_table(
                    rows,
                    width = private$width(),
                    detail = private$ui_value@detail
                )) {
                    private$emit("verbatim", line)
                }
            })
            # }}}
            invisible(NULL)
        },
        # }}}

        # Print the user-level case matrix rather than exposing extraction-plan
        # rows as the main progress model.
        # render_case_table {{{
        render_case_table = function(
            force = FALSE,
            detail = private$ui_value@detail
        ) {
            rows <- private$case_rows
            if (
                is.null(rows) ||
                    !nrow(rows) ||
                    (!isTRUE(force) &&
                        !shift_ui__ui_at_least(private$ui_value, "normal"))
            ) {
                return(invisible(NULL))
            }
            # private$with_output callback {{{
            private$with_output(function() {
                for (line in shift_ui_view__ui_case_table(
                    rows,
                    width = private$width(),
                    detail = detail
                )) {
                    private$emit("verbatim", line)
                }
            })
            # }}}
            invisible(NULL)
        }
        # }}}
    )
)
# }}}

# Construct a reporter after a run and optional job have durable identities.
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
    ShiftReporter$new(
        ui = ui,
        store = store,
        run_id = run_id,
        job_id = job_id,
        background = background,
        step_id = step_id,
        execution = execution
    )
}
# }}}

# Give potentially slow readiness checks a visible lifecycle without creating
# a persisted scientific run. The caller owns the complete final check report.
# shift_reporter__ui_check {{{
shift_reporter__ui_check <- function(ui, label, code) {
    reporter <- shift_reporter__reporter(ui)
    on.exit(reporter$close(), add = TRUE)
    reporter$operation_started(
        "check",
        label,
        context = list(items = label, message = paste("Preparing", label))
    )
    tryCatch(
        code(reporter),
        # error {{{
        error = function(error) {
            reporter$operation_failed(conditionMessage(error))
            stop(error)
        },
        # }}}
        # interrupt {{{
        interrupt = function(error) {
            reporter$operation_failed("Check interrupted.", cancelled = TRUE)
            stop(error)
        }
        # }}}
    )
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
