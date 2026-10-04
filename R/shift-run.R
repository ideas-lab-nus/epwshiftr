#' @include shift-stage.R
NULL

# Execute workflow stages and coordinate their source, transformation and output work.

# Workflow collection uses an explicit ESGF field contract so provider-extra
# metadata cannot change store writes or CMIP6 resolution decisions.
SHIFT_WORKFLOW_FILE_FIELDS <- c(
    "id",
    "dataset_id",
    "master_id",
    "instance_id",
    "tracking_id",
    "version",
    "title",
    "filename",
    "checksum",
    "checksum_type",
    "size",
    "latest",
    "replica",
    "retracted",
    "deprecated",
    "data_node",
    "activity_id",
    "institution_id",
    "source_id",
    "experiment_id",
    "variant_label",
    "frequency",
    "table_id",
    "variable_id",
    "grid_label",
    "datetime_start",
    "datetime_end",
    "url"
)

# Map public standalone functions onto stable task IDs and concise dashboard
# titles. These IDs also form the persisted object-carried step sequence.
# shift_run__task_label {{{
shift_run__task_label <- function(task) {
    labels <- c(
        datasets = "Collect Datasets",
        collect = "Collect CMIP6",
        download = "Download CMIP6",
        extract = "Extract Climate",
        morph = "Morph EPW",
        write_epw = "Write EPW",
        export_epw = "Export EPW"
    )
    key <- as.character(task)[[1L]]
    # Keep package extensions safe even before they add a dedicated title.
    if (key %in% names(labels)) {
        return(unname(labels[[key]]))
    }
    shift_ui_view__ui_stage_label(key)
}
# }}}

# A private dynamic stack transports the one active reporter through S7 dispatch
# and nested stage calls without exposing an implementation parameter publicly.
# Unlike the removed workflow-session scope, this stack lives only for the
# duration of one synchronous operation and never identifies scientific state.
SHIFT_REPORTER_STACK <- new.env(parent = emptyenv())

SHIFT_REPORTER_STACK$values <- list()

# A second private stack carries the number of catalog units owned by a nested
# collect operation. This prevents Dataset helpers from guessing the parent
# task name when they run inside resolve, morph, or another composite stage.
SHIFT_CATALOG_UNIT_TOTAL_STACK <- new.env(parent = emptyenv())

SHIFT_CATALOG_UNIT_TOTAL_STACK$values <- integer()

# Return the most recently scoped value without assigning a global default.
# shift_run__stack_current {{{
shift_run__stack_current <- function(stack, empty = NULL) {
    values <- stack$values
    if (!length(values)) {
        return(empty)
    }
    values[[length(values)]]
}
# }}}

# Evaluate one expression with a temporary value appended to a dynamic stack.
# shift_run__with_stack {{{
shift_run__with_stack <- function(stack, value, code) {
    previous <- stack$values
    stack$values <- c(previous, list(value))
    # Restore the exact preceding container, including its atomic or list type,
    # after normal returns, errors, and nested non-local exits.
    on.exit(stack$values <- previous, add = TRUE)
    force(code)
}
# }}}

# Return the reporter owned by the current operation, if one exists.
# shift_run__current_reporter {{{
shift_run__current_reporter <- function() {
    shift_run__stack_current(SHIFT_REPORTER_STACK)
}
# }}}

# Evaluate one expression with a reporter installed for internal stage methods.
# Nested calls restore the preceding reporter deterministically on every exit.
# shift_run__with_reporter {{{
shift_run__with_reporter <- function(reporter, code) {
    shift_run__with_stack(SHIFT_REPORTER_STACK, reporter, code)
}
# }}}

# Return the catalog-unit scale selected by the nearest composite operation.
# shift_run__catalog_unit_total {{{
shift_run__catalog_unit_total <- function(default = 1L) {
    as.integer(shift_run__stack_current(
        SHIFT_CATALOG_UNIT_TOTAL_STACK,
        empty = default
    ))
}
# }}}

# Evaluate one nested Dataset query on its parent's catalog-unit scale.
# shift_run__with_catalog_unit_total {{{
shift_run__with_catalog_unit_total <- function(total, code) {
    checkmate::assert_int(total, lower = 1L)
    shift_run__with_stack(SHIFT_CATALOG_UNIT_TOTAL_STACK, total, code)
}
# }}}

# Apply an internal stage call under the reporter scope without adding a public
# reporter formal to the shift API.
# shift_run__do_call_with_reporter {{{
shift_run__do_call_with_reporter <- function(reporter, what, args) {
    shift_run__with_reporter(reporter, do.call(what, args))
}
# }}}

# Resume uses a short-lived internal override to append a new attempt to the
# same failed run. This is not ambient user state: it exists only while one
# public stage call is synchronously rebuilt from its persisted step spec.
SHIFT_RUN_OVERRIDE_STACK <- new.env(parent = emptyenv())

SHIFT_RUN_OVERRIDE_STACK$values <- list()

# Return the run selected by the active resume operation, if any.
# shift_run__current_run_override {{{
shift_run__current_run_override <- function() {
    shift_run__stack_current(SHIFT_RUN_OVERRIDE_STACK)
}
# }}}

# Evaluate one reconstructed stage call under a durable run identity.
# shift_run__with_run_override {{{
shift_run__with_run_override <- function(run_id, code) {
    checkmate::assert_string(run_id, min.chars = 1L)
    shift_run__with_stack(SHIFT_RUN_OVERRIDE_STACK, run_id, code)
}
# }}}

# Resolve presentation once per operation. UI state is deliberately not
# inherited from persisted scientific objects and never enters a spec hash.
# shift_run__task_ui {{{
shift_run__task_ui <- function(ui = NULL) {
    value <- shift_stage__coalesce(ui, shift_ui())
    if (!S7::S7_inherits(value, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    value
}
# }}}

# Find the one authoritative store for a task and reject accidental cross-store
# input before any artifact or run row is written.
# shift_run__task_store_value {{{
shift_run__task_store_value <- function(x, store = NULL) {
    input_path <- if (S7::S7_inherits(x, ShiftStage)) x@store_path else NULL
    supplied_path <- if (inherits(store, "EsgStore")) store$path else store
    candidates <- c(input_path, supplied_path)
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    normalized <- unique(vapply(
        candidates,
        # vapply callback {{{
        function(path) {
            normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
        },
        # }}}
        character(1L)
    ))
    if (length(normalized) > 1L) {
        cli::cli_abort(c(
            "A shift task cannot span multiple stores.",
            "x" = "Session, input stage, and `store` must resolve to the same directory."
        ))
    }
    if (inherits(store, "EsgStore")) {
        return(store)
    }
    if (length(normalized)) normalized[[1L]] else store_dir()
}
# }}}

# Read the completed task lineage recursively. Child runs inherit display
# history without mutating a terminal parent or copying its durable steps.
# shift_run__run_task_history {{{
shift_run__run_task_history <- function(store, run_id, seen = character()) {
    if (
        is.null(run_id) || is.na(run_id) || !nzchar(run_id) || run_id %in% seen
    ) {
        return(character())
    }
    wanted_run_id <- run_id
    row <- shift_inspect__rows(store, "shift_run", "run_id", wanted_run_id)
    if (!nrow(row)) {
        return(character())
    }
    spec <- tryCatch(
        jsonlite::fromJSON(row$spec_json[[1L]], simplifyVector = TRUE),
        # error {{{
        error = function(e) list()
        # }}}
    )
    parent <- store__chr1(spec$parent_run_id)
    inherited <- shift_run__run_task_history(store, parent, c(seen, run_id))
    steps <- shift_inspect__rows(
        store,
        "shift_run_step",
        "run_id",
        wanted_run_id
    )
    completed <- as.character(
        steps[
            steps[["status"]] %in%
                c("completed", "partial")
        ]$task
    )
    unique(c(inherited, completed))
}
# }}}

# Build the cumulative stage rail from the input run lineage plus the current
# task. A branched child therefore remains visually connected to its source.
# shift_run__task_sequence {{{
shift_run__task_sequence <- function(store, run_id, task) {
    completed <- shift_run__run_task_history(store, run_id)
    list(
        sequence = unique(c(completed, task)),
        completed = unique(completed)
    )
}
# }}}

# Decide whether an input stage can append to its run. Only the latest completed
# step of a waiting run is a valid continuation point; terminal or stale inputs
# fork a child run so persisted history remains append-only.
# shift_run__task_run_context {{{
shift_run__task_run_context <- function(x, store) {
    ids <- if (S7::S7_inherits(x, ShiftStage)) x@ids else list()
    input_run_id <- store__chr1(ids$run_id)
    input_step_id <- store__chr1(ids$step_id)
    if (is.na(input_run_id) || !nzchar(input_run_id)) {
        return(list(
            run_id = NULL,
            parent_run_id = NULL,
            lineage_id = NULL,
            continued = FALSE
        ))
    }

    row <- shift_inspect__rows(store, "shift_run", "run_id", input_run_id)
    if (!nrow(row)) {
        cli::cli_abort(c(
            "Input stage refers to an unknown shift run {.val {input_run_id}}.",
            "i" = "Use the store that created the stage or recreate the upstream stage."
        ))
    }
    latest <- shift_job__latest_step(store, input_run_id)
    can_continue <- identical(row$status[[1L]], "waiting") &&
        nrow(latest) &&
        !is.na(input_step_id) &&
        nzchar(input_step_id) &&
        identical(latest$step_id[[1L]], input_step_id) &&
        latest$status[[1L]] %in% c("completed", "partial")
    spec <- tryCatch(
        jsonlite::fromJSON(row$spec_json[[1L]], simplifyVector = TRUE),
        # error {{{
        error = function(e) list()
        # }}}
    )
    lineage_id <- as.character(shift_stage__coalesce(
        spec$lineage_id,
        input_run_id
    ))[[
        1L
    ]]
    if (isTRUE(can_continue)) {
        return(list(
            run_id = input_run_id,
            parent_run_id = NULL,
            lineage_id = lineage_id,
            continued = TRUE
        ))
    }
    list(
        run_id = NULL,
        parent_run_id = input_run_id,
        lineage_id = lineage_id,
        continued = FALSE
    )
}
# }}}

# Describe a stage operation without embedding low-level objects in reporter
# state. Detailed IDs remain available through shift_explain() and the store.
# shift_run__task_context {{{
shift_run__task_context <- function(task, x, store) {
    input <- if (S7::S7_inherits(x, ShiftStage)) {
        sprintf("input %s", x@stage)
    } else {
        "new request"
    }
    list(
        title = shift_run__task_label(task),
        items = c(shift_run__task_label(task), input),
        store = store$path,
        message = paste("Preparing", tolower(shift_run__task_label(task)))
    )
}
# }}}

# Summarize the persisted artifact rather than repeating implementation-level
# callbacks in the terminal completion receipt.
# shift_run__task_summary {{{
shift_run__task_summary <- function(task, result) {
    switch(
        task,
        datasets = sprintf(
            "%d Dataset catalog records indexed",
            as.integer(shift_stage__coalesce(result@meta$dataset_count, 0L))
        ),
        collect = sprintf(
            "%d Dataset and %d File catalog records indexed",
            as.integer(shift_stage__coalesce(result@meta$dataset_count, 0L)),
            as.integer(shift_stage__coalesce(result@meta$file_count, 0L))
        ),
        download = {
            session_id <- shift_stage__coalesce(
                result@ids$session_id,
                "download session"
            )
            sprintf("download session %s registered", session_id)
        },
        extract = sprintf(
            "%d extraction plan(s) processed",
            length(shift_stage__coalesce(result@ids$plan_id, character()))
        ),
        morph = sprintf(
            "morph result %s ready",
            shift_stage__coalesce(result@ids$morph_id, "registered")
        ),
        write_epw = sprintf(
            "%d EPW output(s) written",
            nrow(shift_outputs(result))
        ),
        export_epw = sprintf(
            "%d EPW output(s) exported",
            nrow(shift_outputs(result))
        ),
        sprintf("%s completed", shift_run__task_label(task))
    )
}
# }}}

# Return delivery paths for the generic result receipt while leaving other
# stages path-free.
# shift_run__task_output_paths {{{
shift_run__task_output_paths <- function(result) {
    if (!S7::S7_inherits(result, ShiftOutputs)) {
        return(character())
    }
    rows <- shift_outputs(result)
    path <- if ("export_path" %in% names(rows)) rows$export_path else rows$path
    as.character(path[!is.na(path) & nzchar(path)])
}
# }}}

# Attach durable recovery coordinates to the original stage condition without
# replacing its message or call. Callers can inspect run_id/step_id/store while
# interactive users continue to see the reporter's single failure receipt.
# shift_run__task_condition {{{
shift_run__task_condition <- function(condition, run_id, step_id, store_path) {
    condition$run_id <- run_id
    condition$step_id <- step_id
    condition$store <- store_path
    class(condition) <- unique(c("epwshiftr_shift_error", class(condition)))
    condition
}
# }}}

# Execute one public standalone stage through the shared reporter and durable
# run/step state machine. The stage-specific closure remains responsible only
# for scientific work and business-unit progress.
# shift_run__task_execute {{{
shift_run__task_execute <- function(
    task,
    x,
    code,
    store = NULL,
    ui = NULL,
    spec = list(),
    resumable = TRUE,
    nonresumable_reason = NULL,
    auto_complete = FALSE
) {
    checkmate::assert_string(task, min.chars = 1L)
    checkmate::assert_function(code)
    checkmate::assert_list(spec)
    checkmate::assert_flag(resumable)
    checkmate::assert_flag(auto_complete)
    ui <- shift_run__task_ui(ui)
    store_value <- shift_run__task_store_value(x, store)
    opened <- shift_store(store_value, create = TRUE)
    own_store <- !inherits(store_value, "EsgStore")
    if (isTRUE(own_store)) {
        on.exit(try(opened$close(), silent = TRUE), add = TRUE)
    }

    override_run_id <- shift_run__current_run_override()
    context <- if (is.null(override_run_id)) {
        shift_run__task_run_context(x, opened)
    } else {
        list(
            run_id = override_run_id,
            parent_run_id = NULL,
            lineage_id = NULL,
            continued = TRUE
        )
    }
    run_id <- context$run_id
    if (!is.null(override_run_id)) {
        override_run <- shift_job__run_handle(opened, override_run_id)
        override_status <- shift_status(override_run, refresh = FALSE)
        if (!identical(override_status, "waiting")) {
            cli::cli_abort(
                "Resume target {.val {override_run_id}} is not waiting."
            )
        }
    }
    if (is.null(run_id)) {
        run_spec <- c(
            spec,
            list(
                store = opened$path,
                parent_run_id = context$parent_run_id,
                lineage_id = shift_stage__coalesce(context$lineage_id, NULL)
            )
        )
        run_id <- shift_job__task_run_register(
            opened,
            task,
            spec = run_spec,
            status = "queued"
        )
    }
    step <- shift_job__step_create(
        opened,
        run_id,
        task,
        spec,
        input_stage = if (S7::S7_inherits(x, ShiftStage)) x else NULL,
        resumable = resumable,
        nonresumable_reason = nonresumable_reason
    )
    step_id <- step$step_id[[1L]]
    job <- shift_job__job_create(
        opened,
        run_id,
        mode = "foreground",
        ui = ui,
        step_id = step_id
    )
    job_id <- job$job_id[[1L]]
    execution <- shift_execution__context(opened$path, job, opened)
    reporter <- shift_reporter__reporter(
        ui,
        store = opened,
        run_id = run_id,
        job_id = job_id,
        step_id = step_id,
        execution = execution
    )
    on.exit(reporter$close(), add = TRUE)
    sequence <- shift_run__task_sequence(opened, run_id, task)
    reporter$operation_started(
        task,
        shift_run__task_label(task),
        context = shift_run__task_context(task, x, opened),
        stage_sequence = sequence$sequence,
        completed_stages = sequence$completed
    )
    shift_job__run_update(
        opened,
        run_id,
        status = "running",
        current_stage = task,
        completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_
    )

    shift_execution__run(
        execution,
        tryCatch(
            {
                reporter$check_cancel(task)
                result <- code(reporter, opened)
                shift_stage__assert_stage(result)
                result@ids <- utils::modifyList(
                    result@ids,
                    list(run_id = run_id, step_id = step_id)
                )
                artifact_status <- shift_status(result)
                step_status <- if (
                    artifact_status %in% c("partial", "blocked", "failed")
                ) {
                    "partial"
                } else {
                    "completed"
                }
                session_id <- store__chr1(result@ids$session_id)
                detached <- identical(task, "download") &&
                    isTRUE(spec$background) &&
                    !is.na(session_id) &&
                    nzchar(session_id)
                if (isTRUE(detached)) {
                    # The Downloader owns the long-running process after registration.
                    # Keep this step open until shift_run_get() reconciles its durable
                    # session instead of claiming that the next stage is ready.
                    shift_job__step_update(
                        opened,
                        step_id,
                        status = "running",
                        output_stage_json = shift_persist__spec_json(shift_persist__stage_ref(
                            result
                        )),
                        completed_at = as.POSIXct(NA, tz = "UTC"),
                        last_error = NA_character_
                    )
                } else {
                    shift_job__step_finish(
                        opened,
                        step_id,
                        step_status,
                        output_stage = result
                    )
                }
                ids <- result@ids
                run_updates <- list()
                # Each step owns only part of the artifact graph. Preserve identifiers
                # written by upstream steps instead of replacing them with missing
                # fields from the current result object.
                if (!is.null(ids$query_id)) {
                    run_updates$query_id <- store__chr1(ids$query_id)
                }
                if (!is.null(ids$plan_id)) {
                    run_updates$plan_ids_json <-
                        shift_persist__spec_json(as.character(ids$plan_id))
                }
                if (!is.null(ids$morph_id)) {
                    run_updates$morph_id <- store__chr1(ids$morph_id)
                }
                if (!is.null(result@meta$export_dir)) {
                    run_updates$output_dir <- store__chr1(
                        result@meta$export_dir
                    )
                }
                # An empty catalog is not a hand-off point: extraction cannot do useful
                # work without a File record. Other partial stages may still contain a
                # complete subset and therefore retain the established continuation
                # semantics.
                empty_collection <- identical(task, "collect") &&
                    as.integer(shift_stage__coalesce(
                        result@meta$file_count,
                        0L
                    )) <
                        1L
                terminal <- isTRUE(auto_complete) || isTRUE(empty_collection)
                if (isTRUE(detached)) {
                    do.call(
                        shift_job__run_update,
                        c(
                            list(
                                store = opened,
                                run_id = run_id,
                                status = "running",
                                current_stage = task,
                                last_error = NA_character_
                            ),
                            run_updates
                        )
                    )
                } else if (isTRUE(terminal)) {
                    final_status <- shift_job__run_completion_status(
                        opened,
                        run_id
                    )
                    do.call(
                        shift_job__run_finish,
                        c(
                            list(
                                store = opened,
                                run_id = run_id,
                                status = final_status,
                                current_stage = task,
                                last_error = NA_character_
                            ),
                            run_updates
                        )
                    )
                } else {
                    do.call(
                        shift_job__run_update,
                        c(
                            list(
                                store = opened,
                                run_id = run_id,
                                status = "waiting",
                                current_stage = task,
                                last_error = NA_character_
                            ),
                            run_updates
                        )
                    )
                }
                summary <- shift_run__task_summary(task, result)
                paths <- shift_run__task_output_paths(result)
                event_status <- if (isTRUE(detached)) {
                    "running"
                } else if (isTRUE(terminal)) {
                    step_status
                } else {
                    "waiting"
                }
                shift_job__run_event(
                    opened,
                    run_id,
                    task,
                    event_status,
                    summary,
                    details = list(
                        phase = "operation",
                        stage = task,
                        stage_sequence = sequence$sequence,
                        step_id = step_id,
                        outcome = if (isTRUE(detached)) {
                            "running"
                        } else {
                            step_status
                        }
                    ),
                    step_id = step_id
                )
                if (isTRUE(detached)) {
                    reporter$operation_detached(
                        summary,
                        output_paths = paths,
                        output_dir = result@meta$export_dir
                    )
                } else if (isTRUE(empty_collection)) {
                    reporter$operation_partial(
                        summary,
                        output_paths = paths,
                        output_dir = result@meta$export_dir
                    )
                } else if (isTRUE(terminal)) {
                    reporter$operation_completed(
                        summary,
                        output_paths = paths,
                        output_dir = result@meta$export_dir
                    )
                } else {
                    reporter$operation_waiting(
                        summary,
                        output_paths = paths,
                        output_dir = result@meta$export_dir
                    )
                }
                result
            },
            # interrupt {{{
            interrupt = function(e) {
                message <- sprintf(
                    "%s was cancelled.",
                    shift_run__task_label(task)
                )
                try(
                    shift_job__step_finish(
                        opened,
                        step_id,
                        "cancelled",
                        last_error = message
                    ),
                    silent = TRUE
                )
                try(
                    shift_job__run_finish(
                        opened,
                        run_id,
                        "cancelled",
                        current_stage = task,
                        last_error = message
                    ),
                    silent = TRUE
                )
                try(
                    shift_job__run_event(
                        opened,
                        run_id,
                        task,
                        "cancelled",
                        message,
                        details = list(
                            phase = "operation",
                            stage = task,
                            step_id = step_id,
                            outcome = "cancelled"
                        ),
                        step_id = step_id
                    ),
                    silent = TRUE
                )
                reporter$operation_failed(message, cancelled = TRUE)
                stop(shift_run__task_condition(e, run_id, step_id, opened$path))
            },
            # }}}
            # error {{{
            error = function(e) {
                message <- conditionMessage(e)
                cancelled <- inherits(e, "epwshiftr_shift_cancelled")
                outcome <- if (cancelled) "cancelled" else "failed"
                try(
                    shift_job__step_finish(
                        opened,
                        step_id,
                        outcome,
                        last_error = message
                    ),
                    silent = TRUE
                )
                try(
                    shift_job__run_finish(
                        opened,
                        run_id,
                        outcome,
                        current_stage = task,
                        last_error = message
                    ),
                    silent = TRUE
                )
                try(
                    shift_job__run_event(
                        opened,
                        run_id,
                        task,
                        outcome,
                        message,
                        details = list(
                            phase = "operation",
                            stage = task,
                            step_id = step_id,
                            outcome = outcome,
                            cause = message
                        ),
                        step_id = step_id
                    ),
                    silent = TRUE
                )
                reporter$operation_failed(
                    message,
                    cancelled = cancelled,
                    details = list(cause = message)
                )
                stop(shift_run__task_condition(e, run_id, step_id, opened$path))
            }
            # }}}
        )
    )
}
# }}}

# Mark a successful intermediate stage as the intentional endpoint of its run.
# Normal pipelines do not need this helper because EPW export completes the run
# automatically; it exists for workflows that deliberately stop after collect,
# download, extract, morph, or store-local EPW writing.
#' @rdname shift_api
#' @export
# shift_complete {{{
shift_complete <- function(x) {
    shift_stage__assert_stage(x)
    if (S7::S7_inherits(x, ShiftRun)) {
        run <- shift_refresh(x)
        input_step_id <- NA_character_
    } else {
        run <- shift_run_get(x)
        input_step_id <- store__chr1(x@ids$step_id)
    }
    status <- shift_status(run, refresh = FALSE)
    if (status %in% c("completed", "partial")) {
        return(run)
    }
    if (!identical(status, "waiting")) {
        cli::cli_abort(c(
            "Only a waiting shift run can be completed; current status is {.val {status}}.",
            "i" = "Use {.fn shift_resume} for failed or cancelled work."
        ))
    }

    store <- shift_store(run)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    latest <- shift_job__latest_step(store, run@ids$run_id)
    if (!nrow(latest)) {
        cli::cli_abort(
            "Shift run {.val {run@ids$run_id}} has no stage to complete."
        )
    }
    if (
        !S7::S7_inherits(x, ShiftRun) &&
            (is.na(input_step_id) ||
                !identical(input_step_id, latest$step_id[[1L]]))
    ) {
        cli::cli_abort(c(
            "The supplied stage is not the latest result of shift run {.val {run@ids$run_id}}.",
            "i" = "Complete the latest stage or continue from this older stage to create a child run."
        ))
    }
    final_status <- shift_job__run_completion_status(store, run@ids$run_id)
    shift_job__run_finish(
        store,
        run@ids$run_id,
        final_status,
        current_stage = latest$task[[1L]],
        last_error = NA_character_
    )
    shift_job__run_event(
        store,
        run@ids$run_id,
        latest$task[[1L]],
        final_status,
        sprintf(
            "%s marked as the final stage.",
            shift_run__task_label(latest$task[[1L]])
        ),
        details = list(step_id = latest$step_id[[1L]], outcome = final_status),
        step_id = latest$step_id[[1L]]
    )
    shift_job__run_handle(store, run@ids$run_id)
}
# }}}

# Verify that a completed run still owns every required EPW and exported file.
# A terminal database status alone is insufficient after files have been moved
# or manually removed from either the store or the delivery directory.
# shift_run__run_artifacts_complete {{{
shift_run__run_artifacts_complete <- function(store, run_id) {
    wanted_run_id <- run_id
    private <- morpher__private_store(store)
    runs <- private$read_table("shift_run")
    run <- runs[
        runs[["run_id"]] == wanted_run_id &
            runs[["status"]] == "completed"
    ]
    if (nrow(run) != 1L) {
        return(FALSE)
    }

    cases <- shift_inspect__rows(
        store,
        "shift_run_case",
        "run_id",
        wanted_run_id
    )
    required <- cases[cases[["required"]] %in% TRUE]
    if (
        !nrow(required) ||
            any(
                is.na(required[["status"]]) |
                    required[["status"]] != "completed"
            ) ||
            any(is.na(required[["output_id"]])) ||
            any(!nzchar(required[["output_id"]])) ||
            any(is.na(required[["export_path"]])) ||
            any(!file.exists(path.expand(required[["export_path"]])))
    ) {
        return(FALSE)
    }

    morph_id <- store__chr1(run[["morph_id"]][[1L]])
    if (is.na(morph_id) || !nzchar(morph_id)) {
        return(FALSE)
    }
    outputs <- shift_inspect__rows(store, "epw_output", "morph_id", morph_id)
    outputs <- outputs[outputs[["output_id"]] %in% required[["output_id"]]]
    if (
        nrow(outputs) != data.table::uniqueN(required[["output_id"]]) ||
            any(is.na(outputs[["path"]])) ||
            any(!nzchar(outputs[["path"]]))
    ) {
        return(FALSE)
    }
    paths <- vapply(
        outputs[["path"]],
        store_abs_path,
        character(1L),
        root = store$path
    )
    all(file.exists(paths))
}
# }}}

# Resolve an identical persisted task before registering another run. Complete
# runs are reusable only while their durable outputs exist; interrupted runs
# remain resumable under their original run ID and resolved input selection.
# shift_run__run_existing {{{
shift_run__run_existing <- function(plan) {
    store <- shift_store(plan, create = TRUE)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    spec <- shift_persist__plan_spec(plan)
    spec_hashes <- store__hash(shift_persist__spec_json(spec))
    if (identical(spec$control$refresh, FALSE)) {
        # Runs written before explicit catalog refresh was introduced encode
        # the same default behavior by omitting the field altogether.
        legacy_spec <- spec
        legacy_spec$control$refresh <- NULL
        spec_hashes <- unique(c(
            spec_hashes,
            store__hash(shift_persist__spec_json(legacy_spec))
        ))
    }
    runs <- morpher__private_store(store)$read_table("shift_run")
    runs <- runs[
        runs[["task"]] == "future_epw" &
            runs[["spec_hash"]] %in% spec_hashes
    ]
    if (!nrow(runs)) {
        return(NULL)
    }
    data.table::setorderv(runs, "updated_at", order = -1L, na.last = TRUE)

    complete <- runs[runs[["status"]] == "completed"]
    for (run_id in complete[["run_id"]]) {
        if (shift_run__run_artifacts_complete(store, run_id)) {
            return(shift_job__run_handle(store, run_id, plan = plan))
        }
    }
    resumable <- runs[!runs[["status"]] %in% "completed"]
    if (!nrow(resumable)) {
        return(NULL)
    }
    shift_job__run_handle(store, resumable[["run_id"]][[1L]], plan = plan)
}
# }}}

#' @rdname shift_api
#' @export
# shift_run {{{
shift_run <- function(x, background = FALSE, ui = shift_ui(), ...) {
    shift_stage__assert_stage(x)
    if (S7::S7_inherits(x, ShiftBatch)) {
        return(shift_batch__run(
            x,
            background = background,
            ui = ui
        ))
    }
    shift_run__run_one(x, background = background, ui = ui, ...)
}
# }}}

# Execute one plan, optionally owned by a shared batch coordinator.
# shift_run__run_one {{{
shift_run__run_one <- function(
    x,
    background = FALSE,
    ui = shift_ui(),
    execution = NULL,
    ...
) {
    if (!S7::S7_inherits(x, ShiftPlan)) {
        cli::cli_abort(
            "{.fn shift_run} expects a {.cls ShiftPlan} or {.cls ShiftBatch}."
        )
    }
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    if (isTRUE(background)) {
        shift_job__validate_background_plan(x)
    }
    control <- x@meta$control
    if (
        isTRUE(control@resume) &&
            !isTRUE(control@overwrite) &&
            !isTRUE(control@refresh)
    ) {
        existing <- shift_run__run_existing(x)
        if (!is.null(existing)) {
            status <- shift_status(existing, refresh = FALSE)
            if (status %in% c("completed", "queued", "running", "stopping")) {
                return(existing)
            }
            return(shift_job__resume_one(
                existing,
                background = background,
                ui = ui,
                execution = execution
            ))
        }
    }
    run_id <- shift_job__run_register(x)
    store <- shift_store(x, create = TRUE)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    shift_run__start_plan(x, store, run_id, background, ui, execution, ...)
}
# }}}

# Register and execute one plan attempt for both initial runs and recovery.
# Public dispatchers retain their return types; a batch supplies only its owner.
# shift_run__start_plan {{{
shift_run__start_plan <- function(
    plan,
    store,
    run_id,
    background,
    ui,
    execution = NULL,
    ...
) {
    job <- shift_job__job_create(
        store,
        run_id,
        mode = if (background) "process" else "foreground",
        ui = ui
    )
    context <- shift_execution__context(store$path, job, store, execution)
    reporter <- shift_reporter__reporter(
        ui,
        store = store,
        run_id = run_id,
        job_id = context$id,
        background = background,
        execution = context
    )
    on.exit(reporter$close(), add = TRUE)
    reporter$run_started(plan, run_id, background = background)
    shift_batch_execution__register_child(store, run_id, execution)
    if (background) {
        # Capture the handle before releasing DuckDB; reopening it after launch
        # would race the detached worker's exclusive process lock.
        handle <- shift_job__run_handle(store, run_id)
        store_path <- store$path
        store$close()
        shift_job__launch_job(
            store_path,
            run_id,
            context$id,
            job$log_path[[1L]]
        )
        return(handle)
    }
    shift_execution__run(
        context,
        shift_run__plan_run(
            plan,
            run_id = run_id,
            job_id = context$id,
            reporter = reporter,
            ...
        )
    )
}
# }}}

# Persist a Dataset result outside the relational File catalog. The JSON keeps
# the original EsgResultDataset contract intact while the lightweight stage
# provides stable run/step recovery coordinates.
# shift_run__datasets_stage {{{
shift_run__datasets_stage <- function(result, request, store) {
    if (!inherits(result, "EsgResultDataset")) {
        cli::cli_abort(
            "A Dataset task must return an {.cls EsgResultDataset} object."
        )
    }
    if (!S7::S7_inherits(request, ShiftRequest)) {
        cli::cli_abort(
            "A Dataset task must retain its originating {.cls ShiftRequest}."
        )
    }
    payload <- list(
        index_node = priv(result)$index_node,
        parameter = priv(result)$parameter$serialize(null = TRUE),
        records = result$to_data_table()
    )
    result_id <- store__hash(payload)
    path <- file.path(
        store$path,
        "queries",
        sprintf("datasets-%s.json", result_id)
    )
    result$save(path)
    artifact_id <- store$register_artifact(
        kind = "query",
        path = path,
        role = "input",
        project = "CMIP6",
        metadata = list(result_type = "Dataset")
    )
    shift_stage__new(
        ShiftDatasets,
        "datasets",
        store_path = store$path,
        ids = list(result_id = result_id, artifact_id = artifact_id),
        meta = list(
            request = request,
            dataset_count = result$count(),
            result_path = store_rel_path(path, root = store$path),
            datasets = result
        )
    )
}
# }}}

# Load the persisted Dataset result when the live R6 object is no longer
# available, for example after shift_result() reconstructs a previous run.
# shift_run__datasets_result {{{
shift_run__datasets_result <- function(x) {
    if (!S7::S7_inherits(x, ShiftDatasets)) {
        cli::cli_abort("`x` must be an internal Dataset catalog stage.")
    }
    live <- x@meta$datasets
    if (inherits(live, "EsgResultDataset")) {
        return(live)
    }
    path <- store_abs_path(x@meta$result_path, root = x@store_path)
    if (!file.exists(path)) {
        cli::cli_abort(c(
            "The persisted Dataset result is unavailable.",
            "x" = "Missing file: {.path {path}}"
        ))
    }
    esg_result("dataset")$load(path)
}
# }}}

# Carry run coordinates on the returned R6 result without changing its class or
# method surface. shift_run_get() uses these attributes as a convenience only;
# the store remains authoritative.
# shift_run__datasets_attach_run {{{
shift_run__datasets_attach_run <- function(result, stage) {
    attr(result, "epwshiftr.run_id") <- store__chr1(stage@ids$run_id)
    attr(result, "epwshiftr.step_id") <- store__chr1(stage@ids$step_id)
    attr(result, "epwshiftr.store") <- stage@store_path
    result
}
# }}}

#' @rdname shift_api
#' @export
# shift_datasets {{{
shift_datasets <- function(
    x,
    all = TRUE,
    limit = FALSE,
    store = NULL,
    ui = NULL
) {
    shift_stage__assert_stage(x)
    checkmate::assert_flag(all)

    if (S7::S7_inherits(x, ShiftRequest)) {
        reporter <- shift_run__current_reporter()
        if (is.null(reporter)) {
            stage <- shift_run__task_execute(
                "datasets",
                x,
                store = store,
                ui = ui,
                spec = list(all = all, limit = limit),
                auto_complete = TRUE,
                # code {{{
                code = function(reporter, task_store) {
                    result <- shift_run__with_reporter(
                        reporter,
                        shift_datasets(
                            x,
                            all = all,
                            limit = limit,
                            store = task_store
                        )
                    )
                    shift_run__datasets_stage(result, x, task_store)
                }
                # }}}
            )
            return(shift_run__datasets_attach_run(
                shift_run__datasets_result(stage),
                stage
            ))
        }

        query <- shift_resolve__as_query(x)
        node <- query$index_node()
        unit_total <- shift_run__catalog_unit_total()
        reporter$unit_started(
            "Querying Dataset catalog",
            current = 1L,
            total = unit_total,
            details = list(
                unit_type = "catalog",
                catalog_role = "Dataset",
                node = node
            )
        )
        result <- shift_resolve__with_query_reporter(
            reporter,
            query,
            "Dataset",
            query$collect(
                type = "Dataset",
                all = all,
                limit = limit,
                progress = FALSE
            )
        )
        reporter$unit_completed(
            sprintf("Indexed %d Dataset catalog records", result$count()),
            current = 1L,
            total = unit_total,
            details = list(
                unit_type = "catalog",
                catalog_role = "Dataset",
                node = node,
                records = result$count()
            )
        )
        return(result)
    }

    if (S7::S7_inherits(x, ShiftDatasets)) {
        return(shift_run__datasets_result(x))
    }

    files <- shift_stage__nested(x, list(ShiftFiles))
    if (!is.null(files) && !is.null(files@meta$datasets)) {
        return(files@meta$datasets)
    }

    request <- shift_stage__root(x)
    if (!is.null(request)) {
        return(shift_datasets(
            request,
            all = all,
            limit = limit,
            store = store,
            ui = ui
        ))
    }

    cli::cli_abort("No Dataset result is available for this shift stage.")
}
# }}}

# workflow methods
# S7::method(shift_collect, ShiftRequest) {{{
S7::method(shift_collect, ShiftRequest) <- function(
    x,
    store = NULL,
    fields = "*",
    all = TRUE,
    limit = FALSE,
    label = NULL,
    ui = NULL,
    ...
) {
    reporter <- shift_run__current_reporter()
    dots <- list(...)
    if ("progress" %in% names(dots)) {
        cli::cli_abort(c(
            "{.fn shift_collect} no longer accepts a logical `progress` argument.",
            "i" = "Use `ui = shift_ui(progress = ...)`; low-level {.cls EsgQuery} collection still accepts native progress controls."
        ))
    }
    checkmate::assert_character(
        fields,
        any.missing = FALSE,
        min.len = 1L,
        null.ok = TRUE
    )
    checkmate::assert_flag(all)
    checkmate::assert_string(label, null.ok = TRUE)
    if (is.null(store)) {
        cli::cli_abort("`store` is required for {.fn shift_collect}.")
    }
    store <- shift_store(store, create = TRUE)
    datasets <- shift_run__with_catalog_unit_total(
        2L,
        shift_datasets(x, all = all, limit = limit, store = store)
    )
    node <- priv(datasets)$index_node
    if (!is.null(reporter)) {
        reporter$unit_started(
            "Querying File catalog",
            current = 2L,
            total = 2L,
            details = list(
                unit_type = "catalog",
                catalog_role = "File",
                node = node
            )
        )
    }
    files <- shift_resolve__with_query_reporter(
        reporter,
        datasets,
        "File",
        do.call(
            datasets$collect,
            c(
                list(
                    type = "File",
                    fields = fields,
                    all = TRUE,
                    limit = NULL,
                    progress = FALSE
                ),
                dots
            )
        )
    )

    file_time <- shift_stage__coalesce(x@meta$options$file_time, x@meta$time)
    if (
        !is.null(file_time) &&
            !identical(x@meta$options$time_filter_method, "metadata")
    ) {
        time <- as.character(file_time)
        method <- shift_stage__coalesce(
            x@meta$options$time_filter_method,
            "drs"
        )
        if (length(time) == 1L) {
            files <- files$filter_time(time[[1L]], time[[1L]], method = method)
        } else {
            files <- files$filter_time(time[[1L]], time[[2L]], method = method)
        }
    }
    query_id <- store$add_files(files, label = label)
    file_dt <- files$to_data_table()
    variables <- if ("variable_id" %in% names(file_dt)) {
        unique(file_dt$variable_id)
    } else {
        character()
    }
    variables <- variables[!is.na(variables) & nzchar(variables)]

    if (!is.null(reporter)) {
        size <- if ("size" %in% names(file_dt)) {
            sum(suppressWarnings(as.numeric(file_dt$size)), na.rm = TRUE)
        } else {
            NA_real_
        }
        reporter$unit_completed(
            sprintf("Indexed %d File catalog records", files$count()),
            current = 2L,
            total = 2L,
            details = list(
                unit_type = "catalog",
                catalog_role = "File",
                node = node,
                records = files$count(),
                bytes_total = size
            )
        )
    }

    shift_stage__new(
        ShiftFiles,
        "files",
        store_path = store$path,
        ids = list(query_id = query_id),
        meta = list(
            request = x,
            dataset_count = datasets$count(),
            datasets = datasets,
            file_count = files$count(),
            variables = variables,
            fields = fields,
            # Keep the provider response field set with the stage so its
            # persisted receipt can reproduce EsgResultFile's established
            # summary without loading the complete saved result.
            result_fields = files$fields
        )
    )
}
# }}}

# Summarize downloader task state into workflow-specific byte and file metrics.
# shift_run__download_metrics {{{
shift_run__download_metrics <- function(
    downloader,
    session_id,
    variables = 0L
) {
    tasks <- tryCatch(
        downloader$tasks(session_id = session_id),
        # error {{{
        error = function(e) data.frame()
        # }}}
    )
    total <- nrow(tasks)
    completed <- if (total) sum(tasks$status %in% c("done", "skipped")) else 0L
    failed <- if (total) sum(tasks$status %in% c("error", "cancelled")) else 0L
    bytes_done <- if (total && "bytes_done" %in% names(tasks)) {
        sum(suppressWarnings(as.numeric(tasks$bytes_done)), na.rm = TRUE)
    } else {
        0
    }
    sizes <- if (total && "size" %in% names(tasks)) {
        suppressWarnings(as.numeric(tasks$size))
    } else {
        numeric()
    }
    bytes_total <- if (length(sizes) && all(is.finite(sizes) & sizes >= 0)) {
        sum(sizes)
    } else {
        NA_real_
    }
    active <- if (total) tasks$status %in% "downloading" else logical()
    speeds <- if (any(active) && "speed_bps" %in% names(tasks)) {
        suppressWarnings(as.numeric(tasks$speed_bps[active]))
    } else {
        numeric()
    }
    speed_bps <- if (length(speeds) && any(is.finite(speeds) & speeds > 0)) {
        sum(speeds[is.finite(speeds) & speeds > 0])
    } else {
        NA_real_
    }
    eta_seconds <- if (
        is.finite(bytes_total) && is.finite(speed_bps) && speed_bps > 0
    ) {
        max(0, bytes_total - bytes_done) / speed_bps
    } else {
        NA_real_
    }
    active_files <- if (any(active)) {
        column <- if ("filename" %in% names(tasks)) {
            tasks$filename
        } else if ("target_path" %in% names(tasks)) {
            basename(tasks$target_path)
        } else {
            rep(NA_character_, total)
        }
        values <- basename(as.character(column[active]))
        unique(values[!is.na(values) & nzchar(values)])
    } else {
        character()
    }
    list(
        current = completed,
        total = total,
        failed = failed,
        bytes_done = bytes_done,
        bytes_total = bytes_total,
        speed_bps = speed_bps,
        eta_seconds = eta_seconds,
        active_task_count = sum(active),
        active_files = active_files,
        variables = as.integer(variables)
    )
}
# }}}

# Format one task-specific download status shared by progress, completion, and
# persisted workflow events.
# shift_run__download_label {{{
shift_run__download_label <- function(role, metrics, active = NULL) {
    label <- sprintf(
        "%s download \u00b7 %d/%d files \u00b7 %s/%s \u00b7 %d variables",
        role,
        metrics$current,
        metrics$total,
        shift_ui_view__ui_bytes(metrics$bytes_done),
        shift_ui_view__ui_bytes(metrics$bytes_total),
        metrics$variables
    )
    if (is.finite(metrics$speed_bps) && metrics$speed_bps > 0) {
        label <- paste0(
            label,
            " \u00b7 ",
            shift_ui_view__ui_bytes(metrics$speed_bps),
            "/s"
        )
    }
    if (is.finite(metrics$eta_seconds)) {
        label <- paste0(
            label,
            " \u00b7 ETA ",
            shift_ui_view__format_elapsed(metrics$eta_seconds)
        )
    }
    if (
        !is.null(active) && length(active) && !is.na(active) && nzchar(active)
    ) {
        paste0(label, " \u00b7 ", basename(active))
    } else {
        label
    }
}
# }}}

# Bridge downloader callbacks into the workflow reporter. Progress callbacks
# are throttled by ShiftReporter while task/fallback milestones remain durable.
# shift_run__download_reporter_bind {{{
shift_run__download_reporter_bind <- function(
    downloader,
    reporter,
    role,
    variables = 0L,
    nested = FALSE
) {
    checkmate::assert_flag(nested)
    tokens <- character()
    # callback {{{
    callback <- function(event, dl) {
        metrics <- shift_run__download_metrics(
            dl,
            event$session_id,
            variables = variables
        )
        active <- if (length(metrics$active_files)) {
            paste(utils::head(metrics$active_files, 2L), collapse = " + ")
        } else {
            shift_stage__coalesce(event$filename, event$target_path)
        }
        label <- shift_run__download_label(
            role,
            metrics,
            active = if (shift_ui__ui_at_least(reporter$ui(), "detail")) {
                active
            } else {
                NULL
            }
        )
        details <- list(
            unit_type = "download_session",
            catalog_role = role,
            current = metrics$current,
            total = metrics$total,
            bytes_done = metrics$bytes_done,
            bytes_total = metrics$bytes_total,
            speed_bps = metrics$speed_bps,
            eta_seconds = metrics$eta_seconds,
            active_task_count = metrics$active_task_count,
            active_files = utils::head(metrics$active_files, 2L),
            variables = metrics$variables,
            data_node = event$data_node,
            access_method = "HTTPServer"
        )
        switch(
            event$event,
            session_start = if (isTRUE(nested)) {
                reporter$unit_updated(
                    label,
                    current = metrics$current,
                    total = metrics$total,
                    details = details
                )
            } else {
                reporter$unit_started(
                    label,
                    current = metrics$current,
                    total = metrics$total,
                    details = details
                )
            },
            task_start = reporter$unit_updated(
                label,
                current = metrics$current,
                total = metrics$total,
                details = details
            ),
            task_progress = reporter$heartbeat(label, details = details),
            candidate_error = reporter$notice(
                sprintf(
                    "%s download \u00b7 %s unavailable \u00b7 %s",
                    role,
                    shift_stage__coalesce(event$data_node, "candidate"),
                    shift_print__error_summary(event$error)
                ),
                outcome = "fallback",
                details = details
            ),
            task_done = reporter$unit_updated(
                label,
                current = metrics$current,
                total = metrics$total,
                details = details
            ),
            task_error = reporter$unit_updated(
                label,
                current = metrics$current,
                total = metrics$total,
                details = utils::modifyList(
                    details,
                    list(outcome = "failed", error = event$error)
                )
            ),
            task_cancelled = reporter$unit_updated(
                label,
                current = metrics$current,
                total = metrics$total,
                details = utils::modifyList(
                    details,
                    list(outcome = "cancelled", error = event$error)
                )
            ),
            session_done = if (isTRUE(nested)) {
                reporter$unit_updated(
                    label,
                    current = metrics$current,
                    total = metrics$total,
                    details = utils::modifyList(
                        details,
                        list(
                            outcome = if (metrics$failed) {
                                "failed"
                            } else {
                                "completed"
                            }
                        )
                    )
                )
            } else {
                reporter$unit_completed(
                    label,
                    current = metrics$current,
                    total = metrics$total,
                    outcome = if (metrics$failed) "failed" else "completed",
                    details = details
                )
            }
        )
        invisible(TRUE)
    }
    # }}}
    for (event in DOWNLOADER_CALLBACK_EVENTS) {
        tokens <- c(tokens, downloader$on(event, callback))
    }
    # { callback {{{
    function() {
        for (token in tokens) {
            try(downloader$off(token), silent = TRUE)
        }
        invisible(NULL)
    }
    # }}}
}
# }}}

# S7::method(shift_download, ShiftFiles) {{{
S7::method(shift_download, ShiftFiles) <- function(
    x,
    downloader = NULL,
    run = TRUE,
    background = FALSE,
    resume = TRUE,
    overwrite = FALSE,
    session_label = NULL,
    ui = NULL,
    ...
) {
    reporter <- shift_run__current_reporter()
    checkmate::assert_flag(run)
    checkmate::assert_flag(background)
    checkmate::assert_flag(resume)
    checkmate::assert_flag(overwrite)

    store <- shift_store(x)
    if (is.null(downloader) && (!isTRUE(run) || !is.null(reporter))) {
        downloader <- if (isTRUE(run)) {
            store$downloader()
        } else {
            store$downloader(n_workers = 0L)
        }
    }
    cleanup <- NULL
    if (!is.null(reporter)) {
        role <- shift_stage__coalesce(session_label, "CMIP6")
        cleanup <- shift_run__download_reporter_bind(
            downloader,
            reporter,
            role = role,
            variables = length(x@meta$variables)
        )
        on.exit(cleanup(), add = TRUE)
    }
    dots <- list(...)
    if (!is.null(reporter)) {
        # The workflow renderer owns progress; native downloader bars would
        # create a second, competing live region.
        dots$progress <- FALSE
    }
    session <- do.call(
        store$download_files,
        c(
            list(
                query_id = x@ids$query_id,
                downloader = downloader,
                run = run,
                background = background,
                resume = resume,
                overwrite = overwrite,
                session_label = session_label
            ),
            dots
        )
    )
    session_id <- if (is.character(session) && length(session) == 1L) {
        session
    } else if (is.data.frame(session) && "session_id" %in% names(session)) {
        session$session_id[[1L]]
    } else {
        NA_character_
    }
    diagnostics <- shift_check(
        shift_stage__new(
            ShiftDownload,
            "download",
            store_path = x@store_path,
            ids = utils::modifyList(x@ids, list(session_id = session_id)),
            meta = list(files = x, session = session)
        )
    )

    shift_stage__new(
        ShiftDownload,
        "download",
        store_path = x@store_path,
        ids = utils::modifyList(x@ids, list(session_id = session_id)),
        meta = list(files = x, session = session),
        diagnostics = diagnostics
    )
}
# }}}

# shift_run__extract_stage {{{
shift_run__extract_stage <- function(
    x,
    upstream_name,
    site = NULL,
    periods = NULL,
    variables = NULL,
    time = NULL,
    filters = list(),
    method = "nearest",
    fallback = c("auto", "error"),
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL
) {
    checkmate::assert_choice(upstream_name, c("files", "download"))
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("`site` must be created by {.fn shift_site}.")
    }
    checkmate::assert_data_frame(periods)
    checkmate::assert_list(filters, names = "unique")
    method <- match.arg(method, ESG_GRID_METHOD_CHOICES)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)
    fallback <- match.arg(fallback)

    store <- shift_store(x)
    ids <- shift_ids(x)
    variables <- shift_stage__coalesce(variables, shift_stage__variables(x))
    time <- shift_spec__time_window(shift_stage__coalesce(
        time,
        shift_spec__periods_time(periods)
    ))

    plan <- store$plan_region(
        query_id = ids$query_id,
        lon = site@lon,
        lat = site@lat,
        time = time,
        site_id = site@id,
        variable_id = variables,
        filters = filters,
        method = method
    )
    plan_id <- unique(plan$plan_id)
    processed <- store$extract(
        plan_id = plan_id,
        fallback = fallback,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
    coverage <- store$coverage(plan_id = plan_id)
    diagnostics <- shift_stage__diagnostics_from_coverage(coverage)
    upstream <- stats::setNames(list(x), upstream_name)

    shift_stage__new(
        ShiftClimate,
        "climate",
        store_path = x@store_path,
        ids = utils::modifyList(ids, list(plan_id = plan_id)),
        meta = c(
            upstream,
            list(
                site = site,
                periods = data.table::as.data.table(periods),
                variables = variables,
                plan = plan,
                processed = processed,
                coverage = coverage
            )
        ),
        diagnostics = diagnostics
    )
}
# }}}

# Run pre-existing extraction plan IDs through the same durable task boundary
# used by shift_extract(). This adapter lets the CLI retain its plan/run split
# without creating a second progress or persistence implementation.
# shift_run__extract_plans_task {{{
shift_run__extract_plans_task <- function(
    store,
    plan_id,
    fallback = c("auto", "error"),
    overwrite = FALSE,
    resume = TRUE,
    ui = NULL
) {
    store <- shift_store(store, create = FALSE)
    plan_id <- as.character(plan_id)
    checkmate::assert_character(
        plan_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    fallback <- match.arg(fallback)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)
    plans <- shift_inspect__extraction_plan(store, plan_id)
    if (!nrow(plans)) {
        cli::cli_abort("No extraction plan rows were found.")
    }
    identity <- unique(plans[, .(site_id, lon, lat, method)])
    if (nrow(identity) != 1L) {
        cli::cli_abort(
            "One extraction task cannot mix sites or extraction methods."
        )
    }
    query_id <- unique(plans$query_id)
    query_id <- query_id[!is.na(query_id) & nzchar(query_id)]
    if (!length(query_id)) {
        cli::cli_abort("Extraction plans do not contain a source query ID.")
    }
    time_start <- min(plans$time_start, na.rm = TRUE)
    time_stop <- max(plans$time_stop, na.rm = TRUE)
    years <- seq.int(
        as.integer(format(time_start, "%Y", tz = "UTC")),
        as.integer(format(time_stop, "%Y", tz = "UTC"))
    )
    periods <- epw_morph_periods(extract = years)
    site <- shift_site(
        identity$site_id[[1L]],
        identity$lon[[1L]],
        identity$lat[[1L]]
    )
    files <- shift_stage__new(
        ShiftFiles,
        "files",
        store_path = store$path,
        ids = list(query_id = query_id),
        meta = list(
            request = NULL,
            dataset_count = NA_integer_,
            file_count = nrow(shift_inspect__file_catalog(store, query_id)),
            variables = unique(plans$variable_id),
            fields = "*"
        )
    )
    spec <- list(
        site = shift_persist__site_ref(site),
        periods = split(as.integer(periods$year), periods$period),
        variables = unique(plans$variable_id),
        time = c(time_start, time_stop),
        filters = list(),
        method = identity$method[[1L]],
        fallback = fallback,
        overwrite = overwrite,
        resume = resume
    )
    shift_run__task_execute(
        "extract",
        files,
        store = store,
        ui = ui,
        spec = spec,
        # code {{{
        code = function(reporter, task_store) {
            processed <- task_store$extract(
                plan_id = plan_id,
                fallback = fallback,
                overwrite = overwrite,
                resume = resume,
                reporter = reporter
            )
            coverage <- task_store$coverage(plan_id = plan_id)
            shift_stage__new(
                ShiftClimate,
                "climate",
                store_path = task_store$path,
                ids = list(query_id = query_id, plan_id = plan_id),
                meta = list(
                    files = files,
                    site = site,
                    periods = periods,
                    variables = unique(plans$variable_id),
                    plan = plans,
                    processed = processed,
                    coverage = coverage
                ),
                diagnostics = shift_stage__diagnostics_from_coverage(coverage)
            )
        }
        # }}}
    )
}
# }}}

# S7::method(shift_extract, ShiftFiles) {{{
S7::method(shift_extract, ShiftFiles) <- function(
    x,
    site = NULL,
    periods = NULL,
    variables = NULL,
    time = NULL,
    filters = list(),
    method = "nearest",
    fallback = c("auto", "error"),
    overwrite = FALSE,
    resume = TRUE,
    ui = NULL
) {
    shift_run__extract_stage(
        x,
        upstream_name = "files",
        site = site,
        periods = periods,
        variables = variables,
        time = time,
        filters = filters,
        method = method,
        fallback = fallback,
        overwrite = overwrite,
        resume = resume,
        reporter = shift_run__current_reporter()
    )
}
# }}}

# S7::method(shift_extract, ShiftDownload) {{{
S7::method(shift_extract, ShiftDownload) <- function(
    x,
    site = NULL,
    periods = NULL,
    variables = NULL,
    time = NULL,
    filters = list(),
    method = "nearest",
    fallback = c("auto", "error"),
    overwrite = FALSE,
    resume = TRUE,
    ui = NULL
) {
    shift_run__extract_stage(
        x,
        upstream_name = "download",
        site = site,
        periods = periods,
        variables = variables,
        time = time,
        filters = filters,
        method = method,
        fallback = fallback,
        overwrite = overwrite,
        resume = resume,
        reporter = shift_run__current_reporter()
    )
}
# }}}

# Match one coverage table against the expected future cases and, when
# required, the corresponding explicit reference extraction.
# shift_run__case_fulfilment {{{
shift_run__case_fulfilment <- function(
    cases,
    future_coverage,
    reference_coverage,
    required_variables,
    requires_reference,
    requirements = NULL
) {
    cases <- data.table::as.data.table(data.table::copy(cases))
    future_coverage <- data.table::as.data.table(future_coverage)
    reference_coverage <- data.table::as.data.table(reference_coverage)
    coverage_columns <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "variable_id",
        "plan_id",
        "complete"
    )
    for (name in setdiff(coverage_columns, names(future_coverage))) {
        future_coverage[[name]] <- if (identical(name, "complete")) {
            logical(nrow(future_coverage))
        } else {
            character(nrow(future_coverage))
        }
    }
    for (name in setdiff(coverage_columns, names(reference_coverage))) {
        reference_coverage[[name]] <- if (identical(name, "complete")) {
            logical(nrow(reference_coverage))
        } else {
            character(nrow(reference_coverage))
        }
    }
    # match_identity {{{
    match_identity <- function(rows, case, include_experiment = TRUE) {
        keep <- shift_resolve__catalog_match(
            rows$source_id,
            case$source_id[[1L]]
        ) &
            shift_resolve__catalog_match(
                rows$variant_label,
                case$variant_label[[1L]]
            )
        # Grid is a per-table selection in enhanced workflows. Exact
        # partitions already restrict the climate stage, so case fulfilment is
        # intentionally keyed only by model/member (and future experiment).
        if (isTRUE(include_experiment)) {
            keep <- keep &
                shift_resolve__catalog_match(
                    rows$experiment_id,
                    case$experiment_id[[1L]]
                )
        }
        rows[keep]
    }
    # }}}

    for (i in seq_len(nrow(cases))) {
        case <- cases[i]
        missing <- character()
        future <- match_identity(
            future_coverage,
            case,
            include_experiment = TRUE
        )
        for (variable in required_variables) {
            alternatives <- if (is.null(requirements[[variable]])) {
                list(variable)
            } else {
                requirements[[variable]]
            }
            complete_variables <- unique(future[complete %in% TRUE]$variable_id)
            if (
                !length(morpher__requirement_match(
                    complete_variables,
                    alternatives
                ))
            ) {
                missing <- c(missing, sprintf("future/%s", variable))
            }
        }
        if (isTRUE(requires_reference)) {
            reference <- match_identity(
                reference_coverage,
                case,
                include_experiment = FALSE
            )
            for (variable in required_variables) {
                alternatives <- if (is.null(requirements[[variable]])) {
                    list(variable)
                } else {
                    requirements[[variable]]
                }
                complete_variables <- unique(
                    reference[complete %in% TRUE]$variable_id
                )
                if (
                    !length(morpher__requirement_match(
                        complete_variables,
                        alternatives
                    ))
                ) {
                    missing <- c(missing, sprintf("reference/%s", variable))
                }
            }
        }
        cases$status[[i]] <- if (length(missing)) "missing" else "ready"
        cases$missing_reason[[i]] <- if (length(missing)) {
            paste(missing, collapse = ", ")
        } else {
            NA_character_
        }
    }
    cases[]
}
# }}}

# Restrict a ShiftClimate stage to the complete plans that belong to ready user
# cases while retaining the original extraction evidence in metadata.
# shift_run__climate_for_cases {{{
shift_run__climate_for_cases <- function(climate, cases, reference = FALSE) {
    coverage <- shift_coverage(climate)
    ready <- cases[status == "ready"]
    keep <- logical(nrow(coverage))
    for (i in seq_len(nrow(ready))) {
        case <- ready[i]
        identity <- shift_resolve__catalog_match(
            coverage$source_id,
            case$source_id[[1L]]
        ) &
            shift_resolve__catalog_match(
                coverage$variant_label,
                case$variant_label[[1L]]
            )
        # Coverage may legitimately contain different grids for Amon and
        # LImon; the selected plan IDs, not one display grid, are authoritative.
        if (!isTRUE(reference)) {
            identity <- identity &
                shift_resolve__catalog_match(
                    coverage$experiment_id,
                    case$experiment_id[[1L]]
                )
        }
        keep <- keep | identity
    }
    keep <- keep & coverage$complete %in% TRUE
    selected_ids <- unique(coverage$plan_id[keep])
    if (!length(selected_ids)) {
        cli::cli_abort(
            "No complete extraction plans remain after applying the user case contract."
        )
    }
    climate@ids$plan_id <- selected_ids
    climate@meta$coverage <- coverage[plan_id %in% selected_ids]
    climate
}
# }}}

# Merge method-engine case failures into the user-facing run matrix by public
# CMIP identity. Successful morph cases remain ready until their EPW artifacts
# are written; failed siblings become terminal without hiding later successes.
# shift_run__apply_morph_case_status {{{
shift_run__apply_morph_case_status <- function(cases, morph_cases) {
    cases <- data.table::as.data.table(data.table::copy(cases))
    morph_cases <- data.table::as.data.table(morph_cases)
    if (!nrow(morph_cases)) {
        return(cases)
    }
    keys <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "period"
    )
    if (!all(keys %in% names(morph_cases))) {
        cli::cli_abort(
            "Morph case status is missing one or more public identity columns."
        )
    }
    for (i in seq_len(nrow(cases))) {
        if (!identical(cases$status[[i]], "ready")) {
            next
        }
        keep <- rep(TRUE, nrow(morph_cases))
        for (key in keys) {
            keep <- keep &
                shift_resolve__catalog_match(
                    morph_cases[[key]],
                    cases[[key]][[i]]
                )
        }
        hit <- morph_cases[keep]
        if (nrow(hit) != 1L) {
            next
        }
        if (identical(hit$status[[1L]], "failed")) {
            cases$status[[i]] <- "failed"
            cases$missing_reason[[i]] <- hit$last_error[[1L]]
        }
    }
    cases[]
}
# }}}

# Attach output IDs and exported paths to the expected case matrix using the
# public CMIP identity while allowing one case to own a complete year sequence.
# shift_run__complete_output_cases {{{
shift_run__complete_output_cases <- function(cases, outputs) {
    cases <- data.table::as.data.table(data.table::copy(cases))
    outputs <- data.table::as.data.table(outputs)
    for (i in seq_len(nrow(cases))) {
        if (!cases$status[[i]] %in% "ready") {
            next
        }
        hit <- outputs[
            source_id == cases$source_id[[i]] &
                experiment_id == cases$experiment_id[[i]] &
                variant_label == cases$variant_label[[i]] &
                period == cases$period[[i]]
        ]
        expected <- if (!nrow(hit) || !"member_count" %in% names(hit)) {
            1L
        } else {
            count <- unique(as.integer(hit$member_count))
            count <- count[!is.na(count)]
            if (length(count) == 1L) count else NA_integer_
        }
        member_keys <- if (nrow(hit)) {
            paste(
                hit$output_type,
                hit$sequence_id,
                hit$weather_year,
                sep = "\r"
            )
        } else {
            character()
        }
        if (
            nrow(hit) &&
                !is.na(expected) &&
                nrow(hit) == expected &&
                !anyDuplicated(member_keys)
        ) {
            cases$status[[i]] <- "completed"
            # The case table retains its original scalar compatibility field;
            # all member IDs remain authoritative in the output manifest.
            cases$output_id[[i]] <- hit$output_id[[1L]]
            if ("export_path" %in% names(hit)) {
                cases$export_path[[i]] <- hit$export_path[[1L]]
            }
        } else {
            cases$status[[i]] <- "missing"
            cases$missing_reason[[i]] <- if (!nrow(hit)) {
                "final EPW was not produced"
            } else {
                "the expected future-weather sequence is incomplete"
            }
        }
    }
    cases[]
}
# }}}

# Record a run stage transition before executing it so failures always point to
# the last durable workflow boundary.
# shift_run__run_transition {{{
shift_run__run_transition <- function(
    store,
    run_id,
    stage,
    message,
    reporter = NULL,
    current = NULL,
    total = NULL
) {
    shift_job__run_update(
        store,
        run_id,
        status = "running",
        current_stage = stage,
        last_error = NA_character_
    )
    if (!is.null(reporter)) {
        reporter$stage_started(stage, message, current = current, total = total)
    } else {
        shift_job__run_event(store, run_id, stage, "running", message)
    }
    invisible(stage)
}
# }}}

# Build a meaningful interrupt condition after a foreground Ctrl-C so callers
# retain interrupt semantics without rethrowing cli's message-less condition.
# shift_run__cancelled_interrupt {{{
shift_run__cancelled_interrupt <- function(message, run_id, store, stage) {
    structure(
        list(
            message = message,
            call = NULL,
            run_id = run_id,
            store = store,
            stage = stage
        ),
        class = c("epwshiftr_shift_cancelled", "interrupt", "condition")
    )
}
# }}}

# Execute a persisted ShiftPlan through the existing stage primitives while
# enforcing task-level selection, coverage, and completion contracts.
# shift_run__plan_run {{{
shift_run__plan_run <- function(
    x,
    run_id,
    job_id = NULL,
    reporter = NULL,
    resume_existing = FALSE,
    ...
) {
    meta <- x@meta
    control <- meta$control
    store <- shift_store(x, create = TRUE)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    overwrite <- isTRUE(control@overwrite)
    resume <- isTRUE(control@resume) || isTRUE(resume_existing)
    current_stage <- "planned"
    if (is.null(reporter)) {
        reporter <- shift_reporter__reporter(
            shift_ui("none"),
            store = store,
            run_id = run_id,
            job_id = job_id
        )
    }
    reference_expected <- S7::S7_inherits(meta$reference, ShiftReferenceSpec) &&
        identical(meta$reference@mode, "historical")
    stage_total <- 5L +
        as.integer(identical(control@download, "always")) +
        as.integer(reference_expected)
    stage_index <- 0L
    # next_stage {{{
    next_stage <- function(stage, message) {
        stage_index <<- stage_index + 1L
        reporter$check_cancel(stage)
        shift_run__run_transition(
            store,
            run_id,
            stage,
            message,
            reporter = reporter,
            current = stage_index,
            total = stage_total
        )
    }
    # }}}
    # Reopen the elapsed-time clock for a resumed attempt while preserving the
    # original run start and all prior immutable scientific selections.
    shift_job__run_update(
        store,
        run_id,
        status = "running",
        completed_at = as.POSIXct(NA, tz = "UTC"),
        last_error = NA_character_
    )

    # Both terminal paths persist one status and event. Interrupts retain their
    # condition class; ordinary errors retain the original source condition.
    # failed {{{
    failed <- function(e) {
        interrupted <- inherits(e, "interrupt")
        requested <- interrupted &&
            !is.null(job_id) &&
            tryCatch(
                shift_job__job_cancel_requested(store, job_id),
                # error {{{
                error = function(err) FALSE
                # }}}
            )
        message <- if (requested) {
            "Cancellation requested by user."
        } else {
            conditionMessage(e)
        }
        if (
            interrupted &&
                (is.null(message) ||
                    !length(message) ||
                    is.na(message) ||
                    !nzchar(message))
        ) {
            message <- "Interrupted by user."
        }
        cancelled <- interrupted || inherits(e, "epwshiftr_shift_cancelled")
        final_status <- if (isTRUE(cancelled)) "cancelled" else "failed"
        resolution <- if (inherits(e, "epwshiftr_shift_resolution_error")) {
            e$resolution
        } else {
            NULL
        }
        failure_summary <- if (is.null(resolution)) {
            shift_print__error_summary(message)
        } else {
            as.character(resolution$summary)[[1L]]
        }
        failure_details <- utils::modifyList(
            reporter$context(),
            c(
                list(
                    outcome = final_status,
                    error_summary = failure_summary
                ),
                shift_stage__coalesce(resolution, list())
            )
        )
        try(
            shift_job__run_finish(
                store,
                run_id,
                status = final_status,
                current_stage = current_stage,
                last_error = message
            ),
            silent = TRUE
        )
        try(
            shift_job__run_event(
                store,
                run_id,
                current_stage,
                final_status,
                message,
                details = failure_details
            ),
            silent = TRUE
        )
        if (interrupted) {
            reporter$run_failed(
                paste(
                    sprintf(
                        "Future EPW run %s cancelled during %s.",
                        run_id,
                        current_stage
                    ),
                    shift_print__failure_context(
                        failure_details,
                        debug = shift_ui__ui_at_least(reporter$ui(), "debug")
                    )
                ),
                cancelled = TRUE
            )
            stop(shift_run__cancelled_interrupt(
                message,
                run_id,
                store$path,
                current_stage
            ))
        }
        reporter$run_failed(
            message = failure_summary,
            cancelled = cancelled,
            details = failure_details
        )
        if (isTRUE(cancelled)) {
            stop(e)
        }
        shift_print__abort_run(
            e,
            run_id,
            store$path,
            current_stage,
            failure_details,
            debug = shift_ui__ui_at_least(reporter$ui(), "debug")
        )
    }
    # }}}

    result <- tryCatch(
        {
            current_stage <- next_stage(
                "resolve",
                "Resolving complete CMIP6 workflow inputs."
            )
            resolved_inputs <- shift_resolve__collect_resolved_inputs(
                x,
                run_id,
                reporter = reporter,
                job_id = job_id
            )
            selection <- data.table::as.data.table(resolved_inputs$selection)
            selected_partitions <- shift_print__format_cmip6_partitions(
                selection
            )
            cases <- shift_resolve__resolved_expected_cases(x, selection)
            resolved <- list(
                index_node = resolved_inputs$index_node,
                selection = as.data.frame(selection),
                member = unique(selection$variant_label),
                grid = unique(selection$grid_label),
                partitions = selected_partitions
            )
            x@meta$resolved <- resolved
            future_query_id <- resolved_inputs$files@ids$query_id
            reference_query_id <- if (
                is.null(resolved_inputs$reference_files)
            ) {
                NA_character_
            } else {
                resolved_inputs$reference_files@ids$query_id
            }
            shift_job__run_update(
                store,
                run_id,
                resolved_spec_json = shift_persist__spec_json(resolved),
                query_id = future_query_id,
                reference_query_id = reference_query_id
            )
            shift_job__run_cases_write(store, run_id, cases)
            reporter$cases_updated(cases)
            resolved_node_label <- shift_resolve__report_node(
                reporter,
                resolved_inputs$index_node
            )
            reporter$stage_completed(
                sprintf(
                    "Resolved %s with member %s and partitions %s.",
                    resolved_node_label,
                    paste(unique(selection$variant_label), collapse = ", "),
                    selected_partitions
                ),
                details = list(
                    node = resolved_inputs$index_node,
                    future_files = as.integer(
                        resolved_inputs$files@meta$file_count
                    ),
                    reference_files = if (
                        is.null(resolved_inputs$reference_files)
                    ) {
                        0L
                    } else {
                        as.integer(
                            resolved_inputs$reference_files@meta$file_count
                        )
                    },
                    member = unique(selection$variant_label),
                    grid = unique(selection$grid_label),
                    partitions = selected_partitions
                )
            )

            future_stage <- resolved_inputs$files
            reference_stage <- resolved_inputs$reference_files
            if (identical(control@download, "always")) {
                current_stage <- next_stage(
                    "download",
                    "Downloading selected CMIP6 source files."
                )
                future_stage <- shift_resolve__files_for_partitions(
                    future_stage,
                    selection,
                    experiments = if (is.null(meta$climate)) {
                        meta$request@meta$experiment
                    } else {
                        meta$climate@scenarios
                    },
                    years = unique(as.integer(meta$periods$year)),
                    role = "future"
                )
                if (!is.null(reference_stage)) {
                    reference_stage <- shift_resolve__files_for_partitions(
                        reference_stage,
                        selection,
                        experiments = meta$reference@experiment,
                        years = unique(as.integer(meta$reference@periods$year)),
                        role = "reference"
                    )
                }
                download_args <- utils::modifyList(
                    list(
                        run = TRUE,
                        background = FALSE,
                        resume = resume,
                        overwrite = overwrite,
                        # The workflow reporter owns presentation. Native downloader
                        # bars remain disabled while callbacks publish byte/file
                        # metrics into the shared fixed status region.
                        progress = FALSE
                    ),
                    meta$download
                )
                future_stage <- shift_run__do_call_with_reporter(
                    reporter,
                    shift_download,
                    c(
                        list(future_stage),
                        utils::modifyList(
                            download_args,
                            list(session_label = "future")
                        )
                    )
                )
                if (!is.null(reference_stage)) {
                    reference_stage <- shift_run__do_call_with_reporter(
                        reporter,
                        shift_download,
                        c(
                            list(reference_stage),
                            utils::modifyList(
                                download_args,
                                list(session_label = "reference")
                            )
                        )
                    )
                }
                reporter$stage_completed(
                    "Downloaded selected CMIP6 source files."
                )
            }

            fallback <- if (identical(control@download, "never")) {
                "error"
            } else {
                "auto"
            }
            roles <- list(
                future = list(
                    stage = future_stage,
                    experiments = if (is.null(meta$climate)) {
                        meta$request@meta$experiment
                    } else {
                        meta$climate@scenarios
                    },
                    periods = meta$periods,
                    overrides = meta$extract,
                    message = "Extracting future climate data."
                )
            )
            if (!is.null(reference_stage)) {
                roles$reference <- list(
                    stage = reference_stage,
                    experiments = meta$reference@experiment,
                    periods = meta$reference@periods,
                    overrides = meta$reference@extract,
                    message = "Extracting historical reference climate data."
                )
            }
            climates <- coverages <- stats::setNames(
                vector("list", length(roles)),
                names(roles)
            )
            # Future and reference are two ordered I/O stages. Share their
            # execution contract while preserving role-specific selection and overrides.
            for (role in names(roles)) {
                input <- roles[[role]]
                current_stage <- next_stage(
                    paste0("extract_", role),
                    input$message
                )
                extracted <- shift_run__extract_selected_partitions(
                    input$stage,
                    selection = selection,
                    experiments = input$experiments,
                    site = meta$site,
                    periods = input$periods,
                    role = role,
                    time = shift_spec__method_time_window(
                        input$periods,
                        meta$recipe
                    ),
                    method = control@extraction_method,
                    fallback = fallback,
                    overwrite = overwrite,
                    resume = resume,
                    overrides = input$overrides,
                    reporter = reporter
                )
                extracted <- shift_climate__derive_hurs_climate(
                    extracted,
                    meta$recipe,
                    overwrite = overwrite,
                    resume = resume,
                    reporter = reporter
                )
                coverage <- shift_coverage(extracted)
                climates[[role]] <- extracted
                coverages[[role]] <- coverage
                completed <- sum(coverage$complete %in% TRUE)
                reporter$stage_completed(
                    sprintf(
                        "Extracted %s climate: %d/%d plan(s) complete.",
                        role,
                        completed,
                        nrow(coverage)
                    ),
                    details = list(
                        plans_completed = completed,
                        plans_total = nrow(coverage),
                        variables = length(unique(coverage$variable_id))
                    )
                )
            }
            climate <- climates$future
            reference_climate <- climates$reference
            method_reference <- shift_stage__coalesce(
                reference_climate,
                meta$reference
            )

            current_stage <- next_stage(
                "coverage",
                "Checking requested case and reference coverage."
            )
            reference_coverage <- if (!is.null(reference_climate)) {
                coverages$reference
            } else if (S7::S7_inherits(method_reference, ShiftClimate)) {
                shift_coverage(method_reference)
            } else if (
                S7::S7_inherits(method_reference, ShiftReferenceSpec) &&
                    identical(method_reference@mode, "plan")
            ) {
                store$coverage(plan_id = method_reference@plan_id)
            } else {
                data.table::data.table()
            }
            if (
                S7::S7_inherits(method_reference, ShiftClimate) &&
                    is.null(reference_stage)
            ) {
                # Manual ShiftClimate references receive the same canonical
                # derivation contract as automatically extracted historical data.
                method_reference <- shift_climate__derive_hurs_climate(
                    method_reference,
                    meta$recipe,
                    overwrite = overwrite,
                    resume = resume,
                    reporter = reporter
                )
                reference_coverage <- shift_coverage(method_reference)
            }
            cases <- shift_run__case_fulfilment(
                cases,
                future_coverage = coverages$future,
                reference_coverage = reference_coverage,
                required_variables = epw_morph_variables(meta$recipe),
                requires_reference = !is.null(method_reference),
                requirements = morpher__variable_requirements(meta$recipe)
            )
            shift_job__run_cases_write(store, run_id, cases)
            ready <- cases[status == "ready"]
            missing <- cases[status == "missing"]
            if (nrow(missing)) {
                for (i in seq_len(nrow(missing))) {
                    reporter$notice(
                        sprintf(
                            "Missing %s/%s: %s",
                            missing$experiment_id[[i]],
                            missing$period[[i]],
                            missing$missing_reason[[i]]
                        ),
                        outcome = if (isTRUE(control@allow_partial)) {
                            "skipped"
                        } else {
                            "failed"
                        },
                        details = list(
                            unit_type = "future_epw_case",
                            scenario = missing$experiment_id[[i]],
                            period = missing$period[[i]],
                            outcome = if (isTRUE(control@allow_partial)) {
                                "skipped"
                            } else {
                                "failed"
                            }
                        )
                    )
                }
            }
            if (!nrow(ready)) {
                cli::cli_abort(
                    "Zero requested future EPW cases have complete required climate inputs."
                )
            }
            if (nrow(missing) && !isTRUE(control@allow_partial)) {
                cli::cli_abort(c(
                    "Not all requested future EPW cases are complete.",
                    "x" = sprintf(
                        "%s/%s/%s: %s",
                        missing$source_id,
                        missing$experiment_id,
                        missing$period,
                        missing$missing_reason
                    ),
                    "i" = "Set `allow_partial = TRUE` in shift_control() to process only complete cases."
                ))
            }
            reporter$stage_completed(
                sprintf(
                    "Coverage ready for %d/%d requested case(s).",
                    nrow(ready),
                    nrow(cases)
                ),
                details = list(ready = nrow(ready), missing = nrow(missing))
            )
            reporter$cases_updated(cases, show = TRUE)
            climate <- shift_run__climate_for_cases(
                climate,
                cases,
                reference = FALSE
            )
            if (S7::S7_inherits(method_reference, ShiftClimate)) {
                method_reference <- shift_run__climate_for_cases(
                    method_reference,
                    cases,
                    reference = TRUE
                )
            }
            shift_job__run_update(
                store,
                run_id,
                plan_ids_json = shift_persist__spec_json(climate@ids$plan_id),
                reference_plan_ids_json = if (
                    S7::S7_inherits(method_reference, ShiftClimate)
                ) {
                    shift_persist__spec_json(method_reference@ids$plan_id)
                } else {
                    NA_character_
                }
            )

            current_stage <- next_stage(
                "morph",
                "Morphing all complete requested cases."
            )
            morph_args <- utils::modifyList(
                list(
                    baseline = meta$site,
                    transform = meta$transform,
                    reference = method_reference,
                    observed_reference = meta$observed_reference,
                    strict = control@strict,
                    complete_only = TRUE,
                    by = c(
                        "source_id",
                        "experiment_id",
                        "variant_label",
                        "period"
                    ),
                    overwrite = overwrite,
                    resume = resume
                ),
                meta$morph
            )
            morphed <- shift_run__do_call_with_reporter(
                reporter,
                shift_morph,
                c(list(climate), morph_args)
            )
            morph_id <- morphed@ids$morph_id
            shift_job__run_update(store, run_id, morph_id = morph_id)
            # Scientific warnings belong to the completed run receipt, not only to
            # the transient ShiftMorphed object returned inside this process.
            shift_job__run_diagnostics_record(
                store,
                run_id,
                morphed@diagnostics
            )
            cases <- shift_run__apply_morph_case_status(
                cases,
                morphed@meta$cases
            )
            shift_job__run_cases_write(store, run_id, cases)
            reporter$cases_updated(
                cases,
                show = shift_ui__ui_at_least(reporter$ui(), "detail")
            )
            morphed_count <- sum(cases$status == "ready")
            failed_morph_count <- sum(cases$status == "failed")
            reporter$stage_completed(
                sprintf(
                    "Morphed %d requested case(s); %d failed independently.",
                    morphed_count,
                    failed_morph_count
                ),
                details = list(
                    completed = morphed_count,
                    failed = failed_morph_count
                )
            )

            current_stage <- next_stage(
                "write_epw",
                "Writing and exporting final EPW files."
            )
            epw_args <- utils::modifyList(
                list(
                    dir = "outputs/future-epw",
                    separate = identical(control@output_layout, "nested"),
                    export_dir = NULL,
                    overwrite = overwrite,
                    resume = resume
                ),
                meta$epw
            )
            outputs_stage <- shift_run__do_call_with_reporter(
                reporter,
                shift_epw,
                c(list(morphed), epw_args)
            )
            output_rows <- shift_outputs(outputs_stage)
            cases <- shift_run__complete_output_cases(cases, output_rows)
            shift_job__run_cases_write(store, run_id, cases)
            reporter$cases_updated(
                cases,
                show = shift_ui__ui_at_least(reporter$ui(), "detail")
            )
            output_count <- nrow(output_rows)
            if (!output_count) {
                cli::cli_abort("The workflow produced zero final EPW files.")
            }
            reporter$stage_completed(sprintf(
                "Wrote and exported %d EPW file(s).",
                output_count
            ))
            final_status <- if (
                all(cases[required %in% TRUE]$status == "completed")
            ) {
                "completed"
            } else {
                "partial"
            }
            shift_job__run_finish(
                store,
                run_id,
                status = final_status,
                current_stage = "completed",
                last_error = NA_character_
            )
            shift_job__run_event(
                store,
                run_id,
                "completed",
                final_status,
                sprintf("Produced %d final EPW file(s).", output_count)
            )
            run <- shift_job__run_handle(
                store,
                run_id,
                output_stage = outputs_stage,
                plan = x
            )
            reporter$run_completed(run, shift_outputs(run, refresh = FALSE))
            run
        },
        interrupt = failed,
        error = failed
    )
    result
}
# }}}

# S7::method(shift_morph, ShiftClimate) {{{
S7::method(shift_morph, ShiftClimate) <- function(
    x,
    baseline = NULL,
    transform,
    reference = NULL,
    observed_reference = NULL,
    strict = TRUE,
    complete_only = TRUE,
    by = c("source_id", "experiment_id", "variant_label", "period"),
    overwrite = FALSE,
    resume = TRUE,
    ui = NULL
) {
    reporter <- shift_run__current_reporter()
    transform__validate_execution_inputs(
        transform,
        reference,
        observed_reference
    )
    recipe <- transform__recipe(transform)
    checkmate::assert_flag(strict)
    checkmate::assert_flag(complete_only)
    checkmate::assert_character(
        by,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)

    store <- shift_store(x)
    ids <- shift_ids(x)
    site <- shift_target(x)
    baseline <- shift_stage__coalesce(baseline, site)
    epw <- shift_spec__resolve_epw(baseline)
    periods <- x@meta$periods
    reference_resolved <- shift_reference__resolve(
        x = x,
        recipe = recipe,
        site = site,
        reference = reference,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
    observed_resolved <- shift_reference__observed_reference_resolve(
        x = x,
        recipe = recipe,
        site = site,
        observed_reference = observed_reference,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )

    plan_selection <- shift_resolve__morph_complete_plan_selection(
        store,
        ids$plan_id,
        complete_only = complete_only,
        stage = "morph"
    )
    reference_selection <- shift_resolve__morph_complete_plan_selection(
        store,
        reference_resolved$plan_id,
        complete_only = complete_only,
        stage = "reference"
    )
    observed_selection <- shift_resolve__morph_complete_plan_selection(
        store,
        observed_resolved$plan_id,
        complete_only = complete_only,
        stage = "observed reference"
    )
    morpher <- epw_morpher(
        store,
        epw,
        site_id = site@id,
        transform = transform,
        label = site@label
    )
    workflow <- morpher$workflow(
        plan_id = plan_selection$plan_id,
        periods = periods,
        reference_plan_id = reference_selection$plan_id,
        reference_periods = reference_resolved$periods,
        observed_plan_id = observed_selection$plan_id,
        observed_periods = observed_resolved$periods,
        by = by,
        strict = strict,
        dir = NULL,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
    summary_id <- unique(workflow$climate$summary_id)[[1L]]
    baseline_id <- unique(workflow$baseline$baseline_id)[[1L]]
    morph_id <- unique(workflow$plan$morph_id)[[1L]]
    diagnostics <- shift_stage__bind_diagnostics(
        plan_selection$diagnostics,
        reference_selection$diagnostics,
        observed_selection$diagnostics,
        shift_stage__diagnostics_normalize(workflow$diagnostics)
    )

    shift_stage__new(
        ShiftMorphed,
        "morphed",
        store_path = x@store_path,
        ids = utils::modifyList(
            ids,
            list(
                plan_id = plan_selection$plan_id,
                summary_id = summary_id,
                baseline_id = baseline_id,
                morph_id = morph_id
            )
        ),
        meta = list(
            climate = x,
            baseline = baseline,
            reference = reference_resolved$reference,
            reference_spec = reference_resolved$spec,
            reference_plan_id = reference_selection$plan_id,
            observed_reference = observed_resolved$reference,
            observed_reference_spec = observed_resolved$spec,
            observed_plan_id = observed_selection$plan_id,
            original_plan_id = ids$plan_id,
            original_reference_plan_id = reference_resolved$plan_id,
            original_observed_plan_id = observed_resolved$plan_id,
            complete_only = complete_only,
            reference_periods = reference_resolved$periods,
            observed_periods = observed_resolved$periods,
            transform = transform,
            recipe = recipe,
            workflow = workflow,
            preflight = workflow$preflight,
            climate_summary = workflow$climate,
            baseline_summary = workflow$baseline,
            preview = workflow$preview,
            plan = workflow$plan,
            cases = workflow$cases,
            results = workflow$results
        ),
        diagnostics = diagnostics
    )
}
# }}}

# S7::method(shift_epw, ShiftMorphed) {{{
S7::method(shift_epw, ShiftMorphed) <- function(
    x,
    dir = NULL,
    separate = TRUE,
    export_dir = NULL,
    overwrite = FALSE,
    resume = TRUE,
    ui = NULL
) {
    reporter <- shift_run__current_reporter()
    dir <- shift_stage__coalesce(dir, "outputs/future-epw")
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(separate)
    checkmate::assert_string(export_dir, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)

    store <- shift_store(x)
    ids <- shift_ids(x)
    site <- shift_target(x)
    epw <- shift_spec__resolve_epw(shift_stage__coalesce(x@meta$baseline, site))
    morpher <- epw_morpher(
        store,
        epw,
        site_id = site@id,
        transform = x@meta$transform,
        label = site@label
    )
    outputs <- morpher$write_epw(
        morph_id = ids$morph_id,
        dir = dir,
        separate = separate,
        overwrite = overwrite,
        resume = resume,
        reporter = reporter
    )
    path_col <- intersect(
        c("path", "output_path", "relative_path"),
        names(outputs)
    )
    paths <- if (length(path_col)) outputs[[path_col[[1L]]]] else character()

    stage <- shift_stage__new(
        ShiftOutputs,
        "outputs",
        store_path = x@store_path,
        ids = ids,
        meta = list(
            morphed = x,
            format = "epw",
            outputs = outputs,
            paths = paths,
            export_dir = export_dir
        ),
        diagnostics = shift_stage__diagnostics_empty()
    )
    if (!is.null(export_dir)) {
        stage <- shift_export_epw(
            stage,
            dir = export_dir,
            separate = separate,
            overwrite = overwrite,
            resume = resume
        )
    }
    stage
}
# }}}

# Combine independently planned extraction partitions into one climate stage.
# Coverage is re-read from the store for the union of plan IDs so resume and
# diagnostics use the same durable view as an ordinary shift_extract() call.
# shift_run__combine_climate_stages {{{
shift_run__combine_climate_stages <- function(stages) {
    stages <- Filter(
        # Filter callback {{{
        function(stage) S7::S7_inherits(stage, ShiftClimate),
        # }}}
        stages
    )
    if (!length(stages)) {
        cli::cli_abort(
            "No CMIP6 extraction partition produced a climate stage."
        )
    }
    if (length(stages) == 1L) {
        return(stages[[1L]])
    }
    first <- stages[[1L]]
    plan_id <- unique(unlist(
        # lapply callback {{{
        lapply(stages, function(stage) stage@ids$plan_id),
        # }}}
        use.names = FALSE
    ))
    query_id <- unique(unlist(
        # lapply callback {{{
        lapply(stages, function(stage) stage@ids$query_id),
        # }}}
        use.names = FALSE
    ))
    store <- shift_store(first)
    coverage <- store$coverage(plan_id = plan_id)
    # bind_meta {{{
    bind_meta <- function(name) {
        # lapply callback {{{
        values <- lapply(stages, function(stage) stage@meta[[name]])
        # }}}
        values <- Filter(is.data.frame, values)
        if (!length(values)) {
            NULL
        } else {
            data.table::rbindlist(values, use.names = TRUE, fill = TRUE)
        }
    }
    # }}}
    upstream_name <- if (S7::S7_inherits(first@meta$download, ShiftDownload)) {
        "download"
    } else {
        "files"
    }
    upstream <- first@meta[[upstream_name]]
    shift_stage__new(
        ShiftClimate,
        "climate",
        store_path = first@store_path,
        ids = list(query_id = query_id, plan_id = plan_id),
        meta = c(
            stats::setNames(list(upstream), upstream_name),
            list(
                site = first@meta$site,
                periods = first@meta$periods,
                variables = unique(unlist(
                    # lapply callback {{{
                    lapply(stages, function(stage) stage@meta$variables),
                    # }}}
                    use.names = FALSE
                )),
                plan = bind_meta("plan"),
                processed = bind_meta("processed"),
                coverage = coverage
            )
        ),
        diagnostics = shift_stage__diagnostics_from_coverage(coverage)
    )
}
# }}}

# Extract each exact source/member/table/grid partition separately and merge the
# resulting plan IDs only after planning. Selection facets are re-applied after
# user extraction overrides so workflow intent cannot be widened accidentally.
# shift_run__extract_selected_partitions {{{
shift_run__extract_selected_partitions <- function(
    stage,
    selection,
    experiments,
    site,
    periods,
    role = c("future", "reference"),
    time = NULL,
    method = "nearest",
    fallback = "auto",
    overwrite = FALSE,
    resume = TRUE,
    overrides = list(),
    reporter = NULL
) {
    role <- match.arg(role)
    partitions <- shift_resolve__selection_partition_rows(selection, role)
    groups <- unique(partitions[, .(
        source_id,
        variant_label,
        frequency,
        table_id,
        grid_label
    )])
    stages <- vector("list", nrow(groups))
    custom_filters <- shift_stage__coalesce(overrides$filters, list())
    overrides$filters <- NULL
    for (i in seq_len(nrow(groups))) {
        group <- groups[i]
        variables <- unique(partitions[
            shift_resolve__catalog_match(source_id, group$source_id[[1L]]) &
                shift_resolve__catalog_match(
                    variant_label,
                    group$variant_label[[1L]]
                ) &
                shift_resolve__catalog_match(frequency, group$frequency[[1L]]) &
                shift_resolve__catalog_match(table_id, group$table_id[[1L]]) &
                shift_resolve__catalog_match(
                    grid_label,
                    group$grid_label[[1L]]
                ),
            variable_id
        ])
        exact_filters <- list(
            source_id = group$source_id[[1L]],
            experiment_id = experiments,
            variant_label = group$variant_label[[1L]],
            grid_label = group$grid_label[[1L]],
            frequency = group$frequency[[1L]],
            table_id = group$table_id[[1L]]
        )
        args <- utils::modifyList(
            list(
                site = site,
                periods = periods,
                variables = variables,
                time = time,
                filters = utils::modifyList(custom_filters, exact_filters),
                method = method,
                fallback = fallback,
                overwrite = overwrite,
                resume = resume
            ),
            overrides
        )
        # Re-pin scientific selection fields after generic overrides.
        args$site <- site
        args$periods <- periods
        args$variables <- variables
        args$filters <- utils::modifyList(custom_filters, exact_filters)
        args$overwrite <- overwrite
        args$resume <- resume
        if (identical(fallback, "error")) {
            args$fallback <- "error"
        }
        stages[[i]] <- shift_run__do_call_with_reporter(
            reporter,
            shift_extract,
            c(list(stage), args)
        )
    }
    shift_run__combine_climate_stages(stages)
}
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
