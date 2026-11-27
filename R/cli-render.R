#' @include shift-ui-state.R shift-ui-render.R
NULL

# Context & top-level CLI rendering dispatch {{{

# epwshiftr_cli_context {{{
epwshiftr_cli_context <- function(parsed) {
    args <- parsed$args
    if (
        isTRUE(parsed$help) || (length(args) && identical(args[[1L]], "help"))
    ) {
        topic <- if (isTRUE(parsed$help)) args else args[-1L]
        return(list(
            group = "help",
            command = paste(topic, collapse = " "),
            action = NULL
        ))
    }
    if (!length(args)) {
        return(list(group = NULL, command = NULL, action = NULL))
    }
    columns <- if (
        length(args) >= 2L &&
            identical(args[[1L]], "query") &&
            identical(args[[2L]], "search")
    ) {
        epwshiftr_cli_csv(
            epwshiftr_cli_parse_command(
                args[-seq_len(2L)],
                flags = c("--all", "--dry-run", "--no-progress"),
                options = cli_render__cols(
                    "--index-node --type --fields --columns --limit"
                )
            )$options[["--columns"]]
        )
    }
    list(
        group = args[[1L]],
        command = if (length(args) >= 2L) args[[2L]] else NULL,
        action = if (length(args) >= 3L) args[[3L]] else NULL,
        columns = columns
    )
}
# }}}

# epwshiftr_cli_render {{{
epwshiftr_cli_render <- function(result, context = NULL) {
    context <- shift_stage__coalesce(context, list())
    if (is.character(result)) {
        for (line in result) {
            cli::cli_text("{line}")
        }
        return(invisible(NULL))
    }
    group <- shift_stage__coalesce(context$group, "")
    command <- context$command
    action <- context$action
    switch(
        group,
        doctor = cli_render__dispatch_section(result, "doctor"),
        query = epwshiftr_cli_render_query(result, command, context = context),
        download = epwshiftr_cli_render_download(result, command, action),
        esgf = if (identical(command, "report")) {
            cli_render__dispatch_section(result, "esgf:report")
        } else {
            epwshiftr_cli_render_default(result)
        },
        storage = epwshiftr_cli_render_storage(result, command, action),
        shift = epwshiftr_cli_render_shift(result, command),
        extract = if (!cli_render__dispatch_table(result, "extract", command)) {
            epwshiftr_cli_render_table(
                result,
                title = cli_render__title("extract", command)
            )
        },
        morph = epwshiftr_cli_render_morph(result, command),
        epwshiftr_cli_render_default(result)
    )
}
# }}}

# }}}

# Declarative CLI table & multi-section specifications {{{

# Split a space-delimited column specification into a character vector.
# cli_render__cols {{{
cli_render__cols <- function(x) {
    if (is.null(x) || length(x) > 1L) {
        x
    } else {
        strsplit(x, " ", fixed = TRUE)[[1L]]
    }
}
# }}}

# Declarative lookup table for static (group, command) table views, multi-
# section views, command aliases, and fallback titles.
CLI_RENDER_SPECS <- local({
    tbl <- function(
        title,
        columns = NULL,
        field = NULL,
        show_types = TRUE,
        optional = FALSE
    ) {
        list(
            field = field,
            title = title,
            columns = cli_render__cols(columns),
            show_types = show_types,
            optional = optional
        )
    }
    sum_f <- function(field = "summary", title = "Summary", optional = FALSE) {
        list(field = field, title = title, optional = optional)
    }
    sum_k <- function(fields, title = "Summary") {
        list(fields = cli_render__cols(fields), title = title, optional = FALSE)
    }
    sec <- function(heading = NULL, summaries = list(), tables = list()) {
        list(heading = heading, summaries = summaries, tables = tables)
    }

    c_out <- "path case_id source_id experiment_id variant_label period morph_id"
    c_case <- "status source_id experiment_id variant_label grid_label period missing_reason"
    c_dl <- "status filename bytes_done size speed_bps eta_seconds attempts last_error job_id session_id task_id file_key"
    c_node <- "data_node service failure_rate failure_count success_count last_latency cooldown_until"
    c_chg <- "change_type filename size version data_node file_key query_id"
    c_sfile <- "relative_path size mtime"
    c_sreg <- "source file_key relative_path size"

    raw_tables <- list(
        "query:list" = c(
            "Stored ESGF queries",
            "query_id label tracked last_checked_at created_at updated_at"
        ),
        "query:preview" = c(
            "Query update preview",
            "query_id label file_total current_count new_count changed_count stale_count"
        ),
        "query:update" = c(
            "Query updates",
            "query_id label new_count changed_count stale_count file_total created_at update_id"
        ),
        "download:logs" = c(
            "Download events",
            "line message created_at event status error data_node job_id session_id task_id file_key"
        ),
        "download:jobs" = c(
            "Download jobs",
            "status mode bytes_done bytes_total speed_bps active_task_count job_id pid error created_at updated_at"
        ),
        "download:daemon" = c(
            "Download daemon",
            "status daemon_id pid port heartbeat_at error started_at stopped_at"
        ),
        "download:status" = c("Download tasks", c_dl),
        "download:sessions" = c(
            "Download sessions",
            "session_id status task_count label created_at finished_at started_at"
        ),
        "download:nodes" = c("Data nodes", paste(c_node, "dry_run")),
        "extract:plan" = c(
            "Extraction plan",
            "plan_id status query_id file_key site_id variable_id time_start time_stop"
        ),
        "extract:run" = c(
            "Extraction run",
            "plan_id status query_id site_id variable_id attempt_count last_error updated_at"
        ),
        "extract:retry" = c(
            "Extraction retry",
            "plan_id status query_id site_id variable_id attempt_count last_error dry_run"
        ),
        "extract:coverage" = c(
            "Extraction coverage",
            "plan_id complete status query_id site_id variable_id output_rows output_time_count last_error"
        ),
        "extract:artifacts" = c(
            "Extraction artifacts",
            "artifact_id kind role relative_path path created_at"
        ),
        "morph:variables" = c("Morph variables", "variable_id"),
        "morph:backends" = c(
            "Morph backends",
            "backend label required_variables methods"
        ),
        "morph:epw" = c("Morph EPW outputs", c_out),
        "morph:retry" = c(
            "Morph retry",
            "morph_id status label summary_id reference_summary_id baseline_id strict last_error dry_run case_id row_count output_path"
        ),
        "morph:status" = c(
            "Morph status",
            "morph_id status label strict summary_id reference_summary_id baseline_id updated_at last_error"
        ),
        "morph:outputs" = c("Morph outputs", c_out)
    )

    list(
        aliases = c(
            "download:events" = "download:logs",
            "download:stop" = "download:jobs",
            "download:tasks" = "download:status",
            "download:reset-nodes" = "download:nodes"
        ),
        tables = lapply(raw_tables, function(v) tbl(v[[1L]], v[[2L]])),
        sections = list(
            "doctor" = sec(
                "epwshiftr doctor",
                list(sum_f()),
                list(tbl("Checks", "check status message detail", "checks"))
            ),
            "query:preview" = sec(
                "Query update preview",
                list(sum_f()),
                list(tbl("Changes", c_chg, "changes"))
            ),
            "download:preflight" = sec(
                "Download preflight",
                list(sum_f()),
                list(
                    tbl(
                        "Changes",
                        setdiff(cli_render__cols(c_chg), "query_id"),
                        "changes"
                    ),
                    tbl(
                        "Files",
                        "status filename size local_path file_key",
                        "files"
                    ),
                    tbl(
                        "Candidates",
                        "data_node priority probe_ok probe_latency probe_cached target_rel_path file_key url",
                        "candidates"
                    )
                )
            ),
            "download:watch" = sec(
                "Download activity",
                list(sum_f()),
                list(
                    tbl(
                        "Jobs",
                        "status mode bytes_done bytes_total speed_bps active_task_count job_id pid error",
                        "jobs",
                        optional = TRUE
                    ),
                    tbl("Tasks", c_dl, "tasks"),
                    tbl("Data nodes", c_node, "nodes"),
                    tbl(
                        "Recent events",
                        "created_at event status error job_id session_id task_id file_key",
                        "events"
                    )
                )
            ),
            "esgf:report" = sec(
                "ESGF report",
                tables = list(
                    tbl(
                        "Summary",
                        "query_id label tracked file_total current_count local_count bytes_missing download_incomplete download_retryable last_download_session_id",
                        "summary"
                    ),
                    tbl(
                        "Updates",
                        "query_id new_count changed_count stale_count file_total created_at update_id",
                        "updates"
                    ),
                    tbl("Changes", c_chg, "changes"),
                    tbl(
                        "Downloads",
                        "status filename bytes_done size last_error session_id task_id file_key",
                        "downloads"
                    ),
                    tbl("Data nodes", c_node, "nodes")
                )
            ),
            "storage:report" = sec(
                "Storage report",
                list(sum_f()),
                list(
                    tbl("Download files", c_sfile, "downloads"),
                    tbl("Registered files", c_sreg, "registered"),
                    tbl("Untracked files", c_sfile, "untracked_files"),
                    tbl("Missing records", c_sreg, "missing_records"),
                    tbl("Temporary files", c_sfile, "tmp"),
                    tbl(
                        "Orphan records",
                        "file_key filename size local_path",
                        "orphan_records"
                    )
                )
            ),
            "storage:validate" = sec(
                "Storage validation",
                list(sum_f()),
                list(
                    tbl(
                        "Files",
                        "file_key filename local_path exists size_ok checksum_ok layout_ok",
                        "files"
                    ),
                    tbl(
                        "Artifacts",
                        "artifact_id file_key relative_path exists size_ok",
                        "artifacts"
                    ),
                    tbl("Untracked files", c_sfile, "untracked"),
                    tbl(
                        "Actions",
                        "action target file_key from to reason",
                        "actions"
                    )
                )
            ),
            "shift:run" = sec(
                "Shift workflow",
                list(
                    sum_f("intent", "Requested workflow", optional = TRUE),
                    sum_k("status run_id query_id morph_id diagnostic_count")
                ),
                list(
                    tbl("Cases", c_case, "cases"),
                    tbl("Outputs", c_out, "outputs"),
                    tbl(
                        "Next steps",
                        "step command",
                        "next_steps",
                        show_types = FALSE
                    )
                )
            ),
            "shift:show" = sec(
                "Shift workflow run",
                list(sum_f("run", "Run")),
                list(
                    tbl("Cases", c_case, "cases", show_types = FALSE),
                    tbl(
                        "Outputs",
                        "path export_path source_id experiment_id variant_label period",
                        "outputs",
                        show_types = FALSE
                    ),
                    tbl(
                        "Events",
                        "created_at stage status message",
                        "events",
                        show_types = FALSE
                    ),
                    tbl(
                        "Diagnostics",
                        "stage severity code message action",
                        "diagnostics",
                        show_types = FALSE
                    )
                )
            ),
            "shift:config:validate" = sec(
                "Shift config validation",
                list(
                    sum_f("intent", "Requested workflow"),
                    sum_k("status config validation readiness")
                ),
                list(
                    tbl("Cases", NULL, "cases"),
                    tbl("Plan", NULL, "explain"),
                    tbl("Selected models", NULL, "selected_models"),
                    tbl(
                        "Readiness checks",
                        "severity code message action",
                        "diagnostics"
                    )
                )
            ),
            "shift:watch" = sec(
                tables = list(
                    tbl("Outputs", c_out, "outputs", show_types = FALSE),
                    tbl(
                        "Diagnostics",
                        "stage severity code message query_id plan_id morph_id action",
                        "diagnostics",
                        show_types = FALSE
                    )
                )
            ),
            "morph:run" = sec(
                "Morph run",
                list(sum_k("status morph_id diagnostic_count")),
                list(
                    tbl(
                        "Plan",
                        "morph_id status summary_id reference_summary_id baseline_id strict last_error",
                        "plan"
                    ),
                    tbl(
                        "Results",
                        "case_id row_count output_path morph_id",
                        "results"
                    )
                )
            )
        ),
        titles = list(
            query = c(
                status = "Query status",
                files = "Query files",
                updates = "Query updates",
                changes = "Query changes",
                tags = "Query tags",
                track = "Query tracking",
                untrack = "Query tracking",
                tag = "Query tags",
                untag = "Query tags",
                remove = "Query removal",
                .default = "Query result"
            ),
            download = c(
                run = "Download run",
                resume = "Download resume",
                verify = "Download verification",
                cancel = "Download cancellation",
                retry = "Download retry",
                .default = "Download result"
            ),
            storage = c(
                repair = "Storage repair",
                cleanup = "Storage cleanup",
                .default = "Storage result"
            ),
            shift = c(
                cancel = "Shift cancellation",
                logs = "Shift job logs",
                status = "Shift status",
                diagnostics = "Shift diagnostics",
                outputs = "Shift outputs",
                data = "Shift data",
                resume = "Shift resume",
                .default = "Shift result"
            ),
            extract = c(.default = "Extraction result"),
            morph = c(run = "Morph run", .default = "Morph result")
        )
    )
})

# Resolve the static fallback title for a CLI group/command pair.
# cli_render__title {{{
cli_render__title <- function(group, command) {
    titles <- CLI_RENDER_SPECS$titles[[group]]
    if (is.null(titles)) {
        return(NULL)
    }
    key <- if (!is.null(command) && command %in% names(titles)) {
        command
    } else {
        ".default"
    }
    unname(titles[[key]])
}
# }}}

# Render a command whose human output is one declarative table specification.
# cli_render__dispatch_table {{{
cli_render__dispatch_table <- function(result, group, command) {
    if (is.null(command)) {
        return(FALSE)
    }
    key <- paste(group, command, sep = ":")
    if (key %in% names(CLI_RENDER_SPECS$aliases)) {
        key <- CLI_RENDER_SPECS$aliases[[key]]
    }
    spec <- CLI_RENDER_SPECS$tables[[key]]
    if (is.null(spec)) {
        return(FALSE)
    }
    epwshiftr_cli_render_table(
        result,
        title = spec$title,
        columns = spec$columns,
        show_types = shift_stage__coalesce(spec$show_types, TRUE)
    )
    TRUE
}
# }}}

# Render a declarative multi-section CLI view from CLI_RENDER_SPECS$sections.
# cli_render__dispatch_section {{{
cli_render__dispatch_section <- function(result, key) {
    spec <- CLI_RENDER_SPECS$sections[[key]]
    if (!is.null(spec$heading)) {
        cli::cli_h1(spec$heading)
    }
    for (s in spec$summaries) {
        value <- if (!is.null(s$fields)) {
            result[intersect(s$fields, names(result))]
        } else {
            result[[s$field]]
        }
        if (!isTRUE(s$optional) || !is.null(value)) {
            epwshiftr_cli_render_summary(value, s$title)
        }
    }
    for (t in spec$tables) {
        value <- result[[t$field]]
        if (!isTRUE(t$optional) || !is.null(value)) {
            epwshiftr_cli_render_table(
                value,
                title = t$title,
                columns = t$columns,
                show_types = shift_stage__coalesce(t$show_types, TRUE)
            )
        }
    }
    invisible(NULL)
}
# }}}

# }}}

# Command-group CLI renderers {{{

# epwshiftr_cli_render_query {{{
epwshiftr_cli_render_query <- function(result, command, context = NULL) {
    if (identical(command, "search")) {
        if (is.list(result) && !is.data.frame(result)) {
            cli::cli_h1("ESGF search")
            epwshiftr_cli_render_summary(
                result[setdiff(names(result), "query")],
                "Dry run"
            )
            if (!is.null(result$query)) {
                epwshiftr_cli_render_default(
                    result$query,
                    title = "Query parameters"
                )
            }
            return(invisible(NULL))
        }
        columns <- context$columns
        if (!is.null(columns)) {
            missing <- setdiff(columns, names(result))
            if (length(missing)) {
                epwshiftr_cli_usage_abort(sprintf(
                    "Unknown display column(s): %s. Add them to --fields if they are ESGF result fields.",
                    paste(missing, collapse = ", ")
                ))
            }
        } else {
            columns <- cli_render__cols(
                "title filename variable_id experiment_id source_id data_node size id dataset_id"
            )
        }
        return(epwshiftr_cli_render_table(
            result,
            title = "ESGF search results",
            columns = columns
        ))
    }
    if (
        identical(command, "preview") &&
            is.list(result) &&
            !is.data.frame(result)
    ) {
        return(cli_render__dispatch_section(result, "query:preview"))
    }
    if (identical(command, "show")) {
        return(epwshiftr_cli_render_named_list(result, title = "Query"))
    }
    if (cli_render__dispatch_table(result, "query", command)) {
        return(invisible(NULL))
    }
    epwshiftr_cli_render_default(
        result,
        title = cli_render__title("query", command)
    )
}
# }}}

# epwshiftr_cli_render_download {{{
epwshiftr_cli_render_download <- function(result, command, action = NULL) {
    if (identical(command, "preflight")) {
        return(cli_render__dispatch_section(result, "download:preflight"))
    }
    if (identical(command, "watch")) {
        return(epwshiftr_cli_render_download_watch(result))
    }
    if (identical(command, "config")) {
        return(epwshiftr_cli_render_named_list(
            result,
            title = paste(
                "Download config",
                shift_stage__coalesce(shift_ui_state__chr1(action), "")
            )
        ))
    }
    if (cli_render__dispatch_table(result, "download", command)) {
        return(invisible(NULL))
    }
    epwshiftr_cli_render_default(
        result,
        title = cli_render__title("download", command)
    )
}
# }}}

# epwshiftr_cli_render_download_watch {{{
epwshiftr_cli_render_download_watch <- function(result) {
    cli_render__dispatch_section(result, "download:watch")
}
# }}}

# epwshiftr_cli_render_storage {{{
epwshiftr_cli_render_storage <- function(result, command, action = NULL) {
    if (identical(command, "report")) {
        if (is.list(result) && !is.data.frame(result)) {
            return(cli_render__dispatch_section(result, "storage:report"))
        }
        return(epwshiftr_cli_render_summary(result, "Storage report"))
    }
    if (identical(command, "validate")) {
        return(cli_render__dispatch_section(result, "storage:validate"))
    }
    if (identical(command, "layout")) {
        return(epwshiftr_cli_render_summary(
            result,
            paste(
                "Storage layout",
                shift_stage__coalesce(shift_ui_state__chr1(action), "")
            )
        ))
    }
    epwshiftr_cli_render_default(
        result,
        title = cli_render__title("storage", command)
    )
}
# }}}

# epwshiftr_cli_render_shift {{{
epwshiftr_cli_render_shift <- function(result, command) {
    if (identical(command, "list")) {
        return(cli_shift__render_history(result))
    }
    if (identical(command, "summary")) {
        return(cli_shift__render_summary(result))
    }
    if (is.list(result) && !is.data.frame(result) && !is.null(result$batch)) {
        return(cli_shift__render_batch(result))
    }
    if (
        is.data.frame(result) &&
            "method" %in% names(result) &&
            command %in% c("outputs", "diagnostics")
    ) {
        return(cli_shift__render_records(result, command))
    }
    if (identical(command, "run")) {
        return(cli_render__dispatch_section(result, "shift:run"))
    }
    if (identical(command, "show")) {
        return(epwshiftr_cli_render_shift_show(result))
    }
    if (identical(command, "config")) {
        return(epwshiftr_cli_render_shift_config(result))
    }
    if (identical(command, "watch")) {
        return(epwshiftr_cli_render_shift_watch(result))
    }
    epwshiftr_cli_render_default(
        result,
        title = cli_render__title("shift", command)
    )
}
# }}}

# Emit a named list of scalar fields as aligned labeled lines via shared
# shift_ui_render primitives.
# cli_render__emit_fields {{{
cli_render__emit_fields <- function(values, width = shift_ui_render__width()) {
    for (name in names(values)) {
        val <- values[[name]]
        if (!is.null(shift_ui_state__chr1(val))) {
            cli::cli_verbatim(shift_ui_render__labeled_lines(name, val, width))
        }
    }
    invisible(NULL)
}
# }}}

# Join present, non-empty identity fields from a single-row data.table.
# cli_render__row_identity {{{
cli_render__row_identity <- function(row, fields) {
    present <- intersect(fields, names(row))
    vals <- if (length(present)) {
        unlist(row[, present, with = FALSE], use.names = FALSE)
    } else {
        character()
    }
    paste(shift_ui_state__nonempty(vals), collapse = " / ")
}
# }}}

# Render complete run/batch identities and their exact stores so a history row
# can always be used with show/watch, even when child stores are independent.
# cli_shift__render_history {{{
cli_shift__render_history <- function(rows) {
    cli::cli_h1("Workflow history")
    if (!nrow(rows)) {
        cli::cli_text("No matching saved workflows.")
        return(invisible(NULL))
    }
    width <- shift_ui_render__width()
    for (index in seq_len(nrow(rows))) {
        row <- rows[index]
        values <- list()
        values[[epwshiftr_cli_title(row$type)]] <- row$id
        values$Status <- paste(
            shift_ui_state__nonempty(c(row$status, row$updated_at)),
            collapse = " \u00b7 "
        )
        plan_id <- cli_render__row_identity(row, c("method", "model"))
        if (nzchar(plan_id)) {
            values$Plan <- plan_id
        }
        values$Store <- row$store
        if (!is.na(row$error)) {
            values$Failure <- row$error
        }
        cli_render__emit_fields(values, width)
        cli::cli_verbatim("")
    }
    total <- shift_stage__coalesce(
        attr(rows, "shift_history_total"),
        nrow(rows)
    )
    if (total > nrow(rows)) {
        cli::cli_text(
            "{total - nrow(rows)} more records; increase --limit or filter --type/--status."
        )
    }
    cli::cli_text(
        "Use shift show/watch with --run or --batch and the row's --store path."
    )
    invisible(NULL)
}
# }}}

# Keep scientific identity, output type, denominators, and missing-file counts
# together in each comparison record. No weather metric implies method ranking.
# cli_shift__render_summary {{{
cli_shift__render_summary <- function(rows) {
    cli::cli_h1("Workflow comparison")
    if (!nrow(rows)) {
        cli::cli_text("No workflow cases to summarize.")
        return(invisible(NULL))
    }
    id_fields <- cli_render__cols(
        "method scale reconstruction model scenario member grid period"
    )
    weather_specs <- list(
        Temp = c("mean_temperature_c", "degC", "temperature_hours"),
        Humidity = c("mean_relative_humidity_pct", "%", "humidity_hours"),
        Wind = c("mean_wind_speed_ms", "m/s", "wind_hours"),
        Solar = c(
            "mean_global_horizontal_radiation_wh_m2",
            "Wh/m2 per hour",
            "radiation_hours"
        )
    )
    width <- shift_ui_render__width()
    for (index in seq_len(nrow(rows))) {
        row <- rows[index]
        output <- shift_ui_state__nonempty(c(
            row$output_type,
            row$weather_years
        ))
        values <- list(
            Plan = cli_render__row_identity(row, id_fields),
            Status = sprintf(
                "%s \u00b7 %d/%d cases complete \u00b7 %d EPW files (%d available)",
                row$status,
                row$completed_cases,
                row$cases,
                row$epw_files,
                row$available_files
            ),
            Output = if (length(output)) {
                paste(output, collapse = " \u00b7 ")
            } else {
                "Not generated"
            },
            Checks = sprintf(
                "%d warnings \u00b7 %d errors",
                row$warnings,
                row$errors
            ),
            Fields = row$field_roles
        )
        if ("weather_hours" %in% names(row)) {
            values$Weather <- sprintf(
                "%d hourly rows \u00b7 %d unreadable files",
                row$weather_hours,
                row$unreadable_files
            )
            for (nm in names(weather_specs)) {
                sp <- weather_specs[[nm]]
                values[[nm]] <- sprintf(
                    "%.2f %s \u00b7 %d valid hours",
                    row[[sp[[1L]]]],
                    sp[[2L]],
                    row[[sp[[3L]]]]
                )
            }
            if (!is.na(row$weather_error)) {
                values$Failure <- row$weather_error
            }
        }
        cli_render__emit_fields(values, width)
        cli::cli_verbatim("")
    }
    invisible(NULL)
}
# }}}

# Keep the default overview bounded and render complete wrapped records only
# when explicitly requested. Debug additionally exposes raw JSON payloads.
# epwshiftr_cli_render_shift_show {{{
epwshiftr_cli_render_shift_show <- function(result) {
    detail <- shift_stage__coalesce(attr(result, "shift_ui_detail"), "normal")
    if (identical(detail, "normal")) {
        return(cli_render__dispatch_section(result, "shift:show"))
    }
    cli::cli_h1("Shift workflow run")
    for (name in cli_render__cols(
        "run cases outputs events diagnostics explain"
    )) {
        cli_shift__render_detail_records(
            result[[name]],
            epwshiftr_cli_title(name),
            debug = identical(detail, "debug")
        )
    }
    invisible(NULL)
}
# }}}

# Print every row and every selected field without cell or row truncation.
# Vertical records keep long paths, actions and raw payloads readable on small
# terminals while preserving their complete text across wrapped lines.
# cli_shift__render_detail_records {{{
cli_shift__render_detail_records <- function(rows, title, debug = FALSE) {
    rows <- data.table::as.data.table(rows)
    width <- shift_ui_render__width()
    cli::cli_h2(title)
    if (!nrow(rows)) {
        cli::cli_alert_info("No rows.")
        return(invisible(NULL))
    }
    fields <- names(rows)
    if (!debug) {
        fields <- fields[!grepl("_json$", fields)]
    }
    for (index in seq_len(nrow(rows))) {
        cli::cli_h3("{title} {index}/{nrow(rows)}")
        values <- stats::setNames(
            lapply(fields, function(f) {
                paste(
                    epwshiftr_cli_format_named_cell(rows[[f]][index], f),
                    collapse = ", "
                )
            }),
            epwshiftr_cli_title(fields)
        )
        cli_render__emit_fields(values, width)
    }
    invisible(NULL)
}
# }}}

# epwshiftr_cli_render_shift_config {{{
epwshiftr_cli_render_shift_config <- function(result) {
    if (identical(result$action, "example")) {
        cli::cli_h1("Shift config example")
        epwshiftr_cli_render_summary(
            result[intersect(c("status", "output"), names(result))],
            "Summary"
        )
        if (is.na(result$output[[1L]])) {
            json <- jsonlite::toJSON(
                result$config,
                auto_unbox = TRUE,
                pretty = TRUE,
                null = "null"
            )
            for (line in strsplit(json, "\n", fixed = TRUE)[[1L]]) {
                cli::cli_text("{line}")
            }
        }
        return(invisible(NULL))
    }
    if (identical(result$action, "validate")) {
        return(cli_render__dispatch_section(result, "shift:config:validate"))
    }
    epwshiftr_cli_render_default(result, title = "Shift config")
}
# }}}

# Render CLI watch snapshots with the same live dashboard as the foreground R
# reporter, then append terminal artifacts when they exist.
# epwshiftr_cli_render_shift_watch {{{
epwshiftr_cli_render_shift_watch <- function(
    result,
    detail = shift_stage__coalesce(attr(result, "shift_ui_detail"), "normal")
) {
    if (!is.null(result$batch)) {
        return(cli_shift__render_batch(result, detail = detail))
    }
    cli::cli_h1("Shift activity")
    view <- shift_ui_view__ui_table_view(
        row = result$run,
        cases = result$cases,
        events = shift_stage__coalesce(
            attr(result, "shift_ui_events"),
            result$events
        ),
        width = shift_ui_render__width(),
        detail = detail,
        outputs = result$outputs,
        diagnostics = result$diagnostics,
        ui_state = attr(result, "shift_ui_state")
    )
    shift_ui_render__print_view(view, include_tables = TRUE)
    cli_render__dispatch_section(result, "shift:watch")
}
# }}}

# epwshiftr_cli_render_morph {{{
epwshiftr_cli_render_morph <- function(result, command) {
    if (identical(command, "transforms")) {
        cli::cli_h1("Weather transformations")
        for (index in seq_len(nrow(result))) {
            row <- result[index]
            identity <- paste(
                row$scale,
                row$method,
                if (!is.na(row$reconstruction)) row$reconstruction else ""
            )
            cli::cli_text("{trimws(identity)} [{row$status}]")
            cli::cli_text("  {row$label}")
            cli::cli_text("  {row$evidence} \u00b7 {row$output_type}")
        }
        cli::cli_alert_info(
            "Use morph describe --scale SCALE --method METHOD for inputs, options, and field roles."
        )
        return(invisible(NULL))
    }
    if (identical(command, "describe")) {
        cli::cli_h1("Weather transformation")
        method_fields <- cli_render__cols(
            "method label scale status evidence references reconstruction reconstruction_choices output_type stochastic_variables validation"
        )
        epwshiftr_cli_render_summary(
            result[intersect(method_fields, names(result))],
            "Method"
        )
        cli_morph__render_inputs(result$required_inputs, "Required inputs")
        cli_morph__render_inputs(result$optional_inputs, "Optional inputs")
        cli::cli_h2("Options")
        width <- shift_ui_render__width()
        for (index in seq_len(nrow(result$options))) {
            row <- result$options[index]
            cli::cli_verbatim(shift_ui_render__prefixed_lines(
                "  ",
                sprintf(
                    "%s (%s): default %s; selected %s",
                    row$option,
                    row$type,
                    row$default,
                    row$value
                ),
                width
            ))
        }
        epwshiftr_cli_render_summary(result$field_roles, "EPW field roles")
        return(invisible(NULL))
    }
    if (
        identical(command, "run") && is.list(result) && !is.data.frame(result)
    ) {
        return(cli_render__dispatch_section(result, "morph:run"))
    }
    if (cli_render__dispatch_table(result, "morph", command)) {
        return(invisible(NULL))
    }
    epwshiftr_cli_render_table(
        result,
        title = cli_render__title("morph", command)
    )
}
# }}}

# Render AND/OR variable alternatives explicitly; the generic named-list
# renderer cannot display the unnamed vectors inside input contracts.
# cli_morph__render_inputs {{{
cli_morph__render_inputs <- function(inputs, title) {
    cli::cli_h2(title)
    if (!length(inputs)) {
        cli::cli_text("None.")
        return(invisible(NULL))
    }
    width <- shift_ui_render__width()
    for (role in names(inputs)) {
        input <- inputs[[role]]
        items <- sprintf(
            "%s: %s; frequency %s",
            role,
            paste(input$representations, collapse = " / "),
            paste(input$frequencies, collapse = " / ")
        )
        if (length(input$variable_sets)) {
            alts <- vapply(
                input$variable_sets,
                function(vars) paste0("(", paste(vars, collapse = " + "), ")"),
                character(1L)
            )
            items <- c(
                items,
                paste("Variables:", paste(alts, collapse = " OR "))
            )
        }
        if (length(input$variable_frequencies)) {
            vf <- vapply(
                input$variable_frequencies,
                paste,
                character(1L),
                collapse = "/"
            )
            items <- c(
                items,
                paste(
                    "Variable frequencies:",
                    paste(
                        names(input$variable_frequencies),
                        vf,
                        sep = "=",
                        collapse = ", "
                    )
                )
            )
        }
        if (length(input$calendars)) {
            items <- c(
                items,
                paste("Calendars:", paste(input$calendars, collapse = ", "))
            )
        }
        for (item in items) {
            cli::cli_verbatim(shift_ui_render__prefixed_lines(
                "  ",
                item,
                width
            ))
        }
    }
    invisible(NULL)
}
# }}}

# Keep one batch rendering contract for run receipts, show, status, and watch.
# Status-only results use their available summary without requiring case data.
# cli_shift__render_batch {{{
cli_shift__render_batch <- function(
    result,
    detail = shift_stage__coalesce(attr(result, "shift_ui_detail"), "normal")
) {
    if (!is.null(result$intent)) {
        epwshiftr_cli_render_summary(result$intent, "Requested workflow")
    }
    if (all(c("cases", "outputs", "diagnostics") %in% names(result))) {
        shift_ui_render__print_view(
            shift_batch_ui__view(result, detail = detail),
            include_tables = !identical(detail, "normal")
        )
    } else {
        epwshiftr_cli_render_summary(
            as.list(result$batch[1L]),
            "Future EPW Batch"
        )
        epwshiftr_cli_render_table(
            result$children,
            "Children",
            cli_render__cols("method model scale status run_id")
        )
    }
    if (!is.null(result$outputs) && nrow(result$outputs)) {
        cli_shift__render_records(result$outputs, "outputs")
    }
    if (!identical(detail, "normal") && !is.null(result$explain)) {
        epwshiftr_cli_render_table(result$explain, "Plan details")
    }
    epwshiftr_cli_render_table(
        result$next_steps,
        "Next steps",
        c("step", "command"),
        show_types = FALSE
    )
    invisible(NULL)
}
# }}}

# Keep batch output and diagnostic identities attached to their payloads.
# Wrapped records preserve full paths and actionable messages on narrow TTYs.
# cli_shift__render_records {{{
cli_shift__render_records <- function(rows, command) {
    width <- shift_ui_render__width()
    id_fields <- cli_render__cols(
        "method scale reconstruction model member experiment_id period"
    )
    payload_fields <- cli_render__cols(
        if (identical(command, "outputs")) {
            "output_type weather_year export_path path"
        } else {
            "severity code message action"
        }
    )
    cli::cli_h2(epwshiftr_cli_title(command))
    for (index in seq_len(nrow(rows))) {
        row <- rows[index]
        cli::cli_verbatim(shift_ui_render__wrap_lines(
            cli_render__row_identity(row, id_fields),
            width
        ))
        present <- intersect(payload_fields, names(row))
        values <- stats::setNames(
            lapply(present, function(f) as.character(row[[f]])),
            epwshiftr_cli_title(present)
        )
        cli_render__emit_fields(values, width)
    }
    invisible(NULL)
}
# }}}

# }}}

# Generic object & ESGF receipt renderers {{{

# epwshiftr_cli_render_default {{{
epwshiftr_cli_render_default <- function(result, title = NULL) {
    if (is.data.frame(result)) {
        return(epwshiftr_cli_render_table(result, title = title))
    }
    if (is.list(result)) {
        return(epwshiftr_cli_render_named_list(result, title = title))
    }
    if (!is.null(title)) {
        cli::cli_h2(title)
    }
    value <- epwshiftr_cli_format_cell(result)
    cli::cli_text("{value}")
    invisible(NULL)
}
# }}}

# epwshiftr_cli_render_named_list {{{
epwshiftr_cli_render_named_list <- function(result, title = NULL) {
    if (!is.null(title) && nzchar(trimws(title))) {
        cli::cli_h1(trimws(title))
    }
    if (!length(result)) {
        cli::cli_alert_info("No values.")
        return(invisible(NULL))
    }
    nms <- shift_stage__coalesce(names(result), rep("", length(result)))
    scalar <- vapply(result, cli_render__is_scalar, logical(1L))
    if (any(scalar)) {
        epwshiftr_cli_render_summary(result[scalar], "Values")
    }
    for (i in which(!scalar)) {
        subtitle <- if (nzchar(nms[[i]])) epwshiftr_cli_title(nms[[i]])
        epwshiftr_cli_render_default(result[[i]], title = subtitle)
    }
    invisible(NULL)
}
# }}}

# epwshiftr_cli_render_summary {{{
epwshiftr_cli_render_summary <- function(x, title = "Summary") {
    if (is.null(x)) {
        return(invisible(NULL))
    }
    if (is.data.frame(x)) {
        if (!nrow(x)) {
            cli::cli_h2(title)
            cli::cli_alert_info("No rows.")
            return(invisible(NULL))
        }
        if (nrow(x) > 1L) {
            return(epwshiftr_cli_render_table(x, title = title))
        }
        x <- as.list(x[1L, , drop = FALSE])
    }
    if (!is.list(x)) {
        x <- list(value = x)
    }
    cli::cli_h2(title)
    for (name in names(x)) {
        label <- epwshiftr_cli_title(name)
        value <- paste(
            epwshiftr_cli_format_named_cell(x[[name]], name),
            collapse = ", "
        )
        cli::cli_text("{label}: {value}")
    }
    invisible(NULL)
}
# }}}

# Render the shared double-line heading used by established ESGF object
# receipts. Keeping the rule style here prevents high-level Shift facades from
# drifting away from EsgQuery and EsgResult output again.
# esg__print_header {{{
esg__print_header <- function(title) {
    checkmate::assert_string(title, min.chars = 1L)
    div <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
    cli::cli_rule(title)
    cli::cli_end(div)
    invisible(NULL)
}
# }}}

# Render named scalar facts as the bullet summary established by EsgResult.
# Empty facts are removed so incomplete objects do not display fake values.
# esg__print_facts {{{
esg__print_facts <- function(x) {
    if (is.null(x) || !length(x)) {
        return(invisible(NULL))
    }
    checkmate::assert_list(x, names = "named")
    keep <- vapply(
        x,
        function(v) length(shift_ui_state__nonempty(v)) > 0L,
        logical(1L)
    )
    for (name in names(x)[keep]) {
        value <- paste(as.character(x[[name]]), collapse = " \u00b7 ")
        cli::cli_bullets(c("*" = "{name}: {value}"))
    }
    invisible(NULL)
}
# }}}

# Render query parameters through QueryParam's canonical display formatter so
# EsgQuery and ShiftRequest never diverge on names, ordering, or negation.
# esg__print_parameters {{{
esg__print_parameters <- function(params, title = "Query parameters") {
    checkmate::assert_string(title, min.chars = 1L)
    cli::cli_rule(title)
    rendered <- query_param__display(params)
    if (!length(rendered)) {
        cli::cli_alert_info("No query constraints.")
        return(invisible(NULL))
    }
    cli::cli_bullets(stats::setNames(rendered, rep("*", length(rendered))))
    invisible(NULL)
}
# }}}

# Compose the high-level Shift request receipt with the canonical QueryParam
# renderer. Callers decide whether an index node is pinned or auto-selected.
# esg__print_query {{{
esg__print_query <- function(index_node, params, title = "ESGF query") {
    esg__print_header(title)
    esg__print_facts(list("Index node" = index_node))
    esg__print_parameters(params)
    invisible(NULL)
}
# }}}

# }}}

# Boxed CLI table engine & cell formatters {{{

# epwshiftr_cli_render_table {{{
epwshiftr_cli_render_table <- function(
    x,
    title = NULL,
    columns = NULL,
    max_rows = 20L,
    show_types = TRUE,
    more_hint = "use --json for full output.",
    hidden_hint = "Use --json for full output.",
    total_rows = NULL
) {
    x <- if (is.null(x)) {
        data.frame()
    } else if (is.data.frame(x)) {
        as.data.frame(x)
    } else if (
        is.list(x) && all(vapply(x, cli_render__is_scalar, logical(1L)))
    ) {
        as.data.frame(x, stringsAsFactors = FALSE)
    } else {
        data.frame(
            value = epwshiftr_cli_format_cell(x),
            stringsAsFactors = FALSE
        )
    }
    if (is.null(total_rows)) {
        total_rows <- nrow(x)
    } else {
        checkmate::assert_count(total_rows)
        total_rows <- max(as.integer(total_rows), nrow(x))
    }
    if (
        all(c("bytes_done", "size") %in% names(x)) && !"progress" %in% names(x)
    ) {
        x$progress <- cli_render__progress_bar(x$bytes_done, x$size)
        if (
            !is.null(columns) &&
                all(c("bytes_done", "size") %in% columns) &&
                !"progress" %in% columns
        ) {
            columns <- append(
                columns,
                "progress",
                after = match("size", columns)
            )
        }
    }
    if (!is.null(title) && nzchar(title)) {
        cli::cli_h2(title)
    }
    if (!nrow(x)) {
        cli::cli_alert_info("No rows.")
        return(invisible(NULL))
    }
    available <- names(x)
    picked <- if (is.null(columns)) available else intersect(columns, available)
    columns <- if (length(picked)) picked else available
    if (!length(columns)) {
        cli::cli_alert_info("No printable columns.")
        return(invisible(NULL))
    }
    shown <- x[seq_len(min(nrow(x), max_rows)), columns, drop = FALSE]
    display <- as.data.frame(
        stats::setNames(
            lapply(names(shown), function(name) {
                cli_render__truncate_cell(
                    epwshiftr_cli_format_named_cell(shown[[name]], name),
                    cli_render__column_max_width(name)
                )
            }),
            names(shown)
        ),
        stringsAsFactors = FALSE
    )

    chars <- cli_render__table_border("single")
    header <- epwshiftr_cli_title(names(display))
    types <- if (isTRUE(show_types)) cli_render__table_types(shown)
    widths <- cli_render__column_widths(display, header, types)
    max_width <- shift_ui_render__width(getOption("width", 80L))
    if (max_width < 40L) {
        max_width <- 80L
    }
    sep_w <- cli::ansi_nchar(chars$vertical, type = "width")
    totals <- cumsum(widths) +
        (2L * chars$margin + sep_w) * seq_along(widths) +
        sep_w
    min_cols <- max(1L, min(2L, ncol(display)))
    keep <- ncol(display)
    while (keep > min_cols && totals[[keep]] > max_width) {
        keep <- keep - 1L
    }
    dropped <- if (keep < ncol(display)) {
        names(display)[seq.int(keep + 1L, ncol(display))]
    } else {
        character()
    }
    display <- display[seq_len(keep)]
    shown <- shown[seq_len(keep)]
    header <- header[seq_len(keep)]
    if (!is.null(types)) {
        types <- types[seq_len(keep)]
    }
    widths <- widths[seq_len(keep)]

    lines <- cli_render__table_lines(
        display,
        header = header,
        types = types,
        widths = widths,
        chars = chars,
        align = cli_render__table_alignments(shown),
        row_style = cli_render__table_row_styles(shown)
    )
    cli::cli_verbatim(lines)
    extra <- total_rows - nrow(shown)
    if (extra > 0L) {
        # cli_bullets wraps long continuation hints to the active console
        # width, whereas cli_alert_info intentionally keeps one physical line.
        cli::cli_bullets(c("i" = "{extra} more rows; {more_hint}"))
    }
    if (length(dropped)) {
        message <- paste0(
            "Hidden columns for console width: ",
            paste(dropped, collapse = ", "),
            ". ",
            hidden_hint
        )
        cli::cli_bullets(stats::setNames(message, "i"))
    }
    invisible(NULL)
}
# }}}

# cli_render__column_max_width {{{
cli_render__column_max_width <- function(name) {
    name <- tolower(name)
    if (grepl("url|path|detail|message|error|hint", name)) {
        64L
    } else if (grepl("(^|_)id$|_id$|key", name)) {
        28L
    } else if (grepl("filename|label|title", name)) {
        40L
    } else {
        24L
    }
}
# }}}

# Compute display width of each table column using shift_ui_render__display_max.
# cli_render__column_widths {{{
cli_render__column_widths <- function(body, header, types = NULL) {
    vapply(
        seq_along(body),
        function(i) {
            shift_ui_render__display_max(c(
                header[[i]],
                if (!is.null(types)) types[[i]],
                as.character(body[[i]])
            ))
        },
        integer(1L)
    )
}
# }}}

# cli_render__table_lines {{{
cli_render__table_lines <- function(
    body,
    header,
    types = NULL,
    widths = cli_render__column_widths(body, header, types),
    chars = cli_render__table_border("single"),
    align = rep("left", ncol(body)),
    row_style = rep("none", nrow(body))
) {
    if (!ncol(body)) {
        return(character())
    }
    rule <- function(pos) {
        p <- chars[[pos]]
        cells <- strrep(chars$horizontal, widths + chars$margin * 2L)
        paste0(p[["left"]], paste(cells, collapse = p[["mid"]]), p[["right"]])
    }
    pad <- strrep(" ", chars$margin)
    row_str <- function(vals, al) {
        cells <- mapply(
            cli::ansi_align,
            as.character(vals),
            widths,
            al,
            USE.NAMES = FALSE
        )
        paste0(
            chars$vertical,
            paste0(pad, cells, pad, collapse = chars$vertical),
            chars$vertical
        )
    }
    left_al <- rep("left", length(widths))
    lines <- c(rule("top"), row_str(cli::style_bold(header), left_al))
    if (!is.null(types)) {
        lines <- c(lines, row_str(cli::col_grey(types), left_al))
    }
    lines <- c(lines, rule("mid"))
    body_lines <- vapply(
        seq_len(nrow(body)),
        function(i) {
            vals <- vapply(body, `[[`, character(1L), i)
            cli_render__colorize(row_style[[i]], row_str(vals, align))
        },
        character(1L)
    )
    c(lines, body_lines, rule("bottom"))
}
# }}}

CLI_RENDER_CLASS_ABBR <- c(
    list = "<list>",
    integer = "<int>",
    numeric = "<num>",
    double = "<num>",
    character = "<char>",
    Date = "<Date>",
    complex = "<cplx>",
    factor = "<fctr>",
    POSIXct = "<POSc>",
    POSIXlt = "<POSt>",
    logical = "<lgcl>",
    IDate = "<IDat>",
    integer64 = "<i64>",
    raw = "<raw>",
    expression = "<expr>",
    ordered = "<ord>"
)

# cli_render__table_types {{{
cli_render__table_types <- function(x) {
    if (!length(x)) {
        return(character())
    }
    classes <- vapply(
        x,
        function(col) class(col)[[1L]],
        character(1L),
        USE.NAMES = FALSE
    )
    ifelse(
        classes %in% names(CLI_RENDER_CLASS_ABBR),
        unname(CLI_RENDER_CLASS_ABBR[classes]),
        paste0("<", classes, ">")
    )
}
# }}}

# cli_render__progress_bar {{{
cli_render__progress_bar <- function(done, total, width = 8L) {
    done <- suppressWarnings(as.numeric(done))
    total <- suppressWarnings(as.numeric(total))
    out <- rep("-", length(done))
    ok <- !is.na(done) & !is.na(total) & total > 0
    if (!any(ok)) {
        return(out)
    }
    ratio <- pmax(0, pmin(1, done[ok] / total[ok]))
    filled <- pmin(width, pmax(0L, round(ratio * width)))
    out[ok] <- sprintf(
        "[%s%s] %3.0f%%",
        strrep("#", filled),
        strrep("-", width - filled),
        ratio * 100
    )
    out
}
# }}}

CLI_RENDER_BORDERS <- list(
    single = list(
        top = c(left = "\u250c", mid = "\u252c", right = "\u2510"),
        mid = c(left = "\u251c", mid = "\u253c", right = "\u2524"),
        bottom = c(left = "\u2514", mid = "\u2534", right = "\u2518"),
        horizontal = "\u2500",
        vertical = "\u2502",
        margin = 1L
    ),
    ascii = list(
        top = c(left = "+", mid = "+", right = "+"),
        mid = c(left = "+", mid = "+", right = "+"),
        bottom = c(left = "+", mid = "+", right = "+"),
        horizontal = "-",
        vertical = "|",
        margin = 1L
    ),
    none = list(
        top = c(left = "", mid = "", right = ""),
        mid = c(left = "", mid = "", right = ""),
        bottom = c(left = "", mid = "", right = ""),
        horizontal = "",
        vertical = "  ",
        margin = 0L
    )
)

# cli_render__table_border {{{
cli_render__table_border <- function(border = "single") {
    border <- match.arg(border, names(CLI_RENDER_BORDERS))
    if (identical(border, "single") && !isTRUE(cli::is_utf8_output())) {
        border <- "ascii"
    }
    CLI_RENDER_BORDERS[[border]]
}
# }}}

# cli_render__table_alignments {{{
cli_render__table_alignments <- function(x) {
    num_pat <- "bytes|size|count|attempt|priority|latency|rate|total|done|missing|queued|error|cancelled|skipped"
    vapply(
        names(x),
        function(nm) {
            if (is.numeric(x[[nm]]) || grepl(num_pat, nm, ignore.case = TRUE)) {
                "right"
            } else {
                "left"
            }
        },
        character(1L)
    )
}
# }}}

# Map CLI status strings onto semantic tokens handled by
# shift_ui_render__colorize().
CLI_RENDER_STATUS_MAP <- local({
    groups <- list(
        failed = "error failed failure cancelled missing retracted invalid bad blocked",
        partial = "warning stale changed deprecated cooldown retry retryable interrupted",
        completed = "ok done current success verified complete completed",
        running = "queued downloading running enqueue probe new skipped",
        muted = "none unknown na"
    )
    unlist(lapply(names(groups), function(tok) {
        keys <- cli_render__cols(groups[[tok]])
        stats::setNames(rep(tok, length(keys)), keys)
    }))
})

# cli_render__status_token {{{
cli_render__status_token <- function(x) {
    key <- tolower(as.character(x[[1L]]))
    if (is.na(key) || !nzchar(key) || identical(key, "-")) {
        return("none")
    }
    if (key %in% names(CLI_RENDER_STATUS_MAP)) {
        unname(CLI_RENDER_STATUS_MAP[[key]])
    } else {
        "none"
    }
}
# }}}

# cli_render__colorize {{{
cli_render__colorize <- function(token, text) {
    if (identical(token, "none")) {
        return(text)
    }
    shift_ui_render__colorize(
        token,
        text,
        default = if (identical(token, "muted")) cli::col_grey
    )
}
# }}}

# cli_render__table_row_styles {{{
cli_render__table_row_styles <- function(x) {
    if (!nrow(x)) {
        return(character())
    }
    col <- intersect(c("status", "change_type", "check_status"), names(x))
    if (!length(col)) {
        return(rep("none", nrow(x)))
    }
    vapply(
        as.character(x[[col[[1L]]]]),
        function(st) {
            tok <- cli_render__status_token(st)
            if (tok %in% c("failed", "partial")) tok else "none"
        },
        character(1L),
        USE.NAMES = FALSE
    )
}
# }}}

# cli_render__truncate_cell {{{
cli_render__truncate_cell <- function(x, width) {
    x <- as.character(x)
    width <- as.integer(width[[1L]])
    if (is.na(width) || width < 4L) {
        return(x)
    }
    over <- !is.na(x) & cli::ansi_nchar(x, type = "width") > width
    if (any(over)) {
        x[over] <- cli::ansi_strtrim(x[over], width, ellipsis = "...")
    }
    x
}
# }}}

# epwshiftr_cli_format_named_cell {{{
epwshiftr_cli_format_named_cell <- function(x, name) {
    if (
        grepl(
            "(^status$|change_type|check_status|event$)",
            name,
            ignore.case = TRUE
        )
    ) {
        return(vapply(
            epwshiftr_cli_format_cell(x),
            function(v) cli_render__colorize(cli_render__status_token(v), v),
            character(1L),
            USE.NAMES = FALSE
        ))
    }
    if (
        is.logical(x) ||
            grepl(
                "(^|_)(ok|exists|tracked|cached|resume|run|execute|dry_run|incomplete|retryable|would_block)$",
                name,
                ignore.case = TRUE
            )
    ) {
        return(cli_render__color_boolean(epwshiftr_cli_format_cell(x), name))
    }
    if (grepl("bytes|size", name, ignore.case = TRUE)) {
        return(shift_ui_view__format_bytes(x, binary = FALSE, na = "-"))
    }
    if (
        grepl("(^|_)at$|time|date|until", name, ignore.case = TRUE) &&
            inherits(x, c("POSIXct", "POSIXlt", "Date"))
    ) {
        return(cli_render__format_time(x))
    }
    epwshiftr_cli_format_cell(x)
}
# }}}

# cli_render__color_boolean {{{
cli_render__color_boolean <- function(x, name) {
    bad <- grepl(
        "incomplete|retryable|would_block|dry_run",
        name,
        ignore.case = TRUE
    )
    vapply(
        tolower(as.character(x)),
        function(item) {
            tok <- if (item %in% c("yes", "true", "1")) {
                if (bad) "partial" else "completed"
            } else if (item %in% c("no", "false", "0")) {
                if (bad) "completed" else "failed"
            } else {
                "muted"
            }
            cli_render__colorize(tok, item)
        },
        character(1L),
        USE.NAMES = FALSE
    )
}
# }}}

# epwshiftr_cli_format_cell {{{
epwshiftr_cli_format_cell <- function(x) {
    if (is.null(x) || !length(x)) {
        return("-")
    }
    if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
        return(cli_render__format_time(x))
    }
    if (is.list(x) && !is.data.frame(x)) {
        return(vapply(
            x,
            function(v) paste(epwshiftr_cli_format_cell(v), collapse = ","),
            character(1L)
        ))
    }
    if (is.logical(x)) {
        return(as.character(ifelse(is.na(x), "-", ifelse(x, "yes", "no"))))
    }
    out <- as.character(x)
    out[is.na(out) | !nzchar(out)] <- "-"
    out
}
# }}}

# cli_render__format_time {{{
cli_render__format_time <- function(x) {
    out <- as.character(x)
    ok <- !is.na(x)
    if (inherits(x, c("POSIXct", "POSIXlt"))) {
        out[ok] <- format(as.POSIXct(x[ok], tz = "UTC"), "%Y-%m-%d %H:%M:%S %Z")
    }
    out[!ok | !nzchar(out)] <- "-"
    out
}
# }}}

# epwshiftr_cli_title {{{
epwshiftr_cli_title <- function(x) {
    x <- gsub("[_-]", " ", as.character(x))
    vapply(
        strsplit(x, " ", fixed = TRUE),
        function(parts) {
            parts <- parts[nzchar(parts)]
            if (!length(parts)) {
                return("")
            }
            paste(
                paste0(toupper(substring(parts, 1L, 1L)), substring(parts, 2L)),
                collapse = " "
            )
        },
        character(1L)
    )
}
# }}}

# cli_render__is_scalar {{{
cli_render__is_scalar <- function(x) {
    is.null(x) || (!is.data.frame(x) && !is.list(x) && length(x) <= 1L)
}
# }}}

# }}}

# vim: fdm=marker :
