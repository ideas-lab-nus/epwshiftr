epwshiftr_cli_context <- function(parsed) {
    args <- parsed$args
    if (isTRUE(parsed$help)) {
        return(list(group = "help", command = paste(args, collapse = " "), action = NULL))
    }
    if (!length(args)) {
        return(list(group = NULL, command = NULL, action = NULL))
    }
    if (identical(args[[1L]], "help")) {
        topic <- args[-1L]
        return(list(group = "help", command = paste(topic, collapse = " "), action = NULL))
    }
    context <- list(
        group = args[[1L]],
        command = if (length(args) >= 2L) args[[2L]] else NULL,
        action = if (length(args) >= 3L) args[[3L]] else NULL
    )
    context$columns <- epwshiftr_cli_context_columns(args)
    context
}


epwshiftr_cli_context_columns <- function(args) {
    if (length(args) < 2L || !identical(args[[1L]], "query") || !identical(args[[2L]], "search")) {
        return(NULL)
    }
    parsed <- epwshiftr_cli_parse_command(
        args[-seq_len(2L)],
        flags = c("--all", "--dry-run", "--no-progress"),
        options = c("--index-node", "--type", "--fields", "--columns", "--limit")
    )
    epwshiftr_cli_csv(parsed$options[["--columns"]])
}


epwshiftr_cli_render <- function(result, context = NULL) {
    if (is.null(context)) {
        context <- list()
    }
    if (is.character(result)) {
        return(epwshiftr_cli_render_help(result))
    }
    group <- context$group
    command <- context$command
    action <- context$action

    if (identical(group, "doctor")) {
        return(epwshiftr_cli_render_doctor(result))
    }
    if (identical(group, "query")) {
        return(epwshiftr_cli_render_query(result, command, context = context))
    }
    if (identical(group, "download")) {
        return(epwshiftr_cli_render_download(result, command, action))
    }
    if (identical(group, "esgf") && identical(command, "report")) {
        return(epwshiftr_cli_render_esgf_report(result))
    }
    if (identical(group, "storage")) {
        return(epwshiftr_cli_render_storage(result, command, action))
    }
    if (identical(group, "shift")) {
        return(epwshiftr_cli_render_shift(result, command))
    }
    if (identical(group, "extract")) {
        return(epwshiftr_cli_render_extract(result, command))
    }
    if (identical(group, "morph")) {
        return(epwshiftr_cli_render_morph(result, command))
    }
    epwshiftr_cli_render_default(result)
}


epwshiftr_cli_render_help <- function(lines) {
    for (line in lines) {
        cli::cli_text("{line}")
    }
    invisible(NULL)
}


epwshiftr_cli_render_doctor <- function(result) {
    cli::cli_h1("epwshiftr doctor")
    epwshiftr_cli_render_summary(result$summary, "Summary")
    epwshiftr_cli_render_table(
        result$checks,
        title = "Checks",
        columns = c("check", "status", "message", "detail")
    )
    invisible(NULL)
}


epwshiftr_cli_render_query <- function(result, command, context = NULL) {
    if (identical(command, "list")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Stored ESGF queries",
            columns = c("query_id", "label", "tracked", "last_checked_at", "created_at", "updated_at")
        ))
    }
    if (identical(command, "search")) {
        if (is.list(result) && !is.data.frame(result)) {
            cli::cli_h1("ESGF search")
            epwshiftr_cli_render_summary(result[setdiff(names(result), "query")], "Dry run")
            if (!is.null(result$query)) {
                epwshiftr_cli_render_default(result$query, title = "Query parameters")
            }
            return(invisible(NULL))
        }
        columns <- epwshiftr_cli_query_search_columns(result, context)
        return(epwshiftr_cli_render_table(
            result,
            title = "ESGF search results",
            columns = columns
        ))
    }
    if (identical(command, "preview")) {
        if (is.list(result) && !is.data.frame(result)) {
            cli::cli_h1("Query update preview")
            epwshiftr_cli_render_summary(result$summary, "Summary")
            epwshiftr_cli_render_table(
                result$changes,
                title = "Changes",
                columns = c("change_type", "filename", "size", "version", "data_node", "file_key", "query_id")
            )
            return(invisible(NULL))
        }
        return(epwshiftr_cli_render_table(
            result,
            title = "Query update preview",
            columns = c("query_id", "label", "file_total", "current_count", "new_count", "changed_count", "stale_count")
        ))
    }
    if (identical(command, "update")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Query updates",
            columns = c("query_id", "label", "new_count", "changed_count", "stale_count", "file_total", "created_at", "update_id")
        ))
    }
    if (identical(command, "show")) {
        return(epwshiftr_cli_render_named_list(result, title = "Query"))
    }
    title <- switch(
        command,
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
        "Query result"
    )
    epwshiftr_cli_render_default(result, title = title)
}


epwshiftr_cli_query_search_columns <- function(result, context = NULL) {
    columns <- context$columns
    if (!is.null(columns)) {
        return(epwshiftr_cli_validate_display_columns(result, columns))
    }
    c("title", "filename", "variable_id", "experiment_id", "source_id", "data_node", "size", "id", "dataset_id")
}


epwshiftr_cli_validate_display_columns <- function(x, columns) {
    missing <- setdiff(columns, names(x))
    if (length(missing)) {
        epwshiftr_cli_usage_abort(sprintf(
            "Unknown display column(s): %s. Add them to --fields if they are ESGF result fields.",
            paste(missing, collapse = ", ")
        ))
    }
    columns
}


epwshiftr_cli_render_download <- function(result, command, action = NULL) {
    if (identical(command, "preflight")) {
        return(epwshiftr_cli_render_download_preflight(result))
    }
    if (identical(command, "watch")) {
        return(epwshiftr_cli_render_download_watch(result))
    }
    if (identical(command, "logs") || identical(command, "events")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Download events",
            columns = c("line", "message", "created_at", "event", "status", "error", "data_node", "job_id", "session_id", "task_id", "file_key")
        ))
    }
    if (identical(command, "jobs") || identical(command, "stop")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Download jobs",
            columns = c("status", "mode", "bytes_done", "bytes_total", "speed_bps", "active_task_count", "job_id", "pid", "error", "created_at", "updated_at")
        ))
    }
    if (identical(command, "daemon")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Download daemon",
            columns = c("status", "daemon_id", "pid", "port", "heartbeat_at", "error", "started_at", "stopped_at")
        ))
    }
    if (identical(command, "status") || identical(command, "tasks")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Download tasks",
            columns = c("status", "filename", "bytes_done", "size", "speed_bps", "eta_seconds", "attempts", "last_error", "job_id", "session_id", "task_id", "file_key")
        ))
    }
    if (identical(command, "sessions")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Download sessions",
            columns = c("session_id", "status", "task_count", "label", "created_at", "finished_at", "started_at")
        ))
    }
    if (identical(command, "nodes") || identical(command, "reset-nodes")) {
        return(epwshiftr_cli_render_table(
            result,
            title = "Data nodes",
            columns = c("data_node", "service", "failure_rate", "failure_count", "success_count", "last_latency", "cooldown_until", "dry_run")
        ))
    }
    if (identical(command, "config")) {
        return(epwshiftr_cli_render_named_list(result, title = paste("Download config", epwshiftr_cli_string_default(action, ""))))
    }
    title <- switch(
        command,
        run = "Download run",
        resume = "Download resume",
        verify = "Download verification",
        cancel = "Download cancellation",
        retry = "Download retry",
        "Download result"
    )
    epwshiftr_cli_render_default(result, title = title)
}


epwshiftr_cli_render_download_preflight <- function(result) {
    cli::cli_h1("Download preflight")
    epwshiftr_cli_render_summary(result$summary, "Summary")
    epwshiftr_cli_render_table(
        result$changes,
        title = "Changes",
        columns = c("change_type", "filename", "size", "version", "data_node", "file_key")
    )
    epwshiftr_cli_render_table(
        result$files,
        title = "Files",
        columns = c("status", "filename", "size", "local_path", "file_key")
    )
    epwshiftr_cli_render_table(
        result$candidates,
        title = "Candidates",
        columns = c("data_node", "priority", "probe_ok", "probe_latency", "probe_cached", "target_rel_path", "file_key", "url")
    )
    invisible(NULL)
}


epwshiftr_cli_render_download_watch <- function(result) {
    cli::cli_h1("Download activity")
    epwshiftr_cli_render_summary(result$summary, "Summary")
    if (!is.null(result$jobs)) {
        epwshiftr_cli_render_table(
            result$jobs,
            title = "Jobs",
            columns = c("status", "mode", "bytes_done", "bytes_total", "speed_bps", "active_task_count", "job_id", "pid", "error")
        )
    }
    epwshiftr_cli_render_table(
        result$tasks,
        title = "Tasks",
        columns = c("status", "filename", "bytes_done", "size", "speed_bps", "eta_seconds", "attempts", "last_error", "job_id", "session_id", "task_id", "file_key")
    )
    epwshiftr_cli_render_table(
        result$nodes,
        title = "Data nodes",
        columns = c("data_node", "service", "failure_rate", "failure_count", "success_count", "last_latency", "cooldown_until")
    )
    epwshiftr_cli_render_table(
        result$events,
        title = "Recent events",
        columns = c("created_at", "event", "status", "error", "job_id", "session_id", "task_id", "file_key")
    )
    invisible(NULL)
}


epwshiftr_cli_render_esgf_report <- function(result) {
    cli::cli_h1("ESGF report")
    epwshiftr_cli_render_table(
        result$summary,
        title = "Summary",
        columns = c(
            "query_id", "label", "tracked", "file_total", "current_count", "local_count",
            "bytes_missing", "download_incomplete", "download_retryable", "last_download_session_id"
        )
    )
    epwshiftr_cli_render_table(
        result$updates,
        title = "Updates",
        columns = c("query_id", "new_count", "changed_count", "stale_count", "file_total", "created_at", "update_id")
    )
    epwshiftr_cli_render_table(
        result$changes,
        title = "Changes",
        columns = c("change_type", "filename", "size", "version", "data_node", "file_key", "query_id")
    )
    epwshiftr_cli_render_table(
        result$downloads,
        title = "Downloads",
        columns = c("status", "filename", "bytes_done", "size", "last_error", "session_id", "task_id", "file_key")
    )
    epwshiftr_cli_render_table(
        result$nodes,
        title = "Data nodes",
        columns = c("data_node", "service", "failure_rate", "failure_count", "success_count", "last_latency", "cooldown_until")
    )
    invisible(NULL)
}


epwshiftr_cli_render_storage <- function(result, command, action = NULL) {
    if (identical(command, "report")) {
        if (is.list(result) && !is.data.frame(result)) {
            cli::cli_h1("Storage report")
            epwshiftr_cli_render_summary(result$summary, "Summary")
            epwshiftr_cli_render_table(result$downloads, "Download files", c("relative_path", "size", "mtime"))
            epwshiftr_cli_render_table(result$registered, "Registered files", c("source", "file_key", "relative_path", "size"))
            epwshiftr_cli_render_table(result$untracked_files, "Untracked files", c("relative_path", "size", "mtime"))
            epwshiftr_cli_render_table(result$missing_records, "Missing records", c("source", "file_key", "relative_path", "size"))
            epwshiftr_cli_render_table(result$tmp, "Temporary files", c("relative_path", "size", "mtime"))
            epwshiftr_cli_render_table(result$orphan_records, "Orphan records", c("file_key", "filename", "size", "local_path"))
            return(invisible(NULL))
        }
        return(epwshiftr_cli_render_summary(result, "Storage report"))
    }
    if (identical(command, "validate")) {
        cli::cli_h1("Storage validation")
        epwshiftr_cli_render_summary(result$summary, "Summary")
        epwshiftr_cli_render_table(result$files, "Files", c("file_key", "filename", "local_path", "exists", "size_ok", "checksum_ok", "layout_ok"))
        epwshiftr_cli_render_table(result$artifacts, "Artifacts", c("artifact_id", "file_key", "relative_path", "exists", "size_ok"))
        epwshiftr_cli_render_table(result$untracked, "Untracked files", c("relative_path", "size", "mtime"))
        epwshiftr_cli_render_table(result$actions, "Actions", c("action", "target", "file_key", "from", "to", "reason"))
        return(invisible(NULL))
    }
    if (identical(command, "layout")) {
        return(epwshiftr_cli_render_summary(result, paste("Storage layout", epwshiftr_cli_string_default(action, ""))))
    }
    title <- switch(
        command,
        repair = "Storage repair",
        cleanup = "Storage cleanup",
        "Storage result"
    )
    epwshiftr_cli_render_default(result, title = title)
}


epwshiftr_cli_render_shift <- function(result, command) {
    if (identical(command, "run")) {
        cli::cli_h1("Shift workflow")
        epwshiftr_cli_render_summary(result[intersect(c("status", "query_id", "morph_id", "diagnostic_count"), names(result))], "Summary")
        epwshiftr_cli_render_table(result$coverage, "Coverage", c("plan_count", "complete", "failed", "output_rows"))
        epwshiftr_cli_render_table(result$outputs, "Outputs", c("path", "case_id", "source_id", "experiment_id", "variant_label", "period", "morph_id"))
        epwshiftr_cli_render_table(result$next_steps, "Next steps", c("step", "command"), show_types = FALSE)
        return(invisible(NULL))
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
    title <- switch(
        command,
        status = "Shift status",
        diagnostics = "Shift diagnostics",
        outputs = "Shift outputs",
        data = "Shift data",
        "Shift result"
    )
    epwshiftr_cli_render_default(result, title = title)
}


epwshiftr_cli_render_shift_show <- function(result) {
    cli::cli_h1("Shift workflow graph")
    epwshiftr_cli_render_summary(result$summary, "Summary")
    epwshiftr_cli_render_shift_tree(result)
    epwshiftr_cli_render_table(
        result$queries,
        "Queries",
        c("query_id", "label", "tracked", "file_current", "file_total", "download_incomplete", "download_retryable"),
        show_types = FALSE
    )
    epwshiftr_cli_render_table(
        result$plans,
        "Extraction plans",
        c("plan_id", "complete", "status", "query_id", "site_id", "variable_id", "output_rows", "last_error"),
        show_types = FALSE
    )
    epwshiftr_cli_render_table(
        result$morphs,
        "Morph plans",
        c("morph_id", "status", "label", "summary_id", "baseline_id", "strict", "last_error"),
        show_types = FALSE
    )
    if (isTRUE(result$include_outputs) || nrow(result$outputs)) {
        epwshiftr_cli_render_table(
            result$outputs,
            "Outputs",
            c("path", "case_id", "source_id", "experiment_id", "variant_label", "period", "morph_id"),
            show_types = FALSE
        )
    }
    if (isTRUE(result$include_files)) {
        epwshiftr_cli_render_table(
            result$files,
            "Files",
            c("query_id", "file_key", "source_id", "experiment_id", "variant_label", "variable_id", "local_path"),
            show_types = FALSE
        )
    }
    invisible(NULL)
}


epwshiftr_cli_render_shift_tree <- function(result) {
    queries <- data.table::as.data.table(result$queries)
    plans <- data.table::as.data.table(result$plans)
    morphs <- data.table::as.data.table(result$morphs)
    links <- data.table::as.data.table(result$links)
    outputs <- data.table::as.data.table(result$outputs)
    if (!nrow(queries) && !nrow(plans) && !nrow(morphs)) {
        return(invisible(NULL))
    }
    cli::cli_h2("Graph")
    query_ids <- unique(c(
        if (nrow(queries) && "query_id" %in% names(queries)) queries$query_id else character(),
        if (nrow(plans) && "query_id" %in% names(plans)) plans$query_id else character()
    ))
    query_ids <- query_ids[!is.na(query_ids) & nzchar(query_ids)]
    for (query_id in query_ids) {
        cli::cli_text("query {query_id}")
        plan_rows <- if (nrow(plans) && "query_id" %in% names(plans)) plans[plans$query_id == query_id] else plans[0]
        for (i in seq_len(nrow(plan_rows))) {
            plan_id <- plan_rows$plan_id[[i]]
            plan_status <- epwshiftr_cli_string_default(plan_rows$status[[i]], "unknown")
            cli::cli_text("  -> plan {plan_id} [{plan_status}]")
            summary_ids <- if (nrow(links)) links$summary_id[links$plan_id == plan_id] else character()
            morph_rows <- if (nrow(morphs) && length(summary_ids)) morphs[morphs$summary_id %in% summary_ids] else morphs[0]
            for (j in seq_len(nrow(morph_rows))) {
                morph_id <- morph_rows$morph_id[[j]]
                morph_status <- epwshiftr_cli_string_default(morph_rows$status[[j]], "unknown")
                cli::cli_text("     -> morph {morph_id} [{morph_status}]")
                output_rows <- if (nrow(outputs)) outputs[outputs$morph_id == morph_id] else outputs[0]
                if (nrow(output_rows)) {
                    cli::cli_text("        -> outputs {nrow(output_rows)}")
                }
            }
        }
    }
    invisible(NULL)
}


epwshiftr_cli_render_shift_config <- function(result) {
    if (identical(result$action, "example")) {
        cli::cli_h1("Shift config example")
        epwshiftr_cli_render_summary(result[intersect(c("status", "output"), names(result))], "Summary")
        if (is.na(result$output[[1L]])) {
            json <- jsonlite::toJSON(result$config, auto_unbox = TRUE, pretty = TRUE, null = "null")
            for (line in strsplit(json, "\n", fixed = TRUE)[[1L]]) {
                cli::cli_text("{line}")
            }
        }
        return(invisible(NULL))
    }
    if (identical(result$action, "validate")) {
        cli::cli_h1("Shift config validation")
        epwshiftr_cli_render_summary(result[intersect(c("status", "config"), names(result))], "Summary")
        epwshiftr_cli_render_table(result$request, "Request")
        epwshiftr_cli_render_table(result$site, "Site")
        epwshiftr_cli_render_table(result$periods, "Periods")
        return(invisible(NULL))
    }
    epwshiftr_cli_render_default(result, title = "Shift config")
}


epwshiftr_cli_render_shift_watch <- function(result) {
    cli::cli_h1("Shift activity")
    epwshiftr_cli_render_summary(result$summary, "Summary")
    epwshiftr_cli_render_table(
        result$downloads,
        "Downloads",
        c("status", "filename", "bytes_done", "size", "speed_bps", "eta_seconds", "attempts", "last_error", "session_id", "task_id", "file_key"),
        show_types = FALSE
    )
    epwshiftr_cli_render_table(
        result$plans,
        "Extraction plans",
        c("plan_id", "complete", "status", "query_id", "site_id", "variable_id", "output_rows", "last_error"),
        show_types = FALSE
    )
    epwshiftr_cli_render_table(
        result$morphs,
        "Morph plans",
        c("morph_id", "status", "label", "summary_id", "baseline_id", "strict", "last_error"),
        show_types = FALSE
    )
    epwshiftr_cli_render_table(
        result$outputs,
        "Outputs",
        c("path", "case_id", "source_id", "experiment_id", "variant_label", "period", "morph_id"),
        show_types = FALSE
    )
    epwshiftr_cli_render_table(
        result$diagnostics,
        "Diagnostics",
        c("stage", "severity", "code", "message", "query_id", "plan_id", "morph_id", "action"),
        show_types = FALSE
    )
    epwshiftr_cli_render_table(
        result$events,
        "Recent events",
        c("created_at", "event", "status", "error", "job_id", "session_id", "task_id", "file_key"),
        show_types = FALSE
    )
    invisible(NULL)
}


epwshiftr_cli_render_extract <- function(result, command) {
    title <- switch(
        command,
        plan = "Extraction plan",
        run = "Extraction run",
        retry = "Extraction retry",
        coverage = "Extraction coverage",
        artifacts = "Extraction artifacts",
        "Extraction result"
    )
    columns <- switch(
        command,
        plan = c("plan_id", "status", "query_id", "file_key", "site_id", "variable_id", "time_start", "time_stop"),
        run = c("plan_id", "status", "query_id", "site_id", "variable_id", "attempt_count", "last_error", "updated_at"),
        retry = c("plan_id", "status", "query_id", "site_id", "variable_id", "attempt_count", "last_error", "dry_run"),
        coverage = c("plan_id", "complete", "status", "query_id", "site_id", "variable_id", "output_rows", "output_time_count", "last_error"),
        artifacts = c("artifact_id", "kind", "role", "relative_path", "path", "created_at"),
        NULL
    )
    epwshiftr_cli_render_table(result, title = title, columns = columns)
}


epwshiftr_cli_render_morph <- function(result, command) {
    title <- switch(
        command,
        variables = "Morph variables",
        backends = "Morph backends",
        run = "Morph run",
        epw = "Morph EPW outputs",
        retry = "Morph retry",
        status = "Morph status",
        outputs = "Morph outputs",
        "Morph result"
    )
    if (identical(command, "run") && is.list(result) && !is.data.frame(result)) {
        cli::cli_h1(title)
        epwshiftr_cli_render_summary(result[intersect(c("status", "morph_id", "diagnostic_count"), names(result))], "Summary")
        epwshiftr_cli_render_table(result$plan, "Plan", c("morph_id", "status", "summary_id", "reference_summary_id", "baseline_id", "strict", "last_error"))
        epwshiftr_cli_render_table(result$results, "Results", c("case_id", "row_count", "output_path", "morph_id"))
        return(invisible(NULL))
    }
    columns <- switch(
        command,
        variables = c("variable_id"),
        backends = c("backend", "label", "required_variables", "methods"),
        epw = c("path", "case_id", "source_id", "experiment_id", "variant_label", "period", "morph_id"),
        retry = c("morph_id", "status", "label", "summary_id", "reference_summary_id", "baseline_id", "strict", "last_error", "dry_run", "case_id", "row_count", "output_path"),
        status = c("morph_id", "status", "label", "strict", "summary_id", "reference_summary_id", "baseline_id", "updated_at", "last_error"),
        outputs = c("path", "case_id", "source_id", "experiment_id", "variant_label", "period", "morph_id"),
        NULL
    )
    epwshiftr_cli_render_table(result, title = title, columns = columns)
}


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


epwshiftr_cli_render_named_list <- function(result, title = NULL) {
    if (!is.null(title) && nzchar(trimws(title))) {
        cli::cli_h1(trimws(title))
    }
    if (!length(result)) {
        cli::cli_alert_info("No values.")
        return(invisible(NULL))
    }
    nms <- names(result)
    if (is.null(nms)) {
        nms <- rep("", length(result))
    }
    scalar <- vapply(result, epwshiftr_cli_is_scalar_value, logical(1L))
    if (any(scalar)) {
        epwshiftr_cli_render_summary(result[scalar], "Values")
    }
    for (i in which(!scalar)) {
        name <- nms[[i]]
        subtitle <- if (nzchar(name)) epwshiftr_cli_title(name) else NULL
        epwshiftr_cli_render_default(result[[i]], title = subtitle)
    }
    invisible(NULL)
}


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
        value <- paste(epwshiftr_cli_format_named_cell(x[[name]], name), collapse = ", ")
        cli::cli_text("{label}: {value}")
    }
    invisible(NULL)
}


epwshiftr_cli_render_table <- function(x, title = NULL, columns = NULL, max_rows = 20L, show_types = TRUE) {
    x <- epwshiftr_cli_as_data_frame(x)
    augmented <- epwshiftr_cli_add_progress_column(x, columns)
    x <- augmented$x
    columns <- augmented$columns
    if (!is.null(title) && nzchar(title)) {
        cli::cli_h2(title)
    }
    if (!nrow(x)) {
        cli::cli_alert_info("No rows.")
        return(invisible(NULL))
    }
    columns <- epwshiftr_cli_pick_columns(x, columns)
    if (!length(columns)) {
        cli::cli_alert_info("No printable columns.")
        return(invisible(NULL))
    }
    shown <- x[seq_len(min(nrow(x), max_rows)), columns, drop = FALSE]
    display <- as.data.frame(lapply(names(shown), function(name) {
        value <- epwshiftr_cli_format_named_cell(shown[[name]], name)
        epwshiftr_cli_truncate_cell(value, epwshiftr_cli_column_max_width(name))
    }), stringsAsFactors = FALSE)
    names(display) <- names(shown)

    adapted <- epwshiftr_cli_adapt_table_columns(
        display = display,
        raw = shown,
        max_width = epwshiftr_cli_console_width(),
        show_types = show_types
    )
    display <- adapted$display
    shown <- adapted$raw

    lines <- epwshiftr_cli_table_lines(
        display,
        header = epwshiftr_cli_title(names(display)),
        types = if (isTRUE(show_types)) epwshiftr_cli_table_types(shown) else NULL,
        align = epwshiftr_cli_table_alignments(shown),
        row_style = epwshiftr_cli_table_row_styles(shown)
    )
    cli::cli_verbatim(lines)
    extra <- nrow(x) - nrow(shown)
    if (extra > 0L) {
        cli::cli_alert_info("{extra} more rows; use --json for full output.")
    }
    if (length(adapted$dropped)) {
        hidden <- paste(adapted$dropped, collapse = ", ")
        cli::cli_alert_info("Hidden columns for console width: {hidden}. Use --json for full output.")
    }
    invisible(NULL)
}


epwshiftr_cli_as_data_frame <- function(x) {
    if (is.null(x)) {
        return(data.frame())
    }
    if (data.table::is.data.table(x)) {
        return(as.data.frame(x))
    }
    if (is.data.frame(x)) {
        return(as.data.frame(x))
    }
    if (is.list(x) && all(vapply(x, epwshiftr_cli_is_scalar_value, logical(1L)))) {
        return(as.data.frame(x, stringsAsFactors = FALSE))
    }
    data.frame(value = epwshiftr_cli_format_cell(x), stringsAsFactors = FALSE)
}


epwshiftr_cli_pick_columns <- function(x, columns = NULL) {
    available <- names(x)
    if (is.null(columns)) {
        return(available)
    }
    picked <- intersect(columns, available)
    if (length(picked)) {
        return(picked)
    }
    available
}


epwshiftr_cli_column_max_width <- function(name) {
    name <- tolower(name)
    if (grepl("url|path|detail|message|error|hint", name)) {
        return(64L)
    }
    if (grepl("(^|_)id$|_id$|key", name)) {
        return(28L)
    }
    if (grepl("filename|label|title", name)) {
        return(40L)
    }
    24L
}


epwshiftr_cli_table_lines <- function(x, header = names(x), types = NULL, align = NULL, border = "single", row_style = NULL) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    header <- as.character(header)
    if (!ncol(x)) {
        return(character())
    }
    if (!is.null(types)) {
        types <- rep_len(as.character(types), ncol(x))
    }
    if (is.null(align)) {
        align <- rep("left", ncol(x))
    }
    align <- rep_len(align, ncol(x))
    if (is.null(row_style)) {
        row_style <- rep("none", nrow(x))
    }
    row_style <- rep_len(row_style, nrow(x))
    chars <- epwshiftr_cli_table_border(border)
    body <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
    header <- rep_len(header, ncol(body))
    widths <- vapply(seq_along(body), function(i) {
        label <- c(header[[i]], if (!is.null(types)) types[[i]], body[[i]])
        max(cli::ansi_nchar(label, type = "width"), na.rm = TRUE)
    }, integer(1L))

    lines <- c(
        epwshiftr_cli_table_rule(widths, chars, "top"),
        epwshiftr_cli_table_row(cli::style_bold(header), widths, rep("left", length(widths)), chars)
    )
    if (!is.null(types)) {
        lines <- c(lines, epwshiftr_cli_table_row(cli::col_grey(types), widths, rep("left", length(widths)), chars))
    }
    lines <- c(lines, epwshiftr_cli_table_rule(widths, chars, "mid"))
    for (i in seq_len(nrow(body))) {
        row <- vapply(body, `[[`, character(1L), i)
        line <- epwshiftr_cli_table_row(row, widths, align, chars)
        lines <- c(lines, epwshiftr_cli_style_table_row(line, row_style[[i]]))
    }
    c(lines, epwshiftr_cli_table_rule(widths, chars, "bottom"))
}


epwshiftr_cli_adapt_table_columns <- function(display, raw, max_width = epwshiftr_cli_console_width(), min_columns = 2L, show_types = TRUE) {
    if (!ncol(display)) {
        return(list(display = display, raw = raw, dropped = character()))
    }
    max_width <- as.integer(max_width[[1L]])
    if (is.na(max_width) || max_width <= 0L) {
        max_width <- 80L
    }
    min_columns <- max(1L, min(as.integer(min_columns[[1L]]), ncol(display)))
    dropped <- character()
    while (ncol(display) > min_columns && epwshiftr_cli_table_width(display, raw, show_types = show_types) > max_width) {
        drop <- names(display)[[ncol(display)]]
        dropped <- c(drop, dropped)
        display <- display[-ncol(display)]
        raw <- raw[-ncol(raw)]
    }
    list(display = display, raw = raw, dropped = dropped)
}


epwshiftr_cli_table_width <- function(display, raw, show_types = TRUE) {
    lines <- epwshiftr_cli_table_lines(
        display,
        header = epwshiftr_cli_title(names(display)),
        types = if (isTRUE(show_types)) epwshiftr_cli_table_types(raw) else NULL,
        align = epwshiftr_cli_table_alignments(raw)
    )
    if (!length(lines)) {
        return(0L)
    }
    max(cli::ansi_nchar(lines, type = "width"), na.rm = TRUE)
}


epwshiftr_cli_table_types <- function(x) {
    if (!length(x)) {
        return(character())
    }
    class_abbr <- c(
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
    vapply(x, function(col) {
        class <- class(col)[[1L]]
        label <- unname(class_abbr[[class]])
        if (is.null(label) || is.na(label)) {
            label <- paste0("<", class, ">")
        }
        label
    }, character(1L), USE.NAMES = FALSE)
}


epwshiftr_cli_add_progress_column <- function(x, columns = NULL) {
    if (!all(c("bytes_done", "size") %in% names(x)) || "progress" %in% names(x)) {
        return(list(x = x, columns = columns))
    }
    x$progress <- epwshiftr_cli_progress_bar(x$bytes_done, x$size)
    if (!is.null(columns) && "bytes_done" %in% columns && "size" %in% columns && !"progress" %in% columns) {
        pos <- match("size", columns)
        columns <- append(columns, "progress", after = pos)
    }
    list(x = x, columns = columns)
}


epwshiftr_cli_progress_bar <- function(done, total, width = 8L) {
    done <- suppressWarnings(as.numeric(done))
    total <- suppressWarnings(as.numeric(total))
    out <- rep("-", length(done))
    ok <- !is.na(done) & !is.na(total) & total > 0
    if (!any(ok)) {
        return(out)
    }
    ratio <- pmax(0, pmin(1, done[ok] / total[ok]))
    filled <- pmin(width, pmax(0L, round(ratio * width)))
    out[ok] <- vapply(seq_along(ratio), function(i) {
        paste0("[", strrep("#", filled[[i]]), strrep("-", width - filled[[i]]), "] ", sprintf("%3.0f%%", ratio[[i]] * 100))
    }, character(1L))
    out
}


epwshiftr_cli_console_width <- function() {
    width <- getOption("width", 80L)
    width <- suppressWarnings(as.integer(width[[1L]]))
    if (is.na(width) || width < 40L) {
        return(80L)
    }
    width
}


epwshiftr_cli_table_border <- function(border = "single") {
    border <- match.arg(border, c("single", "ascii", "none"))
    if (identical(border, "single") && !isTRUE(cli::is_utf8_output())) {
        border <- "ascii"
    }
    switch(
        border,
        single = list(
            top = c(left = "\u250c", mid = "\u252c", right = "\u2510"),
            middle = c(left = "\u251c", mid = "\u253c", right = "\u2524"),
            bottom = c(left = "\u2514", mid = "\u2534", right = "\u2518"),
            horizontal = "\u2500",
            vertical = "\u2502",
            margin = 1L
        ),
        ascii = list(
            top = c(left = "+", mid = "+", right = "+"),
            middle = c(left = "+", mid = "+", right = "+"),
            bottom = c(left = "+", mid = "+", right = "+"),
            horizontal = "-",
            vertical = "|",
            margin = 1L
        ),
        none = list(
            top = c(left = "", mid = "", right = ""),
            middle = c(left = "", mid = "", right = ""),
            bottom = c(left = "", mid = "", right = ""),
            horizontal = "",
            vertical = "  ",
            margin = 0L
        )
    )
}


epwshiftr_cli_table_rule <- function(widths, chars, position = c("top", "mid", "bottom")) {
    position <- match.arg(position)
    pieces <- switch(position, top = chars$top, mid = chars$middle, bottom = chars$bottom)
    cell_widths <- widths + chars$margin * 2L
    cells <- vapply(cell_widths, function(width) paste(rep(chars$horizontal, width), collapse = ""), character(1L))
    paste0(pieces[["left"]], paste(cells, collapse = pieces[["mid"]]), pieces[["right"]])
}


epwshiftr_cli_table_row <- function(values, widths, align, chars) {
    values <- as.character(values)
    cells <- vapply(seq_along(values), function(i) {
        padded <- epwshiftr_cli_table_pad(values[[i]], widths[[i]], align[[i]])
        paste0(
            paste(rep(" ", chars$margin), collapse = ""),
            padded,
            paste(rep(" ", chars$margin), collapse = "")
        )
    }, character(1L))
    paste0(chars$vertical, paste(cells, collapse = chars$vertical), chars$vertical)
}


epwshiftr_cli_table_pad <- function(value, width, align = "left") {
    value <- as.character(value[[1L]])
    visible <- cli::ansi_nchar(value, type = "width")
    pad <- max(0L, as.integer(width) - as.integer(visible))
    left <- switch(
        align,
        right = pad,
        center = floor(pad / 2L),
        0L
    )
    right <- pad - left
    paste0(strrep(" ", left), value, strrep(" ", right))
}


epwshiftr_cli_table_alignments <- function(x) {
    vapply(names(x), function(name) {
        value <- x[[name]]
        if (is.numeric(value) || is.integer(value)) {
            return("right")
        }
        if (grepl("bytes|size|count|attempt|priority|latency|rate|total|done|missing|queued|error|cancelled|skipped", name, ignore.case = TRUE)) {
            return("right")
        }
        "left"
    }, character(1L))
}


epwshiftr_cli_table_row_styles <- function(x) {
    if (!nrow(x)) {
        return(character())
    }
    value <- epwshiftr_cli_table_status_values(x)
    vapply(value, function(status) {
        group <- epwshiftr_cli_status_group(status)
        switch(
            group,
            danger = "danger",
            warning = "warning",
            "none"
        )
    }, character(1L), USE.NAMES = FALSE)
}


epwshiftr_cli_table_status_values <- function(x) {
    for (name in c("status", "change_type", "check_status")) {
        if (name %in% names(x)) {
            return(as.character(x[[name]]))
        }
    }
    rep(NA_character_, nrow(x))
}


epwshiftr_cli_style_table_row <- function(line, style = "none") {
    switch(
        style,
        danger = cli::col_red(line),
        warning = cli::col_yellow(line),
        info = cli::col_cyan(line),
        success = cli::col_green(line),
        muted = cli::col_grey(line),
        line
    )
}


epwshiftr_cli_truncate_cell <- function(x, width) {
    x <- as.character(x)
    width <- as.integer(width[[1L]])
    if (is.na(width) || width < 4L) {
        return(x)
    }
    vapply(x, function(value) {
        if (is.na(value) || nchar(value, type = "width") <= width) {
            return(value)
        }
        paste0(substr(value, 1L, width - 3L), "...")
    }, character(1L))
}


epwshiftr_cli_format_named_cell <- function(x, name) {
    if (epwshiftr_cli_is_status_column(name)) {
        return(epwshiftr_cli_color_status(epwshiftr_cli_format_cell(x)))
    }
    if (is.logical(x) || epwshiftr_cli_is_boolean_indicator(name)) {
        return(epwshiftr_cli_color_boolean(epwshiftr_cli_format_cell(x), name))
    }
    if (grepl("bytes|size", name, ignore.case = TRUE)) {
        return(epwshiftr_cli_format_bytes(x))
    }
    if (grepl("(^|_)at$|time|date|until", name, ignore.case = TRUE) && inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
        return(epwshiftr_cli_format_time(x))
    }
    epwshiftr_cli_format_cell(x)
}


epwshiftr_cli_is_status_column <- function(name) {
    grepl("(^status$|change_type|check_status|event$)", name, ignore.case = TRUE)
}


epwshiftr_cli_is_boolean_indicator <- function(name) {
    grepl("(^|_)(ok|exists|tracked|cached|resume|run|execute|dry_run|incomplete|retryable|would_block)$", name, ignore.case = TRUE)
}


epwshiftr_cli_color_status <- function(x) {
    vapply(x, function(value) {
        group <- epwshiftr_cli_status_group(value)
        switch(
            group,
            danger = cli::col_red(value),
            warning = cli::col_yellow(value),
            success = cli::col_green(value),
            info = cli::col_cyan(value),
            muted = cli::col_grey(value),
            value
        )
    }, character(1L), USE.NAMES = FALSE)
}


epwshiftr_cli_status_group <- function(x) {
    x <- tolower(as.character(x[[1L]]))
    if (is.na(x) || !nzchar(x) || identical(x, "-")) {
        return("none")
    }
    if (x %in% c("error", "failed", "failure", "cancelled", "missing", "retracted", "invalid", "bad", "blocked")) {
        return("danger")
    }
    if (x %in% c("warning", "stale", "changed", "deprecated", "cooldown", "retry", "retryable", "interrupted")) {
        return("warning")
    }
    if (x %in% c("ok", "done", "current", "success", "verified", "complete", "completed")) {
        return("success")
    }
    if (x %in% c("queued", "downloading", "running", "enqueue", "probe", "new", "skipped")) {
        return("info")
    }
    if (x %in% c("none", "unknown", "na", "-")) {
        return("muted")
    }
    "none"
}


epwshiftr_cli_color_boolean <- function(x, name) {
    value <- tolower(as.character(x))
    positive_is_bad <- grepl("incomplete|retryable|would_block|dry_run", name, ignore.case = TRUE)
    vapply(value, function(item) {
        if (item %in% c("yes", "true", "1")) {
            return(if (positive_is_bad) cli::col_yellow(item) else cli::col_green(item))
        }
        if (item %in% c("no", "false", "0")) {
            return(if (positive_is_bad) cli::col_green(item) else cli::col_red(item))
        }
        cli::col_grey(item)
    }, character(1L), USE.NAMES = FALSE)
}


epwshiftr_cli_format_cell <- function(x) {
    if (is.null(x)) {
        return("-")
    }
    if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
        return(epwshiftr_cli_format_time(x))
    }
    if (is.list(x) && !is.data.frame(x)) {
        return(vapply(x, function(value) paste(epwshiftr_cli_format_cell(value), collapse = ","), character(1L)))
    }
    if (is.logical(x)) {
        out <- ifelse(is.na(x), "-", ifelse(x, "yes", "no"))
        return(as.character(out))
    }
    if (length(x) == 0L) {
        return("-")
    }
    out <- as.character(x)
    out[is.na(out) | !nzchar(out)] <- "-"
    out
}


epwshiftr_cli_format_time <- function(x) {
    out <- as.character(x)
    ok <- !is.na(x)
    if (inherits(x, c("POSIXct", "POSIXlt"))) {
        out[ok] <- format(as.POSIXct(x[ok], tz = "UTC"), "%Y-%m-%d %H:%M:%S %Z")
    }
    out[!ok | !nzchar(out)] <- "-"
    out
}


epwshiftr_cli_format_bytes <- function(x) {
    if (is.null(x) || !length(x)) {
        return("-")
    }
    bytes <- suppressWarnings(as.numeric(x))
    out <- rep("-", length(bytes))
    ok <- !is.na(bytes)
    if (!any(ok)) {
        return(out)
    }
    units <- c("B", "KB", "MB", "GB", "TB", "PB")
    for (i in which(ok)) {
        value <- bytes[[i]]
        unit <- 1L
        while (abs(value) >= 1024 && unit < length(units)) {
            value <- value / 1024
            unit <- unit + 1L
        }
        out[[i]] <- if (unit == 1L) {
            sprintf("%.0f %s", value, units[[unit]])
        } else {
            sprintf("%.1f %s", value, units[[unit]])
        }
    }
    out
}


epwshiftr_cli_title <- function(x) {
    x <- gsub("_", " ", as.character(x), fixed = TRUE)
    x <- gsub("-", " ", x, fixed = TRUE)
    vapply(strsplit(x, " ", fixed = TRUE), function(parts) {
        parts <- parts[nzchar(parts)]
        if (!length(parts)) {
            return("")
        }
        parts <- paste0(toupper(substring(parts, 1L, 1L)), substring(parts, 2L))
        paste(parts, collapse = " ")
    }, character(1L))
}


epwshiftr_cli_is_scalar_value <- function(x) {
    if (is.null(x)) {
        return(TRUE)
    }
    if (is.data.frame(x)) {
        return(FALSE)
    }
    if (is.list(x)) {
        return(FALSE)
    }
    length(x) <= 1L
}


epwshiftr_cli_string_default <- function(x, y) {
    if (is.null(x) || !length(x) || is.na(x[[1L]]) || !nzchar(as.character(x[[1L]]))) y else x
}
