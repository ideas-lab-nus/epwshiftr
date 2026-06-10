epwshiftr_cli_workflow <- function(store, command, args) {
    if (identical(command, "report")) {
        parsed <- epwshiftr_cli_parse_command(args, options = "--query")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$workflow_report(query_id = parsed$options[["--query"]], downloader = epwshiftr_cli_downloader(store)))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown workflow command: %s", command))
}


epwshiftr_cli_storage <- function(store, command, args) {
    if (identical(command, "layout")) {
        if (!length(args)) {
            epwshiftr_cli_usage_abort("Missing storage layout command: show or set.")
        }
        action <- args[[1L]]
        rest <- args[-1L]
        if (identical(action, "show")) {
            parsed <- epwshiftr_cli_parse_command(rest)
            epwshiftr_cli_assert_no_positionals(parsed)
            return(store$download_layout())
        }
        if (identical(action, "set")) {
            parsed <- epwshiftr_cli_parse_command(
                rest,
                options = c("--layout", "--template", "--include-version", "--collision", "--missing")
            )
            epwshiftr_cli_assert_no_positionals(parsed)
            layout <- parsed$options[["--layout"]]
            if (is.null(layout)) {
                epwshiftr_cli_usage_abort("storage layout set requires --layout.")
            }
            store$set_download_layout(
                layout = layout,
                template = parsed$options[["--template"]],
                include_version = epwshiftr_cli_bool(parsed$options[["--include-version"]], "--include-version", default = TRUE),
                collision = if (is.null(parsed$options[["--collision"]])) "error" else parsed$options[["--collision"]],
                missing = if (is.null(parsed$options[["--missing"]])) "fallback" else parsed$options[["--missing"]]
            )
            return(store$download_layout())
        }
        epwshiftr_cli_usage_abort(sprintf("Unknown storage layout command: %s", action))
    }
    if (identical(command, "report")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--detail")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$storage_report(detail = parsed$flags[["--detail"]]))
    }
    if (identical(command, "validate")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--checksum", options = "--query")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$validate_files(query_id = parsed$options[["--query"]], checksum = parsed$flags[["--checksum"]]))
    }
    if (identical(command, "repair")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--execute")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$repair_files(dry_run = !parsed$flags[["--execute"]]))
    }
    if (identical(command, "cleanup")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--execute", options = c("--scope", "--older-than"))
        epwshiftr_cli_assert_no_positionals(parsed)
        scope <- epwshiftr_cli_csv(parsed$options[["--scope"]])
        if (is.null(scope)) {
            scope <- c("tmp", "orphan_records", "untracked_files", "missing_records")
        }
        older_than <- epwshiftr_cli_count(parsed$options[["--older-than"]], "--older-than", positive = FALSE)
        return(store$cleanup_downloads(scope = scope, dry_run = !parsed$flags[["--execute"]], older_than = older_than))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown storage command: %s", command))
}
