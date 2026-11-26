# epwshiftr_cli_esgf {{{
epwshiftr_cli_esgf <- function(store, command, args) {
    if (identical(command, "report")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            options = "--query",
            no_positionals = TRUE
        )
        return(store$workflow_report(
            query_id = parsed$options[["--query"]],
            downloader = epwshiftr_cli_downloader(store)
        ))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown esgf command: %s", command))
}
# }}}

# epwshiftr_cli_storage {{{
epwshiftr_cli_storage <- function(store, command, args) {
    if (identical(command, "layout")) {
        if (!length(args)) {
            epwshiftr_cli_usage_abort(
                "Missing storage layout command: show or set."
            )
        }
        action <- args[[1L]]
        rest <- args[-1L]
        if (identical(action, "show")) {
            epwshiftr_cli_parse_command(rest, no_positionals = TRUE)
            return(store$download_layout())
        }
        if (identical(action, "set")) {
            parsed <- epwshiftr_cli_parse_command(
                rest,
                options = c(
                    "--layout",
                    "--template",
                    "--include-version",
                    "--collision",
                    "--missing"
                ),
                no_positionals = TRUE
            )
            layout <- parsed$options[["--layout"]]
            if (is.null(layout)) {
                epwshiftr_cli_usage_abort(
                    "storage layout set requires --layout."
                )
            }
            store$set_download_layout(
                layout = layout,
                template = parsed$options[["--template"]],
                include_version = epwshiftr_cli_bool(
                    parsed$options[["--include-version"]],
                    "--include-version",
                    default = TRUE
                ),
                collision = shift_stage__coalesce(
                    parsed$options[["--collision"]],
                    "error"
                ),
                missing = shift_stage__coalesce(
                    parsed$options[["--missing"]],
                    "fallback"
                )
            )
            return(store$download_layout())
        }
        epwshiftr_cli_usage_abort(sprintf(
            "Unknown storage layout command: %s",
            action
        ))
    }
    if (identical(command, "report")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = "--detail",
            no_positionals = TRUE
        )
        return(store$storage_report(detail = parsed$flags[["--detail"]]))
    }
    if (identical(command, "validate")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = "--checksum",
            options = "--query",
            no_positionals = TRUE
        )
        return(store$validate_files(
            query_id = parsed$options[["--query"]],
            checksum = parsed$flags[["--checksum"]]
        ))
    }
    if (identical(command, "repair")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = "--execute",
            no_positionals = TRUE
        )
        return(store$repair_files(dry_run = !parsed$flags[["--execute"]]))
    }
    if (identical(command, "cleanup")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = "--execute",
            options = c("--scope", "--older-than"),
            no_positionals = TRUE
        )
        scope <- shift_stage__coalesce(
            epwshiftr_cli_csv(parsed$options[["--scope"]]),
            c("tmp", "orphan_records", "untracked_files", "missing_records")
        )
        older_than <- epwshiftr_cli_count(
            parsed$options[["--older-than"]],
            "--older-than",
            positive = FALSE
        )
        return(store$cleanup_downloads(
            scope = scope,
            dry_run = !parsed$flags[["--execute"]],
            older_than = older_than
        ))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown storage command: %s", command))
}
# }}}

# vim: fdm=marker :
