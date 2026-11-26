# epwshiftr_cli_doctor {{{
epwshiftr_cli_doctor <- function(
    store_path = NULL,
    args = character(),
    ui = shift_ui("none")
) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--network", "--no-progress"),
        options = c("--index-node", "--timeout", "--config"),
        no_positionals = TRUE
    )
    if (isTRUE(parsed$flags[["--no-progress"]])) {
        ui <- shift_ui("none")
    }

    timeout <- epwshiftr_cli_count_or_default(
        parsed$options[["--timeout"]],
        "--timeout",
        10L
    )
    index_node <- shift_stage__coalesce(
        parsed$options[["--index-node"]],
        "https://esgf-node.ornl.gov"
    )

    checks <- list()
    add <- function(check, status, message, detail = NA_character_) {
        checks[[length(checks) + 1L]] <<- data.frame(
            check = check,
            status = status,
            message = message,
            detail = epwshiftr_cli_doctor_detail(detail),
            stringsAsFactors = FALSE
        )
    }
    add_dir_writable <- function(
        name,
        path,
        label,
        missing_status = "skipped",
        missing_message = sprintf("%s is missing.", label)
    ) {
        if (dir.exists(path)) {
            writable <- unname(file.access(path, 2L)) == 0L
            add(
                name,
                if (writable) "ok" else "error",
                sprintf(
                    if (writable) "%s is writable." else "%s is not writable.",
                    label
                ),
                path
            )
        } else {
            add(name, missing_status, missing_message, path)
        }
    }
    add_manifest_checks <- function(prefix, manifest, table, label) {
        meta <- epwshiftr_cli_doctor_manifest_meta(manifest, table)
        add(paste0(prefix, "_manifest"), meta$status, meta$message, manifest)
        schema_version <- epwshiftr_cli_doctor_meta_value(
            meta$data,
            "schema_version"
        )
        ok <- identical(meta$status, "ok")
        add(
            paste0(prefix, "_schema"),
            if (ok) "ok" else "skipped",
            if (!is.na(schema_version)) {
                sprintf("%s schema version is %s.", label, schema_version)
            } else if (ok) {
                sprintf("%s schema version is missing.", label)
            } else {
                sprintf("%s manifest is not readable.", label)
            },
            if (!is.na(schema_version)) schema_version else NA_character_
        )
    }

    add("r_version", "ok", paste("R", getRversion()), R.home())
    rscript <- file.path(R.home("bin"), "Rscript")
    has_rscript <- nzchar(rscript) && file.exists(rscript)
    add(
        "rscript",
        if (has_rscript) "ok" else "warning",
        if (has_rscript) {
            "Rscript is available."
        } else {
            "Rscript was not found under R.home()."
        },
        rscript
    )
    version <- tryCatch(
        as.character(utils::packageVersion("epwshiftr")),
        error = function(e) NA_character_
    )
    add(
        "package",
        if (is.na(version)) "warning" else "ok",
        if (is.na(version)) {
            "Could not resolve epwshiftr package version."
        } else {
            paste("epwshiftr", version)
        },
        NA_character_
    )
    launcher <- Sys.which("epwshiftr")
    add(
        "launcher",
        if (nzchar(launcher)) "ok" else "warning",
        if (nzchar(launcher)) {
            "epwshiftr launcher is on PATH."
        } else {
            "epwshiftr launcher was not found on PATH."
        },
        if (nzchar(launcher)) unname(launcher) else NA_character_
    )

    resolved_store <- epwshiftr_cli_doctor_store_path(store_path)
    store_exists <- dir.exists(resolved_store)
    add(
        "store_path",
        if (store_exists) "ok" else "warning",
        if (store_exists) {
            "Store directory exists."
        } else {
            "Store directory does not exist."
        },
        resolved_store
    )
    add_dir_writable("store_writable", resolved_store, "Store directory")
    add_manifest_checks(
        "store",
        file.path(resolved_store, "manifest.duckdb"),
        "store_meta",
        "Store"
    )

    downloader_manifest <- file.path(
        resolved_store,
        "downloads",
        "_downloader",
        "manifest.duckdb"
    )
    config_check <- epwshiftr_cli_doctor_downloader_config(downloader_manifest)
    add(
        "downloader_config",
        config_check$status,
        config_check$message,
        downloader_manifest
    )
    add_manifest_checks(
        "downloader",
        downloader_manifest,
        "download_meta",
        "Downloader"
    )

    add_dir_writable(
        "tmp_downloads",
        file.path(resolved_store, "tmp", "downloads"),
        "Temporary download directory",
        missing_status = "warning",
        missing_message = "Temporary download directory does not exist."
    )

    if (isTRUE(parsed$flags[["--network"]])) {
        network <- shift_reporter__ui_check(
            ui,
            "Network readiness",
            function(reporter) {
                reporter$stage_started("check", paste("Checking", index_node))
                epwshiftr_cli_doctor_network(index_node, timeout)
            }
        )
        add("index_node", network$status, network$message, network$detail)
    } else {
        add(
            "index_node",
            "skipped",
            "Network check skipped. Use --network to enable it.",
            index_node
        )
    }

    # Reuse provider readiness checks without accepting credentials in JSON or
    # printing secrets. Remote authentication is checked only with --network.
    config_path <- parsed$options[["--config"]]
    if (!is.null(config_path)) {
        config <- epwshiftr_cli_read_shift_config(config_path)
        observed <- cli_shift__config_reference(
            shift_stage__coalesce(
                config$calibration,
                config$observed_reference
            ),
            "observed_reference"
        )
        if (S7::S7_inherits(observed, ShiftReanalysisSpec)) {
            diagnostics <- shift_reporter__ui_check(
                ui,
                "Calibration readiness",
                function(reporter) {
                    reporter$stage_started(
                        "check",
                        "Checking CDS credentials and readiness."
                    )
                    shift_check(
                        observed,
                        network = isTRUE(parsed$flags[["--network"]])
                    )
                }
            )
            if (!nrow(diagnostics)) {
                add(
                    "reanalysis",
                    "ok",
                    sprintf(
                        "%s credential checks passed.",
                        toupper(observed@dataset)
                    )
                )
            } else {
                for (index in seq_len(nrow(diagnostics))) {
                    row <- diagnostics[index]
                    add(
                        row$code,
                        if (row$severity == "error") "error" else "warning",
                        row$message,
                        row$action
                    )
                }
            }
        }
    }
    checks <- do.call(rbind, checks)
    rownames(checks) <- NULL
    status <- if (any(checks$status == "error")) {
        "error"
    } else if (any(checks$status == "warning")) {
        "warning"
    } else {
        "ok"
    }
    summary <- data.frame(
        status = status,
        ok = sum(checks$status == "ok"),
        warning = sum(checks$status == "warning"),
        error = sum(checks$status == "error"),
        skipped = sum(checks$status == "skipped"),
        store = resolved_store,
        stringsAsFactors = FALSE
    )
    list(summary = summary, checks = checks)
}
# }}}

# epwshiftr_cli_doctor_store_path {{{
epwshiftr_cli_doctor_store_path <- function(path = NULL) {
    if (is.null(path)) {
        return(store_dir(init = FALSE))
    }
    normalizePath(path.expand(path), mustWork = FALSE, winslash = "/")
}
# }}}

# epwshiftr_cli_doctor_detail {{{
epwshiftr_cli_doctor_detail <- function(detail) {
    if (is.null(detail) || !length(detail)) {
        return(NA_character_)
    }
    detail <- as.character(detail[[1L]])
    if (!nzchar(detail)) NA_character_ else detail
}
# }}}

# epwshiftr_cli_doctor_manifest_meta {{{
epwshiftr_cli_doctor_manifest_meta <- function(path, table) {
    if (!file.exists(path)) {
        return(list(
            status = "warning",
            message = "Manifest file does not exist.",
            data = data.frame()
        ))
    }
    conn <- NULL
    tryCatch(
        {
            conn <- ddb_connect(path, read_only = TRUE)
            tables <- ddb_list_tables(conn)
            if (!table %in% tables) {
                return(list(
                    status = "warning",
                    message = sprintf(
                        "Manifest table %s does not exist.",
                        table
                    ),
                    data = data.frame()
                ))
            }
            data <- ddb_query(
                conn,
                sprintf("SELECT key, value FROM %s", ddb_ident(conn, table))
            )
            list(
                status = "ok",
                message = "Manifest is readable.",
                data = as.data.frame(data, stringsAsFactors = FALSE)
            )
        },
        error = function(e) {
            list(
                status = "error",
                message = conditionMessage(e),
                data = data.frame()
            )
        },
        finally = {
            if (!is.null(conn)) {
                try(ddb_disconnect(conn, shutdown = TRUE), silent = TRUE)
            }
        }
    )
}
# }}}

# epwshiftr_cli_doctor_meta_value {{{
epwshiftr_cli_doctor_meta_value <- function(meta, key) {
    if (
        !is.data.frame(meta) ||
            !nrow(meta) ||
            !"key" %in% names(meta) ||
            !"value" %in% names(meta)
    ) {
        return(NA_character_)
    }
    row <- meta[meta$key == key, , drop = FALSE]
    if (!nrow(row)) {
        return(NA_character_)
    }
    value <- as.character(row$value[[1L]])
    if (is.na(value) || !nzchar(value)) NA_character_ else value
}
# }}}

# epwshiftr_cli_doctor_downloader_config {{{
epwshiftr_cli_doctor_downloader_config <- function(path) {
    if (!file.exists(path)) {
        return(list(
            status = "warning",
            message = "Downloader manifest file does not exist."
        ))
    }
    conn <- NULL
    tryCatch(
        {
            conn <- ddb_connect(path, read_only = TRUE)
            tables <- ddb_list_tables(conn)
            if (!"download_config" %in% tables) {
                return(list(
                    status = "warning",
                    message = "Downloader config table does not exist."
                ))
            }
            rows <- ddb_query(
                conn,
                "SELECT * FROM download_config WHERE config_id = 'default'"
            )
            rows <- as.data.frame(rows, stringsAsFactors = FALSE)
            if (!nrow(rows)) {
                return(list(
                    status = "warning",
                    message = "Downloader config row is missing."
                ))
            }
            downloader__config_unflatten(rows, manifest = path)
            list(status = "ok", message = "Downloader config is readable.")
        },
        error = function(e) {
            list(status = "error", message = conditionMessage(e))
        },
        finally = {
            if (!is.null(conn)) {
                try(ddb_disconnect(conn, shutdown = TRUE), silent = TRUE)
            }
        }
    )
}
# }}}

# epwshiftr_cli_doctor_network {{{
epwshiftr_cli_doctor_network <- function(index_node, timeout = 10L) {
    tryCatch(
        {
            query <- esg_query(index_node)$limit(0L)
            handle <- curl::new_handle(
                timeout = timeout,
                connecttimeout = min(timeout, 10L)
            )
            response <- curl::curl_fetch_memory(query$url(), handle = handle)
            code <- response$status_code
            list(
                status = if (code >= 200L && code < 400L) "ok" else "error",
                message = sprintf("Index node responded with HTTP %s.", code),
                detail = query$url()
            )
        },
        error = function(e) {
            list(
                status = "error",
                message = conditionMessage(e),
                detail = index_node
            )
        }
    )
}
# }}}

# vim: fdm=marker :
