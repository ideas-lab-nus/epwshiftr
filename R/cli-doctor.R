epwshiftr_cli_doctor <- function(store_path = NULL, args = character()) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = "--network",
        options = c("--index-node", "--timeout")
    )
    epwshiftr_cli_assert_no_positionals(parsed)

    timeout <- if (is.null(parsed$options[["--timeout"]])) {
        10L
    } else {
        epwshiftr_cli_count(parsed$options[["--timeout"]], "--timeout")
    }
    index_node <- parsed$options[["--index-node"]]
    if (is.null(index_node)) {
        index_node <- "https://esgf-node.ornl.gov"
    }

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

    add(
        "r_version",
        "ok",
        paste("R", getRversion()),
        R.home()
    )
    add(
        "rscript",
        if (nzchar(file.path(R.home("bin"), "Rscript")) && file.exists(file.path(R.home("bin"), "Rscript"))) "ok" else "warning",
        if (file.exists(file.path(R.home("bin"), "Rscript"))) "Rscript is available." else "Rscript was not found under R.home().",
        file.path(R.home("bin"), "Rscript")
    )
    version <- tryCatch(as.character(utils::packageVersion("epwshiftr")), error = function(e) NA_character_)
    add(
        "package",
        if (is.na(version)) "warning" else "ok",
        if (is.na(version)) "Could not resolve epwshiftr package version." else paste("epwshiftr", version),
        NA_character_
    )
    launcher <- Sys.which("epwshiftr")
    add(
        "launcher",
        if (nzchar(launcher)) "ok" else "warning",
        if (nzchar(launcher)) "epwshiftr launcher is on PATH." else "epwshiftr launcher was not found on PATH.",
        if (nzchar(launcher)) unname(launcher) else NA_character_
    )

    resolved_store <- epwshiftr_cli_doctor_store_path(store_path)
    add(
        "store_path",
        if (dir.exists(resolved_store)) "ok" else "warning",
        if (dir.exists(resolved_store)) "Store directory exists." else "Store directory does not exist.",
        resolved_store
    )
    if (dir.exists(resolved_store)) {
        writable <- unname(file.access(resolved_store, 2L)) == 0L
        add(
            "store_writable",
            if (writable) "ok" else "error",
            if (writable) "Store directory is writable." else "Store directory is not writable.",
            resolved_store
        )
    } else {
        add("store_writable", "skipped", "Store directory is missing.", resolved_store)
    }

    manifest <- file.path(resolved_store, "manifest.duckdb")
    store_meta <- epwshiftr_cli_doctor_manifest_meta(manifest, "store_meta")
    add(
        "store_manifest",
        store_meta$status,
        store_meta$message,
        manifest
    )
    schema_version <- epwshiftr_cli_doctor_meta_value(store_meta$data, "schema_version")
    add(
        "store_schema",
        if (identical(store_meta$status, "ok")) "ok" else "skipped",
        if (!is.na(schema_version)) {
            sprintf("Store schema version is %s.", schema_version)
        } else if (identical(store_meta$status, "ok")) {
            "Store schema version is missing."
        } else {
            "Store manifest is not readable."
        },
        if (!is.na(schema_version)) schema_version else NA_character_
    )

    downloader_dir <- file.path(resolved_store, "downloads", "_downloader")
    downloader_config <- file.path(downloader_dir, "config.json")
    downloader_manifest <- file.path(downloader_dir, "manifest.duckdb")
    config_check <- epwshiftr_cli_doctor_downloader_config(downloader_config)
    add("downloader_config", config_check$status, config_check$message, downloader_config)
    download_meta <- epwshiftr_cli_doctor_manifest_meta(downloader_manifest, "download_meta")
    add("downloader_manifest", download_meta$status, download_meta$message, downloader_manifest)
    downloader_schema <- epwshiftr_cli_doctor_meta_value(download_meta$data, "schema_version")
    add(
        "downloader_schema",
        if (identical(download_meta$status, "ok")) "ok" else "skipped",
        if (!is.na(downloader_schema)) {
            sprintf("Downloader schema version is %s.", downloader_schema)
        } else if (identical(download_meta$status, "ok")) {
            "Downloader schema version is missing."
        } else {
            "Downloader manifest is not readable."
        },
        if (!is.na(downloader_schema)) downloader_schema else NA_character_
    )

    tmp_downloads <- file.path(resolved_store, "tmp", "downloads")
    if (dir.exists(tmp_downloads)) {
        writable <- unname(file.access(tmp_downloads, 2L)) == 0L
        add(
            "tmp_downloads",
            if (writable) "ok" else "error",
            if (writable) "Temporary download directory is writable." else "Temporary download directory is not writable.",
            tmp_downloads
        )
    } else {
        add("tmp_downloads", "warning", "Temporary download directory does not exist.", tmp_downloads)
    }

    if (isTRUE(parsed$flags[["--network"]])) {
        network <- epwshiftr_cli_doctor_network(index_node, timeout)
        add("index_node", network$status, network$message, network$detail)
    } else {
        add("index_node", "skipped", "Network check skipped. Use --network to enable it.", index_node)
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


epwshiftr_cli_doctor_store_path <- function(path = NULL) {
    if (is.null(path)) {
        return(store_dir(init = FALSE))
    }
    normalizePath(path.expand(path), mustWork = FALSE, winslash = "/")
}


epwshiftr_cli_doctor_detail <- function(detail) {
    if (is.null(detail) || !length(detail)) {
        return(NA_character_)
    }
    detail <- as.character(detail[[1L]])
    if (!nzchar(detail)) NA_character_ else detail
}


epwshiftr_cli_doctor_manifest_meta <- function(path, table) {
    if (!file.exists(path)) {
        return(list(status = "warning", message = "Manifest file does not exist.", data = data.frame()))
    }
    conn <- NULL
    tryCatch(
        {
            conn <- ddb_connect(path, read_only = TRUE)
            tables <- ddb_list_tables(conn)
            if (!table %in% tables) {
                return(list(status = "warning", message = sprintf("Manifest table %s does not exist.", table), data = data.frame()))
            }
            data <- ddb_query(conn, sprintf("SELECT key, value FROM %s", ddb_ident(conn, table)))
            list(status = "ok", message = "Manifest is readable.", data = as.data.frame(data, stringsAsFactors = FALSE))
        },
        error = function(e) {
            list(status = "error", message = conditionMessage(e), data = data.frame())
        },
        finally = {
            if (!is.null(conn)) {
                try(ddb_disconnect(conn, shutdown = TRUE), silent = TRUE)
            }
        }
    )
}


epwshiftr_cli_doctor_meta_value <- function(meta, key) {
    if (!is.data.frame(meta) || !nrow(meta) || !"key" %in% names(meta) || !"value" %in% names(meta)) {
        return(NA_character_)
    }
    row <- meta[meta$key == key, , drop = FALSE]
    if (!nrow(row)) {
        return(NA_character_)
    }
    value <- as.character(row$value[[1L]])
    if (is.na(value) || !nzchar(value)) NA_character_ else value
}


epwshiftr_cli_doctor_downloader_config <- function(path) {
    if (!file.exists(path)) {
        return(list(status = "warning", message = "Downloader config file does not exist."))
    }
    tryCatch(
        {
            downloader__config_read(path)
            list(status = "ok", message = "Downloader config is readable.")
        },
        error = function(e) {
            list(status = "error", message = conditionMessage(e))
        }
    )
}


epwshiftr_cli_doctor_network <- function(index_node, timeout = 10L) {
    tryCatch(
        {
            query <- esg_query(index_node)$limit(0L)
            handle <- curl::new_handle(timeout = timeout, connecttimeout = min(timeout, 10L))
            response <- curl::curl_fetch_memory(query$url(), handle = handle)
            code <- response$status_code
            if (code >= 200L && code < 400L) {
                list(status = "ok", message = sprintf("Index node responded with HTTP %s.", code), detail = query$url())
            } else {
                list(status = "error", message = sprintf("Index node responded with HTTP %s.", code), detail = query$url())
            }
        },
        error = function(e) {
            list(status = "error", message = conditionMessage(e), detail = index_node)
        }
    )
}
