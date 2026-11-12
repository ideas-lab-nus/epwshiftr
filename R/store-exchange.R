# Exchange completed extraction metadata without copying weather files or opening
# another coordinator's live DuckDB manifest. File workers use independent child
# stores under the annual owner's store root. Receivers supply local paths to
# already synchronized, immutable Parquet artifacts inside that same root.

# Select exact identifiers through bound SQL literals, preserving empty schemas.
store_exchange__rows <- function(conn, table, column, ids) {
    predicate <- if (length(ids)) {
        paste0(
            ddb_ident(conn, column),
            " IN (",
            paste(ddb_literal(conn, ids), collapse = ","),
            ")"
        )
    } else {
        "FALSE"
    }
    data.table::as.data.table(ddb_query(
        conn,
        paste0(
            "SELECT * FROM ",
            ddb_ident(conn, table),
            " WHERE ",
            predicate
        )
    ))
}

# Encode timestamps as numeric seconds explicitly. The destination's own schema,
# rather than classes supplied by a receipt, controls safe reconstruction.
store_exchange__encode <- function(rows) {
    lapply(rows, function(column) {
        if (inherits(column, c("POSIXct", "Date")) || is.double(column)) {
            # JSON numeric output may keep only 15 significant digits. Explicit
            # decimal strings preserve all IEEE double bits across both nodes.
            value <- sprintf("%.17g", as.double(column))
            value[is.na(column)] <- NA_character_
            value
        } else {
            column
        }
    })
}

# Parse a complete numeric column with the JSON C parser. Some R decimal-string
# conversions round long epoch/coordinate literals one ULP incorrectly; batching
# preserves the original 17-digit wire format without loosening identity checks.
store_exchange__numeric_values <- function(column) {
    literals <- vapply(
        column,
        function(value) {
            if (is.null(value)) {
                return("null")
            }
            checkmate::assert_atomic(value, len = 1L)
            if (is.numeric(value)) {
                if (is.na(value)) {
                    return("null")
                }
                if (!is.finite(value)) {
                    cli::cli_abort(
                        "Extraction receipt contains a non-finite value."
                    )
                }
                value <- sprintf("%.17g", as.double(value))
            }
            if (
                !is.character(value) ||
                    !grepl(
                        "^-?(0|[1-9][0-9]*)([.][0-9]+)?([eE][+-]?[0-9]+)?$",
                        value
                    )
            ) {
                cli::cli_abort(
                    "Extraction receipt contains an invalid typed value."
                )
            }
            # Force the double parser for signed zero instead of the integer callback.
            if (identical(value, "-0")) "-0.0" else value
        },
        character(1L),
        USE.NAMES = FALSE
    )
    values <- as.double(jsonlite::fromJSON(paste0(
        "[",
        paste(literals, collapse = ","),
        "]"
    )))
    if (any(!is.finite(values) & !is.na(values))) {
        cli::cli_abort("Extraction receipt contains a non-finite value.")
    }
    values
}

# Restore only primitive manifest columns; no R objects or executable serialized
# state are accepted. JSON nulls retain typed missing values in the local schema.
store_exchange__decode <- function(columns, template) {
    if (!identical(names(columns), names(template))) {
        cli::cli_abort(
            "Extraction receipt columns do not match the local schema."
        )
    }
    lengths <- lengths(columns)
    if (length(unique(lengths)) > 1L) {
        cli::cli_abort("Extraction receipt columns have unequal lengths.")
    }
    for (name in names(template)) {
        target <- template[[name]]
        numeric_type <- inherits(target, c("POSIXct", "Date")) ||
            is.numeric(target)
        prototype <- if (numeric_type) {
            NA_real_
        } else if (is.logical(target)) {
            NA
        } else {
            NA_character_
        }
        values <- if (numeric_type) {
            store_exchange__numeric_values(columns[[name]])
        } else {
            vapply(
                columns[[name]],
                function(value) {
                    if (is.null(value)) {
                        return(prototype)
                    }
                    checkmate::assert_atomic(value, len = 1L)
                    if (
                        (is.logical(target) && !is.logical(value)) ||
                            (is.character(target) && !is.character(value))
                    ) {
                        cli::cli_abort(
                            "Extraction receipt contains an invalid typed value."
                        )
                    }
                    value
                },
                prototype,
                USE.NAMES = FALSE
            )
        }
        converted <- if (inherits(target, "POSIXct")) {
            as.POSIXct(as.double(values), origin = "1970-01-01", tz = "UTC")
        } else if (inherits(target, "Date")) {
            as.Date(as.double(values), origin = "1970-01-01")
        } else {
            switch(
                typeof(target),
                character = values,
                integer = as.integer(values),
                double = as.double(values),
                logical = as.logical(values),
                cli::cli_abort("Unsupported extraction receipt column type.")
            )
        }
        if (any(is.na(converted) & !is.na(values))) {
            cli::cli_abort(
                "Extraction receipt contains an invalid typed value."
            )
        }
        if (is.integer(target) && any(converted != values, na.rm = TRUE)) {
            cli::cli_abort("Extraction receipt contains a non-integer count.")
        }
        # Build columns together below: set() cannot extend a zero-row table.
        columns[[name]] <- converted
    }
    data.table::as.data.table(columns)
}

# Compare original plan coordinates/periods and full physical file identities.
# A matching filename or short pool key alone cannot authorize cross-store reuse.
store_exchange__identity <- function(imported, local, columns, key) {
    if (
        anyDuplicated(imported[[key]]) ||
            !all(imported[[key]] %in% local[[key]])
    ) {
        cli::cli_abort(
            "Extraction receipt references unknown or duplicate identities."
        )
    }
    local <- local[match(imported[[key]], local[[key]])]
    for (name in columns) {
        if (
            !isTRUE(all.equal(
                imported[[name]],
                local[[name]],
                check.attributes = TRUE,
                tolerance = 0
            ))
        ) {
            cli::cli_abort(
                "Extraction receipt identity mismatch in {.val {name}}."
            )
        }
    }
    invisible(TRUE)
}

# Export executable plan metadata separately from completed result receipts. The
# original IDs and typed manifest values survive lossy catalogue JSON formatting.
store_exchange__export_plans <- function(store, private, plan_id, path) {
    checkmate::assert_character(
        plan_id,
        min.len = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    checkmate::assert_string(path, min.chars = 1L)
    private$check_open()
    private$with_store_lock({
        plans <- store_exchange__rows(
            private$conn,
            "extraction_plan",
            "plan_id",
            plan_id
        )
        if (
            !setequal(plans$plan_id, plan_id) ||
                !all(plans$status %in% c("pending", "failed"))
        ) {
            cli::cli_abort(
                "Only existing pending or failed plans can be exported for execution."
            )
        }
        plans <- plans[match(plan_id, plans$plan_id)]
        files <- store_exchange__rows(
            private$conn,
            "file_catalog",
            "file_key",
            unique(plans$file_key)
        )
        if (
            any(!is.na(files$local_path) & nzchar(files$local_path)) ||
                any(
                    !is.na(files$local_artifact_id) &
                        nzchar(files$local_artifact_id)
                )
        ) {
            cli::cli_abort(
                "Plans with registered local file caches must stay in their original store."
            )
        }
        queries <- store_exchange__rows(
            private$conn,
            "query_run",
            "query_id",
            unique(plans$query_id)
        )
        links <- store_exchange__rows(
            private$conn,
            "esg_query_file",
            "file_key",
            files$file_key
        )
        links <- links[links$query_id %in% queries$query_id]
        metadata <- list(
            query_run = queries,
            esg_file = store_exchange__rows(
                private$conn,
                "esg_file",
                "file_key",
                files$file_key
            ),
            esg_query_file = links,
            file_catalog = files,
            extraction_plan = plans
        )
        if (
            any(
                !is.na(metadata$esg_file$local_path) &
                    nzchar(metadata$esg_file$local_path)
            ) ||
                any(
                    !is.na(metadata$esg_file$local_artifact_id) &
                        nzchar(metadata$esg_file$local_artifact_id)
                )
        ) {
            cli::cli_abort(
                "Plans with registered local file caches must stay in their original store."
            )
        }
        query_paths <- stats::setNames(
            vapply(
                queries$query_file,
                function(f) {
                    result <- store_abs_path(f, private$store_path)
                    store_rel_path(result, private$store_path)
                    result
                },
                character(1L)
            ),
            queries$query_id
        )
        query_sha <- vapply(
            query_paths,
            store_hash_file,
            character(1L),
            "sha256"
        )
        receipt <- list(
            schema = "epwshiftr-plan-receipt-1",
            store_schema = STORE_SCHEMA_VERSION,
            tables = lapply(metadata, store_exchange__encode),
            query_checksums = as.list(query_sha)
        )
        if (file.exists(path)) {
            cli::cli_abort("Plan receipt already exists.")
        }
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        temporary <- tempfile(tmpdir = dirname(path), fileext = ".json")
        on.exit(unlink(temporary), add = TRUE)
        jsonlite::write_json(
            receipt,
            temporary,
            auto_unbox = FALSE,
            digits = NA,
            null = "null",
            na = "null"
        )
        if (!file.rename(temporary, path)) {
            cli::cli_abort("Cannot publish plan receipt.")
        }
        list(
            path = normalizePath(path, winslash = "/"),
            checksum = store_hash_file(path, "sha256"),
            query_paths = query_paths,
            plan_id = plan_id
        )
    })
}

# Validate intrinsic plan IDs independently of receipt transport. Original query
# IDs remain fixed; no JSON reload or new catalogue query silently remakes them.
store_exchange__validate_plans <- function(plans, files, queries, links) {
    if (
        !nrow(plans) ||
            anyNA(plans$plan_id) ||
            anyDuplicated(plans$plan_id) ||
            anyDuplicated(files$file_key) ||
            anyDuplicated(queries$query_id) ||
            anyDuplicated(links$link_id) ||
            !all(plans$status %in% c("pending", "failed")) ||
            !setequal(unique(plans$file_key), files$file_key) ||
            !setequal(unique(plans$query_id), queries$query_id) ||
            !all(files$query_id %in% queries$query_id) ||
            !all(links$file_key %in% files$file_key) ||
            !all(links$query_id %in% queries$query_id) ||
            !setequal(unique(links$file_key), files$file_key) ||
            anyNA(plans$time_start) ||
            anyNA(plans$time_stop) ||
            any(plans$time_start > plans$time_stop) ||
            any(!is.finite(plans$lon) | plans$lon < -180 | plans$lon > 360) ||
            any(!is.finite(plans$lat) | plans$lat < -90 | plans$lat > 90) ||
            !all(plans$method %in% ESG_GRID_METHOD_CHOICES)
    ) {
        cli::cli_abort("Plan receipt relationships or coordinates are invalid.")
    }
    ids <- vapply(
        seq_len(nrow(plans)),
        function(i) {
            p <- plans[i]
            store__hash(
                p$query_id,
                p$file_key,
                p$site_id,
                p$variable_id,
                p$lon,
                p$lat,
                p$method,
                p$time_start,
                p$time_stop
            )
        },
        character(1L)
    )
    if (!identical(ids, plans$plan_id)) {
        cli::cli_abort(
            "Plan receipt identity mismatch: original plan IDs cannot be changed."
        )
    }
    selected <- files[match(plans$file_key, files$file_key)]
    if (
        !identical(selected$query_id, plans$query_id) ||
            !identical(selected$variable_id, plans$variable_id)
    ) {
        cli::cli_abort("Plan receipt file lineage is invalid.")
    }
    invisible(NULL)
}

# Install only original pending plan/catalogue metadata under this store's sole
# writer. Existing rows, retry history and completed outputs are never replaced.
# Only small query JSON controls are copied; no native nc or Parquet is copied.
store_exchange__import_plans <- function(
    store,
    private,
    path,
    checksum,
    query_paths
) {
    private$check_open()
    checkmate::assert_string(checksum, pattern = "^[a-f0-9]{64}$")
    checkmate::assert_character(query_paths, any.missing = FALSE, unique = TRUE)
    if (!identical(store_hash_file(path, "sha256"), checksum)) {
        cli::cli_abort("Plan receipt checksum mismatch.")
    }
    receipt <- jsonlite::read_json(path, simplifyVector = FALSE)
    tables <- c(
        "query_run",
        "esg_file",
        "esg_query_file",
        "file_catalog",
        "extraction_plan"
    )
    if (
        !identical(receipt$schema, list("epwshiftr-plan-receipt-1")) ||
            !identical(receipt$store_schema, list(STORE_SCHEMA_VERSION)) ||
            !identical(names(receipt$tables), tables)
    ) {
        cli::cli_abort("Unsupported plan receipt schema or tables.")
    }
    private$with_store_lock({
        data <- stats::setNames(
            lapply(tables, function(table) {
                store_exchange__decode(
                    receipt$tables[[table]],
                    ddb_query(
                        private$conn,
                        paste0("SELECT * FROM ", table, " WHERE FALSE")
                    )
                )
            }),
            tables
        )
        plans <- data$extraction_plan
        queries <- data$query_run
        files <- data$file_catalog
        store_exchange__validate_plans(
            plans,
            files,
            queries,
            data$esg_query_file
        )
        if (
            !setequal(names(query_paths), queries$query_id) ||
                anyDuplicated(names(query_paths)) ||
                !setequal(names(receipt$query_checksums), queries$query_id) ||
                any(!is.na(files$local_path) & nzchar(files$local_path)) ||
                any(
                    !is.na(files$local_artifact_id) &
                        nzchar(files$local_artifact_id)
                )
        ) {
            cli::cli_abort(
                "Plan receipt query controls or cache references are invalid."
            )
        }
        # Check every incoming/existing identity before creating files or a DB
        # transaction. Mutable status/timestamps/locations remain local state.
        keys <- c(
            query_run = "query_id",
            esg_file = "file_key",
            esg_query_file = "link_id",
            file_catalog = "file_key",
            extraction_plan = "plan_id"
        )
        columns <- list(
            query_run = c(
                "query_id",
                "result_type",
                "index_node",
                "time_filter_start",
                "time_filter_stop",
                "time_filter_method"
            ),
            esg_file = c(
                "file_key",
                "source_id",
                "experiment_id",
                "variant_label",
                "grid_label",
                "table_id",
                "variable_id",
                "title",
                "version",
                "tracking_id",
                "checksum",
                "checksum_type",
                "size"
            ),
            esg_query_file = c("link_id", "query_id", "file_key"),
            file_catalog = c(
                "file_key",
                "query_id",
                "source_id",
                "experiment_id",
                "variant_label",
                "grid_label",
                "table_id",
                "variable_id",
                "title",
                "version",
                "tracking_id",
                "checksum",
                "checksum_type",
                "size"
            ),
            extraction_plan = c(
                "plan_id",
                "query_id",
                "file_key",
                "site_id",
                "variable_id",
                "lon",
                "lat",
                "method",
                "time_start",
                "time_stop"
            )
        )
        if (!setequal(files$file_key, data$esg_file$file_key)) {
            cli::cli_abort("Plan receipt file registry is inconsistent.")
        }
        store_exchange__identity(
            files,
            data$esg_file,
            columns$esg_file,
            "file_key"
        )
        if (
            any(
                !is.na(data$esg_file$local_path) &
                    nzchar(data$esg_file$local_path)
            ) ||
                any(
                    !is.na(data$esg_file$local_artifact_id) &
                        nzchar(data$esg_file$local_artifact_id)
                )
        ) {
            cli::cli_abort(
                "Plan receipt must not carry another store's cache paths."
            )
        }
        existing <- lapply(tables, function(table) {
            store_exchange__rows(
                private$conn,
                table,
                keys[[table]],
                data[[table]][[keys[[table]]]]
            )
        })
        names(existing) <- tables
        for (table in tables) {
            incoming <- data[[table]]
            same <- incoming[
                incoming[[keys[[table]]]] %in%
                    existing[[table]][[keys[[table]]]]
            ]
            if (nrow(same)) {
                store_exchange__identity(
                    same,
                    existing[[table]],
                    columns[[table]],
                    keys[[table]]
                )
            }
        }
        for (id in queries$query_id) {
            expected <- unlist(receipt$query_checksums[[id]], use.names = FALSE)
            if (
                length(expected) != 1L ||
                    !grepl("^[a-f0-9]{64}$", expected) ||
                    !file.exists(query_paths[[id]]) ||
                    store_hash_file(query_paths[[id]], "sha256") != expected
            ) {
                cli::cli_abort(
                    "Plan query metadata is missing or has changed.",
                    class = "epwshiftr_store_receipt_bytes_unavailable"
                )
            }
            # Parse JSON controls only, without loading an R object or executing
            # a producer. Original typed catalogue rows are authoritative.
            jsonlite::read_json(query_paths[[id]], simplifyVector = FALSE)
        }
        copied <- character()
        committed <- FALSE
        on.exit(
            {
                if (!committed) {
                    for (f in names(copied)) {
                        if (
                            file.exists(f) &&
                                store_hash_file(f, "sha256") == copied[[f]]
                        ) {
                            unlink(f)
                        }
                    }
                }
            },
            add = TRUE
        )
        for (i in seq_len(nrow(queries))) {
            id <- queries$query_id[[i]]
            prior <- existing$query_run[existing$query_run$query_id == id]
            if (nrow(prior)) {
                destination <- store_abs_path(
                    prior$query_file[[1L]],
                    private$store_path
                )
            } else {
                destination <- file.path(
                    private$query_dir,
                    paste0("plans-", id, ".json")
                )
            }
            expected <- unlist(receipt$query_checksums[[id]], use.names = FALSE)
            if (file.exists(destination)) {
                if (store_hash_file(destination, "sha256") != expected) {
                    cli::cli_abort(
                        "Existing query metadata conflicts with plan receipt."
                    )
                }
            } else {
                if (nrow(prior)) {
                    cli::cli_abort("Existing query metadata is missing.")
                }
                if (!file.copy(query_paths[[id]], destination)) {
                    cli::cli_abort("Cannot copy plan query metadata.")
                }
                copied[[destination]] <- expected
                if (store_hash_file(destination, "sha256") != expected) {
                    cli::cli_abort("Copied query metadata checksum mismatch.")
                }
            }
            data.table::set(
                data$query_run,
                i = i,
                j = "query_file",
                value = store_rel_path(destination, private$store_path)
            )
        }
        ddb_exec(private$conn, "BEGIN TRANSACTION")
        on.exit(if (!committed) ddb_exec(private$conn, "ROLLBACK"), add = TRUE)
        for (table in tables) {
            incoming <- data[[table]]
            fresh <- incoming[
                !incoming[[keys[[table]]]] %in%
                    existing[[table]][[keys[[table]]]]
            ]
            if (nrow(fresh)) ddb_append_table(private$conn, table, fresh)
        }
        ddb_exec(private$conn, "COMMIT")
        committed <- TRUE
        data.table::data.table(
            plan_id = plans$plan_id,
            reused = plans$plan_id %in% existing$extraction_plan$plan_id
        )
    })
}
# vim: fdm=marker :
