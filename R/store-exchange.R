# Exchange completed extraction metadata without copying weather files or opening
# another coordinator's live DuckDB manifest. File workers use independent child
# stores under the annual owner's store root. Receivers supply local paths to
# already synchronized, immutable Parquet artifacts inside that same root.

STORE_EXCHANGE_PLAN_COLS <- strsplit(
    "plan_id,query_id,file_key,site_id,variable_id,lon,lat,method,time_start,time_stop",
    ",",
    fixed = TRUE
)[[1L]]
STORE_EXCHANGE_FILE_COLS <- strsplit(
    "file_key,source_id,experiment_id,variant_label,grid_label,table_id,variable_id,title,version,tracking_id,checksum,checksum_type,size",
    ",",
    fixed = TRUE
)[[1L]]

store_exchange__write_receipt <- function(receipt, path, label) {
    # Never replace a prior receipt; its SHA is a durable transfer identity.
    if (file.exists(path)) {
        cli::cli_abort("{label} receipt already exists.")
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
        cli::cli_abort("Cannot publish {tolower(label)} receipt.")
    }
    list(
        path = normalizePath(path, winslash = "/"),
        checksum = store_hash_file(path, "sha256")
    )
}

store_exchange__read_receipt <- function(
    conn,
    path,
    checksum,
    schema,
    tables,
    label,
    combined = FALSE
) {
    checkmate::assert_string(checksum, pattern = "^[a-f0-9]{64}$")
    if (!identical(store_hash_file(path, "sha256"), checksum)) {
        cli::cli_abort("{label} receipt checksum mismatch.")
    }
    receipt <- jsonlite::read_json(path, simplifyVector = FALSE)
    bad_schema <- !identical(receipt$schema, list(schema)) ||
        !identical(receipt$store_schema, list(STORE_SCHEMA_VERSION))
    bad_tables <- !identical(names(receipt$tables), tables)
    if (isTRUE(combined) && (bad_schema || bad_tables)) {
        cli::cli_abort("Unsupported {tolower(label)} receipt schema or tables.")
    }
    if (bad_schema) {
        cli::cli_abort("Unsupported {tolower(label)} receipt schema.")
    }
    if (bad_tables) {
        cli::cli_abort("Invalid {tolower(label)} receipt tables.")
    }
    data <- stats::setNames(
        lapply(tables, function(table) {
            store_exchange__decode(
                receipt$tables[[table]],
                ddb_query(conn, paste0("SELECT * FROM ", table, " WHERE FALSE"))
            )
        }),
        tables
    )
    list(receipt = receipt, data = data)
}

store_exchange__assert_no_local_cache <- function(dt, message) {
    has_path <- any(!is.na(dt$local_path) & nzchar(dt$local_path))
    has_art <- any(!is.na(dt$local_artifact_id) & nzchar(dt$local_artifact_id))
    if (has_path || has_art) {
        cli::cli_abort(message)
    }
    invisible(NULL)
}

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
        paste0("SELECT * FROM ", ddb_ident(conn, table), " WHERE ", predicate)
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

# Publish only terminal plans and their exact manifest rows. Export performs full
# artifact checks but contains metadata only, never weather arrays or native nc.
store_exchange__export <- function(store, private, plan_id, path) {
    checkmate::assert_character(plan_id, any.missing = FALSE, unique = TRUE)
    checkmate::assert_string(path, min.chars = 1L)
    private$with_store_lock({
        fetch <- function(tbl, col, ids) {
            store_exchange__rows(private$conn, tbl, col, ids)
        }
        plans <- fetch("extraction_plan", "plan_id", plan_id)
        if (
            !setequal(plans$plan_id, plan_id) ||
                !all(plans$status %in% c("done", "empty"))
        ) {
            cli::cli_abort(
                "Only existing completed or empty extraction plans can be exported."
            )
        }
        plans <- plans[match(plan_id, plans$plan_id)]
        results <- fetch("extraction_result", "plan_id", plan_id)
        grid <- fetch("extraction_grid_source", "plan_id", plan_id)
        files <- fetch("file_catalog", "file_key", unique(plans$file_key))
        artifacts <- fetch(
            "artifact",
            "artifact_id",
            unique(results$artifact_id)
        )
        done <- plans$plan_id[plans$status == "done"]
        empty <- plans$plan_id[plans$status == "empty"]
        if (
            !setequal(results$plan_id, done) ||
                any(grid$plan_id %in% empty) ||
                !all(artifacts$kind == "extract") ||
                !all(artifacts$status == "available") ||
                !all(plans$available_time_count[plans$status == "empty"] == 0L)
        ) {
            cli::cli_abort("Completed extraction metadata is inconsistent.")
        }
        validation <- store$validate(artifacts$artifact_id)
        if (
            !all(
                validation$exists & validation$size_ok & validation$checksum_ok
            )
        ) {
            cli::cli_abort(
                "Extraction artifacts failed validation before export."
            )
        }
        receipt <- list(
            schema = "epwshiftr-extraction-receipt-1",
            store_schema = STORE_SCHEMA_VERSION,
            tables = lapply(
                list(
                    extraction_plan = plans,
                    file_catalog = files,
                    extraction_result = results,
                    extraction_grid_source = grid,
                    artifact = artifacts
                ),
                store_exchange__encode
            )
        )
        c(
            store_exchange__write_receipt(receipt, path, "Extraction"),
            list(
                artifact_paths = stats::setNames(
                    store$artifact_path(artifacts$artifact_id),
                    artifacts$artifact_id
                )
            )
        )
    })
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

# Adopt verified Parquet references atomically through the sole store writer.
# Existing completed results are validated and reused, never replaced or copied.
# Callers must separately apply their annual CF-clock/scientific completeness gate.
store_exchange__import <- function(
    store,
    private,
    path,
    checksum,
    artifact_paths
) {
    tables <- c(
        "extraction_plan",
        "file_catalog",
        "extraction_result",
        "extraction_grid_source",
        "artifact"
    )
    private$with_store_lock({
        data <- store_exchange__read_receipt(
            private$conn,
            path,
            checksum,
            "epwshiftr-extraction-receipt-1",
            tables,
            "Extraction"
        )$data
        plans <- data$extraction_plan
        results <- data$extraction_result
        artifacts <- data$artifact
        local <- store_exchange__rows(
            private$conn,
            "extraction_plan",
            "plan_id",
            plans$plan_id
        )
        store_exchange__identity(
            plans,
            local,
            STORE_EXCHANGE_PLAN_COLS,
            "plan_id"
        )
        files <- data$file_catalog
        local_files <- store_exchange__rows(
            private$conn,
            "file_catalog",
            "file_key",
            files$file_key
        )
        store_exchange__identity(
            files,
            local_files,
            c("query_id", STORE_EXCHANGE_FILE_COLS),
            "file_key"
        )
        done_plan_ids <- plans$plan_id[plans$status == "done"]
        if (
            !setequal(plans$file_key, files$file_key) ||
                !all(plans$status %in% c("done", "empty")) ||
                !setequal(results$plan_id, done_plan_ids) ||
                !setequal(results$artifact_id, artifacts$artifact_id) ||
                anyDuplicated(artifacts$artifact_id) ||
                !all(artifacts$kind == "extract") ||
                !all(artifacts$status == "available") ||
                !all(plans$available_time_count[plans$status == "empty"] == 0L)
        ) {
            cli::cli_abort("Extraction receipt relationships are inconsistent.")
        }
        checkmate::assert_character(artifact_paths, any.missing = FALSE)
        if (anyDuplicated(names(artifact_paths))) {
            cli::cli_abort("Duplicate extraction artifact paths.")
        }
        if (!setequal(names(artifact_paths), artifacts$artifact_id)) {
            cli::cli_abort(
                "Supply exactly one local path for every extraction artifact."
            )
        }
        artifact_paths <- artifact_paths[artifacts$artifact_id]
        # Validate every byte before starting the manifest transaction. Failed or
        # lagging Drive transfers leave the destination entirely untouched.
        for (i in seq_len(nrow(artifacts))) {
            a <- artifacts[i]
            f <- artifact_paths[[i]]
            store_rel_path(f, root = private$store_path)
            if (
                !file.exists(f) ||
                    file.info(f)$size != a$size ||
                    store_hash_file(f, a$checksum_type) != a$checksum
            ) {
                cli::cli_abort(
                    "An extraction artifact is missing or has changed.",
                    class = "epwshiftr_store_receipt_bytes_unavailable"
                )
            }
            linked <- results[results$artifact_id == a$artifact_id]
            if (nrow(linked) != 1L) {
                cli::cli_abort(
                    "An extraction artifact must belong to exactly one result."
                )
            }
            # Verify the Parquet's own identifiers rather than trusting its path.
            content <- ddb_query(
                private$conn,
                paste0(
                    "SELECT DISTINCT plan_id,file_key,query_id,site_id,variable_id FROM read_parquet(",
                    ddb_literal(private$conn, f),
                    ")"
                )
            )
            expected <- plans[match(linked$plan_id, plans$plan_id)]
            store_exchange__identity(
                data.table::as.data.table(content),
                expected,
                c("plan_id", "file_key", "query_id", "site_id", "variable_id"),
                "plan_id"
            )
            counts <- ddb_query(
                private$conn,
                paste0(
                    "SELECT COUNT(*) AS row_count, COUNT(DISTINCT time) AS unique_time_count, MIN(time) AS time_min, MAX(time) AS time_max, MIN(year) AS first_year, MAX(year) AS last_year FROM read_parquet(",
                    ddb_literal(private$conn, f),
                    ")"
                )
            )
            if (
                nrow(content) != 1L ||
                    counts$row_count != linked$row_count ||
                    counts$unique_time_count != linked$unique_time_count ||
                    !identical(
                        as.double(counts$time_min),
                        as.double(linked$time_min)
                    ) ||
                    !identical(
                        as.double(counts$time_max),
                        as.double(linked$time_max)
                    ) ||
                    counts$first_year != linked$year ||
                    counts$last_year != linked$year
            ) {
                cli::cli_abort(
                    "Extraction Parquet coverage disagrees with its receipt."
                )
            }
        }
        # Exact result coverage above already restricts results to done plans.
        # Grid sources are optional, but every supplied row must belong to one.
        grid_sources <- data$extraction_grid_source
        if (!all(grid_sources$plan_id %in% done_plan_ids)) {
            cli::cli_abort("Extraction result or grid lineage is inconsistent.")
        }
        for (table in list(results, grid_sources)) {
            expected <- plans[match(table$plan_id, plans$plan_id)]
            if (
                !identical(table$file_key, expected$file_key) ||
                    !identical(table$query_id, expected$query_id) ||
                    ("variable_id" %in%
                        names(table) &&
                        !identical(
                            table$variable_id,
                            expected$variable_id
                        )) ||
                    ("method" %in%
                        names(table) &&
                        !identical(table$method, expected$method))
            ) {
                cli::cli_abort(
                    "Extraction result or grid lineage is inconsistent."
                )
            }
        }
        fetch <- function(tbl, col, ids) {
            store_exchange__rows(private$conn, tbl, col, ids)
        }
        reused <- local$plan_id[local$status %in% c("done", "empty")]
        for (id in reused) {
            old <- fetch("extraction_result", "plan_id", id)
            incoming <- results[results$plan_id == id]
            old_artifacts <- fetch("artifact", "artifact_id", old$artifact_id)
            expected_hash <- artifacts$checksum[match(
                incoming$artifact_id,
                artifacts$artifact_id
            )]
            if (
                !identical(
                    local$status[match(id, local$plan_id)],
                    plans$status[match(id, plans$plan_id)]
                ) ||
                    !identical(
                        sort(old_artifacts$checksum),
                        sort(expected_hash)
                    )
            ) {
                cli::cli_abort(
                    "Existing completed extraction conflicts with the receipt.",
                    class = "epwshiftr_store_extract_conflict"
                )
            }
            valid <- store$validate(old_artifacts$artifact_id)
            if (!all(valid$exists & valid$size_ok & valid$checksum_ok)) {
                cli::cli_abort("Existing extraction is no longer valid.")
            }
        }
        pending <- setdiff(plans$plan_id, reused)
        result <- data.table::data.table(
            plan_id = plans$plan_id,
            status = plans$status,
            reused = plans$plan_id %in% reused
        )
        if (!length(pending)) {
            return(result)
        }
        if (
            nrow(fetch("extraction_result", "plan_id", pending)) ||
                nrow(fetch("extraction_grid_source", "plan_id", pending))
        ) {
            cli::cli_abort(
                "Unsealed local extraction fragments must be recovered before import.",
                class = "epwshiftr_store_extract_conflict"
            )
        }
        inserted <- results[results$plan_id %in% pending]
        grid <- data$extraction_grid_source
        incoming <- plans[match(pending, plans$plan_id)]
        private$publish_extract(
            local[match(pending, local$plan_id)],
            status = incoming$status,
            available_time_count = incoming$available_time_count,
            prepare = {
                # Use local artifact IDs and paths for the pending plans. A
                # receipt can reference one artifact from several result rows.
                new_ids <- character(nrow(artifacts))
                for (i in which(
                    artifacts$artifact_id %in% inserted$artifact_id
                )) {
                    a <- artifacts[i]
                    new_ids[[i]] <- store$register_artifact(
                        "extract",
                        artifact_paths[[i]],
                        role = a$role,
                        project = a$project,
                        checksum = a$checksum,
                        checksum_type = a$checksum_type,
                        size = a$size,
                        query_id = a$query_id,
                        file_key = a$file_key,
                        metadata = list(
                            extraction_receipt_sha256 = checksum,
                            original_artifact_id = a$artifact_id
                        )
                    )
                }
                index <- match(inserted$artifact_id, artifacts$artifact_id)
                data.table::set(
                    inserted,
                    j = "artifact_id",
                    value = new_ids[index]
                )
                data.table::set(
                    inserted,
                    j = "output_path",
                    value = vapply(
                        artifact_paths[index],
                        store_rel_path,
                        character(1L),
                        root = private$store_path,
                        USE.NAMES = FALSE
                    )
                )
                list(
                    results = inserted,
                    grid_sources = grid[grid$plan_id %in% pending]
                )
            }
        )
        result
    })
}

# Export executable plan metadata separately from completed result receipts. The
# original IDs and typed manifest values survive lossy catalogue JSON formatting.
store_exchange__export_plans <- function(private, plan_id, path) {
    checkmate::assert_character(
        plan_id,
        min.len = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    checkmate::assert_string(path, min.chars = 1L)
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
        cache_msg <- "Plans with registered local file caches must stay in their original store."
        store_exchange__assert_no_local_cache(files, cache_msg)
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
        store_exchange__assert_no_local_cache(metadata$esg_file, cache_msg)
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
        c(
            store_exchange__write_receipt(receipt, path, "Plan"),
            list(query_paths = query_paths, plan_id = plan_id)
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
            !setequal(plans$file_key, files$file_key) ||
            !setequal(plans$query_id, queries$query_id) ||
            !all(links$query_id %in% queries$query_id) ||
            !setequal(links$file_key, files$file_key) ||
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
    private,
    path,
    checksum,
    query_paths
) {
    checkmate::assert_character(query_paths, any.missing = FALSE, unique = TRUE)
    tables <- c(
        "query_run",
        "esg_file",
        "esg_query_file",
        "file_catalog",
        "extraction_plan"
    )
    private$with_store_lock({
        loaded <- store_exchange__read_receipt(
            private$conn,
            path,
            checksum,
            "epwshiftr-plan-receipt-1",
            tables,
            "Plan",
            combined = TRUE
        )
        receipt <- loaded$receipt
        data <- loaded$data
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
                !setequal(names(receipt$query_checksums), queries$query_id)
        ) {
            cli::cli_abort(
                "Plan receipt query controls or cache references are invalid."
            )
        }
        store_exchange__assert_no_local_cache(
            files,
            "Plan receipt query controls or cache references are invalid."
        )
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
            esg_file = STORE_EXCHANGE_FILE_COLS,
            esg_query_file = c("link_id", "query_id", "file_key"),
            file_catalog = c("query_id", STORE_EXCHANGE_FILE_COLS),
            extraction_plan = STORE_EXCHANGE_PLAN_COLS
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
        store_exchange__assert_no_local_cache(
            data$esg_file,
            "Plan receipt must not carry another store's cache paths."
        )
        existing <- list()
        for (table in tables) {
            key <- keys[[table]]
            incoming <- data[[table]]
            existing[[table]] <- store_exchange__rows(
                private$conn,
                table,
                key,
                incoming[[key]]
            )
            same <- incoming[incoming[[key]] %in% existing[[table]][[key]]]
            if (nrow(same)) {
                store_exchange__identity(
                    same,
                    existing[[table]],
                    columns[[table]],
                    key
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
            if (!committed) {
                for (f in names(copied)) {
                    if (
                        file.exists(f) &&
                            store_hash_file(f, "sha256") == copied[[f]]
                    ) {
                        unlink(f)
                    }
                }
            },
            add = TRUE
        )
        for (i in seq_len(nrow(queries))) {
            id <- queries$query_id[[i]]
            prior <- existing$query_run[existing$query_run$query_id == id]
            dest <- if (nrow(prior)) {
                store_abs_path(prior$query_file[[1L]], private$store_path)
            } else {
                file.path(private$query_dir, paste0("plans-", id, ".json"))
            }
            expected <- unlist(receipt$query_checksums[[id]], use.names = FALSE)
            if (file.exists(dest)) {
                if (store_hash_file(dest, "sha256") != expected) {
                    cli::cli_abort(
                        "Existing query metadata conflicts with plan receipt."
                    )
                }
            } else {
                if (nrow(prior)) {
                    cli::cli_abort("Existing query metadata is missing.")
                }
                if (!file.copy(query_paths[[id]], dest)) {
                    cli::cli_abort("Cannot copy plan query metadata.")
                }
                copied[[dest]] <- expected
                if (store_hash_file(dest, "sha256") != expected) {
                    cli::cli_abort("Copied query metadata checksum mismatch.")
                }
            }
            data.table::set(
                data$query_run,
                i = i,
                j = "query_file",
                value = store_rel_path(dest, private$store_path)
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
