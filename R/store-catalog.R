# Schema initialization, artifact registry, query registry/graph/updates, and
# file catalog storage helpers for EsgStore.

# Build one initialization-local DDL executor from the current catalog. Keep the
# CREATE/ALTER statements as the sole schema definition: skipping a known target
# avoids DuckDB prepare/execute work without caching database state across opens.
# store_catalog__schema_executor {{{
store_catalog__schema_executor <- function(conn, tables) {
    columns <- if (length(tables)) {
        ddb_query(
            conn,
            paste(
                "SELECT table_name, column_name FROM information_schema.columns",
                "WHERE table_catalog = current_database()",
                "AND table_schema = current_schema()"
            )
        )
    } else {
        data.frame(table_name = character(), column_name = character())
    }
    column_keys <- paste(columns$table_name, columns$column_name, sep = ".")
    # Statements that still need to run are queued and sent to DuckDB as one
    # multi-statement call by flush(). DuckDB stops at the first failing
    # statement and keeps the earlier ones, which is the state sequential
    # execution would leave. Callers flush before any read that depends on the
    # queued DDL.
    pending <- character()
    exec <- function(sql) {
        # Only recognize the package's unquoted, idempotent schema declarations;
        # any other SQL still executes normally instead of being guessed at.
        create <- regmatches(
            sql,
            regexec(
                "^[[:space:]]*CREATE TABLE IF NOT EXISTS ([[:alnum:]_]+)[[:space:]]*[(]",
                sql
            )
        )[[1L]]
        if (length(create) && create[[2L]] %in% tables) {
            return(invisible(NULL))
        }
        alter <- regmatches(
            sql,
            regexec(
                paste0(
                    "^[[:space:]]*ALTER TABLE ([[:alnum:]_]+)",
                    " ADD COLUMN IF NOT EXISTS ([[:alnum:]_]+) "
                ),
                sql
            )
        )[[1L]]
        if (
            length(alter) &&
                paste(alter[[2L]], alter[[3L]], sep = ".") %in% column_keys
        ) {
            return(invisible(NULL))
        }
        # Drop the statement's own terminator so joining never doubles it.
        pending <<- c(pending, sub("[[:space:];]+$", "", sql))
        invisible(NULL)
    }
    flush <- function() {
        if (!length(pending)) {
            return(invisible(NULL))
        }
        batch <- pending
        # Clear first so a failed batch is not replayed by a later flush.
        pending <<- character()
        ddb_exec(conn, paste(batch, collapse = ";\n"))
        invisible(NULL)
    }
    list(exec = exec, flush = flush)
}
# }}}

# store_catalog__add_columns {{{
store_catalog__add_columns <- function(exec, specs) {
    for (table in names(specs)) {
        for (col in specs[[table]]) {
            exec(sprintf(
                "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s",
                table,
                col
            ))
        }
    }
    invisible(NULL)
}
# }}}

# Inspect persisted declarations on every open so external repairs or
# damage are visible without repeating already satisfied DDL.
# store_catalog__init_schema {{{
store_catalog__init_schema <- function(self, private) {
    # Capture whether this is a genuinely empty manifest before
    # creating the metadata table. Existing manifests are never
    # upgraded implicitly because workflow state must remain coherent.
    existing_tables <- ddb_list_tables(private$conn)
    schema <- store_catalog__schema_executor(private$conn, existing_tables)
    exec <- schema$exec
    exec(
        "CREATE TABLE IF NOT EXISTS store_meta (
            key VARCHAR PRIMARY KEY, value VARCHAR, updated_at TIMESTAMP
        )"
    )
    # The version check below reads store_meta, so it must exist first.
    schema$flush()
    current_version <- store_catalog__assert_schema_version(
        private,
        existing_tables
    )
    exec(
        "CREATE TABLE IF NOT EXISTS artifact (
            artifact_id VARCHAR PRIMARY KEY, kind VARCHAR, role VARCHAR, project VARCHAR,
            relative_path VARCHAR, checksum VARCHAR, checksum_type VARCHAR, size DOUBLE,
            status VARCHAR, query_id VARCHAR, file_key VARCHAR, dict_id VARCHAR,
            source_url VARCHAR, source_repo VARCHAR, source_tag VARCHAR, source_commit VARCHAR,
            metadata_json VARCHAR, created_at TIMESTAMP, updated_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS query_run (
            query_id VARCHAR PRIMARY KEY, label VARCHAR, result_type VARCHAR, query_file VARCHAR,
            index_node VARCHAR, time_filter_start TIMESTAMP, time_filter_stop TIMESTAMP,
            time_filter_method VARCHAR, created_at TIMESTAMP, package_version VARCHAR
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS esg_query (
            query_id VARCHAR PRIMARY KEY, label VARCHAR, index_node VARCHAR, query_file VARCHAR,
            parameter_json VARCHAR, tracked BOOLEAN, created_at TIMESTAMP, updated_at TIMESTAMP,
            last_checked_at TIMESTAMP, package_version VARCHAR
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS esg_file (
            file_key VARCHAR PRIMARY KEY, esgf_id VARCHAR, dataset_id VARCHAR, master_id VARCHAR,
            instance_id VARCHAR, tracking_id VARCHAR, version VARCHAR, title VARCHAR,
            filename VARCHAR, checksum VARCHAR, checksum_type VARCHAR, size DOUBLE,
            latest BOOLEAN, replica BOOLEAN, retracted BOOLEAN, deprecated BOOLEAN,
            data_node VARCHAR, activity_id VARCHAR, institution_id VARCHAR, source_id VARCHAR,
            experiment_id VARCHAR, variant_label VARCHAR, frequency VARCHAR, table_id VARCHAR,
            variable_id VARCHAR, grid_label VARCHAR, datetime_start TIMESTAMP, datetime_end TIMESTAMP,
            url_opendap VARCHAR, url_download VARCHAR, local_path VARCHAR, local_artifact_id VARCHAR,
            created_at TIMESTAMP, updated_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS esg_query_file (
            link_id VARCHAR PRIMARY KEY, query_id VARCHAR, file_key VARCHAR,
            status VARCHAR, first_seen_at TIMESTAMP, last_seen_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS esg_query_update (
            update_id VARCHAR PRIMARY KEY, query_id VARCHAR, status VARCHAR,
            started_at TIMESTAMP, completed_at TIMESTAMP, fields VARCHAR,
            all_files BOOLEAN, limit_files BOOLEAN, file_total INTEGER,
            current_count INTEGER, new_count INTEGER, stale_count INTEGER,
            changed_count INTEGER, deprecated_count INTEGER, retracted_count INTEGER,
            version_changed_count INTEGER, download_session_id VARCHAR, last_error VARCHAR
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS esg_query_update_file (
            update_file_id VARCHAR PRIMARY KEY, update_id VARCHAR, query_id VARCHAR, file_key VARCHAR,
            change_type VARCHAR, previous_status VARCHAR, current_status VARCHAR,
            previous_version VARCHAR, current_version VARCHAR, previous_checksum VARCHAR,
            current_checksum VARCHAR, previous_size DOUBLE, current_size DOUBLE,
            version_changed BOOLEAN, checksum_changed BOOLEAN, size_changed BOOLEAN,
            url_changed BOOLEAN, data_node_changed BOOLEAN, deprecated BOOLEAN,
            retracted BOOLEAN, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS esg_query_tag (
            tag_id VARCHAR PRIMARY KEY, query_id VARCHAR, tag VARCHAR, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS esg_query_dependency (
            dependency_id VARCHAR PRIMARY KEY, query_id VARCHAR, parent_query_id VARCHAR, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS file_catalog (
            file_key VARCHAR PRIMARY KEY, query_id VARCHAR, esgf_id VARCHAR, dataset_id VARCHAR,
            master_id VARCHAR, instance_id VARCHAR, version VARCHAR, title VARCHAR,
            filename VARCHAR, tracking_id VARCHAR, checksum VARCHAR, checksum_type VARCHAR,
            size DOUBLE, latest BOOLEAN, replica BOOLEAN, retracted BOOLEAN, deprecated BOOLEAN,
            data_node VARCHAR, activity_id VARCHAR, institution_id VARCHAR, source_id VARCHAR,
            experiment_id VARCHAR, variant_label VARCHAR, frequency VARCHAR, table_id VARCHAR,
            variable_id VARCHAR, grid_label VARCHAR, datetime_start TIMESTAMP, datetime_end TIMESTAMP,
            actual_time_start TIMESTAMP, actual_time_end TIMESTAMP, url_opendap VARCHAR,
            url_download VARCHAR, local_path VARCHAR, local_artifact_id VARCHAR, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS extraction_plan (
            plan_id VARCHAR PRIMARY KEY, query_id VARCHAR, file_key VARCHAR, site_id VARCHAR,
            variable_id VARCHAR, lon DOUBLE, lat DOUBLE, method VARCHAR,
            time_start TIMESTAMP, time_stop TIMESTAMP, status VARCHAR, available_time_count INTEGER,
            attempt_count INTEGER, last_error VARCHAR, created_at TIMESTAMP, updated_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS extraction_grid_source (
            source_row_id VARCHAR PRIMARY KEY, plan_id VARCHAR, file_key VARCHAR, query_id VARCHAR,
            variable_id VARCHAR, method VARCHAR, source_index INTEGER, role VARCHAR,
            grid_lon DOUBLE, grid_lat DOUBLE, grid_elevation_m DOUBLE, grid_dist_km DOUBLE,
            weight DOUBLE, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS extraction_result (
            result_id VARCHAR PRIMARY KEY, plan_id VARCHAR, file_key VARCHAR, query_id VARCHAR,
            artifact_id VARCHAR, output_path VARCHAR, year INTEGER, row_count INTEGER,
            unique_time_count INTEGER, time_min TIMESTAMP, time_max TIMESTAMP,
            lon_actual DOUBLE, lat_actual DOUBLE, completed_at TIMESTAMP
        )"
    )
    store_catalog__add_columns(
        exec,
        list(
            esg_file = "deprecated BOOLEAN",
            file_catalog = c(
                "latest BOOLEAN",
                "replica BOOLEAN",
                "retracted BOOLEAN",
                "deprecated BOOLEAN"
            ),
            extraction_grid_source = "grid_elevation_m DOUBLE"
        )
    )
    store_catalog__init_epw_morph_schema(exec)
    store_catalog__init_shift_run_schema(exec)
    # Create every queued table and column before the version row is
    # written and before any caller can read them.
    schema$flush()
    if (is.na(current_version)) {
        private$replace_rows(
            "store_meta",
            data.frame(
                key = "schema_version",
                value = STORE_SCHEMA_VERSION,
                updated_at = store__now(),
                stringsAsFactors = FALSE
            ),
            "key"
        )
    }
    invisible(NULL)
}
# }}}

# Reject old manifests before any current-schema tables are created.
# This deliberately replaces the former best-effort migration path.
# store_catalog__assert_schema_version {{{
store_catalog__assert_schema_version <- function(private, existing_tables) {
    # A manifest with no tables was just created, so its store_meta is
    # empty and holds no version; there is nothing to read or reject.
    if (!length(existing_tables)) {
        return(invisible(NA_character_))
    }
    meta <- tryCatch(
        private$read_table("store_meta"),
        error = function(e) data.table::data.table()
    )
    row <- if (nrow(meta)) {
        meta[meta[["key"]] == "schema_version"]
    } else {
        meta
    }
    current <- if (nrow(row)) {
        v <- as.character(row$value[[1L]])
        if (is.na(v) || !nzchar(v)) NA_character_ else v
    } else {
        NA_character_
    }
    if (is.na(current) || !identical(current, STORE_SCHEMA_VERSION)) {
        shown <- if (is.na(current)) "missing" else current
        reset_command <- store__reset_command(private$store_path)
        private$disconnect()
        cli::cli_abort(c(
            "Store schema version {.val {shown}} does not match the required version {.val {STORE_SCHEMA_VERSION}}.",
            "i" = "Use a different {.arg store} path, or rebuild this one with {.code {reset_command}}.",
            "i" = "The reset helper preserves the previous store as a backup by default."
        ))
    }
    # Return the version read here so init_schema() need not read it again.
    invisible(current)
}
# }}}

# store_catalog__init_epw_morph_schema {{{
store_catalog__init_epw_morph_schema <- function(exec) {
    exec(
        "CREATE TABLE IF NOT EXISTS epw_source (
            epw_id VARCHAR PRIMARY KEY, artifact_id VARCHAR, label VARCHAR, site_id VARCHAR,
            path VARCHAR, checksum VARCHAR, created_at TIMESTAMP, updated_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_baseline_summary (
            baseline_row_id VARCHAR PRIMARY KEY, baseline_id VARCHAR, epw_id VARCHAR,
            epw_field VARCHAR, month INTEGER, stat VARCHAR, value DOUBLE,
            units VARCHAR, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_climate_summary (
            summary_row_id VARCHAR PRIMARY KEY, summary_id VARCHAR, plan_id VARCHAR,
            site_id VARCHAR, source_id VARCHAR, experiment_id VARCHAR, variant_label VARCHAR,
            frequency VARCHAR, table_id VARCHAR, variable_id VARCHAR, period VARCHAR,
            month INTEGER, stat VARCHAR, value DOUBLE, units VARCHAR, lon DOUBLE, lat DOUBLE,
            years_json VARCHAR, coverage DOUBLE, n_records INTEGER, n_valid INTEGER,
            created_at TIMESTAMP
        )"
    )
    # A summary can combine several extraction files. Keep their plan
    # lineage normalized so source fragments do not divide statistics.
    exec(
        "CREATE TABLE IF NOT EXISTS epw_climate_summary_plan (
            summary_plan_id VARCHAR PRIMARY KEY, summary_id VARCHAR, plan_id VARCHAR, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_morph_plan (
            morph_id VARCHAR PRIMARY KEY, epw_id VARCHAR, summary_id VARCHAR,
            reference_summary_id VARCHAR, baseline_id VARCHAR, label VARCHAR,
            by_json VARCHAR, recipe_json VARCHAR, strict BOOLEAN, status VARCHAR,
            created_at TIMESTAMP, updated_at TIMESTAMP, last_error VARCHAR
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_morph_observed_reference (
            morph_id VARCHAR PRIMARY KEY, observed_summary_id VARCHAR, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_morph_factor (
            factor_id VARCHAR PRIMARY KEY, morph_id VARCHAR, case_id VARCHAR, epw_field VARCHAR,
            variable_id VARCHAR, source_id VARCHAR, experiment_id VARCHAR, variant_label VARCHAR,
            period VARCHAR, month INTEGER, method VARCHAR, baseline DOUBLE, reference DOUBLE,
            future DOUBLE, delta DOUBLE, alpha DOUBLE, units VARCHAR, status VARCHAR
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_morph_case (
            morph_case_id VARCHAR PRIMARY KEY, morph_id VARCHAR, case_id VARCHAR,
            source_id VARCHAR, experiment_id VARCHAR, variant_label VARCHAR, period VARCHAR,
            status VARCHAR, error_class VARCHAR, last_error VARCHAR, updated_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_morph_diagnostic (
            diagnostic_id VARCHAR PRIMARY KEY, stage VARCHAR, severity VARCHAR, code VARCHAR,
            message VARCHAR, plan_id VARCHAR, summary_id VARCHAR, baseline_id VARCHAR,
            morph_id VARCHAR, case_id VARCHAR, variable_id VARCHAR, epw_field VARCHAR,
            period VARCHAR, month INTEGER, action VARCHAR
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_morph_result (
            result_id VARCHAR PRIMARY KEY, morph_id VARCHAR, case_id VARCHAR, artifact_id VARCHAR,
            output_path VARCHAR, row_count INTEGER, output_type VARCHAR, sequence_id VARCHAR,
            weather_year INTEGER, calendar VARCHAR, stochastic_seed INTEGER, member_count INTEGER,
            provenance_json VARCHAR, created_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS epw_output (
            output_id VARCHAR PRIMARY KEY, morph_id VARCHAR, case_id VARCHAR, result_id VARCHAR,
            artifact_id VARCHAR, path VARCHAR, source_id VARCHAR, experiment_id VARCHAR,
            variant_label VARCHAR, period VARCHAR, output_type VARCHAR, sequence_id VARCHAR,
            weather_year INTEGER, calendar VARCHAR, stochastic_seed INTEGER, member_count INTEGER,
            provenance_json VARCHAR, created_at TIMESTAMP
        )"
    )
    # Existing stores may predate valid-row pooling or year-addressable
    # result/output columns; add each nullable column idempotently.
    year_cols <- c(
        "output_type VARCHAR",
        "sequence_id VARCHAR",
        "weather_year INTEGER",
        "calendar VARCHAR",
        "stochastic_seed INTEGER",
        "member_count INTEGER",
        "provenance_json VARCHAR"
    )
    store_catalog__add_columns(
        exec,
        list(
            epw_climate_summary = "n_valid INTEGER",
            epw_morph_result = year_cols,
            epw_output = c("result_id VARCHAR", year_cols)
        )
    )
    invisible(NULL)
}
# }}}

# Persist task intent, resolved inputs, case fulfilment, and stage
# events so a failed workflow can be inspected and resumed later.
# store_catalog__init_shift_run_schema {{{
store_catalog__init_shift_run_schema <- function(exec) {
    exec(
        "CREATE TABLE IF NOT EXISTS shift_run (
            run_id VARCHAR PRIMARY KEY, task VARCHAR, spec_hash VARCHAR, spec_json VARCHAR,
            resolved_spec_json VARCHAR, status VARCHAR, current_stage VARCHAR, query_id VARCHAR,
            reference_query_id VARCHAR, plan_ids_json VARCHAR, reference_plan_ids_json VARCHAR,
            morph_id VARCHAR, output_dir VARCHAR, package_version VARCHAR,
            started_at TIMESTAMP, updated_at TIMESTAMP, completed_at TIMESTAMP, last_error VARCHAR
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS shift_run_case (
            run_case_id VARCHAR PRIMARY KEY, run_id VARCHAR, case_id VARCHAR, source_id VARCHAR,
            experiment_id VARCHAR, variant_label VARCHAR, grid_label VARCHAR, period VARCHAR,
            years_json VARCHAR, required BOOLEAN, status VARCHAR, output_id VARCHAR,
            export_path VARCHAR, missing_reason VARCHAR, updated_at TIMESTAMP
        )"
    )
    exec(
        "CREATE TABLE IF NOT EXISTS shift_run_event (
            event_id VARCHAR PRIMARY KEY, run_id VARCHAR, step_id VARCHAR, stage VARCHAR,
            status VARCHAR, message VARCHAR, details_json VARCHAR, created_at TIMESTAMP
        )"
    )
    # A run can have multiple foreground/background attempts. Keeping
    # jobs separate preserves process and log history across resume.
    exec(
        "CREATE TABLE IF NOT EXISTS shift_run_job (
            job_id VARCHAR PRIMARY KEY, run_id VARCHAR, step_id VARCHAR, attempt INTEGER,
            mode VARCHAR, status VARCHAR, pid INTEGER, hostname VARCHAR, log_path VARCHAR,
            ui_json VARCHAR, cancel_requested_at TIMESTAMP, started_at TIMESTAMP,
            heartbeat_at TIMESTAMP, completed_at TIMESTAMP, exit_code INTEGER,
            last_error VARCHAR, created_at TIMESTAMP, updated_at TIMESTAMP
        )"
    )
    # A step records one independently inspectable stage invocation.
    # The returned stage carries run/step identity into the next call;
    # stale or terminal inputs fork a child run instead of mutating it.
    exec(
        "CREATE TABLE IF NOT EXISTS shift_run_step (
            step_id VARCHAR PRIMARY KEY, run_id VARCHAR, ordinal INTEGER, task VARCHAR,
            spec_hash VARCHAR, spec_json VARCHAR, input_stage_json VARCHAR, output_stage_json VARCHAR,
            status VARCHAR, resumable BOOLEAN, nonresumable_reason VARCHAR,
            started_at TIMESTAMP, updated_at TIMESTAMP, completed_at TIMESTAMP, last_error VARCHAR
        )"
    )
    invisible(NULL)
}
# }}}

# Read one explicitly requested artifact set with a single manifest query.
# NULL preserves the complete-table behavior; an empty selection still keeps
# the manifest schema, and missing IDs fail before any artifact file is read.
# store_catalog__artifact_rows {{{
store_catalog__artifact_rows <- function(conn, artifact_id = NULL) {
    if (is.null(artifact_id)) {
        return(ddb_read_table(conn, "artifact"))
    }
    checkmate::assert_character(
        artifact_id,
        any.missing = FALSE,
        min.chars = 1L
    )
    ids <- unique(artifact_id)
    predicate <- if (length(ids)) {
        paste0(
            "artifact_id IN (",
            paste(ddb_literal(conn, ids), collapse = ","),
            ")"
        )
    } else {
        "FALSE"
    }
    rows <- ddb_query(conn, paste0("SELECT * FROM artifact WHERE ", predicate))
    missing <- setdiff(ids, rows$artifact_id)
    if (length(missing)) {
        cli::cli_abort(
            "Artifact ID(s) {.val {missing}} were not found in the store manifest."
        )
    }
    rows[match(ids, rows$artifact_id), , drop = FALSE]
}
# }}}

# store_catalog__register_artifact {{{
store_catalog__register_artifact <- function(
    self,
    private,
    kind,
    path,
    role = NULL,
    project = NULL,
    status = "available",
    checksum = NULL,
    checksum_type = "sha256",
    size = NULL,
    query_id = NULL,
    file_key = NULL,
    dict_id = NULL,
    source_url = NULL,
    source_repo = NULL,
    source_tag = NULL,
    source_commit = NULL,
    metadata = list()
) {
    private$check_open()
    checkmate::assert_choice(
        kind,
        c(
            "query",
            "dict",
            "source",
            "cmip6_index",
            "netcdf",
            "extract",
            "output"
        )
    )
    checkmate::assert_string(path, min.chars = 1L)
    checkmate::assert_choice(
        status,
        c("planned", "available", "failed", "missing")
    )
    checkmate::assert_choice(checksum_type, c("md5", "sha256"))
    checkmate::assert_list(metadata, null.ok = TRUE)

    private$with_store_lock({
        path <- store_abs_path(path, root = private$store_path)
        rel_path <- store_rel_path(path, root = private$store_path)
        exists <- file.exists(path)
        if (is.null(role)) {
            role <- switch(
                kind,
                query = "input",
                dict = "input",
                source = "source",
                cmip6_index = "input",
                netcdf = "download",
                extract = "derived",
                output = "output"
            )
        }
        checkmate::assert_choice(
            role,
            c("input", "source", "download", "derived", "output")
        )

        if (is.null(size) && exists) {
            size <- as.numeric(file.info(path, extra_cols = FALSE)$size)
        }
        if (
            is.null(checksum) &&
                exists &&
                identical(status, "available")
        ) {
            checksum <- store_hash_file(path, checksum_type)
        }
        if (is.null(size)) {
            size <- NA_real_
        }
        if (is.null(checksum)) {
            checksum <- NA_character_
        }
        if (is.null(metadata)) {
            metadata <- list()
        }

        artifact_id <- store__hash(
            kind,
            rel_path,
            checksum,
            file_key,
            dict_id,
            source_commit
        )
        now <- store__now()
        row <- data.frame(
            artifact_id = artifact_id,
            kind = kind,
            role = role,
            project = store__chr1(project),
            relative_path = rel_path,
            checksum = store__chr1(checksum),
            checksum_type = checksum_type,
            size = as.numeric(size),
            status = status,
            query_id = store__chr1(query_id),
            file_key = store__chr1(file_key),
            dict_id = store__chr1(dict_id),
            source_url = store__chr1(source_url),
            source_repo = store__chr1(source_repo),
            source_tag = store__chr1(source_tag),
            source_commit = store__chr1(source_commit),
            metadata_json = jsonlite::toJSON(
                metadata,
                auto_unbox = TRUE,
                null = "null"
            ),
            created_at = now,
            updated_at = now,
            stringsAsFactors = FALSE
        )
        private$replace_rows("artifact", row, "artifact_id")
        artifact_id
    })
}
# }}}

# store_catalog__artifact_path {{{
store_catalog__artifact_path <- function(self, private, artifact_id) {
    private$check_open()
    checkmate::assert_character(
        artifact_id,
        any.missing = FALSE,
        min.chars = 1L
    )
    rows <- store_catalog__artifact_rows(private$conn, artifact_id)
    paths <- rows$relative_path[match(artifact_id, rows$artifact_id)]
    vapply(
        paths,
        store_abs_path,
        character(1L),
        root = private$store_path,
        USE.NAMES = FALSE
    )
}
# }}}

# store_catalog__validate {{{
store_catalog__validate <- function(self, private, artifact_id = NULL) {
    private$check_open()
    artifacts <- data.table::as.data.table(store_catalog__artifact_rows(
        private$conn,
        artifact_id
    ))
    if (!nrow(artifacts)) {
        return(data.table::data.table(
            artifact_id = character(),
            kind = character(),
            expected_path = character(),
            exists = logical(),
            checksum_ok = logical(),
            size_ok = logical(),
            status = character()
        ))
    }

    data.table::set(
        artifacts,
        j = "expected_path",
        value = vapply(
            artifacts$relative_path,
            store_abs_path,
            character(1L),
            root = private$store_path,
            USE.NAMES = FALSE
        )
    )
    data.table::set(
        artifacts,
        j = "exists",
        value = file.exists(artifacts$expected_path)
    )
    data.table::set(
        artifacts,
        j = "checksum_ok",
        value = mapply(
            function(path, ok, checksum, checksum_type) {
                if (!isTRUE(ok) || is.na(checksum) || !nzchar(checksum)) {
                    return(NA)
                }
                identical(
                    tolower(store_hash_file(path, checksum_type)),
                    tolower(checksum)
                )
            },
            artifacts$expected_path,
            artifacts$exists,
            artifacts$checksum,
            artifacts$checksum_type,
            USE.NAMES = FALSE
        )
    )
    data.table::set(
        artifacts,
        j = "size_ok",
        value = mapply(
            function(path, ok, size) {
                if (!isTRUE(ok) || is.na(size)) {
                    return(NA)
                }
                identical(
                    as.numeric(file.info(path, extra_cols = FALSE)$size),
                    as.numeric(size)
                )
            },
            artifacts$expected_path,
            artifacts$exists,
            artifacts$size,
            USE.NAMES = FALSE
        )
    )
    artifacts[,
        .SD,
        .SDcols = c(
            "artifact_id",
            "kind",
            "expected_path",
            "exists",
            "checksum_ok",
            "size_ok",
            "status"
        )
    ]
}
# }}}

# store_catalog__add_query {{{
store_catalog__add_query <- function(
    self,
    private,
    query,
    label = NULL,
    track = FALSE
) {
    private$check_open()
    if (!inherits(query, "EsgQuery")) {
        cli::cli_abort("`query` must be an {.cls EsgQuery} object.")
    }
    checkmate::assert_string(label, null.ok = TRUE)
    checkmate::assert_flag(track)

    private$with_store_lock({
        payload <- store_catalog__query_payload(query)
        query_id <- payload$query_id
        qid <- query_id
        query_file <- file.path(
            private$query_dir,
            sprintf("query-%s.json", query_id)
        )
        query$save(query_file)

        now <- store__now()
        queries <- private$read_table("esg_query")
        existing <- queries[queries[["query_id"]] == qid]
        created_at <- if (nrow(existing)) {
            existing$created_at[[1L]]
        } else {
            now
        }
        tracked <- if (nrow(existing)) {
            store__flag(existing$tracked[[1L]]) || isTRUE(track)
        } else {
            isTRUE(track)
        }
        if (is.null(label) && nrow(existing)) {
            label <- store__chr1(existing$label[[1L]])
            if (is.na(label)) label <- NULL
        }

        row <- data.frame(
            query_id = query_id,
            label = store__chr1(label),
            index_node = payload$index_node,
            query_file = store_rel_path(query_file, private$store_path),
            parameter_json = payload$parameter_json,
            tracked = tracked,
            created_at = created_at,
            updated_at = now,
            last_checked_at = as.POSIXct(NA),
            package_version = as.character(utils::packageVersion("epwshiftr")),
            stringsAsFactors = FALSE
        )
        private$replace_rows("esg_query", row, "query_id")
        self$register_artifact(
            kind = "query",
            path = query_file,
            role = "input",
            project = "CMIP6",
            query_id = query_id,
            metadata = list(result_type = "EsgQuery")
        )
        query_id
    })
}
# }}}

# store_catalog__tag_query {{{
store_catalog__tag_query <- function(
    self,
    private,
    query_id,
    tag,
    replace = FALSE
) {
    private$check_open()
    checkmate::assert_string(query_id, min.chars = 1L)
    checkmate::assert_character(
        tag,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    checkmate::assert_flag(replace)
    private$with_store_lock({
        store_catalog__get_query_row(private, query_id)
        if (isTRUE(replace)) {
            private$delete_by_key("esg_query_tag", "query_id", query_id)
        }
        now <- store__now()
        rows <- data.frame(
            tag_id = vapply(
                tag,
                function(value) store__hash(query_id, value),
                character(1L)
            ),
            query_id = query_id,
            tag = tag,
            created_at = now,
            stringsAsFactors = FALSE
        )
        private$append_new_rows("esg_query_tag", rows, "tag_id")
        self$query_tags(query_id)
    })
}
# }}}

# store_catalog__untag_query {{{
store_catalog__untag_query <- function(self, private, query_id, tag = NULL) {
    store_catalog__delete_query_links(
        private,
        "esg_query_tag",
        "tag_id",
        query_id,
        "tag",
        tag
    )
    self$query_tags(query_id)
}
# }}}

# store_catalog__query_tags {{{
store_catalog__query_tags <- function(self, private, query_id = NULL) {
    private$check_open()
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    tags <- private$read_table("esg_query_tag")
    if (!is.null(query_id) && nrow(tags)) {
        wanted_query_id <- query_id
        tags <- tags[tags[["query_id"]] %in% wanted_query_id]
    }
    tags[]
}
# }}}

# store_catalog__require_query {{{
store_catalog__require_query <- function(
    self,
    private,
    query_id,
    parent_query_id
) {
    private$check_open()
    checkmate::assert_string(query_id, min.chars = 1L)
    checkmate::assert_string(parent_query_id, min.chars = 1L)
    if (identical(query_id, parent_query_id)) {
        cli::cli_abort("A query cannot require itself.")
    }
    private$with_store_lock({
        store_catalog__get_query_row(private, query_id)
        store_catalog__get_query_row(private, parent_query_id)
        now <- store__now()
        row <- data.frame(
            dependency_id = store__hash(query_id, parent_query_id),
            query_id = query_id,
            parent_query_id = parent_query_id,
            created_at = now,
            stringsAsFactors = FALSE
        )
        private$append_new_rows(
            "esg_query_dependency",
            row,
            "dependency_id"
        )
        self$query_graph(
            query_id = query_id,
            direction = "parents",
            recursive = FALSE
        )
    })
}
# }}}

# store_catalog__unrequire_query {{{
store_catalog__unrequire_query <- function(
    self,
    private,
    query_id,
    parent_query_id = NULL
) {
    store_catalog__delete_query_links(
        private,
        "esg_query_dependency",
        "dependency_id",
        query_id,
        "parent_query_id",
        parent_query_id
    )
    self$query_graph(
        query_id = query_id,
        direction = "parents",
        recursive = FALSE
    )
}
# }}}

# store_catalog__query_graph {{{
store_catalog__query_graph <- function(
    self,
    private,
    query_id = NULL,
    direction = c("children", "parents", "both"),
    recursive = TRUE
) {
    private$check_open()
    checkmate::assert_string(query_id, null.ok = TRUE)
    checkmate::assert_flag(recursive)
    direction <- match.arg(direction)
    edges <- private$read_table("esg_query_dependency")
    if (is.null(query_id) || !nrow(edges)) {
        return(edges[])
    }
    ids <- store_catalog__query_related_ids(
        private,
        query_id,
        direction = direction,
        recursive = recursive
    )
    edges[
        edges[["query_id"]] %in%
            ids |
            edges[["parent_query_id"]] %in% ids
    ][]
}
# }}}

# store_catalog__queries {{{
store_catalog__queries <- function(self, private, tracked = NULL) {
    private$check_open()
    checkmate::assert_flag(tracked, null.ok = TRUE)
    queries <- private$read_table("esg_query")
    if (!is.null(tracked) && nrow(queries)) {
        want_tracked <- isTRUE(tracked)
        queries <- queries[as.logical(queries[["tracked"]]) == want_tracked]
    }
    queries[]
}
# }}}

# store_catalog__query_files {{{
store_catalog__query_files <- function(self, private, query_id, status = NULL) {
    private$check_open()
    checkmate::assert_string(query_id, min.chars = 1L)
    checkmate::assert_character(
        status,
        any.missing = FALSE,
        unique = TRUE,
        null.ok = TRUE
    )
    store_catalog__get_query_row(private, query_id)

    qid <- query_id
    links <- private$read_table("esg_query_file")
    links <- links[links[["query_id"]] == qid]
    if (!is.null(status) && nrow(links)) {
        wanted_status <- status
        links <- links[links[["status"]] %in% wanted_status]
    }
    if (!nrow(links)) {
        return(data.table::data.table())
    }

    files <- private$read_table("esg_file")
    out <- merge(
        links,
        files,
        by = "file_key",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setcolorder(
        out,
        c(
            "query_id",
            "file_key",
            "status",
            setdiff(names(out), c("query_id", "file_key", "status"))
        )
    )
    out[]
}
# }}}

# store_catalog__preview_update_queries {{{
store_catalog__preview_update_queries <- function(
    self,
    private,
    query_id = NULL,
    tracked = TRUE,
    tag = NULL,
    children = FALSE,
    detail = FALSE,
    all = TRUE,
    limit = FALSE,
    fields = "*",
    ...
) {
    private$check_open()
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_flag(tracked, null.ok = TRUE)
    checkmate::assert_character(
        tag,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_flag(children)
    checkmate::assert_flag(detail)

    rows <- store_catalog__select_query_rows(
        private,
        query_id = store_catalog__resolve_query_selection(
            private,
            query_id = query_id,
            tag = tag,
            children = children
        ),
        tracked = if (is.null(query_id)) tracked else NULL
    )
    if (!nrow(rows)) {
        empty <- data.table::data.table()
        if (isTRUE(detail)) {
            return(list(summary = empty, changes = empty))
        }
        return(empty)
    }

    previews <- vector("list", nrow(rows))
    for (i in seq_len(nrow(rows))) {
        query <- store_catalog__load_query(private, rows[i])
        files <- query$collect(
            type = "File",
            fields = fields,
            all = all,
            limit = limit,
            ...
        )
        previews[[i]] <- store_catalog__preview_query_update(
            private,
            row = rows[i],
            files = files
        )
    }

    summary <- data.table::rbindlist(
        lapply(previews, `[[`, "summary"),
        fill = TRUE
    )
    if (!isTRUE(detail)) {
        return(summary[])
    }
    changes <- data.table::rbindlist(
        lapply(previews, `[[`, "changes"),
        fill = TRUE
    )
    list(summary = summary[], changes = changes[])
}
# }}}

# store_catalog__update_queries {{{
store_catalog__update_queries <- function(
    self,
    private,
    query_id = NULL,
    tracked = TRUE,
    tag = NULL,
    children = FALSE,
    enqueue = FALSE,
    downloader = NULL,
    replica = "auto",
    session_label = NULL,
    service = "HTTPServer",
    probe = TRUE,
    probe_concurrency = NULL,
    probe_cache_seconds = 3600L,
    strategy = c("fastest", "first", "stable"),
    all = TRUE,
    limit = FALSE,
    fields = "*",
    ...
) {
    private$check_open()
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_flag(tracked, null.ok = TRUE)
    checkmate::assert_character(
        tag,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_flag(children)
    checkmate::assert_flag(enqueue)
    strategy <- match.arg(strategy)
    if (isTRUE(enqueue) && is.null(downloader)) {
        downloader <- self$downloader()
    }
    rows <- store_catalog__select_query_rows(
        private,
        query_id = store_catalog__resolve_query_selection(
            private,
            query_id = query_id,
            tag = tag,
            children = children
        ),
        tracked = if (is.null(query_id)) tracked else NULL
    )
    if (!nrow(rows)) {
        return(data.table::data.table())
    }

    updated <- vector("list", nrow(rows))
    for (i in seq_len(nrow(rows))) {
        # Network collection can be slow; keep it outside the store lock.
        query <- store_catalog__load_query(private, rows[i])
        files <- query$collect(
            type = "File",
            fields = fields,
            all = all,
            limit = limit,
            ...
        )

        # Only the manifest mutation needs the store lock.
        updated[[i]] <- private$with_store_lock({
            private$update_query_files(
                rows$query_id[[i]],
                files,
                fields = fields,
                all = all,
                limit = limit
            )
        })
        if (isTRUE(enqueue)) {
            # Build and enqueue downloader work without holding the store lock.
            sid <- store_download__enqueue_query(
                self,
                private,
                query_id = rows$query_id[[i]],
                files = files,
                current = updated[[i]][
                    updated[[i]][["status"]] %in% "current",
                    ,
                    drop = FALSE
                ],
                downloader = downloader,
                replica = replica,
                session_label = session_label,
                service = service,
                probe = probe,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds,
                strategy = strategy,
                error_if_empty = FALSE
            )
            if (nrow(updated[[i]])) {
                updated[[i]]$download_session_id <- sid
                # Persist the session link with a short manifest write.
                private$with_store_lock({
                    store_catalog__set_query_update_session(
                        private,
                        updated[[i]]$update_id[[1L]],
                        sid
                    )
                })
            }
        }
    }
    data.table::rbindlist(updated, fill = TRUE)
}
# }}}

# store_catalog__query_status {{{
store_catalog__query_status <- function(
    self,
    private,
    query_id = NULL,
    downloader = NULL
) {
    private$check_open()
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    if (is.null(downloader)) {
        downloader <- self$downloader()
    }

    queries <- store_catalog__select_query_rows(private, query_id = query_id)
    if (!is.null(query_id)) {
        missing <- setdiff(query_id, queries$query_id)
        if (length(missing)) {
            cli::cli_abort(
                "Stored ESGF query ID(s) not found: {.val {missing}}."
            )
        }
    }
    if (!nrow(queries)) {
        return(data.table::data.table())
    }

    links <- private$read_table("esg_query_file")
    files <- private$read_table("esg_file")
    # A failed manifest read is not evidence that there are no tasks.
    tasks <- data.table::as.data.table(downloader$tasks())
    store_catalog__summarise_query_status(private, queries, links, files, tasks)
}
# }}}

# store_catalog__query_updates {{{
store_catalog__query_updates <- function(
    self,
    private,
    query_id = NULL,
    latest = FALSE
) {
    private$check_open()
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_flag(latest)
    updates <- private$read_table("esg_query_update")
    if (!is.null(query_id) && nrow(updates)) {
        wanted_query_id <- query_id
        updates <- updates[updates[["query_id"]] %in% wanted_query_id]
    }
    if (isTRUE(latest) && nrow(updates)) {
        data.table::setorderv(
            updates,
            c("query_id", "completed_at", "started_at")
        )
        updates <- updates[, .SD[.N], by = "query_id"]
    }
    updates[]
}
# }}}

# store_catalog__query_changes {{{
store_catalog__query_changes <- function(
    self,
    private,
    update_id = NULL,
    query_id = NULL,
    change_type = NULL
) {
    private$check_open()
    checkmate::assert_character(
        update_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_subset(
        change_type,
        c("new", "current", "stale", "changed"),
        empty.ok = TRUE
    )
    changes <- private$read_table("esg_query_update_file")
    if (!is.null(update_id) && nrow(changes)) {
        wanted_update_id <- update_id
        changes <- changes[changes[["update_id"]] %in% wanted_update_id]
    }
    if (!is.null(query_id) && nrow(changes)) {
        wanted_query_id <- query_id
        changes <- changes[changes[["query_id"]] %in% wanted_query_id]
    }
    if (!is.null(change_type) && nrow(changes)) {
        wanted_change_type <- change_type
        changes <- changes[
            changes[["change_type"]] %in% wanted_change_type
        ]
    }
    changes[]
}
# }}}

# store_catalog__workflow_status {{{
store_catalog__workflow_status <- function(
    self,
    private,
    query_id = NULL,
    downloader = NULL
) {
    private$check_open()
    status <- self$query_status(
        query_id = query_id,
        downloader = downloader
    )
    if (!nrow(status)) {
        return(status)
    }
    updates <- self$query_updates(
        query_id = status$query_id,
        latest = TRUE
    )
    if (nrow(updates)) {
        update_cols <- c(
            "query_id",
            "update_id",
            "status",
            "started_at",
            "completed_at",
            "new_count",
            "stale_count",
            "changed_count",
            "deprecated_count",
            "retracted_count",
            "version_changed_count",
            "download_session_id"
        )
        updates <- updates[,
            intersect(update_cols, names(updates)),
            with = FALSE
        ]
        rename_cols <- intersect(
            c("status", "started_at", "completed_at"),
            names(updates)
        )
        data.table::setnames(
            updates,
            rename_cols,
            paste0("last_update_", rename_cols)
        )
        status <- merge(
            status,
            updates,
            by = "query_id",
            all.x = TRUE,
            sort = FALSE
        )
    }
    # query_status() owns these numeric columns, including zero counts
    # when no tasks exist. Only update/session metadata is optional.
    data.table::set(
        status,
        j = "bytes_missing",
        value = pmax(0, status$bytes_total - status$bytes_done)
    )
    data.table::set(
        status,
        j = "download_retryable",
        value = rowSums(status[,
            c("download_error", "download_cancelled"),
            with = FALSE
        ])
    )
    data.table::set(
        status,
        j = "download_incomplete",
        value = status$download_retryable +
            status$download_queued +
            status$download_downloading >
            0 |
            status$local_available < status$file_current
    )
    data.table::set(
        status,
        j = "last_download_session_id",
        value = if ("download_session_id" %in% names(status)) {
            status$download_session_id
        } else {
            rep(NA_character_, nrow(status))
        }
    )

    tags <- self$query_tags(query_id = status$query_id)
    if (nrow(tags)) {
        tag_summary <- tags[,
            list(tags = paste(sort(unique(.SD[["tag"]])), collapse = ",")),
            by = "query_id"
        ]
        status <- merge(
            status,
            tag_summary,
            by = "query_id",
            all.x = TRUE,
            sort = FALSE
        )
    }

    plans <- private$read_table("extraction_plan")
    if (nrow(plans)) {
        plan_counts <- plans[
            plans[["query_id"]] %in% status$query_id,
            .N,
            by = c("query_id", "status")
        ]
        plan_wide <- data.table::dcast(
            plan_counts,
            query_id ~ status,
            value.var = "N",
            fill = 0L
        )
        plan_status_cols <- setdiff(names(plan_wide), "query_id")
        data.table::setnames(
            plan_wide,
            plan_status_cols,
            paste0("extract_", plan_status_cols)
        )
        status <- merge(
            status,
            plan_wide,
            by = "query_id",
            all.x = TRUE,
            sort = FALSE
        )
    }
    status[]
}
# }}}

# store_catalog__workflow_report {{{
store_catalog__workflow_report <- function(
    self,
    private,
    query_id = NULL,
    downloader = NULL
) {
    private$check_open()
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    if (is.null(downloader)) {
        downloader <- self$downloader()
    }
    summary <- self$workflow_status(
        query_id = query_id,
        downloader = downloader
    )
    if (!nrow(summary)) {
        empty <- data.table::data.table()
        return(list(
            summary = summary,
            updates = empty,
            changes = empty,
            downloads = empty,
            nodes = empty
        ))
    }

    updates <- self$query_updates(query_id = summary$query_id)
    latest_updates <- self$query_updates(
        query_id = summary$query_id,
        latest = TRUE
    )
    changes <- if (nrow(latest_updates)) {
        out <- self$query_changes(update_id = latest_updates$update_id)
        if (nrow(out)) {
            out <- out[out[["change_type"]] != "current"]
        }
        out[]
    } else {
        data.table::data.table()
    }
    downloads <- store_catalog__workflow_downloads(
        private,
        summary$query_id,
        downloader
    )
    nodes <- tryCatch(
        data.table::as.data.table(downloader$data_nodes()),
        error = function(e) data.table::data.table()
    )

    list(
        summary = summary[],
        updates = updates[],
        changes = changes[],
        downloads = downloads[],
        nodes = nodes[]
    )
}
# }}}

# store_catalog__remove_query {{{
store_catalog__remove_query <- function(
    self,
    private,
    query_id,
    delete = c("none", "orphaned")
) {
    private$check_open()
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    delete <- match.arg(delete)

    rows <- store_catalog__select_query_rows(private, query_id = query_id)
    missing <- setdiff(query_id, rows$query_id)
    if (length(missing)) {
        cli::cli_abort(
            "Stored ESGF query ID(s) not found: {.val {missing}}."
        )
    }

    private$with_store_lock({
        links <- private$read_table("esg_query_file")
        wanted_query_id <- query_id
        touched <- links[links[["query_id"]] %in% wanted_query_id]
        artifacts <- private$read_table("artifact")
        query_artifacts <- artifacts[
            artifacts[["query_id"]] %in%
                wanted_query_id &
                artifacts[["kind"]] == "query"
        ]

        private$delete_by_key("esg_query_file", "query_id", query_id)
        private$delete_by_key("esg_query_tag", "query_id", query_id)
        private$delete_by_key("esg_query_dependency", "query_id", query_id)
        private$delete_by_key(
            "esg_query_dependency",
            "parent_query_id",
            query_id
        )
        private$delete_by_key("esg_query", "query_id", query_id)
        if (nrow(query_artifacts)) {
            paths <- vapply(
                query_artifacts$relative_path,
                store_abs_path,
                character(1L),
                root = private$store_path
            )
            unlink(
                paths[file.exists(paths)],
                recursive = FALSE,
                force = TRUE
            )
            private$delete_by_key(
                "artifact",
                "artifact_id",
                query_artifacts$artifact_id
            )
        }

        remaining_links <- private$read_table("esg_query_file")
        orphan_keys <- setdiff(
            unique(touched$file_key),
            remaining_links$file_key
        )
        pruned <- if (identical(delete, "orphaned") && length(orphan_keys)) {
            store_catalog__remove_file_records(
                private,
                orphan_keys,
                delete_local = TRUE,
                force = TRUE
            )
        } else {
            orphans <- store_catalog__orphaned_files(private)
            orphans[orphans[["file_key"]] %in% orphan_keys]
        }

        out <- data.table::as.data.table(rows)
        data.table::set(
            out,
            j = "removed_file_links",
            value = vapply(
                out$query_id,
                function(id) sum(touched$query_id == id),
                integer(1L)
            )
        )
        data.table::set(out, j = "orphaned_file_count", value = nrow(pruned))
        out[]
    })
}
# }}}

# store_catalog__remove_files {{{
store_catalog__remove_files <- function(
    self,
    private,
    file_key,
    delete_local = FALSE,
    force = FALSE
) {
    private$check_open()
    checkmate::assert_character(
        file_key,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    checkmate::assert_flag(delete_local)
    checkmate::assert_flag(force)
    private$with_store_lock({
        store_catalog__remove_file_records(
            private,
            file_key,
            delete_local = delete_local,
            force = force
        )
    })
}
# }}}

# store_catalog__prune_orphans {{{
store_catalog__prune_orphans <- function(self, private, delete_local = FALSE) {
    private$check_open()
    checkmate::assert_flag(delete_local)
    orphans <- store_catalog__orphaned_files(private)
    if (!isTRUE(delete_local) || !nrow(orphans)) {
        return(orphans[])
    }
    private$with_store_lock({
        store_catalog__remove_file_records(
            private,
            orphans$file_key,
            delete_local = TRUE,
            force = TRUE
        )
    })
}
# }}}

# store_catalog__add_files {{{
store_catalog__add_files <- function(self, private, files, label = NULL) {
    checkmate::assert_string(label, null.ok = TRUE)

    private$with_store_lock({
        result_type <- store__result_type(files)
        dt <- store__file_table(files)
        query_id <- store__hash(
            result_type,
            priv(files)$index_node,
            jsonlite::toJSON(
                priv(files)$parameter$serialize(null = TRUE),
                auto_unbox = TRUE
            ),
            jsonlite::toJSON(
                dt,
                dataframe = "rows",
                POSIXt = "ISO8601",
                auto_unbox = TRUE,
                null = "null"
            )
        )

        query_file <- file.path(
            private$query_dir,
            sprintf("files-%s.json", query_id)
        )
        files$save(query_file)
        self$register_artifact(
            kind = "query",
            path = query_file,
            role = "input",
            project = "CMIP6",
            query_id = query_id,
            metadata = list(result_type = result_type)
        )

        time_filter <- files$time_filter
        query_run <- data.frame(
            query_id = query_id,
            label = store__chr1(label),
            result_type = result_type,
            query_file = store_rel_path(query_file, private$store_path),
            index_node = priv(files)$index_node,
            time_filter_start = store__time1(store__pluck(
                time_filter,
                "start"
            )),
            time_filter_stop = store__time1(store__pluck(
                time_filter,
                "stop"
            )),
            time_filter_method = store__chr1(store__pluck(
                time_filter,
                "method"
            )),
            created_at = store__now(),
            package_version = as.character(utils::packageVersion("epwshiftr")),
            stringsAsFactors = FALSE
        )
        private$replace_rows("query_run", query_run, "query_id")

        if (nrow(dt)) {
            now <- store__now()
            file_rows <- store_catalog__file_rows(private, dt, now)
            private$replace_rows(
                "esg_file",
                as.data.frame(file_rows),
                "file_key"
            )
            store_catalog__sync_query_file_links(
                private,
                query_id,
                file_rows,
                now
            )
            store_catalog__sync_file_catalog(private, query_id, file_rows)
        }

        query_id
    })
}
# }}}

# store_catalog__query_payload {{{
store_catalog__query_payload <- function(query) {
    state <- query$state(null = TRUE)
    parameter <- priv(query)$parameter$serialize(null = TRUE)
    parameter_json <- jsonlite::toJSON(
        parameter,
        auto_unbox = TRUE,
        null = "null",
        digits = 6
    )
    list(
        query_id = store__hash(
            "EsgQuery",
            state$index_node,
            parameter_json
        ),
        index_node = state$index_node,
        parameter_json = as.character(parameter_json)
    )
}
# }}}

# store_catalog__set_query_tracked {{{
store_catalog__set_query_tracked <- function(self, private, query_id, tracked) {
    private$check_open()
    checkmate::assert_string(query_id, min.chars = 1L)
    checkmate::assert_flag(tracked)
    row <- store_catalog__get_query_row(private, query_id)
    private$with_store_lock({
        row$tracked <- isTRUE(tracked)
        row$updated_at <- store__now()
        private$replace_rows(
            "esg_query",
            as.data.frame(row),
            "query_id"
        )
    })
    invisible(NULL)
}
# }}}

# store_catalog__delete_query_links {{{
store_catalog__delete_query_links <- function(
    private,
    table,
    id_col,
    query_id,
    filter_col,
    values = NULL
) {
    private$check_open()
    checkmate::assert_string(query_id, min.chars = 1L)
    checkmate::assert_character(
        values,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    private$with_store_lock({
        store_catalog__get_query_row(private, query_id)
        rows <- private$read_table(table)
        remove <- rows[rows[["query_id"]] == query_id]
        if (!is.null(values) && nrow(remove)) {
            remove <- remove[remove[[filter_col]] %in% values]
        }
        if (nrow(remove)) {
            private$delete_by_key(table, id_col, remove[[id_col]])
        }
    })
    invisible(NULL)
}
# }}}

# store_catalog__select_query_rows {{{
store_catalog__select_query_rows <- function(
    private,
    query_id = NULL,
    tracked = NULL
) {
    queries <- private$read_table("esg_query")
    if (!nrow(queries)) {
        return(queries[])
    }
    if (!is.null(query_id) && !length(query_id)) {
        return(queries[0])
    }
    if (!is.null(query_id)) {
        qids <- query_id
        queries <- queries[queries[["query_id"]] %in% qids]
    }
    if (!is.null(tracked) && nrow(queries)) {
        want_tracked <- isTRUE(tracked)
        queries <- queries[as.logical(queries[["tracked"]]) == want_tracked]
    }
    queries[]
}
# }}}

# store_catalog__resolve_query_selection {{{
store_catalog__resolve_query_selection <- function(
    private,
    query_id = NULL,
    tag = NULL,
    children = FALSE
) {
    ids <- query_id
    if (is.null(ids) && !is.null(tag)) {
        wanted_tag <- tag
        tags <- private$read_table("esg_query_tag")
        tags <- tags[tags[["tag"]] %in% wanted_tag]
        ids <- unique(tags$query_id)
    }
    if (isTRUE(children) && !is.null(ids) && length(ids)) {
        ids <- unique(c(
            ids,
            store_catalog__query_related_ids(
                private,
                ids,
                direction = "children",
                recursive = TRUE
            )
        ))
    }
    ids
}
# }}}

# store_catalog__query_related_ids {{{
store_catalog__query_related_ids <- function(
    private,
    query_id,
    direction = c("children", "parents", "both"),
    recursive = TRUE
) {
    direction <- match.arg(direction)
    edges <- private$read_table("esg_query_dependency")
    if (!nrow(edges)) {
        return(unique(query_id))
    }
    seen <- unique(query_id)
    frontier <- seen
    repeat {
        next_ids <- character()
        if (direction %in% c("children", "both")) {
            next_ids <- c(
                next_ids,
                edges[edges[["parent_query_id"]] %in% frontier]$query_id
            )
        }
        if (direction %in% c("parents", "both")) {
            next_ids <- c(
                next_ids,
                edges[edges[["query_id"]] %in% frontier]$parent_query_id
            )
        }
        next_ids <- setdiff(unique(next_ids), seen)
        if (!length(next_ids)) {
            break
        }
        seen <- unique(c(seen, next_ids))
        if (!isTRUE(recursive)) {
            break
        }
        frontier <- next_ids
    }
    seen
}
# }}}

# store_catalog__get_query_row {{{
store_catalog__get_query_row <- function(private, query_id) {
    rows <- store_catalog__select_query_rows(private, query_id = query_id)
    if (!nrow(rows)) {
        cli::cli_abort("Stored ESGF query {.val {query_id}} was not found.")
    }
    rows[1L]
}
# }}}

# store_catalog__summarise_query_status {{{
store_catalog__summarise_query_status <- function(
    private,
    queries,
    links,
    files,
    tasks
) {
    empty_counts <- stats::setNames(
        as.list(rep(0L, length(DOWNLOADER_TASK_STATUS))),
        paste0("download_", DOWNLOADER_TASK_STATUS)
    )
    rows <- vector("list", nrow(queries))
    for (i in seq_len(nrow(queries))) {
        query <- queries[i]
        qid <- query$query_id[[1L]]
        q_links <- links[links[["query_id"]] == qid]
        q_files <- if (nrow(q_links) && nrow(files)) {
            merge(
                q_links,
                files,
                by = "file_key",
                all.x = TRUE,
                sort = FALSE
            )
        } else {
            data.table::data.table()
        }
        current <- q_files[q_files[["status"]] == "current"]

        local_available <- 0L
        if (nrow(current) && "local_path" %in% names(current)) {
            local_paths <- current$local_path
            has_path <- !is.na(local_paths) & nzchar(local_paths)
            if (any(has_path)) {
                local_available <- sum(file.exists(vapply(
                    local_paths[has_path],
                    store_abs_path,
                    character(1L),
                    root = private$store_path
                )))
            }
        }

        q_tasks <- if (
            nrow(tasks) && nrow(q_links) && "file_key" %in% names(tasks)
        ) {
            tasks[tasks[["file_key"]] %in% q_links$file_key]
        } else {
            data.table::data.table()
        }
        if (nrow(q_tasks)) {
            data.table::setorderv(
                q_tasks,
                intersect(
                    c("file_key", "updated_at", "created_at"),
                    names(q_tasks)
                )
            )
            q_tasks <- q_tasks[, .SD[.N], by = "file_key"]
        }
        task_counts <- empty_counts
        if (nrow(q_tasks)) {
            counted <- table(factor(
                q_tasks$status,
                levels = DOWNLOADER_TASK_STATUS
            ))
            task_counts <- as.list(as.integer(counted))
            names(task_counts) <- paste0(
                "download_",
                DOWNLOADER_TASK_STATUS
            )
        }

        bytes_total <- if (nrow(current) && "size" %in% names(current)) {
            sum(suppressWarnings(as.numeric(current$size)), na.rm = TRUE)
        } else {
            0
        }
        bytes_done <- if (nrow(q_tasks) && "bytes_done" %in% names(q_tasks)) {
            sum(
                suppressWarnings(as.numeric(q_tasks$bytes_done[
                    q_tasks$status %in% c("done", "skipped")
                ])),
                na.rm = TRUE
            )
        } else {
            0
        }

        current_count <- sum(q_links$status == "current", na.rm = TRUE)
        complete <- current_count > 0L &&
            local_available >= current_count &&
            sum(unlist(task_counts[c(
                "download_downloading",
                "download_error",
                "download_cancelled"
            )])) ==
                0L

        rows[[i]] <- data.table::as.data.table(c(
            list(
                query_id = qid,
                label = store__chr1(query$label[[1L]]),
                tracked = store__flag(query$tracked[[1L]]),
                file_total = nrow(q_links),
                file_current = current_count,
                file_missing = sum(q_links$status == "missing", na.rm = TRUE),
                file_retracted = sum(
                    q_links$status == "retracted",
                    na.rm = TRUE
                ),
                file_deprecated = sum(
                    q_links$status == "deprecated",
                    na.rm = TRUE
                )
            ),
            task_counts,
            list(
                local_available = as.integer(local_available),
                bytes_total = as.numeric(bytes_total),
                bytes_done = as.numeric(bytes_done),
                complete = isTRUE(complete),
                last_checked_at = query$last_checked_at[[1L]],
                updated_at = query$updated_at[[1L]]
            )
        ))
    }

    data.table::rbindlist(rows, fill = TRUE)
}
# }}}

# store_catalog__workflow_downloads {{{
store_catalog__workflow_downloads <- function(private, query_id, downloader) {
    # A failed manifest read is not evidence that there are no tasks.
    tasks <- data.table::as.data.table(downloader$tasks())
    if (!nrow(tasks)) {
        return(tasks)
    }
    links <- private$read_table("esg_query_file")
    links <- links[links[["query_id"]] %in% query_id]
    if (!nrow(links) || !"file_key" %in% names(tasks)) {
        return(tasks[0])
    }
    tasks <- tasks[tasks[["file_key"]] %in% links$file_key]
    if (!nrow(tasks)) {
        return(tasks[])
    }
    link_cols <- data.table::data.table(
        query_id = links$query_id,
        file_key = links$file_key,
        query_file_status = links$status
    )
    tasks <- merge(
        tasks,
        link_cols,
        by = "file_key",
        all.x = TRUE,
        sort = FALSE
    )
    order_cols <- intersect(
        c("file_key", "updated_at", "created_at"),
        names(tasks)
    )
    if (length(order_cols)) {
        data.table::setorderv(tasks, order_cols)
    }
    tasks[, .SD[.N], by = "file_key"][]
}
# }}}

# store_catalog__load_query {{{
store_catalog__load_query <- function(private, row) {
    file <- store_abs_path(row$query_file[[1L]], root = private$store_path)
    esg_query()$load(file)
}
# }}}

# Shared snapshot preparation for query update and dry-run update preview.
# store_catalog__prepare_query_update {{{
store_catalog__prepare_query_update <- function(
    private,
    query_id,
    files,
    now,
    preview = FALSE
) {
    store__result_type(files)
    dt <- store__file_table(files)
    existing_links <- private$read_table("esg_query_file")
    wanted_query_id <- query_id
    existing_links <- existing_links[
        existing_links[["query_id"]] == wanted_query_id
    ]
    existing_files <- private$read_table("esg_file")
    existing_files <- existing_files[
        existing_files[["file_key"]] %in% existing_links$file_key
    ]
    file_rows <- store_catalog__file_rows(private, dt, now)
    update_id <- if (isTRUE(preview)) {
        store__hash(
            "preview",
            query_id,
            now,
            nrow(file_rows),
            stats::runif(1L)
        )
    } else {
        store__hash(query_id, now, nrow(file_rows), stats::runif(1L))
    }
    changes <- store_catalog__query_update_changes(
        update_id,
        query_id,
        existing_links,
        existing_files,
        file_rows,
        now
    )
    list(
        update_id = update_id,
        file_rows = file_rows,
        changes = changes
    )
}
# }}}

# store_catalog__update_query_files {{{
store_catalog__update_query_files <- function(
    self,
    private,
    query_id,
    files,
    fields = "*",
    all = TRUE,
    limit = FALSE
) {
    now <- store__now()
    prepared <- store_catalog__prepare_query_update(
        private,
        query_id,
        files,
        now,
        preview = FALSE
    )
    update_id <- prepared$update_id
    file_rows <- prepared$file_rows
    changes <- prepared$changes

    if (nrow(file_rows)) {
        private$replace_rows(
            "esg_file",
            as.data.frame(file_rows),
            "file_key"
        )
        store_catalog__sync_file_catalog(private, query_id, file_rows)
    }

    store_catalog__sync_query_file_links(private, query_id, file_rows, now)
    query <- store_catalog__get_query_row(private, query_id)
    query$last_checked_at <- now
    query$updated_at <- now
    private$replace_rows("esg_query", as.data.frame(query), "query_id")
    store_catalog__record_query_update(
        private,
        update_id = update_id,
        query_id = query_id,
        changes = changes,
        fields = fields,
        all = all,
        limit = limit,
        started_at = now,
        completed_at = now
    )
    links <- self$query_files(query_id)
    if (nrow(changes) && nrow(links)) {
        change_cols <- c(
            "query_id",
            "file_key",
            "update_id",
            "change_type",
            "previous_status",
            "current_status",
            "previous_version",
            "current_version",
            "previous_checksum",
            "current_checksum",
            "version_changed",
            "checksum_changed",
            "size_changed",
            "url_changed",
            "data_node_changed"
        )
        links <- merge(
            links,
            changes[, intersect(change_cols, names(changes)), with = FALSE],
            by = c("query_id", "file_key"),
            all.x = TRUE,
            sort = FALSE
        )
    } else {
        links$update_id <- update_id
    }
    links[]
}
# }}}

# store_catalog__preview_query_update {{{
store_catalog__preview_query_update <- function(private, row, files) {
    now <- store__now()
    query_id <- row$query_id[[1L]]
    prepared <- store_catalog__prepare_query_update(
        private,
        query_id,
        files,
        now,
        preview = TRUE
    )
    summary <- store_catalog__query_update_preview_summary(
        row,
        prepared$changes
    )
    list(
        summary = summary,
        changes = prepared$changes,
        files = files,
        file_rows = prepared$file_rows
    )
}
# }}}

# store_catalog__query_update_preview_summary {{{
store_catalog__query_update_preview_summary <- function(row, changes) {
    count_type <- function(type) {
        as.integer(sum(changes$change_type == type, na.rm = TRUE))
    }
    count_flag <- function(column) {
        as.integer(sum(changes[[column]] %in% TRUE, na.rm = TRUE))
    }
    sum_size <- function(rows) {
        sum(suppressWarnings(as.numeric(rows$current_size)), na.rm = TRUE)
    }

    active <- changes[changes[["current_status"]] == "current"]
    data.table::data.table(
        query_id = row$query_id[[1L]],
        label = store__chr1(row$label[[1L]]),
        file_total = as.integer(nrow(changes)),
        current_count = count_type("current"),
        new_count = count_type("new"),
        stale_count = count_type("stale"),
        changed_count = count_type("changed"),
        deprecated_count = count_flag("deprecated"),
        retracted_count = count_flag("retracted"),
        version_changed_count = count_flag("version_changed"),
        bytes_total = as.numeric(sum_size(active)),
        bytes_new = as.numeric(sum_size(active[
            active[["change_type"]] == "new"
        ])),
        bytes_changed = as.numeric(sum_size(active[
            active[["change_type"]] == "changed"
        ]))
    )
}
# }}}

# store_catalog__query_update_changes {{{
store_catalog__query_update_changes <- function(
    update_id,
    query_id,
    existing_links,
    existing_files,
    file_rows,
    now
) {
    current_keys <- if (nrow(file_rows)) {
        unique(file_rows$file_key)
    } else {
        character()
    }
    previous_keys <- if (nrow(existing_links)) {
        unique(existing_links$file_key)
    } else {
        character()
    }
    keys <- unique(c(previous_keys, current_keys))
    if (!length(keys)) {
        return(data.table::data.table())
    }
    rows <- vector("list", length(keys))
    for (i in seq_along(keys)) {
        key <- keys[[i]]
        previous_link <- existing_links[
            existing_links[["file_key"]] == key
        ][1L]
        previous_file <- existing_files[
            existing_files[["file_key"]] == key
        ][1L]
        current_file <- if (nrow(file_rows)) {
            file_rows[file_rows[["file_key"]] == key][1L]
        } else {
            file_rows
        }
        has_previous <- nrow(previous_link) &&
            !is.na(previous_link$file_key[[1L]])
        has_current <- nrow(current_file) &&
            !is.na(current_file$file_key[[1L]])
        previous_status <- if (has_previous) {
            previous_link$status[[1L]]
        } else {
            NA_character_
        }
        current_status <- if (has_current) {
            store_catalog__file_link_status(current_file)[[1L]]
        } else {
            "missing"
        }

        version_changed <- store_catalog__query_update_changed(
            previous_file,
            current_file,
            "version"
        )
        checksum_changed <- store_catalog__query_update_changed(
            previous_file,
            current_file,
            "checksum"
        )
        size_changed <- store_catalog__query_update_changed(
            previous_file,
            current_file,
            "size"
        )
        url_changed <- store_catalog__query_update_changed(
            previous_file,
            current_file,
            "url_download"
        )
        data_node_changed <- store_catalog__query_update_changed(
            previous_file,
            current_file,
            "data_node"
        )
        changed <- any(
            c(
                version_changed,
                checksum_changed,
                size_changed,
                url_changed,
                data_node_changed
            ),
            na.rm = TRUE
        )
        change_type <- if (!has_previous && has_current) {
            "new"
        } else if (has_previous && !has_current) {
            "stale"
        } else if (isTRUE(changed)) {
            "changed"
        } else {
            "current"
        }

        rows[[i]] <- data.table::data.table(
            update_file_id = store__hash(update_id, key),
            update_id = update_id,
            query_id = query_id,
            file_key = key,
            change_type = change_type,
            previous_status = previous_status,
            current_status = current_status,
            previous_version = store_catalog__query_update_value(
                previous_file,
                "version"
            ),
            current_version = store_catalog__query_update_value(
                current_file,
                "version"
            ),
            previous_checksum = store_catalog__query_update_value(
                previous_file,
                "checksum"
            ),
            current_checksum = store_catalog__query_update_value(
                current_file,
                "checksum"
            ),
            previous_size = suppressWarnings(as.numeric(
                store_catalog__query_update_value(previous_file, "size")
            )),
            current_size = suppressWarnings(as.numeric(
                store_catalog__query_update_value(current_file, "size")
            )),
            version_changed = isTRUE(version_changed),
            checksum_changed = isTRUE(checksum_changed),
            size_changed = isTRUE(size_changed),
            url_changed = isTRUE(url_changed),
            data_node_changed = isTRUE(data_node_changed),
            deprecated = has_current &&
                store__flag(current_file$deprecated[[1L]]),
            retracted = has_current &&
                store__flag(current_file$retracted[[1L]]),
            created_at = now
        )
    }
    data.table::rbindlist(rows, fill = TRUE)
}
# }}}

# store_catalog__query_update_value {{{
store_catalog__query_update_value <- function(row, column) {
    if (!nrow(row) || !column %in% names(row)) {
        return(NA_character_)
    }
    store__chr1(row[[column]][[1L]])
}
# }}}

# store_catalog__query_update_changed {{{
store_catalog__query_update_changed <- function(
    previous_row,
    current_row,
    column
) {
    if (!nrow(previous_row) || !nrow(current_row)) {
        return(FALSE)
    }
    previous <- store_catalog__query_update_value(previous_row, column)
    current <- store_catalog__query_update_value(current_row, column)
    if (is.na(previous) && is.na(current)) {
        return(FALSE)
    }
    !identical(as.character(previous), as.character(current))
}
# }}}

# store_catalog__record_query_update {{{
store_catalog__record_query_update <- function(
    private,
    update_id,
    query_id,
    changes,
    fields,
    all,
    limit,
    started_at,
    completed_at
) {
    if (nrow(changes)) {
        private$append_rows("esg_query_update_file", as.data.frame(changes))
    }
    summary <- data.frame(
        update_id = update_id,
        query_id = query_id,
        status = "done",
        started_at = started_at,
        completed_at = completed_at,
        fields = paste(as.character(fields), collapse = ","),
        all_files = isTRUE(all),
        limit_files = isTRUE(limit),
        file_total = as.integer(nrow(changes)),
        current_count = as.integer(sum(
            changes$change_type == "current",
            na.rm = TRUE
        )),
        new_count = as.integer(sum(
            changes$change_type == "new",
            na.rm = TRUE
        )),
        stale_count = as.integer(sum(
            changes$change_type == "stale",
            na.rm = TRUE
        )),
        changed_count = as.integer(sum(
            changes$change_type == "changed",
            na.rm = TRUE
        )),
        deprecated_count = as.integer(sum(
            changes$deprecated %in% TRUE,
            na.rm = TRUE
        )),
        retracted_count = as.integer(sum(
            changes$retracted %in% TRUE,
            na.rm = TRUE
        )),
        version_changed_count = as.integer(sum(
            changes$version_changed %in% TRUE,
            na.rm = TRUE
        )),
        download_session_id = NA_character_,
        last_error = NA_character_,
        stringsAsFactors = FALSE
    )
    private$replace_rows("esg_query_update", summary, "update_id")
    invisible(summary)
}
# }}}

# store_catalog__set_query_update_session {{{
store_catalog__set_query_update_session <- function(
    private,
    update_id,
    session_id
) {
    if (
        is.na(update_id) ||
            !nzchar(update_id) ||
            is.na(session_id) ||
            !nzchar(session_id)
    ) {
        return(invisible(NULL))
    }
    updates <- private$read_table("esg_query_update")
    wanted_update_id <- update_id
    row <- updates[updates[["update_id"]] == wanted_update_id]
    if (!nrow(row)) {
        return(invisible(NULL))
    }
    row$download_session_id <- session_id
    private$replace_rows("esg_query_update", as.data.frame(row), "update_id")
    invisible(row)
}
# }}}

# store_catalog__file_rows {{{
store_catalog__file_rows <- function(private, dt, now) {
    if (!nrow(dt)) {
        return(data.table::data.table())
    }
    dt <- data.table::as.data.table(dt)
    data.table::set(dt, j = "file_key", value = store__file_keys(dt))
    data.table::set(
        dt,
        j = "logical_file_id",
        value = store__logical_file_id(dt)
    )

    # A previously downloaded candidate outranks a fresh remote
    # replica of the same logical file. Remaining ties prefer current,
    # authoritative, newest records with both service URLs available.
    existing <- private$read_table("esg_file")
    existing <- existing[existing[["file_key"]] %in% dt$file_key]
    existing_created <- stats::setNames(existing$created_at, existing$file_key)
    existing_local_path <- stats::setNames(
        existing$local_path,
        existing$file_key
    )
    existing_artifact <- stats::setNames(
        existing$local_artifact_id,
        existing$file_key
    )
    local_path <- store__match_chr(existing_local_path, dt$file_key)
    data.table::set(dt, j = "row_order", value = seq_len(nrow(dt)))
    data.table::set(
        dt,
        j = "local_order",
        value = data.table::fifelse(
            !is.na(local_path) & nzchar(local_path),
            0L,
            1L
        )
    )
    data.table::set(
        dt,
        j = "latest_order",
        value = data.table::fifelse(store__lgl(dt$latest) %in% FALSE, 1L, 0L)
    )
    data.table::set(
        dt,
        j = "replica_order",
        value = data.table::fifelse(store__lgl(dt$replica) %in% TRUE, 1L, 0L)
    )
    data.table::set(
        dt,
        j = "retracted_order",
        value = data.table::fifelse(store__lgl(dt$retracted) %in% TRUE, 1L, 0L)
    )
    data.table::set(
        dt,
        j = "deprecated_order",
        value = data.table::fifelse(
            store__lgl(dt$deprecated) %in% TRUE,
            1L,
            0L
        )
    )
    data.table::set(
        dt,
        j = "version_order",
        value = store__version_rank(dt$version)
    )
    data.table::set(
        dt,
        j = "service_order",
        value = as.integer(is.na(dt$url_opendap) | !nzchar(dt$url_opendap)) +
            as.integer(is.na(dt$url_download) | !nzchar(dt$url_download))
    )
    data.table::setorderv(
        dt,
        c(
            "logical_file_id",
            "retracted_order",
            "deprecated_order",
            "latest_order",
            "local_order",
            "replica_order",
            "version_order",
            "service_order",
            "data_node",
            "file_key",
            "row_order"
        ),
        c(1L, 1L, 1L, 1L, 1L, 1L, -1L, 1L, 1L, 1L, 1L),
        na.last = TRUE
    )
    dt <- dt[!duplicated(dt$logical_file_id)]

    data.table::data.table(
        file_key = dt$file_key,
        esgf_id = dt$id,
        dataset_id = dt$dataset_id,
        master_id = dt$master_id,
        instance_id = dt$instance_id,
        tracking_id = dt$tracking_id,
        version = dt$version,
        title = dt$title,
        filename = dt$filename,
        checksum = dt$checksum,
        checksum_type = dt$checksum_type,
        size = suppressWarnings(as.numeric(dt$size)),
        latest = store__lgl(dt$latest),
        replica = store__lgl(dt$replica),
        retracted = store__lgl(dt$retracted),
        deprecated = store__lgl(dt$deprecated),
        data_node = dt$data_node,
        activity_id = dt$activity_id,
        institution_id = dt$institution_id,
        source_id = dt$source_id,
        experiment_id = dt$experiment_id,
        variant_label = dt$variant_label,
        frequency = dt$frequency,
        table_id = dt$table_id,
        variable_id = dt$variable_id,
        grid_label = dt$grid_label,
        datetime_start = store__time(dt$datetime_start),
        datetime_end = store__time(dt$datetime_end),
        url_opendap = dt$url_opendap,
        url_download = dt$url_download,
        local_path = store__match_chr(existing_local_path, dt$file_key),
        local_artifact_id = store__match_chr(existing_artifact, dt$file_key),
        created_at = store__match_time(existing_created, dt$file_key, now),
        updated_at = now
    )
}
# }}}

# store_catalog__sync_query_file_links {{{
store_catalog__sync_query_file_links <- function(
    private,
    query_id,
    file_rows,
    now
) {
    qid <- query_id
    current_keys <- if (nrow(file_rows)) {
        unique(file_rows$file_key)
    } else {
        character()
    }
    links <- private$read_table("esg_query_file")
    existing <- links[links[["query_id"]] == qid]

    if (nrow(existing)) {
        missing <- existing[!existing[["file_key"]] %in% current_keys]
        if (nrow(missing)) {
            missing$status <- "missing"
            missing$last_seen_at <- now
            private$replace_rows(
                "esg_query_file",
                as.data.frame(missing),
                "link_id"
            )
        }
    }

    if (!length(current_keys)) {
        return(invisible(NULL))
    }

    existing_first_seen <- stats::setNames(
        existing$first_seen_at,
        existing$file_key
    )
    rows <- data.table::data.table(
        link_id = vapply(
            current_keys,
            function(file_key) store__hash(qid, file_key),
            character(1L)
        ),
        query_id = qid,
        file_key = current_keys,
        status = store_catalog__file_link_status(file_rows[match(
            current_keys,
            file_rows$file_key
        )]),
        first_seen_at = store__match_time(
            existing_first_seen,
            current_keys,
            now
        ),
        last_seen_at = now
    )
    private$replace_rows("esg_query_file", as.data.frame(rows), "link_id")
    invisible(NULL)
}
# }}}

# store_catalog__file_link_status {{{
store_catalog__file_link_status <- function(file_rows) {
    data.table::fcase(
        store__lgl(file_rows$retracted) %in% TRUE  ,
        "retracted"                                ,
        store__lgl(file_rows$deprecated) %in% TRUE ,
        "deprecated"                               ,
        default = "current"
    )
}
# }}}

# store_catalog__sync_file_catalog {{{
store_catalog__sync_file_catalog <- function(private, query_id, file_rows) {
    if (!nrow(file_rows)) {
        return(invisible(NULL))
    }
    active <- file_rows[store_catalog__file_link_status(file_rows) == "current"]
    if (!nrow(active)) {
        return(invisible(NULL))
    }
    now <- store__now()
    catalog <- data.frame(
        file_key = active$file_key,
        query_id = query_id,
        esgf_id = active$esgf_id,
        dataset_id = active$dataset_id,
        master_id = active$master_id,
        instance_id = active$instance_id,
        version = active$version,
        title = active$title,
        filename = active$filename,
        tracking_id = active$tracking_id,
        checksum = active$checksum,
        checksum_type = active$checksum_type,
        size = suppressWarnings(as.numeric(active$size)),
        latest = active$latest,
        replica = active$replica,
        retracted = active$retracted,
        deprecated = active$deprecated,
        data_node = active$data_node,
        activity_id = active$activity_id,
        institution_id = active$institution_id,
        source_id = active$source_id,
        experiment_id = active$experiment_id,
        variant_label = active$variant_label,
        frequency = active$frequency,
        table_id = active$table_id,
        variable_id = active$variable_id,
        grid_label = active$grid_label,
        datetime_start = active$datetime_start,
        datetime_end = active$datetime_end,
        actual_time_start = store__time(rep(NA_character_, nrow(active))),
        actual_time_end = store__time(rep(NA_character_, nrow(active))),
        url_opendap = active$url_opendap,
        url_download = active$url_download,
        local_path = active$local_path,
        local_artifact_id = active$local_artifact_id,
        created_at = rep(now, nrow(active)),
        stringsAsFactors = FALSE
    )
    private$replace_rows("file_catalog", catalog, "file_key")
    invisible(NULL)
}
# }}}

# store_catalog__orphaned_files {{{
store_catalog__orphaned_files <- function(private) {
    empty <- data.table::data.table(
        file_key = character(),
        local_path = character(),
        local_artifact_id = character(),
        local_file = character(),
        local_exists = logical()
    )
    files <- private$read_table("esg_file")
    if (!nrow(files)) {
        return(empty)
    }
    links <- private$read_table("esg_query_file")
    linked <- unique(links$file_key)
    orphans <- files[!files[["file_key"]] %in% linked]
    if (!nrow(orphans)) {
        return(empty)
    }

    catalog <- private$read_table("file_catalog")
    artifacts <- private$read_table("artifact")
    catalog <- catalog[catalog[["file_key"]] %in% orphans$file_key]
    artifacts <- artifacts[
        artifacts[["file_key"]] %in%
            orphans$file_key &
            artifacts[["kind"]] == "netcdf"
    ]
    artifacts <- artifacts[!duplicated(artifacts$file_key)]

    catalog_cols <- data.table::data.table(
        file_key = catalog$file_key,
        catalog_local_path = catalog$local_path,
        catalog_artifact_id = catalog$local_artifact_id
    )
    artifact_cols <- data.table::data.table(
        file_key = artifacts$file_key,
        artifact_id = artifacts$artifact_id,
        artifact_path = artifacts$relative_path
    )
    out <- merge(
        orphans,
        catalog_cols,
        by = "file_key",
        all.x = TRUE,
        sort = FALSE
    )
    out <- merge(
        out,
        artifact_cols,
        by = "file_key",
        all.x = TRUE,
        sort = FALSE
    )
    orphan_local_path <- out$local_path
    miss_path <- is.na(orphan_local_path) | !nzchar(orphan_local_path)
    orphan_local_path[miss_path] <- out$catalog_local_path[miss_path]
    miss_path <- is.na(orphan_local_path) | !nzchar(orphan_local_path)
    orphan_local_path[miss_path] <- out$artifact_path[miss_path]

    orphan_artifact_id <- out$local_artifact_id
    miss_art <- is.na(orphan_artifact_id) | !nzchar(orphan_artifact_id)
    orphan_artifact_id[miss_art] <- out$catalog_artifact_id[miss_art]
    miss_art <- is.na(orphan_artifact_id) | !nzchar(orphan_artifact_id)
    orphan_artifact_id[miss_art] <- out$artifact_id[miss_art]

    local_file <- vapply(
        orphan_local_path,
        function(path) {
            if (is.na(path) || !nzchar(path)) {
                return(NA_character_)
            }
            store_abs_path(path, root = private$store_path)
        },
        character(1L)
    )
    data.table::data.table(
        file_key = out$file_key,
        local_path = orphan_local_path,
        local_artifact_id = orphan_artifact_id,
        local_file = local_file,
        local_exists = !is.na(local_file) & file.exists(local_file)
    )
}
# }}}

# store_catalog__remove_file_records {{{
store_catalog__remove_file_records <- function(
    private,
    file_key,
    delete_local = FALSE,
    force = FALSE
) {
    keys <- unique(file_key)
    files <- private$read_table("esg_file")
    rows <- files[files[["file_key"]] %in% keys]
    missing <- setdiff(keys, rows$file_key)
    if (length(missing)) {
        cli::cli_abort("ESGF file key(s) not found: {.val {missing}}.")
    }

    links <- private$read_table("esg_query_file")
    linked <- links[links[["file_key"]] %in% keys]
    if (nrow(linked) && !isTRUE(force)) {
        cli::cli_abort(
            "Cannot remove file(s) still linked to stored queries. Use {.code force = TRUE} to remove links first."
        )
    }

    catalog <- private$read_table("file_catalog")
    catalog <- catalog[catalog[["file_key"]] %in% keys]
    artifacts <- private$read_table("artifact")
    artifacts <- artifacts[
        artifacts[["file_key"]] %in%
            keys &
            artifacts[["kind"]] == "netcdf"
    ]

    local_path <- stats::setNames(rows$local_path, rows$file_key)
    if (nrow(catalog)) {
        missing_path <- is.na(local_path[catalog$file_key]) |
            !nzchar(local_path[catalog$file_key])
        local_path[catalog$file_key[missing_path]] <- catalog$local_path[
            missing_path
        ]
    }
    if (nrow(artifacts)) {
        missing_path <- is.na(local_path[artifacts$file_key]) |
            !nzchar(local_path[artifacts$file_key])
        local_path[artifacts$file_key[missing_path]] <- artifacts$relative_path[
            missing_path
        ]
    }

    local_file <- vapply(
        local_path[keys],
        function(path) {
            if (is.na(path) || !nzchar(path)) {
                return(NA_character_)
            }
            store_abs_path(path, root = private$store_path)
        },
        character(1L)
    )
    deleted_local <- rep(FALSE, length(keys))
    if (isTRUE(delete_local)) {
        exists <- !is.na(local_file) & file.exists(local_file)
        deleted_local[exists] <- unlink(
            local_file[exists],
            recursive = FALSE,
            force = TRUE
        ) ==
            0L
    }

    if (nrow(linked) && isTRUE(force)) {
        private$delete_by_key("esg_query_file", "file_key", keys)
    }
    if (nrow(artifacts)) {
        private$delete_by_key("artifact", "artifact_id", artifacts$artifact_id)
    }
    private$delete_by_key("file_catalog", "file_key", keys)
    private$delete_by_key("esg_file", "file_key", keys)

    data.table::data.table(
        file_key = keys,
        local_file = unname(local_file),
        deleted_local = deleted_local,
        removed_links = vapply(
            keys,
            function(key) sum(linked$file_key == key),
            integer(1L)
        )
    )
}
# }}}

# vim: fdm=marker :
