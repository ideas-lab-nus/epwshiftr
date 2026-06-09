# EsgStore {{{
#' Local ESGF Store
#'
#' @description
#'
#' `EsgStore` manages a local DuckDB manifest and a fixed directory layout for
#' query result snapshots, dictionaries, source files, downloaded NetCDF files,
#' Parquet regional extracts, and generated outputs.
#'
#' @author Hongyuan Jia
#' @name EsgStore
#' @export
EsgStore <- R6::R6Class(
    "EsgStore",
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # initialize {{{
        #' @description
        #' Create or open a local store.
        #'
        #' @param path Store directory. Default: [store_dir()].
        #' @param create If `TRUE`, create the store directory when it does not
        #'        exist. Default: `TRUE`.
        #' @param overwrite If `TRUE`, remove an existing store directory before
        #'        creating a new store. Default: `FALSE`.
        #'
        #' @return An `EsgStore` object.
        initialize = function(path = NULL, create = TRUE, overwrite = FALSE) {
            if (is.null(path)) {
                path <- store_dir(init = create)
            }
            checkmate::assert_string(path)
            checkmate::assert_flag(create)
            checkmate::assert_flag(overwrite)

            path <- path.expand(path)
            if (dir.exists(path) && isTRUE(overwrite)) {
                unlink(path, recursive = TRUE, force = TRUE)
            }
            if (!dir.exists(path)) {
                if (!isTRUE(create)) {
                    cli::cli_abort("Store directory does not exist: {.path {path}}.")
                }
                dir.create(path, recursive = TRUE, showWarnings = FALSE)
            }
            if (!dir.exists(path)) {
                cli::cli_abort("Failed to create store directory: {.path {path}}.")
            }

            private$store_path <- normalizePath(path, mustWork = TRUE, winslash = "/")
            private$query_dir <- file.path(private$store_path, "queries")
            private$dict_dir <- file.path(private$store_path, "dicts")
            private$source_dir <- file.path(private$store_path, "sources")
            private$download_dir <- file.path(private$store_path, "downloads")
            private$extract_dir <- file.path(private$store_path, "extracts")
            private$output_dir <- file.path(private$store_path, "outputs")
            private$tmp_dir <- file.path(private$store_path, "tmp")
            private$tmp_download_dir <- file.path(private$tmp_dir, "downloads")
            private$log_dir <- file.path(private$store_path, "logs")
            dirs <- c(
                private$query_dir,
                private$dict_dir,
                private$source_dir,
                private$download_dir,
                private$extract_dir,
                private$output_dir,
                private$tmp_dir,
                private$tmp_download_dir,
                private$log_dir
            )
            for (dir in dirs) {
                if (!dir.exists(dir)) {
                    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
                }
            }

            private$manifest_path <- file.path(private$store_path, "manifest.duckdb")
            private$connect()
            private$init_schema()

            self
        },
        # }}}

        # close {{{
        #' @description
        #' Close the DuckDB connection.
        #'
        #' @return The store object itself, invisibly.
        close = function() {
            private$disconnect()
            invisible(self)
        },
        # }}}

        # get_meta {{{
        #' @description
        #' Return a store metadata value.
        #'
        #' @param key Metadata key.
        #' @param default Value returned when `key` is not set.
        #'
        #' @return A single string, or `default`.
        get_meta = function(key, default = NULL) {
            private$check_open()
            checkmate::assert_string(key, min.chars = 1L)

            meta <- DBI::dbReadTable(private$conn, "store_meta")
            row <- meta[meta$key == key, , drop = FALSE]
            if (!nrow(row)) {
                return(default)
            }
            row$value[[1L]]
        },
        # }}}

        # set_meta {{{
        #' @description
        #' Set a store metadata value.
        #'
        #' @param key Metadata key.
        #' @param value Metadata value. `NULL` is stored as `NA`.
        #'
        #' @return The store object, invisibly.
        set_meta = function(key, value) {
            private$check_open()
            checkmate::assert_string(key, min.chars = 1L)
            checkmate::assert_string(value, null.ok = TRUE)

            private$replace_rows("store_meta", data.frame(
                key = key,
                value = extract_store_na_character(value),
                updated_at = extract_store_now(),
                stringsAsFactors = FALSE
            ), "key")
            invisible(self)
        },
        # }}}

        # register_artifact {{{
        #' @description
        #' Register a file artifact in the store manifest.
        #'
        #' @param kind Artifact kind.
        #' @param path Artifact path. Absolute paths must be inside the store root.
        #' @param role Artifact role. If `NULL`, a role is inferred from `kind`.
        #' @param project Optional ESGF project.
        #' @param status Artifact status. Default: `"available"`.
        #' @param checksum Expected checksum. If `NULL` and `path` exists, it is
        #'        calculated with `checksum_type`.
        #' @param checksum_type Checksum algorithm. Default: `"sha256"`.
        #' @param size Artifact size in bytes. If `NULL` and `path` exists, it is
        #'        read from the file.
        #' @param query_id,file_key,dict_id Optional manifest links.
        #' @param source_url,source_repo,source_tag,source_commit Optional source
        #'        provenance.
        #' @param metadata Optional metadata list encoded as JSON.
        #'
        #' @return The artifact ID.
        register_artifact = function(kind, path, role = NULL, project = NULL,
                                     status = "available", checksum = NULL,
                                     checksum_type = "sha256", size = NULL,
                                     query_id = NULL, file_key = NULL,
                                     dict_id = NULL, source_url = NULL,
                                     source_repo = NULL, source_tag = NULL,
                                     source_commit = NULL, metadata = list()) {
            private$check_open()
            checkmate::assert_choice(kind, c("query", "dict", "source", "cmip6_index", "netcdf", "extract", "output"))
            checkmate::assert_string(path, min.chars = 1L)
            checkmate::assert_choice(status, c("planned", "available", "failed", "missing"))
            checkmate::assert_choice(checksum_type, c("md5", "sha256"))
            checkmate::assert_list(metadata, null.ok = TRUE)

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
            checkmate::assert_choice(role, c("input", "source", "download", "derived", "output"))

            if (is.null(size) && exists) {
                size <- as.numeric(file.info(path, extra_cols = FALSE)$size)
            }
            if (is.null(checksum) && exists && identical(status, "available")) {
                checksum <- store_hash_file(path, checksum_type)
            }
            if (is.null(size)) size <- NA_real_
            if (is.null(checksum)) checksum <- NA_character_
            if (is.null(metadata)) metadata <- list()

            artifact_id <- extract_store_hash(kind, rel_path, checksum, file_key, dict_id, source_commit)
            now <- extract_store_now()
            row <- data.frame(
                artifact_id = artifact_id,
                kind = kind,
                role = role,
                project = extract_store_na_character(project),
                relative_path = rel_path,
                checksum = extract_store_na_character(checksum),
                checksum_type = checksum_type,
                size = as.numeric(size),
                status = status,
                query_id = extract_store_na_character(query_id),
                file_key = extract_store_na_character(file_key),
                dict_id = extract_store_na_character(dict_id),
                source_url = extract_store_na_character(source_url),
                source_repo = extract_store_na_character(source_repo),
                source_tag = extract_store_na_character(source_tag),
                source_commit = extract_store_na_character(source_commit),
                metadata_json = jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null"),
                created_at = now,
                updated_at = now,
                stringsAsFactors = FALSE
            )
            private$replace_rows("artifact", row, "artifact_id")
            artifact_id
        },
        # }}}

        # artifact_path {{{
        #' @description
        #' Return an artifact path from the manifest.
        #'
        #' @param artifact_id Artifact ID.
        #'
        #' @return Absolute artifact path.
        artifact_path = function(artifact_id) {
            private$check_open()
            checkmate::assert_string(artifact_id, min.chars = 1L)

            artifacts <- DBI::dbReadTable(private$conn, "artifact")
            row <- artifacts[artifacts$artifact_id == artifact_id, , drop = FALSE]
            if (!nrow(row)) {
                cli::cli_abort("Artifact ID {.val {artifact_id}} was not found in the store manifest.")
            }
            store_abs_path(row$relative_path[[1L]], root = private$store_path)
        },
        # }}}

        # validate {{{
        #' @description
        #' Validate registered artifact files against the manifest.
        #'
        #' @return A data.table with validation results.
        validate = function() {
            private$check_open()
            artifacts <- data.table::as.data.table(DBI::dbReadTable(private$conn, "artifact"))
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

            artifacts[, expected_path := vapply(relative_path, store_abs_path, character(1L), root = private$store_path)]
            artifacts[, exists := file.exists(expected_path)]
            artifacts[, checksum_ok := mapply(function(path, ok, checksum, checksum_type) {
                if (!isTRUE(ok) || is.na(checksum) || !nzchar(checksum)) {
                    return(NA)
                }
                identical(tolower(store_hash_file(path, checksum_type)), tolower(checksum))
            }, expected_path, exists, checksum, checksum_type)]
            artifacts[, size_ok := mapply(function(path, ok, size) {
                if (!isTRUE(ok) || is.na(size)) {
                    return(NA)
                }
                identical(as.numeric(file.info(path, extra_cols = FALSE)$size), as.numeric(size))
            }, expected_path, exists, size)]
            artifacts[, .SD, .SDcols = c(
                "artifact_id", "kind", "expected_path", "exists",
                "checksum_ok", "size_ok", "status"
            )]
        },
        # }}}

        # add_query {{{
        #' @description
        #' Add an ESGF query to the long-lived store query registry.
        #'
        #' @param query An [EsgQuery] object.
        #' @param label Optional label.
        #' @param track Whether to mark the query as tracked. Default: `FALSE`.
        #'
        #' @return The stable query ID.
        add_query = function(query, label = NULL, track = FALSE) {
            private$check_open()
            if (!inherits(query, "EsgQuery")) {
                cli::cli_abort("`query` must be an {.cls EsgQuery} object.")
            }
            checkmate::assert_string(label, null.ok = TRUE)
            checkmate::assert_flag(track)

            payload <- private$query_payload(query)
            query_id <- payload$query_id
            qid <- query_id
            query_file <- file.path(private$query_dir, sprintf("query-%s.json", query_id))
            query$save(query_file)

            now <- extract_store_now()
            queries <- private$read_table("esg_query")
            existing <- queries[queries[["query_id"]] == qid]
            created_at <- if (nrow(existing)) existing$created_at[[1L]] else now
            tracked <- if (nrow(existing)) extract_store_is_true(existing$tracked[[1L]]) || isTRUE(track) else isTRUE(track)
            if (is.null(label) && nrow(existing)) {
                label <- extract_store_na_character(existing$label[[1L]])
                if (is.na(label)) label <- NULL
            }

            row <- data.frame(
                query_id = query_id,
                label = extract_store_na_character(label),
                index_node = payload$index_node,
                query_file = extract_store_rel_path(query_file, private$store_path),
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
        },
        # }}}

        # track_query {{{
        #' @description
        #' Mark a stored ESGF query as tracked.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #'
        #' @return The store object, invisibly.
        track_query = function(query_id) {
            private$set_query_tracked(query_id, TRUE)
            invisible(self)
        },
        # }}}

        # untrack_query {{{
        #' @description
        #' Mark a stored ESGF query as untracked.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #'
        #' @return The store object, invisibly.
        untrack_query = function(query_id) {
            private$set_query_tracked(query_id, FALSE)
            invisible(self)
        },
        # }}}

        # queries {{{
        #' @description
        #' List stored ESGF queries.
        #'
        #' @param tracked Optional tracked-state filter.
        #'
        #' @return A data.table of stored query records.
        queries = function(tracked = NULL) {
            private$check_open()
            checkmate::assert_flag(tracked, null.ok = TRUE)
            queries <- private$read_table("esg_query")
            if (!is.null(tracked) && nrow(queries)) {
                want_tracked <- isTRUE(tracked)
                queries <- queries[as.logical(queries[["tracked"]]) == want_tracked]
            }
            queries[]
        },
        # }}}

        # query_files {{{
        #' @description
        #' List files linked to a stored ESGF query.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #' @param status Optional query-file status filter.
        #'
        #' @return A data.table of linked file records.
        query_files = function(query_id, status = NULL) {
            private$check_open()
            checkmate::assert_string(query_id, min.chars = 1L)
            checkmate::assert_character(status, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
            private$get_query_row(query_id)

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
            out <- merge(links, files, by = "file_key", all.x = TRUE, sort = FALSE)
            data.table::setcolorder(out, c("query_id", "file_key", "status", setdiff(names(out), c("query_id", "file_key", "status"))))
            out[]
        },
        # }}}

        # update_queries {{{
        #' @description
        #' Refresh stored ESGF queries and link their current File records.
        #'
        #' @param query_id Optional query ID. If `NULL`, tracked queries are
        #'        updated by default.
        #' @param tracked Tracked-state filter used when `query_id` is `NULL`.
        #' @param all,limit,fields Arguments passed to `EsgQuery$collect()`.
        #' @param ... Additional File query filters passed to `EsgQuery$collect()`.
        #'
        #' @return A data.table of query-file links touched by the update.
        update_queries = function(query_id = NULL, tracked = TRUE, all = TRUE, limit = FALSE, fields = "*", ...) {
            private$check_open()
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_flag(tracked, null.ok = TRUE)
            rows <- private$select_query_rows(query_id = query_id, tracked = if (is.null(query_id)) tracked else NULL)
            if (!nrow(rows)) {
                return(data.table::data.table())
            }

            updated <- vector("list", nrow(rows))
            for (i in seq_len(nrow(rows))) {
                query <- private$load_query(rows[i])
                files <- query$collect(type = "File", fields = fields, all = all, limit = limit, ...)
                updated[[i]] <- private$update_query_files(rows$query_id[[i]], files)
            }
            data.table::rbindlist(updated, fill = TRUE)
        },
        # }}}

        # add_files {{{
        #' @description
        #' Add File or Aggregation query results to the local file catalog.
        #'
        #' @param files An `EsgResultFile` or `EsgResultAggregation` object.
        #' @param label Optional label for this query run.
        #'
        #' @return The created or updated query ID.
        add_files = function(files, label = NULL) {
            checkmate::assert_string(label, null.ok = TRUE)
            private$check_open()

            result_type <- extract_store_result_type(files)
            dt <- extract_store_file_table(files)
            query_id <- extract_store_hash(
                result_type,
                priv(files)$index_node,
                jsonlite::toJSON(priv(files)$parameter$serialize(null = TRUE), auto_unbox = TRUE),
                jsonlite::toJSON(dt, dataframe = "rows", POSIXt = "ISO8601", auto_unbox = TRUE, null = "null")
            )

            query_file <- file.path(private$query_dir, sprintf("files-%s.json", query_id))
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
                label = extract_store_na_character(label),
                result_type = result_type,
                query_file = extract_store_rel_path(query_file, private$store_path),
                index_node = priv(files)$index_node,
                time_filter_start = extract_store_parse_datetime_scalar(extract_store_list_value(time_filter, "start")),
                time_filter_stop = extract_store_parse_datetime_scalar(extract_store_list_value(time_filter, "stop")),
                time_filter_method = extract_store_na_character(extract_store_list_value(time_filter, "method")),
                created_at = extract_store_now(),
                package_version = as.character(utils::packageVersion("epwshiftr")),
                stringsAsFactors = FALSE
            )
            private$replace_rows("query_run", query_run, "query_id")

            if (nrow(dt)) {
                now <- extract_store_now()
                catalog <- data.frame(
                    file_key = extract_store_file_keys(dt),
                    query_id = query_id,
                    esgf_id = dt$id,
                    dataset_id = dt$dataset_id,
                    title = dt$title,
                    filename = dt$filename,
                    tracking_id = dt$tracking_id,
                    checksum = dt$checksum,
                    checksum_type = dt$checksum_type,
                    size = suppressWarnings(as.numeric(dt$size)),
                    data_node = dt$data_node,
                    source_id = dt$source_id,
                    experiment_id = dt$experiment_id,
                    variant_label = dt$variant_label,
                    frequency = dt$frequency,
                    table_id = dt$table_id,
                    variable_id = dt$variable_id,
                    grid_label = dt$grid_label,
                    datetime_start = extract_store_parse_datetime(dt$datetime_start),
                    datetime_end = extract_store_parse_datetime(dt$datetime_end),
                    actual_time_start = extract_store_parse_datetime(rep(NA_character_, nrow(dt))),
                    actual_time_end = extract_store_parse_datetime(rep(NA_character_, nrow(dt))),
                    url_opendap = dt$url_opendap,
                    url_download = dt$url_download,
                    local_path = rep(NA_character_, nrow(dt)),
                    local_artifact_id = rep(NA_character_, nrow(dt)),
                    created_at = rep(now, nrow(dt)),
                    stringsAsFactors = FALSE
                )
                private$replace_rows("file_catalog", catalog, "file_key")
            }

            query_id
        },
        # }}}

        # downloader {{{
        #' @description
        #' Return a FileDownloader bound to this store.
        #'
        #' @param ... Additional arguments passed to `FileDownloader$new()`.
        #'
        #' @return A [FileDownloader] object.
        downloader = function(...) {
            private$check_open()
            FileDownloader$new(
                dest = private$download_dir,
                temp = private$tmp_download_dir,
                manifest = file.path(private$download_dir, "_downloader", "manifest.duckdb"),
                ...
            )
        },
        # }}}

        # download_files {{{
        #' @description
        #' Enqueue and optionally download ESGF file records through the store downloader.
        #'
        #' @param files Optional [EsgResultFile] or [EsgResultAggregation] object.
        #'        If supplied, it is cataloged before the download plan is created.
        #' @param replica Replica policy passed to `$download_plan()`.
        #' @param downloader Optional [FileDownloader]. Default: `$downloader()`.
        #' @param run Whether to run the queued session immediately. Default: `TRUE`.
        #' @param session_label Optional download session label.
        #' @param service ESGF URL service to download from. Default:
        #'        `"HTTPServer"`.
        #' @param probe Whether to lightly probe URLs before ranking them.
        #' @param strategy Candidate ranking strategy.
        #' @param progress Whether to show per-file download progress.
        #' @param overwrite Whether to overwrite existing final files.
        #' @param resume Whether to resume interrupted `.part` files.
        #' @param ... Additional arguments passed to `$download_plan()` and
        #'        `FileDownloader$run()`.
        #'
        #' @return The created downloader session ID.
        download_files = function(files = NULL, replica = "auto", downloader = NULL,
                                  run = TRUE, session_label = NULL, service = "HTTPServer",
                                  probe = TRUE, strategy = c("fastest", "first", "stable"),
                                  progress = TRUE, overwrite = FALSE, resume = TRUE, ...) {
            private$check_open()
            strategy <- match.arg(strategy)
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }

            plan <- if (!is.null(files)) {
                query_id <- self$add_files(files, label = session_label)
                plan_args <- list(replica = replica, service = service, probe = probe, strategy = strategy, ...)
                plan <- do.call(files$download_plan, plan_args)
                private$decorate_download_plan(plan, query_id = query_id)
            } else {
                private$catalog_download_plan()
            }
            session_id <- downloader$enqueue(plan, session_label = session_label)
            if (isTRUE(run)) {
                downloader$run(session_id = session_id, progress = progress, overwrite = overwrite, resume = resume)
                self$sync_downloads(downloader)
            }
            session_id
        },
        # }}}

        # sync_downloads {{{
        #' @description
        #' Register completed downloader tasks as local store artifacts.
        #'
        #' @param downloader Optional [FileDownloader]. Default: `$downloader()`.
        #'
        #' @return A data.table of completed tasks.
        sync_downloads = function(downloader = NULL) {
            private$check_open()
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }
            tasks <- downloader$tasks(status = c("done", "skipped"))
            if (!nrow(tasks)) {
                return(tasks)
            }
            catalog <- data.table::as.data.table(DBI::dbReadTable(private$conn, "file_catalog"))
            if (!nrow(catalog)) {
                return(tasks)
            }
            for (i in seq_len(nrow(tasks))) {
                task <- tasks[i]
                file_key <- private$match_download_task(task, catalog)
                if (is.na(file_key)) {
                    next
                }
                local_path <- task$target_path[[1L]]
                if (!file.exists(local_path)) {
                    next
                }
                fk <- file_key
                row <- catalog[catalog[["file_key"]] == fk]
                checksum <- extract_store_na_character(task$checksum[[1L]])
                checksum_type <- tolower(extract_store_na_character(task$checksum_type[[1L]]))
                if (is.na(checksum_type) || !checksum_type %in% c("md5", "sha256")) {
                    checksum_type <- "sha256"
                }
                if (is.na(checksum)) {
                    checksum <- NULL
                }
                artifact_id <- self$register_artifact(
                    kind = "netcdf",
                    path = local_path,
                    role = "download",
                    project = "CMIP6",
                    checksum = checksum,
                    checksum_type = checksum_type,
                    query_id = row$query_id[[1L]],
                    file_key = file_key,
                    source_url = extract_store_na_character(task$selected_url[[1L]]),
                    metadata = list(filename = extract_store_na_character(task$filename[[1L]]))
                )
                row$local_path <- extract_store_rel_path(local_path, private$store_path)
                row$local_artifact_id <- artifact_id
                private$replace_rows("file_catalog", as.data.frame(row), "file_key")
            }
            tasks
        },
        # }}}

        # plan_region {{{
        #' @description
        #' Plan regional extraction jobs from cataloged files.
        #'
        #' @param query_id Query ID returned by `$add_files()`.
        #' @param lon,lat Target longitude and latitude.
        #' @param time Length-2 time range.
        #' @param site_id Site identifier. Default: `"site-1"`.
        #' @param variable_id Optional variable IDs. If `NULL`, all cataloged
        #'        variables in the query are used.
        #' @param filters Named list of exact-match file catalog filters.
        #' @param nearest Number of nearest grid cells to keep. Default: `1L`.
        #'
        #' @return A data.table of extraction plan rows.
        plan_region = function(query_id, lon, lat, time, site_id = "site-1",
                               variable_id = NULL, filters = list(), nearest = 1L) {
            checkmate::assert_string(query_id)
            checkmate::assert_number(lon, lower = -180, upper = 360, finite = TRUE)
            checkmate::assert_number(lat, lower = -90, upper = 90, finite = TRUE)
            checkmate::assert_string(site_id)
            checkmate::assert_character(variable_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_list(filters, names = "unique")
            checkmate::assert_int(nearest, lower = 1L)
            private$check_open()

            time_range <- extract_store_parse_time_range(time)
            catalog <- data.table::as.data.table(DBI::dbReadTable(private$conn, "file_catalog"))
            catalog <- catalog[catalog$query_id == query_id]
            if (!nrow(catalog)) {
                cli::cli_abort("No cataloged file records were found for query ID {.val {query_id}}.")
            }

            filter_names <- names(filters)
            if (length(filters)) {
                if (is.null(filter_names) || any(!nzchar(filter_names))) {
                    cli::cli_abort("All file catalog filters must be named.")
                }
                unknown <- setdiff(filter_names, names(catalog))
                if (length(unknown)) {
                    cli::cli_abort("Unknown file catalog filter column(s): {.val {unknown}}.")
                }

                for (name in filter_names) {
                    value <- filters[[name]]
                    checkmate::assert_atomic_vector(value, any.missing = FALSE, min.len = 1L, .var.name = sprintf("filters$%s", name))
                    catalog <- catalog[as.character(get(name)) %in% as.character(value)]
                }
            }

            if (!nrow(catalog)) {
                cli::cli_abort("No cataloged file records match the requested extraction plan filters.")
            }

            if (!is.null(variable_id)) {
                if (any(!is.na(catalog$variable_id))) {
                    catalog <- catalog[catalog$variable_id %in% variable_id]
                }
            } else {
                variable_id <- unique(catalog$variable_id)
                variable_id <- variable_id[!is.na(variable_id) & nzchar(variable_id)]
                if (!length(variable_id)) {
                    cli::cli_abort("Cannot plan extraction without `variable_id` because the file catalog does not contain variable IDs.")
                }
            }

            plan <- data.table::as.data.table(catalog)
            if (!any(!is.na(plan$variable_id)) && !is.null(variable_id)) {
                plan <- plan[rep(seq_len(nrow(plan)), each = length(variable_id))]
                plan$variable_id <- rep(variable_id, times = nrow(catalog))
            } else {
                plan <- plan[plan$variable_id %in% variable_id]
            }
            if (!nrow(plan)) {
                cli::cli_abort("No cataloged file records match the requested variable IDs.")
            }

            now <- extract_store_now()
            out <- data.frame(
                plan_id = vapply(seq_len(nrow(plan)), function(i) {
                    extract_store_hash(
                        plan$file_key[[i]],
                        site_id,
                        plan$variable_id[[i]],
                        lon,
                        lat,
                        nearest,
                        time_range$start,
                        time_range$stop
                    )
                }, character(1L)),
                query_id = query_id,
                file_key = plan$file_key,
                site_id = site_id,
                variable_id = plan$variable_id,
                lon = lon,
                lat = lat,
                nearest = nearest,
                time_start = time_range$start,
                time_stop = time_range$stop,
                status = "pending",
                available_time_count = NA_integer_,
                attempt_count = 0L,
                last_error = NA_character_,
                created_at = now,
                updated_at = now,
                stringsAsFactors = FALSE
            )
            out <- unique(out)
            private$append_new_rows("extraction_plan", out, "plan_id")

            existing <- data.table::as.data.table(DBI::dbReadTable(private$conn, "extraction_plan"))
            existing[plan_id %in% out$plan_id]
        },
        # }}}

        # extract {{{
        #' @description
        #' Execute pending or failed regional extraction plans.
        #'
        #' @param plan_id Optional plan IDs to run.
        #' @param status Plan statuses to run when `plan_id` is `NULL`.
        #'        Default: `c("pending", "failed")`.
        #' @param fallback What to do when OPeNDAP is unavailable. `"auto"`
        #'        downloads through HTTPServer when possible; `"error"` marks
        #'        the plan failed without downloading. Default: `"auto"`.
        #' @param overwrite If `TRUE`, overwrite existing Parquet outputs.
        #'        Default: `FALSE`.
        #'
        #' @return A data.table of processed extraction plan rows.
        extract = function(plan_id = NULL, status = c("pending", "failed"),
                           fallback = c("auto", "error"), overwrite = FALSE) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_subset(status, c("pending", "failed", "empty", "done"))
            checkmate::assert_flag(overwrite)
            fallback <- match.arg(fallback)
            private$check_open()

            plans <- data.table::as.data.table(DBI::dbReadTable(private$conn, "extraction_plan"))
            if (is.null(plan_id)) {
                plans <- plans[plans$status %in% status]
            } else {
                plans <- plans[plans$plan_id %in% plan_id]
            }
            if (!nrow(plans)) {
                return(plans)
            }

            catalog <- data.table::as.data.table(DBI::dbReadTable(private$conn, "file_catalog"))
            processed <- vector("list", nrow(plans))
            for (i in seq_len(nrow(plans))) {
                plan <- plans[i]
                file <- catalog[catalog$file_key == plan$file_key[[1L]]]
                if (!nrow(file)) {
                    processed[[i]] <- private$mark_plan_failed(plan, "The cataloged file record no longer exists.")
                    next
                }

                processed[[i]] <- tryCatch(
                    private$extract_one(plan, file[1L], fallback = fallback, overwrite = overwrite),
                    error = function(e) private$mark_plan_failed(plan, conditionMessage(e))
                )
            }

            data.table::rbindlist(processed, use.names = TRUE, fill = TRUE)
        },
        # }}}

        # query {{{
        #' @description
        #' Run a DuckDB SQL query against the extraction manifest.
        #'
        #' @param sql SQL query.
        #'
        #' @return A data.table.
        query = function(sql) {
            checkmate::assert_string(sql)
            private$check_open()

            data.table::as.data.table(DBI::dbGetQuery(private$conn, sql))
        },
        # }}}

        # summarise {{{
        #' @description
        #' Summarise extracted Parquet outputs by manifest columns.
        #'
        #' @param by Character vector of grouping columns. Default groups by
        #'        source, experiment, variant, frequency, variable, site and
        #'        year.
        #'
        #' @return A data.table.
        summarise = function(by = c("source_id", "experiment_id", "variant_label",
                                    "frequency", "variable_id", "site_id", "year")) {
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            private$check_open()

            map <- extract_store_summary_columns()
            unknown <- setdiff(by, names(map))
            if (length(unknown)) {
                cli::cli_abort("Unknown extraction summary column(s): {.val {unknown}}.")
            }

            groups <- unname(map[by])
            select_groups <- paste(sprintf("%s AS %s", groups, DBI::dbQuoteIdentifier(private$conn, by)), collapse = ", ")
            group_by <- paste(groups, collapse = ", ")
            sql <- sprintf(
                paste(
                    "SELECT %s,",
                    "COUNT(DISTINCT p.plan_id) AS plan_count,",
                    "COUNT(DISTINCT r.output_path) AS file_count,",
                    "COALESCE(SUM(r.row_count), 0) AS row_count,",
                    "COALESCE(SUM(r.unique_time_count), 0) AS unique_time_count,",
                    "MIN(r.time_min) AS time_min,",
                    "MAX(r.time_max) AS time_max",
                    "FROM extraction_plan p",
                    "LEFT JOIN file_catalog f ON p.file_key = f.file_key",
                    "LEFT JOIN extraction_result r ON p.plan_id = r.plan_id",
                    "GROUP BY %s",
                    "ORDER BY %s"
                ),
                select_groups,
                group_by,
                group_by
            )

            data.table::as.data.table(DBI::dbGetQuery(private$conn, sql))
        },
        # }}}

        # coverage {{{
        #' @description
        #' Check extraction coverage for planned jobs.
        #'
        #' @param plan_id Optional plan IDs to check.
        #'
        #' @return A data.table with one row per plan.
        coverage = function(plan_id = NULL) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            private$check_open()

            plans <- data.table::as.data.table(DBI::dbReadTable(private$conn, "extraction_plan"))
            if (!is.null(plan_id)) {
                plans <- plans[plans$plan_id %in% plan_id]
            }
            if (!nrow(plans)) {
                return(plans)
            }

            catalog <- data.table::as.data.table(DBI::dbReadTable(private$conn, "file_catalog"))
            results <- data.table::as.data.table(DBI::dbReadTable(private$conn, "extraction_result"))
            agg <- if (nrow(results)) {
                results[, .(
                    output_files = list(output_path),
                    output_file_count = .N,
                    output_rows = sum(row_count, na.rm = TRUE),
                    output_time_count = sum(unique_time_count, na.rm = TRUE),
                    output_time_min = min(time_min, na.rm = TRUE),
                    output_time_max = max(time_max, na.rm = TRUE)
                ), by = plan_id]
            } else {
                data.table::data.table(
                    plan_id = character(),
                    output_files = list(),
                    output_file_count = integer(),
                    output_rows = integer(),
                    output_time_count = integer(),
                    output_time_min = as.POSIXct(character(), tz = "UTC"),
                    output_time_max = as.POSIXct(character(), tz = "UTC")
                )
            }

            out <- merge(plans, catalog, by = c("query_id", "file_key"), all.x = TRUE, suffixes = c("", "_file"))
            out <- merge(out, agg, by = "plan_id", all.x = TRUE)
            out[is.na(output_file_count), `:=`(
                output_file_count = 0L,
                output_rows = 0L,
                output_time_count = 0L
            )]
            if (!"output_files" %in% names(out)) {
                out[, output_files := list(character())]
            }
            out[, output_files_exist := vapply(output_files, function(paths) {
                paths <- unlist(paths, use.names = FALSE)
                length(paths) > 0L && all(file.exists(file.path(private$store_path, paths)))
            }, logical(1L))]
            out[, complete := status == "done" &
                output_files_exist &
                (is.na(available_time_count) | available_time_count <= 0L | output_time_count >= available_time_count)]

            data.table::setcolorder(out, c(
                "plan_id", "complete", "status", "query_id", "file_key",
                "site_id", "source_id", "experiment_id", "variant_label",
                "frequency", "variable_id",
                intersect(c("available_time_count", "output_time_count", "output_rows", "output_file_count", "output_files_exist", "last_error"), names(out)),
                setdiff(names(out), c(
                    "plan_id", "complete", "status", "query_id", "file_key",
                    "site_id", "source_id", "experiment_id", "variant_label",
                    "frequency", "variable_id",
                    "available_time_count", "output_time_count", "output_rows",
                    "output_file_count", "output_files_exist", "last_error"
                ))
            ))
            out[]
        },
        # }}}

        # assert_complete {{{
        #' @description
        #' Assert that selected extraction plans are complete.
        #'
        #' @param plan_id Optional plan IDs to check.
        #'
        #' @return The store object itself, invisibly.
        assert_complete = function(plan_id = NULL) {
            cov <- self$coverage(plan_id = plan_id)
            if (!nrow(cov)) {
                cli::cli_abort("No extraction plans were found to check.")
            }
            bad <- cov[!cov$complete]
            if (nrow(bad)) {
                labels <- paste0(bad$plan_id, " [", bad$status, "]")
                cli::cli_abort(c(
                    "Some extraction plans are incomplete.",
                    "x" = "Incomplete plan(s): {paste(labels, collapse = ', ')}"
                ))
            }

            invisible(self)
        }
        # }}}
    ),
    active = list(
        # path {{{
        #' @field path Store directory.
        path = function() {
            private$store_path
        },
        # }}}

        # manifest {{{
        #' @field manifest DuckDB manifest path.
        manifest = function() {
            private$manifest_path
        },
        # }}}

        # is_open {{{
        #' @field is_open Whether the manifest connection is open.
        is_open = function() {
            !is.null(private$conn) && DBI::dbIsValid(private$conn)
        }
        # }}}
    ),
    private = list(
        store_path = NULL,
        manifest_path = NULL,
        query_dir = NULL,
        dict_dir = NULL,
        source_dir = NULL,
        download_dir = NULL,
        extract_dir = NULL,
        output_dir = NULL,
        tmp_dir = NULL,
        tmp_download_dir = NULL,
        log_dir = NULL,
        conn = NULL,

        # download plan helpers {{{
        catalog_download_plan = function() {
            catalog <- data.table::as.data.table(DBI::dbReadTable(private$conn, "file_catalog"))
            if (!nrow(catalog)) {
                return(data.table::data.table())
            }
            catalog <- catalog[is.na(local_path) | !nzchar(local_path)]
            if (!nrow(catalog)) {
                return(data.table::data.table())
            }
            plan <- data.table::data.table(
                logical_file_id = vapply(seq_len(nrow(catalog)), function(i) {
                    pieces <- c(
                        extract_store_na_character(catalog$tracking_id[[i]]),
                        extract_store_na_character(catalog$checksum[[i]]),
                        extract_store_na_character(catalog$filename[[i]]),
                        extract_store_na_character(catalog$esgf_id[[i]])
                    )
                    paste(pieces[!is.na(pieces) & nzchar(pieces)], collapse = ":")
                }, character(1L)),
                file_key = catalog$file_key,
                esgf_id = catalog$esgf_id,
                dataset_id = catalog$dataset_id,
                filename = catalog$filename,
                subdir = NA_character_,
                checksum = catalog$checksum,
                checksum_type = catalog$checksum_type,
                size = suppressWarnings(as.numeric(catalog$size)),
                url = catalog$url_download,
                service = "HTTPServer",
                data_node = catalog$data_node,
                priority = seq_len(nrow(catalog)),
                probe_latency = NA_real_,
                probe_throughput = NA_real_
            )
            plan[!is.na(url) & nzchar(url)]
        },

        decorate_download_plan = function(plan, query_id) {
            plan <- data.table::as.data.table(plan)
            if (!nrow(plan)) {
                return(plan)
            }
            catalog <- data.table::as.data.table(DBI::dbReadTable(private$conn, "file_catalog"))
            catalog <- catalog[catalog[["query_id"]] == query_id]
            if (!nrow(catalog)) {
                return(plan)
            }
            if (!"file_key" %in% names(plan)) {
                plan$file_key <- NA_character_
            }
            for (i in seq_len(nrow(plan))) {
                if (!is.na(plan$file_key[[i]]) && nzchar(plan$file_key[[i]])) {
                    next
                }
                hit <- catalog[catalog[["esgf_id"]] == plan$esgf_id[[i]]]
                if (!nrow(hit) && "checksum" %in% names(plan)) {
                    hit <- catalog[
                        catalog[["filename"]] == plan$filename[[i]] &
                            catalog[["checksum"]] == plan$checksum[[i]]
                    ]
                }
                if (nrow(hit)) {
                    plan$file_key[[i]] <- hit$file_key[[1L]]
                }
            }
            plan[]
        },

        match_download_task = function(task, catalog) {
            file_key <- extract_store_na_character(task$file_key[[1L]])
            if (!is.na(file_key) && file_key %in% catalog$file_key) {
                return(file_key)
            }
            esgf_id <- extract_store_na_character(task$esgf_id[[1L]])
            if (!is.na(esgf_id)) {
                hit <- catalog[catalog[["esgf_id"]] == esgf_id]
                if (nrow(hit)) return(hit$file_key[[1L]])
            }
            hit <- catalog[
                catalog[["filename"]] == task$filename[[1L]] &
                    catalog[["checksum"]] == task$checksum[[1L]]
            ]
            if (nrow(hit)) hit$file_key[[1L]] else NA_character_
        },
        # }}}

        # connect {{{
        connect = function() {
            private$conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = private$manifest_path, read_only = FALSE)
            invisible(private$conn)
        },
        # }}}

        # disconnect {{{
        disconnect = function() {
            if (is.null(private$conn)) {
                return(invisible(NULL))
            }
            if (DBI::dbIsValid(private$conn)) {
                tryCatch(
                    DBI::dbDisconnect(private$conn, shutdown = TRUE),
                    error = function(e) DBI::dbDisconnect(private$conn)
                )
            }
            private$conn <- NULL
            invisible(NULL)
        },
        # }}}

        # init_schema {{{
        init_schema = function() {
            private$exec("
                CREATE TABLE IF NOT EXISTS store_meta (
                    key VARCHAR PRIMARY KEY,
                    value VARCHAR,
                    updated_at TIMESTAMP
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS artifact (
                    artifact_id VARCHAR PRIMARY KEY,
                    kind VARCHAR,
                    role VARCHAR,
                    project VARCHAR,
                    relative_path VARCHAR,
                    checksum VARCHAR,
                    checksum_type VARCHAR,
                    size DOUBLE,
                    status VARCHAR,
                    query_id VARCHAR,
                    file_key VARCHAR,
                    dict_id VARCHAR,
                    source_url VARCHAR,
                    source_repo VARCHAR,
                    source_tag VARCHAR,
                    source_commit VARCHAR,
                    metadata_json VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS query_run (
                    query_id VARCHAR PRIMARY KEY,
                    label VARCHAR,
                    result_type VARCHAR,
                    query_file VARCHAR,
                    index_node VARCHAR,
                    time_filter_start TIMESTAMP,
                    time_filter_stop TIMESTAMP,
                    time_filter_method VARCHAR,
                    created_at TIMESTAMP,
                    package_version VARCHAR
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS esg_query (
                    query_id VARCHAR PRIMARY KEY,
                    label VARCHAR,
                    index_node VARCHAR,
                    query_file VARCHAR,
                    parameter_json VARCHAR,
                    tracked BOOLEAN,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP,
                    last_checked_at TIMESTAMP,
                    package_version VARCHAR
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS esg_file (
                    file_key VARCHAR PRIMARY KEY,
                    esgf_id VARCHAR,
                    dataset_id VARCHAR,
                    master_id VARCHAR,
                    instance_id VARCHAR,
                    tracking_id VARCHAR,
                    version VARCHAR,
                    title VARCHAR,
                    filename VARCHAR,
                    checksum VARCHAR,
                    checksum_type VARCHAR,
                    size DOUBLE,
                    latest BOOLEAN,
                    replica BOOLEAN,
                    retracted BOOLEAN,
                    deprecated BOOLEAN,
                    data_node VARCHAR,
                    source_id VARCHAR,
                    experiment_id VARCHAR,
                    variant_label VARCHAR,
                    frequency VARCHAR,
                    table_id VARCHAR,
                    variable_id VARCHAR,
                    grid_label VARCHAR,
                    datetime_start TIMESTAMP,
                    datetime_end TIMESTAMP,
                    url_opendap VARCHAR,
                    url_download VARCHAR,
                    local_path VARCHAR,
                    local_artifact_id VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS esg_query_file (
                    link_id VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    file_key VARCHAR,
                    status VARCHAR,
                    first_seen_at TIMESTAMP,
                    last_seen_at TIMESTAMP
                )
            ")
            private$exec("ALTER TABLE esg_file ADD COLUMN IF NOT EXISTS deprecated BOOLEAN")
            private$exec("
                CREATE TABLE IF NOT EXISTS file_catalog (
                    file_key VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    esgf_id VARCHAR,
                    dataset_id VARCHAR,
                    title VARCHAR,
                    filename VARCHAR,
                    tracking_id VARCHAR,
                    checksum VARCHAR,
                    checksum_type VARCHAR,
                    size DOUBLE,
                    data_node VARCHAR,
                    source_id VARCHAR,
                    experiment_id VARCHAR,
                    variant_label VARCHAR,
                    frequency VARCHAR,
                    table_id VARCHAR,
                    variable_id VARCHAR,
                    grid_label VARCHAR,
                    datetime_start TIMESTAMP,
                    datetime_end TIMESTAMP,
                    actual_time_start TIMESTAMP,
                    actual_time_end TIMESTAMP,
                    url_opendap VARCHAR,
                    url_download VARCHAR,
                    local_path VARCHAR,
                    local_artifact_id VARCHAR,
                    created_at TIMESTAMP
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS extraction_plan (
                    plan_id VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    file_key VARCHAR,
                    site_id VARCHAR,
                    variable_id VARCHAR,
                    lon DOUBLE,
                    lat DOUBLE,
                    nearest INTEGER,
                    time_start TIMESTAMP,
                    time_stop TIMESTAMP,
                    status VARCHAR,
                    available_time_count INTEGER,
                    attempt_count INTEGER,
                    last_error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS extraction_result (
                    result_id VARCHAR PRIMARY KEY,
                    plan_id VARCHAR,
                    file_key VARCHAR,
                    query_id VARCHAR,
                    artifact_id VARCHAR,
                    output_path VARCHAR,
                    year INTEGER,
                    row_count INTEGER,
                    unique_time_count INTEGER,
                    time_min TIMESTAMP,
                    time_max TIMESTAMP,
                    lon_actual DOUBLE,
                    lat_actual DOUBLE,
                    dist_min DOUBLE,
                    completed_at TIMESTAMP
                )
            ")

            private$replace_rows("store_meta", data.frame(
                key = "schema_version",
                value = "2",
                updated_at = extract_store_now(),
                stringsAsFactors = FALSE
            ), "key")

            invisible(NULL)
        },
        # }}}

        # exec {{{
        exec = function(sql) {
            DBI::dbExecute(private$conn, sql)
        },
        # }}}

        # read_table {{{
        read_table = function(table) {
            data.table::as.data.table(DBI::dbReadTable(private$conn, table))
        },
        # }}}

        # check_open {{{
        check_open = function() {
            if (!isTRUE(self$is_open)) {
                cli::cli_abort("The store is closed.")
            }

            invisible(NULL)
        },
        # }}}

        # query_payload {{{
        query_payload = function(query) {
            state <- query$state(null = TRUE)
            parameter <- priv(query)$parameter$serialize(null = TRUE)
            parameter_json <- jsonlite::toJSON(parameter, auto_unbox = TRUE, null = "null", digits = 6)
            list(
                query_id = extract_store_hash("EsgQuery", state$index_node, parameter_json),
                index_node = state$index_node,
                parameter_json = as.character(parameter_json)
            )
        },
        # }}}

        # set_query_tracked {{{
        set_query_tracked = function(query_id, tracked) {
            private$check_open()
            checkmate::assert_string(query_id, min.chars = 1L)
            checkmate::assert_flag(tracked)
            qid <- query_id
            queries <- private$read_table("esg_query")
            row <- queries[queries[["query_id"]] == qid]
            if (!nrow(row)) {
                cli::cli_abort("Stored ESGF query {.val {query_id}} was not found.")
            }
            row$tracked <- isTRUE(tracked)
            row$updated_at <- extract_store_now()
            private$replace_rows("esg_query", as.data.frame(row), "query_id")
            invisible(NULL)
        },
        # }}}

        # select_query_rows {{{
        select_query_rows = function(query_id = NULL, tracked = NULL) {
            queries <- private$read_table("esg_query")
            if (!nrow(queries)) {
                return(queries[])
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
        },
        # }}}

        # get_query_row {{{
        get_query_row = function(query_id) {
            rows <- private$select_query_rows(query_id = query_id)
            if (!nrow(rows)) {
                cli::cli_abort("Stored ESGF query {.val {query_id}} was not found.")
            }
            rows[1L]
        },
        # }}}

        # load_query {{{
        load_query = function(row) {
            file <- store_abs_path(row$query_file[[1L]], root = private$store_path)
            esg_query()$load(file)
        },
        # }}}

        # update_query_files {{{
        update_query_files = function(query_id, files) {
            extract_store_result_type(files)
            dt <- extract_store_file_table(files)
            now <- extract_store_now()
            file_rows <- private$file_rows(dt, now)

            if (nrow(file_rows)) {
                private$replace_rows("esg_file", as.data.frame(file_rows), "file_key")
                private$sync_file_catalog(query_id, file_rows)
            }

            private$sync_query_file_links(query_id, file_rows, now)
            query <- private$get_query_row(query_id)
            query$last_checked_at <- now
            query$updated_at <- now
            private$replace_rows("esg_query", as.data.frame(query), "query_id")
            self$query_files(query_id)
        },
        # }}}

        # file_rows {{{
        file_rows = function(dt, now) {
            if (!nrow(dt)) {
                return(data.table::data.table())
            }
            dt <- data.table::as.data.table(dt)
            dt[, file_key := extract_store_file_keys(.SD)]
            dt[, `:=`(
                row_order = seq_len(.N),
                replica_order = data.table::fifelse(extract_store_as_logical(replica) %in% TRUE, 1L, 0L),
                retracted_order = data.table::fifelse(extract_store_as_logical(retracted) %in% TRUE, 1L, 0L),
                deprecated_order = data.table::fifelse(extract_store_as_logical(deprecated) %in% TRUE, 1L, 0L)
            )]
            data.table::setorderv(dt, c("file_key", "retracted_order", "deprecated_order", "replica_order", "row_order"))
            dt <- dt[!duplicated(file_key)]

            existing <- private$read_table("esg_file")
            existing <- existing[existing[["file_key"]] %in% dt$file_key]
            existing_created <- stats::setNames(existing$created_at, existing$file_key)
            existing_local_path <- stats::setNames(existing$local_path, existing$file_key)
            existing_artifact <- stats::setNames(existing$local_artifact_id, existing$file_key)

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
                latest = extract_store_as_logical(dt$latest),
                replica = extract_store_as_logical(dt$replica),
                retracted = extract_store_as_logical(dt$retracted),
                deprecated = extract_store_as_logical(dt$deprecated),
                data_node = dt$data_node,
                source_id = dt$source_id,
                experiment_id = dt$experiment_id,
                variant_label = dt$variant_label,
                frequency = dt$frequency,
                table_id = dt$table_id,
                variable_id = dt$variable_id,
                grid_label = dt$grid_label,
                datetime_start = extract_store_parse_datetime(dt$datetime_start),
                datetime_end = extract_store_parse_datetime(dt$datetime_end),
                url_opendap = dt$url_opendap,
                url_download = dt$url_download,
                local_path = extract_store_match_named(existing_local_path, dt$file_key),
                local_artifact_id = extract_store_match_named(existing_artifact, dt$file_key),
                created_at = extract_store_match_time(existing_created, dt$file_key, now),
                updated_at = now
            )
        },
        # }}}

        # sync_query_file_links {{{
        sync_query_file_links = function(query_id, file_rows, now) {
            qid <- query_id
            current_keys <- if (nrow(file_rows)) unique(file_rows$file_key) else character()
            links <- private$read_table("esg_query_file")
            existing <- links[links[["query_id"]] == qid]

            if (nrow(existing)) {
                missing <- existing[!existing[["file_key"]] %in% current_keys]
                if (nrow(missing)) {
                    missing$status <- "missing"
                    missing$last_seen_at <- now
                    private$replace_rows("esg_query_file", as.data.frame(missing), "link_id")
                }
            }

            if (!length(current_keys)) {
                return(invisible(NULL))
            }

            existing_first_seen <- stats::setNames(existing$first_seen_at, existing$file_key)
            rows <- data.table::data.table(
                link_id = vapply(current_keys, function(file_key) extract_store_hash(qid, file_key), character(1L)),
                query_id = qid,
                file_key = current_keys,
                status = private$file_link_status(file_rows[match(current_keys, file_rows$file_key)]),
                first_seen_at = extract_store_match_time(existing_first_seen, current_keys, now),
                last_seen_at = now
            )
            private$replace_rows("esg_query_file", as.data.frame(rows), "link_id")
            invisible(NULL)
        },
        # }}}

        # file_link_status {{{
        file_link_status = function(file_rows) {
            status <- rep("current", nrow(file_rows))
            deprecated <- extract_store_as_logical(file_rows$deprecated)
            retracted <- extract_store_as_logical(file_rows$retracted)
            status[deprecated %in% TRUE] <- "deprecated"
            status[retracted %in% TRUE] <- "retracted"
            status
        },
        # }}}

        # sync_file_catalog {{{
        sync_file_catalog = function(query_id, file_rows) {
            if (!nrow(file_rows)) {
                return(invisible(NULL))
            }
            active <- file_rows[private$file_link_status(file_rows) == "current"]
            if (!nrow(active)) {
                return(invisible(NULL))
            }
            now <- extract_store_now()
            catalog <- data.frame(
                file_key = active$file_key,
                query_id = query_id,
                esgf_id = active$esgf_id,
                dataset_id = active$dataset_id,
                title = active$title,
                filename = active$filename,
                tracking_id = active$tracking_id,
                checksum = active$checksum,
                checksum_type = active$checksum_type,
                size = suppressWarnings(as.numeric(active$size)),
                data_node = active$data_node,
                source_id = active$source_id,
                experiment_id = active$experiment_id,
                variant_label = active$variant_label,
                frequency = active$frequency,
                table_id = active$table_id,
                variable_id = active$variable_id,
                grid_label = active$grid_label,
                datetime_start = active$datetime_start,
                datetime_end = active$datetime_end,
                actual_time_start = extract_store_parse_datetime(rep(NA_character_, nrow(active))),
                actual_time_end = extract_store_parse_datetime(rep(NA_character_, nrow(active))),
                url_opendap = active$url_opendap,
                url_download = active$url_download,
                local_path = active$local_path,
                local_artifact_id = active$local_artifact_id,
                created_at = rep(now, nrow(active)),
                stringsAsFactors = FALSE
            )
            private$replace_rows("file_catalog", catalog, "file_key")
            invisible(NULL)
        },
        # }}}

        # replace_rows {{{
        replace_rows = function(table, rows, key) {
            checkmate::assert_string(table)
            checkmate::assert_string(key)
            if (!nrow(rows)) {
                return(invisible(NULL))
            }

            keys <- unique(rows[[key]])
            if (!length(keys)) {
                return(invisible(NULL))
            }

            private$delete_by_key(table, key, keys)
            DBI::dbAppendTable(private$conn, table, rows)
            invisible(NULL)
        },
        # }}}

        # append_new_rows {{{
        append_new_rows = function(table, rows, key) {
            checkmate::assert_string(table)
            checkmate::assert_string(key)
            if (!nrow(rows)) {
                return(invisible(NULL))
            }

            q_table <- DBI::dbQuoteIdentifier(private$conn, table)
            q_key <- DBI::dbQuoteIdentifier(private$conn, key)
            current <- DBI::dbGetQuery(private$conn, sprintf("SELECT %s FROM %s", q_key, q_table))[[key]]
            rows <- rows[!rows[[key]] %in% current, , drop = FALSE]
            if (nrow(rows)) {
                DBI::dbAppendTable(private$conn, table, rows)
            }

            invisible(NULL)
        },
        # }}}

        # delete_by_key {{{
        delete_by_key = function(table, key, values) {
            checkmate::assert_string(table)
            checkmate::assert_string(key)
            values <- unique(as.character(values))
            values <- values[!is.na(values)]
            if (!length(values)) {
                return(invisible(NULL))
            }

            q_table <- DBI::dbQuoteIdentifier(private$conn, table)
            q_key <- DBI::dbQuoteIdentifier(private$conn, key)
            q_values <- paste(DBI::dbQuoteString(private$conn, values), collapse = ", ")
            DBI::dbExecute(private$conn, sprintf("DELETE FROM %s WHERE %s IN (%s)", q_table, q_key, q_values))
            invisible(NULL)
        },
        # }}}

        # extract_one {{{
        extract_one = function(plan, file, fallback = "auto", overwrite = FALSE) {
            opened <- private$open_plan_dataset(file, fallback = fallback, overwrite = overwrite)
            ds <- opened$dataset
            on.exit(if (isTRUE(ds$is_open)) ds$close(), add = TRUE)

            time_axis <- ds$get_time_axis(index = 1L)$values
            valid_time <- time_axis[!is.na(time_axis)]
            if (!length(valid_time)) {
                stop("The NetCDF time axis is empty or unavailable.", call. = FALSE)
            }
            actual_start <- min(valid_time)
            actual_end <- max(valid_time)
            requested_time <- c(plan$time_start[[1L]], plan$time_stop[[1L]])
            available_time_count <- sum(valid_time >= requested_time[[1L]] & valid_time <= requested_time[[2L]])
            private$update_file_actual_time(file, actual_start, actual_end)

            dt <- ds$read_region(
                variable = plan$variable_id[[1L]],
                lon = plan$lon[[1L]],
                lat = plan$lat[[1L]],
                time = requested_time,
                nearest = plan$nearest[[1L]]
            )
            if (!nrow(dt)) {
                return(private$mark_plan_status(
                    plan,
                    status = "empty",
                    available_time_count = available_time_count,
                    last_error = NA_character_
                ))
            }

            private$decorate_extract(dt, plan, file)
            results <- private$write_extract_partitions(dt, plan, file, overwrite = overwrite)
            private$delete_by_key("extraction_result", "plan_id", plan$plan_id)
            if (nrow(results)) {
                DBI::dbAppendTable(private$conn, "extraction_result", results)
            }

            private$mark_plan_status(
                plan,
                status = "done",
                available_time_count = available_time_count,
                last_error = NA_character_
            )
        },
        # }}}

        # open_plan_dataset {{{
        open_plan_dataset = function(file, fallback = "auto", overwrite = FALSE) {
            opendap <- extract_store_na_character(file$url_opendap)
            if (!is.na(opendap) && nzchar(opendap)) {
                ds <- EsgDataset$new(opendap)
                open_error <- NULL
                ok <- tryCatch(
                    {
                        ds$open()
                        TRUE
                    },
                    error = function(e) {
                        open_error <<- e
                        FALSE
                    }
                )
                if (isTRUE(ok)) {
                    return(list(dataset = ds, target = opendap))
                }
                if (identical(fallback, "error")) {
                    stop(open_error)
                }
                if (isTRUE(ds$is_open)) {
                    ds$close()
                }
            }

            if (identical(fallback, "error")) {
                stop("OPeNDAP is not available for this file record.", call. = FALSE)
            }

            local_path <- private$download_plan_file(file, overwrite = overwrite)
            ds <- EsgDataset$new(local_path)
            ds$open()
            list(dataset = ds, target = local_path)
        },
        # }}}

        # download_plan_file {{{
        download_plan_file = function(file, overwrite = FALSE) {
            download <- extract_store_na_character(file$url_download)
            if (is.na(download) || !nzchar(download)) {
                stop("HTTPServer download URL is not available for this file record.", call. = FALSE)
            }

            checksum <- extract_store_na_character(file$checksum)
            checksum_type <- tolower(extract_store_na_character(file$checksum_type))
            if (is.na(checksum_type) || !checksum_type %in% c("md5", "sha256")) {
                checksum <- NULL
                checksum_type <- "sha256"
            }
            if (is.na(checksum)) {
                checksum <- NULL
            }

            downloader <- self$downloader(n_workers = 0L)
            filename <- extract_store_na_character(file$filename[[1L]])
            if (is.na(filename)) {
                filename <- basename(sub("\\?.*$", "", download))
            }
            logical_parts <- c(
                extract_store_na_character(file$tracking_id[[1L]]),
                if (is.null(checksum)) NA_character_ else checksum,
                filename,
                extract_store_na_character(file$file_key[[1L]])
            )
            logical_parts <- logical_parts[!is.na(logical_parts) & nzchar(logical_parts)]
            logical_file_id <- paste(logical_parts, collapse = ":")
            if (!nzchar(logical_file_id)) {
                logical_file_id <- file$file_key[[1L]]
            }
            plan <- data.table::data.table(
                logical_file_id = logical_file_id,
                file_key = file$file_key[[1L]],
                esgf_id = file$esgf_id[[1L]],
                dataset_id = file$dataset_id[[1L]],
                filename = filename,
                subdir = NA_character_,
                checksum = if (is.null(checksum)) NA_character_ else checksum,
                checksum_type = checksum_type,
                size = suppressWarnings(as.numeric(file$size[[1L]])),
                url = download,
                service = "HTTPServer",
                data_node = file$data_node[[1L]],
                priority = 1L,
                probe_latency = NA_real_,
                probe_throughput = NA_real_
            )
            session_id <- downloader$enqueue(plan, session_label = sprintf("extract:%s", file$file_key[[1L]]))
            tasks <- downloader$run(session_id = session_id, progress = FALSE, overwrite = overwrite)
            failed <- tasks[!tasks[["status"]] %in% c("done", "skipped")]
            if (nrow(failed)) {
                stop("HTTPServer download failed for this file record.", call. = FALSE)
            }
            self$sync_downloads(downloader)
            tasks$target_path[[1L]]
        },
        # }}}

        # decorate_extract {{{
        decorate_extract = function(dt, plan, file) {
            dt[, `:=`(
                plan_id = plan$plan_id[[1L]],
                file_key = plan$file_key[[1L]],
                query_id = plan$query_id[[1L]],
                site_id = plan$site_id[[1L]],
                requested_lon = plan$lon[[1L]],
                requested_lat = plan$lat[[1L]],
                source_id = file$source_id[[1L]],
                experiment_id = file$experiment_id[[1L]],
                variant_label = file$variant_label[[1L]],
                frequency = file$frequency[[1L]],
                table_id = file$table_id[[1L]],
                variable_id = plan$variable_id[[1L]],
                grid_label = file$grid_label[[1L]]
            )]
            data.table::setcolorder(dt, c(
                "plan_id", "file_key", "query_id", "site_id",
                "source_id", "experiment_id", "variant_label", "frequency",
                "table_id", "variable_id", "variable", "grid_label",
                "time", "lon", "lat", "dist", "value",
                setdiff(names(dt), c(
                    "plan_id", "file_key", "query_id", "site_id",
                    "source_id", "experiment_id", "variant_label", "frequency",
                    "table_id", "variable_id", "variable", "grid_label",
                    "time", "lon", "lat", "dist", "value"
                ))
            ))
            invisible(dt)
        },
        # }}}

        # write_extract_partitions {{{
        write_extract_partitions = function(dt, plan, file, overwrite = FALSE) {
            dt[, year := as.integer(format(time, "%Y", tz = "UTC"))]
            years <- sort(unique(dt$year))
            results <- vector("list", length(years))
            for (i in seq_along(years)) {
                year <- years[[i]]
                chunk <- dt[dt$year == year]
                output_path <- private$output_path(plan, file, year)
                private$write_parquet(chunk, output_path, overwrite = overwrite)
                artifact_id <- self$register_artifact(
                    kind = "extract",
                    path = output_path,
                    role = "derived",
                    project = "CMIP6",
                    query_id = plan$query_id[[1L]],
                    file_key = plan$file_key[[1L]],
                    metadata = list(
                        plan_id = plan$plan_id[[1L]],
                        year = as.integer(year)
                    )
                )
                results[[i]] <- private$extract_result_row(plan, chunk, output_path, year, artifact_id)
            }

            data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
        },
        # }}}

        # output_path {{{
        output_path = function(plan, file, year) {
            parts <- c(
                project = "CMIP6",
                source_id = file$source_id[[1L]],
                experiment_id = file$experiment_id[[1L]],
                variant_label = file$variant_label[[1L]],
                frequency = file$frequency[[1L]],
                variable_id = plan$variable_id[[1L]],
                year = year
            )
            dirs <- paste0(names(parts), "=", vapply(parts, extract_store_partition_value, character(1L)))
            file.path(private$extract_dir, do.call(file.path, as.list(dirs)), sprintf("part-%s.parquet", plan$plan_id[[1L]]))
        },
        # }}}

        # write_parquet {{{
        write_parquet = function(dt, path, overwrite = FALSE) {
            if (file.exists(path) && !isTRUE(overwrite)) {
                stop(sprintf("Parquet output already exists: %s.", path), call. = FALSE)
            }
            dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
            tmp_file <- tempfile(tmpdir = dirname(path), fileext = ".parquet")
            tmp_table <- sprintf("tmp_extract_%s", substr(extract_store_hash(path, Sys.time(), runif(1L)), 1L, 16L))
            on.exit({
                try(DBI::dbExecute(private$conn, sprintf("DROP TABLE IF EXISTS %s", DBI::dbQuoteIdentifier(private$conn, tmp_table))), silent = TRUE)
                if (file.exists(tmp_file)) {
                    unlink(tmp_file)
                }
            }, add = TRUE)

            DBI::dbWriteTable(private$conn, tmp_table, as.data.frame(dt), temporary = TRUE, overwrite = TRUE)
            DBI::dbExecute(private$conn, sprintf(
                "COPY %s TO %s (FORMAT PARQUET)",
                DBI::dbQuoteIdentifier(private$conn, tmp_table),
                DBI::dbQuoteString(private$conn, tmp_file)
            ))
            if (file.exists(path) && isTRUE(overwrite)) {
                unlink(path)
            }
            ok <- file.rename(tmp_file, path)
            if (!isTRUE(ok)) {
                stop(sprintf("Failed to move Parquet output into place: %s.", path), call. = FALSE)
            }

            invisible(path)
        },
        # }}}

        # extract_result_row {{{
        extract_result_row = function(plan, dt, output_path, year, artifact_id) {
            data.frame(
                result_id = extract_store_hash(plan$plan_id[[1L]], year, output_path),
                plan_id = plan$plan_id[[1L]],
                file_key = plan$file_key[[1L]],
                query_id = plan$query_id[[1L]],
                artifact_id = artifact_id,
                output_path = extract_store_rel_path(output_path, private$store_path),
                year = as.integer(year),
                row_count = nrow(dt),
                unique_time_count = data.table::uniqueN(dt$time),
                time_min = min(dt$time),
                time_max = max(dt$time),
                lon_actual = dt$lon[[which.min(dt$dist)]],
                lat_actual = dt$lat[[which.min(dt$dist)]],
                dist_min = min(dt$dist, na.rm = TRUE),
                completed_at = extract_store_now(),
                stringsAsFactors = FALSE
            )
        },
        # }}}

        # update_file_actual_time {{{
        update_file_actual_time = function(file, start, end) {
            file$actual_time_start <- start
            file$actual_time_end <- end
            private$replace_rows("file_catalog", as.data.frame(file), "file_key")
            invisible(NULL)
        },
        # }}}

        # mark_plan_failed {{{
        mark_plan_failed = function(plan, message) {
            private$mark_plan_status(
                plan,
                status = "failed",
                available_time_count = plan$available_time_count[[1L]],
                last_error = message,
                increment_attempt = TRUE
            )
        },
        # }}}

        # mark_plan_status {{{
        mark_plan_status = function(plan, status, available_time_count = NULL,
                                    last_error = NA_character_, increment_attempt = FALSE) {
            plan <- as.data.frame(plan)
            plan$status <- status
            if (!is.null(available_time_count)) {
                plan$available_time_count <- as.integer(available_time_count)
            }
            if (isTRUE(increment_attempt)) {
                plan$attempt_count <- as.integer(plan$attempt_count) + 1L
            }
            plan$last_error <- extract_store_na_character(last_error)
            plan$updated_at <- extract_store_now()
            private$replace_rows("extraction_plan", plan, "plan_id")
            data.table::as.data.table(plan)
        },
        # }}}

        # finalize {{{
        finalize = function() {
            private$disconnect()
        }
        # }}}
    )
)
# }}}

# extract-store helpers {{{
extract_store_result_type <- function(files) {
    if (inherits(files, "EsgResultFile")) {
        return("File")
    }
    if (inherits(files, "EsgResultAggregation")) {
        return("Aggregation")
    }

    cli::cli_abort("`files` must be an EsgResultFile or EsgResultAggregation object.")
}

extract_store_now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}

extract_store_hash <- function(...) {
    text <- paste(vapply(list(...), extract_store_hash_piece, character(1L)), collapse = "\n")
    paste0(openssl::sha256(charToRaw(text)))
}

extract_store_hash_piece <- function(x) {
    if (is.null(x)) {
        return("<NULL>")
    }

    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    paste(x, collapse = "\r")
}

extract_store_na_character <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) {
        return(NA_character_)
    }

    as.character(x[[1L]])
}

extract_store_is_true <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) {
        return(FALSE)
    }

    isTRUE(as.logical(x[[1L]]))
}

extract_store_list_value <- function(x, name) {
    if (is.null(x) || !is.list(x) || !name %in% names(x)) {
        return(NA_character_)
    }

    x[[name]]
}

extract_store_parse_datetime_scalar <- function(x) {
    extract_store_parse_datetime(extract_store_na_character(x))[[1L]]
}

extract_store_parse_datetime <- function(x) {
    if (inherits(x, "POSIXt")) {
        return(as.POSIXct(x, tz = "UTC"))
    }
    x <- as.character(x)
    out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) {
        out[ok] <- parse_datetime(x[ok], tz = "UTC")
    }

    out
}

extract_store_parse_time_range <- function(time) {
    checkmate::assert_atomic_vector(time, len = 2L, any.missing = FALSE)
    parsed <- extract_store_parse_datetime(time)
    if (any(is.na(parsed))) {
        cli::cli_abort("`time` must be a length-2 parseable datetime range.")
    }
    if (parsed[[2L]] < parsed[[1L]]) {
        cli::cli_abort("The second `time` value must be greater than or equal to the first.")
    }

    list(start = parsed[[1L]], stop = parsed[[2L]])
}

extract_store_rel_path <- function(path, root) {
    path <- normalizePath(path, mustWork = FALSE)
    root <- normalizePath(root, mustWork = TRUE)
    prefix <- paste0(root, .Platform$file.sep)
    if (startsWith(path, prefix)) {
        return(substring(path, nchar(prefix) + 1L))
    }

    path
}

extract_store_scalar_column <- function(dt, name, n = nrow(dt)) {
    if (!name %in% names(dt)) {
        return(rep(NA_character_, n))
    }

    value <- dt[[name]]
    if (is.list(value)) {
        value <- vapply(value, extract_store_na_character, character(1L))
    } else {
        value <- as.character(value)
        value[is.na(value)] <- NA_character_
    }

    value
}

extract_store_column_value <- function(dt, name, i) {
    if (!name %in% names(dt)) {
        return(NA_character_)
    }

    extract_store_na_character(dt[[name]][[i]])
}

extract_store_as_logical <- function(x) {
    if (is.null(x)) {
        return(logical())
    }
    if (is.logical(x)) {
        return(x)
    }
    value <- tolower(as.character(x))
    out <- rep(NA, length(value))
    out[value %in% c("true", "t", "1", "yes", "y")] <- TRUE
    out[value %in% c("false", "f", "0", "no", "n")] <- FALSE
    out
}

extract_store_match_named <- function(values, keys) {
    if (!length(values)) {
        return(rep(NA_character_, length(keys)))
    }
    out <- unname(values[as.character(keys)])
    out[is.na(out)] <- NA_character_
    as.character(out)
}

extract_store_match_time <- function(values, keys, default) {
    if (!length(values)) {
        return(rep(default, length(keys)))
    }
    out <- unname(values[as.character(keys)])
    out[is.na(out)] <- default
    out
}

extract_store_file_table <- function(files) {
    extract_store_result_type(files)
    dt <- data.table::copy(files$to_data_table())
    n <- nrow(dt)
    columns <- c(
        "id", "dataset_id", "master_id", "instance_id", "title",
        "filename", "tracking_id", "version", "checksum", "checksum_type",
        "size", "latest", "replica", "retracted", "deprecated",
        "data_node", "source_id", "experiment_id", "variant_label",
        "frequency", "table_id", "variable_id", "grid_label",
        "datetime_start", "datetime_end", "url_opendap", "url_download"
    )

    out <- data.table::as.data.table(
        stats::setNames(rep(list(rep(NA_character_, n)), length(columns)), columns)
    )
    for (name in columns) {
        out[[name]] <- extract_store_scalar_column(dt, name, n)
    }
    if (n && all(is.na(out$filename)) && any(!is.na(out$title))) {
        out$filename <- basename(out$title)
    }

    out[]
}

extract_store_file_keys <- function(dt) {
    if (!nrow(dt)) {
        return(character())
    }

    vapply(seq_len(nrow(dt)), function(i) {
        master_id <- extract_store_column_value(dt, "master_id", i)
        if (!is.na(master_id) && nzchar(master_id)) {
            return(paste0("master:", master_id))
        }

        tracking_id <- extract_store_column_value(dt, "tracking_id", i)
        if (!is.na(tracking_id) && nzchar(tracking_id)) {
            return(paste0("tracking:", tracking_id))
        }

        checksum <- extract_store_column_value(dt, "checksum", i)
        size <- extract_store_column_value(dt, "size", i)
        filename <- extract_store_column_value(dt, "filename", i)
        if (!is.na(checksum) && nzchar(checksum) && !is.na(filename) && nzchar(filename)) {
            return(paste("checksum", checksum, size, filename, sep = ":"))
        }

        id <- extract_store_column_value(dt, "id", i)
        if (!is.na(id) && nzchar(id)) {
            return(paste0("id:", id))
        }

        pieces <- unlist(dt[i, c("url_opendap", "url_download", "title"), with = FALSE], use.names = FALSE)
        if (all(is.na(pieces) | !nzchar(pieces))) {
            cli::cli_abort("Cannot create a stable file key because file record {i} has no master ID, tracking ID, checksum, ID, URL, or title.")
        }
        paste0("fallback:", extract_store_hash(pieces))
    }, character(1L))
}

extract_store_partition_value <- function(x) {
    x <- extract_store_na_character(x)
    if (is.na(x) || !nzchar(x)) {
        return("__missing__")
    }

    x <- gsub("[/\\\\:]+", "_", x)
    x <- gsub("[^A-Za-z0-9_.=-]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- sub("^_+", "", sub("_+$", "", x))
    if (!nzchar(x)) {
        return("__missing__")
    }

    x
}

extract_store_summary_columns <- function() {
    c(
        query_id = "p.query_id",
        plan_id = "p.plan_id",
        file_key = "p.file_key",
        site_id = "p.site_id",
        source_id = "f.source_id",
        experiment_id = "f.experiment_id",
        variant_label = "f.variant_label",
        frequency = "f.frequency",
        table_id = "f.table_id",
        variable_id = "p.variable_id",
        grid_label = "f.grid_label",
        data_node = "f.data_node",
        year = "r.year",
        status = "p.status"
    )
}
# }}}
