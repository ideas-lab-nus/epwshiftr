STORE_SCHEMA_VERSION <- "2.5.0"
STORE_DOWNLOAD_LAYOUT_DEFAULT <- list(
    layout = "flat",
    template = NULL,
    include_version = TRUE,
    collision = "error",
    missing = "fallback"
)
STORE_DOWNLOAD_LAYOUT_CHOICES <- c("flat", "dataset", "drs", "template")
STORE_DOWNLOAD_COLLISION_CHOICES <- c("error", "checksum", "suffix")
STORE_DOWNLOAD_MISSING_CHOICES <- c("fallback", "error")

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
            checkmate::assert_string(path, null.ok = TRUE)
            checkmate::assert_flag(create)
            checkmate::assert_flag(overwrite)
            if (is.null(path)) {
                path <- store_dir(init = create)
            }

            path <- path.expand(path)
            if (dir.exists(path) && overwrite) {
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

            meta <- ddb_read_table(private$conn, "store_meta")
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

            private$with_store_lock({
            private$replace_rows(
                "store_meta",
                data.frame(
                    key = key,
                    value = store__chr1(value),
                    updated_at = store__now(),
                    stringsAsFactors = FALSE
                ),
                "key"
            )
            })
            invisible(self)
        },
        # }}}

        # download_layout {{{
        #' @description
        #' Return the store download layout policy.
        #'
        #' @return A named list describing how store downloads are placed under
        #'         `downloads/`.
        download_layout = function() {
            private$check_open()
            private$download_layout_policy()
        },
        # }}}

        # set_download_layout {{{
        #' @description
        #' Configure how store-managed ESGF downloads are placed under
        #' `downloads/`.
        #'
        #' @param layout Download layout. `"flat"` stores files directly under
        #'        `downloads/`; `"dataset"` groups by dataset; `"drs"` uses a
        #'        CMIP6-style DRS path; `"template"` uses `template`.
        #' @param template Optional subdirectory template for `layout =
        #'        "template"`, using placeholders such as `{source_id}`.
        #' @param include_version Whether DRS paths include the ESGF version.
        #'        Default: `TRUE`.
        #' @param collision How to handle different logical files that map to
        #'        the same local path. Default: `"error"`.
        #' @param missing How to handle missing layout fields. Default:
        #'        `"fallback"`.
        #'
        #' @return The store object, invisibly.
        set_download_layout = function(
            layout = c("flat", "dataset", "drs", "template"),
            template = NULL,
            include_version = TRUE,
            collision = c("error", "checksum", "suffix"),
            missing = c("fallback", "error")
        ) {
            private$check_open()
            policy <- private$normalize_download_layout_policy(list(
                layout = match.arg(layout),
                template = template,
                include_version = include_version,
                collision = match.arg(collision),
                missing = match.arg(missing)
            ))
            private$with_store_lock({
            private$replace_rows(
                "store_meta",
                data.frame(
                    key = "download_layout",
                    value = jsonlite::toJSON(policy, auto_unbox = TRUE, null = "null"),
                    updated_at = store__now(),
                    stringsAsFactors = FALSE
                ),
                "key"
            )
            })
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
        register_artifact = function(
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
            checkmate::assert_choice(kind, c("query", "dict", "source", "cmip6_index", "netcdf", "extract", "output"))
            checkmate::assert_string(path, min.chars = 1L)
            checkmate::assert_choice(status, c("planned", "available", "failed", "missing"))
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
            checkmate::assert_choice(role, c("input", "source", "download", "derived", "output"))

            if (is.null(size) && exists) {
                size <- as.numeric(file.info(path, extra_cols = FALSE)$size)
            }
            if (is.null(checksum) && exists && identical(status, "available")) {
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

            artifact_id <- store__hash(kind, rel_path, checksum, file_key, dict_id, source_commit)
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
                metadata_json = jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null"),
                created_at = now,
                updated_at = now,
                stringsAsFactors = FALSE
            )
            private$replace_rows("artifact", row, "artifact_id")
            artifact_id
            })
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

            artifacts <- ddb_read_table(private$conn, "artifact")
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
            artifacts <- data.table::as.data.table(ddb_read_table(private$conn, "artifact"))
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

            artifacts[,
                expected_path := vapply(relative_path, store_abs_path, character(1L), root = private$store_path)
            ]
            artifacts[, exists := file.exists(expected_path)]
            artifacts[,
                checksum_ok := mapply(
                    function(path, ok, checksum, checksum_type) {
                        if (!isTRUE(ok) || is.na(checksum) || !nzchar(checksum)) {
                            return(NA)
                        }
                        identical(tolower(store_hash_file(path, checksum_type)), tolower(checksum))
                    },
                    expected_path,
                    exists,
                    checksum,
                    checksum_type
                )
            ]
            artifacts[,
                size_ok := mapply(
                    function(path, ok, size) {
                        if (!isTRUE(ok) || is.na(size)) {
                            return(NA)
                        }
                        identical(as.numeric(file.info(path, extra_cols = FALSE)$size), as.numeric(size))
                    },
                    expected_path,
                    exists,
                    size
                )
            ]
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

            private$with_store_lock({
            payload <- private$query_payload(query)
            query_id <- payload$query_id
            qid <- query_id
            query_file <- file.path(private$query_dir, sprintf("query-%s.json", query_id))
            query$save(query_file)

            now <- store__now()
            queries <- private$read_table("esg_query")
            existing <- queries[queries[["query_id"]] == qid]
            created_at <- if (nrow(existing)) existing$created_at[[1L]] else now
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

        # tag_query {{{
        #' @description
        #' Add tags to a stored ESGF query.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #' @param tag Character vector of tags.
        #' @param replace Whether to replace existing tags for the query.
        #'
        #' @return A data.table of tags for the query.
        tag_query = function(query_id, tag, replace = FALSE) {
            private$check_open()
            checkmate::assert_string(query_id, min.chars = 1L)
            checkmate::assert_character(tag, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_flag(replace)
            private$with_store_lock({
                private$get_query_row(query_id)
                if (isTRUE(replace)) {
                    private$delete_by_key("esg_query_tag", "query_id", query_id)
                }
                now <- store__now()
                rows <- data.frame(
                    tag_id = vapply(tag, function(value) store__hash(query_id, value), character(1L)),
                    query_id = query_id,
                    tag = tag,
                    created_at = now,
                    stringsAsFactors = FALSE
                )
                private$append_new_rows("esg_query_tag", rows, "tag_id")
                self$query_tags(query_id)
            })
        },
        # }}}

        # untag_query {{{
        #' @description
        #' Remove tags from a stored ESGF query.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #' @param tag Optional tags. If `NULL`, all tags are removed.
        #'
        #' @return A data.table of remaining tags for the query.
        untag_query = function(query_id, tag = NULL) {
            private$check_open()
            checkmate::assert_string(query_id, min.chars = 1L)
            checkmate::assert_character(tag, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            private$with_store_lock({
                private$get_query_row(query_id)
                wanted_tag <- tag
                wanted_query_id <- query_id
                tags <- private$read_table("esg_query_tag")
                remove <- tags[tags[["query_id"]] == wanted_query_id]
                if (!is.null(wanted_tag) && nrow(remove)) {
                    remove <- remove[remove[["tag"]] %in% wanted_tag]
                }
                if (nrow(remove)) {
                    private$delete_by_key("esg_query_tag", "tag_id", remove$tag_id)
                }
                self$query_tags(query_id)
            })
        },
        # }}}

        # query_tags {{{
        #' @description
        #' List stored ESGF query tags.
        #'
        #' @param query_id Optional query ID filter.
        #'
        #' @return A data.table of query tags.
        query_tags = function(query_id = NULL) {
            private$check_open()
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            tags <- private$read_table("esg_query_tag")
            if (!is.null(query_id) && nrow(tags)) {
                wanted_query_id <- query_id
                tags <- tags[tags[["query_id"]] %in% wanted_query_id]
            }
            tags[]
        },
        # }}}

        # require_query {{{
        #' @description
        #' Record that one stored query depends on another stored query.
        #'
        #' @param query_id Child query ID.
        #' @param parent_query_id Required parent query ID.
        #'
        #' @return A data.table of query dependency edges.
        require_query = function(query_id, parent_query_id) {
            private$check_open()
            checkmate::assert_string(query_id, min.chars = 1L)
            checkmate::assert_string(parent_query_id, min.chars = 1L)
            if (identical(query_id, parent_query_id)) {
                cli::cli_abort("A query cannot require itself.")
            }
            private$with_store_lock({
                private$get_query_row(query_id)
                private$get_query_row(parent_query_id)
                now <- store__now()
                row <- data.frame(
                    dependency_id = store__hash(query_id, parent_query_id),
                    query_id = query_id,
                    parent_query_id = parent_query_id,
                    created_at = now,
                    stringsAsFactors = FALSE
                )
                private$append_new_rows("esg_query_dependency", row, "dependency_id")
                self$query_graph(query_id = query_id, direction = "parents", recursive = FALSE)
            })
        },
        # }}}

        # unrequire_query {{{
        #' @description
        #' Remove query dependency edges.
        #'
        #' @param query_id Child query ID.
        #' @param parent_query_id Optional parent query ID. If `NULL`, all
        #'        parents for `query_id` are removed.
        #'
        #' @return A data.table of remaining dependency edges for the query.
        unrequire_query = function(query_id, parent_query_id = NULL) {
            private$check_open()
            checkmate::assert_string(query_id, min.chars = 1L)
            checkmate::assert_character(parent_query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            private$with_store_lock({
                private$get_query_row(query_id)
                wanted_parent_query_id <- parent_query_id
                wanted_query_id <- query_id
                edges <- private$read_table("esg_query_dependency")
                remove <- edges[edges[["query_id"]] == wanted_query_id]
                if (!is.null(wanted_parent_query_id) && nrow(remove)) {
                    remove <- remove[remove[["parent_query_id"]] %in% wanted_parent_query_id]
                }
                if (nrow(remove)) {
                    private$delete_by_key("esg_query_dependency", "dependency_id", remove$dependency_id)
                }
                self$query_graph(query_id = query_id, direction = "parents", recursive = FALSE)
            })
        },
        # }}}

        # query_graph {{{
        #' @description
        #' List stored query dependency edges.
        #'
        #' @param query_id Optional query ID anchor.
        #' @param direction Which edge direction to return for an anchor.
        #' @param recursive Whether to include transitive edges.
        #'
        #' @return A data.table of dependency edges.
        query_graph = function(query_id = NULL, direction = c("children", "parents", "both"), recursive = TRUE) {
            private$check_open()
            checkmate::assert_string(query_id, null.ok = TRUE)
            checkmate::assert_flag(recursive)
            direction <- match.arg(direction)
            edges <- private$read_table("esg_query_dependency")
            if (is.null(query_id) || !nrow(edges)) {
                return(edges[])
            }
            ids <- private$query_related_ids(query_id, direction = direction, recursive = recursive)
            edges[
                edges[["query_id"]] %in% ids |
                    edges[["parent_query_id"]] %in% ids
            ][]
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
            data.table::setcolorder(
                out,
                c("query_id", "file_key", "status", setdiff(names(out), c("query_id", "file_key", "status")))
            )
            out[]
        },
        # }}}

        # preview_update_queries {{{
        #' @description
        #' Preview tracked ESGF query updates without changing the store.
        #'
        #' @param query_id Optional query ID. If `NULL`, tracked queries are
        #'        previewed by default.
        #' @param tracked Tracked-state filter used when `query_id` is `NULL`.
        #' @param tag Optional query tag filter used when `query_id` is `NULL`.
        #' @param children Whether to include dependency children of selected
        #'        queries.
        #' @param detail Whether to return per-file changes together with the
        #'        summary. Default: `FALSE`.
        #' @param all,limit,fields Arguments passed to `EsgQuery$collect()`.
        #' @param ... Additional File query filters passed to `EsgQuery$collect()`.
        #'
        #' @return A data.table summary, or a list with `summary` and `changes`
        #'         when `detail = TRUE`.
        preview_update_queries = function(
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
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_flag(tracked, null.ok = TRUE)
            checkmate::assert_character(tag, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_flag(children)
            checkmate::assert_flag(detail)

            rows <- private$select_query_rows(
                query_id = private$resolve_query_selection(query_id = query_id, tag = tag, children = children),
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
                query <- private$load_query(rows[i])
                files <- query$collect(type = "File", fields = fields, all = all, limit = limit, ...)
                previews[[i]] <- private$preview_query_update(
                    row = rows[i],
                    files = files,
                    fields = fields,
                    all = all,
                    limit = limit
                )
            }

            summary <- data.table::rbindlist(lapply(previews, `[[`, "summary"), fill = TRUE)
            if (!isTRUE(detail)) {
                return(summary[])
            }
            changes <- data.table::rbindlist(lapply(previews, `[[`, "changes"), fill = TRUE)
            list(summary = summary[], changes = changes[])
        },
        # }}}

        # update_queries {{{
        #' @description
        #' Refresh stored ESGF queries and link their current File records.
        #'
        #' @param query_id Optional query ID. If `NULL`, tracked queries are
        #'        updated by default.
        #' @param tracked Tracked-state filter used when `query_id` is `NULL`.
        #' @param tag Optional query tag filter used when `query_id` is `NULL`.
        #' @param children Whether to include dependency children of selected
        #'        queries.
        #' @param enqueue Whether to enqueue current files after updating.
        #'        Default: `FALSE`.
        #' @param downloader Optional [Downloader] used when `enqueue = TRUE`.
        #' @param replica Replica policy passed to `$download_plan()` when
        #'        enqueuing.
        #' @param session_label Optional download session label.
        #' @param service ESGF URL service used for the download plan.
        #' @param probe Whether to probe candidate URLs before ranking.
        #' @param probe_concurrency Maximum concurrent URL probes when
        #'        `probe = TRUE`. Default comes from the downloader worker count
        #'        when `enqueue = TRUE`.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #' @param strategy Candidate ranking strategy.
        #' @param all,limit,fields Arguments passed to `EsgQuery$collect()`.
        #' @param ... Additional File query filters passed to `EsgQuery$collect()`.
        #'
        #' @return A data.table of query-file links touched by the update.
        update_queries = function(
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
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_flag(tracked, null.ok = TRUE)
            checkmate::assert_character(tag, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_flag(children)
            checkmate::assert_flag(enqueue)
            strategy <- match.arg(strategy)
            if (isTRUE(enqueue) && is.null(downloader)) {
                downloader <- self$downloader()
            }
            rows <- private$select_query_rows(
                query_id = private$resolve_query_selection(query_id = query_id, tag = tag, children = children),
                tracked = if (is.null(query_id)) tracked else NULL
            )
            if (!nrow(rows)) {
                return(data.table::data.table())
            }

            updated <- vector("list", nrow(rows))
            for (i in seq_len(nrow(rows))) {
                # Network collection can be slow; keep it outside the store lock.
                query <- private$load_query(rows[i])
                files <- query$collect(type = "File", fields = fields, all = all, limit = limit, ...)

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
                    sid <- private$enqueue_query_download(
                        query_id = rows$query_id[[i]],
                        files = files,
                        current = updated[[i]][updated[[i]][["status"]] %in% "current", , drop = FALSE],
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
                            private$set_query_update_session(updated[[i]]$update_id[[1L]], sid)
                        })
                    }
                }
            }
            data.table::rbindlist(updated, fill = TRUE)
        },
        # }}}

        # download_preflight {{{
        #' @description
        #' Preview a tracked query download without changing the store.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #' @param downloader Optional [Downloader] used only for node
        #'        history, network policy, and cooldown policy.
        #' @param replica Replica policy passed to `$download_plan()`.
        #' @param service,probe,strategy Download plan arguments.
        #' @param probe_concurrency Maximum concurrent URL probes when
        #'        `probe = TRUE`. Default comes from `downloader` when supplied.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #' @param all,limit,fields Arguments passed to `EsgQuery$collect()`.
        #' @param ... Additional File query filters passed to `EsgQuery$collect()`.
        #'
        #' @return A list with `summary`, `changes`, `files`, and `candidates`.
        download_preflight = function(
            query_id,
            downloader = NULL,
            replica = "auto",
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
            checkmate::assert_string(query_id, min.chars = 1L)
            strategy <- match.arg(strategy)

            row <- private$get_query_row(query_id)
            query <- private$load_query(row)
            files <- query$collect(type = "File", fields = fields, all = all, limit = limit, ...)
            preview <- private$preview_query_update(
                row = row,
                files = files,
                fields = fields,
                all = all,
                limit = limit
            )

            node_stats <- if (!is.null(downloader)) {
                tryCatch(downloader$data_nodes(service = service), error = function(e) NULL)
            } else {
                NULL
            }
            network_policy <- if (!is.null(downloader)) {
                tryCatch(downloader$network_policy, error = function(e) NULL)
            } else {
                NULL
            }
            node_policy <- if (!is.null(downloader)) {
                tryCatch(downloader$node_policy, error = function(e) NULL)
            } else {
                NULL
            }
            probe_concurrency <- private$downloader_probe_concurrency(downloader, probe_concurrency)
            candidates <- files$download_plan(
                replica = replica,
                service = service,
                probe = probe,
                strategy = strategy,
                node_stats = node_stats,
                network_policy = network_policy,
                node_policy = node_policy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds
            )
            candidates <- private$decorate_download_plan_with_files(candidates, preview$file_rows)
            summary <- private$download_preflight_summary(row, preview$file_rows, candidates, downloader = downloader)
            list(
                summary = summary[],
                changes = preview$changes[],
                files = private$preflight_files(preview$file_rows),
                candidates = candidates[]
            )
        },
        # }}}

        # download_query {{{
        #' @description
        #' Refresh, enqueue, and optionally run downloads for a stored ESGF query.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #' @param replica Replica policy passed to `$download_plan()`.
        #' @param dry_run Whether to return a download preflight without
        #'        changing the store, enqueueing, or downloading. Default:
        #'        `FALSE`.
        #' @param run Whether to run the queued session immediately. Default:
        #'        `TRUE`.
        #' @param background Whether to run the queued session in the background.
        #'        Default: `FALSE`.
        #' @param mode Background execution mode. `"process"` starts a detached
        #'        `Rscript`; `"daemon"` submits the job to a running downloader
        #'        daemon.
        #' @param session_label Optional download session label.
        #' @param service,probe,strategy Download plan arguments.
        #' @param probe_concurrency Maximum concurrent URL probes when
        #'        `probe = TRUE`. Default comes from the downloader worker count.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #' @param progress,overwrite,resume Run arguments.
        #' @param all,limit,fields Arguments passed to `EsgQuery$collect()`.
        #' @param ... Additional File query filters passed to `EsgQuery$collect()`.
        #'
        #' @return The created downloader session ID, `NA_character_` when there
        #'         is no pending file to download, or a one-row background job
        #'         record when `run = TRUE` and `background = TRUE`.
        download_query = function(
            query_id,
            downloader = NULL,
            replica = "auto",
            dry_run = FALSE,
            run = TRUE,
            background = FALSE,
            mode = c("process", "daemon"),
            session_label = NULL,
            service = "HTTPServer",
            probe = TRUE,
            probe_concurrency = NULL,
            probe_cache_seconds = 3600L,
            strategy = c("fastest", "first", "stable"),
            progress = TRUE,
            overwrite = FALSE,
            resume = TRUE,
            all = TRUE,
            limit = FALSE,
            fields = "*",
            ...
        ) {
            private$check_open()
            checkmate::assert_string(query_id, min.chars = 1L)
            checkmate::assert_flag(dry_run)
            checkmate::assert_flag(run)
            checkmate::assert_flag(background)
            checkmate::assert_flag(progress)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            strategy <- match.arg(strategy)
            mode <- match.arg(mode)
            if (isTRUE(dry_run)) {
                return(self$download_preflight(
                    query_id = query_id,
                    downloader = downloader,
                    replica = replica,
                    service = service,
                    probe = probe,
                    probe_concurrency = probe_concurrency,
                    probe_cache_seconds = probe_cache_seconds,
                    strategy = strategy,
                    all = all,
                    limit = limit,
                    fields = fields,
                    ...
                ))
            }
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }

            row <- private$get_query_row(query_id)
            # Network collection can be slow; keep it outside the store lock.
            query <- private$load_query(row)
            files <- query$collect(type = "File", fields = fields, all = all, limit = limit, ...)

            # Commit the refreshed file links under a short lock.
            links <- private$with_store_lock({
                private$update_query_files(query_id, files, fields = fields, all = all, limit = limit)
            })

            # Build and enqueue the downloader session after releasing the store lock.
            session_id <- private$enqueue_query_download(
                query_id = query_id,
                files = files,
                current = links[links[["status"]] %in% "current", , drop = FALSE],
                downloader = downloader,
                replica = replica,
                session_label = session_label,
                service = service,
                probe = probe,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds,
                strategy = strategy,
                error_if_empty = TRUE
            )
            if (nrow(links)) {
                # Store only the downloader session reference under lock.
                private$with_store_lock({
                    private$set_query_update_session(links$update_id[[1L]], session_id)
                })
            }
            if (isTRUE(run)) {
                if (isTRUE(background)) {
                    return(downloader$start(
                        session_id = session_id,
                        overwrite = overwrite,
                        resume = resume,
                        mode = mode,
                        store_path = private$store_path
                    ))
                }
                downloader$run(session_id = session_id, progress = progress, overwrite = overwrite, resume = resume)
                self$sync_downloads(downloader)
            }
            session_id
        },
        # }}}

        # download_status {{{
        #' @description
        #' Return downloader tasks linked to stored query files.
        #'
        #' @param query_id Optional stored query ID.
        #' @param session_id Optional downloader session ID.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A data.table of downloader task rows.
        download_status = function(query_id = NULL, session_id = NULL, downloader = NULL) {
            private$check_open()
            checkmate::assert_string(query_id, null.ok = TRUE)
            checkmate::assert_string(session_id, null.ok = TRUE)
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }
            tasks <- data.table::as.data.table(downloader$tasks(session_id = session_id))
            if (is.null(query_id)) {
                return(tasks[])
            }
            links <- self$query_files(query_id)
            if (!nrow(tasks) || !nrow(links)) {
                return(data.table::data.table())
            }
            tasks <- tasks[tasks[["file_key"]] %in% links$file_key]
            if (!nrow(tasks)) {
                return(tasks[])
            }
            link_cols <- links[, .(file_key, query_id, query_file_status = status)]
            merge(tasks, link_cols, by = "file_key", all.x = TRUE, sort = FALSE)
        },
        # }}}

        # query_status {{{
        #' @description
        #' Summarise tracked ESGF query file and download status.
        #'
        #' @param query_id Optional stored query ID vector. If `NULL`, all
        #'        stored ESGF queries are summarised.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A data.table with one row per stored query.
        query_status = function(query_id = NULL, downloader = NULL) {
            private$check_open()
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }

            queries <- private$select_query_rows(query_id = query_id)
            if (!is.null(query_id)) {
                missing <- setdiff(query_id, queries$query_id)
                if (length(missing)) {
                    cli::cli_abort("Stored ESGF query ID(s) not found: {.val {missing}}.")
                }
            }
            if (!nrow(queries)) {
                return(data.table::data.table())
            }

            links <- private$read_table("esg_query_file")
            files <- private$read_table("esg_file")
            tasks <- tryCatch(data.table::as.data.table(downloader$tasks()), error = function(e) data.table::data.table())
            private$summarise_query_status(queries, links, files, tasks)
        },
        # }}}

        # query_updates {{{
        #' @description
        #' List tracked query update runs.
        #'
        #' @param query_id Optional stored query ID filter.
        #' @param latest Whether to return only the latest update per query.
        #'
        #' @return A data.table of update run summaries.
        query_updates = function(query_id = NULL, latest = FALSE) {
            private$check_open()
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_flag(latest)
            updates <- private$read_table("esg_query_update")
            if (!is.null(query_id) && nrow(updates)) {
                wanted_query_id <- query_id
                updates <- updates[updates[["query_id"]] %in% wanted_query_id]
            }
            if (isTRUE(latest) && nrow(updates)) {
                data.table::setorderv(updates, c("query_id", "completed_at", "started_at"))
                updates <- updates[, .SD[.N], by = "query_id"]
            }
            updates[]
        },
        # }}}

        # query_changes {{{
        #' @description
        #' List per-file changes recorded by tracked query updates.
        #'
        #' @param update_id Optional update run ID filter.
        #' @param query_id Optional stored query ID filter.
        #' @param change_type Optional change type filter.
        #'
        #' @return A data.table of per-file query update changes.
        query_changes = function(update_id = NULL, query_id = NULL, change_type = NULL) {
            private$check_open()
            checkmate::assert_character(update_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_subset(change_type, c("new", "current", "stale", "changed"), empty.ok = TRUE)
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
                changes <- changes[changes[["change_type"]] %in% wanted_change_type]
            }
            changes[]
        },
        # }}}

        # workflow_status {{{
        #' @description
        #' Summarise query, download, local, and extraction status together.
        #'
        #' @param query_id Optional stored query ID filter.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A data.table with one row per stored query.
        workflow_status = function(query_id = NULL, downloader = NULL) {
            private$check_open()
            status <- self$query_status(query_id = query_id, downloader = downloader)
            if (!nrow(status)) {
                return(status)
            }
            updates <- self$query_updates(query_id = status$query_id, latest = TRUE)
            if (nrow(updates)) {
                update_cols <- c(
                    "query_id", "update_id", "status", "started_at", "completed_at",
                    "new_count", "stale_count", "changed_count", "deprecated_count",
                    "retracted_count", "version_changed_count", "download_session_id"
                )
                updates <- updates[, intersect(update_cols, names(updates)), with = FALSE]
                data.table::setnames(
                    updates,
                    intersect(c("status", "started_at", "completed_at"), names(updates)),
                    paste0("last_update_", intersect(c("status", "started_at", "completed_at"), names(updates)))
                )
                status <- merge(status, updates, by = "query_id", all.x = TRUE, sort = FALSE)
            }
            if ("bytes_total" %in% names(status) && "bytes_done" %in% names(status)) {
                status[, bytes_missing := pmax(
                    0,
                    suppressWarnings(as.numeric(bytes_total)) - suppressWarnings(as.numeric(bytes_done))
                )]
            } else {
                status[, bytes_missing := NA_real_]
            }
            retry_cols <- intersect(c("download_error", "download_cancelled"), names(status))
            if (length(retry_cols)) {
                retry_values <- as.data.frame(lapply(retry_cols, function(col) suppressWarnings(as.numeric(status[[col]]))))
                status[, download_retryable := rowSums(retry_values, na.rm = TRUE)]
            } else {
                status[, download_retryable := 0L]
            }
            incomplete_cols <- intersect(c("download_queued", "download_downloading", "download_error", "download_cancelled"), names(status))
            if (length(incomplete_cols)) {
                incomplete_values <- as.data.frame(lapply(incomplete_cols, function(col) suppressWarnings(as.numeric(status[[col]]))))
                status[, download_incomplete := rowSums(incomplete_values, na.rm = TRUE) > 0L]
            } else {
                status[, download_incomplete := FALSE]
            }
            if ("file_current" %in% names(status) && "local_available" %in% names(status)) {
                status[local_available < file_current, download_incomplete := TRUE]
            }
            if ("download_session_id" %in% names(status)) {
                status[, last_download_session_id := download_session_id]
            } else {
                status[, last_download_session_id := NA_character_]
            }

            tags <- self$query_tags(query_id = status$query_id)
            if (nrow(tags)) {
                tag_summary <- tags[, .(tags = paste(sort(unique(tag)), collapse = ",")), by = query_id]
                status <- merge(status, tag_summary, by = "query_id", all.x = TRUE, sort = FALSE)
            }

            plans <- private$read_table("extraction_plan")
            if (nrow(plans)) {
                plan_counts <- plans[plans[["query_id"]] %in% status$query_id, .N, by = .(query_id, status)]
                plan_wide <- data.table::dcast(plan_counts, query_id ~ status, value.var = "N", fill = 0L)
                data.table::setnames(plan_wide, setdiff(names(plan_wide), "query_id"), paste0("extract_", setdiff(names(plan_wide), "query_id")))
                status <- merge(status, plan_wide, by = "query_id", all.x = TRUE, sort = FALSE)
            }
            status[]
        },
        # }}}

        # workflow_report {{{
        #' @description
        #' Return a compact ESGF query workflow health report.
        #'
        #' @param query_id Optional stored query ID filter.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A list with `summary`, `updates`, `changes`, `downloads`,
        #'         and `nodes`.
        workflow_report = function(query_id = NULL, downloader = NULL) {
            private$check_open()
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }
            summary <- self$workflow_status(query_id = query_id, downloader = downloader)
            if (!nrow(summary)) {
                empty <- data.table::data.table()
                return(list(summary = summary, updates = empty, changes = empty, downloads = empty, nodes = empty))
            }

            updates <- self$query_updates(query_id = summary$query_id)
            latest_updates <- self$query_updates(query_id = summary$query_id, latest = TRUE)
            changes <- if (nrow(latest_updates)) {
                out <- self$query_changes(update_id = latest_updates$update_id)
                if (nrow(out)) {
                    out <- out[out[["change_type"]] != "current"]
                }
                out[]
            } else {
                data.table::data.table()
            }
            downloads <- private$workflow_downloads(summary$query_id, downloader)
            nodes <- tryCatch(data.table::as.data.table(downloader$data_nodes()), error = function(e) data.table::data.table())

            list(
                summary = summary[],
                updates = updates[],
                changes = changes[],
                downloads = downloads[],
                nodes = nodes[]
            )
        },
        # }}}

        # remove_query {{{
        #' @description
        #' Remove stored ESGF queries and optionally delete orphaned local files.
        #'
        #' @param query_id Stored query ID vector.
        #' @param delete Whether to leave local files untouched (`"none"`) or
        #'        delete files orphaned by the removal (`"orphaned"`).
        #'
        #' @return A data.table describing removed queries.
        remove_query = function(query_id, delete = c("none", "orphaned")) {
            private$check_open()
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE)
            delete <- match.arg(delete)

            rows <- private$select_query_rows(query_id = query_id)
            missing <- setdiff(query_id, rows$query_id)
            if (length(missing)) {
                cli::cli_abort("Stored ESGF query ID(s) not found: {.val {missing}}.")
            }

            private$with_store_lock({
            links <- private$read_table("esg_query_file")
            wanted_query_id <- query_id
            touched <- links[links[["query_id"]] %in% wanted_query_id]
            artifacts <- private$read_table("artifact")
            query_artifacts <- artifacts[artifacts[["query_id"]] %in% wanted_query_id & artifacts[["kind"]] == "query"]

            private$delete_by_key("esg_query_file", "query_id", query_id)
            private$delete_by_key("esg_query_tag", "query_id", query_id)
            private$delete_by_key("esg_query_dependency", "query_id", query_id)
            private$delete_by_key("esg_query_dependency", "parent_query_id", query_id)
            private$delete_by_key("esg_query", "query_id", query_id)
            if (nrow(query_artifacts)) {
                paths <- vapply(query_artifacts$relative_path, store_abs_path, character(1L), root = private$store_path)
                unlink(paths[file.exists(paths)], recursive = FALSE, force = TRUE)
                private$delete_by_key("artifact", "artifact_id", query_artifacts$artifact_id)
            }

            remaining_links <- private$read_table("esg_query_file")
            orphan_keys <- setdiff(unique(touched$file_key), remaining_links$file_key)
            pruned <- if (identical(delete, "orphaned") && length(orphan_keys)) {
                private$remove_file_records(orphan_keys, delete_local = TRUE, force = TRUE)
            } else {
                private$orphaned_files()[file_key %in% orphan_keys]
            }

            out <- data.table::as.data.table(rows)
            out[, removed_file_links := vapply(query_id, function(id) sum(touched$query_id == id), integer(1L))]
            out[, orphaned_file_count := nrow(pruned)]
            out[]
            })
        },
        # }}}

        # remove_files {{{
        #' @description
        #' Remove ESGF file records and optionally delete local artifacts.
        #'
        #' @param file_key File key vector.
        #' @param delete_local Whether to delete local NetCDF files. Default:
        #'        `FALSE`.
        #' @param force Whether to remove files still linked to queries.
        #'        Default: `FALSE`.
        #'
        #' @return A data.table describing removed file records.
        remove_files = function(file_key, delete_local = FALSE, force = FALSE) {
            private$check_open()
            checkmate::assert_character(file_key, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_flag(delete_local)
            checkmate::assert_flag(force)
            private$with_store_lock({
            private$remove_file_records(file_key, delete_local = delete_local, force = force)
            })
        },
        # }}}

        # prune_orphans {{{
        #' @description
        #' Report or remove file records no longer linked to any query.
        #'
        #' @param delete_local Whether to delete local NetCDF files and remove
        #'        orphaned registry records. Default: `FALSE`.
        #'
        #' @return A data.table of orphaned file records.
        prune_orphans = function(delete_local = FALSE) {
            private$check_open()
            checkmate::assert_flag(delete_local)
            orphans <- private$orphaned_files()
            if (!isTRUE(delete_local) || !nrow(orphans)) {
                return(orphans[])
            }
            private$with_store_lock({
            private$remove_file_records(orphans$file_key, delete_local = TRUE, force = TRUE)
            })
        },
        # }}}

        # storage_report {{{
        #' @description
        #' Summarise store download storage, registered local assets, temporary
        #' files, and cleanup candidates.
        #'
        #' @param detail Whether to return detailed file tables. Default:
        #'        `FALSE`.
        #'
        #' @return A summary data.table, or a list when `detail = TRUE`.
        storage_report = function(detail = FALSE) {
            private$check_open()
            checkmate::assert_flag(detail)
            report <- private$download_storage_report()
            if (isTRUE(detail)) {
                return(report)
            }
            report$summary
        },
        # }}}

        # validate_files {{{
        #' @description
        #' Validate store-managed NetCDF downloads against the manifest.
        #'
        #' @param query_id Optional stored query IDs to validate. When `NULL`,
        #'        all known downloaded ESGF files are checked.
        #' @param checksum Whether to compute file checksums. Default: `FALSE`.
        #' @param layout Whether to compare registered files with the current
        #'        download layout policy. Default: `TRUE`.
        #'
        #' @return A list with `summary`, `files`, `artifacts`, `untracked`, and
        #'         `actions` data.tables. The method is read-only.
        validate_files = function(query_id = NULL, checksum = FALSE, layout = TRUE) {
            private$check_open()
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_flag(checksum)
            checkmate::assert_flag(layout)

            private$validate_download_files(query_id = query_id, checksum = checksum, layout = layout)
        },
        # }}}

        # repair_files {{{
        #' @description
        #' Repair safe store download inconsistencies reported by
        #' `$validate_files()`.
        #'
        #' @param actions Optional action table from `$validate_files()$actions`.
        #'        When `NULL`, actions are generated from `$validate_files()`.
        #' @param dry_run Whether to only report planned repairs. Default:
        #'        `TRUE`.
        #'
        #' @return A data.table describing attempted repairs.
        repair_files = function(actions = NULL, dry_run = TRUE) {
            private$check_open()
            checkmate::assert_data_frame(actions, null.ok = TRUE)
            checkmate::assert_flag(dry_run)

            if (is.null(actions)) {
                actions <- self$validate_files()$actions
            }
            actions <- data.table::as.data.table(actions)
            if (isTRUE(dry_run)) {
                return(private$repair_download_files(actions, dry_run = TRUE))
            }
            private$with_store_lock({
            private$repair_download_files(actions, dry_run = FALSE)
            })
        },
        # }}}

        # cleanup_downloads {{{
        #' @description
        #' Report or remove download cleanup candidates.
        #'
        #' @param scope Cleanup scopes. Supported values are `"tmp"`,
        #'        `"orphan_records"`, `"untracked_files"`, and
        #'        `"missing_records"`.
        #' @param dry_run Whether to only report cleanup candidates. Default:
        #'        `TRUE`.
        #' @param older_than Optional age filter for file scopes. A numeric value
        #'        is interpreted as seconds before now; a `POSIXct` value is used
        #'        as an absolute mtime cutoff.
        #'
        #' @return A data.table describing cleanup candidates or removals.
        cleanup_downloads = function(
            scope = c("tmp", "orphan_records", "untracked_files", "missing_records"),
            dry_run = TRUE,
            older_than = NULL
        ) {
            private$check_open()
            checkmate::assert_subset(scope, c("tmp", "orphan_records", "untracked_files", "missing_records"), empty.ok = FALSE)
            checkmate::assert_flag(dry_run)
            cutoff <- private$cleanup_cutoff(older_than)
            report <- private$download_storage_report()
            collect_actions <- function() {
                actions <- list()
                if ("tmp" %in% scope) {
                    actions$tmp <- private$cleanup_file_scope("tmp", report$tmp, dry_run = dry_run, cutoff = cutoff)
                }
                if ("untracked_files" %in% scope) {
                    actions$untracked_files <- private$cleanup_file_scope(
                        "untracked_files",
                        report$untracked_files,
                        dry_run = dry_run,
                        cutoff = cutoff
                    )
                }
                if ("orphan_records" %in% scope) {
                    actions$orphan_records <- private$cleanup_orphan_records(report$orphan_records, dry_run = dry_run)
                }
                if ("missing_records" %in% scope) {
                    actions$missing_records <- private$cleanup_missing_records(report$missing_records, dry_run = dry_run)
                }
                data.table::rbindlist(actions, fill = TRUE)
            }
            if (isTRUE(dry_run)) {
                collect_actions()
            } else {
                private$with_store_lock(collect_actions())
            }
        },
        # }}}

        # retry_downloads {{{
        #' @description
        #' Requeue retryable downloader tasks linked to stored query files.
        #'
        #' @param query_id Optional stored query ID.
        #' @param session_id Optional downloader session ID.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #' @param status Retryable statuses. Default: `c("error", "cancelled")`.
        #' @param run Whether to run requeued tasks immediately. Default: `TRUE`.
        #' @param ... Additional arguments passed to `Downloader$run()`.
        #'
        #' @return A data.table of matching task rows after retry handling.
        retry_downloads = function(
            query_id = NULL,
            session_id = NULL,
            downloader = NULL,
            status = c("error", "cancelled"),
            run = TRUE,
            ...
        ) {
            private$check_open()
            checkmate::assert_string(query_id, null.ok = TRUE)
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_subset(status, c("error", "cancelled"), empty.ok = FALSE)
            checkmate::assert_flag(run)
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }

            tasks <- data.table::as.data.table(downloader$tasks(session_id = session_id, status = status))
            if (!is.null(query_id) && nrow(tasks)) {
                links <- self$query_files(query_id)
                tasks <- tasks[tasks[["file_key"]] %in% links$file_key]
            }
            if (!nrow(tasks)) {
                return(tasks[])
            }
            task_id <- tasks$task_id
            downloader$retry(session_id = session_id, task_id = task_id, status = status)
            out <- if (isTRUE(run)) {
                downloader$run(session_id = session_id, task_id = task_id, ...)
            } else {
                downloader$status(session_id = session_id, task_id = task_id)
            }
            out <- data.table::as.data.table(out)
            if (isTRUE(run)) {
                self$sync_downloads(downloader)
            }
            out[]
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

            private$with_store_lock({
            result_type <- store__result_type(files)
            dt <- store__file_table(files)
            query_id <- store__hash(
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
                label = store__chr1(label),
                result_type = result_type,
                query_file = store_rel_path(query_file, private$store_path),
                index_node = priv(files)$index_node,
                time_filter_start = store__time1(store__pluck(time_filter, "start")),
                time_filter_stop = store__time1(store__pluck(time_filter, "stop")),
                time_filter_method = store__chr1(store__pluck(time_filter, "method")),
                created_at = store__now(),
                package_version = as.character(utils::packageVersion("epwshiftr")),
                stringsAsFactors = FALSE
            )
            private$replace_rows("query_run", query_run, "query_id")

            if (nrow(dt)) {
                now <- store__now()
                file_rows <- private$file_rows(dt, now)
                private$replace_rows("esg_file", as.data.frame(file_rows), "file_key")
                private$sync_query_file_links(query_id, file_rows, now)
                private$sync_file_catalog(query_id, file_rows)
            }

            query_id
            })
        },
        # }}}

        # downloader {{{
        #' @description
        #' Return a Downloader bound to this store.
        #'
        #' @param ... Additional arguments passed to `Downloader$new()`.
        #'
        #' @return A [Downloader] object.
        downloader = function(...) {
            private$check_open()
            Downloader$new(
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
        #' @param query_id Optional file collection query IDs to enqueue when
        #'        `files` is `NULL`. If `NULL`, all cataloged files missing local
        #'        paths are considered.
        #' @param replica Replica policy passed to `$download_plan()`.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #' @param run Whether to run the queued session immediately. Default: `TRUE`.
        #' @param background Whether to run the queued session in the background.
        #'        Default: `FALSE`.
        #' @param mode Background execution mode. `"process"` starts a detached
        #'        `Rscript`; `"daemon"` submits the job to a running downloader
        #'        daemon.
        #' @param session_label Optional download session label.
        #' @param service ESGF URL service to download from. Default:
        #'        `"HTTPServer"`.
        #' @param probe Whether to lightly probe URLs before ranking them.
        #' @param strategy Candidate ranking strategy.
        #' @param probe_concurrency Maximum concurrent URL probes when
        #'        `probe = TRUE`. Default comes from the downloader worker count.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #' @param progress Whether to show per-file download progress.
        #' @param overwrite Whether to overwrite existing final files.
        #' @param resume Whether to resume interrupted `.part` files.
        #' @param ... Additional arguments passed to `$download_plan()` and
        #'        `Downloader$run()`.
        #'
        #' @return The created downloader session ID, or a one-row background
        #'         job record when `run = TRUE` and `background = TRUE`.
        download_files = function(
            files = NULL,
            query_id = NULL,
            replica = "auto",
            downloader = NULL,
            run = TRUE,
            background = FALSE,
            mode = c("process", "daemon"),
            session_label = NULL,
            service = "HTTPServer",
            probe = TRUE,
            probe_concurrency = NULL,
            probe_cache_seconds = 3600L,
            strategy = c("fastest", "first", "stable"),
            progress = TRUE,
            overwrite = FALSE,
            resume = TRUE,
            ...
        ) {
            private$check_open()
            checkmate::assert_character(query_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            strategy <- match.arg(strategy)
            mode <- match.arg(mode)
            checkmate::assert_flag(run)
            checkmate::assert_flag(background)
            checkmate::assert_flag(progress)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }

            plan <- if (!is.null(files)) {
                # Cataloging files mutates the store; plan construction itself stays unlocked.
                query_id <- self$add_files(files, label = session_label)
                current <- private$with_store_lock({
                    # add_files() records file collections in file_catalog, not esg_query_file.
                    catalog <- data.table::as.data.table(ddb_read_table(private$conn, "file_catalog"))
                    catalog[catalog[["query_id"]] == query_id]
                })
                node_stats <- tryCatch(downloader$data_nodes(service = service), error = function(e) NULL)
                network_policy <- tryCatch(downloader$network_policy, error = function(e) NULL)
                node_policy <- tryCatch(downloader$node_policy, error = function(e) NULL)
                resolved_probe_concurrency <- private$downloader_probe_concurrency(downloader, probe_concurrency)
                plan_args <- list(
                    replica = replica,
                    service = service,
                    probe = probe,
                    strategy = strategy,
                    node_stats = node_stats,
                    network_policy = network_policy,
                    node_policy = node_policy,
                    probe_concurrency = resolved_probe_concurrency,
                    probe_cache_seconds = probe_cache_seconds,
                    ...
                )
                plan <- do.call(files$download_plan, plan_args)
                plan <- private$decorate_download_plan_with_files(plan, current)
                plan <- plan[plan[["file_key"]] %in% current$file_key]
                plan
            } else {
                # Catalog reads need a consistent snapshot, but downloader work does not.
                private$with_store_lock({
                    private$catalog_download_plan(query_id = query_id)
                })
            }
            if (!nrow(plan)) {
                return(NA_character_)
            }
            # Downloader manifest/probe operations must not hold the store lock.
            tryCatch(downloader$record_probes(plan, probed = probe), error = function(e) NULL)
            session_id <- downloader$enqueue(plan, session_label = session_label)
            if (isTRUE(run)) {
                if (isTRUE(background)) {
                    return(downloader$start(
                        session_id = session_id,
                        overwrite = overwrite,
                        resume = resume,
                        mode = mode,
                        store_path = private$store_path
                    ))
                }
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
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A data.table of completed tasks.
        sync_downloads = function(downloader = NULL) {
            private$check_open()
            if (is.null(downloader)) {
                downloader <- self$downloader()
            }
            private$with_store_lock({
            tasks <- data.table::as.data.table(downloader$tasks(status = c("done", "skipped")))
            if (!nrow(tasks)) {
                return(tasks)
            }
            catalog <- data.table::as.data.table(ddb_read_table(private$conn, "file_catalog"))
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
                checksum <- store__chr1(task$checksum[[1L]])
                checksum_type <- tolower(store__chr1(task$checksum_type[[1L]]))
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
                    source_url = store__chr1(task$selected_url[[1L]]),
                    metadata = list(filename = store__chr1(task$filename[[1L]]))
                )
                row$local_path <- store_rel_path(local_path, private$store_path)
                row$local_artifact_id <- artifact_id
                private$replace_rows("file_catalog", as.data.frame(row), "file_key")
                private$sync_tracked_file_download(file_key, local_path, artifact_id)
            }
            tasks
            })
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
        #' @param method Grid extraction method. One of `"nearest"`, `"idw"`,
        #'        `"bilinear"`, or `"mean"`. Default: `"nearest"`.
        #'
        #' @return A data.table of extraction plan rows.
        plan_region = function(
            query_id,
            lon,
            lat,
            time,
            site_id = "site-1",
            variable_id = NULL,
            filters = list(),
            method = "nearest"
        ) {
            checkmate::assert_string(query_id)
            checkmate::assert_number(lon, lower = -180, upper = 360, finite = TRUE)
            checkmate::assert_number(lat, lower = -90, upper = 90, finite = TRUE)
            checkmate::assert_string(site_id)
            checkmate::assert_character(variable_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_list(filters, names = "unique")
            method <- match.arg(method, ESG_GRID_METHOD_CHOICES)
            private$check_open()

            time_range <- store__time_range(time)
            catalog <- data.table::as.data.table(ddb_read_table(private$conn, "file_catalog"))
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
                    checkmate::assert_atomic_vector(
                        value,
                        any.missing = FALSE,
                        min.len = 1L,
                        .var.name = sprintf("filters$%s", name)
                    )
                    catalog <- catalog[as.character(get(name)) %in% as.character(value)]
                }
            }

            if (!nrow(catalog)) {
                cli::cli_abort("No cataloged file records match the requested extraction plan filters.")
            }

            requested_variable_id <- variable_id
            if (!is.null(requested_variable_id)) {
                if (any(!is.na(catalog$variable_id))) {
                    catalog <- catalog[catalog$variable_id %in% requested_variable_id]
                }
            } else {
                requested_variable_id <- unique(catalog$variable_id)
                requested_variable_id <- requested_variable_id[
                    !is.na(requested_variable_id) & nzchar(requested_variable_id)
                ]
                if (!length(requested_variable_id)) {
                    cli::cli_abort(
                        "Cannot plan extraction without `variable_id` because the file catalog does not contain variable IDs."
                    )
                }
            }

            plan <- data.table::as.data.table(catalog)
            if (!any(!is.na(plan$variable_id)) && !is.null(requested_variable_id)) {
                plan <- plan[rep(seq_len(nrow(plan)), each = length(requested_variable_id))]
                plan$variable_id <- rep(requested_variable_id, times = nrow(catalog))
            } else {
                plan <- plan[plan$variable_id %in% requested_variable_id]
            }
            if (!nrow(plan)) {
                cli::cli_abort("No cataloged file records match the requested variable IDs.")
            }

            now <- store__now()
            out <- data.frame(
                plan_id = vapply(
                    seq_len(nrow(plan)),
                    function(i) {
                        store__hash(
                            plan$file_key[[i]],
                            site_id,
                            plan$variable_id[[i]],
                            lon,
                            lat,
                            method,
                            time_range$start,
                            time_range$stop
                        )
                    },
                    character(1L)
                ),
                query_id = query_id,
                file_key = plan$file_key,
                site_id = site_id,
                variable_id = plan$variable_id,
                lon = lon,
                lat = lat,
                method = method,
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
            append_out <- out
            existing_plan_cols <- names(private$read_table("extraction_plan"))
            if ("nearest" %in% existing_plan_cols && !"nearest" %in% names(append_out)) {
                append_out$nearest <- NA_integer_
            }
            append_out <- append_out[, intersect(existing_plan_cols, names(append_out)), drop = FALSE]
            private$append_new_rows("extraction_plan", append_out, "plan_id")

            existing <- data.table::as.data.table(ddb_read_table(private$conn, "extraction_plan"))
            plan_cols <- c(
                "plan_id", "query_id", "file_key", "site_id", "variable_id",
                "lon", "lat", "method", "time_start", "time_stop", "status",
                "available_time_count", "attempt_count", "last_error",
                "created_at", "updated_at"
            )
            existing[plan_id %in% out$plan_id, intersect(plan_cols, names(existing)), with = FALSE]
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
        #' @param resume Whether to reuse complete existing extraction outputs.
        #'        Default: `TRUE`.
        #'
        #' @return A data.table of processed extraction plan rows.
        extract = function(
            plan_id = NULL,
            status = c("pending", "failed"),
            fallback = c("auto", "error"),
            overwrite = FALSE,
            resume = TRUE
        ) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_subset(status, c("pending", "failed", "empty", "done"))
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            fallback <- match.arg(fallback)
            private$check_open()

            plans <- data.table::as.data.table(ddb_read_table(private$conn, "extraction_plan"))
            if (is.null(plan_id)) {
                plans <- plans[plans$status %in% status]
            } else {
                plans <- plans[plans$plan_id %in% plan_id]
            }
            if (!nrow(plans)) {
                return(plans)
            }

            catalog <- data.table::as.data.table(ddb_read_table(private$conn, "file_catalog"))
            processed <- vector("list", nrow(plans))
            for (i in seq_len(nrow(plans))) {
                plan <- plans[i]
                resumed <- if (!isTRUE(overwrite) && isTRUE(resume)) private$resume_extract_plan(plan) else NULL
                if (!is.null(resumed)) {
                    processed[[i]] <- resumed
                    next
                }
                file <- catalog[catalog$file_key == plan$file_key[[1L]]]
                if (!nrow(file)) {
                    processed[[i]] <- private$mark_plan_failed(plan, "The cataloged file record no longer exists.")
                    next
                }

                processed[[i]] <- tryCatch(
                    private$extract_one(plan, file[1L], fallback = fallback, overwrite = overwrite),
                    error = function(e) {
                        if (inherits(e, "epwshiftr_store_extract_conflict")) {
                            stop(e)
                        }
                        private$mark_plan_failed(plan, conditionMessage(e))
                    }
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

            data.table::as.data.table(ddb_query(private$conn, sql))
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
        summarise = function(
            by = c("source_id", "experiment_id", "variant_label", "frequency", "variable_id", "site_id", "year")
        ) {
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            private$check_open()

            map <- store__summary_cols()
            unknown <- setdiff(by, names(map))
            if (length(unknown)) {
                cli::cli_abort("Unknown extraction summary column(s): {.val {unknown}}.")
            }

            groups <- unname(map[by])
            select_groups <- paste(sprintf("%s AS %s", groups, ddb_ident(private$conn, by)), collapse = ", ")
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

            data.table::as.data.table(ddb_query(private$conn, sql))
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

            plans <- data.table::as.data.table(ddb_read_table(private$conn, "extraction_plan"))
            if (!is.null(plan_id)) {
                plans <- plans[plans$plan_id %in% plan_id]
            }
            if (!nrow(plans)) {
                return(plans)
            }

            catalog <- data.table::as.data.table(ddb_read_table(private$conn, "file_catalog"))
            results <- data.table::as.data.table(ddb_read_table(private$conn, "extraction_result"))
            agg <- if (nrow(results)) {
                results[,
                    .(
                        output_files = list(output_path),
                        output_file_count = .N,
                        output_rows = sum(row_count, na.rm = TRUE),
                        output_time_count = sum(unique_time_count, na.rm = TRUE),
                        output_time_min = min(time_min, na.rm = TRUE),
                        output_time_max = max(time_max, na.rm = TRUE)
                    ),
                    by = plan_id
                ]
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
            out[
                is.na(output_file_count),
                `:=`(
                    output_file_count = 0L,
                    output_rows = 0L,
                    output_time_count = 0L
                )
            ]
            if (!"output_files" %in% names(out)) {
                out[, output_files := list(character())]
            }
            out[,
                output_files_exist := vapply(
                    output_files,
                    function(paths) {
                        paths <- unlist(paths, use.names = FALSE)
                        length(paths) > 0L && all(file.exists(file.path(private$store_path, paths)))
                    },
                    logical(1L)
                )
            ]
            out[,
                complete := status == "done" &
                    output_files_exist &
                    (is.na(available_time_count) |
                        available_time_count <= 0L |
                        output_time_count >= available_time_count)
            ]

            data.table::setcolorder(
                out,
                c(
                    "plan_id",
                    "complete",
                    "status",
                    "query_id",
                    "file_key",
                    "site_id",
                    "source_id",
                    "experiment_id",
                    "variant_label",
                    "frequency",
                    "variable_id",
                    intersect(
                        c(
                            "available_time_count",
                            "output_time_count",
                            "output_rows",
                            "output_file_count",
                            "output_files_exist",
                            "last_error"
                        ),
                        names(out)
                    ),
                    setdiff(
                        names(out),
                        c(
                            "plan_id",
                            "complete",
                            "status",
                            "query_id",
                            "file_key",
                            "site_id",
                            "source_id",
                            "experiment_id",
                            "variant_label",
                            "frequency",
                            "variable_id",
                            "available_time_count",
                            "output_time_count",
                            "output_rows",
                            "output_file_count",
                            "output_files_exist",
                            "last_error"
                        )
                    )
                )
            )
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
            !is.null(private$conn) && ddb_is_valid(private$conn)
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
        lock_depth = 0L,

        # download layout helpers {{{
        normalize_download_layout_policy = function(policy = NULL) {
            if (is.null(policy)) {
                policy <- list()
            }
            checkmate::assert_list(policy, names = "unique")
            defaults <- STORE_DOWNLOAD_LAYOUT_DEFAULT
            for (name in names(defaults)) {
                if (!name %in% names(policy) || is.null(policy[[name]])) {
                    policy[[name]] <- defaults[[name]]
                }
            }
            checkmate::assert_choice(policy$layout, STORE_DOWNLOAD_LAYOUT_CHOICES)
            policy$template <- downloader__null_if_empty(policy$template)
            if (identical(policy$layout, "template") && is.null(policy$template)) {
                cli::cli_abort("{.arg template} is required when {.code layout = \"template\"}.")
            }
            checkmate::assert_flag(policy$include_version)
            checkmate::assert_choice(policy$collision, STORE_DOWNLOAD_COLLISION_CHOICES)
            checkmate::assert_choice(policy$missing, STORE_DOWNLOAD_MISSING_CHOICES)
            list(
                layout = policy$layout,
                template = policy$template,
                include_version = isTRUE(policy$include_version),
                collision = policy$collision,
                missing = policy$missing
            )
        },

        download_layout_policy = function() {
            meta <- private$read_table("store_meta")
            row <- meta[meta[["key"]] == "download_layout"]
            if (!nrow(row)) {
                return(private$normalize_download_layout_policy())
            }
            policy <- tryCatch(
                jsonlite::fromJSON(row$value[[1L]], simplifyVector = TRUE, simplifyMatrix = FALSE),
                error = function(e) list()
            )
            private$normalize_download_layout_policy(policy)
        },

        apply_download_layout = function(plan, file_rows = NULL) {
            plan <- data.table::copy(data.table::as.data.table(plan))
            plan <- data.table::setalloccol(plan)
            if (!nrow(plan)) {
                return(plan)
            }
            plan <- private$layout_download_plan_candidates(plan, file_rows)
            policy <- private$download_layout_policy()
            private$resolve_download_plan_collisions(plan, policy)
        },

        layout_download_plan_candidates = function(plan, file_rows = NULL) {
            plan <- data.table::copy(data.table::as.data.table(plan))
            plan <- data.table::setalloccol(plan)
            if (!nrow(plan)) {
                return(plan)
            }
            policy <- private$download_layout_policy()
            plan <- private$enrich_download_plan_layout_fields(plan, file_rows)
            plan[["layout_missing_fields"]] <- rep("", nrow(plan))
            if (identical(policy$layout, "flat")) {
                plan[["subdir"]] <- rep(NA_character_, nrow(plan))
            } else {
                subdirs <- vapply(seq_len(nrow(plan)), function(i) {
                    private$download_layout_subdir(plan[i], policy)
                }, character(1L))
                missing <- vapply(seq_len(nrow(plan)), function(i) {
                    private$download_layout_missing_fields(plan[i], policy)
                }, character(1L))
                plan[["subdir"]] <- subdirs
                plan[["layout_missing_fields"]] <- missing
            }
            plan <- private$decorate_download_plan_targets(plan)
            plan[]
        },

        enrich_download_plan_layout_fields = function(plan, file_rows = NULL) {
            fields <- c(
                "activity_id", "institution_id", "source_id", "experiment_id",
                "variant_label", "frequency", "table_id", "variable_id",
                "grid_label", "version", "dataset_id", "checksum", "filename"
            )
            for (field in fields) {
                if (!field %in% names(plan)) {
                    plan[[field]] <- NA_character_
                }
            }
            if (is.null(file_rows) || !nrow(file_rows)) {
                return(plan)
            }
            file_rows <- data.table::as.data.table(file_rows)
            if (!"file_key" %in% names(plan) || !"file_key" %in% names(file_rows)) {
                return(plan)
            }
            idx <- match(plan$file_key, file_rows$file_key)
            for (field in intersect(fields, names(file_rows))) {
                missing <- is.na(plan[[field]]) | !nzchar(as.character(plan[[field]]))
                fill <- !is.na(idx) & missing
                if (any(fill)) {
                    plan[[field]][fill] <- as.character(file_rows[[field]][idx[fill]])
                }
            }
            plan[]
        },

        download_layout_subdir = function(row, policy) {
            if (identical(policy$layout, "dataset")) {
                return(private$download_layout_dataset_subdir(row))
            }
            if (identical(policy$layout, "drs")) {
                missing <- private$download_layout_missing_fields(row, policy)
                if (nzchar(missing)) {
                    if (identical(policy$missing, "error")) {
                        cli::cli_abort("Cannot build DRS download path; missing field(s): {.field {strsplit(missing, ',')[[1L]]}}.")
                    }
                    return(private$download_layout_dataset_subdir(row))
                }
                parts <- c(
                    "CMIP6",
                    private$download_layout_component(row$activity_id),
                    private$download_layout_component(row$institution_id),
                    private$download_layout_component(row$source_id),
                    private$download_layout_component(row$experiment_id),
                    private$download_layout_component(row$variant_label),
                    private$download_layout_component(row$table_id),
                    private$download_layout_component(row$variable_id),
                    private$download_layout_component(row$grid_label)
                )
                if (isTRUE(policy$include_version)) {
                    parts <- c(parts, private$download_layout_version_component(row$version))
                }
                return(private$download_layout_path(parts))
            }
            if (identical(policy$layout, "template")) {
                return(private$download_layout_template_subdir(row, policy))
            }
            NA_character_
        },

        download_layout_dataset_subdir = function(row) {
            dataset_id <- private$download_layout_component(row$dataset_id)
            if (!is.na(dataset_id)) {
                return(file.path("datasets", dataset_id))
            }
            pieces <- c(
                private$download_layout_component(row$source_id),
                private$download_layout_component(row$experiment_id),
                private$download_layout_component(row$variant_label),
                private$download_layout_component(row$table_id),
                private$download_layout_component(row$variable_id),
                private$download_layout_component(row$grid_label)
            )
            pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
            if (!length(pieces)) {
                pieces <- paste0("file-", substr(store__hash(row$logical_file_id, row$file_key, row$filename), 1L, 12L))
            }
            private$download_layout_path(c("datasets", pieces))
        },

        download_layout_template_subdir = function(row, policy) {
            out <- policy$template
            fields <- unique(unlist(regmatches(out, gregexpr("\\{[^{}]+\\}", out))))
            if (length(fields)) {
                for (token in fields) {
                    field <- sub("^\\{", "", sub("\\}$", "", token))
                    value <- if (field %in% names(row)) private$download_layout_component(row[[field]]) else NA_character_
                    if (is.na(value)) {
                        if (identical(policy$missing, "error")) {
                            cli::cli_abort("Cannot build template download path; missing field {.field {field}}.")
                        }
                        value <- "unknown"
                    }
                    out <- gsub(token, value, out, fixed = TRUE)
                }
            }
            private$download_layout_clean_subdir(out)
        },

        download_layout_missing_fields = function(row, policy) {
            if (!identical(policy$layout, "drs")) {
                return("")
            }
            required <- c(
                "activity_id", "institution_id", "source_id", "experiment_id",
                "variant_label", "table_id", "variable_id", "grid_label"
            )
            if (isTRUE(policy$include_version)) {
                required <- c(required, "version")
            }
            missing <- required[vapply(required, function(field) {
                value <- if (field %in% names(row)) row[[field]] else NA_character_
                is.na(private$download_layout_component(value))
            }, logical(1L))]
            paste(missing, collapse = ",")
        },

        decorate_download_plan_targets = function(plan) {
            previous_collision <- if ("target_path_collision" %in% names(plan)) {
                plan$target_path_collision %in% TRUE
            } else {
                rep(FALSE, nrow(plan))
            }
            previous_collision_group <- if ("target_path_collision_group" %in% names(plan)) {
                plan$target_path_collision_group
            } else {
                rep(NA_character_, nrow(plan))
            }
            target_rel <- mapply(function(subdir, filename) {
                filename <- private$download_layout_component(filename)
                if (is.na(filename)) {
                    filename <- "download.nc"
                }
                if (is.na(subdir) || !nzchar(subdir)) {
                    filename
                } else {
                    file.path(subdir, filename)
                }
            }, plan$subdir, plan$filename, USE.NAMES = FALSE)
            plan[["target_rel_path"]] <- target_rel
            plan[["target_path"]] <- file.path(private$download_dir, target_rel)
            plan[["target_path_collision"]] <- previous_collision
            plan[["target_path_collision_group"]] <- previous_collision_group
            plan[]
        },

        resolve_download_plan_collisions = function(plan, policy) {
            collision <- plan[, .(logical_count = data.table::uniqueN(logical_file_id)), by = "target_rel_path"]
            collision <- collision[logical_count > 1L]
            if (!nrow(collision)) {
                return(plan[])
            }
            plan[target_rel_path %in% collision$target_rel_path, target_path_collision := TRUE]
            plan[target_path_collision %in% TRUE, target_path_collision_group := target_rel_path]
            if (identical(policy$collision, "error")) {
                cli::cli_abort(
                    "Download layout maps multiple logical files to the same target path: {.path {collision$target_rel_path}}."
                )
            }
            colliding <- plan[target_path_collision %in% TRUE]
            disambiguators <- colliding[, .SD[1L], by = "logical_file_id"]
            disambiguators[, disambiguator := vapply(seq_len(.N), function(i) {
                if (identical(policy$collision, "checksum")) {
                    checksum <- private$download_layout_component(checksum[[i]])
                    if (!is.na(checksum)) {
                        return(paste0("checksum=", substr(checksum, 1L, 12L)))
                    }
                }
                paste0("file=", substr(store__hash(logical_file_id[[i]], file_key[[i]], filename[[i]]), 1L, 12L))
            }, character(1L))]
            map <- stats::setNames(disambiguators$disambiguator, disambiguators$logical_file_id)
            hit <- plan$logical_file_id %in% names(map)
            plan[hit, subdir := mapply(function(subdir, logical_file_id) {
                extra <- map[[logical_file_id]]
                if (is.na(subdir) || !nzchar(subdir)) extra else file.path(subdir, extra)
            }, subdir, logical_file_id, USE.NAMES = FALSE)]
            private$decorate_download_plan_targets(plan)
        },

        download_layout_component = function(x) {
            x <- store__chr1(x)
            if (is.na(x) || !nzchar(x)) {
                return(NA_character_)
            }
            x <- sub("\\|.*$", "", x)
            x <- gsub("[/\\\\]+", "_", x)
            x <- gsub("[[:cntrl:]]+", "_", x)
            x <- gsub("^\\.+$", "_", x)
            if (!nzchar(x)) NA_character_ else x
        },

        download_layout_version_component = function(x) {
            x <- private$download_layout_component(x)
            if (is.na(x)) {
                return(NA_character_)
            }
            if (startsWith(x, "v")) x else paste0("v", x)
        },

        download_layout_clean_subdir = function(x) {
            x <- gsub("[/\\\\]+", "/", x)
            parts <- strsplit(x, "/", fixed = TRUE)[[1L]]
            parts <- vapply(parts, private$download_layout_component, character(1L))
            parts <- parts[!is.na(parts) & nzchar(parts)]
            private$download_layout_path(parts)
        },

        download_layout_path = function(parts) {
            parts <- parts[!is.na(parts) & nzchar(parts)]
            if (!length(parts)) {
                return(NA_character_)
            }
            do.call(file.path, as.list(parts))
        },
        # }}}

        # download plan helpers {{{
        catalog_download_plan = function(query_id = NULL) {
            catalog <- data.table::as.data.table(ddb_read_table(private$conn, "file_catalog"))
            if (!nrow(catalog)) {
                return(data.table::data.table())
            }
            if (!is.null(query_id)) {
                wanted_query_id <- query_id
                catalog <- catalog[catalog[["query_id"]] %in% wanted_query_id]
                if (!nrow(catalog)) {
                    return(data.table::data.table())
                }
            }
            catalog <- catalog[is.na(local_path) | !nzchar(local_path)]
            if (!nrow(catalog)) {
                return(data.table::data.table())
            }
            plan <- data.table::data.table(
                logical_file_id = vapply(
                    seq_len(nrow(catalog)),
                    function(i) {
                        pieces <- c(
                            store__chr1(catalog$tracking_id[[i]]),
                            store__chr1(catalog$checksum[[i]]),
                            store__chr1(catalog$filename[[i]]),
                            store__chr1(catalog$esgf_id[[i]])
                        )
                        paste(pieces[!is.na(pieces) & nzchar(pieces)], collapse = ":")
                    },
                    character(1L)
                ),
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
            plan <- plan[!is.na(url) & nzchar(url)]
            if (!nrow(plan)) {
                return(plan)
            }
            extra <- intersect(
                c(
                    "latest", "replica", "retracted", "deprecated",
                    "version", "url_download", "local_path", "local_artifact_id"
                ),
                names(catalog)
            )
            if (length(extra)) {
                idx <- match(plan$file_key, catalog$file_key)
                for (name in extra) {
                    plan[[name]] <- catalog[[name]][idx]
                }
            }
            plan <- private$layout_download_plan_candidates(plan, catalog)
            plan <- private$select_catalog_download_candidates(plan)
            private$apply_download_layout(plan, catalog)
        },

        rank_catalog_download_candidates = function(plan) {
            plan <- data.table::copy(data.table::as.data.table(plan))
            plan <- data.table::setalloccol(plan)
            if (!nrow(plan)) {
                plan[["candidate_rank"]] <- integer()
                return(plan)
            }
            for (name in c("latest", "replica", "retracted", "deprecated")) {
                if (!name %in% names(plan)) {
                    data.table::set(plan, j = name, value = NA)
                }
            }
            data.table::set(plan, j = "catalog_row_order", value = seq_len(nrow(plan)))
            data.table::set(plan, j = "retracted_order", value = data.table::fifelse(store__lgl(plan$retracted) %in% TRUE, 1L, 0L))
            data.table::set(plan, j = "deprecated_order", value = data.table::fifelse(store__lgl(plan$deprecated) %in% TRUE, 1L, 0L))
            latest <- store__lgl(plan$latest)
            data.table::set(plan, j = "latest_order", value = data.table::fifelse(
                latest %in% TRUE,
                0L,
                data.table::fifelse(is.na(latest), 1L, 2L)
            ))
            data.table::set(plan, j = "version_rank", value = store__version_rank(plan$version))
            data.table::set(plan, j = "version_missing", value = is.na(plan$version_rank))
            data.table::set(plan, j = "replica_order", value = data.table::fifelse(store__lgl(plan$replica) %in% TRUE, 1L, 0L))
            data.table::set(plan, j = "https_order", value = data.table::fifelse(grepl("^https://", plan$url), 0L, 1L))
            data.table::setorderv(
                plan,
                c(
                    "target_rel_path",
                    "retracted_order",
                    "deprecated_order",
                    "latest_order",
                    "version_missing",
                    "version_rank",
                    "replica_order",
                    "https_order",
                    "data_node",
                    "url",
                    "catalog_row_order"
                ),
                c(1L, 1L, 1L, 1L, 1L, -1L, 1L, 1L, 1L, 1L, 1L)
            )
            plan[, candidate_rank := seq_len(.N), by = "target_rel_path"]
            plan[]
        },

        select_catalog_download_candidates = function(plan) {
            plan <- private$rank_catalog_download_candidates(plan)
            if (!nrow(plan)) {
                return(plan)
            }
            ambiguous <- private$ambiguous_catalog_download_candidates(plan)
            if (nrow(ambiguous)) {
                cli::cli_abort(c(
                    "Cataloged file candidates map to the same download target with different checksums.",
                    "x" = "Cannot safely choose one candidate for {.path {ambiguous$target_rel_path[[1L]]}}.",
                    "i" = "Use more specific file filters or a download layout that separates these files."
                ))
            }
            plan[, candidate_count := .N, by = "target_rel_path"]
            plan[, candidate_selection := data.table::fifelse(candidate_count > 1L, "ranked_by_catalog_metadata", "single")]
            selected <- plan[candidate_rank == 1L]
            data.table::set(selected, j = "selected_candidate", value = TRUE)
            selected[]
        },

        ambiguous_catalog_download_candidates = function(plan) {
            plan <- data.table::as.data.table(plan)
            if (!nrow(plan)) {
                return(data.table::data.table())
            }
            plan[, rank_signature := paste(
                retracted_order,
                deprecated_order,
                latest_order,
                version_missing,
                version_rank,
                replica_order,
                https_order,
                sep = "\r"
            )]
            groups <- plan[, .(
                n = .N,
                checksum_count = data.table::uniqueN(checksum[!is.na(checksum) & nzchar(checksum)]),
                top_signature_count = data.table::uniqueN(rank_signature[candidate_rank %in% c(1L, 2L)])
            ), by = "target_rel_path"]
            plan[, rank_signature := NULL]
            groups[n > 1L & checksum_count > 1L & top_signature_count == 1L]
        },

        decorate_download_plan = function(plan, query_id) {
            plan <- data.table::copy(data.table::as.data.table(plan))
            plan <- data.table::setalloccol(plan)
            if (!nrow(plan)) {
                return(plan)
            }
            catalog <- data.table::as.data.table(ddb_read_table(private$conn, "file_catalog"))
            wanted_query_id <- query_id
            catalog <- catalog[catalog[["query_id"]] == wanted_query_id]
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
            private$apply_download_layout(plan, catalog)
        },

        decorate_download_plan_with_files = function(plan, file_rows) {
            plan <- data.table::copy(data.table::as.data.table(plan))
            file_rows <- data.table::copy(data.table::as.data.table(file_rows))
            plan <- data.table::setalloccol(plan)
            if (!nrow(plan) || !nrow(file_rows)) {
                return(plan)
            }
            if (!"file_key" %in% names(plan)) {
                plan$file_key <- NA_character_
            }
            for (i in seq_len(nrow(plan))) {
                if (!is.na(plan$file_key[[i]]) && nzchar(plan$file_key[[i]])) {
                    next
                }
                hit <- file_rows[file_rows[["esgf_id"]] == plan$esgf_id[[i]]]
                if (!nrow(hit) && "checksum" %in% names(plan)) {
                    hit <- file_rows[
                        file_rows[["filename"]] == plan$filename[[i]] &
                            file_rows[["checksum"]] == plan$checksum[[i]]
                    ]
                }
                if (nrow(hit)) {
                    plan$file_key[[i]] <- hit$file_key[[1L]]
                }
            }
            private$apply_download_layout(plan, file_rows)
        },

        preflight_files = function(file_rows) {
            file_rows <- data.table::as.data.table(file_rows)
            if (!nrow(file_rows)) {
                return(file_rows)
            }
            file_rows[, status := private$file_link_status(file_rows)]
            file_rows[]
        },

        downloader_probe_concurrency = function(downloader = NULL, probe_concurrency = NULL) {
            if (!is.null(probe_concurrency)) {
                checkmate::assert_count(probe_concurrency, positive = TRUE)
                return(as.integer(probe_concurrency))
            }
            if (is.null(downloader)) {
                return(1L)
            }
            workers <- tryCatch(downloader$n_workers, error = function(e) 1L)
            workers <- suppressWarnings(as.integer(workers))
            if (is.na(workers) || workers < 1L) {
                workers <- 1L
            }
            as.integer(min(workers, 8L))
        },

        download_preflight_summary = function(row, file_rows, candidates, downloader = NULL) {
            files <- private$preflight_files(file_rows)
            current <- if (nrow(files)) files[files[["status"]] == "current"] else files
            local_available <- 0L
            if (nrow(current) && "local_path" %in% names(current)) {
                local_path <- current$local_path
                has_path <- !is.na(local_path) & nzchar(local_path)
                if (any(has_path)) {
                    local_available <- sum(file.exists(vapply(
                        local_path[has_path],
                        store_abs_path,
                        character(1L),
                        root = private$store_path
                    )))
                }
            }
            candidate_keys <- if (nrow(candidates) && "file_key" %in% names(candidates)) {
                unique(candidates$file_key[!is.na(candidates$file_key) & nzchar(candidates$file_key)])
            } else {
                character()
            }
            current_keys <- if (nrow(current)) unique(current$file_key) else character()
            cooling_nodes <- 0L
            if (nrow(candidates) && "node_cooldown_rank" %in% names(candidates)) {
                cooling <- candidates[candidates[["node_cooldown_rank"]] > 0L]
                if (nrow(cooling) && "data_node" %in% names(cooling)) {
                    cooling_nodes <- length(unique(cooling$data_node[!is.na(cooling$data_node) & nzchar(cooling$data_node)]))
                }
            }
            bytes_total <- if (nrow(current) && "size" %in% names(current)) {
                sum(suppressWarnings(as.numeric(current$size)), na.rm = TRUE)
            } else {
                0
            }
            target_collisions <- 0L
            if (nrow(candidates) && "target_path_collision" %in% names(candidates)) {
                collision_path <- if ("target_path_collision_group" %in% names(candidates)) {
                    candidates$target_path_collision_group
                } else {
                    candidates$target_rel_path
                }
                target_collisions <- length(unique(collision_path[
                    candidates$target_path_collision %in% TRUE & !is.na(collision_path) & nzchar(collision_path)
                ]))
            }
            missing_layout <- 0L
            if (nrow(candidates) && "layout_missing_fields" %in% names(candidates)) {
                missing_layout <- data.table::uniqueN(candidates$logical_file_id[
                    !is.na(candidates$layout_missing_fields) & nzchar(candidates$layout_missing_fields)
                ])
            }
            summary <- data.table::data.table(
                query_id = row$query_id[[1L]],
                label = store__chr1(row$label[[1L]]),
                file_total = as.integer(nrow(files)),
                current_count = as.integer(nrow(current)),
                candidate_count = as.integer(nrow(candidates)),
                bytes_total = as.numeric(bytes_total),
                local_available = as.integer(local_available),
                needs_download = as.integer(max(0L, length(current_keys) - local_available)),
                no_httpserver = as.integer(length(setdiff(current_keys, candidate_keys))),
                cooldown_nodes = as.integer(cooling_nodes),
                target_path_collision_count = as.integer(target_collisions),
                missing_layout_field_count = as.integer(missing_layout)
            )
            if (!is.null(downloader)) {
                space <- tryCatch(
                    downloader$preflight(plan = candidates, overwrite = FALSE),
                    error = function(e) NULL
                )
                if (!is.null(space) && nrow(space)) {
                    add <- c(
                        "required_bytes",
                        "size_unknown_count",
                        "dest_free_bytes",
                        "tmp_free_bytes",
                        "min_free_space",
                        "disk_ok",
                        "disk_would_block",
                        "disk_preflight"
                    )
                    for (col in intersect(add, names(space))) {
                        summary[[col]] <- space[[col]][[1L]]
                    }
                }
            }
            summary[]
        },

        match_download_task = function(task, catalog) {
            file_key <- store__chr1(task$file_key[[1L]])
            if (!is.na(file_key) && file_key %in% catalog$file_key) {
                return(file_key)
            }
            esgf_id <- store__chr1(task$esgf_id[[1L]])
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
            private$conn <- ddb_connect(private$manifest_path, read_only = FALSE)
            invisible(private$conn)
        },
        # }}}

        # disconnect {{{
        disconnect = function() {
            if (is.null(private$conn)) {
                return(invisible(NULL))
            }
            if (ddb_is_valid(private$conn)) {
                tryCatch(
                    ddb_disconnect(private$conn, shutdown = TRUE),
                    error = function(e) ddb_disconnect(private$conn)
                )
            }
            private$conn <- NULL
            invisible(NULL)
        },
        # }}}

        # init_schema {{{
        init_schema = function() {
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS store_meta (
                    key VARCHAR PRIMARY KEY,
                    value VARCHAR,
                    updated_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
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
            "
            )
            private$exec(
                "
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
            "
            )
            private$exec(
                "
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
            "
            )
            private$exec(
                "
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
                    activity_id VARCHAR,
                    institution_id VARCHAR,
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
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS esg_query_file (
                    link_id VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    file_key VARCHAR,
                    status VARCHAR,
                    first_seen_at TIMESTAMP,
                    last_seen_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS esg_query_update (
                    update_id VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    status VARCHAR,
                    started_at TIMESTAMP,
                    completed_at TIMESTAMP,
                    fields VARCHAR,
                    all_files BOOLEAN,
                    limit_files BOOLEAN,
                    file_total INTEGER,
                    current_count INTEGER,
                    new_count INTEGER,
                    stale_count INTEGER,
                    changed_count INTEGER,
                    deprecated_count INTEGER,
                    retracted_count INTEGER,
                    version_changed_count INTEGER,
                    download_session_id VARCHAR,
                    last_error VARCHAR
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS esg_query_update_file (
                    update_file_id VARCHAR PRIMARY KEY,
                    update_id VARCHAR,
                    query_id VARCHAR,
                    file_key VARCHAR,
                    change_type VARCHAR,
                    previous_status VARCHAR,
                    current_status VARCHAR,
                    previous_version VARCHAR,
                    current_version VARCHAR,
                    previous_checksum VARCHAR,
                    current_checksum VARCHAR,
                    previous_size DOUBLE,
                    current_size DOUBLE,
                    version_changed BOOLEAN,
                    checksum_changed BOOLEAN,
                    size_changed BOOLEAN,
                    url_changed BOOLEAN,
                    data_node_changed BOOLEAN,
                    deprecated BOOLEAN,
                    retracted BOOLEAN,
                    created_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS esg_query_tag (
                    tag_id VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    tag VARCHAR,
                    created_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS esg_query_dependency (
                    dependency_id VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    parent_query_id VARCHAR,
                    created_at TIMESTAMP
                )
            "
            )
            private$exec("ALTER TABLE esg_file ADD COLUMN IF NOT EXISTS deprecated BOOLEAN")
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS file_catalog (
                    file_key VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    esgf_id VARCHAR,
                    dataset_id VARCHAR,
                    master_id VARCHAR,
                    instance_id VARCHAR,
                    version VARCHAR,
                    title VARCHAR,
                    filename VARCHAR,
                    tracking_id VARCHAR,
                    checksum VARCHAR,
                    checksum_type VARCHAR,
                    size DOUBLE,
                    latest BOOLEAN,
                    replica BOOLEAN,
                    retracted BOOLEAN,
                    deprecated BOOLEAN,
                    data_node VARCHAR,
                    activity_id VARCHAR,
                    institution_id VARCHAR,
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
            "
            )
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS latest BOOLEAN")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS replica BOOLEAN")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS retracted BOOLEAN")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS deprecated BOOLEAN")
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS extraction_plan (
                    plan_id VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    file_key VARCHAR,
                    site_id VARCHAR,
                    variable_id VARCHAR,
                    lon DOUBLE,
                    lat DOUBLE,
                    method VARCHAR,
                    time_start TIMESTAMP,
                    time_stop TIMESTAMP,
                    status VARCHAR,
                    available_time_count INTEGER,
                    attempt_count INTEGER,
                    last_error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            "
            )
            private$exec("ALTER TABLE extraction_plan ADD COLUMN IF NOT EXISTS method VARCHAR")
            private$exec("UPDATE extraction_plan SET method = 'nearest' WHERE method IS NULL OR method = ''")
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS extraction_grid_source (
                    source_row_id VARCHAR PRIMARY KEY,
                    plan_id VARCHAR,
                    file_key VARCHAR,
                    query_id VARCHAR,
                    variable_id VARCHAR,
                    method VARCHAR,
                    source_index INTEGER,
                    role VARCHAR,
                    grid_lon DOUBLE,
                    grid_lat DOUBLE,
                    grid_dist_km DOUBLE,
                    weight DOUBLE,
                    created_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
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
            "
            )
            private$init_epw_morph_schema()

            private$migrate_schema()

            invisible(NULL)
        },
        # }}}

        # migrate_schema {{{
        migrate_schema = function() {
            current <- private$store_schema_version()
            private$migrate_schema_to_2(current)
            private$migrate_schema_to_2_1(current)
            private$migrate_schema_to_2_2(current)
            private$migrate_schema_to_2_3(current)
            private$migrate_schema_to_2_4(current)
            private$migrate_schema_to_2_5(current)
            private$set_store_schema_version(STORE_SCHEMA_VERSION)
            invisible(NULL)
        },

        store_schema_version = function() {
            meta <- tryCatch(private$read_table("store_meta"), error = function(e) data.table::data.table())
            if (!nrow(meta)) {
                return(NA_character_)
            }
            row <- meta[meta[["key"]] == "schema_version"]
            if (!nrow(row)) {
                return(NA_character_)
            }
            version <- as.character(row$value[[1L]])
            if (is.na(version) || !nzchar(version)) NA_character_ else version
        },

        set_store_schema_version = function(version) {
            private$replace_rows(
                "store_meta",
                data.frame(
                    key = "schema_version",
                    value = version,
                    updated_at = store__now(),
                    stringsAsFactors = FALSE
                ),
                "key"
            )
            invisible(NULL)
        },

        migrate_schema_to_2 = function(current) {
            if (!is.na(current)) {
                cmp <- tryCatch(utils::compareVersion(current, STORE_SCHEMA_VERSION), error = function(e) -1L)
                if (cmp > 0L) {
                    cli::cli_abort(
                        "Store manifest schema version {.val {current}} is newer than this package supports ({.val {STORE_SCHEMA_VERSION}})."
                    )
                }
            }
            private$exec("ALTER TABLE esg_query_update ADD COLUMN IF NOT EXISTS download_session_id VARCHAR")
            private$exec("ALTER TABLE esg_query_update ADD COLUMN IF NOT EXISTS last_error VARCHAR")
            invisible(NULL)
        },

        migrate_schema_to_2_1 = function(current) {
            private$exec("ALTER TABLE esg_file ADD COLUMN IF NOT EXISTS activity_id VARCHAR")
            private$exec("ALTER TABLE esg_file ADD COLUMN IF NOT EXISTS institution_id VARCHAR")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS master_id VARCHAR")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS instance_id VARCHAR")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS version VARCHAR")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS activity_id VARCHAR")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS institution_id VARCHAR")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS latest BOOLEAN")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS replica BOOLEAN")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS retracted BOOLEAN")
            private$exec("ALTER TABLE file_catalog ADD COLUMN IF NOT EXISTS deprecated BOOLEAN")
            invisible(NULL)
        },

        migrate_schema_to_2_2 = function(current) {
            private$init_epw_morph_schema()
            invisible(NULL)
        },

        migrate_schema_to_2_3 = function(current) {
            private$init_epw_morph_schema()
            private$exec("ALTER TABLE epw_climate_summary ADD COLUMN IF NOT EXISTS lon DOUBLE")
            private$exec("ALTER TABLE epw_climate_summary ADD COLUMN IF NOT EXISTS lat DOUBLE")
            private$exec("ALTER TABLE epw_climate_summary ADD COLUMN IF NOT EXISTS dist DOUBLE")
            private$exec("ALTER TABLE epw_climate_summary ADD COLUMN IF NOT EXISTS years_json VARCHAR")
            invisible(NULL)
        },

        migrate_schema_to_2_4 = function(current) {
            private$init_epw_morph_schema()
            private$exec("ALTER TABLE epw_morph_plan ADD COLUMN IF NOT EXISTS reference_summary_id VARCHAR")
            private$exec("ALTER TABLE epw_morph_factor ADD COLUMN IF NOT EXISTS reference DOUBLE")
            invisible(NULL)
        },

        migrate_schema_to_2_5 = function(current) {
            private$exec("ALTER TABLE extraction_plan ADD COLUMN IF NOT EXISTS method VARCHAR")
            private$exec("UPDATE extraction_plan SET method = 'nearest' WHERE method IS NULL OR method = ''")
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS extraction_grid_source (
                    source_row_id VARCHAR PRIMARY KEY,
                    plan_id VARCHAR,
                    file_key VARCHAR,
                    query_id VARCHAR,
                    variable_id VARCHAR,
                    method VARCHAR,
                    source_index INTEGER,
                    role VARCHAR,
                    grid_lon DOUBLE,
                    grid_lat DOUBLE,
                    grid_dist_km DOUBLE,
                    weight DOUBLE,
                    created_at TIMESTAMP
                )
            "
            )
            invisible(NULL)
        },
        # }}}

        # init_epw_morph_schema {{{
        init_epw_morph_schema = function() {
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS epw_source (
                    epw_id VARCHAR PRIMARY KEY,
                    artifact_id VARCHAR,
                    label VARCHAR,
                    site_id VARCHAR,
                    path VARCHAR,
                    checksum VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS epw_baseline_summary (
                    baseline_row_id VARCHAR PRIMARY KEY,
                    baseline_id VARCHAR,
                    epw_id VARCHAR,
                    epw_field VARCHAR,
                    month INTEGER,
                    stat VARCHAR,
                    value DOUBLE,
                    units VARCHAR,
                    created_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS epw_climate_summary (
                    summary_row_id VARCHAR PRIMARY KEY,
                    summary_id VARCHAR,
                    plan_id VARCHAR,
                    site_id VARCHAR,
                    source_id VARCHAR,
                    experiment_id VARCHAR,
                    variant_label VARCHAR,
                    frequency VARCHAR,
                    table_id VARCHAR,
                    variable_id VARCHAR,
                    period VARCHAR,
                    month INTEGER,
                    stat VARCHAR,
                    value DOUBLE,
                    units VARCHAR,
                    lon DOUBLE,
                    lat DOUBLE,
                    dist DOUBLE,
                    years_json VARCHAR,
                    coverage DOUBLE,
                    n_records INTEGER,
                    created_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS epw_morph_plan (
                    morph_id VARCHAR PRIMARY KEY,
                    epw_id VARCHAR,
                    summary_id VARCHAR,
                    reference_summary_id VARCHAR,
                    baseline_id VARCHAR,
                    label VARCHAR,
                    by_json VARCHAR,
                    recipe_json VARCHAR,
                    strict BOOLEAN,
                    status VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP,
                    last_error VARCHAR
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS epw_morph_factor (
                    factor_id VARCHAR PRIMARY KEY,
                    morph_id VARCHAR,
                    case_id VARCHAR,
                    epw_field VARCHAR,
                    variable_id VARCHAR,
                    source_id VARCHAR,
                    experiment_id VARCHAR,
                    variant_label VARCHAR,
                    period VARCHAR,
                    month INTEGER,
                    method VARCHAR,
                    baseline DOUBLE,
                    reference DOUBLE,
                    future DOUBLE,
                    delta DOUBLE,
                    alpha DOUBLE,
                    units VARCHAR,
                    status VARCHAR
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS epw_morph_result (
                    result_id VARCHAR PRIMARY KEY,
                    morph_id VARCHAR,
                    case_id VARCHAR,
                    artifact_id VARCHAR,
                    output_path VARCHAR,
                    row_count INTEGER,
                    created_at TIMESTAMP
                )
            "
            )
            private$exec(
                "
                CREATE TABLE IF NOT EXISTS epw_output (
                    output_id VARCHAR PRIMARY KEY,
                    morph_id VARCHAR,
                    case_id VARCHAR,
                    artifact_id VARCHAR,
                    path VARCHAR,
                    source_id VARCHAR,
                    experiment_id VARCHAR,
                    variant_label VARCHAR,
                    period VARCHAR,
                    created_at TIMESTAMP
                )
            "
            )
            invisible(NULL)
        },
        # }}}

        # exec {{{
        exec = function(sql) {
            ddb_exec(private$conn, sql)
        },
        # }}}

        # read_table {{{
        read_table = function(table) {
            data.table::as.data.table(ddb_read_table(private$conn, table))
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

        # with_store_lock {{{
        with_store_lock = function(expr) {
            private$check_open()
            if (private$lock_depth > 0L) {
                return(force(expr))
            }
            private$lock_depth <- private$lock_depth + 1L
            on.exit({
                private$lock_depth <- max(0L, private$lock_depth - 1L)
            }, add = TRUE)
            manifest_with_lock(private$manifest_path, force(expr))
        },
        # }}}

        # query_payload {{{
        query_payload = function(query) {
            state <- query$state(null = TRUE)
            parameter <- priv(query)$parameter$serialize(null = TRUE)
            parameter_json <- jsonlite::toJSON(parameter, auto_unbox = TRUE, null = "null", digits = 6)
            list(
                query_id = store__hash("EsgQuery", state$index_node, parameter_json),
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
            private$with_store_lock({
            row$tracked <- isTRUE(tracked)
            row$updated_at <- store__now()
            private$replace_rows("esg_query", as.data.frame(row), "query_id")
            })
            invisible(NULL)
        },
        # }}}

        # select_query_rows {{{
        select_query_rows = function(query_id = NULL, tracked = NULL) {
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
        },
        # }}}

        # resolve_query_selection {{{
        resolve_query_selection = function(query_id = NULL, tag = NULL, children = FALSE) {
            ids <- query_id
            if (is.null(ids) && !is.null(tag)) {
                wanted_tag <- tag
                tags <- private$read_table("esg_query_tag")
                tags <- tags[tags[["tag"]] %in% wanted_tag]
                ids <- unique(tags$query_id)
            }
            if (isTRUE(children) && !is.null(ids) && length(ids)) {
                ids <- unique(c(ids, private$query_related_ids(ids, direction = "children", recursive = TRUE)))
            }
            ids
        },
        # }}}

        # query_related_ids {{{
        query_related_ids = function(query_id, direction = c("children", "parents", "both"), recursive = TRUE) {
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
                    next_ids <- c(next_ids, edges[edges[["parent_query_id"]] %in% frontier]$query_id)
                }
                if (direction %in% c("parents", "both")) {
                    next_ids <- c(next_ids, edges[edges[["query_id"]] %in% frontier]$parent_query_id)
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

        # summarise_query_status {{{
        summarise_query_status = function(queries, links, files, tasks) {
            empty_counts <- stats::setNames(as.list(rep(0L, length(DOWNLOADER_TASK_STATUS))), paste0("download_", DOWNLOADER_TASK_STATUS))
            rows <- vector("list", nrow(queries))
            for (i in seq_len(nrow(queries))) {
                query <- queries[i]
                qid <- query$query_id[[1L]]
                q_links <- links[links[["query_id"]] == qid]
                q_files <- if (nrow(q_links) && nrow(files)) {
                    merge(q_links, files, by = "file_key", all.x = TRUE, sort = FALSE)
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

                q_tasks <- if (nrow(tasks) && nrow(q_links) && "file_key" %in% names(tasks)) {
                    tasks[tasks[["file_key"]] %in% q_links$file_key]
                } else {
                    data.table::data.table()
                }
                if (nrow(q_tasks)) {
                    data.table::setorderv(q_tasks, intersect(c("file_key", "updated_at", "created_at"), names(q_tasks)))
                    q_tasks <- q_tasks[, .SD[.N], by = "file_key"]
                }
                task_counts <- empty_counts
                if (nrow(q_tasks)) {
                    counted <- table(factor(q_tasks$status, levels = DOWNLOADER_TASK_STATUS))
                    task_counts <- as.list(as.integer(counted))
                    names(task_counts) <- paste0("download_", DOWNLOADER_TASK_STATUS)
                }

                bytes_total <- if (nrow(current) && "size" %in% names(current)) {
                    sum(suppressWarnings(as.numeric(current$size)), na.rm = TRUE)
                } else {
                    0
                }
                bytes_done <- if (nrow(q_tasks) && "bytes_done" %in% names(q_tasks)) {
                    sum(suppressWarnings(as.numeric(q_tasks$bytes_done[q_tasks$status %in% c("done", "skipped")])), na.rm = TRUE)
                } else {
                    0
                }

                current_count <- sum(q_links$status == "current", na.rm = TRUE)
                complete <- current_count > 0L &&
                    local_available >= current_count &&
                    sum(unlist(task_counts[c("download_downloading", "download_error", "download_cancelled")])) == 0L

                rows[[i]] <- data.table::as.data.table(c(
                    list(
                        query_id = qid,
                        label = store__chr1(query$label[[1L]]),
                        tracked = store__flag(query$tracked[[1L]]),
                        file_total = nrow(q_links),
                        file_current = current_count,
                        file_missing = sum(q_links$status == "missing", na.rm = TRUE),
                        file_retracted = sum(q_links$status == "retracted", na.rm = TRUE),
                        file_deprecated = sum(q_links$status == "deprecated", na.rm = TRUE)
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
        },
        # }}}

        # workflow_downloads {{{
        workflow_downloads = function(query_id, downloader) {
            tasks <- tryCatch(data.table::as.data.table(downloader$tasks()), error = function(e) data.table::data.table())
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
            link_cols <- links[, .(query_id, file_key, query_file_status = status)]
            tasks <- merge(tasks, link_cols, by = "file_key", all.x = TRUE, sort = FALSE)
            order_cols <- intersect(c("file_key", "updated_at", "created_at"), names(tasks))
            if (length(order_cols)) {
                data.table::setorderv(tasks, order_cols)
            }
            tasks[, .SD[.N], by = "file_key"][]
        },
        # }}}

        # load_query {{{
        load_query = function(row) {
            file <- store_abs_path(row$query_file[[1L]], root = private$store_path)
            esg_query()$load(file)
        },
        # }}}

        # update_query_files {{{
        update_query_files = function(query_id, files, fields = "*", all = TRUE, limit = FALSE) {
            store__result_type(files)
            dt <- store__file_table(files)
            now <- store__now()
            existing_links <- private$read_table("esg_query_file")
            wanted_query_id <- query_id
            existing_links <- existing_links[existing_links[["query_id"]] == wanted_query_id]
            existing_files <- private$read_table("esg_file")
            if (nrow(existing_links) && nrow(existing_files)) {
                existing_files <- existing_files[existing_files[["file_key"]] %in% existing_links$file_key]
            } else {
                existing_files <- existing_files[0]
            }
            file_rows <- private$file_rows(dt, now)
            update_id <- store__hash(query_id, now, nrow(file_rows), stats::runif(1L))
            changes <- private$query_update_changes(update_id, query_id, existing_links, existing_files, file_rows, now)

            if (nrow(file_rows)) {
                private$replace_rows("esg_file", as.data.frame(file_rows), "file_key")
                private$sync_file_catalog(query_id, file_rows)
            }

            private$sync_query_file_links(query_id, file_rows, now)
            query <- private$get_query_row(query_id)
            query$last_checked_at <- now
            query$updated_at <- now
            private$replace_rows("esg_query", as.data.frame(query), "query_id")
            private$record_query_update(
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
                    "query_id", "file_key", "update_id", "change_type",
                    "previous_status", "current_status", "previous_version",
                    "current_version", "previous_checksum", "current_checksum",
                    "version_changed", "checksum_changed", "size_changed",
                    "url_changed", "data_node_changed"
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
        },
        # }}}

        # preview_query_update {{{
        preview_query_update = function(row, files, fields = "*", all = TRUE, limit = FALSE) {
            store__result_type(files)
            dt <- store__file_table(files)
            now <- store__now()
            query_id <- row$query_id[[1L]]
            existing_links <- private$read_table("esg_query_file")
            wanted_query_id <- query_id
            existing_links <- existing_links[existing_links[["query_id"]] == wanted_query_id]
            existing_files <- private$read_table("esg_file")
            if (nrow(existing_links) && nrow(existing_files)) {
                existing_files <- existing_files[existing_files[["file_key"]] %in% existing_links$file_key]
            } else {
                existing_files <- existing_files[0]
            }
            file_rows <- private$file_rows(dt, now)
            update_id <- store__hash("preview", query_id, now, nrow(file_rows), stats::runif(1L))
            changes <- private$query_update_changes(update_id, query_id, existing_links, existing_files, file_rows, now)
            summary <- private$query_update_preview_summary(row, changes)
            list(summary = summary, changes = changes, files = files, file_rows = file_rows)
        },
        # }}}

        # query_update_preview_summary {{{
        query_update_preview_summary = function(row, changes) {
            count_type <- function(type) {
                if (!nrow(changes)) {
                    return(0L)
                }
                as.integer(sum(changes$change_type == type, na.rm = TRUE))
            }
            count_flag <- function(column) {
                if (!nrow(changes) || !column %in% names(changes)) {
                    return(0L)
                }
                as.integer(sum(changes[[column]] %in% TRUE, na.rm = TRUE))
            }
            sum_size <- function(rows) {
                if (!nrow(rows) || !"current_size" %in% names(rows)) {
                    return(0)
                }
                sum(suppressWarnings(as.numeric(rows$current_size)), na.rm = TRUE)
            }

            active <- if (nrow(changes)) {
                changes[changes[["current_status"]] == "current"]
            } else {
                changes
            }
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
                bytes_new = as.numeric(sum_size(active[active[["change_type"]] == "new"])),
                bytes_changed = as.numeric(sum_size(active[active[["change_type"]] == "changed"]))
            )
        },
        # }}}

        # query_update_changes {{{
        query_update_changes = function(update_id, query_id, existing_links, existing_files, file_rows, now) {
            current_keys <- if (nrow(file_rows)) unique(file_rows$file_key) else character()
            previous_keys <- if (nrow(existing_links)) unique(existing_links$file_key) else character()
            keys <- unique(c(previous_keys, current_keys))
            if (!length(keys)) {
                return(data.table::data.table())
            }
            rows <- vector("list", length(keys))
            for (i in seq_along(keys)) {
                key <- keys[[i]]
                previous_link <- existing_links[existing_links[["file_key"]] == key][1L]
                previous_file <- existing_files[existing_files[["file_key"]] == key][1L]
                current_file <- if (nrow(file_rows)) file_rows[file_rows[["file_key"]] == key][1L] else file_rows
                has_previous <- nrow(previous_link) && !is.na(previous_link$file_key[[1L]])
                has_current <- nrow(current_file) && !is.na(current_file$file_key[[1L]])
                previous_status <- if (has_previous) previous_link$status[[1L]] else NA_character_
                current_status <- if (has_current) private$file_link_status(current_file)[[1L]] else "missing"

                version_changed <- private$query_update_changed(previous_file, current_file, "version")
                checksum_changed <- private$query_update_changed(previous_file, current_file, "checksum")
                size_changed <- private$query_update_changed(previous_file, current_file, "size")
                url_changed <- private$query_update_changed(previous_file, current_file, "url_download")
                data_node_changed <- private$query_update_changed(previous_file, current_file, "data_node")
                changed <- any(c(version_changed, checksum_changed, size_changed, url_changed, data_node_changed), na.rm = TRUE)
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
                    previous_version = private$query_update_value(previous_file, "version"),
                    current_version = private$query_update_value(current_file, "version"),
                    previous_checksum = private$query_update_value(previous_file, "checksum"),
                    current_checksum = private$query_update_value(current_file, "checksum"),
                    previous_size = suppressWarnings(as.numeric(private$query_update_value(previous_file, "size"))),
                    current_size = suppressWarnings(as.numeric(private$query_update_value(current_file, "size"))),
                    version_changed = isTRUE(version_changed),
                    checksum_changed = isTRUE(checksum_changed),
                    size_changed = isTRUE(size_changed),
                    url_changed = isTRUE(url_changed),
                    data_node_changed = isTRUE(data_node_changed),
                    deprecated = has_current && store__flag(current_file$deprecated[[1L]]),
                    retracted = has_current && store__flag(current_file$retracted[[1L]]),
                    created_at = now
                )
            }
            data.table::rbindlist(rows, fill = TRUE)
        },
        # }}}

        # query_update_value {{{
        query_update_value = function(row, column) {
            if (!nrow(row) || !column %in% names(row)) {
                return(NA_character_)
            }
            store__chr1(row[[column]][[1L]])
        },
        # }}}

        # query_update_changed {{{
        query_update_changed = function(previous_row, current_row, column) {
            if (!nrow(previous_row) || !nrow(current_row)) {
                return(FALSE)
            }
            previous <- private$query_update_value(previous_row, column)
            current <- private$query_update_value(current_row, column)
            if (is.na(previous) && is.na(current)) {
                return(FALSE)
            }
            !identical(as.character(previous), as.character(current))
        },
        # }}}

        # record_query_update {{{
        record_query_update = function(update_id, query_id, changes, fields, all, limit, started_at, completed_at) {
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
                current_count = as.integer(sum(changes$change_type == "current", na.rm = TRUE)),
                new_count = as.integer(sum(changes$change_type == "new", na.rm = TRUE)),
                stale_count = as.integer(sum(changes$change_type == "stale", na.rm = TRUE)),
                changed_count = as.integer(sum(changes$change_type == "changed", na.rm = TRUE)),
                deprecated_count = as.integer(sum(changes$deprecated %in% TRUE, na.rm = TRUE)),
                retracted_count = as.integer(sum(changes$retracted %in% TRUE, na.rm = TRUE)),
                version_changed_count = as.integer(sum(changes$version_changed %in% TRUE, na.rm = TRUE)),
                download_session_id = NA_character_,
                last_error = NA_character_,
                stringsAsFactors = FALSE
            )
            private$replace_rows("esg_query_update", summary, "update_id")
            invisible(summary)
        },
        # }}}

        # set_query_update_session {{{
        set_query_update_session = function(update_id, session_id) {
            if (is.na(update_id) || !nzchar(update_id) || is.na(session_id) || !nzchar(session_id)) {
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
        },
        # }}}

        # enqueue_query_download {{{
        enqueue_query_download = function(
            query_id,
            files,
            current,
            downloader,
            replica,
            session_label,
            service,
            probe,
            probe_concurrency,
            probe_cache_seconds,
            strategy,
            error_if_empty = TRUE
        ) {
            current <- data.table::as.data.table(current)
            if (!nrow(current)) {
                if (isTRUE(error_if_empty)) {
                    cli::cli_abort("Stored ESGF query {.val {query_id}} has no current files to download.")
                }
                return(NA_character_)
            }

            # Use the committed current-file snapshot supplied by the caller.
            node_stats <- tryCatch(downloader$data_nodes(service = service), error = function(e) NULL)
            network_policy <- tryCatch(downloader$network_policy, error = function(e) NULL)
            node_policy <- tryCatch(downloader$node_policy, error = function(e) NULL)
            probe_concurrency <- private$downloader_probe_concurrency(downloader, probe_concurrency)
            plan <- files$download_plan(
                replica = replica,
                service = service,
                probe = probe,
                strategy = strategy,
                node_stats = node_stats,
                network_policy = network_policy,
                node_policy = node_policy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds
            )
            plan <- private$decorate_download_plan_with_files(plan, current)
            plan <- plan[plan[["file_key"]] %in% current$file_key]
            if (!nrow(plan)) {
                if (isTRUE(error_if_empty)) {
                    cli::cli_abort("Stored ESGF query {.val {query_id}} has no downloadable HTTPServer URLs.")
                }
                return(NA_character_)
            }
            if (is.null(session_label)) {
                session_label <- query_id
            }
            tryCatch(downloader$record_probes(plan, probed = probe), error = function(e) NULL)
            downloader$enqueue(plan, session_label = session_label)
        },
        # }}}

        # file_rows {{{
        file_rows = function(dt, now) {
            if (!nrow(dt)) {
                return(data.table::data.table())
            }
            dt <- data.table::as.data.table(dt)
            dt[, file_key := store__file_keys(.SD)]
            dt[, `:=`(
                row_order = seq_len(.N),
                replica_order = data.table::fifelse(store__lgl(replica) %in% TRUE, 1L, 0L),
                retracted_order = data.table::fifelse(store__lgl(retracted) %in% TRUE, 1L, 0L),
                deprecated_order = data.table::fifelse(store__lgl(deprecated) %in% TRUE, 1L, 0L)
            )]
            data.table::setorderv(
                dt,
                c("file_key", "retracted_order", "deprecated_order", "replica_order", "row_order")
            )
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
                link_id = vapply(current_keys, function(file_key) store__hash(qid, file_key), character(1L)),
                query_id = qid,
                file_key = current_keys,
                status = private$file_link_status(file_rows[match(current_keys, file_rows$file_key)]),
                first_seen_at = store__match_time(existing_first_seen, current_keys, now),
                last_seen_at = now
            )
            private$replace_rows("esg_query_file", as.data.frame(rows), "link_id")
            invisible(NULL)
        },
        # }}}

        # file_link_status {{{
        file_link_status = function(file_rows) {
            status <- rep("current", nrow(file_rows))
            deprecated <- store__lgl(file_rows$deprecated)
            retracted <- store__lgl(file_rows$retracted)
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
        },
        # }}}

        # sync_tracked_file_download {{{
        sync_tracked_file_download = function(file_key, local_path, artifact_id) {
            files <- private$read_table("esg_file")
            wanted_file_key <- file_key
            hit <- files[files[["file_key"]] == wanted_file_key]
            if (!nrow(hit)) {
                return(invisible(NULL))
            }
            hit$local_path <- store_rel_path(local_path, private$store_path)
            hit$local_artifact_id <- artifact_id
            hit$updated_at <- store__now()
            private$replace_rows("esg_file", as.data.frame(hit), "file_key")
            invisible(NULL)
        },
        # }}}

        # validate_download_files {{{
        validate_download_files = function(query_id = NULL, checksum = FALSE, layout = TRUE) {
            catalog <- private$read_table("file_catalog")
            esg_files <- private$read_table("esg_file")
            links <- private$read_table("esg_query_file")
            artifacts <- private$read_table("artifact")
            artifacts <- artifacts[artifacts[["kind"]] == "netcdf"]

            query_label <- NA_character_
            if (!is.null(query_id)) {
                query_label <- paste(query_id, collapse = ",")
                wanted_query <- query_id
                link_keys <- if (nrow(links)) {
                    links[links[["query_id"]] %in% wanted_query]$file_key
                } else {
                    character()
                }
                catalog <- catalog[catalog[["query_id"]] %in% wanted_query | catalog[["file_key"]] %in% link_keys]
                esg_files <- esg_files[esg_files[["file_key"]] %in% link_keys]
                artifact_ids <- unique(c(
                    catalog$local_artifact_id,
                    esg_files$local_artifact_id
                ))
                artifact_ids <- artifact_ids[!is.na(artifact_ids) & nzchar(artifact_ids)]
                artifacts <- artifacts[
                    artifacts[["query_id"]] %in% wanted_query |
                        artifacts[["file_key"]] %in% link_keys |
                        artifacts[["artifact_id"]] %in% artifact_ids
                ]
            }

            storage <- private$download_storage_report()
            files <- private$validate_download_file_rows(catalog, esg_files, artifacts, checksum = checksum, layout = layout)
            artifacts <- private$validate_download_artifact_rows(artifacts, catalog, esg_files)
            actions <- private$validate_download_actions(files, artifacts)
            summary <- data.table::data.table(
                query_id = query_label,
                file_count = as.integer(nrow(files)),
                registered_file_count = as.integer(sum(!is.na(files$local_path) & nzchar(files$local_path), na.rm = TRUE)),
                existing_file_count = as.integer(sum(files$exists %in% TRUE, na.rm = TRUE)),
                missing_file_count = as.integer(sum(files$missing_local_ref %in% TRUE, na.rm = TRUE)),
                bad_size_count = as.integer(sum(files$bad_size %in% TRUE, na.rm = TRUE)),
                bad_checksum_count = as.integer(sum(files$bad_checksum %in% TRUE, na.rm = TRUE)),
                artifact_mismatch_count = as.integer(sum(files$artifact_mismatch %in% TRUE, na.rm = TRUE)),
                layout_mismatch_count = as.integer(sum(files$layout_mismatch %in% TRUE, na.rm = TRUE)),
                untracked_file_count = as.integer(nrow(storage$untracked_files)),
                action_count = as.integer(nrow(actions))
            )
            list(
                summary = summary[],
                files = files[],
                artifacts = artifacts[],
                untracked = storage$untracked_files[],
                actions = actions[]
            )
        },

        validate_download_file_rows = function(catalog, esg_files, artifacts, checksum = FALSE, layout = TRUE) {
            catalog <- data.table::as.data.table(catalog)
            esg_files <- data.table::as.data.table(esg_files)
            artifacts <- data.table::as.data.table(artifacts)
            if (!nrow(catalog)) {
                return(private$validate_download_empty_files())
            }

            if (nrow(esg_files)) {
                local_cols <- c("file_key", "local_path", "local_artifact_id")
                esg_local <- esg_files[, intersect(local_cols, names(esg_files)), with = FALSE]
                data.table::setnames(
                    esg_local,
                    intersect(c("local_path", "local_artifact_id"), names(esg_local)),
                    paste0("esg_", intersect(c("local_path", "local_artifact_id"), names(esg_local)))
                )
                catalog <- merge(catalog, esg_local, by = "file_key", all.x = TRUE, sort = FALSE)
                if ("esg_local_path" %in% names(catalog)) {
                    fill <- is.na(catalog$local_path) | !nzchar(catalog$local_path)
                    catalog$local_path[fill] <- catalog$esg_local_path[fill]
                }
                if ("esg_local_artifact_id" %in% names(catalog)) {
                    fill <- is.na(catalog$local_artifact_id) | !nzchar(catalog$local_artifact_id)
                    catalog$local_artifact_id[fill] <- catalog$esg_local_artifact_id[fill]
                }
            }

            local_path <- store__chr(catalog$local_path)
            actual_path <- vapply(local_path, function(path) {
                if (is.na(path) || !nzchar(path)) {
                    return(NA_character_)
                }
                store_abs_path(path, root = private$store_path)
            }, character(1L))
            exists <- !is.na(actual_path) & file.exists(actual_path)
            size_actual <- rep(NA_real_, nrow(catalog))
            if (any(exists)) {
                size_actual[exists] <- as.numeric(file.info(actual_path[exists], extra_cols = FALSE)$size)
            }
            size_expected <- suppressWarnings(as.numeric(catalog$size))
            size_ok <- rep(NA, nrow(catalog))
            check_size <- exists & !is.na(size_expected)
            size_ok[check_size] <- size_actual[check_size] == size_expected[check_size]

            checksum_type <- tolower(store__chr(catalog$checksum_type))
            checksum_type[!checksum_type %in% c("md5", "sha256")] <- NA_character_
            checksum_expected <- store__chr(catalog$checksum)
            checksum_actual <- rep(NA_character_, nrow(catalog))
            checksum_ok <- rep(NA, nrow(catalog))
            if (isTRUE(checksum)) {
                check_hash <- exists & !is.na(checksum_expected) & nzchar(checksum_expected) & !is.na(checksum_type)
                if (any(check_hash)) {
                    checksum_actual[check_hash] <- mapply(
                        store_hash_file,
                        actual_path[check_hash],
                        checksum_type[check_hash],
                        USE.NAMES = FALSE
                    )
                    checksum_ok[check_hash] <- tolower(checksum_actual[check_hash]) == tolower(checksum_expected[check_hash])
                }
            }

            expected_path <- rep(NA_character_, nrow(catalog))
            expected_rel_path <- rep(NA_character_, nrow(catalog))
            if (isTRUE(layout)) {
                plan <- private$validation_catalog_download_plan(catalog)
                if (nrow(plan)) {
                    idx <- match(catalog$file_key, plan$file_key)
                    has <- !is.na(idx)
                    expected_path[has] <- plan$target_path[idx[has]]
                    expected_rel_path[has] <- vapply(
                        expected_path[has],
                        store_rel_path,
                        character(1L),
                        root = private$store_path
                    )
                }
            }
            layout_ok <- rep(NA, nrow(catalog))
            check_layout <- isTRUE(layout) & exists & !is.na(expected_path) & nzchar(expected_path)
            if (any(check_layout)) {
                layout_ok[check_layout] <- store_normalize_path(actual_path[check_layout]) == store_normalize_path(expected_path[check_layout])
            }

            artifact_id <- store__chr(catalog$local_artifact_id)
            artifact_idx <- match(artifact_id, artifacts$artifact_id)
            artifact_record_exists <- !is.na(artifact_id) & !is.na(artifact_idx)
            artifact_relative_path <- rep(NA_character_, nrow(catalog))
            artifact_path <- rep(NA_character_, nrow(catalog))
            artifact_path_exists <- rep(NA, nrow(catalog))
            if (any(artifact_record_exists)) {
                artifact_relative_path[artifact_record_exists] <- artifacts$relative_path[artifact_idx[artifact_record_exists]]
                artifact_path[artifact_record_exists] <- vapply(
                    artifact_relative_path[artifact_record_exists],
                    store_abs_path,
                    character(1L),
                    root = private$store_path
                )
                artifact_path_exists[artifact_record_exists] <- file.exists(artifact_path[artifact_record_exists])
            }
            artifact_path_matches <- rep(NA, nrow(catalog))
            check_artifact_path <- artifact_record_exists & !is.na(local_path) & nzchar(local_path)
            artifact_path_matches[check_artifact_path] <- artifact_relative_path[check_artifact_path] == local_path[check_artifact_path]

            missing_local_ref <- !is.na(local_path) & nzchar(local_path) & !exists
            bad_size <- size_ok %in% FALSE
            bad_checksum <- checksum_ok %in% FALSE
            artifact_mismatch <- (!is.na(artifact_id) & nzchar(artifact_id) & !artifact_record_exists) |
                (artifact_path_matches %in% FALSE)
            layout_mismatch <- layout_ok %in% FALSE
            issue <- private$validate_issue_vector(
                missing_local_ref = missing_local_ref,
                bad_size = bad_size,
                bad_checksum = bad_checksum,
                artifact_mismatch = artifact_mismatch,
                layout_mismatch = layout_mismatch
            )

            data.table::data.table(
                file_key = catalog$file_key,
                query_id = catalog$query_id,
                filename = catalog$filename,
                local_path = local_path,
                local_artifact_id = artifact_id,
                actual_path = actual_path,
                exists = exists,
                expected_path = expected_path,
                expected_rel_path = expected_rel_path,
                size_expected = size_expected,
                size_actual = size_actual,
                size_ok = size_ok,
                checksum_expected = checksum_expected,
                checksum_type = checksum_type,
                checksum_actual = checksum_actual,
                checksum_ok = checksum_ok,
                artifact_record_exists = artifact_record_exists,
                artifact_path = artifact_path,
                artifact_path_exists = artifact_path_exists,
                artifact_path_matches = artifact_path_matches,
                layout_ok = layout_ok,
                missing_local_ref = missing_local_ref,
                bad_size = bad_size,
                bad_checksum = bad_checksum,
                artifact_mismatch = artifact_mismatch,
                layout_mismatch = layout_mismatch,
                issue = issue
            )
        },

        validate_download_artifact_rows = function(artifacts, catalog, esg_files) {
            artifacts <- data.table::as.data.table(artifacts)
            if (!nrow(artifacts)) {
                return(private$validate_download_empty_artifacts())
            }
            catalog <- data.table::as.data.table(catalog)
            esg_files <- data.table::as.data.table(esg_files)
            refs <- list()
            if (nrow(catalog)) {
                refs$catalog <- data.table::data.table(
                    source = "catalog",
                    artifact_id = store__chr(catalog$local_artifact_id),
                    file_key = catalog$file_key,
                    relative_path = store__chr(catalog$local_path)
                )
            }
            if (nrow(esg_files)) {
                refs$esg_file <- data.table::data.table(
                    source = "esg_file",
                    artifact_id = store__chr(esg_files$local_artifact_id),
                    file_key = esg_files$file_key,
                    relative_path = store__chr(esg_files$local_path)
                )
            }
            refs <- if (length(refs)) data.table::rbindlist(refs, fill = TRUE) else data.table::data.table()
            if (nrow(refs)) {
                refs <- refs[!is.na(artifact_id) & nzchar(artifact_id)]
            }

            path <- vapply(artifacts$relative_path, store_abs_path, character(1L), root = private$store_path)
            exists <- file.exists(path)
            referenced <- if (nrow(refs)) artifacts$artifact_id %in% refs$artifact_id else rep(FALSE, nrow(artifacts))
            file_key_known <- artifacts$file_key %in% unique(c(catalog$file_key, esg_files$file_key))
            path_matches <- rep(NA, nrow(artifacts))
            if (nrow(refs)) {
                ref_split <- split(refs, refs$artifact_id)
                for (i in seq_len(nrow(artifacts))) {
                    ref <- ref_split[[artifacts$artifact_id[[i]]]]
                    if (is.null(ref) || !nrow(ref)) {
                        next
                    }
                    ref_path <- ref$relative_path[!is.na(ref$relative_path) & nzchar(ref$relative_path)]
                    if (length(ref_path)) {
                        path_matches[[i]] <- any(ref_path == artifacts$relative_path[[i]])
                    }
                }
            }

            issue <- private$validate_issue_vector(
                missing_artifact_file = !exists,
                unreferenced_artifact = !referenced,
                unknown_file_key = !file_key_known,
                artifact_path_mismatch = path_matches %in% FALSE
            )
            data.table::data.table(
                artifact_id = artifacts$artifact_id,
                file_key = artifacts$file_key,
                query_id = artifacts$query_id,
                relative_path = artifacts$relative_path,
                path = path,
                exists = exists,
                size = suppressWarnings(as.numeric(artifacts$size)),
                checksum = store__chr(artifacts$checksum),
                checksum_type = tolower(store__chr(artifacts$checksum_type)),
                referenced = referenced,
                file_key_known = file_key_known,
                path_matches = path_matches,
                issue = issue
            )
        },

        validate_download_actions = function(files, artifacts) {
            actions <- list()
            files <- data.table::as.data.table(files)
            artifacts <- data.table::as.data.table(artifacts)

            if (nrow(files)) {
                missing <- files[missing_local_ref %in% TRUE]
                if (nrow(missing)) {
                    actions$clear_missing <- data.table::data.table(
                        action = "clear_missing_local_ref",
                        file_key = missing$file_key,
                        artifact_id = missing$local_artifact_id,
                        from_path = missing$actual_path,
                        to_path = NA_character_,
                        relative_path = missing$local_path,
                        reason = "registered local file is missing",
                        safe = TRUE
                    )
                }
                move <- files[
                    layout_mismatch %in% TRUE &
                        exists %in% TRUE &
                        !is.na(expected_path) &
                        nzchar(expected_path)
                ]
                if (nrow(move)) {
                    move[, target_available := !file.exists(expected_path)]
                    move <- move[target_available %in% TRUE]
                }
                if (nrow(move)) {
                    actions$move_to_layout <- data.table::data.table(
                        action = "move_to_layout",
                        file_key = move$file_key,
                        artifact_id = move$local_artifact_id,
                        from_path = move$actual_path,
                        to_path = move$expected_path,
                        relative_path = move$expected_rel_path,
                        reason = "registered local file does not match current download layout",
                        safe = TRUE
                    )
                }
            }

            if (nrow(artifacts)) {
                missing_artifact <- artifacts[exists %in% FALSE]
                if (nrow(missing_artifact)) {
                    actions$missing_artifact <- data.table::data.table(
                        action = "remove_missing_artifact",
                        file_key = missing_artifact$file_key,
                        artifact_id = missing_artifact$artifact_id,
                        from_path = missing_artifact$path,
                        to_path = NA_character_,
                        relative_path = missing_artifact$relative_path,
                        reason = "artifact file is missing",
                        safe = TRUE
                    )
                }
                orphan_artifact <- artifacts[referenced %in% FALSE]
                if (nrow(orphan_artifact)) {
                    actions$orphan_artifact <- data.table::data.table(
                        action = "remove_orphan_artifact",
                        file_key = orphan_artifact$file_key,
                        artifact_id = orphan_artifact$artifact_id,
                        from_path = orphan_artifact$path,
                        to_path = NA_character_,
                        relative_path = orphan_artifact$relative_path,
                        reason = "artifact is not referenced by file records",
                        safe = TRUE
                    )
                }
            }

            if (!length(actions)) {
                return(private$validate_download_empty_actions())
            }
            out <- unique(data.table::rbindlist(actions, fill = TRUE))
            out[, action_id := vapply(seq_len(.N), function(i) {
                store__hash(action[[i]], file_key[[i]], artifact_id[[i]], from_path[[i]], to_path[[i]], reason[[i]])
            }, character(1L))]
            data.table::setcolorder(out, c(
                "action_id", "action", "file_key", "artifact_id", "from_path",
                "to_path", "relative_path", "reason", "safe"
            ))
            out[]
        },

        validation_catalog_download_plan = function(catalog) {
            catalog <- data.table::as.data.table(catalog)
            if (!nrow(catalog)) {
                return(data.table::data.table())
            }
            plan <- data.table::data.table(
                logical_file_id = vapply(
                    seq_len(nrow(catalog)),
                    function(i) {
                        pieces <- c(
                            store__chr1(catalog$tracking_id[[i]]),
                            store__chr1(catalog$checksum[[i]]),
                            store__chr1(catalog$filename[[i]]),
                            store__chr1(catalog$esgf_id[[i]])
                        )
                        paste(pieces[!is.na(pieces) & nzchar(pieces)], collapse = ":")
                    },
                    character(1L)
                ),
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
            private$apply_download_layout(plan, catalog)
        },

        validate_issue_vector = function(...) {
            flags <- list(...)
            n <- if (length(flags)) length(flags[[1L]]) else 0L
            if (!n) {
                return(character())
            }
            vapply(seq_len(n), function(i) {
                hit <- names(flags)[vapply(flags, function(x) x[[i]] %in% TRUE, logical(1L))]
                if (!length(hit)) {
                    return(NA_character_)
                }
                paste(hit, collapse = ",")
            }, character(1L))
        },

        validate_download_empty_files = function() {
            data.table::data.table(
                file_key = character(),
                query_id = character(),
                filename = character(),
                local_path = character(),
                local_artifact_id = character(),
                actual_path = character(),
                exists = logical(),
                expected_path = character(),
                expected_rel_path = character(),
                size_expected = numeric(),
                size_actual = numeric(),
                size_ok = logical(),
                checksum_expected = character(),
                checksum_type = character(),
                checksum_actual = character(),
                checksum_ok = logical(),
                artifact_record_exists = logical(),
                artifact_path = character(),
                artifact_path_exists = logical(),
                artifact_path_matches = logical(),
                layout_ok = logical(),
                missing_local_ref = logical(),
                bad_size = logical(),
                bad_checksum = logical(),
                artifact_mismatch = logical(),
                layout_mismatch = logical(),
                issue = character()
            )
        },

        validate_download_empty_artifacts = function() {
            data.table::data.table(
                artifact_id = character(),
                file_key = character(),
                query_id = character(),
                relative_path = character(),
                path = character(),
                exists = logical(),
                size = numeric(),
                checksum = character(),
                checksum_type = character(),
                referenced = logical(),
                file_key_known = logical(),
                path_matches = logical(),
                issue = character()
            )
        },

        validate_download_empty_actions = function() {
            data.table::data.table(
                action_id = character(),
                action = character(),
                file_key = character(),
                artifact_id = character(),
                from_path = character(),
                to_path = character(),
                relative_path = character(),
                reason = character(),
                safe = logical()
            )
        },
        # }}}

        # repair_download_files {{{
        repair_download_files = function(actions, dry_run = TRUE) {
            actions <- data.table::as.data.table(actions)
            if (!nrow(actions)) {
                return(private$repair_download_empty_results())
            }
            required <- c(
                "action_id", "action", "file_key", "artifact_id", "from_path",
                "to_path", "relative_path", "reason", "safe"
            )
            missing <- setdiff(required, names(actions))
            if (length(missing)) {
                cli::cli_abort("Repair action table is missing required column(s): {.field {missing}}.")
            }
            data.table::rbindlist(lapply(seq_len(nrow(actions)), function(i) {
                private$repair_download_action(actions[i], dry_run = dry_run)
            }), fill = TRUE)
        },

        repair_download_action = function(action, dry_run = TRUE) {
            base <- data.table::data.table(
                action_id = action$action_id[[1L]],
                action = action$action[[1L]],
                file_key = store__chr1(action$file_key[[1L]]),
                artifact_id = store__chr1(action$artifact_id[[1L]]),
                from_path = store__chr1(action$from_path[[1L]]),
                to_path = store__chr1(action$to_path[[1L]]),
                relative_path = store__chr1(action$relative_path[[1L]]),
                done = FALSE,
                dry_run = isTRUE(dry_run),
                message = NA_character_
            )
            if (!isTRUE(action$safe[[1L]])) {
                base$message <- "skipped unsafe action"
                return(base[])
            }
            if (isTRUE(dry_run)) {
                base$message <- "dry run"
                return(base[])
            }

            result <- tryCatch(
                switch(
                    action$action[[1L]],
                    clear_missing_local_ref = private$repair_clear_missing_local_ref(action),
                    remove_missing_artifact = private$repair_remove_artifact_record(action),
                    remove_orphan_artifact = private$repair_remove_artifact_record(action),
                    move_to_layout = private$repair_move_to_layout(action),
                    list(done = FALSE, message = sprintf("unsupported action: %s", action$action[[1L]]))
                ),
                error = function(e) list(done = FALSE, message = conditionMessage(e))
            )
            base$done <- isTRUE(result$done)
            base$message <- store__chr1(result$message)
            base[]
        },

        repair_clear_missing_local_ref = function(action) {
            file_key <- store__chr1(action$file_key[[1L]])
            if (is.na(file_key) || !nzchar(file_key)) {
                return(list(done = FALSE, message = "missing file_key"))
            }
            private$clear_file_local_reference(file_key, artifact_id = store__chr1(action$artifact_id[[1L]]))
            list(done = TRUE, message = "cleared missing local reference")
        },

        repair_remove_artifact_record = function(action) {
            artifact_id <- store__chr1(action$artifact_id[[1L]])
            if (is.na(artifact_id) || !nzchar(artifact_id)) {
                return(list(done = FALSE, message = "missing artifact_id"))
            }
            private$delete_by_key("artifact", "artifact_id", artifact_id)
            private$clear_file_artifact_reference(artifact_id)
            list(done = TRUE, message = "removed artifact record")
        },

        repair_move_to_layout = function(action) {
            file_key <- store__chr1(action$file_key[[1L]])
            artifact_id <- store__chr1(action$artifact_id[[1L]])
            from_path <- store__chr1(action$from_path[[1L]])
            to_path <- store__chr1(action$to_path[[1L]])
            relative_path <- store__chr1(action$relative_path[[1L]])
            if (is.na(file_key) || !nzchar(file_key)) {
                return(list(done = FALSE, message = "missing file_key"))
            }
            if (is.na(from_path) || !nzchar(from_path) || !file.exists(from_path)) {
                return(list(done = FALSE, message = "source file is missing"))
            }
            if (is.na(to_path) || !nzchar(to_path)) {
                return(list(done = FALSE, message = "missing target path"))
            }
            if (file.exists(to_path)) {
                return(list(done = FALSE, message = "target path already exists"))
            }
            from_path <- store_abs_path(from_path, root = private$store_path)
            to_path <- store_abs_path(to_path, root = private$store_path)
            download_root <- paste0(sub("/+$", "", store_normalize_path(private$download_dir)), "/")
            if (!startsWith(store_normalize_path(from_path), download_root) || !startsWith(store_normalize_path(to_path), download_root)) {
                return(list(done = FALSE, message = "paths are outside the store downloads directory"))
            }
            dir.create(dirname(to_path), recursive = TRUE, showWarnings = FALSE)
            moved <- file.rename(from_path, to_path)
            if (!isTRUE(moved)) {
                moved <- file.copy(from_path, to_path, overwrite = FALSE) && unlink(from_path, recursive = FALSE, force = TRUE) == 0L
            }
            if (!isTRUE(moved)) {
                return(list(done = FALSE, message = "failed to move file"))
            }
            if (is.na(relative_path) || !nzchar(relative_path)) {
                relative_path <- store_rel_path(to_path, root = private$store_path)
            }
            private$set_file_local_reference(file_key, relative_path, artifact_id = artifact_id)
            if (!is.na(artifact_id) && nzchar(artifact_id)) {
                artifacts <- private$read_table("artifact")
                row <- artifacts[artifacts[["artifact_id"]] == artifact_id]
                if (nrow(row)) {
                    row$relative_path <- relative_path
                    row$updated_at <- store__now()
                    private$replace_rows("artifact", as.data.frame(row), "artifact_id")
                }
            }
            private$remove_empty_download_dirs(dirname(from_path))
            list(done = TRUE, message = "moved file to current layout")
        },

        clear_file_local_reference = function(file_key, artifact_id = NA_character_) {
            for (table in c("file_catalog", "esg_file")) {
                rows <- private$read_table(table)
                rows <- rows[rows[["file_key"]] == file_key]
                if (!nrow(rows)) {
                    next
                }
                rows$local_path <- NA_character_
                rows$local_artifact_id <- NA_character_
                if ("updated_at" %in% names(rows)) {
                    rows$updated_at <- store__now()
                }
                private$replace_rows(table, as.data.frame(rows), "file_key")
            }
            invisible(NULL)
        },

        clear_file_artifact_reference = function(artifact_id) {
            if (is.na(artifact_id) || !nzchar(artifact_id)) {
                return(invisible(NULL))
            }
            for (table in c("file_catalog", "esg_file")) {
                rows <- private$read_table(table)
                rows <- rows[rows[["local_artifact_id"]] == artifact_id]
                if (!nrow(rows)) {
                    next
                }
                rows$local_artifact_id <- NA_character_
                if ("updated_at" %in% names(rows)) {
                    rows$updated_at <- store__now()
                }
                private$replace_rows(table, as.data.frame(rows), "file_key")
            }
            invisible(NULL)
        },

        set_file_local_reference = function(file_key, relative_path, artifact_id = NA_character_) {
            for (table in c("file_catalog", "esg_file")) {
                rows <- private$read_table(table)
                rows <- rows[rows[["file_key"]] == file_key]
                if (!nrow(rows)) {
                    next
                }
                rows$local_path <- relative_path
                rows$local_artifact_id <- artifact_id
                if ("updated_at" %in% names(rows)) {
                    rows$updated_at <- store__now()
                }
                private$replace_rows(table, as.data.frame(rows), "file_key")
            }
            invisible(NULL)
        },

        remove_empty_download_dirs = function(path) {
            if (is.na(path) || !nzchar(path)) {
                return(invisible(NULL))
            }
            path <- store_normalize_path(path)
            root <- store_normalize_path(private$download_dir)
            root_prefix <- paste0(sub("/+$", "", root), "/")
            while (dir.exists(path) && startsWith(path, root_prefix) && !identical(path, root)) {
                if (length(list.files(path, all.files = TRUE, no.. = TRUE))) {
                    break
                }
                unlink(path, recursive = TRUE, force = FALSE)
                path <- dirname(path)
            }
            invisible(NULL)
        },

        repair_download_empty_results = function() {
            data.table::data.table(
                action_id = character(),
                action = character(),
                file_key = character(),
                artifact_id = character(),
                from_path = character(),
                to_path = character(),
                relative_path = character(),
                done = logical(),
                dry_run = logical(),
                message = character()
            )
        },
        # }}}

        # download_storage_report {{{
        download_storage_report = function() {
            download_files <- private$list_store_files(private$download_dir)
            if (nrow(download_files)) {
                downloader_prefix <- paste0(file.path(private$download_dir, "_downloader"), "/")
                download_files <- download_files[!startsWith(path, downloader_prefix)]
            }
            tmp_files <- private$list_store_files(private$tmp_download_dir)
            registered <- private$registered_download_paths()
            registered_paths <- unique(registered$path[!is.na(registered$path) & nzchar(registered$path)])
            registered_unique <- registered[!is.na(path) & nzchar(path)]
            if (nrow(registered_unique)) {
                registered_unique <- registered_unique[!duplicated(path)]
            }
            untracked <- if (nrow(download_files)) {
                download_files[!path %in% registered_paths]
            } else {
                download_files
            }
            missing <- if (nrow(registered)) {
                registered[is.na(path) | !nzchar(path) | !file.exists(path)]
            } else {
                registered
            }
            orphans <- private$orphaned_files()

            summary <- data.table::data.table(
                download_file_count = as.integer(nrow(download_files)),
                download_bytes = as.numeric(sum(download_files$size, na.rm = TRUE)),
                registered_file_count = as.integer(nrow(registered_unique)),
                registered_bytes = as.numeric(sum(registered_unique$size, na.rm = TRUE)),
                tmp_file_count = as.integer(nrow(tmp_files)),
                tmp_bytes = as.numeric(sum(tmp_files$size, na.rm = TRUE)),
                untracked_file_count = as.integer(nrow(untracked)),
                untracked_bytes = as.numeric(sum(untracked$size, na.rm = TRUE)),
                missing_record_count = as.integer(nrow(missing)),
                orphan_record_count = as.integer(nrow(orphans))
            )
            list(
                summary = summary[],
                downloads = download_files[],
                registered = registered[],
                untracked_files = untracked[],
                missing_records = missing[],
                tmp = tmp_files[],
                orphan_records = orphans[]
            )
        },

        list_store_files = function(root) {
            empty <- data.table::data.table(
                path = character(),
                relative_path = character(),
                size = numeric(),
                mtime = as.POSIXct(character())
            )
            if (!dir.exists(root)) {
                return(empty)
            }
            files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
            if (!length(files)) {
                return(empty)
            }
            files <- files[file.exists(files) & !dir.exists(files)]
            if (!length(files)) {
                return(empty)
            }
            info <- file.info(files, extra_cols = FALSE)
            rel <- vapply(files, store_rel_path, character(1L), root = private$store_path)
            data.table::data.table(
                path = normalizePath(files, mustWork = FALSE, winslash = "/"),
                relative_path = rel,
                size = as.numeric(info$size),
                mtime = as.POSIXct(info$mtime, tz = "UTC")
            )
        },

        registered_download_paths = function() {
            rows <- list()
            artifacts <- private$read_table("artifact")
            if (nrow(artifacts)) {
                artifacts <- artifacts[artifacts[["kind"]] == "netcdf"]
                if (nrow(artifacts)) {
                    rows$artifact <- data.table::data.table(
                        source = "artifact",
                        artifact_id = artifacts$artifact_id,
                        file_key = artifacts$file_key,
                        relative_path = artifacts$relative_path,
                        size = suppressWarnings(as.numeric(artifacts$size))
                    )
                }
            }
            catalog <- private$read_table("file_catalog")
            if (nrow(catalog)) {
                catalog <- catalog[!is.na(local_path) & nzchar(local_path)]
                if (nrow(catalog)) {
                    rows$catalog <- data.table::data.table(
                        source = "catalog",
                        artifact_id = catalog$local_artifact_id,
                        file_key = catalog$file_key,
                        relative_path = catalog$local_path,
                        size = suppressWarnings(as.numeric(catalog$size))
                    )
                }
            }
            if (!length(rows)) {
                return(data.table::data.table(
                    source = character(),
                    artifact_id = character(),
                    file_key = character(),
                    relative_path = character(),
                    path = character(),
                    exists = logical(),
                    size = numeric()
                ))
            }
            out <- data.table::rbindlist(rows, fill = TRUE)
            out[, path := vapply(relative_path, function(path) {
                if (is.na(path) || !nzchar(path)) {
                    return(NA_character_)
                }
                store_abs_path(path, root = private$store_path)
            }, character(1L))]
            out[, exists := !is.na(path) & file.exists(path)]
            out[]
        },

        cleanup_cutoff = function(older_than = NULL) {
            if (is.null(older_than)) {
                return(NULL)
            }
            if (inherits(older_than, "POSIXt")) {
                return(as.POSIXct(older_than[[1L]], tz = "UTC"))
            }
            checkmate::assert_number(older_than, lower = 0)
            as.POSIXct(Sys.time() - older_than, tz = "UTC")
        },

        cleanup_file_scope = function(scope, files, dry_run = TRUE, cutoff = NULL) {
            files <- data.table::as.data.table(files)
            if (!nrow(files)) {
                return(private$cleanup_empty(scope))
            }
            if (!is.null(cutoff) && "mtime" %in% names(files)) {
                files <- files[mtime <= cutoff]
            }
            if (!nrow(files)) {
                return(private$cleanup_empty(scope))
            }
            deleted <- rep(FALSE, nrow(files))
            if (!isTRUE(dry_run)) {
                deleted <- file.exists(files$path) & unlink(files$path, recursive = FALSE, force = TRUE) == 0L
            }
            data.table::data.table(
                scope = scope,
                action = if (isTRUE(dry_run)) "delete_file" else "deleted_file",
                path = files$path,
                relative_path = files$relative_path,
                file_key = NA_character_,
                artifact_id = NA_character_,
                size = files$size,
                deleted = deleted,
                dry_run = isTRUE(dry_run)
            )
        },

        cleanup_orphan_records = function(orphans, dry_run = TRUE) {
            orphans <- data.table::as.data.table(orphans)
            if (!nrow(orphans)) {
                return(private$cleanup_empty("orphan_records"))
            }
            removed <- rep(FALSE, nrow(orphans))
            deleted <- rep(FALSE, nrow(orphans))
            if (!isTRUE(dry_run)) {
                result <- private$remove_file_records(orphans$file_key, delete_local = TRUE, force = TRUE)
                removed <- orphans$file_key %in% result$file_key
                deleted <- result$deleted_local[match(orphans$file_key, result$file_key)] %in% TRUE
            }
            data.table::data.table(
                scope = "orphan_records",
                action = if (isTRUE(dry_run)) "remove_record" else "removed_record",
                path = orphans$local_file,
                relative_path = orphans$local_path,
                file_key = orphans$file_key,
                artifact_id = orphans$local_artifact_id,
                size = suppressWarnings(as.numeric(file.info(orphans$local_file, extra_cols = FALSE)$size)),
                deleted = deleted,
                record_removed = removed,
                dry_run = isTRUE(dry_run)
            )
        },

        cleanup_missing_records = function(records, dry_run = TRUE) {
            records <- data.table::as.data.table(records)
            if (!nrow(records)) {
                return(private$cleanup_empty("missing_records"))
            }
            removed <- rep(FALSE, nrow(records))
            if (!isTRUE(dry_run)) {
                private$clear_missing_download_records(records)
                removed <- rep(TRUE, nrow(records))
            }
            data.table::data.table(
                scope = "missing_records",
                action = if (isTRUE(dry_run)) "clear_record" else "cleared_record",
                path = records$path,
                relative_path = records$relative_path,
                file_key = records$file_key,
                artifact_id = records$artifact_id,
                size = records$size,
                deleted = FALSE,
                record_removed = removed,
                dry_run = isTRUE(dry_run)
            )
        },

        clear_missing_download_records = function(records) {
            records <- data.table::as.data.table(records)
            artifact_ids <- unique(records$artifact_id[records$source == "artifact" & !is.na(records$artifact_id) & nzchar(records$artifact_id)])
            if (length(artifact_ids)) {
                private$delete_by_key("artifact", "artifact_id", artifact_ids)
            }
            file_keys <- unique(records$file_key[records$source == "catalog" & !is.na(records$file_key) & nzchar(records$file_key)])
            if (length(file_keys)) {
                for (table in c("file_catalog", "esg_file")) {
                    rows <- private$read_table(table)
                    rows <- rows[rows[["file_key"]] %in% file_keys]
                    if (nrow(rows)) {
                        rows$local_path <- NA_character_
                        rows$local_artifact_id <- NA_character_
                        private$replace_rows(table, as.data.frame(rows), "file_key")
                    }
                }
            }
            invisible(NULL)
        },

        cleanup_empty = function(scope_name) {
            data.table::data.table(
                scope = character(),
                action = character(),
                path = character(),
                relative_path = character(),
                file_key = character(),
                artifact_id = character(),
                size = numeric(),
                deleted = logical(),
                record_removed = logical(),
                dry_run = logical()
            )[0]
        },
        # }}}

        # orphaned_files {{{
        orphaned_files = function() {
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
            artifacts <- artifacts[artifacts[["file_key"]] %in% orphans$file_key & artifacts[["kind"]] == "netcdf"]
            artifacts <- artifacts[!duplicated(artifacts$file_key)]

            out <- merge(
                orphans,
                catalog[, .(file_key, catalog_local_path = local_path, catalog_artifact_id = local_artifact_id)],
                by = "file_key",
                all.x = TRUE,
                sort = FALSE
            )
            out <- merge(
                out,
                artifacts[, .(file_key, artifact_id = artifact_id, artifact_path = relative_path)],
                by = "file_key",
                all.x = TRUE,
                sort = FALSE
            )
            out[, orphan_local_path := local_path]
            out[is.na(orphan_local_path) | !nzchar(orphan_local_path), orphan_local_path := catalog_local_path]
            out[is.na(orphan_local_path) | !nzchar(orphan_local_path), orphan_local_path := artifact_path]
            out[, orphan_artifact_id := local_artifact_id]
            out[is.na(orphan_artifact_id) | !nzchar(orphan_artifact_id), orphan_artifact_id := catalog_artifact_id]
            out[is.na(orphan_artifact_id) | !nzchar(orphan_artifact_id), orphan_artifact_id := artifact_id]
            out[, local_file := vapply(orphan_local_path, function(path) {
                if (is.na(path) || !nzchar(path)) {
                    return(NA_character_)
                }
                store_abs_path(path, root = private$store_path)
            }, character(1L))]
            out[, local_exists := !is.na(local_file) & file.exists(local_file)]
            out[, .(
                file_key,
                local_path = orphan_local_path,
                local_artifact_id = orphan_artifact_id,
                local_file,
                local_exists
            )]
        },
        # }}}

        # remove_file_records {{{
        remove_file_records = function(file_key, delete_local = FALSE, force = FALSE) {
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
            artifacts <- artifacts[artifacts[["file_key"]] %in% keys & artifacts[["kind"]] == "netcdf"]

            local_path <- stats::setNames(rows$local_path, rows$file_key)
            if (nrow(catalog)) {
                missing_path <- is.na(local_path[catalog$file_key]) | !nzchar(local_path[catalog$file_key])
                local_path[catalog$file_key[missing_path]] <- catalog$local_path[missing_path]
            }
            if (nrow(artifacts)) {
                missing_path <- is.na(local_path[artifacts$file_key]) | !nzchar(local_path[artifacts$file_key])
                local_path[artifacts$file_key[missing_path]] <- artifacts$relative_path[missing_path]
            }

            local_file <- vapply(local_path[keys], function(path) {
                if (is.na(path) || !nzchar(path)) {
                    return(NA_character_)
                }
                store_abs_path(path, root = private$store_path)
            }, character(1L))
            deleted_local <- rep(FALSE, length(keys))
            if (isTRUE(delete_local)) {
                exists <- !is.na(local_file) & file.exists(local_file)
                deleted_local[exists] <- unlink(local_file[exists], recursive = FALSE, force = TRUE) == 0L
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
                removed_links = vapply(keys, function(key) sum(linked$file_key == key), integer(1L))
            )
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
            ddb_append_table(private$conn, table, rows)
            invisible(NULL)
        },
        # }}}

        # append_rows {{{
        append_rows = function(table, rows) {
            checkmate::assert_string(table)
            if (!nrow(rows)) {
                return(invisible(NULL))
            }
            ddb_append_table(private$conn, table, rows)
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

            q_table <- ddb_ident(private$conn, table)
            q_key <- ddb_ident(private$conn, key)
            current <- ddb_query(private$conn, sprintf("SELECT %s FROM %s", q_key, q_table))[[key]]
            rows <- rows[!rows[[key]] %in% current, , drop = FALSE]
            if (nrow(rows)) {
                ddb_append_table(private$conn, table, rows)
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

            q_table <- ddb_ident(private$conn, table)
            q_key <- ddb_ident(private$conn, key)
            q_values <- paste(ddb_literal(private$conn, values), collapse = ", ")
            ddb_exec(private$conn, sprintf("DELETE FROM %s WHERE %s IN (%s)", q_table, q_key, q_values))
            invisible(NULL)
        },
        # }}}

        # extract_one {{{
        resume_extract_plan = function(plan) {
            if (!identical(plan$status[[1L]], "done")) {
                return(NULL)
            }
            results <- data.table::as.data.table(ddb_read_table(private$conn, "extraction_result"))
            target_plan_id <- plan$plan_id[[1L]]
            results <- results[results[["plan_id"]] == target_plan_id]
            if (!nrow(results)) {
                return(NULL)
            }
            paths <- vapply(results$output_path, store_abs_path, character(1L), root = private$store_path)
            if (!all(file.exists(paths))) {
                return(NULL)
            }
            plan
        },

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
                method = plan$method[[1L]]
            )
            grid_sources <- attr(dt, "grid_sources", exact = TRUE)
            units <- tryCatch(
                as.character(ds$att_get(plan$variable_id[[1L]], "units", index = 1L))[[1L]],
                error = function(e) NA_character_
            )
            dt[, units := units]
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
            grid_sources <- private$decorate_extract_grid_sources(grid_sources, plan, file)
            private$delete_by_key("extraction_result", "plan_id", plan$plan_id)
            private$delete_by_key("extraction_grid_source", "plan_id", plan$plan_id)
            if (nrow(results)) {
                ddb_append_table(private$conn, "extraction_result", results)
            }
            if (nrow(grid_sources)) {
                ddb_append_table(private$conn, "extraction_grid_source", grid_sources)
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
            opendap <- store__chr1(file$url_opendap)
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
            download <- store__chr1(file$url_download)
            if (is.na(download) || !nzchar(download)) {
                stop("HTTPServer download URL is not available for this file record.", call. = FALSE)
            }

            checksum <- store__chr1(file$checksum)
            checksum_type <- tolower(store__chr1(file$checksum_type))
            if (is.na(checksum_type) || !checksum_type %in% c("md5", "sha256")) {
                checksum <- NULL
                checksum_type <- "sha256"
            }
            if (is.null(checksum) || is.na(checksum)) {
                checksum <- NULL
            }

            downloader <- self$downloader(n_workers = 0L)
            filename <- store__chr1(file$filename[[1L]])
            if (is.na(filename)) {
                filename <- basename(sub("\\?.*$", "", download))
            }
            logical_parts <- c(
                store__chr1(file$tracking_id[[1L]]),
                if (is.null(checksum)) NA_character_ else checksum,
                filename,
                store__chr1(file$file_key[[1L]])
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
            plan <- private$apply_download_layout(plan, file)
            session_id <- downloader$enqueue(plan, session_label = sprintf("extract:%s", file$file_key[[1L]]))
            tasks <- downloader$run(session_id = session_id, progress = FALSE, overwrite = overwrite)
            failed <- tasks[!tasks[["status"]] %in% c("done", "skipped"), , drop = FALSE]
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
                source_id = file$source_id[[1L]],
                experiment_id = file$experiment_id[[1L]],
                variant_label = file$variant_label[[1L]],
                frequency = file$frequency[[1L]],
                table_id = file$table_id[[1L]],
                variable_id = plan$variable_id[[1L]],
                method = plan$method[[1L]],
                grid_label = file$grid_label[[1L]]
            )]
            data.table::setcolorder(
                dt,
                c(
                    "plan_id",
                    "file_key",
                    "query_id",
                    "site_id",
                    "source_id",
                    "experiment_id",
                    "variant_label",
                    "frequency",
                    "table_id",
                    "variable_id",
                    "variable",
                    "grid_label",
                    "time",
                    "lon",
                    "lat",
                    "method",
                    "value",
                    setdiff(
                        names(dt),
                        c(
                            "plan_id",
                            "file_key",
                            "query_id",
                            "site_id",
                            "source_id",
                            "experiment_id",
                            "variant_label",
                            "frequency",
                            "table_id",
                            "variable_id",
                            "variable",
                            "grid_label",
                            "time",
                            "lon",
                            "lat",
                            "method",
                            "value"
                        )
                    )
                )
            )
            invisible(dt)
        },
        # }}}

        # decorate_extract_grid_sources {{{
        # Persists one static set of method-specific source grid cells per plan.
        decorate_extract_grid_sources = function(sources, plan, file) {
            if (is.null(sources)) {
                return(data.table::data.table())
            }
            sources <- data.table::as.data.table(data.table::copy(sources))
            if (!nrow(sources)) {
                return(sources)
            }
            now <- store__now()
            sources[, `:=`(
                plan_id = plan$plan_id[[1L]],
                file_key = plan$file_key[[1L]],
                query_id = plan$query_id[[1L]],
                variable_id = plan$variable_id[[1L]],
                method = plan$method[[1L]],
                created_at = now
            )]
            sources[, source_row_id := vapply(
                seq_len(.N),
                function(i) {
                    store__hash(
                        plan_id[[i]],
                        file_key[[i]],
                        variable_id[[i]],
                        method[[i]],
                        source_index[[i]]
                    )
                },
                character(1L)
            )]
            data.table::setcolorder(sources, c(
                "source_row_id",
                "plan_id",
                "file_key",
                "query_id",
                "variable_id",
                "method",
                "source_index",
                "role",
                "grid_lon",
                "grid_lat",
                "grid_dist_km",
                "weight",
                "created_at"
            ))
            sources[, c(
                "source_row_id",
                "plan_id",
                "file_key",
                "query_id",
                "variable_id",
                "method",
                "source_index",
                "role",
                "grid_lon",
                "grid_lat",
                "grid_dist_km",
                "weight",
                "created_at"
            ), with = FALSE]
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
            dirs <- paste0(names(parts), "=", vapply(parts, store__partition, character(1L)))
            file.path(
                private$extract_dir,
                do.call(file.path, as.list(dirs)),
                sprintf("part-%s.parquet", plan$plan_id[[1L]])
            )
        },
        # }}}

        # write_parquet {{{
        write_parquet = function(dt, path, overwrite = FALSE) {
            if (file.exists(path) && !isTRUE(overwrite)) {
                cli::cli_abort(
                    "Parquet output already exists without a complete reusable extraction manifest row: {.path {path}}.",
                    class = "epwshiftr_store_extract_conflict"
                )
            }
            dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
            tmp_file <- tempfile(tmpdir = dirname(path), fileext = ".parquet")
            tmp_table <- sprintf("tmp_extract_%s", substr(store__hash(path, Sys.time(), runif(1L)), 1L, 16L))
            on.exit(
                {
                    try(
                        ddb_exec(private$conn, sprintf("DROP TABLE IF EXISTS %s", ddb_ident(private$conn, tmp_table))),
                        silent = TRUE
                    )
                    if (file.exists(tmp_file)) {
                        unlink(tmp_file)
                    }
                },
                add = TRUE
            )

            ddb_write_table(private$conn, tmp_table, as.data.frame(dt), temporary = TRUE, overwrite = TRUE)
            ddb_exec(
                private$conn,
                sprintf(
                    "COPY %s TO %s (FORMAT PARQUET)",
                    ddb_ident(private$conn, tmp_table),
                    ddb_literal(private$conn, tmp_file)
                )
            )
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
                result_id = store__hash(plan$plan_id[[1L]], year, output_path),
                plan_id = plan$plan_id[[1L]],
                file_key = plan$file_key[[1L]],
                query_id = plan$query_id[[1L]],
                artifact_id = artifact_id,
                output_path = store_rel_path(output_path, private$store_path),
                year = as.integer(year),
                row_count = nrow(dt),
                unique_time_count = data.table::uniqueN(dt$time),
                time_min = min(dt$time),
                time_max = max(dt$time),
                lon_actual = dt$lon[[1L]],
                lat_actual = dt$lat[[1L]],
                dist_min = NA_real_,
                completed_at = store__now(),
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
        mark_plan_status = function(
            plan,
            status,
            available_time_count = NULL,
            last_error = NA_character_,
            increment_attempt = FALSE
        ) {
            plan <- as.data.frame(plan)
            plan$status <- status
            if (!is.null(available_time_count)) {
                plan$available_time_count <- as.integer(available_time_count)
            }
            if (isTRUE(increment_attempt)) {
                plan$attempt_count <- as.integer(plan$attempt_count) + 1L
            }
            plan$last_error <- store__chr1(last_error)
            plan$updated_at <- store__now()
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

# store helpers {{{
store__result_type <- function(files) {
    if (inherits(files, "EsgResultFile")) {
        return("File")
    }
    if (inherits(files, "EsgResultAggregation")) {
        return("Aggregation")
    }

    cli::cli_abort("`files` must be an EsgResultFile or EsgResultAggregation object.")
}

store__now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}

store__hash <- function(...) {
    text <- paste(vapply(list(...), store__hash_piece, character(1L)), collapse = "\n")
    checksum_bytes(charToRaw(text), "sha256")
}

store__hash_piece <- function(x) {
    if (is.null(x)) {
        return("<NULL>")
    }

    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    paste(x, collapse = "\r")
}

store__chr1 <- function(x) {
    if (is.null(x) || !length(x)) {
        return(NA_character_)
    }

    value <- x[[1L]]
    if (is.null(value) || !length(value) || is.na(value[[1L]])) {
        return(NA_character_)
    }

    as.character(value[[1L]])
}

store__chr <- function(x) {
    if (is.null(x)) {
        return(character())
    }
    x <- as.character(x)
    x[is.na(x)] <- NA_character_
    x
}

store__flag <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) {
        return(FALSE)
    }

    isTRUE(as.logical(x[[1L]]))
}

store__pluck <- function(x, name) {
    if (is.null(x) || !is.list(x) || !name %in% names(x)) {
        return(NA_character_)
    }

    x[[name]]
}

store__time1 <- function(x) {
    store__time(store__chr1(x))[[1L]]
}

store__time <- function(x) {
    if (inherits(x, "POSIXt")) {
        return(as.POSIXct(x, tz = "UTC"))
    }
    x <- as.character(x)
    out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) {
        out[ok] <- solrdate__parse(x[ok], tz = "UTC")
    }

    out
}

store__time_range <- function(time) {
    checkmate::assert_atomic_vector(time, len = 2L, any.missing = FALSE)
    parsed <- store__time(time)
    if (any(is.na(parsed))) {
        cli::cli_abort("`time` must be a length-2 parseable datetime range.")
    }
    if (parsed[[2L]] < parsed[[1L]]) {
        cli::cli_abort("The second `time` value must be greater than or equal to the first.")
    }

    list(start = parsed[[1L]], stop = parsed[[2L]])
}

store__col <- function(dt, name, n = nrow(dt)) {
    if (!name %in% names(dt)) {
        return(rep(NA_character_, n))
    }

    value <- dt[[name]]
    if (is.list(value)) {
        value <- vapply(value, store__chr1, character(1L))
    } else {
        value <- as.character(value)
        value[is.na(value)] <- NA_character_
    }

    value
}

store__cell <- function(dt, name, i) {
    if (!name %in% names(dt)) {
        return(NA_character_)
    }

    store__chr1(dt[[name]][[i]])
}

store__lgl <- function(x) {
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

store__version_rank <- function(x) {
    if (is.null(x)) {
        return(numeric())
    }
    value <- as.character(x)
    value[is.na(value)] <- NA_character_
    digits <- gsub("[^0-9]+", "", value)
    digits[!nzchar(digits)] <- NA_character_
    suppressWarnings(as.numeric(digits))
}

store__match_chr <- function(values, keys) {
    if (!length(values)) {
        return(rep(NA_character_, length(keys)))
    }
    out <- unname(values[as.character(keys)])
    out[is.na(out)] <- NA_character_
    as.character(out)
}

store__match_time <- function(values, keys, default) {
    if (!length(values)) {
        return(rep(default, length(keys)))
    }
    out <- unname(values[as.character(keys)])
    out[is.na(out)] <- default
    out
}

store__file_table <- function(files) {
    store__result_type(files)
    dt <- data.table::copy(files$to_data_table())
    n <- nrow(dt)
    columns <- c(
        "id",
        "dataset_id",
        "master_id",
        "instance_id",
        "title",
        "filename",
        "tracking_id",
        "version",
        "checksum",
        "checksum_type",
        "size",
        "latest",
        "replica",
        "retracted",
        "deprecated",
        "data_node",
        "activity_id",
        "institution_id",
        "source_id",
        "experiment_id",
        "variant_label",
        "frequency",
        "table_id",
        "variable_id",
        "grid_label",
        "datetime_start",
        "datetime_end",
        "url_opendap",
        "url_download"
    )

    out <- data.table::as.data.table(
        stats::setNames(rep(list(rep(NA_character_, n)), length(columns)), columns)
    )
    for (name in columns) {
        out[[name]] <- store__col(dt, name, n)
    }
    if (n && all(is.na(out$filename)) && any(!is.na(out$title))) {
        out$filename <- basename(out$title)
    }

    out[]
}

store__file_keys <- function(dt) {
    if (!nrow(dt)) {
        return(character())
    }

    vapply(
        seq_len(nrow(dt)),
        function(i) {
            master_id <- store__cell(dt, "master_id", i)
            if (!is.na(master_id) && nzchar(master_id)) {
                return(paste0("master:", master_id))
            }

            tracking_id <- store__cell(dt, "tracking_id", i)
            if (!is.na(tracking_id) && nzchar(tracking_id)) {
                return(paste0("tracking:", tracking_id))
            }

            checksum <- store__cell(dt, "checksum", i)
            size <- store__cell(dt, "size", i)
            filename <- store__cell(dt, "filename", i)
            if (!is.na(checksum) && nzchar(checksum) && !is.na(filename) && nzchar(filename)) {
                return(paste("checksum", checksum, size, filename, sep = ":"))
            }

            id <- store__cell(dt, "id", i)
            if (!is.na(id) && nzchar(id)) {
                return(paste0("id:", id))
            }

            pieces <- unlist(dt[i, c("url_opendap", "url_download", "title"), with = FALSE], use.names = FALSE)
            if (all(is.na(pieces) | !nzchar(pieces))) {
                cli::cli_abort(
                    "Cannot create a stable file key because file record {i} has no master ID, tracking ID, checksum, ID, URL, or title."
                )
            }
            paste0("fallback:", store__hash(pieces))
        },
        character(1L)
    )
}

store__partition <- function(x) {
    x <- store__chr1(x)
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

store__summary_cols <- function() {
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
