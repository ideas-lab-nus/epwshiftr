STORE_SCHEMA_VERSION <- "2.8.0"
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

# Canonicalize reset targets and reject broad directories whose replacement
# could damage a user profile, R installation, temporary session, or project.
# store__reset_path {{{
store__reset_path <- function(path) {
    checkmate::assert_string(path, min.chars = 1L)
    path <- store_normalize_path(path)
    protected <- unique(vapply(
        c(path.expand("~"), getwd(), R.home(), tempdir()),
        store_normalize_path,
        character(1L)
    ))
    prefix <- if (endsWith(path, "/")) path else paste0(path, "/")
    contains_protected <- vapply(
        protected,
        function(candidate) {
            identical(path, candidate) || startsWith(candidate, prefix)
        },
        logical(1L)
    )
    project_root <- dir.exists(file.path(path, ".git")) ||
        (file.exists(file.path(path, "DESCRIPTION")) &&
            dir.exists(file.path(path, "R")))

    if (
        identical(dirname(path), path) ||
            any(contains_protected) ||
            isTRUE(project_root)
    ) {
        cli::cli_abort(c(
            "Refusing to reset a protected or overly broad directory.",
            "x" = "Reset target: {.path {path}}",
            "i" = "Choose the dedicated epwshiftr store directory instead."
        ))
    }
    path
}
# }}}

# Inspect the manifest without opening EsgStore, because an older schema is
# intentionally rejected by the normal constructor before it can be queried.
# store__reset_inspect {{{
store__reset_inspect <- function(path) {
    if (!dir.exists(path)) {
        return(list(populated = FALSE, version = NA_character_))
    }
    entries <- list.files(path, all.files = TRUE, no.. = TRUE)
    if (!length(entries)) {
        return(list(populated = FALSE, version = NA_character_))
    }

    manifest <- file.path(path, "manifest.duckdb")
    if (!file.exists(manifest)) {
        cli::cli_abort(c(
            "Refusing to reset a non-empty directory that is not an epwshiftr store.",
            "x" = "No store manifest was found at {.path {manifest}}."
        ))
    }

    conn <- tryCatch(ddb_connect(manifest, read_only = TRUE), error = identity)
    if (inherits(conn, "condition")) {
        cli::cli_abort(
            c(
                "The epwshiftr store manifest could not be inspected.",
                "x" = conditionMessage(conn),
                "i" = "Close open EsgStore objects and stop active workers before resetting the store."
            ),
            parent = conn
        )
    }
    on.exit(
        try(ddb_disconnect(conn, shutdown = TRUE), silent = TRUE),
        add = TRUE
    )

    tables <- ddb_list_tables(conn)
    recognized <- "store_meta" %in%
        tables &&
        any(c("artifact", "esg_query", "file_catalog") %in% tables)
    if (!isTRUE(recognized)) {
        cli::cli_abort(c(
            "Refusing to reset a DuckDB database that is not an epwshiftr store.",
            "x" = "Manifest: {.path {manifest}}"
        ))
    }
    version <- NA_character_
    if ("store_meta" %in% tables) {
        meta <- ddb_read_table(conn, "store_meta")
        row <- meta[meta[["key"]] == "schema_version", , drop = FALSE]
        if (nrow(row)) {
            version <- as.character(row[["value"]][[1L]])
        }
    }
    list(populated = TRUE, version = version)
}
# }}}

# Choose a same-filesystem sibling for the backup so replacement is atomic and
# does not copy potentially large downloaded and extracted climate artifacts.
# store__reset_backup_path {{{
store__reset_backup_path <- function(path, version = NA_character_) {
    version <- if (is.na(version) || !nzchar(version)) {
        "unknown"
    } else {
        gsub("[^A-Za-z0-9._-]", "-", version)
    }
    stem <- sprintf(
        "%s-schema-%s-%s",
        path,
        version,
        format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")
    )
    candidate <- stem
    suffix <- 1L
    while (file.exists(candidate)) {
        candidate <- sprintf("%s-%d", stem, suffix)
        suffix <- suffix + 1L
    }
    candidate
}
# }}}

# Format a discoverable recovery command without forcing users to repeat the
# default platform-specific store path printed in an error message.
# store__reset_command {{{
store__reset_command <- function(path) {
    default <- tryCatch(
        store_normalize_path(store_dir(init = FALSE)),
        error = function(e) NA_character_
    )
    if (!is.na(default) && identical(store_normalize_path(path), default)) {
        return("store_reset()")
    }
    sprintf("store_reset(path = %s)", encodeString(path, quote = "\""))
}
# }}}

#' Recreate an epwshiftr store
#'
#' `store_reset()` creates a current-schema store at `path`. Existing stores are
#' moved to a timestamped sibling by default, preserving query snapshots,
#' downloads, extracted climate data, generated EPWs, and run history for
#' manual recovery. Permanent removal requires both `backup = FALSE` and
#' `force = TRUE`.
#'
#' This function does not migrate older manifests. Close open [EsgStore]
#' objects and stop active foreground or background jobs before resetting.
#'
#' @param path Store directory to recreate. Defaults to [store_dir()].
#' @param backup Whether to move an existing store to a timestamped sibling
#'   before creating the replacement. Default: `TRUE`.
#' @param force Required when `backup = FALSE` would permanently remove an
#'   existing populated store. Default: `FALSE`.
#'
#' @return Invisibly, a list containing the new `path`, optional `backup_path`,
#'   previous and current schema versions, and the performed `action`.
#'
#' @export
# store_reset {{{
store_reset <- function(
    path = store_dir(init = FALSE),
    backup = TRUE,
    force = FALSE
) {
    checkmate::assert_flag(backup)
    checkmate::assert_flag(force)
    path <- store__reset_path(path)
    state <- store__reset_inspect(path)

    if (isTRUE(state$populated) && !isTRUE(backup) && !isTRUE(force)) {
        cli::cli_abort(c(
            "Permanent store removal requires explicit confirmation.",
            "x" = "The existing store at {.path {path}} would be deleted.",
            "i" = "Use {.code backup = TRUE}, or set both {.code backup = FALSE} and {.code force = TRUE}."
        ))
    }

    backup_path <- NULL
    action <- "created"
    if (isTRUE(state$populated)) {
        manifest <- file.path(path, "manifest.duckdb")
        release <- manifest_acquire_lock(manifest, timeout = 1)
        lock_held <- TRUE
        on.exit(if (isTRUE(lock_held)) release(), add = TRUE)

        if (isTRUE(backup)) {
            backup_path <- store__reset_backup_path(path, state$version)
            moved <- file.rename(path, backup_path)
            if (!isTRUE(moved)) {
                cli::cli_abort(c(
                    "Failed to back up the existing epwshiftr store.",
                    "x" = "Could not move {.path {path}} to {.path {backup_path}}."
                ))
            }
            # The manifest lock moves with the directory. From this point the
            # original release closure must not target the replacement store.
            lock_held <- FALSE
            action <- "backed_up"
        } else {
            removed <- unlink(path, recursive = TRUE, force = TRUE) == 0L
            if (!isTRUE(removed) || dir.exists(path)) {
                cli::cli_abort(
                    "Failed to remove the existing store at {.path {path}}."
                )
            }
            lock_held <- FALSE
            action <- "removed"
        }
    }

    created <- tryCatch(EsgStore$new(path, create = TRUE), error = identity)
    if (inherits(created, "condition")) {
        restored <- FALSE
        if (!is.null(backup_path) && dir.exists(backup_path)) {
            if (dir.exists(path)) {
                unlink(path, recursive = TRUE, force = TRUE)
            }
            restored <- isTRUE(file.rename(backup_path, path))
            # A reset lock may have moved back with the restored directory.
            lock <- manifest_lock_path(file.path(path, "manifest.duckdb"))
            if (dir.exists(lock)) {
                unlink(lock, recursive = TRUE, force = TRUE)
            }
        }
        message <- c(
            "Failed to create the replacement epwshiftr store.",
            "x" = conditionMessage(created)
        )
        if (isTRUE(restored)) {
            message <- c(message, "i" = "The previous store was restored.")
        } else if (!is.null(backup_path) && dir.exists(backup_path)) {
            message <- c(
                message,
                "i" = "The previous store remains at {.path {backup_path}}."
            )
        }
        cli::cli_abort(message, parent = created)
    }
    created$close()

    # Remove the lock directory that moved into the preserved backup after the
    # replacement is known to be usable.
    if (!is.null(backup_path)) {
        backup_lock <- manifest_lock_path(file.path(
            backup_path,
            "manifest.duckdb"
        ))
        if (dir.exists(backup_lock)) {
            unlink(backup_lock, recursive = TRUE, force = TRUE)
        }
    }

    cli::cli_alert_success(
        "Created epwshiftr store schema {STORE_SCHEMA_VERSION} at {.path {path}}."
    )
    if (!is.null(backup_path)) {
        cli::cli_alert_info(
            "Previous store preserved at {.path {backup_path}}."
        )
    }
    invisible(list(
        path = path,
        backup_path = backup_path,
        previous_schema = state$version,
        schema = STORE_SCHEMA_VERSION,
        action = action
    ))
}
# }}}

# EsgStore
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
# EsgStore {{{
EsgStore <- R6::R6Class(
    "EsgStore",
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # initialize
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
        # initialize {{{
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
                    cli::cli_abort(
                        "Store directory does not exist: {.path {path}}."
                    )
                }
                dir.create(path, recursive = TRUE, showWarnings = FALSE)
            }
            if (!dir.exists(path)) {
                cli::cli_abort(
                    "Failed to create store directory: {.path {path}}."
                )
            }

            private$store_path <- normalizePath(
                path,
                mustWork = TRUE,
                winslash = "/"
            )
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

            private$manifest_path <- file.path(
                private$store_path,
                "manifest.duckdb"
            )
            private$connect()
            private$init_schema()

            self
        },
        # }}}
        # close
        #' @description
        #' Close the DuckDB connection.
        #'
        #' @return The store object itself, invisibly.
        # close {{{
        close = function() {
            private$disconnect()
            invisible(self)
        },
        # }}}
        # get_meta
        #' @description
        #' Return a store metadata value.
        #'
        #' @param key Metadata key.
        #' @param default Value returned when `key` is not set.
        #'
        #' @return A single string, or `default`.
        # get_meta {{{
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
        # set_meta
        #' @description
        #' Set a store metadata value.
        #'
        #' @param key Metadata key.
        #' @param value Metadata value. `NULL` is stored as `NA`.
        #'
        #' @return The store object, invisibly.
        # set_meta {{{
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
        # download_layout
        #' @description
        #' Return the store download layout policy.
        #'
        #' @return A named list describing how store downloads are placed under
        #'         `downloads/`.
        # download_layout {{{
        download_layout = function() {
            store_download__layout(self, private)
        },
        # }}}
        # set_download_layout
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
        # set_download_layout {{{
        set_download_layout = function(
            layout = c("flat", "dataset", "drs", "template"),
            template = NULL,
            include_version = TRUE,
            collision = c("error", "checksum", "suffix"),
            missing = c("fallback", "error")
        ) {
            store_download__set_layout(
                self,
                private,
                layout = layout,
                template = template,
                include_version = include_version,
                collision = collision,
                missing = missing
            )
        },
        # }}}
        # register_artifact
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
        # register_artifact {{{
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
            store_catalog__register_artifact(
                self,
                private,
                kind = kind,
                path = path,
                role = role,
                project = project,
                status = status,
                checksum = checksum,
                checksum_type = checksum_type,
                size = size,
                query_id = query_id,
                file_key = file_key,
                dict_id = dict_id,
                source_url = source_url,
                source_repo = source_repo,
                source_tag = source_tag,
                source_commit = source_commit,
                metadata = metadata
            )
        },
        # }}}
        # artifact_path
        #' @description
        #' Return an artifact path from the manifest.
        #'
        #' @param artifact_id Character vector of artifact IDs. Paths follow the
        #'        requested order, including repeated IDs. An empty vector
        #'        returns no paths. Unknown IDs are errors.
        #'
        #' @return Absolute artifact path.
        # artifact_path {{{
        artifact_path = function(artifact_id) {
            store_catalog__artifact_path(self, private, artifact_id)
        },
        # }}}
        # validate
        #' @description
        #' Validate registered artifact files against the manifest.
        #'
        #' @param artifact_id Optional character vector restricting validation
        #'        to these artifacts. Default `NULL` checks every artifact.
        #'        Repeated IDs are checked once, in first-requested order;
        #'        an empty vector checks no files. Unknown IDs are errors.
        #'
        #' @return A data.table with validation results.
        # validate {{{
        validate = function(artifact_id = NULL) {
            store_catalog__validate(self, private, artifact_id = artifact_id)
        },
        # }}}
        # add_query
        #' @description
        #' Add an ESGF query to the long-lived store query registry.
        #'
        #' @param query An [EsgQuery] object.
        #' @param label Optional label.
        #' @param track Whether to mark the query as tracked. Default: `FALSE`.
        #'
        #' @return The stable query ID.
        # add_query {{{
        add_query = function(query, label = NULL, track = FALSE) {
            store_catalog__add_query(
                self,
                private,
                query,
                label = label,
                track = track
            )
        },
        # }}}
        # track_query
        #' @description
        #' Mark a stored ESGF query as tracked.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #'
        #' @return The store object, invisibly.
        # track_query {{{
        track_query = function(query_id) {
            store_catalog__set_query_tracked(self, private, query_id, TRUE)
            invisible(self)
        },
        # }}}
        # untrack_query
        #' @description
        #' Mark a stored ESGF query as untracked.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #'
        #' @return The store object, invisibly.
        # untrack_query {{{
        untrack_query = function(query_id) {
            store_catalog__set_query_tracked(self, private, query_id, FALSE)
            invisible(self)
        },
        # }}}
        # tag_query
        #' @description
        #' Add tags to a stored ESGF query.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #' @param tag Character vector of tags.
        #' @param replace Whether to replace existing tags for the query.
        #'
        #' @return A data.table of tags for the query.
        # tag_query {{{
        tag_query = function(query_id, tag, replace = FALSE) {
            store_catalog__tag_query(
                self,
                private,
                query_id,
                tag,
                replace = replace
            )
        },
        # }}}
        # untag_query
        #' @description
        #' Remove tags from a stored ESGF query.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #' @param tag Optional tags. If `NULL`, all tags are removed.
        #'
        #' @return A data.table of remaining tags for the query.
        # untag_query {{{
        untag_query = function(query_id, tag = NULL) {
            store_catalog__untag_query(self, private, query_id, tag = tag)
        },
        # }}}
        # query_tags
        #' @description
        #' List stored ESGF query tags.
        #'
        #' @param query_id Optional query ID filter.
        #'
        #' @return A data.table of query tags.
        # query_tags {{{
        query_tags = function(query_id = NULL) {
            store_catalog__query_tags(self, private, query_id = query_id)
        },
        # }}}
        # require_query
        #' @description
        #' Record that one stored query depends on another stored query.
        #'
        #' @param query_id Child query ID.
        #' @param parent_query_id Required parent query ID.
        #'
        #' @return A data.table of query dependency edges.
        # require_query {{{
        require_query = function(query_id, parent_query_id) {
            store_catalog__require_query(
                self,
                private,
                query_id,
                parent_query_id
            )
        },
        # }}}
        # unrequire_query
        #' @description
        #' Remove query dependency edges.
        #'
        #' @param query_id Child query ID.
        #' @param parent_query_id Optional parent query ID. If `NULL`, all
        #'        parents for `query_id` are removed.
        #'
        #' @return A data.table of remaining dependency edges for the query.
        # unrequire_query {{{
        unrequire_query = function(query_id, parent_query_id = NULL) {
            store_catalog__unrequire_query(
                self,
                private,
                query_id,
                parent_query_id = parent_query_id
            )
        },
        # }}}
        # query_graph
        #' @description
        #' List stored query dependency edges.
        #'
        #' @param query_id Optional query ID anchor.
        #' @param direction Which edge direction to return for an anchor.
        #' @param recursive Whether to include transitive edges.
        #'
        #' @return A data.table of dependency edges.
        # query_graph {{{
        query_graph = function(
            query_id = NULL,
            direction = c("children", "parents", "both"),
            recursive = TRUE
        ) {
            store_catalog__query_graph(
                self,
                private,
                query_id = query_id,
                direction = direction,
                recursive = recursive
            )
        },
        # }}}
        # queries
        #' @description
        #' List stored ESGF queries.
        #'
        #' @param tracked Optional tracked-state filter.
        #'
        #' @return A data.table of stored query records.
        # queries {{{
        queries = function(tracked = NULL) {
            store_catalog__queries(self, private, tracked = tracked)
        },
        # }}}
        # query_files
        #' @description
        #' List files linked to a stored ESGF query.
        #'
        #' @param query_id Query ID returned by `$add_query()`.
        #' @param status Optional query-file status filter.
        #'
        #' @return A data.table of linked file records.
        # query_files {{{
        query_files = function(query_id, status = NULL) {
            store_catalog__query_files(
                self,
                private,
                query_id,
                status = status
            )
        },
        # }}}
        # preview_update_queries
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
        # preview_update_queries {{{
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
            store_catalog__preview_update_queries(
                self,
                private,
                query_id = query_id,
                tracked = tracked,
                tag = tag,
                children = children,
                detail = detail,
                all = all,
                limit = limit,
                fields = fields,
                ...
            )
        },
        # }}}
        # update_queries
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
        # update_queries {{{
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
            store_catalog__update_queries(
                self,
                private,
                query_id = query_id,
                tracked = tracked,
                tag = tag,
                children = children,
                enqueue = enqueue,
                downloader = downloader,
                replica = replica,
                session_label = session_label,
                service = service,
                probe = probe,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds,
                strategy = strategy,
                all = all,
                limit = limit,
                fields = fields,
                ...
            )
        },
        # }}}
        # download_preflight
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
        # download_preflight {{{
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
            store_download__preflight(
                self,
                private,
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
            )
        },
        # }}}
        # download_query
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
        # download_query {{{
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
            store_download__query(
                self,
                private,
                query_id = query_id,
                downloader = downloader,
                replica = replica,
                dry_run = dry_run,
                run = run,
                background = background,
                mode = mode,
                session_label = session_label,
                service = service,
                probe = probe,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds,
                strategy = strategy,
                progress = progress,
                overwrite = overwrite,
                resume = resume,
                all = all,
                limit = limit,
                fields = fields,
                ...
            )
        },
        # }}}
        # download_status
        #' @description
        #' Return downloader tasks linked to stored query files.
        #'
        #' @param query_id Optional stored query ID.
        #' @param session_id Optional downloader session ID.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A data.table of downloader task rows.
        # download_status {{{
        download_status = function(
            query_id = NULL,
            session_id = NULL,
            downloader = NULL
        ) {
            store_download__status(
                self,
                private,
                query_id = query_id,
                session_id = session_id,
                downloader = downloader
            )
        },
        # }}}
        # query_status
        #' @description
        #' Summarise tracked ESGF query file and download status.
        #'
        #' @param query_id Optional stored query ID vector. If `NULL`, all
        #'        stored ESGF queries are summarised.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A data.table with one row per stored query.
        # query_status {{{
        query_status = function(query_id = NULL, downloader = NULL) {
            store_catalog__query_status(
                self,
                private,
                query_id = query_id,
                downloader = downloader
            )
        },
        # }}}
        # query_updates
        #' @description
        #' List tracked query update runs.
        #'
        #' @param query_id Optional stored query ID filter.
        #' @param latest Whether to return only the latest update per query.
        #'
        #' @return A data.table of update run summaries.
        # query_updates {{{
        query_updates = function(query_id = NULL, latest = FALSE) {
            store_catalog__query_updates(
                self,
                private,
                query_id = query_id,
                latest = latest
            )
        },
        # }}}
        # query_changes
        #' @description
        #' List per-file changes recorded by tracked query updates.
        #'
        #' @param update_id Optional update run ID filter.
        #' @param query_id Optional stored query ID filter.
        #' @param change_type Optional change type filter.
        #'
        #' @return A data.table of per-file query update changes.
        # query_changes {{{
        query_changes = function(
            update_id = NULL,
            query_id = NULL,
            change_type = NULL
        ) {
            store_catalog__query_changes(
                self,
                private,
                update_id = update_id,
                query_id = query_id,
                change_type = change_type
            )
        },
        # }}}
        # workflow_status
        #' @description
        #' Summarise query, download, local, and extraction status together.
        #'
        #' @param query_id Optional stored query ID filter.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A data.table with one row per stored query.
        # workflow_status {{{
        workflow_status = function(query_id = NULL, downloader = NULL) {
            store_catalog__workflow_status(
                self,
                private,
                query_id = query_id,
                downloader = downloader
            )
        },
        # }}}
        # workflow_report
        #' @description
        #' Return a compact ESGF query workflow health report.
        #'
        #' @param query_id Optional stored query ID filter.
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A list with `summary`, `updates`, `changes`, `downloads`,
        #'         and `nodes`.
        # workflow_report {{{
        workflow_report = function(query_id = NULL, downloader = NULL) {
            store_catalog__workflow_report(
                self,
                private,
                query_id = query_id,
                downloader = downloader
            )
        },
        # }}}
        # remove_query
        #' @description
        #' Remove stored ESGF queries and optionally delete orphaned local files.
        #'
        #' @param query_id Stored query ID vector.
        #' @param delete Whether to leave local files untouched (`"none"`) or
        #'        delete files orphaned by the removal (`"orphaned"`).
        #'
        #' @return A data.table describing removed queries.
        # remove_query {{{
        remove_query = function(query_id, delete = c("none", "orphaned")) {
            store_catalog__remove_query(
                self,
                private,
                query_id = query_id,
                delete = delete
            )
        },
        # }}}
        # remove_files
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
        # remove_files {{{
        remove_files = function(file_key, delete_local = FALSE, force = FALSE) {
            store_catalog__remove_files(
                self,
                private,
                file_key = file_key,
                delete_local = delete_local,
                force = force
            )
        },
        # }}}
        # prune_orphans
        #' @description
        #' Report or remove file records no longer linked to any query.
        #'
        #' @param delete_local Whether to delete local NetCDF files and remove
        #'        orphaned registry records. Default: `FALSE`.
        #'
        #' @return A data.table of orphaned file records.
        # prune_orphans {{{
        prune_orphans = function(delete_local = FALSE) {
            store_catalog__prune_orphans(
                self,
                private,
                delete_local = delete_local
            )
        },
        # }}}
        # storage_report
        #' @description
        #' Summarise store download storage, registered local assets, temporary
        #' files, and cleanup candidates.
        #'
        #' @param detail Whether to return detailed file tables. Default:
        #'        `FALSE`.
        #'
        #' @return A summary data.table, or a list when `detail = TRUE`.
        # storage_report {{{
        storage_report = function(detail = FALSE) {
            store_download__storage_report(self, private, detail = detail)
        },
        # }}}
        # validate_files
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
        # validate_files {{{
        validate_files = function(
            query_id = NULL,
            checksum = FALSE,
            layout = TRUE
        ) {
            store_download__validate_files(
                self,
                private,
                query_id = query_id,
                checksum = checksum,
                layout = layout
            )
        },
        # }}}
        # repair_files
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
        # repair_files {{{
        repair_files = function(actions = NULL, dry_run = TRUE) {
            store_download__repair_files(
                self,
                private,
                actions = actions,
                dry_run = dry_run
            )
        },
        # }}}
        # cleanup_downloads
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
        # cleanup_downloads {{{
        cleanup_downloads = function(
            scope = c(
                "tmp",
                "orphan_records",
                "untracked_files",
                "missing_records"
            ),
            dry_run = TRUE,
            older_than = NULL
        ) {
            store_download__cleanup_downloads(
                self,
                private,
                scope = scope,
                dry_run = dry_run,
                older_than = older_than
            )
        },
        # }}}
        # retry_downloads
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
        # retry_downloads {{{
        retry_downloads = function(
            query_id = NULL,
            session_id = NULL,
            downloader = NULL,
            status = c("error", "cancelled"),
            run = TRUE,
            ...
        ) {
            store_download__retry_downloads(
                self,
                private,
                query_id = query_id,
                session_id = session_id,
                downloader = downloader,
                status = status,
                run = run,
                ...
            )
        },
        # }}}
        # add_files
        #' @description
        #' Add File or Aggregation query results to the local file catalog.
        #'
        #' @param files An `EsgResultFile` or `EsgResultAggregation` object.
        #' @param label Optional label for this query run.
        #'
        #' @return The created or updated query ID.
        # add_files {{{
        add_files = function(files, label = NULL) {
            store_catalog__add_files(self, private, files, label = label)
        },
        # }}}
        # downloader
        #' @description
        #' Return a Downloader bound to this store.
        #'
        #' @param ... Additional arguments passed to `Downloader$new()`.
        #'
        #' @return A [Downloader] object.
        # downloader {{{
        downloader = function(...) {
            store_download__downloader(self, private, ...)
        },
        # }}}
        # download_files
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
        # download_files {{{
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
            store_download__files(
                self,
                private,
                files = files,
                query_id = query_id,
                replica = replica,
                downloader = downloader,
                run = run,
                background = background,
                mode = mode,
                session_label = session_label,
                service = service,
                probe = probe,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds,
                strategy = strategy,
                progress = progress,
                overwrite = overwrite,
                resume = resume,
                ...
            )
        },
        # }}}
        # sync_downloads
        #' @description
        #' Register completed downloader tasks as local store artifacts.
        #'
        #' @param downloader Optional [Downloader]. Default: `$downloader()`.
        #'
        #' @return A data.table of completed tasks.
        # sync_downloads {{{
        sync_downloads = function(downloader = NULL) {
            store_download__sync(self, private, downloader = downloader)
        },
        # }}}
        # plan_region
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
        # plan_region {{{
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
            store_extract__plan_region(
                self,
                private,
                query_id = query_id,
                lon = lon,
                lat = lat,
                time = time,
                site_id = site_id,
                variable_id = variable_id,
                filters = filters,
                method = method
            )
        },
        # }}}
        # plan_regions
        #' @description
        #' Plan extraction for multiple sites with one catalogue lookup and
        #' one append of missing plans. Existing plans keep their status,
        #' attempt count, errors and timestamps.
        #'
        #' @param query_id Query ID returned by `$add_files()`.
        #' @param sites A data.frame or data.table with unique character
        #'   `site_id` values and finite numeric `lon` and `lat` columns.
        #'   Other columns are ignored. The caller's object is not modified.
        #'   An empty table returns zero plan rows without reading the catalogue.
        #' @param time Length-2 time range shared by all sites.
        #' @param variable_id Optional variable IDs. If `NULL`, all cataloged
        #'   variables in the query are used.
        #' @param filters Named list of exact-match file catalog filters.
        #' @param method One of `"nearest"`, `"idw"`, `"bilinear"`, or `"mean"`.
        #'
        #' @return A data.table in input site order. Within a site, existing
        #'   manifest order is retained, as for successive `plan_region()` calls.
        # plan_regions {{{
        plan_regions = function(
            query_id,
            sites,
            time,
            variable_id = NULL,
            filters = list(),
            method = "nearest"
        ) {
            store_extract__plan_regions(
                self,
                private,
                query_id = query_id,
                sites = sites,
                time = time,
                variable_id = variable_id,
                filters = filters,
                method = method
            )
        },
        # }}}
        # plan_sources
        #' @description
        #' Resolve the physical source identities used by explicit extraction
        #' plans, without opening a source or changing plan state. Independent
        #' plans can share a persistent store while callers use these keys to
        #' avoid filling an external queue with work for the same physical file.
        #'
        #' @param plan_id Unique extraction plan IDs. An empty character vector
        #'   returns a typed zero-row table. Unknown plans or missing catalog
        #'   records are errors.
        #'
        #' @return A data.table with `plan_id`, `file_key`, and `source_key`, in
        #'   requested plan order. `source_key` uses the same metadata identity
        #'   as the source reader; published replicas of one physical file can
        #'   therefore share a key despite different access URLs. The keys do
        #'   not certify provider availability or scientific acceptance.
        # plan_sources {{{
        plan_sources = function(plan_id) {
            store_extract__plan_sources(self, private, plan_id)
        },
        # }}}
        # extract
        #' @description
        #' Execute pending or failed regional extraction plans.
        #'
        #' Independent remote reads run concurrently, with at most
        #' `getOption("epwshiftr.mirai_workers", 4L)` source tasks per call.
        #' Set `options(epwshiftr.mirai_workers = 1L)` for serial reads. The same
        #' limit applies to shared batch acquisition; separate R sessions have
        #' separate limits. The calling process owns manifest and Parquet writes.
        #' With shared caching enabled, pending plans for one physical file
        #' reuse its connection. With caching disabled, workers return one plan
        #' at a time to bound memory. Complete ordinary extraction caches are
        #' consumed directly without starting workers. Speedup depends on the
        #' number of independent tasks and the remote service.
        #'
        #' Ordinary and pooled extraction share source ownership and recovery checks.
        #' Complete outputs and extraction caches require matching saved acquisition
        #' lineage and local integrity evidence, which can support offline reuse.
        #' Missing evidence, source conflicts, and changed bytes identify the affected
        #' result without replacing its original source receipt. After checking the
        #' requested source, use `overwrite = TRUE` to recompute rejected results.
        #' Download byte continuation is qualified separately from output `resume`;
        #' saved transfer settings and existing pool limits are preserved.
        #'
        #' @param plan_id Optional plan IDs to run.
        #' @param status Plan statuses to run when `plan_id` is `NULL`.
        #'        Default: `c("pending", "failed")`.
        #' @param fallback What to do when OPeNDAP open, metadata, or read
        #'        access fails. `"auto"` downloads through HTTPServer and
        #'        retries once when possible; `"error"` marks the plan failed
        #'        without downloading. Default: `"auto"`.
        #' @param overwrite If `TRUE`, overwrite existing Parquet outputs.
        #'        Default: `FALSE`.
        #' @param resume Whether to reuse complete existing extraction outputs.
        #'        Default: `TRUE`.
        #' @param reporter Optional workflow reporter used by task-level runs.
        #' @param pool Optional package-owned persistent source pool. With a
        #'        pool, return a pending extraction ticket and persist results
        #'        when the owning process polls the pool. Default synchronous
        #'        behavior is unchanged.
        #'
        #' @return A data.table of processed extraction plan rows. When `pool`
        #'   is supplied, a ticket with `status`, `error`, and `value` fields;
        #'   poll the pool until it resolves before closing this store. HTTP
        #'   fallback uses the same reader slot and a recoverable Downloader;
        #'   it does not block collection of independent source jobs.
        # extract {{{
        extract = function(
            plan_id = NULL,
            status = c("pending", "failed"),
            fallback = c("auto", "error"),
            overwrite = FALSE,
            resume = TRUE,
            reporter = NULL,
            pool = NULL
        ) {
            store_extract__run(
                self,
                private,
                plan_id = plan_id,
                status = status,
                fallback = fallback,
                overwrite = overwrite,
                resume = resume,
                reporter = reporter,
                pool = pool
            )
        },
        # }}}
        # export_extracts
        #' @description
        #' Export immutable metadata for completed extraction plans. Weather data
        #' remain in their existing Parquet files; the receipt contains no values.
        #' @param plan_id Unique completed extraction plan IDs to export.
        #' @param path New JSON receipt path. Existing receipts are never replaced.
        #' @return For `export_extracts()`, the receipt path, SHA256 and named
        #'   artifact paths. For `import_extracts()`, plan status and reuse flags.
        export_extracts = function(plan_id, path) {
            store_exchange__export(self, private, plan_id, path)
        },
        # import_extracts
        #' @description
        #' Atomically reuse completed plans from a checked extraction receipt.
        #' Original query, file, site, period and physical source identities must
        #' already exist locally. No download, file copy or overwrite occurs.
        #' Missing or altered artifacts reject the entire import. Annual scientific
        #' completeness remains the caller's responsibility.
        #' @param path Existing extraction receipt JSON path.
        #' @param checksum Expected SHA256 of the extraction receipt.
        #' @param artifact_paths Named character vector of locally accessible
        #'   Parquet paths inside this store root, keyed by the original artifact
        #'   IDs in the receipt. Independently written child stores can share a
        #'   synchronized parent root without sharing their live manifests.
        import_extracts = function(path, checksum, artifact_paths) {
            store_exchange__import(
                self,
                private,
                path,
                checksum,
                artifact_paths
            )
        },
        # export_plans
        #' @description
        #' Export exact pending or failed plan/catalogue metadata for an independent
        #' execution store. Original IDs, coordinates and retry history are retained.
        #' This metadata receipt does not authorize scientific use or ownership transfer.
        #' @param plan_id Unique pending or failed extraction plan IDs.
        #' @param path New plan receipt JSON path.
        #' @return For `export_plans()`, receipt path, SHA256 and named query JSON paths.
        #'   For `import_plans()`, original plan IDs and reuse flags.
        export_plans = function(plan_id, path) {
            store_exchange__export_plans(private, plan_id, path)
        },
        # import_plans
        #' @description
        #' Atomically install exact original plan metadata. Existing plans, retries
        #' and completed outputs remain unchanged. Only query JSON controls are
        #' copied; no native or extracted weather is copied or downloaded.
        #' @param path Existing plan receipt JSON path.
        #' @param checksum Expected SHA256 of the plan receipt.
        #' @param query_paths Named local paths to the original synchronized query
        #'   JSON controls, keyed by query ID. Each control is checked by SHA256.
        import_plans = function(path, checksum, query_paths) {
            store_exchange__import_plans(
                private,
                path,
                checksum,
                query_paths
            )
        },
        # query
        #' @description
        #' Run a DuckDB SQL query against the extraction manifest.
        #'
        #' @param sql SQL query.
        #'
        #' @return A data.table.
        # query {{{
        query = function(sql) {
            checkmate::assert_string(sql)
            private$check_open()

            data.table::as.data.table(ddb_query(private$conn, sql))
        },
        # }}}
        # summarise
        #' @description
        #' Summarise extracted Parquet outputs by manifest columns.
        #'
        #' @param by Character vector of grouping columns. Default groups by
        #'        source, experiment, variant, frequency, variable, site and
        #'        year.
        #'
        #' @return A data.table.
        # summarise {{{
        summarise = function(
            by = c(
                "source_id",
                "experiment_id",
                "variant_label",
                "frequency",
                "variable_id",
                "site_id",
                "year"
            )
        ) {
            store_extract__summarise(self, private, by = by)
        },
        # }}}
        # coverage
        #' @description
        #' Check extraction coverage for planned jobs.
        #'
        #' @param plan_id Optional plan IDs to check.
        #'
        #' @return A data.table with one row per plan.
        # coverage {{{
        coverage = function(plan_id = NULL) {
            store_extract__coverage(self, private, plan_id = plan_id)
        },
        # }}}
        # assert_complete
        #' @description
        #' Assert that selected extraction plans are complete.
        #'
        #' @param plan_id Optional plan IDs to check.
        #'
        #' @return The store object itself, invisibly.
        # assert_complete {{{
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
        # path
        #' @field path Store directory.
        # path {{{
        path = function() {
            private$store_path
        },
        # }}}
        # manifest
        #' @field manifest DuckDB manifest path.
        # manifest {{{
        manifest = function() {
            private$manifest_path
        },
        # }}}
        # is_open
        #' @field is_open Whether the manifest connection is open.
        # is_open {{{
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

        # Core DB, lock, transaction, and schema-init primitives
        # connect {{{
        connect = function() {
            private$conn <- ddb_connect(
                private$manifest_path,
                read_only = FALSE
            )
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
            store_catalog__init_schema(self, private)
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
            on.exit(
                {
                    private$lock_depth <- max(0L, private$lock_depth - 1L)
                },
                add = TRUE
            )
            manifest_with_lock(private$manifest_path, force(expr))
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
            # Ask DuckDB only about the keys being inserted. Reading the whole
            # key column back made every append cost O(table size), which grows
            # with long event logs. NA keys cannot match and stay in `rows`, so
            # the append still fails on them exactly as before. Chunk the lists
            # so a large batch never builds one enormous statement.
            ids <- rows[[key]]
            if (is.factor(ids)) {
                ids <- as.character(ids)
            }
            ids <- unique(ids[!is.na(ids)])
            current <- unlist(
                lapply(
                    split(ids, ceiling(seq_along(ids) / 2000L)),
                    function(chunk) {
                        ddb_query(
                            private$conn,
                            sprintf(
                                "SELECT %s FROM %s WHERE %s IN (%s)",
                                q_key,
                                q_table,
                                q_key,
                                paste(
                                    ddb_literal(private$conn, chunk),
                                    collapse = ", "
                                )
                            )
                        )[[key]]
                    }
                ),
                use.names = FALSE
            )
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
            q_values <- paste(
                ddb_literal(private$conn, values),
                collapse = ", "
            )
            ddb_exec(
                private$conn,
                sprintf(
                    "DELETE FROM %s WHERE %s IN (%s)",
                    q_table,
                    q_key,
                    q_values
                )
            )
            invisible(NULL)
        },
        # }}}

        # Private delegates retained for sibling modules and test hooks
        # update_query_files {{{
        update_query_files = function(
            query_id,
            files,
            fields = "*",
            all = TRUE,
            limit = FALSE
        ) {
            store_catalog__update_query_files(
                self,
                private,
                query_id = query_id,
                files = files,
                fields = fields,
                all = all,
                limit = limit
            )
        },
        # }}}
        # catalog_download_plan {{{
        catalog_download_plan = function(query_id = NULL) {
            store_download__catalog_plan(self, private, query_id = query_id)
        },
        # }}}
        # decorate_download_plan {{{
        decorate_download_plan = function(plan, file_rows) {
            store_download__decorate_plan(self, private, plan, file_rows)
        },
        # }}}
        # sync_tracked_file_download {{{
        sync_tracked_file_download = function(
            file_key,
            local_path,
            artifact_id
        ) {
            store_download__sync_tracked_file(
                self,
                private,
                file_key = file_key,
                local_path = local_path,
                artifact_id = artifact_id
            )
        },
        # }}}
        # extract_one {{{
        extract_one = function(
            plan,
            file,
            fallback = "auto",
            overwrite = FALSE,
            reporter = NULL,
            resolved = NULL
        ) {
            store_extract__one(
                self,
                private,
                plan = plan,
                file = file,
                fallback = fallback,
                overwrite = overwrite,
                reporter = reporter,
                resolved = resolved
            )
        },
        # }}}
        # persist_extract_payload {{{
        persist_extract_payload = function(
            payload,
            plan,
            file,
            opened,
            overwrite = FALSE
        ) {
            store_extract__persist_payload(
                self,
                private,
                payload = payload,
                plan = plan,
                file = file,
                opened = opened,
                overwrite = overwrite
            )
        },
        # }}}
        # publish_extract {{{
        publish_extract = function(
            plans,
            status,
            available_time_count,
            prepare,
            replace_results = FALSE
        ) {
            store_extract__publish(
                self,
                private,
                plans = plans,
                status = status,
                available_time_count = available_time_count,
                prepare = prepare,
                replace_results = replace_results
            )
        },
        # }}}
        # download_plan_file {{{
        download_plan_file = function(
            file,
            overwrite = FALSE,
            reporter = NULL
        ) {
            store_extract__download_plan_file(
                self,
                private,
                file = file,
                overwrite = overwrite,
                reporter = reporter
            )
        },
        # }}}
        # write_extract_partitions {{{
        write_extract_partitions = function(
            dt,
            plan,
            file,
            overwrite = FALSE,
            project = "CMIP6",
            source_lineage = NULL
        ) {
            store_extract__write_partitions(
                self,
                private,
                dt = dt,
                plan = plan,
                file = file,
                overwrite = overwrite,
                project = project,
                source_lineage = source_lineage
            )
        },
        # }}}
        # write_parquet {{{
        write_parquet = function(dt, path, overwrite = FALSE) {
            store_extract__write_parquet(
                self,
                private,
                dt = dt,
                path = path,
                overwrite = overwrite
            )
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

# Core shared store helpers

# store__result_type {{{
store__result_type <- function(files) {
    if (inherits(files, "EsgResultFile")) {
        return("File")
    }
    if (inherits(files, "EsgResultAggregation")) {
        return("Aggregation")
    }

    cli::cli_abort(
        "`files` must be an EsgResultFile or EsgResultAggregation object."
    )
}
# }}}

# store__now {{{
store__now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}
# }}}

# store__hash {{{
store__hash <- function(...) {
    text <- paste(
        vapply(list(...), store__hash_piece, character(1L)),
        collapse = "\n"
    )
    checksum_bytes(charToRaw(text), "sha256")
}
# }}}

# store__hash_piece {{{
store__hash_piece <- function(x) {
    if (is.null(x)) {
        return("<NULL>")
    }

    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    paste(x, collapse = "\r")
}
# }}}

# store__chr1 {{{
store__chr1 <- function(x) {
    if (!length(x)) {
        return(NA_character_)
    }

    value <- x[[1L]]
    if (!length(value) || is.na(value[[1L]])) {
        return(NA_character_)
    }

    as.character(value[[1L]])
}
# }}}

# store__chr {{{
store__chr <- function(x) {
    if (is.null(x)) {
        return(character())
    }
    x <- as.character(x)
    x[is.na(x)] <- NA_character_
    x
}
# }}}

# store__flag {{{
store__flag <- function(x) {
    if (!length(x) || is.na(x[[1L]])) {
        return(FALSE)
    }

    isTRUE(as.logical(x[[1L]]))
}
# }}}

# store__pluck {{{
store__pluck <- function(x, name) {
    if (is.null(x) || !is.list(x) || !name %in% names(x)) {
        return(NA_character_)
    }

    x[[name]]
}
# }}}

# store__time1 {{{
store__time1 <- function(x) {
    store__time(store__chr1(x))[[1L]]
}
# }}}

# store__time {{{
store__time <- function(x) {
    if (inherits(x, "POSIXt")) {
        return(as.POSIXct(x, tz = "UTC"))
    }
    x <- as.character(x)
    out <- as.POSIXct(
        rep(NA_real_, length(x)),
        origin = "1970-01-01",
        tz = "UTC"
    )
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) {
        out[ok] <- solrdate__parse(x[ok], tz = "UTC")
    }

    out
}
# }}}

# store__time_range {{{
store__time_range <- function(time) {
    checkmate::assert_atomic_vector(time, len = 2L, any.missing = FALSE)
    parsed <- store__time(time)
    if (anyNA(parsed)) {
        cli::cli_abort("`time` must be a length-2 parseable datetime range.")
    }
    if (parsed[[2L]] < parsed[[1L]]) {
        cli::cli_abort(
            "The second `time` value must be greater than or equal to the first."
        )
    }

    list(start = parsed[[1L]], stop = parsed[[2L]])
}
# }}}

# store__col {{{
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
# }}}

# store__cell {{{
store__cell <- function(dt, name, i) {
    if (!name %in% names(dt)) {
        return(NA_character_)
    }

    store__chr1(dt[[name]][[i]])
}
# }}}

# store__lgl {{{
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
# }}}

# store__version_rank {{{
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
# }}}

# store__match_chr {{{
store__match_chr <- function(values, keys) {
    if (!length(values)) {
        return(rep(NA_character_, length(keys)))
    }
    out <- unname(values[as.character(keys)])
    out[is.na(out)] <- NA_character_
    as.character(out)
}
# }}}

# store__match_time {{{
store__match_time <- function(values, keys, default) {
    if (!length(values)) {
        return(rep(default, length(keys)))
    }
    out <- unname(values[as.character(keys)])
    out[is.na(out)] <- default
    out
}
# }}}

# store__file_table {{{
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
        stats::setNames(
            rep(list(rep(NA_character_, n)), length(columns)),
            columns
        )
    )
    for (name in columns) {
        data.table::set(out, j = name, value = store__col(dt, name, n))
    }
    if (n && all(is.na(out$filename)) && any(!is.na(out$title))) {
        data.table::set(out, j = "filename", value = basename(out$title))
    }

    out[]
}
# }}}

# Preserve file-scoped provider identities, qualifying dataset-scoped master IDs
# with the filename so separate native time slices cannot share one store key.
# store__file_keys {{{
store__file_keys <- function(dt) {
    if (!nrow(dt)) {
        return(character())
    }

    vapply(
        seq_len(nrow(dt)),
        function(i) {
            master_id <- store__cell(dt, "master_id", i)
            dataset_id <- store__cell(dt, "dataset_id", i)
            dataset_master <- sub(
                "\\.v[0-9]+$",
                "",
                sub("\\|.*$", "", dataset_id)
            )
            if (
                !is.na(master_id) &&
                    nzchar(master_id) &&
                    identical(master_id, dataset_master)
            ) {
                # Some ESGF bridges return the dataset's master_id on every
                # File record. Keep the raw metadata, but restore file scope
                # for storage. Without a filename, use the existing fallbacks.
                filename <- store__cell(dt, "filename", i)
                if (is.na(filename) || !nzchar(filename)) {
                    filename <- store__cell(dt, "title", i)
                }
                master_id <- if (!is.na(filename) && nzchar(filename)) {
                    paste0(master_id, ".", filename)
                } else {
                    NA_character_
                }
            }
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
            if (
                !is.na(checksum) &&
                    nzchar(checksum) &&
                    !is.na(filename) &&
                    nzchar(filename)
            ) {
                return(paste("checksum", checksum, size, filename, sep = ":"))
            }

            id <- store__cell(dt, "id", i)
            if (is.na(id) || !nzchar(id)) {
                id <- store__cell(dt, "esgf_id", i)
            }
            if (!is.na(id) && nzchar(id)) {
                return(paste0("id:", id))
            }

            file_key <- store__cell(dt, "file_key", i)
            if (!is.na(file_key) && nzchar(file_key)) {
                return(paste0("file-key:", file_key))
            }

            pieces <- c(
                store__cell(dt, "url_opendap", i),
                store__cell(dt, "url_download", i),
                store__cell(dt, "title", i)
            )
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
# }}}

# Identify a standard CMIP6 DRS filename. Unlike node-specific ESGF record IDs,
# the filename carries variable, table, model, experiment, member, grid, and
# time-slice identity and is therefore stable across replicas.
# store__drs_file_name {{{
store__drs_file_name <- function(value) {
    value <- basename(as.character(value))
    valid <- !is.na(value) &
        nzchar(value) &
        grepl(
            "^[^_]+_[^_]+_[^_]+_[^_]+_r[0-9]+i[0-9]+p[0-9]+f[0-9]+_[^_]+_.+\\.nc$",
            value
        )
    value[!valid] <- NA_character_
    value
}
# }}}

# Build one logical file identity shared by catalog normalization and download
# planning. DRS identity takes precedence for CMIP6 files; generic ESGF results
# retain the established master/tracking/checksum fallback hierarchy.
# store__logical_file_id {{{
store__logical_file_id <- function(catalog) {
    catalog <- data.table::as.data.table(catalog)
    if (!nrow(catalog)) {
        return(character())
    }
    filename <- if ("filename" %in% names(catalog)) {
        catalog$filename
    } else if ("title" %in% names(catalog)) {
        catalog$title
    } else {
        rep(NA_character_, nrow(catalog))
    }
    drs <- store__drs_file_name(filename)
    use_drs <- !is.na(drs) & nzchar(drs)
    identity <- rep(NA_character_, nrow(catalog))
    identity[use_drs] <- paste0("drs:", drs[use_drs])
    if (any(!use_drs)) {
        identity[!use_drs] <- store__file_keys(catalog[!use_drs])
    }
    identity
}
# }}}

# store__empty_table {{{
store__empty_table <- function(spec) {
    proto <- list(c = character(), l = logical(), n = numeric())
    parts <- strsplit(
        trimws(strsplit(spec, ",", fixed = TRUE)[[1L]]),
        ":",
        fixed = TRUE
    )
    data.table::as.data.table(stats::setNames(
        lapply(parts, function(p) proto[[p[[2L]]]]),
        vapply(parts, `[[`, character(1L), 1L)
    ))
}
# }}}

# vim: fdm=marker :
