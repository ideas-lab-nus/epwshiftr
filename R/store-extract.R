# OPeNDAP/local extraction orchestration, shared extraction cache, multi-site
# region planning, Parquet persistence, lineage/reconciliation, and coverage
# helpers for EsgStore.

# Wrap one access failure with the exact service, phase, target, duration, and
# original condition needed by workflow diagnostics and final plan errors.
# store__access_error {{{
store__access_error <- function(
    error,
    phase,
    service,
    target,
    started_at,
    source_url = NULL
) {
    elapsed <- proc.time()[["elapsed"]] - started_at
    message <- sprintf(
        "%s %s failed for %s after %.3f seconds: %s",
        service,
        phase,
        target,
        elapsed,
        conditionMessage(error)
    )
    # Replica selection can make the byte producer differ from the catalogue
    # endpoint. Keep that actual URL alongside the unchanged local read target.
    source_url <- store__chr1(source_url)
    if (!is.na(source_url)) {
        message <- paste(message, sprintf("HTTP producer: %s", source_url))
    }
    structure(
        list(
            message = message,
            call = NULL,
            parent = error,
            source_url = source_url,
            phase = phase,
            service = service,
            target = target,
            elapsed_seconds = elapsed,
            error_class = class(error)[[1L]],
            error_message = conditionMessage(error)
        ),
        class = c("epwshiftr_store_access_error", "error", "condition")
    )
}
# }}}

# Preserve both remote and local failures when an automatic HTTP retry cannot
# recover an OPeNDAP extraction attempt.
# store__fallback_error {{{
store__fallback_error <- function(remote, local) {
    structure(
        list(
            message = paste(
                "OPeNDAP extraction and HTTP fallback both failed.",
                sprintf("Remote: %s", conditionMessage(remote)),
                sprintf("Fallback: %s", conditionMessage(local))
            ),
            call = NULL,
            parent = local,
            remote_error = remote,
            fallback_error = local
        ),
        class = c(
            "epwshiftr_store_fallback_error",
            "epwshiftr_store_access_error",
            "error",
            "condition"
        )
    )
}
# }}}

# Record one structured access failure through the existing workflow reporter;
# standalone store calls retain the same information in their condition text.
# store__report_access_failure {{{
store__report_access_failure <- function(
    reporter,
    file,
    error,
    attempt,
    outcome = "fallback"
) {
    if (is.null(reporter)) {
        return(invisible(NULL))
    }
    reporter$notice(
        sprintf(
            "%s %s failed for %s",
            error$service,
            error$phase,
            file$filename[[1L]]
        ),
        outcome = outcome,
        details = list(
            unit_type = "extraction_plan",
            variable = file$variable_id[[1L]],
            access_method = error$service,
            access_phase = error$phase,
            target = error$target,
            data_node = query_result__url_host(error$target),
            elapsed_seconds = error$elapsed_seconds,
            error_class = error$error_class,
            error = error$error_message,
            attempt = as.integer(attempt)
        )
    )
    reporter$detail(
        sprintf("  %s", conditionMessage(error)),
        level = "detail"
    )
    invisible(NULL)
}
# }}}

# Build a content-addressed path for one undecorated point-extraction payload.
# Store-local plan IDs are excluded deliberately so method children requesting
# the same immutable file, location, time range, and spatial method can share
# the expensive NetCDF read while retaining independent manifests.
# store__extract_cache_path {{{
store__extract_cache_path <- function(plan, file) {
    file <- data.table::as.data.table(file)
    # Cache ownership follows the same descriptor as reader exclusion and HTTP
    # recovery. A version label without publisher proof cannot merge endpoints.
    source <- source_recovery__key(source_recovery__describe(file))
    key <- store__hash(
        "site-extraction-v2",
        source,
        list(
            variable_id = plan$variable_id[[1L]],
            lon = as.numeric(plan$lon[[1L]]),
            lat = as.numeric(plan$lat[[1L]]),
            method = plan$method[[1L]],
            time_start = as.numeric(plan$time_start[[1L]]),
            time_stop = as.numeric(plan$time_stop[[1L]])
        )
    )
    file.path(
        cache__option("dir_cache", cache__default_dir()),
        "site-extractions",
        substr(key, 1L, 2L),
        paste0(key, ".rds")
    )
}
# }}}

# Read a shared extraction only when its complete method-neutral payload is
# present. Invalid or interrupted cache files are treated as misses and safely
# replaced while the content-addressed lock is held.
# store__extract_cache_read {{{
store__extract_cache_read <- function(path) {
    if (!file.exists(path)) {
        return(NULL)
    }
    receipt <- paste0(path, ".json")
    if (file.exists(receipt) && !store__extract_cache_available(path)) {
        return(NULL)
    }
    payload <- tryCatch(readRDS(path), error = identity)
    required <- c(
        "data",
        "grid_sources",
        "available_time_count",
        "actual_start",
        "actual_end"
    )
    if (
        inherits(payload, "error") ||
            !is.list(payload) ||
            !all(required %in% names(payload)) ||
            !is.data.frame(payload$data)
    ) {
        return(NULL)
    }
    payload
}
# }}}

# Verify a cache receipt without materializing all native values. Existing
# payloads without a receipt still undergo their full structural check.
# store__extract_cache_available {{{
store__extract_cache_available <- function(path) {
    if (!file.exists(path)) {
        return(FALSE)
    }
    receipt <- paste0(path, ".json")
    if (!file.exists(receipt)) {
        return(!is.null(store__extract_cache_read(path)))
    }
    record <- tryCatch(
        jsonlite::read_json(receipt, simplifyVector = TRUE),
        error = function(e) NULL
    )
    is.list(record) && identical(record[["sha256"]], checksum_file(path))
}
# }}}

# Atomically publish an undecorated extraction payload. Plan/query identities
# are attached only after materialization in the requesting store.
# store__extract_cache_write {{{
store__extract_cache_write <- function(path, payload) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    # Keep staging on the same volume without adding to a long content address.
    temporary <- tempfile(pattern = ".epw-", tmpdir = dirname(path))
    on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
    saveRDS(payload, temporary, version = 3L, compress = FALSE)
    if (file.exists(path)) {
        unlink(path)
    }
    if (!file.rename(temporary, path)) {
        cli::cli_abort("Could not publish the shared extraction cache entry.")
    }
    store_write_json_atomic(
        list(
            sha256 = checksum_file(path),
            source_lineage = payload$source_lineage
        ),
        paste0(path, ".json"),
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        digits = NA
    )
    invisible(path)
}
# }}}

# Resolve one shared extraction under a cross-process lock. The generator owns
# all remote/local fallback behavior; this wrapper stores only successful,
# fully-read payloads and identifies reuse without copying store-specific IDs.
# store__extract_cache_resolve {{{
store__extract_cache_resolve <- function(
    plan,
    file,
    generate,
    overwrite = FALSE
) {
    checkmate::assert_function(generate)
    if (identical(cache__mode(), "off")) {
        value <- source_recovery__attach(generate(), file)
        value$cache_reused <- FALSE
        return(value)
    }
    path <- store__extract_cache_path(plan, file)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    manifest_with_lock(
        path,
        {
            payload <- source_recovery__cache_read(path, file, overwrite)
            if (!is.null(payload)) {
                return(list(
                    payload = payload,
                    opened = list(
                        access_method = "shared_cache",
                        target = path
                    ),
                    recovery_error = NULL,
                    cache_reused = TRUE
                ))
            }
            if (identical(cache__mode(), "offline")) {
                cli::cli_abort(
                    "Shared extraction cache is missing in offline mode."
                )
            }
            value <- source_recovery__attach(generate(), file)
            store__extract_cache_write(path, value$payload)
            value$cache_reused <- FALSE
            value
        },
        timeout = 86400
    )
}
# }}}

# Keep the readable partition layout where supported. Windows paths that exceed
# the conservative file-creation limit use the immutable plan identity instead;
# source/site/year columns and the manifest preserve the same scientific keys.
# store__extract_output_path {{{
store__extract_output_path <- function(
    extract_dir,
    partitions,
    plan_id,
    year,
    windows = identical(.Platform$OS.type, "windows")
) {
    path <- file.path(
        extract_dir,
        do.call(file.path, as.list(partitions)),
        sprintf("part-%s.parquet", plan_id)
    )
    if (isTRUE(windows) && nchar(path, type = "chars") > 240L) {
        # Plans include their file, site, method and time identity in their hash.
        # The year also keeps multi-year partitions of one plan distinct.
        path <- file.path(
            extract_dir,
            "by-plan",
            sprintf("part-%s-%d.parquet", plan_id, year)
        )
    }
    path
}
# }}}

# store_extract__partition {{{
store_extract__partition <- function(x) {
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
# }}}

# store_extract__summary_cols {{{
store_extract__summary_cols <- function() {
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

# Resolve a bounded requested plan set against its normalized catalog rows.
# The source reader's key function is the single authority for physical identity;
# neither access URL nor logical model/field grouping becomes a new lock key.
# store_extract__plan_source_rows {{{
store_extract__plan_source_rows <- function(conn, plan_id) {
    if (!length(plan_id)) {
        return(data.table::data.table(
            plan_id = character(),
            file_key = character(),
            source_key = character()
        ))
    }
    plans <- ddb_query(
        conn,
        paste0(
            "SELECT plan_id,file_key FROM extraction_plan WHERE plan_id IN (",
            paste(ddb_literal(conn, plan_id), collapse = ","),
            ")"
        )
    )
    missing <- setdiff(plan_id, plans$plan_id)
    if (length(missing)) {
        cli::cli_abort(
            "Extraction plan ID(s) {.val {missing}} were not found in the store manifest."
        )
    }
    plans <- plans[match(plan_id, plans$plan_id), , drop = FALSE]
    file_keys <- unique(plans$file_key)
    catalog <- data.table::as.data.table(ddb_query(
        conn,
        paste0(
            "SELECT * FROM file_catalog WHERE file_key IN (",
            paste(ddb_literal(conn, file_keys), collapse = ","),
            ")"
        )
    ))
    if (
        anyNA(file_keys) ||
            anyDuplicated(catalog$file_key) ||
            !all(file_keys %in% catalog$file_key)
    ) {
        cli::cli_abort(
            "An extraction plan has no unambiguous file catalog record."
        )
    }
    # Multiple sites and years can reference one file. Hash each distinct
    # selected catalog record once, then restore the caller's exact plan order.
    physical_keys <- vapply(
        seq_len(nrow(catalog)),
        function(i) source__physical_key(catalog[i]),
        character(1L)
    )
    data.table::data.table(
        plan_id = plans$plan_id,
        file_key = plans$file_key,
        source_key = physical_keys[match(plans$file_key, catalog$file_key)]
    )
}
# }}}

# Construct the same scalar plan identities for every site/file pair. The
# per-row hash is intentional: IDs are part of the persisted public protocol.
# Preallocated site batches avoid repeated table growth and retain site order.
# store_extract__region_plan_rows {{{
store_extract__region_plan_rows <- function(
    plan,
    query_id,
    sites,
    time_range,
    method
) {
    now <- store__now()
    rows <- lapply(seq_len(nrow(sites)), function(site) {
        site_id <- sites$site_id[[site]]
        lon <- sites$lon[[site]]
        lat <- sites$lat[[site]]
        data.frame(
            plan_id = vapply(
                seq_len(nrow(plan)),
                function(i) {
                    store__hash(
                        query_id,
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
    })
    unique(data.table::rbindlist(rows, use.names = TRUE))
}
# }}}

# store_extract__plan_region {{{
store_extract__plan_region <- function(
    self,
    private,
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
    checkmate::assert_character(
        variable_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_list(filters, names = "unique")
    method <- match.arg(method, ESG_GRID_METHOD_CHOICES)
    private$check_open()

    store_extract__plan_regions_from_sites(
        private,
        query_id,
        data.frame(site_id = site_id, lon = lon, lat = lat),
        time,
        variable_id,
        filters,
        method
    )
}
# }}}

# store_extract__plan_regions {{{
store_extract__plan_regions <- function(
    self,
    private,
    query_id,
    sites,
    time,
    variable_id = NULL,
    filters = list(),
    method = "nearest"
) {
    checkmate::assert_string(query_id)
    checkmate::assert_data_frame(sites)
    required <- c("site_id", "lon", "lat")
    if (anyDuplicated(names(sites)) || !all(required %in% names(sites))) {
        cli::cli_abort(
            "`sites` must have unambiguous site_id, lon and lat columns."
        )
    }
    checkmate::assert_character(
        sites$site_id,
        min.chars = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    checkmate::assert_numeric(
        sites$lon,
        lower = -180,
        upper = 360,
        any.missing = FALSE,
        finite = TRUE
    )
    checkmate::assert_numeric(
        sites$lat,
        lower = -90,
        upper = 90,
        any.missing = FALSE,
        finite = TRUE
    )
    checkmate::assert_character(
        variable_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_list(filters, names = "unique")
    method <- match.arg(method, ESG_GRID_METHOD_CHOICES)
    private$check_open()
    store_extract__plan_regions_from_sites(
        private,
        query_id,
        sites,
        time,
        variable_id,
        filters,
        method
    )
}
# }}}

# Share catalogue selection across a batch and persist only absent IDs.
# Validations happen before this method, so a bad site cannot partially
# publish a multi-site batch. Empty work returns the native SQL schema.
# store_extract__plan_regions_from_sites {{{
store_extract__plan_regions_from_sites <- function(
    private,
    query_id,
    sites,
    time,
    variable_id,
    filters,
    method
) {
    time_range <- store__time_range(time)
    if (!nrow(sites)) {
        return(data.table::as.data.table(ddb_query(
            private$conn,
            "SELECT * FROM extraction_plan WHERE FALSE"
        )))
    }
    # file_catalog stores one shared row per file; its query_id is
    # only the latest writer. Use the many-to-many membership table
    # and filter in DuckDB before materializing this query's catalog.
    catalog <- data.table::as.data.table(ddb_query(
        private$conn,
        sprintf(
            paste(
                "SELECT catalog.* FROM file_catalog AS catalog",
                "INNER JOIN esg_query_file AS membership",
                "ON catalog.file_key = membership.file_key",
                "WHERE membership.query_id = %s",
                "AND membership.status = 'current'"
            ),
            ddb_literal(private$conn, query_id)
        )
    ))
    if (!nrow(catalog)) {
        cli::cli_abort(
            "No cataloged file records were found for query ID {.val {query_id}}."
        )
    }

    data.table::set(catalog, j = "query_id", value = query_id)

    filter_names <- names(filters)
    if (length(filters)) {
        if (is.null(filter_names) || any(!nzchar(filter_names))) {
            cli::cli_abort("All file catalog filters must be named.")
        }
        unknown <- setdiff(filter_names, names(catalog))
        if (length(unknown)) {
            cli::cli_abort(
                "Unknown file catalog filter column(s): {.val {unknown}}."
            )
        }

        for (name in filter_names) {
            value <- filters[[name]]
            checkmate::assert_atomic_vector(
                value,
                any.missing = FALSE,
                min.len = 1L,
                .var.name = sprintf("filters$%s", name)
            )
            catalog <- catalog[
                as.character(catalog[[name]]) %in% as.character(value)
            ]
        }
    }

    if (!nrow(catalog)) {
        cli::cli_abort(
            "No cataloged file records match the requested extraction plan filters."
        )
    }

    requested_variable_id <- variable_id
    if (!is.null(requested_variable_id)) {
        if (any(!is.na(catalog$variable_id))) {
            catalog <- catalog[
                catalog$variable_id %in% requested_variable_id
            ]
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
        plan <- plan[rep(
            seq_len(nrow(plan)),
            each = length(requested_variable_id)
        )]
        data.table::set(
            plan,
            j = "variable_id",
            value = rep(requested_variable_id, times = nrow(catalog))
        )
    } else {
        plan <- plan[plan$variable_id %in% requested_variable_id]
    }
    if (!nrow(plan)) {
        cli::cli_abort(
            "No cataloged file records match the requested variable IDs."
        )
    }

    out <- store_extract__region_plan_rows(
        plan,
        query_id,
        sites,
        time_range,
        method
    )
    private$with_store_lock({
        # Restrict both lookups to this batch. Historical unrelated
        # plans must not be materialized once per requested site.
        ids <- paste(
            ddb_literal(private$conn, out$plan_id),
            collapse = ", "
        )
        sql <- paste0(
            "SELECT * FROM extraction_plan WHERE plan_id IN (",
            ids,
            ")"
        )
        existing <- data.table::as.data.table(ddb_query(private$conn, sql))
        fresh <- out[
            !out$plan_id %in% existing$plan_id,
            intersect(names(existing), names(out)),
            with = FALSE
        ]
        if (nrow(fresh)) {
            ddb_append_table(
                private$conn,
                "extraction_plan",
                as.data.frame(fresh)
            )
            existing <- data.table::as.data.table(ddb_query(private$conn, sql))
        }
        existing[
            order(match(existing$site_id, sites$site_id)),
            intersect(names(out), names(existing)),
            with = FALSE
        ]
    })
}
# }}}

# store_extract__plan_sources {{{
store_extract__plan_sources <- function(self, private, plan_id) {
    checkmate::assert_character(
        plan_id,
        any.missing = FALSE,
        min.chars = 1L,
        unique = TRUE
    )
    private$check_open()
    store_extract__plan_source_rows(private$conn, plan_id)
}
# }}}

# store_extract__run {{{
store_extract__run <- function(
    self,
    private,
    plan_id = NULL,
    status = c("pending", "failed"),
    fallback = c("auto", "error"),
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL,
    pool = NULL
) {
    checkmate::assert_character(
        plan_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_subset(
        status,
        c("pending", "failed", "empty", "done")
    )
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)
    fallback <- match.arg(fallback)
    if (!is.null(pool)) {
        stopifnot(inherits(pool, "epwshiftr_source_pool"))
    }
    private$check_open()

    plans <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "extraction_plan"
    ))
    if (is.null(plan_id)) {
        plans <- plans[plans$status %in% status]
    } else {
        # Keep the requested IDs outside data.table evaluation. Using
        # `plan_id` on both sides would resolve both names as the table
        # column and accidentally execute every plan in the store.
        requested_plan_id <- plan_id
        plans <- plans[plans$plan_id %in% requested_plan_id]
    }
    if (!nrow(plans)) {
        return(plans)
    }

    catalog <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "file_catalog"
    ))
    processed <- vector("list", nrow(plans))
    resumed_outputs <- vector("list", nrow(plans))
    file_rows <- match(plans$file_key, catalog$file_key)
    # Persist and report each result only in the manifest-owning process.
    finished <- 0L
    process <- function(i, resolved = NULL) {
        # Source files can finish out of order; UI counts follow actual
        # processing, while result rows retain their original indices.
        finished <<- finished + 1L
        plan <- plans[i]
        file <- catalog[
            if (is.na(file_rows[[i]])) integer() else file_rows[[i]]
        ]
        scenario <- if (nrow(file)) {
            store__chr1(file$experiment_id[[1L]])
        } else {
            "unknown scenario"
        }
        if (!is.null(reporter)) {
            reporter$check_cancel()
            label <- sprintf(
                "%s \u00b7 %s \u00b7 %s to %s",
                scenario,
                plan$variable_id[[1L]],
                format(plan$time_start[[1L]], "%Y-%m-%d"),
                format(plan$time_stop[[1L]], "%Y-%m-%d")
            )
            reporter$unit_started(
                label,
                current = finished,
                total = nrow(plans),
                details = list(
                    unit_type = "extraction_plan",
                    scenario = scenario,
                    variable = plan$variable_id[[1L]],
                    period = sprintf(
                        "%s/%s",
                        format(plan$time_start[[1L]], "%Y"),
                        format(plan$time_stop[[1L]], "%Y")
                    ),
                    access_method = "OPeNDAP"
                )
            )
            if (nrow(file)) {
                source_location <- if (
                    !is.na(file$local_path[[1L]]) &&
                        nzchar(file$local_path[[1L]])
                ) {
                    file$local_path[[1L]]
                } else {
                    file$url_opendap[[1L]]
                }
                reporter$detail(
                    sprintf("  source: %s", source_location),
                    level = "debug"
                )
            }
        }
        resumed <- resumed_outputs[[i]]
        if (!is.null(resumed)) {
            processed[[i]] <<- resumed
            if (!is.null(reporter)) {
                reporter$unit_skipped(
                    sprintf("Reused %s", plan$variable_id[[1L]]),
                    current = finished,
                    total = nrow(plans)
                )
            }
            return(invisible(NULL))
        }
        if (!nrow(file)) {
            processed[[i]] <<- store_extract__mark_plan_failed(
                private,
                plan,
                "The cataloged file record no longer exists."
            )
            if (!is.null(reporter)) {
                reporter$unit_completed(
                    sprintf(
                        "Failed %s: catalog record missing",
                        plan$variable_id[[1L]]
                    ),
                    current = finished,
                    total = nrow(plans),
                    outcome = "failed"
                )
            }
            return(invisible(NULL))
        }

        processed[[i]] <<- tryCatch(
            private$extract_one(
                plan,
                file[1L],
                fallback = fallback,
                overwrite = overwrite,
                reporter = reporter,
                resolved = resolved
            ),
            error = function(e) {
                if (inherits(e, "epwshiftr_store_extract_conflict")) {
                    stop(e)
                }
                store_extract__mark_plan_failed(
                    private,
                    plan,
                    conditionMessage(e)
                )
            }
        )
        if (!is.null(reporter)) {
            result_status <- processed[[i]]$status[[1L]]
            used_access <- attr(
                processed[[i]],
                "access_method",
                exact = TRUE
            )
            if (is.null(used_access)) {
                used_access <- if (
                    !is.na(file$local_path[[1L]]) &&
                        nzchar(file$local_path[[1L]])
                ) {
                    "local"
                } else {
                    "OPeNDAP"
                }
            }
            reporter$unit_completed(
                sprintf("%s %s", result_status, plan$variable_id[[1L]]),
                current = finished,
                total = nrow(plans),
                outcome = if (result_status %in% c("done", "empty")) {
                    "completed"
                } else {
                    "failed"
                },
                details = list(
                    unit_type = "extraction_plan",
                    scenario = store__chr1(file$experiment_id[[1L]]),
                    variable = plan$variable_id[[1L]],
                    period = sprintf(
                        "%s/%s",
                        format(plan$time_start[[1L]], "%Y"),
                        format(plan$time_stop[[1L]], "%Y")
                    ),
                    access_method = used_access
                )
            )
        }
    }

    # Only uncached remote work enters the pool. Local reads, complete
    # plans and cached payloads keep their inexpensive synchronous path.
    jobs <- vector("list", nrow(plans))
    for (i in seq_len(nrow(plans))) {
        plan <- plans[i]
        resumed_outputs[i] <- list(
            if (!overwrite && resume) {
                tryCatch(
                    store_extract__resume_plan(private, plan),
                    error = identity
                )
            } else {
                NULL
            }
        )
        if (inherits(resumed_outputs[[i]], "error")) {
            error <- resumed_outputs[[i]]
            resumed_outputs[i] <- list(NULL)
            process(i, error)
            next
        }
        if (!is.null(resumed_outputs[[i]]) || is.na(file_rows[[i]])) {
            process(i)
            next
        }
        file <- catalog[file_rows[[i]]]
        endpoint <- store__chr1(file$url_opendap)
        local <- store__chr1(file$local_path)
        has_local <- !is.na(local) &&
            nzchar(local) &&
            file.exists(store_abs_path(local, root = private$store_path))
        cached <- if (!identical(cache__mode(), "off")) {
            tryCatch(
                source_recovery__cache_read(
                    store__extract_cache_path(plan, file),
                    file,
                    overwrite
                ),
                error = identity
            )
        } else {
            NULL
        }
        if (inherits(cached, "error")) {
            process(i, cached)
        } else if (!is.null(cached)) {
            process(
                i,
                list(
                    payload = cached,
                    opened = list(
                        access_method = "shared_cache",
                        target = store__extract_cache_path(plan, file)
                    ),
                    recovery_error = NULL,
                    cache_reused = TRUE
                )
            )
        } else if (
            has_local ||
                (is.null(pool) &&
                    (is.na(endpoint) || !grepl("^https?://", endpoint))) ||
                identical(cache__mode(), "offline")
        ) {
            process(i)
        } else {
            jobs[[i]] <- list(index = i, plan = plan, file = file)
        }
    }
    failures <- vector("list", nrow(plans))
    jobs <- Filter(Negate(is.null), jobs)
    if (length(jobs)) {
        # Without disk caching, return one plan per task so a file's
        # entire city collection is never accumulated in worker memory.
        group <- if (!is.null(pool)) {
            vapply(
                jobs,
                function(job) source__physical_key(job$file),
                character(1L)
            )
        } else if (identical(cache__mode(), "off")) {
            seq_along(jobs)
        } else {
            vapply(
                jobs,
                function(job) job$file$file_key[[1L]],
                character(1L)
            )
        }
        groups <- split(seq_along(jobs), group)
        if (!is.null(pool)) {
            # Bound in-memory city payloads even with disk caching off.
            # Groups sharing one physical key are serialized by the pool.
            groups <- unlist(
                lapply(groups, function(rows) {
                    split(rows, ceiling(seq_along(rows) / 8L))
                }),
                recursive = FALSE
            )
        }
        jobs <- lapply(groups, function(rows) {
            list(
                indices = vapply(
                    jobs[rows],
                    function(job) job$index,
                    integer(1L)
                ),
                plans = data.table::rbindlist(lapply(
                    jobs[rows],
                    function(job) job$plan
                )),
                files = lapply(jobs[rows], function(job) job$file),
                file = jobs[[rows[[1L]]]]$file,
                overwrite = overwrite
            )
        })
    }
    collect_source <- function(job, value) {
        # Register verified native downloads only in the manifest owner;
        # readers never receive or write this store's DuckDB connection.
        if (!is.null(value$download_manifest)) {
            # One verified native read may serve distinct replica
            # catalogue rows; register only those exact requests.
            files <- job$files
            if (is.null(files)) {
                files <- list(job$file)
            }
            keys <- vapply(
                files,
                function(file) store__chr1(file$file_key),
                character(1L)
            )
            for (file in files[!duplicated(keys)]) {
                store__sync_pool_download(
                    self,
                    value$download_manifest,
                    source_recovery__receipt_for(
                        file,
                        value$download_receipt
                    ),
                    file
                )
            }
        }
        for (index in seq_along(job$indices)) {
            row <- job$indices[[index]]
            resolved <- value$results[[index]]
            if (inherits(resolved, "error")) {
                failures[[row]] <<- resolved
            } else {
                if (!is.null(resolved$cache_path)) {
                    resolved$payload <- source_recovery__cache_read(
                        resolved$cache_path,
                        job$files[[index]]
                    )
                    if (is.null(resolved$payload)) {
                        cli::cli_abort(
                            "A completed source payload is missing from the shared cache."
                        )
                    }
                }
                process(row, resolved)
            }
        }
    }
    finish <- function() {
        for (i in which(!vapply(failures, is.null, logical(1L)))) {
            process(i, failures[[i]])
        }
        data.table::rbindlist(processed, use.names = TRUE, fill = TRUE)
    }
    if (!is.null(pool)) {
        jobs <- lapply(jobs, function(job) {
            job$source_key <- source__physical_key(job$file)
            if (identical(fallback, "auto")) {
                directory <- pool$download_dir
                if (is.null(directory)) {
                    directory <- private$download_dir
                }
                job$http <- store__pool_download_config(
                    job$file,
                    directory,
                    pool$download_timeout
                )
            }
            # Annual bounded windows prevent accidental whole-period
            # payload accumulation in the persistent reader.
            stopifnot(all(
                as.double(difftime(
                    job$plans$time_stop,
                    job$plans$time_start,
                    units = "days"
                )) <=
                    366
            ))
            job
        })
        return(pool$submit(
            jobs,
            store__read_task,
            collect_source,
            finish = finish,
            on_error = function(job, error) {
                for (i in job$indices) {
                    failures[[i]] <<- error
                }
            }
        ))
    }
    source__apply(
        jobs,
        store__read_task,
        collect = collect_source,
        reporter = reporter
    )
    # HTTP fallback owns the store manifest and starts only after the
    # source pool has drained, so it cannot exceed the read limit.
    finish()
}
# }}}

# store_extract__resume_plan {{{
store_extract__resume_plan <- function(private, plan) {
    if (!identical(plan$status[[1L]], "done")) {
        return(NULL)
    }
    results <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "extraction_result"
    ))
    target_plan_id <- plan$plan_id[[1L]]
    results <- results[results[["plan_id"]] == target_plan_id]
    if (!nrow(results)) {
        return(NULL)
    }
    paths <- vapply(
        results$output_path,
        store_abs_path,
        character(1L),
        root = private$store_path
    )
    if (!all(file.exists(paths))) {
        return(NULL)
    }
    # Check only this requested plan, using the artifact snapshot that
    # committed with its result rather than current catalogue labels.
    artifacts <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "artifact"
    ))
    files <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "file_catalog"
    ))
    file <- files[files[["file_key"]] == plan$file_key[[1L]]]
    context <- paste("plan", target_plan_id, "file", plan$file_key[[1L]])
    if (nrow(file) != 1L) {
        source_recovery__error("insufficient catalogue lineage", context)
    }
    for (index in seq_len(nrow(results))) {
        artifact <- artifacts[
            artifacts[["artifact_id"]] == results$artifact_id[[index]]
        ]
        location <- paste(
            context,
            "artifact",
            results$artifact_id[[index]],
            paths[[index]]
        )
        source_recovery__artifact_check(artifact, paths[[index]], location)
        if (
            !identical(artifact$file_key[[1L]], plan$file_key[[1L]]) ||
                !identical(
                    store_abs_path(
                        artifact$relative_path[[1L]],
                        private$store_path
                    ),
                    paths[[index]]
                )
        ) {
            source_recovery__error(
                "source conflict in artifact association",
                location
            )
        }
        metadata <- tryCatch(
            jsonlite::fromJSON(
                artifact$metadata_json[[1L]],
                simplifyVector = FALSE
            ),
            error = function(e) NULL
        )
        source_recovery__check(metadata$source_lineage, file, location)
    }
    plan
}
# }}}

# store_extract__one {{{
store_extract__one <- function(
    self,
    private,
    plan,
    file,
    fallback = "auto",
    overwrite = FALSE,
    reporter = NULL,
    resolved = NULL
) {
    if (is.null(resolved) || inherits(resolved, "error")) {
        source <- new.env(parent = emptyenv())
        source$opened <- NULL
        source$target <- store__chr1(file$url_opendap)
        source$service <- "OPeNDAP"
        source$remote_error <- resolved
        local <- store__chr1(file$local_path)
        if (!is.na(local) && nzchar(local)) {
            local <- store_abs_path(local, root = private$store_path)
            if (file.exists(local)) {
                source$target <- local
                source$service <- "local"
                source$open_local <- function(target) {
                    source_recovery__local_open(self, file, target)
                }
            }
        }
        on.exit(
            if (!is.null(source$opened)) source$opened$dataset$close(),
            add = TRUE
        )
        # Keep the Store-owned downloader and reporter in the owner;
        # the shared reader only decides when this recovery is needed.
        download <- if (identical(fallback, "auto")) {
            function() {
                path <- private$download_plan_file(
                    file,
                    overwrite = overwrite,
                    reporter = reporter
                )
                list(
                    path = path,
                    manifest = NULL,
                    receipt = attr(path, "source_receipt")
                )
            }
        }
        resolved <- store__read_source(
            plan,
            file,
            source,
            download = download,
            reporter = reporter,
            overwrite = overwrite
        )
        if (!is.null(source$opened)) {
            source$opened$dataset$close()
            source$opened <- NULL
        }
    }
    payload <- resolved$payload
    opened <- resolved$opened
    recovery_error <- resolved$recovery_error

    persist_started <- proc.time()[["elapsed"]]
    tryCatch(
        # Hold the manifest lock before starting the transaction;
        # artifact registration reuses this same reentrant lock.
        private$with_store_lock(private$persist_extract_payload(
            payload,
            plan,
            file,
            opened,
            overwrite = overwrite
        )),
        error = function(error) {
            if (
                inherits(
                    error,
                    c(
                        "epwshiftr_store_access_error",
                        "epwshiftr_store_extract_conflict"
                    )
                )
            ) {
                stop(error)
            }
            classified <- store__access_error(
                error,
                phase = "persist",
                service = opened$access_method,
                target = opened$target,
                started_at = persist_started
            )
            store__report_access_failure(
                reporter,
                file,
                classified,
                attempt = if (
                    identical(opened$access_method, "HTTPServer") &&
                        !is.null(recovery_error)
                ) {
                    2L
                } else {
                    1L
                },
                outcome = "failed"
            )
            if (!is.null(recovery_error)) {
                stop(store__fallback_error(recovery_error, classified))
            }
            stop(classified)
        }
    )
}
# }}}

# Commit one successfully read payload exactly once. Keeping this after
# the recovery boundary prevents a failed OPeNDAP attempt from creating
# duplicate Parquet partitions or time-coverage updates.
# store_extract__persist_payload {{{
store_extract__persist_payload <- function(
    self,
    private,
    payload,
    plan,
    file,
    opened,
    overwrite = FALSE
) {
    source_recovery__check(
        payload$source_lineage,
        file,
        paste("plan", plan$plan_id[[1L]])
    )
    # A payload restored from the shared RDS cache is guaranteed to be
    # a data frame, but it need not retain data.table's by-reference
    # class. Materialize a private data.table copy before attaching
    # store-specific plan, query, and site identities.
    dt <- data.table::as.data.table(data.table::copy(payload$data))
    has_data <- nrow(dt) > 0L
    result <- private$publish_extract(
        plan,
        status = if (has_data) "done" else "empty",
        available_time_count = payload$available_time_count,
        replace_results = has_data,
        prepare = {
            store_extract__update_file_actual_time(
                private,
                file,
                payload$actual_start,
                payload$actual_end
            )
            if (has_data) {
                store_extract__decorate(dt, plan, file)
                # Keep Parquet on disk after rollback for the existing
                # explicit overwrite/conflict recovery path.
                results <- private$write_extract_partitions(
                    dt,
                    plan,
                    file,
                    overwrite = overwrite,
                    source_lineage = payload$source_lineage
                )
                list(
                    results = results,
                    grid_sources = store_extract__decorate_grid_sources(
                        payload$grid_sources,
                        plan
                    )
                )
            } else {
                list()
            }
        }
    )[[1L]]
    attr(result, "access_method") <- opened$access_method
    result
}
# }}}

# With the caller holding the Store lock, prepare and register artifacts
# in the same transaction as result rows, grid lineage and plan state.
# Each outcome is aligned to its local plan so retry history stays local.
# store_extract__publish {{{
store_extract__publish <- function(
    self,
    private,
    plans,
    status,
    available_time_count,
    prepare,
    replace_results = FALSE
) {
    ddb_exec(private$conn, "BEGIN TRANSACTION")
    committed <- FALSE
    on.exit(
        {
            if (!committed) {
                ddb_exec(private$conn, "ROLLBACK")
            }
        },
        add = TRUE
    )
    prepared <- force(prepare)
    if (replace_results) {
        private$delete_by_key("extraction_result", "plan_id", plans$plan_id)
        private$delete_by_key(
            "extraction_grid_source",
            "plan_id",
            plans$plan_id
        )
    }
    if (NROW(prepared$results)) {
        ddb_append_table(private$conn, "extraction_result", prepared$results)
    }
    if (NROW(prepared$grid_sources)) {
        ddb_append_table(
            private$conn,
            "extraction_grid_source",
            prepared$grid_sources
        )
    }

    updated <- lapply(seq_len(nrow(plans)), function(i) {
        store_extract__mark_plan_status(
            private,
            plans[i],
            status = status[[i]],
            available_time_count = available_time_count[[i]],
            last_error = NA_character_
        )
    })
    ddb_exec(private$conn, "COMMIT")
    committed <- TRUE
    updated
}
# }}}

# store_extract__download_plan_file {{{
store_extract__download_plan_file <- function(
    self,
    private,
    file,
    overwrite = FALSE,
    reporter = NULL
) {
    config <- list(
        dest = private$download_dir,
        temp = private$tmp_download_dir,
        manifest = file.path(
            private$download_dir,
            "_downloader",
            "manifest.duckdb"
        ),
        n_workers = 0L,
        cleanup = FALSE
    )
    attr(config, "progress") <- TRUE
    recovered <- source_recovery__download(
        file,
        config,
        overwrite = overwrite,
        reporter = reporter
    )
    source_recovery__register(self, file, recovered$receipt)
    path <- recovered$path
    attr(path, "source_receipt") <- recovered$receipt
    path
}
# }}}

# store_extract__decorate {{{
store_extract__decorate <- function(dt, plan, file) {
    calendar_columns <- intersect(CF_TIME_COORDINATE_COLUMNS, names(dt))
    data.table::set(dt, j = "plan_id", value = plan$plan_id[[1L]])
    data.table::set(dt, j = "file_key", value = plan$file_key[[1L]])
    data.table::set(dt, j = "query_id", value = plan$query_id[[1L]])
    data.table::set(dt, j = "site_id", value = plan$site_id[[1L]])
    data.table::set(dt, j = "source_id", value = file$source_id[[1L]])
    data.table::set(dt, j = "experiment_id", value = file$experiment_id[[1L]])
    data.table::set(dt, j = "variant_label", value = file$variant_label[[1L]])
    data.table::set(dt, j = "frequency", value = file$frequency[[1L]])
    data.table::set(dt, j = "table_id", value = file$table_id[[1L]])
    data.table::set(dt, j = "variable_id", value = plan$variable_id[[1L]])
    data.table::set(dt, j = "method", value = plan$method[[1L]])
    data.table::set(dt, j = "grid_label", value = file$grid_label[[1L]])

    leading <- c(
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
        calendar_columns,
        "lon",
        "lat",
        "method",
        "value"
    )
    data.table::setcolorder(dt, c(leading, setdiff(names(dt), leading)))
    invisible(dt)
}
# }}}

# Persists one static set of method-specific source grid cells per plan.
# store_extract__decorate_grid_sources {{{
store_extract__decorate_grid_sources <- function(sources, plan) {
    if (is.null(sources)) {
        return(data.table::data.table())
    }
    sources <- data.table::as.data.table(data.table::copy(sources))
    if (!nrow(sources)) {
        return(sources)
    }
    if (!"grid_elevation_m" %in% names(sources)) {
        data.table::set(sources, j = "grid_elevation_m", value = NA_real_)
    }
    now <- store__now()
    data.table::set(sources, j = "plan_id", value = plan$plan_id[[1L]])
    data.table::set(sources, j = "file_key", value = plan$file_key[[1L]])
    data.table::set(sources, j = "query_id", value = plan$query_id[[1L]])
    data.table::set(sources, j = "variable_id", value = plan$variable_id[[1L]])
    data.table::set(sources, j = "method", value = plan$method[[1L]])
    data.table::set(sources, j = "created_at", value = now)
    source_row_ids <- vapply(
        seq_len(nrow(sources)),
        function(i) {
            store__hash(
                sources$plan_id[[i]],
                sources$file_key[[i]],
                sources$variable_id[[i]],
                sources$method[[i]],
                sources$source_index[[i]]
            )
        },
        character(1L)
    )
    data.table::set(sources, j = "source_row_id", value = source_row_ids)
    cols <- c(
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
        "grid_elevation_m",
        "grid_dist_km",
        "weight",
        "created_at"
    )
    data.table::setcolorder(sources, cols)
    sources[, cols, with = FALSE]
}
# }}}

# store_extract__write_partitions {{{
store_extract__write_partitions <- function(
    self,
    private,
    dt,
    plan,
    file,
    overwrite = FALSE,
    project = "CMIP6",
    source_lineage = NULL
) {
    # The same durable extraction representation is shared by ESGF
    # projections and provider-normalized reanalysis observations.
    checkmate::assert_string(project, min.chars = 1L)
    # Preserve the historical `year` column while making it agree with
    # the source CF calendar for new extraction artifacts.
    partition_year <- if ("cf_year" %in% names(dt)) {
        as.integer(dt$cf_year)
    } else {
        as.integer(format(dt$time, "%Y", tz = "UTC"))
    }
    data.table::set(dt, j = "year", value = partition_year)
    years <- sort(unique(dt$year))
    results <- vector("list", length(years))
    for (i in seq_along(years)) {
        target_year <- years[[i]]
        # Use a non-column variable name so data.table does not resolve
        # both sides of the predicate to `dt$year`.
        chunk <- dt[dt[["year"]] == target_year]
        output_path <- store_extract__output_path(
            private,
            plan,
            file,
            target_year,
            project = project
        )
        private$write_parquet(chunk, output_path, overwrite = overwrite)
        artifact_id <- self$register_artifact(
            kind = "extract",
            path = output_path,
            role = "derived",
            project = project,
            query_id = plan$query_id[[1L]],
            file_key = plan$file_key[[1L]],
            metadata = list(
                plan_id = plan$plan_id[[1L]],
                year = as.integer(target_year),
                source_lineage = source_lineage,
                # Derived-variable rows carry their scientific lineage
                # in both the Parquet data and artifact manifest. Raw
                # extraction chunks simply record null provenance.
                derived_from = if ("derived_from" %in% names(chunk)) {
                    unique(as.character(chunk$derived_from))
                } else {
                    NULL
                },
                derivation = if ("derivation" %in% names(chunk)) {
                    unique(as.character(chunk$derivation))
                } else {
                    NULL
                },
                source_plan_ids = if ("source_plan_ids" %in% names(chunk)) {
                    unique(as.character(chunk$source_plan_ids))
                } else {
                    NULL
                }
            )
        )
        results[[i]] <- store_extract__result_row(
            private,
            plan,
            chunk,
            output_path,
            target_year,
            artifact_id
        )
    }

    data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
}
# }}}

# store_extract__output_path {{{
store_extract__output_path <- function(
    private,
    plan,
    file,
    year,
    project = "CMIP6"
) {
    checkmate::assert_string(project, min.chars = 1L)
    parts <- c(
        project = project,
        source_id = file$source_id[[1L]],
        experiment_id = file$experiment_id[[1L]],
        variant_label = file$variant_label[[1L]],
        frequency = file$frequency[[1L]],
        variable_id = plan$variable_id[[1L]],
        year = year
    )
    dirs <- paste0(
        names(parts),
        "=",
        vapply(parts, store_extract__partition, character(1L))
    )
    store__extract_output_path(
        private$extract_dir,
        dirs,
        plan$plan_id[[1L]],
        year
    )
}
# }}}

# store_extract__write_parquet {{{
store_extract__write_parquet <- function(
    self,
    private,
    dt,
    path,
    overwrite = FALSE
) {
    if (file.exists(path) && !isTRUE(overwrite)) {
        cli::cli_abort(
            "Parquet output already exists without a complete reusable extraction manifest row: {.path {path}}.",
            class = "epwshiftr_store_extract_conflict"
        )
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp_file <- tempfile(tmpdir = dirname(path), fileext = ".parquet")
    tmp_table <- sprintf(
        "tmp_extract_%s",
        substr(store__hash(path, Sys.time(), stats::runif(1L)), 1L, 16L)
    )
    on.exit(
        {
            try(
                duckdb::duckdb_unregister(private$conn, tmp_table),
                silent = TRUE
            )
            if (file.exists(tmp_file)) {
                unlink(tmp_file)
            }
        },
        add = TRUE
    )

    # Scan the existing R columns directly instead of first inserting
    # every site's weather rows into a temporary DuckDB table.
    duckdb::duckdb_register(private$conn, tmp_table, dt)
    ddb_exec(
        private$conn,
        sprintf(
            "COPY %s TO %s (FORMAT PARQUET, COMPRESSION ZSTD)",
            ddb_ident(private$conn, tmp_table),
            ddb_literal(private$conn, tmp_file)
        )
    )
    if (file.exists(path) && isTRUE(overwrite)) {
        unlink(path)
    }
    ok <- file.rename(tmp_file, path)
    if (!isTRUE(ok)) {
        stop(
            sprintf("Failed to move Parquet output into place: %s.", path),
            call. = FALSE
        )
    }

    invisible(path)
}
# }}}

# store_extract__result_row {{{
store_extract__result_row <- function(
    private,
    plan,
    dt,
    output_path,
    year,
    artifact_id
) {
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
        completed_at = store__now(),
        stringsAsFactors = FALSE
    )
}
# }}}

# store_extract__update_file_actual_time {{{
store_extract__update_file_actual_time <- function(private, file, start, end) {
    # All sites of a file share this native range. Avoid rewriting the
    # catalog row for each site, and preserve unrelated catalog fields.
    values <- ddb_literal(private$conn, c(start, end))
    ddb_exec(
        private$conn,
        sprintf(
            paste(
                "UPDATE file_catalog SET actual_time_start = %s,",
                "actual_time_end = %s WHERE file_key = %s AND",
                "(actual_time_start IS DISTINCT FROM %s OR",
                "actual_time_end IS DISTINCT FROM %s)"
            ),
            values[[1L]],
            values[[2L]],
            ddb_literal(private$conn, file$file_key[[1L]]),
            values[[1L]],
            values[[2L]]
        )
    )
    invisible(NULL)
}
# }}}

# store_extract__mark_plan_failed {{{
store_extract__mark_plan_failed <- function(private, plan, message) {
    store_extract__mark_plan_status(
        private,
        plan,
        status = "failed",
        available_time_count = plan$available_time_count[[1L]],
        last_error = message,
        increment_attempt = TRUE
    )
}
# }}}

# store_extract__mark_plan_status {{{
store_extract__mark_plan_status <- function(
    private,
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
}
# }}}

# store_extract__summarise {{{
store_extract__summarise <- function(
    self,
    private,
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
    checkmate::assert_character(
        by,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    private$check_open()

    map <- store_extract__summary_cols()
    unknown <- setdiff(by, names(map))
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown extraction summary column(s): {.val {unknown}}."
        )
    }

    groups <- unname(map[by])
    select_groups <- paste(
        sprintf("%s AS %s", groups, ddb_ident(private$conn, by)),
        collapse = ", "
    )
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
}
# }}}

# store_extract__coverage {{{
store_extract__coverage <- function(self, private, plan_id = NULL) {
    checkmate::assert_character(
        plan_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    private$check_open()

    plans <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "extraction_plan"
    ))
    if (!is.null(plan_id)) {
        # Avoid data.table's column-name lookup on the RHS; the argument
        # and the plan table both use the name `plan_id`.
        target_plan_id <- plan_id
        plans <- plans[plans[["plan_id"]] %in% target_plan_id]
    }
    if (!nrow(plans)) {
        return(plans)
    }

    catalog <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "file_catalog"
    ))
    results <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "extraction_result"
    ))
    agg <- if (nrow(results)) {
        results[,
            list(
                output_files = list(.SD[["output_path"]]),
                output_file_count = .N,
                output_rows = sum(.SD[["row_count"]], na.rm = TRUE),
                output_time_count = sum(
                    .SD[["unique_time_count"]],
                    na.rm = TRUE
                ),
                output_time_min = min(.SD[["time_min"]], na.rm = TRUE),
                output_time_max = max(.SD[["time_max"]], na.rm = TRUE)
            ),
            by = "plan_id"
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

    out <- merge(
        plans,
        catalog,
        by = c("query_id", "file_key"),
        all.x = TRUE,
        suffixes = c("", "_file")
    )
    out <- merge(out, agg, by = "plan_id", all.x = TRUE)
    miss_count <- which(is.na(out$output_file_count))
    if (length(miss_count)) {
        data.table::set(
            out,
            i = miss_count,
            j = "output_file_count",
            value = 0L
        )
        data.table::set(out, i = miss_count, j = "output_rows", value = 0L)
        data.table::set(
            out,
            i = miss_count,
            j = "output_time_count",
            value = 0L
        )
    }
    if (!"output_files" %in% names(out)) {
        data.table::set(
            out,
            j = "output_files",
            value = rep(list(character()), nrow(out))
        )
    }
    output_files_exist <- vapply(
        out$output_files,
        function(paths) {
            paths <- unlist(paths, use.names = FALSE)
            length(paths) > 0L &&
                all(file.exists(file.path(private$store_path, paths)))
        },
        logical(1L)
    )
    data.table::set(out, j = "output_files_exist", value = output_files_exist)
    data.table::set(
        out,
        j = "complete",
        value = out$status == "done" &
            output_files_exist &
            (is.na(out$available_time_count) |
                out$available_time_count <= 0L |
                out$output_time_count >= out$available_time_count)
    )

    leading <- c(
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
        )
    )
    data.table::setcolorder(out, c(leading, setdiff(names(out), leading)))
    out[]
}
# }}}

# vim: fdm=marker :
