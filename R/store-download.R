# Download orchestration, layout policy, manifest/session tracking, validation,
# repair, and storage cleanup helpers for EsgStore.

# Construct the shared base schema for operational and validation download plans.
# store__download_plan_rows {{{
store__download_plan_rows <- function(catalog) {
    catalog <- data.table::as.data.table(catalog)
    download_node <- query_result__url_host(catalog$url_download)
    use_catalog_node <- is.na(download_node) | !nzchar(download_node)
    download_node[use_catalog_node] <- catalog$data_node[use_catalog_node]
    data.table::data.table(
        logical_file_id = store__logical_file_id(catalog),
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
        data_node = download_node,
        priority = seq_len(nrow(catalog)),
        probe_latency = NA_real_,
        probe_throughput = NA_real_
    )
}
# }}}

# Synchronize a completed download through the package adapter, keeping the
# copyable Downloader independent of EsgStore and its scientific manifest.
# store__sync_download_job {{{
store__sync_download_job <- function(path, downloader) {
    store <- EsgStore$new(path = path)
    on.exit(store$close(), add = TRUE)
    store$sync_downloads(downloader)
    invisible(NULL)
}
# }}}

# store_download__layout {{{
store_download__layout <- function(self, private) {
    private$check_open()
    store_download__layout_policy(private)
}
# }}}

# store_download__set_layout {{{
store_download__set_layout <- function(
    self,
    private,
    layout = c("flat", "dataset", "drs", "template"),
    template = NULL,
    include_version = TRUE,
    collision = c("error", "checksum", "suffix"),
    missing = c("fallback", "error")
) {
    private$check_open()
    policy <- store_download__normalize_layout_policy(list(
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
                value = jsonlite::toJSON(
                    policy,
                    auto_unbox = TRUE,
                    null = "null"
                ),
                updated_at = store__now(),
                stringsAsFactors = FALSE
            ),
            "key"
        )
    })
    invisible(self)
}
# }}}

# store_download__downloader {{{
store_download__downloader <- function(self, private, ...) {
    private$check_open()
    Downloader$new(
        dest = private$download_dir,
        temp = private$tmp_download_dir,
        manifest = file.path(
            private$download_dir,
            "_downloader",
            "manifest.duckdb"
        ),
        ...
    )
}
# }}}

# Shared helper to fetch downloader policies and build a ranked download plan.
# store_download__build_plan {{{
store_download__build_plan <- function(
    files,
    downloader,
    replica,
    service,
    check_urls,
    concurrency,
    cache_seconds,
    strategy,
    ...
) {
    node_stats <- if (!is.null(downloader)) {
        tryCatch(
            downloader$data_nodes(service = service),
            error = function(e) NULL
        )
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
    resolved_concurrency <- store_download__concurrency(
        downloader,
        concurrency
    )
    files$download_plan(
        replica = replica,
        service = service,
        probe = check_urls,
        strategy = strategy,
        node_stats = node_stats,
        network_policy = network_policy,
        node_policy = node_policy,
        probe_concurrency = resolved_concurrency,
        probe_cache_seconds = cache_seconds,
        ...
    )
}
# }}}

# Shared foreground/background session runner used by download_query/download_files.
# store_download__run_session {{{
store_download__run_session <- function(
    self,
    private,
    downloader,
    session_id,
    run,
    background,
    mode,
    progress,
    overwrite,
    resume
) {
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
        downloader$run(
            session_id = session_id,
            progress = progress,
            overwrite = overwrite,
            resume = resume
        )
        self$sync_downloads(downloader)
    }
    session_id
}
# }}}

# store_download__preflight {{{
store_download__preflight <- function(
    self,
    private,
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

    row <- store_catalog__get_query_row(private, query_id)
    query <- store_catalog__load_query(private, row)
    files <- query$collect(
        type = "File",
        fields = fields,
        all = all,
        limit = limit,
        ...
    )
    preview <- store_catalog__preview_query_update(
        private,
        row = row,
        files = files
    )
    candidates <- store_download__build_plan(
        files = files,
        downloader = downloader,
        replica = replica,
        service = service,
        check_urls = probe,
        concurrency = probe_concurrency,
        cache_seconds = probe_cache_seconds,
        strategy = strategy
    )
    candidates <- private$decorate_download_plan(
        candidates,
        preview$file_rows
    )
    summary <- store_download__preflight_summary(
        private,
        row,
        preview$file_rows,
        candidates,
        downloader = downloader
    )
    list(
        summary = summary[],
        changes = preview$changes[],
        files = store_download__preflight_files(preview$file_rows),
        candidates = candidates[]
    )
}
# }}}

# store_download__query {{{
store_download__query <- function(
    self,
    private,
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

    row <- store_catalog__get_query_row(private, query_id)
    # Network collection can be slow; keep it outside the store lock.
    query <- store_catalog__load_query(private, row)
    files <- query$collect(
        type = "File",
        fields = fields,
        all = all,
        limit = limit,
        ...
    )

    # Commit the refreshed file links under a short lock.
    links <- private$with_store_lock({
        private$update_query_files(
            query_id,
            files,
            fields = fields,
            all = all,
            limit = limit
        )
    })

    # Build and enqueue the downloader session after releasing the store lock.
    session_id <- store_download__enqueue_query(
        self,
        private,
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
            store_catalog__set_query_update_session(
                private,
                links$update_id[[1L]],
                session_id
            )
        })
    }
    store_download__run_session(
        self,
        private,
        downloader = downloader,
        session_id = session_id,
        run = run,
        background = background,
        mode = mode,
        progress = progress,
        overwrite = overwrite,
        resume = resume
    )
}
# }}}

# store_download__status {{{
store_download__status <- function(
    self,
    private,
    query_id = NULL,
    session_id = NULL,
    downloader = NULL
) {
    private$check_open()
    checkmate::assert_string(query_id, null.ok = TRUE)
    checkmate::assert_string(session_id, null.ok = TRUE)
    if (is.null(downloader)) {
        downloader <- self$downloader()
    }
    tasks <- data.table::as.data.table(downloader$tasks(
        session_id = session_id
    ))
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
    link_cols <- data.table::data.table(
        file_key = links$file_key,
        query_id = links$query_id,
        query_file_status = links$status
    )
    merge(tasks, link_cols, by = "file_key", all.x = TRUE, sort = FALSE)
}
# }}}

# store_download__storage_report {{{
store_download__storage_report <- function(self, private, detail = FALSE) {
    private$check_open()
    checkmate::assert_flag(detail)
    report <- store_download__build_storage_report(private)
    if (isTRUE(detail)) {
        return(report)
    }
    report$summary
}
# }}}

# store_download__validate_files {{{
store_download__validate_files <- function(
    self,
    private,
    query_id = NULL,
    checksum = FALSE,
    layout = TRUE
) {
    private$check_open()
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_flag(checksum)
    checkmate::assert_flag(layout)

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
        catalog <- catalog[
            catalog[["query_id"]] %in%
                wanted_query |
                catalog[["file_key"]] %in% link_keys
        ]
        esg_files <- esg_files[esg_files[["file_key"]] %in% link_keys]
        artifact_ids <- unique(c(
            catalog$local_artifact_id,
            esg_files$local_artifact_id
        ))
        artifact_ids <- artifact_ids[
            !is.na(artifact_ids) & nzchar(artifact_ids)
        ]
        artifacts <- artifacts[
            artifacts[["query_id"]] %in%
                wanted_query |
                artifacts[["file_key"]] %in% link_keys |
                artifacts[["artifact_id"]] %in% artifact_ids
        ]
    }

    storage <- store_download__build_storage_report(private)
    files <- store_download__validate_file_rows(
        private,
        catalog,
        esg_files,
        artifacts,
        checksum = checksum,
        layout = layout
    )
    artifacts <- store_download__validate_artifact_rows(
        private,
        artifacts,
        catalog,
        esg_files
    )
    actions <- store_download__validate_actions(files, artifacts)
    n_true <- function(x) as.integer(sum(x %in% TRUE, na.rm = TRUE))
    summary <- data.table::data.table(
        query_id = query_label,
        file_count = as.integer(nrow(files)),
        registered_file_count = as.integer(sum(
            !is.na(files$local_path) & nzchar(files$local_path),
            na.rm = TRUE
        )),
        existing_file_count = n_true(files$exists),
        missing_file_count = n_true(files$missing_local_ref),
        bad_size_count = n_true(files$bad_size),
        bad_checksum_count = n_true(files$bad_checksum),
        artifact_mismatch_count = n_true(files$artifact_mismatch),
        layout_mismatch_count = n_true(files$layout_mismatch),
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
}
# }}}

# store_download__repair_files {{{
store_download__repair_files <- function(
    self,
    private,
    actions = NULL,
    dry_run = TRUE
) {
    private$check_open()
    checkmate::assert_data_frame(actions, null.ok = TRUE)
    checkmate::assert_flag(dry_run)

    if (is.null(actions)) {
        actions <- self$validate_files()$actions
    }
    actions <- data.table::as.data.table(actions)
    if (isTRUE(dry_run)) {
        return(store_download__repair_actions(
            private,
            actions,
            dry_run = TRUE
        ))
    }
    private$with_store_lock({
        store_download__repair_actions(private, actions, dry_run = FALSE)
    })
}
# }}}

# store_download__cleanup_downloads {{{
store_download__cleanup_downloads <- function(
    self,
    private,
    scope = c(
        "tmp",
        "orphan_records",
        "untracked_files",
        "missing_records"
    ),
    dry_run = TRUE,
    older_than = NULL
) {
    private$check_open()
    checkmate::assert_subset(
        scope,
        c(
            "tmp",
            "orphan_records",
            "untracked_files",
            "missing_records"
        ),
        empty.ok = FALSE
    )
    checkmate::assert_flag(dry_run)
    cutoff <- store_download__cleanup_cutoff(older_than)
    report <- store_download__build_storage_report(private)
    collect_actions <- function() {
        actions <- list()
        if ("tmp" %in% scope) {
            actions$tmp <- store_download__cleanup_file_scope(
                "tmp",
                report$tmp,
                dry_run = dry_run,
                cutoff = cutoff
            )
        }
        if ("untracked_files" %in% scope) {
            actions$untracked_files <- store_download__cleanup_file_scope(
                "untracked_files",
                report$untracked_files,
                dry_run = dry_run,
                cutoff = cutoff
            )
        }
        if ("orphan_records" %in% scope) {
            actions$orphan_records <- store_download__cleanup_orphan_records(
                private,
                report$orphan_records,
                dry_run = dry_run
            )
        }
        if ("missing_records" %in% scope) {
            actions$missing_records <- store_download__cleanup_missing_records(
                private,
                report$missing_records,
                dry_run = dry_run
            )
        }
        data.table::rbindlist(actions, fill = TRUE)
    }
    if (isTRUE(dry_run)) {
        collect_actions()
    } else {
        private$with_store_lock(collect_actions())
    }
}
# }}}

# store_download__retry_downloads {{{
store_download__retry_downloads <- function(
    self,
    private,
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
    checkmate::assert_subset(
        status,
        c("error", "cancelled"),
        empty.ok = FALSE
    )
    checkmate::assert_flag(run)
    if (is.null(downloader)) {
        downloader <- self$downloader()
    }

    tasks <- data.table::as.data.table(downloader$tasks(
        session_id = session_id,
        status = status
    ))
    if (!is.null(query_id) && nrow(tasks)) {
        links <- self$query_files(query_id)
        tasks <- tasks[tasks[["file_key"]] %in% links$file_key]
    }
    if (!nrow(tasks)) {
        return(tasks[])
    }
    task_id <- tasks$task_id
    downloader$retry(
        session_id = session_id,
        task_id = task_id,
        status = status
    )
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
}
# }}}

# store_download__files {{{
store_download__files <- function(
    self,
    private,
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
    checkmate::assert_character(
        query_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
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
            catalog <- data.table::as.data.table(ddb_read_table(
                private$conn,
                "file_catalog"
            ))
            catalog[catalog[["query_id"]] == query_id]
        })
        plan <- store_download__build_plan(
            files = files,
            downloader = downloader,
            replica = replica,
            service = service,
            check_urls = probe,
            concurrency = probe_concurrency,
            cache_seconds = probe_cache_seconds,
            strategy = strategy,
            ...
        )
        plan <- private$decorate_download_plan(plan, current)
        plan[plan[["file_key"]] %in% current$file_key]
    } else {
        # Catalog reads need a consistent snapshot, but downloader work does not.
        private$with_store_lock({
            private$catalog_download_plan(query_id = query_id)
        })
    }
    if (!nrow(plan)) {
        return(NA_character_)
    }
    # Downloader manifest operations must not hold the store lock.
    tryCatch(
        downloader$record_probes(plan, probed = probe),
        error = function(e) NULL
    )
    session_id <- downloader$enqueue(plan, session_label = session_label)
    store_download__run_session(
        self,
        private,
        downloader = downloader,
        session_id = session_id,
        run = run,
        background = background,
        mode = mode,
        progress = progress,
        overwrite = overwrite,
        resume = resume
    )
}
# }}}

# store_download__sync {{{
store_download__sync <- function(self, private, downloader = NULL) {
    private$check_open()
    if (is.null(downloader)) {
        downloader <- self$downloader()
    }
    private$with_store_lock({
        tasks <- data.table::as.data.table(downloader$tasks(
            status = c("done", "skipped")
        ))
        if (!nrow(tasks)) {
            return(tasks)
        }
        catalog <- data.table::as.data.table(ddb_read_table(
            private$conn,
            "file_catalog"
        ))
        if (!nrow(catalog)) {
            return(tasks)
        }
        for (i in seq_len(nrow(tasks))) {
            task <- tasks[i]
            file_key <- store_download__match_task(task, catalog)
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
            if (
                is.na(checksum_type) ||
                    !checksum_type %in% c("md5", "sha256")
            ) {
                checksum_type <- "sha256"
            }
            if (is.na(checksum)) {
                checksum <- NULL
            }
            # Preserve the exact transport locator for later offline
            # adoption of its saved producer checkpoint.
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
                metadata = list(
                    filename = store__chr1(task$filename[[1L]]),
                    download_manifest = store_normalize_path(
                        downloader$manifest
                    ),
                    download_session_id = store__chr1(task$session_id),
                    download_task_id = store__chr1(task$task_id)
                )
            )
            row$local_path <- store_rel_path(local_path, private$store_path)
            row$local_artifact_id <- artifact_id
            private$replace_rows("file_catalog", as.data.frame(row), "file_key")
            private$sync_tracked_file_download(
                file_key,
                local_path,
                artifact_id
            )
        }
        tasks
    })
}
# }}}

# store_download__sync_tracked_file {{{
store_download__sync_tracked_file <- function(
    self,
    private,
    file_key,
    local_path,
    artifact_id
) {
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
}
# }}}

# Download layout helpers

# store_download__normalize_layout_policy {{{
store_download__normalize_layout_policy <- function(policy = NULL) {
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
        cli::cli_abort(
            "{.arg template} is required when {.code layout = \"template\"}."
        )
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
}
# }}}

# store_download__layout_policy {{{
store_download__layout_policy <- function(private) {
    meta <- private$read_table("store_meta")
    row <- meta[meta[["key"]] == "download_layout"]
    if (!nrow(row)) {
        return(store_download__normalize_layout_policy())
    }
    policy <- tryCatch(
        jsonlite::fromJSON(
            row$value[[1L]],
            simplifyVector = TRUE,
            simplifyMatrix = FALSE
        ),
        error = function(e) list()
    )
    store_download__normalize_layout_policy(policy)
}
# }}}

# store_download__apply_layout {{{
store_download__apply_layout <- function(private, plan, file_rows = NULL) {
    plan <- data.table::copy(data.table::as.data.table(plan))
    plan <- data.table::setalloccol(plan)
    if (!nrow(plan)) {
        return(plan)
    }
    plan <- store_download__layout_plan_candidates(private, plan, file_rows)
    policy <- store_download__layout_policy(private)
    store_download__resolve_plan_collisions(private, plan, policy)
}
# }}}

# store_download__layout_plan_candidates {{{
store_download__layout_plan_candidates <- function(
    private,
    plan,
    file_rows = NULL
) {
    plan <- data.table::copy(data.table::as.data.table(plan))
    plan <- data.table::setalloccol(plan)
    if (!nrow(plan)) {
        return(plan)
    }
    policy <- store_download__layout_policy(private)
    plan <- store_download__enrich_plan_layout_fields(plan, file_rows)
    data.table::set(
        plan,
        j = "layout_missing_fields",
        value = rep("", nrow(plan))
    )
    if (identical(policy$layout, "flat")) {
        data.table::set(
            plan,
            j = "subdir",
            value = rep(NA_character_, nrow(plan))
        )
    } else {
        subdirs <- vapply(
            seq_len(nrow(plan)),
            function(i) store_download__layout_subdir(plan[i], policy),
            character(1L)
        )
        missing <- vapply(
            seq_len(nrow(plan)),
            function(i) store_download__layout_missing_fields(plan[i], policy),
            character(1L)
        )
        data.table::set(plan, j = "subdir", value = subdirs)
        data.table::set(plan, j = "layout_missing_fields", value = missing)
    }
    plan <- store_download__decorate_plan_targets(private, plan)
    plan[]
}
# }}}

# store_download__enrich_plan_layout_fields {{{
store_download__enrich_plan_layout_fields <- function(plan, file_rows = NULL) {
    fields <- c(
        "activity_id",
        "institution_id",
        "source_id",
        "experiment_id",
        "variant_label",
        "frequency",
        "table_id",
        "variable_id",
        "grid_label",
        "version",
        "dataset_id",
        "checksum",
        "filename"
    )
    for (field in fields) {
        if (!field %in% names(plan)) {
            data.table::set(
                plan,
                j = field,
                value = rep(NA_character_, nrow(plan))
            )
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
            data.table::set(
                plan,
                i = which(fill),
                j = field,
                value = as.character(file_rows[[field]][idx[fill]])
            )
        }
    }
    plan[]
}
# }}}

# store_download__layout_subdir {{{
store_download__layout_subdir <- function(row, policy) {
    if (identical(policy$layout, "dataset")) {
        return(store_download__layout_dataset_subdir(row))
    }
    if (identical(policy$layout, "drs")) {
        missing <- store_download__layout_missing_fields(row, policy)
        if (nzchar(missing)) {
            if (identical(policy$missing, "error")) {
                cli::cli_abort(
                    "Cannot build DRS download path; missing field(s): {.field {strsplit(missing, ',')[[1L]]}}."
                )
            }
            return(store_download__layout_dataset_subdir(row))
        }
        drs_cols <- c(
            "activity_id",
            "institution_id",
            "source_id",
            "experiment_id",
            "variant_label",
            "table_id",
            "variable_id",
            "grid_label"
        )
        parts <- c(
            "CMIP6",
            vapply(
                drs_cols,
                function(f) store_download__layout_component(row[[f]]),
                character(1L)
            )
        )
        if (isTRUE(policy$include_version)) {
            ver <- store_download__layout_component(row$version)
            if (!is.na(ver) && !startsWith(ver, "v")) {
                ver <- paste0("v", ver)
            }
            parts <- c(parts, ver)
        }
        return(store_download__layout_path(parts))
    }
    if (identical(policy$layout, "template")) {
        return(store_download__layout_template_subdir(row, policy))
    }
    NA_character_
}
# }}}

# store_download__layout_dataset_subdir {{{
store_download__layout_dataset_subdir <- function(row) {
    dataset_id <- store_download__layout_component(row$dataset_id)
    if (!is.na(dataset_id)) {
        return(file.path("datasets", dataset_id))
    }
    cols <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "table_id",
        "variable_id",
        "grid_label"
    )
    pieces <- vapply(
        cols,
        function(f) store_download__layout_component(row[[f]]),
        character(1L)
    )
    pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
    if (!length(pieces)) {
        pieces <- paste0(
            "file-",
            substr(
                store__hash(
                    row$logical_file_id,
                    row$file_key,
                    row$filename
                ),
                1L,
                12L
            )
        )
    }
    store_download__layout_path(c("datasets", pieces))
}
# }}}

# store_download__layout_template_subdir {{{
store_download__layout_template_subdir <- function(row, policy) {
    out <- policy$template
    fields <- unique(unlist(regmatches(
        out,
        gregexpr("\\{[^{}]+\\}", out)
    )))
    for (token in fields) {
        field <- sub("^\\{", "", sub("\\}$", "", token))
        value <- if (field %in% names(row)) {
            store_download__layout_component(row[[field]])
        } else {
            NA_character_
        }
        if (is.na(value)) {
            if (identical(policy$missing, "error")) {
                cli::cli_abort(
                    "Cannot build template download path; missing field {.field {field}}."
                )
            }
            value <- "unknown"
        }
        out <- gsub(token, value, out, fixed = TRUE)
    }
    parts <- strsplit(gsub("[/\\\\]+", "/", out), "/", fixed = TRUE)[[1L]]
    store_download__layout_path(vapply(
        parts,
        store_download__layout_component,
        character(1L)
    ))
}
# }}}

# store_download__layout_missing_fields {{{
store_download__layout_missing_fields <- function(row, policy) {
    if (!identical(policy$layout, "drs")) {
        return("")
    }
    required <- c(
        "activity_id",
        "institution_id",
        "source_id",
        "experiment_id",
        "variant_label",
        "table_id",
        "variable_id",
        "grid_label",
        if (isTRUE(policy$include_version)) "version"
    )
    missing <- required[vapply(
        required,
        function(field) {
            is.na(store_download__layout_component(
                if (field %in% names(row)) row[[field]] else NA_character_
            ))
        },
        logical(1L)
    )]
    paste(missing, collapse = ",")
}
# }}}

# store_download__decorate_plan_targets {{{
store_download__decorate_plan_targets <- function(private, plan) {
    previous_collision <- if ("target_path_collision" %in% names(plan)) {
        plan$target_path_collision %in% TRUE
    } else {
        rep(FALSE, nrow(plan))
    }
    previous_collision_group <- if (
        "target_path_collision_group" %in% names(plan)
    ) {
        plan$target_path_collision_group
    } else {
        rep(NA_character_, nrow(plan))
    }
    target_rel <- mapply(
        function(subdir, filename) {
            filename <- store_download__layout_component(filename)
            if (is.na(filename)) {
                filename <- "download.nc"
            }
            if (is.na(subdir) || !nzchar(subdir)) {
                filename
            } else {
                file.path(subdir, filename)
            }
        },
        plan$subdir,
        plan$filename,
        USE.NAMES = FALSE
    )
    data.table::set(plan, j = "target_rel_path", value = target_rel)
    data.table::set(
        plan,
        j = "target_path",
        value = file.path(private$download_dir, target_rel)
    )
    data.table::set(
        plan,
        j = "target_path_collision",
        value = previous_collision
    )
    data.table::set(
        plan,
        j = "target_path_collision_group",
        value = previous_collision_group
    )
    plan[]
}
# }}}

# store_download__resolve_plan_collisions {{{
store_download__resolve_plan_collisions <- function(private, plan, policy) {
    collision <- plan[,
        list(logical_count = data.table::uniqueN(.SD[["logical_file_id"]])),
        by = "target_rel_path"
    ]
    collision <- collision[collision[["logical_count"]] > 1L]
    if (!nrow(collision)) {
        return(plan[])
    }
    hit_rel <- plan$target_rel_path %in% collision$target_rel_path
    data.table::set(
        plan,
        i = which(hit_rel),
        j = "target_path_collision",
        value = TRUE
    )
    hit_col <- plan$target_path_collision %in% TRUE
    data.table::set(
        plan,
        i = which(hit_col),
        j = "target_path_collision_group",
        value = plan$target_rel_path[hit_col]
    )
    if (identical(policy$collision, "error")) {
        cli::cli_abort(
            "Download layout maps multiple logical files to the same target path: {.path {collision$target_rel_path}}."
        )
    }
    colliding <- plan[hit_col]
    disambiguators <- colliding[, .SD[1L], by = "logical_file_id"]
    disambig_values <- vapply(
        seq_len(nrow(disambiguators)),
        function(i) {
            if (identical(policy$collision, "checksum")) {
                checksum <- store_download__layout_component(
                    disambiguators$checksum[[i]]
                )
                if (!is.na(checksum)) {
                    return(paste0("checksum=", substr(checksum, 1L, 12L)))
                }
            }
            paste0(
                "file=",
                substr(
                    store__hash(
                        disambiguators$logical_file_id[[i]],
                        disambiguators$file_key[[i]],
                        disambiguators$filename[[i]]
                    ),
                    1L,
                    12L
                )
            )
        },
        character(1L)
    )
    map <- stats::setNames(disambig_values, disambiguators$logical_file_id)
    hit <- which(plan$logical_file_id %in% names(map))
    if (length(hit)) {
        updated_subdir <- mapply(
            function(subdir, logical_file_id) {
                extra <- map[[logical_file_id]]
                if (is.na(subdir) || !nzchar(subdir)) {
                    extra
                } else {
                    file.path(subdir, extra)
                }
            },
            plan$subdir[hit],
            plan$logical_file_id[hit],
            USE.NAMES = FALSE
        )
        data.table::set(plan, i = hit, j = "subdir", value = updated_subdir)
    }
    store_download__decorate_plan_targets(private, plan)
}
# }}}

# store_download__layout_component {{{
store_download__layout_component <- function(x) {
    x <- store__chr1(x)
    if (is.na(x) || !nzchar(x)) {
        return(NA_character_)
    }
    x <- sub("\\|.*$", "", x)
    x <- gsub("[/\\\\]+", "_", x)
    x <- gsub("[[:cntrl:]]+", "_", x)
    x <- gsub("^\\.+$", "_", x)
    if (!nzchar(x)) NA_character_ else x
}
# }}}

# store_download__layout_path {{{
store_download__layout_path <- function(parts) {
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (!length(parts)) {
        return(NA_character_)
    }
    do.call(file.path, as.list(parts))
}
# }}}

# Download plan & preflight helpers

# store_download__catalog_plan {{{
store_download__catalog_plan <- function(self, private, query_id = NULL) {
    catalog <- data.table::as.data.table(ddb_read_table(
        private$conn,
        "file_catalog"
    ))
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
    catalog <- catalog[is.na(catalog$local_path) | !nzchar(catalog$local_path)]
    if (!nrow(catalog)) {
        return(data.table::data.table())
    }
    plan <- store__download_plan_rows(catalog)
    plan <- plan[!is.na(plan$url) & nzchar(plan$url)]
    if (!nrow(plan)) {
        return(plan)
    }
    extra <- intersect(
        c(
            "latest",
            "replica",
            "retracted",
            "deprecated",
            "version",
            "url_download",
            "local_path",
            "local_artifact_id"
        ),
        names(catalog)
    )
    if (length(extra)) {
        idx <- match(plan$file_key, catalog$file_key)
        for (name in extra) {
            data.table::set(plan, j = name, value = catalog[[name]][idx])
        }
    }
    plan <- store_download__layout_plan_candidates(private, plan, catalog)
    plan <- store_download__select_catalog_candidates(plan)
    store_download__apply_layout(private, plan, catalog)
}
# }}}

# store_download__rank_catalog_candidates {{{
store_download__rank_catalog_candidates <- function(plan) {
    plan <- data.table::copy(data.table::as.data.table(plan))
    plan <- data.table::setalloccol(plan)
    if (!nrow(plan)) {
        data.table::set(plan, j = "candidate_rank", value = integer())
        return(plan)
    }
    for (name in c("latest", "replica", "retracted", "deprecated")) {
        if (!name %in% names(plan)) {
            data.table::set(plan, j = name, value = NA)
        }
    }
    data.table::set(plan, j = "catalog_row_order", value = seq_len(nrow(plan)))
    data.table::set(
        plan,
        j = "retracted_order",
        value = data.table::fifelse(
            store__lgl(plan$retracted) %in% TRUE,
            1L,
            0L
        )
    )
    data.table::set(
        plan,
        j = "deprecated_order",
        value = data.table::fifelse(
            store__lgl(plan$deprecated) %in% TRUE,
            1L,
            0L
        )
    )
    latest <- store__lgl(plan$latest)
    data.table::set(
        plan,
        j = "latest_order",
        value = data.table::fifelse(
            latest %in% TRUE,
            0L,
            data.table::fifelse(is.na(latest), 1L, 2L)
        )
    )
    data.table::set(
        plan,
        j = "version_rank",
        value = store__version_rank(plan$version)
    )
    data.table::set(
        plan,
        j = "version_missing",
        value = is.na(plan$version_rank)
    )
    data.table::set(
        plan,
        j = "replica_order",
        value = data.table::fifelse(
            store__lgl(plan$replica) %in% TRUE,
            1L,
            0L
        )
    )
    data.table::set(
        plan,
        j = "https_order",
        value = data.table::fifelse(grepl("^https://", plan$url), 0L, 1L)
    )
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
    plan[, ("candidate_rank") := seq_len(.N), by = "target_rel_path"]
    plan[]
}
# }}}

# store_download__select_catalog_candidates {{{
store_download__select_catalog_candidates <- function(plan) {
    plan <- store_download__rank_catalog_candidates(plan)
    if (!nrow(plan)) {
        return(plan)
    }
    ambiguous <- store_download__ambiguous_catalog_candidates(plan)
    if (nrow(ambiguous)) {
        cli::cli_abort(c(
            "Cataloged file candidates map to the same download target with different checksums.",
            "x" = "Cannot safely choose one candidate for {.path {ambiguous$target_rel_path[[1L]]}}.",
            "i" = "Use more specific file filters or a download layout that separates these files."
        ))
    }
    plan[, ("candidate_count") := .N, by = "target_rel_path"]
    data.table::set(
        plan,
        j = "candidate_selection",
        value = data.table::fifelse(
            plan$candidate_count > 1L,
            "ranked_by_catalog_metadata",
            "single"
        )
    )
    selected <- plan[plan[["candidate_rank"]] == 1L]
    data.table::set(selected, j = "selected_candidate", value = TRUE)
    selected[]
}
# }}}

# store_download__ambiguous_catalog_candidates {{{
store_download__ambiguous_catalog_candidates <- function(plan) {
    plan <- data.table::as.data.table(plan)
    if (!nrow(plan)) {
        return(data.table::data.table())
    }
    data.table::set(
        plan,
        j = "rank_signature",
        value = paste(
            plan$retracted_order,
            plan$deprecated_order,
            plan$latest_order,
            plan$version_missing,
            plan$version_rank,
            plan$replica_order,
            plan$https_order,
            sep = "\r"
        )
    )
    groups <- plan[,
        list(
            n = .N,
            checksum_count = data.table::uniqueN(.SD[["checksum"]][
                !is.na(.SD[["checksum"]]) & nzchar(.SD[["checksum"]])
            ]),
            top_signature_count = data.table::uniqueN(.SD[["rank_signature"]][
                .SD[["candidate_rank"]] %in% c(1L, 2L)
            ])
        ),
        by = "target_rel_path"
    ]
    data.table::set(plan, j = "rank_signature", value = NULL)
    groups[
        groups[["n"]] > 1L &
            groups[["checksum_count"]] > 1L &
            groups[["top_signature_count"]] == 1L
    ]
}
# }}}

# store_download__decorate_plan {{{
store_download__decorate_plan <- function(self, private, plan, file_rows) {
    plan <- data.table::copy(data.table::as.data.table(plan))
    file_rows <- data.table::copy(data.table::as.data.table(file_rows))
    plan <- data.table::setalloccol(plan)
    if (!nrow(plan) || !nrow(file_rows)) {
        return(plan)
    }
    if (!"file_key" %in% names(plan)) {
        data.table::set(
            plan,
            j = "file_key",
            value = rep(NA_character_, nrow(plan))
        )
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
            data.table::set(
                plan,
                i = i,
                j = "file_key",
                value = hit$file_key[[1L]]
            )
        }
    }
    store_download__apply_layout(private, plan, file_rows)
}
# }}}

# store_download__preflight_files {{{
store_download__preflight_files <- function(file_rows) {
    file_rows <- data.table::as.data.table(file_rows)
    if (!nrow(file_rows)) {
        return(file_rows)
    }
    data.table::set(
        file_rows,
        j = "status",
        value = store_catalog__file_link_status(file_rows)
    )
    file_rows[]
}
# }}}

# store_download__concurrency {{{
store_download__concurrency <- function(downloader = NULL, concurrency = NULL) {
    if (!is.null(concurrency)) {
        checkmate::assert_count(concurrency, positive = TRUE)
        return(as.integer(concurrency))
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
}
# }}}

# store_download__preflight_summary {{{
store_download__preflight_summary <- function(
    private,
    row,
    file_rows,
    candidates,
    downloader = NULL
) {
    files <- store_download__preflight_files(file_rows)
    current <- if (nrow(files)) {
        files[files[["status"]] == "current"]
    } else {
        files
    }
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
    candidate_keys <- if (
        nrow(candidates) && "file_key" %in% names(candidates)
    ) {
        unique(candidates$file_key[
            !is.na(candidates$file_key) & nzchar(candidates$file_key)
        ])
    } else {
        character()
    }
    current_keys <- if (nrow(current)) {
        unique(current$file_key)
    } else {
        character()
    }
    cooling_nodes <- 0L
    if (nrow(candidates) && "node_cooldown_rank" %in% names(candidates)) {
        cooling <- candidates[candidates[["node_cooldown_rank"]] > 0L]
        if (nrow(cooling) && "data_node" %in% names(cooling)) {
            cooling_nodes <- length(unique(cooling$data_node[
                !is.na(cooling$data_node) & nzchar(cooling$data_node)
            ]))
        }
    }
    bytes_total <- if (nrow(current) && "size" %in% names(current)) {
        sum(suppressWarnings(as.numeric(current$size)), na.rm = TRUE)
    } else {
        0
    }
    target_collisions <- 0L
    if (nrow(candidates) && "target_path_collision" %in% names(candidates)) {
        collision_path <- if (
            "target_path_collision_group" %in% names(candidates)
        ) {
            candidates$target_path_collision_group
        } else {
            candidates$target_rel_path
        }
        target_collisions <- length(unique(collision_path[
            candidates$target_path_collision %in%
                TRUE &
                !is.na(collision_path) &
                nzchar(collision_path)
        ]))
    }
    missing_layout <- 0L
    if (nrow(candidates) && "layout_missing_fields" %in% names(candidates)) {
        missing_layout <- data.table::uniqueN(candidates$logical_file_id[
            !is.na(candidates$layout_missing_fields) &
                nzchar(candidates$layout_missing_fields)
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
        needs_download = as.integer(max(
            0L,
            length(current_keys) - local_available
        )),
        no_httpserver = as.integer(length(setdiff(
            current_keys,
            candidate_keys
        ))),
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
                data.table::set(summary, j = col, value = space[[col]][[1L]])
            }
        }
    }
    summary[]
}
# }}}

# store_download__enqueue_query {{{
store_download__enqueue_query <- function(
    self,
    private,
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
            cli::cli_abort(
                "Stored ESGF query {.val {query_id}} has no current files to download."
            )
        }
        return(NA_character_)
    }

    # Use the committed current-file snapshot supplied by the caller.
    plan <- store_download__build_plan(
        files = files,
        downloader = downloader,
        replica = replica,
        service = service,
        check_urls = probe,
        concurrency = probe_concurrency,
        cache_seconds = probe_cache_seconds,
        strategy = strategy
    )
    plan <- private$decorate_download_plan(plan, current)
    plan <- plan[plan[["file_key"]] %in% current$file_key]
    if (!nrow(plan)) {
        if (isTRUE(error_if_empty)) {
            cli::cli_abort(
                "Stored ESGF query {.val {query_id}} has no downloadable HTTPServer URLs."
            )
        }
        return(NA_character_)
    }
    if (is.null(session_label)) {
        session_label <- query_id
    }
    tryCatch(
        downloader$record_probes(plan, probed = probe),
        error = function(e) NULL
    )
    downloader$enqueue(plan, session_label = session_label)
}
# }}}

# store_download__match_task {{{
store_download__match_task <- function(task, catalog) {
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
}
# }}}

# Download validation, repair, storage report, and cleanup helpers

# store_download__validate_file_rows {{{
store_download__validate_file_rows <- function(
    private,
    catalog,
    esg_files,
    artifacts,
    checksum = FALSE,
    layout = TRUE
) {
    catalog <- data.table::as.data.table(catalog)
    esg_files <- data.table::as.data.table(esg_files)
    artifacts <- data.table::as.data.table(artifacts)
    if (!nrow(catalog)) {
        return(store__empty_table(paste0(
            "file_key:c,query_id:c,filename:c,local_path:c,local_artifact_id:c,",
            "actual_path:c,exists:l,expected_path:c,expected_rel_path:c,",
            "size_expected:n,size_actual:n,size_ok:l,checksum_expected:c,",
            "checksum_type:c,checksum_actual:c,checksum_ok:l,artifact_record_exists:l,",
            "artifact_path:c,artifact_path_exists:l,artifact_path_matches:l,",
            "layout_ok:l,missing_local_ref:l,bad_size:l,bad_checksum:l,",
            "artifact_mismatch:l,layout_mismatch:l,issue:c"
        )))
    }

    if (nrow(esg_files)) {
        catalog <- data.table::copy(catalog)
        idx <- match(catalog$file_key, esg_files$file_key)
        for (col in intersect(
            c("local_path", "local_artifact_id"),
            names(esg_files)
        )) {
            fill <- !is.na(idx) &
                (is.na(catalog[[col]]) | !nzchar(catalog[[col]]))
            if (any(fill)) {
                data.table::set(
                    catalog,
                    i = which(fill),
                    j = col,
                    value = esg_files[[col]][idx[fill]]
                )
            }
        }
    }

    local_path <- store__chr(catalog$local_path)
    actual_path <- vapply(
        local_path,
        function(path) {
            if (is.na(path) || !nzchar(path)) {
                return(NA_character_)
            }
            store_abs_path(path, root = private$store_path)
        },
        character(1L)
    )
    exists <- !is.na(actual_path) & file.exists(actual_path)
    size_actual <- rep(NA_real_, nrow(catalog))
    if (any(exists)) {
        size_actual[exists] <- as.numeric(
            file.info(actual_path[exists], extra_cols = FALSE)$size
        )
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
        check_hash <- exists &
            !is.na(checksum_expected) &
            nzchar(checksum_expected) &
            !is.na(checksum_type)
        if (any(check_hash)) {
            checksum_actual[check_hash] <- mapply(
                store_hash_file,
                actual_path[check_hash],
                checksum_type[check_hash],
                USE.NAMES = FALSE
            )
            checksum_ok[check_hash] <- tolower(checksum_actual[check_hash]) ==
                tolower(checksum_expected[check_hash])
        }
    }

    expected_path <- rep(NA_character_, nrow(catalog))
    expected_rel_path <- rep(NA_character_, nrow(catalog))
    if (isTRUE(layout)) {
        plan <- store_download__apply_layout(
            private,
            store__download_plan_rows(catalog),
            catalog
        )
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
    check_layout <- isTRUE(layout) &
        exists &
        !is.na(expected_path) &
        nzchar(expected_path)
    if (any(check_layout)) {
        layout_ok[check_layout] <- store_normalize_path(actual_path[
            check_layout
        ]) ==
            store_normalize_path(expected_path[check_layout])
    }

    artifact_id <- store__chr(catalog$local_artifact_id)
    artifact_idx <- match(artifact_id, artifacts$artifact_id)
    artifact_record_exists <- !is.na(artifact_id) & !is.na(artifact_idx)
    artifact_relative_path <- rep(NA_character_, nrow(catalog))
    artifact_path <- rep(NA_character_, nrow(catalog))
    artifact_path_exists <- rep(NA, nrow(catalog))
    if (any(artifact_record_exists)) {
        artifact_relative_path[artifact_record_exists] <-
            artifacts$relative_path[artifact_idx[artifact_record_exists]]
        artifact_path[artifact_record_exists] <- vapply(
            artifact_relative_path[artifact_record_exists],
            store_abs_path,
            character(1L),
            root = private$store_path
        )
        artifact_path_exists[artifact_record_exists] <- file.exists(
            artifact_path[artifact_record_exists]
        )
    }
    artifact_path_matches <- rep(NA, nrow(catalog))
    check_artifact_path <- artifact_record_exists &
        !is.na(local_path) &
        nzchar(local_path)
    artifact_path_matches[check_artifact_path] <-
        artifact_relative_path[check_artifact_path] ==
            local_path[check_artifact_path]

    missing_local_ref <- !is.na(local_path) & nzchar(local_path) & !exists
    bad_size <- size_ok %in% FALSE
    bad_checksum <- checksum_ok %in% FALSE
    artifact_mismatch <- (!is.na(artifact_id) &
        nzchar(artifact_id) &
        !artifact_record_exists) |
        (artifact_path_matches %in% FALSE)
    layout_mismatch <- layout_ok %in% FALSE
    issue <- store_download__validate_issue_vector(
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
}
# }}}

# store_download__validate_artifact_rows {{{
store_download__validate_artifact_rows <- function(
    private,
    artifacts,
    catalog,
    esg_files
) {
    artifacts <- data.table::as.data.table(artifacts)
    if (!nrow(artifacts)) {
        return(store__empty_table(paste0(
            "artifact_id:c,file_key:c,query_id:c,relative_path:c,path:c,exists:l,",
            "size:n,checksum:c,checksum_type:c,referenced:l,file_key_known:l,",
            "path_matches:l,issue:c"
        )))
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
    refs <- if (length(refs)) {
        data.table::rbindlist(refs, fill = TRUE)
    } else {
        data.table::data.table()
    }
    if (nrow(refs)) {
        refs <- refs[!is.na(refs$artifact_id) & nzchar(refs$artifact_id)]
    }

    path <- vapply(
        artifacts$relative_path,
        store_abs_path,
        character(1L),
        root = private$store_path
    )
    exists <- file.exists(path)
    referenced <- if (nrow(refs)) {
        artifacts$artifact_id %in% refs$artifact_id
    } else {
        rep(FALSE, nrow(artifacts))
    }
    file_key_known <- artifacts$file_key %in%
        unique(c(catalog$file_key, esg_files$file_key))
    path_matches <- rep(NA, nrow(artifacts))
    if (nrow(refs)) {
        ref_split <- split(refs, refs$artifact_id)
        for (i in seq_len(nrow(artifacts))) {
            ref <- ref_split[[artifacts$artifact_id[[i]]]]
            if (is.null(ref) || !nrow(ref)) {
                next
            }
            ref_path <- ref$relative_path[
                !is.na(ref$relative_path) & nzchar(ref$relative_path)
            ]
            if (length(ref_path)) {
                path_matches[[i]] <- any(
                    ref_path == artifacts$relative_path[[i]]
                )
            }
        }
    }

    issue <- store_download__validate_issue_vector(
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
}
# }}}

# store_download__validate_actions {{{
store_download__validate_actions <- function(files, artifacts) {
    actions <- list()
    files <- data.table::as.data.table(files)
    artifacts <- data.table::as.data.table(artifacts)
    make_action <- function(
        action,
        file_key,
        artifact_id,
        from_path,
        to_path,
        relative_path,
        reason
    ) {
        data.table::data.table(
            action = action,
            file_key = file_key,
            artifact_id = artifact_id,
            from_path = from_path,
            to_path = to_path,
            relative_path = relative_path,
            reason = reason,
            safe = TRUE
        )
    }

    if (nrow(files)) {
        missing <- files[files[["missing_local_ref"]] %in% TRUE]
        if (nrow(missing)) {
            actions$clear_missing <- make_action(
                "clear_missing_local_ref",
                missing$file_key,
                missing$local_artifact_id,
                missing$actual_path,
                NA_character_,
                missing$local_path,
                "registered local file is missing"
            )
        }
        move <- files[
            files[["layout_mismatch"]] %in%
                TRUE &
                files[["exists"]] %in% TRUE &
                !is.na(files$expected_path) &
                nzchar(files$expected_path)
        ]
        if (nrow(move)) {
            move <- move[!file.exists(move$expected_path)]
        }
        if (nrow(move)) {
            actions$move_to_layout <- make_action(
                "move_to_layout",
                move$file_key,
                move$local_artifact_id,
                move$actual_path,
                move$expected_path,
                move$expected_rel_path,
                "registered local file does not match current download layout"
            )
        }
    }

    if (nrow(artifacts)) {
        missing_artifact <- artifacts[artifacts[["exists"]] %in% FALSE]
        if (nrow(missing_artifact)) {
            actions$missing_artifact <- make_action(
                "remove_missing_artifact",
                missing_artifact$file_key,
                missing_artifact$artifact_id,
                missing_artifact$path,
                NA_character_,
                missing_artifact$relative_path,
                "artifact file is missing"
            )
        }
        orphan_artifact <- artifacts[artifacts[["referenced"]] %in% FALSE]
        if (nrow(orphan_artifact)) {
            actions$orphan_artifact <- make_action(
                "remove_orphan_artifact",
                orphan_artifact$file_key,
                orphan_artifact$artifact_id,
                orphan_artifact$path,
                NA_character_,
                orphan_artifact$relative_path,
                "artifact is not referenced by file records"
            )
        }
    }

    if (!length(actions)) {
        return(store__empty_table(
            "action_id:c,action:c,file_key:c,artifact_id:c,from_path:c,to_path:c,relative_path:c,reason:c,safe:l"
        ))
    }
    out <- unique(data.table::rbindlist(actions, fill = TRUE))
    action_ids <- vapply(
        seq_len(nrow(out)),
        function(i) {
            store__hash(
                out$action[[i]],
                out$file_key[[i]],
                out$artifact_id[[i]],
                out$from_path[[i]],
                out$to_path[[i]],
                out$reason[[i]]
            )
        },
        character(1L)
    )
    data.table::set(out, j = "action_id", value = action_ids)
    data.table::setcolorder(
        out,
        c(
            "action_id",
            "action",
            "file_key",
            "artifact_id",
            "from_path",
            "to_path",
            "relative_path",
            "reason",
            "safe"
        )
    )
    out[]
}
# }}}

# store_download__validate_issue_vector {{{
store_download__validate_issue_vector <- function(...) {
    flags <- list(...)
    n <- if (length(flags)) length(flags[[1L]]) else 0L
    if (!n) {
        return(character())
    }
    vapply(
        seq_len(n),
        function(i) {
            hit <- names(flags)[vapply(
                flags,
                function(x) x[[i]] %in% TRUE,
                logical(1L)
            )]
            if (!length(hit)) {
                NA_character_
            } else {
                paste(hit, collapse = ",")
            }
        },
        character(1L)
    )
}
# }}}

# store_download__repair_actions {{{
store_download__repair_actions <- function(private, actions, dry_run = TRUE) {
    actions <- data.table::as.data.table(actions)
    if (!nrow(actions)) {
        return(store__empty_table(
            "action_id:c,action:c,file_key:c,artifact_id:c,from_path:c,to_path:c,relative_path:c,done:l,dry_run:l,message:c"
        ))
    }
    required <- c(
        "action_id",
        "action",
        "file_key",
        "artifact_id",
        "from_path",
        "to_path",
        "relative_path",
        "reason",
        "safe"
    )
    missing <- setdiff(required, names(actions))
    if (length(missing)) {
        cli::cli_abort(
            "Repair action table is missing required column(s): {.field {missing}}."
        )
    }
    data.table::rbindlist(
        lapply(seq_len(nrow(actions)), function(i) {
            store_download__repair_action(
                private,
                actions[i],
                dry_run = dry_run
            )
        }),
        fill = TRUE
    )
}
# }}}

# store_download__repair_action {{{
store_download__repair_action <- function(private, action, dry_run = TRUE) {
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
            clear_missing_local_ref = store_download__repair_clear_missing_local_ref(
                private,
                action
            ),
            remove_missing_artifact = store_download__repair_remove_artifact_record(
                private,
                action
            ),
            remove_orphan_artifact = store_download__repair_remove_artifact_record(
                private,
                action
            ),
            move_to_layout = store_download__repair_move_to_layout(
                private,
                action
            ),
            list(
                done = FALSE,
                message = sprintf(
                    "unsupported action: %s",
                    action$action[[1L]]
                )
            )
        ),
        error = function(e) {
            list(done = FALSE, message = conditionMessage(e))
        }
    )
    base$done <- isTRUE(result$done)
    base$message <- store__chr1(result$message)
    base[]
}
# }}}

# store_download__repair_clear_missing_local_ref {{{
store_download__repair_clear_missing_local_ref <- function(private, action) {
    file_key <- store__chr1(action$file_key[[1L]])
    if (is.na(file_key) || !nzchar(file_key)) {
        return(list(done = FALSE, message = "missing file_key"))
    }
    store_download__update_file_local_ref(
        private,
        file_key,
        relative_path = NA_character_,
        artifact_id = NA_character_
    )
    list(done = TRUE, message = "cleared missing local reference")
}
# }}}

# store_download__repair_remove_artifact_record {{{
store_download__repair_remove_artifact_record <- function(private, action) {
    artifact_id <- store__chr1(action$artifact_id[[1L]])
    if (is.na(artifact_id) || !nzchar(artifact_id)) {
        return(list(done = FALSE, message = "missing artifact_id"))
    }
    private$delete_by_key("artifact", "artifact_id", artifact_id)
    store_download__clear_file_artifact_reference(private, artifact_id)
    list(done = TRUE, message = "removed artifact record")
}
# }}}

# store_download__repair_move_to_layout {{{
store_download__repair_move_to_layout <- function(private, action) {
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
    download_root <- paste0(
        sub("/+$", "", store_normalize_path(private$download_dir)),
        "/"
    )
    if (
        !startsWith(store_normalize_path(from_path), download_root) ||
            !startsWith(store_normalize_path(to_path), download_root)
    ) {
        return(list(
            done = FALSE,
            message = "paths are outside the store downloads directory"
        ))
    }
    dir.create(dirname(to_path), recursive = TRUE, showWarnings = FALSE)
    moved <- file.rename(from_path, to_path)
    if (!isTRUE(moved)) {
        moved <- file.copy(from_path, to_path, overwrite = FALSE) &&
            unlink(from_path, recursive = FALSE, force = TRUE) == 0L
    }
    if (!isTRUE(moved)) {
        return(list(done = FALSE, message = "failed to move file"))
    }
    if (is.na(relative_path) || !nzchar(relative_path)) {
        relative_path <- store_rel_path(to_path, root = private$store_path)
    }
    store_download__update_file_local_ref(
        private,
        file_key,
        relative_path = relative_path,
        artifact_id = artifact_id
    )
    if (!is.na(artifact_id) && nzchar(artifact_id)) {
        artifacts <- private$read_table("artifact")
        row <- artifacts[artifacts[["artifact_id"]] == artifact_id]
        if (nrow(row)) {
            row$relative_path <- relative_path
            row$updated_at <- store__now()
            private$replace_rows(
                "artifact",
                as.data.frame(row),
                "artifact_id"
            )
        }
    }
    store_download__remove_empty_dirs(private, dirname(from_path))
    list(done = TRUE, message = "moved file to current layout")
}
# }}}

# Shared updater for file_catalog and esg_file local path/artifact references.
# store_download__update_file_local_ref {{{
store_download__update_file_local_ref <- function(
    private,
    file_key,
    relative_path = NA_character_,
    artifact_id = NA_character_
) {
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
}
# }}}

# store_download__clear_file_artifact_reference {{{
store_download__clear_file_artifact_reference <- function(
    private,
    artifact_id
) {
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
}
# }}}

# store_download__remove_empty_dirs {{{
store_download__remove_empty_dirs <- function(private, path) {
    if (is.na(path) || !nzchar(path)) {
        return(invisible(NULL))
    }
    path <- store_normalize_path(path)
    root <- store_normalize_path(private$download_dir)
    root_prefix <- paste0(sub("/+$", "", root), "/")
    while (
        dir.exists(path) &&
            startsWith(path, root_prefix) &&
            !identical(path, root)
    ) {
        if (length(list.files(path, all.files = TRUE, no.. = TRUE))) {
            break
        }
        unlink(path, recursive = TRUE, force = FALSE)
        path <- dirname(path)
    }
    invisible(NULL)
}
# }}}

# store_download__build_storage_report {{{
store_download__build_storage_report <- function(private) {
    download_files <- store_download__list_store_files(
        private,
        private$download_dir
    )
    if (nrow(download_files)) {
        downloader_prefix <- paste0(
            file.path(private$download_dir, "_downloader"),
            "/"
        )
        download_files <- download_files[
            !startsWith(download_files$path, downloader_prefix)
        ]
    }
    tmp_files <- store_download__list_store_files(
        private,
        private$tmp_download_dir
    )
    registered <- store_download__registered_paths(private)
    registered_paths <- unique(registered$path[
        !is.na(registered$path) & nzchar(registered$path)
    ])
    registered_unique <- registered[
        !is.na(registered$path) & nzchar(registered$path)
    ]
    if (nrow(registered_unique)) {
        registered_unique <- registered_unique[
            !duplicated(registered_unique$path)
        ]
    }
    untracked <- if (nrow(download_files)) {
        download_files[!download_files$path %in% registered_paths]
    } else {
        download_files
    }
    missing <- if (nrow(registered)) {
        registered[
            is.na(registered$path) |
                !nzchar(registered$path) |
                !file.exists(registered$path)
        ]
    } else {
        registered
    }
    orphans <- store_catalog__orphaned_files(private)

    summary <- data.table::data.table(
        download_file_count = as.integer(nrow(download_files)),
        download_bytes = as.numeric(sum(download_files$size, na.rm = TRUE)),
        registered_file_count = as.integer(nrow(registered_unique)),
        registered_bytes = as.numeric(sum(
            registered_unique$size,
            na.rm = TRUE
        )),
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
}
# }}}

# store_download__list_store_files {{{
store_download__list_store_files <- function(private, root) {
    empty <- data.table::data.table(
        path = character(),
        relative_path = character(),
        size = numeric(),
        mtime = as.POSIXct(character())
    )
    if (!dir.exists(root)) {
        return(empty)
    }
    files <- list.files(
        root,
        recursive = TRUE,
        full.names = TRUE,
        all.files = TRUE,
        no.. = TRUE
    )
    if (!length(files)) {
        return(empty)
    }
    files <- files[file.exists(files) & !dir.exists(files)]
    if (!length(files)) {
        return(empty)
    }
    info <- file.info(files, extra_cols = FALSE)
    rel <- vapply(
        files,
        store_rel_path,
        character(1L),
        root = private$store_path
    )
    data.table::data.table(
        path = normalizePath(files, mustWork = FALSE, winslash = "/"),
        relative_path = rel,
        size = as.numeric(info$size),
        mtime = as.POSIXct(info$mtime, tz = "UTC")
    )
}
# }}}

# store_download__registered_paths {{{
store_download__registered_paths <- function(private) {
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
        catalog <- catalog[
            !is.na(catalog$local_path) & nzchar(catalog$local_path)
        ]
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
    abs_paths <- vapply(
        out$relative_path,
        function(path) {
            if (is.na(path) || !nzchar(path)) {
                return(NA_character_)
            }
            store_abs_path(path, root = private$store_path)
        },
        character(1L)
    )
    data.table::set(out, j = "path", value = abs_paths)
    data.table::set(
        out,
        j = "exists",
        value = !is.na(abs_paths) & file.exists(abs_paths)
    )
    out[]
}
# }}}

# store_download__cleanup_cutoff {{{
store_download__cleanup_cutoff <- function(older_than = NULL) {
    if (is.null(older_than)) {
        return(NULL)
    }
    if (inherits(older_than, "POSIXt")) {
        return(as.POSIXct(older_than[[1L]], tz = "UTC"))
    }
    checkmate::assert_number(older_than, lower = 0)
    as.POSIXct(Sys.time() - older_than, tz = "UTC")
}
# }}}

# store_download__cleanup_file_scope {{{
store_download__cleanup_file_scope <- function(
    scope,
    files,
    dry_run = TRUE,
    cutoff = NULL
) {
    files <- data.table::as.data.table(files)
    if (!is.null(cutoff) && "mtime" %in% names(files)) {
        files <- files[files[["mtime"]] <= cutoff]
    }
    if (!nrow(files)) {
        return(store_download__cleanup_empty())
    }
    deleted <- rep(FALSE, nrow(files))
    if (!isTRUE(dry_run)) {
        deleted <- file.exists(files$path) &
            unlink(files$path, recursive = FALSE, force = TRUE) == 0L
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
}
# }}}

# store_download__cleanup_orphan_records {{{
store_download__cleanup_orphan_records <- function(
    private,
    orphans,
    dry_run = TRUE
) {
    orphans <- data.table::as.data.table(orphans)
    if (!nrow(orphans)) {
        return(store_download__cleanup_empty())
    }
    removed <- rep(FALSE, nrow(orphans))
    deleted <- rep(FALSE, nrow(orphans))
    if (!isTRUE(dry_run)) {
        result <- store_catalog__remove_file_records(
            private,
            orphans$file_key,
            delete_local = TRUE,
            force = TRUE
        )
        removed <- orphans$file_key %in% result$file_key
        deleted <- result$deleted_local[match(
            orphans$file_key,
            result$file_key
        )] %in%
            TRUE
    }
    data.table::data.table(
        scope = "orphan_records",
        action = if (isTRUE(dry_run)) "remove_record" else "removed_record",
        path = orphans$local_file,
        relative_path = orphans$local_path,
        file_key = orphans$file_key,
        artifact_id = orphans$local_artifact_id,
        size = suppressWarnings(as.numeric(
            file.info(orphans$local_file, extra_cols = FALSE)$size
        )),
        deleted = deleted,
        record_removed = removed,
        dry_run = isTRUE(dry_run)
    )
}
# }}}

# store_download__cleanup_missing_records {{{
store_download__cleanup_missing_records <- function(
    private,
    records,
    dry_run = TRUE
) {
    records <- data.table::as.data.table(records)
    if (!nrow(records)) {
        return(store_download__cleanup_empty())
    }
    removed <- rep(FALSE, nrow(records))
    if (!isTRUE(dry_run)) {
        store_download__clear_missing_records(private, records)
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
}
# }}}

# store_download__clear_missing_records {{{
store_download__clear_missing_records <- function(private, records) {
    records <- data.table::as.data.table(records)
    artifact_ids <- unique(records$artifact_id[
        records$source == "artifact" &
            !is.na(records$artifact_id) &
            nzchar(records$artifact_id)
    ])
    if (length(artifact_ids)) {
        private$delete_by_key("artifact", "artifact_id", artifact_ids)
    }
    file_keys <- unique(records$file_key[
        records$source == "catalog" &
            !is.na(records$file_key) &
            nzchar(records$file_key)
    ])
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
}
# }}}

# store_download__cleanup_empty {{{
store_download__cleanup_empty <- function() {
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
    )
}
# }}}

# vim: fdm=marker :
