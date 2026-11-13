# Read source files without serializing a store connection into a worker.
# Payload publication is shared; plan status and Parquet persistence stay with
# the process that owns the manifest.
# store__read_job {{{
store__read_job <- function(job, source) {
    tryCatch(
        {
            resolved <- store__extract_cache_resolve(
                job$plan,
                job$file,
                function() {
                    if (is.null(source$opened)) {
                        source$opened <- store__open_dataset(
                            job$file$url_opendap[[1L]],
                            "OPeNDAP"
                        )
                    }
                    opened <- source$opened
                    payload <- store__read_extract_dataset(
                        opened$dataset,
                        job$plan,
                        job$file,
                        opened
                    )
                    opened$dataset <- NULL
                    list(
                        payload = payload,
                        opened = opened,
                        recovery_error = NULL
                    )
                }
            )
            # Transfer a cache reference instead of copying weather arrays through
            # IPC. The caller loads and persists one site's payload at a time.
            if (!identical(cache__mode(), "off")) {
                resolved$cache_path <- store__extract_cache_path(
                    job$plan,
                    job$file
                )
                resolved$payload <- NULL
            }
            resolved
        },
        error = base::identity
    )
}
# }}}

# Open a native source and classify access errors before any manifest write.
# store__open_dataset {{{
store__open_dataset <- function(target, service) {
    started_at <- proc.time()[["elapsed"]]
    ds <- NULL
    tryCatch(
        {
            ds <- EsgDataset$new(target)
            ds$open()
            list(
                dataset = ds,
                target = target,
                access_method = service
            )
        },
        error = function(error) {
            if (!is.null(ds) && isTRUE(ds$is_open)) {
                ds$close()
            }
            stop(store__access_error(
                error,
                phase = "open",
                service = service,
                target = target,
                started_at = started_at
            ))
        }
    )
}
# }}}

# Build the same native point payload in the caller or a source worker.
# store__read_extract_dataset {{{
store__read_extract_dataset <- function(
    ds,
    plan,
    file,
    opened,
    reporter = NULL
) {
    metadata_started <- proc.time()[["elapsed"]]
    time_info <- tryCatch(
        {
            value <- dataset__time_axis(ds, index = 1L)
            dataset__require_ordinary_time(value)
            valid <- value$values[!is.na(value$values)]
            if (!length(valid)) {
                stop(
                    "The NetCDF time axis is empty or unavailable.",
                    call. = FALSE
                )
            }
            list(info = value, valid = valid)
        },
        error = function(error) {
            stop(store__access_error(
                error,
                phase = "metadata",
                service = opened$access_method,
                target = opened$target,
                started_at = metadata_started
            ))
        }
    )
    requested_time <- c(plan$time_start[[1L]], plan$time_stop[[1L]])
    # Count the same calendar-native indices that read_region() will
    # extract; surrogate POSIXct years are wrong at 360-day boundaries.
    available_time_count <- length(cf_time__range_indices(
        time_info$info$values,
        time_info$info$coordinates,
        requested_time
    ))

    # Remote reads run in the existing one-shot dataset worker so the
    # main R process can refresh elapsed time and observe cancellation.
    callback <- if (is.null(reporter)) {
        NULL
    } else {
        function(progress) {
            reporter$heartbeat(
                details = list(
                    unit_type = "extraction_plan",
                    scenario = store__chr1(file$experiment_id[[1L]]),
                    variable = plan$variable_id[[1L]],
                    period = sprintf(
                        "%s/%s",
                        format(plan$time_start[[1L]], "%Y"),
                        format(plan$time_stop[[1L]], "%Y")
                    ),
                    access_method = opened$access_method,
                    transfer_state = shift_stage__coalesce(
                        progress$state,
                        "waiting"
                    )
                )
            )
            invisible(TRUE)
        }
    }
    dataset_private <- priv(ds)
    old_callback <- dataset_private$progress_callback
    dataset_private$progress_callback <- callback
    on.exit(dataset_private$progress_callback <- old_callback, add = TRUE)
    read_args <- list(
        variable = plan$variable_id[[1L]],
        lon = plan$lon[[1L]],
        lat = plan$lat[[1L]],
        time = requested_time,
        method = plan$method[[1L]]
    )
    use_async <- !is.null(reporter) &&
        identical(opened$access_method, "OPeNDAP")
    read_started <- proc.time()[["elapsed"]]
    dt <- tryCatch(
        tryCatch(
            do.call(ds$read_region, c(read_args, list(async = use_async))),
            epwshiftr_async_unavailable = function(error) {
                # A worker launch failure changes liveness only; the
                # same OPeNDAP read remains valid synchronously.
                reporter$notice(
                    paste(
                        "Worker unavailable; continuing with",
                        "synchronous OPeNDAP read"
                    ),
                    outcome = "fallback",
                    details = list(
                        unit_type = "extraction_plan",
                        scenario = store__chr1(
                            file$experiment_id[[1L]]
                        ),
                        variable = plan$variable_id[[1L]],
                        access_method = opened$access_method,
                        reason = conditionMessage(error)
                    )
                )
                do.call(ds$read_region, c(read_args, list(async = FALSE)))
            }
        ),
        error = function(error) {
            stop(store__access_error(
                error,
                phase = "read",
                service = opened$access_method,
                target = opened$target,
                started_at = read_started
            ))
        }
    )
    grid_sources <- attr(dt, "grid_sources", exact = TRUE)
    units <- tryCatch(
        as.character(ds$att_get(
            plan$variable_id[[1L]],
            "units",
            index = 1L
        ))[[1L]],
        error = function(error) NA_character_
    )
    data.table::set(dt, j = "units", value = units)
    list(
        data = dt,
        grid_sources = grid_sources,
        available_time_count = available_time_count,
        actual_start = min(time_info$valid),
        actual_end = max(time_info$valid)
    )
}
# }}}

# Keep at most the requested number of source tasks in flight. Workers own
# native handles only; collect() runs in the caller and may commit to its store.
# Stop dispatching on a fatal task error, drain already running work, and keep
# its completed cache entries available for recovery.
# source__apply {{{
source__apply <- function(
    jobs,
    read,
    collect,
    reporter = NULL,
    on_error = NULL
) {
    checkmate::assert_function(on_error, null.ok = TRUE)
    checkmate::assert_function(read)
    checkmate::assert_function(collect)
    workers <- shift_execution__options()$epwshiftr.mirai_workers
    checkmate::assert_count(workers, positive = TRUE)
    workers <- min(workers, length(jobs))
    if (!length(jobs)) {
        return(invisible(NULL))
    }
    # A visible workflow still needs one asynchronous reader so its caller can
    # refresh progress and cancel while the native request is waiting.
    if (workers == 1L && is.null(reporter)) {
        for (job in jobs) {
            if (!is.null(reporter)) {
                reporter$check_cancel()
            }
            value <- tryCatch(read(job), error = base::identity)
            if (inherits(value, "error")) {
                if (is.null(on_error)) {
                    stop(value)
                }
                on_error(job, value)
            } else {
                collect(job, value)
            }
        }
        return(invisible(NULL))
    }
    profile <- paste0(
        "epwshiftr-source-",
        Sys.getpid(),
        "-",
        basename(tempfile())
    )
    # One reader needs no dispatcher; retain it only for distributing work
    # between several independent source connections.
    mirai__start_pool(workers, dispatcher = workers > 1L, .compute = profile)
    on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
    library_paths <- shift_execution__library_paths()
    worker_options <- shift_execution__options()
    setup <- mirai::everywhere(
        {
            .libPaths(library_paths)
            loadNamespace("epwshiftr")
            options(worker_options)
            options(epwshiftr.progress = FALSE, epwshiftr.mirai_workers = 1L)
            data.table::setDTthreads(1L)
            TRUE
        },
        .args = list(
            library_paths = library_paths,
            worker_options = worker_options
        ),
        .compute = profile
    )
    setup <- mirai::collect_mirai(setup)
    if (!all(vapply(setup, isTRUE, logical(1L)))) {
        cli::cli_abort("Could not initialize the source-reading workers.")
    }
    tasks <- vector("list", workers)
    indices <- integer(workers)
    next_job <- 1L
    completed <- 0L
    failure <- NULL
    # Poll a bounded set of live tasks rather than queueing the entire catalog;
    # a reported failure therefore cannot trigger more remote work.
    while (completed < length(jobs)) {
        for (slot in seq_len(workers)) {
            task <- tasks[[slot]]
            if (!is.null(task) && !mirai::unresolved(task)) {
                value <- mirai::collect_mirai(task)
                error <- mirai_error_message(value)
                if (nzchar(error)) {
                    value <- simpleError(error)
                }
                # Collector failures must drain peers just like reader failures.
                # A file-isolation callback may record an expected source error
                # and permit independent files to continue.
                outcome <- tryCatch(
                    {
                        if (inherits(value, "error")) {
                            if (is.null(on_error)) {
                                stop(value)
                            }
                            on_error(jobs[[indices[[slot]]]], value)
                        } else {
                            collect(jobs[[indices[[slot]]]], value)
                        }
                        NULL
                    },
                    error = base::identity
                )
                if (inherits(outcome, "error") && is.null(failure)) {
                    failure <- outcome
                }
                tasks[slot] <- list(NULL)
                completed <- completed + 1L
            }
        }
        if (is.null(failure)) {
            for (slot in which(vapply(tasks, is.null, logical(1L)))) {
                if (next_job > length(jobs)) {
                    break
                }
                if (!is.null(reporter)) {
                    reporter$check_cancel()
                }
                indices[[slot]] <- next_job
                tasks[[slot]] <- mirai::mirai(
                    tryCatch(read(job), error = base::identity),
                    read = read,
                    job = jobs[[next_job]],
                    .compute = profile
                )
                next_job <- next_job + 1L
            }
        }
        if (all(vapply(tasks, is.null, logical(1L)))) {
            break
        }
        if (!is.null(reporter)) {
            reporter$check_cancel()
            reporter$heartbeat(
                details = list(
                    unit_type = "source_reads",
                    current = completed,
                    total = length(jobs),
                    active = sum(
                        indices > 0L &
                            !vapply(tasks, is.null, logical(1L))
                    )
                )
            )
        }
        Sys.sleep(0.02)
    }
    if (!is.null(failure)) {
        stop(failure)
    }
    invisible(NULL)
}
# }}}

# Preserve per-plan failures as results for the manifest owner. HTTP fallback
# occupies this same physical-source slot; peers remain independently pollable.
# store__read_task {{{
store__read_task <- function(job) {
    source <- new.env(parent = emptyenv())
    source$opened <- NULL
    on.exit(
        if (!is.null(source$opened)) source$opened$dataset$close(),
        add = TRUE
    )
    results <- vector("list", nrow(job$plans))
    failure <- NULL
    download_manifest <- NULL
    # A retained transfer is already on the HTTP recovery path. Resume its
    # exact session before repeating an OPeNDAP request that previously failed.
    if (!is.null(job$http) && file.exists(job$http$manifest)) {
        local <- tryCatch(store__pool_http_download(job), error = identity)
        if (inherits(local, "error")) {
            return(list(results = rep(list(local), nrow(job$plans))))
        }
        source$opened <- store__open_dataset(local$path, "HTTPServer")
        download_manifest <- local$manifest
    }
    for (index in seq_len(nrow(job$plans))) {
        if (is.null(failure)) {
            results[[index]] <- store__read_job(
                list(plan = job$plans[index], file = job$file),
                source
            )
            if (inherits(results[[index]], "error")) failure <- results[[index]]
        } else {
            results[[index]] <- failure
        }
    }
    if (
        !is.null(job$http) &&
            inherits(failure, "epwshiftr_store_access_error") &&
            identical(failure$service, "OPeNDAP")
    ) {
        remote_error <- failure
        if (!is.null(source$opened)) {
            source$opened$dataset$close()
            source$opened <- NULL
        }
        recovered <- tryCatch(
            {
                local <- store__pool_http_download(job)
                source$opened <- store__open_dataset(local$path, "HTTPServer")
                local
            },
            error = identity
        )
        if (inherits(recovered, "error")) {
            recovered <- store__fallback_error(remote_error, recovered)
            for (index in seq_along(results)) {
                if (inherits(results[[index]], "error")) {
                    results[[index]] <- recovered
                }
            }
        } else {
            download_manifest <- recovered$manifest
            # Successful earlier city payloads stay untouched. Only unresolved
            # plans are retried through the identical native CF reader.
            for (index in seq_along(results)) {
                if (inherits(results[[index]], "error")) {
                    results[[index]] <- store__read_job(
                        list(plan = job$plans[index], file = job$file),
                        source
                    )
                    if (!inherits(results[[index]], "error")) {
                        results[[index]]$recovery_error <- remote_error
                    }
                }
            }
        }
    }
    list(results = results, download_manifest = download_manifest)
}
# }}}

# Give each physical content identity its own recoverable Downloader manifest.
# Full native files may live outside a synchronized extraction store, and the
# SHA-256 directory key does not depend on a query, city or requested year.
store__pool_download_config <- function(file, directory, timeout) {
    fields <- intersect(
        c(
            "source_id",
            "experiment_id",
            "variant_label",
            "grid_label",
            "table_id",
            "variable_id",
            "title",
            "version",
            "tracking_id",
            "checksum_type",
            "checksum"
        ),
        names(file)
    )
    identity <- as.list(file[1L, fields, with = FALSE])
    key <- checksum_bytes(serialize(identity, NULL))
    directory <- file.path(directory, "_source_pool", key)
    list(
        dest = directory,
        temp = file.path(directory, ".tmp"),
        manifest = file.path(directory, "manifest.duckdb"),
        n_workers = 0L,
        retries = 1L,
        timeout = timeout,
        connect_timeout = 10L,
        # Extraction callers schedule their own retries. The download-only
        # manifest must not add a second hour-long host cooldown to that policy.
        node_policy = list(
            cooldown_after_failures = 3L,
            cooldown_seconds = 1L,
            min_attempts = 1L
        ),
        cleanup = FALSE
    )
}

# Reuse one exact persistent session, including its .part file after a worker
# deadline. This runs synchronously inside the reserved reader, with no nested
# reader pool and no access to the extraction manifest connection.
store__pool_http_download <- function(job) {
    started <- proc.time()[["elapsed"]]
    tryCatch(
        {
            config <- job$http
            dir.create(config$dest, recursive = TRUE, showWarnings = FALSE)
            release <- manifest_acquire_lock(
                file.path(config$dest, "source-transfer"),
                timeout = 5
            )
            on.exit(release(), add = TRUE)
            downloader <- do.call(Downloader$new, config)
            on.exit(priv(downloader)$disconnect_manifest(), add = TRUE)
            plan <- store__download_plan_rows(job$file)
            # Match the ordinary extraction fallback's supported checksum policy.
            checksum_type <- tolower(plan$checksum_type)
            unsupported <- is.na(checksum_type) |
                !checksum_type %in% c("md5", "sha256")
            data.table::set(
                plan,
                j = "checksum_type",
                value = data.table::fifelse(
                    unsupported,
                    "sha256",
                    checksum_type
                )
            )
            data.table::set(
                plan,
                i = which(unsupported),
                j = "checksum",
                value = NA_character_
            )
            tasks <- downloader$tasks()
            if (!nrow(tasks)) {
                session <- downloader$enqueue(
                    plan,
                    session_label = "source-pool-http-fallback"
                )
            } else {
                stopifnot(
                    nrow(tasks) == 1L,
                    tasks$logical_file_id[[1L]] == plan$logical_file_id[[1L]]
                )
                session <- tasks$session_id[[1L]]
            }
            tasks <- downloader$resume(
                session_id = session,
                progress = FALSE,
                overwrite = FALSE,
                resume = TRUE
            )
            stopifnot(nrow(tasks) == 1L)
            if (!tasks$status[[1L]] %in% c("done", "skipped")) {
                stop(
                    paste(
                        "HTTPServer download failed:",
                        tasks$last_error[[1L]]
                    ),
                    call. = FALSE
                )
            }
            verified <- downloader$verify(session_id = session)
            stopifnot(nrow(verified) == 1L, isTRUE(verified$checksum_ok[[1L]]))
            list(path = tasks$target_path[[1L]], manifest = config$manifest)
        },
        error = function(error) {
            stop(store__access_error(
                error,
                "download",
                "HTTPServer",
                store__chr1(job$file$url_download),
                started
            ))
        }
    )
}

# Import store-owned downloads normally. An external shared cache instead gets
# a small provenance artifact inside this store: its native bytes must not be
# copied into every city/year store or admitted as a store-relative artifact.
# The reader has already verified the Downloader checksum before returning.
store__sync_pool_download <- function(store, manifest) {
    downloader <- Downloader$new(manifest = manifest, n_workers = 0L)
    on.exit(priv(downloader)$disconnect_manifest(), add = TRUE)
    root <- store_normalize_path(store$path)
    tasks <- data.table::as.data.table(downloader$tasks(
        status = c("done", "skipped")
    ))
    if (!nrow(tasks)) {
        return(invisible(NULL))
    }
    paths <- vapply(tasks$target_path, store_normalize_path, character(1L))
    inside <- startsWith(paths, paste0(sub("/+$", "", root), "/"))
    if (all(inside)) {
        store$sync_downloads(downloader)
        return(invisible(NULL))
    }
    stopifnot(!any(inside), all(file.exists(paths)))
    catalog <- data.table::as.data.table(store$query(
        "SELECT * FROM file_catalog"
    ))
    for (i in seq_len(nrow(tasks))) {
        task <- tasks[i]
        key <- priv(store)$match_download_task(task, catalog)
        stopifnot(!is.na(key))
        file <- catalog[catalog[["file_key"]] == key]
        reference <- list(
            schema = "epwshiftr-external-download-reference-v1",
            file_key = key,
            logical_file_id = task$logical_file_id[[1L]],
            source_url = task$selected_url[[1L]],
            checksum_type = task$checksum_type[[1L]],
            checksum = task$checksum[[1L]],
            checksum_verified_by_reader = TRUE,
            bytes = as.double(file.info(paths[[i]])$size),
            cache_path = paths[[i]],
            cache_manifest = store_normalize_path(manifest)
        )
        path <- file.path(
            root,
            "sources",
            # Catalogue keys can contain ':'; hashing prevents Windows alternate
            # data streams and other path syntax while keeping the key in JSON.
            paste0("pool-download-", store__hash(key), ".json")
        )
        # Reuse identical evidence rather than rewriting a synchronized file.
        previous <- if (file.exists(path)) jsonlite::read_json(path) else NULL
        if (!isTRUE(all.equal(previous, reference, check.attributes = FALSE))) {
            store_write_json_atomic(
                reference,
                path,
                auto_unbox = TRUE,
                digits = NA,
                null = "null"
            )
        }
        store$register_artifact(
            kind = "source",
            path = path,
            role = "source",
            project = "CMIP6",
            query_id = file$query_id[[1L]],
            file_key = key,
            source_url = task$selected_url[[1L]],
            metadata = list(external_download_reference = TRUE)
        )
    }
    invisible(NULL)
}

# One batch task shares all of its sites and writes recoverable windows. No
# child manifest is opened in the source worker.
# source__read_acquisition {{{
source__read_acquisition <- function(job) {
    tryCatch(
        shift_batch_window__prefetch_acquisition(
            job$root,
            job$acquisition,
            job$consumers,
            cached = job$cached
        ),
        epwshiftr_shared_unavailable = function(error) {
            list(unavailable = error)
        },
        error = function(error) {
            attr(error, "shared_file") <- job$acquisition$filename[[1L]]
            stop(error)
        }
    )
}
# }}}

# Identify physical content independently of catalogue query IDs and replica
# hosts. Distinct versions/fields remain distinct; exact replicas share a key.
# source__physical_key {{{
source__physical_key <- function(file) {
    fields <- intersect(
        c(
            "source_id",
            "experiment_id",
            "variant_label",
            "grid_label",
            "table_id",
            "variable_id",
            "title",
            "version",
            "tracking_id",
            "checksum_type",
            "checksum"
        ),
        names(file)
    )
    identity <- as.list(file[1L, fields, with = FALSE])
    # Missing content and PID values do not identify a replica. Retain the
    # catalog key and available endpoints for weaker historical records.
    strong <- c("tracking_id", "checksum")
    if (
        !all(vapply(
            identity[strong],
            function(x) {
                length(x) == 1L && !is.na(x) && nzchar(x)
            },
            logical(1L)
        ))
    ) {
        fallback <- intersect(
            c("file_key", "url_opendap", "url_download"),
            names(file)
        )
        identity <- c(identity, as.list(file[1L, fallback, with = FALSE]))
    }
    fast_hash(identity)
}
# }}}

# vim: fdm=marker :
