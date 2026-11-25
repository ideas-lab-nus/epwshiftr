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
    download_receipt <- NULL
    # A retained transfer is already on the HTTP recovery path. Resume its
    # exact session before repeating an OPeNDAP request that previously failed.
    if (
        !is.null(job$http) &&
            any(file.exists(c(
                job$http$manifest,
                attr(job$http, "legacy_manifests", exact = TRUE)
            )))
    ) {
        local <- tryCatch(store__pool_http_download(job), error = identity)
        if (inherits(local, "error")) {
            return(list(results = rep(list(local), nrow(job$plans))))
        }
        source$opened <- store__open_dataset(local$path, "HTTPServer")
        download_manifest <- local$manifest
        download_receipt <- local$receipt
    }
    for (index in seq_len(nrow(job$plans))) {
        if (is.null(failure)) {
            results[[index]] <- store__read_job(
                list(
                    plan = job$plans[index],
                    file = if (is.null(job$files)) {
                        job$file
                    } else {
                        job$files[[index]]
                    }
                ),
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
            download_receipt <- recovered$receipt
            # Successful earlier city payloads stay untouched. Only unresolved
            # plans are retried through the identical native CF reader.
            for (index in seq_along(results)) {
                if (inherits(results[[index]], "error")) {
                    results[[index]] <- store__read_job(
                        list(
                            plan = job$plans[index],
                            file = if (is.null(job$files)) {
                                job$file
                            } else {
                                job$files[[index]]
                            }
                        ),
                        source
                    )
                    # Preserve both failures just as for download/open errors.
                    # A successful payload retains the recovered remote error for
                    # later publication diagnostics.
                    if (inherits(results[[index]], "error")) {
                        results[[index]] <- store__fallback_error(
                            remote_error,
                            results[[index]]
                        )
                    } else {
                        results[[index]]$recovery_error <- remote_error
                    }
                }
            }
        }
    }
    if (!is.null(download_receipt)) {
        for (index in seq_along(results)) {
            # Earlier successful DAP reads and cache hits retain their own
            # acquisition evidence when a later plan needs HTTP recovery.
            if (
                !inherits(results[[index]], "error") &&
                    identical(
                        results[[index]]$opened$access_method,
                        "HTTPServer"
                    )
            ) {
                results[[
                    index
                ]]$download_receipt <- source_recovery__receipt_for(
                    if (is.null(job$files)) job$file else job$files[[index]],
                    download_receipt
                )
            }
        }
    }
    list(
        results = results,
        download_manifest = download_manifest,
        download_receipt = download_receipt
    )
}
# }}}

# Give each physical content identity its own recoverable Downloader manifest.
# Full native files may live outside a synchronized extraction store, and the
# SHA-256 directory key does not depend on a query, city or requested year.
store__pool_download_config <- function(file, directory, timeout) {
    key <- source_recovery__key(source_recovery__describe(file))
    directory <- file.path(directory, "_source_pool", key)
    config <- list(
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
    # Inspect the former physical-identity layout only when recovering this
    # request. Its tasks and candidate addresses still require owner validation.
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
    legacy_key <- checksum_bytes(serialize(
        as.list(file[1L, fields, with = FALSE]),
        NULL
    ))
    attr(config, "legacy_manifests") <- file.path(
        dirname(directory),
        legacy_key,
        "manifest.duckdb"
    )
    config
}

# Reuse one exact persistent session, including its .part file after a worker
# deadline. This runs synchronously inside the reserved reader, with no nested
# reader pool and no access to the extraction manifest connection.
store__pool_http_download <- function(job) {
    started <- proc.time()[["elapsed"]]
    tryCatch(
        source_recovery__download(job$file, job$http),
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
# The exact reader receipt separates publisher verification from local hashes.
store__sync_pool_download <- function(
    store,
    manifest,
    receipt = NULL,
    file = NULL
) {
    if (is.null(receipt)) {
        # Old internal idempotent calls may recheck a previously published
        # reference, but may not import every task from an arbitrary manifest.
        references <- list.files(
            file.path(store$path, "sources"),
            "^pool-download-.*[.]json$",
            full.names = TRUE
        )
        known <- vapply(
            references,
            function(path) {
                identical(
                    jsonlite::read_json(path)$cache_manifest,
                    store_normalize_path(manifest)
                )
            },
            logical(1L)
        )
        if (any(known)) {
            return(invisible(NULL))
        }
        stop(
            "An exact source recovery receipt is required for Store registration.",
            call. = FALSE
        )
    }
    source_recovery__register(store, file, receipt)
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
# hosts when publisher content evidence verifies replicas. Weak records keep
# their catalogue record and complete HTTP endpoint in this same key.
# source__physical_key {{{
source__physical_key <- function(file) {
    source_recovery__key(source_recovery__describe(file))
}
# }}}

# vim: fdm=marker :
