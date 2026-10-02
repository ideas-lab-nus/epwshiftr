# Divide one physical file's native axis at calendar-month boundaries. The
# next month's first instant closes the preceding window, which also includes
# day 30 of February in a 360-day calendar without inventing a POSIX date.
shift_batch__windows <- function(axis, acquisition, consumer_count) {
    checkmate::assert_count(consumer_count, positive = TRUE)
    selected <- cf_time__range_indices(
        axis$values,
        axis$coordinates,
        c(acquisition$time_start[[1L]], acquisition$time_stop[[1L]])
    )
    if (!length(selected)) {
        return(data.table::data.table())
    }
    coordinates <- axis$coordinates[selected, , drop = FALSE]
    month <- as.integer(coordinates$cf_year) *
        12L +
        as.integer(coordinates$cf_month) -
        1L
    first <- which(!duplicated(month))
    last <- c(first[-1L] - 1L, length(selected))
    # Four distinct source cells per consumer is the worst supported spatial
    # method. Bound both output rows and the reader's 4096 source requests.
    worst_points <- 4L * consumer_count
    if (worst_points > 4096L) {
        stop(
            "The acquisition exceeds the bounded source-cell limit; split the site collection.",
            call. = FALSE
        )
    }
    max_steps <- max(
        1L,
        min(
            2048L,
            200000L %/% consumer_count,
            max(1L, 250000L %/% worst_points) *
                max(1L, 4096L %/% worst_points)
        )
    )
    if (any(last - first + 1L > max_steps)) {
        stop(
            "One native month exceeds the bounded multi-site read; split the site collection.",
            call. = FALSE
        )
    }
    group <- integer(length(first))
    group_index <- 1L
    used <- 0L
    for (index in seq_along(first)) {
        count <- last[[index]] - first[[index]] + 1L
        if (used && used + count > max_steps) {
            group_index <- group_index + 1L
            used <- 0L
        }
        group[[index]] <- group_index
        used <- used + count
    }
    group_first <- which(!duplicated(group))
    group_last <- c(group_first[-1L] - 1L, length(group))
    start_month <- month[first[group_first]]
    next_month <- month[last[group_last]] + 1L
    start <- as.POSIXct(
        sprintf(
            "%04d-%02d-01",
            start_month %/% 12L,
            start_month %% 12L + 1L
        ),
        tz = "UTC"
    )
    stop <- as.POSIXct(
        sprintf(
            "%04d-%02d-01",
            next_month %/% 12L,
            next_month %% 12L + 1L
        ),
        tz = "UTC"
    )
    # Adjacent closed windows assign their shared boundary to the earlier
    # window. This prevents duplicate native timestamps after recovery.
    if (length(start) > 1L) {
        start[-1L] <- start[-1L] + 1
    }
    start <- pmax(start, acquisition$time_start[[1L]])
    stop <- pmin(stop, acquisition$time_stop[[1L]])
    windows <- data.table::data.table(
        time_start = start,
        time_stop = stop,
        first_index = selected[first[group_first]],
        last_index = selected[last[group_last]]
    )
    data.table::set(
        windows,
        j = "window_id",
        value = vapply(
            seq_len(nrow(windows)),
            function(index) {
                store__hash(
                    acquisition$acquisition_id[[1L]],
                    windows$first_index[[index]],
                    windows$last_index[[index]]
                )
            },
            character(1L)
        )
    )
    windows
}

# Keep each completed native window behind a SHA-256 receipt. A missing receipt
# is an interrupted write, while a checksum mismatch is an error that must not
# silently replace evidence or contaminate child extraction caches.
shift_batch__window_read <- function(path, identity, demand_ids) {
    receipt_path <- paste0(path, ".json")
    if (!file.exists(receipt_path)) {
        return(NULL)
    }
    receipt <- tryCatch(
        jsonlite::read_json(receipt_path, simplifyVector = FALSE),
        error = base::identity
    )
    if (
        inherits(receipt, "error") ||
            !is.list(receipt) ||
            !identical(receipt$identity, identity) ||
            !dir.exists(path) ||
            !is.list(receipt$chunks) ||
            anyDuplicated(vapply(
                receipt$chunks,
                `[[`,
                character(1L),
                "demand_id"
            )) >
                0L
    ) {
        cli::cli_abort(
            "A shared acquisition window has an invalid receipt or checksum.",
            class = "epwshiftr_shared_cache_error"
        )
    }
    receipt_ids <- vapply(
        receipt$chunks,
        `[[`,
        character(1L),
        "demand_id"
    )
    # A complete receipt for a larger group can serve a smaller set of
    # uncached consumers. Do not hash unrelated chunks on that recovery path.
    if (!all(as.character(demand_ids) %in% receipt_ids)) {
        return(NULL)
    }
    selected <- receipt$chunks[match(as.character(demand_ids), receipt_ids)]
    chunks <- stats::setNames(
        vapply(
            selected,
            function(chunk) {
                expected <- paste0(
                    store__hash(
                        "shared-consumer-chunk-v1",
                        identity,
                        chunk$demand_id
                    ),
                    ".rds"
                )
                file <- file.path(path, expected)
                if (
                    !identical(chunk$file, expected) ||
                        !file.exists(file) ||
                        !identical(
                            chunk$sha256,
                            store_hash_file(file, "sha256")
                        )
                ) {
                    cli::cli_abort(
                        "A shared acquisition window has an invalid receipt or checksum.",
                        class = "epwshiftr_shared_cache_error"
                    )
                }
                file
            },
            character(1L)
        ),
        as.character(demand_ids)
    )
    chunks
}

# Split one bounded multi-site result once, then publish small per-consumer
# chunks. The receipt is written last so interrupted windows cannot seed any
# child cache, and reconstruction needs only one site's chunks in memory.
shift_batch__window_write <- function(path, identity, values, demand_ids) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(path)) {
        # Preserve an interrupted publication for diagnosis. Completed
        # windows are verified and returned before this writer is called.
        interrupted <- tempfile(
            pattern = paste0(basename(path), "-interrupted-"),
            tmpdir = dirname(path)
        )
        if (!file.rename(path, interrupted)) {
            stop(
                "Could not preserve an interrupted shared window.",
                call. = FALSE
            )
        }
        receipt_path <- paste0(path, ".json")
        if (
            file.exists(receipt_path) &&
                !file.rename(receipt_path, paste0(interrupted, ".json"))
        ) {
            stop(
                "Could not preserve the previous shared receipt.",
                call. = FALSE
            )
        }
    }
    dir.create(path)
    value_groups <- split(values, by = "demand_id", keep.by = TRUE)
    sources <- attr(values, "grid_sources", exact = TRUE)
    source_groups <- split(sources, by = "demand_id", keep.by = TRUE)
    chunks <- lapply(as.character(demand_ids), function(demand_id) {
        filename <- paste0(
            store__hash(
                "shared-consumer-chunk-v1",
                identity,
                demand_id
            ),
            ".rds"
        )
        target <- file.path(path, filename)
        temporary <- tempfile(pattern = "chunk-", tmpdir = path)
        on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
        payload <- list(
            data = shift_coalesce(value_groups[[demand_id]], values[0L]),
            grid_sources = shift_coalesce(
                source_groups[[demand_id]],
                sources[0L]
            )
        )
        saveRDS(payload, temporary, version = 3L, compress = "gzip")
        sha <- store_hash_file(temporary, "sha256")
        if (!file.rename(temporary, target)) {
            stop("Could not publish a shared consumer chunk.", call. = FALSE)
        }
        list(demand_id = demand_id, file = filename, sha256 = sha)
    })
    receipt_path <- paste0(path, ".json")
    receipt_tmp <- tempfile(pattern = "receipt-", tmpdir = dirname(path))
    on.exit(if (file.exists(receipt_tmp)) unlink(receipt_tmp), add = TRUE)
    jsonlite::write_json(
        list(identity = identity, chunks = chunks),
        receipt_tmp,
        auto_unbox = TRUE
    )
    if (!file.rename(receipt_tmp, receipt_path)) {
        stop("Could not publish the shared acquisition receipt.", call. = FALSE)
    }
    stats::setNames(
        file.path(path, vapply(chunks, `[[`, character(1L), "file")),
        vapply(chunks, `[[`, character(1L), "demand_id")
    )
}

# Persist the small native-axis facts needed to assemble verified windows when
# the source service is temporarily unavailable. The identity and content hash
# prevent a different consumer group from reusing these counts or boundaries.
shift_batch__source_metadata <- function(
    axis,
    acquisition,
    consumers,
    identity,
    units
) {
    requested <- unique(consumers[,
        c("requested_start", "requested_stop"),
        with = FALSE
    ])
    counts <- vapply(
        seq_len(nrow(requested)),
        function(index) {
            length(cf_time__range_indices(
                axis$values,
                axis$coordinates,
                c(
                    requested$requested_start[[index]],
                    requested$requested_stop[[index]]
                )
            ))
        },
        integer(1L)
    )
    match_index <- match(
        paste(consumers$requested_start, consumers$requested_stop),
        paste(requested$requested_start, requested$requested_stop)
    )
    list(
        identity = identity,
        windows = shift_batch__windows(axis, acquisition, nrow(consumers)),
        units = units,
        available_counts = stats::setNames(
            counts[match_index],
            as.character(consumers$demand_id)
        ),
        actual_start = min(axis$values, na.rm = TRUE),
        actual_end = max(axis$values, na.rm = TRUE)
    )
}

# Read a group's metadata only when its complete content still matches the
# saved hash; a missing manifest merely requires opening the original source.
shift_batch__metadata_read <- function(path, identity, demand_ids) {
    if (!file.exists(path)) {
        return(NULL)
    }
    record <- tryCatch(readRDS(path), error = base::identity)
    if (
        inherits(record, "error") ||
            !is.list(record) ||
            !identical(record$identity, identity) ||
            !is.list(record$data) ||
            !identical(record$sha256, store__hash(record$data)) ||
            !data.table::is.data.table(record$data$windows) ||
            !all(
                as.character(demand_ids) %in%
                    names(record$data$available_counts)
            )
    ) {
        cli::cli_abort(
            "A shared acquisition has invalid source metadata.",
            class = "epwshiftr_shared_cache_error"
        )
    }
    record$data
}

# Atomically publish the native-axis summary before any source-value window.
# Existing valid metadata is left untouched across interrupted attempts.
shift_batch__metadata_write <- function(path, identity, data) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(pattern = "source-metadata-", tmpdir = dirname(path))
    on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
    saveRDS(
        list(identity = identity, data = data, sha256 = store__hash(data)),
        temporary,
        version = 3L,
        compress = "gzip"
    )
    if (!file.rename(temporary, path)) {
        stop("Could not publish shared source metadata.", call. = FALSE)
    }
    invisible(path)
}

# Materialize one consumer in the existing site-extraction cache format. Child
# stores can then use their ordinary extraction task, provenance and resume
# logic without knowing that another site shared the source read.
shift_batch__seed_consumer <- function(
    acquisition,
    consumer,
    pieces,
    metadata
) {
    wanted_id <- as.character(consumer$demand_id[[1L]])
    pieces <- Filter(
        function(piece) {
            !is.null(piece) && !is.null(piece[[wanted_id]])
        },
        pieces
    )
    if (!length(pieces)) {
        stop(
            "No shared windows cover the consumer's requested time.",
            call. = FALSE
        )
    }
    chunks <- lapply(pieces, function(piece) {
        payload <- tryCatch(
            readRDS(piece[[wanted_id]]),
            error = base::identity
        )
        if (
            inherits(payload, "error") ||
                !is.list(payload) ||
                !data.table::is.data.table(payload$data) ||
                !data.table::is.data.table(payload$grid_sources)
        ) {
            cli::cli_abort(
                "A shared consumer chunk has an invalid payload.",
                class = "epwshiftr_shared_cache_error"
            )
        }
        payload
    })
    values <- data.table::rbindlist(
        lapply(chunks, `[[`, "data"),
        use.names = TRUE
    )
    if (anyDuplicated(values$time)) {
        cli::cli_abort(
            "A shared acquisition has duplicate native times for one consumer.",
            class = "epwshiftr_shared_cache_error"
        )
    }
    data.table::setorderv(values, "time")
    columns <- setdiff(
        names(values),
        c("consumer_id", "site_id", "demand_id", "child_key", "role")
    )
    values <- data.table::copy(values[, columns, with = FALSE])
    data.table::set(values, j = "units", value = metadata$units)
    sources <- unique(data.table::rbindlist(
        lapply(chunks, `[[`, "grid_sources"),
        use.names = TRUE
    ))
    source_columns <- setdiff(
        names(sources),
        c("consumer_id", "site_id", "demand_id", "child_key")
    )
    sources <- data.table::copy(sources[, source_columns, with = FALSE])
    requested <- c(
        consumer$requested_start[[1L]],
        consumer$requested_stop[[1L]]
    )
    payload <- list(
        data = values,
        grid_sources = sources,
        available_time_count = unname(metadata$available_counts[[wanted_id]]),
        actual_start = metadata$actual_start,
        actual_end = metadata$actual_end
    )
    plan <- data.table::data.table(
        variable_id = consumer$variable_id[[1L]],
        lon = consumer$lon[[1L]],
        lat = consumer$lat[[1L]],
        method = consumer$spatial_method[[1L]],
        time_start = requested[[1L]],
        time_stop = requested[[2L]]
    )
    path <- store__extract_cache_path(plan, acquisition)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    manifest_with_lock(
        path,
        {
            if (is.null(store__extract_cache_read(path))) {
                store__extract_cache_write(path, payload)
            }
        },
        timeout = 86400
    )
    invisible(path)
}

# Resolve only the uncached consumers in every completed window. A missing
# receipt means a source read is still needed; an altered chunk remains an
# error instead of silently being regenerated.
shift_batch__window_pieces <- function(
    directory,
    identity,
    windows,
    consumers,
    cached
) {
    pieces <- vector("list", nrow(windows))
    complete <- TRUE
    for (index in seq_len(nrow(windows))) {
        window <- windows[index]
        active <- consumers[
            !cached &
                consumers$time_start <= window$time_stop[[1L]] &
                consumers$time_stop >= window$time_start[[1L]]
        ]
        if (!nrow(active)) {
            next
        }
        path <- file.path(directory, window$window_id[[1L]])
        piece <- manifest_with_lock(
            path,
            shift_batch__window_read(
                path,
                store__hash(identity, window$window_id[[1L]]),
                active$demand_id
            ),
            timeout = 86400
        )
        if (is.null(piece)) {
            complete <- FALSE
        }
        pieces[index] <- list(piece)
    }
    list(pieces = pieces, complete = complete)
}

# Rebuild only missing site caches after the required window chunks are
# verified, keeping a single consumer's data in memory at any one time.
shift_batch__seed_pending <- function(
    acquisition,
    consumers,
    cached,
    pieces,
    metadata
) {
    for (index in which(!cached)) {
        shift_batch__seed_consumer(
            acquisition,
            consumers[index],
            pieces,
            metadata
        )
    }
    invisible(NULL)
}

# Open each physical file once, resume verified windows, and publish complete
# per-site payloads only after every native window succeeds. Failed reads leave
# earlier window receipts intact for the next batch resume.
shift_batch__prefetch_acquisition <- function(
    batch_root,
    acquisition,
    consumers,
    source = NULL
) {
    if (is.null(source)) {
        source <- new.env(parent = emptyenv())
        source$dataset <- NULL
        on.exit(
            if (!is.null(source$dataset)) {
                try(source$dataset$close(), silent = TRUE)
            },
            add = TRUE
        )
    }
    cache_paths <- vapply(
        seq_len(nrow(consumers)),
        function(index) {
            consumer <- consumers[index]
            plan <- data.table::data.table(
                variable_id = consumer$variable_id[[1L]],
                lon = consumer$lon[[1L]],
                lat = consumer$lat[[1L]],
                method = consumer$spatial_method[[1L]],
                time_start = consumer$requested_start[[1L]],
                time_stop = consumer$requested_stop[[1L]]
            )
            store__extract_cache_path(plan, acquisition)
        },
        character(1L)
    )
    # Several weather methods can ask for the same site, source and native
    # period. Their ordinary child cache key is identical, so read it once.
    keep <- !duplicated(cache_paths)
    consumers <- consumers[keep]
    cache_paths <- cache_paths[keep]
    cached <- vapply(
        cache_paths,
        function(path) !is.null(store__extract_cache_read(path)),
        logical(1L)
    )
    if (all(cached)) {
        return(invisible(0L))
    }
    # Split before opening the source. Recursive groups share this one lazy
    # connection, and verified groups need no connection at all.
    if (nrow(consumers) > 256L) {
        first <- seq.int(1L, nrow(consumers), by = 256L)
        windows <- integer(length(first))
        for (index in seq_along(first)) {
            rows <- seq.int(
                first[[index]],
                min(first[[index]] + 255L, nrow(consumers))
            )
            windows[[index]] <- shift_batch__prefetch_acquisition(
                batch_root,
                acquisition,
                consumers[rows],
                source
            )
        }
        return(invisible(sum(windows)))
    }
    source_identity <- store__hash(
        "shared-window-v1",
        acquisition$physical_file_id[[1L]],
        as.list(consumers)
    )
    directory <- file.path(
        batch_root,
        "shared-acquisitions",
        acquisition$acquisition_id[[1L]],
        source_identity
    )
    metadata_path <- file.path(directory, "source-metadata.rds")
    metadata <- shift_batch__metadata_read(
        metadata_path,
        source_identity,
        consumers$demand_id
    )
    restored <- NULL
    if (!is.null(metadata)) {
        if (!nrow(metadata$windows)) {
            return(invisible(0L))
        }
        # Completed windows can seed missing site caches without reconnecting
        # to a source whose transport is currently unavailable.
        restored <- shift_batch__window_pieces(
            directory,
            source_identity,
            metadata$windows,
            consumers,
            cached
        )
        if (restored$complete) {
            shift_batch__seed_pending(
                acquisition,
                consumers,
                cached,
                restored$pieces,
                metadata
            )
            return(invisible(nrow(metadata$windows)))
        }
    }
    if (is.null(source$dataset)) {
        checksum <- store__chr1(acquisition$checksum)
        checksum_type <- tolower(store__chr1(acquisition$checksum_type))
        if (
            is.na(checksum) ||
                !nzchar(checksum) ||
                is.na(checksum_type) ||
                !checksum_type %in% c("md5", "sha256")
        ) {
            cli::cli_abort(
                "Shared reads require a cataloged source checksum; child extraction remains available.",
                class = "epwshiftr_shared_unavailable"
            )
        }
        endpoint <- acquisition$url_opendap[[1L]]
        if (is.na(endpoint) || !nzchar(endpoint)) {
            local <- acquisition$url_download[[1L]]
            if (is.na(local) || !file.exists(local)) {
                cli::cli_abort(
                    "Shared reads require an OPeNDAP endpoint or a local file; child extraction remains available.",
                    class = "epwshiftr_shared_unavailable"
                )
            }
            endpoint <- local
        }
        checkmate::assert_string(endpoint, min.chars = 1L)
        if (identical(cache__mode(), "offline") && !file.exists(endpoint)) {
            cli::cli_abort(
                "Offline shared reads require a local source file.",
                class = "epwshiftr_shared_unavailable"
            )
        }
        if (
            file.exists(endpoint) &&
                !identical(
                    tolower(store_hash_file(endpoint, checksum_type)),
                    tolower(checksum)
                )
        ) {
            cli::cli_abort(
                "The local source file does not match its catalog checksum.",
                class = "epwshiftr_shared_cache_error"
            )
        }
        dataset <- EsgDataset$new(endpoint)
        opened <- tryCatch(dataset$open(), error = base::identity)
        if (inherits(opened, "error")) {
            try(dataset$close(), silent = TRUE)
            stop(opened)
        }
        source$dataset <- dataset
        source$axis <- source$dataset$get_time_axis(index = 1L)
        source$units <- tryCatch(
            as.character(source$dataset$att_get(
                consumers$variable_id[[1L]],
                "units",
                index = 1L
            ))[[1L]],
            error = function(error) NA_character_
        )
    }
    dataset <- source$dataset
    axis <- source$axis
    if (is.null(metadata)) {
        metadata <- shift_batch__source_metadata(
            axis,
            acquisition,
            consumers,
            source_identity,
            source$units
        )
        shift_batch__metadata_write(metadata_path, source_identity, metadata)
    } else {
        observed <- shift_batch__source_metadata(
            axis,
            acquisition,
            consumers,
            source_identity,
            source$units
        )
        if (
            !identical(
                as.list(metadata$windows),
                as.list(observed$windows)
            ) ||
                !identical(
                    metadata$available_counts,
                    observed$available_counts
                ) ||
                !identical(metadata$units, observed$units) ||
                !identical(
                    as.numeric(metadata$actual_start),
                    as.numeric(observed$actual_start)
                ) ||
                !identical(
                    as.numeric(metadata$actual_end),
                    as.numeric(observed$actual_end)
                )
        ) {
            cli::cli_abort(
                "The source time axis or units changed after shared windows were saved.",
                class = "epwshiftr_shared_cache_error"
            )
        }
    }
    windows <- metadata$windows
    if (!nrow(windows)) {
        return(invisible(0L))
    }
    pieces <- if (is.null(restored)) {
        vector("list", nrow(windows))
    } else {
        restored$pieces
    }
    for (index in seq_len(nrow(windows))) {
        window <- windows[index]
        active <- consumers[
            !cached &
                consumers$time_start <= window$time_stop[[1L]] &
                consumers$time_stop >= window$time_start[[1L]]
        ]
        if (!nrow(active)) {
            next
        }
        if (!is.null(pieces[[index]])) {
            next
        }
        identity <- store__hash(source_identity, window$window_id[[1L]])
        path <- file.path(directory, window$window_id[[1L]])
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        # The receipt check and source read share one lock so concurrent
        # resumes cannot replace each other's incomplete window directory.
        piece <- manifest_with_lock(
            path,
            {
                cached_window <- shift_batch__window_read(
                    path,
                    identity,
                    active$demand_id
                )
                if (!is.null(cached_window)) {
                    cached_window
                } else {
                    active <- data.table::copy(active)
                    data.table::set(
                        active,
                        j = "time_start",
                        value = pmax(active$time_start, window$time_start[[1L]])
                    )
                    data.table::set(
                        active,
                        j = "time_stop",
                        value = pmin(active$time_stop, window$time_stop[[1L]])
                    )
                    bounded <- data.table::copy(acquisition)
                    data.table::set(
                        bounded,
                        j = "time_start",
                        value = window$time_start
                    )
                    data.table::set(
                        bounded,
                        j = "time_stop",
                        value = window$time_stop
                    )
                    values <- shift_batch__read_acquisition(
                        dataset,
                        bounded,
                        active
                    )
                    shift_batch__window_write(
                        path,
                        identity,
                        values,
                        active$demand_id
                    )
                }
            },
            timeout = 86400
        )
        pieces[[index]] <- piece
    }
    shift_batch__seed_pending(acquisition, consumers, cached, pieces, metadata)
    invisible(nrow(windows))
}

# Warm the existing extraction cache before child workflows start. A plan with
# unmatched source demands stays on the ordinary child path. A failed shared
# remote read stops here so the next child does not retry the same source.
shift_batch__prefetch <- function(batch) {
    shared <- batch@meta$shared_plan
    if (
        is.null(shared) ||
            !nrow(shared$acquisitions) ||
            identical(cache__mode(), "off")
    ) {
        return(invisible(0L))
    }
    statuses <- vapply(
        batch@meta$children,
        function(child) {
            shift_status(child, refresh = FALSE)
        },
        character(1L)
    )
    if (all(statuses == "completed")) {
        return(invisible(0L))
    }
    completed <- 0L
    skipped <- character(nrow(shared$acquisitions))
    skip_count <- 0L
    # Partition links once so each file can take its own consumer group without
    # rescanning the full table or paying for a join at every file boundary.
    links <- split(
        shared$consumers,
        by = "acquisition_id",
        keep.by = TRUE
    )
    for (index in seq_len(nrow(shared$acquisitions))) {
        acquisition <- shared$acquisitions[index]
        consumers <- links[[acquisition$acquisition_id[[1L]]]]
        if (is.null(consumers) || !nrow(consumers)) {
            next
        }
        outcome <- tryCatch(
            shift_batch__prefetch_acquisition(
                batch@store_path,
                acquisition,
                consumers
            ),
            error = base::identity
        )
        if (inherits(outcome, "error")) {
            if (!inherits(outcome, "epwshiftr_shared_unavailable")) {
                attr(outcome, "shared_file") <- acquisition$filename[[1L]]
                stop(outcome)
            }
            skip_count <- skip_count + 1L
            skipped[[skip_count]] <- sprintf(
                "%s: %s",
                acquisition$filename[[1L]],
                conditionMessage(outcome)
            )
        } else {
            completed <- completed + 1L
        }
    }
    if (skip_count) {
        cli::cli_warn(c(
            "Shared reads are unavailable for {skip_count} source file(s); their children will use ordinary extraction.",
            "i" = skipped[[1L]]
        ))
    }
    invisible(completed)
}
