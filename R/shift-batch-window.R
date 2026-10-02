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
            length(receipt$chunks) != length(demand_ids) ||
            !setequal(
                vapply(receipt$chunks, `[[`, character(1L), "demand_id"),
                as.character(demand_ids)
            )
    ) {
        cli::cli_abort(
            "A shared acquisition window has an invalid receipt or checksum.",
            class = "epwshiftr_shared_cache_error"
        )
    }
    chunks <- stats::setNames(
        vapply(
            receipt$chunks,
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
        vapply(receipt$chunks, `[[`, character(1L), "demand_id")
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
    shift_batch__window_read(path, identity, demand_ids)
}

# Materialize one consumer in the existing site-extraction cache format. Child
# stores can then use their ordinary extraction task, provenance and resume
# logic without knowing that another site shared the source read.
shift_batch__seed_consumer <- function(
    acquisition,
    consumer,
    pieces,
    axis,
    units
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
    data.table::set(values, j = "units", value = units)
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
        available_time_count = length(cf_time__range_indices(
            axis$values,
            axis$coordinates,
            requested
        )),
        actual_start = min(axis$values, na.rm = TRUE),
        actual_end = max(axis$values, na.rm = TRUE)
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

# Open each physical file once, resume verified windows, and publish complete
# per-site payloads only after every native window succeeds. Failed reads leave
# earlier window receipts intact for the next batch resume.
shift_batch__prefetch_acquisition <- function(
    batch_root,
    acquisition,
    consumers
) {
    # Keep each bounded read below the row and source-cell limits while
    # retaining shared source access within a group of sites and methods.
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
                consumers[rows]
            )
        }
        return(invisible(sum(windows)))
    }
    cached <- vapply(
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
            !is.null(store__extract_cache_read(
                store__extract_cache_path(plan, acquisition)
            ))
        },
        logical(1L)
    )
    if (all(cached)) {
        return(invisible(0L))
    }
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
    dataset$open()
    on.exit(dataset$close(), add = TRUE)
    axis <- dataset$get_time_axis(index = 1L)
    units <- tryCatch(
        as.character(dataset$att_get(
            consumers$variable_id[[1L]],
            "units",
            index = 1L
        ))[[1L]],
        error = function(error) NA_character_
    )
    windows <- shift_batch__windows(axis, acquisition, nrow(consumers))
    if (!nrow(windows)) {
        return(invisible(0L))
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
    pieces <- vector("list", nrow(windows))
    for (index in seq_len(nrow(windows))) {
        window <- windows[index]
        active <- consumers[
            consumers$time_start <= window$time_stop[[1L]] &
                consumers$time_stop >= window$time_start[[1L]]
        ]
        if (!nrow(active)) {
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
    for (index in seq_len(nrow(consumers))) {
        if (cached[[index]]) {
            next
        }
        shift_batch__seed_consumer(
            acquisition,
            consumers[index],
            pieces,
            axis,
            units
        )
    }
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
    for (index in seq_len(nrow(shared$acquisitions))) {
        acquisition <- shared$acquisitions[index]
        consumers <- shared$consumers[
            shared$consumers$acquisition_id == acquisition$acquisition_id[[1L]]
        ]
        if (!nrow(consumers)) {
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
