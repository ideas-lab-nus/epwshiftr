# Check both immutable artifacts without changing manifest or reader state.
subset__block_valid <- function(block) {
    file.exists(block$path) &&
        file.exists(block$metadata_path) &&
        identical(checksum_file(block$path), block$sha256) &&
        identical(checksum_file(block$metadata_path), block$metadata_sha256)
}

# Preserve all available published evidence before cache reuse or inventory
# inheritance. Different declared digest algorithms remain incomparable.
subset__record_content <- function(conn, candidates) {
    if (!nrow(candidates)) {
        return(invisible(NULL))
    }
    columns <- c("source_key", "version", "checksum_type", "checksum", "size")
    current <- unique(candidates[, columns, with = FALSE])
    prior <- data.table::as.data.table(ddb_read_table(conn, "subset_content"))
    for (key in unique(current$source_key)) {
        known <- prior[prior$source_key == key]
        incoming <- current[current$source_key == key]
        for (i in seq_len(nrow(incoming))) {
            if (
                nrow(known) &&
                    any(!query_result__compatible_content(incoming[i], known))
            ) {
                stop(
                    "Conflicting published content metadata for saved source identity.",
                    call. = FALSE
                )
            }
        }
    }
    saved <- unique(data.table::rbindlist(list(prior, current), fill = TRUE))
    ddb_write_table(
        conn,
        "subset_content",
        as.data.frame(saved),
        overwrite = TRUE
    )
    invisible(NULL)
}

# Demand identity records only physical location and requested native window;
# request/case labels continue to live solely in the association table.
subset__demand_ids <- function(links) {
    vapply(
        seq_len(nrow(links)),
        function(i) {
            store__hash(
                "raw-demand-receipt-v1",
                links$source_key[[i]],
                links$lon[[i]],
                links$lat[[i]],
                links$time_start[[i]],
                links$time_stop[[i]]
            )
        },
        character(1L)
    )
}

# Publish independently checked native selection receipts, including an empty
# selection. Empty windows must never borrow another window's completed blocks.
subset__save_demands <- function(conn, demands, dest) {
    if (is.null(demands) || !nrow(demands)) {
        return(invisible(NULL))
    }
    records <- data.table::copy(demands)
    paths <- file.path(
        dest,
        "subsets",
        paste0("demand-", demands$demand_id, ".rds")
    )
    checksums <- character(nrow(demands))
    for (i in seq_len(nrow(demands))) {
        store__extract_cache_write(paths[[i]], as.data.frame(demands[i]))
        checksums[[i]] <- checksum_file(paths[[i]])
        ddb_exec(
            conn,
            sprintf(
                "DELETE FROM subset_demands WHERE demand_id = %s",
                ddb_literal(conn, demands$demand_id[[i]])
            )
        )
    }
    data.table::set(records, j = "native_ranges", value = NULL)
    data.table::set(records, j = "receipt_path", value = paths)
    data.table::set(records, j = "receipt_sha256", value = checksums)
    ddb_append_table(conn, "subset_demands", as.data.frame(records))
    invisible(NULL)
}

# Test interval unions without expanding every native position or borrowing
# spatially unrelated blocks. Intervals are inclusive and sorted only locally.
subset__ranges_covered <- function(required, available) {
    if (!length(required) || !nrow(available)) {
        return(FALSE)
    }
    positions <- order(available$time_start_index)
    starts <- available$time_start_index[positions]
    stops <- starts + available$time_count[positions] - 1L
    for (range in required) {
        cursor <- as.integer(range[[1L]])
        last <- cursor + as.integer(range[[2L]]) - 1L
        for (i in seq_along(starts)) {
            if (stops[[i]] < cursor) {
                next
            }
            if (starts[[i]] > cursor) {
                break
            }
            cursor <- max(cursor, stops[[i]] + 1L)
            if (cursor > last) break
        }
        if (cursor <= last) return(FALSE)
    }
    TRUE
}

# Derive current and historical completion from exact native demand receipts
# plus valid spatial/time coverage, independently of the latest source job.
subset__coverage <- function(links, demands, membership, blocks) {
    ids <- subset__demand_ids(links)
    unique_ids <- unique(ids)
    covered <- logical(length(unique_ids))
    for (i in seq_along(unique_ids)) {
        id <- unique_ids[[i]]
        row <- links[match(id, ids)]
        if (is.na(row$source_key)) {
            next
        }
        receipt <- demands[demands$demand_id == id]
        if (nrow(receipt) != 1L) {
            next
        }
        if (
            !file.exists(receipt$receipt_path) ||
                !identical(
                    checksum_file(receipt$receipt_path),
                    receipt$receipt_sha256
                )
        ) {
            next
        }
        descriptor <- readRDS(receipt$receipt_path)
        if (!identical(subset__demand_ids(descriptor), id)) {
            next
        }
        required <- jsonlite::fromJSON(
            descriptor$native_ranges[[1L]],
            simplifyVector = FALSE
        )
        if (!length(required)) {
            next
        }
        corners <- unique(membership[
            membership$source_key == row$source_key &
                membership$target_lon == row$lon &
                membership$target_lat == row$lat,
            c("ind_lat", "ind_lon"),
            with = FALSE
        ])
        if (nrow(corners) != 4L) {
            next
        }
        source_blocks <- blocks[blocks$source_key == row$source_key]
        points <- vapply(
            seq_len(nrow(corners)),
            function(j) {
                available <- source_blocks[
                    source_blocks$ind_lat <= corners$ind_lat[[j]] &
                        source_blocks$ind_lat + source_blocks$lat_count >
                            corners$ind_lat[[j]] &
                        source_blocks$ind_lon <= corners$ind_lon[[j]] &
                        source_blocks$ind_lon + source_blocks$lon_count >
                            corners$ind_lon[[j]]
                ]
                subset__ranges_covered(required, available)
            },
            logical(1L)
        )
        covered[[i]] <- all(points)
    }
    covered[match(ids, unique_ids)]
}

# Read normalized previously required interval observations from checked local
# metadata only. Weather Parquet payloads need not be read for this comparison.
subset__saved_bounds <- function(blocks) {
    if (!nrow(blocks)) {
        return(NULL)
    }
    pieces <- lapply(blocks$metadata_path, function(path) readRDS(path)$bounds)
    unique(data.table::rbindlist(pieces, fill = TRUE))
}

# Keep endpoint inventory separate from immutable climate content identity.
subset__access_plan <- function(files, expand = TRUE) {
    checkmate::assert_flag(expand)
    if (expand) {
        files <- files$expand_replicas(service = "OPENDAP", all = TRUE)
    }
    docs <- priv(files)$get_docs()
    if (!nrow(docs)) {
        return(data.table::data.table())
    }
    urls <- priv(files)$get_url("OPENDAP", "OPENDAP")
    groups <- query_result__compatible_file_groups(docs)
    logical <- query_result__file_key(data.table::as.data.table(docs))
    fields <- c(
        "source_id",
        "experiment_id",
        "member_id",
        "grid_label",
        "table_id",
        "variable_id",
        "version",
        "checksum_type",
        "checksum",
        "size",
        "data_node"
    )
    out <- data.table::as.data.table(lapply(fields, function(field) {
        as.character(query_result__col(docs, field))
    }))
    data.table::setnames(out, fields)
    data.table::set(out, j = "url", value = sub("[.]html$", "", urls))
    missing_node <- is.na(out$data_node) | !nzchar(out$data_node)
    data.table::set(
        out,
        i = which(missing_node),
        j = "data_node",
        value = sub("^https?://([^/]+).*$", "\\1", out$url[missing_node])
    )
    data.table::set(out, j = "logical_id", value = logical)
    ranges <- query_result__drs_ranges(query_result__drs_labels(docs)$value)
    data.table::set(
        out,
        j = "file_start_year",
        value = suppressWarnings(as.integer(format(
            ranges$datetime_start,
            "%Y"
        )))
    )
    data.table::set(
        out,
        j = "file_stop_year",
        value = suppressWarnings(as.integer(format(ranges$datetime_end, "%Y")))
    )
    data.table::set(out, j = "content_group", value = groups)
    # Use the strongest available version within an already compatible group.
    # Known contradictory groups are not silently merged into a source.
    keys <- character(nrow(out))
    for (group in unique(groups)) {
        rows <- which(groups == group)
        version <- unique(out$version[rows])
        version <- version[!is.na(version) & nzchar(version)]
        key <- store__hash("raw-subset-source-v1", logical[rows[[1L]]], version)
        keys[rows] <- key
    }
    data.table::set(out, j = "source_key", value = keys)
    data.table::set(out, j = "priority", value = seq_len(nrow(out)))
    pairs <- unique(out[, c("source_key", "content_group"), with = FALSE])
    duplicate_keys <- pairs$source_key[duplicated(pairs$source_key)]
    if (length(duplicate_keys)) {
        stop(
            "Conflicting replica content metadata for one source identity.",
            call. = FALSE
        )
    }
    data.table::set(
        out,
        j = "candidate_id",
        value = vapply(
            seq_len(nrow(out)),
            function(i) {
                store__hash(out$source_key[[i]], out$url[[i]])
            },
            character(1L)
        )
    )
    data.table::set(out, j = "record_index", value = seq_len(nrow(docs)))
    out <- unique(out, by = "candidate_id")
    attr(out, "selection") <- files$selection()
    out[]
}

# Save corner membership with each durable block, so partial failed sources
# remain readable and a later replica cannot hide already acquired data.
subset__save_membership <- function(conn, source_key, neighborhoods) {
    membership <- unique(neighborhoods[,
        c(
            "target_lon",
            "target_lat",
            "ind_lat",
            "ind_lon",
            "grid_lat",
            "grid_lon",
            "role"
        ),
        with = FALSE
    ])
    data.table::set(membership, j = "source_key", value = source_key)
    membership <- membership[,
        c(
            "source_key",
            "target_lon",
            "target_lat",
            "ind_lat",
            "ind_lon",
            "grid_lat",
            "grid_lon",
            "role"
        ),
        with = FALSE
    ]
    ddb_append_table(conn, "subset_neighborhoods", as.data.frame(membership))
}

# Create only additive raw-subset tables in the existing single-owner Store.
subset__open <- function(dest) {
    store <- EsgStore$new(dest)
    opened <- FALSE
    on.exit(if (!opened) store$close(), add = TRUE)
    conn <- priv(store)$conn
    definitions <- list(
        subset_content = "source_key VARCHAR, version VARCHAR, checksum_type VARCHAR, checksum VARCHAR, size VARCHAR",
        subset_demands = "demand_id VARCHAR PRIMARY KEY, source_key VARCHAR, lon DOUBLE, lat DOUBLE, time_start VARCHAR, time_stop VARCHAR, receipt_path VARCHAR, receipt_sha256 VARCHAR",
        subset_jobs = "source_key VARCHAR PRIMARY KEY, signature VARCHAR, status VARCHAR, error VARCHAR, contract VARCHAR, expected_blocks VARCHAR",
        subset_blocks = paste(
            "block_id VARCHAR PRIMARY KEY, source_key VARCHAR, contract VARCHAR,",
            "ind_lat INTEGER, ind_lon INTEGER, lat_count INTEGER, lon_count INTEGER,",
            "time_start_index INTEGER, time_count INTEGER, path VARCHAR, sha256 VARCHAR,",
            "metadata_path VARCHAR, metadata_sha256 VARCHAR, url VARCHAR, data_node VARCHAR, observed_at DOUBLE"
        ),
        subset_attempts = paste(
            "source_key VARCHAR, url VARCHAR, data_node VARCHAR, client VARCHAR,",
            "observed_at DOUBLE, open_seconds DOUBLE, elapsed_seconds DOUBLE, native_values DOUBLE,",
            "success BOOLEAN, error VARCHAR, error_kind VARCHAR"
        ),
        subset_links = paste(
            "request_id VARCHAR, case_id VARCHAR, source_key VARCHAR, site_id VARCHAR,",
            "lon DOUBLE, lat DOUBLE, time_start VARCHAR, time_stop VARCHAR"
        ),
        subset_candidates = paste(
            "candidate_id VARCHAR, source_key VARCHAR, url VARCHAR, data_node VARCHAR,",
            "priority INTEGER, variable_id VARCHAR, version VARCHAR"
        ),
        subset_neighborhoods = paste(
            "source_key VARCHAR, target_lon DOUBLE, target_lat DOUBLE,",
            "ind_lat INTEGER, ind_lon INTEGER, grid_lat DOUBLE, grid_lon DOUBLE, role VARCHAR"
        )
    )
    for (table in names(definitions)) {
        ddb_exec(
            conn,
            sprintf(
                "CREATE TABLE IF NOT EXISTS %s (%s)",
                ddb_ident(conn, table),
                definitions[[table]]
            )
        )
    }
    ddb_exec(
        conn,
        "ALTER TABLE subset_attempts ADD COLUMN IF NOT EXISTS error_kind VARCHAR"
    )
    ddb_exec(
        conn,
        "ALTER TABLE subset_jobs ADD COLUMN IF NOT EXISTS expected_blocks VARCHAR"
    )
    ddb_exec(
        conn,
        "CREATE TABLE IF NOT EXISTS subset_invalid_blocks AS SELECT *, CAST(NULL AS DOUBLE) AS invalidated_at FROM subset_blocks WHERE FALSE"
    )
    ddb_exec(conn, "CREATE TABLE IF NOT EXISTS subset_schema (version INTEGER)")
    version <- ddb_query(conn, "SELECT version FROM subset_schema")$version
    if (!length(version)) {
        ddb_exec(conn, "INSERT INTO subset_schema VALUES (1)")
    }
    if (length(version) && !identical(as.integer(version), 1L)) {
        store$close()
        stop("Unsupported raw subset manifest schema.", call. = FALSE)
    }
    # A checked publication receipt survives a process exit between the atomic
    # artifact rename and manifest commit. Only fully verified pairs are adopted.
    pending <- list.files(
        file.path(dest, "subsets"),
        pattern = "^pending-.*[.]rds$",
        full.names = TRUE
    )
    for (path in pending) {
        if (!store__extract_cache_available(path)) {
            next
        }
        row <- readRDS(path)
        if (!subset__block_valid(row)) {
            next
        }
        prior <- ddb_query(
            conn,
            sprintf(
                "SELECT contract FROM subset_blocks WHERE source_key = %s",
                ddb_literal(conn, row$source_key)
            )
        )
        if (nrow(prior) && any(prior$contract != row$contract)) {
            stop(
                "Pending Replica publication conflicts with native identity.",
                call. = FALSE
            )
        }
        present <- ddb_query(
            conn,
            sprintf(
                "SELECT block_id FROM subset_blocks WHERE block_id = %s",
                ddb_literal(conn, row$block_id)
            )
        )
        if (!nrow(present)) {
            ddb_append_table(conn, "subset_blocks", row)
        }
        info <- readRDS(row$metadata_path)
        subset__save_membership(conn, row$source_key, info$neighborhoods)
        subset__save_demands(conn, info$demands, dest)
        unlink(c(path, paste0(path, ".json")))
    }
    opened <- TRUE
    store
}

# Match each explicit demand without broadening a zero-result selector.
subset__requests <- function(requests, candidates) {
    checkmate::assert_data_frame(requests, min.rows = 1L)
    requests <- data.table::as.data.table(data.table::copy(requests))
    required <- c(
        "request_id",
        "site_id",
        "lon",
        "lat",
        "time_start",
        "time_stop"
    )
    checkmate::assert_names(names(requests), must.include = required)
    checkmate::assert_character(
        requests$request_id,
        unique = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_character(requests$site_id, any.missing = FALSE)
    checkmate::assert_numeric(
        requests$lon,
        lower = -180,
        upper = 360,
        any.missing = FALSE,
        finite = TRUE
    )
    checkmate::assert_numeric(
        requests$lat,
        lower = -90,
        upper = 90,
        any.missing = FALSE,
        finite = TRUE
    )
    for (name in c("time_start", "time_stop")) {
        checkmate::assert_character(
            requests[[name]],
            any.missing = FALSE,
            min.chars = 1L
        )
    }
    if (!"case_id" %in% names(requests)) {
        data.table::set(requests, j = "case_id", value = NA_character_)
    }
    selectors <- intersect(
        c(
            "source_id",
            "experiment_id",
            "member_id",
            "grid_label",
            "table_id",
            "variable_id",
            "version"
        ),
        names(requests)
    )
    rows <- vector("list", nrow(requests))
    for (i in seq_len(nrow(requests))) {
        matches <- candidates
        for (field in selectors) {
            value <- as.character(requests[[field]][[i]])
            if (is.na(value)) {
                stop(
                    "Explicit request selectors cannot be missing.",
                    call. = FALSE
                )
            }
            matches <- matches[
                !is.na(matches[[field]]) & matches[[field]] == value
            ]
        }
        start_year <- suppressWarnings(as.integer(substr(
            requests$time_start[[i]],
            1L,
            4L
        )))
        stop_year <- suppressWarnings(as.integer(substr(
            requests$time_stop[[i]],
            1L,
            4L
        )))
        if (is.na(start_year) || is.na(stop_year) || start_year > stop_year) {
            stop(
                "Request windows must have ordered native ISO date endpoints.",
                call. = FALSE
            )
        }
        matches <- matches[
            (is.na(matches$file_start_year) |
                matches$file_start_year <= stop_year) &
                (is.na(matches$file_stop_year) |
                    matches$file_stop_year >= start_year)
        ]
        keys <- unique(matches$source_key)
        if (!length(keys)) {
            keys <- NA_character_
        }
        row <- requests[
            rep.int(i, length(keys)),
            c(required, "case_id"),
            with = FALSE
        ]
        data.table::set(row, j = "source_key", value = keys)
        rows[[i]] <- row
    }
    data.table::rbindlist(rows, fill = TRUE)
}

# Reuse committed blocks only after both artifacts pass local integrity checks.
subset__valid_blocks <- function(store, source_key = NULL) {
    blocks <- ddb_read_table(priv(store)$conn, "subset_blocks")
    if (!is.null(source_key)) {
        blocks <- blocks[blocks$source_key == source_key, , drop = FALSE]
    }
    if (!nrow(blocks)) {
        return(data.table::as.data.table(blocks))
    }
    valid <- vapply(
        seq_len(nrow(blocks)),
        function(i) {
            subset__block_valid(blocks[i, , drop = FALSE])
        },
        logical(1L)
    )
    if (any(!valid)) {
        conn <- priv(store)$conn
        invalid <- blocks[!valid, , drop = FALSE]
        invalid$invalidated_at <- as.double(Sys.time())
        # Preserve the failed receipt as audit history while excluding it from
        # active coverage. A completed job's expected IDs still reveal the gap.
        ddb_exec(conn, "BEGIN TRANSACTION")
        committed <- FALSE
        on.exit(if (!committed) ddb_exec(conn, "ROLLBACK"), add = TRUE)
        ddb_append_table(conn, "subset_invalid_blocks", invalid)
        for (id in invalid$block_id) {
            ddb_exec(
                conn,
                sprintf(
                    "DELETE FROM subset_blocks WHERE block_id = %s",
                    ddb_literal(conn, id)
                )
            )
        }
        ddb_exec(conn, "COMMIT")
        committed <- TRUE
    }
    data.table::as.data.table(blocks[valid, , drop = FALSE])
}

# Publish a single received block through the Store's atomic Parquet writer.
# The calling process alone registers it in the manifest transaction.
subset__commit <- function(store, job, path) {
    receipt <- paste0(path, ".json")
    if (!file.exists(receipt) || !store__extract_cache_available(path)) {
        return(FALSE)
    }
    chunk <- readRDS(path)
    conn <- priv(store)$conn
    slice <- chunk$read_slices
    block_id <- store__hash(
        job$source_key,
        chunk$contract,
        slice$ind_lat,
        slice$ind_lon,
        slice$lat_count,
        slice$lon_count,
        slice$time_start_index,
        slice$time_count
    )
    target <- file.path(job$dest, "subsets", paste0(block_id, ".parquet"))
    metadata <- file.path(job$dest, "subsets", paste0(block_id, ".rds"))
    values <- merge(
        chunk$values,
        chunk$time,
        by = c("file_index", "time_index"),
        all.x = TRUE,
        sort = FALSE
    )
    data.table::set(values, j = "source_key", value = job$source_key)
    priv(store)$write_parquet(values, target, overwrite = TRUE)
    info <- chunk[c(
        "contract",
        "neighborhoods",
        "read_slices",
        "bounds",
        "demands"
    )]
    store__extract_cache_write(metadata, info)
    row <- data.frame(
        block_id = block_id,
        source_key = job$source_key,
        contract = chunk$contract,
        ind_lat = slice$ind_lat,
        ind_lon = slice$ind_lon,
        lat_count = slice$lat_count,
        lon_count = slice$lon_count,
        time_start_index = slice$time_start_index,
        time_count = slice$time_count,
        path = target,
        sha256 = checksum_file(target),
        metadata_path = metadata,
        metadata_sha256 = checksum_file(metadata),
        url = job$url,
        data_node = job$data_node,
        observed_at = as.double(Sys.time())
    )
    prior <- ddb_query(
        conn,
        sprintf(
            "SELECT DISTINCT contract FROM subset_blocks WHERE source_key = %s",
            ddb_literal(conn, job$source_key)
        )
    )
    if (nrow(prior) && !all(prior$contract == chunk$contract)) {
        stop(
            "Received replica conflicts with committed native coordinate identity.",
            call. = FALSE
        )
    }
    pending <- file.path(
        job$dest,
        "subsets",
        paste0("pending-", block_id, ".rds")
    )
    store__extract_cache_write(pending, row)
    ddb_exec(conn, "BEGIN TRANSACTION")
    done <- FALSE
    on.exit(if (!done) ddb_exec(conn, "ROLLBACK"), add = TRUE)
    ddb_exec(
        conn,
        sprintf(
            "DELETE FROM subset_blocks WHERE block_id = %s",
            ddb_literal(conn, block_id)
        )
    )
    ddb_append_table(conn, "subset_blocks", row)
    subset__save_membership(conn, job$source_key, chunk$neighborhoods)
    subset__save_demands(conn, chunk$demands, job$dest)
    ddb_exec(conn, "COMMIT")
    done <- TRUE
    unlink(c(path, receipt, pending, paste0(pending, ".json")))
    TRUE
}

# One source reader keeps its connection while writing bounded transfer files.
# These are transport artifacts; only the parent commits durable public results.
subset__read_source <- function(job) {
    began <- proc.time()[["elapsed"]]
    # netCDF-C otherwise prefetches small variables during nc_open(), before
    # any requested hyperslab. These client options affect only this new path:
    # https://docs.unidata.ucar.edu/nug/current/dap2.html
    separator <- if (grepl("#", job$url, fixed = TRUE)) "&" else "#"
    ds <- EsgDataset$new(paste0(job$url, separator, "noprefetch&nocache"))
    on.exit(ds$close(), add = TRUE)
    ds$open(progress = FALSE)
    opened <- proc.time()[["elapsed"]] - began
    counter <- 0L
    native_values <- 0
    neighborhoods <- NULL
    completed <- job$completed
    expected <- job$contract
    known_bounds <- job$expected_bounds
    targets <- unique(job$links[,
        c("lon", "lat", "time_start", "time_stop"),
        with = FALSE
    ])
    axis <- dataset__time_axis(ds, 1L)
    demands <- data.table::copy(targets)
    data.table::set(demands, j = "source_key", value = job$source_key)
    data.table::set(
        demands,
        j = "demand_id",
        value = subset__demand_ids(demands)
    )
    ranges <- vapply(
        seq_len(nrow(demands)),
        function(i) {
            selected <- dataset__neighborhood_time_indices(
                axis$values,
                axis$coordinates,
                c(demands$time_start[[i]], demands$time_stop[[i]])
            )
            runs <- dataset__region_runs(selected, .Machine$integer.max)
            as.character(jsonlite::toJSON(
                lapply(runs, function(run) c(run[[1L]], length(run))),
                auto_unbox = FALSE
            ))
        },
        character(1L)
    )
    data.table::set(demands, j = "native_ranges", value = ranges)
    data.table::setcolorder(
        demands,
        c(
            "demand_id",
            "source_key",
            "lon",
            "lat",
            "time_start",
            "time_stop",
            "native_ranges"
        )
    )
    locations <- unique(targets[, c("lon", "lat"), with = FALSE])
    neighborhoods <- vector("list", nrow(locations))
    # Different site windows stay separate. Overlapping corners can reuse the
    # coverage just saved by a preceding site without fetching it a second time.
    for (i in seq_len(nrow(locations))) {
        site <- locations[i]
        rows <- targets[
            targets$lon == site$lon[[1L]] & targets$lat == site$lat[[1L]]
        ]
        windows <- lapply(seq_len(nrow(rows)), function(k) {
            c(rows$time_start[[k]], rows$time_stop[[k]])
        })
        ids <- unique(job$links$site_id[
            job$links$lon == site$lon[[1L]] & job$links$lat == site$lat[[1L]]
        ])
        sites <- data.frame(
            site_id = ids,
            lon = site$lon[[1L]],
            lat = site$lat[[1L]]
        )
        receive <- function(chunk) {
            counter <<- counter + 1L
            native_values <<- native_values + nrow(chunk$values)
            chunk$open_seconds <- if (counter == 1L) opened else 0
            path <- file.path(job$inbox, sprintf("block-%08d.rds", counter))
            if (counter == 1L) {
                chunk$demands <- demands
            }
            store__extract_cache_write(path, chunk)
            expected <<- chunk$contract
            store_write_json_atomic(
                list(blocks = counter),
                job$subset_progress,
                auto_unbox = TRUE
            )
        }
        selection <- dataset__read_neighborhood(
            ds,
            job$variable_id,
            sites,
            windows,
            receive,
            completed = completed,
            expected_contract = expected,
            expected_bounds = known_bounds
        )
        neighborhoods[[i]] <- selection$neighborhoods
        known_bounds <- unique(data.table::rbindlist(
            list(known_bounds, selection$bounds),
            fill = TRUE
        ))
        # Merge descriptors once per city, after its disjoint groups finished.
        completed <- data.table::rbindlist(
            list(completed, selection$read_slices),
            fill = TRUE
        )
    }
    list(
        open_seconds = opened,
        elapsed_seconds = proc.time()[["elapsed"]] - began,
        native_values = native_values,
        blocks = counter,
        demands = demands,
        contract = expected,
        neighborhoods = unique(data.table::rbindlist(
            neighborhoods,
            fill = TRUE
        ))
    )
}

# Select candidates from recent actual demand reads on this client only.
subset__rank <- function(
    candidates,
    attempts,
    client,
    now = as.double(Sys.time())
) {
    recent <- attempts[
        attempts$client == client & attempts$observed_at >= now - 86400,
        ,
        drop = FALSE
    ]
    if (nrow(recent)) {
        recent <- recent[
            recent$native_values > 0 |
                (!is.na(recent$error_kind) & recent$error_kind == "transport"),
            ,
            drop = FALSE
        ]
    }
    scores <- lapply(seq_len(nrow(candidates)), function(i) {
        row <- candidates[i]
        node <- recent[recent$data_node == row$data_node[[1L]], , drop = FALSE]
        success <- node$success & node$native_values > 0
        failed_file <- recent$source_key == row$source_key[[1L]] &
            recent$url == row$url[[1L]] &
            !recent$success
        rate <- if (nrow(node)) {
            sum(success) / max(1, sum(success) + sum(!node$success))
        } else {
            .5
        }
        speed <- if (any(success)) {
            stats::median(
                (node$elapsed_seconds[success] +
                    node$open_seconds[success]) /
                    node$native_values[success],
                na.rm = TRUE
            )
        } else {
            Inf
        }
        c(
            failed = sum(failed_file, na.rm = TRUE),
            reliability = -rate,
            seconds_per_value = speed,
            original = row$priority[[1L]]
        )
    })
    if (!length(scores)) {
        return(candidates)
    }
    score <- do.call(rbind, scores)
    candidates[do.call(order, as.data.frame(score))]
}

# Separate transport failures from metadata conflicts and implementation errors.
# Only actual transport failures contribute to data-node reliability ranking.
subset__failure_kind <- function(error) {
    message <- if (inherits(error, "condition")) {
        conditionMessage(error)
    } else {
        error
    }
    if (length(message) != 1L || is.na(message)) {
        return("completion")
    }
    if (
        grepl(
            "Replica|identity|unsupported|require rectilinear|Variable.*absent|not expose|missing required",
            message,
            ignore.case = TRUE
        )
    ) {
        return("source_metadata")
    }
    if (
        grepl(
            "NetCDF:|curl|OPeNDAP connection|execution deadline|timed out|connection reset",
            message,
            ignore.case = TRUE
        )
    ) {
        return("transport")
    }
    "internal"
}

# A source job result or failure becomes a small observation, never a new case.
subset__attempt <- function(
    store,
    job,
    client,
    success,
    error = NA_character_,
    result = NULL
) {
    elapsed <- as.double(Sys.time()) - job$began
    row <- data.frame(
        source_key = job$source_key,
        url = job$url,
        data_node = job$data_node,
        client = client,
        observed_at = as.double(Sys.time()),
        open_seconds = if (is.null(result$open_seconds)) {
            NA_real_
        } else {
            result$open_seconds
        },
        elapsed_seconds = elapsed,
        native_values = 0,
        success = success,
        error = error,
        error_kind = subset__failure_kind(error)
    )
    ddb_append_table(priv(store)$conn, "subset_attempts", row)
}

# Publish caller-owned completed blocks and their real data-read observations.
subset__drain <- function(store, state, client) {
    if (is.null(state$job)) {
        return(invisible(NULL))
    }
    paths <- list.files(
        state$job$inbox,
        pattern = "^block-[0-9]+[.]rds$",
        full.names = TRUE
    )
    for (path in paths) {
        if (!file.exists(paste0(path, ".json"))) {
            next
        }
        if (!store__extract_cache_available(path)) {
            stop(
                "A received raw block failed its local checksum.",
                call. = FALSE
            )
        }
        chunk <- readRDS(path)
        if (subset__commit(store, state$job, path)) {
            row <- data.frame(
                source_key = state$job$source_key,
                url = state$job$url,
                data_node = state$job$data_node,
                client = client,
                observed_at = as.double(Sys.time()),
                open_seconds = chunk$open_seconds,
                elapsed_seconds = chunk$read_slices$elapsed_seconds,
                native_values = nrow(chunk$values),
                success = TRUE,
                error = NA_character_,
                error_kind = "data"
            )
            ddb_append_table(priv(store)$conn, "subset_attempts", row)
        }
    }
    invisible(NULL)
}

# Record a source state with a conditional identity instead of mutating readers.
subset__job_state <- function(
    store,
    key,
    signature,
    status,
    error = NA_character_,
    contract = NA_character_
) {
    conn <- priv(store)$conn
    ddb_exec(
        conn,
        sprintf(
            "DELETE FROM subset_jobs WHERE source_key = %s",
            ddb_literal(conn, key)
        )
    )
    expected <- if (status == "done") {
        as.character(jsonlite::toJSON(
            sort(subset__valid_blocks(store, key)$block_id),
            auto_unbox = FALSE
        ))
    } else {
        NA_character_
    }
    ddb_append_table(
        conn,
        "subset_jobs",
        data.frame(
            source_key = key,
            signature = signature,
            status = status,
            error = error,
            contract = contract,
            expected_blocks = expected
        )
    )
}

# Use a dedicated source-pool job for supplementary directory lookup, so a slow
# index never blocks the owner's result collection or worker deadline checks.
subset__discover <- function(job) {
    subset__access_plan(job$files, expand = TRUE)
}

# Keep the public orchestration small by reusing Store writes and source__pool.
subset__fetch <- function(
    files,
    requests,
    dest,
    workers = 4L,
    resume = TRUE,
    timeout = 180,
    retries = 2L,
    discover = TRUE,
    network_id = "default",
    node_workers = 2L
) {
    checkmate::assert_count(workers, positive = TRUE)
    checkmate::assert_count(node_workers, positive = TRUE)
    checkmate::assert_flag(resume)
    checkmate::assert_count(retries)
    checkmate::assert_flag(discover)
    checkmate::assert_string(network_id)
    dest <- store_abs_path(path.expand(dest), getwd())
    candidates <- subset__access_plan(files, expand = FALSE)
    links <- subset__requests(requests, candidates)
    store <- subset__open(dest)
    on.exit(store$close(), add = TRUE)
    conn <- priv(store)$conn
    subset__record_content(conn, candidates)
    # Request bookkeeping is separate from the identity of persisted values.
    existing_links <- data.table::as.data.table(ddb_read_table(
        conn,
        "subset_links"
    ))
    if (nrow(existing_links)) {
        existing_links <- existing_links[
            !existing_links$request_id %in% requests$request_id
        ]
    }
    all_links <- unique(data.table::rbindlist(
        list(existing_links, links),
        fill = TRUE
    ))
    ddb_write_table(
        conn,
        "subset_links",
        as.data.frame(all_links),
        overwrite = TRUE
    )
    prior_candidates <- data.table::as.data.table(ddb_read_table(
        conn,
        "subset_candidates"
    ))
    candidate_columns <- names(prior_candidates)
    if (nrow(candidates)) {
        saved_candidates <- unique(
            data.table::rbindlist(
                list(
                    prior_candidates,
                    candidates[, candidate_columns, with = FALSE]
                ),
                fill = TRUE
            ),
            by = "candidate_id"
        )
        ddb_write_table(
            conn,
            "subset_candidates",
            as.data.frame(saved_candidates),
            overwrite = TRUE
        )
    }
    # Preserve the caller's exact selection and configuration independently of
    # mutable case links. Each run has checked provenance, including code identity.
    run_id <- basename(tempfile("run-", tmpdir = file.path(dest, "subsets")))
    store__extract_cache_write(
        file.path(dest, "subsets", paste0(run_id, ".rds")),
        list(
            requests = as.data.frame(requests),
            selection = files$selection(),
            candidates = candidates,
            observed_at = as.double(Sys.time()),
            configuration = list(
                workers = workers,
                node_workers = node_workers,
                resume = resume,
                timeout = timeout,
                retries = retries,
                discover = discover,
                network_id = network_id
            ),
            runtime = R.version.string,
            package = as.character(utils::packageVersion("epwshiftr")),
            implementation = store__hash(
                body(subset__fetch),
                body(subset__read_source),
                body(dataset__read_neighborhood),
                body(dataset__neighborhood_time_indices),
                body(dataset__neighborhood_bounds),
                body(subset__coverage),
                body(subset__save_demands),
                body(subset__record_content)
            )
        )
    )
    client <- store__hash(Sys.info()[["nodename"]], network_id)
    states <- vector(
        "list",
        length(unique(links$source_key[!is.na(links$source_key)]))
    )
    names(states) <- unique(links$source_key[!is.na(links$source_key)])
    keys <- unique(links$source_key[!is.na(links$source_key)])
    for (key in keys) {
        state <- new.env(parent = emptyenv())
        state$key <- key
        state$links <- links[links$source_key == key]
        demand <- unique(state$links[,
            c("lon", "lat", "time_start", "time_stop"),
            with = FALSE
        ])
        data.table::setorderv(demand, names(demand))
        state$signature <- store__hash(
            "raw-demand-v1",
            key,
            as.data.frame(demand)
        )
        state$candidates <- candidates[
            candidates$source_key == key &
                !is.na(candidates$url) &
                nzchar(candidates$url)
        ]
        state$candidates <- state$candidates[grepl(
            "^https?://",
            state$candidates$url
        )]
        # Reuse previously discovered endpoint inventory only under the exact
        # pinned source identity; directory rows never create a new demand.
        known <- prior_candidates[prior_candidates$source_key == key]
        if (nrow(known) && nrow(state$candidates)) {
            inherited <- state$candidates[rep.int(1L, nrow(known))]
            for (column in names(known)) {
                data.table::set(inherited, j = column, value = known[[column]])
            }
            state$candidates <- unique(
                data.table::rbindlist(
                    list(state$candidates, inherited),
                    fill = TRUE
                ),
                by = "candidate_id"
            )
        }
        state$record <- files$slice(candidates$record_index[match(
            key,
            candidates$source_key
        )])
        state$ticket <- NULL
        state$job <- NULL
        state$result <- NULL
        state$error <- NULL
        state$tried <- new.env(hash = TRUE, parent = emptyenv())
        state$round <- 0L
        state$discovered <- !discover
        state$finished <- FALSE
        complete <- subset__valid_blocks(store, key)
        if (resume) {
            state$finished <- all(subset__coverage(
                state$links,
                data.table::as.data.table(ddb_read_table(
                    conn,
                    "subset_demands"
                )),
                data.table::as.data.table(ddb_read_table(
                    conn,
                    "subset_neighborhoods"
                )),
                complete
            ))
        }
        # A checked publication can finish after the prior owner exited while
        # its job still said running. Receipt coverage is authoritative locally.
        if (state$finished) {
            subset__job_state(
                store,
                key,
                state$signature,
                "done",
                contract = if (nrow(complete)) {
                    complete$contract[[1L]]
                } else {
                    NA_character_
                }
            )
        }
        states[[key]] <- state
    }
    pool <- NULL
    on.exit(if (!is.null(pool)) pool$close(force = TRUE), add = TRUE)
    if (any(!vapply(states, function(s) s$finished, logical(1L)))) {
        pool <- source__pool(workers, timeout = timeout)
    }
    # Each state has at most one active attempt. Queue only available worker
    # slots rather than serializing every File record into an unbounded queue.
    while (any(!vapply(states, function(s) s$finished, logical(1L)))) {
        active <- 0L
        for (state in states) {
            if (is.null(state$ticket)) {
                next
            }
            subset__drain(store, state, client)
        }
        pool$poll()
        for (state in states) {
            if (is.null(state$ticket)) {
                next
            }
            subset__drain(store, state, client)
            if (state$ticket$status == "pending") {
                active <- active + 1L
                next
            }
            if (inherits(state$ticket$error, "error")) {
                stop(state$ticket$error)
            }
            if (identical(state$job$kind, "discover")) {
                state$discovered <- TRUE
                if (is.null(state$error) && nrow(state$result)) {
                    additions <- state$result[
                        state$result$source_key == state$key
                    ]
                    subset__record_content(conn, additions)
                    saved <- data.table::as.data.table(ddb_read_table(
                        conn,
                        "subset_candidates"
                    ))
                    saved <- unique(
                        data.table::rbindlist(
                            list(
                                saved,
                                additions[, names(saved), with = FALSE]
                            ),
                            fill = TRUE
                        ),
                        by = "candidate_id"
                    )
                    ddb_write_table(
                        conn,
                        "subset_candidates",
                        as.data.frame(saved),
                        overwrite = TRUE
                    )
                    state$candidates <- unique(
                        data.table::rbindlist(
                            list(state$candidates, additions),
                            fill = TRUE
                        ),
                        by = "candidate_id"
                    )
                }
            } else {
                subset__attempt(
                    store,
                    state$job,
                    client,
                    is.null(state$error),
                    if (is.null(state$error)) {
                        NA_character_
                    } else {
                        conditionMessage(state$error)
                    },
                    state$result
                )
                if (
                    !is.null(state$error) &&
                        subset__failure_kind(state$error) == "internal"
                ) {
                    stop(state$error)
                }
                if (is.null(state$error)) {
                    blocks <- subset__valid_blocks(store, state$key)
                    subset__save_demands(conn, state$result$demands, dest)
                    covered <- subset__coverage(
                        state$links,
                        data.table::as.data.table(ddb_read_table(
                            conn,
                            "subset_demands"
                        )),
                        data.table::as.data.table(ddb_read_table(
                            conn,
                            "subset_neighborhoods"
                        )),
                        blocks
                    )
                    if (!all(covered)) {
                        subset__job_state(
                            store,
                            state$key,
                            state$signature,
                            "failed",
                            "No native values in the requested windows."
                        )
                        state$finished <- TRUE
                        state$ticket <- NULL
                        state$job <- NULL
                        next
                    }
                    contract <- if (nrow(blocks)) {
                        unique(blocks$contract)[[1L]]
                    } else {
                        NA_character_
                    }
                    metadata <- file.path(
                        dest,
                        "subsets",
                        paste0("sites-", state$signature, ".rds")
                    )
                    store__extract_cache_write(
                        metadata,
                        list(neighborhoods = state$result$neighborhoods)
                    )
                    subset__save_membership(
                        conn,
                        state$key,
                        state$result$neighborhoods
                    )
                    subset__job_state(
                        store,
                        state$key,
                        state$signature,
                        "done",
                        contract = contract
                    )
                    state$finished <- TRUE
                }
            }
            state$ticket <- NULL
            state$job <- NULL
        }
        for (state in states[order(vapply(
            states,
            function(s) s$round,
            integer(1L)
        ))]) {
            if (active >= workers) {
                break
            }
            if (state$finished || !is.null(state$ticket)) {
                next
            }
            available <- state$candidates[
                !vapply(
                    state$candidates$candidate_id,
                    exists,
                    logical(1L),
                    envir = state$tried,
                    inherits = FALSE
                )
            ]
            kind <- "read"
            if (!nrow(available)) {
                if (!state$discovered) {
                    kind <- "discover"
                } else if (state$round < retries && nrow(state$candidates)) {
                    state$round <- state$round + 1L
                    state$tried <- new.env(hash = TRUE, parent = emptyenv())
                    next
                } else {
                    subset__job_state(
                        store,
                        state$key,
                        state$signature,
                        "failed",
                        if (is.null(state$error)) {
                            "No compatible OPeNDAP candidate."
                        } else {
                            conditionMessage(state$error)
                        }
                    )
                    state$finished <- TRUE
                    next
                }
            }
            attempts <- ddb_read_table(conn, "subset_attempts")
            available <- subset__rank(available, attempts, client)
            if (kind == "read") {
                occupied <- vapply(
                    Filter(
                        function(s) {
                            !is.null(s$ticket) &&
                                s$ticket$status == "pending" &&
                                identical(s$job$kind, "read")
                        },
                        states
                    ),
                    function(s) s$job$data_node,
                    character(1L)
                )
                slots <- vapply(
                    available$data_node,
                    function(node) sum(occupied == node),
                    integer(1L)
                )
                available <- available[slots < node_workers]
                if (!nrow(available)) next
            }
            inbox <- tempfile(
                pattern = "attempt-",
                tmpdir = file.path(dest, "tmp")
            )
            dir.create(inbox)
            blocks <- if (resume) {
                subset__valid_blocks(store, state$key)
            } else {
                data.table::data.table()
            }
            contract <- if (nrow(blocks)) {
                unique(blocks$contract)[[1L]]
            } else {
                NULL
            }
            row <- if (nrow(available)) available[1L] else state$candidates[0L]
            job <- list(
                source_key = state$key,
                kind = kind,
                dest = dest,
                url = if (nrow(row)) row$url[[1L]] else NA_character_,
                data_node = if (nrow(row)) {
                    row$data_node[[1L]]
                } else {
                    NA_character_
                },
                variable_id = if (nrow(row)) {
                    row$variable_id[[1L]]
                } else {
                    NA_character_
                },
                links = state$links,
                completed = blocks,
                expected_bounds = subset__saved_bounds(blocks),
                contract = contract,
                inbox = inbox,
                subset_progress = file.path(inbox, "progress.json"),
                began = as.double(Sys.time()),
                files = if (kind == "discover") state$record else NULL
            )
            if (kind == "read") {
                state$tried[[row$candidate_id[[1L]]]] <- TRUE
            }
            state$job <- job
            state$result <- NULL
            state$error <- NULL
            state$ticket <- local({
                target <- state
                pool$submit(
                    list(job),
                    if (kind == "read") {
                        subset__read_source
                    } else {
                        subset__discover
                    },
                    collect = function(job, value) target$result <- value,
                    on_error = function(job, error) target$error <- error
                )
            })
            subset__job_state(store, state$key, state$signature, "running")
            active <- active + 1L
        }
        Sys.sleep(.02)
    }
    if (!is.null(pool)) {
        pool$close()
        pool <- NULL
    }
    jobs <- ddb_read_table(conn, "subset_jobs")
    list(
        dest = dest,
        jobs = jobs,
        unmatched_requests = links[is.na(links$source_key)],
        complete = all(jobs$status[jobs$source_key %in% keys] == "done") &&
            !anyNA(links$source_key)
    )
}

#' Inspect saved OPeNDAP subset acquisition
#'
#' These views read only the local acquisition manifest. They never contact an
#' ESGF index or data node, and data-node observations retain their client and age.
#' @param dest Acquisition directory returned by `fetch_subset()`.
#' @param view One of summary, blocks, attempts, nodes, links or candidates.
#' @return A summary list or a data.table of local manifest records.
#' @export
subset_status <- function(
    dest,
    view = c("summary", "blocks", "attempts", "nodes", "links", "candidates")
) {
    view <- match.arg(view)
    conn <- ddb_connect(
        file.path(path.expand(dest), "manifest.duckdb"),
        read_only = TRUE
    )
    on.exit(ddb_disconnect(conn), add = TRUE)
    if (view == "summary") {
        jobs <- data.table::as.data.table(ddb_read_table(conn, "subset_jobs"))
        count <- ddb_query(
            conn,
            paste(
                "SELECT count(*) AS blocks,",
                "coalesce(sum(lat_count * lon_count * time_count), 0) AS native_values FROM subset_blocks"
            )
        )
        return(list(
            jobs = jobs,
            blocks = count$blocks[[1L]],
            native_values = count$native_values[[1L]],
            observed_at = as.double(Sys.time())
        ))
    }
    if (view == "nodes") {
        return(data.table::as.data.table(ddb_query(
            conn,
            paste(
                "SELECT data_node, client, sum(CASE WHEN success AND native_values > 0 THEN 1 ELSE 0 END) AS data_successes,",
                "sum(CASE WHEN NOT success AND error_kind = 'transport' THEN 1 ELSE 0 END) AS failures,",
                "sum(native_values) AS native_values, max(observed_at) AS observed_at,",
                "median(CASE WHEN success AND open_seconds > 0 THEN open_seconds ELSE NULL END) AS open_seconds,",
                "median(CASE WHEN success AND native_values > 0 THEN elapsed_seconds / native_values ELSE NULL END) AS seconds_per_value",
                "FROM subset_attempts GROUP BY data_node, client"
            )
        )))
    }
    data.table::as.data.table(ddb_read_table(conn, paste0("subset_", view)))
}

#' Read locally saved raw climate neighborhoods
#'
#' Returns the unique native values together with city/corner membership and
#' explicit acquisition gaps. No interpolation or implicit remote access occurs.
#' @param dest Acquisition directory.
#' @param case_id Optional case labels to select.
#' @param site_id Optional site identifiers to select.
#' @param variable Optional climate variable names to select.
#' @param time Optional pair of native-calendar endpoints.
#' @return A list with values, neighborhoods, links and gaps.
#'   `gaps$incomplete_requests` identifies selected requests whose exact native
#'   coverage or checked selection receipt is missing, including empty native
#'   windows. A later successful window never clears an earlier request's gap.
#' @export
read_subset <- function(
    dest,
    case_id = NULL,
    site_id = NULL,
    variable = NULL,
    time = NULL
) {
    conn <- ddb_connect(
        file.path(path.expand(dest), "manifest.duckdb"),
        read_only = TRUE
    )
    on.exit(ddb_disconnect(conn), add = TRUE)
    links <- data.table::as.data.table(ddb_read_table(conn, "subset_links"))
    wanted_cases <- case_id
    wanted_sites <- site_id
    wanted_variables <- variable
    if (!is.null(wanted_cases)) {
        links <- links[links$case_id %in% wanted_cases]
    }
    if (!is.null(wanted_sites)) {
        links <- links[links$site_id %in% wanted_sites]
    }
    if (!is.null(wanted_variables)) {
        inventory <- ddb_read_table(conn, "subset_candidates")
        selected_sources <- inventory$source_key[
            inventory$variable_id %in% wanted_variables
        ]
        links <- links[
            is.na(links$source_key) | links$source_key %in% selected_sources
        ]
    }
    blocks <- data.table::as.data.table(ddb_read_table(conn, "subset_blocks"))
    blocks <- blocks[blocks$source_key %in% links$source_key]
    membership <- unique(data.table::as.data.table(ddb_read_table(
        conn,
        "subset_neighborhoods"
    )))
    neighborhoods <- merge(
        links,
        membership,
        by.x = c("source_key", "lon", "lat"),
        by.y = c("source_key", "target_lon", "target_lat"),
        sort = FALSE,
        allow.cartesian = TRUE
    )
    pieces <- vector("list", nrow(blocks))
    invalid <- character(nrow(blocks))
    invalid_count <- 0L
    valid <- rep.int(TRUE, nrow(blocks))
    for (i in seq_len(nrow(blocks))) {
        path <- blocks$path[[i]]
        if (!subset__block_valid(blocks[i])) {
            invalid_count <- invalid_count + 1L
            invalid[[invalid_count]] <- blocks$block_id[[i]]
            valid[[i]] <- FALSE
            next
        }
        values <- data.table::as.data.table(ddb_query(
            conn,
            sprintf("SELECT * FROM read_parquet(%s)", ddb_literal(conn, path))
        ))
        if (!is.null(wanted_variables)) {
            values <- values[values$variable %in% wanted_variables]
        }
        needed <- neighborhoods[
            neighborhoods$source_key == blocks$source_key[[i]]
        ]
        keep <- rep.int(FALSE, nrow(values))
        for (j in seq_len(nrow(needed))) {
            positions <- which(
                values$ind_lat == needed$ind_lat[[j]] &
                    values$ind_lon == needed$ind_lon[[j]]
            )
            if (!length(positions)) {
                next
            }
            native <- values[positions]
            selected <- dataset__neighborhood_time_indices(
                native$time,
                as.data.frame(native[,
                    CF_TIME_COORDINATE_COLUMNS,
                    with = FALSE
                ]),
                c(needed$time_start[[j]], needed$time_stop[[j]])
            )
            keep[positions[selected]] <- TRUE
        }
        values <- values[keep]
        if (!is.null(time) && nrow(values)) {
            values <- values[dataset__neighborhood_time_indices(
                values$time,
                as.data.frame(values[,
                    CF_TIME_COORDINATE_COLUMNS,
                    with = FALSE
                ]),
                time
            )]
        }
        pieces[[i]] <- values
    }
    jobs <- data.table::as.data.table(ddb_read_table(conn, "subset_jobs"))
    coverage <- subset__coverage(
        links,
        data.table::as.data.table(ddb_read_table(conn, "subset_demands")),
        membership,
        blocks[valid]
    )
    incomplete <- links[!coverage]
    affected <- unique(incomplete$source_key[!is.na(incomplete$source_key)])
    affected_jobs <- data.table::copy(jobs[jobs$source_key %in% affected])
    if (nrow(affected_jobs)) {
        data.table::set(affected_jobs, j = "status", value = "incomplete")
        data.table::set(
            affected_jobs,
            j = "error",
            value = "Selected requests have missing or empty native coverage."
        )
    }
    gaps <- list(
        unmatched = links[is.na(links$source_key)],
        incomplete_sources = affected_jobs,
        incomplete_requests = incomplete,
        invalid_blocks = invalid[seq_len(invalid_count)]
    )
    list(
        values = unique(data.table::rbindlist(pieces, fill = TRUE)),
        neighborhoods = neighborhoods,
        links = links,
        gaps = gaps
    )
}

# vim: fdm=marker :
