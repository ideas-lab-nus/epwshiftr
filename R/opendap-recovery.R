# Validate portable endpoint evidence against the original File catalogue.
# The published physical version describes the URL, not the frozen logical key.
opendap_recovery__add_replicas <- function(files, candidates, replicas) {
    if (is.null(replicas)) {
        return(candidates)
    }
    fields <- c(
        "source_key",
        "url",
        "logical_version",
        "physical_version",
        "tracking_id",
        "checksum",
        "size",
        "receipt_sha256"
    )
    checkmate::assert_data_frame(replicas)
    checkmate::assert_names(names(replicas), must.include = fields)
    if (!nrow(replicas)) {
        return(candidates)
    }
    for (field in fields) {
        checkmate::assert_character(
            replicas[[field]],
            any.missing = FALSE,
            min.chars = 1L
        )
    }
    stopifnot(
        !anyDuplicated(replicas[c("source_key", "url")]),
        all(grepl("^[[:xdigit:]]{64}$", replicas$checksum)),
        all(grepl("^[[:xdigit:]]{64}$", replicas$receipt_sha256)),
        all(grepl("^https?://[^/?#@]+/", replicas$url)),
        all(grepl("^[0-9]+$", replicas$size))
    )
    docs <- files$to_data_table(fields = c("tracking_id", "title"))
    filename <- basename(sub("[?#].*$", "", replicas$url))
    additions <- lapply(seq_len(nrow(replicas)), function(i) {
        entry <- replicas[i, , drop = FALSE]
        match <- which(
            candidates$source_key == entry$source_key &
                candidates$version == entry$logical_version &
                toupper(candidates$checksum_type) == "SHA256" &
                tolower(candidates$checksum) == tolower(entry$checksum) &
                candidates$size == entry$size &
                docs$tracking_id[candidates$record_index] == entry$tracking_id &
                docs$title[candidates$record_index] == filename[[i]]
        )
        if (!length(match)) {
            stop(
                "Verified replica does not match the frozen File identity.",
                call. = FALSE
            )
        }
        row <- data.table::copy(candidates[match[1L]])
        data.table::set(row, j = "url", value = entry$url)
        data.table::set(
            row,
            j = "data_node",
            value = sub("^https?://([^/]+).*", "\\1", entry$url)
        )
        data.table::set(
            row,
            j = "candidate_id",
            value = store__hash(entry$source_key, entry$url)
        )
        data.table::set(
            row,
            j = "priority",
            value = max(candidates$priority) + i
        )
        row
    })
    unique(
        data.table::rbindlist(c(list(candidates), additions)),
        by = "candidate_id"
    )
}

# Retain the verified physical source independently of the candidate's logical identity.
opendap_recovery__record_replicas <- function(conn, replicas) {
    if (is.null(replicas) || !nrow(replicas)) {
        return(invisible(NULL))
    }
    ddb_exec(
        conn,
        paste(
            "CREATE TABLE IF NOT EXISTS subset_verified_replicas (source_key VARCHAR,",
            "url VARCHAR, logical_version VARCHAR, physical_version VARCHAR, tracking_id VARCHAR,",
            "checksum VARCHAR, size VARCHAR, receipt_sha256 VARCHAR)"
        )
    )
    prior <- opendap__source_rows(
        conn,
        "subset_verified_replicas",
        unique(replicas$source_key)
    )
    rows <- data.table::as.data.table(replicas)[, names(prior), with = FALSE]
    new <- data.table::fsetdiff(rows, prior)
    if (nrow(new)) {
        ddb_append_table(conn, "subset_verified_replicas", as.data.frame(new))
    }
    invisible(NULL)
}

# Copy and verify a bounded, explicitly selected manifest artifact before any
# manifest transaction registers its new location.
opendap_recovery__copy_artifacts <- function(records, path, hash, from, to) {
    if (!nrow(records)) {
        return(records)
    }
    paths <- unique(records[, c(path, hash), with = FALSE])
    stopifnot(!anyDuplicated(paths[[path]]))
    targets <- vapply(
        seq_len(nrow(paths)),
        function(i) {
            source <- normalizePath(
                paths[[path]][[i]],
                winslash = "/",
                mustWork = TRUE
            )
            expected <- paths[[hash]][[i]]
            stopifnot(
                startsWith(source, paste0(from, "/")),
                identical(checksum_file(source), expected)
            )
            dest <- file.path(to, "subsets", basename(source))
            if (!file.exists(dest)) {
                pending <- tempfile(".epw-", tmpdir = dirname(dest))
                on.exit(unlink(pending), add = TRUE)
                stopifnot(
                    file.copy(source, pending),
                    identical(checksum_file(pending), expected)
                )
                if (!file.rename(pending, dest)) {
                    stop(
                        "Could not publish imported subset artifact.",
                        call. = FALSE
                    )
                }
            }
            if (!identical(checksum_file(dest), expected)) {
                stop(
                    "Imported subset artifact conflicts with destination bytes.",
                    call. = FALSE
                )
            }
            dest
        },
        ""
    )
    data.table::set(
        records,
        j = path,
        value = targets[match(records[[path]], paths[[path]])]
    )
    records
}

# Merge content, history, and keyed rows without silently replacing a valid
# committed block or window. File copies can be reused after an interrupted run.
opendap_recovery__merge_import <- function(
    target,
    table,
    incoming,
    prior,
    keys
) {
    if (!nrow(incoming)) {
        return(0L)
    }
    if (length(keys)) {
        common <- merge(
            incoming,
            prior,
            by = keys,
            suffixes = c(".incoming", ".prior")
        )
        if (table == "subset_blocks" && nrow(common)) {
            stable <- c("source_key", "contract", "sha256", "metadata_sha256")
            for (name in stable) {
                if (
                    any(
                        common[[paste0(name, ".incoming")]] !=
                            common[[paste0(name, ".prior")]]
                    )
                ) {
                    stop(
                        "Imported block conflicts with a committed block.",
                        call. = FALSE
                    )
                }
            }
            existing <- prior[incoming, on = keys, nomatch = 0L]
            stopifnot(all(vapply(
                seq_len(nrow(existing)),
                function(i) opendap__block_valid(existing[i]),
                TRUE
            )))
        }
        if (table == "subset_links" && nrow(common)) {
            # The same case label cannot be associated with different windows during import.
            stable <- c(
                "case_id",
                "site_id",
                "lon",
                "lat",
                "time_start",
                "time_stop"
            )
            for (name in stable) {
                x <- common[[paste0(name, ".incoming")]]
                y <- common[[paste0(name, ".prior")]]
                stopifnot(identical(x, y))
            }
        }
        incoming <- incoming[!prior, on = keys]
    } else {
        incoming <- data.table::fsetdiff(incoming, prior)
    }
    if (nrow(incoming)) {
        ddb_append_table(target, table, as.data.frame(incoming))
    }
    nrow(incoming)
}

#' Transfer verified native subset blocks between closed Stores
#'
#' Copy blocks and the associated native metadata, demand receipts, request
#' links and replica observations for selected source keys. SHA-256 checks
#' precede registration and a manifest transaction commits the imported rows.
#' Repeating the import reuses identical files and rows.
#'
#' @param from Path to an existing closed local subset Store.
#' @param dest Path to the destination local subset Store.
#' @param source_keys Character vector of exact frozen File keys.
#' @param artifact_root Directory containing the source manifest's artifacts.
#'   Defaults to the Store's subsets directory. An explicit root can identify
#'   the immutable object directory used by an older transfer receipt.
#' @return A named integer vector with the number of rows imported per table.
#' @export
import_subset_blocks <- function(
    from,
    dest,
    source_keys,
    artifact_root = file.path(from, "subsets")
) {
    checkmate::assert_character(
        source_keys,
        min.len = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    from <- normalizePath(from, winslash = "/", mustWork = TRUE)
    artifact_root <- normalizePath(
        artifact_root,
        winslash = "/",
        mustWork = TRUE
    )
    stopifnot(dir.exists(artifact_root))
    dest <- store_abs_path(path.expand(dest), getwd())
    stopifnot(!identical(from, dest))
    dir.create(
        file.path(dest, "subsets"),
        recursive = TRUE,
        showWarnings = FALSE
    )
    release <- manifest_acquire_lock(file.path(dest, "subset-recovery"))
    on.exit(release(), add = TRUE)
    source <- ddb_connect(file.path(from, "manifest.duckdb"), read_only = TRUE)
    on.exit(ddb_disconnect(source), add = TRUE)
    store <- opendap__open(dest)
    on.exit(store$close(), add = TRUE, after = FALSE)
    target <- priv(store)$conn
    tables <- list(
        subset_content = NULL,
        subset_candidates = "candidate_id",
        subset_metadata = NULL,
        subset_blocks = "block_id",
        subset_demands = "demand_id",
        subset_neighborhoods = NULL,
        subset_links = c("request_id", "source_key"),
        subset_attempts = NULL,
        subset_jobs = "source_key"
    )
    source_tables <- ddb_list_tables(source)
    tables <- tables[names(tables) %in% source_tables]
    stopifnot(all(
        c(
            "subset_content",
            "subset_blocks",
            "subset_demands",
            "subset_neighborhoods"
        ) %in%
            names(tables)
    ))
    incoming <- lapply(names(tables), function(table) {
        opendap__source_rows(source, table, source_keys)
    })
    names(incoming) <- names(tables)
    prior <- lapply(names(tables), function(table) {
        opendap__source_rows(target, table, source_keys)
    })
    names(prior) <- names(tables)
    counts <- stats::setNames(integer(length(tables)), names(tables))
    if (!all(source_keys %in% incoming$subset_blocks$source_key)) {
        stop(
            "A selected source has no committed blocks to import.",
            call. = FALSE
        )
    }
    for (key in source_keys) {
        blocks <- incoming$subset_blocks[
            incoming$subset_blocks$source_key == key
        ]
        contracts <- unique(c(
            blocks$contract,
            incoming$subset_metadata$contract[
                incoming$subset_metadata$source_key == key
            ],
            opendap__saved_identity(target, key)$contract
        ))
        if (length(contracts) > 1L) {
            stop(
                "Imported blocks conflict with the destination native contract.",
                call. = FALSE
            )
        }
        opendap_recovery__check_overlap(
            blocks,
            prior$subset_blocks[prior$subset_blocks$source_key == key]
        )
    }
    for (table in names(incoming)) {
        for (field in intersect(
            c("path", "metadata_path", "receipt_path"),
            names(incoming[[table]])
        )) {
            checksum <- if (field == "path") {
                "sha256"
            } else {
                sub("_path$", "_sha256", field)
            }
            incoming[[table]] <- opendap_recovery__copy_artifacts(
                incoming[[table]],
                field,
                checksum,
                artifact_root,
                dest
            )
        }
    }
    ddb_exec(target, "BEGIN TRANSACTION")
    committed <- FALSE
    on.exit(
        if (!committed) ddb_exec(target, "ROLLBACK"),
        add = TRUE,
        after = FALSE
    )
    opendap__record_content(target, incoming$subset_content)
    counts[["subset_content"]] <- nrow(data.table::fsetdiff(
        incoming$subset_content,
        prior$subset_content
    ))
    for (table in setdiff(names(incoming), "subset_content")) {
        counts[[table]] <- opendap_recovery__merge_import(
            target,
            table,
            incoming[[table]],
            prior[[table]],
            tables[[table]]
        )
    }
    if ("subset_verified_replicas" %in% source_tables) {
        opendap_recovery__record_replicas(
            target,
            opendap__source_rows(
                source,
                "subset_verified_replicas",
                source_keys
            )
        )
    }
    ddb_exec(target, "COMMIT")
    committed <- TRUE
    counts
}

# A distinct block ID covering existing native indices must be reconciled
# before import. Registering both would make offline reads ambiguous.
opendap_recovery__check_overlap <- function(incoming, prior) {
    for (i in which(!incoming$block_id %in% prior$block_id)) {
        row <- incoming[i]
        overlap <- rep(TRUE, nrow(prior))
        starts <- c("ind_lat", "ind_lon", "time_start_index")
        counts <- c("lat_count", "lon_count", "time_count")
        for (j in seq_along(starts)) {
            begin <- starts[[j]]
            count <- counts[[j]]
            overlap <- overlap &
                prior[[begin]] < row[[begin]] + row[[count]] &
                row[[begin]] < prior[[begin]] + prior[[count]]
        }
        if (any(overlap)) {
            stop(
                "Imported blocks overlap different committed native blocks.",
                call. = FALSE
            )
        }
    }
}

# Load the last SHA-addressed legacy generation. A fork or corrupt commit
# requires investigation rather than selecting an earlier, more convenient state.
opendap_recovery__load <- function(journal) {
    paths <- sort(
        list.files(
            journal,
            "^state-[0-9]{8}-[a-f0-9]{64}[.]rds$",
            full.names = TRUE
        ),
        decreasing = TRUE
    )
    if (!length(paths)) {
        stop("No frozen subset journal state found.", call. = FALSE)
    }
    latest <- basename(paths[[1L]])
    prefix <- substr(latest, 1L, 14L)
    stopifnot(
        sum(startsWith(basename(paths), prefix)) == 1L,
        file.size(paths[[1L]]) <= 16 * 1024^2,
        endsWith(latest, paste0(checksum_file(paths[[1L]]), ".rds"))
    )
    state <- readRDS(paths[[1L]])
    stopifnot(identical(state$generation, as.integer(substr(prefix, 7L, 14L))))
    state
}

# Use the same ownership folders as the legacy caller. A surviving owner or
# incomplete lock is rejected before opening any destination Store.
opendap_recovery__lock <- function(journal) {
    gate <- file.path(journal, "owner-acquisition")
    lock <- file.path(journal, "owner-lock")
    if (!dir.create(gate, showWarnings = FALSE)) {
        stop("The subset journal acquisition gate is occupied.", call. = FALSE)
    }
    on.exit(unlink(gate, recursive = TRUE))
    owner <- file.path(lock, "owner.rds")
    if (dir.exists(lock)) {
        old <- readRDS(owner)
        stopifnot(
            is.numeric(old$pid),
            length(old$pid) == 1L,
            is.finite(old$created)
        )
        running <- tryCatch(
            ps::ps_is_running(ps::ps_handle(
                old$pid,
                as.POSIXct(old$created, origin = "1970-01-01", tz = "UTC")
            )),
            no_such_process = function(error) FALSE
        )
        if (
            running ||
                !identical(
                    list.files(lock, all.files = TRUE, no.. = TRUE),
                    "owner.rds"
                )
        ) {
            stop(
                "The subset journal has a live or unverified owner.",
                call. = FALSE
            )
        }
        stopifnot(unlink(lock, recursive = TRUE) == 0L)
    }
    stopifnot(dir.create(lock))
    self <- ps::ps_handle()
    identity <- list(
        pid = Sys.getpid(),
        created = as.double(ps::ps_create_time(self)),
        command = ps::ps_cmdline(self)
    )
    saveRDS(identity, owner, compress = FALSE)
    function() {
        stopifnot(identical(readRDS(owner), identity))
        unlink(lock, recursive = TRUE)
    }
}

# Publish immutable generations in the original format, preserving every
# prior state and the admission budget even after a failure.
opendap_recovery__persist <- function(state, journal) {
    state$generation <- state$generation + 1L
    state$updated <- as.double(Sys.time())
    staged <- tempfile(".state-", tmpdir = journal)
    on.exit(unlink(staged))
    saveRDS(state, staged, version = 3L, compress = FALSE)
    name <- sprintf(
        "state-%08d-%s.rds",
        state$generation,
        checksum_file(staged)
    )
    stopifnot(!any(startsWith(list.files(journal), substr(name, 1L, 15L))))
    if (!file.rename(staged, file.path(journal, name))) {
        stop("Could not publish the subset journal generation.", call. = FALSE)
    }
    state
}

# Authenticate the original Mac delegation membership and policy without
# expanding the PC queue to include unassigned Mac Files.
opendap_recovery__delegation <- function(spec, identity) {
    if (is.null(identity$Mac_delegation)) {
        stopifnot(is.null(spec))
        return(list(keys = character(), holds = character()))
    }
    stopifnot(
        is.list(spec),
        identical(checksum_file(spec$manifest), identity$Mac_delegation),
        identical(checksum_file(spec$origin_policy), identity$Mac_origin_policy)
    )
    manifest <- jsonlite::read_json(spec$manifest)
    state <- file.path(dirname(spec$manifest), "delegation-state.rds")
    ref <- Filter(
        function(x) identical(x$path, "delegation-state.rds"),
        manifest$entries
    )
    stopifnot(
        length(ref) == 1L,
        identical(checksum_file(state), ref[[1L]]$sha256)
    )
    delegated <- readRDS(state)
    policy <- readRDS(spec$origin_policy)
    stopifnot(
        identical(delegated$requested_executor, identity$node),
        identical(
            delegated$original_identity[c("files", "requests", "denominator")],
            identity[c("files", "requests", "denominator")]
        ),
        identical(delegated$original_identity, policy$original_identity),
        !anyDuplicated(delegated$delegated_source_keys)
    )
    list(
        keys = delegated$delegated_source_keys,
        holds = policy$original_holds$source_key
    )
}

# Recover only the immutable File and case-window data selected by the journal.
opendap_recovery__inputs <- function(plan, denominator, identity, delegation) {
    paths <- file.path(plan, c("files.parquet", "requests.parquet"))
    metadata <- jsonlite::read_json(file.path(plan, "manifest.json"))
    stopifnot(
        identical(checksum_file(paths[[1L]]), identity$files),
        identical(checksum_file(paths[[2L]]), identity$requests),
        identical(checksum_file(denominator), identity$denominator),
        identical(metadata$files_sha256, identity$files),
        identical(metadata$requests_sha256, identity$requests)
    )
    frozen <- readRDS(denominator)
    stopifnot(
        identical(frozen$source_files_sha256, identity$files),
        identical(frozen$requests_sha256, identity$requests)
    )
    db <- ddb_connect(":memory:")
    on.exit(ddb_disconnect(db))
    read <- function(path) {
        data.table::as.data.table(ddb_query(
            db,
            paste0(
                "SELECT * FROM read_parquet(",
                ddb_literal(db, path),
                ")"
            )
        ))
    }
    files <- read(paths[[1L]])
    stopifnot(!anyDuplicated(files$source_key))
    delegated <- opendap_recovery__delegation(delegation, identity)
    stopifnot(all(delegated$keys %in% files$source_key[files$owner == "mac"]))
    files <- files[
        files$owner == identity$node | files$source_key %in% delegated$keys
    ]
    windows <- data.table::as.data.table(frozen$demands)
    windows <- windows[windows$source_key %in% files$source_key]
    index <- match(windows$source_key, files$source_key)
    stopifnot(
        setequal(windows$source_key, files$source_key),
        identical(windows$owner, files$owner[index]),
        identical(windows$batch, files$batch[index])
    )
    list(
        plan = plan,
        files = files,
        windows = windows,
        requests = read(paths[[2L]]),
        workers = metadata$capacities[[identity$node]],
        holds = delegated$holds
    )
}

# Join coverage against the frozen physical windows. An absent receipt
# remains eligible; an existing nonretryable gap or hold is never bypassed.
opendap_recovery__coverage <- function(windows, dest, holds) {
    keys <- c("source_key", "lon", "lat", "time_start", "time_stop")
    expected <- unique(windows[, keys, with = FALSE])
    known <- if (file.exists(file.path(dest, "manifest.duckdb"))) {
        subset_status(
            dest,
            "coverage",
            source_key = unique(expected$source_key)
        )
    }
    if (!is.null(known) && nrow(known)) {
        fields <- c(
            keys,
            "complete",
            "retryable",
            "retry_after",
            "available_native_values",
            "expected_native_values"
        )
        known <- unique(known[, fields, with = FALSE])
        stopifnot(!anyDuplicated(known, by = keys))
        expected <- merge(
            expected,
            known,
            by = keys,
            all.x = TRUE,
            sort = FALSE
        )
    } else {
        data.table::set(expected, j = "complete", value = FALSE)
        data.table::set(expected, j = "retryable", value = TRUE)
        data.table::set(expected, j = "retry_after", value = NA_real_)
        data.table::set(
            expected,
            j = "available_native_values",
            value = NA_real_
        )
        data.table::set(
            expected,
            j = "expected_native_values",
            value = NA_real_
        )
    }
    data.table::set(
        expected,
        i = which(is.na(expected$complete)),
        j = "complete",
        value = FALSE
    )
    data.table::set(
        expected,
        i = which(is.na(expected$retryable)),
        j = "retryable",
        value = TRUE
    )
    data.table::set(
        expected,
        i = which(expected$source_key %in% holds),
        j = "retryable",
        value = FALSE
    )
    expected
}

# Bind each request to its exact File key after matching the frozen case labels
# and physical windows, so Files in one batch can share the worker pool safely.
opendap_recovery__requests <- function(inputs, rows, windows, catalogue) {
    plan <- catalogue$access_plan(expand = FALSE)
    stopifnot(setequal(rows$source_key, plan$source_key))
    fields <- c("source_id", "experiment_id", "table_id", "variable_id")
    profiles <- unique(plan[, c("source_key", fields), with = FALSE])
    stopifnot(!anyDuplicated(profiles$source_key))
    physical <- c("lon", "lat", "time_start", "time_stop")
    selected <- merge(
        windows[, c("source_key", physical), with = FALSE],
        profiles,
        by = "source_key"
    )
    requests <- merge(
        selected,
        inputs$requests,
        by = c(fields, physical),
        sort = FALSE
    )
    stopifnot(
        nrow(unique(requests[, c("source_key", physical), with = FALSE])) ==
            nrow(windows)
    )
    data.table::set(
        requests,
        j = "request_id",
        value = paste(requests$source_key, requests$request_id, sep = "|")
    )
    stopifnot(!anyDuplicated(requests$request_id))
    requests
}

# Open an exact catalogue slice from the frozen manifest for one admission.
opendap_recovery__catalogue <- function(plan, rows) {
    stopifnot(
        length(unique(rows$catalogue)) == 1L,
        length(unique(rows$catalogue_sha256)) == 1L
    )
    path <- file.path(plan, rows$catalogue[[1L]])
    stopifnot(identical(checksum_file(path), rows$catalogue_sha256[[1L]]))
    indices <- sort(unique(unlist(lapply(
        rows$record_indices,
        jsonlite::fromJSON
    ))))
    esg_result("file")$load(path)$slice(indices)
}

#' Resume acquisition from an existing frozen subset plan
#'
#' Continue a SHA-addressed `recovery-v1` journal with the original File and
#' request tables. The same generation history, in-flight admission, counters,
#' and completed local blocks survive a restart. No new File is added to the
#' frozen selection.
#'
#' @param plan Directory containing the frozen `manifest.json`, File and
#'   request Parquet tables, and saved ESGF catalogues.
#' @param denominator Path to the original physical-window RDS.
#' @param journal Path to the existing `recovery-v1` directory.
#' @param dest Local subset Store. For a delegation, provide a named vector of
#'   Store paths keyed by original node, including the executor and `mac`.
#' @param verified_replicas Optional table supplied from already verified
#'   replica evidence. Required character columns are `source_key`, `url`,
#'   `logical_version`, `physical_version`, `tracking_id`, `checksum`
#'   (SHA-256), `size` and `receipt_sha256`. The package compares the frozen
#'   File's identity and checks native compatibility during acquisition.
#' @param delegation Optional list with paths `manifest` and `origin_policy`
#'   for an existing Mac delegation recorded by this journal.
#' @param max_batches Maximum number of batches processed by this invocation.
#'   Zero validates the input and journal without admitting a batch.
#' @param discover Whether to look for compatible replicas after known
#'   endpoints fail.
#' @return The latest journal state, including the frozen coverage summary
#'   after a run.
#' @export
resume_subsets <- function(
    plan,
    denominator,
    journal,
    dest,
    verified_replicas = NULL,
    delegation = NULL,
    max_batches = Inf,
    discover = TRUE
) {
    checkmate::assert_number(max_batches, lower = 0)
    stopifnot(is.infinite(max_batches) || max_batches == floor(max_batches))
    checkmate::assert_flag(discover)
    state <- opendap_recovery__load(journal)
    stopifnot(identical(state$configuration$version, 2L))
    identity <- state$configuration$identity
    inputs <- opendap_recovery__inputs(plan, denominator, identity, delegation)
    checkmate::assert_character(dest, min.len = 1L, any.missing = FALSE)
    if (is.null(names(dest)) && length(dest) == 1L) {
        names(dest) <- identity$node
    }
    owners <- unique(inputs$files$owner)
    stopifnot(!anyDuplicated(names(dest)), setequal(names(dest), owners))
    dest <- vapply(
        dest,
        function(path) store_abs_path(path.expand(path), getwd()),
        ""
    )
    stopifnot(!anyDuplicated(dest))
    batches <- unique(inputs$files[, c("batch", "owner"), with = FALSE])
    stopifnot(
        !anyDuplicated(batches$batch),
        all(names(state$attempted) %in% batches$batch)
    )
    if (!is.null(state$in_flight)) {
        rows <- inputs$files[inputs$files$batch == state$in_flight$batch]
        stopifnot(
            length(state$in_flight$source_keys) > 0L,
            !anyDuplicated(state$in_flight$source_keys),
            all(state$in_flight$source_keys %in% rows$source_key)
        )
    }
    if (max_batches == 0L) {
        return(state)
    }
    release <- opendap_recovery__lock(journal)
    on.exit(release(), add = TRUE)
    stopifnot(identical(opendap_recovery__load(journal), state))
    releases <- list()
    on.exit(
        for (unlock in rev(releases)) {
            unlock()
        },
        add = TRUE,
        after = FALSE
    )
    for (path in sort(unname(dest))) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        releases[[length(releases) + 1L]] <- manifest_acquire_lock(file.path(
            path,
            "subset-recovery"
        ))
    }
    opendap_recovery__run(
        state,
        journal,
        inputs,
        batches,
        dest,
        verified_replicas,
        max_batches,
        discover
    )
}

# Reuse one drained reader pool for sequential admissions. Failures leave the
# exact admission in flight, so retry never spends the same admission twice.
opendap_recovery__run <- function(
    state,
    journal,
    inputs,
    batches,
    dest,
    replicas,
    max_batches,
    discover
) {
    config <- state$configuration
    workers <- as.integer(inputs$workers)
    session <- opendap__session(workers, 90)
    on.exit(session$close(), add = TRUE)
    coverage <- lapply(names(dest), function(origin) {
        opendap_recovery__coverage(
            inputs$windows[inputs$windows$owner == origin],
            dest[[origin]],
            inputs$holds
        )
    })
    coverage <- data.table::rbindlist(coverage, use.names = TRUE)
    batch_by_key <- stats::setNames(inputs$files$batch, inputs$files$source_key)
    owner_by_batch <- stats::setNames(batches$owner, batches$batch)
    origin_by_key <- stats::setNames(
        inputs$files$owner,
        inputs$files$source_key
    )
    summarize <- function(rows = coverage) {
        files <- rows[,
            list(
                covered = all(complete),
                values = sum(available_native_values, na.rm = TRUE)
            ),
            by = "source_key"
        ]
        known <- !is.na(rows$expected_native_values)
        list(
            files_complete = sum(files$covered),
            files_partial = sum(!files$covered & files$values > 0),
            files_total = nrow(files),
            four_corner_windows_complete = sum(rows$complete),
            four_corner_windows_total = nrow(rows),
            known_axis_windows = sum(known),
            expected_native_values_known_axes = sum(rows$expected_native_values[
                known
            ]),
            available_native_values_known_axes = sum(
                rows$available_native_values[known],
                na.rm = TRUE
            ),
            global_native_value_denominator_known = all(known),
            scientific_acceptance = FALSE
        )
    }
    persist <- function(phase) {
        state$phase <- phase
        state$summary <- utils::modifyList(as.list(state$summary), summarize())
        state$summary$by_origin <- stats::setNames(
            lapply(names(dest), function(origin) {
                summarize(coverage[
                    unname(origin_by_key[coverage$source_key]) == origin
                ])
            }),
            names(dest)
        )
        opendap_recovery__persist(state, journal)
    }
    baseline <- summarize()
    if (is.null(state$round_start_complete)) {
        state$round_start_complete <- baseline$files_complete
    }
    if (is.null(state$round_start_values)) {
        state$round_start_values <- baseline$available_native_values_known_axes
    }
    if (
        identical(state$phase, "no_progress") && state$round < config$max_rounds
    ) {
        state$round <- state$round + 1L
        state$attempted[] <- FALSE
        state$round_start_complete <- baseline$files_complete
        state$round_start_values <- baseline$available_native_values_known_axes
    }
    admitted <- 0
    repeat {
        now <- as.double(Sys.time())
        if (is.null(state$in_flight)) {
            if (all(coverage$complete)) {
                return(persist("complete"))
            }
            if (admitted >= max_batches) {
                return(state)
            }
            if (now - state$started >= config$max_elapsed) {
                return(persist("budget_exhausted"))
            }
            if (state$round == 0L) {
                state$round <- 1L
            }
            retryable <- coverage[!coverage$complete & coverage$retryable]
            if (!nrow(retryable)) {
                return(persist("needs_attention"))
            }
            ready <- retryable[
                is.na(retryable$retry_after) | retryable$retry_after <= now
            ]
            untried <- setdiff(
                intersect(
                    batches$batch,
                    unname(batch_by_key[ready$source_key])
                ),
                names(state$attempted)[state$attempted]
            )
            if (!length(untried)) {
                # Preserve cooldown deadlines and original total wait budget.
                future <- retryable$retry_after[
                    !is.na(retryable$retry_after) & retryable$retry_after > now
                ]
                if (!nrow(ready) && length(future)) {
                    delay <- min(future) - now
                    if (
                        delay > config$max_wait - state$wait_seconds ||
                            delay >= config$max_elapsed - (now - state$started)
                    ) {
                        return(persist("deferred"))
                    }
                    state$wait_seconds <- state$wait_seconds + delay
                    state <- persist("waiting_for_eligibility")
                    Sys.sleep(delay)
                    next
                }
                summary <- summarize()
                progressed <- summary$files_complete >
                    state$round_start_complete ||
                    summary$available_native_values_known_axes >
                        state$round_start_values
                if (!progressed || state$round >= config$max_rounds) {
                    return(persist(
                        if (progressed) "budget_exhausted" else "no_progress"
                    ))
                }
                state$round <- state$round + 1L
                state$attempted[] <- FALSE
                state$round_start_complete <- summary$files_complete
                state$round_start_values <- summary$available_native_values_known_axes
                next
            }
            batch <- untried[[1L]]
            keys <- unique(ready$source_key[
                batch_by_key[ready$source_key] == batch
            ])
            state$in_flight <- list(
                batch = batch,
                source_keys = keys,
                started = now
            )
            state$admissions <- state$admissions + 1L
            state <- persist("recovering")
        }
        batch <- state$in_flight$batch
        owner <- owner_by_batch[[batch]]
        keys <- state$in_flight$source_keys
        pending <- coverage[
            coverage$source_key %in%
                keys &
                !coverage$complete &
                coverage$retryable
        ]
        if (nrow(pending)) {
            rows <- inputs$files[
                inputs$files$source_key %in% pending$source_key
            ]
            catalogue <- opendap_recovery__catalogue(inputs$plan, rows)
            request <- opendap_recovery__requests(
                inputs,
                rows,
                pending,
                catalogue
            )
            alternates <- if (!is.null(replicas)) {
                replicas[
                    replicas$source_key %in% rows$source_key,
                    ,
                    drop = FALSE
                ]
            }
            error <- tryCatch(
                {
                    opendap__fetch(
                        catalogue,
                        request,
                        dest[[owner]],
                        workers = workers,
                        timeout = 90,
                        discover = discover,
                        network_id = paste0("paper-", owner),
                        node_workers = 4L,
                        max_values = 65536L,
                        replica_cooldown = 60,
                        session = session,
                        verified_replicas = alternates
                    )
                    NULL
                },
                error = identity
            )
            if (!is.null(error)) {
                state$error <- conditionMessage(error)
                return(persist("blocked_error"))
            }
            checked <- inputs$windows[inputs$windows$source_key %in% keys]
            coverage <- data.table::rbindlist(
                list(
                    coverage[!coverage$source_key %in% keys],
                    opendap_recovery__coverage(
                        checked,
                        dest[[owner]],
                        inputs$holds
                    )
                ),
                use.names = TRUE
            )
        }
        state$attempted[[batch]] <- TRUE
        state["in_flight"] <- list(NULL)
        state["error"] <- list(NULL)
        admitted <- admitted + 1L
        state <- persist("recovering")
    }
}
