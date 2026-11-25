# Describe a selected climate file independently of extraction mode. Publisher
# content evidence can identify replicas; weak records retain their full HTTP
# request and catalogue identity, including resource-selecting query strings.
# source_recovery__describe {{{
source_recovery__describe <- function(file) {
    fields <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "table_id",
        "variable_id",
        "frequency",
        "title",
        "version",
        "tracking_id"
    )
    identity <- stats::setNames(
        lapply(fields, function(name) {
            store__chr1(file[[name]])
        }),
        fields
    )
    algorithm <- tolower(store__chr1(file$checksum_type))
    checksum <- tolower(store__chr1(file$checksum))
    supported <- !is.na(algorithm) &&
        algorithm %in% c("md5", "sha256") &&
        !is.na(checksum) &&
        nzchar(checksum)
    list(
        schema = "epwshiftr-source-request-v1",
        identity = identity,
        file_key = store__chr1(file$file_key),
        endpoint = store__chr1(file$url_download),
        filename = store__chr1(file$filename),
        checksum_type = if (supported) algorithm else NA_character_,
        checksum = if (supported) checksum else NA_character_
    )
}
# }}}

# Use exactly the same identity for reader exclusion and recovery location. A
# tracking identifier alone cannot certify bytes from another HTTP resource.
# source_recovery__key {{{
source_recovery__key <- function(request) {
    identity <- list(
        identity = request$identity,
        filename = request$filename,
        checksum_type = request$checksum_type,
        checksum = request$checksum
    )
    if (is.na(request$checksum) || is.na(request$identity$version)) {
        identity$file_key <- request$file_key
        identity$endpoint <- request$endpoint
    }
    checksum_bytes(serialize(identity, NULL))
}
# }}}

# Compare known scientific fields before considering record or replica evidence.
# Missing source fields cannot establish cross-resource version equivalence.
# source_recovery__compatible {{{
source_recovery__compatible <- function(request, owner) {
    if (!identical(request$filename, owner$filename)) {
        return(FALSE)
    }
    # Empty or missing metadata cannot establish a source identity claim.
    known <- function(x) {
        !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    }
    for (name in names(request$identity)) {
        wanted <- request$identity[[name]]
        saved <- owner$identity[[name]]
        if (known(wanted) && known(saved) && !identical(wanted, saved)) {
            return(FALSE)
        }
    }
    same_resource <- identical(request$file_key, owner$file_key) &&
        identical(request$endpoint, owner$endpoint)
    if (same_resource) {
        return(TRUE)
    }
    if (
        !known(request$checksum) ||
            !known(owner$checksum) ||
            !identical(request$checksum_type, owner$checksum_type) ||
            !identical(request$checksum, owner$checksum) ||
            !known(request$identity$version) ||
            !known(owner$identity$version)
    ) {
        return(FALSE)
    }
    all(vapply(
        names(request$identity),
        function(name) {
            wanted <- request$identity[[name]]
            !known(wanted) || identical(wanted, owner$identity[[name]])
        },
        logical(1L)
    ))
}
# }}}

# Keep the original task owner beside its Downloader manifest. Atomic JSON
# publication is independent of transport metadata and never relabels bytes.
# source_recovery__owner_path {{{
source_recovery__owner_path <- function(manifest, task_id) {
    file.path(dirname(manifest), "source-owners", paste0(task_id, ".json"))
}
# }}}

# Reconstruct only evidence actually present in an old task. Standard DRS names
# encode field/model/member identity; version-bearing saved IDs encode version.
# Unrecorded legacy metadata remains missing instead of inheriting new claims.
# source_recovery__legacy_owner {{{
source_recovery__legacy_owner <- function(task, request) {
    owner <- request
    owner$identity[] <- NA_character_
    owner$identity$title <- store__chr1(task$filename)
    filename <- store__drs_file_name(task$filename)
    if (!is.na(filename)) {
        pieces <- strsplit(filename, "_", fixed = TRUE)[[1L]]
        fields <- c(
            "variable_id",
            "table_id",
            "source_id",
            "experiment_id",
            "variant_label",
            "grid_label"
        )
        owner$identity[fields] <- as.list(pieces[seq_along(fields)])
    }
    ids <- c(store__chr1(task$dataset_id), store__chr1(task$esgf_id))
    versions <- ids[!is.na(ids) & grepl("[.]v[0-9]+([.]|[|]|$)", ids)]
    versions <- unique(sub("^.*[.]v([0-9]+)([.]|[|]|$).*$", "\\1", versions))
    if (length(versions) == 1L) {
        owner$identity$version <- versions[[1L]]
    }
    for (name in intersect(names(owner$identity), names(task))) {
        value <- store__chr1(task[[name]])
        if (!is.na(value)) owner$identity[[name]] <- value
    }
    owner
}
# }}}

# A candidate URL and saved task identifiers must corroborate the owner record.
# Legacy tasks with no receipt are adopted only from their original record and
# endpoint; DRS names or matching directory names alone never grant ownership.
# source_recovery__select {{{
source_recovery__select <- function(downloader, request, file) {
    tasks <- data.table::as.data.table(downloader$tasks())
    if (!nrow(tasks)) {
        return(NULL)
    }
    candidates <- priv(downloader)$read_table("download_candidate")
    for (index in rev(seq_len(nrow(tasks)))) {
        task <- tasks[index]
        urls <- candidates$url[candidates$task_id == task$task_id[[1L]]]
        urls <- unique(urls[!is.na(urls) & nzchar(urls)])
        if (
            !length(urls) ||
                !identical(store__chr1(task$filename), request$filename)
        ) {
            next
        }
        owner_path <- source_recovery__owner_path(
            downloader$manifest,
            task$task_id[[1L]]
        )
        owner <- if (file.exists(owner_path)) {
            tryCatch(
                jsonlite::read_json(owner_path, simplifyVector = TRUE),
                error = function(error) NULL
            )
        } else {
            NULL
        }
        if (!is.null(owner)) {
            # JSON nulls represent missing source evidence, not a distinct
            # identity. Restore the descriptor's scalar missing-value contract.
            owner$request$identity <- lapply(
                owner$request$identity,
                store__chr1
            )
            for (name in c(
                "file_key",
                "endpoint",
                "filename",
                "checksum_type",
                "checksum"
            )) {
                owner$request[[name]] <- store__chr1(owner$request[[name]])
            }

            if (
                !identical(owner$task_id, task$task_id[[1L]]) ||
                    !identical(owner$session_id, task$session_id[[1L]]) ||
                    !source_recovery__compatible(request, owner$request)
            ) {
                next
            }
            original <- owner$request
            # Every actual candidate must belong to the saved request. Verified
            # replicas may be selected through their identical publisher hash.
            if (is.na(original$checksum) && !all(urls == original$endpoint)) {
                next
            }
        } else {
            selected <- store__chr1(task$selected_url)
            if (
                !identical(store__chr1(task$file_key), request$file_key) ||
                    !all(urls == request$endpoint) ||
                    (!is.na(selected) &&
                        !identical(selected, request$endpoint)) ||
                    !identical(
                        store__chr1(task$dataset_id),
                        store__chr1(file$dataset_id)
                    )
            ) {
                next
            }
            saved_id <- store__chr1(task$esgf_id)
            if (
                !is.na(saved_id) &&
                    !identical(saved_id, store__chr1(file$esgf_id))
            ) {
                next
            }
            original <- source_recovery__legacy_owner(task, request)
            if (!source_recovery__compatible(request, original)) next
        }
        expected <- store__chr1(task$checksum)
        algorithm <- tolower(store__chr1(task$checksum_type))
        if (
            !is.na(request$checksum) &&
                (!identical(tolower(expected), request$checksum) ||
                    !identical(algorithm, request$checksum_type))
        ) {
            next
        }
        if (is.na(request$checksum) && !is.na(expected)) {
            next
        }
        return(list(task = task, owner = original))
    }
    NULL
}
# }}}

# Recover one exact task under a lock held from construction through connection
# close. The outer source lock is separate from Downloader's reentrant operation
# lock, so internal manifest writes retain their existing lock contract.
# source_recovery__download {{{
source_recovery__download <- function(
    file,
    config,
    overwrite = FALSE,
    reporter = NULL
) {
    request <- source_recovery__describe(file)
    if (is.na(request$endpoint)) {
        stop(
            "HTTPServer download URL is not available for this file record.",
            call. = FALSE
        )
    }
    legacy <- attr(config, "legacy_manifests", exact = TRUE)
    manifests <- unique(c(config$manifest, legacy))
    manifests <- c(manifests[file.exists(manifests)], config$manifest)
    manifests <- unique(manifests)
    # Each candidate is inspected and executed under its own lifetime lock;
    # returning NULL preserves ineligible history before trying another location.
    recover <- function(manifest, create) {
        dir.create(dirname(manifest), recursive = TRUE, showWarnings = FALSE)
        release <- manifest_acquire_lock(
            file.path(dirname(manifest), "source-transfer"),
            timeout = 5
        )
        # LIFO cleanup is explicit: the DuckDB connection must close before the
        # lifecycle lock permits a replacement reader to construct Downloader.
        downloader <- NULL
        on.exit(
            {
                if (!is.null(downloader)) {
                    priv(downloader)$disconnect_manifest()
                }
                release()
            },
            add = TRUE
        )
        args <- if (file.exists(manifest)) {
            config[setdiff(names(config), c("dest", "temp"))]
        } else {
            config
        }
        args$manifest <- manifest
        downloader <- do.call(Downloader$new, args)
        selected <- source_recovery__select(downloader, request, file)
        if (is.null(selected) && !create) {
            return(NULL)
        }
        if (is.null(selected)) {
            plan <- store__download_plan_rows(file)
            data.table::set(plan, j = "checksum", value = request$checksum)
            data.table::set(
                plan,
                j = "checksum_type",
                value = if (is.na(request$checksum_type)) {
                    "sha256"
                } else {
                    request$checksum_type
                }
            )
            # Both session and destination distinguish a fresh request from
            # unrelated or unverifiable retained data, including orphan finals.
            generation <- basename(tempfile("recovery-"))
            data.table::set(
                plan,
                j = "subdir",
                value = file.path(
                    "source-recovery",
                    source_recovery__key(request),
                    generation
                )
            )
            session <- downloader$enqueue(
                plan,
                session_label = "source-http-fallback"
            )
            task <- data.table::as.data.table(downloader$tasks(
                session_id = session
            ))
            selected <- list(task = task, owner = request)
        }
        task <- selected$task
        stopifnot(nrow(task) == 1L)
        owner_path <- source_recovery__owner_path(manifest, task$task_id[[1L]])
        if (!file.exists(owner_path)) {
            dir.create(
                dirname(owner_path),
                recursive = TRUE,
                showWarnings = FALSE
            )
            store_write_json_atomic(
                list(
                    schema = "epwshiftr-source-owner-v1",
                    request = selected$owner,
                    session_id = task$session_id[[1L]],
                    task_id = task$task_id[[1L]]
                ),
                owner_path,
                auto_unbox = TRUE,
                na = "null",
                null = "null"
            )
        }
        # Workflow callbacks remain an adapter concern; execution still uses the
        # existing synchronous transport and only this selected task.
        unbind <- if (!is.null(reporter)) {
            shift_run__download_reporter_bind(
                downloader,
                reporter,
                role = "HTTP fallback",
                variables = 1L,
                nested = TRUE
            )
        } else {
            NULL
        }
        if (!is.null(unbind)) {
            on.exit(unbind(), add = TRUE)
        }
        tasks <- downloader$resume(
            session_id = task$session_id[[1L]],
            task_id = task$task_id[[1L]],
            progress = is.null(reporter) && isTRUE(attr(config, "progress")),
            overwrite = overwrite,
            resume = TRUE
        )
        stopifnot(nrow(tasks) == 1L, identical(tasks$task_id, task$task_id))
        if (!tasks$status[[1L]] %in% c("done", "skipped")) {
            stop(
                paste("HTTPServer download failed:", tasks$last_error[[1L]]),
                call. = FALSE
            )
        }
        verified <- downloader$verify(
            session_id = task$session_id[[1L]],
            task_id = task$task_id[[1L]]
        )
        if (nrow(verified) != 1L || !isTRUE(verified$checksum_ok[[1L]])) {
            stop(
                "HTTPServer publisher checksum verification failed.",
                call. = FALSE
            )
        }
        path <- tasks$target_path[[1L]]
        bytes <- as.double(file.info(path)$size)
        expected_size <- suppressWarnings(as.double(file$size[[1L]]))
        if (
            length(expected_size) == 1L &&
                !is.na(expected_size) &&
                bytes != expected_size
        ) {
            stop("HTTPServer source length verification failed.", call. = FALSE)
        }
        publisher_verified <- !is.na(request$checksum) &&
            identical(
                tolower(checksum_file(path, request$checksum_type)),
                request$checksum
            )
        if (!is.na(request$checksum) && !publisher_verified) {
            stop(
                "HTTPServer publisher checksum verification failed.",
                call. = FALSE
            )
        }
        receipt <- list(
            schema = "epwshiftr-source-receipt-v1",
            request = request,
            owner = selected$owner,
            manifest = store_normalize_path(manifest),
            session_id = task$session_id[[1L]],
            task_id = task$task_id[[1L]],
            source_url = store__chr1(tasks$selected_url),
            final_url = store__chr1(tasks$transfer_final_url),
            path = store_normalize_path(path),
            bytes = bytes,
            local_sha256 = checksum_file(path),
            checksum_verified = publisher_verified,
            verification_method = if (publisher_verified) {
                "publisher-checksum"
            } else {
                "local-sha256"
            },
            task = as.list(tasks[1L, , drop = FALSE])
        )
        list(path = path, manifest = manifest, receipt = receipt)
    }
    for (manifest in manifests) {
        result <- recover(
            manifest,
            create = identical(manifest, config$manifest) &&
                identical(manifest, tail(manifests, 1L))
        )
        if (!is.null(result)) return(result)
    }
    recover(config$manifest, create = TRUE)
}
# }}}

# A verified native transfer may satisfy several replica catalogue requests.
# Preserve its actual owner/task while attaching each caller's own descriptor.
# source_recovery__receipt_for {{{
source_recovery__receipt_for <- function(file, receipt) {
    request <- source_recovery__describe(file)
    if (!source_recovery__compatible(request, receipt$owner)) {
        stop(
            "Source receipt does not satisfy this catalogue request.",
            call. = FALSE
        )
    }
    receipt$request <- request
    receipt
}
# }}}

# Validate a reader's exact request receipt before linking its native file to
# this Store. Locally calculated hashes and publisher verification remain
# separate facts in the persisted source reference consumed on later readback.
# source_recovery__register {{{
source_recovery__register <- function(store, file, receipt) {
    request <- source_recovery__describe(file)
    if (
        !identical(request, receipt$request) ||
            !source_recovery__compatible(request, receipt$owner)
    ) {
        stop(
            "Source receipt does not belong to the requesting catalogue record.",
            call. = FALSE
        )
    }
    path <- receipt$path
    if (
        !file.exists(path) ||
            !identical(checksum_file(path), receipt$local_sha256)
    ) {
        stop(
            "Source receipt local content hash does not match its native file.",
            call. = FALSE
        )
    }
    root <- store_normalize_path(store$path)
    inside <- startsWith(
        store_normalize_path(path),
        paste0(sub("/+$", "", root), "/")
    )
    # The small source reference is store-owned even for external shared bytes.
    reference <- list(
        schema = "epwshiftr-external-download-reference-v1",
        file_key = request$file_key,
        logical_file_id = receipt$task$logical_file_id[[1L]],
        source_url = receipt$source_url,
        checksum_type = request$checksum_type,
        checksum = request$checksum,
        checksum_verified_by_reader = receipt$checksum_verified,
        bytes = receipt$bytes,
        cache_path = receipt$path,
        cache_manifest = receipt$manifest,
        request = request,
        owner = receipt$owner,
        session_id = receipt$session_id,
        task_id = receipt$task_id,
        final_url = receipt$final_url,
        local_sha256 = receipt$local_sha256,
        verification_method = receipt$verification_method
    )
    reference_path <- file.path(
        root,
        "sources",
        paste0("pool-download-", store__hash(request$file_key), ".json")
    )
    previous <- if (file.exists(reference_path)) {
        jsonlite::read_json(reference_path)
    } else {
        NULL
    }
    if (
        !identical(
            jsonlite::toJSON(
                previous,
                auto_unbox = TRUE,
                digits = NA,
                null = "null"
            ),
            jsonlite::toJSON(
                reference,
                auto_unbox = TRUE,
                digits = NA,
                na = "null",
                null = "null"
            )
        )
    ) {
        store_write_json_atomic(
            reference,
            reference_path,
            auto_unbox = TRUE,
            digits = NA,
            na = "null",
            null = "null"
        )
    }
    store$register_artifact(
        kind = "source",
        path = reference_path,
        role = "source",
        project = "CMIP6",
        query_id = file$query_id[[1L]],
        file_key = request$file_key,
        source_url = receipt$source_url,
        metadata = list(
            external_download_reference = !inside,
            source_receipt = TRUE
        )
    )
    if (inside) {
        artifact_id <- store$register_artifact(
            kind = "netcdf",
            path = path,
            role = "download",
            project = "CMIP6",
            query_id = file$query_id[[1L]],
            file_key = request$file_key,
            source_url = receipt$source_url,
            checksum = receipt$local_sha256,
            checksum_type = "sha256",
            metadata = list(
                filename = request$filename,
                source_receipt = reference_path
            )
        )
        row <- data.table::copy(file)
        data.table::set(
            row,
            j = "local_path",
            value = store_rel_path(path, root)
        )
        data.table::set(row, j = "local_artifact_id", value = artifact_id)
        priv(store)$replace_rows("file_catalog", as.data.frame(row), "file_key")
        priv(store)$sync_tracked_file_download(
            request$file_key,
            path,
            artifact_id
        )
    }
    invisible(reference)
}
# }}}

# vim: fdm=marker :
