#' Acquire and persist independent hourly ERA5 data
#'
#' Combine [shift_plan_reanalysis()], [shift_download_reanalysis()] and
#' [shift_read_reanalysis()] without an EPW template or future climate case.
#' The saved table contains canonical raw dependencies, not derived weather.
#'
#' @inheritParams shift_plan_reanalysis
#' @param dir Directory dedicated to one plan and acquisition origin.
#' @param data Optional local files named exactly once by every request ID.
#' @inheritParams shift_download_reanalysis
#' @param dry_run Return the public request plan without credentials, network
#'   access or file writes.
#' @return A named list containing `schema_version`, `status`, `directory`,
#'   `plan`, `requests`, `artifacts`, `diagnostics`, `execution` and `data`.
#'   Only `complete` provides a `data` descriptor with `path`, `format = "rds"`,
#'   `rows`, `sha256`, `schema_version` and `normalization_version`. Use
#'   `readRDS(result$data$path)` to read the complete data.table explicitly.
#'   Its `reanalysis_schema` attribute describes normalization and provenance.
#'   A partial or failed result has `data = NULL`.
#' @details
#' Raw requests, transport receipts and normalized data have separate identities.
#' Each execution directory belongs to one exact plan and one provider API origin
#' (or local replay). Changed sites, fields, years or origin require a different
#' directory. Local inputs are copied and checked against their retained hashes;
#' their provider origin is not authenticated. Original source paths must remain
#' available when repeating a local replay.
#'
#' `identity.rds` records the plan and origin; `result.rds` is the latest execution
#' snapshot. `receipts/` holds individual validation records. Network assets and
#' submission accounting live under `transfer/`; local copies live under `raw/`.
#' A versioned `normalized-<id>.rds` table and `normalization.rds` descriptor are
#' published only after every dependency and cross-field grid check passes.
#' Normalized identity includes schema/conversion version, the full plan, origin
#' and actual raw hashes. Reopening verifies raw files and the normalized payload
#' and descriptor; corrupt files are not silently accepted or overwritten.
#'
#' `limits` constrain the network transfer stage and retain the cumulative
#' accounting and interrupted-transfer rules of [shift_download_reanalysis()].
#' Local copies and normalization are not HTTP traffic; use an external process
#' supervisor for a whole-workflow wall-clock limit. A separate owner lock spans
#' acquisition, validation and publication. Orphan locks are not taken over.
#' `resume = FALSE` refuses an existing execution. Failed asset diagnostics use
#' fixed error categories; [shift_read_reanalysis()] can reproduce detailed local
#' parsing failures. Configuration errors raise errors; validation or transfer
#' failures never produce a complete data descriptor.
#'
#' `complete` certifies this acquisition and strict local data contract. It does
#' not establish a point's scientific suitability, TMY representativeness, or
#' building-performance validity. All dependency fields and UTC boundary hours
#' remain in the saved table; requested derived variables are not calculated.
#' @examples
#' source <- shift_era5(2001, variables = c("tas", "rsds"),
#'     frequency = "hour", access = "arco")
#' site <- shift_site("point", lat = 1.25, lon = 104)
#' shift_reanalysis_data(source, site, dry_run = TRUE)$requests
#' @export
# shift_reanalysis_data {{{
shift_reanalysis_data <- function(
    source,
    sites,
    dir,
    data = NULL,
    limits = list(),
    resume = TRUE,
    dry_run = FALSE,
    dates = NULL
) {
    plan <- shift_plan_reanalysis(source, sites, dates = dates)
    checkmate::assert_flag(dry_run)
    if (dry_run) {
        return(plan)
    }
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_flag(resume)
    reanalysis_transfer__limits(limits)
    local <- !is.null(data)
    origin <- "local"
    if (local) {
        checkmate::assert_character(data, min.len = 1L, any.missing = FALSE)
        if (
            is.null(names(data)) ||
                anyDuplicated(names(data)) ||
                !setequal(names(data), plan$requests$request_id)
        ) {
            cli::cli_abort(
                "Local data must be named exactly once by every request ID."
            )
        }
        files <- normalizePath(data[plan$requests$request_id], mustWork = TRUE)
    } else {
        config <- cds__config(require_key = FALSE)
        if (
            !grepl("^https?://[^/@?#]+(/[^?#]*)?$", config$url) ||
                (nzchar(config$key) &&
                    grepl(config$key, config$url, fixed = TRUE))
        ) {
            cli::cli_abort(
                "Provider URL must not contain credentials, queries or fragments."
            )
        }
        origin <- config$url
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir <- normalizePath(dir, mustWork = TRUE)
    release <- reanalysis_data__lock(dir)
    on.exit(release(), add = TRUE)
    identity_path <- file.path(dir, "identity.rds")
    plan_id <- checksum_bytes(charToRaw(jsonlite::toJSON(
        plan,
        auto_unbox = TRUE,
        digits = NA
    )))
    identity <- list(schema_version = 1L, plan_id = plan_id, origin = origin)
    if (file.exists(identity_path)) {
        if (!resume) {
            cli::cli_abort("A data execution already exists.")
        }
        if (!identical(readRDS(identity_path), identity)) {
            cli::cli_abort("Existing execution has a different plan or origin.")
        }
    } else {
        reanalysis_data__write(identity, identity_path)
    }
    started <- proc.time()[["elapsed"]]
    started_at <- reanalysis_transfer__time()
    result <- list(
        schema_version = 1L,
        status = "running",
        directory = dir,
        plan = plan,
        requests = data.table::copy(plan$requests),
        artifacts = data.table::data.table(),
        diagnostics = list(),
        execution = list(
            submissions = 0,
            bytes_received = 0,
            origin = origin,
            normalized_reused = FALSE
        ),
        data = NULL
    )
    # Replace a former complete result before inspecting mutable cache files.
    # A failed or interrupted invocation must not leave a current success claim.
    reanalysis_data__write(result, file.path(dir, "result.rds"))
    finish <- function(code = NULL, status = "failed") {
        if (!is.null(code)) {
            result$status <- status
            result$data <- NULL
            result$diagnostics <- c(result$diagnostics, list(list(code = code)))
        }
        result$execution$started_at <- started_at
        result$execution$finished_at <- reanalysis_transfer__time()
        result$execution$package_version <- as.character(utils::packageVersion(
            "epwshiftr"
        ))
        result$execution$r_version <- R.version.string
        result$execution$status <- result$status
        result$execution$elapsed_seconds <- proc.time()[["elapsed"]] - started
        reanalysis_data__write(result, file.path(dir, "result.rds"))
        result
    }
    normalized_receipt <- file.path(dir, "normalization.rds")
    previous <- if (file.exists(normalized_receipt)) {
        tryCatch(readRDS(normalized_receipt), error = function(e) e)
    } else {
        NULL
    }
    if (
        !is.null(previous) && !reanalysis_data__descriptor_valid(previous, dir)
    ) {
        return(finish("invalid_normalization_receipt"))
    }
    if (
        !is.null(previous) &&
            (!file.exists(previous$path) ||
                !identical(checksum_file(previous$path), previous$sha256))
    ) {
        return(finish("normalized_asset_changed"))
    }
    if (!local) {
        transfer <- shift_download_reanalysis(
            plan,
            file.path(dir, "transfer"),
            limits = limits,
            resume = resume
        )
        result$execution <- c(
            transfer$execution,
            list(
                origin = origin,
                normalized_reused = FALSE,
                cumulative = transfer$cumulative,
                accounting_uncertain = transfer$accounting_uncertain,
                transport_status = transfer$status,
                transfer_receipt = transfer$receipt
            )
        )
        data.table::set(
            result$requests,
            j = "status",
            value = vapply(
                transfer$requests,
                function(x) x$state,
                character(1)
            )
        )
        owned <- transfer$files
        expected_hash <- stats::setNames(
            vapply(
                transfer$requests,
                function(x) {
                    if (is.null(x$raw_asset$sha256)) {
                        NA_character_
                    } else {
                        x$raw_asset$sha256
                    }
                },
                character(1)
            ),
            plan$requests$request_id
        )
    } else {
        dir.create(file.path(dir, "raw"), showWarnings = FALSE)
        owned <- file.path(
            dir,
            "raw",
            paste0(plan$requests$request_id, ".asset")
        )
        names(owned) <- plan$requests$request_id
        expected_hash <- stats::setNames(
            rep(NA_character_, length(owned)),
            names(owned)
        )
        for (i in seq_along(files)) {
            receipt <- paste0(owned[[i]], ".rds")
            hash <- checksum_file(files[[i]])
            expected_hash[[i]] <- hash
            if (file.exists(receipt)) {
                saved <- tryCatch(readRDS(receipt), error = function(e) e)
                if (
                    inherits(saved, "error") ||
                        !is.list(saved) ||
                        !identical(saved$schema_version, 1L) ||
                        !identical(saved$request_id, names(owned)[[i]]) ||
                        !identical(saved$origin, "local")
                ) {
                    return(finish("invalid_raw_receipt"))
                }
                if (!identical(hash, saved$sha256)) {
                    return(finish("local_source_changed"))
                }
                if (
                    !file.exists(owned[[i]]) ||
                        !identical(checksum_file(owned[[i]]), saved$sha256)
                ) {
                    return(finish("raw_asset_changed"))
                }
            } else {
                part <- paste0(owned[[i]], ".part")
                if (
                    !file.copy(files[[i]], part, overwrite = TRUE) ||
                        !identical(checksum_file(part), hash) ||
                        !identical(checksum_file(files[[i]]), hash) ||
                        !file.rename(part, owned[[i]])
                ) {
                    return(finish("local_copy_failed"))
                }
                reanalysis_data__write(
                    list(
                        schema_version = 1L,
                        request_id = names(owned)[[i]],
                        sha256 = hash,
                        source = files[[i]],
                        origin = "local"
                    ),
                    receipt
                )
            }
        }
    }
    rows <- vector("list", nrow(plan$requests))
    artifacts <- vector("list", nrow(plan$requests))
    receipt_dir <- file.path(dir, "receipts")
    dir.create(receipt_dir, showWarnings = FALSE)
    valid <- rep(FALSE, nrow(plan$requests))
    # Only assets returned as verified by this acquisition are eligible. Raw
    # transport state stays independent from these weather-validation receipts.
    for (i in seq_len(nrow(plan$requests))) {
        id <- plan$requests$request_id[[i]]
        if (!id %in% names(owned)) {
            next
        }
        receipt <- file.path(receipt_dir, paste0(id, ".rds"))
        asset <- tryCatch(
            reanalysis_read__asset(plan$requests[i], owned[[id]]),
            error = function(e) e
        )
        error <- if (inherits(asset, "error")) "strict_read_failed" else NULL
        hash <- checksum_file(owned[[id]])
        if (
            !identical(hash, expected_hash[[id]]) ||
                (is.null(error) && !identical(asset$artifact$raw_sha256, hash))
        ) {
            error <- "raw_asset_changed"
        }
        if (!is.null(error)) {
            data.table::set(
                result$requests,
                i = i,
                j = "status",
                value = "validation_failed"
            )
            artifact <- data.table::data.table(
                request_id = id,
                path = owned[[id]],
                raw_sha256 = hash,
                variable_id = plan$requests$variable_id[[i]],
                status = "validation_failed",
                origin = origin,
                receipt = receipt
            )
            artifacts[[i]] <- artifact
            result$diagnostics[[length(result$diagnostics) + 1L]] <- list(
                request_id = id,
                code = error
            )
            reanalysis_data__write(
                list(
                    schema_version = 1L,
                    request_id = id,
                    origin = origin,
                    state = "validation_failed",
                    updated_at = reanalysis_transfer__time(),
                    error = error,
                    artifact = artifact
                ),
                receipt
            )
            next
        }
        rows[[i]] <- asset$data
        artifact <- asset$artifact
        data.table::set(artifact, j = "receipt", value = receipt)
        data.table::set(artifact, j = "status", value = "validated")
        data.table::set(artifact, j = "origin", value = origin)
        artifacts[[i]] <- artifact
        data.table::set(
            result$requests,
            i = i,
            j = "status",
            value = "validated"
        )
        reanalysis_data__write(
            list(
                schema_version = 1L,
                request_id = id,
                origin = origin,
                state = "validated",
                updated_at = reanalysis_transfer__time(),
                artifact = artifact,
                normalization_version = "era5-point-1"
            ),
            receipt
        )
        valid[[i]] <- TRUE
    }
    result$artifacts <- data.table::rbindlist(artifacts, fill = TRUE)
    if (!all(valid)) {
        return(finish(
            "incomplete_assets",
            if (any(valid)) "partial" else "failed"
        ))
    }
    read <- tryCatch(
        reanalysis_read__combine(rows, artifacts),
        error = function(e) e
    )
    if (inherits(read, "error")) {
        return(finish("inconsistent_fields"))
    }
    identity <- checksum_bytes(serialize(
        list(
            schema_version = 1L,
            normalization_version = read$normalization_version,
            plan_id = plan_id,
            origin = origin,
            raw_sha256 = read$artifacts$raw_sha256
        ),
        NULL,
        version = 3
    ))
    schema <- list(
        schema_version = 1L,
        normalization_version = read$normalization_version,
        normalization_id = identity,
        origin = origin,
        variables = plan$variables
    )
    data.table::setattr(read$data, "reanalysis_schema", schema)
    path <- file.path(dir, paste0("normalized-", identity, ".rds"))
    if (!is.null(previous) && identical(previous$normalization_id, identity)) {
        # A valid payload hash alone does not authenticate descriptor metadata.
        # Derive its row count, location and versions from the current read.
        expected <- c(
            list(
                path = path,
                format = "rds",
                rows = nrow(read$data),
                sha256 = checksum_file(path)
            ),
            schema[c(
                "schema_version",
                "normalization_version",
                "normalization_id"
            )]
        )
        if (!identical(previous, expected)) {
            return(finish("invalid_normalization_receipt"))
        }
        result$execution$normalized_reused <- TRUE
        result$data <- previous
    } else {
        reanalysis_data__write(read$data, path)
        result$data <- c(
            list(
                path = path,
                format = "rds",
                rows = nrow(read$data),
                sha256 = checksum_file(path)
            ),
            schema[c(
                "schema_version",
                "normalization_version",
                "normalization_id"
            )]
        )
        reanalysis_data__write(result$data, normalized_receipt)
    }
    data.table::set(result$requests, j = "status", value = "validated")
    result$artifacts <- read$artifacts
    result$status <- "complete"
    finish()
}
# }}}

# Atomic same-directory RDS publication keeps partial writes out of descriptors.
reanalysis_data__write <- function(value, path) {
    temporary <- tempfile("reanalysis-", tmpdir = dirname(path))
    on.exit(unlink(temporary), add = TRUE)
    saveRDS(value, temporary, version = 3)
    if (!file.rename(temporary, path)) {
        cli::cli_abort("Cannot publish reanalysis result.")
    }
}

# Validate the disk descriptor before using its paths or counters. A corrupted
# receipt is a failed execution, not a reason to overwrite a possibly good file.
reanalysis_data__descriptor_valid <- function(x, dir) {
    is.list(x) &&
        !inherits(x, "error") &&
        identical(x$schema_version, 1L) &&
        identical(x$format, "rds") &&
        checkmate::test_string(x$normalization_version, min.chars = 1L) &&
        checkmate::test_string(
            x$normalization_id,
            pattern = "^[a-f0-9]{64}$"
        ) &&
        checkmate::test_string(x$sha256, pattern = "^[a-f0-9]{64}$") &&
        checkmate::test_count(x$rows, positive = TRUE) &&
        identical(
            x$path,
            file.path(dir, paste0("normalized-", x$normalization_id, ".rds"))
        )
}

# A separate orchestration lock spans raw transfer, validation and publication.
# Only its recorded owner can remove it; orphan recovery remains explicit.
reanalysis_data__lock <- function(dir) {
    lock <- file.path(dir, ".data-lock")
    if (!dir.create(lock, showWarnings = FALSE)) {
        cli::cli_abort("Data directory is locked; verify its recorded owner.")
    }
    owner <- list(
        pid = Sys.getpid(),
        host = Sys.info()[["nodename"]],
        nonce = basename(tempfile()),
        started_at = reanalysis_transfer__time()
    )
    path <- file.path(lock, "owner.json")
    reanalysis_transfer__write(owner, path)
    function() {
        current <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
        if (identical(current$nonce, owner$nonce)) {
            unlink(lock, recursive = TRUE)
        }
    }
}

# vim: fdm=marker :
