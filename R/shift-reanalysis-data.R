#' @include shift-download-reanalysis.R
NULL

#' Validate and read local ERA5 point files
#'
#' Read the raw dependencies in an independent [shift_plan_reanalysis()] plan.
#' Every request must have an explicitly associated local NetCDF file or CDS ZIP
#' containing one top-level NetCDF member. Files
#' retain their actual grid coordinates, source values, units and SHA-256 hashes.
#' No credentials, downloads, derived weather variables or EPW files are involved.
#'
#' @param plan An unchanged result of [shift_plan_reanalysis()].
#' @param files Character vector of existing NetCDF or CDS ZIP paths, named by request ID.
#'   Supply exactly the requests in `plan`, with no duplicate names.
#' @return A list with `schema_version`, `normalization_version`,
#'   `status = "validated_local"`, `data` (a long data.table of raw dependency
#'   fields in canonical units) and `artifacts` (one row per request).
#'   `data` includes site ID, variable ID, UTC validity time, value/units,
#'   temporal kind, interval start/end, actual grid coordinates, raw value/units,
#'   request ID and file hash. Instantaneous fields have missing interval bounds.
#' @details
#' ERA5 single-level hourly ARCO and explicit CDS point files are supported. Complete,
#'   strictly ordered hourly coverage, finite values, recognized units and a
#'   consistent nearest 0.25-degree grid point are required. Multiple version
#'   or member slices are rejected rather than silently combined. All source
#'   fields for a site must use the same grid. Failed validation returns an error,
#'   never a filtered or partially joined table.
#'
#' ERA5 radiation and precipitation retain their UTC interval-end validity
#'   times and the preceding one-hour interval. Accumulated energy is divided
#'   by 3600 to obtain mean flux; precipitation depth is converted to mass flux.
#'   No timestamp shift depends on which other variables were requested.
#'   Skin and air temperature are expressed in kelvin. Raw values are decoded
#'   physical NetCDF values before these unit conversions; original packed bytes
#'   remain in the identified file. Local validation cannot authenticate a file's
#'   provider origin or prove scientific suitability of its grid point.
#'
#' ZIP containers are retained unchanged and identified by `raw_sha256`.
#' The artifacts also record `netcdf_member` (missing for a bare NetCDF file)
#' and `netcdf_sha256`. A unique top-level NetCDF member is extracted to a
#' temporary directory using the existing CDS archive validation, read with
#' the same strict weather rules, then removed. Multiple or nested NetCDF
#' members are rejected; the archive filename extension is not used for detection.
#' @export
# shift_read_reanalysis {{{
shift_read_reanalysis <- function(plan, files) {
    reanalysis_plan__validate(plan)
    checkmate::assert_character(files, min.len = 1L, any.missing = FALSE)
    if (
        is.null(names(files)) ||
            anyDuplicated(names(files)) ||
            !setequal(names(files), plan$requests$request_id)
    ) {
        cli::cli_abort("Files must be named exactly once by every request ID.")
    }
    files <- normalizePath(
        path.expand(files[plan$requests$request_id]),
        mustWork = TRUE
    )
    rows <- vector("list", nrow(plan$requests))
    artifacts <- vector("list", nrow(plan$requests))
    # File I/O is bounded to one point and one request at a time. Preallocation
    # avoids repeatedly growing large hourly tables during multi-year reads.
    for (i in seq_len(nrow(plan$requests))) {
        asset <- reanalysis_read__asset(plan$requests[i], files[[i]])
        rows[[i]] <- asset$data
        artifacts[[i]] <- asset$artifact
    }
    reanalysis_read__combine(rows, artifacts)
}
# }}}

# Apply exactly the same strict field rules to a public full read and to each
# acquired asset. Partial execution can retain individually validated receipts.
reanalysis_read__asset <- function(request, path) {
    hash <- checksum_file(path)
    netcdf <- path
    member <- NA_character_
    # CDS can wrap even a single point response in ZIP. Preserve the response
    # identity and apply the same strict NetCDF checks to its unique safe member.
    if (cds__is_zip_file(path)) {
        temporary <- tempfile("reanalysis-member-")
        dir.create(temporary)
        on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
        netcdf <- cds__extract_netcdf_archive(path, temporary)
        member <- basename(netcdf)
    }
    netcdf_hash <- checksum_file(netcdf)
    field <- reanalysis_read__field(netcdf, request)
    if (!identical(hash, checksum_file(path))) {
        cli::cli_abort("Source file changed during reading.")
    }
    data <- field$data
    expected <- seq(
        as.POSIXct(request$date_start, tz = "UTC"),
        by = "hour",
        length.out = request$expected_hours
    )
    if (
        length(data$utc_time) != length(expected) ||
            anyNA(data$utc_time) ||
            !identical(as.numeric(data$utc_time), as.numeric(expected))
    ) {
        cli::cli_abort(
            "File must have complete, ordered, unique hourly UTC coverage."
        )
    }
    raw <- data$value
    if (any(!is.finite(raw))) {
        cli::cli_abort(
            "Source values must be finite, without missing samples."
        )
    }
    converted <- reanalysis_read__convert(
        raw,
        field$units,
        request$variable_id
    )
    positive <- request$variable_id %in%
        c("tas", "tdps", "skt", "ps", "psl")
    nonnegative <- request$variable_id %in%
        c("rsds", "fdir", "rlds", "pr", "clt")
    if (
        any(!is.finite(converted$value)) ||
            (positive && any(converted$value <= 0)) ||
            (nonnegative && any(converted$value < 0)) ||
            (request$variable_id == "clt" && any(converted$value > 100))
    ) {
        cli::cli_abort("Values violate the variable's physical bounds.")
    }
    interval <- converted$temporal_kind == "interval_mean"
    missing_time <- as.POSIXct(
        rep(NA_real_, length(expected)),
        origin = "1970-01-01",
        tz = "UTC"
    )
    normalized <- data.table::data.table(
        variable_id = request$variable_id,
        utc_time = expected,
        value = converted$value,
        units = converted$units,
        temporal_kind = converted$temporal_kind,
        raw_temporal_kind = converted$raw_temporal_kind,
        interval_start = if (interval) expected - 3600 else missing_time,
        interval_end = if (interval) expected else missing_time,
        grid_lat = unname(field$grid[["latitude"]]),
        grid_lon = unname(field$grid[["longitude"]]),
        raw_value = raw,
        raw_units = field$units,
        request_id = request$request_id,
        raw_sha256 = hash
    )
    # Shared raw requests retain every site alias; no inner join can hide
    # a missing field or timestamp. All requests must validate first.
    rows <- data.table::rbindlist(lapply(
        request$site_ids[[1L]],
        function(id) {
            copy <- data.table::copy(normalized)
            data.table::set(copy, j = "site_id", value = id)
            copy
        }
    ))
    artifact <- data.table::data.table(
        request_id = request$request_id,
        path = path,
        raw_sha256 = hash,
        netcdf_member = member,
        netcdf_sha256 = netcdf_hash,
        variable_id = request$variable_id,
        raw_units = field$units,
        grid_lat = unname(field$grid[["latitude"]]),
        grid_lon = unname(field$grid[["longitude"]]),
        rows = nrow(data),
        source_expver = field$expver,
        status = "validated_local"
    )
    list(data = rows, artifact = artifact)
}

# Cross-field grid consistency is a batch requirement, independent of whether
# raw assets came from local replay or the bounded transport layer.
reanalysis_read__combine <- function(rows, artifacts) {
    data <- data.table::rbindlist(rows)
    grids <- unique(data[, c("site_id", "grid_lat", "grid_lon"), with = FALSE])
    if (anyDuplicated(grids$site_id)) {
        cli::cli_abort("Fields for one site use different actual grid points.")
    }
    data.table::setorderv(data, c("site_id", "variable_id", "utc_time"))
    list(
        schema_version = 1L,
        normalization_version = "era5-point-1",
        status = "validated_local",
        data = data,
        artifacts = data.table::rbindlist(artifacts)
    )
}

# Require actual point coordinates before reusing the spatially bounded NetCDF
# reader. A request coordinate never substitutes for absent file metadata.
reanalysis_read__field <- function(path, request) {
    nc <- RNetCDF::open.nc(path)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    variables <- era__netcdf_variables(nc)
    if (length(intersect(c("time", "valid_time"), variables)) != 1L) {
        cli::cli_abort(
            "File must declare exactly one validity-time coordinate."
        )
    }
    for (axis in list(c("latitude", "lat"), c("longitude", "lon"))) {
        name <- intersect(axis, variables)
        if (
            length(name) != 1L || length(era__netcdf_coordinate(nc, name)) != 1L
        ) {
            cli::cli_abort(
                "ERA5 point files must declare actual scalar point coordinates."
            )
        }
        coordinate <- era__netcdf_coordinate(nc, name)
        upper <- if (axis[[1L]] == "latitude") 90 else 360
        lower <- if (axis[[1L]] == "latitude") -90 else -180
        if (
            !is.finite(coordinate) || coordinate < lower || coordinate > upper
        ) {
            cli::cli_abort("Invalid actual point coordinate.")
        }
        declared <- era__netcdf_attribute(nc, name, "units", "")
        allowed <- if (axis[[1L]] == "latitude") {
            c("degrees_north", "degree_north", "degrees_N", "degree_N")
        } else {
            c("degrees_east", "degree_east", "degrees_E", "degree_E")
        }
        if (!declared %in% allowed) {
            cli::cli_abort("Unknown coordinate units.")
        }
    }
    version <- NA_character_
    if ("expver" %in% variables) {
        versions <- unique(as.character(RNetCDF::var.get.nc(nc, "expver")))
        if (
            length(versions) != 1L || !versions %in% c("1", "5", "0001", "0005")
        ) {
            cli::cli_abort(
                "Ambiguous or unsupported expver version; select one version first."
            )
        }
        version <- versions
    }
    if (
        "number" %in%
            variables &&
            !identical(as.numeric(RNetCDF::var.get.nc(nc, "number")), 0)
    ) {
        cli::cli_abort("Only the deterministic ERA5 member is supported.")
    }
    field <- era_epw__read_field(
        nc,
        request$variable_id,
        shift_site("read", lat = request$latitude, lon = request$longitude)
    )
    lat <- unname(field$grid[["latitude"]])
    lon <- (unname(field$grid[["longitude"]]) + 180) %% 360 - 180
    # A point response must be a nearest point on this product's 0.25-degree
    # grid. Ties are allowed; actual coastal representativeness is a separate
    # research check, never inferred from this geometric acceptance.
    if (
        abs(lat) > 90 ||
            any(abs(c(lat, lon) * 4 - round(c(lat, lon) * 4)) > 1e-6) ||
            abs(lat - request$latitude) > 0.125 + 1e-6 ||
            abs((lon - request$longitude + 180) %% 360 - 180) > 0.125 + 1e-6
    ) {
        cli::cli_abort("Actual coordinates are not a nearest ERA5 grid point.")
    }
    field$grid[["longitude"]] <- lon
    field$expver <- version
    field
}

# Convert only explicitly recognized physical units. Do not infer a unit from
# data magnitudes or accept a misspelling as an already normalized field.
reanalysis_read__convert <- function(value, units, variable) {
    unit <- tolower(gsub("[[:space:]_^*]", "", units))
    if (variable %in% c("tas", "tdps", "skt")) {
        if (unit %in% c("c", "degc", "degreecelsius", "degreescelsius")) {
            value <- value + 273.15
        } else if (!unit %in% c("k", "kelvin")) {
            cli::cli_abort("Unknown temperature units {.val {units}}.")
        }
        return(list(
            value = value,
            units = "K",
            temporal_kind = "instantaneous",
            raw_temporal_kind = "instantaneous"
        ))
    }
    if (variable %in% c("rsds", "fdir", "rlds")) {
        accumulation <- unit %in% c("jm-2", "j/m2")
        if (!accumulation && !unit %in% c("wm-2", "w/m2")) {
            cli::cli_abort("Unknown radiation units {.val {units}}.")
        }
        # ERA5 single-level hourly energy belongs to the preceding hour; this
        # fixed interval must not be inferred from gaps between surviving rows.
        return(list(
            value = if (accumulation) value / 3600 else value,
            units = "W m-2",
            temporal_kind = "interval_mean",
            raw_temporal_kind = if (accumulation) {
                "accumulation"
            } else {
                "interval_mean"
            }
        ))
    }
    if (variable %in% c("ps", "psl")) {
        if (unit %in% c("hpa", "mbar", "millibar")) {
            value <- value * 100
        } else if (!unit %in% c("pa", "pascal", "pascals")) {
            cli::cli_abort("Unknown pressure units {.val {units}}.")
        }
        return(list(
            value = value,
            units = "Pa",
            temporal_kind = "instantaneous",
            raw_temporal_kind = "instantaneous"
        ))
    }
    if (variable %in% c("uas", "vas")) {
        if (!unit %in% c("ms-1", "m/s")) {
            cli::cli_abort("Unknown wind units {.val {units}}.")
        }
        return(list(
            value = value,
            units = "m s-1",
            temporal_kind = "instantaneous",
            raw_temporal_kind = "instantaneous"
        ))
    }
    if (variable == "clt") {
        # Units, never value magnitudes, distinguish fraction from percent.
        if (unit %in% c("1", "0-1", "(0-1)", "fraction")) {
            value <- value * 100
        } else if (!unit %in% c("%", "percent", "percentage")) {
            cli::cli_abort("Unknown cloud-cover units {.val {units}}.")
        }
        return(list(
            value = value,
            units = "%",
            temporal_kind = "instantaneous",
            raw_temporal_kind = "instantaneous"
        ))
    }
    if (variable == "pr") {
        accumulation <- unit %in% c("m", "mm", "kgm-2", "kg/m2")
        if (unit == "m") {
            value <- value * 1000 / 3600
        } else if (accumulation) {
            value <- value / 3600
        } else if (!unit %in% c("kgm-2s-1", "kg/m2/s")) {
            cli::cli_abort("Unknown precipitation units {.val {units}}.")
        }
        return(list(
            value = value,
            units = "kg m-2 s-1",
            temporal_kind = "interval_mean",
            raw_temporal_kind = if (accumulation) {
                "accumulation"
            } else {
                "interval_mean"
            }
        ))
    }
    cli::cli_abort("Unsupported raw variable {.val {variable}}.")
}

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
#' shift_data_reanalysis(source, site, dry_run = TRUE)$requests
#' @export
# shift_data_reanalysis {{{
shift_data_reanalysis <- function(
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
    checked <- reanalysis_data__verify_receipt(
        normalized_receipt,
        validate = function(x) reanalysis_data__descriptor_valid(x, dir),
        asset_path = function(x) x$path,
        invalid_code = "invalid_normalization_receipt",
        changed_code = "normalized_asset_changed"
    )
    if (!is.null(checked$error)) {
        return(finish(checked$error))
    }
    previous <- checked$receipt
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
                checked <- reanalysis_data__verify_receipt(
                    receipt,
                    validate = function(x) {
                        identical(x$request_id, names(owned)[[i]]) &&
                            identical(x$origin, "local")
                    },
                    asset_path = owned[[i]],
                    invalid_code = "invalid_raw_receipt",
                    changed_code = "raw_asset_changed",
                    source_hash = hash,
                    source_changed_code = "local_source_changed"
                )
                if (!is.null(checked$error)) {
                    return(finish(checked$error))
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
        expected <- reanalysis_data__descriptor(path, nrow(read$data), schema)
        if (!identical(previous, expected)) {
            return(finish("invalid_normalization_receipt"))
        }
        result$execution$normalized_reused <- TRUE
        result$data <- previous
    } else {
        reanalysis_data__write(read$data, path)
        result$data <- reanalysis_data__descriptor(
            path,
            nrow(read$data),
            schema
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

# Build the normalized payload descriptor from the persisted file and schema.
reanalysis_data__descriptor <- function(path, rows, schema) {
    c(
        list(
            path = path,
            format = "rds",
            rows = rows,
            sha256 = checksum_file(path)
        ),
        schema[c(
            "schema_version",
            "normalization_version",
            "normalization_id"
        )]
    )
}

# Read and validate a persisted RDS receipt plus its referenced file's SHA-256.
reanalysis_data__verify_receipt <- function(
    path,
    validate,
    asset_path,
    invalid_code,
    changed_code,
    source_hash = NULL,
    source_changed_code = NULL
) {
    if (!file.exists(path)) {
        return(list(receipt = NULL, error = NULL))
    }
    saved <- tryCatch(readRDS(path), error = function(e) e)
    if (
        inherits(saved, "error") ||
            !is.list(saved) ||
            !identical(saved$schema_version, 1L) ||
            !isTRUE(validate(saved))
    ) {
        return(list(receipt = NULL, error = invalid_code))
    }
    if (!is.null(source_hash) && !identical(source_hash, saved$sha256)) {
        return(list(receipt = NULL, error = source_changed_code))
    }
    target <- if (is.function(asset_path)) asset_path(saved) else asset_path
    if (
        !file.exists(target) || !identical(checksum_file(target), saved$sha256)
    ) {
        return(list(receipt = NULL, error = changed_code))
    }
    list(receipt = saved, error = NULL)
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
