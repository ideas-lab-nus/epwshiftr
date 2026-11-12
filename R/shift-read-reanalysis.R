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
    if (!is.list(plan) || !identical(plan$schema_version, 1L)) {
        cli::cli_abort("Supply an unchanged reanalysis plan.")
    }
    # Reconstruct the public plan rather than trusting editable request counts,
    # dates or hashes. This also validates its supported product and coordinates.
    canonical <- shift_plan_reanalysis(
        shift_era5(
            plan$source$years,
            variables = plan$variables$variable_id,
            product = plan$source$product,
            frequency = plan$source$frequency,
            access = plan$source$access
        ),
        lapply(seq_len(nrow(plan$sites)), function(i) {
            shift_site(
                plan$sites$site_id[[i]],
                lat = plan$sites$latitude[[i]],
                lon = plan$sites$longitude[[i]]
            )
        }),
        dates = plan$source$dates
    )
    if (!reanalysis_plan__identical(plan, canonical)) {
        cli::cli_abort("Supply an unchanged reanalysis plan.")
    }
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

# vim: fdm=marker :
