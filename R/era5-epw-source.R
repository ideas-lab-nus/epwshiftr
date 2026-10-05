#' @include source-era5.R adapter-era-cf.R
NULL

# Primitive ERA5 single-level fields: FDIR is horizontal direct radiation,
# never direct-normal radiation. ERA5-Land has a different accumulation contract.
ERA_EPW_VARIABLES <- c(
    "tas",
    "tdps",
    "ps",
    "uas",
    "vas",
    "rsds",
    "fdir",
    "rlds",
    "clt",
    "pr"
)

# Hash plain values rather than data.table pointers for cross-process reuse.
era_epw__hash <- function(value) {
    checksum_bytes(serialize(value, NULL, version = 2L))
}

# Publish metadata only after its payload has been written successfully.
era_epw__json <- function(value, path) {
    temporary <- tempfile("receipt-", tmpdir = dirname(path))
    on.exit(unlink(temporary), add = TRUE)
    jsonlite::write_json(
        value,
        temporary,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null",
        digits = NA
    )
    if (!file.rename(temporary, path)) {
        cli::cli_abort(
            "Cannot publish reference-weather receipt {.path {path}}."
        )
    }
}

# Accept explicit metadata without requiring a baseline weather file.
# Whole-hour standard offsets avoid implicit interpolation of hourly records.
era_epw__site <- function(site) {
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("Each site must be constructed with {.fn shift_site}.")
    }
    metadata <- site@metadata
    timezone <- metadata$timezone
    elevation <- metadata$elevation
    checkmate::assert_number(timezone, lower = -12, upper = 14, finite = TRUE)
    if (timezone != round(timezone)) {
        cli::cli_abort(
            "Reference EPW currently requires a whole-hour standard timezone."
        )
    }
    checkmate::assert_number(
        elevation,
        lower = -500,
        upper = 9000,
        finite = TRUE
    )
    label <- shift_stage__coalesce(site@label, site@id)
    fields <- c(
        label,
        shift_stage__coalesce(metadata$state, ""),
        shift_stage__coalesce(metadata$country, "")
    )
    if (anyNA(fields) || any(grepl("[,\r\n]", fields))) {
        cli::cli_abort(
            "Site labels, state and country must not contain commas or line breaks."
        )
    }
    list(
        id = site@id,
        label = label,
        latitude = site@lat,
        longitude = reanalysis__longitude(site@lon),
        timezone = timezone,
        elevation = elevation,
        state = fields[[2L]],
        country = fields[[3L]]
    )
}

# Read one nearest rectilinear grid point without loading the whole spatial
# variable. Extra ensemble/forecast dimensions are never silently combined.
era_epw__read_field <- function(handle, variable, site) {
    names <- era__netcdf_variables(handle)
    aliases <- era5__variable_manifest()[variable_id == variable]$aliases[[1L]]
    found <- intersect(aliases, names)
    if (length(found) != 1L) {
        cli::cli_abort("Expected exactly one ERA5 field for {.val {variable}}.")
    }
    time <- era__netcdf_time(handle, names)
    calendar <- as.character(era__netcdf_attribute(
        handle,
        time$name,
        "calendar",
        "standard"
    ))
    if (!calendar %in% c("standard", "gregorian", "proleptic_gregorian")) {
        cli::cli_abort("ERA5 reference EPW requires a Gregorian calendar.")
    }
    lat_name <- intersect(c("latitude", "lat"), names)
    lon_name <- intersect(c("longitude", "lon"), names)
    if (length(lat_name) != 1L || length(lon_name) != 1L) {
        cli::cli_abort(
            "ERA5 files must declare latitude and longitude coordinates."
        )
    }
    latitude <- era__netcdf_coordinate(handle, lat_name)
    longitude <- reanalysis__longitude(era__netcdf_coordinate(handle, lon_name))
    if (any(!is.finite(c(latitude, longitude)))) {
        cli::cli_abort("Non-finite ERA5 grid coordinates.")
    }
    yi <- which.min(abs(latitude - site@lat))
    xi <- which.min(abs(((longitude - site@lon + 180) %% 360) - 180))
    info <- RNetCDF::var.inq.nc(handle, found)
    dims <- lapply(info$dimids, function(id) RNetCDF::dim.inq.nc(handle, id))
    dn <- vapply(dims, "[[", character(1L), "name")
    counts <- vapply(dims, function(x) as.integer(x$length), integer(1L))
    starts <- rep(1L, length(dn))
    # RNetCDF represents a scalar's dimids with NA, not integer(0).
    # Use ndims to distinguish a scalar coordinate from an unresolved axis.
    dimensions <- function(name) {
        info <- RNetCDF::var.inq.nc(handle, name)
        if (info$ndims == 0L) integer() else info$dimids
    }
    time_dims <- dimensions(time$name)
    lat_dims <- dimensions(lat_name)
    lon_dims <- dimensions(lon_name)
    if (
        length(time_dims) != 1L ||
            length(lat_dims) > 1L ||
            length(lon_dims) > 1L ||
            (length(lat_dims) &&
                length(lon_dims) &&
                identical(lat_dims, lon_dims))
    ) {
        cli::cli_abort(
            "Only rectilinear ERA5 grids or scalar point coordinates are supported."
        )
    }
    ti <- match(time_dims, info$dimids)
    if (is.na(ti)) {
        cli::cli_abort("ERA5 field lacks its validity-time dimension.")
    }
    for (index in seq_along(dn)) {
        dimid <- info$dimids[[index]]
        if (dimid %in% lat_dims) {
            starts[[index]] <- yi
            counts[[index]] <- 1L
        } else if (dimid %in% lon_dims) {
            starts[[index]] <- xi
            counts[[index]] <- 1L
        } else if (index != ti && counts[[index]] != 1L) {
            cli::cli_abort(
                "Ambiguous ERA5 dimension {.val {dn[[index]]}}; select one version/member first."
            )
        }
    }
    values <- as.numeric(RNetCDF::var.get.nc(
        handle,
        found,
        start = starts,
        count = counts,
        # Decode packed CF values before applying physical unit conversions.
        unpack = TRUE
    ))
    if (length(values) != length(time$value)) {
        cli::cli_abort(
            "ERA5 values and validity-time coordinate differ in length."
        )
    }
    list(
        data = data.table::data.table(utc_time = time$value, value = values),
        units = as.character(era__netcdf_attribute(handle, found, "units", "")),
        grid = c(latitude = latitude[[yi]], longitude = longitude[[xi]])
    )
}

# Assemble split files after checking native units, grid identity and exact
# validity-time alignment. Overlapping records must not be deduplicated away.
era_epw__read_files <- function(paths, site) {
    checkmate::assert_character(
        paths,
        min.len = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    paths <- normalizePath(path.expand(paths), winslash = "/", mustWork = TRUE)
    entries <- stats::setNames(
        vector("list", length(ERA_EPW_VARIABLES)),
        ERA_EPW_VARIABLES
    )
    # Preallocate one slot per source file; each read remains spatially bounded.
    entries <- lapply(entries, function(x) vector("list", length(paths)))
    for (path_index in seq_along(paths)) {
        path <- paths[[path_index]]
        handle <- RNetCDF::open.nc(path)
        fields <- tryCatch(
            {
                variables <- era__netcdf_variables(handle)
                selected <- ERA_EPW_VARIABLES[vapply(
                    ERA_EPW_VARIABLES,
                    function(v) {
                        any(
                            era5__variable_manifest()[
                                variable_id == v
                            ]$aliases[[1L]] %in%
                                variables
                        )
                    },
                    logical(1L)
                )]
                stats::setNames(
                    lapply(selected, function(v) {
                        era_epw__read_field(handle, v, site)
                    }),
                    selected
                )
            },
            finally = RNetCDF::close.nc(handle)
        )
        for (v in names(fields)) {
            entries[[v]][[path_index]] <- fields[[v]]
        }
    }
    entries <- lapply(entries, function(x) Filter(Negate(is.null), x))
    absent <- names(entries)[!lengths(entries)]
    if (length(absent)) {
        cli::cli_abort("Missing ERA5 field(s): {.val {absent}}.")
    }
    units <- stats::setNames(character(length(entries)), names(entries))
    grid <- NULL
    table <- NULL
    for (v in names(entries)) {
        pieces <- entries[[v]]
        declared <- unique(vapply(pieces, "[[", character(1L), "units"))
        grids <- lapply(pieces, "[[", "grid")
        if (
            length(declared) != 1L ||
                !all(vapply(grids, identical, logical(1L), grids[[1L]]))
        ) {
            cli::cli_abort(
                "Split ERA5 files change units or grid for {.val {v}}."
            )
        }
        if (is.null(grid)) {
            grid <- grids[[1L]]
        }
        if (!identical(grid, grids[[1L]])) {
            cli::cli_abort("ERA5 variables use different nearest grid points.")
        }
        values <- data.table::rbindlist(lapply(pieces, "[[", "data"))
        data.table::setorder(values, utc_time)
        if (anyDuplicated(values$utc_time)) {
            cli::cli_abort("Duplicate ERA5 validity times for {.val {v}}.")
        }
        if (is.null(table)) {
            table <- values[, "utc_time", with = FALSE]
        }
        if (
            !identical(as.numeric(table$utc_time), as.numeric(values$utc_time))
        ) {
            cli::cli_abort(
                "ERA5 variables do not share the same validity-time axis."
            )
        }
        data.table::set(table, j = v, value = values$value)
        units[[v]] <- declared
    }
    list(
        data = table,
        units = units,
        grid = as.list(grid),
        interval_seconds = 3600,
        provenance = list(
            files = as.data.frame(data.table::data.table(
                path = paths,
                sha256 = vapply(paths, checksum_file, character(1L))
            ))
        )
    )
}

# Convert an explicit native-unit bundle without changing caller-owned data.
# Instantaneous fields remain at valid time; accumulations cover its prior hour.
era_epw__normalize <- function(bundle) {
    if (
        !is.list(bundle) ||
            is.null(bundle$data) ||
            is.null(bundle$units) ||
            !identical(as.numeric(bundle$interval_seconds), 3600)
    ) {
        cli::cli_abort(
            "Local ERA5 data requires data, units, grid and interval_seconds = 3600."
        )
    }
    checkmate::assert_data_frame(bundle$data, min.rows = 1L)
    checkmate::assert_character(
        bundle$units,
        any.missing = FALSE,
        min.len = 10L
    )
    if (is.null(names(bundle$units)) || anyDuplicated(names(bundle$units))) {
        cli::cli_abort("ERA5 units require unique field names.")
    }
    required <- c("utc_time", ERA_EPW_VARIABLES)
    if (
        !all(required %in% names(bundle$data)) ||
            !all(ERA_EPW_VARIABLES %in% names(bundle$units))
    ) {
        cli::cli_abort(
            "Local ERA5 data is missing required fields or unit declarations."
        )
    }
    data <- data.table::as.data.table(data.table::copy(bundle$data))[,
        required,
        with = FALSE
    ]
    if (
        !inherits(data$utc_time, "POSIXct") ||
            anyNA(data$utc_time) ||
            anyDuplicated(data$utc_time) ||
            is.unsorted(data$utc_time) ||
            any(as.numeric(data$utc_time) %% 3600 != 0)
    ) {
        cli::cli_abort(
            "ERA5 validity times must be unique ordered UTC whole hours."
        )
    }
    # Gaps outside the requested years are allowed. Each annual conversion
    # checks every required hour, including hours omitted by leap-day policy.
    data.table::set(
        data,
        j = "utc_time",
        value = as.POSIXct(
            as.numeric(data$utc_time),
            origin = "1970-01-01",
            tz = "UTC"
        )
    )
    # Retain the actual input representation, not just its converted values:
    # changing units or the preprocessing provenance invalidates reuse.
    native <- lapply(data, function(x) {
        if (inherits(x, "POSIXct")) as.numeric(x) else x
    })
    provenance <- if (is.null(bundle$provenance)) {
        NULL
    } else {
        jsonlite::fromJSON(
            jsonlite::toJSON(
                bundle$provenance,
                auto_unbox = TRUE,
                null = "null",
                digits = NA
            ),
            simplifyVector = FALSE
        )
    }
    grid <- bundle$grid
    checkmate::assert_number(
        grid$latitude,
        lower = -90,
        upper = 90,
        finite = TRUE
    )
    checkmate::assert_number(
        grid$longitude,
        lower = -180,
        upper = 360,
        finite = TRUE
    )
    grid$longitude <- reanalysis__longitude(grid$longitude)
    for (v in ERA_EPW_VARIABLES) {
        if (!is.numeric(data[[v]])) {
            cli::cli_abort("ERA5 {.val {v}} must be numeric.")
        }
        unit <- tolower(gsub("[[:space:]_*^()]", "", bundle$units[[v]]))
        if (v %in% c("tas", "tdps")) {
            if (unit %in% c("c", "degc", "degreecelsius", "degreescelsius")) {
                data.table::set(data, j = v, value = data[[v]] + 273.15)
            } else if (!unit %in% c("k", "kelvin")) {
                cli::cli_abort("Unsupported temperature units for {.val {v}}.")
            }
        } else if (v == "ps") {
            if (unit %in% c("hpa", "mbar")) {
                data.table::set(data, j = v, value = data[[v]] * 100)
            } else if (unit != "pa") {
                cli::cli_abort("Pressure units must be Pa or hPa.")
            }
        } else if (v %in% c("uas", "vas")) {
            if (!unit %in% c("ms-1", "m/s")) {
                cli::cli_abort("Wind units must be m/s.")
            }
        } else if (v %in% c("rsds", "fdir", "rlds")) {
            if (unit %in% c("jm-2", "j/m2")) {
                data.table::set(data, j = v, value = data[[v]] / 3600)
            } else if (!unit %in% c("wm-2", "w/m2", "whm-2", "wh/m2")) {
                cli::cli_abort(
                    "Radiation units must be J/m2, Wh/m2 or hourly mean W/m2."
                )
            }
        } else if (v == "clt") {
            if (unit == "%") {
                data.table::set(data, j = v, value = data[[v]] / 100)
            } else if (!unit %in% c("1", "0-1")) {
                cli::cli_abort("Cloud fraction units must be 1 or percent.")
            }
        } else if (v == "pr") {
            if (unit == "m") {
                data.table::set(data, j = v, value = data[[v]] * 1000)
            } else if (!unit %in% c("mm", "kgm-2")) {
                cli::cli_abort(
                    "Precipitation must be a preceding-hour depth in m or mm."
                )
            }
        }
    }
    plain <- lapply(data, function(x) {
        if (inherits(x, "POSIXct")) as.numeric(x) else x
    })
    list(
        data = data,
        grid = grid,
        provenance = provenance,
        native_units = as.list(bundle$units),
        sha256 = era_epw__hash(list(
            native_data = native,
            native_units = bundle$units,
            data = plain,
            grid = grid,
            interval_seconds = 3600,
            provenance = provenance
        ))
    )
}

# Split CDS requests by month and step type, including the boundary days needed
# for UTC-to-local conversion. No annual Cartesian request crosses item limits.
era_epw__requests <- function(source, site) {
    base <- era5__request(source, "tas", site, "cds")
    dates <- sort(unique(do.call(
        c,
        lapply(source@years, function(year) {
            seq(
                as.Date(sprintf("%d-01-01", year)) - 1L,
                as.Date(sprintf("%d-01-01", year + 1L)),
                by = "day"
            )
        })
    )))
    months <- split(dates, format(dates, "%Y-%m"))
    groups <- list(
        instant = c("tas", "tdps", "ps", "uas", "vas", "clt"),
        accumulated = c("rsds", "fdir", "rlds", "pr")
    )
    manifest <- era5__variable_manifest()
    result <- stats::setNames(
        vector("list", length(months) * length(groups)),
        as.vector(t(outer(names(months), names(groups), paste, sep = "-")))
    )
    for (month in names(months)) {
        for (group in names(groups)) {
            request <- base
            attr(request, "requested_years") <- NULL
            request$year <- substr(month, 1L, 4L)
            request$month <- substr(month, 6L, 7L)
            request$day <- format(months[[month]], "%d")
            request$variable <- manifest[
                match(groups[[group]], variable_id),
                era_variable
            ]
            result[[paste(month, group, sep = "-")]] <- request
        }
    }
    result
}

# Resume the same provider locator after a timeout or interrupted download.
# An ambiguous POST cannot be retried blindly without its remote identity.
era_epw__retrieve <- function(request, source, directory) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    target <- file.path(directory, "source.nc")
    receipt_path <- file.path(directory, "receipt.json")
    receipt <- if (file.exists(receipt_path)) {
        jsonlite::read_json(receipt_path, simplifyVector = TRUE)
    } else {
        NULL
    }
    identity <- era_epw__hash(list(
        dataset = era5__dataset_id("single_levels", "cds"),
        request = request
    ))
    if (!is.null(receipt) && !identical(receipt$input_sha256, identity)) {
        cli::cli_abort("CDS request receipt does not match its input.")
    }
    if (
        !is.null(receipt) &&
            identical(receipt$status, "downloaded") &&
            file.exists(target) &&
            identical(checksum_file(target), receipt$output_sha256)
    ) {
        return(target)
    }
    if (!is.null(receipt) && is.null(receipt$job)) {
        cli::cli_abort(
            "CDS submission needs review; no remote locator was recorded. Receipt: {.path {receipt_path}}."
        )
    }
    # Keep interrupted request receipts and corrupt cache bytes as evidence
    # before resuming their original provider locator.
    history <- file.path(directory, "attempts")
    dir.create(history, showWarnings = FALSE)
    attempt <- file.path(
        history,
        sprintf(
            "attempt-%03d",
            length(list.dirs(history, recursive = FALSE)) + 1L
        )
    )
    if (!dir.create(attempt)) {
        cli::cli_abort("Cannot record CDS retrieval attempt.")
    }
    if (file.exists(receipt_path)) {
        file.copy(receipt_path, file.path(attempt, "previous-receipt.json"))
    }
    if (file.exists(target)) {
        file.copy(target, file.path(attempt, "previous-source.nc"))
    }
    on.exit(
        {
            if (file.exists(receipt_path)) {
                file.copy(
                    receipt_path,
                    file.path(attempt, "receipt.json"),
                    overwrite = TRUE
                )
            }
        },
        add = TRUE
    )
    config <- cds__config()
    if (is.null(receipt)) {
        receipt <- list(
            input_sha256 = identity,
            request = request,
            status = "submitting",
            started_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
        era_epw__json(receipt, receipt_path)
        receipt$job <- cds__submit(
            era5__dataset_id("single_levels", "cds"),
            request,
            config
        )
        receipt$status <- "submitted"
        era_epw__json(receipt, receipt_path)
    }
    tryCatch(
        {
            completed <- cds__wait(
                receipt$job,
                config,
                timeout = shift_stage__coalesce(source@options$timeout, 86400),
                poll_interval = shift_stage__coalesce(
                    source@options$poll_interval,
                    5
                )
            )
            receipt$job <- completed[c(
                "dataset_id",
                "request_id",
                "monitor_url",
                "status"
            )]
            receipt$status <- "successful"
            era_epw__json(receipt, receipt_path)
            cds__download(cds__result(completed, config), target, config)
            receipt$status <- "downloaded"
            receipt$output_sha256 <- checksum_file(target)
            era_epw__json(receipt, receipt_path)
            target
        },
        error = function(error) {
            receipt$status <- "retrieval_error"
            receipt$error <- cds__redact(conditionMessage(error), config$key)
            era_epw__json(receipt, receipt_path)
            stop(error)
        }
    )
}

# Use the existing CDS transport with separate reference-weather receipts.
# Explicit local inputs never read credentials or submit a provider request.
era_epw__download <- function(source, site, directory) {
    if (identical(source@access, "arco")) {
        cli::cli_abort(
            "The configured CDS point time-series API lacks required FDIR/cloud fields; use access = 'cds' or provide local data."
        )
    }
    requests <- era_epw__requests(source, site)
    vapply(
        requests,
        function(request) {
            era_epw__retrieve(
                request,
                source,
                file.path(directory, era_epw__hash(request))
            )
        },
        character(1L),
        USE.NAMES = FALSE
    )
}
