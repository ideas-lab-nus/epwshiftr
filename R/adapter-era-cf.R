#' @include shift-stage.R source-era5.R source-cds.R
NULL

# ERA reanalysis to CF weather input -----------------------------------------

# Translate public and method-declared frequency labels to the temporal bins
# that ERA normalization can construct from hourly source values.
era__frequency_kind <- function(frequency) {
    checkmate::assert_string(frequency, min.chars = 1L)
    normalized <- tolower(frequency)
    if (normalized %in% c("hour", "1hr", "1hrpt")) {
        return(list(label = if (normalized == "hour") "1hr" else frequency,
            kind = "subdaily", hours = 1L,
            point = grepl("pt$", normalized)))
    }
    if (grepl("^[0-9]+hr(pt)?$", normalized)) {
        hours <- as.integer(sub("hr.*$", "", normalized))
        return(list(label = frequency, kind = "subdaily", hours = hours,
            point = grepl("pt$", normalized)))
    }
    if (normalized %in% c("day", "daily")) {
        return(list(label = "day", kind = "day", hours = 24L,
            point = FALSE))
    }
    if (normalized %in% c("mon", "month", "monthly")) {
        return(list(label = "mon", kind = "mon", hours = NA_integer_,
            point = FALSE))
    }
    cli::cli_abort(
        "ERA normalization does not support frequency {.val {frequency}}."
    )
}

# Resolve the observed-reference requirement from the canonical recipe rather
# than copying variable lists into the ERA provider adapter.
reanalysis__requirement <- function(recipe) {
    specification <- morpher__recipe_spec(recipe)
    requirement <- specification@required_inputs[["observed_reference"]]
    if (is.null(requirement)) {
        requirement <- specification@optional_inputs[["observed_reference"]]
    }
    requirement
}

# Select one satisfiable observed variable alternative and preserve its method
# order so persisted plans do not change with provider response ordering.
reanalysis__variables <- function(spec, recipe) {
    requirement <- reanalysis__requirement(recipe)
    if (is.null(requirement)) {
        return(character())
    }
    manifest <- era5__variable_manifest()
    supported <- manifest[get(spec@product) %in% TRUE, variable_id]
    alternatives <- requirement@variable_sets
    if (!length(alternatives)) {
        cli::cli_abort(
            "The observed-reference contract does not declare source variables."
        )
    }
    # An explicit provider selection may intentionally choose a later method
    # alternative, so satisfiability must be checked before applying the
    # registry's deterministic first-alternative preference.
    candidates <- Filter(function(variables) {
        variables <- as.character(variables)
        all(variables %in% supported) &&
            (is.null(spec@variables) || all(variables %in% spec@variables))
    }, alternatives)
    selected <- if (length(candidates)) {
        as.character(candidates[[1L]])
    } else {
        NULL
    }
    if (is.null(selected)) {
        cli::cli_abort(
            "ERA5 cannot satisfy any observed-reference variable alternative for this method."
        )
    }
    if (!is.null(spec@variables)) {
        unknown <- setdiff(spec@variables, supported)
        if (length(unknown)) {
            cli::cli_abort(
                "ERA5 does not provide or derive selected CF variable(s): {.val {unknown}}."
            )
        }
        return(unique(c(selected, spec@variables)))
    }
    selected
}

# Resolve variable-specific output frequencies from an explicit ERA override
# or the observed-reference role contract.
reanalysis__frequencies <- function(spec, recipe, variables) {
    requirement <- reanalysis__requirement(recipe)
    if (!length(variables) || is.null(requirement)) {
        return(character())
    }
    supplied <- spec@frequency
    if (!is.null(supplied)) {
        supplied_names <- names(supplied)
        supplied <- as.character(unlist(supplied, use.names = TRUE))
        names(supplied) <- supplied_names
        if (is.null(names(supplied))) {
            return(stats::setNames(rep(supplied[[1L]], length(variables)),
                variables))
        }
        missing <- setdiff(variables, names(supplied))
        if (length(missing)) {
            cli::cli_abort(
                "Variable-specific ERA5 frequencies are missing {.val {missing}}."
            )
        }
        return(supplied[variables])
    }

    mapped <- requirement@variable_frequencies
    output <- stats::setNames(rep(NA_character_, length(variables)), variables)
    for (variable in variables) {
        value <- mapped[[variable]]
        if (is.null(value)) {
            value <- requirement@frequencies
        }
        if (!length(value)) {
            cli::cli_abort(
                "Observed variable {.val {variable}} has no frequency contract."
            )
        }
        output[[variable]] <- as.character(value[[1L]])
    }
    output
}

# Enumerate NetCDF variable names through the public RNetCDF inquiry API.
era__netcdf_variables <- function(handle) {
    info <- RNetCDF::file.inq.nc(handle)
    if (!info$nvars) {
        return(character())
    }
    vapply(0:(info$nvars - 1L), function(id) {
        RNetCDF::var.inq.nc(handle, id)$name
    }, character(1L))
}

# Read an optional NetCDF attribute without masking errors from required
# coordinate or data reads.
era__netcdf_attribute <- function(handle, variable, attribute, default = NULL) {
    tryCatch(
        RNetCDF::att.get.nc(handle, variable, attribute),
        error = function(error) default
    )
}

# Read one NetCDF coordinate variable and retain a numeric vector. Coordinates
# with unsupported string encodings fail at the adapter boundary.
era__netcdf_coordinate <- function(handle, name) {
    value <- RNetCDF::var.get.nc(handle, name, collapse = TRUE)
    if (!is.numeric(value)) {
        cli::cli_abort(
            "ERA NetCDF coordinate {.val {name}} must be numeric."
        )
    }
    as.numeric(value)
}

# Parse both current `time` and legacy `valid_time` CF axes while rejecting
# files that do not state a reproducible time unit.
era__netcdf_time <- function(handle, names) {
    time_name <- intersect(c("time", "valid_time"), names)
    if (!length(time_name)) {
        cli::cli_abort("ERA NetCDF result does not contain a time coordinate.")
    }
    time_name <- time_name[[1L]]
    units <- era__netcdf_attribute(handle, time_name, "units")
    if (is.null(units) || !nzchar(as.character(units))) {
        cli::cli_abort("ERA NetCDF time coordinate does not declare CF units.")
    }
    calendar <- era__netcdf_attribute(
        handle,
        time_name,
        "calendar",
        default = "standard"
    )
    list(
        name = time_name,
        value = parse_cf_time(
            era__netcdf_coordinate(handle, time_name),
            as.character(units),
            as.character(calendar)
        )
    )
}

# Read one requested ERA source field and select the nearest returned grid
# centre. The full CDS request may return a small cell while the ARCO-backed
# time-series service normally returns one point.
era__read_netcdf <- function(path, source_variable, site) {
    checkmate::assert_string(path, min.chars = 1L)
    checkmate::assert_string(source_variable, min.chars = 1L)
    if (!file.exists(path)) {
        cli::cli_abort("ERA NetCDF result is missing: {.path {path}}.")
    }
    handle <- RNetCDF::open.nc(path)
    on.exit(RNetCDF::close.nc(handle), add = TRUE)
    variables <- era__netcdf_variables(handle)
    manifest <- era5__variable_manifest()
    row <- manifest[variable_id == source_variable]
    aliases <- row$aliases[[1L]]
    data_name <- intersect(aliases, variables)
    if (!length(data_name)) {
        cli::cli_abort(
            "ERA NetCDF result does not contain {.val {row$era_variable}}."
        )
    }
    data_name <- data_name[[1L]]
    info <- RNetCDF::var.inq.nc(handle, data_name)
    dimensions <- lapply(info$dimids, function(id) {
        RNetCDF::dim.inq.nc(handle, id)
    })
    dimension_names <- vapply(dimensions, `[[`, character(1L), "name")
    dimension_lengths <- vapply(dimensions, function(value) {
        as.integer(value$length)
    }, integer(1L))
    values <- RNetCDF::var.get.nc(handle, data_name, collapse = FALSE)
    if (!length(dimension_lengths)) {
        cli::cli_abort("ERA weather variables must contain a time dimension.")
    }
    dim(values) <- dimension_lengths
    indices <- arrayInd(seq_along(values), .dim = dimension_lengths)
    data <- data.table::data.table(value = as.numeric(values))
    time <- era__netcdf_time(handle, variables)

    for (index in seq_along(dimension_names)) {
        name <- dimension_names[[index]]
        coordinate_name <- if (name %in% variables) {
            name
        } else if (name == "latitude" && "lat" %in% variables) {
            "lat"
        } else if (name == "longitude" && "lon" %in% variables) {
            "lon"
        } else {
            NULL
        }
        if (is.null(coordinate_name)) {
            data[[name]] <- indices[, index]
        } else if (coordinate_name == time$name) {
            data[["utc_time"]] <- time$value[indices[, index]]
        } else {
            coordinate <- era__netcdf_coordinate(handle, coordinate_name)
            data[[name]] <- coordinate[indices[, index]]
        }
    }
    if (!"utc_time" %in% names(data)) {
        cli::cli_abort("ERA weather variable is not indexed by its time axis.")
    }

    latitude_name <- intersect(c("latitude", "lat"), names(data))
    longitude_name <- intersect(c("longitude", "lon"), names(data))
    grid_lat <- if (length(latitude_name)) {
        as.numeric(data[[latitude_name[[1L]]]])
    } else {
        scalar <- intersect(c("latitude", "lat"), variables)
        if (length(scalar)) era__netcdf_coordinate(handle, scalar[[1L]]) else
            site@lat
    }
    grid_lon <- if (length(longitude_name)) {
        as.numeric(data[[longitude_name[[1L]]]])
    } else {
        scalar <- intersect(c("longitude", "lon"), variables)
        if (length(scalar)) era__netcdf_coordinate(handle, scalar[[1L]]) else
            site@lon
    }
    if (length(grid_lat) == 1L) {
        grid_lat <- rep(grid_lat, nrow(data))
    }
    if (length(grid_lon) == 1L) {
        grid_lon <- rep(grid_lon, nrow(data))
    }
    if (length(grid_lat) != nrow(data) || length(grid_lon) != nrow(data)) {
        cli::cli_abort("ERA grid coordinates do not align with weather values.")
    }
    normalized_lon <- reanalysis__longitude(grid_lon)
    site_lon <- reanalysis__longitude(site@lon)
    distances <- tunnel_dist(grid_lat, normalized_lon, site@lat,
        site_lon)
    nearest <- which.min(distances)
    selected_lat <- grid_lat[[nearest]]
    selected_lon <- normalized_lon[[nearest]]
    keep <- abs(grid_lat - selected_lat) <= sqrt(.Machine$double.eps) &
        abs(normalized_lon - selected_lon) <= sqrt(.Machine$double.eps)
    data <- data[keep]
    data[, grid_lat := selected_lat]
    data[, grid_lon := selected_lon]
    data[, grid_dist_km := tunnel_dist(
        selected_lat,
        selected_lon,
        site@lat,
        site_lon
    )]
    # ERA5 results can retain two expver slices near the ERA5/ERA5T boundary.
    # Prefer the first finite value in coordinate order for each valid time.
    data <- data[is.finite(value)]
    if (!nrow(data)) {
        cli::cli_abort(
            "ERA NetCDF result contains no finite {.val {source_variable}} values."
        )
    }
    data.table::setorderv(data, setdiff(names(data), "value"))
    data <- data[, .(
        value = value[[1L]],
        grid_lat = grid_lat[[1L]],
        grid_lon = grid_lon[[1L]],
        grid_dist_km = grid_dist_km[[1L]]
    ), by = utc_time]
    units <- as.character(era__netcdf_attribute(
        handle,
        data_name,
        "units",
        default = ""
    ))
    list(data = data[], units = units, variable = data_name,
        path = normalizePath(path, winslash = "/", mustWork = TRUE))
}

# Infer the interval represented by an accumulated hourly field. The first
# sample uses the modal positive spacing because it has no preceding row.
era__interval_seconds <- function(time) {
    seconds <- diff(as.numeric(time))
    seconds <- seconds[is.finite(seconds) & seconds > 0]
    if (!length(seconds)) {
        return(rep(3600, length(time)))
    }
    interval <- stats::median(seconds)
    rep(interval, length(time))
}

# Convert one raw ERA source field to the CF-compatible units consumed by
# existing weather method contracts.
era__convert_source <- function(source, source_variable) {
    data <- data.table::copy(source$data)
    units <- tolower(gsub("[[:space:]_]", "", source$units))
    if (source_variable %in% c("tas", "tdps")) {
        if (units %in% c("c", "degc", "degreecelsius", "degreescelsius")) {
            data[, value := value + 273.15]
        }
    } else if (source_variable %in% c("ps", "psl")) {
        if (units %in% c("hpa", "mbar", "millibar")) {
            data[, value := value * 100]
        }
    } else if (source_variable %in% c("rsds", "rlds")) {
        if (grepl("j", units, fixed = TRUE)) {
            data[, value := value / era__interval_seconds(utc_time)]
        }
    } else if (identical(source_variable, "pr")) {
        interval <- era__interval_seconds(data$utc_time)
        if (units %in% c("m", "metre", "meter")) {
            data[, value := value * 1000 / interval]
        } else if (!grepl("s-1|/s", units)) {
            data[, value := value / interval]
        }
    } else if (identical(source_variable, "clt")) {
        if (max(data$value, na.rm = TRUE) <= 1 + sqrt(.Machine$double.eps)) {
            data[, value := value * 100]
        }
    }
    data[]
}

# Combine primitive ERA source fields and derive the canonical humidity and
# wind variables required by weather transformations.
era__canonical_hourly <- function(raw, variables) {
    converted <- lapply(names(raw), function(variable) {
        data <- era__convert_source(raw[[variable]], variable)
        data.table::setnames(data, "value", variable)
        data[, c("utc_time", variable), with = FALSE]
    })
    names(converted) <- names(raw)
    common <- Reduce(function(left, right) {
        merge(left, right, by = "utc_time", all = FALSE, sort = TRUE,
            suffixes = c("", ".source"))
    }, converted)
    if (!nrow(common)) {
        cli::cli_abort("ERA source fields have no common timestamps.")
    }
    first_grid <- raw[[1L]]$data
    grid_lat <- first_grid$grid_lat[[1L]]
    grid_lon <- first_grid$grid_lon[[1L]]
    grid_dist_km <- first_grid$grid_dist_km[[1L]]

    if ("hurs" %in% variables) {
        common[, hurs := 100 * exp(
            epwphys__psychro_ln_pws(tdps - 273.15) -
                epwphys__psychro_ln_pws(tas - 273.15)
        )]
        common[, hurs := pmin(100, pmax(0, hurs))]
    }
    if ("huss" %in% variables) {
        relative <- 100 * exp(
            epwphys__psychro_ln_pws(common$tdps - 273.15) -
                epwphys__psychro_ln_pws(common$tas - 273.15)
        )
        common[, huss := epwphys__huss_from_rh_si(
            tas - 273.15,
            relative,
            ps
        )]
    }
    if ("sfcWind" %in% variables) {
        common[, sfcWind := sqrt(uas ^ 2 + vas ^ 2)]
    }
    # Daily extrema are derived later from the hourly temperature field.
    for (variable in intersect(c("tasmin", "tasmax"), variables)) {
        common[[variable]] <- common$tas
    }
    missing <- setdiff(variables, names(common))
    if (length(missing)) {
        cli::cli_abort(
            "ERA normalization did not produce CF variable(s): {.val {missing}}."
        )
    }
    list(
        data = common[, c("utc_time", variables), with = FALSE],
        grid = list(
            lon = grid_lon,
            lat = grid_lat,
            distance_km = grid_dist_km,
            elevation_m = NA_real_
        )
    )
}

# Resolve the fixed standard-time offset that defines calendar aggregation for
# one EPW site. This value also participates in persistent reanalysis IDs.
era__site_timezone <- function(site) {
    epw <- shift_resolve_epw(site)
    morpher__epw_location_numeric(
        epw,
        c("time_zone", "timezone", "N4_time_zone"),
        default = 0
    )
}

# Convert UTC hourly data to the EPW fixed standard offset before any daily or
# monthly grouping, then attach the same CF coordinate columns used by CMIP6.
era__aggregate_variable <- function(
    data,
    variable,
    frequency,
    timezone,
    years,
    lon,
    lat
) {
    specification <- era__frequency_kind(frequency)
    local_seconds <- as.numeric(data$utc_time) + timezone * 3600
    local_time <- as.POSIXct(local_seconds, origin = "1970-01-01", tz = "UTC")
    values <- data[[variable]]
    table <- data.table::data.table(local_time = local_time, value = values)
    aggregation <- if (identical(variable, "tasmin")) {
        min
    } else if (identical(variable, "tasmax")) {
        max
    } else {
        mean
    }

    if (identical(specification$kind, "subdaily")) {
        hour <- as.POSIXlt(table$local_time, tz = "UTC")$hour
        if (isTRUE(specification$point)) {
            table <- table[hour %% specification$hours == 0L]
            output <- table
        } else {
            block_seconds <- specification$hours * 3600
            table[, block := floor(as.numeric(local_time) / block_seconds)]
            output <- table[, .(
                local_time = as.POSIXct(
                    min(as.numeric(local_time)),
                    origin = "1970-01-01",
                    tz = "UTC"
                ),
                value = aggregation(value, na.rm = TRUE)
            ), by = block]
            output[, block := NULL]
        }
    } else if (identical(specification$kind, "day")) {
        table[, date := as.Date(local_time, tz = "UTC")]
        output <- table[, .(
            local_time = as.POSIXct(
                paste(date[[1L]], "12:00:00"),
                tz = "UTC"
            ),
            value = aggregation(value, na.rm = TRUE)
        ), by = date]
        output[, date := NULL]
    } else {
        fields <- as.POSIXlt(table$local_time, tz = "UTC")
        table[, `:=`(
            year = fields$year + 1900L,
            month = fields$mon + 1L
        )]
        output <- table[, .(
            local_time = as.POSIXct(
                sprintf("%04d-%02d-15 12:00:00", year[[1L]],
                    month[[1L]]),
                tz = "UTC"
            ),
            value = aggregation(value, na.rm = TRUE)
        ), by = .(year, month)]
        output[, c("year", "month") := NULL]
    }
    year <- as.POSIXlt(output$local_time, tz = "UTC")$year + 1900L
    output <- output[year %in% years & is.finite(value)]
    fields <- as.POSIXlt(output$local_time, tz = "UTC")
    calendar_fields <- data.frame(
        year = fields$year + 1900L,
        month = fields$mon + 1L,
        day = fields$mday,
        hour = fields$hour,
        minute = fields$min,
        second = fields$sec
    )
    coordinates <- cf_time__coordinates(calendar_fields, "standard")
    output[, `:=`(
        variable_id = variable,
        frequency = specification$label,
        time = local_time,
        lon = lon,
        lat = lat,
        units = era5__variable_manifest()[
            variable_id == variable,
            units
        ][[1L]],
        time_basis = "epw_local_standard",
        utc_offset_hours = timezone
    )]
    output[, local_time := NULL]
    for (name in names(coordinates)) {
        output[[name]] <- coordinates[[name]]
    }
    data.table::setcolorder(output, c(
        "variable_id", "frequency", "time", CF_TIME_COORDINATE_COLUMNS,
        "lon", "lat", "units", "time_basis", "utc_offset_hours", "value"
    ))
    output[]
}

# Normalize all requested variables and frequencies to the long CF weather
# representation consumed by ShiftClimate and EpwMorpher.
era__normalize <- function(raw, variables, frequencies, site, years) {
    hourly <- era__canonical_hourly(raw, variables)
    timezone <- era__site_timezone(site)
    rows <- lapply(variables, function(variable) {
        era__aggregate_variable(
            hourly$data,
            variable = variable,
            frequency = frequencies[[variable]],
            timezone = timezone,
            years = years,
            lon = site@lon,
            lat = site@lat
        )
    })
    list(
        data = data.table::rbindlist(rows, use.names = TRUE, fill = TRUE),
        grid = hourly$grid,
        timezone = timezone
    )
}

# Build stable provider, file, and plan IDs before network access so complete
# observed references can be resumed without submitting another CDS job.
reanalysis__identities <- function(spec, site, variables, frequencies, access) {
    timezone <- era__site_timezone(site)
    request_id <- store__hash(
        "reanalysis-v1",
        spec@dataset,
        spec@product,
        paste(spec@years, collapse = ","),
        site@id,
        reanalysis__longitude(site@lon),
        site@lat,
        timezone,
        paste(names(frequencies), frequencies, sep = "=", collapse = ";"),
        access
    )
    file_keys <- stats::setNames(vapply(variables, function(variable) {
        store__hash("reanalysis-file-v1", request_id, variable)
    }, character(1L)), variables)
    plan_ids <- stats::setNames(vapply(variables, function(variable) {
        store__hash(
            "reanalysis-plan-v1",
            request_id,
            file_keys[[variable]],
            variable,
            frequencies[[variable]]
        )
    }, character(1L)), variables)
    list(query_id = request_id, file_keys = file_keys, plan_ids = plan_ids)
}

# Restore a complete provider-normalized reference without reading or
# downloading its raw ERA files again.
reanalysis__existing_climate <- function(
    store,
    spec,
    site,
    periods,
    variables,
    frequencies,
    access,
    identities
) {
    coverage <- tryCatch(
        store$coverage(plan_id = unname(identities$plan_ids)),
        error = function(error) data.table::data.table()
    )
    if (nrow(coverage) != length(identities$plan_ids) ||
        !all(coverage$complete %in% TRUE)) {
        return(NULL)
    }
    shift_stage_new(
        ShiftClimate,
        "climate",
        store_path = store$path,
        ids = list(
            query_id = identities$query_id,
            plan_id = unname(identities$plan_ids)
        ),
        meta = list(
            site = site,
            periods = periods,
            variables = variables,
            coverage = coverage,
            source = spec,
            access = access,
            reused = TRUE
        ),
        diagnostics = shift_diagnostics_from_coverage(coverage)
    )
}

# Construct one full-width file catalog row for provider-normalized source data
# so existing ShiftClimate joins and artifact inspectors remain reusable.
reanalysis__file_row <- function(
    store,
    spec,
    variable,
    frequency,
    dataset_id,
    path,
    artifact_id,
    identities,
    grid,
    data
) {
    now <- store__now()
    file_key <- identities$file_keys[[variable]]
    data.frame(
        file_key = file_key,
        query_id = identities$query_id,
        esgf_id = NA_character_,
        dataset_id = dataset_id,
        master_id = NA_character_,
        instance_id = sprintf("%s.%s", spec@dataset, variable),
        version = toupper(spec@dataset),
        title = sprintf("%s %s", toupper(spec@dataset), variable),
        filename = basename(path),
        tracking_id = NA_character_,
        checksum = store_hash_file(path, "sha256"),
        checksum_type = "sha256",
        size = as.numeric(file.info(path)$size),
        latest = TRUE,
        replica = FALSE,
        retracted = FALSE,
        deprecated = FALSE,
        data_node = "cds.climate.copernicus.eu",
        activity_id = "reanalysis",
        institution_id = "ECMWF",
        source_id = toupper(spec@dataset),
        experiment_id = "observation",
        variant_label = "reanalysis",
        frequency = frequency,
        table_id = dataset_id,
        variable_id = variable,
        grid_label = sprintf("%.3f_%.3f", grid$lat, grid$lon),
        datetime_start = min(data$time),
        datetime_end = max(data$time),
        actual_time_start = min(data$time),
        actual_time_end = max(data$time),
        url_opendap = NA_character_,
        url_download = NA_character_,
        local_path = store_rel_path(path, store$path),
        local_artifact_id = artifact_id,
        created_at = now,
        stringsAsFactors = FALSE
    )
}

# Persist normalized reanalysis rows through the same plan/result/artifact
# tables and Parquet writer used by CMIP6 extraction.
reanalysis__persist <- function(
    store,
    spec,
    site,
    periods,
    normalized,
    variables,
    frequencies,
    access,
    dataset_id,
    raw,
    identities,
    overwrite = FALSE
) {
    private <- priv(store)
    plan_ids <- character()
    jobs <- list()
    for (variable in variables) {
        source_variables <- era5__source_variables(variable, spec@product)
        source_variable <- source_variables[[1L]]
        source <- raw[[source_variable]]
        file_key <- identities$file_keys[[variable]]
        artifact_id <- store$register_artifact(
            kind = "netcdf",
            path = source$path,
            role = "download",
            project = toupper(spec@dataset),
            query_id = identities$query_id,
            file_key = file_key,
            source_url = sprintf(
                "https://cds.climate.copernicus.eu/datasets/%s",
                dataset_id
            ),
            metadata = list(
                provider = spec@provider,
                dataset = spec@dataset,
                product = spec@product,
                access = access,
                request_ids = unname(vapply(
                    raw[source_variables],
                    function(value) as.character(value$request_id),
                    character(1L)
                )),
                source_variables = source_variables,
                source_paths = unname(vapply(
                    raw[source_variables],
                    `[[`,
                    character(1L),
                    "path"
                ))
            )
        )
        rows <- normalized$data[variable_id == variable]
        file <- reanalysis__file_row(
            store,
            spec,
            variable,
            frequencies[[variable]],
            dataset_id,
            source$path,
            artifact_id,
            identities,
            normalized$grid,
            rows
        )
        private$replace_rows("file_catalog", file, "file_key")
        now <- store__now()
        plan <- data.frame(
            plan_id = identities$plan_ids[[variable]],
            query_id = identities$query_id,
            file_key = file_key,
            site_id = site@id,
            variable_id = variable,
            lon = site@lon,
            lat = site@lat,
            method = "nearest",
            time_start = min(rows$time),
            time_stop = max(rows$time),
            status = "done",
            available_time_count = data.table::uniqueN(rows$time),
            attempt_count = 1L,
            last_error = NA_character_,
            created_at = now,
            updated_at = now,
            stringsAsFactors = FALSE
        )
        rows[, `:=`(
            plan_id = plan$plan_id[[1L]],
            file_key = file_key,
            query_id = identities$query_id,
            site_id = site@id,
            source_id = toupper(spec@dataset),
            experiment_id = "observation",
            variant_label = "reanalysis",
            table_id = dataset_id,
            variable = variable,
            grid_label = file$grid_label[[1L]],
            method = "nearest"
        )]
        private$replace_rows("extraction_plan", plan, "plan_id")
        private$delete_by_key("extraction_result", "plan_id", plan$plan_id)
        results <- private$write_extract_partitions(
            rows,
            data.table::as.data.table(plan),
            data.table::as.data.table(file),
            overwrite = overwrite,
            project = toupper(spec@dataset)
        )
        private$replace_rows("extraction_result", as.data.frame(results),
            "result_id")
        grid_source <- data.frame(
            source_row_id = store__hash(plan$plan_id, "grid", 1L),
            plan_id = plan$plan_id,
            file_key = file_key,
            query_id = identities$query_id,
            variable_id = variable,
            method = "nearest",
            source_index = 1L,
            role = "nearest",
            grid_lon = normalized$grid$lon,
            grid_lat = normalized$grid$lat,
            grid_elevation_m = normalized$grid$elevation_m,
            grid_dist_km = normalized$grid$distance_km,
            weight = 1,
            created_at = now,
            stringsAsFactors = FALSE
        )
        private$replace_rows(
            "extraction_grid_source",
            grid_source,
            "source_row_id"
        )
        plan_ids <- c(plan_ids, plan$plan_id)
        jobs[[variable]] <- unname(vapply(
            raw[source_variables],
            function(value) as.character(value$request_id),
            character(1L)
        ))
    }
    coverage <- store$coverage(plan_id = plan_ids)
    diagnostics <- shift_bind_diagnostics(
        shift_diagnostics_from_coverage(coverage),
        shift_diagnostic(
            "observed reference",
            "info",
            "reanalysis_grid_selection",
            sprintf(
                "Selected %s grid centre %.4f, %.4f (%.2f km from the EPW site).",
                toupper(spec@dataset),
                normalized$grid$lat,
                normalized$grid$lon,
                normalized$grid$distance_km
            ),
            action = "No altitude correction was applied."
        ),
        if (is.na(normalized$grid$elevation_m)) {
            shift_diagnostic(
                "observed reference",
                "info",
                "reanalysis_grid_elevation_unavailable",
                paste(
                    "The selected reanalysis product did not include grid",
                    "elevation; it is recorded as missing."
                ),
                action = "Supply an explicit elevation policy in a future workflow if correction is required."
            )
        }
    )
    shift_stage_new(
        ShiftClimate,
        "climate",
        store_path = store$path,
        ids = list(query_id = identities$query_id, plan_id = plan_ids),
        meta = list(
            site = site,
            periods = periods,
            variables = variables,
            frequencies = frequencies,
            coverage = coverage,
            source = spec,
            access = access,
            dataset_id = dataset_id,
            jobs = jobs,
            grid = normalized$grid,
            timezone = normalized$timezone,
            reused = FALSE
        ),
        diagnostics = diagnostics
    )
}

# Materialize an ERA5 source only when a method actually consumes the observed
# reference role. Dry-run plans therefore remain network-free after CMIP model
# discovery, while execution produces an ordinary ShiftClimate stage.
reanalysis__materialize <- function(
    x,
    recipe,
    site,
    spec,
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL
) {
    if (!S7::S7_inherits(spec, ShiftReanalysisSpec)) {
        cli::cli_abort("`spec` must be a {.cls ShiftReanalysisSpec}.")
    }
    if (!identical(spec@dataset, "era5")) {
        cli::cli_abort(
            "Reanalysis dataset {.val {spec@dataset}} is not available."
        )
    }
    variables <- reanalysis__variables(spec, recipe)
    frequencies <- reanalysis__frequencies(spec, recipe, variables)
    access <- era5__resolve_access(spec, variables)
    dataset_id <- era5__dataset_id(spec@product, access)
    periods <- shift__periods_from_years(
        spec@years,
        period = "observed",
        arg = "calibration$years"
    )
    store <- shift_store(x)
    identities <- reanalysis__identities(
        spec,
        site,
        variables,
        frequencies,
        access
    )
    if (isTRUE(resume) && !isTRUE(overwrite)) {
        existing <- reanalysis__existing_climate(
            store,
            spec,
            site,
            periods,
            variables,
            frequencies,
            access,
            identities
        )
        if (!is.null(existing)) {
            return(existing)
        }
    }

    source_variables <- era5__source_variables(variables, spec@product)
    retrieve <- getOption("epwshiftr.reanalysis.retrieve", cds__retrieve)
    reader <- getOption("epwshiftr.reanalysis.read", era__read_netcdf)
    if (!is.function(retrieve) || !is.function(reader)) {
        cli::cli_abort("Configured reanalysis adapters must be functions.")
    }
    raw <- list()
    for (index in seq_along(source_variables)) {
        source_variable <- source_variables[[index]]
        request <- era5__request(spec, source_variable, site, access)
        padding_years <- attr(request, "requested_years", exact = TRUE)
        requested_years <- if (is.null(padding_years)) {
            spec@years
        } else {
            as.integer(padding_years)
        }
        target <- file.path(
            store$path,
            "sources",
            "reanalysis",
            spec@dataset,
            access,
            source_variable,
            sprintf(
                "%d-%d-%s.nc",
                min(requested_years),
                max(requested_years),
                substr(store__hash(request), 1L, 12L)
            )
        )
        if (!is.null(reporter)) {
            reporter$unit_started(
                sprintf("ERA5 %s", source_variable),
                current = index,
                total = length(source_variables),
                details = list(
                    unit_type = "reanalysis_variable",
                    variable = source_variable,
                    access = access
                )
            )
        }
        retrieved <- do.call(retrieve, c(list(
            dataset_id,
            request,
            target,
            reporter = reporter,
            overwrite = overwrite
        ), spec@options))
        read <- reader(retrieved$path, source_variable, site)
        read$path <- retrieved$path
        read$request_id <- if (is.null(retrieved$job)) {
            NA_character_
        } else {
            retrieved$job$request_id
        }
        read$provider_variable <- era5__variable_manifest()[
            variable_id == source_variable,
            era_variable
        ][[1L]]
        read$requested_years <- requested_years
        raw[[source_variable]] <- read
        if (!is.null(reporter)) {
            reporter$unit_completed(
                sprintf("Prepared ERA5 %s", source_variable),
                current = index,
                total = length(source_variables),
                outcome = if (isTRUE(retrieved$reused)) "skipped" else
                    "completed",
                details = list(
                    unit_type = "reanalysis_variable",
                    variable = source_variable,
                    access = access
                )
            )
        }
    }
    normalized <- era__normalize(
        raw,
        variables,
        frequencies,
        site,
        spec@years
    )
    reanalysis__persist(
        store,
        spec,
        site,
        periods,
        normalized,
        variables,
        frequencies,
        access,
        dataset_id,
        raw,
        identities,
        overwrite = overwrite
    )
}
