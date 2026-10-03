# Operational native-value limit for each subset request, shared by weather
# reads, CF bounds and batch scheduling. This is a tested conservative default,
# not a NetCDF limit or a universal optimum; retune it with transport benchmarks.
DATASET_REQUEST_MAX_VALUES <- 8192L

# Return the multi-site schema even when a file has no selected native times.
# The provenance tables remain present so callers can inspect an empty read.
dataset__empty_regions <- function() {
    values <- data.table::data.table(
        file_index = integer(),
        variable = character(),
        site_id = character(),
        time = as.POSIXct(character(), tz = "UTC"),
        time_bound_start = as.POSIXct(character(), tz = "UTC"),
        time_bound_end = as.POSIXct(character(), tz = "UTC"),
        cf_calendar = character(),
        cf_year = integer(),
        cf_month = integer(),
        cf_day = integer(),
        cf_day_of_year = integer(),
        cf_year_days = integer(),
        cf_second_of_day = numeric(),
        annual_phase = numeric(),
        lon = numeric(),
        lat = numeric(),
        method = character(),
        value = numeric()
    )
    attr(values, "grid_sources") <- data.table::data.table(
        site_id = character(),
        source_index = integer(),
        role = character(),
        grid_lon = numeric(),
        grid_lat = numeric(),
        grid_dist_km = numeric(),
        weight = numeric(),
        file_index = integer(),
        variable = character(),
        method = character()
    )
    attr(values, "read_slices") <- data.table::data.table(
        file_index = integer(),
        variable = character(),
        ind_lat = integer(),
        ind_lon = integer(),
        lat_count = integer(),
        lon_count = integer(),
        time_start_index = integer(),
        time_count = integer()
    )
    values
}

# Validate one site table before opening any NetCDF variable. The site ID is
# retained in both values and source-cell provenance to keep consumers apart.
dataset__region_sites <- function(sites) {
    checkmate::assert_data_table(sites, min.rows = 1L)
    required <- c("site_id", "lon", "lat", "method")
    checkmate::assert_names(names(sites), must.include = required)
    has_windows <- all(c("time_start", "time_stop") %in% names(sites))
    if (xor("time_start" %in% names(sites), "time_stop" %in% names(sites))) {
        stop(
            "`sites` must provide both time_start and time_stop.",
            call. = FALSE
        )
    }
    columns <- if (has_windows) {
        c(required, "time_start", "time_stop")
    } else {
        required
    }
    sites <- data.table::copy(sites[, columns, with = FALSE])
    checkmate::assert_character(
        sites$site_id,
        any.missing = FALSE,
        min.chars = 1L,
        unique = TRUE
    )
    checkmate::assert_numeric(
        sites$lon,
        lower = -180,
        upper = 360,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_numeric(
        sites$lat,
        lower = -90,
        upper = 90,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_character(sites$method, any.missing = FALSE)
    checkmate::assert_subset(sites$method, ESG_GRID_METHOD_CHOICES)
    if (has_windows) {
        start <- as.POSIXct(sites$time_start, tz = "UTC")
        stop <- as.POSIXct(sites$time_stop, tz = "UTC")
        if (anyNA(start) || anyNA(stop) || any(start > stop)) {
            stop(
                "Each site time window must have valid ordered UTC endpoints.",
                call. = FALSE
            )
        }
        data.table::set(sites, j = "time_start", value = start)
        data.table::set(sites, j = "time_stop", value = stop)
    }
    sites
}

# Partition source cells into at most 2-by-2 native rectangles. Starting at
# the lowest unassigned latitude/longitude keeps distant sites in separate
# requests while allowing adjacent interpolation cells to share one read.
dataset__region_cell_groups <- function(points) {
    data.table::setorderv(points, c("ind_lat", "ind_lon"))
    keys <- paste(points$ind_lat, points$ind_lon, sep = ":")
    assigned <- rep.int(FALSE, nrow(points))
    groups <- vector("list", nrow(points))
    count <- 0L
    for (first in seq_len(nrow(points))) {
        if (assigned[[first]]) {
            next
        }
        lat <- points$ind_lat[[first]]
        lon <- points$ind_lon[[first]]
        candidates <- paste(
            rep.int(c(lat, lat + 1L), times = 2L),
            rep(c(lon, lon + 1L), each = 2L),
            sep = ":"
        )
        members <- match(candidates, keys)
        members <- members[!is.na(members) & !assigned[members]]
        assigned[members] <- TRUE
        count <- count + 1L
        groups[[count]] <- list(
            members = members,
            lat = lat,
            lon = lon,
            lat_count = max(points$ind_lat[members]) - lat + 1L,
            lon_count = max(points$ind_lon[members]) - lon + 1L
        )
    }
    groups[seq_len(count)]
}

# Partition contiguous native positions into bounded runs. Spatial reads use
# a time limit based on their spatial area; interval bounds account for both
# endpoints. Callers derive their time limit from DATASET_REQUEST_MAX_VALUES.
dataset__region_runs <- function(indices, max_time) {
    if (!length(indices)) {
        return(list())
    }
    contiguous <- split(
        indices,
        cumsum(c(1L, as.integer(diff(indices) != 1L)))
    )
    unlist(
        lapply(contiguous, function(run) {
            split(run, ceiling(seq_along(run) / max_time))
        }),
        recursive = FALSE
    )
}

# Read one variable from one file in bounded native-time and spatial slices.
# The returned table is deliberately capped; longer acquisitions must be
# scheduled as separate time windows by the batch execution stage.
dataset__read_regions_one <- function(
    dataset,
    variable,
    sites,
    time,
    index,
    async,
    timeout
) {
    private <- dataset__private(dataset)
    # Variable presence is checked by the caller; metadata or transport errors
    # must reach the caller instead of being reported as an absent variable.
    meta <- private$get_var_dim_meta(variable, index)
    required_dims <- c("time", "lat", "lon")
    if (!all(required_dims %in% meta$names)) {
        stop(
            sprintf(
                "Variable '%s' is missing a required time or spatial dimension.",
                variable
            ),
            call. = FALSE
        )
    }
    extra <- setdiff(meta$names, required_dims)
    if (length(extra) && any(meta$lengths[match(extra, meta$names)] != 1L)) {
        stop(
            sprintf(
                "Variable '%s' has an unsupported non-spatiotemporal dimension.",
                variable
            ),
            call. = FALSE
        )
    }

    time_info <- dataset__time_axis(dataset, index)
    base_selected <- cf_time__range_indices(
        time_info$values,
        time_info$coordinates,
        time
    )
    site_times <- if ("time_start" %in% names(sites)) {
        # Many consumers share the same period; resolve each CF-native window
        # once instead of scanning the full native axis for every site.
        window_key <- paste(
            as.numeric(sites$time_start),
            as.numeric(sites$time_stop),
            sep = "/"
        )
        first <- which(!duplicated(window_key))
        unique_times <- lapply(first, function(site_index) {
            intersect(
                base_selected,
                cf_time__range_indices(
                    time_info$values,
                    time_info$coordinates,
                    c(
                        sites$time_start[[site_index]],
                        sites$time_stop[[site_index]]
                    )
                )
            )
        })
        unique_times[match(window_key, window_key[first])]
    } else {
        rep(list(base_selected), nrow(sites))
    }
    selected <- sort(unique(unlist(site_times, use.names = FALSE)))
    if (!length(selected)) {
        return(dataset__empty_regions())
    }
    if (sum(lengths(site_times)) > 250000L) {
        stop(
            "The region read exceeds 250000 output rows; split the acquisition into time windows.",
            call. = FALSE
        )
    }
    grid <- dataset$get_spatial_grid(index = index)
    if (is.null(grid$lat) || is.null(grid$lon)) {
        stop(
            "The file does not expose both lat and lon coordinates.",
            call. = FALSE
        )
    }
    if (length(dim(grid$lat)) > 1L || length(dim(grid$lon)) > 1L) {
        stop(
            "Sparse multi-site reads require rectilinear 1D coordinates.",
            call. = FALSE
        )
    }
    grid_lat <- as.vector(grid$lat)
    grid_lon <- as.vector(grid$lon)
    # Nearest and IDW sites reuse one coordinate table. Cell methods use only
    # their four surrounding coordinates and need no full-grid expansion.
    nearest_coords <- if (any(sites$method %in% c("nearest", "idw"))) {
        private$make_region_grid_coords(grid_lat, grid_lon)
    } else {
        NULL
    }
    sources <- vector("list", nrow(sites))
    for (site_index in seq_len(nrow(sites))) {
        site <- sites[site_index]
        target_lon <- private$normalize_lon_for_grid(site$lon[[1L]], grid_lon)
        source <- private$region_grid_sources(
            site$method[[1L]],
            grid_lat,
            grid_lon,
            site$lat[[1L]],
            target_lon,
            coords = nearest_coords
        )
        data.table::set(source, j = "site_id", value = site$site_id[[1L]])
        sources[[site_index]] <- source
    }
    sources <- data.table::rbindlist(sources, use.names = TRUE)
    points <- unique(sources[, c("ind_lat", "ind_lon"), with = FALSE])
    groups <- dataset__region_cell_groups(points)
    if (length(groups) > 4096L) {
        stop(
            "The region read exceeds 4096 source requests; split the site collection.",
            call. = FALSE
        )
    }
    point_key <- paste(points$ind_lat, points$ind_lon, sep = ":")
    data.table::set(
        sources,
        j = "point_index",
        value = match(
            paste(sources$ind_lat, sources$ind_lon, sep = ":"),
            point_key
        )
    )

    # A group's time demand is the union of its consumers. Limit the source
    # matrix to 250000 values even when site windows barely overlap.
    point_users <- split(sources$site_id, sources$point_index)
    point_times <- lapply(point_users, function(users) {
        using <- unique(users)
        sort(unique(unlist(
            site_times[match(using, sites$site_id)],
            use.names = FALSE
        )))
    })
    group_times <- lapply(groups, function(group) {
        sort(unique(unlist(point_times[group$members], use.names = FALSE)))
    })
    # Single-cell groups can use the full native-value allowance. The working
    # matrix has its own limit; each spatial group splits runs independently.
    max_times <- vapply(
        groups,
        function(group) {
            DATASET_REQUEST_MAX_VALUES %/% (group$lat_count * group$lon_count)
        },
        integer(1L)
    )
    block_time <- min(max(max_times), max(1L, 250000L %/% nrow(points)))
    blocks <- split(selected, ceiling(seq_along(selected) / block_time))
    requests <- lapply(blocks, function(block) {
        lapply(seq_along(groups), function(index) {
            dataset__region_runs(
                intersect(block, group_times[[index]]),
                max_time = max_times[[index]]
            )
        })
    })
    request_count <- sum(vapply(
        requests,
        function(block) {
            sum(lengths(block))
        },
        integer(1L)
    ))
    if (request_count > 4096L) {
        stop(
            "The region read exceeds 4096 source requests; split the acquisition into time windows.",
            call. = FALSE
        )
    }
    read_slices <- vector("list", request_count)
    pieces <- vector("list", length(blocks) * nrow(sites))
    slice_index <- 0L
    time_position <- match("time", meta$names)
    lat_position <- match("lat", meta$names)
    lon_position <- match("lon", meta$names)
    bounds <- dataset__selected_bounds(dataset, index, time_info, selected)
    clock <- data.table::as.data.table(time_info$coordinates[
        selected,
        CF_TIME_COORDINATE_COLUMNS,
        drop = FALSE
    ])
    data.table::set(clock, j = "time", value = time_info$values[selected])
    if (!is.null(bounds)) {
        data.table::set(
            clock,
            j = "time_bound_start",
            value = bounds$start[seq_along(selected)]
        )
        data.table::set(
            clock,
            j = "time_bound_end",
            value = bounds$end[seq_along(selected)]
        )
    }
    for (block_index in seq_along(blocks)) {
        block <- blocks[[block_index]]
        values <- matrix(NA_real_, nrow = length(block), ncol = nrow(points))
        for (group_index in seq_along(groups)) {
            group <- groups[[group_index]]
            for (run in requests[[block_index]][[group_index]]) {
                start <- rep.int(1L, length(meta$names))
                count <- rep.int(1L, length(meta$names))
                start[[time_position]] <- run[[1L]]
                start[[lat_position]] <- group$lat
                start[[lon_position]] <- group$lon
                count[[time_position]] <- length(run)
                count[[lat_position]] <- group$lat_count
                count[[lon_position]] <- group$lon_count
                raw <- dataset$var_get(
                    variable,
                    start = start,
                    count = count,
                    index = index,
                    collapse = FALSE,
                    async = async,
                    timeout = timeout
                )
                if (length(raw) != prod(count)) {
                    stop(
                        "A spatial slice did not return the requested native values.",
                        call. = FALSE
                    )
                }
                ordered <- aperm(
                    raw,
                    c(
                        time_position,
                        lat_position,
                        lon_position,
                        setdiff(
                            seq_along(count),
                            c(time_position, lat_position, lon_position)
                        )
                    )
                )
                raw_values <- matrix(as.vector(ordered), nrow = length(run))
                member <- group$members
                columns <- (points$ind_lon[member] - group$lon) *
                    group$lat_count +
                    points$ind_lat[member] -
                    group$lat +
                    1L
                values[match(run, block), member] <- raw_values[,
                    columns,
                    drop = FALSE
                ]
                slice_index <- slice_index + 1L
                read_slices[[slice_index]] <- data.table::data.table(
                    file_index = index,
                    variable = variable,
                    ind_lat = group$lat,
                    ind_lon = group$lon,
                    lat_count = group$lat_count,
                    lon_count = group$lon_count,
                    time_start_index = run[[1L]],
                    time_count = length(run)
                )
            }
        }
        for (site_index in seq_len(nrow(sites))) {
            rows <- match(intersect(site_times[[site_index]], block), block)
            if (!length(rows)) {
                next
            }
            site <- sites[site_index]
            source <- sources[sources$site_id == site$site_id[[1L]]]
            piece <- data.table::copy(clock[match(block[rows], selected)])
            # Matrix multiplication preserves missing-source propagation.
            weighted <- as.vector(
                values[rows, source$point_index, drop = FALSE] %*%
                    source$weight
            )
            data.table::set(piece, j = "value", value = weighted)
            data.table::set(piece, j = "file_index", value = index)
            data.table::set(piece, j = "variable", value = variable)
            data.table::set(piece, j = "site_id", value = site$site_id[[1L]])
            data.table::set(piece, j = "lon", value = site$lon[[1L]])
            data.table::set(piece, j = "lat", value = site$lat[[1L]])
            data.table::set(piece, j = "method", value = site$method[[1L]])
            data.table::setcolorder(
                piece,
                c(
                    "file_index",
                    "variable",
                    "site_id",
                    "time",
                    intersect(
                        c("time_bound_start", "time_bound_end"),
                        names(piece)
                    ),
                    CF_TIME_COORDINATE_COLUMNS,
                    "lon",
                    "lat",
                    "method",
                    "value"
                )
            )
            pieces[[(site_index - 1L) * length(blocks) + block_index]] <- piece
        }
    }
    output <- data.table::rbindlist(pieces, use.names = TRUE)
    source_columns <- c(
        "site_id",
        "source_index",
        "role",
        "grid_lon",
        "grid_lat",
        "grid_dist_km",
        "weight"
    )
    provenance <- data.table::copy(sources[, source_columns, with = FALSE])
    data.table::set(provenance, j = "file_index", value = index)
    data.table::set(provenance, j = "variable", value = variable)
    data.table::set(
        provenance,
        j = "method",
        value = sites$method[match(provenance$site_id, sites$site_id)]
    )
    attr(output, "grid_sources") <- provenance
    attr(output, "read_slices") <- data.table::rbindlist(read_slices)
    output
}

# Assemble file-variable pieces without losing native CF or point provenance.
# Missing variables follow the single-site reader's warning contract.
dataset__read_regions <- function(
    dataset,
    variable,
    sites,
    time = "auto",
    async = FALSE,
    timeout = NULL
) {
    checkmate::assert_character(
        variable,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    sites <- dataset__region_sites(sites)
    private <- dataset__private(dataset)
    private$validate_async_request(async, timeout)
    private$check_open()
    time <- private$normalize_region_time(time)
    pieces <- vector("list", length(private$urls) * length(variable))
    piece_index <- 0L
    output_rows <- 0L
    found <- stats::setNames(rep.int(FALSE, length(variable)), variable)
    for (index in seq_along(private$urls)) {
        available <- dataset$get_variables(index = index)
        for (name in variable) {
            if (!name %in% available) {
                next
            }
            piece <- dataset__read_regions_one(
                dataset,
                name,
                sites,
                time,
                index,
                async,
                timeout
            )
            output_rows <- output_rows + nrow(piece)
            if (output_rows > 250000L) {
                stop(
                    "The combined region read exceeds 250000 output rows; split the acquisition into time windows.",
                    call. = FALSE
                )
            }
            found[[name]] <- TRUE
            piece_index <- piece_index + 1L
            pieces[[piece_index]] <- piece
        }
    }
    pieces <- pieces[seq_len(piece_index)]
    missing <- names(found)[!found]
    if (length(missing) == length(found)) {
        stop(
            sprintf(
                "None of the requested variable(s) were found: %s.",
                paste(missing, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    if (length(missing)) {
        warning(
            sprintf(
                "The following variable(s) were skipped: %s.",
                paste(missing, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    if (!length(pieces)) {
        return(dataset__empty_regions())
    }
    output <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
    attr(output, "grid_sources") <- data.table::rbindlist(
        lapply(
            pieces,
            attr,
            which = "grid_sources",
            exact = TRUE
        ),
        use.names = TRUE,
        fill = TRUE
    )
    attr(output, "read_slices") <- data.table::rbindlist(
        lapply(
            pieces,
            attr,
            which = "read_slices",
            exact = TRUE
        ),
        use.names = TRUE,
        fill = TRUE
    )
    output
}
