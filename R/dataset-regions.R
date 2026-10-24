# Operational native-value limit for each subset request, shared by weather
# reads, CF bounds and batch scheduling. This is a tested conservative default,
# not a NetCDF limit or a universal optimum; retune it with transport benchmarks.
DATASET_REQUEST_MAX_VALUES <- 8192L

# Return the multi-site schema even when a file has no selected native times.
# The provenance tables remain present so callers can inspect an empty read.
# dataset__empty_regions {{{
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
# }}}

# Validate one site table before opening any NetCDF variable. The site ID is
# retained in both values and source-cell provenance to keep consumers apart.
# dataset__region_sites {{{
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
# }}}

# Partition source cells into at most 2-by-2 native rectangles. Starting at
# the lowest unassigned latitude/longitude keeps distant sites in separate
# requests while allowing adjacent interpolation cells to share one read.
# dataset__region_cell_groups {{{
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
# }}}

# Partition contiguous native positions into bounded runs. Spatial reads use
# a time limit based on their spatial area; interval bounds account for both
# endpoints. Callers derive their time limit from DATASET_REQUEST_MAX_VALUES.
# dataset__region_runs {{{
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
# }}}

# Read one variable from one file in bounded native-time and spatial slices.
# The returned table is deliberately capped; longer acquisitions must be
# scheduled as separate time windows by the batch execution stage.
# dataset__read_regions_one {{{
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
# }}}

# Assemble file-variable pieces without losing native CF or point provenance.
# Missing variables follow the single-site reader's warning contract.
# dataset__read_regions {{{
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
# }}}

# Match raw ISO boundaries as native date tuples. A 360-day February 30 must
# never be normalized into Gregorian March; existing POSIX inputs retain the
# shared range selector's behavior. Nonexistent endpoint dates can still bound
# a window (for example December 31 around a 360-day model year).
dataset__neighborhood_time_indices <- function(time, coordinates, range) {
    if (!is.character(range)) {
        return(cf_time__range_indices(time, coordinates, range))
    }
    checkmate::assert_character(range, len = 2L, any.missing = FALSE)
    fields <- utils::strcapture(
        "^([0-9]{4})-([0-9]{2})-([0-9]{2})(?:[T ]([0-9]{2}):([0-9]{2})(?::([0-9]{2}(?:[.][0-9]+)?))?)?Z?$",
        range,
        proto = list(
            year = integer(),
            month = integer(),
            day = integer(),
            hour = integer(),
            minute = integer(),
            second = numeric()
        ),
        perl = TRUE
    )
    if (
        anyNA(fields[, c("year", "month", "day")]) ||
            any(
                fields$month < 1L |
                    fields$month > 12L |
                    fields$day < 1L |
                    fields$day > 31L
            )
    ) {
        stop(
            "Raw neighborhood windows require ISO date or UTC datetime endpoints.",
            call. = FALSE
        )
    }
    for (column in c("hour", "minute", "second")) {
        fields[[column]][is.na(fields[[column]])] <- 0
    }
    if (any(fields$hour > 23L | fields$minute > 59L | fields$second >= 60)) {
        stop("Invalid raw neighborhood clock endpoint.", call. = FALSE)
    }
    boundary <- ((as.double(fields$year) * 13 + fields$month) *
        32 +
        fields$day) +
        (fields$hour * 3600 + fields$minute * 60 + fields$second) / 86400
    if (boundary[[2L]] < boundary[[1L]]) {
        stop("Raw time end precedes start.", call. = FALSE)
    }
    key <- ((as.double(coordinates$cf_year) * 13 + coordinates$cf_month) *
        32 +
        coordinates$cf_day) +
        coordinates$cf_second_of_day / 86400
    which(!is.na(time) & key >= boundary[[1L]] & key <= boundary[[2L]])
}

# Read only requested or previously required native bounds, normalize their
# dimension order, and reject interval contradictions before weather I/O.
dataset__neighborhood_bounds <- function(
    dataset,
    index,
    variable,
    axis,
    selected,
    expected
) {
    prior <- if (is.null(expected)) {
        NULL
    } else {
        expected[expected$file_index == index & expected$variable == variable]
    }
    indices <- sort(unique(c(selected, prior$time_index)))
    bounds <- if (!is.null(axis$bounds)) {
        axis$bounds
    } else {
        dataset__time_bounds(
            dataset,
            index,
            axis$units,
            axis$calendar,
            axis$length,
            indices
        )
    }
    values <- data.table::data.table(
        file_index = rep.int(index, length(indices)),
        variable = rep.int(variable, length(indices)),
        time_index = as.integer(indices),
        present = rep.int(!is.null(bounds), length(indices)),
        bound_start_value = rep.int(NA_real_, length(indices)),
        bound_end_value = rep.int(NA_real_, length(indices))
    )
    if (!is.null(bounds)) {
        private <- dataset__private(dataset)
        meta <- private$get_var_dim_meta(bounds$name, index)
        native <- if (match("time", meta$names) == 1L) {
            bounds$raw_values
        } else {
            t(bounds$raw_values)
        }
        if (!is.null(axis$bounds)) {
            native <- native[indices, , drop = FALSE]
        }
        for (attribute in c("units", "calendar")) {
            declared <- tryCatch(
                dataset$att_get(bounds$name, attribute, index),
                error = function(e) NULL
            )
            if (is.null(declared)) {
                next
            }
            actual <- if (attribute == "calendar") {
                normalize_cf_calendar(declared)
            } else {
                as.character(declared)
            }
            required <- if (attribute == "calendar") {
                axis$calendar
            } else {
                as.character(axis$units)
            }
            if (!identical(actual, required)) {
                stop(
                    "Replica CF time bounds units or calendar contradict the time coordinate.",
                    call. = FALSE
                )
            }
        }
        data.table::set(
            values,
            j = "bound_start_value",
            value = as.double(native[, 1L])
        )
        data.table::set(
            values,
            j = "bound_end_value",
            value = as.double(native[, 2L])
        )
        # The normal clock writer can reuse these exact values without fetching
        # each weather chunk's interval metadata for a second time.
        private$metadata_cache[[sprintf("time_bounds_%d", index)]] <- list(
            indices = indices,
            bounds = bounds
        )
    }
    if (!is.null(prior) && nrow(prior)) {
        positions <- match(prior$time_index, values$time_index)
        columns <- c("present", "bound_start_value", "bound_end_value")
        same <- !anyNA(positions) &&
            isTRUE(all.equal(
                as.data.frame(values[positions, columns, with = FALSE]),
                as.data.frame(prior[, columns, with = FALSE]),
                tolerance = 0,
                check.attributes = FALSE
            ))
        if (!same) {
            stop(
                "Replica CF time bounds identity does not match saved intervals.",
                call. = FALSE
            )
        }
    }
    values
}

# Collect descriptors with geometric capacity, avoiding a full list copy for
# every streamed block when the eventual number of sparse runs is unknown.
dataset__block_buffer <- function(capacity = 16L) {
    values <- vector("list", capacity)
    size <- 0L
    list(
        add = function(value) {
            size <<- size + 1L
            if (size > length(values)) {
                length(values) <<- max(size, 2L * length(values))
            }
            values[[size]] <<- value
            invisible(NULL)
        },
        values = function() values[seq_len(size)]
    )
}

# Index completed native intervals by file, variable and grid point. Each point
# owns a geometrically reserved buffer; a new city never copies other points'
# accumulated descriptors. Spatial expansion is bounded to each small raw cell.
dataset__coverage_index <- function(
    completed = NULL,
    variable = NULL,
    index = 1L
) {
    points <- new.env(hash = TRUE, parent = emptyenv())
    key <- function(index, variable, lat, lon) {
        paste(index, variable, lat, lon, sep = "/")
    }
    add <- function(rows) {
        if (is.null(rows) || !nrow(rows)) {
            return(invisible(NULL))
        }
        for (i in seq_len(nrow(rows))) {
            file <- if ("file_index" %in% names(rows)) {
                rows$file_index[[i]]
            } else {
                index
            }
            field <- if ("variable" %in% names(rows)) {
                rows$variable[[i]]
            } else {
                variable
            }
            interval <- c(
                rows$time_start_index[[i]],
                rows$time_start_index[[i]] + rows$time_count[[i]]
            )
            for (lat in seq.int(
                rows$ind_lat[[i]],
                length.out = rows$lat_count[[i]]
            )) {
                for (lon in seq.int(
                    rows$ind_lon[[i]],
                    length.out = rows$lon_count[[i]]
                )) {
                    id <- key(file, field, lat, lon)
                    if (!exists(id, envir = points, inherits = FALSE)) {
                        points[[id]] <- dataset__block_buffer(4L)
                    }
                    points[[id]]$add(interval)
                }
            }
        }
        invisible(NULL)
    }
    add(completed)
    structure(
        list(
            add = add,
            overlaps = function(index, variable, group) {
                any(vapply(
                    seq.int(group$lat, length.out = group$lat_count),
                    function(lat) {
                        any(vapply(
                            seq.int(group$lon, length.out = group$lon_count),
                            function(lon) {
                                exists(
                                    key(index, variable, lat, lon),
                                    envir = points,
                                    inherits = FALSE
                                )
                            },
                            logical(1L)
                        ))
                    },
                    logical(1L)
                ))
            },
            missing = function(index, variable, lat, lon, selected) {
                id <- key(index, variable, lat, lon)
                if (!exists(id, envir = points, inherits = FALSE)) {
                    return(selected)
                }
                intervals <- points[[id]]$values()
                for (interval in intervals) {
                    selected <- selected[
                        selected < interval[[1L]] | selected >= interval[[2L]]
                    ]
                }
                selected
            }
        ),
        class = "esg_native_coverage"
    )
}

# Preserve enclosing native corners independently of later spatial algorithms.
# Each callback receives a bounded value block plus its CF and grid provenance.
dataset__read_neighborhood <- function(
    dataset,
    variable,
    sites,
    time,
    on_chunk,
    completed = NULL,
    expected_contract = NULL,
    expected_bounds = NULL,
    max_values = DATASET_REQUEST_MAX_VALUES,
    on_metadata = NULL
) {
    checkmate::assert_int(max_values, lower = 4L)
    checkmate::assert_character(
        variable,
        min.len = 1L,
        any.missing = FALSE,
        unique = TRUE
    )
    checkmate::assert_data_frame(sites)
    checkmate::assert_function(on_chunk, null.ok = TRUE)
    checkmate::assert_function(on_metadata, null.ok = TRUE)
    sites <- data.table::as.data.table(data.table::copy(sites))
    data.table::set(sites, j = "method", value = "mean")
    sites <- dataset__region_sites(sites)
    private <- dataset__private(dataset)
    private$check_open()
    windows <- if (is.list(time) && !inherits(time, "POSIXt")) {
        time
    } else {
        list(time)
    }
    windows <- lapply(windows, function(window) {
        if (is.character(window) && length(window) == 2L) {
            window
        } else {
            private$normalize_region_time(window)
        }
    })
    coverage <- if (inherits(completed, "esg_native_coverage")) {
        completed
    } else {
        dataset__coverage_index(completed, variable[[1L]])
    }
    parts <- dataset__block_buffer()
    memberships <- vector("list", dataset$file_count * length(variable))
    membership_count <- 0L
    clocks <- dataset__block_buffer()
    slices <- dataset__block_buffer()
    bounds_records <- vector("list", dataset$file_count * length(variable))
    total <- 0L
    for (index in seq_len(dataset$file_count)) {
        grid <- dataset$get_spatial_grid(index)
        if (
            length(dim(grid$lat)) > 1L ||
                length(dim(grid$lon)) > 1L ||
                is.null(grid$lat) ||
                is.null(grid$lon)
        ) {
            stop(
                "Raw neighborhoods require rectilinear lat/lon coordinates.",
                call. = FALSE
            )
        }
        axis <- dataset__time_axis(dataset, index)
        selected <- sort(unique(unlist(
            lapply(windows, function(window) {
                dataset__neighborhood_time_indices(
                    axis$values,
                    axis$coordinates,
                    window
                )
            }),
            use.names = FALSE
        )))
        for (name in variable) {
            if (!name %in% dataset$get_variables(index)) {
                stop(
                    sprintf(
                        "Variable '%s' is absent from file %d.",
                        name,
                        index
                    ),
                    call. = FALSE
                )
            }
            meta <- private$get_var_dim_meta(name, index)
            positions <- match(c("time", "lat", "lon"), meta$names)
            if (anyNA(positions) || any(meta$lengths[-positions] != 1L)) {
                stop(
                    "Raw neighborhoods require time/lat/lon and only singleton extra dimensions.",
                    call. = FALSE
                )
            }
            source <- data.table::rbindlist(lapply(
                seq_len(nrow(sites)),
                function(i) {
                    corners <- private$region_grid_sources(
                        "mean",
                        grid$lat,
                        grid$lon,
                        sites$lat[[i]],
                        private$normalize_lon_for_grid(sites$lon[[i]], grid$lon)
                    )
                    data.table::set(
                        corners,
                        j = "site_id",
                        value = sites$site_id[[i]]
                    )
                    data.table::set(
                        corners,
                        j = "target_lon",
                        value = sites$lon[[i]]
                    )
                    data.table::set(
                        corners,
                        j = "target_lat",
                        value = sites$lat[[i]]
                    )
                    corners
                }
            ))
            data.table::set(source, j = "weight", value = NULL)
            data.table::set(source, j = "file_index", value = index)
            data.table::set(source, j = "variable", value = name)
            points <- unique(source[,
                c("ind_lat", "ind_lon", "grid_lat", "grid_lon"),
                with = FALSE
            ])
            groups <- dataset__region_cell_groups(points)
            # Both sparse overlaps and resumed coverage split into the same
            # one-cell groups, preserving point order and integer dimensions.
            point_group <- function(i) {
                list(
                    members = i,
                    lat = points$ind_lat[[i]],
                    lon = points$ind_lon[[i]],
                    lat_count = 1L,
                    lon_count = 1L
                )
            }
            # A sparse L-shaped overlap must not add the missing fourth cell.
            groups <- unlist(
                lapply(groups, function(group) {
                    if (
                        length(group$members) ==
                            group$lat_count * group$lon_count
                    ) {
                        return(list(group))
                    }
                    lapply(group$members, point_group)
                }),
                recursive = FALSE
            )
            membership_count <- membership_count + 1L
            memberships[[membership_count]] <- source
            units <- dataset$att_get(name, "units", index)
            # Preserve temporal interpretation before any downstream weather
            # method chooses between point values and interval means.
            cell_methods <- tryCatch(
                as.character(dataset$att_get(name, "cell_methods", index)),
                error = function(e) NA_character_
            )
            standard_name <- tryCatch(
                as.character(dataset$att_get(name, "standard_name", index)),
                error = function(e) NA_character_
            )

            # Resume coverage is native-index based only after its complete
            # grid and CF-axis identity has been checked on this endpoint.
            contract <- checksum_bytes(
                serialize(
                    list(
                        variable = name,
                        units = as.character(units),
                        cell_methods = cell_methods,
                        standard_name = standard_name,
                        lat = as.double(grid$lat),
                        lon = as.double(grid$lon),
                        time = as.double(axis$raw_values),
                        time_units = axis$units,
                        calendar = axis$calendar,
                        time_bounds_present = !is.null(tryCatch(
                            dataset$att_get("time", "bounds", index),
                            error = function(e) NULL
                        )),
                        dimensions = meta$names,
                        lengths = as.integer(meta$lengths)
                    ),
                    NULL,
                    version = 3L
                ),
                "sha256"
            )
            if (
                !is.null(expected_contract) &&
                    !identical(contract, expected_contract)
            ) {
                stop(
                    "Replica grid, variable or CF time identity does not match saved blocks.",
                    call. = FALSE
                )
            }
            native_bounds <- dataset__neighborhood_bounds(
                dataset,
                index,
                name,
                axis,
                selected,
                expected_bounds
            )
            bounds_records[[membership_count]] <- native_bounds
            # Publish checked identity and exact selection before weather I/O.
            # A later transport failure must not erase the native denominator.
            if (!is.null(on_metadata)) {
                on_metadata(list(
                    contract = contract,
                    neighborhoods = source,
                    bounds = native_bounds
                ))
            }
            first_block <- TRUE
            # Split only cells whose points already have native coverage.
            groups <- unlist(
                lapply(groups, function(group) {
                    if (!coverage$overlaps(index, name, group)) {
                        return(list(group))
                    }
                    lapply(group$members, point_group)
                }),
                recursive = FALSE
            )
            for (group in groups) {
                needed <- coverage$missing(
                    index,
                    name,
                    group$lat,
                    group$lon,
                    selected
                )
                runs <- dataset__region_runs(
                    needed,
                    max_values %/% (group$lat_count * group$lon_count)
                )
                for (run in runs) {
                    start <- rep.int(1L, length(meta$names))
                    count <- rep.int(1L, length(meta$names))
                    start[positions] <- c(run[[1L]], group$lat, group$lon)
                    count[positions] <- c(
                        length(run),
                        group$lat_count,
                        group$lon_count
                    )
                    began <- proc.time()[["elapsed"]]
                    raw <- dataset$var_get(
                        name,
                        start = start,
                        count = count,
                        index = index,
                        collapse = FALSE
                    )
                    elapsed <- proc.time()[["elapsed"]] - began
                    if (length(raw) != prod(count)) {
                        stop(
                            "Incomplete native neighborhood block.",
                            call. = FALSE
                        )
                    }
                    raw <- aperm(
                        raw,
                        c(positions, setdiff(seq_along(count), positions))
                    )
                    values <- data.table::CJ(
                        time_index = as.integer(run),
                        ind_lat = seq.int(
                            group$lat,
                            length.out = group$lat_count
                        ),
                        ind_lon = seq.int(
                            group$lon,
                            length.out = group$lon_count
                        ),
                        sorted = FALSE
                    )
                    # NetCDF's first dimension varies fastest; construct the
                    # same ordering explicitly before attaching the payload.
                    data.table::setorderv(
                        values,
                        c("ind_lon", "ind_lat", "time_index")
                    )
                    data.table::set(values, j = "value", value = as.double(raw))
                    data.table::set(
                        values,
                        j = "grid_lat",
                        value = grid$lat[values$ind_lat]
                    )
                    data.table::set(
                        values,
                        j = "grid_lon",
                        value = grid$lon[values$ind_lon]
                    )
                    data.table::set(values, j = "variable", value = name)
                    data.table::set(values, j = "file_index", value = index)
                    data.table::set(
                        values,
                        j = "units",
                        value = as.character(units)
                    )
                    data.table::set(
                        values,
                        j = "cell_methods",
                        value = cell_methods
                    )
                    data.table::set(
                        values,
                        j = "standard_name",
                        value = standard_name
                    )
                    clock <- data.table::as.data.table(axis$coordinates[
                        run,
                        ,
                        drop = FALSE
                    ])
                    data.table::set(
                        clock,
                        j = "time_index",
                        value = as.integer(run)
                    )
                    data.table::set(clock, j = "file_index", value = index)
                    data.table::set(clock, j = "time", value = axis$values[run])
                    data.table::set(
                        clock,
                        j = "time_value",
                        value = axis$raw_values[run]
                    )
                    data.table::set(clock, j = "time_units", value = axis$units)
                    bounds <- dataset__selected_bounds(
                        dataset,
                        index,
                        axis,
                        run
                    )
                    if (!is.null(bounds)) {
                        data.table::set(
                            clock,
                            j = "time_bound_start",
                            value = bounds$start
                        )
                        data.table::set(
                            clock,
                            j = "time_bound_end",
                            value = bounds$end
                        )
                        # The shared selected-bounds accessor intentionally
                        # returns only parsed endpoints. Recover native values
                        # from its verified cache, without a second DAP read.
                        cached_bounds <- private$metadata_cache[[sprintf(
                            "time_bounds_%d",
                            index
                        )]]
                        raw_bounds <- if (!is.null(axis$bounds)) {
                            axis$bounds
                        } else {
                            cached_bounds$bounds
                        }
                        bound_meta <- private$get_var_dim_meta(
                            raw_bounds$name,
                            index
                        )
                        native <- if (match("time", bound_meta$names) == 1L) {
                            raw_bounds$raw_values
                        } else {
                            t(raw_bounds$raw_values)
                        }
                        bound_positions <- if (!is.null(axis$bounds)) {
                            run
                        } else {
                            match(run, cached_bounds$indices)
                        }
                        native <- native[bound_positions, , drop = FALSE]
                        data.table::set(
                            clock,
                            j = "bound_start_value",
                            value = native[, 1L]
                        )
                        data.table::set(
                            clock,
                            j = "bound_end_value",
                            value = native[, 2L]
                        )
                    }
                    slice <- data.table::data.table(
                        file_index = index,
                        variable = name,
                        ind_lat = group$lat,
                        ind_lon = group$lon,
                        lat_count = group$lat_count,
                        lon_count = group$lon_count,
                        time_start_index = as.integer(run[[1L]]),
                        time_count = length(run),
                        elapsed_seconds = elapsed,
                        native_values = length(raw)
                    )
                    chunk <- list(
                        contract = contract,
                        bounds = if (first_block) {
                            native_bounds
                        } else {
                            native_bounds[native_bounds$time_index %in% run]
                        },
                        values = values,
                        neighborhoods = source,
                        time = clock,
                        read_slices = slice
                    )
                    if (is.null(on_chunk)) {
                        total <- total + nrow(values)
                        if (total > 250000L) {
                            stop(
                                "Use on_chunk for neighborhoods exceeding 250000 values.",
                                call. = FALSE
                            )
                        }
                        parts$add(values)
                    } else {
                        on_chunk(chunk)
                    }
                    # The callback has saved this block before coverage advances.
                    coverage$add(slice)
                    first_block <- FALSE
                    clocks$add(clock)
                    slices$add(slice)
                }
            }
        }
    }
    list(
        values = data.table::rbindlist(parts$values(), fill = TRUE),
        neighborhoods = unique(data.table::rbindlist(memberships, fill = TRUE)),
        time = unique(data.table::rbindlist(clocks$values(), fill = TRUE)),
        read_slices = data.table::rbindlist(slices$values(), fill = TRUE),
        bounds = unique(data.table::rbindlist(bounds_records, fill = TRUE))
    )
}

# vim: fdm=marker :
