# Shared rectilinear source selection; weights operate on native fields before
# nonlinear derived weather or typical-month selection. No height or land-mask
# correction is implied. GCM tie order and missing-coordinate behavior are kept.

# Converts any longitude convention into a cyclic 0-360 axis for cell math.
spatial__lon_to_360 <- function(lon) {
    (lon + 360) %% 360
}

# Build grid coordinates once per file; site distances are computed only
# when selecting the few source cells required by a particular site.
spatial__make_region_grid_coords <- function(grid_lat, grid_lon) {
    coords <- data.table::CJ(
        ind_lat = seq_along(grid_lat),
        ind_lon = seq_along(grid_lon)
    )
    data.table::set(
        coords,
        j = "grid_lat",
        value = grid_lat[coords$ind_lat]
    )
    data.table::set(
        coords,
        j = "grid_lon",
        value = grid_lon[coords$ind_lon]
    )
    coords
}

# Select up to four nearest cells without copying and sorting the full
# grid for every site. which.min() keeps the original grid tie order.
spatial__select_nearest_grid_sources <- function(
    coords,
    method,
    lat,
    target_lon
) {
    count <- if (identical(method, "nearest")) 1L else 4L
    distances <- tunnel_dist(
        coords$grid_lat,
        coords$grid_lon,
        lat,
        target_lon
    )
    count <- min(count, sum(is.finite(distances)))
    if (!count) {
        stop(
            "The spatial grid has no finite source coordinates.",
            call. = FALSE
        )
    }
    selected <- integer(count)
    for (index in seq_len(count)) {
        selected[[index]] <- which.min(distances)
        distances[[selected[[index]]]] <- Inf
    }
    sources <- data.table::copy(coords[selected])
    data.table::set(
        sources,
        j = "grid_dist_km",
        value = tunnel_dist(
            sources$grid_lat,
            sources$grid_lon,
            lat,
            target_lon
        )
    )
    data.table::set(sources, j = "source_index", value = seq_len(count))

    if (identical(method, "nearest")) {
        data.table::set(sources, j = "role", value = "nearest")
        data.table::set(sources, j = "weight", value = 1)
        return(sources)
    }

    eps <- sqrt(.Machine$double.eps)
    exact <- which(sources$grid_dist_km <= eps)
    if (length(exact)) {
        weights <- rep(0, nrow(sources))
        weights[[exact[[1L]]]] <- 1
    } else {
        raw <- 1 / (sources$grid_dist_km^2)
        weights <- raw / sum(raw)
    }
    data.table::set(
        sources,
        j = "role",
        value = sprintf("nearest_%d", sources$source_index)
    )
    data.table::set(sources, j = "weight", value = weights)
    sources
}

# Finds the south/north coordinates that enclose a target latitude.
spatial__find_lat_bounds <- function(grid_lat, lat) {
    vals <- sort(unique(as.numeric(grid_lat[!is.na(grid_lat)])))
    if (length(vals) < 2L) {
        stop(
            "Bilinear and mean grid methods require at least two latitude coordinates.",
            call. = FALSE
        )
    }
    if (lat < vals[[1L]] || lat > vals[[length(vals)]]) {
        stop(
            sprintf(
                "Target latitude %.6f is outside the grid latitude range [%.6f, %.6f].",
                lat,
                vals[[1L]],
                vals[[length(vals)]]
            ),
            call. = FALSE
        )
    }
    if (lat <= vals[[1L]]) {
        return(c(south = vals[[1L]], north = vals[[2L]]))
    }
    if (lat >= vals[[length(vals)]]) {
        return(c(
            south = vals[[length(vals) - 1L]],
            north = vals[[length(vals)]]
        ))
    }

    south <- max(vals[vals <= lat])
    north_candidates <- vals[vals > south]
    if (!length(north_candidates)) {
        stop(
            "Cannot identify a northern latitude bound for the requested point.",
            call. = FALSE
        )
    }
    c(south = south, north = min(north_candidates))
}

# Finds west/east longitudes on a cyclic axis, including dateline wrap.
spatial__find_lon_bounds <- function(grid_lon, target_lon, cyclic = TRUE) {
    if (!cyclic) {
        # A regional file must enclose the target; its two distant edges must
        # never become a cell by wrapping around the unrepresented world.
        offset <- (grid_lon - target_lon + 180) %% 360 - 180
        bounds <- spatial__find_lat_bounds(offset, 0)
        return(list(
            west_lon = grid_lon[match(bounds[["south"]], offset)],
            east_lon = grid_lon[match(bounds[["north"]], offset)],
            west360 = target_lon + bounds[["south"]],
            east360 = target_lon + bounds[["north"]],
            target360 = target_lon
        ))
    }
    lon_tbl <- unique(data.table::data.table(
        grid_lon = as.numeric(grid_lon[!is.na(grid_lon)]),
        lon360 = spatial__lon_to_360(as.numeric(grid_lon[
            !is.na(grid_lon)
        ]))
    ))
    if (nrow(lon_tbl) < 2L) {
        stop(
            "Bilinear and mean grid methods require at least two longitude coordinates.",
            call. = FALSE
        )
    }
    if (any(duplicated(lon_tbl$lon360))) {
        stop(
            "Longitude coordinates are duplicated after converting to a 0-360 grid.",
            call. = FALSE
        )
    }

    vals <- sort(lon_tbl$lon360)
    target360 <- spatial__lon_to_360(target_lon)
    west_candidates <- vals[vals <= target360]
    west <- if (length(west_candidates)) {
        max(west_candidates)
    } else {
        vals[[length(vals)]]
    }
    east_candidates <- vals[vals > west]
    east <- if (length(east_candidates)) {
        min(east_candidates)
    } else {
        vals[[1L]]
    }

    west_row <- lon_tbl[match(west, lon_tbl$lon360)]
    east_row <- lon_tbl[match(east, lon_tbl$lon360)]
    list(
        west_lon = west_row$grid_lon[[1L]],
        east_lon = east_row$grid_lon[[1L]],
        west360 = west,
        east360 = east,
        target360 = target360
    )
}

# Selects the four enclosing cell corners and computes method weights.
spatial__select_cell_grid_sources <- function(
    grid_lat,
    grid_lon,
    lat,
    target_lon,
    method,
    cyclic = TRUE
) {
    lat_bounds <- spatial__find_lat_bounds(grid_lat, lat)
    lon_bounds <- spatial__find_lon_bounds(grid_lon, target_lon, cyclic)

    south_idx <- match(lat_bounds[["south"]], grid_lat)
    north_idx <- match(lat_bounds[["north"]], grid_lat)
    west_idx <- match(lon_bounds$west_lon, grid_lon)
    east_idx <- match(lon_bounds$east_lon, grid_lon)
    if (anyNA(c(south_idx, north_idx, west_idx, east_idx))) {
        stop(
            "Cannot map enclosing grid coordinates back to NetCDF indices.",
            call. = FALSE
        )
    }

    sources <- data.table::data.table(
        ind_lat = c(south_idx, south_idx, north_idx, north_idx),
        ind_lon = c(west_idx, east_idx, west_idx, east_idx),
        source_index = 1:4,
        role = c(
            "lower_left",
            "lower_right",
            "upper_left",
            "upper_right"
        ),
        grid_lon = grid_lon[c(west_idx, east_idx, west_idx, east_idx)],
        grid_lat = grid_lat[c(
            south_idx,
            south_idx,
            north_idx,
            north_idx
        )]
    )
    data.table::set(
        sources,
        j = "grid_dist_km",
        value = tunnel_dist(
            sources$grid_lat,
            sources$grid_lon,
            lat,
            target_lon
        )
    )

    if (identical(method, "mean")) {
        data.table::set(sources, j = "weight", value = 0.25)
        return(sources)
    }

    x1 <- lon_bounds$west360
    x2 <- lon_bounds$east360
    target_x <- lon_bounds$target360
    # Crossing the dateline means the eastern bound is numerically smaller.
    if (x2 <= x1) {
        x2 <- x2 + 360
    }
    if (target_x < x1) {
        target_x <- target_x + 360
    }
    y1 <- lat_bounds[["south"]]
    y2 <- lat_bounds[["north"]]
    if (x2 == x1 || y2 == y1) {
        stop(
            "Cannot compute bilinear weights for a zero-area grid cell.",
            call. = FALSE
        )
    }

    wx_east <- (target_x - x1) / (x2 - x1)
    wx_west <- 1 - wx_east
    wy_north <- (lat - y1) / (y2 - y1)
    wy_south <- 1 - wy_north
    data.table::set(
        sources,
        j = "weight",
        value = c(
            wx_west * wy_south,
            wx_east * wy_south,
            wx_west * wy_north,
            wx_east * wy_north
        )
    )
    sources
}

# Dispatches method-specific source-cell selection for point extraction.
spatial__region_grid_sources <- function(
    method,
    grid_lat,
    grid_lon,
    lat,
    target_lon,
    coords = NULL,
    cyclic = TRUE
) {
    if (method %in% c("nearest", "idw")) {
        if (is.null(coords)) {
            coords <- spatial__make_region_grid_coords(
                grid_lat,
                grid_lon
            )
        }
        return(spatial__select_nearest_grid_sources(
            coords,
            method,
            lat,
            target_lon
        ))
    }
    spatial__select_cell_grid_sources(
        grid_lat,
        grid_lon,
        lat,
        target_lon,
        method,
        cyclic
    )
}
