# extract_location_dict {{{
extract_location_dict <- function (pattern) {
    checkmate::assert_string(pattern)

    pattern <- gsub("\\s+", ".", pattern)
    d <- data.table::as.data.table(utils::getFromNamespace("WEATHER_DB", "eplusr"))
    res <- d[grepl(pattern, title, ignore.case = TRUE)]

    mes_location <- function (index = NULL, title, country, state_province, location, wmo_number, source_type, longitude, latitude) {
        if (!is.null(index)) {
            h <- paste0("[", index, "] ", title, " -----")
        } else {
            h <- paste0(title, " -----")
        }
        country <- if (is.na(country)) NULL else paste0(" * Country: ", country)
        state_province <- if (is.na(state_province)) NULL else paste0(" * State or Province: ", state_province)
        location <- if (is.na(location)) NULL else paste0(" * Location: ", location)
        wmo_number <- if (is.na(wmo_number)) NULL else paste0(" * WMO number: ", wmo_number)
        source_type <- if (is.na(source_type)) NULL else paste0(" * Source type: ", source_type)
        longitude <- paste0(" * Longitude: ", longitude)
        latitude <- paste0(" * Latitude: ", latitude)
        paste(h, country, state_province, location, wmo_number, source_type, longitude, latitude,
            sep = "\n"
        )
    }

    res[, index := .I]

    if (!nrow(res)) {
        cat("No matched result found.\n")
        return(invisible(NULL))
    }

    if (nrow(res) > 1L) {
        m <- res[, mes_location(index, title, country, state_province, location, wmo_number, source_type, longitude, latitude), by = index]$V1
        h <- paste0("< ", nrow(res), " MATCHED RESULTS FOUND >\nPlease select which one to use as the base location:")
        ch <- res$title
        r <- utils::menu(ch, title = paste0(h, "\n\n", paste(m, collapse = "\n\n")))
        if (r == 0) return(invisible(NULL))
        res <- res[index == r]
    } else {
        m <- res[, mes_location(NULL, title, country, state_province, location, wmo_number, source_type, longitude, latitude), by = index]$V1
        h <- paste0("< 1 MATCHED RESULT FOUND >\nPlease confirm to use it as the base location:")
        r <- utils::menu(c("Yes", "No"), title = paste0(h, "\n\n", paste(m, collapse = "\n\n")))
        if (r != 1) return(invisible(NULL))
    }

    res
}
# }}}

# match_location_coord {{{
match_location_coord <- function (path, dict, threshold = list(lon = 1.0, lat = 1.0), max_num = NULL) {
    checkmate::assert_list(dict)
    checkmate::assert_names(names(dict), must.include = c("longitude", "latitude"))
    checkmate::assert_number(dict$longitude, lower = -180.0, upper = 180.0)
    checkmate::assert_number(dict$latitude, lower = -90.0, upper = 90.0)

    match_nc_coord(path, dict$latitude, dict$longitude, threshold, max_num)
}
# }}}

# match_coord {{{
#' Match coordinates of input EPW in the CMIP6 output file database
#'
#' `match_coord()` takes an EPW and uses its longitude and latitude to calculate
#' the distance between the EPW location and the global grid points in NetCDF
#' files.
#'
#' `match_coord()` uses [future.apply][future.apply::future_lapply()]
#' underneath. You can use your preferable future backend to
#' speed up data extraction in parallel. By default, `match_coord()` uses
#' `future::sequential` backend, which runs things in sequential.
#'
#' @section Geographical distance calculation:
#'
#' `match_coord()` calculates the geographical distances based formulas of
#' spherical trigonometry:
#'
#' \deqn{
#' \Delta{X}=\cos(\phi_2)\cos(\lambda_2) - \cos(\phi_1)\cos(\lambda_1)
#' }
#'
#' \deqn{
#' \Delta{Y}=\cos(\phi_2)\sin(\lambda_2) - \cos(\phi_1)\sin(\lambda_1)
#' }
#'
#' \deqn{
#' \Delta{Z}=\sin(\phi_2) - \sin(\phi_1)
#' }
#'
#' \deqn{
#' C_h=\sqrt{(\Delta{X})^2 + (\Delta{Y})^2 + (\Delta{Z})^2}
#' }
#'
#' where \eqn{phi} is the latitude and \eqn{lambda} is the longitude.  This
#' formula treats the Earth as a sphere. The geographical distance between
#' points on the surface of a spherical Earth is \eqn{D = RC_h}.
#'
#' For more details, please see this [Wikipedia](https://en.wikipedia.org/wiki/Geographical_distance#Tunnel_distance)
#'
#' @param epw Possible values:
#'
#' * A file path of EPW file
#' * An [eplusr::Epw] object
#' * A regular expression used to search locations in EnergyPlus Weather
#'   Database, e.g. "los angeles.*tmy3". You will be asked to select a matched
#'   EPW to download and read. It will be saved into [tempdir()]. Note that the
#'   search is case-insensitive
#'
#' @param threshold A list of 2 elements `lon` and `lat` specifying the
#'        absolute distance threshold used for subsetting longitude and
#'        latitude value for calculating distances. If `NULL`, no subsetting is
#'        performed and the distances between the target location and all grid
#'        points are calculated. This is useful set the `threshold` value to
#'        exclude some points that are absolute too far away from the target
#'        location. Default: `list(lon = 1.0, lat = 1.0)`
#'
#' @param max_num The maximum number of grid points to be matched. Default is
#'        `NULL`, which means no number limit and the total matched grid points
#'        are determined by the `threshold` input.
#'
#' @return An `epw_cmip6_coord` object, which is basically a list of 3 elements:
#'
#' * `epw`: An [eplusr::Epw] object parsed from input `epw` argument
#' * `meta`: A list containing basic meta data of input EPW, including `city`,
#'   `state_province`, `country`, `latitute` and `longitude`.
#' * `coord`: A [data.table::data.table()] which is basically CMIP6 index
#'            database with an appending new list column `coord` that contains
#'            matched latitudes and longitudes in each NetCDF file. Each element
#'            in `coord` is a [data.table::data.table()] of 6 columns describing
#'            the matched coordinates.
#'
#'     * `index`: the indices of matched coordinates
#'     * `ind_lon`, `ind_lat`: The value indices of longitude or latitude in the
#'       NetCDF coordinate grids. These values are used to extract the
#'       corresponding variable values
#'     * `lon`, `lat`: the actual longitude or latitude in the NetCDF coordinate
#'       grids
#'     * `dist`: the distance in km between the coordinate values in NetCDF and
#'       input EPW
#'
#' @examples
#' \dontrun{
#' # download an EPW from EnergyPlus website
#' epw <- eplusr::download_weather("los angeles.*TMY3", dir = tempdir(),
#'     type = "EPW", ask = FALSE)
#'
#' match_coord(epw, threshold = list(lon = 1.0, lat = 1.0))
#' }
#'
#' @export
match_coord <- function (epw, threshold = list(lon = 1.0, lat = 1.0), max_num = NULL) {
    # load file index
    index <- load_cmip6_index()

    # remove empty
    if (!"file_path" %in% names(index)) {
        stop("No NetCDF database has been identified. Please run 'summary_database()' first.", call. = FALSE)
    }

    index <- index[!J(NA_character_), on = "file_path"]

    if (checkmate::test_r6(epw, "Epw")) {
        epw <- epw
        dict <- epw$location()
    } else {
        checkmate::assert_scalar(epw)
        if (tolower(tools::file_ext(epw)) == "epw") {
            epw <- eplusr::read_epw(epw)
            dict <- epw$location()
        } else {
            dict <- extract_location_dict(epw)
            if (is.null(dict)) return(invisible())
            epw <- eplusr::read_epw(dict$epw_url)
            epw$save(file.path(tempdir(), basename(dict$epw_url)), overwrite = TRUE)
        }
    }

    if (is.data.frame(dict)) {
        meta <- as.list(dict[, list(city = location, state_province, country,
            latitude, longitude)])
    } else {
        meta <- dict[c("city", "state_province", "country", "latitude", "longitude")]
    }

    message("Start to match coordinates...")
    progressr::with_progress({
        p <- progressr::progressor(nrow(index))
        i <- 0L

        coords <- future.apply::future_lapply(index$file_path, function (f) {
            i <<- i + 1L
            p(message = sprintf("[%i/%i]", i, nrow(index)))
            match_location_coord(f, meta, threshold, max_num)
        })
    })

    data.table::set(index, NULL, "coord", coords)

    structure(list(epw = epw, meta = meta, coord = index), class = "epw_cmip6_coord")
}
# }}}

# ref: https://en.wikipedia.org/wiki/Geographical_distance#Tunnel_distance
tunnel_dist <- function(lat1, lon1, lat2, lon2) {
    EARTH_RADIUS <- 6371.009

    lat1 <- to_radian(lat1)
    lon1 <- to_radian(lon1)

    lat2 <- to_radian(lat2)
    lon2 <- to_radian(lon2)

    delta_x <- cos(lat2) * cos(lon2) - cos(lat1) * cos(lon1)
    delta_y <- cos(lat2) * sin(lon2) - cos(lat1) * sin(lon1)
    delta_z <- sin(lat2) - sin(lat1)

    sqrt(delta_x ^ 2 + delta_y ^ 2 + delta_z ^ 2) * EARTH_RADIUS
}

# match_nc_coord {{{
match_nc_coord <- function (x, lat, lon, threshold = NULL, max_num = NULL) {
    checkmate::assert_number(lat, lower = -90.0, upper = 90.0)
    checkmate::assert_number(lon, lower = -180.0, upper = 180.0)

    checkmate::assert_list(threshold, null.ok = TRUE)
    if (!is.null(threshold)) {
        checkmate::assert_names(names(threshold), must.include = c("lon", "lat"))
        checkmate::assert_number(threshold$lon, lower = 0.0, upper = 180.0)
        checkmate::assert_number(threshold$lat, lower = 0.0, upper = 90.0)
    }

    checkmate::assert_count(max_num, positive = TRUE, null.ok = TRUE)

    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get latitude and longitude
    dim <- lapply(c("lat", "lon"), function (var) as.numeric(RNetCDF::var.get.nc(nc, var)))
    names(dim) <- c("lat", "lon")

    # use all grid points for distance calculation
    if (is.null(threshold)) {
        ind_lat <- seq.int(length(dim$lat))
        ind_lon <- seq.int(length(dim$lon))
    # to speed things up, use the input threshold to get rid of grid points that
    # are too far away
    } else {
        # change EPW longitude to [0, 360] range
        if (min(dim$lon) >= 0 && lon < 0) {
            lon <- 180 -lon
        }

        dis_lat <- abs(dim$lat - lat)
        dis_lon <- abs(dim$lon - lon)

        ind_lat <- which(dis_lat <= threshold$lat)
        ind_lon <- which(dis_lon <= threshold$lon)

        if (!length(ind_lat)) {
            stop("Threshold for latitude ('", threshold$lat, "') is smaller than ",
                 "the minimum distance ('", round(min(dis_lat), 2), "') of current file resolution.")
        }
        if (!length(ind_lon)) {
            stop("Threshold for latitude ('", threshold$lon, "') is smaller than ",
                 "the minimum distance ('", round(min(dis_lon), 2), "') of current file resolution.")
        }
    }

    # get the tunnel distances
    coord <- data.table::CJ(ind_lat = ind_lat, ind_lon = ind_lon)
    data.table::set(coord, NULL, "dist",
        tunnel_dist(dim$lat[coord$ind_lat], dim$lon[coord$ind_lon], lat, lon)
    )
    data.table::setorderv(coord, "dist")

    if (!is.null(max_num)) coord <- coord[seq.int(as.integer(max_num))]

    data.table::set(coord, NULL, "index", seq.int(nrow(coord)))
    data.table::set(coord, NULL, "lat", dim$lat[coord$ind_lat])
    data.table::set(coord, NULL, "lon", dim$lon[coord$ind_lon])
    data.table::setcolorder(coord, c("index", "ind_lon", "ind_lat", "lon", "lat"))

    coord
}
# }}}
