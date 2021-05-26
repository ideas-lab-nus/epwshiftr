# extract_location_dict {{{
#' @importFrom checkmate assert_string
#' @importFrom utils menu
extract_location_dict <- function (pattern) {
    assert_string(pattern)

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
#' @importFrom checkmate assert_count assert_list assert_names assert_number
match_location_coord <- function (path, dict, threshold = list(lon = 1.0, lat = 1.0), max_num = NULL) {
    assert_list(dict)
    assert_names(names(dict), must.include = c("longitude", "latitude"))
    assert_number(dict$longitude, lower = -180.0, upper = 180.0)
    assert_number(dict$latitude, lower = -90.0, upper = 90.0)

    match_nc_coord(path, dict$latitude, dict$longitude, threshold, max_num)
}
# }}}

# match_coord {{{
#' Match coordinates of input EPW in the CMIP6 output file database
#'
#' `match_coord()` takes an EPW and uses its longitude and latitude to match
#' corresponding values that meet specified threshold in NetCDF files.
#'
#' `match_coord()` uses [future.apply][future.apply::future_lapply()]
#' underneath. You can use your preferable future backend to
#' speed up data extraction in parallel. By default, `match_coord()` uses
#' `future::sequential` backend, which runs things in sequential.
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
#'        absolute distance threshold used when matching longitude and latitude.
#'        Default: `list(lon = 1.0, lat = 1.0)`
#'
#' @param max_num The maximum number to be matched for both longitude and
#'        latitude when `threshold` is matched. Default is `NULL`, which means
#'        no limit
#'
#' @return An `epw_cmip6_coord` object, which is basically a list of 3 elements:
#'
#' * `epw`: An [eplusr::Epw] object parsed from input `epw` argument
#' * `meta`: A list containing basic meta data of input EPW, including `city`,
#'   `state_province`, `country`, `latitute` and `longitude`.
#' * `coord`: A [data.table::data.table()] which is basically CMIP6 index
#'            database with an appending new list column `coord` that contains
#'            matched latitudes and longitudes in each NetCDF file. Each element
#'            in `coord` contains 2 elements `lat` and `lon`, in which contains
#'            the 4 components describing the matched coordinates.
#'     * `index`: the indices of matched coordinates
#'     * `value`: the actual longitude or latitude in the NetCDF coordinate
#'       grids
#'     * `dis`: the distance between the coordinate values in NetCDF and input
#'       EPW
#'     * `which`: The value indices of longitude or latitude in the NetCDF
#'       coordinate grids. These values are used to extract the corresponding
#'       variable values
#'
#' @importFrom progressr with_progress
#' @importFrom checkmate assert_scalar test_file_exists test_r6
#' @importFrom eplusr read_epw
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

    if (test_r6(epw, "Epw")) {
        epw <- epw
        dict <- epw$location()
    } else {
        assert_scalar(epw)
        if (tolower(tools::file_ext(epw)) == "epw") {
            epw <- eplusr::read_epw(epw)
            dict <- epw$location()
        } else {
            dict <- extract_location_dict(epw)
            if (is.null(dict)) return(invisible())
            epw <- eplusr::read_epw(dict$epw_url)
            epw$save(file.path(tempdir(), basename(dict$epw_url)))
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

# match_nc_coord {{{
match_nc_coord <- function (x, lat, lon, threshold = list(lat = 1.0, lon = 1.0), max_num = NULL) {
    assert_number(lat, lower = -90.0, upper = 90.0)
    assert_number(lon, lower = -180.0, upper = 180.0)

    assert_list(threshold)
    assert_names(names(threshold), must.include = c("lon", "lat"))
    assert_number(threshold$lon, lower = 0, upper = 180.0)
    assert_number(threshold$lat, lower = 0, upper = 90.0)

    assert_count(max_num, positive = TRUE, null.ok = TRUE)

    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get latitude and longitude
    dim <- lapply(c("lat", "lon"), function (var) as.numeric(RNetCDF::var.get.nc(nc, var)))
    names(dim) <- c("lat", "lon")

    # calculate distance
    dis_lat <- dim$lat - lat

    # change EPW longitude to [0, 360] range
    if (all(dim$lon >= 0) && lon < 0) {
        lon <- 180 -lon
    }
    dis_lon <- dim$lon - lon

    i_lat <- which(abs(dis_lat) <= threshold$lat)
    i_lon <- which(abs(dis_lon) <= threshold$lon)

    if (!length(i_lat)) {
        stop("Threshold for latitude ('", threshold$lat, "') is smaller than ",
             "the minimum distance ('", round(min(abs(dis_lat)), 2), "') of current file resolution.")
    }
    if (!length(i_lon)) {
        stop("Threshold for latitude ('", threshold$lon, "') is smaller than ",
             "the minimum distance ('", round(min(abs(dis_lon)), 2), "') of current file resolution.")
    }

    if (!is.null(max_num)) {
        if (max_num < length(i_lat)) i_lat <- i_lat[seq.int(as.integer(max_num))]
        if (max_num < length(i_lon)) i_lon <- i_lon[seq.int(as.integer(max_num))]
    }

    list(lat = list(index = seq_along(i_lat), value = dim$lat[i_lat], dis = dis_lat[i_lat], which = i_lat),
         lon = list(index = seq_along(i_lon), value = dim$lon[i_lon], dis = dis_lon[i_lon], which = i_lon)
    )
}
# }}}
