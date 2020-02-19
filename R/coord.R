# extract_location_dict {{{
#' @importFrom checkmate assert_string
#' @importFrom utils menu
extract_location_dict <- function (pattern) {
    assert_string(pattern)

    pattern <- gsub("\\s+", ".", pattern)
    d <- data.table::as.data.table(eplusr:::WEATHER_DB)
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

    assert_list(threshold)
    assert_names(names(threshold), must.include = c("lon", "lat"))
    assert_number(threshold$lon, lower = 0, upper = 180.0)
    assert_number(threshold$lat, lower = 0, upper = 90.0)

    assert_count(max_num, positive = TRUE, null.ok = TRUE)

    nc <- tidync::tidync(path)
    trans <- nc[["transforms"]]

    dt_lat <- data.table::setDT(trans$lat)
    data.table::set(dt_lat, NULL, "dis", abs(dt_lat$lat - dict$latitude))
    data.table::setorderv(dt_lat, "dis")

    dt_lon <- data.table::setDT(trans$lon)
    data.table::set(dt_lon, NULL, "dis", abs(dt_lon$lon - dict$longitude))
    data.table::setorderv(dt_lon, "dis")

    res_lat <- dt_lat[dis <= threshold$lat]
    res_lon <- dt_lon[dis <= threshold$lon]

    if (!nrow(res_lat)) {
        stop("Threshold for latitude ('", threshold$lat, "') is smaller than ",
             "the minimum distance ('", min(dt_lat$dis), "') of current file resolution.")
    }
    if (!nrow(res_lon)) {
        stop("Threshold for latitude ('", threshold$lon, "') is smaller than ",
             "the minimum distance ('", min(dt_lon$dis), "') of current file resolution.")
    }

    if (!is.null(max_num)) {
        if (max_num < nrow(res_lat)) res_lat <- res_lat[seq.int(as.integer(max_num))]
        if (max_num < nrow(res_lon)) res_lon <- res_lon[seq.int(as.integer(max_num))]
    }

    list(longitude = res_lon[, list(index = .I, lon, dis)],
         latitude = res_lat[, list(index = .I, lat, dis)]
    )
}
# }}}

# match_location {{{
#' @importFrom progress progress_bar
#' @importFrom checkmate assert_scalar test_file_exists test_r6
#' @importFrom eplusr read_epw
#' @export
match_location <- function (pattern, threshold = list(lon = 1.0, lat = 1.0), max_num = NULL) {
    # load file index database
    index <- load_cmip6_index()
    if (test_r6(pattern, "Epw")) {
        dict <- pattern$location()
    } else {
        assert_scalar(pattern)
        if (tolower(tools::file_ext(pattern)) == "epw") {
            dict <- eplusr::read_epw(pattern)$location()
        } else {
            dict <- extract_location_dict(pattern)
            if (is.null(dict)) return(invisible())
        }
    }

    if (is.data.frame(dict)) {
        meta <- as.list(dict[, list(city = location, state_province, country,
            latitude, longitude)])
    } else {
        meta <- dict[c("city", "state_province", "country", "latitude", "longitude")]
    }

    # remove empty
    index <- index[!J(NA_character_), on = "file_path"]

    p <- progress::progress_bar$new(format = "[:current/:total][:bar] :percent [:elapsedfull]",
        total = nrow(index), clear = FALSE)

    coords <- lapply(index$file_path, function (f) {
        p$message(paste0("Processing file ", f, "..."))
        p$tick()
        match_location_coord(f, meta, threshold, max_num)
    })

    data.table::set(index, NULL, "coord", coords)

    structure(list(epw = meta, coord = index), class = "epw_coords")
}
# }}}
