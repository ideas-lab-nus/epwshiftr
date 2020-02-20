# get_nc_meta {{{
#' Extract CMIP6 NetCDF file meta data
#' @param file A file path of NetCDF file.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{setDT}},\code{\link[data.table]{setattr}}
#'  \code{\link[ncmeta]{nc_atts}}
#' @rdname get_nc_meta
#' @export
#' @importFrom data.table setDT setattr
get_nc_meta <- function (file) {
    # to avoid No visible binding for global variable check NOTE
    J <- value <- name <- NULL
    verbose("Parsing meta data of NetCDF file '", file, "'...")
    # get all attributes
    atts <- get_nc_atts(file)

    # get meta data
    meta <- as.list(atts[
        J("NC_GLOBAL",
          c("mip_era", "activity_id", "institution_id", "source_id",
            "experiment_id", "variant_label", "table_id", "grid_label",
            "nominal_resolution", "variable_id", "tracking_id")
        ),
        on = c("variable", "attribute"),
        {v <- unlist(value); names(v) <- attribute; v}])

    # get variable long name and units
    meta <- c(
        meta,
        setattr(
            atts[J(meta$variable_id, c("standard_name", "units")), on = c("variable", "attribute"), value],
            "names", c("standard_name", "units")
        )
    )

    # get time origin and unit
    c(
        meta,
        setattr(
            atts[J("time", c("units", "calendar")), on = c("variable", "attribute"), value],
            "names", c("time_units", "time_calendar")
        )
    )
}
# }}}

# summary_database {{{
#' Summary CMIP6 model output file status
#'
#' `summary_database()` scan the directory specified and returns a
#' [data.table()] containing summary information about all the CMIP6
#' files available against the output file index database loaded using
#' [load_cmip6_index()].
#'
#' @param dir A single string indcating the directory where CMIP6 model output
#'        NetCDF files are stored.
#'
#' @param by The grouping column to summary the database status. Should be a
#'        subeset of:
#'
#' * `"experiment"`: root experiment identifiers
#' * `"source"`: model identifiers
#' * `"variable"`: variable identifiers
#' * `"activity"`: activity identifiers
#' * `"frequency"`: sampling frequency
#' * `"variant"`: variant label
#' * `"resolution"`: approximate horizontal resolution
#'
#' @param mult Actions when multiple files match a same case in the CMIP6
#'        index database. If `"latest"`, the file with latest modification time
#'        will be used. If `"skip"`, all matched files will be skip and this
#'        case will be kept as unmatched. Default: `"skip"`.
#'
#' @param recursive If `TRUE`, scan recursively into directories. Default:
#'        `FALSE`.
#'
#' @param update If `TRUE`, the output file index database will be updated based
#'        on the matched NetCDF files in specified directory. If `FALSE`, only
#'        current loaded index database will be updated, but the actual index
#'        database file saved in [rappdirs::app_dir()] will remain unchanged.
#'        Default: `FALSE`.
#'
#' @param warning If `TRUE`, warning messages will show when multiple files
#'        match a same case. Default: `TRUE`.
#'
#' @return A [data.table()] containing corresponding grouping
#' columns plus:
#'
#' | Column           | Type           | Description                         |
#' | -----            | -----          | -----                               |
#' | `datetime_start` | POSIXct        | Start date and time of simulation   |
#' | `datetime_end`   | POSIXct        | End date and time of simulation     |
#' | `file_num`       | Integer        | Total number of file per group      |
#' | `file_size`      | Units [Mbytes] | Approximate total size of file      |
#' | `dl_num`         | Integer        | Total number of file downloaded     |
#' | `dl_percent`     | Units [%]      | Total percentage of file downloaded |
#' | `dl_size`        | Units [Mbytes] | Total size of file downloaded       |
#'
#' For the meaning of grouping columns, see [init_cmip6_index()].
#'
#' @examples
#' \dontrun{
#' summary_database()
#' }
#' @export
summary_database <- function (
    dir,
    by = c("activity", "experiment", "variant", "frequency", "variable", "source", "resolution"),
    mult = c("skip", "latest"),
    recursive = FALSE, update = FALSE, warning = TRUE)
{
    # column names
    dict <- c(activity_drs = "activity", experiment_id = "experiment", member_id = "variant",
              table_id = "frequency", variable_id = "variable", source_id = "source",
              nominal_resolution = "resolution")

    assert_directory_exists(dir, "w")
    assert_subset(by, choices = dict)

    mult <- match.arg(mult)

    # load index database
    idx <- load_cmip6_index()

    # find all nc files in specified directory
    ncfiles <- list.files(dir, "\\.nc$", full.names = TRUE, recursive = recursive)

    p <- progress::progress_bar$new(format = "[:current/:total][:bar] :percent [:elapsedfull]",
        total = length(ncfiles), clear = FALSE)

    p$message(paste0("", length(ncfiles), " NetCDF files found."))

    # columns to be added
    cols <- c("file_path", "file_realsize", "file_mtime", "time_units", "time_calendar")

    if (!length(ncfiles)) {
        # add empty columns
        set(idx, NULL, cols, list(NA_character_, NA_real_, Sys.time()[NA], NA_character_, NA_character_))

    } else {
        ncmeta <- rbindlist(lapply(ncfiles, function (f) {
            p$message(paste0("Processing file ", f, "..."))
            p$tick()
            meta <- get_nc_meta(f)
            time <- as.list(get_nc_time(f, range = TRUE))
            names(time) <- c("datetime_start", "datetime_end")
            c(meta, time)
        }))

        ncmeta[, `:=`(file_path = ncfiles, file_realsize = file.size(ncfiles), file_mtime = file.mtime(ncfiles))]

        # remove existing file meta column
        if (length(cols_del <- cols[cols %in% names(idx)])) set(idx, NULL, cols_del, NULL)
        # store original column names
        cols_idx <- names(idx)

        # add index
        idx[, index := .I]

        # add variable units, time units and file paths
        # NOTE: Should use right join here instead of adding by reference. This
        # is because for some GCMs, tracking id can be the same for multiple
        # files. The most safe way is to add additional checking for datetime
        # a) first match using tracking id
        idx <- ncmeta[, .SD, .SDcols = c("tracking_id", "datetime_start", "datetime_end", cols)][idx, on = "tracking_id"]
        # b) remove files that do not have any overlap in terms of datetime range
        # but keep rows whose files have not yet been found
        idx <- idx[!(datetime_start > i.datetime_end | datetime_end < i.datetime_start) | is.na(file_path)]

        # remove helper column
        set(idx, NULL, c("i.datetime_start", "i.datetime_end"), NULL)

        if (length(dup_idx <- unique(idx$index[duplicated(idx$index)]))) {
            dup <- idx[J(dup_idx), on = "index"]

            if (mult == "skip") {
                # remove duplications
                idx <- idx[!duplicated(index)]
                # reset abnormal case status to non-matched
                set(idx, dup_idx, cols, NA)
            } else {
                # order by file mtime
                data.table::setorderv(dup, c("index", "file_mtime"), c(1, -1))
                # remove old files
                idx <- idx[!dup[, by = "index", list(file_path = file_path[-1L])], on = c("index", "file_path")]
            }

            if (warning) {
                # construct error message:
                dup[, index_case := data.table::rleid(index)]
                mes <- dup[, by = "index_case", {
                    head <- sprintf("#%i | For case '%s':\n", .BY$index_case, gsub("\\|.+$", "", file_id[1]))
                    file <- sprintf("   --> [%i] '%s'", seq_along(file_path), file_path)
                    list(message = paste0(head, paste0(file, collapse = "\n")))
                }]$message

                ori <- getOption("warning.length")
                options(warning.length = 8170L)
                on.exit(options(warning.length = ori), add = TRUE)

                act <- if (mult == "skip") {
                    "Those files will be skipped and the cases will remain unmatched."
                } else {
                    "The file with latest modification time, listed as the first, will be used. All other files will be skipped"
                }

                warning("Case(s) shown below matches multiple NetCDF files in the database. ",
                    "Please check if there are duplicated NetCDF files in your database ",
                    "and check NetCDF global attributes of those files ",
                    "to see if there are any incorrect values. ", act, "\n",
                    paste0(mes, collapse = "\n"), call. = FALSE)
            }
        }

        # remove index
        set(idx, NULL, "index", NULL)

        # keep the original order
        setcolorder(idx, setdiff(cols_idx, "index"))
    }

    # update index database
    EPWSHIFTR_ENV$index_db <- copy(idx)

    if (update) {
        # save database into the app data directory
        fwrite(idx, file.path(.data_dir(TRUE), "cmip6_index.csv"))
        verbose("Data file index database updated and saved to '", normalizePath(file.path(.data_dir(TRUE), "cmip6_index.csv")), "'")
    }

    by_cols <- names(dict)[which(dict %in% by)]

    sm <- idx[, list(
        datetime_start = min(datetime_start),
        datetime_end = max(datetime_end),
        file_num = .N,
        file_size = round(units::set_units(units::set_units(sum(file_size, na.rm = TRUE), "bytes"), "Mbytes")),
        dl_num = sum(!is.na(file_path)),
        dl_percent = round(units::set_units(sum(!is.na(file_path)) / .N * 100, "%"), 3),
        dl_size = round(units::set_units(units::set_units(sum(file_realsize, na.rm = TRUE), "bytes"), "Mbytes"))
    ), by = c(by_cols)][order(-dl_percent)]

    sm
}
# }}}

# get_nc_data {{{
#' @importFrom data.table as.data.table set setattr setcolorder
#' @importFrom RNetCDF utcal.nc
#' @importFrom units set_units
#' @export
get_nc_data <- function (x, lats, lons, years, unit = TRUE) {
    assert_flag(unit)

    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get all attributes
    atts <- get_nc_atts(nc)

    # get variable name
    var <- atts[J("NC_GLOBAL", "variable_id"), on = c("variable", "attribute"), value[[1L]]]

    # get variable long name
    var_long <- atts[J(var, "long_name"), on = c("variable", "attribute"), value[[1L]]]

    # get variable units
    units <- atts[variable == var & attribute == "units", value[[1L]]]

    # match time
    time <- match_nc_time(nc, years)

    which_lat <- lats$which
    which_lon <- lons$which
    which_time <- time$which

    dt <- rbindlist(lapply(seq_along(which_time), function (i) {
        if (!length(which_time[[i]])) {
            return(data.table(
                value = double(),
                lon = double(), lat = double(),
                datetime = Sys.time()[0]
            ))
        }

        # Awkwardness arises mainly from one thing: NetCDF data are written
        # with the last dimension varying fastest, whereas R works opposite.
        # Thus, the order of the dimensions according to the CDL conventions
        # (e.g., time, latitude, longitude) is reversed in the R array (e.g.,
        # longitude, latitude, time).
        dt <- as.data.table(RNetCDF::var.get.nc(nc, var,
            c(min(which_lon), min(which_lat), min(which_time[[i]])),
            c(length(which_lon), length(which_lat), length(which_time[[i]]))
        ))

        set(dt, NULL, "lon", lons$lon[dt$V1])
        set(dt, NULL, "lat", lats$lat[dt$V2])
        set(dt, NULL, "datetime", time$datetime[[i]][dt$V3])
        set(dt, NULL, c("V1", "V2", "V3"), NULL)
    }))

    if (unit && nrow(dt)) {
        set(dt, NULL, var, units::set_units(dt[[var]], units, mode = "standard"))
    }

    # change to tidy format
    set(dt, NULL, c("variable", "description", "units"), list(var, var_long, units))

    # change column order
    setcolorder(dt, c("datetime", "lat", "lon", "variable", "description", "units", "value"))

    set(dt, NULL,
        c("activity_drs", "experiment_id", "institution_id", "source_id", "member_id", "table_id"),
        atts[J("NC_GLOBAL", c("activity_id", "experiment_id", "institution_id", "source_id", "variant_label", "frequency")),
            on = c("variable", "attribute"), value]
    )
    setcolorder(dt, c("activity_drs", "experiment_id", "institution_id", "source_id", "member_id", "table_id"))

    dt
}
# }}}

# extract_data {{{
#' Extract data
#'
#' @param pattern XX
#' @param years XX
#' @param threshold XX
#' @param max_num XX
#' @param unit If `TRUE`, units will be added to values
#'
#' @return A data.table
#'
#' @importFrom checkmate assert_class
#' @export
extract_data <- function (pattern, years = NULL, threshold = list(lon = 1.0, lat = 1.0), max_num = NULL, unit = FALSE) {
    loc <- match_location(pattern = pattern, threshold = threshold, max_num = max_num)

    # get matched coords
    loc <- loc$coord

    # initial progress bar
    p <- progress::progress_bar$new(
        format = "[:current/:total][:bar] :percent [:elapsedfull]",
        total = nrow(loc), clear = FALSE)

    data <- Map(
        function (path, coord) {
            p$message(sprintf("Processing file '%s'...", path))
            d <- get_nc_data(path, lats = coord$lat, lons = coord$lon, years = years, unit = unit)
            p$tick()
            d
        },
        loc$file_path, loc$coord
    )

    set(loc, NULL, "data", data)

    loc
}
# }}}

# download_nc {{{
download_nc <- function (url, dir, overwrite = FALSE) {
    p <- progress::progress_bar$new(format = "[:current/:total][:bar] :percent [:elapsedfull]",
        total = length(url), clear = FALSE)

    lapply(url, function (i) {
        p$message(paste0("Downloading file ", basename(i), "..."))
        f <- file.path(dir, basename(i))
        if (!overwrite && file.exists(f)) return(normalizePath(f))
        p$tick()
        tryCatch(utils::download.file(url, f, quiet = FALSE, mode = "wb", method = "libcurl"), error = function (e) NULL)
        f
    })
}
# }}}

# get_nc_atts {{{
get_nc_atts <- function(x) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get file info
    inq <- RNetCDF::file.inq.nc(nc)

    # global attributes
    glo <- rbindlist(lapply(seq_len(inq$ngatts), function (i) {
        RNetCDF::att.inq.nc(nc, "NC_GLOBAL", i - 1L)
    }))
    # set global attribute id to -1
    set(glo, NULL, "id", -1L)
    set(glo, NULL, "variable", "NC_GLOBAL")
    set(glo, NULL, c("length", "type"), NULL)
    set(glo, NULL, "value",
        lapply(seq_len(inq$ngatts), function (i) RNetCDF::att.get.nc(nc, "NC_GLOBAL", i - 1L))
    )
    setnames(glo, "name", "attribute")

    # get variables
    vars <- unique(get_nc_vars(nc)[natts > 0L, .SD, .SDcols = c("id", "name", "natts")])
    vars <- vars[, by = list(idx = seq_len(nrow(vars))), {
        nm <- lapply(seq_len(natts) - 1L, function (i) RNetCDF::att.inq.nc(nc, id, i)$name)
        att <- lapply(seq_len(natts) - 1L, function (i) RNetCDF::att.get.nc(nc, id, i))
        list(id = id, variable = rep(name, length(nm)), attribute = unlist(nm), value = att)
    }]
    set(vars, NULL, "idx", NULL)

    rbindlist(list(vars, glo), use.names = TRUE)
}
# }}}

# get_nc_vars {{{
get_nc_vars <- function (x) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get file info
    inq <- RNetCDF::file.inq.nc(nc)

    vars <- rbindlist(lapply(seq_len(inq$nvars) - 1L, function (i) {
        res <- RNetCDF::var.inq.nc(nc, i)
        res <- res[names(res) != "dimids"]
        res[vapply(res, length, integer(1)) > 0L]
    }))

    vars
}
# }}}

# get_nc_dims {{{
get_nc_dims <- function (x) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # get file info
    inq <- RNetCDF::file.inq.nc(nc)

    rbindlist(lapply(seq_len(inq$ndims) - 1L, function (i) {
        RNetCDF::dim.inq.nc(nc, i)
    }))
}
# }}}

# get_nc_axes {{{
get_nc_axes <- function (x) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    # wl wl get file info
    inq <- RNetCDF::file.inq.nc(nc)

    vars <- rbindlist(lapply(seq_len(inq$nvars) - 1L, function (i) {
        RNetCDF::var.inq.nc(nc, i)
    }))[, `:=`(axis = .I)]
    set(vars, NULL, setdiff(names(vars), c("axis", "name", "dimids")), NULL)
    setcolorder(vars, "axis")
    setnames(vars, c("axis", "variable", "dimension"))

    vars[]
}
# }}}

# get_nc_time {{{
get_nc_time <- function (x, range = FALSE) {
    if (inherits(x, "NetCDF")) {
        nc <- x
    } else {
        nc <- RNetCDF::open.nc(x)
        on.exit(RNetCDF::close.nc(nc), add = TRUE)
    }

    n <- get_nc_dims(nc)[J("time"), on = "name", length]

    # get variable attributes
    atts <- get_nc_atts(nc)

    # get time origin and units
    ori <- atts[variable == "time" & attribute == "units", value[[1L]]]

    # get first time value
    time_start <- RNetCDF::var.get.nc(nc, "time", 1, 1)

    # get last time value
    time_end <- RNetCDF::var.get.nc(nc, "time", n, 1)

    if (range) {
        c(
            RNetCDF::utcal.nc(ori, time_start, "c"),
            RNetCDF::utcal.nc(ori, time_end, "c")
        )
    } else {
        # create time sequences
        seq(
            RNetCDF::utcal.nc(ori, time_start, "c"),
            RNetCDF::utcal.nc(ori, time_end, "c"),
            length.out = n
        )
    }
}
# }}}

# match_nc_time {{{
match_nc_time <- function (x, years = NULL) {
    assert_integerish(years, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

    time <- get_nc_time(x)

    if (is.null(years)) {
        list(datetime = list(time), which = list(seq_along(time)))
    } else {
        y <- data.table::year(time)
        i <- lapply(as.integer(years), function (x) which(y == x))

        list(datetime = lapply(i, function (idx) time[idx]), which = i)
    }
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
    dis_lat <- abs(dim$lat - lat)
    dis_lon <- abs(dim$lon - lon)

    i_lat <- which(dis_lat <= threshold$lat)
    i_lon <- which(dis_lon <= threshold$lon)

    if (!length(i_lat)) {
        stop("Threshold for latitude ('", threshold$lat, "') is smaller than ",
             "the minimum distance ('", round(min(dis_lat), 2), "') of current file resolution.")
    }
    if (!length(i_lon)) {
        stop("Threshold for latitude ('", threshold$lon, "') is smaller than ",
             "the minimum distance ('", round(min(dis_lon), 2), "') of current file resolution.")
    }

    if (!is.null(max_num)) {
        if (max_num < length(i_lat)) i_lat <- i_lat[seq.int(as.integer(max_num))]
        if (max_num < length(i_lon)) i_lon <- i_lon[seq.int(as.integer(max_num))]
    }

    list(lat = list(index = seq_along(i_lat), lat = dim$lat[i_lat], dis = dis_lat[i_lat], which = i_lat),
         lon = list(index = seq_along(i_lon), lon = dim$lon[i_lon], dis = dis_lon[i_lon], which = i_lon)
    )
}
# }}}
