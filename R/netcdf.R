# get_nc_meta {{{
#' @importFrom data.table setDT setattr
get_nc_meta <- function (file) {
    # to avoid No visible binding for global variable check NOTE
    J <- value <- name <- NULL
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
#' files available against the output file index loaded using
#' [load_cmip6_index()].
#'
#' `summary_database()` uses [future.apply][future.apply::future_lapply()]
#' underneath. You can use your preferable [future][future::plan] backend to
#' speed up data extraction in parallel. By default, `summary_database()` uses
#' [future::sequential] backend, which runs things in sequential.
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
#' @param append If `TRUE`, status of CMIP6 files will only be updated if they
#'        are not found in previous summary. This is useful if CMIP6 files are
#'        stored in different directories. Default: `FALSE`.
#'
#' @param mult Actions when multiple files match a same case in the CMIP6
#'        index. If `"latest"`, the file with latest modification time
#'        will be used. If `"skip"`, all matched files will be skip and this
#'        case will be kept as unmatched. Default: `"skip"`.
#'
#' @param recursive If `TRUE`, scan recursively into directories. Default:
#'        `FALSE`.
#'
#' @param update If `TRUE`, the output file index will be updated based
#'        on the matched NetCDF files in specified directory. If `FALSE`, only
#'        current loaded index will be updated, but the actual index
#'        database file saved in [get_data_dir()] will remain unchanged.
#'        Default: `FALSE`.
#'
#' @param warning If `TRUE`, warning messages will show when multiple files
#'        match a same case. Default: `TRUE`.
#'
#' @return A [data.table::data.table()] containing corresponding grouping
#' columns plus:
#'
#' | Column           | Type           | Description                         |
#' | -----            | -----          | -----                               |
#' | `datetime_start` | POSIXct        | Start date and time of simulation   |
#' | `datetime_end`   | POSIXct        | End date and time of simulation     |
#' | `file_num`       | Integer        | Total number of file per group      |
#' | `file_size`      | Units (Mbytes) | Approximate total size of file      |
#' | `dl_num`         | Integer        | Total number of file downloaded     |
#' | `dl_percent`     | Units (%)      | Total percentage of file downloaded |
#' | `dl_size`        | Units (Mbytes) | Total size of file downloaded       |
#'
#' Also an attribute `not_matched` is added to the returned
#' [data.table::data.table()] which contains meta data for those CMIP6 output
#' files that are not covered by current CMIP6 output file index.
#'
#' For the meaning of grouping columns, see [init_cmip6_index()].
#'
#' @examples
#' \dontrun{
#' summary_database()
#'
#' summary_database(by = "experiment")
#' }
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress
#' @export
summary_database <- function (
    dir,
    by = c("activity", "experiment", "variant", "frequency", "variable", "source", "resolution"),
    mult = c("skip", "latest"), append = FALSE,
    recursive = FALSE, update = FALSE, warning = TRUE)
{
    # column names
    dict <- c(activity_drs = "activity", experiment_id = "experiment", member_id = "variant",
              table_id = "frequency", variable_id = "variable", source_id = "source",
              nominal_resolution = "resolution")

    assert_directory_exists(dir, "w")
    assert_subset(by, choices = dict)

    mult <- match.arg(mult)

    # load index
    idx <- load_cmip6_index()

    # find all nc files in specified directory
    ncfiles <- list.files(dir, "\\.nc$", full.names = TRUE, recursive = recursive)

    message(paste0("", length(ncfiles), " NetCDF files found."))

    # columns to be added
    cols <- c("file_path", "file_realsize", "file_mtime", "time_units", "time_calendar")

    if (!length(ncfiles)) {
        vals <- list(NA_character_, NA_real_, Sys.time()[NA], NA_character_, NA_character_)
        names(vals) <- cols

        if (!append) {
            set(idx, NULL, cols, vals)
        } else {
            # add empty columns
            for (nm in cols) {
                if (nm %in% names(idx)) next
                set(idx, NULL, nm, vals[[nm]])
            }
        }
    } else {
        progressr::with_progress({
            p <- progressr::progressor(along = ncfiles)

            ncmeta <- rbindlist(future.apply::future_lapply(seq_along(ncfiles),
                function (i) {
                    p(message = sprintf("[%i/%i]", i, length(ncfiles)))
                    meta <- get_nc_meta(ncfiles[i])
                    time <- as.list(get_nc_time(ncfiles[i], range = TRUE))
                    names(time) <- c("datetime_start", "datetime_end")
                    c(meta, time)
                }
            ))
        })

        ncmeta[, `:=`(file_path = ncfiles, file_realsize = file.size(ncfiles), file_mtime = file.mtime(ncfiles))]

        # store original column names
        cols_idx <- names(idx)

        # add index
        idx[, index := .I]

        # add variable units, time units and file paths
        # NOTE: Should use right join here instead of adding by reference. This
        # is because for some GCMs, tracking id can be the same for multiple
        # files. The most safe way is to add additional checking for datetime
        # a) first match using tracking id
        idx_m <- ncmeta[, .SD, .SDcols = c("tracking_id", "datetime_start", "datetime_end", cols)][idx, on = "tracking_id", nomatch = NULL]

        # b) remove files that do not have any overlap in terms of datetime range
        idx_m <- idx_m[!(datetime_start > i.datetime_end | datetime_end < i.datetime_start)]

        # c) calculate the overlapped percentages coverred datetime of input
        # file to index data
        set(idx_m, NULL, "index_match", seq_len(nrow(idx_m)))
        idx_m[, by = c("index_match"),
            overlap := {
                diff <- as.numeric(difftime(i.datetime_end, i.datetime_start, units = "days"))
                sq <- seq(datetime_start, datetime_end, by = "1 day")
                sum(sq >= i.datetime_start & sq <= i.datetime_end) / diff
        }]

        # d) only keep items that have overlapped percentages larger than 60%
        idx_m <- idx_m[overlap >= 0.6]
        set(idx_m, NULL, c("overlap", "index_match"), NULL)

        # e) keep rows whose files have not yet been found
        idx <- rbindlist(list(idx[!idx_m, on = "index"], idx_m), fill = TRUE)
        data.table::setorderv(idx, "index")

        # store files that are not matched
        left <- ncmeta[!idx, on = "file_path"]

        # file meta columns in original index
        orig <- names(idx)[startsWith(names(idx), "i.")]
        if (!append) {
            # remove original file mata columns
            set(idx, NULL, orig, NULL)
        } else {
            # keep original match
            idx[J(NA_character_), on = "i.file_path", `:=`(
                i.file_path = file_path,
                i.file_realsize = file_realsize,
                i.file_mtime = file_mtime,
                i.time_units = time_units,
                i.time_calendar = time_calendar,
                i.datetime_start = datetime_start,
                i.datetime_end = datetime_end
            )]
            new <- gsub("^i\\.", "", orig)
            set(idx, NULL, new, NULL)
            setnames(idx, orig, new)
        }

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

    # update index
    EPWSHIFTR_ENV$index_db <- copy(idx)

    if (update) {
        # save database into the app data directory
        fwrite(idx, file.path(.data_dir(TRUE), "cmip6_index.csv"))
        verbose("Data file index updated and saved to '", normalizePath(file.path(.data_dir(TRUE), "cmip6_index.csv")), "'")
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

    setattr(sm, "not_matched", left)

    sm
}
# }}}

# get_nc_data {{{
#' @importFrom data.table as.data.table set setattr setcolorder
#' @importFrom RNetCDF utcal.nc
#' @importFrom units set_units
get_nc_data <- function (x, lats, lons, years, unit = TRUE) {
    assert_flag(unit)
    assert_integerish(years, lower = 1900, unique = TRUE, sorted = TRUE, any.missing = FALSE, null.ok = TRUE)

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

    # get dimensions
    dims <- get_nc_dims(nc)[J(c("lon", "lat", "time")), on = "name"][order(id)][, id := .I]
    dims[J("lon"), on = "name", `:=`(value = list(which_lon))]
    dims[J("lat"), on = "name", `:=`(value = list(which_lat))]

    dt <- rbindlist(use.names = TRUE, lapply(seq_along(which_time), function (i) {
        if (!length(which_time[[i]])) {
            return(data.table(
                value = double(),
                lon = double(), lat = double(),
                datetime = Sys.time()[0]
            ))
        }

        dims[J("time"), on = "name", `:=`(value = list(which_time[[i]]))]
        # Awkwardness arises mainly from one thing: NetCDF data are written
        # with the last dimension varying fastest, whereas R works opposite.
        # Thus, the order of the dimensions according to the CDL conventions
        # (e.g., time, latitude, longitude) is reversed in the R array (e.g.,
        # longitude, latitude, time).
        dt <- tryCatch(as.data.table(RNetCDF::var.get.nc(nc, var,
            c(min(dims$value[[1L]]), min(dims$value[[2L]]), min(dims$value[[3L]])),
            c(length(dims$value[[1L]]), length(dims$value[[2L]]), length(dims$value[[3L]])),
            collapse = FALSE)),
            error = function (e) {
                if (grepl("exceeds dimension bound", conditionMessage(e), fixed = TRUE)) {
                    NULL
                } else {
                    signalCondition(e)
                }
            }
        )

        # in case permutation did not work
        if (is.null(dt)) {
            dims <- dims[c(3L, 2L, 1L)][, id := .I]
            dt <- as.data.table(RNetCDF::var.get.nc(nc, var,
                c(min(dims$value[[1L]]), min(dims$value[[2L]]), min(dims$value[[3L]])),
                c(length(dims$value[[1L]]), length(dims$value[[2L]]), length(dims$value[[3L]])),
                collapse = FALSE
            ))
        }

        setnames(dt, c(dims$name, "value"))
        setnames(dt, "time", "datetime")

        set(dt, NULL, "lon", lons$value[dt$lon])
        set(dt, NULL, "lat", lats$value[dt$lat])
        set(dt, NULL, "datetime", time$datetime[[i]][dt$datetime])
    }))

    if (unit && nrow(dt)) {
        set(dt, NULL, "value", units::set_units(dt$value, units, mode = "standard"))
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
    setcolorder(dt, c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id"))

    dt
}
# }}}

# extract_data {{{
#' Extract data
#'
#' `extract_data()` takes an `epw_cmip6_coord` object generated using
#' [match_coord()] and extracts CMIP6 data using the coordinates and years of
#' interest specified.
#'
#' `extract_data()` uses [future.apply][future.apply::future_lapply()]
#' underneath. You can use your preferable [future][future::plan] backend to
#' speed up data extraction in parallel. By default, `extract_data()` uses
#' [future::sequential] backend, which runs things in sequential.
#'
#' @param coord An `epw_cmip6_coord` object created using [match_coord()]
#'
#' @param years An integer vector indicating the target years to be included in
#'        the data file. All other years will be excluded. If `NULL`, no
#'        subsetting on years will be performed. Default: `NULL`.
#'
#' @param unit If `TRUE`, units will be added to values using
#'        [units::set_units()].
#'
#' @param out_dir The directory to save extracted data using [fst::write_fst()].
#'        If `NULL`, all data will be kept in memory by default. Default: `NULL`.
#'
#' @param by A character vector of variable names used to split data
#'        during extraction. Should be a subset of:
#'
#' * `"experiment"`: root experiment identifiers
#' * `"source"`: model identifiers
#' * `"variable"`: variable identifiers
#' * `"activity"`: activity identifiers
#' * `"frequency"`: sampling frequency
#' * `"variant"`: variant label
#' * `"resolution"`: approximate horizontal resolution
#'
#' If `NULL` and `out_dir` is given, file name `data.fst` will be used. Default:
#' `NULL`.
#'
#' @param keep Whether keep extracted data in memory. Default: `TRUE` if
#'        `out_dir` is `NULL`, and `FALSE` otherwise.
#'
#' @param compress A single integer in the range 0 to 100, indicating the amount
#'        of compression to use. Lower values mean larger file sizes. Default:
#'        `100`.
#'
#' @return An `epw_cmip6_data` object, which is basically a list of 3 elements:
#'
#' * `epw`: An [eplusr::Epw] object whose longitude and latitute are used to
#'   extract CMIP6 data. It is the same object as created in [match_coord()]
#' * `meta`: A list containing basic meta data of input EPW, including `city`,
#'   `state_province`, `country`, `latitute` and `longitude`.
#' * `data`: An empty [data.table::data.table()] if `keep` is `FALSE` or a
#'   [data.table::data.table()] of 12 columns if `keep` is `TRUE`:
#'
#'     | No.  | Column           | Type      | Description                                                          |
#'     | ---: | -----            | -----     | -----                                                                |
#'     | 1    | `activity_drs`   | Character | Activity DRS (Data Reference Syntax)                                 |
#'     | 2    | `institution_id` | Character | Institution identifier                                               |
#'     | 3    | `source_id`      | Character | Model identifier                                                     |
#'     | 4    | `experiment_id`  | Character | Root experiment identifier                                           |
#'     | 5    | `member_id`      | Character | A compound construction from `sub_experiment_id` and `variant_label` |
#'     | 6    | `table_id`       | Character | Table identifier                                                     |
#'     | 7    | `lat`            | Double    | Latitude of extracted location                                       |
#'     | 8    | `lon`            | Double    | Latitude of extracted location                                       |
#'     | 9    | `variable`       | Character | Variable identifier                                                  |
#'     | 10   | `description`    | Character | Variable long name                                                   |
#'     | 11   | `units`          | Character | Units of variable                                                    |
#'     | 12   | `value`          | Double    | Start date and time of simulation                                    |
#'
#' @importFrom checkmate assert_class
#' @importFrom units set_units
#' @importFrom future.apply future_Map
#' @importFrom fst write_fst
#' @examples
#' \dontrun{
#' coord <- match_coord("path_to_an_EPW")
#' extract_data(coord, years = 2030:2060)
#' }
#' @export
extract_data <- function (coord, years = NULL, unit = FALSE, out_dir = NULL,
                          by = NULL, keep = is.null(out_dir), compress = 100) {
    assert_class(coord, "epw_cmip6_coord")

    # column names
    dict <- c(activity_drs = "activity", experiment_id = "experiment", member_id = "variant",
              table_id = "frequency", variable_id = "variable", source_id = "source",
              nominal_resolution = "resolution")

    # get matched coords
    m_coord <- coord$coord

    if (is.null(out_dir)) {
        m_coord <- list(m_coord)
    } else {
        assert_directory_exists(out_dir, "w")
        assert_subset(by, choices = dict)
        by_cols <- names(dict)[match(by, dict, 0L)]
        if (length(by_cols)) {
            m_coord <- split(m_coord[, .SD, .SDcols = c("file_path", "coord", by_cols)], by = by_cols)
            out_files <- file.path(normalizePath(out_dir), paste0(names(m_coord), ".fst"))
        } else {
            m_coord <- list(data = m_coord)
            out_files <- file.path(normalizePath(out_dir), "data.fst")
        }
    }

    # initial progress bar
    message("Start to extract CMIP6 data according to matched coordinates...")

    data <- data.table()

    for (i in seq_along(m_coord)) {
        co <- m_coord[[i]]

        if (!is.null(out_dir) && length(by_cols)) {
            message("Extracting data for case '", names(m_coord)[[i]], "'...")
        }
        progressr::with_progress({
            p <- progressr::progressor(nrow(co))
            ip <- 0L

            d <- rbindlist(
                future.apply::future_Map(
                    function (path, coord) {
                        ip <<- ip + 1L
                        p(message = sprintf("[%i/%i]", ip, nrow(co)))
                        get_nc_data(path, lats = coord$lat, lons = coord$lon, years = years, unit = unit)
                    },
                    co$file_path, co$coord
            ))
        })

        if (!is.null(out_dir)) {
            f <- out_files[i]
            fst::write_fst(d, f, compress = compress)
        }

        if (keep) {
            data <- rbindlist(list(data, d))
        }
    }

    structure(list(epw = coord$epw, meta = coord$meta, data = data), class = "epw_cmip6_data")
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

    vars <- rbindlist(fill = TRUE, lapply(seq_len(inq$nvars) - 1L, function (i) {
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

        j <- 1L
        l <- list()
        l[[1L]] <- i[[1L]]
        for (m in i[-1L]) {
            if (!length(m)) next
            if (length(l[[j]]) && l[[j]][length(l[[j]])] + 1L == m[1L]) {
                l[[j]] <- c(l[[j]], m)
            } else {
                j <- j + 1L
                l[[j]] <- m
            }
        }

        list(datetime = lapply(l, function (idx) time[idx]), which = l)
    }
}
# }}}

# rechunk_nc_dims {{{
rechunk_nc_dims <- function (nc, out_file) {
    if (Sys.which("nccopy") == "") return()

    out_file <- normalizePath(out_file, "/", mustWork = FALSE)

    system2("nccopy", c(
        "-k", "nc4",
        "-u",
        "-c", "time/365,lon/1,lat/1",
        paste0("'", nc, "'"), paste0("'", out_file, "'")
    ))

    normalizePath(out_file)
}
# }}}

# permute_nc_dims {{{
permute_nc_dims <- function (nc, out_file) {
    if (Sys.which("ncpdq") == "") return()

    out_file <- normalizePath(out_file, "/", mustWork = FALSE)

    system2("ncpdq", c(
        "--no_tmp_fl",
        "-h",
        "-O",
        "-a", "lat,lon,time",
        paste0("'", nc, "'"), paste0("'", out_file, "'")
    ))

    normalizePath(out_file)
}
# }}}

# reorganize_nc_dims {{{
# Ref: https://github.com/ebimodeling/model-drivers/tree/master/met/cruncep
reorganize_nc_dims <- function (nc) {
    f <- rechunk_nc_dims(nc, tempfile(fileext = ".nc"))
    if (is.null(f)) return(nc)
    re <- permute_nc_dims(f, nc)
    unlink(f)

    if (is.null(re)) return(nc)

    re
}
# }}}
