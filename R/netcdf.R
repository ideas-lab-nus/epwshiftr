# get_nc_meta {{{
get_nc_meta <- function (file) {
    # to avoid No visible binding for global variable check NOTE
    J <- value <- name <- NULL
    # get all attributes
    atts <- get_nc_atts(file)

    # get metadata
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
        data.table::setattr(
            atts[J(meta$variable_id, c("standard_name", "units")), on = c("variable", "attribute"), value],
            "names", c("standard_name", "units")
        )
    )

    # get time origin and unit
    c(
        meta,
        data.table::setattr(
            atts[J("time", c("units", "calendar")), on = c("variable", "attribute"), value],
            "names", c("time_units", "time_calendar")
        )
    )
}
# }}}

summary_database_scan_file <- function (file) {
    nc <- RNetCDF::open.nc(file)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)

    time <- as.list(get_nc_time(nc, range = TRUE))
    names(time) <- c("datetime_start", "datetime_end")

    data.table::as.data.table(c(
        get_nc_meta(nc),
        time,
        list(
            file_path = file,
            file_realsize = file.size(file),
            file_mtime = file.mtime(file)
        )
    ))
}

# summary_database {{{
#' Summary CMIP6 model output file status
#'
#' `summary_database()` scans the directory specified and returns a
#' [data.table()] containing summary information about all the CMIP6
#' files available against the output file index loaded using
#' [load_cmip6_index()].
#'
#' The database here can be any directory that stores the NetCDF files for CMIP6
#' GCMs. It can also be the same as [store_dir()] where epwshiftr keeps the
#' persistent store, if you want to keep the output file index and output files
#' together.
#'
#' `summary_database()` uses the `tracking_id`, `datetime_start` and
#' `datetime_end` global attributes of each NetCDF file to match against the
#' output file index. So the names of NetCDF files do not necessarily follow the
#' CMIP6 file name encoding.
#'
#' `summary_database()` will append 5 columns in the CMIP6 output file index:
#'
#' - `file_path`: the full path of matched NetCDF file for every case.
#'
#' `summary_database()` uses [future.apply][future.apply::future_lapply()]
#' underneath to speed up the data processing if applicable. You can use your
#' preferable future backend to speed up data extraction in parallel. By default,
#' `summary_database()` uses `future::sequential` backend, which runs things in
#' sequential.
#'
#' @param dir A single string indicating the directory where CMIP6 model output
#'        NetCDF files are stored.
#'
#' @param by The grouping column to summary the database status. Should be a
#'        subset of:
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
#' @param miss Actions when matched files in the previous summary do not exist
#'        when running current summary. Only applicable when `append` is set to
#'        `TRUE`. If `"keep"`, the metadata for the missing output files will
#'        be kept. If `"overwrite"`, existing metadata of those output will be
#'        first removed from the output file index and overwritten based on the
#'        newly matched files if possible. Default: `"keep"`.
#'
#' @param recursive If `TRUE`, scan recursively into directories. Default:
#'        `FALSE`.
#'
#' @param update If `TRUE`, the output file index will be updated based
#'        on the matched NetCDF files in specified directory. If `FALSE`, only
#'        current loaded index will be updated, but the actual index file saved
#'        in the persistent store will remain unchanged.
#'        Default: `FALSE`.
#'
#' @param warning If `TRUE`, warning messages will show when target files are
#'        missing or multiple files match a same case. Default: `TRUE`.
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
#' Also 2 extra [data.table::data.table()] are attached as **attributes**:
#'
#' - `not_found`: A [data.table::data.table()] that contains metadata for those
#'   CMIP6 outputs that are listed in current CMIP6 output file index but the
#'   existing file paths are not valid now and cannot be found in current
#'   database.
#'
#' - `not_matched`: A [data.table::data.table()] that contains metadata for
#'   those CMIP6 output files that are found in current database but not listed
#'   in current CMIP6 output file index.
#'
#' For the meaning of grouping columns, see [init_cmip6_index()].
#'
#' @examples
#' \dontrun{
#' summary_database()
#'
#' summary_database(by = "experiment")
#' }
#'
#' @export
# TODO: should all not_matched and not_found merged int one?
summary_database <- function (
    dir,
    by = c("activity", "experiment", "variant", "frequency", "variable", "source", "resolution"),
    mult = c("skip", "latest"), append = FALSE, miss = c("keep", "overwrite"),
    recursive = FALSE, update = FALSE, warning = TRUE)
{
    # column names
    dict <- c(activity_drs = "activity", experiment_id = "experiment", member_id = "variant",
              table_id = "frequency", variable_id = "variable", source_id = "source",
              nominal_resolution = "resolution")

    checkmate::assert_directory_exists(dir, "w")
    checkmate::assert_subset(by, choices = dict)

    mult <- match.arg(mult)
    miss <- match.arg(miss)

    # load index
    idx <- load_cmip6_index()

    # find all nc files in specified directory
    ncfiles <- list.files(dir, "[.](nc)|(hdf)$", full.names = TRUE, recursive = recursive)

    vmsg(sprintf("%s NetCDF files found.", length(ncfiles)))

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

        left <- data.table::data.table()
        miss <- data.table::data.table()
    } else {
        if (length(ncfiles) == 1L) {
            ncmeta <- summary_database_scan_file(ncfiles)
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
        }

        # store original column names
        cols_idx <- names(idx)

        # add index
        idx[, index := .I]

        # add variable units, time units and file paths
        # NOTE: Should use right join here instead of adding by reference. This
        # is because for some GCMs, tracking id can be the same for multiple
        # files. The most safe way is to add additional checking for datetime
        #
        # a) remove existing output file metadata if necessary
        if (!append && any(cols %in% cols_idx)) {
            set(idx, NULL, cols[cols %in% cols_idx], NULL)
        }

        # b) check existence of previous matched files
        is_lost <- FALSE # in case append is set to FALSE
        if (append && "file_path" %in% cols_idx && any(is_lost <- !is.na(idx$file_path) & !file.exists(idx$file_path))) {
            lost <- idx[is_lost]
            if (miss == "overwrite") {
                i_lost <- which(is_lost)
                set(idx, i_lost, "file_path", NA_character_)
                set(idx, i_lost, "file_realsize", NA_real_)
                set(idx, i_lost, "file_mtime", as.POSIXct(NA))
                set(idx, i_lost, "time_units", NA_character_)
                set(idx, i_lost, "time_calendar", NA_character_)
            }

            if (warning) {
                # construct error message:
                lost[, index_case := data.table::rleid(index)]
                mes <- lost[, by = "index_case", {
                    head <- sprintf("#%i | For case '%s':\n", .BY$index_case, gsub("\\|.+$", "", file_id[1]))
                    file <- sprintf("   --> file '%s'", normalizePath(file_path, mustWork = FALSE))
                    list(message = paste0(head, file))
                }]$message
                set(lost, NULL, "index_case", NULL)

                ori <- getOption("warning.length")
                options(warning.length = 8170L)
                on.exit(options(warning.length = ori), add = TRUE)

                warning("Previously matched NetCDF file(s) below does not exist anymore. ",
                    if (miss == "keep") {
                        "Its metadata are kept in the output file index as `miss` is set to `\"keep\"`.\n"
                    } else if (miss == "overwrite") {
                        paste(
                            "Since `miss` is set to `\"overwrite\"`, its metadata has been **removed** from the output file index",
                            "and will be overwritten by the data from new matched NetCDF file if possible.\n"
                        )
                    },
                    paste0(mes, collapse = "\n"),
                    "\n\nYou can run `attr(x, \"not_found\")` to see the metadata of those files.",
                    call. = FALSE
                )
            }
            set(lost, NULL, "index", NULL)
        }

        # c) first match using tracking id
        idx_m <- ncmeta[, .SD, .SDcols = c("tracking_id", "datetime_start", "datetime_end", cols)][idx, on = "tracking_id", nomatch = NULL]

        # d) remove files that do not have any overlap in terms of datetime range
        idx_m <- idx_m[!(datetime_start > i.datetime_end | datetime_end < i.datetime_start)]

        # e) calculate the overlapped percentages coverred datetime of input
        # file to index data
        set(idx_m, NULL, "index_match", seq_len(nrow(idx_m)))
        if (!nrow(idx_m)) {
            set(idx_m, NULL, "overlap", double())
        } else {
            idx_m[, by = c("index_match"),
                overlap := {
                    diff <- as.numeric(difftime(i.datetime_end, i.datetime_start, units = "days"))
                    sq <- seq(datetime_start, datetime_end, by = "1 day")
                    sum(sq >= i.datetime_start & sq <= i.datetime_end) / diff
            }]
        }

        # f) only keep items that have overlapped percentages larger than 60%
        idx_m <- idx_m[overlap >= 0.6]
        set(idx_m, NULL, c("overlap", "index_match"), NULL)

        # g) keep rows whose files have not yet been found
        if (!any(is_lost)) {
            idx <- rbindlist(list(idx[!idx_m, on = "index"], idx_m), fill = TRUE)
        } else {
            should_keep <- is_lost | !idx$index %in% idx_m$index
            idx <- rbindlist(list(idx[should_keep], idx_m[J(idx$index[!should_keep]), on = "index"]), fill = TRUE)
        }
        data.table::setorderv(idx, "index")

        # store files that are not matched
        left <- ncmeta[!idx, on = "file_path"]

        # file meta columns in original index
        orig <- names(idx)[startsWith(names(idx), "i.")]
        if (!append) {
            # remove original file mata columns
            set(idx, NULL, orig, NULL)
        } else {
            if ("i.file_path" %in% orig) {
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
            }
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
                    paste0(mes, collapse = "\n"),
                    "\nYou can run `attr(x, \"not_matched\")` to see the metadata of those files.",
                    call. = FALSE)
            }
        }

        # keep the original order
        setcolorder(idx, setdiff(cols_idx, c("index", cols)))

        # store files that are not found after updating
        miss <- idx[is.na(file_path)]
        set(miss, NULL, cols, NULL)

        if (nrow(miss) && warning) {
            # construct error message:
            miss[, index_case := data.table::rleid(index)]
            mes <- miss[, by = "index_case", {
                list(message = sprintf("#%i | For case '%s':\n", .BY$index_case, gsub("\\|.+$", "", file_id[1])))
            }]$message
            set(miss, NULL, "index_case", NULL)

            ori <- getOption("warning.length")
            options(warning.length = 8170L)
            on.exit(options(warning.length = ori), add = TRUE)

            warning("Case(s) shown below does not matche any NetCDF file in the database. ",
                "Please make sure all needed NetCDF files listed in the file index have been downloaded and placed in the database.\n",
                paste0(mes, collapse = "\n"),
                "\n\nYou can run `attr(x, \"not_found\")` to see the metadata of those files.",
                call. = FALSE)
        }

        # remove index
        set(miss, NULL, "index", NULL)
        set(idx, NULL, "index", NULL)

        # combine not found and not matched together
        if (any(is_lost)) {
            # in this case, file_path, file_realsize and other columns are added
            # for missed data
            miss <- rbindlist(list(miss, lost), fill = TRUE)
        }
    }

    # update index
    this$index_db <- copy(idx)

    if (update) {
        index_path <- store_cmip6_index_save(idx)
        vmsg(sprintf(
            "Data file index updated and saved to '%s'",
            normalizePath(index_path, winslash = "/", mustWork = FALSE)
        ))
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

    data.table::setattr(sm, "not_matched", left)
    data.table::setattr(sm, "not_found", miss)

    sm
}
# }}}

# get_nc_data {{{
#' @importFrom data.table set setcolorder
get_nc_data <- function (x, coord, years, unit = TRUE) {
    checkmate::assert_flag(unit)
    checkmate::assert_integerish(years, lower = 1900, unique = TRUE, sorted = TRUE, any.missing = FALSE, null.ok = TRUE)

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
    which_time <- time$which

    # get dimensions
    dims <- get_nc_dims(nc)[J(c("lon", "lat", "time")), on = "name"][order(id)][, id := .I]
    dims[J("lon"), on = "name", `:=`(value = list(coord$ind_lon))]
    dims[J("lat"), on = "name", `:=`(value = list(coord$ind_lat))]

    # find correct dimentions
    ord <- list(
        c("lon", "lat", "time"),
        c("lat", "lon", "time"),
        c("time", "lon", "lat"),
        c("lat", "time", "lon"),
        c("lon", "time", "lat"),
        c("time", "lat", "lon")
    )
    ord <- ord[vapply(ord, function (i) {
        d <- dims[J(i), on = "name"]$length
        !is.null(tryCatch(RNetCDF::var.get.nc(nc, var, d, rep(1L, 3), collapse = FALSE),
            error = function (e) {
                if (grepl("exceeds dimension bound", conditionMessage(e), fixed = TRUE)) {
                    NULL
                } else {
                    # nocov start
                    signalCondition(e)
                    # nocov end
                }
            }
        ))
    }, logical(1))][[1L]]
    dims <- dims[J(ord), on = "name"]

    dt <- rbindlist(use.names = TRUE, lapply(seq_along(which_time), function (i) {
        if (!length(which_time[[i]])) {
            return(data.table::data.table(
                index = integer(),
                lon = double(), lat = double(), dist = double(),
                datetime = Sys.time()[0], value = double()
            ))
        }

        dims[J("time"), on = "name", `:=`(value = list(which_time[[i]]))]
        # Awkwardness arises mainly from one thing: NetCDF data are written
        # with the last dimension varying fastest, whereas R works opposite.
        # Thus, the order of the dimensions according to the CDL conventions
        # (e.g., time, latitude, longitude) is reversed in the R array (e.g.,
        # longitude, latitude, time).
        dt <- data.table::as.data.table(RNetCDF::var.get.nc(nc, var,
            c(min(dims$value[[1L]]), min(dims$value[[2L]]), min(dims$value[[3L]])),
            c(length(unique(dims$value[[1L]])), length(unique(dims$value[[2L]])), length(dims$value[[3L]])),
            collapse = FALSE)
        )

        setnames(dt, c(paste0("ord_", dims$name), "value"))
        setnames(dt, "ord_time", "datetime")
        set(dt, NULL, "datetime", time$datetime[[i]][dt$datetime])

        # only keep the matched points
        ind_lon <- ind_lat <- ord_lon <- ord_lat <- .GRP <- NULL # to make CRAN check happy
        # NOTE: make a copy of input coord here, otherwise there will be a shallow
        # copy warning
        # should be cheap to do so, since the coord is normally a small DT
        co <- copy(coord)
        co[order(ind_lon), ord_lon := .GRP, by = "ind_lon"]
        co[order(ind_lat), ord_lat := .GRP, by = "ind_lat"]
        res <- dt[co, on = c("ord_lon", "ord_lat")]
        set(res, NULL, c("ord_lon", "ord_lat", "ind_lon", "ind_lat"), NULL)
        setcolorder(res, setdiff(names(res), c("datetime", "value")))

        res
    }))

    if (unit && nrow(dt)) {
        set(dt, NULL, "value", units::set_units(dt$value, units, mode = "standard"))
    }

    # change to tidy format
    set(dt, NULL, c("variable", "description", "units"), list(var, var_long, units))

    # change column order
    setcolorder(dt, c("index", "lon", "lat", "dist", "datetime", "variable", "description", "units", "value"))

    set(dt, NULL,
        c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id"),
        atts[J("NC_GLOBAL", c("activity_id", "institution_id", "source_id", "experiment_id", "variant_label", "frequency")),
            on = c("variable", "attribute"), value]
    )
    setcolorder(dt, c("index", "activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id"))

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
#' `extract_data()` parses CF-compliant time coordinates into UTC `POSIXct`
#' values using epwshiftr's internal NetCDF time parser.
#'
#' `extract_data()` uses [future.apply][future.apply::future_lapply()]
#' underneath. You can use your preferable future backend to
#' speed up data extraction in parallel. By default, `extract_data()` uses
#' `future::sequential` backend, which runs things in sequential.
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
#' * `epw`: An [eplusr::Epw] object whose longitude and latitude are used to
#'   extract CMIP6 data. It is the same object as created in [match_coord()]
#' * `meta`: A list containing basic metadata of input EPW, including `city`,
#'   `state_province`, `country`, `latitude` and `longitude`.
#' * `data`: An empty [data.table::data.table()] if `keep` is `FALSE` or a
#'   [data.table::data.table()] of 14 columns if `keep` is `TRUE`:
#'
#'     | No.  | Column           | Type      | Description                                                            |
#'     | ---: | -----            | -----     | -----                                                                  |
#'     | 1    | `activity_drs`   | Character | Activity DRS (Data Reference Syntax)                                   |
#'     | 2    | `institution_id` | Character | Institution identifier                                                 |
#'     | 3    | `source_id`      | Character | Model identifier                                                       |
#'     | 4    | `experiment_id`  | Character | Root experiment identifier                                             |
#'     | 5    | `member_id`      | Character | A compound construction from `sub_experiment_id` and `variant_label`   |
#'     | 6    | `table_id`       | Character | Table identifier                                                       |
#'     | 7    | `lon`            | Double    | Longitude of extracted location                                        |
#'     | 8    | `lat`            | Double    | Latitude of extracted location                                         |
#'     | 9    | `dist`           | Double    | The spherical distance in km between EPW location and grid coordinates |
#'     | 10   | `datetime`       | POSIXct   | Datetime for the predicted value                                       |
#'     | 11   | `variable`       | Character | Variable identifier                                                    |
#'     | 12   | `description`    | Character | Variable long name                                                     |
#'     | 13   | `units`          | Character | Units of variable                                                      |
#'     | 14   | `value`          | Double    | The actual predicted value                                             |
#'
#' @examples
#' \dontrun{
#' coord <- match_coord("path_to_an_EPW")
#' extract_data(coord, years = 2030:2060)
#' }
#' @export
extract_data <- function (coord, years = NULL, unit = FALSE, out_dir = NULL,
                          by = NULL, keep = is.null(out_dir), compress = 100) {
    checkmate::assert_class(coord, "epw_cmip6_coord")

    # column names
    dict <- c(activity_drs = "activity", experiment_id = "experiment", member_id = "variant",
              table_id = "frequency", variable_id = "variable", source_id = "source",
              nominal_resolution = "resolution")

    # get matched coords
    m_coord <- coord$coord

    if (is.null(out_dir)) {
        m_coord <- list(m_coord)
    } else {
        checkmate::assert_directory_exists(out_dir, "w")
        checkmate::assert_subset(by, choices = dict)
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
    vmsg("Start to extract CMIP6 data according to matched coordinates...")

    data <- data.table::data.table()

    for (i in seq_along(m_coord)) {
        co <- m_coord[[i]]

        if (!is.null(out_dir) && length(by_cols)) {
            vmsg(sprintf("Extracting data for case '%s'...", names(m_coord)[[i]]))
        }
        progressr::with_progress({
            p <- progressr::progressor(nrow(co))

            d <- rbindlist(
                future.apply::future_Map(
                    function(ip, path, coord) {
                        p(message = sprintf("[%i/%i]", ip, nrow(co)))
                        d <- get_nc_data(path, coord, years = years, unit = unit)
                        set(d, NULL, "index", NULL)
                    },
                    seq.int(nrow(co)), co$file_path, co$coord
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

    # get file info
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
normalize_cf_calendar <- function (calendar) {
    if (!length(calendar) || is.na(calendar[[1L]]) || !nzchar(trimws(calendar[[1L]]))) {
        return("standard")
    }

    tolower(as.character(calendar[[1L]]))
}

get_nc_time_att <- function (atts, attribute, default = NULL) {
    idx <- which(atts$variable == "time" & atts$attribute == attribute)

    if (!length(idx)) {
        if (!is.null(default)) return(default)
        stop(sprintf("Missing NetCDF 'time' attribute: '%s'.", attribute), call. = FALSE)
    }

    atts$value[[idx[[1L]]]]
}

CF_TIME_CALENDARS <- c(
    "standard",
    "gregorian",
    "proleptic_gregorian",
    "noleap",
    "365_day",
    "360_day",
    "366_day",
    "all_leap"
)

CF_TIME_UNIT_SECONDS <- c(
    seconds = 1,
    minutes = 60,
    hours = 3600,
    days = 86400
)

cf_time_check_calendar <- function (calendar) {
    if (!calendar %in% CF_TIME_CALENDARS) {
        stop("Invalid calendar specification", call. = FALSE)
    }
    calendar
}

cf_time_parse_unit <- function (unit) {
    aliases <- c(
        years = "years",
        year = "years",
        yr = "years",
        months = "months",
        month = "months",
        mon = "months",
        days = "days",
        day = "days",
        d = "days",
        hours = "hours",
        hour = "hours",
        hr = "hours",
        h = "hours",
        minutes = "minutes",
        minute = "minutes",
        min = "minutes",
        seconds = "seconds",
        second = "seconds",
        sec = "seconds",
        s = "seconds"
    )
    raw_unit <- tolower(unit)
    unit <- aliases[[raw_unit]]
    if (is.null(unit)) {
        stop(sprintf("Unsupported CF time unit: %s", raw_unit), call. = FALSE)
    }
    unit
}

cf_time_is_leap_gregorian <- function (year) {
    (year %% 4L == 0L & year %% 100L != 0L) | year %% 400L == 0L
}

cf_time_month_days <- function (year, month, calendar) {
    common <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    leap <- c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

    switch(
        calendar,
        "360_day" = rep(30L, length(month)),
        "365_day" = common[month],
        "noleap" = common[month],
        "366_day" = leap[month],
        "all_leap" = leap[month],
        ifelse(cf_time_is_leap_gregorian(year), leap[month], common[month])
    )
}

cf_time_valid_days <- function (parts, calendar) {
    valid <- !is.na(parts$year) & !is.na(parts$month) & !is.na(parts$day) &
        parts$month >= 1L & parts$month <= 12L & parts$day >= 1L
    days <- cf_time_month_days(parts$year, parts$month, calendar)
    valid & !is.na(days) & parts$day <= days
}

cf_time_parse_origin <- function (origin, calendar) {
    pattern <- paste0(
        "^\\s*",
        "([+-]?[0-9]{1,4})",
        "(?:-(0?[1-9]|1[0-2]))?",
        "(?:-(0?[1-9]|[12][0-9]|3[01]))?",
        "(?:[T ]",
        "([01]?[0-9]|2[0-3])",
        "(?::([0-5]?[0-9]))?",
        "(?::([0-5]?[0-9](?:\\.[0-9]+)?))?",
        ")?",
        "(?:\\s*(Z|UTC|[+-][0-9]{2}(?::?[0-9]{2})?))?",
        "\\s*$"
    )
    proto <- data.frame(
        year = integer(),
        month = integer(),
        day = integer(),
        hour = integer(),
        minute = integer(),
        second = numeric(),
        zone = character()
    )
    parts <- utils::strcapture(pattern, trimws(origin), proto)

    if (is.na(parts$year[[1L]])) {
        stop(
            "Definition string does not appear to be a CF-compliant time coordinate description: invalid base date specification",
            call. = FALSE
        )
    }

    parts$month[is.na(parts$month)] <- 1L
    parts$day[is.na(parts$day)] <- 1L
    parts$hour[is.na(parts$hour)] <- 0L
    parts$minute[is.na(parts$minute)] <- 0L
    parts$second[is.na(parts$second)] <- 0
    parts$tz <- ifelse(is.na(parts$zone), "+0000", parts$zone)
    parts$zone <- NULL

    if (!cf_time_valid_days(parts, calendar)) {
        stop(
            "Definition string does not appear to be a CF-compliant time coordinate description: invalid base date specification",
            call. = FALSE
        )
    }

    parts
}

cf_time_parse_definition <- function (units, calendar) {
    units <- as.character(units[[1L]])
    pattern <- "^\\s*([[:alpha:]]+)\\s+(since|after|from|ref|per)\\s+(.+?)\\s*$"
    match <- regexec(pattern, units, ignore.case = TRUE)
    parts <- regmatches(units, match)[[1L]]

    if (length(parts) != 4L) {
        stop("Definition string does not appear to be a CF-compliant time coordinate description", call. = FALSE)
    }

    list(
        unit = cf_time_parse_unit(parts[[2L]]),
        origin = cf_time_parse_origin(parts[[4L]], calendar)
    )
}

cf_time_gregorian_date2offset <- function (parts) {
    year1 <- parts$year - 1L
    corr <- ifelse(parts$month <= 2L, 0L, as.integer(cf_time_is_leap_gregorian(parts$year)) - 2L)
    365L * year1 + year1 %/% 4L - year1 %/% 100L + year1 %/% 400L +
        (367L * parts$month - 362L) %/% 12L + corr + parts$day
}

cf_time_gregorian_offset2date <- function (offsets) {
    d0 <- offsets - 1L
    n400 <- d0 %/% 146097L
    d1 <- d0 %% 146097L
    n100 <- d1 %/% 36524L
    d2 <- d1 %% 36524L
    n4 <- d2 %/% 1461L
    d3 <- d2 %% 1461L
    n1 <- d3 %/% 365L
    year <- 400L * n400 + 100L * n100 + 4L * n4 + n1
    year <- ifelse(n100 == 4L | n1 == 4L, year, year + 1L)
    year1 <- year - 1L
    leap <- cf_time_is_leap_gregorian(year)
    jan1 <- 365L * year1 + year1 %/% 4L - year1 %/% 100L + year1 %/% 400L + 1L
    prior_days <- offsets - jan1 + ifelse(offsets < jan1 + 59L + as.integer(leap), 0L, 2L - as.integer(leap))
    month <- (12L * prior_days + 373L) %/% 367L
    day <- offsets - cf_time_gregorian_date2offset(data.frame(year = year, month = month, day = 1L)) + 1L

    data.frame(year = year, month = month, day = day)
}

cf_time_365_date2offset <- function (parts) {
    corr <- ifelse(parts$month <= 2L, 0L, -2L)
    365L * (parts$year - 1L) + (367L * parts$month - 362L) %/% 12L + corr + parts$day
}

cf_time_365_offset2date <- function (offsets) {
    d0 <- offsets - 1L
    year <- d0 %/% 365L + 1L
    d1 <- d0 %% 365L
    corr <- ifelse(d1 < 59L, 0L, 2L)
    month <- (12L * (d1 + corr) + 373L) %/% 367L
    day <- d1 - (367L * month - 362L) %/% 12L + corr + 1L

    data.frame(year = year, month = month, day = day)
}

cf_time_366_date2offset <- function (parts) {
    corr <- ifelse(parts$month <= 2L, 0L, -1L)
    366L * (parts$year - 1L) + (367L * parts$month - 362L) %/% 12L + corr + parts$day
}

cf_time_366_offset2date <- function (offsets) {
    d0 <- offsets - 1L
    year <- d0 %/% 366L + 1L
    d1 <- d0 %% 366L
    corr <- ifelse(d1 < 60L, 0L, 1L)
    month <- (12L * (d1 + corr) + 373L) %/% 367L
    day <- d1 - (367L * month - 362L) %/% 12L + corr + 1L

    data.frame(year = year, month = month, day = day)
}

cf_time_date2offset <- function (parts, origin, calendar) {
    switch(
        calendar,
        "360_day" = (parts$year - origin$year) * 360L +
            (parts$month - origin$month) * 30L + parts$day - origin$day,
        "365_day" = cf_time_365_date2offset(parts) - cf_time_365_date2offset(origin),
        "noleap" = cf_time_365_date2offset(parts) - cf_time_365_date2offset(origin),
        "366_day" = cf_time_366_date2offset(parts) - cf_time_366_date2offset(origin),
        "all_leap" = cf_time_366_date2offset(parts) - cf_time_366_date2offset(origin),
        cf_time_gregorian_date2offset(parts) - cf_time_gregorian_date2offset(origin)
    )
}

cf_time_offset2date <- function (offsets, origin, calendar) {
    switch(
        calendar,
        "360_day" = {
            year <- origin$year + offsets %/% 360L
            month <- origin$month + (offsets %% 360L) %/% 30L
            day <- origin$day + offsets %% 30L

            over <- day > 30L
            day[over] <- day[over] - 30L
            month[over] <- month[over] + 1L
            over <- month > 12L
            month[over] <- month[over] - 12L
            year[over] <- year[over] + 1L

            data.frame(year = year, month = month, day = day)
        },
        "365_day" = cf_time_365_offset2date(offsets + cf_time_365_date2offset(origin)),
        "noleap" = cf_time_365_offset2date(offsets + cf_time_365_date2offset(origin)),
        "366_day" = cf_time_366_offset2date(offsets + cf_time_366_date2offset(origin)),
        "all_leap" = cf_time_366_offset2date(offsets + cf_time_366_date2offset(origin)),
        cf_time_gregorian_offset2date(offsets + cf_time_gregorian_date2offset(origin))
    )
}

cf_time_origin_posix <- function (origin, tz) {
    ISOdatetime(origin$year[[1L]], origin$month[[1L]], 1L, 0L, 0L, 0, tz = tz) +
        (origin$day[[1L]] - 1L) * 86400 +
        origin$hour[[1L]] * 3600 +
        origin$minute[[1L]] * 60 +
        origin$second[[1L]]
}

cf_time_add_seconds_to_fields <- function (fields, seconds, origin, calendar) {
    total_seconds <- fields$hour * 3600 + fields$minute * 60 + fields$second + seconds
    day_offsets <- cf_time_date2offset(fields, origin, calendar) + total_seconds %/% 86400L
    seconds <- round(total_seconds %% 86400L, 3L)

    fields <- cf_time_offset2date(day_offsets, origin, calendar)
    fields$hour <- seconds %/% 3600L
    fields$minute <- (seconds %% 3600L) %/% 60L
    fields$second <- seconds %% 60L
    fields
}

cf_time_offsets2fields <- function (offsets, unit, origin, calendar) {
    if (unit %in% names(CF_TIME_UNIT_SECONDS)) {
        seconds <- offsets * CF_TIME_UNIT_SECONDS[[unit]] +
            origin$hour * 3600 + origin$minute * 60 + origin$second
        day_offsets <- seconds %/% 86400L
        seconds <- round(seconds %% 86400L, 3L)

        fields <- cf_time_offset2date(day_offsets, origin, calendar)
        fields$hour <- seconds %/% 3600L
        fields$minute <- (seconds %% 3600L) %/% 60L
        fields$second <- seconds %% 60L
        return(fields)
    }

    whole_offsets <- offsets %/% 1L
    fractional_offsets <- offsets - whole_offsets
    fields <- origin[rep(1L, length(offsets)), c("year", "month", "day", "hour", "minute", "second")]
    if (unit == "months") {
        months <- fields$month + whole_offsets - 1L
        fields$month <- months %% 12L + 1L
        fields$year <- fields$year + months %/% 12L
        fractional_seconds <- fractional_offsets * 30 * 86400
    } else {
        fields$year <- fields$year + whole_offsets
        fractional_seconds <- fractional_offsets * 365 * 86400
    }

    if (any(fractional_seconds != 0)) {
        fields <- cf_time_add_seconds_to_fields(fields, fractional_seconds, origin, calendar)
    }

    fields
}

cf_time_fields2posix <- function (fields, origin, calendar, tz) {
    if (calendar %in% c("standard", "gregorian", "proleptic_gregorian")) {
        return(as.POSIXct(
            ISOdatetime(fields$year, fields$month, fields$day, fields$hour, fields$minute, fields$second, tz = tz),
            tz = tz
        ))
    }

    origin_time <- cf_time_origin_posix(origin, tz)
    day_offsets <- cf_time_date2offset(fields, origin, calendar)
    second_offsets <- fields$hour * 3600 + fields$minute * 60 + fields$second -
        (origin$hour[[1L]] * 3600 + origin$minute[[1L]] * 60 + origin$second[[1L]])

    as.POSIXct(origin_time + day_offsets * 86400 + second_offsets, tz = tz)
}

parse_cf_time <- function (offsets, units, calendar = "standard", tz = "UTC") {
    calendar <- normalize_cf_calendar(calendar)
    calendar <- cf_time_check_calendar(calendar)
    units <- as.character(units[[1L]])

    if (!is.numeric(offsets) || anyNA(offsets) || any(!is.finite(offsets))) {
        stop("Invalid offsets for CF time coordinate.", call. = FALSE)
    }
    dim(offsets) <- NULL

    parsed <- cf_time_parse_definition(units, calendar)
    fields <- cf_time_offsets2fields(offsets, parsed$unit, parsed$origin, calendar)
    posix_time <- cf_time_fields2posix(fields, parsed$origin, calendar, tz)

    data.table::setattr(posix_time, "cf_units", units)
    data.table::setattr(posix_time, "cf_calendar", calendar)
    posix_time
}

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

    time_units <- get_nc_time_att(atts, "units")
    time_calendar <- get_nc_time_att(atts, "calendar", default = "standard")

    if (range) {
        offsets <- c(
            RNetCDF::var.get.nc(nc, "time", 1, 1),
            RNetCDF::var.get.nc(nc, "time", n, 1)
        )
    } else {
        offsets <- RNetCDF::var.get.nc(nc, "time", 1, n)
    }

    parse_cf_time(offsets, time_units, time_calendar)
}
# }}}

# match_nc_time {{{
match_nc_time <- function (x, years = NULL) {
    checkmate::assert_integerish(years, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

    time <- get_nc_time(x)

    if (is.null(years)) {
        list(datetime = list(time), which = list(seq_along(time)))
    } else {
        y <- data.table::year(time)
        i <- lapply(as.integer(years), function (x) which(y == x))

        j <- 1L
        l <- vector("list", length(i))
        l[[1L]] <- i[[1L]]
        for (m in i[-1L]) {
            if (!length(m)) next
            if (length(l[[j]]) && (l[[j]][length(l[[j]])] + 1L) == m[1L]) {
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

# nocov start
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
# nocov end
