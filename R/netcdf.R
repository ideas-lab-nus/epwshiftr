# get_nc_meta {{{
#' @importFrom data.table setDT setattr
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
#' `summary_database()` scans the directory specified and returns a
#' [data.table()] containing summary information about all the CMIP6
#' files available against the output file index loaded using
#' [load_cmip6_index()].
#'
#' The database here can be any directory that stores the NetCDF files for CMIP6
#' GCMs. It can be also be the same as `get_data_dir()` where epwshiftr stores
#' the output file index, if you want to save the output file index and output
#' files in the same place.
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
#'        current loaded index will be updated, but the actual index
#'        database file saved in [get_data_dir()] will remain unchanged.
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
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress
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

    assert_directory_exists(dir, "w")
    assert_subset(by, choices = dict)

    mult <- match.arg(mult)
    miss <- match.arg(miss)

    # load index
    idx <- load_cmip6_index()

    # find all nc files in specified directory
    ncfiles <- list.files(dir, "[.](nc)|(hdf)$", full.names = TRUE, recursive = recursive)

    verbose(paste0("", length(ncfiles), " NetCDF files found."))

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

        left <- data.table()
        miss <- data.table()
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
    setattr(sm, "not_found", miss)

    sm
}
# }}}

# get_nc_data {{{
#' @importFrom data.table as.data.table set setattr setcolorder
#' @importFrom RNetCDF utcal.nc
#' @importFrom units set_units
get_nc_data <- function (x, coord, years, unit = TRUE) {
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
            return(data.table(
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
        dt <- as.data.table(RNetCDF::var.get.nc(nc, var,
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
#' `extract_data()` supports common calendars, including `365_day` and
#' `360_day`, thanks to the [PCICt][PCICt::as.PCICt] package.
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
    verbose("Start to extract CMIP6 data according to matched coordinates...")

    data <- data.table()

    for (i in seq_along(m_coord)) {
        co <- m_coord[[i]]

        if (!is.null(out_dir) && length(by_cols)) {
            verbose("Extracting data for case '", names(m_coord)[[i]], "'...")
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
#' @importFrom PCICt as.PCICt
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

    # get calendar
    cal <- tolower(tolower(atts[variable == "time" & attribute == "calendar", value[[1L]]]))
    CALENDARS <- c("gregorian", "standard", "proleptic_gregorian", "noleap",
        "365_day", "all_leap", "366_day", "360_day")
    if (!cal %in% CALENDARS) {
        stop(sprintf("Unsupported calendar type found: '%s'.", cal))
    }

    # get time origin and units
    time_spec <- atts[variable == "time" & attribute == "units", value[[1L]]]
    time_units <- strsplit(time_spec, " ")[[1L]]
    time_res <- tolower(time_units[1L])

    if (length(time_units) >= 4L) {
        time_ori_str <- paste(time_units[3L], time_units[4L:length(time_units)])
    } else if (length(time_units) == 3L) {
        time_ori_str <- time_units[3L]
    } else {
        stop(sprintf("Invalid time units found: '%s'.", time_spec))
    }
    # PCICt only handle standard format string
    if (time_res %in% c("months", "month")) {
        time_ori_str <- paste0(time_ori_str, "-01")
        warning("Month time resolution found. Seconds in a month will be set to 86400 (seconds in day) * 30 (day). ",
            "Time conversion may be not accurate.")
    }
    time_ori <- PCICt::as.PCICt.default(time_ori_str, cal = cal)

    secs <- switch(time_res,
        second = 1, seconds = 1,
        minute = 60, minutes = 60,
        hour = 3600, hours = 3600,
        day = 86400, days = 86400,
        month = 86400 * 30, months = 86400 * 30,
        stop(sprintf("Unsupported time resolution found: '%s'. Should be one of [%s]",
            time_res, paste0("'", c("second(s)", "minute(s)", "hour(s)", "day(s)", "month(s)"), "'", collapse = ", ")
        ))
    )

    if (range) {
        time_num_start <- RNetCDF::var.get.nc(nc, "time", 1, 1)
        time_num_end <- RNetCDF::var.get.nc(nc, "time", n, 1)
        as.POSIXct(time_ori + c(time_num_start, time_num_end) * secs)
    } else {
        # create time sequences
        time_num <- RNetCDF::var.get.nc(nc, "time", 1, n)
        as.POSIXct(time_ori + time_num * secs)
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
