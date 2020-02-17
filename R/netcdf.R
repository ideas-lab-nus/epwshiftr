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
#' @importFrom ncmeta nc_atts
get_nc_meta <- function (file) {
    # to avoid No visible binding for global variable check NOTE
    J <- value <- name <- NULL
    verbose("Parsing meta data of NetCDF file '", file, "'...")
    # get all attributes
    atts <- data.table::setDT(ncmeta::nc_atts(file))

    # get meta data
    meta <- as.list(atts[
        J("NC_GLOBAL",
          c("mip_era", "activity_id", "institution_id", "source_id",
            "experiment_id", "variant_label", "table_id", "grid_label",
            "nominal_resolution", "variable_id", "tracking_id")
        ),
        on = c("variable", "name"),
        {v <- unlist(value); names(v) <- name; v}])

    # get variable long name and units
    meta <- c(
        meta,
        data.table::setattr(
            atts[J(meta$variable_id, c("standard_name", "units")), on = c("variable", "name"), value],
            "names", c("standard_name", "units")
        )
    )

    # get time origin and unit
    c(
        meta,
        data.table::setattr(
            atts[J("time", c("units", "calendar")), on = c("variable", "name"), value],
            "names", c("time_units", "time_calendar")
        )
    )
}
# }}}

# summary_database {{{
#' Summary CMIP6 model output file status
#'
#' `summary_database()` scan the directory specified and returns a
#' [data.table::data.table()] containing summary information about all the CMIP6
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
#' @param recursive If `TRUE`, scan recursively into directories. Default:
#'        `FALSE`.
#'
#' @param update If `TRUE`, the output file index database will be updated based
#'        on the matched NetCDF files in specified directory. Default: `TRUE`.
#'
#' @return A [data.table::data.table()] containing corresponding grouping
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
    recursive = FALSE, update = TRUE)
{
    # column names
    dict <- c(activity_drs = "activity", experiment_id = "experiment", member_id = "variant",
              table_id = "frequency", variable_id = "variable", source_id = "source",
              nominal_resolution = "resolution")

    assert_directory_exists(dir, "w")
    assert_subset(by, empty.ok = FALSE, choices = dict)

    # load index database
    idx <- load_cmip6_index()

    # find all nc files in specified directory
    ncfiles <- list.files(dir, "\\.nc$", full.names = TRUE, recursive = recursive)

    p <- progress::progress_bar$new(format = "[:current/:total][:bar] :percent [:elapsedfull]",
        total = length(ncfiles), clear = FALSE)

    p$message(paste0("", length(ncfiles), " NetCDF files found."))

    if (!length(ncfiles)) return(invisible())

    ncmeta <- data.table::rbindlist(lapply(ncfiles, function (f) {
        p$message(paste0("Processing file ", f, "..."))
        p$tick()
        get_nc_meta(f)
    }))

    ncmeta[, `:=`(file_path = ncfiles, file_size = file.size(ncfiles), file_mtime = file.mtime(ncfiles))]

    # reset existing columns
    idx[, `:=`(file_path = NA_character_, file_realsize = NA_real_,
               file_mtime = NA_real_, time_units = NA_character_,
               time_calendar = NA_character_)]

    # add variable units, time units and file paths
    idx[ncmeta, on = "tracking_id",
        `:=`(file_path = i.file_path, file_realsize = i.file_size, file_mtime = i.file_mtime,
             time_units = i.time_units, time_calendar = i.time_units)
    ]

    # update index database
    EPWSHIFTR_ENV$index_db <- copy(idx)

    if (update) {
        # save database into the app data directory
        data.table::fwrite(idx, file.path(.data_dir(TRUE), "cmip6_index.csv"))
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
#' @importFrom tidync tidync hyper_tibble hyper_filter
#' @importFrom data.table as.data.table set setattr setcolorder setDT
#' @importFrom RNetCDF utcal.nc
#' @importFrom ncmeta nc_att
#' @importFrom units set_units
#' @export
get_nc_data <- function (path, lons, lats, unit = TRUE) {
    assert_flag(unit)

    dt <- tidync::hyper_tibble(tidync::hyper_filter(tidync::tidync(path),
        lon = lon %in% lons, lat = lat %in% lats))
    data.table::setDT(dt)

    # get variables
    vars <- data.table::setDT(ncmeta::nc_vars(path))
    # get variable attributes
    atts <- data.table::setDT(ncmeta::nc_atts(path))[variable %in% vars$name]

    # get time origin and units
    ori <- atts[variable == "time" & name == "units", value[[1L]]]

    # add datetime and its components
    data.table::set(dt, NULL, "datetime", RNetCDF::utcal.nc(ori, dt$time, "c"))
    comp <- RNetCDF::utcal.nc(ori, dt$time, "n")
    data.table::set(dt, NULL, dimnames(comp)[[2L]],
        data.table::as.data.table(comp)[, lapply(.SD, as.integer)]
    )
    data.table::set(dt, NULL, "time", NULL)

    # extract meta attributes
    glo_atts <- data.table::setDT(ncmeta::nc_atts(path, "NC_GLOBAL"))

    # get the variable name
    var <- glo_atts[name == "variable_id", value[[1L]]]
    # get long name
    var_long <- atts[J(var, "long_name"), on = c("variable", "name"), value[[1L]]]

    # assign units
    units <- atts[variable == var & name == "units", value[[1L]]]
    if (unit) {
        data.table::set(dt, NULL, var, units::set_units(dt[[var]], units, mode = "standard"))
    }

    # change to tidy format
    data.table::setnames(dt, var, "value")
    data.table::set(dt, NULL, c("variable", "description", "units"), list(var, var_long, units))

    # change column order
    data.table::setcolorder(dt, c("datetime", dimnames(comp)[[2L]], "lon", "lat"))
    data.table::setcolorder(dt, setdiff(names(dt), c(var, var_long, "units", "value")))

    data.table::set(dt, NULL,
        c("activity_drs", "experiment_id", "institution_id", "source_id", "member_id", "table_id"),
        glo_atts[J(c("activity_id", "experiment_id", "institution_id", "source_id", "variant_label", "frequency")), on = "name", value]
    )
    data.table::setcolorder(dt, c("activity_drs", "experiment_id", "institution_id", "source_id", "member_id", "table_id"))

    dt
}
# }}}

# extract_data {{{
#' Extract data
#'
#' @param locations Location data extracted using [match_location()]
#' @param unit If `TRUE`, units will be added to values
#' @param save If `TRUE`, a [fst][fst::write_fst()] file will be written
#' @param overwrite If `TRUE`, overwrite the fst file if exists
#'
#' @return A data.table
#' @importFrom checkmate assert_class
#' @export
extract_data <- function (locations, unit = FALSE, dir = NULL, overwrite = FALSE) {
    assert_class(locations, "epw_coords")

    loc <- locations$coord

    p <- progress::progress_bar$new(
        format = "[:current/:total][:bar] :percent [:elapsedfull]",
        total = nrow(loc), clear = FALSE)

    data <- Map(
        function (path, coord) {
            p$message(paste0("Processing file ", basename(path), "..."))
            if (!is.null(dir)) {
                f <- paste0(tools::file_path_sans_ext(path), ".fst")
                p$message(paste0("Saving file ", f, "..."))
                if (!overwrite && file.exists(f)) {
                    p$message(paste0("File exists. Skip..."))
                    return(normalizePath(f))
                }
            }
            d <- get_nc_data(path, lons = coord$longitude$lon, lats = coord$latitude$lat, unit = unit)
            p$tick()
            if (is.null(dir)) return(d)
            fst::write_fst(d, f)
        },
        loc$file_path, loc$coord
    )

    data.table::set(loc, NULL, "data", data)

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
        tryCatch(curl::curl_download(url, f, quiet = FALSE),
            error = function (e) NULL
        )
        f
    })
}
# }}}
