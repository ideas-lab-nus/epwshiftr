# get_nc_meta {{{
#' @importFrom data.table setattr setDT
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
    recursive = FALSE)
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

    ncmeta <- data.table::rbindlist(lapply(ncfiles, get_nc_meta))
    ncmeta[, `:=`(file_path = ncfiles, file_size = file.size(ncfiles), file_mtime = file.mtime(ncfiles))]

    # add variable units, time units and file paths
    idx[ncmeta, on = "tracking_id",
        `:=`(file_path = i.file_path, file_realsize = i.file_size, file_mtime = i.file_mtime,
             time_units = i.time_units, time_calendar = i.time_units)
    ]

    # update index database
    EPWSHIFTR_ENV$index_db <- copy(idx)

    by_cols <- names(dict)[which(dict %in% by)]

    sm <- idx[, list(
        datetime_start = min(datetime_start),
        datetime_end = max(datetime_end),
        file_num = .N,
        file_size = round(units::set_units(units::set_units(sum(file_size, na.rm = TRUE), "bytes"), "Mbytes")),
        dl_num = sum(!is.na(file_path)),
        dl_percent = round(units::set_units(sum(!is.na(file_path)) / .N, "%"), 3),
        dl_size = round(units::set_units(units::set_units(sum(file_realsize, na.rm = TRUE), "bytes"), "Mbytes"))
    ), by = c(by_cols)][order(-dl_percent)]

    sm
}
