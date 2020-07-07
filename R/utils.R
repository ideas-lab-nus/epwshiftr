# verbose {{{
verbose <- function (..., sep = "") {
    if (getOption("epwshiftr.verbose", FALSE)) {
        cat(..., "\n", sep = sep)
    }
}
# }}}

# .data_dir {{{
#' Get the package data storage directory
#'
#' If option `epwshiftr.dir` is set, use it. Otherwise, get package data storage
#' directory using [rappdirs::user_data_dir()].
#'
#' @param init If TRUE, the directory will be created if not exists.
#' @param force If TRUE, issue an error if the directory does not exist.
#'
#' @return A single string indicating the directory location.
#'
#' @importFrom rappdirs user_data_dir
#' @importFrom checkmate test_directory_exists
#' @noRd
.data_dir <- function (init = FALSE, force = TRUE) {
    d <- getOption("epwshiftr.dir", NULL)
    if (is.null(d)) {
        if (.Platform$OS.type == "windows") {
            d <- normalizePath(rappdirs::user_data_dir(appauthor = "epwshiftr"), mustWork = FALSE)
        } else {
            d <- normalizePath(rappdirs::user_data_dir(appname = "epwshiftr"), mustWork = FALSE)
        }

        if (init && !dir.exists(d)) {
            verbose(sprintf("Creating %s package data storage directory '%s'", "epwshiftr", d))
            dir.create(d, recursive = TRUE)
        }
    } else {
        # make sure user specified directory exists
        d <- normalizePath(d, mustWork = FALSE)
        init <- FALSE
        force <- TRUE
    }

    if ((init || force) && !test_directory_exists(d, "rw")) {
        stop(sprintf("%s package data storage directory '%s' does not exists or is not writable.",
            "epwshiftr", d
        ))
    }

    d
}
# }}}

# get rid of R CMD check NOTEs on global variables
utils::globalVariables(c(
    ".BY", ".I", ".N", ".SD", "J", "activity_drs", "alpha", "attribute",
    "country", "data_node", "datetime", "datetime_end", "datetime_start",
    "day_of_year", "degree_Celsius", "delta", "dew_point_temperature",
    "diffuse_horizontal_radiation", "direct_normal_radiation", "dl_percent",
    "dry_bulb_temperature", "epw_max", "epw_mean", "epw_min", "experiment_id",
    "file_id", "file_path", "file_realsize", "file_size",
    "global_horizontal_radiation",
    "horizontal_infrared_radiation_intensity_from_sky", "hour",
    "i.datetime_end", "i.datetime_start", "i.diffuse_horizontal_radiation",
    "i.opaque_sky_cover", "i.relative_humidity", "i.val_max", "i.val_mean",
    "i.val_min", "i.value", "id", "index", "index_case", "institution_id",
    "lat", "latitude", "location", "lon", "longitude", "member_id", "name",
    "natts", "opaque_sky_cover", "ping", "relative_humidity", "source_id",
    "source_type", "state_province", "table_id", "title", "total_sky_cover",
    "val_max", "val_mean", "val_min", "value", "value_max", "value_min",
    "variable", "wmo_number", "file_mtime", "i.file_path", "i.interval",
    "interval", "time_calendar", "time_units", "overlap", "frequency"
))
