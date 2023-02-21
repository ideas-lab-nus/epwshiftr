lpad <- function(x, pad = " ", width = NULL) {
    wid <- nchar(x, "width")
    if (is.null(width)) width <- max(wid)
    paste0(strrep(pad, pmax(width - wid, 0)), x)
}

# a little bit faster
unlst <- function(x) unlist(x, FALSE, FALSE)

trim_ws <- function(x) sub("\\s*$", "", sub("^\\s*", "", x))

to_title_case <- function(x) {
    sub("(.)", "\\U\\1", gsub("_", " ", x, fixed = TRUE), perl = TRUE)
}

priv <- function(x) {
    checkmate::assert_r6(x)
    x$.__enclos_env__[["private"]]
}

verbose <- function (..., sep = "") {
    if (getOption("epwshiftr.verbose", FALSE)) {
        cat(..., "\n", sep = sep)
    }
}

vb <- function(expr) {
    if (!getOption("epwshiftr.verbose", FALSE)) return()
    force(expr)
}

eval_with_bang <- function(..., .env = parent.frame()) {
    l <- eval(substitute(alist(...)))
    checkmate::assert_list(l, .var.name = "Input", min.len = 1L)
    lapply(l, function(elem) {
        if (!is.symbol(elem) && !is.null(elem) && elem[[1L]] == "!") {
            negate <- TRUE
            elem[[1L]] <- as.name("c")
        } else {
            negate <- FALSE
        }

        list(value = eval(elem, .env), negate = negate)
    })
}

now <- function(tz = "UTC") {
    t <- Sys.time()
    attr(t, "tzone") <- tz
    t
}

# nocov start
rd_query_is_facets <- function(x) {
    x %in% c("facets", "fields", "shards", "replica", "latest", "type", "limit", "offset", "distrib")
}

rd_query_method_param <- function(method, type, negate, default, nullable = TRUE) {
    val_quote <- if (grepl("character|string", type)) '"' else ""
    def_quote <- if (!missing(default) && is.null(default)) "" else val_quote
    rd <- c(
        paste(
            paste("The", if (rd_query_is_facets(method)) "facet" else "", "parameter value."),
            if (!missing(default)) {
                sprintf("Default: \\code{%s%s%s}.", def_quote, if (is.null(default)) "NULL" else default, def_quote)
            }
        ),

        "There are two options:",
        "\\itemize{",
        "\\item If \\code{value} is not given, current value is returned.",
        paste(
            sprintf("\\item %s %s%s.", if (type == "integer") "An" else "A", type, if (nullable) " or \\code{NULL}" else ""),
        if (method %in% c("fields", "facets")) {
            sprintf(
                paste(
                    "The special notation \\code{\"*\"} can be used to indicate that",
                    "all available %s should be considered."
                ),
                method
            )
        },
        if (!missing(negate)) {
            sprintf(
                paste(
                    "Note that you can put a preceding \\code{!} to negate the facet constraints.",
                    "For example, \\code{$%s(!c(%s))} searches for all \\code{%s}s except for",
                    "%s."
                ),
                method, paste0(val_quote, negate, val_quote, collapse = ", "),
                method, paste0("\\code{", negate, "}", collapse = " and ")
            )
        }),
        "}"
    )

    paste(rd, collapse = "\n")
}

rd_query_method_return <- function() {
    paste(
        "\\itemize{",
        "\\item If \\code{value} is given, the modified \\code{EsgfQuery} object.",
        paste(
            "\\item Otherwise, an \\code{EsgfQueryParam} object which is essentially a list of three elements:",
            "\\itemize{",
                "\\item \\code{value}: input values.",
                "\\item \\code{negate}: Whether there is a preceding \\code{!}.",
                "\\item \\code{name}: Parameter name.",
            "}"
        ),
        "}"
    )
}
# nocov end

set_size_units <- function(x) {
    if (!length(x)) return(NULL)
    base <- 1024L
    iec <- c("Byte", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
    if (!inherits(x, "units")) {
        x <- units::set_units(x, iec[[1L]], mode = "standard")
    } else {
        bytes <- try(units::set_units(x, iec[[1L]], mode = "standard"), silent = TRUE)
        if (inherits(bytes, "try-error")) {
            warning("Failed to set input units to 'Byte'. Conversion skipped.")
            return(x)
        }
        x <- bytes
    }

    power <- log(units::drop_units(x), base = base)
    power[is.infinite(power)] <- 0
    power <- min(as.integer(power), length(iec))
    units::set_units(x, iec[power + 1L], mode = "standard")
}

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
#' @noRd
.data_dir <- function (init = FALSE, force = TRUE) {
    checkmate::assert_flag(init)
    checkmate::assert_flag(force)

    d <- getOption("epwshiftr.dir", NULL)
    if (is.null(d)) {
        # nocov start
        if (.Platform$OS.type == "windows") {
            d <- normalizePath(rappdirs::user_data_dir(appauthor = "epwshiftr"), mustWork = FALSE)
        } else {
            d <- normalizePath(rappdirs::user_data_dir(appname = "epwshiftr"), mustWork = FALSE)
        }
        # nocov end

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

    if ((init || force) && !checkmate::test_directory_exists(d, "rw")) {
        stop(sprintf("%s package data storage directory '%s' does not exists or is not writable.",
            "epwshiftr", d
        ))
    }

    d
}

# get rid of R CMD check NOTEs on global variables
utils::globalVariables(c(
    ".BY", ".I", ".N", ".SD", "J", ".GRP", "activity_drs", "alpha", "attribute",
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
    "interval", "time_calendar", "time_units", "overlap", "frequency",
    "ind_lon", "ind_lat", "ord_lon", "ord_lat", "dist", "num_years", "type",
    "year"
))
