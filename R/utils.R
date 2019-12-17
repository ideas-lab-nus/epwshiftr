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
#' Get package data storage directory using [rappdirs::user_data_dir()].
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
        d <- normalizePath(rappdirs::user_data_dir(appauthor = "epwshiftr"), mustWork = FALSE)
    } else {
        # make sure user specified directory exists
        d <- normalizePath(d, mustWork = FALSE)
        init <- FALSE
        force <- TRUE
    }

    if (init && !dir.exists(d)) {
        verbose(sprintf("Creating %s package data storage directory '%s'...", "epwshiftr", d))
        dir.create(d, recursive = TRUE)
    }

    if ((init || force) && !test_directory_exists(d, "rw")) {
        stop(sprintf("%s package data storage directory %s does not exists or writable.",
            "epwshiftr", d
        ))
    }

    d
}
# }}}
