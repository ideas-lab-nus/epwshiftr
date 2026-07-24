#!/usr/bin/env Rscript

# Keep the repository command stable while the installed implementation lives
# under inst/tools so package and coverage tests can locate it reliably.
file_arg <- grep(
    "^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(file_arg)) {
    stop("Cannot resolve the daily CMIP6 probe wrapper.", call. = FALSE)
}
wrapper <- normalizePath(
    sub("^--file=", "", file_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
)
implementation <- normalizePath(
    file.path(
        dirname(wrapper),
        "..",
        "inst",
        "tools",
        basename(wrapper)
    ),
    winslash = "/",
    mustWork = TRUE
)
environment <- new.env(parent = globalenv())
sys.source(implementation, envir = environment)
environment$daily_probe__main()
