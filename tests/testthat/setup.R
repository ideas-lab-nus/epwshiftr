get_cache_epw()

if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    old_verbose <- options("epwshiftr.verbose" = TRUE)

    withr::defer(
        {
            options(old_verbose)
        },
        envir = testthat::teardown_env()
    )
}
