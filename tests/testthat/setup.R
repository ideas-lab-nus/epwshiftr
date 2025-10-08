if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    test_cache_dir <- tempfile("epwshiftr-test-cache-", tmpdir = tempdir())
    old_verbose <- options("epwshiftr.verbose" = TRUE)
    Sys.setenv(EPWSHIFTR_CHECK_CACHE = test_cache_dir)

    withr::defer(
        {
            unlink(test_cache_dir, recursive = TRUE)
            options(old_verbose)
            Sys.unsetenv("EPWSHIFTR_CHECK_CACHE")
        },
        envir = testthat::teardown_env()
    )
}
