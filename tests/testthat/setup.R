if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    test_cache_dir <- tempfile("epwshiftr-test-cache-", tmpdir = tempdir())
    Sys.setenv(EPWSHIFTR_CHECK_CACHE = test_cache_dir)

    withr::defer(
        {
            unlink(test_cache_dir, recursive = TRUE)
            Sys.unsetenv("EPWSHIFTR_CHECK_CACHE")
        },
        envir = testthat::teardown_env()
    )
}
