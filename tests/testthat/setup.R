get_cache_epw()
get_cache_nc()
get_cache_parquet()

# Extraction tests exercise the content-addressed cache added for cross-method
# reuse. Keep those disposable entries inside the test session so package
# checks never write to, or reuse state from, the user's normal cache.
test_cache_dir <- tempfile("epwshiftr-test-cache-")
old_cache_option <- options("epwshiftr.dir_cache" = test_cache_dir)
withr::defer(
    {
        options(old_cache_option)
        if (dir.exists(test_cache_dir)) {
            unlink(test_cache_dir, recursive = TRUE, force = TRUE)
        }
    },
    envir = testthat::teardown_env()
)

if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    old_verbose <- options("epwshiftr.verbose" = TRUE)

    withr::defer(
        {
            options(old_verbose)
        },
        envir = testthat::teardown_env()
    )
}
