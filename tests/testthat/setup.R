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

# duckdb prints a storage-location notice from duckdb() until it has shown it
# 60 times in a process. A serial run uses the allowance up early, but a fresh
# test process, such as a parallel worker or one file run alone, prints it into
# whatever output the first test captures. Giving duckdb an explicit home
# removes the notice, and keeps tests from touching the user's ~/.duckdb.
test_duckdb_home <- tempfile("epwshiftr-test-duckdb-")
old_duckdb_option <- options("duckdb.home" = test_duckdb_home)
withr::defer(
    {
        options(old_duckdb_option)
        if (dir.exists(test_duckdb_home)) {
            unlink(test_duckdb_home, recursive = TRUE, force = TRUE)
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

# vim: fdm=marker :
