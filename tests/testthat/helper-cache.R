# Get test data directory path (for EPW/NetCDF files, not DiskCache objects)
#
# Uses EPWSHIFTR_CHECK_CACHE env var in CI, otherwise a fixed subdir of tempdir().
# The directory is created if it doesn't exist.
test_data_dir <- function() {
    dir <- Sys.getenv("EPWSHIFTR_CHECK_CACHE", NA)
    if (is.na(dir)) {
        dir <- file.path(tempdir(), "epwshiftr-test-data")
    }
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    dir
}

get_cache_epw <- function() {
    dir <- test_data_dir()
    # The EPW fixture is tiny, so rewrite it to avoid stale CI cache content
    # from older fixture versions without adding schema/version probes.
    epwshiftr_example_epw(dir = dir, overwrite = TRUE)
}

get_cache_nc <- function(reset = FALSE) {
    dir <- test_data_dir()
    paths <- file.path(dir, vapply(local_cmip6_test_years, local_cmip6_nc_file, character(1)))

    if (reset) unlink(paths, force = TRUE)
    unlink(paths[file.exists(paths)], force = TRUE)

    for (i in seq_along(paths)) {
        write_local_cmip6_netcdf_fixture(paths[[i]], local_cmip6_test_years[[i]])
    }

    normalizePath(dir)
}

read_test_parquet <- function(path) {
    conn <- ddb_connect(":memory:")
    on.exit(ddb_disconnect(conn), add = TRUE)

    data.table::as.data.table(ddb_query(conn, sprintf(
        "SELECT * FROM read_parquet(%s)",
        ddb_literal(conn, path)
    )))
}

get_cache_parquet <- function(reset = FALSE) {
    dir <- get_cache_nc(reset = reset)
    path <- file.path(dir, "EC-Earth3.ssp585.tas.parquet")

    if (reset && file.exists(path)) unlink(path)
    write_local_morph_tas_fixture(path)

    normalizePath(path)
}

# Scoped cache mode switch for tests
local_cache_mode <- function(mode, env = parent.frame()) {
    old <- getOption("epwshiftr.cache")
    withr::defer(options(epwshiftr.cache = old), envir = env)
    opt_val <- switch(
        mode,
        "normal" = TRUE,
        "off" = FALSE,
        "offline" = "offline",
        stop(sprintf("Unknown cache mode: '%s'", mode))
    )
    options(epwshiftr.cache = opt_val)
}

# Create a temporary test cache and set it as the package cache
#
# @param scope Character. One of:
#   - "test" (default): ephemeral cache in tempfile(), deleted on exit
#   - "session": persists within the R session (tempdir()-based), not deleted
#   - "persist": persists for the testthat run, then is deleted at teardown
# @param env The environment for withr::defer cleanup
local_test_cache <- function(scope = c("test", "session", "persist"), env = parent.frame()) {
    scope <- match.arg(scope)

    dir <- switch(scope,
        "test" = tempfile("epwshiftr-test-cache-"),
        "session" = file.path(tempdir(), "epwshiftr-test-cache"),
        "persist" = file.path(dirname(tempdir()), "epwshiftr-test-cache")
    )

    cache <- DiskCache$new(dir = dir, max_size = "100 MB", max_age = Inf, max_n = Inf)
    old_cache <- cache__set(cache)
    cleanup_env <- if (identical(scope, "persist")) testthat::teardown_env() else env
    withr::defer(
        {
            cache__set(old_cache)
            if (scope %in% c("test", "persist")) unlink(dir, recursive = TRUE)
        },
        envir = cleanup_env
    )
    cache
}
