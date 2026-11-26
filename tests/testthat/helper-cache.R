# Get test data directory path (for EPW/NetCDF files, not DiskCache objects)
#
# EPWSHIFTR_CHECK_CACHE selects a parent directory, not a shared mutable fixture.
# Each fresh R process owns one disposable child; cached CI inputs remain intact.
# test_data_dir {{{
test_data_dir <- function() {
    dir <- Sys.getenv("EPWSHIFTR_CHECK_CACHE", NA)
    if (is.na(dir)) {
        dir <- tempdir()
    }
    dir <- file.path(
        dir,
        paste0("epwshiftr-test-data-", basename(tempdir()), "-", Sys.getpid())
    )
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
        # Only this process's child is disposable; never remove the parent.
        withr::defer(
            unlink(dir, recursive = TRUE, force = TRUE),
            envir = testthat::teardown_env()
        )
    }
    dir
}
# }}}

test_epw_cache__env <- new.env(parent = emptyenv())

# get_cache_epw {{{
get_cache_epw <- function() {
    dir <- test_data_dir()
    fixture <- system.file(
        "extdata/examples/SGP_Singapore.486980_IWEC.epw",
        package = "epwshiftr",
        mustWork = TRUE
    )
    path <- file.path(dir, basename(fixture))
    if (!identical(test_epw_cache__env$fixture_path, fixture)) {
        test_epw_cache__env$fixture_path <- fixture
        test_epw_cache__env$fixture_size <- file.size(fixture)
        test_epw_cache__env$fixture_md5 <- unname(tools::md5sum(fixture))
    }
    # Refresh the cached copy whenever it is missing or differs from the
    # packaged fixture, while skipping redundant copies on unchanged files.
    needs_copy <- !file.exists(path) ||
        !identical(file.size(path), test_epw_cache__env$fixture_size) ||
        !identical(unname(tools::md5sum(path)), test_epw_cache__env$fixture_md5)
    if (needs_copy) {
        ok <- file.copy(fixture, path, overwrite = TRUE)
        if (!isTRUE(ok)) {
            stop(
                sprintf("Failed to copy EPW fixture to test cache: %s", path),
                call. = FALSE
            )
        }
    }
    normalizePath(path, winslash = "/", mustWork = TRUE)
}
# }}}

# Memoize reading the packaged EPW fixture once per worker and return a fresh
# deep copy so mutations in one test cannot corrupt the cached template.
# get_cached_epw {{{
get_cached_epw <- function() {
    path <- get_cache_epw()
    if (
        is.null(test_epw_cache__env$epw) ||
            !identical(test_epw_cache__env$epw_path, path)
    ) {
        test_epw_cache__env$epw <- epw_file_read(path)
        test_epw_cache__env$epw_path <- path
    }
    epw <- test_epw_cache__env$epw$clone(deep = TRUE)
    private_env <- epw$.__enclos_env__$private
    if (
        is.environment(private_env) &&
            data.table::is.data.table(private_env$weather)
    ) {
        private_env$weather <- data.table::copy(private_env$weather)
    }
    epw
}
# }}}

# read_test_parquet {{{
read_test_parquet <- function(path) {
    conn <- ddb_connect(":memory:")
    on.exit(ddb_disconnect(conn), add = TRUE)

    data.table::as.data.table(ddb_query(
        conn,
        sprintf(
            "SELECT * FROM read_parquet(%s)",
            ddb_literal(conn, path)
        )
    ))
}
# }}}

# Build a dependency-free stand-in for the public eplusr Epw protocol so input
# compatibility can be tested without declaring eplusr in Suggests.
# test_external_epw {{{
test_external_epw <- function(path) {
    generator <- R6::R6Class(
        "Epw",
        public = list(
            source_path = NULL,
            initialize = function(path) {
                self$source_path <- normalizePath(
                    path,
                    winslash = "/",
                    mustWork = TRUE
                )
            },
            path = function() {
                self$source_path
            },
            location = function() {
                epw_file_read(self$source_path)$location()
            },
            save = function(path, overwrite = FALSE) {
                if (file.exists(path) && !isTRUE(overwrite)) {
                    stop("Target exists.")
                }
                dir.create(
                    dirname(path),
                    recursive = TRUE,
                    showWarnings = FALSE
                )
                if (
                    !isTRUE(file.copy(
                        self$source_path,
                        path,
                        overwrite = overwrite
                    ))
                ) {
                    stop("Failed to save fake Epw.")
                }
                self$source_path <- normalizePath(
                    path,
                    winslash = "/",
                    mustWork = TRUE
                )
                invisible(self$source_path)
            }
        )
    )
    generator$new(path)
}
# }}}

# Scoped cache mode switch for tests
# local_cache_mode {{{
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
# }}}

# Create a temporary test cache and set it as the package cache
#
# @param scope Character. One of:
#   - "test" (default): ephemeral cache in tempfile(), deleted on exit
#   - "session": persists within the R session (tempdir()-based), not deleted
#   - "persist": persists for the calling file/scope in an isolated directory,
#     restores the prior cache, and deletes the directory on exit
# @param env The environment for withr::defer cleanup
# local_test_cache {{{
local_test_cache <- function(
    scope = c("test", "session", "persist"),
    env = parent.frame()
) {
    scope <- match.arg(scope)

    dir <- switch(
        scope,
        "test" = tempfile("epwshiftr-test-cache-"),
        "session" = file.path(tempdir(), "epwshiftr-test-cache"),
        "persist" = tempfile("epwshiftr-persist-cache-")
    )

    cache <- DiskCache$new(
        dir = dir,
        max_size = "100 MB",
        max_age = Inf,
        max_n = Inf
    )
    old_cache <- cache__set(cache)
    withr::defer(
        {
            cache__set(old_cache)
            if (scope %in% c("test", "persist")) {
                unlink(dir, recursive = TRUE, force = TRUE)
            }
        },
        envir = env
    )
    cache
}
# }}}

# vim: fdm=marker :
