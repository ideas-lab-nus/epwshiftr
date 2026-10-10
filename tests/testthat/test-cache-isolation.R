# A configured CI cache is a location for private work, not mutable shared input.
test_that("test data preserves the configured cache parent", {
    root <- withr::local_tempdir()
    withr::local_envvar(EPWSHIFTR_CHECK_CACHE = root)
    shared <- file.path(root, "SGP_Singapore.486980_IWEC.epw")
    writeLines("shared input sentinel", shared)

    path <- get_cache_epw()
    expect_identical(
        normalizePath(test_data_dir(), winslash = "/"),
        dirname(path)
    )
    expect_identical(
        dirname(dirname(path)),
        normalizePath(root, winslash = "/")
    )
    expect_identical(readLines(shared), "shared input sentinel")
    expect_false(identical(path, normalizePath(shared, winslash = "/")))
})

# Fresh R processes must never choose the same fixture directory under one root.
test_that("test data directories are isolated between R processes", {
    root <- withr::local_tempdir()
    # Send only the helper body: exporting the test environment would capture
    # unrelated package state and make this isolation test unnecessarily costly.
    resolve <- eval(call("function", NULL, body(test_data_dir)), baseenv())
    results <- lapply(seq_len(2L), function(index) {
        callr::r(
            function(resolve, root, index) {
                Sys.setenv(EPWSHIFTR_CHECK_CACHE = root)
                # Use a real testthat run so its teardown owns the child.
                script <- tempfile(fileext = ".R")
                writeLines(
                    c(
                        "state$path <- resolve()",
                        "writeLines(as.character(index), file.path(state$path, 'owner.txt'))",
                        "state$owner <- readLines(file.path(state$path, 'owner.txt'))"
                    ),
                    script
                )
                env <- new.env(parent = baseenv())
                env$resolve <- resolve
                env$index <- index
                env$state <- new.env(parent = emptyenv())
                testthat::test_file(script, env = env, reporter = "silent")
                list(
                    path = env$state$path,
                    owner = env$state$owner,
                    exists = dir.exists(env$state$path)
                )
            },
            args = list(resolve = resolve, root = root, index = index)
        )
    })
    paths <- vapply(results, `[[`, character(1L), "path")
    expect_false(identical(paths[[1L]], paths[[2L]]))
    expect_identical(
        normalizePath(dirname(paths), winslash = "/"),
        rep(normalizePath(root, winslash = "/"), 2L)
    )
    expect_identical(results[[1L]]$owner, "1")
    expect_identical(results[[2L]]$owner, "2")
    expect_false(results[[1L]]$exists)
    expect_false(results[[2L]]$exists)
})

# Persist means one test run, never the shared parent of R session directories.
test_that("persistent test caches stay inside the current R temporary directory", {
    cache <- local_test_cache(scope = "persist")
    expect_identical(
        normalizePath(dirname(cache$info()$dir), winslash = "/"),
        normalizePath(tempdir(), winslash = "/")
    )
    cache$set("isolation", "retained within this process")
    expect_identical(cache$get("isolation"), "retained within this process")
})

# Parquet preparation is independent of existing NetCDF files and their handles.
test_that("Parquet preparation does not rewrite NetCDF fixtures", {
    root <- withr::local_tempdir()
    withr::local_envvar(EPWSHIFTR_CHECK_CACHE = root)
    dir <- get_cache_nc()
    paths <- list.files(dir, pattern = "\\.nc$", full.names = TRUE)
    expect_length(paths, 3L)
    before <- tools::md5sum(paths)
    old_time <- as.POSIXct("2000-01-01", tz = "UTC")
    Sys.setFileTime(paths, old_time)
    timestamps <- file.info(paths)$mtime

    parquet <- get_cache_parquet()
    expect_identical(tools::md5sum(paths), before)
    expect_identical(file.info(paths)$mtime, timestamps)
    expect_equal(nrow(read_test_parquet(parquet)), 366L * 6L)

    # A reset replaces only the requested Parquet product as well.
    get_cache_parquet(reset = TRUE)
    expect_identical(tools::md5sum(paths), before)
    expect_identical(file.info(paths)$mtime, timestamps)
})

# vim: fdm=marker :
