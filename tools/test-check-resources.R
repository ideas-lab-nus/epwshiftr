# Run from the package root with uvr run tools/test-check-resources.R. These
# tests exercise resource selection and entry-point routing without workers.
source("tests/support/check-parallel.R")

testthat::test_that("automatic shard counts respect memory and core limits", {
    gib <- 1024^3
    for (memory in c(1, 6, 7, 12, 16, 18, 32, 64)) {
        expected <- as.integer(max(1, min(3, floor(memory / 6))))
        testthat::expect_identical(
            checks__default_shards(memory_bytes = memory * gib, cores = 32),
            expected
        )
    }
    testthat::expect_identical(
        checks__default_shards(memory_bytes = 64 * gib, cores = 1),
        1L
    )
    testthat::expect_identical(
        checks__default_shards(memory_bytes = 64 * gib, cores = 2),
        2L
    )
    testthat::expect_identical(
        checks__default_shards(
            maximum = 1,
            memory_bytes = 64 * gib,
            cores = 16
        ),
        1L
    )
    testthat::expect_identical(
        checks__default_shards(
            maximum = 4,
            memory_bytes = 64 * gib,
            cores = 16
        ),
        4L
    )
    testthat::expect_identical(
        checks__default_shards(memory_bytes = 12 * gib - 1, cores = 16),
        1L
    )
    testthat::expect_identical(
        checks__default_shards(memory_bytes = 18 * gib - 1, cores = 16),
        2L
    )
})

testthat::test_that("missing host information selects serial execution", {
    for (value in list(NA_real_, numeric(), c(1, 2), -1, 0, Inf, "unknown")) {
        testthat::expect_identical(
            checks__default_shards(memory_bytes = value, cores = 16),
            1L
        )
        testthat::expect_identical(
            checks__default_shards(memory_bytes = 64 * 1024^3, cores = value),
            1L
        )
    }
    testthat::expect_identical(
        checks__default_shards(memory_bytes = stop("unavailable"), cores = 16),
        1L
    )
    testthat::expect_identical(
        checks__default_shards(
            memory_bytes = 64 * 1024^3,
            cores = stop("unavailable")
        ),
        1L
    )
    for (maximum in list(NA_real_, 0, -1, 1.5, Inf, "3", c(1, 2))) {
        testthat::expect_error(
            checks__default_shards(maximum, 64 * 1024^3, 16),
            "maximum must be a positive integer"
        )
    }
})

# Evaluate only the test entry's routing, substituting worker/test entry points
# so explicit shard settings can be checked without running the package suite.
checks_test__entry <- function(setting = NA_character_, automatic = 2L) {
    withr::local_envvar(c(EPWSHIFTR_TEST_SHARDS = setting))
    env <- new.env(parent = baseenv())
    env$library <- function(...) NULL
    env$source <- function(...) {
        env$checks__default_shards <- function(...) automatic
    }
    env$test_check <- function(...) env$result <- "serial"
    env$checks__run <- function(..., shards) env$result <- shards
    env$find.package <- function(...) "/library/epwshiftr"
    env$read.csv <- function(...) data.frame()
    sys.source("tests/testthat.R", envir = env)
    env$result
}

testthat::test_that("test entry retains serial default and explicit local shard counts", {
    testthat::expect_identical(checks_test__entry(), "serial")
    testthat::expect_identical(checks_test__entry("1"), "serial")
    testthat::expect_identical(checks_test__entry("4"), 4L)
    testthat::expect_identical(checks_test__entry("6"), 6L)
    testthat::expect_identical(checks_test__entry("7"), 7L)
    testthat::expect_identical(checks_test__entry("8"), 8L)
    testthat::expect_error(checks_test__entry("9"), "must be 'auto'")
    testthat::expect_error(checks_test__entry("0"), "must be 'auto'")
    testthat::expect_identical(checks_test__entry("auto", 2L), 2L)
    testthat::expect_identical(checks_test__entry("auto", 1L), "serial")
    testthat::expect_error(checks_test__entry("invalid"), "must be 'auto'")
})

# Stop the real entry scripts at their first post-validation operation so their
# limits are checked without creating an installation or starting any workers.
checks_test__performance_entry <- function(shards) {
    env <- new.env(parent = baseenv())
    env$commandArgs <- function(...) {
        c("ordinary", "unused-output", as.character(shards), "1")
    }
    env$dir.exists <- function(...) FALSE
    env$normalizePath <- function(...) stop("validated before preparation")
    sys.source("tools/check-performance.R", envir = env)
}

testthat::test_that("explicit runner and performance entry accept eight but reject nine", {
    for (shards in c(1L, 6L, 7L, 8L)) {
        testthat::expect_error(
            checks__run(
                stop("validated before execution"),
                tempfile(),
                shards = shards
            ),
            "validated before execution"
        )
        testthat::expect_error(
            checks_test__performance_entry(shards),
            "validated before preparation"
        )
    }
    for (shards in c(0L, 9L)) {
        testthat::expect_error(
            checks__run(
                stop("unexpected preparation"),
                tempfile(),
                shards = shards
            ),
            "shards"
        )
        testthat::expect_error(checks_test__performance_entry(shards), "shards")
    }
    testthat::expect_identical(
        checks__default_shards(memory_bytes = 64 * 1024^3, cores = 12),
        3L
    )
})

# vim: fdm=marker :
