# Run from the package root with Rscript .github/testing/tests/test-check-resources.R. These
# tests exercise resource selection and entry-point routing without workers.
source(".github/testing/check-parallel.R")

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
checks_test__entry <- function(
    setting = NA_character_,
    automatic = 2L,
    external = FALSE
) {
    withr::local_envvar(c(
        EPWSHIFTR_TEST_SHARDS = setting,
        EPWSHIFTR_TEST_RUNNER = if (external) {
            normalizePath(".github/testing/check-package.R", winslash = "/")
        } else {
            NA_character_
        }
    ))
    env <- new.env(parent = baseenv())
    env$library <- function(...) NULL
    env$source <- function(file, ...) {
        if (basename(file) == "check-package.R") {
            sys.source(file, envir = env)
        } else {
            env$checks__default_shards <- function(...) automatic
        }
    }
    env$test_check <- function(...) env$result <- "serial"
    env$checks__run <- function(..., shards) env$result <- shards
    env$find.package <- function(...) "/library/epwshiftr"
    env$read.csv <- function(...) stop("CI must not read local timing tables")
    sys.source("tests/testthat.R", envir = env)
    env$result
}

testthat::test_that("package checks stay serial unless an external runner is supplied", {
    testthat::expect_identical(checks_test__entry(), "serial")
    testthat::expect_identical(checks_test__entry("auto"), "serial")
    testthat::expect_identical(checks_test__entry("4"), "serial")
    testthat::expect_identical(checks_test__entry(external = TRUE), 2L)
    testthat::expect_identical(
        checks_test__entry("1", external = TRUE),
        "serial"
    )
    testthat::expect_identical(checks_test__entry("4", external = TRUE), 4L)
    testthat::expect_identical(checks_test__entry("6", external = TRUE), 6L)
    testthat::expect_identical(checks_test__entry("7", external = TRUE), 7L)
    testthat::expect_identical(checks_test__entry("8", external = TRUE), 8L)
    testthat::expect_error(
        checks_test__entry("9", external = TRUE),
        "must be 'auto'"
    )
    testthat::expect_error(
        checks_test__entry("0", external = TRUE),
        "must be 'auto'"
    )
    testthat::expect_identical(
        checks_test__entry("auto", 2L, external = TRUE),
        2L
    )
    testthat::expect_identical(
        checks_test__entry("auto", 1L, external = TRUE),
        "serial"
    )
    testthat::expect_error(
        checks_test__entry("invalid", external = TRUE),
        "must be 'auto'"
    )
})

testthat::test_that("explicit runner accepts eight shards but rejects nine", {
    for (shards in c(1L, 6L, 7L, 8L)) {
        testthat::expect_error(
            checks__run(
                stop("validated before execution"),
                tempfile(),
                shards = shards
            ),
            "validated before execution"
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
    }
    testthat::expect_identical(
        checks__default_shards(memory_bytes = 64 * 1024^3, cores = 12),
        3L
    )
})

# CI must keep every file when no machine-specific cost table is available.
testthat::test_that("unweighted scheduling assigns every file exactly once", {
    files <- sprintf("test-file-%02d.R", seq_len(13L))
    groups <- checks__partition(files, shards = 3L)
    assigned <- unlist(groups, use.names = FALSE)
    testthat::expect_identical(sort(assigned), files)
    testthat::expect_identical(anyDuplicated(assigned), 0L)
    testthat::expect_lte(diff(range(lengths(groups))), 1L)
    testthat::expect_true(all(vapply(
        groups,
        function(group) identical(group, sort(group)),
        logical(1L)
    )))
})

# vim: fdm=marker :
