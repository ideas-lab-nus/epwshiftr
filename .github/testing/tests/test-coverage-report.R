# Run from the package root with Rscript .github/testing/tests/test-coverage-report.R. Compare
# the report serializer with covr on small fixtures without starting workers.
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else "."
source(file.path(root, ".github", "coverage.R"), local = TRUE)

# Use complete line rows with uncovered lines, absent function names and a
# same-named function in two files. Special characters exercise XML attributes.
coverage_test__lines <- function() {
    data.frame(
        filename = c(
            'R/a<&>"\t\n\r汉.R',
            'R/a<&>"\t\n\r汉.R',
            'R/b.R',
            'R/b.R'
        ),
        functions = c('f<&>"\t\n\r汉', NA, 'f<&>"\t\n\r汉', 'plain'),
        line = c(1L, 2L, 1L, 2L),
        value = c(0, 2, 3, 0)
    )
}

# Parse away formatting whitespace while preserving every node and attribute.
# Check line tuples separately so repeated method/class lines remain explicit.
coverage_test__compare_xml <- function(original, candidate) {
    left <- xml2::read_xml(original, options = "NOBLANKS")
    right <- xml2::read_xml(candidate, options = "NOBLANKS")
    testthat::expect_identical(as.character(right), as.character(left))
    left_lines <- xml2::xml_find_all(left, "//line")
    right_lines <- xml2::xml_find_all(right, "//line")
    testthat::expect_identical(length(right_lines), length(left_lines))
    for (attribute in c("number", "hits", "branch")) {
        testthat::expect_identical(
            xml2::xml_attr(right_lines, attribute),
            xml2::xml_attr(left_lines, attribute)
        )
    }
    testthat::expect_identical(
        xml2::xml_find_num(right, "count(//@*)"),
        xml2::xml_find_num(left, "count(//@*)")
    )
}

testthat::test_that("batched XML preserves covr structure and escaping", {
    lines <- coverage_test__lines()
    tally <- covr::tally_coverage
    testthat::local_mocked_bindings(
        tally_coverage = function(x, by) {
            if (inherits(x, "coverage")) lines else tally(x, by)
        },
        .package = "covr"
    )
    directory <- withr::local_tempdir()
    original <- file.path(directory, "covr.xml")
    candidate <- file.path(directory, "batch.xml")
    packages <- list(
        list(package = 'p<&>"\t\n\r汉', path = 'source<&>"\t\n\r汉]]>'),
        list(package = "package"),
        NULL
    )
    for (package in packages) {
        coverage <- structure(list(), class = "coverage", package = package)
        covr::to_cobertura(coverage, original)
        doc <- xml2::read_xml(original)
        timestamp <- xml2::xml_attr(
            xml2::xml_find_first(doc, "/coverage"),
            "timestamp"
        )
        coverage__cobertura(coverage, lines, candidate, timestamp)
        coverage_test__compare_xml(original, candidate)
    }
    # A root source path is used when package metadata has no source path.
    attr(coverage, "root") <- "fallback<&>"
    covr::to_cobertura(coverage, original)
    doc <- xml2::read_xml(original)
    timestamp <- xml2::xml_attr(
        xml2::xml_find_first(doc, "/coverage"),
        "timestamp"
    )
    coverage__cobertura(coverage, lines, candidate, timestamp)
    coverage_test__compare_xml(original, candidate)
})

testthat::test_that("report reuses one complete tally and preserves other artifacts", {
    lines <- coverage_test__lines()
    lines$filename <- c("R/a.R", "R/a.R", "R/b.R", "R/b.R")
    calls <- 0L
    testthat::local_mocked_bindings(
        tally_coverage = function(x, by) {
            calls <<- calls + 1L
            lines
        },
        .package = "covr"
    )
    coverage <- structure(
        list(),
        class = "coverage",
        package = list(package = "fixture")
    )
    manifest <- data.frame(pid = 1L, create_time = 100)
    attr(coverage, "trace_manifest") <- manifest
    directory <- withr::local_tempdir()
    invisible(capture.output(files <- coverage__report(coverage, directory)))
    testthat::expect_identical(calls, 1L)
    testthat::expect_identical(
        readRDS(file.path(directory, "coverage.rds")),
        coverage
    )
    testthat::expect_equal(
        read.csv(file.path(directory, "trace-manifest.csv")),
        manifest
    )
    testthat::expect_equal(files$covered, c(1L, 1L))
    testthat::expect_equal(files$total, c(2L, 2L))
    testthat::expect_equal(files$percent, c(50, 50))
    testthat::expect_equal(
        data.table::fread(file.path(directory, "coverage-by-file.csv")),
        files
    )
    testthat::expect_true(any(grepl(
        "**50.00%** (2 / 4 instrumented lines covered across 2 files).",
        readLines(file.path(directory, "summary.md")),
        fixed = TRUE
    )))
    doc <- xml2::read_xml(file.path(directory, "coverage.xml"))
    testthat::expect_identical(
        xml2::xml_attr(xml2::xml_find_first(doc, "/coverage"), "lines-valid"),
        "4"
    )
    testthat::expect_identical(xml2::xml_find_num(doc, "count(//line)"), 9)
})

testthat::test_that("zero coverage and absent methods retain all class lines", {
    lines <- coverage_test__lines()
    lines$functions <- NA_character_
    lines$value <- 0
    tally <- covr::tally_coverage
    testthat::local_mocked_bindings(
        tally_coverage = function(x, by) {
            if (inherits(x, "coverage")) lines else tally(x, by)
        },
        .package = "covr"
    )
    coverage <- structure(
        list(),
        class = "coverage",
        package = list(package = "fixture")
    )
    directory <- withr::local_tempdir()
    original <- file.path(directory, "covr.xml")
    candidate <- file.path(directory, "batch.xml")
    covr::to_cobertura(coverage, original)
    doc <- xml2::read_xml(original)
    timestamp <- xml2::xml_attr(
        xml2::xml_find_first(doc, "/coverage"),
        "timestamp"
    )
    coverage__cobertura(coverage, lines, candidate, timestamp)
    coverage_test__compare_xml(original, candidate)
})

testthat::test_that("line tally preserves covr source slicing and blank rules", {
    source_lines <- get("srcfile_lines", asNamespace("covr"))
    tally <- covr::tally_coverage
    texts <- list(
        c("", "# comment", "x <- 1", "  # indented", "\t"),
        c(
            '#line 1 "a.R"',
            "x <- 1",
            "# comment",
            '#line 1 "b.R"',
            "y <- 2",
            ""
        ),
        c('#line 1 "a.R"', "x", '#line 1 "a.R"', "y"),
        c('#line 10 "a.R"', "x", ""),
        c('#line 1 "quoted file.R"', "x", "")
    )
    for (text in texts) {
        # Different source names deliberately share one full lines vector;
        # revisiting a.R checks cached matches without sharing source slicing.
        files <- lapply(
            c("a.R", "b.R", "quoted file.R", "a.R"),
            function(name) {
                srcfilecopy(name, text, isFile = FALSE)
            }
        )
        coverage <- structure(
            lapply(seq_along(files), function(i) {
                list(
                    srcref = srcref(
                        files[[i]],
                        c(1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L)
                    ),
                    value = c(0, 1, 3, 2)[[i]],
                    functions = c("same", NA, "different", "same")[[i]]
                )
            }),
            class = "coverage"
        )
        testthat::expect_identical(
            coverage__tally_lines(coverage),
            tally(coverage, by = "line")
        )
        # A fresh call must not retain the previous call's source cache.
        testthat::expect_identical(
            coverage__tally_lines(coverage),
            tally(coverage, by = "line")
        )
    }
    testthat::expect_identical(
        get("srcfile_lines", asNamespace("covr")),
        source_lines
    )
    testthat::expect_identical(covr::tally_coverage, tally)
    empty <- structure(list(), class = "coverage")
    testthat::expect_identical(
        coverage__tally_lines(empty),
        tally(empty, by = "line")
    )
})

testthat::test_that("unknown covr tally shapes retain the original fallback", {
    marker <- data.frame(fallback = TRUE)
    testthat::local_mocked_bindings(
        tally_coverage = function(x, by) marker,
        .package = "covr"
    )
    testthat::expect_identical(coverage__tally_lines(NULL), marker)
})

testthat::test_that("collector fast path preserves numeric attributes and strict keys", {
    directory <- withr::local_tempdir()
    src <- srcfilecopy("fixture.R", c("a", "b", "c"))
    metadata <- setNames(
        lapply(1:3, function(i) {
            list(
                value = 0,
                srcref = srcref(src, rep(as.integer(i), 8L)),
                functions = "fixture"
            )
        }),
        paste0("fixture:", 1:3)
    )
    saveRDS(metadata, file.path(directory, "covr-source-metadata.rds"))
    for (pid in 1:2) {
        file.create(file.path(directory, paste0("expected-trace-", pid)))
        saveRDS(
            list(
                pid = pid,
                create_time = Sys.time() - 10,
                registered_at = Sys.time() - 1,
                jit_level = 0L,
                jit_env = "0"
            ),
            file.path(directory, paste0("process-", pid, ".rds"))
        )
        saveRDS(
            list(pid = pid, user = 1, system = 0, elapsed = 2),
            file.path(directory, paste0("resource-", pid, ".rds"))
        )
    }
    values <- setNames(c(1, 0, 3), names(metadata))
    saveRDS(values[3:1], file.path(directory, "covr_trace_2"))
    valid <- list(
        values,
        values[3:1],
        structure(values, tag = "tag"),
        structure(values, dim = c(3L, 1L)),
        structure(values, dim = c(1L, 3L, 1L)),
        structure(values, class = "coverage_numeric")
    )
    for (value in valid) {
        saveRDS(value, file.path(directory, "covr_trace_1"))
        result <- coverage__collect(directory, timeout = 0)
        testthat::expect_identical(result$counts, values * 2)
        testthat::expect_identical(
            vapply(result$counters, `[[`, numeric(1L), "value"),
            values * 2
        )
        testthat::expect_identical(
            coverage__source_identity(result$counters),
            coverage__source_identity(metadata)
        )
    }
    bad <- list(
        unname(values),
        values[-1L],
        c(values, extra = 0),
        setNames(values, rep(names(values)[[1L]], 3L)),
        setNames(as.character(values), names(values)),
        as.list(values)
    )
    for (invalid in c(-1, NA_real_, Inf)) {
        value <- values
        value[[1L]] <- invalid
        bad[[length(bad) + 1L]] <- value
    }
    for (value in bad) {
        saveRDS(value, file.path(directory, "covr_trace_1"))
        testthat::expect_error(
            coverage__collect(directory, timeout = 0),
            "does not match the complete"
        )
    }
})

# vim: fdm=marker :
