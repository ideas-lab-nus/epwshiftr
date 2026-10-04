# Exercise reporting with real covr instrumentation, including an uncalled
# function. These checks run before the full suite in the coverage workflow.
source(".github/coverage.R")

testthat::test_that("coverage reports retain the measured numerator and denominator", {
    path <- tempfile("coverage-report-test-")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    called <- file.path(path, "called.R")
    uncalled <- file.path(path, "uncalled.R")
    tests <- file.path(path, "exercise.R")
    writeLines(
        c(
            "called <- function() {",
            "    value <- 42L",
            "    value + 1L",
            "}"
        ),
        called
    )
    writeLines(
        c(
            "uncalled <- function() {",
            "    value <- 7L",
            "    value * 2L",
            "}"
        ),
        uncalled
    )
    writeLines("stopifnot(called() == 43L)", tests)
    coverage <- covr::file_coverage(c(called, uncalled), tests)
    report <- file.path(path, "report")
    output <- capture.output(files <- coverage__report(coverage, report))
    tally <- covr::tally_coverage(coverage, by = "line")
    expected <- covr::percent_coverage(coverage, by = "line")
    testthat::expect_gt(expected, 0)
    testthat::expect_lt(expected, 100)
    testthat::expect_equal(sum(files$covered), sum(tally$value > 0))
    testthat::expect_equal(sum(files$total), nrow(tally))
    testthat::expect_equal(
        files$percent[basename(files$filename) == "uncalled.R"],
        0
    )
    testthat::expect_equal(
        data.table::fread(file.path(report, "coverage-by-file.csv")),
        files
    )
    testthat::expect_equal(
        covr::percent_coverage(readRDS(file.path(report, "coverage.rds"))),
        expected
    )
    xml <- xml2::read_xml(file.path(report, "coverage.xml"))
    testthat::expect_equal(
        as.numeric(xml2::xml_attr(xml, "line-rate")) * 100,
        expected
    )
    testthat::expect_equal(
        as.integer(xml2::xml_attr(xml, "lines-valid")),
        sum(files$total)
    )
    testthat::expect_equal(
        as.integer(xml2::xml_attr(xml, "lines-covered")),
        sum(files$covered)
    )
    testthat::expect_match(
        paste(output, collapse = "\n"),
        sprintf("%.2f%%", expected),
        fixed = TRUE
    )
    testthat::expect_equal(output, readLines(file.path(report, "summary.md")))
})

# vim: fdm=marker :
