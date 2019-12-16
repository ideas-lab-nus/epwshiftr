test_that("Verbose message", {
    expect_silent(verbose(""))
    testthat::expect_output({options("epwshiftr.verbose" = TRUE); verbose("a")})
    options("epwshiftr.verbose" = FALSE)
})
