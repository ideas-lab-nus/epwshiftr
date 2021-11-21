test_that("Verbose message", {
    expect_silent(verbose(""))
    testthat::expect_output({options("epwshiftr.verbose" = TRUE); verbose("a")})
    options("epwshiftr.verbose" = FALSE)
})

test_that(".data_dir()", {
    options("epwshiftr.verbose" = FALSE)

    # can stop if user specified data directory did not exist
    options("epwshiftr.dir" = file.path(tempdir(), "test"))
    expect_error(.data_dir())

    # can create data dir if not exists in the user home
    skip_on_cran()
    options("epwshiftr.dir" = NULL)
    if (dir.exists(.data_dir(force = FALSE))) unlink(.data_dir(force = FALSE), recursive = TRUE)
    expect_true(dir.exists(.data_dir(init = TRUE, force = TRUE)))
})
