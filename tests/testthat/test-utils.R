test_that("Verbose message", {
    withr::with_options(
        list("epwshiftr.verbose" = FALSE),
        expect_silent(vmsg("a"))
    )
    withr::with_options(
        list("epwshiftr.verbose" = TRUE),
        expect_message(vmsg("a"), "a")
    )
})

test_that("now()", {
    expect_s3_class(now(), "POSIXct")
    expect_equal(attr(now(), "tzone"), "UTC")
})

test_that("set_size_units()", {
    expect_null(set_size_units(logical()))
    expect_warning(set_size_units(units::set_units(1:3, "Kelvin")))

    suppressWarnings(
        expect_equal(
            set_size_units(units::set_units(1, "Kelvin")),
            units::set_units(1, "Kelvin")
        )
    )

    expect_equal(
        units(set_size_units(units::set_units(1, "GByte")))$numerator,
        "MiB"
    )

    expect_equal(attr(now(), "tzone"), "UTC")
})


test_that(".data_dir()", {
    options("epwshiftr.verbose" = FALSE)

    # can stop if user specified data directory did not exist
    options("epwshiftr.dir" = file.path(tempdir(), "test"))
    expect_error(.data_dir())

    # can create data dir if not exists in the user home
    skip_on_cran()
    options("epwshiftr.dir" = NULL)
    if (dir.exists(.data_dir(force = FALSE))) {
        unlink(.data_dir(force = FALSE), recursive = TRUE)
    }
    expect_true(dir.exists(.data_dir(init = TRUE, force = TRUE)))
})
