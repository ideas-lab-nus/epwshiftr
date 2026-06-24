test_that("get_cache_epw() prepares a stable local EPW fixture", {
    dir <- withr::local_tempdir()
    withr::local_envvar(EPWSHIFTR_CHECK_CACHE = dir)

    path <- get_cache_epw()

    expect_true(file.exists(path))
    expect_identical(basename(path), "SGP_Singapore.486980_IWEC.epw")

    epw <- eplusr::read_epw(path)
    expect_equal(epw$location()$city, "Singapore")
    expect_equal(epw$location()$country, "Singapore")
    expect_equal(nrow(epw$data()), 8760L)

    expect_identical(get_cache_epw(), path)
})