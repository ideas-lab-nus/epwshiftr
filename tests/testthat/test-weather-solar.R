test_that("shared solar kernels preserve vector and matrix geometry", {
    expect_equal(solar__radians(c(0, 90, 180)), c(0, pi / 2, pi))
    expect_equal(solar__cos_zenith(0, 0, 0), 1)
    expect_equal(solar__cos_zenith(0, 0, pi / 2), 0, tolerance = 1e-15)

    angle <- matrix(c(0, pi / 2, pi, 3 * pi / 2), nrow = 2L)
    declination <- solar__spencer_declination(angle)
    equation_of_time <- solar__spencer_equation_of_time(angle)
    expect_identical(dim(declination), dim(angle))
    expect_identical(dim(equation_of_time), dim(angle))
    expect_true(all(is.finite(declination)))
    expect_true(all(is.finite(equation_of_time)))
})

test_that("Belcher declination delegates without changing its day convention", {
    day <- c(1, 32, 183, 365)

    expect_equal(
        morpher__belcher_declination(day),
        solar__spencer_declination(morpher__belcher_day_angle(day)),
        tolerance = 0
    )
})
