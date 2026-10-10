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

test_that("Original morphing declination delegates without changing its day convention", {
    day <- c(1, 32, 183, 365)

    expect_equal(
        original_morphing__declination(day),
        solar__spencer_declination(original_morphing__day_angle(day)),
        tolerance = 0
    )
})

# Shared terms must retain array structure and missing-value positions through
# both the existing standalone helpers and the combined geometry entry.
test_that("Spencer geometry shares terms without changing values or attributes", {
    angles <- list(
        numeric(),
        c(equinox = 0, missing = NA_real_, undefined = NaN),
        matrix(
            c(0, pi / 2, NA_real_, pi),
            nrow = 2L,
            dimnames = list(c("north", "south"), c("early", "late"))
        )
    )
    for (angle in angles) {
        terms <- solar__spencer_terms(angle)
        geometry <- solar__spencer_geometry(angle, include_eccentricity = TRUE)
        expect_identical(
            geometry$declination,
            solar__spencer_declination(angle)
        )
        expect_identical(
            geometry$equation_of_time,
            solar__spencer_equation_of_time(angle)
        )
        expect_identical(
            solar__spencer_declination(angle, terms),
            geometry$declination
        )
        expect_identical(
            solar__spencer_equation_of_time(angle, terms),
            geometry$equation_of_time
        )
        expect_identical(attributes(geometry$eccentricity), attributes(angle))
        expect_identical(is.na(geometry$eccentricity), is.na(angle))
    }
    # Fixed annual-angle values independently anchor the Fourier coefficients.
    geometry <- solar__spencer_geometry(0, include_eccentricity = TRUE)
    expect_equal(geometry$declination, -0.402449, tolerance = 1e-14)
    expect_equal(geometry$equation_of_time, -2.90416896, tolerance = 1e-14)
    expect_equal(geometry$eccentricity, 1.03505, tolerance = 1e-14)
    expect_named(
        solar__spencer_geometry(0),
        c("declination", "equation_of_time")
    )
})

# A solar implementation change must invalidate ERA5-derived cache identity,
# even when the public conversion adapter itself has not changed.
test_that("ERA5 implementation identity includes shared solar terms", {
    identity <- era_epw__implementation()
    original <- solar__spencer_terms
    testthat::local_mocked_bindings(
        solar__spencer_terms = function(day_angle) original(day_angle),
        .package = "epwshiftr"
    )
    expect_false(identical(era_epw__implementation(), identity))
})

# vim: fdm=marker :
