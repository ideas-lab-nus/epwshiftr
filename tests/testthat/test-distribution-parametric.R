test_that("parametric distributions support optional fixed locations", {
    probability <- c(0.1, 0.5, 0.9)
    gamma <- list(
        family = "gamma",
        parameters = list(shape = 2, scale = 3, location = 5),
        sample_size = 20L,
        method = "test"
    )
    weibull <- list(
        family = "weibull",
        parameters = list(shape = 1.5, scale = 2, location = 0.25),
        sample_size = 20L,
        method = "test"
    )

    for (fit in list(gamma, weibull)) {
        expect_equal(
            distribution__cdf(
                fit,
                distribution__quantile(fit, probability)
            ),
            probability,
            tolerance = 1e-12
        )
        expect_equal(
            distribution__cdf(fit, fit$parameters$location - 1),
            0
        )
    }
})

test_that("parametric distribution validation rejects incomplete fits", {
    expect_error(
        distribution__cdf(
            list(
                family = "weibull",
                parameters = list(shape = 2),
                sample_size = 10L,
                method = "test"
            ),
            1
        ),
        "requires `shape` and `scale`"
    )
})
