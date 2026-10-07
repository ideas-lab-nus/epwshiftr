# Build a complete, constant-wind year so fallback arithmetic has an analytic
# reference while cyclic smoothing still exercises both year boundaries.
monthly_stretch_test__year <- function() {
    time <- seq(
        as.POSIXct("2001-01-01", tz = "UTC"),
        by = "hour",
        length.out = 8760L
    )
    data.table::data.table(
        datetime = time,
        year = 2001L,
        month = as.integer(format(time, "%m")),
        day = as.integer(format(time, "%d")),
        hour = as.integer(format(time, "%H")) + 1L,
        minute = 60L,
        wind_speed = 2
    )
}

# Exercise both reference choices and the two invalid-stretch conditions.
test_that("enhanced stretch fallbacks apply the recorded additive method", {
    withr::local_options(epwshiftr.threshold_alpha = 3)
    for (absolute in c(FALSE, TRUE)) {
        for (reason in c("zero", "threshold")) {
            for (width in c(0L, 72L)) {
                epw <- monthly_stretch_test__year()
                future <- data.table::data.table(
                    month = 1:12,
                    units = "m s-1",
                    value = 4
                )
                reference <- data.table::copy(future)
                data.table::set(reference, j = "value", value = 2)
                january <- which(epw$month == 1L)
                if (reason == "zero") {
                    data.table::set(reference, i = 1L, j = "value", value = 0)
                    if (absolute) {
                        data.table::set(
                            epw,
                            i = january,
                            j = "wind_speed",
                            value = 0
                        )
                    }
                } else {
                    data.table::set(future, i = 1L, j = "value", value = 10)
                }
                before <- data.table::copy(epw)
                args <- list(
                    var = "wind_speed",
                    data_epw = epw,
                    data_mean = future,
                    type = "stretch",
                    transition_hours = width
                )
                if (absolute) {
                    fun <- original_morphing__from_monthly_enhanced
                    baseline_mean <- mean(epw$wind_speed[january])
                } else {
                    fun <- original_morphing__from_monthly_change_enhanced
                    args$reference_mean <- reference
                    baseline_mean <- reference$value[1L]
                }
                actual <- do.call(fun, args)
                expected <- epw$wind_speed[january] +
                    future$value[1L] -
                    baseline_mean
                expect_equal(
                    actual$wind_speed[january],
                    expected,
                    tolerance = 1e-12
                )
                expect_true(all(actual$method_applied[january] == "shift"))
                expect_true(all(actual$alpha[january] == 0))
                expect_true(all(grepl(
                    "fallback_shift",
                    actual$factor_status[january]
                )))
                expect_equal(
                    actual$wind_speed,
                    data.table::fifelse(
                        actual$method_applied == "shift",
                        epw$wind_speed + actual$delta,
                        epw$wind_speed * actual$alpha
                    ),
                    tolerance = 1e-12
                )
                expect_identical(epw, before)
            }
        }
    }
})

# Versioned recipe identity prevents corrected monthly outputs from reusing
# artifacts whose arithmetic was produced before the fallback fix.
test_that("enhanced monthly recipe invalidates the earlier arithmetic identity", {
    transform <- monthly_transform("epwshiftr")
    expect_identical(transform@recipe_version, 3L)
    expect_identical(method__get("epwshiftr_monthly")@version, 3L)
    for (version in c(1L, 2L)) {
        spec <- transform__spec_value(transform)
        spec$recipe_version <- version
        expect_error(transform__from_spec(spec), "version")
    }
})

# vim: fdm=marker :
