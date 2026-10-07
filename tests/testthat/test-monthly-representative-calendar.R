# Build a representative EPW year with known monthly signals. Source-year labels
# are deliberately independent of the physical month/day ordering of the year.
monthly_calendar_test__year <- function(mixed = FALSE) {
    time <- seq(
        as.POSIXct("2001-01-01", tz = "UTC"),
        by = "hour",
        length.out = 8760L
    )
    month <- as.integer(format(time, "%m"))
    day <- as.integer(format(time, "%d"))
    hour <- as.integer(format(time, "%H")) + 1L
    year <- if (mixed) {
        c(
            1991L,
            1985L,
            1994L,
            1982L,
            1993L,
            1987L,
            1992L,
            1981L,
            1990L,
            1983L,
            1989L,
            1986L
        )[month]
    } else {
        rep.int(2001L, length(time))
    }
    data.table::data.table(
        datetime = as.POSIXct(
            sprintf("%d-%02d-%02d", year, month, day),
            tz = "UTC"
        ) +
            hour * 3600,
        year = year,
        month = month,
        day = day,
        hour = hour,
        minute = 60L,
        temperature = 20 + 5 * sin(2 * pi * (hour - 8) / 24),
        epw_mean = 20,
        delta_target = sin(2 * pi * month / 12),
        alpha_target = 0.2 * cos(2 * pi * month / 12),
        method_applied = "combined"
    )
}

# A TMY's source years must not change its cyclic factors. Reversed input order
# additionally checks that smoothing restores caller order after calendar sorting.
test_that("monthly smoothing follows the representative calendar across source years", {
    ordinary <- monthly_calendar_test__year()
    mixed <- monthly_calendar_test__year(mixed = TRUE)
    before <- data.table::copy(mixed)
    for (method in c("shift", "combined", "stretch")) {
        for (width in c(72L, 73L)) {
            expected <- morpher__smooth_enhanced_factors(
                ordinary,
                "temperature",
                method,
                width
            )
            actual <- morpher__smooth_enhanced_factors(
                mixed,
                "temperature",
                method,
                width
            )
            expect_equal(actual$delta, expected$delta, tolerance = 1e-12)
            expect_equal(actual$alpha, expected$alpha, tolerance = 1e-12)
            expect_identical(actual$datetime, mixed$datetime)
            reverse <- nrow(mixed):1L
            reordered <- morpher__smooth_enhanced_factors(
                mixed[reverse],
                "temperature",
                method,
                width
            )
            expect_equal(
                reordered$delta,
                expected$delta[reverse],
                tolerance = 1e-12
            )
            expect_equal(
                reordered$alpha,
                expected$alpha[reverse],
                tolerance = 1e-12
            )
            expect_identical(reordered$datetime, mixed$datetime[reverse])
        }
    }
    expect_identical(mixed, before)
})

# vim: fdm=marker :
