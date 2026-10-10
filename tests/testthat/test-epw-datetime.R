# Compare the public timestamp behavior with the established string parser,
# including unusual inputs that remain on the compatibility path.
test_that("EPW numeric timestamps preserve Gregorian dates and UTC attributes", {
    # The former implementation is an independent oracle for date conversion.
    reference <- function(year, month, day, hour) {
        safe_year <- as.integer(year)
        safe_year[
            is.na(safe_year) | safe_year < 1600L | safe_year > 9999L
        ] <- 2001L
        start <- as.POSIXct(
            sprintf(
                "%04d-%02d-%02d 00:00:00",
                safe_year,
                as.integer(month),
                as.integer(day)
            ),
            tz = "UTC"
        )
        start + as.numeric(hour) * 3600
    }
    # A complete Gregorian leap cycle covers ordinary, century, and 400-year
    # leap years, with every day checked through the original interface.
    dates <- as.POSIXlt(
        seq(as.Date("1800-01-01"), as.Date("2199-12-31"), by = "day"),
        tz = "UTC"
    )
    args <- list(
        dates$year + 1900L,
        dates$mon + 1L,
        dates$mday,
        rep(c(0, 1, 12.5, 24), length.out = length(dates))
    )
    expect_identical(
        do.call(epw_file__datetime, args),
        do.call(reference, args)
    )
    examples <- list(
        list(
            c(1600, 1900, 2000, 2100, 9999),
            rep(12, 5),
            rep(31, 5),
            rep(24, 5)
        ),
        list(
            c(NA, 0, 1599, 10000, 2000),
            rep(2, 5),
            rep(28, 5),
            c(NA, -1, 24, 25, Inf)
        ),
        list(integer(), integer(), integer(), integer()),
        list(c(2000, 2001), c(2, NA), c(29, 1), c(1, 24)),
        list(2000, c(1, 2), c(1, 29), c(1, 24)),
        list(c(2000, 2001), 1, 1, c(0, 24)),
        list(c(2000, 2000), c(2, 2), c(30, 31), c(1, 24)),
        list(c(2000, 2000), c(0, 13), c(1, 1), c(1, 24)),
        list(c(2000, 2000), c(1, 1), c(0, 32), c(1, 24)),
        list(NA_integer_, NA_integer_, NA_integer_, NA_real_),
        list(c(2000.9, 2001.2), c(2.5, 1.5), c(29.5, 1.5), c(1, 24)),
        list(c(a = 2000, b = 2001), c(1, 1), c(1, 1), c(1, 24))
    )
    # Errors are part of the existing behavior when no input date can parse.
    capture <- function(fun, args) {
        tryCatch(do.call(fun, args), error = function(e) {
            list(class = class(e), message = conditionMessage(e))
        })
    }
    for (args in examples) {
        expect_identical(
            capture(epw_file__datetime, args),
            capture(reference, args)
        )
    }
    withr::local_timezone("America/New_York")
    expect_identical(
        epw_file__datetime(2020, 3, 8, 2),
        reference(2020, 3, 8, 2)
    )
})

# vim: fdm=marker :
