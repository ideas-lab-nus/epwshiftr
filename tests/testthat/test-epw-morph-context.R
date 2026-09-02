test_that("morpher calendar columns prefer CF identity with legacy fallback", {
    climate <- data.table::data.table(
        time = as.POSIXct(
            c("2060-12-25 12:00:00", "2060-12-26 12:00:00"),
            tz = "UTC"
        ),
        year = c(2060L, 2062L),
        cf_year = c(2061L, NA_integer_),
        cf_month = c(1L, NA_integer_),
        cf_day = c(1L, NA_integer_)
    )

    resolved <- morpher__resolve_calendar_columns(
        climate,
        month = TRUE,
        day = TRUE
    )

    expect_identical(resolved$year, c(2061L, 2062L))
    expect_identical(resolved$month, c(1L, 12L))
    expect_identical(resolved$day, c(1L, 26L))

    legacy <- data.table::data.table(
        time = as.POSIXct("2061-02-03 12:00:00", tz = "UTC")
    )
    legacy <- morpher__resolve_calendar_columns(
        legacy,
        month = TRUE,
        day = TRUE
    )
    expect_identical(legacy$year, 2061L)
    expect_identical(legacy$month, 2L)
    expect_identical(legacy$day, 3L)
})
