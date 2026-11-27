# local_nc_time_file {{{
local_nc_time_file <- function(time_vals, units, calendar = "standard") {
    path <- tempfile(fileext = ".nc")
    nc <- RNetCDF::create.nc(path)

    RNetCDF::dim.def.nc(nc, "time", length(time_vals))
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "test")
    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", units)
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", calendar)
    RNetCDF::var.put.nc(nc, "time", time_vals, count = length(time_vals))
    RNetCDF::close.nc(nc)

    path
}
# }}}

test_that("parse_cf_time() returns UTC POSIXct with CF metadata", {
    time <- parse_cf_time(
        c(0, 1.5),
        "days since 2000-01-01 00:00:00",
        "standard"
    )

    expect_s3_class(time, "POSIXct")
    expect_equal(as.numeric(time), c(946684800, 946814400))
    expect_identical(attr(time, "tzone"), "UTC")
    expect_identical(attr(time, "cf_units"), "days since 2000-01-01 00:00:00")
    expect_identical(attr(time, "cf_calendar"), "standard")
    expect_s3_class(
        parse_cf_time(c(0, 1), "months since 1850-01", "standard"),
        "POSIXct"
    )
    expect_error(
        parse_cf_time(0, "days since 1850-01-01", "invalid"),
        "Invalid calendar specification"
    )
    expect_error(
        parse_cf_time(0, "months 1850-01", "standard"),
        "CF-compliant time coordinate"
    )
})

test_that("parse_cf_time() returns POSIXct for common CF calendars", {
    calendars <- c(
        "standard",
        "gregorian",
        "proleptic_gregorian",
        "noleap",
        "365_day",
        "360_day"
    )

    for (calendar in calendars) {
        time <- parse_cf_time(58:62, "days since 2000-01-01 00:00:00", calendar)

        expect_true(inherits(time, "POSIXct"), info = calendar)
        expect_identical(attr(time, "tzone"), "UTC", info = calendar)
        expect_identical(
            attr(time, "cf_units"),
            "days since 2000-01-01 00:00:00",
            info = calendar
        )
        expect_identical(attr(time, "cf_calendar"), calendar, info = calendar)
        expect_equal(length(time), 5L, info = calendar)
        expect_equal(
            unname(diff(as.numeric(time))),
            rep(86400, 4L),
            info = calendar
        )
    }
})

test_that("parse_cf_time() preserves fixed-calendar boundaries and aliases", {
    cases <- list(
        list(
            calendars = c("365_day", "noleap"),
            year_days = 365L,
            offsets = c(-366L, -365L, -1L, 0L, 30L, 31L, 58L, 59L, 364L, 365L),
            year = c(
                1999L,
                2000L,
                2000L,
                2001L,
                2001L,
                2001L,
                2001L,
                2001L,
                2001L,
                2002L
            ),
            month = c(12L, 1L, 12L, 1L, 1L, 2L, 2L, 3L, 12L, 1L),
            day = c(31L, 1L, 31L, 1L, 31L, 1L, 28L, 1L, 31L, 1L),
            day_of_year = c(365L, 1L, 365L, 1L, 31L, 32L, 59L, 60L, 365L, 1L)
        ),
        list(
            calendars = c("366_day", "all_leap"),
            year_days = 366L,
            offsets = c(
                -367L,
                -366L,
                -1L,
                0L,
                30L,
                31L,
                58L,
                59L,
                60L,
                365L,
                366L
            ),
            year = c(
                1999L,
                2000L,
                2000L,
                2001L,
                2001L,
                2001L,
                2001L,
                2001L,
                2001L,
                2001L,
                2002L
            ),
            month = c(12L, 1L, 12L, 1L, 1L, 2L, 2L, 2L, 3L, 12L, 1L),
            day = c(31L, 1L, 31L, 1L, 31L, 1L, 28L, 29L, 1L, 31L, 1L),
            day_of_year = c(
                366L,
                1L,
                366L,
                1L,
                31L,
                32L,
                59L,
                60L,
                61L,
                366L,
                1L
            )
        )
    )

    for (case in cases) {
        alias_coordinates <- lapply(case$calendars, function(calendar) {
            time <- parse_cf_time(
                case$offsets,
                "days since 2001-01-01 00:00:00",
                calendar
            )
            coordinates <- attr(time, "cf_coordinates", exact = TRUE)

            expect_identical(coordinates$cf_year, case$year, info = calendar)
            expect_identical(coordinates$cf_month, case$month, info = calendar)
            expect_identical(coordinates$cf_day, case$day, info = calendar)
            expect_identical(
                coordinates$cf_day_of_year,
                case$day_of_year,
                info = calendar
            )
            expect_identical(
                coordinates$cf_year_days,
                rep.int(case$year_days, length(case$offsets)),
                info = calendar
            )

            coordinates[c("cf_year", "cf_month", "cf_day", "cf_day_of_year")]
        })

        expect_identical(alias_coordinates[[1L]], alias_coordinates[[2L]])
    }
})

test_that("parse_cf_time() retains calendar-native coordinates and annual phase", {
    year_lengths <- c(
        standard = 366L,
        gregorian = 366L,
        proleptic_gregorian = 366L,
        noleap = 365L,
        `365_day` = 365L,
        `360_day` = 360L,
        `366_day` = 366L,
        all_leap = 366L
    )

    for (calendar in names(year_lengths)) {
        year_days <- year_lengths[[calendar]]
        time <- parse_cf_time(
            c(0, year_days - 1L, year_days),
            "days since 2000-01-01 00:00:00",
            calendar
        )
        coordinates <- attr(time, "cf_coordinates", exact = TRUE)

        expect_named(coordinates, CF_TIME_COORDINATE_COLUMNS, info = calendar)
        expect_identical(
            coordinates$cf_calendar,
            rep.int(calendar, 3L),
            info = calendar
        )
        expect_identical(
            coordinates$cf_year,
            c(2000L, 2000L, 2001L),
            info = calendar
        )
        expect_identical(coordinates$cf_month, c(1L, 12L, 1L), info = calendar)
        expect_identical(
            coordinates$cf_day,
            c(1L, if (identical(calendar, "360_day")) 30L else 31L, 1L),
            info = calendar
        )
        expect_identical(
            coordinates$cf_day_of_year,
            c(1L, year_days, 1L),
            info = calendar
        )
        expect_identical(
            coordinates$cf_year_days,
            cf_time__year_days(c(2000L, 2000L, 2001L), calendar),
            info = calendar
        )
        expect_equal(
            coordinates$annual_phase,
            c(0, (year_days - 1L) / year_days, 0),
            info = calendar
        )
        expect_true(all(coordinates$annual_phase >= 0), info = calendar)
        expect_true(all(coordinates$annual_phase < 1), info = calendar)
    }
})

test_that("parse_cf_time() preserves native clock seconds across calendar boundaries", {
    for (calendar in CF_TIME_CALENDARS) {
        year_seconds <- cf_time__year_days(2000L, calendar) * 86400
        offsets <- c(
            -0.25,
            0,
            0.125,
            3661.25,
            86399.75,
            86400,
            year_seconds - 0.25,
            year_seconds + 3661.25
        )
        time <- parse_cf_time(
            offsets,
            "seconds since 2000-01-01 00:00:00",
            calendar
        )
        coordinates <- attr(time, "cf_coordinates", exact = TRUE)

        # Expected native clock values are independent of surrogate POSIX dates.
        expect_identical(
            coordinates$cf_second_of_day,
            c(86399.75, 0, 0.125, 3661.25, 86399.75, 0, 86399.75, 3661.25),
            info = calendar
        )
        expect_identical(
            coordinates$cf_year,
            c(1999L, rep.int(2000L, 6L), 2001L),
            info = calendar
        )
        expect_equal(
            coordinates$annual_phase,
            (coordinates$cf_day_of_year -
                1 +
                coordinates$cf_second_of_day / 86400) /
                coordinates$cf_year_days,
            info = calendar
        )
    }
})

test_that("EsgDataset$get_time_axis() shares CF time parsing", {
    path <- local_nc_time_file(
        time_vals = c(0, 1.5),
        units = "days since 2000-01-01 00:00:00",
        calendar = "standard"
    )
    on.exit(unlink(path), add = TRUE)

    expected <- as.POSIXct(
        c("2000-01-01 00:00:00", "2000-01-02 12:00:00"),
        tz = "UTC"
    )

    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    time_info <- ds$get_time_axis()
    expect_s3_class(time_info$values, "POSIXct")
    expect_equal(time_info$length, 2L)
    expect_identical(time_info$units, "days since 2000-01-01 00:00:00")
    expect_identical(time_info$calendar, "standard")
    expect_identical(time_info$coordinates$cf_second_of_day, c(0, 43200))
    expect_equal(as.numeric(time_info$values), as.numeric(expected))
    expect_identical(attr(time_info$values, "cf_units"), time_info$units)
    expect_identical(attr(time_info$values, "cf_calendar"), time_info$calendar)
    expect_identical(
        time_info$coordinates,
        attr(time_info$values, "cf_coordinates", exact = TRUE)
    )
})

test_that("EsgDataset$get_time_axis() returns POSIXct for 360_day", {
    path <- local_nc_time_file(
        time_vals = 58:62,
        units = "days since 2000-01-01 00:00:00",
        calendar = "360_day"
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    time_info <- ds$get_time_axis()
    expect_s3_class(time_info$values, "POSIXct")
    expect_identical(time_info$units, "days since 2000-01-01 00:00:00")
    expect_identical(time_info$calendar, "360_day")
    expect_equal(unname(diff(as.numeric(time_info$values))), rep(86400, 4L))
    expect_identical(attr(time_info$values, "cf_units"), time_info$units)
    expect_identical(attr(time_info$values, "cf_calendar"), time_info$calendar)
    expect_identical(
        time_info$coordinates,
        attr(time_info$values, "cf_coordinates", exact = TRUE)
    )
    expect_identical(time_info$coordinates$cf_month, c(2L, 2L, 3L, 3L, 3L))
    expect_identical(time_info$coordinates$cf_day, c(29L, 30L, 1L, 2L, 3L))
})

# Julian century leap days must remain native dates, including subdaily and
# negative offsets; surrogate POSIX timestamps do not redefine the calendar.
test_that("Julian CF coordinates preserve century leap days and round trips", {
    for (year in c(1900L, 2100L)) {
        units <- sprintf("hours since %d-02-28 12:00:00", year)
        time <- parse_cf_time(c(-12, 0, 12, 36, 60), units, "julian")
        fields <- attr(time, "cf_coordinates")
        expect_identical(fields$cf_year, rep.int(year, 5L))
        expect_identical(fields$cf_month, c(2L, 2L, 2L, 3L, 3L))
        expect_identical(fields$cf_day, c(28L, 28L, 29L, 1L, 2L))
        expect_identical(fields$cf_year_days, rep.int(366L, 5L))
        expect_equal(fields$cf_second_of_day, c(0, 43200, 0, 0, 0))
        expect_equal(
            fields$annual_phase,
            (fields$cf_day_of_year - 1 + fields$cf_second_of_day / 86400) / 366
        )
        origin <- cf_time_parse_origin(sprintf("%d-01-01", year), "julian")
        native <- data.frame(
            year = fields$cf_year,
            month = fields$cf_month,
            day = fields$cf_day
        )
        offsets <- cf_time_date2offset(native, origin, "julian")
        expect_equal(cf_time_offset2date(offsets, origin, "julian"), native)
        leap_origin <- parse_cf_time(
            c(-1, 0, 1),
            sprintf("days since %d-02-29", year),
            "julian"
        )
        expect_identical(
            attr(leap_origin, "cf_coordinates")$cf_day,
            c(28L, 29L, 1L)
        )
    }
    time <- parse_cf_time(54787, "days since 1850-01-01", "julian")
    expect_identical(attr(time, "cf_coordinates")$cf_year, 2000L)
    expect_identical(attr(time, "cf_coordinates")$cf_day_of_year, 1L)
    expect_error(parse_cf_time(0, "days since 1901-02-29", "julian"))
})


# CF Julian reference dates and computed ordinary dates cannot use negative
# years; climatological year zero needs its separate CF interpretation.
test_that("Julian ordinary dates reject unsupported epoch crossings", {
    time <- parse_cf_time(c(-1, 0), "days since 0001-01-02", "julian")
    fields <- attr(time, "cf_coordinates")
    expect_identical(fields$cf_year, c(1L, 1L))
    expect_identical(fields$cf_day, c(1L, 2L))
    expect_error(
        parse_cf_time(-1, "days since 0001-01-01", "julian"),
        "positive years"
    )
    expect_error(
        parse_cf_time(0, "days since 0000-01-01", "julian"),
        "base date"
    )
    expect_error(
        parse_cf_time(0, "days since -0001-01-01", "julian"),
        "base date"
    )
    months <- parse_cf_time(c(0, 12), "months since 1900-03-01", "julian")
    years <- parse_cf_time(c(0, 1), "years since 1900-03-01", "julian")
    expect_identical(
        attr(months, "cf_coordinates"),
        attr(years, "cf_coordinates")
    )
})


# Zone offsets locate the reference instant; the display zone must not change
# that instant or the canonical UTC calendar fields.
test_that("CF normalization applies reference zones independently of display zones", {
    for (calendar in CF_TIME_CALENDARS) {
        for (zone in c("+02:00", "+0200", "+2", "+02")) {
            units <- paste("hours since 2000-01-01 00:00:00", zone)
            utc <- parse_cf_time(c(0, 2), units, calendar)
            local <- parse_cf_time(
                c(0, 2),
                units,
                calendar,
                tz = "Asia/Shanghai"
            )
            expect_equal(as.numeric(utc), c(946677600, 946684800))
            expect_equal(as.numeric(local), as.numeric(utc))
            expect_identical(
                attr(local, "cf_coordinates"),
                attr(utc, "cf_coordinates")
            )
            expect_identical(attr(utc, "cf_units"), units)
            fields <- attr(utc, "cf_coordinates")
            expect_identical(fields$cf_year, c(1999L, 2000L))
            expect_equal(fields$cf_second_of_day, c(79200, 0))
        }
    }
    negative <- parse_cf_time(0, "hours since 2000-01-01 00:00:00 -6")
    expect_equal(as.numeric(negative), 946706400)
    expect_error(
        parse_cf_time(0, "hours since 2000-01-01 00:00:00 +25:00"),
        "zone"
    )
    expect_error(
        parse_cf_time(0, "hours since 2000-01-01 00:00:00 +02:99"),
        "zone"
    )
})

# Mixed-calendar arithmetic must cross the Gregorian reform as one day and
# reject dates that never existed, even when the epoch predates modern data.
test_that("CF normalization preserves the standard calendar reform and ancient epochs", {
    for (calendar in c("standard", "gregorian")) {
        time <- parse_cf_time(-1:2, "days since 1582-10-04", calendar)
        fields <- attr(time, "cf_coordinates")
        expect_identical(fields$cf_day, c(3L, 4L, 15L, 16L))
        expect_identical(fields$cf_day_of_year, 276:279)
        expect_identical(fields$cf_year_days, rep.int(355L, 4L))
        expect_equal(diff(as.numeric(time)), rep(86400, 3L))
        leap <- parse_cf_time(0:2, "days since 1500-02-28", calendar)
        expect_identical(attr(leap, "cf_coordinates")$cf_day, c(28L, 29L, 1L))
        modern <- parse_cf_time(730121, "days since 0001-01-01", calendar)
        expect_equal(as.numeric(modern), 946684800)
        for (origin in c(
            "1582-10-05",
            "1582-10-10",
            "1582-10-14",
            "0000-01-01",
            "-0001-01-01"
        )) {
            expect_error(
                parse_cf_time(0, paste("days since", origin), calendar),
                "base date"
            )
        }
        expect_error(
            parse_cf_time(-1, "days since 0001-01-01", calendar),
            "positive years"
        )
        dates <- data.frame(
            year = c(1L, 1500L, 1582L, 1582L, 2000L),
            month = c(1L, 2L, 10L, 10L, 1L),
            day = c(1L, 29L, 4L, 15L, 1L)
        )
        origin <- cf_time_parse_origin("0001-01-01", calendar)
        expect_equal(
            cf_time_offset2date(
                cf_time_date2offset(dates, origin, calendar),
                origin,
                calendar
            ),
            dates
        )
    }
    proleptic <- parse_cf_time(
        0:2,
        "days since 1582-10-04",
        "proleptic_gregorian"
    )
    expect_identical(attr(proleptic, "cf_coordinates")$cf_day, 4:6)
})

# UDUNITS months and years are fixed physical durations, irrespective of the
# model calendar or the sampling frequency of monthly climate output.
test_that("CF normalization interprets months and years as UDUNITS durations", {
    year_seconds <- 365.242198781 * 86400
    for (calendar in CF_TIME_CALENDARS) {
        epoch <- if (calendar == "360_day") "2000-01-30" else "2000-01-31"
        months <- parse_cf_time(
            c(-1.25, 0, 1, 12),
            paste("months since", epoch),
            calendar
        )
        seconds <- parse_cf_time(
            c(-1.25, 0, 1, 12) * year_seconds / 12,
            paste("seconds since", epoch),
            calendar
        )
        expect_equal(as.numeric(months), as.numeric(seconds))
        expect_equal(
            attr(months, "cf_coordinates"),
            attr(seconds, "cf_coordinates")
        )
        years <- parse_cf_time(
            c(-1, 0, .5, 1),
            "years since 2000-01-01",
            calendar
        )
        expect_equal(
            diff(as.numeric(years)),
            c(year_seconds, year_seconds / 2, year_seconds / 2)
        )
    }
    month <- parse_cf_time(1, "months since 2000-01-31")
    expect_identical(attr(month, "cf_coordinates")$cf_month, 3L)
    expect_identical(attr(month, "cf_coordinates")$cf_day, 1L)
    expect_error(
        parse_cf_time(0, "fortnights since 2000-01-01"),
        "Unsupported CF time unit"
    )
})

# Preserve available double precision without manufacturing a 86400th second
# in a day; fractional and negative offsets retain consistent native dates.
test_that("CF normalization preserves submillisecond precision and midnight carries", {
    offsets <- c(-.0004, 0, .0004, 86399.9996, 86400, 86400.0004)
    for (calendar in CF_TIME_CALENDARS) {
        time <- parse_cf_time(offsets, "seconds since 2000-01-01", calendar)
        fields <- attr(time, "cf_coordinates")
        expect_identical(
            fields$cf_day,
            c(if (calendar == "360_day") 30L else 31L, 1L, 1L, 1L, 2L, 2L)
        )
        expect_true(all(
            fields$cf_second_of_day >= 0 & fields$cf_second_of_day < 86400
        ))
        expect_lt(
            max(abs(
                fields$cf_second_of_day -
                    c(86399.9996, 0, .0004, 86399.9996, 0, .0004)
            )),
            1e-10
        )
        expect_equal(as.numeric(time) - 946684800, offsets, tolerance = 1e-7)
        empty <- parse_cf_time(numeric(), "seconds since 2000-01-01", calendar)
        expect_length(empty, 0L)
        expect_equal(nrow(attr(empty, "cf_coordinates")), 0L)
        near <- parse_cf_time(c(0, .0004, .0008), "seconds since 2000-01-01")
        expect_identical(
            cf_time__range_indices(
                near,
                attr(near, "cf_coordinates"),
                as.POSIXct(
                    c(946684800.0002, 946684800.0006),
                    origin = "1970-01-01",
                    tz = "UTC"
                )
            ),
            2L
        )
        micro <- parse_cf_time(
            c(0, .000002, .000004),
            "seconds since 2000-01-01"
        )
        expect_identical(
            cf_time__range_indices(
                micro,
                attr(micro, "cf_coordinates"),
                as.POSIXct(
                    c(946684800.000001, 946684800.000003),
                    origin = "1970-01-01",
                    tz = "UTC"
                )
            ),
            2L
        )
        expect_identical(
            dataset__neighborhood_time_indices(
                micro,
                attr(micro, "cf_coordinates"),
                c("2000-01-01T00:00:00.000001", "2000-01-01T00:00:00.000003")
            ),
            2L
        )
    }
})

# vim: fdm=marker :
