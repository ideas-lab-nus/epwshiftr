test_that("daily phase grid uses calendar-neutral day midpoints", {
    phase <- daily__phase_grid(365L)

    expect_length(phase, 365L)
    expect_equal(phase[c(1L, 365L)], c(0.5, 364.5) / 365)
    expect_true(all(phase >= 0 & phase < 1))
})

test_that("daily phase windows wrap and retain their requested odd width", {
    phase <- daily__phase_grid(365L)

    for (window_days in c(21L, 31L, 61L)) {
        selected <- daily__phase_window(
            phase,
            phase[[1L]],
            window_days = window_days
        )
        radius <- window_days %/% 2L

        expect_identical(sum(selected), window_days)
        expect_true(all(selected[seq_len(radius + 1L)]))
        expect_true(all(selected[(365L - radius + 1L):365L]))
    }

    expect_equal(
        daily__phase_distance(c(0.99, 0.01, 0.5), 0),
        c(0.01, 0.01, 0.5)
    )
})

test_that("daily climatology maps native calendars onto one target grid", {
    calendar_days <- c(`360_day` = 360L, `365_day` = 365L, `366_day` = 366L)
    source <- data.table::rbindlist(lapply(names(calendar_days), function(calendar) {
        year_days <- calendar_days[[calendar]]
        phase <- daily__phase_grid(year_days)
        data.table::data.table(
            calendar = calendar,
            annual_phase = phase,
            value = 12 + 6 * sin(2 * pi * phase) + 2 * cos(4 * pi * phase)
        )
    }))

    mapped <- daily__climatology(
        source,
        by = "calendar",
        window_days = 31L,
        target_year_days = 365L
    )
    curves <- split(mapped$climatology, mapped$calendar)

    expect_identical(
        vapply(curves, length, integer(1L)),
        stats::setNames(rep.int(365L, 3L), names(calendar_days))
    )
    # A boxcar boundary can admit one different native-calendar sample. The
    # resulting discretization stays below 0.065 for this eight-unit signal.
    expect_lt(max(abs(curves[["360_day"]] - curves[["365_day"]])), 0.065)
    expect_lt(max(abs(curves[["366_day"]] - curves[["365_day"]])), 0.065)
})

test_that("daily climatology preserves groups, counts values, and does not mutate", {
    phase <- daily__phase_grid(365L)
    source <- data.table::data.table(
        site = rep(c("complete", "missing"), each = 365L),
        annual_phase = rep(phase, 2L),
        value = c(rep(2, 365L), rep(NA_real_, 365L))
    )
    source$value[[1L]] <- NA_real_
    original <- data.table::copy(source)

    result <- daily__climatology(source, by = "site")
    complete_day_one <- result[site == "complete" & target_day == 1L]
    missing <- result[site == "missing"]

    expect_identical(source, original)
    expect_named(
        result,
        c("site", "target_day", "annual_phase", "climatology", "n")
    )
    expect_identical(complete_day_one$n, 30L)
    expect_identical(complete_day_one$climatology, 2)
    expect_true(all(missing$n == 0L))
    expect_true(all(is.na(missing$climatology)))
})

test_that("daily climatology validates phases, windows, and columns", {
    valid <- data.frame(annual_phase = c(0.1, 0.2), value = c(1, 2))

    expect_error(daily__phase_grid(365.5), "integerish")
    expect_error(daily__phase_window(c(0.1, 1), 0.1), "\\[0, 1\\)")
    expect_error(daily__phase_window(0.1, c(0.1, 0.2)), "one annual-phase")
    expect_error(daily__phase_window(0.1, 0.1, window_days = 30L), "must be odd")
    expect_error(
        daily__phase_window(0.1, 0.1, window_days = 367L),
        "must not exceed"
    )
    expect_error(daily__climatology(valid[0L, ]), "at least one observation")
    expect_error(daily__climatology(valid, value = "tas"), "missing required")
    expect_error(daily__climatology(valid, by = "annual_phase"), "cannot use")
})
