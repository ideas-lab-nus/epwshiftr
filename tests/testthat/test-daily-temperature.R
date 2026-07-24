# Build deterministic native-calendar tas/tasmax/tasmin rows for target tests.
daily_test__temperature_source <- function(
    calendar_days, mean_shift = 0, minimum_shift = 0, maximum_shift = 0,
    include_extrema = TRUE
) {
    data.table::rbindlist(lapply(names(calendar_days), function(calendar) {
        phase <- daily__phase_grid(calendar_days[[calendar]])
        mean_value <- 15 + 7 * sin(2 * pi * phase)
        values <- list(
            tas = mean_value + mean_shift
        )
        if (isTRUE(include_extrema)) {
            values$tasmin <- mean_value - 4 + minimum_shift
            values$tasmax <- mean_value + 5 + maximum_shift
        }
        data.table::rbindlist(lapply(names(values), function(variable_id) {
            data.table::data.table(
                calendar = calendar,
                variable_id = variable_id,
                annual_phase = phase,
                value = values[[variable_id]]
            )
        }))
    }))
}

test_that("daily temperature targets recover mean and DTR changes across calendars", {
    calendar_days <- c(`360_day` = 360L, `365_day` = 365L, `366_day` = 366L)
    historical <- daily_test__temperature_source(calendar_days)
    future <- daily_test__temperature_source(
        calendar_days,
        mean_shift = 2,
        minimum_shift = 1,
        maximum_shift = 3
    )
    data.table::set(future, j = "source", value = future[["calendar"]])
    data.table::set(
        historical, j = "source", value = historical[["calendar"]]
    )
    original_future <- data.table::copy(future)
    original_historical <- data.table::copy(historical)

    targets <- daily__temperature_targets(
        future,
        historical,
        by = c("calendar", "source")
    )

    expect_identical(future, original_future)
    expect_identical(historical, original_historical)
    expect_identical(
        targets[, .N, by = calendar]$N,
        rep.int(365L, 3L)
    )
    expect_equal(targets$mean_delta, rep(2, nrow(targets)), tolerance = 1e-12)
    expect_equal(targets$minimum_delta, rep(1, nrow(targets)), tolerance = 1e-12)
    expect_equal(targets$maximum_delta, rep(3, nrow(targets)), tolerance = 1e-12)
    expect_equal(targets$dtr_delta, rep(2, nrow(targets)), tolerance = 1e-12)
    expect_true(all(targets$dtr_status == "adjusted"))
    expect_true(all(targets$n_future_mean > 0L))
    expect_true(all(targets$n_historical_mean > 0L))
})

test_that("daily temperature targets inherit DTR when extrema are unavailable", {
    calendar_days <- c(`365_day` = 365L)
    historical <- daily_test__temperature_source(
        calendar_days,
        include_extrema = FALSE
    )
    future <- daily_test__temperature_source(
        calendar_days,
        mean_shift = 2.5,
        include_extrema = FALSE
    )

    targets <- daily__temperature_targets(future, historical)

    expect_equal(targets$mean_delta, rep(2.5, 365L), tolerance = 1e-12)
    expect_true(all(is.na(targets$minimum_delta)))
    expect_true(all(is.na(targets$maximum_delta)))
    expect_true(all(is.na(targets$dtr_delta)))
    expect_true(all(targets$dtr_status == "inherited_missing_extremes"))
})

test_that("daily temperature projection closes feasible mean and extrema targets", {
    hour <- 1:24
    shape <- 5 * sin(2 * pi * (hour - 1) / 24)
    template <- data.table::data.table(
        site = rep("A", 48L),
        target_day = rep(1:2, each = 24L),
        hour = rep(hour, 2L),
        value = c(20 + shape, 22 + shape)
    )
    template <- template[c(24:1, 48:25)]
    original <- data.table::copy(template)
    targets <- data.table::data.table(
        site = "A",
        target_day = 1:2,
        mean_delta = c(1, 3),
        minimum_delta = c(1, 1),
        maximum_delta = c(3, 3),
        dtr_status = "adjusted"
    )

    projected <- daily__project_temperature(
        template,
        targets,
        by = "site"
    )

    expect_identical(template, original)
    expect_identical(nrow(projected), 48L)
    expect_identical(
        projected[, names(template), with = FALSE],
        template
    )
    expect_equal(
        projected[, unique(projected_mean), by = target_day]$V1,
        c(21, 25),
        tolerance = 1e-9
    )
    expect_equal(
        projected[, unique(projected_minimum), by = target_day]$V1,
        c(16, 18),
        tolerance = 1e-9
    )
    expect_equal(
        projected[, unique(projected_maximum), by = target_day]$V1,
        c(28, 30),
        tolerance = 1e-9
    )
    expect_true(all(projected$projection_status == "projected"))
    expect_true(all(is.finite(projected$boundary_jump)))
    expect_true(all(is.finite(projected$boundary_jump_change)))

    for (day_value in 1:2) {
        source_day <- template[target_day == day_value]
        projected_day <- projected[target_day == day_value]
        value_order <- order(source_day$value)

        expect_true(all(diff(projected_day$temperature_projected[value_order]) >= -1e-12))
        expect_identical(
            which.min(projected_day$temperature_projected),
            which.min(source_day$value)
        )
        expect_identical(
            which.max(projected_day$temperature_projected),
            which.max(source_day$value)
        )
    }
})

test_that("daily temperature projection records inherited and flat-template fallbacks", {
    hour <- 1:24
    source <- 18 + 4 * sin(2 * pi * (hour - 1) / 24)
    inherited <- daily__project_temperature(
        data.table::data.table(target_day = 1L, hour = hour, value = source),
        data.table::data.table(
            target_day = 1L,
            mean_delta = 2,
            minimum_delta = NA_real_,
            maximum_delta = NA_real_,
            dtr_status = "inherited_missing_extremes"
        )
    )
    expect_equal(inherited$temperature_projected, source + 2)
    expect_true(all(inherited$projection_status == "shift_inherited_dtr"))

    flat <- daily__project_temperature(
        data.table::data.table(target_day = 1L, hour = hour, value = rep(20, 24L)),
        data.table::data.table(
            target_day = 1L,
            mean_delta = 2,
            minimum_delta = 1,
            maximum_delta = 3,
            dtr_status = "adjusted"
        )
    )
    expect_equal(flat$temperature_projected, rep(22, 24L))
    expect_true(all(flat$projection_status == "fallback_shift_flat_template"))
    expect_equal(unique(flat$target_minimum), 21)
    expect_equal(unique(flat$target_maximum), 23)
})

test_that("daily temperature projection rejects infeasible targets and incomplete days", {
    hour <- 1:24
    source <- 20 + 5 * sin(2 * pi * (hour - 1) / 24)
    target <- data.table::data.table(
        target_day = 1L,
        mean_delta = 20,
        minimum_delta = 0,
        maximum_delta = 0,
        dtr_status = "adjusted"
    )

    expect_error(
        daily__project_temperature(
            data.table::data.table(target_day = 1L, hour = hour, value = source),
            target
        ),
        "mean must lie between"
    )
    expect_error(
        daily__project_temperature(
            data.table::data.table(
                target_day = 1L,
                hour = hour[-1L],
                value = source[-1L]
            ),
            target
        ),
        "exactly 24 unique"
    )
})
