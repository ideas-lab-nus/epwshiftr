test_that("Eames transfer weights implement the published equation", {
    normalized <- c(0, 0.25, 0.5, 0.75, 1)

    expect_equal(
        btws__transfer_weight(normalized, 1, 1),
        normalized * (1 - normalized),
        tolerance = 0
    )
    expect_identical(
        btws__transfer_weight(c(0, 1), 0, 0),
        c(0, 0)
    )
})

test_that("Eames default projection follows equations 7 to 16", {
    hour <- 1:24
    source <- 20 + 5 * sin(2 * pi * (hour - 1) / 24)
    baseline_minimum <- min(source)
    baseline_range <- max(source) - baseline_minimum
    normalized <- (source - baseline_minimum) / baseline_range
    target_normalized_mean <- (
        mean(source) + 0.5 - baseline_minimum
    ) / baseline_range
    scale <- target_normalized_mean / mean(normalized) - 1
    weight <- normalized * (1 - normalized)
    expected_normalized <- normalized +
        scale * mean(normalized) * weight / mean(weight)

    projected <- btws__project_temperature_day(
        source,
        mean_delta = 0.5,
        minimum_delta = 0,
        maximum_delta = 0,
        dtr_status = "adjusted",
        tolerance = 1e-8
    )

    expect_identical(projected$status, "projected_btws")
    expect_equal(projected$scale, scale, tolerance = 1e-12)
    expect_identical(projected$m, 1)
    expect_identical(projected$n, 1)
    expect_equal(
        projected$value,
        baseline_minimum + baseline_range * expected_normalized,
        tolerance = 1e-12
    )
    expect_equal(mean(projected$value), mean(source) + 0.5, tolerance = 1e-10)
    expect_equal(min(projected$value), min(source), tolerance = 1e-10)
    expect_equal(max(projected$value), max(source), tolerance = 1e-10)
})

test_that("Eames zero change is an exact identity", {
    source <- 17 + 6 * sin(2 * pi * (0:23) / 24)
    projected <- btws__project_temperature_day(
        source,
        mean_delta = 0,
        minimum_delta = 0,
        maximum_delta = 0,
        dtr_status = "adjusted",
        tolerance = 1e-8
    )

    expect_equal(projected$value, source, tolerance = 0)
    expect_equal(projected$scale, 0, tolerance = 1e-15)
    expect_identical(projected$m, 1)
    expect_identical(projected$n, 1)
    expect_true(is.na(projected$fallback_reason))
})

test_that("Eames projection reduces the relevant exponent to retain bounds", {
    source <- 20 + 5 * sin(2 * pi * (0:23) / 24)
    warmer_mean <- btws__project_temperature_day(
        source,
        mean_delta = 3,
        minimum_delta = 0,
        maximum_delta = 2,
        dtr_status = "adjusted",
        tolerance = 1e-8
    )
    cooler_mean <- btws__project_temperature_day(
        source,
        mean_delta = -3,
        minimum_delta = -2,
        maximum_delta = 0,
        dtr_status = "adjusted",
        tolerance = 1e-8
    )

    expect_identical(warmer_mean$status, "projected_btws_adjusted_m")
    expect_true(warmer_mean$m > 0 && warmer_mean$m < 1)
    expect_identical(warmer_mean$n, 1)
    expect_equal(mean(warmer_mean$value), mean(source) + 3, tolerance = 1e-8)
    expect_equal(min(warmer_mean$value), min(source), tolerance = 1e-8)
    expect_equal(max(warmer_mean$value), max(source) + 2, tolerance = 1e-8)

    expect_identical(cooler_mean$status, "projected_btws_adjusted_n")
    expect_identical(cooler_mean$m, 1)
    expect_true(cooler_mean$n > 0 && cooler_mean$n < 1)
    expect_equal(mean(cooler_mean$value), mean(source) - 3, tolerance = 1e-8)
    expect_equal(min(cooler_mean$value), min(source) - 2, tolerance = 1e-8)
    expect_equal(max(cooler_mean$value), max(source), tolerance = 1e-8)
})

test_that("Eames projection reports mean-shift fallbacks", {
    source <- 20 + 5 * sin(2 * pi * (0:23) / 24)
    infeasible <- btws__project_temperature_day(
        source,
        mean_delta = 20,
        minimum_delta = 0,
        maximum_delta = 0,
        dtr_status = "adjusted",
        tolerance = 1e-8
    )
    binary <- rep(c(10, 20), each = 12L)
    no_interior <- btws__project_temperature_day(
        binary,
        mean_delta = 1,
        minimum_delta = 0,
        maximum_delta = 0,
        dtr_status = "adjusted",
        tolerance = 1e-8
    )

    expect_identical(infeasible$status, "fallback_shift_mean")
    expect_identical(infeasible$fallback_reason, "infeasible_targets")
    expect_equal(infeasible$value, source + 20, tolerance = 0)
    expect_true(is.na(infeasible$scale))

    expect_identical(no_interior$status, "fallback_shift_mean")
    expect_identical(no_interior$fallback_reason, "no_admissible_m")
    expect_equal(no_interior$value, binary + 1, tolerance = 0)
})

test_that("grouped Eames projection retains method diagnostics and row order", {
    hour <- 1:24
    source <- 20 + 5 * sin(2 * pi * (hour - 1) / 24)
    template <- data.table::data.table(
        target_day = rep(1:2, each = 24L),
        hour = rep(hour, 2L),
        value = c(source, source + 1)
    )
    template <- template[c(24:1, 48:25)]
    targets <- data.table::data.table(
        target_day = 1:2,
        mean_delta = c(0.5, -0.5),
        minimum_delta = 0,
        maximum_delta = 0,
        dtr_status = "adjusted"
    )

    projected <- btws__project_temperature(template, targets)
    power <- daily__project_temperature(template, targets)

    expect_identical(
        projected[, names(template), with = FALSE],
        template
    )
    expect_true(all(c(
        "btws_scale", "btws_m", "btws_n", "btws_fallback_reason",
        "boundary_jump", "boundary_jump_change"
    ) %in% names(projected)))
    expect_false("shape_exponent" %in% names(projected))
    expect_true("shape_exponent" %in% names(power))
    expect_false("btws_scale" %in% names(power))
    expect_false(isTRUE(all.equal(
        projected$temperature_projected,
        power$temperature_projected,
        tolerance = 1e-12
    )))
    expect_equal(
        projected$projected_mean,
        power$projected_mean,
        tolerance = 1e-9
    )
    expect_equal(
        projected$projected_minimum,
        power$projected_minimum,
        tolerance = 1e-9
    )
    expect_equal(
        projected$projected_maximum,
        power$projected_maximum,
        tolerance = 1e-9
    )
    expect_true(all(is.finite(projected$boundary_jump)))
    expect_true(all(is.finite(projected$boundary_jump_change)))
})
