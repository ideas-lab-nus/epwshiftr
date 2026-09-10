test_that("BWS preserves both bounds and closes an attainable mean", {
    source <- c(0, 10, seq(1, 9, length.out = 22L))
    projected <- bws__project(
        source,
        target_mean = mean(source) + 0.5,
        lower = 0,
        upper = 10
    )

    expect_equal(projected$value[source == 0], 0, tolerance = 0)
    expect_equal(projected$value[source == 10], 10, tolerance = 0)
    expect_true(all(projected$value >= 0 & projected$value <= 10))
    expect_equal(mean(projected$value), mean(source) + 0.5, tolerance = 1e-9)
    expect_match(projected$status, "^projected_bws")
})

test_that("BWS zero change is an exact identity including bound-only data", {
    source <- rep(c(0, 10), each = 12L)
    projected <- bws__project(
        source,
        target_mean = mean(source),
        lower = 0,
        upper = 10
    )

    expect_identical(projected$value, as.numeric(source))
    expect_identical(projected$scale, 0)
    expect_identical(projected$m, 1)
    expect_identical(projected$n, 1)

    collapsed <- bws__project(
        rep(0, 24L),
        target_mean = 0,
        lower = 0,
        upper = 0
    )
    expect_identical(collapsed$value, rep(0, 24L))
    expect_identical(collapsed$status, "projected_bws_collapsed_range")
})

test_that("BWS reports an infeasible bounded signal", {
    expect_error(
        bws__project(
            rep(c(0, 10), each = 12L),
            target_mean = 6,
            lower = 0,
            upper = 10
        ),
        class = "epwshiftr_bws_infeasible_error"
    )
    expect_error(
        bws__project(1:5, target_mean = 11, lower = 0, upper = 10),
        class = "epwshiftr_bws_infeasible_error"
    )
})

test_that("monthly BWS preserves ordering and returns equation parameters", {
    month <- rep(seq_len(12L), each = 24L)
    source <- rep(c(0, 10, seq(1, 9, length.out = 22L)), 12L)
    baseline <- vapply(seq_len(12L), function(calendar_month) {
        mean(source[month == calendar_month])
    }, numeric(1L))
    projected <- bws__project_monthly(
        source,
        month,
        target_mean = baseline + 0.25,
        upper = rep(10, 12L),
        variable_id = "clt"
    )

    expect_length(projected$value, length(source))
    expect_identical(projected$factors$month, seq_len(12L))
    expect_true(all(projected$factors$variable_id == "clt"))
    expect_equal(
        projected$factors$projected_mean,
        baseline + 0.25,
        tolerance = 1e-9
    )
    expect_true(all(projected$factors$m >= 0 & projected$factors$m <= 1))
    expect_true(all(projected$factors$n >= 0 & projected$factors$n <= 1))
})

test_that("integer BWS uses the closest attainable monthly mean", {
    value <- rep(c(0, 10, rep(5, 22L)), 12L)
    month <- rep(seq_len(12L), each = 24L)
    target <- rep(5.2, 12L)
    projected <- bws__project_monthly(
        value,
        month,
        target_mean = target,
        upper = rep(10, 12L),
        variable_id = "clt",
        integer = TRUE
    )

    expect_type(projected$value, "integer")
    expect_true(all(projected$value >= 0L & projected$value <= 10L))
    expect_equal(
        projected$factors$projected_mean,
        round(target * 24L) / 24L,
        tolerance = 0
    )
    expect_true(all(
        abs(projected$factors$closure_error) <= 0.5 / 24L
    ))
})
