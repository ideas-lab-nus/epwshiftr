test_that("sub-daily lattice identifies complete regular daily positions", {
    hourly <- temporal__daily_lattice(seq.int(0, 23 * 3600, by = 3600))

    expect_true(hourly$regular)
    expect_identical(hourly$offsets, as.numeric(seq.int(
        0,
        23 * 3600,
        by = 3600
    )))
    expect_false(
        temporal__daily_lattice(seq.int(0, 22 * 3600, by = 3600))$regular
    )
    expect_false(
        temporal__daily_lattice(c(0, 3600, 7500, 10800))$regular
    )
})

test_that("sub-daily lattice supports explicit non-hourly expectations", {
    three_hourly <- temporal__daily_lattice(
        seq.int(0, 21 * 3600, by = 10800),
        expected_positions = 8L,
        step_seconds = 10800
    )

    expect_true(three_hourly$regular)
    expect_error(
        temporal__daily_lattice(0, step_seconds = 0),
        "must be positive"
    )
})
