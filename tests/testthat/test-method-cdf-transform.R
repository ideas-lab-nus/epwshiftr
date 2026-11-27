# Compact one-year block overrides shared by CDF-t unit fixtures.
cdft_test__defaults <- list(
    future_window_years = 1L,
    output_block_years = 1L,
    min_samples = 2L
)

test_that("CDF-t constructs the future target CDF from the published chain", {
    values <- c(1, 2, 4, 7, 11, 16)
    target <- cdft__target_cdf(
        values,
        values,
        values,
        target_grid_points = 1000L,
        tail_development_factor = 2
    )

    expect_equal(
        target$probability,
        cdft__empirical_cdf(values, target$grid)
    )
    expect_equal(target$shift, 0)
    expect_identical(
        target$diagnostics$lower_extended_points,
        0L
    )
    expect_identical(
        target$diagnostics$upper_extended_points,
        0L
    )
})

test_that("CDF-t transfers a location bias onto the future-model backbone", {
    observed <- signal_test__series("tas", 2001L, 1:20)
    historical <- signal_test__series("tas", 1991L, 6:25)
    future <- signal_test__series("tas", 2061L, 11:30)

    execution <- signal_test__execute(
        cdft__component(),
        "tas",
        observed,
        historical,
        future,
        default_overrides = cdft_test__defaults
    )
    adjusted <- execution@values[[1L]]
    window <- adjusted@provenance$diagnostics$windows[[1L]]

    expect_true(S7::S7_inherits(execution, SignalExecutionResult))
    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_equal(
        adjusted@data$value,
        future$value - 5,
        tolerance = 1
    )
    expect_identical(
        adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
        future[BIAS_DAILY_SERIES_COLUMNS[-2L]]
    )
    expect_identical(adjusted@output_role, "model_future")
    expect_identical(adjusted@transformation, "cdf_transform")
    expect_identical(
        adjusted@provenance$output_backbone,
        "model_future"
    )
    expect_identical(adjusted@provenance$range_alignment, "additive_mean")
    expect_identical(
        adjusted@provenance$temporal_policy$edge_policy,
        "truncate"
    )
    expect_identical(
        adjusted@provenance$temporal_policy$window_source,
        "user_override"
    )
    expect_true(is.na(
        adjusted@provenance$temporal_policy$window_reference
    ))
    expect_identical(
        adjusted@provenance$temporal_policy$edge_policy_source,
        "epwshiftr_implementation"
    )
    expect_identical(
        execution@profiles$tas$metadata$temporal_window_source,
        "famien_2018_application"
    )
    expect_identical(
        execution@profiles$tas$metadata$temporal_window_reference,
        CDFT_FAMIEN_REFERENCE
    )
    expect_equal(window$range_alignment_shift, -5)
    expect_identical(execution@diagnostics$status, "ok")
})

test_that("CDF-t preserves identity across native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    values <- c(1, 3, 2, 5, 4, 8, 7, 6, 10, 9, 12, 11)

    for (calendar in calendars) {
        observed <- signal_test__series(
            "tas",
            2001L,
            values,
            calendar
        )
        historical <- signal_test__series(
            "tas",
            1991L,
            values,
            calendar
        )
        future <- signal_test__series(
            "tas",
            2061L,
            values,
            calendar
        )
        adjusted <- signal_test__execute(
            cdft__component(),
            "tas",
            observed,
            historical,
            future,
            default_overrides = cdft_test__defaults
        )@values[[1L]]

        expect_equal(
            adjusted@data$value,
            future$value,
            tolerance = 0.02
        )
        expect_identical(
            adjusted@data$cf_calendar,
            future$cf_calendar
        )
    }
})

test_that("precipitation SSR is deterministic without changing global RNG", {
    values <- c(0, 0, 0, 2e-7, 3e-7, 4e-7)
    series <- list(
        observed_reference = signal_test__series(
            "pr",
            2001L,
            values
        ),
        model_historical = signal_test__series(
            "pr",
            1991L,
            values
        ),
        model_future = signal_test__series(
            "pr",
            2061L,
            values
        )
    )
    settings <- cdft__settings(list(
        pr = signal_test__profile_settings(cdft__profiles(), "pr")
    ))
    set.seed(2026)
    seed_before <- .Random.seed

    first <- cdft__prepared_values(
        series,
        settings,
        list(site = "A"),
        "pr"
    )
    second <- cdft__prepared_values(
        series,
        settings,
        list(site = "A"),
        "pr"
    )

    expect_identical(.Random.seed, seed_before)
    expect_identical(first, second)
    expect_identical(
        first$precipitation$input_randomized_values,
        c(
            observed_reference = 3L,
            model_historical = 3L,
            model_future = 3L
        )
    )
    expect_true(all(vapply(
        first$values,
        function(value) {
            all(value > 0) &&
                all(value[seq_len(3L)] < CDFT_PR_SSR_THRESHOLD)
        },
        logical(1L)
    )))
})

test_that("precipitation CDF-t restores a strict dry-day singularity", {
    observed <- signal_test__series(
        "pr",
        2001L,
        c(rep.int(0, 8L), seq.int(2L, 13L) * 1e-7)
    )
    historical <- signal_test__series(
        "pr",
        1991L,
        c(rep.int(0, 5L), seq.int(2L, 16L) * 1e-7)
    )
    future <- signal_test__series(
        "pr",
        2061L,
        c(rep.int(0, 10L), seq.int(2L, 11L) * 1e-7)
    )
    set.seed(2027)
    seed_before <- .Random.seed

    execution <- signal_test__execute(
        cdft__component(),
        "pr",
        observed,
        historical,
        future,
        overrides = list(min_samples = 5L),
        default_overrides = cdft_test__defaults
    )
    adjusted <- execution@values[[1L]]
    precipitation <- adjusted@provenance$diagnostics$precipitation

    expect_identical(.Random.seed, seed_before)
    expect_true(all(
        adjusted@data$value == 0 |
            adjusted@data$value >= CDFT_PR_SSR_THRESHOLD
    ))
    expect_identical(
        precipitation$output_positive_below_threshold_values,
        0L
    )
    expect_identical(
        precipitation$random_generator,
        "park_miller_16807"
    )
    expect_identical(
        precipitation$ssr_threshold,
        CDFT_PR_SSR_THRESHOLD
    )
})

test_that("CDF-t rejects incompatible window settings", {
    invalid <- signal_test__profile_settings(cdft__profiles(), "tas")
    invalid$future_window_years <- 4L
    invalid$output_block_years <- 1L
    expect_error(
        cdft__settings(list(tas = invalid)),
        "even, non-negative"
    )
})

# vim: fdm=marker :
