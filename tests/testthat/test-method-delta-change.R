test_that("Delta Change transfers additive changes onto observed daily data", {
    observed <- bias_adjustment_test__series(
        "tas",
        2001L,
        c(10, 14, 20, 24),
        calendar = "360_day"
    )
    historical <- bias_adjustment_test__series(
        "tas",
        1991L,
        c(8, 10, 17, 19),
        calendar = "noleap"
    )
    future <- bias_adjustment_test__series(
        "tas",
        2061L,
        c(18, 22, 30, 34),
        calendar = "all_leap"
    )
    execution <- signal_test__execute(
        bias__delta_change_component(),
        "tas",
        observed,
        historical,
        future
    )
    adjusted <- execution@values[[1L]]

    expect_true(S7::S7_inherits(execution, SignalExecutionResult))
    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_equal(adjusted@data$value, c(21, 25, 34, 38))
    expect_identical(
        adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
        observed[BIAS_DAILY_SERIES_COLUMNS[-2L]]
    )
    expect_equal(
        diff(adjusted@data$value[c(1L, 2L)]),
        diff(observed$value[c(1L, 2L)])
    )
    expect_equal(
        diff(adjusted@data$value[c(3L, 4L)]),
        diff(observed$value[c(3L, 4L)])
    )
    expect_identical(adjusted@output_role, "observed_reference")
    expect_identical(adjusted@transformation, "additive")
    expect_identical(adjusted@provenance$method, "delta_change")
    expect_identical(adjusted@provenance$grouping, "calendar_month")
    expect_identical(adjusted@provenance$statistic, "mean")
    expect_equal(
        adjusted@provenance$monthly_changes$change,
        c(11, 14)
    )
    expect_identical(adjusted@provenance$group_key, list(site = "A"))
    expect_identical(
        names(adjusted@settings),
        c("transformation", "bounds", "zero_tolerance")
    )
    expect_identical(execution@diagnostics$status, "ok")
    expect_identical(
        execution@profiles$tas$metadata$output_role,
        "observed_reference"
    )
    expect_identical(
        execution@profiles$tas$metadata$grouping,
        "calendar_month"
    )
    expect_identical(execution@profiles$tas$metadata$statistic, "mean")
})

test_that("Delta Change has zero-change identity for every CF calendar", {
    component <- bias__delta_change_component()
    settings <- signal_test__profile_settings(
        bias__delta_change_profiles(),
        "tas"
    )

    for (calendar in CF_TIME_CALENDARS) {
        observed <- bias_adjustment_test__series(
            "tas",
            2000L,
            c(10, 14, 20, 24),
            calendar = calendar
        )
        historical <- bias_adjustment_test__series(
            "tas",
            1990L,
            c(8, 10, 17, 19),
            calendar = calendar
        )
        future <- historical
        future$cf_year <- rep.int(2060L, nrow(future))
        coordinates <- cf_time__coordinates(
            data.frame(
                year = future$cf_year,
                month = future$cf_month,
                day = future$cf_day,
                hour = rep.int(12, nrow(future)),
                minute = rep.int(0, nrow(future)),
                second = rep.int(0, nrow(future))
            ),
            calendar
        )
        future[names(coordinates)] <- coordinates

        result <- component__execute(
            component,
            "apply_group",
            inputs = list(
                observed_reference = observed,
                model_historical = historical,
                model_future = future
            ),
            settings = list(tas = settings),
            key = list(calendar = calendar)
        )

        expect_identical(result@data$value, observed$value)
        expect_identical(result@data$cf_calendar, observed$cf_calendar)
        expect_equal(result@provenance$monthly_changes$change, c(0, 0))
    }
})

test_that("Delta Change transfers multiplicative precipitation changes", {
    observed <- bias_adjustment_test__series(
        "pr",
        2001L,
        c(0, 4, 2, 6)
    )
    historical <- bias_adjustment_test__series(
        "pr",
        1991L,
        c(1, 3, 2, 2)
    )
    future <- bias_adjustment_test__series(
        "pr",
        2061L,
        c(2, 6, 6, 2)
    )
    component <- bias__delta_change_component()
    settings <- signal_test__profile_settings(
        bias__delta_change_profiles(),
        "pr"
    )
    result <- component__execute(
        component,
        "apply_group",
        inputs = list(
            observed_reference = observed,
            model_historical = historical,
            model_future = future
        ),
        settings = list(pr = settings),
        key = list(site = "A")
    )

    expect_equal(result@data$value, c(0, 8, 4, 12))
    expect_identical(
        result@data$value == 0,
        observed$value == 0
    )
    expect_equal(
        result@provenance$monthly_changes$change,
        c(2, 2)
    )
    expect_identical(result@settings$bounds, c(0, Inf))
    expect_identical(result@provenance$clipped_values, 0L)
})

test_that("Delta Change rejects undefined ratios and records clipping", {
    observed <- bias_adjustment_test__series(
        "pr",
        2001L,
        c(1, 2, 3, 4)
    )
    historical <- bias_adjustment_test__series(
        "pr",
        1991L,
        c(0, 0, 1, 1)
    )
    future <- bias_adjustment_test__series(
        "pr",
        2061L,
        c(2, 2, 2, 2)
    )
    component <- bias__delta_change_component()
    settings <- signal_test__profile_settings(
        bias__delta_change_profiles(),
        "pr"
    )
    roles <- list(
        observed_reference = observed,
        model_historical = historical,
        model_future = future
    )

    expect_error(
        component__execute(
            component,
            "apply_group",
            inputs = roles,
            settings = list(pr = settings),
            key = list()
        ),
        "historical monthly mean is zero"
    )

    roles$model_historical$value <- rep.int(1, 4L)
    bounded <- component__execute(
        component,
        "apply_group",
        inputs = roles,
        settings = list(
            pr = utils::modifyList(
                settings,
                list(bounds = c(0, 5))
            )
        ),
        key = list()
    )
    expect_equal(bounded@data$value, c(2, 4, 5, 5))
    expect_identical(bounded@provenance$clipped_values, 2L)
})

# vim: fdm=marker :
