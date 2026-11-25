# Build one regular-lattice hourly series sampled at noon on every native CF
# day, keeping tests compact while exercising sub-daily calendar metadata.
# kqdm_test__series {{{
kqdm_test__series <- function(
    variable,
    year,
    values,
    calendar = "noleap",
    units = switch(
        variable,
        tas = "K",
        ps = "Pa",
        psl = "Pa",
        hurs = "%",
        sfcWind = "m s-1",
        rsds = "W m-2",
        rsdsdiff = "W m-2",
        rlds = "W m-2",
        "1"
    )
) {
    fields <- cf_time_offset2date(
        seq.int(0L, length(values) - 1L),
        data.frame(year = as.integer(year), month = 1L, day = 1L),
        calendar
    )
    fields$hour <- 12L
    fields$minute <- 0L
    fields$second <- 0
    data.frame(
        variable_id = rep.int(variable, length(values)),
        value = as.numeric(values),
        units = rep.int(units, length(values)),
        frequency = rep.int("hour", length(values)),
        cf_time__coordinates(fields, calendar),
        cf_second_of_day = rep.int(43200, length(values)),
        source_row = seq_along(values),
        stringsAsFactors = FALSE
    )
}
# }}}

# Return a smooth, non-constant annual signal with the exact number of days in
# the requested native calendar.
# kqdm_test__annual_values {{{
kqdm_test__annual_values <- function(year, calendar, offset = 0) {
    day_count <- cf_time__year_days(as.integer(year), calendar)
    phase <- (seq_len(day_count) - 0.5) / day_count
    10 + 2 * sin(2 * pi * phase) + 0.01 * seq_len(day_count) + offset
}
# }}}

# Execute the registered signal contract with a smaller deterministic KDE grid
# so unit tests retain the production algorithm without unnecessary runtime.
# kqdm_test__execute {{{
kqdm_test__execute <- function(
    variable,
    observed,
    historical,
    future,
    overrides = list(),
    warn_experimental = FALSE
) {
    signal_test__execute(
        kqdm__component(),
        variable = variable,
        observed = observed,
        historical = historical,
        future = future,
        overrides = overrides,
        default_overrides = list(grid_points = 512L, min_samples = 10L),
        warn_experimental = warn_experimental
    )
}
# }}}

test_that("hourly kernel QDM transfers additive quantile changes", {
    base <- kqdm_test__annual_values(1991L, "noleap")
    observed <- kqdm_test__series("tas", 2001L, base - 2)
    historical <- kqdm_test__series("tas", 1991L, base)
    future <- kqdm_test__series("tas", 2061L, base + 4)

    execution <- kqdm_test__execute(
        "tas",
        observed,
        historical,
        future
    )
    adjusted <- execution@values[[1L]]

    expect_s7_class(execution, SignalExecutionResult)
    expect_s7_class(adjusted, SubdailyAdjustedSeries)
    expect_equal(adjusted@data$value, future$value - 2, tolerance = 0.02)
    expect_identical(adjusted@data$source_row, future$source_row)
    expect_identical(adjusted@frequency, "hour")
    expect_identical(as.numeric(adjusted@time_step_seconds), 3600)
    expect_identical(adjusted@output_role, "model_future")
    expect_identical(
        adjusted@transformation,
        "kernel_quantile_delta_mapping"
    )
    expect_identical(execution@diagnostics$status, "ok")
})

test_that("hourly kernel QDM transfers multiplicative quantile changes", {
    base <- kqdm_test__annual_values(1991L, "noleap")
    observed <- kqdm_test__series("sfcWind", 2001L, base * 1.5)
    historical <- kqdm_test__series("sfcWind", 1991L, base)
    future <- kqdm_test__series("sfcWind", 2061L, base * 2)

    adjusted <- kqdm_test__execute(
        "sfcWind",
        observed,
        historical,
        future
    )@values[[1L]]

    expect_equal(adjusted@data$value, future$value * 1.5, tolerance = 0.04)
    expect_identical(adjusted@settings$transformation, "multiplicative")
    expect_true(all(adjusted@data$value >= 0))
})

test_that("hourly kernel QDM wraps month windows on native CF calendars", {
    expect_identical(kqdm__window_months(1L), c(12L, 1L, 2L))
    expect_identical(kqdm__window_months(12L), c(11L, 12L, 1L))

    for (calendar in c("360_day", "all_leap")) {
        year <- if (identical(calendar, "all_leap")) 2000L else 2001L
        values <- kqdm_test__annual_values(year, calendar)
        observed <- kqdm_test__series(
            "tas",
            year,
            values,
            calendar
        )
        historical <- kqdm_test__series(
            "tas",
            year - 10L,
            values,
            calendar
        )
        future <- kqdm_test__series(
            "tas",
            year + 60L,
            values,
            calendar
        )
        adjusted <- kqdm_test__execute(
            "tas",
            observed,
            historical,
            future
        )@values[[1L]]

        expect_equal(
            adjusted@data$value,
            values,
            tolerance = 0.02,
            info = calendar
        )
        january <- adjusted@provenance$diagnostics$months
        january <- january[january$center_month == 1L, ]
        expect_identical(january$window_months, "12,1,2", info = calendar)
        expect_identical(
            unique(adjusted@data$cf_calendar),
            calendar,
            info = calendar
        )
    }
})

test_that("hourly kernel QDM records package-selected numerical settings", {
    base <- kqdm_test__annual_values(1991L, "noleap")
    observed <- kqdm_test__series("tas", 2001L, base)
    historical <- kqdm_test__series("tas", 1991L, base)
    future <- kqdm_test__series("tas", 2061L, base + 1)

    execution <- NULL
    expect_warning(
        execution <- kqdm_test__execute(
            "tas",
            observed,
            historical,
            future,
            overrides = list(
                kernel = "epanechnikov",
                bandwidth_method = "nrd",
                bandwidth_adjust = 0.75
            ),
            warn_experimental = TRUE
        ),
        "experimental"
    )
    adjusted <- execution@values[[1L]]
    numerics <- adjusted@provenance$package_selected_numerics

    expect_identical(adjusted@settings$bandwidth_adjust, 0.75)
    expect_identical(adjusted@settings$grid_points, 512L)
    expect_identical(adjusted@settings$kernel, "epanechnikov")
    expect_identical(adjusted@settings$bandwidth_method, "nrd")
    expect_identical(numerics$kernel, "epanechnikov")
    expect_identical(numerics$bandwidth_method, "nrd")
    expect_identical(numerics$bandwidth_adjust, 0.75)
    expect_identical(numerics$grid_points, 512L)
    expect_identical(
        adjusted@provenance$published_method_settings,
        c(
            "kernel_density_cdf",
            "centered_three_month_window",
            "additive"
        )
    )
    expect_equal(nrow(adjusted@provenance$diagnostics$months), 12L)
})

test_that("hourly kernel QDM is deterministic and records clipping", {
    base <- kqdm_test__annual_values(1991L, "noleap") + 30
    observed <- kqdm_test__series("hurs", 2001L, base * 3)
    historical <- kqdm_test__series("hurs", 1991L, base)
    future <- kqdm_test__series("hurs", 2061L, base)

    first <- kqdm_test__execute(
        "hurs",
        observed,
        historical,
        future
    )@values[[1L]]
    second <- kqdm_test__execute(
        "hurs",
        observed,
        historical,
        future
    )@values[[1L]]

    expect_identical(first@data$value, second@data$value)
    expect_true(all(first@data$value <= 100))
    expect_true(first@provenance$diagnostics$clipped_values > 0L)
    expect_equal(
        sum(first@provenance$diagnostics$months$clipped_values),
        first@provenance$diagnostics$clipped_values
    )
    # Persisted mapping rows identify exact CF times and both sides of clipping.
    mapping <- first@provenance$mapping
    expect_identical(mapping$rows$row_index, seq_len(nrow(first@data)))
    expect_equal(mapping$rows$adjusted_value, first@data$value)
    expect_equal(mapping$rows$cf_year, first@data$cf_year)
    expect_equal(
        mapping$rows$bounds_correction,
        mapping$rows$adjusted_value - mapping$rows$unbounded_value
    )
    expect_equal(
        sum(mapping$rows$bounds_correction != 0),
        first@provenance$diagnostics$clipped_values
    )
    expect_equal(mapping, second@provenance$mapping)
    # Re-evaluate the numerical curves without calling the method's mapping
    # helpers. This verifies that the saved products suffice for recalculation.
    for (fit in mapping$distributions) {
        rows <- mapping$rows[mapping$rows$cf_month == fit$center_month]
        p <- stats::approx(
            fit$model_future$x,
            fit$model_future$probability,
            xout = rows$input_value,
            rule = 2,
            ties = "ordered"
        )$y
        observed <- stats::approx(
            fit$observed_reference$inverse_probability,
            fit$observed_reference$inverse_x,
            xout = p,
            rule = 2,
            ties = "ordered"
        )$y
        historical <- stats::approx(
            fit$model_historical$inverse_probability,
            fit$model_historical$inverse_x,
            xout = p,
            rule = 2,
            ties = "ordered"
        )$y
        expect_equal(p, rows$probability)
        expect_equal(observed, rows$observed_quantile)
        expect_equal(historical, rows$historical_quantile)
        expect_equal(
            observed * rows$input_value / historical,
            rows$unbounded_value
        )
        expect_equal(
            pmin(pmax(rows$unbounded_value, 0), 100),
            rows$adjusted_value
        )
    }
})

test_that("hourly kernel QDM rejects invalid numerical settings and retains subdaily role metadata", {
    base <- kqdm_test__annual_values(1991L, "noleap")
    observed <- kqdm_test__series("sfcWind", 2001L, base)
    historical <- kqdm_test__series("sfcWind", 1991L, base)
    future <- kqdm_test__series("sfcWind", 2061L, base)
    boundary <- signal_test__execution_inputs(observed, historical, future)
    settings <- signal_test__profile_settings(kqdm__profiles(), "sfcWind")
    settings$grid_points <- 512L
    settings$min_samples <- 10L

    invalid_grid <- settings
    invalid_grid$grid_points <- 500L
    expect_error(
        kqdm__apply_group(
            boundary$group@inputs,
            list(sfcWind = invalid_grid),
            list(site = "A")
        ),
        "power of two"
    )

    constant <- rep.int(5, length(base))
    expect_error(
        kqdm_test__execute(
            "sfcWind",
            kqdm_test__series("sfcWind", 2001L, constant),
            kqdm_test__series("sfcWind", 1991L, constant),
            kqdm_test__series("sfcWind", 2061L, constant)
        ),
        "at least two distinct values"
    )

    component <- kqdm__component()
    profiles <- component@metadata$signal_profiles
    expect_true(all(vapply(
        component@required_inputs,
        function(requirement) !length(requirement@frequencies),
        logical(1L)
    )))
    expect_identical(
        profiles$tas$metadata$variable_evidence,
        "method_literature"
    )
    expect_identical(
        profiles$psl$metadata$variable_evidence,
        "package_extension"
    )
    expect_identical(profiles$tas$metadata$published_settings, "transformation")
    expect_identical(
        profiles$tas$metadata$published_method$window_months,
        3L
    )
})

# vim: fdm=marker :
