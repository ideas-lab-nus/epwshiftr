# Build native-calendar daily rows for CDF-t fixtures without coercing dates
# through Gregorian base Date semantics.
cdft_test__series <- function(
  variable_id,
  year,
  values,
  calendar = "noleap",
  units = if (identical(variable_id, "pr")) {
      "kg m-2 s-1"
  } else {
      "K"
  }
) {
    origin <- data.frame(
        year = as.integer(year),
        month = 1L,
        day = 1L
    )
    fields <- cf_time_offset2date(
        seq.int(0L, length(values) - 1L),
        origin,
        calendar
    )
    fields$hour <- 12L
    fields$minute <- 0L
    fields$second <- 0
    data.frame(
        variable_id = rep.int(variable_id, length(values)),
        value = as.numeric(values),
        units = rep.int(units, length(values)),
        frequency = rep.int("day", length(values)),
        cf_time__coordinates(fields, calendar),
        stringsAsFactors = FALSE
    )
}

# Construct the package role metadata and aligned signal group used by CDF-t
# component-execution tests.
cdft_test__execution_inputs <- function(
  observed,
  historical,
  future,
  key = list(site = "A")
) {
    list(
        inputs = weather__new_inputs(
            observed_reference = weather__new_input(
                "observed_reference",
                observed
            ),
            model_historical = weather__new_input(
                "model_historical",
                historical
            ),
            model_future = weather__new_input(
                "model_future",
                future
            )
        ),
        group = signal__group(
            key = key,
            inputs = list(
                observed_reference = observed,
                model_historical = historical,
                model_future = future
            ),
            variables = unique(future$variable_id)
        )
    )
}

# Execute compact one-year fixtures through the common signal lifecycle while
# retaining the method defaults for empirical-CDF and SSR behavior.
cdft_test__execute <- function(
  variable,
  observed,
  historical,
  future,
  overrides = list(),
  key = list(site = "A")
) {
    boundary <- cdft_test__execution_inputs(
        observed,
        historical,
        future,
        key
    )
    settings <- utils::modifyList(
        list(
            future_window_years = 1L,
            output_block_years = 1L,
            min_samples = 2L
        ),
        overrides
    )
    component__execute(
        cdft__component(),
        "apply",
        inputs = boundary$inputs,
        groups = list(boundary$group),
        overrides = stats::setNames(list(settings), variable),
        warn_experimental = FALSE
    )
}

# Retrieve one complete default profile for direct settings validation.
cdft_test__settings <- function(variable) {
    profiles <- cdft__profiles()
    index <- which(vapply(
        profiles,
        function(profile) identical(profile@variable_id, variable),
        logical(1L)
    ))
    profiles[[index]]@settings
}

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

test_that("CDF-t uses published future windows and retained blocks", {
    blocks <- cdft__future_blocks(
        2001:2035,
        future_window_years = 17L,
        output_block_years = 9L
    )

    expect_identical(blocks[[1L]]$output_years, 2001:2009)
    expect_identical(blocks[[1L]]$window_years, 2001:2013)
    expect_true(blocks[[1L]]$truncated_left)
    expect_identical(blocks[[2L]]$output_years, 2010:2018)
    expect_identical(blocks[[2L]]$window_years, 2006:2022)
    expect_false(blocks[[2L]]$truncated_left)
    expect_false(blocks[[2L]]$truncated_right)
    expect_identical(blocks[[4L]]$output_years, 2028:2035)
    expect_identical(blocks[[4L]]$window_years, 2024:2035)
    expect_true(blocks[[4L]]$truncated_right)
    expect_error(
        cdft__future_blocks(2001:2003 * 2L, 3L, 1L),
        "contiguous"
    )
})

test_that("CDF-t transfers a location bias onto the future-model backbone", {
    observed <- cdft_test__series("tas", 2001L, 1:20)
    historical <- cdft_test__series("tas", 1991L, 6:25)
    future <- cdft_test__series("tas", 2061L, 11:30)

    execution <- cdft_test__execute(
        "tas",
        observed,
        historical,
        future
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
    expect_identical(
        adjusted@provenance$temporal_policy$source,
        "user_override"
    )
    expect_equal(window$range_alignment_shift, -5)
    expect_identical(execution@diagnostics$status, "ok")
})

test_that("CDF-t preserves identity across native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    values <- c(1, 3, 2, 5, 4, 8, 7, 6, 10, 9, 12, 11)

    for (calendar in calendars) {
        observed <- cdft_test__series(
            "tas",
            2001L,
            values,
            calendar
        )
        historical <- cdft_test__series(
            "tas",
            1991L,
            values,
            calendar
        )
        future <- cdft_test__series(
            "tas",
            2061L,
            values,
            calendar
        )
        adjusted <- cdft_test__execute(
            "tas",
            observed,
            historical,
            future
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
        observed_reference = cdft_test__series(
            "pr",
            2001L,
            values
        ),
        model_historical = cdft_test__series(
            "pr",
            1991L,
            values
        ),
        model_future = cdft_test__series(
            "pr",
            2061L,
            values
        )
    )
    settings <- cdft__settings(list(
        pr = cdft_test__settings("pr")
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
    observed <- cdft_test__series(
        "pr",
        2001L,
        c(rep.int(0, 8L), seq.int(2L, 13L) * 1e-7)
    )
    historical <- cdft_test__series(
        "pr",
        1991L,
        c(rep.int(0, 5L), seq.int(2L, 16L) * 1e-7)
    )
    future <- cdft_test__series(
        "pr",
        2061L,
        c(rep.int(0, 10L), seq.int(2L, 11L) * 1e-7)
    )
    set.seed(2027)
    seed_before <- .Random.seed

    execution <- cdft_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = list(min_samples = 5L)
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

test_that("CDF-t rejects incompatible settings and invalid inputs", {
    settings <- cdft_test__settings("tas")
    invalid <- settings
    invalid$future_window_years <- 4L
    invalid$output_block_years <- 1L
    expect_error(
        cdft__settings(list(tas = invalid)),
        "even, non-negative"
    )

    invalid <- settings
    invalid$tie_method <- "mean"
    expect_error(
        cdft__settings(list(tas = invalid)),
        "left-endpoint"
    )

    observed <- cdft_test__series("tas", 2001L, 1:5)
    historical <- cdft_test__series("tas", 1991L, 1:5)
    future <- cdft_test__series("tas", 2061L, 1:5)
    boundary <- cdft_test__execution_inputs(
        observed,
        historical,
        future
    )
    expect_error(
        component__execute(
            cdft__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(tas = settings),
            key = list()
        ),
        "fewer than 10"
    )

    precipitation <- lapply(
        boundary$group@inputs,
        function(data) {
            data$variable_id <- "pr"
            data$units <- "kg m-2 s-1"
            data$value <- abs(data$value) * 1e-7
            data
        }
    )
    precipitation$model_future$value[[1L]] <- -1
    expect_error(
        cdft__inputs(
            precipitation,
            "pr",
            "precipitation_ssr"
        ),
        "non-negative"
    )
})

test_that("CDF-t profiles retain published evidence and registration", {
    cdft__register_component()
    component <- component__get("signal", "cdf_transform_daily")
    profiles <- component@metadata$signal_profiles

    expect_true(S7::S7_inherits(component, WeatherComponentSpec))
    expect_identical(component@stage, "signal")
    expect_identical(
        component@input_kinds,
        "calendar_indexed_daily_series"
    )
    expect_identical(component@output_kinds, "daily_adjusted_series")
    expect_identical(component@scopes, "univariate")
    expect_true(component@stochastic)
    expect_identical(
        sort(names(profiles)),
        sort(CDFT_PUBLISHED_VARIABLES)
    )
    expect_true(all(vapply(
        profiles,
        function(profile) identical(profile$evidence, "published"),
        logical(1L)
    )))
    expect_identical(
        profiles$pr$settings$distribution_model,
        "precipitation_ssr"
    )
    expect_identical(
        profiles$tas$settings$distribution_model,
        "continuous"
    )

    calendar <- component__spec(
        name = "cdft_calendar_test",
        stage = "calendar",
        input_kinds = "preprocessed_daily_series",
        output_kinds = "calendar_indexed_daily_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "cdft_sequence_test",
        stage = "sequence",
        input_kinds = "daily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
