# Build native-calendar daily rows for Equidistant CDF Matching fixtures
# without coercing 360-, 365-, or 366-day coordinates through base Date.
edcdf_test__series <- function(
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

# Construct the package role metadata and one aligned signal group used by
# component lifecycle tests.
edcdf_test__execution_inputs <- function(
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

# Execute compact fixtures with reduced sample thresholds while retaining all
# published distribution and package-adaptation settings.
edcdf_test__execute <- function(
  variable,
  observed,
  historical,
  future,
  overrides = list(),
  key = list(site = "A"),
  warn_experimental = FALSE
) {
    boundary <- edcdf_test__execution_inputs(
        observed,
        historical,
        future,
        key
    )
    settings <- utils::modifyList(
        list(
            min_samples = 2L,
            min_positive_samples = 2L
        ),
        overrides
    )
    component__execute(
        edcdf__component(),
        "apply",
        inputs = boundary$inputs,
        groups = list(boundary$group),
        overrides = stats::setNames(list(settings), variable),
        warn_experimental = warn_experimental
    )
}

# Retrieve one complete default profile for direct settings validation.
edcdf_test__settings <- function(variable) {
    profiles <- edcdf__profiles()
    index <- which(vapply(
        profiles,
        function(profile) identical(profile@variable_id, variable),
        logical(1L)
    ))
    profiles[[index]]@settings
}

test_that("four-parameter Beta fit uses the Li range convention", {
    values <- c(1, 2, 3, 4, 6, 9)
    fit <- edcdf__fit_beta4(
        values,
        range_extension_sd = 0.5,
        tolerance = 1e-10,
        max_iterations = 1000L
    )

    expect_identical(fit$family, "beta_four_parameter")
    expect_equal(
        fit$parameters$lower,
        min(values) - 0.5 * stats::sd(values)
    )
    expect_equal(
        fit$parameters$upper,
        max(values) + 0.5 * stats::sd(values)
    )
    expect_gt(fit$parameters$shape1, 0)
    expect_gt(fit$parameters$shape2, 0)
    probabilities <- c(0.1, 0.5, 0.9)
    expect_equal(
        edcdf__cdf(
            fit,
            edcdf__quantile(fit, probabilities)
        ),
        probabilities,
        tolerance = 1e-8
    )
    expect_error(
        edcdf__fit_beta4(
            rep(2, 5),
            0.5,
            1e-10,
            1000L
        ),
        "constant"
    )
})

test_that("mixed Gamma fit retains dry mass and positive amounts", {
    values <- c(0, 0, 1, 2, 4, 8)
    fit <- edcdf__fit_mixed_gamma(
        values,
        dry_threshold = 0,
        min_positive_samples = 2L,
        tolerance = 1e-10,
        max_iterations = 1000L
    )

    expect_identical(fit$family, "mixed_gamma")
    expect_equal(fit$parameters$dry_probability, 2 / 6)
    expect_equal(fit$parameters$wet_probability, 4 / 6)
    expect_identical(fit$positive_sample_size, 4L)
    expect_equal(edcdf__quantile(fit, 0.2), 0)
    expect_gt(edcdf__quantile(fit, 0.8), 0)
    positive_probabilities <- c(0.5, 0.7, 0.9)
    expect_equal(
        edcdf__cdf(
            fit,
            edcdf__quantile(fit, positive_probabilities)
        ),
        positive_probabilities,
        tolerance = 1e-8
    )
    expect_error(
        edcdf__fit_mixed_gamma(
            c(0, 0, 1),
            0,
            2L,
            1e-10,
            1000L
        ),
        "at least 2 positive"
    )
})

test_that("Equidistant CDF Matching applies the published additive equation", {
    observed <- c(1, 2, 4, 7, 11, 16)
    historical <- observed + 5
    future <- observed + 12
    resolved <- edcdf_test__settings("tas")
    resolved$min_samples <- 2L
    mapped <- edcdf__map_values(
        observed,
        historical,
        future,
        resolved
    )
    probability <- distribution__clamp_probability(
        edcdf__cdf(mapped$fits$model_future, future),
        resolved$cdf_epsilon
    )
    expected <- future +
        edcdf__quantile(
            mapped$fits$observed_reference,
            probability
        ) -
        edcdf__quantile(
            mapped$fits$model_historical,
            probability
        )

    expect_equal(mapped$value, expected)
    expect_equal(mapped$value, future - 5, tolerance = 1e-6)
    expect_equal(mapped$diagnostics$correction_range, c(-5, -5))
})

test_that("daily adaptation pools native calendar months separately", {
    base <- seq_len(59) + sin(seq_len(59))
    historical_offset <- c(rep(5, 31), rep(10, 28))
    future_offset <- c(rep(15, 31), rep(30, 28))
    observed <- edcdf_test__series("tas", 2001L, base)
    historical <- edcdf_test__series(
        "tas",
        1991L,
        base + historical_offset
    )
    future <- edcdf_test__series(
        "tas",
        2061L,
        base + future_offset
    )
    execution <- edcdf_test__execute(
        "tas",
        observed,
        historical,
        future
    )
    adjusted <- execution@values[[1L]]

    expect_equal(
        adjusted@data$value,
        future$value - historical_offset,
        tolerance = 1e-5
    )
    expect_identical(
        adjusted@provenance$diagnostics$month_count,
        2L
    )
    expect_identical(
        adjusted@provenance$frequency_source,
        "epwshiftr_daily_adaptation"
    )
    expect_identical(
        adjusted@provenance$equation_equivalence,
        "absolute_quantile_delta_mapping"
    )
})

test_that("temperature identity is preserved on native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    observed_values <- c(1, 3, 2, 5, 4, 8, 7, 6, 10, 9, 12, 11)
    future_values <- c(11, 13, 12, 15, 14, 18, 17, 16, 20, 19, 22, 21)

    for (calendar in calendars) {
        observed <- edcdf_test__series(
            "tas",
            2001L,
            observed_values,
            calendar
        )
        historical <- edcdf_test__series(
            "tas",
            1991L,
            observed_values,
            calendar
        )
        future <- edcdf_test__series(
            "tas",
            2061L,
            future_values,
            calendar
        )
        execution <- edcdf_test__execute(
            "tas",
            observed,
            historical,
            future
        )
        adjusted <- execution@values[[1L]]

        expect_equal(adjusted@data$value, future_values)
        expect_identical(
            adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
            future[BIAS_DAILY_SERIES_COLUMNS[-2L]]
        )
        expect_identical(
            unique(adjusted@data$cf_calendar),
            calendar
        )
    }
})

test_that("mixed Gamma precipitation preserves identity and dry values", {
    observed_values <- c(0, 0, 1, 2, 3, 5, 8, 13)
    future_values <- c(0, 0, 0, 2, 4, 6, 9, 15)
    observed <- edcdf_test__series(
        "pr",
        2001L,
        observed_values
    )
    historical <- edcdf_test__series(
        "pr",
        1991L,
        observed_values
    )
    future <- edcdf_test__series(
        "pr",
        2061L,
        future_values
    )
    execution <- edcdf_test__execute(
        "pr",
        observed,
        historical,
        future
    )
    adjusted <- execution@values[[1L]]
    diagnostics <- adjusted@provenance$diagnostics$precipitation

    expect_equal(adjusted@data$value, future_values)
    expect_identical(diagnostics$input_dry_values, c(
        observed_reference = 2L,
        model_historical = 2L,
        model_future = 3L
    ))
    expect_identical(diagnostics$output_dry_values, 3L)
    expect_identical(
        adjusted@settings$negative_precipitation_policy,
        "clip_zero"
    )
})

test_that("negative additive precipitation corrections are explicit", {
    observed <- edcdf_test__series(
        "pr",
        2001L,
        c(0, 0, 1, 2, 3, 5, 8, 13)
    )
    historical <- edcdf_test__series(
        "pr",
        1991L,
        c(0, 0, 10, 20, 30, 50, 80, 130)
    )
    future <- edcdf_test__series(
        "pr",
        2061L,
        c(0, 0, 1, 2, 3, 5, 8, 13)
    )
    execution <- edcdf_test__execute(
        "pr",
        observed,
        historical,
        future
    )
    adjusted <- execution@values[[1L]]
    diagnostics <- adjusted@provenance$diagnostics$precipitation

    expect_true(all(adjusted@data$value >= 0))
    expect_gt(diagnostics$negative_before_clipping, 0L)
    expect_gt(diagnostics$output_dry_values, 2L)
})

test_that("settings and incompatible inputs fail explicitly", {
    settings <- edcdf_test__settings("tas")
    invalid <- settings
    invalid$range_extension_sd <- 0
    expect_error(
        edcdf__settings(list(tas = invalid)),
        "must be positive"
    )

    invalid <- settings
    invalid$seasonal_grouping <- "annual_phase"
    expect_error(
        edcdf__settings(list(tas = invalid)),
        "currently requires"
    )

    invalid <- settings
    invalid$unexpected <- TRUE
    expect_error(
        edcdf__settings(list(tas = invalid)),
        "Unexpected"
    )

    negative <- edcdf_test__series(
        "pr",
        2001L,
        c(0, -1, 1, 2)
    )
    positive <- edcdf_test__series(
        "pr",
        1991L,
        c(0, 1, 2, 3)
    )
    boundary <- edcdf_test__execution_inputs(
        negative,
        positive,
        positive
    )
    expect_error(
        component__execute(
            edcdf__component(),
            "apply",
            inputs = boundary$inputs,
            groups = list(boundary$group),
            overrides = list(pr = list(
                min_samples = 2L,
                min_positive_samples = 2L
            )),
            warn_experimental = FALSE
        ),
        "non-negative"
    )
})

test_that("profiles expose monthly evidence and daily adaptation", {
    edcdf__register_component()
    component <- component__get(
        "signal",
        "equidistant_cdf_matching_daily"
    )
    profiles <- component@metadata$signal_profiles

    expect_true(S7::S7_inherits(component, WeatherComponentSpec))
    expect_identical(component@stage, "signal")
    expect_identical(
        component@input_kinds,
        "calendar_indexed_daily_series"
    )
    expect_identical(component@output_kinds, "daily_adjusted_series")
    expect_identical(component@scopes, "univariate")
    expect_false(component@stochastic)
    expect_identical(
        sort(names(profiles)),
        sort(EDCDF_LI_VARIABLES)
    )
    expect_true(all(vapply(
        profiles,
        function(profile) identical(
            profile$evidence,
            "experimental"
        ),
        logical(1L)
    )))
    expect_true(all(vapply(
        profiles,
        function(profile) identical(
            profile$metadata$method_variable_source,
            "li_2010_monthly"
        ),
        logical(1L)
    )))
    expect_identical(
        profiles$tas$settings$distribution_model,
        "beta_four_parameter"
    )
    expect_identical(
        profiles$pr$settings$distribution_model,
        "mixed_gamma"
    )
    expect_identical(component@metadata$published_frequency, "mon")
    expect_identical(component@metadata$adapted_frequency, "day")
})

test_that("experimental daily profile warning and contracts are retained", {
    values <- c(1, 3, 2, 5, 4, 8, 7, 6, 10, 9, 12, 11)
    observed <- edcdf_test__series("tas", 2001L, values)
    historical <- edcdf_test__series("tas", 1991L, values)
    future <- edcdf_test__series("tas", 2061L, values + 5)

    expect_warning(
        edcdf_test__execute(
            "tas",
            observed,
            historical,
            future,
            warn_experimental = TRUE
        ),
        "experimental"
    )

    calendar <- component__spec(
        name = "edcdf_calendar_test",
        stage = "calendar",
        input_kinds = "preprocessed_daily_series",
        output_kinds = "calendar_indexed_daily_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "edcdf_sequence_test",
        stage = "sequence",
        input_kinds = "daily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    component <- edcdf__component()
    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
