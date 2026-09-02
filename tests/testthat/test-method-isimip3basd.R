# Build native-calendar daily fixtures without routing 360-, 365-, or 366-day
# coordinates through a Gregorian Date conversion.
isimip_test__series <- function(
  variable_id,
  year,
  values,
  calendar = "noleap",
  units = "1"
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

# Construct the common input metadata and one aligned signal group for
# end-to-end component tests.
isimip_test__execution_inputs <- function(
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

# Retrieve one complete publication-backed variable profile.
isimip_test__settings <- function(variable) {
    profiles <- isimip__profiles()
    index <- which(vapply(
        profiles,
        function(profile) identical(profile@variable_id, variable),
        logical(1L)
    ))
    profiles[[index]]@settings
}

# Execute a compact 12-target-day test configuration while preserving the
# method equations, variable distribution, and declared stochastic policy.
isimip_test__execute <- function(
  variable,
  observed,
  historical,
  future,
  overrides = list(),
  key = list(site = "A")
) {
    boundary <- isimip_test__execution_inputs(
        observed,
        historical,
        future,
        key
    )
    settings <- utils::modifyList(
        list(
            running_window_days = 3L,
            target_year_days = 12L,
            n_quantiles = 8L,
            upper_bound_window_days = 3L,
            min_samples = 5L
        ),
        overrides
    )
    component__execute(
        isimip__component(),
        "apply",
        inputs = boundary$inputs,
        groups = list(boundary$group),
        overrides = stats::setNames(list(settings), variable),
        warn_experimental = FALSE
    )
}

test_that("published profiles cover direct and reconstructed components", {
    component <- isimip__component()
    profiles <- component@metadata$signal_profiles

    expect_identical(
        sort(names(profiles)),
        sort(ISIMIP_VARIABLES)
    )
    expect_true(all(vapply(
        profiles,
        function(profile) identical(profile$evidence, "published"),
        logical(1L)
    )))
    expect_true(all(vapply(
        profiles,
        function(profile) identical(
            profile$references,
            ISIMIP_REFERENCES
        ),
        logical(1L)
    )))
    expect_identical(
        profiles$pr$settings$lower_threshold,
        0.1 / 86400
    )
    expect_identical(
        profiles$hurs$settings$trend_preservation,
        "bounded"
    )
    expect_true(
        profiles$hurs$settings$unconditional_change_transfer
    )
    expect_true(
        profiles$hurs$settings$trendless_bound_frequency
    )
    expect_true(profiles$rsds$settings$scale_by_upper_bound_cycle)
    expect_identical(
        profiles$rsds$metadata$mapping_domain,
        "upper_bound_fraction"
    )
    expect_true(profiles$prsnratio$settings$impute_missing)
    expect_identical(
        profiles$prsnratio$metadata$reconstructed_outputs,
        "prsn"
    )
    expect_identical(
        profiles$tasrange$metadata$reconstructed_outputs,
        c("tasmin", "tasmax")
    )
    expect_identical(component@stage, "signal")
    expect_true(component@stochastic)
    expect_identical(
        component@metadata$spatial_downscaling,
        "separate_stage"
    )
})

test_that("change-transfer equations retain their declared semantics", {
    base <- isimip_test__settings("tas")
    observed <- c(2, 4, 6)
    historical <- c(3, 5, 7)
    future <- c(4, 8, 10)

    additive <- base
    additive$trend_preservation <- "additive"
    expect_equal(
        isimip__transfer_change(
            observed,
            historical,
            future,
            additive
        ),
        observed + future - historical
    )

    multiplicative <- base
    multiplicative$trend_preservation <- "multiplicative"
    expect_equal(
        isimip__transfer_change(
            observed,
            historical,
            future,
            multiplicative
        ),
        observed * future / historical
    )

    mixed <- isimip_test__settings("pr")
    expect_equal(
        isimip__transfer_change(
            c(2, 20),
            c(4, 4),
            c(8, 8),
            mixed
        ),
        c(4, 32)
    )

    bounded <- isimip_test__settings("tasskew")
    value <- isimip__transfer_change(
        c(0.7, 0.3, 0.5),
        c(0.5, 0.5, 0.5),
        c(0.6, 0.4, 0.5),
        bounded
    )
    expect_true(all(value >= 0 & value <= 1))
    expect_equal(value[[3L]], 0.5)
})

test_that("bound frequency transfer respects trendless and bounded cases", {
    expect_equal(
        isimip__transfer_frequency(0.2, 0.1, 0.3, TRUE),
        0.2
    )
    transferred <- isimip__transfer_frequency(
        0.2,
        0.1,
        0.3,
        FALSE
    )
    expect_true(transferred >= 0)
    expect_true(transferred <= 1)
    expect_gt(transferred, 0.2)
})

test_that("fixed-location Weibull fit and inverse CDF round trip", {
    values <- c(0.02, 0.05, 0.08, 0.14, 0.24, 0.4)
    settings <- isimip_test__settings("sfcWind")
    fit <- isimip__fit_weibull(
        values,
        settings$lower_threshold,
        settings$fit_tolerance,
        settings$fit_max_iterations
    )
    probability <- c(0.1, 0.5, 0.9)

    expect_identical(fit$family, "weibull")
    expect_gt(fit$parameters$shape, 0)
    expect_gt(fit$parameters$scale, 0)
    expect_equal(
        distribution__cdf(
            fit,
            distribution__quantile(fit, probability)
        ),
        probability,
        tolerance = 1e-8
    )
    expect_error(
        isimip__fit_weibull(
            rep(0.1, 5),
            0.01,
            1e-10,
            1000L
        ),
        "distinct"
    )
})

test_that("calendar utilities fill and smooth circular annual cycles", {
    filled <- isimip__circular_fill(c(1, NA, 3, 4, NA))
    expect_false(anyNA(filled))
    expect_equal(filled[[1L]], 1)
    expect_equal(filled[[3L]], 3)
    expect_equal(
        isimip__circular_running(1:5, 3L, "mean"),
        c(8 / 3, 2, 3, 4, 10 / 3)
    )
    expect_equal(
        isimip__circular_running(1:5, 3L, "max"),
        c(5, 3, 4, 5, 5)
    )
})

test_that("imputation and threshold randomization are deterministic", {
    first <- isimip__impute_missing(
        c(0.1, NA, 0.7, NA),
        17L,
        0
    )
    second <- isimip__impute_missing(
        c(0.1, NA, 0.7, NA),
        17L,
        0
    )
    fallback <- isimip__impute_missing(
        c(NA, NA),
        17L,
        0
    )
    randomized <- isimip__randomize_threshold(
        c(0, 0, 0.2),
        0,
        0.1,
        "lower",
        23L
    )

    expect_equal(first$value, second$value)
    expect_identical(first$missing, 2L)
    expect_true(all(first$value >= 0.1 & first$value <= 0.7))
    expect_equal(fallback$value, c(0, 0))
    expect_true(fallback$all_missing_fallback)
    expect_identical(randomized$count, 2L)
    expect_true(all(randomized$value[1:2] > 0))
    expect_true(all(randomized$value[1:2] < 0.1))
})

test_that("annual-mean detrending restores an inspectable trend term", {
    years <- rep(2001:2005, each = 10)
    values <- rep(seq(0, 20, length.out = 5), each = 10) +
        rep(seq(-1, 1, length.out = 10), 5)
    result <- isimip__detrend(values, years, 0.05)

    expect_true(result$applied)
    expect_gt(result$slope, 0)
    expect_lt(result$p_value, 0.05)
    expect_equal(
        result$value + result$trend,
        values,
        tolerance = 1e-12
    )
})

test_that("temperature identity retains a future-model native calendar", {
    n <- 360L
    values <- 280 +
        8 * sin(2 * pi * (seq_len(n) - 0.5) / n) +
        0.3 * sin(14 * pi * seq_len(n) / n)
    observed <- isimip_test__series(
        "tas",
        2001L,
        values,
        "360_day",
        "K"
    )
    historical <- isimip_test__series(
        "tas",
        1991L,
        values,
        "360_day",
        "K"
    )
    future <- isimip_test__series(
        "tas",
        2061L,
        values,
        "360_day",
        "K"
    )
    execution <- isimip_test__execute(
        "tas",
        observed,
        historical,
        future
    )
    adjusted <- execution@values[[1L]]

    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_identical(adjusted@output_role, "model_future")
    expect_identical(
        adjusted@transformation,
        "isimip3basd_bias_adjustment"
    )
    expect_identical(
        unique(adjusted@data$cf_calendar),
        "360_day"
    )
    expect_equal(adjusted@data$value, future$value, tolerance = 1e-7)
    expect_identical(
        adjusted@provenance$calendar_source,
        "epwshiftr_native_cf_phase_adapter"
    )
    expect_identical(
        adjusted@provenance$spatial_downscaling,
        "separate_stage"
    )
})

test_that("all profile families produce finite bounded daily output", {
    n <- 365L
    phase <- (seq_len(n) - 0.5) / n
    fixtures <- list(
        pr = list(
            value = ifelse(seq_len(n) %% 4L == 0L, 0, 2e-5) *
                (1 + 0.3 * sin(2 * pi * phase)),
            units = "kg m-2 s-1"
        ),
        hurs = list(
            value = 55 + 35 * sin(2 * pi * phase),
            units = "%"
        ),
        ps = list(
            value = 100000 + 500 * sin(2 * pi * phase),
            units = "Pa"
        ),
        psl = list(
            value = 101000 + 600 * sin(2 * pi * phase),
            units = "Pa"
        ),
        rlds = list(
            value = 300 + 40 * sin(2 * pi * phase),
            units = "W m-2"
        ),
        rsds = list(
            value = 300 * pmax(sin(2 * pi * phase), 0) + 1,
            units = "W m-2"
        ),
        sfcWind = list(
            value = 2 + abs(sin(8 * pi * phase)),
            units = "m s-1"
        ),
        tas = list(
            value = 280 + 10 * sin(2 * pi * phase),
            units = "K"
        ),
        prsnratio = list(
            value = pmin(pmax(0.5 + 0.5 * sin(2 * pi * phase), 0), 1),
            units = "1"
        ),
        tasrange = list(
            value = 8 + 2 * abs(sin(4 * pi * phase)),
            units = "K"
        ),
        tasskew = list(
            value = pmin(pmax(0.5 + 0.4 * sin(2 * pi * phase), 0), 1),
            units = "1"
        )
    )
    fixtures$prsnratio$value[c(7L, 107L, 207L)] <- NA_real_

    set.seed(912)
    rng_before <- .Random.seed
    for (variable in names(fixtures)) {
        fixture <- fixtures[[variable]]
        observed <- isimip_test__series(
            variable,
            2001L,
            fixture$value,
            "noleap",
            fixture$units
        )
        historical <- isimip_test__series(
            variable,
            1991L,
            fixture$value,
            "noleap",
            fixture$units
        )
        future_values <- fixture$value
        if (!identical(variable, "prsnratio")) {
            future_values <- future_values * 1.03
        }
        future <- isimip_test__series(
            variable,
            2061L,
            future_values,
            "noleap",
            fixture$units
        )
        execution <- isimip_test__execute(
            variable,
            observed,
            historical,
            future
        )
        adjusted <- execution@values[[1L]]
        bounds <- isimip_test__settings(variable)$bounds

        expect_false(anyNA(adjusted@data$value), info = variable)
        expect_true(
            all(is.finite(adjusted@data$value)),
            info = variable
        )
        expect_true(
            all(adjusted@data$value >= bounds[[1L]]),
            info = variable
        )
        if (!identical(variable, "rsds")) {
            expect_true(
                all(adjusted@data$value <= bounds[[2L]]),
                info = variable
            )
        }
    }
    expect_identical(.Random.seed, rng_before)
})

test_that("settings and input failures remain explicit", {
    invalid <- isimip_test__settings("tas")
    invalid$unexpected <- TRUE
    expect_error(
        isimip__settings(list(tas = invalid)),
        "Unexpected"
    )

    invalid <- isimip_test__settings("tas")
    invalid$running_window_days <- 30L
    expect_error(
        isimip__settings(list(tas = invalid)),
        "must be odd"
    )

    observed <- isimip_test__series(
        "tas",
        2001L,
        c(280, NA, 282),
        "noleap",
        "K"
    )
    historical <- isimip_test__series(
        "tas",
        1991L,
        280:282,
        "noleap",
        "K"
    )
    future <- isimip_test__series(
        "tas",
        2061L,
        280:282,
        "noleap",
        "K"
    )
    boundary <- isimip_test__execution_inputs(
        observed,
        historical,
        future
    )
    expect_error(
        component__execute(
            isimip__component(),
            "apply",
            inputs = boundary$inputs,
            groups = list(boundary$group),
            warn_experimental = FALSE
        ),
        "missing values"
    )
})

test_that("component remains compatible with calendar and sequence stages", {
    calendar <- component__spec(
        name = "isimip_calendar_test",
        stage = "calendar",
        input_kinds = "preprocessed_daily_series",
        output_kinds = "calendar_indexed_daily_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "isimip_sequence_test",
        stage = "sequence",
        input_kinds = "daily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    component <- isimip__component()

    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
