# Build one calendar-native daily series from consecutive CF-calendar days so
# Quantile Mapping tests never rely on Gregorian Date coercion.
qm_test__series <- function(
    variable_id,
    year,
    values,
    calendar = "noleap",
    units = if (identical(variable_id, "pr")) {
        "kg m-2 s-1"
    } else if (identical(variable_id, "hurs")) {
        "%"
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

# Construct role metadata and the aligned group from the same three canonical
# tables, matching the package's standalone signal execution boundary.
qm_test__execution_inputs <- function(
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

# Execute one variable through the common signal lifecycle while replacing the
# default seasonal window with a full-cycle window for compact test fixtures.
qm_test__execute <- function(
    variable,
    observed,
    historical,
    future,
    overrides = list(),
    key = list(site = "A"),
    warn_experimental = FALSE
) {
    boundary <- qm_test__execution_inputs(
        observed,
        historical,
        future,
        key
    )
    settings <- utils::modifyList(
        list(
            seasonal_window_days = 365L,
            target_year_days = 365L,
            min_samples = 2L
        ),
        overrides
    )
    component__execute(
        qm__component(),
        "apply",
        inputs = boundary$inputs,
        groups = list(boundary$group),
        overrides = stats::setNames(list(settings), variable),
        warn_experimental = warn_experimental
    )
}

# Retrieve one default settings record without relying on profile construction
# order in tests that call the single-group kernel directly.
qm_test__settings <- function(variable) {
    profiles <- qm__profiles()
    index <- which(vapply(
        profiles,
        function(profile) identical(profile@variable_id, variable),
        logical(1L)
    ))
    profiles[[index]]@settings
}

test_that("empirical CDF conventions make ties and tails explicit", {
    anchors <- qm__cdf_anchors(c(1, 1, 2, 4))

    expect_equal(anchors$value, c(1, 2, 4))
    expect_equal(anchors$probability, c(1 / 6, 2 / 3, 1))

    mapped <- qm__map_continuous(
        historical = c(1, 1, 2, 4),
        observed = c(10, 20, 30, 40),
        future = c(-1, 1, 2, 4, 6)
    )
    expect_equal(mapped$value, c(10, 15, 30, 40, 40))
    expect_identical(
        mapped$lower_tail,
        c(TRUE, FALSE, FALSE, FALSE, FALSE)
    )
    expect_identical(
        mapped$upper_tail,
        c(FALSE, FALSE, FALSE, FALSE, TRUE)
    )
    expect_identical(mapped$tied_historical_values, 1L)
    expect_identical(mapped$tied_observed_values, 0L)
})

test_that("Quantile Mapping returns a typed future-backbone daily series", {
    observed <- qm_test__series(
        "tas",
        2001L,
        c(10, 20, 30, 40)
    )
    historical <- qm_test__series(
        "tas",
        1991L,
        c(0, 10, 20, 30)
    )
    future <- qm_test__series(
        "tas",
        2061L,
        c(5, 15, -10, 40)
    )

    execution <- qm_test__execute(
        "tas",
        observed,
        historical,
        future
    )
    adjusted <- execution@values[[1L]]

    expect_true(S7::S7_inherits(execution, SignalExecutionResult))
    expect_true(S7::S7_inherits(adjusted, DailyAdjustedSeries))
    expect_equal(adjusted@data$value, c(15, 25, 10, 40))
    expect_identical(
        adjusted@data[BIAS_DAILY_SERIES_COLUMNS[-2L]],
        future[BIAS_DAILY_SERIES_COLUMNS[-2L]]
    )
    expect_identical(adjusted@output_role, "model_future")
    expect_identical(adjusted@transformation, "quantile_mapping")
    expect_identical(adjusted@provenance$output_backbone, "model_future")
    expect_identical(
        adjusted@provenance$diagnostics$lower_tail_values,
        1L
    )
    expect_identical(
        adjusted@provenance$diagnostics$upper_tail_values,
        1L
    )
    expect_identical(execution@diagnostics$status, "ok")
})

test_that("Quantile Mapping preserves identity across native CF calendars", {
    calendars <- c("360_day", "noleap", "all_leap")
    values <- c(2, 2, 4, 7, 9)

    for (calendar in calendars) {
        observed <- qm_test__series(
            "tas",
            2000L,
            values,
            calendar
        )
        historical <- qm_test__series(
            "tas",
            1990L,
            values,
            calendar
        )
        future <- qm_test__series(
            "tas",
            2060L,
            values,
            calendar
        )
        execution <- qm_test__execute(
            "tas",
            observed,
            historical,
            future
        )

        expect_equal(
            execution@values[[1L]]@data$value,
            values,
            info = calendar
        )
        expect_identical(
            execution@values[[1L]]@data$cf_calendar,
            rep.int(calendar, length(values)),
            info = calendar
        )
    }
})

test_that("circular windows bridge the annual-phase boundary", {
    observed <- qm_test__series(
        "tas",
        2001L,
        seq_len(365L) + 10
    )
    historical <- qm_test__series(
        "tas",
        1991L,
        seq_len(365L)
    )
    future <- qm_test__series(
        "tas",
        2061L,
        seq_len(365L)
    )
    boundary <- qm_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- qm_test__settings("tas")
    settings$seasonal_window_days <- 3L
    settings$min_samples <- 3L

    adjusted <- component__execute(
        qm__component(),
        "apply_group",
        inputs = boundary$group@inputs,
        settings = list(tas = settings),
        key = boundary$group@key
    )

    expect_equal(adjusted@data$value[c(1L, 365L)], c(11, 375))
    expect_equal(
        adjusted@provenance$diagnostics$observed_window_samples,
        c(minimum = 3, median = 3, maximum = 3)
    )
})

test_that("bounds clip mapped values and are retained in diagnostics", {
    observed <- qm_test__series("hurs", 2001L, c(-10, 25, 75, 120))
    historical <- qm_test__series("hurs", 1991L, c(0, 1, 2, 3))
    future <- qm_test__series("hurs", 2061L, c(0, 1, 2, 3))

    execution <- qm_test__execute(
        "hurs",
        observed,
        historical,
        future
    )
    adjusted <- execution@values[[1L]]

    expect_equal(adjusted@data$value, c(0, 25, 75, 100))
    expect_identical(
        adjusted@provenance$diagnostics$clipped_values,
        2L
    )
    expect_identical(adjusted@settings$bounds, c(0, 100))
})

test_that("precipitation hurdle mapping has deterministic dry-day control", {
    count <- 40L
    observed <- qm_test__series(
        "pr",
        2001L,
        c(rep.int(0, 8L), seq_len(32L))
    )
    historical <- qm_test__series(
        "pr",
        1991L,
        c(rep.int(0, 20L), seq_len(20L))
    )
    future <- qm_test__series(
        "pr",
        2061L,
        rep.int(0, count)
    )
    set.seed(2026)
    seed_before <- .Random.seed

    first <- qm_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = list(dry_threshold = 0, random_seed = 99L)
    )
    second <- qm_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = list(dry_threshold = 0, random_seed = 99L)
    )
    different <- qm_test__execute(
        "pr",
        observed,
        historical,
        future,
        overrides = list(dry_threshold = 0, random_seed = 100L)
    )

    first_values <- first@values[[1L]]@data$value
    expect_identical(.Random.seed, seed_before)
    expect_identical(first_values, second@values[[1L]]@data$value)
    expect_false(identical(
        first_values,
        different@values[[1L]]@data$value
    ))
    expect_true(any(first_values == 0))
    expect_true(any(first_values > 0))
    precipitation <- (
        first@values[[1L]]@provenance$diagnostics$precipitation
    )
    expect_identical(precipitation$input_dry_values, count)
    expect_identical(precipitation$randomized_dry_values, count)
    expect_true(precipitation$output_dry_values < count)
    expect_identical(precipitation$random_seed, 99L)
    expect_identical(
        precipitation$random_generator,
        "park_miller_16807"
    )
})

test_that("Quantile Mapping rejects unsupported or insufficient inputs", {
    observed <- qm_test__series("tas", 2001L, c(1, 2, 3, 4))
    historical <- qm_test__series("tas", 1991L, c(1, 2, 3, 4))
    future <- qm_test__series("tas", 2061L, c(1, 2, 3, 4))
    boundary <- qm_test__execution_inputs(
        observed,
        historical,
        future
    )
    settings <- qm_test__settings("tas")

    unsupported <- settings
    unsupported$cdf_method <- "step_function"
    expect_error(
        component__execute(
            qm__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(tas = unsupported),
            key = list()
        ),
        "requires linear empirical CDF"
    )

    expect_error(
        component__execute(
            qm__component(),
            "apply_group",
            inputs = boundary$group@inputs,
            settings = list(tas = settings),
            key = list()
        ),
        "fewer than 10"
    )

    incompatible_units <- boundary$group@inputs
    incompatible_units$model_future$units <- "degC"
    expect_error(
        component__execute(
            qm__component(),
            "apply_group",
            inputs = incompatible_units,
            settings = list(tas = utils::modifyList(
                settings,
                list(
                    seasonal_window_days = 365L,
                    min_samples = 2L
                )
            )),
            key = list()
        ),
        "identical units"
    )

    precipitation <- lapply(
        boundary$group@inputs,
        function(data) {
            data$variable_id <- "pr"
            data$units <- "kg m-2 s-1"
            data
        }
    )
    precipitation$model_future$value[[1L]] <- -1
    pr_settings <- qm_test__settings("pr")
    expect_error(
        component__execute(
            qm__component(),
            "apply_group",
            inputs = precipitation,
            settings = list(pr = utils::modifyList(
                pr_settings,
                list(
                    seasonal_window_days = 365L,
                    min_samples = 2L
                )
            )),
            key = list()
        ),
        "non-negative"
    )
})

test_that("Quantile Mapping profiles retain evidence and registration", {
    qm__register_component()
    component <- component__get("signal", "quantile_mapping_daily")
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
        sort(c(QM_PUBLISHED_VARIABLES, QM_EXPERIMENTAL_VARIABLES))
    )
    expect_identical(profiles$tas$evidence, "published")
    expect_identical(profiles$pr$evidence, "published")
    expect_identical(profiles$hurs$evidence, "experimental")
    expect_identical(
        profiles$hurs$metadata$default_source,
        "package_implementation"
    )

    calendar <- component__spec(
        name = "qm_calendar_test",
        stage = "calendar",
        input_kinds = "preprocessed_daily_series",
        output_kinds = "calendar_indexed_daily_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "qm_sequence_test",
        stage = "sequence",
        input_kinds = "daily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
