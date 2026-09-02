# Build one regular-lattice hourly series sampled at noon on every native CF
# day, keeping tests compact while exercising sub-daily calendar metadata.
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

# Return a smooth, non-constant annual signal with the exact number of days in
# the requested native calendar.
kqdm_test__annual_values <- function(year, calendar, offset = 0) {
    day_count <- cf_time__year_days(as.integer(year), calendar)
    phase <- (seq_len(day_count) - 0.5) / day_count
    10 + 2 * sin(2 * pi * phase) + 0.01 * seq_len(day_count) + offset
}

# Construct role descriptors and one aligned signal group for common component
# execution tests.
kqdm_test__boundary <- function(observed, historical, future) {
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
            key = list(site = "A"),
            inputs = list(
                observed_reference = observed,
                model_historical = historical,
                model_future = future
            ),
            variables = unique(future$variable_id)
        )
    )
}

# Execute the registered signal contract with a smaller deterministic KDE grid
# so unit tests retain the production algorithm without unnecessary runtime.
kqdm_test__execute <- function(
    variable,
    observed,
    historical,
    future,
    overrides = list(),
    warn_experimental = FALSE
) {
    boundary <- kqdm_test__boundary(observed, historical, future)
    settings <- utils::modifyList(
        list(grid_points = 512L, min_samples = 10L),
        overrides
    )
    component__execute(
        kqdm__component(),
        "apply",
        inputs = boundary$inputs,
        groups = list(boundary$group),
        overrides = stats::setNames(list(settings), variable),
        warn_experimental = warn_experimental
    )
}

# Retrieve one default profile by variable for direct kernel validation.
kqdm_test__settings <- function(variable) {
    profiles <- kqdm__profiles()
    index <- which(vapply(
        profiles,
        function(profile) identical(profile@variable_id, variable),
        logical(1L)
    ))
    profiles[[index]]@settings
}

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
})

test_that("hourly kernel QDM rejects invalid inputs and numerical settings", {
    base <- kqdm_test__annual_values(1991L, "noleap")
    observed <- kqdm_test__series("sfcWind", 2001L, base)
    historical <- kqdm_test__series("sfcWind", 1991L, base)
    future <- kqdm_test__series("sfcWind", 2061L, base)
    boundary <- kqdm_test__boundary(observed, historical, future)
    settings <- kqdm_test__settings("sfcWind")
    settings$grid_points <- 512L
    settings$min_samples <- 10L

    negative <- boundary$group@inputs
    negative$model_future$value[[1L]] <- -1
    expect_error(
        kqdm__apply_group(
            negative,
            list(sfcWind = settings),
            list(site = "A")
        ),
        "non-negative"
    )

    wrong_frequency <- boundary$group@inputs
    wrong_frequency$model_future$frequency <- "3hr"
    expect_error(
        kqdm__apply_group(
            wrong_frequency,
            list(sfcWind = settings),
            list(site = "A")
        ),
        "declared `frequency`"
    )

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
})

test_that("hourly kernel QDM is registered with frequency-aware contracts", {
    kqdm__register_component()
    component <- component__get(
        "signal",
        "kernel_quantile_delta_mapping_hourly"
    )
    profiles <- component@metadata$signal_profiles

    expect_s7_class(component, WeatherComponentSpec)
    expect_identical(component@stage, "signal")
    expect_identical(
        component@input_kinds,
        "calendar_indexed_hourly_series"
    )
    expect_true(all(vapply(
        component@required_inputs,
        function(requirement) !length(requirement@frequencies),
        logical(1L)
    )))
    expect_identical(component@output_kinds, "subdaily_adjusted_series")
    expect_identical(component@scopes, "univariate")
    expect_false(component@stochastic)
    expect_identical(
        sort(names(profiles)),
        sort(c(
            KQDM_PUBLISHED_VARIABLES,
            KQDM_EXPERIMENTAL_VARIABLES
        ))
    )
    expect_true(all(vapply(
        profiles,
        function(profile) identical(profile$evidence, "experimental"),
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

    calendar <- component__spec(
        name = "kqdm_calendar_test",
        stage = "calendar",
        input_kinds = "hourly_role_inputs",
        output_kinds = "calendar_indexed_hourly_series",
        operations = list(apply = identity)
    )
    sequence <- component__spec(
        name = "kqdm_sequence_test",
        stage = "sequence",
        input_kinds = "subdaily_adjusted_series",
        output_kinds = "weather_sequence",
        operations = list(generate = identity)
    )
    expect_true(component__compatible(calendar, component))
    expect_true(component__compatible(component, sequence))
})
