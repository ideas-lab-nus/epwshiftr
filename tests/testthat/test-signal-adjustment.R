# Build a regular calendar-native sub-daily table while exposing the exact
# time-of-day coordinate and one untouched source timestamp column.
# adjusted_series_test__subdaily {{{
adjusted_series_test__subdaily <- function(
    variable = "tas",
    year = 2061L,
    days = 2L,
    calendar = "noleap",
    frequency = "3hr",
    time_step_seconds = 10800,
    time_offset_seconds = 0
) {
    offsets <- seq.int(
        from = time_offset_seconds,
        to = days * 86400 - time_step_seconds + time_offset_seconds,
        by = time_step_seconds
    )
    day_offsets <- offsets %/% 86400
    seconds <- offsets %% 86400
    fields <- cf_time_offset2date(
        day_offsets,
        data.frame(year = year, month = 1L, day = 1L),
        calendar
    )
    fields$hour <- seconds %/% 3600
    fields$minute <- (seconds %% 3600) %/% 60
    fields$second <- seconds %% 60
    coordinates <- cf_time__coordinates(fields, calendar)
    data.frame(
        variable_id = rep.int(variable, length(offsets)),
        value = seq_along(offsets),
        units = rep.int("K", length(offsets)),
        frequency = rep.int(frequency, length(offsets)),
        coordinates,
        cf_second_of_day = as.numeric(seconds),
        source_time = sprintf(
            "%s:%05d",
            calendar,
            as.integer(offsets)
        ),
        stringsAsFactors = FALSE
    )
}
# }}}

test_that("shared signal requirements preserve the three-role contract", {
    requirements <- signal__three_role_requirements(
        as.list(c("tas", "pr")),
        frequencies = "day"
    )

    expect_named(requirements, SIGNAL_THREE_INPUT_ROLES)
    for (role in SIGNAL_THREE_INPUT_ROLES) {
        requirement <- requirements[[role]]
        expect_s7_class(requirement, WeatherInputRequirement)
        expect_identical(requirement@role, role)
        expect_identical(requirement@representations, "series")
        expect_identical(requirement@frequencies, "day")
        expect_identical(requirement@variable_sets, list("tas", "pr"))
    }
})

test_that("shared adjusted result validation retains diagnostic contracts", {
    daily <- bias__daily_adjusted_series(
        signal_test__series("tas", 2061L, c(280, 282, 284, 286)),
        output_role = "model_future",
        transformation = "test_adjustment"
    )
    validate_daily <- function(value, output_role = "model_future") {
        signal__validate_adjusted_result(
            value,
            DailyAdjustedSeries,
            "DailyAdjustedSeries",
            output_role,
            "Test method"
        )
    }

    expect_true(validate_daily(daily))
    expect_identical(
        validate_daily(1),
        "Test method must return a DailyAdjustedSeries object."
    )
    expect_identical(
        validate_daily(daily, "observed_reference"),
        "Test method output must retain the `observed_reference` role."
    )

    subdaily <- bias__subdaily_adjusted_series(
        adjusted_series_test__subdaily(),
        frequency = "3hr",
        time_step_seconds = 10800,
        output_role = "model_future",
        transformation = "test_adjustment"
    )
    expect_identical(
        signal__validate_adjusted_result(
            subdaily,
            SubdailyAdjustedSeries,
            "SubdailyAdjustedSeries",
            "model_future",
            "Test method",
            frequency = "hour",
            time_step_seconds = 3600,
            temporal_message = "Test method must return hourly values."
        ),
        "Test method must return hourly values."
    )
})

test_that("shared future blocks preserve fitting-window edge diagnostics", {
    blocks <- signal__future_blocks(
        2041:2050,
        future_window_years = 7L,
        output_block_years = 3L,
        method_label = "Test method"
    )

    expect_length(blocks, 4L)
    expect_identical(blocks[[1L]]$output_years, 2041:2043)
    expect_identical(blocks[[1L]]$window_years, 2041:2045)
    expect_true(blocks[[1L]]$truncated_left)
    expect_false(blocks[[1L]]$truncated_right)
    expect_identical(blocks[[4L]]$output_years, 2050L)
    expect_true(blocks[[4L]]$truncated_right)

    # Verify the published CDF-t (17/9) and SDM (30/10) block schedules.
    cdft_blocks <- signal__future_blocks(2041:2067, 17L, 9L, "CDF-t")
    expect_length(cdft_blocks, 3L)
    expect_identical(cdft_blocks[[2L]]$output_years, 2050:2058)
    expect_identical(cdft_blocks[[2L]]$window_years, 2046:2062)
    expect_false(cdft_blocks[[2L]]$truncated_left)
    expect_false(cdft_blocks[[2L]]$truncated_right)

    sdm_blocks <- signal__future_blocks(2041:2070, 30L, 10L, "SDM")
    expect_length(sdm_blocks, 3L)
    expect_identical(sdm_blocks[[2L]]$output_years, 2051:2060)
    expect_identical(sdm_blocks[[2L]]$window_years, 2041:2070)
    expect_false(sdm_blocks[[2L]]$truncated_left)
    expect_false(sdm_blocks[[2L]]$truncated_right)

    expect_error(
        signal__future_blocks(2041:2043 * 2L, 3L, 1L, "Test method"),
        "Test method requires contiguous future model years"
    )
})

test_that("shared signal bounds return values and exact clipping counts", {
    bounded <- signal__bound_values(
        c(-1, 0, 0.5, 1, 2),
        c(0, 1)
    )

    expect_identical(bounded$value, c(0, 0, 0.5, 1, 1))
    expect_identical(bounded$clipped, 2L)
    expect_identical(
        signal__bound_values(c(-1, 2), c(-Inf, Inf))$value,
        c(-1, 2)
    )
    expect_identical(
        signal__bound_values(c(-Inf, Inf), c(0, 1)),
        list(value = c(0, 1), clipped = 2L)
    )
})

test_that("daily adjusted series remains a strict specialization", {
    data <- signal_test__series("tas", 2061L, c(280, 282, 284, 286))
    adjusted <- bias__daily_adjusted_series(
        data,
        output_role = "model_future",
        transformation = "test_adjustment"
    )

    expect_s7_class(adjusted, DailyAdjustedSeries)
    expect_true(S7::S7_inherits(adjusted, AdjustedWeatherSeries))
    expect_identical(adjusted@frequency, "day")
    expect_identical(as.numeric(adjusted@time_step_seconds), 86400)
    expect_identical(adjusted@data, data)
})

test_that("sub-daily adjusted series retains its regular CF time lattice", {
    for (calendar in CF_TIME_CALENDARS) {
        data <- adjusted_series_test__subdaily(calendar = calendar)
        adjusted <- bias__subdaily_adjusted_series(
            data,
            frequency = "3hr",
            time_step_seconds = 10800,
            output_role = "model_future",
            transformation = "test_adjustment",
            settings = list(window_months = 3L),
            provenance = list(method = "test_adjustment")
        )

        expect_s7_class(adjusted, SubdailyAdjustedSeries)
        expect_true(
            S7::S7_inherits(adjusted, AdjustedWeatherSeries),
            info = calendar
        )
        expect_identical(adjusted@frequency, "3hr", info = calendar)
        expect_identical(
            as.numeric(adjusted@time_step_seconds),
            10800,
            info = calendar
        )
        expect_identical(
            adjusted@data$source_time,
            data$source_time,
            info = calendar
        )
        expect_identical(
            adjusted@variable_metadata$tas$frequency,
            "3hr",
            info = calendar
        )
    }

    offset_data <- adjusted_series_test__subdaily(
        time_offset_seconds = 5400
    )
    expect_no_error(
        bias__subdaily_adjusted_series(
            offset_data,
            "3hr",
            10800,
            "model_future",
            "test_adjustment"
        )
    )
})

test_that("sub-daily adjusted series rejects ambiguous temporal metadata", {
    data <- adjusted_series_test__subdaily()

    duplicate <- rbind(data, data[1L, ])
    expect_error(
        bias__subdaily_adjusted_series(
            duplicate,
            "3hr",
            10800,
            "model_future",
            "test_adjustment"
        ),
        "unique variable-calendar-year-month-day-second"
    )

    inconsistent_phase <- data
    inconsistent_phase$annual_phase[[2L]] <-
        inconsistent_phase$annual_phase[[2L]] + 0.00001
    expect_error(
        bias__subdaily_adjusted_series(
            inconsistent_phase,
            "3hr",
            10800,
            "model_future",
            "test_adjustment"
        ),
        "annual_phase.*cf_second_of_day"
    )

    irregular <- data
    irregular$cf_second_of_day[[2L]] <- 7200
    irregular$annual_phase[[2L]] <-
        irregular$cf_second_of_day[[2L]] /
        86400 /
        irregular$cf_year_days[[2L]]
    expect_error(
        bias__subdaily_adjusted_series(
            irregular,
            "3hr",
            10800,
            "model_future",
            "test_adjustment"
        ),
        "regular timestep lattice"
    )

    expect_error(
        bias__subdaily_adjusted_series(
            data,
            "3hr",
            0,
            "model_future",
            "test_adjustment"
        ),
        "positive number"
    )
    expect_error(
        bias__subdaily_adjusted_series(
            data,
            "3hr",
            10000,
            "model_future",
            "test_adjustment"
        ),
        "divide one 86400-second day"
    )
    expect_error(
        bias__subdaily_adjusted_series(
            data,
            "1hr",
            3600,
            "model_future",
            "test_adjustment"
        ),
        "declared `frequency`"
    )
})

test_that("daily adjusted series enforces canonical calendar-native data", {
    source <- bias_adjustment_test__series(
        "tas",
        2061L,
        c(280, 282, 284, 286)
    )
    result <- bias__daily_adjusted_series(
        source,
        output_role = "model_future",
        transformation = "additive",
        settings = list(transformation = "additive"),
        provenance = list(method = "test")
    )

    expect_true(S7::S7_inherits(result, DailyAdjustedSeries))
    expect_identical(result@data, source)
    expect_identical(result@output_role, "model_future")
    expect_identical(result@variable_metadata$tas$units, "K")
    expect_identical(result@variable_metadata$tas$frequency, "day")
    expect_identical(result@variable_metadata$tas$calendars, "noleap")

    for (calendar in CF_TIME_CALENDARS) {
        calendar_data <- bias_adjustment_test__series(
            "tas",
            2000L,
            c(280, 282, 284, 286),
            calendar = calendar
        )
        expect_no_error(
            bias__daily_adjusted_series(
                calendar_data,
                "model_future",
                "additive"
            )
        )
    }

    duplicate <- rbind(source, source[1L, ])
    expect_error(
        bias__daily_adjusted_series(
            duplicate,
            "model_future",
            "additive"
        ),
        "unique variable-calendar-year-month-day"
    )
    invalid_date <- source
    invalid_date$cf_day[[1L]] <- 31L
    invalid_date$cf_month[[1L]] <- 2L
    expect_error(
        bias__daily_adjusted_series(
            invalid_date,
            "model_future",
            "additive"
        ),
        "invalid date"
    )
    invalid_phase <- source
    invalid_phase$annual_phase[[1L]] <- 0.9
    expect_error(
        bias__daily_adjusted_series(
            invalid_phase,
            "model_future",
            "additive"
        ),
        "annual_phase"
    )
})

# Shared daily/subdaily validators must retain the raw reader's distinction
# when callers construct canonical weather tables directly.
test_that("CF climatology cannot enter ordinary bias-adjustment samples", {
    daily <- signal_test__series("tas", 2061L, c(280, 282, 284, 286))
    daily$time_kind <- "climatology"
    expect_match(bias__daily_data_error(daily), "climatolog")
    subdaily <- adjusted_series_test__subdaily()
    subdaily$time_kind <- "climatology"
    expect_match(
        bias__subdaily_data_error(subdaily, "3hr", 10800),
        "climatolog"
    )
})

test_that("shared three-role input validator enforces daily, subdaily, and missing contracts", {
    observed <- bias_adjustment_test__series(
        "tas",
        2001L,
        c(10, 12, 14, 16),
        calendar = "360_day"
    )
    historical <- bias_adjustment_test__series(
        "tas",
        1991L,
        c(9, 11, 13, 15),
        calendar = "noleap"
    )
    future <- bias_adjustment_test__series(
        "tas",
        2061L,
        c(11, 13, 15, 17),
        calendar = "all_leap"
    )
    roles <- list(
        observed_reference = observed,
        model_historical = historical,
        model_future = future
    )

    validated <- signal__validate_three_role_inputs(roles, "tas", "Test Method")
    expect_named(validated, SIGNAL_THREE_INPUT_ROLES)

    expect_error(
        signal__validate_three_role_inputs(roles[-3L], "tas", "Test Method"),
        "observed, historical-model, and future-model role payloads"
    )

    wrong_var <- roles
    wrong_var$model_future$variable_id <- "pr"
    expect_error(
        signal__validate_three_role_inputs(wrong_var, "tas", "Test Method"),
        "must contain only variable"
    )

    mixed_cal <- roles
    mixed_cal$observed_reference <- rbind(
        observed[1:2, ],
        bias_adjustment_test__series(
            "tas",
            2001L,
            c(14, 16),
            months = c(2L, 2L),
            days = c(1L, 2L),
            calendar = "noleap"
        )
    )
    expect_error(
        signal__validate_three_role_inputs(mixed_cal, "tas", "Test Method"),
        "one native calendar"
    )

    bad_units <- roles
    bad_units$model_future$units <- "degC"
    expect_error(
        signal__validate_three_role_inputs(bad_units, "tas", "Test Method"),
        "identical units"
    )

    negative <- roles
    negative$model_future$value[[1L]] <- -1
    expect_error(
        signal__validate_three_role_inputs(
            negative,
            "tas",
            "Test Method",
            min_value = 0
        ),
        "non-negative"
    )
    expect_error(
        signal__validate_three_role_inputs(
            negative,
            "tas",
            "Test Method",
            min_value = 0,
            min_value_message = "Custom lower-bound failure."
        ),
        "Custom lower-bound failure"
    )

    missing_roles <- roles
    missing_roles$observed_reference$value[[2L]] <- NA_real_
    expect_error(
        signal__validate_three_role_inputs(
            missing_roles,
            "tas",
            "Test Method",
            allow_missing = FALSE
        ),
        "missing values"
    )
    masked <- signal__validate_three_role_inputs(
        missing_roles,
        "tas",
        "Test Method",
        allow_missing = TRUE
    )
    expect_true(is.na(masked$observed_reference$value[[2L]]))
    expect_identical(masked$observed_reference$value[[1L]], 10)

    non_finite <- missing_roles
    non_finite$observed_reference$value[[1L]] <- Inf
    expect_error(
        signal__validate_three_role_inputs(
            non_finite,
            "tas",
            "Test Method",
            allow_missing = TRUE
        ),
        "non-finite"
    )

    subdaily_roles <- list(
        observed_reference = adjusted_series_test__subdaily(
            year = 2001L,
            frequency = "hour",
            time_step_seconds = 3600
        ),
        model_historical = adjusted_series_test__subdaily(
            year = 1991L,
            frequency = "hour",
            time_step_seconds = 3600
        ),
        model_future = adjusted_series_test__subdaily(
            year = 2061L,
            frequency = "hour",
            time_step_seconds = 3600
        )
    )
    subdaily_ok <- signal__validate_three_role_inputs(
        subdaily_roles,
        "tas",
        "Hourly Method",
        frequency = "hour",
        time_step_seconds = 3600
    )
    expect_identical(
        unique(subdaily_ok$model_future$frequency),
        "hour"
    )
    expect_error(
        signal__validate_three_role_inputs(
            subdaily_roles,
            "tas",
            "Three-Hourly Method",
            frequency = "3hr",
            time_step_seconds = 10800
        ),
        "declared `frequency`"
    )
    wrong_freq <- roles
    wrong_freq$model_future$frequency <- "hour"
    expect_error(
        signal__validate_three_role_inputs(
            wrong_freq,
            "tas",
            "Daily Method",
            frequency = "day"
        ),
        "must be `day`"
    )
})

test_that("shared sample summaries and monthly block runner aggregate diagnostics", {
    expect_identical(
        signal__sample_summary(c(2, 4, 6)),
        c(minimum = 2, median = 4, maximum = 6)
    )
    expect_identical(
        signal__sample_summary(c(2L, 6L), include_median = FALSE),
        c(minimum = 2L, maximum = 6L)
    )

    make_years <- function(years, offset) {
        do.call(
            rbind,
            lapply(years, function(year) {
                bias_adjustment_test__series(
                    "tas",
                    year,
                    c(10, 12, 20, 22) + offset
                )
            })
        )
    }
    series <- list(
        observed_reference = make_years(2001:2002, 0),
        model_historical = make_years(1991:1992, 0),
        model_future = make_years(2041:2044, 5)
    )
    run <- signal__run_monthly_blocks(
        series = series,
        future_window_years = 4L,
        output_block_years = 2L,
        min_samples = 2L,
        bounds = c(-Inf, 25),
        method_label = "Block Method",
        apply_window = function(
            month,
            block,
            observed_rows,
            historical_rows,
            future_rows,
            output_rows
        ) {
            list(
                value = series$model_future$value[future_rows] + 1,
                record = list(
                    month = month,
                    truncated_left = block$truncated_left,
                    truncated_right = block$truncated_right,
                    observed_samples = length(observed_rows),
                    historical_samples = length(historical_rows),
                    future_samples = length(future_rows)
                )
            )
        }
    )

    expect_equal(
        run$value,
        pmin(series$model_future$value + 1, 25)
    )
    expect_identical(run$diagnostics$window_count, 4L)
    expect_identical(run$diagnostics$truncated_edge_windows, 4L)
    expect_identical(run$diagnostics$clipped_values, 8L)
    expect_identical(
        run$diagnostics$observed_window_samples,
        c(minimum = 4, median = 4, maximum = 4)
    )
})

test_that("all built-in signal and preprocess components satisfy registration and stage contracts", {
    contracts <- list(
        list(
            stage = "signal",
            name = "linear_scaling_daily",
            register = bias__register_linear_scaling_component,
            input_kinds = "calendar_indexed_daily_series",
            output_kinds = "daily_adjusted_series",
            scopes = "univariate",
            stochastic = FALSE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = c("tas", "tasmin", "tasmax", "pr"),
            published = c("tas", "tasmin", "tasmax", "pr"),
            experimental = character()
        ),
        list(
            stage = "signal",
            name = "delta_change_daily",
            register = bias__register_delta_change_component,
            input_kinds = "calendar_indexed_daily_series",
            output_kinds = "daily_adjusted_series",
            scopes = "univariate",
            stochastic = FALSE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = c("tas", "tasmin", "tasmax", "pr"),
            published = c("tas", "tasmin", "tasmax", "pr"),
            experimental = character()
        ),
        list(
            stage = "signal",
            name = "quantile_mapping_daily",
            register = qm__register_component,
            input_kinds = "calendar_indexed_daily_series",
            output_kinds = "daily_adjusted_series",
            scopes = "univariate",
            stochastic = TRUE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = c(QM_PUBLISHED_VARIABLES, QM_EXPERIMENTAL_VARIABLES),
            published = QM_PUBLISHED_VARIABLES,
            experimental = QM_EXPERIMENTAL_VARIABLES
        ),
        list(
            stage = "signal",
            name = "quantile_delta_mapping_daily",
            register = qdm__register_component,
            input_kinds = "calendar_indexed_daily_series",
            output_kinds = "daily_adjusted_series",
            scopes = "univariate",
            stochastic = TRUE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = c(QDM_PUBLISHED_VARIABLES, QDM_EXPERIMENTAL_VARIABLES),
            published = QDM_PUBLISHED_VARIABLES,
            experimental = QDM_EXPERIMENTAL_VARIABLES
        ),
        list(
            stage = "signal",
            name = "cdf_transform_daily",
            register = cdft__register_component,
            input_kinds = "calendar_indexed_daily_series",
            output_kinds = "daily_adjusted_series",
            scopes = "univariate",
            stochastic = TRUE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = CDFT_FAMIEN_VARIABLES,
            published = CDFT_FAMIEN_VARIABLES,
            experimental = character()
        ),
        list(
            stage = "signal",
            name = "equidistant_cdf_matching_daily",
            register = edcdf__register_component,
            input_kinds = "calendar_indexed_daily_series",
            output_kinds = "daily_adjusted_series",
            scopes = "univariate",
            stochastic = FALSE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = EDCDF_LI_VARIABLES,
            published = character(),
            experimental = EDCDF_LI_VARIABLES
        ),
        list(
            stage = "signal",
            name = "scaled_distribution_mapping_daily",
            register = sdm__register_component,
            input_kinds = "calendar_indexed_daily_series",
            output_kinds = "daily_adjusted_series",
            scopes = "univariate",
            stochastic = FALSE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = c(SDM_PUBLISHED_VARIABLES, SDM_EXPERIMENTAL_VARIABLES),
            published = SDM_PUBLISHED_VARIABLES,
            experimental = SDM_EXPERIMENTAL_VARIABLES
        ),
        list(
            stage = "signal",
            name = "isimip3basd_daily",
            register = isimip__register_component,
            input_kinds = "calendar_indexed_daily_series",
            output_kinds = "daily_adjusted_series",
            scopes = "univariate",
            stochastic = TRUE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = ISIMIP_VARIABLES,
            published = ISIMIP_VARIABLES,
            experimental = character()
        ),
        list(
            stage = "signal",
            name = "kernel_quantile_delta_mapping_hourly",
            register = kqdm__register_component,
            input_kinds = "calendar_indexed_hourly_series",
            output_kinds = "subdaily_adjusted_series",
            scopes = "univariate",
            stochastic = FALSE,
            required_roles = SIGNAL_THREE_INPUT_ROLES,
            variables = c(
                KQDM_PUBLISHED_VARIABLES,
                KQDM_EXPERIMENTAL_VARIABLES
            ),
            published = character(),
            experimental = c(
                KQDM_PUBLISHED_VARIABLES,
                KQDM_EXPERIMENTAL_VARIABLES
            )
        ),
        list(
            stage = "preprocess",
            name = "linear_temporal_interpolation",
            register = temporal__register_linear_component,
            input_kinds = "role_inputs",
            output_kinds = "hourly_role_inputs",
            scopes = "univariate",
            stochastic = FALSE,
            required_roles = c("model_historical", "model_future"),
            variables = TEMPORAL_LINEAR_VARIABLES
        ),
        list(
            stage = "preprocess",
            name = "solar_radiation_interpolation",
            register = solar__register_component,
            input_kinds = "role_inputs",
            output_kinds = "hourly_role_inputs",
            scopes = "univariate",
            stochastic = FALSE,
            required_roles = c("model_historical", "model_future"),
            variables = SOLAR_RADIATION_VARIABLES
        )
    )

    for (spec in contracts) {
        spec$register()
        component <- component__get(spec$stage, spec$name)
        expect_true(
            S7::S7_inherits(component, WeatherComponentSpec),
            info = spec$name
        )
        expect_identical(component@stage, spec$stage, info = spec$name)
        expect_identical(
            component@input_kinds,
            spec$input_kinds,
            info = spec$name
        )
        expect_identical(
            component@output_kinds,
            spec$output_kinds,
            info = spec$name
        )
        expect_identical(component@scopes, spec$scopes, info = spec$name)
        expect_identical(
            component@stochastic,
            spec$stochastic,
            info = spec$name
        )
        expect_identical(
            sort(names(component@required_inputs)),
            sort(spec$required_roles),
            info = spec$name
        )
        for (role in spec$required_roles) {
            req_vars <- unlist(
                component@required_inputs[[role]]@variable_sets,
                use.names = FALSE
            )
            expect_identical(
                sort(req_vars),
                sort(spec$variables),
                info = paste(spec$name, role)
            )
        }

        if (identical(spec$stage, "signal")) {
            profiles <- component@metadata$signal_profiles
            expect_identical(
                sort(names(profiles)),
                sort(spec$variables),
                info = spec$name
            )
            for (variable in spec$published) {
                expect_identical(
                    profiles[[variable]]$evidence,
                    "published",
                    info = paste(spec$name, variable)
                )
                expect_true(
                    length(profiles[[variable]]$references) >= 1L,
                    info = paste(spec$name, variable)
                )
            }
            for (variable in spec$experimental) {
                expect_identical(
                    profiles[[variable]]$evidence,
                    "experimental",
                    info = paste(spec$name, variable)
                )
            }

            upstream_calendar <- component__spec(
                name = paste0(spec$name, "_calendar_test"),
                stage = "calendar",
                input_kinds = "preprocessed_series",
                output_kinds = spec$input_kinds,
                operations = list(apply = identity)
            )
            downstream_sequence <- component__spec(
                name = paste0(spec$name, "_sequence_test"),
                stage = "sequence",
                input_kinds = spec$output_kinds,
                output_kinds = "weather_sequence",
                operations = list(generate = identity)
            )
            expect_true(
                component__compatible(upstream_calendar, component),
                info = spec$name
            )
            expect_true(
                component__compatible(component, downstream_sequence),
                info = spec$name
            )
        } else {
            expect_identical(
                sort(component@metadata$supported_variables),
                sort(spec$variables),
                info = spec$name
            )
            downstream_calendar <- component__spec(
                name = paste0(spec$name, "_calendar_test"),
                stage = "calendar",
                input_kinds = spec$output_kinds,
                output_kinds = "calendar_indexed_hourly_series",
                operations = list(apply = identity)
            )
            expect_true(
                component__compatible(component, downstream_calendar),
                info = spec$name
            )
        }
    }
})

# vim: fdm=marker :
