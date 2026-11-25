#' @include signal-adjustment.R distribution-parametric.R
NULL

# Scaled Distribution Mapping follows the published daily temperature and
# precipitation equations and records the public reference implementation
# separately as reproducibility evidence.
SDM_REFERENCES <- c(
    "https://doi.org/10.5194/hess-21-2649-2017",
    "https://github.com/wegener-center/pyCAT"
)

# Switanek et al. treat precipitation below 0.1 mm/day as dry. CMIP6 daily
# precipitation flux expresses the same threshold in kg m-2 s-1.
SDM_PR_DRY_THRESHOLD <- 0.1 / 86400

# The publication directly evaluates daily temperature and precipitation.
# Applying the same Normal branch to daily extrema remains an explicit package
# experiment until separately validated.
SDM_PUBLISHED_VARIABLES <- c("pr", "tas")
SDM_EXPERIMENTAL_VARIABLES <- c("tasmin", "tasmax")

# Construct one complete SDM settings record. Calendar-month grouping,
# truncated edge windows, wet-day retention, zero-location Gamma MLE, and
# linear normalized-rank interpolation are fixed published conventions recorded
# in component metadata and result provenance.
# sdm__default_settings {{{
sdm__default_settings <- function(
    bounds,
    mapping_type = c("absolute", "relative"),
    distribution = c("normal", "gamma"),
    detrending = c("linear", "none"),
    dry_threshold = 0,
    cdf_epsilon = 1e-4
) {
    mapping_type <- match.arg(mapping_type)
    distribution <- match.arg(distribution)
    detrending <- match.arg(detrending)
    list(
        mapping_type = mapping_type,
        distribution = distribution,
        detrending = detrending,
        future_window_years = 30L,
        output_block_years = 10L,
        min_samples = 10L,
        cdf_epsilon = cdf_epsilon,
        bounds = bounds,
        dry_threshold = dry_threshold,
        gamma_fit_tolerance = 1e-10,
        gamma_fit_max_iterations = 1000L
    )
}
# }}}

# Build evidence-aware profiles without attributing the extrema defaults to
# the SDM publication.
# sdm__profiles {{{
sdm__profiles <- function() {
    normal_settings <- sdm__default_settings(
        c(-Inf, Inf),
        "absolute",
        "normal",
        "linear",
        cdf_epsilon = 1e-4
    )
    settings <- list(
        pr = sdm__default_settings(
            c(0, Inf),
            "relative",
            "gamma",
            "none",
            SDM_PR_DRY_THRESHOLD,
            1e-7
        ),
        tas = normal_settings,
        tasmin = normal_settings,
        tasmax = normal_settings
    )
    signal__build_profiles(
        settings_by_variable = settings,
        published_variables = SDM_PUBLISHED_VARIABLES,
        references = SDM_REFERENCES,
        method = "scaled_distribution_mapping",
        output_role = "model_future",
        evidence_mode = "by_variable",
        metadata_fn = function(variable, published) {
            list(
                default_source = if (published) {
                    "method_literature"
                } else {
                    "package_implementation"
                },
                temporal_policy_source = "method_literature",
                seasonal_grouping = "calendar_month",
                edge_policy = "truncate",
                wet_day_increase_policy = "retain_future_count",
                gamma_fit_method = "maximum_likelihood_zero_location",
                rank_interpolation = "linear_normalized_rank"
            )
        }
    )
}
# }}}

# Validate all published and numerical SDM choices at the signal-kernel
# boundary so no incompatible distribution branch can be selected silently.
# sdm__settings {{{
sdm__settings <- function(settings) {
    expected <- c(
        "mapping_type",
        "distribution",
        "detrending",
        "future_window_years",
        "output_block_years",
        "min_samples",
        "cdf_epsilon",
        "bounds",
        "dry_threshold",
        "gamma_fit_tolerance",
        "gamma_fit_max_iterations"
    )
    resolved <- signal__resolve_settings(
        settings,
        expected,
        "Scaled Distribution Mapping"
    )
    checkmate::assert_choice(resolved$mapping_type, c("absolute", "relative"))
    checkmate::assert_choice(resolved$distribution, c("normal", "gamma"))
    checkmate::assert_choice(resolved$detrending, c("linear", "none"))
    if (
        identical(resolved$mapping_type, "absolute") &&
            (!identical(resolved$distribution, "normal") ||
                !identical(resolved$detrending, "linear"))
    ) {
        cli::cli_abort(
            "Absolute Scaled Distribution Mapping requires a Normal distribution and linear detrending."
        )
    }
    if (
        identical(resolved$mapping_type, "relative") &&
            (!identical(resolved$distribution, "gamma") ||
                !identical(resolved$detrending, "none"))
    ) {
        cli::cli_abort(
            "Relative Scaled Distribution Mapping requires a Gamma distribution without detrending."
        )
    }
    resolved <- signal__validate_future_block_window(resolved)
    resolved$min_samples <- signal__integer_setting(
        resolved$min_samples,
        "min_samples",
        lower = 2L
    )
    resolved$cdf_epsilon <- signal__positive_number(
        resolved$cdf_epsilon,
        "cdf_epsilon",
        upper = 0.5,
        strict_upper = TRUE
    )
    signal__ordered_bounds(
        resolved$bounds,
        "Scaled Distribution Mapping bounds must be ordered from lower to upper."
    )
    checkmate::assert_number(
        resolved$dry_threshold,
        lower = 0,
        finite = TRUE
    )
    if (
        identical(resolved$mapping_type, "relative") &&
            resolved$dry_threshold <= 0
    ) {
        cli::cli_abort(
            "Relative Scaled Distribution Mapping requires a positive `dry_threshold`."
        )
    }
    resolved$gamma_fit_tolerance <- signal__positive_number(
        resolved$gamma_fit_tolerance,
        "gamma_fit_tolerance"
    )
    resolved$gamma_fit_max_iterations <- signal__integer_setting(
        resolved$gamma_fit_max_iterations,
        "gamma_fit_max_iterations",
        lower = 1L
    )
    resolved
}
# }}}

# Convert native CF coordinates to a monotonic within-role time coordinate for
# the published linear detrending step.
# sdm__time_coordinate {{{
sdm__time_coordinate <- function(data) {
    as.numeric(data[["cf_year"]]) + as.numeric(data[["annual_phase"]])
}
# }}}

# Remove a least-squares linear trend while retaining the fitted values needed
# to restore the future model's temporal evolution after adjustment.
# sdm__detrend {{{
sdm__detrend <- function(values, time) {
    checkmate::assert_numeric(
        values,
        min.len = 2L,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_numeric(
        time,
        len = length(values),
        finite = TRUE,
        any.missing = FALSE
    )
    centered_time <- time - mean(time)
    if (!any(centered_time != 0)) {
        cli::cli_abort(
            "Linear SDM detrending requires at least two distinct time coordinates."
        )
    }
    fit <- stats::lm.fit(
        x = cbind(intercept = 1, time = centered_time),
        y = values
    )
    trend <- as.numeric(fit$fitted.values)
    list(
        residual = as.numeric(values - trend),
        trend = trend,
        coefficients = stats::setNames(
            as.numeric(fit$coefficients),
            c("intercept", "slope")
        )
    )
}
# }}}

# Convert fitted CDF probabilities to one- or two-tailed recurrence intervals,
# following Eqs. (4) and (9) in Switanek et al.
# sdm__recurrence_interval {{{
sdm__recurrence_interval <- function(
    probability,
    tails = c("one", "two")
) {
    tails <- match.arg(tails)
    checkmate::assert_numeric(
        probability,
        lower = 0,
        upper = 1,
        finite = TRUE,
        any.missing = FALSE
    )
    if (identical(tails, "one")) {
        return(1 / (1 - probability))
    }
    1 / (0.5 - abs(probability - 0.5))
}
# }}}

# Scale observed recurrence intervals by the projected-to-historical modeled
# recurrence ratio, then convert the result back to a CDF probability. This is
# the event-likelihood adjustment in Eqs. (5), (6), and (10).
# sdm__scaled_probability {{{
sdm__scaled_probability <- function(
    observed_probability,
    historical_probability,
    future_probability,
    tails = c("one", "two"),
    epsilon
) {
    tails <- match.arg(tails)
    observed_ri <- sdm__recurrence_interval(
        observed_probability,
        tails
    )
    historical_ri <- sdm__recurrence_interval(
        historical_probability,
        tails
    )
    future_ri <- sdm__recurrence_interval(future_probability, tails)
    scaled_ri <- pmax(
        1,
        observed_ri * future_ri / historical_ri
    )
    probability <- if (identical(tails, "one")) {
        1 - 1 / scaled_ri
    } else {
        0.5 +
            sign(observed_probability - 0.5) *
                abs(0.5 - 1 / scaled_ri)
    }
    list(
        probability = distribution__clamp_probability(
            probability,
            epsilon
        ),
        recurrence_interval = scaled_ri
    )
}
# }}}

# Calculate the published future wet-day count
# n_future_wet * (p_observed_wet / p_historical_wet), while retaining the
# method's limitation that dry future days are never turned into wet days.
# sdm__expected_wet_days {{{
sdm__expected_wet_days <- function(
    future_wet,
    future_total,
    observed_wet,
    observed_total,
    historical_wet,
    historical_total
) {
    counts <- c(
        future_wet,
        future_total,
        observed_wet,
        observed_total,
        historical_wet,
        historical_total
    )
    checkmate::assert_integerish(
        counts,
        lower = 0L,
        any.missing = FALSE
    )
    if (
        future_total <= 0L ||
            observed_total <= 0L ||
            historical_total <= 0L ||
            historical_wet <= 0L
    ) {
        cli::cli_abort(
            "Scaled Distribution Mapping wet-day adjustment requires positive totals and at least one historical-model wet day."
        )
    }
    # Reject impossible role counts before applying the published frequency
    # ratio so malformed diagnostics cannot become a plausible integer result.
    wet <- counts[c(1L, 3L, 5L)]
    total <- counts[c(2L, 4L, 6L)]
    if (any(wet > total)) {
        cli::cli_abort(
            "Scaled Distribution Mapping wet-day counts cannot exceed their corresponding total-day counts."
        )
    }
    requested <- as.integer(round(
        future_wet *
            (observed_wet / observed_total) /
            (historical_wet / historical_total)
    ))
    list(
        requested = requested,
        retained = min(requested, as.integer(future_wet)),
        increase_not_supported = requested > future_wet
    )
}
# }}}

# Retrieve a named fitted parameter while producing NA for a parameter that
# belongs only to the other distribution family.
# sdm__fit_parameter {{{
sdm__fit_parameter <- function(fit, parameter) {
    value <- fit$parameters[[parameter]]
    if (is.null(value)) NA_real_ else as.numeric(value)
}
# }}}

# Flatten three distribution fits into inspectable per-window provenance.
# sdm__fit_record {{{
sdm__fit_record <- function(observed, historical, future) {
    list(
        distribution = observed$family,
        observed_location = sdm__fit_parameter(observed, "location"),
        observed_shape = sdm__fit_parameter(observed, "shape"),
        observed_scale = sdm__fit_parameter(observed, "scale"),
        historical_location = sdm__fit_parameter(
            historical,
            "location"
        ),
        historical_shape = sdm__fit_parameter(historical, "shape"),
        historical_scale = sdm__fit_parameter(historical, "scale"),
        future_location = sdm__fit_parameter(future, "location"),
        future_shape = sdm__fit_parameter(future, "shape"),
        future_scale = sdm__fit_parameter(future, "scale")
    )
}
# }}}

# Apply the published absolute temperature branch to one monthly future
# fitting window. Corrected residuals are centered before the modeled trend and
# observed-minus-historical mean bias are restored, matching the public pyCAT
# reference implementation.
# sdm__absolute_window {{{
sdm__absolute_window <- function(
    observed,
    historical,
    future,
    epsilon
) {
    observed_detrended <- sdm__detrend(
        observed[["value"]],
        sdm__time_coordinate(observed)
    )
    historical_detrended <- sdm__detrend(
        historical[["value"]],
        sdm__time_coordinate(historical)
    )
    future_detrended <- sdm__detrend(
        future[["value"]],
        sdm__time_coordinate(future)
    )
    fit_observed <- distribution__fit_normal(
        observed_detrended$residual
    )
    fit_historical <- distribution__fit_normal(
        historical_detrended$residual
    )
    fit_future <- distribution__fit_normal(
        future_detrended$residual
    )

    future_order <- order(
        future_detrended$residual,
        seq_along(future_detrended$residual)
    )
    observed_probability <- sort(distribution__clamp_probability(
        distribution__cdf(
            fit_observed,
            observed_detrended$residual
        ),
        epsilon
    ))
    historical_probability <- sort(distribution__clamp_probability(
        distribution__cdf(
            fit_historical,
            historical_detrended$residual
        ),
        epsilon
    ))
    future_probability <- distribution__clamp_probability(
        distribution__cdf(
            fit_future,
            future_detrended$residual[future_order]
        ),
        epsilon
    )
    observed_probability <- distribution__interpolate_ordered(
        observed_probability,
        length(future_probability)
    )
    historical_probability <- distribution__interpolate_ordered(
        historical_probability,
        length(future_probability)
    )

    # Eq. (8): preserve the modeled quantile delta after scaling it by the
    # observed-to-historical modeled standard-deviation ratio.
    scaling <- (distribution__quantile(fit_future, future_probability) -
        distribution__quantile(
            fit_historical,
            future_probability
        )) *
        fit_observed$parameters$scale /
        fit_historical$parameters$scale
    scaled <- sdm__scaled_probability(
        observed_probability,
        historical_probability,
        future_probability,
        tails = "two",
        epsilon = epsilon
    )
    corrected_sorted <- (distribution__quantile(
        fit_observed,
        scaled$probability
    ) +
        scaling)
    corrected_sorted <- corrected_sorted - mean(corrected_sorted)
    corrected_residual <- numeric(length(corrected_sorted))
    corrected_residual[future_order] <- corrected_sorted

    # Restoring the modeled fitted trend and historical mean bias makes the
    # adjusted mean equal observed + (future model - historical model).
    target_mean <- mean(observed$value) +
        mean(future$value) -
        mean(historical$value)
    future_trend_anomaly <- future_detrended$trend -
        mean(future$value)
    list(
        value = corrected_residual +
            target_mean +
            future_trend_anomaly,
        fits = sdm__fit_record(
            fit_observed,
            fit_historical,
            fit_future
        ),
        diagnostics = list(
            target_mean = target_mean,
            adjusted_mean = mean(
                corrected_residual +
                    target_mean +
                    future_trend_anomaly
            ),
            observed_detrend = observed_detrended$coefficients,
            historical_detrend = historical_detrended$coefficients,
            future_detrend = future_detrended$coefficients,
            scaled_probability_range = range(scaled$probability),
            scaled_recurrence_interval_range = range(
                scaled$recurrence_interval
            )
        )
    )
}
# }}}

# Apply the published relative precipitation branch to one monthly future
# fitting window. Positive amounts use zero-location Gamma fits; expected wet
# values are rank-normalized and reinserted on the largest future events.
# sdm__relative_window {{{
sdm__relative_window <- function(
    observed,
    historical,
    future,
    resolved
) {
    threshold <- resolved$dry_threshold
    observed_wet <- observed$value >= threshold
    historical_wet <- historical$value >= threshold
    future_wet <- future$value >= threshold
    wet_counts <- c(
        observed_reference = sum(observed_wet),
        model_historical = sum(historical_wet),
        model_future = sum(future_wet)
    )
    if (any(wet_counts < resolved$min_samples)) {
        cli::cli_abort(
            "Relative Scaled Distribution Mapping has fewer than {resolved$min_samples} wet values in an observed, historical, or future fitting window."
        )
    }
    observed_positive <- sort(observed$value[observed_wet])
    historical_positive <- sort(historical$value[historical_wet])
    future_order <- order(future$value, seq_along(future$value))
    future_positive <- future$value[future_order][
        future$value[future_order] >= threshold
    ]

    fit_observed <- distribution__fit_gamma(
        observed_positive,
        tolerance = resolved$gamma_fit_tolerance,
        max_iterations = resolved$gamma_fit_max_iterations
    )
    fit_historical <- distribution__fit_gamma(
        historical_positive,
        tolerance = resolved$gamma_fit_tolerance,
        max_iterations = resolved$gamma_fit_max_iterations
    )
    fit_future <- distribution__fit_gamma(
        future_positive,
        tolerance = resolved$gamma_fit_tolerance,
        max_iterations = resolved$gamma_fit_max_iterations
    )
    observed_probability <- distribution__clamp_probability(
        distribution__cdf(fit_observed, observed_positive),
        resolved$cdf_epsilon
    )
    historical_probability <- distribution__clamp_probability(
        distribution__cdf(fit_historical, historical_positive),
        resolved$cdf_epsilon
    )
    future_probability <- distribution__clamp_probability(
        distribution__cdf(fit_future, future_positive),
        resolved$cdf_epsilon
    )
    observed_probability <- distribution__interpolate_ordered(
        observed_probability,
        length(future_probability)
    )
    historical_probability <- distribution__interpolate_ordered(
        historical_probability,
        length(future_probability)
    )

    # Eq. (3): preserve the modeled multiplicative change at each future
    # fitted probability.
    historical_quantile <- distribution__quantile(
        fit_historical,
        future_probability
    )
    if (any(historical_quantile <= 0)) {
        cli::cli_abort(
            "Relative Scaled Distribution Mapping encountered a non-positive historical-model Gamma quantile."
        )
    }
    scaling <- distribution__quantile(
        fit_future,
        future_probability
    ) /
        historical_quantile
    scaled <- sdm__scaled_probability(
        observed_probability,
        historical_probability,
        future_probability,
        tails = "one",
        epsilon = resolved$cdf_epsilon
    )
    initial <- distribution__quantile(
        fit_observed,
        scaled$probability
    ) *
        scaling
    expected <- sdm__expected_wet_days(
        future_wet = sum(future_wet),
        future_total = nrow(future),
        observed_wet = sum(observed_wet),
        observed_total = nrow(observed),
        historical_wet = sum(historical_wet),
        historical_total = nrow(historical)
    )
    adjusted_positive <- distribution__interpolate_ordered(
        initial,
        expected$retained
    )
    adjusted <- numeric(nrow(future))
    if (expected$retained > 0L) {
        retained_rows <- tail(future_order, expected$retained)
        adjusted[retained_rows] <- adjusted_positive
    }
    list(
        value = adjusted,
        fits = sdm__fit_record(
            fit_observed,
            fit_historical,
            fit_future
        ),
        diagnostics = list(
            wet_counts = wet_counts,
            expected_wet_days = expected,
            adjusted_wet_days = sum(adjusted > 0),
            adjusted_positive_below_threshold_days = sum(
                adjusted > 0 & adjusted < threshold
            ),
            scaled_probability_range = range(scaled$probability),
            scaled_recurrence_interval_range = range(
                scaled$recurrence_interval
            ),
            scaling_range = range(scaling)
        )
    )
}
# }}}

# Convert one transformed month/window into a compact inspectable record that
# retains distribution fits, sample coverage, temporal policy, and wet-day
# limitations without storing every daily intermediate value.
# sdm__window_record {{{
sdm__window_record <- function(
    month,
    block,
    observed,
    historical,
    future,
    output_rows,
    transformed
) {
    c(
        list(
            month = as.integer(month),
            output_start_year = min(block$output_years),
            output_end_year = max(block$output_years),
            future_start_year = min(block$window_years),
            future_end_year = max(block$window_years),
            requested_future_start_year = block$requested_start,
            requested_future_end_year = block$requested_end,
            truncated_left = block$truncated_left,
            truncated_right = block$truncated_right,
            observed_samples = nrow(observed),
            historical_samples = nrow(historical),
            future_samples = nrow(future),
            output_samples = length(output_rows)
        ),
        transformed$fits,
        transformed$diagnostics
    )
}
# }}}

# Execute disjoint retained year blocks for every calendar month. Each
# transformation is fitted on its surrounding future window, but only the
# retained block is written, preventing overlapping values from being adjusted
# more than once.
# sdm__adjust_values {{{
sdm__adjust_values <- function(series, resolved) {
    observed <- series$observed_reference
    historical <- series$model_historical
    future <- series$model_future

    run <- signal__run_monthly_blocks(
        series = series,
        future_window_years = resolved$future_window_years,
        output_block_years = resolved$output_block_years,
        min_samples = resolved$min_samples,
        bounds = resolved$bounds,
        method_label = "Scaled Distribution Mapping",
        apply_window = function(
            month,
            block,
            observed_rows,
            historical_rows,
            future_rows,
            output_rows
        ) {
            observed_window <- observed[observed_rows, , drop = FALSE]
            historical_window <- historical[historical_rows, , drop = FALSE]
            future_window <- future[future_rows, , drop = FALSE]
            transformed <- if (identical(resolved$mapping_type, "absolute")) {
                sdm__absolute_window(
                    observed_window,
                    historical_window,
                    future_window,
                    resolved$cdf_epsilon
                )
            } else {
                sdm__relative_window(
                    observed_window,
                    historical_window,
                    future_window,
                    resolved
                )
            }
            list(
                value = transformed$value,
                record = sdm__window_record(
                    month,
                    block,
                    observed_window,
                    historical_window,
                    future_window,
                    output_rows,
                    transformed
                )
            )
        }
    )

    diagnostics <- run$diagnostics
    if (identical(resolved$mapping_type, "relative")) {
        bounded <- run$value
        records <- diagnostics$windows
        diagnostics$precipitation <- list(
            dry_threshold = resolved$dry_threshold,
            input_dry_values = c(
                observed_reference = sum(
                    observed$value < resolved$dry_threshold
                ),
                model_historical = sum(
                    historical$value < resolved$dry_threshold
                ),
                model_future = sum(
                    future$value < resolved$dry_threshold
                )
            ),
            output_dry_values = sum(bounded == 0),
            output_positive_below_threshold_values = sum(
                bounded > 0 & bounded < resolved$dry_threshold
            ),
            wet_day_increase_not_supported_windows = sum(vapply(
                records,
                function(record) {
                    isTRUE(record$expected_wet_days$increase_not_supported)
                },
                logical(1L)
            ))
        )
    }
    list(value = run$value, diagnostics = diagnostics)
}
# }}}

# Execute SDM for one aligned univariate signal group and return the common
# future-backbone DailyAdjustedSeries contract.
# sdm__apply_group {{{
sdm__apply_group <- function(inputs, settings, key) {
    resolved <- sdm__settings(settings)
    variable <- names(settings)[[1L]]
    is_relative <- identical(resolved$mapping_type, "relative")
    series <- signal__validate_three_role_inputs(
        inputs,
        variable = variable,
        method = "Scaled Distribution Mapping",
        min_value = if (is_relative) 0 else -Inf,
        min_value_message = if (is_relative) {
            "Relative Scaled Distribution Mapping requires non-negative input values."
        } else {
            NULL
        }
    )
    mapped <- sdm__adjust_values(series, resolved)
    future <- series$model_future
    future[["value"]] <- mapped$value

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = "scaled_distribution_mapping",
        settings = resolved,
        provenance = list(
            method = "scaled_distribution_mapping",
            references = SDM_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            temporal_policy = list(
                seasonal_grouping = "calendar_month",
                future_window_years = resolved$future_window_years,
                output_block_years = resolved$output_block_years,
                edge_policy = "truncate",
                source = if (
                    resolved$future_window_years == 30L &&
                        resolved$output_block_years == 10L
                ) {
                    "method_literature"
                } else {
                    "user_override"
                }
            ),
            method_conventions = list(
                wet_day_increase_policy = "retain_future_count",
                gamma_fit_method = "maximum_likelihood_zero_location",
                rank_interpolation = "linear_normalized_rank"
            ),
            diagnostics = mapped$diagnostics
        )
    )
}
# }}}

# Construct the reusable daily SDM signal with three explicit input roles and
# evidence-aware variable alternatives.
# sdm__component {{{
sdm__component <- function() {
    signal__three_role_component(
        name = "scaled_distribution_mapping_daily",
        label = "Daily Scaled Distribution Mapping",
        variables = c(
            SDM_PUBLISHED_VARIABLES,
            SDM_EXPERIMENTAL_VARIABLES
        ),
        profiles = sdm__profiles(),
        apply_group = sdm__apply_group,
        validate_options = sdm__settings,
        method_label = "Scaled Distribution Mapping",
        output_role = "model_future",
        metadata = list(
            method_family = "parametric_distribution_mapping",
            reference_implementation = "wegener-center/pyCAT",
            temporal_policy = "calendar_month_30_year_window_10_year_block",
            seasonal_grouping = "calendar_month",
            edge_policy = "truncate",
            wet_day_increase_policy = "retain_future_count",
            gamma_fit_method = "maximum_likelihood_zero_location",
            rank_interpolation = "linear_normalized_rank"
        )
    )
}
# }}}

# Register the native SDM component during package load.
# sdm__register_component {{{
sdm__register_component <- function() {
    component__register_builtin(sdm__component())
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
