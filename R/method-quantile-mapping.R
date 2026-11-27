#' @include signal-adjustment.R calendar.R distribution.R
NULL

# Quantile Mapping references identify the published transfer equation and
# the review used to delimit the method's assumptions and evidence.
QM_REFERENCES <- c(
    "https://doi.org/10.1175/JCLI-D-14-00754.1",
    "https://doi.org/10.1007/s40641-016-0050-x"
)

# A 0.1 mm/day trace threshold is expressed in the native CMIP6 precipitation
# flux unit kg m-2 s-1; callers using another unit must override the setting.
QM_PR_DRY_THRESHOLD <- 0.1 / 86400

# The method literature supports direct Quantile Mapping defaults for mean
# temperature and precipitation. Other variable profiles are implementation
# choices and therefore remain explicitly experimental.
QM_PUBLISHED_VARIABLES <- c("pr", "tas")
QM_EXPERIMENTAL_VARIABLES <- c(
    "hurs",
    "psl",
    "rlds",
    "sfcWind",
    "tasmin",
    "tasmax"
)

# Construct the tunable empirical Quantile Mapping settings record shared by
# every variable profile. Fixed empirical-CDF conventions remain in component
# metadata and output provenance.
# qm__default_settings {{{
qm__default_settings <- function(
    bounds,
    distribution_model = c("continuous", "precipitation_hurdle"),
    dry_threshold = 0
) {
    distribution_model <- match.arg(distribution_model)
    list(
        seasonal_window_days = 31L,
        target_year_days = 365L,
        min_samples = 10L,
        bounds = bounds,
        distribution_model = distribution_model,
        dry_threshold = dry_threshold,
        random_seed = 1L
    )
}
# }}}

# Build published and experimental profiles without assigning literature
# provenance to implementation-selected variable defaults.
# qm__profiles {{{
qm__profiles <- function() {
    settings <- list(
        pr = qm__default_settings(
            c(0, Inf),
            "precipitation_hurdle",
            QM_PR_DRY_THRESHOLD
        ),
        tas = qm__default_settings(c(-Inf, Inf)),
        hurs = qm__default_settings(c(0, 100)),
        psl = qm__default_settings(c(0, Inf)),
        rlds = qm__default_settings(c(0, Inf)),
        sfcWind = qm__default_settings(c(0, Inf)),
        tasmin = qm__default_settings(c(-Inf, Inf)),
        tasmax = qm__default_settings(c(-Inf, Inf))
    )
    signal__build_profiles(
        settings_by_variable = settings,
        published_variables = QM_PUBLISHED_VARIABLES,
        references = QM_REFERENCES,
        method = "quantile_mapping",
        output_role = "model_future",
        evidence_mode = "by_variable",
        metadata_fn = function(variable, published) {
            list(
                default_source = if (published) {
                    "method_literature"
                } else {
                    "package_implementation"
                }
            )
        }
    )
}
# }}}

# Validate all tunable method settings at the kernel boundary.
# qm__settings {{{
qm__settings <- function(settings) {
    expected <- c(
        "seasonal_window_days",
        "target_year_days",
        "min_samples",
        "bounds",
        "distribution_model",
        "dry_threshold",
        "random_seed"
    )
    resolved <- signal__resolve_settings(
        settings,
        expected,
        "Quantile Mapping"
    )
    resolved$seasonal_window_days <- signal__integer_setting(
        resolved$seasonal_window_days,
        "seasonal_window_days",
        lower = 1L
    )
    resolved$target_year_days <- signal__integer_setting(
        resolved$target_year_days,
        "target_year_days",
        lower = 3L
    )
    resolved$min_samples <- signal__integer_setting(
        resolved$min_samples,
        "min_samples",
        lower = 2L
    )
    # Reuse the calendar-neutral window validator to enforce odd widths and
    # prevent a wider-than-year seasonal window.
    daily__window_spec(
        resolved$seasonal_window_days,
        resolved$target_year_days
    )
    signal__ordered_bounds(
        resolved$bounds,
        "Quantile Mapping bounds must be ordered from lower to upper."
    )
    checkmate::assert_choice(
        resolved$distribution_model,
        c("continuous", "precipitation_hurdle")
    )
    checkmate::assert_number(
        resolved$dry_threshold,
        lower = 0,
        finite = TRUE
    )
    resolved$random_seed <- signal__random_seed(resolved$random_seed)
    resolved
}
# }}}

# Apply x* = F_obs^{-1}(F_hist(x_future)) with the declared interpolation,
# tie, and tail conventions to one or more future values.
# qm__map_continuous {{{
qm__map_continuous <- function(historical, observed, future) {
    cdf <- quantile__empirical_cdf(historical, future)
    list(
        value = quantile__inverse_cdf(observed, cdf$probability),
        probability = cdf$probability,
        lower_tail = cdf$lower_tail,
        upper_tail = cdf$upper_tail,
        tied_historical_values = cdf$tied_sample_values,
        tied_observed_values = length(observed) -
            length(unique(observed))
    )
}
# }}}

# Map a mixed precipitation distribution: values at or below the trace
# threshold are randomized uniformly across the historical dry-day mass, while
# positive amounts use empirical conditional CDFs on both sides of the hurdle.
# qm__map_precipitation {{{
qm__map_precipitation <- function(
    historical,
    observed,
    future,
    uniform,
    dry_threshold
) {
    historical_dry <- historical <= dry_threshold
    observed_dry <- observed <= dry_threshold
    future_dry <- future <= dry_threshold
    historical_dry_probability <- mean(historical_dry)
    observed_dry_probability <- mean(observed_dry)
    historical_positive <- historical[!historical_dry]
    observed_positive <- observed[!observed_dry]

    probability <- numeric(length(future))
    probability[future_dry] <- (uniform[future_dry] *
        historical_dry_probability)
    positive <- !future_dry
    lower_tail <- upper_tail <- rep.int(FALSE, length(future))
    tied_historical <- 0L
    if (any(positive)) {
        if (!length(historical_positive)) {
            cli::cli_abort(
                "A precipitation window has positive future values but no positive historical-model calibration values."
            )
        }
        positive_cdf <- quantile__empirical_cdf(
            historical_positive,
            future[positive]
        )
        probability[positive] <- historical_dry_probability +
            (1 - historical_dry_probability) *
                positive_cdf$probability
        lower_tail[positive] <- positive_cdf$lower_tail
        upper_tail[positive] <- positive_cdf$upper_tail
        tied_historical <- positive_cdf$tied_sample_values
    }

    mapped <- numeric(length(future))
    output_positive <- probability > observed_dry_probability
    if (any(output_positive)) {
        if (!length(observed_positive)) {
            cli::cli_abort(
                "A precipitation window requires positive output amounts but has no positive observed-reference calibration values."
            )
        }
        conditional_probability <- (probability[output_positive] -
            observed_dry_probability) /
            (1 - observed_dry_probability)
        mapped[output_positive] <- quantile__inverse_cdf(
            observed_positive,
            conditional_probability
        )
    }
    list(
        value = mapped,
        probability = probability,
        lower_tail = lower_tail,
        upper_tail = upper_tail,
        tied_historical_values = tied_historical,
        tied_observed_values = length(observed_positive) -
            length(unique(observed_positive)),
        historical_dry_probability = historical_dry_probability,
        observed_dry_probability = observed_dry_probability,
        randomized_dry_values = sum(future_dry)
    )
}
# }}}

# Summarize sample coverage and mapping behavior without storing one additional
# provenance row for every day in a multi-decadal future series.
# qm__diagnostics {{{
qm__diagnostics <- function(
    observed_samples,
    historical_samples,
    probability,
    lower_tail,
    upper_tail,
    tied_historical,
    tied_observed,
    clipped,
    precipitation = NULL
) {
    diagnostics <- list(
        observed_window_samples = signal__sample_summary(observed_samples),
        historical_window_samples = signal__sample_summary(
            historical_samples
        ),
        mapped_probability_range = range(probability),
        lower_tail_values = sum(lower_tail),
        upper_tail_values = sum(upper_tail),
        tied_historical_values = sum(tied_historical),
        tied_observed_values = sum(tied_observed),
        clipped_values = clipped
    )
    if (!is.null(precipitation)) {
        diagnostics$precipitation <- precipitation
    }
    diagnostics
}
# }}}

# Apply the calendar-neutral circular window independently at each future day,
# retaining the future series as the output backbone.
# qm__adjust_values {{{
qm__adjust_values <- function(series, resolved, key, variable) {
    observed <- series$observed_reference
    historical <- series$model_historical
    future <- series$model_future
    n_future <- nrow(future)
    observed_samples <- historical_samples <- integer(n_future)
    probability <- adjusted <- numeric(n_future)
    lower_tail <- upper_tail <- logical(n_future)
    tied_historical <- tied_observed <- integer(n_future)
    effective_seed <- quantile__group_seed(
        resolved$random_seed,
        key,
        variable
    )
    uniform <- quantile__uniform(n_future, effective_seed)
    precipitation_diagnostics <- if (
        identical(
            resolved$distribution_model,
            "precipitation_hurdle"
        )
    ) {
        list(
            historical_dry_probability = numeric(n_future),
            observed_dry_probability = numeric(n_future),
            randomized_dry_values = 0L,
            input_dry_values = sum(
                future[["value"]] <= resolved$dry_threshold
            )
        )
    } else {
        NULL
    }

    for (index in seq_len(n_future)) {
        center <- future[["annual_phase"]][[index]]
        observed_window <- daily__phase_window(
            observed[["annual_phase"]],
            center,
            resolved$seasonal_window_days,
            resolved$target_year_days
        )
        historical_window <- daily__phase_window(
            historical[["annual_phase"]],
            center,
            resolved$seasonal_window_days,
            resolved$target_year_days
        )
        observed_values <- observed[["value"]][observed_window]
        historical_values <- historical[["value"]][historical_window]
        observed_samples[[index]] <- length(observed_values)
        historical_samples[[index]] <- length(historical_values)
        if (
            observed_samples[[index]] < resolved$min_samples ||
                historical_samples[[index]] < resolved$min_samples
        ) {
            cli::cli_abort(
                "Quantile Mapping future row {index} has fewer than {resolved$min_samples} observed or historical calibration values in its circular window."
            )
        }

        mapped <- if (
            identical(
                resolved$distribution_model,
                "precipitation_hurdle"
            )
        ) {
            qm__map_precipitation(
                historical_values,
                observed_values,
                future[["value"]][[index]],
                uniform[[index]],
                resolved$dry_threshold
            )
        } else {
            qm__map_continuous(
                historical_values,
                observed_values,
                future[["value"]][[index]]
            )
        }
        adjusted[[index]] <- mapped$value
        probability[[index]] <- mapped$probability
        lower_tail[[index]] <- mapped$lower_tail
        upper_tail[[index]] <- mapped$upper_tail
        tied_historical[[index]] <- mapped$tied_historical_values
        tied_observed[[index]] <- mapped$tied_observed_values
        if (!is.null(precipitation_diagnostics)) {
            precipitation_diagnostics$historical_dry_probability[[index]] <-
                mapped$historical_dry_probability
            precipitation_diagnostics$observed_dry_probability[[index]] <-
                mapped$observed_dry_probability
            precipitation_diagnostics$randomized_dry_values <-
                precipitation_diagnostics$randomized_dry_values +
                mapped$randomized_dry_values
        }
    }

    bounded_result <- signal__bound_values(adjusted, resolved$bounds)
    bounded <- bounded_result$value
    clipped <- bounded_result$clipped
    if (!is.null(precipitation_diagnostics)) {
        precipitation_diagnostics$historical_dry_probability_range <- range(
            precipitation_diagnostics$historical_dry_probability
        )
        precipitation_diagnostics$observed_dry_probability_range <- range(
            precipitation_diagnostics$observed_dry_probability
        )
        precipitation_diagnostics$historical_dry_probability <- NULL
        precipitation_diagnostics$observed_dry_probability <- NULL
        precipitation_diagnostics$output_dry_values <- sum(
            bounded <= resolved$dry_threshold
        )
        precipitation_diagnostics$dry_threshold <- resolved$dry_threshold
        precipitation_diagnostics$random_seed <- resolved$random_seed
        precipitation_diagnostics$effective_seed <- effective_seed
        precipitation_diagnostics$random_generator <-
            "park_miller_16807"
    }
    list(
        value = bounded,
        diagnostics = qm__diagnostics(
            observed_samples,
            historical_samples,
            probability,
            lower_tail,
            upper_tail,
            tied_historical,
            tied_observed,
            clipped,
            precipitation_diagnostics
        )
    )
}
# }}}

# Execute empirical Quantile Mapping for one aligned univariate signal group
# and return the common DailyAdjustedSeries contract.
# qm__apply_group {{{
qm__apply_group <- function(inputs, settings, key) {
    resolved <- qm__settings(settings)
    variable <- names(settings)[[1L]]
    is_hurdle <- identical(
        resolved$distribution_model,
        "precipitation_hurdle"
    )
    series <- signal__validate_three_role_inputs(
        inputs,
        variable = variable,
        method = "Quantile Mapping",
        min_value = if (is_hurdle) 0 else -Inf,
        min_value_message = if (is_hurdle) {
            "Precipitation-hurdle Quantile Mapping requires non-negative input values."
        } else {
            NULL
        }
    )
    mapped <- qm__adjust_values(series, resolved, key, variable)
    future <- series$model_future
    future[["value"]] <- mapped$value

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = "quantile_mapping",
        settings = resolved,
        provenance = list(
            method = "quantile_mapping",
            mapping_type = "nonparametric",
            detrending = "none",
            empirical_conventions = list(
                cdf = "linear_interpolation",
                inverse_cdf = "linear_type_7",
                ties = "average_rank",
                tails = "clamp"
            ),
            references = QM_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            diagnostics = mapped$diagnostics
        )
    )
}
# }}}

# Construct the reusable Quantile Mapping signal with three explicit daily
# input roles and method-evidence-aware variable alternatives.
# qm__component {{{
qm__component <- function() {
    signal__three_role_component(
        name = "quantile_mapping_daily",
        label = "Daily Quantile Mapping",
        variables = c(QM_PUBLISHED_VARIABLES, QM_EXPERIMENTAL_VARIABLES),
        profiles = qm__profiles(),
        apply_group = qm__apply_group,
        validate_options = qm__settings,
        method_label = "Quantile Mapping",
        output_role = "model_future",
        stochastic = TRUE,
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            references = QM_REFERENCES,
            stochastic_operation = "precipitation_dry_day_randomization",
            mapping_type = "nonparametric",
            detrending = "none",
            empirical_conventions = list(
                cdf = "linear_interpolation",
                inverse_cdf = "linear_type_7",
                ties = "average_rank",
                tails = "clamp"
            )
        )
    )
}
# }}}

# Register Quantile Mapping once so package load and repeated tests share one
# discoverable process-local component.
# qm__register_component {{{
qm__register_component <- function() {
    component__register_builtin(qm__component())
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
