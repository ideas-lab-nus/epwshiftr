#' @include signal-adjustment.R calendar-daily.R distribution-quantile.R
NULL

# Quantile Delta Mapping follows the original precipitation formulation and
# its appendix describing the additive form for interval variables.
QDM_REFERENCES <- c(
    "https://doi.org/10.1175/JCLI-D-14-00754.1"
)

# Cannon et al. treat precipitation below 0.05 mm/day as censored dry values.
# CMIP6 daily precipitation flux uses the equivalent kg m-2 s-1 value.
QDM_PR_DRY_THRESHOLD <- 0.05 / 86400

# The publication directly supports precipitation and the additive
# interval-variable form exemplified by temperature. Other variable defaults
# are retained as explicit package experiments for controlled comparisons.
QDM_PUBLISHED_VARIABLES <- c("pr", "tas")
QDM_EXPERIMENTAL_VARIABLES <- c(
    "hurs",
    "psl",
    "rlds",
    "sfcWind",
    "tasmin",
    "tasmax"
)

# Construct one complete QDM settings record. The 91-day seasonal window
# represents the published three-month pool, while the odd 31-year future
# window gives a symmetric discrete-year implementation of its 30-year window.
qdm__default_settings <- function(
    bounds,
    trend_preservation = c("absolute", "relative"),
    distribution_model = c("continuous", "precipitation_censored"),
    dry_threshold = 0
) {
    trend_preservation <- match.arg(trend_preservation)
    distribution_model <- match.arg(distribution_model)
    list(
        mapping_type = "nonparametric",
        trend_preservation = trend_preservation,
        seasonal_window_days = 91L,
        future_window_years = 31L,
        target_year_days = 365L,
        min_samples = 10L,
        cdf_method = "linear_interpolation",
        inverse_cdf_method = "linear_type_7",
        tie_method = "average_rank",
        tail_policy = "future_window_support",
        bounds = bounds,
        distribution_model = distribution_model,
        dry_threshold = dry_threshold,
        zero_denominator_policy = "error",
        random_seed = 1L
    )
}

# Build method-evidence-aware variable profiles without attributing
# implementation-selected transformations to the QDM publication.
qdm__profiles <- function() {
    settings <- list(
        pr = qdm__default_settings(
            c(0, Inf),
            "relative",
            "precipitation_censored",
            QDM_PR_DRY_THRESHOLD
        ),
        tas = qdm__default_settings(c(-Inf, Inf), "absolute"),
        hurs = qdm__default_settings(c(0, 100), "absolute"),
        psl = qdm__default_settings(c(0, Inf), "absolute"),
        rlds = qdm__default_settings(c(0, Inf), "absolute"),
        sfcWind = qdm__default_settings(c(0, Inf), "absolute"),
        tasmin = qdm__default_settings(c(-Inf, Inf), "absolute"),
        tasmax = qdm__default_settings(c(-Inf, Inf), "absolute")
    )
    variables <- c(QDM_PUBLISHED_VARIABLES, QDM_EXPERIMENTAL_VARIABLES)
    lapply(variables, function(variable) {
        published <- variable %in% QDM_PUBLISHED_VARIABLES
        signal__variable_profile(
            variable,
            settings = settings[[variable]],
            evidence = if (published) "published" else "experimental",
            references = if (published) QDM_REFERENCES else character(),
            metadata = list(
                method = "quantile_delta_mapping",
                output_role = "model_future",
                default_source = if (published) {
                    "method_literature"
                } else {
                    "package_implementation"
                }
            )
        )
    })
}

# Validate every QDM convention at the signal-kernel boundary so overrides
# cannot silently change the published transfer semantics.
qdm__settings <- function(settings) {
    expected <- c(
        "mapping_type",
        "trend_preservation",
        "seasonal_window_days",
        "future_window_years",
        "target_year_days",
        "min_samples",
        "cdf_method",
        "inverse_cdf_method",
        "tie_method",
        "tail_policy",
        "bounds",
        "distribution_model",
        "dry_threshold",
        "zero_denominator_policy",
        "random_seed"
    )
    resolved <- signal__resolve_settings(
        settings,
        expected,
        "Quantile Delta Mapping"
    )
    if (!identical(resolved$mapping_type, "nonparametric")) {
        cli::cli_abort(
            "Quantile Delta Mapping currently supports only nonparametric mapping."
        )
    }
    checkmate::assert_choice(
        resolved$trend_preservation,
        c("absolute", "relative")
    )
    if (!identical(resolved$cdf_method, "linear_interpolation") ||
        !identical(resolved$inverse_cdf_method, "linear_type_7") ||
        !identical(resolved$tie_method, "average_rank") ||
        !identical(resolved$tail_policy, "future_window_support")) {
        cli::cli_abort(
            "Quantile Delta Mapping currently requires linear empirical CDF interpolation, type-7 inverse quantiles, average-rank ties, and future-window endpoint support."
        )
    }
    resolved$seasonal_window_days <- signal__integer_setting(
        resolved$seasonal_window_days,
        "seasonal_window_days",
        lower = 1L
    )
    resolved$future_window_years <- signal__integer_setting(
        resolved$future_window_years,
        "future_window_years",
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
    daily__window_spec(
        resolved$seasonal_window_days,
        resolved$target_year_days
    )
    if (resolved$future_window_years %% 2L != 1L) {
        cli::cli_abort(
            "Quantile Delta Mapping requires an odd `future_window_years` for a symmetric centered window."
        )
    }
    signal__ordered_bounds(
        resolved$bounds,
        "Quantile Delta Mapping bounds must be ordered from lower to upper."
    )
    checkmate::assert_choice(
        resolved$distribution_model,
        c("continuous", "precipitation_censored")
    )
    checkmate::assert_number(
        resolved$dry_threshold,
        lower = 0,
        finite = TRUE
    )
    if (identical(
        resolved$distribution_model,
        "precipitation_censored"
    ) && resolved$dry_threshold <= 0) {
        cli::cli_abort(
            "Censored-precipitation Quantile Delta Mapping requires a positive `dry_threshold`."
        )
    }
    if (identical(
        resolved$distribution_model,
        "precipitation_censored"
    ) && !identical(resolved$trend_preservation, "relative")) {
        cli::cli_abort(
            "Censored-precipitation Quantile Delta Mapping requires relative trend preservation."
        )
    }
    if (!identical(resolved$zero_denominator_policy, "error")) {
        cli::cli_abort(
            "Quantile Delta Mapping currently requires `zero_denominator_policy = \"error\"`."
        )
    }
    resolved$random_seed <- signal__random_seed(resolved$random_seed)
    resolved
}

# Validate the three role-addressable daily inputs while preserving their
# independent CF calendars and date coordinates.
qdm__inputs <- function(inputs, variable, distribution_model) {
    roles <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    if (!identical(sort(names(inputs)), sort(roles))) {
        cli::cli_abort(
            "Quantile Delta Mapping requires observed, historical-model, and future-model role payloads."
        )
    }
    series <- lapply(roles, function(role) {
        bias__daily_table(inputs[[role]], role)
    })
    names(series) <- roles
    for (role in roles) {
        role_variables <- unique(series[[role]][["variable_id"]])
        if (!identical(role_variables, variable)) {
            cli::cli_abort(
                "Quantile Delta Mapping role {.val {role}} must contain only variable {.val {variable}}."
            )
        }
        if (length(unique(series[[role]][["cf_calendar"]])) != 1L) {
            cli::cli_abort(
                "Quantile Delta Mapping role {.val {role}} must contain one native calendar per signal group."
            )
        }
    }
    units <- vapply(
        series,
        function(data) unique(data[["units"]]),
        character(1L)
    )
    if (length(unique(units)) != 1L) {
        cli::cli_abort(
            "Quantile Delta Mapping inputs for {.val {variable}} must use identical units."
        )
    }
    if (identical(distribution_model, "precipitation_censored") &&
        any(vapply(
            series,
            function(data) any(data[["value"]] < 0),
            logical(1L)
        ))) {
        cli::cli_abort(
            "Censored-precipitation Quantile Delta Mapping requires non-negative input values."
        )
    }
    series
}

# Select a symmetric discrete-year window around one projected row. Seasonal
# filtering is applied separately on the calendar-neutral annual phase.
qdm__future_year_window <- function(year, center, width) {
    half_width <- width %/% 2L
    year >= center - half_width & year <= center + half_width
}

# Preprocess each role once so a source row receives one reproducible censored
# value even when it contributes to several overlapping QDM windows.
qdm__prepared_values <- function(series, resolved, key, variable) {
    if (!identical(
        resolved$distribution_model,
        "precipitation_censored"
    )) {
        return(list(
            values = lapply(series, `[[`, "value"),
            precipitation = NULL
        ))
    }

    randomized <- signal__randomize_threshold_values(
        series,
        resolved$random_seed,
        key,
        variable,
        resolved$dry_threshold,
        inclusive = TRUE
    )
    list(
        values = randomized$values,
        precipitation = list(
            input_censored_values = randomized$counts,
            random_seed = resolved$random_seed,
            effective_seeds = randomized$seeds,
            random_generator = "park_miller_16807",
            dry_threshold = resolved$dry_threshold
        )
    )
}

# Apply the Cannon et al. QDM equations at one future value. The future CDF
# supplies p, then observed and historical quantiles at p define the absolute
# delta or relative ratio transferred to the observed quantile.
qdm__map_value <- function(
    observed,
    historical,
    future_sample,
    future_value,
    trend_preservation
) {
    future_cdf <- quantile__empirical_cdf(
        future_sample,
        future_value
    )
    probability <- future_cdf$probability
    historical_quantile <- quantile__inverse_cdf(
        historical,
        probability
    )
    observed_quantile <- quantile__inverse_cdf(
        observed,
        probability
    )
    if (identical(trend_preservation, "absolute")) {
        change <- future_value - historical_quantile
        adjusted <- observed_quantile + change
    } else {
        if (historical_quantile == 0) {
            cli::cli_abort(
                "Relative Quantile Delta Mapping encountered a zero historical-model quantile."
            )
        }
        change <- future_value / historical_quantile
        adjusted <- observed_quantile * change
    }
    list(
        value = adjusted,
        probability = probability,
        change = change,
        future_lower_tail = future_cdf$lower_tail,
        future_upper_tail = future_cdf$upper_tail,
        tied_observed_values = length(observed) -
            length(unique(observed)),
        tied_historical_values = length(historical) -
            length(unique(historical)),
        tied_future_values = future_cdf$tied_sample_values
    )
}

# Summarize window coverage and transfer behavior without retaining one
# provenance record for every adjusted day.
qdm__diagnostics <- function(
    observed_samples,
    historical_samples,
    future_samples,
    future_year_counts,
    probability,
    change,
    future_lower_tail,
    future_upper_tail,
    tied_observed,
    tied_historical,
    tied_future,
    clipped,
    precipitation
) {
    diagnostics <- list(
        observed_window_samples = c(
            minimum = min(observed_samples),
            median = stats::median(observed_samples),
            maximum = max(observed_samples)
        ),
        historical_window_samples = c(
            minimum = min(historical_samples),
            median = stats::median(historical_samples),
            maximum = max(historical_samples)
        ),
        future_window_samples = c(
            minimum = min(future_samples),
            median = stats::median(future_samples),
            maximum = max(future_samples)
        ),
        future_window_years = c(
            minimum = min(future_year_counts),
            median = stats::median(future_year_counts),
            maximum = max(future_year_counts)
        ),
        mapped_probability_range = range(probability),
        transferred_change_range = range(change),
        future_lower_tail_values = sum(future_lower_tail),
        future_upper_tail_values = sum(future_upper_tail),
        tied_observed_values = sum(tied_observed),
        tied_historical_values = sum(tied_historical),
        tied_future_values = sum(tied_future),
        zero_historical_quantiles = 0L,
        clipped_values = clipped
    )
    if (!is.null(precipitation)) {
        diagnostics$precipitation <- precipitation
    }
    diagnostics
}

# Apply seasonal and future-period windows independently at each projected day
# while retaining the future-model sequence as the adjusted output backbone.
qdm__adjust_values <- function(series, resolved, key, variable) {
    observed <- series$observed_reference
    historical <- series$model_historical
    future <- series$model_future
    prepared <- qdm__prepared_values(series, resolved, key, variable)
    observed_value <- prepared$values$observed_reference
    historical_value <- prepared$values$model_historical
    future_value <- prepared$values$model_future
    n_future <- nrow(future)
    observed_samples <- historical_samples <- future_samples <-
        future_year_counts <- integer(n_future)
    probability <- change <- adjusted <- numeric(n_future)
    future_lower_tail <- future_upper_tail <- logical(n_future)
    tied_observed <- tied_historical <- tied_future <- integer(n_future)

    for (index in seq_len(n_future)) {
        center_phase <- future[["annual_phase"]][[index]]
        observed_window <- daily__phase_window(
            observed[["annual_phase"]],
            center_phase,
            resolved$seasonal_window_days,
            resolved$target_year_days
        )
        historical_window <- daily__phase_window(
            historical[["annual_phase"]],
            center_phase,
            resolved$seasonal_window_days,
            resolved$target_year_days
        )
        future_season <- daily__phase_window(
            future[["annual_phase"]],
            center_phase,
            resolved$seasonal_window_days,
            resolved$target_year_days
        )
        future_year <- qdm__future_year_window(
            future[["cf_year"]],
            future[["cf_year"]][[index]],
            resolved$future_window_years
        )
        future_window <- future_season & future_year
        observed_values <- observed_value[observed_window]
        historical_values <- historical_value[historical_window]
        future_values <- future_value[future_window]
        observed_samples[[index]] <- length(observed_values)
        historical_samples[[index]] <- length(historical_values)
        future_samples[[index]] <- length(future_values)
        future_year_counts[[index]] <- length(unique(
            future[["cf_year"]][future_window]
        ))
        if (observed_samples[[index]] < resolved$min_samples ||
            historical_samples[[index]] < resolved$min_samples ||
            future_samples[[index]] < resolved$min_samples) {
            cli::cli_abort(
                "Quantile Delta Mapping future row {index} has fewer than {resolved$min_samples} observed, historical, or future values in its seasonal and future-period windows."
            )
        }

        mapped <- qdm__map_value(
            observed_values,
            historical_values,
            future_values,
            future_value[[index]],
            resolved$trend_preservation
        )
        adjusted[[index]] <- mapped$value
        probability[[index]] <- mapped$probability
        change[[index]] <- mapped$change
        future_lower_tail[[index]] <- mapped$future_lower_tail
        future_upper_tail[[index]] <- mapped$future_upper_tail
        tied_observed[[index]] <- mapped$tied_observed_values
        tied_historical[[index]] <- mapped$tied_historical_values
        tied_future[[index]] <- mapped$tied_future_values
    }

    if (!is.null(prepared$precipitation)) {
        recensored <- adjusted <= resolved$dry_threshold
        adjusted[recensored] <- 0
        prepared$precipitation$output_censored_values <- sum(recensored)
    }
    bounded_result <- signal__bound_values(adjusted, resolved$bounds)
    bounded <- bounded_result$value
    clipped <- bounded_result$clipped
    list(
        value = bounded,
        diagnostics = qdm__diagnostics(
            observed_samples,
            historical_samples,
            future_samples,
            future_year_counts,
            probability,
            change,
            future_lower_tail,
            future_upper_tail,
            tied_observed,
            tied_historical,
            tied_future,
            clipped,
            prepared$precipitation
        )
    )
}

# Execute QDM for one aligned univariate signal group and return the common
# DailyAdjustedSeries contract with resolved settings and provenance.
qdm__apply_group <- function(inputs, settings, key) {
    resolved <- qdm__settings(settings)
    variable <- names(settings)[[1L]]
    series <- qdm__inputs(
        inputs,
        variable,
        resolved$distribution_model
    )
    mapped <- qdm__adjust_values(series, resolved, key, variable)
    future <- series$model_future
    future[["value"]] <- mapped$value

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = "quantile_delta_mapping",
        settings = resolved,
        provenance = list(
            method = "quantile_delta_mapping",
            references = QDM_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            diagnostics = mapped$diagnostics
        )
    )
}

# Return one explicit diagnostic string when QDM violates the package-native
# future-model output contract.
qdm__validate_result <- function(value, inputs, key) {
    signal__validate_adjusted_result(
        value,
        DailyAdjustedSeries,
        "DailyAdjustedSeries",
        "model_future",
        "Quantile Delta Mapping"
    )
}

# Construct the reusable QDM signal with explicit daily roles and
# method-evidence-aware variable alternatives.
qdm__component <- function() {
    alternatives <- as.list(c(
        QDM_PUBLISHED_VARIABLES,
        QDM_EXPERIMENTAL_VARIABLES
    ))
    requirements <- signal__three_role_requirements(
        alternatives,
        frequencies = "day"
    )
    signal__component(
        name = "quantile_delta_mapping_daily",
        label = "Daily Quantile Delta Mapping",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = TRUE,
        profiles = qdm__profiles(),
        apply_group = qdm__apply_group,
        operations = list(validate_result = qdm__validate_result),
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            references = QDM_REFERENCES,
            stochastic_operation = "precipitation_censor_randomization",
            empirical_conventions = list(
                cdf = "linear_interpolation",
                inverse_cdf = "linear_type_7",
                ties = "average_rank",
                tails = "future_window_support"
            ),
            window_defaults = list(
                seasonal_days = 91L,
                future_years = 31L,
                future_step_years = 1L
            )
        )
    )
}

# Register QDM once so package load and repeated tests share one discoverable
# process-local component.
qdm__register_component <- function() {
    component__register_builtin(qdm__component())
    invisible(NULL)
}
