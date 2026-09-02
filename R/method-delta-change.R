#' @include signal-adjustment.R
NULL

# Delta Change defaults follow the published monthly additive-temperature and
# multiplicative-precipitation change-factor formulation.
BIAS_DELTA_CHANGE_REFERENCES <- c(
    "https://doi.org/10.5194/hess-16-4343-2012"
)

# Return one explicit diagnostic string when Delta Change fails to produce its
# package-native observed-reference result.
bias__validate_delta_change_result <- function(value, inputs, key) {
    signal__validate_adjusted_result(
        value,
        DailyAdjustedSeries,
        "DailyAdjustedSeries",
        "observed_reference",
        "Delta Change"
    )
}
# Define published Delta Change defaults for additive temperature changes and
# multiplicative precipitation changes on the observed-reference backbone.
bias__delta_change_profiles <- function() {
    temperature_settings <- list(
        grouping = "calendar_month",
        statistic = "mean",
        transformation = "additive",
        bounds = c(-Inf, Inf),
        zero_tolerance = 0
    )
    precipitation_settings <- list(
        grouping = "calendar_month",
        statistic = "mean",
        transformation = "multiplicative",
        bounds = c(0, Inf),
        zero_tolerance = sqrt(.Machine$double.eps)
    )
    temperature <- lapply(c("tas", "tasmin", "tasmax"), function(variable) {
        signal__variable_profile(
            variable,
            settings = temperature_settings,
            evidence = "published",
            references = BIAS_DELTA_CHANGE_REFERENCES,
            metadata = list(
                method = "delta_change",
                output_role = "observed_reference"
            )
        )
    })
    precipitation <- signal__variable_profile(
        "pr",
        settings = precipitation_settings,
        evidence = "published",
        references = BIAS_DELTA_CHANGE_REFERENCES,
        metadata = list(
            method = "delta_change",
            output_role = "observed_reference"
        )
    )
    c(temperature, list(precipitation))
}

# Apply published monthly Delta Change equations to the observed daily series
# and retain that series as the typed temporal backbone of the result.
bias__delta_change_apply_group <- function(inputs, settings, key) {
    method <- "Delta Change"
    resolved <- bias__mean_change_settings(settings, method)
    variable <- names(settings)[[1L]]
    series <- bias__mean_change_inputs(
        inputs,
        variable,
        resolved$transformation,
        method
    )
    monthly <- bias__mean_change_monthly_means(
        series,
        "observed_reference",
        method
    )

    if (identical(resolved$transformation, "additive")) {
        # For temperature, Delta_m = mean(future_m) - mean(historical_m)
        # transfers the modeled mean change without replacing observed
        # day-to-day anomalies.
        monthly$change <- (
            monthly$future_mean - monthly$historical_mean
        )
    } else {
        denominator_zero <- (
            abs(monthly$historical_mean) <= resolved$zero_tolerance
        )
        if (any(denominator_zero)) {
            cli::cli_abort(
                "Multiplicative Delta Change is undefined because the historical monthly mean is zero for month(s) {.val {monthly$cf_month[denominator_zero]}}."
            )
        }
        # For precipitation, R_m = mean(future_m) / mean(historical_m)
        # scales observed wet-day magnitudes while preserving the observed
        # zero/non-zero occurrence sequence.
        monthly$change <- (
            monthly$future_mean / monthly$historical_mean
        )
    }

    observed <- series$observed_reference
    change <- monthly$change[
        match(observed[["cf_month"]], monthly$cf_month)
    ]
    if (identical(resolved$transformation, "additive")) {
        # The additive output equation is x'_obs,d = x_obs,d + Delta_m(d).
        adjusted <- observed[["value"]] + change
    } else {
        # The multiplicative output equation is x'_obs,d = x_obs,d * R_m(d).
        adjusted <- observed[["value"]] * change
    }
    bounded_result <- signal__bound_values(adjusted, resolved$bounds)
    bounded <- bounded_result$value
    clipped <- bounded_result$clipped
    observed[["value"]] <- bounded

    bias__daily_adjusted_series(
        observed,
        output_role = "observed_reference",
        transformation = resolved$transformation,
        settings = resolved,
        provenance = list(
            method = "delta_change",
            references = BIAS_DELTA_CHANGE_REFERENCES,
            group_key = key,
            monthly_changes = monthly,
            clipped_values = clipped
        )
    )
}

# Construct the package-native Delta Change signal with explicit three-role
# inputs and an observed-reference daily output.
bias__delta_change_component <- function() {
    alternatives <- as.list(c("tas", "tasmin", "tasmax", "pr"))
    requirements <- signal__three_role_requirements(
        alternatives,
        frequencies = "day"
    )
    signal__component(
        name = "delta_change_daily",
        label = "Daily Delta Change",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = FALSE,
        profiles = bias__delta_change_profiles(),
        apply_group = bias__delta_change_apply_group,
        operations = list(
            validate_result = bias__validate_delta_change_result
        ),
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            references = BIAS_DELTA_CHANGE_REFERENCES
        )
    )
}

# Register Delta Change once so it is discoverable alongside Linear Scaling
# through the process-local component registry.
bias__register_delta_change_component <- function() {
    component__register_builtin(bias__delta_change_component())
    invisible(NULL)
}
