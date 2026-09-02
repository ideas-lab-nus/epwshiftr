#' @include signal-adjustment.R
NULL

# Linear Scaling defaults cite the review in which the monthly additive
# temperature and multiplicative precipitation transformations are defined.
BIAS_LINEAR_SCALING_REFERENCES <- c(
    "https://doi.org/10.1016/j.jhydrol.2012.05.052"
)

# Return one explicit diagnostic string when Linear Scaling fails to produce
# its package-native future-model result.
bias__validate_linear_scaling_result <- function(value, inputs, key) {
    signal__validate_adjusted_result(
        value,
        DailyAdjustedSeries,
        "DailyAdjustedSeries",
        "model_future",
        "Linear Scaling"
    )
}
# Define the published monthly-mean defaults separately for temperature and
# precipitation while retaining their evidence and source in signal profiles.
bias__linear_scaling_profiles <- function() {
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
            references = BIAS_LINEAR_SCALING_REFERENCES,
            metadata = list(
                method = "linear_scaling",
                output_role = "model_future"
            )
        )
    })
    precipitation <- signal__variable_profile(
        "pr",
        settings = precipitation_settings,
        evidence = "published",
        references = BIAS_LINEAR_SCALING_REFERENCES,
        metadata = list(
            method = "linear_scaling",
            output_role = "model_future"
        )
    )
    c(temperature, list(precipitation))
}
# Apply the published monthly additive or multiplicative Linear Scaling
# equation and return a typed daily model-future series.
bias__linear_scaling_apply_group <- function(inputs, settings, key) {
    method <- "Linear Scaling"
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
        "model_future",
        method
    )

    if (identical(resolved$transformation, "additive")) {
        # Temperature uses the monthly observed-minus-historical mean bias as
        # an additive correction on every future daily value in that month.
        monthly$correction <- (
            monthly$observed_mean - monthly$historical_mean
        )
    } else {
        denominator_zero <- (
            abs(monthly$historical_mean) <= resolved$zero_tolerance
        )
        if (any(denominator_zero)) {
            cli::cli_abort(
                "Multiplicative Linear Scaling is undefined because the historical monthly mean is zero for month(s) {.val {monthly$cf_month[denominator_zero]}}."
            )
        }
        # Precipitation uses the monthly observed-to-historical mean ratio as
        # a multiplicative correction on future daily values.
        monthly$correction <- (
            monthly$observed_mean / monthly$historical_mean
        )
    }

    future <- series$model_future
    correction <- monthly$correction[
        match(future[["cf_month"]], monthly$cf_month)
    ]
    if (identical(resolved$transformation, "additive")) {
        adjusted <- future[["value"]] + correction
    } else {
        adjusted <- future[["value"]] * correction
    }
    bounded_result <- signal__bound_values(adjusted, resolved$bounds)
    bounded <- bounded_result$value
    clipped <- bounded_result$clipped
    future[["value"]] <- bounded

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = resolved$transformation,
        settings = resolved,
        provenance = list(
            method = "linear_scaling",
            references = BIAS_LINEAR_SCALING_REFERENCES,
            group_key = key,
            monthly_corrections = monthly,
            clipped_values = clipped
        )
    )
}

# Construct the package-native Linear Scaling signal component with three
# explicit input roles and alternative supported univariate variables.
bias__linear_scaling_component <- function() {
    alternatives <- as.list(c("tas", "tasmin", "tasmax", "pr"))
    requirements <- signal__three_role_requirements(
        alternatives,
        frequencies = "day"
    )
    signal__component(
        name = "linear_scaling_daily",
        label = "Daily Linear Scaling",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = FALSE,
        profiles = bias__linear_scaling_profiles(),
        apply_group = bias__linear_scaling_apply_group,
        operations = list(
            validate_result = bias__validate_linear_scaling_result
        ),
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            references = BIAS_LINEAR_SCALING_REFERENCES
        )
    )
}

# Register the Linear Scaling signal component once so it is discoverable
# through the same process-local component registry as complete recipes.
bias__register_linear_scaling_component <- function() {
    component__register_builtin(bias__linear_scaling_component())
    invisible(NULL)
}
