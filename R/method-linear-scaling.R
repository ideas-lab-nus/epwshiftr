#' @include signal-adjustment.R
NULL

# Linear Scaling defaults cite the review in which the monthly additive
# temperature and multiplicative precipitation transformations are defined.
BIAS_LINEAR_SCALING_REFERENCES <- c(
    "https://doi.org/10.1016/j.jhydrol.2012.05.052"
)

# Validate and normalize Linear Scaling settings through the same method
# contract used immediately before the numerical kernel.
# bias__linear_scaling_settings {{{
bias__linear_scaling_settings <- function(settings) {
    bias__mean_change_settings(settings, "Linear Scaling")
}
# }}}

# Define the published monthly-mean defaults separately for temperature and
# precipitation while retaining their evidence and source in signal profiles.
# bias__linear_scaling_profiles {{{
bias__linear_scaling_profiles <- function() {
    bias__mean_change_profiles(
        method_id = "linear_scaling",
        output_role = "model_future",
        references = BIAS_LINEAR_SCALING_REFERENCES
    )
}
# }}}

# Apply the published monthly additive or multiplicative Linear Scaling
# equation and return a typed daily model-future series.
# bias__linear_scaling_apply_group {{{
bias__linear_scaling_apply_group <- function(inputs, settings, key) {
    bias__mean_change_apply_group(
        inputs = inputs,
        settings = settings,
        key = key,
        method_id = "linear_scaling",
        method_label = "Linear Scaling",
        output_role = "model_future",
        numerator_column = "observed_mean",
        delta_column = "correction",
        provenance_key = "monthly_corrections",
        references = BIAS_LINEAR_SCALING_REFERENCES
    )
}
# }}}

# Construct the package-native Linear Scaling signal component with three
# explicit input roles and alternative supported univariate variables.
# bias__linear_scaling_component {{{
bias__linear_scaling_component <- function() {
    signal__three_role_component(
        name = "linear_scaling_daily",
        label = "Daily Linear Scaling",
        variables = c("tas", "tasmin", "tasmax", "pr"),
        profiles = bias__linear_scaling_profiles(),
        apply_group = bias__linear_scaling_apply_group,
        validate_options = bias__linear_scaling_settings,
        method_label = "Linear Scaling",
        output_role = "model_future",
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            grouping = "calendar_month",
            statistic = "mean",
            references = BIAS_LINEAR_SCALING_REFERENCES
        )
    )
}
# }}}

# Register the Linear Scaling signal component once so it is discoverable
# through the same process-local component registry as complete recipes.
# bias__register_linear_scaling_component {{{
bias__register_linear_scaling_component <- function() {
    component__register_builtin(bias__linear_scaling_component())
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
