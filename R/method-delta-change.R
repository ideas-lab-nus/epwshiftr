#' @include signal-adjustment.R
NULL

# Delta Change defaults follow the published monthly additive-temperature and
# multiplicative-precipitation change-factor formulation.
BIAS_DELTA_CHANGE_REFERENCES <- c(
    "https://doi.org/10.5194/hess-16-4343-2012"
)

# Validate and normalize Delta Change settings through the same method
# contract used immediately before the numerical kernel.
# bias__delta_change_settings {{{
bias__delta_change_settings <- function(settings) {
    bias__mean_change_settings(settings, "Delta Change")
}
# }}}

# Define published Delta Change defaults for additive temperature changes and
# multiplicative precipitation changes on the observed-reference backbone.
# bias__delta_change_profiles {{{
bias__delta_change_profiles <- function() {
    bias__mean_change_profiles(
        method_id = "delta_change",
        output_role = "observed_reference",
        references = BIAS_DELTA_CHANGE_REFERENCES
    )
}
# }}}

# Apply published monthly Delta Change equations to the observed daily series
# and retain that series as the typed temporal backbone of the result.
# bias__delta_change_apply_group {{{
bias__delta_change_apply_group <- function(inputs, settings, key) {
    bias__mean_change_apply_group(
        inputs = inputs,
        settings = settings,
        key = key,
        method_id = "delta_change",
        method_label = "Delta Change",
        output_role = "observed_reference",
        numerator_column = "future_mean",
        delta_column = "change",
        provenance_key = "monthly_changes",
        references = BIAS_DELTA_CHANGE_REFERENCES
    )
}
# }}}

# Construct the package-native Delta Change signal with explicit three-role
# inputs and an observed-reference daily output.
# bias__delta_change_component {{{
bias__delta_change_component <- function() {
    signal__three_role_component(
        name = "delta_change_daily",
        label = "Daily Delta Change",
        variables = c("tas", "tasmin", "tasmax", "pr"),
        profiles = bias__delta_change_profiles(),
        apply_group = bias__delta_change_apply_group,
        validate_options = bias__delta_change_settings,
        method_label = "Delta Change",
        output_role = "observed_reference",
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            grouping = "calendar_month",
            statistic = "mean",
            references = BIAS_DELTA_CHANGE_REFERENCES
        )
    )
}
# }}}

# Register Delta Change once so it is discoverable alongside Linear Scaling
# through the process-local component registry.
# bias__register_delta_change_component {{{
bias__register_delta_change_component <- function() {
    component__register_builtin(bias__delta_change_component())
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
