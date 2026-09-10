#' @include method-bws.R
NULL

# Bounded temperature weighted stretch {{{

# Return the paper's additive mean-shift fallback together with the requested
# extrema and a machine-readable reason for later diagnostics.
btws__mean_shift_fallback <- function(
    value,
    mean_delta,
    target_mean,
    target_minimum,
    target_maximum,
    reason
) {
    list(
        value = value + mean_delta,
        target_mean = target_mean,
        target_minimum = target_minimum,
        target_maximum = target_maximum,
        status = "fallback_shift_mean",
        scale = NA_real_,
        m = NA_real_,
        n = NA_real_,
        fallback_reason = reason
    )
}

# Project one 24-hour template using the BTWS equations (7)-(16) published by
# Eames et al. Degenerate, unphysical, or numerically inadmissible days use the
# documented mean shift.
btws__project_temperature_day <- function(
    value,
    mean_delta,
    minimum_delta,
    maximum_delta,
    dtr_status,
    tolerance
) {
    baseline_mean <- mean(value)
    baseline_minimum <- min(value)
    baseline_maximum <- max(value)
    baseline_range <- baseline_maximum - baseline_minimum
    target_mean <- baseline_mean + mean_delta

    if (!identical(dtr_status, "adjusted")) {
        return(btws__mean_shift_fallback(
            value,
            mean_delta,
            target_mean,
            baseline_minimum + mean_delta,
            baseline_maximum + mean_delta,
            "inherited_dtr"
        ))
    }
    if (!is.finite(minimum_delta) || !is.finite(maximum_delta)) {
        return(btws__mean_shift_fallback(
            value,
            mean_delta,
            target_mean,
            baseline_minimum + mean_delta,
            baseline_maximum + mean_delta,
            "missing_extrema"
        ))
    }

    target_minimum <- baseline_minimum + minimum_delta
    target_maximum <- baseline_maximum + maximum_delta
    target_range <- target_maximum - target_minimum
    if (target_range < -tolerance ||
        target_mean < target_minimum - tolerance ||
        target_mean > target_maximum + tolerance) {
        return(btws__mean_shift_fallback(
            value,
            mean_delta,
            target_mean,
            target_minimum,
            target_maximum,
            "infeasible_targets"
        ))
    }

    # A collapsed target range has one exact solution even though the published
    # normalized equations would divide by zero.
    if (target_range <= tolerance) {
        return(list(
            value = rep.int(target_mean, length(value)),
            target_mean = target_mean,
            target_minimum = target_minimum,
            target_maximum = target_maximum,
            status = "projected_btws_collapsed_range",
            scale = NA_real_,
            m = NA_real_,
            n = NA_real_,
            fallback_reason = NA_character_
        ))
    }
    if (baseline_range <= tolerance) {
        return(btws__mean_shift_fallback(
            value,
            mean_delta,
            target_mean,
            target_minimum,
            target_maximum,
            "flat_template"
        ))
    }

    normalized <- (value - baseline_minimum) / baseline_range
    normalized <- pmin(1, pmax(0, normalized))
    normalized_target_mean <- (
        target_mean - target_minimum
    ) / target_range
    shape_tolerance <- tolerance / max(target_range, 1)
    if (normalized_target_mean < -shape_tolerance ||
        normalized_target_mean > 1 + shape_tolerance) {
        return(btws__mean_shift_fallback(
            value,
            mean_delta,
            target_mean,
            target_minimum,
            target_maximum,
            "infeasible_normalized_mean"
        ))
    }
    normalized_target_mean <- pmin(1, pmax(0, normalized_target_mean))

    shape <- bws__bounded_normalized_projection(
        normalized,
        normalized_target_mean,
        shape_tolerance
    )
    if (is.null(shape$value)) {
        return(btws__mean_shift_fallback(
            value,
            mean_delta,
            target_mean,
            target_minimum,
            target_maximum,
            shape$reason
        ))
    }
    projected <- target_minimum + target_range * shape$value

    # Successful BTWS days must close all three requested daily statistics.
    closure_error <- max(
        abs(mean(projected) - target_mean),
        abs(min(projected) - target_minimum),
        abs(max(projected) - target_maximum)
    )
    if (!is.finite(closure_error) ||
        closure_error > max(tolerance, 1e-9)) {
        return(btws__mean_shift_fallback(
            value,
            mean_delta,
            target_mean,
            target_minimum,
            target_maximum,
            "numerical_statistic_closure"
        ))
    }

    list(
        value = projected,
        target_mean = target_mean,
        target_minimum = target_minimum,
        target_maximum = target_maximum,
        status = sub("^projected_bws", "projected_btws", shape$status),
        scale = shape$scale,
        m = shape$m,
        n = shape$n,
        fallback_reason = NA_character_
    )
}

# Apply the shared grouped daily projection and boundary diagnostics with the
# BTWS day kernel selected explicitly.
btws__project_temperature <- function(
    template,
    targets,
    value = "value",
    day = "target_day",
    hour = "hour",
    by = character(),
    tolerance = 1e-8
) {
    daily__project_temperature(
        template = template,
        targets = targets,
        value = value,
        day = day,
        hour = hour,
        by = by,
        tolerance = tolerance,
        method = "btws"
    )
}

# }}}
