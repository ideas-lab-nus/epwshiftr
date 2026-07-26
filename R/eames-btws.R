# Eames bounded temperature weighted stretch {{{

# Evaluate the Eames transfer function g = x^m (1 - x)^n while keeping the
# normalized daily minimum and maximum fixed at zero and one.
btws__transfer_weight <- function(normalized, m, n) {
    weight <- numeric(length(normalized))
    interior <- normalized > 0 & normalized < 1
    weight[interior] <- normalized[interior] ^ m *
        (1 - normalized[interior]) ^ n
    weight
}

# Apply equations (9)-(12) on the normalized daily profile for one selected
# exponent pair. NULL records that the transfer has no usable interior mass.
btws__normalized_projection <- function(normalized, scale, m, n) {
    weight <- btws__transfer_weight(normalized, m, n)
    mean_weight <- mean(weight)
    if (!is.finite(mean_weight) ||
        mean_weight <= .Machine$double.eps) {
        return(NULL)
    }

    normalized + scale * mean(normalized) * weight / mean_weight
}

# Select the largest admissible exponent under the paper's directed reduction:
# reduce m for positive stretches and n for negative stretches. Eames et al. do
# not publish solver code, so a deterministic bisection on [0, 1] is used.
btws__bounded_normalized_projection <- function(
    normalized,
    target_mean,
    tolerance
) {
    baseline_mean <- mean(normalized)
    if (!is.finite(baseline_mean) ||
        baseline_mean <= .Machine$double.eps) {
        return(list(reason = "zero_normalized_baseline_mean"))
    }

    # Equation (16) is S = q' / q - 1, where q and q' are normalized means.
    scale <- target_mean / baseline_mean - 1
    if (abs(scale) <= tolerance) {
        return(list(
            value = normalized,
            scale = scale,
            m = 1,
            n = 1,
            status = "projected_btws"
        ))
    }

    project <- function(m, n) {
        btws__normalized_projection(normalized, scale, m, n)
    }
    admissible <- function(candidate) {
        !is.null(candidate) &&
            all(is.finite(candidate)) &&
            min(candidate) >= -tolerance &&
            max(candidate) <= 1 + tolerance
    }

    candidate <- project(1, 1)
    if (admissible(candidate)) {
        return(list(
            value = pmin(1, pmax(0, candidate)),
            scale = scale,
            m = 1,
            n = 1,
            status = "projected_btws"
        ))
    }

    reduce_m <- scale > 0
    lower_candidate <- if (reduce_m) project(0, 1) else project(1, 0)
    if (!admissible(lower_candidate)) {
        return(list(
            reason = if (reduce_m) {
                "no_admissible_m"
            } else {
                "no_admissible_n"
            }
        ))
    }

    # The lower end is admissible and the published default at one is not.
    # Bisection retains the largest exponent that still satisfies both bounds.
    lower <- 0
    upper <- 1
    for (iteration in seq_len(80L)) {
        midpoint <- (lower + upper) / 2
        midpoint_candidate <- if (reduce_m) {
            project(midpoint, 1)
        } else {
            project(1, midpoint)
        }
        if (admissible(midpoint_candidate)) {
            lower <- midpoint
            lower_candidate <- midpoint_candidate
        } else {
            upper <- midpoint
        }
    }

    value <- pmin(1, pmax(0, lower_candidate))
    if (abs(mean(value) - target_mean) > max(tolerance, 1e-10)) {
        return(list(reason = "numerical_mean_closure"))
    }
    list(
        value = value,
        scale = scale,
        m = if (reduce_m) lower else 1,
        n = if (reduce_m) 1 else lower,
        status = if (reduce_m) {
            "projected_btws_adjusted_m"
        } else {
            "projected_btws_adjusted_n"
        }
    )
}

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

# Project one 24-hour template using Eames equations (7)-(16). Degenerate,
# unphysical, or numerically inadmissible days use the documented mean shift.
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

    shape <- btws__bounded_normalized_projection(
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
        status = shape$status,
        scale = shape$scale,
        m = shape$m,
        n = shape$n,
        fallback_reason = NA_character_
    )
}

# Apply the shared grouped daily projection and boundary diagnostics with the
# Eames day kernel selected explicitly.
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
        method = "eames_btws"
    )
}

# }}}
