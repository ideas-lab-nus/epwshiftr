# Bounded weighted stretch {{{

# Evaluate the Eames et al. transfer function from equation (7). Both BWS and
# BTWS use this kernel, so the mathematical definition has one implementation.
bws__transfer_weight <- function(normalized, m = 1, n = 1) {
    normalized <- as.numeric(normalized)
    weight <- numeric(length(normalized))
    interior <- normalized > 0 & normalized < 1
    weight[interior] <- normalized[interior] ^ m *
        (1 - normalized[interior]) ^ n
    weight
}

# Apply equation (8) for one exponent pair. Returning NULL records that the
# input contains no interior transfer mass and therefore cannot change its mean
# while retaining the lower and upper endpoints.
bws__project_normalized <- function(normalized, scale, m, n) {
    weight <- bws__transfer_weight(normalized, m, n)
    mean_weight <- mean(weight)
    if (!is.finite(mean_weight) ||
        mean_weight <= .Machine$double.eps) {
        return(NULL)
    }

    normalized + scale * mean(normalized) * weight / mean_weight
}

# Retain the largest admissible exponent when the symmetric equation (7)
# projection would cross a bound. This is the directed m/n reduction described
# by Eames et al.; deterministic bisection supplies the unpublished solver.
bws__bounded_normalized_projection <- function(
    normalized,
    target_mean,
    tolerance
) {
    baseline_mean <- mean(normalized)
    if (!is.finite(baseline_mean) ||
        baseline_mean <= .Machine$double.eps) {
        return(list(reason = "zero_normalized_baseline_mean"))
    }

    scale <- target_mean / baseline_mean - 1
    if (abs(scale) <= tolerance) {
        return(list(
            value = normalized,
            scale = scale,
            m = 1,
            n = 1,
            status = "projected_bws"
        ))
    }

    project <- function(m, n) {
        bws__project_normalized(normalized, scale, m, n)
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
            status = "projected_bws"
        ))
    }

    # Positive stretches move weight away from the upper endpoint by reducing
    # m; negative stretches move it away from the lower endpoint by reducing n.
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

    lower <- 0
    upper <- 1
    exponent_tolerance <- max(tolerance, sqrt(.Machine$double.eps))
    while (upper - lower > exponent_tolerance) {
        midpoint <- (lower + upper) / 2
        if (identical(midpoint, lower) || identical(midpoint, upper)) {
            break
        }
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
            "projected_bws_adjusted_m"
        } else {
            "projected_bws_adjusted_n"
        }
    )
}

# Project one bounded series to a requested mean while preserving every value
# at the declared lower or upper bound. Infeasible signals are explicit errors
# because silently clipping them would lose either the bound or climate signal.
bws__project <- function(
    value,
    target_mean,
    lower,
    upper,
    tolerance = 1e-8
) {
    value <- as.numeric(value)
    checkmate::assert_number(target_mean, finite = TRUE)
    checkmate::assert_number(lower, finite = TRUE)
    checkmate::assert_number(upper, lower = lower, finite = TRUE)
    checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
    if (!length(value) || any(!is.finite(value))) {
        cli::cli_abort("BWS input must contain finite values.")
    }
    if (upper < lower) {
        cli::cli_abort("BWS requires an upper bound at least as large as its lower bound.")
    }
    if (any(value < lower - tolerance | value > upper + tolerance)) {
        cli::cli_abort("BWS input contains values outside its declared bounds.")
    }
    if (target_mean < lower - tolerance || target_mean > upper + tolerance) {
        cli::cli_abort(
            "BWS target mean lies outside its declared bounds.",
            class = "epwshiftr_bws_infeasible_error"
        )
    }

    baseline_mean <- mean(value)
    # A collapsed physical range has one admissible state. This matters for an
    # unchanged all-zero radiation month at sites with polar night.
    if (abs(upper - lower) <= tolerance) {
        if (abs(target_mean - lower) > tolerance ||
            any(abs(value - lower) > tolerance)) {
            cli::cli_abort(
                "BWS cannot change a series whose lower and upper bounds coincide.",
                class = "epwshiftr_bws_infeasible_error"
            )
        }
        return(list(
            value = value,
            baseline_mean = baseline_mean,
            target_mean = target_mean,
            projected_mean = baseline_mean,
            lower = lower,
            upper = upper,
            scale = 0,
            m = 1,
            n = 1,
            status = "projected_bws_collapsed_range",
            closure_error = baseline_mean - target_mean
        ))
    }
    if (abs(target_mean - baseline_mean) <= tolerance) {
        return(list(
            value = value,
            baseline_mean = baseline_mean,
            target_mean = target_mean,
            projected_mean = baseline_mean,
            lower = lower,
            upper = upper,
            scale = 0,
            m = 1,
            n = 1,
            status = "projected_bws",
            closure_error = baseline_mean - target_mean
        ))
    }

    span <- upper - lower
    normalized <- (pmin(upper, pmax(lower, value)) - lower) / span
    normalized_target <- (target_mean - lower) / span
    shape <- bws__bounded_normalized_projection(
        normalized,
        normalized_target,
        tolerance / span
    )
    if (is.null(shape$value)) {
        cli::cli_abort(
            "BWS could not preserve the requested mean and both bounds: {shape$reason}.",
            class = "epwshiftr_bws_infeasible_error"
        )
    }
    projected <- lower + span * shape$value
    closure_error <- mean(projected) - target_mean
    if (!is.finite(closure_error) ||
        abs(closure_error) > max(tolerance, 1e-9)) {
        cli::cli_abort(
            "BWS failed numerical mean closure.",
            class = "epwshiftr_bws_infeasible_error"
        )
    }

    list(
        value = projected,
        baseline_mean = baseline_mean,
        target_mean = target_mean,
        projected_mean = mean(projected),
        lower = lower,
        upper = upper,
        scale = shape$scale,
        m = shape$m,
        n = shape$n,
        status = shape$status,
        closure_error = closure_error
    )
}

# Convert a continuous bounded projection to the integer lattice required by
# EPW sky-cover fields while retaining the closest attainable aggregate mean.
bws__round_to_mean <- function(value, target_mean, lower, upper) {
    value <- pmin(upper, pmax(lower, as.numeric(value)))
    checkmate::assert_number(target_mean, finite = TRUE)
    checkmate::assert_number(lower, finite = TRUE)
    checkmate::assert_integerish(lower, len = 1L)
    checkmate::assert_number(upper, finite = TRUE)
    checkmate::assert_integerish(upper, len = 1L)
    if (!length(value) || any(!is.finite(value))) {
        cli::cli_abort("Bounded integer projection requires finite values.")
    }

    rounded_target_sum <- as.integer(round(target_mean * length(value)))
    rounded_target_sum <- min(
        as.integer(upper * length(value)),
        max(as.integer(lower * length(value)), rounded_target_sum)
    )
    result <- as.integer(floor(value))
    increment <- rounded_target_sum - sum(result)
    if (increment > 0L) {
        eligible <- which(result < upper)
        order <- eligible[order(
            value[eligible] - result[eligible],
            decreasing = TRUE,
            method = "radix"
        )]
        if (increment > length(order)) {
            cli::cli_abort("Bounded integer projection cannot attain its target sum.")
        }
        result[order[seq_len(increment)]] <-
            result[order[seq_len(increment)]] + 1L
    }
    result
}

# Apply the generic BWS projection independently to calendar-month groups and
# retain the resolved equation parameters for method-specific diagnostics.
# Integer projection is optional so the same helper can support both continuous
# bounded variables and discrete EPW fields such as sky cover.
bws__project_monthly <- function(
    value,
    month,
    target_mean,
    upper,
    variable_id,
    integer = FALSE,
    tolerance = 1e-8
) {
    value <- as.numeric(value)
    month <- as.integer(month)
    target_mean <- as.numeric(target_mean)
    upper <- as.numeric(upper)
    checkmate::assert_string(variable_id, min.chars = 1L)
    checkmate::assert_flag(integer)
    checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
    if (length(value) != length(month) ||
        !identical(sort(unique(month)), seq_len(12L)) ||
        length(target_mean) != 12L || length(upper) != 12L) {
        cli::cli_abort(
            "Monthly BWS projection requires hourly values and 12 monthly targets and bounds."
        )
    }

    projected <- numeric(length(value))
    factors <- vector("list", 12L)
    for (calendar_month in seq_len(12L)) {
        index <- which(month == calendar_month)
        result <- bws__project(
            value[index],
            target_mean = target_mean[[calendar_month]],
            lower = 0,
            upper = upper[[calendar_month]],
            tolerance = tolerance
        )
        continuous_mean <- result$projected_mean
        output <- if (isTRUE(integer)) {
            bws__round_to_mean(
                result$value,
                result$target_mean,
                lower = 0L,
                upper = as.integer(round(result$upper))
            )
        } else {
            result$value
        }
        projected[index] <- output
        factors[[calendar_month]] <- data.table::data.table(
            variable_id = variable_id,
            month = calendar_month,
            baseline_mean = result$baseline_mean,
            target_mean = result$target_mean,
            continuous_mean = continuous_mean,
            projected_mean = mean(output),
            lower_bound = result$lower,
            upper_bound = result$upper,
            scale = result$scale,
            m = result$m,
            n = result$n,
            status = result$status,
            closure_error = mean(output) - result$target_mean
        )
    }
    list(
        value = if (isTRUE(integer)) as.integer(projected) else projected,
        factors = data.table::rbindlist(factors)
    )
}

# }}}
