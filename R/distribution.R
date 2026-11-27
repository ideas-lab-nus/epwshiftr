# The compact distribution helpers in this module support native statistical
# signals without introducing an external distribution class hierarchy.

# Validate one fitted distribution record before it is used by a CDF or
# inverse-CDF calculation.
# distribution__validate_fit {{{
distribution__validate_fit <- function(fit) {
    if (
        !is.list(fit) ||
            !identical(
                sort(names(fit)),
                sort(c("family", "parameters", "sample_size", "method"))
            )
    ) {
        cli::cli_abort(
            "A parametric distribution fit must contain `family`, `parameters`, `sample_size`, and `method`."
        )
    }
    checkmate::assert_choice(fit$family, c("normal", "gamma", "weibull"))
    checkmate::assert_list(fit$parameters, names = "unique")
    checkmate::assert_count(fit$sample_size, positive = TRUE)
    checkmate::assert_string(fit$method, min.chars = 1L)
    if (identical(fit$family, "normal")) {
        if (!identical(sort(names(fit$parameters)), c("location", "scale"))) {
            cli::cli_abort(
                "A Normal fit requires `location` and `scale` parameters."
            )
        }
    } else {
        parameter_names <- sort(names(fit$parameters))
        valid_names <- identical(parameter_names, c("scale", "shape")) ||
            identical(parameter_names, c("location", "scale", "shape"))
        if (!valid_names) {
            family <- if (identical(fit$family, "gamma")) {
                "Gamma"
            } else {
                "Weibull"
            }
            cli::cli_abort(
                "A {family} fit requires `shape` and `scale` parameters and may provide `location`."
            )
        }
    }
    checkmate::assert_number(fit$parameters$scale, lower = 0)
    if (fit$parameters$scale <= 0) {
        cli::cli_abort("A fitted distribution scale must be positive.")
    }
    if (identical(fit$family, "normal")) {
        checkmate::assert_number(fit$parameters$location, finite = TRUE)
    } else {
        checkmate::assert_number(fit$parameters$shape, lower = 0)
        if (fit$parameters$shape <= 0) {
            family <- if (identical(fit$family, "gamma")) {
                "Gamma"
            } else {
                "Weibull"
            }
            cli::cli_abort("A fitted {family} shape must be positive.")
        }
        if (!is.null(fit$parameters$location)) {
            checkmate::assert_number(
                fit$parameters$location,
                finite = TRUE
            )
        }
    }
    invisible(TRUE)
}
# }}}

# Fit the Normal maximum-likelihood location and scale. The scale uses the
# population denominator n, matching a likelihood fit rather than R's
# sample-standard-deviation denominator n - 1.
# distribution__fit_normal {{{
distribution__fit_normal <- function(values) {
    checkmate::assert_numeric(
        values,
        min.len = 2L,
        finite = TRUE,
        any.missing = FALSE
    )
    location <- mean(values)
    scale <- sqrt(mean((values - location)^2))
    if (!is.finite(scale) || scale <= 0) {
        cli::cli_abort(
            "A Normal distribution cannot be fitted to a constant sample."
        )
    }
    list(
        family = "normal",
        parameters = list(location = location, scale = scale),
        sample_size = length(values),
        method = "maximum_likelihood"
    )
}
# }}}

# Fit a zero-location Gamma distribution by maximum likelihood. For positive
# x, the profile-likelihood shape solves
# log(shape) - digamma(shape) = log(mean(x)) - mean(log(x));
# scale is then mean(x) / shape.
# distribution__fit_gamma {{{
distribution__fit_gamma <- function(
    values,
    tolerance = 1e-10,
    max_iterations = 1000L
) {
    checkmate::assert_numeric(
        values,
        min.len = 2L,
        lower = 0,
        finite = TRUE,
        any.missing = FALSE
    )
    if (any(values <= 0)) {
        cli::cli_abort(
            "A zero-location Gamma fit requires strictly positive values."
        )
    }
    checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
    if (tolerance <= 0) {
        cli::cli_abort("Gamma-fit `tolerance` must be positive.")
    }
    checkmate::assert_integerish(
        max_iterations,
        lower = 1L,
        len = 1L,
        any.missing = FALSE
    )

    mean_value <- mean(values)
    log_gap <- log(mean_value) - mean(log(values))
    if (!is.finite(log_gap) || log_gap <= tolerance) {
        cli::cli_abort(
            "A Gamma distribution cannot be fitted to a constant or numerically degenerate positive sample."
        )
    }

    score <- function(shape) {
        log(shape) - digamma(shape) - log_gap
    }
    lower <- 1e-8
    upper <- max(1, 1 / (2 * log_gap))
    while (score(upper) > 0 && upper < 1e12) {
        upper <- upper * 2
    }
    if (score(upper) > 0) {
        cli::cli_abort(
            "Gamma maximum-likelihood fitting could not bracket a finite shape parameter."
        )
    }
    shape <- stats::uniroot(
        score,
        interval = c(lower, upper),
        tol = tolerance,
        maxiter = as.integer(max_iterations)
    )$root
    scale <- mean_value / shape
    fit <- list(
        family = "gamma",
        parameters = list(shape = shape, scale = scale),
        sample_size = length(values),
        method = "maximum_likelihood_zero_location"
    )
    distribution__validate_fit(fit)
    fit
}
# }}}

# Evaluate the CDF of one validated Normal, Gamma, or Weibull distribution,
# including an optional fixed location for either positive-family fit.
# distribution__cdf {{{
distribution__cdf <- function(fit, values) {
    distribution__validate_fit(fit)
    checkmate::assert_numeric(
        values,
        finite = TRUE,
        any.missing = FALSE
    )
    if (identical(fit$family, "normal")) {
        return(stats::pnorm(
            values,
            mean = fit$parameters$location,
            sd = fit$parameters$scale
        ))
    }
    location <- if (is.null(fit$parameters$location)) {
        0
    } else {
        fit$parameters$location
    }
    shifted <- pmax(values - location, 0)
    if (identical(fit$family, "gamma")) {
        return(stats::pgamma(
            shifted,
            shape = fit$parameters$shape,
            scale = fit$parameters$scale
        ))
    }
    stats::pweibull(
        shifted,
        shape = fit$parameters$shape,
        scale = fit$parameters$scale
    )
}
# }}}

# Evaluate the inverse CDF of one validated Normal, Gamma, or Weibull fit while
# restoring the optional fixed location used by positive-family models.
# distribution__quantile {{{
distribution__quantile <- function(fit, probability) {
    distribution__validate_fit(fit)
    checkmate::assert_numeric(
        probability,
        lower = 0,
        upper = 1,
        finite = TRUE,
        any.missing = FALSE
    )
    if (identical(fit$family, "normal")) {
        return(stats::qnorm(
            probability,
            mean = fit$parameters$location,
            sd = fit$parameters$scale
        ))
    }
    location <- if (is.null(fit$parameters$location)) {
        0
    } else {
        fit$parameters$location
    }
    quantile <- if (identical(fit$family, "gamma")) {
        stats::qgamma(
            probability,
            shape = fit$parameters$shape,
            scale = fit$parameters$scale
        )
    } else {
        stats::qweibull(
            probability,
            shape = fit$parameters$shape,
            scale = fit$parameters$scale
        )
    }
    location + quantile
}
# }}}

# Clamp fitted probabilities away from zero and one so inverse CDFs remain
# finite under the method's declared numerical threshold.
# distribution__clamp_probability {{{
distribution__clamp_probability <- function(probability, epsilon) {
    checkmate::assert_numeric(
        probability,
        lower = 0,
        upper = 1,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_number(
        epsilon,
        lower = 0,
        upper = 0.5,
        finite = TRUE
    )
    if (epsilon <= 0 || epsilon >= 0.5) {
        cli::cli_abort(
            "Probability-clamp `epsilon` must lie strictly between zero and 0.5."
        )
    }
    pmin(pmax(probability, epsilon), 1 - epsilon)
}
# }}}

# Interpolate one ordered vector onto a requested ordered length using a
# normalized rank coordinate. This is the SDM paper's length-normalization
# operation when calibration and projected samples differ.
# distribution__interpolate_ordered {{{
distribution__interpolate_ordered <- function(values, length_out) {
    checkmate::assert_numeric(
        values,
        min.len = 1L,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_count(length_out)
    if (length_out == 0L) {
        return(numeric())
    }
    if (length(values) == length_out) {
        return(as.numeric(values))
    }
    if (length(values) == 1L) {
        return(rep.int(as.numeric(values), length_out))
    }
    stats::approx(
        x = seq(0, 1, length.out = length(values)),
        y = as.numeric(values),
        xout = seq(0, 1, length.out = length_out),
        method = "linear",
        rule = 2,
        ties = "ordered"
    )$y
}
# }}}

# The empirical distribution primitives in this module are shared by
# quantile-based signals while leaving each method's transfer equation intact.

# Define interpolated empirical-CDF anchors with average-rank ties. For a
# sample x(1)...x(n), each distinct value receives
# p = (average_rank - 1) / (n - 1), with a constant sample placed at p = 0.5.
# quantile__cdf_anchors {{{
quantile__cdf_anchors <- function(sample) {
    checkmate::assert_numeric(
        sample,
        min.len = 1L,
        finite = TRUE,
        any.missing = FALSE
    )
    ordered <- sort(as.numeric(sample))
    runs <- rle(ordered)
    end_rank <- cumsum(runs$lengths)
    start_rank <- end_rank - runs$lengths + 1L
    average_rank <- (start_rank + end_rank) / 2
    probability <- if (length(ordered) == 1L) {
        0.5
    } else {
        (average_rank - 1) / (length(ordered) - 1)
    }
    data.frame(
        value = runs$values,
        probability = probability
    )
}
# }}}

# Evaluate the explicit empirical CDF and clamp values outside the sample to
# endpoint probabilities rather than extrapolating a new tail.
# quantile__empirical_cdf {{{
quantile__empirical_cdf <- function(sample, values) {
    checkmate::assert_numeric(
        values,
        finite = TRUE,
        any.missing = FALSE
    )
    anchors <- quantile__cdf_anchors(sample)
    probability <- if (nrow(anchors) == 1L) {
        rep.int(anchors$probability, length(values))
    } else {
        stats::approx(
            x = anchors$value,
            y = anchors$probability,
            xout = values,
            method = "linear",
            rule = 2,
            ties = "ordered"
        )$y
    }
    lower_tail <- values < min(anchors$value)
    upper_tail <- values > max(anchors$value)
    # Strictly external values receive endpoint probabilities even when tied
    # sample endpoints have interior average-rank probabilities.
    probability[lower_tail] <- 0
    probability[upper_tail] <- 1
    list(
        probability = probability,
        lower_tail = lower_tail,
        upper_tail = upper_tail,
        tied_sample_values = length(sample) - nrow(anchors)
    )
}
# }}}

# Evaluate an inverse empirical CDF with R's type-7 linear quantile rule.
# quantile__inverse_cdf {{{
quantile__inverse_cdf <- function(sample, probability) {
    checkmate::assert_numeric(
        probability,
        lower = 0,
        upper = 1,
        finite = TRUE,
        any.missing = FALSE
    )
    as.numeric(stats::quantile(
        sample,
        probs = probability,
        names = FALSE,
        type = 7
    ))
}
# }}}

# Derive a stable group-specific seed so stochastic preprocessing remains
# reproducible without assigning identical sequences to every location.
# quantile__group_seed {{{
quantile__group_seed <- function(seed, key, variable) {
    text <- paste(
        c(
            variable,
            unlist(
                Map(
                    function(name, value) {
                        paste0(name, "=", as.character(value))
                    },
                    names(key),
                    key
                ),
                use.names = FALSE
            )
        ),
        collapse = "\u001f"
    )
    modulus <- .Machine$integer.max - 1
    hash <- (as.double(seed) %% modulus) + 1
    for (code in utf8ToInt(enc2utf8(text))) {
        hash <- (hash * 131 + code) %% modulus
    }
    as.integer(hash + 1)
}
# }}}

# Generate reproducible uniform variates with the Park-Miller
# state[i] = 16807 * state[i-1] mod 2147483647 recurrence.
# quantile__uniform {{{
quantile__uniform <- function(n, seed) {
    checkmate::assert_count(n)
    checkmate::assert_int(
        seed,
        lower = 1L,
        upper = .Machine$integer.max - 1L
    )
    modulus <- as.double(.Machine$integer.max)
    state <- as.double(seed)
    out <- numeric(n)
    for (index in seq_len(n)) {
        state <- (16807 * state) %% modulus
        out[[index]] <- state / modulus
    }
    out
}
# }}}

# vim: fdm=marker :
