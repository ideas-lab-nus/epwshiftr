# The compact distribution helpers in this module support native statistical
# signals without introducing an external distribution class hierarchy.

# Validate one fitted distribution record before it is used by a CDF or
# inverse-CDF calculation.
distribution__validate_fit <- function(fit) {
    if (!is.list(fit) ||
        !identical(
            sort(names(fit)),
            sort(c("family", "parameters", "sample_size", "method"))
        )) {
        cli::cli_abort(
            "A parametric distribution fit must contain `family`, `parameters`, `sample_size`, and `method`."
        )
    }
    checkmate::assert_choice(fit$family, c("normal", "gamma"))
    checkmate::assert_list(fit$parameters, names = "unique")
    checkmate::assert_count(fit$sample_size, positive = TRUE)
    checkmate::assert_string(fit$method, min.chars = 1L)
    if (identical(fit$family, "normal")) {
        if (!identical(sort(names(fit$parameters)), c("location", "scale"))) {
            cli::cli_abort(
                "A Normal fit requires `location` and `scale` parameters."
            )
        }
    } else if (!identical(
        sort(names(fit$parameters)),
        c("scale", "shape")
    )) {
        cli::cli_abort(
            "A Gamma fit requires `shape` and `scale` parameters."
        )
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
            cli::cli_abort("A fitted Gamma shape must be positive.")
        }
    }
    invisible(TRUE)
}

# Fit the Normal maximum-likelihood location and scale. The scale uses the
# population denominator n, matching a likelihood fit rather than R's
# sample-standard-deviation denominator n - 1.
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

# Fit a zero-location Gamma distribution by maximum likelihood. For positive
# x, the profile-likelihood shape solves
# log(shape) - digamma(shape) = log(mean(x)) - mean(log(x));
# scale is then mean(x) / shape.
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

# Dispatch the two published SDM distribution families through one
# method-neutral fitting boundary.
distribution__fit <- function(
  values,
  family,
  tolerance = 1e-10,
  max_iterations = 1000L
) {
    checkmate::assert_choice(family, c("normal", "gamma"))
    if (identical(family, "normal")) {
        return(distribution__fit_normal(values))
    }
    distribution__fit_gamma(
        values,
        tolerance = tolerance,
        max_iterations = max_iterations
    )
}

# Evaluate the CDF of one validated fitted distribution.
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
    stats::pgamma(
        values,
        shape = fit$parameters$shape,
        scale = fit$parameters$scale
    )
}

# Evaluate the inverse CDF of one validated fitted distribution.
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
    stats::qgamma(
        probability,
        shape = fit$parameters$shape,
        scale = fit$parameters$scale
    )
}

# Clamp fitted probabilities away from zero and one so inverse CDFs remain
# finite under the method's declared numerical threshold.
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

# Interpolate one ordered vector onto a requested ordered length using a
# normalized rank coordinate. This is the SDM paper's length-normalization
# operation when calibration and projected samples differ.
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
