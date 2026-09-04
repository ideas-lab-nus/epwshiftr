#' @include signal-adjustment.R distribution-parametric.R
NULL

# Equidistant CDF Matching uses the Li et al. monthly method and the Cannon et
# al. proof that its additive equation is equivalent to absolute QDM.
EDCDF_REFERENCES <- c(
    "https://doi.org/10.1029/2009JD012882",
    "https://doi.org/10.1175/JCLI-D-14-00754.1"
)

# Li et al. fit their method to monthly temperature and precipitation. The
# package profiles retain those variables but label daily pooling as adapted.
EDCDF_LI_VARIABLES <- c("pr", "tas")

# Construct one complete settings record for the fixed Li distribution
# families and the package-selected native-calendar daily adaptation.
edcdf__default_settings <- function(
  bounds,
  distribution_model = c("beta_four_parameter", "mixed_gamma")
) {
    distribution_model <- match.arg(distribution_model)
    list(
        mapping = "additive_equidistant",
        seasonal_grouping = "calendar_month",
        projection_grouping = "complete_requested_period",
        distribution_model = distribution_model,
        range_extension_sd = 0.5,
        beta_fit_method = "maximum_likelihood_fixed_range",
        gamma_fit_method = "maximum_likelihood_zero_location",
        cdf_epsilon = 1e-10,
        min_samples = 10L,
        min_positive_samples = 2L,
        dry_threshold = 0,
        bounds = bounds,
        negative_precipitation_policy = "clip_zero",
        fit_tolerance = 1e-10,
        fit_max_iterations = 1000L
    )
}

# Build conservative daily profiles: the method-variable pairs are published
# at monthly frequency, while daily calendar-month pooling is a package
# adaptation and therefore remains visibly experimental.
edcdf__profiles <- function() {
    settings <- list(
        pr = edcdf__default_settings(c(0, Inf), "mixed_gamma"),
        tas = edcdf__default_settings(
            c(-Inf, Inf),
            "beta_four_parameter"
        )
    )
    lapply(EDCDF_LI_VARIABLES, function(variable) {
        signal__variable_profile(
            variable,
            settings = settings[[variable]],
            evidence = "experimental",
            references = EDCDF_REFERENCES,
            metadata = list(
                method = "equidistant_cdf_matching",
                output_role = "model_future",
                method_variable_source = "li_2010_monthly",
                frequency_source = "epwshiftr_daily_adaptation",
                equation_equivalence =
                    "absolute_quantile_delta_mapping"
            )
        )
    })
}

# Validate the complete numerical policy at the method boundary so a user
# override cannot silently select a distribution or temporal variant that the
# native kernel does not implement.
edcdf__settings <- function(settings) {
    expected <- c(
        "mapping",
        "seasonal_grouping",
        "projection_grouping",
        "distribution_model",
        "range_extension_sd",
        "beta_fit_method",
        "gamma_fit_method",
        "cdf_epsilon",
        "min_samples",
        "min_positive_samples",
        "dry_threshold",
        "bounds",
        "negative_precipitation_policy",
        "fit_tolerance",
        "fit_max_iterations"
    )
    resolved <- signal__resolve_settings(
        settings,
        expected,
        "Equidistant CDF Matching"
    )
    if (!identical(resolved$mapping, "additive_equidistant") ||
        !identical(resolved$seasonal_grouping, "calendar_month") ||
        !identical(
            resolved$projection_grouping,
            "complete_requested_period"
        ) ||
        !identical(
            resolved$beta_fit_method,
            "maximum_likelihood_fixed_range"
        ) ||
        !identical(
            resolved$gamma_fit_method,
            "maximum_likelihood_zero_location"
        ) ||
        !identical(
            resolved$negative_precipitation_policy,
            "clip_zero"
        )) {
        cli::cli_abort(
            "Equidistant CDF Matching currently requires the additive Li equation, native calendar-month pooling, complete requested projection periods, fixed-range Beta maximum likelihood, zero-location Gamma maximum likelihood, and zero clipping for negative precipitation."
        )
    }
    checkmate::assert_choice(
        resolved$distribution_model,
        c("beta_four_parameter", "mixed_gamma")
    )
    checkmate::assert_number(
        resolved$range_extension_sd,
        lower = 0,
        finite = TRUE
    )
    if (resolved$range_extension_sd <= 0) {
        cli::cli_abort("`range_extension_sd` must be positive.")
    }
    checkmate::assert_number(
        resolved$cdf_epsilon,
        lower = 0,
        upper = 0.5,
        finite = TRUE
    )
    if (resolved$cdf_epsilon <= 0 ||
        resolved$cdf_epsilon >= 0.5) {
        cli::cli_abort(
            "`cdf_epsilon` must lie strictly between zero and 0.5."
        )
    }
    resolved$min_samples <- signal__integer_setting(
        resolved$min_samples,
        "min_samples",
        lower = 2L
    )
    resolved$min_positive_samples <- signal__integer_setting(
        resolved$min_positive_samples,
        "min_positive_samples",
        lower = 2L
    )
    checkmate::assert_number(
        resolved$dry_threshold,
        lower = 0,
        finite = TRUE
    )
    signal__ordered_bounds(
        resolved$bounds,
        "Equidistant CDF Matching bounds must be ordered."
    )
    checkmate::assert_number(
        resolved$fit_tolerance,
        lower = 0,
        finite = TRUE
    )
    if (resolved$fit_tolerance <= 0) {
        cli::cli_abort("`fit_tolerance` must be positive.")
    }
    resolved$fit_max_iterations <- signal__integer_setting(
        resolved$fit_max_iterations,
        "fit_max_iterations",
        lower = 1L
    )
    if (identical(
        resolved$distribution_model,
        "beta_four_parameter"
    ) && resolved$dry_threshold != 0) {
        cli::cli_abort(
            "Temperature Equidistant CDF Matching requires `dry_threshold = 0`."
        )
    }

    resolved
}

# Resolve and validate the three role-addressable daily series without
# coercing their native CF calendars to Gregorian dates.
edcdf__inputs <- function(inputs, variable, distribution_model) {
    roles <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    if (!is.list(inputs) || !all(roles %in% names(inputs))) {
        cli::cli_abort(
            "Equidistant CDF Matching requires observed, historical-model, and future-model role payloads."
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
                "Equidistant CDF Matching role {.val {role}} must contain only variable {.val {variable}}."
            )
        }
        if (length(unique(series[[role]][["cf_calendar"]])) != 1L) {
            cli::cli_abort(
                "Equidistant CDF Matching role {.val {role}} must contain one native calendar per signal group."
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
            "Equidistant CDF Matching inputs for {.val {variable}} must use identical units."
        )
    }
    if (identical(distribution_model, "mixed_gamma") &&
        any(vapply(
            series,
            function(data) any(data[["value"]] < 0),
            logical(1L)
        ))) {
        cli::cli_abort(
            "Mixed-Gamma Equidistant CDF Matching requires non-negative input values."
        )
    }
    series
}

# Validate the method-local four-parameter Beta or mixed-Gamma fit record
# before evaluating its CDF or generalized inverse.
edcdf__validate_fit <- function(fit) {
    required <- c(
        "family",
        "parameters",
        "sample_size",
        "method",
        "log_likelihood"
    )
    if (!is.list(fit) || !all(required %in% names(fit))) {
        cli::cli_abort(
            "An Equidistant CDF Matching fit must contain its family, parameters, sample size, method, and log likelihood."
        )
    }
    checkmate::assert_choice(
        fit$family,
        c("beta_four_parameter", "mixed_gamma")
    )
    checkmate::assert_list(fit$parameters, names = "unique")
    checkmate::assert_count(fit$sample_size, positive = TRUE)
    checkmate::assert_string(fit$method, min.chars = 1L)
    checkmate::assert_number(fit$log_likelihood, finite = TRUE)
    if (identical(fit$family, "beta_four_parameter")) {
        expected <- c("lower", "shape1", "shape2", "upper")
        if (!identical(sort(names(fit$parameters)), sort(expected))) {
            cli::cli_abort(
                "A four-parameter Beta fit requires lower, upper, shape1, and shape2 parameters."
            )
        }
        if (fit$parameters$lower >= fit$parameters$upper ||
            fit$parameters$shape1 <= 0 ||
            fit$parameters$shape2 <= 0) {
            cli::cli_abort(
                "A four-parameter Beta fit requires ordered bounds and positive shapes."
            )
        }
        return(invisible(TRUE))
    }
    expected <- c(
        "dry_probability",
        "dry_threshold",
        "scale",
        "shape",
        "wet_probability"
    )
    if (!identical(sort(names(fit$parameters)), sort(expected))) {
        cli::cli_abort(
            "A mixed-Gamma fit requires dry and wet probabilities, dry threshold, shape, and scale."
        )
    }
    if (fit$parameters$dry_probability < 0 ||
        fit$parameters$wet_probability <= 0 ||
        abs(
            fit$parameters$dry_probability +
                fit$parameters$wet_probability - 1
        ) > sqrt(.Machine$double.eps) ||
        fit$parameters$shape <= 0 ||
        fit$parameters$scale <= 0) {
        cli::cli_abort(
            "A mixed-Gamma fit requires valid mixture probabilities and positive Gamma parameters."
        )
    }
    invisible(TRUE)
}

# Fit the Li et al. four-parameter Beta distribution. The range endpoints are
# fixed at the sample extrema plus or minus one-half standard deviation by
# default, then the two shape parameters are estimated by maximum likelihood.
edcdf__fit_beta4 <- function(
  values,
  range_extension_sd,
  tolerance,
  max_iterations
) {
    checkmate::assert_numeric(
        values,
        min.len = 2L,
        finite = TRUE,
        any.missing = FALSE
    )
    spread <- stats::sd(values)
    if (!is.finite(spread) || spread <= 0) {
        cli::cli_abort(
            "A four-parameter Beta distribution cannot be fitted to a constant sample."
        )
    }
    lower <- min(values) - range_extension_sd * spread
    upper <- max(values) + range_extension_sd * spread
    scaled <- (values - lower) / (upper - lower)
    scaled_mean <- mean(scaled)
    scaled_variance <- stats::var(scaled)
    concentration <- (
        scaled_mean * (1 - scaled_mean) / scaled_variance
    ) - 1
    if (!is.finite(concentration) || concentration <= 0) {
        initial_shape <- c(1, 1)
    } else {
        initial_shape <- c(
            scaled_mean * concentration,
            (1 - scaled_mean) * concentration
        )
    }
    initial_shape <- pmin(pmax(initial_shape, 1e-3), 1e3)

    # Optimizing log-shapes guarantees positive parameters without changing
    # the fixed range selected from the publication.
    objective <- function(log_shape) {
        shape <- exp(log_shape)
        value <- -sum(stats::dbeta(
            scaled,
            shape1 = shape[[1L]],
            shape2 = shape[[2L]],
            log = TRUE
        ))
        if (is.finite(value)) value else .Machine$double.xmax
    }
    optimization <- stats::optim(
        par = log(initial_shape),
        fn = objective,
        method = "L-BFGS-B",
        lower = log(c(1e-6, 1e-6)),
        upper = log(c(1e6, 1e6)),
        control = list(
            factr = max(
                1,
                tolerance / .Machine$double.eps
            ),
            maxit = max_iterations
        )
    )
    if (optimization$convergence != 0L ||
        !is.finite(optimization$value)) {
        cli::cli_abort(
            "Four-parameter Beta maximum-likelihood fitting did not converge."
        )
    }
    shape <- exp(optimization$par)
    fit <- list(
        family = "beta_four_parameter",
        parameters = list(
            lower = lower,
            upper = upper,
            shape1 = shape[[1L]],
            shape2 = shape[[2L]]
        ),
        sample_size = length(values),
        method = "maximum_likelihood_fixed_range",
        log_likelihood = -optimization$value -
            length(values) * log(upper - lower)
    )
    edcdf__validate_fit(fit)
    fit
}

# Fit the Li et al. precipitation mixture: an empirical point mass for dry
# values and a zero-location Gamma maximum-likelihood fit for wet amounts.
edcdf__fit_mixed_gamma <- function(
  values,
  dry_threshold,
  min_positive_samples,
  tolerance,
  max_iterations
) {
    checkmate::assert_numeric(
        values,
        min.len = 2L,
        lower = 0,
        finite = TRUE,
        any.missing = FALSE
    )
    wet <- values > dry_threshold
    positive <- values[wet]
    if (length(positive) < min_positive_samples) {
        cli::cli_abort(
            "Mixed-Gamma fitting requires at least {min_positive_samples} positive values above the dry threshold."
        )
    }
    gamma <- distribution__fit_gamma(
        positive,
        tolerance = tolerance,
        max_iterations = max_iterations
    )
    dry_probability <- mean(!wet)
    wet_probability <- 1 - dry_probability
    dry_log_likelihood <- if (any(!wet)) {
        sum(!wet) * log(dry_probability)
    } else {
        0
    }
    wet_log_likelihood <- length(positive) * log(wet_probability) +
        sum(stats::dgamma(
            positive,
            shape = gamma$parameters$shape,
            scale = gamma$parameters$scale,
            log = TRUE
        ))
    fit <- list(
        family = "mixed_gamma",
        parameters = list(
            dry_probability = dry_probability,
            wet_probability = wet_probability,
            dry_threshold = dry_threshold,
            shape = gamma$parameters$shape,
            scale = gamma$parameters$scale
        ),
        sample_size = length(values),
        positive_sample_size = length(positive),
        method = "point_mass_and_maximum_likelihood_zero_location",
        log_likelihood = dry_log_likelihood + wet_log_likelihood
    )
    edcdf__validate_fit(fit)
    fit
}

# Dispatch the two publication-backed distribution families while retaining
# their method-specific fitting conventions.
edcdf__fit_distribution <- function(values, resolved) {
    if (identical(
        resolved$distribution_model,
        "beta_four_parameter"
    )) {
        return(edcdf__fit_beta4(
            values,
            resolved$range_extension_sd,
            resolved$fit_tolerance,
            resolved$fit_max_iterations
        ))
    }
    edcdf__fit_mixed_gamma(
        values,
        resolved$dry_threshold,
        resolved$min_positive_samples,
        resolved$fit_tolerance,
        resolved$fit_max_iterations
    )
}

# Evaluate the fitted future distribution at projected values using either the
# bounded Beta CDF or the dry-point-mass plus Gamma CDF.
edcdf__cdf <- function(fit, values) {
    edcdf__validate_fit(fit)
    checkmate::assert_numeric(
        values,
        finite = TRUE,
        any.missing = FALSE
    )
    parameters <- fit$parameters
    if (identical(fit$family, "beta_four_parameter")) {
        scaled <- (values - parameters$lower) /
            (parameters$upper - parameters$lower)
        return(stats::pbeta(
            pmin(pmax(scaled, 0), 1),
            shape1 = parameters$shape1,
            shape2 = parameters$shape2
        ))
    }
    probability <- rep.int(
        parameters$dry_probability,
        length(values)
    )
    wet <- values > parameters$dry_threshold
    probability[wet] <- parameters$dry_probability +
        parameters$wet_probability * stats::pgamma(
            values[wet],
            shape = parameters$shape,
            scale = parameters$scale
        )
    probability
}

# Evaluate the generalized inverse for a four-parameter Beta or mixed-Gamma
# fit. Probabilities inside the dry point mass map deterministically to zero.
edcdf__quantile <- function(fit, probability) {
    edcdf__validate_fit(fit)
    checkmate::assert_numeric(
        probability,
        lower = 0,
        upper = 1,
        finite = TRUE,
        any.missing = FALSE
    )
    parameters <- fit$parameters
    if (identical(fit$family, "beta_four_parameter")) {
        return(
            parameters$lower +
                (parameters$upper - parameters$lower) *
                    stats::qbeta(
                        probability,
                        shape1 = parameters$shape1,
                        shape2 = parameters$shape2
                    )
        )
    }
    value <- numeric(length(probability))
    wet <- probability > parameters$dry_probability
    conditional_probability <- (
        probability[wet] - parameters$dry_probability
    ) / parameters$wet_probability
    value[wet] <- stats::qgamma(
        pmin(pmax(conditional_probability, 0), 1),
        shape = parameters$shape,
        scale = parameters$scale
    )
    value
}

# Replace values at or below the declared dry threshold with the point-mass
# value before fitting, recording role-specific occurrence counts.
edcdf__prepared_values <- function(series, resolved) {
    values <- lapply(series, function(data) data[["value"]])
    if (!identical(resolved$distribution_model, "mixed_gamma")) {
        return(list(values = values, precipitation = NULL))
    }
    dry_counts <- vapply(
        values,
        function(value) sum(value <= resolved$dry_threshold),
        integer(1L)
    )
    values <- lapply(values, function(value) {
        value[value <= resolved$dry_threshold] <- 0
        value
    })
    list(
        values = values,
        precipitation = list(
            dry_threshold = resolved$dry_threshold,
            input_dry_values = dry_counts
        )
    )
}

# Apply the Li additive equation
# x_adjusted = x_future + Q_observed(p) - Q_historical(p),
# where p is the fitted future-model probability.
edcdf__map_values <- function(
  observed,
  historical,
  future,
  resolved
) {
    fit_observed <- edcdf__fit_distribution(observed, resolved)
    fit_historical <- edcdf__fit_distribution(
        historical,
        resolved
    )
    fit_future <- edcdf__fit_distribution(future, resolved)
    raw_probability <- edcdf__cdf(fit_future, future)
    probability <- distribution__clamp_probability(
        raw_probability,
        resolved$cdf_epsilon
    )
    observed_quantile <- edcdf__quantile(
        fit_observed,
        probability
    )
    historical_quantile <- edcdf__quantile(
        fit_historical,
        probability
    )
    correction <- observed_quantile - historical_quantile
    list(
        value = future + correction,
        fits = list(
            observed_reference = fit_observed,
            model_historical = fit_historical,
            model_future = fit_future
        ),
        diagnostics = list(
            probability_range = range(probability),
            probability_clamped_values = sum(
                probability != raw_probability
            ),
            observed_quantile_range = range(observed_quantile),
            historical_quantile_range = range(historical_quantile),
            correction_range = range(correction)
        )
    )
}

# Convert one native calendar-month adjustment into inspectable provenance
# without retaining the full intermediate daily arrays.
edcdf__month_record <- function(
  month,
  observed,
  historical,
  future,
  mapped
) {
    c(
        list(
            month = as.integer(month),
            observed_samples = nrow(observed),
            historical_samples = nrow(historical),
            future_samples = nrow(future),
            observed_years = range(observed[["cf_year"]]),
            historical_years = range(historical[["cf_year"]]),
            future_years = range(future[["cf_year"]]),
            fits = mapped$fits
        ),
        mapped$diagnostics
    )
}

# Fit and apply one distribution triplet per native calendar month. This is an
# explicit daily adaptation of the publication's separate monthly fields.
edcdf__adjust_values <- function(series, resolved) {
    prepared <- edcdf__prepared_values(series, resolved)
    observed <- series$observed_reference
    historical <- series$model_historical
    future <- series$model_future
    adjusted <- rep.int(NA_real_, nrow(future))
    records <- list()
    months <- sort(unique(future[["cf_month"]]))

    for (record_index in seq_along(months)) {
        month <- months[[record_index]]
        observed_rows <- observed[["cf_month"]] == month
        historical_rows <- historical[["cf_month"]] == month
        future_rows <- future[["cf_month"]] == month
        sample_counts <- c(
            observed = sum(observed_rows),
            historical = sum(historical_rows),
            future = sum(future_rows)
        )
        if (any(sample_counts < resolved$min_samples)) {
            cli::cli_abort(
                "Equidistant CDF Matching month {month} has fewer than {resolved$min_samples} observed, historical, or future daily values."
            )
        }
        mapped <- edcdf__map_values(
            prepared$values$observed_reference[observed_rows],
            prepared$values$model_historical[historical_rows],
            prepared$values$model_future[future_rows],
            resolved
        )
        adjusted[future_rows] <- mapped$value
        records[[record_index]] <- edcdf__month_record(
            month,
            observed[observed_rows, , drop = FALSE],
            historical[historical_rows, , drop = FALSE],
            future[future_rows, , drop = FALSE],
            mapped
        )
    }
    if (anyNA(adjusted)) {
        cli::cli_abort(
            "Equidistant CDF Matching did not assign every future-model daily row exactly once."
        )
    }

    precipitation <- prepared$precipitation
    if (!is.null(precipitation)) {
        precipitation$negative_before_clipping <- sum(adjusted < 0)
        recensored <- adjusted <= resolved$dry_threshold
        adjusted[recensored] <- 0
        precipitation$output_dry_values <- sum(recensored)
    }
    bounded_result <- signal__bound_values(adjusted, resolved$bounds)
    bounded <- bounded_result$value
    diagnostics <- list(
        month_count = length(records),
        observed_month_samples = c(
            minimum = min(vapply(
                records,
                function(record) record$observed_samples,
                integer(1L)
            )),
            maximum = max(vapply(
                records,
                function(record) record$observed_samples,
                integer(1L)
            ))
        ),
        historical_month_samples = c(
            minimum = min(vapply(
                records,
                function(record) record$historical_samples,
                integer(1L)
            )),
            maximum = max(vapply(
                records,
                function(record) record$historical_samples,
                integer(1L)
            ))
        ),
        future_month_samples = c(
            minimum = min(vapply(
                records,
                function(record) record$future_samples,
                integer(1L)
            )),
            maximum = max(vapply(
                records,
                function(record) record$future_samples,
                integer(1L)
            ))
        ),
        clipped_values = bounded_result$clipped,
        months = records
    )
    if (!is.null(precipitation)) {
        diagnostics$precipitation <- precipitation
    }
    list(value = bounded, diagnostics = diagnostics)
}

# Execute one univariate Equidistant CDF Matching group and return the common
# future-backbone DailyAdjustedSeries contract.
edcdf__apply_group <- function(inputs, settings, key) {
    resolved <- edcdf__settings(settings)
    variable <- names(settings)[[1L]]
    series <- edcdf__inputs(
        inputs,
        variable,
        resolved$distribution_model
    )
    mapped <- edcdf__adjust_values(series, resolved)
    future <- series$model_future
    future[["value"]] <- mapped$value

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = "equidistant_cdf_matching",
        settings = resolved,
        provenance = list(
            method = "equidistant_cdf_matching",
            references = EDCDF_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            published_frequency = "mon",
            adapted_frequency = "day",
            frequency_source = "epwshiftr_daily_adaptation",
            seasonal_grouping = "native_calendar_month",
            equation_equivalence =
                "absolute_quantile_delta_mapping",
            diagnostics = mapped$diagnostics
        )
    )
}

# Return an explicit diagnostic if the method violates the common
# future-model output contract.
edcdf__validate_result <- function(value, inputs, key) {
    signal__validate_adjusted_result(
        value,
        DailyAdjustedSeries,
        "DailyAdjustedSeries",
        "model_future",
        "Equidistant CDF Matching"
    )
}

# Construct the package-native daily signal with only the Li temperature and
# precipitation variable alternatives.
edcdf__component <- function() {
    alternatives <- as.list(EDCDF_LI_VARIABLES)
    requirements <- signal__three_role_requirements(
        alternatives,
        frequencies = "day"
    )
    signal__component(
        name = "equidistant_cdf_matching_daily",
        label = "Daily Equidistant CDF Matching",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = FALSE,
        profiles = edcdf__profiles(),
        apply_group = edcdf__apply_group,
        operations = list(validate_result = edcdf__validate_result),
        metadata = list(
            method_family = "parametric_quantile_mapping",
            published_frequency = "mon",
            adapted_frequency = "day",
            daily_adaptation = "native_calendar_month_pooling",
            equation_equivalence =
                "absolute_quantile_delta_mapping",
            distributions = list(
                tas = "beta_four_parameter",
                pr = "mixed_gamma"
            )
        )
    )
}

# Register the daily Equidistant CDF Matching signal during package load.
edcdf__register_component <- function() {
    component__register_builtin(edcdf__component())
    invisible(NULL)
}
