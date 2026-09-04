#' @include signal-adjustment.R
NULL

# Wang et al. document the hourly kernel-density QDM workflow, while Cannon
# et al. provide the additive and multiplicative trend-preserving equations.
KQDM_REFERENCES <- c(
    "https://doi.org/10.1038/s41467-023-41458-5",
    "https://doi.org/10.1175/JCLI-D-14-00754.1"
)

# These variables follow the hourly weather fields and transformation families
# stated by Wang et al.; package extensions remain separate below.
KQDM_PUBLISHED_VARIABLES <- c(
    "tas",
    "ps",
    "hurs",
    "sfcWind",
    "rsds",
    "rsdsdiff"
)
KQDM_EXPERIMENTAL_VARIABLES <- c("psl", "rlds")

# Construct the complete settings schema while keeping the paper's method
# choices separate from numerical KDE defaults selected by this package.
kqdm__default_settings <- function(
    transformation = c("additive", "multiplicative"),
    bounds
) {
    transformation <- match.arg(transformation)
    list(
        transformation = transformation,
        window_months = 3L,
        window_alignment = "centered",
        cdf_method = "kernel_density",
        kernel = "gaussian",
        bandwidth_method = "nrd0",
        bandwidth_adjust = 1,
        grid_points = 2048L,
        tail_policy = "density_grid_clamp",
        min_samples = 30L,
        bounds = bounds,
        zero_tolerance = sqrt(.Machine$double.eps),
        zero_denominator_policy = "zero_future_else_error"
    )
}

# Build variable-specific profiles. Every profile is labelled experimental
# because the publication does not report the kernel, bandwidth, grid, tail,
# or zero-denominator conventions needed for an executable implementation.
kqdm__profiles <- function() {
    specifications <- list(
        tas = list("additive", c(-Inf, Inf)),
        ps = list("additive", c(0, Inf)),
        hurs = list("multiplicative", c(0, 100)),
        sfcWind = list("multiplicative", c(0, Inf)),
        rsds = list("multiplicative", c(0, Inf)),
        rsdsdiff = list("multiplicative", c(0, Inf)),
        psl = list("additive", c(0, Inf)),
        rlds = list("multiplicative", c(0, Inf))
    )
    lapply(names(specifications), function(variable) {
        specification <- specifications[[variable]]
        published_variable <- variable %in% KQDM_PUBLISHED_VARIABLES
        signal__variable_profile(
            variable,
            settings = kqdm__default_settings(
                specification[[1L]],
                specification[[2L]]
            ),
            evidence = "experimental",
            references = KQDM_REFERENCES,
            metadata = list(
                method = "kernel_quantile_delta_mapping",
                output_role = "model_future",
                variable_evidence = if (published_variable) {
                    "method_literature"
                } else {
                    "package_extension"
                },
                published_settings = c(
                    "transformation",
                    "window_months",
                    "window_alignment",
                    "cdf_method"
                ),
                package_selected_settings = c(
                    "kernel",
                    "bandwidth_method",
                    "bandwidth_adjust",
                    "grid_points",
                    "tail_policy",
                    "min_samples",
                    "bounds",
                    "zero_tolerance",
                    "zero_denominator_policy"
                )
            )
        )
    })
}

# Validate every executable convention at the signal boundary so user
# overrides cannot silently introduce an unsupported numerical method.
kqdm__settings <- function(settings) {
    expected <- c(
        "transformation",
        "window_months",
        "window_alignment",
        "cdf_method",
        "kernel",
        "bandwidth_method",
        "bandwidth_adjust",
        "grid_points",
        "tail_policy",
        "min_samples",
        "bounds",
        "zero_tolerance",
        "zero_denominator_policy"
    )
    resolved <- signal__resolve_settings(
        settings,
        expected,
        "Kernel-density Quantile Delta Mapping"
    )
    checkmate::assert_choice(
        resolved$transformation,
        c("additive", "multiplicative")
    )
    resolved$window_months <- signal__integer_setting(
        resolved$window_months,
        "window_months",
        lower = 1L,
        upper = 12L
    )
    if (!identical(resolved$window_months, 3L) ||
        !identical(resolved$window_alignment, "centered")) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping currently requires the published centered three-month window."
        )
    }
    if (!identical(resolved$cdf_method, "kernel_density")) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping requires `cdf_method = \"kernel_density\"`."
        )
    }
    checkmate::assert_choice(
        resolved$kernel,
        c(
            "gaussian",
            "epanechnikov",
            "rectangular",
            "triangular",
            "biweight",
            "cosine",
            "optcosine"
        )
    )
    checkmate::assert_choice(
        resolved$bandwidth_method,
        c("nrd0", "nrd", "ucv", "bcv", "SJ-ste", "SJ-dpi")
    )
    checkmate::assert_number(
        resolved$bandwidth_adjust,
        lower = 0,
        finite = TRUE
    )
    if (resolved$bandwidth_adjust <= 0) {
        cli::cli_abort("`bandwidth_adjust` must be positive.")
    }
    resolved$grid_points <- signal__integer_setting(
        resolved$grid_points,
        "grid_points",
        lower = 128L
    )
    grid_points <- resolved$grid_points
    if (abs(log2(grid_points) - round(log2(grid_points))) >
        sqrt(.Machine$double.eps)) {
        cli::cli_abort("`grid_points` must be a power of two.")
    }
    checkmate::assert_choice(
        resolved$tail_policy,
        c("density_grid_clamp", "error")
    )
    resolved$min_samples <- signal__integer_setting(
        resolved$min_samples,
        "min_samples",
        lower = 3L
    )
    signal__ordered_bounds(
        resolved$bounds,
        "Kernel-density Quantile Delta Mapping bounds must be ordered from lower to upper."
    )
    checkmate::assert_number(
        resolved$zero_tolerance,
        lower = 0,
        finite = TRUE
    )
    checkmate::assert_choice(
        resolved$zero_denominator_policy,
        c("zero_future_else_error", "error")
    )

    resolved$grid_points <- grid_points
    resolved
}

# Validate the three role-addressable hourly inputs without coercing any native
# CF calendar to Gregorian dates or discarding the time-of-day coordinate.
kqdm__inputs <- function(inputs, variable, transformation) {
    roles <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    if (!identical(sort(names(inputs)), sort(roles))) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping requires observed, historical-model, and future-model role payloads."
        )
    }
    series <- lapply(roles, function(role) {
        bias__subdaily_table(
            inputs[[role]],
            frequency = "hour",
            time_step_seconds = 3600,
            name = role
        )
    })
    names(series) <- roles
    for (role in roles) {
        role_variables <- unique(series[[role]][["variable_id"]])
        if (!identical(role_variables, variable)) {
            cli::cli_abort(
                "Kernel-density Quantile Delta Mapping role {.val {role}} must contain only variable {.val {variable}}."
            )
        }
        if (length(unique(series[[role]][["cf_calendar"]])) != 1L) {
            cli::cli_abort(
                "Kernel-density Quantile Delta Mapping role {.val {role}} must contain one native calendar per signal group."
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
            "Kernel-density Quantile Delta Mapping inputs for {.val {variable}} must use identical units."
        )
    }
    if (identical(transformation, "multiplicative") &&
        any(vapply(
            series,
            function(data) any(data[["value"]] < 0),
            logical(1L)
        ))) {
        cli::cli_abort(
            "Multiplicative kernel-density Quantile Delta Mapping requires non-negative input values."
        )
    }
    series
}

# Return the previous, current, and following calendar months with December-to-
# January wrapping independent of the number of days in either source calendar.
kqdm__window_months <- function(center_month) {
    checkmate::assert_integerish(
        center_month,
        lower = 1L,
        upper = 12L,
        len = 1L,
        any.missing = FALSE
    )
    as.integer(((as.integer(center_month) - 1L + -1L:1L) %% 12L) + 1L)
}

# Estimate one Gaussian kernel-density CDF on an explicit grid and retain only
# compact fit metadata rather than serializing the full numerical curve.
kqdm__density_cdf <- function(values, resolved, label) {
    sample_count <- length(values)
    if (sample_count < resolved$min_samples) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping {.val {label}} has {sample_count} samples; at least {resolved$min_samples} are required."
        )
    }
    if (length(unique(values)) < 2L) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping {.val {label}} requires at least two distinct values."
        )
    }
    # The explicit three-bandwidth support and grid size make the otherwise
    # unreported numerical CDF approximation reproducible across executions.
    estimate <- tryCatch(
        stats::density(
            values,
            bw = resolved$bandwidth_method,
            adjust = resolved$bandwidth_adjust,
            kernel = resolved$kernel,
            n = resolved$grid_points,
            cut = 3,
            na.rm = FALSE
        ),
        error = function(error) {
            cli::cli_abort(
                "Kernel-density Quantile Delta Mapping could not fit {.val {label}} with bandwidth method {.val {resolved$bandwidth_method}}: {conditionMessage(error)}"
            )
        }
    )
    bandwidth <- estimate$bw
    if (!is.finite(bandwidth) || bandwidth <= 0) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping could not estimate a positive bandwidth for {.val {label}}."
        )
    }
    increments <- diff(estimate$x) * (
        head(estimate$y, -1L) + tail(estimate$y, -1L)
    ) / 2
    cumulative <- c(0, cumsum(pmax(increments, 0)))
    total <- cumulative[[length(cumulative)]]
    if (!is.finite(total) || total <= 0) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping produced an invalid CDF for {.val {label}}."
        )
    }
    cumulative <- cumulative / total
    inverse_index <- !duplicated(cumulative, fromLast = TRUE)
    if (sum(inverse_index) < 2L) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping produced fewer than two distinct CDF probabilities for {.val {label}}."
        )
    }
    list(
        x = estimate$x,
        probability = cumulative,
        inverse_probability = cumulative[inverse_index],
        inverse_x = estimate$x[inverse_index],
        summary = list(
            samples = sample_count,
            sample_range = range(values),
            bandwidth = bandwidth,
            density_support = range(estimate$x)
        )
    )
}

# Evaluate one fitted KDE CDF with the declared endpoint clamp and report use
# of the finite density-grid tails for diagnostics.
kqdm__cdf <- function(distribution, values, tail_policy) {
    lower_tail <- values < distribution$x[[1L]]
    upper_tail <- values > distribution$x[[length(distribution$x)]]
    if (identical(tail_policy, "error") &&
        any(lower_tail | upper_tail)) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping encountered {sum(lower_tail | upper_tail)} value(s) outside the fitted density grid."
        )
    }
    list(
        probability = stats::approx(
            distribution$x,
            distribution$probability,
            xout = values,
            rule = 2,
            ties = "ordered"
        )$y,
        lower_tail = lower_tail,
        upper_tail = upper_tail
    )
}

# Numerically invert one monotone KDE CDF grid using the same endpoint clamp as
# forward evaluation so tail behavior stays explicit and deterministic.
kqdm__inverse_cdf <- function(distribution, probability) {
    stats::approx(
        distribution$inverse_probability,
        distribution$inverse_x,
        xout = probability,
        rule = 2,
        ties = "ordered"
    )$y
}

# Apply the additive or multiplicative QDM equation to every future value in
# one calendar month after fitting all three role distributions.
kqdm__map_values <- function(
    observed_distribution,
    historical_distribution,
    future_distribution,
    future_values,
    resolved,
    center_month
) {
    future_cdf <- kqdm__cdf(
        future_distribution,
        future_values,
        resolved$tail_policy
    )
    probability <- future_cdf$probability
    observed_quantile <- kqdm__inverse_cdf(
        observed_distribution,
        probability
    )
    historical_quantile <- kqdm__inverse_cdf(
        historical_distribution,
        probability
    )

    if (identical(resolved$transformation, "additive")) {
        # Additive QDM transfers x_future - Q_historical(p) onto the observed
        # quantile without replacing the future model's hourly chronology.
        change <- future_values - historical_quantile
        adjusted <- observed_quantile + change
        zero_future_values <- 0L
    } else {
        denominator_invalid <- (
            historical_quantile <= resolved$zero_tolerance
        )
        future_zero <- abs(future_values) <= resolved$zero_tolerance
        undefined <- denominator_invalid & (
            !future_zero |
                identical(resolved$zero_denominator_policy, "error")
        )
        if (any(undefined)) {
            cli::cli_abort(
                "Multiplicative kernel-density Quantile Delta Mapping month {center_month} encountered {sum(undefined)} positive future value(s) with a non-positive or near-zero historical-model quantile."
            )
        }

        # Exact future zeros remain zero when their smoothed historical
        # quantile is undefined; all positive values use the published ratio.
        change <- numeric(length(future_values))
        valid <- !denominator_invalid
        change[valid] <- future_values[valid] / historical_quantile[valid]
        adjusted <- numeric(length(future_values))
        adjusted[valid] <- observed_quantile[valid] * change[valid]
        zero_future_values <- sum(denominator_invalid & future_zero)
    }
    list(
        value = adjusted,
        probability = probability,
        change = change,
        lower_tail = future_cdf$lower_tail,
        upper_tail = future_cdf$upper_tail,
        zero_future_values = zero_future_values
    )
}

# Fit centered three-month KDEs independently for each output month, then map
# all rows while retaining the original future-model ordering and coordinates.
kqdm__adjust_values <- function(series, resolved) {
    observed <- series$observed_reference
    historical <- series$model_historical
    future <- series$model_future
    adjusted <- rep.int(NA_real_, nrow(future))
    diagnostics <- vector("list", length(unique(future[["cf_month"]])))
    center_months <- sort(unique(future[["cf_month"]]))

    for (month_index in seq_along(center_months)) {
        center_month <- center_months[[month_index]]
        window_months <- kqdm__window_months(center_month)
        observed_values <- observed[["value"]][
            observed[["cf_month"]] %in% window_months
        ]
        historical_values <- historical[["value"]][
            historical[["cf_month"]] %in% window_months
        ]
        future_values <- future[["value"]][
            future[["cf_month"]] %in% window_months
        ]
        observed_distribution <- kqdm__density_cdf(
            observed_values,
            resolved,
            sprintf("observed month %d window", center_month)
        )
        historical_distribution <- kqdm__density_cdf(
            historical_values,
            resolved,
            sprintf("historical-model month %d window", center_month)
        )
        future_distribution <- kqdm__density_cdf(
            future_values,
            resolved,
            sprintf("future-model month %d window", center_month)
        )

        output_index <- which(future[["cf_month"]] == center_month)
        mapped <- kqdm__map_values(
            observed_distribution,
            historical_distribution,
            future_distribution,
            future[["value"]][output_index],
            resolved,
            center_month
        )
        adjusted[output_index] <- mapped$value
        diagnostics[[month_index]] <- data.frame(
            center_month = center_month,
            window_months = paste(window_months, collapse = ","),
            observed_samples = observed_distribution$summary$samples,
            historical_samples = historical_distribution$summary$samples,
            future_samples = future_distribution$summary$samples,
            observed_bandwidth = observed_distribution$summary$bandwidth,
            historical_bandwidth = historical_distribution$summary$bandwidth,
            future_bandwidth = future_distribution$summary$bandwidth,
            probability_min = min(mapped$probability),
            probability_max = max(mapped$probability),
            change_min = min(mapped$change),
            change_max = max(mapped$change),
            future_lower_tail_values = sum(mapped$lower_tail),
            future_upper_tail_values = sum(mapped$upper_tail),
            zero_future_values = mapped$zero_future_values,
            stringsAsFactors = FALSE
        )
    }
    if (anyNA(adjusted)) {
        cli::cli_abort(
            "Kernel-density Quantile Delta Mapping did not assign every future-model row."
        )
    }

    bounded_result <- signal__bound_values(adjusted, resolved$bounds)
    bounded <- bounded_result$value
    month_diagnostics <- do.call(rbind, diagnostics)
    month_diagnostics$clipped_values <- vapply(
        month_diagnostics$center_month,
        function(center_month) {
            index <- future[["cf_month"]] == center_month
            sum(bounded[index] != adjusted[index])
        },
        integer(1L)
    )
    rownames(month_diagnostics) <- NULL
    list(
        value = bounded,
        diagnostics = list(
            months = month_diagnostics,
            mapped_probability_range = range(c(
                month_diagnostics$probability_min,
                month_diagnostics$probability_max
            )),
            transferred_change_range = range(c(
                month_diagnostics$change_min,
                month_diagnostics$change_max
            )),
            clipped_values = sum(month_diagnostics$clipped_values),
            implementation = list(
                kernel = resolved$kernel,
                bandwidth_method = resolved$bandwidth_method,
                grid_points = resolved$grid_points,
                tail_policy = resolved$tail_policy
            )
        )
    )
}

# Execute one aligned hourly univariate group and return the common
# frequency-aware adjusted-series contract with full numerical provenance.
kqdm__apply_group <- function(inputs, settings, key) {
    resolved <- kqdm__settings(settings)
    variable <- names(settings)[[1L]]
    series <- kqdm__inputs(
        inputs,
        variable,
        resolved$transformation
    )
    mapped <- kqdm__adjust_values(series, resolved)
    future <- series$model_future
    future[["value"]] <- mapped$value

    bias__subdaily_adjusted_series(
        future,
        frequency = "hour",
        time_step_seconds = 3600,
        output_role = "model_future",
        transformation = "kernel_quantile_delta_mapping",
        settings = resolved,
        provenance = list(
            method = "kernel_quantile_delta_mapping",
            references = KQDM_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            published_method_settings = c(
                "kernel_density_cdf",
                "centered_three_month_window",
                resolved$transformation
            ),
            package_selected_numerics = list(
                kernel = resolved$kernel,
                bandwidth_method = resolved$bandwidth_method,
                bandwidth_adjust = resolved$bandwidth_adjust,
                grid_points = resolved$grid_points,
                tail_policy = resolved$tail_policy,
                zero_denominator_policy = resolved$zero_denominator_policy
            ),
            diagnostics = mapped$diagnostics
        )
    )
}

# Return an explicit diagnostic if a kernel violates the future-model hourly
# output contract expected by downstream sequence components.
kqdm__validate_result <- function(value, inputs, key) {
    signal__validate_adjusted_result(
        value,
        SubdailyAdjustedSeries,
        "SubdailyAdjustedSeries",
        "model_future",
        "Kernel-density Quantile Delta Mapping",
        frequency = "hour",
        time_step_seconds = 3600,
        temporal_message = paste0(
            "Kernel-density Quantile Delta Mapping output must retain the ",
            "hourly frequency and 3600-second timestep."
        )
    )
}

# Construct the reusable hourly signal with explicit role, variable, frequency,
# intermediate-kind, and output-contract declarations.
kqdm__component <- function() {
    alternatives <- as.list(c(
        KQDM_PUBLISHED_VARIABLES,
        KQDM_EXPERIMENTAL_VARIABLES
    ))
    # Source roles may begin at three- or six-hour resolution because a
    # preprocessing stage produces the hourly intermediate consumed here. The
    # input kind and group kernel enforce the actual hourly boundary.
    requirements <- signal__three_role_requirements(alternatives)
    signal__component(
        name = "kernel_quantile_delta_mapping_hourly",
        label = "Hourly kernel-density Quantile Delta Mapping",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_hourly_series",
        output_kinds = "subdaily_adjusted_series",
        scopes = "univariate",
        stochastic = FALSE,
        profiles = kqdm__profiles(),
        apply_group = kqdm__apply_group,
        operations = list(validate_result = kqdm__validate_result),
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "subdaily_adjusted_series",
            output_frequency = "hour",
            output_step_seconds = 3600,
            references = KQDM_REFERENCES,
            published_method = list(
                cdf = "kernel_density",
                window = "centered_three_months",
                additive_variables = c("tas", "ps"),
                multiplicative_variables = c(
                    "hurs",
                    "sfcWind",
                    "rsds",
                    "rsdsdiff"
                )
            ),
            unreported_numerical_defaults = c(
                "kernel",
                "bandwidth_method",
                "bandwidth_adjust",
                "grid_points",
                "tail_policy",
                "min_samples",
                "bounds",
                "zero_tolerance",
                "zero_denominator_policy"
            )
        )
    )
}

# Register the standalone signal once so recipes and component inspection can
# resolve it without constructing an author-specific complete workflow.
kqdm__register_component <- function() {
    component__register_builtin(kqdm__component())
    invisible(NULL)
}
