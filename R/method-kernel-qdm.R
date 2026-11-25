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

# Construct the complete tunable settings schema while keeping fixed published
# conventions (centered three-month window and kernel-density CDF) in component
# metadata and result provenance.
# kqdm__default_settings {{{
kqdm__default_settings <- function(
    transformation = c("additive", "multiplicative"),
    bounds
) {
    transformation <- match.arg(transformation)
    list(
        transformation = transformation,
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
# }}}

# Build variable-specific profiles. Every profile is labelled experimental
# because the publication does not report the kernel, bandwidth, grid, tail,
# or zero-denominator conventions needed for an executable implementation.
# kqdm__profiles {{{
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
    settings <- lapply(specifications, function(specification) {
        kqdm__default_settings(specification[[1L]], specification[[2L]])
    })
    signal__build_profiles(
        settings_by_variable = settings,
        published_variables = KQDM_PUBLISHED_VARIABLES,
        references = KQDM_REFERENCES,
        method = "kernel_quantile_delta_mapping",
        output_role = "model_future",
        evidence_mode = "all_experimental",
        metadata_fn = function(variable, published) {
            list(
                variable_evidence = if (published) {
                    "method_literature"
                } else {
                    "package_extension"
                },
                published_settings = "transformation",
                published_method = list(
                    cdf_method = "kernel_density",
                    window_months = 3L,
                    window_alignment = "centered"
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
        }
    )
}
# }}}

# Validate every executable convention at the signal boundary so user
# overrides cannot silently introduce an unsupported numerical method.
# kqdm__settings {{{
kqdm__settings <- function(settings) {
    expected <- c(
        "transformation",
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
    resolved$bandwidth_adjust <- signal__positive_number(
        resolved$bandwidth_adjust,
        "bandwidth_adjust"
    )
    resolved$grid_points <- signal__integer_setting(
        resolved$grid_points,
        "grid_points",
        lower = 128L
    )
    if (
        abs(log2(resolved$grid_points) - round(log2(resolved$grid_points))) >
            sqrt(.Machine$double.eps)
    ) {
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
    resolved
}
# }}}

# Return the previous, current, and following calendar months with December-to-
# January wrapping independent of the number of days in either source calendar.
# kqdm__window_months {{{
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
# }}}

# Estimate a kernel-density CDF on an explicit grid. Both forward and inverse
# grids are retained so a saved mapping can be evaluated without refitting.
# kqdm__density_cdf {{{
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
    increments <- diff(estimate$x) *
        (head(estimate$y, -1L) + tail(estimate$y, -1L)) /
        2
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
# }}}

# Evaluate one fitted KDE CDF with the declared endpoint clamp and report use
# of the finite density-grid tails for diagnostics.
# kqdm__cdf {{{
kqdm__cdf <- function(distribution, values, tail_policy) {
    lower_tail <- values < distribution$x[[1L]]
    upper_tail <- values > distribution$x[[length(distribution$x)]]
    if (
        identical(tail_policy, "error") &&
            any(lower_tail | upper_tail)
    ) {
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
# }}}

# Numerically invert one monotone KDE CDF grid using the same endpoint clamp as
# forward evaluation so tail behavior stays explicit and deterministic.
# kqdm__inverse_cdf {{{
kqdm__inverse_cdf <- function(distribution, probability) {
    stats::approx(
        distribution$inverse_probability,
        distribution$inverse_x,
        xout = probability,
        rule = 2,
        ties = "ordered"
    )$y
}
# }}}

# Apply the additive or multiplicative QDM equation to every future value in
# one calendar month after fitting all three role distributions.
# kqdm__map_values {{{
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
        zero_denominator_used <- rep(FALSE, length(future_values))
    } else {
        denominator_invalid <- (historical_quantile <= resolved$zero_tolerance)
        future_zero <- abs(future_values) <= resolved$zero_tolerance
        undefined <- denominator_invalid &
            (!future_zero |
                identical(resolved$zero_denominator_policy, "error"))
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
        zero_denominator_used <- denominator_invalid & future_zero
    }
    list(
        value = adjusted,
        probability = probability,
        change = change,
        observed_quantile = observed_quantile,
        historical_quantile = historical_quantile,
        zero_denominator_used = zero_denominator_used,
        lower_tail = future_cdf$lower_tail,
        upper_tail = future_cdf$upper_tail,
        zero_future_values = zero_future_values
    )
}
# }}}

# Fit centered three-month KDEs independently for each output month, then map
# all rows while retaining the original future-model ordering and coordinates.
# kqdm__adjust_values {{{
kqdm__adjust_values <- function(series, resolved) {
    observed <- series$observed_reference
    historical <- series$model_historical
    future <- series$model_future
    adjusted <- rep.int(NA_real_, nrow(future))
    diagnostics <- vector("list", length(unique(future[["cf_month"]])))
    center_months <- sort(unique(future[["cf_month"]]))
    fits <- vector("list", length(center_months))
    names(fits) <- as.character(center_months)
    # Row indices refer to the adjusted series in its original future order.
    # Native CF coordinates avoid silently turning a 360-day date into UTC.
    coordinates <- intersect(
        c(
            "variable_id",
            "cf_calendar",
            "cf_year",
            "cf_month",
            "cf_day",
            "cf_second_of_day"
        ),
        names(future)
    )
    mapping <- data.table::as.data.table(future)[,
        coordinates,
        with = FALSE
    ]
    data.table::set(mapping, j = "row_index", value = seq_len(nrow(future)))
    data.table::set(mapping, j = "input_value", value = future[["value"]])
    for (name in c(
        "probability",
        "observed_quantile",
        "historical_quantile",
        "change"
    )) {
        data.table::set(mapping, j = name, value = rep(NA_real_, nrow(future)))
    }
    for (name in c("lower_tail", "upper_tail", "zero_denominator_used")) {
        data.table::set(mapping, j = name, value = rep(FALSE, nrow(future)))
    }

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
        fits[[month_index]] <- list(
            center_month = center_month,
            window_months = window_months,
            observed_reference = observed_distribution,
            model_historical = historical_distribution,
            model_future = future_distribution
        )
        for (name in c(
            "probability",
            "observed_quantile",
            "historical_quantile",
            "change",
            "lower_tail",
            "upper_tail",
            "zero_denominator_used"
        )) {
            data.table::set(
                mapping,
                i = output_index,
                j = name,
                value = mapped[[name]]
            )
        }
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
    data.table::set(mapping, j = "unbounded_value", value = adjusted)
    data.table::set(mapping, j = "adjusted_value", value = bounded)
    data.table::set(
        mapping,
        j = "bounds_correction",
        value = bounded - adjusted
    )
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
    month_diagnostics$maximum_bounds_correction <- vapply(
        month_diagnostics$center_month,
        function(month) {
            max(abs(mapping$bounds_correction[future[["cf_month"]] == month]))
        },
        numeric(1L)
    )
    list(
        value = bounded,
        mapping = list(distributions = fits, rows = mapping),
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
# }}}

# Execute one aligned hourly univariate group and return the common
# frequency-aware adjusted-series contract with full numerical provenance.
# kqdm__apply_group {{{
kqdm__apply_group <- function(inputs, settings, key) {
    resolved <- kqdm__settings(settings)
    variable <- names(settings)[[1L]]
    is_multiplicative <- identical(resolved$transformation, "multiplicative")
    series <- signal__validate_three_role_inputs(
        inputs,
        variable = variable,
        method = "Kernel-density Quantile Delta Mapping",
        frequency = "hour",
        time_step_seconds = 3600,
        min_value = if (is_multiplicative) 0 else -Inf,
        min_value_message = if (is_multiplicative) {
            "Multiplicative kernel-density Quantile Delta Mapping requires non-negative input values."
        } else {
            NULL
        }
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
            diagnostics = mapped$diagnostics,
            mapping = mapped$mapping
        )
    )
}
# }}}

# Construct the reusable hourly signal with explicit role, variable, frequency,
# intermediate-kind, and output-contract declarations.
# kqdm__component {{{
kqdm__component <- function() {
    # Source roles may begin at three- or six-hour resolution because a
    # preprocessing stage produces the hourly intermediate consumed here. The
    # input kind and group kernel enforce the actual hourly boundary.
    signal__three_role_component(
        name = "kernel_quantile_delta_mapping_hourly",
        label = "Hourly kernel-density Quantile Delta Mapping",
        variables = c(
            KQDM_PUBLISHED_VARIABLES,
            KQDM_EXPERIMENTAL_VARIABLES
        ),
        profiles = kqdm__profiles(),
        apply_group = kqdm__apply_group,
        validate_options = kqdm__settings,
        method_label = "Kernel-density Quantile Delta Mapping",
        output_role = "model_future",
        frequency = "hour",
        time_step_seconds = 3600,
        require_frequencies = character(),
        input_kinds = "calendar_indexed_hourly_series",
        output_kinds = "subdaily_adjusted_series",
        temporal_message = paste0(
            "Kernel-density Quantile Delta Mapping output must retain the ",
            "hourly frequency and 3600-second timestep."
        ),
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "subdaily_adjusted_series",
            output_frequency = "hour",
            output_step_seconds = 3600,
            references = KQDM_REFERENCES,
            published_method = list(
                cdf = "kernel_density",
                window = "centered_three_months",
                window_months = 3L,
                window_alignment = "centered",
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
# }}}

# Register the standalone signal once so recipes and component inspection can
# resolve it without constructing an author-specific complete workflow.
# kqdm__register_component {{{
kqdm__register_component <- function() {
    component__register_builtin(kqdm__component())
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
