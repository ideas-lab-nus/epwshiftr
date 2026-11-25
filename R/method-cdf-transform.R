#' @include signal-adjustment.R distribution-quantile.R
NULL

# Identify the daily Africa application separately because it supplies the
# temporal-window defaults, not the underlying CDF-t transformation.
CDFT_FAMIEN_REFERENCE <- "https://doi.org/10.5194/esd-9-313-2018"

# CDF-t references separate the original transformation, its empirical daily
# application, precipitation SSR, and the method authors' maintained R code.
CDFT_REFERENCES <- c(
    "https://doi.org/10.1029/2009GL038401",
    "https://doi.org/10.5194/nhess-12-2769-2012",
    "https://doi.org/10.1002/2015JD024511",
    CDFT_FAMIEN_REFERENCE,
    "https://CRAN.R-project.org/package=CDFt"
)

# Famien et al. apply CDF-t directly to these six daily climate variables.
CDFT_FAMIEN_VARIABLES <- c(
    "pr",
    "tas",
    "tasmin",
    "tasmax",
    "rsds",
    "sfcWind"
)

# Vrac et al. use 10^-8 mm/s for the precipitation SSR application. CMIP daily
# precipitation flux has the numerically equivalent kg m-2 s-1 unit.
CDFT_PR_SSR_THRESHOLD <- 1e-8

# Construct the tunable settings record shared by the Famien et al. daily
# profiles. Their Africa application supplies the 17-year fitting window and
# central 9-year output block; fixed empirical and edge conventions remain in
# component metadata and output provenance.
# cdft__default_settings {{{
cdft__default_settings <- function(
    bounds,
    distribution_model = c("continuous", "precipitation_ssr"),
    ssr_threshold = 0
) {
    distribution_model <- match.arg(distribution_model)
    list(
        future_window_years = 17L,
        output_block_years = 9L,
        min_samples = 10L,
        target_grid_points = 1000L,
        tail_development_factor = 2,
        bounds = bounds,
        distribution_model = distribution_model,
        ssr_threshold = ssr_threshold,
        random_seed = 1L
    )
}
# }}}

# Build only literature-supported variable profiles; unlike more generic
# quantile methods, no unvalidated variable alternatives are added here.
# cdft__profiles {{{
cdft__profiles <- function() {
    settings <- list(
        pr = cdft__default_settings(
            c(0, Inf),
            "precipitation_ssr",
            CDFT_PR_SSR_THRESHOLD
        ),
        tas = cdft__default_settings(c(-Inf, Inf)),
        tasmin = cdft__default_settings(c(-Inf, Inf)),
        tasmax = cdft__default_settings(c(-Inf, Inf)),
        rsds = cdft__default_settings(c(0, Inf)),
        sfcWind = cdft__default_settings(c(0, Inf))
    )
    signal__build_profiles(
        settings_by_variable = settings,
        published_variables = CDFT_FAMIEN_VARIABLES,
        references = CDFT_REFERENCES,
        method = "cdf_transform",
        output_role = "model_future",
        evidence_mode = "all_published",
        metadata_fn = function(variable, published) {
            list(
                method_settings_source = "method_literature_and_author_implementation",
                temporal_window_source = "famien_2018_application",
                temporal_window_reference = CDFT_FAMIEN_REFERENCE,
                edge_policy_source = "epwshiftr_implementation"
            )
        }
    )
}
# }}}

# Validate all tunable CDF-t settings at the signal-kernel boundary.
# cdft__settings {{{
cdft__settings <- function(settings) {
    expected <- c(
        "future_window_years",
        "output_block_years",
        "min_samples",
        "target_grid_points",
        "tail_development_factor",
        "bounds",
        "distribution_model",
        "ssr_threshold",
        "random_seed"
    )
    resolved <- signal__resolve_settings(settings, expected, "CDF-t")
    resolved <- signal__validate_future_block_window(resolved)
    resolved$min_samples <- signal__integer_setting(
        resolved$min_samples,
        "min_samples",
        lower = 2L
    )
    resolved$target_grid_points <- signal__integer_setting(
        resolved$target_grid_points,
        "target_grid_points",
        lower = 100L
    )
    resolved$tail_development_factor <- signal__positive_number(
        resolved$tail_development_factor,
        "tail_development_factor"
    )
    signal__ordered_bounds(
        resolved$bounds,
        "CDF-t bounds must be ordered from lower to upper."
    )
    checkmate::assert_choice(
        resolved$distribution_model,
        c("continuous", "precipitation_ssr")
    )
    checkmate::assert_number(
        resolved$ssr_threshold,
        lower = 0,
        finite = TRUE
    )
    if (
        identical(resolved$distribution_model, "precipitation_ssr") &&
            resolved$ssr_threshold <= 0
    ) {
        cli::cli_abort(
            "Precipitation CDF-t requires a positive `ssr_threshold`."
        )
    }
    if (
        identical(resolved$distribution_model, "continuous") &&
            resolved$ssr_threshold != 0
    ) {
        cli::cli_abort(
            "Continuous CDF-t requires `ssr_threshold = 0`."
        )
    }
    resolved$random_seed <- signal__random_seed(resolved$random_seed)
    resolved
}
# }}}

# Evaluate the empirical step CDF used by the method authors' implementation.
# cdft__empirical_cdf {{{
cdft__empirical_cdf <- function(sample, values) {
    checkmate::assert_numeric(
        sample,
        min.len = 2L,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_numeric(
        values,
        finite = TRUE,
        any.missing = FALSE
    )
    as.numeric(stats::ecdf(sample)(values))
}
# }}}

# Extend the lower and upper plateaus of the transformed future CDF by the
# constant-correction construction in Michelangeli et al. and the authors' R
# implementation. The reference CDF supplies the added tail shape.
# cdft__extend_target_tails {{{
cdft__extend_target_tails <- function(
    grid,
    probability,
    observed_probability,
    observed,
    future
) {
    extended <- probability
    lower_points <- 0L
    upper_points <- 0L

    if (min(observed) < min(future)) {
        reference_limit <- quantile__inverse_cdf(
            observed,
            extended[[1L]]
        )
        reference_index <- which(grid > reference_limit)[[1L]]
        future_index <- which(grid >= min(future))[[1L]]
        while (future_index > 0L && reference_index > 0L) {
            extended[[future_index]] <- observed_probability[[
                reference_index
            ]]
            lower_points <- lower_points + 1L
            future_index <- future_index - 1L
            reference_index <- reference_index - 1L
        }
        if (future_index > 0L) {
            extended[seq_len(future_index)] <- 0
            lower_points <- lower_points + future_index
        }
    }

    last <- length(grid)
    if (extended[[last]] < 1) {
        reference_limit <- quantile__inverse_cdf(
            observed,
            extended[[last]]
        )
        below <- which(grid < reference_limit)
        reference_index <- if (length(below)) {
            max(below) + 1L
        } else {
            1L
        }
        non_plateau <- which(extended < extended[[last]])
        if (!length(non_plateau)) {
            cli::cli_abort(
                "CDF-t could not resolve the upper target-CDF plateau; increase `tail_development_factor`."
            )
        }
        future_index <- max(non_plateau)
        count <- min(last - future_index, last - reference_index)
        source <- seq.int(reference_index, reference_index + count)
        target <- seq.int(future_index, future_index + count)
        extended[target] <- observed_probability[source]
        upper_points <- length(target)
        endpoint <- future_index + count
        if (endpoint < last) {
            extended[endpoint:last] <- 1
            upper_points <- upper_points + last - endpoint
        }
    }

    # Floating-point and repeated empirical probabilities can introduce tiny
    # downward steps; a cumulative maximum restores the defining CDF property.
    monotone <- cummax(pmin(pmax(extended, 0), 1))
    list(
        probability = monotone,
        lower_extended_points = lower_points,
        upper_extended_points = upper_points,
        monotonicity_repairs = sum(monotone != extended)
    )
}
# }}}

# Estimate F_Sf(x) = F_Sh(F_Gh^-1(F_Gf(x))) on the explicit empirical grid.
# Model historical and future values receive the same observed-minus-historical
# mean shift before the CDFs are fitted, matching the authors' maintained code.
# cdft__target_cdf {{{
cdft__target_cdf <- function(
    observed,
    historical,
    future,
    target_grid_points,
    tail_development_factor
) {
    checkmate::assert_numeric(
        observed,
        min.len = 2L,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_numeric(
        historical,
        min.len = 2L,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_numeric(
        future,
        min.len = 2L,
        finite = TRUE,
        any.missing = FALSE
    )
    shift <- mean(observed) - mean(historical)
    historical_aligned <- historical + shift
    future_aligned <- future + shift
    development <- tail_development_factor *
        abs(mean(future) - mean(historical))
    lower <- min(observed, historical_aligned, future_aligned) -
        development
    upper <- max(observed, historical_aligned, future_aligned) +
        development
    if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
        cli::cli_abort(
            "CDF-t requires a non-degenerate observed, historical, or future fitting range."
        )
    }
    grid <- seq(lower, upper, length.out = target_grid_points)
    future_probability <- cdft__empirical_cdf(
        future_aligned,
        grid
    )
    historical_quantile <- quantile__inverse_cdf(
        historical_aligned,
        future_probability
    )
    target_probability <- cdft__empirical_cdf(
        observed,
        historical_quantile
    )
    observed_probability <- cdft__empirical_cdf(observed, grid)
    tails <- cdft__extend_target_tails(
        grid,
        target_probability,
        observed_probability,
        observed,
        future_aligned
    )
    if (length(unique(tails$probability)) < 2L) {
        cli::cli_abort(
            "CDF-t produced a degenerate future target CDF."
        )
    }
    list(
        grid = grid,
        probability = tails$probability,
        shift = shift,
        historical_aligned = historical_aligned,
        future_aligned = future_aligned,
        diagnostics = list(
            range_alignment_shift = shift,
            target_grid_range = range(grid),
            target_probability_range = range(tails$probability),
            lower_extended_points = tails$lower_extended_points,
            upper_extended_points = tails$upper_extended_points,
            monotonicity_repairs = tails$monotonicity_repairs,
            tied_observed_values = length(observed) -
                length(unique(observed)),
            tied_historical_values = length(historical) -
                length(unique(historical)),
            tied_future_values = length(future) -
                length(unique(future))
        )
    )
}
# }}}

# Generate the corrected future sequence by quantile matching F_Gf to the
# transformed target CDF. The future input order is retained unchanged.
# cdft__map_window {{{
cdft__map_window <- function(observed, historical, future, resolved) {
    target <- cdft__target_cdf(
        observed,
        historical,
        future,
        resolved$target_grid_points,
        resolved$tail_development_factor
    )
    future_probability <- cdft__empirical_cdf(
        target$future_aligned,
        target$future_aligned
    )
    mapped <- stats::approx(
        x = target$probability,
        y = target$grid,
        xout = future_probability,
        method = "linear",
        rule = 2,
        # A CDF quantile is the leftmost x at which the requested probability
        # is attained; choosing the left endpoint also avoids averaging across
        # a flat probability-one tail.
        ties = min
    )$y
    list(
        value = as.numeric(mapped),
        diagnostics = c(
            target$diagnostics,
            list(
                mapped_probability_range = range(future_probability)
            )
        )
    )
}
# }}}

# Replace sub-threshold precipitation singularities once per source role using
# a reproducible method-local generator, leaving R's global RNG untouched.
# cdft__prepared_values {{{
cdft__prepared_values <- function(series, resolved, key, variable) {
    if (
        !identical(
            resolved$distribution_model,
            "precipitation_ssr"
        )
    ) {
        return(list(
            values = lapply(series, `[[`, "value"),
            precipitation = NULL
        ))
    }

    randomized <- signal__randomize_threshold_values(
        series,
        resolved$random_seed,
        key,
        variable,
        resolved$ssr_threshold,
        inclusive = FALSE
    )
    list(
        values = randomized$values,
        precipitation = list(
            ssr_threshold = resolved$ssr_threshold,
            input_randomized_values = randomized$counts,
            random_seed = resolved$random_seed,
            effective_seeds = randomized$seeds,
            random_generator = "park_miller_16807"
        )
    )
}
# }}}

# Record one month/block fit without retaining full empirical arrays in result
# provenance.
# cdft__window_record {{{
cdft__window_record <- function(
    month,
    block,
    observed_samples,
    historical_samples,
    future_samples,
    output_values,
    diagnostics
) {
    c(
        list(
            month = month,
            output_years = range(block$output_years),
            fitting_years = range(block$window_years),
            requested_years = c(
                block$requested_start,
                block$requested_end
            ),
            truncated_left = block$truncated_left,
            truncated_right = block$truncated_right,
            observed_samples = observed_samples,
            historical_samples = historical_samples,
            future_samples = future_samples,
            output_values = output_values
        ),
        diagnostics
    )
}
# }}}

# Apply native-calendar monthly CDF-t fits on the configured temporal policy.
# The defaults reproduce the Famien et al. 17/9-year application schedule, and
# each row is written once even though adjacent fitting windows overlap.
# cdft__adjust_values {{{
cdft__adjust_values <- function(series, resolved, key, variable) {
    prepared <- cdft__prepared_values(
        series,
        resolved,
        key,
        variable
    )
    run <- signal__run_monthly_blocks(
        series = series,
        future_window_years = resolved$future_window_years,
        output_block_years = resolved$output_block_years,
        min_samples = resolved$min_samples,
        bounds = resolved$bounds,
        method_label = "CDF-t",
        apply_window = function(
            month,
            block,
            observed_rows,
            historical_rows,
            future_rows,
            output_rows
        ) {
            transformed <- cdft__map_window(
                prepared$values$observed_reference[observed_rows],
                prepared$values$model_historical[historical_rows],
                prepared$values$model_future[future_rows],
                resolved
            )
            if (!is.null(prepared$precipitation)) {
                recensored <- transformed$value < resolved$ssr_threshold
                transformed$value[recensored] <- 0
                transformed$diagnostics$output_recensored_values <-
                    sum(recensored)
            }
            list(
                value = transformed$value,
                record = cdft__window_record(
                    month,
                    block,
                    length(observed_rows),
                    length(historical_rows),
                    length(future_rows),
                    length(output_rows),
                    transformed$diagnostics
                )
            )
        }
    )
    diagnostics <- run$diagnostics
    if (!is.null(prepared$precipitation)) {
        prepared$precipitation$output_dry_values <- sum(run$value == 0)
        prepared$precipitation$output_positive_below_threshold_values <-
            sum(
                run$value > 0 &
                    run$value < resolved$ssr_threshold
            )
        diagnostics$precipitation <- prepared$precipitation
    }
    list(value = run$value, diagnostics = diagnostics)
}
# }}}

# Execute CDF-t for one aligned univariate signal group and return the common
# future-backbone DailyAdjustedSeries contract.
# cdft__apply_group {{{
cdft__apply_group <- function(inputs, settings, key) {
    resolved <- cdft__settings(settings)
    variable <- names(settings)[[1L]]
    is_ssr <- identical(
        resolved$distribution_model,
        "precipitation_ssr"
    )
    series <- signal__validate_three_role_inputs(
        inputs,
        variable = variable,
        method = "CDF-t",
        min_value = if (is_ssr) 0 else -Inf,
        min_value_message = if (is_ssr) {
            "Precipitation CDF-t requires non-negative input values."
        } else {
            NULL
        }
    )
    mapped <- cdft__adjust_values(
        series,
        resolved,
        key,
        variable
    )
    future <- series$model_future
    future[["value"]] <- mapped$value

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = "cdf_transform",
        settings = resolved,
        provenance = list(
            method = "cdf_transform",
            range_alignment = "additive_mean",
            empirical_conventions = list(
                cdf = "empirical_step",
                inverse_cdf = "linear_type_7",
                ties = "left_endpoint",
                tails = "constant_correction"
            ),
            references = CDFT_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            temporal_policy = list(
                seasonal_grouping = "calendar_month",
                future_window_years = resolved$future_window_years,
                output_block_years = resolved$output_block_years,
                edge_policy = "truncate",
                seasonal_grouping_source = "famien_2018_application",
                window_source = if (
                    resolved$future_window_years == 17L &&
                        resolved$output_block_years == 9L
                ) {
                    "famien_2018_application"
                } else {
                    "user_override"
                },
                window_reference = if (
                    resolved$future_window_years == 17L &&
                        resolved$output_block_years == 9L
                ) {
                    CDFT_FAMIEN_REFERENCE
                } else {
                    NA_character_
                },
                edge_policy_source = "epwshiftr_implementation"
            ),
            diagnostics = mapped$diagnostics
        )
    )
}
# }}}

# Construct the reusable daily CDF-t signal with the six variables used by
# Famien et al. and the package-native three-role contract.
# cdft__component {{{
cdft__component <- function() {
    signal__three_role_component(
        name = "cdf_transform_daily",
        label = "Daily CDF-t",
        variables = CDFT_FAMIEN_VARIABLES,
        profiles = cdft__profiles(),
        apply_group = cdft__apply_group,
        validate_options = cdft__settings,
        method_label = "CDF-t",
        output_role = "model_future",
        stochastic = TRUE,
        metadata = list(
            method_family = "future_target_distribution_mapping",
            output_contract = "daily_adjusted_series",
            references = CDFT_REFERENCES,
            reference_implementation = "CRAN CDFt",
            stochastic_operation = "precipitation_ssr",
            range_alignment = "additive_mean",
            empirical_conventions = list(
                cdf = "empirical_step",
                inverse_cdf = "linear_type_7",
                ties = "left_endpoint",
                tails = "constant_correction"
            ),
            temporal_policy = list(
                seasonal_grouping = "calendar_month",
                seasonal_grouping_source = "famien_2018_application",
                future_window_years = 17L,
                output_block_years = 9L,
                window_source = "famien_2018_application",
                window_reference = CDFT_FAMIEN_REFERENCE,
                edge_policy = "truncate",
                edge_policy_source = "epwshiftr_implementation"
            )
        )
    )
}
# }}}

# Register the native CDF-t component during package load.
# cdft__register_component {{{
cdft__register_component <- function() {
    component__register_builtin(cdft__component())
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
