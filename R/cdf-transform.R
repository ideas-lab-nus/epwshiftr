#' @include bias-adjustment.R quantile-distribution.R
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

# Construct the complete settings record shared by the Famien et al. daily
# profiles. Their Africa application supplies the 17-year fitting window and
# central 9-year output block; edge truncation is an epwshiftr policy because
# that application does not specify how to handle missing boundary flanks.
cdft__default_settings <- function(
  bounds,
  distribution_model = c("continuous", "precipitation_ssr"),
  ssr_threshold = 0
) {
    distribution_model <- match.arg(distribution_model)
    list(
        range_alignment = "additive_mean",
        seasonal_grouping = "calendar_month",
        future_window_years = 17L,
        output_block_years = 9L,
        edge_policy = "truncate",
        min_samples = 10L,
        cdf_method = "empirical_step",
        inverse_cdf_method = "linear_type_7",
        tie_method = "left_endpoint",
        tail_policy = "constant_correction",
        target_grid_points = 1000L,
        tail_development_factor = 2,
        bounds = bounds,
        distribution_model = distribution_model,
        ssr_threshold = ssr_threshold,
        random_seed = 1L
    )
}

# Build only literature-supported variable profiles; unlike more generic
# quantile methods, no unvalidated variable alternatives are added here.
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
    lapply(CDFT_FAMIEN_VARIABLES, function(variable) {
        signal__variable_profile(
            variable,
            settings = settings[[variable]],
            evidence = "published",
            references = CDFT_REFERENCES,
            metadata = list(
                method = "cdf_transform",
                output_role = "model_future",
                method_settings_source =
                    "method_literature_and_author_implementation",
                temporal_window_source = "famien_2018_application",
                temporal_window_reference = CDFT_FAMIEN_REFERENCE,
                edge_policy_source = "epwshiftr_implementation"
            )
        )
    })
}

# Validate every CDF-t numerical convention at the signal-kernel boundary so
# user overrides cannot silently select an unimplemented empirical variant.
cdft__settings <- function(settings) {
    if (length(settings) != 1L ||
        is.null(names(settings)) ||
        !nzchar(names(settings)[[1L]]) ||
        !is.list(settings[[1L]])) {
        cli::cli_abort(
            "CDF-t requires settings for exactly one variable."
        )
    }
    resolved <- settings[[1L]]
    expected <- c(
        "range_alignment",
        "seasonal_grouping",
        "future_window_years",
        "output_block_years",
        "edge_policy",
        "min_samples",
        "cdf_method",
        "inverse_cdf_method",
        "tie_method",
        "tail_policy",
        "target_grid_points",
        "tail_development_factor",
        "bounds",
        "distribution_model",
        "ssr_threshold",
        "random_seed"
    )
    missing <- setdiff(expected, names(resolved))
    unexpected <- setdiff(names(resolved), expected)
    if (length(missing) || length(unexpected)) {
        cli::cli_abort(c(
            "CDF-t settings must use the complete supported schema.",
            "x" = "Missing setting(s): {.val {missing}}.",
            "x" = "Unexpected setting(s): {.val {unexpected}}."
        ))
    }
    if (!identical(resolved$range_alignment, "additive_mean") ||
        !identical(resolved$seasonal_grouping, "calendar_month") ||
        !identical(resolved$edge_policy, "truncate") ||
        !identical(resolved$cdf_method, "empirical_step") ||
        !identical(resolved$inverse_cdf_method, "linear_type_7") ||
        !identical(resolved$tie_method, "left_endpoint") ||
        !identical(resolved$tail_policy, "constant_correction")) {
        cli::cli_abort(
            "CDF-t currently requires additive-mean range alignment, native calendar-month grouping, truncated edge windows, empirical step CDFs, type-7 inverse quantiles, left-endpoint target inversion, and constant-correction tails."
        )
    }
    checkmate::assert_integerish(
        resolved$future_window_years,
        lower = 1L,
        len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_integerish(
        resolved$output_block_years,
        lower = 1L,
        len = 1L,
        any.missing = FALSE
    )
    if (resolved$output_block_years > resolved$future_window_years ||
        (resolved$future_window_years -
            resolved$output_block_years) %% 2L != 0L) {
        cli::cli_abort(
            "`future_window_years` must exceed `output_block_years` by an even, non-negative number of years."
        )
    }
    checkmate::assert_integerish(
        resolved$min_samples,
        lower = 2L,
        len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_integerish(
        resolved$target_grid_points,
        lower = 100L,
        len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_number(
        resolved$tail_development_factor,
        lower = 0,
        finite = TRUE
    )
    if (resolved$tail_development_factor <= 0) {
        cli::cli_abort("`tail_development_factor` must be positive.")
    }
    checkmate::assert_numeric(
        resolved$bounds,
        len = 2L,
        any.missing = FALSE
    )
    if (resolved$bounds[[1L]] > resolved$bounds[[2L]]) {
        cli::cli_abort(
            "CDF-t bounds must be ordered from lower to upper."
        )
    }
    checkmate::assert_choice(
        resolved$distribution_model,
        c("continuous", "precipitation_ssr")
    )
    checkmate::assert_number(
        resolved$ssr_threshold,
        lower = 0,
        finite = TRUE
    )
    if (identical(resolved$distribution_model, "precipitation_ssr") &&
        resolved$ssr_threshold <= 0) {
        cli::cli_abort(
            "Precipitation CDF-t requires a positive `ssr_threshold`."
        )
    }
    if (identical(resolved$distribution_model, "continuous") &&
        resolved$ssr_threshold != 0) {
        cli::cli_abort(
            "Continuous CDF-t requires `ssr_threshold = 0`."
        )
    }
    checkmate::assert_integerish(
        resolved$random_seed,
        lower = 0,
        upper = .Machine$integer.max - 1L,
        len = 1L,
        any.missing = FALSE
    )

    resolved$future_window_years <- as.integer(
        resolved$future_window_years
    )
    resolved$output_block_years <- as.integer(
        resolved$output_block_years
    )
    resolved$min_samples <- as.integer(resolved$min_samples)
    resolved$target_grid_points <- as.integer(
        resolved$target_grid_points
    )
    resolved$random_seed <- as.integer(resolved$random_seed)
    resolved
}

# Validate the three role-addressable daily inputs while retaining every
# source's native CF calendar and future-model row order.
cdft__inputs <- function(inputs, variable, distribution_model) {
    roles <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    if (!identical(sort(names(inputs)), sort(roles))) {
        cli::cli_abort(
            "CDF-t requires observed, historical-model, and future-model role payloads."
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
                "CDF-t role {.val {role}} must contain only variable {.val {variable}}."
            )
        }
        if (length(unique(series[[role]][["cf_calendar"]])) != 1L) {
            cli::cli_abort(
                "CDF-t role {.val {role}} must contain one native calendar per signal group."
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
            "CDF-t inputs for {.val {variable}} must use identical units."
        )
    }
    if (identical(distribution_model, "precipitation_ssr") &&
        any(vapply(
            series,
            function(data) any(data[["value"]] < 0),
            logical(1L)
        ))) {
        cli::cli_abort(
            "Precipitation CDF-t requires non-negative input values."
        )
    }
    series
}

# Partition future years into disjoint output blocks and symmetric fitting
# windows. The Famien et al. defaults use 17/9 years; epwshiftr truncates a
# missing four-year flank when the requested series starts or ends too soon.
cdft__future_blocks <- function(
  year,
  future_window_years,
  output_block_years
) {
    checkmate::assert_integerish(
        year,
        min.len = 1L,
        any.missing = FALSE
    )
    years <- sort(unique(as.integer(year)))
    if (length(years) > 1L && any(diff(years) != 1L)) {
        cli::cli_abort(
            "CDF-t requires contiguous future model years."
        )
    }
    flank <- (future_window_years - output_block_years) %/% 2L
    starts <- seq.int(1L, length(years), by = output_block_years)
    lapply(starts, function(start) {
        stop <- min(start + output_block_years - 1L, length(years))
        output_years <- years[start:stop]
        requested_start <- min(output_years) - flank
        requested_end <- max(output_years) + flank
        window_years <- years[
            years >= requested_start & years <= requested_end
        ]
        list(
            output_years = output_years,
            window_years = window_years,
            requested_start = requested_start,
            requested_end = requested_end,
            truncated_left = min(window_years) > requested_start,
            truncated_right = max(window_years) < requested_end
        )
    })
}

# Evaluate the empirical step CDF used by the method authors' implementation.
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

# Extend the lower and upper plateaus of the transformed future CDF by the
# constant-correction construction in Michelangeli et al. and the authors' R
# implementation. The reference CDF supplies the added tail shape.
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

# Estimate F_Sf(x) = F_Sh(F_Gh^-1(F_Gf(x))) on the explicit empirical grid.
# Model historical and future values receive the same observed-minus-historical
# mean shift before the CDFs are fitted, matching the authors' maintained code.
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

# Generate the corrected future sequence by quantile matching F_Gf to the
# transformed target CDF. The future input order is retained unchanged.
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

# Replace sub-threshold precipitation singularities once per source role using
# a reproducible method-local generator, leaving R's global RNG untouched.
cdft__prepared_values <- function(series, resolved, key, variable) {
    if (!identical(
        resolved$distribution_model,
        "precipitation_ssr"
    )) {
        return(list(
            values = lapply(series, `[[`, "value"),
            precipitation = NULL
        ))
    }

    values <- vector("list", length(series))
    names(values) <- names(series)
    randomized <- integer(length(series))
    names(randomized) <- names(series)
    seeds <- integer(length(series))
    names(seeds) <- names(series)
    for (role in names(series)) {
        role_key <- c(key, list(input_role = role))
        seeds[[role]] <- quantile__group_seed(
            resolved$random_seed,
            role_key,
            variable
        )
        uniform <- quantile__uniform(nrow(series[[role]]), seeds[[role]])
        source <- series[[role]][["value"]]
        singular <- source < resolved$ssr_threshold
        source[singular] <- uniform[singular] *
            resolved$ssr_threshold
        values[[role]] <- source
        randomized[[role]] <- sum(singular)
    }
    list(
        values = values,
        precipitation = list(
            ssr_threshold = resolved$ssr_threshold,
            input_randomized_values = randomized,
            random_seed = resolved$random_seed,
            effective_seeds = seeds,
            random_generator = "park_miller_16807"
        )
    )
}

# Record one month/block fit without retaining full empirical arrays in result
# provenance.
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

# Apply native-calendar monthly CDF-t fits on the configured temporal policy.
# The defaults reproduce the Famien et al. 17/9-year application schedule, and
# each row is written once even though adjacent fitting windows overlap.
cdft__adjust_values <- function(series, resolved, key, variable) {
    observed <- series$observed_reference
    historical <- series$model_historical
    future <- series$model_future
    prepared <- cdft__prepared_values(
        series,
        resolved,
        key,
        variable
    )
    blocks <- cdft__future_blocks(
        future[["cf_year"]],
        resolved$future_window_years,
        resolved$output_block_years
    )
    adjusted <- rep.int(NA_real_, nrow(future))
    records <- list()
    record_index <- 0L

    for (block in blocks) {
        output_year <- future[["cf_year"]] %in% block$output_years
        months <- sort(unique(future[["cf_month"]][output_year]))
        for (month in months) {
            observed_rows <- observed[["cf_month"]] == month
            historical_rows <- historical[["cf_month"]] == month
            future_rows <- future[["cf_month"]] == month &
                future[["cf_year"]] %in% block$window_years
            output_rows <- which(
                future[["cf_month"]] == month & output_year
            )
            observed_values <- prepared$values$observed_reference[
                observed_rows
            ]
            historical_values <- prepared$values$model_historical[
                historical_rows
            ]
            future_values <- prepared$values$model_future[future_rows]
            sample_counts <- c(
                observed = length(observed_values),
                historical = length(historical_values),
                future = length(future_values)
            )
            if (any(sample_counts < resolved$min_samples)) {
                cli::cli_abort(
                    "CDF-t month {month} and output years {min(block$output_years)}-{max(block$output_years)} have fewer than {resolved$min_samples} observed, historical, or future values."
                )
            }
            transformed <- cdft__map_window(
                observed_values,
                historical_values,
                future_values,
                resolved
            )
            if (!is.null(prepared$precipitation)) {
                recensored <- transformed$value <
                    resolved$ssr_threshold
                transformed$value[recensored] <- 0
                transformed$diagnostics$output_recensored_values <-
                    sum(recensored)
            }
            future_window_rows <- which(future_rows)
            output_positions <- match(output_rows, future_window_rows)
            adjusted[output_rows] <- transformed$value[
                output_positions
            ]
            record_index <- record_index + 1L
            records[[record_index]] <- cdft__window_record(
                month,
                block,
                length(observed_values),
                length(historical_values),
                length(future_values),
                length(output_rows),
                transformed$diagnostics
            )
        }
    }
    if (anyNA(adjusted)) {
        cli::cli_abort(
            "CDF-t did not assign every future-model daily row exactly once."
        )
    }
    bounded <- pmin(
        pmax(adjusted, resolved$bounds[[1L]]),
        resolved$bounds[[2L]]
    )
    observed_samples <- vapply(
        records,
        `[[`,
        numeric(1L),
        "observed_samples"
    )
    historical_samples <- vapply(
        records,
        `[[`,
        numeric(1L),
        "historical_samples"
    )
    future_samples <- vapply(
        records,
        `[[`,
        numeric(1L),
        "future_samples"
    )
    diagnostics <- list(
        window_count = length(records),
        observed_window_samples = c(
            minimum = min(observed_samples),
            median = stats::median(observed_samples),
            maximum = max(observed_samples)
        ),
        historical_window_samples = c(
            minimum = min(historical_samples),
            median = stats::median(historical_samples),
            maximum = max(historical_samples)
        ),
        future_window_samples = c(
            minimum = min(future_samples),
            median = stats::median(future_samples),
            maximum = max(future_samples)
        ),
        truncated_edge_windows = sum(vapply(
            records,
            function(record) {
                isTRUE(record$truncated_left) ||
                    isTRUE(record$truncated_right)
            },
            logical(1L)
        )),
        clipped_values = sum(bounded != adjusted),
        windows = records
    )
    if (!is.null(prepared$precipitation)) {
        prepared$precipitation$output_dry_values <- sum(bounded == 0)
        prepared$precipitation$output_positive_below_threshold_values <-
            sum(
                bounded > 0 &
                    bounded < resolved$ssr_threshold
            )
        diagnostics$precipitation <- prepared$precipitation
    }
    list(value = bounded, diagnostics = diagnostics)
}

# Execute CDF-t for one aligned univariate signal group and return the common
# future-backbone DailyAdjustedSeries contract.
cdft__apply_group <- function(inputs, settings, key) {
    resolved <- cdft__settings(settings)
    variable <- names(settings)[[1L]]
    series <- cdft__inputs(
        inputs,
        variable,
        resolved$distribution_model
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
            references = CDFT_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            temporal_policy = list(
                seasonal_grouping = "calendar_month",
                future_window_years = resolved$future_window_years,
                output_block_years = resolved$output_block_years,
                edge_policy = resolved$edge_policy,
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

# Return one explicit diagnostic string when CDF-t violates the package-native
# future-model output contract.
cdft__validate_result <- function(value, inputs, key) {
    if (!S7::S7_inherits(value, DailyAdjustedSeries)) {
        return("CDF-t must return a DailyAdjustedSeries object.")
    }
    if (!identical(value@output_role, "model_future")) {
        return("CDF-t output must retain the `model_future` role.")
    }
    TRUE
}

# Construct the reusable daily CDF-t signal with the six variables used by
# Famien et al. and the package-native three-role contract.
cdft__component <- function() {
    alternatives <- as.list(CDFT_FAMIEN_VARIABLES)
    roles <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    requirements <- lapply(roles, function(role) {
        component__input_requirement(
            role,
            representations = "series",
            frequencies = "day",
            variable_sets = alternatives
        )
    })
    names(requirements) <- roles
    signal__component(
        name = "cdf_transform_daily",
        label = "Daily CDF-t",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = TRUE,
        profiles = cdft__profiles(),
        apply_group = cdft__apply_group,
        operations = list(validate_result = cdft__validate_result),
        metadata = list(
            method_family = "future_target_distribution_mapping",
            output_contract = "daily_adjusted_series",
            references = CDFT_REFERENCES,
            reference_implementation = "CRAN CDFt",
            stochastic_operation = "precipitation_ssr",
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

# Register the native CDF-t component during package load.
cdft__register_component <- function() {
    component__register(cdft__component(), overwrite = TRUE)
}
