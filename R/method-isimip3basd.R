#' @include signal-adjustment.R calendar-daily.R distribution-parametric.R distribution-quantile.R
NULL

# ISIMIP3BASD settings follow the trend-preserving bias-adjustment method and
# the official 3.0.x application configuration. The R implementation below is
# independent and uses only the published equations and configuration contract.
ISIMIP_REFERENCES <- c(
    "https://doi.org/10.5194/gmd-12-3055-2019",
    "https://doi.org/10.5281/zenodo.7151476"
)

# The component exposes every marginal variable in the official application.
# Variables reconstructed from these components belong to the later physics
# stage and are therefore not adjusted independently here.
ISIMIP_DIRECT_VARIABLES <- c(
    "hurs",
    "pr",
    "ps",
    "psl",
    "rlds",
    "rsds",
    "sfcWind",
    "tas"
)
ISIMIP_DERIVED_VARIABLES <- c(
    "prsnratio",
    "tasrange",
    "tasskew"
)
ISIMIP_VARIABLES <- c(
    ISIMIP_DIRECT_VARIABLES,
    ISIMIP_DERIVED_VARIABLES
)

# The published 0.1 mm/day precipitation threshold is represented in the
# native CMIP flux unit kg m-2 s-1.
ISIMIP_PR_THRESHOLD <- 0.1 / 86400

# Construct the complete settings schema shared by every variable profile.
# Variable-specific arguments select only method choices documented by the
# ISIMIP3BASD application rather than inventing implicit transformations.
isimip__default_settings <- function(
  mapping_model,
  trend_preservation,
  bounds = c(-Inf, Inf),
  lower_threshold = NULL,
  upper_threshold = NULL,
  unconditional_change_transfer = FALSE,
  trendless_bound_frequency = FALSE,
  detrend = FALSE,
  impute_missing = FALSE,
  all_missing_fallback = NULL,
  scale_by_upper_bound_cycle = FALSE
) {
    list(
        method_version = "3.0.x",
        seasonal_grouping = "circular_running_window",
        running_window_days = 31L,
        running_window_step_days = 1L,
        target_year_days = 365L,
        n_quantiles = 50L,
        mapping_model = mapping_model,
        trend_preservation = trend_preservation,
        bounds = bounds,
        lower_threshold = lower_threshold,
        upper_threshold = upper_threshold,
        unconditional_change_transfer =
            unconditional_change_transfer,
        trendless_bound_frequency = trendless_bound_frequency,
        detrend = detrend,
        detrend_alpha = 0.05,
        impute_missing = impute_missing,
        all_missing_fallback = all_missing_fallback,
        scale_by_upper_bound_cycle = scale_by_upper_bound_cycle,
        upper_bound_window_days = 31L,
        adjust_event_likelihood = FALSE,
        cdf_epsilon = 1e-10,
        max_change_factor = 100,
        max_adjustment_factor = 9,
        ks_threshold = 0.5,
        min_samples = 10L,
        random_seed = 0L,
        fit_tolerance = 1e-10,
        fit_max_iterations = 1000L
    )
}

# Build publication-backed profiles for direct and component variables. The
# metadata distinguishes direct CMIP variables from quantities that must later
# be recombined into physically related output variables.
isimip__profiles <- function() {
    settings <- list(
        hurs = isimip__default_settings(
            "empirical",
            "bounded",
            c(0, 100),
            0.01,
            99.99,
            unconditional_change_transfer = TRUE,
            trendless_bound_frequency = TRUE
        ),
        pr = isimip__default_settings(
            "gamma",
            "mixed",
            c(0, Inf),
            ISIMIP_PR_THRESHOLD
        ),
        ps = isimip__default_settings(
            "normal",
            "additive",
            detrend = TRUE
        ),
        psl = isimip__default_settings(
            "normal",
            "additive",
            detrend = TRUE
        ),
        rlds = isimip__default_settings(
            "normal",
            "additive",
            detrend = TRUE
        ),
        rsds = isimip__default_settings(
            "empirical",
            "bounded",
            c(0, 1),
            0.0001,
            0.9999,
            scale_by_upper_bound_cycle = TRUE
        ),
        sfcWind = isimip__default_settings(
            "weibull",
            "mixed",
            c(0, Inf),
            0.01
        ),
        tas = isimip__default_settings(
            "normal",
            "additive",
            detrend = TRUE
        ),
        prsnratio = isimip__default_settings(
            "empirical",
            "bounded",
            c(0, 1),
            0.0001,
            0.9999,
            impute_missing = TRUE,
            all_missing_fallback = 0
        ),
        tasrange = isimip__default_settings(
            "weibull",
            "mixed",
            c(0, Inf),
            0.01
        ),
        tasskew = isimip__default_settings(
            "empirical",
            "bounded",
            c(0, 1),
            0.0001,
            0.9999
        )
    )
    reconstructed <- list(
        prsnratio = "prsn",
        tasrange = c("tasmin", "tasmax"),
        tasskew = c("tasmin", "tasmax")
    )

    lapply(ISIMIP_VARIABLES, function(variable) {
        derived <- variable %in% ISIMIP_DERIVED_VARIABLES
        signal__variable_profile(
            variable,
            settings = settings[[variable]],
            evidence = "published",
            references = ISIMIP_REFERENCES,
            metadata = list(
                method = "isimip3basd",
                method_version = "3.0.x",
                output_role = "model_future",
                variable_role = if (derived) "derived_component" else "direct",
                reconstructed_outputs = reconstructed[[variable]],
                mapping_domain = if (identical(variable, "rsds")) {
                    "upper_bound_fraction"
                } else {
                    "native_units"
                },
                spatial_downscaling = "separate_stage"
            )
        )
    })
}

# Validate the complete method schema so partial overrides cannot silently
# change the official application semantics or create invalid threshold pairs.
isimip__settings <- function(settings) {
    expected <- c(
        "method_version",
        "seasonal_grouping",
        "running_window_days",
        "running_window_step_days",
        "target_year_days",
        "n_quantiles",
        "mapping_model",
        "trend_preservation",
        "bounds",
        "lower_threshold",
        "upper_threshold",
        "unconditional_change_transfer",
        "trendless_bound_frequency",
        "detrend",
        "detrend_alpha",
        "impute_missing",
        "all_missing_fallback",
        "scale_by_upper_bound_cycle",
        "upper_bound_window_days",
        "adjust_event_likelihood",
        "cdf_epsilon",
        "max_change_factor",
        "max_adjustment_factor",
        "ks_threshold",
        "min_samples",
        "random_seed",
        "fit_tolerance",
        "fit_max_iterations"
    )
    resolved <- signal__resolve_settings(
        settings,
        expected,
        "ISIMIP3BASD"
    )
    if (!identical(resolved$method_version, "3.0.x") ||
        !identical(
            resolved$seasonal_grouping,
            "circular_running_window"
        ) ||
        !identical(resolved$running_window_step_days, 1L) ||
        !identical(resolved$adjust_event_likelihood, FALSE)) {
        cli::cli_abort(
            "ISIMIP3BASD currently implements the 3.0.x one-day circular-window application without event-likelihood adjustment."
        )
    }
    resolved$running_window_days <- signal__integer_setting(
        resolved$running_window_days,
        "running_window_days",
        lower = 1L
    )
    resolved$target_year_days <- signal__integer_setting(
        resolved$target_year_days,
        "target_year_days",
        lower = 3L
    )
    resolved$upper_bound_window_days <- signal__integer_setting(
        resolved$upper_bound_window_days,
        "upper_bound_window_days",
        lower = 1L
    )
    daily__window_spec(
        resolved$running_window_days,
        resolved$target_year_days
    )
    daily__window_spec(
        resolved$upper_bound_window_days,
        resolved$target_year_days
    )
    resolved$n_quantiles <- signal__integer_setting(
        resolved$n_quantiles,
        "n_quantiles",
        lower = 1L
    )
    resolved$min_samples <- signal__integer_setting(
        resolved$min_samples,
        "min_samples",
        lower = 2L
    )
    checkmate::assert_choice(
        resolved$mapping_model,
        c("empirical", "normal", "gamma", "weibull")
    )
    checkmate::assert_choice(
        resolved$trend_preservation,
        c("additive", "multiplicative", "mixed", "bounded")
    )
    signal__ordered_bounds(
        resolved$bounds,
        "ISIMIP3BASD bounds must be strictly ordered.",
        strict = TRUE
    )

    lower <- resolved$lower_threshold
    upper <- resolved$upper_threshold
    if (!is.null(lower)) {
        checkmate::assert_number(lower, finite = TRUE)
        if (!is.finite(resolved$bounds[[1L]]) ||
            lower <= resolved$bounds[[1L]] ||
            lower >= resolved$bounds[[2L]]) {
            cli::cli_abort(
                "`lower_threshold` must lie strictly above a finite lower bound."
            )
        }
    }
    if (!is.null(upper)) {
        checkmate::assert_number(upper, finite = TRUE)
        if (!is.finite(resolved$bounds[[2L]]) ||
            upper >= resolved$bounds[[2L]] ||
            upper <= resolved$bounds[[1L]]) {
            cli::cli_abort(
                "`upper_threshold` must lie strictly below a finite upper bound."
            )
        }
    }
    if (!is.null(lower) && !is.null(upper) && lower >= upper) {
        cli::cli_abort(
            "ISIMIP3BASD lower and upper thresholds must be ordered."
        )
    }
    if (identical(resolved$trend_preservation, "bounded") &&
        (!is.finite(resolved$bounds[[1L]]) ||
            !is.finite(resolved$bounds[[2L]]))) {
        cli::cli_abort(
            "Bounded ISIMIP3BASD transfer requires two finite bounds."
        )
    }
    if (identical(resolved$mapping_model, "gamma") &&
        is.null(lower)) {
        cli::cli_abort(
            "Gamma ISIMIP3BASD mapping requires a lower threshold."
        )
    }
    if (identical(resolved$mapping_model, "weibull") &&
        is.null(lower)) {
        cli::cli_abort(
            "Weibull ISIMIP3BASD mapping requires a lower threshold."
        )
    }
    for (name in c(
        "unconditional_change_transfer",
        "trendless_bound_frequency",
        "detrend",
        "impute_missing",
        "scale_by_upper_bound_cycle"
    )) {
        checkmate::assert_flag(resolved[[name]])
    }
    checkmate::assert_number(
        resolved$detrend_alpha,
        lower = 0,
        upper = 1,
        finite = TRUE
    )
    if (resolved$detrend_alpha <= 0 ||
        resolved$detrend_alpha >= 1) {
        cli::cli_abort("`detrend_alpha` must lie strictly between zero and one.")
    }
    if (!resolved$impute_missing &&
        !is.null(resolved$all_missing_fallback)) {
        cli::cli_abort(
            "`all_missing_fallback` requires `impute_missing = TRUE`."
        )
    }
    if (!is.null(resolved$all_missing_fallback)) {
        checkmate::assert_number(
            resolved$all_missing_fallback,
            finite = TRUE
        )
    }
    for (name in c(
        "cdf_epsilon",
        "fit_tolerance"
    )) {
        checkmate::assert_number(
            resolved[[name]],
            lower = 0,
            finite = TRUE
        )
        if (resolved[[name]] <= 0) {
            cli::cli_abort("ISIMIP3BASD {.arg {name}} must be positive.")
        }
    }
    if (resolved$cdf_epsilon >= 0.5) {
        cli::cli_abort("`cdf_epsilon` must be smaller than 0.5.")
    }
    checkmate::assert_number(
        resolved$max_change_factor,
        lower = 1,
        finite = TRUE
    )
    checkmate::assert_number(
        resolved$max_adjustment_factor,
        lower = 1,
        finite = TRUE
    )
    if (resolved$max_change_factor <= 1 ||
        resolved$max_adjustment_factor <= 1) {
        cli::cli_abort(
            "ISIMIP3BASD change and adjustment factors must exceed one."
        )
    }
    checkmate::assert_number(
        resolved$ks_threshold,
        lower = 0,
        upper = 1,
        finite = TRUE
    )
    resolved$random_seed <- signal__random_seed(resolved$random_seed)
    resolved$fit_max_iterations <- signal__integer_setting(
        resolved$fit_max_iterations,
        "fit_max_iterations",
        lower = 1L
    )
    resolved
}

# Validate one input while temporarily replacing permitted missing values only
# for the common structural validator. The original missing mask is restored
# before any method calculation is performed.
isimip__input_table <- function(data, role, allow_missing) {
    if (!is.data.frame(data)) {
        cli::cli_abort(
            "ISIMIP3BASD role {.val {role}} must be a canonical daily data frame."
        )
    }
    out <- as.data.frame(data, stringsAsFactors = FALSE)
    if (!"value" %in% names(out) || !is.numeric(out[["value"]])) {
        cli::cli_abort(
            "ISIMIP3BASD role {.val {role}} must contain a numeric `value` column."
        )
    }
    missing <- is.na(out[["value"]])
    if (any(missing) && !allow_missing) {
        cli::cli_abort(
            "ISIMIP3BASD role {.val {role}} contains missing values for a variable without published imputation."
        )
    }
    if (any(!is.finite(out[["value"]][!missing]))) {
        cli::cli_abort(
            "ISIMIP3BASD role {.val {role}} contains non-finite values."
        )
    }
    checked <- out
    checked[["value"]][missing] <- 0
    checked <- bias__daily_table(checked, role)
    checked[["value"]][missing] <- NA_real_
    checked
}

# Resolve the three role-addressable daily inputs and retain their independent
# native calendars. Only the snow-ratio component admits missing observations,
# matching the published imputation step.
isimip__inputs <- function(inputs, variable, resolved) {
    roles <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    if (!is.list(inputs) ||
        !identical(sort(names(inputs)), sort(roles))) {
        cli::cli_abort(
            "ISIMIP3BASD requires observed, historical-model, and future-model role payloads."
        )
    }
    series <- lapply(roles, function(role) {
        isimip__input_table(
            inputs[[role]],
            role,
            allow_missing = resolved$impute_missing
        )
    })
    names(series) <- roles
    for (role in roles) {
        role_variables <- unique(series[[role]][["variable_id"]])
        if (!identical(role_variables, variable)) {
            cli::cli_abort(
                "ISIMIP3BASD role {.val {role}} must contain only variable {.val {variable}}."
            )
        }
        if (length(unique(series[[role]][["cf_calendar"]])) != 1L) {
            cli::cli_abort(
                "ISIMIP3BASD role {.val {role}} must contain one native calendar per signal group."
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
            "ISIMIP3BASD inputs for {.val {variable}} must use identical units."
        )
    }
    if (is.finite(resolved$bounds[[1L]]) &&
        any(vapply(series, function(data) {
            any(data[["value"]] < resolved$bounds[[1L]], na.rm = TRUE)
        }, logical(1L)))) {
        cli::cli_abort(
            "ISIMIP3BASD inputs for {.val {variable}} contain values below the declared physical lower bound."
        )
    }
    series
}

# Map a native-calendar annual phase to the common target day without pairing
# nominal dates across 360-, 365-, and 366-day source calendars.
isimip__target_day <- function(annual_phase, target_year_days) {
    daily__target_day(annual_phase, target_year_days)
}

# Derive a deterministic seed for one operation and window. Method-local
# Park-Miller uniforms then leave R's global random-number state untouched.
isimip__seed <- function(resolved, key, variable, operation, target_day = 0L) {
    quantile__group_seed(
        resolved$random_seed,
        c(
            key,
            list(
                operation = operation,
                target_day = as.integer(target_day)
            )
        ),
        variable
    )
}

# Fill missing target-day climatology values through circular linear
# interpolation so a 360-day input can participate on the common 365-day grid
# without duplicating or discarding native dates.
isimip__circular_fill <- function(values) {
    checkmate::assert_numeric(
        values,
        min.len = 3L,
        finite = TRUE,
        any.missing = TRUE
    )
    valid <- which(!is.na(values))
    if (!length(valid)) {
        cli::cli_abort(
            "ISIMIP3BASD cannot interpolate an empty annual cycle."
        )
    }
    if (length(valid) == 1L) {
        return(rep.int(values[[valid]], length(values)))
    }
    n <- length(values)
    daily__circular_interpolate(
        valid,
        values[valid],
        seq_len(n),
        period = n
    )
}

# Calculate a circular running statistic at every target day using an odd
# target-grid window. This helper is deliberately value-agnostic so both the
# running maximum and subsequent running mean remain directly testable.
isimip__circular_running <- function(
  values,
  window_days,
  statistic = c("mean", "max")
) {
    statistic <- match.arg(statistic)
    spec <- daily__window_spec(window_days, length(values))
    half <- spec$window_days %/% 2L
    offsets <- seq.int(-half, half)
    out <- numeric(length(values))
    for (day in seq_along(values)) {
        index <- ((day - 1L + offsets) %% length(values)) + 1L
        out[[day]] <- if (identical(statistic, "mean")) {
            mean(values[index])
        } else {
            max(values[index])
        }
    }
    out
}

# Estimate the smoothed annual upper-bound cycle used to normalize short-wave
# radiation. A daily maximum is followed by a circular running maximum and
# running mean, matching the published two-stage upper-bound construction.
isimip__upper_bound_cycle <- function(
  data,
  window_days,
  target_year_days
) {
    target_day <- isimip__target_day(
        data[["annual_phase"]],
        target_year_days
    )
    daily_max <- tapply(
        data[["value"]],
        target_day,
        max,
        na.rm = TRUE
    )
    cycle <- rep.int(NA_real_, target_year_days)
    cycle[as.integer(names(daily_max))] <- unname(daily_max)
    missing_days <- which(is.na(cycle))
    cycle <- isimip__circular_fill(cycle)
    running_max <- isimip__circular_running(
        cycle,
        window_days,
        "max"
    )
    upper_bound <- isimip__circular_running(
        running_max,
        window_days,
        "mean"
    )
    if (any(!is.finite(upper_bound)) || any(upper_bound <= 0)) {
        cli::cli_abort(
            "ISIMIP3BASD short-wave upper-bound climatology must be positive."
        )
    }
    list(
        value = upper_bound,
        missing_target_days = as.integer(missing_days),
        source_range = range(cycle),
        upper_bound_range = range(upper_bound)
    )
}

# Scale all roles to a dimensionless short-wave fraction and transfer the
# simulated upper-bound-cycle change to the observed cycle. The target cycle is
# clipped to a positive numerical floor before final rescaling.
isimip__scale_upper_bounds <- function(series, resolved) {
    cycles <- lapply(series, function(data) {
        isimip__upper_bound_cycle(
            data,
            resolved$upper_bound_window_days,
            resolved$target_year_days
        )
    })
    scaled <- Map(function(data, cycle) {
        day <- isimip__target_day(
            data[["annual_phase"]],
            resolved$target_year_days
        )
        out <- data
        out[["value"]] <- data[["value"]] / cycle$value[day]
        out[["value"]] <- pmin(pmax(out[["value"]], 0), 1)
        out
    }, series, cycles)
    names(scaled) <- names(series)

    observed <- cycles$observed_reference$value
    historical <- cycles$model_historical$value
    future <- cycles$model_future$value
    ratio <- future / historical
    invalid <- !is.finite(ratio) | historical <= 0
    ratio[invalid] <- 1
    ratio <- pmin(
        pmax(ratio, 1 / resolved$max_change_factor),
        resolved$max_change_factor
    )
    target <- pmax(
        observed * ratio,
        sqrt(.Machine$double.eps)
    )
    list(
        series = scaled,
        target = target,
        diagnostics = list(
            role_cycles = lapply(cycles, function(cycle) {
                cycle[c(
                    "missing_target_days",
                    "source_range",
                    "upper_bound_range"
                )]
            }),
            invalid_change_days = as.integer(which(invalid)),
            target_upper_bound_range = range(target)
        )
    )
}

# Impute published snow-ratio missing values from the empirical distribution of
# available values. The deterministic generator records both the number filled
# and whether the all-missing fallback was needed.
isimip__impute_missing <- function(values, seed, fallback = NULL) {
    missing <- is.na(values)
    if (!any(missing)) {
        return(list(
            value = values,
            missing = 0L,
            all_missing_fallback = FALSE,
            seed = as.integer(seed)
        ))
    }
    available <- values[!missing]
    used_fallback <- !length(available)
    if (used_fallback) {
        if (is.null(fallback)) {
            cli::cli_abort(
                "ISIMIP3BASD cannot impute an entirely missing sample without a fallback."
            )
        }
        values[missing] <- fallback
    } else {
        probability <- quantile__uniform(sum(missing), seed)
        values[missing] <- quantile__inverse_cdf(
            available,
            probability
        )
    }
    list(
        value = values,
        missing = as.integer(sum(missing)),
        all_missing_fallback = used_fallback,
        seed = as.integer(seed)
    )
}

# Remove a statistically detectable linear trend in annual means while
# retaining the centered trend term needed to restore the future sequence.
isimip__detrend <- function(values, years, alpha) {
    checkmate::assert_numeric(
        values,
        min.len = 2L,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_integerish(
        years,
        len = length(values),
        any.missing = FALSE
    )
    annual <- tapply(values, years, mean)
    annual_year <- as.numeric(names(annual))
    if (length(annual) < 3L || length(unique(annual)) < 2L) {
        return(list(
            value = values,
            trend = rep.int(0, length(values)),
            slope = 0,
            p_value = 1,
            applied = FALSE
        ))
    }
    centered_year <- annual_year - mean(annual_year)
    design <- cbind(1, centered_year)
    fit <- stats::lm.fit(design, as.numeric(annual))
    residual_df <- length(annual) - fit$rank
    slope <- unname(fit$coefficients[[2L]])
    if (residual_df <= 0L || !is.finite(slope)) {
        return(list(
            value = values,
            trend = rep.int(0, length(values)),
            slope = 0,
            p_value = 1,
            applied = FALSE
        ))
    }
    residual_variance <- sum(fit$residuals^2) / residual_df
    standard_error <- sqrt(
        residual_variance /
            sum(centered_year^2)
    )
    p_value <- if (!is.finite(standard_error) ||
        standard_error <= 0) {
        if (slope == 0) 1 else 0
    } else {
        2 * stats::pt(
            -abs(slope / standard_error),
            df = residual_df
        )
    }
    applied <- is.finite(p_value) && p_value < alpha
    row_trend <- if (applied) {
        slope * (as.numeric(years) - mean(annual_year))
    } else {
        rep.int(0, length(values))
    }
    list(
        value = values - row_trend,
        trend = row_trend,
        slope = slope,
        p_value = p_value,
        applied = applied
    )
}

# Replace lower- or upper-threshold values by deterministic within-threshold
# values whose rank order is reproducible. This removes a point mass before
# fitting while preserving its occurrence count for final de-randomization.
isimip__randomize_threshold <- function(
  values,
  bound,
  threshold,
  side = c("lower", "upper"),
  seed
) {
    side <- match.arg(side)
    selected <- if (identical(side, "lower")) {
        values <= threshold
    } else {
        values >= threshold
    }
    count <- sum(selected)
    if (!count) {
        return(list(
            value = values,
            count = 0L,
            seed = as.integer(seed)
        ))
    }
    uniform <- quantile__uniform(count, seed)
    randomized <- if (identical(side, "lower")) {
        bound + uniform * (threshold - bound)
    } else {
        threshold + uniform * (bound - threshold)
    }
    # Assign sorted random values by the original value rank, with stable input
    # order resolving ties instead of invoking R's random tie handling.
    original_order <- order(values[selected], seq_len(count))
    assigned <- numeric(count)
    assigned[original_order] <- sort(randomized)
    values[selected] <- assigned
    list(
        value = values,
        count = as.integer(count),
        seed = as.integer(seed)
    )
}

# Randomize both published threshold regions in a fixed order, deriving a new
# seed for the upper operation so the two tails never share a random stream.
isimip__randomize_bounds <- function(
  values,
  resolved,
  lower_seed,
  upper_seed
) {
    diagnostics <- list()
    if (!is.null(resolved$lower_threshold)) {
        lower <- isimip__randomize_threshold(
            values,
            resolved$bounds[[1L]],
            resolved$lower_threshold,
            "lower",
            lower_seed
        )
        values <- lower$value
        diagnostics$lower <- lower[c("count", "seed")]
    }
    if (!is.null(resolved$upper_threshold)) {
        upper <- isimip__randomize_threshold(
            values,
            resolved$bounds[[2L]],
            resolved$upper_threshold,
            "upper",
            upper_seed
        )
        values <- upper$value
        diagnostics$upper <- upper[c("count", "seed")]
    }
    list(value = values, diagnostics = diagnostics)
}

# Transfer an additive, capped multiplicative, mixed, or bounded change between
# common quantiles. The mixed cosine transition and bounded branch implement
# the published equations rather than selecting transformations by variable
# name inside the kernel.
isimip__transfer_change <- function(
  observed,
  historical,
  future,
  resolved
) {
    method <- resolved$trend_preservation
    if (identical(method, "additive")) {
        return(observed + future - historical)
    }
    if (identical(method, "multiplicative") ||
        identical(method, "mixed")) {
        ratio <- rep.int(1, length(future))
        nonzero <- historical != 0
        ratio[nonzero] <- future[nonzero] / historical[nonzero]
        ratio <- pmin(
            pmax(ratio, 1 / resolved$max_change_factor),
            resolved$max_change_factor
        )
        multiplicative <- observed * ratio
        if (identical(method, "multiplicative")) {
            return(multiplicative)
        }
        additive <- observed + future - historical
        fraction <- numeric(length(future))
        fraction[historical >= observed] <- 1
        transition <- historical < observed &
            historical > 0 &
            observed <
                resolved$max_adjustment_factor * historical
        fraction[transition] <- 0.5 * (
            1 + cos(
                (observed[transition] / historical[transition] - 1) *
                    pi /
                    (resolved$max_adjustment_factor - 1)
            )
        )
        return(
            fraction * multiplicative +
                (1 - fraction) * additive
        )
    }

    lower <- resolved$bounds[[1L]]
    upper <- resolved$bounds[[2L]]
    negative_bias <- historical < observed
    positive_bias <- historical > observed
    zero_bias <- !(negative_bias | positive_bias)
    additive <- (negative_bias & future < historical) |
        (positive_bias & future > historical)
    value <- numeric(length(future))
    upper_denominator <- upper - historical[negative_bias]
    value[negative_bias] <- upper -
        (upper - observed[negative_bias]) *
            (upper - future[negative_bias]) /
            upper_denominator
    value[zero_bias] <- future[zero_bias]
    lower_denominator <- historical[positive_bias] - lower
    value[positive_bias] <- lower +
        (observed[positive_bias] - lower) *
            (future[positive_bias] - lower) /
            lower_denominator
    value[additive] <- observed[additive] +
        future[additive] -
        historical[additive]
    value[!is.finite(value)] <- observed[!is.finite(value)] +
        future[!is.finite(value)] -
        historical[!is.finite(value)]
    pmin(pmax(value, lower), upper)
}

# Transfer the model change in bound-event frequency onto the observed event
# frequency using the same bounded climate-change equation on [0, 1].
isimip__transfer_frequency <- function(
  observed,
  historical,
  future,
  trendless
) {
    if (trendless) {
        return(observed)
    }
    settings <- list(
        trend_preservation = "bounded",
        bounds = c(0, 1)
    )
    value <- isimip__transfer_change(
        observed,
        historical,
        future,
        settings
    )
    pmin(pmax(value, 0), 1)
}

# Fit a fixed-location Weibull distribution by maximizing the profile
# likelihood over log-shape. The scale has a closed-form value conditional on
# shape, so positivity is guaranteed without an external optimizer package.
isimip__fit_weibull <- function(
  values,
  location,
  tolerance,
  max_iterations
) {
    shifted <- values - location
    if (length(shifted) < 2L ||
        any(!is.finite(shifted)) ||
        any(shifted <= 0) ||
        length(unique(shifted)) < 2L) {
        cli::cli_abort(
            "A fixed-location Weibull fit requires at least two distinct values above its location."
        )
    }
    log_values <- log(shifted)
    objective <- function(log_shape) {
        shape <- exp(log_shape)
        powered <- shape * log_values
        maximum <- max(powered)
        log_mean_power <- maximum +
            log(mean(exp(powered - maximum)))
        log_scale <- log_mean_power / shape
        log_likelihood <- length(shifted) * (
            log(shape) - shape * log_scale
        ) +
            (shape - 1) * sum(log_values) -
            sum(exp(shape * (log_values - log_scale)))
        if (is.finite(log_likelihood)) {
            -log_likelihood
        } else {
            .Machine$double.xmax
        }
    }
    optimization <- stats::optim(
        par = 0,
        fn = objective,
        method = "L-BFGS-B",
        lower = log(0.05),
        upper = log(100),
        control = list(
            factr = max(
                1,
                tolerance / .Machine$double.eps
            ),
            maxit = as.integer(max_iterations)
        )
    )
    if (optimization$convergence != 0L ||
        !is.finite(optimization$value)) {
        cli::cli_abort(
            "Fixed-location Weibull maximum-likelihood fitting did not converge."
        )
    }
    shape <- exp(optimization$par)
    maximum <- max(shape * log_values)
    log_scale <- (
        maximum +
            log(mean(exp(shape * log_values - maximum)))
    ) / shape
    list(
        family = "weibull",
        parameters = list(
            shape = shape,
            scale = exp(log_scale),
            location = location
        ),
        sample_size = length(values),
        method = "maximum_likelihood_fixed_location"
    )
}

# Fit one profile-selected distribution and retain an explicit threshold
# location for positive Gamma and Weibull families.
isimip__fit_distribution <- function(values, resolved) {
    model <- resolved$mapping_model
    if (identical(model, "normal")) {
        return(distribution__fit_normal(values))
    }
    if (identical(model, "gamma")) {
        shifted <- values - resolved$lower_threshold
        fit <- distribution__fit_gamma(
            shifted,
            tolerance = resolved$fit_tolerance,
            max_iterations = resolved$fit_max_iterations
        )
        fit$parameters$location <- resolved$lower_threshold
        fit$method <- "maximum_likelihood_fixed_location"
        return(fit)
    }
    if (identical(model, "weibull")) {
        return(isimip__fit_weibull(
            values,
            resolved$lower_threshold,
            resolved$fit_tolerance,
            resolved$fit_max_iterations
        ))
    }
    cli::cli_abort(
        "Empirical ISIMIP3BASD mapping does not use a parametric fit."
    )
}

# Calculate the two-sided one-sample Kolmogorov-Smirnov statistic without
# relying on the distribution-free p-value that is not used by the 3.0.x
# application configuration.
isimip__ks_statistic <- function(values, fit) {
    ordered <- sort(values)
    probability <- distribution__cdf(fit, ordered)
    n <- length(ordered)
    max(c(
        abs(probability - seq.int(0L, n - 1L) / n),
        abs(seq_len(n) / n - probability)
    ))
}

# Attempt one parametric fit and return a structured failure reason instead of
# allowing optimizer warnings to escape the group execution boundary.
isimip__try_fit <- function(values, resolved) {
    if (length(values) < resolved$min_samples ||
        length(unique(values)) < 2L) {
        return(list(
            fit = NULL,
            reason = "insufficient_distinct_samples"
        ))
    }
    fit <- tryCatch(
        suppressWarnings(isimip__fit_distribution(values, resolved)),
        error = function(error) error
    )
    if (inherits(fit, "error")) {
        return(list(
            fit = NULL,
            reason = conditionMessage(fit)
        ))
    }
    statistic <- isimip__ks_statistic(values, fit)
    if (!is.finite(statistic) ||
        statistic > resolved$ks_threshold) {
        return(list(
            fit = NULL,
            reason = "kolmogorov_smirnov_threshold",
            ks_statistic = statistic
        ))
    }
    list(
        fit = fit,
        reason = NULL,
        ks_statistic = statistic
    )
}

# Interpolate probabilities from a fixed quantile grid while averaging the
# probability coordinate of duplicated quantiles.
isimip__probability_from_quantiles <- function(
  values,
  quantiles,
  probability
) {
    groups <- split(probability, quantiles)
    anchors <- as.numeric(names(groups))
    anchor_probability <- vapply(groups, mean, numeric(1L))
    order_index <- order(anchors)
    anchors <- anchors[order_index]
    anchor_probability <- anchor_probability[order_index]
    if (length(anchors) == 1L) {
        return(rep.int(anchor_probability, length(values)))
    }
    stats::approx(
        anchors,
        anchor_probability,
        xout = values,
        method = "linear",
        rule = 2,
        ties = "ordered"
    )$y
}

# Map one empirical source distribution to a target distribution with the
# published constant-correction tail extrapolation.
isimip__map_empirical <- function(
  values,
  source,
  target,
  n_quantiles
) {
    count <- min(
        as.integer(n_quantiles) + 1L,
        length(source),
        length(target)
    )
    if (count < 2L ||
        length(unique(source)) < 2L ||
        !length(target)) {
        return(list(
            value = values,
            quantiles = count - 1L,
            fallback = "insufficient_empirical_support"
        ))
    }
    probability <- seq(0, 1, length.out = count)
    source_quantile <- quantile__inverse_cdf(source, probability)
    target_quantile <- quantile__inverse_cdf(target, probability)
    groups <- split(seq_along(source_quantile), source_quantile)
    source_anchor <- as.numeric(names(groups))
    target_anchor <- vapply(groups, function(index) {
        mean(target_quantile[index])
    }, numeric(1L))
    order_index <- order(source_anchor)
    source_anchor <- source_anchor[order_index]
    target_anchor <- target_anchor[order_index]
    if (length(source_anchor) == 1L) {
        mapped <- values + target_anchor - source_anchor
    } else {
        mapped <- stats::approx(
            source_anchor,
            target_anchor,
            xout = values,
            method = "linear",
            rule = 2,
            ties = "ordered"
        )$y
        lower <- values < source_anchor[[1L]]
        upper <- values > source_anchor[[length(source_anchor)]]
        mapped[lower] <- values[lower] +
            target_anchor[[1L]] -
            source_anchor[[1L]]
        mapped[upper] <- values[upper] +
            target_anchor[[length(target_anchor)]] -
            source_anchor[[length(source_anchor)]]
    }
    list(
        value = mapped,
        quantiles = count - 1L,
        fallback = NULL
    )
}

# Quantile-map arbitrary selected source values onto the empirical distribution
# of source-fit values before applying a bounded parametric model.
isimip__brute_force_to_fit <- function(values, source_fit) {
    if (!length(values) || length(unique(source_fit)) < 2L) {
        return(values)
    }
    probability <- (
        rank(values, ties.method = "average") - 1
    ) / length(values)
    quantile__inverse_cdf(source_fit, probability)
}

# Generate pseudo future observations by transferring modeled changes at
# corresponding empirical quantiles onto historical observations.
isimip__pseudo_future <- function(
  observed,
  historical,
  future,
  resolved
) {
    observed_keep <- rep.int(TRUE, length(observed))
    historical_keep <- rep.int(TRUE, length(historical))
    future_keep <- rep.int(TRUE, length(future))
    if (!resolved$unconditional_change_transfer) {
        if (!is.null(resolved$lower_threshold)) {
            observed_keep <- observed_keep &
                observed > resolved$lower_threshold
            historical_keep <- historical_keep &
                historical > resolved$lower_threshold
            future_keep <- future_keep &
                future > resolved$lower_threshold
        }
        if (!is.null(resolved$upper_threshold)) {
            observed_keep <- observed_keep &
                observed < resolved$upper_threshold
            historical_keep <- historical_keep &
                historical < resolved$upper_threshold
            future_keep <- future_keep &
                future < resolved$upper_threshold
        }
    }
    count <- min(
        resolved$n_quantiles + 1L,
        sum(observed_keep),
        sum(historical_keep),
        sum(future_keep)
    )
    target <- observed
    if (count < 2L) {
        return(list(
            value = target,
            diagnostics = list(
                quantiles = 0L,
                fallback = "insufficient_change_transfer_support"
            )
        ))
    }
    probability <- seq(0, 1, length.out = count)
    observed_quantile <- quantile__inverse_cdf(
        observed[observed_keep],
        probability
    )
    historical_quantile <- quantile__inverse_cdf(
        historical[historical_keep],
        probability
    )
    future_quantile <- quantile__inverse_cdf(
        future[future_keep],
        probability
    )
    observed_probability <- isimip__probability_from_quantiles(
        observed[observed_keep],
        observed_quantile,
        probability
    )
    historical_at_probability <- stats::approx(
        probability,
        historical_quantile,
        xout = observed_probability,
        method = "linear",
        rule = 2,
        ties = "ordered"
    )$y
    future_at_probability <- stats::approx(
        probability,
        future_quantile,
        xout = observed_probability,
        method = "linear",
        rule = 2,
        ties = "ordered"
    )$y
    target[observed_keep] <- isimip__transfer_change(
        observed[observed_keep],
        historical_at_probability,
        future_at_probability,
        resolved
    )
    list(
        value = target,
        diagnostics = list(
            quantiles = as.integer(count - 1L),
            observed_samples = as.integer(sum(observed_keep)),
            historical_samples = as.integer(sum(historical_keep)),
            future_samples = as.integer(sum(future_keep)),
            fallback = NULL
        )
    )
}

# Calculate target lower- and upper-bound event proportions, normalizing the
# pair when their transferred frequencies would otherwise overlap.
isimip__target_frequencies <- function(
  original,
  resolved
) {
    frequencies <- list(lower = 0, upper = 0)
    if (!is.null(resolved$lower_threshold)) {
        probability <- vapply(original, function(values) {
            mean(values <= resolved$lower_threshold, na.rm = TRUE)
        }, numeric(1L))
        frequencies$lower <- isimip__transfer_frequency(
            probability[["observed_reference"]],
            probability[["model_historical"]],
            probability[["model_future"]],
            resolved$trendless_bound_frequency
        )
    }
    if (!is.null(resolved$upper_threshold)) {
        probability <- vapply(original, function(values) {
            mean(values >= resolved$upper_threshold, na.rm = TRUE)
        }, numeric(1L))
        frequencies$upper <- isimip__transfer_frequency(
            probability[["observed_reference"]],
            probability[["model_historical"]],
            probability[["model_future"]],
            resolved$trendless_bound_frequency
        )
    }
    total <- frequencies$lower + frequencies$upper
    normalized <- total > 1
    if (normalized) {
        frequencies$lower <- frequencies$lower / total
        frequencies$upper <- frequencies$upper / total
    }
    c(
        frequencies,
        list(normalized = normalized)
    )
}

# Bias-adjust one randomized future window onto its pseudo-observed target.
# Bound frequencies are set first; parametric fitting falls back explicitly to
# empirical mapping if either fitted distribution is unsupported.
isimip__map_window <- function(
  future,
  pseudo,
  original,
  resolved
) {
    frequencies <- isimip__target_frequencies(original, resolved)
    n <- length(future)
    lower_count <- as.integer(floor(n * frequencies$lower + 0.5))
    upper_count <- as.integer(floor(n * frequencies$upper + 0.5))
    if (lower_count + upper_count > n) {
        overflow <- lower_count + upper_count - n
        upper_count <- max(0L, upper_count - overflow)
    }
    ordered <- order(future, seq_along(future))
    lower_index <- if (lower_count) {
        ordered[seq_len(lower_count)]
    } else {
        integer()
    }
    upper_index <- if (upper_count) {
        tail(ordered, upper_count)
    } else {
        integer()
    }
    source_index <- setdiff(
        seq_along(future),
        c(lower_index, upper_index)
    )
    mapped <- future
    if (length(lower_index)) {
        mapped[lower_index] <- resolved$bounds[[1L]]
    }
    if (length(upper_index)) {
        mapped[upper_index] <- resolved$bounds[[2L]]
    }

    source_fit <- future
    target_fit <- pseudo
    if (!is.null(resolved$lower_threshold)) {
        source_fit <- source_fit[
            source_fit > resolved$lower_threshold
        ]
        target_fit <- target_fit[
            target_fit > resolved$lower_threshold
        ]
    }
    if (!is.null(resolved$upper_threshold)) {
        source_fit <- source_fit[
            source_fit < resolved$upper_threshold
        ]
        target_fit <- target_fit[
            target_fit < resolved$upper_threshold
        ]
    }

    mapping <- list(
        requested = resolved$mapping_model,
        used = resolved$mapping_model,
        fallback = NULL,
        source_fit = NULL,
        target_fit = NULL
    )
    if (length(source_index) &&
        length(source_fit) &&
        length(target_fit)) {
        values <- future[source_index]
        if (identical(resolved$mapping_model, "empirical")) {
            empirical <- isimip__map_empirical(
                values,
                source_fit,
                target_fit,
                resolved$n_quantiles
            )
            mapped[source_index] <- empirical$value
            mapping$used <- "empirical"
            mapping$fallback <- empirical$fallback
            mapping$quantiles <- empirical$quantiles
        } else {
            source_attempt <- isimip__try_fit(source_fit, resolved)
            target_attempt <- isimip__try_fit(target_fit, resolved)
            if (is.null(source_attempt$fit) ||
                is.null(target_attempt$fit)) {
                empirical <- isimip__map_empirical(
                    values,
                    source_fit,
                    target_fit,
                    resolved$n_quantiles
                )
                mapped[source_index] <- empirical$value
                mapping$used <- "empirical"
                mapping$fallback <- list(
                    source = source_attempt$reason,
                    target = target_attempt$reason,
                    empirical = empirical$fallback
                )
                mapping$quantiles <- empirical$quantiles
            } else {
                source_map <- if (
                    !is.null(resolved$lower_threshold) ||
                        !is.null(resolved$upper_threshold)
                ) {
                    isimip__brute_force_to_fit(values, source_fit)
                } else {
                    values
                }
                raw_probability <- distribution__cdf(
                    source_attempt$fit,
                    source_map
                )
                probability <- distribution__clamp_probability(
                    raw_probability,
                    resolved$cdf_epsilon
                )
                mapped[source_index] <- distribution__quantile(
                    target_attempt$fit,
                    probability
                )
                mapping$source_fit <- source_attempt$fit
                mapping$target_fit <- target_attempt$fit
                mapping$source_ks <- source_attempt$ks_statistic
                mapping$target_ks <- target_attempt$ks_statistic
                mapping$clamped_probabilities <- as.integer(sum(
                    raw_probability != probability
                ))
            }
        }
    } else if (length(source_index)) {
        mapping$used <- "identity"
        mapping$fallback <- "no_within_threshold_target_support"
    }

    # Collapse mapped threshold-region values back to their point masses and
    # enforce physical bounds after any empirical tail extrapolation.
    if (!is.null(resolved$lower_threshold)) {
        mapped[mapped <= resolved$lower_threshold] <-
            resolved$bounds[[1L]]
    }
    if (!is.null(resolved$upper_threshold)) {
        mapped[mapped >= resolved$upper_threshold] <-
            resolved$bounds[[2L]]
    }
    bounded_result <- signal__bound_values(mapped, resolved$bounds)
    bounded <- bounded_result$value
    list(
        value = bounded,
        diagnostics = list(
            target_frequencies = frequencies,
            target_counts = c(
                lower = lower_count,
                upper = upper_count
            ),
            source_samples = as.integer(length(source_fit)),
            target_samples = as.integer(length(target_fit)),
            mapping = mapping,
            clipped_values = bounded_result$clipped
        )
    )
}

# Prepare one role inside a seasonal window in the published operation order:
# missing-value imputation, optional annual-mean detrending, then deterministic
# threshold randomization.
isimip__prepare_window_role <- function(
  data,
  role,
  resolved,
  key,
  variable,
  target_day
) {
    imputation_seed <- isimip__seed(
        resolved,
        key,
        variable,
        paste0("impute_", role),
        target_day
    )
    imputed <- if (resolved$impute_missing) {
        isimip__impute_missing(
            data[["value"]],
            imputation_seed,
            resolved$all_missing_fallback
        )
    } else {
        list(
            value = data[["value"]],
            missing = 0L,
            all_missing_fallback = FALSE,
            seed = imputation_seed
        )
    }
    detrended <- if (resolved$detrend) {
        isimip__detrend(
            imputed$value,
            data[["cf_year"]],
            resolved$detrend_alpha
        )
    } else {
        list(
            value = imputed$value,
            trend = rep.int(0, nrow(data)),
            slope = 0,
            p_value = 1,
            applied = FALSE
        )
    }
    randomized <- isimip__randomize_bounds(
        detrended$value,
        resolved,
        isimip__seed(
            resolved,
            key,
            variable,
            paste0("randomize_lower_", role),
            target_day
        ),
        isimip__seed(
            resolved,
            key,
            variable,
            paste0("randomize_upper_", role),
            target_day
        )
    )
    list(
        original = imputed$value,
        value = randomized$value,
        trend = detrended$trend,
        diagnostics = list(
            imputation = imputed[c(
                "missing",
                "all_missing_fallback",
                "seed"
            )],
            detrending = detrended[c(
                "slope",
                "p_value",
                "applied"
            )],
            randomization = randomized$diagnostics
        )
    )
}

# Execute all marginal operations for one target-day window and return values
# for the complete future window. The caller retains only the center-day rows,
# preventing overlapping windows from writing a row more than once.
isimip__adjust_window <- function(
  series,
  center,
  target_day,
  resolved,
  key,
  variable
) {
    rows <- lapply(series, function(data) {
        daily__phase_window(
            data[["annual_phase"]],
            center,
            resolved$running_window_days,
            resolved$target_year_days
        )
    })
    counts <- vapply(rows, sum, integer(1L))
    if (any(counts < resolved$min_samples)) {
        cli::cli_abort(
            "ISIMIP3BASD target day {target_day} has fewer than {resolved$min_samples} observed, historical, or future daily values in its running window."
        )
    }
    window <- Map(function(data, keep) {
        data[keep, , drop = FALSE]
    }, series, rows)
    prepared <- Map(function(data, role) {
        isimip__prepare_window_role(
            data,
            role,
            resolved,
            key,
            variable,
            target_day
        )
    }, window, names(window))
    names(prepared) <- names(window)

    pseudo <- isimip__pseudo_future(
        prepared$observed_reference$value,
        prepared$model_historical$value,
        prepared$model_future$value,
        resolved
    )
    mapped <- isimip__map_window(
        prepared$model_future$value,
        pseudo$value,
        lapply(prepared, function(role) role$original),
        resolved
    )
    restored <- mapped$value + prepared$model_future$trend
    bounded <- signal__bound_values(restored, resolved$bounds)$value
    if (!is.null(resolved$lower_threshold)) {
        bounded[bounded <= resolved$lower_threshold] <-
            resolved$bounds[[1L]]
    }
    if (!is.null(resolved$upper_threshold)) {
        bounded[bounded >= resolved$upper_threshold] <-
            resolved$bounds[[2L]]
    }
    list(
        value = bounded,
        future_rows = which(rows$model_future),
        diagnostics = list(
            target_day = as.integer(target_day),
            center_phase = center,
            samples = counts,
            role_preparation = lapply(
                prepared,
                function(role) role$diagnostics
            ),
            change_transfer = pseudo$diagnostics,
            distribution_mapping = mapped$diagnostics,
            post_mapping_clipped_values = as.integer(sum(
                bounded != restored
            ))
        )
    )
}

# Apply every required target-day window to the future-model backbone. Each
# native-calendar row is assigned through its annual phase, while short-wave
# scaling and rescaling happen outside the repeated fitting loop.
isimip__adjust_values <- function(
  series,
  resolved,
  key,
  variable
) {
    upper_bound <- NULL
    working <- series
    if (resolved$scale_by_upper_bound_cycle) {
        upper_bound <- isimip__scale_upper_bounds(series, resolved)
        working <- upper_bound$series
    }
    future <- working$model_future
    future_target_day <- isimip__target_day(
        future[["annual_phase"]],
        resolved$target_year_days
    )
    target_days <- sort(unique(future_target_day))
    grid <- daily__phase_grid(resolved$target_year_days)
    adjusted <- rep.int(NA_real_, nrow(future))
    records <- vector("list", length(target_days))

    for (record_index in seq_along(target_days)) {
        target_day <- target_days[[record_index]]
        window <- isimip__adjust_window(
            working,
            grid[[target_day]],
            target_day,
            resolved,
            key,
            variable
        )
        center_in_window <- future_target_day[window$future_rows] ==
            target_day
        output_rows <- window$future_rows[center_in_window]
        adjusted[output_rows] <- window$value[center_in_window]
        records[[record_index]] <- window$diagnostics
    }
    if (anyNA(adjusted)) {
        cli::cli_abort(
            "ISIMIP3BASD did not assign every future-model daily row exactly once."
        )
    }
    if (!is.null(upper_bound)) {
        adjusted <- adjusted *
            upper_bound$target[future_target_day]
    }
    if (any(!is.finite(adjusted))) {
        cli::cli_abort(
            "ISIMIP3BASD produced a non-finite adjusted value."
        )
    }
    list(
        value = adjusted,
        diagnostics = list(
            target_day_count = as.integer(length(target_days)),
            target_days = as.integer(target_days),
            native_calendars = vapply(
                series,
                function(data) unique(data[["cf_calendar"]]),
                character(1L)
            ),
            calendar_adapter = list(
                coordinate = "annual_phase",
                target_year_days = resolved$target_year_days,
                source_policy = "preserve_native_cf_calendars"
            ),
            upper_bound_scaling = if (is.null(upper_bound)) {
                NULL
            } else {
                upper_bound$diagnostics
            },
            windows = records
        )
    )
}

# Execute one univariate ISIMIP3BASD group and return the shared future-model
# DailyAdjustedSeries result with complete method and calendar provenance.
isimip__apply_group <- function(inputs, settings, key) {
    resolved <- isimip__settings(settings)
    variable <- names(settings)[[1L]]
    series <- isimip__inputs(inputs, variable, resolved)
    mapped <- isimip__adjust_values(
        series,
        resolved,
        key,
        variable
    )
    future <- series$model_future
    future[["value"]] <- mapped$value
    derived <- variable %in% ISIMIP_DERIVED_VARIABLES
    reconstructed <- switch(
        variable,
        prsnratio = "prsn",
        tasrange = c("tasmin", "tasmax"),
        tasskew = c("tasmin", "tasmax"),
        NULL
    )

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = "isimip3basd_bias_adjustment",
        variable_metadata = stats::setNames(
            list(list(
                units = unique(future[["units"]]),
                frequency = "day",
                calendars = sort(unique(future[["cf_calendar"]])),
                variable_role = if (derived) {
                    "derived_component"
                } else {
                    "direct"
                },
                reconstructed_outputs = reconstructed,
                mapping_domain = if (
                    resolved$scale_by_upper_bound_cycle
                ) {
                    "upper_bound_fraction"
                } else {
                    "native_units"
                }
            )),
            variable
        ),
        settings = resolved,
        provenance = list(
            method = "isimip3basd",
            method_version = "3.0.x",
            references = ISIMIP_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            published_frequency = "day",
            calendar_source = "epwshiftr_native_cf_phase_adapter",
            marginal_adjustment = TRUE,
            spatial_downscaling = "separate_stage",
            reconstructed_outputs = reconstructed,
            mapping_domain = if (
                resolved$scale_by_upper_bound_cycle
            ) {
                "upper_bound_fraction"
            } else {
                "native_units"
            },
            stochastic_generator = "park_miller_method_local",
            diagnostics = mapped$diagnostics
        )
    )
}

# Return an explicit diagnostic if the component violates the package-native
# future-model result contract.
isimip__validate_result <- function(value, inputs, key) {
    signal__validate_adjusted_result(
        value,
        DailyAdjustedSeries,
        "DailyAdjustedSeries",
        "model_future",
        "ISIMIP3BASD"
    )
}

# Construct the package-native signal component for every direct and component
# variable in the published ISIMIP3BASD application configuration.
isimip__component <- function() {
    alternatives <- as.list(ISIMIP_VARIABLES)
    requirements <- signal__three_role_requirements(
        alternatives,
        frequencies = "day"
    )
    signal__component(
        name = "isimip3basd_daily",
        label = "Daily ISIMIP3BASD",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = TRUE,
        profiles = isimip__profiles(),
        apply_group = isimip__apply_group,
        operations = list(validate_result = isimip__validate_result),
        metadata = list(
            method_family = "trend_preserving_bias_adjustment",
            method_version = "3.0.x",
            output_contract = "daily_adjusted_series",
            references = ISIMIP_REFERENCES,
            published_frequency = "day",
            spatial_downscaling = "separate_stage",
            calendar_adapter = "native_cf_annual_phase",
            direct_variables = ISIMIP_DIRECT_VARIABLES,
            derived_components = ISIMIP_DERIVED_VARIABLES,
            reconstructed_outputs = c("prsn", "tasmin", "tasmax"),
            window_defaults = list(
                seasonal_days = 31L,
                step_days = 1L,
                target_year_days = 365L
            )
        )
    )
}

# Register ISIMIP3BASD once so package load and repeated tests share one
# discoverable process-local component.
isimip__register_component <- function() {
    component__register_builtin(isimip__component())
    invisible(NULL)
}
