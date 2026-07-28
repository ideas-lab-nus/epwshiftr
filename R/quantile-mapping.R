#' @include bias-adjustment.R daily-climatology.R quantile-distribution.R
NULL

# Quantile Mapping references identify the published transfer equation and
# the review used to delimit the method's assumptions and evidence.
QM_REFERENCES <- c(
    "https://doi.org/10.1175/JCLI-D-14-00754.1",
    "https://doi.org/10.1007/s40641-016-0050-x"
)

# A 0.1 mm/day trace threshold is expressed in the native CMIP6 precipitation
# flux unit kg m-2 s-1; callers using another unit must override the setting.
QM_PR_DRY_THRESHOLD <- 0.1 / 86400

# The method literature supports direct Quantile Mapping defaults for mean
# temperature and precipitation. Other variable profiles are implementation
# choices and therefore remain explicitly experimental.
QM_PUBLISHED_VARIABLES <- c("pr", "tas")
QM_EXPERIMENTAL_VARIABLES <- c(
    "hurs",
    "psl",
    "rlds",
    "sfcWind",
    "tasmin",
    "tasmax"
)

# Construct the complete empirical Quantile Mapping settings record shared by
# every variable profile, including conventions that publications leave open.
qm__default_settings <- function(
    bounds,
    distribution_model = c("continuous", "precipitation_hurdle"),
    dry_threshold = 0
) {
    distribution_model <- match.arg(distribution_model)
    list(
        mapping_type = "nonparametric",
        detrending = "none",
        seasonal_window_days = 31L,
        target_year_days = 365L,
        min_samples = 10L,
        cdf_method = "linear_interpolation",
        inverse_cdf_method = "linear_type_7",
        tie_method = "average_rank",
        tail_policy = "clamp",
        bounds = bounds,
        distribution_model = distribution_model,
        dry_threshold = dry_threshold,
        random_seed = 1L
    )
}

# Build published and experimental profiles without assigning literature
# provenance to implementation-selected variable defaults.
qm__profiles <- function() {
    settings <- list(
        pr = qm__default_settings(
            c(0, Inf),
            "precipitation_hurdle",
            QM_PR_DRY_THRESHOLD
        ),
        tas = qm__default_settings(c(-Inf, Inf)),
        hurs = qm__default_settings(c(0, 100)),
        psl = qm__default_settings(c(0, Inf)),
        rlds = qm__default_settings(c(0, Inf)),
        sfcWind = qm__default_settings(c(0, Inf)),
        tasmin = qm__default_settings(c(-Inf, Inf)),
        tasmax = qm__default_settings(c(-Inf, Inf))
    )
    variables <- c(QM_PUBLISHED_VARIABLES, QM_EXPERIMENTAL_VARIABLES)
    lapply(variables, function(variable) {
        published <- variable %in% QM_PUBLISHED_VARIABLES
        signal__variable_profile(
            variable,
            settings = settings[[variable]],
            evidence = if (published) "published" else "experimental",
            references = if (published) QM_REFERENCES else character(),
            metadata = list(
                method = "quantile_mapping",
                output_role = "model_future",
                default_source = if (published) {
                    "method_literature"
                } else {
                    "package_implementation"
                }
            )
        )
    })
}

# Validate all method conventions at the kernel boundary so unsupported
# empirical-CDF variants cannot be selected silently through an override.
qm__settings <- function(settings) {
    if (length(settings) != 1L ||
        is.null(names(settings)) ||
        !nzchar(names(settings)[[1L]]) ||
        !is.list(settings[[1L]])) {
        cli::cli_abort(
            "Quantile Mapping requires settings for exactly one variable."
        )
    }
    resolved <- settings[[1L]]
    expected <- c(
        "mapping_type",
        "detrending",
        "seasonal_window_days",
        "target_year_days",
        "min_samples",
        "cdf_method",
        "inverse_cdf_method",
        "tie_method",
        "tail_policy",
        "bounds",
        "distribution_model",
        "dry_threshold",
        "random_seed"
    )
    missing <- setdiff(expected, names(resolved))
    unexpected <- setdiff(names(resolved), expected)
    if (length(missing) || length(unexpected)) {
        cli::cli_abort(c(
            "Quantile Mapping settings must use the complete supported schema.",
            "x" = "Missing setting(s): {.val {missing}}.",
            "x" = "Unexpected setting(s): {.val {unexpected}}."
        ))
    }
    if (!identical(resolved$mapping_type, "nonparametric")) {
        cli::cli_abort(
            "Quantile Mapping currently supports only nonparametric mapping."
        )
    }
    if (!identical(resolved$detrending, "none")) {
        cli::cli_abort(
            "Quantile Mapping currently supports only `detrending = \"none\"`."
        )
    }
    if (!identical(resolved$cdf_method, "linear_interpolation") ||
        !identical(resolved$inverse_cdf_method, "linear_type_7") ||
        !identical(resolved$tie_method, "average_rank") ||
        !identical(resolved$tail_policy, "clamp")) {
        cli::cli_abort(
            "Quantile Mapping currently requires linear empirical CDF interpolation, type-7 inverse quantiles, average-rank ties, and clamped tails."
        )
    }
    checkmate::assert_integerish(
        resolved$seasonal_window_days,
        lower = 1L,
        len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_integerish(
        resolved$target_year_days,
        lower = 3L,
        len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_integerish(
        resolved$min_samples,
        lower = 2L,
        len = 1L,
        any.missing = FALSE
    )
    # Reuse the calendar-neutral window validator to enforce odd widths and
    # prevent a wider-than-year seasonal window.
    daily__window_spec(
        as.integer(resolved$seasonal_window_days),
        as.integer(resolved$target_year_days)
    )
    checkmate::assert_numeric(
        resolved$bounds,
        len = 2L,
        any.missing = FALSE
    )
    if (resolved$bounds[[1L]] > resolved$bounds[[2L]]) {
        cli::cli_abort(
            "Quantile Mapping bounds must be ordered from lower to upper."
        )
    }
    checkmate::assert_choice(
        resolved$distribution_model,
        c("continuous", "precipitation_hurdle")
    )
    checkmate::assert_number(
        resolved$dry_threshold,
        lower = 0,
        finite = TRUE
    )
    checkmate::assert_integerish(
        resolved$random_seed,
        lower = 0,
        upper = .Machine$integer.max - 1L,
        len = 1L,
        any.missing = FALSE
    )

    resolved$seasonal_window_days <- as.integer(
        resolved$seasonal_window_days
    )
    resolved$target_year_days <- as.integer(resolved$target_year_days)
    resolved$min_samples <- as.integer(resolved$min_samples)
    resolved$random_seed <- as.integer(resolved$random_seed)
    resolved
}

# Validate the three role-addressable daily inputs without pairing their native
# dates or requiring their CF calendars to be identical.
qm__inputs <- function(inputs, variable, distribution_model) {
    roles <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    if (!identical(sort(names(inputs)), sort(roles))) {
        cli::cli_abort(
            "Quantile Mapping requires observed, historical-model, and future-model role payloads."
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
                "Quantile Mapping role {.val {role}} must contain only variable {.val {variable}}."
            )
        }
        if (length(unique(series[[role]][["cf_calendar"]])) != 1L) {
            cli::cli_abort(
                "Quantile Mapping role {.val {role}} must contain one native calendar per signal group."
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
            "Quantile Mapping inputs for {.val {variable}} must use identical units."
        )
    }
    if (identical(distribution_model, "precipitation_hurdle") &&
        any(vapply(
            series,
            function(data) any(data[["value"]] < 0),
            logical(1L)
        ))) {
        cli::cli_abort(
            "Precipitation-hurdle Quantile Mapping requires non-negative input values."
        )
    }
    series
}

# Apply x* = F_obs^{-1}(F_hist(x_future)) with the declared interpolation,
# tie, and tail conventions to one or more future values.
qm__map_continuous <- function(historical, observed, future) {
    cdf <- quantile__empirical_cdf(historical, future)
    list(
        value = quantile__inverse_cdf(observed, cdf$probability),
        probability = cdf$probability,
        lower_tail = cdf$lower_tail,
        upper_tail = cdf$upper_tail,
        tied_historical_values = cdf$tied_sample_values,
        tied_observed_values = length(observed) -
            length(unique(observed))
    )
}

# Map a mixed precipitation distribution: values at or below the trace
# threshold are randomized uniformly across the historical dry-day mass, while
# positive amounts use empirical conditional CDFs on both sides of the hurdle.
qm__map_precipitation <- function(
    historical,
    observed,
    future,
    uniform,
    dry_threshold
) {
    historical_dry <- historical <= dry_threshold
    observed_dry <- observed <= dry_threshold
    future_dry <- future <= dry_threshold
    historical_dry_probability <- mean(historical_dry)
    observed_dry_probability <- mean(observed_dry)
    historical_positive <- historical[!historical_dry]
    observed_positive <- observed[!observed_dry]

    probability <- numeric(length(future))
    probability[future_dry] <- (
        uniform[future_dry] * historical_dry_probability
    )
    positive <- !future_dry
    lower_tail <- upper_tail <- rep.int(FALSE, length(future))
    tied_historical <- 0L
    if (any(positive)) {
        if (!length(historical_positive)) {
            cli::cli_abort(
                "A precipitation window has positive future values but no positive historical-model calibration values."
            )
        }
        positive_cdf <- quantile__empirical_cdf(
            historical_positive,
            future[positive]
        )
        probability[positive] <- historical_dry_probability +
            (1 - historical_dry_probability) *
                positive_cdf$probability
        lower_tail[positive] <- positive_cdf$lower_tail
        upper_tail[positive] <- positive_cdf$upper_tail
        tied_historical <- positive_cdf$tied_sample_values
    }

    mapped <- numeric(length(future))
    output_positive <- probability > observed_dry_probability
    if (any(output_positive)) {
        if (!length(observed_positive)) {
            cli::cli_abort(
                "A precipitation window requires positive output amounts but has no positive observed-reference calibration values."
            )
        }
        conditional_probability <- (
            probability[output_positive] - observed_dry_probability
        ) / (1 - observed_dry_probability)
        mapped[output_positive] <- quantile__inverse_cdf(
            observed_positive,
            conditional_probability
        )
    }
    list(
        value = mapped,
        probability = probability,
        lower_tail = lower_tail,
        upper_tail = upper_tail,
        tied_historical_values = tied_historical,
        tied_observed_values = length(observed_positive) -
            length(unique(observed_positive)),
        historical_dry_probability = historical_dry_probability,
        observed_dry_probability = observed_dry_probability,
        randomized_dry_values = sum(future_dry)
    )
}

# Summarize sample coverage and mapping behavior without storing one additional
# provenance row for every day in a multi-decadal future series.
qm__diagnostics <- function(
    observed_samples,
    historical_samples,
    probability,
    lower_tail,
    upper_tail,
    tied_historical,
    tied_observed,
    clipped,
    precipitation = NULL
) {
    diagnostics <- list(
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
        mapped_probability_range = range(probability),
        lower_tail_values = sum(lower_tail),
        upper_tail_values = sum(upper_tail),
        tied_historical_values = sum(tied_historical),
        tied_observed_values = sum(tied_observed),
        clipped_values = clipped
    )
    if (!is.null(precipitation)) {
        diagnostics$precipitation <- precipitation
    }
    diagnostics
}

# Apply the calendar-neutral circular window independently at each future day,
# retaining the future series as the output backbone.
qm__adjust_values <- function(series, resolved, key, variable) {
    observed <- series$observed_reference
    historical <- series$model_historical
    future <- series$model_future
    n_future <- nrow(future)
    observed_samples <- historical_samples <- integer(n_future)
    probability <- adjusted <- numeric(n_future)
    lower_tail <- upper_tail <- logical(n_future)
    tied_historical <- tied_observed <- integer(n_future)
    effective_seed <- quantile__group_seed(
        resolved$random_seed,
        key,
        variable
    )
    uniform <- quantile__uniform(n_future, effective_seed)
    precipitation_diagnostics <- if (identical(
        resolved$distribution_model,
        "precipitation_hurdle"
    )) {
        list(
            historical_dry_probability = numeric(n_future),
            observed_dry_probability = numeric(n_future),
            randomized_dry_values = 0L,
            input_dry_values = sum(
                future[["value"]] <= resolved$dry_threshold
            )
        )
    } else {
        NULL
    }

    for (index in seq_len(n_future)) {
        center <- future[["annual_phase"]][[index]]
        observed_window <- daily__phase_window(
            observed[["annual_phase"]],
            center,
            resolved$seasonal_window_days,
            resolved$target_year_days
        )
        historical_window <- daily__phase_window(
            historical[["annual_phase"]],
            center,
            resolved$seasonal_window_days,
            resolved$target_year_days
        )
        observed_values <- observed[["value"]][observed_window]
        historical_values <- historical[["value"]][historical_window]
        observed_samples[[index]] <- length(observed_values)
        historical_samples[[index]] <- length(historical_values)
        if (observed_samples[[index]] < resolved$min_samples ||
            historical_samples[[index]] < resolved$min_samples) {
            cli::cli_abort(
                "Quantile Mapping future row {index} has fewer than {resolved$min_samples} observed or historical calibration values in its circular window."
            )
        }

        mapped <- if (identical(
            resolved$distribution_model,
            "precipitation_hurdle"
        )) {
            qm__map_precipitation(
                historical_values,
                observed_values,
                future[["value"]][[index]],
                uniform[[index]],
                resolved$dry_threshold
            )
        } else {
            qm__map_continuous(
                historical_values,
                observed_values,
                future[["value"]][[index]]
            )
        }
        adjusted[[index]] <- mapped$value
        probability[[index]] <- mapped$probability
        lower_tail[[index]] <- mapped$lower_tail
        upper_tail[[index]] <- mapped$upper_tail
        tied_historical[[index]] <- mapped$tied_historical_values
        tied_observed[[index]] <- mapped$tied_observed_values
        if (!is.null(precipitation_diagnostics)) {
            precipitation_diagnostics$historical_dry_probability[[index]] <-
                mapped$historical_dry_probability
            precipitation_diagnostics$observed_dry_probability[[index]] <-
                mapped$observed_dry_probability
            precipitation_diagnostics$randomized_dry_values <-
                precipitation_diagnostics$randomized_dry_values +
                    mapped$randomized_dry_values
        }
    }

    bounded <- pmin(
        pmax(adjusted, resolved$bounds[[1L]]),
        resolved$bounds[[2L]]
    )
    clipped <- sum(bounded != adjusted)
    if (!is.null(precipitation_diagnostics)) {
        precipitation_diagnostics$historical_dry_probability_range <- range(
            precipitation_diagnostics$historical_dry_probability
        )
        precipitation_diagnostics$observed_dry_probability_range <- range(
            precipitation_diagnostics$observed_dry_probability
        )
        precipitation_diagnostics$historical_dry_probability <- NULL
        precipitation_diagnostics$observed_dry_probability <- NULL
        precipitation_diagnostics$output_dry_values <- sum(
            bounded <= resolved$dry_threshold
        )
        precipitation_diagnostics$dry_threshold <- resolved$dry_threshold
        precipitation_diagnostics$random_seed <- resolved$random_seed
        precipitation_diagnostics$effective_seed <- effective_seed
        precipitation_diagnostics$random_generator <-
            "park_miller_16807"
    }
    list(
        value = bounded,
        diagnostics = qm__diagnostics(
            observed_samples,
            historical_samples,
            probability,
            lower_tail,
            upper_tail,
            tied_historical,
            tied_observed,
            clipped,
            precipitation_diagnostics
        )
    )
}

# Execute empirical Quantile Mapping for one aligned univariate signal group
# and return the common DailyAdjustedSeries contract.
qm__apply_group <- function(inputs, settings, key) {
    resolved <- qm__settings(settings)
    variable <- names(settings)[[1L]]
    series <- qm__inputs(
        inputs,
        variable,
        resolved$distribution_model
    )
    mapped <- qm__adjust_values(series, resolved, key, variable)
    future <- series$model_future
    future[["value"]] <- mapped$value

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = "quantile_mapping",
        settings = resolved,
        provenance = list(
            method = "quantile_mapping",
            references = QM_REFERENCES,
            group_key = key,
            output_backbone = "model_future",
            diagnostics = mapped$diagnostics
        )
    )
}

# Return one explicit diagnostic string when Quantile Mapping violates the
# package-native future-model output contract.
qm__validate_result <- function(value, inputs, key) {
    if (!S7::S7_inherits(value, DailyAdjustedSeries)) {
        return(
            "Quantile Mapping must return a DailyAdjustedSeries object."
        )
    }
    if (!identical(value@output_role, "model_future")) {
        return(
            "Quantile Mapping output must retain the `model_future` role."
        )
    }
    TRUE
}

# Construct the reusable Quantile Mapping signal with three explicit daily
# input roles and method-evidence-aware variable alternatives.
qm__component <- function() {
    alternatives <- as.list(c(
        QM_PUBLISHED_VARIABLES,
        QM_EXPERIMENTAL_VARIABLES
    ))
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
        name = "quantile_mapping_daily",
        label = "Daily Quantile Mapping",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = TRUE,
        profiles = qm__profiles(),
        apply_group = qm__apply_group,
        operations = list(validate_result = qm__validate_result),
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            references = QM_REFERENCES,
            stochastic_operation = "precipitation_dry_day_randomization",
            empirical_conventions = list(
                cdf = "linear_interpolation",
                inverse_cdf = "linear_type_7",
                ties = "average_rank",
                tails = "clamp"
            )
        )
    )
}

# Register Quantile Mapping once so package load and repeated tests share one
# discoverable process-local component.
qm__register_component <- function() {
    component <- qm__component()
    key <- component__registry_key(component@stage, component@name)
    if (!exists(
        key,
        envir = WEATHER_COMPONENT_REGISTRY,
        inherits = FALSE
    )) {
        component__register(component)
    }
    invisible(NULL)
}
