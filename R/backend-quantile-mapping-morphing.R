#' @include weather-temperature.R
NULL

# Quantile-mapping morphing for temperature
# Quantile-mapping morphing isolates the published additive temperature
# path. Other variables use different additive or multiplicative equations and
# require their own units, zero handling, and hourly aggregation contracts.
EPW_MORPH_QUANTILE_MAPPING_TEMPERATURE_METHODS <- c(tdb = "percentile_additive")

# Dry-bulb temperature consumes daily mean tas. Humidity fields are preserved
# or physically closed after the same daily factor reaches every hourly row.
EPW_MORPH_QUANTILE_MAPPING_TEMPERATURE_RULES <- data.table::data.table(
    step = c("tdb", "rh", "tdew"),
    epw_field = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature"
    ),
    variable_id = c("tas", NA_character_, NA_character_),
    optional_variable_id = NA_character_,
    method = c("percentile_additive", "policy", "policy"),
    required = c(TRUE, FALSE, FALSE),
    derived = c(FALSE, TRUE, TRUE),
    method_choices = list("percentile_additive", "policy", "policy")
)

# The published workflow fixes the smoother at a nine-point moving mean repeated
# three times. Only shared deterministic output policies remain configurable.
EPW_MORPH_QUANTILE_MAPPING_TEMPERATURE_OPTIONS <- EPW_MORPH_TEMPERATURE_OPTIONS

# These numerical conventions are explicit implementation choices because the
# publications define inverse CDFs but not an empirical quantile algorithm.
EPW_MORPH_QUANTILE_MAPPING_QUANTILE_TYPE <- 7L
EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_WINDOW <- 9L
EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_PASSES <- 3L

# Validate JSON-safe options used by foreground and resumed method recipes.
# quantile_mapping_morphing__temperature_options {{{
quantile_mapping_morphing__temperature_options <- function(options = NULL) {
    temperature__backend_options(
        options,
        defaults = EPW_MORPH_QUANTILE_MAPPING_TEMPERATURE_OPTIONS,
        label = "Quantile-mapping morphing"
    )
}
# }}}

# Declare the TMY, historical model, future model, and observed daily inputs
# required by the quantile-mapping morphing workflow.
# quantile_mapping_morphing__temperature_inputs {{{
quantile_mapping_morphing__temperature_inputs <- function() {
    list(
        weather_template = component__input_requirement(
            "weather_template",
            representations = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        ),
        observed_reference = component__input_requirement(
            "observed_reference",
            representations = "series",
            frequencies = "day",
            variable_sets = "tas"
        ),
        model_historical = component__input_requirement(
            "model_historical",
            representations = "series",
            frequencies = "day",
            variable_sets = "tas"
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = "day",
            variable_sets = "tas"
        )
    )
}
# }}}

# Normalize one daily temperature source and retain only the month-wise samples
# used by the published CDF construction.
# quantile_mapping_morphing__temperature_series {{{
quantile_mapping_morphing__temperature_series <- function(data, name) {
    source <- weather__normalize_climate_units(
        data,
        name = name,
        expected_frequency = "day",
        target_units = c(tas = "degC"),
        required_variables = "tas"
    )
    source <- morpher__resolve_calendar_columns(source, month = TRUE)
    month <- as.integer(source[["month"]])
    value <- source[["value"]]
    if (
        anyNA(month) || any(month < 1L | month > 12L) || any(!is.finite(value))
    ) {
        cli::cli_abort(
            "{.arg {name}} must contain finite daily {.val tas} values with valid calendar months."
        )
    }
    data.table::data.table(
        month = month,
        value = value
    )
}
# }}}

# Reduce the hourly TMY to one daily mean while preserving the target-day and
# calendar-month keys needed to return a factor to all 24 hours.
# quantile_mapping_morphing__baseline_days {{{
quantile_mapping_morphing__baseline_days <- function(baseline) {
    template <- data.table::copy(baseline$template)
    data.table::set(
        template,
        j = "month",
        value = as.integer(baseline$weather[["month"]])
    )
    days <- template[,
        list(
            month = unique(.SD[["month"]]),
            baseline_daily_mean = mean(
                .SD[["dry_bulb_temperature"]]
            )
        ),
        by = "target_day",
        .SDcols = c("month", "dry_bulb_temperature")
    ]
    if (
        nrow(days) != 365L ||
            any(lengths(days[["month"]]) != 1L) ||
            any(!is.finite(days[["baseline_daily_mean"]]))
    ) {
        cli::cli_abort(
            "Quantile-mapping morphing requires one finite daily mean for every baseline EPW day."
        )
    }
    days[, month := as.integer(unlist(month))]
    data.table::setorderv(days, "target_day")
    days[]
}
# }}}

# Apply one pass of the paper's endpoint-aware nine-point moving mean. Interior
# ranks use centered windows; the four ranks at either end use the fixed mean
# of the nearest nine ranks rather than a shorter or circular window.
# quantile_mapping_morphing__smooth_pass {{{
quantile_mapping_morphing__smooth_pass <- function(
    value,
    window = EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_WINDOW
) {
    checkmate::assert_numeric(
        value,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_int(window, lower = 1L)
    if (window %% 2L != 1L || length(value) < window) {
        cli::cli_abort(
            "Quantile-mapping morphing requires an odd smoothing window no longer than the percentile change function."
        )
    }

    half <- (window - 1L) %/% 2L
    out <- rep.int(NA_real_, length(value))
    out[seq_len(half)] <- mean(value[seq_len(window)])
    out[seq.int(length(value) - half + 1L, length(value))] <-
        mean(value[seq.int(length(value) - window + 1L, length(value))])
    interior <- seq.int(half + 1L, length(value) - half)
    for (index in interior) {
        out[[index]] <- mean(value[seq.int(index - half, index + half)])
    }
    out
}
# }}}

# Repeat the endpoint-aware moving mean exactly three times, matching the KZ-
# like smoothing procedure documented by Arima et al.
# quantile_mapping_morphing__smooth_change {{{
quantile_mapping_morphing__smooth_change <- function(
    value,
    window = EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_WINDOW,
    passes = EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_PASSES
) {
    checkmate::assert_int(passes, lower = 1L)
    out <- as.numeric(value)
    for (pass in seq_len(passes)) {
        out <- quantile_mapping_morphing__smooth_pass(out, window)
    }
    out
}
# }}}

# Construct one empirical inverse-CDF change function per calendar month. A
# common midpoint probability grid permits unequal native-calendar sample
# counts while keeping historical and future quantiles directly comparable.
# quantile_mapping_morphing__change_functions {{{
quantile_mapping_morphing__change_functions <- function(
    historical,
    future,
    quantile_type = EPW_MORPH_QUANTILE_MAPPING_QUANTILE_TYPE
) {
    checkmate::assert_data_frame(historical)
    checkmate::assert_data_frame(future)
    checkmate::assert_int(quantile_type, lower = 1L, upper = 9L)
    rows <- vector("list", 12L)
    for (month in seq_len(12L)) {
        historical_value <- historical[
            historical[["month"]] == month,
            "value"
        ][[1L]]
        future_value <- future[
            future[["month"]] == month,
            "value"
        ][[1L]]
        n_historical <- length(historical_value)
        n_future <- length(future_value)
        n_common <- min(n_historical, n_future)
        if (n_common < EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_WINDOW) {
            cli::cli_abort(
                "Quantile-mapping morphing requires at least {EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_WINDOW} historical and future daily values in month {month}."
            )
        }

        # Midpoint plotting positions avoid assigning empirical inverse CDFs
        # directly to unattained 0 and 1 probabilities.
        percentile <- (seq_len(n_common) - 0.5) / n_common
        historical_quantile <- as.numeric(stats::quantile(
            historical_value,
            probs = percentile,
            names = FALSE,
            type = quantile_type
        ))
        future_quantile <- as.numeric(stats::quantile(
            future_value,
            probs = percentile,
            names = FALSE,
            type = quantile_type
        ))
        raw_delta <- future_quantile - historical_quantile
        rows[[month]] <- data.table::data.table(
            month = month,
            rank = seq_len(n_common),
            percentile = percentile,
            historical_quantile = historical_quantile,
            future_quantile = future_quantile,
            raw_delta = raw_delta,
            smoothed_delta = quantile_mapping_morphing__smooth_change(
                raw_delta
            ),
            n_historical = n_historical,
            n_future = n_future,
            n_common = n_common
        )
    }
    data.table::rbindlist(rows)
}
# }}}

# Evaluate the observed monthly empirical CDF at each baseline TMY daily mean,
# then interpolate the smoothed model change function at that percentile.
# quantile_mapping_morphing__daily_factors {{{
quantile_mapping_morphing__daily_factors <- function(
    baseline_days,
    observed,
    functions
) {
    checkmate::assert_data_frame(baseline_days)
    checkmate::assert_data_frame(observed)
    checkmate::assert_data_frame(functions)
    rows <- vector("list", nrow(baseline_days))
    for (index in seq_len(nrow(baseline_days))) {
        day <- baseline_days[index]
        month <- day[["month"]]
        observed_value <- observed[
            observed[["month"]] == month,
            "value"
        ][[1L]]
        if (!length(observed_value)) {
            cli::cli_abort(
                "Quantile-mapping morphing requires observed daily values in month {month}."
            )
        }
        percentile <- mean(
            observed_value <= day[["baseline_daily_mean"]]
        )
        change <- functions[functions[["month"]] == month]
        delta <- stats::approx(
            x = change[["percentile"]],
            y = change[["smoothed_delta"]],
            xout = percentile,
            method = "linear",
            rule = 2,
            ties = "ordered"
        )$y
        rows[[index]] <- data.table::data.table(
            target_day = day[["target_day"]],
            month = month,
            baseline_daily_mean = day[["baseline_daily_mean"]],
            observed_percentile = percentile,
            temperature_delta = delta,
            percentile_clamped = percentile < min(change[["percentile"]]) |
                percentile > max(change[["percentile"]]),
            n_observed = length(observed_value)
        )
    }
    data.table::rbindlist(rows)
}
# }}}

# Apply each daily additive factor to all 24 TMY hours as specified by Arima
# equation (4)/(7), retaining the original hourly temperature profile shape.
# quantile_mapping_morphing__hourly_reconstruct {{{
quantile_mapping_morphing__hourly_reconstruct <- function(
    data,
    options = NULL
) {
    options <- quantile_mapping_morphing__temperature_options(options)
    baseline <- data$baseline
    template <- data.table::copy(baseline$template)
    factors <- data.table::copy(data$factors)
    factor_index <- match(
        template[["target_day"]],
        factors[["target_day"]]
    )
    if (anyNA(factor_index)) {
        cli::cli_abort(
            "Quantile-mapping morphing factors must cover every baseline EPW target day."
        )
    }
    temperature_delta <- factors[["temperature_delta"]][factor_index]
    data.table::set(
        template,
        j = "temperature_delta",
        value = temperature_delta
    )
    data.table::set(
        template,
        j = "temperature_projected",
        value = template[["dry_bulb_temperature"]] +
            temperature_delta
    )
    data.table::set(
        template,
        j = "observed_percentile",
        value = factors[["observed_percentile"]][factor_index]
    )
    data.table::set(
        template,
        j = "percentile_clamped",
        value = factors[["percentile_clamped"]][factor_index]
    )
    list(
        baseline = baseline,
        functions = data$functions,
        factors = factors,
        hourly = template,
        options = options
    )
}
# }}}

# Apply either paper-faithful humidity preservation or the package's shared
# specific-humidity closure without changing the quantile-mapping climate signal.
# quantile_mapping_morphing__physics_apply {{{
quantile_mapping_morphing__physics_apply <- function(data, context) {
    hourly <- data$hourly
    extra_diagnostics <- list()
    clamped <- data$factors[["percentile_clamped"]]
    if (any(clamped)) {
        extra_diagnostics[[1L]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "info",
            code = "quantile_mapping_morphing_percentile_endpoint_clamped",
            message = sprintf(
                "Quantile-mapping morphing clamped %d baseline day percentile(s) to the empirical model change-function endpoints.",
                sum(clamped)
            ),
            variable_id = "tas",
            epw_field = "dry_bulb_temperature",
            action = "Inspect quantile_mapping_morphing_observed_percentile and quantile_mapping_morphing_percentile_clamped."
        )
    }
    closed <- temperature__policy_physics_apply(
        baseline = data$baseline,
        temperature = hourly[["temperature_projected"]],
        policy_recipe = context$recipe,
        adapter = "quantile_mapping_morphing",
        prefix = "quantile_mapping_morphing",
        label = "Quantile-mapping morphing",
        hourly_cols = list(
            quantile_mapping_morphing_target_day = hourly[["target_day"]],
            quantile_mapping_morphing_observed_percentile = hourly[[
                "observed_percentile"
            ]],
            quantile_mapping_morphing_delta = hourly[["temperature_delta"]],
            quantile_mapping_morphing_percentile_clamped = hourly[[
                "percentile_clamped"
            ]]
        ),
        inconsistent_code = "quantile_mapping_morphing_only_state_not_closed",
        inconsistent_message = "The paper-faithful quantile-mapping morphing mode left %d hourly humidity state(s) inconsistent with projected dry-bulb temperature.",
        extra_diagnostics = extra_diagnostics
    )

    settings <- list(
        distributions = "calendar-month empirical CDFs",
        probability_grid = "midpoint ranks on min(n_historical, n_future)",
        quantile_type = EPW_MORPH_QUANTILE_MAPPING_QUANTILE_TYPE,
        smoothing = paste(
            EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_WINDOW,
            "point moving mean repeated",
            EPW_MORPH_QUANTILE_MAPPING_SMOOTHING_PASSES,
            "times with fixed endpoint means"
        ),
        observed_percentile = "empirical CDF P(observed <= TMY daily mean)",
        interpolation = "linear with endpoint clamping",
        hourly_equation = "x_future = x_TMY + daily_percentile_delta",
        physical_policy = closed$physical_policy
    )

    list(
        epw = closed$epw,
        weather = closed$weather,
        hourly = hourly,
        functions = data$functions,
        factors = data$factors,
        diagnostics = closed$diagnostics,
        settings = settings
    )
}
# }}}

# Execute quantile-mapping morphing directly on a canonical morphing context
# and return a standard epw_morph_result.
# quantile_mapping_morphing__run {{{
quantile_mapping_morphing__run <- function(context, backend = NULL) {
    morpher__validate_context(context)
    options <- quantile_mapping_morphing__temperature_options(
        context$recipe$options
    )
    inputs <- context$inputs
    baseline <- temperature__epw_template(
        weather__get_input(inputs, "weather_template")@source
    )
    historical <- quantile_mapping_morphing__temperature_series(
        weather__get_input(inputs, "model_historical")@source,
        "historical model climate"
    )
    future <- quantile_mapping_morphing__temperature_series(
        weather__get_input(inputs, "model_future")@source,
        "future model climate"
    )
    observed <- quantile_mapping_morphing__temperature_series(
        weather__get_input(inputs, "observed_reference")@source,
        "observed reference weather"
    )
    functions <- quantile_mapping_morphing__change_functions(historical, future)
    baseline_days <- quantile_mapping_morphing__baseline_days(baseline)
    factors <- quantile_mapping_morphing__daily_factors(
        baseline_days,
        observed,
        functions
    )
    hourly <- quantile_mapping_morphing__hourly_reconstruct(
        list(baseline = baseline, functions = functions, factors = factors),
        options = options
    )
    closed <- quantile_mapping_morphing__physics_apply(
        hourly,
        context = context
    )
    epw_morph_result(
        context,
        epw = closed$epw,
        data = closed$weather,
        parts = list(
            temperature = closed$hourly,
            change_functions = closed$functions,
            factors = closed$factors,
            settings = closed$settings
        ),
        diagnostics = closed$diagnostics,
        factors = closed$factors
    )
}
# }}}

# vim: fdm=marker :
