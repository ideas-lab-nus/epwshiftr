#' @include weather-temperature.R
NULL

# Arima month-wise temperature workflow {{{

# The first Arima implementation isolates the published additive temperature
# path. Other variables use different additive or multiplicative equations and
# require their own units, zero handling, and hourly aggregation contracts.
EPW_MORPH_ARIMA_TEMPERATURE_METHODS <- c(tdb = "percentile_additive")

# Dry-bulb temperature consumes daily mean tas. Humidity fields are preserved
# or physically closed after the same daily factor reaches every hourly row.
EPW_MORPH_ARIMA_TEMPERATURE_RULES <- data.table::data.table(
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

# Arima fixes the change-function smoother at a nine-point moving mean repeated
# three times. Only shared deterministic output policies remain configurable.
EPW_MORPH_ARIMA_TEMPERATURE_OPTIONS <- EPW_MORPH_TEMPERATURE_OPTIONS

# These numerical conventions are explicit implementation choices because the
# publications define inverse CDFs but not an empirical quantile algorithm.
EPW_MORPH_ARIMA_QUANTILE_TYPE <- 7L
EPW_MORPH_ARIMA_SMOOTHING_WINDOW <- 9L
EPW_MORPH_ARIMA_SMOOTHING_PASSES <- 3L

# Validate the JSON-safe options used by foreground and resumed Arima recipes.
arima__temperature_options <- function(options = NULL) {
    temperature__backend_options(
        options,
        defaults = EPW_MORPH_ARIMA_TEMPERATURE_OPTIONS,
        label = "Arima temperature"
    )
}

# Declare the TMY, historical model, future model, and observed daily inputs
# required by the Arima percentile-transfer workflow.
arima__temperature_inputs <- function() {
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

# Normalize one daily temperature source and retain only the month-wise samples
# used by the published CDF construction.
arima__temperature_series <- function(data, name) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(name, min.chars = 1L)
    required <- c("variable_id", "value", "units", "frequency")
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg {name}} is missing Arima daily temperature column{?s}: {.val {missing}}."
        )
    }
    frequencies <- unique(tolower(as.character(data[["frequency"]])))
    frequencies <- frequencies[
        !is.na(frequencies) & nzchar(frequencies)
    ]
    if (!identical(frequencies, "day")) {
        shown <- if (length(frequencies)) frequencies else "<missing>"
        cli::cli_abort(
            "{.arg {name}} must use frequency {.val day}; found {.val {shown}}."
        )
    }

    source <- data.table::as.data.table(data.table::copy(data))
    source <- source[source[["variable_id"]] == "tas"]
    if (!nrow(source)) {
        cli::cli_abort("{.arg {name}} does not contain daily {.val tas}.")
    }
    units <- vapply(
        source[["units"]],
        morpher__unit_alias,
        character(1L)
    )
    unknown <- unique(
        units[is.na(units) | !units %in% c("K", "degC")]
    )
    if (length(unknown)) {
        cli::cli_abort(
            "{.arg {name}} contains unsupported temperature unit(s): {.val {unknown}}."
        )
    }
    value <- temperature__to_celsius(source[["value"]], units)
    data.table::set(source, j = "value", value = value)
    source <- morpher__resolve_calendar_columns(source, month = TRUE)
    month <- as.integer(source[["month"]])
    value <- source[["value"]]
    if (anyNA(month) || any(month < 1L | month > 12L) ||
        any(!is.finite(value))) {
        cli::cli_abort(
            "{.arg {name}} must contain finite daily {.val tas} values with valid calendar months."
        )
    }
    data.table::data.table(
        month = month,
        value = value
    )
}

# Reduce the hourly TMY to one daily mean while preserving the target-day and
# calendar-month keys needed to return a factor to all 24 hours.
arima__baseline_days <- function(baseline) {
    template <- data.table::copy(baseline$template)
    data.table::set(
        template,
        j = "month",
        value = as.integer(baseline$weather[["month"]])
    )
    days <- template[
        ,
        list(
            month = unique(.SD[["month"]]),
            baseline_daily_mean = mean(
                .SD[["dry_bulb_temperature"]]
            )
        ),
        by = "target_day",
        .SDcols = c("month", "dry_bulb_temperature")
    ]
    if (nrow(days) != 365L ||
        any(lengths(days[["month"]]) != 1L) ||
        any(!is.finite(days[["baseline_daily_mean"]]))) {
        cli::cli_abort(
            "Arima temperature requires one finite daily mean for every baseline EPW day."
        )
    }
    days[, month := as.integer(unlist(month))]
    data.table::setorderv(days, "target_day")
    days[]
}

# Apply one pass of the paper's endpoint-aware nine-point moving mean. Interior
# ranks use centered windows; the four ranks at either end use the fixed mean
# of the nearest nine ranks rather than a shorter or circular window.
arima__smooth_pass <- function(
    value,
    window = EPW_MORPH_ARIMA_SMOOTHING_WINDOW
) {
    checkmate::assert_numeric(
        value,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_int(window, lower = 1L)
    if (window %% 2L != 1L || length(value) < window) {
        cli::cli_abort(
            "Arima smoothing requires an odd window no longer than the percentile change function."
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

# Repeat the endpoint-aware moving mean exactly three times, matching the KZ-
# like smoothing procedure documented by Arima et al.
arima__smooth_change <- function(
    value,
    window = EPW_MORPH_ARIMA_SMOOTHING_WINDOW,
    passes = EPW_MORPH_ARIMA_SMOOTHING_PASSES
) {
    checkmate::assert_int(passes, lower = 1L)
    out <- as.numeric(value)
    for (pass in seq_len(passes)) {
        out <- arima__smooth_pass(out, window)
    }
    out
}

# Construct one empirical inverse-CDF change function per calendar month. A
# common midpoint probability grid permits unequal native-calendar sample
# counts while keeping historical and future quantiles directly comparable.
arima__change_functions <- function(
    historical,
    future,
    quantile_type = EPW_MORPH_ARIMA_QUANTILE_TYPE
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
        if (n_common < EPW_MORPH_ARIMA_SMOOTHING_WINDOW) {
            cli::cli_abort(
                "Arima temperature requires at least {EPW_MORPH_ARIMA_SMOOTHING_WINDOW} historical and future daily values in month {month}."
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
            smoothed_delta = arima__smooth_change(raw_delta),
            n_historical = n_historical,
            n_future = n_future,
            n_common = n_common
        )
    }
    data.table::rbindlist(rows)
}

# Evaluate the observed monthly empirical CDF at each baseline TMY daily mean,
# then interpolate the smoothed model change function at that percentile.
arima__daily_factors <- function(baseline_days, observed, functions) {
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
                "Arima temperature requires observed daily values in month {month}."
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
            percentile_clamped = percentile <
                min(change[["percentile"]]) |
                percentile > max(change[["percentile"]]),
            n_observed = length(observed_value)
        )
    }
    data.table::rbindlist(rows)
}

# Normalize all four role-addressable inputs before any CDF or percentile
# interpretation occurs.
arima__preprocess_apply <- function(inputs, context, options) {
    morpher__validate_context(context)
    options <- arima__temperature_options(options)
    template <- weather__get_input(inputs, "weather_template")
    historical <- weather__get_input(inputs, "model_historical")
    future <- weather__get_input(inputs, "model_future")
    observed <- weather__get_input(inputs, "observed_reference")
    list(
        baseline = temperature__epw_template(template@source),
        historical = arima__temperature_series(
            historical@source,
            "historical model climate"
        ),
        future = arima__temperature_series(
            future@source,
            "future model climate"
        ),
        observed = arima__temperature_series(
            observed@source,
            "observed reference weather"
        ),
        options = options
    )
}

# Keep source calendars native and reduce them to monthly distributions. This
# is the Arima method's calendar strategy: it never pairs model calendar dates
# directly with the 365 baseline EPW dates.
arima__calendar_apply <- function(data, inputs, context, options) {
    list(signal__group(
        inputs = list(
            weather_template = data$baseline,
            observed_reference = data$observed,
            model_historical = data$historical,
            model_future = data$future
        ),
        variables = "tas"
    ))
}

# Calculate the month-wise change functions and select one additive factor for
# every baseline EPW day from its observed-reference percentile.
arima__signal_apply_group <- function(inputs, settings, key) {
    functions <- arima__change_functions(
        inputs$model_historical,
        inputs$model_future
    )
    baseline_days <- arima__baseline_days(inputs$weather_template)
    list(
        baseline = inputs$weather_template,
        functions = functions,
        factors = arima__daily_factors(
            baseline_days,
            inputs$observed_reference,
            functions
        )
    )
}

# Preserve the original TMY day sequence instead of sampling or reordering
# events after the percentile-dependent climate signal has been estimated.
arima__sequence_generate <- function(data, inputs, context, options) {
    signal__single_value(data, "Arima")
}

# Apply each daily additive factor to all 24 TMY hours as specified by Arima
# equation (4)/(7), retaining the original hourly temperature profile shape.
arima__hourly_reconstruct <- function(data, inputs, context, options) {
    options <- arima__temperature_options(options)
    baseline <- data$baseline
    template <- data.table::copy(baseline$template)
    factors <- data.table::copy(data$factors)
    factor_index <- match(
        template[["target_day"]],
        factors[["target_day"]]
    )
    if (anyNA(factor_index)) {
        cli::cli_abort(
            "Arima factors must cover every baseline EPW target day."
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

# Apply either the temperature-only published comparison or the package's
# shared specific-humidity closure without changing the Arima climate signal.
arima__physics_apply <- function(data, inputs, context, options) {
    policy <- context$recipe$policy
    checkmate::assert_choice(
        policy,
        c("paper_faithful", "harmonized")
    )
    baseline <- data$baseline
    hourly <- data$hourly
    # Both published and harmonized variants pass through the shared physical
    # executor; the selected policy decides whether humidity is retained or
    # closed after the percentile-dependent temperature change.
    physical <- epwphys__apply(
        EpwPhysicalRequest(
            template = baseline$weather,
            fields = list(
                dry_bulb_temperature = hourly[["temperature_projected"]]
            ),
            provenance = list(adapter = "arima_temperature")
        ),
        epwphys__recipe_policy(context$recipe)
    )
    weather <- data.table::copy(physical@weather)
    moisture <- physical@state$humidity

    diagnostic_values <- list(
        arima_target_day = hourly[["target_day"]],
        arima_observed_percentile = hourly[["observed_percentile"]],
        arima_temperature_delta = hourly[["temperature_delta"]],
        arima_percentile_clamped = hourly[["percentile_clamped"]]
    )
    if (identical(policy, "harmonized")) {
        diagnostic_values <- c(
            diagnostic_values,
            list(
                arima_baseline_specific_humidity =
                    moisture$baseline_specific_humidity,
                arima_specific_humidity = moisture$specific_humidity,
                arima_humidity_closure_status = moisture$status
            )
        )
    }
    for (name in names(diagnostic_values)) {
        data.table::set(
            weather,
            j = name,
            value = diagnostic_values[[name]]
        )
    }

    diagnostics <- list()
    clamped <- data$factors[["percentile_clamped"]]
    if (any(clamped)) {
        diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "info",
            code = "arima_percentile_endpoint_clamped",
            message = sprintf(
                "Arima clamped %d baseline day percentile(s) to the empirical model change-function endpoints.",
                sum(clamped)
            ),
            variable_id = "tas",
            epw_field = "dry_bulb_temperature",
            action = "Inspect arima_observed_percentile and arima_percentile_clamped."
        )
    }
    if (identical(policy, "paper_faithful")) {
        # The physical policy already diagnoses unchanged humidity fields
        # against projected temperature; retain only Arima's diagnostic text.
        invalid <- physical@corrections$humidity_inconsistent
        if (invalid > 0L) {
            diagnostics[[length(diagnostics) + 1L]] <-
                morpher__diagnostic(
                    stage = "runtime",
                    severity = "warning",
                    code = "arima_temperature_only_state_not_closed",
                    message = sprintf(
                        "The Arima temperature-only comparison left %d hourly humidity state(s) inconsistent with projected dry-bulb temperature.",
                        invalid
                    ),
                    epw_field = paste(
                        "dry_bulb_temperature,dew_point_temperature,",
                        "relative_humidity",
                        sep = ""
                    ),
                    action = paste(
                        "Treat this as temperature-only paper comparison",
                        "output or select the harmonized policy."
                    )
                )
        }
    } else {
        clipped <- physical@corrections$humidity_saturation_clipped
        if (clipped > 0L) {
            diagnostics[[length(diagnostics) + 1L]] <-
                morpher__diagnostic(
                    stage = "runtime",
                    severity = "info",
                    code = "arima_humidity_saturation_clipped",
                    message = sprintf(
                        "Arima harmonized closure clipped %d hourly moisture state(s) to saturation.",
                        clipped
                    ),
                    epw_field = "dew_point_temperature,relative_humidity",
                    action = "Inspect arima_humidity_closure_status."
                )
        }
    }

    settings <- list(
        distributions = "calendar-month empirical CDFs",
        probability_grid = "midpoint ranks on min(n_historical, n_future)",
        quantile_type = EPW_MORPH_ARIMA_QUANTILE_TYPE,
        smoothing = paste(
            EPW_MORPH_ARIMA_SMOOTHING_WINDOW,
            "point moving mean repeated",
            EPW_MORPH_ARIMA_SMOOTHING_PASSES,
            "times with fixed endpoint means"
        ),
        observed_percentile = "empirical CDF P(observed <= TMY daily mean)",
        interpolation = "linear with endpoint clamping",
        hourly_equation = "x_future = x_TMY + daily_percentile_delta",
        physical_policy = if (identical(policy, "paper_faithful")) {
            "preserve_baseline_humidity_fields"
        } else {
            "specific_humidity_closure"
        }
    )

    list(
        epw = baseline$epw,
        weather = weather,
        hourly = hourly,
        functions = data$functions,
        factors = data$factors,
        diagnostics = morpher__bind_diagnostics(diagnostics),
        settings = settings
    )
}

# Return the common result while retaining raw/smoothed change functions,
# selected daily factors, and empirical-CDF conventions as inspectable parts.
arima__output_write <- function(data, inputs, context, options, stages) {
    epw_morph_result(
        context,
        epw = data$epw,
        data = data$weather,
        parts = list(
            temperature = data$hourly,
            change_functions = data$functions,
            factors = data$factors,
            settings = data$settings
        ),
        diagnostics = data$diagnostics,
        factors = data$factors
    )
}

# Define seven method-neutral stages so the monthly percentile-change signal
# and inherited hourly sequence can be replaced independently in comparisons.
arima__component_specs <- function() {
    complete_inputs <- arima__temperature_inputs()
    template <- complete_inputs$weather_template
    reference <- "https://doi.org/10.69357/asim2024.1178"
    profile <- signal__variable_profile(
        "tas",
        evidence = "published",
        references = reference,
        metadata = list(
            statistic = "calendar_month_empirical_distribution",
            change = "additive_inverse_cdf_difference"
        )
    )
    list(
        preprocess = component__spec(
            name = "monthly_percentile_temperature_inputs",
            stage = "preprocess",
            label = "Monthly percentile-temperature input normalization",
            required_inputs = complete_inputs,
            input_kinds = "role_inputs",
            output_kinds = "monthly_percentile_temperature_preprocessed",
            scopes = "multivariate",
            operations = list(apply = arima__preprocess_apply)
        ),
        calendar = component__spec(
            name = "monthly_temperature_distributions",
            stage = "calendar",
            label = "Native-calendar monthly temperature distributions",
            required_inputs = complete_inputs,
            input_kinds = "monthly_percentile_temperature_preprocessed",
            output_kinds = "monthly_temperature_samples",
            scopes = "multivariate",
            operations = list(apply = arima__calendar_apply)
        ),
        signal = signal__component(
            name = "percentile_temperature_change_function",
            label = "Percentile-dependent temperature change",
            required_inputs = complete_inputs,
            input_kinds = "monthly_temperature_samples",
            output_kinds = "daily_percentile_temperature_factors",
            scopes = "multivariate",
            profiles = list(profile),
            apply_group = arima__signal_apply_group
        ),
        sequence = component__spec(
            name = "preserve_percentile_tmy_sequence",
            stage = "sequence",
            label = "Preserve baseline TMY percentile sequence",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_percentile_temperature_factors",
            output_kinds = "percentile_temperature_sequence",
            scopes = "multivariate",
            operations = list(generate = arima__sequence_generate)
        ),
        hourly = component__spec(
            name = "daily_percentile_temperature_shift",
            stage = "hourly",
            label = "Daily percentile-dependent temperature shift",
            required_inputs = list(weather_template = template),
            input_kinds = "percentile_temperature_sequence",
            output_kinds = "percentile_temperature_hourly",
            scopes = "multivariate",
            operations = list(reconstruct = arima__hourly_reconstruct)
        ),
        physics = component__spec(
            name = "percentile_temperature_physical_policy",
            stage = "physics",
            label = "Percentile-temperature physical policy",
            required_inputs = list(weather_template = template),
            input_kinds = "percentile_temperature_hourly",
            output_kinds = "percentile_temperature_weather",
            scopes = "multivariate",
            operations = list(apply = arima__physics_apply),
            metadata = list(
                physical_policies = c(
                    "preserve_humidity_fields",
                    "preserve_specific_humidity"
                )
            )
        ),
        output = component__spec(
            name = "percentile_temperature_epw_result",
            stage = "output",
            label = "Percentile-temperature EPW result",
            required_inputs = list(weather_template = template),
            input_kinds = "percentile_temperature_weather",
            output_kinds = "epw_morph_result",
            scopes = "multivariate",
            operations = list(write = arima__output_write)
        )
    )
}

# Register the monthly percentile-temperature components once without replacing
# process-local implementations already stored under the same stable keys.
arima__register_components <- function() {
    component__register_builtins(arima__component_specs())
}

# Compose the temperature-focused Arima recipe from method-neutral stages while
# retaining its publication identity at the complete-recipe boundary.
arima__pipeline <- function() {
    arima__register_components()
    pipeline__spec(list(
        preprocess = "monthly_percentile_temperature_inputs",
        calendar = "monthly_temperature_distributions",
        signal = "percentile_temperature_change_function",
        sequence = "preserve_percentile_tmy_sequence",
        hourly = "daily_percentile_temperature_shift",
        physics = "percentile_temperature_physical_policy",
        output = "percentile_temperature_epw_result"
    ))
}

# }}}
