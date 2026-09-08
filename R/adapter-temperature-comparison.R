#' @include backend-arima-temperature.R backend-daily-temperature.R backend-eames-monthly-temperature.R backend-ek-daily-temperature.R backend-sobie-curry.R
NULL

# Controlled temperature comparison adapters {{{

# Stable backend identifiers keep the comparison adapters distinct from the
# publication-oriented recipes that use the same method signals.
TEMPERATURE_COMPARISON_BACKENDS <- c(
    eames = "eames_temperature_comparison",
    ek = "ek_temperature_comparison",
    arima = "arima_temperature_comparison",
    sobie_curry = "sobie_curry_temperature_comparison"
)

# The Eames comparison backend names the retained monthly signal independently
# of the BTWS reconstruction used by the publication-oriented recipe.
TEMPERATURE_COMPARISON_EAMES_METHODS <- c(
    tdb = "monthly_mean_extrema"
)

# Preserve the Eames input variables while replacing the rule label that would
# otherwise incorrectly imply that the comparison still executes BTWS.
tempcompare__eames_rules <- function() {
    rules <- data.table::copy(EPW_MORPH_DAILY_TEMPERATURE_BTWS_RULES)
    row <- which(rules[["step"]] == "tdb")
    data.table::set(
        rules,
        i = row,
        j = "method",
        value = "monthly_mean_extrema"
    )
    rules[["method_choices"]][row] <- list("monthly_mean_extrema")
    rules[]
}

# Summarize the common EPW day shape once for adapters whose native signal is
# expressed as a mean and DTR change rather than explicit extrema changes.
tempcompare__baseline_days <- function(baseline) {
    if (!is.list(baseline) ||
        !is.data.frame(baseline$template) ||
        !all(c("target_day", "dry_bulb_temperature") %in%
            names(baseline$template))) {
        cli::cli_abort(
            "Temperature comparison requires a normalized baseline EPW template."
        )
    }
    days <- data.table::as.data.table(baseline$template)[, .(
        baseline_mean = mean(.SD[["dry_bulb_temperature"]]),
        baseline_minimum = min(.SD[["dry_bulb_temperature"]]),
        baseline_maximum = max(.SD[["dry_bulb_temperature"]])
    ), by = "target_day"]
    data.table::set(
        days,
        j = "baseline_dtr",
        value = days[["baseline_maximum"]] - days[["baseline_minimum"]]
    )
    days[]
}

# Validate the common target representation before the shared POWER
# reconstruction receives method-specific climate signals.
tempcompare__targets <- function(targets, label) {
    checkmate::assert_string(label, min.chars = 1L)
    targets <- data.table::as.data.table(data.table::copy(targets))
    required <- c(
        "target_day", "annual_phase", "mean_delta", "minimum_delta",
        "maximum_delta", "dtr_delta", "dtr_status"
    )
    missing <- setdiff(required, names(targets))
    if (length(missing)) {
        cli::cli_abort(
            "{label} comparison targets lack required column{?s}: {.val {missing}}."
        )
    }
    if (nrow(targets) != 365L ||
        !setequal(as.integer(targets[["target_day"]]), seq_len(365L))) {
        cli::cli_abort(
            "{label} comparison targets must cover every day of the 365-day EPW calendar."
        )
    }
    data.table::setorderv(targets, "target_day")
    targets[]
}

# Convert Ek's daily mean and relative-DTR factors into explicit EPW-day
# extrema targets. The published signal is retained while POWER owns the common
# within-day shape used by the comparison protocol.
tempcompare__ek_sequence <- function(data, inputs, context, options) {
    value <- signal__single_value(data, "Ek temperature comparison")
    factors <- data.table::as.data.table(data.table::copy(value$targets))
    days <- tempcompare__baseline_days(value$baseline)
    targets <- merge(
        factors,
        days,
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    adjusted <- targets[["dtr_status"]] == "adjusted"
    relative_change <- ifelse(
        adjusted,
        targets[["dtr_relative_change"]],
        0
    )
    data.table::set(
        targets,
        j = "method_dtr_status",
        value = targets[["dtr_status"]]
    )
    data.table::set(
        targets,
        j = "minimum_delta",
        value = targets[["mean_delta"]] + relative_change * (
            targets[["baseline_minimum"]] - targets[["baseline_mean"]]
        )
    )
    data.table::set(
        targets,
        j = "maximum_delta",
        value = targets[["mean_delta"]] + relative_change * (
            targets[["baseline_maximum"]] - targets[["baseline_mean"]]
        )
    )
    data.table::set(
        targets,
        j = "dtr_delta",
        value = relative_change * targets[["baseline_dtr"]]
    )
    data.table::set(
        targets,
        j = "dtr_status",
        value = ifelse(
            adjusted,
            "adjusted",
            "inherited_missing_extremes"
        )
    )
    list(
        baseline = value$baseline,
        targets = tempcompare__targets(targets, "Ek"),
        method_parts = list(ek_factors = factors)
    )
}

# Convert Arima's percentile-dependent additive factor into the common daily
# target contract. Missing extrema deliberately preserve the baseline DTR, so
# the shared reconstruction is algebraically identical to an hourly shift.
tempcompare__arima_sequence <- function(data, inputs, context, options) {
    value <- signal__single_value(data, "Arima temperature comparison")
    targets <- data.table::as.data.table(data.table::copy(value$factors))
    data.table::set(
        targets,
        j = "annual_phase",
        value = daily__phase_grid(365L)[targets[["target_day"]]]
    )
    data.table::set(
        targets,
        j = "mean_delta",
        value = targets[["temperature_delta"]]
    )
    data.table::set(targets, j = "minimum_delta", value = NA_real_)
    data.table::set(targets, j = "maximum_delta", value = NA_real_)
    data.table::set(targets, j = "dtr_delta", value = NA_real_)
    data.table::set(
        targets,
        j = "dtr_status",
        value = rep.int("inherited_missing_extremes", nrow(targets))
    )
    list(
        baseline = value$baseline,
        targets = tempcompare__targets(targets, "Arima"),
        method_parts = list(
            arima_change_functions = value$functions,
            arima_factors = value$factors
        )
    )
}

# Convert the temperature portion of Sobie-Curry's thermodynamic factors into
# common mean/minimum/maximum targets. Humidity and pressure factors remain
# inspectable but are not applied in this temperature-only comparison recipe.
tempcompare__sobie_sequence <- function(data, inputs, context, options) {
    value <- signal__single_value(data, "Sobie-Curry temperature comparison")
    factors <- data.table::as.data.table(data.table::copy(value$factors))
    days <- tempcompare__baseline_days(value$baseline)
    targets <- merge(
        factors,
        days,
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    tolerance <- sobie__backend_options(options)$tolerance
    target_dtr <- targets[["baseline_dtr"]] +
        targets[["temperature_dtr_delta"]]
    adjusted <- targets[["baseline_dtr"]] > tolerance &
        is.finite(target_dtr) & target_dtr >= 0
    relative_change <- numeric(nrow(targets))
    relative_change[adjusted] <- targets[["temperature_dtr_delta"]][adjusted] /
        targets[["baseline_dtr"]][adjusted]

    # The adapter records an explicit fallback whenever the method requests a
    # range that the common bounded reconstruction cannot represent.
    data.table::set(
        targets,
        j = "method_dtr_status",
        value = ifelse(adjusted, "adjusted", "inherited_unfeasible_dtr")
    )
    data.table::set(
        targets,
        j = "mean_delta",
        value = targets[["temperature_mean_delta"]]
    )
    data.table::set(
        targets,
        j = "minimum_delta",
        value = targets[["mean_delta"]] + relative_change * (
            targets[["baseline_minimum"]] - targets[["baseline_mean"]]
        )
    )
    data.table::set(
        targets,
        j = "maximum_delta",
        value = targets[["mean_delta"]] + relative_change * (
            targets[["baseline_maximum"]] - targets[["baseline_mean"]]
        )
    )
    data.table::set(
        targets,
        j = "dtr_delta",
        value = relative_change * targets[["baseline_dtr"]]
    )
    data.table::set(
        targets,
        j = "dtr_status",
        value = ifelse(
            adjusted,
            "adjusted",
            "inherited_missing_extremes"
        )
    )
    list(
        baseline = value$baseline,
        targets = tempcompare__targets(targets, "Sobie-Curry"),
        method_parts = list(sobie_curry_factors = factors)
    )
}

# Register the three signal-to-target bridges needed when a method's native
# signal representation differs from the common daily-temperature contract.
tempcompare__component_specs <- function() {
    template <- component__input_requirement(
        "weather_template",
        representations = "epw",
        frequencies = "hour",
        calendars = "gregorian"
    )
    list(
        component__spec(
            name = "ek_temperature_comparison_sequence",
            stage = "sequence",
            label = "Ek factors to common temperature targets",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_mean_dtr_targets",
            output_kinds = "daily_temperature_sequence",
            scopes = "multivariate",
            operations = list(generate = tempcompare__ek_sequence)
        ),
        component__spec(
            name = "arima_temperature_comparison_sequence",
            stage = "sequence",
            label = "Arima factors to common temperature targets",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_percentile_temperature_factors",
            output_kinds = "daily_temperature_sequence",
            scopes = "multivariate",
            operations = list(generate = tempcompare__arima_sequence)
        ),
        component__spec(
            name = "sobie_curry_temperature_comparison_sequence",
            stage = "sequence",
            label = "Sobie-Curry factors to common temperature targets",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_thermodynamic_factors",
            output_kinds = "daily_temperature_sequence",
            scopes = "multivariate",
            operations = list(generate = tempcompare__sobie_sequence)
        )
    )
}

# Register the common POWER boundary and method-specific target adapters once.
tempcompare__register_components <- function() {
    daily__register_temperature_components()
    component__register_builtins(tempcompare__component_specs())
    invisible(NULL)
}

# Compose an Eames comparison pipeline that retains its monthly signal while
# replacing BTWS with the same POWER reconstruction used by peer methods.
tempcompare__eames_pipeline <- function() {
    tempcompare__register_components()
    eames__register_monthly_temperature_components()
    pipeline__spec(list(
        preprocess = "monthly_mean_extrema_inputs",
        calendar = "monthly_mean_extrema_climatology",
        signal = "monthly_mean_extrema_changes",
        sequence = "preserve_epw_sequence",
        hourly = "constrained_daily_temperature",
        physics = "specific_humidity_closure",
        output = "daily_temperature_epw_result"
    ))
}

# Compose an Ek comparison pipeline that changes only the signal calculation
# and its representation adapter before the shared reconstruction boundary.
tempcompare__ek_pipeline <- function() {
    tempcompare__register_components()
    ek__register_components()
    pipeline__spec(list(
        preprocess = "daily_extrema_change_inputs",
        calendar = "daily_extrema_climatology",
        signal = "daily_mean_dtr_change_factors",
        sequence = "ek_temperature_comparison_sequence",
        hourly = "constrained_daily_temperature",
        physics = "specific_humidity_closure",
        output = "daily_temperature_epw_result"
    ))
}

# Compose an Arima comparison pipeline that retains its month-wise percentile
# signal and standardizes only the downstream EPW reconstruction and closure.
tempcompare__arima_pipeline <- function() {
    tempcompare__register_components()
    arima__register_components()
    pipeline__spec(list(
        preprocess = "monthly_percentile_temperature_inputs",
        calendar = "monthly_temperature_distributions",
        signal = "percentile_temperature_change_function",
        sequence = "arima_temperature_comparison_sequence",
        hourly = "constrained_daily_temperature",
        physics = "specific_humidity_closure",
        output = "daily_temperature_epw_result"
    ))
}

# Compose a temperature-only Sobie-Curry comparison pipeline while preserving
# all original thermodynamic factors as diagnostic result parts.
tempcompare__sobie_pipeline <- function() {
    tempcompare__register_components()
    sobie__register_components()
    pipeline__spec(list(
        preprocess = "daily_thermodynamic_inputs",
        calendar = "circular_thermodynamic_climatology",
        signal = "daily_thermodynamic_change_factors",
        sequence = "sobie_curry_temperature_comparison_sequence",
        hourly = "constrained_daily_temperature",
        physics = "specific_humidity_closure",
        output = "daily_temperature_epw_result"
    ))
}

# Construct thin comparison backends without altering the existing publication
# backends or their selectable paper-faithful physical policies.
tempcompare__backend_specs <- function() {
    specs <- list(
        eames = EpwMorphBackend$new(
            name = TEMPERATURE_COMPARISON_BACKENDS[["eames"]],
            label = "Eames signal under common temperature controls",
            methods = TEMPERATURE_COMPARISON_EAMES_METHODS,
            method_choices = "monthly_mean_extrema",
            rules = tempcompare__eames_rules(),
            requires_reference = TRUE,
            pipeline = tempcompare__eames_pipeline()
        ),
        ek = EpwMorphBackend$new(
            name = TEMPERATURE_COMPARISON_BACKENDS[["ek"]],
            label = "Ek signal under common temperature controls",
            methods = EPW_MORPH_EK_DAILY_TEMPERATURE_METHODS,
            method_choices = "daily_mean_dtr",
            rules = EPW_MORPH_EK_DAILY_TEMPERATURE_RULES,
            requires_reference = TRUE,
            pipeline = tempcompare__ek_pipeline()
        ),
        arima = EpwMorphBackend$new(
            name = TEMPERATURE_COMPARISON_BACKENDS[["arima"]],
            label = "Arima signal under common temperature controls",
            methods = EPW_MORPH_ARIMA_TEMPERATURE_METHODS,
            method_choices = "percentile_additive",
            rules = EPW_MORPH_ARIMA_TEMPERATURE_RULES,
            requires_reference = TRUE,
            pipeline = tempcompare__arima_pipeline()
        ),
        sobie_curry = EpwMorphBackend$new(
            name = TEMPERATURE_COMPARISON_BACKENDS[["sobie_curry"]],
            label = "Sobie-Curry signal under common temperature controls",
            methods = EPW_MORPH_SOBIE_CURRY_METHODS,
            method_choices = unname(EPW_MORPH_SOBIE_CURRY_METHODS),
            rules = EPW_MORPH_SOBIE_CURRY_RULES,
            requires_reference = TRUE,
            pipeline = tempcompare__sobie_pipeline()
        )
    )
    names(specs) <- unname(TEMPERATURE_COMPARISON_BACKENDS)
    specs
}

# }}}
