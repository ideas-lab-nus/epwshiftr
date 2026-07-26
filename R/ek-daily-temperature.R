# Ek daily temperature workflow {{{

# The first Ek implementation is deliberately temperature-only. The paper's
# prose and Table 2 disagree for wind and cloud, so those variables remain
# unsupported until their transformations can be reproduced without invention.
EPW_MORPH_EK_DAILY_TEMPERATURE_METHODS <- c(tdb = "ek_combined")

# Dry-bulb temperature uses paired daily extrema. Humidity fields are either
# preserved as in the temperature-only comparison or closed by policy later.
EPW_MORPH_EK_DAILY_TEMPERATURE_RULES <- data.table::data.table(
    step = c("tdb", "rh", "tdew"),
    epw_field = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature"
    ),
    variable_id = c("tasmin,tasmax", NA_character_, NA_character_),
    optional_variable_id = NA_character_,
    method = c("ek_combined", "policy", "policy"),
    required = c(TRUE, FALSE, FALSE),
    derived = c(FALSE, TRUE, TRUE),
    method_choices = list("ek_combined", "policy", "policy")
)

# Ek does not publish a smoothing window. These options therefore contain only
# numerical tolerance and the shared deterministic EPW-header policies.
EPW_MORPH_EK_DAILY_TEMPERATURE_OPTIONS <-
    EPW_MORPH_DAILY_TEMPERATURE_OPTIONS[
        setdiff(
            names(EPW_MORPH_DAILY_TEMPERATURE_OPTIONS),
            "window_days"
        )
    ]

# Validate Ek settings through the shared temperature validator without
# exposing the injected daily-window value to the method itself.
ek__daily_temperature_options <- function(options = NULL) {
    defaults <- EPW_MORPH_EK_DAILY_TEMPERATURE_OPTIONS
    if (is.null(options)) {
        options <- defaults
    } else {
        if (!is.list(options) || is.null(names(options)) ||
            any(!nzchar(names(options)))) {
            cli::cli_abort(
                "Ek daily temperature `options` must be a named list."
            )
        }
        unknown <- setdiff(names(options), names(defaults))
        if (length(unknown)) {
            cli::cli_abort(
                "Unknown Ek daily temperature option(s): {.val {unknown}}."
            )
        }
        options <- utils::modifyList(defaults, options)
    }

    validated <- daily__temperature_backend_options(c(
        list(window_days = 31L),
        options
    ))
    validated$window_days <- NULL
    validated
}

# Declare the daily extrema and baseline EPW inputs required by every Ek stage.
ek__daily_temperature_inputs <- function() {
    list(
        weather_template = component__input_requirement(
            "weather_template",
            representations = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        ),
        model_historical = component__input_requirement(
            "model_historical",
            representations = "series",
            frequencies = "day",
            variable_sets = c("tasmin", "tasmax")
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = "day",
            variable_sets = c("tasmin", "tasmax")
        )
    )
}

# Normalize one native-calendar year onto the 365 EPW phase grid with circular
# linear interpolation. This is a calendar adapter, not a smoothing window:
# every target value is determined by the two adjacent native-calendar days.
ek__interpolate_calendar_year <- function(annual_phase, value, target_phase) {
    annual_phase <- daily__check_phase(annual_phase)
    checkmate::assert_numeric(
        value,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_numeric(
        target_phase,
        lower = 0,
        upper = 1,
        finite = TRUE,
        any.missing = FALSE
    )
    if (length(annual_phase) != length(value) ||
        length(annual_phase) < 3L) {
        cli::cli_abort(
            "Each Ek calendar year must contain at least three aligned daily values."
        )
    }

    order <- order(annual_phase)
    annual_phase <- annual_phase[order]
    value <- as.numeric(value[order])
    if (anyDuplicated(annual_phase)) {
        cli::cli_abort(
            "Each Ek variable and calendar year must contain one value per annual phase."
        )
    }

    # Extend one cycle on both sides so interpolation across New Year is
    # continuous and does not privilege a Gregorian January 1 boundary.
    extended_phase <- c(annual_phase - 1, annual_phase, annual_phase + 1)
    extended_value <- rep.int(value, 3L)
    stats::approx(
        x = extended_phase,
        y = extended_value,
        xout = target_phase,
        method = "linear",
        ties = "ordered",
        rule = 2
    )$y
}

# Reproduce Ek's day-of-year baseline construction while adapting each native
# CF calendar year to the common 365-day coordinate before averaging years.
ek__daily_temperature_climatology <- function(data, name) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(name, min.chars = 1L)
    required <- c("variable_id", "annual_phase", "value")
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg {name}} is missing Ek daily column{?s}: {.val {missing}}."
        )
    }

    source <- data.table::as.data.table(data.table::copy(data))
    source <- morpher__resolve_calendar_columns(source)
    if (!"year" %in% names(source) || anyNA(source[["year"]])) {
        cli::cli_abort(
            "{.arg {name}} must provide a source-calendar year for every daily value."
        )
    }
    source <- source[
        get("variable_id") %in% c("tasmin", "tasmax")
    ]
    variables <- sort(unique(as.character(source[["variable_id"]])))
    missing_variables <- setdiff(c("tasmin", "tasmax"), variables)
    if (length(missing_variables)) {
        cli::cli_abort(
            "{.arg {name}} is missing required variable{?s}: {.val {missing_variables}}."
        )
    }
    source_phase <- daily__check_phase(
        source[["annual_phase"]],
        sprintf("%s annual_phase", name)
    )
    source_value <- as.numeric(source[["value"]])
    if (any(!is.finite(source_value))) {
        cli::cli_abort(
            "{.arg {name}} must contain finite daily temperature values."
        )
    }
    data.table::set(source, j = "annual_phase", value = source_phase)
    data.table::set(source, j = "value", value = source_value)

    target_phase <- daily__phase_grid(365L)
    yearly <- source[
        ,
        data.table::data.table(
            target_day = seq_len(365L),
            annual_phase = target_phase,
            value = ek__interpolate_calendar_year(
                .SD[["annual_phase"]],
                .SD[["value"]],
                target_phase
            )
        ),
        by = c("variable_id", "year"),
        .SDcols = c("annual_phase", "value")
    ]

    # Ek used 30-year periods. The package records the actual contributing
    # years rather than silently rejecting shorter controlled test periods.
    climatology <- yearly[
        ,
        list(
            climatology = mean(.SD[["value"]]),
            n_years = .N
        ),
        by = c("variable_id", "target_day", "annual_phase"),
        .SDcols = "value"
    ]
    data.table::setorderv(
        climatology,
        c("variable_id", "target_day")
    )
    climatology[]
}

# Convert aligned tasmin/tasmax baselines into Ek temperature change factors.
# The relative DTR change is the Belcher-compatible interpretation that makes
# zero climate change an identity and satisfies the mean/variance statements
# accompanying Ek equation (5).
ek__daily_temperature_factors <- function(
    future_climatology,
    historical_climatology,
    tolerance = 1e-8
) {
    checkmate::assert_data_frame(future_climatology)
    checkmate::assert_data_frame(historical_climatology)
    checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
    keys <- c("variable_id", "target_day", "annual_phase")
    aligned <- merge(
        data.table::as.data.table(data.table::copy(future_climatology)),
        data.table::as.data.table(data.table::copy(historical_climatology)),
        by = keys,
        all = TRUE,
        suffixes = c("_future", "_historical"),
        sort = FALSE
    )
    required <- c(
        "climatology_future", "climatology_historical",
        "n_years_future", "n_years_historical"
    )
    missing <- setdiff(required, names(aligned))
    if (length(missing) || nrow(aligned) != 730L ||
        any(!is.finite(aligned[["climatology_future"]])) ||
        any(!is.finite(aligned[["climatology_historical"]]))) {
        cli::cli_abort(
            "Matching future and historical Ek tasmin/tasmax climatologies are required for all 365 target days."
        )
    }

    factors <- data.table::data.table(
        target_day = seq_len(365L),
        annual_phase = daily__phase_grid(365L)
    )
    for (variable in c("tasmin", "tasmax")) {
        rows <- aligned[aligned[["variable_id"]] == variable]
        rows <- rows[match(factors[["target_day"]], rows[["target_day"]])]
        statistic <- if (identical(variable, "tasmin")) {
            "minimum"
        } else {
            "maximum"
        }
        for (period in c("future", "historical")) {
            data.table::set(
                factors,
                j = sprintf("%s_%s", period, statistic),
                value = rows[[sprintf("climatology_%s", period)]]
            )
            data.table::set(
                factors,
                j = sprintf("n_years_%s_%s", period, statistic),
                value = as.integer(rows[[sprintf("n_years_%s", period)]])
            )
        }
    }

    invalid <- factors[["future_maximum"]] <
        factors[["future_minimum"]] |
        factors[["historical_maximum"]] <
            factors[["historical_minimum"]]
    if (any(invalid)) {
        cli::cli_abort(
            "Ek daily extrema must satisfy {.val tasmax >= tasmin} in both periods."
        )
    }

    historical_mean <- (
        factors[["historical_minimum"]] +
            factors[["historical_maximum"]]
    ) / 2
    future_mean <- (
        factors[["future_minimum"]] +
            factors[["future_maximum"]]
    ) / 2
    historical_dtr <- factors[["historical_maximum"]] -
        factors[["historical_minimum"]]
    future_dtr <- factors[["future_maximum"]] -
        factors[["future_minimum"]]
    adjusted <- historical_dtr > tolerance
    relative_change <- rep.int(0, nrow(factors))
    relative_change[adjusted] <- (
        future_dtr[adjusted] - historical_dtr[adjusted]
    ) / historical_dtr[adjusted]

    values <- list(
        historical_mean = historical_mean,
        future_mean = future_mean,
        historical_dtr = historical_dtr,
        future_dtr = future_dtr,
        mean_delta = future_mean - historical_mean,
        dtr_relative_change = relative_change,
        dtr_ratio = 1 + relative_change,
        dtr_status = ifelse(
            adjusted,
            "adjusted",
            "inherited_zero_historical_dtr"
        )
    )
    for (column in names(values)) {
        data.table::set(factors, j = column, value = values[[column]])
    }
    factors[]
}

# Build daily Ek factors directly from two normalized daily temperature sources.
ek__daily_temperature_targets <- function(
    future,
    historical,
    tolerance = 1e-8
) {
    ek__daily_temperature_factors(
        ek__daily_temperature_climatology(
            future,
            "future climate"
        ),
        ek__daily_temperature_climatology(
            historical,
            "historical climate"
        ),
        tolerance
    )
}

# Normalize role-addressable inputs and preserve Ek's lack of a smoothing
# setting before any calendar or climate-signal calculation occurs.
ek__preprocess_apply <- function(inputs, context, options) {
    morpher__validate_context(context)
    options <- ek__daily_temperature_options(options)
    future <- weather__get_input(inputs, "model_future")
    historical <- weather__get_input(inputs, "model_historical")
    template <- weather__get_input(inputs, "weather_template")
    list(
        baseline = daily__temperature_epw_template(template@source),
        future = daily__temperature_backend_climate(
            future@source,
            "future climate"
        ),
        historical = daily__temperature_backend_climate(
            historical@source,
            "historical climate"
        ),
        options = options
    )
}

# Map each native CF year to the shared annual coordinate before averaging the
# day-of-year baselines described by Ek.
ek__calendar_apply <- function(data, inputs, context, options) {
    future <- ek__daily_temperature_climatology(
        data$future,
        "future climate"
    )
    historical <- ek__daily_temperature_climatology(
        data$historical,
        "historical climate"
    )
    data.table::set(
        future,
        j = "ek_tolerance",
        value = rep.int(data$options$tolerance, nrow(future))
    )
    data.table::set(
        historical,
        j = "ek_tolerance",
        value = rep.int(data$options$tolerance, nrow(historical))
    )
    list(signal__group(
        inputs = list(
            weather_template = data$baseline,
            model_historical = historical,
            model_future = future
        ),
        variables = c("tasmin", "tasmax")
    ))
}

# Calculate the Ek mean shift and DTR-relative-change signal after all calendar
# interpretation has been completed by the preceding component.
ek__signal_apply_group <- function(inputs, settings, key) {
    tolerance <- unique(inputs$model_future[["ek_tolerance"]])
    if (length(tolerance) != 1L || !is.finite(tolerance)) {
        cli::cli_abort(
            "Ek calendar output must retain one finite numerical tolerance."
        )
    }
    list(
        baseline = inputs$weather_template,
        targets = ek__daily_temperature_factors(
            inputs$model_future,
            inputs$model_historical,
            tolerance
        )
    )
}

# Retain the baseline EPW day order so the Ek comparison changes only the
# climate signal and published within-day transformation.
ek__sequence_generate <- function(data, inputs, context, options) {
    if (!S7::S7_inherits(data, SignalExecutionResult) ||
        length(data@values) != 1L ||
        is.null(data@values[[1L]])) {
        cli::cli_abort(
            "Ek sequence input must contain one successful signal group."
        )
    }
    data@values[[1L]]
}

# Apply Ek equation (5) day by day using relative DTR change as the anomaly
# multiplier: x = x0 + delta_mean + alpha_dtr * (x0 - daily_mean_x0).
ek__hourly_reconstruct <- function(data, inputs, context, options) {
    options <- ek__daily_temperature_options(options)
    baseline <- data$baseline
    targets <- data.table::as.data.table(data.table::copy(data$targets))
    template <- data.table::as.data.table(
        data.table::copy(baseline$template)
    )
    target_index <- match(
        template[["target_day"]],
        targets[["target_day"]]
    )
    if (anyNA(target_index)) {
        cli::cli_abort(
            "Ek factors must cover every baseline EPW target day."
        )
    }

    projected_value <- numeric(nrow(template))
    daily_rows <- vector("list", 365L)
    for (target_day in seq_len(365L)) {
        index <- which(template[["target_day"]] == target_day)
        factor <- targets[target_day]
        baseline_value <- template[["dry_bulb_temperature"]][index]
        baseline_mean <- mean(baseline_value)
        relative_change <- factor[["dtr_relative_change"]]
        if (!identical(factor[["dtr_status"]], "adjusted")) {
            relative_change <- 0
        }

        # This form preserves the original hourly timing and ranks while
        # changing the daily mean and range by the model-derived factors.
        projected <- baseline_value +
            factor[["mean_delta"]] +
            relative_change * (baseline_value - baseline_mean)
        projected_value[index] <- projected
        daily_rows[[target_day]] <- data.table::data.table(
            target_day = target_day,
            annual_phase = factor[["annual_phase"]],
            baseline_mean = baseline_mean,
            baseline_minimum = min(baseline_value),
            baseline_maximum = max(baseline_value),
            baseline_dtr = max(baseline_value) - min(baseline_value),
            target_mean = baseline_mean + factor[["mean_delta"]],
            target_dtr = (
                max(baseline_value) - min(baseline_value)
            ) * factor[["dtr_ratio"]],
            projected_mean = mean(projected),
            projected_minimum = min(projected),
            projected_maximum = max(projected),
            projected_dtr = max(projected) - min(projected),
            projection_status = if (identical(
                factor[["dtr_status"]],
                "adjusted"
            )) {
                "ek_combined"
            } else {
                "mean_shift_zero_historical_dtr"
            }
        )
    }
    factors <- merge(
        targets,
        data.table::rbindlist(daily_rows),
        by = c("target_day", "annual_phase"),
        all.x = TRUE,
        sort = FALSE
    )
    data.table::set(
        factors,
        j = "mean_closure_error",
        value = factors[["projected_mean"]] - factors[["target_mean"]]
    )
    data.table::set(
        factors,
        j = "dtr_closure_error",
        value = factors[["projected_dtr"]] - factors[["target_dtr"]]
    )

    projected <- data.table::copy(template)
    data.table::set(
        projected,
        j = "temperature_projected",
        value = projected_value
    )
    hourly_columns <- c(
        "annual_phase", "mean_delta", "dtr_relative_change",
        "dtr_ratio", "dtr_status"
    )
    for (column in hourly_columns) {
        data.table::set(
            projected,
            j = column,
            value = targets[[column]][target_index]
        )
    }

    daily_first_baseline <- template[
        ,
        .SD[["dry_bulb_temperature"]][[1L]],
        by = "target_day",
        .SDcols = "dry_bulb_temperature"
    ][["V1"]]
    daily_last_baseline <- template[
        ,
        .SD[["dry_bulb_temperature"]][[.N]],
        by = "target_day",
        .SDcols = "dry_bulb_temperature"
    ][["V1"]]
    daily_first_projected <- projected[
        ,
        .SD[["temperature_projected"]][[1L]],
        by = "target_day",
        .SDcols = "temperature_projected"
    ][["V1"]]
    daily_last_projected <- projected[
        ,
        .SD[["temperature_projected"]][[.N]],
        by = "target_day",
        .SDcols = "temperature_projected"
    ][["V1"]]
    baseline_jump <- daily_first_baseline -
        daily__cyclic_previous(daily_last_baseline)
    projected_jump <- daily_first_projected -
        daily__cyclic_previous(daily_last_projected)
    jump_change <- projected_jump - baseline_jump
    for (column in c(
        "boundary_jump",
        "boundary_jump_change",
        "projection_status"
    )) {
        values <- switch(
            column,
            boundary_jump = projected_jump,
            boundary_jump_change = jump_change,
            projection_status = factors[["projection_status"]]
        )
        data.table::set(
            factors,
            j = column,
            value = values
        )
        data.table::set(
            projected,
            j = column,
            value = values[match(
                projected[["target_day"]],
                factors[["target_day"]]
            )]
        )
    }
    data.table::setorderv(factors, "target_day")

    list(
        baseline = baseline,
        targets = targets,
        projected = projected,
        factors = factors,
        hourly = projected,
        options = options
    )
}

# Select the temperature-only paper comparison or shared humidity closure
# without changing the preceding Ek signal and hourly transformation.
ek__physics_apply <- function(data, inputs, context, options) {
    baseline <- data$baseline
    hourly <- data$hourly
    factors <- data$factors
    policy <- context$recipe$policy
    checkmate::assert_choice(
        policy,
        c("paper_faithful", "harmonized")
    )
    weather <- data.table::copy(baseline$weather)
    data.table::set(
        weather,
        j = "dry_bulb_temperature",
        value = hourly[["temperature_projected"]]
    )

    moisture <- NULL
    if (identical(policy, "harmonized")) {
        moisture <- daily__temperature_moisture(
            baseline$weather,
            hourly[["temperature_projected"]]
        )
        data.table::set(
            weather,
            j = "relative_humidity",
            value = moisture$relative_humidity
        )
        data.table::set(
            weather,
            j = "dew_point_temperature",
            value = moisture$dew_point_temperature
        )
    }

    diagnostic_values <- list(
        ek_target_day = hourly[["target_day"]],
        ek_annual_phase = hourly[["annual_phase"]],
        ek_temperature_mean_delta = hourly[["mean_delta"]],
        ek_temperature_dtr_relative_change =
            hourly[["dtr_relative_change"]],
        ek_temperature_dtr_ratio = hourly[["dtr_ratio"]],
        ek_temperature_dtr_status = hourly[["dtr_status"]],
        ek_temperature_projection_status =
            hourly[["projection_status"]],
        ek_temperature_boundary_jump = hourly[["boundary_jump"]],
        ek_temperature_boundary_jump_change =
            hourly[["boundary_jump_change"]]
    )
    if (identical(policy, "harmonized")) {
        diagnostic_values <- c(
            diagnostic_values,
            list(
                ek_baseline_specific_humidity =
                    moisture$baseline_specific_humidity,
                ek_specific_humidity = moisture$specific_humidity,
                ek_humidity_closure_status = moisture$status
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
    fallback <- factors[["dtr_status"]] ==
        "inherited_zero_historical_dtr"
    if (any(fallback)) {
        diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "warning",
            code = "ek_zero_historical_model_dtr",
            message = sprintf(
                "Ek inherited the baseline EPW DTR for %d target day(s) with zero historical model DTR.",
                sum(fallback)
            ),
            variable_id = "tasmin,tasmax",
            epw_field = "dry_bulb_temperature",
            action = "Inspect ek_temperature_dtr_status."
        )
    }
    if (identical(policy, "paper_faithful")) {
        invalid <- (
            weather[["relative_humidity"]] < 0 |
                weather[["relative_humidity"]] > 100 |
                weather[["dew_point_temperature"]] >
                    weather[["dry_bulb_temperature"]]
        )
        invalid[is.na(invalid)] <- TRUE
        if (any(invalid)) {
            diagnostics[[length(diagnostics) + 1L]] <-
                morpher__diagnostic(
                    stage = "runtime",
                    severity = "warning",
                    code = "ek_temperature_only_state_not_closed",
                    message = sprintf(
                        "The Ek temperature-only comparison left %d hourly humidity state(s) inconsistent with projected dry-bulb temperature.",
                        sum(invalid)
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
        clipped <- moisture$status == "saturation_clipped"
        if (any(clipped)) {
            diagnostics[[length(diagnostics) + 1L]] <-
                morpher__diagnostic(
                    stage = "runtime",
                    severity = "info",
                    code = "ek_humidity_saturation_clipped",
                    message = sprintf(
                        "Ek harmonized closure clipped %d hourly moisture state(s) to saturation.",
                        sum(clipped)
                    ),
                    epw_field = "dew_point_temperature,relative_humidity",
                    action = "Inspect ek_humidity_closure_status."
                )
        }
    }

    settings <- list(
        climatology = "per-year daily values averaged after 365-phase mapping",
        smoothing = "none",
        temperature_mean = "(tasmin + tasmax) / 2",
        dtr_relative_change = paste(
            "(DTR_future - DTR_historical) / DTR_historical"
        ),
        hourly_equation = paste(
            "x0 + delta_mean + alpha_dtr *",
            "(x0 - baseline_daily_mean)"
        ),
        physical_policy = if (identical(policy, "paper_faithful")) {
            "preserve_baseline_humidity_fields"
        } else {
            "specific_humidity_closure"
        }
    )

    list(
        epw = baseline$epw,
        weather = weather,
        projected = data$projected,
        factors = factors,
        diagnostics = morpher__bind_diagnostics(diagnostics),
        settings = settings
    )
}

# Return the common EPW morph result with Ek factors and formula settings
# retained as separately inspectable parts.
ek__output_write <- function(data, inputs, context, options, stages) {
    epw_morph_result(
        context,
        epw = data$epw,
        data = data$weather,
        parts = list(
            temperature = data$projected,
            factors = data$factors,
            settings = data$settings
        ),
        diagnostics = data$diagnostics,
        factors = data$factors
    )
}

# Define the seven Ek stages so its signal and hourly equation remain
# independently inspectable and replaceable in controlled comparisons.
ek__component_specs <- function() {
    complete_inputs <- ek__daily_temperature_inputs()
    template <- complete_inputs$weather_template
    reference <- paste0(
        "https://dspace.library.uvic.ca/items/",
        "5e8e6684-c704-4d2e-8480-2c81bdbafde9"
    )
    profiles <- lapply(
        c("tasmin", "tasmax"),
        function(variable) {
            signal__variable_profile(
                variable,
                evidence = "published",
                references = reference,
                metadata = list(
                    statistic = "day_of_year_30_year_baseline"
                )
            )
        }
    )

    list(
        preprocess = component__spec(
            name = "ek_daily_temperature_inputs",
            stage = "preprocess",
            label = "Ek daily temperature input normalization",
            required_inputs = complete_inputs,
            input_kinds = "role_inputs",
            output_kinds = "ek_daily_temperature_preprocessed",
            scopes = "multivariate",
            operations = list(apply = ek__preprocess_apply)
        ),
        calendar = component__spec(
            name = "ek_daily_calendar_baselines",
            stage = "calendar",
            label = "Ek day-of-year climate baselines",
            required_inputs = complete_inputs,
            input_kinds = "ek_daily_temperature_preprocessed",
            output_kinds = "ek_daily_temperature_climatologies",
            scopes = "multivariate",
            operations = list(apply = ek__calendar_apply)
        ),
        signal = signal__component(
            name = "ek_daily_temperature_factors",
            label = "Ek daily temperature change factors",
            required_inputs = complete_inputs,
            input_kinds = "ek_daily_temperature_climatologies",
            output_kinds = "ek_daily_temperature_targets",
            scopes = "multivariate",
            profiles = profiles,
            apply_group = ek__signal_apply_group
        ),
        sequence = component__spec(
            name = "ek_preserve_epw_sequence",
            stage = "sequence",
            label = "Preserve baseline EPW sequence for Ek",
            required_inputs = list(weather_template = template),
            input_kinds = "ek_daily_temperature_targets",
            output_kinds = "ek_daily_temperature_sequence",
            scopes = "multivariate",
            operations = list(generate = ek__sequence_generate)
        ),
        hourly = component__spec(
            name = "ek_daily_combined_temperature",
            stage = "hourly",
            label = "Ek daily shift-and-stretch temperature",
            required_inputs = list(weather_template = template),
            input_kinds = "ek_daily_temperature_sequence",
            output_kinds = "ek_daily_temperature_hourly",
            scopes = "multivariate",
            operations = list(reconstruct = ek__hourly_reconstruct)
        ),
        physics = component__spec(
            name = "ek_temperature_physical_policy",
            stage = "physics",
            label = "Ek temperature physical policy",
            required_inputs = list(weather_template = template),
            input_kinds = "ek_daily_temperature_hourly",
            output_kinds = "ek_daily_temperature_weather",
            scopes = "multivariate",
            operations = list(apply = ek__physics_apply)
        ),
        output = component__spec(
            name = "ek_daily_temperature_epw_result",
            stage = "output",
            label = "Ek daily temperature EPW result",
            required_inputs = list(weather_template = template),
            input_kinds = "ek_daily_temperature_weather",
            output_kinds = "epw_morph_result",
            scopes = "multivariate",
            operations = list(write = ek__output_write)
        )
    )
}

# Register Ek components once without replacing process-local implementations
# already stored under the same stable component keys.
ek__register_components <- function() {
    components <- ek__component_specs()
    for (stage in names(components)) {
        key <- component__registry_key(stage, components[[stage]]@name)
        if (!exists(
            key,
            envir = WEATHER_COMPONENT_REGISTRY,
            inherits = FALSE
        )) {
            component__register(components[[stage]])
        }
    }
    invisible(NULL)
}

# Compose the temperature-focused Ek recipe from its seven declared stages.
ek__pipeline <- function() {
    ek__register_components()
    pipeline__spec(list(
        preprocess = "ek_daily_temperature_inputs",
        calendar = "ek_daily_calendar_baselines",
        signal = "ek_daily_temperature_factors",
        sequence = "ek_preserve_epw_sequence",
        hourly = "ek_daily_combined_temperature",
        physics = "ek_temperature_physical_policy",
        output = "ek_daily_temperature_epw_result"
    ))
}

# }}}
