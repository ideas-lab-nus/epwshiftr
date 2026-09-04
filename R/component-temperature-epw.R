# Shared temperature-to-EPW components {{{

# The reusable hourly projectors consume only a numerical tolerance. Signal and
# EPW-header options remain owned by their corresponding stages.
EPW_MORPH_TEMPERATURE_PROJECTION_OPTIONS <- list(tolerance = 1e-8)

# Preserve baseline specific humidity after changing dry-bulb temperature and
# expose the common physical result for method-specific diagnostics.
temperature__moisture <- function(weather, temperature) {
    physical <- epwphys__apply(
        EpwPhysicalRequest(
            template = weather,
            fields = list(
                dry_bulb_temperature = as.numeric(temperature)
            ),
            provenance = list(adapter = "daily_temperature")
        ),
        epwphys__policy("preserve_specific_humidity")
    )
    humidity <- physical@state$humidity
    list(
        weather = physical@weather,
        relative_humidity = physical@weather[["relative_humidity"]],
        dew_point_temperature = physical@weather[["dew_point_temperature"]],
        baseline_specific_humidity = humidity$baseline_specific_humidity,
        specific_humidity = humidity$specific_humidity,
        status = humidity$status,
        physical = physical
    )
}

# Reduce one hourly projection to auditable daily targets and numerical closure
# values shared by POWER, BTWS, and Eames temperature workflows.
temperature__factor_rows <- function(targets, projected) {
    method_columns <- intersect(
        c(
            "shape_exponent", "btws_scale", "btws_m", "btws_n",
            "btws_fallback_reason"
        ),
        names(projected)
    )
    projection_columns <- c(
        "dry_bulb_temperature", "target_mean", "target_minimum",
        "target_maximum", "projected_mean", "projected_minimum",
        "projected_maximum", "dtr_status", "projection_status",
        method_columns, "boundary_jump", "boundary_jump_change"
    )
    # Explicit .SD access keeps package checks free from data.table NSE notes
    # while preserving one diagnostic value for every projected target day.
    daily_projection <- projected[, {
        row <- list(
            baseline_mean = mean(.SD[["dry_bulb_temperature"]]),
            baseline_minimum = min(.SD[["dry_bulb_temperature"]]),
            baseline_maximum = max(.SD[["dry_bulb_temperature"]]),
            target_mean = unique(.SD[["target_mean"]]),
            target_minimum = unique(.SD[["target_minimum"]]),
            target_maximum = unique(.SD[["target_maximum"]]),
            projected_mean = unique(.SD[["projected_mean"]]),
            projected_minimum = unique(.SD[["projected_minimum"]]),
            projected_maximum = unique(.SD[["projected_maximum"]]),
            dtr_status = unique(.SD[["dtr_status"]]),
            projection_status = unique(.SD[["projection_status"]])
        )
        for (column in method_columns) {
            row[[column]] <- unique(.SD[[column]])
        }
        row$boundary_jump <- unique(.SD[["boundary_jump"]])
        row$boundary_jump_change <-
            unique(.SD[["boundary_jump_change"]])
        row
    }, by = "target_day", .SDcols = projection_columns]
    factors <- merge(
        data.table::copy(targets),
        daily_projection,
        by = c("target_day", "dtr_status"),
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
        j = "minimum_closure_error",
        value = factors[["projected_minimum"]] - factors[["target_minimum"]]
    )
    data.table::set(
        factors,
        j = "maximum_closure_error",
        value = factors[["projected_maximum"]] - factors[["target_maximum"]]
    )
    data.table::setorderv(factors, "target_day")
    factors[]
}

# Preserve the baseline EPW day order after a temperature signal has produced
# one successful target group.
temperature__sequence_generate <- function(
    data,
    inputs,
    context,
    options
) {
    signal__single_value(data, "Daily temperature")
}

# Select and validate only options owned by the shared hourly temperature
# projection contract, excluding signal overrides and header policies.
temperature__projection_options <- function(options) {
    names <- intersect(
        names(options),
        names(EPW_MORPH_TEMPERATURE_PROJECTION_OPTIONS)
    )
    temperature__backend_options(
        options[names],
        defaults = EPW_MORPH_TEMPERATURE_PROJECTION_OPTIONS,
        label = "Temperature projection",
        unknown_label = "temperature projection"
    )
}

# Run one selected hourly projector and assemble the common payload consumed by
# the shared physical closure component.
temperature__hourly_result <- function(data, options, projector) {
    checkmate::assert_function(projector)
    options <- temperature__projection_options(options)
    baseline <- data$baseline
    targets <- data$targets
    projected <- projector(
        baseline$template,
        targets,
        value = "dry_bulb_temperature",
        day = "target_day",
        hour = "hour",
        tolerance = options$tolerance
    )
    # The kernel guarantees caller-order output, so this private row key can be
    # added after projection without colliding with its own working column.
    data.table::set(
        projected,
        j = ".daily_row",
        value = seq_len(nrow(projected))
    )
    factors <- temperature__factor_rows(targets, projected)

    # Join target deltas back to every hourly row before physical closure.
    target_columns <- c(
        "target_day", "annual_phase", "mean_delta", "minimum_delta",
        "maximum_delta", "dtr_delta"
    )
    hourly <- merge(
        projected,
        targets[, target_columns, with = FALSE],
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setorderv(hourly, ".daily_row")
    list(
        baseline = baseline,
        targets = targets,
        projected = projected,
        factors = factors,
        hourly = hourly
    )
}

# Close humidity through the shared physical policy while preserving the
# established POWER, BTWS, and Eames diagnostic columns and messages.
temperature__physics_apply <- function(
    data,
    inputs,
    context,
    options
) {
    baseline <- data$baseline
    hourly <- data$hourly
    factors <- data$factors
    moisture <- temperature__moisture(
        baseline$weather,
        hourly[["temperature_projected"]]
    )

    weather <- data.table::copy(moisture$weather)

    method_diagnostic_values <- if ("shape_exponent" %in% names(hourly)) {
        list(
            daily_temperature_shape_exponent =
                hourly[["shape_exponent"]]
        )
    } else {
        list(
            btws_scale = hourly[["btws_scale"]],
            btws_m = hourly[["btws_m"]],
            btws_n = hourly[["btws_n"]],
            btws_fallback_reason =
                hourly[["btws_fallback_reason"]]
        )
    }
    diagnostic_values <- c(list(
        daily_target_day = hourly[["target_day"]],
        daily_annual_phase = hourly[["annual_phase"]],
        daily_temperature_mean_delta = hourly[["mean_delta"]],
        daily_temperature_minimum_delta = hourly[["minimum_delta"]],
        daily_temperature_maximum_delta = hourly[["maximum_delta"]],
        daily_temperature_dtr_delta = hourly[["dtr_delta"]],
        daily_temperature_dtr_status = hourly[["dtr_status"]],
        daily_temperature_projection_status = hourly[["projection_status"]]
    ), method_diagnostic_values, list(
        daily_temperature_target_mean = hourly[["target_mean"]],
        daily_temperature_target_minimum = hourly[["target_minimum"]],
        daily_temperature_target_maximum = hourly[["target_maximum"]],
        daily_temperature_projected_mean = hourly[["projected_mean"]],
        daily_temperature_projected_minimum = hourly[["projected_minimum"]],
        daily_temperature_projected_maximum = hourly[["projected_maximum"]],
        daily_temperature_boundary_jump = hourly[["boundary_jump"]],
        daily_temperature_boundary_jump_change =
            hourly[["boundary_jump_change"]],
        daily_temperature_baseline_specific_humidity =
            moisture$baseline_specific_humidity,
        daily_temperature_specific_humidity = moisture$specific_humidity,
        daily_temperature_moisture_status = moisture$status
    ))
    for (name in names(diagnostic_values)) {
        data.table::set(weather, j = name, value = diagnostic_values[[name]])
    }

    diagnostics <- list()
    if (any(factors[["dtr_status"]] == "inherited_missing_extremes")) {
        diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "warning",
            code = "daily_temperature_dtr_inherited",
            message = paste(
                "Daily temperature range was inherited from the baseline EPW",
                "because paired future and historical tasmin/tasmax were unavailable."
            ),
            variable_id = "tasmin,tasmax",
            epw_field = "dry_bulb_temperature",
            action = paste(
                "Provide paired daily tasmin and tasmax for both future and",
                "historical periods to adjust the daily temperature range."
            )
        )
    }
    if ("btws_fallback_reason" %in% names(factors)) {
        fallback <- !is.na(factors[["btws_fallback_reason"]]) &
            nzchar(factors[["btws_fallback_reason"]])
        if (any(fallback)) {
            reasons <- sort(unique(
                factors[["btws_fallback_reason"]][fallback]
            ))
            diagnostics[[length(diagnostics) + 1L]] <-
                morpher__diagnostic(
                    stage = "runtime",
                    severity = "warning",
                    code = "btws_mean_shift_fallback",
                    message = sprintf(
                        paste(
                            "BTWS used the additive mean-shift fallback",
                            "for %d target day(s): %s."
                        ),
                        sum(fallback),
                        paste(reasons, collapse = ", ")
                    ),
                    variable_id = "tas,tasmin,tasmax",
                    epw_field = "dry_bulb_temperature",
                    action = paste(
                        "Inspect btws_fallback_reason in the morphed",
                        "data artifact."
                    )
                )
        }
    }
    # Use the correction count produced by the shared physical executor so this
    # component does not independently reinterpret a closure status.
    clipped <- moisture$physical@corrections$humidity_saturation_clipped
    if (clipped > 0L) {
        diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "info",
            code = "daily_temperature_moisture_saturation_clipped",
            message = sprintf(
                "Inherited baseline moisture was clipped to saturation for %d hourly row(s).",
                clipped
            ),
            epw_field = "relative_humidity",
            action = "Inspect daily_temperature_moisture_status in the morphed data artifact."
        )
    }

    list(
        epw = baseline$epw,
        weather = weather,
        projected = data$projected,
        factors = factors,
        diagnostics = morpher__bind_diagnostics(diagnostics)
    )
}

# Assemble the shared physics-closed payload into the existing backend result
# contract while leaving persistent file writes to EpwMorpher.
temperature__output_write <- function(
    data,
    inputs,
    context,
    options,
    stages
) {
    epw_morph_result(
        context,
        epw = data$epw,
        data = data$weather,
        parts = list(
            temperature = data$projected,
            daily_targets = data$factors
        ),
        diagnostics = data$diagnostics,
        factors = data$factors
    )
}

# Build the sequence, physics, and output components shared by every workflow
# that produces the package's daily temperature target representation.
temperature__component_specs <- function() {
    template <- component__input_requirement(
        "weather_template",
        representations = "epw",
        frequencies = "hour",
        calendars = "gregorian"
    )
    list(
        sequence = component__spec(
            name = "preserve_epw_sequence",
            stage = "sequence",
            label = "Preserve baseline EPW day sequence",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_temperature_targets",
            output_kinds = "daily_temperature_sequence",
            scopes = "multivariate",
            operations = list(
                generate = temperature__sequence_generate
            )
        ),
        physics = component__spec(
            name = "specific_humidity_closure",
            stage = "physics",
            label = "Specific-humidity temperature closure",
            required_inputs = list(weather_template = template),
            input_kinds = "hourly_temperature_projected",
            output_kinds = "hourly_weather_closed",
            scopes = "multivariate",
            operations = list(
                apply = temperature__physics_apply
            ),
            metadata = list(
                physical_policies = "preserve_specific_humidity"
            )
        ),
        output = component__spec(
            name = "daily_temperature_epw_result",
            stage = "output",
            label = "Daily temperature EPW result",
            required_inputs = list(weather_template = template),
            input_kinds = "hourly_weather_closed",
            output_kinds = "epw_morph_result",
            scopes = "multivariate",
            operations = list(
                write = temperature__output_write
            )
        )
    )
}

# Register the method-neutral temperature-to-EPW components once while
# preserving any explicit process-local extensions under the same keys.
temperature__register_components <- function() {
    component__register_builtins(temperature__component_specs())
}

# }}}
