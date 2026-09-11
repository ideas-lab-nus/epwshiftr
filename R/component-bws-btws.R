#' @include backend-bws-btws.R method-bws.R method-btws.R component-temperature-epw.R
NULL

# BWS and BTWS EPW components {{{

# Convert every adjusted BWS monthly target into a method-level diagnostic with
# the climate means, EPW baseline, requested target, applied target, and exact
# attainable interval needed to reproduce the decision.
bws_btws_epw__target_diagnostics <- function(factors, context) {
    factors <- data.table::as.data.table(data.table::copy(factors))
    adjustment <- factors[["target_adjustment"]]
    adjusted <- factors[!is.na(adjustment) & adjustment != "none"]
    if (!nrow(adjusted)) {
        return(morpher__empty_diagnostics())
    }
    case <- data.table::as.data.table(context$case)
    case_value <- function(name) {
        if (name %in% names(case)) {
            store__chr1(case[[name]][[1L]])
        } else {
            NA_character_
        }
    }
    source_id <- case_value("source_id")
    experiment_id <- case_value("experiment_id")
    variant_label <- case_value("variant_label")
    period <- case_value("period")

    rows <- vector("list", nrow(adjusted))
    for (i in seq_len(nrow(adjusted))) {
        row <- adjusted[i]
        rows[[i]] <- morpher__diagnostic(
            stage = "runtime",
            severity = "warning",
            code = "bws_target_adjusted",
            message = sprintf(
                paste(
                    "BWS adjusted %s month %d for %s/%s/%s/%s",
                    "from %.8g to %.8g",
                    "within the attainable interval [%.8g, %.8g]",
                    "(%s); GCM historical mean %.8g, GCM future mean %.8g,",
                    "and baseline EPW mean %.8g."
                ),
                row$variable_id[[1L]],
                row$month[[1L]],
                source_id,
                experiment_id,
                variant_label,
                period,
                row$requested_target_mean[[1L]],
                row$target_mean[[1L]],
                row$attainable_lower[[1L]],
                row$attainable_upper[[1L]],
                row$target_adjustment[[1L]],
                row$model_historical_mean[[1L]],
                row$model_future_mean[[1L]],
                row$baseline_mean[[1L]]
            ),
            variable_id = row$variable_id[[1L]],
            epw_field = if (identical(row$variable_id[[1L]], "clt")) {
                "total_sky_cover"
            } else {
                "global_horizontal_radiation"
            },
            period = period,
            month = row$month[[1L]],
            action = paste(
                "Inspect bws_factors for the requested and applied targets;",
                "the Historical-to-Scenario signal is retained unchanged."
            )
        )
    }
    morpher__bind_diagnostics(rows)
}

# Apply BTWS to dry-bulb temperature and BWS to the two published bounded
# variables while retaining baseline hourly ordering for physical closure.
bws_btws_epw__hourly_reconstruct <- function(
    data,
    inputs,
    context,
    options
) {
    options <- bws_btws__options(options)
    temperature <- temperature__hourly_result(
        data,
        options,
        btws__project_temperature
    )
    baseline <- temperature$baseline
    weather <- baseline$weather
    required <- c(
        "year", "month", "day", "hour",
        "global_horizontal_radiation", "diffuse_horizontal_radiation",
        "total_sky_cover", "opaque_sky_cover"
    )
    missing <- setdiff(required, names(weather))
    if (length(missing)) {
        cli::cli_abort(
            "Baseline EPW is missing BWS/BTWS weather field{?s}: {.val {missing}}."
        )
    }
    bounded <- data.table::as.data.table(
        data.table::copy(data$monthly_bws_targets)
    )
    if (nrow(bounded) != 12L ||
        !identical(sort(as.integer(bounded[["month"]])), seq_len(12L))) {
        cli::cli_abort("BWS targets must contain all 12 months.")
    }
    data.table::setorderv(bounded, "month")

    month <- as.integer(weather[["month"]])
    baseline_ghi <- as.numeric(weather[["global_horizontal_radiation"]])
    baseline_cover <- as.numeric(weather[["total_sky_cover"]])
    if (any(!is.finite(baseline_ghi) | baseline_ghi < 0) ||
        any(!is.finite(baseline_cover) |
            baseline_cover < 0 | baseline_cover > 10)) {
        cli::cli_abort(
            "Baseline EPW radiation and total sky cover must contain finite values within the BWS bounds."
        )
    }
    ghi_mean <- vapply(seq_len(12L), function(calendar_month) {
        mean(baseline_ghi[month == calendar_month])
    }, numeric(1L))
    ghi_upper <- vapply(seq_len(12L), function(calendar_month) {
        max(baseline_ghi[month == calendar_month])
    }, numeric(1L))
    cover_mean <- vapply(seq_len(12L), function(calendar_month) {
        mean(baseline_cover[month == calendar_month])
    }, numeric(1L))

    radiation <- bws__project_monthly(
        baseline_ghi,
        month,
        target_mean = ghi_mean + bounded[["rsds_delta"]],
        upper = ghi_upper,
        variable_id = "rsds",
        tolerance = options$tolerance
    )
    cloud <- bws__project_monthly(
        baseline_cover,
        month,
        target_mean = cover_mean * (1 + bounded[["clt_scale"]]),
        upper = rep.int(10, 12L),
        variable_id = "clt",
        integer = TRUE,
        tolerance = options$tolerance
    )
    opaque <- epwphys__opaque_sky_cover(
        cloud$value,
        weather[["total_sky_cover"]],
        weather[["opaque_sky_cover"]]
    )
    diffuse <- radiation__preserved_diffuse(weather, radiation$value)
    latitude <- morpher__epw_location_numeric(
        baseline$epw,
        c("latitude", "lat", "N2_latitude")
    )
    longitude <- morpher__epw_location_numeric(
        baseline$epw,
        c("longitude", "lon", "N3_longitude")
    )
    timezone <- morpher__epw_location_numeric(
        baseline$epw,
        c("time_zone", "timezone", "N4_time_zone"),
        default = 0
    )
    geometry <- solar__epw_interval_geometry(
        weather,
        latitude = latitude,
        longitude = longitude,
        timezone = timezone
    )

    bws_factors <- data.table::rbindlist(
        list(radiation$factors, cloud$factors),
        use.names = TRUE,
        fill = TRUE
    )
    bounded_row <- match(bws_factors[["month"]], bounded[["month"]])
    radiation_row <- bws_factors[["variable_id"]] == "rsds"
    data.table::set(
        bws_factors,
        j = "model_rsds_delta",
        value = bounded[["rsds_delta"]][bounded_row]
    )
    data.table::set(
        bws_factors,
        j = "model_clt_scale",
        value = bounded[["clt_scale"]][bounded_row]
    )
    data.table::set(
        bws_factors,
        j = "model_future_mean",
        value = ifelse(
            radiation_row,
            bounded[["future_rsds_mean"]][bounded_row],
            bounded[["future_clt_mean"]][bounded_row]
        )
    )
    data.table::set(
        bws_factors,
        j = "model_historical_mean",
        value = ifelse(
            radiation_row,
            bounded[["historical_rsds_mean"]][bounded_row],
            bounded[["historical_clt_mean"]][bounded_row]
        )
    )
    temperature$bws_projection <- list(
        global_horizontal = radiation$value,
        diffuse_horizontal = diffuse,
        total_sky_cover = as.integer(cloud$value),
        opaque_sky_cover = as.integer(opaque),
        geometry = geometry
    )
    temperature$method_parts <- c(
        temperature$method_parts,
        list(bws_factors = bws_factors[])
    )
    temperature
}

# Apply all BWS/BTWS candidates in one call to the common physical layer so
# humidity and shortwave closure see the same final temperature and radiation.
bws_btws_epw__physics_apply <- function(
    data,
    inputs,
    context,
    options
) {
    bounded <- data$bws_projection
    physical <- epwphys__apply(
        EpwPhysicalRequest(
            template = data$baseline$weather,
            fields = list(
                dry_bulb_temperature =
                    data$hourly[["temperature_projected"]],
                total_sky_cover = bounded$total_sky_cover,
                opaque_sky_cover = bounded$opaque_sky_cover
            ),
            shortwave = list(
                global_horizontal = bounded$global_horizontal,
                diffuse_horizontal = bounded$diffuse_horizontal
            ),
            geometry = bounded$geometry,
            provenance = list(adapter = "bws_btws_weather")
        ),
        epwphys__policy("bws_btws_weather")
    )
    result <- temperature__physics_payload(data, physical)
    result$diagnostics <- morpher__bind_diagnostics(
        result$diagnostics,
        bws_btws_epw__target_diagnostics(
            data$method_parts$bws_factors,
            context
        )
    )

    # Surface any physical-layer radiation adjustment as one compact runtime
    # diagnostic while the detailed monthly BWS factors remain in result parts.
    radiation_corrections <- sum(c(
        physical@corrections$radiation_night_values_zeroed,
        physical@corrections$radiation_negative_global_clipped,
        physical@corrections$radiation_negative_diffuse_clipped,
        physical@corrections$radiation_diffuse_above_global_clipped,
        physical@corrections$radiation_excess_beam_reallocated
    ))
    if (radiation_corrections > 0L) {
        diagnostic <- morpher__diagnostic(
            stage = "runtime",
            severity = "info",
            code = "bws_btws_shortwave_closed",
            message = sprintf(
                "BWS/BTWS shortwave physical closure adjusted %d candidate state(s).",
                radiation_corrections
            ),
            variable_id = "rsds",
            epw_field = "global_horizontal_radiation",
            action = "Inspect the physical-policy corrections and bws_factors."
        )
        result$diagnostics <- morpher__bind_diagnostics(
            result$diagnostics,
            diagnostic
        )
    }
    result
}

# Define the method-specific sequence, hourly, physical, and output stages around
# the reusable BWS, BTWS, temperature-result, solar, and EPW-physics helpers.
bws_btws_epw__component_specs <- function() {
    complete_inputs <- bws_btws__inputs()
    list(
        sequence = component__spec(
            name = "bws_btws_preserve_epw_sequence",
            stage = "sequence",
            label = "Preserve baseline EPW sequence for BWS/BTWS",
            required_inputs = complete_inputs,
            input_kinds = "bws_btws_weather_targets",
            output_kinds = "bws_btws_weather_sequence",
            scopes = "multivariate",
            operations = list(
                generate = temperature__sequence_generate
            )
        ),
        hourly = component__spec(
            name = "bws_btws_hourly_projection",
            stage = "hourly",
            label = "BWS and BTWS hourly projection",
            required_inputs = complete_inputs,
            input_kinds = "bws_btws_weather_sequence",
            output_kinds = "hourly_bws_btws_weather",
            scopes = "multivariate",
            operations = list(
                reconstruct = bws_btws_epw__hourly_reconstruct
            ),
            metadata = list(
                reconstruction = c(
                    temperature = "bounded_temperature_weighted_stretch",
                    radiation = "bounded_weighted_stretch",
                    cloud_cover = "bounded_weighted_stretch"
                )
            )
        ),
        physics = component__spec(
            name = "bws_btws_physical_closure",
            stage = "physics",
            label = "BWS/BTWS physical closure",
            required_inputs = complete_inputs,
            input_kinds = "hourly_bws_btws_weather",
            output_kinds = "hourly_weather_closed",
            scopes = "multivariate",
            operations = list(
                apply = bws_btws_epw__physics_apply
            ),
            metadata = list(
                physical_policies = "bws_btws_weather"
            )
        ),
        output = component__spec(
            name = "bws_btws_epw_result",
            stage = "output",
            label = "BWS/BTWS EPW result",
            required_inputs = complete_inputs,
            input_kinds = "hourly_weather_closed",
            output_kinds = "epw_morph_result",
            scopes = "multivariate",
            operations = list(
                write = temperature__output_write
            ),
            metadata = list(
                target_calendar = "epw_365_day"
            )
        )
    )
}

# Register the complete BWS/BTWS EPW stages without replacing a
# process-local extension that owns the same stable keys.
bws_btws_epw__register_components <- function() {
    component__register_builtins(bws_btws_epw__component_specs())
    invisible(NULL)
}

# }}}
