#' @include weather-temperature.R component-temperature-epw.R method-bws.R method-btws.R
NULL

# BWS and BTWS monthly weather workflow
# The complete backend applies BTWS to temperature and BWS to global
# shortwave radiation and cloud cover before common EPW physical closure.
EPW_MORPH_BWS_BTWS_METHODS <- c(
    tdb = "btws",
    glob_rad = "bws",
    total_cover = "bws"
)

# Declare method-owned and derived EPW fields so preflight requires every
# climate variable needed by the complete revised method.
EPW_MORPH_BWS_BTWS_RULES <- data.table::data.table(
    step = c(
        "tdb",
        "glob_rad",
        "total_cover",
        "rh",
        "tdew",
        "diff_rad",
        "norm_rad",
        "opaque_cover"
    ),
    epw_field = c(
        "dry_bulb_temperature",
        "global_horizontal_radiation",
        "total_sky_cover",
        "relative_humidity",
        "dew_point_temperature",
        "diffuse_horizontal_radiation",
        "direct_normal_radiation",
        "opaque_sky_cover"
    ),
    variable_id = c(
        "tas,tasmin,tasmax",
        "rsds",
        "clt",
        rep(NA_character_, 5L)
    ),
    optional_variable_id = NA_character_,
    method = c("btws", "bws", "bws", rep("derived", 5L)),
    required = c(rep(TRUE, 3L), rep(FALSE, 5L)),
    derived = c(rep(FALSE, 3L), rep(TRUE, 5L)),
    method_choices = list(
        "btws",
        "bws",
        "bws",
        "derived",
        "derived",
        "derived",
        "derived",
        "derived"
    )
)

# BWS and BTWS expose only the shared deterministic tolerance and EPW-header
# controls; the published BWS and BTWS equations do not add tuning options.
EPW_MORPH_BWS_BTWS_OPTIONS <- EPW_MORPH_TEMPERATURE_OPTIONS

# Validate the options shared by the complete calendar-month BWS/BTWS method.
# bws_btws__options {{{
bws_btws__options <- function(options = NULL) {
    temperature__backend_options(
        options,
        defaults = EPW_MORPH_BWS_BTWS_OPTIONS,
        label = "BWS and BTWS monthly morphing"
    )
}
# }}}

# Declare the role-addressable inputs shared by every BWS/BTWS component. Monthly
# CMIP6 values map directly to the monthly change factors defined by the paper.
# bws_btws__inputs {{{
bws_btws__inputs <- function() {
    variables <- c("tas", "tasmin", "tasmax", "rsds", "clt")
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
            frequencies = "mon",
            variable_sets = variables
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = "mon",
            variable_sets = variables
        )
    )
}
# }}}

# Normalize the five monthly CMIP6 variables to method-owned units before
# climatology aggregation. This keeps source adaptation separate from BWS/BTWS.
# bws_btws__monthly_climate {{{
bws_btws__monthly_climate <- function(data, name) {
    variables <- c("tas", "tasmin", "tasmax", "rsds", "clt")
    weather__normalize_climate_units(
        data,
        name = name,
        expected_frequency = "mon",
        target_units = c(
            tas = "degC",
            tasmin = "degC",
            tasmax = "degC",
            rsds = "W/m^2",
            clt = "%"
        ),
        required_variables = variables,
        single_unit = TRUE
    )
}
# }}}

# Aggregate multi-year monthly model values into the five monthly climatologies
# used by the BWS/BTWS change factors. Native CF fields take precedence over
# surrogate timestamps when resolving the calendar month.
# bws_btws__monthly_climatology {{{
bws_btws__monthly_climatology <- function(data, name) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(name, min.chars = 1L)
    required <- c("variable_id", "value")
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg {name}} is missing monthly climate column{?s}: {.val {missing}}."
        )
    }

    source <- data.table::as.data.table(data.table::copy(data))
    source <- morpher__resolve_calendar_columns(
        source,
        month = TRUE
    )
    calendar_missing <- setdiff("month", names(source))
    if (length(calendar_missing)) {
        cli::cli_abort(
            paste0(
                "{.arg {name}} must provide canonical CF month/day fields ",
                "or timestamps that can resolve them."
            )
        )
    }
    month <- as.integer(source[["month"]])
    if (anyNA(month) || any(month < 1L | month > 12L)) {
        cli::cli_abort(
            "{.arg {name}} contains an invalid monthly calendar field."
        )
    }
    data.table::set(source, j = "month", value = month)
    source <- source[
        variable_id %in% c("tas", "tasmin", "tasmax", "rsds", "clt")
    ]

    variables <- c("tas", "tasmin", "tasmax", "rsds", "clt")
    missing_variables <- setdiff(
        variables,
        unique(as.character(source[["variable_id"]]))
    )
    if (length(missing_variables)) {
        cli::cli_abort(
            "{.arg {name}} is missing required variable{?s}: {.val {missing_variables}}."
        )
    }

    # Monthly tasmin and tasmax retain the CMIP definitions of monthly averages
    # of daily extrema; pooling years forms the selected-period climatology.
    monthly <- source[,
        {
            finite <- is.finite(.SD[["value"]])
            list(
                climatology = if (any(finite)) {
                    mean(as.numeric(.SD[["value"]][finite]))
                } else {
                    NA_real_
                },
                n = sum(finite)
            )
        },
        by = c("variable_id", "month"),
        .SDcols = "value"
    ]
    expected <- data.table::CJ(
        variable_id = variables,
        month = seq_len(12L),
        unique = TRUE
    )
    monthly <- merge(
        expected,
        monthly,
        by = c("variable_id", "month"),
        all.x = TRUE,
        sort = FALSE
    )
    incomplete <- !is.finite(monthly[["climatology"]]) |
        is.na(monthly[["n"]]) |
        monthly[["n"]] < 1L
    if (any(incomplete)) {
        labels <- sprintf(
            "%s month %d",
            monthly[["variable_id"]][incomplete],
            monthly[["month"]][incomplete]
        )
        cli::cli_abort(
            "{.arg {name}} lacks finite observations for: {.val {labels}}."
        )
    }
    data.table::setorderv(monthly, c("variable_id", "month"))
    monthly[]
}
# }}}

# Convert aligned climatologies into the temperature deltas, shortwave absolute
# delta, and cloud-cover fractional scale used by BWS for each calendar month.
# bws_btws__monthly_changes {{{
bws_btws__monthly_changes <- function(
    future_climatology,
    historical_climatology
) {
    checkmate::assert_data_frame(future_climatology)
    checkmate::assert_data_frame(historical_climatology)
    keys <- c("variable_id", "month")
    changes <- merge(
        data.table::as.data.table(data.table::copy(future_climatology)),
        data.table::as.data.table(data.table::copy(historical_climatology)),
        by = keys,
        all = TRUE,
        suffixes = c("_future", "_historical"),
        sort = FALSE
    )
    required <- c(
        "climatology_future",
        "climatology_historical",
        "n_future",
        "n_historical"
    )
    missing <- setdiff(required, names(changes))
    if (
        length(missing) ||
            nrow(changes) != 60L ||
            any(!is.finite(changes[["climatology_future"]])) ||
            any(!is.finite(changes[["climatology_historical"]])) ||
            any(changes[["n_future"]] < 1L) ||
            any(changes[["n_historical"]] < 1L)
    ) {
        cli::cli_abort(
            paste0(
                "Matching future and historical monthly tas, tasmin, tasmax, ",
                "rsds, and clt climatologies are required for all 12 months."
            )
        )
    }

    monthly <- data.table::data.table(month = seq_len(12L))
    metrics <- c(
        tas = "mean",
        tasmin = "minimum",
        tasmax = "maximum"
    )
    for (variable in names(metrics)) {
        metric <- unname(metrics[[variable]])
        rows <- changes[changes[["variable_id"]] == variable]
        rows <- rows[match(monthly[["month"]], rows[["month"]])]
        for (source in c("future", "historical")) {
            data.table::set(
                monthly,
                j = sprintf("%s_%s", source, metric),
                value = rows[[sprintf("climatology_%s", source)]]
            )
            data.table::set(
                monthly,
                j = sprintf("n_%s_%s", source, metric),
                value = as.integer(rows[[sprintf("n_%s", source)]])
            )
        }
    }

    invalid_extrema <- (monthly[["future_maximum"]] <
        monthly[["future_minimum"]] |
        monthly[["historical_maximum"]] < monthly[["historical_minimum"]])
    if (any(invalid_extrema)) {
        cli::cli_abort(
            paste0(
                "Monthly temperature extrema must satisfy ",
                "{.val tasmax >= tasmin} in both periods."
            )
        )
    }

    # BTWS uses three independently estimated additive changes. DTR change is
    # retained as their difference for diagnostics and the shared result schema.
    data.table::set(
        monthly,
        j = "mean_delta",
        value = monthly[["future_mean"]] - monthly[["historical_mean"]]
    )
    data.table::set(
        monthly,
        j = "minimum_delta",
        value = monthly[["future_minimum"]] -
            monthly[["historical_minimum"]]
    )
    data.table::set(
        monthly,
        j = "maximum_delta",
        value = monthly[["future_maximum"]] -
            monthly[["historical_maximum"]]
    )
    data.table::set(
        monthly,
        j = "dtr_delta",
        value = monthly[["maximum_delta"]] - monthly[["minimum_delta"]]
    )
    data.table::set(
        monthly,
        j = "dtr_status",
        value = rep.int("adjusted", nrow(monthly))
    )

    # Radiation receives the published absolute mean change. Cloud cover uses
    # the future/historical mean ratio because the paper's cloud anomaly is a
    # percentage change; a positive future value from a zero baseline is
    # undefined and therefore rejected instead of silently inventing a scale.
    for (variable in c("rsds", "clt")) {
        rows <- changes[changes[["variable_id"]] == variable]
        rows <- rows[match(monthly[["month"]], rows[["month"]])]
        data.table::set(
            monthly,
            j = sprintf("future_%s_mean", variable),
            value = rows[["climatology_future"]]
        )
        data.table::set(
            monthly,
            j = sprintf("historical_%s_mean", variable),
            value = rows[["climatology_historical"]]
        )
    }
    data.table::set(
        monthly,
        j = "rsds_delta",
        value = monthly[["future_rsds_mean"]] -
            monthly[["historical_rsds_mean"]]
    )
    zero_cloud <- abs(monthly[["historical_clt_mean"]]) <=
        sqrt(.Machine$double.eps)
    undefined_cloud <- zero_cloud &
        abs(monthly[["future_clt_mean"]]) > sqrt(.Machine$double.eps)
    if (any(undefined_cloud)) {
        cli::cli_abort(
            paste(
                "BWS cloud-cover percentage change is undefined where",
                "historical monthly clt is zero and future clt is non-zero."
            ),
            class = "epwshiftr_bws_infeasible_error"
        )
    }
    clt_scale <- numeric(nrow(monthly))
    clt_scale[!zero_cloud] <-
        monthly[["future_clt_mean"]][!zero_cloud] /
        monthly[["historical_clt_mean"]][!zero_cloud] -
        1
    data.table::set(monthly, j = "clt_scale", value = clt_scale)

    monthly[]
}
# }}}

# Expand the monthly BTWS temperature deltas to the 365 daily targets consumed
# by the shared BTWS reconstruction component.
# bws_btws__daily_temperature_targets {{{
bws_btws__daily_temperature_targets <- function(monthly) {
    checkmate::assert_data_frame(monthly)
    if (
        nrow(monthly) != 12L ||
            !identical(sort(as.integer(monthly[["month"]])), seq_len(12L))
    ) {
        cli::cli_abort("BWS/BTWS monthly changes must contain all 12 months.")
    }

    month_days <- c(
        31L,
        28L,
        31L,
        30L,
        31L,
        30L,
        31L,
        31L,
        30L,
        31L,
        30L,
        31L
    )
    target <- data.table::data.table(
        target_day = seq_len(365L),
        month = rep.int(seq_len(12L), month_days),
        annual_phase = daily__phase_grid(365L)
    )
    target <- merge(
        target,
        monthly[,
            c(
                "month",
                "mean_delta",
                "minimum_delta",
                "maximum_delta",
                "dtr_delta",
                "dtr_status"
            ),
            with = FALSE
        ],
        by = "month",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setorderv(target, "target_day")
    data.table::setcolorder(
        target,
        c(
            "target_day",
            "annual_phase",
            "month",
            "mean_delta",
            "minimum_delta",
            "maximum_delta",
            "dtr_delta",
            "dtr_status"
        )
    )
    target[]
}
# }}}

# Convert every adjusted BWS monthly target into a method-level diagnostic with
# the climate means, EPW baseline, requested target, applied target, and exact
# attainable interval needed to reproduce the decision.
# bws_btws_epw__target_diagnostics {{{
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
# }}}

# Apply BTWS to dry-bulb temperature and BWS to the two published bounded
# variables while retaining baseline hourly ordering for physical closure.
# bws_btws_epw__hourly_reconstruct {{{
bws_btws_epw__hourly_reconstruct <- function(data, options = NULL) {
    options <- bws_btws__options(options)
    temperature <- temperature__hourly_result(
        data,
        options,
        btws__project_temperature
    )
    baseline <- temperature$baseline
    weather <- baseline$weather
    required <- c(
        "year",
        "month",
        "day",
        "hour",
        "global_horizontal_radiation",
        "diffuse_horizontal_radiation",
        "total_sky_cover",
        "opaque_sky_cover"
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
    if (
        nrow(bounded) != 12L ||
            !identical(sort(as.integer(bounded[["month"]])), seq_len(12L))
    ) {
        cli::cli_abort("BWS targets must contain all 12 months.")
    }
    data.table::setorderv(bounded, "month")

    month <- as.integer(weather[["month"]])
    baseline_ghi <- as.numeric(weather[["global_horizontal_radiation"]])
    baseline_cover <- as.numeric(weather[["total_sky_cover"]])
    if (
        any(!is.finite(baseline_ghi) | baseline_ghi < 0) ||
            any(
                !is.finite(baseline_cover) |
                    baseline_cover < 0 |
                    baseline_cover > 10
            )
    ) {
        cli::cli_abort(
            "Baseline EPW radiation and total sky cover must contain finite values within the BWS bounds."
        )
    }
    ghi_mean <- vapply(
        seq_len(12L),
        function(calendar_month) {
            mean(baseline_ghi[month == calendar_month])
        },
        numeric(1L)
    )
    ghi_upper <- vapply(
        seq_len(12L),
        function(calendar_month) {
            max(baseline_ghi[month == calendar_month])
        },
        numeric(1L)
    )
    cover_mean <- vapply(
        seq_len(12L),
        function(calendar_month) {
            mean(baseline_cover[month == calendar_month])
        },
        numeric(1L)
    )

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
    loc <- morpher__epw_location(baseline$epw)
    geometry <- solar__epw_interval_geometry(
        weather,
        latitude = loc$latitude,
        longitude = loc$longitude,
        timezone = loc$timezone
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
# }}}

# Apply all BWS/BTWS candidates in one call to the common physical layer so
# humidity and shortwave closure see the same final temperature and radiation.
# bws_btws_epw__physics_apply {{{
bws_btws_epw__physics_apply <- function(data, context) {
    bounded <- data$bws_projection
    physical <- epwphys__apply(
        EpwPhysicalRequest(
            template = data$baseline$weather,
            fields = list(
                dry_bulb_temperature = data$hourly[["temperature_projected"]],
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
# }}}

# Execute the complete calendar-month BWS/BTWS workflow directly on a canonical
# morphing context and return a standard epw_morph_result.
# bws_btws__run {{{
bws_btws__run <- function(context, backend = NULL) {
    morpher__validate_context(context)
    options <- bws_btws__options(context$recipe$options)
    inputs <- context$inputs
    baseline <- temperature__epw_template(
        weather__get_input(inputs, "weather_template")@source
    )
    future <- bws_btws__monthly_climatology(
        bws_btws__monthly_climate(
            weather__get_input(inputs, "model_future")@source,
            "future climate"
        ),
        "future climate"
    )
    historical <- bws_btws__monthly_climatology(
        bws_btws__monthly_climate(
            weather__get_input(inputs, "model_historical")@source,
            "historical climate"
        ),
        "historical climate"
    )
    monthly <- bws_btws__monthly_changes(future, historical)
    signal_data <- list(
        baseline = baseline,
        targets = bws_btws__daily_temperature_targets(monthly),
        monthly_bws_targets = monthly[,
            c(
                "month",
                "rsds_delta",
                "clt_scale",
                "future_rsds_mean",
                "historical_rsds_mean",
                "future_clt_mean",
                "historical_clt_mean"
            ),
            with = FALSE
        ]
    )
    hourly <- bws_btws_epw__hourly_reconstruct(signal_data, options = options)
    closed <- bws_btws_epw__physics_apply(hourly, context = context)
    temperature__output_write(
        closed,
        inputs = inputs,
        context = context,
        options = options,
        stages = list()
    )
}
# }}}

# vim: fdm=marker :
