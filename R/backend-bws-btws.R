#' @include weather-temperature.R component-temperature-epw.R method-bws.R method-btws.R
NULL

# BWS and BTWS monthly weather workflow {{{

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
        "tdb", "glob_rad", "total_cover", "rh", "tdew", "diff_rad",
        "norm_rad", "opaque_cover"
    ),
    epw_field = c(
        "dry_bulb_temperature", "global_horizontal_radiation",
        "total_sky_cover", "relative_humidity", "dew_point_temperature",
        "diffuse_horizontal_radiation", "direct_normal_radiation",
        "opaque_sky_cover"
    ),
    variable_id = c(
        "tas,tasmin,tasmax", "rsds", "clt", rep(NA_character_, 5L)
    ),
    optional_variable_id = NA_character_,
    method = c("btws", "bws", "bws", rep("derived", 5L)),
    required = c(rep(TRUE, 3L), rep(FALSE, 5L)),
    derived = c(rep(FALSE, 3L), rep(TRUE, 5L)),
    method_choices = list(
        "btws", "bws", "bws", "derived", "derived", "derived",
        "derived", "derived"
    )
)

# BWS and BTWS expose only the shared deterministic tolerance and EPW-header
# controls; the published BWS and BTWS equations do not add tuning options.
EPW_MORPH_BWS_BTWS_OPTIONS <- EPW_MORPH_TEMPERATURE_OPTIONS

# Validate the options shared by the complete calendar-month BWS/BTWS method.
bws_btws__options <- function(options = NULL) {
    temperature__backend_options(
        options,
        defaults = EPW_MORPH_BWS_BTWS_OPTIONS,
        label = "BWS and BTWS monthly morphing"
    )
}

# Declare the role-addressable inputs shared by every BWS/BTWS component. Monthly
# CMIP6 values map directly to the monthly change factors defined by the paper.
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

# Normalize the five monthly CMIP6 variables to method-owned units before
# climatology aggregation. This keeps source adaptation separate from BWS/BTWS.
bws_btws__monthly_climate <- function(data, name) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(name, min.chars = 1L)
    required <- c(
        "variable_id", "value", "units", "frequency"
    )
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg {name}} is missing BWS/BTWS climate column{?s}: {.val {missing}}."
        )
    }
    frequency <- unique(tolower(as.character(data[["frequency"]])))
    frequency <- frequency[!is.na(frequency) & nzchar(frequency)]
    if (!identical(frequency, "mon")) {
        shown <- if (length(frequency)) frequency else "<missing>"
        cli::cli_abort(
            "{.arg {name}} must use CMIP frequency {.val mon}; found {.val {shown}}."
        )
    }

    variables <- c("tas", "tasmin", "tasmax", "rsds", "clt")
    out <- data.table::as.data.table(data.table::copy(data))[
        variable_id %in% variables
    ]
    present <- unique(as.character(out[["variable_id"]]))
    missing_variables <- setdiff(variables, present)
    if (length(missing_variables)) {
        cli::cli_abort(
            "{.arg {name}} is missing required variable{?s}: {.val {missing_variables}}."
        )
    }

    targets <- c(
        tas = "degC",
        tasmin = "degC",
        tasmax = "degC",
        rsds = "W/m^2",
        clt = "%"
    )
    for (variable in variables) {
        rows <- which(out[["variable_id"]] == variable)
        aliases <- unique(vapply(
            out[["units"]][rows],
            morpher__unit_alias,
            character(1L)
        ))
        if (length(aliases) != 1L || is.na(aliases)) {
            cli::cli_abort(
                "{.arg {name}} variable {.val {variable}} must use one supported unit."
            )
        }
        converted <- morpher__convert_value_checked(
            out[["value"]][rows],
            aliases,
            targets[[variable]]
        )
        if (!isTRUE(converted$ok)) {
            cli::cli_abort(converted$message)
        }
        data.table::set(
            out,
            i = rows,
            j = "value",
            value = converted$value
        )
        data.table::set(
            out,
            i = rows,
            j = "units",
            value = targets[[variable]]
        )
    }
    out[]
}

# Aggregate multi-year monthly model values into the five monthly climatologies
# used by the BWS/BTWS change factors. Native CF fields take precedence over
# surrogate timestamps when resolving the calendar month.
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
    monthly <- source[, {
        finite <- is.finite(.SD[["value"]])
        list(
            climatology = if (any(finite)) {
                mean(as.numeric(.SD[["value"]][finite]))
            } else {
                NA_real_
            },
            n = sum(finite)
        )
    }, by = c("variable_id", "month"), .SDcols = "value"]
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
        is.na(monthly[["n"]]) | monthly[["n"]] < 1L
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

# Convert aligned climatologies into the temperature deltas, shortwave absolute
# delta, and cloud-cover fractional scale used by BWS for each calendar month.
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
        "climatology_future", "climatology_historical",
        "n_future", "n_historical"
    )
    missing <- setdiff(required, names(changes))
    if (length(missing) || nrow(changes) != 60L ||
        any(!is.finite(changes[["climatology_future"]])) ||
        any(!is.finite(changes[["climatology_historical"]])) ||
        any(changes[["n_future"]] < 1L) ||
        any(changes[["n_historical"]] < 1L)) {
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

    invalid_extrema <- (
        monthly[["future_maximum"]] < monthly[["future_minimum"]] |
            monthly[["historical_maximum"]] <
                monthly[["historical_minimum"]]
    )
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
        monthly[["historical_clt_mean"]][!zero_cloud] - 1
    data.table::set(monthly, j = "clt_scale", value = clt_scale)

    monthly[]
}

# Expand the monthly BTWS temperature deltas to the 365 daily targets consumed
# by the shared BTWS reconstruction component.
bws_btws__daily_temperature_targets <- function(monthly) {
    checkmate::assert_data_frame(monthly)
    if (nrow(monthly) != 12L ||
        !identical(sort(as.integer(monthly[["month"]])), seq_len(12L))) {
        cli::cli_abort("BWS/BTWS monthly changes must contain all 12 months.")
    }

    month_days <- c(
        31L, 28L, 31L, 30L, 31L, 30L,
        31L, 31L, 30L, 31L, 30L, 31L
    )
    target <- data.table::data.table(
        target_day = seq_len(365L),
        month = rep.int(seq_len(12L), month_days),
        annual_phase = daily__phase_grid(365L)
    )
    target <- merge(
        target,
        monthly[, c(
            "month",
            "mean_delta",
            "minimum_delta",
            "maximum_delta",
            "dtr_delta",
            "dtr_status"
        ), with = FALSE],
        by = "month",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setorderv(target, "target_day")
    data.table::setcolorder(
        target,
        c(
            "target_day", "annual_phase", "month",
            "mean_delta", "minimum_delta", "maximum_delta", "dtr_delta",
            "dtr_status"
        )
    )
    target[]
}

# Build both BWS monthly signals and expanded BTWS targets from normalized
# monthly sources. This pure entry point keeps tests independent of pipelines.
bws_btws__monthly_targets <- function(future, historical) {
    monthly <- bws_btws__monthly_changes(
        bws_btws__monthly_climatology(future, "future climate"),
        bws_btws__monthly_climatology(
            historical,
            "historical climate"
        )
    )
    list(
        monthly = monthly,
        temperature = bws_btws__daily_temperature_targets(monthly)
    )
}

# Normalize the role-addressable EPW and monthly CMIP6 inputs without
# introducing temporal smoothing that does not belong to BWS or BTWS.
bws_btws__preprocess_apply <- function(
    inputs,
    context,
    options
) {
    morpher__validate_context(context)
    options <- bws_btws__options(options)
    future <- weather__get_input(inputs, "model_future")
    historical <- weather__get_input(inputs, "model_historical")
    template <- weather__get_input(inputs, "weather_template")
    list(
        baseline = temperature__epw_template(template@source),
        future = bws_btws__monthly_climate(future@source, "future climate"),
        historical = bws_btws__monthly_climate(
            historical@source,
            "historical climate"
        ),
        options = options
    )
}

# Interpret each model's native calendar before the signal kernel so the kernel
# receives only aligned monthly statistics and cannot infer Gregorian dates.
bws_btws__calendar_apply <- function(
    data,
    inputs,
    context,
    options
) {
    future <- bws_btws__monthly_climatology(
        data$future,
        "future climate"
    )
    historical <- bws_btws__monthly_climatology(
        data$historical,
        "historical climate"
    )
    list(signal__group(
        inputs = list(
            weather_template = data$baseline,
            model_historical = historical,
            model_future = future
        ),
        variables = c("tas", "tasmin", "tasmax", "rsds", "clt")
    ))
}

# Calculate the complete monthly signal and emit both the BTWS daily
# temperature targets and the monthly BWS radiation/cloud targets.
bws_btws__signal_apply_group <- function(
    inputs,
    settings,
    key
) {
    monthly <- bws_btws__monthly_changes(
        inputs$model_future,
        inputs$model_historical
    )
    list(
        baseline = inputs$weather_template,
        targets = bws_btws__daily_temperature_targets(monthly),
        monthly_bws_targets = monthly[, c(
            "month",
            "rsds_delta",
            "clt_scale",
            "future_rsds_mean",
            "historical_rsds_mean",
            "future_clt_mean",
            "historical_clt_mean"
        ), with = FALSE]
    )
}

# Define the BWS/BTWS monthly input, native-calendar, and joint signal stages.
bws_btws__component_specs <- function() {
    complete_inputs <- bws_btws__inputs()
    reference <- "https://doi.org/10.1177/01436244231218861"
    profiles <- lapply(
        c("tas", "tasmin", "tasmax", "rsds", "clt"),
        function(variable) {
            signal__variable_profile(
                variable,
                evidence = "published",
                references = reference,
                metadata = list(
                    statistic = switch(
                        variable,
                        tas = "monthly_mean_temperature",
                        tasmin = "monthly_average_daily_minimum",
                        tasmax = "monthly_average_daily_maximum",
                        rsds = "monthly_mean_shortwave_absolute_change",
                        clt = "monthly_mean_cloud_fractional_change"
                    )
                )
            )
        }
    )

    list(
        preprocess = component__spec(
            name = "bws_btws_monthly_inputs",
            stage = "preprocess",
            label = "BWS/BTWS monthly CMIP6 input normalization",
            required_inputs = complete_inputs,
            input_kinds = "role_inputs",
            output_kinds = "bws_btws_monthly_preprocessed",
            scopes = "multivariate",
            operations = list(
                apply = bws_btws__preprocess_apply
            )
        ),
        calendar = component__spec(
            name = "bws_btws_monthly_climatology",
            stage = "calendar",
            label = "Native-calendar BWS/BTWS monthly climatology",
            required_inputs = complete_inputs,
            input_kinds = "bws_btws_monthly_preprocessed",
            output_kinds = "calendar_indexed_bws_btws_climate",
            scopes = "multivariate",
            operations = list(
                apply = bws_btws__calendar_apply
            )
        ),
        signal = signal__component(
            name = "bws_btws_monthly_changes",
            label = "BWS/BTWS monthly temperature, radiation, and cloud changes",
            required_inputs = complete_inputs,
            input_kinds = "calendar_indexed_bws_btws_climate",
            output_kinds = "bws_btws_weather_targets",
            scopes = "multivariate",
            profiles = profiles,
            apply_group = bws_btws__signal_apply_group
        )
    )
}

# Register the BWS/BTWS stages once while preserving process-local extensions.
bws_btws__register_components <- function() {
    component__register_builtins(bws_btws__component_specs())
}

# Compose BWS/BTWS monthly signals with shared EPW sequencing, their combined
# BTWS/BWS hourly adapter, unified physical closure, and common result writer.
bws_btws__pipeline <- function() {
    temperature__register_components()
    bws_btws_epw__register_components()
    bws_btws__register_components()
    pipeline__spec(list(
        preprocess = "bws_btws_monthly_inputs",
        calendar = "bws_btws_monthly_climatology",
        signal = "bws_btws_monthly_changes",
        sequence = "bws_btws_preserve_epw_sequence",
        hourly = "bws_btws_hourly_projection",
        physics = "bws_btws_physical_closure",
        output = "bws_btws_epw_result"
    ))
}

# }}}
