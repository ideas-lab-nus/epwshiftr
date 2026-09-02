#' @include weather-temperature.R component-temperature-epw.R
NULL

# Eames monthly temperature workflow {{{

# The temperature-only Eames workflow shares the daily backend's deterministic
# hourly and EPW-header controls but does not estimate a circular daily signal.
EPW_MORPH_EAMES_MONTHLY_TEMPERATURE_OPTIONS <-
    EPW_MORPH_TEMPERATURE_OPTIONS

# Validate the options that remain meaningful after replacing the daily
# climatology with the published calendar-month signal.
eames__monthly_temperature_options <- function(options = NULL) {
    temperature__backend_options(
        options,
        defaults = EPW_MORPH_EAMES_MONTHLY_TEMPERATURE_OPTIONS,
        label = "Eames monthly temperature"
    )
}

# Declare the three role-addressable inputs shared by the Eames components and
# complete recipe. Daily frequency describes the CMIP6 source, not the temporal
# resolution of the resulting change factors.
eames__monthly_temperature_inputs <- function() {
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
            variable_sets = c("tas", "tasmin", "tasmax")
        ),
        model_future = component__input_requirement(
            "model_future",
            representations = "series",
            frequencies = "day",
            variable_sets = c("tas", "tasmin", "tasmax")
        )
    )
}

# Aggregate daily model values into the three monthly statistics used by Eames:
# mean tas, average daily tasmin, and average daily tasmax. Native CF month/day
# fields take precedence over surrogate timestamps, and February 29 is removed
# before mapping the source calendar to a non-leap EPW year.
eames__monthly_temperature_climatology <- function(data, name) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(name, min.chars = 1L)
    required <- c("variable_id", "value")
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg {name}} is missing monthly temperature column{?s}: {.val {missing}}."
        )
    }

    source <- data.table::as.data.table(data.table::copy(data))
    source <- morpher__resolve_calendar_columns(
        source,
        month = TRUE,
        day = TRUE
    )
    calendar_missing <- setdiff(c("month", "day"), names(source))
    if (length(calendar_missing)) {
        cli::cli_abort(
            paste0(
                "{.arg {name}} must provide canonical CF month/day fields ",
                "or timestamps that can resolve them."
            )
        )
    }
    month <- as.integer(source[["month"]])
    day <- as.integer(source[["day"]])
    if (anyNA(month) || any(month < 1L | month > 12L) ||
        anyNA(day) || any(day < 1L)) {
        cli::cli_abort(
            "{.arg {name}} contains invalid daily calendar fields."
        )
    }
    data.table::set(source, j = "month", value = month)
    data.table::set(source, j = "day", value = day)
    source <- source[
        variable_id %in% c("tas", "tasmin", "tasmax") &
            !(month == 2L & day == 29L)
    ]

    variables <- c("tas", "tasmin", "tasmax")
    missing_variables <- setdiff(
        variables,
        unique(as.character(source[["variable_id"]]))
    )
    if (length(missing_variables)) {
        cli::cli_abort(
            "{.arg {name}} is missing required variable{?s}: {.val {missing_variables}}."
        )
    }

    # Pool the daily values within each native calendar month. For tasmin and
    # tasmax this is the paper's average of daily extrema, not a monthly extreme.
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

# Convert aligned monthly climatologies into one constant set of Eames changes
# per EPW calendar month, then expand those 12 rows to the 365 target days
# required by the shared BTWS hourly component.
eames__monthly_temperature_changes <- function(
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
    if (length(missing) || nrow(changes) != 36L ||
        any(!is.finite(changes[["climatology_future"]])) ||
        any(!is.finite(changes[["climatology_historical"]])) ||
        any(changes[["n_future"]] < 1L) ||
        any(changes[["n_historical"]] < 1L)) {
        cli::cli_abort(
            paste0(
                "Matching future and historical monthly tas, tasmin, and ",
                "tasmax climatologies are required for all 12 months."
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

    # Eames uses three independently estimated additive changes. DTR change is
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
        monthly,
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
            "dtr_status",
            setdiff(
                names(target),
                c(
                    "target_day", "annual_phase", "month",
                    "mean_delta", "minimum_delta", "maximum_delta",
                    "dtr_delta", "dtr_status"
                )
            )
        )
    )
    target[]
}

# Build expanded Eames target rows directly from two normalized daily sources.
# This pure entry point keeps aggregation tests independent of pipeline classes.
eames__monthly_temperature_targets <- function(future, historical) {
    eames__monthly_temperature_changes(
        eames__monthly_temperature_climatology(future, "future climate"),
        eames__monthly_temperature_climatology(
            historical,
            "historical climate"
        )
    )
}

# Normalize the role-addressable EPW and daily CMIP6 inputs without introducing
# a daily smoothing window that does not belong to the Eames monthly method.
eames__monthly_temperature_preprocess_apply <- function(
    inputs,
    context,
    options
) {
    morpher__validate_context(context)
    options <- eames__monthly_temperature_options(options)
    future <- weather__get_input(inputs, "model_future")
    historical <- weather__get_input(inputs, "model_historical")
    template <- weather__get_input(inputs, "weather_template")
    list(
        baseline = temperature__epw_template(template@source),
        future = temperature__daily_climate(
            future@source,
            "future climate"
        ),
        historical = temperature__daily_climate(
            historical@source,
            "historical climate"
        ),
        options = options
    )
}

# Interpret each model's native calendar before the signal kernel so the kernel
# receives only aligned monthly statistics and cannot infer Gregorian dates.
eames__monthly_temperature_calendar_apply <- function(
    data,
    inputs,
    context,
    options
) {
    future <- eames__monthly_temperature_climatology(
        data$future,
        "future climate"
    )
    historical <- eames__monthly_temperature_climatology(
        data$historical,
        "historical climate"
    )
    list(signal__group(
        inputs = list(
            weather_template = data$baseline,
            model_historical = historical,
            model_future = future
        ),
        variables = c("tas", "tasmin", "tasmax")
    ))
}

# Calculate the three published future-minus-historical monthly changes after
# calendar interpretation, and emit the shared daily-target payload for BTWS.
eames__monthly_temperature_signal_apply_group <- function(
    inputs,
    settings,
    key
) {
    list(
        baseline = inputs$weather_template,
        targets = eames__monthly_temperature_changes(
            inputs$model_future,
            inputs$model_historical
        )
    )
}

# Define the three monthly mean/extrema stages. Sequence, hourly
# reconstruction, physical closure, and output remain shared components.
eames__monthly_temperature_component_specs <- function() {
    complete_inputs <- eames__monthly_temperature_inputs()
    reference <- "https://doi.org/10.1177/01436244231218861"
    profiles <- lapply(
        c("tas", "tasmin", "tasmax"),
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
                        tasmax = "monthly_average_daily_maximum"
                    )
                )
            )
        }
    )

    list(
        preprocess = component__spec(
            name = "monthly_mean_extrema_inputs",
            stage = "preprocess",
            label = "Monthly mean/extrema input normalization",
            required_inputs = complete_inputs,
            input_kinds = "role_inputs",
            output_kinds = "monthly_mean_extrema_preprocessed",
            scopes = "multivariate",
            operations = list(
                apply = eames__monthly_temperature_preprocess_apply
            )
        ),
        calendar = component__spec(
            name = "monthly_mean_extrema_climatology",
            stage = "calendar",
            label = "Native-calendar monthly mean/extrema climatology",
            required_inputs = complete_inputs,
            input_kinds = "monthly_mean_extrema_preprocessed",
            output_kinds = "calendar_indexed_monthly_temperature",
            scopes = "multivariate",
            operations = list(
                apply = eames__monthly_temperature_calendar_apply
            )
        ),
        signal = signal__component(
            name = "monthly_mean_extrema_changes",
            label = "Monthly mean/extrema temperature changes",
            required_inputs = complete_inputs,
            input_kinds = "calendar_indexed_monthly_temperature",
            output_kinds = "daily_temperature_targets",
            scopes = "multivariate",
            profiles = profiles,
            apply_group = eames__monthly_temperature_signal_apply_group
        )
    )
}

# Register the monthly mean/extrema stages once while preserving any explicit
# process-local implementation already stored under their stable keys.
eames__register_monthly_temperature_components <- function() {
    component__register_builtins(eames__monthly_temperature_component_specs())
}

# Compose the Eames monthly signal with the already registered EPW sequence,
# BTWS hourly reconstruction, humidity closure, and result writer.
eames__monthly_temperature_pipeline <- function() {
    temperature__register_components()
    btws__register_hourly_component()
    eames__register_monthly_temperature_components()
    pipeline__spec(list(
        preprocess = "monthly_mean_extrema_inputs",
        calendar = "monthly_mean_extrema_climatology",
        signal = "monthly_mean_extrema_changes",
        sequence = "preserve_epw_sequence",
        hourly = "btws_temperature_projection",
        physics = "specific_humidity_closure",
        output = "daily_temperature_epw_result"
    ))
}

# }}}
