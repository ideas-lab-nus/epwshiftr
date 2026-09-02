#' @include weather-signal.R
NULL

# Adjusted weather series share calendar-native coordinates independently of
# the signal method and retain their native temporal frequency explicitly.
BIAS_ADJUSTED_SERIES_COLUMNS <- c(
    "variable_id",
    "value",
    "units",
    "frequency",
    "cf_calendar",
    "cf_year",
    "cf_month",
    "cf_day",
    "cf_day_of_year",
    "cf_year_days",
    "annual_phase"
)

# Daily methods retain their original canonical table without inventing a
# time-of-day coordinate that is irrelevant to daily statistics.
BIAS_DAILY_SERIES_COLUMNS <- BIAS_ADJUSTED_SERIES_COLUMNS

# Sub-daily values add their exact position within the native-calendar day so
# multiple samples never collapse onto the same date key.
BIAS_SUBDAILY_SERIES_COLUMNS <- c(
    BIAS_ADJUSTED_SERIES_COLUMNS,
    "cf_second_of_day"
)

# Three-role bias-adjustment methods share one explicit source contract. The
# constant keeps role order stable in component specifications and diagnostics.
SIGNAL_THREE_INPUT_ROLES <- c(
    "observed_reference",
    "model_historical",
    "model_future"
)

# Construct the common observed/historical/future input requirements while
# leaving each method responsible for its supported variables and frequency.
signal__three_role_requirements <- function(
    variable_sets,
    representations = "series",
    frequencies = character(),
    calendars = character()
) {
    requirements <- lapply(SIGNAL_THREE_INPUT_ROLES, function(role) {
        component__input_requirement(
            role,
            representations = representations,
            frequencies = frequencies,
            calendars = calendars,
            variable_sets = variable_sets
        )
    })
    names(requirements) <- SIGNAL_THREE_INPUT_ROLES
    requirements
}

# Validate the adjusted-series class, optional temporal lattice, and output
# role through one contract while preserving method-specific diagnostic text.
signal__validate_adjusted_result <- function(
    value,
    result_class,
    result_class_label,
    output_role,
    method_label,
    frequency = NULL,
    time_step_seconds = NULL,
    temporal_message = NULL
) {
    checkmate::assert_string(result_class_label, min.chars = 1L)
    checkmate::assert_choice(output_role, WEATHER_INPUT_ROLES)
    checkmate::assert_string(method_label, min.chars = 1L)
    checkmate::assert_string(frequency, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_number(
        time_step_seconds,
        lower = 0,
        finite = TRUE,
        null.ok = TRUE
    )
    checkmate::assert_string(temporal_message, min.chars = 1L, null.ok = TRUE)

    if (!S7::S7_inherits(value, result_class)) {
        return(sprintf(
            "%s must return a %s object.",
            method_label,
            result_class_label
        ))
    }
    frequency_invalid <- !is.null(frequency) &&
        !identical(value@frequency, frequency)
    timestep_invalid <- !is.null(time_step_seconds) &&
        !identical(
            as.numeric(value@time_step_seconds),
            as.numeric(time_step_seconds)
        )
    if (frequency_invalid || timestep_invalid) {
        if (!is.null(temporal_message)) {
            return(temporal_message)
        }
        return(sprintf(
            "%s output does not retain its declared temporal lattice.",
            method_label
        ))
    }
    if (!identical(value@output_role, output_role)) {
        return(sprintf(
            "%s output must retain the `%s` role.",
            method_label,
            output_role
        ))
    }
    TRUE
}

# Partition contiguous projected years into retained output blocks and
# symmetric fitting windows while reporting truncation at available-data edges.
signal__future_blocks <- function(
    year,
    future_window_years,
    output_block_years,
    method_label
) {
    checkmate::assert_integerish(
        year,
        min.len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_string(method_label, min.chars = 1L)
    years <- sort(unique(as.integer(year)))
    if (length(years) > 1L && any(diff(years) != 1L)) {
        cli::cli_abort(
            "{method_label} requires contiguous future model years."
        )
    }
    flank <- (future_window_years - output_block_years) %/% 2L
    starts <- seq.int(1L, length(years), by = output_block_years)
    lapply(starts, function(start) {
        stop <- min(start + output_block_years - 1L, length(years))
        output_years <- years[start:stop]
        requested_start <- min(output_years) - flank
        requested_end <- max(output_years) + flank
        window_years <- years[
            years >= requested_start & years <= requested_end
        ]
        list(
            output_years = output_years,
            window_years = window_years,
            requested_start = requested_start,
            requested_end = requested_end,
            truncated_left = min(window_years) > requested_start,
            truncated_right = max(window_years) < requested_end
        )
    })
}

# Apply method-declared signal bounds and return the exact number of changed
# values without coupling statistical methods to EPW field specifications.
signal__bound_values <- function(value, bounds) {
    # Infinite method outputs remain valid inputs here because finite bounds
    # intentionally convert them to the declared endpoint.
    checkmate::assert_numeric(value, any.missing = FALSE)
    checkmate::assert_numeric(
        bounds,
        len = 2L,
        any.missing = FALSE,
        sorted = TRUE
    )
    bounded <- pmin(pmax(value, bounds[[1L]]), bounds[[2L]])
    list(
        value = bounded,
        clipped = as.integer(sum(bounded != value))
    )
}

bias__named_list_error <- function(value, name) {
    if (!is.list(value)) {
        return(sprintf("`%s` must be a list.", name))
    }
    if (length(value) &&
        (is.null(names(value)) ||
            anyNA(names(value)) ||
            any(!nzchar(names(value))) ||
            anyDuplicated(names(value)))) {
        return(sprintf("`%s` must be a uniquely named list.", name))
    }
    NULL
}

# Validate the calendar-native fields shared by daily and sub-daily signal
# results before applying frequency-specific sampling rules.
bias__calendar_data_error <- function(
    data,
    required_columns,
    label,
    key_columns,
    key_message
) {
    if (!is.data.frame(data)) {
        return("`data` must be a data frame.")
    }
    missing <- setdiff(required_columns, names(data))
    if (length(missing)) {
        return(sprintf(
            "`data` is missing canonical %s column(s): %s.",
            label,
            paste(sprintf("`%s`", missing), collapse = ", ")
        ))
    }
    if (!nrow(data)) {
        return(sprintf("`data` must contain at least one %s value.", label))
    }
    if (!is.character(data[["variable_id"]]) ||
        anyNA(data[["variable_id"]]) ||
        any(!grepl(
            "^[A-Za-z][A-Za-z0-9_]*$",
            data[["variable_id"]]
        ))) {
        return("`variable_id` must contain CMIP-style identifiers.")
    }
    if (!is.numeric(data[["value"]]) ||
        any(!is.finite(data[["value"]]))) {
        return("`value` must contain only finite numeric values.")
    }
    if (!is.character(data[["units"]]) ||
        anyNA(data[["units"]]) ||
        any(!nzchar(data[["units"]]))) {
        return("`units` must contain non-missing, non-empty strings.")
    }
    if (!is.character(data[["frequency"]]) ||
        anyNA(data[["frequency"]]) ||
        any(!nzchar(data[["frequency"]]))) {
        return("`frequency` must contain non-missing, non-empty strings.")
    }
    if (!is.character(data[["cf_calendar"]]) ||
        anyNA(data[["cf_calendar"]]) ||
        any(!data[["cf_calendar"]] %in% CF_TIME_CALENDARS)) {
        return("`cf_calendar` contains an unsupported CF calendar.")
    }

    integer_columns <- c(
        "cf_year",
        "cf_month",
        "cf_day",
        "cf_day_of_year",
        "cf_year_days"
    )
    for (column in integer_columns) {
        value <- data[[column]]
        if (!is.numeric(value) ||
            any(!is.finite(value)) ||
            any(value != as.integer(value))) {
            return(sprintf(
                "`%s` must contain finite integer values.",
                column
            ))
        }
    }
    phase <- data[["annual_phase"]]
    if (!is.numeric(phase) ||
        any(!is.finite(phase)) ||
        any(phase < 0 | phase >= 1)) {
        return("`annual_phase` must contain finite values in [0, 1).")
    }

    # Validate dates and derived coordinates separately for each native
    # calendar so no Gregorian interpretation is imposed on 360/365/366-day
    # data.
    for (calendar in unique(data[["cf_calendar"]])) {
        index <- which(data[["cf_calendar"]] == calendar)
        parts <- data.frame(
            year = as.integer(data[["cf_year"]][index]),
            month = as.integer(data[["cf_month"]][index]),
            day = as.integer(data[["cf_day"]][index])
        )
        if (!all(cf_time_valid_days(parts, calendar))) {
            return(sprintf(
                "`data` contains an invalid date for calendar `%s`.",
                calendar
            ))
        }
        origin <- data.frame(
            year = parts$year,
            month = 1L,
            day = 1L
        )
        expected_day <- as.integer(
            cf_time_date2offset(parts, origin, calendar) + 1L
        )
        expected_days <- as.integer(
            cf_time__year_days(parts$year, calendar)
        )
        if (any(data[["cf_day_of_year"]][index] != expected_day)) {
            return("`cf_day_of_year` is inconsistent with the CF date.")
        }
        if (any(data[["cf_year_days"]][index] != expected_days)) {
            return("`cf_year_days` is inconsistent with the CF calendar.")
        }
        lower <- (expected_day - 1) / expected_days
        upper <- expected_day / expected_days
        tolerance <- sqrt(.Machine$double.eps)
        if (any(phase[index] < lower - tolerance |
            phase[index] >= upper + tolerance)) {
            return(
                "`annual_phase` is inconsistent with the calendar-native day."
            )
        }
    }

    for (variable in unique(data[["variable_id"]])) {
        index <- data[["variable_id"]] == variable
        if (length(unique(data[["units"]][index])) != 1L) {
            return(sprintf(
                "Variable `%s` must use one unit within an adjusted series.",
                variable
            ))
        }
    }
    key <- data[key_columns]
    if (anyDuplicated(key)) {
        return(key_message)
    }
    NULL
}

# Validate the calendar-native daily table at the boundary shared by all
# existing daily bias-adjustment methods.
bias__daily_data_error <- function(data) {
    error <- bias__calendar_data_error(
        data,
        BIAS_DAILY_SERIES_COLUMNS,
        "daily",
        c(
            "variable_id",
            "cf_calendar",
            "cf_year",
            "cf_month",
            "cf_day"
        ),
        "`data` must have unique variable-calendar-year-month-day keys."
    )
    if (!is.null(error)) {
        return(error)
    }
    if (any(data[["frequency"]] != "day")) {
        return("`frequency` must be `day` for every row.")
    }
    NULL
}

# Validate exact sub-day positions and their regular timestep without reducing
# native CF dates to Gregorian timestamps.
bias__subdaily_data_error <- function(data, frequency, time_step_seconds) {
    error <- bias__calendar_data_error(
        data,
        BIAS_SUBDAILY_SERIES_COLUMNS,
        "sub-daily",
        c(
            "variable_id",
            "cf_calendar",
            "cf_year",
            "cf_month",
            "cf_day",
            "cf_second_of_day"
        ),
        paste(
            "`data` must have unique",
            "variable-calendar-year-month-day-second keys."
        )
    )
    if (!is.null(error)) {
        return(error)
    }
    if (any(data[["frequency"]] != frequency)) {
        return("`data` frequency must match the declared `frequency`.")
    }
    seconds <- data[["cf_second_of_day"]]
    if (!is.numeric(seconds) ||
        any(!is.finite(seconds)) ||
        any(seconds < 0 | seconds >= 86400)) {
        return("`cf_second_of_day` must contain finite values in [0, 86400).")
    }

    # The annual phase and explicit time-of-day must describe the same native
    # CF instant; this prevents ambiguous ordering around day boundaries.
    expected_phase <- (
        data[["cf_day_of_year"]] - 1 + seconds / 86400
    ) / data[["cf_year_days"]]
    tolerance <- sqrt(.Machine$double.eps)
    if (any(abs(data[["annual_phase"]] - expected_phase) > tolerance)) {
        return(
            "`annual_phase` is inconsistent with `cf_second_of_day`."
        )
    }

    # All samples must lie on one regular lattice even when incomplete periods
    # are retained for a later completeness diagnostic.
    remainders <- seconds %% time_step_seconds
    distance <- abs(remainders - remainders[[1L]])
    circular_distance <- pmin(
        distance,
        time_step_seconds - distance
    )
    if (any(circular_distance > 1e-6)) {
        return("Sub-daily samples must share one regular timestep lattice.")
    }
    NULL
}

# Validate semantic metadata once for every adjusted-series specialization.
bias__adjusted_series_error <- function(self) {
    if (length(self@frequency) != 1L ||
        is.na(self@frequency) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", self@frequency)) {
        return("`frequency` must be one non-empty frequency identifier.")
    }
    if (length(self@time_step_seconds) != 1L ||
        is.na(self@time_step_seconds) ||
        !is.finite(self@time_step_seconds) ||
        self@time_step_seconds <= 0) {
        return("`time_step_seconds` must be one positive finite number.")
    }
    if (!is.data.frame(self@data) ||
        !"frequency" %in% names(self@data) ||
        !identical(unique(self@data[["frequency"]]), self@frequency)) {
        return("`data` must contain exactly the declared `frequency`.")
    }
    if (length(self@output_role) != 1L ||
        is.na(self@output_role) ||
        !self@output_role %in% WEATHER_INPUT_ROLES) {
        return("`output_role` must identify one future-weather input role.")
    }
    if (length(self@transformation) != 1L ||
        is.na(self@transformation) ||
        !grepl("^[a-z][a-z0-9_]*$", self@transformation)) {
        return("`transformation` must use lower snake_case.")
    }
    variables <- unique(self@data[["variable_id"]])
    metadata_error <- bias__named_list_error(
        self@variable_metadata,
        "variable_metadata"
    )
    if (!is.null(metadata_error) ||
        !setequal(names(self@variable_metadata), variables) ||
        length(self@variable_metadata) != length(variables) ||
        !all(vapply(
            self@variable_metadata,
            is.list,
            logical(1L)
        ))) {
        return(
            "`variable_metadata` must contain one named list per variable."
        )
    }
    for (name in c("settings", "provenance")) {
        error <- bias__named_list_error(S7::prop(self, name), name)
        if (!is.null(error)) {
            return(error)
        }
    }
    NULL
}

# AdjustedWeatherSeries is the package-native, frequency-aware signal result
# shared by daily and sub-daily methods.
AdjustedWeatherSeries <- S7::new_class(
    "AdjustedWeatherSeries",
    abstract = TRUE,
    properties = list(
        data = S7::new_property(S7::class_any),
        frequency = S7::new_property(S7::class_character),
        time_step_seconds = S7::new_property(S7::class_numeric),
        output_role = S7::new_property(S7::class_character),
        transformation = S7::new_property(S7::class_character),
        variable_metadata = S7::new_property(S7::class_list),
        settings = S7::new_property(S7::class_list, default = list()),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = bias__adjusted_series_error
)

# DailyAdjustedSeries preserves the original strict daily contract while also
# satisfying the common frequency-aware adjusted-series boundary.
DailyAdjustedSeries <- S7::new_class(
    "DailyAdjustedSeries",
    parent = AdjustedWeatherSeries,
    validator = function(self) {
        if (!identical(self@frequency, "day") ||
            !identical(as.numeric(self@time_step_seconds), 86400)) {
            return(
                "DailyAdjustedSeries requires `day` frequency and an 86400-second timestep."
            )
        }
        error <- bias__daily_data_error(self@data)
        if (!is.null(error)) {
            return(error)
        }
        NULL
    }
)

# SubdailyAdjustedSeries retains a regular native-calendar time lattice for
# hourly and multi-hourly signal outputs.
SubdailyAdjustedSeries <- S7::new_class(
    "SubdailyAdjustedSeries",
    parent = AdjustedWeatherSeries,
    validator = function(self) {
        if (identical(self@frequency, "day") ||
            self@time_step_seconds >= 86400) {
            return(
                "SubdailyAdjustedSeries requires a sub-daily frequency and timestep."
            )
        }
        samples_per_day <- 86400 / self@time_step_seconds
        if (abs(samples_per_day - round(samples_per_day)) >
            sqrt(.Machine$double.eps)) {
            return(
                "`time_step_seconds` must divide one 86400-second day exactly."
            )
        }
        error <- bias__subdaily_data_error(
            self@data,
            self@frequency,
            self@time_step_seconds
        )
        if (!is.null(error)) {
            return(error)
        }
        NULL
    }
)

# Copy and normalize a canonical daily table without inferring missing dates or
# calendars inside a signal method.
bias__daily_table <- function(data, name = "data") {
    if (!is.data.frame(data)) {
        cli::cli_abort("{.arg {name}} must be a canonical daily data frame.")
    }
    out <- as.data.frame(data, stringsAsFactors = FALSE)
    error <- bias__daily_data_error(out)
    if (!is.null(error)) {
        cli::cli_abort("{.arg {name}} is invalid: {error}")
    }
    out
}

# Derive stable per-variable descriptors directly from the validated output
# table unless a method supplies richer metadata explicitly.
bias__variable_metadata <- function(data, frequency) {
    variables <- unique(data[["variable_id"]])
    metadata <- lapply(variables, function(variable) {
        index <- data[["variable_id"]] == variable
        list(
            units = unique(data[["units"]][index]),
            frequency = frequency,
            calendars = sort(unique(data[["cf_calendar"]][index]))
        )
    })
    stats::setNames(metadata, variables)
}

# Construct the frequency-aware result type so signal kernels cannot omit its
# temporal semantics, role, settings, or provenance.
bias__adjusted_series <- function(
    data,
    frequency,
    time_step_seconds,
    output_role,
    transformation,
    variable_metadata = NULL,
    settings = list(),
    provenance = list()
) {
    checkmate::assert_string(
        frequency,
        pattern = "^[A-Za-z0-9][A-Za-z0-9._-]*$"
    )
    checkmate::assert_number(time_step_seconds, lower = 0, finite = TRUE)
    if (time_step_seconds <= 0) {
        cli::cli_abort(
            "{.arg time_step_seconds} must be one positive number."
        )
    }
    if (identical(frequency, "day")) {
        data <- bias__daily_table(data)
    } else {
        samples_per_day <- 86400 / time_step_seconds
        if (time_step_seconds >= 86400 ||
            abs(samples_per_day - round(samples_per_day)) >
                sqrt(.Machine$double.eps)) {
            cli::cli_abort(
                "{.arg time_step_seconds} must divide one 86400-second day exactly."
            )
        }
        data <- bias__subdaily_table(
            data,
            frequency,
            time_step_seconds
        )
    }
    checkmate::assert_choice(output_role, WEATHER_INPUT_ROLES)
    checkmate::assert_string(
        transformation,
        pattern = "^[a-z][a-z0-9_]*$"
    )
    if (is.null(variable_metadata)) {
        variable_metadata <- bias__variable_metadata(data, frequency)
    }
    checkmate::assert_list(variable_metadata, names = "unique")
    checkmate::assert_list(settings, names = "unique")
    checkmate::assert_list(provenance, names = "unique")

    constructor <- if (identical(frequency, "day")) {
        DailyAdjustedSeries
    } else {
        SubdailyAdjustedSeries
    }
    constructor(
        data = data,
        frequency = frequency,
        time_step_seconds = time_step_seconds,
        output_role = output_role,
        transformation = transformation,
        variable_metadata = variable_metadata,
        settings = settings,
        provenance = provenance
    )
}

# Copy and validate a canonical sub-daily table without inferring its timestep
# from incomplete or gapped observations.
bias__subdaily_table <- function(
    data,
    frequency,
    time_step_seconds,
    name = "data"
) {
    if (!is.data.frame(data)) {
        cli::cli_abort(
            "{.arg {name}} must be a canonical sub-daily data frame."
        )
    }
    out <- as.data.frame(data, stringsAsFactors = FALSE)
    error <- bias__subdaily_data_error(
        out,
        frequency,
        time_step_seconds
    )
    if (!is.null(error)) {
        cli::cli_abort("{.arg {name}} is invalid: {error}")
    }
    out
}

# Preserve the daily constructor used by all existing signal kernels while
# routing its metadata through the common adjusted-series class hierarchy.
bias__daily_adjusted_series <- function(
    data,
    output_role,
    transformation,
    variable_metadata = NULL,
    settings = list(),
    provenance = list()
) {
    bias__adjusted_series(
        data = data,
        frequency = "day",
        time_step_seconds = 86400,
        output_role = output_role,
        transformation = transformation,
        variable_metadata = variable_metadata,
        settings = settings,
        provenance = provenance
    )
}

# Construct a sub-daily adjusted series only when the caller declares its
# exact frequency and regular timestep explicitly.
bias__subdaily_adjusted_series <- function(
    data,
    frequency,
    time_step_seconds,
    output_role,
    transformation,
    variable_metadata = NULL,
    settings = list(),
    provenance = list()
) {
    bias__adjusted_series(
        data = data,
        frequency = frequency,
        time_step_seconds = time_step_seconds,
        output_role = output_role,
        transformation = transformation,
        variable_metadata = variable_metadata,
        settings = settings,
        provenance = provenance
    )
}

# Resolve the monthly mean-change settings shared by Linear Scaling and Delta
# Change while keeping method names in user-facing diagnostics.
bias__mean_change_settings <- function(settings, method) {
    if (length(settings) != 1L ||
        is.null(names(settings)) ||
        !nzchar(names(settings)[[1L]])) {
        cli::cli_abort(
            "{method} requires settings for exactly one variable."
        )
    }
    resolved <- settings[[1L]]
    if (!is.list(resolved)) {
        cli::cli_abort("{method} settings must be a named list.")
    }
    if (!identical(resolved$grouping, "calendar_month")) {
        cli::cli_abort(
            "{method} currently supports only `calendar_month` grouping."
        )
    }
    if (!identical(resolved$statistic, "mean")) {
        cli::cli_abort(
            "{method} currently supports only the monthly mean statistic."
        )
    }
    checkmate::assert_choice(
        resolved$transformation,
        c("additive", "multiplicative")
    )
    checkmate::assert_numeric(
        resolved$bounds,
        len = 2L,
        any.missing = FALSE
    )
    if (resolved$bounds[[1L]] > resolved$bounds[[2L]]) {
        cli::cli_abort(
            "{method} bounds must be ordered from lower to upper."
        )
    }
    checkmate::assert_number(
        resolved$zero_tolerance,
        lower = 0,
        finite = TRUE
    )
    resolved
}

# Validate role payloads as one calendar-native, univariate unit of work and
# reject unit changes that would make monthly corrections ambiguous.
bias__mean_change_inputs <- function(
    inputs,
    variable,
    transformation,
    method
) {
    roles <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    if (!identical(sort(names(inputs)), sort(roles))) {
        cli::cli_abort(
            "{method} requires observed, historical-model, and future-model role payloads."
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
                "{method} role {.val {role}} must contain only variable {.val {variable}}."
            )
        }
        calendars <- unique(series[[role]][["cf_calendar"]])
        if (length(calendars) != 1L) {
            cli::cli_abort(
                "{method} role {.val {role}} must contain one native calendar per signal group."
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
            "{method} inputs for {.val {variable}} must use identical units."
        )
    }
    if (identical(transformation, "multiplicative") &&
        any(vapply(
            series,
            function(data) any(data[["value"]] < 0),
            logical(1L)
        ))) {
        cli::cli_abort(
            "Multiplicative {method} requires non-negative input values."
        )
    }
    series
}

# Calculate one native-calendar monthly mean per role for the months present
# in the method's declared output backbone.
bias__mean_change_monthly_means <- function(
    series,
    output_role,
    method
) {
    output_months <- sort(unique(series[[output_role]][["cf_month"]]))
    monthly <- lapply(series, function(data) {
        means <- tapply(
            data[["value"]],
            data[["cf_month"]],
            mean
        )
        values <- unname(means[as.character(output_months)])
        if (anyNA(values)) {
            cli::cli_abort(
                "{method} inputs do not cover every output calendar month."
            )
        }
        values
    })
    data.frame(
        cf_month = output_months,
        observed_mean = monthly$observed_reference,
        historical_mean = monthly$model_historical,
        future_mean = monthly$model_future
    )
}
