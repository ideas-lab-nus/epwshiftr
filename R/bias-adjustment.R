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

# Linear Scaling defaults cite the review in which the monthly additive
# temperature and multiplicative precipitation transformations are defined.
BIAS_LINEAR_SCALING_REFERENCES <- c(
    "https://doi.org/10.1016/j.jhydrol.2012.05.052"
)

# Delta Change defaults follow the published monthly additive-temperature and
# multiplicative-precipitation change-factor formulation.
BIAS_DELTA_CHANGE_REFERENCES <- c(
    "https://doi.org/10.5194/hess-16-4343-2012"
)

# Check the named-list fields carried by the signal result without constraining
# the method-specific values stored inside them.
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

# Return one explicit diagnostic string when Linear Scaling fails to produce
# its package-native future-model result.
bias__validate_linear_scaling_result <- function(value, inputs, key) {
    if (!S7::S7_inherits(value, DailyAdjustedSeries)) {
        return(
            "Linear Scaling must return a DailyAdjustedSeries object."
        )
    }
    if (!identical(value@output_role, "model_future")) {
        return(
            "Linear Scaling output must retain the `model_future` role."
        )
    }
    TRUE
}

# Return one explicit diagnostic string when Delta Change fails to produce its
# package-native observed-reference result.
bias__validate_delta_change_result <- function(value, inputs, key) {
    if (!S7::S7_inherits(value, DailyAdjustedSeries)) {
        return(
            "Delta Change must return a DailyAdjustedSeries object."
        )
    }
    if (!identical(value@output_role, "observed_reference")) {
        return(
            "Delta Change output must retain the `observed_reference` role."
        )
    }
    TRUE
}

# Define the published monthly-mean defaults separately for temperature and
# precipitation while retaining their evidence and source in signal profiles.
bias__linear_scaling_profiles <- function() {
    temperature_settings <- list(
        grouping = "calendar_month",
        statistic = "mean",
        transformation = "additive",
        bounds = c(-Inf, Inf),
        zero_tolerance = 0
    )
    precipitation_settings <- list(
        grouping = "calendar_month",
        statistic = "mean",
        transformation = "multiplicative",
        bounds = c(0, Inf),
        zero_tolerance = sqrt(.Machine$double.eps)
    )
    temperature <- lapply(c("tas", "tasmin", "tasmax"), function(variable) {
        signal__variable_profile(
            variable,
            settings = temperature_settings,
            evidence = "published",
            references = BIAS_LINEAR_SCALING_REFERENCES,
            metadata = list(
                method = "linear_scaling",
                output_role = "model_future"
            )
        )
    })
    precipitation <- signal__variable_profile(
        "pr",
        settings = precipitation_settings,
        evidence = "published",
        references = BIAS_LINEAR_SCALING_REFERENCES,
        metadata = list(
            method = "linear_scaling",
            output_role = "model_future"
        )
    )
    c(temperature, list(precipitation))
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

# Apply the published monthly additive or multiplicative Linear Scaling
# equation and return a typed daily model-future series.
bias__linear_scaling_apply_group <- function(inputs, settings, key) {
    method <- "Linear Scaling"
    resolved <- bias__mean_change_settings(settings, method)
    variable <- names(settings)[[1L]]
    series <- bias__mean_change_inputs(
        inputs,
        variable,
        resolved$transformation,
        method
    )
    monthly <- bias__mean_change_monthly_means(
        series,
        "model_future",
        method
    )

    if (identical(resolved$transformation, "additive")) {
        # Temperature uses the monthly observed-minus-historical mean bias as
        # an additive correction on every future daily value in that month.
        monthly$correction <- (
            monthly$observed_mean - monthly$historical_mean
        )
    } else {
        denominator_zero <- (
            abs(monthly$historical_mean) <= resolved$zero_tolerance
        )
        if (any(denominator_zero)) {
            cli::cli_abort(
                "Multiplicative Linear Scaling is undefined because the historical monthly mean is zero for month(s) {.val {monthly$cf_month[denominator_zero]}}."
            )
        }
        # Precipitation uses the monthly observed-to-historical mean ratio as
        # a multiplicative correction on future daily values.
        monthly$correction <- (
            monthly$observed_mean / monthly$historical_mean
        )
    }

    future <- series$model_future
    correction <- monthly$correction[
        match(future[["cf_month"]], monthly$cf_month)
    ]
    if (identical(resolved$transformation, "additive")) {
        adjusted <- future[["value"]] + correction
    } else {
        adjusted <- future[["value"]] * correction
    }
    bounded <- pmin(
        pmax(adjusted, resolved$bounds[[1L]]),
        resolved$bounds[[2L]]
    )
    clipped <- sum(bounded != adjusted)
    future[["value"]] <- bounded

    bias__daily_adjusted_series(
        future,
        output_role = "model_future",
        transformation = resolved$transformation,
        settings = resolved,
        provenance = list(
            method = "linear_scaling",
            references = BIAS_LINEAR_SCALING_REFERENCES,
            group_key = key,
            monthly_corrections = monthly,
            clipped_values = clipped
        )
    )
}

# Construct the package-native Linear Scaling signal component with three
# explicit input roles and alternative supported univariate variables.
bias__linear_scaling_component <- function() {
    alternatives <- as.list(c("tas", "tasmin", "tasmax", "pr"))
    requirements <- lapply(
        c(
            "observed_reference",
            "model_historical",
            "model_future"
        ),
        function(role) {
            component__input_requirement(
                role,
                representations = "series",
                frequencies = "day",
                variable_sets = alternatives
            )
        }
    )
    names(requirements) <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    signal__component(
        name = "linear_scaling_daily",
        label = "Daily Linear Scaling",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = FALSE,
        profiles = bias__linear_scaling_profiles(),
        apply_group = bias__linear_scaling_apply_group,
        operations = list(
            validate_result = bias__validate_linear_scaling_result
        ),
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            references = BIAS_LINEAR_SCALING_REFERENCES
        )
    )
}

# Register the Linear Scaling signal component once so it is discoverable
# through the same process-local component registry as complete recipes.
bias__register_linear_scaling_component <- function() {
    component <- bias__linear_scaling_component()
    key <- component__registry_key(component@stage, component@name)
    if (!exists(
        key,
        envir = WEATHER_COMPONENT_REGISTRY,
        inherits = FALSE
    )) {
        component__register(component)
    }
    invisible(NULL)
}

# Define published Delta Change defaults for additive temperature changes and
# multiplicative precipitation changes on the observed-reference backbone.
bias__delta_change_profiles <- function() {
    temperature_settings <- list(
        grouping = "calendar_month",
        statistic = "mean",
        transformation = "additive",
        bounds = c(-Inf, Inf),
        zero_tolerance = 0
    )
    precipitation_settings <- list(
        grouping = "calendar_month",
        statistic = "mean",
        transformation = "multiplicative",
        bounds = c(0, Inf),
        zero_tolerance = sqrt(.Machine$double.eps)
    )
    temperature <- lapply(c("tas", "tasmin", "tasmax"), function(variable) {
        signal__variable_profile(
            variable,
            settings = temperature_settings,
            evidence = "published",
            references = BIAS_DELTA_CHANGE_REFERENCES,
            metadata = list(
                method = "delta_change",
                output_role = "observed_reference"
            )
        )
    })
    precipitation <- signal__variable_profile(
        "pr",
        settings = precipitation_settings,
        evidence = "published",
        references = BIAS_DELTA_CHANGE_REFERENCES,
        metadata = list(
            method = "delta_change",
            output_role = "observed_reference"
        )
    )
    c(temperature, list(precipitation))
}

# Apply published monthly Delta Change equations to the observed daily series
# and retain that series as the typed temporal backbone of the result.
bias__delta_change_apply_group <- function(inputs, settings, key) {
    method <- "Delta Change"
    resolved <- bias__mean_change_settings(settings, method)
    variable <- names(settings)[[1L]]
    series <- bias__mean_change_inputs(
        inputs,
        variable,
        resolved$transformation,
        method
    )
    monthly <- bias__mean_change_monthly_means(
        series,
        "observed_reference",
        method
    )

    if (identical(resolved$transformation, "additive")) {
        # For temperature, Delta_m = mean(future_m) - mean(historical_m)
        # transfers the modeled mean change without replacing observed
        # day-to-day anomalies.
        monthly$change <- (
            monthly$future_mean - monthly$historical_mean
        )
    } else {
        denominator_zero <- (
            abs(monthly$historical_mean) <= resolved$zero_tolerance
        )
        if (any(denominator_zero)) {
            cli::cli_abort(
                "Multiplicative Delta Change is undefined because the historical monthly mean is zero for month(s) {.val {monthly$cf_month[denominator_zero]}}."
            )
        }
        # For precipitation, R_m = mean(future_m) / mean(historical_m)
        # scales observed wet-day magnitudes while preserving the observed
        # zero/non-zero occurrence sequence.
        monthly$change <- (
            monthly$future_mean / monthly$historical_mean
        )
    }

    observed <- series$observed_reference
    change <- monthly$change[
        match(observed[["cf_month"]], monthly$cf_month)
    ]
    if (identical(resolved$transformation, "additive")) {
        # The additive output equation is x'_obs,d = x_obs,d + Delta_m(d).
        adjusted <- observed[["value"]] + change
    } else {
        # The multiplicative output equation is x'_obs,d = x_obs,d * R_m(d).
        adjusted <- observed[["value"]] * change
    }
    bounded <- pmin(
        pmax(adjusted, resolved$bounds[[1L]]),
        resolved$bounds[[2L]]
    )
    clipped <- sum(bounded != adjusted)
    observed[["value"]] <- bounded

    bias__daily_adjusted_series(
        observed,
        output_role = "observed_reference",
        transformation = resolved$transformation,
        settings = resolved,
        provenance = list(
            method = "delta_change",
            references = BIAS_DELTA_CHANGE_REFERENCES,
            group_key = key,
            monthly_changes = monthly,
            clipped_values = clipped
        )
    )
}

# Construct the package-native Delta Change signal with explicit three-role
# inputs and an observed-reference daily output.
bias__delta_change_component <- function() {
    alternatives <- as.list(c("tas", "tasmin", "tasmax", "pr"))
    requirements <- lapply(
        c(
            "observed_reference",
            "model_historical",
            "model_future"
        ),
        function(role) {
            component__input_requirement(
                role,
                representations = "series",
                frequencies = "day",
                variable_sets = alternatives
            )
        }
    )
    names(requirements) <- c(
        "observed_reference",
        "model_historical",
        "model_future"
    )
    signal__component(
        name = "delta_change_daily",
        label = "Daily Delta Change",
        required_inputs = requirements,
        input_kinds = "calendar_indexed_daily_series",
        output_kinds = "daily_adjusted_series",
        scopes = "univariate",
        stochastic = FALSE,
        profiles = bias__delta_change_profiles(),
        apply_group = bias__delta_change_apply_group,
        operations = list(
            validate_result = bias__validate_delta_change_result
        ),
        metadata = list(
            method_family = "bias_adjustment",
            output_contract = "daily_adjusted_series",
            references = BIAS_DELTA_CHANGE_REFERENCES
        )
    )
}

# Register Delta Change once so it is discoverable alongside Linear Scaling
# through the process-local component registry.
bias__register_delta_change_component <- function() {
    component <- bias__delta_change_component()
    key <- component__registry_key(component@stage, component@name)
    if (!exists(
        key,
        envir = WEATHER_COMPONENT_REGISTRY,
        inherits = FALSE
    )) {
        component__register(component)
    }
    invisible(NULL)
}
