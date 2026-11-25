# Native CF sub-daily coordinate support
# Regular source frequencies shared by point-state and interval-mean temporal
# components. Named seconds keep validation independent of timestamp parsing.
TEMPORAL_SOURCE_STEPS <- c(
    `3hr` = 10800,
    `3hrPt` = 10800,
    `6hr` = 21600,
    `6hrPt` = 21600
)

# Known identity fields prevent unrelated sites, models, members, periods, or
# extracted grid points from sharing a temporal operation.
TEMPORAL_ID_COLUMNS <- c(
    "site_id",
    "activity_drs",
    "activity_id",
    "institution_id",
    "source_id",
    "experiment_id",
    "variant_label",
    "member_id",
    "grid_label",
    "table_id",
    "period",
    "variable_id",
    "units",
    "lon",
    "lat",
    "method"
)

# Fields that change along a time series cannot be declared as group identity
# columns by an input adapter or morphing context.
TEMPORAL_TIME_COLUMNS <- c(
    "value",
    "time",
    "time_bound_start",
    "time_bound_end",
    "datetime",
    "year",
    "month",
    "day",
    "hour",
    "minute",
    "second",
    "cf_year",
    "cf_month",
    "cf_day",
    "cf_day_of_year",
    "cf_year_days",
    "cf_second_of_day",
    "annual_phase"
)

# Describe the unique time-of-day positions and whether they form one complete
# regular daily lattice. Callers retain context-specific error diagnostics.
# temporal__daily_lattice {{{
temporal__daily_lattice <- function(
    second_of_day,
    expected_positions = 24L,
    step_seconds = 3600
) {
    checkmate::assert_numeric(
        second_of_day,
        min.len = 1L,
        finite = TRUE,
        any.missing = FALSE
    )
    checkmate::assert_count(expected_positions, positive = TRUE)
    checkmate::assert_number(step_seconds, lower = 0, finite = TRUE)
    if (step_seconds <= 0) {
        cli::cli_abort("{.arg step_seconds} must be positive.")
    }
    offsets <- sort(unique(as.numeric(second_of_day)))
    wrapped_steps <- diff(c(offsets, offsets[[1L]] + 86400))
    list(
        offsets = offsets,
        regular = length(offsets) == expected_positions &&
            all(abs(wrapped_steps - step_seconds) <= 1e-6)
    )
}
# }}}

# Read explicit native clock seconds, deriving them from annual phase only for
# persisted or externally supplied tables without the separate clock field.
# temporal__second_of_day {{{
temporal__second_of_day <- function(data, name) {
    if ("cf_second_of_day" %in% names(data)) {
        seconds <- as.numeric(data[["cf_second_of_day"]])
    } else {
        raw_seconds <- (data[["annual_phase"]] *
            data[["cf_year_days"]] -
            (data[["cf_day_of_year"]] - 1L)) *
            86400
        seconds <- round(raw_seconds)
        if (
            any(!is.finite(raw_seconds)) ||
                any(abs(raw_seconds - seconds) > 1e-4)
        ) {
            cli::cli_abort(
                "{.arg {name}} cannot derive exact whole-second sub-daily coordinates from `annual_phase`."
            )
        }
    }
    if (
        any(!is.finite(seconds)) ||
            any(seconds < 0 | seconds >= 86400) ||
            any(abs(seconds - round(seconds)) > 1e-6)
    ) {
        cli::cli_abort(
            "{.arg {name}} must use whole-second `cf_second_of_day` values in [0, 86400)."
        )
    }
    as.numeric(round(seconds))
}
# }}}

# Convert CF date tuples to an absolute native-calendar second key. Year 1 is
# a stable arithmetic origin and never assigns Gregorian dates to other calendars.
# temporal__native_seconds {{{
temporal__native_seconds <- function(data, calendar) {
    origin <- data.frame(year = 1L, month = 1L, day = 1L)
    parts <- data.frame(
        year = as.integer(data[["cf_year"]]),
        month = as.integer(data[["cf_month"]]),
        day = as.integer(data[["cf_day"]])
    )
    as.numeric(cf_time_date2offset(parts, origin, calendar)) *
        86400 +
        as.numeric(data[["cf_second_of_day"]])
}
# }}}

# Format one native CF timestamp without converting dates such as February 30
# into a Gregorian surrogate.
# temporal__cf_time_label {{{
temporal__cf_time_label <- function(data) {
    seconds <- as.integer(round(data[["cf_second_of_day"]]))
    hour <- seconds %/% 3600L
    minute <- (seconds %% 3600L) %/% 60L
    second <- seconds %% 60L
    sprintf(
        "%s:%04d-%02d-%02dT%02d:%02d:%02d",
        data[["cf_calendar"]],
        as.integer(data[["cf_year"]]),
        as.integer(data[["cf_month"]]),
        as.integer(data[["cf_day"]]),
        hour,
        minute,
        second
    )
}
# }}}

# Resolve group identity from standard climate metadata plus any explicit
# adapter or context fields while rejecting time-varying grouping keys.
# temporal__group_columns {{{
temporal__group_columns <- function(data, input, context, role) {
    declared <- input@metadata$group_columns
    if (is.null(declared)) {
        declared <- character()
    }
    checkmate::assert_character(
        declared,
        any.missing = FALSE,
        unique = TRUE
    )
    context_by <- if (inherits(context, "morpher__context")) {
        context$by
    } else {
        character()
    }
    checkmate::assert_character(
        context_by,
        any.missing = FALSE,
        unique = TRUE
    )
    requested <- unique(c(
        intersect(TEMPORAL_ID_COLUMNS, names(data)),
        declared,
        context_by,
        "variable_id",
        "units",
        "frequency",
        "cf_calendar"
    ))
    missing <- setdiff(requested, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "Role {.val {role}} declares missing interpolation group column(s): {.val {missing}}."
        )
    }
    forbidden <- intersect(requested, TEMPORAL_TIME_COLUMNS)
    if (length(forbidden)) {
        cli::cli_abort(
            "Role {.val {role}} cannot group temporal interpolation by time-varying column(s): {.val {forbidden}}."
        )
    }
    requested
}
# }}}

# Render a stable diagnostic label for one independently processed native-time
# series.
# temporal__group_label {{{
temporal__group_label <- function(data, group_columns) {
    values <- vapply(
        group_columns,
        function(column) {
            value <- data[[column]][[1L]]
            if (is.na(value)) "<NA>" else as.character(value)
        },
        character(1L)
    )
    paste(sprintf("%s=%s", group_columns, values), collapse = ",")
}
# }}}

# Convert absolute native seconds back to exact CF coordinates on an hourly,
# midnight-anchored target lattice.
# temporal__target_coordinates {{{
temporal__target_coordinates <- function(target_seconds, calendar) {
    origin <- data.frame(year = 1L, month = 1L, day = 1L)
    day_offset <- floor(target_seconds / 86400)
    second_of_day <- target_seconds - day_offset * 86400
    fields <- cf_time_offset2date(day_offset, origin, calendar)
    fields$hour <- second_of_day %/% 3600
    fields$minute <- (second_of_day %% 3600) %/% 60
    fields$second <- second_of_day %% 60
    coordinates <- cf_time__coordinates(fields, calendar)
    coordinates[["cf_second_of_day"]] <- as.numeric(second_of_day)
    list(fields = fields, coordinates = coordinates)
}
# }}}

# Validate a materialized sub-daily WeatherInput role shared by point-state
# and interval-mean temporal interpolation components.
# temporal__validate_source_role {{{
temporal__validate_source_role <- function(
    input,
    role,
    allowed_variables,
    extra_columns = character(),
    time_columns = "time",
    require_utc = FALSE,
    missing_label = "canonical interpolation",
    sample_label = "source samples",
    variable_semantics = "supported temporal semantics",
    variable_hint = NULL
) {
    if (!S7::S7_inherits(input, WeatherInput)) {
        cli::cli_abort("Role {.val {role}} must contain a WeatherInput object.")
    }
    if (
        !identical(input@representation, "series") ||
            !is.data.frame(input@source)
    ) {
        cli::cli_abort(
            "Role {.val {role}} must contain a materialized series input."
        )
    }
    data <- data.table::as.data.table(data.table::copy(input@source))
    required <- c(BIAS_ADJUSTED_SERIES_COLUMNS, "time", extra_columns)
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "Role {.val {role}} is missing {missing_label} column(s): {.val {missing}}."
        )
    }
    if (!nrow(data)) {
        cli::cli_abort(
            "Role {.val {role}} must contain sub-daily {sample_label}."
        )
    }
    for (column in time_columns) {
        value <- data[[column]]
        if (
            !inherits(value, "POSIXt") ||
                anyNA(value) ||
                any(!is.finite(as.numeric(value)))
        ) {
            cli::cli_abort(
                "Role {.val {role}} must provide finite POSIX {.field {column}} values."
            )
        }
    }
    if (isTRUE(require_utc)) {
        time_zone <- attr(data[["time"]], "tzone")
        if (
            is.null(time_zone) ||
                !length(time_zone) ||
                !time_zone[[1L]] %in% c("UTC", "GMT")
        ) {
            cli::cli_abort(
                "Role {.val {role}} must express CMIP time coordinates in UTC."
            )
        }
    }
    frequencies <- unique(as.character(data[["frequency"]]))
    unsupported_frequencies <- setdiff(
        frequencies,
        names(TEMPORAL_SOURCE_STEPS)
    )
    if (length(unsupported_frequencies)) {
        cli::cli_abort(
            "Role {.val {role}} contains unsupported source frequency value(s): {.val {unsupported_frequencies}}. Supported values are {.val {names(TEMPORAL_SOURCE_STEPS)}}."
        )
    }
    variables <- unique(as.character(data[["variable_id"]]))
    unsupported <- setdiff(variables, allowed_variables)
    if (length(unsupported)) {
        message <- paste0(
            "Role {.val {role}} contains variable(s) without ",
            variable_semantics,
            ": {.val {unsupported}}."
        )
        if (!is.null(variable_hint)) {
            message <- c(message, "i" = variable_hint)
        }
        cli::cli_abort(message)
    }
    data.table::set(
        data,
        j = "cf_second_of_day",
        value = temporal__second_of_day(data, role)
    )
    list(data = data, frequencies = frequencies)
}
# }}}

# Preserve compatibility time aliases only when the source table carried them,
# updating each column to describe the target coordinate rather than source.
# temporal__set_time_aliases {{{
temporal__set_time_aliases <- function(out, source_names, fields, time) {
    aliases <- list(
        year = as.integer(fields$year),
        month = as.integer(fields$month),
        day = as.integer(fields$day),
        hour = as.integer(fields$hour),
        minute = as.integer(fields$minute),
        second = as.numeric(fields$second),
        datetime = time
    )
    for (column in intersect(names(aliases), source_names)) {
        data.table::set(out, j = column, value = aliases[[column]])
    }
    invisible(out)
}
# }}}

# Assemble ordered hourly output rows, WeatherInput descriptor, and role-tagged
# diagnostics from per-group interpolation results.
# temporal__finalize_role {{{
temporal__finalize_role <- function(
    input,
    role,
    results,
    group_columns,
    interpolation_record,
    extra_metadata = list()
) {
    data <- data.table::rbindlist(
        lapply(results, function(result) result$data),
        use.names = TRUE,
        fill = TRUE
    )
    output_group_columns <- unique(c(
        setdiff(group_columns, "frequency"),
        "source_frequency"
    ))
    order_columns <- c(
        setdiff(output_group_columns, "cf_calendar"),
        "cf_calendar",
        "cf_year",
        "cf_day_of_year",
        "cf_second_of_day"
    )
    data.table::setorderv(data, intersect(order_columns, names(data)))
    interpolation_record$source_group_columns <- group_columns
    interpolation_record$output_group_columns <- output_group_columns
    output <- weather__new_input(
        role,
        data,
        representation = "series",
        variables = unique(as.character(data[["variable_id"]])),
        frequencies = "hour",
        calendars = unique(as.character(data[["cf_calendar"]])),
        provenance = utils::modifyList(
            input@provenance,
            list(temporal_interpolation = interpolation_record)
        ),
        metadata = utils::modifyList(
            input@metadata,
            c(
                list(
                    group_columns = output_group_columns,
                    time_step_seconds = 3600
                ),
                extra_metadata
            )
        )
    )
    diagnostics <- data.table::rbindlist(
        lapply(results, function(result) result$diagnostic),
        use.names = TRUE,
        fill = TRUE
    )
    data.table::set(
        diagnostics,
        j = "role",
        value = rep.int(role, nrow(diagnostics))
    )
    data.table::setcolorder(
        diagnostics,
        c("role", setdiff(names(diagnostics), "role"))
    )
    list(
        input = output,
        diagnostics = diagnostics,
        provenance = interpolation_record
    )
}
# }}}

# Apply one sub-daily interpolation role function to both model roles while
# preserving template and observational inputs unchanged.
# temporal__apply_model_roles {{{
temporal__apply_model_roles <- function(
    inputs,
    context,
    options,
    component_name,
    role_fn,
    diagnostic_name,
    extra_provenance = list(),
    metadata = list()
) {
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    checkmate::assert_list(options, names = "unique")
    if (length(options)) {
        cli::cli_abort(
            "`{component_name}` does not accept component options."
        )
    }
    roles <- c("model_historical", "model_future")
    results <- lapply(roles, function(role) {
        role_fn(weather__get_input(inputs, role), role, context)
    })
    names(results) <- roles
    output_inputs <- weather__new_inputs(
        weather_template = weather__get_input(inputs, "weather_template"),
        observed_reference = weather__get_input(inputs, "observed_reference"),
        model_historical = results$model_historical$input,
        model_future = results$model_future$input
    )
    diagnostics <- data.table::rbindlist(
        lapply(results, function(result) result$diagnostics),
        use.names = TRUE,
        fill = TRUE
    )
    provenance <- c(
        list(
            method = component_name,
            roles = roles,
            source_frequencies = lapply(
                results,
                function(result) result$provenance$source_frequencies
            ),
            source_step_seconds = lapply(
                results,
                function(result) result$provenance$source_step_seconds
            ),
            target_frequency = "hour",
            target_step_seconds = 3600
        ),
        extra_provenance
    )
    WeatherStageResult(
        stage = "preprocess",
        component = component_name,
        kind = "hourly_role_inputs",
        value = output_inputs,
        diagnostics = stats::setNames(list(diagnostics), diagnostic_name),
        provenance = provenance,
        metadata = metadata
    )
}
# }}}

# Build a standard sub-daily-to-hourly preprocess component specification.
# temporal__preprocess_component {{{
temporal__preprocess_component <- function(
    name,
    label,
    supported_variables,
    apply_fn,
    metadata = list()
) {
    variables <- lapply(supported_variables, identity)
    requirement <- function(role) {
        component__input_requirement(
            role,
            representations = "series",
            frequencies = names(TEMPORAL_SOURCE_STEPS),
            calendars = CF_TIME_CALENDARS,
            variable_sets = variables
        )
    }
    component__spec(
        name = name,
        stage = "preprocess",
        label = label,
        required_inputs = list(
            model_historical = requirement("model_historical"),
            model_future = requirement("model_future")
        ),
        input_kinds = "role_inputs",
        output_kinds = "hourly_role_inputs",
        scopes = "univariate",
        stochastic = FALSE,
        operations = list(apply = apply_fn),
        metadata = c(
            list(
                source_frequencies = names(TEMPORAL_SOURCE_STEPS),
                target_frequency = "hour",
                target_step_seconds = 3600,
                supported_variables = supported_variables
            ),
            metadata
        )
    )
}
# }}}

# vim: fdm=marker :
