# Native CF sub-daily coordinate support {{{

# Regular source frequencies shared by point-state and interval-mean temporal
# components. Named seconds keep validation independent of timestamp parsing.
TEMPORAL_SOURCE_STEPS <- c(
    `3hr` = 10800,
    `6hr` = 21600
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

# Derive an exact native-calendar second-of-day coordinate from annual phase
# when an extracted climate table predates the explicit sub-daily field.
temporal__second_of_day <- function(data, name) {
    if ("cf_second_of_day" %in% names(data)) {
        seconds <- as.numeric(data[["cf_second_of_day"]])
    } else {
        raw_seconds <- (
            data[["annual_phase"]] * data[["cf_year_days"]] -
                (data[["cf_day_of_year"]] - 1L)
        ) * 86400
        seconds <- round(raw_seconds)
        if (any(!is.finite(raw_seconds)) ||
            any(abs(raw_seconds - seconds) > 1e-4)) {
            cli::cli_abort(
                "{.arg {name}} cannot derive exact whole-second sub-daily coordinates from `annual_phase`."
            )
        }
    }
    if (any(!is.finite(seconds)) ||
        any(seconds < 0 | seconds >= 86400) ||
        any(abs(seconds - round(seconds)) > 1e-6)) {
        cli::cli_abort(
            "{.arg {name}} must use whole-second `cf_second_of_day` values in [0, 86400)."
        )
    }
    as.numeric(round(seconds))
}

# Convert CF date tuples to an absolute native-calendar second key. Year 1 is
# a stable arithmetic origin and never assigns Gregorian dates to other calendars.
temporal__native_seconds <- function(data, calendar) {
    origin <- data.frame(year = 1L, month = 1L, day = 1L)
    parts <- data.frame(
        year = as.integer(data[["cf_year"]]),
        month = as.integer(data[["cf_month"]]),
        day = as.integer(data[["cf_day"]])
    )
    as.numeric(cf_time_date2offset(parts, origin, calendar)) * 86400 +
        as.numeric(data[["cf_second_of_day"]])
}

# Format one native CF timestamp without converting dates such as February 30
# into a Gregorian surrogate.
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

# Resolve group identity from standard climate metadata plus any explicit
# adapter or context fields while rejecting time-varying grouping keys.
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

# Render a stable diagnostic label for one independently processed native-time
# series.
temporal__group_label <- function(data, group_columns) {
    values <- vapply(group_columns, function(column) {
        value <- data[[column]][[1L]]
        if (is.na(value)) "<NA>" else as.character(value)
    }, character(1L))
    paste(sprintf("%s=%s", group_columns, values), collapse = ",")
}

# Convert absolute native seconds back to exact CF coordinates on an hourly,
# midnight-anchored target lattice.
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
