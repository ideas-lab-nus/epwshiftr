# Hourly signal groups use one observed source and matching historical and
# future model sources while keeping every role's native CF calendar intact.
HOURLY_CALENDAR_ROLES <- c(
    "observed_reference",
    "model_historical",
    "model_future"
)

# Model identity fields must agree between historical and future roles within
# one signal group; scenario and period remain future-case provenance instead.
HOURLY_CALENDAR_MODEL_ID_COLUMNS <- c(
    "activity_drs",
    "activity_id",
    "institution_id",
    "source_id",
    "variant_label",
    "member_id",
    "grid_label"
)

# Role-local fields detect accidental mixtures of independent time series
# before calendar validation is reduced to one variable and one site.
HOURLY_CALENDAR_SERIES_ID_COLUMNS <- c(
    HOURLY_CALENDAR_MODEL_ID_COLUMNS,
    "experiment_id",
    "table_id",
    "period",
    "method",
    "lon",
    "lat"
)

# Stable group-key fields identify the future-model case without treating
# role-specific calendars or timestamps as shared alignment coordinates.
HOURLY_CALENDAR_KEY_COLUMNS <- c(
    "site_id",
    "source_id",
    "experiment_id",
    "variant_label",
    "member_id",
    "grid_label",
    "period"
)

# Copy and validate one materialized hourly role before it is split into
# independent calendar-native signal series.
hourly_calendar__role_data <- function(input, role) {
    if (!S7::S7_inherits(input, WeatherInput)) {
        cli::cli_abort("Role {.val {role}} must contain a WeatherInput object.")
    }
    if (!identical(input@representation, "series") ||
        !is.data.frame(input@source)) {
        cli::cli_abort(
            "Role {.val {role}} must contain a materialized series input."
        )
    }
    data <- data.table::as.data.table(data.table::copy(input@source))
    missing <- setdiff(BIAS_SUBDAILY_SERIES_COLUMNS, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "Role {.val {role}} is missing canonical hourly column(s): {.val {missing}}."
        )
    }
    if (!nrow(data)) {
        cli::cli_abort("Role {.val {role}} must contain hourly samples.")
    }
    variables <- sort(unique(as.character(data[["variable_id"]])))
    if (anyNA(variables) || any(!nzchar(variables))) {
        cli::cli_abort(
            "Role {.val {role}} contains a missing or empty `variable_id`."
        )
    }
    if (length(input@variables) &&
        !setequal(input@variables, variables)) {
        cli::cli_abort(
            "Role {.val {role}} variable descriptors do not match its materialized rows."
        )
    }
    if (!identical(unique(as.character(data[["frequency"]])), "hour") ||
        (length(input@frequencies) &&
            !identical(input@frequencies, "hour"))) {
        cli::cli_abort(
            "Role {.val {role}} must contain only hourly data."
        )
    }
    data[]
}

# Return explicit site identifiers, rejecting partial site labels that could
# silently merge distinct locations into an implicit singleton group.
hourly_calendar__site_values <- function(data, role) {
    if (!"site_id" %in% names(data)) {
        return(character())
    }
    values <- as.character(data[["site_id"]])
    missing <- is.na(values) | !nzchar(values)
    if (any(missing) && !all(missing)) {
        cli::cli_abort(
            "Role {.val {role}} mixes labelled and unlabelled `site_id` values."
        )
    }
    if (all(missing)) {
        return(character())
    }
    sort(unique(values))
}

# Resolve the shared site groups. A role without `site_id` may be broadcast
# only when every labelled role contains the same single site.
hourly_calendar__sites <- function(role_data) {
    sites <- lapply(names(role_data), function(role) {
        hourly_calendar__site_values(role_data[[role]], role)
    })
    names(sites) <- names(role_data)
    labelled <- sites[lengths(sites) > 0L]
    if (!length(labelled)) {
        return(list(NULL))
    }
    expected <- labelled[[1L]]
    if (!all(vapply(labelled, identical, logical(1L), expected))) {
        cli::cli_abort(
            "Hourly signal roles must contain the same `site_id` values."
        )
    }
    if (length(expected) > 1L && any(lengths(sites) == 0L)) {
        cli::cli_abort(
            "A role without `site_id` cannot be aligned to multiple hourly sites."
        )
    }
    as.list(expected)
}

# Select one site while allowing an unlabelled singleton role to supply the
# same reference series for the sole labelled site.
hourly_calendar__site_rows <- function(data, site) {
    if (is.null(site) || !"site_id" %in% names(data)) {
        return(data)
    }
    values <- as.character(data[["site_id"]])
    labelled <- !is.na(values) & nzchar(values)
    if (!any(labelled)) {
        return(data)
    }
    data[values == site]
}

# Reduce role-local metadata to atomic scalar identities and reject accidental
# mixtures of models, members, grids, periods, or geographic points.
hourly_calendar__series_identity <- function(data, role, variable, site) {
    columns <- intersect(HOURLY_CALENDAR_SERIES_ID_COLUMNS, names(data))
    identity <- list()
    for (column in columns) {
        values <- unique(data[[column]])
        present <- !is.na(values)
        if (is.character(values)) {
            present <- present & nzchar(values)
        }
        values <- values[present]
        if (length(values) > 1L) {
            site_label <- if (is.null(site)) "<implicit>" else site
            cli::cli_abort(
                "Role {.val {role}} contains multiple {.field {column}} values for variable {.val {variable}} at site {.val {site_label}}."
            )
        }
        if (length(values) == 1L && is.atomic(values)) {
            identity[[column]] <- values[[1L]]
        }
    }
    identity
}

# Compare the stable historical/future model identity without requiring their
# experiment, period, table, or native calendar to be identical.
hourly_calendar__validate_model_identity <- function(
    historical,
    future,
    variable
) {
    columns <- intersect(
        HOURLY_CALENDAR_MODEL_ID_COLUMNS,
        intersect(names(historical), names(future))
    )
    for (column in columns) {
        if (!isTRUE(all.equal(
            historical[[column]],
            future[[column]],
            check.attributes = FALSE
        ))) {
            cli::cli_abort(
                "Historical and future model identities differ in {.field {column}} for variable {.val {variable}}."
            )
        }
    }
    invisible(TRUE)
}

# Validate one complete hourly native-calendar series and return rows in exact
# CF chronological order together with compact coverage diagnostics.
hourly_calendar__series <- function(data, role, variable, site) {
    label <- paste(
        role,
        variable,
        if (is.null(site)) "<implicit-site>" else site,
        sep = "/"
    )
    canonical <- bias__subdaily_table(
        as.data.frame(data, stringsAsFactors = FALSE),
        frequency = "hour",
        time_step_seconds = 3600,
        name = label
    )
    calendars <- unique(as.character(canonical[["cf_calendar"]]))
    units <- unique(as.character(canonical[["units"]]))
    if (length(calendars) != 1L) {
        cli::cli_abort(
            "Hourly series {.val {label}} must contain one native CF calendar."
        )
    }
    if (length(units) != 1L) {
        cli::cli_abort(
            "Hourly series {.val {label}} must contain one unit."
        )
    }

    # Every complete native-calendar day must use one shared 24-position
    # hourly lattice, independent of the role's calendar or starting phase.
    offsets <- sort(unique(as.numeric(canonical[["cf_second_of_day"]])))
    wrapped_steps <- diff(c(offsets, offsets[[1L]] + 86400))
    if (length(offsets) != 24L ||
        any(abs(wrapped_steps - 3600) > 1e-6)) {
        cli::cli_abort(
            "Hourly series {.val {label}} must use all 24 positions of one regular hourly daily lattice."
        )
    }
    day_key <- paste(
        canonical[["cf_year"]],
        canonical[["cf_day_of_year"]],
        sep = "\r"
    )
    day_offsets <- split(canonical[["cf_second_of_day"]], day_key)
    complete_days <- vapply(day_offsets, function(value) {
        identical(sort(as.numeric(value)), offsets)
    }, logical(1L))
    if (!all(complete_days)) {
        cli::cli_abort(
            "Hourly series {.val {label}} contains incomplete native-calendar day(s)."
        )
    }

    # Full-year coverage is required here because later direct-model sequence
    # output cannot reconstruct a missing day without changing chronology.
    years <- sort(unique(as.integer(canonical[["cf_year"]])))
    for (year in years) {
        rows <- canonical[["cf_year"]] == year
        observed_days <- sort(unique(as.integer(
            canonical[["cf_day_of_year"]][rows]
        )))
        expected_days <- unique(as.integer(
            canonical[["cf_year_days"]][rows]
        ))
        if (length(expected_days) != 1L ||
            !identical(observed_days, seq_len(expected_days))) {
            cli::cli_abort(
                "Hourly series {.val {label}} must cover every native-calendar day in year {year}."
            )
        }
    }

    canonical <- data.table::as.data.table(canonical)
    data.table::setorderv(
        canonical,
        c("cf_year", "cf_day_of_year", "cf_second_of_day")
    )
    list(
        data = as.data.frame(canonical, stringsAsFactors = FALSE),
        identity = hourly_calendar__series_identity(
            canonical,
            role,
            variable,
            site
        ),
        diagnostic = data.table::data.table(
            role = role,
            site_id = if (is.null(site)) NA_character_ else site,
            variable_id = variable,
            units = units,
            cf_calendar = calendars,
            first_year = min(years),
            last_year = max(years),
            complete_years = length(years),
            complete_days = length(day_offsets),
            samples = nrow(canonical),
            hour_phase_seconds = offsets[[1L]]
        )
    )
}

# Return one scalar case key from the pipeline context and future-model
# metadata, preferring the explicit future case where both sources provide it.
hourly_calendar__group_key <- function(context, future_identity, site) {
    key <- list()
    if (inherits(context, "morpher__context") &&
        is.data.frame(context$case) &&
        nrow(context$case) == 1L) {
        for (column in intersect(
            HOURLY_CALENDAR_KEY_COLUMNS,
            names(context$case)
        )) {
            value <- context$case[[column]][[1L]]
            if (is.atomic(value) && length(value) == 1L && !is.na(value) &&
                (!is.character(value) || nzchar(value))) {
                key[[column]] <- value
            }
        }
    }
    for (column in intersect(
        HOURLY_CALENDAR_KEY_COLUMNS,
        names(future_identity)
    )) {
        key[[column]] <- future_identity[[column]]
    }
    if (!is.null(site)) {
        key$site_id <- site
    }
    key[HOURLY_CALENDAR_KEY_COLUMNS[
        HOURLY_CALENDAR_KEY_COLUMNS %in% names(key)
    ]]
}

# Convert role-addressable hourly inputs into univariate SignalGroup objects
# without pairing timestamps or coercing any role to another role's calendar.
hourly_calendar__apply <- function(data, inputs, context, options) {
    if (!S7::S7_inherits(data, WeatherInputs)) {
        cli::cli_abort(
            "`hourly_calendar_grouping` requires `hourly_role_inputs` carried by a WeatherInputs object."
        )
    }
    checkmate::assert_list(options, names = "unique")
    if (length(options)) {
        cli::cli_abort(
            "`hourly_calendar_grouping` does not accept component options."
        )
    }
    role_data <- lapply(HOURLY_CALENDAR_ROLES, function(role) {
        hourly_calendar__role_data(weather__get_input(data, role), role)
    })
    names(role_data) <- HOURLY_CALENDAR_ROLES
    variable_sets <- lapply(role_data, function(value) {
        sort(unique(as.character(value[["variable_id"]])))
    })
    if (!all(vapply(
        variable_sets[-1L],
        identical,
        logical(1L),
        variable_sets[[1L]]
    ))) {
        details <- paste(
            sprintf(
                "%s=[%s]",
                names(variable_sets),
                vapply(variable_sets, paste, character(1L), collapse = ",")
            ),
            collapse = "; "
        )
        cli::cli_abort(
            "Hourly signal roles must contain identical variable sets: {details}."
        )
    }

    groups <- list()
    diagnostics <- list()
    sites <- hourly_calendar__sites(role_data)
    variables <- variable_sets[[1L]]
    for (site in sites) {
        site_data <- lapply(role_data, hourly_calendar__site_rows, site = site)
        for (variable in variables) {
            series <- lapply(names(site_data), function(role) {
                rows <- site_data[[role]][["variable_id"]] == variable
                if (!any(rows)) {
                    site_label <- if (is.null(site)) "<implicit>" else site
                    cli::cli_abort(
                        "Role {.val {role}} has no hourly {.val {variable}} series for site {.val {site_label}}."
                    )
                }
                hourly_calendar__series(
                    site_data[[role]][rows],
                    role,
                    variable,
                    site
                )
            })
            names(series) <- names(site_data)
            units <- vapply(
                series,
                function(value) unique(value$data[["units"]]),
                character(1L)
            )
            if (length(unique(units)) != 1L) {
                cli::cli_abort(
                    "Hourly signal roles for variable {.val {variable}} must use identical units."
                )
            }
            hourly_calendar__validate_model_identity(
                series$model_historical$identity,
                series$model_future$identity,
                variable
            )
            key <- hourly_calendar__group_key(
                context,
                series$model_future$identity,
                site
            )
            groups[[length(groups) + 1L]] <- signal__group(
                key = key,
                inputs = lapply(series, `[[`, "data"),
                variables = variable
            )
            diagnostics <- c(
                diagnostics,
                lapply(series, `[[`, "diagnostic")
            )
        }
    }
    diagnostic_table <- data.table::rbindlist(
        diagnostics,
        use.names = TRUE,
        fill = TRUE
    )
    WeatherStageResult(
        stage = "calendar",
        component = "hourly_calendar_grouping",
        kind = "calendar_indexed_hourly_series",
        value = groups,
        diagnostics = list(
            hourly_calendar_grouping = diagnostic_table
        ),
        provenance = list(
            method = "hourly_calendar_grouping",
            roles = HOURLY_CALENDAR_ROLES,
            variables = variables,
            group_count = length(groups),
            calendar_policy = "preserve_role_native_cf_calendar",
            date_pairing = "none",
            completeness_policy = "complete_native_calendar_years",
            time_step_seconds = 3600
        ),
        metadata = list(
            grouping = "univariate_by_case_and_site",
            calendar_alignment = "role_native_no_rowwise_pairing"
        )
    )
}

# Describe the reusable hourly calendar-grouping boundary independently of any
# one bias-adjustment method or complete future-weather recipe.
hourly_calendar__component <- function() {
    requirements <- lapply(HOURLY_CALENDAR_ROLES, function(role) {
        component__input_requirement(
            role,
            representations = "series",
            calendars = CF_TIME_CALENDARS
        )
    })
    names(requirements) <- HOURLY_CALENDAR_ROLES
    component__spec(
        name = "hourly_calendar_grouping",
        stage = "calendar",
        label = "Hourly calendar grouping",
        required_inputs = requirements,
        input_kinds = "hourly_role_inputs",
        output_kinds = "calendar_indexed_hourly_series",
        scopes = "univariate",
        stochastic = FALSE,
        operations = list(apply = hourly_calendar__apply),
        metadata = list(
            grouping = "variable_case_site",
            required_frequency = "hour",
            required_time_step_seconds = 3600,
            calendar_policy = "preserve_role_native_cf_calendar",
            date_pairing = "none",
            completeness_policy = "complete_native_calendar_years",
            output_contract = "calendar_indexed_hourly_series"
        )
    )
}

# Register the standalone calendar implementation once so hourly signal
# components can resolve it without constructing a complete recipe.
hourly_calendar__register_component <- function() {
    component <- hourly_calendar__component()
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
