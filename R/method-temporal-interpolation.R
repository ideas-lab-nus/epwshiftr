#' @include calendar-subdaily.R
NULL

# Piecewise-linear interpolation is valid here only for continuous state or
# point variables. Fluxes, accumulations, and radiation require other methods.
TEMPORAL_LINEAR_VARIABLES <- c(
    "tas",
    "huss",
    "hurs",
    "ps",
    "psl",
    "uas",
    "vas"
)

# Normalize one role table to the canonical sub-daily contract before splitting
# it into independent interpolation series.
temporal__linear_source <- function(input, role) {
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
    required <- c(
        BIAS_ADJUSTED_SERIES_COLUMNS,
        "time"
    )
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "Role {.val {role}} is missing canonical interpolation column(s): {.val {missing}}."
        )
    }
    if (!nrow(data)) {
        cli::cli_abort(
            "Role {.val {role}} must contain sub-daily source samples."
        )
    }
    if (!inherits(data[["time"]], "POSIXt") ||
        anyNA(data[["time"]]) ||
        any(!is.finite(as.numeric(data[["time"]])))) {
        cli::cli_abort(
            "Role {.val {role}} must retain finite, non-missing POSIX source `time` values."
        )
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
    unsupported <- setdiff(variables, TEMPORAL_LINEAR_VARIABLES)
    if (length(unsupported)) {
        cli::cli_abort(c(
            "Role {.val {role}} contains variable(s) without linear point-state semantics: {.val {unsupported}}.",
            "i" = "Radiation, fluxes, accumulations, and derived variables require their dedicated temporal method."
        ))
    }
    data.table::set(
        data,
        j = "cf_second_of_day",
        value = temporal__second_of_day(data, role)
    )
    list(data = data, frequencies = frequencies)
}

# Validate one ordered source series, including its declared timestep and the
# consistency between surrogate POSIX time and native CF elapsed seconds.
temporal__validate_linear_group <- function(
    data,
    frequency,
    time_step_seconds,
    label
) {
    # The shared validator selects key columns with data-frame semantics, so
    # normalize the internal data.table before crossing that boundary.
    error <- bias__subdaily_data_error(
        as.data.frame(data),
        frequency,
        time_step_seconds
    )
    if (!is.null(error)) {
        cli::cli_abort(
            "Interpolation group {.val {label}} is invalid: {error}"
        )
    }
    native_seconds <- temporal__native_seconds(
        data,
        data[["cf_calendar"]][[1L]]
    )
    if (anyDuplicated(native_seconds)) {
        cli::cli_abort(
            "Interpolation group {.val {label}} contains duplicate native CF times."
        )
    }
    elapsed <- diff(native_seconds)
    tolerance <- 1e-6
    if (length(elapsed) &&
        any(abs(elapsed - time_step_seconds) > tolerance)) {
        cli::cli_abort(
            "Interpolation group {.val {label}} contains a gap or irregular source timestep."
        )
    }
    posix_elapsed <- diff(as.numeric(data[["time"]]))
    if (length(posix_elapsed) &&
        any(abs(posix_elapsed - elapsed) > tolerance)) {
        cli::cli_abort(
            "Interpolation group {.val {label}} has source `time` values inconsistent with its native CF chronology."
        )
    }
    if (length(native_seconds) < 2L) {
        cli::cli_abort(
            "Interpolation group {.val {label}} must contain at least two source samples."
        )
    }
    native_seconds
}

# Interpolate one continuous variable group and record the exact two source
# samples contributing to every target value.
temporal__linear_group <- function(
    data,
    group_columns,
    frequency,
    time_step_seconds,
    anchors = NULL
) {
    data <- data.table::as.data.table(data.table::copy(data))
    data.table::set(
        data,
        j = ".temporal_source_row",
        value = seq_len(nrow(data))
    )
    data.table::setorderv(
        data,
        c(
            "cf_year",
            "cf_day_of_year",
            "cf_second_of_day"
        )
    )
    label <- temporal__group_label(data, group_columns)
    native_seconds <- temporal__validate_linear_group(
        data,
        frequency,
        time_step_seconds,
        label
    )

    # Optional hourly anchors extend the interpolation support without
    # weakening validation of the original regular sub-daily source series.
    support_seconds <- native_seconds
    support_value <- as.numeric(data[["value"]])
    support_label <- temporal__cf_time_label(data)
    support_row <- data[[".temporal_source_row"]]
    support_kind <- rep.int("model_subdaily", nrow(data))
    if (!is.null(anchors)) {
        checkmate::assert_data_frame(
            anchors,
            min.rows = 1L,
            col.names = "unique"
        )
        required_anchor_columns <- c(
            "native_second",
            "value",
            "source_time",
            "source_row",
            "source_kind"
        )
        missing <- setdiff(required_anchor_columns, names(anchors))
        if (length(missing)) {
            cli::cli_abort(
                "Interpolation anchors for group {.val {label}} are missing column(s): {.val {missing}}."
            )
        }
        anchor_seconds <- as.numeric(anchors[["native_second"]])
        if (any(!is.finite(anchor_seconds)) ||
            any(abs(anchor_seconds / 3600 - round(anchor_seconds / 3600)) >
                1e-6)) {
            cli::cli_abort(
                "Interpolation anchors for group {.val {label}} must use whole-hour native CF times."
            )
        }
        if (any(anchor_seconds <= native_seconds[[1L]] |
            anchor_seconds >= native_seconds[[length(native_seconds)]])) {
            cli::cli_abort(
                "Interpolation anchors for group {.val {label}} must lie strictly inside source support."
            )
        }
        if (anyDuplicated(anchor_seconds) ||
            any(anchor_seconds %in% native_seconds)) {
            cli::cli_abort(
                "Interpolation anchors for group {.val {label}} must use unique times between original source samples."
            )
        }
        if (any(!is.finite(as.numeric(anchors[["value"]])))) {
            cli::cli_abort(
                "Interpolation anchors for group {.val {label}} must contain finite values."
            )
        }
        support_seconds <- c(support_seconds, anchor_seconds)
        support_value <- c(
            support_value,
            as.numeric(anchors[["value"]])
        )
        support_label <- c(
            support_label,
            as.character(anchors[["source_time"]])
        )
        support_row <- c(
            support_row,
            as.integer(anchors[["source_row"]])
        )
        support_kind <- c(
            support_kind,
            as.character(anchors[["source_kind"]])
        )
        support_order <- order(support_seconds)
        support_seconds <- support_seconds[support_order]
        support_value <- support_value[support_order]
        support_label <- support_label[support_order]
        support_row <- support_row[support_order]
        support_kind <- support_kind[support_order]
    }

    # Hourly targets are anchored to native midnight and remain within the
    # observed support interval, so this component never extrapolates.
    target_step <- 3600
    target_start <- ceiling((native_seconds[[1L]] - 1e-6) / target_step) *
        target_step
    target_end <- floor((native_seconds[[length(native_seconds)]] + 1e-6) /
        target_step) * target_step
    target_seconds <- seq.int(
        from = target_start,
        to = target_end,
        by = target_step
    )
    exact <- match(target_seconds, support_seconds)
    left <- findInterval(target_seconds, support_seconds)
    right <- left + 1L
    matched <- !is.na(exact)
    left[matched] <- exact[matched]
    right[matched] <- exact[matched]
    if (any(left < 1L | right > length(support_seconds))) {
        cli::cli_abort(
            "Interpolation group {.val {label}} produced a target outside source support."
        )
    }

    # The right-hand weight defines the linear equation while exact source
    # instants retain their original value without floating-point averaging.
    denominator <- support_seconds[right] - support_seconds[left]
    weight_right <- numeric(length(target_seconds))
    interior <- left != right
    weight_right[interior] <- (
        target_seconds[interior] - support_seconds[left[interior]]
    ) / denominator[interior]
    value <- support_value[left] * (1 - weight_right) +
        support_value[right] * weight_right
    value[matched] <- support_value[exact[matched]]

    calendar <- data[["cf_calendar"]][[1L]]
    target <- temporal__target_coordinates(target_seconds, calendar)
    time_zone <- attr(data[["time"]], "tzone")
    if (is.null(time_zone) || !length(time_zone) || !nzchar(time_zone[[1L]])) {
        time_zone <- "UTC"
    } else {
        time_zone <- time_zone[[1L]]
    }
    target_time <- as.POSIXct(
        as.numeric(data[["time"]][[1L]]) +
            target_seconds - native_seconds[[1L]],
        origin = "1970-01-01",
        tz = time_zone
    )
    source_labels <- temporal__cf_time_label(data)
    output_group_columns <- setdiff(
        group_columns,
        c("cf_calendar", "frequency")
    )
    constant <- as.data.frame(data)[
        rep.int(1L, length(target_seconds)),
        output_group_columns,
        drop = FALSE
    ]
    out <- data.table::as.data.table(cbind(
        constant,
        data.frame(
            value = value,
            frequency = rep.int("hour", length(target_seconds)),
            time = target_time,
            target$coordinates,
            source_frequency = rep.int(
                frequency,
                length(target_seconds)
            ),
            source_time_left = support_label[left],
            source_time_right = support_label[right],
            source_row_left = support_row[left],
            source_row_right = support_row[right],
            source_kind_left = support_kind[left],
            source_kind_right = support_kind[right],
            interpolation_weight_right = weight_right,
            temporal_interpolation = rep.int(
                "linear",
                length(target_seconds)
            ),
            stringsAsFactors = FALSE
        )
    ))

    # Preserve compatibility aliases only when the source table carried them;
    # their values must describe the new target coordinates.
    aliases <- list(
        year = as.integer(target$fields$year),
        month = as.integer(target$fields$month),
        day = as.integer(target$fields$day),
        hour = as.integer(target$fields$hour),
        minute = as.integer(target$fields$minute),
        second = as.numeric(target$fields$second),
        datetime = target_time
    )
    for (column in intersect(names(aliases), names(data))) {
        data.table::set(out, j = column, value = aliases[[column]])
    }

    diagnostic <- data.table::data.table(
        group = label,
        variable_id = data[["variable_id"]][[1L]],
        source_frequency = frequency,
        source_step_seconds = as.numeric(time_step_seconds),
        target_frequency = "hour",
        target_step_seconds = as.numeric(target_step),
        source_samples = nrow(data),
        anchor_samples = length(support_seconds) - nrow(data),
        anchor_hour_policies = if (is.null(anchors) ||
            !"hour_policy" %in% names(anchors)) {
            NA_character_
        } else {
            paste(sort(unique(anchors[["hour_policy"]])), collapse = ",")
        },
        anchor_pair_policies = if (is.null(anchors) ||
            !"pair_policy" %in% names(anchors)) {
            NA_character_
        } else {
            paste(sort(unique(anchors[["pair_policy"]])), collapse = ",")
        },
        target_samples = nrow(out),
        interpolated_samples = sum(interior),
        source_start = source_labels[[1L]],
        source_end = source_labels[[length(source_labels)]],
        target_start = temporal__cf_time_label(out)[[1L]],
        target_end = temporal__cf_time_label(out)[[nrow(out)]],
        boundary_policy = "bounded_by_source"
    )
    list(data = out[], diagnostic = diagnostic)
}

# Interpolate every independent group in one semantic role and rebuild its
# WeatherInput descriptor with hourly frequency and retained source provenance.
temporal__linear_role <- function(
    input,
    role,
    context,
    anchor_factory = NULL
) {
    source <- temporal__linear_source(input, role)
    group_columns <- temporal__group_columns(
        source$data,
        input,
        context,
        role
    )
    groups <- base::split(
        source$data,
        by = group_columns,
        keep.by = TRUE,
        drop = TRUE
    )
    results <- lapply(groups, function(group) {
        # Frequency is part of the group identity, allowing one role to retain
        # variables drawn from different supported CMIP6 sub-daily tables.
        frequency <- unique(as.character(group[["frequency"]]))
        if (length(frequency) != 1L) {
            cli::cli_abort(
                "Each temporal interpolation group must contain one source frequency."
            )
        }
        anchors <- if (is.null(anchor_factory)) {
            NULL
        } else {
            anchor_factory(group, group_columns)
        }
        temporal__linear_group(
            group,
            group_columns = group_columns,
            frequency = frequency,
            time_step_seconds = unname(
                TEMPORAL_SOURCE_STEPS[[frequency]]
            ),
            anchors = anchors
        )
    })
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
    interpolation_record <- list(
        method = "linear_temporal_interpolation",
        source_frequencies = sort(source$frequencies),
        source_step_seconds = unname(
            TEMPORAL_SOURCE_STEPS[sort(source$frequencies)]
        ),
        target_frequency = "hour",
        target_step_seconds = 3600,
        daily_extrema_anchors = sum(vapply(
            results,
            function(result) result$diagnostic$anchor_samples[[1L]],
            integer(1L)
        )),
        boundary_policy = "bounded_by_source",
        source_group_columns = group_columns,
        output_group_columns = output_group_columns
    )
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
            list(
                group_columns = output_group_columns,
                time_step_seconds = 3600
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

# Apply piecewise-linear interpolation to matching historical and future model
# roles while preserving all other role inputs unchanged.
temporal__linear_apply <- function(inputs, context, options) {
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    checkmate::assert_list(options, names = "unique")
    if (length(options)) {
        cli::cli_abort(
            "`linear_temporal_interpolation` does not accept component options."
        )
    }
    roles <- c("model_historical", "model_future")
    results <- lapply(roles, function(role) {
        temporal__linear_role(
            weather__get_input(inputs, role),
            role,
            context
        )
    })
    names(results) <- roles
    output_inputs <- weather__new_inputs(
        weather_template = weather__get_input(
            inputs,
            "weather_template"
        ),
        observed_reference = weather__get_input(
            inputs,
            "observed_reference"
        ),
        model_historical = results$model_historical$input,
        model_future = results$model_future$input
    )
    diagnostics <- data.table::rbindlist(
        lapply(results, function(result) result$diagnostics),
        use.names = TRUE,
        fill = TRUE
    )
    provenance <- list(
        method = "linear_temporal_interpolation",
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
        target_step_seconds = 3600,
        boundary_policy = "bounded_by_source"
    )
    WeatherStageResult(
        stage = "preprocess",
        component = "linear_temporal_interpolation",
        kind = "hourly_role_inputs",
        value = output_inputs,
        diagnostics = list(temporal_interpolation = diagnostics),
        provenance = provenance,
        metadata = list(
            interpolation = "piecewise_linear",
            extrapolation = "none"
        )
    )
}

# Describe the reusable point-state interpolation component independently of a
# complete future-weather recipe.
temporal__linear_component <- function() {
    variables <- lapply(TEMPORAL_LINEAR_VARIABLES, identity)
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
        name = "linear_temporal_interpolation",
        stage = "preprocess",
        label = "Linear temporal interpolation",
        required_inputs = list(
            model_historical = requirement("model_historical"),
            model_future = requirement("model_future")
        ),
        input_kinds = "role_inputs",
        output_kinds = "hourly_role_inputs",
        scopes = "univariate",
        stochastic = FALSE,
        operations = list(apply = temporal__linear_apply),
        metadata = list(
            algorithm = "piecewise_linear_interpolation",
            source_frequencies = names(TEMPORAL_SOURCE_STEPS),
            target_frequency = "hour",
            target_step_seconds = 3600,
            supported_variables = TEMPORAL_LINEAR_VARIABLES,
            boundary_policy = "bounded_by_source",
            radiation_policy = "dedicated_solar_interpolation_required"
        )
    )
}

# Register the reusable preprocess implementation once for recipe compilation
# and standalone component inspection.
temporal__register_linear_component <- function() {
    component__register_builtin(temporal__linear_component())
    invisible(NULL)
}
