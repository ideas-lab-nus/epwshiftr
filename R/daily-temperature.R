# Daily temperature targets and constrained projection {{{

# Validate one long-form daily temperature source before climatology estimation.
# Only tas, tasmax, and tasmin participate; unrelated variables are left out.
daily__temperature_source <- function(data, name, by) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_character(by, any.missing = FALSE, unique = TRUE)

    if (!nrow(data)) {
        cli::cli_abort("{.arg {name}} must contain at least one observation.")
    }
    if (anyDuplicated(names(data))) {
        cli::cli_abort("{.arg {name}} must have unique column names.")
    }

    required <- unique(c("variable_id", "annual_phase", "value", by))
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg {name}} is missing required column{?s}: {.val {missing}}."
        )
    }

    reserved_by <- intersect(
        by,
        c(
            "variable_id", "annual_phase", "value", "target_day",
            "climatology", "n", ".daily_value",
            "future_mean", "future_minimum", "future_maximum",
            "historical_mean", "historical_minimum", "historical_maximum",
            "n_future_mean", "n_future_minimum", "n_future_maximum",
            "n_historical_mean", "n_historical_minimum",
            "n_historical_maximum",
            "mean_delta", "minimum_delta", "maximum_delta", "dtr_delta",
            "dtr_status"
        )
    )
    if (length(reserved_by)) {
        cli::cli_abort(
            "{.arg by} cannot use temperature input or output column{?s}: {.val {reserved_by}}."
        )
    }

    checkmate::assert_character(
        data[["variable_id"]],
        any.missing = FALSE,
        min.len = 1L,
        .var.name = sprintf("%s[['variable_id']]", name)
    )
    annual_phase <- daily__check_phase(
        data[["annual_phase"]],
        sprintf("%s[['annual_phase']]", name)
    )
    checkmate::assert_numeric(
        data[["value"]],
        finite = TRUE,
        any.missing = TRUE,
        .var.name = sprintf("%s[['value']]", name)
    )

    # Work on a private table so callers retain the original extraction rows.
    source <- data.table::as.data.table(data.table::copy(data))
    data.table::set(source, j = "annual_phase", value = annual_phase)
    data.table::set(source, j = "value", value = as.numeric(source[["value"]]))
    temperature_rows <- source[["variable_id"]] %in%
        c("tas", "tasmax", "tasmin")
    source <- source[
        temperature_rows,
        c(by, "variable_id", "annual_phase", "value"),
        with = FALSE
    ]
    mean_rows <- source[["variable_id"]] == "tas"
    if (!any(mean_rows)) {
        cli::cli_abort("{.arg {name}} must contain {.val tas} observations.")
    }

    source[]
}

# Estimate one common-grid climatology for every available temperature variable.
daily__temperature_climatology <- function(data, by, window_days,
                                           target_year_days) {
    daily__climatology(
        data,
        value = "value",
        by = c(by, "variable_id"),
        window_days = window_days,
        target_year_days = target_year_days
    )
}

# Select one temperature metric from a long climatology and give its value and
# sample-count columns source-specific names for deterministic joins.
daily__temperature_metric <- function(climatology, variable, by, source,
                                      metric) {
    keys <- c(by, "target_day", "annual_phase")
    requested_variable <- variable
    keep <- climatology[["variable_id"]] == requested_variable
    out <- climatology[
        keep,
        c(keys, "climatology", "n"),
        with = FALSE
    ]
    data.table::setnames(
        out,
        c("climatology", "n"),
        c(
            sprintf("%s_%s", source, metric),
            sprintf("n_%s_%s", source, metric)
        )
    )
    out[]
}

# Convert one source's tas/tasmax/tasmin climatologies into one row per target
# day. The tas grid is authoritative; optional extrema are left-joined to it.
daily__temperature_wide <- function(climatology, by, source) {
    keys <- c(by, "target_day", "annual_phase")
    out <- daily__temperature_metric(
        climatology, "tas", by, source, "mean"
    )
    for (spec in list(
        c(variable_id = "tasmin", metric = "minimum"),
        c(variable_id = "tasmax", metric = "maximum")
    )) {
        metric <- daily__temperature_metric(
            climatology,
            unname(spec[["variable_id"]]),
            by,
            source,
            unname(spec[["metric"]])
        )
        if (nrow(metric)) {
            out <- merge(
                out,
                metric,
                by = keys,
                all.x = TRUE,
                sort = FALSE
            )
        } else {
            value_name <- sprintf("%s_%s", source, spec[["metric"]])
            count_name <- sprintf("n_%s_%s", source, spec[["metric"]])
            data.table::set(out, j = value_name, value = NA_real_)
            data.table::set(out, j = count_name, value = NA_integer_)
        }
    }
    out[]
}

# Calculate daily temperature deltas from future and historical climatologies
# that have already been mapped onto the same calendar-neutral target grid.
daily__temperature_target_changes <- function(
    future_climatology,
    historical_climatology,
    by = character()
) {
    checkmate::assert_character(by, any.missing = FALSE, unique = TRUE)
    future_wide <- daily__temperature_wide(
        future_climatology, by, "future"
    )
    historical_wide <- daily__temperature_wide(
        historical_climatology, by, "historical"
    )

    keys <- c(by, "target_day", "annual_phase")
    targets <- merge(
        future_wide,
        historical_wide,
        by = keys,
        all = TRUE,
        sort = FALSE
    )
    missing_mean <- (
        !is.finite(targets[["future_mean"]]) |
            !is.finite(targets[["historical_mean"]]) |
            targets[["n_future_mean"]] < 1L |
            targets[["n_historical_mean"]] < 1L
    )
    if (any(missing_mean)) {
        cli::cli_abort(
            paste0(
                "Matching future and historical {.val tas} climatologies are ",
                "required for every target day and group."
            )
        )
    }

    extrema_columns <- c(
        "future_minimum", "future_maximum",
        "historical_minimum", "historical_maximum"
    )
    extrema_counts <- c(
        "n_future_minimum", "n_future_maximum",
        "n_historical_minimum", "n_historical_maximum"
    )
    complete_extrema <- Reduce(
        `&`,
        lapply(extrema_columns, function(column) {
            is.finite(targets[[column]])
        })
    ) & Reduce(
        `&`,
        lapply(extrema_counts, function(column) {
            !is.na(targets[[column]]) & targets[[column]] > 0L
        })
    )

    invalid_extrema <- complete_extrema & (
        targets[["future_maximum"]] < targets[["future_minimum"]] |
            targets[["historical_maximum"]] < targets[["historical_minimum"]]
    )
    if (any(invalid_extrema)) {
        cli::cli_abort(
            paste0(
                "Daily temperature extrema must satisfy ",
                "{.val tasmax >= tasmin} for future and historical ",
                "climatologies."
            )
        )
    }

    # Mean temperature always has an additive target; extrema remain explicitly
    # unavailable until all four future/historical extrema series are present.
    data.table::set(
        targets,
        j = "mean_delta",
        value = as.numeric(
            targets[["future_mean"]] - targets[["historical_mean"]]
        )
    )
    data.table::set(
        targets, j = "minimum_delta", value = rep.int(NA_real_, nrow(targets))
    )
    data.table::set(
        targets, j = "maximum_delta", value = rep.int(NA_real_, nrow(targets))
    )
    data.table::set(
        targets, j = "dtr_delta", value = rep.int(NA_real_, nrow(targets))
    )
    data.table::set(
        targets,
        j = "dtr_status",
        value = rep.int("inherited_missing_extremes", nrow(targets))
    )

    # For complete extrema, DTR change is the future-minus-historical
    # difference between their independently estimated daily ranges.
    adjusted_rows <- which(complete_extrema)
    if (length(adjusted_rows)) {
        future_minimum <- targets[["future_minimum"]][adjusted_rows]
        future_maximum <- targets[["future_maximum"]][adjusted_rows]
        historical_minimum <- targets[["historical_minimum"]][adjusted_rows]
        historical_maximum <- targets[["historical_maximum"]][adjusted_rows]
        data.table::set(
            targets,
            i = adjusted_rows,
            j = "minimum_delta",
            value = as.numeric(future_minimum - historical_minimum)
        )
        data.table::set(
            targets,
            i = adjusted_rows,
            j = "maximum_delta",
            value = as.numeric(future_maximum - historical_maximum)
        )
        data.table::set(
            targets,
            i = adjusted_rows,
            j = "dtr_delta",
            value = as.numeric(
                (future_maximum - future_minimum) -
                    (historical_maximum - historical_minimum)
            )
        )
        data.table::set(
            targets,
            i = adjusted_rows,
            j = "dtr_status",
            value = "adjusted"
        )
    }

    ordered <- c(
        by, "target_day", "annual_phase",
        "mean_delta", "minimum_delta", "maximum_delta", "dtr_delta",
        "dtr_status",
        "n_future_mean", "n_historical_mean",
        "n_future_minimum", "n_historical_minimum",
        "n_future_maximum", "n_historical_maximum"
    )
    data.table::setcolorder(targets, ordered)
    data.table::setorderv(targets, c(by, "target_day"))
    targets[]
}

# Build calendar-neutral daily temperature changes from matching future and
# historical climatologies. Missing extrema retain the mean delta explicitly.
daily__temperature_targets <- function(
    future, historical, by = character(), window_days = 31L,
    target_year_days = 365L
) {
    checkmate::assert_character(by, any.missing = FALSE, unique = TRUE)
    future <- daily__temperature_source(future, "future", by)
    historical <- daily__temperature_source(historical, "historical", by)

    future_climatology <- daily__temperature_climatology(
        future, by, window_days, target_year_days
    )
    historical_climatology <- daily__temperature_climatology(
        historical, by, window_days, target_year_days
    )
    daily__temperature_target_changes(
        future_climatology,
        historical_climatology,
        by
    )
}

# Construct a monotone normalized shape with fixed zero/one endpoints and the
# requested mean. A power family retains all ordering and extrema positions.
daily__temperature_shape <- function(normalized, target_mean, tolerance) {
    lower_mean <- mean(normalized == 1)
    upper_mean <- mean(normalized > 0)
    if (
        target_mean < lower_mean - tolerance ||
            target_mean > upper_mean + tolerance
    ) {
        cli::cli_abort(
            paste0(
                "The requested daily mean is infeasible while preserving all ",
                "template minimum and maximum positions."
            )
        )
    }

    # Exact boundary means require a monotone step; interior means use a smooth
    # power curve whose exponent is solved on the log scale.
    if (target_mean <= lower_mean + tolerance) {
        return(list(value = as.numeric(normalized == 1), exponent = Inf))
    }
    if (target_mean >= upper_mean - tolerance) {
        return(list(value = as.numeric(normalized > 0), exponent = 0))
    }
    if (abs(target_mean - mean(normalized)) <= tolerance) {
        return(list(value = normalized, exponent = 1))
    }

    objective <- function(log_exponent) {
        mean(normalized ^ exp(log_exponent)) - target_mean
    }
    log_exponent <- stats::uniroot(
        objective,
        interval = c(-40, 40),
        tol = min(tolerance, 1e-12)
    )$root
    exponent <- exp(log_exponent)
    list(value = normalized ^ exponent, exponent = exponent)
}

# Project one finite hourly temperature vector onto requested daily statistics.
# Explicit shift fallbacks keep missing extrema and flat templates traceable.
daily__project_temperature_day <- function(
    value, mean_delta, minimum_delta, maximum_delta, dtr_status,
    tolerance
) {
    baseline_mean <- mean(value)
    baseline_minimum <- min(value)
    baseline_maximum <- max(value)
    baseline_range <- baseline_maximum - baseline_minimum
    target_mean <- baseline_mean + mean_delta

    if (!identical(dtr_status, "adjusted")) {
        projected <- value + mean_delta
        return(list(
            value = projected,
            target_mean = target_mean,
            target_minimum = baseline_minimum + mean_delta,
            target_maximum = baseline_maximum + mean_delta,
            status = "shift_inherited_dtr",
            exponent = 1
        ))
    }
    if (!is.finite(minimum_delta) || !is.finite(maximum_delta)) {
        cli::cli_abort(
            "Adjusted DTR targets require finite minimum and maximum changes."
        )
    }

    target_minimum <- baseline_minimum + minimum_delta
    target_maximum <- baseline_maximum + maximum_delta
    target_range <- target_maximum - target_minimum
    if (target_range < -tolerance) {
        cli::cli_abort(
            "The requested daily maximum is lower than the requested daily minimum."
        )
    }
    if (
        target_mean < target_minimum - tolerance ||
            target_mean > target_maximum + tolerance
    ) {
        cli::cli_abort(
            "The requested daily mean must lie between the requested minimum and maximum."
        )
    }

    if (baseline_range <= tolerance) {
        projected <- value + mean_delta
        return(list(
            value = projected,
            target_mean = target_mean,
            target_minimum = target_minimum,
            target_maximum = target_maximum,
            status = "fallback_shift_flat_template",
            exponent = 1
        ))
    }
    if (target_range <= tolerance) {
        projected <- rep.int(target_mean, length(value))
        return(list(
            value = projected,
            target_mean = target_mean,
            target_minimum = target_minimum,
            target_maximum = target_maximum,
            status = "projected_collapsed_range",
            exponent = NA_real_
        ))
    }

    normalized <- (value - baseline_minimum) / baseline_range
    normalized_mean <- (target_mean - target_minimum) / target_range
    shape_tolerance <- tolerance / max(target_range, 1)
    shape <- daily__temperature_shape(
        normalized, normalized_mean, shape_tolerance
    )
    projected <- target_minimum + target_range * shape$value

    # Guard the numerical contract before any grouped result reaches a backend.
    closure_error <- max(
        abs(mean(projected) - target_mean),
        abs(min(projected) - target_minimum),
        abs(max(projected) - target_maximum)
    )
    if (!is.finite(closure_error) || closure_error > max(tolerance, 1e-9)) {
        cli::cli_abort(
            "Daily temperature projection failed its mean/minimum/maximum closure check."
        )
    }

    list(
        value = projected,
        target_mean = target_mean,
        target_minimum = target_minimum,
        target_maximum = target_maximum,
        status = "projected",
        exponent = shape$exponent
    )
}

# Return cyclic previous values for annual boundary diagnostics. A single-day
# input has no meaningful adjacent-day boundary and therefore returns NA.
daily__cyclic_previous <- function(value) {
    if (length(value) < 2L) {
        return(rep.int(NA_real_, length(value)))
    }
    c(value[[length(value)]], value[-length(value)])
}

# Apply daily target changes to grouped 24-hour templates and expose closure and
# cyclic boundary diagnostics without mutating the caller's source rows.
daily__project_temperature <- function(
    template, targets, value = "value", day = "target_day", hour = "hour",
    by = character(), tolerance = 1e-8,
    method = c("power", "btws")
) {
    checkmate::assert_data_frame(template)
    checkmate::assert_data_frame(targets)
    checkmate::assert_string(value, min.chars = 1L)
    checkmate::assert_string(day, min.chars = 1L)
    checkmate::assert_string(hour, min.chars = 1L)
    checkmate::assert_character(by, any.missing = FALSE, unique = TRUE)
    checkmate::assert_number(tolerance, lower = 0, finite = TRUE)
    method <- match.arg(method)
    value_column <- value
    day_column <- day
    hour_column <- hour
    reserved_by <- intersect(
        by,
        c(
            value_column, day_column, hour_column,
            "mean_delta", "minimum_delta", "maximum_delta", "dtr_status"
        )
    )
    if (length(reserved_by)) {
        cli::cli_abort(
            "{.arg by} cannot use template, target, or output column{?s}: {.val {reserved_by}}."
        )
    }

    if (!nrow(template) || !nrow(targets)) {
        cli::cli_abort(
            "{.arg template} and {.arg targets} must both contain rows."
        )
    }
    if (anyDuplicated(names(template)) || anyDuplicated(names(targets))) {
        cli::cli_abort(
            "{.arg template} and {.arg targets} must have unique column names."
        )
    }

    template_required <- unique(c(by, day, hour, value))
    target_required <- unique(c(
        by, day, "mean_delta", "minimum_delta", "maximum_delta", "dtr_status"
    ))
    missing_template <- setdiff(template_required, names(template))
    missing_targets <- setdiff(target_required, names(targets))
    if (length(missing_template)) {
        cli::cli_abort(
            "{.arg template} is missing required column{?s}: {.val {missing_template}}."
        )
    }
    if (length(missing_targets)) {
        cli::cli_abort(
            "{.arg targets} is missing required column{?s}: {.val {missing_targets}}."
        )
    }

    method_output_columns <- if (identical(method, "power")) {
        "shape_exponent"
    } else {
        c(
            "btws_scale", "btws_m", "btws_n",
            "btws_fallback_reason"
        )
    }
    output_columns <- c(
        "temperature_projected", "target_mean", "target_minimum",
        "target_maximum", "projected_mean", "projected_minimum",
        "projected_maximum", "dtr_status", "projection_status",
        method_output_columns, "boundary_jump", "boundary_jump_change"
    )
    working_columns <- c(
        "mean_delta", "minimum_delta", "maximum_delta", "dtr_status",
        ".daily_row", ".daily_target_found"
    )
    conflicts <- intersect(
        c(output_columns, working_columns),
        names(template)
    )
    if (length(conflicts)) {
        cli::cli_abort(
            "{.arg template} already contains output column{?s}: {.val {conflicts}}."
        )
    }

    checkmate::assert_numeric(
        template[[value]],
        finite = TRUE,
        any.missing = FALSE,
        .var.name = sprintf("template[['%s']]", value)
    )
    checkmate::assert_integerish(
        template[[day]],
        lower = 1L,
        any.missing = FALSE,
        .var.name = sprintf("template[['%s']]", day)
    )
    checkmate::assert_numeric(
        template[[hour]],
        finite = TRUE,
        any.missing = FALSE,
        .var.name = sprintf("template[['%s']]", hour)
    )
    checkmate::assert_numeric(
        targets[["mean_delta"]],
        finite = TRUE,
        any.missing = FALSE,
        .var.name = "targets[['mean_delta']]"
    )
    checkmate::assert_numeric(
        targets[["minimum_delta"]],
        finite = TRUE,
        any.missing = TRUE,
        .var.name = "targets[['minimum_delta']]"
    )
    checkmate::assert_numeric(
        targets[["maximum_delta"]],
        finite = TRUE,
        any.missing = TRUE,
        .var.name = "targets[['maximum_delta']]"
    )
    checkmate::assert_integerish(
        targets[[day_column]],
        lower = 1L,
        any.missing = FALSE,
        .var.name = sprintf("targets[['%s']]", day_column)
    )
    checkmate::assert_character(
        targets[["dtr_status"]],
        any.missing = FALSE,
        .var.name = "targets[['dtr_status']]"
    )
    unknown_status <- setdiff(
        unique(targets[["dtr_status"]]),
        c("adjusted", "inherited_missing_extremes")
    )
    if (length(unknown_status)) {
        cli::cli_abort(
            "Unknown daily temperature DTR status value{?s}: {.val {unknown_status}}."
        )
    }

    keys <- c(by, day)
    target_work <- data.table::as.data.table(data.table::copy(targets))
    if (anyDuplicated(target_work[, keys, with = FALSE])) {
        cli::cli_abort(
            "{.arg targets} must contain one row per requested day and group."
        )
    }
    data.table::set(
        target_work,
        j = ".daily_target_found",
        value = rep.int(TRUE, nrow(target_work))
    )

    working <- data.table::as.data.table(data.table::copy(template))
    data.table::set(
        working, j = ".daily_row", value = seq_len(nrow(working))
    )

    # A left merge makes target coverage explicit while the private row index
    # preserves the caller's original order through all grouped calculations.
    target_join_columns <- c(
        keys, "mean_delta", "minimum_delta", "maximum_delta", "dtr_status",
        ".daily_target_found"
    )
    working <- merge(
        working,
        target_work[, target_join_columns, with = FALSE],
        by = keys,
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setorderv(working, ".daily_row")
    if (any(!working[[".daily_target_found"]] %in% TRUE)) {
        cli::cli_abort(
            "{.arg targets} does not cover every template day and group."
        )
    }

    group_columns <- c(by, day)
    template_shape <- working[, .(
        rows = .N,
        unique_hours = data.table::uniqueN(get(hour_column))
    ), by = group_columns]
    if (any(template_shape$rows != 24L | template_shape$unique_hours != 24L)) {
        cli::cli_abort(
            "Each template day and group must contain exactly 24 unique hourly rows."
        )
    }

    projection_input_columns <- c(
        value_column, ".daily_row", "mean_delta", "minimum_delta",
        "maximum_delta", "dtr_status"
    )
    projected <- working[, {
        # Both projectors exchange the same core daily statistics. Method-
        # specific numerical parameters are appended without changing the
        # established power-projection output schema.
        projector <- if (identical(method, "power")) {
            daily__project_temperature_day
        } else {
            btws__project_temperature_day
        }
        result <- projector(
            value = as.numeric(.SD[[value_column]]),
            mean_delta = unique(.SD[["mean_delta"]]),
            minimum_delta = unique(.SD[["minimum_delta"]]),
            maximum_delta = unique(.SD[["maximum_delta"]]),
            dtr_status = unique(.SD[["dtr_status"]]),
            tolerance = tolerance
        )
        row <- list(
            .daily_row = .SD[[".daily_row"]],
            temperature_projected = result$value,
            target_mean = result$target_mean,
            target_minimum = result$target_minimum,
            target_maximum = result$target_maximum,
            projected_mean = mean(result$value),
            projected_minimum = min(result$value),
            projected_maximum = max(result$value),
            dtr_status = unique(.SD[["dtr_status"]]),
            projection_status = result$status
        )
        if (identical(method, "power")) {
            row$shape_exponent <- result$exponent
        } else {
            row$btws_scale <- result$scale
            row$btws_m <- result$m
            row$btws_n <- result$n
            row$btws_fallback_reason <- result$fallback_reason
        }
        row
    }, by = group_columns, .SDcols = projection_input_columns]

    original_columns <- names(template)
    out <- data.table::as.data.table(data.table::copy(template))
    data.table::set(out, j = ".daily_row", value = seq_len(nrow(out)))
    projection_output_columns <- c(
        ".daily_row", "temperature_projected", "target_mean",
        "target_minimum", "target_maximum", "projected_mean",
        "projected_minimum", "projected_maximum", "dtr_status",
        "projection_status", method_output_columns
    )
    out <- merge(
        out,
        projected[, projection_output_columns, with = FALSE],
        by = ".daily_row",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setorderv(out, ".daily_row")

    # Measure each boundary at the first chronological hour of a target day.
    chronological <- out[
        ,
        .SD,
        .SDcols = c(
            by, day_column, hour_column, value_column,
            "temperature_projected"
        )
    ]
    data.table::setorderv(
        chronological,
        c(by, day_column, hour_column)
    )
    boundary <- chronological[, {
        daily_hour <- .SD[[hour_column]]
        source_temperature <- .SD[[value_column]]
        projected_temperature <- .SD[["temperature_projected"]]
        list(
            source_first = source_temperature[which.min(daily_hour)],
            source_last = source_temperature[which.max(daily_hour)],
            projected_first = projected_temperature[which.min(daily_hour)],
            projected_last = projected_temperature[which.max(daily_hour)]
        )
    }, by = group_columns, .SDcols = c(
        hour_column, value_column, "temperature_projected"
    )]
    data.table::setorderv(boundary, group_columns)

    previous_columns <- c(
        "previous_source_last", "previous_projected_last"
    )
    if (length(by)) {
        boundary[, (previous_columns) := list(
            daily__cyclic_previous(.SD[["source_last"]]),
            daily__cyclic_previous(.SD[["projected_last"]])
        ), by = by, .SDcols = c("source_last", "projected_last")]
    } else {
        data.table::set(
            boundary,
            j = "previous_source_last",
            value = daily__cyclic_previous(boundary[["source_last"]])
        )
        data.table::set(
            boundary,
            j = "previous_projected_last",
            value = daily__cyclic_previous(boundary[["projected_last"]])
        )
    }
    projected_jump <- abs(
        boundary[["projected_first"]] -
            boundary[["previous_projected_last"]]
    )
    source_jump <- abs(
        boundary[["source_first"]] - boundary[["previous_source_last"]]
    )
    data.table::set(
        boundary, j = "boundary_jump", value = projected_jump
    )
    data.table::set(
        boundary,
        j = "boundary_jump_change",
        value = projected_jump - source_jump
    )
    boundary_output_columns <- c(
        keys, "boundary_jump", "boundary_jump_change"
    )
    out <- merge(
        out,
        boundary[, boundary_output_columns, with = FALSE],
        by = keys,
        all.x = TRUE,
        sort = FALSE
    )

    data.table::setorderv(out, ".daily_row")
    data.table::set(out, j = ".daily_row", value = NULL)
    data.table::setcolorder(out, c(
        original_columns,
        setdiff(names(out), original_columns)
    ))
    out[]
}

# }}}
