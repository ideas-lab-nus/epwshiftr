#' @include temporal-interpolation.R solar-radiation-interpolation.R
NULL

# Model variables accepted by the composite hourly weather preprocessing
# component. Daily temperature extrema are consumed only as interpolation anchors.
HOURLY_WEATHER_TARGET_VARIABLES <- c(
    TEMPORAL_LINEAR_VARIABLES,
    SOLAR_RADIATION_VARIABLES
)

HOURLY_WEATHER_EXTREMA_VARIABLES <- c(
    "tasmin",
    "tasmax"
)

HOURLY_WEATHER_REFERENCES <- c(
    "https://doi.org/10.1038/s41467-023-41458-5"
)

# Copy a variable family into a new WeatherInput while retaining only metadata
# grouping fields that still exist in the materialized subset.
weather_interp__subset_input <- function(input, role, variables) {
    data <- data.table::as.data.table(data.table::copy(input@source))
    data <- data[get("variable_id") %in% variables]
    if (!nrow(data)) {
        return(NULL)
    }
    metadata <- input@metadata
    if (!is.null(metadata$group_columns)) {
        metadata$group_columns <- intersect(
            metadata$group_columns,
            names(data)
        )
    }
    weather__new_input(
        role,
        as.data.frame(data, stringsAsFactors = FALSE),
        representation = "series",
        variables = unique(as.character(data[["variable_id"]])),
        frequencies = unique(as.character(data[["frequency"]])),
        calendars = unique(as.character(data[["cf_calendar"]])),
        provenance = input@provenance,
        metadata = metadata
    )
}

# Validate one model role before its point-state, radiation, and daily-extrema
# rows are dispatched to algorithms with different temporal semantics.
weather_interp__model_source <- function(input, role) {
    if (!S7::S7_inherits(input, WeatherInput) ||
        !identical(input@representation, "series") ||
        !is.data.frame(input@source)) {
        cli::cli_abort(
            "Role {.val {role}} must contain a materialized series WeatherInput."
        )
    }
    data <- data.table::as.data.table(data.table::copy(input@source))
    required <- c(BIAS_ADJUSTED_SERIES_COLUMNS, "time")
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "Role {.val {role}} is missing hourly-weather column(s): {.val {missing}}."
        )
    }
    if (!nrow(data)) {
        cli::cli_abort("Role {.val {role}} must contain model samples.")
    }
    variables <- unique(as.character(data[["variable_id"]]))
    unsupported <- setdiff(
        variables,
        c(HOURLY_WEATHER_TARGET_VARIABLES, HOURLY_WEATHER_EXTREMA_VARIABLES)
    )
    if (length(unsupported)) {
        cli::cli_abort(
            "Role {.val {role}} contains unsupported hourly-weather variable(s): {.val {unsupported}}."
        )
    }
    targets <- intersect(variables, HOURLY_WEATHER_TARGET_VARIABLES)
    if (!length(targets)) {
        cli::cli_abort(
            "Role {.val {role}} must contain at least one supported hourly target variable."
        )
    }

    extrema <- intersect(variables, HOURLY_WEATHER_EXTREMA_VARIABLES)
    if (length(extrema) == 1L) {
        cli::cli_abort(
            "Role {.val {role}} must provide `tasmin` and `tasmax` together."
        )
    }
    if (length(extrema) && !"tas" %in% targets) {
        cli::cli_abort(
            "Role {.val {role}} cannot use daily extrema without three-hourly `tas`."
        )
    }

    frequency_by_variable <- split(
        as.character(data[["frequency"]]),
        as.character(data[["variable_id"]])
    )
    frequency_by_variable <- lapply(frequency_by_variable, unique)
    for (variable in intersect(targets, TEMPORAL_LINEAR_VARIABLES)) {
        allowed <- names(TEMPORAL_SOURCE_STEPS)
        if (!all(frequency_by_variable[[variable]] %in% allowed)) {
            cli::cli_abort(
                "Role {.val {role}} variable {.val {variable}} must use frequency {.val {allowed}}."
            )
        }
    }
    for (variable in intersect(targets, SOLAR_RADIATION_VARIABLES)) {
        allowed <- names(TEMPORAL_SOURCE_STEPS)
        if (!all(frequency_by_variable[[variable]] %in% allowed)) {
            cli::cli_abort(
                "Role {.val {role}} variable {.val {variable}} must use frequency {.val {allowed}}."
            )
        }
    }
    for (variable in extrema) {
        if (!identical(frequency_by_variable[[variable]], "day")) {
            cli::cli_abort(
                "Role {.val {role}} variable {.val {variable}} must use daily frequency."
            )
        }
    }
    if (length(extrema) &&
        !identical(frequency_by_variable[["tas"]], "3hr")) {
        cli::cli_abort(
            "Role {.val {role}} requires three-hourly `tas` when daily extrema anchors are supplied."
        )
    }

    data.table::set(
        data,
        j = ".weather_source_row",
        value = seq_len(nrow(data))
    )
    list(
        data = data,
        targets = sort(targets),
        has_extrema = length(extrema) == 2L
    )
}

# Return the most frequent observed extreme hour with an earliest-hour tie
# break so the result is stable across platforms and input row order.
weather_interp__mode_second <- function(seconds) {
    counts <- table(as.numeric(seconds))
    as.numeric(names(counts)[counts == max(counts)])[[1L]]
}

# Learn site- and month-specific modal maximum/minimum hours from the hourly
# observed-reference temperature without pairing its dates to model calendars.
weather_interp__observed_modes <- function(input) {
    if (!S7::S7_inherits(input, WeatherInput) ||
        !identical(input@representation, "series") ||
        !is.data.frame(input@source)) {
        cli::cli_abort(
            "Role `observed_reference` must contain a materialized hourly series WeatherInput."
        )
    }
    data <- data.table::as.data.table(data.table::copy(input@source))
    required <- BIAS_ADJUSTED_SERIES_COLUMNS
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "Role `observed_reference` is missing hourly-weather column(s): {.val {missing}}."
        )
    }
    if (any(as.character(data[["frequency"]]) != "hour")) {
        cli::cli_abort(
            "Role `observed_reference` must contain only hourly target variables."
        )
    }
    data.table::set(
        data,
        j = "cf_second_of_day",
        value = temporal__second_of_day(data, "observed_reference")
    )
    data.table::set(
        data,
        j = ".weather_site_id",
        value = if ("site_id" %in% names(data)) {
            as.character(data[["site_id"]])
        } else {
            rep.int("<implicit>", nrow(data))
        }
    )

    tas <- data[get("variable_id") == "tas"]
    if (!nrow(tas)) {
        return(data.table::data.table(
            site_id = character(),
            cf_month = integer(),
            extreme = character(),
            cf_second_of_day = numeric()
        ))
    }
    sites <- split(tas, by = ".weather_site_id", keep.by = TRUE)
    daily <- lapply(sites, function(site) {
        canonical <- data.table::as.data.table(bias__subdaily_table(
            as.data.frame(site, stringsAsFactors = FALSE),
            frequency = "hour",
            time_step_seconds = 3600,
            name = "observed_reference tas"
        ))
        by <- c(
            ".weather_site_id",
            "cf_calendar",
            "cf_year",
            "cf_month",
            "cf_day"
        )
        maximum <- canonical[
            order(get("cf_second_of_day")),
            .SD[which.max(get("value"))],
            by = by
        ]
        minimum <- canonical[
            order(get("cf_second_of_day")),
            .SD[which.min(get("value"))],
            by = by
        ]
        data.table::set(
            maximum,
            j = "extreme",
            value = rep.int("tasmax", nrow(maximum))
        )
        data.table::set(
            minimum,
            j = "extreme",
            value = rep.int("tasmin", nrow(minimum))
        )
        data.table::rbindlist(list(maximum, minimum), use.names = TRUE)
    })
    daily <- data.table::rbindlist(daily, use.names = TRUE, fill = TRUE)
    modes <- daily[, .(
        cf_second_of_day = weather_interp__mode_second(
            get("cf_second_of_day")
        )
    ), by = c(".weather_site_id", "cf_month", "extreme")]
    data.table::setnames(modes, ".weather_site_id", "site_id")
    modes[]
}

# Match daily extrema to one independently interpolated model-temperature
# group without treating table or variable identifiers as shared identity.
weather_interp__matching_extrema <- function(extrema, group, role) {
    identity_columns <- intersect(
        setdiff(
            TEMPORAL_ID_COLUMNS,
            c("variable_id", "units", "frequency", "table_id")
        ),
        intersect(names(extrema), names(group))
    )
    matched <- data.table::copy(extrema)
    for (column in identity_columns) {
        values <- unique(group[[column]])
        if (length(values) != 1L) {
            cli::cli_abort(
                "Temperature group in role {.val {role}} has non-scalar identity field {.field {column}}."
            )
        }
        if (is.na(values[[1L]])) {
            matched <- matched[is.na(get(column))]
        } else {
            matched <- matched[get(column) == values[[1L]]]
        }
    }
    if (!nrow(matched)) {
        cli::cli_abort(
            "Role {.val {role}} has no daily extrema matching one three-hourly `tas` group."
        )
    }
    data.table::as.data.table(bias__daily_table(
        as.data.frame(matched, stringsAsFactors = FALSE)
    ))
}

# Select the adjacent three-hourly pair that best represents one daily high or
# low while reporting when the two unconstrained extreme samples are not adjacent.
weather_interp__extreme_pair <- function(day, extreme) {
    data.table::setorderv(day, "cf_second_of_day")
    if (nrow(day) < 2L) {
        cli::cli_abort(
            "Every anchored `tas` day must contain at least two three-hourly samples."
        )
    }
    left <- seq_len(nrow(day) - 1L)
    right <- left + 1L
    score <- (as.numeric(day[["value"]][left]) +
        as.numeric(day[["value"]][right])) / 2
    selected <- if (identical(extreme, "tasmax")) {
        which.max(score)
    } else {
        which.min(score)
    }
    value_order <- order(
        as.numeric(day[["value"]]),
        decreasing = identical(extreme, "tasmax")
    )
    published_pair <- setequal(
        sort(value_order[seq_len(2L)]),
        c(left[[selected]], right[[selected]])
    )
    list(
        left = left[[selected]],
        right = right[[selected]],
        policy = if (published_pair) {
            "two_extreme_samples"
        } else {
            "best_adjacent_pair"
        }
    )
}

# Choose one interior hourly anchor using the observed modal extreme hour when
# available and an earliest-interior deterministic fallback otherwise.
weather_interp__anchor_second <- function(
    left_second,
    right_second,
    modes,
    site,
    month_value,
    extreme_id
) {
    candidates <- seq.int(
        left_second + 3600,
        right_second - 3600,
        by = 3600
    )
    observed <- modes[
        get("site_id") == site &
            get("cf_month") == month_value &
            get("extreme") == extreme_id
    ][["cf_second_of_day"]]
    if (length(observed)) {
        distance <- abs(candidates - observed[[1L]])
        return(list(
            second = candidates[[which.min(distance)]],
            policy = "observed_monthly_mode"
        ))
    }
    list(
        second = candidates[[1L]],
        policy = "earliest_interior_hour"
    )
}

# Build daily minimum and maximum support points for one regular three-hourly
# `tas` group, preserving the exact source row and selection policy.
weather_interp__anchors <- function(
    group,
    extrema,
    modes,
    role
) {
    if (!identical(unique(as.character(group[["frequency"]])), "3hr") ||
        !identical(unique(as.character(group[["variable_id"]])), "tas")) {
        return(NULL)
    }
    matched <- weather_interp__matching_extrema(extrema, group, role)
    units <- unique(c(
        as.character(group[["units"]]),
        as.character(matched[["units"]])
    ))
    if (length(units) != 1L) {
        cli::cli_abort(
            "Role {.val {role}} requires matching units for `tas`, `tasmin`, and `tasmax`."
        )
    }
    site_id <- if ("site_id" %in% names(group)) {
        as.character(group[["site_id"]][[1L]])
    } else {
        "<implicit>"
    }
    day_columns <- c(
        "cf_calendar",
        "cf_year",
        "cf_month",
        "cf_day"
    )
    days <- split(group, by = day_columns, keep.by = TRUE, drop = TRUE)
    anchors <- lapply(days, function(day) {
        # A single padding sample may bracket a requested period boundary but
        # cannot define a within-day extreme insertion interval.
        if (nrow(day) < 2L) {
            return(NULL)
        }
        rows <- matched[
            get("cf_calendar") == day[["cf_calendar"]][[1L]] &
                get("cf_year") == day[["cf_year"]][[1L]] &
                get("cf_month") == day[["cf_month"]][[1L]] &
                get("cf_day") == day[["cf_day"]][[1L]]
        ]
        if (!identical(
            sort(as.character(rows[["variable_id"]])),
            sort(HOURLY_WEATHER_EXTREMA_VARIABLES)
        )) {
            cli::cli_abort(
                "Role {.val {role}} requires one matching `tasmin` and `tasmax` value for every anchored `tas` day."
            )
        }
        minimum <- rows[get("variable_id") == "tasmin"]
        maximum <- rows[get("variable_id") == "tasmax"]
        if (minimum[["value"]][[1L]] > maximum[["value"]][[1L]]) {
            cli::cli_abort(
                "Role {.val {role}} contains daily `tasmin` above `tasmax`."
            )
        }

        day_anchors <- lapply(HOURLY_WEATHER_EXTREMA_VARIABLES, function(
            extreme
        ) {
            row <- rows[get("variable_id") == extreme]
            pair <- weather_interp__extreme_pair(day, extreme)
            left <- day[pair$left]
            right <- day[pair$right]
            anchor_value <- as.numeric(row[["value"]][[1L]])
            endpoint_values <- c(left[["value"]], right[["value"]])
            consistent <- if (identical(extreme, "tasmax")) {
                anchor_value >= max(endpoint_values)
            } else {
                anchor_value <= min(endpoint_values)
            }
            if (!consistent) {
                cli::cli_abort(
                    "Role {.val {role}} daily {.val {extreme}} is inconsistent with its selected three-hourly `tas` bracket."
                )
            }
            selected <- weather_interp__anchor_second(
                as.numeric(left[["cf_second_of_day"]][[1L]]),
                as.numeric(right[["cf_second_of_day"]][[1L]]),
                modes,
                site_id,
                as.integer(day[["cf_month"]][[1L]]),
                extreme
            )
            coordinate <- as.data.frame(row, stringsAsFactors = FALSE)
            coordinate[["cf_second_of_day"]] <- selected$second
            native_second <- temporal__native_seconds(
                coordinate,
                coordinate[["cf_calendar"]][[1L]]
            )
            label <- paste0(
                temporal__cf_time_label(coordinate),
                "/",
                extreme
            )
            data.table::data.table(
                native_second = native_second,
                value = anchor_value,
                source_time = label,
                source_row = as.integer(row[[".weather_source_row"]][[1L]]),
                source_kind = paste0("model_daily_", extreme),
                extreme = extreme,
                hour_policy = selected$policy,
                pair_policy = pair$policy
            )
        })
        day_anchors <- data.table::rbindlist(day_anchors)
        if (anyDuplicated(day_anchors[["native_second"]])) {
            cli::cli_abort(
                "Role {.val {role}} selected the same hourly position for daily minimum and maximum anchors."
            )
        }
        day_anchors
    })
    anchors <- anchors[lengths(anchors) > 0L]
    if (!length(anchors)) {
        return(NULL)
    }
    data.table::rbindlist(anchors, use.names = TRUE, fill = TRUE)
}

# Interpolate one point-state family and optionally inject paired daily
# temperature extrema into each matching three-hourly `tas` group.
weather_interp__state_role <- function(
    input,
    source,
    role,
    context,
    modes
) {
    variables <- intersect(source$targets, TEMPORAL_LINEAR_VARIABLES)
    state_input <- weather_interp__subset_input(input, role, variables)
    if (is.null(state_input)) {
        return(NULL)
    }
    extrema <- if (source$has_extrema) {
        source$data[
            get("variable_id") %in% HOURLY_WEATHER_EXTREMA_VARIABLES
        ]
    } else {
        NULL
    }
    anchor_factory <- if (is.null(extrema)) {
        NULL
    } else {
        function(group, group_columns) {
            weather_interp__anchors(group, extrema, modes, role)
        }
    }
    temporal__linear_role(
        state_input,
        role,
        context,
        anchor_factory = anchor_factory
    )
}

# Merge hourly variable families back into one role descriptor without
# discarding their family-specific row provenance or interval columns.
weather_interp__combine_role <- function(input, role, pieces) {
    pieces <- pieces[lengths(pieces) > 0L]
    data <- data.table::rbindlist(
        lapply(pieces, function(piece) piece$input@source),
        use.names = TRUE,
        fill = TRUE
    )
    order_columns <- c(
        intersect(TEMPORAL_ID_COLUMNS, names(data)),
        "cf_calendar",
        "cf_year",
        "cf_day_of_year",
        "cf_second_of_day"
    )
    data.table::setorderv(data, unique(order_columns))
    group_columns <- lapply(pieces, function(piece) {
        piece$input@metadata$group_columns
    })
    group_columns <- group_columns[lengths(group_columns) > 0L]
    common_groups <- if (length(group_columns)) {
        Reduce(intersect, group_columns)
    } else {
        character()
    }
    weather__new_input(
        role,
        as.data.frame(data, stringsAsFactors = FALSE),
        representation = "series",
        variables = unique(as.character(data[["variable_id"]])),
        frequencies = "hour",
        calendars = unique(as.character(data[["cf_calendar"]])),
        provenance = utils::modifyList(
            input@provenance,
            list(hourly_weather_interpolation = lapply(
                pieces,
                function(piece) piece$provenance
            ))
        ),
        metadata = utils::modifyList(
            input@metadata,
            list(
                group_columns = common_groups,
                time_step_seconds = 3600,
                variable_specific_temporal_semantics = TRUE
            )
        )
    )
}

# Validate every merged variable/model/site series against the same native-
# calendar hourly contract while retaining its point or interval sampling phase.
weather_interp__hourly_coordinates <- function(input, role) {
    data <- data.table::as.data.table(data.table::copy(input@source))
    if (any(as.character(data[["frequency"]]) != "hour")) {
        cli::cli_abort(
            "Merged role {.val {role}} must contain only hourly variables."
        )
    }
    data.table::set(
        data,
        j = "cf_second_of_day",
        value = temporal__second_of_day(data, role)
    )
    group_columns <- unique(c(
        intersect(TEMPORAL_ID_COLUMNS, names(data)),
        "variable_id",
        "units",
        "cf_calendar"
    ))
    groups <- base::split(
        data,
        by = group_columns,
        keep.by = TRUE,
        drop = TRUE
    )
    diagnostics <- lapply(groups, function(group) {
        label <- temporal__group_label(group, group_columns)
        canonical <- bias__subdaily_table(
            as.data.frame(group, stringsAsFactors = FALSE),
            frequency = "hour",
            time_step_seconds = 3600,
            name = paste(role, label, sep = "/")
        )
        native_seconds <- temporal__native_seconds(
            canonical,
            canonical[["cf_calendar"]][[1L]]
        )
        native_seconds <- sort(native_seconds)
        if (length(native_seconds) > 1L &&
            any(abs(diff(native_seconds) - 3600) > 1e-6)) {
            cli::cli_abort(
                "Merged hourly series {.val {paste(role, label, sep = '/')}} contains a native-calendar gap or overlap."
            )
        }
        phases <- unique(as.numeric(
            canonical[["cf_second_of_day"]] %% 3600
        ))
        if (length(phases) != 1L) {
            cli::cli_abort(
                "Merged hourly series {.val {paste(role, label, sep = '/')}} changes sampling phase within the series."
            )
        }
        data.table::data.table(
            role = role,
            group = label,
            variable_id = canonical[["variable_id"]][[1L]],
            units = canonical[["units"]][[1L]],
            cf_calendar = canonical[["cf_calendar"]][[1L]],
            hour_phase_seconds = phases[[1L]],
            samples = nrow(canonical),
            first_native_second = native_seconds[[1L]],
            last_native_second = native_seconds[[length(native_seconds)]]
        )
    })
    data.table::rbindlist(diagnostics, use.names = TRUE, fill = TRUE)
}

# Apply variable-specific temporal algorithms through one preprocess stage and
# return the common hourly role-input contract required by calendar grouping.
weather_interp__apply <- function(inputs, context, options) {
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    checkmate::assert_list(options, names = "unique")
    if (length(options)) {
        cli::cli_abort(
            "`hourly_weather_interpolation` does not accept component options."
        )
    }
    observed <- weather__get_input(inputs, "observed_reference")
    modes <- weather_interp__observed_modes(observed)
    roles <- c("model_historical", "model_future")
    sources <- lapply(roles, function(role) {
        weather_interp__model_source(
            weather__get_input(inputs, role),
            role
        )
    })
    names(sources) <- roles
    if (!identical(sources$model_historical$targets,
        sources$model_future$targets)) {
        cli::cli_abort(
            "Historical and future model roles must contain identical hourly target variable sets."
        )
    }
    if (!identical(sources$model_historical$has_extrema,
        sources$model_future$has_extrema)) {
        cli::cli_abort(
            "Historical and future model roles must provide daily extrema anchors consistently."
        )
    }
    observed_variables <- unique(as.character(
        observed@source[["variable_id"]]
    ))
    missing_observed <- setdiff(
        sources$model_future$targets,
        observed_variables
    )
    if (length(missing_observed)) {
        cli::cli_abort(
            "Role `observed_reference` is missing hourly target variable(s): {.val {missing_observed}}."
        )
    }
    observed_output <- weather_interp__subset_input(
        observed,
        "observed_reference",
        sources$model_future$targets
    )

    results <- lapply(roles, function(role) {
        input <- weather__get_input(inputs, role)
        source <- sources[[role]]
        state <- weather_interp__state_role(
            input,
            source,
            role,
            context,
            modes
        )
        radiation_input <- weather_interp__subset_input(
            input,
            role,
            intersect(source$targets, SOLAR_RADIATION_VARIABLES)
        )
        radiation <- if (is.null(radiation_input)) {
            NULL
        } else {
            solar__role(radiation_input, role, context)
        }
        pieces <- Filter(Negate(is.null), list(
            point_state = state,
            solar_radiation = radiation
        ))
        list(
            input = weather_interp__combine_role(input, role, pieces),
            pieces = pieces
        )
    })
    names(results) <- roles
    diagnostics <- list()
    for (role in roles) {
        for (family in names(results[[role]]$pieces)) {
            table <- data.table::copy(
                results[[role]]$pieces[[family]]$diagnostics
            )
            data.table::set(table, j = "family", value = family)
            diagnostics[[length(diagnostics) + 1L]] <- table
        }
    }
    diagnostic_table <- data.table::rbindlist(
        diagnostics,
        use.names = TRUE,
        fill = TRUE
    )
    output <- weather__new_inputs(
        weather_template = weather__get_input(inputs, "weather_template"),
        observed_reference = observed_output,
        model_historical = results$model_historical$input,
        model_future = results$model_future$input
    )
    coordinate_diagnostics <- data.table::rbindlist(
        lapply(
            c("observed_reference", roles),
            function(role) {
                weather_interp__hourly_coordinates(
                    weather__get_input(output, role),
                    role
                )
            }
        ),
        use.names = TRUE,
        fill = TRUE
    )
    WeatherStageResult(
        stage = "preprocess",
        component = "hourly_weather_interpolation",
        kind = "hourly_role_inputs",
        value = output,
        diagnostics = list(
            hourly_weather_interpolation = diagnostic_table,
            hourly_weather_coordinates = coordinate_diagnostics
        ),
        provenance = list(
            method = "hourly_weather_interpolation",
            references = HOURLY_WEATHER_REFERENCES,
            roles = roles,
            variables = sources$model_future$targets,
            point_state_method = "linear_temporal_interpolation",
            radiation_method = "solar_radiation_interpolation",
            daily_extrema_anchors = sources$model_future$has_extrema,
            observed_extreme_hour_policy = "site_month_mode",
            coordinate_policy = "regular_hourly_native_calendar_per_series",
            cross_variable_phase_policy = "retain_temporal_semantics",
            target_frequency = "hour"
        ),
        metadata = list(
            variable_dispatch = TRUE,
            daily_extrema_are_auxiliary = TRUE
        )
    )
}

# Describe the composite hourly-weather preprocessing boundary used when one
# climate source contains variables with point and interval-mean semantics.
weather_interp__component <- function() {
    target_sets <- lapply(HOURLY_WEATHER_TARGET_VARIABLES, identity)
    model_requirement <- function(role) {
        component__input_requirement(
            role,
            representations = "series",
            frequencies = c(names(TEMPORAL_SOURCE_STEPS), "day"),
            calendars = CF_TIME_CALENDARS,
            variable_sets = target_sets
        )
    }
    component__spec(
        name = "hourly_weather_interpolation",
        stage = "preprocess",
        label = "Hourly weather interpolation",
        required_inputs = list(
            observed_reference = component__input_requirement(
                "observed_reference",
                representations = "series",
                frequencies = "hour",
                calendars = CF_TIME_CALENDARS,
                variable_sets = target_sets
            ),
            model_historical = model_requirement("model_historical"),
            model_future = model_requirement("model_future")
        ),
        input_kinds = "role_inputs",
        output_kinds = "hourly_role_inputs",
        scopes = "multivariate",
        stochastic = FALSE,
        operations = list(apply = weather_interp__apply),
        metadata = list(
            algorithm = "variable_specific_hourly_interpolation",
            references = HOURLY_WEATHER_REFERENCES,
            target_frequency = "hour",
            supported_variables = HOURLY_WEATHER_TARGET_VARIABLES,
            auxiliary_variables = HOURLY_WEATHER_EXTREMA_VARIABLES,
            point_state_method = "piecewise_linear_interpolation",
            radiation_method = "solar_projection_interval_allocation",
            daily_extrema_policy = "hourly_anchor",
            observed_extreme_hour_policy = "site_month_mode",
            coordinate_policy = "regular_hourly_native_calendar_per_series",
            cross_variable_phase_policy = "retain_temporal_semantics"
        )
    )
}

# Register the composite preprocessing implementation once for complete
# recipes that require state, radiation, and optional extrema inputs together.
weather_interp__register_component <- function() {
    component <- weather_interp__component()
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
