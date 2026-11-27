#' @include weather-sequence.R epw-file.R epw-morph-context.R epw-physics.R
NULL

# Climate variables sampled at an instant use circular interpolation when a
# native CF year has to be represented on the fixed 365-day EPW lattice.
HOURMAP_POINT_VARIABLES <- c(
    "tas",
    "huss",
    "hurs",
    "ps",
    "psl",
    "uas",
    "vas",
    "sfcWind"
)

# Hourly radiation values represent interval means, so calendar conversion
# must conserve the normalized annual mean instead of interpolating samples.
HOURMAP_INTERVAL_MEAN_VARIABLES <- c(
    "rlds",
    "rsds",
    "rsdsdiff"
)

HOURMAP_TARGET_DAYS <- 365L
HOURMAP_TARGET_HOURS <- HOURMAP_TARGET_DAYS * 24L

HOURMAP_REQUIRED_COLUMNS <- c(
    "epw_row",
    "epw_day",
    "year",
    "month",
    "day",
    "hour",
    "minute",
    "target_annual_phase",
    "variable_id",
    "value",
    "units",
    "mapping_method",
    "source_calendar",
    "source_year",
    "source_second_of_day",
    "source_hour_phase_seconds"
)

# Validate one mapped group independently of the physical variables that a
# later stage will derive from its climate-series values.
# hourmap__series_error {{{
hourmap__series_error <- function(self) {
    error <- sequence__first_error(
        sequence__validate_group_series(
            self@group_id,
            self@key,
            self@variables
        ),
        if (!is.data.frame(self@data) || !nrow(self@data)) {
            "`data` must be a non-empty mapped hourly table."
        },
        sequence__provenance_error(self@provenance)
    )
    if (!is.null(error)) {
        return(error)
    }
    missing <- setdiff(HOURMAP_REQUIRED_COLUMNS, names(self@data))
    if (length(missing)) {
        return(sprintf(
            "`data` is missing mapped hourly column(s): %s.",
            paste(sprintf("`%s`", missing), collapse = ", ")
        ))
    }
    error <- sequence__first_error(
        if (
            !setequal(
                self@variables,
                unique(as.character(self@data[["variable_id"]]))
            )
        ) {
            "`variables` must match the mapped hourly table."
        },
        if (
            !is.numeric(self@data[["value"]]) ||
                any(!is.finite(self@data[["value"]]))
        ) {
            "Mapped hourly `value` must contain only finite numbers."
        },
        if (nrow(self@data) != HOURMAP_TARGET_HOURS * length(self@variables)) {
            "Every mapped variable must contain exactly 8760 EPW rows."
        },
        if (
            !is.data.frame(self@diagnostics) ||
                nrow(self@diagnostics) != length(self@variables)
        ) {
            "`diagnostics` must contain one row per mapped variable."
        }
    )
    if (!is.null(error)) {
        return(error)
    }
    for (variable in self@variables) {
        rows <- self@data[
            self@data[["variable_id"]] == variable,
            ,
            drop = FALSE
        ]
        if (
            !identical(
                as.integer(rows[["epw_row"]]),
                seq_len(HOURMAP_TARGET_HOURS)
            )
        ) {
            return(sprintf(
                "Mapped variable `%s` must use ordered EPW rows 1 through 8760.",
                variable
            ))
        }
    }
    NULL
}
# }}}

# MappedHourlyClimateSeries retains one signal group after its native source
# year has been translated onto the target EPW hour rows.
# MappedHourlyClimateSeries {{{
MappedHourlyClimateSeries <- S7::new_class(
    "MappedHourlyClimateSeries",
    properties = list(
        group_id = S7::new_property(S7::class_character),
        key = S7::new_property(S7::class_list, default = list()),
        variables = S7::new_property(S7::class_character),
        data = S7::new_property(S7::class_any),
        diagnostics = S7::new_property(S7::class_any),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = hourmap__series_error
)
# }}}

# Validate one future-model year after all variable groups share the same EPW
# target grid while retaining their independent source metadata.
# hourmap__member_error {{{
hourmap__member_error <- function(self) {
    error <- sequence__first_error(
        sequence__validate_member_header(
            self@sequence_id,
            self@weather_year,
            self@source_calendar,
            calendar_name = "source_calendar",
            provenance = self@provenance
        ),
        sequence__member_class_error(
            self@series,
            MappedHourlyClimateSeries,
            "`series` must contain MappedHourlyClimateSeries objects."
        )
    )
    if (!is.null(error)) {
        return(error)
    }
    group_ids <- vapply(self@series, function(x) x@group_id, character(1L))
    variables <- unlist(
        lapply(self@series, function(x) x@variables),
        use.names = FALSE
    )
    years <- unique(unlist(
        lapply(self@series, function(x) as.integer(x@data[["year"]])),
        use.names = FALSE
    ))
    sequence__first_error(
        if (anyDuplicated(group_ids)) {
            "Mapped hourly group identities must be unique within a year."
        },
        if (anyDuplicated(variables)) {
            "Each mapped hourly variable must occur in exactly one group."
        },
        if (!identical(years, self@weather_year)) {
            "Every mapped hourly row must match `weather_year`."
        }
    )
}
# }}}

# MappedHourlyClimateMember groups every mapped climate variable for one
# source-model year before EPW physical fields are constructed.
# MappedHourlyClimateMember {{{
MappedHourlyClimateMember <- S7::new_class(
    "MappedHourlyClimateMember",
    properties = list(
        sequence_id = S7::new_property(S7::class_character),
        weather_year = S7::new_property(S7::class_integer),
        source_calendar = S7::new_property(S7::class_character),
        series = S7::new_property(S7::class_list),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = hourmap__member_error
)
# }}}

# Validate the complete mapped sequence independently of the number of source
# years retained by the selected future-model period.
# hourmap__sequence_error {{{
hourmap__sequence_error <- function(self) {
    error <- sequence__first_error(
        sequence__validate_members(
            self@members,
            MappedHourlyClimateMember,
            "MappedHourlyClimateMember",
            "Mapped hourly members",
            provenance = self@provenance
        ),
        if (
            !identical(self@frequency, "hour") ||
                !identical(as.numeric(self@time_step_seconds), 3600)
        ) {
            "Mapped hourly climate sequences require a 3600-second hourly timestep."
        },
        if (!identical(self@target_calendar, "epw_365_day")) {
            "`target_calendar` must be `epw_365_day`."
        }
    )
    if (!is.null(error)) {
        return(error)
    }
    variable_sets <- lapply(self@members, function(member) {
        sort(unlist(
            lapply(member@series, function(s) s@variables),
            use.names = FALSE
        ))
    })
    sequence__shared_sets_error(
        variable_sets,
        "Every mapped hourly member must contain the same variables."
    )
}
# }}}

# MappedHourlyClimateSequence is the typed boundary consumed by later physical
# closure and output components for direct-model future years.
# MappedHourlyClimateSequence {{{
MappedHourlyClimateSequence <- S7::new_class(
    "MappedHourlyClimateSequence",
    properties = list(
        members = S7::new_property(S7::class_list),
        frequency = S7::new_property(S7::class_character),
        time_step_seconds = S7::new_property(S7::class_numeric),
        target_calendar = S7::new_property(S7::class_character),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = hourmap__sequence_error
)
# }}}

# Read and validate the fixed EPW target lattice without relying on the
# template year, which may be a conventional placeholder rather than a model year.
# hourmap__target_grid {{{
hourmap__target_grid <- function(template) {
    if (!inherits(template, "EpwFile")) {
        cli::cli_abort(
            "`weather_template` must contain an internal {.cls EpwFile} object."
        )
    }
    weather <- data.table::as.data.table(data.table::copy(template$data()))
    required <- c("month", "day", "hour", "minute")
    missing <- setdiff(required, names(weather))
    if (length(missing)) {
        cli::cli_abort(
            "The EPW template is missing calendar column(s): {.val {missing}}."
        )
    }
    if (nrow(weather) != HOURMAP_TARGET_HOURS) {
        cli::cli_abort(
            "The EPW template must contain exactly 8760 rows on a 365-day hourly calendar."
        )
    }

    day_fields <- cf_time_offset2date(
        seq.int(0L, HOURMAP_TARGET_DAYS - 1L),
        data.frame(year = 2001L, month = 1L, day = 1L),
        "noleap"
    )
    expected_month <- rep(as.integer(day_fields$month), each = 24L)
    expected_day <- rep(as.integer(day_fields$day), each = 24L)
    expected_hour <- rep.int(seq_len(24L), HOURMAP_TARGET_DAYS)
    if (
        !identical(as.integer(weather[["month"]]), expected_month) ||
            !identical(as.integer(weather[["day"]]), expected_day) ||
            !identical(as.integer(weather[["hour"]]), expected_hour)
    ) {
        cli::cli_abort(
            "The EPW template must use ordered non-leap month/day rows with hours 1 through 24."
        )
    }
    if (any(!is.finite(as.numeric(weather[["minute"]])))) {
        cli::cli_abort("The EPW template must contain finite minute values.")
    }

    data.table::data.table(
        epw_row = seq_len(HOURMAP_TARGET_HOURS),
        epw_day = rep(seq_len(HOURMAP_TARGET_DAYS), each = 24L),
        month = expected_month,
        day = expected_day,
        hour = expected_hour,
        minute = as.integer(weather[["minute"]])
    )
}
# }}}

# Integrate piecewise-constant source interval means over uniform target bins
# on normalized annual phase, preserving the annual mean for every CF calendar.
# hourmap__conservative_interval_mean {{{
hourmap__conservative_interval_mean <- function(value, target_count) {
    checkmate::assert_numeric(value, min.len = 1L, any.missing = FALSE)
    checkmate::assert_count(target_count, positive = TRUE)
    source_count <- length(value)
    if (source_count == target_count) {
        return(as.numeric(value))
    }

    # The primitive is expressed on [0, 1]. Evaluating it at every target edge
    # avoids a large source-by-target overlap matrix for full hourly years.
    cumulative <- c(0, cumsum(as.numeric(value)))
    target_edge <- seq.int(0L, target_count) / target_count
    source_position <- target_edge * source_count
    whole <- pmin(source_count, floor(source_position))
    fraction <- source_position - whole
    primitive <- cumulative[whole + 1L] / source_count
    partial <- whole < source_count & fraction > 0
    primitive[partial] <- primitive[partial] +
        fraction[partial] * value[whole[partial] + 1L] / source_count
    diff(primitive) * target_count
}
# }}}

# Return the common 24-position daily lattice so calendar mapping changes only
# seasonal day position and never shifts a variable's time of day.
# hourmap__daily_offsets {{{
hourmap__daily_offsets <- function(data) {
    lattice <- temporal__daily_lattice(data[["cf_second_of_day"]])
    if (!isTRUE(lattice$regular)) {
        cli::cli_abort(
            "Mapped hourly variables must use one complete regular 24-position daily lattice."
        )
    }
    lattice$offsets
}
# }}}

# Traverse each time-of-day series on the shared native-year-to-EPW lattice.
# hourmap__map_daily_slots {{{
hourmap__map_daily_slots <- function(data, target_days, mapper) {
    checkmate::assert_function(mapper)
    source_count <- nrow(data)
    target_count <- target_days * 24L
    offsets <- hourmap__daily_offsets(data)
    if (source_count == target_count) {
        return(list(
            value = as.numeric(data[["value"]]),
            target_phase = as.numeric(data[["annual_phase"]]),
            source_second_of_day = as.numeric(
                data[["cf_second_of_day"]]
            ),
            hour_phase_seconds = offsets[[1L]] %% 3600
        ))
    }

    mapped <- numeric(target_count)
    target_phase <- numeric(target_count)
    source_second_of_day <- numeric(target_count)
    target_day <- seq.int(0L, target_days - 1L)
    for (offset_index in seq_along(offsets)) {
        offset <- offsets[[offset_index]]
        source_rows <- which(data[["cf_second_of_day"]] == offset)
        target_rows <- seq.int(offset_index, target_count, by = 24L)
        phase <- (target_day + offset / 86400) / target_days

        # The callback owns only the numerical mapping kernel. This shell owns
        # target placement and time-of-day provenance for every variable type.
        mapped[target_rows] <- mapper(source_rows, phase)
        target_phase[target_rows] <- phase
        source_second_of_day[target_rows] <- offset
    }
    list(
        value = mapped,
        target_phase = target_phase,
        source_second_of_day = source_second_of_day,
        hour_phase_seconds = offsets[[1L]] %% 3600
    )
}
# }}}

# Map point samples separately at each source time of day by circular annual
# phase, preventing 360/366-day conversion from drifting the diurnal cycle.
# hourmap__circular_point_values {{{
hourmap__circular_point_values <- function(data, target_days) {
    hourmap__map_daily_slots(
        data,
        target_days,
        mapper = function(source_rows, target_phase) {
            source_phase <- as.numeric(data[["annual_phase"]][source_rows])
            source_value <- as.numeric(data[["value"]][source_rows])

            # Repeat one source year on each side so January and December use
            # the same circular chronology as all interior target days.
            daily__circular_interpolate(
                source_phase,
                source_value,
                target_phase
            )
        }
    )
}
# }}}

# Conservatively remap each time-of-day series across calendar days so
# interval means retain both their diurnal slot and normalized annual mean.
# hourmap__conservative_interval_values {{{
hourmap__conservative_interval_values <- function(data, target_days) {
    hourmap__map_daily_slots(
        data,
        target_days,
        mapper = function(source_rows, target_phase) {
            hourmap__conservative_interval_mean(
                data[["value"]][source_rows],
                length(target_phase)
            )
        }
    )
}
# }}}

# Map one adjusted variable and retain the explicit temporal semantics and
# numerical diagnostics required by later physical and output stages.
# hourmap__variable {{{
hourmap__variable <- function(data, variable, member, target) {
    data <- data.table::as.data.table(data.table::copy(data))
    # Capture the function argument outside data.table evaluation because
    # source provenance may legitimately include a column named `variable`.
    target_variable <- variable
    data <- data[variable_id == target_variable]
    data.table::setorderv(
        data,
        c("cf_day_of_year", "cf_second_of_day")
    )
    expected_samples <- cf_time__year_days(
        member@weather_year,
        member@calendar
    )[[1L]] *
        24L
    if (nrow(data) != expected_samples) {
        cli::cli_abort(
            paste(
                "Variable {.val {variable}} in weather year",
                "{member@weather_year} has {nrow(data)} samples; expected",
                "{expected_samples} for one complete hourly native-calendar year."
            )
        )
    }
    units <- unique(as.character(data[["units"]]))
    if (length(units) != 1L) {
        cli::cli_abort(
            "Variable {.val {variable}} in weather year {member@weather_year} must use one unit."
        )
    }

    if (variable %in% HOURMAP_POINT_VARIABLES) {
        mapping_method <- if (nrow(data) == HOURMAP_TARGET_HOURS) {
            "identity_365_day"
        } else {
            "circular_linear_annual_phase"
        }
        mapped <- hourmap__circular_point_values(
            data,
            HOURMAP_TARGET_DAYS
        )
        annual_mean_error <- NA_real_
    } else if (variable %in% HOURMAP_INTERVAL_MEAN_VARIABLES) {
        mapping_method <- if (nrow(data) == HOURMAP_TARGET_HOURS) {
            "identity_365_day"
        } else {
            "conservative_normalized_interval"
        }
        mapped <- hourmap__conservative_interval_values(
            data,
            HOURMAP_TARGET_DAYS
        )
        annual_mean_error <- mean(mapped$value) - mean(data[["value"]])
    } else {
        cli::cli_abort(c(
            "Variable {.val {variable}} has no declared native-calendar-to-EPW hourly mapping semantics.",
            "i" = "Supported point variables: {.val {HOURMAP_POINT_VARIABLES}}.",
            "i" = "Supported interval-mean variables: {.val {HOURMAP_INTERVAL_MEAN_VARIABLES}}."
        ))
    }

    mapped_wind_direction <- NULL
    wind_direction_mapping <- NA_character_
    if (
        identical(variable, "sfcWind") &&
            "wind_direction" %in% names(data)
    ) {
        direction <- as.numeric(data[["wind_direction"]])
        if (any(!is.finite(direction))) {
            cli::cli_abort(
                "Hourly `sfcWind` direction metadata must contain finite values."
            )
        }
        # Interpolate direction as unit-vector components so the circular
        # 0/360-degree boundary cannot create an artificial reversal.
        eastward <- data.table::copy(data)
        northward <- data.table::copy(data)
        data.table::set(
            eastward,
            j = "value",
            value = -sin(direction * pi / 180)
        )
        data.table::set(
            northward,
            j = "value",
            value = -cos(direction * pi / 180)
        )
        mapped_eastward <- hourmap__circular_point_values(
            eastward,
            HOURMAP_TARGET_DAYS
        )$value
        mapped_northward <- hourmap__circular_point_values(
            northward,
            HOURMAP_TARGET_DAYS
        )$value
        mapped_wind_direction <- epwphys__wind_from_components(
            mapped_eastward,
            mapped_northward
        )$direction
        wind_direction_mapping <- "circular_vector_interpolation"
    }

    output <- data.table::copy(target)
    n_out <- nrow(output)
    columns <- list(
        year = rep.int(member@weather_year, n_out),
        target_annual_phase = mapped$target_phase,
        variable_id = rep.int(variable, n_out),
        value = mapped$value
    )
    if (!is.null(mapped_wind_direction)) {
        columns$wind_direction <- mapped_wind_direction
    }
    columns <- c(
        columns,
        list(
            units = rep.int(units, n_out),
            mapping_method = rep.int(mapping_method, n_out),
            source_calendar = rep.int(member@calendar, n_out),
            source_year = rep.int(member@weather_year, n_out),
            source_second_of_day = mapped$source_second_of_day,
            source_hour_phase_seconds = rep.int(
                mapped$hour_phase_seconds,
                n_out
            )
        )
    )
    for (col in names(columns)) {
        data.table::set(output, j = col, value = columns[[col]])
    }
    diagnostic <- data.frame(
        variable_id = variable,
        units = units,
        source_calendar = member@calendar,
        source_year = member@weather_year,
        source_samples = nrow(data),
        target_samples = nrow(output),
        mapping_method = mapping_method,
        source_hour_phase_seconds = mapped$hour_phase_seconds,
        source_minimum = min(data[["value"]]),
        source_maximum = max(data[["value"]]),
        target_minimum = min(mapped$value),
        target_maximum = max(mapped$value),
        annual_mean_error = annual_mean_error,
        wind_direction_mapping = wind_direction_mapping,
        stringsAsFactors = FALSE
    )
    list(data = output[], diagnostic = diagnostic)
}
# }}}

# Map every variable carried by one signal group without flattening its group
# identity or upstream signal provenance into the hourly table.
# hourmap__series {{{
hourmap__series <- function(series, member, target) {
    mapped <- lapply(series@variables, function(variable) {
        hourmap__variable(
            series@adjusted@data,
            variable,
            member,
            target
        )
    })
    MappedHourlyClimateSeries(
        group_id = series@group_id,
        key = series@key,
        variables = series@variables,
        data = data.table::rbindlist(
            lapply(mapped, `[[`, "data"),
            use.names = TRUE,
            fill = TRUE
        ),
        diagnostics = do.call(
            rbind,
            lapply(mapped, `[[`, "diagnostic")
        ),
        provenance = list(
            source_transformation = series@adjusted@transformation,
            source_settings = series@adjusted@settings,
            source_provenance = series@adjusted@provenance,
            target_calendar = "epw_365_day",
            point_mapping = "circular_linear_annual_phase",
            interval_mapping = "conservative_normalized_interval"
        )
    )
}
# }}}

# Reconstruct all direct-model years on one fixed EPW target lattice while
# leaving physical variable conversion to the following component stage.
# hourmap__reconstruct {{{
hourmap__reconstruct <- function(data, inputs, context, options) {
    if (!S7::S7_inherits(data, DirectModelSequence)) {
        cli::cli_abort(
            "`direct_model_epw_calendar_mapping` requires a DirectModelSequence object."
        )
    }
    if (
        !identical(data@frequency, "hour") ||
            !identical(as.numeric(data@time_step_seconds), 3600)
    ) {
        cli::cli_abort(
            "`direct_model_epw_calendar_mapping` requires an hourly direct-model sequence with a 3600-second timestep."
        )
    }
    checkmate::assert_list(options, names = "unique")
    target <- hourmap__target_grid(sequence__epw_template_input(inputs))

    members <- lapply(data@members, function(member) {
        variables <- unlist(
            lapply(
                member@series,
                function(series) series@variables
            ),
            use.names = FALSE
        )
        if (anyDuplicated(variables)) {
            cli::cli_abort(
                "Weather year {member@weather_year} contains duplicate direct-model variable groups: {.val {unique(variables[duplicated(variables)])}}."
            )
        }
        series <- lapply(
            member@series,
            hourmap__series,
            member = member,
            target = target
        )
        MappedHourlyClimateMember(
            sequence_id = member@sequence_id,
            weather_year = member@weather_year,
            source_calendar = member@calendar,
            series = series,
            provenance = utils::modifyList(
                member@provenance,
                list(
                    source_calendar = member@calendar,
                    target_calendar = "epw_365_day",
                    target_days = HOURMAP_TARGET_DAYS,
                    target_hours = HOURMAP_TARGET_HOURS
                )
            )
        )
    })
    MappedHourlyClimateSequence(
        members = members,
        frequency = "hour",
        time_step_seconds = 3600,
        target_calendar = "epw_365_day",
        provenance = list(
            method = "direct_model_epw_calendar_mapping",
            source_sequence = data@provenance,
            target_calendar = "epw_365_day",
            target_days = HOURMAP_TARGET_DAYS,
            target_hours = HOURMAP_TARGET_HOURS,
            point_mapping = "circular_linear_annual_phase",
            interval_mapping = "conservative_normalized_interval",
            physical_conversion = "deferred"
        )
    )
}
# }}}

# Describe the reusable direct-model calendar bridge independently of any one
# bias-adjustment kernel, physical closure, or complete published recipe.
# hourmap__component {{{
hourmap__component <- function() {
    component__spec(
        name = "direct_model_epw_calendar_mapping",
        stage = "hourly",
        label = "Direct-model EPW calendar mapping",
        required_inputs = sequence__epw_template_requirement(),
        input_kinds = "direct_model_sequence",
        output_kinds = "epw_hourly_climate_sequence",
        scopes = "multivariate",
        stochastic = FALSE,
        operations = list(reconstruct = hourmap__reconstruct),
        metadata = list(
            algorithm = "native_cf_to_epw_hourly_mapping",
            source_frequency = "hour",
            source_step_seconds = 3600,
            target_calendar = "epw_365_day",
            target_days = HOURMAP_TARGET_DAYS,
            target_hours = HOURMAP_TARGET_HOURS,
            point_variables = HOURMAP_POINT_VARIABLES,
            interval_mean_variables = HOURMAP_INTERVAL_MEAN_VARIABLES,
            point_mapping = "circular_linear_annual_phase",
            interval_mapping = "conservative_normalized_interval",
            output_contract = "epw_hourly_climate_sequence"
        )
    )
}
# }}}

# Register the standalone hourly mapping once so later physical and output
# components can resolve it through the common component registry.
# hourmap__register_component {{{
hourmap__register_component <- function() {
    component__register_builtin(hourmap__component())
    invisible(NULL)
}
# }}}

# The direct-model EPW physical boundary requires the climate families needed
# to construct the principal EnergyPlus weather drivers for every future year.
DIRECT_EPW_CORE_VARIABLES <- c("tas", "ps", "rsds", "rsdsdiff")
DIRECT_EPW_HUMIDITY_VARIABLES <- c("hurs", "huss")
DIRECT_EPW_WIND_VARIABLES <- c("sfcWind", "uas", "vas")
DIRECT_EPW_OPTIONAL_VARIABLES <- "rlds"
DIRECT_EPW_SUPPORTED_VARIABLES <- c(
    DIRECT_EPW_CORE_VARIABLES,
    DIRECT_EPW_HUMIDITY_VARIABLES,
    DIRECT_EPW_WIND_VARIABLES,
    DIRECT_EPW_OPTIONAL_VARIABLES
)

# Modified fields are declared once so diagnostics and later output components
# can distinguish constructed values from fields inherited from the EPW template.
DIRECT_EPW_CONSTRUCTED_FIELDS <- c(
    "dry_bulb_temperature",
    "dew_point_temperature",
    "relative_humidity",
    "atmospheric_pressure",
    "global_horizontal_radiation",
    "direct_normal_radiation",
    "diffuse_horizontal_radiation",
    "wind_speed"
)

# Validate one physically closed future year without depending on a complete
# recipe or the final WeatherSequenceResult output wrapper.
# direct_epw__member_error {{{
direct_epw__member_error <- function(self) {
    error <- sequence__first_error(
        sequence__validate_member_header(
            self@sequence_id,
            self@weather_year,
            self@source_calendar,
            calendar_name = "source_calendar",
            provenance = self@provenance
        ),
        if (
            !is.data.frame(self@data) || nrow(self@data) != HOURMAP_TARGET_HOURS
        ) {
            "`data` must contain exactly 8760 EPW weather rows."
        },
        if (!is.data.frame(self@diagnostics) || nrow(self@diagnostics) != 1L) {
            "`diagnostics` must contain one row for the closed weather year."
        }
    )
    if (!is.null(error)) {
        return(error)
    }
    missing <- setdiff(EPW_FILE_COLUMNS, names(self@data))
    if (length(missing)) {
        return(sprintf(
            "`data` is missing EPW field(s): %s.",
            paste(sprintf("`%s`", missing), collapse = ", ")
        ))
    }
    if (
        !identical(unique(as.integer(self@data[["year"]])), self@weather_year)
    ) {
        return("Every physically closed row must match `weather_year`.")
    }
    NULL
}
# }}}

# EpwHourlyWeatherMember carries one baseline-shaped future EPW year after all
# dependent thermodynamic, wind, and shortwave fields have been closed.
# EpwHourlyWeatherMember {{{
EpwHourlyWeatherMember <- S7::new_class(
    "EpwHourlyWeatherMember",
    properties = list(
        sequence_id = S7::new_property(S7::class_character),
        weather_year = S7::new_property(S7::class_integer),
        source_calendar = S7::new_property(S7::class_character),
        data = S7::new_property(S7::class_any),
        diagnostics = S7::new_property(S7::class_any),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = direct_epw__member_error
)
# }}}

# Validate the ordered collection of closed years before an output component
# converts it into the package's public multi-year result contract.
# direct_epw__sequence_error {{{
direct_epw__sequence_error <- function(self) {
    sequence__first_error(
        sequence__validate_members(
            self@members,
            EpwHourlyWeatherMember,
            "EpwHourlyWeatherMember",
            "Closed EPW weather members",
            year_label = "years",
            provenance = self@provenance
        ),
        if (!identical(self@target_calendar, "epw_365_day")) {
            "`target_calendar` must be `epw_365_day`."
        },
        if (
            !length(self@constructed_fields) ||
                anyNA(self@constructed_fields) ||
                any(!self@constructed_fields %in% EPW_FILE_COLUMNS) ||
                anyDuplicated(self@constructed_fields)
        ) {
            "`constructed_fields` must contain unique supported EPW fields."
        }
    )
}
# }}}

# EpwHourlyWeatherSequence is the typed output of the physics stage and the
# direct input expected by the later multi-year EPW result writer.
# EpwHourlyWeatherSequence {{{
EpwHourlyWeatherSequence <- S7::new_class(
    "EpwHourlyWeatherSequence",
    properties = list(
        members = S7::new_property(S7::class_list),
        target_calendar = S7::new_property(S7::class_character),
        constructed_fields = S7::new_property(S7::class_character),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = direct_epw__sequence_error
)
# }}}

# Collect one mapped member into unique variable tables while retaining the
# row-level mapping metadata for provenance and validation.
# direct_epw__variables {{{
direct_epw__variables <- function(member) {
    variables <- unlist(
        lapply(
            member@series,
            function(series) series@variables
        ),
        use.names = FALSE
    )
    if (anyDuplicated(variables)) {
        cli::cli_abort(
            "Weather year {member@weather_year} contains duplicate mapped variable groups: {.val {unique(variables[duplicated(variables)])}}."
        )
    }
    tables <- lapply(variables, function(variable) {
        containing <- Filter(
            function(series) variable %in% series@variables,
            member@series
        )
        rows <- containing[[1L]]@data[
            containing[[1L]]@data[["variable_id"]] == variable,
            ,
            drop = FALSE
        ]
        rows[order(rows[["epw_row"]]), , drop = FALSE]
    })
    names(tables) <- variables
    tables
}
# }}}

# Resolve the mutually exclusive humidity and wind input paths before any
# values are converted so ambiguity cannot be hidden by downstream clipping.
# direct_epw__variable_contract {{{
direct_epw__variable_contract <- function(variables, weather_year) {
    present <- names(variables)
    unsupported <- setdiff(present, DIRECT_EPW_SUPPORTED_VARIABLES)
    if (length(unsupported)) {
        cli::cli_abort(
            "Weather year {weather_year} contains unsupported EPW physical variable(s): {.val {unsupported}}."
        )
    }
    missing <- setdiff(DIRECT_EPW_CORE_VARIABLES, present)
    if (length(missing)) {
        cli::cli_abort(
            "Weather year {weather_year} is missing required EPW physical variable(s): {.val {missing}}."
        )
    }

    humidity <- intersect(DIRECT_EPW_HUMIDITY_VARIABLES, present)
    if (length(humidity) != 1L) {
        cli::cli_abort(
            "Weather year {weather_year} must provide exactly one humidity path: `hurs` or `huss`."
        )
    }
    has_speed <- "sfcWind" %in% present
    vector_parts <- intersect(c("uas", "vas"), present)
    if (length(vector_parts) == 1L) {
        cli::cli_abort(
            "Weather year {weather_year} must provide `uas` and `vas` together."
        )
    }
    has_vector <- length(vector_parts) == 2L
    if (identical(has_speed, has_vector)) {
        cli::cli_abort(
            "Weather year {weather_year} must provide exactly one wind path: `sfcWind` or paired `uas` and `vas`."
        )
    }
    list(
        humidity = humidity[[1L]],
        wind = if (has_speed) "sfcWind" else "uas_vas",
        has_rlds = "rlds" %in% present
    )
}
# }}}

DIRECT_EPW_VARIABLE_UNITS <- list(
    tas = list(allowed = c("K", "degC"), field = "dry_bulb_temperature"),
    ps = list(allowed = c("Pa", "hPa"), field = "atmospheric_pressure"),
    hurs = list(allowed = "%", field = "relative_humidity"),
    huss = list(allowed = c("1", "kg/kg"), field = NULL),
    sfcWind = list(allowed = "m/s", field = "wind_speed"),
    uas = list(allowed = "m/s", field = "wind_speed"),
    vas = list(allowed = "m/s", field = "wind_speed"),
    rsds = list(allowed = "W/m^2", field = "global_horizontal_radiation"),
    rsdsdiff = list(allowed = "W/m^2", field = "diffuse_horizontal_radiation"),
    rlds = list(
        allowed = "W/m^2",
        field = "horizontal_infrared_radiation_intensity_from_sky"
    )
)

# Convert one mapped variable using a narrow unit schema chosen for the EPW
# fields constructed by this component.
# direct_epw__values {{{
direct_epw__values <- function(variables, variable) {
    rows <- variables[[variable]]
    if (is.null(rows)) {
        cli::cli_abort(
            "Mapped EPW climate variable {.val {variable}} is missing."
        )
    }
    units <- unique(vapply(
        unique(as.character(rows[["units"]])),
        morpher__unit_alias,
        character(1L)
    ))
    if (length(units) != 1L) {
        cli::cli_abort(
            "Mapped EPW climate variable {.val {variable}} must use one unit."
        )
    }
    unit <- units[[1L]]
    spec <- DIRECT_EPW_VARIABLE_UNITS[[variable]]
    if (is.na(unit) || !unit %in% spec$allowed) {
        cli::cli_abort(
            "Mapped EPW climate variable {.val {variable}} uses unsupported unit {.val {unit}}; expected {.val {spec$allowed}}."
        )
    }
    target_unit <- if (is.null(spec$field)) unit else epw_file_unit(spec$field)
    converted <- morpher__convert_value_checked(
        rows[["value"]],
        unit,
        target_unit
    )
    if (!isTRUE(converted$ok)) {
        cli::cli_abort(converted$message)
    }
    converted$value
}
# }}}

# Compute solar geometry on a fixed non-leap surrogate year so a future leap
# year cannot shift March-through-December EPW rows by one astronomical day.
# direct_epw__solar_geometry {{{
direct_epw__solar_geometry <- function(template, epw) {
    loc <- morpher__epw_location(epw)
    geometry_input <- data.table::data.table(
        year = rep.int(2001L, nrow(template)),
        month = as.integer(template[["month"]]),
        day = as.integer(template[["day"]]),
        hour = as.integer(template[["hour"]])
    )
    solar__epw_interval_geometry(
        geometry_input,
        loc$latitude,
        loc$longitude,
        loc$timezone
    )
}
# }}}

# Construct one future EPW weather year and retain every field outside the
# declared climate families exactly as stored in the baseline template. The
# adapter supplies source-specific candidates; the shared physical layer owns
# every derivation, bound, and closure operation.
# direct_epw__member {{{
direct_epw__member <- function(member, epw, template, geometry) {
    variables <- direct_epw__variables(member)
    contract <- direct_epw__variable_contract(
        variables,
        member@weather_year
    )
    physical_template <- data.table::as.data.table(data.table::copy(template))
    data.table::set(
        physical_template,
        j = "year",
        value = rep.int(member@weather_year, nrow(physical_template))
    )
    fields <- list(
        dry_bulb_temperature = direct_epw__values(variables, "tas"),
        atmospheric_pressure = direct_epw__values(variables, "ps")
    )
    humidity <- if (identical(contract$humidity, "hurs")) {
        list(relative_humidity = direct_epw__values(variables, "hurs"))
    } else {
        list(target_specific_humidity = direct_epw__values(variables, "huss"))
    }
    wind <- if (identical(contract$wind, "sfcWind")) {
        speed <- direct_epw__values(variables, "sfcWind")
        rows <- variables[["sfcWind"]]
        if ("wind_direction" %in% names(rows)) {
            list(
                speed = speed,
                direction = as.numeric(rows[["wind_direction"]])
            )
        } else {
            list(speed = speed)
        }
    } else {
        list(
            eastward = direct_epw__values(variables, "uas"),
            northward = direct_epw__values(variables, "vas")
        )
    }
    constructed_fields <- DIRECT_EPW_CONSTRUCTED_FIELDS
    if (
        identical(contract$wind, "uas_vas") ||
            "direction" %in% names(wind)
    ) {
        constructed_fields <- c(constructed_fields, "wind_direction")
    }
    if (contract$has_rlds) {
        fields$horizontal_infrared_radiation_intensity_from_sky <-
            direct_epw__values(variables, "rlds")
        constructed_fields <- c(
            constructed_fields,
            "horizontal_infrared_radiation_intensity_from_sky"
        )
    }
    physical <- epwphys__apply(
        EpwPhysicalRequest(
            template = physical_template,
            fields = fields,
            humidity = humidity,
            wind = wind,
            shortwave = list(
                global_horizontal = direct_epw__values(variables, "rsds"),
                diffuse_horizontal = direct_epw__values(variables, "rsdsdiff")
            ),
            geometry = geometry,
            provenance = list(
                source_calendar = member@source_calendar,
                source_variables = names(variables)
            )
        ),
        epwphys__policy("absolute_model_fields")
    )
    weather <- physical@weather
    corrections <- physical@corrections
    wind_state <- physical@state$wind
    if ("datetime" %in% names(weather)) {
        data.table::set(
            weather,
            j = "datetime",
            value = epw_file__datetime(
                weather[["year"]],
                weather[["month"]],
                weather[["day"]],
                weather[["hour"]]
            )
        )
        data.table::setcolorder(
            weather,
            c("datetime", setdiff(names(weather), "datetime"))
        )
    }

    diagnostics <- data.frame(
        weather_year = member@weather_year,
        source_calendar = member@source_calendar,
        humidity_source = contract$humidity,
        wind_source = contract$wind,
        wind_direction_policy = wind_state$direction_policy,
        temperature_clipped = corrections$temperature_clipped,
        pressure_clipped = corrections$pressure_clipped,
        humidity_saturation_clipped = corrections$humidity_saturation_clipped,
        specific_humidity_clipped = corrections$specific_humidity_clipped,
        dew_point_clipped = corrections$dew_point_clipped,
        wind_speed_clipped = corrections$wind_speed_clipped,
        radiation_night_values_zeroed = corrections$radiation_night_values_zeroed,
        radiation_negative_global_clipped = corrections$radiation_negative_global_clipped,
        radiation_negative_diffuse_clipped = corrections$radiation_negative_diffuse_clipped,
        radiation_diffuse_above_global_clipped = corrections$radiation_diffuse_above_global_clipped,
        radiation_excess_beam_reallocated = corrections$radiation_excess_beam_reallocated,
        radiation_maximum_closure_error = corrections$radiation_maximum_closure_error,
        infrared_negative_clipped = corrections$infrared_negative_clipped,
        stringsAsFactors = FALSE
    )
    EpwHourlyWeatherMember(
        sequence_id = member@sequence_id,
        weather_year = member@weather_year,
        source_calendar = member@source_calendar,
        data = weather[],
        diagnostics = diagnostics,
        provenance = list(
            method = "epw_hourly_physical_closure",
            source = member@provenance,
            source_variables = names(variables),
            humidity_source = contract$humidity,
            wind_source = contract$wind,
            wind_direction_policy = wind_state$direction_policy,
            physical_policy = physical@policy@name,
            solar_calendar = "fixed_365_day",
            shortwave_closure = "ghi_equals_dhi_plus_dni_projection",
            constructed_fields = constructed_fields,
            inherited_fields = setdiff(
                EPW_FILE_COLUMNS,
                c("year", constructed_fields)
            )
        )
    )
}
# }}}

# Close every mapped climate member against one shared EPW template while
# allowing unrelated recipe options to remain owned by their declared stages.
# direct_epw__apply {{{
direct_epw__apply <- function(data, inputs, context, options) {
    if (!S7::S7_inherits(data, MappedHourlyClimateSequence)) {
        cli::cli_abort(
            "`epw_hourly_physical_closure` requires a MappedHourlyClimateSequence object."
        )
    }
    checkmate::assert_list(options, names = "unique")
    epw <- sequence__epw_template_input(inputs, require_epw_file = TRUE)
    hourmap__target_grid(epw)
    # Absolute-model recipes inherit fields outside their climate contract;
    # represent missing inherited observations as NA inside the result.
    template <- epw_file__calculation_weather(epw$data())
    geometry <- direct_epw__solar_geometry(template, epw)
    members <- lapply(
        data@members,
        direct_epw__member,
        epw = epw,
        template = template,
        geometry = geometry
    )
    EpwHourlyWeatherSequence(
        members = members,
        target_calendar = "epw_365_day",
        constructed_fields = unique(unlist(
            lapply(
                members,
                function(member) member@provenance$constructed_fields
            ),
            use.names = FALSE
        )),
        provenance = list(
            method = "epw_hourly_physical_closure",
            physical_policy = "absolute_model_fields",
            source = data@provenance,
            target_calendar = "epw_365_day",
            thermodynamic_closure = "humidity_dew_point_temperature_pressure",
            solar_closure = "ghi_dhi_dni_fixed_365_day_geometry",
            unchanged_field_policy = "inherit_epw_template",
            member_count = length(members)
        )
    )
}
# }}}

# Describe the shared physical closure independently of the signal method or
# complete future-weather recipe that supplies the mapped climate sequence.
# direct_epw__component {{{
direct_epw__component <- function() {
    component__spec(
        name = "epw_hourly_physical_closure",
        stage = "physics",
        label = "EPW hourly physical closure",
        required_inputs = sequence__epw_template_requirement(),
        input_kinds = "epw_hourly_climate_sequence",
        output_kinds = "epw_hourly_weather_sequence",
        scopes = "multivariate",
        stochastic = FALSE,
        operations = list(apply = direct_epw__apply),
        metadata = list(
            algorithm = "epw_hourly_physical_closure",
            required_variables = DIRECT_EPW_CORE_VARIABLES,
            humidity_alternatives = DIRECT_EPW_HUMIDITY_VARIABLES,
            wind_alternatives = list(
                scalar = "sfcWind",
                vector = c("uas", "vas")
            ),
            optional_variables = DIRECT_EPW_OPTIONAL_VARIABLES,
            physical_policies = "absolute_model_fields",
            target_calendar = "epw_365_day",
            shortwave_closure = "ghi_equals_dhi_plus_dni_projection",
            unchanged_field_policy = "inherit_epw_template",
            output_contract = "epw_hourly_weather_sequence"
        )
    )
}
# }}}

# Register the method-neutral physical boundary once so later output and recipe
# components can resolve it through the shared component registry.
# direct_epw__register_component {{{
direct_epw__register_component <- function() {
    component__register_builtin(direct_epw__component())
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
