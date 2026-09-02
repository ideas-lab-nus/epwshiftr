#' @include weather-sequence.R
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

# Validate one mapped group independently of the physical variables that a
# later stage will derive from its climate-series values.
hourmap__series_error <- function(self) {
    if (length(self@group_id) != 1L ||
        is.na(self@group_id) ||
        !grepl("^[a-z][a-z0-9-]*$", self@group_id)) {
        return("`group_id` must use lower-case letters, numbers, and hyphens.")
    }
    if (length(self@key) &&
        (is.null(names(self@key)) ||
            any(!nzchar(names(self@key))) ||
            anyDuplicated(names(self@key)))) {
        return("`key` must be a uniquely named list.")
    }
    if (!length(self@variables) ||
        anyNA(self@variables) ||
        any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", self@variables)) ||
        anyDuplicated(self@variables)) {
        return("`variables` must contain unique CMIP-style identifiers.")
    }
    if (!is.data.frame(self@data) || !nrow(self@data)) {
        return("`data` must be a non-empty mapped hourly table.")
    }
    required <- c(
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
    missing <- setdiff(required, names(self@data))
    if (length(missing)) {
        return(sprintf(
            "`data` is missing mapped hourly column(s): %s.",
            paste(sprintf("`%s`", missing), collapse = ", ")
        ))
    }
    if (!setequal(
        self@variables,
        unique(as.character(self@data[["variable_id"]]))
    )) {
        return("`variables` must match the mapped hourly table.")
    }
    if (!is.numeric(self@data[["value"]]) ||
        any(!is.finite(self@data[["value"]]))) {
        return("Mapped hourly `value` must contain only finite numbers.")
    }
    expected_rows <- HOURMAP_TARGET_HOURS * length(self@variables)
    if (nrow(self@data) != expected_rows) {
        return("Every mapped variable must contain exactly 8760 EPW rows.")
    }
    for (variable in self@variables) {
        rows <- self@data[
            self@data[["variable_id"]] == variable,
            ,
            drop = FALSE
        ]
        if (!identical(as.integer(rows[["epw_row"]]), seq_len(
            HOURMAP_TARGET_HOURS
        ))) {
            return(sprintf(
                "Mapped variable `%s` must use ordered EPW rows 1 through 8760.",
                variable
            ))
        }
    }
    if (!is.data.frame(self@diagnostics) ||
        nrow(self@diagnostics) != length(self@variables)) {
        return("`diagnostics` must contain one row per mapped variable.")
    }
    if (length(self@provenance) &&
        (is.null(names(self@provenance)) ||
            any(!nzchar(names(self@provenance))) ||
            anyDuplicated(names(self@provenance)))) {
        return("`provenance` must be a uniquely named list.")
    }
    NULL
}

# MappedHourlyClimateSeries retains one signal group after its native source
# year has been translated onto the target EPW hour rows.
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

# Validate one future-model year after all variable groups share the same EPW
# target grid while retaining their independent source metadata.
hourmap__member_error <- function(self) {
    if (length(self@sequence_id) != 1L ||
        is.na(self@sequence_id) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", self@sequence_id)) {
        return("`sequence_id` contains unsupported characters.")
    }
    if (length(self@weather_year) != 1L ||
        is.na(self@weather_year) ||
        self@weather_year < 1L) {
        return("`weather_year` must be one positive integer.")
    }
    if (length(self@source_calendar) != 1L ||
        is.na(self@source_calendar) ||
        !self@source_calendar %in% CF_TIME_CALENDARS) {
        return("`source_calendar` must identify one supported CF calendar.")
    }
    if (!length(self@series) ||
        !all(vapply(
            self@series,
            S7::S7_inherits,
            logical(1L),
            class = MappedHourlyClimateSeries
        ))) {
        return("`series` must contain MappedHourlyClimateSeries objects.")
    }
    group_ids <- vapply(
        self@series,
        function(series) series@group_id,
        character(1L)
    )
    if (anyDuplicated(group_ids)) {
        return("Mapped hourly group identities must be unique within a year.")
    }
    variables <- unlist(lapply(
        self@series,
        function(series) series@variables
    ), use.names = FALSE)
    if (anyDuplicated(variables)) {
        return("Each mapped hourly variable must occur in exactly one group.")
    }
    years <- unique(unlist(lapply(self@series, function(series) {
        as.integer(series@data[["year"]])
    }), use.names = FALSE))
    if (!identical(years, self@weather_year)) {
        return("Every mapped hourly row must match `weather_year`.")
    }
    if (length(self@provenance) &&
        (is.null(names(self@provenance)) ||
            any(!nzchar(names(self@provenance))) ||
            anyDuplicated(names(self@provenance)))) {
        return("`provenance` must be a uniquely named list.")
    }
    NULL
}

# MappedHourlyClimateMember groups every mapped climate variable for one
# source-model year before EPW physical fields are constructed.
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

# Validate the complete mapped sequence independently of the number of source
# years retained by the selected future-model period.
hourmap__sequence_error <- function(self) {
    if (!length(self@members) ||
        !all(vapply(
            self@members,
            S7::S7_inherits,
            logical(1L),
            class = MappedHourlyClimateMember
        ))) {
        return("`members` must contain MappedHourlyClimateMember objects.")
    }
    if (!identical(self@frequency, "hour") ||
        !identical(as.numeric(self@time_step_seconds), 3600)) {
        return("Mapped hourly climate sequences require a 3600-second hourly timestep.")
    }
    if (!identical(self@target_calendar, "epw_365_day")) {
        return("`target_calendar` must be `epw_365_day`.")
    }
    years <- vapply(
        self@members,
        function(member) member@weather_year,
        integer(1L)
    )
    if (anyDuplicated(years) || !identical(years, sort(years))) {
        return("Mapped hourly members must use unique ascending weather years.")
    }
    sequence_ids <- vapply(
        self@members,
        function(member) member@sequence_id,
        character(1L)
    )
    if (length(unique(sequence_ids)) != 1L) {
        return("Mapped hourly members must share one `sequence_id`.")
    }
    variable_sets <- lapply(self@members, function(member) {
        sort(unlist(lapply(
            member@series,
            function(series) series@variables
        ), use.names = FALSE))
    })
    if (length(variable_sets) > 1L &&
        !all(vapply(
            variable_sets[-1L],
            identical,
            logical(1L),
            variable_sets[[1L]]
        ))) {
        return("Every mapped hourly member must contain the same variables.")
    }
    if (length(self@provenance) &&
        (is.null(names(self@provenance)) ||
            any(!nzchar(names(self@provenance))) ||
            anyDuplicated(names(self@provenance)))) {
        return("`provenance` must be a uniquely named list.")
    }
    NULL
}

# MappedHourlyClimateSequence is the typed boundary consumed by later physical
# closure and output components for direct-model future years.
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

# Read and validate the fixed EPW target lattice without relying on the
# template year, which may be a conventional placeholder rather than a model year.
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
    if (!identical(as.integer(weather[["month"]]), expected_month) ||
        !identical(as.integer(weather[["day"]]), expected_day) ||
        !identical(as.integer(weather[["hour"]]), expected_hour)) {
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

# Integrate piecewise-constant source interval means over uniform target bins
# on normalized annual phase, preserving the annual mean for every CF calendar.
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

# Return the common 24-position daily lattice so calendar mapping changes only
# seasonal day position and never shifts a variable's time of day.
hourmap__daily_offsets <- function(data) {
    lattice <- temporal__daily_lattice(data[["cf_second_of_day"]])
    if (!isTRUE(lattice$regular)) {
        cli::cli_abort(
            "Mapped hourly variables must use one complete regular 24-position daily lattice."
        )
    }
    lattice$offsets
}

# Map point samples separately at each source time of day by circular annual
# phase, preventing 360/366-day conversion from drifting the diurnal cycle.
hourmap__circular_point_values <- function(data, target_days) {
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
    for (offset_index in seq_along(offsets)) {
        offset <- offsets[[offset_index]]
        source_rows <- which(data[["cf_second_of_day"]] == offset)
        target_rows <- seq.int(offset_index, target_count, by = 24L)
        source_phase <- as.numeric(data[["annual_phase"]][source_rows])
        source_value <- as.numeric(data[["value"]][source_rows])
        phase <- (
            seq.int(0L, target_days - 1L) + offset / 86400
        ) / target_days

        # Repeat one source year on each side so January and December use the
        # same circular chronology as all interior target days.
        mapped[target_rows] <- daily__circular_interpolate(
            source_phase,
            source_value,
            phase
        )
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

# Conservatively remap each time-of-day series across calendar days so
# interval means retain both their diurnal slot and normalized annual mean.
hourmap__conservative_interval_values <- function(data, target_days) {
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
    for (offset_index in seq_along(offsets)) {
        offset <- offsets[[offset_index]]
        source_rows <- which(data[["cf_second_of_day"]] == offset)
        target_rows <- seq.int(offset_index, target_count, by = 24L)
        mapped[target_rows] <- hourmap__conservative_interval_mean(
            data[["value"]][source_rows],
            target_days
        )
        target_phase[target_rows] <- (
            seq.int(0L, target_days - 1L) + offset / 86400
        ) / target_days
        source_second_of_day[target_rows] <- offset
    }
    list(
        value = mapped,
        target_phase = target_phase,
        source_second_of_day = source_second_of_day,
        hour_phase_seconds = offsets[[1L]] %% 3600
    )
}

# Map one adjusted variable and retain the explicit temporal semantics and
# numerical diagnostics required by later physical and output stages.
hourmap__variable <- function(data, variable, member, target) {
    data <- data.table::as.data.table(data.table::copy(data))
    data <- data[get("variable_id") == variable]
    data.table::setorderv(
        data,
        c("cf_day_of_year", "cf_second_of_day")
    )
    expected_samples <- cf_time__year_days(
        member@weather_year,
        member@calendar
    )[[1L]] * 24L
    if (nrow(data) != expected_samples) {
        cli::cli_abort(
            "Variable {.val {variable}} in weather year {member@weather_year} does not contain one complete hourly native-calendar year."
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

    output <- data.table::copy(target)
    data.table::set(
        output,
        j = "year",
        value = rep.int(member@weather_year, nrow(output))
    )
    data.table::set(
        output,
        j = "target_annual_phase",
        value = mapped$target_phase
    )
    data.table::set(
        output,
        j = "variable_id",
        value = rep.int(variable, nrow(output))
    )
    data.table::set(output, j = "value", value = mapped$value)
    data.table::set(output, j = "units", value = rep.int(units, nrow(output)))
    data.table::set(
        output,
        j = "mapping_method",
        value = rep.int(mapping_method, nrow(output))
    )
    data.table::set(
        output,
        j = "source_calendar",
        value = rep.int(member@calendar, nrow(output))
    )
    data.table::set(
        output,
        j = "source_year",
        value = rep.int(member@weather_year, nrow(output))
    )
    data.table::set(
        output,
        j = "source_second_of_day",
        value = mapped$source_second_of_day
    )
    data.table::set(
        output,
        j = "source_hour_phase_seconds",
        value = rep.int(mapped$hour_phase_seconds, nrow(output))
    )
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
        stringsAsFactors = FALSE
    )
    list(data = output[], diagnostic = diagnostic)
}

# Map every variable carried by one signal group without flattening its group
# identity or upstream signal provenance into the hourly table.
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

# Reconstruct all direct-model years on one fixed EPW target lattice while
# leaving physical variable conversion to the following component stage.
hourmap__reconstruct <- function(data, inputs, context, options) {
    if (!S7::S7_inherits(data, DirectModelSequence)) {
        cli::cli_abort(
            "`direct_model_epw_calendar_mapping` requires a DirectModelSequence object."
        )
    }
    if (!identical(data@frequency, "hour") ||
        !identical(as.numeric(data@time_step_seconds), 3600)) {
        cli::cli_abort(
            "`direct_model_epw_calendar_mapping` requires an hourly direct-model sequence with a 3600-second timestep."
        )
    }
    checkmate::assert_list(options, names = "unique")
    template_input <- weather__get_input(inputs, "weather_template")
    if (!S7::S7_inherits(template_input, WeatherInput) ||
        !identical(template_input@representation, "epw")) {
        cli::cli_abort(
            "Role `weather_template` must contain an EPW WeatherInput."
        )
    }
    target <- hourmap__target_grid(template_input@source)

    members <- lapply(data@members, function(member) {
        variables <- unlist(lapply(
            member@series,
            function(series) series@variables
        ), use.names = FALSE)
        if (anyDuplicated(variables)) {
            cli::cli_abort(
                "Weather year {member@weather_year} contains duplicate direct-model variable groups: {.val {unique(variables[duplicated(variables)])}}."
            )
        }
        series <- lapply(member@series, hourmap__series,
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

# Describe the reusable direct-model calendar bridge independently of any one
# bias-adjustment kernel, physical closure, or complete published recipe.
hourmap__component <- function() {
    component__spec(
        name = "direct_model_epw_calendar_mapping",
        stage = "hourly",
        label = "Direct-model EPW calendar mapping",
        required_inputs = list(
            weather_template = component__input_requirement(
                "weather_template",
                representations = "epw",
                frequencies = "hour",
                calendars = "gregorian"
            )
        ),
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

# Register the standalone hourly mapping once so later physical and output
# components can resolve it through the common component registry.
hourmap__register_component <- function() {
    component__register_builtin(hourmap__component())
    invisible(NULL)
}
