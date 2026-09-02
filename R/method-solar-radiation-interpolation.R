#' @include calendar-subdaily.R weather-solar.R
NULL

# Shortwave variables used by the published solar-angle temporal allocation.
# Both quantities are interval-mean fluxes in the CMIP6 three-hourly table.
SOLAR_RADIATION_VARIABLES <- c(
    "rsds",
    "rsdsdiff"
)

# The method provenance distinguishes the original interval-allocation method
# from its later use in the Wang et al. CMIP6 future-weather workflow.
SOLAR_RADIATION_REFERENCES <- c(
    "https://doi.org/10.5194/essd-7-157-2015",
    "https://doi.org/10.1038/s41467-023-41458-5"
)

# One-minute midpoint integration resolves sunrise and sunset within an hourly
# target while using the same samples for exact source-interval normalization.
SOLAR_INTEGRATION_STEP_SECONDS <- 60

# Validate one role as materialized, bounded shortwave interval means before
# any values are assigned to the hourly target lattice.
solar__source <- function(input, role) {
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
        "time",
        "time_bound_start",
        "time_bound_end",
        "lon",
        "lat"
    )
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "Role {.val {role}} is missing bounded-radiation column(s): {.val {missing}}."
        )
    }
    if (!nrow(data)) {
        cli::cli_abort(
            "Role {.val {role}} must contain sub-daily radiation intervals."
        )
    }
    for (column in c("time", "time_bound_start", "time_bound_end")) {
        value <- data[[column]]
        if (!inherits(value, "POSIXt") ||
            anyNA(value) ||
            any(!is.finite(as.numeric(value)))) {
            cli::cli_abort(
                "Role {.val {role}} must provide finite POSIX {.field {column}} values."
            )
        }
    }
    time_zone <- attr(data[["time"]], "tzone")
    if (is.null(time_zone) ||
        !length(time_zone) ||
        !time_zone[[1L]] %in% c("UTC", "GMT")) {
        cli::cli_abort(
            "Role {.val {role}} must express CMIP time coordinates in UTC."
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
    unsupported_variables <- setdiff(
        variables,
        SOLAR_RADIATION_VARIABLES
    )
    if (length(unsupported_variables)) {
        cli::cli_abort(
            "Role {.val {role}} contains variable(s) without supported shortwave interval-mean semantics: {.val {unsupported_variables}}."
        )
    }
    values <- as.numeric(data[["value"]])
    if (any(!is.finite(values)) || any(values < 0)) {
        cli::cli_abort(
            "Role {.val {role}} radiation values must be finite and non-negative."
        )
    }
    longitude <- as.numeric(data[["lon"]])
    latitude <- as.numeric(data[["lat"]])
    if (any(!is.finite(longitude)) ||
        any(longitude < -180 | longitude > 360) ||
        any(!is.finite(latitude)) ||
        any(latitude < -90 | latitude > 90)) {
        cli::cli_abort(
            "Role {.val {role}} must provide finite longitude and latitude within their geographic ranges."
        )
    }

    data.table::set(
        data,
        j = "cf_second_of_day",
        value = temporal__second_of_day(data, role)
    )
    list(data = data, frequencies = frequencies)
}

# Translate POSIX interval endpoints back onto the exact native CF chronology.
# The POSIX values are used only for elapsed seconds, never as Gregorian dates.
solar__native_intervals <- function(
    data,
    calendar,
    frequency,
    time_step_seconds,
    label
) {
    error <- bias__subdaily_data_error(
        as.data.frame(data),
        frequency,
        time_step_seconds
    )
    if (!is.null(error)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} is invalid: {error}"
        )
    }
    sample <- temporal__native_seconds(data, calendar)
    start <- sample + as.numeric(
        data[["time_bound_start"]] - data[["time"]],
        units = "secs"
    )
    end <- sample + as.numeric(
        data[["time_bound_end"]] - data[["time"]],
        units = "secs"
    )
    tolerance <- 1e-6
    if (any(abs(start - round(start)) > tolerance) ||
        any(abs(end - round(end)) > tolerance)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} must use whole-second CF interval bounds."
        )
    }
    start <- round(start)
    end <- round(end)
    duration <- end - start
    if (any(abs(duration - time_step_seconds) > tolerance)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} bounds do not match its declared source frequency."
        )
    }
    if (length(start) > 1L &&
        any(abs(start[-1L] - end[-length(end)]) > tolerance)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} contains gapped or overlapping source intervals."
        )
    }
    if (any(sample < start - tolerance | sample > end + tolerance)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} has a coordinate outside its CF time bounds."
        )
    }
    position <- (sample - start) / duration
    if (any(abs(position - position[[1L]]) > tolerance)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} must use one consistent coordinate position within its CF intervals."
        )
    }
    if (length(sample) > 1L) {
        native_elapsed <- diff(sample)
        posix_elapsed <- diff(as.numeric(data[["time"]]))
        if (any(abs(native_elapsed - time_step_seconds) > tolerance) ||
            any(abs(posix_elapsed - native_elapsed) > tolerance)) {
            cli::cli_abort(
                "Radiation interpolation group {.val {label}} has time coordinates inconsistent with its contiguous CF intervals."
            )
        }
    }

    list(
        sample = sample,
        start = start,
        end = end,
        duration = duration,
        position = position
    )
}

# Integrate the positive cosine of solar zenith over arbitrary native-calendar
# intervals. Native annual phase is mapped onto a 365-day solar cycle before
# Spencer's declination and equation-of-time series are evaluated in UTC.
solar__interval_projection <- function(
    start,
    end,
    calendar,
    latitude,
    longitude,
    integration_step_seconds = SOLAR_INTEGRATION_STEP_SECONDS
) {
    start <- as.numeric(start)
    end <- as.numeric(end)
    checkmate::assert_number(latitude, lower = -90, upper = 90, finite = TRUE)
    checkmate::assert_number(longitude, lower = -180, upper = 360, finite = TRUE)
    checkmate::assert_number(
        integration_step_seconds,
        lower = 1,
        finite = TRUE
    )
    if (!length(start) ||
        length(start) != length(end) ||
        any(!is.finite(start)) ||
        any(!is.finite(end)) ||
        any(end <= start)) {
        cli::cli_abort(
            "Solar projection requires matching finite intervals with increasing bounds."
        )
    }
    duration <- end - start
    samples <- duration / integration_step_seconds
    if (any(abs(samples - round(samples)) > 1e-6)) {
        cli::cli_abort(
            "Solar projection intervals must contain a whole number of integration steps."
        )
    }
    samples <- as.integer(round(samples))
    interval <- rep.int(seq_along(start), samples)
    midpoint <- rep.int(start, samples) +
        (sequence(samples) - 0.5) * integration_step_seconds
    position <- temporal__target_coordinates(midpoint, calendar)$coordinates

    # Mapping annual phase to a 365-day astronomical cycle lets 360-day,
    # no-leap, and all-leap model calendars share the same seasonal geometry.
    gamma <- 2 * pi * (position[["annual_phase"]] - 0.5 / 365)
    declination <- solar__spencer_declination(gamma)
    equation_of_time <- solar__spencer_equation_of_time(gamma)
    longitude <- ((longitude + 180) %% 360) - 180
    utc_minutes <- position[["cf_second_of_day"]] / 60
    apparent_solar_minutes <- utc_minutes + 4 * longitude + equation_of_time
    hour_angle <- solar__radians(apparent_solar_minutes / 4 - 180)
    latitude_radian <- solar__radians(latitude)

    # Positive cosine of zenith is the horizontal solar projection used in the
    # interval-allocation ratio; values below the horizon contribute zero.
    projection <- pmax(
        solar__cos_zenith(latitude_radian, declination, hour_angle),
        0
    )
    sums <- rowsum(projection, interval, reorder = FALSE)
    as.numeric(sums[, 1L]) / samples
}

# Format arbitrary native-second interval bounds without relying on the POSIX
# surrogate date assigned to a non-Gregorian calendar.
solar__native_labels <- function(seconds, calendar) {
    target <- temporal__target_coordinates(seconds, calendar)
    temporal__cf_time_label(target$coordinates)
}

# Allocate every source interval to hourly interval means and retain the exact
# source row, bounds, geometry, weight, and conservation result for inspection.
solar__group <- function(
    data,
    group_columns,
    frequency,
    time_step_seconds
) {
    data <- data.table::as.data.table(data.table::copy(data))
    data.table::set(
        data,
        j = ".solar_source_row",
        value = seq_len(nrow(data))
    )
    data.table::setorderv(data, "time_bound_start")
    label <- temporal__group_label(data, group_columns)
    calendar <- data[["cf_calendar"]][[1L]]
    interval <- solar__native_intervals(
        data,
        calendar,
        frequency,
        time_step_seconds,
        label
    )
    target_step <- 3600
    target_counts <- as.integer(interval$duration / target_step)
    source_index <- rep.int(seq_len(nrow(data)), target_counts)
    target_start <- rep.int(interval$start, target_counts) +
        (sequence(target_counts) - 1L) * target_step
    target_end <- target_start + target_step
    latitude <- unique(as.numeric(data[["lat"]]))
    longitude <- unique(as.numeric(data[["lon"]]))
    if (length(latitude) != 1L || length(longitude) != 1L) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} must identify one site coordinate."
        )
    }
    target_projection <- solar__interval_projection(
        target_start,
        target_end,
        calendar,
        latitude,
        longitude
    )
    source_projection <- solar__interval_projection(
        interval$start,
        interval$end,
        calendar,
        latitude,
        longitude
    )
    partition_projection <- as.numeric(rowsum(
        target_projection,
        source_index,
        reorder = FALSE
    )) / target_counts
    if (any(abs(source_projection - partition_projection) > 1e-12)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} produced inconsistent source and hourly solar integrations."
        )
    }

    source_value <- as.numeric(data[["value"]])
    unresolved <- partition_projection <= .Machine$double.eps &
        source_value > 0
    if (any(unresolved)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} contains positive shortwave radiation in a source interval with zero solar projection."
        )
    }
    solar_weight <- numeric(length(target_projection))
    daylight <- partition_projection[source_index] > .Machine$double.eps
    solar_weight[daylight] <- target_projection[daylight] /
        partition_projection[source_index[daylight]]
    value <- source_value[source_index] * solar_weight

    target_sample <- target_start +
        interval$position[source_index] * target_step
    rounded_sample <- round(target_sample)
    if (any(abs(target_sample - rounded_sample) > 1e-6)) {
        cli::cli_abort(
            "Radiation interpolation group {.val {label}} cannot preserve its coordinate position on a whole-second hourly lattice."
        )
    }
    target_sample <- rounded_sample
    target <- temporal__target_coordinates(target_sample, calendar)
    target_time <- as.POSIXct(
        as.numeric(data[["time"]][source_index]) +
            target_sample - interval$sample[source_index],
        origin = "1970-01-01",
        tz = "UTC"
    )
    target_bound_start <- as.POSIXct(
        as.numeric(data[["time"]][source_index]) +
            target_start - interval$sample[source_index],
        origin = "1970-01-01",
        tz = "UTC"
    )
    target_bound_end <- as.POSIXct(
        as.numeric(data[["time"]][source_index]) +
            target_end - interval$sample[source_index],
        origin = "1970-01-01",
        tz = "UTC"
    )
    output_group_columns <- setdiff(
        group_columns,
        c("cf_calendar", "frequency")
    )
    constant <- as.data.frame(data)[
        source_index,
        output_group_columns,
        drop = FALSE
    ]
    source_labels <- temporal__cf_time_label(data)
    source_start_labels <- solar__native_labels(interval$start, calendar)
    source_end_labels <- solar__native_labels(interval$end, calendar)
    out <- data.table::as.data.table(cbind(
        constant,
        data.frame(
            value = value,
            frequency = rep.int("hour", length(value)),
            time = target_time,
            time_bound_start = target_bound_start,
            time_bound_end = target_bound_end,
            target$coordinates,
            source_frequency = rep.int(frequency, length(value)),
            source_time = source_labels[source_index],
            source_interval_start = source_start_labels[source_index],
            source_interval_end = source_end_labels[source_index],
            source_row = data[[".solar_source_row"]][source_index],
            solar_projection = target_projection,
            source_solar_projection = source_projection[source_index],
            solar_weight = solar_weight,
            temporal_interpolation = rep.int(
                "solar_projection",
                length(value)
            ),
            stringsAsFactors = FALSE
        )
    ))

    # Compatibility aliases describe the target coordinate rather than either
    # edge of the hourly averaging interval.
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

    reconstructed <- as.numeric(rowsum(
        value,
        source_index,
        reorder = FALSE
    )) / target_counts
    conservation_error <- reconstructed - source_value
    diagnostic <- data.table::data.table(
        group = label,
        variable_id = data[["variable_id"]][[1L]],
        source_frequency = frequency,
        source_step_seconds = as.numeric(time_step_seconds),
        target_frequency = "hour",
        target_step_seconds = as.numeric(target_step),
        source_samples = nrow(data),
        target_samples = nrow(out),
        coordinate_position = interval$position[[1L]],
        zero_solar_source_intervals = sum(
            partition_projection <= .Machine$double.eps
        ),
        maximum_conservation_error = max(abs(conservation_error)),
        maximum_projection_partition_error = max(abs(
            source_projection - partition_projection
        )),
        source_start = source_start_labels[[1L]],
        source_end = source_end_labels[[length(source_end_labels)]],
        target_start = solar__native_labels(target_start[[1L]], calendar),
        target_end = solar__native_labels(
            target_end[[length(target_end)]],
            calendar
        ),
        interval_policy = "cf_time_bounds",
        conservation_policy = "source_interval_mean"
    )
    list(data = out[], diagnostic = diagnostic)
}

# Interpolate every independent radiation group in one semantic role and
# rebuild its WeatherInput descriptor with retained CF interval bounds.
solar__role <- function(input, role, context) {
    source <- solar__source(input, role)
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
        frequency <- unique(as.character(group[["frequency"]]))
        if (length(frequency) != 1L) {
            cli::cli_abort(
                "Each solar radiation interpolation group must contain one source frequency."
            )
        }
        solar__group(
            group,
            group_columns = group_columns,
            frequency = frequency,
            time_step_seconds = unname(
                TEMPORAL_SOURCE_STEPS[[frequency]]
            )
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
        method = "solar_radiation_interpolation",
        references = SOLAR_RADIATION_REFERENCES,
        source_frequencies = sort(source$frequencies),
        published_source_frequency = "3hr",
        adapted_source_frequencies = "6hr",
        source_step_seconds = unname(
            TEMPORAL_SOURCE_STEPS[sort(source$frequencies)]
        ),
        target_frequency = "hour",
        target_step_seconds = 3600,
        interval_policy = "cf_time_bounds",
        conservation_policy = "source_interval_mean",
        solar_geometry = "spencer_positive_zenith_projection",
        integration_step_seconds = SOLAR_INTEGRATION_STEP_SECONDS,
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
                time_step_seconds = 3600,
                interval_bounds = c(
                    "time_bound_start",
                    "time_bound_end"
                )
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

# Apply solar-projection interpolation to matching historical and future model
# roles while preserving template and observational inputs unchanged.
solar__apply <- function(inputs, context, options) {
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    checkmate::assert_list(options, names = "unique")
    if (length(options)) {
        cli::cli_abort(
            "`solar_radiation_interpolation` does not accept component options."
        )
    }
    roles <- c("model_historical", "model_future")
    results <- lapply(roles, function(role) {
        solar__role(
            weather__get_input(inputs, role),
            role,
            context
        )
    })
    names(results) <- roles
    output_inputs <- weather__new_inputs(
        weather_template = weather__get_input(inputs, "weather_template"),
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
        method = "solar_radiation_interpolation",
        references = SOLAR_RADIATION_REFERENCES,
        roles = roles,
        source_frequencies = lapply(
            results,
            function(result) result$provenance$source_frequencies
        ),
        published_source_frequency = "3hr",
        adapted_source_frequencies = "6hr",
        source_step_seconds = lapply(
            results,
            function(result) result$provenance$source_step_seconds
        ),
        target_frequency = "hour",
        target_step_seconds = 3600,
        interval_policy = "cf_time_bounds",
        conservation_policy = "source_interval_mean",
        solar_geometry = "spencer_positive_zenith_projection",
        integration_step_seconds = SOLAR_INTEGRATION_STEP_SECONDS
    )
    WeatherStageResult(
        stage = "preprocess",
        component = "solar_radiation_interpolation",
        kind = "hourly_role_inputs",
        value = output_inputs,
        diagnostics = list(solar_radiation_interpolation = diagnostics),
        provenance = provenance,
        metadata = list(
            interpolation = "solar_projection",
            interval_conservation = "source_mean"
        )
    )
}

# Describe the reusable interval-mean radiation interpolation component without
# coupling it to a complete future-weather recipe.
solar__component <- function() {
    variables <- lapply(SOLAR_RADIATION_VARIABLES, identity)
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
        name = "solar_radiation_interpolation",
        stage = "preprocess",
        label = "Solar radiation interpolation",
        required_inputs = list(
            model_historical = requirement("model_historical"),
            model_future = requirement("model_future")
        ),
        input_kinds = "role_inputs",
        output_kinds = "hourly_role_inputs",
        scopes = "univariate",
        stochastic = FALSE,
        operations = list(apply = solar__apply),
        metadata = list(
            algorithm = "source_interval_mean_scaled_by_solar_projection",
            references = SOLAR_RADIATION_REFERENCES,
            source_frequencies = names(TEMPORAL_SOURCE_STEPS),
            published_source_frequency = "3hr",
            adapted_source_frequencies = "6hr",
            target_frequency = "hour",
            target_step_seconds = 3600,
            supported_variables = SOLAR_RADIATION_VARIABLES,
            interval_policy = "cf_time_bounds",
            conservation_policy = "source_interval_mean",
            solar_geometry = "spencer_positive_zenith_projection",
            integration_step_seconds = SOLAR_INTEGRATION_STEP_SECONDS
        )
    )
}

# Register the solar radiation preprocess implementation once for recipe
# compilation and standalone component inspection.
solar__register_component <- function() {
    component__register_builtin(solar__component())
    invisible(NULL)
}
