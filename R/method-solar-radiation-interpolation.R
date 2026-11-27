#' @include calendar.R epw-physics.R
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

# Validate one role as materialized, bounded radiation interval means before
# any values are assigned to the hourly target lattice.
# solar__source {{{
solar__source <- function(
    input,
    role,
    allowed_variables = SOLAR_RADIATION_VARIABLES
) {
    source <- temporal__validate_source_role(
        input,
        role,
        allowed_variables = allowed_variables,
        extra_columns = c("time_bound_start", "time_bound_end", "lon", "lat"),
        time_columns = c("time", "time_bound_start", "time_bound_end"),
        require_utc = TRUE,
        missing_label = "bounded-radiation",
        sample_label = "radiation intervals",
        variable_semantics = "supported shortwave interval-mean semantics"
    )
    values <- as.numeric(source$data[["value"]])
    if (any(!is.finite(values)) || any(values < 0)) {
        cli::cli_abort(
            "Role {.val {role}} radiation values must be finite and non-negative."
        )
    }
    longitude <- as.numeric(source$data[["lon"]])
    latitude <- as.numeric(source$data[["lat"]])
    if (
        any(!is.finite(longitude)) ||
            any(longitude < -180 | longitude > 360) ||
            any(!is.finite(latitude)) ||
            any(latitude < -90 | latitude > 90)
    ) {
        cli::cli_abort(
            "Role {.val {role}} must provide finite longitude and latitude within their geographic ranges."
        )
    }
    source
}
# }}}

# Translate POSIX interval endpoints back onto the exact native CF chronology.
# The POSIX values are used only for elapsed seconds, never as Gregorian dates.
# solar__native_intervals {{{
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
    start <- sample +
        as.numeric(
            data[["time_bound_start"]] - data[["time"]],
            units = "secs"
        )
    end <- sample +
        as.numeric(
            data[["time_bound_end"]] - data[["time"]],
            units = "secs"
        )
    tolerance <- 1e-6
    if (
        any(abs(start - round(start)) > tolerance) ||
            any(abs(end - round(end)) > tolerance)
    ) {
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
    if (
        length(start) > 1L &&
            any(abs(start[-1L] - end[-length(end)]) > tolerance)
    ) {
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
        if (
            any(abs(native_elapsed - time_step_seconds) > tolerance) ||
                any(abs(posix_elapsed - native_elapsed) > tolerance)
        ) {
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
# }}}

# Integrate the positive cosine of solar zenith over arbitrary native-calendar
# intervals. Native annual phase is mapped onto a 365-day solar cycle before
# Spencer's declination and equation-of-time series are evaluated in UTC.
# solar__interval_projection {{{
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
    checkmate::assert_number(
        longitude,
        lower = -180,
        upper = 360,
        finite = TRUE
    )
    checkmate::assert_number(
        integration_step_seconds,
        lower = 1,
        finite = TRUE
    )
    if (
        !length(start) ||
            length(start) != length(end) ||
            any(!is.finite(start)) ||
            any(!is.finite(end)) ||
            any(end <= start)
    ) {
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
# }}}

# Format arbitrary native-second interval bounds without relying on the POSIX
# surrogate date assigned to a non-Gregorian calendar.
# solar__native_labels {{{
solar__native_labels <- function(seconds, calendar) {
    target <- temporal__target_coordinates(seconds, calendar)
    temporal__cf_time_label(target$coordinates)
}
# }}}

# Allocate every source interval to hourly interval means and retain the exact
# source row, bounds, geometry, weight, and conservation result for inspection.
# solar__group {{{
solar__group <- function(
    data,
    group_columns,
    frequency,
    time_step_seconds,
    allocation = "solar_projection"
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
    if (allocation == "constant_interval_mean") {
        return(solar__longwave_overlap(
            data,
            group_columns,
            frequency,
            interval,
            calendar,
            label
        ))
    }
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
    )) /
        target_counts
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
            target_sample -
            interval$sample[source_index],
        origin = "1970-01-01",
        tz = "UTC"
    )
    target_bound_start <- as.POSIXct(
        as.numeric(data[["time"]][source_index]) +
            target_start -
            interval$sample[source_index],
        origin = "1970-01-01",
        tz = "UTC"
    )
    target_bound_end <- as.POSIXct(
        as.numeric(data[["time"]][source_index]) +
            target_end -
            interval$sample[source_index],
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
                allocation,
                length(value)
            ),
            stringsAsFactors = FALSE
        )
    ))
    # Compatibility aliases describe the target coordinate rather than either
    # edge of the hourly averaging interval.
    temporal__set_time_aliases(out, names(data), target$fields, target_time)

    reconstructed <- as.numeric(rowsum(
        value,
        source_index,
        reorder = FALSE
    )) /
        target_counts
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
# }}}

# Integrate piecewise-constant longwave flux over clock-hour intervals. An
# interval may overlap two source cells when CF bounds start between hours;
# partial edge hours are omitted rather than filled from outside the source.
solar__longwave_overlap <- function(
    data,
    group_columns,
    frequency,
    interval,
    calendar,
    label
) {
    start <- ceiling(min(interval$start) / 3600) * 3600
    last <- floor(max(interval$end) / 3600) * 3600 - 3600
    if (last < start) {
        cli::cli_abort("Longwave input contains no complete clock hour.")
    }
    target_start <- seq(start, last, by = 3600)
    target_end <- target_start + 3600
    index <- findInterval(target_start, interval$start)
    next_index <- pmin(index + 1L, nrow(data))
    first_seconds <- pmin(interval$end[index], target_end) - target_start
    second_seconds <- 3600 - first_seconds
    # Flux times overlap duration is energy per area; divide once by 3600 s.
    value <- (data$value[index] *
        first_seconds +
        data$value[next_index] * second_seconds) /
        3600
    target <- temporal__target_coordinates(target_start, calendar)
    time <- as.POSIXct(
        as.numeric(data$time[index]) + target_start - interval$sample[index],
        origin = "1970-01-01",
        tz = "UTC"
    )
    constant <- as.data.frame(data)[
        index,
        setdiff(group_columns, c("cf_calendar", "frequency")),
        drop = FALSE
    ]
    out <- data.table::as.data.table(cbind(
        constant,
        data.frame(
            value = value,
            frequency = "hour",
            time = time,
            time_bound_start = time,
            time_bound_end = time + 3600,
            target$coordinates,
            source_frequency = frequency,
            source_row = data$.solar_source_row[index],
            second_source_row = data$.solar_source_row[next_index],
            first_overlap_seconds = first_seconds,
            second_overlap_seconds = second_seconds,
            temporal_interpolation = "constant_interval_mean"
        )
    ))
    temporal__set_time_aliases(out, names(data), target$fields, time)
    covered <- pmax(
        0,
        pmin(interval$end, max(target_end)) -
            pmax(interval$start, min(target_start))
    )
    energy_error <- abs(sum(value * 3600) - sum(data$value * covered))
    if (energy_error > max(1e-6, abs(sum(data$value * covered)) * 1e-12)) {
        cli::cli_abort(
            "Longwave clock-hour overlap integration does not conserve covered energy."
        )
    }
    list(
        data = out[],
        diagnostic = data.table::data.table(
            group = label,
            variable_id = "rlds",
            source_frequency = frequency,
            source_samples = nrow(data),
            target_samples = nrow(out),
            maximum_conservation_error = energy_error / sum(covered),
            omitted_edge_seconds = sum(interval$duration) - sum(covered),
            interval_policy = "cf_time_bounds_clock_hour_overlap",
            conservation_policy = "covered_interval_energy"
        )
    )
}

# Interpolate every independent radiation group in one semantic role and
# rebuild its WeatherInput descriptor with retained CF interval bounds.
# solar__role {{{
solar__role <- function(input, role, context, longwave = FALSE) {
    source <- solar__source(
        input,
        role,
        if (longwave) "rlds" else SOLAR_RADIATION_VARIABLES
    )
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
            ),
            allocation = if (longwave) {
                "constant_interval_mean"
            } else {
                "solar_projection"
            }
        )
    })
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
        integration_step_seconds = SOLAR_INTEGRATION_STEP_SECONDS
    )
    if (longwave) {
        interpolation_record$method <- "constant_interval_mean"
        interpolation_record$evidence <- "experimental_package_extension"
        interpolation_record$solar_geometry <- NULL
        interpolation_record$integration_step_seconds <- NULL
        interpolation_record$references <- character()
        interpolation_record$interval_policy <- "cf_time_bounds_clock_hour_overlap"
        interpolation_record$conservation_policy <- "covered_interval_energy"
    }
    temporal__finalize_role(
        input,
        role,
        results,
        group_columns,
        interpolation_record,
        extra_metadata = list(
            interval_bounds = c("time_bound_start", "time_bound_end")
        )
    )
}
# }}}

# Apply solar-projection interpolation to matching historical and future model
# roles while preserving template and observational inputs unchanged.
# solar__apply {{{
solar__apply <- function(inputs, context, options) {
    temporal__apply_model_roles(
        inputs,
        context,
        options,
        component_name = "solar_radiation_interpolation",
        role_fn = solar__role,
        diagnostic_name = "solar_radiation_interpolation",
        extra_provenance = list(
            references = SOLAR_RADIATION_REFERENCES,
            published_source_frequency = "3hr",
            adapted_source_frequencies = "6hr",
            interval_policy = "cf_time_bounds",
            conservation_policy = "source_interval_mean",
            solar_geometry = "spencer_positive_zenith_projection",
            integration_step_seconds = SOLAR_INTEGRATION_STEP_SECONDS
        ),
        metadata = list(
            interpolation = "solar_projection",
            interval_conservation = "source_mean"
        )
    )
}
# }}}

# Describe the reusable interval-mean radiation interpolation component without
# coupling it to a complete future-weather recipe.
# solar__component {{{
solar__component <- function() {
    temporal__preprocess_component(
        name = "solar_radiation_interpolation",
        label = "Solar radiation interpolation",
        supported_variables = SOLAR_RADIATION_VARIABLES,
        apply_fn = solar__apply,
        metadata = list(
            algorithm = "source_interval_mean_scaled_by_solar_projection",
            references = SOLAR_RADIATION_REFERENCES,
            published_source_frequency = "3hr",
            adapted_source_frequencies = "6hr",
            interval_policy = "cf_time_bounds",
            conservation_policy = "source_interval_mean",
            solar_geometry = "spencer_positive_zenith_projection",
            integration_step_seconds = SOLAR_INTEGRATION_STEP_SECONDS
        )
    )
}
# }}}

# Register the solar radiation preprocess implementation once for recipe
# compilation and standalone component inspection.
# solar__register_component {{{
solar__register_component <- function() {
    component__register_builtin(solar__component())
    invisible(NULL)
}
# }}}

# vim: fdm=marker :
