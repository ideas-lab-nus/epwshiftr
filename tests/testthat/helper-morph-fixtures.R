# Shared synthetic climate and context fixtures for morphing backend tests.
# Build deterministic daily CMIP climate rows across daily temperature, BTWS,
# Ek, quantile-mapping morphing, Sobie-Curry, and daily-adjustment tests.
# morph_test__daily_climate {{{
morph_test__daily_climate <- function(
    years,
    period,
    experiment,
    variables = c("tas", "tasmin", "tasmax"),
    calendar_days = 365L,
    frequency = "day",
    mean_shift = 0,
    minimum_shift = 0,
    maximum_shift = 0,
    base_temp = 20,
    seasonal_amp = 7,
    replicate_step = 0,
    dtr_base = NULL,
    dtr_ratio = 1,
    dtr_shift = 0,
    relative_humidity_ratio = 1,
    pressure_shift = 0,
    omit = character(),
    source_id = "EC-Earth3",
    institution_id = "EC-Earth-Consortium",
    lon = 104,
    lat = 1.37,
    cf_noleap = FALSE,
    include_month = FALSE
) {
    years <- as.integer(years)
    calendar_days <- as.integer(calendar_days)
    selected <- setdiff(variables, omit)
    activity_id <- if (identical(experiment, "historical")) {
        "CMIP"
    } else {
        "ScenarioMIP"
    }
    units_map <- c(
        tas = "K",
        tasmin = "K",
        tasmax = "K",
        huss = "kg kg-1",
        ps = "Pa"
    )

    rows <- lapply(seq_along(years), function(index) {
        year <- years[[index]]
        replicate_offset <- (index - mean(seq_along(years))) * replicate_step
        coordinates <- NULL
        month <- NULL

        if (isTRUE(cf_noleap)) {
            fields <- cf_time_offset2date(
                seq.int(0L, calendar_days - 1L),
                data.frame(year = year, month = 1L, day = 1L),
                "noleap"
            )
            fields$hour <- 12L
            fields$minute <- 0L
            fields$second <- 0
            coordinates <- cf_time__coordinates(fields, "noleap")
            phase <- coordinates$annual_phase
            time <- as.POSIXct(
                sprintf(
                    "%04d-%02d-%02d 12:00:00",
                    fields$year,
                    fields$month,
                    fields$day
                ),
                tz = "UTC"
            )
        } else if (isTRUE(include_month)) {
            dates <- seq.Date(
                as.Date(sprintf("%04d-01-01", year)),
                as.Date(sprintf("%04d-12-31", year)),
                by = "day"
            )
            dates <- dates[format(dates, "%m-%d") != "02-29"]
            phase <- (seq_along(dates) - 0.5) / length(dates)
            time <- as.POSIXct(dates, tz = "UTC") + 12 * 3600
            month <- as.integer(format(dates, "%m"))
        } else {
            phase <- (seq_len(calendar_days) - 0.5) / calendar_days
            time <- as.POSIXct(
                sprintf("%04d-01-01 12:00:00", year),
                tz = "UTC"
            ) +
                seq.int(0L, calendar_days - 1L) * 86400
        }

        seasonal <- seasonal_amp * sin(2 * pi * phase)
        mean_temperature <- base_temp +
            seasonal +
            replicate_offset +
            mean_shift
        if (is.null(dtr_base)) {
            minimum <- base_temp + seasonal - 4 + minimum_shift
            maximum <- base_temp + seasonal + 5 + maximum_shift
        } else {
            baseline_dtr <- if (identical(dtr_base, 0)) {
                rep.int(0, length(phase))
            } else {
                dtr_base + 1.5 * cos(2 * pi * phase)
            }
            dtr <- baseline_dtr * dtr_ratio + dtr_shift
            minimum <- mean_temperature - dtr / 2
            maximum <- mean_temperature + dtr / 2
        }

        values <- list(
            tas = mean_temperature + 273.15,
            tasmin = minimum + 273.15,
            tasmax = maximum + 273.15
        )
        if (any(c("huss", "ps") %in% selected)) {
            pressure <- 100000 + 300 * cos(2 * pi * phase) + pressure_shift
            relative_humidity <- (62 +
                8 * cos(2 * pi * phase) +
                replicate_offset) *
                relative_humidity_ratio
            values$huss <- epwphys__huss_from_rh_si(
                mean_temperature,
                relative_humidity,
                pressure
            )
            values$ps <- pressure
        }

        data.table::rbindlist(lapply(selected, function(variable_id) {
            dt <- data.table::data.table(
                activity_id = activity_id,
                institution_id = institution_id,
                source_id = source_id,
                experiment_id = experiment,
                variant_label = "r1i1p1f1",
                frequency = frequency,
                table_id = "day",
                variable_id = variable_id,
                time = time,
                year = year,
                annual_phase = phase,
                period = period,
                lon = lon,
                lat = lat,
                units = units_map[[variable_id]],
                value = values[[variable_id]]
            )
            if (isTRUE(include_month)) {
                data.table::set(dt, j = "month", value = month)
            }
            if (isTRUE(cf_noleap)) {
                data.table::set(dt, j = "grid_label", value = "gn")
                dt <- cbind(
                    dt[, setdiff(names(dt), "annual_phase"), with = FALSE],
                    coordinates
                )
            }
            dt
        }))
    })
    data.table::rbindlist(rows, use.names = TRUE)
}
# }}}

# Build deterministic monthly CMIP6 rows for BWS/BTWS tests.
# morph_test__monthly_climate {{{
morph_test__monthly_climate <- function(
    years,
    period,
    experiment,
    mean_shift = rep(0, 12L),
    minimum_shift = rep(0, 12L),
    maximum_shift = rep(0, 12L),
    radiation_shift = rep(0, 12L),
    cloud_scale = rep(0, 12L),
    include_extrema = TRUE,
    frequency = "mon"
) {
    years <- as.integer(years)
    shifts <- list(
        tas = as.numeric(mean_shift),
        tasmin = as.numeric(minimum_shift),
        tasmax = as.numeric(maximum_shift),
        rsds = as.numeric(radiation_shift),
        clt = as.numeric(cloud_scale)
    )
    stopifnot(all(lengths(shifts) == 12L))
    units_map <- c(
        tas = "K",
        tasmin = "K",
        tasmax = "K",
        rsds = "W m-2",
        clt = "%"
    )

    data.table::rbindlist(lapply(years, function(year) {
        month <- seq_len(12L)
        time <- as.POSIXct(
            sprintf("%04d-%02d-15 12:00:00", year, month),
            tz = "UTC"
        )
        phase <- (month - 0.5) / 12
        seasonal <- 7 * sin(2 * pi * phase)
        values <- list(
            tas = 20 + seasonal,
            tasmin = 16 + seasonal,
            tasmax = 25 + seasonal,
            rsds = 180 + 40 * sin(2 * pi * phase - pi / 2),
            clt = 55 + 15 * sin(2 * pi * phase + pi / 4)
        )
        if (!isTRUE(include_extrema)) {
            values <- values["tas"]
        }
        data.table::rbindlist(lapply(names(values), function(variable_id) {
            data.table::data.table(
                activity_id = if (identical(experiment, "historical")) {
                    "CMIP"
                } else {
                    "ScenarioMIP"
                },
                institution_id = "EC-Earth-Consortium",
                source_id = "EC-Earth3",
                experiment_id = experiment,
                variant_label = "r1i1p1f1",
                grid_label = "gn",
                frequency = frequency,
                table_id = if (identical(frequency, "mon")) "Amon" else "day",
                variable_id = variable_id,
                time = time,
                year = year,
                period = period,
                lon = 104,
                lat = 1.37,
                units = units_map[[variable_id]],
                value = if (identical(variable_id, "clt")) {
                    values[[variable_id]] * (1 + shifts[[variable_id]][month])
                } else {
                    values[[variable_id]] +
                        shifts[[variable_id]][month] +
                        if (variable_id %in% c("tas", "tasmin", "tasmax")) {
                            273.15
                        } else {
                            0
                        }
                }
            )
        }))
    }))
}
# }}}

# Build a canonical morphing context around the packaged EPW fixture.
# morph_test__context {{{
morph_test__context <- function(
    climate,
    recipe,
    reference_climate = NULL,
    observed_reference = NULL,
    epw = get_cached_epw()
) {
    morpher__context(
        epw = epw,
        climate = climate,
        reference_climate = reference_climate,
        observed_reference = observed_reference,
        recipe = recipe
    )
}
# }}}

# Return a unit-free data.table copy of a context's baseline EPW weather rows.
# morph_test__baseline_weather {{{
morph_test__baseline_weather <- function(context) {
    baseline <- context$epw$clone()
    suppressMessages(baseline$drop_unit())
    data.table::as.data.table(baseline$data())
}
# }}}

# vim: fdm=marker :
