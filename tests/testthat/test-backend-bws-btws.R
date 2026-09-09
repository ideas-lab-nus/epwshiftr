# Build deterministic monthly CMIP6 rows whose future changes vary by calendar
# month and whose temperature extrema retain their monthly CMIP definitions.
bws_btws_monthly_test__climate <- function(
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
                units = switch(
                    variable_id,
                    tas = "K",
                    tasmin = "K",
                    tasmax = "K",
                    rsds = "W m-2",
                    clt = "%"
                ),
                value = if (identical(variable_id, "clt")) {
                    values[[variable_id]] *
                        (1 + shifts[[variable_id]][month])
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

# Assemble one direct context for the complete registered BWS/BTWS recipe.
bws_btws_monthly_test__context <- function(
    mean_shift = seq(0.5, 1.6, by = 0.1),
    minimum_shift = seq(0.3, 1.4, by = 0.1),
    maximum_shift = seq(0.7, 1.8, by = 0.1),
    radiation_shift = rep(1, 12L),
    cloud_scale = rep(0.02, 12L),
    recipe_name = "bws_btws_monthly"
) {
    historical <- bws_btws_monthly_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical"
    )
    future <- bws_btws_monthly_test__climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        mean_shift = mean_shift,
        minimum_shift = minimum_shift,
        maximum_shift = maximum_shift,
        radiation_shift = radiation_shift,
        cloud_scale = cloud_scale
    )
    morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        recipe = epw_morph_recipe(recipe_name, policy = "harmonized")
    )
}

test_that("BWS/BTWS monthly sources produce 12 month-constant target sets", {
    mean_shift <- seq(0.5, 1.6, by = 0.1)
    minimum_shift <- seq(0.3, 1.4, by = 0.1)
    maximum_shift <- seq(0.7, 1.8, by = 0.1)
    radiation_shift <- seq(0.2, 1.3, by = 0.1)
    cloud_scale <- seq(-0.02, 0.035, by = 0.005)
    historical <- bws_btws_monthly_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical"
    )
    future <- bws_btws_monthly_test__climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        mean_shift = mean_shift,
        minimum_shift = minimum_shift,
        maximum_shift = maximum_shift,
        radiation_shift = radiation_shift,
        cloud_scale = cloud_scale
    )

    targets <- bws_btws__monthly_targets(future, historical)
    temperature <- targets$temperature

    expect_identical(nrow(targets$monthly), 12L)
    expect_identical(nrow(temperature), 365L)
    expect_identical(sort(unique(temperature$month)), seq_len(12L))
    expect_true(all(temperature$dtr_status == "adjusted"))
    expect_equal(
        temperature$mean_delta,
        mean_shift[temperature$month],
        tolerance = 1e-12
    )
    expect_equal(
        temperature$minimum_delta,
        minimum_shift[temperature$month],
        tolerance = 1e-12
    )
    expect_equal(
        temperature$maximum_delta,
        maximum_shift[temperature$month],
        tolerance = 1e-12
    )
    expect_equal(
        temperature$dtr_delta,
        (maximum_shift - minimum_shift)[temperature$month],
        tolerance = 1e-12
    )
    expect_true(all(
        temperature[, data.table::uniqueN(mean_delta), by = "month"]$V1 == 1L
    ))
    expect_equal(
        targets$monthly$rsds_delta,
        radiation_shift,
        tolerance = 1e-12
    )
    expect_equal(
        targets$monthly$clt_scale,
        cloud_scale,
        tolerance = 1e-12
    )
})

test_that("BWS/BTWS climatology pools years by native calendar month", {
    source <- bws_btws_monthly_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical"
    )
    source[, cf_month := as.integer(format(time, "%m"))]
    baseline <- bws_btws__monthly_climatology(
        source,
        "historical climate"
    )

    time_scrambled <- data.table::copy(source)
    time_scrambled[, time := as.POSIXct("2001-01-15", tz = "UTC")]
    mapped <- bws_btws__monthly_climatology(
        time_scrambled,
        "historical climate"
    )

    expect_equal(mapped$climatology, baseline$climatology, tolerance = 0)
    expect_equal(mapped$n, baseline$n, tolerance = 0)
    expect_true(all(mapped$n == 2L))
})

test_that("BWS/BTWS recipe exposes the complete weather boundary", {
    expect_true("bws_btws_monthly" %in% epw_morph_backends())
    expect_true(
        "bws_btws_monthly" %in% epw_morph_recipes()[["name"]]
    )

    backend <- epw_morph_backend("bws_btws_monthly")
    recipe <- epw_morph_recipe("bws_btws_monthly")
    spec <- epw_morph_recipe_spec("bws_btws_monthly")

    expect_true(backend$requires_reference)
    expect_equal(
        backend$required_variables(),
        c("tas", "tasmin", "tasmax", "rsds", "clt")
    )
    expect_identical(recipe$policy, "harmonized")
    expect_identical(
        recipe$components$signal,
        "bws_btws_monthly_changes"
    )
    expect_identical(
        recipe$components$sequence,
        "bws_btws_preserve_epw_sequence"
    )
    expect_identical(
        recipe$components$hourly,
        "bws_btws_hourly_projection"
    )
    expect_identical(
        recipe$components$output,
        "bws_btws_epw_result"
    )
    expect_identical(spec@source$type, "adapted_publication")
    expect_match(spec@source$signal_note, "monthly CMIP6")
    expect_match(spec@source$signal_note, "does not apply daily-varying")
    expect_match(spec@source$implementation_note, "cloud cover")
    expect_identical(
        spec@physical_policies[["harmonized"]],
        "bws_btws_weather"
    )
    expect_identical(
        spec@calendar_policy,
        "cf_calendar_month_to_epw_365"
    )
    expect_identical(
        morpher__recipe_required_frequency(recipe),
        "mon"
    )
    expect_error(
        transform__validate_execution_inputs(monthly_transform("bws_btws")),
        "requires.*reference"
    )
})

test_that("BWS/BTWS closes temperature, radiation, and cloud", {
    mean_shift <- seq(0.5, 1.6, by = 0.1)
    minimum_shift <- seq(0.3, 1.4, by = 0.1)
    maximum_shift <- seq(0.7, 1.8, by = 0.1)
    context <- bws_btws_monthly_test__context(
        mean_shift,
        minimum_shift,
        maximum_shift
    )
    baseline <- context$epw$clone()
    suppressMessages(baseline$drop_unit())
    baseline_data <- data.table::as.data.table(baseline$data())

    result <- morpher__run_context(context)
    weather <- result$data

    expect_s3_class(result, "epw_morph_result")
    expect_identical(nrow(weather), 8760L)
    expect_identical(nrow(result$factors), 365L)
    expect_true(all(result$parts$component_pipeline$status == "ok"))
    expect_identical(
        result$parts$component_pipeline$component[
            result$parts$component_pipeline$stage == "signal"
        ],
        "bws_btws_monthly_changes"
    )
    expect_identical(nrow(result$parts$bws_factors), 24L)
    expect_identical(
        sort(unique(result$parts$bws_factors$variable_id)),
        c("clt", "rsds")
    )
    expect_lt(max(abs(result$factors$mean_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$minimum_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$maximum_closure_error)), 1e-8)
    expect_false(any(!is.na(result$factors$btws_fallback_reason)))

    achieved <- weather[, .(
        baseline_mean = mean(baseline_data$dry_bulb_temperature[.I]),
        baseline_minimum = min(baseline_data$dry_bulb_temperature[.I]),
        baseline_maximum = max(baseline_data$dry_bulb_temperature[.I]),
        projected_mean = mean(dry_bulb_temperature),
        projected_minimum = min(dry_bulb_temperature),
        projected_maximum = max(dry_bulb_temperature)
    ), by = c("daily_target_day")]
    month <- result$factors$month
    expect_equal(
        achieved$projected_mean - achieved$baseline_mean,
        mean_shift[month],
        tolerance = 1e-8
    )
    expect_equal(
        achieved$projected_minimum - achieved$baseline_minimum,
        minimum_shift[month],
        tolerance = 1e-8
    )
    expect_equal(
        achieved$projected_maximum - achieved$baseline_maximum,
        maximum_shift[month],
        tolerance = 1e-8
    )

    baseline_month <- baseline_data[, .(
        ghi_mean = mean(global_horizontal_radiation),
        ghi_maximum = max(global_horizontal_radiation),
        cloud_mean = mean(total_sky_cover)
    ), by = "month"]
    projected_month <- weather[, .(
        ghi_mean = mean(global_horizontal_radiation),
        ghi_maximum = max(global_horizontal_radiation),
        cloud_mean = mean(total_sky_cover)
    ), by = "month"]
    expect_equal(
        projected_month$ghi_mean - baseline_month$ghi_mean,
        rep(1, 12L),
        tolerance = 1e-8
    )
    expect_equal(
        projected_month$ghi_maximum,
        baseline_month$ghi_maximum,
        tolerance = 0
    )
    expect_true(all(
        abs(
            projected_month$cloud_mean -
                baseline_month$cloud_mean * 1.02
        ) <= 0.5 / baseline_data[, .N, by = "month"]$N
    ))
    expect_type(weather$total_sky_cover, "integer")
    expect_type(weather$opaque_sky_cover, "integer")
    expect_true(all(weather$total_sky_cover >= 0L &
        weather$total_sky_cover <= 10L))
    expect_true(all(weather$opaque_sky_cover >= 0L &
        weather$opaque_sky_cover <= weather$total_sky_cover))

    geometry <- solar__epw_interval_geometry(
        baseline_data,
        latitude = morpher__epw_location_numeric(
            baseline,
            c("latitude", "lat", "N2_latitude")
        ),
        longitude = morpher__epw_location_numeric(
            baseline,
            c("longitude", "lon", "N3_longitude")
        ),
        timezone = morpher__epw_location_numeric(
            baseline,
            c("time_zone", "timezone", "N4_time_zone"),
            default = 0
        )
    )
    expect_equal(
        weather$global_horizontal_radiation,
        weather$diffuse_horizontal_radiation +
            weather$direct_normal_radiation *
                geometry$effective_solar_projection,
        tolerance = 1e-8
    )
})

test_that("BTWS records infeasible-day fallback in the combined recipe", {
    context <- bws_btws_monthly_test__context(
        mean_shift = rep(20, 12L),
        minimum_shift = rep(0, 12L),
        maximum_shift = rep(0, 12L)
    )

    result <- morpher__run_context(context)

    expect_true(any(
        !is.na(result$factors$btws_fallback_reason)
    ))
    expect_true(
        "btws_mean_shift_fallback" %in% result$diagnostics$code
    )
})

test_that("BWS/BTWS validates all five monthly climate inputs", {
    missing_extrema <- bws_btws_monthly_test__climate(
        2001,
        period = "reference",
        experiment = "historical",
        include_extrema = FALSE
    )
    wrong_frequency <- bws_btws_monthly_test__climate(
        2001,
        period = "reference",
        experiment = "historical",
        frequency = "day"
    )

    expect_error(
        bws_btws__monthly_climatology(
            missing_extrema,
            "historical climate"
        ),
        "missing required variable"
    )
    expect_error(
        bws_btws__monthly_climate(
            wrong_frequency,
            "historical climate"
        ),
        "must use CMIP frequency"
    )
    expect_error(
        epw_morph_recipe(
            "bws_btws_monthly",
            options = list(window_days = 31L)
        ),
        "Unknown BWS and BTWS monthly morphing option"
    )
})

test_that("BWS/BTWS public transform survives dry-run plan reconstruction", {
    transform <- monthly_transform("bws_btws")
    reference <- historical_reference(years = 1995:2014)
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        frequency = "mon",
        table = "Amon"
    )
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = climate,
        periods = list(`2060s` = 2061L),
        transform = transform,
        reference = reference,
        dir = tempfile("bws_btws-monthly-output-"),
        store = tempfile("bws_btws-monthly-store-"),
        dry_run = TRUE
    )
    rebuilt <- shift__plan_from_spec(shift__plan_spec(plan))

    expect_identical(
        plan@meta$recipe$backend,
        "bws_btws_monthly"
    )
    expect_identical(
        rebuilt@meta$recipe$recipe_spec,
        "bws_btws_monthly"
    )
    expect_identical(
        rebuilt@meta$recipe$components$signal,
        "bws_btws_monthly_changes"
    )
    expect_silent(shift__validate_background_plan(plan))
})
