# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

# Assemble one direct context for the complete registered BWS/BTWS recipe.
# bws_btws_monthly_test__context {{{
bws_btws_monthly_test__context <- function(
    mean_shift = seq(0.5, 1.6, by = 0.1),
    minimum_shift = seq(0.3, 1.4, by = 0.1),
    maximum_shift = seq(0.7, 1.8, by = 0.1),
    radiation_shift = rep(1, 12L),
    cloud_scale = rep(0.02, 12L),
    recipe_name = "bws_btws_monthly"
) {
    morph_test__context(
        climate = morph_test__monthly_climate(
            2061:2062,
            period = "2060s",
            experiment = "ssp585",
            mean_shift = mean_shift,
            minimum_shift = minimum_shift,
            maximum_shift = maximum_shift,
            radiation_shift = radiation_shift,
            cloud_scale = cloud_scale
        ),
        reference_climate = morph_test__monthly_climate(
            2001:2002,
            period = "reference",
            experiment = "historical"
        ),
        recipe = epw_morph_recipe(recipe_name, policy = "harmonized")
    )
}
# }}}

test_that("BWS/BTWS monthly sources produce 12 month-constant target sets", {
    mean_shift <- seq(0.5, 1.6, by = 0.1)
    minimum_shift <- seq(0.3, 1.4, by = 0.1)
    maximum_shift <- seq(0.7, 1.8, by = 0.1)
    radiation_shift <- seq(0.2, 1.3, by = 0.1)
    cloud_scale <- seq(-0.02, 0.035, by = 0.005)
    historical <- morph_test__monthly_climate(
        2001:2002,
        period = "reference",
        experiment = "historical"
    )
    future <- morph_test__monthly_climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        mean_shift = mean_shift,
        minimum_shift = minimum_shift,
        maximum_shift = maximum_shift,
        radiation_shift = radiation_shift,
        cloud_scale = cloud_scale
    )

    monthly <- bws_btws__monthly_changes(
        bws_btws__monthly_climatology(future, "future climate"),
        bws_btws__monthly_climatology(historical, "historical climate")
    )
    temperature <- bws_btws__daily_temperature_targets(monthly)

    expect_identical(nrow(monthly), 12L)
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
    expect_equal(monthly$rsds_delta, radiation_shift, tolerance = 1e-12)
    expect_equal(monthly$clt_scale, cloud_scale, tolerance = 1e-12)
})

test_that("BWS/BTWS climatology pools years by native calendar month", {
    source <- morph_test__monthly_climate(
        2001:2002,
        period = "reference",
        experiment = "historical"
    )
    source[, cf_month := as.integer(format(time, "%m"))]
    baseline <- bws_btws__monthly_climatology(source, "historical climate")

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
    expect_identical(nrow(result$parts$bws_factors), 24L)
    expect_identical(
        sort(unique(result$parts$bws_factors$variable_id)),
        c("clt", "rsds")
    )
    expect_lt(max(abs(result$factors$mean_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$minimum_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$maximum_closure_error)), 1e-8)
    expect_false(any(!is.na(result$factors$btws_fallback_reason)))

    achieved <- weather[,
        .(
            baseline_mean = mean(baseline_data$dry_bulb_temperature[.I]),
            baseline_minimum = min(baseline_data$dry_bulb_temperature[.I]),
            baseline_maximum = max(baseline_data$dry_bulb_temperature[.I]),
            projected_mean = mean(dry_bulb_temperature),
            projected_minimum = min(dry_bulb_temperature),
            projected_maximum = max(dry_bulb_temperature)
        ),
        by = c("daily_target_day")
    ]
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

    baseline_month <- baseline_data[,
        .(
            ghi_mean = mean(global_horizontal_radiation),
            ghi_maximum = max(global_horizontal_radiation),
            cloud_mean = mean(total_sky_cover)
        ),
        by = "month"
    ]
    projected_month <- weather[,
        .(
            ghi_mean = mean(global_horizontal_radiation),
            ghi_maximum = max(global_horizontal_radiation),
            cloud_mean = mean(total_sky_cover)
        ),
        by = "month"
    ]
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
        ) <=
            0.5 / baseline_data[, .N, by = "month"]$N
    ))
    expect_type(weather$total_sky_cover, "integer")
    expect_type(weather$opaque_sky_cover, "integer")
    expect_true(all(
        weather$total_sky_cover >= 0L &
            weather$total_sky_cover <= 10L
    ))
    expect_true(all(
        weather$opaque_sky_cover >= 0L &
            weather$opaque_sky_cover <= weather$total_sky_cover
    ))

    loc <- morpher__epw_location(baseline)
    geometry <- solar__epw_interval_geometry(
        baseline_data,
        latitude = loc$latitude,
        longitude = loc$longitude,
        timezone = loc$timezone
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

    expect_true(any(!is.na(result$factors$btws_fallback_reason)))
    expect_true("btws_mean_shift_fallback" %in% result$diagnostics$code)
})

test_that("BWS records bounded climate targets without aborting sibling work", {
    context <- bws_btws_monthly_test__context(cloud_scale = rep(3, 12L))
    context$case <- data.table::data.table(
        source_id = "IPSL-CM6A-LR",
        experiment_id = "ssp126",
        variant_label = "r1i1p1f1",
        period = "2050"
    )

    result <- morpher__run_context(context)
    adjusted <- result$parts$bws_factors[
        variable_id == "clt" & target_adjustment != "none"
    ]

    expect_gt(nrow(adjusted), 0L)
    expect_true(all(adjusted$requested_target_mean > adjusted$target_mean))
    expect_true(all(adjusted$target_mean <= adjusted$attainable_upper))
    expect_true("bws_target_adjusted" %in% result$diagnostics$code)
    diagnostic <- result$diagnostics[code == "bws_target_adjusted"][1L]
    expect_identical(diagnostic$variable_id, "clt")
    expect_identical(diagnostic$period, "2050")
    expect_match(diagnostic$message, "IPSL-CM6A-LR/ssp126/r1i1p1f1/2050")
    expect_match(diagnostic$message, "GCM historical mean")
    expect_match(diagnostic$message, "baseline EPW mean")
})

test_that("BWS/BTWS validates all five monthly climate inputs", {
    missing_extrema <- morph_test__monthly_climate(
        2001,
        period = "reference",
        experiment = "historical",
        include_extrema = FALSE
    )
    wrong_frequency <- morph_test__monthly_climate(
        2001,
        period = "reference",
        experiment = "historical",
        frequency = "day"
    )

    expect_error(
        bws_btws__monthly_climatology(missing_extrema, "historical climate"),
        "missing required variable"
    )
    expect_error(
        bws_btws__monthly_climate(wrong_frequency, "historical climate"),
        "must use CMIP frequency"
    )
    expect_error(
        epw_morph_recipe(
            "bws_btws_monthly",
            options = list(window_days = 31L)
        ),
        "Unknown BWS and BTWS monthly morphing option"
    )
    expect_error(
        transform__validate_execution_inputs(monthly_transform("bws_btws")),
        "requires.*reference"
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
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = climate,
        periods = list(`2060s` = 2061L),
        transform = transform,
        reference = reference,
        dir = tempfile("bws_btws-monthly-output-"),
        store = tempfile("bws_btws-monthly-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
    rebuilt <- shift_persist__plan_from_spec(shift_persist__plan_spec(plan))

    expect_identical(plan@meta$recipe$backend, "bws_btws_monthly")
    expect_identical(rebuilt@meta$recipe$recipe_spec, "bws_btws_monthly")
    expect_silent(shift_job__validate_background_plan(plan))
})

# Exercise the registered monthly recipe and the EPW round-trip with both
# valid observations and unknown opaque cover at different total covers.
test_that("monthly BWS/BTWS retains unknown opaque cover on EPW readback", {
    context <- bws_btws_monthly_test__context()
    weather <- context$epw$data()
    data.table::set(weather, 1:4, "total_sky_cover", c(0L, 8L, 0L, 8L))
    data.table::set(
        weather,
        1:4,
        "opaque_sky_cover",
        c(NA_integer_, NA_integer_, 0L, 4L)
    )
    context$epw$set(weather)
    result <- morpher__run_context(context)
    output <- context$epw$clone()
    output$set(result$data)
    path <- tempfile(fileext = ".epw")
    output$save(path)
    actual <- epw_file_read(path)$data()[1:4]
    expect_true(all(is.na(actual$opaque_sky_cover[1:2])))
    expect_true(all(is.finite(actual$opaque_sky_cover[3:4])))
    expect_true(all(actual$opaque_sky_cover[3:4] >= 0L))
    expect_true(all(
        actual$opaque_sky_cover[3:4] <= actual$total_sky_cover[3:4]
    ))
})

# Assemble one direct role-addressable context for the composite BTWS recipe.
# btws_test__context {{{
btws_test__context <- function(
    mean_shift = 0.5,
    minimum_shift = 0,
    maximum_shift = 0
) {
    morph_test__context(
        climate = morph_test__daily_climate(
            2061:2062,
            period = "2060s",
            experiment = "ssp585",
            mean_shift = mean_shift,
            minimum_shift = minimum_shift,
            maximum_shift = maximum_shift
        ),
        reference_climate = morph_test__daily_climate(
            2001:2002,
            period = "reference",
            experiment = "historical"
        ),
        recipe = epw_morph_recipe("epwshiftr_daily_btws")
    )
}
# }}}

test_that("daily BTWS backend requires all three temperature extrema variables", {
    backend <- epw_morph_backend("daily_temperature_btws")
    expect_true(backend$requires_reference)
    expect_equal(
        backend$required_variables(),
        c("tas", "tasmin", "tasmax")
    )
    expect_error(
        transform__validate_execution_inputs(
            daily_transform("epwshiftr", reconstruction = "btws")
        ),
        "requires.*reference"
    )
})

test_that("daily CMIP6 and BTWS composition closes a future EPW year", {
    context <- btws_test__context()
    baseline_data <- morph_test__baseline_weather(context)

    result <- morpher__run_context(context)
    weather <- result$data

    expect_s3_class(result, "epw_morph_result")
    expect_identical(nrow(weather), 8760L)
    expect_identical(nrow(result$factors), 365L)
    expect_true(all(result$parts$component_pipeline$status == "ok"))
    expect_identical(
        result$parts$component_pipeline$component[
            result$parts$component_pipeline$stage == "hourly"
        ],
        "btws_temperature_projection"
    )
    expect_lt(max(abs(result$factors$mean_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$minimum_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$maximum_closure_error)), 1e-8)
    expect_false(any(!is.na(result$factors$btws_fallback_reason)))
    expect_true(all(is.finite(result$factors$btws_scale)))
    expect_true(all(is.finite(result$factors$btws_m)))
    expect_true(all(is.finite(result$factors$btws_n)))
    expect_true(all(
        c(
            "btws_scale",
            "btws_m",
            "btws_n",
            "btws_fallback_reason"
        ) %in%
            names(weather)
    ))
    expect_false("daily_temperature_shape_exponent" %in% names(weather))

    achieved <- weather[,
        .(
            baseline_mean = mean(baseline_data$dry_bulb_temperature[.I]),
            projected_mean = mean(dry_bulb_temperature)
        ),
        by = "daily_target_day"
    ]
    expect_equal(
        achieved$projected_mean - achieved$baseline_mean,
        rep(0.5, 365L),
        tolerance = 1e-8
    )
    expect_true(all(
        weather$relative_humidity >= 0 &
            weather$relative_humidity <= 100
    ))
    expect_true(all(
        weather$dew_point_temperature <= weather$dry_bulb_temperature
    ))
})

test_that("daily temperature selects BTWS and survives plan reconstruction", {
    transform <- daily_transform(
        "epwshiftr",
        reconstruction = "btws",
        window_days = 15L
    )
    reference <- historical_reference(years = 1995:2014)
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        frequency = "day",
        table = "day"
    )
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = climate,
        periods = list(`2060s` = 2061L),
        transform = transform,
        reference = reference,
        dir = tempfile("daily-btws-output-"),
        store = tempfile("daily-btws-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
    rebuilt <- shift_persist__plan_from_spec(shift_persist__plan_spec(plan))

    expect_identical(plan@meta$recipe$backend, "daily_temperature_btws")
    expect_identical(plan@meta$recipe$recipe_spec, "epwshiftr_daily_btws")
    expect_identical(rebuilt@meta$recipe$options$window_days, 15L)
    expect_identical(
        rebuilt@meta$recipe$components$hourly,
        "btws_temperature_projection"
    )
    expect_silent(shift_job__validate_background_plan(plan))
})

test_that("daily temperature reconstruction selects one hourly component", {
    power <- daily_transform("epwshiftr", reconstruction = "power")
    btws <- daily_transform("epwshiftr", reconstruction = "btws")
    power_recipe <- transform__recipe(power)
    btws_recipe <- transform__recipe(btws)

    expect_identical(power_recipe$recipe_spec, "epwshiftr_daily_power")
    expect_identical(
        power_recipe$components$hourly,
        "constrained_daily_temperature"
    )
    expect_identical(btws_recipe$recipe_spec, "epwshiftr_daily_btws")
    expect_identical(
        btws_recipe$components$hourly,
        "btws_temperature_projection"
    )
    expect_error(
        daily_transform("epwshiftr", reconstruction = "unknown"),
        "power.*btws"
    )
})

# vim: fdm=marker :
