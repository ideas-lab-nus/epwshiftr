# Build deterministic daily CMIP thermodynamic rows whose derived humidity
# state is known before the Sobie-Curry factor equations are applied.
sobie_test__climate <- function(
    years,
    period,
    experiment,
    calendar_days = 365L,
    temperature_shift = 0,
    dtr_shift = 0,
    relative_humidity_ratio = 1,
    pressure_shift = 0,
    omit = character()
) {
    years <- as.integer(years)
    calendar_days <- as.integer(calendar_days)
    phase <- (seq_len(calendar_days) - 0.5) / calendar_days
    rows <- lapply(seq_along(years), function(index) {
        seasonal <- 9 * sin(2 * pi * phase)
        replicate_offset <- (index - mean(seq_along(years))) * 0.4
        mean_temperature <- 12 + seasonal + replicate_offset +
            temperature_shift
        dtr <- 8 + 1.5 * cos(2 * pi * phase) + dtr_shift
        minimum <- mean_temperature - dtr / 2
        maximum <- mean_temperature + dtr / 2
        pressure <- 100000 + 300 * cos(2 * pi * phase) +
            pressure_shift
        relative_humidity <- (
            62 + 8 * cos(2 * pi * phase) + replicate_offset
        ) * relative_humidity_ratio
        specific_humidity <- morpher__huss_from_rh_si(
            mean_temperature,
            relative_humidity,
            pressure
        )
        time <- as.POSIXct(
            sprintf("%04d-01-01 12:00:00", years[[index]]),
            tz = "UTC"
        ) + seq.int(0L, calendar_days - 1L) * 86400
        values <- list(
            tas = mean_temperature + 273.15,
            tasmin = minimum + 273.15,
            tasmax = maximum + 273.15,
            huss = specific_humidity,
            ps = pressure
        )
        units <- c(
            tas = "K",
            tasmin = "K",
            tasmax = "K",
            huss = "kg kg-1",
            ps = "Pa"
        )
        data.table::rbindlist(lapply(setdiff(names(values), omit), function(
            variable_id
        ) {
            data.table::data.table(
                activity_id = if (identical(experiment, "historical")) {
                    "CMIP"
                } else {
                    "ScenarioMIP"
                },
                institution_id = "PCIC-test",
                source_id = "TestModel",
                experiment_id = experiment,
                variant_label = "r1i1p1f1",
                frequency = "day",
                table_id = "day",
                variable_id = variable_id,
                time = time,
                year = years[[index]],
                annual_phase = phase,
                period = period,
                lon = -123,
                lat = 49,
                units = units[[variable_id]],
                value = values[[variable_id]]
            )
        }))
    })
    data.table::rbindlist(rows)
}

# Build one backend context from the packaged EPW fixture and aligned daily
# future/historical thermodynamic sources.
sobie_test__context <- function(
    temperature_shift = 0,
    dtr_shift = 0,
    relative_humidity_ratio = 1,
    pressure_shift = 0,
    historical_days = 365L,
    future_days = 365L,
    omit_future = character(),
    policy = "paper_faithful"
) {
    historical <- sobie_test__climate(
        2001:2003,
        period = "reference",
        experiment = "historical",
        calendar_days = historical_days
    )
    future <- sobie_test__climate(
        2061:2063,
        period = "2060s",
        experiment = "ssp585",
        calendar_days = future_days,
        temperature_shift = temperature_shift,
        dtr_shift = dtr_shift,
        relative_humidity_ratio = relative_humidity_ratio,
        pressure_shift = pressure_shift,
        omit = omit_future
    )
    morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        recipe = epw_morph_recipe(
            "sobie_curry_daily",
            policy = policy
        )
    )
}

test_that("Sobie-Curry recipe registers its published daily contract", {
    expect_true("sobie_curry_daily" %in% epw_morph_backends())
    expect_true("sobie_curry_daily" %in% epw_morph_recipes()[["name"]])
    backend <- epw_morph_backend("sobie_curry_daily")
    recipe <- epw_morph_recipe("sobie_curry_daily")
    spec <- epw_morph_recipe_spec("sobie_curry_daily")

    expect_true(backend$requires_reference)
    expect_identical(recipe$policy, "paper_faithful")
    expect_identical(
        names(spec@policy_profiles),
        c("paper_faithful", "harmonized")
    )
    expect_identical(spec@version, 2L)
    expect_identical(recipe$options$window_days, 21L)
    expect_identical(spec@calendar_policy, "cf_annual_phase_365")
    expect_identical(
        epw_morph_variables(recipe),
        c("tasmin", "tasmax", "tas", "huss", "ps")
    )
    expect_identical(
        names(recipe$components),
        WEATHER_COMPONENT_STAGES
    )
    expect_identical(
        recipe$components$physics,
        "daily_thermodynamic_closure"
    )
    expect_identical(
        morpher__recipe_required_frequency(recipe),
        "day"
    )
    expect_error(sobie_curry_daily(), "requires an explicit reference")
    expect_error(
        epw_morph_recipe(
            "sobie_curry_daily",
            options = list(window_days = 20L)
        ),
        "must be odd"
    )
    expect_identical(
        epw_morph_recipe(
            "sobie_curry_daily",
            policy = "harmonized"
        )$policy,
        "harmonized"
    )
    expect_error(
        epw_morph_recipe(
            "sobie_curry_daily",
            version = 1L
        ),
        "requires definition version 2"
    )
})

test_that("Sobie-Curry zero climate change is an hourly identity", {
    context <- sobie_test__context()
    baseline <- context$epw$clone()
    suppressMessages(baseline$drop_unit())
    baseline_weather <- data.table::as.data.table(baseline$data())
    result <- morpher__run_context(context)

    expect_s3_class(result, "epw_morph_result")
    expect_identical(nrow(result$data), 8760L)
    expect_identical(nrow(result$factors), 365L)
    expect_equal(
        result$data$dry_bulb_temperature,
        baseline_weather$dry_bulb_temperature,
        tolerance = 1e-10
    )
    expect_equal(
        result$data$dew_point_temperature,
        baseline_weather$dew_point_temperature,
        tolerance = 1e-10
    )
    expect_equal(
        result$data$relative_humidity,
        baseline_weather$relative_humidity,
        tolerance = 1e-10
    )
    expect_equal(
        result$data$atmospheric_pressure,
        baseline_weather$atmospheric_pressure,
        tolerance = 1e-10
    )
    expect_equal(
        result$factors$dew_point_sd_relative_change,
        rep(0, 365L),
        tolerance = 1e-10
    )
    expect_identical(
        result$parts$settings$dew_point_sd_factor,
        "sigma_future / sigma_historical - 1"
    )
    expect_identical(nrow(result$diagnostics), 0L)
})

test_that("Sobie-Curry equations close daily means and temperature ranges", {
    context <- sobie_test__context(
        temperature_shift = 2,
        dtr_shift = 1.5,
        relative_humidity_ratio = 0.9,
        pressure_shift = 125
    )
    baseline <- context$epw$clone()
    suppressMessages(baseline$drop_unit())
    baseline_weather <- data.table::as.data.table(baseline$data())
    result <- morpher__run_context(context)
    factors <- result$factors
    weather <- result$data

    expect_equal(
        factors$temperature_mean_delta,
        rep(2, 365L),
        tolerance = 1e-10
    )
    expect_equal(
        factors$temperature_dtr_delta,
        rep(1.5, 365L),
        tolerance = 1e-10
    )
    expect_equal(
        factors$relative_humidity_ratio,
        rep(0.9, 365L),
        tolerance = 1e-10
    )
    expect_equal(
        factors$pressure_delta,
        rep(125, 365L),
        tolerance = 1e-10
    )
    expect_lt(max(abs(factors$temperature_mean_closure_error)), 1e-9)
    expect_lt(max(abs(factors$temperature_dtr_closure_error)), 1e-9)
    expect_lt(max(abs(factors$dew_point_mean_closure_error)), 1e-9)
    expect_equal(
        weather$relative_humidity,
        baseline_weather$relative_humidity *
            weather$sobie_curry_relative_humidity_ratio,
        tolerance = 1e-10
    )
    expect_equal(
        weather$atmospheric_pressure,
        baseline_weather$atmospheric_pressure +
            weather$sobie_curry_pressure_delta,
        tolerance = 1e-10
    )
    expect_identical(weather$wind_speed, baseline_weather$wind_speed)
})

test_that("Sobie-Curry harmonized policy retains temperature and closes HUSS", {
    paper <- morpher__run_context(sobie_test__context(
        temperature_shift = 2,
        dtr_shift = 1.5,
        relative_humidity_ratio = 0.9,
        pressure_shift = 125
    ))
    harmonized <- morpher__run_context(sobie_test__context(
        temperature_shift = 2,
        dtr_shift = 1.5,
        relative_humidity_ratio = 0.9,
        pressure_shift = 125,
        policy = "harmonized"
    ))

    expect_equal(
        harmonized$factors,
        paper$factors,
        tolerance = 1e-12
    )
    expect_equal(
        harmonized$data$dry_bulb_temperature,
        paper$data$dry_bulb_temperature,
        tolerance = 1e-12
    )
    expect_equal(
        harmonized$data$atmospheric_pressure,
        paper$data$atmospheric_pressure,
        tolerance = 1e-12
    )
    expect_true(any(abs(
        harmonized$data$relative_humidity -
            paper$data$relative_humidity
    ) > 1e-6))
    expect_true(all(
        harmonized$data$relative_humidity >= 0 &
            harmonized$data$relative_humidity <= 100
    ))
    expect_true(all(
        harmonized$data$dew_point_temperature <=
            harmonized$data$dry_bulb_temperature
    ))

    closed_huss <- morpher__huss_from_rh_si(
        harmonized$data$dry_bulb_temperature,
        harmonized$data$relative_humidity,
        harmonized$data$atmospheric_pressure
    )
    expect_equal(
        closed_huss,
        harmonized$data$sobie_curry_specific_humidity,
        tolerance = 1e-10
    )
    expect_equal(
        harmonized$data$sobie_curry_target_specific_humidity,
        harmonized$data$sobie_curry_baseline_specific_humidity +
            harmonized$data$sobie_curry_specific_humidity_delta,
        tolerance = 1e-12
    )
    expect_identical(
        harmonized$parts$settings$physical_policy,
        "specific_humidity_delta_closure"
    )
    expect_identical(
        paper$parts$settings$physical_policy,
        "independent_paper_transforms"
    )
})

test_that("Sobie-Curry harmonized closure reports both physical bounds", {
    hourly <- data.table::data.table(
        temperature_projected = c(0, 20),
        pressure_projected = c(100000, 100000),
        dry_bulb_temperature = c(20, 20),
        relative_humidity = c(90, 10),
        atmospheric_pressure = c(100000, 100000),
        specific_humidity_delta = c(0.02, -0.02)
    )
    humidity <- sobie__harmonized_humidity(hourly)

    expect_identical(
        humidity$status,
        c("saturation_clipped", "zero_clipped")
    )
    expect_equal(humidity$relative_humidity, c(100, 0), tolerance = 1e-8)
    expect_equal(humidity$dew_point_temperature[[1L]], 0, tolerance = 1e-8)
    expect_true(
        humidity$dew_point_temperature[[2L]] <=
            hourly$temperature_projected[[2L]]
    )
})

test_that("Sobie-Curry circular windows smooth and bridge CF calendars", {
    value <- numeric(365L)
    value[[100L]] <- 1
    smoothed <- sobie__smooth_factor(
        value,
        sobie__smoothing_windows(21L),
        "test_factor"
    )
    expect_equal(
        sum(smoothed > 0),
        21L
    )
    expect_equal(
        smoothed[smoothed > 0],
        rep(1 / 21, 21L),
        tolerance = 1e-12
    )

    context <- sobie_test__context(
        historical_days = 360L,
        future_days = 366L
    )
    result <- morpher__run_context(context)
    expect_identical(nrow(result$factors), 365L)
    expect_true(all(is.finite(
        result$factors$temperature_mean_delta
    )))
    expect_true(all(is.finite(
        result$factors$dew_point_sd_relative_change
    )))
})

test_that("Sobie-Curry rejects incomplete daily thermodynamic inputs", {
    context <- sobie_test__context(omit_future = "huss")
    expect_error(
        morpher__run_context(context),
        "lacks variable alternative.*huss"
    )
})
