# Build one Sobie-Curry backend context around the packaged EPW fixture.
# sobie_test__context {{{
sobie_test__context <- function(
    temperature_shift = 0,
    dtr_shift = 0,
    relative_humidity_ratio = 1,
    pressure_shift = 0,
    historical_days = 365L,
    future_days = 365L,
    omit_future = character(),
    policy = "paper_faithful",
    recipe_name = "sobie_curry_daily"
) {
    variables <- c("tas", "tasmin", "tasmax", "huss", "ps")
    morph_test__context(
        climate = morph_test__daily_climate(
            2061:2063,
            period = "2060s",
            experiment = "ssp585",
            variables = variables,
            calendar_days = future_days,
            mean_shift = temperature_shift,
            base_temp = 12,
            seasonal_amp = 9,
            replicate_step = 0.4,
            dtr_base = 8,
            dtr_shift = dtr_shift,
            relative_humidity_ratio = relative_humidity_ratio,
            pressure_shift = pressure_shift,
            omit = omit_future,
            source_id = "TestModel",
            institution_id = "PCIC-test",
            lon = -123,
            lat = 49
        ),
        reference_climate = morph_test__daily_climate(
            2001:2003,
            period = "reference",
            experiment = "historical",
            variables = variables,
            calendar_days = historical_days,
            base_temp = 12,
            seasonal_amp = 9,
            replicate_step = 0.4,
            dtr_base = 8,
            source_id = "TestModel",
            institution_id = "PCIC-test",
            lon = -123,
            lat = 49
        ),
        recipe = epw_morph_recipe(recipe_name, policy = policy)
    )
}
# }}}

test_that("Sobie-Curry recipe validates window options, policy, and version", {
    recipe <- epw_morph_recipe("sobie_curry_daily")
    expect_identical(recipe$options$window_days, 21L)
    expect_identical(
        epw_morph_recipe("sobie_curry_daily", policy = "harmonized")$policy,
        "harmonized"
    )
    expect_error(
        transform__validate_execution_inputs(daily_transform("sobie_curry")),
        "requires.*reference"
    )
    expect_error(
        epw_morph_recipe(
            "sobie_curry_daily",
            options = list(window_days = 20L)
        ),
        "must be odd"
    )
    expect_error(
        epw_morph_recipe("sobie_curry_daily", version = 1L),
        "requires definition version 2"
    )
})

test_that("Sobie-Curry zero climate change is an hourly identity", {
    context <- sobie_test__context()
    baseline_weather <- morph_test__baseline_weather(context)
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
    baseline_weather <- morph_test__baseline_weather(context)
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
    expect_equal(factors$pressure_delta, rep(125, 365L), tolerance = 1e-10)
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

    expect_equal(harmonized$factors, paper$factors, tolerance = 1e-12)
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
    expect_true(any(
        abs(
            harmonized$data$relative_humidity -
                paper$data$relative_humidity
        ) >
            1e-6
    ))
    expect_true(all(
        harmonized$data$relative_humidity >= 0 &
            harmonized$data$relative_humidity <= 100
    ))
    expect_true(all(
        harmonized$data$dew_point_temperature <=
            harmonized$data$dry_bulb_temperature
    ))

    closed_huss <- epwphys__huss_from_rh_si(
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
    target <- sobie__specific_humidity_target(hourly)
    humidity <- epwphys__close_specific_humidity(
        hourly$temperature_projected,
        hourly$pressure_projected,
        target$target_specific_humidity
    )

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
    expect_equal(sum(smoothed > 0), 21L)
    expect_equal(
        smoothed[smoothed > 0],
        rep(1 / 21, 21L),
        tolerance = 1e-12
    )

    context <- sobie_test__context(historical_days = 360L, future_days = 366L)
    result <- morpher__run_context(context)
    expect_identical(nrow(result$factors), 365L)
    expect_true(all(is.finite(result$factors$temperature_mean_delta)))
    expect_true(all(is.finite(result$factors$dew_point_sd_relative_change)))
})

test_that("Sobie-Curry rejects incomplete daily thermodynamic inputs", {
    context <- sobie_test__context(omit_future = "huss")
    expect_error(
        morpher__run_context(context),
        "lacks variable alternative.*huss"
    )
})

# vim: fdm=marker :
