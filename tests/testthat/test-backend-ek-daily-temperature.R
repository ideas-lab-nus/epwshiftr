# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

# Build deterministic native-calendar daily extrema for Ek tests.
# ek_test__climate {{{
ek_test__climate <- function(
    years,
    period,
    experiment,
    calendar_days = 365L,
    temperature_shift = 0,
    dtr_ratio = 1,
    historical_dtr = 8,
    omit = character(),
    frequency = "day"
) {
    morph_test__daily_climate(
        years = years,
        period = period,
        experiment = experiment,
        variables = c("tasmin", "tasmax"),
        calendar_days = calendar_days,
        frequency = frequency,
        mean_shift = temperature_shift,
        base_temp = 12,
        seasonal_amp = 7,
        replicate_step = 0.2,
        dtr_base = historical_dtr,
        dtr_ratio = dtr_ratio,
        omit = omit,
        source_id = "TestModel",
        institution_id = "PCIC-test",
        lon = -123,
        lat = 49
    )
}
# }}}

# Build a complete Ek backend context around the packaged EPW fixture.
# ek_test__context {{{
ek_test__context <- function(
    temperature_shift = 0,
    dtr_ratio = 1,
    historical_dtr = 8,
    historical_days = 365L,
    future_days = 365L,
    policy = "paper_faithful",
    recipe_name = "ek_daily_factors"
) {
    morph_test__context(
        climate = ek_test__climate(
            2061:2063,
            period = "2060s",
            experiment = "ssp585",
            calendar_days = future_days,
            temperature_shift = temperature_shift,
            dtr_ratio = dtr_ratio,
            historical_dtr = historical_dtr
        ),
        reference_climate = ek_test__climate(
            2001:2003,
            period = "reference",
            experiment = "historical",
            calendar_days = historical_days,
            historical_dtr = historical_dtr
        ),
        recipe = epw_morph_recipe(recipe_name, policy = policy)
    )
}
# }}}

test_that("Ek recipe rejects unsupported smoothing options and missing reference", {
    expect_error(
        transform__validate_execution_inputs(daily_transform("ek")),
        "requires.*reference"
    )
    expect_error(
        epw_morph_recipe(
            "ek_daily_factors",
            options = list(window_days = 21L)
        ),
        "Unknown Ek daily temperature option"
    )
})

test_that("Ek calendar adapter maps 360-day years without smoothing", {
    historical <- ek_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical",
        calendar_days = 360L
    )
    future <- ek_test__climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        calendar_days = 360L,
        temperature_shift = 2,
        dtr_ratio = 1.25
    )
    targets <- ek__daily_temperature_factors(
        ek__daily_temperature_climatology(
            temperature__daily_climate(future, "future climate"),
            "future climate"
        ),
        ek__daily_temperature_climatology(
            temperature__daily_climate(historical, "historical climate"),
            "historical climate"
        )
    )

    expect_identical(nrow(targets), 365L)
    expect_equal(targets$mean_delta, rep(2, 365L), tolerance = 1e-10)
    expect_equal(
        targets$dtr_relative_change,
        rep(0.25, 365L),
        tolerance = 1e-10
    )
    expect_equal(targets$dtr_ratio, rep(1.25, 365L), tolerance = 1e-10)
    expect_true(all(targets$dtr_status == "adjusted"))
    expect_true(all(targets$n_years_future_minimum == 2L))
    expect_true(all(targets$n_years_historical_maximum == 2L))
})

test_that("Ek zero climate change is an hourly identity", {
    context <- ek_test__context()
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
        result$data$relative_humidity,
        baseline_weather$relative_humidity,
        tolerance = 0
    )
    expect_equal(
        result$data$dew_point_temperature,
        baseline_weather$dew_point_temperature,
        tolerance = 0
    )
    expect_equal(
        result$factors$mean_closure_error,
        rep(0, 365L),
        tolerance = 1e-10
    )
    expect_equal(
        result$factors$dtr_closure_error,
        rep(0, 365L),
        tolerance = 1e-10
    )
    expect_identical(result$parts$settings$smoothing, "none")
})

test_that("Ek combined equation closes daily mean and DTR changes", {
    context <- ek_test__context(temperature_shift = 2, dtr_ratio = 1.25)
    result <- morpher__run_context(context)
    factors <- result$factors

    expect_equal(factors$mean_delta, rep(2, 365L), tolerance = 1e-10)
    expect_equal(
        factors$dtr_relative_change,
        rep(0.25, 365L),
        tolerance = 1e-10
    )
    expect_lt(max(abs(factors$mean_closure_error)), 1e-10)
    expect_lt(max(abs(factors$dtr_closure_error)), 1e-10)
    expect_equal(
        factors$projected_mean - factors$baseline_mean,
        rep(2, 365L),
        tolerance = 1e-10
    )
    expect_equal(
        factors$projected_dtr / factors$baseline_dtr,
        rep(1.25, 365L),
        tolerance = 1e-10
    )
    expect_true(all(factors$projection_status == "daily_mean_dtr"))
})

test_that("Ek harmonized policy closes humidity against temperature", {
    paper <- morpher__run_context(ek_test__context(
        temperature_shift = -8,
        dtr_ratio = 1.1,
        policy = "paper_faithful"
    ))
    harmonized <- morpher__run_context(ek_test__context(
        temperature_shift = -8,
        dtr_ratio = 1.1,
        policy = "harmonized"
    ))

    expect_equal(
        harmonized$data$dry_bulb_temperature,
        paper$data$dry_bulb_temperature,
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
        harmonized$data$ek_specific_humidity,
        tolerance = 1e-10
    )
    expect_identical(
        harmonized$parts$settings$physical_policy,
        "specific_humidity_closure"
    )
    expect_identical(
        paper$parts$settings$physical_policy,
        "preserve_baseline_humidity_fields"
    )
})

test_that("Ek records zero historical model DTR fallback", {
    context <- ek_test__context(temperature_shift = 2, historical_dtr = 0)
    result <- morpher__run_context(context)

    expect_true(all(
        result$factors$dtr_status == "inherited_zero_historical_dtr"
    ))
    expect_true(all(
        result$factors$projection_status == "mean_shift_zero_historical_dtr"
    ))
    expect_true(
        "ek_zero_historical_model_dtr" %in% result$diagnostics$code
    )
})

test_that("Ek validates extrema, year, frequency, and daily completeness", {
    missing_extrema <- ek_test__climate(
        2001,
        period = "reference",
        experiment = "historical",
        omit = "tasmax"
    )
    missing_year <- ek_test__climate(
        2001,
        period = "reference",
        experiment = "historical"
    )[, c("year", "time") := NULL]
    wrong_frequency <- ek_test__climate(
        2001,
        period = "reference",
        experiment = "historical",
        frequency = "mon"
    )
    duplicated <- ek_test__climate(
        2001,
        period = "reference",
        experiment = "historical"
    )
    duplicated <- data.table::rbindlist(
        list(duplicated, duplicated[1L]),
        use.names = TRUE
    )

    expect_error(
        ek__daily_temperature_climatology(
            missing_extrema,
            "historical climate"
        ),
        "missing required variable"
    )
    expect_error(
        ek__daily_temperature_climatology(missing_year, "historical climate"),
        "source-calendar year"
    )
    expect_error(
        temperature__daily_climate(wrong_frequency, "historical climate"),
        "must use CMIP frequency"
    )
    expect_error(
        ek__daily_temperature_climatology(duplicated, "historical climate"),
        "one value per annual phase"
    )
})

test_that("Ek public transform survives dry-run plan reconstruction", {
    transform <- daily_transform("ek")
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
        dir = tempfile("ek-daily-output-"),
        store = tempfile("ek-daily-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
    rebuilt <- shift_persist__plan_from_spec(shift_persist__plan_spec(plan))

    expect_identical(plan@meta$recipe$backend, "ek_daily_temperature")
    expect_identical(rebuilt@meta$recipe$recipe_spec, "ek_daily_factors")
    expect_identical(
        rebuilt@meta$recipe$policy,
        transform__recipe(transform)$policy
    )
    expect_silent(shift_job__validate_background_plan(plan))
})

# vim: fdm=marker :
