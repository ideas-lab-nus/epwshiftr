# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

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
