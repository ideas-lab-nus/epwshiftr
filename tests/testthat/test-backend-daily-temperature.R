# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

# Build one direct daily-temperature context using shared morph fixtures.
# daily_backend_test__context {{{
daily_backend_test__context <- function(
    include_extrema = TRUE,
    frequency = "day",
    mean_shift = 2,
    minimum_shift = 1,
    maximum_shift = 3
) {
    variables <- if (isTRUE(include_extrema)) {
        c("tas", "tasmin", "tasmax")
    } else {
        "tas"
    }
    morph_test__context(
        climate = morph_test__daily_climate(
            2061:2062,
            period = "2060s",
            experiment = "ssp585",
            variables = variables,
            mean_shift = mean_shift,
            minimum_shift = minimum_shift,
            maximum_shift = maximum_shift,
            frequency = frequency
        ),
        reference_climate = morph_test__daily_climate(
            2001:2002,
            period = "reference",
            experiment = "historical",
            variables = variables,
            frequency = frequency
        ),
        recipe = epw_morph_recipe("daily_temperature")
    )
}
# }}}

# Construct one three-role context reused across the daily adjustment methods.
# daily_adjustment_test__context {{{
daily_adjustment_test__context <- function(recipe) {
    morph_test__context(
        climate = morph_test__daily_climate(
            2061:2062,
            period = "future",
            experiment = "ssp585",
            variables = "tas",
            mean_shift = 3,
            source_id = "Test-GCM",
            institution_id = "Test Institute",
            cf_noleap = TRUE
        ),
        reference_climate = morph_test__daily_climate(
            1991:1992,
            period = "historical",
            experiment = "historical",
            variables = "tas",
            mean_shift = 1,
            source_id = "Test-GCM",
            institution_id = "Test Institute",
            cf_noleap = TRUE
        ),
        observed_reference = morph_test__daily_climate(
            1981:1982,
            period = "observed",
            experiment = "observation",
            variables = "tas",
            mean_shift = 0,
            source_id = "Test-GCM",
            institution_id = "Test Institute",
            cf_noleap = TRUE
        ),
        recipe = recipe
    )
}
# }}}

test_that("shared temperature conversion handles mixed supported units", {
    expect_equal(
        temperature__to_celsius(
            c(273.15, 20, 293.15),
            c("K", "degC", "kelvin")
        ),
        c(0, 20, 20)
    )
    expect_equal(temperature__to_celsius(273.15, "K"), 0)
})

test_that("daily temperature backend validates options and optional extrema", {
    backend <- epw_morph_backend("daily_temperature")
    expect_true(backend$requires_reference)
    expect_identical(backend$required_variables(), "tas")
    expect_equal(
        epw_morph_variables(backend, include_optional = TRUE),
        c("tas", "tasmin", "tasmax")
    )

    recipe <- epw_morph_recipe(
        "daily_temperature",
        options = list(window_days = 15L)
    )
    expect_identical(recipe$options$window_days, 15L)
    expect_identical(recipe$methods, c(tdb = "constrained"))
    expect_error(
        epw_morph_recipe(
            "daily_temperature",
            options = list(window_days = 14L)
        ),
        "must be odd"
    )
    expect_error(
        transform__validate_execution_inputs(daily_transform("epwshiftr")),
        "requires.*reference"
    )
})

test_that("daily temperature backend closes full-year mean and extrema targets", {
    context <- daily_backend_test__context(include_extrema = TRUE)
    baseline_data <- morph_test__baseline_weather(context)

    result <- morpher__run_context(context)
    weather <- result$data

    expect_s3_class(result, "epw_morph_result")
    expect_identical(nrow(weather), 8760L)
    expect_identical(nrow(result$factors), 365L)
    expect_identical(
        result$parts$component_pipeline$stage,
        WEATHER_COMPONENT_STAGES
    )
    expect_true(all(result$parts$component_pipeline$status == "ok"))
    expect_true(all(result$factors$dtr_status == "adjusted"))
    expect_true(all(result$factors$projection_status == "projected"))
    expect_lt(max(abs(result$factors$mean_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$minimum_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$maximum_closure_error)), 1e-8)

    achieved <- weather[,
        .(
            baseline_mean = mean(baseline_data$dry_bulb_temperature[.I]),
            baseline_minimum = min(baseline_data$dry_bulb_temperature[.I]),
            baseline_maximum = max(baseline_data$dry_bulb_temperature[.I]),
            projected_mean = mean(dry_bulb_temperature),
            projected_minimum = min(dry_bulb_temperature),
            projected_maximum = max(dry_bulb_temperature)
        ),
        by = "daily_target_day"
    ]
    expect_equal(
        achieved$projected_mean - achieved$baseline_mean,
        rep(2, 365L),
        tolerance = 1e-8
    )
    expect_equal(
        achieved$projected_minimum - achieved$baseline_minimum,
        rep(1, 365L),
        tolerance = 1e-8
    )
    expect_equal(
        achieved$projected_maximum - achieved$baseline_maximum,
        rep(3, 365L),
        tolerance = 1e-8
    )

    expect_true(all(
        weather$relative_humidity >= 0 &
            weather$relative_humidity <= 100
    ))
    expect_true(all(
        weather$dew_point_temperature <= weather$dry_bulb_temperature
    ))
    expect_equal(
        weather$daily_temperature_specific_humidity,
        weather$daily_temperature_baseline_specific_humidity,
        tolerance = 1e-12
    )
    expect_true(all(weather$daily_temperature_moisture_status == "inherited"))
    expect_identical(weather$wind_speed, baseline_data$wind_speed)
    expect_true(all(
        c(
            "daily_temperature_mean_delta",
            "daily_temperature_projection_status",
            "daily_temperature_boundary_jump_change"
        ) %in%
            names(weather)
    ))
})

test_that("registered daily recipe preserves the established backend output", {
    ad_hoc_context <- daily_backend_test__context(include_extrema = TRUE)
    registered_context <- ad_hoc_context
    registered_context$recipe <- epw_morph_recipe(
        name = "daily_temperature",
        policy = "harmonized",
        spec = "epwshiftr_daily_power"
    )

    ad_hoc <- morpher__run_context(ad_hoc_context)
    registered <- morpher__run_context(registered_context)

    expect_identical(
        registered$recipe$recipe_spec,
        "epwshiftr_daily_power"
    )
    expect_identical(registered$recipe$policy, "harmonized")
    expect_equal(registered$data, ad_hoc$data, tolerance = 0)
    expect_equal(registered$factors, ad_hoc$factors, tolerance = 0)
    expect_equal(
        registered$diagnostics,
        ad_hoc$diagnostics,
        tolerance = 0
    )
})

test_that("daily temperature backend clips inherited moisture at saturation", {
    context <- daily_backend_test__context(
        include_extrema = FALSE,
        mean_shift = -20
    )
    result <- morpher__run_context(context)
    clipped <- result$data$daily_temperature_moisture_status ==
        "saturation_clipped"

    expect_true(any(clipped))
    expect_equal(
        result$data$relative_humidity[clipped],
        rep(100, sum(clipped)),
        tolerance = 1e-8
    )
    expect_true(all(
        result$data$daily_temperature_specific_humidity <=
            result$data$daily_temperature_baseline_specific_humidity +
                1e-12
    ))
    expect_true(
        "daily_temperature_moisture_saturation_clipped" %in%
            result$diagnostics$code
    )
})

test_that("daily temperature backend records missing-extrema fallback and frequency errors", {
    context <- daily_backend_test__context(include_extrema = FALSE)
    baseline_data <- morph_test__baseline_weather(context)

    result <- morpher__run_context(context)

    expect_equal(
        result$data$dry_bulb_temperature,
        baseline_data$dry_bulb_temperature + 2,
        tolerance = 1e-10
    )
    expect_true(all(
        result$data$daily_temperature_dtr_status == "inherited_missing_extremes"
    ))
    expect_true(all(
        result$data$daily_temperature_projection_status == "shift_inherited_dtr"
    ))
    expect_true(
        "daily_temperature_dtr_inherited" %in% result$diagnostics$code
    )

    monthly_context <- daily_backend_test__context(
        include_extrema = FALSE,
        frequency = "mon"
    )
    expect_error(
        morpher__run_context(monthly_context),
        "frequencies.*mon.*day"
    )
})

test_that("daily adjusted temperature produces a standard EPW result via shared components", {
    recipe <- epw_morph_recipe("linear_scaling_daily_temperature")
    context <- daily_adjustment_test__context(recipe)
    result <- morpher__run_context(context)

    expect_s3_class(result, "epw_morph_result")
    expect_identical(nrow(result$data), 8760L)
    expect_identical(nrow(result$factors), 365L)
    expect_true(is.data.frame(result$parts$adjusted_series))
    expect_identical(
        result$parts$adjusted_series_metadata$output_role,
        "model_future"
    )
    expect_named(
        result$parts$adjusted_series_metadata,
        c("output_role", "transformation", "settings", "provenance")
    )
    pipeline <- result$parts$component_pipeline
    expect_identical(
        pipeline[stage == "signal", component],
        "linear_scaling_daily"
    )
    expect_identical(
        pipeline[stage == "hourly", component],
        "constrained_daily_temperature"
    )
    expect_identical(
        pipeline[stage == "physics", component],
        "specific_humidity_closure"
    )
    expect_identical(
        pipeline[stage == "output", component],
        "daily_temperature_epw_result"
    )
})

test_that("all daily adjustment methods produce the same output contract", {
    skip_on_cran()
    recipe_names <- paste0(
        unname(DAILY_ADJUSTMENT_METHOD_COMPONENTS),
        "_temperature"
    )
    results <- lapply(recipe_names, function(name) {
        recipe <- epw_morph_recipe(name)
        context <- daily_adjustment_test__context(recipe)
        suppressWarnings(morpher__run_context(context))
    })

    expect_true(all(vapply(
        results,
        inherits,
        logical(1L),
        what = "epw_morph_result"
    )))
    expect_identical(
        unique(vapply(
            results,
            function(result) nrow(result$data),
            integer(1L)
        )),
        8760L
    )
    expect_identical(
        unique(vapply(
            results,
            function(result) nrow(result$factors),
            integer(1L)
        )),
        365L
    )
    expect_true(all(vapply(
        results,
        function(result) {
            identical(
                result$parts$component_pipeline[
                    stage == "physics",
                    component
                ],
                "specific_humidity_closure"
            )
        },
        logical(1L)
    )))
})

test_that("daily temperature transform validates frequency and reconstructs", {
    transform <- daily_transform("epwshiftr", window_days = 15L)
    reference <- historical_reference(years = 1995:2014)
    daily_climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        frequency = "day",
        table = "day"
    )
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = daily_climate,
        periods = list(`2060s` = 2061L),
        transform = transform,
        reference = reference,
        dir = tempfile("daily-temperature-output-"),
        store = tempfile("daily-temperature-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
    rebuilt <- shift_persist__plan_from_spec(shift_persist__plan_spec(plan))

    expect_true(S7::S7_inherits(plan, ShiftPlan))
    expect_identical(plan@meta$recipe$backend, "daily_temperature")
    expect_identical(
        plan@meta$recipe$recipe_spec,
        "epwshiftr_daily_power"
    )
    expect_identical(plan@meta$recipe$policy, "harmonized")
    expect_identical(rebuilt@meta$recipe$options$window_days, 15L)
    expect_identical(
        rebuilt@meta$recipe$recipe_spec,
        plan@meta$recipe$recipe_spec
    )
    expect_identical(
        rebuilt@meta$recipe$components,
        plan@meta$recipe$components
    )
    expect_silent(shift_job__validate_background_plan(plan))
    inferred <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6("EC-Earth3", "ssp585"),
        periods = list(`2060s` = 2061L),
        transform = transform,
        reference = reference,
        dir = tempfile("daily-temperature-output-"),
        store = tempfile("daily-temperature-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
    expect_identical(
        unique(unname(inferred@meta$request@meta$frequency)),
        "day"
    )
})

test_that("daily temperature backend runs and resumes through EpwMorpher", {
    skip_on_cran()

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(
        nc,
        2061L,
        variable_id = "tas"
    )
    on.exit(unlink(nc), add = TRUE)

    store <- EsgStore$new(tempfile("daily-temperature-workflow-"))
    on.exit(store$close(), add = TRUE)
    docs <- esgf_test__file_docs(
        basename(nc),
        opendap_url = nc,
        download_url = nc,
        variable_id = "tas",
        datetime_start = "2061-01-01T00:00:00Z",
        datetime_end = "2061-12-31T23:59:59Z"
    )
    query_id <- store$add_files(esgf_test__file_result(docs))
    extraction <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c(
            "2061-01-01T00:00:00Z",
            "2061-12-31T23:59:59Z"
        ),
        site_id = "SIN",
        variable_id = "tas"
    )
    expect_true(all(
        store$extract(plan_id = extraction$plan_id)$status == "done"
    ))

    morpher <- epw_morpher(
        store,
        epw = get_cache_epw(),
        site_id = "SIN",
        transform = daily_transform(
            "epwshiftr",
            reconstruction = "power"
        )
    )
    periods <- epw_morph_periods(`2060s` = 2061L)
    workflow <- morpher$workflow(
        plan_id = extraction$plan_id,
        periods = periods,
        reference_plan_id = extraction$plan_id,
        reference_periods = periods,
        strict = TRUE,
        dir = "outputs/daily-temperature",
        separate = FALSE,
        overwrite = TRUE
    )
    result_path <- store_abs_path(
        workflow$results$output_path,
        root = store$path
    )
    weather <- read_test_parquet(result_path)
    resumed <- morpher$run(
        workflow$plan$morph_id,
        overwrite = FALSE,
        resume = TRUE
    )

    expect_identical(nrow(weather), 8760L)
    expect_true("daily_temperature_mean_delta" %in% names(weather))
    expect_equal(
        weather$daily_temperature_mean_delta,
        rep(0, 8760L),
        tolerance = 1e-10
    )
    expect_true(file.exists(store_abs_path(
        workflow$outputs$path,
        root = store$path
    )))
    expect_identical(resumed$result_id, workflow$results$result_id)
})

# vim: fdm=marker :
