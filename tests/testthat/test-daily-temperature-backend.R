# Build deterministic daily CMIP temperature rows with complete canonical
# context columns for backend and workflow tests.
daily_backend_test__climate <- function(
    years, period, experiment, mean_shift = 0,
    minimum_shift = 0, maximum_shift = 0,
    include_extrema = TRUE, frequency = "day"
) {
    years <- as.integer(years)
    phase <- daily__phase_grid(365L)
    rows <- lapply(years, function(year) {
        time <- as.POSIXct(
            as.Date(sprintf("%04d-01-01", year)) + seq.int(0L, 364L),
            tz = "UTC"
        ) + 12 * 3600
        seasonal <- 7 * sin(2 * pi * phase)
        values <- list(tas = 20 + seasonal + mean_shift)
        if (isTRUE(include_extrema)) {
            values$tasmin <- 20 + seasonal - 4 + minimum_shift
            values$tasmax <- 20 + seasonal + 5 + maximum_shift
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
                frequency = frequency,
                table_id = "day",
                variable_id = variable_id,
                time = time,
                year = year,
                annual_phase = phase,
                period = period,
                lon = 104,
                lat = 1.37,
                units = "K",
                value = values[[variable_id]] + 273.15
            )
        }))
    })
    data.table::rbindlist(rows)
}

# Build one direct backend context from the packaged EPW fixture and matching
# future/historical daily climate rows.
daily_backend_test__context <- function(
    include_extrema = TRUE, frequency = "day", mean_shift = 2,
    minimum_shift = 1, maximum_shift = 3
) {
    historical <- daily_backend_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical",
        include_extrema = include_extrema,
        frequency = frequency
    )
    future <- daily_backend_test__climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        mean_shift = mean_shift,
        minimum_shift = minimum_shift,
        maximum_shift = maximum_shift,
        include_extrema = include_extrema,
        frequency = frequency
    )
    morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        recipe = epw_morph_recipe("daily_temperature")
    )
}

test_that("daily temperature backend is registered with a daily reference contract", {
    expect_true("daily_temperature" %in% epw_morph_backends())
    backend <- epw_morph_backend("daily_temperature")
    expect_true(backend$requires_reference)
    expect_true(backend$accepts_reference)
    expect_true(S7::S7_inherits(
        backend$component_pipeline(),
        WeatherPipelineSpec
    ))
    plan <- pipeline__compile(
        backend$component_pipeline(),
        daily_backend_test__context()$inputs
    )
    expect_true(S7::S7_inherits(plan, WeatherPipelinePlan))
    expect_identical(names(plan@components), WEATHER_COMPONENT_STAGES)
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
    expect_identical(
        names(recipe$components),
        WEATHER_COMPONENT_STAGES
    )
    expect_identical(
        morpher__recipe_required_frequency(recipe),
        "day"
    )
    expect_error(
        epw_morph_recipe(
            "daily_temperature",
            options = list(window_days = 14L)
        ),
        "must be odd"
    )
    expect_error(daily_temperature(), "requires an explicit reference")
})

test_that("daily temperature CLI options retain their numeric types", {
    options <- cli_shift__recipe_options(
        list(window_days = "15", tolerance = "0.000001"),
        "daily_temperature"
    )

    expect_identical(options$window_days, 15L)
    expect_identical(options$tolerance, 1e-6)
    expect_error(
        cli_shift__recipe_options(
            list(window_days = "not-a-number"),
            "daily_temperature"
        ),
        "window_days must be an odd integer"
    )
    expect_error(
        cli_shift__recipe_options(
            list(tolerance = "-1"),
            "daily_temperature"
        ),
        "tolerance must be a non-negative number"
    )
})

test_that("daily temperature backend closes full-year mean and extrema targets", {
    context <- daily_backend_test__context(include_extrema = TRUE)
    baseline <- context$epw$clone()
    suppressMessages(baseline$drop_unit())
    baseline_data <- data.table::as.data.table(baseline$data())

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

    achieved <- weather[, .(
        baseline_mean = mean(baseline_data$dry_bulb_temperature[.I]),
        baseline_minimum = min(baseline_data$dry_bulb_temperature[.I]),
        baseline_maximum = max(baseline_data$dry_bulb_temperature[.I]),
        projected_mean = mean(dry_bulb_temperature),
        projected_minimum = min(dry_bulb_temperature),
        projected_maximum = max(dry_bulb_temperature)
    ), by = "daily_target_day"]
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

    expect_true(all(weather$relative_humidity >= 0 &
        weather$relative_humidity <= 100))
    expect_true(all(weather$dew_point_temperature <=
        weather$dry_bulb_temperature))
    expect_equal(
        weather$daily_temperature_specific_humidity,
        weather$daily_temperature_baseline_specific_humidity,
        tolerance = 1e-12
    )
    expect_true(all(weather$daily_temperature_moisture_status == "inherited"))
    expect_identical(weather$wind_speed, baseline_data$wind_speed)
    expect_true(all(c(
        "daily_temperature_mean_delta",
        "daily_temperature_projection_status",
        "daily_temperature_boundary_jump_change"
    ) %in% names(weather)))
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
    baseline <- context$epw$clone()
    suppressMessages(baseline$drop_unit())

    result <- morpher__run_context(context)

    expect_equal(
        result$data$dry_bulb_temperature,
        baseline$data()$dry_bulb_temperature + 2,
        tolerance = 1e-10
    )
    expect_true(all(
        result$data$daily_temperature_dtr_status ==
            "inherited_missing_extremes"
    ))
    expect_true(all(
        result$data$daily_temperature_projection_status ==
            "shift_inherited_dtr"
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

test_that("daily temperature shift method validates frequency and reconstructs", {
    method <- daily_temperature(
        historical_reference(years = 1995:2014),
        window_days = 15L
    )
    daily_climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        frequency = "day",
        table = "day"
    )
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = daily_climate,
        periods = list(`2060s` = 2061L),
        method = method,
        dir = tempfile("daily-temperature-output-"),
        store = tempfile("daily-temperature-store-"),
        dry_run = TRUE
    )
    rebuilt <- shift__plan_from_spec(shift__plan_spec(plan))

    expect_true(S7::S7_inherits(plan, ShiftPlan))
    expect_identical(plan@meta$method@recipe$backend, "daily_temperature")
    expect_identical(
        rebuilt@meta$method@recipe$options$window_days,
        15L
    )
    expect_identical(
        rebuilt@meta$method@recipe$components,
        plan@meta$method@recipe$components
    )
    expect_silent(shift__validate_background_plan(plan))
    expect_error(
        shift_future_epw(
            epw = get_cache_epw(),
            climate = shift_cmip6("EC-Earth3", "ssp585"),
            periods = list(`2060s` = 2061L),
            method = method,
            dir = tempfile("daily-temperature-output-"),
            store = tempfile("daily-temperature-store-"),
            dry_run = TRUE
        ),
        "requires CMIP frequency.*day"
    )
})

test_that("daily temperature backend runs and resumes through EpwMorpher", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(
        nc,
        2061L,
        variable_id = "tas"
    )
    on.exit(unlink(nc), add = TRUE)

    store <- EsgStore$new(tempfile("daily-temperature-workflow-"))
    on.exit(store$close(), add = TRUE)
    docs <- cli_shift_test_file_docs(
        basename(nc),
        opendap_url = nc,
        download_url = nc,
        variable_id = "tas",
        datetime_start = "2061-01-01T00:00:00Z",
        datetime_end = "2061-12-31T23:59:59Z"
    )
    query_id <- store$add_files(cli_shift_test_file_result(docs))
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
        recipe = epw_morph_recipe("daily_temperature")
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
