# Build deterministic daily temperature rows for the complete BTWS backend.
btws_test__climate <- function(
    years,
    period,
    experiment,
    mean_shift = 0,
    minimum_shift = 0,
    maximum_shift = 0
) {
    phase <- daily__phase_grid(365L)
    data.table::rbindlist(lapply(as.integer(years), function(year) {
        time <- as.POSIXct(
            as.Date(sprintf("%04d-01-01", year)) + seq.int(0L, 364L),
            tz = "UTC"
        ) + 12 * 3600
        seasonal <- 7 * sin(2 * pi * phase)
        values <- list(
            tas = 20 + seasonal + mean_shift,
            tasmin = 16 + seasonal + minimum_shift,
            tasmax = 25 + seasonal + maximum_shift
        )
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
                frequency = "day",
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
    }))
}

# Assemble one direct role-addressable context from the packaged EPW fixture.
btws_test__context <- function(
    mean_shift = 0.5,
    minimum_shift = 0,
    maximum_shift = 0
) {
    historical <- btws_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical"
    )
    future <- btws_test__climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        mean_shift = mean_shift,
        minimum_shift = minimum_shift,
        maximum_shift = maximum_shift
    )
    morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        recipe = epw_morph_recipe("epwshiftr_daily_btws")
    )
}

test_that("Eames BTWS backend and composite recipe expose strict contracts", {
    expect_true("daily_btws" %in% epw_morph_backends())
    expect_true("epwshiftr_daily_btws" %in% epw_morph_recipes()[["name"]])

    backend <- epw_morph_backend("daily_btws")
    recipe <- epw_morph_recipe("epwshiftr_daily_btws")
    spec <- epw_morph_recipe_spec("epwshiftr_daily_btws")

    expect_true(backend$requires_reference)
    expect_equal(
        backend$required_variables(),
        c("tas", "tasmin", "tasmax")
    )
    expect_equal(
        epw_morph_variables(recipe),
        c("tas", "tasmin", "tasmax")
    )
    expect_identical(recipe$backend, "daily_btws")
    expect_identical(recipe$policy, "harmonized")
    expect_identical(recipe$recipe_spec, "epwshiftr_daily_btws")
    expect_identical(
        recipe$components$hourly,
        "eames_btws_temperature"
    )
    expect_identical(spec@source$type, "combined_prior_methods")
    expect_match(spec@source$citation, "combined")
    expect_match(spec@source$equation_note, "bisection")
    expect_identical(
        morpher__recipe_required_frequency(recipe),
        "day"
    )
    expect_error(daily_btws(), "requires an explicit reference")
})

test_that("Eames BTWS backend closes a complete future EPW year", {
    context <- btws_test__context()
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
            result$parts$component_pipeline$stage == "hourly"
        ],
        "eames_btws_temperature"
    )
    expect_lt(max(abs(result$factors$mean_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$minimum_closure_error)), 1e-8)
    expect_lt(max(abs(result$factors$maximum_closure_error)), 1e-8)
    expect_false(any(!is.na(result$factors$btws_fallback_reason)))
    expect_true(all(is.finite(result$factors$btws_scale)))
    expect_true(all(is.finite(result$factors$btws_m)))
    expect_true(all(is.finite(result$factors$btws_n)))
    expect_true(all(c(
        "eames_btws_scale",
        "eames_btws_m",
        "eames_btws_n",
        "eames_btws_fallback_reason"
    ) %in% names(weather)))
    expect_false(
        "daily_temperature_shape_exponent" %in% names(weather)
    )

    achieved <- weather[, .(
        baseline_mean = mean(baseline_data$dry_bulb_temperature[.I]),
        projected_mean = mean(dry_bulb_temperature)
    ), by = "daily_target_day"]
    expect_equal(
        achieved$projected_mean - achieved$baseline_mean,
        rep(0.5, 365L),
        tolerance = 1e-8
    )
    expect_true(all(weather$relative_humidity >= 0 &
        weather$relative_humidity <= 100))
    expect_true(all(weather$dew_point_temperature <=
        weather$dry_bulb_temperature))
})

test_that("daily_btws method validates and survives plan reconstruction", {
    method <- daily_btws(
        historical_reference(years = 1995:2014),
        window_days = 15L
    )
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        frequency = "day",
        table = "day"
    )
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = climate,
        periods = list(`2060s` = 2061L),
        method = method,
        dir = tempfile("daily-btws-output-"),
        store = tempfile("daily-btws-store-"),
        dry_run = TRUE
    )
    rebuilt <- shift__plan_from_spec(shift__plan_spec(plan))

    expect_identical(plan@meta$method@recipe$backend, "daily_btws")
    expect_identical(
        plan@meta$method@recipe$recipe_spec,
        "epwshiftr_daily_btws"
    )
    expect_identical(
        rebuilt@meta$method@recipe$options$window_days,
        15L
    )
    expect_identical(
        rebuilt@meta$method@recipe$components$hourly,
        "eames_btws_temperature"
    )
    expect_silent(shift__validate_background_plan(plan))
})
