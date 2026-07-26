# Build deterministic native-calendar daily extrema for the Ek factor tests.
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
    years <- as.integer(years)
    calendar_days <- as.integer(calendar_days)
    phase <- (seq_len(calendar_days) - 0.5) / calendar_days
    rows <- lapply(seq_along(years), function(index) {
        replicate_offset <- (index - mean(seq_along(years))) * 0.2
        mean_temperature <- 12 +
            7 * sin(2 * pi * phase) +
            replicate_offset +
            temperature_shift
        baseline_dtr <- if (identical(historical_dtr, 0)) {
            rep.int(0, calendar_days)
        } else {
            historical_dtr + 1.5 * cos(2 * pi * phase)
        }
        dtr <- baseline_dtr * dtr_ratio
        values <- list(
            tasmin = mean_temperature - dtr / 2 + 273.15,
            tasmax = mean_temperature + dtr / 2 + 273.15
        )
        data.table::rbindlist(lapply(
            setdiff(names(values), omit),
            function(variable_id) {
                data.table::data.table(
                    activity_id = if (identical(
                        experiment,
                        "historical"
                    )) {
                        "CMIP"
                    } else {
                        "ScenarioMIP"
                    },
                    institution_id = "PCIC-test",
                    source_id = "TestModel",
                    experiment_id = experiment,
                    variant_label = "r1i1p1f1",
                    frequency = frequency,
                    table_id = "day",
                    variable_id = variable_id,
                    time = as.POSIXct(
                        sprintf("%04d-01-01 12:00:00", years[[index]]),
                        tz = "UTC"
                    ) + seq.int(0L, calendar_days - 1L) * 86400,
                    year = years[[index]],
                    annual_phase = phase,
                    period = period,
                    lon = -123,
                    lat = 49,
                    units = "K",
                    value = values[[variable_id]]
                )
            }
        ))
    })
    data.table::rbindlist(rows)
}

# Build a complete Ek backend context around the packaged EPW fixture.
ek_test__context <- function(
    temperature_shift = 0,
    dtr_ratio = 1,
    historical_dtr = 8,
    historical_days = 365L,
    future_days = 365L,
    policy = "paper_faithful"
) {
    historical <- ek_test__climate(
        2001:2003,
        period = "reference",
        experiment = "historical",
        calendar_days = historical_days,
        historical_dtr = historical_dtr
    )
    future <- ek_test__climate(
        2061:2063,
        period = "2060s",
        experiment = "ssp585",
        calendar_days = future_days,
        temperature_shift = temperature_shift,
        dtr_ratio = dtr_ratio,
        historical_dtr = historical_dtr
    )
    morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        recipe = epw_morph_recipe(
            "ek_daily_factors",
            policy = policy
        )
    )
}

test_that("Ek recipe registers its temperature-focused daily contract", {
    expect_true("ek_daily_temperature" %in% epw_morph_backends())
    expect_true("ek_daily_factors" %in% epw_morph_recipes()[["name"]])
    backend <- epw_morph_backend("ek_daily_temperature")
    recipe <- epw_morph_recipe("ek_daily_factors")
    spec <- epw_morph_recipe_spec("ek_daily_factors")

    expect_true(backend$requires_reference)
    expect_identical(recipe$policy, "paper_faithful")
    expect_identical(
        names(spec@policy_profiles),
        c("paper_faithful", "harmonized")
    )
    expect_identical(
        epw_morph_variables(recipe),
        c("tasmin", "tasmax")
    )
    expect_identical(
        spec@calendar_policy,
        "cf_yearly_linear_to_epw_365"
    )
    expect_identical(
        recipe$components$signal,
        "ek_daily_temperature_factors"
    )
    expect_identical(
        recipe$components$hourly,
        "ek_daily_combined_temperature"
    )
    expect_match(spec@source$ambiguity_note, "not fully self-consistent")
    expect_match(spec@source$implementation_note, "temperature")
    expect_identical(
        morpher__recipe_required_frequency(recipe),
        "day"
    )
    expect_error(
        ek_daily_temperature(),
        "requires an explicit reference"
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
    targets <- ek__daily_temperature_targets(
        daily__temperature_backend_climate(
            future,
            "future climate"
        ),
        daily__temperature_backend_climate(
            historical,
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
    context <- ek_test__context(
        temperature_shift = 2,
        dtr_ratio = 1.25
    )
    result <- morpher__run_context(context)
    factors <- result$factors

    expect_equal(
        factors$mean_delta,
        rep(2, 365L),
        tolerance = 1e-10
    )
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
    expect_true(all(factors$projection_status == "ek_combined"))
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
    historical <- ek_test__climate(
        2001:2002,
        period = "reference",
        experiment = "historical",
        historical_dtr = 0
    )
    future <- ek_test__climate(
        2061:2062,
        period = "2060s",
        experiment = "ssp585",
        temperature_shift = 2,
        historical_dtr = 0
    )
    context <- morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        recipe = epw_morph_recipe("ek_daily_factors")
    )
    result <- morpher__run_context(context)

    expect_true(all(
        result$factors$dtr_status ==
            "inherited_zero_historical_dtr"
    ))
    expect_true(all(
        result$factors$projection_status ==
            "mean_shift_zero_historical_dtr"
    ))
    expect_true(
        "ek_zero_historical_model_dtr" %in%
            result$diagnostics$code
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
        ek__daily_temperature_climatology(
            missing_year,
            "historical climate"
        ),
        "source-calendar year"
    )
    expect_error(
        daily__temperature_backend_climate(
            wrong_frequency,
            "historical climate"
        ),
        "must use CMIP frequency"
    )
    expect_error(
        ek__daily_temperature_climatology(
            duplicated,
            "historical climate"
        ),
        "one value per annual phase"
    )
})

test_that("Ek public method survives dry-run plan reconstruction", {
    method <- ek_daily_temperature(
        historical_reference(years = 1995:2014),
        policy = "harmonized"
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
        dir = tempfile("ek-daily-output-"),
        store = tempfile("ek-daily-store-"),
        dry_run = TRUE
    )
    rebuilt <- shift__plan_from_spec(shift__plan_spec(plan))

    expect_identical(
        plan@meta$method@recipe$backend,
        "ek_daily_temperature"
    )
    expect_identical(
        rebuilt@meta$method@recipe$recipe_spec,
        "ek_daily_factors"
    )
    expect_identical(
        rebuilt@meta$method@recipe$components$signal,
        "ek_daily_temperature_factors"
    )
    expect_identical(
        rebuilt@meta$method@recipe$policy,
        "harmonized"
    )
    expect_silent(shift__validate_background_plan(plan))
})
