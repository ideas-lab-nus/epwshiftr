# Build deterministic daily tas rows for model and observed Arima inputs.
arima_test__climate <- function(
    years,
    period,
    experiment,
    temperature_shift = 0,
    frequency = "day",
    source_id = "TestModel"
) {
    rows <- lapply(as.integer(years), function(year) {
        dates <- seq.Date(
            as.Date(sprintf("%d-01-01", year)),
            as.Date(sprintf("%d-12-31", year)),
            by = "day"
        )
        # Exclude leap day so test inputs retain one unambiguous annual phase.
        dates <- dates[format(dates, "%m-%d") != "02-29"]
        day <- seq_along(dates)
        phase <- (day - 0.5) / length(day)
        temperature <- 12 +
            8 * sin(2 * pi * phase) +
            temperature_shift
        data.table::data.table(
            activity_id = if (identical(
                experiment,
                "historical"
            )) {
                "CMIP"
            } else {
                "ScenarioMIP"
            },
            institution_id = "test",
            source_id = source_id,
            experiment_id = experiment,
            variant_label = "r1i1p1f1",
            frequency = frequency,
            table_id = "day",
            variable_id = "tas",
            time = as.POSIXct(dates, tz = "UTC") + 12 * 3600,
            year = year,
            month = as.integer(format(dates, "%m")),
            annual_phase = phase,
            period = period,
            lon = 103.8,
            lat = 1.3,
            units = "K",
            value = temperature + 273.15
        )
    })
    data.table::rbindlist(rows)
}

# Build a complete four-role context around the packaged EPW fixture.
arima_test__context <- function(
    temperature_shift = 0,
    policy = "paper_faithful"
) {
    historical <- arima_test__climate(
        2001:2003,
        period = "reference",
        experiment = "historical"
    )
    future <- arima_test__climate(
        2061:2063,
        period = "2060s",
        experiment = "ssp585",
        temperature_shift = temperature_shift
    )
    observed <- arima_test__climate(
        2001:2003,
        period = "observed",
        experiment = "observed",
        source_id = "Observed"
    )
    morpher__context(
        epw = epw_file_read(get_cache_epw()),
        climate = future,
        reference_climate = historical,
        observed_reference = observed,
        recipe = epw_morph_recipe(
            "monthly_percentile_temperature",
            policy = policy
        )
    )
}

test_that("Arima recipe registers all four required input roles", {
    expect_true("arima_temperature" %in% epw_morph_backends())
    expect_true(
        "monthly_percentile_temperature" %in% epw_morph_recipes()[["name"]]
    )
    recipe <- epw_morph_recipe("monthly_percentile_temperature")
    spec <- epw_morph_recipe_spec("monthly_percentile_temperature")

    expect_identical(recipe$policy, "paper_faithful")
    expect_identical(
        names(spec@required_inputs),
        c(
            "weather_template",
            "observed_reference",
            "model_historical",
            "model_future"
        )
    )
    expect_true(morpher__recipe_requires_reference(recipe))
    expect_true(morpher__recipe_requires_observed_reference(recipe))
    expect_identical(epw_morph_variables(recipe), "tas")
    expect_identical(
        recipe$components$signal,
        "percentile_temperature_change_function"
    )
    expect_identical(
        morpher__recipe_required_frequency(recipe),
        "day"
    )
    expect_error(
        arima_temperature(),
        "requires an explicit reference"
    )
})

test_that("Arima smoother uses fixed nine-rank endpoint means", {
    expected_one_pass <- c(
        rep(5, 5),
        6:12,
        rep(13, 5)
    )
    expect_equal(
        arima__smooth_pass(1:17),
        expected_one_pass
    )
    expect_equal(
        arima__smooth_change(rep(2.5, 17)),
        rep(2.5, 17)
    )
    expect_error(
        arima__smooth_change(1:8),
        "no longer than"
    )
})

test_that("Arima monthly inputs do not require an annual-phase mapping", {
    source <- arima_test__climate(
        2001L,
        period = "observed",
        experiment = "observed",
        source_id = "Observed"
    )
    source[, annual_phase := NULL]
    normalized <- arima__temperature_series(
        source,
        "observed reference weather"
    )

    expect_identical(nrow(normalized), 365L)
    expect_identical(sort(unique(normalized$month)), 1:12)
    expect_true(all(is.finite(normalized$value)))
})

test_that("Arima zero climate change is an hourly identity", {
    context <- arima_test__context()
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
        result$factors$temperature_delta,
        rep(0, 365L),
        tolerance = 1e-10
    )
    expect_identical(
        result$parts$settings$quantile_type,
        EPW_MORPH_ARIMA_QUANTILE_TYPE
    )
})

test_that("Arima applies the selected daily factor to every hour", {
    context <- arima_test__context(temperature_shift = 2.25)
    result <- morpher__run_context(context)
    baseline <- context$epw$clone()
    suppressMessages(baseline$drop_unit())
    hourly_delta <- result$data$dry_bulb_temperature -
        baseline$data()[["dry_bulb_temperature"]]

    expect_equal(hourly_delta, rep(2.25, 8760L), tolerance = 1e-10)
    expect_equal(
        result$factors$temperature_delta,
        rep(2.25, 365L),
        tolerance = 1e-10
    )
    expect_true(all(
        result$parts$temperature[
            ,
            data.table::uniqueN(temperature_delta),
            by = "target_day"
        ][["V1"]] == 1L
    ))
})

test_that("Arima factors vary with observed monthly percentile", {
    historical <- data.table::data.table(
        month = rep(seq_len(12L), each = 20L),
        value = rep(seq_len(20L), 12L)
    )
    future <- data.table::copy(historical)
    future[, value := value + value / 10]
    observed <- data.table::copy(historical)
    functions <- arima__change_functions(historical, future)
    baseline <- data.table::data.table(
        target_day = 1:24,
        month = rep(seq_len(12L), each = 2L),
        baseline_daily_mean = rep(c(2, 19), 12L)
    )
    factors <- arima__daily_factors(
        baseline,
        observed,
        functions
    )

    low <- factors[seq.int(1L, 24L, by = 2L)]
    high <- factors[seq.int(2L, 24L, by = 2L)]
    expect_true(all(high$observed_percentile > low$observed_percentile))
    expect_true(all(high$temperature_delta > low$temperature_delta))
    expect_true(all(functions$smoothed_delta >= 0))
})

test_that("Arima harmonized policy closes the humidity state", {
    result <- morpher__run_context(
        arima_test__context(
            temperature_shift = -12,
            policy = "harmonized"
        )
    )

    expect_true(all(result$data$relative_humidity >= 0))
    expect_true(all(result$data$relative_humidity <= 100))
    expect_true(all(
        result$data$dew_point_temperature <=
            result$data$dry_bulb_temperature + 1e-8
    ))
    expect_true(
        "arima_humidity_closure_status" %in% names(result$data)
    )
})

test_that("Arima public method persists both reference roles", {
    historical <- historical_reference(years = 1995:2014)
    observed <- shift_reference_plan(
        "observed-plan",
        periods = epw_morph_periods(observed = 1995:2014)
    )
    method <- arima_temperature(
        reference = historical,
        observed_reference = observed,
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
        dir = tempfile("arima-output-"),
        store = tempfile("arima-store-"),
        dry_run = TRUE
    )
    rebuilt <- shift__plan_from_spec(shift__plan_spec(plan))
    explanation <- shift__plan_explain(plan)

    expect_identical(
        rebuilt@meta$method@recipe$recipe_spec,
        "monthly_percentile_temperature"
    )
    expect_identical(
        rebuilt@meta$method@observed_reference@plan_id,
        "observed-plan"
    )
    expect_true("observed_reference" %in% explanation$step)
    expect_silent(shift__validate_background_plan(plan))
})

test_that("EpwMorpher persists and executes the observed reference separately", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    years <- c(
        historical = 2001L,
        observed = 2002L,
        future = 2061L
    )
    paths <- stats::setNames(
        vapply(names(years), function(role) {
            path <- tempfile(sprintf("arima-%s-", role), fileext = ".nc")
            write_local_cmip6_netcdf_fixture(
                path,
                years[[role]],
                variable_id = "tas"
            )
            path
        }, character(1L)),
        names(years)
    )
    on.exit(unlink(paths), add = TRUE)

    store <- EsgStore$new(tempfile("arima-workflow-"))
    on.exit(store$close(), add = TRUE)
    plans <- lapply(names(years), function(role) {
        year <- years[[role]]
        docs <- cli_shift_test_file_docs(
            basename(paths[[role]]),
            opendap_url = paths[[role]],
            download_url = paths[[role]],
            variable_id = "tas",
            datetime_start = sprintf(
                "%d-01-01T00:00:00Z",
                year
            ),
            datetime_end = sprintf(
                "%d-12-31T23:59:59Z",
                year
            )
        )
        query_id <- store$add_files(cli_shift_test_file_result(docs))
        plan <- store$plan_region(
            query_id = query_id,
            lon = 103.98,
            lat = 1.37,
            time = c(
                sprintf("%d-01-01T00:00:00Z", year),
                sprintf("%d-12-31T23:59:59Z", year)
            ),
            site_id = "SIN",
            variable_id = "tas"
        )
        processed <- store$extract(plan_id = plan$plan_id)
        completed <- processed[processed$status == "done"]
        if ("year" %in% names(completed)) {
            completed <- completed[completed$year == year]
        }
        expect_true(
            nrow(completed) >= 1L,
            info = paste(
                processed$status,
                processed$last_error,
                collapse = " | "
            )
        )
        completed$plan_id
    })
    names(plans) <- names(years)

    morpher <- epw_morpher(
        store,
        epw = get_cache_epw(),
        site_id = "SIN",
        recipe = epw_morph_recipe("monthly_percentile_temperature")
    )
    workflow <- morpher$workflow(
        plan_id = plans$future,
        periods = epw_morph_periods(`2060s` = years[["future"]]),
        reference_plan_id = plans$historical,
        reference_periods = epw_morph_periods(
            reference = years[["historical"]]
        ),
        observed_plan_id = plans$observed,
        observed_periods = epw_morph_periods(
            observed = years[["observed"]]
        ),
        strict = TRUE,
        dir = NULL,
        overwrite = TRUE
    )
    mapping <- morpher__read_table(
        store,
        "epw_morph_observed_reference"
    )
    result_path <- store_abs_path(
        workflow$results$output_path,
        root = store$path
    )
    weather <- read_test_parquet(result_path)

    observed_mapping <- mapping[
        morph_id == workflow$plan$morph_id
    ]
    expect_identical(nrow(observed_mapping), 1L)
    expect_true(nzchar(observed_mapping$observed_summary_id))
    expect_identical(nrow(weather), 8760L)
    expect_true("arima_temperature_delta" %in% names(weather))
    expect_equal(
        weather$arima_temperature_delta,
        rep(0, 8760L),
        tolerance = 1e-7
    )
})
