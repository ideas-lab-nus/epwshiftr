# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

# Build a complete four-role context around the packaged EPW fixture.
# quantile_mapping_morphing_test__context {{{
quantile_mapping_morphing_test__context <- function(
    temperature_shift = 0,
    policy = "paper_faithful",
    recipe_name = "quantile_mapping_morphing_daily"
) {
    make_series <- function(
        years,
        period,
        experiment,
        shift = 0,
        source = "TestModel"
    ) {
        morph_test__daily_climate(
            years = years,
            period = period,
            experiment = experiment,
            variables = "tas",
            mean_shift = shift,
            base_temp = 12,
            seasonal_amp = 8,
            source_id = source,
            institution_id = "test",
            lon = 103.8,
            lat = 1.3,
            include_month = TRUE
        )
    }
    morph_test__context(
        climate = make_series(
            2061:2063,
            "2060s",
            "ssp585",
            shift = temperature_shift
        ),
        reference_climate = make_series(2001:2003, "reference", "historical"),
        observed_reference = make_series(
            2001:2003,
            "observed",
            "observed",
            source = "Observed"
        ),
        recipe = epw_morph_recipe(recipe_name, policy = policy)
    )
}
# }}}

test_that("Quantile-mapping morphing requires both historical and observed references", {
    recipe <- epw_morph_recipe("quantile_mapping_morphing_daily")
    expect_true(morpher__recipe_requires_reference(recipe))
    expect_true(morpher__recipe_requires_observed_reference(recipe))
    expect_error(
        transform__validate_execution_inputs(daily_transform("qm_morphing")),
        "requires.*reference"
    )
})

test_that("Quantile-mapping morphing smoother uses fixed nine-rank endpoint means", {
    expected_one_pass <- c(
        rep(5, 5),
        6:12,
        rep(13, 5)
    )
    expect_equal(
        quantile_mapping_morphing__smooth_pass(1:17),
        expected_one_pass
    )
    expect_equal(
        quantile_mapping_morphing__smooth_change(rep(2.5, 17)),
        rep(2.5, 17)
    )
    expect_error(
        quantile_mapping_morphing__smooth_change(1:8),
        "no longer than"
    )
})

test_that("Quantile-mapping morphing monthly inputs do not require an annual-phase mapping", {
    source <- morph_test__daily_climate(
        2001L,
        period = "observed",
        experiment = "observed",
        variables = "tas",
        base_temp = 12,
        seasonal_amp = 8,
        source_id = "Observed",
        institution_id = "test",
        lon = 103.8,
        lat = 1.3,
        include_month = TRUE
    )
    source[, annual_phase := NULL]
    normalized <- quantile_mapping_morphing__temperature_series(
        source,
        "observed reference weather"
    )

    expect_identical(nrow(normalized), 365L)
    expect_identical(sort(unique(normalized$month)), 1:12)
    expect_true(all(is.finite(normalized$value)))
})

test_that("Quantile-mapping morphing zero climate change is an hourly identity", {
    context <- quantile_mapping_morphing_test__context()
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
        result$factors$temperature_delta,
        rep(0, 365L),
        tolerance = 1e-10
    )
    expect_identical(
        result$parts$settings$quantile_type,
        EPW_MORPH_QUANTILE_MAPPING_QUANTILE_TYPE
    )
})

test_that("Quantile-mapping morphing applies the selected daily factor to every hour", {
    context <- quantile_mapping_morphing_test__context(temperature_shift = 2.25)
    result <- morpher__run_context(context)
    baseline_weather <- morph_test__baseline_weather(context)
    hourly_delta <- result$data$dry_bulb_temperature -
        baseline_weather[["dry_bulb_temperature"]]

    expect_equal(hourly_delta, rep(2.25, 8760L), tolerance = 1e-10)
    expect_equal(
        result$factors$temperature_delta,
        rep(2.25, 365L),
        tolerance = 1e-10
    )
    expect_true(all(
        result$parts$temperature[,
            data.table::uniqueN(temperature_delta),
            by = "target_day"
        ][["V1"]] ==
            1L
    ))
})

test_that("Quantile-mapping morphing factors vary with observed monthly percentile", {
    historical <- data.table::data.table(
        month = rep(seq_len(12L), each = 20L),
        value = rep(seq_len(20L), 12L)
    )
    future <- data.table::copy(historical)
    future[, value := value + value / 10]
    observed <- data.table::copy(historical)
    functions <- quantile_mapping_morphing__change_functions(historical, future)
    baseline <- data.table::data.table(
        target_day = 1:24,
        month = rep(seq_len(12L), each = 2L),
        baseline_daily_mean = rep(c(2, 19), 12L)
    )
    factors <- quantile_mapping_morphing__daily_factors(
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

test_that("Quantile-mapping morphing harmonized policy closes the humidity state", {
    result <- morpher__run_context(
        quantile_mapping_morphing_test__context(
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
        "quantile_mapping_morphing_humidity_closure_status" %in%
            names(result$data)
    )
})

test_that("Quantile-mapping morphing public method persists both reference roles", {
    historical <- historical_reference(years = 1995:2014)
    observed <- shift_reference_from_plan(
        "observed-plan",
        periods = epw_morph_periods(observed = 1995:2014),
        role = "observed_reference"
    )
    transform <- daily_transform("qm_morphing")
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        frequency = "day",
        table = "day"
    )
    periods <- epw_morph_periods(`2060s` = 2061L)
    plan <- shift_spec__plan(
        request = shift_spec__request_from_cmip6(climate, periods, transform),
        site = shift_site(epw = get_cache_epw()),
        periods = periods,
        transform = transform,
        reference = historical,
        observed_reference = observed,
        store = tempfile("method-reference-store-"),
        epw = list(export_dir = tempfile("method-reference-output-"))
    )
    rebuilt <- shift_persist__plan_from_spec(shift_persist__plan_spec(plan))
    explanation <- shift_print__plan_explain(plan)

    expect_identical(
        rebuilt@meta$recipe$recipe_spec,
        "quantile_mapping_morphing_daily"
    )
    expect_identical(
        rebuilt@meta$observed_reference@plan_id,
        "observed-plan"
    )
    expect_true("observed_reference" %in% explanation$step)
    expect_silent(shift_job__validate_background_plan(plan))
})

test_that("EpwMorpher persists and executes the observed reference separately", {
    skip_on_cran()

    years <- c(
        historical = 2001L,
        observed = 2002L,
        future = 2061L
    )
    paths <- stats::setNames(
        vapply(
            names(years),
            function(role) {
                path <- tempfile(
                    sprintf("qm_morphing-%s-", role),
                    fileext = ".nc"
                )
                write_local_cmip6_netcdf_fixture(
                    path,
                    years[[role]],
                    variable_id = "tas"
                )
                path
            },
            character(1L)
        ),
        names(years)
    )
    on.exit(unlink(paths), add = TRUE)

    store <- EsgStore$new(tempfile("qm_morphing-workflow-"))
    on.exit(store$close(), add = TRUE)
    plans <- lapply(names(years), function(role) {
        year <- years[[role]]
        docs <- esgf_test__file_docs(
            basename(paths[[role]]),
            opendap_url = paths[[role]],
            download_url = paths[[role]],
            variable_id = "tas",
            datetime_start = sprintf("%d-01-01T00:00:00Z", year),
            datetime_end = sprintf("%d-12-31T23:59:59Z", year)
        )
        query_id <- store$add_files(esgf_test__file_result(docs))
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
        transform = daily_transform("qm_morphing")
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
    mapping <- morpher__read_table(store, "epw_morph_observed_reference")
    result_path <- store_abs_path(
        workflow$results$output_path,
        root = store$path
    )
    weather <- read_test_parquet(result_path)

    observed_mapping <- mapping[morph_id == workflow$plan$morph_id]
    expect_identical(nrow(observed_mapping), 1L)
    expect_true(nzchar(observed_mapping$observed_summary_id))
    expect_identical(nrow(weather), 8760L)
    expect_true("quantile_mapping_morphing_delta" %in% names(weather))
    expect_equal(
        weather$quantile_mapping_morphing_delta,
        rep(0, 8760L),
        tolerance = 1e-7
    )
})

# vim: fdm=marker :
