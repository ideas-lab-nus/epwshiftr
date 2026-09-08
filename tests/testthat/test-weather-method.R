test_that("method catalog contains algorithm-owned metadata only", {
    methods <- epw_morph_methods()

    expect_setequal(methods$name, WEATHER_METHOD_DEFAULTS)
    expect_setequal(methods$domain, WEATHER_METHOD_DOMAINS)
    expect_false(any(c(
        "calendar_policy", "physical_policy", "output_type",
        "data_source", "periods", "study_preset"
    ) %in% names(methods)))

    daily <- epw_morph_method_spec("quantile_delta_mapping_daily")
    expect_true(S7::S7_inherits(daily, WeatherMethodSpec))
    expect_identical(daily@domain, "daily_bias_adjustment")
    expect_identical(daily@implementation_key, "quantile_delta_mapping_daily")
    expect_identical(daily@output_role, "model_future")
    expect_true("tas" %in% daily@output_variables)
})

test_that("every built-in recipe resolves one registered method", {
    recipes <- epw_morph_recipes()

    expect_true(all(recipes$method %in% epw_morph_methods()$name))
    expect_false(any(grepl("_comparison$", recipes$name)))

    for (name in recipes$name) {
        spec <- epw_morph_recipe_spec(name)
        method <- epw_morph_method_spec(spec@method)

        expect_true(S7::S7_inherits(spec, WeatherRecipeSpec))
        expect_true(S7::S7_inherits(method, WeatherMethodSpec))
        expect_identical(spec@method, method@name)
        expect_true(length(recipe__target_calendar(spec)) == 1L)
    }
})

test_that("daily adjustment methods share one reusable EPW adapter", {
    recipe_names <- paste0(
        unname(DAILY_ADJUSTMENT_METHOD_COMPONENTS),
        "_temperature"
    )
    recipes <- lapply(recipe_names, epw_morph_recipe_spec)
    components <- lapply(recipes, function(recipe) recipe@components)
    signal_names <- vapply(
        components,
        `[[`,
        character(1L),
        "signal"
    )

    expect_setequal(
        signal_names,
        unname(DAILY_ADJUSTMENT_METHOD_COMPONENTS)
    )
    for (stage in setdiff(WEATHER_COMPONENT_STAGES, "signal")) {
        expect_length(unique(vapply(
            components,
            `[[`,
            character(1L),
            stage
        )), 1L)
    }
    expect_true(all(vapply(
        recipe_names,
        function(name) {
            recipe <- epw_morph_recipe(name)
            identical(
                epwphys__recipe_policy(recipe)@name,
                "preserve_specific_humidity"
            )
        },
        logical(1L)
    )))
})

test_that("daily adjusted temperature produces a standard EPW result", {
    recipe <- epw_morph_recipe("linear_scaling_daily_temperature")
    context <- daily_adjustment_test__context(recipe)
    result <- morpher__run_context(context)

    expect_s3_class(result, "epw_morph_result")
    expect_identical(nrow(result$data), 8760L)
    expect_identical(nrow(result$factors), 365L)
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
        pipeline[stage == "physics", component],
        "daily_adjusted_specific_humidity_closure"
    )
})

test_that("all daily adjustment methods produce the same output contract", {
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
        unique(vapply(results, function(result) nrow(result$data), integer(1L))),
        8760L
    )
    expect_identical(
        unique(vapply(results, function(result) nrow(result$factors), integer(1L))),
        365L
    )
    expect_true(all(vapply(results, function(result) {
        identical(
            result$parts$component_pipeline[
                stage == "physics",
                component
            ],
            "daily_adjusted_specific_humidity_closure"
        )
    }, logical(1L))))
})
