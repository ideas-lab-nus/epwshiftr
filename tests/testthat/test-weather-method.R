test_that("registered methods resolve their algorithm definitions", {
    daily <- method__get("quantile_delta_mapping_daily")
    expect_true(S7::S7_inherits(daily, WeatherMethodSpec))
    expect_identical(daily@domain, "daily_bias_adjustment")
    expect_identical(daily@implementation_key, "quantile_delta_mapping_daily")
    expect_identical(daily@output_role, "model_future")
    expect_true("tas" %in% daily@output_variables)
})

test_that("every built-in recipe resolves one registered method", {
    recipes <- epw_morph_recipes()

    for (name in recipes$name) {
        spec <- recipe__get(name)
        method <- method__get(spec@method)

        expect_identical(spec@method, method@name)
        expect_true(length(recipe__target_calendar(spec)) == 1L)
    }
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
                "daily_adjusted_specific_humidity_closure"
            )
        },
        logical(1L)
    )))
})

test_that("method construction rejects invalid definitions", {
    args <- list(
        name = "shared_validation",
        label = "Shared validation",
        domain = "daily_bias_adjustment",
        implementation = "signal_component",
        implementation_key = "linear_scaling_daily",
        frequencies = c(input = "day"),
        input_roles = "model_future",
        variable_sets = "tas",
        output_role = "model_future",
        references = "Method reference",
        version = 1
    )
    invalid <- args
    invalid$version <- 1.5
    expect_error(do.call(method__spec, invalid), "version")
    invalid <- args
    invalid$implementation_key <- "invalid key"
    expect_error(do.call(method__spec, invalid), "implementation_key")
    invalid <- args
    invalid$stochastic_variables <- "pr"
    expect_error(do.call(method__spec, invalid), "stochastic_variables")
})

# vim: fdm=marker :
