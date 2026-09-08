test_that("weather transform registry exposes every canonical method once", {
    transforms <- weather_transforms()
    tuples <- paste(
        transforms$scale,
        transforms$method,
        transforms$reconstruction,
        sep = "/"
    )

    expect_identical(anyDuplicated(tuples), 0L)
    expect_setequal(transforms$scale, WEATHER_TRANSFORM_SCALES)
    expect_false(any(c(
        "recipe", "backend", "profile", "policy", "component"
    ) %in% names(transforms)))

    resolved_recipes <- unlist(lapply(
        transform__records(),
        function(record) unname(record$recipe)
    ))
    expect_setequal(resolved_recipes, epw_morph_recipes()$name)
})

test_that("scale-specific constructors resolve fixed and selectable reconstruction", {
    belcher <- monthly_transform("belcher")
    eames <- monthly_transform("eames")
    power <- daily_transform("epwshiftr")
    btws <- daily_transform("epwshiftr", reconstruction = "btws")
    hourly <- hourly_transform("kernel_qdm")

    expect_s7_class(belcher, WeatherTransformSpec)
    expect_identical(eames@scale, "monthly")
    expect_identical(eames@source_frequencies$model_future, "day")
    expect_identical(power@reconstruction, "power")
    expect_identical(btws@reconstruction, "btws")
    expect_identical(hourly@output_type, "multi_year")
    expect_error(
        monthly_transform("belcher", reconstruction = "btws"),
        "fixed hourly reconstruction"
    )
    expect_error(daily_transform("unknown"), "Available methods")
    expect_error(monthly_transform("qdm"), "Unknown monthly")
})

test_that("transform options use method schemas and survive persistence", {
    daily <- daily_transform("epwshiftr", window_days = 15L)
    restored <- transform__from_spec(transform__spec_value(daily))

    expect_identical(restored@scale, daily@scale)
    expect_identical(restored@method, daily@method)
    expect_identical(restored@recipe, daily@recipe)
    expect_identical(restored@recipe_version, daily@recipe_version)
    expect_identical(restored@options, daily@options)
    expect_error(
        daily_transform("epwshiftr", unknown_setting = TRUE),
        "Unknown transformation option"
    )
    expect_error(
        daily_transform("epwshiftr", window_days = 14L),
        "must be odd"
    )
})

test_that("resolved options update the public input contract", {
    required_snow <- monthly_transform(
        "epwshiftr",
        snow_depth = "required"
    )
    huss_only <- monthly_transform(
        "epwshiftr",
        humidity_source = "huss"
    )

    expect_true(all(vapply(
        required_snow@required_inputs$model_future@variable_sets,
        function(variables) "snd" %in% variables,
        logical(1L)
    )))
    expect_true(all(vapply(
        required_snow@optional_inputs$model_historical@variable_sets,
        function(variables) "snd" %in% variables,
        logical(1L)
    )))
    expect_identical(
        huss_only@required_inputs$model_future@variable_sets,
        list(c("tas", "huss", "ps", "psl", "rlds", "rsds", "sfcWind",
            "clt", "pr"))
    )
})

test_that("transform specifications contain no execution-owned data", {
    transform <- daily_transform("qdm")
    prohibited <- c(
        "reference", "observed_reference", "climate", "epw", "periods",
        "model", "member", "grid", "scenario", "node", "dir", "path"
    )

    expect_false(any(prohibited %in% S7::props(transform)))
    expect_true(transform__requires_input(transform, "model_historical"))
    expect_true(transform__requires_input(transform, "observed_reference"))
    expect_error(
        transform__validate_execution_inputs(transform),
        "requires.*reference"
    )
})
