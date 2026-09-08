# Build one role-labelled climate input with only the metadata needed by recipe
# contract tests.
recipe_test__climate_input <- function(role, frequency, variables) {
    weather__new_input(
        role,
        data.frame(
            variable_id = variables,
            frequency = rep(frequency, length(variables))
        )
    )
}

# Build the common EPW template input without reading or mutating a weather
# fixture.
recipe_test__weather_template <- function() {
    weather__new_input(
        "weather_template",
        "baseline.epw",
        representation = "epw",
        frequencies = "hour",
        calendars = "gregorian"
    )
}

test_that("built-in complete recipes expose inspectable stable metadata", {
    recipes <- epw_morph_recipes()

    expect_named(
        recipes,
        c(
            "name", "version", "label", "method", "backend",
            "implementation", "default_policy", "policies",
            "physical_policies", "calendar_policy", "target_calendar",
            "output_type", "stochastic", "status", "source",
            "required_inputs", "optional_inputs", "components",
            "diagnostics", "provenance"
        )
    )
    expect_setequal(
        recipes$name,
        WEATHER_RECIPE_DEFAULTS
    )
    expect_true(all(lengths(recipes$components) == 7L))
    expect_true(all(
        recipes$output_type[recipes$name != "hourly_kernel_qdm"] ==
            "representative_year"
    ))
    expect_identical(
        recipes$output_type[recipes$name == "hourly_kernel_qdm"],
        "multi_year"
    )
    expect_false(any(recipes$stochastic))

    daily <- epw_morph_recipe_spec("epwshiftr_daily_power")
    expect_true(S7::S7_inherits(daily, WeatherRecipeSpec))
    expect_identical(daily@backend, "daily_temperature")
    expect_identical(daily@method, "daily_temperature_delta")
    expect_identical(daily@implementation, "pipeline")
    expect_identical(daily@default_policy, "harmonized")
    expect_identical(
        daily@physical_policies,
        c(harmonized = "preserve_specific_humidity")
    )
    expect_identical(
        daily@components,
        pipeline__records(
            epw_morph_backend("daily_temperature")$component_pipeline()
        )
    )
    expect_false(any(vapply(
        c(daily@source, daily@components),
        is.function,
        logical(1L)
    )))
})

test_that("recipe registry rejects duplicate and incompatible definitions", {
    registry <- new.env(parent = emptyenv())
    daily <- epw_morph_recipe_spec("epwshiftr_daily_power")

    expect_identical(
        recipe__register(daily, registry = registry),
        daily
    )
    expect_identical(
        recipe__get(
            "epwshiftr_daily_power",
            version = 1L,
            registry = registry
        ),
        daily
    )
    expect_identical(
        recipe__list(registry)[["name"]],
        "epwshiftr_daily_power"
    )
    expect_error(
        recipe__register(daily, registry = registry),
        "already registered"
    )
    expect_error(
        recipe__get(
            "epwshiftr_daily_power",
            version = 2L,
            registry = registry
        ),
        "persisted version"
    )

    incompatible_components <- daily@components
    incompatible_components$signal <- "missing_signal"
    incompatible <- recipe__spec(
        name = "incompatible_daily",
        label = "Incompatible daily test",
        method = "daily_temperature_delta",
        backend = "daily_temperature",
        implementation = "pipeline",
        source = list(
            type = "test",
            citation = "Test-only incompatible recipe"
        ),
        required_inputs = daily@required_inputs,
        calendar_policy = daily@calendar_policy,
        components = incompatible_components,
        policy_profiles = c(harmonized = "default"),
        physical_policies = c(
            harmonized = "preserve_specific_humidity"
        ),
        default_policy = "harmonized"
    )
    expect_error(
        recipe__register(incompatible, registry = registry),
        "do not match"
    )

})

test_that("registered recipe policies resolve backend profiles explicitly", {
    faithful <- epw_morph_recipe("belcher_monthly")
    enhanced <- epw_morph_recipe("epwshiftr_monthly")
    daily <- epw_morph_recipe("epwshiftr_daily_power")
    hourly <- epw_morph_recipe("hourly_kernel_qdm")

    expect_identical(faithful$backend, "belcher")
    expect_identical(faithful$profile, "legacy")
    expect_identical(faithful$policy, "paper_faithful")
    expect_identical(faithful$recipe_spec, "belcher_monthly")
    expect_identical(faithful$recipe_version, 1L)
    expect_true(morpher__recipe_requires_reference(faithful))
    expect_true(morpher__recipe_accepts_reference(faithful))
    expect_identical(
        morpher__recipe_required_frequency(faithful),
        "mon"
    )

    expect_identical(enhanced$backend, "belcher")
    expect_identical(enhanced$profile, "enhanced")
    expect_identical(enhanced$policy, "harmonized")
    expect_false(morpher__recipe_requires_reference(enhanced))
    expect_true(morpher__recipe_accepts_reference(enhanced))
    expect_identical(
        morpher__recipe_required_frequency(enhanced),
        "mon"
    )

    expect_identical(daily$backend, "daily_temperature")
    expect_identical(daily$profile, "default")
    expect_identical(daily$policy, "harmonized")
    expect_identical(
        epw_morph_variables("epwshiftr_daily_power"),
        "tas"
    )
    expect_identical(
        morpher__recipe_required_frequency(daily),
        "day"
    )
    expect_identical(
        morpher__recipe_required_frequency(hourly),
        HOURLY_KQDM_MODEL_FREQUENCIES
    )

    expect_error(
        epw_morph_recipe(
            "belcher_monthly",
            policy = "harmonized"
        ),
        "Must be element"
    )
    expect_error(
        epw_morph_recipe(
            "epwshiftr_monthly",
            profile = "legacy"
        ),
        "requires backend profile"
    )
    expect_error(
        epw_morph_recipe(
            "epwshiftr_daily_power",
            backend = "belcher"
        ),
        "uses backend"
    )
    expect_error(
        epw_morph_recipe(
            "epwshiftr_daily_power",
            version = 2L
        ),
        "persisted version"
    )

    ad_hoc <- epw_morph_recipe("belcher")
    expect_null(ad_hoc$recipe_spec)
    expect_null(ad_hoc$recipe_version)
    expect_null(ad_hoc$policy)
    expect_identical(ad_hoc$profile, "enhanced")
})

test_that("recipe input roles validate before backend execution", {
    monthly_variables <- c(
        "tas", "psl", "rlds", "rsds", "sfcWind", "clt", "pr", "hurs"
    )
    future <- recipe_test__climate_input(
        "model_future",
        "mon",
        monthly_variables
    )
    historical <- recipe_test__climate_input(
        "model_historical",
        "mon",
        monthly_variables
    )
    template <- recipe_test__weather_template()
    faithful <- epw_morph_recipe_spec("belcher_monthly")
    enhanced <- epw_morph_recipe_spec("epwshiftr_monthly")

    without_historical <- weather__new_inputs(
        weather_template = template,
        model_future = future
    )
    expect_match(
        recipe__input_errors(faithful, without_historical),
        "required role `model_historical` is missing"
    )
    expect_identical(
        recipe__input_errors(enhanced, without_historical),
        character()
    )
    complete <- weather__new_inputs(
        weather_template = template,
        model_historical = historical,
        model_future = future
    )
    expect_invisible(recipe__validate_inputs(faithful, complete))

    wrong_frequency <- weather__new_inputs(
        weather_template = template,
        model_historical = historical,
        model_future = recipe_test__climate_input(
            "model_future",
            "day",
            monthly_variables
        )
    )
    expect_match(
        recipe__input_errors(faithful, wrong_frequency),
        "frequencies `day` do not satisfy `mon`"
    )

    context <- structure(
        list(
            recipe = epw_morph_recipe("belcher_monthly"),
            inputs = without_historical
        ),
        class = "morpher__context"
    )
    expect_error(
        morpher__run_context(context),
        "input requirements are not satisfied"
    )
})

test_that("registered recipe identity survives JSON and transform persistence", {
    transform <- daily_transform("epwshiftr", window_days = 21L)
    recipe <- transform__recipe(transform)
    json_roundtrip <- epwshiftr_cli_recipe_from_json(
        morpher__json(recipe)
    )
    transform_roundtrip <- transform__recipe(
        transform__from_spec(transform__spec_value(transform))
    )

    for (rebuilt in list(json_roundtrip, transform_roundtrip)) {
        expect_identical(
            rebuilt$recipe_spec,
            "epwshiftr_daily_power"
        )
        expect_identical(rebuilt$recipe_version, 1L)
        expect_identical(rebuilt$policy, "harmonized")
        expect_identical(rebuilt$backend, "daily_temperature")
        expect_identical(rebuilt$options$window_days, 21L)
        expect_identical(rebuilt$components, recipe$components)
    }

    cli_recipe <- transform__recipe(monthly_transform("epwshiftr"))
    expect_identical(cli_recipe$recipe_spec, "epwshiftr_monthly")
    expect_identical(cli_recipe$profile, "enhanced")

    aliased <- transform__recipe(daily_transform("epwshiftr"))
    aliased_roundtrip <- epwshiftr_cli_recipe_from_json(
        morpher__json(aliased)
    )
    expect_identical(aliased_roundtrip$name, "epwshiftr_daily_power")
    expect_identical(
        aliased_roundtrip$recipe_spec,
        "epwshiftr_daily_power"
    )
    expect_identical(aliased_roundtrip$policy, "harmonized")
})
