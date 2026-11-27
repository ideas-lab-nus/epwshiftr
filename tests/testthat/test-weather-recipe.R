# Build one role-labelled climate input with only the metadata needed by recipe
# contract tests.
# recipe_test__climate_input {{{
recipe_test__climate_input <- function(role, frequency, variables) {
    weather__new_input(
        role,
        data.frame(
            variable_id = variables,
            frequency = rep(frequency, length(variables))
        )
    )
}
# }}}

test_that("complete recipes expose one comparable EPW field-role contract", {
    original <- morpher__weather_field_roles(
        epw_morph_recipe("original_morphing_monthly")
    )
    bounded <- morpher__weather_field_roles(
        epw_morph_recipe("bws_btws_monthly")
    )
    isimip <- morpher__weather_field_roles(
        epw_morph_recipe("isimip3basd_daily_temperature")
    )

    expect_named(
        original,
        c(
            "transformed_fields",
            "derived_fields",
            "physically_closed_fields",
            "inherited_fields"
        )
    )
    expect_true("liquid_precip_depth" %in% original$transformed_fields)
    expect_true("dry_bulb_temperature" %in% bounded$transformed_fields)
    expect_true("total_sky_cover" %in% bounded$transformed_fields)
    expect_true("relative_humidity" %in% bounded$physically_closed_fields)
    expect_true("dry_bulb_temperature" %in% isimip$transformed_fields)
    expect_true("wind_speed" %in% isimip$inherited_fields)
    expect_length(
        Reduce(
            intersect,
            list(
                isimip$transformed_fields,
                isimip$derived_fields,
                isimip$inherited_fields
            )
        ),
        0L
    )
})

test_that("registered recipe policies, versions, and backend contracts enforce compatibility", {
    faithful <- epw_morph_recipe("original_morphing_monthly")
    enhanced <- epw_morph_recipe("epwshiftr_monthly")
    daily <- epw_morph_recipe("epwshiftr_daily_power")
    hourly <- epw_morph_recipe("hourly_kernel_qdm")

    expect_identical(faithful$backend, "original_morphing")
    expect_identical(faithful$profile, "legacy")
    expect_identical(faithful$policy, "paper_faithful")
    expect_identical(faithful$recipe_version, 2L)
    expect_identical(faithful$methods[["tdb"]], "combined")
    expect_true(all(
        c("tas", "tasmax", "tasmin") %in% epw_morph_variables(faithful)
    ))
    expect_true(morpher__recipe_requires_reference(faithful))
    expect_identical(morpher__recipe_required_frequency(faithful), "mon")
    expect_identical(faithful$components, list())

    expect_identical(enhanced$profile, "enhanced")
    expect_identical(enhanced$policy, "harmonized")
    expect_identical(enhanced$recipe_version, 4L)
    expect_false(morpher__recipe_requires_reference(enhanced))
    expect_true(morpher__recipe_accepts_reference(enhanced))
    expect_identical(morpher__recipe_required_frequency(enhanced), "mon")

    expect_identical(daily$backend, "daily_temperature")
    expect_identical(epw_morph_variables("epwshiftr_daily_power"), "tas")
    expect_identical(morpher__recipe_required_frequency(daily), "day")
    expect_identical(
        morpher__recipe_required_frequency(hourly),
        HOURLY_KQDM_MODEL_FREQUENCIES
    )

    expect_error(
        epw_morph_recipe("original_morphing_monthly", policy = "harmonized"),
        "Must be element"
    )
    expect_error(
        epw_morph_recipe("epwshiftr_monthly", profile = "legacy"),
        "requires backend profile"
    )
    expect_error(
        epw_morph_recipe(
            "epwshiftr_daily_power",
            backend = "original_morphing"
        ),
        "uses backend"
    )
    expect_error(
        epw_morph_recipe("epwshiftr_daily_power", version = 2L),
        "persisted version"
    )

    # Incompatible pipeline components are rejected at registration time
    registry <- new.env(parent = emptyenv())
    daily_spec <- recipe__get("epwshiftr_daily_power")
    recipe__register(daily_spec, registry = registry)
    expect_error(
        recipe__register(daily_spec, registry = registry),
        "already registered"
    )
    bad_components <- daily_spec@components
    bad_components$signal <- "missing_signal"
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
        required_inputs = daily_spec@required_inputs,
        calendar_policy = daily_spec@calendar_policy,
        components = bad_components,
        policy_profiles = c(harmonized = "default"),
        physical_policies = c(harmonized = "preserve_specific_humidity"),
        default_policy = "harmonized"
    )
    expect_error(
        recipe__register(incompatible, registry = registry),
        "do not match"
    )

    ad_hoc <- epw_morph_recipe("original_morphing")
    expect_null(ad_hoc$recipe_spec)
    expect_null(ad_hoc$recipe_version)
    expect_null(ad_hoc$policy)
    expect_identical(ad_hoc$profile, "enhanced")
})

test_that("recipe input roles validate before backend execution", {
    monthly_variables <- c(
        "tas",
        "tasmax",
        "tasmin",
        "psl",
        "rlds",
        "rsds",
        "sfcWind",
        "clt",
        "pr",
        "hurs"
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
    template <- weather__new_input(
        "weather_template",
        "baseline.epw",
        representation = "epw",
        frequencies = "hour",
        calendars = "gregorian"
    )
    faithful <- recipe__get("original_morphing_monthly")
    enhanced <- recipe__get("epwshiftr_monthly")

    without_historical <- weather__new_inputs(
        weather_template = template,
        model_future = future
    )
    expect_error(
        recipe__validate_inputs(faithful, without_historical),
        "required role `model_historical` is missing"
    )
    expect_invisible(recipe__validate_inputs(enhanced, without_historical))
    expect_invisible(recipe__validate_inputs(
        faithful,
        weather__new_inputs(
            weather_template = template,
            model_historical = historical,
            model_future = future
        )
    ))

    wrong_frequency <- weather__new_inputs(
        weather_template = template,
        model_historical = historical,
        model_future = recipe_test__climate_input(
            "model_future",
            "day",
            monthly_variables
        )
    )
    expect_error(
        recipe__validate_inputs(faithful, wrong_frequency),
        "frequencies `day` do not satisfy `mon`"
    )

    context <- structure(
        list(
            recipe = epw_morph_recipe("original_morphing_monthly"),
            inputs = without_historical
        ),
        class = "morpher__context"
    )
    expect_error(
        morpher__run_context(context),
        "input requirements are not satisfied"
    )
})

test_that("epw_morph_recipe() accepts morph.R statistical downscaling method overrides", {
    recipe <- epw_morph_recipe(methods = c(tdb = "shift", rh = "shift"))

    expect_s3_class(recipe, "epw_morph_recipe")
    expect_equal(recipe$backend, "original_morphing")
    expect_equal(recipe$methods[c("tdb", "rh")], c(tdb = "shift", rh = "shift"))
    expect_equal(
        recipe$rules[epw_field == "dry_bulb_temperature", method],
        "shift"
    )
    expect_equal(
        recipe$rules[epw_field == "relative_humidity", method],
        "shift"
    )
    expect_equal(
        epw_morph_variables(recipe),
        epw_morph_variables("recommended")
    )
    expect_error(epw_morph_recipe(methods = c(foo = "shift")), "Unknown")
    expect_error(epw_morph_recipe(methods = c(tdb = "scale")), "Unsupported")
})

# vim: fdm=marker :
