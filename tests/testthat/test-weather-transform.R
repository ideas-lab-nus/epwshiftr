test_that("weather transform registry exposes canonical methods with controlled metadata", {
    transforms <- weather_transforms()
    tuples <- paste(
        transforms$scale,
        transforms$method,
        transforms$reconstruction,
        sep = "/"
    )

    expect_identical(anyDuplicated(tuples), 0L)
    expect_setequal(transforms$scale, WEATHER_TRANSFORM_SCALES)
    expect_false(any(
        c("recipe", "backend", "profile", "policy", "component") %in%
            names(transforms)
    ))

    resolved_recipes <- unlist(lapply(transform__records(), function(r) {
        unname(r$recipe)
    }))
    expect_setequal(resolved_recipes, epw_morph_recipes()$name)
    expect_false(any(grepl("_", transforms$reconstruction_label)))
    expect_true(all(is.na(transforms[
        !(scale == "daily" & method == "epwshiftr"),
        reconstruction
    ])))
    expect_setequal(
        transforms[scale == "daily" & method == "epwshiftr", reconstruction],
        c("power", "btws")
    )

    expect_true(all(transforms$evidence %in% WEATHER_TRANSFORM_EVIDENCE))
    expect_true(all(transforms$status %in% WEATHER_RECIPE_STATUSES))
    expect_true(all(lengths(transforms$references) > 0L))
    daily_adj <- transforms[
        method %in%
            c(
                "linear_scaling",
                "delta_change",
                "qm",
                "qdm",
                "sdm",
                "cdf_t",
                "isimip3basd"
            )
    ]
    expect_true(all(daily_adj$evidence == "adapted_publication"))
    expect_identical(transforms[method == "edcdfm", evidence], "experimental")
})

test_that("scale-specific constructors, role validators, and printing enforce contracts", {
    original_morphing <- monthly_transform("original_morphing")
    bws_btws <- monthly_transform("bws_btws")
    power <- daily_transform("epwshiftr")
    daily_btws <- daily_transform("epwshiftr", reconstruction = "btws")
    hourly <- hourly_transform("kernel_qdm")

    expect_s7_class(original_morphing, WeatherTransformSpec)
    expect_identical(original_morphing@recipe_version, 2L)
    expect_true(all(
        c("tas", "tasmax", "tasmin") %in%
            original_morphing@required_inputs$model_future@variable_sets[[1L]]
    ))
    expect_identical(bws_btws@source_frequencies$model_future, "mon")
    expect_identical(bws_btws@reconstruction, "bws_btws_weather")
    expect_equal(
        bws_btws@required_inputs$model_future@variable_sets[[1L]],
        c("tas", "tasmin", "tasmax", "rsds", "clt")
    )
    expect_identical(power@reconstruction, "power")
    expect_identical(daily_btws@reconstruction, "btws")
    expect_identical(hourly@output_type, "multi_year")

    # Printed transforms hide internal execution identifiers
    printed <- capture.output(print(original_morphing), type = "message")
    expect_true(any(grepl("Transformation scale: monthly", printed)))
    expect_false(any(grepl(
        "original_morphing_monthly|original_morphing_field_equations|paper_faithful|legacy_epw_field_closure|backend",
        printed
    )))

    # S7 role/metadata invariants
    qdm <- daily_transform("qdm")
    bad_req <- qdm@required_inputs$model_future
    bad_req@role <- "model_historical"
    expect_error(
        qdm@required_inputs <- utils::modifyList(
            qdm@required_inputs,
            list(model_future = bad_req)
        ),
        "matching WeatherInputRequirement"
    )
    expect_error(
        {
            x <- qdm
            x@optional_inputs <- list(
                model_future = x@required_inputs$model_future
            )
        },
        "both required and optional"
    )
    expect_error(
        {
            x <- monthly_transform("epwshiftr")
            x@optional_variable_frequencies <- list()
        },
        "Optional variable metadata"
    )
    expect_error(
        {
            x <- monthly_transform("epwshiftr")
            x@optional_variable_frequencies$model_future$tasmax <- NULL
        },
        "Every optional source variable"
    )
    expect_error(
        {
            x <- qdm
            x@references <- NA_character_
        },
        "scientific references"
    )
    expect_error(
        {
            x <- qdm
            x@options <- list(1)
        },
        "uniquely named scientific settings"
    )
})

test_that("transform options validate schemas, reject unknown settings, and round-trip", {
    daily <- daily_transform("epwshiftr", window_days = 15L)
    restored <- transform__from_spec(transform__spec_value(daily))
    expect_identical(restored@options, daily@options)
    expect_identical(restored@recipe, daily@recipe)

    qdm <- daily_transform("qdm")
    linear <- daily_transform(
        "linear_scaling",
        climatology_window_days = 15L,
        zero_tolerance = 0.01
    )
    expect_named(
        qdm@options$signal_overrides$tas,
        names(
            component__get(
                "signal",
                "quantile_delta_mapping_daily"
            )@metadata$signal_profiles$tas$settings
        )
    )
    expect_identical(linear@options$climatology_window_days, 15L)
    expect_equal(linear@options$signal_overrides$tas$zero_tolerance, 0.01)

    # Parameterized invalid constructor and option rejection
    invalid_cases <- list(
        list(
            fn = monthly_transform,
            args = list("original_morphing", reconstruction = "btws"),
            pat = "fixed hourly reconstruction"
        ),
        list(
            fn = daily_transform,
            args = list("unknown"),
            pat = "Available methods"
        ),
        list(
            fn = monthly_transform,
            args = list("qdm"),
            pat = "Unknown monthly"
        ),
        list(
            fn = daily_transform,
            args = list("ek", policy = "harmonized"),
            pat = "Unknown transformation option"
        ),
        list(
            fn = daily_transform,
            args = list("sobie_curry", profile = "enhanced"),
            pat = "Unknown transformation option"
        ),
        list(
            fn = daily_transform,
            args = list("epwshiftr", unknown_setting = TRUE),
            pat = "Unknown transformation option"
        ),
        list(
            fn = daily_transform,
            args = list("epwshiftr", window_days = 14L),
            pat = "must be odd"
        ),
        list(
            fn = daily_transform,
            args = list("qdm", seasonal_window_days = 2L),
            pat = "must be odd"
        ),
        list(
            fn = daily_transform,
            args = list("qdm", trend_preservation = "nonsense"),
            pat = "Must be element"
        ),
        list(
            fn = daily_transform,
            args = list("isimip3basd", n_quantiles = 0L),
            pat = "not >= 1"
        )
    )
    for (case in invalid_cases) {
        expect_error(do.call(case$fn, case$args), case$pat)
    }
})

test_that("built-in transforms resolve canonical physical policy, survive persistence, and reject drift", {
    for (record in transform__records()) {
        for (reconstruction in record$reconstructions) {
            transform <- transform__new(
                record$scale,
                record$method,
                if (length(record$reconstructions) > 1L) {
                    reconstruction
                } else {
                    NULL
                }
            )
            recipe <- transform__recipe(transform)
            spec <- morpher__recipe_spec(recipe)
            expect_identical(recipe$policy, spec@default_policy)
            expect_identical(
                epwphys__recipe_policy(recipe)@name,
                unname(spec@physical_policies[[spec@default_policy]])
            )

            value <- transform__spec_value(transform)
            round_trip <- jsonlite::fromJSON(
                jsonlite::toJSON(
                    value,
                    auto_unbox = TRUE,
                    null = "null",
                    na = "null"
                ),
                simplifyVector = TRUE
            )
            restored <- transform__from_spec(round_trip)
            expect_equal(restored@options, transform@options)
            expect_identical(restored@recipe, transform@recipe)
            expect_identical(restored@reconstruction, transform@reconstruction)

            recipe_rt <- transform__from_recipe_object(cli_shift__recipe_from_json(morpher__json(
                recipe
            )))
            expect_identical(recipe_rt@recipe, transform@recipe)
            expect_identical(recipe_rt@reconstruction, transform@reconstruction)
            expect_identical(
                recipe_rt@reconstruction_label,
                transform@reconstruction_label
            )
        }
    }

    # Persisted transforms reject reconstruction and catalog drift
    spec <- transform__spec_value(daily_transform("qdm"))
    expect_true(all(
        c(
            "method_version",
            "required_inputs",
            "optional_inputs",
            "provenance"
        ) %in%
            names(spec)
    ))

    bad_recon <- spec
    bad_recon$reconstruction <- "btws"
    expect_error(transform__from_spec(bad_recon), "fixed hourly reconstruction")

    bad_contract <- spec
    bad_contract$provenance$status <- "production"
    expect_error(
        transform__from_spec(bad_contract),
        "recorded scientific contract"
    )
})

test_that("resolved options update the public input contract", {
    required_snow <- monthly_transform("epwshiftr", snow_depth = "required")
    huss_only <- monthly_transform("epwshiftr", humidity_source = "huss")

    expect_true(all(vapply(
        required_snow@required_inputs$model_future@variable_sets,
        function(v) "snd" %in% v,
        logical(1L)
    )))
    expect_true(all(vapply(
        required_snow@optional_inputs$model_historical@variable_sets,
        function(v) "snd" %in% v,
        logical(1L)
    )))
    expect_identical(
        huss_only@required_inputs$model_future@variable_sets,
        list(c(
            "tas",
            "huss",
            "ps",
            "psl",
            "rlds",
            "rsds",
            "sfcWind",
            "clt",
            "pr"
        ))
    )

    for (method in c("original_morphing", "epwshiftr")) {
        transform <- monthly_transform(method, precipitation = "off")
        expect_false(any(vapply(
            transform@required_inputs$model_future@variable_sets,
            function(v) "pr" %in% v,
            logical(1L)
        )))
        recipe <- transform__recipe(transform)
        expect_identical(recipe$options$precipitation, "off")
        expect_false(
            "pr" %in% epw_morph_variables(recipe, include_optional = TRUE)
        )
        expect_false(morpher__recipe_rules(recipe)[step == "precip", required])
        expect_true(
            "liquid_precip_depth" %in%
                morpher__weather_field_roles(recipe)$inherited_fields
        )
    }
})

test_that("reference roles and required vs optional source frequencies remain distinct", {
    periods <- epw_morph_periods(reference = 1995:2014)
    historical <- shift_reference_from_plan("historical-plan", periods)
    observed <- shift_reference_from_plan(
        "observed-plan",
        periods,
        role = "observed_reference"
    )
    observed_stage <- shift_stage__new(
        ShiftClimate,
        "climate",
        ids = list(plan_id = "observed-stage-plan"),
        meta = list(periods = periods)
    )
    qdm <- daily_transform("qdm")

    expect_true(transform__requires_input(qdm, "model_historical"))
    expect_true(transform__requires_input(qdm, "observed_reference"))
    expect_error(
        transform__validate_execution_inputs(qdm),
        "requires.*reference"
    )
    expect_error(
        transform__validate_execution_inputs(
            qdm,
            reference = historical,
            observed_reference = historical
        ),
        "declares role.*model_historical"
    )
    expect_invisible(transform__validate_execution_inputs(
        qdm,
        reference = historical,
        observed_reference = observed
    ))
    expect_invisible(transform__validate_execution_inputs(
        qdm,
        reference = historical,
        observed_reference = observed_stage
    ))
    expect_identical(
        shift_persist__reference_spec_value(
            observed_stage,
            role = "observed_reference"
        )$role,
        "observed_reference"
    )

    daily <- daily_transform("epwshiftr")
    hourly <- hourly_transform("kernel_qdm")
    expect_identical(daily@source_frequencies$model_future, "day")
    expect_identical(
        daily@optional_variable_frequencies$model_future,
        list(tasmin = "day", tasmax = "day")
    )
    expect_false(any(
        c("tasmin", "tasmax") %in% names(hourly@source_frequencies$model_future)
    ))
    expect_named(
        hourly@optional_variable_frequencies$model_future,
        c("tasmin", "tasmax")
    )
})

test_that("all built-in recipes, backends, and policy profiles satisfy registration contracts", {
    recipes <- epw_morph_recipes()
    expect_setequal(recipes$name, WEATHER_RECIPE_DEFAULTS)
    expect_true(all(EPW_MORPH_BACKEND_DEFAULTS %in% epw_morph_backends()))

    expected_backend_recipes <- c(
        "original_morphing_monthly",
        "epwshiftr_monthly",
        "bws_btws_monthly",
        "ek_daily_factors",
        "quantile_mapping_morphing_daily",
        "sobie_curry_daily"
    )
    expect_identical(
        recipe__get("original_morphing_monthly")@method_version,
        2L
    )
    expect_identical(recipe__get("epwshiftr_monthly")@method_version, 4L)

    for (name in WEATHER_RECIPE_DEFAULTS) {
        spec <- recipe__get(name)
        backend <- epw_morph_backend(spec@backend)
        expect_true(S7::S7_inherits(spec, WeatherRecipeSpec))
        expect_true(inherits(backend, "EpwMorphBackend"))
        expect_identical(length(recipe__target_calendar(spec)), 1L)
        expect_true(spec@default_policy %in% names(spec@policy_profiles))
        expect_identical(
            names(spec@physical_policies),
            names(spec@policy_profiles)
        )
        if (name %in% expected_backend_recipes) {
            expect_identical(spec@implementation, "backend")
            expect_identical(spec@components, list())
            expect_null(backend$component_pipeline())
        } else {
            expect_identical(spec@implementation, "pipeline")
            expect_identical(names(spec@components), WEATHER_COMPONENT_STAGES)
            expect_identical(
                spec@components,
                pipeline__records(backend$component_pipeline())
            )
        }
        for (policy in names(spec@policy_profiles)) {
            resolved <- epw_morph_recipe(name, policy = policy)
            expect_identical(resolved$policy, policy)
            expect_identical(
                resolved$profile,
                unname(spec@policy_profiles[[policy]])
            )
            expect_identical(
                epwphys__recipe_policy(resolved)@name,
                unname(spec@physical_policies[[policy]])
            )
        }
    }
})

# vim: fdm=marker :
