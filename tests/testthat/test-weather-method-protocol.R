test_that("method catalog excludes protocol and study conditions", {
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

test_that("comparison protocols own all shared benchmark conditions", {
    protocols <- epw_morph_protocols()

    expect_setequal(protocols$name, WEATHER_PROTOCOL_DEFAULTS)
    daily <- epw_morph_protocol_spec(
        "daily_bias_adjustment_comparison"
    )
    expect_true(S7::S7_inherits(daily, WeatherProtocolSpec))
    expect_identical(
        daily@data_source$identity_fields,
        c("source_id", "variant_label", "grid_label")
    )
    expect_identical(
        daily@periods$shared_roles,
        WEATHER_PROTOCOL_PERIOD_ROLES
    )
    expect_identical(daily@weather_template$comparison, "identical")
    expect_identical(daily@input_calendar_semantics, "native_cf_calendar")
    expect_identical(daily@target_calendar, "epw_365_day")
    expect_identical(
        daily@hourly_reconstruction,
        "constrained_daily_temperature"
    )
    expect_identical(
        daily@physical_policy,
        "preserve_specific_humidity"
    )
    expect_identical(daily@output_type, "representative_year")
    expect_identical(daily@random_seed, 1L)
    expect_identical(daily@replicates, 1L)
    expect_true(length(daily@diagnostics) > 0L)
    expect_true(length(daily@metrics) > 0L)

    temperature <- epw_morph_protocol_spec(
        "daily_temperature_comparison"
    )
    expect_identical(
        temperature@periods$shared_roles,
        c("model_historical", "model_future")
    )
    expect_identical(
        temperature@periods$optional_shared_roles,
        "observed_reference"
    )
})

test_that("comparison protocols enforce one shared input boundary", {
    protocol <- epw_morph_protocol_spec(
        "daily_bias_adjustment_comparison"
    )
    first <- daily_adjustment_test__context(
        epw_morph_recipe("linear_scaling_daily_temperature")
    )
    second <- daily_adjustment_test__context(
        epw_morph_recipe("quantile_delta_mapping_daily_temperature")
    )

    expect_length(protocol__validate_shared_inputs(
        protocol,
        list(linear_scaling = first$inputs, qdm = second$inputs)
    ), 2L)

    # Rebuild one role-addressable input set after changing only the future
    # model identity so the mismatch is isolated from every other condition.
    replace_role <- function(inputs, role, source) {
        values <- stats::setNames(lapply(
            WEATHER_INPUT_ROLES,
            function(input_role) weather__get_input(inputs, input_role)
        ), WEATHER_INPUT_ROLES)
        values[[role]] <- weather__new_input(role, source)
        do.call(weather__new_inputs, values)
    }
    different_source <- data.table::copy(
        second$inputs@model_future@source
    )
    different_source[, source_id := "Different-GCM"]
    source_inputs <- replace_role(
        second$inputs,
        "model_future",
        different_source
    )
    expect_error(
        protocol__validate_shared_inputs(
            protocol,
            list(linear_scaling = first$inputs, qdm = source_inputs)
        ),
        "same source, member, and grid"
    )

    different_values <- data.table::copy(
        second$inputs@model_future@source
    )
    different_values[1L, value := value + 0.5]
    value_inputs <- replace_role(
        second$inputs,
        "model_future",
        different_values
    )
    expect_error(
        protocol__validate_shared_inputs(
            protocol,
            list(linear_scaling = first$inputs, qdm = value_inputs)
        ),
        "common input variable"
    )

    different_period <- data.table::copy(
        second$inputs@model_future@source
    )
    different_period <- different_period[cf_year == min(cf_year)]
    period_inputs <- replace_role(
        second$inputs,
        "model_future",
        different_period
    )
    expect_error(
        protocol__validate_shared_inputs(
            protocol,
            list(linear_scaling = first$inputs, qdm = period_inputs)
        ),
        "protocol input boundary"
    )
})

test_that("publication conditions remain optional study presets", {
    presets <- epw_morph_study_presets()

    expect_setequal(presets$name, STUDY_PRESET_DEFAULTS)
    wang <- epw_morph_study_preset("wang_2023")
    expect_true(S7::S7_inherits(wang, StudyPreset))
    expect_identical(
        wang@method,
        "kernel_quantile_delta_mapping_hourly"
    )
    expect_identical(wang@status, "documented")
    expect_identical(nrow(wang@data_source$manifest), 10L)
    expect_match(
        wang@data_source$manifest[
            source_id == "IITM-ESM",
            special_treatment
        ],
        "psl"
    )
    expect_false("wang_2023" %in% epw_morph_recipes()$name)
})

test_that("compatibility matrix keeps the four method strata explicit", {
    matrix <- epw_morph_compatibility()

    expect_identical(
        nrow(matrix),
        length(WEATHER_METHOD_DEFAULTS) *
            length(WEATHER_PROTOCOL_DEFAULTS)
    )
    compatible <- matrix[compatible == TRUE]
    expect_identical(nrow(compatible), length(WEATHER_METHOD_DEFAULTS))
    expect_true(all(
        compatible$method_domain == compatible$protocol_domain
    ))
    expect_true(all(lengths(compatible$common_variables) > 0L))
    expect_false(matrix[
        method == "belcher_monthly" &
            protocol == "daily_bias_adjustment_comparison",
        compatible
    ])
})

test_that("recipes identify methods and only claim conforming protocols", {
    recipes <- epw_morph_recipes()

    expect_true(all(recipes$method %in% epw_morph_methods()$name))
    linked <- recipes[!is.na(protocol)]
    expect_true(nrow(linked) > 0L)
    expect_true(all(linked$protocol %in% epw_morph_protocols()$name))
    for (index in seq_len(nrow(linked))) {
        spec <- epw_morph_recipe_spec(linked$name[[index]])
        protocol <- epw_morph_protocol_spec(linked$protocol[[index]])
        expect_true(all(c(
            "weather_template",
            protocol@periods$shared_roles
        ) %in% names(spec@required_inputs)))
        expect_identical(spec@output_type, protocol@output_type)
        expect_true(all(
            spec@physical_policies == protocol@physical_policy
        ))
        if (length(protocol@hourly_reconstruction)) {
            expect_identical(
                recipe__hourly_reconstruction(spec),
                protocol@hourly_reconstruction
            )
        }
    }

    expect_true(is.na(recipes[
        name == "eames_monthly_temperature",
        protocol
    ]))
    expect_true(is.na(recipes[
        name == "ek_daily_factors",
        protocol
    ]))
    expect_true(is.na(recipes[
        name == "epwshiftr_monthly",
        protocol
    ]))
    expect_true(is.na(recipes[
        name == "epwshiftr_daily_btws",
        protocol
    ]))
    expect_setequal(
        recipes[
            protocol == "daily_temperature_comparison",
            name
        ],
        c(
            "epwshiftr_daily_power",
            "eames_monthly_temperature_comparison",
            "ek_daily_temperature_comparison",
            "arima_temperature_comparison",
            "sobie_curry_temperature_comparison"
        )
    )
})

test_that("temperature comparisons standardize the downstream EPW boundary", {
    names <- c(
        "epwshiftr_daily_power",
        "eames_monthly_temperature_comparison",
        "ek_daily_temperature_comparison",
        "arima_temperature_comparison",
        "sobie_curry_temperature_comparison"
    )
    recipes <- lapply(names, epw_morph_recipe_spec)

    expect_length(unique(vapply(
        recipes,
        function(recipe) recipe@components$hourly,
        character(1L)
    )), 1L)
    expect_length(unique(vapply(
        recipes,
        function(recipe) recipe@components$physics,
        character(1L)
    )), 1L)
    expect_length(unique(vapply(
        recipes,
        function(recipe) recipe@components$output,
        character(1L)
    )), 1L)
    expect_gt(length(unique(vapply(
        recipes,
        function(recipe) recipe@calendar_policy,
        character(1L)
    ))), 1L)

    publication_reconstruction <- c(
        eames_monthly_temperature = "btws_temperature_projection",
        ek_daily_factors = "daily_mean_dtr_shift_stretch",
        monthly_percentile_temperature =
            "daily_percentile_temperature_shift",
        sobie_curry_daily = "daily_thermodynamic_transform"
    )
    actual <- vapply(names(publication_reconstruction), function(name) {
        epw_morph_recipe_spec(name)@components$hourly
    }, character(1L))
    expect_identical(actual, publication_reconstruction)
})

test_that("eight daily methods share one complete temperature pipeline", {
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

test_that("daily adjusted temperature closes one representative EPW", {
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
    expect_identical(
        result$parts$adjusted_series_metadata$protocol,
        "daily_bias_adjustment_comparison"
    )
    expect_identical(
        result$parts$adjusted_series_metadata$random_seed,
        1L
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

test_that("all daily adjustment methods use the same EPW boundary", {
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
