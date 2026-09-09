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
    expect_false(any(grepl("_", transforms$reconstruction_label)))
    expect_true(all(is.na(
        transforms[!(scale == "daily" & method == "epwshiftr"), reconstruction]
    )))
    expect_setequal(
        transforms[scale == "daily" & method == "epwshiftr", reconstruction],
        c("power", "btws")
    )
})

test_that("weather transform registry rejects ambiguous or incomplete mappings", {
    records <- transform__records()
    expect_invisible(transform__validate_records(records))

    duplicated <- records
    duplicated[[2L]] <- duplicated[[1L]]
    expect_error(
        transform__validate_records(duplicated),
        "duplicate scale/method records"
    )

    split_method <- records
    split_method[[length(split_method) + 1L]] <- split_method[[4L]]
    split_method[[4L]]$recipe <- split_method[[4L]]$recipe[["power"]]
    split_method[[4L]]$reconstructions <- "power"
    split_method[[4L]]$reconstruction_labels <-
        split_method[[4L]]$reconstruction_labels[["power"]]
    names(split_method[[4L]]$reconstruction_labels) <- "power"
    split_method[[4L]]$default_reconstruction <- "power"
    split_method[[length(split_method)]]$recipe <-
        split_method[[length(split_method)]]$recipe[["btws"]]
    split_method[[length(split_method)]]$reconstructions <- "btws"
    split_method[[length(split_method)]]$reconstruction_labels <-
        split_method[[length(split_method)]]$reconstruction_labels[["btws"]]
    names(split_method[[length(split_method)]]$reconstruction_labels) <- "btws"
    split_method[[length(split_method)]]$default_reconstruction <- "btws"
    expect_error(
        transform__validate_records(split_method),
        "duplicate scale/method records"
    )

    missing <- records[-1L]
    expect_error(
        transform__validate_records(missing),
        "Every built-in canonical recipe"
    )

    wrong_method <- records
    wrong_method[[1L]]$method_definition <- "epwshiftr_monthly"
    expect_error(
        transform__validate_records(wrong_method),
        "Recipe method identity disagrees"
    )

    unlabelled <- records
    names(unlabelled[[1L]]$reconstruction_labels) <- "other"
    expect_error(
        transform__validate_records(unlabelled),
        "must label every reconstruction"
    )
})

test_that("public catalog uses controlled evidence and status labels", {
    transforms <- weather_transforms()

    expect_true(all(transforms$evidence %in% WEATHER_TRANSFORM_EVIDENCE))
    expect_true(all(transforms$status %in% WEATHER_RECIPE_STATUSES))
    expect_true(all(lengths(transforms$references) > 0L))
    daily_adjustments <- transforms[
        method %in% c(
            "linear_scaling", "delta_change", "qm", "qdm", "sdm",
            "cdf_t", "isimip3basd"
        )
    ]
    expect_true(all(daily_adjustments$evidence == "adapted_publication"))
    expect_identical(
        transforms[method == "edcdfm", evidence],
        "experimental"
    )
})

test_that("transform type enforces complete role contracts", {
    transform <- daily_transform("qdm")
    invalid_role <- transform
    requirement <- invalid_role@required_inputs$model_future
    requirement@role <- "model_historical"
    expect_error(
        invalid_role@required_inputs <- utils::modifyList(
            invalid_role@required_inputs,
            list(model_future = requirement)
        ),
        "matching WeatherInputRequirement"
    )

    overlapping <- transform
    expect_error(
        overlapping@optional_inputs <- list(
            model_future = overlapping@required_inputs$model_future
        ),
        "both required and optional"
    )

    missing_frequency <- monthly_transform("epwshiftr")
    expect_error(
        missing_frequency@optional_variable_frequencies <- list(),
        "Optional variable metadata"
    )

    incomplete_frequency <- monthly_transform("epwshiftr")
    expect_error(
        incomplete_frequency@optional_variable_frequencies$model_future$tasmax <-
            NULL,
        "Every optional source variable"
    )

    missing_reference <- transform
    expect_error(
        missing_reference@references <- NA_character_,
        "scientific references"
    )

    unnamed_options <- transform
    expect_error(
        unnamed_options@options <- list(1),
        "uniquely named scientific settings"
    )
})

test_that("scale-specific constructors resolve fixed and selectable reconstruction", {
    original_morphing <- monthly_transform("original_morphing")
    bws_btws <- monthly_transform("bws_btws")
    power <- daily_transform("epwshiftr")
    daily_btws <- daily_transform("epwshiftr", reconstruction = "btws")
    hourly <- hourly_transform("kernel_qdm")

    expect_s7_class(original_morphing, WeatherTransformSpec)
    expect_identical(original_morphing@recipe_version, 2L)
    expect_true(all(c("tas", "tasmax", "tasmin") %in%
        original_morphing@required_inputs$model_future@variable_sets[[1L]]))
    expect_identical(bws_btws@scale, "monthly")
    expect_identical(bws_btws@source_frequencies$model_future, "mon")
    expect_identical(bws_btws@reconstruction, "bws_btws_weather")
    expect_equal(
        bws_btws@required_inputs$model_future@variable_sets[[1L]],
        c("tas", "tasmin", "tasmax", "rsds", "clt")
    )
    expect_identical(power@reconstruction, "power")
    expect_identical(daily_btws@reconstruction, "btws")
    expect_identical(hourly@output_type, "multi_year")
    expect_error(
        monthly_transform("original_morphing", reconstruction = "btws"),
        "fixed hourly reconstruction"
    )
    expect_error(daily_transform("unknown"), "Available methods")
    expect_error(monthly_transform("qdm"), "Unknown monthly")
})

test_that("printed transforms hide internal execution identifiers", {
    output <- capture.output(
        print(monthly_transform("original_morphing")),
        type = "message"
    )

    expect_true(any(grepl("Transformation scale: monthly", output)))
    expect_true(any(grepl("Required source frequency", output)))
    expect_true(any(grepl("Hourly reconstruction: Original morphing field equations", output)))
    expect_false(any(grepl(
        paste(
            "original_morphing_monthly|original_morphing_field_equations|paper_faithful|",
            "legacy_epw_field_closure|backend"
        ),
        output
    )))
})

test_that("public transforms always resolve the canonical physical treatment", {
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
        }
    }

    expect_error(
        daily_transform("ek", policy = "harmonized"),
        "Unknown transformation option"
    )
    expect_error(
        daily_transform("sobie_curry", profile = "enhanced"),
        "Unknown transformation option"
    )
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

test_that("stored recipes recover their public reconstruction label", {
    transform <- monthly_transform("original_morphing")
    restored <- transform__from_recipe_object(transform__recipe(transform))

    expect_identical(restored@reconstruction, transform@reconstruction)
    expect_identical(
        restored@reconstruction_label,
        "Original morphing field equations"
    )
})

test_that("signal transforms validate values and persist complete defaults", {
    qdm <- daily_transform("qdm")
    linear <- daily_transform(
        "linear_scaling",
        climatology_window_days = 15L,
        zero_tolerance = 0.01
    )

    expect_named(
        qdm@options$signal_overrides$tas,
        names(method__get("quantile_delta_mapping_daily")@parameters$tas)
    )
    expect_identical(linear@options$climatology_window_days, 15L)
    expect_equal(
        linear@options$signal_overrides$tas$zero_tolerance,
        0.01
    )
    expect_error(
        daily_transform("qdm", seasonal_window_days = 2L),
        "must be odd"
    )
    expect_error(
        daily_transform("qdm", trend_preservation = "nonsense"),
        "Must be element"
    )
    expect_error(
        daily_transform("isimip3basd", n_quantiles = 0L),
        "not >= 1"
    )
})

test_that("persisted transforms reject reconstruction and catalog drift", {
    transform <- daily_transform("qdm")
    spec <- transform__spec_value(transform)
    round_trip <- jsonlite::fromJSON(
        jsonlite::toJSON(
            spec,
            auto_unbox = TRUE,
            null = "null",
            na = "null"
        ),
        simplifyVector = TRUE
    )
    restored <- transform__from_spec(round_trip)

    expect_equal(restored@options, transform@options)
    expect_true(all(c(
        "method_version", "required_inputs", "optional_inputs",
        "provenance"
    ) %in% names(spec)))

    changed_reconstruction <- spec
    changed_reconstruction$reconstruction <- "btws"
    expect_error(
        transform__from_spec(changed_reconstruction),
        "fixed hourly reconstruction"
    )

    changed_contract <- spec
    changed_contract$provenance$status <- "production"
    expect_error(
        transform__from_spec(changed_contract),
        "recorded scientific contract"
    )
})

test_that("every built-in transform survives both persistence boundaries", {
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

            # JSON may represent a whole-valued double as an integer, but the
            # validated scientific value and canonical selection must agree.
            expect_equal(restored@options, transform@options)
            expect_identical(restored@recipe, transform@recipe)
            expect_identical(
                restored@reconstruction,
                transform@reconstruction
            )

            recipe <- cli_shift__recipe_from_json(
                morpher__json(transform__recipe(transform))
            )
            recipe_transform <- transform__from_recipe_object(recipe)
            expect_identical(recipe_transform@recipe, transform@recipe)
            expect_identical(
                recipe_transform@reconstruction,
                transform@reconstruction
            )
        }
    }
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

test_that("reference objects retain distinct semantic input roles", {
    periods <- epw_morph_periods(reference = 1995:2014)
    historical <- shift_reference_plan("historical-plan", periods)
    observed <- shift_reference_plan(
        "observed-plan",
        periods,
        role = "observed_reference"
    )
    observed_stage <- shift_stage_new(
        ShiftClimate,
        "climate",
        ids = list(plan_id = "observed-stage-plan"),
        meta = list(periods = periods)
    )
    transform <- daily_transform("qdm")

    expect_error(
        transform__validate_execution_inputs(
            transform,
            reference = historical,
            observed_reference = historical
        ),
        "declares role.*model_historical"
    )
    expect_invisible(transform__validate_execution_inputs(
        transform,
        reference = historical,
        observed_reference = observed
    ))
    expect_invisible(transform__validate_execution_inputs(
        transform,
        reference = historical,
        observed_reference = observed_stage
    ))
    expect_identical(
        shift__reference_spec_value(
            observed_stage,
            role = "observed_reference"
        )$role,
        "observed_reference"
    )
})

test_that("required and optional source frequencies remain distinguishable", {
    daily <- daily_transform("epwshiftr")
    hourly <- hourly_transform("kernel_qdm")

    expect_identical(daily@source_frequencies$model_future, "day")
    expect_identical(
        daily@optional_variable_frequencies$model_future,
        list(tasmin = "day", tasmax = "day")
    )
    expect_false(any(c("tasmin", "tasmax") %in%
        names(hourly@source_frequencies$model_future)))
    expect_named(
        hourly@optional_variable_frequencies$model_future,
        c("tasmin", "tasmax")
    )
})
