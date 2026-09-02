# Build a compact EPW-shaped state for policy-level tests without involving
# calendar mapping or a complete morphing backend.
epwphys_test__weather <- function(rows = 2L) {
    data.table::data.table(
        dry_bulb_temperature = rep(c(20, 25), length.out = rows),
        dew_point_temperature = rep(c(10, 15), length.out = rows),
        relative_humidity = rep(c(50, 55), length.out = rows),
        atmospheric_pressure = rep(101325, rows),
        wind_direction = rep(c(90, 180), length.out = rows),
        wind_speed = rep(2, rows),
        global_horizontal_radiation = rep(0, rows),
        direct_normal_radiation = rep(0, rows),
        diffuse_horizontal_radiation = rep(0, rows),
        horizontal_infrared_radiation_intensity_from_sky = rep(300, rows)
    )
}

# Construct the minimum geometry columns consumed by the shortwave policy.
epwphys_test__geometry <- function(projection = c(0, 0.5)) {
    data.table::data.table(
        effective_solar_projection = projection,
        extraterrestrial_direct_normal_radiation = rep(1000, length(projection))
    )
}

test_that("all registered complete recipes resolve a physical policy", {
    recipes <- epw_morph_recipes()
    resolved <- lapply(seq_len(nrow(recipes)), function(index) {
        recipe <- epw_morph_recipe(
            recipes$name[[index]],
            policy = recipes$default_policy[[index]]
        )
        epwphys__recipe_policy(recipe)
    })

    expect_length(resolved, nrow(recipes))
    expect_true(all(vapply(
        resolved,
        S7::S7_inherits,
        logical(1L),
        class = EpwPhysicalPolicy
    )))
    expected <- c(
        belcher_monthly = "legacy_independent_fields",
        eames_monthly_temperature = "preserve_specific_humidity",
        ek_daily_factors = "preserve_humidity_fields",
        epwshiftr_daily_btws = "preserve_specific_humidity",
        epwshiftr_daily_power = "preserve_specific_humidity",
        epwshiftr_monthly = "monthly_harmonized",
        monthly_percentile_temperature = "preserve_humidity_fields",
        sobie_curry_daily = "independent_thermodynamic_fields"
    )
    expect_identical(
        stats::setNames(vapply(
            resolved,
            function(policy) policy@name,
            character(1L)
        ), recipes$name),
        expected[recipes$name]
    )
    expect_identical(
        epwphys__recipe_policy(
            epw_morph_recipe(
                "monthly_percentile_temperature",
                policy = "paper_faithful"
            )
        )@name,
        "preserve_humidity_fields"
    )
    expect_identical(
        epwphys__recipe_policy(
            epw_morph_recipe(
                "monthly_percentile_temperature",
                policy = "harmonized"
            )
        )@name,
        "preserve_specific_humidity"
    )
    expect_identical(
        epwphys__recipe_policy(
            epw_morph_recipe(
                "sobie_curry_daily",
                policy = "harmonized"
            )
        )@name,
        "specific_humidity_delta"
    )
    expect_identical(
        epwphys__policy("absolute_model_fields")@name,
        "absolute_model_fields"
    )
})

test_that("paper-faithful preservation diagnoses without changing humidity", {
    template <- epwphys_test__weather()
    result <- epwphys__apply(
        EpwPhysicalRequest(
            template = template,
            fields = list(dry_bulb_temperature = c(5, 25))
        ),
        epwphys__policy("preserve_humidity_fields")
    )

    expect_identical(
        result@weather$relative_humidity,
        template$relative_humidity
    )
    expect_identical(
        result@weather$dew_point_temperature,
        template$dew_point_temperature
    )
    expect_identical(result@corrections$humidity_inconsistent, 1L)
    expect_identical(
        result@state$inconsistency$humidity,
        c(TRUE, FALSE)
    )
    expect_identical(
        result@state$inconsistency$thermodynamic,
        c(TRUE, FALSE)
    )
})

test_that("paper-faithful diagnostics retain pressure and union row masks", {
    template <- epwphys_test__weather(3L)
    template$relative_humidity[[1L]] <- 120
    template$atmospheric_pressure[c(1L, 2L)] <- c(-1, 0)
    result <- epwphys__apply(
        EpwPhysicalRequest(template = template),
        epwphys__policy("independent_thermodynamic_fields")
    )

    expect_identical(
        result@state$inconsistency$humidity,
        c(TRUE, FALSE, FALSE)
    )
    expect_identical(
        result@state$inconsistency$pressure,
        c(TRUE, TRUE, FALSE)
    )
    expect_identical(
        result@state$inconsistency$thermodynamic,
        c(TRUE, TRUE, FALSE)
    )
    expect_identical(result@corrections$humidity_inconsistent, 1L)
})

test_that("specific-humidity policies retain targets and both clipping states", {
    template <- epwphys_test__weather()
    target <- c(-0.01, 0.05)
    result <- epwphys__apply(
        EpwPhysicalRequest(
            template = template,
            humidity = list(target_specific_humidity = target)
        ),
        epwphys__policy("specific_humidity_delta")
    )
    humidity <- result@state$humidity

    expect_identical(humidity$status, c(
        "zero_clipped",
        "saturation_clipped"
    ))
    expect_equal(humidity$target_specific_humidity, target)
    expect_true(all(result@weather$relative_humidity >= 0))
    expect_true(all(result@weather$relative_humidity <= 100))
    expect_true(all(
        result@weather$dew_point_temperature <=
            result@weather$dry_bulb_temperature
    ))
})

test_that("preserved specific humidity closes only valid baseline rows", {
    template <- epwphys_test__weather()
    template$relative_humidity[[2L]] <- NA_real_
    result <- epwphys__apply(
        EpwPhysicalRequest(
            template = template,
            fields = list(dry_bulb_temperature = c(-20, 30))
        ),
        epwphys__policy("preserve_specific_humidity")
    )

    expect_true(result@state$humidity$status[[1L]] %in%
        c("inherited", "saturation_clipped"))
    expect_identical(
        result@state$humidity$status[[2L]],
        "missing_baseline_state"
    )
    expect_true(is.na(result@weather$relative_humidity[[2L]]))
})

test_that("absolute policy derives vector wind and closes shortwave fields", {
    template <- epwphys_test__weather()
    result <- epwphys__apply(
        EpwPhysicalRequest(
            template = template,
            fields = list(
                dry_bulb_temperature = c(20, 100),
                atmospheric_pressure = c(101325, 101325)
            ),
            humidity = list(relative_humidity = c(50, 120)),
            wind = list(
                eastward = c(0, -3),
                northward = c(-2, 0)
            ),
            shortwave = list(
                global_horizontal = c(100, 800),
                diffuse_horizontal = c(50, 100)
            ),
            geometry = epwphys_test__geometry()
        ),
        epwphys__policy("absolute_model_fields")
    )

    expect_identical(
        result@state$wind$direction_policy,
        "derive_from_uas_vas"
    )
    expect_equal(result@weather$wind_direction, c(0, 90), tolerance = 1e-12)
    expect_equal(
        result@weather$global_horizontal_radiation,
        result@weather$diffuse_horizontal_radiation +
            result@weather$direct_normal_radiation * c(0, 0.5),
        tolerance = 1e-12
    )
    expect_identical(result@corrections$radiation_night_values_zeroed, 1L)
    expect_identical(result@corrections$humidity_saturation_clipped, 1L)
    expect_identical(result@corrections$temperature_clipped, 1L)
})

test_that("grouped physical execution preserves case and row order", {
    weather <- data.table::rbindlist(list(
        data.table::data.table(case = "b", epwphys_test__weather()),
        data.table::data.table(case = "a", epwphys_test__weather())
    ))
    result <- epwphys__apply_groups(
        weather,
        epwphys__policy("legacy_independent_fields"),
        group_columns = "case",
        expected_rows = 2L
    )

    expect_identical(result$weather$case, weather$case)
    expect_equal(result$weather, weather)
    expect_length(result$results, 2L)
})
