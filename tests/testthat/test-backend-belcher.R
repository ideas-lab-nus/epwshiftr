enhanced_test__hourly_year <- function() {
    dates <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day")
    data.table::CJ(date = dates, hour = 1:24)[, `:=`(
        datetime = as.POSIXct(date, tz = "UTC") + hour * 3600,
        year = 2001L,
        month = as.integer(format(date, "%m")),
        day = as.integer(format(date, "%d")),
        minute = 60L,
        dry_bulb_temperature = 20 + 5 * sin(2 * pi * (hour - 8) / 24)
    )][]
}


enhanced_test__catalog <- function(experiment, variables, years,
                                    include_snd = TRUE) {
    variables <- if (isTRUE(include_snd)) variables else
        setdiff(variables, "snd")
    data.table::data.table(
        source_id = "Model-A",
        experiment_id = experiment,
        variant_label = "r1i1p1f1",
        grid_label = ifelse(variables == "snd", "gr", "gn"),
        frequency = "mon",
        table_id = ifelse(variables == "snd", "LImon", "Amon"),
        variable_id = variables,
        datetime_start = sprintf("%d-01-01T00:00:00Z", min(years)),
        datetime_end = sprintf("%d-12-31T23:59:59Z", max(years))
    )
}


# Build the deterministic monthly case whose full legacy EPW result is checked
# against the real Singapore IWEC baseline fixture below.
enhanced_test__legacy_climate <- function() {
    month <- 1:12
    phase <- 2 * pi * (month - 1) / 12
    spec <- list(
        tas = list(units = "K", value = 300 + 2 * sin(phase)),
        hurs = list(units = "%", value = 72 + 6 * cos(phase)),
        psl = list(units = "Pa", value = 101000 + 150 * sin(phase)),
        rlds = list(units = "W/m2", value = 390 + 15 * cos(phase)),
        rsds = list(units = "W/m2", value = 190 + 20 * sin(phase)),
        sfcWind = list(units = "m s-1", value = 2.5 + 0.3 * cos(phase)),
        clt = list(units = "%", value = 65 + 5 * sin(phase)),
        pr = list(units = "kg m-2 s-1", value = 2e-5 + 2e-6 * cos(phase))
    )
    data.table::rbindlist(lapply(names(spec), function(variable_id) {
        data.table::data.table(
            activity_drs = "ScenarioMIP", institution_id = "Test",
            source_id = "Model-A", experiment_id = "ssp585",
            member_id = "r1i1p1f1", variant_label = "r1i1p1f1",
            table_id = "Amon", variable_id = variable_id,
            time = as.POSIXct(sprintf("2060-%02d-15", month), tz = "UTC"),
            period = "future", year = 2060L, lon = 103.98, lat = 1.37,
            units = spec[[variable_id]]$units,
            value = spec[[variable_id]]$value
        )
    }))
}


# Hash all 35 EPW fields after fixed-decimal canonicalization. This preserves a
# full-year golden regression while ignoring platform line endings and harmless
# sub-micro-unit floating-point differences from R and system math libraries.
enhanced_test__legacy_weather_digest <- function(weather, digits = 6L) {
    weather <- data.table::as.data.table(weather)[,
        EPW_FILE_COLUMNS, with = FALSE]
    encoded <- lapply(weather, function(value) {
        missing <- is.na(value)
        if (is.numeric(value) || is.integer(value)) {
            output <- formatC(as.numeric(value), format = "f",
                digits = digits, decimal.mark = ".")
        } else {
            output <- as.character(value)
        }
        output[missing] <- "<NA>"
        output
    })
    rows <- do.call(paste, c(encoded, sep = "\u001f"))
    checksum_bytes(charToRaw(paste(rows, collapse = "\n")), "sha256")
}


# Encode a result column deterministically before hashing complete runner
# tables. Classes and factor levels are recorded separately by the snapshot.
belcher_test__canonical_column <- function(value, significant_digits = 7L) {
    if (is.list(value) && !is.data.frame(value)) {
        return(vapply(value, function(item) {
            jsonlite::toJSON(
                item,
                auto_unbox = TRUE,
                null = "null",
                na = "string"
            )
        }, character(1L)))
    }

    if (inherits(value, "POSIXt")) {
        output <- format(
            value,
            format = "%Y-%m-%dT%H:%M:%OS6Z",
            tz = "UTC",
            usetz = FALSE
        )
    } else if (inherits(value, "Date")) {
        output <- format(value, "%Y-%m-%d")
    } else if (is.factor(value)) {
        output <- as.character(value)
    } else if (is.integer(value)) {
        output <- as.character(value)
    } else if (is.numeric(value)) {
        output <- rep.int(NA_character_, length(value))
        finite <- is.finite(value)
        # Use magnitude-neutral precision so large illuminance values and small
        # humidity ratios receive the same cross-platform relative tolerance.
        output[finite] <- formatC(
            value[finite],
            format = "e",
            digits = significant_digits - 1L,
            decimal.mark = "."
        )
        output[is.infinite(value) & value > 0] <- "<Inf>"
        output[is.infinite(value) & value < 0] <- "<-Inf>"
        output[is.nan(value)] <- "<NaN>"
    } else if (is.logical(value)) {
        output <- ifelse(value, "TRUE", "FALSE")
    } else {
        output <- as.character(value)
    }

    # Preserve missing values as data rather than allowing a platform-specific
    # character representation to enter the digest.
    output[is.na(value) & !is.nan(value)] <- "<NA>"
    output
}


# Capture the complete schema, row order, and values of one runner table in a
# compact, reviewable form suitable for cross-platform test snapshots.
belcher_test__table_behavior <- function(data, significant_digits = 7L) {
    data <- data.table::as.data.table(data)
    schema <- vapply(seq_along(data), function(index) {
        value <- data[[index]]
        details <- character()
        if (is.factor(value)) {
            details <- c(details, sprintf(
                "levels=%s",
                paste(levels(value), collapse = "/")
            ))
        }
        if (inherits(value, "POSIXt")) {
            details <- c(details, sprintf(
                "tz=%s",
                attr(value, "tzone", exact = TRUE) %||% ""
            ))
        }
        suffix <- if (length(details)) {
            sprintf("[%s]", paste(details, collapse = ";"))
        } else {
            ""
        }
        sprintf(
            "%s:%s%s",
            names(data)[[index]],
            paste(class(value), collapse = "/"),
            suffix
        )
    }, character(1L))
    encoded <- lapply(
        data,
        belcher_test__canonical_column,
        significant_digits = significant_digits
    )
    rows <- if (nrow(data) && ncol(data)) {
        do.call(paste, c(encoded, sep = "\u001f"))
    } else {
        character()
    }
    payload <- c(
        sprintf("rows=%d", nrow(data)),
        sprintf("columns=%d", ncol(data)),
        names(data),
        vapply(data, function(value) {
            paste(class(value), collapse = "/")
        }, character(1L)),
        rows
    )

    list(
        dimensions = sprintf("%dx%d", nrow(data), ncol(data)),
        schema = paste(schema, collapse = ","),
        digest = checksum_bytes(
            charToRaw(paste(payload, collapse = "\n")),
            "sha256"
        )
    )
}


# Render compact behavior records as stable text rather than serializing R's
# nested object metadata into the checked-in snapshot.
belcher_test__snapshot_json <- function(value) {
    jsonlite::toJSON(
        value,
        auto_unbox = TRUE,
        null = "null",
        na = "string",
        pretty = TRUE
    )
}


# Snapshot every persisted and intermediate surface returned by a Belcher
# runner while retaining the method identity and selected physical policy.
belcher_test__result_behavior <- function(result) {
    policy <- epwphys__recipe_policy(result$recipe)
    list(
        backend = result$backend,
        profile = result$recipe$profile,
        policy = policy@name,
        data = belcher_test__table_behavior(result$data),
        parts = lapply(result$parts, belcher_test__table_behavior),
        factors = belcher_test__table_behavior(result$factors),
        diagnostics = belcher_test__table_behavior(result$diagnostics)
    )
}


# Build the same single-case context boundary used by EpwMorpher after it has
# separated model, scenario, member, and period cases.
belcher_test__context <- function(epw, climate, backend, profile,
                                  reference_climate = NULL) {
    morpher__context(
        epw,
        climate,
        recipe = suppressWarnings(epw_morph_recipe(
            backend,
            profile = profile
        )),
        reference_climate = reference_climate,
        years = 2060L,
        labels = "future",
        reference_years = if (is.null(reference_climate)) NULL else 1995L,
        reference_labels = if (is.null(reference_climate)) {
            NULL
        } else {
            "reference"
        },
        by = c("source_id", "experiment_id", "variant_label", "period"),
        strict = TRUE
    )
}


# Build matching future/reference cases that exercise every enhanced runner
# branch, including optional extrema, HUSS state humidity, and LImon snow.
enhanced_test__change_climate <- function(reference = FALSE) {
    month <- 1:12
    phase <- 2 * pi * (month - 1) / 12
    offset <- if (isTRUE(reference)) 0 else 1
    spec <- list(
        tas = list("K", 298 + 2 * offset + sin(phase)),
        tasmax = list("K", 303 + 3 * offset + sin(phase)),
        tasmin = list("K", 293 + offset + sin(phase)),
        huss = list("kg kg-1", 0.016 + 0.002 * offset + phase * 0),
        ps = list("Pa", 100700 + 100 * offset + phase * 0),
        hurs = list("%", 65 - 30 * offset + phase * 0),
        psl = list("Pa", 101000 + 100 * offset + 100 * sin(phase)),
        rlds = list("W/m2", 380 + 10 * offset + 10 * cos(phase)),
        rsds = list("W/m2", 180 + 20 * offset + 15 * sin(phase)),
        sfcWind = list("m s-1", 2.5 + 0.5 * offset + 0.2 * cos(phase)),
        clt = list("%", 60 + 5 * offset + 5 * sin(phase)),
        pr = list("kg m-2 s-1", 2e-5 + 2e-6 * offset + phase * 0),
        snd = list("m", 0.1 + 0.02 * offset + phase * 0)
    )
    year <- if (isTRUE(reference)) 1995L else 2060L
    data.table::rbindlist(lapply(names(spec), function(variable_id) {
        data.table::data.table(
            activity_drs = if (isTRUE(reference)) "CMIP" else "ScenarioMIP",
            institution_id = "Test", source_id = "Model-A",
            experiment_id = if (isTRUE(reference)) "historical" else "ssp585",
            member_id = "r1i1p1f1", variant_label = "r1i1p1f1",
            table_id = if (variable_id == "snd") "LImon" else "Amon",
            variable_id = variable_id,
            time = as.POSIXct(sprintf("%d-%02d-15", year, month), tz = "UTC"),
            period = if (isTRUE(reference)) "reference" else "future",
            year = year, lon = 103.98, lat = 1.37,
            units = spec[[variable_id]][[1L]],
            value = spec[[variable_id]][[2L]]
        )
    }))
}


test_that("Belcher runner behavior is fixed across modes and profiles", {
    epw <- epw_file_read(get_cache_epw())
    future <- enhanced_test__change_climate()
    reference <- enhanced_test__change_climate(reference = TRUE)
    contexts <- list(
        absolute_legacy = belcher_test__context(
            epw, future, "belcher_absolute", "legacy"
        ),
        absolute_enhanced = belcher_test__context(
            epw, future, "belcher_absolute", "enhanced"
        ),
        change_legacy = belcher_test__context(
            epw, future, "belcher", "legacy", reference
        ),
        change_enhanced = belcher_test__context(
            epw, future, "belcher", "enhanced", reference
        ),
        baseline_fallback = belcher_test__context(
            epw, future, "belcher", "enhanced"
        )
    )
    results <- lapply(contexts, morpher__run_context)

    # The no-reference Belcher path must remain the absolute-target runner for
    # its own recipe rather than silently switching method identity.
    direct_fallback <- morpher__belcher_absolute_run(
        contexts$baseline_fallback,
        epw_morph_backend("belcher")
    )
    expect_identical(
        belcher_test__result_behavior(results$baseline_fallback),
        belcher_test__result_behavior(direct_fallback)
    )
    expect_identical(results$baseline_fallback$backend, "belcher")

    expect_snapshot(cat(belcher_test__snapshot_json(
        lapply(results, belcher_test__result_behavior)
    )), cran = TRUE)
})


test_that("Belcher production case contexts preserve identity and isolation", {
    epw <- epw_file_read(get_cache_epw())
    future_a <- enhanced_test__change_climate()
    reference_a <- enhanced_test__change_climate(reference = TRUE)
    future_b <- data.table::copy(future_a)
    reference_b <- data.table::copy(reference_a)
    future_b[, source_id := "Model-B"]
    reference_b[, source_id := "Model-B"]
    # Give the second model a distinct temperature change so accidental case
    # reuse is observable in the final full-year weather digest.
    future_b[variable_id == "tas", value := value + 1]

    results <- list(
        model_a = morpher__run_context(belcher_test__context(
            epw, future_a, "belcher", "enhanced", reference_a
        )),
        model_b = morpher__run_context(belcher_test__context(
            epw, future_b, "belcher", "enhanced", reference_b
        ))
    )

    expect_identical(unique(results$model_a$data$source_id), "Model-A")
    expect_identical(unique(results$model_b$data$source_id), "Model-B")
    expect_identical(
        results$model_a$data$datetime,
        results$model_b$data$datetime
    )
    expect_true(any(abs(
        results$model_a$data$dry_bulb_temperature -
            results$model_b$data$dry_bulb_temperature
    ) > 1e-6))
    expect_snapshot(cat(belcher_test__snapshot_json(
        lapply(results, belcher_test__result_behavior)
    )), cran = TRUE)
})


test_that("enhanced profiles and persisted legacy recipes have explicit semantics", {
    enhanced <- epw_morph_recipe("belcher")
    legacy <- epw_morph_recipe("belcher", profile = "legacy")

    expect_identical(enhanced$profile, "enhanced")
    expect_identical(enhanced$methods[["tdb"]], "auto")
    expect_identical(enhanced$options$transition_hours, 72L)
    expect_identical(legacy$profile, "legacy")
    expect_identical(legacy$options$transition_hours, 0L)
    expect_identical(legacy$options$design_conditions, "preserve")
    expect_error(belcher_options(transition_hours = 337L), "0 and 336")

    restored <- shift__recipe_from_ref(list(
        name = "belcher", backend = "belcher", methods = NULL
    ))
    expect_identical(restored$profile, "legacy")
    expect_true(all(c("tasmax", "tasmin", "snd") %in%
        epw_morph_variables(enhanced, include_optional = TRUE)))

    named_json <- morpher__json(epw_morph_recipe(
        "belcher", methods = c(tdb = "shift")
    ))
    named_recipe <- epwshiftr_cli_recipe_from_json(named_json)
    expect_identical(unname(named_recipe$methods[["tdb"]]), "shift")

    old_array_json <- jsonlite::toJSON(list(
        name = "belcher", backend = "belcher",
        methods = unname(legacy$methods)
    ), auto_unbox = TRUE, null = "null")
    old_array_recipe <- epwshiftr_cli_recipe_from_json(old_array_json)
    expect_identical(old_array_recipe$profile, "legacy")
    expect_identical(old_array_recipe$methods, legacy$methods)
})


test_that("legacy profile preserves the historical 35-field EPW golden output", {
    input <- get_cache_epw()
    epw <- epw_file_read(input)
    context <- morpher__context(
        epw, enhanced_test__legacy_climate(),
        recipe = suppressWarnings(epw_morph_recipe(
            "belcher_absolute", profile = "legacy"
        )),
        years = 2060L, labels = "future", strict = TRUE
    )
    result <- morpher__run_context(context)
    expect_equal(nrow(result$data), 8760L)
    expect_equal(ncol(result$data) - 1L, 35L)

    output <- tempfile(fileext = ".epw")
    result$epw$set(result$data)$save(output, overwrite = TRUE)
    expect_identical(
        readLines(output, n = 8L, warn = FALSE),
        readLines(input, n = 8L, warn = FALSE)
    )
    written <- epw_file_read(output)$data()
    expect_equal(nrow(written), 8760L)
    expect_equal(ncol(written) - 1L, 35L)
    expect_identical(
        enhanced_test__legacy_weather_digest(written),
        "d8730e0039d5e44c3c942103abc10f4c8b915d9ef17714613cd6ee4439ed9abd"
    )
})


test_that("enhanced temperature uses mean daily DTR and guarded auto fallback", {
    synthetic <- data.table::data.table(
        year = 2001L,
        month = 1L,
        day = rep(1:2, each = 2L),
        dry_bulb_temperature = c(10, 20, 20, 30)
    )
    dtr <- morpher__belcher_epw_monthly_dtr(
        synthetic, "dry_bulb_temperature"
    )
    expect_equal(dtr$val_daily_max, 25)
    expect_equal(dtr$val_daily_min, 15)
    expect_equal(dtr$val_dtr, 10)

    epw <- enhanced_test__hourly_year()
    baseline <- morpher__belcher_epw_monthly_dtr(
        epw, "dry_bulb_temperature"
    )
    target <- baseline[, .(
        month,
        value = val_mean + 2,
        units = "degC"
    )]
    target_max <- baseline[, .(
        month,
        value = val_mean + 2 + 0.6 * val_dtr,
        units = "degC"
    )]
    target_min <- baseline[, .(
        month,
        value = val_mean + 2 - 0.6 * val_dtr,
        units = "degC"
    )]
    morphed <- morpher__belcher_from_monthly_enhanced(
        "dry_bulb_temperature", epw, target, target_max, target_min,
        type = "auto", transition_hours = 72L
    )
    monthly <- morphed[, .(
        value = mean(dry_bulb_temperature)
    ), by = month]
    expect_equal(monthly$value, target$value, tolerance = 1e-10)
    expect_true(all(morphed$method_applied == "combined"))

    target_max$value[[1L]] <- NA_real_
    fallback <- morpher__belcher_from_monthly_enhanced(
        "dry_bulb_temperature", epw, target, target_max, target_min,
        type = "auto", transition_hours = 0L
    )
    expect_true(all(fallback[month == 1L]$method_applied == "shift"))
    expect_true(all(fallback[month == 1L]$factor_status ==
        "fallback_shift_missing_extremes"))
})


test_that("monthly extrema share aggregation with explicit scientific identities", {
    target <- data.table::data.table(
        activity_drs = "ScenarioMIP",
        institution_id = "Institute",
        source_id = "Model-A",
        experiment_id = c("ssp245", "ssp585"),
        member_id = "r1i1p1f1",
        interval = c("near", "far"),
        month = 1L
    )
    projected <- data.table::data.table(
        activity_drs = "ScenarioMIP",
        institution_id = "Institute",
        source_id = "Model-A",
        experiment_id = rep(c("ssp245", "ssp585"), each = 2L),
        member_id = "r1i1p1f1",
        interval = rep(c("near", "far"), each = 2L),
        month = 1L,
        table_id = rep(c("day", "Amon"), 2L),
        value = c(10, 14, 20, 22)
    )
    attached <- morpher__attach_extreme_value(
        target, projected, "projected_max"
    )
    expect_equal(attached$projected_max, c(12, 21))

    # Historical extrema intentionally aggregate across experiment and interval
    # while retaining the model, member, and month identity.
    reference <- data.table::data.table(
        activity_drs = "ScenarioMIP",
        institution_id = "Institute",
        source_id = "Model-A",
        experiment_id = c("historical", "hist-nat"),
        member_id = "r1i1p1f1",
        interval = c("baseline-a", "baseline-b"),
        month = 1L,
        value = c(4, 8)
    )
    attached <- morpher__attach_reference_extreme(
        target, reference, "reference_max"
    )
    expect_equal(attached$reference_max, c(6, 6))

    missing <- morpher__attach_extreme_value(
        target, NULL, "projected_max"
    )
    expect_true(all(is.na(missing$projected_max)))

    without_month <- data.table::copy(target)
    without_month[, month := NULL]
    expect_error(
        morpher__attach_extreme_value(
            without_month, projected, "projected_max"
        ),
        "Cannot align monthly extrema without a month column.",
        fixed = TRUE
    )
    expect_error(
        morpher__attach_reference_extreme(
            without_month, reference, "reference_max"
        ),
        "Cannot align historical monthly extrema without a month column.",
        fixed = TRUE
    )
})


test_that("cyclic smoothing is continuous and conserves every monthly target", {
    epw <- enhanced_test__hourly_year()
    target <- seq(-3, 8, length.out = 12L)
    factor <- morpher__constrained_month_series(
        epw$month, target, transition_hours = 72L
    )
    means <- data.table::data.table(
        month = epw$month, factor = factor
    )[, .(factor = mean(factor)), by = month]

    expect_equal(means$factor, target, tolerance = 1e-12)
    expect_lt(abs(factor[[1L]] - factor[[length(factor)]]), 0.5)
    expect_error(
        morpher__cyclic_month_basis(epw$month, 337L),
        "0 and 336"
    )

    # Spatial means can differ at machine precision between calendar months;
    # coordinates are metadata and must not split one scientific case.
    identity <- data.table::data.table(
        activity_drs = "ScenarioMIP", institution_id = "Institute",
        source_id = "Model-A", experiment_id = "ssp585",
        member_id = "r1i1p1f1", interval = "future",
        lon = c(103.98, 103.98 + 1e-14), lat = c(1.37, 1.37 + 1e-14)
    )
    expect_false(any(c("lon", "lat") %in%
        morpher__factor_case_columns(identity)))
})


test_that("specific humidity round trips and saturates at physical bounds", {
    temperature <- c(-10, 5, 25, 40)
    humidity <- c(15, 45, 70, 99)
    pressure <- c(80000, 90000, 101325, 105000)
    huss <- epwphys__huss_from_rh_si(temperature, humidity, pressure)
    roundtrip <- epwphys__hurs_from_huss_si(
        huss, temperature + 273.15, pressure
    )
    expect_equal(roundtrip, humidity, tolerance = 1e-8)

    saturation <- epwphys__saturation_huss_si(temperature, pressure)
    expect_equal(
        epwphys__hurs_from_huss_si(
            saturation, temperature + 273.15, pressure
        ),
        rep(100, length(temperature)),
        tolerance = 1e-8
    )
    dew <- epwphys__dew_point_from_rh(temperature, humidity / 100)
    expect_true(all(dew <= temperature))
})


test_that("enhanced runner integrates HUSS, radiation, snow, and final headers", {
    epw <- epw_file_read(get_cache_epw())
    context <- morpher__context(
        epw, enhanced_test__change_climate(),
        recipe = epw_morph_recipe("belcher"),
        reference_climate = enhanced_test__change_climate(reference = TRUE),
        years = 2060L, labels = "future",
        reference_years = 1995L, reference_labels = "reference",
        strict = TRUE
    )
    expect_identical(morpher__belcher_humidity_source(context), "huss")
    result <- morpher__run_context(context)
    weather <- result$data

    expect_equal(nrow(weather), 8760L)
    expect_true(all(weather$relative_humidity >= 0 &
        weather$relative_humidity <= 100))
    expect_true(all(weather$dew_point_temperature <=
        weather$dry_bulb_temperature + 1e-10))
    expect_true(all(weather$diffuse_horizontal_radiation >= 0 &
        weather$diffuse_horizontal_radiation <=
            weather$global_horizontal_radiation + 1e-10))
    expect_true(all(weather$direct_normal_radiation >= 0 &
        weather$direct_normal_radiation <=
            weather$extraterrestrial_direct_normal_radiation + 1e-10))

    geometry <- solar__epw_interval_geometry(
        weather, latitude = 1.37, longitude = 103.98, timezone = 8
    )
    expect_equal(
        weather$global_horizontal_radiation,
        weather$diffuse_horizontal_radiation +
            weather$direct_normal_radiation *
                geometry$effective_solar_projection,
        tolerance = 1e-8
    )
    result$epw$set(weather)
    epw_file__apply_morph_headers(
        result$epw, weather, context$recipe$options
    )
    output <- tempfile(fileext = ".epw")
    result$epw$fill_abnormal()$save(output, overwrite = TRUE)
    reread <- epw_file_read(output)
    expect_equal(nrow(reread$data()), 8760L)
    expect_identical(reread$header("DESIGN CONDITIONS"), "0")
    expect_identical(reread$header("GROUND TEMPERATURES")[[1L]], "3")
    expect_identical(reread$header("TYPICAL/EXTREME PERIODS")[[1L]], "6")
})


test_that("integrated solar geometry and radiation models obey EPW closure", {
    hours <- data.table::data.table(
        year = 2001L, month = 3L, day = 21L, hour = 1:24
    )
    geometry <- solar__epw_interval_geometry(
        hours, latitude = 0, longitude = 0, timezone = 0
    )
    expect_true(all(geometry$extraterrestrial_horizontal_radiation >= 0))
    expect_true(all(geometry$extraterrestrial_direct_normal_radiation >= 0))
    expect_true(any(geometry$extraterrestrial_horizontal_radiation == 0))
    expect_gt(max(geometry$extraterrestrial_horizontal_radiation), 1000)

    ghi <- 0.55 * geometry$extraterrestrial_horizontal_radiation
    dhi <- radiation__rbl_2010_diffuse(
        ghi, geometry, rep("2001-03-21", 24L)
    )
    closed <- epwphys__close_shortwave(ghi, dhi, geometry)
    expect_true(all(closed$dhi >= 0 & closed$dhi <= closed$ghi))
    expect_true(all(closed$dni >= 0 &
        closed$dni <= geometry$extraterrestrial_direct_normal_radiation +
            1e-10))
    expect_equal(
        closed$ghi,
        closed$dhi + closed$dni * geometry$effective_solar_projection,
        tolerance = 1e-10
    )

    light <- illuminance__perez_1990(
        closed$ghi, closed$dhi, closed$dni, geometry,
        dew_point = rep(15, 24L)
    )
    night <- geometry$effective_solar_projection <= .Machine$double.eps
    expect_true(all(unlist(light[night]) == 0))
    expect_true(all(unlist(light[!night]) >= 0, na.rm = TRUE))
})


test_that("snow depth uses metres-to-centimetres ratios without new events", {
    epw <- epw_file_read(get_cache_epw())
    weather <- epw$data()
    weather[, snow_depth := 0]
    # Place the synthetic event away from the month-boundary transition so the
    # monthly ratio itself is tested independently of cyclic smoothing.
    weather[day == 15L & hour == 12L, snow_depth := 10]
    months <- 1:12
    climate <- data.table::data.table(
        time = as.POSIXct(sprintf("2060-%02d-15", months), tz = "UTC"),
        variable_id = "snd", period = "future", year = 2060L,
        lon = 0, lat = 45, units = "m", value = 0.2,
        source_id = "Model-A", experiment_id = "ssp585",
        variant_label = "r1i1p1f1", table_id = "LImon"
    )
    reference <- data.table::copy(climate)
    reference[, `:=`(
        time = as.POSIXct(sprintf("1995-%02d-15", months), tz = "UTC"),
        period = "reference", year = 1995L,
        experiment_id = "historical", value = 0.1
    )]
    reference[as.integer(format(time, "%m", tz = "UTC")) == 1L,
        value := 0]
    context <- morpher__context(
        epw, climate,
        recipe = epw_morph_recipe("belcher"),
        reference_climate = reference,
        years = 2060L, labels = "future",
        reference_years = 1995L, reference_labels = "reference",
        strict = TRUE
    )
    snow <- morpher__belcher_snow_depth(weather, context)$data

    expect_equal(snow[month == 1L, mean(alpha)], 1, tolerance = 1e-10)
    expect_equal(snow[month == 2L, mean(alpha)], 2, tolerance = 1e-10)
    expect_equal(snow[month == 1L & snow_depth > 0]$snow_depth, 10, tolerance = 0.25)
    expect_equal(snow[month == 2L & snow_depth > 0]$snow_depth, 20, tolerance = 0.25)
    zero_time <- weather[snow_depth == 0, datetime]
    expect_true(all(snow[datetime %in% zero_time]$snow_depth == 0))
})


test_that("structured EPW headers round trip and enhanced policies recalculate", {
    path <- get_cache_epw()
    epw <- epw_file_read(path)
    untouched <- tempfile(fileext = ".epw")
    epw$save(untouched, overwrite = TRUE)
    expect_identical(readLines(path, n = 8L), readLines(untouched, n = 8L))

    weather <- epw$data()
    epw_file__apply_morph_headers(epw, weather, belcher_options())
    expect_identical(epw$header("DESIGN CONDITIONS"), "0")
    expect_identical(epw$header("GROUND TEMPERATURES")[[1L]], "3")
    expect_length(epw$header("GROUND TEMPERATURES"), 49L)
    expect_identical(epw$header("TYPICAL/EXTREME PERIODS")[[1L]], "6")
    expect_length(epw$header("TYPICAL/EXTREME PERIODS"), 25L)

    north <- epw_file__typical_extreme_periods(weather, 45)
    south <- epw_file__typical_extreme_periods(weather, -45)
    expect_length(north, 25L)
    expect_length(south, 25L)
    expect_false(identical(north, south))
})


test_that("CMIP6 auto tables resolve and intersect Amon plus LImon partitions", {
    method <- belcher(reference = historical_reference(1995:2014))
    variables <- morpher__input_variables(method@recipe)
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6("Model-A", "ssp585"),
        periods = list(`2060s` = 2055:2065),
        method = method,
        dir = tempfile("enhanced-multitable-output-"),
        store = tempfile("enhanced-multitable-store-"),
        dry_run = TRUE
    )
    future <- enhanced_test__catalog(
        "ssp585", variables, 2055:2065
    )
    reference <- enhanced_test__catalog(
        "historical", variables, 1995:2014
    )
    selection <- shift__resolve_cmip6_selection(plan, future, reference)
    partitions <- shift__selection_partition_rows(selection, "future")

    expect_identical(selection$grid_label, "gn")
    expect_true(any(partitions$variable_id == "snd" &
        partitions$table_id == "LImon" & partitions$grid_label == "gr"))
    expect_true(any(partitions$variable_id == "tas" &
        partitions$table_id == "Amon" & partitions$grid_label == "gn"))
    expect_false("hurs" %in% partitions$variable_id)
    expect_true(all(c("huss", "tas", "ps") %in% partitions$variable_id))

    without_reference_snd <- shift__resolve_cmip6_selection(
        plan, future,
        enhanced_test__catalog("historical", variables, 1995:2014,
            include_snd = FALSE)
    )
    expect_false("snd" %in% shift__selection_partition_rows(
        without_reference_snd, "future"
    )$variable_id)

    required_method <- belcher(
        reference = historical_reference(1995:2014),
        options = belcher_options(snow_depth = "required")
    )
    required_plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6("Model-A", "ssp585"),
        periods = list(`2060s` = 2055:2065),
        method = required_method,
        dir = tempfile("required-snd-output-"),
        store = tempfile("required-snd-store-"),
        dry_run = TRUE
    )
    expect_error(
        shift__resolve_cmip6_selection(
            required_plan, future,
            enhanced_test__catalog("historical", variables, 1995:2014,
                include_snd = FALSE)
        ),
        class = "epwshiftr_shift_resolution_error"
    )
})


test_that("exact Amon and LImon partitions gate File rows and extraction plans", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    file_doc <- function(path, variable_id) {
        data.frame(
            id = sprintf("%s|dataset", basename(path)),
            dataset_id = "dataset", size = 123, checksum = "abc",
            checksum_type = "SHA256", instance_id = "instance",
            master_id = "master", replica = FALSE,
            tracking_id = "hdl:test/file", title = basename(path),
            version = 20260101L, latest = TRUE, retracted = FALSE,
            deprecated = FALSE,
            datetime_start = "2060-01-01T00:00:00Z",
            datetime_end = "2060-12-31T23:59:59Z",
            data_node = "example.org", activity_id = "ScenarioMIP",
            institution_id = "Test", source_id = "Model-A",
            experiment_id = "ssp585", variant_label = "r1i1p1f1",
            frequency = "mon", table_id = "Amon",
            variable_id = variable_id, grid_label = "gn",
            url = I(list(c(
                sprintf("%s|application/netcdf|OPENDAP", path),
                sprintf("%s|application/netcdf|HTTPServer", path)
            ))), check.names = FALSE
        )
    }
    file_result <- function(docs) {
        params <- query_param__as_store(list(
            project = "CMIP6", distrib = TRUE, limit = 10L,
            type = "File", format = QUERY_PARAM__FORMAT_JSON
        ))
        response <- esgf_test__response(docs)
        query_result__new(
            EsgResultFile, index_node = "https://example.org",
            params = params, result = response
        )
    }

    paths <- c(tas = tempfile(fileext = ".nc"),
        snd = tempfile(fileext = ".nc"))
    write_local_cmip6_netcdf_fixture(paths[["tas"]], 2060L, "tas")
    write_local_cmip6_netcdf_fixture(paths[["snd"]], 2060L, "snd")
    on.exit(unlink(paths), add = TRUE)

    # Include the two tempting cross-combinations that union-style table/grid
    # filtering used to admit; only Amon/gn tas and LImon/gr snd are selected.
    combinations <- data.table::data.table(
        variable_id = c("tas", "tas", "snd", "snd"),
        table_id = c("Amon", "Amon", "LImon", "LImon"),
        grid_label = c("gn", "gr", "gr", "gn")
    )
    docs <- data.table::rbindlist(lapply(seq_len(nrow(combinations)), function(i) {
        variable_id <- combinations$variable_id[[i]]
        row <- file_doc(paths[[variable_id]], variable_id)
        suffix <- sprintf("%s-%s-%s", variable_id,
            combinations$table_id[[i]], combinations$grid_label[[i]])
        row$source_id <- "Model-A"
        row$frequency <- "mon"
        row$table_id <- combinations$table_id[[i]]
        row$grid_label <- combinations$grid_label[[i]]
        row$dataset_id <- paste0("dataset-", suffix)
        row$master_id <- paste0("master-", suffix)
        row$instance_id <- paste0("instance-", suffix)
        row$tracking_id <- paste0("hdl:test/", suffix)
        row$id <- paste0("file-", suffix, "|", row$dataset_id)
        row$title <- paste0(suffix, ".nc")
        row
    }), fill = TRUE)

    store_path <- tempfile("enhanced-partition-store-")
    store <- EsgStore$new(store_path)
    query_id <- store$add_files(file_result(as.data.frame(docs)))
    store$close()
    request <- shift_request(
        project = "CMIP6", source = "Model-A", experiment = "ssp585",
        variant = "r1i1p1f1", variables = c("tas", "snd"),
        frequency = "mon"
    )
    files <- shift_stage_new(
        ShiftFiles, "files", store_path = store_path,
        ids = list(query_id = query_id),
        meta = list(request = request, dataset_count = 4L,
            file_count = 4L, variables = c("tas", "snd"), fields = "*",
            result_fields = names(docs))
    )
    partitions <- data.table::data.table(
        variable_id = c("tas", "snd"),
        table_id = c("Amon", "LImon"),
        grid_label = c("gn", "gr"),
        required = c(TRUE, FALSE)
    )
    expect_identical(class(shift__cmip6_partition_json(partitions)),
        "character")
    selection <- data.table::data.table(
        source_id = "Model-A", variant_label = "r1i1p1f1",
        frequency = "mon",
        future_partitions_json = shift__cmip6_partition_json(partitions)
    )

    selected <- shift__files_for_partitions(
        files, selection, "ssp585", role = "future"
    )
    selected_rows <- shift_files(selected)$to_data_table()
    expect_equal(nrow(selected_rows), 2L)
    expect_setequal(
        paste(selected_rows$variable_id, selected_rows$table_id,
            selected_rows$grid_label, sep = "/"),
        c("tas/Amon/gn", "snd/LImon/gr")
    )

    climate <- shift__extract_selected_partitions(
        selected, selection, "ssp585",
        site = shift_site("SIN", 103.98, 1.37),
        periods = epw_morph_periods(future = 2060L),
        role = "future", fallback = "error"
    )
    coverage <- shift_coverage(climate)
    expect_equal(length(shift_ids(climate)$plan_id), 2L)
    expect_true(all(coverage$complete))
    expect_setequal(coverage$variable_id, c("tas", "snd"))
})


test_that("CMIP6 table pins and named overrides persist through task specs", {
    pinned <- shift_cmip6("Model-A", "ssp585", table = "Amon")
    overridden <- shift_cmip6(
        "Model-A", "ssp585", table = c(snd = "LImon")
    )
    expect_identical(
        unname(unique(shift__cmip6_variable_tables(
            c("tas", "snd"), "mon", pinned@table
        ))),
        "Amon"
    )
    expect_identical(
        unname(shift__cmip6_variable_tables(
            c("tas", "snd"), "mon", overridden@table
        )),
        c("Amon", "LImon")
    )
    expect_identical(
        cli_shift__table_spec(list(snd = "LImon")),
        c(snd = "LImon")
    )
})

test_that("Belcher humidity capabilities keep hurs canonical and derive from surface inputs", {
    recipe <- epw_morph_recipe("belcher")
    requirements <- morpher__variable_requirements(recipe)
    guidance <- morpher__missing_variable_guidance("hurs", present_variables = c("tas", "huss"))

    expect_equal(requirements$hurs, list(c("huss", "tas", "ps"), "hurs"))
    expect_equal(
        morpher__variable_requirements(
            epw_morph_recipe("belcher", profile = "legacy")
        )$hurs,
        list("hurs", c("huss", "tas", "ps"))
    )
    expect_true(all(c("hurs", "huss", "tas", "ps") %in%
        morpher__input_variables(recipe)))
    expect_match(guidance$suffix, "huss \\+ tas \\+ ps")
    expect_no_match(guidance$suffix, "psl")

    expected <- 100 * (0.01 * 100000 /
        (0.621945 + (1 - 0.621945) * 0.01)) /
        exp(epwphys__psychro_ln_pws(300 - 273.15))
    expect_equal(
        epwphys__hurs_from_huss_si(0.01, 300, 100000),
        expected,
        tolerance = 1e-12
    )
    expect_error(
        morpher__humidity_input_si(100000, "hPa", "ps"),
        NA
    )
})

test_that("Belcher change-factor and solar radiation helpers follow reference formulas", {
    data_epw <- data.table::data.table(
        datetime = as.POSIXct(c("2001-01-15 08:00:00", "2001-01-15 09:00:00"), tz = "UTC"),
        year = 2001L,
        month = 1L,
        day = 15:16,
        hour = c(8L, 9L),
        minute = 60L,
        dry_bulb_temperature = c(20, 30)
    )
    future <- data.table::data.table(
        activity_drs = "ScenarioMIP",
        institution_id = "inst",
        source_id = "model",
        experiment_id = "ssp585",
        member_id = "r1i1p1f1",
        table_id = "day",
        lon = 0,
        lat = 0,
        units = "K",
        value = 305,
        month = 1L,
        interval = "future"
    )
    reference <- data.table::copy(future)
    reference[, `:=`(
        experiment_id = "historical",
        value = 300,
        interval = "reference"
    )]
    shifted <- morpher__belcher_from_monthly_change(
        "dry_bulb_temperature",
        data_epw,
        future,
        reference,
        type = "shift"
    )
    expect_equal(shifted$dry_bulb_temperature, c(25, 35), tolerance = 1e-8)

    glob <- data.table::data.table(
        activity_drs = "ScenarioMIP",
        institution_id = "inst",
        source_id = "model",
        experiment_id = "ssp585",
        member_id = "r1i1p1f1",
        table_id = "day",
        lon = 0,
        lat = 0,
        interval = "future",
        datetime = as.POSIXct(c("2001-03-21 08:00:00", "2001-03-21 01:00:00"), tz = "UTC"),
        year = 2001L,
        month = 3L,
        day = 21L,
        hour = c(8L, 1L),
        minute = 60L,
        global_horizontal_radiation = c(800, 800),
        delta = 0,
        alpha = 1
    )
    diff <- data.table::copy(glob)
    diff[, `:=`(
        global_horizontal_radiation = NULL,
        diffuse_horizontal_radiation = c(200, 200)
    )]
    dni <- morpher__belcher_direct_normal_radiation(glob, diff, latitude = 0, longitude = 0, timezone = 0)
    dni_value <- dni$direct_normal_radiation
    expect_gt(dni_value[[1L]], 800)
    expect_equal(dni_value[[2L]], 0)

    cloud_epw <- data.table::data.table(
        datetime = as.POSIXct("2001-01-01 01:00:00", tz = "UTC"),
        year = 2001L,
        month = 1L,
        day = 1L,
        hour = 1L,
        minute = 60L,
        total_sky_cover = 0L,
        opaque_sky_cover = 0L
    )
    cloud_target <- data.table::data.table(
        activity_drs = "ScenarioMIP",
        institution_id = "inst",
        source_id = "model",
        experiment_id = "ssp585",
        member_id = "r1i1p1f1",
        table_id = "day",
        lon = 0,
        lat = 0,
        units = "%",
        value = 40,
        month = 1L,
        interval = "future"
    )
    total_cover <- morpher__belcher_total_sky_cover(cloud_epw, NULL, data_mean = cloud_target)
    opaque <- morpher__belcher_opaque_sky_cover(cloud_epw, total_cover)
    expect_equal(total_cover$total_sky_cover, 4L)
    expect_true(is.na(total_cover$alpha))
    expect_equal(opaque$opaque_sky_cover, 2L)

    precip_epw <- data.table::data.table(
        datetime = as.POSIXct(c("2001-01-01 01:00:00", "2001-01-01 02:00:00"), tz = "UTC"),
        year = 2001L,
        month = 1L,
        day = 1L,
        hour = 1:2,
        minute = 60L,
        liquid_precip_depth = c(1, 1),
        liquid_precip_rate = c(1, 1)
    )
    precip_target <- data.table::data.table(
        activity_drs = "ScenarioMIP",
        institution_id = "inst",
        source_id = "model",
        experiment_id = "ssp585",
        member_id = "r1i1p1f1",
        table_id = "day",
        lon = 0,
        lat = 0,
        dist = 0,
        units = "mm",
        value = 2,
        reference_value = 1,
        month = 1L,
        interval = "future"
    )
    doubled_precip <- morpher__belcher_precip_from_monthly(
        precip_epw,
        precip_target,
        strict = TRUE,
        change_factor = TRUE
    )
    expect_equal(sum(doubled_precip$liquid_precip_depth), 4)
    expect_equal(unique(doubled_precip$liquid_precip_rate), 1)

    dry_precip_epw <- data.table::copy(precip_epw)
    dry_precip_epw[, liquid_precip_depth := 0]
    expect_error(
        morpher__belcher_precip_from_monthly(dry_precip_epw, precip_target, strict = TRUE),
        "no wet hours"
    )
    relaxed_dry <- NULL
    expect_warning(
        relaxed_dry <- morpher__belcher_precip_from_monthly(dry_precip_epw, precip_target, strict = FALSE),
        "keeping the month dry"
    )
    expect_equal(sum(relaxed_dry$liquid_precip_depth), 0)
})
