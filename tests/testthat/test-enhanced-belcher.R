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


# Build the deterministic monthly case whose full legacy EPW hash was captured
# from the historical implementation before the enhanced profile was introduced.
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


test_that("legacy profile preserves the historical 35-field EPW golden output", {
    epw <- epw_file_read(get_cache_epw())
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
    expect_equal(
        store_hash_file(output, "sha256"),
        "046a9445f632f0c3b050393f04fd1703c5444b27cd448c4d852a16fdff0d145c"
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
    huss <- morpher__huss_from_rh_si(temperature, humidity, pressure)
    roundtrip <- morpher__hurs_from_huss_si(
        huss, temperature + 273.15, pressure
    )
    expect_equal(roundtrip, humidity, tolerance = 1e-8)

    saturation <- morpher__saturation_huss_si(temperature, pressure)
    expect_equal(
        morpher__hurs_from_huss_si(
            saturation, temperature + 273.15, pressure
        ),
        rep(100, length(temperature)),
        tolerance = 1e-8
    )
    dew <- morpher__dew_point_from_rh(temperature, humidity / 100)
    expect_true(all(dew <= temperature))
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
    closed <- radiation__close_components(ghi, dhi, geometry)
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
