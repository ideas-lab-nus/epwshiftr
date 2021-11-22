test_that("monthly_mean()", {
    skip_on_cran()

    cache <- "~/epwshiftr-cache"
    file <- "SGP_Singapore.486980_IWEC.epw"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        epw <- eplusr::read_epw(path)

        epw$add_unit()
        d <- epw$data()
        expect_is(res1 <- monthly_mean(d, "dry_bulb_temperature"), "data.table")
        expect_equal(nrow(res1), 12L)
        expect_equal(names(res1), c("month", "val_mean", "val_max", "val_min"))
        expect_equal(res1$month, 1:12)
        expect_is(res1$val_mean, "units")
        expect_is(res1$val_max, "units")
        expect_is(res1$val_min, "units")

        epw$drop_unit()
        d <- epw$data()
        expect_is(res2 <- monthly_mean(d, "dry_bulb_temperature"), "data.table")
        expect_equal(nrow(res2), 12L)
        expect_equal(names(res2), c("month", "val_mean", "val_max", "val_min"))
        expect_equal(res2$month, 1:12)
        expect_is(res2$val_mean, "numeric")
        expect_is(res2$val_max, "numeric")
        expect_is(res2$val_min, "numeric")
    }
})

test_that("preprocess_morphing()", {
    skip_on_cran()

    cache <- "~/epwshiftr-cache"
    path <- file.path(cache, "EC-Earth3.ssp585.tas.fst")
    if (file.exists(path)) {
        d <- fst::read_fst(path, as.data.table = TRUE)

        expect_is(res <- preprocess_morphing(d), "data.table")
        expect_equal(names(res),
            c("activity_drs", "institution_id", "source_id", "experiment_id",
              "member_id", "table_id", "lat", "lon", "units", "value", "month",
              "interval"
            )
        )
        expect_equal(nrow(res), 12L)
        expect_is(res$interval, "factor")

        # can stop if specified years were not found
        expect_error(preprocess_morphing(d, years = 2059:2061), "does not contain any data")

        expect_is(res <- preprocess_morphing(d, years = 2060, labels = as.factor("2060s")), "data.table")
        expect_equal(names(res),
            c("activity_drs", "institution_id", "source_id", "experiment_id",
              "member_id", "table_id", "lat", "lon", "units", "value", "month",
              "interval"
            )
        )
        expect_equal(nrow(res), 12L)
        expect_is(res$interval, "factor")
    }
})

test_that("morphing_from_mean()", {
    skip_on_cran()

    cache <- "~/epwshiftr-cache"
    file <- "EC-Earth3.ssp585.tas.fst"
    path <- file.path(cache, file)

    epw <- file.path(cache, "SGP_Singapore.486980_IWEC.epw")
    if (file.exists(path) && file.exists(epw)) {
        data_mean <- fst::read_fst(path, as.data.table = TRUE)
        data_epw <- read_epw(epw)$add_unit()$data()

        expect_equal(
            morphing_from_mean(
                "dry_bulb_temperature",
                data_epw, data.table(), NULL, NULL,
                years = 2060L, labels = NULL,
                type = "stretch"
            ),
            data.table()
        )

        # can use morphing using shift factor
        expect_is(class = "data.table",
            res <- morphing_from_mean(
                "dry_bulb_temperature",
                data_epw, data_mean, NULL, NULL,
                years = 2060L, labels = "2060s",
                type = "shift"
            )
        )
        expect_equal(names(res),
            c("activity_drs", "experiment_id", "institution_id", "source_id",
              "member_id", "table_id", "lon", "lat", "interval", "datetime",
              "year", "month", "day", "hour", "minute", "dry_bulb_temperature",
              "delta", "alpha"
            )
        )
        expect_is(res$dry_bulb_temperature, "units")
        expect_is(res$delta, "units")
        expect_is(res$alpha, "units")
        expect_equal(res$dry_bulb_temperature - data_epw$dry_bulb_temperature, res$delta)

        # can use morphing using stretch factor
        expect_is(class = "data.table",
            res <- morphing_from_mean(
                "dry_bulb_temperature",
                data_epw, data_mean, NULL, NULL,
                years = 2060L, labels = "2060s",
                type = "shift"
            )
        )
        expect_equal(names(res),
            c("activity_drs", "experiment_id", "institution_id", "source_id",
              "member_id", "table_id", "lon", "lat", "interval", "datetime",
              "year", "month", "day", "hour", "minute", "dry_bulb_temperature",
              "delta", "alpha"
            )
        )
        expect_is(res$dry_bulb_temperature, "units")
        expect_is(res$delta, "units")
        expect_is(res$alpha, "units")

        # can use morphing using combined factor
        expect_is(class = "data.table",
            res <- morphing_from_mean(
                "dry_bulb_temperature",
                data_epw, data_mean, data_mean, data_mean,
                years = 2060L, labels = "2060s",
                type = "combined"
            )
        )
        expect_equal(names(res),
            c("activity_drs", "experiment_id", "institution_id", "source_id",
              "member_id", "table_id", "lon", "lat", "interval", "datetime",
              "year", "month", "day", "hour", "minute", "dry_bulb_temperature",
              "delta", "alpha"
            )
        )
        expect_is(res$dry_bulb_temperature, "units")
        expect_is(res$delta, "units")
        expect_is(res$alpha, "units")

        # can use morphing using combined factor
        expect_is(class = "data.table",
            res <- morphing_from_mean(
                "dry_bulb_temperature",
                data_epw, data_mean, data_mean, data_mean,
                years = 2060L, labels = "2060s",
                type = "combined"
            )
        )
        expect_equal(names(res),
            c("activity_drs", "experiment_id", "institution_id", "source_id",
              "member_id", "table_id", "lon", "lat", "interval", "datetime",
              "year", "month", "day", "hour", "minute", "dry_bulb_temperature",
              "delta", "alpha"
            )
        )
        expect_is(res$dry_bulb_temperature, "units")
        expect_is(res$delta, "units")
        expect_is(res$alpha, "units")

        # can use morphing using combined factor without max and min values
        expect_is(class = "data.table",
            res <- morphing_from_mean(
                "dry_bulb_temperature",
                data_epw, data_mean, NULL, NULL,
                years = 2060L, labels = "2060s",
                type = "combined"
            )
        )
        expect_equal(names(res),
            c("activity_drs", "experiment_id", "institution_id", "source_id",
              "member_id", "table_id", "lon", "lat", "interval", "datetime",
              "year", "month", "day", "hour", "minute", "dry_bulb_temperature",
              "delta", "alpha"
            )
        )
        expect_is(res$dry_bulb_temperature, "units")
        expect_is(res$delta, "units")
        expect_is(res$alpha, "units")

        # can fallback to "shift" if missing values found in data_min or
        # data_max
        expect_warning(
            res <- morphing_from_mean(
                "dry_bulb_temperature",
                data_epw, data_mean,
                copy(data_mean)[2000L, value := NA], copy(data_mean)[1000L, value := NA],
                years = 2060L, labels = "2060s",
                type = "combined"
            )
        )
        expect_equal(res$dry_bulb_temperature - data_epw$dry_bulb_temperature, res$delta)

    }
})

test_that("morphing_epw()", {
    skip_on_cran()

    cache <- "~/epwshiftr-cache"
    file <- "EC-Earth3.ssp585.tas.fst"
    path <- file.path(cache, file)

    epw <- file.path(cache, "SGP_Singapore.486980_IWEC.epw")
    if (file.exists(path) && file.exists(epw)) {
        data_epw <- read_epw(epw)$add_unit()$data()
        data_mean <- fst::read_fst(path, as.data.table = TRUE)

        d <- structure(
            list(epw = eplusr::read_epw(epw), meta = list(), data = copy(data_mean)),
            class = "epw_cmip6_data"
        )

        # can still process if no data found
        d$data[, variable := "tas-1"]
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(names(res),
            c("epw", "tdb", "tdew", "rh", "p", "hor_ir", "glob_rad", "norm_rad",
              "diff_rad", "wind", "total_cover", "opaque_cover"
            )
        )
        expect_is(res$epw, "Epw")
        expect_equal(res$tdb, data.table())
        expect_equal(res$tdew, data.table())
        expect_equal(res$rh, data.table())
        expect_equal(res$p, data.table())
        expect_equal(res$hor_ir, data.table())
        expect_equal(res$glob_rad, data.table())
        expect_equal(res$norm_rad, data.table())
        expect_equal(res$diff_rad, data.table())
        expect_equal(res$wind, data.table())
        expect_equal(res$total_cover, data.table())
        expect_equal(res$opaque_cover, data.table())

        d$data[, variable := "tas-1"]
        expect_is(res <- morphing_epw(d, 2060L, methods = c(tdb = "shift")), "epw_cmip6_morphed")
        expect_equal(names(res),
            c("epw", "tdb", "tdew", "rh", "p", "hor_ir", "glob_rad", "norm_rad",
              "diff_rad", "wind", "total_cover", "opaque_cover"
            )
        )
        expect_is(res$epw, "Epw")
        expect_equal(res$tdb, data.table())
        expect_equal(res$tdew, data.table())
        expect_equal(res$rh, data.table())
        expect_equal(res$p, data.table())
        expect_equal(res$hor_ir, data.table())
        expect_equal(res$glob_rad, data.table())
        expect_equal(res$norm_rad, data.table())
        expect_equal(res$diff_rad, data.table())
        expect_equal(res$wind, data.table())
        expect_equal(res$total_cover, data.table())
        expect_equal(res$opaque_cover, data.table())

        # can morph tas
        d$data[, variable := "tas"]
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(nrow(res$tdb), 8760L)
        expect_equal(nrow(res$tdew), 0L)

        # can morph rh
        d$data[, `:=`(variable = "hurs", value = 80, units = "%")]
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(nrow(res$rh), 8760L)
        expect_equal(nrow(res$tdew), 0L)

        # can calculate tdew
        d$data <- rbindlist(list(
            data_mean, copy(data_mean)[, `:=`(variable = "hurs", value = 80, units = "%")]
        ))
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(nrow(res$tdb), 8760L)
        expect_equal(nrow(res$rh), 8760L)
        expect_equal(nrow(res$tdew), 8760L)

        # can morph pa
        d$data <- copy(data_mean)[, `:=`(variable = "psl", value = 101300, units = "Pa")]
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(nrow(res$p), 8760L)

        # can morph hor_ir
        d$data <- copy(data_mean)[, `:=`(variable = "rlds", value = 400, units = "W/m^2")]
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(nrow(res$hor_ir), 8760L)

        # can morph global_rad, calculate diff_rad and norm_rad
        d$data <- copy(data_mean)[, `:=`(variable = "rsds", value = 700, units = "W/m^2")]
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(nrow(res$glob_rad), 8760L)
        expect_equal(nrow(res$diff_rad), 8760L)
        expect_equal(nrow(res$norm_rad), 8760L)
        expect_equal(morphing_diff_rad(data_epw, data.table()), data.table())
        expect_equal(morphing_norm_rad(data_epw, data.table()), data.table())

        # can morph wind speed
        d$data <- copy(data_mean)[, `:=`(variable = "sfcWind", value = 1, units = "m/s")]
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(nrow(res$wind), 8760L)

        # can morph total sky cover and opaque sky cover
        d$data <- copy(data_mean)[, `:=`(variable = "clt", value = 80, units = "%")]
        expect_is(res <- morphing_epw(d, 2060L), "epw_cmip6_morphed")
        expect_equal(nrow(res$total_cover), 8760L)
        expect_equal(nrow(res$opaque_cover), 8760L)
        expect_equal(morphing_total_sky_cover(data_epw, data.table()), data.table())
        expect_equal(morphing_opaque_sky_cover(data_epw, data.table()), data.table())
    }
})

test_that("future_epw()", {
    skip_on_cran()

    cache <- "~/epwshiftr-cache"
    file <- "EC-Earth3.ssp585.tas.fst"
    path <- file.path(cache, file)

    epw <- file.path(cache, "SGP_Singapore.486980_IWEC.epw")

    if (file.exists(path) && file.exists(epw)) {
        data_mean <- fst::read_fst(path, as.data.table = TRUE)

        d <- structure(
            list(epw = eplusr::read_epw(epw), meta = list(), data = data_mean[0L]),
            class = "epw_cmip6_data"
        )

        # can stop if no data to process
        morphed <- morphing_epw(d, 2060L)
        expect_error(suppressWarnings(future_epw(morphed)), "No morphed data found")

        # can give warnings on missing morphed variables
        d$data <- copy(data_mean)[, `:=`(variable = "clt", value = 80, units = "%")]
        morphed <- morphing_epw(d, 2060L)
        expect_warning(res <- future_epw(morphed, dir = file.path(tempdir(), "future_epw"), overwrite = TRUE))
        expect_is(res, "list")
        expect_equal(length(res), 1L)
        expect_is(res[[1]], "Epw")
        expect_true(file.exists(file.path(
            tempdir(), "future_epw",
            "ssp585",
            "EC-Earth3",
            "2060",
            "SGP_Singapore.486980_IWEC.ssp585.EC-Earth3.2060.epw"
        )))

        # can get full summary of future EPWs
        expect_warning(res <- future_epw(morphed, dir = file.path(tempdir(), "future_epw"), overwrite = TRUE, separate = FALSE, full = TRUE))
        expect_is(res, "data.table")
        expect_equal(names(res), c("experiment_id", "source_id", "interval", "epw", "path"))
        expect_true(file.exists(file.path(
            tempdir(), "future_epw",
            "SGP_Singapore.486980_IWEC.ssp585.EC-Earth3.2060.epw"
        )))
    }
})
