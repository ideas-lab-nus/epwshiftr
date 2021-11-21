test_that("get_nc_meta()", {
    skip_on_cran()
    cache <- "~/epwshiftr-cache"
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20590101-20591231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(meta <- get_nc_meta(path), "list")
        expect_equal(names(meta),
            c("mip_era", "activity_id", "institution_id", "source_id",
              "experiment_id", "variant_label", "table_id", "grid_label",
              "nominal_resolution", "variable_id", "tracking_id",
              "standard_name", "units", "time_units", "time_calendar"
            )
        )
    }
})

test_that("get_nc_atts()", {
    skip_on_cran()
    cache <- "~/epwshiftr-cache"
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(atts <- get_nc_atts(path), "data.table")
        expect_equal(names(atts), c("id", "variable", "attribute", "value"))
        expect_true(all(unique(atts$variable) %in% c("height", "lat", "lon", "NC_GLOBAL", "tas", "time")))
        expect_equal(atts[variable == "NC_GLOBAL", unique(id)], -1L)
    }
})

test_that("get_nc_vars()", {
    skip_on_cran()
    cache <- "~/epwshiftr-cache"
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(vars <- get_nc_vars(path), "data.table")
        expect_equal(names(vars), c("id", "name", "type", "ndims", "natts"))
    }
})

test_that("get_nc_dims()", {
    skip_on_cran()
    cache <- "~/epwshiftr-cache"
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(dims <- get_nc_dims(path), "data.table")
        expect_equal(names(dims), c("id", "name", "length", "unlim"))
    }
})

test_that("get_nc_axes()", {
    skip_on_cran()
    cache <- "~/epwshiftr-cache"
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(axes <- get_nc_axes(path), "data.table")
        expect_equal(names(axes), c("axis", "variable", "dimension"))
    }
})

test_that("get_nc_time()", {
    skip_on_cran()
    cache <- "~/epwshiftr-cache"
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(time <- get_nc_time(path), "POSIXct")
        expect_equal(length(time), 366)

        expect_is(time <- get_nc_time(path, range = TRUE), "POSIXct")
        expect_equal(length(time), 2L)
    }
})

test_that("match_nc_time()", {
    skip_on_cran()
    cache <- "~/epwshiftr-cache"
    file <- "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc"
    path <- file.path(cache, file)

    if (file.exists(path)) {
        expect_is(matched <- match_nc_time(path, 2060), "list")
        expect_equal(names(matched), c("datetime", "which"))
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 366L)
        expect_equal(length(matched$which), 1L)
        expect_equal(matched$which[[1L]], 1:366)

        expect_is(matched <- match_nc_time(path), "list")
        expect_equal(names(matched), c("datetime", "which"))
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 366L)
        expect_equal(length(matched$which), 1L)
        expect_equal(matched$which[[1L]], 1:366)

        expect_is(matched <- match_nc_time(path, 2059), "list")
        expect_equal(length(matched$datetime), 1L)
        expect_equal(length(matched$datetime[[1L]]), 0L)
        expect_equal(length(matched$which), 1L)
        expect_equal(length(matched$which[[1L]]), 0L)
    }
})
