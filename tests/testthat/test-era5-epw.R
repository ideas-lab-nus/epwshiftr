# Provide an independently specified end-of-hour input covering an entire year.
# Zero shortwave avoids tying timing/thermodynamic tests to the solar fixture.
reference_test__bundle <- function(year = 2001L, timezone = 8) {
    begin <- as.POSIXct(sprintf("%d-01-01", year), tz = "UTC") - timezone * 3600
    end <- as.POSIXct(sprintf("%d-01-01", year + 1L), tz = "UTC") -
        timezone * 3600
    time <- seq(begin + 3600, end, by = 3600)
    list(
        data = data.table::data.table(
            utc_time = time,
            tas = 263.15,
            tdps = 258.15,
            ps = 101325,
            uas = 3,
            vas = 4,
            rsds = 0,
            fdir = 0,
            rlds = 1080000,
            clt = 0.51,
            pr = 0.001
        ),
        units = c(
            tas = "K",
            tdps = "K",
            ps = "Pa",
            uas = "m s-1",
            vas = "m s-1",
            rsds = "J m-2",
            fdir = "J m-2",
            rlds = "J m-2",
            clt = "1",
            pr = "m"
        ),
        grid = list(latitude = 23.25, longitude = 113.25),
        interval_seconds = 3600,
        provenance = list(source = "synthetic test")
    )
}

# Match the explicit metadata expected from a site with no baseline EPW.
reference_test__site <- function() {
    shift_site(
        "point",
        113.3,
        23.2,
        metadata = list(timezone = 8, elevation = 41, country = "China")
    )
}

test_that("reference EPW maps interval end, units, wind and explicit missing fields", {
    input <- reference_test__bundle()
    original <- serialize(input, NULL)
    normalized <- era_epw__normalize(input)
    annual <- era_epw__annual(
        normalized,
        era_epw__site(reference_test__site()),
        2001,
        "drop",
        "missing"
    )
    weather <- annual$weather
    expect_identical(serialize(input, NULL), original)
    expect_equal(nrow(weather), 8760)
    expect_equal(
        weather[1, .(month, day, hour, minute)],
        data.table::data.table(month = 1L, day = 1L, hour = 1L, minute = 60L)
    )
    expect_equal(
        weather[8760, .(month, day, hour)],
        data.table::data.table(
            month = 12L,
            day = 31L,
            hour = 24L
        )
    )
    expect_equal(
        annual$diagnostics$utc_time[[1]],
        as.POSIXct("2000-12-31 17:00:00", tz = "UTC")
    )
    expect_equal(weather$dry_bulb_temperature, rep(-10, 8760))
    expect_equal(weather$dew_point_temperature, rep(-15, 8760))
    expect_equal(
        weather$horizontal_infrared_radiation_intensity_from_sky,
        rep(300, 8760)
    )
    expect_equal(weather$wind_speed, rep(5, 8760))
    expect_equal(weather$wind_direction, rep(216.869897645844, 8760))
    expect_equal(weather$total_sky_cover, rep(5L, 8760))
    expect_equal(weather$opaque_sky_cover, rep(99, 8760))
    expect_equal(weather$liquid_precip_depth, rep(999, 8760))
    expect_equal(
        annual$diagnostics$source_precipitation_water_equivalent_mm,
        rep(1, 8760)
    )
    expect_true(all(
        annual$diagnostics$energyplus_humidity_ratio_kg_kg <
            annual$diagnostics$source_humidity_ratio_kg_kg
    ))
    # Direct value from the documented IFS equation, independent of the helper.
    expected_rh <- 100 *
        exp(
            17.502 *
                (258.15 - 273.16) /
                (258.15 - 32.19) -
                17.502 * (263.15 - 273.16) / (263.15 - 32.19)
        )
    expect_equal(weather$relative_humidity, rep(expected_rh, 8760))
    # IFS chapter 12 gas constants define the source mixing ratio. Verify
    # against q/(1-q) from equation 7.4, independently of the adapter's form.
    source_e <- 611.21 * exp(17.502 * (258.15 - 273.16) / (258.15 - 32.19))
    epsilon <- 287.0597 / 461.5250
    source_q <- epsilon * source_e / (101325 - (1 - epsilon) * source_e)
    expect_equal(
        annual$diagnostics$source_humidity_ratio_kg_kg,
        rep(source_q / (1 - source_q), 8760),
        tolerance = 1e-12
    )
    liquid <- era_epw__annual(
        normalized,
        era_epw__site(reference_test__site()),
        2001,
        "drop",
        "total_water_equivalent"
    )
    expect_equal(liquid$weather$liquid_precip_depth, rep(1, 8760))
    expect_equal(liquid$weather$liquid_precip_rate, rep(1, 8760))
})

test_that("reference leap policy checks omitted hours and preserves them", {
    input <- era_epw__normalize(reference_test__bundle(2000))
    meta <- era_epw__site(reference_test__site())
    drop <- era_epw__annual(input, meta, 2000, "drop", "missing")
    keep <- era_epw__annual(input, meta, 2000, "keep", "missing")
    expect_equal(nrow(drop$weather), 8760)
    expect_equal(nrow(drop$omitted), 24)
    expect_equal(nrow(keep$weather), 8784)
    expect_equal(sum(keep$weather$month == 2 & keep$weather$day == 29), 24)
    input$data <- input$data[
        -which(format(input$data$utc_time, "%m-%d") == "02-29")[[1]]
    ]
    expect_error(
        era_epw__annual(input, meta, 2000, "drop", "missing"),
        "Missing 1 required UTC"
    )
    western <- era_epw__calendar(2001, -8, "keep")
    expect_equal(
        tail(western$utc_time, 1),
        as.POSIXct("2002-01-01 08:00:00", tz = "UTC")
    )
})

test_that("reference inputs reject implicit repairs and ambiguous units or times", {
    native <- reference_test__bundle()
    flux <- reference_test__bundle()
    flux$data$rlds <- flux$data$rlds / 3600
    flux$units[["rlds"]] <- "W/m2"
    expect_equal(era_epw__normalize(native)$data, era_epw__normalize(flux)$data)
    expect_false(identical(
        era_epw__normalize(native)$sha256,
        era_epw__normalize(flux)$sha256
    ))
    bundle <- reference_test__bundle()
    bundle$units[["pr"]] <- "kg m-2 s-1"
    expect_error(era_epw__normalize(bundle), "preceding-hour depth")
    bundle <- reference_test__bundle()
    bundle$data$utc_time[2] <- bundle$data$utc_time[1]
    expect_error(era_epw__normalize(bundle), "unique ordered")
    bundle <- reference_test__bundle()
    bundle$interval_seconds <- 10800
    expect_error(era_epw__normalize(bundle), "interval_seconds")
    meta <- era_epw__site(reference_test__site())
    for (field in c("tas", "rlds", "ps")) {
        input <- era_epw__normalize(reference_test__bundle())
        input$data[[field]][1] <- NA_real_
        expect_error(
            era_epw__annual(input, meta, 2001, "drop", "missing"),
            "non-finite"
        )
    }
    for (field in c("rsds", "clt", "pr")) {
        input <- era_epw__normalize(reference_test__bundle())
        input$data[[field]][1] <- -1e-9
        expect_error(
            era_epw__annual(input, meta, 2001, "drop", "missing"),
            "physical inconsistency"
        )
    }
    input <- era_epw__normalize(reference_test__bundle())
    input$data$tdps[1] <- input$data$tas[1] + 0.0001
    expect_error(
        era_epw__annual(input, meta, 2001, "drop", "missing"),
        "physical inconsistency"
    )
})

test_that("reference shortwave conserves horizontal energy and rejects impossible beam", {
    bundle <- reference_test__bundle()
    meta <- era_epw__site(reference_test__site())
    calendar <- era_epw__calendar(2001, 8, "drop")
    geometry <- solar__epw_interval_geometry(calendar, 23.2, 113.3, 8)
    # Set a noon beam below the extraterrestrial limit. A shared geometry
    # fixture tests conservation; absolute solar accuracy is tested elsewhere.
    i <- which(calendar$month == 6 & calendar$day == 21 & calendar$hour == 13)
    bundle$data$rsds[i] <- 400 * 3600
    bundle$data$fdir[i] <- 250 * 3600
    annual <- era_epw__annual(
        era_epw__normalize(bundle),
        meta,
        2001,
        "drop",
        "missing"
    )
    expect_equal(annual$weather$diffuse_horizontal_radiation[i], 150)
    expect_equal(
        annual$weather$direct_normal_radiation[i] *
            geometry$effective_solar_projection[i],
        250
    )
    bundle$data$fdir[i] <- 401 * 3600
    expect_error(
        era_epw__annual(
            era_epw__normalize(bundle),
            meta,
            2001,
            "drop",
            "missing"
        ),
        "physical inconsistency"
    )
    bundle$data$fdir[i] <- bundle$data$rsds[i] <- 2000 * 3600
    expect_error(
        era_epw__annual(
            era_epw__normalize(bundle),
            meta,
            2001,
            "drop",
            "missing"
        ),
        "DNI exceeds"
    )
    bundle <- reference_test__bundle()
    bundle$data$fdir[1] <- bundle$data$rsds[1] <- 1
    expect_error(
        era_epw__annual(
            era_epw__normalize(bundle),
            meta,
            2001,
            "drop",
            "missing"
        ),
        "without solar projection"
    )
})

test_that("public reference conversion is offline, resumable and failure preserving", {
    local_mocked_bindings(cds__config = function(...) {
        stop("unexpected network")
    })
    dir <- tempfile()
    withr::defer(unlink(dir, recursive = TRUE))
    input <- reference_test__bundle()
    one <- shift_epw_reanalysis(
        shift_era5(2001),
        reference_test__site(),
        dir,
        data = input
    )
    expect_identical(one$status, "complete")
    expect_false(one$reused)
    expect_true(file.exists(one$epw))
    expect_equal(EpwFile$new(one$epw)$header("GROUND TEMPERATURES"), "0")
    two <- shift_epw_reanalysis(
        shift_era5(2001),
        reference_test__site(),
        dir,
        data = input
    )
    expect_true(two$reused)
    expect_identical(two$epw, one$epw)
    # A damaged output must not be accepted on a subsequent run.
    cat("\ncorrupt\n", file = one$epw, append = TRUE)
    three <- shift_epw_reanalysis(
        shift_era5(2001),
        reference_test__site(),
        dir,
        data = input
    )
    expect_false(three$reused)
    expect_true(file.exists(one$epw))
    expect_false(identical(one$epw, three$epw))
    hash <- checksum_file(three$epw)
    four <- shift_epw_reanalysis(
        shift_era5(2001),
        reference_test__site(),
        dir,
        data = input,
        resume = FALSE
    )
    expect_identical(checksum_file(four$epw), hash)
    input$data$tdps[1] <- input$data$tas[1] + 1
    expect_warning(
        failed <- shift_epw_reanalysis(
            shift_era5(2001),
            reference_test__site(),
            dir,
            data = input
        ),
        "1 reference EPW job"
    )
    expect_identical(failed$status, "failed")
    expect_true(is.na(failed$epw))
    expect_true(file.exists(failed$receipt))
    expect_false(dir.exists(file.path(dir, ".reference-lock")))
    expect_error(
        shift_epw_reanalysis(
            shift_era5(2001, product = "land"),
            reference_test__site(),
            dir,
            data = input
        ),
        "single_levels"
    )
    no_height <- shift_site("point", 113, 23, metadata = list(timezone = 8))
    expect_error(shift_epw_reanalysis(
        shift_era5(2001),
        no_height,
        dir,
        data = input
    ))
})

test_that("monthly CDS requests respect requested years and retain boundary days", {
    request <- era_epw__requests(shift_era5(1995:2014), reference_test__site())
    expect_length(request, 484)
    expect_identical(request[["1994-12-instant"]]$day, "31")
    expect_identical(request[["2015-01-accumulated"]]$day, "01")
    expect_length(request[["2000-02-instant"]]$day, 29)
    expect_length(request[["2001-02-instant"]]$variable, 6)
    expect_length(request[["2001-02-accumulated"]]$variable, 4)
    expect_equal(request[[1]]$area, c(23.325, 113.175, 23.075, 113.425))
    dir <- tempfile()
    dir.create(dir)
    withr::defer(unlink(dir, recursive = TRUE))
    input_hash <- era_epw__hash(list(
        dataset = era5__dataset_id("single_levels", "cds"),
        request = request[[1]]
    ))
    era_epw__json(
        list(input_sha256 = input_hash, status = "submitting"),
        file.path(dir, "receipt.json")
    )
    local_mocked_bindings(cds__submit = function(...) stop("must not resubmit"))
    expect_error(
        era_epw__retrieve(request[[1]], shift_era5(1995), dir),
        "no remote locator"
    )
})

# Write a tiny rectilinear NetCDF with distinct point values so nearest-point
# selection is checked against known values, not another extraction helper.
reference_test__netcdf <- function(path) {
    handle <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(handle))
    for (axis in c("latitude", "longitude", "valid_time")) {
        RNetCDF::dim.def.nc(handle, axis, 2)
        RNetCDF::var.def.nc(handle, axis, "NC_DOUBLE", axis)
    }
    RNetCDF::var.put.nc(handle, "latitude", c(23, 23.25))
    RNetCDF::var.put.nc(handle, "longitude", c(113, 113.25))
    RNetCDF::var.put.nc(handle, "valid_time", c(0, 3600))
    RNetCDF::att.put.nc(
        handle,
        "valid_time",
        "units",
        "NC_CHAR",
        "seconds since 2000-01-01 00:00:00"
    )
    RNetCDF::var.def.nc(
        handle,
        "t2m",
        "NC_DOUBLE",
        c("longitude", "latitude", "valid_time")
    )
    RNetCDF::att.put.nc(handle, "t2m", "units", "NC_CHAR", "K")
    RNetCDF::var.put.nc(handle, "t2m", array(271:278, dim = c(2, 2, 2)))
}

test_that("reference NetCDF extraction selects a bounded nearest point", {
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(path))
    reference_test__netcdf(path)
    handle <- RNetCDF::open.nc(path)
    withr::defer(RNetCDF::close.nc(handle))
    field <- era_epw__read_field(handle, "tas", reference_test__site())
    expect_equal(field$data$value, c(274, 278))
    expect_equal(field$grid, c(latitude = 23.25, longitude = 113.25))
    expect_equal(as.numeric(diff(field$data$utc_time), units = "secs"), 3600)
    expect_identical(field$units, "K")
})

test_that("CDS interruption resumes the original job and validates cached bytes", {
    dir <- tempfile()
    dir.create(dir)
    withr::defer(unlink(dir, recursive = TRUE))
    request <- list(variable = "test")
    submitted <- 0L
    downloaded <- 0L
    waits <- 0L
    local_mocked_bindings(
        cds__config = function() list(key = "private-test-key"),
        cds__submit = function(...) {
            submitted <<- submitted + 1L
            list(
                dataset_id = "era5",
                request_id = "same-job",
                monitor_url = "https://example.invalid/same-job",
                status = "accepted"
            )
        },
        cds__wait = function(job, ...) {
            waits <<- waits + 1L
            expect_identical(job$request_id, "same-job")
            if (waits == 1L) {
                stop("temporary interruption")
            }
            job$status <- "successful"
            job
        },
        cds__result = function(...) {
            list(url = "https://example.invalid/result")
        },
        cds__download = function(asset, target, ...) {
            downloaded <<- downloaded + 1L
            writeBin(charToRaw("known response"), target)
        }
    )
    expect_error(
        era_epw__retrieve(request, shift_era5(2001), dir),
        "temporary interruption"
    )
    path <- era_epw__retrieve(request, shift_era5(2001), dir)
    expect_equal(submitted, 1)
    expect_equal(downloaded, 1)
    expect_identical(era_epw__retrieve(request, shift_era5(2001), dir), path)
    expect_equal(downloaded, 1)
    expect_equal(waits, 2)
    writeBin(charToRaw("corrupted"), path)
    era_epw__retrieve(request, shift_era5(2001), dir)
    expect_equal(downloaded, 2)
    expect_equal(submitted, 1)
})

test_that("batch results retain absent years and lock ownership is respected", {
    dir <- tempfile()
    dir.create(dir)
    withr::defer(unlink(dir, recursive = TRUE))
    lock <- file.path(dir, ".reference-lock")
    dir.create(lock)
    writeLines("active test owner", file.path(lock, "owner.json"))
    expect_error(
        shift_epw_reanalysis(
            shift_era5(2001),
            reference_test__site(),
            dir,
            data = reference_test__bundle()
        ),
        "locked"
    )
    expect_true(file.exists(file.path(lock, "owner.json")))
    unlink(lock, recursive = TRUE)
    sites <- list(
        reference_test__site(),
        shift_site(
            "other",
            113.3,
            23.2,
            metadata = list(timezone = 8, elevation = 41)
        )
    )
    expect_warning(
        result <- shift_epw_reanalysis(
            shift_era5(2001:2002),
            sites,
            dir,
            data = list(
                point = reference_test__bundle(),
                other = reference_test__bundle()
            )
        ),
        "2 reference EPW job"
    )
    expect_equal(nrow(result), 4)
    expect_equal(result[year == 2001, status], rep("complete", 2))
    expect_equal(result[year == 2002, status], rep("failed", 2))
})

# Store all ten fields on the same native hourly NetCDF axis for an end-to-end
# offline reader test; this also exercises unit and source-hash provenance.
reference_test__netcdf_year <- function(path, bundle) {
    handle <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(handle))
    RNetCDF::dim.def.nc(handle, "time", nrow(bundle$data))
    RNetCDF::var.def.nc(handle, "time", "NC_DOUBLE", "time")
    RNetCDF::var.put.nc(handle, "time", as.numeric(bundle$data$utc_time))
    RNetCDF::att.put.nc(
        handle,
        "time",
        "units",
        "NC_CHAR",
        "seconds since 1970-01-01 00:00:00"
    )
    for (axis in c("latitude", "longitude")) {
        RNetCDF::var.def.nc(handle, axis, "NC_DOUBLE", character())
        RNetCDF::var.put.nc(handle, axis, bundle$grid[[axis]])
    }
    manifest <- era5__variable_manifest()
    for (v in ERA_EPW_VARIABLES) {
        name <- manifest[variable_id == v]$aliases[[1L]][[1L]]
        RNetCDF::var.def.nc(handle, name, "NC_DOUBLE", "time")
        RNetCDF::att.put.nc(handle, name, "units", "NC_CHAR", bundle$units[[v]])
        RNetCDF::var.put.nc(handle, name, bundle$data[[v]])
    }
}

test_that("a complete native NetCDF converts without contacting CDS", {
    path <- tempfile(fileext = ".nc")
    dir <- tempfile()
    withr::defer(unlink(c(path, dir), recursive = TRUE))
    bundle <- reference_test__bundle()
    reference_test__netcdf_year(path, bundle)
    local_mocked_bindings(cds__config = function(...) {
        stop("unexpected network")
    })
    read <- era_epw__read_files(path, reference_test__site())
    expect_equal(
        as.numeric(read$data$utc_time),
        as.numeric(bundle$data$utc_time)
    )
    expect_equal(read$data$rlds, bundle$data$rlds)
    expect_identical(read$provenance$files$sha256, checksum_file(path))
    result <- shift_epw_reanalysis(
        shift_era5(2001),
        reference_test__site(),
        dir,
        data = path
    )
    expect_identical(result$status, "complete")
    expect_equal(nrow(EpwFile$new(result$epw)$data()), 8760)
    expect_error(
        era_epw__read_files(c(path, path), reference_test__site()),
        "duplicated"
    )
})

# Provider identity must be checked even if a caller supplies a structurally
# valid future reanalysis descriptor with the same product name.
test_that("reference EPW rejects a different reanalysis provider", {
    source <- shift_era5(2001)
    source@dataset <- "era6"
    expect_error(
        shift_epw_reanalysis(source, reference_test__site(), tempfile()),
        "requires"
    )
})

# Packed CF storage must be decoded before temperatures reach weather conversion.
test_that("reference NetCDF extraction unpacks scale and offset", {
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(path))
    reference_test__netcdf(path)
    handle <- RNetCDF::open.nc(path, write = TRUE)
    RNetCDF::att.put.nc(handle, "t2m", "scale_factor", "NC_DOUBLE", 0.1)
    RNetCDF::att.put.nc(handle, "t2m", "add_offset", "NC_DOUBLE", 250)
    RNetCDF::close.nc(handle)
    handle <- RNetCDF::open.nc(path)
    withr::defer(RNetCDF::close.nc(handle))
    field <- era_epw__read_field(handle, "tas", reference_test__site())
    expect_equal(field$data$value, c(277.4, 277.8))
})

# An explicit local-data map cannot authorize a remote fallback for missing data.
test_that("missing per-site local input remains offline", {
    downloads <- 0L
    local_mocked_bindings(era_epw__download = function(...) {
        downloads <<- downloads + 1L
        stop("unexpected download")
    })
    missing <- shift_site(
        "missing",
        113.3,
        23.2,
        metadata = list(timezone = 8, elevation = 41)
    )
    expect_warning(
        result <- shift_epw_reanalysis(
            shift_era5(2001),
            list(reference_test__site(), missing),
            withr::local_tempdir(),
            data = list(point = reference_test__bundle(), missing = NULL)
        ),
        "1 reference EPW job"
    )
    expect_identical(downloads, 0L)
    expect_identical(result$status, c("complete", "failed"))
    expect_match(result$error[[2L]], "Local input is missing")
})
