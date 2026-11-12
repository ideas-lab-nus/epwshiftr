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
    skip_on_cran()
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

# A complete native UTC leap year plus padding, with independently calculable
# alternating accumulated inputs. Solar fields are zero to isolate timing.
fractional_test__bundle <- function() {
    time <- seq(
        as.POSIXct("1999-12-30", tz = "UTC"),
        as.POSIXct("2001-01-02", tz = "UTC"),
        by = "hour"
    )
    even <- (as.numeric(time) / 3600) %% 2 == 0
    list(
        data = data.table::data.table(
            utc_time = time,
            tas = 280 + as.integer(even),
            tdps = 270,
            ps = 101325,
            uas = 3,
            vas = 4,
            rsds = 0,
            fdir = 0,
            rlds = data.table::fifelse(even, 200, 400),
            clt = 0.5,
            pr = data.table::fifelse(even, 1, 3)
        ),
        units = c(
            tas = "K",
            tdps = "K",
            ps = "Pa",
            uas = "m/s",
            vas = "m/s",
            rsds = "Wh/m2",
            fdir = "Wh/m2",
            rlds = "Wh/m2",
            clt = "1",
            pr = "mm"
        ),
        grid = list(latitude = -12.5, longitude = 131),
        interval_seconds = 3600,
        provenance = list(source = "synthetic fractional-offset test")
    )
}

test_that("fractional reference calendars preserve overlap energy and leap hours", {
    input <- era_epw__normalize(fractional_test__bundle())
    site <- shift_site(
        "darwin",
        lat = -12.415,
        lon = 130.877,
        metadata = list(timezone = 9.5, elevation = 31.4)
    )
    metadata <- era_epw__site(site)
    original <- serialize(input, NULL)
    annual <- era_epw__annual(
        input,
        metadata,
        2000,
        "keep",
        "total_water_equivalent"
    )
    expect_equal(nrow(annual$weather), 8784)
    expect_equal(annual$weather$dry_bulb_temperature, rep(280.5 - 273.15, 8784))
    expect_equal(
        annual$weather$horizontal_infrared_radiation_intensity_from_sky,
        rep(300, 8784)
    )
    expect_equal(annual$weather$liquid_precip_depth, rep(2, 8784))
    expect_equal(sum(annual$weather$liquid_precip_depth), 17568)
    expect_identical(serialize(input, NULL), original)
    expect_equal(
        annual$alignment$source_utc_lower[1],
        as.POSIXct("1999-12-31 15:00:00", tz = "UTC")
    )
    expect_equal(
        annual$alignment$source_utc_upper[1],
        as.POSIXct("1999-12-31 16:00:00", tz = "UTC")
    )
    expect_equal(annual$alignment$upper_weight, rep(0.5, 8784))
    dropped <- era_epw__annual(
        input,
        metadata,
        2000,
        "drop",
        "total_water_equivalent"
    )
    expect_equal(nrow(dropped$weather), 8760)
    expect_equal(nrow(dropped$omitted), 24)
    expect_equal(sum(!dropped$alignment$retained), 24)
    quarter <- era_epw__calendar(2000, 5.75, "keep")
    aligned <- era_epw__align_local(input$data, quarter)
    expect_equal(unique(aligned$alignment$upper_weight), 0.25)
    expect_setequal(unique(aligned$data$rlds), c(250, 350))
    gap <- input
    gap$data <- gap$data[
        -which(gap$data$utc_time == annual$alignment$source_utc_lower[1])
    ]
    expect_error(
        era_epw__annual(gap, metadata, 2000, "drop", "missing"),
        "Missing"
    )
    directory <- tempfile()
    withr::defer(unlink(directory, recursive = TRUE))
    result <- shift_epw_reanalysis(
        shift_era5(2000),
        site,
        directory,
        data = fractional_test__bundle(),
        leap_day = "keep"
    )
    expect_identical(result$status, "complete")
    expect_match(readLines(result$epw, n = 1), ",9.5,31.4$")
    mapping <- file.path(dirname(result$epw), "time-alignment.csv")
    expect_true(file.exists(mapping))
    expect_equal(nrow(data.table::fread(mapping)), 8784)
    reused <- shift_epw_reanalysis(
        shift_era5(2000),
        site,
        directory,
        data = fractional_test__bundle(),
        leap_day = "keep"
    )
    expect_true(reused$reused)
})

# Two individually unsaturated endpoints can yield supersaturation under T/e
# interpolation. This is a physical failure, never a reason to silently clip.
test_that("fractional alignment exposes moisture conflicts and invalid endpoints", {
    input <- era_epw__normalize(fractional_test__bundle())$data[1:2]
    data.table::set(input, j = "tas", value = c(293.0739746, 291.6134949))
    data.table::set(input, j = "tdps", value = c(293.057251, 291.6097412))
    calendar <- era_epw__calendar(2000, 9.5, "keep")[1]
    calendar$utc_time <- input$utc_time[1] + 1800
    expect_true(all(input$tdps < input$tas))
    expect_error(
        era_epw__align_local(input, calendar),
        "physical inconsistency"
    )
    input$tdps <- 270
    input$rsds <- c(-1, 3)
    expect_error(
        era_epw__align_local(input, calendar),
        "physical inconsistency"
    )
    input$rsds <- 0
    input$ps <- c(100, 101325)
    expect_error(
        era_epw__align_local(input, calendar),
        "physical inconsistency"
    )
})

# Write a complete synthetic year with a known affine temperature field and
# opposing east/west winds. Independent analytic expectations expose errors
# in corner order, axis direction, derived-variable order and time alignment.
era_spatial_test__file <- function(
    path,
    reverse_lat = FALSE,
    longitude = c(-0.125, 0.125),
    missing_corner = FALSE
) {
    handle <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(handle))
    latitude <- if (reverse_lat) c(0.25, 0) else c(0, 0.25)
    time <- as.numeric(as.POSIXct("2001-01-01", tz = "UTC")) + (1:8760) * 3600
    axes <- list(longitude = longitude, latitude = latitude, time = time)
    for (axis in names(axes)) {
        RNetCDF::dim.def.nc(handle, axis, length(axes[[axis]]))
        RNetCDF::var.def.nc(handle, axis, "NC_DOUBLE", axis)
        RNetCDF::var.put.nc(handle, axis, axes[[axis]])
    }
    RNetCDF::att.put.nc(
        handle,
        "time",
        "units",
        "NC_CHAR",
        "seconds since 1970-01-01 00:00:00"
    )
    base <- c(
        t2m = 283.15,
        d2m = 273.15,
        sp = 101325,
        u10 = 0,
        v10 = 3,
        ssrd = 0,
        fdir = 0,
        strd = 1080000,
        tcc = 0.5,
        tp = 0
    )
    units <- c(
        "K",
        "K",
        "Pa",
        "m s-1",
        "m s-1",
        "J m-2",
        "J m-2",
        "J m-2",
        "1",
        "m"
    )
    for (i in seq_along(base)) {
        variable <- names(base)[[i]]
        RNetCDF::var.def.nc(handle, variable, "NC_DOUBLE", names(axes))
        RNetCDF::att.put.nc(handle, variable, "units", "NC_CHAR", units[[i]])
        corners <- rep(base[[i]], 4)
        if (variable %in% c("t2m", "d2m")) {
            corners <- corners + c(0, 4, 8, 12)
        }
        if (variable == "u10") {
            corners <- c(-2, 2, -2, 2)
        }
        if (missing_corner && variable == "t2m") {
            RNetCDF::att.put.nc(
                handle,
                variable,
                "_FillValue",
                "NC_DOUBLE",
                -9999
            )
            corners[[4L]] <- -9999
        }
        if (reverse_lat) {
            corners <- corners[c(3, 4, 1, 2)]
        }
        RNetCDF::var.put.nc(
            handle,
            variable,
            array(rep(corners, 8760), c(2, 2, 8760))
        )
    }
}

# Common site metadata isolates extraction from timezone and height choices.
era_spatial_test__site <- function(lon = 0, lat = 0.0625) {
    shift_site(
        "spatial",
        lon,
        lat,
        metadata = list(timezone = 0, elevation = 0)
    )
}

test_that("local ERA5 spatial methods produce traceable annual weather and distinct caches", {
    path <- tempfile(fileext = ".nc")
    directory <- tempfile()
    withr::defer(unlink(c(path, directory), recursive = TRUE))
    era_spatial_test__file(path)
    site <- era_spatial_test__site()
    results <- lapply(c("nearest", "bilinear", "mean"), function(method) {
        result <- shift_epw_reanalysis(
            shift_era5(2001),
            site,
            directory,
            data = path,
            extraction_method = method
        )
        expect_identical(result$status, "complete")
        expect_false(result$reused)
        result
    })
    temperatures <- vapply(
        results,
        function(result) {
            weather <- EpwFile$new(result$epw)$data()
            expect_equal(nrow(weather), 8760)
            weather$dry_bulb_temperature[[1L]]
        },
        numeric(1L)
    )
    expect_equal(temperatures, c(10, 14, 16))
    interpolated <- EpwFile$new(results[[2L]]$epw)$data()
    # Averaging wind speed first would retain sqrt(13), rather than 3 m/s.
    expect_equal(interpolated$wind_speed, rep(3, 8760))
    receipt <- jsonlite::read_json(results[[2L]]$receipt, simplifyVector = TRUE)
    expect_identical(receipt$options$extraction_method, "bilinear")
    expect_equal(
        receipt$provenance$spatial$sources$weight,
        c(0.375, 0.375, 0.125, 0.125)
    )
    expect_identical(receipt$provenance$spatial$grid_role, "target_point")
    expect_equal(receipt$grid$latitude, 0.0625)
    expect_equal(
        receipt$provenance$spatial$sources$grid_lat,
        c(0, 0, 0.25, 0.25)
    )
    expect_true(all(is.finite(receipt$provenance$spatial$sources$grid_dist_km)))
    expect_length(
        unique(vapply(results, function(x) x$receipt, character(1L))),
        3
    )
    again <- shift_epw_reanalysis(
        shift_era5(2001),
        site,
        directory,
        data = path,
        extraction_method = "bilinear"
    )
    expect_true(again$reused)
    expect_identical(again$epw, results[[2L]]$epw)
    idw <- shift_epw_reanalysis(
        shift_era5(2001),
        era_spatial_test__site(-0.125, 0),
        directory,
        data = path,
        extraction_method = "idw"
    )
    expect_identical(idw$status, "complete")
    expect_equal(
        EpwFile$new(idw$epw)$data()$dry_bulb_temperature,
        rep(10, 8760)
    )
    idw_receipt <- jsonlite::read_json(idw$receipt, simplifyVector = TRUE)
    expect_equal(idw_receipt$provenance$spatial$sources$weight, c(1, 0, 0, 0))
})

test_that("ERA5 interpolation respects descending axes, dateline and regional edges", {
    path <- tempfile(fileext = ".nc")
    directory <- tempfile()
    withr::defer(unlink(c(path, directory), recursive = TRUE))
    era_spatial_test__file(
        path,
        reverse_lat = TRUE,
        longitude = c(179.875, -179.875)
    )
    result <- shift_epw_reanalysis(
        shift_era5(2001),
        era_spatial_test__site(180),
        directory,
        data = path,
        extraction_method = "bilinear"
    )
    expect_identical(result$status, "complete")
    expect_equal(
        EpwFile$new(result$epw)$data()$dry_bulb_temperature,
        rep(14, 8760)
    )
    # An exact eastern endpoint remains a valid cell boundary.
    edge <- shift_epw_reanalysis(
        shift_era5(2001),
        era_spatial_test__site(-179.875, 0.25),
        directory,
        data = path,
        extraction_method = "bilinear"
    )
    expect_identical(edge$status, "complete")
    expect_equal(
        EpwFile$new(edge$epw)$data()$dry_bulb_temperature,
        rep(22, 8760)
    )
    expect_warning(
        outside <- shift_epw_reanalysis(
            shift_era5(2001),
            era_spatial_test__site(179.5),
            directory,
            data = path,
            extraction_method = "bilinear"
        ),
        "failed"
    )
    expect_identical(outside$status, "failed")
    expect_match(outside$error, "outside")
})

test_that("ERA5 interpolation cannot silently drop missing sources or fetch point data", {
    path <- tempfile(fileext = ".nc")
    directory <- tempfile()
    withr::defer(unlink(c(path, directory), recursive = TRUE))
    era_spatial_test__file(path, missing_corner = TRUE)
    expect_warning(
        result <- shift_epw_reanalysis(
            shift_era5(2001),
            era_spatial_test__site(-0.125, 0),
            directory,
            data = path,
            extraction_method = "idw"
        ),
        "failed"
    )
    expect_identical(result$status, "failed")
    expect_match(result$error, "finite")
    expect_error(
        shift_epw_reanalysis(
            shift_era5(2001),
            era_spatial_test__site(),
            tempfile(),
            extraction_method = "bilinear"
        ),
        "local NetCDF"
    )
    expect_error(
        shift_epw_reanalysis(
            shift_era5(2001),
            era_spatial_test__site(),
            tempfile(),
            data = list(data = data.frame(x = 1)),
            extraction_method = "mean"
        ),
        "local NetCDF"
    )
})

# vim: fdm=marker :
