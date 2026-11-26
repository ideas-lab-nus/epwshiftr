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

reference_test__annual <- function(
    bundle = reference_test__bundle(),
    year = 2001,
    leap_day = "drop",
    precipitation = "missing",
    site = reference_test__site(),
    normalized = NULL
) {
    input <- if (is.null(normalized)) era_epw__normalize(bundle) else normalized
    era_epw__annual(input, era_epw__site(site), year, leap_day, precipitation)
}

reference_test__run <- function(
    dir,
    data = reference_test__bundle(),
    year = 2001,
    sites = reference_test__site(),
    ...
) {
    shift_epw_reanalysis(shift_era5(year), sites, dir, data = data, ...)
}

test_that("reference EPW maps interval end, units, wind and explicit missing fields", {
    input <- reference_test__bundle()
    original <- serialize(input, NULL)
    normalized <- era_epw__normalize(input)
    annual <- reference_test__annual(normalized = normalized)
    weather <- annual$weather
    expect_identical(serialize(input, NULL), original)
    expect_equal(nrow(weather), 8760)
    expect_equal(
        weather[1, .(month, day, hour, minute)],
        data.table::data.table(month = 1L, day = 1L, hour = 1L, minute = 60L)
    )
    expect_equal(
        weather[8760, .(month, day, hour)],
        data.table::data.table(month = 12L, day = 31L, hour = 24L)
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
    liquid <- reference_test__annual(
        normalized = normalized,
        precipitation = "total_water_equivalent"
    )
    expect_equal(liquid$weather$liquid_precip_depth, rep(1, 8760))
    expect_equal(liquid$weather$liquid_precip_rate, rep(1, 8760))
})

test_that("reference leap policy checks omitted hours and preserves them", {
    input <- era_epw__normalize(reference_test__bundle(2000))
    drop <- reference_test__annual(normalized = input, year = 2000)
    keep <- reference_test__annual(
        normalized = input,
        year = 2000,
        leap_day = "keep"
    )
    expect_equal(nrow(drop$weather), 8760)
    expect_equal(nrow(drop$omitted), 24)
    expect_equal(nrow(keep$weather), 8784)
    expect_equal(sum(keep$weather$month == 2 & keep$weather$day == 29), 24)
    input$data <- input$data[
        -which(format(input$data$utc_time, "%m-%d") == "02-29")[[1]]
    ]
    expect_error(
        reference_test__annual(normalized = input, year = 2000),
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
    for (field in c("tas", "rlds", "ps")) {
        input <- era_epw__normalize(reference_test__bundle())
        input$data[[field]][1] <- NA_real_
        expect_error(reference_test__annual(normalized = input), "non-finite")
    }
    for (field in c("rsds", "clt", "pr")) {
        input <- era_epw__normalize(reference_test__bundle())
        input$data[[field]][1] <- -1e-9
        expect_error(
            reference_test__annual(normalized = input),
            "physical inconsistency"
        )
    }
    input <- era_epw__normalize(reference_test__bundle())
    input$data$tdps[1] <- input$data$tas[1] + 0.0001
    expect_error(
        reference_test__annual(normalized = input),
        "physical inconsistency"
    )
})

test_that("reference shortwave conserves horizontal energy and rejects impossible beam", {
    bundle <- reference_test__bundle()
    calendar <- era_epw__calendar(2001, 8, "drop")
    geometry <- solar__epw_interval_geometry(calendar, 23.2, 113.3, 8)
    # Set a noon beam below the extraterrestrial limit. A shared geometry
    # fixture tests conservation; absolute solar accuracy is tested elsewhere.
    i <- which(calendar$month == 6 & calendar$day == 21 & calendar$hour == 13)
    bundle$data$rsds[i] <- 400 * 3600
    bundle$data$fdir[i] <- 250 * 3600
    annual <- reference_test__annual(bundle)
    expect_equal(annual$weather$diffuse_horizontal_radiation[i], 150)
    expect_equal(
        annual$weather$direct_normal_radiation[i] *
            geometry$effective_solar_projection[i],
        250
    )
    bundle$data$fdir[i] <- 401 * 3600
    expect_error(reference_test__annual(bundle), "physical inconsistency")
    bundle$data$fdir[i] <- bundle$data$rsds[i] <- 2000 * 3600
    expect_error(reference_test__annual(bundle), "DNI exceeds")
    bundle <- reference_test__bundle()
    bundle$data$fdir[1] <- bundle$data$rsds[1] <- 1
    expect_error(reference_test__annual(bundle), "without solar projection")
})

test_that("public reference conversion is offline, resumable and failure preserving", {
    local_mocked_bindings(cds__config = function(...) {
        stop("unexpected network")
    })
    dir <- withr::local_tempdir()
    input <- reference_test__bundle()
    one <- reference_test__run(dir, data = input)
    expect_identical(one$status, "complete")
    expect_false(one$reused)
    expect_true(file.exists(one$epw))
    expect_equal(EpwFile$new(one$epw)$header("GROUND TEMPERATURES"), "0")
    two <- reference_test__run(dir, data = input)
    expect_true(two$reused)
    expect_identical(two$epw, one$epw)
    # A damaged output must not be accepted on a subsequent run.
    cat("\ncorrupt\n", file = one$epw, append = TRUE)
    three <- reference_test__run(dir, data = input)
    expect_false(three$reused)
    expect_true(file.exists(one$epw))
    expect_false(identical(one$epw, three$epw))
    hash <- checksum_file(three$epw)
    four <- reference_test__run(dir, data = input, resume = FALSE)
    expect_identical(checksum_file(four$epw), hash)
    input$data$tdps[1] <- input$data$tas[1] + 1
    expect_warning(
        failed <- reference_test__run(dir, data = input),
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
    # Provider identity must be checked even if a caller supplies a structurally
    # valid future reanalysis descriptor with the same product name.
    other_provider <- shift_era5(2001)
    other_provider@dataset <- "era6"
    expect_error(
        shift_epw_reanalysis(other_provider, reference_test__site(), dir),
        "requires"
    )
    no_height <- shift_site("point", 113, 23, metadata = list(timezone = 8))
    expect_error(reference_test__run(dir, data = input, sites = no_height))
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
    dir <- withr::local_tempdir()
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

test_that("reference NetCDF extraction selects bounded nearest point and unpacks scale/offset", {
    path <- withr::local_tempfile(fileext = ".nc")
    reference_test__netcdf(path)
    handle <- RNetCDF::open.nc(path)
    field <- tryCatch(
        era_epw__read_field(handle, "tas", reference_test__site()),
        finally = RNetCDF::close.nc(handle)
    )
    expect_equal(field$data$value, c(274, 278))
    expect_equal(field$grid, c(latitude = 23.25, longitude = 113.25))
    expect_equal(as.numeric(diff(field$data$utc_time), units = "secs"), 3600)
    expect_identical(field$units, "K")

    # Packed CF storage must be decoded before temperatures reach weather conversion.
    handle <- RNetCDF::open.nc(path, write = TRUE)
    RNetCDF::att.put.nc(handle, "t2m", "scale_factor", "NC_DOUBLE", 0.1)
    RNetCDF::att.put.nc(handle, "t2m", "add_offset", "NC_DOUBLE", 250)
    RNetCDF::close.nc(handle)
    handle <- RNetCDF::open.nc(path)
    withr::defer(RNetCDF::close.nc(handle))
    packed <- era_epw__read_field(handle, "tas", reference_test__site())
    expect_equal(packed$data$value, c(277.4, 277.8))
})

test_that("CDS interruption resumes the original job and validates cached bytes", {
    dir <- withr::local_tempdir()
    request <- list(variable = "test")
    submitted <- downloaded <- waits <- 0L
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
    dir <- withr::local_tempdir()
    lock <- file.path(dir, ".reference-lock")
    dir.create(lock)
    writeLines("active test owner", file.path(lock, "owner.json"))
    expect_error(reference_test__run(dir), "locked")
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
        result <- reference_test__run(
            dir,
            year = 2001:2002,
            sites = sites,
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

test_that("complete native NetCDF converts offline and missing per-site input stays offline", {
    skip_on_cran()
    path <- withr::local_tempfile(fileext = ".nc")
    dir <- withr::local_tempdir()
    bundle <- reference_test__bundle()
    reference_test__netcdf_year(path, bundle)
    downloads <- 0L
    local_mocked_bindings(
        cds__config = function(...) stop("unexpected network"),
        era_epw__download = function(...) {
            downloads <<- downloads + 1L
            stop("unexpected download")
        }
    )
    read <- era_epw__read_files(path, reference_test__site())
    expect_equal(
        as.numeric(read$data$utc_time),
        as.numeric(bundle$data$utc_time)
    )
    expect_equal(read$data$rlds, bundle$data$rlds)
    expect_identical(read$provenance$files$sha256, checksum_file(path))
    result <- reference_test__run(dir, data = path)
    expect_identical(result$status, "complete")
    expect_equal(nrow(EpwFile$new(result$epw)$data()), 8760)
    expect_error(
        era_epw__read_files(c(path, path), reference_test__site()),
        "duplicated"
    )

    # An explicit local-data map cannot authorize a remote fallback for missing data.
    missing <- shift_site(
        "missing",
        113.3,
        23.2,
        metadata = list(timezone = 8, elevation = 41)
    )
    expect_warning(
        res_miss <- reference_test__run(
            withr::local_tempdir(),
            sites = list(reference_test__site(), missing),
            data = list(point = bundle, missing = NULL)
        ),
        "1 reference EPW job"
    )
    expect_identical(downloads, 0L)
    expect_identical(res_miss$status, c("complete", "failed"))
    expect_match(res_miss$error[[2L]], "Local input is missing")
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
    bundle <- reference_test__bundle()
    bundle$data <- data.table::data.table(
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
    )
    bundle$units[c("uas", "vas", "rsds", "fdir", "rlds", "pr")] <- c(
        "m/s",
        "m/s",
        "Wh/m2",
        "Wh/m2",
        "Wh/m2",
        "mm"
    )
    bundle$grid <- list(latitude = -12.5, longitude = 131)
    bundle$provenance <- list(source = "synthetic fractional-offset test")
    bundle
}

test_that("fractional reference calendars preserve overlap energy and leap hours", {
    input <- era_epw__normalize(fractional_test__bundle())
    site <- shift_site(
        "darwin",
        lat = -12.415,
        lon = 130.877,
        metadata = list(timezone = 9.5, elevation = 31.4)
    )
    original <- serialize(input, NULL)
    annual <- reference_test__annual(
        normalized = input,
        year = 2000,
        leap_day = "keep",
        precipitation = "total_water_equivalent",
        site = site
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
    dropped <- reference_test__annual(
        normalized = input,
        year = 2000,
        precipitation = "total_water_equivalent",
        site = site
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
        reference_test__annual(normalized = gap, year = 2000, site = site),
        "Missing"
    )
    directory <- withr::local_tempdir()
    result <- reference_test__run(
        directory,
        year = 2000,
        sites = site,
        data = fractional_test__bundle(),
        leap_day = "keep"
    )
    expect_identical(result$status, "complete")
    expect_match(readLines(result$epw, n = 1), ",9.5,31.4$")
    mapping <- file.path(dirname(result$epw), "time-alignment.csv")
    expect_true(file.exists(mapping))
    expect_equal(nrow(data.table::fread(mapping)), 8784)
    reused <- reference_test__run(
        directory,
        year = 2000,
        sites = site,
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

era_spatial_test__run <- function(
    directory,
    data,
    site = era_spatial_test__site(),
    method = "bilinear"
) {
    reference_test__run(
        directory,
        sites = site,
        data = data,
        extraction_method = method
    )
}

era_spatial_test__expect_tdb <- function(res, expected) {
    expect_identical(res$status, "complete")
    expect_equal(
        EpwFile$new(res$epw)$data()$dry_bulb_temperature,
        rep(expected, 8760)
    )
}

test_that("local ERA5 spatial methods produce traceable annual weather and distinct caches", {
    path <- withr::local_tempfile(fileext = ".nc")
    directory <- withr::local_tempdir()
    era_spatial_test__file(path)
    site <- era_spatial_test__site()
    results <- lapply(c("nearest", "bilinear", "mean"), function(method) {
        result <- era_spatial_test__run(directory, path, site, method)
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
    # Averaging wind speed first would retain sqrt(13), rather than 3 m/s.
    expect_equal(EpwFile$new(results[[2L]]$epw)$data()$wind_speed, rep(3, 8760))
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
    expect_length(unique(vapply(results, `[[`, character(1L), "receipt")), 3)
    again <- era_spatial_test__run(directory, path, site, "bilinear")
    expect_true(again$reused)
    expect_identical(again$epw, results[[2L]]$epw)
    idw <- era_spatial_test__run(
        directory,
        path,
        era_spatial_test__site(-0.125, 0),
        "idw"
    )
    era_spatial_test__expect_tdb(idw, 10)
    idw_receipt <- jsonlite::read_json(idw$receipt, simplifyVector = TRUE)
    expect_equal(idw_receipt$provenance$spatial$sources$weight, c(1, 0, 0, 0))
})

test_that("ERA5 interpolation respects descending axes, dateline and regional edges", {
    path <- withr::local_tempfile(fileext = ".nc")
    directory <- withr::local_tempdir()
    era_spatial_test__file(
        path,
        reverse_lat = TRUE,
        longitude = c(179.875, -179.875)
    )
    era_spatial_test__expect_tdb(
        era_spatial_test__run(directory, path, era_spatial_test__site(180)),
        14
    )
    # An exact eastern endpoint remains a valid cell boundary.
    era_spatial_test__expect_tdb(
        era_spatial_test__run(
            directory,
            path,
            era_spatial_test__site(-179.875, 0.25)
        ),
        22
    )
    expect_warning(
        outside <- era_spatial_test__run(
            directory,
            path,
            era_spatial_test__site(179.5)
        ),
        "failed"
    )
    expect_identical(outside$status, "failed")
    expect_match(outside$error, "outside")
})

test_that("ERA5 interpolation cannot silently drop missing sources or fetch point data", {
    path <- withr::local_tempfile(fileext = ".nc")
    directory <- withr::local_tempdir()
    era_spatial_test__file(path, missing_corner = TRUE)
    expect_warning(
        result <- era_spatial_test__run(
            directory,
            path,
            era_spatial_test__site(-0.125, 0),
            "idw"
        ),
        "failed"
    )
    expect_identical(result$status, "failed")
    expect_match(result$error, "finite")
    expect_error(
        era_spatial_test__run(withr::local_tempdir(), NULL),
        "local NetCDF"
    )
    expect_error(
        era_spatial_test__run(
            withr::local_tempdir(),
            list(data = data.frame(x = 1)),
            method = "mean"
        ),
        "local NetCDF"
    )
})

# vim: fdm=marker :
