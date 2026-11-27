read_test__plan <- function(
    variables = "skt",
    years = 2000,
    access = "arco",
    lat = 1.25,
    lon = 104,
    sites = shift_site("point", lat = lat, lon = lon),
    dates = NULL
) {
    shift_plan_reanalysis(
        shift_era5(
            years,
            variables = variables,
            frequency = "hour",
            access = access
        ),
        sites,
        dates = dates
    )
}

read_test__temp_files <- function(plan) {
    stats::setNames(
        vapply(
            seq_len(nrow(plan$requests)),
            function(i) tempfile(fileext = ".nc"),
            character(1L)
        ),
        plan$requests$request_id
    )
}

# Write actual CF NetCDF point files. Literal values and explicit hourly offsets
# are independent fixtures, not output copied from the reader under test.
test_read__file <- function(
    path,
    variable = "skt",
    units = "K",
    values = rep(280, 8832),
    time = -24:8807,
    latitude = 1.25,
    longitude = 104,
    calendar = "standard",
    versions = NULL,
    coordinate = TRUE
) {
    nc <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    RNetCDF::dim.def.nc(nc, "time", length(time))
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::att.put.nc(
        nc,
        "time",
        "units",
        "NC_CHAR",
        "hours since 2000-01-01 00:00:00"
    )
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", calendar)
    if (coordinate) {
        RNetCDF::var.def.nc(nc, "latitude", "NC_DOUBLE", NA)
        RNetCDF::var.def.nc(nc, "longitude", "NC_DOUBLE", NA)
        RNetCDF::att.put.nc(nc, "latitude", "units", "NC_CHAR", "degrees_north")
        RNetCDF::att.put.nc(nc, "longitude", "units", "NC_CHAR", "degrees_east")
    }
    dims <- "time"
    if (!is.null(versions)) {
        RNetCDF::dim.def.nc(nc, "expver", length(versions))
        RNetCDF::var.def.nc(nc, "expver", "NC_INT", "expver")
        dims <- c(dims, "expver")
    }
    RNetCDF::var.def.nc(nc, variable, "NC_DOUBLE", dims)
    RNetCDF::att.put.nc(nc, variable, "units", "NC_CHAR", units)
    RNetCDF::att.put.nc(nc, variable, "_FillValue", "NC_DOUBLE", -9999)
    RNetCDF::var.put.nc(nc, "time", time)
    if (coordinate) {
        RNetCDF::var.put.nc(nc, "latitude", latitude)
        RNetCDF::var.put.nc(nc, "longitude", longitude)
    }
    if (!is.null(versions)) {
        RNetCDF::var.put.nc(nc, "expver", versions)
    }
    RNetCDF::var.put.nc(
        nc,
        variable,
        values,
        count = c(length(time), if (!is.null(versions)) length(versions))
    )
    path
}

# Lazily create immutable valid NetCDF templates once per test file to avoid
# repeating identical full-file NetCDF writes across unrelated tests.
read_test__valid_nc <- local({
    cache <- new.env(parent = emptyenv())
    function(kind = c("year2000", "day2000")) {
        kind <- match.arg(kind)
        path <- cache[[kind]]
        if (is.null(path) || !file.exists(path)) {
            path <- tempfile(paste0("reanalysis-", kind, "-"), fileext = ".nc")
            if (identical(kind, "year2000")) {
                test_read__file(path)
            } else {
                test_read__file(path, values = rep(280, 24L), time = 24:47)
            }
            cache[[kind]] <- path
            withr::defer(unlink(path), envir = testthat::teardown_env())
        }
        path
    }
})

test_that("local skin temperature retains request contract, UTC, original values and file identity", {
    plan <- read_test__plan(lat = 1.26, lon = 104.01)
    expect_identical(plan$requests$request[[1L]]$variable, "skin_temperature")
    expect_equal(plan$requests$expected_hours, 8832L)

    files <- read_test__temp_files(plan)
    withr::defer(unlink(files))
    test_read__file(files[[1L]], units = "degC", values = rep(6.85, 8832))
    result <- shift_read_reanalysis(plan, files)
    expect_equal(nrow(result$data), 8832L)
    expect_equal(unique(result$data$value), 280)
    expect_identical(unique(result$data$units), "K")
    expect_identical(unique(result$data$raw_value), 6.85)
    expect_identical(unique(result$data$raw_units), "degC")
    expect_equal(unique(result$data$grid_lat), 1.25)
    expect_equal(unique(result$data$grid_lon), 104)
    expect_equal(
        sum(format(result$data$utc_time, "%m-%d", tz = "UTC") == "02-29"),
        24
    )
    expect_identical(attr(result$data$utc_time, "tzone"), "UTC")
    expect_true(all(is.na(result$data$interval_start)))
    expect_match(result$artifacts$raw_sha256, "^[a-f0-9]{64}$")
    expect_identical(result$status, "validated_local")
})

test_that("point validation rejects missing, distant and inconsistent grids", {
    plan <- read_test__plan(
        variables = c("skt", "tas"),
        lat = 1.375,
        dates = as.Date("2000-01-02")
    )
    paths <- read_test__temp_files(plan)
    withr::defer(unlink(paths))
    for (i in 1:2) {
        variable <- if (plan$requests$variable_id[[i]] == "tas") {
            "t2m"
        } else {
            "skt"
        }
        test_read__file(
            paths[[i]],
            variable = variable,
            values = rep(280, 24L),
            time = 24:47,
            latitude = c(1.25, 1.5)[[i]]
        )
    }
    expect_error(shift_read_reanalysis(plan, paths), "different actual grid")
    variable <- if (plan$requests$variable_id[[1L]] == "tas") "t2m" else "skt"
    test_read__file(
        paths[[1L]],
        variable = variable,
        values = rep(280, 24L),
        time = 24:47,
        latitude = 20
    )
    expect_error(shift_read_reanalysis(plan, paths), "nearest")
    test_read__file(
        paths[[1L]],
        variable = variable,
        values = rep(280, 24L),
        time = 24:47,
        coordinate = FALSE
    )
    expect_error(shift_read_reanalysis(plan, paths), "actual scalar point")
})

test_that("radiation retains its end time and one-hour accumulation interval", {
    plan <- read_test__plan(
        variables = "rsds",
        sites = list(
            shift_site("a", lat = 1.25, lon = 104),
            shift_site("b", lat = 1.25, lon = 104)
        )
    )
    files <- read_test__temp_files(plan)
    withr::defer(unlink(files))
    test_read__file(
        files[[1L]],
        variable = "ssrd",
        units = "J m**-2",
        values = rep(360000, 8832)
    )
    data <- shift_read_reanalysis(plan, files)$data
    expect_equal(unique(data$value), 100)
    expect_identical(unique(data$units), "W m-2")
    expect_equal(unique(data$raw_value), 360000)
    expect_equal(nrow(data), 17664)
    expect_identical(unique(data$raw_temporal_kind), "accumulation")
    expect_equal(
        data$utc_time[[1]],
        as.POSIXct("1999-12-31 00:00:00", tz = "UTC")
    )
    expect_equal(
        data$interval_start[[1]],
        as.POSIXct("1999-12-30 23:00:00", tz = "UTC")
    )
    expect_identical(data$utc_time, data$interval_end)

    test_read__file(
        files[[1L]],
        variable = "ssrd",
        units = "W m-2",
        values = rep(100, 8832)
    )
    flux <- shift_read_reanalysis(plan, files)$data
    expect_identical(flux$value, data$value)
    expect_identical(flux$utc_time, data$utc_time)
    expect_identical(unique(flux$raw_temporal_kind), "interval_mean")
})

test_that("other raw dependencies use declared SI units", {
    variables <- c("ps", "psl", "uas", "vas", "pr")
    aliases <- c("sp", "msl", "u10", "v10", "tp")
    units <- c("hPa", "Pa", "m s**-1", "m/s", "m")
    raw <- c(1000, 100100, -3, 4, 0.0036)
    expected <- c(100000, 100100, -3, 4, 0.001)
    plan <- read_test__plan(
        variables = variables,
        dates = as.Date("2000-01-02")
    )
    paths <- read_test__temp_files(plan)
    withr::defer(unlink(paths))
    for (i in 1:5) {
        j <- match(plan$requests$variable_id[[i]], variables)
        test_read__file(
            paths[[i]],
            variable = aliases[[j]],
            units = units[[j]],
            values = rep(raw[[j]], 24L),
            time = 24:47
        )
    }
    data <- shift_read_reanalysis(plan, paths)$data
    for (j in 1:5) {
        expect_equal(
            unique(data$value[data$variable_id == variables[[j]]]),
            expected[[j]]
        )
    }
    expect_identical(unique(data$units[data$variable_id == "pr"]), "kg m-2 s-1")
    expect_identical(
        unique(data$raw_temporal_kind[data$variable_id == "pr"]),
        "accumulation"
    )
})

test_that("strict local reading never repairs bad time axes, units or values", {
    plan <- read_test__plan(dates = as.Date("2000-01-02"))
    files <- read_test__temp_files(plan)
    path <- files[[1L]]
    withr::defer(unlink(path))
    expect_err <- function(p, f, pat) {
        expect_error(shift_read_reanalysis(p, f), pat)
    }
    cases <- list(
        list(time = (24:47)[-12], values = rep(280, 23L)),
        list(time = c(24, 24:46), values = rep(280, 24L)),
        list(time = rev(24:47), values = rep(280, 24L)),
        list(time = (24:47) + 0.5, values = rep(280, 24L)),
        list(time = 24:48, values = rep(280, 25L))
    )
    for (case in cases) {
        do.call(test_read__file, c(list(path = path), case))
        expect_err(plan, files, "hourly UTC coverage")
    }
    bad_specs <- list(
        list(
            args = list(
                time = 24:47,
                values = rep(280, 24L),
                calendar = "360_day"
            ),
            pat = "Gregorian"
        ),
        list(
            args = list(
                time = 24:47,
                values = rep(280, 24L),
                units = "mystery"
            ),
            pat = "Unknown temperature units"
        ),
        list(
            args = list(time = 24:47, values = c(NA_real_, rep(280, 23L))),
            pat = "finite"
        ),
        list(
            args = list(time = 24:47, values = c(Inf, rep(280, 23L))),
            pat = "finite"
        ),
        list(
            args = list(time = 24:47, values = c(-Inf, rep(280, 23L))),
            pat = "finite"
        ),
        list(
            args = list(time = 24:47, values = rep(-1, 24L)),
            pat = "physical"
        ),
        list(
            args = list(
                time = 24:47,
                values = c(rep(280, 24L), rep(281, 24L)),
                versions = c(1, 5)
            ),
            pat = "version|expver"
        ),
        list(
            args = list(time = 24:47, values = rep(280, 24L), versions = 999),
            pat = "version|expver"
        )
    )
    for (spec in bad_specs) {
        do.call(test_read__file, c(list(path = path), spec$args))
        expect_err(plan, files, spec$pat)
    }
    expect_err(plan, unname(files), "request ID")
    expect_err(plan, c(files, files), "request ID")
    broken <- plan
    broken$requests <- data.table::copy(plan$requests)
    data.table::set(broken$requests, j = "expected_hours", value = 1L)
    expect_err(broken, files, "unchanged")
})

test_that("non-leap coverage and longitude aliases preserve actual file identity", {
    plan <- read_test__plan(years = 2001, lon = -180)
    files <- read_test__temp_files(plan)
    path <- files[[1L]]
    withr::defer(unlink(path))
    test_read__file(
        path,
        time = 8760:17567,
        values = rep(280, 8808),
        longitude = 180,
        versions = 1
    )
    result <- shift_read_reanalysis(plan, files)
    expect_equal(nrow(result$data), 8808)
    expect_equal(unique(result$data$grid_lon), -180)
    expect_identical(result$artifacts$source_expver, "1")
    expect_false(any(
        format(result$data$utc_time, "%m-%d", tz = "UTC") == "02-29"
    ))
    nc <- RNetCDF::open.nc(path, write = TRUE)
    RNetCDF::att.put.nc(nc, "skt", "scale_factor", "NC_DOUBLE", 0.1)
    RNetCDF::att.put.nc(nc, "skt", "add_offset", "NC_DOUBLE", 252)
    RNetCDF::close.nc(nc)
    packed <- shift_read_reanalysis(plan, files)
    expect_equal(unique(packed$data$raw_value), 280)
    expect_false(identical(
        packed$artifacts$raw_sha256,
        result$artifacts$raw_sha256
    ))
    nc <- RNetCDF::open.nc(path, write = TRUE)
    RNetCDF::att.put.nc(nc, "longitude", "units", "NC_CHAR", "radians")
    RNetCDF::close.nc(nc)
    expect_error(shift_read_reanalysis(plan, files), "coordinate units")
})

test_that("point archives retain container and member identity through public execution", {
    skip_if_not(nzchar(Sys.which("zip")), "The zip utility is unavailable.")
    directory <- tempfile("point-archive-")
    dir.create(directory)
    withr::defer(unlink(directory, recursive = TRUE))
    path <- file.path(directory, "weather.nc")
    expect_true(file.copy(read_test__valid_nc("year2000"), path))
    archive <- file.path(directory, "response.asset")
    withr::local_dir(directory)
    utils::zip(archive, "weather.nc", flags = "-q")
    plan <- read_test__plan()
    read <- shift_read_reanalysis(
        plan,
        stats::setNames(archive, plan$requests$request_id)
    )
    expect_equal(nrow(read$data), 8832L)
    expect_identical(unique(read$data$value), 280)
    expect_identical(
        unique(read$data$raw_sha256),
        unname(tools::sha256sum(archive))
    )
    expect_identical(read$artifacts$netcdf_member, "weather.nc")
    expect_identical(
        read$artifacts$netcdf_sha256,
        unname(tools::sha256sum(path))
    )
})

test_that("archive packaging never relaxes field or member validation", {
    skip_if_not(nzchar(Sys.which("zip")), "The zip utility is unavailable.")
    directory <- tempfile("point-archive-invalid-")
    dir.create(directory)
    withr::defer(unlink(directory, recursive = TRUE))
    withr::local_dir(directory)
    plan <- read_test__plan(dates = as.Date("2000-01-02"))
    req_id <- plan$requests$request_id
    # Wrap independently invalid NetCDF inputs; only the transport container
    # changes, so original coordinate, temporal and unit rules must still apply.
    cases <- list(
        list(time = 24:47, values = rep(280, 24L), latitude = 20),
        list(time = 24:46, values = rep(280, 23L)),
        list(time = 24:47, values = rep(280, 24L), units = "unknown")
    )
    messages <- c(
        "nearest ERA5 grid",
        "complete, ordered, unique",
        "Unknown temperature units"
    )
    for (i in seq_along(cases)) {
        do.call(test_read__file, c(list(path = "weather.nc"), cases[[i]]))
        archive <- file.path(directory, paste0("invalid-", i, ".asset"))
        utils::zip(archive, "weather.nc", flags = "-q")
        expect_error(
            shift_read_reanalysis(plan, stats::setNames(archive, req_id)),
            messages[[i]]
        )
    }
    valid_day <- read_test__valid_nc("day2000")
    expect_true(file.copy(valid_day, "first.nc"))
    expect_true(file.copy(valid_day, "second.nc"))
    multiple <- file.path(directory, "multiple.asset")
    utils::zip(multiple, c("first.nc", "second.nc"), flags = "-q")
    expect_error(
        shift_read_reanalysis(plan, stats::setNames(multiple, req_id)),
        "one top-level NetCDF"
    )
    dir.create("nested")
    expect_true(file.copy(valid_day, "nested/weather.nc"))
    nested <- file.path(directory, "nested.asset")
    utils::zip(nested, "nested/weather.nc", flags = "-q")
    expect_error(
        shift_read_reanalysis(plan, stats::setNames(nested, req_id)),
        "one top-level NetCDF"
    )
})

test_that("saved point plans retain exact contracts through local reading", {
    directory <- tempfile("saved-point-plan-")
    dir.create(directory)
    withr::defer(unlink(directory, recursive = TRUE))
    plan <- read_test__plan()
    path <- read_test__valid_nc("year2000")
    saved <- file.path(directory, "plan.rds")
    saveRDS(plan, saved)
    restored <- readRDS(saved)
    before <- serialize(restored, NULL)
    files <- stats::setNames(path, plan$requests$request_id)
    result <- shift_read_reanalysis(restored, files)
    expect_identical(result$data$value, rep(280, 8832))
    expect_identical(serialize(restored, NULL), before)
    # Ignore only process-local pointers. Even minute changes to request data
    # and changes to types or dimensions must still fail the exact contract.
    changed <- readRDS(saved)
    changed$requests$expected_hours[[1L]] <- changed$requests$expected_hours[[
        1L
    ]] +
        1e-10
    expect_error(shift_read_reanalysis(changed, files), "unchanged")
    changed <- readRDS(saved)
    changed$requests$expected_hours <- as.double(
        changed$requests$expected_hours
    )
    expect_error(shift_read_reanalysis(changed, files), "unchanged")
})

# ARCO fields retain the same native interval and unit contract as CDS; this
# checks actual NetCDF reading rather than only the availability flags.
test_that("ARCO cloud and direct radiation keep their source semantics", {
    plan <- read_test__plan(
        variables = c("rsdsdiff", "clt"),
        dates = as.Date("2000-01-02")
    )
    expect_setequal(plan$requests$variable_id, c("clt", "fdir", "rsds"))
    expect_true(all(
        plan$requests$dataset == "reanalysis-era5-single-levels-timeseries"
    ))
    files <- read_test__temp_files(plan)
    withr::defer(unlink(files))
    aliases <- c(clt = "tcc", fdir = "fdir", rsds = "ssrd")
    units <- c(clt = "1", fdir = "J m-2", rsds = "J m-2")
    values <- c(clt = 0.4, fdir = 180000, rsds = 360000)
    for (i in seq_along(files)) {
        field <- plan$requests$variable_id[i]
        test_read__file(
            files[i],
            variable = aliases[[field]],
            units = units[[field]],
            values = rep(values[[field]], 24L),
            time = 24:47
        )
    }
    read <- shift_read_reanalysis(plan, files)$data
    expect_equal(unique(read[variable_id == "clt", value]), 40)
    expect_equal(unique(read[variable_id == "fdir", value]), 50)
    expect_equal(unique(read[variable_id == "rsds", value]), 100)
    expect_true(all(is.na(read[variable_id == "clt", interval_start])))
    accumulated <- read[variable_id %in% c("fdir", "rsds")]
    expect_equal(
        as.numeric(difftime(
            accumulated$interval_end,
            accumulated$interval_start,
            units = "secs"
        )),
        rep(3600, 48L)
    )
})

# Native CDS-style singleton spatial dimensions and valid_time are literal
# fixtures. UTC hours are supplied by the test, independently of the planner.
cds_data_test__file <- function(path, variable, units, value, hours = 24:47) {
    nc <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(nc))
    for (axis in c("longitude", "latitude", "valid_time")) {
        RNetCDF::dim.def.nc(
            nc,
            axis,
            if (axis == "valid_time") length(hours) else 1L
        )
        RNetCDF::var.def.nc(nc, axis, "NC_DOUBLE", axis)
    }
    RNetCDF::att.put.nc(nc, "longitude", "units", "NC_CHAR", "degrees_east")
    RNetCDF::att.put.nc(nc, "latitude", "units", "NC_CHAR", "degrees_north")
    RNetCDF::att.put.nc(
        nc,
        "valid_time",
        "units",
        "NC_CHAR",
        "hours since 2000-01-01 00:00:00"
    )
    RNetCDF::var.put.nc(nc, "longitude", 104)
    RNetCDF::var.put.nc(nc, "latitude", 1.25)
    RNetCDF::var.put.nc(nc, "valid_time", hours)
    RNetCDF::var.def.nc(
        nc,
        variable,
        "NC_DOUBLE",
        c("longitude", "latitude", "valid_time")
    )
    RNetCDF::att.put.nc(nc, variable, "units", "NC_CHAR", units)
    RNetCDF::var.put.nc(
        nc,
        variable,
        array(value, dim = c(1L, 1L, length(hours)))
    )
    path
}

# Exact dates avoid year/month/day cross-products, including leap days and gaps.
test_that("CDS point plans preserve exact days and raw dependencies", {
    dates <- as.Date(c("1999-12-31", "2000-02-28", "2000-02-29", "2000-03-02"))
    source <- shift_era5(
        2000,
        variables = c("rsdsdiff", "clt"),
        frequency = "hour",
        access = "cds"
    )
    site <- shift_site("a", lat = 1.25, lon = 104)
    plan <- shift_plan_reanalysis(source, site, dates = dates)
    expect_equal(nrow(plan$requests), 9L)
    expect_equal(sum(plan$requests$expected_hours), 4 * 24 * 3)
    expect_setequal(plan$requests$variable_id, c("rsds", "fdir", "clt"))
    expect_identical(plan$source$dates, as.character(dates))
    expect_true(all(plan$requests$dataset == "reanalysis-era5-single-levels"))
    for (req in plan$requests$request) {
        expect_identical(req$area, c(1.25, 104, 1.25, 104))
        expect_length(req$year, 1L)
        expect_length(req$month, 1L)
        expect_length(req$variable, 1L)
        expect_length(req$time, 24L)
        expect_true(all(
            as.Date(paste(req$year, req$month, req$day, sep = "-")) %in% dates
        ))
    }
    expect_true(reanalysis_plan__identical(
        plan,
        shift_plan_reanalysis(source, site, dates = c(rev(dates), dates[1]))
    ))
    expect_true(reanalysis_plan__identical(
        plan,
        shift_plan_reanalysis(source, site, dates = data.table::as.IDate(dates))
    ))
    expect_true(reanalysis_plan__identical(
        plan,
        shift_plan_reanalysis(source, site, dates = plan$source$dates)
    ))
    expect_error(
        shift_plan_reanalysis(
            source,
            shift_site("b", lat = 1.3, lon = 104),
            dates
        ),
        "native 0.25"
    )
    for (bad in list(
        character(),
        "2000-02-30",
        as.Date(0.5, origin = "2000-01-01")
    )) {
        expect_error(
            shift_plan_reanalysis(source, site, dates = bad),
            "[Dd]ates|ISO"
        )
    }
    arco <- read_test__plan(variables = "tas", sites = site, dates = dates)
    expect_equal(sum(arco$requests$expected_hours), 96)
    expect_equal(nrow(arco$requests), 3L)
})

# The public local-acquisition seam exercises plan reconstruction, real NetCDF
# decoding, physical units, persistence, exact date identity and cache reuse.
test_that("CDS radiation and clouds pass through public local replay", {
    directory <- tempfile()
    raw <- tempfile()
    dir.create(raw)
    withr::defer(unlink(c(directory, raw), recursive = TRUE))
    source <- shift_era5(
        2000,
        variables = c("rsdsdiff", "clt"),
        frequency = "hour",
        access = "cds"
    )
    site <- shift_site("a", lat = 1.25, lon = 104)
    dates <- as.Date("2000-01-02")
    plan <- shift_data_reanalysis(source, site, dates = dates, dry_run = TRUE)
    files <- stats::setNames(
        file.path(raw, paste0(plan$requests$variable_id, ".nc")),
        plan$requests$request_id
    )
    aliases <- c(fdir = "fdir", rsds = "ssrd", clt = "tcc")
    units <- c(fdir = "J m**-2", rsds = "J m**-2", clt = "(0 - 1)")
    values <- c(fdir = 180000, rsds = 360000, clt = 0.4)
    for (i in seq_along(files)) {
        variable <- plan$requests$variable_id[i]
        cds_data_test__file(
            files[i],
            aliases[[variable]],
            units[[variable]],
            values[[variable]]
        )
    }
    restored <- tempfile()
    withr::defer(unlink(restored))
    saveRDS(plan, restored)
    direct <- shift_read_reanalysis(readRDS(restored), files)
    expect_equal(unique(direct$data[variable_id == "fdir", value]), 50)
    expect_equal(unique(direct$data[variable_id == "rsds", value]), 100)
    expect_equal(unique(direct$data[variable_id == "clt", value]), 40)
    expect_equal(nrow(direct$data), 72)
    expect_equal(
        as.numeric(
            direct$data[variable_id == "fdir", interval_end - interval_start],
            units = "secs"
        ),
        rep(3600, 24)
    )
    expect_true(all(is.na(direct$data[variable_id == "clt", interval_start])))
    first <- shift_data_reanalysis(
        source,
        site,
        directory,
        data = files,
        dates = dates
    )
    expect_identical(first$status, "complete")
    second <- shift_data_reanalysis(
        source,
        site,
        directory,
        data = files,
        dates = dates
    )
    expect_identical(second$data, first$data)
    expect_true(second$execution$normalized_reused)
    expect_equal(second$execution$submissions, 0)
    altered <- data.table::copy(plan)
    altered$source$dates <- "2000-01-03"
    expect_error(shift_read_reanalysis(altered, files), "unchanged")
    clt_plan <- shift_plan_reanalysis(
        shift_era5(2000, variables = "clt", frequency = "hour", access = "cds"),
        site,
        dates = dates
    )
    cloud <- unname(files[plan$requests$variable_id == "clt"])
    clt_files <- stats::setNames(cloud, clt_plan$requests$request_id)
    cds_data_test__file(cloud, "tcc", "%", 0.4)
    expect_equal(
        unique(shift_read_reanalysis(clt_plan, clt_files)$data[
            variable_id == "clt",
            value
        ]),
        0.4
    )
    cds_data_test__file(cloud, "tcc", "(0 - 1)", 1.01)
    expect_error(shift_read_reanalysis(clt_plan, clt_files), "physical bounds")
    cds_data_test__file(cloud, "tcc", "unknown", 0.4)
    expect_error(
        shift_read_reanalysis(clt_plan, clt_files),
        "cloud-cover units"
    )
    cds_data_test__file(cloud, "tcc", "0-1", 0.4)
    expect_equal(
        unique(shift_read_reanalysis(clt_plan, clt_files)$data[
            variable_id == "clt",
            value
        ]),
        40
    )
    cds_data_test__file(cloud, "tcc", "1", 0.4, hours = c(24:46, 48))
    expect_error(
        shift_read_reanalysis(clt_plan, clt_files),
        "coverage|hourly|axis"
    )
})

# Produce a real hourly CF file with literal values for the public disk seam.
reanalysis_data_test__file <- function(
    path,
    variable = "t2m",
    units = "K",
    value = 280,
    latitude = 1.25,
    longitude = 104
) {
    nc <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(nc))
    RNetCDF::dim.def.nc(nc, "time", 8808L)
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::att.put.nc(
        nc,
        "time",
        "units",
        "NC_CHAR",
        "hours since 2001-01-01 00:00:00"
    )
    for (axis in c("latitude", "longitude")) {
        RNetCDF::var.def.nc(nc, axis, "NC_DOUBLE", NA)
        RNetCDF::att.put.nc(
            nc,
            axis,
            "units",
            "NC_CHAR",
            if (axis == "latitude") "degrees_north" else "degrees_east"
        )
    }
    RNetCDF::var.def.nc(nc, variable, "NC_DOUBLE", "time")
    RNetCDF::att.put.nc(nc, variable, "units", "NC_CHAR", units)
    RNetCDF::var.put.nc(nc, "time", -24:8783)
    RNetCDF::var.put.nc(nc, "latitude", latitude)
    RNetCDF::var.put.nc(nc, "longitude", longitude)
    RNetCDF::var.put.nc(nc, variable, rep(value, 8808L))
    path
}

test_that("independent local execution persists a complete self-described table", {
    source <- shift_era5(
        2001,
        variables = "tas",
        frequency = "hour",
        access = "arco"
    )
    site <- shift_site("point", lat = 1.25, lon = 104)
    directory <- tempfile()
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(c(directory, path), recursive = TRUE))
    plan <- shift_plan_reanalysis(source, site)
    reanalysis_data_test__file(path)
    result <- shift_data_reanalysis(
        source,
        site,
        directory,
        data = setNames(path, plan$requests$request_id)
    )
    expect_identical(result$status, "complete")
    expect_identical(result$data$format, "rds")
    table <- readRDS(result$data$path)
    expect_s3_class(table, "data.table")
    expect_equal(nrow(table), 8808L)
    expect_equal(unique(table$value), 280)
    expect_equal(unique(table$raw_value), 280)
    expect_identical(attr(table$utc_time, "tzone"), "UTC")
    expect_identical(
        attr(table, "reanalysis_schema")$normalization_version,
        result$data$normalization_version
    )
    expect_identical(result$requests$status, "validated")
    expect_equal(result$execution$submissions, 0)
    expect_true(file.exists(result$artifacts$path))
    expect_match(result$data$sha256, "^[a-f0-9]{64}$")
})

test_that("reopening verifies raw and normalized identities before reuse", {
    source <- shift_era5(
        2001,
        variables = "tas",
        frequency = "hour",
        access = "arco"
    )
    site <- shift_site("point", lat = 1.25, lon = 104)
    directory <- tempfile()
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(c(directory, path), recursive = TRUE))
    files <- setNames(
        reanalysis_data_test__file(path),
        shift_plan_reanalysis(source, site)$requests$request_id
    )
    first <- shift_data_reanalysis(source, site, directory, data = files)
    second <- shift_data_reanalysis(source, site, directory, data = files)
    expect_identical(second$data, first$data)
    expect_true(second$execution$normalized_reused)
    expect_equal(second$execution$submissions, 0)
    expect_error(
        shift_data_reanalysis(
            source,
            site,
            directory,
            data = files,
            resume = FALSE
        ),
        "already exists"
    )
    expect_error(
        shift_data_reanalysis(
            source,
            shift_site("alias", lat = 1.25, lon = 104),
            directory,
            data = files
        ),
        "different plan"
    )
    writeBin(as.raw(1:10), first$data$path)
    bad <- shift_data_reanalysis(source, site, directory, data = files)
    expect_identical(bad$status, "failed")
    expect_null(bad$data)
    expect_identical(bad$diagnostics[[1L]]$code, "normalized_asset_changed")
    expect_null(readRDS(file.path(directory, "result.rds"))$data)
})

# Serve actual NetCDF bytes through an OGC-shaped external boundary. Submission
# order follows the public plan; every file still has independent CF coordinates.
reanalysis_data_test__server <- function(files) {
    app <- webfakes::new_app()
    app$locals$files <- unname(files)
    app$locals$posts <- 0L
    app$post(
        "/api/retrieve/v1/processes/:dataset/execution",
        function(req, res, locals) {
            locals$posts <- locals$posts + 1L
            res$send_json(
                list(
                    status = "accepted",
                    links = list(list(
                        rel = "monitor",
                        href = paste0("/api/retrieve/v1/jobs/", locals$posts)
                    ))
                ),
                auto_unbox = TRUE
            )
        }
    )
    app$get("/api/retrieve/v1/jobs/:job", function(req, res) {
        res$send_json(list(status = "successful"), auto_unbox = TRUE)
    })
    app$get("/api/retrieve/v1/jobs/:job/results", function(req, res) {
        res$send_json(
            list(
                asset = list(
                    value = list(
                        href = paste0("/asset/", req$params$job)
                    )
                )
            ),
            auto_unbox = TRUE
        )
    })
    app$get("/asset/:job", function(req, res, locals) {
        path <- locals$files[[as.integer(req$params$job)]]
        res$send(readBin(path, "raw", n = file.info(path)$size))
    })
    webfakes::new_app_process(app)
}
environment(reanalysis_data_test__server) <- baseenv()

test_that("six points and six fields complete through HTTP and then use zero submissions", {
    skip_if_not_installed("webfakes")
    source <- shift_era5(
        2001,
        variables = c("tas", "tdps", "ps", "uas", "vas", "rsds"),
        frequency = "hour",
        access = "arco"
    )
    lat <- c(1.25, 1.5, 25.25, 25.25, -34, -34)
    lon <- c(104, 104, 55.25, 55.5, 18.5, 18.75)
    sites <- lapply(seq_along(lat), function(i) {
        shift_site(paste0("point", i), lat = lat[[i]], lon = lon[[i]])
    })
    plan <- shift_plan_reanalysis(source, sites)
    fixture <- tempfile()
    directory <- tempfile()
    dir.create(fixture)
    withr::defer(unlink(c(fixture, directory), recursive = TRUE))
    paths <- file.path(fixture, paste0(plan$requests$request_id, ".nc"))
    variable <- c(
        tas = "t2m",
        tdps = "d2m",
        ps = "sp",
        uas = "u10",
        vas = "v10",
        rsds = "ssrd"
    )
    units <- c(
        tas = "K",
        tdps = "K",
        ps = "Pa",
        uas = "m s-1",
        vas = "m s-1",
        rsds = "J m**-2"
    )
    values <- c(
        tas = 280,
        tdps = 275,
        ps = 101325,
        uas = 2,
        vas = -1,
        rsds = 360000
    )
    for (i in seq_along(paths)) {
        v <- plan$requests$variable_id[[i]]
        reanalysis_data_test__file(
            paths[[i]],
            variable[[v]],
            units[[v]],
            values[[v]],
            plan$requests$latitude[[i]],
            plan$requests$longitude[[i]]
        )
    }
    server <- reanalysis_data_test__server(paths)
    withr::defer(server$stop())
    withr::local_envvar(c(
        ECMWF_DATASTORES_URL = paste0(sub("/$", "", server$url()), "/api"),
        ECMWF_DATASTORES_KEY = "local-fixture-token"
    ))
    result <- shift_data_reanalysis(
        source,
        sites,
        directory,
        limits = list(max_submissions = 36)
    )
    expect_identical(result$status, "complete")
    expect_equal(result$execution$submissions, 36)
    expect_equal(nrow(result$artifacts), 36L)
    table <- readRDS(result$data$path)
    expect_equal(nrow(table), 317088L)
    radiation <- table[table$variable_id == "rsds"]
    expect_equal(unique(radiation$value), 100)
    expect_equal(unique(radiation$raw_value), 360000)
    expect_equal(
        as.numeric(
            radiation$interval_end - radiation$interval_start,
            units = "secs"
        ),
        rep(3600, nrow(radiation))
    )
    expect_true(all(result$requests$status == "validated"))
    again <- shift_data_reanalysis(source, sites, directory)
    expect_identical(again$data, result$data)
    expect_true(again$execution$normalized_reused)
    expect_equal(again$execution$submissions, 0)
    expect_equal(again$execution$bytes_received, 0)
})

test_that("a bounded partial run preserves verified assets and resumes within cumulative limits", {
    skip_if_not_installed("webfakes")
    source <- shift_era5(
        2001,
        variables = "tas",
        frequency = "hour",
        access = "arco"
    )
    sites <- list(
        shift_site("a", lat = 1.25, lon = 104),
        shift_site("b", lat = 1.5, lon = 104)
    )
    plan <- shift_plan_reanalysis(source, sites)
    fixture <- tempfile()
    directory <- tempfile()
    dir.create(fixture)
    withr::defer(unlink(c(fixture, directory), recursive = TRUE))
    paths <- file.path(fixture, paste0(1:2, ".nc"))
    for (i in seq_along(paths)) {
        reanalysis_data_test__file(
            paths[[i]],
            latitude = plan$requests$latitude[[i]]
        )
    }
    server <- reanalysis_data_test__server(paths)
    withr::defer(server$stop())
    withr::local_envvar(c(
        ECMWF_DATASTORES_URL = paste0(sub("/$", "", server$url()), "/api"),
        ECMWF_DATASTORES_KEY = "local-fixture-token"
    ))
    partial <- shift_data_reanalysis(
        source,
        sites,
        directory,
        limits = list(max_submissions = 1)
    )
    expect_identical(partial$status, "partial")
    expect_null(partial$data)
    expect_equal(sum(partial$requests$status == "validated"), 1)
    expect_equal(nrow(partial$artifacts), 1L)
    expect_true(file.exists(partial$artifacts$receipt))
    expect_identical(readRDS(partial$artifacts$receipt)$state, "validated")
    limited <- shift_data_reanalysis(source, sites, directory)
    expect_identical(limited$status, "partial")
    expect_equal(limited$execution$submissions, 0)
    result <- shift_data_reanalysis(
        source,
        sites,
        directory,
        limits = list(max_submissions = 2)
    )
    expect_identical(result$status, "complete")
    expect_equal(result$execution$submissions, 1)
    expect_equal(result$execution$cumulative$submissions, 2)
    expect_equal(result$data$rows, 17616L)
    withr::local_envvar(c(ECMWF_DATASTORES_URL = "http://127.0.0.1:1/api"))
    expect_error(
        shift_data_reanalysis(source, sites, directory),
        "different plan or origin"
    )
})

test_that("semantically invalid raw assets remain locatable without a complete data descriptor", {
    source <- shift_era5(
        2001,
        variables = c("tas", "skt"),
        frequency = "hour",
        access = "arco"
    )
    site <- shift_site("point", lat = 1.25, lon = 104)
    plan <- shift_plan_reanalysis(source, site)
    fixture <- tempfile()
    directory <- tempfile()
    dir.create(fixture)
    withr::defer(unlink(c(fixture, directory), recursive = TRUE))
    files <- setNames(
        file.path(fixture, paste0(1:2, ".nc")),
        plan$requests$request_id
    )
    for (i in seq_along(files)) {
        bad <- plan$requests$variable_id[[i]] == "skt"
        reanalysis_data_test__file(
            files[[i]],
            if (bad) "skt" else "t2m",
            if (bad) "unknown_temperature_unit" else "K"
        )
    }
    result <- shift_data_reanalysis(source, site, directory, data = files)
    expect_identical(result$status, "partial")
    expect_null(result$data)
    expect_equal(nrow(result$artifacts), 2L)
    expect_equal(sum(result$requests$status == "validation_failed"), 1)
    expect_true(all(file.exists(result$artifacts$path)))
    expect_identical(
        result$diagnostics[[1L]]$request_id,
        plan$requests$request_id[plan$requests$variable_id == "skt"]
    )
    again <- shift_data_reanalysis(source, site, directory, data = files)
    expect_identical(again$status, "partial")
    expect_null(readRDS(file.path(directory, "result.rds"))$data)
})

test_that("dry planning needs no directory or credentials and writes nothing", {
    source <- shift_era5(
        2001,
        variables = "skt",
        frequency = "hour",
        access = "arco"
    )
    site <- shift_site("point", lat = 1.25, lon = 104)
    directory <- tempfile()
    withr::local_envvar(c(
        ECMWF_DATASTORES_URL = "http://invalid/secret?token=hidden",
        ECMWF_DATASTORES_KEY = ""
    ))
    expect_identical(
        shift_data_reanalysis(source, site, dry_run = TRUE),
        shift_plan_reanalysis(source, site)
    )
    shift_data_reanalysis(source, site, directory, dry_run = TRUE)
    expect_false(dir.exists(directory))
})

test_that("raw corruption and foreign locks cannot create a successful reopening", {
    source <- shift_era5(
        2001,
        variables = "tas",
        frequency = "hour",
        access = "arco"
    )
    site <- shift_site("point", lat = 1.25, lon = 104)
    directory <- tempfile()
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(c(directory, path), recursive = TRUE))
    files <- setNames(
        reanalysis_data_test__file(path),
        shift_plan_reanalysis(source, site)$requests$request_id
    )
    result <- shift_data_reanalysis(source, site, directory, data = files)
    lock <- file.path(directory, ".data-lock")
    dir.create(lock)
    writeLines('{"nonce":"other-owner"}', file.path(lock, "owner.json"))
    expect_error(
        shift_data_reanalysis(source, site, directory, data = files),
        "locked"
    )
    expect_true(dir.exists(lock))
    unlink(lock, recursive = TRUE)
    writeBin(as.raw(1:10), result$artifacts$path)
    bad <- shift_data_reanalysis(source, site, directory, data = files)
    expect_identical(bad$status, "failed")
    expect_null(bad$data)
    expect_identical(bad$diagnostics[[1L]]$code, "raw_asset_changed")
})

test_that("normalization descriptors are checked against the actual verified table", {
    source <- shift_era5(
        2001,
        variables = "tas",
        frequency = "hour",
        access = "arco"
    )
    site <- shift_site("point", lat = 1.25, lon = 104)
    directory <- tempfile()
    alternate <- tempfile()
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(c(directory, alternate, path), recursive = TRUE))
    files <- setNames(
        reanalysis_data_test__file(path),
        shift_plan_reanalysis(source, site)$requests$request_id
    )
    first <- shift_data_reanalysis(source, site, directory, data = files)
    alias <- shift_data_reanalysis(
        source,
        shift_site("alias", lat = 1.25, lon = 104),
        alternate,
        data = files
    )
    expect_identical(first$artifacts$raw_sha256, alias$artifacts$raw_sha256)
    expect_false(identical(
        first$data$normalization_id,
        alias$data$normalization_id
    ))
    expect_identical(unique(readRDS(alias$data$path)$site_id), "alias")
    descriptor <- readRDS(file.path(directory, "normalization.rds"))
    descriptor$rows <- 1L
    saveRDS(descriptor, file.path(directory, "normalization.rds"))
    bad <- shift_data_reanalysis(source, site, directory, data = files)
    expect_identical(bad$status, "failed")
    expect_null(bad$data)
    expect_identical(
        bad$diagnostics[[1L]]$code,
        "invalid_normalization_receipt"
    )
    writeBin(as.raw(1:10), file.path(directory, "normalization.rds"))
    corrupt <- shift_data_reanalysis(source, site, directory, data = files)
    expect_identical(corrupt$status, "failed")
    expect_null(corrupt$data)
    expect_identical(
        corrupt$diagnostics[[1L]]$code,
        "invalid_normalization_receipt"
    )
})

# vim: fdm=marker :
