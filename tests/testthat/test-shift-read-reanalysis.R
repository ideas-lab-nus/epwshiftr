test_that("skin temperature has an explicit ERA5 point-service request", {
    plan <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = "skt",
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.25, lon = 104)
    )
    expect_identical(plan$requests$request[[1L]]$variable, "skin_temperature")
    expect_equal(plan$requests$expected_hours, 8832L)
})

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

test_that("local skin temperature retains UTC, original values and file identity", {
    plan <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = "skt",
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.26, lon = 104.01)
    )
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(path))
    test_read__file(path, units = "degC", values = rep(6.85, 8832))
    result <- shift_read_reanalysis(
        plan,
        setNames(path, plan$requests$request_id)
    )
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
    plan <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = c("skt", "tas"),
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.375, lon = 104)
    )
    paths <- setNames(
        vapply(1:2, function(i) tempfile(fileext = ".nc"), character(1)),
        plan$requests$request_id
    )
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
            latitude = c(1.25, 1.5)[[i]]
        )
    }
    expect_error(shift_read_reanalysis(plan, paths), "different actual grid")
    variable <- if (plan$requests$variable_id[[1L]] == "tas") "t2m" else "skt"
    test_read__file(paths[[1L]], variable = variable, latitude = 20)
    expect_error(shift_read_reanalysis(plan, paths), "nearest")
    test_read__file(paths[[1L]], variable = variable, coordinate = FALSE)
    expect_error(shift_read_reanalysis(plan, paths), "actual scalar point")
})

test_that("radiation retains its end time and one-hour accumulation interval", {
    plan <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = "rsds",
            frequency = "hour",
            access = "arco"
        ),
        list(
            shift_site("a", lat = 1.25, lon = 104),
            shift_site("b", lat = 1.25, lon = 104)
        )
    )
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(path))
    test_read__file(
        path,
        variable = "ssrd",
        units = "J m**-2",
        values = rep(360000, 8832)
    )
    data <- shift_read_reanalysis(
        plan,
        setNames(path, plan$requests$request_id)
    )$data
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
        path,
        variable = "ssrd",
        units = "W m-2",
        values = rep(100, 8832)
    )
    flux <- shift_read_reanalysis(
        plan,
        setNames(path, plan$requests$request_id)
    )$data
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
    plan <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = variables,
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.25, lon = 104)
    )
    paths <- setNames(
        vapply(1:5, function(i) tempfile(fileext = ".nc"), character(1)),
        plan$requests$request_id
    )
    withr::defer(unlink(paths))
    for (i in 1:5) {
        j <- match(plan$requests$variable_id[[i]], variables)
        test_read__file(
            paths[[i]],
            variable = aliases[[j]],
            units = units[[j]],
            values = rep(raw[[j]], 8832)
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
    plan <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = "skt",
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.25, lon = 104)
    )
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(path))
    files <- setNames(path, plan$requests$request_id)
    cases <- list(
        list(time = (-24:8807)[-25], values = rep(280, 8831)),
        list(time = c(-24, -24:8806)),
        list(time = rev(-24:8807)),
        list(time = (-24:8807) + 0.5),
        list(time = -24:8808, values = rep(280, 8833))
    )
    for (case in cases) {
        do.call(test_read__file, c(list(path = path), case))
        expect_error(shift_read_reanalysis(plan, files), "hourly UTC coverage")
    }
    test_read__file(path, calendar = "360_day")
    expect_error(shift_read_reanalysis(plan, files), "Gregorian")
    test_read__file(path, units = "mystery")
    expect_error(
        shift_read_reanalysis(plan, files),
        "Unknown temperature units"
    )
    for (bad in c(NA_real_, Inf, -Inf)) {
        test_read__file(path, values = c(bad, rep(280, 8831)))
        expect_error(shift_read_reanalysis(plan, files), "finite")
    }
    test_read__file(path, values = rep(-1, 8832))
    expect_error(shift_read_reanalysis(plan, files), "physical")
    test_read__file(
        path,
        values = c(rep(280, 8832), rep(281, 8832)),
        versions = c(1, 5)
    )
    expect_error(shift_read_reanalysis(plan, files), "version|expver")
    test_read__file(path, values = rep(280, 8832), versions = 999)
    expect_error(shift_read_reanalysis(plan, files), "version|expver")
    expect_error(shift_read_reanalysis(plan, unname(files)), "request ID")
    expect_error(shift_read_reanalysis(plan, c(files, files)), "request ID")
    broken <- plan
    broken$requests <- data.table::copy(plan$requests)
    data.table::set(broken$requests, j = "expected_hours", value = 1L)
    expect_error(shift_read_reanalysis(broken, files), "unchanged")
})

test_that("non-leap coverage and longitude aliases preserve actual file identity", {
    plan <- shift_plan_reanalysis(
        shift_era5(
            2001,
            variables = "skt",
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.25, lon = -180)
    )
    path <- tempfile(fileext = ".nc")
    withr::defer(unlink(path))
    test_read__file(
        path,
        time = 8760:17567,
        values = rep(280, 8808),
        longitude = 180,
        versions = 1
    )
    files <- setNames(path, plan$requests$request_id)
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
    path <- test_read__file(file.path(directory, "weather.nc"))
    archive <- file.path(directory, "response.asset")
    withr::local_dir(directory)
    utils::zip(archive, "weather.nc", flags = "-q")
    source <- shift_era5(
        2000,
        variables = "skt",
        frequency = "hour",
        access = "arco"
    )
    site <- shift_site("point", lat = 1.25, lon = 104)
    plan <- shift_plan_reanalysis(source, site)
    files <- setNames(archive, plan$requests$request_id)
    read <- shift_read_reanalysis(plan, files)
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
    source <- shift_era5(
        2000,
        variables = "skt",
        frequency = "hour",
        access = "arco"
    )
    plan <- shift_plan_reanalysis(
        source,
        shift_site("point", lat = 1.25, lon = 104)
    )
    # Wrap independently invalid NetCDF inputs; only the transport container
    # changes, so original coordinate, temporal and unit rules must still apply.
    cases <- list(
        list(latitude = 20),
        list(time = -24:8806, values = rep(280, 8831)),
        list(units = "unknown")
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
            shift_read_reanalysis(
                plan,
                setNames(archive, plan$requests$request_id)
            ),
            messages[[i]]
        )
    }
    test_read__file("first.nc")
    test_read__file("second.nc")
    multiple <- file.path(directory, "multiple.asset")
    utils::zip(multiple, c("first.nc", "second.nc"), flags = "-q")
    expect_error(
        shift_read_reanalysis(
            plan,
            setNames(multiple, plan$requests$request_id)
        ),
        "one top-level NetCDF"
    )
    dir.create("nested")
    test_read__file("nested/weather.nc")
    nested <- file.path(directory, "nested.asset")
    utils::zip(nested, "nested/weather.nc", flags = "-q")
    expect_error(
        shift_read_reanalysis(plan, setNames(nested, plan$requests$request_id)),
        "one top-level NetCDF"
    )
})

test_that("saved point plans retain exact contracts through local reading", {
    directory <- tempfile("saved-point-plan-")
    dir.create(directory)
    withr::defer(unlink(directory, recursive = TRUE))
    plan <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = "skt",
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.25, lon = 104)
    )
    path <- test_read__file(file.path(directory, "weather.nc"))
    saved <- file.path(directory, "plan.rds")
    saveRDS(plan, saved)
    restored <- readRDS(saved)
    before <- serialize(restored, NULL)
    files <- setNames(path, plan$requests$request_id)
    result <- shift_read_reanalysis(restored, files)
    expect_identical(result$data$value, rep(280, 8832))
    expect_identical(serialize(restored, NULL), before)
    # Ignore only process-local pointers. Even minute changes to request data
    # and changes to types or dimensions must still fail the exact contract.
    for (column in c("latitude", "expected_hours")) {
        changed <- readRDS(saved)
        changed$requests[[column]][[1L]] <- changed$requests[[column]][[1L]] +
            1e-10
        expect_error(shift_read_reanalysis(changed, files), "unchanged")
    }
    changed <- readRDS(saved)
    changed$requests$expected_hours <- as.double(
        changed$requests$expected_hours
    )
    expect_error(shift_read_reanalysis(changed, files), "unchanged")
})

# ARCO fields retain the same native interval and unit contract as CDS; this
# checks actual NetCDF reading rather than only the availability flags.
test_that("ARCO cloud and direct radiation keep their source semantics", {
    plan <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = c("rsdsdiff", "clt"),
            frequency = "hour",
            access = "arco"
        ),
        shift_site("point", lat = 1.25, lon = 104),
        dates = as.Date("2000-01-02")
    )
    expect_setequal(plan$requests$variable_id, c("clt", "fdir", "rsds"))
    expect_true(all(
        plan$requests$dataset == "reanalysis-era5-single-levels-timeseries"
    ))
    files <- setNames(
        vapply(
            seq_len(nrow(plan$requests)),
            function(i) tempfile(fileext = ".nc"),
            character(1)
        ),
        plan$requests$request_id
    )
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
        selected <- as.Date(paste(req$year, req$month, req$day, sep = "-"))
        expect_true(all(selected %in% dates))
    }
    reordered <- shift_plan_reanalysis(
        source,
        site,
        dates = c(rev(dates), dates[1])
    )
    expect_true(reanalysis_plan__identical(plan, reordered))
    integer_dates <- shift_plan_reanalysis(
        source,
        site,
        dates = data.table::as.IDate(dates)
    )
    expect_true(reanalysis_plan__identical(plan, integer_dates))
    from_iso <- shift_plan_reanalysis(source, site, dates = plan$source$dates)
    expect_true(reanalysis_plan__identical(plan, from_iso))
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
        NA_character_,
        "2000-02-30",
        "2000-01-02junk",
        "1998-01-01",
        1,
        as.Date(0.5, origin = "2000-01-01")
    )) {
        expect_error(
            shift_plan_reanalysis(source, site, dates = bad),
            "[Dd]ates|ISO"
        )
    }
    arco <- shift_plan_reanalysis(
        shift_era5(
            2000,
            variables = "tas",
            frequency = "hour",
            access = "arco"
        ),
        site,
        dates
    )
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
    plan <- shift_reanalysis_data(source, site, dates = dates, dry_run = TRUE)
    files <- setNames(
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
    first <- shift_reanalysis_data(
        source,
        site,
        directory,
        data = files,
        dates = dates
    )
    expect_identical(first$status, "complete")
    second <- shift_reanalysis_data(
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
    cloud <- files[plan$requests$variable_id == "clt"]
    cds_data_test__file(cloud, "tcc", "%", 0.4)
    expect_equal(
        unique(shift_read_reanalysis(plan, files)$data[
            variable_id == "clt",
            value
        ]),
        0.4
    )
    cds_data_test__file(cloud, "tcc", "(0 - 1)", 1.01)
    expect_error(shift_read_reanalysis(plan, files), "physical bounds")
    cds_data_test__file(cloud, "tcc", "unknown", 0.4)
    expect_error(shift_read_reanalysis(plan, files), "cloud-cover units")
    cds_data_test__file(cloud, "tcc", "0-1", 0.4)
    expect_equal(
        unique(shift_read_reanalysis(plan, files)$data[
            variable_id == "clt",
            value
        ]),
        40
    )
    cds_data_test__file(cloud, "tcc", "1", 0.4, hours = c(24:46, 48))
    expect_error(shift_read_reanalysis(plan, files), "coverage|hourly|axis")
})

# vim: fdm=marker :
