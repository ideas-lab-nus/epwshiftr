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
    result <- shift_reanalysis_data(
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
    first <- shift_reanalysis_data(source, site, directory, data = files)
    second <- shift_reanalysis_data(source, site, directory, data = files)
    expect_identical(second$data, first$data)
    expect_true(second$execution$normalized_reused)
    expect_equal(second$execution$submissions, 0)
    expect_error(
        shift_reanalysis_data(
            source,
            site,
            directory,
            data = files,
            resume = FALSE
        ),
        "already exists"
    )
    expect_error(
        shift_reanalysis_data(
            source,
            shift_site("alias", lat = 1.25, lon = 104),
            directory,
            data = files
        ),
        "different plan"
    )
    writeBin(as.raw(1:10), first$data$path)
    bad <- shift_reanalysis_data(source, site, directory, data = files)
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
    result <- shift_reanalysis_data(
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
    again <- shift_reanalysis_data(source, sites, directory)
    expect_identical(again$data, result$data)
    expect_true(again$execution$normalized_reused)
    expect_equal(again$execution$submissions, 0)
    expect_equal(again$execution$bytes_received, 0)
})

test_that("a bounded partial run preserves verified assets and resumes within cumulative limits", {
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
    partial <- shift_reanalysis_data(
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
    limited <- shift_reanalysis_data(source, sites, directory)
    expect_identical(limited$status, "partial")
    expect_equal(limited$execution$submissions, 0)
    result <- shift_reanalysis_data(
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
        shift_reanalysis_data(source, sites, directory),
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
    result <- shift_reanalysis_data(source, site, directory, data = files)
    expect_identical(result$status, "partial")
    expect_null(result$data)
    expect_equal(nrow(result$artifacts), 2L)
    expect_equal(sum(result$requests$status == "validation_failed"), 1)
    expect_true(all(file.exists(result$artifacts$path)))
    expect_identical(
        result$diagnostics[[1L]]$request_id,
        plan$requests$request_id[plan$requests$variable_id == "skt"]
    )
    again <- shift_reanalysis_data(source, site, directory, data = files)
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
        shift_reanalysis_data(source, site, dry_run = TRUE),
        shift_plan_reanalysis(source, site)
    )
    shift_reanalysis_data(source, site, directory, dry_run = TRUE)
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
    result <- shift_reanalysis_data(source, site, directory, data = files)
    lock <- file.path(directory, ".data-lock")
    dir.create(lock)
    writeLines('{"nonce":"other-owner"}', file.path(lock, "owner.json"))
    expect_error(
        shift_reanalysis_data(source, site, directory, data = files),
        "locked"
    )
    expect_true(dir.exists(lock))
    unlink(lock, recursive = TRUE)
    writeBin(as.raw(1:10), result$artifacts$path)
    bad <- shift_reanalysis_data(source, site, directory, data = files)
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
    first <- shift_reanalysis_data(source, site, directory, data = files)
    alias <- shift_reanalysis_data(
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
    bad <- shift_reanalysis_data(source, site, directory, data = files)
    expect_identical(bad$status, "failed")
    expect_null(bad$data)
    expect_identical(
        bad$diagnostics[[1L]]$code,
        "invalid_normalization_receipt"
    )
    writeBin(as.raw(1:10), file.path(directory, "normalization.rds"))
    corrupt <- shift_reanalysis_data(source, site, directory, data = files)
    expect_identical(corrupt$status, "failed")
    expect_null(corrupt$data)
    expect_identical(
        corrupt$diagnostics[[1L]]$code,
        "invalid_normalization_receipt"
    )
})

# vim: fdm=marker :
