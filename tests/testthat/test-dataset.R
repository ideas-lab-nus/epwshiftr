# q <- esg_query(INDEX_NODES[["ORNL"]])$
#      activity_id("ScenarioMIP")$
#      source_id("AWI-CM-1-1-MR")$
#      frequency("day")$
#      variable_id("tas")$
#      experiment_id("ssp585")$
#      variant_label("r1i1p1f1")$
#      fields(c("source_id", "experiment_id", "frequency"))$
#      limit(2)$
#      collect()
#
# f <- q$collect(which(q$has_opendap()))
# url_nc <- f$url_opendap[1]
# url_nc2 <- f$url_opendap[2]
# this url is obtained from the query above and is used in multiple tests, so we define it here for reuse
url_nc <- "http://esgf-node.ornl.gov/thredds/dodsC/css03_data/CMIP6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/tas/gn/v20190529/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20250101-20251231.nc"
url_nc2 <- "http://esgf-node.ornl.gov/thredds/dodsC/css03_data/CMIP6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/tas/gn/v20190529/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20300101-20301231.nc"

local_dataset_table_file <- function(time_vals, time_units, tas_vals = seq_along(time_vals), calendar = "standard") {
    path <- tempfile(fileext = ".nc")
    nc <- RNetCDF::create.nc(path)

    RNetCDF::dim.def.nc(nc, "time", length(time_vals))
    RNetCDF::dim.def.nc(nc, "lat", 1L)
    RNetCDF::dim.def.nc(nc, "lon", 1L)
    RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
    RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", "lat")
    RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", "lon")
    RNetCDF::var.def.nc(nc, "tas", "NC_DOUBLE", c("time", "lat", "lon"))

    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", time_units)
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", calendar)
    RNetCDF::var.put.nc(nc, "time", time_vals, count = length(time_vals))
    RNetCDF::var.put.nc(nc, "lat", 1, count = 1L)
    RNetCDF::var.put.nc(nc, "lon", 2, count = 1L)
    RNetCDF::var.put.nc(
        nc,
        "tas",
        array(tas_vals, dim = c(length(time_vals), 1L, 1L)),
        count = c(length(time_vals), 1L, 1L)
    )
    RNetCDF::close.nc(nc)

    path
}

mirai_dataset_symbols <- c(
    "EsgDataset",
    "DatasetAsyncTask",
    "dataset_async_error_condition",
    "dataset_async_result_error"
)

start_mirai_dataset_runtime <- function(workers) {
    testthat::skip_if_not_installed("mirai")

    workers <- as.integer(workers[[1L]])
    if (is.na(workers) || workers < 1L) {
        workers <- 1L
    }

    compute_profile <- sprintf(
        "test-dataset-%s-%s",
        Sys.getpid(),
        sprintf("%06d", sample.int(999999L, 1L))
    )
    started <- FALSE
    on.exit(
        {
            if (!started) {
                try(mirai::daemons(0, .compute = compute_profile), silent = TRUE)
            }
        },
        add = TRUE
    )

    startup_error <- NULL
    tryCatch(
        {
            mirai::daemons(workers, dispatcher = TRUE, .compute = compute_profile)
            started <- TRUE

            ready <- mirai::collect_mirai(mirai::mirai(TRUE, .compute = compute_profile))
            if (!isTRUE(ready)) {
                stop("mirai readiness probe returned a non-TRUE result.")
            }
        },
        error = function(err) {
            startup_error <<- err
        }
    )

    if (!is.null(startup_error)) {
        testthat::skip(sprintf(
            "Concurrent async test requires a working mirai runtime: %s",
            conditionMessage(startup_error)
        ))
    }

    list(compute_profile = compute_profile, workers = workers)
}

stop_mirai_dataset_runtime <- function(runtime) {
    if (is.null(runtime$compute_profile)) {
        return(invisible(NULL))
    }

    try(mirai::daemons(0, .compute = runtime$compute_profile), silent = TRUE)
    invisible(NULL)
}

mirai_dataset_lapply <- function(X, FUN, ..., workers = min(2L, length(X))) {
    if (!length(X)) {
        return(vector("list", 0L))
    }

    runtime <- start_mirai_dataset_runtime(workers)
    on.exit(stop_mirai_dataset_runtime(runtime), add = TRUE)

    worker_symbols <- mget(mirai_dataset_symbols, envir = asNamespace("epwshiftr"), inherits = FALSE)
    dot_args <- list(...)
    tasks <- lapply(X, function(x) {
        mirai::mirai(
            {
                list2env(worker_symbols, envir = .GlobalEnv)
                on.exit(rm(list = names(worker_symbols), envir = .GlobalEnv), add = TRUE)
                environment(FUN) <- list2env(worker_symbols, parent = environment(FUN))
                try(do.call(FUN, c(list(x), dot_args)), silent = TRUE)
            },
            FUN = FUN,
            x = x,
            dot_args = dot_args,
            worker_symbols = worker_symbols,
            .compute = runtime$compute_profile
        )
    })

    lapply(tasks, mirai::collect_mirai)
}

test_that("EsgDataset can work with a single file", {
    skip_on_cran()
    skip_if_offline()

    # Test single URL
    ds <- EsgDataset$new(url_nc)
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 1L)
    expect_false(ds$is_aggregated)
    expect_false(ds$is_open)

    # Test open
    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open OPeNDAP dataset:", e$message))
    })
    expect_true(ds$is_open)

    # Test close
    ds$close()
    expect_false(ds$is_open)

    # Test file_inq
    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open OPeNDAP dataset:", e$message))
    })
    on.exit(ds$close(), add = TRUE)
    info <- ds$file_inq()
    expect_type(info, "list")
    expect_true("nvars" %in% names(info))
    expect_true("ndims" %in% names(info))

    # Test var_inq
    var_info <- ds$var_inq("tas")
    expect_type(var_info, "list")
    expect_equal(var_info$name, "tas")

    # Test dim_inq
    dim_info <- ds$dim_inq("time")
    expect_type(dim_info, "list")
    expect_equal(dim_info$name, "time")

    # Test att_get
    units <- ds$att_get("tas", "units")
    expect_type(units, "character")

    # Test get_variables
    vars <- ds$get_variables()
    expect_type(vars, "character")
    expect_true(length(vars) > 0)
    expect_true("tas" %in% vars)

    # Test get_dimensions
    dims <- ds$get_dimensions()
    expect_type(dims, "character")
    expect_true("time" %in% dims)
    expect_true("lat" %in% dims)
    expect_true("lon" %in% dims)

    # Test get_time_axis
    time_info <- ds$get_time_axis()
    expect_type(time_info, "list")
    expect_true("values" %in% names(time_info))
    expect_true("units" %in% names(time_info))
    expect_true("calendar" %in% names(time_info))
    expect_s3_class(time_info$values, "POSIXct")

    # Test get_spatial_grid
    grid <- ds$get_spatial_grid()
    expect_type(grid, "list")
    expect_true("lat" %in% names(grid))
    expect_true("lon" %in% names(grid))

    # Test var_get with subset
    data <- ds$var_get("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = TRUE)
    expect_type(data, "double")
    expect_equal(dim(data), c(5, 5))
    data <- ds$var_get("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = FALSE)
    expect_type(data, "double")
    expect_equal(dim(data), c(1, 5, 5))

    # Test read_array
    arr_list <- ds$read_array("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = TRUE)
    expect_type(arr_list, "list")
    expect_length(arr_list, 1L)
    arr <- arr_list[[1L]]
    expect_type(arr, "double")
    expect_equal(dim(arr), c(5, 5))

    arr_list <- ds$read_array("tas", start = c(1, 1, 1), count = c(1, 5, 5), collapse = FALSE)
    expect_type(arr_list, "list")
    expect_length(arr_list, 1L)
    arr <- arr_list[[1L]]
    expect_type(arr, "double")
    expect_equal(dim(arr), c(1, 5, 5))

    # Test read_data_table
    start <- c(1, 1, 1)
    count <- c(1, 5, 5)
    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_type(dt_list, "list")
    expect_length(dt_list, 1L)
    dt <- dt_list[[1L]]
    expect_s3_class(dt, "data.table")
    expect_true("tas" %in% names(dt))
    expect_equal(nrow(dt), prod(count))

    vinfo <- ds$var_inq("tas")
    dim_names <- vapply(vinfo$dimids, function(id) ds$dim_inq(id)$name, character(1L))
    expect_true(all(dim_names %in% names(dt)))
    for (j in seq_along(dim_names)) {
        nm <- dim_names[[j]]
        if (identical(nm, "time")) {
            coord <- ds$get_time_axis()$values[seq.int(start[[j]], length.out = count[[j]])]
            expect_s3_class(dt[[nm]], "POSIXct")
            expect_identical(attr(dt[[nm]], "tzone"), "UTC")
            expect_equal(as.numeric(sort(unique(dt[[nm]]))), as.numeric(sort(coord)))
        } else {
            coord <- ds$var_get(nm, start = start[[j]], count = count[[j]], collapse = TRUE)
            expect_equal(sort(unique(dt[[nm]])), sort(as.vector(coord)))
        }
    }

    ds$close()
    expect_false(ds$is_open)
})

test_that("EsgDataset can work with multiple files", {
    skip_on_cran()
    skip_if_offline()

    # Test multiple URLs
    ds <- EsgDataset$new(c(url_nc, url_nc2))
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 2L)
    expect_true(ds$is_aggregated)
    expect_false(ds$is_open)

    # Test open
    tryCatch(ds$open(), error = function(e) {
        skip(paste("Cannot open OPeNDAP dataset:", e$message))
    })
    on.exit(ds$close(), add = TRUE)

    # Test get_time_axis
    time_info <- ds$get_time_axis(2)
    expect_type(time_info, "list")
    expect_true("values" %in% names(time_info))
    expect_true("units" %in% names(time_info))
    expect_true("calendar" %in% names(time_info))
    expect_s3_class(time_info$values, "POSIXct")

    # Test read_data_table
    start <- c(1, 1, 1)
    count <- c(1, 5, 5)

    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_type(dt_list, "list")
    expect_length(dt_list, 2L)
    expect_true(all(vapply(dt_list, function(x) inherits(x, "data.table"), logical(1L))))

    # Coordinate values should be interpreted within each file's context
    vinfo <- ds$var_inq("tas")
    dim_names <- vapply(vinfo$dimids, function(id) ds$dim_inq(id)$name, character(1L))
    for (i in 1:2) {
        dti <- dt_list[[i]]
        for (j in seq_along(dim_names)) {
            nm <- dim_names[[j]]
            if (identical(nm, "time")) {
                coord <- ds$get_time_axis(i)$values[seq.int(start[[j]], length.out = count[[j]])]
                expect_s3_class(dti[[nm]], "POSIXct")
                expect_identical(attr(dti[[nm]], "tzone"), "UTC")
                expect_equal(as.numeric(sort(unique(dti[[nm]]))), as.numeric(sort(coord)))
            } else {
                coord <- ds$var_get(nm, start = start[[j]], count = count[[j]], index = i, collapse = TRUE)
                expect_equal(sort(unique(dti[[nm]])), sort(as.vector(coord)))
            }
        }
    }

    # Optionally row-bind with file_index
    dt_all <- ds$read_data_table("tas", start = start, count = count, rbind = TRUE)
    expect_s3_class(dt_all, "data.table")
    expect_true("file_index" %in% names(dt_all))
    expect_equal(sort(unique(dt_all$file_index)), c(1L, 2L))
})

test_that("EsgDataset read_data_table() returns UTC POSIXct time for CMIP6-like CF units", {
    path1 <- local_dataset_table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 1850-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    path2 <- local_dataset_table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2010-01-01 00:00:00",
        tas_vals = c(21, 22, 23)
    )
    on.exit(unlink(c(path1, path2)), add = TRUE)

    ds <- EsgDataset$new(c(path1, path2))
    ds$open()
    on.exit(ds$close(), add = TRUE)

    start <- c(2, 1, 1)
    count <- c(2, 1, 1)
    expected <- list(
        as.POSIXct(c("1850-01-02 00:00:00", "1850-01-03 00:00:00"), tz = "UTC"),
        as.POSIXct(c("2010-01-02 00:00:00", "2010-01-03 00:00:00"), tz = "UTC")
    )

    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_length(dt_list, 2L)

    for (i in seq_along(dt_list)) {
        time <- dt_list[[i]]$time
        expect_true(inherits(time, "POSIXct"))
        expect_identical(attr(time, "tzone"), "UTC")
        expect_length(time, count[[1L]])
        expect_false(is.unsorted(as.numeric(time), strictly = TRUE))
        expect_equal(as.numeric(time), as.numeric(expected[[i]]))
    }

    dt_all <- ds$read_data_table("tas", start = start, count = count, rbind = TRUE)
    expect_true(inherits(dt_all$time, "POSIXct"))
    expect_identical(attr(dt_all$time, "tzone"), "UTC")
    expect_equal(as.numeric(dt_all[file_index == 1L, time]), as.numeric(expected[[1L]]))
    expect_equal(as.numeric(dt_all[file_index == 2L, time]), as.numeric(expected[[2L]]))
})

test_that("EsgDataset read_region() reads nearest grid cells and time windows", {
    path1 <- tempfile(fileext = ".nc")
    path2 <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path1, 2060L)
    write_local_cmip6_netcdf_fixture(path2, 2061L)
    on.exit(unlink(c(path1, path2)), add = TRUE)

    ds <- EsgDataset$new(c(path1, path2))
    ds$open()
    on.exit(ds$close(), add = TRUE)

    dt <- ds$read_region(
        variable = "tas",
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        nearest = 2L
    )
    expect_s3_class(dt, "data.table")
    expect_named(dt, c("file_index", "variable", "time", "lon", "lat", "dist", "value"))
    expect_identical(unique(dt$file_index), 1L)
    expect_identical(unique(dt$variable), "tas")
    expect_equal(nrow(dt), 4L)
    expect_equal(
        sort(unique(as.Date(dt$time))),
        as.Date(c("2060-01-02", "2060-01-03"))
    )

    coords <- data.table::CJ(lat = c(1.0, 2.0, 41.0), lon = c(103.5, 104.0, 104.5, 254.0))
    coords[, dist := local_tunnel_dist(lat, lon, 1.37, 103.98)]
    data.table::setorder(coords, dist)
    expected_coords <- coords[1:2, .(lat, lon)]
    got_coords <- unique(dt[, .(lat, lon)])
    data.table::setorder(got_coords, lat, lon)
    data.table::setorder(expected_coords, lat, lon)
    expect_equal(got_coords, expected_coords)

    dt_list <- ds$read_region(
        variable = "tas",
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        nearest = 1L,
        rbind = FALSE
    )
    expect_type(dt_list, "list")
    expect_length(dt_list, 2L)
    expect_true(all(vapply(dt_list, function(x) inherits(x, "data.table"), logical(1L))))

    dt_neg_lon <- ds$read_region(
        variable = "tas",
        lon = -106,
        lat = 41,
        time = c("2060-01-01T00:00:00Z", "2060-01-01T23:59:59Z"),
        nearest = 1L
    )
    expect_identical(unique(dt_neg_lon$lon), 254)
    expect_identical(unique(dt_neg_lon$lat), 41)

    expect_error(
        ds$read_region("hurs", lon = 103.98, lat = 1.37),
        "None of the requested variable"
    )
})

test_that("EsgDataset read_region() reuses recorded result time filters by default", {
    path1 <- tempfile(fileext = ".nc")
    path2 <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path1, 2060L)
    write_local_cmip6_netcdf_fixture(path2, 2061L)
    on.exit(unlink(c(path1, path2)), add = TRUE)

    ds <- EsgDataset$new(c(path1, path2))
    esg_dataset_set_context(ds, list(time_filter = list(
        start = "2060-01-02T00:00:00Z",
        stop = "2060-01-03T23:59:59Z",
        method = "drs"
    )))
    ds$open()
    on.exit(ds$close(), add = TRUE)

    expect_identical(ds$time_filter$method, "drs")

    dt_default <- ds$read_region("tas", lon = 103.98, lat = 1.37)
    dt_auto <- ds$read_region("tas", lon = 103.98, lat = 1.37, time = "auto")
    expect_equal(dt_default, dt_auto)
    expect_identical(unique(dt_auto$file_index), 1L)
    expect_equal(
        sort(unique(as.Date(dt_auto$time))),
        as.Date(c("2060-01-02", "2060-01-03"))
    )

    dt_all <- ds$read_region("tas", lon = 103.98, lat = 1.37, time = NULL)
    expect_true(nrow(dt_all) > nrow(dt_auto))
    expect_identical(sort(unique(dt_all$file_index)), c(1L, 2L))

    expect_warning(
        dt_outside <- ds$read_region(
            "tas",
            lon = 103.98,
            lat = 1.37,
            time = c("2061-01-01T00:00:00Z", "2061-01-02T23:59:59Z")
        ),
        "extends outside"
    )
    expect_identical(unique(dt_outside$file_index), 2L)
})

test_that("EsgDataset public async open keeps the dataset opened after return", {
    path <- local_dataset_table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    returned <- ds$open(async = TRUE, timeout = 2)
    on.exit(ds$close(), add = TRUE)

    expect_identical(returned, ds)
    expect_true(ds$is_open)
    expect_identical(private$async_state, "completed")
    expect_null(private$async_task)
    expect_equal(
        as.numeric(ds$var_get("tas", start = c(1L, 1L, 1L), count = c(2L, 1L, 1L), collapse = TRUE)),
        c(11, 12)
    )
})

test_that("EsgDataset public async reads match sync results and keep open-state checks", {
    path1 <- local_dataset_table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    path2 <- local_dataset_table_file(
        time_vals = c(3, 4, 5),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(21, 22, 23)
    )
    on.exit(unlink(c(path1, path2)), add = TRUE)

    ds_closed <- EsgDataset$new(path1)
    expect_error(ds_closed$read_array("tas", async = TRUE, timeout = 2), "not open")
    expect_error(ds_closed$var_get("tas", timeout = 2), "only supported")

    ds <- EsgDataset$new(c(path1, path2))
    ds$open()
    on.exit(ds$close(), add = TRUE)

    start <- c(1L, 1L, 1L)
    count <- c(2L, 1L, 1L)

    expect_equal(
        ds$var_get("tas", start = start, count = count, index = 2L, collapse = TRUE, async = TRUE, timeout = 2),
        ds$var_get("tas", start = start, count = count, index = 2L, collapse = TRUE)
    )

    expect_equal(
        ds$read_array("tas", start = start, count = count, collapse = FALSE, async = TRUE, timeout = 2),
        ds$read_array("tas", start = start, count = count, collapse = FALSE)
    )

    expect_equal(
        ds$read_data_table("tas", start = start, count = count, rbind = TRUE, async = TRUE, timeout = 2),
        ds$read_data_table("tas", start = start, count = count, rbind = TRUE)
    )

    expect_true(ds$is_open)
})

test_that("EsgDataset public async open supports concurrent local datasets", {
    skip_on_cran()

    paths <- c(
        local_dataset_table_file(
            time_vals = c(0, 1, 2),
            time_units = "days since 2000-01-01 00:00:00",
            tas_vals = c(11, 12, 13)
        ),
        local_dataset_table_file(
            time_vals = c(3, 4, 5),
            time_units = "days since 2000-01-01 00:00:00",
            tas_vals = c(21, 22, 23)
        )
    )
    on.exit(unlink(paths), add = TRUE)

    results <- mirai_dataset_lapply(seq_along(paths), function(i, paths) {
        ds <- EsgDataset$new(paths[[i]])
        private <- ds$.__enclos_env__$private
        on.exit(ds$close(), add = TRUE)

        ds$open(async = TRUE, timeout = 2)

        list(
            is_open = ds$is_open,
            async_state = private$async_state,
            async_task_is_null = is.null(private$async_task),
            values = as.numeric(ds$var_get("tas", collapse = TRUE))
        )
    }, paths = paths)

    expect_false(any(vapply(results, inherits, logical(1L), "try-error")))
    expect_true(all(vapply(results, `[[`, logical(1L), "is_open")))
    expect_true(all(vapply(results, `[[`, logical(1L), "async_task_is_null")))
    expect_equal(vapply(results, `[[`, character(1L), "async_state"), rep("completed", 2L))
    expect_equal(
        lapply(results, `[[`, "values"),
        list(c(11, 12, 13), c(21, 22, 23))
    )
})

test_that("EsgDataset public async reads support concurrent local datasets", {
    skip_on_cran()

    paths <- c(
        local_dataset_table_file(
            time_vals = c(0, 1, 2),
            time_units = "days since 2000-01-01 00:00:00",
            tas_vals = c(11, 12, 13)
        ),
        local_dataset_table_file(
            time_vals = c(3, 4, 5),
            time_units = "days since 2000-01-01 00:00:00",
            tas_vals = c(21, 22, 23)
        )
    )
    on.exit(unlink(paths), add = TRUE)

    start <- c(2L, 1L, 1L)
    count <- c(2L, 1L, 1L)
    results <- mirai_dataset_lapply(seq_along(paths), function(i, paths, start, count) {
        ds <- EsgDataset$new(paths[[i]])
        private <- ds$.__enclos_env__$private
        ds$open()
        on.exit(ds$close(), add = TRUE)

        async_values <- as.numeric(ds$var_get(
            "tas",
            start = start,
            count = count,
            collapse = TRUE,
            async = TRUE,
            timeout = 2
        ))

        list(
            is_open = ds$is_open,
            async_state = private$async_state,
            async_task_is_null = is.null(private$async_task),
            async_values = async_values,
            sync_values = as.numeric(ds$var_get(
                "tas",
                start = start,
                count = count,
                collapse = TRUE
            ))
        )
    }, paths = paths, start = start, count = count)

    expect_false(any(vapply(results, inherits, logical(1L), "try-error")))
    expect_true(all(vapply(results, `[[`, logical(1L), "is_open")))
    expect_true(all(vapply(results, `[[`, logical(1L), "async_task_is_null")))
    expect_equal(vapply(results, `[[`, character(1L), "async_state"), rep("completed", 2L))
    expect_equal(
        lapply(results, `[[`, "async_values"),
        list(c(12, 13), c(22, 23))
    )
    expect_equal(
        lapply(results, `[[`, "async_values"),
        lapply(results, `[[`, "sync_values")
    )
})

test_that("EsgDataset public async open failures leave the dataset closed and clean", {
    path <- tempfile(fileext = ".nc")
    if (file.exists(path)) {
        unlink(path)
    }

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    expect_error(ds$open(async = TRUE, timeout = 2), "Failed to open OPeNDAP connection")

    expect_false(ds$is_open)
    expect_identical(private$async_state, "failed")
    expect_null(private$async_task)
    expect_true(all(vapply(private$nc_handles, is.null, logical(1L))))
})

test_that("EsgDataset internal helpers transfer partially opened handles", {
    path_opened <- local_dataset_table_file(
        time_vals = c(0, 1),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12)
    )
    path_pending <- local_dataset_table_file(
        time_vals = c(2, 3),
        time_units = "days since 2000-01-03 00:00:00",
        tas_vals = c(13, 14)
    )
    on.exit(unlink(c(path_opened, path_pending)), add = TRUE)

    expect_error(EsgDataset$new(path_opened, nc_handles = list(NULL)), "unused argument")

    source <- EsgDataset$new(path_opened)
    source$open()
    handles <- esg_dataset_detach_handles(source)
    on.exit(esg_dataset_close_handles(path_opened, handles), add = TRUE)
    source_private <- source$.__enclos_env__$private

    expect_false(source$is_open)
    expect_true(all(vapply(source_private$nc_handles, is.null, logical(1L))))

    ds <- EsgDataset$new(c(path_opened, path_pending))
    esg_dataset_adopt_handles(ds, list(handles[[1L]], NULL))
    handles <- vector("list", length(handles))
    private <- ds$.__enclos_env__$private
    on.exit(ds$close(), add = TRUE)

    expect_false(ds$is_open)
    expect_false(is.null(private$nc_handles[[1L]]))
    expect_null(private$nc_handles[[2L]])

    ds$open()

    expect_true(ds$is_open)
    expect_false(is.null(private$nc_handles[[1L]]))
    expect_false(is.null(private$nc_handles[[2L]]))
    expect_equal(as.numeric(ds$var_get("tas", index = 1L)), c(11, 12))
    expect_equal(as.numeric(ds$var_get("tas", index = 2L)), c(13, 14))

    ds$close()
    expect_false(ds$is_open)
    expect_true(all(vapply(private$nc_handles, is.null, logical(1L))))

    missing_path <- tempfile(fileext = ".nc")
    if (file.exists(missing_path)) {
        unlink(missing_path)
    }
    failing_source <- EsgDataset$new(path_opened)
    failing_source$open()
    failing_handles <- esg_dataset_detach_handles(failing_source)
    failing <- EsgDataset$new(c(path_opened, missing_path))
    esg_dataset_adopt_handles(failing, list(failing_handles[[1L]], NULL))
    failing_handles <- vector("list", length(failing_handles))
    failing_private <- failing$.__enclos_env__$private

    expect_error(failing$open(), "Failed to open OPeNDAP connection")
    expect_false(failing$is_open)
    expect_true(all(vapply(failing_private$nc_handles, is.null, logical(1L))))
})

test_that("EsgDataset public async read failures clear task state and keep sync handles usable", {
    path <- local_dataset_table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(11, 12, 13)
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)
    private <- ds$.__enclos_env__$private

    expect_error(
        ds$var_get("missing_var", async = TRUE, timeout = 2),
        "Failed to read variable data"
    )

    expect_true(ds$is_open)
    expect_identical(private$async_state, "failed")
    expect_null(private$async_task)
    expect_equal(as.numeric(ds$var_get("tas", collapse = TRUE)), c(11, 12, 13))
})

test_that("EsgDataset handles errors gracefully", {
    skip_on_cran()

    ds <- EsgDataset$new(url_nc)

    # Test methods on closed dataset
    expect_error(ds$file_inq(), "not open")
    expect_error(ds$var_inq("tas"), "not open")
    expect_error(ds$var_get("tas"), "not open")
})

test_that("EsgDataset print works", {
    skip_on_cran()

    url <- "https://example.com/data.nc"
    ds <- EsgDataset$new(url)

    msg <- capture.output(print(ds), type = "message")
    expect_true(any(grepl("ESGF Dataset", msg, fixed = TRUE)))
    expect_true(any(grepl("URLs: 1", msg, fixed = TRUE)))
    expect_true(any(grepl("Status: Closed", msg, fixed = TRUE)))
})

test_that("EsgDataset internal async tasks keep sync handle state separate", {
    path <- local_dataset_table_file(
        time_vals = c(0, 1, 2),
        time_units = "days since 2000-01-01 00:00:00",
        tas_vals = c(101, 102, 103)
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    task <- private$start_async_operation(
        operation = "read variable data",
        handler = function(urls, nc_handles, variable, start, count, collapse) {
            RNetCDF::var.get.nc(nc_handles[[1L]], variable, start = start, count = count, collapse = collapse)
        },
        handler_args = list(
            variable = "tas",
            start = c(1L, 1L, 1L),
            count = c(2L, 1L, 1L),
            collapse = TRUE
        ),
        timeout = 2
    )

    expect_false(ds$is_open)
    expect_identical(private$async_state, "running")

    result <- private$collect_async_task(task)

    expect_identical(private$async_state, "completed")
    expect_null(private$async_task)
    expect_true(task$backend_released)
    expect_false(ds$is_open)
    expect_equal(as.numeric(result), c(101, 102))
})

test_that("EsgDataset internal async tasks surface timeout errors and clear lifecycle state", {
    path <- local_dataset_table_file(
        time_vals = c(0, 1),
        time_units = "days since 2000-01-01 00:00:00"
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    task <- private$start_async_operation(
        operation = "simulate timeout",
        handler = function(urls, nc_handles) {
            Sys.sleep(0.3)
            TRUE
        },
        timeout = 0.05
    )

    expect_error(private$collect_async_task(task), "timed out")
    expect_identical(private$async_state, "timed_out")
    expect_null(private$async_task)
    expect_true(task$backend_released)
})

test_that("EsgDataset close() best-effort cancels pending internal async work", {
    path <- local_dataset_table_file(
        time_vals = c(0, 1),
        time_units = "days since 2000-01-01 00:00:00"
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    ds$open()
    private <- ds$.__enclos_env__$private

    task <- private$start_async_operation(
        operation = "simulate cancellation",
        handler = function(urls, nc_handles) {
            Sys.sleep(5)
            TRUE
        },
        timeout = 10
    )

    ds$close()

    expect_false(ds$is_open)
    expect_true(task$cancellation_requested)
    expect_true(task$backend_released)
    expect_identical(private$async_state, "cancelled")
    expect_null(private$async_task)
})

test_that("EsgDataset internal async cancel race keeps cancelled terminal state", {
    path <- local_dataset_table_file(
        time_vals = c(0, 1),
        time_units = "days since 2000-01-01 00:00:00"
    )
    on.exit(unlink(path), add = TRUE)

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    task <- private$start_async_operation(
        operation = "cancel-race probe",
        handler = function(urls, nc_handles) {
            Sys.sleep(5)
            TRUE
        },
        timeout = 10
    )

    expect_true(isTRUE(mirai::stop_mirai(task$mirai_obj)))
    while (mirai::unresolved(task$mirai_obj)) {
        Sys.sleep(0.01)
    }

    requested <- private$cancel_async_task(task = task, clear = TRUE)

    expect_false(requested)
    expect_identical(task$status, "cancelled")
    expect_true(inherits(task$error, "epwshiftr_async_cancelled"))
    expect_match(conditionMessage(task$error), "cancelled", fixed = TRUE)
    expect_identical(private$async_state, "cancelled")
    expect_true(task$backend_released)
    expect_null(private$async_task)
})
