# Include instrumented namespace loading in worker startup. Deliberate timeout
# tests below keep their short deadlines and test cancellation independently.
dataset_async_timeout <- 60

# local_dataset_table_file() / local_dataset_cmip6_files() / mirai_dataset_symbols / start_mirai_dataset_runtime() / stop_mirai_dataset_runtime() / mirai_dataset_lapply() {{{
# Shared local native NetCDF builder for table, point, neighborhood, and climatology tests.
dataset_test__write_nc <- function(
    time_values,
    units,
    calendar = "standard",
    lat = c(0, 1),
    lon = c(10, 11),
    tas = NULL,
    bounds = NULL,
    bounds_attr = "bounds",
    bounds_var = "time_bnds",
    bounds_dim = "bnds",
    reversed = FALSE,
    method = NULL,
    extra_vars = NULL,
    extra_edit = NULL,
    path = tempfile(fileext = ".nc")
) {
    nc <- RNetCDF::create.nc(path)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    nt <- length(time_values)
    RNetCDF::dim.def.nc(nc, "time", nt)
    RNetCDF::dim.def.nc(nc, "lat", length(lat))
    RNetCDF::dim.def.nc(nc, "lon", length(lon))
    for (nm in c("time", "lat", "lon")) {
        RNetCDF::var.def.nc(nc, nm, "NC_DOUBLE", nm)
    }
    RNetCDF::var.def.nc(nc, "tas", "NC_DOUBLE", c("time", "lat", "lon"))
    RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", units)
    RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", calendar)
    RNetCDF::att.put.nc(nc, "tas", "units", "NC_CHAR", "K")
    if (!is.null(method)) {
        RNetCDF::att.put.nc(nc, "tas", "cell_methods", "NC_CHAR", method)
    }
    RNetCDF::var.put.nc(nc, "time", time_values)
    RNetCDF::var.put.nc(nc, "lat", lat)
    RNetCDF::var.put.nc(nc, "lon", lon)
    if (is.null(tas)) {
        tas <- array(
            270 + seq_len(nt * length(lat) * length(lon)),
            c(nt, length(lat), length(lon))
        )
    }
    RNetCDF::var.put.nc(nc, "tas", tas)
    if (length(extra_vars)) {
        for (nm in names(extra_vars)) {
            spec <- extra_vars[[nm]]
            RNetCDF::var.def.nc(nc, nm, "NC_DOUBLE", c("time", "lat", "lon"))
            RNetCDF::att.put.nc(nc, nm, "units", "NC_CHAR", "K")
            if (!is.null(spec$method)) {
                RNetCDF::att.put.nc(
                    nc,
                    nm,
                    "cell_methods",
                    "NC_CHAR",
                    spec$method
                )
            }
            RNetCDF::var.put.nc(
                nc,
                nm,
                if (is.null(spec$values)) tas else spec$values
            )
        }
    }
    if (!is.null(bounds)) {
        RNetCDF::dim.def.nc(nc, bounds_dim, 2L)
        dims <- if (reversed) c(bounds_dim, "time") else c("time", bounds_dim)
        RNetCDF::var.def.nc(nc, bounds_var, "NC_DOUBLE", dims)
        RNetCDF::att.put.nc(nc, "time", bounds_attr, "NC_CHAR", bounds_var)
        RNetCDF::var.put.nc(
            nc,
            bounds_var,
            if (reversed) t(bounds) else bounds
        )
    }
    if (is.function(extra_edit)) {
        extra_edit(nc)
    }
    path
}

# Memoized read-only fixtures shared across immutable tests in this file.
dataset_test__shared_cmip6_2060_2061 <- local({
    paths <- NULL
    function() {
        if (is.null(paths) || !all(file.exists(paths))) {
            paths <<- vapply(
                c(2060L, 2061L),
                function(year) {
                    path <- tempfile(
                        sprintf("dataset-cmip6-%d-", year),
                        fileext = ".nc"
                    )
                    write_local_cmip6_netcdf_fixture(path, year)
                    path
                },
                character(1L)
            )
            withr::defer(unlink(paths), envir = testthat::teardown_env())
        }
        paths
    }
})

dataset_test__shared_table_pair <- local({
    paths <- NULL
    function() {
        if (is.null(paths) || !all(file.exists(paths))) {
            time_vals <- list(c(0, 1, 2), c(3, 4, 5))
            tas_vals <- list(c(11, 12, 13), c(21, 22, 23))
            paths <<- vapply(
                1:2,
                function(i) {
                    local_dataset_table_file(
                        time_vals = time_vals[[i]],
                        time_units = "days since 2000-01-01 00:00:00",
                        tas_vals = tas_vals[[i]]
                    )
                },
                character(1L)
            )
            withr::defer(unlink(paths), envir = testthat::teardown_env())
        }
        paths
    }
})

# local_dataset_table_file {{{
local_dataset_table_file <- function(
    time_vals,
    time_units,
    tas_vals = seq_along(time_vals),
    calendar = "standard"
) {
    dataset_test__write_nc(
        time_vals,
        time_units,
        calendar = calendar,
        lat = 1,
        lon = 2,
        tas = array(tas_vals, dim = c(length(time_vals), 1L, 1L))
    )
}
# }}}

# local_dataset_cmip6_files {{{
local_dataset_cmip6_files <- function(years) {
    paths <- vapply(
        years,
        function(year) {
            path <- tempfile(fileext = ".nc")
            write_local_cmip6_netcdf_fixture(path, year)
            path
        },
        character(1L)
    )
    withr::defer(unlink(paths), envir = parent.frame())
    paths
}
# }}}

dataset_test__open <- function(paths, env = parent.frame()) {
    ds <- EsgDataset$new(paths)
    ds$open()
    withr::defer(ds$close(), envir = env)
    ds
}

dataset_test__open_temp <- function(path, env = parent.frame()) {
    withr::defer(unlink(path), envir = env)
    dataset_test__open(path, env = env)
}

dataset_test__edit_nc <- function(path, fn) {
    nc <- RNetCDF::open.nc(path, write = TRUE)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    fn(nc)
}

dataset_test__table_pair <- function(
    time_units = rep("days since 2000-01-01 00:00:00", 2L),
    time_vals = list(c(0, 1, 2), c(3, 4, 5)),
    tas_vals = list(c(11, 12, 13), c(21, 22, 23)),
    env = parent.frame()
) {
    paths <- vapply(
        seq_along(time_vals),
        function(i) {
            local_dataset_table_file(
                time_vals = time_vals[[i]],
                time_units = time_units[[i]],
                tas_vals = tas_vals[[i]]
            )
        },
        character(1L)
    )
    withr::defer(unlink(paths), envir = env)
    paths
}

dataset_test__mock_progress <- function(env = parent.frame()) {
    state <- list2env(
        list(bars = list(), updates = list(), dones = list()),
        parent = emptyenv()
    )
    testthat::local_mocked_bindings(
        cli_progress_bar = function(name = NULL, total = NA, ...) {
            state$bars[[length(state$bars) + 1L]] <- list(
                name = name,
                total = total
            )
            "progress-id"
        },
        cli_progress_update = function(id = NULL, set = NULL, ...) {
            state$updates[[length(state$updates) + 1L]] <- list(
                id = id,
                set = set
            )
        },
        cli_progress_done = function(id = NULL, result = "done", ...) {
            state$dones[[length(state$dones) + 1L]] <- list(
                id = id,
                result = result
            )
        },
        .package = "cli",
        .env = env
    )
    state
}

mirai_dataset_symbols <- c(
    "EsgDataset",
    "DatasetAsyncTask",
    "dataset__async_condition",
    "dataset__async_error",
    "dataset__progress_bar",
    "dataset__progress_update",
    "dataset__progress_done"
)

# start_mirai_dataset_runtime {{{
start_mirai_dataset_runtime <- function(workers) {
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
                try(
                    mirai::daemons(0, .compute = compute_profile),
                    silent = TRUE
                )
            }
        },
        add = TRUE
    )

    startup_error <- NULL
    tryCatch(
        {
            mirai__start_pool(
                workers,
                dispatcher = TRUE,
                .compute = compute_profile
            )
            started <- TRUE

            ready <- mirai::collect_mirai(mirai::mirai(
                TRUE,
                .compute = compute_profile
            ))
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
# }}}

# stop_mirai_dataset_runtime {{{
stop_mirai_dataset_runtime <- function(runtime) {
    if (is.null(runtime$compute_profile)) {
        return(invisible(NULL))
    }

    try(mirai::daemons(0, .compute = runtime$compute_profile), silent = TRUE)
    invisible(NULL)
}
# }}}

# mirai_dataset_lapply {{{
mirai_dataset_lapply <- function(X, FUN, ..., workers = min(2L, length(X))) {
    if (!length(X)) {
        return(vector("list", 0L))
    }

    runtime <- start_mirai_dataset_runtime(workers)
    on.exit(stop_mirai_dataset_runtime(runtime), add = TRUE)

    worker_symbols <- mget(
        mirai_dataset_symbols,
        envir = asNamespace("epwshiftr"),
        inherits = FALSE
    )
    dot_args <- list(...)
    tasks <- lapply(X, function(x) {
        mirai::mirai(
            {
                list2env(worker_symbols, envir = .GlobalEnv)
                on.exit(
                    rm(list = names(worker_symbols), envir = .GlobalEnv),
                    add = TRUE
                )
                environment(FUN) <- list2env(
                    worker_symbols,
                    parent = environment(FUN)
                )
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
# }}}
# }}}

# EsgDataset lifecycle and properties {{{
test_that("EsgDataset$new(), properties, $open(), and $close()", {
    paths <- dataset_test__shared_cmip6_2060_2061()

    single <- EsgDataset$new(paths[[1L]])
    multi <- EsgDataset$new(paths)
    expect_s3_class(single, "EsgDataset")
    expect_identical(single$url, paths[[1L]])
    expect_identical(multi$url, paths)
    expect_equal(single$file_count, 1L)
    expect_equal(multi$file_count, 2L)
    expect_false(single$is_aggregated)
    expect_true(multi$is_aggregated)
    expect_false(single$is_open)
    expect_false(multi$is_open)

    expect_identical(single$open(), single)
    expect_true(single$is_open)
    expect_identical(single$close(), single)
    expect_false(single$is_open)
})

test_that("EsgDataset$open() progress reporting", {
    paths <- dataset_test__shared_cmip6_2060_2061()

    prog <- dataset_test__mock_progress()
    ds <- EsgDataset$new(paths)
    withr::defer(ds$close())
    expect_identical(ds$open(progress = TRUE), ds)
    expect_true(ds$is_open)
    expect_equal(
        prog$bars,
        list(list(name = "Opening dataset files", total = 2L))
    )
    expect_equal(vapply(prog$updates, `[[`, integer(1L), "set"), c(1L, 2L))
    expect_equal(prog$dones, list(list(id = "progress-id", result = "done")))

    # Already adopted handles still report full progress.
    source <- EsgDataset$new(paths[[1L]])
    source$open()
    handles <- dataset__detach_handles(source)
    prog2 <- dataset_test__mock_progress()
    ds2 <- EsgDataset$new(paths)
    dataset__adopt_handles(ds2, list(handles[[1L]], NULL))
    withr::defer(ds2$close())
    ds2$open(progress = TRUE)
    expect_equal(
        prog2$bars,
        list(list(name = "Opening dataset files", total = 2L))
    )
    expect_equal(vapply(prog2$updates, `[[`, integer(1L), "set"), c(1L, 2L))
    expect_true(ds2$is_open)

    # progress = FALSE suppresses bar creation.
    prog3 <- dataset_test__mock_progress()
    ds3 <- EsgDataset$new(paths[[1L]])
    withr::defer(ds3$close())
    ds3$open(progress = FALSE)
    expect_length(prog3$bars, 0L)

    # Failures close the progress bar with result = "failed".
    missing_path <- tempfile(fileext = ".nc")
    prog4 <- dataset_test__mock_progress()
    ds4 <- EsgDataset$new(missing_path)
    expect_error(ds4$open(progress = TRUE), "Failed to open OPeNDAP connection")
    expect_false(ds4$is_open)
    expect_equal(prog4$dones, list(list(id = "progress-id", result = "failed")))
})
# }}}

# EsgDataset metadata, time axis, and spatial grid {{{
test_that("EsgDataset metadata inquiry, enumeration, time axis, and spatial grid", {
    paths <- dataset_test__shared_cmip6_2060_2061()
    ds <- dataset_test__open(paths)

    info <- ds$file_inq()
    expect_type(info, "list")
    expect_true(all(c("nvars", "ndims") %in% names(info)))
    expect_equal(ds$var_inq("tas")$name, "tas")
    expect_equal(ds$dim_inq("time")$name, "time")
    expect_identical(ds$att_get("tas", "units"), "K")
    expect_true("tas" %in% ds$get_variables())
    expect_true(all(c("time", "lat", "lon") %in% ds$get_dimensions()))

    for (index in seq_along(paths)) {
        fi <- ds$file_inq(index)
        expected_vars <- vapply(
            seq_len(fi$nvars) - 1L,
            function(id) ds$var_inq(id, index)$name,
            character(1L)
        )
        expected_dims <- vapply(
            seq_len(fi$ndims) - 1L,
            function(id) ds$dim_inq(id, index)$name,
            character(1L)
        )
        expect_identical(ds$get_variables(index), expected_vars)
        expect_identical(ds$get_dimensions(index), expected_dims)
    }

    time_info <- ds$get_time_axis(2)
    expect_true(all(
        c("values", "units", "calendar", "coordinates", "bounds") %in%
            names(time_info)
    ))
    expect_s3_class(time_info$values, "POSIXct")
    expect_named(time_info$coordinates, CF_TIME_COORDINATE_COLUMNS)
    expect_identical(time_info$bounds$name, "time_bnds")
    expect_s3_class(time_info$bounds$start, "POSIXct")
    expect_s3_class(time_info$bounds$end, "POSIXct")
    expect_identical(
        as.numeric(time_info$bounds$end - time_info$bounds$start),
        rep.int(1, time_info$length)
    )

    grid <- ds$get_spatial_grid()
    expect_true(all(c("lat", "lon") %in% names(grid)))
})

# A failed enumeration must be retried. A successful declaration list is valid
# only for the current open handles, even if the next open uses the same path.
test_that("variable declarations are cached only after success until close", {
    path <- file.path(withr::local_tempdir(), "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    ds <- dataset_test__open(path)

    inquire <- RNetCDF::var.inq.nc
    declaration_reads <- 0L
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.inq.nc = function(ncfile, variable) {
            declaration_reads <<- declaration_reads + 1L
            if (declaration_reads == 2L) {
                stop("NetCDF: declaration inquiry failed", call. = FALSE)
            }
            inquire(ncfile, variable)
        }
    )
    expect_error(ds$get_variables(), "declaration inquiry failed")
    declared <- ds$get_variables()
    expect_true("tas" %in% declared)
    count <- declaration_reads
    expect_identical(count, ds$file_inq()$nvars + 2L)
    expect_identical(ds$get_variables(), declared)
    expect_identical(declaration_reads, count)

    ds$close()
    write_local_cmip6_netcdf_fixture(path, 2060L, "huss")
    ds$open()
    expect_identical(
        ds$get_variables(),
        replace(declared, declared == "tas", "huss")
    )
    expect_gt(declaration_reads, count)
})

# Simulate one native read failure with a real NetCDF file so the retry uses
# the same dataset handle and must retrieve the original coordinate values.
# A missing declaration is still a valid partial grid.
test_that("EsgDataset retries failed coordinate reads and returns NULL for undeclared axes", {
    path <- file.path(withr::local_tempdir(), "grid.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    read_variable <- RNetCDF::var.get.nc
    fail_latitude <- TRUE
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.get.nc = function(ncfile, variable, ...) {
            if (fail_latitude && identical(variable, "lat")) {
                stop("NetCDF: DAP server error", call. = FALSE)
            }
            read_variable(ncfile, variable, ...)
        }
    )
    ds <- dataset_test__open(path)
    expect_error(ds$get_spatial_grid(), "DAP server error")
    fail_latitude <- FALSE
    grid <- ds$get_spatial_grid()
    expect_equal(as.numeric(grid$lat), c(1, 2, 41))
    expect_equal(as.numeric(grid$lon), c(103.5, 104, 104.5, 254))

    path2 <- file.path(withr::local_tempdir(), "partial.nc")
    write_local_cmip6_netcdf_fixture(path2, 2060L)
    dataset_test__edit_nc(path2, function(nc) {
        RNetCDF::var.rename.nc(nc, "lat", "other_latitude")
    })
    grid2 <- dataset_test__open(path2)$get_spatial_grid()
    expect_named(grid2, c("lat", "lon"))
    expect_null(grid2$lat)
    expect_equal(as.numeric(grid2$lon), c(103.5, 104, 104.5, 254))
})
# }}}

# EsgDataset$var_get(), $read_array(), $read_data_table() {{{
test_that("EsgDataset$var_get(), $read_array(), and $read_data_table() read single and multi-file slices", {
    paths <- dataset_test__shared_cmip6_2060_2061()
    ds1 <- dataset_test__open(paths[[1L]])
    ds <- dataset_test__open(paths)
    start <- c(1, 1, 1)
    count <- c(2, 2, 2)

    for (collapse in c(TRUE, FALSE)) {
        v <- ds1$var_get(
            "tas",
            start = start,
            count = count,
            collapse = collapse
        )
        expect_type(v, "double")
        expect_equal(dim(v), c(2, 2, 2))
        arr_list <- ds1$read_array(
            "tas",
            start = start,
            count = count,
            collapse = collapse
        )
        expect_length(arr_list, 1L)
        expect_type(arr_list[[1L]], "double")
        expect_equal(dim(arr_list[[1L]]), c(2, 2, 2))
    }

    vinfo <- ds$var_inq("tas")
    dim_names <- vapply(
        vinfo$dimids,
        function(id) ds$dim_inq(id)$name,
        character(1L)
    )
    check_dt <- function(target_ds, dt, index) {
        expect_s3_class(dt, "data.table")
        expect_true(all(c("tas", dim_names) %in% names(dt)))
        expect_equal(nrow(dt), prod(count))
        for (j in seq_along(dim_names)) {
            nm <- dim_names[[j]]
            if (identical(nm, "time")) {
                coord <- target_ds$get_time_axis(index)$values[seq.int(
                    start[[j]],
                    length.out = count[[j]]
                )]
                expect_s3_class(dt[[nm]], "POSIXct")
                expect_identical(attr(dt[[nm]], "tzone"), "UTC")
                expect_equal(
                    as.numeric(sort(unique(dt[[nm]]))),
                    as.numeric(sort(coord))
                )
            } else {
                coord <- target_ds$var_get(
                    nm,
                    start = start[[j]],
                    count = count[[j]],
                    index = index,
                    collapse = TRUE
                )
                expect_equal(sort(unique(dt[[nm]])), sort(as.vector(coord)))
            }
        }
    }

    dt1 <- ds1$read_data_table("tas", start = start, count = count)
    expect_length(dt1, 1L)
    check_dt(ds1, dt1[[1L]], 1L)

    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_length(dt_list, 2L)
    for (i in 1:2) {
        check_dt(ds, dt_list[[i]], i)
    }

    dt_all <- ds$read_data_table(
        "tas",
        start = start,
        count = count,
        rbind = TRUE
    )
    expect_s3_class(dt_all, "data.table")
    expect_equal(sort(unique(dt_all$file_index)), c(1L, 2L))
})

test_that("EsgDataset$read_data_table() returns UTC POSIXct time for CMIP6-like CF units", {
    paths <- dataset_test__table_pair(
        time_units = c(
            "days since 1850-01-01 00:00:00",
            "days since 2010-01-01 00:00:00"
        ),
        time_vals = list(0:2, 0:2)
    )
    ds <- dataset_test__open(paths)
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
        expect_s3_class(time, "POSIXct")
        expect_identical(attr(time, "tzone"), "UTC")
        expect_length(time, count[[1L]])
        expect_false(is.unsorted(as.numeric(time), strictly = TRUE))
        expect_equal(as.numeric(time), as.numeric(expected[[i]]))
    }
    dt_all <- ds$read_data_table(
        "tas",
        start = start,
        count = count,
        rbind = TRUE
    )
    expect_s3_class(dt_all$time, "POSIXct")
    expect_identical(attr(dt_all$time, "tzone"), "UTC")
    expect_equal(
        as.numeric(dt_all[file_index == 1L, time]),
        as.numeric(expected[[1L]])
    )
    expect_equal(
        as.numeric(dt_all[file_index == 2L, time]),
        as.numeric(expected[[2L]])
    )
})
# }}}
# EsgDataset$read_region() {{{
# The first file lacks tas, while two later files declare it. A failed
# inquiry in the second file must not yield a successful, partial series.
test_that("region readers preserve metadata failures and retry the full series", {
    root <- withr::local_tempdir()
    paths <- file.path(root, c("huss.nc", "tas-first.nc", "tas-second.nc"))
    write_local_cmip6_netcdf_fixture(paths[[1L]], 2059L, "huss")
    write_local_cmip6_netcdf_fixture(paths[[2L]], 2060L)
    write_local_cmip6_netcdf_fixture(paths[[3L]], 2061L)

    inquire <- RNetCDF::var.inq.nc
    fail_next <- FALSE
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.inq.nc = function(ncfile, variable) {
            info <- inquire(ncfile, variable)
            if (fail_next && identical(variable, "tas")) {
                fail_next <<- FALSE
                stop("NetCDF: DAP dimension inquiry failed", call. = FALSE)
            }
            info
        }
    )
    sites <- data.table::data.table(
        site_id = "point",
        lon = 104,
        lat = 1,
        method = "nearest"
    )

    # Use a fresh dataset per entry point so the other read cannot warm its
    # metadata cache. Retry immediately on the same object after the error.
    read_with_retry <- function(read) {
        ds <- EsgDataset$new(paths)
        ds$open()
        on.exit(ds$close(), add = TRUE)
        fail_next <<- TRUE
        expect_error(read(ds), "DAP dimension inquiry failed")
        read(ds)
    }
    scalar <- read_with_retry(function(ds) {
        ds$read_region("tas", lon = 104, lat = 1, time = NULL)
    })
    multi <- read_with_retry(function(ds) {
        dataset__read_regions(ds, "tas", sites, time = NULL)
    })

    expect_identical(unique(scalar$file_index), c(2L, 3L))
    expect_equal(nrow(scalar), 731L)
    columns <- c("file_index", CF_TIME_COORDINATE_COLUMNS, "value")
    expect_equal(
        lapply(columns, function(name) multi[[name]]),
        lapply(columns, function(name) scalar[[name]])
    )
})

test_that("nearest and IDW source cells preserve full-grid ranking", {
    grid_lat <- c(-80, -40, 0, 40, 80)
    grid_lon <- c(0, 60, 120, 180, 240, 300)
    coords <- spatial__make_region_grid_coords(grid_lat, grid_lon)

    for (target in list(c(22, 75), c(79, 359), c(-78, 181))) {
        distances <- tunnel_dist(
            coords$grid_lat,
            coords$grid_lon,
            target[[1L]],
            target[[2L]]
        )
        expected <- coords[order(distances)]
        for (method in c("nearest", "idw")) {
            actual <- spatial__region_grid_sources(
                method,
                grid_lat,
                grid_lon,
                target[[1L]],
                target[[2L]],
                coords = coords
            )
            count <- if (identical(method, "nearest")) 1L else 4L
            expect_identical(actual$ind_lat, expected$ind_lat[seq_len(count)])
            expect_identical(actual$ind_lon, expected$ind_lon[seq_len(count)])
            expect_equal(sum(actual$weight), 1)
        }
    }
})

test_that("the internal multi-site reader shares sparse cells across distant sites", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    on.exit(unlink(path), add = TRUE)
    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    sites <- data.table::data.table(
        site_id = c("Singapore", "Singapore-copy", "Western-point"),
        lon = c(103.98, 103.98, -106),
        lat = c(1.37, 1.37, 41),
        method = rep.int("nearest", 3L)
    )
    window <- c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    actual <- dataset__read_regions(ds, "tas", sites, time = window)
    sources <- attr(actual, "grid_sources")

    expect_equal(nrow(actual), 6L)
    expect_equal(nrow(sources), 3L)
    expect_true(all(sources$weight == 1))
    expect_identical(unique(actual$cf_calendar), "360_day")
    for (index in seq_len(nrow(sites))) {
        site <- sites[index]
        expected <- ds$read_region(
            "tas",
            lon = site$lon[[1L]],
            lat = site$lat[[1L]],
            time = window,
            method = site$method[[1L]]
        )
        observed <- actual[actual$site_id == site$site_id[[1L]]]
        expect_equal(observed$value, expected$value)
        expect_equal(observed$cf_day, expected$cf_day)
        expect_equal(observed$time_bound_start, expected$time_bound_start)
        expect_equal(observed$time_bound_end, expected$time_bound_end)
    }
})

test_that("the internal multi-site reader keeps method weights per site", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    on.exit(unlink(path), add = TRUE)
    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    sites <- data.table::data.table(
        site_id = ESG_GRID_METHOD_CHOICES,
        lon = rep.int(103.98, 4L),
        lat = rep.int(1.37, 4L),
        method = ESG_GRID_METHOD_CHOICES
    )
    window <- c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    actual <- dataset__read_regions(ds, "tas", sites, time = window)
    sources <- attr(actual, "grid_sources")
    for (method in ESG_GRID_METHOD_CHOICES) {
        target_method <- method
        expected <- ds$read_region(
            "tas",
            lon = 103.98,
            lat = 1.37,
            time = window,
            method = method
        )
        observed <- actual[actual$site_id == target_method]
        expect_equal(observed$value, expected$value, tolerance = 1e-5)
        expect_equal(sum(sources$weight[sources$site_id == method]), 1)
    }
})

test_that("multi-site native reads reject oversized output requests", {
    path <- local_dataset_table_file(
        time_vals = 0:8999,
        time_units = "days since 2060-01-01 00:00:00",
        tas_vals = seq_len(9000L)
    )
    on.exit(unlink(path), add = TRUE)
    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)
    many_sites <- data.table::data.table(
        site_id = sprintf("site-%03d", seq_len(105L)),
        lon = rep.int(2, 105L),
        lat = rep.int(1, 105L),
        method = rep.int("nearest", 105L)
    )
    expect_error(
        dataset__read_regions(ds, "tas", many_sites),
        "exceeds 250000 output rows"
    )
})

test_that("the internal multi-site reader respects native site windows", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(
        path,
        2060L,
        calendar = "360_day",
        n_years = 2L
    )
    on.exit(unlink(path), add = TRUE)
    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    sites <- data.table::data.table(
        site_id = c("first", "second"),
        lon = c(103.98, -106),
        lat = c(1.37, 41),
        method = c("nearest", "nearest"),
        time_start = as.POSIXct(c("2060-12-30", "2061-01-01"), tz = "UTC"),
        time_stop = as.POSIXct(
            c("2060-12-30 23:59:59", "2061-01-01 23:59:59"),
            tz = "UTC"
        )
    )
    actual <- dataset__read_regions(ds, "tas", sites)
    expect_equal(nrow(actual), 2L)
    expect_identical(actual$site_id, c("first", "second"))
    expect_identical(actual$cf_year, c(2060L, 2061L))
    expect_identical(actual$cf_month, c(12L, 1L))
    expect_identical(actual$cf_day, c(30L, 1L))
})

test_that("the internal multi-site reader rejects ambiguous sites, empty input, and preserves missing values", {
    path <- local_dataset_cmip6_files(2060L)
    ds <- dataset_test__open(path)

    sites <- data.table::data.table(
        site_id = "first",
        lon = 103.98,
        lat = 1.37,
        method = "nearest"
    )
    expect_error(dataset__read_regions(ds, "tas", sites[0L]), "at least 1 rows")
    expect_error(
        dataset__read_regions(ds, "tas", sites[c(1L, 1L)]),
        "duplicated"
    )
    expect_error(
        dataset__read_regions(ds, "missing", sites),
        "None of the requested variable"
    )
    expect_error(
        dataset__read_regions_one(
            ds,
            "tas",
            sites,
            time = NULL,
            index = 2L,
            async = FALSE,
            timeout = NULL
        ),
        "index"
    )
    empty <- dataset__read_regions(
        ds,
        "tas",
        sites,
        time = c("2061-01-01", "2061-01-02")
    )
    expect_equal(nrow(empty), 0L)
    expect_s3_class(attr(empty, "grid_sources"), "data.table")
    expect_s3_class(attr(empty, "read_slices"), "data.table")
    expect_named(
        attr(empty, "grid_sources"),
        c(
            "site_id",
            "source_index",
            "role",
            "grid_lon",
            "grid_lat",
            "grid_dist_km",
            "weight",
            "file_index",
            "variable",
            "method"
        )
    )
    expect_named(
        attr(empty, "read_slices"),
        c(
            "file_index",
            "variable",
            "ind_lat",
            "ind_lon",
            "lat_count",
            "lon_count",
            "time_start_index",
            "time_count"
        )
    )

    ds$close()
    dataset_test__edit_nc(path, function(nc) {
        RNetCDF::var.put.nc(
            nc,
            "tas",
            NA_real_,
            start = c(2L, 1L, 2L),
            count = c(1L, 1L, 1L)
        )
    })
    ds$open()
    sites_na <- data.table::data.table(
        site_id = c("nearest", "mean"),
        lon = c(103.98, 103.98),
        lat = c(1.37, 1.37),
        method = c("nearest", "mean")
    )
    actual <- dataset__read_regions(
        ds,
        "tas",
        sites_na,
        time = c("2060-01-02", "2060-01-03 23:59:59")
    )
    for (method in sites_na$method) {
        observed <- actual[actual$site_id == method]
        expect_true(is.na(observed$value[[1L]]))
        expect_true(is.finite(observed$value[[2L]]))
    }
})

test_that("EsgDataset$read_region() reads grid-method values, windows, and 360-day CF boundaries", {
    ds <- dataset_test__open(dataset_test__shared_cmip6_2060_2061())
    window <- c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    expected_values <- function(sources) {
        lat_vals <- c(1.0, 2.0, 41.0)
        lon_vals <- c(103.5, 104.0, 104.5, 254.0)
        spatial <- match(sources$grid_lat, lat_vals) /
            10 +
            match(sources$grid_lon, lon_vals) / 20
        vapply(
            c(1.5, 2.5),
            function(t) {
                sum(
                    (299 + 5 * sin(2 * pi * t / 366) + spatial) * sources$weight
                )
            },
            numeric(1L)
        )
    }

    expected_cols <- c(
        "file_index",
        "variable",
        "time",
        "time_bound_start",
        "time_bound_end",
        CF_TIME_COORDINATE_COLUMNS,
        "lon",
        "lat",
        "method",
        "value"
    )
    for (method in ESG_GRID_METHOD_CHOICES) {
        dt <- ds$read_region(
            "tas",
            lon = 103.98,
            lat = 1.37,
            time = window,
            method = method
        )
        sources <- attr(dt, "grid_sources")
        expect_s3_class(dt, "data.table")
        expect_named(dt, expected_cols)
        expect_equal(nrow(dt), 2L)
        expect_equal(
            as.list(dt[, .(
                file_index,
                variable,
                method,
                cf_calendar,
                cf_year,
                cf_month,
                cf_day,
                cf_day_of_year,
                cf_year_days,
                cf_second_of_day,
                lon,
                lat
            )]),
            list(
                file_index = rep(1L, 2L),
                variable = rep("tas", 2L),
                method = rep(method, 2L),
                cf_calendar = rep("proleptic_gregorian", 2L),
                cf_year = rep(2060L, 2L),
                cf_month = rep(1L, 2L),
                cf_day = 2:3,
                cf_day_of_year = 2:3,
                cf_year_days = rep(366L, 2L),
                cf_second_of_day = rep(43200, 2L),
                lon = rep(103.98, 2L),
                lat = rep(1.37, 2L)
            )
        )
        expect_equal(dt$annual_phase, c(1.5, 2.5) / 366)
        expect_equal(
            as.numeric(dt$time_bound_end - dt$time_bound_start),
            c(1, 1)
        )
        expect_equal(
            as.numeric(dt$time - dt$time_bound_start, units = "days"),
            c(0.5, 0.5)
        )
        expect_equal(
            sort(unique(as.Date(dt$time))),
            as.Date(c("2060-01-02", "2060-01-03"))
        )
        expect_s3_class(sources, "data.table")
        expect_equal(sum(sources$weight), 1)
        expect_equal(
            nrow(sources),
            if (identical(method, "nearest")) 1L else 4L
        )
        expect_equal(dt$value, expected_values(sources), tolerance = 1e-5)
    }

    dt_list <- ds$read_region(
        "tas",
        lon = 103.98,
        lat = 1.37,
        time = window,
        rbind = FALSE
    )
    expect_length(dt_list, 2L)
    expect_true(all(vapply(dt_list, inherits, logical(1L), "data.table")))
    expect_true(all(vapply(
        dt_list,
        function(x) inherits(attr(x, "grid_sources"), "data.table"),
        logical(1L)
    )))

    dt_neg <- ds$read_region(
        "tas",
        lon = -106,
        lat = 41,
        time = c("2060-01-01T00:00:00Z", "2060-01-01T23:59:59Z")
    )
    expect_identical(c(unique(dt_neg$lon), unique(dt_neg$lat)), c(-106, 41))
    expect_identical(
        c(
            unique(attr(dt_neg, "grid_sources")$grid_lon),
            unique(attr(dt_neg, "grid_sources")$grid_lat)
        ),
        c(254, 41)
    )
    expect_error(
        ds$read_region("hurs", lon = 103.98, lat = 1.37),
        "None of the requested variable"
    )

    path360 <- withr::local_tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(
        path360,
        2060L,
        calendar = "360_day",
        n_years = 2L
    )
    dt360 <- dataset_test__open(path360)$read_region(
        "tas",
        lon = 103.98,
        lat = 1.37,
        time = c("2060-12-30T00:00:00Z", "2061-01-01T23:59:59Z")
    )
    expect_equal(
        as.list(dt360[, .(
            cf_calendar,
            cf_year,
            cf_month,
            cf_day,
            cf_day_of_year,
            cf_year_days,
            cf_second_of_day
        )]),
        list(
            cf_calendar = rep("360_day", 2L),
            cf_year = c(2060L, 2061L),
            cf_month = c(12L, 1L),
            cf_day = c(30L, 1L),
            cf_day_of_year = c(360L, 1L),
            cf_year_days = rep(360L, 2L),
            cf_second_of_day = rep(43200, 2L)
        )
    )
    expect_equal(dt360$annual_phase, c(359.5 / 360, 0.5 / 360))
    expect_equal(
        as.numeric(dt360$time_bound_end - dt360$time_bound_start),
        c(1, 1)
    )
    # Both surrogate timestamps remain in Gregorian 2060; CF columns are authoritative.
    expect_identical(
        as.integer(format(dt360$time, "%Y", tz = "UTC")),
        c(2060L, 2060L)
    )
})

test_that("EsgDataset$read_region() retains native clock schema for empty results", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    on.exit(unlink(path), add = TRUE)
    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)

    empty <- ds$read_region(
        "tas",
        lon = 103.98,
        lat = 1.37,
        time = c("2061-01-01", "2061-01-02")
    )
    expect_equal(nrow(empty), 0L)
    expect_true(all(CF_TIME_COORDINATE_COLUMNS %in% names(empty)))
    expect_identical(empty$cf_second_of_day, numeric())
    pieces <- ds$read_region(
        "tas",
        lon = 103.98,
        lat = 1.37,
        time = c("2061-01-01", "2061-01-02"),
        rbind = FALSE
    )
    expect_length(pieces, 1L)
    expect_identical(pieces[[1L]], empty)
})

test_that("EsgDataset$read_region() reuses recorded result time filters by default", {
    ds <- EsgDataset$new(dataset_test__shared_cmip6_2060_2061())
    dataset__set_context(
        ds,
        list(
            time_filter = list(
                start = "2060-01-02T00:00:00Z",
                stop = "2060-01-03T23:59:59Z",
                method = "drs"
            )
        )
    )
    ds$open()
    withr::defer(ds$close())

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
# }}}
# EsgDataset$slice() / $selection() {{{
test_that("EsgDataset$slice() and $selection() preserve context, validate selectors, and reopen", {
    paths <- dataset_test__table_pair(
        time_units = rep("days since 2000-01-01 00:00:00", 3L),
        time_vals = list(0:1, 2:3, 4:5),
        tas_vals = list(11:12, 21:22, 31:32)
    )
    expect_identical(
        EsgDataset$new(paths[1:2])$selection(),
        list(source_count = 2L, source_num_found = 2L, source_indices = 1:2)
    )
    ds <- EsgDataset$new(paths)
    dataset__set_context(
        ds,
        list(
            time_filter = list(
                start = "2000-01-01T00:00:00Z",
                stop = "2000-01-02T23:59:59Z",
                method = "drs"
            ),
            selection = list(
                source_count = 5L,
                source_num_found = 10L,
                source_indices = c(2L, 4L, 5L)
            )
        )
    )
    sliced <- ds$slice(c(3L, 1L))
    expect_false(sliced$is_open)
    expect_identical(sliced$url, paths[c(3L, 1L)])
    expect_identical(ds$url, paths)
    expect_identical(sliced$time_filter$method, "drs")
    expect_identical(
        sliced$selection(),
        list(
            source_count = 5L,
            source_num_found = 10L,
            source_indices = c(5L, 2L)
        )
    )
    chained <- sliced$slice(2L)
    expect_identical(chained$url, paths[[1L]])
    expect_identical(chained$selection()$source_indices, 2L)
    expect_identical(
        ds$slice(c(TRUE, FALSE, TRUE))$selection()$source_indices,
        c(2L, 5L)
    )
    expect_identical(ds$slice(-2L)$url, paths[c(1L, 3L)])

    ds2 <- dataset_test__open(paths[1:2])
    expect_error(ds2$slice(1L), "reopen = TRUE")
    reopened <- ds2$slice(2L, reopen = TRUE)
    withr::defer(reopened$close())
    expect_true(ds2$is_open && reopened$is_open)
    expect_identical(reopened$url, paths[2L])
    expect_identical(reopened$selection()$source_indices, 2L)
    expect_equal(
        as.numeric(reopened$var_get(
            "tas",
            start = c(1L, 1L, 1L),
            count = c(1L, 1L, 1L),
            collapse = TRUE
        )),
        21
    )
    expect_equal(
        as.numeric(ds2$var_get(
            "tas",
            index = 2L,
            start = c(1L, 1L, 1L),
            count = c(1L, 1L, 1L),
            collapse = TRUE
        )),
        21
    )

    ds_closed <- EsgDataset$new(paths[1:2])
    expect_error(ds_closed$slice(), "must be supplied")
    for (case in list(
        list(NULL, "must not be `NULL`"),
        list(integer(), "at least one file"),
        list(c(FALSE, FALSE), "at least one file"),
        list(c(TRUE, FALSE, TRUE), "length"),
        list(c(1L, 1L), "duplicate"),
        list(0L, "zero"),
        list(c(1L, -2L), "mix"),
        list(3L, "outside"),
        list("1", "integer")
    )) {
        expect_error(ds_closed$slice(case[[1L]]), case[[2L]])
    }
})
# }}}
# EsgDataset$reachable() {{{
test_that("EsgDataset$reachable() checks local files, Windows paths, and remote data nodes", {
    pair <- dataset_test__shared_table_pair()
    paths <- c(pair[[1L]], tempfile(fileext = ".nc"), pair[[2L]])
    diag <- EsgDataset$new(paths)$slice(c(3L, 2L))$reachable()
    expect_s3_class(diag, "data.table")
    expect_named(
        diag,
        c(
            "file_index",
            "source_index",
            "data_node",
            "service",
            "url",
            "reachable",
            "latency_ms",
            "error",
            "probe_level",
            "probe_url",
            "probe_cached"
        )
    )
    expect_equal(
        as.list(diag),
        list(
            file_index = 1:2,
            source_index = c(3L, 2L),
            data_node = c(NA_character_, NA_character_),
            service = c("local", "local"),
            url = paths[c(3L, 2L)],
            reachable = c(TRUE, FALSE),
            latency_ms = c(0, 0),
            error = c(NA_character_, "File does not exist."),
            probe_level = c("local", "local"),
            probe_url = paths[c(3L, 2L)],
            probe_cached = c(FALSE, FALSE)
        )
    )

    file_url <- EsgDataset$new(paste0("file://", paths[[1L]]))$reachable()
    expect_identical(file_url$service, "local")
    expect_true(file_url$reachable && file_url$latency_ms == 0)

    win_path <- "C:/epwshiftr/missing.nc"
    expect_identical(query_result__url_scheme(win_path), NA_character_)
    expect_true(query_result__url_local(win_path))
    win_diag <- EsgDataset$new(win_path)$reachable()
    expect_identical(
        c(win_diag$service, win_diag$probe_level, win_diag$probe_url),
        c("local", "local", win_path)
    )
    expect_false(win_diag$reachable)
    expect_identical(win_diag$error, "File does not exist.")

    ds <- EsgDataset$new(c(
        "https://ok.example.org/data.nc",
        "https://bad.example.org/data.nc"
    ))
    dataset__set_context(
        ds,
        list(
            selection = list(
                source_count = 11L,
                source_num_found = 20L,
                source_indices = c(10L, 11L)
            ),
            reachability = list(stale = TRUE)
        )
    )
    seen <- NULL
    testthat::local_mocked_bindings(
        query_result__reach_nodes = function(
            data_node,
            timeout = 5,
            network_policy = NULL,
            probe_concurrency = 1L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        ) {
            seen <<- list(
                data_node = data_node,
                timeout = timeout,
                useragent = network_policy$useragent,
                probe_concurrency = probe_concurrency,
                cache_seconds = cache_seconds,
                cache_failures_seconds = cache_failures_seconds
            )
            data.table::data.table(
                data_node = data_node,
                reachable = c(TRUE, FALSE),
                latency_ms = c(12, NA_real_),
                error = c(NA_character_, "remote boom"),
                probe_url = paste0("https://", data_node, "/"),
                probe_cached = FALSE
            )
        },
        .package = "epwshiftr"
    )
    rdiag <- ds$reachable(
        probe = list(
            timeout = 7,
            concurrency = 2L,
            network_policy = list(useragent = "dataset-agent")
        )
    )
    expect_identical(
        seen,
        list(
            data_node = c("ok.example.org", "bad.example.org"),
            timeout = 7,
            useragent = "dataset-agent",
            probe_concurrency = 2L,
            cache_seconds = 3600L,
            cache_failures_seconds = 0L
        )
    )
    expect_identical(rdiag$source_index, c(10L, 11L))
    expect_true(all(is.na(rdiag$service)))
    expect_identical(rdiag$reachable, c(TRUE, FALSE))
    expect_equal(rdiag$latency_ms, c(12, NA))
    expect_identical(rdiag$error, c(NA_character_, "remote boom"))
    expect_identical(rdiag$probe_level, c("data_node", "data_node"))
    expect_false(any(rdiag$probe_cached))
})
# }}}
# EsgDataset async open / read parity and concurrency {{{
test_that("EsgDataset async open, reads, progress, and failures match sync contracts", {
    skip_on_cran()
    paths <- dataset_test__shared_table_pair()
    start <- c(1L, 1L, 1L)
    count <- c(2L, 1L, 1L)
    to <- dataset_async_timeout

    ds_closed <- EsgDataset$new(paths[[1L]])
    expect_error(ds_closed$var_get("tas", timeout = to), "only supported")
    expect_error(
        ds_closed$read_array("tas", async = TRUE, timeout = to),
        "not open"
    )

    prog <- dataset_test__mock_progress()
    ds <- EsgDataset$new(paths)
    private <- ds$.__enclos_env__$private
    expect_identical(ds$open(async = TRUE, timeout = to, progress = TRUE), ds)
    withr::defer(ds$close())
    expect_true(ds$is_open)
    expect_identical(private$async_state, "completed")
    expect_null(private$async_task)
    expect_equal(
        vapply(prog$updates, `[[`, integer(1L), "set"),
        c(1L, 2L)
    )
    expect_equal(
        as.numeric(ds$var_get(
            "tas",
            start = start,
            count = count,
            collapse = TRUE
        )),
        c(11, 12)
    )
    expect_equal(
        ds$var_get(
            "tas",
            start = start,
            count = count,
            index = 2L,
            collapse = TRUE,
            async = TRUE,
            timeout = to
        ),
        ds$var_get(
            "tas",
            start = start,
            count = count,
            index = 2L,
            collapse = TRUE
        )
    )
    expect_equal(
        ds$read_array(
            "tas",
            start = start,
            count = count,
            collapse = FALSE,
            async = TRUE,
            timeout = to
        ),
        ds$read_array("tas", start = start, count = count, collapse = FALSE)
    )
    expect_equal(
        ds$read_data_table(
            "tas",
            start = start,
            count = count,
            rbind = TRUE,
            async = TRUE,
            timeout = to
        ),
        ds$read_data_table("tas", start = start, count = count, rbind = TRUE)
    )
    expect_true(ds$is_open)

    # Failed async open leaves the dataset closed and clean.
    bad <- EsgDataset$new(tempfile(fileext = ".nc"))
    bad_priv <- bad$.__enclos_env__$private
    expect_error(
        bad$open(async = TRUE, timeout = to),
        "Failed to open OPeNDAP connection"
    )
    expect_false(bad$is_open)
    expect_identical(bad_priv$async_state, "failed")
    expect_null(bad_priv$async_task)
    expect_true(all(vapply(bad_priv$nc_handles, is.null, logical(1L))))
})

test_that("EsgDataset async open and var_get support concurrent local datasets", {
    skip_on_cran()
    paths <- dataset_test__shared_table_pair()
    start <- c(2L, 1L, 1L)
    count <- c(2L, 1L, 1L)
    results <- mirai_dataset_lapply(
        seq_along(paths),
        function(i, paths, start, count) {
            ds <- EsgDataset$new(paths[[i]])
            priv <- ds$.__enclos_env__$private
            on.exit(ds$close(), add = TRUE)
            ds$open(async = TRUE, timeout = dataset_async_timeout)
            ok_open <- ds$is_open &&
                identical(priv$async_state, "completed") &&
                is.null(priv$async_task)
            full <- as.numeric(ds$var_get("tas", collapse = TRUE))
            async_vals <- as.numeric(ds$var_get(
                "tas",
                start = start,
                count = count,
                collapse = TRUE,
                async = TRUE,
                timeout = dataset_async_timeout
            ))
            ok_read <- ds$is_open &&
                identical(priv$async_state, "completed") &&
                is.null(priv$async_task)
            list(
                ok = ok_open && ok_read,
                full = full,
                async_values = async_vals,
                sync_values = as.numeric(ds$var_get(
                    "tas",
                    start = start,
                    count = count,
                    collapse = TRUE
                ))
            )
        },
        paths = paths,
        start = start,
        count = count
    )
    expect_false(any(vapply(results, inherits, logical(1L), "try-error")))
    expect_true(all(vapply(results, `[[`, logical(1L), "ok")))
    expect_equal(
        lapply(results, `[[`, "full"),
        list(c(11, 12, 13), c(21, 22, 23))
    )
    expect_equal(
        lapply(results, `[[`, "async_values"),
        list(c(12, 13), c(22, 23))
    )
    expect_equal(
        lapply(results, `[[`, "async_values"),
        lapply(results, `[[`, "sync_values")
    )
})
# }}}
# dataset__detach_handles() / dataset__adopt_handles() {{{
test_that("dataset__detach_handles() / dataset__adopt_handles() transfer partially opened handles", {
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

    source <- EsgDataset$new(path_opened)
    source$open()
    handles <- dataset__detach_handles(source)
    on.exit(dataset__close_handles(path_opened, handles), add = TRUE)
    source_private <- source$.__enclos_env__$private

    expect_false(source$is_open)
    expect_true(all(vapply(source_private$nc_handles, is.null, logical(1L))))

    ds <- EsgDataset$new(c(path_opened, path_pending))
    dataset__adopt_handles(ds, list(handles[[1L]], NULL))
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
    failing_handles <- dataset__detach_handles(failing_source)
    failing <- EsgDataset$new(c(path_opened, missing_path))
    dataset__adopt_handles(failing, list(failing_handles[[1L]], NULL))
    failing_handles <- vector("list", length(failing_handles))
    failing_private <- failing$.__enclos_env__$private

    expect_error(failing$open(), "Failed to open OPeNDAP connection")
    expect_false(failing$is_open)
    expect_true(all(vapply(failing_private$nc_handles, is.null, logical(1L))))
})
# }}}
# EsgDataset$var_get() {{{
test_that("EsgDataset$var_get(async = TRUE) failures clear task state and keep sync handles usable", {
    skip_on_cran()
    path <- dataset_test__shared_table_pair()[[1L]]

    ds <- EsgDataset$new(path)
    ds$open()
    on.exit(ds$close(), add = TRUE)
    private <- ds$.__enclos_env__$private

    expect_error(
        ds$var_get(
            "missing_var",
            async = TRUE,
            timeout = dataset_async_timeout
        ),
        "Failed to read variable data"
    )

    expect_true(ds$is_open)
    expect_identical(private$async_state, "failed")
    expect_null(private$async_task)
    expect_equal(as.numeric(ds$var_get("tas", collapse = TRUE)), c(11, 12, 13))
})
# }}}
# private$collect_async_task() {{{
test_that("private$collect_async_task() surfaces timeout errors and clears lifecycle state", {
    path <- dataset_test__shared_table_pair()[[1L]]

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
# }}}
# EsgDataset$close() {{{
test_that("EsgDataset$close() best-effort cancels pending internal async work", {
    path <- dataset_test__shared_table_pair()[[1L]]

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
# }}}
# private$cancel_async_task() {{{
test_that("private$cancel_async_task() keeps cancelled terminal state", {
    skip_on_cran()
    path <- dataset_test__shared_table_pair()[[1L]]

    ds <- EsgDataset$new(path)
    private <- ds$.__enclos_env__$private

    task <- private$start_async_operation(
        operation = "cancel-race task",
        handler = function(urls, nc_handles) {
            Sys.sleep(5)
            TRUE
        },
        timeout = 10
    )

    # stop_mirai() is best-effort and may report FALSE when the dispatcher has
    # already delivered cancellation. The observable contract is the resolved
    # cancellation state checked below, not this timing-sensitive return value.
    mirai::stop_mirai(task$mirai_obj)
    while (mirai::unresolved(task$mirai_obj)) {
        Sys.sleep(0.01)
    }

    requested <- private$cancel_async_task()

    expect_false(requested)
    expect_identical(task$status, "cancelled")
    expect_true(inherits(task$error, "epwshiftr_async_cancelled"))
    expect_match(conditionMessage(task$error), "cancelled", fixed = TRUE)
    expect_identical(private$async_state, "cancelled")
    expect_true(task$backend_released)
    expect_null(private$async_task)
})
# }}}
# EsgDataset closed-dataset guards {{{
test_that("EsgDataset inquiry and read methods reject closed datasets", {
    ds <- EsgDataset$new("https://example.org/data.nc")
    expect_error(ds$get_variables(), "not open")
    expect_error(ds$get_dimensions(), "not open")
    expect_error(ds$file_inq(), "not open")
    expect_error(ds$var_inq("tas"), "not open")
    expect_error(ds$var_get("tas"), "not open")
})
# }}}
# EsgDataset$print() {{{
test_that("EsgDataset$print()", {
    ds <- EsgDataset$new("https://example.org/data.nc")

    expect_snapshot(print(ds))
})
# }}}

dataset_test__point_site <- function() {
    data.frame(site_id = "a", lon = 10.25, lat = .25)
}

dataset_test__assert_no_tas_read <- function(expr) {
    reads <- 0L
    reader <- RNetCDF::var.get.nc
    testthat::with_mocked_bindings(
        {
            force(expr)
            expect_identical(reads, 0L)
        },
        var.get.nc = function(...) {
            args <- list(...)
            if (length(args) >= 2L && identical(args[[2L]], "tas")) {
                reads <<- reads + 1L
            }
            reader(...)
        },
        .package = "RNetCDF"
    )
}

# Distinct native values detect lost corners, transposition and extra times.
test_that("raw neighborhoods retain four corners and share disjoint demand", {
    native <- expand.grid(time = 1:6, lat = 1:3, lon = 1:3)
    values <- array(
        native$time + 100 * native$lat + 10000 * native$lon,
        c(6, 3, 3)
    )
    values[1, 1, 1] <- NA_real_
    path <- dataset_test__write_nc(
        0:5,
        "days since 2060-01-01",
        calendar = "360_day",
        lat = 0:2,
        lon = 10:12,
        tas = values,
        bounds = cbind(0:5, 1:6),
        reversed = TRUE,
        path = withr::local_tempfile(fileext = ".nc")
    )
    ds <- dataset_test__open(path)
    sites <- data.frame(
        site_id = c("a", "copy", "b"),
        lon = c(10.25, 10.25, 11.25),
        lat = c(.25, .25, 1.25)
    )
    windows <- list(
        c("2060-01-01", "2060-01-01"),
        c("2060-01-04", "2060-01-04")
    )
    actual <- ds$read_neighborhood("tas", sites, time = windows)
    expect_equal(
        c(nrow(actual$neighborhoods), nrow(actual$values)),
        c(12L, 14L)
    )
    expect_equal(sort(unique(actual$values$time_index)), c(1L, 4L))
    expect_identical(unique(actual$time$cf_calendar), "360_day")
    expect_equal(actual$time$bound_start_value, actual$time$time_index - 1)
    expect_equal(actual$time$bound_end_value, actual$time$time_index)
    expected <- values[cbind(
        actual$values$time_index,
        actual$values$ind_lat,
        actual$values$ind_lon
    )]
    expect_equal(actual$values$value, expected)
    expect_equal(sum(actual$read_slices$native_values), 14)
    expect_true(anyNA(actual$values$value))
    expect_false("weight" %in% names(actual$neighborhoods))
    expect_identical(sites$site_id, c("a", "copy", "b"))

    chunks <- list()
    streamed <- ds$read_neighborhood(
        "tas",
        sites,
        time = windows,
        on_chunk = function(chunk) chunks[[length(chunks) + 1L]] <<- chunk
    )
    expect_equal(nrow(streamed$values), 0L)
    expect_equal(
        data.table::rbindlist(lapply(chunks, `[[`, "values")),
        actual$values
    )

    native_path <- withr::local_tempfile(fileext = ".nc")
    file.copy(path, native_path)
    dataset_test__edit_nc(native_path, function(nc) {
        RNetCDF::att.put.nc(
            nc,
            "time",
            "units",
            "NC_CHAR",
            "days since 2060-02-28"
        )
    })
    feb <- dataset_test__open(native_path)$read_neighborhood(
        "tas",
        sites[1L, ],
        time = c("2060-02-30", "2060-02-30")
    )
    expect_equal(nrow(feb$values), 4L)
    expect_identical(
        c(unique(feb$values$time_index), unique(feb$time$cf_day)),
        c(3L, 30L)
    )
    expect_error(
        ds$read_neighborhood("tas", transform(sites[1:2, ], site_id = "same")),
        "duplicated"
    )
})

# This local native 2x2 fixture exercises the public reader with both bounds
# dimension orders and per-variable temporal semantics, without remote I/O.
dataset_test__cf_point_fixture <- function(
    method = "time: point",
    calendar = "julian",
    width = 0,
    reversed = FALSE,
    units = "days since 1900-02-28",
    time_values = 0:2,
    bounds = NULL,
    extra_vars = NULL,
    extra_edit = NULL
) {
    dataset_test__write_nc(
        time_values,
        units,
        calendar = calendar,
        bounds = if (is.null(bounds)) {
            cbind(time_values, time_values + width)
        } else {
            bounds
        },
        reversed = reversed,
        method = method,
        extra_vars = extra_vars,
        extra_edit = extra_edit
    )
}

# Bounds cached for a point variable must not authorize another variable's
# averages. Native durations remain authoritative below POSIX precision.
test_that("instantaneous native bounds preserve Julian corners and strict variable validation", {
    sites <- dataset_test__point_site()
    for (reversed in c(FALSE, TRUE)) {
        ds <- dataset_test__open_temp(
            dataset_test__cf_point_fixture(reversed = reversed)
        )
        # Populate the shared bounds cache before the variable-specific read.
        axis <- ds$get_time_axis()
        expect_equal(as.numeric(axis$bounds$end), as.numeric(axis$bounds$start))
        actual <- ds$read_neighborhood(
            "tas",
            sites,
            time = c("1900-02-29", "1900-02-29")
        )
        expect_equal(nrow(actual$values), 4L)
        expect_equal(sort(actual$values$value), c(272, 275, 278, 281))
        expect_identical(unique(actual$time$cf_day), 29L)
        expect_identical(unique(actual$time$cf_calendar), "julian")
        expect_equal(actual$time$bound_start_value, rep(1, nrow(actual$time)))
        expect_equal(actual$time$bound_end_value, actual$time$bound_start_value)
    }

    neg_ds <- dataset_test__open_temp(
        dataset_test__cf_point_fixture(width = -1, calendar = "standard")
    )
    expect_error(neg_ds$get_time_axis(), "increasing|non-decreasing")

    ds_var <- dataset_test__open_temp(
        dataset_test__cf_point_fixture(
            calendar = "standard",
            extra_vars = list(
                tas_mean = list(
                    method = "time: mean",
                    values = array(280, c(3, 2, 2))
                ),
                tas_sum = list(method = "time: sum"),
                tas_null = list(method = NULL),
                tas_both = list(method = "time: point time: mean")
            )
        )
    )
    expect_equal(nrow(ds_var$read_neighborhood("tas", sites)$values), 12L)
    expect_error(
        ds_var$read_neighborhood("tas_mean", sites),
        "positive intervals"
    )
    for (var_name in c("tas_mean", "tas_sum", "tas_null", "tas_both")) {
        expect_error(
            ds_var$read_neighborhood(var_name, sites),
            "positive|increasing"
        )
    }

    short_ds <- dataset_test__open_temp(
        dataset_test__cf_point_fixture(
            calendar = "standard",
            method = "time: mean",
            width = 1e-10
        )
    )
    short_out <- short_ds$read_neighborhood("tas", sites)
    expect_equal(nrow(short_out$values), 12L)
    expect_true(all(
        short_out$time$bound_end_value > short_out$time$bound_start_value
    ))

    wrong <- dataset_test__cf_point_fixture(
        calendar = "standard",
        bounds = cbind(1:3, 1:3)
    )
    expect_error(
        dataset_test__open_temp(wrong)$read_neighborhood("tas", sites),
        "equal their native time coordinate"
    )
})

# Public NetCDF reads must preserve the exact source coordinates and bounds
# while selecting four native corners with corrected UTC and calendar fields.
test_that("CF normalization preserves public native offsets bounds and selections", {
    sites <- dataset_test__point_site()
    cases <- list(
        list(
            units = "hours since 2000-01-01 00:00:00 +02:00",
            offsets = 0:2,
            range = c("1999-12-31T22:00:00", "2000-01-01"),
            days = c(31L, 31L, 1L)
        ),
        list(
            units = "days since 1582-10-04",
            offsets = 0:2,
            range = c("1582-10-04", "1582-10-16"),
            days = c(4L, 15L, 16L)
        ),
        list(
            units = "months since 2000-01-31",
            offsets = 0:2,
            range = c("2000-01-31", "2000-04-02"),
            days = c(31L, 1L, 31L)
        ),
        list(
            units = "seconds since 2000-01-01",
            offsets = c(0, .000002, .000004),
            range = c(
                "2000-01-01T00:00:00.000001",
                "2000-01-01T00:00:00.000003"
            ),
            days = 1L
        )
    )
    for (case in cases) {
        ds <- dataset_test__open_temp(dataset_test__cf_point_fixture(
            calendar = "standard",
            units = case$units,
            time_values = case$offsets
        ))
        axis <- ds$get_time_axis()
        expect_identical(as.numeric(axis$raw_values), as.numeric(case$offsets))
        expect_identical(
            as.numeric(axis$bounds$raw_values),
            as.numeric(cbind(case$offsets, case$offsets))
        )
        expect_identical(axis$units, case$units)
        result <- ds$read_neighborhood("tas", sites, time = case$range)
        expect_equal(nrow(result$values), 4L * length(case$days))
        expect_identical(result$time$cf_day, case$days)
        expect_equal(result$time$bound_start_value, result$time$time_value)
        expect_equal(result$time$bound_end_value, result$time$time_value)
        expect_identical(unique(result$time$time_units), case$units)
    }
})

# Encode the CF seasonal-climatology example as a local native 2x2 file. Raw
# boundaries retain both dimension orders and are never inferred from spacing.
dataset_test__climatology_fixture <- function(
    method = "time: minimum within years time: mean over years",
    calendar = "standard",
    reversed = FALSE,
    units = "days since 1960-01-01",
    times = NULL,
    bounds = NULL,
    bounds_attr = "climatology",
    extra_vars = NULL,
    extra_edit = NULL
) {
    if (is.null(times) || is.null(bounds)) {
        origin <- cf_time_parse_origin("1960-01-01", calendar)
        to_off <- function(y, m, d) {
            cf_time_date2offset(
                data.frame(year = y, month = m, day = d),
                origin,
                calendar
            )
        }
        if (is.null(times)) {
            times <- to_off(
                c(1960L, 1960L, 1960L, 1961L),
                c(4L, 7L, 10L, 1L),
                16L
            )
        }
        if (is.null(bounds)) {
            bounds <- cbind(
                to_off(1960L, c(3L, 6L, 9L, 12L), 1L),
                to_off(c(1990L, 1990L, 1990L, 1991L), c(6L, 9L, 12L, 3L), 1L)
            )
        }
    }
    dataset_test__write_nc(
        times,
        units,
        calendar = calendar,
        bounds = bounds,
        bounds_attr = bounds_attr,
        bounds_var = "climatology_bounds",
        bounds_dim = "nv",
        reversed = reversed,
        method = method,
        extra_vars = extra_vars,
        extra_edit = extra_edit
    )
}

# Reading a precomputed climatology preserves its statistics, representative
# labels and discontinuous annual cells instead of inventing ordinary intervals.
test_that("CF climatology exposes seasonal bounds and ordered statistics", {
    sites <- dataset_test__point_site()
    cases <- list(
        list(calendar = "standard", reversed = FALSE),
        list(calendar = "standard", reversed = TRUE),
        list(calendar = "julian", reversed = FALSE),
        list(calendar = "360_day", reversed = TRUE),
        list(calendar = "365_day", reversed = FALSE)
    )
    for (case in cases) {
        calendar <- case$calendar
        reversed <- case$reversed
        ds <- dataset_test__open_temp(
            dataset_test__climatology_fixture(
                calendar = calendar,
                reversed = reversed
            )
        )
        axis <- ds$get_time_axis(variable = "tas")
        expect_identical(axis$kind, "climatology")
        expect_null(axis$bounds)
        expect_identical(axis$climatology$name, "climatology_bounds")
        expect_identical(
            axis$statistics$operations$method,
            c("minimum", "mean")
        )
        expect_identical(
            axis$statistics$operations$scope,
            c("within years", "over years")
        )
        expect_identical(
            axis$climatology$interpretation$wraps_year,
            c(FALSE, FALSE, FALSE, TRUE)
        )
        expect_equal(
            dim(axis$climatology$raw_values),
            if (reversed) c(2L, 4L) else c(4L, 2L)
        )
        actual <- ds$read_neighborhood(
            "tas",
            sites,
            time = c("1960-01-01", "1961-12-31")
        )
        expect_equal(sort(actual$values$value), as.double(271:286))
        expect_identical(unique(actual$time$time_kind), "climatology")
        expect_false("time_bound_start" %in% names(actual$time))
        native <- if (reversed) {
            t(axis$climatology$raw_values)
        } else {
            axis$climatology$raw_values
        }
        expect_equal(
            actual$time$climatology_bound_start_value,
            native[, 1L]
        )
        expect_equal(actual$time$climatology_bound_end_value, native[, 2L])
        expect_equal(actual$time$time_value, as.double(axis$raw_values))
        expect_identical(
            unique(actual$time$climatology_cell_methods),
            axis$statistics$raw
        )
        table <- ds$read_data_table("tas", rbind = TRUE)
        expect_identical(unique(table$time_kind), "climatology")
        expect_equal(sort(table$tas), as.double(271:286))
        expect_equal(
            sort(unique(table$time_value)),
            sort(as.double(axis$raw_values))
        )
        expect_equal(
            unique(table$climatology_bound_start_value),
            native[, 1L]
        )
        expect_error(
            ds$read_region(
                "tas",
                lon = 10.25,
                lat = .25,
                time = c("1960-01-01", "1961-12-31")
            ),
            "climatolog"
        )
    }
})

# Daily and combined climatologies preserve native clock ranges and separate
# annual day spans from daily wraps.
test_that("CF climatology interprets daily, annual, and combined statistical cycles", {
    bounds <- rbind(c(0, 30 * 24), c(23, 30 * 24 + 1))
    for (method in c(
        "time: mean within days time: mean over days",
        "area: mean time: mean within days (interval: 1 hour) time: sum over days time: mean over years"
    )) {
        ds <- dataset_test__open_temp(dataset_test__climatology_fixture(
            method = method,
            units = "hours since 1995-04-01",
            times = c(12, 24),
            bounds = bounds
        ))
        axis <- ds$get_time_axis(variable = "tas")
        expect_identical(
            axis$climatology$interpretation$full_day,
            c(TRUE, FALSE)
        )
        expect_identical(
            axis$climatology$interpretation$wraps_day,
            c(FALSE, TRUE)
        )
        expect_identical(axis$statistics$raw, method)
        expect_equal(
            as.numeric(axis$climatology$raw_values),
            as.numeric(bounds)
        )
    }

    combined <- dataset_test__open_temp(dataset_test__climatology_fixture(
        method = "time: mean within days time: sum over days time: mean over years"
    ))$get_time_axis(variable = "tas")$climatology$interpretation
    expect_identical(combined$wraps_year, c(FALSE, FALSE, FALSE, TRUE))
    expect_identical(combined$full_day, rep(TRUE, 4L))
    expect_identical(combined$wraps_day, rep(FALSE, 4L))
})

# A shared coordinate cache must never reuse another variable's statistical
# operator sequence, and each raw clock must remain joinable to its own values.
test_that("CF climatology keeps statistics per variable through shared caches", {
    path <- dataset_test__climatology_fixture(
        extra_vars = list(
            tasmax = list(
                method = "time: mean within years time: maximum over years",
                values = array(as.double(301:316), c(4L, 2L, 2L))
            )
        )
    )
    ds <- dataset_test__open_temp(path)
    expect_identical(
        ds$get_time_axis(variable = "tas")$statistics$operations$method,
        c("minimum", "mean")
    )
    expect_identical(
        ds$get_time_axis(variable = "tasmax")$statistics$operations$method,
        c("mean", "maximum")
    )
    raw <- ds$read_neighborhood(c("tas", "tasmax"), dataset_test__point_site())
    expect_identical(sort(unique(raw$time$variable)), c("tas", "tasmax"))
    joined <- merge(
        raw$values,
        raw$time,
        by = c("file_index", "time_index", "variable")
    )
    expect_equal(nrow(joined), 32L)
    expect_equal(
        sort(joined$value[joined$variable == "tas"]),
        as.double(271:286)
    )
    expect_equal(
        sort(joined$value[joined$variable == "tasmax"]),
        as.double(301:316)
    )
    expect_true(all(grepl(
        "mean within years",
        joined$climatology_cell_methods[joined$variable == "tasmax"],
        fixed = TRUE
    )))
})

# Malformed statistical declarations, conflicting attributes/dimensions/numbers
# must fail before any weather values are read.
test_that("CF climatology validates statistical declarations and boundary identity before weather reads", {
    sites <- dataset_test__point_site()
    bad_methods <- list(
        tas_null = list(method = NULL),
        tas_mean = list(method = "time: mean"),
        tas_rev = list(
            method = "time: mean over years time: mean within years"
        ),
        tas_banana = list(
            method = "time: banana within years time: mean over years"
        ),
        tas_mixed = list(
            method = "time: mean within days time: mean over years"
        )
    )
    ds_methods <- dataset_test__open_temp(
        dataset_test__climatology_fixture(extra_vars = bad_methods)
    )
    for (var_name in names(bad_methods)) {
        expect_error(
            ds_methods$get_time_axis(variable = var_name),
            "climatolog|cell_methods"
        )
        expect_error(
            ds_methods$read_neighborhood(var_name, sites),
            "climatolog|cell_methods"
        )
    }

    both_path <- dataset_test__climatology_fixture(
        extra_edit = function(nc) {
            RNetCDF::att.put.nc(
                nc,
                "time",
                "bounds",
                "NC_CHAR",
                "climatology_bounds"
            )
        }
    )
    expect_error(
        dataset_test__open_temp(both_path)$get_time_axis(),
        "climatolog.*bounds|bounds.*climatolog"
    )

    att_edits <- list(
        list(
            "climatology_bounds",
            "units",
            "NC_CHAR",
            "hours since 1960-01-01"
        ),
        list("climatology_bounds", "calendar", "NC_CHAR", "360_day"),
        list("time", "climatology", "NC_CHAR", "missing_boundary"),
        list("time", "climatology", "NC_CHAR", " "),
        list("time", "climatology", "NC_INT", 1L),
        list("time", "climatology", "NC_CHAR", "lat")
    )
    val_edits <- list(
        matrix(c(2, rep(1, 7)), 4L, 2L),
        matrix(c(NA_real_, rep(1, 7)), 4L, 2L)
    )
    for (ed in c(
        lapply(att_edits, function(a) {
            function(nc) {
                RNetCDF::att.put.nc(nc, a[[1L]], a[[2L]], a[[3L]], a[[4L]])
            }
        }),
        lapply(val_edits, function(m) {
            function(nc) RNetCDF::var.put.nc(nc, "climatology_bounds", m)
        })
    )) {
        ds <- dataset_test__open_temp(
            dataset_test__climatology_fixture(extra_edit = ed)
        )
        dataset_test__assert_no_tas_read({
            expect_error(ds$get_time_axis(variable = "tas"))
            expect_error(ds$read_neighborhood("tas", sites))
        })
    }

    deleted <- dataset_test__climatology_fixture(
        extra_edit = function(nc) {
            RNetCDF::att.delete.nc(nc, "time", "climatology")
        }
    )
    ds_del <- dataset_test__open_temp(deleted)
    expect_error(ds_del$get_time_axis(variable = "tas"), "climatolog")
    expect_error(ds_del$read_neighborhood("tas", sites), "climatolog")
})

# Attribute read failures cannot erase a declared climatology association, and
# custom variable names colliding with metadata columns must not be overwritten.
test_that("CF climatology propagates attribute failures and isolates source value columns", {
    col_names <- c("variable", "time_kind", "climatology_bound_start")
    expected <- c(
        "variable_value",
        "time_kind_value",
        "climatology_bound_start_value_value"
    )
    extra_cols <- stats::setNames(
        lapply(col_names, function(...) {
            list(method = "time: minimum within years time: mean over years")
        }),
        col_names
    )
    ds <- dataset_test__open_temp(
        dataset_test__climatology_fixture(extra_vars = extra_cols)
    )
    expect_identical(dataset__time_kind(ds, 1L), "climatology")
    reader <- RNetCDF::att.get.nc
    testthat::with_mocked_bindings(
        expect_error(
            ds$get_time_axis(variable = "tas"),
            "declared attribute I/O failure"
        ),
        att.get.nc = function(ncfile, variable, attribute, ...) {
            if (
                identical(variable, "time") &&
                    identical(attribute, "climatology")
            ) {
                stop("declared attribute I/O failure")
            }
            reader(ncfile, variable, attribute, ...)
        },
        .package = "RNetCDF"
    )

    for (i in seq_along(col_names)) {
        expect_warning(
            table <- ds$read_data_table(col_names[[i]], rbind = TRUE),
            "conflicts"
        )
        expect_equal(sort(table[[expected[[i]]]]), as.double(271:286))
        expect_identical(unique(table$time_kind), "climatology")
    }
})

# Grouped cell-method headers keep axis names case-sensitive and method keywords
# case-insensitive, while rejecting bare prefix tokens and stray identifiers.
test_that("CF review parses grouped point and climatology cell_methods strictly", {
    sites <- dataset_test__point_site()
    valid_point <- c(
        "time: lat: point",
        "lat: time: POINT",
        "time: lat: point lon: mean"
    )
    invalid_point <- c(
        "TIME: point",
        "lat: TIME: point",
        "time: lat: mean",
        "time: point time: mean",
        "time: point-invalid",
        "time: lat: point-invalid",
        "lon: mean not-time: point",
        "lon: mean not.time: point"
    )
    point_vars <- stats::setNames(
        lapply(c(valid_point, invalid_point), function(m) list(method = m)),
        c(
            paste0("pt_ok_", seq_along(valid_point)),
            paste0("pt_bad_", seq_along(invalid_point))
        )
    )
    ds_point <- dataset_test__open_temp(
        dataset_test__cf_point_fixture(
            calendar = "standard",
            extra_vars = point_vars
        )
    )
    for (i in seq_along(valid_point)) {
        actual <- ds_point$read_neighborhood(paste0("pt_ok_", i), sites)
        expect_equal(nrow(actual$values), 12L)
        expect_identical(actual$time$bound_start_value, actual$time$time_value)
        expect_identical(actual$time$bound_end_value, actual$time$time_value)
    }
    for (i in seq_along(invalid_point)) {
        expect_error(
            ds_point$read_neighborhood(paste0("pt_bad_", i), sites),
            "point|cell_methods"
        )
    }

    invalid_clim <- c(
        "TIME: mean within years TIME: mean over years",
        "garbage time: mean within years time: mean over years",
        "lon: mean not-time: minimum within years time: mean over years",
        "lon: mean not.time: minimum within years time: mean over years"
    )
    mixed <- "time: MINIMUM Within Years time: MEAN Over Years"
    clim_vars <- stats::setNames(
        lapply(c(invalid_clim, mixed), function(m) list(method = m)),
        c(paste0("clim_bad_", seq_along(invalid_clim)), "clim_mixed")
    )
    ds_clim <- dataset_test__open_temp(
        dataset_test__climatology_fixture(extra_vars = clim_vars)
    )
    for (i in seq_along(invalid_clim)) {
        expect_error(
            ds_clim$get_time_axis(variable = paste0("clim_bad_", i)),
            "cell_methods|climatolog"
        )
    }
    statistics <- ds_clim$get_time_axis(variable = "clim_mixed")$statistics
    expect_identical(statistics$raw, mixed)
    expect_identical(statistics$operations$method, c("minimum", "mean"))
})

# Normalized endpoint columns retain provenance to reconstruct bounds losslessly
# and refuse a replica with contradictory boundary layout before weather reads.
test_that("CF review preserves climatology boundary dimensions and refuses contradictory replicas", {
    sites <- dataset_test__point_site()
    datasets <- vector("list", 2L)
    for (idx in seq_len(2L)) {
        reversed <- idx == 2L
        ds <- dataset_test__open_temp(
            dataset_test__climatology_fixture(reversed = reversed)
        )
        datasets[[idx]] <- ds
        axis <- ds$get_time_axis(variable = "tas")
        expected <- if (reversed) c("nv", "time") else c("time", "nv")
        expect_identical(axis$climatology$dimensions, expected)
        raw <- ds$read_neighborhood("tas", sites)
        expect_identical(
            unique(raw$bounds$bounds_time_dimension),
            match("time", expected)
        )
        expect_identical(
            jsonlite::fromJSON(raw$bounds$bounds_dimensions[[1L]]),
            expected
        )
        expect_identical(
            unique(raw$time$climatology_bounds_time_dimension),
            match("time", expected)
        )
        expect_identical(
            jsonlite::fromJSON(raw$time$climatology_bounds_dimensions[[1L]]),
            expected
        )
        reconstructed <- cbind(
            raw$bounds$bound_start_value,
            raw$bounds$bound_end_value
        )
        if (reversed) {
            reconstructed <- t(reconstructed)
        }
        expect_identical(reconstructed, axis$climatology$raw_values)
        expect_identical(
            unique(
                ds$read_data_table(
                    "tas",
                    rbind = TRUE
                )$climatology_bounds_time_dimension
            ),
            match("time", expected)
        )
    }

    captured <- NULL
    datasets[[1L]]$read_neighborhood("tas", sites, on_chunk = function(chunk) {
        captured <<- chunk
    })
    dataset_test__assert_no_tas_read(
        expect_error(
            dataset__read_neighborhood(
                datasets[[2L]],
                "tas",
                sites,
                time = NULL,
                on_chunk = NULL,
                expected_contract = captured$contract,
                expected_bounds = captured$bounds
            ),
            "identity"
        )
    )
})

# CMIP6 monthly extrema use ordinary contiguous bounds, even though their
# ordered cell methods contain within-days and over-days statistics.
test_that("CMIP6 monthly statistics retain ordinary bounds and raw methods", {
    sites <- dataset_test__point_site()
    offsets <- c(60280.5, 60310, 60339.5)
    endpoints <- cbind(c(60265, 60296, 60324), c(60296, 60324, 60355))
    methods <- c(
        tas = "area: mean time: maximum within days time: mean over days",
        tasmin = "area: mean time: minimum within days time: mean over days"
    )
    path <- dataset_test__climatology_fixture(
        method = methods[["tas"]],
        calendar = "proleptic_gregorian",
        units = "days since 1850-01-01",
        times = offsets,
        bounds = endpoints,
        bounds_attr = "bounds",
        extra_vars = list(tasmin = list(method = methods[["tasmin"]]))
    )
    ds <- dataset_test__open_temp(path)
    for (var_name in names(methods)) {
        method <- methods[[var_name]]
        axis <- ds$get_time_axis(variable = var_name)
        expect_identical(axis$kind, "ordinary")
        expect_null(axis$climatology)
        expect_identical(as.double(axis$raw_values), offsets)
        expect_equal(axis$bounds$raw_values, endpoints)
        expect_identical(axis$statistics$raw, method)
        expect_identical(
            axis$statistics$operations$scope,
            c("within days", "over days")
        )
        raw <- ds$read_neighborhood(var_name, sites)
        expect_equal(nrow(raw$values), 12L)
        expect_identical(unique(raw$values$cell_methods), method)
        expect_identical(raw$time$time_value, offsets)
        expect_equal(raw$time$bound_start_value, endpoints[, 1L])
        expect_equal(raw$time$bound_end_value, endpoints[, 2L])
        expect_false(any(startsWith(names(raw$time), "climatology_")))
    }

    ds$close()
    dataset_test__edit_nc(path, function(nc) {
        RNetCDF::att.delete.nc(nc, "time", "bounds")
    })
    ds$open()
    for (var_name in names(methods)) {
        expect_error(
            ds$get_time_axis(variable = var_name),
            "explicit time bounds"
        )
        expect_error(
            ds$read_neighborhood(var_name, sites),
            "explicit time bounds"
        )
    }
    # Year aggregation without a climatology declaration remains ambiguous.
    expect_error(
        dataset__parse_time_statistics(
            "time: mean within years time: mean over years"
        ),
        "climatology"
    )
})

# Exercise all spatial areas against one real local NetCDF axis. Distinct
# values at every cell also detect reshaping or ordering errors across blocks.
test_that("regional reads preserve values across long time axes and site groups", {
    values <- array(seq_len(81000L), dim = c(9000L, 3L, 3L))
    path <- dataset_test__write_nc(
        0:8999,
        "days since 2060-01-01",
        calendar = "365_day",
        lat = 0:2,
        lon = 0:2,
        tas = values,
        path = withr::local_tempfile(fileext = ".nc")
    )
    dataset <- dataset_test__open(path)
    sites <- data.table::data.table(
        site_id = letters[1:5],
        lon = c(0, 1, 0, 1, 2),
        lat = c(0, 0, 1, 1, 2),
        method = "nearest"
    )
    # Exercise different spatial shapes with actual local NetCDF reads.
    for (size in c(1L, 2L, 4L, 5L)) {
        actual <- dataset__read_regions(dataset, "tas", sites[seq_len(size)])
        slices <- attr(actual, "read_slices")
        expect_true(all(
            slices$time_count * slices$lat_count * slices$lon_count <=
                DATASET_REQUEST_MAX_VALUES
        ))
        for (site in seq_len(size)) {
            expected <- values[, sites$lat[[site]] + 1L, sites$lon[[site]] + 1L]
            expect_identical(
                actual$value[actual$site_id == sites$site_id[[site]]],
                as.numeric(expected)
            )
        }
    }
})

# Metadata bounds, coordinate reuse, and backend isolation {{{
# Check real native reads, including bounds-first and time-first CF layouts.
test_that("selected CF bounds preserve native indices and dimension order", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    withr::defer(unlink(path))
    nc <- RNetCDF::open.nc(path, write = TRUE)
    raw <- RNetCDF::var.get.nc(nc, "time_bnds", collapse = FALSE)
    RNetCDF::var.def.nc(nc, "alternate_bounds", "NC_DOUBLE", c("time", "bnds"))
    RNetCDF::var.put.nc(nc, "alternate_bounds", t(raw), count = rev(dim(raw)))
    RNetCDF::close.nc(nc)
    ds <- EsgDataset$new(path)
    ds$open()
    withr::defer(ds$close())
    full <- ds$get_time_axis()
    selected <- c(1L, 2L, 30L, 60L, 360L)
    subset <- dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        selected
    )
    expect_equal(subset$start[seq_along(selected)], full$bounds$start[selected])
    expect_equal(subset$end[seq_along(selected)], full$bounds$end[selected])
    expect_equal(
        data.table::as.data.table(subset$start_coordinates),
        data.table::as.data.table(full$bounds$start_coordinates[selected, ])
    )
    expect_null(dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        integer()
    ))
    expect_error(dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        c(1L, 1L)
    ))
    expect_error(dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        NA_integer_
    ))
    expect_error(dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        361L
    ))
    ds$close()
    nc <- RNetCDF::open.nc(path, write = TRUE)
    RNetCDF::att.put.nc(nc, "time", "bounds", "NC_CHAR", "alternate_bounds")
    RNetCDF::close.nc(nc)
    ds$open()
    reordered <- dataset__time_bounds(
        ds,
        1L,
        full$units,
        full$calendar,
        full$length,
        selected
    )
    expect_equal(reordered$start, subset$start)
    expect_equal(reordered$end, subset$end)
})

# Instrument the NetCDF wrapper to ensure the optimized path requests only
# selected bounds; the public full-axis method still reads all intervals.
test_that("multi-site reads fetch only selected CF bounds", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "365_day")
    withr::defer(unlink(path))
    ds <- EsgDataset$new(path)
    ds$open()
    withr::defer(ds$close())
    original <- RNetCDF::var.get.nc
    bounds_counts <- list()
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.get.nc = function(ncfile, variable, ...) {
            arguments <- list(...)
            if (identical(variable, "time_bnds")) {
                bounds_counts[[length(bounds_counts) + 1L]] <<- arguments$count
            }
            original(ncfile, variable, ...)
        }
    )
    sites <- data.table::data.table(
        site_id = c("first", "last"),
        lon = c(104, 254),
        lat = c(1, 41),
        method = "nearest",
        time_start = as.POSIXct(c("2060-01-01", "2060-12-31"), tz = "UTC"),
        time_stop = as.POSIXct(
            c("2060-01-01 23:59:59", "2060-12-31 23:59:59"),
            tz = "UTC"
        )
    )
    result <- dataset__read_regions(ds, "tas", sites)
    expect_equal(nrow(result), 2L)
    expect_equal(bounds_counts, list(c(2L, 1L), c(2L, 1L)))
    full <- ds$get_time_axis()
    expect_equal(length(full$bounds$start), 365L)
    expect_equal(tail(bounds_counts, 1L), list(c(2L, 365L)))
    expect_equal(result$time_bound_start, full$bounds$start[c(1L, 365L)])
    empty <- dataset__read_regions(
        ds,
        "tas",
        sites[, c("site_id", "lon", "lat", "method"), with = FALSE],
        time = c("2059-01-01", "2059-01-31")
    )
    expect_equal(nrow(empty), 0L)
    expect_length(bounds_counts, 3L)
})

# Coordinate reuse belongs to the opened dataset object. A separate object
# reads its own native coordinates without publishing a cross-batch cache.
test_that("native coordinates are reused only within a dataset object", {
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    withr::defer(unlink(path))
    root <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_cache = root))
    original <- RNetCDF::var.get.nc
    reads <- character()
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.get.nc = function(ncfile, variable, ...) {
            reads <<- c(reads, variable)
            original(ncfile, variable, ...)
        }
    )
    ds <- EsgDataset$new(path)
    ds$open()
    withr::defer(ds$close())
    axis <- dataset__time_axis(ds)
    grid <- ds$get_spatial_grid()
    expect_equal(reads, c("time", "lat", "lon"))
    expect_null(axis$bounds)
    reads <- character()
    expect_identical(dataset__time_axis(ds), axis)
    expect_identical(ds$get_spatial_grid(), grid)
    expect_length(reads, 0L)

    other <- EsgDataset$new(path)
    other$open()
    withr::defer(other$close())
    expect_identical(dataset__time_axis(other), axis)
    expect_identical(other$get_spatial_grid(), grid)
    expect_equal(reads, c("time", "lat", "lon"))
    expect_false(dir.exists(file.path(root, "source-metadata")))
})

# Bound metadata is shared across the acquisition's value windows, so a long
# period does not add one metadata round trip per window.
test_that("batch prefetch shares selected bounds across value windows", {
    cache <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_cache = cache))
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(
        path,
        2060L,
        calendar = "365_day",
        n_years = 24L
    )
    withr::defer(unlink(path))
    acquisition <- data.table::data.table(
        acquisition_id = "twenty-four-years",
        physical_file_id = "twenty-four-years",
        filename = basename(path),
        variable_id = "tas",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        grid_label = "gr",
        frequency = "day",
        table_id = "day",
        version = "v1",
        tracking_id = "fixture",
        checksum = store_hash_file(path, "sha256"),
        checksum_type = "sha256",
        url_opendap = path,
        url_download = path,
        time_start = as.POSIXct("2060-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2083-12-31 23:59:59", tz = "UTC")
    )
    consumer <- data.table::data.table(
        acquisition_id = "twenty-four-years",
        demand_id = "site",
        child_key = "site",
        site_id = "site",
        role = "future",
        variable_id = "tas",
        lon = 104,
        lat = 1,
        spatial_method = "nearest",
        time_start = acquisition$time_start,
        time_stop = acquisition$time_stop,
        requested_start = acquisition$time_start,
        requested_stop = acquisition$time_stop
    )
    original <- RNetCDF::var.get.nc
    bounds_counts <- list()
    testthat::local_mocked_bindings(
        .package = "RNetCDF",
        var.get.nc = function(ncfile, variable, ...) {
            if (identical(variable, "time_bnds")) {
                bounds_counts[[length(bounds_counts) + 1L]] <<- list(...)$count
            }
            original(ncfile, variable, ...)
        }
    )
    expect_equal(
        shift_batch_window__prefetch_acquisition(
            withr::local_tempdir(),
            acquisition,
            consumer
        ),
        2L
    )
    expect_equal(bounds_counts, list(c(2L, 4096L), c(2L, 4096L), c(2L, 568L)))

    expect_false(dir.exists(file.path(cache, "source-metadata")))
})

# Ordinary point reads must retain real, irregular intervals while avoiding
# bounds outside the requested window. Repeated cities reuse the same subset.
test_that("point reads reuse selected actual bounds without loading the full axis", {
    skip_on_cran()
    for (calendar in c("365_day", "360_day", "proleptic_gregorian")) {
        path <- tempfile(fileext = ".nc")
        write_local_cmip6_netcdf_fixture(path, 2060L, calendar = calendar)
        withr::defer(unlink(path))
        nc <- RNetCDF::open.nc(path, write = TRUE)
        raw <- RNetCDF::var.get.nc(nc, "time_bnds", collapse = FALSE)
        # Deliberately nonuniform intervals rule out reconstruction from spacing.
        raw[1L, 2:3] <- raw[1L, 2:3] + c(0.1, 0.2)
        RNetCDF::var.put.nc(nc, "time_bnds", raw)
        RNetCDF::close.nc(nc)
        ds <- EsgDataset$new(path)
        ds$open()
        withr::defer(ds$close())
        original <- RNetCDF::var.get.nc
        counts <- list()
        testthat::local_mocked_bindings(
            .package = "RNetCDF",
            var.get.nc = function(ncfile, variable, ...) {
                if (identical(variable, "time_bnds")) {
                    counts[[length(counts) + 1L]] <<- list(...)$count
                }
                original(ncfile, variable, ...)
            }
        )
        window <- c("2060-01-02", "2060-01-03 23:59:59")
        actual <- ds$read_region("tas", lon = 104, lat = 1, time = window)
        expect_equal(counts, list(c(2L, 2L)))
        expect_equal(nrow(actual), 2L)
        again <- ds$read_region("tas", lon = 254, lat = 41, time = window)
        expect_equal(again$time_bound_start, actual$time_bound_start)
        expect_length(counts, 1L)
        ds$read_region(
            "tas",
            lon = 104,
            lat = 1,
            time = c("2060-01-03", "2060-01-03 23:59:59")
        )
        expect_length(counts, 1L)
        empty <- ds$read_region(
            "tas",
            lon = 104,
            lat = 1,
            time = c("2059-01-01", "2059-01-02")
        )
        expect_equal(nrow(empty), 0L)
        expect_length(counts, 1L)
        full <- ds$get_time_axis()
        expect_equal(tail(counts, 1L), list(c(2L, ncol(raw))))
        expected <- ds$read_region("tas", lon = 104, lat = 1, time = window)
        expect_identical(actual, expected)
        expect_equal(actual$time_bound_start, full$bounds$start[2:3])
        expect_length(counts, 2L)
        ds$close()
        if (identical(calendar, "proleptic_gregorian")) {
            nc <- RNetCDF::open.nc(path, write = TRUE)
            raw[1L, 2L] <- raw[1L, 2L] + 0.1
            RNetCDF::var.put.nc(nc, "time_bnds", raw)
            RNetCDF::close.nc(nc)
            ds$open()
            reopened <- ds$read_region("tas", lon = 104, lat = 1, time = window)
            expect_false(identical(
                reopened$time_bound_start,
                actual$time_bound_start
            ))
            expect_equal(tail(counts, 1L), list(c(2L, 2L)))
            expect_length(counts, 3L)
            ds$close()
        }
        # Restore between calendars; do not stack wrappers through loop bindings.
        testthat::local_mocked_bindings(
            .package = "RNetCDF",
            var.get.nc = original
        )
    }
})

# Independent Dataset objects must not reset each other's compute profiles.
test_that("concurrent dataset tasks in one process retain separate backends", {
    skip_on_cran()
    datasets <- lapply(seq_len(2L), function(index) EsgDataset$new("unused.nc"))
    on.exit(lapply(datasets, function(dataset) dataset$close()), add = TRUE)
    tasks <- lapply(seq_along(datasets), function(index) {
        private <- datasets[[index]]$.__enclos_env__$private
        # No remote I/O is needed to exercise the task ownership boundary.
        private$urls <- character()
        private$start_async_operation(
            "return task identity",
            function(urls, nc_handles, value) {
                Sys.sleep(0.1)
                value
            },
            handler_args = list(value = index),
            # The ownership contract has no startup-speed requirement; fresh
            # workers may take longer to initialize on slower machines.
            timeout = 60
        )
    })
    expect_false(identical(
        tasks[[1L]]$compute_profile,
        tasks[[2L]]$compute_profile
    ))
    expect_identical(tasks[[2L]]$collect(), 2L)
    expect_identical(tasks[[1L]]$collect(), 1L)
    expect_true(all(vapply(
        tasks,
        function(task) task$backend_released,
        logical(1L)
    )))
})
# }}}

# Loopback OPeNDAP transport smoke tests {{{
# Use a real loopback DAP2 server so the transport tests are independent
# of public data hosts. The dedicated ESGF service tests retain live coverage.
test_that("EsgDataset$new() / EsgDataset$open() / EsgDataset$file_inq() / EsgDataset$var_inq() / EsgDataset$dim_inq() / EsgDataset$att_get() / EsgDataset$get_variables() / EsgDataset$get_dimensions() / EsgDataset$get_time_axis() / EsgDataset$get_spatial_grid() / EsgDataset$var_get() / EsgDataset$read_array() / EsgDataset$read_data_table() loopback OPeNDAP single-file smoke", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(
        subset_test__dap_app(calendar = "standard")
    )
    on.exit(server$stop(), add = TRUE)

    ds <- EsgDataset$new(paste0(server$url(), "fast.nc"))
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 1L)
    expect_false(ds$is_aggregated)
    expect_false(ds$is_open)

    ds$open()
    expect_true(ds$is_open)

    ds$close()
    expect_false(ds$is_open)

    ds$open()
    on.exit(ds$close(), add = TRUE)

    info <- ds$file_inq()
    expect_type(info, "list")
    expect_true("nvars" %in% names(info))
    expect_true("ndims" %in% names(info))

    var_info <- ds$var_inq("tas")
    expect_type(var_info, "list")
    expect_equal(var_info$name, "tas")

    dim_info <- ds$dim_inq("time")
    expect_type(dim_info, "list")
    expect_equal(dim_info$name, "time")

    expect_type(ds$att_get("tas", "units"), "character")
    expect_true("tas" %in% ds$get_variables())
    expect_true(all(c("time", "lat", "lon") %in% ds$get_dimensions()))
    expect_s3_class(ds$get_time_axis()$values, "POSIXct")

    grid <- ds$get_spatial_grid()
    expect_true(all(c("lat", "lon") %in% names(grid)))

    data <- ds$var_get(
        "tas",
        start = c(1, 1, 1),
        count = c(1, 2, 2),
        collapse = TRUE
    )
    expect_type(data, "double")
    expect_equal(dim(data), c(2, 2))

    data <- ds$var_get(
        "tas",
        start = c(1, 1, 1),
        count = c(1, 2, 2),
        collapse = FALSE
    )
    expect_type(data, "double")
    expect_equal(dim(data), c(1, 2, 2))

    arr <- ds$read_array(
        "tas",
        start = c(1, 1, 1),
        count = c(1, 2, 2),
        collapse = TRUE
    )[[1L]]
    expect_type(arr, "double")
    expect_equal(dim(arr), c(2, 2))

    dt <- ds$read_data_table("tas", start = c(1, 1, 1), count = c(1, 2, 2))[[
        1L
    ]]
    expect_s3_class(dt, "data.table")
    expect_true("tas" %in% names(dt))
    expect_equal(nrow(dt), 4L)
})

test_that("EsgDataset$new() / EsgDataset$open() / EsgDataset$get_time_axis() / EsgDataset$read_data_table() loopback OPeNDAP multi-file smoke", {
    skip_if_not_installed("webfakes")
    server <- webfakes::new_app_process(
        subset_test__dap_app(calendar = "standard")
    )
    on.exit(server$stop(), add = TRUE)

    ds <- EsgDataset$new(paste0(server$url(), c("fast.nc", "june.nc")))
    expect_s3_class(ds, "EsgDataset")
    expect_equal(ds$file_count, 2L)
    expect_true(ds$is_aggregated)
    expect_false(ds$is_open)

    ds$open()
    on.exit(ds$close(), add = TRUE)

    expect_s3_class(ds$get_time_axis(2)$values, "POSIXct")

    start <- c(1, 1, 1)
    count <- c(1, 2, 2)
    dt_list <- ds$read_data_table("tas", start = start, count = count)
    expect_type(dt_list, "list")
    expect_length(dt_list, 2L)
    expect_true(all(vapply(
        dt_list,
        function(x) inherits(x, "data.table"),
        logical(1L)
    )))

    dt_all <- ds$read_data_table(
        "tas",
        start = start,
        count = count,
        rbind = TRUE
    )
    expect_s3_class(dt_all, "data.table")
    expect_true("file_index" %in% names(dt_all))
    expect_equal(sort(unique(dt_all$file_index)), c(1L, 2L))
    expect_equal(nrow(dt_all), 8L)
})
# }}}

# vim: fdm=marker :
