# Include instrumented namespace loading in worker startup. Deliberate timeout
# tests below keep their short deadlines and test cancellation independently.
DATASET_TEST_ASYNC_TIMEOUT <- 60

# Write a tiny real NetCDF fixture and return its path after closing the handle.
# dataset_test__table_file {{{
dataset_test__table_file <- function(
    time_vals,
    time_units,
    tas_vals = seq_along(time_vals),
    calendar = "standard"
) {
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
# }}}

# Create independent CMIP6 fixture copies and delete them with the calling test.
# dataset_test__cmip6_files {{{
dataset_test__cmip6_files <- function(years) {
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

DATASET_TEST_WORKER_SYMBOLS <- c(
    "EsgDataset",
    "DatasetAsyncTask",
    "dataset__async_condition",
    "dataset__async_error",
    "dataset__progress_bar",
    "dataset__progress_update",
    "dataset__progress_done"
)

# Start a private outer pool for callers that each launch their own async task.
# dataset_test__start_runtime {{{
dataset_test__start_runtime <- function(workers) {
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
                try(
                    mirai::daemons(NULL, .compute = compute_profile),
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

# Release only the outer pool owned by this test, preserving real worker exit.
# dataset_test__stop_runtime {{{
dataset_test__stop_runtime <- function(runtime) {
    if (is.null(runtime$compute_profile)) {
        return(invisible(NULL))
    }

    # Signal persistent coverage workers before resetting the owned transport.
    try(mirai::daemons(NULL, .compute = runtime$compute_profile), silent = TRUE)
    invisible(NULL)
}
# }}}

# Execute real dataset callers concurrently without sharing their NetCDF handles.
# dataset_test__lapply {{{
dataset_test__lapply <- function(X, FUN, ..., workers = min(2L, length(X))) {
    if (!length(X)) {
        return(vector("list", 0L))
    }

    runtime <- dataset_test__start_runtime(workers)
    on.exit(dataset_test__stop_runtime(runtime), add = TRUE)

    worker_symbols <- mget(
        DATASET_TEST_WORKER_SYMBOLS,
        envir = asNamespace("epwshiftr"),
        inherits = FALSE
    )
    # Test-file environments no longer own this shared helper constant. Send
    # it explicitly so nested callers retain the same 60-second deadline.
    worker_symbols[["DATASET_TEST_ASYNC_TIMEOUT"]] <- DATASET_TEST_ASYNC_TIMEOUT
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

# vim: fdm=marker :
