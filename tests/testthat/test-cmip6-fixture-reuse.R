# Replace only the test helper's real writer and restore it after this test.
cmip6_fixture__local_writer <- function(replacement, envir = parent.frame()) {
    target <- environment(cmip6_fixture__write)
    previous <- get("cmip6_fixture__write", envir = target, inherits = FALSE)
    assign("cmip6_fixture__write", replacement, envir = target)
    withr::defer(
        assign("cmip6_fixture__write", previous, envir = target),
        envir = envir
    )
}

# The fixture interface must keep real NetCDF output while avoiding repeated
# creation and isolating every caller's changes from the reusable template.
test_that("CMIP6 fixture copies share content but not mutable files", {
    raw_writer <- cmip6_fixture__write
    calls <- 0L
    cmip6_fixture__local_writer(
        function(...) {
            calls <<- calls + 1L
            raw_writer(...)
        }
    )
    paths <- file.path(withr::local_tempdir(), paste0(1:3, ".nc"))
    write_local_cmip6_netcdf_fixture(paths[1], 3044L)
    first_calls <- calls
    write_local_cmip6_netcdf_fixture(paths[2], 3044L)
    expect_identical(calls, first_calls)
    original_hash <- unname(tools::md5sum(paths[2]))
    expect_identical(unname(tools::md5sum(paths[1])), original_hash)
    nc <- RNetCDF::open.nc(paths[1], write = TRUE)
    RNetCDF::var.put.nc(
        nc,
        "tas",
        NA_real_,
        start = c(1, 1, 1),
        count = c(1, 1, 1)
    )
    RNetCDF::close.nc(nc)
    unlink(paths[1])
    write_local_cmip6_netcdf_fixture(paths[3], 3044L)
    expect_identical(unname(tools::md5sum(paths[3])), original_hash)
    expect_identical(unname(tools::md5sum(paths[2])), original_hash)
    expect_identical(calls, first_calls)
})

# Each semantic input is exercised against the real writer, including the raw
# year representation used by NetCDF metadata rather than only its integer year.
test_that("CMIP6 fixture identity includes every generation argument", {
    inputs <- list(
        list(year = 3050L),
        list(year = "03050"),
        list(year = 3051L),
        list(year = 3050L, variable_id = "hurs"),
        list(year = 3050L, calendar = "360_day"),
        list(year = 3050L, n_years = 2L),
        list(year = 3050L, frequency = "mon")
    )
    root <- withr::local_tempdir()
    for (i in seq_along(inputs)) {
        fresh <- file.path(root, paste0(i, "-fresh.nc"))
        reused <- file.path(root, paste0(i, "-copy.nc"))
        do.call(cmip6_fixture__write, c(list(path = fresh), inputs[[i]]))
        do.call(
            write_local_cmip6_netcdf_fixture,
            c(list(path = reused), inputs[[i]])
        )
        expect_identical(
            unname(tools::md5sum(fresh)),
            unname(tools::md5sum(reused))
        )
    }
})

# A partial writer failure must be retried, never reused as a completed template.
test_that("CMIP6 fixture failures do not publish partial content", {
    raw_writer <- cmip6_fixture__write
    calls <- 0L
    cmip6_fixture__local_writer(
        function(path, ...) {
            calls <<- calls + 1L
            if (calls == 1L) {
                writeBin(charToRaw("partial"), path)
                stop("controlled fixture failure")
            }
            raw_writer(path, ...)
        }
    )
    path <- file.path(withr::local_tempdir(), "retry.nc")
    expect_error(
        write_local_cmip6_netcdf_fixture(path, 3045L),
        "controlled fixture failure"
    )
    expect_false(file.exists(path))
    expect_silent(write_local_cmip6_netcdf_fixture(path, 3045L))
    expect_identical(calls, 2L)
    expect_error(
        write_local_cmip6_netcdf_fixture(dirname(path), 3045L),
        "Failed to copy"
    )
})

# vim: fdm=marker :
