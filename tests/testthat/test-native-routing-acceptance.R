# Observe every whole-file request independently of native DAP acquisition. A
# valid NetCDF remains available so a hidden fallback would be observable.
native_acceptance__http_app <- function(bytes) {
    app <- webfakes::new_app()
    app$locals$bytes <- bytes
    app$locals$requests <- character()
    app$all("/source.nc", function(req, res, locals) {
        locals$requests <- c(locals$requests, paste(req$method, req$path))
        res$set_header("Content-Length", length(locals$bytes))
        res$send(if (req$method == "HEAD") raw() else locals$bytes)
    })
    app$get("/requests", function(req, res, locals) {
        res$send_json(locals$requests)
    })
    app
}
environment(native_acceptance__http_app) <- baseenv()

# Exercise the public native acquisition boundary with both services advertised:
# successful four-corner reads and failed DAP reads must never request HTTP bytes.
test_that("native acquisition keeps whole-file HTTP idle through DAP success and failure", {
    skip_if_not_installed("webfakes")
    root <- withr::local_tempdir()
    path <- file.path(root, "source.nc")
    write_local_cmip6_netcdf_fixture(path, 2060L, calendar = "360_day")
    dap <- webfakes::new_app_process(subset_test__dap_app())
    http <- webfakes::new_app_process(native_acceptance__http_app(
        readBin(path, "raw", file.info(path)$size)
    ))
    withr::defer(dap$stop())
    withr::defer(http$stop())
    docs <- esgf_test__file_docs(
        path,
        opendap_url = dap$url("/fast.nc"),
        download_url = http$url("/source.nc")
    )
    files <- esgf_test__file_result(docs)
    requests <- data.frame(
        request_id = "first-day",
        site_id = "city",
        lon = 10.25,
        lat = .25,
        time_start = "2060-01-01",
        time_stop = "2060-01-01"
    )
    success <- file.path(root, "success")
    expect_true(
        files$fetch_subset(
            requests,
            success,
            workers = 2L,
            retries = 0L,
            timeout = 5,
            discover = FALSE
        )$complete
    )
    saved <- read_subset(success)
    expect_equal(sort(saved$values$value), as.double(1:4))
    expect_equal(nrow(saved$neighborhoods), 4L)
    expect_identical(unique(saved$values$cf_calendar), "360_day")
    expect_length(http_acceptance__requests(http), 0L)

    # The same public request now encounters real DAP weather-read failures.
    # A fresh destination prevents cached native blocks from hiding the failure.
    curl::curl_fetch_memory(dap$url("/fail"))
    failed <- file.path(root, "failed")
    expect_false(
        files$fetch_subset(
            requests,
            failed,
            workers = 2L,
            retries = 0L,
            timeout = 5,
            discover = FALSE
        )$complete
    )
    attempts <- subset_status(failed, "attempts")
    expect_true(any(!attempts$success))
    expect_equal(nrow(subset_status(failed, "blocks")), 0L)
    expect_length(http_acceptance__requests(http), 0L)

    # A directory record without DAP remains an explicit acquisition gap even
    # though the advertised whole-file HTTP representation can be downloaded.
    only_http <- esgf_test__file_result(esgf_test__file_docs(
        path,
        download_url = http$url("/source.nc"),
        include_opendap = FALSE
    ))
    missing <- file.path(root, "missing-dap")
    expect_false(
        only_http$fetch_subset(
            requests,
            missing,
            workers = 2L,
            retries = 0L,
            timeout = 5,
            discover = FALSE
        )$complete
    )
    expect_equal(nrow(subset_status(missing, "blocks")), 0L)
    expect_length(http_acceptance__requests(http), 0L)
    http_acceptance__save(
        "native-routing",
        list(
            fixture_sha256 = checksum_file(path),
            values = saved$values,
            neighborhoods = saved$neighborhoods,
            failed_attempts = attempts,
            dap_requests = http_acceptance__requests(dap),
            whole_file_requests = http_acceptance__requests(http)
        )
    )
})

# vim: fdm=marker :
