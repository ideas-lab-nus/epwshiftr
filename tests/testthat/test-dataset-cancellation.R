# Cancellation must end the actual owned process before its one-shot backend is
# reported released. The worker's file marker is independent OS-level evidence.
test_that("Dataset close stops executing owned workers before backend release", {
    skip_on_cran()
    path <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(path, 2060L)
    withr::defer(unlink(path))
    for (iteration in seq_len(8L)) {
        marker <- tempfile()
        withr::defer(unlink(marker))
        dataset <- EsgDataset$new(path)
        dataset$open()
        # This existing async-start seam leaves work pending for public close().
        # A base-only callback keeps test environments out of the worker payload.
        handler <- function(urls, nc_handles, marker) {
            writeLines(as.character(Sys.getpid()), marker)
            Sys.sleep(5)
            TRUE
        }
        environment(handler) <- baseenv()
        task <- dataset$.__enclos_env__$private$start_async_operation(
            "cancel executing native reader",
            handler,
            handler_args = list(marker = marker),
            timeout = 10
        )
        deadline <- Sys.time() + 5
        while (!file.exists(marker) && Sys.time() < deadline) {
            Sys.sleep(.01)
        }
        expect_true(file.exists(marker))
        owned <- ps::ps_handle(as.integer(readLines(marker)))
        dataset$close()
        expect_false(ps::ps_is_running(owned))
        expect_false(dataset$is_open)
        expect_true(task$backend_released)
    }
})

# vim: fdm=marker :
