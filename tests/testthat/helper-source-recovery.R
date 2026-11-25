# Serve two distinguishable native climate files behind the same resource name.
# Request counters are returned by the server rather than inferred from tasks.
recovery_test__app <- function(bytes) {
    app <- webfakes::new_app()
    app$locals$bytes <- bytes
    app$locals$gets <- 0L
    app$all("/dods/source.nc*", function(req, res) {
        res$set_status(504L)$send("DAP unavailable")
    })
    app$all("/files/source.nc", function(req, res, locals) {
        res$set_header("Content-Type", "application/x-netcdf")$set_header(
            "Content-Length",
            length(locals$bytes)
        )$set_header("ETag", '"source-v1"')
        if (req$method == "HEAD") {
            return(res$send(raw()))
        }
        locals$gets <- locals$gets + 1L
        res$send(locals$bytes)
    })
    app$get("/counts", function(req, res, locals) {
        res$send(as.character(locals$gets))
    })
    app
}
environment(recovery_test__app) <- baseenv()

# Use the normal File catalogue while deliberately withholding content evidence.
recovery_test__files <- function(
    path,
    server,
    record = "same-record",
    checksum = NA_character_
) {
    docs <- esgf_test__file_docs(
        path,
        opendap_url = server$url("/dods/source.nc"),
        download_url = server$url("/files/source.nc")
    )
    data.table::set(docs, j = "master_id", value = record)
    data.table::set(docs, j = "tracking_id", value = NA_character_)
    data.table::set(docs, j = "checksum", value = checksum)
    data.table::set(docs, j = "size", value = as.double(file.info(path)$size))
    esgf_test__file_result(docs)
}

# Drain through the public persistent-pool contract with a bounded deadline.
recovery_test__drain <- function(pool, ticket) {
    end <- as.double(Sys.time()) + 30
    while (ticket$status == "pending") {
        pool$poll()
        stopifnot(as.double(Sys.time()) < end)
        Sys.sleep(.01)
    }
    stopifnot(ticket$status == "done")
    ticket$value
}

# Read only the public extraction output, preserving its native CF coordinates.
recovery_test__values <- function(store) {
    rows <- store$query("SELECT output_path FROM extraction_result")
    stopifnot(nrow(rows) == 1L)
    store$query(paste0(
        "SELECT value FROM read_parquet('",
        file.path(store$path, rows$output_path[[1L]]),
        "') ORDER BY cf_year,cf_month,cf_day,cf_second_of_day"
    ))$value
}

# vim: fdm=marker :
