# Stream a real climate file slowly enough for source-pool liveness sampling.
# The first request can hold a large weakly identified partial until its reader
# is retired; subsequent requests expose each flushed chunk below that size.
source_pool_transfer_test__app <- function(
    bytes,
    release,
    requests,
    ranges = FALSE,
    bootstrap = TRUE,
    stall = FALSE,
    noise = NULL
) {
    app <- webfakes::new_app()
    app$locals$bytes <- bytes
    app$locals$release <- release
    app$locals$requests <- requests
    app$locals$gets <- 0L
    app$locals$ranges <- ranges
    app$locals$bootstrap <- bootstrap
    app$locals$stall <- stall
    app$locals$noise <- noise
    app$all("/dods/source.nc*", function(req, res) {
        res$set_status(504L)$send("DAP unavailable")
    })
    app$all("/files/source.nc", function(req, res, locals) {
        size <- length(locals$bytes)
        res$set_header("Content-Type", "application/x-netcdf")$set_header(
            "Content-Length",
            size
        )$set_header("Accept-Ranges", "bytes")
        if (locals$ranges) {
            res$set_header("ETag", '"pool-source-v1"')
        }
        if (req$method == "HEAD") {
            return(res$send(raw()))
        }
        locals$gets <- locals$gets + 1L
        range <- req$get_header("Range")
        cat(
            if (is.null(range)) "full" else range,
            "\n",
            file = locals$requests,
            append = TRUE
        )
        if (locals$bootstrap && locals$gets == 1L) {
            res$send_chunk(locals$bytes[1:20480])
            end <- as.double(Sys.time()) + 15
            while (
                !file.exists(locals$release) && as.double(Sys.time()) < end
            ) {
                Sys.sleep(.02)
            }
            return(res$send_chunk(locals$bytes[20481:size]))
        }
        first <- 0L
        last <- size - 1L
        if (locals$ranges && !is.null(range)) {
            bounds <- regmatches(
                range,
                regexec("^bytes=([0-9]+)-([0-9]*)$", range)
            )[[1L]]
            first <- as.integer(bounds[[2L]])
            last <- if (nzchar(bounds[[3L]])) as.integer(bounds[[3L]]) else last
            res$set_status(206L)$set_header(
                "Content-Range",
                sprintf("bytes %d-%d/%d", first, last, size)
            )$set_header(
                "Content-Length",
                last - first + 1L
            )
        }
        body <- locals$bytes[seq.int(first + 1L, last + 1L)]
        # The range capability check is intentionally fast; only actual data
        # responses need the long, continuously progressing reader lifetime.
        if (length(body) == 1L) {
            return(res$send(body))
        }
        if (locals$stall) {
            res$send_chunk(body[1:1024])
            # Simulate independent legacy work and retained history continuing
            # while the selected response sends no further bytes.
            paths <- file.path(
                locals$noise,
                c(
                    "unrelated.part",
                    "old.part.retained-history",
                    "old.pieces.retained-history/000001.done"
                )
            )
            dir.create(
                dirname(paths[[3L]]),
                recursive = TRUE,
                showWarnings = FALSE
            )
            end <- as.double(Sys.time()) + 8
            while (as.double(Sys.time()) < end) {
                for (path in paths) {
                    con <- file(path, "ab")
                    writeBin(as.raw(rep(0L, 1024L)), con)
                    close(con)
                }
                Sys.sleep(.2)
            }
            return(res$send_chunk(body[1025:length(body)]))
        }
        for (start in seq.int(1L, length(body), by = 1024L)) {
            res$send_chunk(body[start:min(start + 1023L, length(body))])
            Sys.sleep(.25)
        }
    })
    app
}
environment(source_pool_transfer_test__app) <- baseenv()

# Poll through the public pool API, with an independent process-level bound in
# the test runner and a shorter local guard for an unexpected pending ticket.
source_pool_transfer_test__drain <- function(pool, ticket) {
    end <- proc.time()[["elapsed"]] + 40
    while (ticket$status == "pending") {
        pool$poll()
        stopifnot(proc.time()[["elapsed"]] < end)
        Sys.sleep(.02)
    }
    stopifnot(ticket$status == "done")
    ticket$value
}

# Seed the released pool locator through public Downloader configuration. Its
# saved temporary directory deliberately differs from the manifest directory.
source_pool_transfer_test__legacy <- function(
    root,
    path,
    server,
    env = parent.frame()
) {
    store <- EsgStore$new(file.path(root, "store"))
    withr::defer(store$close(), envir = env)
    query <- store$add_files(recovery_test__files(
        path,
        server,
        checksum = checksum_file(path)
    ))
    file <- data.table::as.data.table(store$query("SELECT * FROM file_catalog"))
    fields <- c(
        "source_id",
        "experiment_id",
        "variant_label",
        "grid_label",
        "table_id",
        "variable_id",
        "title",
        "version",
        "tracking_id",
        "checksum_type",
        "checksum"
    )
    key <- checksum_bytes(serialize(
        as.list(file[1L, fields, with = FALSE]),
        NULL
    ))
    shared <- file.path(root, "shared")
    dest <- file.path(shared, "_source_pool", key)
    downloader <- Downloader$new(
        dest = dest,
        temp = file.path(root, "actual-retained"),
        manifest = file.path(dest, "manifest.duckdb"),
        n_workers = 0L,
        cleanup = FALSE,
        timeout = 25L
    )
    session <- downloader$enqueue(data.frame(
        logical_file_id = "legacy-source",
        filename = file$filename,
        file_key = file$file_key,
        dataset_id = file$dataset_id,
        checksum = checksum_file(path),
        checksum_type = "sha256",
        size = file.info(path)$size,
        url = server$url("/files/source.nc")
    ))
    task <- downloader$tasks(session_id = session)
    partial <- file.path(downloader$config$temp, paste0(task$task_id, ".part"))
    writeBin(readBin(path, "raw", n = 1024L), partial)
    manifest <- normalizePath(downloader$manifest)
    rm(downloader)
    gc()
    plan <- store$plan_region(query, 104, 1.37, c("2060-01-01", "2060-01-03"))
    list(
        store = store,
        plan = plan,
        shared = shared,
        task = task,
        session = session,
        manifest = manifest,
        partial = partial
    )
}

# vim: fdm=marker :
