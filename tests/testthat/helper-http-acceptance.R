# Observe actual HTTP requests while keeping the NetCDF representation and its
# deliberately shared ETag identical across independently hosted resources.
http_acceptance__app <- function(bytes, partial = FALSE, gate = NULL) {
    app <- webfakes::new_app()
    app$locals$bytes <- bytes
    app$locals$partial <- partial
    app$locals$gate <- gate
    app$locals$requests <- list()
    app$locals$active <- 0L
    app$locals$maximum_active <- 0L
    app$locals$dap_requests <- list()
    dap_handler <- function(req, res, locals) {
        locals$dap_requests[[length(locals$dap_requests) + 1L]] <- list(
            path = req$path,
            method = req$method
        )
        res$set_status(504L)$send("DAP unavailable")
    }
    for (suffix in c("", ".dds", ".das", ".dods")) {
        app$all(paste0("/dods/source.nc", suffix), dap_handler)
    }
    app$get("/dap-requests", function(req, res, locals) {
        res$send_json(locals$dap_requests, auto_unbox = TRUE)
    })
    handler <- function(req, res, locals) {
        range <- req$get_header("Range")
        if (is.null(range)) {
            range <- ""
        }
        res$set_header("ETag", '"same-resource-tag"')
        if (req$method == "HEAD") {
            return(res$set_header(
                "Content-Length",
                length(locals$bytes)
            )$send(raw()))
        }
        locals$requests[[length(locals$requests) + 1L]] <- list(
            path = req$path,
            range = range,
            start = as.double(Sys.time())
        )
        index <- length(locals$requests)
        locals$active <- locals$active + 1L
        locals$maximum_active <- max(locals$maximum_active, locals$active)
        on.exit(
            {
                locals$requests[[index]]$end <- as.double(Sys.time())
                locals$active <- locals$active - 1L
            },
            add = TRUE
        )
        total <- length(locals$bytes)

        start <- 0L
        end <- total - 1L
        if (nzchar(range)) {
            bounds <- strsplit(sub("^bytes=", "", range), "-", fixed = TRUE)[[
                1L
            ]]
            start <- as.integer(bounds[[1L]])
            if (length(bounds) > 1L && nzchar(bounds[[2L]])) {
                end <- as.integer(bounds[[2L]])
            }
            res$set_status(206L)$set_header(
                "Content-Range",
                sprintf("bytes %d-%d/%d", start, end, total)
            )
        }
        if (!is.null(locals$gate) && !locals$partial && range != "bytes=0-0") {
            writeLines("started", paste0(locals$gate, ".started"))
            deadline <- Sys.time() + 30
            while (!file.exists(locals$gate) && Sys.time() < deadline) {
                Sys.sleep(.01)
            }
            if (!file.exists(locals$gate)) {
                return(res$set_status(503L)$send("gate expired"))
            }
        }
        if (locals$partial && range != "bytes=0-0") {
            locals$partial <- FALSE
            res$set_header("Content-Length", end - start + 1L)
            res$send_chunk(locals$bytes[(start + 1L):min(start + 1024L, total)])
            return(invisible(NULL))
        }
        res$send(locals$bytes[(start + 1L):(end + 1L)])
    }
    for (path in c(
        "/files/source.nc",
        "/files/first.bin",
        "/files/second.bin"
    )) {
        app$all(path, handler)
    }
    app$get("/stats", function(req, res, locals) {
        res$send_json(
            list(
                active = locals$active,
                maximum_active = locals$maximum_active
            ),
            auto_unbox = TRUE
        )
    })
    app$get("/requests", function(req, res, locals) {
        res$send_json(locals$requests, auto_unbox = TRUE)
    })
    app
}
environment(http_acceptance__app) <- baseenv()

# Return the server's observations, independently of Downloader bookkeeping.
http_acceptance__requests <- function(server, path = "/requests") {
    jsonlite::fromJSON(rawToChar(
        curl::curl_fetch_memory(server$url(path))$content
    ))
}

# Read the persisted public source receipt used by a one-file Store extraction.
http_acceptance__receipt <- function(store) {
    paths <- list.files(
        file.path(store$path, "sources"),
        "^pool-download-.*[.]json$",
        full.names = TRUE
    )
    stopifnot(length(paths) == 1L)
    jsonlite::read_json(paths[[1L]], simplifyVector = TRUE)
}

# Preserve fixture, observed requests and public task receipts when the acceptance
# runner requests a durable evidence directory; normal package tests stay local.
http_acceptance__save <- function(name, evidence) {
    root <- Sys.getenv("EPWSHIFTR_HTTP_ACCEPTANCE_EVIDENCE")
    if (!nzchar(root)) {
        return(invisible(NULL))
    }
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
        evidence,
        file.path(root, paste0(name, ".json")),
        auto_unbox = TRUE,
        na = "null",
        null = "null",
        pretty = TRUE
    )
    invisible(NULL)
}

# Reproduce the released pool locator through a real public Downloader; this is
# retained-state input, not a call into the new recovery selection implementation.
http_acceptance__legacy_downloader <- function(store, file, shared = NULL) {
    if (is.null(shared)) {
        return(store$downloader(n_workers = 0L, retries = 1L, cleanup = FALSE))
    }
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
    dest <- file.path(shared, "_source_pool", key)
    Downloader$new(
        dest = dest,
        temp = file.path(dest, ".tmp"),
        manifest = file.path(dest, "manifest.duckdb"),
        n_workers = 0L,
        retries = 1L,
        cleanup = FALSE
    )
}

# Read public Parquet output in native-clock/site order, selecting scientific
# fields whose identity does not depend on the Store's plan or artifact IDs.
http_acceptance__rows <- function(store) {
    paths <- file.path(
        store$path,
        store$query("SELECT output_path FROM extraction_result")$output_path
    )
    quoted <- vapply(
        paths,
        function(path) paste0("'", gsub("'", "''", path, fixed = TRUE), "'"),
        character(1L)
    )
    store$query(paste0(
        "SELECT site_id,cf_calendar,cf_year,cf_month,cf_day,cf_second_of_day,",
        "time,time_bound_start,time_bound_end,value,units FROM read_parquet([",
        paste(quoted, collapse = ","),
        "]) ORDER BY site_id,cf_year,cf_month,cf_day,cf_second_of_day"
    ))
}

# vim: fdm=marker :
