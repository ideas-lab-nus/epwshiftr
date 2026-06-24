downloader_http_bytes <- function(size = 4096L) {
    as.raw(rep(0:255, length.out = size))
}

downloader_http_checksum <- function(size = 4096L, algo = "md5") {
    checksum_bytes(downloader_http_bytes(size), algo)
}

local_downloader_http_server <- function(env = parent.frame()) {
    # webfakes starts a callr-backed R process; covr can pick up partial trace
    # files from that process and fail while merging coverage.
    skip_on_covr()
    skip_if_not_installed("webfakes")

    app <- webfakes::new_app()
    app$locals$ok <- downloader_http_bytes(4096L)
    app$locals$range <- downloader_http_bytes(8193L)
    app$locals$slow <- downloader_http_bytes(2048L)
    app$locals$flaky_count <- 0L

    request_header <- function(req, name) {
        value <- tryCatch(req$get_header(name), error = function(e) NULL)
        if (!is.null(value)) {
            return(value)
        }
        headers <- req$headers
        if (is.null(headers)) {
            return(NULL)
        }
        hit <- match(tolower(name), tolower(names(headers)))
        if (is.na(hit)) NULL else headers[[hit]]
    }

    parse_range <- function(req, size) {
        header <- request_header(req, "Range")
        if (is.null(header) || !length(header) || !nzchar(header[[1L]])) {
            return(NULL)
        }
        match <- regexec("^bytes=([0-9]+)-([0-9]*)$", header[[1L]])
        parts <- regmatches(header[[1L]], match)[[1L]]
        if (length(parts) != 3L) {
            return(FALSE)
        }
        start <- as.integer(parts[[2L]])
        end <- if (nzchar(parts[[3L]])) as.integer(parts[[3L]]) else size - 1L
        if (is.na(start) || is.na(end) || start < 0L || end < start || start >= size) {
            return(FALSE)
        }
        c(start = start, end = min(end, size - 1L))
    }

    send_bytes <- function(req, res, bytes, range = TRUE) {
        size <- length(bytes)
        res$
            set_header("Accept-Ranges", if (isTRUE(range)) "bytes" else "none")$
            set_header("Content-Type", "application/octet-stream")

        req_range <- if (isTRUE(range)) parse_range(req, size) else NULL
        if (isFALSE(req_range)) {
            return(res$
                set_status(416L)$
                set_header("Content-Range", sprintf("bytes */%d", size))$
                send(raw()))
        }
        if (!is.null(req_range)) {
            start <- unname(req_range[["start"]])
            end <- unname(req_range[["end"]])
            body <- bytes[(start + 1L):(end + 1L)]
            return(res$
                set_status(206L)$
                set_header("Content-Range", sprintf("bytes %d-%d/%d", start, end, size))$
                set_header("Content-Length", length(body))$
                send(body))
        }

        res$
            set_status(200L)$
            set_header("Content-Length", size)$
            send(bytes)
    }

    app$all("/files/ok.bin", function(req, res, locals) {
        send_bytes(req, res, locals$ok, range = FALSE)
    })
    app$all("/files/range.bin", function(req, res, locals) {
        send_bytes(req, res, locals$range, range = TRUE)
    })
    app$all("/files/range-copy.bin", function(req, res, locals) {
        send_bytes(req, res, locals$range, range = TRUE)
    })
    app$all("/files/no-range.bin", function(req, res, locals) {
        send_bytes(req, res, locals$range, range = FALSE)
    })
    app$all("/files/flaky.bin", function(req, res, locals) {
        locals$flaky_count <- locals$flaky_count + 1L
        if (locals$flaky_count == 1L) {
            res$set_status(500L)$send("temporary failure")
        } else {
            send_bytes(req, res, locals$ok, range = FALSE)
        }
    })
    app$all("/files/missing.bin", function(req, res) {
        res$set_status(404L)$send("missing")
    })
    app$all("/files/slow.bin", function(req, res, locals) {
        res$set_status(200L)$set_header("Content-Type", "application/octet-stream")
        res$send_chunk(locals$slow[1:1024])
        Sys.sleep(0.05)
        res$send_chunk(locals$slow[1025:2048])
    })
    app$all("/redirect/range.bin", function(req, res) {
        res$redirect("/files/range.bin")
    })

    proc <- webfakes::new_app_process(app)
    withr::defer(proc$stop(), envir = env)
    proc
}
