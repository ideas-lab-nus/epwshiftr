# downloader_http_bytes {{{
downloader_http_bytes <- function(size = 4096L) {
    as.raw(rep(0:255, length.out = size))
}
# }}}

# downloader_http_checksum {{{
downloader_http_checksum <- function(size = 4096L, algo = "md5") {
    checksum_bytes(downloader_http_bytes(size), algo)
}
# }}}

# Keep HTTP handlers independent of the package/test namespace. Serializing a
# test environment into a disposable server would load the instrumented package
# there even though the server never executes its download or climate code.
# downloader_http_app {{{
downloader_http_app <- function(ok, range, slow) {
    app <- webfakes::new_app()
    app$locals$ok <- ok
    app$locals$range <- range
    app$locals$slow <- slow
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
        if (
            is.na(start) ||
                is.na(end) ||
                start < 0L ||
                end < start ||
                start >= size
        ) {
            return(FALSE)
        }
        c(start = start, end = min(end, size - 1L))
    }

    send_bytes <- function(req, res, bytes, range = TRUE) {
        size <- length(bytes)
        res$set_header(
            "Accept-Ranges",
            if (isTRUE(range)) "bytes" else "none"
        )$set_header("Content-Type", "application/octet-stream")

        req_range <- if (isTRUE(range)) parse_range(req, size) else NULL
        if (isFALSE(req_range)) {
            return(res$set_status(416L)$set_header(
                "Content-Range",
                sprintf("bytes */%d", size)
            )$send(raw()))
        }
        if (!is.null(req_range)) {
            start <- unname(req_range[["start"]])
            end <- unname(req_range[["end"]])
            body <- bytes[(start + 1L):(end + 1L)]
            return(res$set_status(206L)$set_header(
                "Content-Range",
                sprintf("bytes %d-%d/%d", start, end, size)
            )$set_header("Content-Length", length(body))$send(body))
        }

        res$set_status(200L)$set_header("Content-Length", size)$send(bytes)
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
        res$set_status(200L)$set_header(
            "Content-Type",
            "application/octet-stream"
        )
        res$send_chunk(locals$slow[1:1024])
        Sys.sleep(0.05)
        res$send_chunk(locals$slow[1025:2048])
    })
    app$all("/redirect/range.bin", function(req, res) {
        res$redirect("/files/range.bin")
    })
    app$all("/dods/valid.nc", function(req, res) {
        res$set_status(200L)$send("dataset landing page")
    })
    app$all("/dods/valid.nc.dds", function(req, res) {
        res$set_status(200L)$set_header("Content-Type", "text/plain")$send(
            "Dataset { Float32 tas[time = 1]; } valid.nc;"
        )
    })
    app$all("/dods/html.nc", function(req, res) {
        res$set_status(200L)$send("dataset landing page")
    })
    app$all("/dods/html.nc.dds", function(req, res) {
        res$set_status(200L)$set_header("Content-Type", "text/html")$send(
            "<html><body>temporary error</body></html>"
        )
    })
    app$all("/dods/missing.nc.dds", function(req, res) {
        res$set_status(404L)$send("missing")
    })

    app
}
# }}}
environment(downloader_http_app) <- baseenv()

# Start the isolated HTTP fixture and tie cleanup to the requesting test.
# local_downloader_http_server {{{
local_downloader_http_server <- function(env = parent.frame()) {
    skip_if_not_installed("webfakes")
    app <- downloader_http_app(
        downloader_http_bytes(4096L),
        downloader_http_bytes(8193L),
        downloader_http_bytes(2048L)
    )
    proc <- webfakes::new_app_process(app)
    withr::defer(proc$stop(), envir = env)
    proc
}
# }}}

# Serve DAP2 arrays with real constraints and XDR payloads, for real libnetcdf I/O.
subset_test__dap_app <- function() {
    app <- webfakes::new_app()
    app$locals$weather <- 0L
    app$locals$requests <- character()
    app$get('/requests', function(req, res, locals) {
        res$send_json(locals$requests)
    })
    app$get('/reset', function(req, res, locals) {
        locals$requests <- character()
        res$send('ok')
    })
    app$use(function(req, res, locals) {
        locals$requests <- c(locals$requests, paste(req$path, req$query_string))
        name <- basename(req$path)
        if (grepl('[.]das$', name)) {
            attributes <- 'Attributes { time { String units "days since 2060-01-01"; String calendar "360_day"; } tas { String units "K"; } }'
            if (grepl('bounds', name)) {
                attributes <- sub(
                    'calendar "360_day";',
                    'calendar "360_day"; String bounds "time_bnds";',
                    attributes,
                    fixed = TRUE
                )
            }
            if (grepl('wrong[.]nc', name)) {
                attributes <- sub(
                    'units "K"',
                    'units "C"',
                    attributes,
                    fixed = TRUE
                )
            }
            return(res$send(attributes))
        }
        query <- utils::URLdecode(
            if (is.null(req$query_string)) '' else req$query_string
        )
        if (startsWith(query, '?')) {
            query <- substring(query, 2L)
        }
        variables <- list(
            time = 0:7,
            lat = 0:1,
            lon = 10:11,
            tas = array(1:32, c(2, 2, 8)),
            time_bnds = rbind(0:7, 1:8)
        )
        dims <- list(
            time = 'time',
            lat = 'lat',
            lon = 'lon',
            tas = c('time', 'lat', 'lon'),
            time_bnds = c('time', 'bnds')
        )
        sizes <- c(time = 8L, lat = 2L, lon = 2L, bnds = 2L)
        select <- if (nzchar(query)) {
            strsplit(query, ',', fixed = TRUE)[[1L]]
        } else {
            names(variables)
        }
        declarations <- character()
        payload <- raw()
        for (part in select) {
            var <- sub('[[].*$', '', part)
            ranges <- regmatches(part, gregexpr('[[][0-9:]+[]]', part))[[1L]]
            indices <- lapply(sizes[dims[[var]]], seq_len)
            if (length(ranges) && ranges[[1L]] != '') {
                indices <- lapply(ranges, function(z) {
                    x <- as.integer(strsplit(
                        substring(z, 2, nchar(z) - 1),
                        ':',
                        fixed = TRUE
                    )[[1L]])
                    if (length(x) == 1L) {
                        x + 1L
                    } else if (length(x) == 2L) {
                        seq(x[1L], x[2L]) + 1L
                    } else {
                        seq(x[1L], x[3L], by = x[2L]) + 1L
                    }
                })
            }
            declarations <- c(
                declarations,
                paste0(
                    'Float64 ',
                    var,
                    paste0(
                        '[',
                        dims[[var]],
                        ' = ',
                        lengths(indices),
                        ']',
                        collapse = ''
                    ),
                    ';'
                )
            )
            value <- variables[[var]]
            if (var == 'tas') {
                if (grepl('[.]dods$', name)) {
                    locals$weather <- locals$weather + 1L
                }
                if (
                    grepl('[.]dods$', name) &&
                        grepl('bad', name) &&
                        locals$weather > 1L
                ) {
                    return(res$set_status(503L)$send('temporary read failure'))
                }
                value <- as.vector(value[
                    indices[[3L]],
                    indices[[2L]],
                    indices[[1L]],
                    drop = FALSE
                ])
                if (grepl('[.]dods$', name) && grepl('slow', name)) {
                    Sys.sleep(.25)
                }
                if (grepl('[.]dods$', name) && grepl('progress', name)) {
                    Sys.sleep(.75)
                }
                if (grepl('[.]dods$', name) && grepl('hang', name)) {
                    Sys.sleep(10)
                }
            } else if (var == 'time_bnds') {
                if (grepl('wrong-bounds', name)) {
                    value <- value + .5
                }
                value <- as.vector(value[
                    indices[[2L]],
                    indices[[1L]],
                    drop = FALSE
                ])
            } else {
                value <- value[indices[[1L]]]
            }
            payload <- c(
                payload,
                writeBin(
                    rep(as.integer(length(value)), 2),
                    raw(),
                    size = 4,
                    endian = 'big'
                ),
                writeBin(as.double(value), raw(), size = 8, endian = 'big')
            )
        }
        dds <- paste(
            'Dataset {',
            paste(declarations, collapse = ' '),
            '} data;'
        )
        if (grepl('[.]dds$', name)) {
            return(res$send(dds))
        }
        res$set_header('Content-Type', 'application/octet-stream')$send(c(
            charToRaw(paste0(dds, '\nData:\n')),
            payload
        ))
    })
    app
}
environment(subset_test__dap_app) <- baseenv()

# vim: fdm=marker :
