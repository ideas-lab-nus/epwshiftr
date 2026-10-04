# Run from the source checkout after dependencies are installed. This integration
# check copies one file and never loads the epwshiftr namespace or other sources.
# local callback {{{
local({
    root <- tempfile("copied-downloader-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    source_file <- file.path(root, "download module.R")
    stopifnot(file.copy("R/downloader.R", source_file))
    module <- new.env(parent = baseenv())
    sys.source(source_file, envir = module)
    stopifnot(!"epwshiftr" %in% loadedNamespaces())

    input <- file.path(root, "input.bin")
    writeBin(as.raw(rep(0:255, 32L)), input)
    checksum <- unname(tools::md5sum(input))
    prefix <- if (.Platform$OS.type == "windows") "file:///" else "file://"
    url <- paste0(prefix, normalizePath(input, winslash = "/"))
    dl <- module$Downloader$new(
        dest = file.path(root, "direct"),
        n_workers = 0L
    )
    output <- dl$download(url, checksum = checksum, checksum_type = "md5")
    stopifnot(identical(unname(tools::md5sum(output)), checksum))
    stopifnot(identical(
        dl$download(url, checksum = checksum, checksum_type = "md5"),
        output
    ))
    module$downloader__config_validate(dl$config)
    invalid <- dl$config
    invalid$retries <- 0L
    stopifnot(inherits(
        tryCatch(module$downloader__config_validate(invalid), error = identity),
        "error"
    ))

    manifest <- file.path(root, "manifest.duckdb")
    queued <- module$Downloader$new(
        dest = file.path(root, "background"),
        manifest = manifest,
        n_workers = 0L
    )
    session <- queued$enqueue(data.table::data.table(
        logical_file_id = "standalone-input",
        filename = "background.bin",
        url = url,
        checksum = checksum,
        checksum_type = "md5",
        priority = 1L
    ))
    job <- queued$start(session_id = session)
    # Release the parent connection before the detached writer opens DuckDB.
    queued$.__enclos_env__$private$disconnect_manifest()
    deadline <- Sys.time() + 60
    repeat {
        # Read a terminal manifest only when the worker has released its lock.
        # tryCatch callback {{{
        status <- tryCatch(
            {
                conn <- module$downloader__ddb_connect(
                    manifest,
                    read_only = TRUE
                )
                tryCatch(
                    module$downloader__ddb_read_table(conn, "download_job"),
                    finally = module$downloader__ddb_disconnect(conn)
                )
            },
            error = function(error) NULL
        )
        # }}}
        if (
            !is.null(status) &&
                any(status$status %in% c("done", "error", "cancelled"))
        ) {
            break
        }
        if (Sys.time() >= deadline) {
            stop(
                "Standalone background download did not finish within 60 seconds."
            )
        }
        Sys.sleep(0.2)
    }
    stopifnot(identical(status$status, "done"))
    stopifnot(identical(
        unname(tools::md5sum(file.path(root, "background", "background.bin"))),
        checksum
    ))
    pid <- status$pid[[1L]]
    while (module$downloader__pid_alive(pid) && Sys.time() < deadline) {
        Sys.sleep(0.1)
    }
    stopifnot(!module$downloader__pid_alive(pid))

    asynchronous <- module$Downloader$new(
        dest = file.path(root, "async"),
        n_workers = 1L
    )
    on.exit(asynchronous$.__enclos_env__$private$finalize(), add = TRUE)
    task <- asynchronous$download(
        url,
        checksum = checksum,
        checksum_type = "md5",
        block = FALSE
    )
    asynchronous$wait_for_tasks(task, progress = FALSE)
    stopifnot(identical(
        unname(tools::md5sum(file.path(root, "async", "input.bin"))),
        checksum
    ))
    stopifnot(!"epwshiftr" %in% loadedNamespaces())
    cat(
        "Standalone foreground, cache reuse, detached job and asynchronous download checks passed.\n"
    )
})
# }}}

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
