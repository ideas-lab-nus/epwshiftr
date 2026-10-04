# Save covr's complete source metadata before workers start their actual tasks.
# Exit-time traces contain all counters, without repeatedly serializing source
# environments. A process registration makes missing exit traces detectable.
# coverage__register {{{
coverage__register <- function(directory) {
    metadata <- file.path(directory, "covr-source-metadata.rds")
    if (!file.exists(metadata)) {
        pending <- tempfile("pending-metadata-", tmpdir = directory)
        saveRDS(
            as.list(get(".counters", asNamespace("covr"))),
            pending,
            compress = FALSE
        )
        if (!file.rename(pending, metadata)) {
            stop("Could not publish coverage source metadata.")
        }
    }
    # mirai otherwise force-kills disconnected workers after 200 ms, before R
    # can reliably run namespace finalizers. Only this instrumented process uses
    # graceful exit; the normal package check still exercises default shutdown.
    mirai_ns <- asNamespace("mirai")
    original <- get("daemons", envir = mirai_ns)
    if (!isTRUE(attr(original, "coverage_graceful"))) {
        graceful_daemons <- function(...) {
            args <- list(...)
            args$autoexit <- NA
            do.call(original, args)
        }
        attr(graceful_daemons, "coverage_graceful") <- TRUE
        unlockBinding("daemons", mirai_ns)
        assign("daemons", graceful_daemons, envir = mirai_ns)
        lockBinding("daemons", mirai_ns)
    }
    writeLines(
        as.character(Sys.getpid()),
        file.path(directory, paste0("expected-trace-", Sys.getpid()))
    )
}
# }}}

# Publish every line counter atomically, including zero counts. Metadata is
# already durable; short worker shutdown cannot leave a partly readable trace.
# coverage__save_trace {{{
coverage__save_trace <- function(directory) {
    pending <- tempfile("pending-trace-", tmpdir = directory)
    complete <- file.path(directory, paste0("covr_trace_", Sys.getpid()))
    values <- vapply(
        as.list(get(".counters", asNamespace("covr"))),
        function(counter) counter$value,
        numeric(1L)
    )
    saveRDS(values, pending, compress = FALSE)
    if (!file.rename(pending, complete)) {
        stop("Could not publish a complete coverage trace.")
    }
    invisible(complete)
}
# }}}

# Install this adapter only in covr's temporary instrumented package. Production
# code and the installed covr package remain unchanged. Merge counters by their
# original source keys, retaining covr's full source metadata and denominator.
# coverage__run {{{
coverage__run <- function(path = ".", ...) {
    original <- get("add_hooks", asNamespace("covr"))
    library_path <- NULL
    # Replace the temporary loader's exit hook and register each participating process.
    add_hooks <- function(pkg_name, lib, ...) {
        original(pkg_name, lib, ...)
        library_path <<- lib
        loader <- file.path(lib, pkg_name, "R", pkg_name)
        lines <- readLines(loader, warn = FALSE)
        hit <- grep("covr:::save_trace(", lines, fixed = TRUE)
        if (length(hit) != 1L) {
            stop("The covr exit hook changed; review the coverage adapter.")
        }
        register <- paste(
            deparse(coverage__register, width.cutoff = 500L),
            collapse = "\n"
        )
        writer <- paste(
            deparse(coverage__save_trace, width.cutoff = 500L),
            collapse = "\n"
        )
        lines[hit] <- paste0(
            "setHook(packageEvent(pkg, 'onLoad'), function(...) (",
            register,
            ")(Sys.getenv('COVERAGE_DIR', ",
            encodeString(lib, quote = '"'),
            ")))\n",
            sub(
                "covr:::save_trace",
                paste0("(", writer, ")"),
                lines[hit],
                fixed = TRUE
            )
        )
        writeLines(lines, loader)
    }
    # Refuse interrupted workers or source-key changes before reporting coverage.
    merge_traces <- function(files) {
        # Detached workers may publish their final counters just after the test
        # process exits. Wait only for registered receipts, with a fixed bound.
        deadline <- Sys.time() + 30
        repeat {
            expected <- sub(
                "^expected-trace-",
                "covr_trace_",
                list.files(library_path, pattern = "^expected-trace-")
            )
            files <- list.files(
                library_path,
                pattern = "^covr_trace_",
                full.names = TRUE
            )
            pending <- list.files(
                library_path,
                pattern = "^pending-(trace|metadata)-"
            )
            if (
                (!length(pending) && all(expected %in% basename(files))) ||
                    Sys.time() >= deadline
            ) {
                break
            }
            Sys.sleep(0.1)
        }
        if (
            !length(expected) ||
                length(pending) ||
                !all(expected %in% basename(files))
        ) {
            stop(
                "Unfinished worker coverage traces remain in ",
                library_path,
                "; coverage has not been uploaded."
            )
        }
        counters <- readRDS(file.path(library_path, "covr-source-metadata.rds"))
        values <- numeric(length(counters))
        for (file in files) {
            counts <- readRDS(file)
            positions <- match(names(counts), names(counters))
            if (
                !is.numeric(counts) ||
                    is.null(names(counts)) ||
                    anyNA(counts) ||
                    anyNA(positions) ||
                    anyDuplicated(names(counts))
            ) {
                stop(
                    "Coverage trace does not match the saved source metadata: ",
                    file
                )
            }
            values[positions] <- values[positions] + counts
        }
        for (index in seq_along(counters)) {
            counters[[index]]$value <- values[[index]]
        }
        message(
            "Merged ",
            length(files),
            " complete process coverage traces from ",
            library_path
        )
        counters
    }
    testthat::local_mocked_bindings(
        add_hooks = add_hooks,
        merge_coverage = merge_traces,
        .package = "covr"
    )
    covr::package_coverage(path, clean = FALSE, ...)
}
# }}}

# vim: fdm=marker :
