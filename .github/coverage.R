# Reuse covr's exact transformed bodies and source counters across fresh workers.
# Environments remain those of the current namespace, including R6 methods.
coverage__trace_environment <- function(ns, directory) {
    cache <- file.path(directory, "instrumented-functions")
    dir.create(cache, showWarnings = FALSE)
    covr_ns <- asNamespace("covr")
    original <- get("trace_calls", covr_ns)
    busy <- FALSE
    metadata_path <- file.path(directory, "covr-source-metadata.rds")
    source_metadata <- if (file.exists(metadata_path)) {
        readRDS(metadata_path)
    } else {
        NULL
    }
    # Collect only covr's literal counter calls from its transformed syntax.
    counter_keys <- function(x) {
        keys <- character()
        visit <- function(value) {
            if (missing(value)) {
                return(invisible(NULL))
            }
            if (is.call(value) && identical(value[[1L]], quote(covr:::count))) {
                keys <<- c(keys, value[[2L]])
            } else if (is.recursive(value)) {
                for (part in as.list(value)) {
                    visit(part)
                }
            }
        }
        visit(formals(x))
        visit(body(x))
        unique(keys)
    }
    # Cache top-level transforms; recursive calls retain covr's own semantics.
    cached_trace <- function(x, parent_functions = NULL, parent_ref = NULL) {
        if (busy || !is.function(x) || is.primitive(x)) {
            return(original(x, parent_functions, parent_ref))
        }
        busy <<- TRUE
        on.exit(busy <<- FALSE)
        srcref <- attr(x, "srcref")
        source <- attr(srcref, "srcfile")
        filename <- if (is.null(source)) NULL else source$filename
        key <- rlang::hash(list(
            deparse(x, width.cutoff = 500L),
            parent_functions,
            as.integer(srcref),
            filename
        ))
        path <- file.path(cache, paste0(key, ".rds"))
        counters <- get(".counters", covr_ns)
        if (file.exists(path)) {
            saved <- readRDS(path)
            body(x) <- saved$body
            formals(x) <- saved$formals
            if (!is.null(source_metadata)) {
                list2env(source_metadata[saved$keys], counters)
            }
            return(x)
        }
        value <- original(x, parent_functions, parent_ref)
        clean <- rlang::zap_srcref(value)
        saved <- list(
            body = body(clean),
            formals = formals(clean),
            keys = counter_keys(value)
        )
        pending <- tempfile("pending-function-", tmpdir = cache)
        saveRDS(saved, pending, compress = FALSE)
        if (!file.rename(pending, path)) {
            stop("Could not publish instrumented coverage function.")
        }
        value
    }
    unlockBinding("trace_calls", covr_ns)
    assign("trace_calls", cached_trace, covr_ns)
    lockBinding("trace_calls", covr_ns)
    on.exit(
        {
            unlockBinding("trace_calls", covr_ns)
            assign("trace_calls", original, covr_ns)
            lockBinding("trace_calls", covr_ns)
        },
        add = TRUE
    )
    covr:::trace_environment(ns)
}

# Save covr's complete source metadata before workers start their actual tasks.
# Exit traces and flushed hit journals retain counters without serializing source
# environments on every hit. Registered process identities bound trace collection.
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
    # A force-stopped native reader cannot run an R exit finalizer. Journal
    # every recorded counter before its expression executes, so intentional
    # process termination retains the same complete source denominator.
    keys <- file.path(directory, "covr-source-keys.rds")
    if (!file.exists(keys)) {
        saveRDS(names(readRDS(metadata)), keys)
    }
    source_keys <- readRDS(keys)
    # Hit recording is a hot path: hashed lookup avoids scanning every source key.
    positions <- list2env(
        setNames(as.list(seq_along(source_keys)), source_keys),
        parent = emptyenv()
    )
    journal <- file(
        file.path(directory, paste0("counter-journal-", Sys.getpid())),
        "ab"
    )
    covr_ns <- asNamespace("covr")
    original <- get("count", covr_ns)
    durable_count <- function(key) {
        position <- positions[[key]]
        if (is.null(position)) {
            stop("Coverage counter key changed during execution.")
        }
        writeBin(as.integer(position), journal, size = 4L)
        flush(journal)
        original(key)
    }
    unlockBinding("count", covr_ns)
    assign("count", durable_count, covr_ns)
    lockBinding("count", covr_ns)
    saveRDS(
        list(pid = Sys.getpid(), created = ps::ps_create_time(ps::ps_handle())),
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
        trace_hook <- grep("covr:::trace_environment(ns)", lines, fixed = TRUE)
        if (length(trace_hook) != 1L) {
            stop("The covr instrumentation hook changed.")
        }
        cached_trace <- paste(
            deparse(coverage__trace_environment, width.cutoff = 500L),
            collapse = "\n"
        )
        lines[trace_hook] <- paste0(
            "setHook(packageEvent(pkg, 'onLoad'), function(...) (",
            cached_trace,
            ")(ns, Sys.getenv('COVERAGE_DIR', ",
            encodeString(lib, quote = '"'),
            ")))"
        )
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
    # Recover intentional worker termination, but refuse live workers or invalid records.
    merge_traces <- function(files) {
        # Every registered process must have exited before its journal is read.
        # Hard-killed readers have all recorded hits in a flushed binary journal.
        counters <- readRDS(file.path(library_path, "covr-source-metadata.rds"))
        source_keys <- names(counters)
        expected_paths <- list.files(
            library_path,
            pattern = "^expected-trace-",
            full.names = TRUE
        )
        deadline <- Sys.time() + 30
        repeat {
            alive <- vapply(
                expected_paths,
                function(path) {
                    process <- readRDS(path)
                    handle <- tryCatch(
                        ps::ps_handle(process$pid, process$created),
                        error = function(e) NULL
                    )
                    !is.null(handle) && ps::ps_is_running(handle)
                },
                logical(1L)
            )
            if (!any(alive) || Sys.time() >= deadline) {
                break
            }
            Sys.sleep(.1)
        }
        if (!length(expected_paths) || any(alive)) {
            stop(
                "Unfinished worker coverage processes remain in ",
                library_path
            )
        }
        values <- numeric(length(counters))
        for (path in expected_paths) {
            pid <- readRDS(path)$pid
            trace <- file.path(library_path, paste0("covr_trace_", pid))
            journal <- file.path(library_path, paste0("counter-journal-", pid))
            if (file.exists(trace)) {
                counts <- readRDS(trace)
                positions <- match(names(counts), source_keys)
                if (
                    !is.numeric(counts) ||
                        is.null(names(counts)) ||
                        anyNA(counts) ||
                        anyNA(positions) ||
                        anyDuplicated(names(counts))
                ) {
                    stop(
                        "Coverage trace does not match saved source metadata: ",
                        trace
                    )
                }
                values[positions] <- values[positions] + counts
            } else {
                bytes <- file.info(journal)$size
                if (is.na(bytes) || bytes %% 4L != 0L) {
                    stop("Incomplete coverage journal: ", journal)
                }
                connection <- file(journal, "rb")
                # Bound memory even when an instrumented worker executes many hits.
                repeat {
                    hits <- readBin(
                        connection,
                        integer(),
                        n = 65536L,
                        size = 4L
                    )
                    if (!length(hits)) {
                        break
                    }
                    if (
                        anyNA(hits) || any(hits < 1L | hits > length(counters))
                    ) {
                        close(connection)
                        stop(
                            "Invalid source position in coverage journal: ",
                            journal
                        )
                    }
                    values <- values + tabulate(hits, nbins = length(counters))
                }
                close(connection)
            }
        }
        for (index in seq_along(counters)) {
            counters[[index]]$value <- values[[index]]
        }
        message(
            "Merged ",
            length(expected_paths),
            " process coverage records from ",
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

# Persist the merged covr result before contacting an external service. All
# summaries use covr's line tally, including lines with no recorded execution.
# coverage__report {{{
coverage__report <- function(coverage, path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    saveRDS(coverage, file.path(path, "coverage.rds"))
    lines <- data.table::as.data.table(covr::tally_coverage(
        coverage,
        by = "line"
    ))
    if (!nrow(lines)) {
        stop("No instrumented lines were found; coverage cannot be reported.")
    }
    files <- lines[,
        list(
            covered = sum(.SD[["value"]] > 0),
            total = .N
        ),
        by = "filename",
        .SDcols = "value"
    ]
    data.table::set(
        files,
        j = "percent",
        value = 100 * files$covered / files$total
    )
    data.table::setorderv(files, "filename")
    data.table::fwrite(files, file.path(path, "coverage-by-file.csv"))
    covr::to_cobertura(coverage, filename = file.path(path, "coverage.xml"))
    summary <- c(
        "## R line coverage",
        "",
        sprintf(
            "**%.2f%%** (%d / %d instrumented lines covered across %d files).",
            100 * sum(files$covered) / sum(files$total),
            sum(files$covered),
            sum(files$total),
            nrow(files)
        ),
        "",
        "Includes merged parent and worker traces. This measures executed R lines, not branch coverage.",
        "",
        "Download the coverage-report artifact for the covr result, per-file CSV and Cobertura XML."
    )
    writeLines(summary, file.path(path, "summary.md"))
    cat(summary, sep = "\n")
    invisible(files)
}
# }}}

# vim: fdm=marker :
