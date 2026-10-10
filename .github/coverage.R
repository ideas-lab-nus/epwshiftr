# Register OS process identity before dynamic instrumentation starts. The
# supervisor can distinguish an owned process from later reuse of the same PID;
# this CI-only loader relies on ps from the existing test tooling environment.
# coverage__register_process {{{
coverage__register_process <- function(directory) {
    process <- ps::ps_handle()
    receipt <- list(
        pid = Sys.getpid(),
        create_time = ps::ps_create_time(process),
        registered_at = Sys.time(),
        jit_level = compiler::enableJIT(-1),
        jit_env = Sys.getenv("R_ENABLE_JIT")
    )
    complete <- file.path(directory, paste0("process-", receipt$pid, ".rds"))
    if (file.exists(complete)) {
        previous <- readRDS(complete)
        if (
            !identical(previous$pid, receipt$pid) ||
                !identical(previous$create_time, receipt$create_time)
        ) {
            stop("A coverage PID was reused within one execution directory.")
        }
        return(invisible(previous))
    }
    pending <- tempfile("pending-process-", tmpdir = directory)
    saveRDS(receipt, pending, compress = FALSE)
    if (!file.rename(pending, complete)) {
        stop("Could not publish coverage process identity.")
    }
    invisible(receipt)
}
# }}}

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
    # Keep process accounting separate from line counters so coverage keys and
    # the denominator remain unchanged when results from several shards merge.
    usage <- proc.time()
    saveRDS(
        list(
            pid = Sys.getpid(),
            user = unname(usage[["user.self"]]),
            system = unname(usage[["sys.self"]]),
            elapsed = unname(usage[["elapsed"]])
        ),
        file.path(directory, paste0("resource-", Sys.getpid(), ".rds")),
        compress = FALSE
    )
    saveRDS(values, pending, compress = FALSE)
    if (!file.rename(pending, complete)) {
        stop("Could not publish a complete coverage trace.")
    }
    invisible(complete)
}
# }}}

# Reuse pure source-coordinate inference while retaining covr's complete AST
# traversal, per-process closure instrumentation and new zero-valued counters.
# The private imputation contract is validated for covr 3.6.5 only. Changes to
# source binaries, runtime version or the original imputer reject the artifact.
# coverage__trace_environment {{{
coverage__trace_environment <- function(
    env,
    path,
    source_identity,
    record = FALSE,
    load_parse_data = NULL
) {
    namespace <- asNamespace("covr")
    original <- get("impute_srcref", namespace)
    signature <- paste(deparse(original, width.cutoff = 500L), collapse = "\n")
    identity <- list(
        version = 1L,
        package = getNamespaceName(env),
        r_version = as.character(getRversion()),
        covr_version = as.character(utils::packageVersion("covr")),
        source = source_identity,
        imputer = signature
    )
    if (!identical(identity$covr_version, "3.6.5")) {
        stop("This coverage source-map adapter requires covr 3.6.5.")
    }
    if (record) {
        mappings <- new.env(parent = emptyenv())
    } else {
        cache <- readRDS(path)
        if (
            !identical(cache$identity, identity) ||
                !is.list(cache$data) ||
                (length(cache$data) > 0L && is.null(names(cache$data))) ||
                anyDuplicated(names(cache$data))
        ) {
            stop(
                "Prepared coverage source mappings do not match this installation."
            )
        }
        # The serialized artifact is data only: no source environments, AST,
        # executable closures, counter state or pointers may enter this cache.
        valid <- vapply(
            cache$data,
            function(entry) {
                is.list(entry) &&
                    identical(names(entry), "value") &&
                    (is.null(entry$value) ||
                        (is.list(entry$value) &&
                            all(vapply(
                                entry$value,
                                function(ref) {
                                    is.null(ref) ||
                                        (is.integer(ref) &&
                                            length(ref) == 8L &&
                                            !anyNA(ref) &&
                                            is.null(attributes(ref)))
                                },
                                logical(1L)
                            ))))
            },
            logical(1L)
        )
        if (!all(valid)) {
            stop("Coverage source mappings contain non-coordinate data.")
        }
        mappings <- list2env(cache$data, parent = emptyenv())
    }
    if (!is.null(load_parse_data)) {
        stopifnot(is.function(load_parse_data))
        original_parser <- get("get_parse_data", namespace)
        parser_locked <- bindingIsLocked("get_parse_data", namespace)
        parser_loaded <- FALSE
        # Complete source mappings normally avoid parser access entirely. Read
        # the immutable cache only on a real miss, inside timed instrumentation.
        deferred_parser <- function(srcfile) {
            if (!parser_loaded) {
                load_parse_data()
                parser_loaded <<- TRUE
            }
            original_parser(srcfile)
        }
        unlockBinding("get_parse_data", namespace)
        assign("get_parse_data", deferred_parser, envir = namespace)
        lockBinding("get_parse_data", namespace)
        on.exit(
            {
                unlockBinding("get_parse_data", namespace)
                assign("get_parse_data", original_parser, envir = namespace)
                if (parser_locked) {
                    lockBinding("get_parse_data", namespace)
                }
            },
            add = TRUE
        )
    }
    conditional <- get("is_conditional_or_loop", namespace)
    # This matches every input used by the pinned imputer: source file, all
    # srcref coordinates, operator and call length. Escaping disambiguates paths.
    map_key <- function(x, parent_ref) {
        if (is.null(parent_ref) || !conditional(x)) {
            return(NULL)
        }
        source <- attr(parent_ref, "srcfile")
        filename <- source[["filename"]]
        if (length(filename) != 1L) {
            return(NULL)
        }
        paste(
            encodeString(filename, quote = '"'),
            as.character(x[[1L]]),
            length(x),
            paste(as.integer(parent_ref), collapse = ","),
            sep = "\034"
        )
    }
    impute <- function(x, parent_ref) {
        key <- map_key(x, parent_ref)
        if (is.null(key)) {
            return(original(x, parent_ref))
        }
        entry <- mappings[[key]]
        if (!record && !is.null(entry)) {
            if (is.null(entry$value)) {
                return(NULL)
            }
            return(lapply(entry$value, function(ref) {
                if (is.null(ref)) {
                    NULL
                } else {
                    srcref(attr(parent_ref, "srcfile"), ref)
                }
            }))
        }
        value <- original(x, parent_ref)
        if (record) {
            pure <- if (is.null(value)) {
                NULL
            } else {
                lapply(value, function(ref) {
                    if (is.null(ref)) NULL else as.integer(ref)
                })
            }
            if (!is.null(entry) && !identical(entry$value, pure)) {
                stop(
                    "A coverage source-map key represents different source coordinates."
                )
            }
            mappings[[key]] <- list(value = pure)
        }
        value
    }
    # Restore covr even if instrumentation fails. Later unrelated tracing in
    # this process must not inherit the prepared package's temporary adapter.
    unlockBinding("impute_srcref", namespace)
    assign("impute_srcref", impute, envir = namespace)
    lockBinding("impute_srcref", namespace)
    on.exit(
        {
            unlockBinding("impute_srcref", namespace)
            assign("impute_srcref", original, envir = namespace)
            lockBinding("impute_srcref", namespace)
        },
        add = TRUE
    )
    get("trace_environment", namespace)(env)
    if (record) {
        saveRDS(
            list(
                identity = identity,
                data = as.list(mappings, all.names = TRUE)
            ),
            path,
            compress = FALSE
        )
    }
    invisible(NULL)
}
# }}}

# Install this adapter only in covr's temporary instrumented package. Production
# code and the installed covr package remain unchanged. Merge counters by their
# original source keys, retaining covr's full source metadata and denominator.
# coverage__run {{{
coverage__run <- function(path = ".", ..., .source_map = NULL) {
    original <- get("add_hooks", asNamespace("covr"))
    library_path <- NULL
    # Replace the temporary loader's exit hook and register each participating process.
    add_hooks <- function(pkg_name, lib, ...) {
        original(pkg_name, lib, ...)
        library_path <<- lib
        loader <- file.path(lib, pkg_name, "R", pkg_name)
        lines <- readLines(loader, warn = FALSE)
        instrument <- grep("covr:::trace_environment(ns)", lines, fixed = TRUE)
        if (length(instrument) != 1L) {
            stop(
                "The covr instrumentation hook changed; review the coverage adapter."
            )
        }
        trace_hook <- lines[instrument]
        if (!is.null(.source_map)) {
            # Hash immutable code/source databases before editing the loader.
            # The hashes are embedded in this installation's hook and artifact.
            source_identity <- tools::md5sum(file.path(
                lib,
                pkg_name,
                "R",
                paste0(pkg_name, c(".rdb", ".rdx"))
            ))
            names(source_identity) <- c("rdb", "rdx")
            tracer <- paste(
                deparse(coverage__trace_environment, width.cutoff = 500L),
                collapse = "\n"
            )
            trace_hook <- paste0(
                "setHook(packageEvent(pkg, 'onLoad'), function(...) (",
                tracer,
                ")(ns, ",
                encodeString(.source_map, quote = '"'),
                ", ",
                paste(capture.output(dput(source_identity)), collapse = ""),
                ", record = TRUE))"
            )
        }
        process_register <- paste(
            deparse(coverage__register_process, width.cutoff = 500L),
            collapse = "\n"
        )
        # The process receipt precedes instrumentation; the expected-trace
        # receipt still follows complete source metadata registration below.
        lines[instrument] <- paste0(
            "setHook(packageEvent(pkg, 'onLoad'), function(...) (",
            process_register,
            ")(Sys.getenv('COVERAGE_DIR', ",
            encodeString(lib, quote = '"'),
            ")))\n",
            "# epwshiftr coverage dynamic instrumentation\n",
            trace_hook
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
    # The same strict collector serves the existing CI entry point and shards.
    merge_traces <- function(files) {
        coverage__collect(library_path)$counters
    }
    testthat::local_mocked_bindings(
        add_hooks = add_hooks,
        merge_coverage = merge_traces,
        .package = "covr"
    )
    covr::package_coverage(path, clean = FALSE, ...)
}
# }}}

# Compare source identity without serializing mutable counter values or source
# environments. Original covr keys, source locations and owning functions must
# agree across independently instrumented processes.
# coverage__source_identity {{{
coverage__source_identity <- function(counters) {
    if (is.null(names(counters)) || anyDuplicated(names(counters))) {
        stop("Coverage source metadata has missing or duplicate keys.")
    }
    counters <- counters[order(names(counters))]
    lapply(counters, function(counter) {
        list(
            filename = getSrcFilename(counter$srcref, full.names = TRUE),
            location = as.integer(counter$srcref),
            functions = counter$functions
        )
    })
}
# }}}

# Collect one isolated execution directory. Every registered process must have
# exactly one atomic trace with the complete source key set, including zeros.
# This refuses partial traces rather than silently shrinking the denominator.
# coverage__collect {{{
coverage__collect <- function(
    directory,
    metadata = NULL,
    timeout = 30,
    prepared_identity = NULL,
    values_only = FALSE
) {
    deadline <- Sys.time() + timeout
    repeat {
        expected <- sub(
            "^expected-trace-",
            "covr_trace_",
            list.files(directory, pattern = "^expected-trace-[0-9]+$")
        )
        files <- list.files(
            directory,
            pattern = "^covr_trace_[0-9]+$",
            full.names = TRUE
        )
        processes <- list.files(directory, pattern = "^process-[0-9]+[.]rds$")
        process_traces <- paste0(
            "covr_trace_",
            sub("[.]rds$", "", sub("^process-", "", processes))
        )
        pending <- list.files(
            directory,
            pattern = "^pending-(trace|metadata|process)-"
        )
        if (
            (!length(pending) &&
                setequal(expected, basename(files)) &&
                setequal(expected, process_traces)) ||
                Sys.time() >= deadline
        ) {
            break
        }
        Sys.sleep(0.1)
    }
    if (
        !length(expected) ||
            length(pending) ||
            !setequal(expected, basename(files)) ||
            !setequal(expected, process_traces)
    ) {
        stop(
            "Unfinished or unregistered worker coverage traces remain in ",
            directory
        )
    }
    counters <- readRDS(file.path(directory, "covr-source-metadata.rds"))
    identity <- coverage__source_identity(counters)
    if (
        !is.null(metadata) &&
            !identical(
                identity,
                if (is.null(prepared_identity)) {
                    coverage__source_identity(metadata)
                } else {
                    prepared_identity
                }
            )
    ) {
        stop(
            "Coverage source metadata differs from the prepared package: ",
            directory
        )
    }
    values <- numeric(length(counters))
    manifest <- vector("list", length(files))
    for (index in seq_along(files)) {
        counts <- readRDS(files[[index]])
        # Metadata keys are already unique. Plain named vectors in that exact
        # order need no hash match; attributed numeric inputs retain the legacy
        # indexed assignment so their dimensions/classes cannot enter values.
        same_order <- identical(names(counts), names(counters)) &&
            identical(names(attributes(counts)), "names")
        positions <- if (!same_order) match(names(counts), names(counters))
        if (
            !is.numeric(counts) ||
                is.null(names(counts)) ||
                length(counts) != length(counters) ||
                any(!is.finite(counts)) ||
                any(counts < 0) ||
                (!same_order && anyNA(positions)) ||
                (!same_order && anyDuplicated(names(counts)))
        ) {
            stop(
                "Coverage trace does not match the complete saved source metadata: ",
                files[[index]]
            )
        }
        pid <- as.integer(sub("^covr_trace_", "", basename(files[[index]])))
        process <- readRDS(file.path(
            directory,
            paste0("process-", pid, ".rds")
        ))
        usage <- readRDS(file.path(directory, paste0("resource-", pid, ".rds")))
        if (
            !is.list(process) ||
                !identical(process$pid, pid) ||
                !inherits(process$create_time, "POSIXt") ||
                !inherits(process$registered_at, "POSIXt") ||
                length(process$create_time) != 1L ||
                length(process$registered_at) != 1L ||
                any(
                    !is.finite(as.numeric(c(
                        process$create_time,
                        process$registered_at
                    )))
                ) ||
                process$create_time > process$registered_at
        ) {
            stop(
                "Coverage process identity is missing or invalid: ",
                files[[index]]
            )
        }
        # Query-only registration records startup policy without changing JIT.
        if (
            !is.numeric(process$jit_level) ||
                length(process$jit_level) != 1L ||
                is.na(process$jit_level) ||
                !process$jit_level %in% 0:3 ||
                !is.character(process$jit_env) ||
                length(process$jit_env) != 1L ||
                is.na(process$jit_env)
        ) {
            stop(
                "Coverage process JIT observation is missing or invalid: ",
                files[[index]]
            )
        }
        if (
            !is.list(usage) ||
                !identical(usage$pid, pid) ||
                !identical(
                    names(usage),
                    c("pid", "user", "system", "elapsed")
                ) ||
                any(lengths(usage) != 1L) ||
                !is.numeric(unlist(usage)) ||
                any(!is.finite(unlist(usage))) ||
                any(unlist(usage) < 0)
        ) {
            stop(
                "Coverage process accounting is missing or invalid: ",
                files[[index]]
            )
        }
        if (same_order) {
            values <- values + counts
        } else {
            values[positions] <- values[positions] + counts
        }
        manifest[[index]] <- data.frame(
            directory = directory,
            pid = pid,
            create_time = process$create_time,
            registered_at = process$registered_at,
            jit_level = process$jit_level,
            jit_env = process$jit_env,
            user = usage$user,
            system = usage$system,
            elapsed = usage$elapsed,
            counters = length(counts),
            stringsAsFactors = FALSE
        )
    }
    # Merge needs only validated numeric counts. Keep the legacy collector
    # result available without rebuilding thousands of counter records per shard.
    if (!values_only) {
        for (index in seq_along(counters)) {
            counters[[index]]$value <- values[[index]]
        }
    }
    message(
        "Merged ",
        length(files),
        " complete process coverage traces from ",
        directory
    )
    list(
        counters = if (values_only) NULL else counters,
        counts = stats::setNames(values, names(counters)),
        manifest = do.call(rbind, manifest)
    )
}
# }}}

# Reuse only immutable parser tables from this exact prepared installation.
# Every namespace still runs covr's original trace_environment(), which creates
# fresh zero counters and instruments fresh closures in the current process.
# coverage__load_parse_data {{{
coverage__load_parse_data <- function(path, package) {
    cache <- readRDS(path)
    if (
        !identical(cache$version, 1L) ||
            !identical(cache$package, package) ||
            !identical(cache$r_version, as.character(getRversion())) ||
            !identical(
                cache$covr_version,
                as.character(utils::packageVersion("covr"))
            ) ||
            !is.list(cache$data) ||
            !length(cache$data) ||
            is.null(names(cache$data)) ||
            anyDuplicated(names(cache$data)) ||
            !all(vapply(cache$data, is.data.frame, logical(1L)))
    ) {
        stop(
            "Prepared coverage parser data does not match this R, covr and package."
        )
    }
    target <- get("package_parse_data", asNamespace("covr"))
    if (length(target)) {
        # A namespace reload in the same process can reuse its existing parser
        # tables, but must not replace parser state belonging to another source.
        if (!setequal(names(cache$data), ls(target, all.names = TRUE))) {
            stop(
                "Coverage parser state belongs to a different prepared source."
            )
        }
        return(invisible(FALSE))
    }
    list2env(cache$data, envir = target)
    invisible(TRUE)
}
# }}}

# Build a source-preserving instrumented installation once, outside measured
# execution. The no-test load freezes covr's original exclusions and denominator;
# every measured process still dynamically instruments its own fresh namespace.
# coverage__prepare {{{
coverage__prepare <- function(path, directory, parse_cache = TRUE, ...) {
    if (
        dir.exists(directory) &&
            length(list.files(directory, all.files = TRUE, no.. = TRUE))
    ) {
        stop("Coverage preparation requires an empty directory: ", directory)
    }
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
    library <- file.path(directory, "library")
    cache_path <- if (isTRUE(parse_cache)) {
        file.path(directory, "parse-data.rds")
    } else {
        NULL
    }
    source_map_path <- if (
        isTRUE(parse_cache) &&
            identical(as.character(utils::packageVersion("covr")), "3.6.5")
    ) {
        file.path(directory, "source-map.rds")
    } else {
        NULL
    }
    code <- if (isTRUE(parse_cache)) {
        paste0(
            "saveRDS(list(version = 1L, package = ",
            encodeString(
                read.dcf(file.path(path, "DESCRIPTION"))[[1L, "Package"]],
                quote = '"'
            ),
            ", r_version = as.character(getRversion()), ",
            "covr_version = as.character(utils::packageVersion('covr')), ",
            "data = as.list(get('package_parse_data', asNamespace('covr')), all.names = TRUE)), ",
            encodeString(cache_path, quote = '"'),
            ", compress = FALSE)"
        )
    } else {
        character()
    }
    coverage <- coverage__run(
        path,
        type = "none",
        code = code,
        install_path = library,
        .source_map = source_map_path,
        ...
    )
    if (isTRUE(parse_cache)) {
        # Install the read-only cache hook only after preparation has generated
        # parser tables. Process identity registration remains the first hook.
        package <- attr(coverage, "package")$package
        loader <- file.path(library, package, "R", package)
        lines <- readLines(loader, warn = FALSE)
        instrument <- which(
            lines == "# epwshiftr coverage dynamic instrumentation"
        )
        if (length(instrument) != 1L) {
            stop(
                "The covr instrumentation hook changed; review parser preparation."
            )
        }
        reader <- paste(
            deparse(coverage__load_parse_data, width.cutoff = 500L),
            collapse = "\n"
        )
        read_cache <- paste0(
            "function() (",
            reader,
            ")(",
            encodeString(cache_path, quote = '"'),
            ", pkg)"
        )
        if (!is.null(source_map_path)) {
            recorder <- grep(", record = TRUE))", lines, fixed = TRUE)
            if (length(recorder) != 1L) {
                stop("Could not finalize coverage source-map preparation.")
            }
            # Keep all dynamic instrumentation in the worker. A source-map
            # miss invokes the same validated parser reader through this thunk.
            lines[recorder] <- sub(
                ", record = TRUE))",
                paste0(
                    ", record = FALSE, load_parse_data = ",
                    read_cache,
                    "))"
                ),
                lines[recorder],
                fixed = TRUE
            )
        } else {
            # Without prepared source mappings every trace needs parser data.
            lines[instrument] <- paste0(
                "setHook(packageEvent(pkg, 'onLoad'), function(...) (",
                read_cache,
                ")())\n",
                lines[instrument]
            )
        }
        writeLines(lines, loader)
    }

    metadata <- readRDS(file.path(library, "covr-source-metadata.rds"))
    # Preparation counters are deliberately discarded. They cannot contribute
    # execution credit to any of the independently measured acceptance rounds.
    for (index in seq_along(coverage)) {
        coverage[[index]]$value <- 0
    }
    for (index in seq_along(metadata)) {
        metadata[[index]]$value <- 0
    }
    template <- file.path(directory, "coverage-template.rds")
    source_metadata <- file.path(directory, "source-metadata.rds")
    saveRDS(coverage, template, compress = FALSE)
    saveRDS(metadata, source_metadata, compress = FALSE)
    descriptor <- list(
        version = 1L,
        package = attr(coverage, "package")$package,
        library = library,
        template = template,
        metadata = source_metadata,
        parse_data = cache_path,
        source_map = source_map_path,
        covr_version = as.character(utils::packageVersion("covr")),
        r_version = as.character(getRversion())
    )
    destination <- file.path(directory, "descriptor.rds")
    saveRDS(descriptor, destination)
    invisible(destination)
}
# }}}

# Sum complete process traces across disjoint shards into the unchanged covr
# template. The caller owns test selection, process termination and timing, and
# must provide every shard directory only after its test process has exited.
# coverage__merge {{{
coverage__merge <- function(prepared, trace_directories, timeout = 30) {
    descriptor <- if (is.character(prepared)) readRDS(prepared) else prepared
    if (!identical(descriptor$version, 1L)) {
        stop("Unsupported prepared coverage descriptor.")
    }
    if (
        !identical(
            descriptor$covr_version,
            as.character(utils::packageVersion("covr"))
        ) ||
            !identical(descriptor$r_version, as.character(getRversion()))
    ) {
        stop(
            "Coverage preparation and merge require identical R and covr versions."
        )
    }
    directories <- normalizePath(
        trace_directories,
        winslash = "/",
        mustWork = TRUE
    )
    if (!length(directories) || anyDuplicated(directories)) {
        stop("Coverage merge requires distinct nonempty shard directories.")
    }
    metadata <- readRDS(descriptor$metadata)
    # The prepared metadata is immutable during this merge. Compute its full
    # identity once while independently validating every shard against it.
    prepared_identity <- coverage__source_identity(metadata)
    coverage <- readRDS(descriptor$template)
    positions <- match(names(coverage), names(metadata))
    if (anyNA(positions) || anyDuplicated(names(coverage))) {
        stop(
            "The coverage template does not match the prepared source metadata."
        )
    }
    values <- numeric(length(metadata))
    manifests <- vector("list", length(directories))
    for (index in seq_along(directories)) {
        result <- coverage__collect(
            directories[[index]],
            metadata,
            timeout,
            prepared_identity = prepared_identity,
            values_only = TRUE
        )
        counts <- result$counts
        values[match(names(counts), names(metadata))] <-
            values[match(names(counts), names(metadata))] + counts
        manifests[[index]] <- result$manifest
    }
    for (index in seq_along(coverage)) {
        coverage[[index]]$value <- values[[positions[[index]]]]
    }
    attr(coverage, "trace_manifest") <- do.call(rbind, manifests)
    coverage
}
# }}}

# Escape XML attributes in vector batches. Character references preserve tabs
# and line endings that an XML parser would otherwise normalize to spaces.
# coverage__xml_escape {{{
coverage__xml_escape <- function(value) {
    value <- as.character(value)
    value <- gsub("&", "&amp;", value, fixed = TRUE)
    value <- gsub('"', "&quot;", value, fixed = TRUE)
    value <- gsub("<", "&lt;", value, fixed = TRUE)
    value <- gsub(">", "&gt;", value, fixed = TRUE)
    value <- gsub("\t", "&#9;", value, fixed = TRUE)
    value <- gsub("\n", "&#10;", value, fixed = TRUE)
    gsub("\r", "&#13;", value, fixed = TRUE)
}
# }}}

# Match covr 3.6.5's Cobertura structure and grouping using the report's complete
# line tally. Batch line serialization avoids thousands of XML node mutations.
# coverage__cobertura {{{
coverage__cobertura <- function(
    coverage,
    lines,
    filename,
    timestamp = as.character(Sys.time())
) {
    # Retain percent_coverage()'s arithmetic, including its percent conversion.
    overall <- (sum(lines$value > 0) / length(lines$value)) * 100 / 100
    per_file <- tapply(lines$value, lines$filename, function(x) {
        sum(x > 0) / length(x)
    })
    per_function <- tapply(lines$value, lines$functions, function(x) {
        sum(x > 0) / length(x)
    })
    files <- unique(lines$filename)
    file_rows <- split(seq_len(nrow(lines)), lines$filename)
    function_rows <- split(seq_len(nrow(lines)), lines$functions)
    line_xml <- paste0(
        '<line number="',
        coverage__xml_escape(lines$line),
        '" hits="',
        coverage__xml_escape(lines$value),
        '" branch="false"/>'
    )
    classes <- vector("list", length(files))
    for (index in seq_along(files)) {
        file <- files[[index]]
        rows <- file_rows[[match(file, names(file_rows))]]
        functions <- unique(stats::na.omit(lines$functions[rows]))
        methods <- vector("list", length(functions))
        for (j in seq_along(functions)) {
            name <- functions[[j]]
            # covr groups method lines by name across files. Preserve that
            # convention, including repeated line nodes for same-named methods.
            method_rows <- function_rows[[match(name, names(function_rows))]]
            methods[[j]] <- c(
                paste0(
                    '<method name="',
                    coverage__xml_escape(name),
                    '" signature="" line-rate="',
                    coverage__xml_escape(per_function[name]),
                    '" branch-rate="0" complexity="0"><lines>'
                ),
                line_xml[method_rows],
                '</lines></method>'
            )
        }
        method_xml <- if (length(methods)) {
            c("<methods>", unlist(methods, use.names = FALSE), "</methods>")
        } else {
            "<methods/>"
        }
        classes[[index]] <- c(
            paste0(
                '<class name="',
                coverage__xml_escape(basename(file)),
                '" filename="',
                coverage__xml_escape(file),
                '" line-rate="',
                coverage__xml_escape(per_file[file]),
                '" branch-rate="0" complexity="0">'
            ),
            method_xml,
            '<lines>',
            line_xml[rows],
            '</lines></class>'
        )
    }
    package <- attr(coverage, "package")
    path <- package$path
    if (is.null(path)) {
        path <- attr(coverage, "root")
    }
    sources <- if (is.null(path)) {
        '<sources/>'
    } else {
        paste0(
            '<sources><source>',
            coverage__xml_escape(path),
            '</source></sources>'
        )
    }
    package_name <- if (is.null(package$package)) {
        # xml2 serializes a named NULL attribute as the literal string "NULL".
        ' name="NULL"'
    } else {
        paste0(' name="', coverage__xml_escape(package$package), '"')
    }
    xml <- c(
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<!DOCTYPE coverage SYSTEM "https://raw.githubusercontent.com/cobertura/cobertura/master/cobertura/src/site/htdocs/xml/coverage-04.dtd">',
        paste0(
            '<coverage line-rate="',
            coverage__xml_escape(overall),
            '" branch-rate="0" lines-covered="',
            sum(lines$value > 0),
            '" lines-valid="',
            nrow(lines),
            '" branches-covered="0" branches-valid="0" complexity="0" version="',
            utils::packageVersion("covr"),
            '" timestamp="',
            coverage__xml_escape(timestamp),
            '">'
        ),
        sources,
        paste0(
            '<packages><package',
            package_name,
            ' line-rate="',
            coverage__xml_escape(overall),
            '" branch-rate="0" complexity="0"><classes>'
        ),
        unlist(classes, use.names = FALSE),
        '</classes></package></packages></coverage>'
    )
    writeLines(xml, filename, useBytes = TRUE)
    invisible(filename)
}
# }}}

# Reuse the identical concatenated source's #line matches within one tally only.
# All source slicing, blank/comment rules and aggregation remain owned by covr.
# Unsupported covr versions or helper layouts retain its unmodified implementation.
# coverage__tally_lines {{{
coverage__tally_lines <- function(coverage) {
    namespace <- asNamespace("covr")
    tally <- get("tally_coverage", namespace)
    source_lines <- get("srcfile_lines", namespace)
    source_body <- as.list(body(source_lines))
    expected <- quote({
        lines <- getSrcLines(srcfile, 1, Inf)
        matches <- rex::re_matches(
            lines,
            rex::rex(
                start,
                any_spaces,
                "#line",
                spaces,
                capture(name = "line_number", digit),
                spaces,
                quotes,
                capture(name = "filename", anything),
                quotes
            )
        )
        matches <- na.omit(matches)
    })
    if (
        as.character(utils::packageVersion("covr")) != "3.6.5" ||
            length(source_body) < 4L ||
            !identical(source_body[2:4], as.list(expected)[2:4]) ||
            !grepl(
                "srcfile_lines(srcfile)",
                paste(deparse(body(tally)), collapse = ""),
                fixed = TRUE
            )
    ) {
        return(tally(coverage, by = "line"))
    }
    # Preserve the original matching expressions, including their row names and
    # na.action attributes. Only an exact lines-vector match can reuse them.
    compute <- as.call(c(
        list(quote(`{`)),
        source_body[3:4],
        list(quote(matches))
    ))
    has_lines <- FALSE
    last_lines <- NULL
    last_matches <- NULL
    scope <- new.env(parent = namespace)
    scope$directives <- function(lines) {
        if (has_lines && identical(lines, last_lines)) {
            return(last_matches)
        }
        matches <- eval(compute, list(lines = lines), namespace)
        has_lines <<- TRUE
        last_lines <<- lines
        last_matches <<- matches
        matches
    }
    source_body[[3L]] <- quote(matches <- directives(lines))
    source_body[[4L]] <- NULL
    body(source_lines) <- as.call(source_body)
    environment(source_lines) <- scope
    scope$srcfile_lines <- source_lines
    # These are local closure copies: no locked covr binding or source object
    # changes, and no parsed text or matches survive into the next report.
    environment(tally) <- scope
    tally(coverage, by = "line")
}
# }}}

# Persist the merged covr result before contacting an external service. All
# summaries use covr's line tally, including lines with no recorded execution.
# coverage__report {{{
coverage__report <- function(coverage, path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    saveRDS(coverage, file.path(path, "coverage.rds"))
    manifest <- attr(coverage, "trace_manifest")
    if (!is.null(manifest)) {
        data.table::fwrite(manifest, file.path(path, "trace-manifest.csv"))
    }
    lines <- data.table::as.data.table(coverage__tally_lines(coverage))
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
    coverage__cobertura(coverage, lines, file.path(path, "coverage.xml"))
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
