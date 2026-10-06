# Compare mirai's default local transport with the package's TCP candidate.
# Each configuration runs in an externally supervised process so a native wait
# cannot hang the entire comparison or suppress the other transport's result.

# Bound cooperative waits; the parent separately bounds blocking native calls.
# transport__wait {{{
transport__wait <- function(predicate, label, timeout = 30) {
    deadline <- proc.time()[["elapsed"]] + timeout
    while (!predicate()) {
        if (proc.time()[["elapsed"]] >= deadline) {
            stop("Timed out waiting for ", label, call. = FALSE)
        }
        Sys.sleep(0.01)
    }
}
# }}}

# Execute one transport/worker/dispatcher configuration, recording each timing
# only after validating the returned data or lifecycle state.
# transport__case {{{
transport__case <- function(root, transport, workers, dispatcher, output) {
    pkgload::load_all(root, quiet = TRUE)
    profile <- paste0("transport-check-", Sys.getpid())
    prefix <- file.path(
        output,
        paste(transport, workers, dispatcher, sep = "-")
    )
    timing_path <- paste0(prefix, "-timings.csv")
    markers <- tempfile("mirai-markers-")
    dir.create(markers)
    on.exit(unlink(markers, recursive = TRUE), add = TRUE)
    on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)

    # Append small measurements immediately so evidence survives a later hang.
    record <- function(stage, iteration, bytes, elapsed) {
        row <- data.frame(
            transport = transport,
            workers = workers,
            dispatcher = dispatcher,
            stage = stage,
            iteration = iteration,
            bytes = bytes,
            seconds = elapsed
        )
        exists <- file.exists(timing_path)
        utils::write.table(
            row,
            timing_path,
            sep = ",",
            row.names = FALSE,
            col.names = !exists,
            append = exists
        )
    }
    # Keep production startup for TCP; the default branch is the control arm.
    start <- function(iteration) {
        cat("Starting", transport, workers, dispatcher, iteration, "\n")
        started <- proc.time()[["elapsed"]]
        if (identical(transport, "tcp")) {
            getFromNamespace("downloader__start_pool", "epwshiftr")(
                workers,
                dispatcher = dispatcher,
                .compute = profile
            )
        } else {
            mirai::daemons(workers, dispatcher = dispatcher, .compute = profile)
        }
        stopifnot(identical(
            mirai::status(.compute = profile)$connections,
            workers
        ))
        record("startup", iteration, 0, proc.time()[["elapsed"]] - started)
    }
    # Native shutdown is covered by the external parent-process deadline.
    shutdown <- function(iteration) {
        cat("Closing", transport, workers, dispatcher, iteration, "\n")
        started <- proc.time()[["elapsed"]]
        mirai::daemons(0L, .compute = profile)
        stopifnot(identical(mirai::status(.compute = profile)$connections, 0L))
        record("shutdown", iteration, 0, proc.time()[["elapsed"]] - started)
    }
    # Inspect specific cancellation/timeout codes, rejecting unrelated errors.
    expect_code <- function(job, code) {
        value <- mirai::collect_mirai(job)
        stopifnot(
            mirai::is_error_value(value),
            identical(as.integer(value), code)
        )
    }

    start(0L)
    # A release barrier requires every worker to enter its task before any can
    # finish, proving that the requested number of processes really executes.
    release <- file.path(markers, "release")
    ready <- file.path(markers, paste0("ready-", seq_len(workers)))
    jobs <- lapply(ready, function(marker) {
        mirai::mirai(
            {
                stopifnot(file.create(marker))
                while (!file.exists(release)) {
                    Sys.sleep(0.01)
                }
                Sys.getpid()
            },
            marker = marker,
            release = release,
            .compute = profile
        )
    })
    transport__wait(
        function() all(file.exists(ready)),
        "all workers to execute"
    )
    if (dispatcher) {
        # All workers are occupied: cancellation must remove this queued task
        # without allowing its observable side effect to execute later.
        forbidden <- file.path(markers, "cancelled-queued-task")
        queued <- mirai::mirai(
            file.create(path),
            path = forbidden,
            .compute = profile
        )
        stopifnot(mirai::stop_mirai(queued))
        expect_code(queued, 20L)
    }
    stopifnot(file.create(release))
    pids <- vapply(jobs, mirai::collect_mirai, integer(1L))
    stopifnot(length(unique(pids)) == workers)
    value <- mirai::collect_mirai(mirai::mirai(42L, .compute = profile))
    stopifnot(identical(value, 42L))
    if (dispatcher) {
        stopifnot(!file.exists(forbidden))
    }

    failed <- mirai::mirai(stop("expected worker error"), .compute = profile)
    stopifnot(mirai::is_mirai_error(mirai::collect_mirai(failed)))
    stopifnot(identical(
        mirai::collect_mirai(mirai::mirai(42L, .compute = profile)),
        42L
    ))

    # Repeated exact round trips measure serialisation + local communication,
    # not climate processing or an end-to-end EPW speedup. Warm up each size.
    for (bytes in c(1024^2, 16 * 1024^2)) {
        payload <- rep(as.raw(0:255), length.out = bytes)
        warmup <- mirai::mirai(x, x = payload, .compute = profile)
        stopifnot(identical(mirai::collect_mirai(warmup), payload))
        for (iteration in seq_len(5L)) {
            started <- proc.time()[["elapsed"]]
            jobs <- lapply(seq_len(workers), function(i) {
                mirai::mirai(x, x = payload, .compute = profile)
            })
            values <- lapply(jobs, mirai::collect_mirai)
            elapsed <- proc.time()[["elapsed"]] - started
            stopifnot(all(vapply(values, identical, logical(1L), y = payload)))
            record("roundtrip", iteration, bytes, elapsed)
        }
    }
    shutdown(0L)

    # Exercise ten start/stop cycles. Alternating interrupt paths wait for a
    # worker-written marker, so an executing task is cancelled or times out.
    for (iteration in seq_len(10L)) {
        start(iteration)
        if (dispatcher) {
            marker <- file.path(markers, paste0("executing-", iteration))
            timed <- iteration %% 2L == 0L
            job <- mirai::mirai(
                {
                    stopifnot(file.create(marker))
                    Sys.sleep(60)
                },
                marker = marker,
                .compute = profile,
                .timeout = if (timed) 1500L else NULL
            )
            transport__wait(function() file.exists(marker), "task execution")
            if (!timed) {
                stopifnot(mirai::stop_mirai(job))
            }
            expect_code(job, if (timed) 5L else 20L)
        } else {
            stopifnot(identical(
                mirai::collect_mirai(mirai::mirai(42L, .compute = profile)),
                42L
            ))
        }
        shutdown(iteration)
    }
    # A fresh pool remains usable after the repeated cancellation/shutdown path.
    start(11L)
    stopifnot(identical(
        mirai::collect_mirai(mirai::mirai(42L, .compute = profile)),
        42L
    ))
    shutdown(11L)
    TRUE
}
# }}}

# Run all control/candidate cases, retaining per-case failures and raw timings.
# Compare transports on the same CI host and alternate which goes first.
# transport__main {{{
transport__main <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    output <- if (length(args)) args[[1L]] else "transport-results"
    # A fresh output directory prevents timings from separate runs being mixed.
    if (
        dir.exists(output) &&
            length(list.files(output, all.files = TRUE, no.. = TRUE))
    ) {
        stop("Use an empty transport-results directory.", call. = FALSE)
    }
    root <- normalizePath(".", mustWork = TRUE)
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    output <- normalizePath(output, mustWork = TRUE)
    libraries <- .libPaths()
    Sys.setenv(R_LIBS_USER = paste(libraries, collapse = .Platform$path.sep))
    writeLines(
        c(
            capture.output(sessionInfo()),
            capture.output(Sys.info()),
            paste("mirai:", utils::packageVersion("mirai")),
            paste("nanonext:", utils::packageVersion("nanonext")),
            capture.output(nanonext::nng_version()),
            paste("Commit:", Sys.getenv("GITHUB_SHA", "local")),
            paste("Libraries:", libraries)
        ),
        file.path(output, "runtime.txt")
    )
    configurations <- expand.grid(
        workers = c(1L, 2L),
        dispatcher = c(FALSE, TRUE),
        KEEP.OUT.ATTRS = FALSE
    )
    results <- vector("list", nrow(configurations) * 2L)
    index <- 0L
    for (i in seq_len(nrow(configurations))) {
        order <- if (i %% 2L) c("default", "tcp") else c("tcp", "default")
        for (transport in order) {
            index <- index + 1L
            workers <- configurations$workers[[i]]
            dispatcher <- configurations$dispatcher[[i]]
            name <- paste(transport, workers, dispatcher, sep = "-")
            message("Checking ", name)
            process <- callr::r_bg(
                function(root, transport, workers, dispatcher, output) {
                    source(file.path(root, "tools", "check-mirai-transports.R"))
                    transport__case(
                        root,
                        transport,
                        workers,
                        dispatcher,
                        output
                    )
                },
                args = list(root, transport, workers, dispatcher, output),
                libpath = libraries,
                supervise = TRUE,
                stdout = file.path(output, paste0(name, "-stdout.log")),
                stderr = file.path(output, paste0(name, "-stderr.log"))
            )
            # Never leave this comparison's child tree running if interrupted.
            on.exit(if (process$is_alive()) process$kill_tree(), add = TRUE)
            deadline <- proc.time()[["elapsed"]] + 240
            timed_out <- FALSE
            while (process$is_alive()) {
                if (proc.time()[["elapsed"]] >= deadline) {
                    timed_out <- TRUE
                    process$kill_tree()
                    break
                }
                process$poll_io(100)
            }
            error <- if (timed_out) {
                "Configuration exceeded the 240 second process deadline"
            } else {
                tryCatch(
                    {
                        stopifnot(isTRUE(process$get_result()))
                        ""
                    },
                    error = function(error) conditionMessage(error)
                )
            }
            results[[index]] <- data.frame(
                transport = transport,
                workers = workers,
                dispatcher = dispatcher,
                passed = !nzchar(error),
                error = error
            )
            utils::write.csv(
                do.call(rbind, results[seq_len(index)]),
                file.path(output, "results.csv"),
                row.names = FALSE
            )
            if (nzchar(error)) message(name, ": ", error)
        }
    }
    results <- do.call(rbind, results)
    paths <- list.files(output, pattern = "-timings[.]csv$", full.names = TRUE)
    summary <- c(
        "# mirai transport comparison",
        "",
        "Lifecycle results:",
        "",
        capture.output(print(results, row.names = FALSE)),
        "",
        "Median seconds; round trips include serialisation and both directions.",
        "These measurements do not estimate end-to-end climate workflow performance.",
        ""
    )
    if (length(paths)) {
        timings <- do.call(rbind, lapply(paths, utils::read.csv))
        utils::write.csv(
            timings,
            file.path(output, "timings.csv"),
            row.names = FALSE
        )
        medians <- stats::aggregate(
            seconds ~ transport + workers + dispatcher + stage + bytes,
            data = timings,
            FUN = stats::median
        )
        utils::write.csv(
            medians,
            file.path(output, "medians.csv"),
            row.names = FALSE
        )
        summary <- c(
            summary,
            "```text",
            capture.output(print(medians, row.names = FALSE)),
            "```"
        )
    }
    writeLines(summary, file.path(output, "summary.md"))
    step_summary <- Sys.getenv("GITHUB_STEP_SUMMARY")
    if (nzchar(step_summary)) {
        cat(summary, sep = "\n", file = step_summary, append = TRUE)
    }
    if (!all(results$passed)) {
        stop("Transport comparison failed; see retained logs.", call. = FALSE)
    }
    message("All transport configurations passed.")
}
# }}}

if (sys.nframe() == 0L) {
    transport__main()
}

# vim: fdm=marker :
