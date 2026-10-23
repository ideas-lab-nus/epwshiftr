# Keep native coverage test waits outside the supervisor's R process. Every exit
# retains progress and owned process identities; a timeout is never a pass.
coverage__supervise <- function(root, output, timeout = 3600) {
    root <- normalizePath(root, mustWork = TRUE)
    entry <- sub(
        "^--file=",
        "",
        grep("^--file=", commandArgs(), value = TRUE)[[1L]]
    )
    adapter <- normalizePath(
        file.path(dirname(entry), "coverage.R"),
        mustWork = TRUE
    )
    if (
        dir.exists(output) &&
            length(list.files(output, all.files = TRUE, no.. = TRUE))
    ) {
        stop("Coverage output must be a new or empty directory.", call. = FALSE)
    }
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    output <- normalizePath(output, mustWork = TRUE)
    diagnostics <- file.path(output, "diagnostics")
    dir.create(diagnostics, showWarnings = FALSE)
    libraries <- .libPaths()
    interval <- as.numeric(Sys.getenv("EPWSHIFTR_COVERAGE_PROGRESS", "30"))
    stall <- as.numeric(Sys.getenv("EPWSHIFTR_COVERAGE_STALL", "300"))
    stopifnot(
        is.finite(timeout),
        timeout > 0,
        is.finite(interval),
        interval > 0,
        is.finite(stall),
        stall > 0
    )
    Sys.setenv(R_LIBS_USER = paste(libraries, collapse = .Platform$path.sep))
    writeLines(
        c(
            capture.output(sessionInfo()),
            "",
            libraries,
            "",
            vapply(
                c("covr", "testthat", "mirai", "nanonext", "ps", "processx"),
                function(package) {
                    paste(package, as.character(utils::packageVersion(package)))
                },
                character(1L)
            )
        ),
        file.path(diagnostics, "runtime.txt")
    )
    message(
        "Coverage artifacts: ",
        output,
        "; total timeout: ",
        timeout,
        " seconds"
    )
    process <- callr::r_bg(
        function(root, output, adapter, libraries) {
            .libPaths(libraries)
            source(adapter)
            coverage <- coverage__run(
                root,
                quiet = FALSE,
                install_path = file.path(output, "covr-library")
            )
            coverage__report(coverage, file.path(output, "coverage-report"))
            TRUE
        },
        args = list(root, output, adapter, libraries),
        libpath = libraries,
        stdout = file.path(diagnostics, "stdout.log"),
        stderr = file.path(diagnostics, "stderr.log"),
        supervise = TRUE
    )
    main_handle <- ps::ps_handle(process$get_pid())
    known <- new.env(parent = emptyenv())
    registered <- new.env(parent = emptyenv())
    started <- Sys.time()
    last_change <- started
    last_report <- started - interval
    previous <- NULL
    sampled <- FALSE
    status <- "interrupted"
    reason <- NULL

    # Remember creation times as well as PIDs, including workers later reparented
    # by the OS. Never signal an unrelated process that reuses a recorded PID.
    remember <- function() {
        handles <- tryCatch(
            {
                if (ps::ps_is_running(main_handle)) {
                    c(
                        list(main_handle),
                        ps::ps_children(main_handle, recursive = TRUE)
                    )
                } else {
                    list()
                }
            },
            error = function(e) list()
        )
        # mirai daemons can detach before the next process-tree poll. Their
        # immutable records come only from this run's instrumented package.
        records <- list.files(
            file.path(output, "covr-library"),
            pattern = "^expected-trace-",
            full.names = TRUE
        )
        for (path in records[!records %in% ls(registered)]) {
            handle <- tryCatch(
                {
                    identity <- readRDS(path)
                    ps::ps_handle(identity$pid, identity$created)
                },
                error = function(e) NULL
            )
            if (!is.null(handle)) {
                handles[[length(handles) + 1L]] <- handle
                registered[[path]] <- TRUE
            }
        }
        for (handle in handles) {
            key <- paste(
                ps::ps_pid(handle),
                ps::ps_create_time(handle),
                sep = "-"
            )
            known[[key]] <- handle
        }
        Filter(
            function(handle) {
                tryCatch(ps::ps_is_running(handle), error = function(e) FALSE)
            },
            as.list(known)
        )
    }
    # Append lightweight CPU snapshots; these distinguish active instrumentation
    # from idle native waits without guessing from the last test output line.
    snapshot <- function(handles) {
        rows <- lapply(handles, function(handle) {
            tryCatch(
                {
                    cpu <- ps::ps_cpu_times(handle)
                    data.frame(
                        time = format(Sys.time(), tz = "UTC", usetz = TRUE),
                        pid = ps::ps_pid(handle),
                        parent = ps::ps_ppid(handle),
                        created = ps::ps_create_time(handle),
                        name = ps::ps_name(handle),
                        status = ps::ps_status(handle),
                        user = cpu[["user"]],
                        system = cpu[["system"]]
                    )
                },
                error = function(e) NULL
            )
        })
        rows <- Filter(Negate(is.null), rows)
        if (length(rows)) {
            path <- file.path(diagnostics, "processes.csv")
            utils::write.table(
                do.call(rbind, rows),
                path,
                sep = ",",
                row.names = FALSE,
                col.names = !file.exists(path),
                append = file.exists(path)
            )
        }
        cat(sprintf(
            "Coverage elapsed %.0fs; %d owned processes.\n",
            as.numeric(difftime(Sys.time(), started, units = "secs")),
            length(handles)
        ))
        flush.console()
    }
    # Copy bounded test output during the run, so cancellation artifacts do not
    # depend on covr returning or on an R exit handler completing.
    progress <- function() {
        paths <- list.files(
            file.path(output, "covr-library"),
            pattern = "[.]Rout([.]fail)?$",
            recursive = TRUE,
            full.names = TRUE
        )
        paths <- c(file.path(diagnostics, c("stdout.log", "stderr.log")), paths)
        info <- file.info(paths)
        signature <- paste(
            paths,
            info$size,
            as.numeric(info$mtime),
            collapse = "|"
        )
        if (!identical(signature, previous)) {
            previous <<- signature
            last_change <<- Sys.time()
            for (path in paths) {
                if (!file.exists(path)) {
                    next
                }
                lines <- readLines(path, warn = FALSE)
                if (endsWith(path, ".Rout") || endsWith(path, ".Rout.fail")) {
                    relative <- substring(path, nchar(output) + 2L)
                    saved <- gsub("[/\\\\:]", "-", relative)
                    writeLines(
                        tail(lines, 2000L),
                        file.path(diagnostics, saved)
                    )
                }
                if (length(lines)) {
                    cat(
                        basename(path),
                        ":\n",
                        paste(tail(lines, 6L), collapse = "\n"),
                        "\n",
                        sep = ""
                    )
                }
            }
        }
    }
    # Native sampling itself has a per-process bound. Samples are taken before
    # terminating this run's workers, not inferred from a generic test timeout.
    sample_owned <- function(handles, label) {
        if (!identical(Sys.info()[["sysname"]], "Darwin")) {
            return(invisible(NULL))
        }
        for (handle in handles) {
            tryCatch(
                {
                    if (!grepl("^R(script)?$", ps::ps_name(handle))) {
                        next
                    }
                    pid <- ps::ps_pid(handle)
                    processx::run(
                        "/usr/bin/sample",
                        c(
                            as.character(pid),
                            "1",
                            "-file",
                            file.path(
                                diagnostics,
                                paste0("sample-", label, "-", pid, ".txt")
                            )
                        ),
                        timeout = 5,
                        error_on_status = FALSE
                    )
                },
                error = function(e) {
                    message("Sample unavailable: ", conditionMessage(e))
                }
            )
        }
    }
    # Cleanup also covers ordinary child errors and interrupts. Saved handles
    # retain ownership after a worker detaches from the immediate process tree.
    on.exit(
        {
            handles <- remember()
            try(progress(), silent = TRUE)
            try(snapshot(handles), silent = TRUE)
            if (length(handles)) {
                if (!identical(status, "success")) {
                    try(sample_owned(handles, status), silent = TRUE)
                }
                try(process$kill_tree(), silent = TRUE)
                for (handle in rev(handles)) {
                    try(ps::ps_kill(handle), silent = TRUE)
                }
                process$wait(5000)
                deadline <- Sys.time() + 5
                remaining <- remember()
                while (length(remaining) && Sys.time() < deadline) {
                    Sys.sleep(.05)
                    remaining <- remember()
                }
                if (length(remaining)) {
                    status <- "cleanup-failure"
                    reason <- paste(
                        "Could not confirm owned process exit:",
                        paste(
                            vapply(remaining, ps::ps_pid, integer(1L)),
                            collapse = ","
                        )
                    )
                }
            }
            saveRDS(
                list(
                    status = status,
                    reason = reason,
                    started = started,
                    finished = Sys.time()
                ),
                file.path(output, "status.rds")
            )
            writeLines(
                c(paste("Status:", status), paste("Reason:", reason)),
                file.path(diagnostics, "status.txt")
            )
            if (identical(status, "cleanup-failure")) {
                stop(reason, call. = FALSE)
            }
        },
        add = TRUE
    )
    while (process$is_alive()) {
        handles <- remember()
        now <- Sys.time()
        if (
            as.numeric(difftime(now, last_report, units = "secs")) >= interval
        ) {
            progress()
            snapshot(handles)
            last_report <- now
        }
        if (
            !sampled &&
                as.numeric(difftime(now, last_change, units = "secs")) >= stall
        ) {
            sample_owned(handles, "stalled")
            sampled <- TRUE
        }
        if (as.numeric(difftime(now, started, units = "secs")) >= timeout) {
            status <- "timeout"
            reason <- paste(
                "Coverage exceeded",
                timeout,
                "seconds; incomplete coverage is not a pass."
            )
            stop(reason, call. = FALSE)
        }
        process$poll_io(1000)
    }
    result <- tryCatch(process$get_result(), error = function(error) {
        status <<- "failure"
        reason <<- conditionMessage(error)
        stop(error)
    })
    report <- file.path(output, "coverage-report", "summary.md")
    summary <- Sys.getenv("GITHUB_STEP_SUMMARY")
    if (nzchar(summary)) {
        cat(readLines(report), sep = "\n", file = summary, append = TRUE)
    }
    status <- "success"
    invisible(result)
}

# Resolve the adapter from the executed script, including fixture package roots.
coverage__main <- function() {
    args <- commandArgs(TRUE)
    root <- if (length(args)) args[[1L]] else "."
    output <- if (length(args) > 1L) {
        args[[2L]]
    } else {
        file.path(Sys.getenv("RUNNER_TEMP", tempdir()), "coverage-run")
    }
    timeout <- if (length(args) > 2L) {
        as.numeric(args[[3L]])
    } else {
        as.numeric(Sys.getenv("EPWSHIFTR_COVERAGE_TIMEOUT", "3600"))
    }
    coverage__supervise(root, output, timeout)
}

coverage__main()

# vim: fdm=marker :
