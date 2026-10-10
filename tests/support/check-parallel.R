# Select conservative CI parallelism from total physical memory and logical
# cores. Four coverage shards peaked at 18.8 GB locally; reserving 6 GiB per
# shard leaves headroom for workers and instrumentation. Explicit CLI choices
# bypass this default. Injectable observations keep resource boundaries testable.
checks__default_shards <- function(
    maximum = 3L,
    memory_bytes = ps::ps_system_memory()[["total"]],
    cores = parallel::detectCores(logical = TRUE)
) {
    if (
        !is.numeric(maximum) ||
            length(maximum) != 1L ||
            !is.finite(maximum) ||
            maximum < 1 ||
            maximum > .Machine$integer.max ||
            maximum != floor(maximum)
    ) {
        stop("maximum must be a positive integer.")
    }
    # Unknown resources must not make shared CI runners overcommit memory.
    memory_bytes <- tryCatch(memory_bytes, error = function(e) NA_real_)
    cores <- tryCatch(cores, error = function(e) NA_real_)
    if (
        !is.numeric(memory_bytes) ||
            length(memory_bytes) != 1L ||
            !is.finite(memory_bytes) ||
            memory_bytes <= 0
    ) {
        memory_bytes <- 0
    }
    if (
        !is.numeric(cores) ||
            length(cores) != 1L ||
            !is.finite(cores) ||
            cores < 1
    ) {
        cores <- 1
    }
    as.integer(max(
        1,
        min(maximum, floor(cores), floor(memory_bytes / 1024^3 / 6))
    ))
}

# Assign complete test files once using measured costs, retaining alphabetical
# execution within each process so the serial suite's ordering stays predictable.
checks__partition <- function(files, shards, durations = NULL) {
    costs <- rep(1, length(files))
    names(costs) <- files
    if (!is.null(durations)) {
        matched <- match(files, durations$file)
        known <- !is.na(matched)
        costs[known] <- pmax(0.001, durations$elapsed[matched[known]])
        if (any(known)) costs[!known] <- stats::median(costs[known])
    }
    groups <- rep(list(character()), shards)
    loads <- numeric(shards)
    for (file in names(sort(costs, decreasing = TRUE))) {
        group <- which.min(loads)
        groups[[group]] <- c(groups[[group]], file)
        loads[[group]] <- loads[[group]] + costs[[file]]
    }
    lapply(groups, sort)
}

# Execute installed tests in a private working directory. The reporter preserves
# every testthat result while recording file time without enabling Rprof overhead.
checks__worker <- function(directory, files, profile) {
    started <- proc.time()
    setwd(file.path(directory, "tests"))
    data.table::setDTthreads(1L)
    options(cli.hyperlink = FALSE)
    Sys.setenv(TESTTHAT_IS_CHECKING = "true")
    library(epwshiftr)
    timing <- list()
    # ListReporter remains the source of assertion outcomes; timing only observes
    # file boundaries and does not alter test execution or failure handling.
    Reporter <- R6::R6Class(
        "TimedCheckReporter",
        inherit = testthat::ListReporter,
        public = list(
            current_file = NULL,
            file_start = NULL,
            start_file = function(name) {
                self$current_file <- name
                self$file_start <- proc.time()
                super$start_file(name)
            },
            end_file = function() {
                elapsed <- proc.time() - self$file_start
                timing[[length(timing) + 1L]] <<- data.frame(
                    file = self$current_file,
                    elapsed = unname(elapsed[["elapsed"]]),
                    cpu = unname(sum(elapsed[c("user.self", "sys.self")]))
                )
                write.csv(
                    do.call(rbind, timing),
                    file.path(directory, "files-progress.csv"),
                    row.names = FALSE
                )
                super$end_file()
            }
        )
    )
    reporter <- Reporter$new()
    if (profile) {
        Rprof(file.path(directory, "Rprof.out"), interval = 0.02)
    }
    error <- tryCatch(
        {
            testthat::test_check(
                "epwshiftr",
                filter = paste0(
                    "^(",
                    paste(
                        sub("[.]R$", "", sub("^test-", "", files)),
                        collapse = "|"
                    ),
                    ")$"
                ),
                reporter = reporter,
                stop_on_failure = FALSE
            )
            NULL
        },
        error = function(e) conditionMessage(e)
    )
    if (profile) {
        Rprof(NULL)
    }
    results <- reporter$get_results()
    saveRDS(results, file.path(directory, "results.rds"))
    if (length(timing)) {
        write.csv(
            do.call(rbind, timing),
            file.path(directory, "files.csv"),
            row.names = FALSE
        )
    }
    outcome <- list(
        error = error,
        timing = proc.time() - started,
        pid = Sys.getpid(),
        create_time = ps::ps_create_time(ps::ps_handle())
    )
    saveRDS(outcome, file.path(directory, "outcome.rds"))
    outcome
}

# Use full OS creation-time precision so PID reuse cannot combine ownership or
# CPU accounting from two different processes.
checks__identity <- function(pid, create_time) {
    paste(pid, sprintf("%.9f", as.numeric(create_time)), sep = "-")
}

# Keep monitor diagnostics separate from process ownership. Append failures as
# they happen so an interrupted or timed-out run retains the exact failing call.
checks__monitor <- function(
    log = NULL,
    windows = .Platform$OS.type == "windows"
) {
    monitor <- new.env(parent = emptyenv())
    monitor$log <- log
    monitor$counts <- integer()
    monitor$pending <- character()
    monitor$windows <- windows
    monitor$timing <- list()
    monitor$parents <- new.env(parent = emptyenv())
    monitor$root_identities <- character()
    monitor$topology_gaps <- new.env(parent = emptyenv())
    monitor$last_missing <- NULL
    monitor$since <- Inf
    monitor$receipts_at <- -Inf
    monitor$sampled_at <- -Inf
    monitor$samples <- list(count = 0L, max_gap = 0, overruns = 0L)
    monitor$operations <- c(
        ppid_queries = 0L,
        ppid_hits = 0L,
        receipt_scans = 0L,
        receipt_reads = 0L
    )
    codes <- ps::errno()
    monitor$permission_errno <- codes$value[
        codes$name %in% c("EPERM", "EACCES")
    ]
    monitor
}

# Keep aggregate monitoring cost visible without a sampling profiler. Timed
# stages do not overlap: observe includes its own liveness/resource queries.
checks__monitor_time <- function(monitor, stage, started) {
    duration <- (proc.time() - started)[c("elapsed", "user.self", "sys.self")]
    previous <- monitor$timing[[stage]]
    monitor$timing[[stage]] <- if (is.null(previous)) {
        duration
    } else {
        previous + duration
    }
    invisible(NULL)
}

# Record both explicit PS failures and omitted owned identities in a discovery
# table. A missing row is diagnostic data, not evidence that a process exited.
checks__record_missing <- function(
    error,
    monitor,
    operation,
    pid,
    create_time,
    call
) {
    count <- if (operation %in% names(monitor$counts)) {
        monitor$counts[[operation]] + 1L
    } else {
        1L
    }
    monitor$counts[[operation]] <- count
    monitor$last_missing <- list(
        message = conditionMessage(error),
        error_class = paste(class(error), collapse = "/")
    )
    if (!is.null(monitor$log)) {
        record <- data.frame(
            time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6%z"),
            operation = operation,
            call = paste(deparse(call), collapse = " "),
            pid = pid,
            create_time = sprintf("%.9f", as.numeric(create_time)),
            error_class = paste(class(error), collapse = "/"),
            errno = if (is.null(error$errno)) NA_integer_ else error$errno,
            message = conditionMessage(error),
            missing_count = count
        )
        append <- file.exists(monitor$log)
        utils::write.table(
            record,
            monitor$log,
            sep = ",",
            row.names = FALSE,
            col.names = !append,
            append = append
        )
    }
    invisible(NULL)
}

# Retry only known PS permission failures on the next outer polling iteration.
# A disappearing process is also a missing sample, never positive exit evidence.
checks__ps_query <- function(
    expr,
    monitor,
    operation,
    pid,
    create_time = NA_real_,
    missing_process = FALSE,
    missing_value = NULL
) {
    call <- substitute(expr)
    tryCatch(
        expr,
        ps_error = function(error) {
            permission <- inherits(error, "access_denied") ||
                (inherits(error, "os_error") &&
                    length(error$errno) == 1L &&
                    error$errno %in% monitor$permission_errno)
            disappeared <- missing_process && inherits(error, "no_such_process")
            if (!permission && !disappeared) {
                stop(error)
            }
            checks__record_missing(
                error,
                monitor,
                operation,
                pid,
                create_time,
                call
            )
            if (disappeared) missing_value else NULL
        }
    )
}

# Retain one OS handle per process lifetime, including exited processes whose
# final CPU totals are still needed in the report.
checks__track <- function(handle, registry) {
    pid <- ps::ps_pid(handle)
    create_time <- ps::ps_create_time(handle)
    identity <- checks__identity(pid, create_time)
    if (is.null(registry[[identity]])) {
        registry[[identity]] <- list(
            handle = handle,
            pid = pid,
            create_time = create_time,
            cpu = 0,
            rss = 0,
            exited = FALSE,
            verified_at = as.POSIXct(
                NA_real_,
                origin = "1970-01-01",
                tz = "UTC"
            )
        )
    }
    invisible(identity)
}

# A failed OS query is not proof of exit. Once the creation-time-bound handle
# is confirmed dead, cache that result and avoid repeated expensive queries.
checks__living <- function(registry, monitor) {
    identities <- ls(registry)
    living <- stats::setNames(logical(length(identities)), identities)
    # Windows ps_is_running() turns native permission failures into FALSE.
    # A complete PID enumeration plus a fresh lifetime read avoids that shortcut.
    pids <- if (
        monitor$windows &&
            any(vapply(
                as.list(registry),
                function(entry) !entry$exited,
                logical(1L)
            ))
    ) {
        checks__ps_query(ps::ps_pids(), monitor, "liveness_pids", NA_integer_)
    } else {
        NULL
    }
    for (identity in identities) {
        entry <- registry[[identity]]
        if (entry$exited) {
            next
        }
        running <- if (monitor$windows) {
            checks__windows_alive(entry, pids, monitor)
        } else {
            checks__ps_query(
                ps::ps_is_running(entry$handle),
                monitor,
                "liveness",
                entry$pid,
                entry$create_time
            )
        }
        # Unknown status must keep the run waiting within its original deadline.
        living[[identity]] <- is.null(running) || running
        if (!living[[identity]]) {
            entry$exited <- TRUE
            entry$verified_at <- Sys.time()
            registry[[identity]] <- entry
        }
    }
    living
}

# A successful PID enumeration can prove absence; a successful new handle can
# prove PID reuse. Even no_such_process while the PID was present stays unknown,
# since ps's Windows create-time query also uses that error for access denial.
checks__windows_alive <- function(entry, pids, monitor) {
    if (is.null(pids)) {
        return(NULL)
    }
    if (!entry$pid %in% pids) {
        return(FALSE)
    }
    current <- checks__ps_query(
        ps::ps_handle(entry$pid),
        monitor,
        "liveness_identity",
        entry$pid,
        entry$create_time,
        missing_process = TRUE
    )
    if (is.null(current)) {
        return(NULL)
    }
    identical(
        checks__identity(entry$pid, ps::ps_create_time(current)),
        checks__identity(entry$pid, entry$create_time)
    )
}

# Windows reports the process creator PID for a lifetime. Cache only successful
# topology queries; current identities and liveness are always read afresh. Unix
# parents can change after reparenting and must use the uncached table instead.
checks__windows_snapshot <- function(since, monitor) {
    snapshot <- checks__ps_query(
        ps::ps(after = since, columns = c("pid", "created", "ps_handle")),
        monitor,
        "children_snapshot",
        NA_integer_,
        since
    )
    if (is.null(snapshot)) {
        return(NULL)
    }
    snapshot$ppid <- rep(NA_integer_, nrow(snapshot))
    identities <- checks__identity(snapshot$pid, snapshot$created)
    for (index in seq_len(nrow(snapshot))) {
        identity <- identities[[index]]
        gap <- paste0("topology:", identity)
        parent <- monitor$parents[[identity]]
        if (is.null(parent)) {
            monitor$operations[["ppid_queries"]] <-
                monitor$operations[["ppid_queries"]] + 1L
            parent <- checks__ps_query(
                ps::ps_ppid(snapshot$ps_handle[[index]]),
                monitor,
                "children_ppid",
                snapshot$pid[[index]],
                snapshot$created[[index]],
                missing_process = TRUE
            )
            # A denied/disappearing query remains missing, never an empty
            # relationship or exit proof. Retry that identity at the next tick.
            if (!is.null(parent) && !is.na(parent)) {
                monitor$parents[[identity]] <- parent
                previous <- monitor$topology_gaps[[identity]]
                if (!is.null(previous)) {
                    previous$status <- "resolved-parent"
                    monitor$topology_gaps[[identity]] <- previous
                }
                monitor$pending <- monitor$pending[
                    names(monitor$pending) != gap
                ]
            } else {
                # Do not treat unknown ancestry as either an unrelated process
                # or an exited one. Its independent audit needs positive proof.
                checks__topology_missing(
                    identity,
                    snapshot$ps_handle[[index]],
                    monitor
                )
            }
        } else {
            monitor$operations[["ppid_hits"]] <-
                monitor$operations[["ppid_hits"]] + 1L
        }
        if (!is.null(parent)) snapshot$ppid[[index]] <- parent
    }
    snapshot
}

# Keep failed creator queries distinct from positively owned process lifetimes.
# The diagnostic survives a later positive exit without inventing its ancestry.
checks__topology_missing <- function(identity, handle, monitor) {
    previous <- monitor$topology_gaps[[identity]]
    if (is.null(previous)) {
        previous <- list(
            handle = handle,
            pid = ps::ps_pid(handle),
            create_time = ps::ps_create_time(handle),
            status = "pending",
            error = monitor$last_missing$message,
            error_class = monitor$last_missing$error_class,
            observed_at = Sys.time(),
            verified_at = as.POSIXct(
                NA_real_,
                origin = "1970-01-01",
                tz = "UTC"
            ),
            proof = ""
        )
    }
    monitor$topology_gaps[[identity]] <- previous
    monitor$pending[[paste0("topology:", identity)]] <- "children_ppid"
    invisible(NULL)
}

# Unknown recent processes may exit before ancestry is readable. Only raw PID
# absence or a fresh different lifetime can close that unowned gap. A receipt
# that later establishes ownership reopens it: known-owned scan gaps stay strict.
checks__resolve_topology <- function(registry, monitor) {
    pids <- NULL
    enumerated <- FALSE
    for (identity in ls(monitor$topology_gaps)) {
        entry <- monitor$topology_gaps[[identity]]
        if (identical(entry$status, "resolved-parent")) {
            next
        }
        key <- paste0("topology:", identity)
        if (!is.null(registry[[identity]])) {
            entry$status <- "owned-pending"
            monitor$pending[[key]] <- "children_ppid"
        } else if (!identical(entry$status, "unresolved-parent-exit")) {
            if (!enumerated) {
                pids <- checks__ps_query(
                    ps::ps_pids(),
                    monitor,
                    "topology_pids",
                    NA_integer_
                )
                enumerated <- TRUE
            }
            running <- checks__windows_alive(entry, pids, monitor)
            if (identical(running, FALSE)) {
                entry$status <- "unresolved-parent-exit"
                entry$verified_at <- Sys.time()
                entry$proof <- if (entry$pid %in% pids) {
                    "pid-reused"
                } else {
                    "pid-absent"
                }
                monitor$pending <- monitor$pending[
                    names(monitor$pending) != key
                ]
            }
        }
        monitor$topology_gaps[[identity]] <- entry
    }
    invisible(NULL)
}

# Export only durable diagnostic fields, never native handles. This audit also
# persists for incomplete runs and distinguishes closed unknown ancestry gaps.
checks__topology_audit <- function(monitor) {
    identities <- ls(monitor$topology_gaps)
    if (!length(identities)) {
        return(data.frame(
            identity = character(),
            pid = integer(),
            create_time = numeric(),
            status = character(),
            error = character(),
            error_class = character(),
            observed_at = character(),
            verified_at = character(),
            proof = character()
        ))
    }
    do.call(
        rbind,
        lapply(identities, function(identity) {
            entry <- monitor$topology_gaps[[identity]]
            data.frame(
                identity = identity,
                pid = entry$pid,
                create_time = as.numeric(entry$create_time),
                status = entry$status,
                error = entry$error,
                error_class = entry$error_class,
                observed_at = format(
                    entry$observed_at,
                    "%Y-%m-%dT%H:%M:%OS6%z"
                ),
                verified_at = format(
                    entry$verified_at,
                    "%Y-%m-%dT%H:%M:%OS6%z"
                ),
                proof = entry$proof
            )
        })
    )
}

# Share one recent-process discovery table across all roots. On Windows each
# individual PPID query creates a system snapshot; filtering by root lifetime
# before PPID queries avoids repeatedly scanning old unrelated system processes.
checks__discover <- function(roots, registry, monitor) {
    if (!length(roots)) {
        return(invisible(NULL))
    }
    since <- min(vapply(
        roots,
        function(root) as.numeric(ps::ps_create_time(root)),
        numeric(1L)
    ))
    # Never advance the discovery cutoff when an older root exits: a previously
    # inaccessible lifetime may become visible later in this same run.
    monitor$since <- min(monitor$since, since)
    since <- as.POSIXct(monitor$since, origin = "1970-01-01", tz = "UTC")
    snapshot <- if (monitor$windows) {
        checks__windows_snapshot(since, monitor)
    } else {
        checks__ps_query(
            ps::ps(
                after = since,
                columns = c("pid", "ppid", "created", "ps_handle")
            ),
            monitor,
            "children_snapshot",
            NA_integer_,
            since
        )
    }
    # ps() suppresses some inaccessible rows internally. A missing owned root
    # must be checked against raw PIDs instead of being mistaken for an exit.
    pids <- NULL
    for (root in roots) {
        pid <- ps::ps_pid(root)
        key <- checks__identity(pid, ps::ps_create_time(root))
        index <- if (is.null(snapshot)) {
            NA_integer_
        } else {
            match(pid, snapshot$pid)
        }
        valid <- !is.na(index) &&
            identical(
                checks__identity(pid, snapshot$created[[index]]),
                checks__identity(pid, ps::ps_create_time(root))
            )
        if (!valid) {
            if (!is.null(snapshot)) {
                if (is.null(pids)) {
                    pids <- checks__ps_query(
                        ps::ps_pids(),
                        monitor,
                        "discovery_pids",
                        NA_integer_
                    )
                }
                # Preserve ordinary parent-exit races, but never erase an older
                # permission gap just because the parent has since disappeared.
                if (!is.null(pids) && !pid %in% pids) {
                    next
                }
                # A previously observed child may exit and its PID be reused
                # between samples. Positive exit proof must not invent a new
                # scan gap, and must never clear a gap from an earlier failure.
                running <- if (monitor$windows) {
                    checks__windows_alive(
                        list(pid = pid, create_time = ps::ps_create_time(root)),
                        pids,
                        monitor
                    )
                } else {
                    checks__ps_query(
                        ps::ps_is_running(root),
                        monitor,
                        "discovery_liveness",
                        pid,
                        ps::ps_create_time(root)
                    )
                }
                if (identical(running, FALSE)) {
                    next
                }
                checks__record_missing(
                    simpleError(
                        "Owned root missing or changed in process snapshot."
                    ),
                    monitor,
                    "children_identity",
                    pid,
                    ps::ps_create_time(root),
                    quote(ps::ps(after = since, columns = columns))
                )
            }
            monitor$pending[[key]] <- "children_snapshot"
            next
        }
        monitor$pending <- monitor$pending[names(monitor$pending) != key]
        pending <- index
        seen <- rep(FALSE, nrow(snapshot))
        while (length(pending)) {
            current <- pending[[1L]]
            pending <- pending[-1L]
            if (seen[[current]]) {
                next
            }
            seen[[current]] <- TRUE
            checks__track(snapshot$ps_handle[[current]], registry)
            # Validate every edge's lifetime so PID reuse cannot adopt an older
            # unrelated child. Keep all already-owned lifetimes in the registry.
            children <- which(
                snapshot$ppid == snapshot$pid[[current]] &
                    snapshot$created >= snapshot$created[[current]]
            )
            pending <- c(pending, children[!seen[children]])
        }
    }
    invisible(NULL)
}

# Monitor only descendants of this run, retaining handles across parent exit.
# Observed CPU is a lower bound for very short-lived children; peak RSS is the
# sampled sum of resident memory, not the sum of individual lifetime peaks.
checks__observe <- function(processes, registry, monitor) {
    started <- proc.time()
    on.exit(checks__monitor_time(monitor, "observe", started), add = TRUE)
    roots <- list()
    parent_pids <- integer()
    for (process in processes) {
        if (!process$is_alive()) {
            next
        }
        pid <- process$get_pid()
        parent_pids <- c(parent_pids, pid)
        key <- as.character(pid)
        parent <- checks__ps_query(
            ps::ps_handle(process$get_pid()),
            monitor,
            "parent_handle",
            pid,
            missing_process = TRUE,
            missing_value = FALSE
        )
        if (identical(parent, FALSE)) {
            if (monitor$windows) {
                pids <- checks__ps_query(
                    ps::ps_pids(),
                    monitor,
                    "parent_pids",
                    pid
                )
                if (is.null(pids) || pid %in% pids) {
                    monitor$pending[[key]] <- "parent_handle"
                }
            }
            next
        }
        if (is.null(parent)) {
            monitor$pending[[key]] <- "parent_handle"
            next
        }
        # Resolve only this parent's temporary handle gap. Descendant-scan
        # gaps use full lifetime keys and cannot be erased by a recycled PID.
        if (
            key %in%
                names(monitor$pending) &&
                identical(monitor$pending[[key]], "parent_handle")
        ) {
            monitor$pending <- monitor$pending[names(monitor$pending) != key]
        }
        identity <- checks__track(parent, registry)
        monitor$root_identities <- union(monitor$root_identities, identity)
        roots[[length(roots) + 1L]] <- parent
    }
    # A known child can outlive its callr parent and create more descendants.
    # Seed those owned lifetimes too, without bypassing a current parent's
    # failed handle query or using cached topology as evidence of liveness.
    descendant_ids <- setdiff(ls(registry), monitor$root_identities)
    descendants <- Filter(
        function(entry) !entry$exited && !entry$pid %in% parent_pids,
        mget(descendant_ids, envir = registry, inherits = FALSE)
    )
    roots <- c(roots, lapply(descendants, function(entry) entry$handle))
    checks__discover(roots, registry, monitor)
    checks__resolve_topology(registry, monitor)
    rss <- 0
    alive <- 0L
    living <- checks__living(registry, monitor)
    for (identity in names(living)[living]) {
        entry <- registry[[identity]]
        alive <- alive + 1L
        cpu <- checks__ps_query(
            ps::ps_cpu_times(entry$handle),
            monitor,
            "cpu",
            entry$pid,
            entry$create_time,
            missing_process = TRUE
        )
        memory <- checks__ps_query(
            ps::ps_memory_info(entry$handle),
            monitor,
            "rss",
            entry$pid,
            entry$create_time,
            missing_process = TRUE
        )
        if (!is.null(cpu)) {
            entry$cpu <- max(entry$cpu, sum(cpu[c("user", "system")]))
        }
        if (!is.null(memory)) {
            current_rss <- unname(memory[["rss"]])
            entry$rss <- max(entry$rss, current_rss)
            rss <- rss + current_rss
        }
        registry[[identity]] <- entry
    }
    c(rss = rss, processes = alive)
}

# Check one run-wide deadline at phase boundaries. Return only the remaining
# budget so trace collection cannot receive a fresh allowance after tests finish.
checks__deadline <- function(
    elapsed,
    timeout,
    output = "",
    phase = "test execution"
) {
    if (elapsed > timeout) {
        stop(
            "Parallel tests timed out during ",
            phase,
            "; incomplete run retained: ",
            output
        )
    }
    max(0, timeout - elapsed)
}

# Acceptance needs both explicit exits and no unresolved descendant scan gaps.
# Permission failures stay pending within the caller's run-wide deadline.
checks__exit_ready <- function(
    registry,
    monitor,
    elapsed = 0,
    timeout = Inf,
    output = ""
) {
    started <- proc.time()
    on.exit(checks__monitor_time(monitor, "exit_liveness", started), add = TRUE)
    checks__resolve_topology(registry, monitor)
    living <- checks__living(registry, monitor)
    ready <- !any(living) && !length(monitor$pending)
    checks__deadline(
        elapsed + unname((proc.time() - started)[["elapsed"]]),
        timeout,
        output,
        if (ready) {
            "process exit verification"
        } else {
            "Owned workers or descendant scans remain unverified"
        }
    )
    ready
}

# Import coverage process identities even after their immediate parent exits.
# Creation times prevent a recycled PID from being mistaken for an owned worker.
checks__receipts <- function(directories, registry, receipts, monitor) {
    started <- proc.time()
    on.exit(checks__monitor_time(monitor, "receipts", started), add = TRUE)
    monitor$operations[["receipt_scans"]] <- monitor$operations[[
        "receipt_scans"
    ]] +
        1L
    paths <- unlist(
        lapply(directories, function(directory) {
            list.files(
                file.path(directory, "traces"),
                pattern = "^process-[0-9]+[.]rds$",
                full.names = TRUE
            )
        }),
        use.names = FALSE
    )
    for (path in setdiff(paths, ls(receipts))) {
        entry <- readRDS(path)
        monitor$operations[["receipt_reads"]] <- monitor$operations[[
            "receipt_reads"
        ]] +
            1L
        stopifnot(is.numeric(entry$pid), inherits(entry$create_time, "POSIXt"))
        handle <- ps::ps_handle(entry$pid, time = entry$create_time)
        checks__track(handle, registry)
        receipts[[path]] <- entry
    }
    invisible(length(paths))
}

# Durable registration files need one routine directory scan per sample period.
# Terminal decisions and merge boundaries explicitly force a fresh scan.
checks__poll_receipts <- function(
    directories,
    registry,
    receipts,
    monitor,
    force = FALSE,
    now = proc.time()[["elapsed"]]
) {
    if (!force && now - monitor$receipts_at < 2) {
        return(invisible(FALSE))
    }
    monitor$receipts_at <- now
    checks__receipts(directories, registry, receipts, monitor)
    invisible(TRUE)
}

# Schedule from sample starts, and retain actual gaps rather than claiming that
# slow synchronous OS calls can meet a hard two-second bound.
checks__sample_begin <- function(monitor, now = proc.time()[["elapsed"]]) {
    if (is.finite(monitor$sampled_at)) {
        gap <- now - monitor$sampled_at
        monitor$samples$max_gap <- max(monitor$samples$max_gap, gap)
        monitor$samples$overruns <- monitor$samples$overruns +
            as.integer(gap > 2)
    }
    monitor$samples$count <- monitor$samples$count + 1L
    monitor$sampled_at <- now
    invisible(NULL)
}

# Keep completion polling responsive while sleeping no later than the next
# sample deadline. A slow sample causes an immediate next tick, not extra delay.
checks__poll_delay <- function(
    monitor,
    maximum,
    now = proc.time()[["elapsed"]]
) {
    min(maximum, max(0, 2 - (now - monitor$sampled_at)))
}

# Check only an explicit startup policy; absent or other environment values are
# recorded without imposing a platform-dependent default on coverage workers.
checks__validate_jit <- function(manifest, jit_level, jit_env) {
    if (!jit_env %in% as.character(0:3)) {
        return(invisible(NULL))
    }
    expected <- as.integer(jit_env)
    if (
        !identical(as.integer(jit_level), expected) ||
            !all(c("jit_level", "jit_env") %in% names(manifest)) ||
            anyNA(manifest$jit_level) ||
            anyNA(manifest$jit_env) ||
            !all(manifest$jit_level == expected) ||
            !all(manifest$jit_env == jit_env)
    ) {
        stop(
            "Coverage process JIT observations do not match explicit R_ENABLE_JIT=",
            jit_env
        )
    }
    invisible(NULL)
}

# Own one complete run: isolate files and caches, supervise all test processes,
# verify the selected file set, and merge complete coverage before ending time.
checks__run <- function(
    library,
    output,
    shards = 3L,
    durations = NULL,
    coverage = NULL,
    coverage_script = NULL,
    filter = NULL,
    timeout = 1200,
    profile = FALSE,
    test_root = NULL
) {
    jit_level <- compiler::enableJIT(-1)
    jit_env <- Sys.getenv("R_ENABLE_JIT")
    stopifnot(shards >= 1L, shards <= 8L, !dir.exists(output))
    library <- normalizePath(library, winslash = "/", mustWork = TRUE)
    dir.create(output, recursive = TRUE)
    output <- normalizePath(output, winslash = "/")
    installed <- file.path(library, "epwshiftr")
    if (is.null(test_root)) {
        test_root <- file.path(installed, "tests")
    }
    if (!dir.exists(test_root)) {
        test_root <- file.path(installed, "epwshiftr-tests")
    }
    files <- sort(list.files(
        file.path(test_root, "testthat"),
        pattern = "^test-.*\\.[Rr]$"
    ))
    if (!is.null(filter)) {
        files <- files[grepl(filter, files)]
    }
    stopifnot(length(files) >= shards)
    groups <- checks__partition(files, shards, durations)
    stopifnot(identical(sort(unlist(groups, use.names = FALSE)), files))
    saveRDS(groups, file.path(output, "assignment.rds"))
    libs <- unique(c(library, .libPaths()))
    directories <- file.path(output, paste0("shard-", seq_len(shards)))
    for (directory in directories) {
        dir.create(directory)
        dir.create(file.path(directory, "tmp"))
        dir.create(file.path(directory, "traces"))
        dir.create(file.path(directory, "cache"))
        dir.create(file.path(directory, "tests"))
        stopifnot(all(file.copy(
            list.files(
                test_root,
                full.names = TRUE,
                all.files = TRUE,
                no.. = TRUE
            ),
            file.path(directory, "tests"),
            recursive = TRUE
        )))
    }
    processes <- list()
    registry <- new.env(parent = emptyenv())
    monitor <- checks__monitor(file.path(output, "monitor-errors.csv"))
    receipts <- new.env(parent = emptyenv())
    topology_saved <- FALSE
    # Normal audit writes remain inside the timed run. On failure retain the
    # current diagnostics too, without converting cleanup into exit evidence.
    persist_topology <- function() {
        topology <- checks__topology_audit(monitor)
        saveRDS(topology, file.path(output, "monitor-topology-audit.rds"))
        write.csv(
            topology,
            file.path(output, "monitor-topology-audit.csv"),
            row.names = FALSE
        )
        topology_saved <<- TRUE
    }
    on.exit(if (!topology_saved) persist_topology(), add = TRUE)
    peak <- c(rss = 0, processes = 0)
    # Any failure terminates only processes started and observed by this run.
    on.exit(
        {
            for (process in processes) {
                if (process$is_alive()) process$kill_tree()
            }
            for (identity in ls(registry)) {
                handle <- registry[[identity]]$handle
                try(
                    if (ps::ps_is_running(handle)) ps::ps_kill(handle),
                    silent = TRUE
                )
            }
        },
        add = TRUE
    )
    started <- proc.time()
    # Every terminal phase shares the test-process launch clock, including a
    # successful exit check and all synchronous merge/report work.
    remaining <- function(phase) {
        checks__deadline(
            unname((proc.time() - started)[["elapsed"]]),
            timeout,
            output,
            phase
        )
    }
    for (index in seq_len(shards)) {
        directory <- directories[[index]]
        env <- c(
            R_LIBS_USER = paste(libs, collapse = .Platform$path.sep),
            TMPDIR = file.path(directory, "tmp"),
            TMP = file.path(directory, "tmp"),
            TEMP = file.path(directory, "tmp"),
            EPWSHIFTR_CHECK_CACHE = file.path(directory, "cache"),
            COVERAGE_DIR = file.path(directory, "traces"),
            R_COVR = if (is.null(coverage)) "" else "true",
            TZ = "UTC",
            EPWSHIFTR_DB_THREADS = "1",
            OMP_NUM_THREADS = "1",
            OPENBLAS_NUM_THREADS = "1",
            EPWSHIFTR_RUN_LIVE_ESGF = Sys.getenv(
                "EPWSHIFTR_RUN_LIVE_ESGF",
                "false"
            ),
            EPWSHIFTR_RUN_LIVE_ERA5 = Sys.getenv(
                "EPWSHIFTR_RUN_LIVE_ERA5",
                "false"
            )
        )
        processes[[index]] <- callr::r_bg(
            checks__worker,
            args = list(directory, groups[[index]], profile),
            libpath = libs,
            env = env,
            user_profile = FALSE,
            system_profile = FALSE,
            supervise = TRUE,
            stdout = file.path(directory, "stdout.log"),
            stderr = file.path(directory, "stderr.log")
        )
    }
    repeat {
        if (proc.time()[["elapsed"]] - monitor$sampled_at >= 2) {
            checks__sample_begin(monitor)
            peak <- pmax(peak, checks__observe(processes, registry, monitor))
        }
        if (!is.null(coverage)) {
            checks__poll_receipts(directories, registry, receipts, monitor)
        }
        if (unname((proc.time() - started)[["elapsed"]]) > timeout) {
            stop("Parallel tests timed out; incomplete run retained: ", output)
        }
        alive <- vapply(
            processes,
            function(process) process$is_alive(),
            logical(1L)
        )
        if (!any(alive)) {
            if (!is.null(coverage)) {
                checks__poll_receipts(
                    directories,
                    registry,
                    receipts,
                    monitor,
                    force = TRUE
                )
            }
            break
        }
        Sys.sleep(checks__poll_delay(monitor, 0.25))
    }
    outcomes <- lapply(processes, function(process) process$get_result())
    errors <- vapply(
        outcomes,
        function(x) if (is.null(x$error)) "" else x$error,
        character(1L)
    )
    if (any(nzchar(errors))) {
        stop(paste(errors[nzchar(errors)], collapse = "\n"))
    }
    invisible(loadNamespace("testthat"))
    results <- lapply(directories, function(directory) {
        as.data.frame(readRDS(file.path(directory, "results.rds")))
    })
    results <- do.call(rbind, results)
    rownames(results) <- NULL
    saveRDS(results, file.path(output, "results.rds"))
    write.csv(
        results[, setdiff(names(results), "result")],
        file.path(output, "results.csv"),
        row.names = FALSE
    )
    timings <- do.call(
        rbind,
        lapply(directories, function(directory) {
            read.csv(file.path(directory, "files.csv"))
        })
    )
    stopifnot(identical(sort(timings$file), files))
    write.csv(timings, file.path(output, "files.csv"), row.names = FALSE)
    if (sum(results$failed) || any(results$error)) {
        stop("Test failures retained in ", output)
    }
    # Very short test processes may finish between OS samples. Their own exit
    # receipt still supplies the exact lifetime that must appear in the audit.
    for (outcome in outcomes) {
        checks__track(
            ps::ps_handle(outcome$pid, time = outcome$create_time),
            registry
        )
    }
    # Include finalizers and late registrations in the same run-wide deadline.
    # Reuse this wait after merging: a complete trace can appear before its
    # process has actually finished namespace finalization and exited.
    await_exit <- function() {
        if (!is.null(coverage)) {
            checks__poll_receipts(
                directories,
                registry,
                receipts,
                monitor,
                force = TRUE
            )
        }
        repeat {
            if (!is.null(coverage)) {
                checks__poll_receipts(directories, registry, receipts, monitor)
            }
            if (
                checks__exit_ready(
                    registry,
                    monitor,
                    elapsed = unname((proc.time() - started)[["elapsed"]]),
                    timeout = timeout,
                    output = output
                )
            ) {
                # A receipt can arrive since the routine scan while its parent
                # exits. Never return ready with stale registration knowledge.
                if (!is.null(coverage)) {
                    checks__poll_receipts(
                        directories,
                        registry,
                        receipts,
                        monitor,
                        force = TRUE
                    )
                }
                if (
                    checks__exit_ready(
                        registry,
                        monitor,
                        elapsed = unname((proc.time() - started)[["elapsed"]]),
                        timeout = timeout,
                        output = output
                    )
                ) {
                    break
                }
            }
            if (proc.time()[["elapsed"]] - monitor$sampled_at >= 2) {
                checks__sample_begin(monitor)
                peak <<- pmax(
                    peak,
                    checks__observe(processes, registry, monitor)
                )
            }
            Sys.sleep(checks__poll_delay(monitor, 0.1))
        }
    }
    await_exit()
    manifest <- NULL
    if (!is.null(coverage)) {
        source(coverage_script, local = TRUE)
        combined <- coverage__merge(
            coverage,
            file.path(directories, "traces"),
            timeout = min(30, remaining("coverage merge"))
        )
        remaining("coverage merge")
        manifest <- attr(combined, "trace_manifest")
        checks__validate_jit(manifest, jit_level, jit_env)
        manifest_ids <- checks__identity(manifest$pid, manifest$create_time)
        # Import again after collection so processes first seen while traces
        # were being merged also receive a strict, creation-time-bound wait.
        checks__poll_receipts(
            directories,
            registry,
            receipts,
            monitor,
            force = TRUE
        )
        await_exit()
        receipt_ids <- vapply(
            as.list(receipts),
            function(receipt) {
                checks__identity(receipt$pid, receipt$create_time)
            },
            character(1L)
        )
        if (
            anyDuplicated(manifest_ids) ||
                anyDuplicated(receipt_ids) ||
                !setequal(manifest_ids, receipt_ids) ||
                !all(manifest_ids %in% ls(registry))
        ) {
            stop(
                "Coverage process identities changed during collection: ",
                output
            )
        }
    }
    # Persist positive exit evidence instead of relying on cleanup to remove a
    # worker after timing has ended. Unknown OS status always fails above.
    entries <- as.list(registry)
    audit <- do.call(
        rbind,
        lapply(names(entries), function(identity) {
            entry <- entries[[identity]]
            data.frame(
                identity = identity,
                pid = entry$pid,
                create_time = entry$create_time,
                exited = entry$exited,
                verified_at = entry$verified_at,
                coverage_registered = if (is.null(manifest)) {
                    FALSE
                } else {
                    identity %in% manifest_ids
                }
            )
        })
    )
    stopifnot(nrow(audit) > 0L, all(audit$exited), !anyNA(audit$verified_at))
    saveRDS(audit, file.path(output, "process-exit-audit.rds"))
    write.csv(
        audit,
        file.path(output, "process-exit-audit.csv"),
        row.names = FALSE
    )
    if (!is.null(coverage)) {
        remaining("coverage report")
        coverage__report(combined, file.path(output, "coverage-report"))
        remaining("coverage report")
    }
    # Merge OS and exit-time CPU by complete lifetime identity, never bare PID.
    # Short unobserved native children remain an explicit sampling limitation.
    accounting <- do.call(
        rbind,
        lapply(names(entries), function(identity) {
            entry <- entries[[identity]]
            data.frame(
                identity = identity,
                pid = entry$pid,
                create_time = entry$create_time,
                cpu = entry$cpu
            )
        })
    )
    add_cpu <- function(pid, create_time, cpu) {
        identity <- checks__identity(pid, create_time)
        index <- match(identity, accounting$identity)
        if (is.na(index)) {
            accounting <<- rbind(
                accounting,
                data.frame(
                    identity = identity,
                    pid = pid,
                    create_time = create_time,
                    cpu = cpu
                )
            )
        } else {
            accounting$cpu[[index]] <<- max(accounting$cpu[[index]], cpu)
        }
    }
    for (outcome in outcomes) {
        add_cpu(
            outcome$pid,
            outcome$create_time,
            sum(outcome$timing[c("user.self", "sys.self")])
        )
    }
    if (!is.null(manifest)) {
        for (index in seq_len(nrow(manifest))) {
            add_cpu(
                manifest$pid[[index]],
                manifest$create_time[[index]],
                manifest$user[[index]] + manifest$system[[index]]
            )
        }
    }
    write.csv(
        accounting,
        file.path(output, "process-cpu.csv"),
        row.names = FALSE
    )
    persist_topology()
    remaining("final accounting")
    supervisor_cpu <- unname(sum((proc.time() - started)[c(
        "user.self",
        "sys.self"
    )]))
    metrics <- list(
        elapsed = unname((proc.time() - started)[["elapsed"]]),
        test_process_cpu = sum(vapply(
            outcomes,
            function(x) sum(x$timing[c("user.self", "sys.self")]),
            numeric(1L)
        )),
        accounted_total_cpu = sum(accounting$cpu) + supervisor_cpu,
        supervisor_cpu = supervisor_cpu,
        jit_level = jit_level,
        jit_env = jit_env,
        cpu_note = "Exit receipts and scheduled 2-second OS samples (actual gaps reported); short native children may be undercounted.",
        monitor_missing_queries = monitor$counts,
        monitor_operations = monitor$operations,
        monitor_samples = monitor$samples,
        monitor_unresolved_parent_exits = sum(
            checks__topology_audit(monitor)$status == "unresolved-parent-exit"
        ),
        monitor_tree_note = paste(
            "Known and registered lifetimes require positive exit evidence.",
            "Unresolved-parent-exit rows identify unclassified lifetimes that ended;",
            "their CPU and unobserved descendants cannot be reconstructed.",
            "Sampling and ps's inaccessible-row omissions do not prove every global tree was observed."
        ),
        monitor_timing = monitor$timing,
        monitor_wall = sum(vapply(
            monitor$timing,
            function(time) time[["elapsed"]],
            numeric(1L)
        )),
        monitor_cpu = sum(vapply(
            monitor$timing,
            function(time) sum(time[c("user.self", "sys.self")]),
            numeric(1L)
        )),
        observed_tree_cpu = sum(vapply(
            as.list(registry),
            function(x) x$cpu,
            numeric(1L)
        )),
        sampled_peak_rss = unname(peak[["rss"]]),
        sampled_peak_processes = unname(peak[["processes"]]),
        files = nrow(timings),
        passed = sum(results$passed),
        skipped = sum(results$skipped),
        warnings = sum(results$warning),
        shards = shards,
        not_cran = Sys.getenv("NOT_CRAN"),
        profile = profile
    )
    saveRDS(metrics, file.path(output, "metrics.rds"))
    dput(metrics, file = file.path(output, "metrics.txt"))
    remaining("metrics write")
    metrics
}
