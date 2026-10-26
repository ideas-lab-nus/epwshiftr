# Own a reusable bounded reader pool. Each slot has a separate mirai compute
# profile so a timed-out native reader can be replaced without cancelling peers.
# Stores and collection callbacks remain exclusively in the calling process.
source__pool <- function(
    workers = 4L,
    timeout = 180,
    output = FALSE
) {
    checkmate::assert_count(workers, positive = TRUE)
    checkmate::assert_number(timeout, lower = .1, finite = TRUE)
    checkmate::assert_flag(output)
    owner <- Sys.getpid()
    closed <- FALSE
    queue <- list()
    slots <- vector("list", workers)
    sequence <- 0L
    clock <- function() proc.time()[["elapsed"]]
    guard <- function() {
        stopifnot(!closed, Sys.getpid() == owner)
    }
    # Poll only unresolved bootstrap requests; a native lifecycle stall is also
    # bounded by the independently owned coordinator supervisor at deployment.
    await <- function(task) {
        end <- clock() + 30
        while (mirai::unresolved(task)) {
            if (clock() > end) {
                stop("Source worker bootstrap timed out.")
            }
            Sys.sleep(.01)
        }
        value <- mirai::collect_mirai(task)
        message <- mirai_error_message(value)
        if (nzchar(message)) {
            stop(message)
        }
        value
    }
    # Capture the worker's process identity before loading application code.
    start <- function(i) {
        profile <- paste0(
            "epwshiftr-pool-",
            owner,
            "-",
            basename(tempfile()),
            "-",
            i
        )
        # Native C-library messages bypass R's message handlers. Opt-in mirai
        # forwarding preserves those diagnostics in the owning process's logs.
        mirai__start_pool(
            1L,
            dispatcher = FALSE,
            .compute = profile,
            output = output
        )
        # Until its process identity reaches a slot, this frame owns the profile.
        # Constructor cleanup can only retire registered slots, so inspection
        # and bootstrap failures must release this exact profile here.
        registered <- FALSE
        on.exit(
            if (!registered) mirai::daemons(0L, .compute = profile),
            add = TRUE
        )
        pid <- await(mirai::mirai(Sys.getpid(), .compute = profile))
        handle <- ps::ps_handle(pid)
        slots[[i]] <<- list(
            profile = profile,
            handle = handle,
            command = ps::ps_cmdline(handle),
            parent = ps::ps_ppid(handle),
            active = NULL
        )
        registered <- TRUE
        libraries <- shift_execution__library_paths()
        settings <- shift_execution__options()
        # Immediate transfer warnings use the existing serialized option list,
        # avoiding an unevaluated worker-global symbol in package checks.
        if (output) {
            settings$warn <- 1L
        }
        ready <- await(mirai::mirai(
            {
                .libPaths(libraries)
                loadNamespace("epwshiftr")
                options(settings)
                options(
                    epwshiftr.mirai_workers = 1L,
                    epwshiftr.progress = FALSE
                )
                data.table::setDTthreads(1L)
                TRUE
            },
            libraries = libraries,
            settings = settings,
            .compute = profile
        ))
        stopifnot(isTRUE(ready))
    }
    # A mirai timeout resolves a result but does not prove a native read stopped.
    # Terminate only this captured process tree and verify exit before key reuse.
    retire <- function(i) {
        slot <- slots[[i]]
        h <- slot$handle
        if (ps::ps_is_running(h)) {
            stopifnot(
                identical(ps::ps_cmdline(h), slot$command),
                identical(ps::ps_ppid(h), slot$parent)
            )
            children <- ps::ps_children(h, recursive = TRUE)
            for (child in rev(children)) {
                if (ps::ps_is_running(child)) ps::ps_kill(child)
            }
            ps::ps_kill(h)
            deadline <- clock() + 5
            while (
                any(vapply(
                    c(children, list(h)),
                    ps::ps_is_running,
                    logical(1L)
                ))
            ) {
                if (clock() > deadline) {
                    stop("Owned source worker did not exit.")
                }
                Sys.sleep(.01)
            }
        }
        mirai::daemons(0L, .compute = slot$profile)
        slots[i] <<- list(NULL)
    }
    pool <- new.env(parent = emptyenv())
    class(pool) <- "epwshiftr_source_pool"
    # A ticket owns only its callbacks and small result metadata. Weather payloads
    # are collected and persisted promptly rather than retained for the batch.
    pool$submit <- function(
        jobs,
        read,
        collect,
        finish = function() NULL,
        on_error = function(job, error) stop(error)
    ) {
        guard()
        checkmate::assert_function(read)
        checkmate::assert_function(collect)
        ticket <- new.env(parent = emptyenv())
        ticket$status <- "pending"
        ticket$remaining <- length(jobs)
        ticket$error <- NULL
        ticket$value <- NULL
        complete <- function() {
            result <- tryCatch(finish(), error = identity)
            if (inherits(result, "error")) {
                ticket$error <- result
            } else {
                ticket$value <- result
            }
            ticket$status <- if (is.null(ticket$error)) "done" else "failed"
        }
        if (!length(jobs)) {
            complete()
            return(ticket)
        }
        # Reserve known submission slots once rather than grow on every job.
        offset <- length(queue)
        length(queue) <<- offset + length(jobs)
        for (j in seq_along(jobs)) {
            job <- jobs[[j]]
            stopifnot(
                is.character(job$source_key),
                length(job$source_key) == 1L,
                nzchar(job$source_key)
            )
            sequence <<- sequence + 1L
            queue[[offset + j]] <<- list(
                id = sequence,
                job = job,
                read = read,
                collect = collect,
                finish = complete,
                on_error = on_error,
                ticket = ticket
            )
        }
        ticket
    }
    # Collect before refilling slots. Sources remain exclusive until the actual
    # worker finishes or its captured process has been confirmed dead.
    pool$poll <- function() {
        guard()
        for (i in seq_along(slots)) {
            active <- slots[[i]]$active
            if (is.null(active)) {
                next
            }
            now <- clock()
            timed <- now - active$started >= timeout
            # Subset workers announce only atomically saved native blocks.
            # Their monotonic counter bounds stalls without imposing a total
            # file duration on a healthy, continuously progressing reader.
            subset_progress <- active$entry$job$subset_progress
            if (!is.null(subset_progress)) {
                progress <- tryCatch(
                    if (file.exists(subset_progress)) {
                        jsonlite::read_json(
                            subset_progress,
                            simplifyVector = TRUE
                        )
                    } else {
                        NULL
                    },
                    error = function(e) NULL
                )
                count <- if (is.null(progress$blocks)) 0 else progress$blocks
                previous <- if (is.null(active$subset_blocks)) {
                    0
                } else {
                    active$subset_blocks
                }
                if (
                    is.numeric(count) &&
                        length(count) == 1L &&
                        is.finite(count) &&
                        count > previous
                ) {
                    active$subset_blocks <- count
                    active$last_progress <- now
                    slots[[i]]$active <<- active
                }
                timed <- now - active$last_progress >= timeout
            }
            if (mirai::unresolved(active$task) && !timed) {
                next
            }
            entry <- active$entry
            if (timed) {
                retire(i)
                value <- structure(
                    list(
                        message = "Source read exceeded its execution deadline.",
                        call = NULL
                    ),
                    class = c("epwshiftr_source_timeout", "error", "condition")
                )
                start(i)
            } else {
                value <- mirai::collect_mirai(active$task)
                message <- mirai_error_message(value)
                if (nzchar(message)) {
                    value <- simpleError(message)
                }
                slots[[i]]$active <<- NULL
            }
            outcome <- tryCatch(
                {
                    if (inherits(value, "error")) {
                        entry$on_error(entry$job, value)
                    } else {
                        entry$collect(entry$job, value)
                    }
                    NULL
                },
                error = identity
            )
            if (inherits(outcome, "error")) {
                entry$ticket$error <- outcome
            }
            entry$ticket$remaining <- entry$ticket$remaining - 1L
            if (entry$ticket$remaining == 0L) entry$finish()
        }
        keys <- vapply(
            slots,
            function(slot) {
                if (is.null(slot$active)) {
                    NA_character_
                } else {
                    slot$active$entry$job$source_key
                }
            },
            character(1L)
        )
        for (i in seq_along(slots)) {
            if (!is.null(slots[[i]]$active) || !length(queue)) {
                next
            }
            eligible <- which(
                !vapply(
                    queue,
                    function(e) e$job$source_key %in% keys,
                    logical(1L)
                )
            )
            if (!length(eligible)) {
                next
            }
            index <- eligible[1L]
            entry <- queue[[index]]
            queue[index] <<- NULL
            # Send only the reader and its small job descriptor. This explicit
            # binding also lets static checks resolve the remote expression
            # without exporting collection callbacks or suppressing globals.
            request <- list(read = entry$read, job = entry$job)
            task <- mirai::mirai(
                tryCatch(request$read(request$job), error = base::identity),
                request = request,
                .compute = slots[[i]]$profile
            )
            slots[[i]]$active <<- list(
                entry = entry,
                task = task,
                started = clock(),
                last_progress = clock()
            )
            keys[[i]] <- entry$job$source_key
        }
        invisible(pool$status())
    }
    # Expose genuine reader identities for node feedback without inventing active
    # work when a persistent daemon is idle.
    pool$status <- function() {
        guard()
        lapply(slots, function(s) {
            list(
                pid = ps::ps_pid(s$handle),
                active = !is.null(s$active),
                source_key = if (is.null(s$active)) {
                    NULL
                } else {
                    s$active$entry$job$source_key
                },
                elapsed_seconds = if (is.null(s$active)) {
                    NULL
                } else {
                    clock() - s$active$started
                }
            )
        })
    }
    # A caller may reuse this pool only after its queue and all slots are drained.
    pool$idle <- function() {
        guard()
        !length(queue) &&
            all(vapply(slots, function(s) is.null(s$active), logical(1L)))
    }
    pool$close <- function(force = FALSE) {
        if (closed) {
            return(invisible(NULL))
        }
        guard()
        busy <- length(queue) ||
            any(vapply(slots, function(s) !is.null(s$active), logical(1L)))
        if (busy && !force) {
            stop("Drain source jobs before closing the pool.")
        }
        for (i in seq_along(slots)) {
            if (!is.null(slots[[i]])) retire(i)
        }
        closed <<- TRUE
        invisible(NULL)
    }
    initialized <- FALSE
    on.exit(if (!initialized) pool$close(force = TRUE), add = TRUE)
    for (i in seq_along(slots)) {
        start(i)
    }
    initialized <- TRUE
    pool
}

# Own a lazy pool across consecutive internal subset calls in one coordinator.
# Failed calls discard only this session's pool; budgets and stores stay caller-owned.
opendap__session <- function(workers, timeout) {
    checkmate::assert_count(workers, positive = TRUE)
    checkmate::assert_number(timeout, lower = .1, finite = TRUE)
    owner <- Sys.getpid()
    pool <- NULL
    closed <- FALSE
    session <- new.env(parent = emptyenv())
    class(session) <- "epwshiftr_subset_session"
    guard <- function() stopifnot(!closed, Sys.getpid() == owner)
    # Reject configuration drift or reuse by a different coordinator.
    session$validate <- function(requested_workers, requested_timeout) {
        guard()
        stopifnot(
            identical(as.integer(requested_workers), as.integer(workers)),
            identical(as.double(requested_timeout), as.double(timeout))
        )
        invisible(NULL)
    }
    # Only a fully drained pool may cross a call boundary.
    session$acquire <- function(requested_workers, requested_timeout) {
        session$validate(requested_workers, requested_timeout)
        if (is.null(pool)) {
            pool <<- source__pool(workers, timeout = timeout)
        }
        stopifnot(pool$idle())
        pool
    }
    # Failed calls discard their own pool before any later source admission.
    session$reset <- function() {
        guard()
        if (!is.null(pool)) {
            pool$close(force = TRUE)
        }
        pool <<- NULL
        invisible(NULL)
    }
    # The campaign owner can release its session repeatedly during unwinding.
    session$close <- function() {
        if (!closed) {
            session$reset()
            closed <<- TRUE
        }
        invisible(NULL)
    }
    session
}

# vim: fdm=marker :
