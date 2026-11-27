#' Create a reusable pool of source-reading workers
#'
#' Pass this pool to `EsgStore$extract(pool = pool)`. Automatic HTTP fallback
#' stays within its source's reader slot and retains interrupted transfers.
#' Poll it from the store-owning process until each returned ticket has status
#' `"done"` or `"failed"`. Only that process writes extraction manifests and
#' Parquet files. A pool serializes jobs for identical physical source content,
#' independently of the extraction cache policy.
#'
#' @param workers Positive integer number of persistent readers.
#' @param timeout Positive execution deadline in seconds for a dispatched source
#'   job. A timed-out reader is confirmed stopped before its source is reused.
#' @param output Whether to forward reader standard output and standard error to
#'   the owning R process. Defaults to `FALSE`. Enable for native-library
#'   diagnostics that R message handlers cannot capture; this does not increase
#'   the worker count or change source exclusion and execution deadlines.
#' @param download_dir Optional directory for recoverable HTTP fallback files.
#'   Defaults to each store's download directory. Use a persistent node-local
#'   directory to share verified files and partial transfers across stores.
#' @param download_timeout Optional total HTTP transfer limit in seconds, at
#'   least `timeout`. When set, a transfer may exceed the ordinary source
#'   deadline while its retained file grows. No new bytes for `timeout` seconds
#'   still retires that reader. OPeNDAP reads keep the ordinary deadline.
#' @return An opaque pool with `poll()`, `status()` and `close()` methods.
#'   Drain pending tickets before calling `close()`.
#' @export
source_pool <- function(
    workers = 4L,
    timeout = 180,
    output = FALSE,
    download_dir = NULL,
    download_timeout = NULL
) {
    source__pool(workers, timeout, output, download_dir, download_timeout)
}

# Own a reusable bounded reader pool. Each slot has a separate mirai compute
# profile so a timed-out native reader can be replaced without cancelling peers.
# Stores and collection callbacks remain exclusively in the calling process.
source__pool <- function(
    workers = 4L,
    timeout = 180,
    output = FALSE,
    download_dir = NULL,
    download_timeout = NULL
) {
    checkmate::assert_count(workers, positive = TRUE)
    checkmate::assert_number(timeout, lower = .1, finite = TRUE)
    checkmate::assert_flag(output)
    checkmate::assert_string(download_dir, null.ok = TRUE)
    checkmate::assert_number(
        download_timeout,
        null.ok = TRUE,
        lower = timeout,
        finite = TRUE
    )
    if (!is.null(download_dir)) {
        # Readers may have a different working directory from the caller.
        download_dir <- store_abs_path(
            path.expand(download_dir),
            root = getwd()
        )
    }
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
    # Disconnect direct readers so they exit without deserializing the
    # shutdown message, which can raise R_UnboundValue in the daemon.
    close_idle <- function(profile) {
        mirai::daemons(0L, .compute = profile)
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
            if (!registered) close_idle(profile),
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
        if (is.null(slot$active)) {
            # Idle readers can exit through their transport. Busy or timed-out
            # readers still require owned kill before the next source is admitted.
            close_idle(slot$profile)
            deadline <- clock() + 5
            while (ps::ps_is_running(h) && clock() < deadline) {
                Sys.sleep(.01)
            }
        }
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
        # An active reader cannot publish after confirmed retirement. Its
        # request-local descriptor must not be reused by a replacement reader.
        progress_path <- attr(
            slot$active$entry$job$http,
            "source_progress",
            exact = TRUE
        )
        if (!is.null(progress_path)) {
            unlink(progress_path)
        }
        mirai::daemons(0L, .compute = slot$profile)
        slots[i] <<- list(NULL)
    }
    pool <- new.env(parent = emptyenv())
    # Store adapters may choose a durable cache without opening a manifest in
    # the coordinator's dispatch path. The actual deadline remains pool-owned.
    pool$download_dir <- download_dir
    pool$download_timeout <- max(
        1L,
        as.integer(ceiling(
            if (is.null(download_timeout)) timeout else download_timeout
        ))
    )
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
            http <- active$entry$job$http
            if (!is.null(download_timeout) && !is.null(http)) {
                # Sample small generation records and filesystem metadata only.
                # Each retained generation has its own byte high-water mark.
                if (now - active$progress_sample >= min(1, timeout / 4)) {
                    active$progress_sample <- now
                    transfer <- source__transfer_descriptor(active$entry$job)
                    if (!is.null(transfer) || file.exists(http$manifest)) {
                        if (is.null(active$transfer_started)) {
                            active$transfer_started <- now
                            active$last_progress <- now
                        }
                        sizes <- source__transfer_state(
                            active$entry$job,
                            transfer
                        )
                        previous <- active$transfer_bytes[names(sizes)]
                        grown <- sizes > 0 &
                            (is.na(previous) | sizes > previous)
                        if (any(grown)) {
                            active$last_progress <- now
                        }
                        previous[is.na(previous)] <- 0
                        active$transfer_bytes[names(sizes)] <- pmax(
                            sizes,
                            previous
                        )
                    }
                    slots[[i]]$active <<- active
                }
                if (!is.null(active$transfer_started)) {
                    timed <- now - active$last_progress >= timeout ||
                        now - active$transfer_started >= download_timeout
                }
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
                progress_path <- attr(http, "source_progress", exact = TRUE)
                if (!is.null(progress_path)) {
                    unlink(progress_path)
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
            # A unique handoff path binds the worker's selected transfer to this
            # dispatch; a stale descriptor cannot represent a later attempt.
            if (!is.null(download_timeout) && !is.null(entry$job$http)) {
                attr(entry$job$http, "source_progress") <- tempfile(
                    "epwshiftr-source-progress-",
                    fileext = ".json"
                )
            }
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
                progress_sample = clock() - 1,
                transfer_started = NULL,
                last_progress = clock(),
                transfer_bytes = if (is.null(download_timeout)) {
                    0
                } else {
                    source__transfer_state(entry$job)
                }
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

# Read the tiny atomic handoff emitted only after common recovery selects and
# locks an exact task. The coordinator never inspects a live DuckDB manifest.
source__transfer_descriptor <- function(job) {
    path <- attr(job$http, "source_progress", exact = TRUE)
    if (is.null(path) || !file.exists(path)) {
        return(NULL)
    }
    transfer <- tryCatch(
        jsonlite::read_json(path, simplifyVector = TRUE),
        error = function(e) NULL,
        warning = function(w) NULL
    )
    if (
        !is.list(transfer) ||
            !identical(transfer$schema, "epwshiftr-source-progress-v1")
    ) {
        return(NULL)
    }
    transfer
}

# Keep an independent high-water mark for each byte generation. A replacement
# from zero can grow below the prior partial length; retained history never
# appears under a canonical .part/.done pathname or supplies active progress.
source__transfer_state <- function(
    job,
    transfer = source__transfer_descriptor(job)
) {
    http <- job$http
    if (is.null(http)) {
        return(stats::setNames(numeric(), character()))
    }
    if (is.null(transfer)) {
        paths <- list.files(
            http$temp,
            pattern = "[.](part|done)$",
            full.names = TRUE,
            all.files = TRUE
        )
        piece_dirs <- list.files(
            http$temp,
            pattern = "[.]pieces$",
            full.names = TRUE,
            all.files = TRUE
        )
        target <- if (is.null(job$file$title)) {
            character()
        } else {
            file.path(http$dest, job$file$title[[1L]])
        }
    } else {
        # Saved Downloader temp/target locations are authoritative, including
        # adopted legacy layouts. Other tasks in that directory stay excluded.
        paths <- file.path(
            transfer$temp,
            paste0(transfer$task_id, c(".part", ".done"))
        )
        piece_dirs <- file.path(
            transfer$temp,
            paste0(transfer$task_id, ".pieces")
        )
        target <- transfer$target_path
    }
    # Only canonical task .pieces directories participate. Do not recurse into
    # retained histories or infer progress from a directory's modification time.
    paths <- c(
        paths,
        list.files(
            piece_dirs,
            pattern = "[.](part|done)$",
            full.names = TRUE,
            all.files = TRUE
        ),
        target
    )
    # Downloader atomically publishes this small sidecar before body writes.
    # A missing legacy sidecar still uses a stable pathname identity so merely
    # rewriting its existing bytes cannot extend the reader's stall deadline.
    records <- sub("[.](part|done)$", ".transfer.json", paths)
    if (!is.null(transfer)) {
        # Final publication carries the task's same generation. Renaming or
        # copying its complete file must not announce a second byte history.
        records[paths == transfer$target_path] <- file.path(
            transfer$temp,
            paste0(transfer$task_id, ".transfer.json")
        )
    }

    # Piece publication renames 000001.done.part to 000001.done while its
    # generation stays in 000001.done.transfer.json, preventing a false jump.
    piece_done <- grepl("[.]pieces$", basename(dirname(paths))) &
        grepl("[.]done$", paths)
    records[piece_done] <- paste0(paths[piece_done], ".transfer.json")

    identities <- vapply(
        records,
        function(path) {
            state <- if (
                grepl("[.]transfer[.]json$", path) && file.exists(path)
            ) {
                tryCatch(
                    jsonlite::read_json(path, simplifyVector = TRUE),
                    error = function(e) NULL,
                    warning = function(w) NULL
                )
            } else {
                NULL
            }
            generation <- if (is.list(state)) state$generation else NULL
            if (
                is.character(generation) &&
                    length(generation) == 1L &&
                    !is.na(generation) &&
                    nzchar(generation)
            ) {
                paste(path, generation, sep = "::")
            } else {
                path
            }
        },
        character(1L)
    )
    # Read generation before size: replacement moves old bytes before publishing
    # a new sidecar, so an old large stat cannot become the new generation's
    # high-water mark. A concurrent rename may simply defer one sample.
    info <- file.info(paths, extra_cols = FALSE)
    valid <- is.finite(info$size) & !is.na(info$isdir) & !info$isdir
    sizes <- info$size[valid]
    identities <- identities[valid]
    if (!length(sizes)) {
        return(stats::setNames(numeric(), character()))
    }
    # .part -> .done renames retain the same identity and cannot count twice.
    vapply(split(sizes, identities), max, numeric(1L))
}

# Count the largest retained generation without reading a weather payload.
source__transfer_size <- function(job) {
    max(c(0, source__transfer_state(job)))
}

# vim: fdm=marker :
