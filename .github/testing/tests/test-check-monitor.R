# Run from the package root with Rscript .github/testing/tests/test-check-monitor.R. No workers
# are launched: OS failures are injected around a creation-time-bound handle.
source(".github/testing/check-parallel.R")

# Construct the condition shape emitted by the installed ps native bindings.
checks_test__ps_error <- function(class = "access_denied", errno = 0L) {
    structure(
        list(message = "Permission denied", errno = errno, pid = NA_integer_),
        class = c(class, "ps_error", "error", "condition")
    )
}

# Give each regression an independent registry and append-only diagnostic log.
checks_test__monitor <- function() {
    registry <- new.env(parent = emptyenv())
    handle <- ps::ps_handle()
    identity <- checks__track(handle, registry)
    monitor <- checks__monitor(tempfile(fileext = ".csv"), windows = FALSE)
    # Resource-only fixtures represent an already identified original root.
    # Descendant seeding is exercised with synthetic children in its own case.
    monitor$root_identities <- identity
    list(
        registry = registry,
        monitor = monitor,
        handle = handle,
        identity = identity
    )
}

# Build the same public ps table shape while controlling all process lifetimes.
checks_test__snapshot <- function(handles, parents = rep(0L, length(handles))) {
    data.frame(
        pid = vapply(handles, ps::ps_pid, integer(1L)),
        ppid = parents,
        created = as.POSIXct(
            vapply(
                handles,
                function(handle) as.numeric(ps::ps_create_time(handle)),
                numeric(1L)
            ),
            origin = "1970-01-01",
            tz = "UTC"
        ),
        ps_handle = I(handles)
    )
}

testthat::test_that("one resource denial preserves ownership and recovers", {
    state <- checks_test__monitor()
    calls <- 0L
    testthat::local_mocked_bindings(
        ps_cpu_times = function(p) {
            calls <<- calls + 1L
            if (calls == 1L) {
                stop(checks_test__ps_error())
            }
            c(user = 2, system = 1)
        },
        ps_memory_info = function(p) c(rss = 1024),
        .package = "ps"
    )
    first <- checks__observe(list(), state$registry, state$monitor)
    testthat::expect_equal(first, c(rss = 1024, processes = 1))
    testthat::expect_false(state$registry[[state$identity]]$exited)
    testthat::expect_equal(state$registry[[state$identity]]$cpu, 0)
    checks__observe(list(), state$registry, state$monitor)
    testthat::expect_equal(state$registry[[state$identity]]$cpu, 3)
    testthat::expect_identical(state$monitor$counts, c(cpu = 1L))
    log <- read.csv(state$monitor$log)
    testthat::expect_identical(log$operation, "cpu")
    testthat::expect_match(log$call, "ps::ps_cpu_times", fixed = TRUE)
    testthat::expect_identical(log$pid, Sys.getpid())
    testthat::expect_equal(log$create_time, as.numeric(ps::ps_create_time()))
    testthat::expect_match(log$error_class, "access_denied", fixed = TRUE)
    testthat::expect_identical(log$missing_count, 1L)
})

testthat::test_that("RSS and disappearing resource queries are missing samples", {
    state <- checks_test__monitor()
    testthat::local_mocked_bindings(
        ps_cpu_times = function(p) {
            stop(checks_test__ps_error("no_such_process"))
        },
        ps_memory_info = function(p) stop(checks_test__ps_error()),
        .package = "ps"
    )
    result <- checks__observe(list(), state$registry, state$monitor)
    testthat::expect_equal(result, c(rss = 0, processes = 1))
    testthat::expect_false(state$registry[[state$identity]]$exited)
    testthat::expect_true(is.na(state$registry[[state$identity]]$verified_at))
    testthat::expect_identical(state$monitor$counts, c(cpu = 1L, rss = 1L))
    testthat::expect_false(checks__exit_ready(state$registry, state$monitor))
})

testthat::test_that("liveness denial waits for explicit exit evidence", {
    state <- checks_test__monitor()
    calls <- 0L
    testthat::local_mocked_bindings(
        ps_is_running = function(p) {
            calls <<- calls + 1L
            if (calls == 1L) {
                stop(checks_test__ps_error())
            }
            FALSE
        },
        .package = "ps"
    )
    testthat::expect_false(checks__exit_ready(state$registry, state$monitor))
    testthat::expect_false(state$registry[[state$identity]]$exited)
    testthat::expect_true(checks__exit_ready(state$registry, state$monitor))
    testthat::expect_true(state$registry[[state$identity]]$exited)
    testthat::expect_false(is.na(state$registry[[state$identity]]$verified_at))
    testthat::expect_identical(calls, 2L)
    testthat::expect_true(checks__exit_ready(state$registry, state$monitor))
    testthat::expect_identical(calls, 2L)
})

testthat::test_that("persistent unknown liveness never satisfies acceptance", {
    state <- checks_test__monitor()
    testthat::local_mocked_bindings(
        ps_is_running = function(p) stop(checks_test__ps_error()),
        .package = "ps"
    )
    for (iteration in seq_len(3L)) {
        testthat::expect_false(checks__exit_ready(
            state$registry,
            state$monitor
        ))
    }
    testthat::expect_false(state$registry[[state$identity]]$exited)
    testthat::expect_identical(state$monitor$counts, c(liveness = 3L))
    testthat::expect_identical(read.csv(state$monitor$log)$missing_count, 1:3)
    testthat::expect_error(
        checks__exit_ready(
            state$registry,
            state$monitor,
            elapsed = 2,
            timeout = 1
        ),
        "remain unverified"
    )
})

testthat::test_that("enumeration denial recovers only after a successful scan", {
    state <- checks_test__monitor()
    calls <- 0L
    process <- list(is_alive = function() TRUE, get_pid = Sys.getpid)
    testthat::local_mocked_bindings(
        ps = function(after, columns) {
            calls <<- calls + 1L
            if (calls == 1L) {
                stop(checks_test__ps_error())
            }
            checks_test__snapshot(list(state$handle))
        },
        .package = "ps"
    )
    checks__observe(list(process), state$registry, state$monitor)
    testthat::expect_identical(
        unname(state$monitor$pending),
        "children_snapshot"
    )
    checks__observe(list(process), state$registry, state$monitor)
    testthat::expect_length(state$monitor$pending, 0L)
    testthat::expect_identical(state$monitor$counts, c(children_snapshot = 1L))
    testthat::expect_identical(
        read.csv(state$monitor$log)$operation,
        "children_snapshot"
    )
})

testthat::test_that("parent exit cannot close a failed descendant scan", {
    state <- checks_test__monitor()
    alive <- TRUE
    process <- list(is_alive = function() alive, get_pid = Sys.getpid)
    testthat::local_mocked_bindings(
        ps = function(after, columns) stop(checks_test__ps_error()),
        ps_is_running = function(p) alive,
        .package = "ps"
    )
    checks__observe(list(process), state$registry, state$monitor)
    alive <- FALSE
    checks__observe(list(process), state$registry, state$monitor)
    testthat::expect_true(state$registry[[state$identity]]$exited)
    testthat::expect_false(checks__exit_ready(state$registry, state$monitor))
    testthat::expect_length(state$monitor$pending, 1L)
    testthat::expect_error(
        checks__exit_ready(
            state$registry,
            state$monitor,
            elapsed = 2,
            timeout = 1
        ),
        "descendant scans remain unverified"
    )
})

testthat::test_that("ordinary exit races do not erase an earlier enumeration gap", {
    state <- checks_test__monitor()
    process <- list(is_alive = function() TRUE, get_pid = Sys.getpid)
    testthat::local_mocked_bindings(
        ps = function(after, columns) checks_test__snapshot(list()),
        ps_pids = function() integer(),
        .package = "ps"
    )
    checks__observe(list(process), state$registry, state$monitor)
    testthat::expect_length(state$monitor$pending, 0L)
    state$monitor$pending[[as.character(Sys.getpid())]] <- "children_snapshot"
    checks__observe(list(process), state$registry, state$monitor)
    testthat::expect_identical(
        unname(state$monitor$pending),
        "children_snapshot"
    )
    testthat::expect_length(state$monitor$counts, 0L)
})

testthat::test_that("parent handle denial is retained and retried", {
    state <- checks_test__monitor()
    calls <- 0L
    process <- list(is_alive = function() TRUE, get_pid = Sys.getpid)
    testthat::local_mocked_bindings(
        ps_handle = function(...) {
            calls <<- calls + 1L
            if (calls == 1L) {
                stop(checks_test__ps_error())
            }
            state$handle
        },
        ps = function(after, columns) checks_test__snapshot(list(state$handle)),
        .package = "ps"
    )
    checks__observe(list(process), state$registry, state$monitor)
    testthat::expect_identical(unname(state$monitor$pending), "parent_handle")
    checks__observe(list(process), state$registry, state$monitor)
    testthat::expect_length(state$monitor$pending, 0L)
    testthat::expect_true(is.na(read.csv(state$monitor$log)$create_time))
})

testthat::test_that("PID reuse does not merge lifetimes or inherit exit evidence", {
    state <- checks_test__monitor()
    other <- ps::ps_handle(Sys.getpid(), time = ps::ps_create_time() - 100)
    previous <- checks__track(other, state$registry)
    testthat::expect_false(identical(state$identity, previous))
    living <- checks__living(state$registry, state$monitor)
    testthat::expect_true(living[[state$identity]])
    testthat::expect_false(living[[previous]])
    testthat::expect_false(state$registry[[state$identity]]$exited)
    testthat::expect_true(state$registry[[previous]]$exited)
    testthat::expect_length(ls(state$registry), 2L)
})

testthat::test_that("only explicit PS permission conditions are recoverable", {
    state <- checks_test__monitor()
    for (errno in state$monitor$permission_errno) {
        testthat::expect_null(checks__ps_query(
            stop(checks_test__ps_error("os_error", errno)),
            state$monitor,
            "rss",
            Sys.getpid()
        ))
    }
    testthat::expect_error(
        checks__ps_query(stop("Permission denied"), state$monitor, "rss", 1L),
        "Permission denied"
    )
    testthat::expect_error(
        checks__ps_query(
            stop(checks_test__ps_error("os_error", 5L)),
            state$monitor,
            "rss",
            1L
        ),
        class = "os_error"
    )
    testthat::expect_error(
        checks__ps_query(
            stop(checks_test__ps_error("no_such_process")),
            state$monitor,
            "liveness",
            1L
        ),
        class = "no_such_process"
    )
})

testthat::test_that("one filtered snapshot serves four lifetime-bound trees", {
    registry <- new.env(parent = emptyenv())
    monitor <- checks__monitor(windows = FALSE)
    times <- c(10, 11, 12, 13, 20, 30, 5, 40)
    handles <- lapply(seq_along(times), function(index) {
        ps::ps_handle(
            as.integer(100 + index),
            time = as.POSIXct(times[[index]], origin = "1970-01-01", tz = "UTC")
        )
    })
    snapshot <- checks_test__snapshot(
        handles,
        c(0L, 0L, 0L, 0L, 101L, 105L, 101L, 999L)
    )
    calls <- 0L
    observed_after <- NULL
    testthat::local_mocked_bindings(
        ps = function(after, columns) {
            calls <<- calls + 1L
            observed_after <<- after
            testthat::expect_identical(
                columns,
                c("pid", "ppid", "created", "ps_handle")
            )
            snapshot
        },
        .package = "ps"
    )
    checks__discover(handles[1:4], registry, monitor)
    testthat::expect_identical(calls, 1L)
    testthat::expect_equal(as.numeric(observed_after), 10)
    testthat::expect_identical(
        unname(sort(vapply(
            as.list(registry),
            function(entry) entry$pid,
            integer(1L)
        ))),
        101:106
    )
    testthat::expect_length(monitor$pending, 0L)
})

testthat::test_that("omitted owned roots remain pending until discovery recovers", {
    state <- checks_test__monitor()
    snapshot <- checks_test__snapshot(list())
    testthat::local_mocked_bindings(
        ps = function(after, columns) snapshot,
        ps_pids = function() Sys.getpid(),
        .package = "ps"
    )
    checks__discover(list(state$handle), state$registry, state$monitor)
    testthat::expect_identical(
        unname(state$monitor$pending),
        "children_snapshot"
    )
    testthat::expect_identical(state$monitor$counts, c(children_identity = 1L))
    testthat::expect_match(
        read.csv(state$monitor$log)$message,
        "Owned root missing"
    )
    snapshot <- checks_test__snapshot(list(state$handle))
    checks__discover(list(state$handle), state$registry, state$monitor)
    testthat::expect_length(state$monitor$pending, 0L)
})

testthat::test_that("a reused root PID cannot close the previous discovery gap", {
    state <- checks_test__monitor()
    reused <- ps::ps_handle(Sys.getpid(), time = ps::ps_create_time() + 10)
    testthat::local_mocked_bindings(
        ps = function(after, columns) checks_test__snapshot(list(reused)),
        ps_pids = function() Sys.getpid(),
        .package = "ps"
    )
    checks__discover(list(state$handle), state$registry, state$monitor)
    testthat::expect_length(state$monitor$pending, 1L)
    testthat::expect_length(ls(state$registry), 1L)
    testthat::expect_false(state$registry[[state$identity]]$exited)
    checks__discover(list(reused), state$registry, state$monitor)
    testthat::expect_length(state$monitor$pending, 1L)
    testthat::expect_identical(names(state$monitor$pending), state$identity)
})

testthat::test_that("Windows false shortcuts cannot override a present matching lifetime", {
    state <- checks_test__monitor()
    state$monitor$windows <- TRUE
    testthat::local_mocked_bindings(
        ps_is_running = function(p) stop("Must not use Windows false shortcut"),
        ps_pids = function() Sys.getpid(),
        .package = "ps"
    )
    testthat::expect_true(checks__living(state$registry, state$monitor)[[
        state$identity
    ]])
    testthat::expect_false(state$registry[[state$identity]]$exited)
})

testthat::test_that("Windows permissions and disguised disappearance remain unknown", {
    state <- checks_test__monitor()
    state$monitor$windows <- TRUE
    pids <- Sys.getpid()
    kind <- "access_denied"
    testthat::local_mocked_bindings(
        ps_pids = function() pids,
        ps_handle = function(...) stop(checks_test__ps_error(kind)),
        .package = "ps"
    )
    testthat::expect_false(checks__exit_ready(state$registry, state$monitor))
    kind <- "no_such_process"
    testthat::expect_false(checks__exit_ready(state$registry, state$monitor))
    testthat::expect_false(state$registry[[state$identity]]$exited)
    testthat::expect_error(
        checks__exit_ready(
            state$registry,
            state$monitor,
            elapsed = 2,
            timeout = 1
        ),
        "remain unverified"
    )
    pids <- integer()
    testthat::expect_true(checks__exit_ready(state$registry, state$monitor))
    testthat::expect_true(state$registry[[state$identity]]$exited)
})

testthat::test_that("failed Windows PID enumeration cannot prove absence", {
    state <- checks_test__monitor()
    state$monitor$windows <- TRUE
    testthat::local_mocked_bindings(
        ps_pids = function() stop(checks_test__ps_error()),
        .package = "ps"
    )
    testthat::expect_false(checks__exit_ready(state$registry, state$monitor))
    testthat::expect_false(state$registry[[state$identity]]$exited)
    testthat::expect_identical(state$monitor$counts, c(liveness_pids = 1L))
})

testthat::test_that("Windows fresh identity proves only the old lifetime exited", {
    state <- checks_test__monitor()
    state$monitor$windows <- TRUE
    current <- ps::ps_handle(Sys.getpid(), time = ps::ps_create_time() + 10)
    new_identity <- checks__track(current, state$registry)
    testthat::local_mocked_bindings(
        ps_pids = function() Sys.getpid(),
        ps_handle = function(...) current,
        .package = "ps"
    )
    living <- checks__living(state$registry, state$monitor)
    testthat::expect_false(living[[state$identity]])
    testthat::expect_true(living[[new_identity]])
    testthat::expect_true(state$registry[[state$identity]]$exited)
    testthat::expect_false(state$registry[[new_identity]]$exited)
})

testthat::test_that("Windows parent disappearance errors cannot hide a present root", {
    state <- checks_test__monitor()
    state$monitor$windows <- TRUE
    process <- list(is_alive = function() TRUE, get_pid = Sys.getpid)
    testthat::local_mocked_bindings(
        ps_pids = function() Sys.getpid(),
        ps_handle = function(...) {
            stop(checks_test__ps_error("no_such_process"))
        },
        .package = "ps"
    )
    checks__observe(list(process), state$registry, state$monitor)
    testthat::expect_identical(unname(state$monitor$pending), "parent_handle")
    testthat::expect_false(state$registry[[state$identity]]$exited)
    testthat::expect_false(checks__exit_ready(state$registry, state$monitor))
})

testthat::test_that("monitor stage accounting excludes sleep and report work", {
    state <- checks_test__monitor()
    checks__observe(list(), state$registry, state$monitor)
    checks__exit_ready(state$registry, state$monitor)
    receipts <- new.env(parent = emptyenv())
    checks__receipts(character(), state$registry, receipts, state$monitor)
    testthat::expect_named(
        state$monitor$timing,
        c("observe", "exit_liveness", "receipts")
    )
    for (duration in state$monitor$timing) {
        testthat::expect_named(duration, c("elapsed", "user.self", "sys.self"))
        testthat::expect_true(all(duration >= 0))
    }
})

# Use synthetic lifetime handles to exercise cache and tree logic without OS
# enumeration, child processes, or changes to the user's process state.
checks_test__handle <- function(pid, created) {
    ps::ps_handle(
        as.integer(pid),
        time = as.POSIXct(created, origin = "1970-01-01", tz = "UTC")
    )
}

testthat::test_that("Windows caches PPID only for an unchanged process lifetime", {
    monitor <- checks__monitor(windows = TRUE)
    handles <- list(checks_test__handle(101, 10), checks_test__handle(102, 20))
    queried <- integer()
    snapshots <- 0L
    testthat::local_mocked_bindings(
        ps = function(after, columns) {
            snapshots <<- snapshots + 1L
            testthat::expect_identical(
                columns,
                c("pid", "created", "ps_handle")
            )
            checks_test__snapshot(handles)[, columns, drop = FALSE]
        },
        ps_ppid = function(p) {
            queried <<- c(queried, ps::ps_pid(p))
            if (ps::ps_pid(p) == 101L) 0L else 101L
        },
        .package = "ps"
    )
    first <- checks__windows_snapshot(
        as.POSIXct(10, origin = "1970-01-01"),
        monitor
    )
    second <- checks__windows_snapshot(
        as.POSIXct(10, origin = "1970-01-01"),
        monitor
    )
    testthat::expect_identical(first$ppid, second$ppid)
    testthat::expect_identical(queried, c(101L, 102L))
    # A recycled child PID and a newly observed unrelated process both require
    # a new topology query; a bare PID cache must not suppress either query.
    handles <- list(
        handles[[1L]],
        checks_test__handle(102, 30),
        checks_test__handle(999, 31)
    )
    third <- checks__windows_snapshot(
        as.POSIXct(10, origin = "1970-01-01"),
        monitor
    )
    testthat::expect_identical(queried, c(101L, 102L, 102L, 999L))
    testthat::expect_identical(third$pid, c(101L, 102L, 999L))
    testthat::expect_identical(snapshots, 3L)
    testthat::expect_identical(
        monitor$operations[c("ppid_queries", "ppid_hits")],
        c(ppid_queries = 4L, ppid_hits = 3L)
    )
    testthat::expect_length(ls(monitor$parents), 4L)
})

testthat::test_that("failed Windows topology reads are retried and cannot prove exit", {
    monitor <- checks__monitor(windows = TRUE)
    registry <- new.env(parent = emptyenv())
    handle <- checks_test__handle(101, 10)
    identity <- checks__track(handle, registry)
    calls <- 0L
    kind <- "access_denied"
    testthat::local_mocked_bindings(
        ps = function(after, columns) {
            checks_test__snapshot(list(handle))[, columns, drop = FALSE]
        },
        ps_ppid = function(p) {
            calls <<- calls + 1L
            if (calls < 3L) {
                stop(checks_test__ps_error(kind))
            }
            1L
        },
        ps_pids = function() 101L,
        ps_handle = function(...) stop(checks_test__ps_error()),
        .package = "ps"
    )
    since <- as.POSIXct(0, origin = "1970-01-01")
    for (error_class in c("access_denied", "no_such_process")) {
        kind <- error_class
        snapshot <- checks__windows_snapshot(since, monitor)
        testthat::expect_true(is.na(snapshot$ppid))
        testthat::expect_length(ls(monitor$parents), 0L)
        testthat::expect_false(checks__exit_ready(registry, monitor))
        testthat::expect_false(registry[[identity]]$exited)
    }
    snapshot <- checks__windows_snapshot(since, monitor)
    testthat::expect_identical(snapshot$ppid, 1L)
    testthat::expect_identical(monitor$parents[[identity]], 1L)
    # Even a populated topology cache cannot replace fresh liveness evidence.
    testthat::expect_error(
        checks__exit_ready(registry, monitor, elapsed = 2, timeout = 1),
        "remain unverified"
    )
    testthat::expect_false(registry[[identity]]$exited)
    testthat::expect_identical(calls, 3L)
})

testthat::test_that("owned children discover new descendants after their root exits", {
    monitor <- checks__monitor(windows = FALSE)
    monitor$since <- 10
    registry <- new.env(parent = emptyenv())
    root <- checks_test__handle(101, 10)
    child <- checks_test__handle(102, 20)
    grandchild <- checks_test__handle(103, 30)
    root_id <- checks__track(root, registry)
    child_id <- checks__track(child, registry)
    entry <- registry[[root_id]]
    entry$exited <- TRUE
    entry$verified_at <- Sys.time()
    registry[[root_id]] <- entry
    observed_after <- NULL
    testthat::local_mocked_bindings(
        ps = function(after, columns) {
            observed_after <<- after
            checks_test__snapshot(list(child, grandchild), c(101L, 102L))
        },
        ps_is_running = function(p) ps::ps_pid(p) != 101L,
        ps_cpu_times = function(p) c(user = 1, system = 0),
        ps_memory_info = function(p) c(rss = 512),
        .package = "ps"
    )
    process <- list(is_alive = function() FALSE, get_pid = function() 101L)
    result <- checks__observe(list(process), registry, monitor)
    testthat::expect_equal(result, c(rss = 1024, processes = 2))
    testthat::expect_length(ls(registry), 3L)
    testthat::expect_false(registry[[child_id]]$exited)
    testthat::expect_true(registry[[root_id]]$exited)
    testthat::expect_equal(as.numeric(observed_after), 10)
    testthat::expect_length(monitor$pending, 0L)
})

testthat::test_that("Unix discovery reads changed parent relationships each time", {
    monitor <- checks__monitor(windows = FALSE)
    registry <- new.env(parent = emptyenv())
    roots <- list(checks_test__handle(101, 10), checks_test__handle(102, 11))
    child <- checks_test__handle(103, 20)
    parent <- 101L
    calls <- 0L
    testthat::local_mocked_bindings(
        ps = function(after, columns) {
            calls <<- calls + 1L
            checks_test__snapshot(c(roots, list(child)), c(0L, 0L, parent))
        },
        .package = "ps"
    )
    checks__discover(roots, registry, monitor)
    parent <- 102L
    checks__discover(roots, registry, monitor)
    testthat::expect_identical(calls, 2L)
    testthat::expect_length(ls(monitor$parents), 0L)
    testthat::expect_length(ls(registry), 3L)
})

testthat::test_that("receipt polling is bounded but terminal scans force new identities", {
    directory <- withr::local_tempdir()
    dir.create(file.path(directory, "traces"))
    monitor <- checks__monitor(windows = FALSE)
    registry <- new.env(parent = emptyenv())
    receipts <- new.env(parent = emptyenv())
    # Publish complete files as the real loader does, without any worker.
    publish <- function(pid) {
        saveRDS(
            list(
                pid = as.integer(pid),
                create_time = as.POSIXct(pid, origin = "1970-01-01", tz = "UTC")
            ),
            file.path(directory, "traces", paste0("process-", pid, ".rds"))
        )
    }
    publish(101)
    testthat::expect_true(checks__poll_receipts(
        directory,
        registry,
        receipts,
        monitor,
        now = 0
    ))
    publish(102)
    testthat::expect_false(checks__poll_receipts(
        directory,
        registry,
        receipts,
        monitor,
        now = .25
    ))
    testthat::expect_length(ls(registry), 1L)
    testthat::expect_true(checks__poll_receipts(
        directory,
        registry,
        receipts,
        monitor,
        now = 2
    ))
    publish(103)
    testthat::expect_true(checks__poll_receipts(
        directory,
        registry,
        receipts,
        monitor,
        force = TRUE,
        now = 2.01
    ))
    testthat::expect_length(ls(registry), 3L)
    testthat::expect_true(checks__poll_receipts(
        directory,
        registry,
        receipts,
        monitor,
        force = TRUE,
        now = 2.02
    ))
    testthat::expect_identical(
        monitor$operations[c("receipt_scans", "receipt_reads")],
        c(receipt_scans = 4L, receipt_reads = 3L)
    )
    testthat::expect_true(all(vapply(
        as.list(registry),
        function(entry) !entry$exited,
        logical(1L)
    )))
})

testthat::test_that("sample scheduling uses start times and exposes observed overruns", {
    monitor <- checks__monitor(windows = FALSE)
    checks__sample_begin(monitor, now = 10)
    testthat::expect_equal(checks__poll_delay(monitor, .25, now = 11.9), .1)
    checks__sample_begin(monitor, now = 12)
    testthat::expect_equal(monitor$samples$max_gap, 2)
    testthat::expect_identical(monitor$samples$overruns, 0L)
    testthat::expect_equal(checks__poll_delay(monitor, .25, now = 14.3), 0)
    checks__sample_begin(monitor, now = 14.3)
    testthat::expect_equal(monitor$samples$max_gap, 2.3)
    testthat::expect_identical(monitor$samples$overruns, 1L)
    testthat::expect_identical(monitor$samples$count, 3L)
})

testthat::test_that("unclassified exits retain evidence and late ownership reopens their gap", {
    monitor <- checks__monitor(windows = TRUE)
    registry <- new.env(parent = emptyenv())
    unknown <- checks_test__handle(201, 20)
    snapshot <- checks_test__snapshot(list(unknown))
    pids <- 201L
    testthat::local_mocked_bindings(
        ps = function(after, columns) snapshot[, columns, drop = FALSE],
        ps_ppid = function(p) stop(checks_test__ps_error()),
        ps_pids = function() pids,
        ps_handle = function(...) unknown,
        .package = "ps"
    )
    checks__windows_snapshot(as.POSIXct(10, origin = "1970-01-01"), monitor)
    testthat::expect_length(ls(registry), 0L)
    testthat::expect_length(monitor$pending, 1L)
    testthat::expect_false(checks__exit_ready(registry, monitor))
    # Omission from a filtered snapshot alone is not positive exit evidence.
    snapshot <- checks_test__snapshot(list())
    checks__windows_snapshot(as.POSIXct(10, origin = "1970-01-01"), monitor)
    testthat::expect_false(checks__exit_ready(registry, monitor))
    pids <- integer()
    testthat::expect_true(checks__exit_ready(registry, monitor))
    audit <- checks__topology_audit(monitor)
    testthat::expect_identical(audit$status, "unresolved-parent-exit")
    testthat::expect_identical(audit$proof, "pid-absent")
    testthat::expect_match(audit$error_class, "access_denied", fixed = TRUE)
    testthat::expect_false(anyNA(audit$verified_at))
    # A late registration establishes this was owned. Its unresolved scan gap
    # is now strict even though this lifetime has positively exited.
    checks__track(unknown, registry)
    testthat::expect_false(checks__exit_ready(registry, monitor))
    testthat::expect_identical(
        checks__topology_audit(monitor)$status,
        "owned-pending"
    )
    testthat::expect_true(
        registry[[checks__identity(201L, ps::ps_create_time(unknown))]]$exited
    )
    testthat::expect_error(
        checks__exit_ready(registry, monitor, elapsed = 2, timeout = 1),
        "descendant scans remain unverified"
    )
})

testthat::test_that("an unknown topology permission gap needs positive lifetime evidence", {
    monitor <- checks__monitor(windows = TRUE)
    registry <- new.env(parent = emptyenv())
    unknown <- checks_test__handle(201, 20)
    reused <- checks_test__handle(201, 30)
    denied <- TRUE
    testthat::local_mocked_bindings(
        ps = function(after, columns) {
            checks_test__snapshot(list(unknown))[, columns, drop = FALSE]
        },
        ps_ppid = function(p) stop(checks_test__ps_error()),
        ps_pids = function() 201L,
        ps_handle = function(...) {
            if (denied) {
                stop(checks_test__ps_error())
            }
            reused
        },
        .package = "ps"
    )
    checks__windows_snapshot(as.POSIXct(10, origin = "1970-01-01"), monitor)
    testthat::expect_false(checks__exit_ready(registry, monitor))
    testthat::expect_error(
        checks__exit_ready(registry, monitor, elapsed = 2, timeout = 1),
        "remain unverified"
    )
    denied <- FALSE
    testthat::expect_true(checks__exit_ready(registry, monitor))
    audit <- checks__topology_audit(monitor)
    testthat::expect_identical(audit$status, "unresolved-parent-exit")
    testthat::expect_identical(audit$proof, "pid-reused")
    testthat::expect_length(ls(registry), 0L)
})

testthat::test_that("a cached child cannot attach to a younger reused parent PID", {
    monitor <- checks__monitor(windows = TRUE)
    monitor$since <- 5
    registry <- new.env(parent = emptyenv())
    root <- checks_test__handle(101, 30)
    older_child <- checks_test__handle(102, 20)
    child_id <- checks__identity(102L, ps::ps_create_time(older_child))
    monitor$parents[[child_id]] <- 101L
    testthat::local_mocked_bindings(
        ps = function(after, columns) {
            checks_test__snapshot(list(root, older_child))[,
                columns,
                drop = FALSE
            ]
        },
        ps_ppid = function(p) 0L,
        .package = "ps"
    )
    checks__discover(list(root), registry, monitor)
    testthat::expect_length(ls(registry), 1L)
    testthat::expect_null(registry[[child_id]])
    testthat::expect_length(monitor$pending, 0L)
    testthat::expect_identical(monitor$operations[["ppid_hits"]], 1L)
})

testthat::test_that("reused child PID creates no new scan gap and preserves prior gaps", {
    # The child's old lifetime has ended before descendant seeding. Reusing its
    # PID is positive exit evidence, not a newly failed discovery of that child.
    check <- function(windows, previous_gap) {
        monitor <- checks__monitor(windows = windows)
        registry <- new.env(parent = emptyenv())
        root <- checks_test__handle(101, 10)
        child <- checks_test__handle(102, 20)
        replacement <- checks_test__handle(102, 30)
        root_id <- checks__track(root, registry)
        child_id <- checks__track(child, registry)
        registry[[root_id]]$exited <- TRUE
        registry[[root_id]]$verified_at <- Sys.time()
        monitor$root_identities <- root_id
        monitor$since <- 10
        if (previous_gap) {
            monitor$pending[[child_id]] <- "children_snapshot"
        }
        testthat::local_mocked_bindings(
            ps = function(after, columns) {
                checks_test__snapshot(list(replacement), 999L)[,
                    columns,
                    drop = FALSE
                ]
            },
            ps_ppid = function(p) 999L,
            ps_pids = function() 102L,
            ps_handle = function(...) replacement,
            ps_is_running = function(p) FALSE,
            .package = "ps"
        )
        process <- list(is_alive = function() FALSE, get_pid = function() 101L)
        checks__observe(list(process), registry, monitor)
        testthat::expect_true(registry[[child_id]]$exited)
        testthat::expect_length(ls(registry), 2L)
        testthat::expect_identical(
            checks__exit_ready(registry, monitor),
            !previous_gap
        )
        testthat::expect_identical(
            names(monitor$pending),
            if (previous_gap) child_id else NULL
        )
        if (previous_gap) {
            testthat::expect_error(
                checks__exit_ready(registry, monitor, elapsed = 2, timeout = 1),
                "descendant scans remain unverified"
            )
        }
    }
    for (windows in c(FALSE, TRUE)) {
        for (previous_gap in c(FALSE, TRUE)) {
            check(windows, previous_gap)
        }
    }
})

# vim: fdm=marker :
