# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

# The live snapshot is a durable read model of a run. Reading it through one
# verified connection must produce the same payload as reading every table
# through store$query(), and must not repeat the lifecycle check per table.
shift_live_snapshot_test__run <- function(env = parent.frame()) {
    store_path <- tempfile("shift-snapshot-store-")
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "EC-Earth3",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gr",
            frequency = "mon",
            table = "Amon"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-snapshot-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    run_id <- shift_job__run_register(plan)
    store <- shift_store(plan)
    withr::defer(store$close(), envir = env)
    shift_job__job_create(
        store,
        run_id,
        mode = "foreground",
        ui = shift_ui("none", heartbeat = 0)
    )
    for (i in 1:3) {
        shift_job__run_event(
            store,
            run_id,
            stage = "extract",
            status = "running",
            message = paste("event", i),
            snapshot = FALSE
        )
    }
    list(store = store, run_id = run_id)
}

# Rebuild the payload with the earlier per-table store$query() reads.
shift_live_snapshot_test__oracle <- function(store, run_id, event_limit) {
    by <- function(table, key, id) {
        store$query(sprintf(
            "SELECT * FROM %s WHERE %s IN (%s)",
            table,
            key,
            shift_stage__query_ids(id)
        ))
    }
    run <- by("shift_run", "run_id", run_id)
    events <- store$query(sprintf(
        paste(
            "SELECT * FROM (SELECT * FROM shift_run_event WHERE run_id IN (%s)",
            "ORDER BY created_at DESC LIMIT %d) ORDER BY created_at"
        ),
        shift_stage__query_ids(run_id),
        as.integer(event_limit)
    ))
    jobs <- by("shift_run_job", "run_id", run_id)
    steps <- by("shift_run_step", "run_id", run_id)
    list(
        run = run,
        cases = by("shift_run_case", "run_id", run_id),
        events = events,
        jobs = jobs[order(jobs[["attempt"]])],
        steps = steps[order(steps[["ordinal"]])]
    )
}

test_that("live snapshot payload matches per-table reads and checks the store once", {
    fixture <- shift_live_snapshot_test__run()
    store <- fixture$store
    run_id <- fixture$run_id

    for (limit in c(200L, 2L, 0L)) {
        expected <- shift_live_snapshot_test__oracle(store, run_id, limit)
        payload <- shift_job__live_snapshot_write(
            store,
            run_id,
            event_limit = limit
        )
        for (field in names(expected)) {
            expect_identical(payload[[field]], expected[[field]], info = field)
        }
        expect_identical(payload$run_id, run_id)
        expect_identical(payload$store_path, store$path)
    }
    # The event tail is bounded by event_limit and keeps the newest rows.
    all_events <- shift_live_snapshot_test__oracle(store, run_id, 1000L)$events
    expect_gte(nrow(all_events), 3L)
    tail_events <- shift_job__live_snapshot_write(
        store,
        run_id,
        event_limit = 2L
    )$events
    expect_identical(nrow(tail_events), 2L)
    expect_identical(
        tail_events$event_id,
        tail(all_events$event_id, 2L)
    )

    original <- ddb_is_valid
    checks <- 0L
    local_mocked_bindings(
        ddb_is_valid = function(conn) {
            checks <<- checks + 1L
            original(conn)
        },
        .package = "epwshiftr"
    )
    shift_job__live_snapshot_write(store, run_id)
    # One lifecycle check for the snapshot plus one inside each ddb_query call
    # made by DuckDB itself is outside this package's wrappers.
    expect_identical(checks, 1L)
})

test_that("live snapshot of an unknown run writes nothing and a closed store errors", {
    fixture <- shift_live_snapshot_test__run()
    store <- fixture$store
    expect_null(shift_job__live_snapshot_write(store, "no-such-run"))
    expect_false(file.exists(shift_job__live_path(store$path, "no-such-run")))

    closed <- EsgStore$new(tempfile("shift-snapshot-closed-"))
    closed$close()
    expect_error(
        shift_job__live_snapshot_write(closed, fixture$run_id),
        "closed"
    )
})

# Coalescing: bursts of changes inside one interval write once, while urgent
# changes always write, and a deferred write is delivered later.
shift_live_snapshot_test__record <- function(env = parent.frame()) {
    writes <- new.env(parent = emptyenv())
    writes$ui <- list()
    original <- shift_job__live_snapshot_write
    local_mocked_bindings(
        shift_job__live_snapshot_write = function(
            store,
            run_id,
            event_limit = 200L,
            ui_state = NULL
        ) {
            # Wrap the value: assigning NULL into a list element is a no-op, so
            # a bare NULL ui_state would leave the call unrecorded.
            writes$ui[[length(writes$ui) + 1L]] <- list(ui_state)
            invisible(NULL)
        },
        .package = "epwshiftr",
        .env = env
    )
    writes
}

test_that("non-urgent snapshots coalesce inside the interval and urgent ones do not", {
    store <- list(path = tempfile("coalesce-"))
    writes <- shift_live_snapshot_test__record()
    local_mocked_bindings(
        shift_job__snapshot_interval = function() 3600,
        .package = "epwshiftr"
    )
    key <- "run-coalesce"
    withr::defer(rm(
        list = ls(shift_job__snapshot_state, all.names = TRUE),
        envir = shift_job__snapshot_state
    ))

    expect_true(shift_job__live_snapshot(store, key))
    # Inside the interval a burst of ordinary changes is deferred.
    for (i in 1:5) {
        expect_false(shift_job__live_snapshot(store, key))
    }
    expect_length(writes$ui, 1L)
    # An urgent change writes immediately, even inside the interval.
    expect_true(shift_job__live_snapshot(store, key, urgent = TRUE))
    expect_length(writes$ui, 2L)
    expect_true(shift_job__live_snapshot(store, key, urgent = TRUE))
    expect_length(writes$ui, 3L)
    # Different runs and different stores never share an interval.
    expect_true(shift_job__live_snapshot(store, "other-run"))
    expect_true(shift_job__live_snapshot(list(path = "elsewhere"), key))
    expect_length(writes$ui, 5L)
})

test_that("deferred snapshots are delivered once the interval has passed", {
    store <- list(path = tempfile("flush-"))
    writes <- shift_live_snapshot_test__record()
    interval <- 3600
    local_mocked_bindings(
        shift_job__snapshot_interval = function() interval,
        .package = "epwshiftr"
    )
    withr::defer(rm(
        list = ls(shift_job__snapshot_state, all.names = TRUE),
        envir = shift_job__snapshot_state
    ))
    key <- "run-flush"

    # Nothing is pending before the first write, and an unknown run is a no-op.
    expect_false(shift_job__live_snapshot_flush(store, key))
    shift_job__live_snapshot(store, key)
    expect_false(shift_job__live_snapshot_flush(store, key))
    shift_job__live_snapshot(store, key)
    # Pending, but the interval has not passed yet.
    expect_false(shift_job__live_snapshot_flush(store, key))
    expect_length(writes$ui, 1L)
    # Once the interval passes, flush writes exactly once and clears the flag.
    interval <- 0
    expect_true(shift_job__live_snapshot_flush(store, key))
    expect_length(writes$ui, 2L)
    expect_false(shift_job__live_snapshot_flush(store, key))
    expect_length(writes$ui, 2L)
})

test_that("a deferred snapshot keeps the last reporter state and a failed write stays pending", {
    store <- list(path = tempfile("uistate-"))
    writes <- shift_live_snapshot_test__record()
    local_mocked_bindings(
        shift_job__snapshot_interval = function() 3600,
        .package = "epwshiftr"
    )
    withr::defer(rm(
        list = ls(shift_job__snapshot_state, all.names = TRUE),
        envir = shift_job__snapshot_state
    ))
    key <- "run-ui"

    shift_job__live_snapshot(store, key, ui_state = list(unit = "first"))
    # A later change that carries no reporter state keeps the last one.
    shift_job__live_snapshot(store, key, ui_state = list(unit = "second"))
    shift_job__live_snapshot(store, key, urgent = TRUE)
    expect_identical(writes$ui[[1L]][[1L]], list(unit = "first"))
    expect_identical(writes$ui[[2L]][[1L]], list(unit = "second"))

    # A write that fails is not recorded, so the next call tries again.
    attempts <- 0L
    local_mocked_bindings(
        shift_job__live_snapshot_write = function(store, run_id, ...) {
            attempts <<- attempts + 1L
            if (attempts == 1L) {
                stop("disk full")
            }
            invisible(NULL)
        },
        .package = "epwshiftr"
    )
    expect_error(
        shift_job__live_snapshot(store, "run-fails", urgent = TRUE),
        "disk full"
    )
    expect_true(shift_job__live_snapshot(store, "run-fails"))
    expect_identical(attempts, 2L)
})

test_that("resetting a run forgets its interval and its reporter state", {
    store <- list(path = tempfile("reset-"))
    writes <- shift_live_snapshot_test__record()
    local_mocked_bindings(
        shift_job__snapshot_interval = function() 3600,
        .package = "epwshiftr"
    )
    withr::defer(rm(
        list = ls(shift_job__snapshot_state, all.names = TRUE),
        envir = shift_job__snapshot_state
    ))
    key <- "run-reset"

    shift_job__live_snapshot(store, key, ui_state = list(unit = "old"))
    expect_false(shift_job__live_snapshot(store, key))
    shift_job__snapshot_reset(store, key)
    # The next write is immediate again and carries no stale reporter state.
    expect_true(shift_job__live_snapshot(store, key))
    expect_null(writes$ui[[2L]][[1L]])
    # Resetting an unknown run is harmless.
    expect_silent(shift_job__snapshot_reset(store, "never-written"))
})

test_that("terminal changes write a snapshot immediately and ordinary ones coalesce", {
    fixture <- shift_live_snapshot_test__run()
    store <- fixture$store
    run_id <- fixture$run_id
    withr::defer(rm(
        list = ls(shift_job__snapshot_state, all.names = TRUE),
        envir = shift_job__snapshot_state
    ))
    local_mocked_bindings(
        shift_job__snapshot_interval = function() 3600,
        .package = "epwshiftr"
    )
    path <- shift_job__live_path(store$path, run_id)
    live_status <- function() {
        shift_job__live_run_get(run_id, store$path)@meta$run$status
    }

    shift_job__run_event(store, run_id, "extract", "running", "go")
    first <- file.mtime(path)
    expect_false(is.na(first))
    # Ordinary events inside the interval leave the sidecar as it was.
    for (i in 1:3) {
        shift_job__run_event(store, run_id, "extract", "running", "again")
    }
    expect_identical(file.mtime(path), first)
    # A terminal run update is written at once and shows the new status.
    shift_job__run_finish(store, run_id, "failed", last_error = "boom")
    expect_identical(live_status(), "failed")
    # The terminal snapshot carries the events deferred before it.
    events <- shift_job__live_run_get(run_id, store$path)@meta$events
    expect_gte(sum(events$message == "again"), 3L)
})

# vim: fdm=marker :
