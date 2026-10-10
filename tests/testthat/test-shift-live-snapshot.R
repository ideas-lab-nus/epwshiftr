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
    skip_if_not_installed("duckdb")
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
    skip_if_not_installed("duckdb")
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

# vim: fdm=marker :
