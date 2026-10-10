# Build only persisted lifecycle records; heartbeat tests do not need climate
# inputs or a running process to exercise the real DuckDB and JSON boundaries.
heartbeat_test__fixture <- function(env = parent.frame()) {
    path <- tempfile("shift-heartbeat-")
    store <- EsgStore$new(path)
    withr::defer(
        {
            store$close()
            unlink(path, recursive = TRUE)
        },
        envir = env
    )
    run_id <- shift_job__task_run_register(store, "heartbeat-test")
    job <- shift_job__job_create(store, run_id, ui = shift_ui("none"))
    list(store = store, run_id = run_id, job_id = job$job_id[[1L]])
}

# The optimized liveness write must return and publish the same state as the
# general updater, including sub-microsecond R timestamp precision.
test_that("heartbeat matches the general job updater and live snapshot", {
    skip_if_not_installed("duckdb")
    fixture <- heartbeat_test__fixture()
    instant <- as.POSIXct(1700000000.1234567, origin = "1970-01-01", tz = "UTC")
    local_mocked_bindings(
        store__now = function() instant,
        .package = "epwshiftr"
    )
    ui_state <- list(
        status = "running",
        current_details = list(variable = "tas")
    )
    expected <- shift_job__job_update(
        fixture$store,
        fixture$job_id,
        heartbeat_at = instant,
        .ui_state = ui_state
    )
    path <- shift_job__live_path(fixture$store$path, fixture$run_id)
    expected_snapshot <- readLines(path, warn = FALSE)
    actual <- withVisible(shift_job__job_touch(
        fixture$store,
        fixture$job_id,
        ui_state
    ))
    expect_false(actual$visible)
    expect_identical(actual$value, expected)
    expect_identical(readLines(path, warn = FALSE), expected_snapshot)
})

# Heartbeats must not change immutable attempt identity, terminal fields,
# cancellation ownership, or the event stream while updating the live receipt.
test_that("heartbeat preserves cancellation terminal states and other attempts", {
    skip_if_not_installed("duckdb")
    fixture <- heartbeat_test__fixture()
    store <- fixture$store
    other <- shift_job__job_create(store, fixture$run_id, ui = shift_ui("none"))
    other_before <- shift_inspect__rows(
        store,
        "shift_run_job",
        "job_id",
        other$job_id
    )
    events_before <- shift_inspect__rows(
        store,
        "shift_run_event",
        "run_id",
        fixture$run_id
    )
    shift_job__cancel_request_write(store$path, fixture$run_id, fixture$job_id)
    completed <- as.POSIXct("2020-01-01", tz = "UTC")
    for (status in c("stopping", "completed", "failed", "cancelled")) {
        shift_job__job_update(
            store,
            fixture$job_id,
            status = status,
            completed_at = completed,
            last_error = "preserve"
        )
        before <- shift_inspect__rows(
            store,
            "shift_run_job",
            "job_id",
            fixture$job_id
        )
        shift_job__job_touch(store, fixture$job_id)
        after <- shift_inspect__rows(
            store,
            "shift_run_job",
            "job_id",
            fixture$job_id
        )
        fields <- setdiff(names(before), c("heartbeat_at", "updated_at"))
        expect_identical(
            after[, fields, with = FALSE],
            before[, fields, with = FALSE]
        )
        expect_gte(after$heartbeat_at, before$heartbeat_at)
        expect_true(shift_job__cancel_request_exists(
            store$path,
            fixture$run_id,
            fixture$job_id
        ))
    }
    expect_identical(
        shift_inspect__rows(store, "shift_run_job", "job_id", other$job_id),
        other_before
    )
    expect_identical(
        shift_inspect__rows(store, "shift_run_event", "run_id", fixture$run_id),
        events_before
    )
    live <- shift_job__live_run_get(fixture$run_id, store$path)
    expect_identical(live@meta$jobs$job_id, c(fixture$job_id, other$job_id))
})

# Count statements at the DuckDB boundary so a future refactor cannot silently
# reintroduce the pre-update SELECT that motivated this fixed-field path.
test_that("heartbeat updates and returns its job in one statement", {
    skip_if_not_installed("duckdb")
    fixture <- heartbeat_test__fixture()
    queries <- character()
    original <- ddb_query
    local_mocked_bindings(
        ddb_query = function(conn, sql) {
            queries <<- c(queries, sql)
            original(conn, sql)
        },
        .package = "epwshiftr"
    )
    shift_job__job_touch(fixture$store, fixture$job_id)
    expect_identical(
        sum(grepl("^UPDATE shift_run_job .*RETURNING .* \\*$", queries)),
        1L
    )
    expect_false(any(grepl(
        "SELECT .* FROM .*shift_run_job.* WHERE .*job_id",
        queries
    )))
    expect_identical(
        sum(grepl("SELECT .* FROM .*shift_run_job.* WHERE .*run_id", queries)),
        1L,
        info = paste(queries, collapse = "\n")
    )
})

# Preserve errors at the original lifecycle boundaries, including unusual IDs
# delegated to the general updater and durable writes before a sidecar failure.
test_that("heartbeat preserves lookup closed-store and publication failures", {
    skip_if_not_installed("duckdb")
    fixture <- heartbeat_test__fixture()
    expect_error(
        shift_job__job_touch(fixture$store, "missing"),
        "Shift job.*missing.*was not found"
    )
    expect_error(
        shift_job__job_touch(fixture$store, character()),
        "was not found"
    )
    instant <- as.POSIXct("2030-01-01", tz = "UTC")
    local_mocked_bindings(
        store__now = function() instant,
        store_write_json_atomic = function(...) stop("sidecar unavailable"),
        .package = "epwshiftr"
    )
    expect_error(
        shift_job__job_touch(fixture$store, fixture$job_id),
        "sidecar unavailable"
    )
    row <- shift_inspect__rows(
        fixture$store,
        "shift_run_job",
        "job_id",
        fixture$job_id
    )
    expect_equal(row$heartbeat_at, instant)
    expect_equal(row$updated_at, instant)
    fixture$store$close()
    expect_error(
        shift_job__job_touch(fixture$store, fixture$job_id),
        "The store is closed"
    )
})

# vim: fdm=marker :
