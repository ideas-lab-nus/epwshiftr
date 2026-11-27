# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("R/shift-ui-state.R and ViewModel outputs remain strictly zero-ANSI", {
    state_file <- testthat::test_path("..", "..", "R", "shift-ui-state.R")
    if (file.exists(state_file)) {
        src <- paste(readLines(state_file, warn = FALSE), collapse = "\n")
        expect_false(grepl("\\bcli::", src))
        expect_false(grepl("\\\\033|\\\\x1b|\033", src))
    }

    now <- as.POSIXct("2026-07-20 00:00:00", tz = "UTC")
    spec <- shift_persist__spec_json(list(
        climate = list(
            model = "BCC-CSM2-MR",
            scenarios = c("ssp126", "ssp585")
        ),
        periods = list(`2060s` = 2055:2065),
        transform = transform__spec_value(monthly_transform("epwshiftr")),
        control = list(download = "auto")
    ))
    row <- data.table::data.table(
        run_id = "run-zero-ansi",
        status = "running",
        current_stage = "resolve",
        spec_json = spec,
        output_dir = "/exports",
        started_at = now,
        completed_at = as.POSIXct(NA, tz = "UTC")
    )
    events <- data.table::data.table(
        event_id = "ev-1",
        stage = "resolve",
        status = "completed",
        message = "ORNL selected",
        details_json = shift_persist__spec_json(list(
            stage = "resolve",
            phase = "unit",
            unit_type = "index_node",
            node = INDEX_NODES[["ORNL"]],
            future_files = 12L,
            reference_files = 6L,
            elapsed_seconds = 3.2,
            result = "selected"
        )),
        created_at = now + 3
    )
    cases <- data.table::data.table(
        experiment_id = "ssp585",
        period = "2060s",
        variant_label = "r1i1p1f1",
        status = "ready"
    )
    state <- shift_ui_state__ui_table_state(row, events, cases)
    strings <- unlist(
        rapply(state, as.character, how = "unlist"),
        use.names = FALSE
    )
    expect_false(any(grepl("\033", strings, fixed = TRUE)))
})

test_that("Shift display paths compact Windows temp roots lexically", {
    root <- "C:\\Users\\runneradmin\\AppData\\Local\\Temp\\Rtmp123"
    path <- paste0(
        "c:/users/runneradmin/AppData/Local/Temp/Rtmp123/",
        "shift-print-output"
    )
    expect_identical(
        shift_print__display_path(path, temp_root = root),
        "<tempdir>/shift-print-output"
    )
})

test_that("watch event deltas never tail away unseen milestones", {
    events <- data.table::data.table(
        event_id = paste0("e", seq_len(20L)),
        stage = "resolve",
        status = "running",
        message = paste("event", seq_len(20L)),
        details_json = "{}",
        created_at = as.POSIXct("2026-07-20 00:00:00", tz = "UTC") +
            seq_len(20L)
    )
    initial <- shift_ui_state__ui_event_delta(
        events,
        initial_limit = 5L,
        initial = TRUE
    )
    expect_equal(initial$rows$event_id, paste0("e", 16:20))
    expect_identical(initial$cursor, "e20")

    unseen <- shift_ui_state__ui_event_delta(
        events,
        last_event_id = "e5",
        initial_limit = 5L,
        initial = FALSE
    )
    expect_equal(unseen$rows$event_id, paste0("e", 6:20))
    expect_false(unseen$gap)

    truncated <- shift_ui_state__ui_event_delta(
        events[11:20],
        last_event_id = "e5",
        initial_limit = 5L,
        initial = FALSE
    )
    expect_equal(truncated$rows$event_id, paste0("e", 11:20))
    expect_true(truncated$gap)
})

test_that("batch snapshots include cancellation counts and live child activity", {
    batch <- ui_workflows__batch(tempfile("ui-snapshot-"))
    stamp <- as.POSIXct("2026-09-16 00:00:00", tz = "UTC")
    statuses <- c("cancelled", "stopping", "completed")
    ui_state <- list(
        status = "running",
        stage = "morph",
        unit_label = "ERA5 t2m",
        elapsed_seconds = 20,
        current_details = list(
            unit_type = "reanalysis_variable",
            current = 2L,
            total = 6L
        )
    )
    for (i in seq_along(statuses)) {
        plan <- batch@meta$children[[i]]
        batch@meta$children[[i]] <- shift_stage__new(
            ShiftRun,
            "run",
            store_path = plan@store_path,
            ids = list(run_id = paste0("run", i), morph_id = NA_character_),
            meta = list(
                run = data.table::data.table(
                    status = statuses[[i]],
                    task = "future_epw",
                    current_stage = "morph",
                    started_at = stamp,
                    updated_at = stamp + 10
                ),
                jobs = data.table::data.table(heartbeat_at = stamp + 20),
                cases = plan@meta$expected_cases,
                ui_state = ui_state
            )
        )
    }
    snapshot <- shift_batch_ui__snapshot(batch, refresh = FALSE)
    expect_equal(snapshot$batch$cancelled, 1L)
    counts <- snapshot$batch[,
        c("completed", "active", "failed", "partial", "waiting", "cancelled"),
        with = FALSE
    ]
    expect_equal(sum(unlist(counts)), snapshot$batch$children)
    expect_match(snapshot$activity[[2L]]$updated_at, "00:00:20")
    expect_identical(
        c(snapshot$activity[[1L]]$status, snapshot$activity[[2L]]$status),
        c("cancelled", "stopping")
    )
    snap_strings <- unlist(
        rapply(snapshot, as.character, how = "unlist"),
        use.names = FALSE
    )
    expect_false(any(grepl("\033", snap_strings, fixed = TRUE)))
})

test_that("live JSON timestamps retain clock time, fractions and UTC offsets", {
    stamp <- as.POSIXct("2026-09-18 01:23:45", tz = "UTC")
    original <- data.table::data.table(
        started_at = stamp,
        updated_at = stamp + 60,
        completed_at = as.POSIXct(NA, tz = "UTC")
    )
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    store_write_json_atomic(
        original,
        path,
        dataframe = "rows",
        na = "null",
        POSIXt = "ISO8601",
        digits = 15
    )
    restored <- shift_job__live_table(jsonlite::fromJSON(path))
    expect_equal(
        as.numeric(restored$started_at) - as.numeric(stamp),
        0,
        tolerance = 1e-6
    )
    expect_equal(
        as.numeric(difftime(
            restored$updated_at,
            restored$started_at,
            units = "secs"
        )),
        60
    )
    expect_s3_class(restored$completed_at, "POSIXct")
    expect_true(is.na(restored$completed_at))

    values <- c(
        "2026-09-18T01:23:45.125Z",
        "2026-09-18T09:23:45.125+08:00",
        "2026-09-17T20:23:45.125-0500",
        "2026-09-18 01:23:45.125",
        NA_character_
    )
    parsed <- shift_job__live_table(data.frame(
        heartbeat_at = values
    ))$heartbeat_at
    expect_equal(as.numeric(parsed[1:4]) - as.numeric(stamp), rep(0.125, 4L))
    expect_true(is.na(parsed[[5L]]))
    expect_equal(shift_job__live_time(stamp), stamp)
    expect_equal(
        shift_job__live_time("2026-09-18"),
        as.POSIXct("2026-09-18", tz = "UTC")
    )
})

test_that("batch event cursors retain interleaved events and detect gaps", {
    events <- data.table::data.table(
        event_id = c("a1", "a2", "b1", "a3", "a4"),
        run_id = c("run-a", "run-a", "run-b", "run-a", "run-a"),
        child_key = c("a", "a", "b", "a", "a"),
        method = "qdm",
        model = "Model-A",
        stage = "morph",
        status = c("running", "failed", "running", "running", "completed"),
        message = c(
            "A started",
            "Late failure A2",
            "B started",
            "Final event A3",
            "Final event A4"
        ),
        created_at = as.POSIXct("2026-09-18 01:00:00", tz = "UTC") +
            seq_len(5L),
        details_json = "{}"
    )
    first <- shift_batch_ui__event_delta(
        events[c(1L, 3L)],
        initial_limit = 1L,
        initial = TRUE
    )
    expect_identical(first$rows$event_id, "b1")
    second <- shift_batch_ui__event_delta(events[1:3], first$cursor)
    expect_identical(second$rows$event_id, "a2")
    expect_false(second$gap)
    expect_equal(
        nrow(
            shift_batch_ui__event_delta(
                events[c(3L, 2L, 1L)],
                second$cursor
            )$rows
        ),
        0L
    )

    new_child <- data.table::copy(events[1L])
    data.table::set(
        new_child,
        j = c("event_id", "run_id", "child_key"),
        value = list("c1", "run-c", "c")
    )
    expanded <- shift_batch_ui__event_delta(
        rbind(events[1:3], new_child),
        second$cursor
    )
    expect_identical(expanded$rows$event_id, "c1")
    expect_false(expanded$gap)

    truncated <- shift_batch_ui__event_delta(events[4:5], expanded$cursor)
    expect_identical(truncated$rows$event_id, c("a3", "a4"))
    expect_true(truncated$gap)
    restored <- shift_batch_ui__event_delta(events, truncated$cursor)
    expect_equal(nrow(restored$rows), 0L)
    expect_false(restored$gap)
    expect_identical(
        shift_batch_ui__event_delta(events[0], restored$cursor)$cursor,
        restored$cursor
    )
})

# vim: fdm=marker :
