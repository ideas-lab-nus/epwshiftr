test_that("shared status views are four rows and remain within 80 columns", {
    state <- list(
        status = "running",
        stage = "resolve",
        stage_current = 1L,
        stage_total = 6L,
        stage_message = "Resolving complete CMIP6 workflow inputs.",
        unit_label = paste(rep("DKRZ future catalog waiting", 8L), collapse = " "),
        unit_current = 1L,
        unit_total = 6L,
        cases_ready = 0L,
        cases_total = 2L,
        outputs_completed = 0L,
        last_event = paste(rep("Catalog retry", 12L), collapse = " "),
        elapsed_seconds = 12
    )
    lines <- unname(shift__ui_status_lines(state, width = 80L))
    expect_length(lines, 4L)
    expect_true(all(nchar(lines, type = "width") <= 80L))
    expect_match(lines[[1L]], "Stage.*Resolve.*RUNNING")
    expect_match(lines[[2L]], "Current.*1/6")
    expect_match(lines[[3L]], "ready 0/2.*outputs 0/2")
    expect_match(lines[[4L]], "Last")
})

test_that("auto mode follows terminal capability for R and Rscript callers", {
    testthat::local_mocked_bindings(
        is_dynamic_tty = function(...) FALSE,
        .package = "cli"
    )
    expect_identical(shift__ui_mode(shift_ui("auto")), "log")

    testthat::local_mocked_bindings(
        is_dynamic_tty = function(...) TRUE,
        .package = "cli"
    )
    expect_identical(shift__ui_mode(shift_ui("auto")), "dynamic")
})

test_that("node and case tables use task-level rows and responsive columns", {
    nodes <- data.table::data.table(
        node = c("DKRZ", "CEDA", "ORNL"),
        future = c(38L, NA_integer_, 48L),
        reference = c(12L, NA_integer_, 24L),
        result = c(
            "rejected: missing hurs",
            "HTTP 503 while collecting the reference catalog",
            "selected r1i1p1f1 / gn"
        )
    )
    node_lines <- unname(shift__ui_node_table(nodes, width = 80L))
    expect_true(all(nchar(node_lines, type = "width") <= 80L))
    expect_match(paste(node_lines, collapse = "\n"), "Resolver attempts")
    expect_match(paste(node_lines, collapse = "\n"), "ORNL.*48.*24.*selected")

    cases <- data.table::data.table(
        experiment_id = c("ssp126", "ssp585"),
        period = "2060s",
        variant_label = "r1i1p1f1",
        status = c("ready", "missing"),
        missing_reason = c(NA_character_, "future/hurs, future/tas")
    )
    normal <- unname(shift__ui_case_table(cases, width = 60L,
        detail = "normal"))
    detailed <- unname(shift__ui_case_table(cases, width = 80L,
        detail = "detail"))
    expect_true(all(nchar(normal, type = "width") <= 60L))
    expect_false(any(grepl("Member", normal, fixed = TRUE)))
    expect_false(any(grepl("future/hurs", normal, fixed = TRUE)))
    expect_true(any(grepl("future/hurs", detailed, fixed = TRUE)))
})

test_that("persisted watch tables rebuild the shared state and resolver result", {
    now <- as.POSIXct("2026-07-20 00:00:00", tz = "UTC")
    row <- data.table::data.table(
        run_id = "run-test",
        status = "running",
        current_stage = "resolve",
        started_at = now,
        completed_at = as.POSIXct(NA, tz = "UTC")
    )
    events <- data.table::data.table(
        event_id = c("stage", "node-start", "node-done"),
        stage = "resolve",
        status = c("running", "running", "completed"),
        message = c(
            "Resolving complete CMIP6 workflow inputs.",
            "ORNL · checking future + reference catalogs",
            "ORNL · selected r1i1p1f1 / gn"
        ),
        details_json = vapply(list(
            list(stage = "resolve", phase = "stage", current = 1L, total = 6L),
            list(stage = "resolve", phase = "unit", unit_type = "index_node",
                unit_label = "ORNL · checking future + reference catalogs",
                current = 3L, total = 6L, node = INDEX_NODES[["ORNL"]]),
            list(stage = "resolve", phase = "unit", unit_type = "index_node",
                unit_label = "ORNL · selected r1i1p1f1 / gn",
                current = 3L, total = 6L, node = INDEX_NODES[["ORNL"]],
                future_files = 48L, reference_files = 24L,
                outcome = "completed", result = "selected r1i1p1f1 / gn")
        ), shift__spec_json, character(1L)),
        created_at = now + 0:2
    )
    cases <- data.table::data.table(
        experiment_id = c("ssp126", "ssp585"),
        period = "2060s", variant_label = "r1i1p1f1",
        status = c("pending", "pending")
    )
    view <- shift__ui_table_view(row, cases, events, width = 80L)
    expect_match(view$lines[[1L]], "[1/6] Resolve", fixed = TRUE)
    expect_match(view$lines[[2L]], "[3/6] ORNL", fixed = TRUE)
    expect_match(paste(view$nodes, collapse = "\n"), "ORNL.*48.*24.*selected")
    expect_match(paste(view$cases, collapse = "\n"), "ssp126")
})

test_that("terminal watch state freezes elapsed time at durable activity", {
    started <- as.POSIXct("2026-07-20 00:00:00", tz = "UTC")
    stopped <- started + 90
    row <- data.table::data.table(
        run_id = "run-failed",
        status = "failed",
        current_stage = "resolve",
        started_at = started,
        updated_at = stopped,
        completed_at = as.POSIXct(NA, tz = "UTC")
    )
    state <- shift__ui_table_state(row, data.table::data.table(),
        data.table::data.table())
    expect_equal(state$elapsed_seconds, 90)
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
    initial <- shift__ui_event_delta(events, initial_limit = 5L,
        initial = TRUE)
    expect_equal(initial$rows$event_id, paste0("e", 16:20))
    expect_identical(initial$cursor, "e20")

    unseen <- shift__ui_event_delta(events, last_event_id = "e5",
        initial_limit = 5L, initial = FALSE)
    expect_equal(unseen$rows$event_id, paste0("e", 6:20))
    expect_false(unseen$gap)

    truncated <- shift__ui_event_delta(events[11:20],
        last_event_id = "e5", initial_limit = 5L, initial = FALSE)
    expect_equal(truncated$rows$event_id, paste0("e", 11:20))
    expect_true(truncated$gap)
})

test_that("error summaries keep one primary cause", {
    message <- paste(
        "No ESGF index node produced a complete input set.",
        "x DKRZ: missing hurs",
        "i More detail",
        sep = "\n"
    )
    expect_identical(
        shift__error_summary(message),
        "No ESGF index node produced a complete input set."
    )
})

test_that("normal, detail, and debug keep URLs at the intended boundary", {
    event <- data.table::data.table(
        stage = "resolve",
        message = "ORNL · future catalog · 12 files",
        details_json = shift__spec_json(list(
            node = INDEX_NODES[["ORNL"]], catalog_role = "future"
        )),
        created_at = as.POSIXct("2026-07-20 00:00:00", tz = "UTC")
    )
    normal <- shift__ui_persisted_event_line(event, detail = "normal")
    debug <- shift__ui_persisted_event_line(event, detail = "debug")
    expect_match(normal, "[ORNL][future]", fixed = TRUE)
    expect_false(grepl("https://", normal, fixed = TRUE))
    expect_true(grepl("https://", debug, fixed = TRUE))
})

test_that("download views report files, bytes, and variable counts", {
    downloader <- list(tasks = function(session_id = NULL) {
        data.frame(
            status = c("done", "downloading", "queued"),
            bytes_done = c(1024, 512, 0),
            size = c(1024, 2048, 4096)
        )
    })
    metrics <- shift__download_metrics(downloader, "session", variables = 8L)
    label <- shift__download_label("future", metrics)
    expect_equal(metrics$current, 1L)
    expect_equal(metrics$total, 3L)
    expect_equal(metrics$bytes_done, 1536)
    expect_match(label, "future download")
    expect_match(label, "1/3 files", fixed = TRUE)
    expect_match(label, "1.5 KiB/7.0 KiB", fixed = TRUE)
    expect_match(label, "8 variables", fixed = TRUE)
})

test_that("download task progress is bridged into the workflow heartbeat", {
    callbacks <- new.env(parent = emptyenv())
    seen <- character()
    downloader <- list(
        tasks = function(session_id = NULL) data.frame(
            status = c("done", "downloading"),
            bytes_done = c(1024, 512),
            size = c(1024, 2048)
        ),
        on = function(event, fun) {
            callbacks[[event]] <- fun
            event
        },
        off = function(token) TRUE
    )
    reporter <- list(
        ui = function() shift_ui("none"),
        unit_started = function(...) invisible(NULL),
        unit_updated = function(...) invisible(NULL),
        unit_completed = function(...) invisible(NULL),
        notice = function(...) invisible(NULL),
        heartbeat = function(message, ...) {
            seen <<- c(seen, message)
            invisible(TRUE)
        }
    )
    cleanup <- shift__download_reporter_bind(
        downloader, reporter, role = "future", variables = 8L)
    callbacks$task_progress(list(
        event = "task_progress", session_id = "session",
        filename = "tas.nc", target_path = NA_character_,
        data_node = "example.org"
    ), downloader)
    cleanup()

    expect_match(seen, "future download")
    expect_match(seen, "1/2 files", fixed = TRUE)
    expect_match(seen, "1.5 KiB/3.0 KiB", fixed = TRUE)
    expect_match(seen, "8 variables", fixed = TRUE)
})

test_that("reporter updates the current unit label and keeps heartbeat labels stable", {
    reporter <- shift__reporter(shift_ui("none"))
    reporter$stage_started("download", "Downloading files.")
    reporter$unit_started("future download · 0/3 files",
        current = 0L, total = 3L)
    reporter$unit_updated("future download · 1/3 files · tas.nc",
        current = 1L, total = 3L)

    state <- reporter$snapshot()
    expect_identical(state$unit_label,
        "future download · 1/3 files · tas.nc")
    expect_equal(state$unit_current, 1L)
})

test_that("normal failure output includes concrete missing case reasons", {
    reporter <- shift__reporter(shift_ui("log", detail = "normal"))
    reporter$cases_updated(data.table::data.table(
        experiment_id = "ssp585",
        period = "2060s",
        variant_label = "r1i1p1f1",
        status = "missing",
        missing_reason = "future/hurs, future/tas"
    ))
    output <- capture.output(reporter$run_failed(), type = "message")
    expect_true(any(grepl("future/hurs", output, fixed = TRUE)))
})

test_that("startup plan summaries are exactly three width-bounded lines", {
    skip_if_not_installed("duckdb")

    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "BCC-CSM2-MR", c("ssp126", "ssp585"),
            member = "r1i1p1f1", grid = "gn"
        ),
        periods = list(`2060s` = 2055:2065),
        method = belcher(),
        dir = tempfile("shift-ui-output-"),
        store = tempfile("shift-ui-store-"),
        dry_run = TRUE
    )
    lines <- unname(shift__ui_plan_summary(plan, "run-test", width = 80L))
    expect_length(lines, 3L)
    expect_true(all(nchar(lines, type = "width") <= 80L))
    expect_match(lines[[1L]], "Future EPW.*run-test.*STARTING")
    expect_match(lines[[2L]], "BCC-CSM2-MR.*ssp126.*2060s")
    expect_match(lines[[3L]], "belcher.*baseline EPW.*2 expected")
})

test_that("shift_watch() renders the shared status view instead of one long string", {
    skip_if_not_installed("duckdb")

    store <- tempfile("shift-watch-view-store-")
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "BCC-CSM2-MR", c("ssp126", "ssp585"),
            member = "r1i1p1f1", grid = "gn"
        ),
        periods = list(`2060s` = 2055:2065),
        method = belcher(),
        dir = tempfile("shift-watch-view-output-"),
        store = store,
        dry_run = TRUE
    )
    withr::local_options(list(
        epwshiftr.shift.launcher = function(...) invisible(0L)
    ))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))
    on.exit(shift_cancel(run), add = TRUE)

    output <- capture.output(
        watched <- shift_watch(run, follow = FALSE, ui = shift_ui("log")),
        type = "message"
    )
    expect_s7_class(watched, ShiftRun)
    expect_true(any(grepl("^Stage", output)))
    expect_true(any(grepl("^Current", output)))
    expect_true(any(grepl("^Cases", output)))
    expect_true(any(grepl("^Last", output)))
    expect_false(any(grepl("queued | planned | cases", output, fixed = TRUE)))
})
