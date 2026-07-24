test_that("shared status views are stage-adaptive and remain within the terminal width", {
    state <- list(
        status = "running",
        stage = "resolve",
        stage_sequence = c("resolve", "extract_future", "extract_reference",
            "coverage", "morph", "write_epw"),
        completed_stages = character(),
        stage_current = 1L,
        stage_total = 6L,
        stage_message = "Resolving complete CMIP6 workflow inputs.",
        unit_label = paste(rep("DKRZ future catalog waiting", 8L), collapse = " "),
        unit_current = 1L,
        unit_total = 6L,
        plan_context = list(
            line = "BCC-CSM2-MR \u00b7 ssp126 + ssp585 \u00b7 2060s \u00b7 belcher",
            selection = "member auto \u00b7 grid auto"
        ),
        cases_ready = 0L,
        cases_total = 2L,
        outputs_completed = 0L,
        recent_events = c("Catalog retry", "Previous request completed"),
        recent_outcomes = c("fallback", "completed"),
        elapsed_seconds = 12
    )
    lines <- unname(shift__ui_status_lines(state, width = 80L,
        motion = "full", frame = 2L))
    expect_gte(length(lines), 11L)
    expect_true(all(cli::ansi_nchar(lines, type = "width") <= 79L))
    plain <- cli::ansi_strip(lines)
    expect_match(plain[[1L]], "Future EPW.*RUNNING")
    expect_true(any(grepl("Plan.*BCC-CSM2-MR.*belcher", plain)))
    expect_true(any(grepl("Workflow", plain, fixed = TRUE)))
    expect_true(any(grepl("Flow.*Resolve.*Future", plain)))
    expect_true(any(grepl("Now.*DKRZ", plain)))
    expect_true(any(grepl("Status.*node 1 of 6.*member auto", plain)))
    expect_false(any(grepl("%", plain, fixed = TRUE)))
    expect_true(any(grepl("Activity", plain, fixed = TRUE)))
    expect_true(any(grepl("Attempts", plain)))
    expect_true(all(cli::ansi_nchar(lines, type = "width") == 79L))
})

test_that("wide dashboards add quiet hierarchy while narrow views preserve content", {
    state <- list(
        run_id = "run-panel", status = "running", stage = "resolve",
        stage_sequence = c("resolve", "extract_future", "coverage", "morph",
            "write_epw"),
        unit_label = "CEDA · future catalog · waiting",
        unit_current = 2L, unit_total = 6L,
        current_details = list(current = 2L, total = 6L),
        plan_context = list(
            line = "BCC-CSM2-MR · ssp585 · 2060s · belcher",
            selection = "member auto · grid auto"
        ),
        elapsed_seconds = 5
    )
    wide <- shift__ui_status_lines(state, width = 60L)
    narrow <- shift__ui_status_lines(state, width = 59L)
    wide_plain <- cli::ansi_strip(wide)
    narrow_plain <- cli::ansi_strip(narrow)

    expect_gte(length(wide), 11L)
    expect_true(all(cli::ansi_nchar(wide, type = "width") == 59L))
    expect_match(wide_plain[[1L]], "^╭─ Future EPW")
    expect_true(any(grepl("^├─ Workflow", wide_plain)))
    expect_true(any(grepl("^├─ Activity", wide_plain)))
    expect_match(utils::tail(wide_plain, 1L), "^╰─")

    expect_gte(length(narrow), 8L)
    expect_false(any(grepl("^[╭├╰│]", narrow_plain)))
    expect_match(narrow_plain[[1L]], "Future EPW", fixed = TRUE)
})

test_that("title-like dashboard labels use semantic emphasis", {
    withr::local_options(cli.num_colors = 256L)

    expect_identical(shift__ui_label_role("Plan"), "accent")
    expect_identical(shift__ui_label_role("Flow"), "accent")
    expect_identical(shift__ui_label_role("Failure"), "danger")
    expect_identical(shift__ui_label_role("Status"), "accent")
    expect_identical(shift__ui_label_role("Summary"), "accent")
    expect_identical(shift__ui_label_role("Attempts"), "quiet")

    plan <- shift__ui_labeled_line("Plan", "BCC-CSM2-MR")
    failure <- shift__ui_labeled_line("Failure", "resolver exhausted")
    quiet <- shift__ui_labeled_line("Attempts", "6 tried")
    expect_true(all(cli::ansi_has_any(c(plan, failure, quiet))))
    expect_identical(cli::ansi_strip(plan), "Plan     BCC-CSM2-MR")
    expect_identical(cli::ansi_strip(failure),
        "Failure  resolver exhausted")
    expect_false(identical(plan, failure))
    expect_false(identical(plan, quiet))
})

test_that("frame renderer paints each dashboard update atomically", {
    writes <- character()
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    renderer <- ShiftFrameRenderer$new(
        output = output,
        backend = "frame",
        writer = function(text) writes <<- c(writes, text)
    )

    expect_true(renderer$draw(c("one", "two", "three")))
    expect_true(renderer$draw(c("ONE", "TWO")))

    expect_length(writes, 2L)
    expect_identical(writes[[1L]],
        "\rone\033[K\n\rtwo\033[K\n\rthree\033[K\r")
    expect_identical(writes[[2L]], paste0(
        "\033[2A\rONE\033[K\n\rTWO\033[K\r",
        "\n\r\033[K\033[1A\r"
    ))
})

test_that("frame renderer commits a terminal frame without erasing it", {
    writes <- character()
    shown <- 0L
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    testthat::local_mocked_bindings(
        ansi_hide_cursor = function(...) invisible(NULL),
        ansi_show_cursor = function(...) {
            shown <<- shown + 1L
            invisible(NULL)
        },
        .package = "cli"
    )
    renderer <- ShiftFrameRenderer$new(
        output = output,
        backend = "frame",
        writer = function(text) writes <<- c(writes, text)
    )

    renderer$draw(c("failed one", "failed two"))
    renderer$commit("failed")
    renderer$commit("failed")

    expect_length(writes, 2L)
    expect_match(writes[[1L]], "failed one", fixed = TRUE)
    expect_identical(writes[[2L]], "\n")
    expect_equal(shown, 1L)
    expect_false(renderer$active())
})

test_that("frame renderer suspends output and restores the last frame once", {
    writes <- character()
    hidden <- 0L
    shown <- 0L
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    testthat::local_mocked_bindings(
        ansi_hide_cursor = function(...) hidden <<- hidden + 1L,
        ansi_show_cursor = function(...) shown <<- shown + 1L,
        .package = "cli"
    )
    renderer <- ShiftFrameRenderer$new(
        output = output,
        backend = "frame",
        writer = function(text) writes <<- c(writes, text)
    )
    renderer$draw(c("one", "two"))
    renderer$suspend(function() {
        writes <<- c(writes, "notice\n")
        renderer$suspend(function() writes <<- c(writes, "detail\n"))
    })
    renderer$close()
    renderer$close()

    expect_equal(hidden, 1L)
    expect_equal(shown, 1L)
    expect_identical(writes[[3L]], "notice\n")
    expect_identical(writes[[4L]], "detail\n")
    expect_match(writes[[5L]], "one", fixed = TRUE)
    expect_false(renderer$active())
})

test_that("compact renderer delegates one live row to cli", {
    created <- 0L
    updates <- character()
    closed <- 0L
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    testthat::local_mocked_bindings(
        cli_progress_bar = function(...) {
            created <<- created + 1L
            "compact-id"
        },
        cli_progress_update = function(id, status, ...) {
            updates <<- c(updates, status)
            invisible(id)
        },
        cli_progress_done = function(...) {
            closed <<- closed + 1L
            invisible(TRUE)
        },
        .package = "cli"
    )
    renderer <- ShiftFrameRenderer$new(output, backend = "compact")
    renderer$draw(c("full row 1", "full row 2"), compact = "compact one")
    renderer$draw(c("full row 1", "full row 2"), compact = "compact two")
    renderer$close()

    expect_equal(created, 1L)
    expect_identical(updates, c("compact one", "compact two"))
    expect_equal(closed, 1L)
})

test_that("renderer backend uses cli capabilities and degrades safely", {
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    testthat::local_mocked_bindings(
        is_dynamic_tty = function(...) TRUE,
        .package = "cli"
    )
    expect_identical(shift__ui_renderer_backend(output), "compact")

    testthat::local_mocked_bindings(
        is_dynamic_tty = function(...) FALSE,
        .package = "cli"
    )
    expect_identical(shift__ui_renderer_backend(output), "log")
})

test_that("compact status preserves stage, unit, progress, and elapsed time", {
    state <- list(
        status = "running", stage = "resolve",
        stage_message = "Resolving workflow inputs",
        unit_label = "Checking catalog", unit_current = 2L, unit_total = 6L,
        current_details = list(current = 2L, total = 6L,
            node = INDEX_NODES[["CEDA"]], catalog_role = "reference"),
        elapsed_seconds = 15
    )
    line <- cli::ansi_strip(shift__ui_compact_line(state, width = 80L,
        motion = "full", frame = 1L))
    expect_match(line, "Resolve", fixed = TRUE)
    expect_match(line, "2/6", fixed = TRUE)
    expect_match(line, "CEDA", fixed = TRUE)
    expect_match(line, "reference", fixed = TRUE)
    expect_match(line, "15s", fixed = TRUE)
    expect_lte(nchar(line, type = "width"), 80L)
})

test_that("auto mode follows terminal capability for R and Rscript callers", {
    withr::local_envvar(c(CI = NA_character_, TERM = "xterm-256color"))
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

test_that("motion frames animate only in full mode", {
    expect_false(identical(
        shift__ui_spinner("full", 0L),
        shift__ui_spinner("full", 1L)
    ))
    expect_identical(
        shift__ui_spinner("reduced", 0L),
        shift__ui_spinner("reduced", 9L)
    )
    expect_identical(shift__ui_spinner("none", 0L), "")
})

test_that("stage labels cover Dataset tasks and safely format extensions", {
    expect_identical(shift__ui_stage_label("datasets"), "Datasets")
    expect_identical(shift__task_label("datasets"), "Collect Datasets")
    expect_identical(shift__ui_stage_label("custom_stage"), "custom stage")
    expect_identical(shift__task_label("custom_stage"), "custom stage")
})

test_that("recent activity keeps three semantic milestones", {
    reporter <- shift__reporter(shift_ui("none"))
    reporter$stage_started("resolve", "Resolving inputs.")
    for (i in seq_len(4L)) {
        reporter$unit_started(sprintf("Node %d", i), i, 4L)
        reporter$unit_completed(sprintf("Node %d rejected", i), i, 4L,
            outcome = "rejected")
    }
    state <- reporter$snapshot()
    expect_length(state$recent_events, 3L)
    expect_false(any(grepl("Node 1", state$recent_events, fixed = TRUE)))
    expect_true(any(grepl("Node 4", state$recent_events, fixed = TRUE)))
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
        spec_json = shift__spec_json(list(
            climate = list(model = "BCC-CSM2-MR", scenarios = "ssp585",
                member = NULL, grid = NULL, table = NULL),
            periods = list(`2060s` = 2055:2065),
            method = list(
                name = "belcher",
                recipe = list(
                    backend = "belcher", profile = "enhanced",
                    options = unclass(belcher_options())
                ),
                reference_mode = "baseline_epw"
            ),
            control = list(download = "auto")
        )),
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
            paste(
                "ORNL · selected member r1i1p1f1",
                "Amon=gn · LImon=gr", sep = " · "
            )
        ),
        details_json = vapply(list(
            list(stage = "resolve", phase = "stage", current = 1L, total = 6L,
                next_stage = "extract_future",
                stage_sequence = c("resolve", "extract_future",
                    "extract_reference", "coverage", "morph", "write_epw")),
            list(stage = "resolve", phase = "unit", unit_type = "index_node",
                unit_label = "ORNL · checking future + reference catalogs",
                current = 3L, total = 6L, node = INDEX_NODES[["ORNL"]]),
            list(stage = "resolve", phase = "unit", unit_type = "index_node",
                unit_label = paste(
                    "ORNL · selected member r1i1p1f1",
                    "Amon=gn · LImon=gr", sep = " · "
                ),
                current = 3L, total = 6L, node = INDEX_NODES[["ORNL"]],
                future_files = 48L, reference_files = 24L,
                outcome = "completed",
                result = "r1i1p1f1 · Amon=gn · LImon=gr")
        ), shift__spec_json, character(1L)),
        created_at = now + 0:2
    )
    cases <- data.table::data.table(
        experiment_id = c("ssp126", "ssp585"),
        period = "2060s", variant_label = "r1i1p1f1",
        status = c("pending", "pending")
    )
    view <- shift__ui_table_view(row, cases, events, width = 80L)
    plain <- cli::ansi_strip(view$lines)
    expect_match(plain[[1L]], "Future EPW", fixed = TRUE)
    expect_match(plain[[2L]], "BCC-CSM2-MR", fixed = TRUE)
    expect_match(paste(plain, collapse = " "),
        "belcher \\[enhanced\\]", perl = TRUE)
    expect_true(any(grepl("tables auto by variable", plain,
        fixed = TRUE)))
    expect_true(any(grepl("Workflow", plain, fixed = TRUE)))
    expect_true(any(grepl("Resolve", plain, fixed = TRUE)))
    expect_true(any(grepl("ORNL", plain, fixed = TRUE)))
    expect_true(any(grepl("node 3 of 6", plain, fixed = TRUE)))
    expect_true(any(grepl("Activity", plain, fixed = TRUE)))
    expect_match(paste(view$nodes, collapse = "\n"),
        "ORNL.*48.*24.*Amon=gn.*LImon=gr")
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
            size = c(1024, 2048, 4096),
            speed_bps = c(NA, 256, NA),
            filename = c("done.nc", "tas.nc", "queued.nc")
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
    expect_equal(metrics$speed_bps, 256)
    expect_equal(metrics$active_files, "tas.nc")
    expect_equal(metrics$active_task_count, 1L)
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

test_that("nested fallback downloads retain extraction unit ownership", {
    callbacks <- new.env(parent = emptyenv())
    calls <- character()
    downloader <- list(
        tasks = function(session_id = NULL) data.frame(
            status = "done", bytes_done = 1024, size = 1024,
            filename = "tas.nc"
        ),
        on = function(event, fun) {
            callbacks[[event]] <- fun
            event
        },
        off = function(token) TRUE
    )
    reporter <- list(
        ui = function() shift_ui("none"),
        unit_started = function(...) calls <<- c(calls, "started"),
        unit_updated = function(...) calls <<- c(calls, "updated"),
        unit_completed = function(...) calls <<- c(calls, "completed"),
        notice = function(...) invisible(NULL),
        heartbeat = function(...) invisible(TRUE)
    )
    cleanup <- shift__download_reporter_bind(
        downloader, reporter, role = "HTTP fallback", variables = 1L,
        nested = TRUE
    )
    event <- list(
        session_id = "fallback-session", filename = "tas.nc",
        target_path = NA_character_, data_node = "example.org"
    )
    callbacks$session_start(c(event, event = "session_start"), downloader)
    callbacks$session_done(c(event, event = "session_done"), downloader)
    cleanup()

    expect_equal(calls, c("updated", "updated"))
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

test_that("dynamic failures commit one structured terminal dashboard", {
    frames <- list()
    commits <- character()
    closes <- 0L
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) list(
            draw = function(lines, compact = NULL) {
                frames[[length(frames) + 1L]] <<- lines
                TRUE
            },
            suspend = function(code) code(),
            commit = function(result) {
                commits <<- c(commits, result)
                invisible(NULL)
            },
            close = function(...) {
                closes <<- closes + 1L
                invisible(NULL)
            }
        ),
        .package = "epwshiftr"
    )
    reporter <- shift__reporter(shift_ui("dynamic", motion = "none"))
    reporter$stage_started("resolve", "Resolving inputs.", 1L, 6L)
    reporter$unit_completed(
        "Rejected: incomplete coverage",
        current = 6L,
        total = 6L,
        outcome = "rejected",
        details = list(
            unit_type = "index_node", node = INDEX_NODES[["LIU"]],
            future_files = 83L, reference_files = 121L,
            error_kind = "coverage", error = "incomplete coverage"
        )
    )
    failure <- list(
        kind = "resolver_exhausted",
        summary = "No ESGF index node resolved a complete CMIP6 input set.",
        cause = "Future and historical catalogs have no complete identity in common.",
        nodes_checked = 6L, usable_nodes = 0L,
        coverage_failures = 5L, timeout_failures = 1L,
        network_failures = 0L, other_failures = 0L,
        closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn"),
        missing = "reference: historical/hurs: missing years 1995"
    )
    output <- capture.output(reporter$run_failed(
        message = failure$summary,
        details = failure
    ), type = "message")
    plain <- cli::ansi_strip(frames[[length(frames)]])

    expect_length(output, 0L)
    expect_identical(commits, "failed")
    expect_equal(closes, 0L)
    expect_match(plain[[1L]], "FAILED", fixed = TRUE)
    expect_true(any(grepl("Failure", plain, fixed = TRUE)))
    expect_true(any(grepl("Diagnosis", plain, fixed = TRUE)))
    expect_true(any(grepl("6 checked.*5 incomplete.*1 timeout", plain)))
    expect_true(any(grepl("BCC-CSM2-MR/r1i1p1f1/gn", plain, fixed = TRUE)))
    expect_false(any(grepl("Resolver attempts", output, fixed = TRUE)))
})

test_that("dynamic completions commit one durable results dashboard", {
    frames <- list()
    commits <- character()
    closes <- 0L
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) list(
            draw = function(lines, compact = NULL) {
                frames[[length(frames) + 1L]] <<- lines
                TRUE
            },
            suspend = function(code) code(),
            backend = function() "frame",
            commit = function(result) {
                commits <<- c(commits, result)
                invisible(NULL)
            },
            close = function(...) {
                closes <<- closes + 1L
                invisible(NULL)
            }
        ),
        .package = "epwshiftr"
    )
    output_dir <- tempfile("shift-completed-output-")
    paths <- file.path(output_dir, c(
        "BCC-CSM2-MR_ssp126_2060s.epw",
        "BCC-CSM2-MR_ssp585_2060s.epw"
    ))
    run <- shift_stage_new(
        ShiftRun,
        "run",
        ids = list(run_id = "run-complete"),
        meta = list(run = data.table::data.table(
            run_id = "run-complete",
            status = "completed",
            output_dir = output_dir
        ))
    )
    reporter <- shift__reporter(
        shift_ui("dynamic", motion = "none"),
        run_id = "run-complete"
    )
    reporter$stage_started("write_epw", "Writing final EPWs.")
    reporter$cases_updated(data.table::data.table(
        status = c("completed", "completed")
    ))
    reporter$unit_completed("Exported final EPWs", 2L, 2L,
        details = list(unit_type = "epw_export"))
    output <- capture.output(reporter$run_completed(
        run, data.table::data.table(export_path = paths)
    ), type = "message")
    plain <- cli::ansi_strip(frames[[length(frames)]])

    expect_length(output, 0L)
    expect_identical(commits, "done")
    expect_equal(closes, 0L)
    expect_true(any(grepl("Results", plain, fixed = TRUE)))
    expect_true(any(grepl("Summary.*2/2 EPWs exported.*0 missing", plain)))
    expect_true(any(grepl("Output", plain, fixed = TRUE)))
    expect_true(all(vapply(basename(paths), function(path) {
        grepl(path, paste(plain, collapse = ""), fixed = TRUE)
    }, logical(1L))))
})

test_that("log completions retain the append-only summary fallback", {
    output_dir <- tempfile("shift-log-output-")
    path <- file.path(output_dir, "BCC-CSM2-MR_ssp126_2060s.epw")
    run <- shift_stage_new(
        ShiftRun,
        "run",
        ids = list(run_id = "run-log-complete"),
        meta = list(run = data.table::data.table(
            run_id = "run-log-complete",
            status = "completed",
            output_dir = output_dir
        ))
    )
    reporter <- shift__reporter(
        shift_ui("log", detail = "normal"),
        run_id = "run-log-complete"
    )
    output <- capture.output(reporter$run_completed(
        run, data.table::data.table(export_path = path)
    ), type = "message")

    expect_true(any(grepl("1 output(s)", output, fixed = TRUE)))
    expect_true(any(grepl("Output directory", output, fixed = TRUE)))
})

test_that("failed dashboards remain legible without colour and at narrow widths", {
    withr::local_options(cli.num_colors = 1L)
    state <- list(
        run_id = "run-failed-narrow",
        status = "failed",
        stage = "resolve",
        stage_sequence = c("resolve", "extract_future", "coverage", "morph",
            "write_epw"),
        unit_label = "No ESGF index node resolved a complete input set",
        current_details = list(current = 6L, total = 6L),
        plan_context = list(
            line = "BCC-CSM2-MR · ssp126 + ssp585 · 2060s · belcher",
            selection = "member auto · grid auto"
        ),
        failure_details = list(
            summary = "No ESGF index node resolved a complete input set.",
            cause = "Future and historical catalogs have no complete identity in common.",
            nodes_checked = 6L, usable_nodes = 0L,
            coverage_failures = 5L, timeout_failures = 1L,
            closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn"),
            missing = "reference: historical/hurs: missing years 1995"
        ),
        elapsed_seconds = 30
    )

    wide <- shift__ui_status_lines(state, width = 80L, motion = "none")
    expect_false(any(cli::ansi_has_any(wide)))
    expect_match(wide[[1L]], "FAILED", fixed = TRUE)
    expect_true(any(grepl("Failure", wide, fixed = TRUE)))
    expect_true(any(grepl("Diagnosis", wide, fixed = TRUE)))
    expect_true(any(grepl("6 checked.*5 incomplete.*1 timeout", wide)))

    narrow <- shift__ui_status_lines(state, width = 32L, motion = "none")
    expect_true(all(cli::ansi_nchar(narrow, type = "width") <= 32L))
    expect_false(any(grepl("^[╭├╰│]", narrow)))
    expect_true(any(grepl("Failure", narrow, fixed = TRUE)))
    expect_true(any(grepl("Summary", narrow, fixed = TRUE)))
    expect_true(any(grepl("✖", narrow, fixed = TRUE)))
})

test_that("watch reconstruction retains the structured terminal diagnosis", {
    now <- as.POSIXct("2026-07-20 00:00:00", tz = "UTC")
    row <- data.table::data.table(
        run_id = "run-watch-failed",
        status = "failed",
        current_stage = "resolve",
        spec_json = shift__spec_json(list(
            climate = list(model = "BCC-CSM2-MR", scenarios = "ssp585"),
            periods = list(`2060s` = 2055:2065),
            method = list(name = "belcher", reference_mode = "baseline_epw")
        )),
        started_at = now,
        completed_at = now + 12,
        updated_at = now + 12
    )
    failure <- list(
        stage = "resolve", phase = "unit", unit_type = "index_node",
        unit_label = "No ESGF index node resolved a complete input set.",
        current = 6L, total = 6L,
        summary = "No ESGF index node resolved a complete input set.",
        cause = "Future catalog coverage is incomplete.",
        nodes_checked = 6L, coverage_failures = 6L, usable_nodes = 0L,
        closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn"),
        missing = "future: ssp585/hurs: missing years 2055"
    )
    events <- data.table::data.table(
        event_id = "failure",
        stage = "resolve",
        status = "failed",
        message = failure$summary,
        details_json = shift__spec_json(failure),
        created_at = now + 12
    )
    view <- shift__ui_table_view(row, data.table::data.table(), events,
        width = 80L)
    plain <- cli::ansi_strip(view$lines)

    expect_identical(view$state$failure_details$cause, failure$cause)
    expect_match(plain[[1L]], "FAILED", fixed = TRUE)
    expect_true(any(grepl("Diagnosis", plain, fixed = TRUE)))
    expect_true(any(grepl("6 checked.*6 incomplete", plain)))
    expect_true(any(grepl("BCC-CSM2-MR/r1i1p1f1/gn", plain,
        fixed = TRUE)))
})

test_that("normal resolver tables abbreviate repeated errors", {
    rows <- data.table::data.table(
        node = "DKRZ", future = 28L, reference = 39L,
        duration = "2s", outcome = "rejected",
        result = paste(
            "coverage: No complete CMIP6 member/grid candidate was found",
            "for model BCC-CSM2-MR with a very long diagnostic"
        )
    )
    normal <- shift__ui_node_table(rows, width = 180L, detail = "normal")
    detail <- shift__ui_node_table(rows, width = 180L, detail = "detail")

    expect_match(paste(normal, collapse = "\n"), "incomplete coverage",
        fixed = TRUE)
    expect_false(any(grepl("very long diagnostic", normal, fixed = TRUE)))
    expect_true(any(grepl("very long diagnostic", detail, fixed = TRUE)))
})

test_that("startup plan summaries include output and selection without a full dump", {
    skip_if_not_installed("duckdb")

    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            "BCC-CSM2-MR", c("ssp126", "ssp585"),
            member = "r1i1p1f1", grid = "gn",
            table = c(snd = "LImon")
        ),
        periods = list(`2060s` = 2055:2065),
        method = belcher(),
        dir = tempfile("shift-ui-output-"),
        store = tempfile("shift-ui-store-"),
        dry_run = TRUE
    )
    lines <- unname(shift__ui_plan_summary(plan, "run-test", width = 80L))
    expect_gte(length(lines), 5L)
    expect_true(all(nchar(lines, type = "width") <= 80L))
    expect_match(lines[[1L]], "Future EPW.*run-test.*STARTING")
    expect_match(lines[[2L]], "BCC-CSM2-MR.*ssp126.*2060s")
    expect_match(lines[[3L]],
        "belcher \\[enhanced\\].*baseline EPW.*2 expected")
    expect_match(paste(lines, collapse = " "),
        "Selection.*member r1i1p1f1.*grid gn.*tables auto by variable.*snd=LImon")
    expect_true(any(grepl("Output", lines, fixed = TRUE)))

    detail_lines <- unname(shift__ui_plan_summary(
        plan, "run-test", width = 80L, detail = "detail"
    ))
    detail_text <- paste(detail_lines, collapse = " ")
    expect_match(detail_text, "Options.*transition_hours=72")
    expect_match(detail_text, "snow_depth=auto")
    expect_match(detail_text, "design_conditions=drop")
})

test_that("persisted plan context marks pre-profile Belcher tasks as legacy", {
    row <- data.table::data.table(
        spec_json = shift__spec_json(list(
            task = "future_epw",
            climate = list(
                model = "BCC-CSM2-MR", scenarios = "ssp585",
                member = "r1i1p1f1", grid = "gn",
                table = list(snd = "LImon")
            ),
            periods = list(`2060s` = 2055:2065),
            method = list(
                name = "belcher", reference_mode = "baseline_epw"
            )
        )),
        output_dir = tempfile("future-epw-")
    )
    context <- shift__ui_plan_context_from_row(row, cases_total = 1L)

    expect_true("belcher [legacy] / baseline EPW" %in% context$items)
    expect_match(context$selection,
        "member r1i1p1f1.*grid gn.*auto by variable.*snd=LImon")
})

test_that("dynamic startup is a replaceable first frame rather than a transcript", {
    skip_if_not_installed("duckdb")

    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585")),
        periods = list(`2060s` = 2055:2065),
        method = belcher(),
        dir = tempfile("shift-ui-output-"),
        store = tempfile("shift-ui-store-"),
        dry_run = TRUE
    )
    frames <- list()
    closed <- 0L
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) list(
            draw = function(lines, compact = NULL) {
                frames[[length(frames) + 1L]] <<- lines
                TRUE
            },
            suspend = function(code) code(),
            close = function(...) {
                closed <<- closed + 1L
                invisible(NULL)
            }
        ),
        .package = "epwshiftr"
    )
    reporter <- shift__reporter(shift_ui("dynamic", motion = "none"))
    startup_output <- capture.output(
        reporter$run_started(plan, "run_534d9b84b235029404b318bd"),
        type = "message"
    )
    expect_length(startup_output, 0L)
    expect_length(frames, 1L)
    plain <- cli::ansi_strip(frames[[1L]])

    milestone_output <- capture.output({
        reporter$stage_started("resolve", "Resolving inputs.", 1L, 6L)
        reporter$stage_completed("Resolved inputs.")
    }, type = "message")
    reporter$close()

    expect_length(milestone_output, 0L)
    expect_match(plain[[1L]], "run 04b318bd", fixed = TRUE)
    expect_match(paste(plain, collapse = " "),
        "BCC-CSM2-MR.*ssp126.*belcher")
    expect_true(any(grepl("Workflow", plain, fixed = TRUE)))
    expect_true(any(grepl("Resolve", plain, fixed = TRUE)))
    expect_equal(closed, 1L)
})

test_that("wide resolver frames keep one motion focus and terse node outcomes", {
    state <- list(
        run_id = "run_534d9b84b235029404b318bd",
        status = "running",
        stage = "resolve",
        stage_sequence = c("resolve", "extract_future", "extract_reference",
            "coverage", "morph", "write_epw"),
        current_details = list(
            current = 2L, total = 6L, node = INDEX_NODES[["CEDA"]],
            catalog_role = "future", unit_label = "Waiting for catalog response"
        ),
        unit_label = "Waiting for catalog response",
        unit_current = 2L,
        unit_total = 6L,
        plan_context = list(
            line = paste("BCC-CSM2-MR", "ssp126 + ssp585", "2060s (2055\u20132065)",
                "belcher / historical 1995\u20132014", "2 EPWs", sep = " \u00b7 "),
            selection = "member auto \u00b7 grid auto"
        ),
        node_rows = data.table::data.table(
            node = "DKRZ", future = 28L, reference = 0L,
            outcome = "rejected", duration = "10s",
            result = paste("coverage: No complete CMIP6 member/grid candidate",
                "was found for model BCC-CSM2-MR")
        ),
        elapsed_seconds = 12
    )
    frame <- shift__ui_status_lines(state, width = 180L,
        motion = "full", frame = 2L)
    plain <- cli::ansi_strip(frame)
    spinner <- shift__ui_spinner("full", 2L)

    expect_gte(length(frame), 11L)
    expect_true(all(cli::ansi_nchar(frame, type = "width") == 179L))
    expect_equal(sum(grepl(spinner, plain, fixed = TRUE)), 1L)
    expect_true(any(grepl("Workflow", plain, fixed = TRUE)))
    expect_true(any(grepl("node 2 of 6", plain)))
    expect_false(any(grepl("%", plain, fixed = TRUE)))
    expect_true(any(grepl("Activity", plain, fixed = TRUE)))
    node_line <- plain[grepl("no reference files", plain, fixed = TRUE)]
    expect_length(node_line, 1L)
    expect_false(grepl("BCC-CSM2-MR", node_line, fixed = TRUE))
})

test_that("dashboard plan content reflows with the current terminal width", {
    context <- list(items = c(
        "BCC-CSM2-MR",
        "ssp126 + ssp585",
        "2060s (2055–2065)",
        "belcher / historical 1995–2014",
        "2 EPWs"
    ))
    medium <- shift__ui_plan_lines(context, width = 90L)
    wide <- shift__ui_plan_lines(context, width = 180L)

    expect_gt(length(medium), 1L)
    expect_length(wide, 1L)
    expect_match(paste(cli::ansi_strip(medium), collapse = " "),
        "2 EPWs", fixed = TRUE)
    expect_false(grepl("…",
        paste(cli::ansi_strip(medium), collapse = " "), fixed = TRUE))
    expect_equal(shift__ui_dashboard_width(90L), 89L)
    expect_equal(shift__ui_dashboard_width(180L), 179L)
})

test_that("dashboard prose wraps semantically and preserves a safe terminal column", {
    state <- list(
        run_id = "run-wrap", status = "failed", stage = "resolve",
        stage_sequence = c("resolve", "extract_future", "extract_reference",
            "coverage", "morph", "write_epw"),
        unit_label = paste("LIU reference catalog could not resolve",
            "a complete CMIP6 input set for this workflow"),
        current_details = list(current = 6L, total = 6L),
        plan_context = list(items = c(
            "BCC-CSM2-MR", "ssp126 + ssp585", "2060s (2055–2065)",
            "belcher / historical 1995–2014", "2 EPWs"),
            selection = "member auto · grid auto"),
        failure_details = list(
            cause = paste("No member and grid covers every requested future",
                "scenario, humidity input, and target year."),
            nodes_checked = 6L, coverage_failures = 5L,
            timeout_failures = 1L, usable_nodes = 0L,
            closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1",
                grid = "gn"),
            missing = paste("future: ssp126/hurs has no files;",
                "huss plus pressure may be available")),
        elapsed_seconds = 39
    )

    for (terminal_width in c(60L, 80L, 121L)) {
        lines <- shift__ui_status_lines(state, width = terminal_width,
            motion = "none")
        plain <- cli::ansi_strip(lines)
        expect_true(all(cli::ansi_nchar(lines, type = "width") <=
            terminal_width - 1L))
        expect_false(any(cli::ansi_nchar(lines, type = "width") ==
            terminal_width))
        expect_true(any(grepl("Failure", plain, fixed = TRUE)))
        expect_true(any(grepl("Summary", plain, fixed = TRUE)))
        expect_match(paste(trimws(plain), collapse = " "),
            "humidity.*input, and target year")
    }
})

test_that("flow rail chooses semantic compact variants instead of clipping", {
    state <- list(
        status = "running", stage = "resolve",
        stage_sequence = c("resolve", "extract_future", "extract_reference",
            "coverage", "morph", "write_epw")
    )
    medium <- cli::ansi_strip(shift__ui_stage_rail(state, width = 44L))
    narrow <- cli::ansi_strip(shift__ui_stage_rail(state, width = 25L))

    expect_match(medium, "Flow.*Resolve", fixed = FALSE)
    expect_false(grepl("Coverage", medium, fixed = TRUE))
    expect_match(narrow, "Flow.*Resolve", fixed = FALSE)
    expect_false(any(grepl("…", c(medium, narrow), fixed = TRUE)))
    expect_true(cli::ansi_nchar(medium, type = "width") <= 44L)
    expect_true(cli::ansi_nchar(narrow, type = "width") <= 25L)
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
    expect_true(any(grepl("Future EPW", output, fixed = TRUE)))
    expect_true(any(grepl("Plan", output, fixed = TRUE)))
    expect_true(any(grepl("Workflow", output, fixed = TRUE)))
    expect_true(any(grepl("Flow", output, fixed = TRUE)))
    expect_true(any(grepl("Now", output, fixed = TRUE)))
    expect_true(any(grepl("Status", output, fixed = TRUE)))
    expect_true(any(grepl("Activity", output, fixed = TRUE)))
    expect_true(any(grepl("Recent", output, fixed = TRUE)))
    expect_false(any(grepl("queued | planned | cases", output, fixed = TRUE)))
})

test_that("generic operation reporters preserve receipts in log and dynamic modes", {
    log_output <- capture.output({
        reporter <- ShiftReporter$new(shift_ui("log"),
            run_id = "run-generic-log", step_id = "step-generic-log")
        reporter$operation_started("collect", "Collect CMIP6",
            context = list(items = c("Collect CMIP6", "input request")),
            stage_sequence = c("collect", "extract"))
        reporter$operation_waiting("12 files collected")
    }, type = "message")
    expect_true(any(grepl("Collect CMIP6", log_output, fixed = TRUE)))
    expect_true(any(grepl("ready: 12 files collected", log_output,
        fixed = TRUE)))

    quiet_output <- capture.output({
        reporter <- ShiftReporter$new(shift_ui("none"),
            run_id = "run-generic-none")
        reporter$operation_started("extract", "Extract Climate")
        reporter$operation_waiting("3 plans processed")
    }, type = "message")
    expect_length(quiet_output, 0L)

    draws <- 0L
    commits <- character()
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) list(
            draw = function(...) {
                draws <<- draws + 1L
                TRUE
            },
            commit = function(result = c("done", "failed", "cancelled")) {
                result <- match.arg(result)
                commits <<- c(commits, result)
                invisible(NULL)
            },
            close = function(...) invisible(NULL),
            suspend = function(code) code(),
            backend = function() "frame"
        ),
        .package = "epwshiftr"
    )
    reporter <- ShiftReporter$new(shift_ui("dynamic"),
        run_id = "run-generic-dynamic")
    reporter$operation_started("datasets", "Collect Datasets",
        stage_sequence = "datasets")
    reporter$operation_completed("1 dataset collected")
    expect_gte(draws, 2L)
    expect_identical(commits, "done")

    # A successful intermediate operation keeps the workflow waiting for the
    # next R call, but releases the terminal renderer through its done state.
    reporter <- ShiftReporter$new(shift_ui("dynamic"),
        run_id = "run-generic-waiting")
    reporter$operation_started("collect", "Collect CMIP6")
    reporter$operation_waiting("12 files collected")
    expect_identical(commits, c("done", "done"))

    # Incomplete artifacts keep a durable receipt but do not advertise that a
    # downstream stage is ready to start.
    reporter <- ShiftReporter$new(shift_ui("dynamic"),
        run_id = "run-generic-partial")
    reporter$operation_started("collect", "Collect CMIP6")
    reporter$operation_partial("0 files collected")
    expect_identical(commits, rep("done", 3L))

    # Detached background work likewise leaves the run active after the local
    # operation ends without turning renderer cleanup into a workflow failure.
    reporter <- ShiftReporter$new(shift_ui("dynamic"),
        run_id = "run-generic-running")
    reporter$operation_started("download", "Download CMIP6")
    reporter$operation_detached("download continues in background")
    expect_identical(commits, rep("done", 4L))
})

test_that("dynamic watch animates cached state between store polls", {
    skip_if_not_installed("duckdb")

    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6("BCC-CSM2-MR", "ssp585"),
        periods = list(`2060s` = 2060L),
        method = belcher(),
        dir = tempfile("shift-watch-animation-output-"),
        store = tempfile("shift-watch-animation-store-"),
        dry_run = TRUE
    )
    run_id <- shift__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    base <- shift__run_handle(store, run_id)
    running <- base
    running_meta <- running@meta
    running_meta$run <- data.table::copy(running_meta$run)
    running_meta$run$status <- "running"
    running_meta$run$current_stage <- "resolve"
    running_meta$run$started_at <- Sys.time()
    running@meta <- running_meta
    completed <- running
    completed_meta <- completed@meta
    completed_meta$run <- data.table::copy(completed_meta$run)
    completed_meta$run$status <- "completed"
    completed_meta$run$completed_at <- Sys.time()
    completed@meta <- completed_meta

    polls <- 0L
    updates <- 0L
    closes <- 0L
    clock <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    testthat::local_mocked_bindings(
        shift_run_get = function(...) {
            polls <<- polls + 1L
            if (polls >= 3L) completed else running
        },
        shift__watch_now = function() clock,
        shift__watch_sleep = function(seconds) {
            clock <<- clock + seconds
            invisible(NULL)
        },
        .package = "epwshiftr"
    )
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) list(
            draw = function(...) {
                updates <<- updates + 1L
                TRUE
            },
            close = function(...) {
                closes <<- closes + 1L
                invisible(NULL)
            }
        ),
        .package = "epwshiftr"
    )

    capture.output(
        result <- shift_watch(
            running, follow = TRUE, interval = 0.5,
            ui = shift_ui("dynamic", refresh = 0.125)
        ),
        type = "message"
    )
    expect_equal(shift_status(result, refresh = FALSE), "completed")
    # Initial/final handle refreshes surround two workflow polls. The fake
    # clock advances four dashboard frames before the completion poll.
    expect_equal(polls, 4L)
    expect_equal(updates, 4L)
    expect_gte(closes, 1L)
})

test_that("wide characters and narrow terminals never overflow their display width", {
    for (width in c(20L, 32L, 40L, 80L)) {
        value <- shift__ui_fit(strrep("测", 40L), width)
        expect_lte(cli::ansi_nchar(value, type = "width"), width)
    }
    token <- "/very/long/path/without/spaces"
    wrapped_token <- shift__ui_hard_wrap(token, 8L)
    expect_true(all(cli::ansi_nchar(wrapped_token, type = "width") <= 8L))
    expect_identical(paste0(cli::ansi_strip(wrapped_token), collapse = ""),
        token)

    cases <- data.table::data.table(
        experiment_id = "ssp585",
        period = "2060年代",
        variant_label = "r1i1p1f1",
        status = "missing",
        missing_reason = "缺少近地面比湿变量：2055–2065 全部年份"
    )
    for (width in c(32L, 40L, 60L, 80L)) {
        lines <- shift__ui_case_table(cases, width = width, detail = "detail")
        expect_true(all(cli::ansi_nchar(lines, type = "width") <= width))
    }

    state <- list(
        run_id = "run-narrow-terminal", status = "running",
        stage = "extract_future",
        stage_sequence = c("resolve", "extract_future", "extract_reference",
            "coverage", "morph", "write_epw"),
        completed_stages = "resolve",
        unit_label = "ssp585 \u00b7 near-surface air temperature \u00b7 2055\u20132065",
        unit_current = 5L, unit_total = 16L,
        current_details = list(current = 5L, total = 16L,
            variable = "tas", scenario = "ssp585"),
        elapsed_seconds = 20
    )
    for (width in c(24L, 32L, 40L, 80L)) {
        lines <- shift__ui_status_lines(state, width = width,
            motion = "full", frame = 3L)
        expect_true(all(cli::ansi_nchar(lines, type = "width") <= width - 1L))
    }
})

test_that("terminal dashboards stop all animation", {
    state <- list(
        run_id = "run-complete", status = "completed", stage = "write_epw",
        stage_sequence = c("resolve", "extract_future", "coverage", "morph",
            "write_epw"),
        completed_stages = c("resolve", "extract_future", "coverage", "morph",
            "write_epw"),
        unit_label = "Exported final EPW", unit_current = 2L, unit_total = 2L,
        current_details = list(current = 2L, total = 2L,
            outcome = "completed"),
        outputs_completed = 2L, cases_total = 2L,
        output_dir = "/tmp/future epw output",
        output_paths = c(
            "/tmp/future epw output/BCC-CSM2-MR_ssp126_2060s.epw",
            "/tmp/future epw output/BCC-CSM2-MR_ssp585_2060s.epw"
        ),
        elapsed_seconds = 30
    )
    first <- cli::ansi_strip(shift__ui_status_lines(state,
        motion = "full", frame = 1L))
    second <- cli::ansi_strip(shift__ui_status_lines(state,
        motion = "full", frame = 7L))
    expect_identical(first, second)
    expect_match(first[[1L]], "COMPLETED")
    expect_true(any(grepl("100%", first, fixed = TRUE)))
    expect_true(any(grepl("Results", first, fixed = TRUE)))
    expect_true(any(grepl("Summary.*2/2 EPWs exported.*0 missing", first)))
    expect_true(any(grepl("Output", first, fixed = TRUE)))
})

test_that("completion receipts preserve long output names at every width", {
    filename <- paste0(
        "BCC-CSM2-MR_ssp585_r1i1p1f1_gn_Singapore_",
        "2060s_2055-2065_final.epw"
    )
    state <- list(
        run_id = "run-complete-narrow",
        status = "completed",
        stage = "write_epw",
        stage_sequence = c("resolve", "extract_future", "coverage", "morph",
            "write_epw"),
        completed_stages = c("resolve", "extract_future", "coverage", "morph",
            "write_epw"),
        unit_label = "Exported final EPW",
        current_details = list(current = 1L, total = 1L,
            outcome = "completed"),
        outputs_completed = 1L,
        cases_total = 1L,
        output_dir = "/tmp/a deliberately long future epw output directory",
        output_paths = file.path("/tmp", filename),
        elapsed_seconds = 30
    )

    for (width in c(24L, 40L, 60L, 80L)) {
        lines <- shift__ui_status_lines(state, width = width,
            motion = "none")
        plain <- cli::ansi_strip(lines)
        normalized <- gsub("[[:space:]]+", " ", paste(plain,
            collapse = " "))
        file_start <- grep("Files", plain, fixed = TRUE)[[1L]]
        file_text <- gsub("[[:space:]│]+", "", paste(
            plain[file_start:length(plain)], collapse = ""))
        expect_true(all(cli::ansi_nchar(lines, type = "width") <= width - 1L))
        expect_true(grepl(filename, file_text, fixed = TRUE))
        expect_true(grepl("1/1 EPW exported", normalized, fixed = TRUE))
    }
})

test_that("download stages expose measured transfer metrics and active files", {
    state <- list(
        run_id = "run-download",
        stage = "download",
        status = "running",
        elapsed_seconds = 20,
        unit_label = "future download · tas.nc + hurs.nc",
        unit_current = 2L,
        unit_total = 8L,
        current_details = list(
            unit_type = "download_session",
            current = 2L,
            total = 8L,
            bytes_done = 1024^2,
            bytes_total = 4 * 1024^2,
            speed_bps = 512 * 1024,
            eta_seconds = 6,
            active_task_count = 2L,
            active_files = c("tas.nc", "hurs.nc")
        ),
        cases_ready = 0L,
        cases_total = 2L,
        outputs_completed = 0L,
        last_event = "Selected r1i1p1f1 / gn"
    )
    lines <- shift__ui_status_lines(state, width = 100L)
    plain <- cli::ansi_strip(lines)
    expect_true(any(grepl("2/8", plain, fixed = TRUE)))
    expect_true(any(grepl("1.0 MiB/4.0 MiB", plain, fixed = TRUE)))
    expect_true(any(grepl("512.0 KiB/s", plain, fixed = TRUE)))
    expect_true(any(grepl("ETA 6s", plain, fixed = TRUE)))
    expect_true(any(grepl("2 active", plain, fixed = TRUE)))
})

test_that("coverage, morph, and EPW stages expose distinct metrics", {
    base <- list(
        status = "running", stage_message = "Working", unit_label = "case",
        unit_current = 1L, unit_total = 2L,
        current_details = list(current = 1L, total = 2L),
        cases_ready = 1L, cases_total = 2L, outputs_completed = 0L,
        last_event = "Resolved", elapsed_seconds = 1
    )

    coverage <- shift__ui_status_lines(utils::modifyList(base,
        list(stage = "coverage")), width = 80L)
    morph <- shift__ui_status_lines(utils::modifyList(base,
        list(stage = "morph")), width = 80L)
    epw <- shift__ui_status_lines(utils::modifyList(base,
        list(stage = "write_epw", outputs_completed = 1L)), width = 80L)

    expect_true(any(grepl("Cases.*1/2.*ready 1.*missing 1",
        cli::ansi_strip(coverage))))
    expect_true(any(grepl("Cases.*1/2", cli::ansi_strip(morph))))
    expect_true(any(grepl("EPWs.*1/2.*exported 1/2",
        cli::ansi_strip(epw))))
})

test_that("transient updates do not replace the last completed milestone", {
    reporter <- shift__reporter(shift_ui("none"))
    reporter$stage_started("resolve", "Resolving inputs.")
    reporter$unit_started("Checking catalogs", 1L, 2L)
    reporter$unit_completed("Selected DKRZ", 1L, 2L)
    reporter$unit_updated("Waiting for the next catalog", 2L, 2L)
    expect_identical(reporter$snapshot()$last_event, "Selected DKRZ")
})

test_that("heartbeat details update live state without unthrottled durable touches", {
    touches <- 0L
    testthat::local_mocked_bindings(
        shift__job_touch = function(store, job_id, ui_state = NULL) {
            touches <<- touches + 1L
            invisible(NULL)
        },
        shift__job_check_cancel = function(...) invisible(FALSE),
        .package = "epwshiftr"
    )
    reporter <- shift__reporter(
        shift_ui("none", heartbeat = 10),
        store = list(), run_id = "run-test", job_id = "job-test"
    )
    reporter$heartbeat(details = list(
        unit_type = "catalog", catalog_role = "future",
        access_method = "HTTPServer", bytes_done = 1024
    ))
    reporter$heartbeat(details = list(bytes_done = 2048))

    state <- reporter$snapshot()
    expect_identical(state$current_details$catalog_role, "future")
    expect_identical(state$current_details$access_method, "HTTPServer")
    expect_equal(state$current_details$bytes_done, 2048)
    expect_equal(touches, 1L)
})

test_that("dynamic frames advance faster than durable liveness", {
    touches <- 0L
    updates <- 0L
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) list(
            draw = function(...) {
                updates <<- updates + 1L
                TRUE
            },
            suspend = function(code) code(),
            close = function(...) invisible(NULL)
        ),
        .package = "epwshiftr"
    )
    testthat::local_mocked_bindings(
        shift__job_touch = function(store, job_id, ui_state = NULL) {
            touches <<- touches + 1L
            invisible(NULL)
        },
        shift__job_check_cancel = function(...) invisible(FALSE),
        .package = "epwshiftr"
    )
    reporter <- shift__reporter(
        shift_ui("dynamic", motion = "full", refresh = 0.05,
            heartbeat = 100),
        store = list(), run_id = "run-test", job_id = "job-test"
    )
    reporter$heartbeat("Waiting for catalog")
    Sys.sleep(0.06)
    reporter$heartbeat("Waiting for catalog")

    expect_equal(touches, 1L)
    expect_gte(updates, 2L)
})

test_that("append-only logs preserve complete messages at narrow widths", {
    withr::local_options(width = 20L)
    reporter <- shift__reporter(shift_ui("log", detail = "debug"))
    reporter$stage_started("resolve", "Resolving inputs.")
    message <- paste(rep("complete-catalog-context", 8L), collapse = " ")
    output <- capture.output(reporter$unit_started(message), type = "message")
    expect_true(any(grepl(message, output, fixed = TRUE)))
})

test_that("auto mode uses logs in CI and dumb terminals", {
    withr::local_envvar(c(CI = "true", TERM = "xterm-256color"))
    expect_identical(shift__ui_mode(shift_ui("auto")), "log")
    withr::local_envvar(c(CI = NA_character_, TERM = "dumb"))
    expect_identical(shift__ui_mode(shift_ui("auto")), "log")
})
