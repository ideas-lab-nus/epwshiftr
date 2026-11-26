# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

UI_TEST_STAGES <- c(
    "resolve",
    "extract_future",
    "extract_reference",
    "coverage",
    "morph",
    "write_epw"
)

ui_test__plain <- function(x, collapse = "\n") {
    paste(cli::ansi_strip(x), collapse = collapse)
}
ui_test__msg <- function(expr, collapse = "\n") {
    ui_test__plain(capture.output(expr, type = "message"), collapse = collapse)
}

ui_test__plan <- function(table = NULL) {
    shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "BCC-CSM2-MR",
            c("ssp126", "ssp585"),
            member = "r1i1p1f1",
            grid = "gn",
            table = table
        ),
        periods = list(`2060s` = 2055:2065),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-ui-output-"),
        store = tempfile("shift-ui-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
}

test_that("shared status and compact views adapt to stage, width, and transfer metrics", {
    resolve_state <- shift_test__ui_state(
        stage_sequence = UI_TEST_STAGES,
        stage_current = 1L,
        stage_total = 6L,
        unit_label = strrep("DKRZ waiting ", 8L),
        unit_current = 1L,
        unit_total = 6L,
        plan_context = list(
            line = "BCC-CSM2-MR \u00b7 ssp126 + ssp585 \u00b7 2060s \u00b7 original_morphing",
            selection = "member auto \u00b7 grid auto"
        ),
        elapsed_seconds = 12
    )
    wide <- unname(shift_ui_view__ui_status_lines(resolve_state, width = 80L))
    wide_plain <- cli::ansi_strip(wide)
    wide_text <- paste(wide_plain, collapse = "\n")
    expect_gte(length(wide), 9L)
    expect_true(all(cli::ansi_nchar(wide, type = "width") == 79L))
    expect_match(wide_plain[[1L]], "^╭─ Future EPW.*RUNNING")
    expect_match(wide_text, "BCC-CSM2-MR.*original_morphing")
    expect_match(wide_text, "node 1 of 6.*member auto")
    expect_false(grepl("%", wide_text, fixed = TRUE))
    expect_match(utils::tail(wide_plain, 1L), "^╰─")

    narrow <- cli::ansi_strip(shift_ui_view__ui_status_lines(
        resolve_state,
        width = 59L
    ))
    expect_false(any(grepl("^[╭├╰│]", narrow)))
    expect_match(narrow[[1L]], "Future EPW", fixed = TRUE)

    # Compact line preserves stage, unit, progress, and elapsed time
    compact_state <- shift_test__ui_state(
        unit_label = "Checking catalog",
        unit_current = 2L,
        unit_total = 6L,
        current_details = list(
            current = 2L,
            total = 6L,
            node = INDEX_NODES[["CEDA"]],
            catalog_role = "reference"
        ),
        elapsed_seconds = 15
    )
    compact <- cli::ansi_strip(shift_ui_view__ui_compact_line(
        compact_state,
        width = 80L,
        motion = "full",
        frame = 1L
    ))
    expect_match(compact, "Resolve.*2/6.*CEDA.*reference.*15s")
    expect_lte(nchar(compact, type = "width"), 80L)

    # Download transfer metrics
    dl_state <- shift_test__ui_state(
        stage = "download",
        elapsed_seconds = 20,
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
        )
    )
    dl_text <- ui_test__plain(shift_ui_view__ui_status_lines(
        dl_state,
        width = 100L
    ))
    expect_match(dl_text, "2/8.*1\\.0 MiB/4\\.0 MiB.*512\\.0 KiB/s.*ETA 6s")
    expect_match(dl_text, "2 active", fixed = TRUE)

    # Coverage, morph, and write_epw stage-specific metrics
    base <- shift_test__ui_state(
        unit_label = "case",
        unit_current = 1L,
        unit_total = 2L,
        current_details = list(current = 1L, total = 2L),
        cases_ready = 1L
    )
    st <- function(...) {
        ui_test__plain(shift_ui_view__ui_status_lines(
            utils::modifyList(base, list(...)),
            width = 80L
        ))
    }
    expect_match(st(stage = "coverage"), "Cases.*1/2.*ready 1.*missing 1")
    expect_match(st(stage = "morph"), "Cases.*1/2")
    expect_match(
        st(stage = "write_epw", outputs_completed = 1L),
        "EPWs.*1/2.*exported 1 files"
    )
})

test_that("ShiftFrameRenderer paints atomically, suspends/restores, and commits cleanly", {
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
    expect_true(renderer$draw(c("one", "two", "three")))
    expect_true(renderer$draw(c("ONE", "TWO")))
    expect_identical(writes[[1L]], "\rone\033[K\n\rtwo\033[K\n\rthree\033[K\r")
    expect_identical(
        writes[[2L]],
        "\033[2A\rONE\033[K\n\rTWO\033[K\r\n\r\033[K\033[1A\r"
    )

    renderer$suspend(function() {
        writes <<- c(writes, "notice\n")
        renderer$suspend(function() writes <<- c(writes, "detail\n"))
    })
    expect_true(all(c("notice\n", "detail\n") %in% writes))

    renderer$commit("failed")
    renderer$commit("failed")
    expect_identical(utils::tail(writes, 1L), "\n")
    expect_equal(c(hidden, shown), c(1L, 1L))
    expect_false(renderer$active())
})

test_that("compact renderer, backend detection, and auto UI mode follow terminal capability", {
    created <- 0L
    closed <- 0L
    updates <- character()
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
        is_dynamic_tty = function(...) TRUE,
        .package = "cli"
    )
    renderer <- ShiftFrameRenderer$new(output, backend = "compact")
    renderer$draw(c("full 1", "full 2"), compact = "compact one")
    renderer$draw(c("full 1", "full 2"), compact = "compact two")
    renderer$close()
    expect_equal(c(created, closed), c(1L, 1L))
    expect_identical(updates, c("compact one", "compact two"))

    withr::local_envvar(c(CI = NA_character_, TERM = "xterm-256color"))
    expect_identical(shift_tui__ui_renderer_backend(output), "compact")
    expect_identical(shift_ui__ui_mode(shift_ui("auto")), "dynamic")

    testthat::local_mocked_bindings(
        is_dynamic_tty = function(...) FALSE,
        .package = "cli"
    )
    expect_identical(shift_tui__ui_renderer_backend(output), "log")
    expect_identical(shift_ui__ui_mode(shift_ui("auto")), "log")

    withr::local_envvar(c(CI = "true", TERM = "xterm-256color"))
    expect_identical(shift_ui__ui_mode(shift_ui("auto")), "log")
    withr::local_envvar(c(CI = NA_character_, TERM = "dumb"))
    expect_identical(shift_ui__ui_mode(shift_ui("auto")), "log")
})

test_that("reporter retains recent milestones without clobbering last completed event", {
    reporter <- shift_reporter__reporter(shift_ui("none"))
    reporter$stage_started("resolve", "Resolving inputs.")
    for (i in seq_len(4L)) {
        reporter$unit_started(sprintf("Node %d", i), i, 4L)
        reporter$unit_completed(
            sprintf("Node %d rejected", i),
            i,
            4L,
            outcome = "rejected"
        )
    }
    reporter$unit_updated("Waiting for the next catalog", 4L, 4L)
    state <- reporter$snapshot()

    expect_length(state$recent_events, 3L)
    expect_false(any(grepl("Node 1", state$recent_events, fixed = TRUE)))
    expect_true(any(grepl("Node 4", state$recent_events, fixed = TRUE)))
    expect_identical(state$unit_label, "Waiting for the next catalog")
    expect_identical(state$last_event, "Node 4 rejected")
})

test_that("node/case tables, error summaries, and persisted events respect detail level", {
    nodes <- data.table::data.table(
        node = c("DKRZ", "CEDA", "ORNL"),
        future = c(38L, NA_integer_, 48L),
        reference = c(12L, NA_integer_, 24L),
        outcome = c("rejected", "rejected", "completed"),
        result = c(
            "coverage: No complete CMIP6 member/grid candidate was found for model BCC-CSM2-MR with a very long diagnostic",
            "HTTP 503 while collecting the reference catalog",
            "selected r1i1p1f1 / gn"
        )
    )
    normal_nodes <- ui_test__plain(shift_ui_view__ui_node_table(
        nodes,
        width = 120L,
        detail = "normal"
    ))
    detail_nodes <- ui_test__plain(shift_ui_view__ui_node_table(
        nodes,
        width = 180L,
        detail = "detail"
    ))
    expect_match(normal_nodes, "ORNL.*48.*24.*selected")
    expect_match(normal_nodes, "incomplete coverage", fixed = TRUE)
    expect_false(grepl("very long diagnostic", normal_nodes, fixed = TRUE))
    expect_true(grepl("very long diagnostic", detail_nodes, fixed = TRUE))

    cases <- data.table::data.table(
        experiment_id = c("ssp126", "ssp585"),
        period = "2060s",
        variant_label = "r1i1p1f1",
        status = c("ready", "missing"),
        missing_reason = c(NA_character_, "future/hurs, future/tas")
    )
    normal_cases <- unname(shift_ui_view__ui_case_table(
        cases,
        width = 60L,
        detail = "normal"
    ))
    detail_cases <- unname(shift_ui_view__ui_case_table(
        cases,
        width = 80L,
        detail = "detail"
    ))
    expect_true(all(nchar(normal_cases, type = "width") <= 60L))
    expect_false(any(grepl("future/hurs", normal_cases, fixed = TRUE)))
    expect_true(any(grepl("future/hurs", detail_cases, fixed = TRUE)))

    expect_identical(
        shift_print__error_summary(
            "No ESGF index node produced a complete input set.\nx DKRZ: missing hurs\ni More detail"
        ),
        "No ESGF index node produced a complete input set."
    )

    event <- data.table::data.table(
        stage = "resolve",
        message = "ORNL · future catalog · 12 files",
        details_json = shift_persist__spec_json(list(
            node = INDEX_NODES[["ORNL"]],
            catalog_role = "future"
        )),
        created_at = as.POSIXct("2026-07-20 00:00:00", tz = "UTC")
    )
    normal_ev <- shift_ui_view__ui_persisted_event_line(
        event,
        detail = "normal"
    )
    debug_ev <- shift_ui_view__ui_persisted_event_line(event, detail = "debug")
    expect_match(normal_ev, "[ORNL][future]", fixed = TRUE)
    expect_false(grepl("https://", normal_ev, fixed = TRUE))
    expect_true(grepl("https://", debug_ev, fixed = TRUE))
})

test_that("persisted watch tables rebuild running state, terminal elapsed time, and failure diagnosis", {
    now <- as.POSIXct("2026-07-20 00:00:00", tz = "UTC")
    spec <- shift_persist__spec_json(list(
        climate = list(model = "BCC-CSM2-MR", scenarios = "ssp585"),
        periods = list(`2060s` = 2055:2065),
        transform = transform__spec_value(monthly_transform("epwshiftr")),
        control = list(download = "auto")
    ))
    row <- data.table::data.table(
        run_id = "run-test",
        status = "running",
        current_stage = "resolve",
        spec_json = spec,
        started_at = now,
        completed_at = as.POSIXct(NA, tz = "UTC")
    )
    items <- list(
        list(
            stage = "resolve",
            phase = "stage",
            current = 1L,
            total = 6L,
            stage_sequence = UI_TEST_STAGES
        ),
        list(
            stage = "resolve",
            phase = "unit",
            unit_type = "index_node",
            current = 3L,
            total = 6L,
            node = INDEX_NODES[["ORNL"]],
            future_files = 48L,
            reference_files = 24L,
            outcome = "completed",
            result = "r1i1p1f1 · Amon=gn · LImon=gr"
        )
    )
    events <- data.table::data.table(
        event_id = c("stage", "node-done"),
        stage = "resolve",
        status = c("running", "completed"),
        message = c("Resolving inputs.", "ORNL · selected"),
        details_json = vapply(items, shift_persist__spec_json, character(1L)),
        created_at = now + 0:1
    )
    cases <- data.table::data.table(
        experiment_id = c("ssp126", "ssp585"),
        period = "2060s",
        variant_label = "r1i1p1f1",
        status = "pending"
    )
    view <- shift_ui_view__ui_table_view(row, cases, events, width = 80L)
    plain <- ui_test__plain(view$lines)
    expect_match(plain, "Future EPW", fixed = TRUE)
    expect_match(plain, "epwshiftr / no reference", fixed = TRUE)
    expect_match(plain, "node 3 of 6", fixed = TRUE)
    expect_match(
        paste(view$nodes, collapse = "\n"),
        "ORNL.*48.*24.*Amon=gn.*LImon=gr"
    )
    expect_match(paste(view$cases, collapse = "\n"), "ssp126")

    # Failed terminal state freezes elapsed time and preserves structured diagnosis
    failed_row <- data.table::data.table(
        run_id = "run-failed",
        status = "failed",
        current_stage = "resolve",
        spec_json = spec,
        started_at = now,
        updated_at = now + 90,
        completed_at = as.POSIXct(NA, tz = "UTC")
    )
    failure <- list(
        stage = "resolve",
        phase = "unit",
        unit_type = "index_node",
        current = 6L,
        total = 6L,
        summary = "No ESGF index node resolved a complete input set.",
        cause = "Future catalog coverage is incomplete.",
        nodes_checked = 6L,
        coverage_failures = 6L,
        closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn")
    )
    failed_events <- data.table::data.table(
        event_id = "failure",
        stage = "resolve",
        status = "failed",
        message = failure$summary,
        details_json = shift_persist__spec_json(failure),
        created_at = now + 90
    )
    failed_view <- shift_ui_view__ui_table_view(
        failed_row,
        data.table::data.table(),
        failed_events,
        width = 80L
    )
    failed_text <- ui_test__plain(failed_view$lines)
    expect_equal(failed_view$state$elapsed_seconds, 90)
    expect_identical(failed_view$state$failure_details$cause, failure$cause)
    expect_match(failed_text, "FAILED", fixed = TRUE)
    expect_match(failed_text, "6 checked.*6 incomplete")
    expect_match(failed_text, "BCC-CSM2-MR/r1i1p1f1/gn", fixed = TRUE)
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

test_that("download metrics and reporter bindings bridge progress and nested fallbacks", {
    callbacks <- new.env(parent = emptyenv())
    seen <- character()
    calls <- character()
    downloader <- list(
        tasks = function(session_id = NULL) {
            data.frame(
                status = c("done", "downloading", "queued"),
                bytes_done = c(1024, 512, 0),
                size = c(1024, 2048, 4096),
                speed_bps = c(NA, 256, NA),
                filename = c("done.nc", "tas.nc", "queued.nc")
            )
        },
        on = function(event, fun) {
            callbacks[[event]] <- fun
            event
        },
        off = function(token) TRUE
    )
    metrics <- shift_run__download_metrics(
        downloader,
        "session",
        variables = 8L
    )
    expect_equal(
        c(
            metrics$current,
            metrics$total,
            metrics$bytes_done,
            metrics$speed_bps
        ),
        c(1, 3, 1536, 256)
    )
    expect_equal(metrics$active_files, "tas.nc")
    expect_match(
        shift_run__download_label("future", metrics),
        "future download.*1/3 files.*1.5 KiB/7.0 KiB.*8 variables"
    )

    reporter <- list(
        ui = function() shift_ui("none"),
        unit_started = function(...) calls <<- c(calls, "started"),
        unit_updated = function(...) calls <<- c(calls, "updated"),
        unit_completed = function(...) calls <<- c(calls, "completed"),
        notice = function(...) invisible(NULL),
        heartbeat = function(message, ...) {
            seen <<- c(seen, message)
            invisible(TRUE)
        }
    )
    event <- list(
        session_id = "session",
        filename = "tas.nc",
        target_path = NA_character_,
        data_node = "example.org"
    )
    cleanup <- shift_run__download_reporter_bind(
        downloader,
        reporter,
        role = "future",
        variables = 8L
    )
    callbacks$task_progress(c(event, event = "task_progress"), downloader)
    cleanup()
    expect_match(
        seen,
        "future download.*1/3 files.*1.5 KiB/7.0 KiB.*8 variables"
    )

    nested_cleanup <- shift_run__download_reporter_bind(
        downloader,
        reporter,
        role = "HTTP fallback",
        variables = 1L,
        nested = TRUE
    )
    callbacks$session_start(c(event, event = "session_start"), downloader)
    callbacks$session_done(c(event, event = "session_done"), downloader)
    nested_cleanup()
    expect_equal(calls, c("updated", "updated"))
})

test_that("dynamic and log terminal receipts retain failures, completions, and debug lines", {
    mock <- shift_test__mock_renderer()

    # Dynamic failure receipt
    reporter <- shift_reporter__reporter(shift_ui("dynamic", motion = "none"))
    reporter$stage_started("resolve", "Resolving inputs.", 1L, 6L)
    failure <- list(
        kind = "resolver_exhausted",
        summary = "No ESGF index node resolved a complete CMIP6 input set.",
        nodes_checked = 6L,
        coverage_failures = 5L,
        timeout_failures = 1L,
        closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn")
    )
    expect_identical(
        ui_test__msg(reporter$run_failed(
            message = failure$summary,
            details = failure
        )),
        ""
    )
    fail_plain <- ui_test__plain(utils::tail(mock$frames, 1L)[[1L]])
    expect_identical(mock$commits, "failed")
    expect_equal(mock$closes, 0L)
    expect_match(fail_plain, "FAILED", fixed = TRUE)
    expect_match(fail_plain, "6 checked.*5 incomplete.*1 timeout")
    expect_match(fail_plain, "BCC-CSM2-MR/r1i1p1f1/gn", fixed = TRUE)

    # Dynamic completion receipt
    output_dir <- tempfile("shift-completed-output-")
    paths <- file.path(
        output_dir,
        c("BCC-CSM2-MR_ssp126_2060s.epw", "BCC-CSM2-MR_ssp585_2060s.epw")
    )
    cases_done <- data.table::data.table(status = c("completed", "completed"))
    run <- shift_test__run_stage(
        "run-complete",
        output_dir = output_dir,
        cases = cases_done
    )
    done_rep <- shift_reporter__reporter(
        shift_ui("dynamic", motion = "none"),
        run_id = "run-complete"
    )
    done_rep$stage_started("write_epw", "Writing final EPWs.")
    done_rep$cases_updated(cases_done)
    done_rep$unit_completed(
        "Exported final EPWs",
        2L,
        2L,
        details = list(unit_type = "epw_export")
    )
    expect_identical(
        ui_test__msg(done_rep$run_completed(
            run,
            data.table::data.table(export_path = paths)
        )),
        ""
    )
    done_plain <- ui_test__plain(utils::tail(mock$frames, 1L)[[1L]])
    expect_identical(mock$commits, c("failed", "done"))
    expect_match(
        done_plain,
        "Summary.*2/2 cases completed.*2 EPW files.*0 warnings"
    )
    expect_true(all(vapply(
        basename(paths),
        grepl,
        logical(1L),
        x = done_plain,
        fixed = TRUE
    )))

    # Log mode failure, completion, and untruncated debug lines
    fail_rep <- shift_reporter__reporter(shift_ui("log", detail = "normal"))
    fail_rep$cases_updated(data.table::data.table(
        experiment_id = "ssp585",
        period = "2060s",
        variant_label = "r1i1p1f1",
        status = "missing",
        missing_reason = "future/hurs, future/tas"
    ))
    expect_match(
        ui_test__msg(fail_rep$run_failed()),
        "future/hurs",
        fixed = TRUE
    )

    log_dir <- tempfile("shift-log-output-")
    log_rep <- shift_reporter__reporter(
        shift_ui("log", detail = "normal"),
        run_id = "run-log-complete"
    )
    log_out <- ui_test__msg(log_rep$run_completed(
        shift_test__run_stage("run-log-complete", output_dir = log_dir),
        data.table::data.table(export_path = file.path(log_dir, "a.epw"))
    ))
    expect_match(log_out, "1 output(s)", fixed = TRUE)
    expect_match(log_out, "Output directory", fixed = TRUE)

    withr::local_options(width = 20L)
    debug_rep <- shift_reporter__reporter(shift_ui("log", detail = "debug"))
    debug_rep$stage_started("resolve", "Resolving inputs.")
    long_msg <- strrep("complete-catalog-context ", 8L)
    expect_match(
        ui_test__msg(debug_rep$unit_started(long_msg)),
        long_msg,
        fixed = TRUE
    )
})

test_that("startup plan summaries, persisted context, and dynamic startup frames stay consistent", {
    plan <- ui_test__plan(table = c(snd = "LImon"))

    lines <- unname(shift_ui_view__ui_plan_summary(
        plan,
        "run-test",
        width = 80L
    ))
    expect_true(all(nchar(lines, type = "width") <= 80L))
    expect_match(lines[[1L]], "Future EPW.*run-test.*STARTING")
    expect_match(
        paste(lines, collapse = " "),
        "Enhanced epwshiftr.*no reference.*2 expected.*member r1i1p1f1.*grid gn.*tas=Amon"
    )

    detail_text <- paste(
        shift_ui_view__ui_plan_summary(
            plan,
            "run-test",
            width = 80L,
            detail = "detail"
        ),
        collapse = " "
    )
    expect_match(detail_text, "Options.*transition_hours=72.*snow_depth=auto")

    spec <- shift_persist__plan_spec(plan)
    spec$climate$table <- list(snd = "LImon")
    spec$climate$table_map <- NULL
    row <- data.table::data.table(
        spec_json = shift_persist__spec_json(spec),
        output_dir = tempfile("future-epw-")
    )
    context <- shift_ui_state__ui_plan_context_from_row(row, cases_total = 1L)
    expect_true("epwshiftr / no reference" %in% context$items)
    expect_match(
        context$selection,
        "member r1i1p1f1.*grid gn.*auto by variable.*snd=LImon"
    )

    # Dynamic startup draws a replaceable frame instead of printing transcript lines
    mock <- shift_test__mock_renderer()
    reporter <- shift_reporter__reporter(shift_ui("dynamic", motion = "none"))
    expect_identical(
        ui_test__msg(reporter$run_started(
            plan,
            "run_534d9b84b235029404b318bd"
        )),
        ""
    )
    expect_length(mock$frames, 1L)
    plain <- cli::ansi_strip(mock$frames[[1L]])
    expect_false(grepl("run 04b318bd", plain[[1L]], fixed = TRUE))
    expect_match(
        paste(plain, collapse = " "),
        "BCC-CSM2-MR.*ssp126.*Enhanced epwshiftr"
    )

    milestone_out <- ui_test__msg({
        reporter$stage_started("resolve", "Resolving inputs.", 1L, 6L)
        reporter$stage_completed("Resolved inputs.")
    })
    reporter$close()
    expect_identical(milestone_out, "")
    expect_equal(mock$closes, 1L)
})

test_that("shift_watch() and generic operation reporters preserve receipts across modes", {
    plan <- ui_test__plan()
    test_local_dependencies(list(shift_job__launch_job = function(...) {
        invisible(0L)
    }))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))
    on.exit(shift_cancel(run), add = TRUE)

    output <- ui_test__msg(
        watched <- shift_watch(run, follow = FALSE, ui = shift_ui("log"))
    )
    expect_s7_class(watched, ShiftRun)
    expect_match(output, "Future EPW", fixed = TRUE)
    expect_match(output, "Workflow", fixed = TRUE)
    expect_match(output, "Activity", fixed = TRUE)
    expect_false(grepl("queued | planned | cases", output, fixed = TRUE))

    log_out <- ui_test__msg({
        rep <- ShiftReporter$new(
            shift_ui("log"),
            run_id = "run-generic-log",
            step_id = "step-generic-log"
        )
        rep$operation_started(
            "collect",
            "Collect CMIP6",
            context = list(items = c("Collect CMIP6", "input request")),
            stage_sequence = c("collect", "extract")
        )
        rep$operation_waiting("12 files collected")
    })
    expect_match(log_out, "Collect CMIP6", fixed = TRUE)
    expect_match(log_out, "ready: 12 files collected", fixed = TRUE)

    expect_identical(
        ui_test__msg({
            rep <- ShiftReporter$new(
                shift_ui("none"),
                run_id = "run-generic-none"
            )
            rep$operation_started("extract", "Extract Climate")
            rep$operation_waiting("3 plans processed")
        }),
        ""
    )

    mock <- shift_test__mock_renderer()
    ops <- c(
        datasets = "operation_completed",
        collect = "operation_waiting",
        collect = "operation_partial",
        download = "operation_detached"
    )
    for (i in seq_along(ops)) {
        rep <- ShiftReporter$new(
            shift_ui("dynamic"),
            run_id = paste0("run-", ops[[i]])
        )
        rep$operation_started(names(ops)[[i]], paste("Stage", i))
        rep[[ops[[i]]]](paste("msg", i))
    }
    expect_identical(mock$commits, rep("done", 4L))
})

test_that("heartbeat details merge live state without unthrottled durable touches", {
    touches <- 0L
    testthat::local_mocked_bindings(
        shift_job__job_touch = function(...) {
            touches <<- touches + 1L
            invisible(NULL)
        },
        shift_job__job_check_cancel = function(...) invisible(FALSE),
        .package = "epwshiftr"
    )
    reporter <- shift_reporter__reporter(
        shift_ui("none", heartbeat = 10),
        store = list(),
        run_id = "run-test",
        job_id = "job-test"
    )
    reporter$heartbeat(
        details = list(
            unit_type = "catalog",
            catalog_role = "future",
            access_method = "HTTPServer",
            bytes_done = 1024
        )
    )
    reporter$heartbeat(details = list(bytes_done = 2048))

    state <- reporter$snapshot()
    expect_identical(state$current_details$catalog_role, "future")
    expect_identical(state$current_details$access_method, "HTTPServer")
    expect_equal(state$current_details$bytes_done, 2048)
    expect_equal(touches, 1L)
})

# Exercise the real discovery loop and Dataset stage while replacing remote
# catalogs with complete local CMIP6 records. This proves panel ownership at
# nested API boundaries instead of merely checking a fabricated final frame.
test_that("discovery owns one reporter across methods and nested catalog stages", {
    skip_on_cran()
    mock <- shift_test__mock_renderer(strip = TRUE, bind = FALSE)
    reporters <- list()
    withr::local_options(list(cli.width = 112L, cli.num_colors = 1L))
    testthat::local_mocked_bindings(
        shift_tui__ui_renderer = mock$factory,
        shift_resolve__with_query_reporter = function(
            reporter,
            query,
            phase,
            expr
        ) {
            force(expr)
        },
        shift_resolve__as_query = function(x) {
            list(
                index_node = function() "https://example.org",
                collect = function(...) list(count = function() 964L)
            )
        },
        shift_run__task_execute = function(...) {
            stop("unexpected standalone operation")
        },
        shift_resolve__cmip6_coverage_catalog = function(
            request,
            store,
            ui,
            label
        ) {
            reporter <- shift_run__current_reporter()
            reporters[[length(reporters) + 1L]] <<- reporter
            reporter$unit_started(
                "Querying File catalog",
                details = list(unit_type = "catalog", catalog_role = "File")
            )
            rows <- data.table::CJ(
                source_id = request@meta$source,
                experiment_id = request@meta$experiment,
                variable_id = request@meta$variables
            )
            rows[, `:=`(
                variant_label = "r1i1p1f1",
                grid_label = "gn",
                frequency = "mon",
                table_id = "Amon",
                latest = TRUE,
                datetime_start = "1900-01-01T00:00:00Z",
                datetime_end = "2100-12-31T23:59:59Z"
            )]
            reporter$unit_completed(
                sprintf("Indexed %d File catalog records", nrow(rows)),
                details = list(unit_type = "catalog", catalog_role = "File")
            )
            rows
        },
        .package = "epwshiftr"
    )
    test_local_dependencies(list(
        availability = function(...) {
            reporter <- shift_run__current_reporter()
            reporters[[length(reporters) + 1L]] <<- reporter
            shift_datasets(shift_spec__cmip6_scenario(
                source = "Model-A",
                scenario = "ssp585",
                variables = "tas"
            ))
            test_cmip6_availability(...)
        },
        shift_resolve__cmip6_period_coverage = test_dependency_originals$shift_resolve__cmip6_period_coverage
    ))
    transforms <- shift_batch__transforms(c("original_morphing", "bws_btws"))
    transforms$epwshiftr <- monthly_transform("epwshiftr")
    reference <- shift_reference__historical(data.frame(
        period = "reference",
        year = 1973:2005
    ))
    result <- shift_batch_ui__discover_models(
        shift_cmip6(
            model = 3L,
            scenarios = c("ssp126", "ssp245", "ssp370", "ssp585")
        ),
        transforms,
        periods = shift_spec__periods_from_input(list(
            mid = 2041:2060,
            late = 2071:2090
        )),
        references = lapply(transforms, function(x) {
            list(reference = reference)
        }),
        store = tempfile(),
        ui = shift_ui("dynamic"),
        site = "San Francisco"
    )
    expect_equal(nrow(result$identities), 3L)
    expect_equal(mock$opened, 1L)
    expect_equal(length(mock$commits), 1L)
    expect_true(all(vapply(reporters, identical, logical(1L), reporters[[1L]])))
    expect_null(shift_run__current_reporter())
    text <- paste(unlist(mock$frames), collapse = "\n")
    expect_match(text, "San Francisco")
    expect_match(text, "3 GCMs.*4 scenarios.*3 methods")
    expect_match(text, "Future coverage")
    expect_match(text, "Historical coverage")
    expect_match(text, "1973.*2005")
    expect_match(text, "combination 2/2")
    expect_match(text, "specific humidity.*huss.*ps")
    expect_false(grepl("READY|input request|Collect CMIP6|%", text))
    final <- paste(utils::tail(mock$frames, 1L)[[1L]], collapse = "\n")
    expect_match(final, "COMPLETED")
    expect_match(final, "Model-A.*Model-B.*Model-C")
    expect_false(grepl("Now|Cancel|Current search", final))
})

# Verify the nested File path with a real store as well: discovery bypasses the
# standalone task wrapper but must still persist catalogs and close its handle.
test_that("nested coverage collection persists files and closes its owned store", {
    calls <- shift_test__mock_collect(esgf_test__file_docs("tas_day.nc"))
    testthat::local_mocked_bindings(
        shift_run__task_execute = function(...) {
            stop("unexpected standalone operation")
        },
        .package = "epwshiftr"
    )
    request <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    root <- tempfile("discovery-owned-store-")
    reporter <- shift_reporter__reporter(shift_ui("none"))
    on.exit(reporter$close(), add = TRUE)
    catalog <- shift_run__with_reporter(
        reporter,
        shift_resolve__cmip6_coverage_catalog(
            request,
            root,
            shift_ui("none"),
            "future-coverage"
        )
    )
    expect_equal(nrow(catalog), 1L)
    expect_identical(calls$types, c("Dataset", "File"))
    expect_equal(catalog$variable_id, "tas")
    # DuckDB rejects a differently configured connection while a writable
    # handle to the same database is still open in this process.
    connection <- ddb_connect(
        file.path(root, "manifest.duckdb"),
        read_only = TRUE
    )
    expect_s4_class(connection, "duckdb_connection")
    ddb_disconnect(connection, shutdown = TRUE)
})

# Error and interrupt paths must release the shared stack and produce exactly
# one terminal receipt, even if a node fails before any catalog is returned.
test_that("discovery restores reporter ownership after failures and interrupts", {
    for (interrupted in c(FALSE, TRUE)) {
        reporter <- NULL
        test_local_dependencies(list(availability = function(...) {
            reporter <<- shift_run__current_reporter()
            expect_null(reporter$context()$request_started_at)
            reporter$heartbeat(
                "Waiting for catalog response",
                details = list(
                    request_started_at = as.numeric(Sys.time()),
                    records_received = 12L
                )
            )
            if (interrupted) {
                stop(structure(
                    list(message = "interrupted", call = NULL),
                    class = c("interrupt", "condition")
                ))
            }
            stop("catalog unavailable")
        }))
        transforms <- shift_batch__transforms("original_morphing")
        condition <- tryCatch(
            shift_batch_ui__discover_models(
                shift_cmip6(model = 1L, scenarios = "ssp585"),
                transforms,
                shift_spec__periods_from_input(list(mid = 2041:2060)),
                NULL,
                tempfile(),
                shift_ui("none")
            ),
            error = function(e) e,
            interrupt = function(e) e
        )
        expect_null(shift_run__current_reporter())
        expect_false(is.null(reporter), info = conditionMessage(condition))
        expect_identical(
            reporter$snapshot()$status,
            if (interrupted) "cancelled" else "failed"
        )
    }
})

# Use callback events from two real logical requests to distinguish request
# time, previous responses, received rows, and cache reuse without estimating
# overall completion from a method or node ordinal.
test_that("catalog callbacks retain truthful request and cache metrics", {
    withr::local_options(epwshiftr.query.timeout = 300)
    reporter <- shift_reporter__reporter(shift_ui("none"))
    on.exit(reporter$close(), add = TRUE)
    reporter$operation_started("collect", "Collect CMIP6")
    query <- EsgQuery$new(index_node = "https://example.org")
    shift_resolve__with_query_reporter(reporter, query, "File", {
        callback <- priv(query)$progress_callback
        callback(list(state = "started"))
        callback(list(state = "completed", downloaded = 128L))
        callback(list(state = "parsed", records = 12L))
        callback(list(state = "cached", records = 3L))
        callback(list(state = "started"))
        callback(list(state = "transfer", download = c(1000, 20)))
    })
    state <- reporter$snapshot()
    expect_equal(state$current_details$bytes_done, 20)
    expect_equal(state$current_details$responses, 1L)
    expect_equal(state$current_details$cache_hits, 1L)
    expect_equal(state$current_details$records_received, 15L)
    state$now_seconds <- state$current_details$request_started_at + 40
    text <- paste(shift_ui_view__ui_query_lines(state, 112L), collapse = " ")
    expect_match(text, "waiting 40s")
    expect_match(text, "timeout 5m 00s")
    expect_match(text, "last response")
    expect_match(
        text,
        "1 responses.*1 cached.*15 File catalog records received"
    )
    expect_false(grepl("%|ETA|downloaded", text))
})

# Drive the actual JSON reader twice so cache hits cannot silently bypass the
# progress callback or get counted as a second network response.
test_that("JSON catalog reads report parsed rows and cache hits separately", {
    cache <- DiskCache$new(tempfile("discovery-cache-"), prune_on_init = FALSE)
    testthat::local_mocked_bindings(
        cache__get = function(...) cache,
        .package = "epwshiftr"
    )
    testthat::local_mocked_bindings(
        curl_fetch_memory = function(...) {
            list(
                content = charToRaw(
                    '{"response":{"numFound":2,"docs":[{"id":"a"},{"id":"b"}]}}'
                ),
                status_code = 200L
            )
        },
        .package = "curl"
    )
    events <- list()
    callback <- function(event) events[[length(events) + 1L]] <<- event
    online <- cache__read_json(
        "https://example.org/catalog",
        cache = TRUE,
        progress_callback = callback
    )
    cached <- cache__read_json(
        "https://example.org/catalog",
        cache = TRUE,
        progress_callback = callback
    )
    expect_equal(online$response, cached$response)
    expect_identical(
        vapply(events, `[[`, character(1L), "state"),
        c("started", "completed", "parsed", "cached")
    )
    expect_equal(events[[3L]]$records, 2L)
    expect_equal(events[[4L]]$records, 2L)
})

test_that("calibration, started cases, and multi-year exports preserve honest units and counts", {
    state <- shift_test__ui_state(
        stage = "morph",
        unit_label = "ERA5 t2m",
        current_details = list(
            unit_type = "reanalysis_variable",
            phase = "unit",
            current = 2L,
            total = 6L
        )
    )
    line <- cli::ansi_strip(shift_ui_view__ui_metric_line(state, 112L))
    expect_match(line, "Variables.*1/6")
    expect_false(grepl("Cases", line))
    expect_match(
        ui_test__plain(shift_ui_view__ui_status_lines(state), " "),
        "Calibration.*ERA5"
    )
    expect_match(
        shift_ui_view__ui_compact_line(state, width = 112L),
        "Calibration.*1/6 variables"
    )

    state$current_details <- list(
        unit_type = "morph_case",
        phase = "unit",
        current = 1L,
        total = 1L
    )
    expect_match(shift_ui_view__ui_metric_line(state), "0/1")
    state$current_details$outcome <- "completed"
    expect_match(shift_ui_view__ui_metric_line(state), "1/1")

    state$stage <- "write_epw"
    state$cases_total <- 1L
    state$outputs_completed <- 2L
    state$current_details <- list(
        unit_type = "epw_export",
        current = 2L,
        total = 20L
    )
    line <- cli::ansi_strip(shift_ui_view__ui_metric_line(state, 112L))
    expect_match(line, "2/20.*exported 2 files")
    expect_false(grepl("exported 2/1", line, fixed = TRUE))

    state$stage <- "resolve"
    state$current_details <- list(phase = "unit", current = 1L, total = 3L)
    expect_match(shift_ui_view__ui_metric_line(state), "node 1 of 3")

    # Case completion preserves measured multi-year export counts in memory and persisted state
    reporter <- shift_reporter__reporter(shift_ui("none"))
    on.exit(reporter$close(), add = TRUE)
    reporter$stage_started("write_epw", "Exporting weather")
    reporter$unit_started(
        "Last file",
        20L,
        20L,
        details = list(unit_type = "epw_export")
    )
    reporter$unit_completed("Exported last file", 20L, 20L)
    reporter$cases_updated(data.table::data.table(status = "completed"))
    expect_equal(reporter$snapshot()$outputs_completed, 20L)

    stamp <- as.POSIXct("2026-09-16 00:00:00", tz = "UTC")
    row <- data.table::data.table(
        run_id = "multi-year",
        status = "completed",
        current_stage = "write_epw",
        started_at = stamp,
        completed_at = stamp + 1
    )
    events <- data.table::data.table(
        stage = "write_epw",
        status = "completed",
        message = "Exported last file",
        created_at = stamp + 1,
        details_json = shift_persist__spec_json(list(
            stage = "write_epw",
            phase = "unit",
            unit_type = "epw_export",
            current = 20L,
            total = 20L,
            outcome = "completed"
        ))
    )
    persisted <- shift_ui_state__ui_table_state(
        row,
        events,
        data.table::data.table(status = "completed")
    )
    expect_equal(persisted$outputs_completed, 20L)
})

test_that("batch viewports cap height, prioritize failures, and outrank warnings with actions", {
    snapshot <- ui_workflows__states()[[1L]]
    ch <- data.table::copy(snapshot$children[rep(seq_len(4L), 8L)])
    n <- nrow(ch)
    ch$child_key <- paste0("child", seq_len(n))
    ch$model <- paste0("Model-", seq_len(n))
    ch$status <- "completed"
    ch$status[31:32] <- c("running", "failed")
    ch$current_stage[31L] <- "download"
    snapshot$children <- ch
    data.table::set(
        snapshot$batch,
        j = c("status", "children", "completed", "active", "failed"),
        value = list("failed", 32L, 30L, 1L, 1L)
    )
    snapshot$activity <- list(
        child31 = list(
            stage = "download",
            unit_label = "Downloading tas",
            elapsed_seconds = 35,
            updated_at = "2026-09-16 00:00:01",
            current_details = list(
                unit_type = "download_session",
                bytes_done = 2048,
                bytes_total = 4096,
                speed_bps = 1024,
                current = 1L,
                total = 2L
            )
        )
    )
    compact_view <- shift_batch_ui__view(snapshot, width = 80L, height = 23L)
    expect_lte(length(compact_view$lines), 23L)
    expect_true(all(cli::ansi_nchar(compact_view$lines, type = "width") <= 79L))
    text <- ui_test__plain(compact_view$lines, " ")
    expect_match(text, "Model-32.*Model-31|Model-31.*Model-32")
    expect_match(text, "Downloading tas")
    expect_match(text, "hidden")
    expect_lte(
        length(shift_batch_ui__view(snapshot, width = 60L, height = 10L)$lines),
        10L
    )

    full <- ui_test__plain(
        shift_batch_ui__view(snapshot, width = 112L, detail = "detail")$lines,
        " "
    )
    expect_match(full, "Model-1.*Model-32")
    expect_match(full, "2026-09-16 00:00:01", fixed = TRUE)
    expect_false(grepl("children hidden", full, fixed = TRUE))

    # Errors outrank warnings and retain recovery actions
    diag_snap <- ui_workflows__states()[[3L]]
    diag_snap$batch$status <- "failed"
    diag_snap$diagnostics <- data.table::data.table(
        method = "qdm",
        model = "Model-B",
        severity = c(rep("warning", 3L), "error"),
        message = c(paste("Routine", 1:3), "Missing calibration file"),
        action = c(rep(NA_character_, 3L), "Restore calibration and resume")
    )
    view <- ui_test__plain(shift_batch_ui__view(diag_snap, width = 112L)$lines)
    expect_match(view, "Failure.*Missing calibration file")
    expect_match(view, "Action.*Restore calibration and resume")
    expect_match(view, "1 more diagnostics")
    expect_lt(
        regexpr("Missing calibration", view)[[1L]],
        regexpr("Routine", view)[[1L]]
    )
    full_diag <- ui_test__plain(
        shift_batch_ui__view(diag_snap, detail = "detail")$lines,
        " "
    )
    expect_match(full_diag, "Routine 3", fixed = TRUE)
    expect_false(grepl("more diagnostics", full_diag, fixed = TRUE))
})

test_that("frame renderers cap live viewport height and retain full completion receipts", {
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    writes <- character()
    mk <- function() {
        writes <<- character()
        ShiftFrameRenderer$new(
            output,
            backend = "frame",
            writer = function(text) writes <<- c(writes, text)
        )
    }

    withr::local_options(list(epwshiftr.ui_height = 8L, cli.width = 80L))
    r1 <- mk()
    r1$draw(paste("row", 1:30), compact = "safe compact status")
    r1$suspend(function() writes <<- c(writes, "diagnostic\n"))
    expect_match(utils::tail(writes, 1L), "safe compact status", fixed = TRUE)
    expect_false(grepl("row 30", utils::tail(writes, 1L), fixed = TRUE))
    r1$commit()
    expect_match(utils::tail(writes, 1L), "row 30\n", fixed = TRUE)
    expect_false(r1$active())

    withr::local_options(list(epwshiftr.ui_height = 24L, cli.width = 80L))
    r2 <- mk()
    testthat::local_mocked_bindings(shift_tui__ui_renderer = function(...) r2)
    rep <- shift_reporter__reporter(
        shift_ui("dynamic", detail = "detail", motion = "none"),
        run_id = "multi-year-receipt"
    )
    rep$operation_started("write_epw", "Future EPW")
    paths <- file.path(
        "/exports",
        paste0("Weather_Model-A_ssp585_", 2041:2060, ".epw")
    )
    run <- shift_test__run_stage(
        "multi-year-receipt",
        output_dir = "/exports",
        task = "future_epw",
        cases = data.table::data.table(status = "completed")
    )
    rep$run_completed(run, data.table::data.table(export_path = paths))
    receipt <- cli::ansi_strip(utils::tail(writes, 1L))
    expect_match(receipt, "/exports", fixed = TRUE)
    expect_true(all(vapply(
        basename(paths),
        grepl,
        logical(1L),
        x = receipt,
        fixed = TRUE
    )))
    expect_false(r2$active())
})

test_that("R and CLI batch watch leave a complete final receipt and handle fallback/interrupt", {
    states <- ui_workflows__states()
    run_batch_watch <- function(
        follow = TRUE,
        draw_ok = TRUE,
        interrupt = FALSE
    ) {
        index <- 0L
        mock <- shift_test__mock_renderer(draw_ok = draw_ok, bind = FALSE)
        text <- ui_test__msg(
            testthat::with_mocked_bindings(
                invisible(shift_batch_ui__watch(
                    NULL,
                    follow,
                    interval = 0.1,
                    events = 10L,
                    ui = shift_ui("dynamic", motion = "none")
                )),
                shift_tui__ui_renderer = mock$factory,
                shift_batch__refresh = function(x) x,
                shift_job__watch_sleep = function(...) {
                    if (interrupt) {
                        stop(structure(
                            list(message = "interrupt", call = NULL),
                            class = c("interrupt", "condition")
                        ))
                    }
                    NULL
                },
                shift_job__watch_now = function() {
                    as.POSIXct(index * 10, origin = "1970-01-01")
                },
                shift_batch_ui__snapshot = function(...) {
                    index <<- index + 1L
                    if (follow && index == 1L) states[[1L]] else states[[3L]]
                },
                .package = "epwshiftr"
            ),
            " "
        )
        list(text = text, mock = mock)
    }

    for (follow in c(FALSE, TRUE)) {
        res <- run_batch_watch(follow = follow)
        expect_match(res$text, "Future EPW Batch.*8 EPW files")
        expect_identical("draw" %in% res$mock$calls, follow)
        expect_identical(utils::tail(res$mock$calls, 1L), "close")
    }
    fallback <- run_batch_watch(draw_ok = FALSE)
    expect_equal(fallback$mock$closes, 1L)
    expect_match(fallback$text, "8 EPW files", fixed = TRUE)

    interrupted <- run_batch_watch(interrupt = TRUE)
    expect_equal(interrupted$mock$closes, 1L)
    expect_match(
        interrupted$text,
        "Stopped watching; batch children continue",
        fixed = TRUE
    )

    mock_cli <- shift_test__mock_renderer()
    testthat::local_mocked_bindings(
        shift_batch_get = function(...) NULL,
        shift_batch_ui__snapshot = function(...) states[[1L]]
    )
    cli_text <- ui_test__msg(
        invisible(epwshiftr_cli_shift_watch_follow(
            NULL,
            "batch-test",
            batch_id = "batch-test",
            progress = "dynamic",
            count = 1L
        )),
        " "
    )
    expect_match(cli_text, "Future EPW Batch")
    expect_identical(utils::tail(mock_cli$calls, 1L), "close")
})

test_that("download resume, discovery context, and config templates respect output policy", {
    flags <- epwshiftr_cli_parse_command(character(), flags = "--network")
    expect_identical(
        epwshiftr_cli_task_ui(flags, quiet = TRUE)@progress,
        "none"
    )

    seen <- logical()
    downloader <- list(resume = function(...) {
        seen <<- c(seen, list(...)$progress)
        data.table::data.table()
    })
    testthat::local_mocked_bindings(epwshiftr_cli_downloader = function(...) {
        downloader
    })
    for (mode in c("quiet", "json", "jsonl")) {
        args <- list(
            store = list(sync_downloads = function(...) NULL),
            command = "resume",
            args = c("--session", "test")
        )
        args[[mode]] <- TRUE
        do.call(epwshiftr_cli_download, args)
    }
    expect_identical(seen, rep(FALSE, 3L))

    ui <- shift_ui("log")
    ui@batch_context <- list(
        kind = "discovery",
        current = 2L,
        total = 3L,
        message = "method 2/3: qdm \u00b7 CEDA \u00b7 tas, hurs"
    )
    reporter <- shift_reporter__reporter(ui)
    on.exit(reporter$close(), add = TRUE)
    lines <- capture.output(
        {
            reporter$operation_started("collect", "Collect CMIP6")
            reporter$unit_started("Collecting Dataset catalog", 1L, 2L)
            reporter$heartbeat(force = TRUE)
            reporter$heartbeat(force = TRUE)
        },
        type = "message"
    )
    expect_match(
        ui_test__plain(lines),
        "Discovery.*method 2/3: qdm.*CEDA.*tas, hurs"
    )
    expect_equal(sum(grepl("method 2/3", lines, fixed = TRUE)), 1L)
    expect_match(
        cli::ansi_strip(shift_ui_view__ui_compact_line(
            reporter$snapshot(),
            width = 80L
        )),
        "method 2/3: qdm.*CEDA.*tas, hurs"
    )

    ui@progress <- "none"
    quiet <- shift_reporter__reporter(ui)
    on.exit(quiet$close(), add = TRUE)
    expect_identical(
        ui_test__msg(quiet$operation_started("collect", "Collect CMIP6"), " "),
        ""
    )

    testthat::local_mocked_bindings(store_dir = function(...) {
        stop("Unexpected store init")
    })
    out <- epwshiftr_cli(c(
        "--quiet",
        "shift",
        "config",
        "example",
        "--methods",
        "original_morphing"
    ))
    expect_equal(out$status, 0L, info = out$error)
})

test_that("weather comparison excludes missing codes and weights valid hourly rows", {
    original <- get_cache_epw()
    baseline <- epw_file_read(original)$data()
    paths <- vapply(
        c(2L, 3L),
        function(n) {
            path <- tempfile(fileext = ".epw")
            writeLines(readLines(original, n = 8L), path)
            values <- data.table::copy(baseline[seq_len(n)])
            data.table::set(
                values,
                j = "dry_bulb_temperature",
                value = if (n == 2L) c(10, 99.9) else 20
            )
            data.table::set(
                values,
                j = "relative_humidity",
                value = if (n == 2L) c(50, 999) else 80
            )
            data.table::fwrite(
                values[, EPW_FILE_COLUMNS, with = FALSE],
                path,
                append = TRUE,
                col.names = FALSE,
                quote = FALSE,
                na = ""
            )
            path
        },
        character(1L)
    )
    withr::defer(unlink(paths))
    result <- shift_inspect__weather(c(paths, tempfile("missing-")))
    expect_equal(
        c(
            result$weather_hours,
            result$temperature_hours,
            result$humidity_hours,
            result$unreadable_files
        ),
        c(5L, 4L, 4L, 1L)
    )
    expect_equal(
        c(result$mean_temperature_c, result$mean_relative_humidity_pct),
        c(17.5, 72.5)
    )
    expect_true(nzchar(result$weather_error))
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
    text <- ui_test__plain(
        shift_batch_ui__view(snapshot, width = 112L)$lines,
        " "
    )
    expect_match(text, "1 cancelled")
    expect_match(text, "ERA5 t2m")
    expect_match(text, "Variables.*2/6")
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

test_that("cancellation overrides stale worker frames in R and CLI watch", {
    root <- tempfile("ui-cancel-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    stamp <- as.POSIXct("2026-09-18 01:23:45", tz = "UTC")
    id <- "cancel-test"
    payload <- list(
        run_id = id,
        run = data.table::data.table(
            run_id = id,
            task = "future_epw",
            status = "running",
            current_stage = "morph",
            spec_json = "{}",
            started_at = stamp,
            updated_at = stamp + 10,
            completed_at = as.POSIXct(NA),
            last_error = NA_character_
        ),
        cases = data.table::data.table(),
        events = data.table::data.table(
            event_id = "first",
            stage = "morph",
            status = "running",
            created_at = stamp + 10,
            message = "Morphing",
            details_json = "{}"
        ),
        jobs = data.table::data.table(
            job_id = "cancel-job",
            status = "running"
        ),
        ui_state = list(
            run_id = id,
            task_label = "Future EPW",
            status = "running",
            stage = "morph",
            unit_label = "ssp585",
            elapsed_seconds = 10,
            current_details = list(
                unit_type = "morph_case",
                current = 1L,
                total = 2L
            )
        )
    )
    path <- shift_job__live_path(root, id)
    dir.create(dirname(path), recursive = TRUE)
    store_write_json_atomic(
        payload,
        path,
        auto_unbox = TRUE,
        dataframe = "rows",
        null = "null",
        na = "null",
        POSIXt = "ISO8601"
    )
    run <- shift_job__live_cancel_mark(root, id, "cancel-job", "stopping")
    expect_identical(
        c(run@meta$run$status, run@meta$ui_state$status, run@meta$jobs$status),
        rep("stopping", 3L)
    )
    expect_false(is.na(run@meta$jobs$cancel_requested_at))

    # Older sidecars may still carry a pre-cancellation frame.
    run@meta$ui_state$status <- "running"
    testthat::local_mocked_bindings(
        shift_outputs = function(...) data.table::data.table(),
        shift_job__watch_now = function() stamp + 60
    )
    view <- shift_ui_view__ui_run_view(run)
    expect_identical(view$state$status, "stopping")
    expect_equal(view$state$elapsed_seconds, 60)
    expect_match(ui_test__plain(view$lines, " "), "STOPPING")
    expect_match(cli::ansi_strip(view$compact), "STOPPING")

    snapshot <- list(
        run = run@meta$run,
        cases = run@meta$cases,
        events = run@meta$events,
        outputs = data.table::data.table(),
        diagnostics = shift_stage__diagnostics_empty()
    )
    attr(snapshot, "shift_ui_state") <- run@meta$ui_state
    mock <- shift_test__mock_renderer()
    testthat::local_mocked_bindings(
        epwshiftr_cli_shift_watch_snapshot = function(...) snapshot
    )
    ui_test__msg(
        invisible(epwshiftr_cli_shift_watch_follow(
            NULL,
            id,
            progress = "dynamic",
            count = 1L
        )),
        " "
    )
    frames <- c(unlist(mock$frames), mock$compacts)
    expect_match(ui_test__plain(frames, " "), "STOPPING")
    expect_false(any(grepl("RUNNING", frames, fixed = TRUE)))

    snapshot$run$status <- "cancelled"
    snapshot$run$completed_at <- stamp + 30
    terminal <- shift_ui_view__ui_table_view(
        snapshot$run,
        snapshot$cases,
        snapshot$events,
        ui_state = attr(snapshot, "shift_ui_state")
    )
    expect_identical(terminal$state$status, "cancelled")
    expect_equal(terminal$state$elapsed_seconds, 30)
})

# Interleave two child histories with a failure that becomes visible late.
test_that("batch event cursors retain interleaved events and watchers emit them once", {
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

    # R and CLI batch watchers emit late and terminal events exactly once
    states <- ui_workflows__states()
    ev_slices <- list(events[c(1L, 3L)], events[1:3], events[1:3], events)
    snapshots <- lapply(seq_len(4L), function(i) {
        s <- states[[if (i == 4L) 3L else 1L]]
        s$events <- events[0]
        attr(s, "shift_ui_events") <- ev_slices[[i]]
        s
    })
    index <- 0L
    testthat::local_mocked_bindings(
        shift_batch_get = function(...) NULL,
        shift_batch__refresh = function(x) x,
        shift_job__watch_sleep = function(...) NULL,
        shift_job__watch_now = function() {
            as.POSIXct(index * 10, origin = "1970-01-01")
        },
        shift_batch_ui__snapshot = function(...) {
            index <<- index + 1L
            snapshots[[index]]
        }
    )
    output <- capture.output(invisible(epwshiftr_cli_shift_watch_follow(
        NULL,
        "batch-test",
        batch_id = "batch-test",
        jsonl = TRUE,
        progress = "none",
        interval = 0,
        event_count = 0L
    )))
    records <- lapply(output, jsonlite::fromJSON)
    expect_identical(
        vapply(records, `[[`, character(1L), "type"),
        c("snapshot", "event", "event", "event", "terminal")
    )
    expect_identical(
        vapply(records[2:4], function(x) x$event$event_id, character(1L)),
        c("a2", "a3", "a4")
    )
    for (interface in c("R", "CLI")) {
        index <- 0L
        out <- capture.output(
            if (interface == "R") {
                invisible(shift_batch_ui__watch(
                    NULL,
                    TRUE,
                    0.1,
                    0L,
                    shift_ui("log")
                ))
            } else {
                invisible(epwshiftr_cli_shift_watch_follow(
                    NULL,
                    "batch-test",
                    batch_id = "batch-test",
                    progress = "log",
                    interval = 0,
                    event_count = 0L
                ))
            },
            type = "message"
        )
        for (msg in events$message[c(2L, 4L, 5L)]) {
            expect_equal(
                sum(grepl(msg, out, fixed = TRUE)),
                1L,
                info = interface
            )
        }
    }
})

test_that("single-run show detail retains all rows and long fields without changing JSON", {
    withr::local_options(list(cli.width = 60L, cli.num_colors = 1L))
    long_path <- paste0(
        "/weather/",
        strrep("reference-model/", 8L),
        "25_weather.epw"
    )
    action <- paste0(
        "Recover ",
        strrep("missing historical input ", 8L),
        "ACTION_END"
    )
    raw <- '{"raw_debug_payload":"RAW_PAYLOAD_END"}'
    run <- shift_test__run_stage(
        "show-detail",
        spec_json = raw,
        events = data.table::data.table(
            stage = "morph",
            status = "completed",
            message = paste0("EVENT_", seq_len(25L), "_END"),
            created_at = Sys.time(),
            details_json = raw
        )
    )
    outputs <- data.table::data.table(
        path = c(paste0("/outputs/", seq_len(24L), ".epw"), long_path)
    )
    testthat::local_mocked_bindings(
        cli_shift__target = function(...) run,
        shift_cases = function(...) data.table::data.table(),
        shift_outputs = function(...) outputs,
        shift_diagnostics = function(...) {
            data.table::data.table(
                stage = "morph",
                severity = "error",
                action = action
            )
        },
        shift_explain = function(...) {
            data.table::data.table(stage = "morph", status = "ready")
        }
    )
    snapshots <- lapply(
        list(character(), "--verbose", "--debug"),
        function(flag) {
            epwshiftr_cli_shift_show(NULL, c("--run", "show-detail", flag))
        }
    )
    output <- lapply(snapshots, function(v) {
        ui_test__msg(epwshiftr_cli_render_shift_show(v), "")
    })
    expect_identical(
        vapply(snapshots, attr, character(1L), "shift_ui_detail"),
        c("normal", "detail", "debug")
    )
    expect_false(grepl("EVENT_25_END", output[[1L]], fixed = TRUE))
    for (index in 2:3) {
        text <- gsub("[[:space:]]", "", output[[index]])
        expect_match(text, long_path, fixed = TRUE)
        expect_match(text, gsub("[[:space:]]", "", action), fixed = TRUE)
        expect_match(text, "EVENT_25_END", fixed = TRUE)
    }
    expect_false(grepl("RAW_PAYLOAD_END", output[[2L]], fixed = TRUE))
    expect_match(output[[3L]], "RAW_PAYLOAD_END", fixed = TRUE)
    json <- lapply(
        snapshots,
        jsonlite::toJSON,
        dataframe = "rows",
        POSIXt = "ISO8601"
    )
    expect_identical(json[[1L]], json[[2L]])
    expect_identical(json[[1L]], json[[3L]])
})

# vim: fdm=marker :
