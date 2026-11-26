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

ui_test__plain <- function(x) paste(cli::ansi_strip(x), collapse = "\n")
ui_test__msg <- function(expr) {
    ui_test__plain(capture.output(expr, type = "message"))
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

# vim: fdm=marker :
