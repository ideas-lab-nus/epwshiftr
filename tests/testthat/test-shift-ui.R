# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

shift_ui_test__mock_renderer <- function(frames, commits, closes) {
    function(...) {
        list(
            draw = function(lines, compact = NULL) {
                frames$value[[length(frames$value) + 1L]] <- lines
                TRUE
            },
            suspend = function(code) code(),
            backend = function() "frame",
            commit = function(result = c("done", "failed", "cancelled")) {
                result <- match.arg(result)
                commits$value <- c(commits$value, result)
                invisible(NULL)
            },
            close = function(...) {
                closes$value <- closes$value + 1L
                invisible(NULL)
            }
        )
    }
}

test_that("shared status views adapt to stage, width, and transfer metrics", {
    resolve_state <- list(
        run_id = "run-panel",
        status = "running",
        stage = "resolve",
        stage_sequence = c(
            "resolve",
            "extract_future",
            "extract_reference",
            "coverage",
            "morph",
            "write_epw"
        ),
        completed_stages = character(),
        stage_current = 1L,
        stage_total = 6L,
        stage_message = "Resolving complete CMIP6 workflow inputs.",
        unit_label = paste(
            rep("DKRZ future catalog waiting", 8L),
            collapse = " "
        ),
        unit_current = 1L,
        unit_total = 6L,
        plan_context = list(
            line = "BCC-CSM2-MR \u00b7 ssp126 + ssp585 \u00b7 2060s \u00b7 original_morphing",
            selection = "member auto \u00b7 grid auto"
        ),
        cases_ready = 0L,
        cases_total = 2L,
        outputs_completed = 0L,
        recent_events = c("Catalog retry", "Previous request completed"),
        recent_outcomes = c("fallback", "completed"),
        elapsed_seconds = 12
    )
    wide <- unname(shift_ui_view__ui_status_lines(resolve_state, width = 80L))
    wide_plain <- cli::ansi_strip(wide)
    expect_gte(length(wide), 11L)
    expect_true(all(cli::ansi_nchar(wide, type = "width") == 79L))
    expect_match(wide_plain[[1L]], "^╭─ Future EPW.*RUNNING")
    expect_true(any(grepl("Plan.*BCC-CSM2-MR.*original_morphing", wide_plain)))
    expect_true(any(grepl("Flow.*Resolve.*Future", wide_plain)))
    expect_true(any(grepl("Status.*node 1 of 6.*member auto", wide_plain)))
    expect_false(any(grepl("%", wide_plain, fixed = TRUE)))
    expect_match(utils::tail(wide_plain, 1L), "^╰─")

    narrow_plain <- cli::ansi_strip(shift_ui_view__ui_status_lines(
        resolve_state,
        width = 59L
    ))
    expect_false(any(grepl("^[╭├╰│]", narrow_plain)))
    expect_match(narrow_plain[[1L]], "Future EPW", fixed = TRUE)

    # Download transfer metrics
    dl_state <- list(
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
    dl_plain <- cli::ansi_strip(shift_ui_view__ui_status_lines(
        dl_state,
        width = 100L
    ))
    expect_true(any(grepl("2/8", dl_plain, fixed = TRUE)))
    expect_true(any(grepl("1.0 MiB/4.0 MiB", dl_plain, fixed = TRUE)))
    expect_true(any(grepl("512.0 KiB/s", dl_plain, fixed = TRUE)))
    expect_true(any(grepl("ETA 6s", dl_plain, fixed = TRUE)))
    expect_true(any(grepl("2 active", dl_plain, fixed = TRUE)))

    # Coverage, morph, and write_epw stage-specific metrics
    base <- list(
        status = "running",
        stage_message = "Working",
        unit_label = "case",
        unit_current = 1L,
        unit_total = 2L,
        current_details = list(current = 1L, total = 2L),
        cases_ready = 1L,
        cases_total = 2L,
        outputs_completed = 0L,
        last_event = "Resolved",
        elapsed_seconds = 1
    )
    coverage <- cli::ansi_strip(shift_ui_view__ui_status_lines(
        utils::modifyList(base, list(stage = "coverage")),
        width = 80L
    ))
    morph <- cli::ansi_strip(shift_ui_view__ui_status_lines(
        utils::modifyList(base, list(stage = "morph")),
        width = 80L
    ))
    epw <- cli::ansi_strip(shift_ui_view__ui_status_lines(
        utils::modifyList(
            base,
            list(stage = "write_epw", outputs_completed = 1L)
        ),
        width = 80L
    ))
    expect_true(any(grepl("Cases.*1/2.*ready 1.*missing 1", coverage)))
    expect_true(any(grepl("Cases.*1/2", morph)))
    expect_true(any(grepl("EPWs.*1/2.*exported 1 files", epw)))
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
        paste0("\033[2A\rONE\033[K\n\rTWO\033[K\r", "\n\r\033[K\033[1A\r")
    )

    renderer$suspend(function() {
        writes <<- c(writes, "notice\n")
        renderer$suspend(function() writes <<- c(writes, "detail\n"))
    })
    expect_true(any(writes == "notice\n"))
    expect_true(any(writes == "detail\n"))

    renderer$commit("failed")
    renderer$commit("failed")
    expect_identical(utils::tail(writes, 1L), "\n")
    expect_equal(hidden, 1L)
    expect_equal(shown, 1L)
    expect_false(renderer$active())
})

test_that("compact renderer, backend detection, and auto UI mode follow terminal capability", {
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
        is_dynamic_tty = function(...) TRUE,
        .package = "cli"
    )
    renderer <- ShiftFrameRenderer$new(output, backend = "compact")
    renderer$draw(c("full 1", "full 2"), compact = "compact one")
    renderer$draw(c("full 1", "full 2"), compact = "compact two")
    renderer$close()
    expect_equal(created, 1L)
    expect_identical(updates, c("compact one", "compact two"))
    expect_equal(closed, 1L)

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

test_that("compact status preserves stage, unit, progress, and elapsed time", {
    state <- list(
        status = "running",
        stage = "resolve",
        stage_message = "Resolving workflow inputs",
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
    line <- cli::ansi_strip(shift_ui_view__ui_compact_line(
        state,
        width = 80L,
        motion = "full",
        frame = 1L
    ))
    expect_match(line, "Resolve", fixed = TRUE)
    expect_match(line, "2/6", fixed = TRUE)
    expect_match(line, "CEDA", fixed = TRUE)
    expect_match(line, "reference", fixed = TRUE)
    expect_match(line, "15s", fixed = TRUE)
    expect_lte(nchar(line, type = "width"), 80L)
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

test_that("node and case tables respect detail level and abbreviate repeated errors", {
    nodes <- data.table::data.table(
        node = c("DKRZ", "CEDA", "ORNL"),
        future = c(38L, NA_integer_, 48L),
        reference = c(12L, NA_integer_, 24L),
        outcome = c("rejected", "rejected", "completed"),
        result = c(
            paste(
                "coverage: No complete CMIP6 member/grid candidate was found",
                "for model BCC-CSM2-MR with a very long diagnostic"
            ),
            "HTTP 503 while collecting the reference catalog",
            "selected r1i1p1f1 / gn"
        )
    )
    normal_nodes <- shift_ui_view__ui_node_table(
        nodes,
        width = 120L,
        detail = "normal"
    )
    detail_nodes <- shift_ui_view__ui_node_table(
        nodes,
        width = 180L,
        detail = "detail"
    )
    expect_match(paste(normal_nodes, collapse = "\n"), "Resolver attempts")
    expect_match(paste(normal_nodes, collapse = "\n"), "ORNL.*48.*24.*selected")
    expect_match(
        paste(normal_nodes, collapse = "\n"),
        "incomplete coverage",
        fixed = TRUE
    )
    expect_false(any(grepl("very long diagnostic", normal_nodes, fixed = TRUE)))
    expect_true(any(grepl("very long diagnostic", detail_nodes, fixed = TRUE)))

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
})

test_that("persisted watch tables rebuild running state, terminal elapsed time, and failure diagnosis", {
    now <- as.POSIXct("2026-07-20 00:00:00", tz = "UTC")
    spec <- shift_persist__spec_json(list(
        climate = list(
            model = "BCC-CSM2-MR",
            scenarios = "ssp585",
            member = NULL,
            grid = NULL,
            table = NULL
        ),
        periods = list(`2060s` = 2055:2065),
        transform = transform__spec_value(monthly_transform("epwshiftr")),
        reference = NULL,
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
    events <- data.table::data.table(
        event_id = c("stage", "node-start", "node-done"),
        stage = "resolve",
        status = c("running", "running", "completed"),
        message = c(
            "Resolving complete CMIP6 workflow inputs.",
            "ORNL · checking future + reference catalogs",
            "ORNL · selected member r1i1p1f1 · Amon=gn · LImon=gr"
        ),
        details_json = vapply(
            list(
                list(
                    stage = "resolve",
                    phase = "stage",
                    current = 1L,
                    total = 6L,
                    next_stage = "extract_future",
                    stage_sequence = c(
                        "resolve",
                        "extract_future",
                        "extract_reference",
                        "coverage",
                        "morph",
                        "write_epw"
                    )
                ),
                list(
                    stage = "resolve",
                    phase = "unit",
                    unit_type = "index_node",
                    unit_label = "ORNL · checking future + reference catalogs",
                    current = 3L,
                    total = 6L,
                    node = INDEX_NODES[["ORNL"]]
                ),
                list(
                    stage = "resolve",
                    phase = "unit",
                    unit_type = "index_node",
                    unit_label = "ORNL · selected member r1i1p1f1 · Amon=gn · LImon=gr",
                    current = 3L,
                    total = 6L,
                    node = INDEX_NODES[["ORNL"]],
                    future_files = 48L,
                    reference_files = 24L,
                    outcome = "completed",
                    result = "r1i1p1f1 · Amon=gn · LImon=gr"
                )
            ),
            shift_persist__spec_json,
            character(1L)
        ),
        created_at = now + 0:2
    )
    cases <- data.table::data.table(
        experiment_id = c("ssp126", "ssp585"),
        period = "2060s",
        variant_label = "r1i1p1f1",
        status = c("pending", "pending")
    )
    view <- shift_ui_view__ui_table_view(row, cases, events, width = 80L)
    plain <- cli::ansi_strip(view$lines)
    expect_match(plain[[1L]], "Future EPW", fixed = TRUE)
    expect_match(
        paste(plain, collapse = " "),
        "epwshiftr / no reference",
        fixed = TRUE
    )
    expect_true(any(grepl("node 3 of 6", plain, fixed = TRUE)))
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
        unit_label = "No ESGF index node resolved a complete input set.",
        current = 6L,
        total = 6L,
        summary = "No ESGF index node resolved a complete input set.",
        cause = "Future catalog coverage is incomplete.",
        nodes_checked = 6L,
        coverage_failures = 6L,
        usable_nodes = 0L,
        closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn"),
        missing = "future: ssp585/hurs: missing years 2055"
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
    failed_plain <- cli::ansi_strip(failed_view$lines)
    expect_equal(failed_view$state$elapsed_seconds, 90)
    expect_identical(failed_view$state$failure_details$cause, failure$cause)
    expect_match(failed_plain[[1L]], "FAILED", fixed = TRUE)
    expect_true(any(grepl("6 checked.*6 incomplete", failed_plain)))
    expect_true(any(grepl(
        "BCC-CSM2-MR/r1i1p1f1/gn",
        failed_plain,
        fixed = TRUE
    )))
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

test_that("error summaries keep primary cause and persisted events gate URLs by detail level", {
    message <- paste(
        "No ESGF index node produced a complete input set.",
        "x DKRZ: missing hurs",
        "i More detail",
        sep = "\n"
    )
    expect_identical(
        shift_print__error_summary(message),
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
    normal <- shift_ui_view__ui_persisted_event_line(event, detail = "normal")
    debug <- shift_ui_view__ui_persisted_event_line(event, detail = "debug")
    expect_match(normal, "[ORNL][future]", fixed = TRUE)
    expect_false(grepl("https://", normal, fixed = TRUE))
    expect_true(grepl("https://", debug, fixed = TRUE))
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
    label <- shift_run__download_label("future", metrics)
    expect_equal(metrics$current, 1L)
    expect_equal(metrics$total, 3L)
    expect_equal(metrics$bytes_done, 1536)
    expect_equal(metrics$speed_bps, 256)
    expect_equal(metrics$active_files, "tas.nc")
    expect_match(
        label,
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
    cleanup <- shift_run__download_reporter_bind(
        downloader,
        reporter,
        role = "future",
        variables = 8L
    )
    callbacks$task_progress(
        list(
            event = "task_progress",
            session_id = "session",
            filename = "tas.nc",
            target_path = NA_character_,
            data_node = "example.org"
        ),
        downloader
    )
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
    event <- list(
        session_id = "fallback-session",
        filename = "tas.nc",
        target_path = NA_character_,
        data_node = "example.org"
    )
    callbacks$session_start(c(event, event = "session_start"), downloader)
    callbacks$session_done(c(event, event = "session_done"), downloader)
    nested_cleanup()
    expect_equal(calls, c("updated", "updated"))
})

test_that("dynamic failures and completions commit one structured terminal dashboard", {
    frames <- new.env(parent = emptyenv())
    commits <- new.env(parent = emptyenv())
    closes <- new.env(parent = emptyenv())
    frames$value <- list()
    commits$value <- character()
    closes$value <- 0L
    testthat::local_mocked_bindings(
        shift_tui__ui_renderer = shift_ui_test__mock_renderer(
            frames,
            commits,
            closes
        ),
        .package = "epwshiftr"
    )

    # Failure receipt
    reporter <- shift_reporter__reporter(shift_ui("dynamic", motion = "none"))
    reporter$stage_started("resolve", "Resolving inputs.", 1L, 6L)
    reporter$unit_completed(
        "Rejected: incomplete coverage",
        current = 6L,
        total = 6L,
        outcome = "rejected",
        details = list(
            unit_type = "index_node",
            node = INDEX_NODES[["LIU"]],
            future_files = 83L,
            reference_files = 121L,
            error_kind = "coverage",
            error = "incomplete coverage"
        )
    )
    failure <- list(
        kind = "resolver_exhausted",
        summary = "No ESGF index node resolved a complete CMIP6 input set.",
        cause = "Future and historical catalogs have no complete identity in common.",
        nodes_checked = 6L,
        usable_nodes = 0L,
        coverage_failures = 5L,
        timeout_failures = 1L,
        network_failures = 0L,
        other_failures = 0L,
        closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn"),
        missing = "reference: historical/hurs: missing years 1995"
    )
    fail_out <- capture.output(
        reporter$run_failed(message = failure$summary, details = failure),
        type = "message"
    )
    fail_plain <- cli::ansi_strip(utils::tail(frames$value, 1L)[[1L]])
    expect_length(fail_out, 0L)
    expect_identical(commits$value, "failed")
    expect_equal(closes$value, 0L)
    expect_match(fail_plain[[1L]], "FAILED", fixed = TRUE)
    expect_true(any(grepl("6 checked.*5 incomplete.*1 timeout", fail_plain)))
    expect_true(any(grepl("BCC-CSM2-MR/r1i1p1f1/gn", fail_plain, fixed = TRUE)))

    # Completion receipt
    output_dir <- tempfile("shift-completed-output-")
    paths <- file.path(
        output_dir,
        c("BCC-CSM2-MR_ssp126_2060s.epw", "BCC-CSM2-MR_ssp585_2060s.epw")
    )
    run <- shift_stage__new(
        ShiftRun,
        "run",
        ids = list(run_id = "run-complete"),
        meta = list(
            run = data.table::data.table(
                run_id = "run-complete",
                status = "completed",
                output_dir = output_dir
            ),
            cases = data.table::data.table(status = c("completed", "completed"))
        )
    )
    done_rep <- shift_reporter__reporter(
        shift_ui("dynamic", motion = "none"),
        run_id = "run-complete"
    )
    done_rep$stage_started("write_epw", "Writing final EPWs.")
    done_rep$cases_updated(data.table::data.table(
        status = c("completed", "completed")
    ))
    done_rep$unit_completed(
        "Exported final EPWs",
        2L,
        2L,
        details = list(unit_type = "epw_export")
    )
    done_out <- capture.output(
        done_rep$run_completed(
            run,
            data.table::data.table(export_path = paths)
        ),
        type = "message"
    )
    done_plain <- cli::ansi_strip(utils::tail(frames$value, 1L)[[1L]])
    expect_length(done_out, 0L)
    expect_identical(commits$value, c("failed", "done"))
    expect_true(any(grepl(
        "Summary.*2/2 cases completed.*2 EPW files.*0 warnings",
        done_plain
    )))
    expect_true(all(vapply(
        basename(paths),
        function(p) grepl(p, paste(done_plain, collapse = ""), fixed = TRUE),
        logical(1L)
    )))
})

test_that("log mode reporters retain missing reasons, completion summary, and full debug lines", {
    fail_rep <- shift_reporter__reporter(shift_ui("log", detail = "normal"))
    fail_rep$cases_updated(data.table::data.table(
        experiment_id = "ssp585",
        period = "2060s",
        variant_label = "r1i1p1f1",
        status = "missing",
        missing_reason = "future/hurs, future/tas"
    ))
    fail_out <- capture.output(fail_rep$run_failed(), type = "message")
    expect_true(any(grepl("future/hurs", fail_out, fixed = TRUE)))

    output_dir <- tempfile("shift-log-output-")
    run <- shift_stage__new(
        ShiftRun,
        "run",
        ids = list(run_id = "run-log-complete"),
        meta = list(
            run = data.table::data.table(
                run_id = "run-log-complete",
                status = "completed",
                output_dir = output_dir
            )
        )
    )
    done_rep <- shift_reporter__reporter(
        shift_ui("log", detail = "normal"),
        run_id = "run-log-complete"
    )
    done_out <- capture.output(
        done_rep$run_completed(
            run,
            data.table::data.table(export_path = file.path(output_dir, "a.epw"))
        ),
        type = "message"
    )
    expect_true(any(grepl("1 output(s)", done_out, fixed = TRUE)))
    expect_true(any(grepl("Output directory", done_out, fixed = TRUE)))

    withr::local_options(width = 20L)
    debug_rep <- shift_reporter__reporter(shift_ui("log", detail = "debug"))
    debug_rep$stage_started("resolve", "Resolving inputs.")
    long_msg <- paste(rep("complete-catalog-context", 8L), collapse = " ")
    debug_out <- capture.output(
        debug_rep$unit_started(long_msg),
        type = "message"
    )
    expect_true(any(grepl(long_msg, debug_out, fixed = TRUE)))
})

test_that("startup plan summaries, persisted context, and dynamic startup frames stay consistent", {
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "BCC-CSM2-MR",
            c("ssp126", "ssp585"),
            member = "r1i1p1f1",
            grid = "gn",
            table = c(snd = "LImon")
        ),
        periods = list(`2060s` = 2055:2065),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-ui-output-"),
        store = tempfile("shift-ui-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]

    lines <- unname(shift_ui_view__ui_plan_summary(
        plan,
        "run-test",
        width = 80L
    ))
    expect_true(all(nchar(lines, type = "width") <= 80L))
    expect_match(lines[[1L]], "Future EPW.*run-test.*STARTING")
    expect_match(
        lines[[3L]],
        "Enhanced epwshiftr monthly morphing.*no reference.*2 expected"
    )
    expect_match(
        paste(lines, collapse = " "),
        "Selection.*member r1i1p1f1.*grid gn.*tables auto by variable.*tas=Amon"
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
    expect_match(detail_text, "Options.*transition_hours=72")
    expect_match(detail_text, "snow_depth=auto")

    row <- data.table::data.table(
        spec_json = shift_persist__spec_json(list(
            task = "future_epw",
            climate = list(
                model = "BCC-CSM2-MR",
                scenarios = "ssp585",
                member = "r1i1p1f1",
                grid = "gn",
                table = list(snd = "LImon")
            ),
            periods = list(`2060s` = 2055:2065),
            transform = transform__spec_value(monthly_transform("epwshiftr")),
            reference = NULL
        )),
        output_dir = tempfile("future-epw-")
    )
    context <- shift_ui_state__ui_plan_context_from_row(row, cases_total = 1L)
    expect_true("epwshiftr / no reference" %in% context$items)
    expect_match(
        context$selection,
        "member r1i1p1f1.*grid gn.*auto by variable.*snd=LImon"
    )

    # Dynamic startup draws a replaceable frame instead of printing transcript lines
    frames <- new.env(parent = emptyenv())
    commits <- new.env(parent = emptyenv())
    closes <- new.env(parent = emptyenv())
    frames$value <- list()
    commits$value <- character()
    closes$value <- 0L
    testthat::local_mocked_bindings(
        shift_tui__ui_renderer = shift_ui_test__mock_renderer(
            frames,
            commits,
            closes
        ),
        .package = "epwshiftr"
    )
    reporter <- shift_reporter__reporter(shift_ui("dynamic", motion = "none"))
    startup_out <- capture.output(
        reporter$run_started(plan, "run_534d9b84b235029404b318bd"),
        type = "message"
    )
    expect_length(startup_out, 0L)
    expect_length(frames$value, 1L)
    plain <- cli::ansi_strip(frames$value[[1L]])
    expect_false(grepl("run 04b318bd", plain[[1L]], fixed = TRUE))
    expect_match(
        paste(plain, collapse = " "),
        "BCC-CSM2-MR.*ssp126.*Enhanced epwshiftr"
    )

    milestone_out <- capture.output(
        {
            reporter$stage_started("resolve", "Resolving inputs.", 1L, 6L)
            reporter$stage_completed("Resolved inputs.")
        },
        type = "message"
    )
    reporter$close()
    expect_length(milestone_out, 0L)
    expect_equal(closes$value, 1L)
})

test_that("shift_watch() renders the shared status view", {
    plan <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "BCC-CSM2-MR",
            c("ssp126", "ssp585"),
            member = "r1i1p1f1",
            grid = "gn"
        ),
        periods = list(`2060s` = 2055:2065),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-watch-view-output-"),
        store = tempfile("shift-watch-view-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
    test_local_dependencies(list(
        shift_job__launch_job = function(...) invisible(0L)
    ))
    run <- shift_run(plan, background = TRUE, ui = shift_ui("none"))
    on.exit(shift_cancel(run), add = TRUE)

    output <- capture.output(
        watched <- shift_watch(run, follow = FALSE, ui = shift_ui("log")),
        type = "message"
    )
    expect_s7_class(watched, ShiftRun)
    expect_true(any(grepl("Future EPW", output, fixed = TRUE)))
    expect_true(any(grepl("Workflow", output, fixed = TRUE)))
    expect_true(any(grepl("Activity", output, fixed = TRUE)))
    expect_false(any(grepl("queued | planned | cases", output, fixed = TRUE)))
})

test_that("generic operation reporters preserve receipts across log, none, and dynamic modes", {
    log_output <- capture.output(
        {
            reporter <- ShiftReporter$new(
                shift_ui("log"),
                run_id = "run-generic-log",
                step_id = "step-generic-log"
            )
            reporter$operation_started(
                "collect",
                "Collect CMIP6",
                context = list(items = c("Collect CMIP6", "input request")),
                stage_sequence = c("collect", "extract")
            )
            reporter$operation_waiting("12 files collected")
        },
        type = "message"
    )
    expect_true(any(grepl("Collect CMIP6", log_output, fixed = TRUE)))
    expect_true(any(grepl(
        "ready: 12 files collected",
        log_output,
        fixed = TRUE
    )))

    quiet_output <- capture.output(
        {
            reporter <- ShiftReporter$new(
                shift_ui("none"),
                run_id = "run-generic-none"
            )
            reporter$operation_started("extract", "Extract Climate")
            reporter$operation_waiting("3 plans processed")
        },
        type = "message"
    )
    expect_length(quiet_output, 0L)

    frames <- new.env(parent = emptyenv())
    commits <- new.env(parent = emptyenv())
    closes <- new.env(parent = emptyenv())
    frames$value <- list()
    commits$value <- character()
    closes$value <- 0L
    testthat::local_mocked_bindings(
        shift_tui__ui_renderer = shift_ui_test__mock_renderer(
            frames,
            commits,
            closes
        ),
        .package = "epwshiftr"
    )

    for (case in list(
        list(
            stage = "datasets",
            label = "Collect Datasets",
            fn = "operation_completed",
            msg = "1 dataset collected"
        ),
        list(
            stage = "collect",
            label = "Collect CMIP6",
            fn = "operation_waiting",
            msg = "12 files collected"
        ),
        list(
            stage = "collect",
            label = "Collect CMIP6",
            fn = "operation_partial",
            msg = "0 files collected"
        ),
        list(
            stage = "download",
            label = "Download CMIP6",
            fn = "operation_detached",
            msg = "download continues"
        )
    )) {
        rep <- ShiftReporter$new(
            shift_ui("dynamic"),
            run_id = paste0("run-", case$fn)
        )
        rep$operation_started(case$stage, case$label)
        rep[[case$fn]](case$msg)
    }
    expect_identical(commits$value, rep("done", 4L))
})

test_that("heartbeat details merge live state without unthrottled durable touches", {
    touches <- 0L
    testthat::local_mocked_bindings(
        shift_job__job_touch = function(
            store,
            job_id,
            ui_state = NULL,
            urgent = FALSE
        ) {
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
