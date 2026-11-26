wf_plain <- function(x, collapse = " ") {
    paste(cli::ansi_strip(x), collapse = collapse)
}
wf_msg <- function(expr, collapse = " ") {
    wf_plain(capture.output(expr, type = "message"), collapse)
}

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
        wf_plain(shift_ui_view__ui_status_lines(state)),
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
    text <- wf_plain(compact_view$lines)
    expect_match(text, "Model-32.*Model-31|Model-31.*Model-32")
    expect_match(text, "Downloading tas")
    expect_match(text, "hidden")
    expect_lte(
        length(shift_batch_ui__view(snapshot, width = 60L, height = 10L)$lines),
        10L
    )

    full <- wf_plain(
        shift_batch_ui__view(snapshot, width = 112L, detail = "detail")$lines
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
    view <- wf_plain(shift_batch_ui__view(diag_snap, width = 112L)$lines, "\n")
    expect_match(view, "Failure.*Missing calibration file")
    expect_match(view, "Action.*Restore calibration and resume")
    expect_match(view, "1 more diagnostics")
    expect_lt(
        regexpr("Missing calibration", view)[[1L]],
        regexpr("Routine", view)[[1L]]
    )
    full_diag <- wf_plain(
        shift_batch_ui__view(diag_snap, detail = "detail")$lines
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
        text <- wf_msg(testthat::with_mocked_bindings(
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
        ))
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
    cli_text <- wf_msg(invisible(epwshiftr_cli_shift_watch_follow(
        NULL,
        "batch-test",
        batch_id = "batch-test",
        progress = "dynamic",
        count = 1L
    )))
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
        wf_plain(lines, "\n"),
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
        wf_msg(quiet$operation_started("collect", "Collect CMIP6")),
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
    text <- wf_plain(shift_batch_ui__view(snapshot, width = 112L)$lines)
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
    expect_match(wf_plain(view$lines), "STOPPING")
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
    wf_msg(invisible(epwshiftr_cli_shift_watch_follow(
        NULL,
        id,
        progress = "dynamic",
        count = 1L
    )))
    frames <- c(unlist(mock$frames), mock$compacts)
    expect_match(wf_plain(frames), "STOPPING")
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
        wf_msg(epwshiftr_cli_render_shift_show(v), "")
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
