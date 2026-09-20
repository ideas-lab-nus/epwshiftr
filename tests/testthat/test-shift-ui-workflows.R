# Reuse the deterministic README inputs while exercising production renderers.
ui_workflows__states <- function() {
    env <- new.env(parent = asNamespace("epwshiftr"))
    sys.source(testthat::test_path("..", "..", "tools", "readme-asciicast.R"), env)
    env$readme__batch_states()
}

# Plan a real offline matrix with saved receipts, avoiding remote catalog work.
ui_workflows__batch <- function(root) {
    withr::local_options(list(epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage))
    shift_future_epw(get_cache_epw(), shift_cmip6(model = 2L, scenarios = "ssp585"),
        periods = list(mid = 2049:2050), methods = c("original_morphing", "bws_btws"),
        reference = shift_reference_historical(data.frame(period = "reference", year = 1995:2014)),
        dir = file.path(root, "exports"), store = root, dry_run = TRUE, ui = shift_ui("none"))
}

test_that("calibration, started cases and multi-year exports have honest units", {
    state <- list(stage = "morph", status = "running", unit_label = "ERA5 t2m",
        current_details = list(unit_type = "reanalysis_variable", phase = "unit", current = 2L, total = 6L))
    line <- cli::ansi_strip(shift__ui_metric_line(state, 112L))
    expect_match(line, "Variables.*1/6")
    expect_false(grepl("Cases", line))
    expect_match(paste(cli::ansi_strip(shift__ui_status_lines(state)), collapse = " "), "Calibration.*ERA5")
    expect_match(shift__ui_compact_line(state, width = 112L), "Calibration.*1/6 variables")
    state$current_details <- list(unit_type = "morph_case", phase = "unit", current = 1L, total = 1L)
    expect_match(shift__ui_metric_line(state), "0/1")
    expect_match(shift__ui_compact_line(state, width = 112L), "0/1")
    state$current_details$outcome <- "completed"
    expect_match(shift__ui_metric_line(state), "1/1")
    state$stage <- "write_epw"
    state$cases_total <- 1L
    state$outputs_completed <- 2L
    state$current_details <- list(unit_type = "epw_export", current = 2L, total = 20L)
    line <- cli::ansi_strip(shift__ui_metric_line(state, 112L))
    expect_match(line, "2/20.*exported 2 files")
    expect_false(grepl("exported 2/1", line, fixed = TRUE))
    state$stage <- "resolve"
    state$current_details <- list(phase = "unit", current = 1L, total = 3L)
    expect_match(shift__ui_metric_line(state), "node 1 of 3")
})

test_that("case completion preserves measured multi-year export counts", {
    reporter <- shift__reporter(shift_ui("none"))
    on.exit(reporter$close(), add = TRUE)
    reporter$stage_started("write_epw", "Exporting weather")
    reporter$unit_started("Last file", current = 20L, total = 20L,
        details = list(unit_type = "epw_export"))
    reporter$unit_completed("Exported last file", current = 20L, total = 20L)
    reporter$cases_updated(data.table::data.table(status = "completed"))
    expect_equal(reporter$snapshot()$outputs_completed, 20L)
    stamp <- as.POSIXct("2026-09-16 00:00:00", tz = "UTC")
    row <- data.table::data.table(run_id = "multi-year", status = "completed",
        current_stage = "write_epw", started_at = stamp, completed_at = stamp + 1)
    events <- data.table::data.table(stage = "write_epw", status = "completed",
        message = "Exported last file", created_at = stamp + 1,
        details_json = jsonlite::toJSON(list(stage = "write_epw", phase = "unit",
            unit_type = "epw_export", current = 20L, total = 20L, outcome = "completed"), auto_unbox = TRUE))
    state <- shift__ui_table_state(row, events, data.table::data.table(status = "completed"))
    expect_equal(state$outputs_completed, 20L)
})

test_that("long labels keep a separator and aligned continuations", {
    for (label in c("Output Type", "Weather Year", "Export Path", "校准数据变量")) {
        rows <- cli::ansi_strip(shift__ui_labeled_lines(label, strrep("value ", 20L), 40L))
        prefix <- paste0(label, strrep(" ", max(1L, 9L - cli::ansi_nchar(label, type = "width"))))
        expect_true(startsWith(rows[[1L]], prefix))
        expect_true(startsWith(rows[[2L]], strrep(" ", cli::ansi_nchar(prefix, type = "width"))))
        expect_true(all(cli::ansi_nchar(rows, type = "width") <= 40L))
    }
})

test_that("large batch viewports prioritize failures and active work within their height", {
    snapshot <- ui_workflows__states()[[1L]]
    snapshot$children <- snapshot$children[rep(seq_len(4L), 8L)]
    snapshot$children[, `:=`(child_key = paste0("child", seq_len(.N)),
        model = paste0("Model-", seq_len(.N)), status = "completed")]
    snapshot$children[31L, `:=`(status = "running", current_stage = "download")]
    snapshot$children[32L, status := "failed"]
    snapshot$batch[, `:=`(status = "failed", children = 32L, completed = 30L,
        active = 1L, failed = 1L)]
    snapshot$activity <- list(child31 = list(stage = "download", unit_label = "Downloading tas",
        elapsed_seconds = 35, updated_at = "2026-09-16 00:00:01",
        current_details = list(unit_type = "download_session", bytes_done = 2048,
            bytes_total = 4096, speed_bps = 1024, current = 1L, total = 2L)))
    for (width in c(48L, 60L, 80L, 112L)) for (height in c(8L, 15L, 23L, 39L)) {
        view <- shift_batch__view(snapshot, width = width, height = height)
        expect_lte(length(view$lines), height)
        expect_true(all(cli::ansi_nchar(view$lines, type = "width") <= width - 1L))
        if (height >= 23L && width >= 80L) {
            text <- paste(cli::ansi_strip(view$lines), collapse = " ")
            expect_match(text, "Model-32")
            expect_match(text, "Model-31")
            expect_match(text, "Downloading tas")
            expect_match(text, "hidden")
        }
    }
    full <- paste(cli::ansi_strip(shift_batch__view(snapshot, width = 112L, detail = "detail")$lines), collapse = " ")
    expect_match(full, "Model-1")
    expect_match(full, "Model-32")
    expect_match(full, "2026-09-16 00:00:01")
    expect_false(grepl("children hidden", full, fixed = TRUE))
})

test_that("detailed failures reserve space for active batch children", {
    snapshot <- ui_workflows__states()[[1L]]
    snapshot$children <- snapshot$children[rep(seq_len(4L), 4L)]
    snapshot$children[, `:=`(child_key = paste0("child", seq_len(.N)),
        model = paste0("Model-", seq_len(.N)), status = "completed")]
    snapshot$children[14L, `:=`(status = "running", current_stage = "morph")]
    snapshot$children[15:16, status := "failed"]
    snapshot$batch[, `:=`(status = "failed", children = 16L, completed = 13L,
        active = 1L, failed = 2L)]
    snapshot$activity <- list(child15 = list(stage = "morph", unit_label = "ERA5 t2m",
        last_event = "Calibration failed", elapsed_seconds = 35,
        updated_at = "2026-09-16 00:00:01",
        current_details = list(unit_type = "reanalysis_variable", current = 1L, total = 6L)))
    snapshot$activity$child14 <- snapshot$activity$child15
    snapshot$execution <- data.table::data.table(action = "started", elapsed_seconds = 5)
    snapshot$diagnostics <- data.table::data.table(severity = "error", method = "qdm",
        model = "Model-15", message = "Calibration failed", action = "Restore calibration")
    snapshot$outputs <- data.table::data.table(child_key = "child15",
        provenance_json = jsonlite::toJSON(list(weather_field_roles = list(
            transformed_fields = "tas")), auto_unbox = TRUE))
    for (width in c(80L, 112L)) for (height in c(19L, 23L, 39L)) {
        view <- shift_batch__view(snapshot, width = width, height = height, detail = "detail")
        text <- paste(cli::ansi_strip(view$lines), collapse = "\n")
        expect_lte(length(view$lines), height)
        expect_true(all(cli::ansi_nchar(view$lines, type = "width") <= width - 1L))
        expect_match(text, "Model-15")
        expect_match(text, "Model-14")
    }
    full <- paste(cli::ansi_strip(shift_batch__view(snapshot, detail = "detail")$lines), collapse = "\n")
    expect_match(full, "Model-16")
    expect_match(full, "Calibration failed")
    expect_match(full, "2026-09-16 00:00:01")
})

test_that("errors outrank warnings and retain recovery actions in receipts", {
    snapshot <- ui_workflows__states()[[3L]]
    snapshot$batch$status <- "failed"
    snapshot$diagnostics <- data.table::data.table(method = "qdm", model = "Model-B",
        severity = c(rep("warning", 3L), "error"),
        message = c(paste("Routine", 1:3), "Missing calibration file"),
        action = c(rep(NA_character_, 3L), "Restore calibration and resume"))
    view <- paste(cli::ansi_strip(shift_batch__view(snapshot, width = 112L)$lines), collapse = "\n")
    expect_match(view, "Diagnosis")
    expect_match(view, "Failure.*Missing calibration file")
    expect_match(view, "Action.*Restore calibration and resume")
    expect_match(view, "1 more diagnostics")
    expect_lt(regexpr("Missing calibration", view)[[1L]], regexpr("Routine", view)[[1L]])
    full <- paste(shift_batch__view(snapshot, detail = "detail")$lines, collapse = " ")
    expect_match(full, "Routine 3")
    expect_false(grepl("more diagnostics", full, fixed = TRUE))
})

test_that("frame renderers never paint more rows than the viewport", {
    withr::local_options(epwshiftr.ui_height = 8L)
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    writes <- character()
    renderer <- ShiftFrameRenderer$new(output, backend = "frame",
        writer = function(text) writes <<- c(writes, text))
    renderer$draw(paste("row", 1:30), compact = "safe compact status")
    renderer$suspend(function() writes <<- c(writes, "diagnostic\n"))
    expect_match(utils::tail(writes, 1L), "safe compact status", fixed = TRUE)
    expect_false(grepl("row 30", utils::tail(writes, 1L), fixed = TRUE))
    renderer$commit()
    expect_match(writes[[1L]], "safe compact status", fixed = TRUE)
    expect_false(grepl("row 30", writes[[1L]], fixed = TRUE))
    expect_match(utils::tail(writes, 1L), "row 30\n", fixed = TRUE)
    expect_false(renderer$active())
})

test_that("foreground completion retains every output after viewport fallback", {
    withr::local_options(list(epwshiftr.ui_height = 24L, cli.width = 80L))
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    writes <- character()
    renderer <- ShiftFrameRenderer$new(output, backend = "frame",
        writer = function(text) writes <<- c(writes, text))
    testthat::local_mocked_bindings(shift__ui_renderer = function(...) renderer)
    reporter <- shift__reporter(shift_ui("dynamic", detail = "detail", motion = "none"),
        run_id = "multi-year-receipt")
    reporter$operation_started("write_epw", "Future EPW")
    paths <- file.path("/exports", paste0("Weather_Model-A_ssp585_", 2041:2060, ".epw"))
    run <- shift_stage_new(ShiftRun, "run", ids = list(run_id = "multi-year-receipt"),
        meta = list(run = data.table::data.table(status = "completed", task = "future_epw",
            output_dir = "/exports"), cases = data.table::data.table(status = "completed")))
    reporter$run_completed(run, data.table::data.table(export_path = paths))
    receipt <- cli::ansi_strip(utils::tail(writes, 1L))
    expect_match(receipt, "/exports", fixed = TRUE)
    for (path in paths) expect_match(receipt, basename(path), fixed = TRUE)
    expect_false(renderer$active())
})

test_that("R batch watch leaves a complete final receipt in every human mode", {
    states <- ui_workflows__states()
    for (follow in c(FALSE, TRUE)) {
        index <- 0L
        calls <- character()
        renderer <- list(draw = function(...) { calls <<- c(calls, "draw"); TRUE },
            close = function(...) { calls <<- c(calls, "close"); invisible(NULL) })
        testthat::with_mocked_bindings({
            printed <- capture.output(invisible(shift_batch__watch(NULL, follow,
                interval = 0.1, events = 10L, ui = shift_ui("dynamic", motion = "none"))), type = "message")
        }, shift__ui_renderer = function(...) renderer,
            shift_batch__refresh = function(x) x,
            shift__watch_sleep = function(...) NULL,
            shift__watch_now = function() as.POSIXct(index * 10, origin = "1970-01-01"),
            shift_batch__snapshot = function(...) {
                index <<- index + 1L
                if (follow && index == 1L) states[[1L]] else states[[3L]]
            }, .package = "epwshiftr")
        expect_match(paste(cli::ansi_strip(printed), collapse = " "), "Future EPW Batch.*8 EPW files")
        if (follow) expect_true("draw" %in% calls) else expect_false("draw" %in% calls)
        expect_identical(utils::tail(calls, 1L), "close")
    }
})

test_that("CLI count-limited dynamic watch keeps its final snapshot", {
    snapshot <- ui_workflows__states()[[1L]]
    testthat::local_mocked_bindings(shift_batch_get = function(...) NULL,
        shift_batch__snapshot = function(...) snapshot,
        shift__ui_renderer = function(...) list(draw = function(...) TRUE, close = function(...) NULL))
    output <- capture.output(invisible(epwshiftr_cli_shift_watch_follow(NULL, "batch-test",
        batch_id = "batch-test", progress = "dynamic", count = 1L)), type = "message")
    expect_match(paste(cli::ansi_strip(output), collapse = " "), "Future EPW Batch")
})

test_that("download resume and partial flag sets respect global output policy", {
    flags <- epwshiftr_cli_parse_command(character(), flags = "--network")
    expect_identical(epwshiftr_cli_task_ui(flags, quiet = TRUE)@progress, "none")
    seen <- logical()
    downloader <- list(resume = function(...) {
        seen <<- c(seen, list(...)$progress)
        data.table::data.table()
    })
    testthat::local_mocked_bindings(epwshiftr_cli_downloader = function(...) downloader)
    for (mode in c("quiet", "json", "jsonl")) {
        args <- list(store = list(sync_downloads = function(...) NULL), command = "resume",
            args = c("--session", "test"))
        args[[mode]] <- TRUE
        do.call(epwshiftr_cli_download, args)
    }
    expect_identical(seen, rep(FALSE, 3L))
})

test_that("discovery context reaches logs and compact views without heartbeat repeats", {
    ui <- shift_ui("log")
    ui@batch_context <- list(kind = "discovery", current = 2L, total = 3L,
        message = "method 2/3: qdm \u00b7 CEDA \u00b7 tas, hurs")
    reporter <- shift__reporter(ui)
    on.exit(reporter$close(), add = TRUE)
    lines <- capture.output({
        reporter$operation_started("collect", "Collect CMIP6")
        reporter$unit_started("Collecting Dataset catalog", current = 1L, total = 2L)
        reporter$heartbeat(force = TRUE)
        reporter$heartbeat(force = TRUE)
    }, type = "message")
    text <- paste(cli::ansi_strip(lines), collapse = "\n")
    expect_match(text, "Discovery.*method 2/3: qdm.*CEDA.*tas, hurs")
    expect_equal(sum(grepl("method 2/3", lines, fixed = TRUE)), 1L)
    state <- reporter$snapshot()
    for (width in c(48L, 80L, 112L)) {
        compact <- cli::ansi_strip(shift__ui_compact_line(state, width = width))
        expect_match(compact, "method 2/3: qdm.*CEDA")
        expect_lte(cli::ansi_nchar(compact, type = "width"), width)
        if (width >= 80L) expect_match(compact, "tas, hurs", fixed = TRUE)
    }
    ui@progress <- "none"
    quiet <- shift__reporter(ui)
    on.exit(quiet$close(), add = TRUE)
    expect_output(quiet$operation_started("collect", "Collect CMIP6"), NA, type = "message")
})

test_that("history and summary inspect saved dry-run matrices without discovery", {
    root <- tempfile("ui-history-")
    batch <- ui_workflows__batch(root)
    testthat::local_mocked_bindings(shift_batch__discover_models = function(...) stop("Unexpected network"))
    history <- shift_history(root, type = "batch")
    expect_equal(nrow(history), 1L)
    expect_identical(history$status, "planned")
    expect_identical(history$id, batch@ids$batch_id)
    expect_equal(nrow(shift_history(root, status = "failed")), 0L)
    output <- capture.output(result <- epwshiftr_cli(c("--json", "--store", root,
        "shift", "list", "--type", "batch")))
    expect_equal(result$status, 0L, info = result$error)
    expect_true(jsonlite::validate(paste(output, collapse = "\n")))
    summary <- shift_summary(batch, refresh = FALSE)
    expect_equal(nrow(summary), 4L)
    expect_equal(sum(summary$cases), 4L)
    expect_true(all(summary$epw_files == 0L))
    expect_setequal(summary$method, c("original_morphing", "bws_btws"))
    cli <- epwshiftr_cli(c("--quiet", "--store", root, "shift", "summary", "--batch", batch@ids$batch_id))
    expect_equal(cli$status, 0L, info = cli$error)
    missing <- tempfile("no-store-")
    expect_equal(nrow(shift_history(missing)), 0L)
    expect_false(dir.exists(missing))
    writeBin(charToRaw("broken"), shift_batch__receipt_path(batch@store_path))
    expect_identical(shift_history(root, type = "batch")$status, "unavailable")
    expect_equal(nrow(shift_history(root, type = "run")), 0L)
})

test_that("malformed batch receipts do not hide healthy history or break JSON output", {
    root <- tempfile("ui-malformed-history-")
    batch <- ui_workflows__batch(root)
    original <- readRDS(shift_batch__receipt_path(batch@store_path))
    for (kind in c("child", "manifest", "identity")) {
        receipt <- original
        receipt$batch_id <- paste0("batch-malformed-", kind)
        if (kind == "child") receipt$children <- list("invalid-child")
        if (kind == "manifest") receipt$manifest <- "invalid-manifest"
        if (kind == "identity") receipt$children[[1L]]$child_key <- "missing-child"
        path <- file.path(root, "batches", receipt$batch_id)
        dir.create(path)
        saveRDS(receipt, shift_batch__receipt_path(path))
    }
    receipts <- list.files(file.path(root, "batches"), recursive = TRUE, full.names = TRUE)
    before <- tools::md5sum(receipts)
    history <- shift_history(root)
    expect_equal(nrow(history), 4L)
    expect_equal(sum(history$status == "planned"), 1L)
    expect_equal(sum(history$status == "unavailable"), 3L)
    expect_true(all(nzchar(history$error[history$status == "unavailable"])))
    expect_equal(nrow(shift_history(root, type = "run")), 0L)
    expect_equal(nrow(shift_history(root, type = "batch", status = "unavailable")), 3L)
    output <- capture.output(result <- epwshiftr_cli(c("--json", "--store", root,
        "shift", "list", "--type", "batch")))
    expect_equal(result$status, 0L, info = result$error)
    expect_true(jsonlite::validate(paste(output, collapse = "\n")))
    expect_match(paste(output, collapse = "\n"), batch@ids$batch_id, fixed = TRUE)
    expect_identical(tools::md5sum(receipts), before)
})

test_that("history retains unavailable children after reading an earlier child", {
    root <- tempfile("ui-unavailable-")
    batch <- ui_workflows__batch(root)
    path <- shift_batch__receipt_path(batch@store_path)
    receipt <- readRDS(path)
    for (index in seq_along(receipt$children)) {
        receipt$children[[index]]$run_id <- paste0("missing-run-", index)
    }
    saveRDS(receipt, path)
    rows <- shift_history(root, type = "run")
    expect_equal(nrow(rows), 4L)
    expect_true(all(rows$status == "unavailable"))
    expect_true(all(nzchar(rows$error)))
    expect_equal(nrow(shift_history(root, status = "planned")), 0L)
})

test_that("terminal height handles explicit, missing and invalid dimensions", {
    expect_equal(shift__ui_height(18L), 17L)
    expect_equal(shift__ui_height(integer()), 23L)
    expect_equal(shift__ui_height("invalid"), 23L)
    withr::local_options(epwshiftr.ui_height = 14L)
    expect_equal(shift__ui_height(), 13L)
})

test_that("weather comparison excludes missing codes and weights valid hourly rows", {
    original <- get_cache_epw()
    baseline <- epw_file_read(original)$data()
    paths <- vapply(c(2L, 3L), function(n) {
        path <- tempfile(fileext = ".epw")
        writeLines(readLines(original, n = 8L), path)
        values <- data.table::copy(baseline[seq_len(n)])
        values[, dry_bulb_temperature := if (n == 2L) c(10, 99.9) else 20]
        values[, relative_humidity := if (n == 2L) c(50, 999) else 80]
        data.table::fwrite(values[, EPW_FILE_COLUMNS, with = FALSE], path,
            append = TRUE, col.names = FALSE, quote = FALSE, na = "")
        path
    }, character(1L))
    withr::defer(unlink(paths))
    result <- shift_inspect__weather(c(paths, tempfile("missing-")))
    expect_equal(result$weather_hours, 5L)
    expect_equal(result$temperature_hours, 4L)
    expect_equal(result$mean_temperature_c, 17.5)
    expect_equal(result$humidity_hours, 4L)
    expect_equal(result$mean_relative_humidity_pct, 72.5)
    expect_equal(result$unreadable_files, 1L)
    expect_true(nzchar(result$weather_error))
})

test_that("staged summaries preserve output identities, transforms and weather groups", {
    original <- get_cache_epw()
    baseline <- epw_file_read(original)$data()
    paths <- vapply(c(10, 20, 40), function(temperature) {
        path <- tempfile(fileext = ".epw")
        writeLines(readLines(original, n = 8L), path)
        values <- data.table::copy(baseline[1L])
        values[, dry_bulb_temperature := temperature]
        data.table::fwrite(values[, EPW_FILE_COLUMNS, with = FALSE], path,
            append = TRUE, col.names = FALSE, quote = FALSE, na = "")
        path
    }, character(1L))
    withr::defer(unlink(paths))
    outputs <- data.table::data.table(source_id = c("Model-A", "Model-A", "Model-B"),
        experiment_id = c("ssp126", "ssp126", "ssp585"), variant_label = "r1i1p1f1",
        grid_label = "gn", period = "2060s", case_id = c("morph-1", "morph-1", "morph-2"),
        path = paths, output_type = "actual_year", weather_year = c(2060L, 2061L, 2060L))
    transform <- daily_transform("epwshiftr")
    morphed <- shift_stage_new(ShiftMorphed, "morphed", meta = list(transform = transform))
    stage <- shift_stage_new(ShiftOutputs, "outputs", store_path = tempdir(),
        meta = list(outputs = outputs, morphed = morphed))
    run <- shift_stage_new(ShiftRun, "run", store_path = tempdir(),
        ids = list(run_id = "staged-run"), meta = list(
            run = data.table::data.table(task = "collect", status = "completed",
                spec_json = '{"task":"collect"}'), cases = data.table::data.table()))
    # Isolate result restoration; exercise real public grouping and EPW reads.
    testthat::local_mocked_bindings(shift_result = function(...) stage)
    summary <- shift_summary(run, refresh = FALSE, weather = TRUE, ui = shift_ui("none"))
    data.table::setorder(summary, model)
    expect_equal(nrow(summary), 2L)
    expect_identical(summary$model, c("Model-A", "Model-B"))
    expect_identical(summary$scenario, c("ssp126", "ssp585"))
    expect_true(all(summary$member == "r1i1p1f1" & summary$grid == "gn" & summary$period == "2060s"))
    expect_true(all(summary$method == transform@method & summary$scale == transform@scale))
    expect_true(all(summary$reconstruction == transform@reconstruction))
    expect_equal(summary$cases, c(1L, 1L))
    expect_equal(summary$completed_cases, c(1L, 1L))
    expect_equal(summary$epw_files, c(2L, 1L))
    expect_equal(summary$mean_temperature_c, c(15, 40))
    expect_equal(summary$temperature_hours, c(2L, 1L))
})

test_that("batch snapshots include cancellation counts and live child activity", {
    root <- tempfile("ui-snapshot-")
    batch <- ui_workflows__batch(root)
    statuses <- c("cancelled", "stopping", "completed")
    for (index in seq_along(statuses)) {
        plan <- batch@meta$children[[index]]
        batch@meta$children[[index]] <- shift_stage_new(ShiftRun, "run", store_path = plan@store_path,
            ids = list(run_id = paste0("run", index), morph_id = NA_character_),
            meta = list(run = data.table::data.table(status = statuses[[index]], task = "future_epw",
                current_stage = "morph", started_at = as.POSIXct("2026-09-16 00:00:00", tz = "UTC"),
                updated_at = as.POSIXct("2026-09-16 00:00:10", tz = "UTC")),
                jobs = data.table::data.table(heartbeat_at = as.POSIXct("2026-09-16 00:00:20", tz = "UTC")),
                cases = plan@meta$expected_cases,
                ui_state = list(status = "running", stage = "morph", unit_label = "ERA5 t2m", elapsed_seconds = 20,
                    current_details = list(unit_type = "reanalysis_variable", current = 2L, total = 6L))))
    }
    snapshot <- shift_batch__snapshot(batch, refresh = FALSE)
    expect_equal(snapshot$batch$cancelled, 1L)
    counts <- snapshot$batch[, c("completed", "active", "failed", "partial", "waiting", "cancelled"), with = FALSE]
    expect_equal(sum(unlist(counts)), snapshot$batch$children)
    expect_match(snapshot$activity[[2L]]$updated_at, "00:00:20")
    expect_identical(snapshot$activity[[1L]]$status, "cancelled")
    expect_identical(snapshot$activity[[2L]]$status, "stopping")
    text <- paste(cli::ansi_strip(shift_batch__view(snapshot, width = 112L)$lines), collapse = " ")
    expect_match(text, "1 cancelled")
    expect_match(text, "ERA5 t2m")
    expect_match(text, "Variables.*2/6")
})

test_that("live JSON timestamps retain clock time, fractions and UTC offsets", {
    stamp <- as.POSIXct("2026-09-18 01:23:45", tz = "UTC")
    original <- data.table::data.table(started_at = stamp,
        updated_at = stamp + 60, completed_at = as.POSIXct(NA, tz = "UTC"))
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    store_write_json_atomic(original, path, dataframe = "rows", na = "null",
        POSIXt = "ISO8601", digits = 15)
    restored <- shift__live_table(jsonlite::fromJSON(path))
    expect_equal(as.numeric(restored$started_at) - as.numeric(stamp), 0, tolerance = 1e-6)
    expect_equal(as.numeric(difftime(restored$updated_at, restored$started_at,
        units = "secs")), 60)
    expect_s3_class(restored$completed_at, "POSIXct")
    expect_true(is.na(restored$completed_at))
    values <- c("2026-09-18T01:23:45.125Z", "2026-09-18T09:23:45.125+08:00",
        "2026-09-17T20:23:45.125-0500", "2026-09-18 01:23:45.125", NA_character_)
    parsed <- shift__live_table(data.frame(heartbeat_at = values))$heartbeat_at
    expect_equal(as.numeric(parsed[1:4]) - as.numeric(stamp), rep(0.125, 4L))
    expect_true(is.na(parsed[[5L]]))
    expect_equal(shift__live_time(stamp), stamp)
    expect_equal(shift__live_time("2026-09-18"), as.POSIXct("2026-09-18", tz = "UTC"))
})

test_that("cancellation overrides stale worker frames in R and CLI watch", {
    root <- tempfile("ui-cancel-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    stamp <- as.POSIXct("2026-09-18 01:23:45", tz = "UTC")
    id <- "cancel-test"
    row <- data.table::data.table(run_id = id, task = "future_epw", status = "running",
        current_stage = "morph", spec_json = "{}", started_at = stamp,
        updated_at = stamp + 10, completed_at = as.POSIXct(NA), last_error = NA_character_)
    events <- data.table::data.table(event_id = "first", stage = "morph", status = "running",
        created_at = stamp + 10, message = "Morphing", details_json = "{}")
    state <- list(run_id = id, task_label = "Future EPW", status = "running",
        stage = "morph", unit_label = "ssp585 temperature", elapsed_seconds = 10,
        current_details = list(unit_type = "morph_case", current = 1L, total = 2L))
    payload <- list(run_id = id, run = row, cases = data.table::data.table(), events = events,
        jobs = data.table::data.table(job_id = "cancel-job", status = "running"), ui_state = state)
    path <- shift__live_path(root, id)
    dir.create(dirname(path), recursive = TRUE)
    store_write_json_atomic(payload, path, auto_unbox = TRUE, dataframe = "rows",
        null = "null", na = "null", POSIXt = "ISO8601")
    run <- shift__live_cancel_mark(root, id, "cancel-job", "stopping")
    expect_identical(run@meta$run$status, "stopping")
    expect_identical(run@meta$ui_state$status, "stopping")
    expect_identical(run@meta$jobs$status, "stopping")
    expect_false(is.na(run@meta$jobs$cancel_requested_at))
    # Older sidecars and snapshots may still carry a pre-cancellation frame.
    run@meta$ui_state$status <- "running"
    testthat::local_mocked_bindings(shift_outputs = function(...) data.table::data.table(),
        shift__watch_now = function() stamp + 60)
    view <- shift__ui_run_view(run)
    expect_identical(view$state$status, "stopping")
    expect_equal(view$state$elapsed_seconds, 60)
    expect_match(paste(cli::ansi_strip(view$lines), collapse = " "), "STOPPING")
    expect_match(cli::ansi_strip(view$compact), "STOPPING")
    expect_identical(view$state$unit_label, "ssp585 temperature")
    snapshot <- list(run = run@meta$run, cases = run@meta$cases, events = run@meta$events,
        outputs = data.table::data.table(), diagnostics = shift_diagnostics_empty())
    attr(snapshot, "shift_ui_state") <- run@meta$ui_state
    output <- capture.output(epwshiftr_cli_render_shift_watch(snapshot), type = "message")
    expect_match(paste(cli::ansi_strip(output), collapse = " "), "STOPPING")
    frames <- character()
    testthat::local_mocked_bindings(epwshiftr_cli_shift_watch_snapshot = function(...) snapshot,
        shift__ui_renderer = function(...) list(draw = function(lines, compact) {
            frames <<- c(frames, lines, compact)
            TRUE
        }, close = function(...) NULL))
    invisible(capture.output(invisible(epwshiftr_cli_shift_watch_follow(NULL, id,
        progress = "dynamic", count = 1L)), type = "message"))
    expect_match(paste(cli::ansi_strip(frames), collapse = " "), "STOPPING")
    expect_false(any(grepl("RUNNING", frames, fixed = TRUE)))
    snapshot$run$status <- "cancelled"
    snapshot$run$completed_at <- stamp + 30
    terminal <- shift__ui_table_view(snapshot$run, snapshot$cases, snapshot$events,
        ui_state = attr(snapshot, "shift_ui_state"))
    expect_identical(terminal$state$status, "cancelled")
    expect_equal(terminal$state$elapsed_seconds, 30)
})

# Interleave two child histories with a failure that becomes visible late.
ui_workflows__interleaved_events <- function() {
    data.table::data.table(event_id = c("a1", "a2", "b1", "a3", "a4"),
        run_id = c("run-a", "run-a", "run-b", "run-a", "run-a"),
        child_key = c("a", "a", "b", "a", "a"), method = "qdm", model = "Model-A",
        stage = "morph", status = c("running", "failed", "running", "running", "completed"),
        message = c("A started", "Late failure A2", "B started", "Final event A3", "Final event A4"),
        created_at = as.POSIXct("2026-09-18 01:00:00", tz = "UTC") + seq_len(5L),
        details_json = "{}")
}

test_that("batch event cursors retain interleaved and reordered child events", {
    events <- ui_workflows__interleaved_events()
    first <- shift_batch__event_delta(events[c(1L, 3L)], initial_limit = 1L, initial = TRUE)
    expect_identical(first$rows$event_id, "b1")
    second <- shift_batch__event_delta(events[1:3], first$cursor)
    expect_identical(second$rows$event_id, "a2")
    expect_false(second$gap)
    unchanged <- shift_batch__event_delta(events[c(3L, 2L, 1L)], second$cursor)
    expect_equal(nrow(unchanged$rows), 0L)
    new_child <- data.table::copy(events[1L])
    new_child[, `:=`(event_id = "c1", run_id = "run-c", child_key = "c")]
    expanded <- shift_batch__event_delta(rbind(events[1:3], new_child), second$cursor)
    expect_identical(expanded$rows$event_id, "c1")
    expect_false(expanded$gap)
    truncated <- shift_batch__event_delta(events[4:5], expanded$cursor)
    expect_identical(truncated$rows$event_id, c("a3", "a4"))
    expect_true(truncated$gap)
    # Opening the durable store after a bounded live buffer must not replay
    # previously seen events, even when the full history reappears.
    restored <- shift_batch__event_delta(events, truncated$cursor)
    expect_equal(nrow(restored$rows), 0L)
    expect_false(restored$gap)
    empty <- shift_batch__event_delta(events[0], restored$cursor)
    expect_identical(empty$cursor, restored$cursor)
})

test_that("R and CLI batch watchers emit late and terminal events exactly once", {
    events <- ui_workflows__interleaved_events()
    states <- ui_workflows__states()
    snapshots <- lapply(seq_len(4L), function(index) {
        snapshot <- states[[if (index == 4L) 3L else 1L]]
        snapshot$events <- events[0]
        attr(snapshot, "shift_ui_events") <- if (index == 1L) events[c(1L, 3L)] else
            if (index == 4L) events else events[1:3]
        snapshot
    })
    index <- 0L
    testthat::local_mocked_bindings(shift_batch_get = function(...) NULL,
        shift_batch__refresh = function(x) x,
        shift__watch_sleep = function(...) NULL,
        shift__watch_now = function() as.POSIXct(index * 10, origin = "1970-01-01"),
        shift_batch__snapshot = function(...) {
            index <<- index + 1L
            snapshots[[index]]
        })
    output <- capture.output(invisible(epwshiftr_cli_shift_watch_follow(NULL, "batch-test",
        batch_id = "batch-test", jsonl = TRUE, progress = "none", interval = 0, event_count = 0L)))
    records <- lapply(output, jsonlite::fromJSON)
    expect_identical(vapply(records, `[[`, character(1L), "type"),
        c("snapshot", "event", "event", "event", "terminal"))
    expect_identical(vapply(records[2:4], function(x) x$event$event_id, character(1L)),
        c("a2", "a3", "a4"))
    for (interface in c("R", "CLI")) {
        index <- 0L
        output <- capture.output({
            if (interface == "R") invisible(shift_batch__watch(NULL, TRUE, 0.1, 0L, shift_ui("log"))) else
                invisible(epwshiftr_cli_shift_watch_follow(NULL, "batch-test", batch_id = "batch-test",
                    progress = "log", interval = 0, event_count = 0L))
        }, type = "message")
        for (message in events$message[c(2L, 4L, 5L)]) {
            expect_equal(sum(grepl(message, output, fixed = TRUE)), 1L, info = interface)
        }
    }
})

test_that("single-run show detail retains all rows and long fields without changing JSON", {
    withr::local_options(list(cli.width = 60L, cli.num_colors = 1L))
    long_path <- paste0("/weather/", strrep("reference-model/", 8L), "25_weather.epw")
    action <- paste0("Recover ", strrep("missing historical input ", 8L), "ACTION_END")
    raw <- '{"raw_debug_payload":"RAW_PAYLOAD_END"}'
    run <- shift_stage_new(ShiftRun, "run", ids = list(run_id = "show-detail"),
        meta = list(run = data.table::data.table(run_id = "show-detail", status = "completed", spec_json = raw),
            events = data.table::data.table(stage = "morph", status = "completed",
                message = paste0("EVENT_", seq_len(25L), "_END"), created_at = Sys.time(), details_json = raw)))
    outputs <- data.table::data.table(path = c(paste0("/outputs/", seq_len(24L), ".epw"), long_path))
    testthat::local_mocked_bindings(cli_shift__target = function(...) run,
        shift_cases = function(...) data.table::data.table(), shift_outputs = function(...) outputs,
        shift_diagnostics = function(...) data.table::data.table(stage = "morph", severity = "error", action = action),
        shift_explain = function(...) data.table::data.table(stage = "morph", status = "ready"))
    snapshots <- lapply(list(character(), "--verbose", "--debug"), function(flag) {
        epwshiftr_cli_shift_show(NULL, c("--run", "show-detail", flag))
    })
    output <- lapply(snapshots, function(value) cli::ansi_strip(capture.output(
        epwshiftr_cli_render_shift_show(value), type = "message")))
    expect_identical(vapply(snapshots, attr, character(1L), "shift_ui_detail"), c("normal", "detail", "debug"))
    expect_false(any(grepl("EVENT_25_END", output[[1L]], fixed = TRUE)))
    for (index in 2:3) {
        text <- gsub("[[:space:]]", "", paste(output[[index]], collapse = ""))
        expect_match(text, long_path, fixed = TRUE)
        expect_match(text, gsub("[[:space:]]", "", action), fixed = TRUE)
        expect_match(text, "EVENT_25_END", fixed = TRUE)
        expect_true(all(cli::ansi_nchar(output[[index]], type = "width") <= 60L))
    }
    expect_false(any(grepl("RAW_PAYLOAD_END", output[[2L]], fixed = TRUE)))
    expect_match(paste(output[[3L]], collapse = ""), "RAW_PAYLOAD_END", fixed = TRUE)
    json <- lapply(snapshots, jsonlite::toJSON, dataframe = "rows", POSIXt = "ISO8601")
    expect_identical(json[[1L]], json[[2L]])
    expect_identical(json[[1L]], json[[3L]])
})

test_that("config templates do not initialize the default store", {
    testthat::local_mocked_bindings(store_dir = function(...) stop("Unexpected store initialization"))
    out <- epwshiftr_cli(c("--quiet", "shift", "config", "example", "--methods", "original_morphing"))
    expect_equal(out$status, 0L, info = out$error)
})
