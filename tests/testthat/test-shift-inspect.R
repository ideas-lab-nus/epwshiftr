# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("artifact row reader preserves order, metadata, and global limits", {
    root <- tempfile("shift-artifact-reader-")
    dir.create(root)
    paths <- file.path(root, paste0("part-", 1:3, ".data"))
    expect_true(all(file.create(paths)))
    on.exit(unlink(root, recursive = TRUE), add = TRUE)

    records <- data.frame(
        relative_path = basename(paths),
        artifact_label = c("first", "second", "third"),
        check.names = FALSE
    )
    calls <- new.env(parent = emptyenv())
    calls$paths <- character()
    calls$limits <- numeric()
    reader <- function(path, limit, columns) {
        calls$paths <- c(calls$paths, basename(path))
        calls$limits <- c(calls$limits, limit)
        rows <- data.table::data.table(value = c(1L, 2L))
        if (!is.infinite(limit)) {
            rows <- utils::head(rows, limit)
        }
        rows
    }

    out <- shift_inspect__read_artifact_rows(
        store = list(path = root),
        records = records,
        n = 3L,
        columns = c("artifact_label", "value"),
        path_column = "relative_path",
        reader = reader,
        metadata = function(records, i) {
            list(artifact_label = records$artifact_label[[i]])
        },
        missing = c(
            "Fixture artifact is missing.",
            "x" = "{.path {path}}"
        ),
        stage = "fixture"
    )

    expect_named(out, c("artifact_label", "value"))
    expect_identical(out$artifact_label, c("first", "first", "second"))
    expect_identical(out$value, c(1L, 2L, 1L))
    expect_identical(calls$paths, basename(paths[1:2]))
    expect_identical(calls$limits, c(3, 1))

    unlink(paths[[1L]])
    expect_error(
        shift_inspect__read_artifact_rows(
            store = list(path = root),
            records = records[1L, , drop = FALSE],
            n = Inf,
            path_column = "relative_path",
            reader = reader,
            missing = c(
                "Fixture artifact is missing.",
                "x" = "{.path {path}}"
            ),
            stage = "fixture"
        ),
        "Fixture artifact is missing"
    )
})

test_that("history and summary inspect saved dry-run matrices without discovery", {
    root <- tempfile("ui-history-")
    batch <- ui_workflows__batch(root)
    testthat::local_mocked_bindings(shift_batch_ui__discover_models = function(
        ...
    ) {
        stop("Unexpected network")
    })
    history <- shift_history(root, type = "batch")
    expect_equal(nrow(history), 1L)
    expect_identical(history$status, "planned")
    expect_identical(history$id, batch@ids$batch_id)
    expect_equal(nrow(shift_history(root, status = "failed")), 0L)
    output <- capture.output(
        result <- epwshiftr_cli(c(
            "--json",
            "--store",
            root,
            "shift",
            "list",
            "--type",
            "batch"
        ))
    )
    expect_equal(result$status, 0L, info = result$error)
    expect_true(jsonlite::validate(paste(output, collapse = "\n")))
    summary <- shift_summary(batch, refresh = FALSE)
    expect_equal(nrow(summary), 4L)
    expect_equal(sum(summary$cases), 4L)
    expect_true(all(summary$epw_files == 0L))
    expect_setequal(summary$method, c("original_morphing", "bws_btws"))
    cli <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        root,
        "shift",
        "summary",
        "--batch",
        batch@ids$batch_id
    ))
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
        if (kind == "child") {
            receipt$children <- list("invalid-child")
        }
        if (kind == "manifest") {
            receipt$manifest <- "invalid-manifest"
        }
        if (kind == "identity") {
            receipt$children[[1L]]$child_key <- "missing-child"
        }
        path <- file.path(root, "batches", receipt$batch_id)
        dir.create(path)
        saveRDS(receipt, shift_batch__receipt_path(path))
    }
    receipts <- list.files(
        file.path(root, "batches"),
        recursive = TRUE,
        full.names = TRUE
    )
    before <- tools::md5sum(receipts)
    history <- shift_history(root)
    expect_equal(nrow(history), 4L)
    expect_equal(sum(history$status == "planned"), 1L)
    expect_equal(sum(history$status == "unavailable"), 3L)
    expect_true(all(nzchar(history$error[history$status == "unavailable"])))
    expect_equal(nrow(shift_history(root, type = "run")), 0L)
    expect_equal(
        nrow(shift_history(root, type = "batch", status = "unavailable")),
        3L
    )
    output <- capture.output(
        result <- epwshiftr_cli(c(
            "--json",
            "--store",
            root,
            "shift",
            "list",
            "--type",
            "batch"
        ))
    )
    expect_equal(result$status, 0L, info = result$error)
    expect_true(jsonlite::validate(paste(output, collapse = "\n")))
    expect_match(
        paste(output, collapse = "\n"),
        batch@ids$batch_id,
        fixed = TRUE
    )
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

test_that("staged summaries preserve output identities, transforms and weather groups", {
    original <- get_cache_epw()
    baseline <- epw_file_read(original)$data()
    paths <- vapply(
        c(10, 20, 40),
        function(temperature) {
            path <- tempfile(fileext = ".epw")
            writeLines(readLines(original, n = 8L), path)
            values <- data.table::copy(baseline[1L])
            values[, dry_bulb_temperature := temperature]
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
    outputs <- data.table::data.table(
        source_id = c("Model-A", "Model-A", "Model-B"),
        experiment_id = c("ssp126", "ssp126", "ssp585"),
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        period = "2060s",
        case_id = c("morph-1", "morph-1", "morph-2"),
        path = paths,
        output_type = "actual_year",
        weather_year = c(2060L, 2061L, 2060L)
    )
    transform <- daily_transform("epwshiftr")
    morphed <- shift_stage__new(
        ShiftMorphed,
        "morphed",
        meta = list(transform = transform)
    )
    stage <- shift_stage__new(
        ShiftOutputs,
        "outputs",
        store_path = tempdir(),
        meta = list(outputs = outputs, morphed = morphed)
    )
    run <- shift_stage__new(
        ShiftRun,
        "run",
        store_path = tempdir(),
        ids = list(run_id = "staged-run"),
        meta = list(
            run = data.table::data.table(
                task = "collect",
                status = "completed",
                spec_json = '{"task":"collect"}'
            ),
            cases = data.table::data.table()
        )
    )
    # Isolate result restoration; exercise real public grouping and EPW reads.
    testthat::local_mocked_bindings(shift_result = function(...) stage)
    summary <- shift_summary(
        run,
        refresh = FALSE,
        weather = TRUE,
        ui = shift_ui("none")
    )
    data.table::setorder(summary, model)
    expect_equal(nrow(summary), 2L)
    expect_identical(summary$model, c("Model-A", "Model-B"))
    expect_identical(summary$scenario, c("ssp126", "ssp585"))
    expect_true(all(
        summary$member == "r1i1p1f1" &
            summary$grid == "gn" &
            summary$period == "2060s"
    ))
    expect_true(all(
        summary$method == transform@method & summary$scale == transform@scale
    ))
    expect_true(all(summary$reconstruction == transform@reconstruction))
    expect_equal(summary$cases, c(1L, 1L))
    expect_equal(summary$completed_cases, c(1L, 1L))
    expect_equal(summary$epw_files, c(2L, 1L))
    expect_equal(summary$mean_temperature_c, c(15, 40))
    expect_equal(summary$temperature_hours, c(2L, 1L))
})

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
