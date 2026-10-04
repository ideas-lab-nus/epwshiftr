# Exercise recovery through the same public plans and native File snapshots
# used by execution, with only remote catalog transport replaced.
test_that("batch recovery retries one failed selection for all existing runs", {
    fixture <- shared_inputs_test__fixture()
    cli_shift_test_mock_collect(fixture$docs[fixture$docs$variable_id != "tas"])
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    batch@meta$children <- lapply(batch@meta$children, function(child) {
        shift_batch__run_child(shift_run(child, ui = shift_ui("none")))
    })
    expect_true(all(vapply(
        batch@meta$children,
        function(child) S7::S7_inherits(child, ShiftRun),
        logical(1L)
    )))
    old_specs <- lapply(batch@meta$children, function(child) {
        child@meta$run$spec_json
    })
    retry <- cli_shift_test_mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(batch)
    expect_equal(sum(retry$types == "File"), 2L)
    expect_equal(nrow(batch@meta$shared_plan$acquisitions), 6L)
    expect_equal(
        lapply(batch@meta$children, function(child) child@meta$run$spec_json),
        old_specs
    )
    reopened <- shift_batch_get(batch@ids$batch_id, store = batch@store_path)
    expect_null(reopened@meta$children[[1L]]@meta$shared_inputs$failure)
    expect_identical(
        reopened@meta$children[[1L]]@meta$shared_inputs,
        reopened@meta$children[[2L]]@meta$shared_inputs
    )
    for (child in reopened@meta$children) {
        inputs <- shift_resolve__collect_resolved_inputs(
            shift_batch_plan__child_plan(child),
            NULL
        )
        expect_equal(inputs$files@meta$file_count, 3L)
    }
    child <- reopened@meta$children[[1L]]
    restored <- shift_batch__restore_child(list(
        run_id = child@ids$run_id,
        store_path = child@store_path,
        status = "failed",
        shared_inputs = child@meta$shared_inputs
    ))
    expect_identical(restored@meta$shared_inputs, child@meta$shared_inputs)
    expect_equal(sum(retry$types == "File"), 2L)
})

test_that("prefetch excludes completed and active consumers", {
    fixture <- shared_inputs_test__fixture()
    cli_shift_test_mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    seen <- character()
    local_mocked_bindings(
        shift_status = function(child, ...) {
            if (child@meta$site@id == "one") "completed" else "planned"
        },
        source__apply = function(jobs, ...) {
            seen <<- unlist(lapply(jobs, function(job) {
                job$consumers$site_id[!job$cached]
            }))
        }
    )
    shift_batch_window__prefetch(batch)
    expect_true(length(seen) > 0L)
    expect_identical(unique(seen), "two")
    local_mocked_bindings(shift_status = function(...) "running")
    seen <- character()
    shift_batch_window__prefetch(batch)
    expect_length(seen, 0L)
})

test_that("shared failures retain ownership and do not stop other files", {
    fixture <- shared_inputs_test__fixture()
    cli_shift_test_mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    seen <- character()
    first <- batch@meta$shared_plan$acquisitions$acquisition_id[[1L]]
    local_mocked_bindings(source__read_acquisition = function(job) {
        id <- job$acquisition$acquisition_id[[1L]]
        seen <<- c(seen, id)
        if (id == first) {
            stop("unavailable source")
        }
        1L
    })
    result <- shift_batch_window__prefetch(batch)
    expect_length(seen, 6L)
    expect_equal(as.integer(result), 5L)
    expect_length(attr(result, "failures"), 1L)
    expect_setequal(
        attr(result, "failures")[[1L]]$child_keys,
        names(batch@meta$children)
    )
})

test_that("only dependent children are blocked after shared reading", {
    fixture <- shared_inputs_test__fixture()
    batch <- fixture$batch
    first <- names(batch@meta$children)[[1L]]
    started <- character()
    local_mocked_bindings(
        shift_batch_plan__resolve_inputs = function(batch, reporter = NULL) {
            batch
        },
        shift_batch_window__prefetch = function(...) {
            structure(
                1L,
                failures = list(list(
                    file = "failed.nc",
                    message = "unavailable",
                    child_keys = first
                ))
            )
        },
        shift_run__run_one = function(child, ...) {
            started <<- c(started, child@meta$site@id)
            child
        },
        shift_batch_ui__report = function(...) NULL
    )
    result <- shift_batch__execute(batch, ui = shift_ui("none"))
    expect_identical(started, "two")
    expect_identical(result@meta[["shared_failure"]]$child_keys, first)
    expect_identical(result@meta$execution$action, c("blocked", "started"))
})

test_that("warm shared caches need no worker or payload deserialization", {
    fixture <- shared_inputs_test__fixture()
    cli_shift_test_mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    shift_batch_window__prefetch(batch)
    unlink(list.files(fixture$root, pattern = "[.]nc$", full.names = TRUE))
    local_mocked_bindings(
        store__extract_cache_read = function(...) {
            stop("unexpected payload read")
        },
        source__apply = function(jobs, ...) expect_length(jobs, 0L)
    )
    expect_equal(as.integer(shift_batch_window__prefetch(batch)), 0L)
})

test_that("cache receipts detect changed bytes before shared reuse", {
    path <- file.path(withr::local_tempdir(), "payload.rds")
    payload <- list(
        data = data.table::data.table(value = 1),
        grid_sources = data.table::data.table(),
        available_time_count = 1L,
        actual_start = Sys.time(),
        actual_end = Sys.time()
    )
    store__extract_cache_write(path, payload)
    expect_true(store__extract_cache_available(path))
    payload$data$value <- 2
    saveRDS(payload, path)
    expect_false(store__extract_cache_available(path))
    expect_null(store__extract_cache_read(path))
})

# Filtering finished children must keep the original window identity. Their
# missing caches are irrelevant, while unfinished consumers can reuse receipts.
test_that("partial resume keeps window identity without completed cache reads", {
    fixture <- shared_inputs_test__fixture()
    cli_shift_test_mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    shift_batch_window__prefetch(batch)
    paths <- unlist(lapply(
        seq_len(nrow(batch@meta$shared_plan$acquisitions)),
        function(i) {
            acquisition <- batch@meta$shared_plan$acquisitions[i]
            consumers <- batch@meta$shared_plan$consumers[
                batch@meta$shared_plan$consumers$acquisition_id ==
                    acquisition$acquisition_id
            ]
            shift_batch_window__cache_paths(acquisition, consumers)
        }
    ))
    unlink(paths)
    unlink(list.files(fixture$root, pattern = "[.]nc$", full.names = TRUE))
    local_mocked_bindings(shift_status = function(child, ...) {
        if (child@meta$site@id == "one") "completed" else "planned"
    })
    result <- shift_batch_window__prefetch(batch)
    expect_length(attr(result, "failures"), 0L)
    expect_equal(sum(file.exists(paths)), 6L)
})

# Parseable scalar JSON and incomplete field names are not cache receipts.
test_that("invalid cache receipt shapes remain cache misses", {
    path <- tempfile(fileext = ".rds")
    withr::defer(unlink(c(path, paste0(path, ".json"))))
    saveRDS(list(data = data.table::data.table()), path)
    receipt <- paste0(path, ".json")
    for (text in c("1", "null", "[]", "{}", "{bad")) {
        writeLines(text, receipt)
        expect_false(store__extract_cache_available(path))
        expect_null(store__extract_cache_read(path))
    }
    store_write_json_atomic(list(sha256_extra = checksum_file(path)), receipt)
    expect_false(store__extract_cache_available(path))
})

# vim: fdm=marker fmr=\{\{\{,#\ \}\}\} :
