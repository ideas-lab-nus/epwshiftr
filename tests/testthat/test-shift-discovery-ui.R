# Exercise the real discovery loop and Dataset stage while replacing remote
# catalogs with complete local CMIP6 records. This proves panel ownership at
# nested API boundaries instead of merely checking a fabricated final frame.
test_that("discovery owns one reporter across methods and nested catalog stages", {
    frames <- list()
    opened <- 0L
    committed <- 0L
    reporters <- list()
    withr::local_options(list(cli.width = 112L, cli.num_colors = 1L))
    testthat::local_mocked_bindings(
        shift__ui_renderer = function(...) {
            opened <<- opened + 1L
            list(draw = function(lines, compact) {
                frames[[length(frames) + 1L]] <<- cli::ansi_strip(lines)
                TRUE
            }, backend = function() "frame",
            commit = function(...) committed <<- committed + 1L,
            close = function(...) NULL,
            suspend = function(code) code())
        },
        shift_as_query = function(x) list(
            index_node = function() "https://example.org",
            collect = function(...) list(count = function() 964L)),
        shift__task_execute = function(...) stop("unexpected standalone operation"),
        shift__cmip6_coverage_catalog = function(request, store, ui, label) {
            reporter <- shift__current_reporter()
            reporters[[length(reporters) + 1L]] <<- reporter
            reporter$unit_started("Querying File catalog",
                details = list(unit_type = "catalog", catalog_role = "File"))
            rows <- data.table::CJ(source_id = request@meta$source,
                experiment_id = request@meta$experiment,
                variable_id = request@meta$variables)
            rows[, `:=`(variant_label = "r1i1p1f1", grid_label = "gn",
                frequency = "mon", table_id = "Amon", latest = TRUE,
                datetime_start = "1900-01-01T00:00:00Z",
                datetime_end = "2100-12-31T23:59:59Z")]
            reporter$unit_completed(sprintf("Indexed %d File catalog records", nrow(rows)),
                details = list(unit_type = "catalog", catalog_role = "File"))
            rows
        }, .package = "epwshiftr")
    withr::local_options(list(
        epwshiftr.cmip6.availability = function(...) {
            reporter <- shift__current_reporter()
            reporters[[length(reporters) + 1L]] <<- reporter
            shift_datasets(shift_cmip6_scenario(source = "Model-A",
                scenario = "ssp585", variables = "tas"))
            test_cmip6_availability(...)
        }, epwshiftr.cmip6.period_coverage = shift__cmip6_period_coverage))
    transforms <- shift_batch__transforms(c("original_morphing", "bws_btws"))
    transforms$epwshiftr <- monthly_transform("epwshiftr")
    reference <- shift_reference_historical(data.frame(period = "reference", year = 1973:2005))
    result <- shift_batch__discover_models(shift_cmip6(model = 3L,
        scenarios = c("ssp126", "ssp245", "ssp370", "ssp585")), transforms,
        periods = shift__periods_from_input(list(mid = 2041:2060, late = 2071:2090)),
        references = lapply(transforms, function(x) list(reference = reference)),
        store = tempfile(), ui = shift_ui("dynamic"), site = "San Francisco")
    expect_equal(nrow(result$identities), 3L)
    expect_equal(opened, 1L)
    expect_equal(committed, 1L)
    expect_true(all(vapply(reporters, identical, logical(1L), reporters[[1L]])))
    expect_null(shift__current_reporter())
    text <- paste(unlist(frames), collapse = "\n")
    expect_match(text, "San Francisco")
    expect_match(text, "3 GCMs.*4 scenarios.*3 methods")
    expect_match(text, "Future coverage")
    expect_match(text, "Historical coverage")
    expect_match(text, "1973.*2005")
    expect_match(text, "combination 2/2")
    expect_match(text, "specific humidity.*huss.*ps")
    expect_false(grepl("READY|input request|Collect CMIP6|%", text))
    final <- paste(utils::tail(frames, 1L)[[1L]], collapse = "\n")
    expect_match(final, "COMPLETED")
    expect_match(final, "Model-A.*Model-B.*Model-C")
    expect_false(grepl("Now|Cancel|Current search", final))
})

# Verify the nested File path with a real store as well: discovery bypasses the
# standalone task wrapper but must still persist catalogs and close its handle.
test_that("nested coverage collection persists files and closes its owned store", {
    calls <- cli_shift_test_mock_collect(cli_shift_test_file_docs("tas_day.nc"))
    testthat::local_mocked_bindings(
        shift__task_execute = function(...) stop("unexpected standalone operation"),
        .package = "epwshiftr")
    request <- shift_request(project = "CMIP6", experiment = "ssp585",
        variables = "tas", frequency = "day")
    root <- tempfile("discovery-owned-store-")
    reporter <- shift__reporter(shift_ui("none"))
    on.exit(reporter$close(), add = TRUE)
    catalog <- shift__with_reporter(reporter, shift__cmip6_coverage_catalog(
        request, root, shift_ui("none"), "future-coverage"))
    expect_equal(nrow(catalog), 1L)
    expect_identical(calls$types, c("Dataset", "File"))
    expect_equal(catalog$variable_id, "tas")
    # DuckDB rejects a differently configured connection while a writable
    # handle to the same database is still open in this process.
    connection <- ddb_connect(file.path(root, "manifest.duckdb"), read_only = TRUE)
    expect_s4_class(connection, "duckdb_connection")
    ddb_disconnect(connection, shutdown = TRUE)
})

# Error and interrupt paths must release the shared stack and produce exactly
# one terminal receipt, even if a node fails before any catalog is returned.
test_that("discovery restores reporter ownership after failures and interrupts", {
    for (interrupted in c(FALSE, TRUE)) {
        reporter <- NULL
        withr::local_options(list(epwshiftr.cmip6.availability = function(...) {
            reporter <<- shift__current_reporter()
            expect_null(reporter$context()$request_started_at)
            reporter$heartbeat("Waiting for catalog response",
                details = list(request_started_at = as.numeric(Sys.time()), records_received = 12L))
            if (interrupted) stop(structure(list(message = "interrupted", call = NULL),
                class = c("interrupt", "condition")))
            stop("catalog unavailable")
        }))
        transforms <- shift_batch__transforms("original_morphing")
        condition <- tryCatch(shift_batch__discover_models(shift_cmip6(model = 1L,
            scenarios = "ssp585"), transforms,
            shift__periods_from_input(list(mid = 2041:2060)), NULL, tempfile(), shift_ui("none")),
            error = function(e) e, interrupt = function(e) e)
        expect_null(shift__current_reporter())
        expect_false(is.null(reporter), info = conditionMessage(condition))
        expect_identical(reporter$snapshot()$status, if (interrupted) "cancelled" else "failed")
    }
})

# Use callback events from two real logical requests to distinguish request
# time, previous responses, received rows, and cache reuse without estimating
# overall completion from a method or node ordinal.
test_that("catalog callbacks retain truthful request and cache metrics", {
    withr::local_options(epwshiftr.query.timeout = 300)
    reporter <- shift__reporter(shift_ui("none"))
    on.exit(reporter$close(), add = TRUE)
    reporter$operation_started("collect", "Collect CMIP6")
    shift__with_query_reporter(reporter, "https://example.org", "File", {
        callback <- getOption("epwshiftr.query.progress_callback")
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
    text <- paste(shift__ui_query_lines(state, 112L), collapse = " ")
    expect_match(text, "waiting 40s")
    expect_match(text, "timeout 5m 00s")
    expect_match(text, "last response")
    expect_match(text, "1 responses.*1 cached.*15 File catalog records received")
    expect_false(grepl("%|ETA|downloaded", text))
})

# Drive the actual JSON reader twice so cache hits cannot silently bypass the
# progress callback or get counted as a second network response.
test_that("JSON catalog reads report parsed rows and cache hits separately", {
    cache <- DiskCache$new(tempfile("discovery-cache-"), prune_on_init = FALSE)
    testthat::local_mocked_bindings(cache__get = function(...) cache,
        .package = "epwshiftr")
    testthat::local_mocked_bindings(curl_fetch_memory = function(...) list(
        content = charToRaw('{"response":{"numFound":2,"docs":[{"id":"a"},{"id":"b"}]}}'),
        status_code = 200L), .package = "curl")
    events <- list()
    callback <- function(event) events[[length(events) + 1L]] <<- event
    online <- cache__read_json("https://example.org/catalog", cache = TRUE,
        progress_callback = callback)
    cached <- cache__read_json("https://example.org/catalog", cache = TRUE,
        progress_callback = callback)
    expect_equal(online$response, cached$response)
    expect_identical(vapply(events, `[[`, character(1L), "state"),
        c("started", "completed", "parsed", "cached"))
    expect_equal(events[[3L]]$records, 2L)
    expect_equal(events[[4L]]$records, 2L)
})

# Standalone receipts retain their box and one result, without duplicating the
# title, elapsed time, result text, or meaningless single-step flow rail.
test_that("standalone catalog receipts are concise and unambiguous", {
    reporter <- shift__reporter(shift_ui("none"))
    on.exit(reporter$close(), add = TRUE)
    reporter$operation_started("collect", "Collect CMIP6",
        context = list(items = c("Collect CMIP6", "input request")))
    reporter$operation_waiting("12 Dataset and 42 File catalog records indexed")
    for (width in c(48L, 60L, 80L, 112L)) {
        lines <- cli::ansi_strip(shift__ui_status_lines(reporter$snapshot(), width))
        text <- paste(lines, collapse = " ")
        expect_match(text, "CATALOG READY")
        expect_equal(sum(grepl("42 File", lines, fixed = TRUE)), 1L)
        expect_false(grepl("Flow|Now|input request|elapsed", text))
        expect_true(all(cli::ansi_nchar(lines, type = "width") <= width - 1L))
    }
})

# Reset actual output bytes so a previous application's green SGR cannot carry
# into neutral body text, while no-color users receive no new SGR escapes.
test_that("frame writes isolate ANSI state and respect no-color output", {
    output <- rawConnection(raw(), "wb")
    on.exit(close(output), add = TRUE)
    for (colors in c(1L, 256L)) {
        withr::local_options(cli.num_colors = colors)
        writes <- character()
        renderer <- ShiftFrameRenderer$new(output, backend = "frame",
            writer = function(text) writes <<- c(writes, text))
        renderer$draw(c("Neutral body", cli::col_green("COMPLETED")))
        renderer$commit()
        if (colors > 1L) {
            expect_true(startsWith(writes[[1L]], "\033[0m\rNeutral body"))
            expect_true(endsWith(writes[[1L]], "\033[0m"))
            expect_match(writes[[1L]], "\033[32mCOMPLETED", fixed = TRUE)
        } else {
            expect_false(any(grepl("\033\\[[0-9;]*m", writes)))
        }
    }
})
