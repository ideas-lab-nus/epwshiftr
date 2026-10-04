# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("historical workflow queries preserve years without exact datetime bounds", {
    reference_years <- 1995:2014
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "BCC-CSM2-MR",
            c("ssp126", "ssp585"),
            index_nodes = "https://example.org"
        ),
        periods = list(`2060s` = 2055:2065),
        transform = monthly_transform("original_morphing"),
        reference = historical_reference(reference_years),
        dir = tempfile("historical-query-output-"),
        store = tempfile("historical-query-store-"),
        dry_run = TRUE
    )@meta$children[[1L]]
    request <- shift_resolve__historical_request(plan, "https://example.org")
    query <- shift_resolve__as_esg_query(request)

    expect_null(request@meta$time)
    expect_equal(plan@meta$reference@periods$year, reference_years)
    expect_false(grepl("datetime_start|datetime_stop", query$url()))

    # Real monthly CMIP6 Dataset metadata uses representative mid-month
    # timestamps. A December 16 endpoint still covers the calendar year 2014.
    variables <- epw_morph_variables(plan@meta$recipe)
    reference_catalog <- data.table::rbindlist(
        lapply(variables, function(variable) {
            docs <- esgf_test__file_docs(
                sprintf("historical_%s.nc", variable),
                variable_id = variable,
                datetime_start = "1850-01-16T12:00:00Z",
                datetime_end = "2014-12-16T12:00:00Z"
            )
            docs$source_id <- "BCC-CSM2-MR"
            docs$experiment_id <- "historical"
            docs$frequency <- "mon"
            docs$table_id <- "Amon"
            docs$grid_label <- "gn"
            docs
        }),
        fill = TRUE
    )
    candidates <- shift_resolve__cmip6_candidates(
        reference_catalog,
        models = "BCC-CSM2-MR",
        experiments = "historical",
        variables = variables,
        years = reference_years,
        frequency = "mon",
        table = "Amon"
    )
    expect_true(any(candidates$complete))

    error <- expect_error(
        shift_resolve__resolve_cmip6_selection(
            plan,
            future_catalog = data.table::data.table(),
            reference_catalog = data.table::data.table()
        ),
        class = "epwshiftr_shift_reference_catalog_empty"
    )
    expect_match(
        conditionMessage(error),
        "Historical reference catalog is empty"
    )
    expect_match(conditionMessage(error), "1995–2014")
})

test_that("workflow File collection fills omitted ESGF times from DRS names", {
    skip_if_not_installed("duckdb")

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    docs <- esgf_test__file_docs(
        "tas_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_205501-206512.nc",
        datetime_start = NA_character_,
        datetime_end = NA_character_
    )
    shift_test__mock_collect(docs, calls)
    request <- shift_request(
        project = "CMIP6",
        source = "BCC-CSM2-MR",
        experiment = "ssp585",
        variables = "tas",
        frequency = "mon",
        time = c(2055L, 2065L),
        filters = list(table_id = "Amon", grid_label = "gn"),
        options = list(time_filter_method = "auto")
    )
    files <- shift_collect(
        request,
        store = tempfile("shift-drs-time-store-")
    )
    catalog <- shift_inspect__file_catalog(
        shift_store(files),
        shift_ids(files)$query_id
    )

    expect_false(is.na(catalog$datetime_start[[1L]]))
    expect_false(is.na(catalog$datetime_end[[1L]]))
    expect_equal(format(catalog$datetime_start[[1L]], "%Y", tz = "UTC"), "2055")
    expect_equal(format(catalog$datetime_end[[1L]], "%Y", tz = "UTC"), "2065")
})

test_that("workflow resolver resolves both File service paths", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-service-store-")
    store <- EsgStore$new(store_path)
    docs <- esgf_test__file_docs(
        "tas_day_Model_ssp585_r1i1p1f1_gn_20600101-20601231.nc"
    )
    query_id <- store$add_files(esgf_test__file_result(docs))
    store$close()
    files <- shift_stage__new(
        ShiftFiles,
        "files",
        store_path = store_path,
        ids = list(query_id = query_id),
        meta = list(
            request = shift_request(),
            dataset_count = 1L,
            file_count = 1L,
            fields = SHIFT_WORKFLOW_FILE_FIELDS
        )
    )
    resolver_calls <- 0L
    resolver_check <- NULL
    test_local_dependencies(list(
        query_result__resolve_file_services = function(
            value,
            index_node = NULL,
            check = NULL
        ) {
            resolver_calls <<- resolver_calls + 1L
            resolver_check <<- check
            list(
                result = value,
                diagnostics = data.table::data.table(
                    service = c("OPENDAP", "HTTPServer"),
                    selected = TRUE
                )
            )
        }
    ))

    resolved <- shift_resolve__resolve_file_services(files, "future")
    resolved_store <- shift_store(resolved)
    on.exit(resolved_store$close(), add = TRUE)
    catalog <- shift_inspect__file_catalog(
        resolved_store,
        resolved@ids$query_id
    )

    expect_identical(resolver_calls, 1L)
    expect_identical(resolver_check$concurrency, 32L)
    expect_identical(resolver_check$cache_seconds, 3600L)
    expect_identical(resolver_check$cache_failures_seconds, 1800L)
    expect_s7_class(resolved, ShiftFiles)
    expect_equal(nrow(catalog), 1L)
    expect_equal(
        catalog$url_opendap,
        sub("\\|.*$", "", docs$url[[1L]][[1L]])
    )
    expect_equal(
        catalog$url_download,
        sub("\\|.*$", "", docs$url[[1L]][[2L]])
    )

    shift_resolve__resolve_file_services(files, "future", refresh = TRUE)
    expect_identical(resolver_calls, 2L)
    expect_identical(resolver_check$cache_seconds, 0L)
    expect_identical(resolver_check$cache_failures_seconds, 0L)
})

test_that("resolver coverage defensively repairs cached catalogs without times", {
    catalog <- esgf_test__file_docs(
        "tas_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_205501-206512.nc",
        variable_id = "tas",
        datetime_start = NA_character_,
        datetime_end = NA_character_
    )
    catalog$source_id <- "BCC-CSM2-MR"
    catalog$experiment_id <- "ssp585"
    catalog$frequency <- "mon"
    catalog$table_id <- "Amon"
    catalog$grid_label <- "gn"
    catalog$filename <- catalog$title
    catalog$title <- NA_character_
    catalog$datetime_start <- NULL
    catalog$datetime_end <- NULL
    candidates <- shift_resolve__cmip6_candidates(
        catalog,
        models = "BCC-CSM2-MR",
        experiments = "ssp585",
        variables = "tas",
        years = 2055:2065,
        frequency = "mon",
        table = "Amon"
    )

    expect_true(candidates$complete[[1L]])
    expect_true(is.na(candidates$missing[[1L]]))
})

test_that("catalog time enrichment keeps its label fallback order", {
    catalog <- data.table::rbindlist(
        list(
            esgf_test__file_docs(
                "tas_day_Model_ssp245_r1i1p1f1_gn_20410101-20411231.nc"
            ),
            esgf_test__file_docs(
                "tas_day_Model_ssp245_r1i1p1f1_gn_20420101-20421231.nc"
            ),
            esgf_test__file_docs(
                "tas_day_Model_ssp245_r1i1p1f1_gn_20430101-20431231.nc"
            )
        ),
        fill = TRUE
    )
    catalog$filename <- c(
        "tas_day_Model_ssp245_r1i1p1f1_gn_20910101-20911231.nc",
        "tas_day_Model_ssp245_r1i1p1f1_gn_20420101-20421231.nc",
        NA_character_
    )
    catalog$esgf_id <- c(
        "tas_day_Model_ssp245_r1i1p1f1_gn_20920101-20921231.nc",
        "tas_day_Model_ssp245_r1i1p1f1_gn_20930101-20931231.nc",
        "tas_day_Model_ssp245_r1i1p1f1_gn_20430101-20431231.nc"
    )
    catalog$title[[2L]] <- NA_character_
    catalog$title[[3L]] <- ""
    catalog$datetime_start <- NULL
    catalog$datetime_end <- NULL

    repaired <- shift_resolve__catalog_fill_time_ranges(catalog)

    expect_identical(
        substr(repaired$datetime_start, 1L, 4L),
        c("2041", "2042", "2043")
    )
    expect_identical(
        substr(repaired$datetime_end, 1L, 4L),
        c("2041", "2042", "2043")
    )
})

test_that("resolver inputs are not masked by provider convenience columns", {
    catalog <- data.table::as.data.table(esgf_test__file_docs(
        "tas_day_Model-A_ssp245_r1i1p1f1_gn_20410101-20411231.nc",
        variable_id = "tas",
        datetime_start = "2041-01-01T00:00:00Z",
        datetime_end = "2041-12-31T23:59:59Z"
    ))
    catalog$source_id <- "Model-A"
    catalog$experiment_id <- "ssp245"
    catalog$variant_label <- "r1i1p1f1"
    catalog$frequency <- "day"
    catalog$table_id <- "day"
    catalog$grid_label <- "gn"
    # These provider aliases deliberately disagree with the canonical fields.
    catalog$variable <- "provider-variable"
    catalog$grid <- "provider-grid"
    identity <- data.table::data.table(
        source_id = "Model-A",
        variant_label = "r1i1p1f1"
    )

    expect_true(shift_resolve__cmip6_input_complete(
        catalog,
        identity,
        experiment = "ssp245",
        variable = "tas",
        frequency = "day",
        table = "day",
        grid = "gn",
        years = 2041L
    ))
})

test_that("resolver enforces variable-specific CMIP6 frequencies", {
    recipe <- epw_morph_recipe("hourly_kernel_qdm")
    variables <- morpher__input_variables(recipe)
    frequencies <- morpher__recipe_required_frequency(recipe)
    tables <- shift_spec__cmip6_variable_tables(variables, frequencies)
    catalog <- data.table::rbindlist(
        lapply(
            c("ssp245", "historical"),
            function(experiment) {
                data.table::rbindlist(
                    lapply(variables, function(variable) {
                        docs <- esgf_test__file_docs(
                            sprintf(
                                "%s_%s_Model-A_%s_r1i1p1f1_gn_19900101-20651231.nc",
                                variable,
                                tables[[variable]],
                                experiment
                            ),
                            variable_id = variable,
                            datetime_start = "1990-01-01T00:00:00Z",
                            datetime_end = "2065-12-31T23:59:59Z"
                        )
                        docs$source_id <- "Model-A"
                        docs$experiment_id <- experiment
                        docs$variant_label <- "r1i1p1f1"
                        docs$frequency <- frequencies[[variable]]
                        docs$table_id <- tables[[variable]]
                        docs$grid_label <- "gn"
                        docs
                    }),
                    use.names = TRUE,
                    fill = TRUE
                )
            }
        ),
        use.names = TRUE,
        fill = TRUE
    )

    candidates <- shift_resolve__cmip6_candidates(
        catalog,
        models = "Model-A",
        experiments = c("ssp245", "historical"),
        variables = variables,
        years = 2000:2001,
        frequency = frequencies,
        requirements = morpher__variable_requirements(recipe)
    )
    partitions <- shift_resolve__cmip6_partitions(candidates$partitions_json[[
        1L
    ]])

    expect_true(candidates$complete[[1L]])
    expect_identical(candidates$frequency[[1L]], "3hrPt+3hr+day")
    expect_identical(
        stats::setNames(partitions$frequency, partitions$variable_id)[
            variables
        ],
        frequencies
    )
    expect_false(any(partitions[
        variable_id %in% HOURLY_WEATHER_EXTREMA_VARIABLES,
        required
    ]))

    core_candidates <- shift_resolve__cmip6_candidates(
        catalog[!variable_id %in% HOURLY_WEATHER_EXTREMA_VARIABLES],
        models = "Model-A",
        experiments = c("ssp245", "historical"),
        variables = variables,
        years = 2000:2001,
        frequency = frequencies,
        requirements = morpher__variable_requirements(recipe)
    )
    core_partitions <- shift_resolve__cmip6_partitions(
        core_candidates$partitions_json[[1L]]
    )
    expect_true(core_candidates$complete[[1L]])
    expect_false(any(
        core_partitions$variable_id %in% HOURLY_WEATHER_EXTREMA_VARIABLES
    ))

    scalar_candidates <- shift_resolve__cmip6_candidates(
        catalog,
        models = "Model-A",
        experiments = c("ssp245", "historical"),
        variables = variables,
        years = 2000:2001,
        frequency = "3hr",
        requirements = morpher__variable_requirements(recipe)
    )
    expect_false(scalar_candidates$complete[[1L]])
})

test_that("resolver satisfies canonical hurs only from direct data or huss plus tas and ps", {
    make_catalog <- function(variables) {
        data.table::rbindlist(
            lapply(variables, function(variable) {
                docs <- esgf_test__file_docs(
                    sprintf(
                        "%s_Amon_BCC-CSM2-MR_ssp126_r1i1p1f1_gn_205501-206512.nc",
                        variable
                    ),
                    variable_id = variable,
                    datetime_start = "2055-01-01T00:00:00Z",
                    datetime_end = "2065-12-31T23:59:59Z"
                )
                docs$source_id <- "BCC-CSM2-MR"
                docs$experiment_id <- "ssp126"
                docs$frequency <- "mon"
                docs$table_id <- "Amon"
                docs$grid_label <- "gn"
                docs
            }),
            fill = TRUE
        )
    }
    requirements <- list(hurs = list("hurs", c("huss", "tas", "ps")))
    candidates <- function(variables) {
        shift_resolve__cmip6_candidates(
            make_catalog(variables),
            models = "BCC-CSM2-MR",
            experiments = "ssp126",
            variables = unique(unlist(requirements, recursive = TRUE)),
            years = 2055:2065,
            frequency = "mon",
            table = "Amon",
            requirements = requirements
        )
    }

    expect_true(candidates("hurs")$complete[[1L]])
    expect_true(candidates(c("huss", "tas", "ps"))$complete[[1L]])
    psl_only <- candidates(c("huss", "tas", "psl"))
    expect_false(psl_only$complete[[1L]])
    expect_match(psl_only$missing[[1L]], "huss\\+tas\\+ps")
})

test_that("shift_collect() uses Dataset collection before File collection", {
    skip_if_not_installed("duckdb")

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    shift_test__mock_collect(esgf_test__file_docs("tas_day.nc"), calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    store_path <- tempfile("shift-store-")
    dataset_store_path <- tempfile("shift-datasets-store-")
    dataset_output <- capture.output(
        datasets <- shift_datasets(
            req,
            store = dataset_store_path,
            ui = shift_ui("log")
        ),
        type = "message"
    )
    expect_equal(datasets$count(), 1L)
    expect_true(any(grepl("Collect Datasets", dataset_output, fixed = TRUE)))
    expect_equal(calls$values, "Dataset")
    expect_true(calls$query_reporter[[1L]])
    dataset_run <- shift_run_get(datasets)
    expect_equal(shift_status(dataset_run), "completed")
    expect_equal(dataset_run@meta$run$task[[1L]], "datasets")
    expect_equal(shift_result(dataset_run)$count(), 1L)
    expect_true(nzchar(attr(datasets, "epwshiftr.run_id", exact = TRUE)))
    expect_true(nzchar(attr(datasets, "epwshiftr.step_id", exact = TRUE)))
    dataset_event <- dataset_run@meta$events[
        message == "Querying Dataset catalog"
    ]
    dataset_details <- jsonlite::fromJSON(
        dataset_event$details_json[[1L]],
        simplifyVector = TRUE
    )
    expect_identical(dataset_details$total, 1L)
    expect_error(
        shift_collect(req, store = tempfile("shift-store-"), progress = FALSE),
        "no longer accepts"
    )

    files <- req |>
        shift_collect(store = store_path, label = "shift-test")

    expect_true(S7::S7_inherits(files, ShiftFiles))
    expect_equal(calls$values, c("Dataset", "Dataset", "File"))
    expect_true(all(calls$query_reporter))
    expect_identical(calls$file_fields[[1L]], "*")
    expect_equal(shift_status(files), "collected")
    expect_true(length(shift_ids(files)$query_id) == 1L)
    expect_true(nzchar(shift_ids(files)$run_id))
    expect_true(nzchar(shift_ids(files)$step_id))
    expect_equal(shift_status(shift_run_get(files)), "waiting")
    collect_run <- shift_run_get(files)
    collect_event <- collect_run@meta$events[
        message == "Querying Dataset catalog"
    ]
    collect_details <- jsonlite::fromJSON(
        collect_event$details_json[[1L]],
        simplifyVector = TRUE
    )
    expect_identical(collect_details$total, 2L)
    expect_error(shift_resume(files), "waiting for its next stage")
    expect_equal(nrow(data.table::as.data.table(files)), 1L)
    expect_equal(shift_datasets(files)$count(), 1L)
    file_result <- shift_files(files)
    expect_s3_class(file_result, "EsgResultFile")
    expect_equal(file_result$count(), 1L)
    expect_equal(file_result$filename, "tas_day.nc")
    expect_error(shift_files(req), "No File result")
    expect_named(
        shift_check(files, strict = TRUE),
        shift_stage__diagnostic_columns()
    )
    expect_equal(shift_status(shift_refresh(files)), "collected")

    printed <- capture.output(print(files), type = "message")
    expect_true(any(grepl("ESGF Query Result [File]", printed, fixed = TRUE)))
    expect_true(any(grepl("^[=═]{2} ESGF Query Result", printed)))
    expect_true(any(grepl("^[*•] Index Node:", printed)))
    expect_true(any(grepl("^[*•] Result count: 1", printed)))
    expect_true(any(grepl("^[*•] Fields:", printed)))
    expect_true(any(grepl("EC-Earth3", printed, fixed = TRUE)))
    expect_true(any(grepl("tas", printed, fixed = TRUE)))
    expect_false(any(grepl("<char>", printed, fixed = TRUE)))
    expect_false(any(grepl("Catalog:", printed, fixed = TRUE)))
    expect_false(any(grepl("Identity:", printed, fixed = TRUE)))

    verbose <- capture.output(
        print(files, width = 60L, verbose = TRUE),
        type = "message"
    )
    expect_true(any(grepl("Workflow", verbose, fixed = TRUE)))
    expect_true(any(grepl("Store:", verbose, fixed = TRUE)))
    expect_true(any(grepl("Hidden columns", verbose, fixed = TRUE)))

    detached <- shift_stage__new(
        ShiftFiles,
        "files",
        store_path = tempfile("missing-shift-store-"),
        ids = list(query_id = "query-detached"),
        meta = list(
            request = req,
            file_count = 1L,
            result_fields = c("source_id", "variable_id")
        )
    )
    detached_text <- capture.output(print(detached), type = "message")
    expect_true(any(grepl("Result count: 1", detached_text, fixed = TRUE)))
    expect_true(any(grepl(
        "Persisted preview unavailable",
        detached_text,
        fixed = TRUE
    )))

    capped_calls <- new.env(parent = emptyenv())
    capped_calls$values <- character()
    capped_calls$file_fields <- list()
    capped_calls$dataset_all <- logical()
    capped_calls$dataset_limit <- list()
    shift_test__mock_collect(esgf_test__file_docs("tas_day.nc"), capped_calls)
    shift_collect(
        req,
        store = tempfile("shift-capped-store-"),
        limit = 10L,
        ui = shift_ui("none")
    )
    expect_false(capped_calls$dataset_all[[1L]])
    expect_identical(capped_calls$dataset_limit[[1L]], 10L)

    store <- shift_store(files)
    store$add_files(esgf_test__file_result(esgf_test__file_docs(
        "hurs_day.nc",
        variable_id = "hurs"
    )))
    dl <- shift_download(files, run = FALSE, probe = FALSE)
    expect_identical(shift_ids(dl)$run_id, shift_ids(files)$run_id)
    expect_false(identical(shift_ids(dl)$step_id, shift_ids(files)$step_id))
    expect_equal(nrow(data.table::as.data.table(dl)), 1L)
    expect_equal(shift_datasets(dl)$count(), 1L)
    expect_equal(shift_files(dl)$filename, "tas_day.nc")

    rds <- tempfile(fileext = ".rds")
    saveRDS(files, rds)
    restored <- readRDS(rds)
    expect_equal(shift_status(restored), "collected")
    expect_equal(nrow(data.table::as.data.table(restored)), 1L)

    completed <- shift_complete(dl)
    expect_equal(shift_status(completed), "partial")
})

test_that("rejected resolver nodes remain results rather than diagnostics", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-rejected-node-store-")
    plan <- shift_future_epw(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            "BCC-CSM2-MR",
            "ssp585",
            member = "r1i1p1f1",
            grid = "gn"
        ),
        periods = list(`2060s` = 2060L),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("shift-rejected-node-output-"),
        store = store_path,
        dry_run = TRUE
    )@meta$children[[1L]]
    store_path <- plan@store_path
    run_id <- shift_job__run_register(plan)
    store <- shift_store(plan)
    on.exit(store$close(), add = TRUE)
    shift_job__run_event(
        store,
        run_id,
        "resolve",
        "rejected",
        "DKRZ rejected: missing hurs.",
        details = list(
            stage = "resolve",
            phase = "unit",
            unit_type = "index_node",
            node = INDEX_NODES[["DKRZ"]],
            future_files = 12L,
            reference_files = 4L,
            error = "missing hurs",
            outcome = "rejected"
        )
    )

    run <- shift_job__run_handle(store, run_id)
    expect_equal(nrow(shift_diagnostics(run, refresh = FALSE)), 0L)
    nodes <- shift_ui_state__ui_event_nodes(run@meta$events)
    expect_equal(nodes$node, "DKRZ")
    expect_equal(nodes$result, "coverage: missing hurs")
})

test_that("file year selection preserves an exact disjoint year union", {
    rows <- data.table::data.table(
        datetime_start = c(
            "2041-01-01T00:00:00Z",
            "2061-01-01T00:00:00Z",
            NA_character_
        ),
        datetime_end = c(
            "2060-12-31T23:59:59Z",
            "2070-12-31T23:59:59Z",
            NA_character_
        )
    )

    expect_identical(
        shift_resolve__file_year_match(rows, c(2041:2060, 2071:2090)),
        c(TRUE, FALSE, TRUE)
    )
})

test_that("resolver exhaustion preserves closest candidate and recovery semantics", {
    future <- data.table::data.table(
        source_id = "BCC-CSM2-MR",
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        frequency = "mon",
        table_id = "Amon",
        complete = FALSE,
        missing = paste(
            "ssp126/hurs: no files;",
            "ssp585/tas: missing years 2055"
        )
    )
    reference <- data.table::rbindlist(list(
        data.table::copy(future),
        data.table::copy(future)[, variant_label := "r2i1p1f1"]
    ))
    reference[, `:=`(complete = TRUE, missing = NA_character_)]
    node_diagnostic <- shift_resolve__cmip6_resolution_diagnostic(
        future,
        reference,
        "BCC-CSM2-MR",
        reference_required = TRUE
    )
    records <- list(
        list(
            node = "DKRZ",
            kind = "coverage",
            future_files = 28L,
            reference_files = 39L,
            resolution = node_diagnostic
        ),
        list(
            node = "IPSL",
            kind = "timeout",
            future_files = NA_integer_,
            reference_files = NA_integer_,
            resolution = NULL
        )
    )
    aggregate <- shift_resolve__resolver_failure_diagnostic(records)
    condition <- tryCatch(
        shift_resolve__abort_resolver_exhausted(records),
        epwshiftr_shift_resolver_exhausted = identity
    )

    expect_identical(node_diagnostic$reason, "future_incomplete")
    # The reference-only r2 identity has one generic unavailable marker, but
    # r1 is the real future near-match and must remain the diagnostic identity.
    expect_identical(node_diagnostic$closest$member, "r1i1p1f1")
    expect_match(node_diagnostic$missing[[1L]], "ssp126/hurs", fixed = TRUE)
    expect_equal(aggregate$nodes_checked, 2L)
    expect_equal(aggregate$coverage_failures, 1L)
    expect_equal(aggregate$timeout_failures, 1L)
    expect_false(aggregate$retryable)
    expect_identical(aggregate$recovery, "inspect")
    expect_s3_class(condition, "epwshiftr_shift_resolution_error")
    expect_null(conditionCall(condition))
    expect_match(conditionMessage(condition), "2 nodes checked", fixed = TRUE)
})

test_that("resolver recommends retry only when every node failure is transient", {
    records <- list(
        list(
            node = "IPSL",
            kind = "timeout",
            future_files = NA_integer_,
            reference_files = NA_integer_,
            resolution = NULL
        ),
        list(
            node = "LIU",
            kind = "network",
            future_files = NA_integer_,
            reference_files = NA_integer_,
            resolution = NULL
        )
    )
    diagnostic <- shift_resolve__resolver_failure_diagnostic(records)

    expect_true(diagnostic$retryable)
    expect_identical(diagnostic$recovery, "retry")
})

test_that("resolution evidence tolerates omitted aggregate counters", {
    evidence <- shift_print__resolution_evidence(list(
        summary = "Selection incomplete",
        closest = list(model = "BCC-CSM2-MR", member = "r1i1p1f1", grid = "gn"),
        missing = "future: ssp585/hurs"
    ))

    expect_length(evidence, 2L)
    expect_match(evidence[[1L]], "BCC-CSM2-MR/r1i1p1f1/gn", fixed = TRUE)
    expect_match(evidence[[2L]], "ssp585/hurs", fixed = TRUE)
})

test_that("CMIP6 resolver preserves explicit member/grid choices and rejects ties", {
    transform <- monthly_transform("epwshiftr")
    variables <- epw_morph_variables(transform__recipe(transform))

    # Create two otherwise equivalent non-native grids so automatic preference
    # rules cannot choose one without user input.
    catalogs <- data.table::rbindlist(
        lapply(c("gr1", "gr2"), function(grid) {
            data.table::rbindlist(
                lapply(variables, function(variable_id) {
                    docs <- esgf_test__file_docs(
                        sprintf("%s_%s.nc", variable_id, grid),
                        variable_id = variable_id
                    )
                    docs$grid_label <- grid
                    docs$frequency <- "mon"
                    docs$table_id <- "Amon"
                    docs$id <- sprintf("%s-%s", variable_id, grid)
                    docs$dataset_id <- sprintf(
                        "dataset-%s-%s",
                        variable_id,
                        grid
                    )
                    docs
                }),
                fill = TRUE
            )
        }),
        fill = TRUE
    )

    climate_spec <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        frequency = "mon",
        table = "Amon"
    )
    periods <- epw_morph_periods(`2060s` = 2060L)
    plan <- shift_plan(
        request = shift_spec__request_from_cmip6(
            climate_spec,
            periods,
            transform
        ),
        site = shift_site(epw = get_cache_epw()),
        periods = periods,
        transform = transform,
        store = tempfile("resolver-store-")
    )
    plan@meta$climate <- climate_spec
    expect_error(
        shift_resolve__resolve_cmip6_selection(plan, catalogs),
        class = "epwshiftr_shift_resolution_ambiguity"
    )

    climate_spec <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        member = "r1i1p1f1",
        grid = "gr1",
        frequency = "mon",
        table = "Amon"
    )
    periods <- epw_morph_periods(`2060s` = 2060L)
    explicit <- shift_plan(
        request = shift_spec__request_from_cmip6(
            climate_spec,
            periods,
            transform
        ),
        site = shift_site(epw = get_cache_epw()),
        periods = periods,
        transform = transform,
        store = tempfile("resolver-store-")
    )
    explicit@meta$climate <- climate_spec
    expect_equal(
        shift_resolve__resolve_cmip6_selection(explicit, catalogs)$grid_label,
        "gr1"
    )

    climate_spec <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        member = "r2i1p1f1",
        grid = "gr1",
        frequency = "mon",
        table = "Amon"
    )
    periods <- epw_morph_periods(`2060s` = 2060L)
    missing_member <- shift_plan(
        request = shift_spec__request_from_cmip6(
            climate_spec,
            periods,
            transform
        ),
        site = shift_site(epw = get_cache_epw()),
        periods = periods,
        transform = transform,
        store = tempfile("resolver-store-")
    )
    missing_member@meta$climate <- climate_spec
    expect_error(
        shift_resolve__resolve_cmip6_selection(missing_member, catalogs),
        "No complete CMIP6 member/grid candidate"
    )
})

# vim: fdm=marker :

test_that("workflow resolver removes HTTP-only files when downloads are forbidden", {
    skip_if_not_installed("duckdb")

    store_path <- tempfile("shift-opendap-only-store-")
    store <- EsgStore$new(store_path)
    docs <- esgf_test__file_docs(
        "tas_day_Model_ssp585_r1i1p1f1_gn_20600101-20601231.nc"
    )
    docs$url <- I(list(
        "https://example.org/files/tas.nc|application/netcdf|HTTPServer"
    ))
    query_id <- store$add_files(esgf_test__file_result(docs))
    store$close()
    files <- shift_stage__new(
        ShiftFiles,
        "files",
        store_path = store_path,
        ids = list(query_id = query_id),
        meta = list(
            request = shift_request(),
            dataset_count = 1L,
            file_count = 1L,
            fields = SHIFT_WORKFLOW_FILE_FIELDS
        )
    )

    test_local_dependencies(list(
        query_result__resolve_file_services = function(
            value,
            index_node = NULL,
            check = NULL
        ) {
            expect_identical(check$sample_per_node, 9L)
            expect_identical(check$concurrency, 1L)
            expect_equal(check$timeout, 20)
            list(
                result = value,
                diagnostics = data.table::data.table(
                    service = "HTTPServer",
                    selected = TRUE
                )
            )
        }
    ))

    resolved <- shift_resolve__resolve_file_services(
        files,
        "future",
        require_opendap = TRUE
    )
    resolved_store <- shift_store(resolved)
    on.exit(resolved_store$close(), add = TRUE)
    catalog <- shift_inspect__file_catalog(
        resolved_store,
        resolved@ids$query_id
    )

    expect_equal(nrow(catalog), 0L)
})
