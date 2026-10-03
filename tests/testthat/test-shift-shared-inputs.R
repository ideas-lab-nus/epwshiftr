test_that("actual optional inputs are resolved once and pinned across cities", {
    fixture <- shared_inputs_test__fixture()
    calls <- cli_shift_test_mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    # Two roles each collect Dataset + File exactly once, independent of cities.
    expect_equal(sum(calls$types == "File"), 2L)
    plan <- batch@meta$shared_plan
    expect_equal(nrow(plan$acquisitions), 6L)
    expect_equal(nrow(plan$consumers), 12L)
    expect_equal(nrow(plan$unmatched), 0L)
    expect_setequal(plan$consumers$variable_id, c("tas", "tasmin", "tasmax"))
    expect_identical(
        batch@meta$children[[1L]]@meta$shared_inputs,
        batch@meta$children[[2L]]@meta$shared_inputs
    )
    count <- length(calls$types)
    reopened <- shift_batch_get(batch@ids$batch_id, store = batch@store_path)
    expect_identical(
        shift_batch_plan__resolve_inputs(reopened)@meta$shared_plan,
        plan
    )
    # JSON is also the background worker's persisted-plan boundary.
    child <- batch@meta$children[[2L]]
    child <- shift_persist__plan_from_spec(jsonlite::fromJSON(
        shift_persist__spec_json(shift_persist__plan_spec(child)),
        simplifyVector = TRUE
    ))
    inputs <- shift_resolve__collect_resolved_inputs(child, run_id = NULL)
    expect_equal(length(calls$types), count)
    expect_equal(inputs$files@meta$file_count, 3L)
    expect_equal(inputs$reference_files@meta$file_count, 3L)
    expect_identical(
        inputs$selection,
        data.table::as.data.table(child@meta$shared_inputs$selection)
    )
    expect_identical(inputs$files@store_path, child@store_path)
    target <- shift_store(child, create = TRUE)
    on.exit(target$close(), add = TRUE)
    # The restored selection must also work without opening another child's
    # store, as required when background processes own separate databases.
    testthat::local_mocked_bindings(
        shift_store = function(...) stop("Unexpected shared database access"),
        .package = "epwshiftr"
    )
    expect_equal(
        shift_resolve__import_shared_inputs(
            child@meta$shared_inputs,
            target
        )$selection,
        inputs$selection
    )
    # Verify actual native values, bounds and grid provenance against the
    # ordinary extraction path for a selected optional variable.
    acquisition <- plan$acquisitions[
        plan$acquisitions$experiment_id == "ssp585" &
            plan$acquisitions$variable_id == "tasmin"
    ]
    consumers <- plan$consumers[
        plan$consumers$acquisition_id == acquisition$acquisition_id
    ]
    dataset <- EsgDataset$new(acquisition$url_opendap)
    dataset$open()
    on.exit(dataset$close(), add = TRUE)
    shared <- shift_batch_read__read_acquisition(
        dataset,
        acquisition,
        consumers
    )
    for (i in seq_len(nrow(consumers))) {
        ordinary <- dataset$read_region(
            "tasmin",
            lon = consumers$lon[[i]],
            lat = consumers$lat[[i]],
            method = "nearest",
            time = c(consumers$time_start[[i]], consumers$time_stop[[i]])
        )
        observed <- shared[
            shared$consumer_id == as.character(i)
        ]
        cols <- intersect(names(ordinary), names(observed))
        expect_equal(
            lapply(cols, function(column) observed[[column]]),
            lapply(cols, function(column) ordinary[[column]])
        )
        sources <- attr(shared, "grid_sources")
        sources <- sources[sources$consumer_id == as.character(i)]
        expected_sources <- attr(ordinary, "grid_sources")
        source_cols <- setdiff(
            intersect(names(sources), names(expected_sources)),
            "site_id"
        )
        expect_equal(
            lapply(source_cols, function(column) sources[[column]]),
            lapply(source_cols, function(column) expected_sources[[column]])
        )
    }
})

test_that("optional partitions require matching future and historical inputs", {
    fixture <- shared_inputs_test__fixture()
    calls <- cli_shift_test_mock_collect(fixture$docs[
        !(fixture$docs$experiment_id == "historical" &
            fixture$docs$variable_id == "tasmin")
    ])
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    expect_false("tasmin" %in% batch@meta$shared_plan$consumers$variable_id)
    expect_true("tas" %in% batch@meta$shared_plan$consumers$variable_id)
    expect_equal(nrow(batch@meta$shared_plan$unmatched), 0L)
    selected <- batch@meta$children[[1L]]@meta$shared_inputs$selection
    expect_false(
        "tasmin" %in%
            shift_resolve__selection_partition_rows(
                selected,
                "future"
            )$variable_id
    )
    expect_false(
        "tasmin" %in%
            shift_resolve__selection_partition_rows(
                selected,
                "reference"
            )$variable_id
    )
})

test_that("shared source matching respects the selected snapshot per method", {
    fixture <- shared_inputs_test__fixture()
    cli_shift_test_mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    children <- batch@meta$children
    consumers <- shift_batch_plan__consumers(children, batch@meta$manifest)
    input <- children[[1L]]@meta$shared_inputs
    store <- shift_store(input$store)
    on.exit(store$close(), add = TRUE)
    catalog <- shift_inspect__file_catalog(
        store,
        c(input$files$ids$query_id, input$reference_files$ids$query_id)
    )
    data.table::set(catalog, j = "input_id", value = input$input_id)
    distractor <- data.table::copy(catalog)
    data.table::set(distractor, j = "input_id", value = "other-selection")
    data.table::set(distractor, j = "checksum", value = "different-file")
    original <- data.table::copy(consumers)
    plan <- shift_batch_plan__shared_plan(
        data.table::rbindlist(list(catalog, distractor)),
        consumers
    )
    expect_equal(nrow(plan$acquisitions), 6L)
    expect_false("different-file" %in% plan$acquisitions$checksum)
    expect_identical(consumers, original)
    # Missing snapshots must fail locally rather than silently rediscover data.
    inputs <- children[[2L]]@meta$shared_inputs
    inputs$files$snapshot <- tempfile("missing-query-")
    target <- shift_store(children[[2L]], create = TRUE)
    on.exit(target$close(), add = TRUE)
    expect_error(shift_resolve__import_shared_inputs(inputs, target), "exist")
})


test_that("optional identity mismatches do not change required model eligibility", {
    fixture <- shared_inputs_test__fixture()
    plan <- fixture$batch@meta$children[[1L]]
    future <- fixture$docs[fixture$docs$experiment_id == "ssp585"]
    historical <- fixture$docs[fixture$docs$experiment_id == "historical"]
    original <- data.table::copy(historical)
    # Each case changes one source facet, without changing required tas data.
    for (facet in c("frequency", "table_id", "grid_label")) {
        changed <- data.table::copy(historical)
        data.table::set(
            changed,
            i = which(changed$variable_id == "tasmin"),
            j = facet,
            value = switch(
                facet,
                frequency = "mon",
                table_id = "Amon",
                grid_label = "gn"
            )
        )
        selected <- shift_resolve__resolve_cmip6_selection(
            plan,
            future,
            changed
        )
        expect_identical(selected$source_id, "EC-Earth3")
        partitions <- shift_resolve__selection_partition_rows(
            selected,
            "future"
        )
        expect_true("tas" %in% partitions$variable_id)
        expect_false("tasmin" %in% partitions$variable_id)
    }
    selected <- shift_resolve__resolve_cmip6_selection(
        plan,
        future[future$variable_id == "tas"],
        historical[historical$variable_id == "tas"]
    )
    expect_identical(
        shift_resolve__selection_partition_rows(selected)$variable_id,
        "tas"
    )
    expect_error(
        shift_resolve__resolve_cmip6_selection(plan, future, historical[0L]),
        "Historical reference catalog is empty"
    )
    expect_identical(historical, original)
})


test_that("shared resolution failures preserve child diagnostics and explicit retry", {
    fixture <- shared_inputs_test__fixture()
    calls <- cli_shift_test_mock_collect(fixture$docs[
        fixture$docs$variable_id != "tas"
    ])
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    expect_equal(nrow(batch@meta$shared_plan$acquisitions), 0L)
    expect_equal(nrow(batch@meta$shared_plan$consumers), 0L)
    count <- length(calls$types)
    for (child in batch@meta$children) {
        child <- shift_persist__plan_from_spec(jsonlite::fromJSON(
            shift_persist__spec_json(shift_persist__plan_spec(child)),
            simplifyVector = TRUE
        ))
        expect_error(
            shift_resolve__collect_resolved_inputs(child, run_id = NULL),
            class = "epwshiftr_shift_resolver_exhausted"
        )
    }
    expect_equal(length(calls$types), count)
    # A later explicit attempt can resolve corrected data; it must not be
    # permanently pinned to the first attempt's failure.
    retry <- cli_shift_test_mock_collect(fixture$docs)
    testthat::local_mocked_bindings(
        shift_job__latest_job = function(...) {
            data.table::data.table(attempt = 2L)
        }
    )
    resolved <- shift_resolve__collect_resolved_inputs(
        batch@meta$children[[1L]],
        NULL
    )
    expect_equal(resolved$files@meta$file_count, 3L)
    expect_equal(sum(retry$types == "File"), 2L)
})
