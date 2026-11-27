# Round-trip a child ShiftPlan across the JSON boundary used by workers.
# plan_test__roundtrip {{{
plan_test__roundtrip <- function(child) {
    shift_persist__plan_from_spec(jsonlite::fromJSON(
        shift_persist__spec_json(shift_persist__plan_spec(child)),
        simplifyVector = TRUE
    ))
}
# }}}

# Build a minimal File catalog row for shared-plan matching tests.
# plan_test__catalog {{{
plan_test__catalog <- function() {
    data.table::data.table(
        file_key = "file-a",
        filename = "tas_day_Model-A_ssp245_r1i1p1f1_gn_20500101-20501231.nc",
        source_id = "Model-A",
        experiment_id = "ssp245",
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        version = "v1",
        tracking_id = "tracking-a",
        checksum = "checksum-a",
        checksum_type = "SHA256",
        size = 1000,
        datetime_start = "2050-01-01T00:00:00Z",
        datetime_end = "2050-12-31T23:59:59Z",
        url_opendap = "https://example.org/a.nc",
        url_download = "https://example.org/a.nc",
        data_node = "example.org"
    )
}
# }}}

test_that("actual optional inputs are resolved once and pinned across cities", {
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    calls <- shift_test__mock_collect(fixture$docs)
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
    child <- plan_test__roundtrip(batch@meta$children[[2L]])
    inputs <- shift_resolve__collect_resolved_inputs(child, run_id = NULL)
    expect_equal(length(calls$types), count)
    expect_equal(inputs$files@meta$file_count, 3L)
    expect_equal(inputs$reference_files@meta$file_count, 3L)
    expect_identical(
        inputs$selection,
        data.table::as.data.table(child@meta$shared_inputs$selection)
    )
    expect_identical(inputs$files@store_path, child@store_path)
    target <- shift_inspect__store(child, create = TRUE)
    on.exit(target$close(), add = TRUE)
    # The restored selection must also work without opening another child's
    # store, as required when background processes own separate databases.
    testthat::local_mocked_bindings(
        shift_inspect__store = function(...) {
            stop("Unexpected shared database access")
        },
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
        observed <- shared[shared$consumer_id == as.character(i)]
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
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    shift_test__mock_collect(fixture$docs[
        !(fixture$docs$experiment_id == "historical" &
            fixture$docs$variable_id == "tasmin")
    ])
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    expect_false("tasmin" %in% batch@meta$shared_plan$consumers$variable_id)
    expect_true("tas" %in% batch@meta$shared_plan$consumers$variable_id)
    expect_equal(nrow(batch@meta$shared_plan$unmatched), 0L)
    selected <- batch@meta$children[[1L]]@meta$shared_inputs$selection
    for (partition in c("future", "reference")) {
        expect_false(
            "tasmin" %in%
                shift_resolve__selection_partition_rows(
                    selected,
                    partition
                )$variable_id
        )
    }
})

test_that("shared source matching respects the selected snapshot per method", {
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    shift_test__mock_collect(fixture$docs)
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    children <- batch@meta$children
    consumers <- shift_batch_plan__consumers(children, batch@meta$manifest)
    input <- children[[1L]]@meta$shared_inputs
    store <- shift_inspect__store(input$store)
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
    target <- shift_inspect__store(children[[2L]], create = TRUE)
    on.exit(target$close(), add = TRUE)
    expect_error(shift_resolve__import_shared_inputs(inputs, target), "exist")
})

test_that("optional identity mismatches do not change required model eligibility", {
    fixture <- shared_inputs_test__fixture()
    plan <- fixture$batch@meta$children[[1L]]
    future <- fixture$docs[fixture$docs$experiment_id == "ssp585"]
    historical <- fixture$docs[fixture$docs$experiment_id == "historical"]
    original <- data.table::copy(historical)
    # Each case changes one source facet without changing required tas data.
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
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    calls <- shift_test__mock_collect(fixture$docs[
        fixture$docs$variable_id != "tas"
    ])
    batch <- shift_batch_plan__resolve_inputs(fixture$batch)
    expect_equal(nrow(batch@meta$shared_plan$acquisitions), 0L)
    expect_equal(nrow(batch@meta$shared_plan$consumers), 0L)
    count <- length(calls$types)
    for (child in batch@meta$children) {
        expect_error(
            shift_resolve__collect_resolved_inputs(
                plan_test__roundtrip(child),
                run_id = NULL
            ),
            class = "epwshiftr_shift_resolver_exhausted"
        )
    }
    expect_equal(length(calls$types), count)
    # A later explicit attempt can resolve corrected data; it must not be
    # permanently pinned to the first attempt's failure.
    retry <- shift_test__mock_collect(fixture$docs)
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

# Exercise recovery through the same public plans and native File snapshots
# used by execution, with only remote catalog transport replaced.
test_that("batch recovery retries one failed selection for all existing runs", {
    skip_on_cran()
    fixture <- shared_inputs_test__fixture()
    shift_test__mock_collect(fixture$docs[fixture$docs$variable_id != "tas"])
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
    retry <- shift_test__mock_collect(fixture$docs)
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

test_that("shared File planning groups intervals, isolates versions, and preserves gaps", {
    catalog <- plan_test__catalog()
    consumers <- data.table::CJ(
        site_id = c("Harbin", "Guangzhou"),
        method = c("monthly", "daily"),
        sorted = FALSE
    )
    consumers[, `:=`(
        child_key = paste(site_id, method),
        demand_id = paste(site_id, method),
        source_id = catalog$source_id[[1L]],
        experiment_id = catalog$experiment_id[[1L]],
        variant_label = catalog$variant_label[[1L]],
        grid_label = catalog$grid_label[[1L]],
        variable_id = catalog$variable_id[[1L]],
        frequency = catalog$frequency[[1L]],
        table_id = catalog$table_id[[1L]],
        role = "future",
        time_start = as.POSIXct("2050-01-01", tz = "UTC"),
        time_stop = as.POSIXct("2050-12-31", tz = "UTC"),
        lon = c(126.6, 126.6, 113.3, 113.3),
        lat = c(45.8, 45.8, 23.1, 23.1),
        spatial_method = "nearest"
    )]

    original_catalog <- data.table::copy(catalog)
    original_consumers <- data.table::copy(consumers)
    plan <- shift_batch_plan__shared_plan(catalog, consumers)

    expect_equal(nrow(plan$acquisitions), 1L)
    expect_equal(nrow(plan$consumers), 4L)
    expect_equal(data.table::uniqueN(plan$consumers$acquisition_id), 1L)
    expect_setequal(plan$consumers$child_key, consumers$child_key)
    expect_equal(nrow(plan$unmatched), 0L)
    expect_identical(catalog, original_catalog)
    expect_identical(consumers, original_consumers)

    # A separate file version must retain its own acquisition identity.
    another <- data.table::copy(catalog)
    another[, `:=`(
        file_key = "file-b",
        version = "v2",
        checksum = "checksum-b"
    )]
    versions <- shift_batch_plan__shared_plan(
        data.table::rbindlist(list(catalog, another)),
        consumers
    )
    expect_equal(nrow(versions$acquisitions), 2L)

    # Enclosed and touching windows share one acquisition; a gap stays apart.
    windows <- data.table::copy(consumers)
    windows[, `:=`(
        time_start = as.POSIXct(
            c("2050-01-01", "2050-02-01", "2050-04-30", "2050-08-01"),
            tz = "UTC"
        ),
        time_stop = as.POSIXct(
            c("2050-04-30", "2050-02-28", "2050-05-31", "2050-08-31"),
            tz = "UTC"
        )
    )]
    intervals <- shift_batch_plan__shared_plan(catalog, windows)
    expect_equal(nrow(intervals$acquisitions), 2L)
    expect_equal(nrow(intervals$consumers), 4L)
    expect_setequal(
        as.character(intervals$acquisitions$time_stop),
        as.character(as.POSIXct(c("2050-05-31", "2050-08-31"), tz = "UTC"))
    )

    # Disjoint months stay separate and unmatched variables are preserved.
    gapped <- data.table::data.table(
        demand_id = c("jan", "mar", "missing"),
        child_key = c("first", "second", "third"),
        site_id = c("A", "B", "C"),
        method = "daily",
        role = "future",
        source_id = "Model-A",
        experiment_id = "ssp245",
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        variable_id = c("tas", "tas", "huss"),
        frequency = "day",
        table_id = "day",
        time_start = as.POSIXct(
            c("2050-01-01", "2050-03-01", "2050-01-01"),
            tz = "UTC"
        ),
        time_stop = as.POSIXct(
            c("2050-01-31", "2050-03-31", "2050-12-31"),
            tz = "UTC"
        ),
        lon = 110,
        lat = 30,
        spatial_method = "nearest"
    )
    gapped_plan <- shift_batch_plan__shared_plan(catalog, gapped)
    expect_equal(nrow(gapped_plan$acquisitions), 2L)
    expect_setequal(gapped_plan$consumers$demand_id, c("jan", "mar"))
    expect_identical(gapped_plan$unmatched$demand_id, "missing")
})

test_that("child demands retain site and historical source roles", {
    transform <- daily_transform("epwshiftr", reconstruction = "power")
    climate <- shift_cmip6(
        model = "EC-Earth3",
        scenarios = "ssp585",
        member = "r1i1p1f1",
        grid = "gr"
    )
    periods <- shift_spec__periods_from_input(list(mid = 2060))
    request <- shift_spec__request_from_cmip6(climate, periods, transform)
    reference <- historical_reference(1995:2014)
    epw <- get_cache_epw()
    sites <- list(
        shift_site("Harbin", lon = 126.6, lat = 45.8, epw = epw),
        shift_site("Guangzhou", lon = 113.3, lat = 23.1, epw = epw)
    )
    children <- lapply(sites, function(site) {
        child <- shift_spec__plan(
            request,
            site,
            periods,
            store = tempfile("shared-child-"),
            transform = transform,
            reference = reference
        )
        child@meta$climate <- climate
        child
    })
    manifest <- data.table::data.table(
        child_key = c("Harbin--daily", "Guangzhou--daily"),
        site_id = c("Harbin", "Guangzhou"),
        method = "epwshiftr"
    )

    demands <- shift_batch_plan__consumers(children, manifest)
    expect_s3_class(demands, "data.table")
    expect_setequal(demands$site_id, manifest$site_id)
    expect_setequal(demands$role, c("future", "historical"))
    expect_setequal(demands$experiment_id, c("ssp585", "historical"))
    expect_equal(data.table::uniqueN(demands$demand_id), nrow(demands))
    expect_equal(data.table::uniqueN(demands$child_key), 2L)
    expect_true(all(demands$spatial_method == "nearest"))

    # The discovery store is read locally and each site keeps its own link.
    discovery <- tempfile("shared-discovery-")
    store <- EsgStore$new(discovery)
    params <- query_param__as_store(list(
        project = "CMIP6",
        latest = TRUE,
        distrib = TRUE,
        limit = 10L,
        type = "File",
        format = QUERY_PARAM__FORMAT_JSON
    ))
    response <- esgf_fixture_response("file-success.json")
    response$response$docs$checksum <- "checksum-a"
    response$response$docs$checksum_type <- "SHA256"
    response$response$docs$title <- response$response$docs$id
    response$response$docs$variant_label <- "r1i1p1f1"
    files <- query_result__new(
        EsgResultFile,
        index_node = "https://example.org",
        params = params,
        result = response
    )
    store$add_files(files)
    store$close()
    plan <- shift_batch_plan__plan_from_discovery(children, manifest, discovery)
    expect_equal(nrow(plan$acquisitions), 1L)
    expect_equal(nrow(plan$consumers), 2L)
    expect_setequal(plan$consumers$site_id, manifest$site_id)
    expect_true(nrow(plan$unmatched) > 0L)
    # The batch writer and an independently collected child File row must
    # resolve to the same extraction cache key.
    cache_plan <- shift_test__cache_plan(plan$consumers[1L])
    store <- EsgStore$new(discovery, create = FALSE)
    child_file <- store$query("SELECT * FROM file_catalog LIMIT 1")
    store$close()
    expect_identical(
        store__extract_cache_path(cache_plan, plan$acquisitions[1L]),
        store__extract_cache_path(cache_plan, child_file[1L])
    )
})

test_that("empty child collection creates an empty local plan", {
    consumers <- shift_batch_plan__consumers(list(), data.table::data.table())
    plan <- shift_batch_plan__shared_plan(data.table::data.table(), consumers)
    expect_s3_class(consumers, "data.table")
    expect_equal(nrow(plan$acquisitions), 0L)
    expect_equal(nrow(plan$consumers), 0L)
    expect_equal(nrow(plan$unmatched), 0L)
})

# vim: fdm=marker :
