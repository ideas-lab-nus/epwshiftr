# Helper fixtures for batch discovery, per-method pools, receipts, and
# multi-site workflow tests.

# batch_test__local_availability {{{
batch_test__local_availability <- function(env = parent.frame()) {
    test_local_dependencies(
        list(
            availability = test_cmip6_availability,
            shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
        ),
        .local_envir = env
    )
}
# }}}

# batch_test__child_run {{{
batch_test__child_run <- function(status = "completed") {
    shift_stage__new(
        ShiftRun,
        "run",
        store_path = "/example/child",
        ids = list(run_id = "run-child"),
        meta = list(run = data.table::data.table(status = status))
    )
}
# }}}

# batch_test__child_ref {{{
batch_test__child_ref <- function(status = "completed") {
    list(
        run_id = "run-child",
        store_path = "/example/child",
        status = status
    )
}
# }}}

# batch_test__catalog {{{
batch_test__catalog <- function(
    sources = c("Model-A", "Model-B", "Model-C"),
    experiments = "ssp585",
    start = "2041-01-01T00:00:00Z",
    end = "2090-12-31T23:59:59Z",
    variable = "tas",
    frequency = "day",
    table_id = "day",
    grid_label = "gn"
) {
    rows <- data.table::CJ(
        source_id = sources,
        experiment_id = experiments,
        unique = TRUE
    )
    rows[, `:=`(
        variant_label = "r1i1p1f1",
        frequency = frequency,
        table_id = table_id,
        variable_id = variable,
        grid_label = grid_label,
        latest = TRUE,
        retracted = FALSE,
        deprecated = FALSE,
        datetime_start = start,
        datetime_end = end
    )]
    rows
}
# }}}

# batch_test__period_coverage {{{
batch_test__period_coverage <- function(
    candidates,
    scenarios = "ssp585",
    transform = daily_transform("epwshiftr", reconstruction = "power"),
    variables = "tas",
    frequency = c(tas = "day"),
    periods = shift_spec__periods_from_years(2050L),
    reference = NULL
) {
    shift_resolve__cmip6_period_coverage(
        candidates = candidates,
        climate = shift_cmip6(model = NULL, scenarios = scenarios),
        transform = transform,
        variables = variables,
        frequency = frequency,
        periods = periods,
        reference = reference,
        node = "https://example.org",
        store = tempfile("coverage-store-"),
        ui = shift_ui(progress = "none")
    )
}
# }}}

# Construct real Dataset-shaped method inputs with deliberately different
# model pools. Coverage remains a separate adapter in each test.
# batch_test__pool_catalog {{{
batch_test__pool_catalog <- function() {
    monthly <- monthly_transform("original_morphing")
    variables <- monthly@required_inputs$model_future@variable_sets[[1L]]
    frequencies <- shift_spec__transform_cmip6_frequencies(monthly, variables)
    tables <- shift_spec__cmip6_variable_tables(variables, frequencies, NULL)
    monthly_rows <- data.table::CJ(
        source_id = c("A", "B"),
        variable_id = variables,
        experiment_id = c("ssp585", "historical")
    )
    monthly_rows[, `:=`(
        member_id = "r1i1p1f1",
        grid_label = "gn",
        frequency = unname(frequencies[variable_id]),
        table_id = unname(tables[variable_id])
    )]
    daily_rows <- data.table::CJ(
        source_id = c("B", "C"),
        experiment_id = c("ssp585", "historical")
    )
    daily_rows[, `:=`(
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        member_id = "r1i1p1f1",
        grid_label = "gn"
    )]
    data.table::rbindlist(list(monthly_rows, daily_rows), use.names = TRUE)
}
# }}}

# Plan a small batch through the public entry point without any data transfer.
# batch_test__plan_pools {{{
batch_test__plan_pools <- function(
    common = TRUE,
    model = NULL,
    store = tempfile(),
    methods = c("original_morphing", "isimip3basd")
) {
    shift_epw_future(
        shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(
            model = model,
            scenarios = "ssp585",
            common = common,
            index_nodes = "https://example.org/esg-search"
        ),
        periods = list(mid = 2050L),
        methods = methods,
        calibration = shift_era5(years = 1995:2014),
        dir = tempfile("batch-pool-output-"),
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
}
# }}}

# Mock Dataset collection and File coverage for pool-discovery tests.
# batch_test__mock_discovery {{{
batch_test__mock_discovery <- function(
    collect_fn,
    coverage = test_cmip6_period_coverage,
    .env = parent.frame()
) {
    test_local_dependencies(
        list(
            availability = NULL,
            shift_resolve__cmip6_period_coverage = coverage
        ),
        .local_envir = .env
    )
    testthat::local_mocked_bindings(
        availability__collect = collect_fn,
        .package = "epwshiftr",
        .env = .env
    )
}
# }}}

# Run shift_batch__discover_candidates() with test defaults.
# batch_test__discover {{{
batch_test__discover <- function(
    climate,
    transforms = shift_batch__transforms(methods = "qdm"),
    references = NULL,
    periods = shift_spec__periods_from_years(2050L)
) {
    shift_batch__discover_candidates(
        climate,
        transforms,
        periods,
        references,
        tempfile(),
        shift_ui(progress = "none")
    )
}
# }}}

# Build per-transform reference specs with common historical/ERA5 defaults.
# batch_test__refs {{{
batch_test__refs <- function(
    transforms,
    reference = historical_reference(1995:2014),
    calibration = shift_era5(1995:2014)
) {
    lapply(
        transforms,
        shift_batch__references,
        reference = reference,
        calibration = calibration
    )
}
# }}}

# Serialize test inputs through the same JSON boundary as user configs.
# batch_test__write_config {{{
batch_test__write_config <- function(config) {
    path <- tempfile(fileext = ".json")
    jsonlite::write_json(config, path, auto_unbox = TRUE, null = "null")
    path
}
# }}}

# Create a genuine second EPW header while retaining deterministic weather data.
# batch_test__epw {{{
batch_test__epw <- function() {
    path <- tempfile(fileext = ".epw")
    lines <- readLines(get_cache_epw())
    header <- strsplit(lines[[1L]], ",", fixed = TRUE)[[1L]]
    header[c(2L, 7L, 8L, 9L, 10L)] <- c(
        "Second city",
        "45.75",
        "126.63",
        "8",
        "140"
    )
    lines[[1L]] <- paste(header, collapse = ",")
    writeLines(lines, path)
    path
}
# }}}

# Keep discovery deterministic while exercising multi-site public planning.
# batch_test__plan_sites {{{
batch_test__plan_sites <- function(
    sites,
    store = tempfile("multi-site-store-"),
    ...
) {
    shift_epw_future(
        sites = sites,
        climate = shift_cmip6(model = 1L, scenarios = c("ssp126", "ssp585")),
        periods = list(mid = 2050:2051),
        methods = c("original_morphing", "bws_btws"),
        dir = tempfile("multi-site-output-"),
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none"),
        ...
    )
}
# }}}

# batch_test__cmip6_docs {{{
batch_test__cmip6_docs <- function(variables, files) {
    docs <- data.table::rbindlist(
        lapply(variables, function(variable) {
            esgf_test__file_docs(
                basename(files[[variable]]),
                opendap_url = files[[variable]],
                download_url = files[[variable]],
                variable_id = variable,
                frequency = "mon",
                table_id = "Amon"
            )
        }),
        fill = TRUE
    )
    vars <- docs$variable_id
    updates <- list(
        dataset_id = paste0("future-", vars),
        master_id = paste0("future-", vars),
        instance_id = paste0("future-", vars, ".v20260101"),
        tracking_id = paste0("hdl:21.14100/future-", vars),
        id = paste0(docs$title, "|future-", vars),
        checksum = vapply(
            vars,
            function(v) store_hash_file(files[[v]], "sha256"),
            character(1L)
        )
    )
    for (col in names(updates)) {
        data.table::set(docs, j = col, value = updates[[col]])
    }
    docs
}
# }}}

test_that("method keys resolve through the transform registry without materializing the public catalog", {
    testthat::local_mocked_bindings(
        weather_transforms = function() {
            stop("Public transform catalog was materialized.")
        },
        .package = "epwshiftr"
    )

    transforms <- shift_batch__transforms(c(
        "original_morphing",
        "bws_btws",
        "isimip3basd"
    ))

    expect_identical(
        unname(vapply(transforms, function(value) value@method, character(1L))),
        c("original_morphing", "bws_btws", "isimip3basd")
    )
    expect_identical(
        unname(vapply(transforms, function(value) value@recipe, character(1L))),
        c(
            "original_morphing_monthly",
            "bws_btws_monthly",
            "isimip3basd_daily_temperature"
        )
    )
    expected <- list(
        monthly_transform("original_morphing"),
        monthly_transform("bws_btws"),
        daily_transform("isimip3basd")
    )
    expect_identical(
        unname(lapply(transforms, transform__spec_value)),
        lapply(expected, transform__spec_value)
    )
    expect_error(
        shift_batch__transforms("epwshiftr"),
        "more than one configuration"
    )
    expect_error(
        shift_batch__transforms(character()),
        "unique, non-empty method keys"
    )
    expect_error(
        shift_batch__transforms("missing_method"),
        "Unknown weather method"
    )
})

test_that("shift_run dispatches a dry-run batch to the batch runner", {
    batch <- shift_stage__new(
        ShiftBatch,
        "batch",
        meta = list(children = list(), manifest = data.table::data.table())
    )
    testthat::local_mocked_bindings(
        shift_batch__run = function(x, background, ui) "started",
        .package = "epwshiftr"
    )

    expect_identical(
        shift_run(batch, ui = shift_ui(progress = "none")),
        "started"
    )
})

test_that("batch child failures return their durable run handles", {
    failed <- structure(
        list(
            message = "child failed",
            call = NULL,
            run_id = "run-child",
            store = "/example/store"
        ),
        class = c("epwshiftr_shift_error", "error", "condition")
    )
    testthat::local_mocked_bindings(
        shift_run_get = function(run_id, store) {
            list(run_id = run_id, store = store)
        },
        .package = "epwshiftr"
    )

    recovered <- shift_batch__run_child(stop(failed))

    expect_identical(recovered$run_id, "run-child")
    expect_identical(recovered$store, "/example/store")
})

test_that("batch child restoration verifies authoritative store state and handles locks", {
    restore_case <- function(run_status, artifacts_complete = TRUE) {
        opened <- closed <- verified <- 0L
        child_store <- new.env(parent = emptyenv())
        child_store$close <- function() {
            closed <<- closed + 1L
            invisible(NULL)
        }
        run <- batch_test__child_run(run_status)
        testthat::local_mocked_bindings(
            shift_inspect__store = function(x, create = FALSE) {
                opened <<- opened + 1L
                expect_identical(x, "/example/child")
                expect_false(create)
                child_store
            },
            shift_run_get = function(run_id, store) {
                expect_identical(run_id, "run-child")
                expect_identical(store, child_store)
                run
            },
            shift_run__run_artifacts_complete = function(store, run_id) {
                verified <<- verified + 1L
                expect_identical(store, child_store)
                expect_identical(run_id, "run-child")
                artifacts_complete
            },
            .package = "epwshiftr"
        )
        list(
            restored = shift_batch__restore_child(batch_test__child_ref()),
            run = run,
            opened = opened,
            closed = closed,
            verified = verified
        )
    }

    # Complete artifacts reuse one authoritative store and close cleanly.
    ok <- restore_case("completed", artifacts_complete = TRUE)
    expect_identical(ok$restored, ok$run)
    expect_identical(ok$opened, 1L)
    expect_identical(ok$closed, 1L)
    expect_identical(ok$verified, 1L)

    # Stale receipt hints do not override authoritative running status.
    running <- restore_case("running", artifacts_complete = TRUE)
    expect_identical(shift_status(running$restored, refresh = FALSE), "running")
    expect_identical(running$verified, 0L)

    # Missing artifacts invalidate completed child restoration.
    missing <- restore_case("completed", artifacts_complete = FALSE)
    expect_null(missing$restored)
    expect_identical(missing$closed, 1L)

    # Locked stores fall back to path-based live restoration.
    store_arguments <- list()
    live_run <- batch_test__child_run("running")
    testthat::local_mocked_bindings(
        shift_inspect__store = function(x, create = FALSE) {
            stop("manifest locked")
        },
        shift_run_get = function(run_id, store) {
            store_arguments[[length(store_arguments) + 1L]] <<- store
            live_run
        },
        .package = "epwshiftr"
    )
    expect_identical(
        shift_batch__restore_child(batch_test__child_ref()),
        live_run
    )
    expect_identical(store_arguments, list("/example/child"))
})

test_that("high-level workflows select common models and retain child plans", {
    availability_calls <- 0L
    availability <- function(...) {
        availability_calls <<- availability_calls + 1L
        test_cmip6_availability(...)
    }
    test_local_dependencies(list(
        availability = availability,
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    output <- tempfile("batch-output-")
    store <- tempfile("batch-store-")
    plan_batch <- function(
        dir = output,
        calibration = shift_era5(years = 1995:2014),
        control = shift_control()
    ) {
        shift_epw_future(
            sites = shift_site(epw = get_cache_epw()),
            climate = shift_cmip6(
                model = 2L,
                scenarios = c("ssp126", "ssp585")
            ),
            periods = list(`2050` = 2049:2050, `2080` = 2079:2080),
            methods = c("original_morphing", "bws_btws", "isimip3basd"),
            calibration = calibration,
            control = control,
            dir = dir,
            store = store,
            dry_run = TRUE,
            ui = shift_ui(progress = "none")
        )
    }

    batch <- plan_batch()

    expect_true(S7::S7_inherits(batch, ShiftBatch))
    expect_identical(shift_status(batch), "planned")
    expect_length(batch@meta$children, 6L)
    expect_identical(
        batch@meta$selected_models$source_id,
        c("Model-A", "Model-B")
    )
    expect_true(all(batch@meta$selected_models$variant_label == "r1i1p1f1"))
    expect_equal(nrow(shift_cases(batch)), 24L)
    expect_true(all(vapply(
        batch@meta$children,
        function(child) S7::S7_inherits(child, ShiftPlan),
        logical(1L)
    )))
    manifest <- batch@meta$manifest
    expect_true(all(manifest[method == "isimip3basd", calibration_used]))
    expect_false(any(manifest[method != "isimip3basd", calibration_used]))
    expect_true(all(
        file.path(shift_batch__store_root(store), "batches") ==
            dirname(dirname(dirname(manifest$store)))
    ))
    expect_silent(shift_logs(batch))
    first_discovery_calls <- availability_calls
    expect_gt(first_discovery_calls, 0L)

    # Delivery paths do not change the scientific batch identity or reusable
    # store root selected for the same EPW, climate, periods, and methods.
    second <- plan_batch(dir = tempfile("another-batch-output-"))
    expect_identical(second@ids$batch_id, batch@ids$batch_id)
    expect_identical(availability_calls, first_discovery_calls)

    # Explicit refresh bypasses the persisted batch selection without changing
    # the scientific batch identity.
    refreshed <- plan_batch(control = shift_control(refresh = TRUE))
    expect_identical(refreshed@ids$batch_id, batch@ids$batch_id)
    expect_gt(availability_calls, first_discovery_calls)

    changed_calibration <- plan_batch(
        dir = tempfile("another-batch-output-"),
        calibration = shift_era5(years = 2000:2019)
    )
    expect_false(identical(
        changed_calibration@ids$batch_id,
        batch@ids$batch_id
    ))
})

test_that("direct high-level execution builds the whole batch before running", {
    batch_test__local_availability()
    received <- NULL
    testthat::local_mocked_bindings(
        shift_batch__run = function(x, background, ui) {
            received <<- x
            "executed"
        },
        .package = "epwshiftr"
    )

    result <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(model = 1L, scenarios = "ssp585"),
        periods = list(`2050` = 2049:2050),
        transform = monthly_transform("epwshiftr"),
        dir = tempfile("direct-batch-output-"),
        store = tempfile("direct-batch-store-"),
        dry_run = FALSE,
        ui = shift_ui(progress = "none")
    )

    expect_identical(result, "executed")
    expect_true(S7::S7_inherits(received, ShiftBatch))
    expect_true(all(vapply(
        received@meta$children,
        function(child) S7::S7_inherits(child, ShiftPlan),
        logical(1L)
    )))
})

test_that("batch discovery keeps r1i1p1f1 as a hard default", {
    test_local_dependencies(list(
        availability = NULL,
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    climate <- shift_cmip6(
        model = 1L,
        scenarios = "ssp585",
        member = "r2i1p1f1"
    )

    expect_error(
        shift_batch_ui__discover_models(
            climate,
            shift_batch__transforms("isimip3basd"),
            shift_spec__periods_from_years(2050L),
            references = NULL,
            store = tempfile("discovery-"),
            ui = shift_ui(progress = "none")
        ),
        "requires member `r1i1p1f1`"
    )
})

test_that("batch discovery applies historical coverage per method", {
    calls <- list()
    collect <- function(
        variables,
        scenarios,
        include_historical,
        member,
        frequency,
        index_node,
        ...
    ) {
        calls[[length(calls) + 1L]] <<- list(
            variables = variables,
            include_historical = include_historical
        )
        test_cmip6_availability(
            variables = variables,
            scenarios = scenarios,
            member = member,
            frequency = frequency,
            index_node = index_node
        )
    }
    test_local_dependencies(list(
        availability = collect,
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    transforms <- list(
        monthly = monthly_transform("epwshiftr"),
        daily = daily_transform("isimip3basd")
    )
    references <- list(
        monthly = list(reference = NULL),
        daily = list(reference = historical_reference(1995:2014))
    )
    shift_batch_ui__discover_models(
        shift_cmip6(model = 1L, scenarios = "ssp585"),
        transforms,
        shift_spec__periods_from_years(2050L),
        references,
        tempfile("discovery-"),
        shift_ui(progress = "none")
    )

    monthly_calls <- Filter(function(value) length(value$variables) > 1L, calls)
    daily_calls <- Filter(
        function(value) identical(value$variables, "tas"),
        calls
    )
    expect_true(length(monthly_calls) > 0L)
    expect_true(all(
        !vapply(
            monthly_calls,
            `[[`,
            logical(1L),
            "include_historical"
        )
    ))
    expect_true(length(daily_calls) > 0L)
    expect_true(all(vapply(
        daily_calls,
        `[[`,
        logical(1L),
        "include_historical"
    )))
})

test_that("NULL model selection retains every compatible common model", {
    batch_test__local_availability()
    transforms <- shift_batch__transforms("isimip3basd")
    references <- stats::setNames(
        list(list(reference = historical_reference(1995:2014))),
        names(transforms)
    )
    selection <- shift_batch_ui__discover_models(
        shift_cmip6(model = NULL, scenarios = "ssp585"),
        transforms,
        shift_spec__periods_from_years(2050L),
        references,
        tempfile("discovery-"),
        shift_ui(progress = "none")
    )

    expect_identical(
        selection$identities$source_id,
        c("Model-A", "Model-B", "Model-C")
    )
})

test_that("numeric model selection prefers less fragmented complete inputs", {
    test_local_dependencies(list(
        availability = test_cmip6_availability,
        shift_resolve__cmip6_period_coverage = function(candidates, ...) {
            candidates[,
                source_file_count := c(
                    `Model-A` = 1200,
                    `Model-B` = 120,
                    `Model-C` = 240
                )[source_id]
            ]
            candidates
        }
    ))
    selection <- shift_batch_ui__discover_models(
        shift_cmip6(model = 2L, scenarios = "ssp585"),
        transforms = list(monthly = monthly_transform("original_morphing")),
        periods = shift_spec__periods_from_years(2050L),
        references = NULL,
        store = tempfile("fragment-ranking-store-"),
        ui = shift_ui(progress = "none")
    )

    expect_identical(selection$identities$source_id, c("Model-B", "Model-C"))
    expect_identical(selection$identities$source_file_count, c(120, 240))
})

test_that("batch discovery applies period coverage before model counts", {
    coverage_calls <- list()
    test_local_dependencies(list(
        availability = test_cmip6_availability,
        shift_resolve__cmip6_period_coverage = function(
            candidates,
            periods,
            ...
        ) {
            coverage_calls[[length(coverage_calls) + 1L]] <<- sort(unique(
                periods$year
            ))
            candidates[source_id != "Model-B"]
        }
    ))

    batch <- shift_epw_future(
        sites = shift_site(epw = get_cache_epw()),
        climate = shift_cmip6(model = 2L, scenarios = c("ssp126", "ssp585")),
        periods = list(`2050` = 2041:2060, `2080` = 2071:2090),
        methods = "bws_btws",
        dir = tempfile("period-aware-output-"),
        store = tempfile("period-aware-store-"),
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )

    expect_identical(
        batch@meta$selected_models$source_id,
        c("Model-A", "Model-C")
    )
    expect_true(length(coverage_calls) > 0L)
    expect_identical(coverage_calls[[1L]], c(2041:2060, 2071:2090))
})

test_that("File coverage rejects gaps inside requested CMIP6 periods", {
    candidates <- data.table::as.data.table(test_cmip6_availability(
        variables = "tas",
        scenarios = c("ssp126", "ssp585"),
        member = "r1i1p1f1",
        frequency = c(tas = "day"),
        index_node = "https://example.org"
    ))
    catalog <- batch_test__catalog(experiments = c("ssp126", "ssp585"))
    data.table::set(
        catalog,
        i = which(catalog$source_id == "Model-B"),
        j = "datetime_end",
        value = "2050-12-31T23:59:59Z"
    )
    # Model-B resumes after an internal 2051 gap. Additional 2051 records use
    # incompatible member, table, or grid identities and therefore must not be
    # combined to manufacture complete coverage.
    model_b_tail <- batch_test__catalog(
        sources = "Model-B",
        experiments = c("ssp126", "ssp585"),
        start = "2052-01-01T00:00:00Z",
        end = "2090-12-31T23:59:59Z"
    )
    distractors <- data.table::rbindlist(
        lapply(
            list(
                list(variant_label = "r2i1p1f1"),
                list(table_id = "3hr"),
                list(grid_label = "gr")
            ),
            function(override) {
                rows <- batch_test__catalog(
                    sources = "Model-B",
                    experiments = c("ssp126", "ssp585"),
                    start = "2051-01-01T00:00:00Z",
                    end = "2051-12-31T23:59:59Z"
                )
                for (name in names(override)) {
                    data.table::set(
                        rows,
                        j = name,
                        value = rep(override[[name]], nrow(rows))
                    )
                }
                rows
            }
        ),
        use.names = TRUE,
        fill = TRUE
    )
    catalog <- data.table::rbindlist(
        list(catalog, model_b_tail, distractors),
        use.names = TRUE,
        fill = TRUE
    )
    testthat::local_mocked_bindings(
        shift_resolve__cmip6_coverage_catalog = function(...) catalog,
        .package = "epwshiftr"
    )

    covered <- batch_test__period_coverage(
        candidates,
        scenarios = c("ssp126", "ssp585"),
        periods = shift_spec__periods_from_input(list(
            `2050` = 2041:2060,
            `2080` = 2071:2090
        ))
    )

    expect_identical(covered$source_id, c("Model-A", "Model-C"))
    direct <- shift_resolve__cmip6_candidates(
        catalog,
        models = unique(candidates$source_id),
        experiments = c("ssp126", "ssp585"),
        variables = "tas",
        years = c(2041:2060, 2071:2090),
        frequency = c(tas = "day"),
        table = c(tas = "day"),
        requirements = list(tas = list("tas"))
    )
    expect_setequal(covered$source_id, direct[complete %in% TRUE]$source_id)
})

test_that("File coverage retains completion on the exact table mapping", {
    daily <- data.table::as.data.table(test_cmip6_availability(
        variables = "tas",
        scenarios = "ssp585",
        member = "r1i1p1f1",
        frequency = c(tas = "day"),
        index_node = "https://example.org",
        source = "Model-A"
    ))
    monthly <- data.table::copy(daily)
    monthly[, table_id := "Amon"]
    monthly$table <- list(c(tas = "Amon"))
    candidates <- data.table::rbindlist(
        list(monthly, daily),
        use.names = TRUE,
        fill = TRUE
    )
    catalog <- batch_test__catalog(
        sources = "Model-A",
        start = "2041-01-01T00:00:00Z",
        end = "2060-12-31T23:59:59Z"
    )
    data.table::set(catalog, j = "replica", value = FALSE)
    testthat::local_mocked_bindings(
        shift_resolve__cmip6_coverage_catalog = function(...) catalog,
        .package = "epwshiftr"
    )

    covered <- batch_test__period_coverage(
        candidates,
        periods = shift_spec__periods_from_input(list(`2050` = 2041:2060))
    )

    expect_equal(nrow(covered), 1L)
    expect_identical(covered$table[[1L]], c(tas = "day"))
})

test_that("File coverage applies the same year kernel to historical reference", {
    candidates <- data.table::as.data.table(test_cmip6_availability(
        variables = "tas",
        scenarios = "ssp585",
        member = "r1i1p1f1",
        frequency = c(tas = "day"),
        index_node = "https://example.org"
    ))
    future <- batch_test__catalog(
        experiments = "ssp585",
        start = "2050-01-01T00:00:00Z",
        end = "2050-12-31T23:59:59Z"
    )
    historical <- batch_test__catalog(
        experiments = "historical",
        start = "1995-01-01T00:00:00Z",
        end = "2014-12-31T23:59:59Z"
    )
    data.table::set(
        historical,
        i = which(historical$source_id == "Model-B"),
        j = "datetime_end",
        value = "2010-12-31T23:59:59Z"
    )
    testthat::local_mocked_bindings(
        shift_resolve__cmip6_coverage_catalog = function(request, ...) {
            if (identical(request@meta$experiment, "historical")) {
                historical
            } else {
                future
            }
        },
        .package = "epwshiftr"
    )

    covered <- batch_test__period_coverage(
        candidates,
        transform = daily_transform("isimip3basd"),
        reference = historical_reference(1995:2014)
    )

    expect_identical(covered$source_id, c("Model-A", "Model-C"))
})

# Both roles have enough years, but a shared display grid cannot hide a
# different grid in the required monthly partition.
test_that("historical File pairing checks all required grids", {
    frequency <- c(tas = "day", pr = "mon")
    candidates <- test_cmip6_availability(
        variables = names(frequency),
        scenarios = "ssp585",
        member = "r1i1p1f1",
        frequency = frequency,
        index_node = "https://example.org",
        source = "Model-A"
    )
    future <- data.table::data.table(
        source_id = "Model-A",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        variable_id = names(frequency),
        frequency = unname(frequency),
        table_id = c("day", "Amon"),
        grid_label = c("gn", "gr"),
        datetime_start = "2050-01-01T00:00:00Z",
        datetime_end = "2050-12-31T23:59:59Z"
    )
    historical <- data.table::copy(future)
    historical[, `:=`(
        experiment_id = "historical",
        datetime_start = "1995-01-01T00:00:00Z",
        datetime_end = "2014-12-31T23:59:59Z",
        grid_label = "gn"
    )]
    testthat::local_mocked_bindings(
        shift_resolve__cmip6_coverage_catalog = function(request, ...) {
            if (identical(request@meta$experiment, "historical")) {
                historical
            } else {
                future
            }
        },
        .package = "epwshiftr"
    )
    cover <- function() {
        batch_test__period_coverage(
            candidates,
            transform = daily_transform("isimip3basd"),
            variables = names(frequency),
            frequency = frequency,
            reference = historical_reference(1995:2014)
        )
    }

    expect_equal(nrow(cover()), 0L)
    data.table::set(historical, i = 2L, j = "grid_label", value = "gr")
    expect_identical(cover()$source_file_count, 4L)
})

# Use the same File records for direct resolution and for the offline ESGF
# fixture behind batch discovery. The Dataset summaries cannot reveal 2007.
test_that("single-case and batch CMIP6 evidence identify an interior historical gap", {
    root <- withr::local_tempdir()
    transform <- daily_transform("isimip3basd")
    climate <- shift_cmip6(
        "EC-Earth3",
        "ssp585",
        member = "r1i1p1f1",
        grid = "gr",
        index_nodes = "https://example.org"
    )
    periods <- epw_morph_periods(future = 2060L)
    reference <- historical_reference(1995:2014)
    observed <- shift_era5(1995:2014)
    site <- shift_site(epw = get_cache_epw())
    plan <- shift_spec__plan(
        shift_spec__request_from_cmip6(climate, periods, transform),
        site = site,
        periods = periods,
        transform = transform,
        reference = reference,
        observed_reference = observed,
        store = file.path(root, "single")
    )
    plan@meta$climate <- climate
    files <- data.table::rbindlist(lapply(seq_len(3L), function(i) {
        docs <- esgf_test__file_docs(
            c("future.nc", "historical-first.nc", "historical-last.nc")[[i]],
            datetime_start = paste0(
                c("2060", "1995", "2008")[[i]],
                "-01-01T00:00:00Z"
            ),
            datetime_end = paste0(
                c("2060", "2006", "2014")[[i]],
                "-12-31T23:59:59Z"
            )
        )
        if (i > 1L) {
            data.table::set(docs, j = "experiment_id", value = "historical")
            data.table::set(docs, j = "activity_id", value = "CMIP")
        }
        docs
    }))
    datasets <- unique(files[,
        c(
            "source_id",
            "experiment_id",
            "activity_id",
            "variable_id",
            "variant_label",
            "grid_label",
            "table_id",
            "frequency",
            "data_node"
        ),
        with = FALSE
    ])
    data.table::set(datasets, j = "id", value = datasets$experiment_id)
    data.table::set(datasets, j = "instance_id", value = datasets$id)
    testthat::local_mocked_bindings(
        query__collect = function(index_node, params, ...) {
            type <- query_param__value(params$type())
            docs <- if (identical(type, "Dataset")) datasets else files
            experiments <- shift_test__param_value(params, "experiment_id")
            if (length(experiments)) {
                docs <- docs[docs$experiment_id %in% experiments]
            }
            response <- esgf_test__response(docs)
            list(
                response = response,
                docs = response$response$docs,
                parameter = params
            )
        },
        .package = "epwshiftr"
    )

    single <- expect_error(
        shift_resolve__resolve_cmip6_selection(
            plan,
            files[experiment_id == "ssp585"],
            files[experiment_id == "historical"]
        ),
        class = "epwshiftr_shift_resolution_incomplete"
    )
    batch <- expect_error(shift_epw_future(
        sites = site,
        climate = climate,
        periods = periods,
        transform = transform,
        reference = reference,
        observed_reference = observed,
        dir = file.path(root, "outputs"),
        store = file.path(root, "batch"),
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    ))
    expect_identical(single$resolution$reason, "reference_incomplete")
    expect_match(conditionMessage(single), "missing years 2007", fixed = TRUE)
    expect_match(conditionMessage(batch), "missing years 2007", fixed = TRUE)
    expect_identical(batch$coverage[[1L]]$missing, single$resolution$missing)
})

# Model the handoff while an existing receipt is temporarily absent: the
# reader must acquire the publication lock before deciding that it is missing.
test_that("batch receipt reads wait for publication before checking existence", {
    root <- withr::local_tempdir()
    path <- shift_batch__receipt_path(root)
    receipt <- list(
        version = 1L,
        batch_id = "batch-handoff",
        discovery = list(
            identities = list(),
            candidates = list(),
            selection = list()
        ),
        manifest = data.frame(),
        children = list(),
        output_dir = root,
        status = "running",
        climate = list(),
        periods = data.frame()
    )
    acquired <- 0L
    released <- 0L
    local_mocked_bindings(manifest_acquire_lock = function(path, ...) {
        acquired <<- acquired + 1L
        saveRDS(receipt, path)
        function() {
            released <<- released + 1L
        }
    })
    expect_false(file.exists(path))
    expect_identical(shift_batch__receipt_read(root, "batch-handoff"), receipt)
    expect_identical(acquired, 1L)
    expect_identical(released, 1L)
    expect_null(shift_batch__receipt_read(root, "wrong-batch"))
    expect_identical(released, 2L)
    expect_null(shift_batch__receipt_read(
        file.path(root, "absent"),
        "batch-handoff"
    ))
    expect_identical(acquired, 2L)
})

# Receipt publication owns a separate, short lock, allowing observers to
# coordinate without holding the coordinator or child-store execution lock.
test_that("batch receipt publication holds its own lock", {
    fixture <- shared_inputs_test__fixture()
    path <- shift_batch__receipt_path(fixture$batch@store_path)
    original_rename <- base::file.rename
    observed <- FALSE
    writer <- shift_batch__receipt_write
    # Bind the filesystem observation only in this function's environment;
    # changing base::file.rename would affect unrelated package activity.
    environment(writer) <- list2env(
        list(file.rename = function(from, to) {
            expect_identical(to, path)
            expect_true(dir.exists(manifest_lock_path(path)))
            observed <<- TRUE
            original_rename(from, to)
        }),
        parent = environment(writer)
    )
    writer(fixture$batch)
    expect_true(observed)
    expect_false(dir.exists(manifest_lock_path(path)))
    expect_false(is.null(shift_batch__receipt_read(
        fixture$batch@store_path,
        fixture$batch@ids$batch_id
    )))
})

test_that("batch methods share Dataset discovery while keeping separate pools", {
    catalog <- batch_test__pool_catalog()
    original <- data.table::copy(catalog)
    calls <- list()
    batch_test__mock_discovery(function(request, ...) {
        calls[[length(calls) + 1L]] <<- request
        catalog
    })

    common <- batch_test__plan_pools()
    expect_length(calls, 1L)
    expect_identical(common@meta$manifest$model, c("B", "B"))
    expect_false(
        "batch_method_pools_differ" %in% shift_diagnostics(common)$code
    )
    expect_setequal(calls[[1L]]@meta$frequency, c("mon", "day"))
    expect_setequal(calls[[1L]]@meta$experiment, c("historical", "ssp585"))
    expect_setequal(
        calls[[1L]]@meta$variables,
        unique(unlist(lapply(
            shift_batch__transforms(c("original_morphing", "isimip3basd")),
            function(x) x@required_inputs$model_future@variable_sets
        )))
    )

    store <- tempfile("per-method-receipt-")
    batch <- batch_test__plan_pools(FALSE, store = store)
    expect_length(calls, 2L)
    expect_length(batch@meta$children, 4L)
    expect_identical(
        batch@meta$manifest[method == "original_morphing", model],
        c("A", "B")
    )
    expect_identical(
        batch@meta$manifest[method == "isimip3basd", model],
        c("B", "C")
    )
    expect_true(data.table::is.data.table(batch@meta$discovery$selection))
    expect_true("batch_method_pools_differ" %in% shift_diagnostics(batch)$code)
    expect_false(
        "batch_method_pools_differ" %in%
            shift_diagnostics(batch, severity = "error")$code
    )
    expect_identical(catalog, original)
    expect_false(identical(common@ids$batch_id, batch@ids$batch_id))

    # Receipt restoration and repeat planning reuse exactly the selected matrix.
    calls_before <- length(calls)
    restored <- shift_batch_get(batch@ids$batch_id, store = store)
    repeated <- batch_test__plan_pools(FALSE, store = store)
    expect_identical(restored@meta$manifest, batch@meta$manifest)
    expect_identical(restored@meta$climate@common, FALSE)
    expect_identical(
        repeated@meta$discovery$selection,
        batch@meta$discovery$selection
    )
    expect_identical(length(calls), calls_before)
    expect_true(
        "batch_method_pools_differ" %in% shift_diagnostics(restored)$code
    )
})

test_that("per-method selection applies counts and explicit allowlists after coverage", {
    catalog <- batch_test__pool_catalog()
    batch_test__mock_discovery(
        function(...) catalog,
        coverage = function(candidates, transform, ...) {
            # A is cheaper than B for monthly data; C is cheaper for daily data.
            candidates[,
                source_file_count := c(A = 1, B = 10, C = 1)[source_id]
            ]
            candidates
        }
    )
    bounded <- batch_test__plan_pools(FALSE, model = 1L)
    expect_setequal(bounded@meta$manifest$model, c("A", "C"))
    expect_equal(nrow(bounded@meta$manifest), 2L)
    named <- batch_test__plan_pools(FALSE, model = c("A", "C"))
    expect_identical(named@meta$manifest$model, c("A", "C"))
    expect_error(
        batch_test__plan_pools(TRUE, model = c("A", "C")),
        "No common"
    )
    expect_error(
        batch_test__plan_pools(FALSE, model = c("A", "B", "missing")),
        "lack complete coverage for any method"
    )

    # Catalogue presence does not waive missing File-year coverage for B.
    test_local_dependencies(list(
        shift_resolve__cmip6_period_coverage = function(candidates, ...) {
            candidates[source_id != "B"]
        }
    ))
    expect_error(batch_test__plan_pools(TRUE), "No common")
    expect_error(
        batch_test__plan_pools(FALSE, model = 2L),
        "Only 1 complete"
    )
    expect_setequal(
        batch_test__plan_pools(FALSE)@meta$manifest$model,
        c("A", "C")
    )
})

test_that("shared catalog failover does not repeat failed nodes or mix identity pins", {
    catalog <- batch_test__pool_catalog()
    # Extra provider rows may not override a pinned member, grid or model.
    extra <- data.table::copy(catalog)
    extra[, `:=`(
        source_id = "other",
        member_id = "r2i1p1f1",
        grid_label = "gr"
    )]
    catalog <- data.table::rbindlist(list(catalog, extra))
    calls <- character()
    batch_test__mock_discovery(function(request, ...) {
        node <- request@meta$options$index_node
        calls <<- c(calls, node)
        if (grepl("first", node)) {
            stop("Index temporarily unavailable")
        }
        catalog
    })
    transforms <- shift_batch__transforms(c("original_morphing", "isimip3basd"))
    references <- batch_test__refs(transforms, reference = NULL)
    climate <- shift_cmip6(
        model = NULL,
        scenarios = "ssp585",
        grid = "gn",
        index_nodes = c(
            "https://first.example/esg-search",
            "https://second.example/esg-search"
        ),
        common = FALSE
    )
    selected <- shift_batch_ui__discover_models(
        climate,
        transforms,
        shift_spec__periods_from_years(2050L),
        references,
        tempfile(),
        shift_ui(progress = "none")
    )
    expect_identical(calls, climate@index_nodes)
    expect_setequal(selected$identities$source_id, c("A", "B", "C"))
    expect_true(all(selected$selection$grid_label == "gn"))
    expect_true(all(selected$selection$variant_label == "r1i1p1f1"))
})

test_that("batch alternatives remain available after File coverage rejects the first", {
    transform <- monthly_transform("epwshiftr")
    sets <- transform@required_inputs$model_future@variable_sets
    expect_gt(length(sets), 1L)
    variables <- unique(unlist(sets))
    frequency <- shift_spec__transform_cmip6_frequencies(transform, variables)
    tables <- shift_spec__cmip6_variable_tables(variables, frequency, NULL)
    catalog <- data.table::data.table(
        source_id = "A",
        experiment_id = "ssp585",
        member_id = "r1i1p1f1",
        grid_label = "gn",
        variable_id = variables,
        frequency = unname(frequency),
        table_id = unname(tables)
    )
    calls <- 0L
    batch_test__mock_discovery(
        function(...) {
            calls <<- calls + 1L
            catalog
        },
        coverage = function(candidates, variables, ...) {
            if (identical(variables, as.character(sets[[1L]]))) {
                candidates[0L]
            } else {
                candidates
            }
        }
    )
    selected <- shift_batch_ui__discover_models(
        shift_cmip6(
            model = 1L,
            scenarios = "ssp585",
            index_nodes = "https://example.org/esg-search"
        ),
        list(monthly = transform),
        shift_spec__periods_from_years(2050L),
        NULL,
        tempfile(),
        shift_ui(progress = "none")
    )
    expect_identical(calls, 1L)
    expect_identical(selected$candidates$monthly$alternative, 2L)
    expect_setequal(
        selected$candidates$monthly$selected_variables[[1L]],
        sets[[2L]]
    )
})

test_that("climate serialization and current batch receipts round-trip", {
    climate <- shift_cmip6(model = NULL, scenarios = "ssp585")
    value <- shift_persist__climate_spec_value(climate)
    expect_false("common" %in% names(value))
    expect_identical(
        shift_persist__climate_spec_value(shift_persist__climate_from_spec(
            value
        )),
        value
    )
    expect_identical(shift_persist__climate_from_spec(value)@common, TRUE)
    climate@common <- FALSE
    value <- shift_persist__climate_spec_value(climate)
    roundtrip <- jsonlite::fromJSON(jsonlite::toJSON(
        value,
        auto_unbox = TRUE,
        null = "null"
    ))
    expect_identical(shift_persist__climate_from_spec(roundtrip)@common, FALSE)
    # A flag must reject strings, coercion, missing values, and vectors.
    for (flag in list("common", 1, NA, NULL, c(TRUE, FALSE))) {
        expect_error(shift_cmip6(scenarios = "ssp585", common = flag), "common")
    }
    expect_error(climate@common <- NA, "common")

    batch_test__local_availability()
    store <- tempfile()
    batch <- batch_test__plan_pools(model = 1L, store = store)
    path <- shift_batch__receipt_path(batch@store_path)
    saveRDS(readRDS(path), path)
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected discovery")
    }))
    restored <- batch_test__plan_pools(model = 1L, store = store)
    expect_identical(restored@ids$batch_id, batch@ids$batch_id)
    expect_identical(
        restored@meta$manifest[, .(method, model, member, grid)],
        batch@meta$manifest[, .(method, model, member, grid)]
    )
})

test_that("workflow configuration validates and accepts a boolean common flag", {
    config <- epwshiftr_cli_shift_example_config()
    for (value in list("common", "false", 1L, NA, c(TRUE, FALSE))) {
        config$climate$common <- value
        expect_error(
            schema_validate(
                SCHEMA_SHIFT_WORKFLOW_CONFIG,
                config,
                name = "config"
            ),
            "common"
        )
    }
    config$climate$common <- NULL
    expect_true(epwshiftr_cli_config_climate(config$climate)@common)

    # Calibration readiness is outside this configuration test; keep it
    # independent of local CDS credentials and remote services.
    local_mocked_bindings(
        shift_check = function(x, network = FALSE, ...) {
            expect_false(network)
            shift_stage__diagnostics_empty()
        },
        .package = "epwshiftr"
    )
    config$climate$common <- FALSE
    config$climate$model <- 1L
    config$methods <- c("original_morphing", "isimip3basd")
    config$transform <- NULL
    config$calibration <- list(dataset = "era5", years = 1995:2014)
    path <- batch_test__write_config(config)
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected discovery")
    }))
    result <- epwshiftr_cli(c(
        "--quiet",
        "--store",
        tempfile(),
        "shift",
        "config",
        "validate",
        "--config",
        path
    ))
    expect_identical(result$status, 0L, info = result$error)
    expect_identical(result$result$status, "valid")
    expect_identical(result$result$intent$`Common models`, FALSE)
})

# Dataset normalization and matching must scale by node, not by method/alternative.
test_that("all batch alternatives share one normalized and matched catalog", {
    catalog <- batch_test__pool_catalog()
    normalize <- eligibility__catalog
    match <- eligibility__match
    normalizations <- matches <- collections <- 0L
    local_mocked_bindings(
        availability__collect = function(...) {
            collections <<- collections + 1L
            catalog
        },
        eligibility__catalog = function(datasets) {
            normalizations <<- normalizations + 1L
            normalize(datasets)
        },
        eligibility__match = function(...) {
            matches <<- matches + 1L
            match(...)
        },
        .package = "epwshiftr"
    )
    test_local_dependencies(list(
        availability = NULL,
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    transforms <- shift_batch__transforms(
        transform = list(
            monthly_transform("original_morphing"),
            monthly_transform("epwshiftr"),
            daily_transform("qdm")
        )
    )
    references <- batch_test__refs(transforms)
    result <- batch_test__discover(
        shift_cmip6(
            model = NULL,
            scenarios = "ssp585",
            common = FALSE,
            index_nodes = "https://example.org/esg-search"
        ),
        transforms,
        references
    )
    expect_equal(nrow(result$selection), 6L)
    expect_identical(collections, 1L)
    expect_identical(normalizations, 1L)
    expect_identical(matches, 1L)
})

test_that("public and batch discovery agree on cross-period variable alternatives", {
    transform <- monthly_transform("epwshiftr")
    alternatives <- transform@required_inputs$model_future@variable_sets
    catalog <- data.table::rbindlist(list(
        data.table::data.table(
            experiment_id = "historical",
            variable_id = alternatives[[1L]]
        ),
        data.table::data.table(
            experiment_id = "ssp245",
            variable_id = alternatives[[2L]]
        )
    ))
    catalog[, `:=`(
        source_id = "Model-A",
        member_id = "r1i1p1f1",
        grid_label = "gn",
        frequency = "mon",
        table_id = "Amon"
    )]
    coverage_calls <- 0L
    batch_test__mock_discovery(
        function(...) catalog,
        coverage = function(candidates, ...) {
            coverage_calls <<- coverage_calls + 1L
            candidates
        }
    )
    public <- shift_cmip6_avail(
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_false(public$selected)
    transforms <- shift_batch__transforms(transform = transform)
    references <- batch_test__refs(transforms, calibration = NULL)
    climate <- shift_cmip6(
        model = "Model-A",
        scenarios = "ssp245",
        index_nodes = "https://example.org/esg-search"
    )
    expect_error(
        batch_test__discover(climate, transforms, references),
        "No complete"
    )
    expect_identical(coverage_calls, 0L)
    # Adding the matching historical alternative makes both entry points agree.
    historical <- data.table::copy(catalog[experiment_id == "ssp245"])
    historical[, experiment_id := "historical"]
    catalog <- data.table::rbindlist(list(catalog, historical))
    public <- shift_cmip6_avail(
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    batch <- batch_test__discover(climate, transforms, references)
    expect_true(public$selected)
    expect_identical(batch$selection$source_id, public$source_id)
    expect_identical(
        batch$candidates[[1L]]$selected_variables[[1L]],
        public$variables[[1L]]
    )
})

test_that("public and batch discovery apply the same Dataset filter precedence", {
    requests <- list()
    batch_test__mock_discovery(function(request, ...) {
        requests[[length(requests) + 1L]] <<- request
        batch_test__pool_catalog()
    })
    # Explicit selections override conflicting filters without dropping extras.
    filters <- list(
        project = "CMIP5",
        source_id = "Other",
        experiment_id = "ssp126",
        variant_label = "r2i1p1f1",
        member_id = "r2i1p1f1",
        variable_id = "pr",
        frequency = "mon",
        table_id = "Amon",
        type = "File",
        activity_id = "Other",
        grid_label = "gr",
        data_node = "other.example",
        latest = FALSE,
        replica = TRUE,
        fields = "id",
        institution_id = "Example"
    )
    original <- filters
    node <- "https://example.org/esg-search"
    shift_cmip6_avail(
        methods = "qdm",
        scenarios = "ssp585",
        source = "B",
        member = "r1i1p1f1",
        grid = "gn",
        data_node = "data.example",
        index_node = node,
        filters = filters
    )
    transforms <- shift_batch__transforms(methods = "qdm")
    references <- batch_test__refs(transforms)
    batch_test__discover(
        shift_cmip6(
            model = "B",
            scenarios = "ssp585",
            member = "r1i1p1f1",
            grid = "gn",
            data_node = "data.example",
            index_nodes = node,
            filters = filters
        ),
        transforms,
        references
    )
    expect_length(requests, 2L)
    expect_identical(requests[[1L]]@meta, requests[[2L]]@meta)
    expect_identical(filters, original)
    expect_identical(requests[[1L]]@meta$source, "B")
    expect_identical(requests[[1L]]@meta$variables, "tas")
    expect_identical(requests[[1L]]@meta$frequency, "day")
    expect_identical(
        requests[[1L]]@meta$filters,
        list(
            activity_id = c("ScenarioMIP", "CMIP"),
            grid_label = "gn",
            data_node = "data.example",
            latest = TRUE,
            replica = TRUE,
            fields = AVAILABILITY__DATASET_FIELDS,
            institution_id = "Example"
        )
    )
})

test_that("cost ranking retains the least fragmented grid before selecting models", {
    catalog <- data.table::CJ(
        source_id = c("A", "B"),
        grid_label = c("gn", "gr"),
        experiment_id = c("historical", "ssp245")
    )
    catalog[, `:=`(
        member_id = "r1i1p1f1",
        variable_id = "tas",
        frequency = "day",
        table_id = "day"
    )]
    batch_test__mock_discovery(
        function(...) catalog,
        coverage = function(candidates, ...) {
            candidates[,
                source_file_count := data.table::fifelse(
                    source_id == "B",
                    2,
                    data.table::fifelse(grid_label == "gr", 1, 100)
                )
            ]
            candidates
        }
    )
    result <- batch_test__discover(
        shift_cmip6(
            model = 1L,
            scenarios = "ssp245",
            common = FALSE,
            index_nodes = "https://example.org/esg-search"
        )
    )
    expect_identical(result$identities$source_id, "A")
    expect_identical(result$identities$grid_label, "gr")
    expect_equal(result$identities$source_file_count, 1)
})

test_that("native batch discovery respects explicit frequency and table pins", {
    catalog <- data.table::CJ(
        source_id = "A",
        experiment_id = c("historical", "ssp245"),
        frequency = c("day", "mon", NA_character_),
        table_id = c("Amon", "Eday", "day")
    )
    catalog[, `:=`(
        member_id = "r1i1p1f1",
        grid_label = "gn",
        variable_id = "tas"
    )]
    requests <- list()
    checked <- NULL
    batch_test__mock_discovery(
        function(request, ...) {
            requests[[length(requests) + 1L]] <<- request
            catalog
        },
        coverage = function(candidates, frequency, ...) {
            checked <<- frequency
            candidates
        }
    )
    result <- batch_test__discover(
        shift_cmip6(
            model = 1L,
            scenarios = "ssp245",
            frequency = "day",
            table = "Eday",
            index_nodes = "https://example.org/esg-search"
        )
    )
    expect_identical(checked, c(tas = "day"))
    expect_identical(result$candidates[[1L]]$table[[1L]], c(tas = "Eday"))
    expect_identical(requests[[1L]]@meta$frequency, "day")
    catalog <- catalog[0L]
    expect_error(
        batch_test__discover(
            shift_cmip6(
                model = 1L,
                scenarios = "ssp245",
                index_nodes = "https://example.org/esg-search"
            )
        ),
        "No complete"
    )
})

# Unused historical contracts must not eliminate future variable paths or
# frequencies before the batch's actual reference policy has been applied.
test_that("unused optional history cannot constrain batch Dataset matching", {
    transform <- monthly_transform("epwshiftr")
    catalog <- data.table::data.table(
        source_id = "Model-A",
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        variable_id = unique(unlist(
            transform@required_inputs$model_future@variable_sets
        )),
        experiment_id = "ssp245",
        frequency = "mon",
        table_id = "Amon"
    )
    requests <- list()
    local_mocked_bindings(
        availability__collect = function(request, ...) {
            requests[[length(requests) + 1L]] <<- request
            catalog
        },
        .package = "epwshiftr"
    )
    test_local_dependencies(list(availability = NULL))
    for (conflict in c("frequency", "variables")) {
        optional <- transform@optional_inputs
        if (conflict == "frequency") {
            optional$model_historical@frequencies <- "day"
        } else {
            optional$model_historical@variable_sets <- list("pr")
        }
        transform@optional_inputs <- optional
        transforms <- shift_batch__transforms(transform = transform)
        public <- shift_cmip6_avail(transform = transform, scenarios = "ssp245")
        expect_true(public$selected)
        for (reference in list(
            NULL,
            shift_reference_from_plan(
                "local-plan",
                data.table::data.table(period = "reference", year = 1995L)
            )
        )) {
            references <- stats::setNames(
                list(list(reference = reference)),
                names(transforms)
            )
            reader <- shift_batch__candidate_reader(
                shift_cmip6(model = "Model-A", scenarios = "ssp245"),
                transforms,
                references,
                tempfile(),
                shift_ui(progress = "none")
            )
            candidates <- reader(
                names(transforms)[[1L]],
                1L,
                "https://example.org"
            )
            expect_true(all(candidates$complete))
            expect_equal(nrow(candidates), 1L)
            expect_identical(tail(requests, 1L)[[1L]]@meta$experiment, "ssp245")
            expect_identical(tail(requests, 1L)[[1L]]@meta$frequency, "mon")
        }
    }
})

# Matching is shared without building public presentation rows. Identical
# future/history File checks are reused only within the current discovery call.
test_that("batch methods share File coverage and skip public summaries", {
    file_requests <- reductions <- 0L
    candidates <- shift_resolve__cmip6_candidates
    local_mocked_bindings(
        availability__collect = function(...) batch_test__pool_catalog(),
        eligibility__summarize = function(...) {
            stop("Unexpected public summary")
        },
        shift_resolve__cmip6_coverage_catalog = function(request, ...) {
            file_requests <<- file_requests + 1L
            rows <- data.table::CJ(
                source_id = request@meta$source,
                experiment_id = request@meta$experiment,
                variable_id = request@meta$variables
            )
            rows[, `:=`(
                variant_label = "r1i1p1f1",
                grid_label = "gn",
                frequency = "day",
                table_id = "day",
                latest = TRUE,
                datetime_start = "1900-01-01T00:00:00Z",
                datetime_end = "2100-12-31T23:59:59Z"
            )]
            rows
        },
        shift_resolve__cmip6_candidates = function(...) {
            reductions <<- reductions + 1L
            candidates(...)
        },
        .package = "epwshiftr"
    )
    test_local_dependencies(list(
        availability = NULL,
        shift_resolve__cmip6_period_coverage = shift_resolve__cmip6_period_coverage
    ))
    first <- batch_test__plan_pools(methods = c("qdm", "isimip3basd"))
    expect_equal(nrow(first@meta$manifest), 4L)
    expect_identical(file_requests, 2L)
    expect_identical(reductions, 2L)
    second <- batch_test__plan_pools(methods = c("qdm", "isimip3basd"))
    expect_identical(
        second@meta$discovery$selection,
        first@meta$discovery$selection
    )
    expect_identical(file_requests, 4L)
    expect_identical(reductions, 4L)
})

# Repeated candidate maps should be serialized once per distinct mapping;
# keeping their original JSON keys preserves deterministic group ordering.
test_that("candidate frequency grouping serializes distinct maps only", {
    mappings <- list(c(tas = "mon"), c(tas = "day"), c(huss = "mon"))
    rows <- data.table::data.table(
        source_id = paste0("Model-", 1:99),
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        complete = TRUE,
        frequency_spec = rep(mappings, 33L)
    )
    before <- data.table::copy(rows)
    serialize <- shift_persist__spec_json
    serialized <- list()
    checked <- list()
    local_mocked_bindings(
        shift_persist__spec_json = function(spec) {
            serialized[[length(serialized) + 1L]] <<- spec
            serialize(spec)
        },
        .package = "epwshiftr"
    )
    test_local_dependencies(list(
        shift_resolve__cmip6_period_coverage = function(
            candidates,
            climate,
            transform,
            variables,
            frequency,
            periods,
            reference,
            node,
            store,
            ui
        ) {
            checked[[length(checked) + 1L]] <<- frequency
            candidates
        }
    ))
    result <- shift_batch__available_alternative(
        shift_cmip6(scenarios = "ssp245", index_nodes = "https://example.org"),
        daily_transform("qdm"),
        "tas",
        shift_spec__periods_from_years(2050L),
        NULL,
        tempfile(),
        shift_ui(progress = "none"),
        function(node) rows
    )
    expect_length(serialized, 3L)
    expect_identical(checked, rev(mappings))
    expect_identical(
        result$source_id,
        rows$source_id[c(seq(3L, 99L, 3L), seq(2L, 99L, 3L), seq(1L, 99L, 3L))]
    )
    expect_identical(rows, before)
})

# Coverage keys include the request, exact core years, and per-variable table
# mapping. Returned data.tables must never expose the cached object by reference.
test_that("File coverage reuse respects identity and protects cached evidence", {
    calls <- 0L
    fail <- FALSE
    empty <- FALSE
    local_mocked_bindings(
        shift_resolve__cmip6_coverage_catalog = function(...) {
            calls <<- calls + 1L
            if (fail) {
                stop("File service unavailable")
            }
            data.table::data.table()
        },
        shift_resolve__cmip6_candidates = function(...) {
            result <- data.table::data.table(
                source_id = "A",
                variant_label = "r1i1p1f1",
                grid_label = "gn",
                complete = TRUE
            )
            if (empty) result[0L] else result
        },
        shift_resolve__cmip6_candidate_file_count = function(...) 1L,
        .package = "epwshiftr"
    )
    cache <- new.env(parent = emptyenv())
    request <- shift_request(
        project = "CMIP6",
        source = "A",
        experiment = "ssp245",
        variant = "r1i1p1f1",
        variables = "tas",
        frequency = c(tas = "day"),
        options = list(index_node = "https://example.org")
    )
    read <- function(
        value = request,
        years = 2041:2060,
        table = c(tas = "day")
    ) {
        shift_resolve__cmip6_coverage_candidates(
            value,
            years,
            table,
            NULL,
            shift_ui(progress = "none"),
            cache
        )
    }
    first <- read()
    expected <- data.table::copy(first)
    data.table::set(first, j = "source_file_count", value = 999L)
    expect_identical(read(years = 2060:2041), expected)
    expect_identical(calls, 1L)
    read(years = 2041:2050)
    read(table = c(tas = "Eday"))
    variants <- list(
        source = "B",
        experiment = "historical",
        variant = "r2i1p1f1",
        variables = "pr",
        frequency = c(tas = "mon"),
        time = c("2041", "2050"),
        filters = list(grid_label = "gr"),
        options = list(index_node = "https://other.example.org")
    )
    for (field in names(variants)) {
        changed <- request
        meta <- changed@meta
        meta[[field]] <- variants[[field]]
        changed@meta <- meta
        read(changed)
    }
    expect_identical(calls, 11L)
    empty <- TRUE
    result <- read(years = 2071L)
    expect_equal(nrow(result), 0L)
    expect_type(result$source_file_count, "integer")
    expect_identical(read(years = 2071L), result)
    expect_identical(calls, 12L)
    fail <- TRUE
    expect_error(read(years = 2080L), "File service unavailable")
    fail <- FALSE
    expect_equal(nrow(read(years = 2080L)), 0L)
    expect_identical(calls, 14L)
})

# Keep a failed shared source read visible in the saved batch even when no
# child run was started, so the user can inspect and resume that batch.
test_that("shared prefetch failure persists as a blocked batch", {
    batch_test__local_availability()
    root <- tempfile("blocked-shared-batch-")
    batch <- batch_test__plan_sites(
        list(shift_site("South", epw = get_cache_epw())),
        store = root
    )
    # This test isolates failure persistence after input resolution. The input
    # resolver itself is exercised with real local catalogs in plan tests.
    testthat::local_mocked_bindings(
        shift_batch_plan__resolve_inputs = function(batch, reporter = NULL) {
            batch
        },
        shift_batch_window__prefetch = function(...) {
            stop("source connection closed")
        }
    )
    expect_error(
        shift_batch_execution__resume(batch),
        "source connection closed"
    )
    restored <- shift_batch_get(batch@ids$batch_id, root)
    expect_identical(shift_status(restored), "blocked")
    expect_true(
        "batch_shared_read_failed" %in% shift_diagnostics(restored)$code
    )
    expect_length(restored@meta$shared_failure_history, 1L)
})

test_that("multiple sites share discovery and retain distinct durable plans", {
    calls <- 0L
    test_local_dependencies(list(
        availability = function(...) {
            calls <<- calls + 1L
            test_cmip6_availability(...)
        },
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    sites <- list(
        shift_site("South", epw = get_cache_epw()),
        shift_site("North", epw = batch_test__epw())
    )
    original <- sites
    root <- tempfile("multi-site-persistence-")
    batch <- batch_test__plan_sites(sites, root)
    expect_identical(sites, original)
    expect_s7_class(batch, ShiftBatch)
    manifest <- batch@meta$manifest
    expect_identical(manifest$site_id, rep(c("North", "South"), each = 2L))
    expect_identical(
        manifest$method,
        rep(c("original_morphing", "bws_btws"), 2L)
    )
    expect_equal(data.table::uniqueN(manifest$store), 4L)
    expect_equal(data.table::uniqueN(manifest$output_dir), 4L)
    expect_s3_class(batch@meta$shared_plan$acquisitions, "data.table")
    expect_s3_class(batch@meta$shared_plan$consumers, "data.table")
    expect_s3_class(batch@meta$shared_plan$unmatched, "data.table")
    expect_equal(nrow(shift_cases(batch)), 8L)
    expect_identical(unique(shift_cases(batch)$site_id), c("North", "South"))
    child_sites <- lapply(batch@meta$children, function(child) child@meta$site)
    expect_equal(child_sites[[1L]]@lon, 126.63)
    expect_equal(child_sites[[1L]]@lat, 45.75)
    expect_identical(child_sites[[1L]]@id, "North")
    expect_identical(child_sites[[3L]]@id, "South")
    expect_false(identical(child_sites[[1L]]@lon, child_sites[[3L]]@lon))

    first_calls <- calls
    single <- batch_test__plan_sites(sites[1L])
    expect_identical(calls, first_calls * 2L)
    expect_length(single@meta$children, 2L)
    # Input ordering does not invalidate the saved discovery.
    calls <- 0L
    reordered <- batch_test__plan_sites(sites[2:1], root)
    expect_identical(reordered@ids$batch_id, batch@ids$batch_id)
    expect_identical(calls, 0L)
    reopened <- shift_batch_get(batch@ids$batch_id, root)
    expect_identical(reopened@meta$manifest, reordered@meta$manifest)
    expect_identical(reopened@meta$periods, reordered@meta$periods)
    expect_identical(reopened@meta$climate, reordered@meta$climate)
    expect_identical(reopened@meta$shared_plan, reordered@meta$shared_plan)
    expect_identical(
        lapply(reopened@meta$children, shift_persist__plan_spec),
        lapply(reordered@meta$children, shift_persist__plan_spec)
    )
    expect_identical(calls, 0L)
    snapshot <- shift_batch_ui__snapshot(reopened, refresh = FALSE)
    expect_setequal(snapshot$children$site_id, c("North", "South"))
    expect_setequal(shift_summary(reopened)$site_id, c("North", "South"))
})

test_that("site objects preserve coordinates, metadata and EPW identities", {
    batch_test__local_availability()
    epw <- get_cache_epw()
    sites <- list(
        shift_site(
            "a/b",
            lon = 110,
            lat = 30,
            epw = epw,
            metadata = list(note = "target")
        ),
        shift_site("a-b", lon = 120, lat = 40, epw = epw)
    )
    batch <- batch_test__plan_sites(sites)
    expect_length(batch@meta$children, 4L)
    expect_equal(
        data.table::uniqueN(dirname(batch@meta$manifest$output_dir)),
        2L
    )
    child <- batch@meta$children[[3L]]
    expect_identical(child@meta$site@metadata, list(note = "target"))
    expect_identical(
        shift_persist__plan_from_spec(shift_persist__plan_spec(
            child
        ))@meta$site@metadata,
        list(note = "target")
    )
    original <- batch@ids$batch_id
    sites[[1L]]@lon <- 111
    expect_false(identical(
        batch_test__plan_sites(sites)@ids$batch_id,
        original
    ))
    sites[[1L]]@lon <- 110
    sites[[1L]]@id <- "changed"
    expect_false(identical(
        batch_test__plan_sites(sites)@ids$batch_id,
        original
    ))
    sites[[1L]]@id <- "a/b"
    sites[[1L]]@epw <- batch_test__epw()
    expect_false(identical(
        batch_test__plan_sites(sites)@ids$batch_id,
        original
    ))
})

test_that("site validation, coordinate defaults, and single-vs-multi batch contract stay consistent", {
    batch_test__local_availability()
    epw <- get_cache_epw()
    derived <- shift_site("A", epw = epw)
    explicit <- shift_site("A", derived@lon, derived@lat, epw = epw)
    expect_identical(explicit, derived)
    sites_dt <- shift_batch__sites(list(
        explicit,
        shift_site("B", 110, 30, epw = epw)
    ))
    expect_s3_class(sites_dt, "data.table")
    expect_identical(sites_dt$site[[2L]]@label, derived@label)
    expect_equal(sites_dt$site[[2L]]@lon, 110)

    root <- tempfile()
    single <- batch_test__plan_sites(derived, root)
    wrapped <- batch_test__plan_sites(list(derived), root)
    expect_s7_class(single, ShiftBatch)
    expect_identical(single@ids$batch_id, wrapped@ids$batch_id)
    expect_identical(single@meta$manifest$site_id, c("A", "A"))

    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected catalog access")
    }))
    expect_error(batch_test__plan_sites(epw), "shift_site")
    expect_error(
        batch_test__plan_sites(data.table::data.table(id = "A", epw = epw)),
        "shift_site"
    )
    expect_error(batch_test__plan_sites(list()), "non-empty")
    expect_error(batch_test__plan_sites(list(derived, derived)), "duplicated")
    expect_error(
        batch_test__plan_sites(shift_site(" ", epw = epw)),
        "whitespace"
    )
    expect_error(shift_site(NA_character_, epw = epw), "NA|missing")
    expect_error(
        shift_site("A", lon = NA_real_, lat = NA_real_, epw = epw),
        "NA|missing"
    )
    expect_error(shift_site("A", lat = 91, epw = epw), "<= 90")
    expect_error(
        batch_test__plan_sites(shift_site("A", 110, 30)),
        "baseline EPW"
    )
    expect_error(
        shift_site("A", epw = "/missing/baseline.epw"),
        "does not exist"
    )
})

test_that("version 3 location arrays plan and restore through the CLI", {
    batch_test__local_availability()
    config <- epwshiftr_cli_shift_example_config()
    config$version <- 3L
    config$sites <- list(
        list(id = "South", epw = config$sites[[1L]]$epw),
        list(id = "North", epw = batch_test__epw(), label = "North")
    )
    config$climate$model <- 1L
    config$dir <- tempfile("cli-multi-output-")
    path <- batch_test__write_config(config)
    root <- tempfile("cli-multi-store-")
    base <- c("--quiet", "--store", root, "shift")
    checked <- epwshiftr_cli(c(base, "config", "validate", "--config", path))
    expect_equal(checked$status, 0L, info = checked$error)
    expect_equal(checked$result$validation, "local")
    expect_identical(checked$result$sites$site_id, c("North", "South"))
    planned <- epwshiftr_cli(c(base, "run", "--config", path, "--dry-run"))
    expect_equal(planned$status, 0L, info = planned$error)
    expect_length(planned$result$batch_id, 1L)
    expect_equal(nrow(planned$result$children), 2L)
    shown <- epwshiftr_cli(c(base, "show", "--batch", planned$result$batch_id))
    expect_equal(shown$status, 0L, info = shown$error)
    expect_setequal(shown$result$children$site_id, c("North", "South"))
    config$sites <- config$sites[1L]
    expect_silent(epwshiftr_cli_read_shift_config(batch_test__write_config(
        config
    )))
    config$epw <- get_cache_epw()
    expect_error(
        epwshiftr_cli_read_shift_config(batch_test__write_config(config)),
        "epw"
    )
    config$epw <- NULL
    for (ver in c(2L, 1L)) {
        config$version <- ver
        expect_error(
            epwshiftr_cli_read_shift_config(batch_test__write_config(config)),
            "version 3"
        )
    }
})

test_that("multi-site references are resolved for each location", {
    batch_test__local_availability()
    sites <- list(
        shift_site("A", epw = get_cache_epw()),
        shift_site("B", epw = batch_test__epw())
    )
    plan <- shift_epw_future(
        sites = sites,
        climate = shift_cmip6(model = 1L, scenarios = "ssp585"),
        periods = 2050:2051,
        methods = c("original_morphing", "isimip3basd"),
        calibration = shift_era5(1995:2014),
        dir = tempfile(),
        store = tempfile(),
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
    children <- plan@meta$children
    expect_null(children[[1L]]@meta$observed_reference)
    expect_null(children[[3L]]@meta$observed_reference)
    expect_s7_class(children[[2L]]@meta$observed_reference, ShiftReanalysisSpec)
    expect_s7_class(children[[4L]]@meta$observed_reference, ShiftReanalysisSpec)
    expect_false(identical(
        children[[2L]]@meta$site@lon,
        children[[4L]]@meta$site@lon
    ))
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected catalog access")
    }))
    expect_error(
        batch_test__plan_sites(
            sites,
            reference = shift_reference_from_plan(
                "existing-plan",
                epw_morph_periods(hist = 1995:2014)
            )
        ),
        "cannot share an extracted reference plan"
    )
})

test_that("new site inputs validate EPW generation metadata before discovery", {
    path <- batch_test__epw()
    lines <- readLines(path)
    header <- strsplit(lines[[1L]], ",", fixed = TRUE)[[1L]]
    header[[9L]] <- "unknown"
    lines[[1L]] <- paste(header, collapse = ",")
    writeLines(lines, path)
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected catalog access")
    }))
    sites <- shift_site("A", epw = path)
    expect_error(batch_test__plan_sites(sites), "EPW time zone")
    header[[9L]] <- "8"
    header[[10L]] <- "unknown"
    lines[[1L]] <- paste(header, collapse = ",")
    writeLines(lines, path)
    expect_error(batch_test__plan_sites(sites), "EPW elevation")
})

test_that("multi-site batches execute locally and reuse each site's outputs", {
    skip_on_cran()
    # Fixture catalog identities must not reuse another test's disk cache.
    local_test_cache()
    withr::local_options(epwshiftr.dir_cache = withr::local_tempdir())
    test_local_dependencies(list(
        availability = function(...) {
            args <- list(...)
            args$source <- "EC-Earth3"
            do.call(test_cmip6_availability, args)
        },
        shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
    ))
    transform <- monthly_transform("epwshiftr")
    variables <- epw_morph_variables(transform__recipe(transform))
    files <- stats::setNames(
        vapply(
            variables,
            function(variable) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    2060L,
                    variable_id = variable,
                    frequency = "mon"
                )
                path
            },
            character(1L)
        ),
        variables
    )
    withr::defer(unlink(files))
    docs <- batch_test__cmip6_docs(variables, files)
    calls <- shift_test__mock_collect(docs)
    sites <- list(
        shift_site("A", lon = 103.98, lat = 1.37, epw = get_cache_epw()),
        shift_site("B", lon = 104.4, lat = 2, epw = get_cache_epw())
    )
    plan <- shift_epw_future(
        sites = sites,
        climate = shift_cmip6(
            model = 1L,
            scenarios = "ssp585",
            frequency = "mon",
            table = "Amon",
            grid = "gr",
            index_nodes = "https://example.org"
        ),
        periods = 2060,
        transform = transform,
        dir = tempfile("multi-export-"),
        store = tempfile("multi-store-"),
        dry_run = TRUE,
        control = shift_control(strict = FALSE),
        ui = shift_ui(progress = "none")
    )
    discovery_store <- EsgStore$new(file.path(plan@store_path, "discovery"))
    discovery_store$add_files(esgf_test__file_result(docs))
    discovery_store$close()
    plan@meta$shared_plan <- shift_batch_plan__plan_from_discovery(
        plan@meta$children,
        plan@meta$manifest,
        file.path(plan@store_path, "discovery")
    )
    expect_gt(nrow(plan@meta$shared_plan$acquisitions), 0L)
    shift_batch__receipt_write(plan)
    completed <- shift_run(plan, ui = shift_ui(progress = "none"))
    expect_null(completed@meta[["shared_failure"]])
    expect_identical(shift_status(completed), "completed")
    expect_gt(length(shift_test__shared_receipts(plan@store_path)), 0L)
    output <- shift_outputs(completed)
    expect_equal(data.table::uniqueN(output$site_id), 2L)
    expect_equal(data.table::uniqueN(output$export_path), 2L)
    expect_true(all(file.exists(output$export_path)))
    # Compare the actual extracted cells and native monthly axis against the
    # NetCDF source, independently of successful execution and file existence.
    nc <- RNetCDF::open.nc(files[["tas"]])
    source_values <- RNetCDF::var.get.nc(nc, "tas")
    source_time <- RNetCDF::var.get.nc(nc, "time")
    source_units <- RNetCDF::att.get.nc(nc, "time", "units")
    expect_identical(RNetCDF::att.get.nc(nc, "NC_GLOBAL", "frequency"), "mon")
    expect_identical(RNetCDF::att.get.nc(nc, "NC_GLOBAL", "table_id"), "Amon")
    RNetCDF::close.nc(nc)
    for (i in seq_along(completed@meta$children)) {
        child <- completed@meta$children[[i]]
        climate <- shift_stage__new(
            ShiftClimate,
            "climate",
            store_path = child@store_path,
            ids = list(
                plan_id = jsonlite::fromJSON(child@meta$run$plan_ids_json[[1L]])
            )
        )
        raw <- shift_data(climate, n = Inf, variables = "tas")
        expect_equal(nrow(raw), 12L)
        expect_equal(raw$lon, rep(sites[[i]]@lon, 12L))
        expect_equal(raw$lat, rep(sites[[i]]@lat, 12L))
        store <- shift_inspect__store(child)
        shared_events <- store$query(paste(
            "SELECT details_json FROM shift_run_event",
            "WHERE details_json LIKE '%shared_cache%'"
        ))
        expect_gt(nrow(shared_events), 0L)
        grid <- morpher__private_store(store)$read_table(
            "extraction_grid_source"
        )
        grid <- grid[grid$variable_id == "tas"]
        expect_equal(unique(grid$grid_lon), c(104, 104.5)[[i]])
        expect_equal(unique(grid$grid_lat), c(1, 2)[[i]])
        expect_true(all(grid$weight == 1))
        store$close()
        expect_equal(
            raw$value,
            source_values[c(2L, 3L)[[i]], c(1L, 2L)[[i]], ],
            tolerance = 1e-7
        )
        expect_identical(unique(raw$units), "K")
        expect_identical(as.integer(format(raw$time, "%m")), seq_len(12L))
        expect_true(all(raw$year == 2060L))
        # The serialized EPW must expose a complete chronological hourly year.
        weather <- epw_file_read(output$export_path[[i]])$data()
        expect_equal(nrow(weather), 8760L)
        expect_true(all(is.finite(weather$dry_bulb_temperature)))
    }
    expect_length(source_time, 12L)
    expect_match(source_units, "days since 2060-01-01")
    expect_equal(
        nrow(shift_data(
            completed,
            n = 2L,
            columns = c("site_id", "dry_bulb_temperature")
        )),
        2L
    )
    hashes <- tools::md5sum(output$export_path)
    collected <- length(calls$types)
    reopened <- shift_batch_get(plan@ids$batch_id, plan@store_path)
    expect_identical(shift_outputs(reopened)$export_path, output$export_path)
    resumed <- shift_resume(reopened, ui = shift_ui(progress = "none"))
    expect_identical(shift_status(resumed), "completed")
    expect_identical(tools::md5sum(shift_outputs(resumed)$export_path), hashes)
    expect_identical(length(calls$types), collected)
    expect_setequal(shift_summary(resumed)$site_id, c("A", "B"))
    run_future <- function(control, dry_run = TRUE) {
        shift_epw_future(
            sites,
            plan@meta$climate,
            2060,
            transform = transform,
            dir = plan@meta$output_dir,
            store = dirname(dirname(plan@store_path)),
            control = control,
            dry_run = dry_run,
            ui = shift_ui("none")
        )
    }
    restored <- run_future(shift_control(strict = FALSE), dry_run = FALSE)
    expect_identical(restored@meta$execution$action, c("reused", "reused"))
    for (control in list(
        shift_control(strict = TRUE),
        shift_control(strict = FALSE, extraction_method = "idw")
    )) {
        changed <- run_future(control)
        expect_false(identical(changed@ids$batch_id, plan@ids$batch_id))
        expect_true(all(vapply(
            changed@meta$children,
            S7::S7_inherits,
            logical(1L),
            ShiftPlan
        )))
        expect_identical(changed@meta$children[[1L]]@meta$control, control)
    }
    expect_identical(tools::md5sum(output$export_path), hashes)
})

test_that("R and CLI reject invalid method periods before discovery", {
    test_local_dependencies(list(availability = function(...) {
        stop("Unexpected catalog access")
    }))
    config <- epwshiftr_cli_shift_example_config()
    config$transform <- list(scale = "hourly", method = "kernel_qdm")
    config$periods <- list(mid = 2050L)
    config$calibration <- list(dataset = "era5", years = "1995:2014")
    path <- batch_test__write_config(config)
    root <- tempfile()
    for (cmd in list(c("config", "validate"), c("run", "--dry-run"))) {
        res <- epwshiftr_cli(c(
            "--quiet",
            "--store",
            root,
            "shift",
            cmd,
            "--config",
            path
        ))
        expect_equal(res$status, 1L)
        expect_match(res$error, "at least two weather years")
    }
    expect_error(
        shift_epw_future(
            shift_site(epw = get_cache_epw()),
            shift_cmip6("Model-A", "ssp585"),
            2050L,
            transform = hourly_transform("kernel_qdm"),
            calibration = shift_era5(1995:2014),
            dir = tempfile(),
            store = root,
            dry_run = TRUE
        ),
        "at least two weather years"
    )
    expect_false(dir.exists(root))
})

# vim: fdm=marker :
