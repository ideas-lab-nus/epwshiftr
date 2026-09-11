test_that("method keys resolve through the transform registry", {
    transforms <- shift_batch__transforms(c(
        "original_morphing",
        "bws_btws",
        "isimip3basd"
    ))

    expect_identical(
        unname(vapply(
            transforms,
            function(value) value@method,
            character(1L)
        )),
        c("original_morphing", "bws_btws", "isimip3basd")
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

test_that("method key resolution does not materialize the public catalog", {
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
        unname(vapply(
            transforms,
            function(value) value@recipe,
            character(1L)
        )),
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
})

test_that("shift_run dispatches a dry-run batch to the batch runner", {
    batch <- shift_stage_new(
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

test_that("completed batch children reuse one authoritative store", {
    opened <- 0L
    closed <- 0L
    verified <- 0L
    child_store <- new.env(parent = emptyenv())
    child_store$close <- function() {
        closed <<- closed + 1L
        invisible(NULL)
    }
    run <- shift_stage_new(
        ShiftRun,
        "run",
        store_path = "/example/child",
        ids = list(run_id = "run-child"),
        meta = list(run = data.table::data.table(status = "completed"))
    )
    testthat::local_mocked_bindings(
        shift_store = function(x, create = FALSE) {
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
        shift__run_artifacts_complete = function(store, run_id) {
            verified <<- verified + 1L
            expect_identical(store, child_store)
            expect_identical(run_id, "run-child")
            TRUE
        },
        .package = "epwshiftr"
    )

    restored <- shift_batch__restore_child(list(
        run_id = "run-child",
        store_path = "/example/child",
        status = "completed"
    ))

    expect_identical(restored, run)
    expect_identical(opened, 1L)
    expect_identical(closed, 1L)
    expect_identical(verified, 1L)
})

test_that("completed receipt hints do not replace authoritative run status", {
    verified <- 0L
    child_store <- new.env(parent = emptyenv())
    child_store$close <- function() invisible(NULL)
    run <- shift_stage_new(
        ShiftRun,
        "run",
        store_path = "/example/child",
        ids = list(run_id = "run-child"),
        meta = list(run = data.table::data.table(status = "running"))
    )
    testthat::local_mocked_bindings(
        shift_store = function(x, create = FALSE) child_store,
        shift_run_get = function(run_id, store) run,
        shift__run_artifacts_complete = function(store, run_id) {
            verified <<- verified + 1L
            TRUE
        },
        .package = "epwshiftr"
    )

    restored <- shift_batch__restore_child(list(
        run_id = "run-child",
        store_path = "/example/child",
        status = "completed"
    ))

    expect_identical(shift_status(restored, refresh = FALSE), "running")
    expect_identical(verified, 0L)
})

test_that("completed batch child restoration rejects missing artifacts", {
    closed <- 0L
    child_store <- new.env(parent = emptyenv())
    child_store$close <- function() {
        closed <<- closed + 1L
        invisible(NULL)
    }
    run <- shift_stage_new(
        ShiftRun,
        "run",
        store_path = "/example/child",
        ids = list(run_id = "run-child"),
        meta = list(run = data.table::data.table(status = "completed"))
    )
    testthat::local_mocked_bindings(
        shift_store = function(x, create = FALSE) child_store,
        shift_run_get = function(run_id, store) run,
        shift__run_artifacts_complete = function(store, run_id) FALSE,
        .package = "epwshiftr"
    )

    restored <- shift_batch__restore_child(list(
        run_id = "run-child",
        store_path = "/example/child",
        status = "completed"
    ))

    expect_null(restored)
    expect_identical(closed, 1L)
})

test_that("locked completed children retain path-based live restoration", {
    store_arguments <- list()
    run <- shift_stage_new(
        ShiftRun,
        "run",
        store_path = "/example/child",
        ids = list(run_id = "run-child"),
        meta = list(run = data.table::data.table(status = "running"))
    )
    testthat::local_mocked_bindings(
        shift_store = function(x, create = FALSE) {
            stop("manifest locked")
        },
        shift_run_get = function(run_id, store) {
            store_arguments[[length(store_arguments) + 1L]] <<- store
            run
        },
        .package = "epwshiftr"
    )

    restored <- shift_batch__restore_child(list(
        run_id = "run-child",
        store_path = "/example/child",
        status = "completed"
    ))

    expect_identical(restored, run)
    expect_identical(store_arguments, list("/example/child"))
})

test_that("high-level workflows select common models and retain child plans", {
    availability_calls <- 0L
    availability <- function(...) {
        availability_calls <<- availability_calls + 1L
        test_cmip6_availability(...)
    }
    withr::local_options(list(
        epwshiftr.cmip6.availability = availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    output <- tempfile("batch-output-")
    store <- tempfile("batch-store-")

    batch <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = 2L,
            scenarios = c("ssp126", "ssp585")
        ),
        periods = list(`2050` = 2049:2050, `2080` = 2079:2080),
        methods = c("original_morphing", "bws_btws", "isimip3basd"),
        calibration = shift_era5(years = 1995:2014),
        dir = output,
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )

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
    expect_true(all(file.path(shift_batch__store_root(store), "batches") ==
        dirname(dirname(manifest$store))))
    expect_silent(shift_logs(batch))
    first_discovery_calls <- availability_calls
    expect_gt(first_discovery_calls, 0L)

    # Delivery paths do not change the scientific batch identity or reusable
    # store root selected for the same EPW, climate, periods, and methods.
    second <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = 2L,
            scenarios = c("ssp126", "ssp585")
        ),
        periods = list(`2050` = 2049:2050, `2080` = 2079:2080),
        methods = c("original_morphing", "bws_btws", "isimip3basd"),
        calibration = shift_era5(years = 1995:2014),
        dir = tempfile("another-batch-output-"),
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
    expect_identical(second@ids$batch_id, batch@ids$batch_id)
    expect_identical(availability_calls, first_discovery_calls)

    # Explicit refresh bypasses the persisted batch selection without changing
    # the scientific batch identity.
    refreshed <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = 2L,
            scenarios = c("ssp126", "ssp585")
        ),
        periods = list(`2050` = 2049:2050, `2080` = 2079:2080),
        methods = c("original_morphing", "bws_btws", "isimip3basd"),
        calibration = shift_era5(years = 1995:2014),
        control = shift_control(refresh = TRUE),
        dir = output,
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
    expect_identical(refreshed@ids$batch_id, batch@ids$batch_id)
    expect_gt(availability_calls, first_discovery_calls)

    changed_calibration <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = 2L,
            scenarios = c("ssp126", "ssp585")
        ),
        periods = list(`2050` = 2049:2050, `2080` = 2079:2080),
        methods = c("original_morphing", "bws_btws", "isimip3basd"),
        calibration = shift_era5(years = 2000:2019),
        dir = tempfile("another-batch-output-"),
        store = store,
        dry_run = TRUE,
        ui = shift_ui(progress = "none")
    )
    expect_false(identical(
        changed_calibration@ids$batch_id,
        batch@ids$batch_id
    ))
})

test_that("direct high-level execution builds the whole batch before running", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    received <- NULL
    testthat::local_mocked_bindings(
        shift_batch__run = function(x, background, ui) {
            received <<- x
            "executed"
        },
        .package = "epwshiftr"
    )

    result <- shift_future_epw(
        epw = get_cache_epw(),
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
    expect_true(all(vapply(received@meta$children, function(child) {
        S7::S7_inherits(child, ShiftPlan)
    }, logical(1L))))
})

test_that("batch discovery keeps r1i1p1f1 as a hard default", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    climate <- shift_cmip6(
        model = 1L,
        scenarios = "ssp585",
        member = "r2i1p1f1"
    )

    expect_error(
        shift_batch__discover_models(
            climate,
            shift_batch__transforms("isimip3basd"),
            shift__periods_from_years(2050L),
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
    withr::local_options(list(
        epwshiftr.cmip6.availability = collect,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    transforms <- list(
        monthly = monthly_transform("epwshiftr"),
        daily = daily_transform("isimip3basd")
    )

    references <- list(
        monthly = list(reference = NULL),
        daily = list(reference = historical_reference(1995:2014))
    )
    shift_batch__discover_models(
        shift_cmip6(model = 1L, scenarios = "ssp585"),
        transforms,
        shift__periods_from_years(2050L),
        references,
        tempfile("discovery-"),
        shift_ui(progress = "none")
    )

    monthly_calls <- Filter(function(value) {
        length(value$variables) > 1L
    }, calls)
    daily_calls <- Filter(function(value) {
        identical(value$variables, "tas")
    }, calls)
    expect_true(length(monthly_calls) > 0L)
    expect_true(all(!vapply(
        monthly_calls,
        `[[`,
        logical(1L),
        "include_historical"
    )))
    expect_true(length(daily_calls) > 0L)
    expect_true(all(vapply(
        daily_calls,
        `[[`,
        logical(1L),
        "include_historical"
    )))
})

test_that("NULL model selection retains every compatible common model", {
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    transforms <- shift_batch__transforms("isimip3basd")
    references <- stats::setNames(
        list(list(reference = historical_reference(1995:2014))),
        names(transforms)
    )
    selection <- shift_batch__discover_models(
        shift_cmip6(model = NULL, scenarios = "ssp585"),
        transforms,
        shift__periods_from_years(2050L),
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
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = function(candidates, ...) {
            candidates[, source_file_count := c(
                `Model-A` = 1200,
                `Model-B` = 120,
                `Model-C` = 240
            )[source_id]]
            candidates
        }
    ))
    climate <- shift_cmip6(model = 2L, scenarios = "ssp585")
    selection <- shift_batch__discover_models(
        climate,
        transforms = list(monthly = monthly_transform("original_morphing")),
        periods = shift__periods_from_years(2050L),
        references = NULL,
        store = tempfile("fragment-ranking-store-"),
        ui = shift_ui(progress = "none")
    )

    expect_identical(
        selection$identities$source_id,
        c("Model-B", "Model-C")
    )
    expect_identical(
        selection$identities$source_file_count,
        c(120, 240)
    )
})

test_that("batch discovery applies period coverage before model counts", {
    coverage_calls <- list()
    period_coverage <- function(candidates, periods, ...) {
        coverage_calls[[length(coverage_calls) + 1L]] <<- sort(unique(
            periods$year
        ))
        candidates[source_id != "Model-B"]
    }
    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = period_coverage
    ))

    batch <- shift_future_epw(
        epw = get_cache_epw(),
        climate = shift_cmip6(
            model = 2L,
            scenarios = c("ssp126", "ssp585")
        ),
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
    expect_identical(
        coverage_calls[[1L]],
        c(2041:2060, 2071:2090)
    )
})

test_that("File coverage rejects gaps inside requested CMIP6 periods", {
    candidates <- data.table::as.data.table(test_cmip6_availability(
        variables = "tas",
        scenarios = c("ssp126", "ssp585"),
        member = "r1i1p1f1",
        frequency = c(tas = "day"),
        index_node = "https://example.org"
    ))
    catalog <- data.table::CJ(
        source_id = c("Model-A", "Model-B", "Model-C"),
        experiment_id = c("ssp126", "ssp585"),
        unique = TRUE
    )
    catalog[, `:=`(
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = "tas",
        grid_label = "gn",
        latest = TRUE,
        retracted = FALSE,
        deprecated = FALSE,
        datetime_start = "2041-01-01T00:00:00Z",
        datetime_end = data.table::fifelse(
            source_id == "Model-B",
            "2050-12-31T23:59:59Z",
            "2090-12-31T23:59:59Z"
        )
    )]
    # Model-B resumes after an internal 2051 gap. Additional 2051 records use
    # incompatible member, table, or grid identities and therefore must not be
    # combined to manufacture complete coverage.
    model_b_tail <- data.table::copy(catalog[source_id == "Model-B"])
    model_b_tail[, `:=`(
        datetime_start = "2052-01-01T00:00:00Z",
        datetime_end = "2090-12-31T23:59:59Z"
    )]
    distractors <- data.table::rbindlist(lapply(
        c("member", "table", "grid"),
        function(kind) {
            rows <- data.table::copy(catalog[source_id == "Model-B"])
            rows[, `:=`(
                datetime_start = "2051-01-01T00:00:00Z",
                datetime_end = "2051-12-31T23:59:59Z"
            )]
            if (identical(kind, "member")) rows[, variant_label := "r2i1p1f1"]
            if (identical(kind, "table")) rows[, table_id := "3hr"]
            if (identical(kind, "grid")) rows[, grid_label := "gr"]
            rows
        }
    ), use.names = TRUE, fill = TRUE)
    catalog <- data.table::rbindlist(
        list(catalog, model_b_tail, distractors),
        use.names = TRUE,
        fill = TRUE
    )
    testthat::local_mocked_bindings(
        shift__cmip6_coverage_catalog = function(...) catalog,
        .package = "epwshiftr"
    )

    covered <- shift__cmip6_period_coverage(
        candidates = candidates,
        climate = shift_cmip6(
            model = NULL,
            scenarios = c("ssp126", "ssp585")
        ),
        transform = daily_transform(
            "epwshiftr",
            reconstruction = "power"
        ),
        variables = "tas",
        frequency = c(tas = "day"),
        periods = shift__periods_from_input(list(
            `2050` = 2041:2060,
            `2080` = 2071:2090
        )),
        reference = NULL,
        node = "https://example.org",
        store = tempfile("coverage-store-"),
        ui = shift_ui(progress = "none")
    )

    expect_identical(covered$source_id, c("Model-A", "Model-C"))
    direct <- shift__cmip6_candidates(
        catalog,
        models = unique(candidates$source_id),
        experiments = c("ssp126", "ssp585"),
        variables = "tas",
        years = c(2041:2060, 2071:2090),
        frequency = c(tas = "day"),
        table = c(tas = "day"),
        requirements = list(tas = list("tas"))
    )
    expect_setequal(
        covered$source_id,
        direct[complete %in% TRUE]$source_id
    )
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
    catalog <- data.table::data.table(
        source_id = "Model-A",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        grid_label = "gn",
        replica = FALSE,
        latest = TRUE,
        deprecated = FALSE,
        datetime_start = "2041-01-01T00:00:00Z",
        datetime_end = "2060-12-31T23:59:59Z"
    )
    testthat::local_mocked_bindings(
        shift__cmip6_coverage_catalog = function(...) catalog,
        .package = "epwshiftr"
    )

    covered <- shift__cmip6_period_coverage(
        candidates = candidates,
        climate = shift_cmip6(model = NULL, scenarios = "ssp585"),
        transform = daily_transform("epwshiftr", reconstruction = "power"),
        variables = "tas",
        frequency = c(tas = "day"),
        periods = shift__periods_from_input(list(`2050` = 2041:2060)),
        reference = NULL,
        node = "https://example.org",
        store = tempfile("table-coverage-store-"),
        ui = shift_ui(progress = "none")
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
    make_catalog <- function(experiment, start, end) {
        rows <- data.table::data.table(
            source_id = c("Model-A", "Model-B", "Model-C")
        )
        rows[, `:=`(
            experiment_id = experiment,
            variant_label = "r1i1p1f1",
            frequency = "day",
            table_id = "day",
            variable_id = "tas",
            grid_label = "gn",
            latest = TRUE,
            retracted = FALSE,
            deprecated = FALSE,
            datetime_start = start,
            datetime_end = end
        )]
        rows
    }
    future <- make_catalog(
        "ssp585",
        "2050-01-01T00:00:00Z",
        "2050-12-31T23:59:59Z"
    )
    historical <- make_catalog(
        "historical",
        "1995-01-01T00:00:00Z",
        "2014-12-31T23:59:59Z"
    )
    historical[source_id == "Model-B",
        datetime_end := "2010-12-31T23:59:59Z"]
    testthat::local_mocked_bindings(
        shift__cmip6_coverage_catalog = function(request, ...) {
            if (identical(request@meta$experiment, "historical")) {
                historical
            } else {
                future
            }
        },
        .package = "epwshiftr"
    )

    covered <- shift__cmip6_period_coverage(
        candidates = candidates,
        climate = shift_cmip6(model = NULL, scenarios = "ssp585"),
        transform = daily_transform("isimip3basd"),
        variables = "tas",
        frequency = c(tas = "day"),
        periods = shift__periods_from_years(2050L),
        reference = historical_reference(1995:2014),
        node = "https://example.org",
        store = tempfile("historical-coverage-store-"),
        ui = shift_ui(progress = "none")
    )

    expect_identical(covered$source_id, c("Model-A", "Model-C"))
})

test_that("single explicit transforms preserve the established plan API", {
    climate <- shift_cmip6(
        model = "Model-A",
        scenarios = "ssp585"
    )
    plan <- shift_future_epw(
        epw = get_cache_epw(),
        climate = climate,
        periods = list(`2050` = 2049:2050),
        transform = daily_transform("isimip3basd"),
        observed_reference = shift_reference_plan(
            "observed-plan",
            shift__periods_from_years(1995:2014),
            role = "observed_reference"
        ),
        reference = historical_reference(1995:2014),
        dir = tempfile("single-output-"),
        store = tempfile("single-store-"),
        dry_run = TRUE
    )

    expect_true(S7::S7_inherits(plan, ShiftPlan))
    expect_identical(unique(plan@meta$request@meta$frequency), "day")
})
