# Construct real Dataset-shaped method inputs, with deliberately different
# model pools. Coverage remains a separate adapter in each test.
batch_pool_test__catalog <- function() {
    monthly <- monthly_transform("original_morphing")
    variables <- monthly@required_inputs$model_future@variable_sets[[1L]]
    frequencies <- shift__transform_cmip6_frequencies(monthly, variables)
    tables <- shift__cmip6_variable_tables(variables, frequencies, NULL)
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

# Plan a small batch through the public entry point without any data transfer.
batch_pool_test__plan <- function(
    pool = "common",
    model = NULL,
    store = tempfile(),
    methods = c("original_morphing", "isimip3basd")
) {
    shift_future_epw(
        get_cache_epw(),
        climate = shift_cmip6(
            model = model,
            scenarios = "ssp585",
            pool = pool,
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

test_that("batch methods share Dataset discovery while keeping separate pools", {
    catalog <- batch_pool_test__catalog()
    original <- data.table::copy(catalog)
    calls <- list()
    withr::local_options(list(
        epwshiftr.cmip6.availability = NULL,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    testthat::local_mocked_bindings(
        availability__collect = function(request, ...) {
            calls[[length(calls) + 1L]] <<- request
            catalog
        },
        .package = "epwshiftr"
    )

    common <- batch_pool_test__plan()
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
    batch <- batch_pool_test__plan("per_method", store = store)
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
    repeated <- batch_pool_test__plan("per_method", store = store)
    expect_identical(restored@meta$manifest, batch@meta$manifest)
    expect_identical(restored@meta$climate@pool, "per_method")
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
    catalog <- batch_pool_test__catalog()
    withr::local_options(list(
        epwshiftr.cmip6.availability = NULL,
        epwshiftr.cmip6.period_coverage = function(candidates, transform, ...) {
            # A is cheaper than B for monthly data; C is cheaper for daily data.
            candidates[,
                source_file_count := c(A = 1, B = 10, C = 1)[source_id]
            ]
            candidates
        }
    ))
    testthat::local_mocked_bindings(
        availability__collect = function(...) catalog,
        .package = "epwshiftr"
    )
    bounded <- batch_pool_test__plan("per_method", model = 1L)
    expect_setequal(bounded@meta$manifest$model, c("A", "C"))
    expect_equal(nrow(bounded@meta$manifest), 2L)
    named <- batch_pool_test__plan("per_method", model = c("A", "C"))
    expect_identical(named@meta$manifest$model, c("A", "C"))
    expect_error(
        batch_pool_test__plan("common", model = c("A", "C")),
        "No common"
    )
    expect_error(
        batch_pool_test__plan("per_method", model = c("A", "B", "missing")),
        "lack complete coverage for any method"
    )

    # Catalogue presence does not waive missing File-year coverage for B.
    withr::local_options(list(epwshiftr.cmip6.period_coverage = function(
        candidates,
        ...
    ) {
        candidates[source_id != "B"]
    }))
    expect_error(batch_pool_test__plan("common"), "No common")
    expect_error(
        batch_pool_test__plan("per_method", model = 2L),
        "Only 1 complete"
    )
    expect_setequal(
        batch_pool_test__plan("per_method")@meta$manifest$model,
        c("A", "C")
    )
})

test_that("shared catalog failover does not repeat failed nodes or mix identity pins", {
    catalog <- batch_pool_test__catalog()
    # Extra provider rows may not override a pinned member, grid or model.
    extra <- data.table::copy(catalog)
    extra[, `:=`(
        source_id = "other",
        member_id = "r2i1p1f1",
        grid_label = "gr"
    )]
    catalog <- data.table::rbindlist(list(catalog, extra))
    calls <- character()
    withr::local_options(list(
        epwshiftr.cmip6.availability = NULL,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    testthat::local_mocked_bindings(
        availability__collect = function(request, ...) {
            node <- request@meta$options$index_node
            calls <<- c(calls, node)
            if (grepl("first", node)) {
                stop("Index temporarily unavailable")
            }
            catalog
        },
        .package = "epwshiftr"
    )
    transforms <- shift_batch__transforms(c("original_morphing", "isimip3basd"))
    references <- lapply(
        transforms,
        shift_batch__references,
        reference = NULL,
        calibration = shift_era5(1995:2014)
    )
    climate <- shift_cmip6(
        model = NULL,
        scenarios = "ssp585",
        grid = "gn",
        index_nodes = c(
            "https://first.example/esg-search",
            "https://second.example/esg-search"
        ),
        pool = "per_method"
    )
    selected <- shift_batch__discover_models(
        climate,
        transforms,
        shift__periods_from_years(2050L),
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
    frequency <- shift__transform_cmip6_frequencies(transform, variables)
    tables <- shift__cmip6_variable_tables(variables, frequency, NULL)
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
    withr::local_options(list(
        epwshiftr.cmip6.availability = NULL,
        epwshiftr.cmip6.period_coverage = function(candidates, variables, ...) {
            if (identical(variables, as.character(sets[[1L]]))) {
                candidates[0L]
            } else {
                candidates
            }
        }
    ))
    testthat::local_mocked_bindings(
        availability__collect = function(...) {
            calls <<- calls + 1L
            catalog
        },
        .package = "epwshiftr"
    )
    selected <- shift_batch__discover_models(
        shift_cmip6(
            model = 1L,
            scenarios = "ssp585",
            index_nodes = "https://example.org/esg-search"
        ),
        list(monthly = transform),
        shift__periods_from_years(2050L),
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

test_that("default climate serialization keeps old identities and accepts old receipts", {
    climate <- shift_cmip6(model = NULL, scenarios = "ssp585")
    value <- shift__climate_spec_value(climate)
    expect_false("pool" %in% names(value))
    expect_identical(
        shift__climate_spec_value(shift__climate_from_spec(value)),
        value
    )
    expect_identical(shift__climate_from_spec(value)@pool, "common")
    climate@pool <- "per_method"
    value <- shift__climate_spec_value(climate)
    roundtrip <- jsonlite::fromJSON(jsonlite::toJSON(
        value,
        auto_unbox = TRUE,
        null = "null"
    ))
    expect_identical(shift__climate_from_spec(roundtrip)@pool, "per_method")
    expect_error(shift_cmip6(scenarios = "ssp585", pool = "union"), "arg")

    withr::local_options(list(
        epwshiftr.cmip6.availability = test_cmip6_availability,
        epwshiftr.cmip6.period_coverage = test_cmip6_period_coverage
    ))
    store <- tempfile()
    batch <- batch_pool_test__plan(model = 1L, store = store)
    path <- shift_batch__receipt_path(batch@store_path)
    receipt <- readRDS(path)
    receipt$discovery$selection <- NULL
    saveRDS(receipt, path)
    withr::local_options(list(epwshiftr.cmip6.availability = function(...) {
        stop("Unexpected discovery")
    }))
    restored <- batch_pool_test__plan(model = 1L, store = store)
    expect_identical(restored@ids$batch_id, batch@ids$batch_id)
    expect_identical(
        restored@meta$manifest[, .(method, model, member, grid)],
        batch@meta$manifest[, .(method, model, member, grid)]
    )
})

test_that("workflow configuration accepts and displays a per-method pool locally", {
    config <- epwshiftr_cli_shift_example_config()
    config$climate$pool <- "per_method"
    config$climate$model <- 1L
    config$methods <- c("original_morphing", "isimip3basd")
    config$transform <- NULL
    config$calibration <- list(dataset = "era5", years = 1995:2014)
    path <- tempfile(fileext = ".json")
    jsonlite::write_json(config, path, auto_unbox = TRUE, null = "null")
    withr::local_options(list(epwshiftr.cmip6.availability = function(...) {
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
    expect_identical(result$result$status, "valid")
    expect_identical(result$result$intent$Pool, "per_method")
})
