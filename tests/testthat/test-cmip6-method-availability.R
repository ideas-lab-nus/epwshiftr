# Construct complete Dataset identities for offline public-query tests.
# method_availability_test__datasets {{{
method_availability_test__datasets <- function(
    variables = "tas",
    source = "Model-A",
    experiments = c("historical", "ssp245"),
    frequency = "day",
    table = "day"
) {
    rows <- data.table::CJ(
        source_id = source,
        experiment_id = experiments,
        variant_label = "r1i1p1f1",
        grid_label = "gn",
        variable_id = variables,
        frequency = frequency,
        table_id = table,
        sorted = FALSE
    )
    rows[,
        id := paste(
            source_id,
            experiment_id,
            variable_id,
            frequency,
            table_id,
            sep = "."
        )
    ]
    rows[, `:=`(size = 1, latest = TRUE, replica = FALSE)]
    rows
}
# }}}

test_that("method queries share a union catalog and keep rejected identities", {
    calls <- list()
    datasets <- data.table::rbindlist(list(
        method_availability_test__datasets(source = "Temperature-only"),
        method_availability_test__datasets(
            c("tas", "tasmin", "tasmax", "huss", "ps"),
            source = "Full"
        )
    ))
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls[[length(calls) + 1L]] <<- list(
                request = request,
                store = store,
                ui = ui
            )
            datasets
        },
        .package = "epwshiftr"
    )
    result <- shift_cmip6_avail(
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245",
        store = "method-store",
        ui = "method-ui"
    )
    expect_length(calls, 1L)
    expect_identical(calls[[1L]]$store, "method-store")
    expect_identical(calls[[1L]]$ui, "method-ui")
    expect_setequal(
        calls[[1L]]$request@meta$variables,
        c("tas", "tasmin", "tasmax", "huss", "ps")
    )
    expect_setequal(
        calls[[1L]]$request@meta$experiment,
        c("historical", "ssp245")
    )
    expect_identical(calls[[1L]]$request@meta$frequency, "day")
    expect_null(calls[[1L]]$request@meta$filters$table_id)
    expect_equal(nrow(result), 4L)
    expect_equal(sum(result$selected), 3L)
    expect_identical(unique(result$common), FALSE)

    rejected <- result[
        source_id == "Temperature-only" & method == "sobie_curry"
    ]
    expect_false(rejected$selected)
    expect_match(rejected$missing, "historical:huss")
    expect_identical(result[method == "qdm"]$variables[[1L]], "tas")
    expect_identical(
        result[method == "qdm"]$frequency_spec[[1L]],
        c(tas = "day")
    )
    expect_identical(result[method == "qdm"]$table[[1L]], c(tas = "day"))

    common <- shift_cmip6_avail(
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245",
        common = TRUE
    )
    expect_equal(sum(common$selected), 2L)
    expect_identical(unique(common$common), TRUE)
    expect_identical(result$method_eligible, common$method_eligible)
    expect_identical(unique(result$period_coverage), "not_checked")
    expect_identical(unique(result$readability), "not_checked")
    expect_identical(unique(result$quality), "not_checked")
})

test_that("explicit transforms drive variables, optional historical queries, and cross-period consistency", {
    transform <- monthly_transform("epwshiftr", precipitation = "off")
    variables <- transform@required_inputs$model_future@variable_sets[[1L]]
    calls <- list()
    catalog <- method_availability_test__datasets(
        variables,
        experiments = "ssp245",
        frequency = "mon",
        table = "Amon"
    )
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls[[length(calls) + 1L]] <<- request
            catalog
        },
        .package = "epwshiftr"
    )
    result <- shift_cmip6_avail(transform = transform, scenarios = "ssp245")
    expect_true(result$selected)
    expect_false("pr" %in% calls[[1L]]@meta$variables)
    expect_identical(calls[[1L]]@meta$experiment, "ssp245")
    expect_identical(calls[[1L]]@meta$frequency, "mon")

    historical <- shift_cmip6_avail(
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_false(historical$selected)
    expect_match(historical$missing, "historical:")
    expect_setequal(calls[[2L]]@meta$experiment, c("historical", "ssp245"))

    # Reject incompatible humidity alternatives split across historical vs future
    alternatives <- monthly_transform(
        "epwshiftr"
    )@required_inputs$model_future@variable_sets
    catalog <- data.table::rbindlist(list(
        method_availability_test__datasets(
            alternatives[[1L]],
            experiments = "historical",
            frequency = "mon",
            table = "Amon"
        ),
        method_availability_test__datasets(
            alternatives[[2L]],
            experiments = "ssp245",
            frequency = "mon",
            table = "Amon"
        )
    ))
    mismatched <- shift_cmip6_avail(
        transform = monthly_transform("epwshiftr"),
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_false(mismatched$selected)
    expect_match(mismatched$missing, "historical:|ssp245:")
})

test_that("method queries preserve empty schemas, defensive filters, replica policy, and table lookup caching", {
    response <- data.table::data.table()
    calls <- list()
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls[[length(calls) + 1L]] <<- request
            response
        },
        .package = "epwshiftr"
    )
    empty <- shift_cmip6_avail(methods = "qdm", scenarios = "ssp245")
    expect_s3_class(empty, "data.table")
    expect_equal(nrow(empty), 0L)
    expect_type(empty$selected, "logical")
    expect_type(empty$variables, "list")

    response <- method_availability_test__datasets(
        source = c("Wanted", "Other")
    )
    response <- data.table::rbindlist(list(
        response,
        data.table::copy(response)[, grid_label := "gr"],
        data.table::copy(response)[, variant_label := NA_character_]
    ))
    result <- shift_cmip6_avail(
        methods = "qdm",
        scenarios = "ssp245",
        source = "Wanted",
        grid = "gn",
        member = "r1i1p1f1",
        filters = list(variable_id = "pr", frequency = "mon", table_id = "Amon")
    )
    expect_identical(result$source_id, "Wanted")
    expect_identical(result$grid_label, "gn")
    expect_true(result$selected)
    expect_identical(names(empty), names(result))
    expect_null(calls[[2L]]@meta$filters$variable_id)
    expect_null(calls[[2L]]@meta$filters$frequency)
    expect_null(calls[[2L]]@meta$filters$table_id)

    # Explicit replica filters are preserved without mutating caller input
    response <- method_availability_test__datasets()
    for (filters in list(list(), list(replica = TRUE), list(replica = NULL))) {
        original <- filters
        shift_cmip6_avail(
            methods = "qdm",
            scenarios = "ssp245",
            filters = filters
        )
        expected <- if ("replica" %in% names(filters)) {
            filters$replica
        } else {
            FALSE
        }
        expect_identical(
            utils::tail(calls, 1L)[[1L]]@meta$filters$replica,
            expected
        )
        expect_identical(filters, original)
    }

    # Frequency table defaults resolve once per unique frequency across many models
    orig_table <- shift_spec__cmip6_table_id
    table_calls <- character()
    response <- method_availability_test__datasets(
        source = paste0("Model-", 1:50)
    )
    local_mocked_bindings(
        shift_spec__cmip6_table_id = function(frequency) {
            table_calls <<- c(table_calls, frequency)
            orig_table(frequency)
        },
        .package = "epwshiftr"
    )
    many <- shift_cmip6_avail(methods = "qdm", scenarios = "ssp245")
    expect_equal(nrow(many), 50L)
    expect_identical(table_calls, "day")
})

test_that("invalid method combinations fail before querying", {
    local_mocked_bindings(
        availability__collect = function(...) stop("Unexpected query"),
        .package = "epwshiftr"
    )
    bad_calls <- list(
        list(
            args = list(variables = "tas", methods = "qdm"),
            pat = "not a mixture"
        ),
        list(
            args = list(methods = "qdm", transform = daily_transform("qdm")),
            pat = "either"
        ),
        list(args = list(methods = "qdm", frequency = "mon"), pat = "derive"),
        list(
            args = list(methods = "qdm", include_historical = FALSE),
            pat = "derive"
        ),
        list(args = list(methods = "qdm", table = "day"), pat = "derive"),
        list(
            args = list(methods = "qdm", scenarios = "historical"),
            pat = "future experiment"
        ),
        list(
            args = list(methods = "unknown-method"),
            pat = "Unknown weather method"
        ),
        list(args = list(methods = c("qdm", "qdm")), pat = "unique"),
        list(args = list(variables = "tas", common = TRUE), pat = "require")
    )
    for (case in bad_calls) {
        expect_error(do.call(shift_cmip6_avail, case$args), case$pat)
    }
    for (value in list(NA, NULL, 1, "common", c(TRUE, FALSE))) {
        expect_error(
            shift_cmip6_avail(methods = "qdm", common = value),
            "common"
        )
    }
})

test_that("method queries derive mixed frequencies and reject missing catalog fields", {
    transform <- hourly_transform("kernel_qdm")
    contracts <- shift_batch__transforms(transform = transform)
    requirements <- eligibility__requirements(
        contracts,
        "ssp245",
        stats::setNames(TRUE, names(contracts))
    )
    response <- unique(requirements$lookup[,
        c("experiment_id", "variable_id", "frequency"),
        with = FALSE
    ])
    response[, `:=`(
        source_id = "Mixed",
        variant_label = "r1i1p1f1",
        grid_label = "gn"
    )]
    response[,
        table_id := vapply(frequency, shift_spec__cmip6_table_id, character(1L))
    ]

    calls <- list()
    local_mocked_bindings(
        availability__collect = function(request, ...) {
            calls[[length(calls) + 1L]] <<- request
            response
        },
        .package = "epwshiftr"
    )
    result <- shift_cmip6_avail(transform = transform, scenarios = "ssp245")
    expect_true(all(result$selected))
    expect_setequal(calls[[1L]]@meta$frequency, c("3hr", "3hrPt"))
    expect_identical(unname(result$frequency_spec[[1L]]["tas"]), "3hrPt")

    response[variable_id == "tas", table_id := NA_character_]
    rejected <- shift_cmip6_avail(transform = transform, scenarios = "ssp245")
    expect_false(any(rejected$selected))
    expect_match(rejected$missing, "tas")
})

test_that("shared method discovery reuses the existing HTTP cache across order and pool changes", {
    skip_on_cran()
    local_test_cache()
    local_cache_mode("normal")
    datasets <- method_availability_test__datasets(
        c("tas", "tasmin", "tasmax", "huss", "ps"),
        experiments = c("historical", "ssp245", "ssp585")
    )
    bytes <- charToRaw(jsonlite::toJSON(
        esgf_test__response(datasets),
        auto_unbox = TRUE,
        dataframe = "rows",
        null = "null"
    ))
    calls <- character()
    local_mocked_bindings(
        curl_fetch_memory = function(url, handle, ...) {
            calls <<- c(calls, url)
            list(content = bytes, status_code = 200L)
        },
        .package = "curl"
    )
    first <- shift_cmip6_avail(
        methods = c("qdm", "sobie_curry"),
        scenarios = c("ssp245", "ssp585"),
        index_node = "https://example.org",
        store = tempfile("method-cold-"),
        ui = shift_ui(progress = "none")
    )
    cold_requests <- length(calls)
    expect_gt(cold_requests, 0L)
    expect_true(all(first$selected))

    local_cache_mode("offline")
    second <- shift_cmip6_avail(
        methods = c("sobie_curry", "qdm"),
        scenarios = c("ssp585", "ssp245"),
        index_node = "https://example.org",
        store = tempfile("method-warm-"),
        ui = shift_ui(progress = "none")
    )
    expect_identical(first, second)
    expect_length(calls, cold_requests)

    common <- shift_cmip6_avail(
        methods = c("sobie_curry", "qdm"),
        scenarios = c("ssp585", "ssp245"),
        common = TRUE,
        index_node = "https://example.org",
        store = tempfile("method-common-"),
        ui = shift_ui(progress = "none")
    )
    expect_identical(first$catalog_eligible, common$catalog_eligible)
    expect_length(calls, cold_requests)
    expect_false(any(grepl("type=File", calls, fixed = TRUE)))
})

# vim: fdm=marker :
