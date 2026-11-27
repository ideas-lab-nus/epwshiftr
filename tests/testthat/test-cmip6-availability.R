# Build variable-specific Dataset rows for deterministic availability tests.
# availability_test__datasets {{{
availability_test__datasets <- function(
    source = "Model-A",
    experiment = c("historical", "ssp245"),
    variables = "tas",
    member = "r1i1p1f1",
    grid = "gn",
    frequency = "day",
    table = "day",
    member_column = "member_id"
) {
    rows <- data.table::CJ(
        source_id = source,
        experiment_id = experiment,
        variable_id = variables,
        sorted = FALSE
    )
    variable_frequency <- if (!is.null(names(frequency))) {
        unname(frequency[rows$variable_id])
    } else {
        rep.int(frequency[[1L]], nrow(rows))
    }
    variable_table <- if (!is.null(names(table))) {
        unname(table[rows$variable_id])
    } else {
        rep.int(table[[1L]], nrow(rows))
    }
    rows[, `:=`(
        id = sprintf(
            "CMIP6.%s.%s.%s.%s.%s",
            source_id,
            experiment_id,
            member,
            variable_id,
            grid
        ),
        frequency = variable_frequency,
        table_id = variable_table,
        grid_label = grid,
        latest = TRUE,
        replica = FALSE,
        size = 1
    )]
    data.table::set(rows, j = member_column, value = member)
    data.table::setcolorder(
        rows,
        c(
            "id",
            "source_id",
            "experiment_id",
            member_column,
            "frequency",
            "table_id",
            "variable_id",
            "grid_label",
            "latest",
            "replica",
            "size"
        )
    )
    rows
}
# }}}

test_that("availability reduction requires every experiment-variable pair", {
    variables <- c("tas", "hurs", "pr")
    complete <- data.table::rbindlist(lapply(
        c("ssp245", "ssp585", "historical"),
        function(experiment) {
            availability_test__datasets("Model-A", experiment, variables)
        }
    ))
    incomplete <- data.table::rbindlist(list(
        availability_test__datasets("Model-B", "ssp245", variables),
        availability_test__datasets(
            "Model-B",
            "ssp585",
            c("tas", "hurs")
        ),
        availability_test__datasets("Model-B", "historical", variables)
    ))
    summary <- availability__summarize(
        data.table::rbindlist(list(complete, incomplete)),
        experiments = c("ssp245", "ssp585", "historical"),
        variables = variables,
        frequency = "day",
        table = "day",
        index_node = "https://example.org/esg-search"
    )

    expect_equal(summary$source_id, c("Model-A", "Model-B"))
    expect_identical(summary$complete, c(TRUE, FALSE))
    expect_equal(summary$complete_experiments, c(3L, 2L))
    expect_equal(summary$available_pairs, c(9L, 8L))
    expect_equal(summary$missing[[2L]], "ssp585:pr")
    expect_identical(
        summary$table[[1L]],
        stats::setNames(rep("day", length(variables)), variables)
    )
})

test_that("availability combines point, mean, and daily CMIP6 frequencies", {
    variables <- names(HOURLY_KQDM_MODEL_FREQUENCIES)
    datasets <- data.table::rbindlist(lapply(
        c("ssp245", "historical"),
        function(experiment) {
            availability_test__datasets(
                "Model-A",
                experiment,
                variables,
                frequency = HOURLY_KQDM_MODEL_FREQUENCIES,
                table = c(
                    stats::setNames(
                        rep("3hr", 7L),
                        EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES
                    ),
                    tasmin = "day",
                    tasmax = "day"
                )
            )
        }
    ))
    summary <- availability__summarize(
        datasets,
        experiments = c("ssp245", "historical"),
        variables = variables,
        frequency = HOURLY_KQDM_MODEL_FREQUENCIES,
        table = NULL,
        index_node = "https://example.org/esg-search"
    )

    expect_true(summary$complete[[1L]])
    expect_identical(summary$frequency[[1L]], "3hrPt+3hr+day")
    expect_identical(
        summary$frequency_spec[[1L]],
        HOURLY_KQDM_MODEL_FREQUENCIES
    )
    expect_identical(summary$table_id[[1L]], "3hr+day")
})

test_that("availability discovers one table per variable", {
    experiments <- c("ssp585", "historical")
    datasets <- data.table::rbindlist(list(
        availability_test__datasets(
            "Model-A",
            experiments[[1L]],
            "tas",
            frequency = "3hr",
            table = "3hr"
        ),
        availability_test__datasets(
            "Model-A",
            experiments[[2L]],
            "tas",
            frequency = "3hr",
            table = "3hr"
        ),
        availability_test__datasets(
            "Model-A",
            experiments[[1L]],
            "uas",
            frequency = "3hr",
            table = "E3hr"
        ),
        availability_test__datasets(
            "Model-A",
            experiments[[2L]],
            "uas",
            frequency = "3hr",
            table = "E3hr"
        )
    ))
    summary <- availability__summarize(
        datasets,
        experiments = experiments,
        variables = c("tas", "uas"),
        frequency = "3hr",
        table = NULL,
        index_node = "https://example.org/esg-search"
    )

    expect_true(summary$complete[[1L]])
    expect_identical(summary$table_id[[1L]], "3hr+E3hr")
    expect_identical(
        summary$table[[1L]],
        c(tas = "3hr", uas = "E3hr")
    )
    climate <- shift_cmip6(
        model = summary$source_id[[1L]],
        scenarios = "ssp585",
        frequency = "3hr",
        table = summary$table[[1L]]
    )
    expect_identical(climate@table, summary$table[[1L]])
})

test_that("availability does not combine one variable across tables", {
    datasets <- data.table::rbindlist(list(
        availability_test__datasets(
            "Model-A",
            "historical",
            "tas",
            frequency = "3hr",
            table = "3hr"
        ),
        availability_test__datasets(
            "Model-A",
            "ssp585",
            "tas",
            frequency = "3hr",
            table = "E3hr"
        )
    ))
    summary <- availability__summarize(
        datasets,
        experiments = c("ssp585", "historical"),
        variables = "tas",
        frequency = "3hr",
        table = NULL,
        index_node = "https://example.org/esg-search"
    )

    expect_false(summary$complete[[1L]])
    expect_identical(summary$table[[1L]], c(tas = "3hr"))
    expect_identical(summary$missing[[1L]], "ssp585:tas")
})

test_that("availability identities do not combine members or grids", {
    variables <- c("tas", "pr")
    split_grid <- data.table::rbindlist(list(
        availability_test__datasets(
            "Model-A",
            "ssp245",
            "tas",
            grid = "gn"
        ),
        availability_test__datasets(
            "Model-A",
            "ssp245",
            "pr",
            grid = "gr"
        )
    ))
    split_member <- data.table::rbindlist(list(
        availability_test__datasets(
            "Model-B",
            "ssp245",
            "tas",
            member = "r1i1p1f1"
        ),
        availability_test__datasets(
            "Model-B",
            "ssp245",
            "pr",
            member = "r2i1p1f1"
        )
    ))
    summary <- availability__summarize(
        data.table::rbindlist(list(split_grid, split_member)),
        experiments = "ssp245",
        variables = variables,
        frequency = "day",
        table = "day",
        index_node = "https://example.org/esg-search"
    )

    expect_equal(nrow(summary), 4L)
    expect_false(any(summary$complete))
    expect_setequal(
        summary$missing,
        c("ssp245:pr", "ssp245:tas")
    )
})

test_that("shift_cmip6_avail builds an unconstrained Dataset query", {
    calls <- new.env(parent = emptyenv())
    datasets <- data.table::rbindlist(lapply(
        c("ssp245", "historical"),
        function(experiment) {
            availability_test__datasets(
                "Model-A",
                experiment,
                c("tas", "pr")
            )
        }
    ))
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls$request <- request
            calls$store <- store
            calls$ui <- ui
            datasets
        },
        .package = "epwshiftr"
    )

    result <- shift_cmip6_avail(
        variables = c("tas", "pr"),
        scenarios = "ssp245",
        source = NULL,
        frequency = "day",
        index_node = "https://example.org/esg-search",
        filters = list(table_id = "Amon"),
        store = "availability-store",
        ui = "availability-ui"
    )
    request <- calls$request

    expect_true(result$complete[[1L]])
    expect_null(request@meta$source)
    expect_null(request@meta$variant)
    expect_equal(request@meta$experiment, c("ssp245", "historical"))
    expect_equal(request@meta$variables, c("tas", "pr"))
    expect_equal(request@meta$frequency, "day")
    expect_null(request@meta$filters$table_id)
    expect_equal(
        request@meta$filters$activity_id,
        c("ScenarioMIP", "CMIP")
    )
    expect_identical(calls$store, "availability-store")
    expect_identical(calls$ui, "availability-ui")
})

test_that("availability discovers every member without a preferred label", {
    calls <- new.env(parent = emptyenv())
    datasets <- data.table::rbindlist(list(
        availability_test__datasets(
            "Model-A",
            "ssp585",
            "tas",
            member = "r1i1p1f1"
        ),
        availability_test__datasets(
            "Model-A",
            "ssp585",
            c("tas", "pr"),
            member = "r2i1p1f1"
        )
    ))
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls$request <- request
            datasets
        },
        .package = "epwshiftr"
    )

    result <- shift_cmip6_avail(
        variables = c("tas", "pr"),
        scenarios = "ssp585",
        include_historical = FALSE,
        index_node = "https://example.org/esg-search"
    )

    expect_null(calls$request@meta$variant)
    expect_identical(result$variant_label, c("r2i1p1f1", "r1i1p1f1"))
    expect_identical(result$complete, c(TRUE, FALSE))
})

test_that("availability accepts named table overrides", {
    calls <- new.env(parent = emptyenv())
    datasets <- data.table::rbindlist(list(
        availability_test__datasets(
            "Model-A",
            "ssp585",
            "tas",
            frequency = "3hr",
            table = "3hr"
        ),
        availability_test__datasets(
            "Model-A",
            "ssp585",
            "uas",
            frequency = "3hr",
            table = "E3hr"
        )
    ))
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls$request <- request
            datasets
        },
        .package = "epwshiftr"
    )

    result <- shift_cmip6_avail(
        variables = c("tas", "uas"),
        scenarios = "ssp585",
        include_historical = FALSE,
        frequency = "3hr",
        table = c(uas = "E3hr"),
        index_node = "https://example.org/esg-search"
    )

    expect_true(result$complete[[1L]])
    expect_setequal(
        calls$request@meta$filters$table_id,
        c("3hr", "E3hr")
    )
    expect_identical(result$table[[1L]], c(tas = "3hr", uas = "E3hr"))
})

test_that("shift_cmip6_avail supports the named ORNL Bridge endpoint", {
    bridge_url <- "https://esgf-node.ornl.gov/esgf-1-5-bridge"
    expect_identical(availability__index_node("llnl"), bridge_url)

    calls <- new.env(parent = emptyenv())
    datasets <- availability_test__datasets(
        "Model-A",
        "ssp245",
        c("tas", "pr")
    )
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls$request <- request
            datasets
        },
        .package = "epwshiftr"
    )

    result <- shift_cmip6_avail(
        variables = c("tas", "pr"),
        scenarios = "ssp245",
        include_historical = FALSE,
        index_node = "ORNL"
    )
    request <- calls$request
    url <- shift_resolve__as_query(request)$url()
    decoded_url <- curl::curl_unescape(url)

    expect_true(result$complete[[1L]])
    expect_identical(
        request@meta$options$index_node,
        bridge_url
    )
    expect_true(startsWith(
        url,
        "https://esgf-node.ornl.gov/esgf-1-5-bridge?"
    ))
    expect_true(grepl(
        "variable_id=tas,pr",
        decoded_url,
        fixed = TRUE
    ))
    expect_false(grepl("fields=", decoded_url, fixed = TRUE))
})

test_that("availability can omit historical and returns a typed empty table", {
    calls <- new.env(parent = emptyenv())
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls$request <- request
            data.table::data.table()
        },
        .package = "epwshiftr"
    )
    result <- shift_cmip6_avail(
        variables = "tas",
        scenarios = "ssp585",
        include_historical = FALSE,
        index_node = "https://example.org/esg-search"
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0L)
    expect_named(result, names(availability__empty()))
    expect_type(result$table, "list")
    expect_equal(calls$request@meta$experiment, "ssp585")
    expect_equal(calls$request@meta$filters$activity_id, "ScenarioMIP")
})

test_that("availability can discover tables for frequencies without defaults", {
    calls <- new.env(parent = emptyenv())
    local_mocked_bindings(
        availability__collect = function(request, store, ui) {
            calls$request <- request
            availability_test__datasets(
                "Model-A",
                "ssp585",
                "orog",
                frequency = "fx",
                table = "fx"
            )
        },
        .package = "epwshiftr"
    )
    result <- shift_cmip6_avail(
        variables = "orog",
        scenarios = "ssp585",
        include_historical = FALSE,
        frequency = "fx",
        index_node = "https://example.org/esg-search"
    )

    expect_true(result$complete[[1L]])
    expect_null(calls$request@meta$filters$table_id)
    expect_identical(result$table[[1L]], c(orog = "fx"))
})

test_that("availability ranks coverage before default and lexical table choices", {
    datasets <- data.table::rbindlist(list(
        availability_test__datasets("Coverage", "ssp245", "tas"),
        availability_test__datasets(
            "Coverage",
            c("historical", "ssp245"),
            "tas",
            table = "Eday"
        ),
        availability_test__datasets(
            "Default",
            c("historical", "ssp245"),
            "tas",
            table = "Aday"
        ),
        availability_test__datasets(
            "Default",
            c("historical", "ssp245"),
            "tas"
        ),
        availability_test__datasets("Lexical", "ssp245", "tas", table = "Zday"),
        availability_test__datasets("Lexical", "ssp245", "tas", table = "Eday")
    ))
    # Repeated replica records must not inflate experiment coverage.
    datasets <- data.table::rbindlist(list(
        datasets,
        datasets[1L],
        datasets[1L]
    ))
    result <- availability__summarize(
        datasets,
        c("ssp245", "historical"),
        c("tas", "pr"),
        "day",
        NULL,
        "local"
    )
    expect_identical(result$source_id, c("Coverage", "Default", "Lexical"))
    expect_identical(
        result$table,
        list(
            c(tas = "Eday", pr = NA_character_),
            c(tas = "day", pr = NA_character_),
            c(tas = "Eday", pr = NA_character_)
        )
    )
    expect_identical(result$available_pairs, c(2L, 2L, 1L))
    expect_identical(
        result$missing,
        c(
            "historical:pr; ssp245:pr",
            "historical:pr; ssp245:pr",
            "historical:pr; historical:tas; ssp245:pr"
        )
    )
})

test_that("availability keeps variable-specific frequency and table matches", {
    datasets <- availability_test__datasets(
        "Model-A",
        "ssp245",
        c("tas", "tasmax"),
        frequency = c(tas = "3hr", tasmax = "day"),
        table = c(tas = "3hr", tasmax = "day")
    )
    # These rows use valid requested frequencies or tables for the wrong variable.
    wrong_frequency <- data.table::copy(datasets)
    data.table::set(wrong_frequency, j = "experiment_id", value = "historical")
    data.table::set(wrong_frequency, j = "frequency", value = c("day", "3hr"))
    wrong_table <- data.table::copy(datasets)
    data.table::set(wrong_table, j = "experiment_id", value = "historical")
    data.table::set(wrong_table, j = "table_id", value = c("day", "3hr"))
    result <- availability__summarize(
        data.table::rbindlist(list(datasets, wrong_frequency, wrong_table)),
        c("ssp245", "historical"),
        c("tasmax", "tas"),
        c(tasmax = "day", tas = "3hr"),
        c(tasmax = "day", tas = "3hr"),
        "local"
    )
    expect_identical(result$available_pairs, 2L)
    expect_identical(result$complete_experiments, 1L)
    expect_identical(result$missing, "historical:tas; historical:tasmax")
    expect_identical(result$table[[1L]], c(tasmax = "day", tas = "3hr"))
    expect_identical(
        result$frequency_spec[[1L]],
        c(tasmax = "day", tas = "3hr")
    )
})

test_that("availability ignores invalid partitions and preserves empty types", {
    valid <- availability_test__datasets("Model-A", "ssp245", "tas")
    incomplete <- data.table::copy(valid)
    data.table::set(incomplete, j = "source_id", value = "Incomplete")
    data.table::set(incomplete, j = "table_id", value = NA_character_)
    result <- availability__summarize(
        data.table::rbindlist(list(valid, incomplete)),
        "ssp245",
        "tas",
        "day",
        NULL,
        "local"
    )
    expect_identical(result$source_id, "Model-A")
    expect_true(result$complete)
    all_missing <- data.table::as.data.table(lapply(valid, function(x) {
        rep(NA, length(x))
    }))
    for (datasets in list(
        valid[0L],
        data.table::data.table(),
        all_missing,
        incomplete
    )) {
        result <- availability__summarize(
            datasets,
            "ssp245",
            "tas",
            "day",
            NULL,
            "local"
        )
        expect_identical(result, availability__empty())
    }
})

test_that("method queries share a union catalog and keep rejected identities", {
    calls <- list()
    datasets <- data.table::rbindlist(list(
        availability_test__datasets(
            source = "Temperature-only",
            member_column = "variant_label"
        ),
        availability_test__datasets(
            source = "Full",
            variables = c("tas", "tasmin", "tasmax", "huss", "ps"),
            member_column = "variant_label"
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
    catalog <- availability_test__datasets(
        experiment = "ssp245",
        variables = variables,
        frequency = "mon",
        table = "Amon",
        member_column = "variant_label"
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
        availability_test__datasets(
            experiment = "historical",
            variables = alternatives[[1L]],
            frequency = "mon",
            table = "Amon",
            member_column = "variant_label"
        ),
        availability_test__datasets(
            experiment = "ssp245",
            variables = alternatives[[2L]],
            frequency = "mon",
            table = "Amon",
            member_column = "variant_label"
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

    response <- availability_test__datasets(
        source = c("Wanted", "Other"),
        member_column = "variant_label"
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
    response <- availability_test__datasets(member_column = "variant_label")
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
    response <- availability_test__datasets(
        source = paste0("Model-", 1:50),
        member_column = "variant_label"
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
    datasets <- availability_test__datasets(
        experiment = c("historical", "ssp245", "ssp585"),
        variables = c("tas", "tasmin", "tasmax", "huss", "ps"),
        member_column = "variant_label"
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

# Resolve public transform constructors only in this test convenience helper.
# eligibility_test__evaluate {{{
eligibility_test__evaluate <- function(
    catalog,
    methods = NULL,
    transform = NULL,
    scenarios = c("ssp245", "ssp585"),
    common = FALSE,
    include_optional_historical = FALSE
) {
    transforms <- shift_batch__transforms(methods, transform)
    historical <- vapply(
        transforms,
        function(value) {
            "model_historical" %in%
                names(value@required_inputs) ||
                (include_optional_historical &&
                    "model_historical" %in% names(value@optional_inputs))
        },
        logical(1L)
    )
    requirements <- eligibility__requirements(transforms, scenarios, historical)
    eligibility__summarize(
        eligibility__match(eligibility__catalog(catalog), requirements),
        scenarios,
        common
    )
}
# }}}

# Build self-contained Dataset rows without querying any external service.
# eligibility_test__catalog {{{
eligibility_test__catalog <- function(
    variables = "tas",
    source = "Model-A",
    experiments = c("historical", "ssp245"),
    frequency = "day",
    table = "day",
    member = "r1i1p1f1",
    grid = "gn"
) {
    data.table::CJ(
        source_id = source,
        experiment_id = experiments,
        member_id = member,
        grid_label = grid,
        variable_id = variables,
        frequency = frequency,
        table_id = table,
        sorted = FALSE
    )
}
# }}}

test_that("method eligibility expands candidates without a universal variable list", {
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(source = "Temperature-only"),
        eligibility_test__catalog(
            c("tas", "tasmin", "tasmax", "huss", "ps"),
            source = "Full-inputs"
        )
    ))
    result <- eligibility_test__evaluate(
        catalog,
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245"
    )

    expect_equal(nrow(result$matrix), 4L)
    expect_equal(sum(result$matrix$selected), 3L)
    incomplete <- subset(
        result$matrix,
        source_id == "Temperature-only" & method == "sobie_curry"
    )
    expect_false(incomplete$selected)
    expect_match(incomplete$missing, "historical:huss")
    expect_match(incomplete$missing, "ssp245:ps")
    expect_identical(unique(result$matrix$period_coverage), "not_checked")
    expect_identical(unique(result$matrix$readability), "not_checked")
    expect_identical(unique(result$matrix$quality), "not_checked")
    common <- eligibility_test__evaluate(
        catalog,
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245",
        common = TRUE
    )
    expect_equal(sum(common$matrix$selected), 2L)
    expect_identical(
        unique(common$matrix$source_id[common$matrix$selected]),
        "Full-inputs"
    )
    expect_identical(
        result$matrix$method_eligible,
        common$matrix$method_eligible
    )
    # A different pool policy must not change matching evidence or input paths.
    expect_identical(result$requirements, common$requirements)
    expect_identical(
        result$matrix$catalog_eligible,
        common$matrix$catalog_eligible
    )
    expect_identical(result$matrix$path_id, common$matrix$path_id)
})

test_that("required historical roles remain required", {
    result <- eligibility_test__evaluate(
        eligibility_test__catalog(experiments = "ssp245"),
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_equal(nrow(result$matrix), 1L)
    expect_false(result$matrix$selected)
    expect_match(result$matrix$missing, "historical:tas")
})

test_that("alternative input paths preserve optional historical policy", {
    transform <- monthly_transform("epwshiftr")
    # Use the published registered relative-humidity alternative, not a
    # hand-maintained list that can drift from the weather method contract.
    alternatives <- transform@required_inputs$model_future@variable_sets
    alternative <- which(vapply(
        alternatives,
        function(variables) {
            "hurs" %in% variables && !"huss" %in% variables
        },
        logical(1L)
    ))
    variables <- alternatives[[alternative]]
    catalog <- eligibility_test__catalog(
        variables,
        experiments = "ssp245",
        frequency = "mon",
        table = "Amon"
    )
    result <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245"
    )

    expect_true(result$matrix$selected)
    chosen <- subset(result$requirements, path_id == result$matrix$path_id)
    expect_identical(chosen$alternative, as.integer(alternative))
    expect_identical(chosen$variables[[1L]], variables)
    expect_identical(unique(result$requirements$role), "model_future")
    expect_true(any(!result$requirements$complete))
    optional <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_false(optional$matrix$selected)
    expect_match(optional$matrix$missing, "historical:")
    expect_true("model_historical" %in% optional$requirements$role)
})

test_that("joint alternatives cannot switch across scenarios or shared roles", {
    transform <- monthly_transform("epwshiftr")
    alternatives <- transform@required_inputs$model_future@variable_sets
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(
            alternatives[[1L]],
            experiments = "ssp245",
            frequency = "mon",
            table = "Amon"
        ),
        eligibility_test__catalog(
            alternatives[[2L]],
            experiments = "ssp585",
            frequency = "mon",
            table = "Amon"
        )
    ))
    result <- eligibility_test__evaluate(catalog, transform = transform)
    expect_false(any(result$matrix$selected))
    expect_true(any(result$matrix$catalog_eligible))
    expect_match(
        result$matrix$missing[result$matrix$catalog_eligible],
        "No single input path"
    )
})

test_that("source members and grids cannot be stitched together", {
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(experiments = "ssp245", grid = "gn"),
        eligibility_test__catalog(experiments = "historical", grid = "gr"),
        eligibility_test__catalog(source = "Model-B", experiments = "ssp245"),
        eligibility_test__catalog(
            source = "Model-B",
            experiments = "historical",
            member = "r2i1p1f1"
        )
    ))
    result <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_equal(nrow(result$matrix), 4L)
    expect_false(any(result$matrix$selected))
    expect_true(all(!is.na(result$matrix$missing)))
})

test_that("frequency and table partitions cannot be stitched across experiments", {
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(experiments = "ssp245", table = "day"),
        eligibility_test__catalog(experiments = "historical", table = "Eday"),
        eligibility_test__catalog(source = "Wrong-frequency", frequency = "mon")
    ))
    result <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_false(any(result$matrix$selected))
    valid_frequency <- subset(result$requirements, source_id == "Model-A")
    expect_identical(unique(unlist(valid_frequency$table)), "day")
    expect_match(result$matrix$missing[[1L]], "historical:tas")
    expect_true(all(
        !subset(result$requirements, source_id == "Wrong-frequency")$complete
    ))
})

test_that("mixed frequencies and calendar requirements come from registered roles", {
    transform <- hourly_transform("kernel_qdm")
    requirement <- transform@required_inputs$model_future
    variables <- requirement@variable_sets[[1L]]
    catalog <- data.table::rbindlist(lapply(variables, function(variable) {
        frequency <- requirement@variable_frequencies[[variable]][[1L]]
        eligibility_test__catalog(
            variable,
            frequency = frequency,
            table = if (frequency == "day") "day" else "3hr"
        )
    }))
    result <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245"
    )
    expect_true(result$matrix$selected)
    detail <- subset(result$requirements, role == "model_future")
    expect_identical(
        unname(detail$frequency_spec[[1L]]),
        unname(vapply(
            variables,
            function(variable) {
                requirement@variable_frequencies[[variable]][[1L]]
            },
            character(1L)
        ))
    )
    expect_identical(detail$calendars[[1L]], requirement@calendars)
    # Removing one point-frequency variable must reject the method even when
    # a mean-frequency Dataset with the same variable ID is present.
    point <- variables[vapply(
        requirement@variable_frequencies[variables],
        function(value) "3hrPt" %in% value,
        logical(1L)
    )][[1L]]
    catalog$frequency[catalog$variable_id == point] <- "3hr"
    rejected <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245"
    )
    expect_false(rejected$matrix$selected)
    expect_match(rejected$matrix$missing, paste0(":", point))
})

test_that("allowed frequencies are not reduced to their first choice", {
    transform <- daily_transform("qdm")
    # An explicit transform can extend its own contract; the offline evaluator
    # must inspect that object rather than reconstructing a default recipe.
    inputs <- transform@required_inputs
    for (role in c("model_future", "model_historical")) {
        inputs[[role]]@frequencies <- c("day", "mon")
    }
    transform@required_inputs <- inputs
    result <- eligibility_test__evaluate(
        eligibility_test__catalog(
            frequency = "mon",
            table = "Amon"
        ),
        transform = transform,
        scenarios = "ssp245"
    )
    expect_true(result$matrix$selected)
    expect_identical(unique(unlist(result$requirements$frequency_spec)), "mon")
    inputs$model_historical@frequencies <- "day"
    transform@required_inputs <- inputs
    incompatible <- eligibility_test__evaluate(
        eligibility_test__catalog(
            frequency = "mon",
            table = "Amon"
        ),
        transform = transform,
        scenarios = "ssp245"
    )
    expect_false(incompatible$matrix$selected)
})

test_that("offline results are deterministic and do not mutate or query the catalog", {
    catalog <- data.table::as.data.table(eligibility_test__catalog())
    original <- data.table::copy(catalog)
    local_mocked_bindings(
        availability__collect = function(...) stop("Unexpected Dataset query"),
        shift_resolve__cmip6_period_coverage = function(...) {
            stop("Unexpected File query")
        },
        .package = "epwshiftr"
    )
    result <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_identical(catalog, original)
    reversed <- eligibility_test__evaluate(
        catalog[nrow(catalog):1L],
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_identical(result, reversed)
    # Missing provider identity aliases must fall back to member_id.
    catalog$variant_label <- NA_character_
    expect_identical(
        result,
        eligibility_test__evaluate(
            catalog,
            methods = "qdm",
            scenarios = "ssp245"
        )
    )
})

test_that("empty catalogs and malformed inputs have explicit contracts", {
    catalog <- eligibility_test__catalog()[0L]
    empty <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_equal(nrow(empty$matrix), 0L)
    expect_equal(nrow(empty$requirements), 0L)
    expect_type(empty$matrix$selected, "logical")
    expect_error(
        eligibility_test__evaluate(
            catalog,
            methods = "qdm",
            scenarios = "historical"
        ),
        "future experiment"
    )
    expect_error(
        eligibility_test__evaluate(
            catalog,
            methods = "qdm",
            scenarios = c("ssp245", "ssp245")
        ),
        "duplicated"
    )
    expect_error(eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        common = "unknown"
    ))
    expect_error(
        eligibility_test__evaluate(
            catalog,
            methods = "qdm",
            transform = daily_transform("qdm")
        ),
        "either"
    )
    expect_error(
        eligibility_test__evaluate(catalog, methods = "missing"),
        "Unknown"
    )
})

test_that("internal eligibility preserves data.table outputs and caller indexes", {
    catalog <- eligibility_test__catalog(c(
        "tas",
        "tasmin",
        "tasmax",
        "huss",
        "ps"
    ))
    data.table::setkeyv(catalog, c("member_id", "variable_id"))
    data.table::setindexv(catalog, "experiment_id")
    original <- data.table::copy(catalog)
    indexes <- data.table::indices(catalog)
    result <- eligibility_test__evaluate(
        catalog,
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245"
    )
    expect_true(data.table::is.data.table(result$matrix))
    expect_true(data.table::is.data.table(result$requirements))
    expect_identical(catalog, original)
    expect_identical(data.table::key(catalog), data.table::key(original))
    expect_identical(data.table::indices(catalog), indexes)
    empty <- eligibility_test__evaluate(
        catalog[0L],
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_true(data.table::is.data.table(empty$matrix))
    expect_true(data.table::is.data.table(empty$requirements))
    expect_identical(names(result$matrix), names(empty$matrix))
    expect_identical(names(result$requirements), names(empty$requirements))
    # Base input support is an explicit compatibility boundary, not an
    # internal storage format; compare it against the data.table result.
    compatible <- eligibility_test__evaluate(
        as.data.frame(catalog),
        methods = c("qdm", "sobie_curry"),
        scenarios = "ssp245"
    )
    expect_identical(result, compatible)
})

test_that("replicas and missing table partitions cannot manufacture coverage", {
    catalog <- eligibility_test__catalog(experiments = "ssp245")
    once <- eligibility_test__evaluate(
        catalog,
        methods = "qdm",
        scenarios = "ssp245"
    )
    repeated <- data.table::rbindlist(rep(list(catalog), 100L))
    expect_identical(
        once,
        eligibility_test__evaluate(
            repeated,
            methods = "qdm",
            scenarios = "ssp245"
        )
    )
    invalid <- eligibility_test__catalog(experiments = "historical")
    invalid[, table_id := NA_character_]
    combined <- data.table::rbindlist(list(catalog, invalid))
    result <- eligibility_test__evaluate(
        combined,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_false(result$matrix$selected)
    expect_match(result$matrix$missing, "historical:tas")
    expect_identical(unname(result$requirements$table[[1L]]), "day")
})

test_that("joint path numbering retains declared alternative priority", {
    transform <- monthly_transform("epwshiftr")
    roles <- c(transform@required_inputs, transform@optional_inputs)
    variables <- unique(unlist(
        lapply(roles, function(role) {
            unlist(role@variable_sets, use.names = FALSE)
        }),
        use.names = FALSE
    ))
    catalog <- eligibility_test__catalog(
        variables,
        experiments = c("historical", "ssp245", "ssp585"),
        frequency = "mon",
        table = "Amon"
    )
    result <- eligibility_test__evaluate(
        catalog,
        transform = monthly_transform("epwshiftr"),
        include_optional_historical = TRUE
    )
    expect_true(all(result$matrix$selected))
    expect_identical(unique(result$matrix$path_id), 1L)
    expect_true(all(result$requirements$complete))
})

# Cross-period humidity substitutions must not be advertised as executable.
test_that("historical and future periods require the same variable alternative", {
    transform <- monthly_transform("epwshiftr")
    alternatives <- transform@required_inputs$model_future@variable_sets
    catalog <- data.table::rbindlist(list(
        eligibility_test__catalog(
            alternatives[[1L]],
            experiments = "historical",
            frequency = "mon",
            table = "Amon"
        ),
        eligibility_test__catalog(
            alternatives[[2L]],
            experiments = "ssp245",
            frequency = "mon",
            table = "Amon"
        )
    ))
    mixed <- eligibility_test__evaluate(
        catalog,
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_false(mixed$matrix$selected)
    complete <- data.table::rbindlist(list(
        catalog,
        eligibility_test__catalog(
            alternatives[[2L]],
            experiments = "historical",
            frequency = "mon",
            table = "Amon"
        )
    ))
    # Equivalent contracts with different alternative order must still match.
    optional <- transform@optional_inputs
    optional$model_historical@variable_sets <- rev(
        optional$model_historical@variable_sets
    )
    transform@optional_inputs <- optional
    result <- eligibility_test__evaluate(
        complete,
        transform = transform,
        scenarios = "ssp245",
        include_optional_historical = TRUE
    )
    expect_true(result$matrix$selected)
})

test_that("catalog normalization preserves inputs and resolves member aliases", {
    datasets <- data.table::data.table(
        source_id = factor(c("B", "A", "C", "D", "E", "F", "G")),
        experiment_id = "ssp245",
        grid_label = "gn",
        variable_id = "tas",
        frequency = "day",
        table_id = "day",
        variant_label = factor(c("r2i1p1f1", "", NA, "", NA, "", NA)),
        member_id = factor(c(
            "r1i1p1f1",
            "r1i1p1f1",
            "r1i1p1f1",
            "",
            NA,
            NA,
            ""
        )),
        unrelated = list(1:3, 4:6, 7:9, 10:12, 13:15, 16:18, 19:21)
    )
    datasets <- data.table::rbindlist(list(datasets, datasets[1L]))
    data.table::setkeyv(datasets, "source_id")
    data.table::setattr(datasets, "catalog_origin", "fixture")
    before <- data.table::copy(datasets)
    result <- eligibility__catalog(datasets)

    expect_identical(datasets, before)
    expect_identical(data.table::key(datasets), "source_id")
    expect_identical(attr(datasets, "catalog_origin"), "fixture")
    expect_identical(result$source_id, c("A", "B", "C"))
    expect_identical(
        result$variant_label,
        c("r1i1p1f1", "r2i1p1f1", "r1i1p1f1")
    )
    expect_named(
        result,
        c(
            "source_id",
            "experiment_id",
            "grid_label",
            "variable_id",
            "frequency",
            "table_id",
            "variant_label"
        )
    )
    expect_true(all(vapply(result, is.character, logical(1L))))
})

test_that("catalog normalization retains incomplete partitions for diagnostics", {
    datasets <- data.table::data.table(
        source_id = "Model-A",
        member_id = "r1i1p1f1",
        grid_label = "gn"
    )
    result <- eligibility__catalog(datasets)
    expect_equal(nrow(result), 1L)
    expect_identical(result$variant_label, "r1i1p1f1")
    expect_true(all(is.na(result$experiment_id)))
    expect_true(all(is.na(result$frequency)))
    expect_true(all(is.na(result$table_id)))
    expect_true(all(is.na(result$variable_id)))
    # A relevant variable/experiment with missing partition metadata remains
    # visible as a rejected candidate in the method query.
    data.table::set(datasets, j = "experiment_id", value = "ssp245")
    data.table::set(datasets, j = "variable_id", value = "tas")
    rejected <- eligibility_test__evaluate(
        datasets,
        methods = "qdm",
        scenarios = "ssp245"
    )
    expect_identical(rejected$matrix$selected, FALSE)
    expect_match(rejected$matrix$missing, "ssp245:tas")
    expect_match(rejected$matrix$missing, "historical:tas")
    for (empty in list(
        data.table::data.table(),
        datasets[0L],
        data.table::data.table(source_id = NA)
    )) {
        normalized <- eligibility__catalog(empty)
        expect_equal(nrow(normalized), 0L)
        expect_identical(
            vapply(normalized, typeof, character(1L)),
            stats::setNames(rep("character", 7L), names(result))
        )
    }
})

# Public presentation must not change the matched evidence object or its
# attributes; batch discovery consumes that same matching contract directly.
test_that("eligibility summaries preserve matched evidence by reference", {
    transforms <- shift_batch__transforms(methods = "qdm")
    requirements <- eligibility__requirements(
        transforms,
        "ssp245",
        stats::setNames(TRUE, names(transforms))
    )
    details <- eligibility__match(
        eligibility__catalog(eligibility_test__catalog()),
        requirements
    )
    before <- data.table::copy(details)
    result <- eligibility__summarize(details, "ssp245", FALSE)
    expect_identical(details, before)
    expect_identical(result$requirements, before)
    expect_null(data.table::indices(details))
    expect_true(result$matrix$selected)
})

# vim: fdm=marker :
