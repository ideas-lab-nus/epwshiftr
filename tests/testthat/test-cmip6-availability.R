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

# vim: fdm=marker :
