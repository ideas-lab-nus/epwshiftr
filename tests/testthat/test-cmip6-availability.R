# Build variable-specific Dataset rows for deterministic availability tests.
availability_test__datasets <- function(source, experiment, variables,
                                        member = "r1i1p1f1", grid = "gn",
                                        frequency = "day", table = "day") {
    data.table::rbindlist(lapply(variables, function(variable) {
        variable_frequency <- if (!is.null(names(frequency))) {
            unname(frequency[[variable]])
        } else {
            frequency[[1L]]
        }
        variable_table <- if (!is.null(names(table))) {
            unname(table[[variable]])
        } else {
            table[[1L]]
        }
        data.table::data.table(
            id = sprintf(
                "CMIP6.%s.%s.%s.%s.%s",
                source, experiment, member, variable, grid
            ),
            source_id = source,
            experiment_id = experiment,
            member_id = member,
            frequency = variable_frequency,
            table_id = variable_table,
            variable_id = variable,
            grid_label = grid,
            latest = TRUE,
            replica = FALSE,
            size = 1
        )
    }), use.names = TRUE, fill = TRUE)
}

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
            "Model-B", "ssp585", c("tas", "hurs")),
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
            "Model-A", experiments[[1L]], "tas",
            frequency = "3hr", table = "3hr"
        ),
        availability_test__datasets(
            "Model-A", experiments[[2L]], "tas",
            frequency = "3hr", table = "3hr"
        ),
        availability_test__datasets(
            "Model-A", experiments[[1L]], "uas",
            frequency = "3hr", table = "E3hr"
        ),
        availability_test__datasets(
            "Model-A", experiments[[2L]], "uas",
            frequency = "3hr", table = "E3hr"
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
            "Model-A", "historical", "tas",
            frequency = "3hr", table = "3hr"
        ),
        availability_test__datasets(
            "Model-A", "ssp585", "tas",
            frequency = "3hr", table = "E3hr"
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
            "Model-A", "ssp245", "tas", grid = "gn"),
        availability_test__datasets(
            "Model-A", "ssp245", "pr", grid = "gr")
    ))
    split_member <- data.table::rbindlist(list(
        availability_test__datasets(
            "Model-B", "ssp245", "tas", member = "r1i1p1f1"),
        availability_test__datasets(
            "Model-B", "ssp245", "pr", member = "r2i1p1f1")
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
                "Model-A", experiment, c("tas", "pr"))
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
            "Model-A", "ssp585", "tas", member = "r1i1p1f1"
        ),
        availability_test__datasets(
            "Model-A", "ssp585", c("tas", "pr"), member = "r2i1p1f1"
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
            "Model-A", "ssp585", "tas",
            frequency = "3hr", table = "3hr"
        ),
        availability_test__datasets(
            "Model-A", "ssp585", "uas",
            frequency = "3hr", table = "E3hr"
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
        "Model-A", "ssp245", c("tas", "pr"))
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
    url <- shift_as_query(request)$url()
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
        "variable_id=tas,pr", decoded_url, fixed = TRUE))
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
                "Model-A", "ssp585", "orog",
                frequency = "fx", table = "fx"
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
