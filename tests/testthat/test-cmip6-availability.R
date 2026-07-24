# Build variable-specific Dataset rows for deterministic availability tests.
availability_test__datasets <- function(source, experiment, variables,
                                        member = "r1i1p1f1", grid = "gn") {
    data.table::rbindlist(lapply(variables, function(variable) {
        data.table::data.table(
            id = sprintf(
                "CMIP6.%s.%s.%s.%s.%s",
                source, experiment, member, variable, grid
            ),
            source_id = source,
            experiment_id = experiment,
            member_id = member,
            frequency = "day",
            table_id = "day",
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
        store = "availability-store",
        ui = "availability-ui"
    )
    request <- calls$request

    expect_true(result$complete[[1L]])
    expect_null(request@meta$source)
    expect_equal(request@meta$experiment, c("ssp245", "historical"))
    expect_equal(request@meta$variables, c("tas", "pr"))
    expect_equal(request@meta$frequency, "day")
    expect_equal(request@meta$filters$table_id, "day")
    expect_equal(
        request@meta$filters$activity_id,
        c("ScenarioMIP", "CMIP")
    )
    expect_identical(calls$store, "availability-store")
    expect_identical(calls$ui, "availability-ui")
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
    expect_equal(calls$request@meta$experiment, "ssp585")
    expect_equal(calls$request@meta$filters$activity_id, "ScenarioMIP")
})

test_that("availability requires an explicit table for unknown frequencies", {
    expect_error(
        shift_cmip6_avail(
            variables = "tas",
            scenarios = "ssp585",
            frequency = "fx",
            index_node = "https://example.org/esg-search"
        ),
        "set `table` explicitly"
    )
})
