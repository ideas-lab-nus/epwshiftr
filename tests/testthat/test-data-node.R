local_data_node_status_response <- function() {
    list(
        status = "success",
        data = list(
            result = list(
                metric = data.frame(
                    instance = c("node-up.example", "node-down.example"),
                    stringsAsFactors = FALSE
                ),
                value = list(c("0", "1"), c("0", "0"))
            )
        )
    )
}

test_that("data_node_status() returns node states from metagrid status", {
    testthat::with_mocked_bindings(
        cache__url = function(...) local_data_node_status_response(),
        {
            res <- data_node_status(index_node = INDEX_NODES[["ORNL"]])
        },
        .package = "epwshiftr"
    )

    expect_s3_class(res, "data.table")
    expect_named(res, c("data_node", "status"))
    expect_equal(res$data_node, c("node-up.example", "node-down.example"))
    expect_equal(res$status, c("UP", "DOWN"))
})

test_that("data_node_status() probes UP nodes when requested", {
    testthat::with_mocked_bindings(
        cache__url = function(...) local_data_node_status_response(),
        data_node_http_probe = function(node, timeout = 3) {
            expect_equal(node, "node-up.example")
            expect_equal(timeout, 0.25)
            12.5
        },
        {
            res <- data_node_status(speed_test = TRUE, timeout = 0.25, index_node = INDEX_NODES[["ORNL"]])
        },
        .package = "epwshiftr"
    )

    expect_named(res, c("data_node", "status", "probe_ms"))
    expect_equal(res$probe_ms[res$data_node == "node-up.example"], 12.5)
    expect_true(is.na(res$probe_ms[res$data_node == "node-down.example"]))
})

test_that("data_node_status() returns an empty table when status lookup fails", {
    testthat::with_mocked_bindings(
        cache__url = function(...) list(status = "error"),
        {
            expect_message(
                res <- data_node_status(index_node = INDEX_NODES[["ORNL"]]),
                "Failed to retrieve the data node status"
            )
        },
        .package = "epwshiftr"
    )

    expect_s3_class(res, "data.table")
    expect_named(res, c("data_node", "status"))
    expect_equal(nrow(res), 0L)
})
