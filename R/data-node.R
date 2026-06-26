# data_node_status {{{
data_node_http_probe <- function(node, timeout = 3) {
    urls <- if (grepl("^https?://", node, ignore.case = TRUE)) {
        node
    } else {
        c(sprintf("https://%s/", node), sprintf("http://%s/", node))
    }

    for (url in urls) {
        handle <- curl::new_handle(
            nobody = TRUE,
            timeout = timeout,
            connecttimeout = timeout,
            followlocation = TRUE,
            failonerror = FALSE
        )
        start <- proc.time()[["elapsed"]]
        ok <- tryCatch(
            {
                curl::curl_fetch_memory(url, handle = handle)
                TRUE
            },
            error = function(e) FALSE
        )
        if (ok) {
            return((proc.time()[["elapsed"]] - start) * 1000)
        }
    }

    NA_real_
}

#' Get status of ESGF data nodes
#'
#' `data_node_status()` is the user-facing replacement for the legacy
#' data-node helper name used in earlier releases.
#'
#' @param speed_test If `TRUE`, perform a lightweight HTTP probe on each `UP`
#'        data node. A `probe_ms` column is appended in returned data.table
#'        which stores elapsed request time in milliseconds. Default: `FALSE`.
#' @param timeout Timeout for each HTTP probe in seconds. Default: `3`.
#' @param index_node The index node to query for data-node status.
#'        Default: `INDEX_NODES[["ORNL"]]`.
#'
#' @return A [data.table::data.table()] of 2 or 3 (when `speed_test` is `TRUE`)
#' columns:
#'
#' | Column      | Type      | Description                                                                     |
#' | -----       | -----     | -----                                                                           |
#' | `data_node` | character | Web address of data node                                                        |
#' | `status`    | character | Status of data node. `"UP"` means OK and `"DOWN"` means currently not available |
#' | `probe_ms`  | double    | HTTP probe elapsed time in milliseconds for `UP` data nodes                     |
#'
#' @examples
#' \dontrun{
#' data_node_status()
#' }
#'
#' @export
data_node_status <- function(speed_test = FALSE, timeout = 3, index_node = INDEX_NODES[["ORNL"]]) {
    checkmate::assert_flag(speed_test)
    checkmate::assert_number(timeout, lower = 0)

    empty_nodes <- function() {
        if (speed_test) {
            data.table::data.table(
                data_node = character(),
                status = character(),
                probe_ms = numeric()
            )
        } else {
            data.table::data.table(
                data_node = character(),
                status = character()
            )
        }
    }

    # use the metagrid-backend to get the data node status
    # see: https://github.com/esgf2-us/metagrid/blob/2e90dd10317506a82f120217e39c4a3cde6a7560/backend/.envs/.django#L30
    #      https://github.com/ESGF/esgf-utils/blob/master/node_status/query_prom.py
    path <- "proxy/status"
    parsed <- query__normalize_node(index_node, raw = TRUE)
    if (parsed$path == "/esgf-1-5-bridge") {
        url <- curl::curl_modify_url(parsed$url, path = path)
    } else {
        url <- curl::curl_modify_url(parsed$url, path = paste(parsed$path, path, sep = "/"))
    }

    msg <- NULL
    res <- cache__url(
        "datanode",
        url,
        function() {
            tryCatch(
                jsonlite::fromJSON(url),
                warning = function(w) {
                    msg <<- conditionMessage(w)
                    NULL
                },
                error = function(e) {
                    msg <<- conditionMessage(e)
                    NULL
                }
            )
        },
        validate = function(res) !is.null(res)
    )

    # nocov start
    if (is.null(res) || res$status != "success") {
        message("Failed to retrieve the data node status from aims2.llnl.gov. Reason:\n  ", msg)
        return(empty_nodes())
    }
    # nocov end

    res <- data.table::data.table(
        data_node = res$data$result$metric$instance,
        status = data.table::fifelse(
            vapply(res$data$result$value, .subset2, character(1L), 2L) == "1",
            "UP",
            "DOWN"
        )
    )

    data.table::setorderv(res, "status", -1)

    if (!speed_test) {
        return(res)
    }

    # nocov start
    res[, probe_ms := NA_real_]

    if (!length(nodes_up <- res$data_node[res$status == "UP"])) {
        message("No working data nodes available now. Skip HTTP probe")
        return(res)
    }
    # nocov end

    probe <- vapply(
        nodes_up,
        function(node) {
            message(sprintf("Probing data node '%s'...", node))
            data_node_http_probe(node, timeout = timeout)
        },
        numeric(1)
    )

    res[status == "UP", probe_ms := probe][order(probe_ms)]
}
# }}}
