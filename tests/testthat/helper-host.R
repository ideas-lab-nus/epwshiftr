get_fast_index_node <- function(force = FALSE) {
    cache <- get_test_cache()
    cache_key <- "fast_index_node"

    if (!force) {
        cached <- cache$get(cache_key)
        if (!is.key_missing(cached)) {
            return(cached)
        }
    }

    nodes <- INDEX_NODES

    conn_time <- function(node) {
        before <- now()
        res <- suppressWarnings(
            try(readLines(substr(node, 1L, nchar(node) - 11L), n = 1L, warn = FALSE), silent = TRUE)
        )
        if (inherits(res, "try-error")) res <- Inf
        after <- now()
        as.numeric(after - before)
    }

    times <- vapply(nodes, conn_time, double(1L))
    # all failed to connect, just use the first one
    if (all(is.infinite(times))) {
        result <- nodes[1L]
    } else {
        result <- nodes[which.min(times)]
    }

    cache$set(cache_key, result)
    result
}
