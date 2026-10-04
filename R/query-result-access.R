#' @include query-result.R
NULL

# Check source endpoints and reuse their reachability and latency evidence.

# query_result__url_scheme {{{
query_result__url_scheme <- function(url) {
    url <- as.character(url)
    out <- rep(NA_character_, length(url))
    ok <- !is.na(url) & nzchar(url)
    windows_drive_path <- ok & grepl("^[A-Za-z]:[/\\\\]", url)
    has_scheme <- ok &
        !windows_drive_path &
        grepl("^[A-Za-z][A-Za-z0-9+.-]*:", url)
    out[has_scheme] <- tolower(sub(
        "^([A-Za-z][A-Za-z0-9+.-]*):.*$",
        "\\1",
        url[has_scheme]
    ))
    out
}
# }}}

# query_result__url_http {{{
query_result__url_http <- function(url) {
    query_result__url_scheme(url) %in% c("http", "https")
}
# }}}

# query_result__url_local {{{
query_result__url_local <- function(url) {
    scheme <- query_result__url_scheme(url)
    missing <- is.na(url) | !nzchar(url)
    !missing & (is.na(scheme) | scheme == "file")
}
# }}}

# query_result__url_path {{{
query_result__url_path <- function(url) {
    url <- as.character(url[[1L]])
    scheme <- query_result__url_scheme(url)
    if (identical(scheme, "file")) {
        path <- sub("^file://", "", url, ignore.case = TRUE)
        path <- sub("^localhost(?=/)", "", path, perl = TRUE)
        return(utils::URLdecode(path))
    }

    url
}
# }}}

# query_result__url_host {{{
query_result__url_host <- function(url) {
    url <- as.character(url)
    out <- rep(NA_character_, length(url))
    use <- query_result__url_http(url)
    out[use] <- sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/:?#]+).*$", "\\1", url[use])
    out
}
# }}}

# query_result__reach_missing {{{
query_result__reach_missing <- function(error) {
    list(reachable = NA, latency_ms = NA_real_, error = error)
}
# }}}

# query_result__reach_local {{{
query_result__reach_local <- function(url) {
    path <- query_result__url_path(url)
    ok <- file.exists(path)
    list(
        reachable = ok,
        latency_ms = 0,
        error = if (ok) NA_character_ else "File does not exist."
    )
}
# }}}

# query_result__reach_config {{{
query_result__reach_config <- function(
    probe = NULL,
    include_level = FALSE,
    default_level = "data_node"
) {
    defaults <- list(
        timeout = 5,
        concurrency = 1L,
        network_policy = NULL,
        cache_seconds = 3600L,
        cache_failures_seconds = 0L
    )
    if (isTRUE(include_level)) {
        defaults$level <- default_level
    }
    if (!is.null(probe)) {
        if (!is.list(probe) || is.data.frame(probe)) {
            cli::cli_abort("`probe` must be `NULL` or a named list.")
        }
        if (length(probe)) {
            names <- names(probe)
            if (is.null(names) || any(!nzchar(names))) {
                cli::cli_abort("`probe` must be a named list.")
            }
            unknown <- setdiff(names, names(defaults))
            if (length(unknown)) {
                cli::cli_abort("Unknown `probe` field{?s}: {.field {unknown}}.")
            }
            defaults[names] <- probe
        }
    }

    query_result__reach_check(
        timeout = defaults$timeout,
        network_policy = defaults$network_policy,
        probe_concurrency = defaults$concurrency
    )
    checkmate::assert_count(defaults$cache_seconds, positive = FALSE)
    checkmate::assert_count(defaults$cache_failures_seconds, positive = FALSE)
    defaults$concurrency <- as.integer(defaults$concurrency)
    defaults$cache_seconds <- as.integer(defaults$cache_seconds)
    defaults$cache_failures_seconds <- as.integer(
        defaults$cache_failures_seconds
    )
    if (isTRUE(include_level)) {
        defaults$level <- match.arg(defaults$level, c("data_node", "url"))
    }

    defaults
}
# }}}

# query_result__reach_check {{{
query_result__reach_check <- function(
    timeout = 5,
    network_policy = NULL,
    probe_concurrency = NULL
) {
    checkmate::assert_number(timeout, lower = 0, finite = TRUE)
    if (timeout <= 0) {
        cli::cli_abort("`timeout` must be greater than zero.")
    }
    if (!is.null(network_policy)) {
        checkmate::assert_list(network_policy, names = "unique")
    }
    if (!is.null(probe_concurrency)) {
        checkmate::assert_count(probe_concurrency, positive = TRUE)
    }

    invisible(NULL)
}
# }}}

# query_result__node_urls {{{
query_result__node_urls <- function(node) {
    node <- as.character(node[[1L]])
    if (is.na(node) || !nzchar(node)) {
        return(character())
    }
    if (grepl("^https?://", node, ignore.case = TRUE)) {
        return(node)
    }

    c(sprintf("https://%s/", node), sprintf("http://%s/", node))
}
# }}}

# query_result__node_try {{{
query_result__node_try <- function(url, timeout = 5, network_policy = NULL) {
    if (is.null(network_policy)) {
        network_policy <- list()
    }
    connect_timeout <- network_policy$connect_timeout
    if (is.null(connect_timeout)) {
        connect_timeout <- min(timeout, 3)
    }
    ssl_verifypeer <- network_policy$ssl_verifypeer
    if (is.null(ssl_verifypeer)) {
        ssl_verifypeer <- TRUE
    }

    start <- proc.time()[["elapsed"]]
    tryCatch(
        {
            handle <- downloader__curl_handle(
                timeout = timeout,
                connect_timeout = connect_timeout,
                ssl_verifypeer = ssl_verifypeer,
                proxy = network_policy$proxy,
                useragent = network_policy$useragent,
                nobody = TRUE
            )
            curl::handle_setopt(handle, failonerror = FALSE)
            curl::curl_fetch_memory(url, handle = handle)
            list(
                reachable = TRUE,
                latency_ms = (proc.time()[["elapsed"]] - start) * 1000,
                error = NA_character_,
                probe_url = url
            )
        },
        error = function(e) {
            list(
                reachable = FALSE,
                latency_ms = NA_real_,
                error = conditionMessage(e),
                probe_url = url
            )
        }
    )
}
# }}}

# query_result__reach_node_url {{{
query_result__reach_node_url <- function(
    url,
    timeout = 5,
    network_policy = NULL
) {
    query_result__reach_check(timeout, network_policy)
    if (is.na(url) || !nzchar(url)) {
        probe <- query_result__reach_missing("Missing URL.")
        probe$probe_url <- NA_character_
        return(probe)
    }
    if (!query_result__url_http(url)) {
        probe <- query_result__reach_missing("Unsupported URL scheme.")
        probe$probe_url <- url
        return(probe)
    }

    query_result__node_try(
        url,
        timeout = timeout,
        network_policy = network_policy
    )
}
# }}}

# Execute a normalized batch of URL checks through one shared curl pool.
# query_result__run_url_checks {{{
query_result__run_url_checks <- function(
    urls,
    timeout,
    network_policy,
    concurrency,
    serial_check,
    done_result,
    clock = function() proc.time()[["elapsed"]],
    failonerror = NULL,
    nobody = TRUE,
    request_url = function(url) url,
    retry_failed = TRUE
) {
    checkmate::assert_flag(retry_failed)
    # Keep serial execution authoritative for one target and as the fallback path.
    serial <- function(targets) {
        stats::setNames(lapply(targets, serial_check), targets)
    }
    if (!length(urls)) {
        return(stats::setNames(list(), character()))
    }
    if (concurrency <= 1L || length(urls) <= 1L) {
        return(serial(urls))
    }

    out <- vector("list", length(urls))
    names(out) <- urls
    failed <- rep(FALSE, length(urls))
    failure_messages <- rep(NA_character_, length(urls))
    tryCatch(
        {
            if (is.null(network_policy)) {
                network_policy <- list()
            }
            connect_timeout <- network_policy$connect_timeout
            if (is.null(connect_timeout)) {
                connect_timeout <- min(timeout, 3)
            }
            ssl_verifypeer <- network_policy$ssl_verifypeer
            if (is.null(ssl_verifypeer)) {
                ssl_verifypeer <- TRUE
            }

            # Register one active wave at a time. libcurl counts time spent in
            # a multi queue against CURLOPT_TIMEOUT, so enqueuing hundreds of
            # handles at once can expire later records before they are started.
            starts <- seq.int(1L, length(urls), by = concurrency)
            for (start in starts) {
                indices <- seq.int(
                    start,
                    min(start + concurrency - 1L, length(urls))
                )
                pool <- curl::new_pool(
                    total_con = length(indices),
                    host_con = length(indices)
                )
                # Capture each index and start value before registering its
                # callbacks in the current active wave.
                for (i in indices) {
                    local({
                        j <- i
                        started_at <- clock()
                        handle <- downloader__curl_handle(
                            timeout = timeout,
                            connect_timeout = connect_timeout,
                            ssl_verifypeer = ssl_verifypeer,
                            proxy = network_policy$proxy,
                            useragent = network_policy$useragent,
                            nobody = nobody
                        )
                        if (!is.null(failonerror)) {
                            curl::handle_setopt(
                                handle,
                                failonerror = isTRUE(failonerror)
                            )
                        }
                        curl::handle_setopt(
                            handle,
                            url = request_url(urls[[j]])
                        )
                        curl::multi_add(
                            handle,
                            done = function(response) {
                                out[[j]] <<- done_result(
                                    response = response,
                                    url = urls[[j]],
                                    started_at = started_at
                                )
                            },
                            fail = function(error) {
                                failed[[j]] <<- TRUE
                                failure_messages[[j]] <<- if (
                                    inherits(
                                        error,
                                        "condition"
                                    )
                                ) {
                                    conditionMessage(error)
                                } else {
                                    as.character(error)[[1L]]
                                }
                            },
                            pool = pool
                        )
                    })
                }
                curl::multi_run(
                    timeout = max(2 * timeout, 1),
                    poll = TRUE,
                    pool = pool
                )
            }
            TRUE
        },
        error = function(e) FALSE
    )

    # Pool errors retain completed callbacks and use the same bounded recovery
    # as a timeout; never restart every previously successful URL.

    # HTTP checks may need a serial HEAD-to-Range recovery. A failed DDS GET is
    # already conclusive and should not repeat the same timeout one URL at a
    # time after the concurrent request has finished.
    unreported <- vapply(out, is.null, logical(1L)) & !failed
    if (any(unreported)) {
        retry_unreported <- isTRUE(retry_failed) ||
            sum(unreported) <= min(concurrency, 8L)
        if (isTRUE(retry_unreported)) {
            # HTTP reachability checks and a small indeterminate tail retain
            # serial recovery without reopening hundreds of timed-out handles.
            out[unreported] <- lapply(urls[unreported], serial_check)
        } else {
            # A large OPeNDAP pool may reach its wall-clock limit before every
            # queued handle invokes a callback. Classify those handles without
            # turning one bounded concurrent check into hours of serial retries.
            out[unreported] <- lapply(which(unreported), function(index) {
                list(
                    reachable = FALSE,
                    latency_ms = NA_real_,
                    error = paste(
                        "Concurrent URL check ended without a response",
                        "before the pool timeout."
                    )
                )
            })
        }
    }
    if (any(failed)) {
        if (isTRUE(retry_failed)) {
            out[failed] <- lapply(urls[failed], serial_check)
        } else {
            out[failed] <- lapply(which(failed), function(index) {
                message <- failure_messages[[index]]
                if (is.na(message) || !nzchar(message)) {
                    message <- "Concurrent URL check failed without a response."
                }
                list(
                    reachable = FALSE,
                    latency_ms = NA_real_,
                    error = message
                )
            })
        }
    }

    out
}
# }}}

# query_result__reach_node_urls {{{
query_result__reach_node_urls <- function(
    urls,
    timeout = 5,
    network_policy = NULL,
    probe_concurrency = 1L
) {
    urls <- unique(urls[!is.na(urls) & nzchar(urls)])
    urls <- urls[query_result__url_http(urls)]
    query_result__run_url_checks(
        urls = urls,
        timeout = timeout,
        network_policy = network_policy,
        concurrency = probe_concurrency,
        # Keep data-node response handling independent of HTTP status.
        serial_check = function(url) {
            query_result__reach_node_url(
                url,
                timeout = timeout,
                network_policy = network_policy
            )
        },
        # Retain millisecond timing and the actual node URL in successful checks.
        done_result = function(response, url, started_at) {
            list(
                reachable = TRUE,
                latency_ms = (proc.time()[["elapsed"]] - started_at) * 1000,
                error = NA_character_,
                probe_url = url
            )
        },
        failonerror = FALSE
    )
}
# }}}

# query_result__net_key {{{
query_result__net_key <- function(network_policy = NULL) {
    if (is.null(network_policy) || !length(network_policy)) {
        return(NULL)
    }
    network_policy[sort(names(network_policy))]
}
# }}}

# query_result__reach_cache_key {{{
query_result__reach_cache_key <- function(
    level,
    target,
    timeout = 5,
    network_policy = NULL
) {
    cache__key(
        "reach",
        list(
            level = level,
            target = target,
            timeout = timeout,
            network_policy = query_result__net_key(network_policy)
        )
    )
}
# }}}

# query_result__reach_cache_get {{{
query_result__reach_cache_get <- function(
    level,
    target,
    timeout = 5,
    network_policy = NULL,
    cache_seconds = 3600L,
    cache_failures_seconds = 0L
) {
    if (cache__mode() == "off") {
        return(NULL)
    }
    key <- query_result__reach_cache_key(level, target, timeout, network_policy)
    cached <- cache__get()$get(key)
    if (cache__missing(cached)) {
        if (cache__mode() == "offline") {
            cli::cli_abort(
                "Cache miss in offline mode for reachability probe target {.val {target}}."
            )
        }
        return(NULL)
    }
    if (is.null(cached$timestamp) || !inherits(cached$timestamp, "POSIXt")) {
        return(NULL)
    }

    age <- as.numeric(difftime(Sys.time(), cached$timestamp, units = "secs"))
    ttl <- if (isTRUE(cached$result$reachable)) {
        cache_seconds
    } else {
        cache_failures_seconds
    }
    if (is.na(age) || is.na(ttl) || ttl <= 0L || age > ttl) {
        return(NULL)
    }

    result <- cached$result
    result$probe_cached <- TRUE
    result
}
# }}}

# query_result__reach_cache_set {{{
query_result__reach_cache_set <- function(
    level,
    target,
    timeout = 5,
    network_policy = NULL,
    result,
    cache_seconds = 3600L,
    cache_failures_seconds = 0L
) {
    if (cache__mode() == "off" || is.na(target) || !nzchar(target)) {
        return(invisible(NULL))
    }
    ok <- isTRUE(result$reachable)
    if ((!ok && cache_failures_seconds <= 0L) || (ok && cache_seconds <= 0L)) {
        return(invisible(NULL))
    }

    key <- query_result__reach_cache_key(level, target, timeout, network_policy)
    value <- list(
        timestamp = Sys.time(),
        result = list(
            reachable = as.logical(result$reachable),
            latency_ms = as.numeric(result$latency_ms),
            error = as.character(result$error),
            probe_url = as.character(result$probe_url)
        )
    )
    cache__get()$set(key, value)
    invisible(NULL)
}
# }}}

# query_result__url_try {{{
query_result__url_try <- function(
    url,
    timeout = 5,
    network_policy = NULL,
    nobody = TRUE,
    range = FALSE
) {
    if (is.null(network_policy)) {
        network_policy <- list()
    }
    connect_timeout <- network_policy$connect_timeout
    if (is.null(connect_timeout)) {
        connect_timeout <- min(timeout, 3)
    }
    ssl_verifypeer <- network_policy$ssl_verifypeer
    if (is.null(ssl_verifypeer)) {
        ssl_verifypeer <- TRUE
    }

    start <- proc.time()[["elapsed"]]
    tryCatch(
        {
            handle <- downloader__curl_handle(
                timeout = timeout,
                connect_timeout = connect_timeout,
                ssl_verifypeer = ssl_verifypeer,
                proxy = network_policy$proxy,
                useragent = network_policy$useragent,
                nobody = nobody
            )
            curl::handle_setopt(handle, failonerror = TRUE)
            if (isTRUE(range)) {
                curl::handle_setheaders(handle, Range = "bytes=0-0")
            }
            curl::curl_fetch_memory(url, handle = handle)
            list(
                ok = TRUE,
                latency_ms = (proc.time()[["elapsed"]] - start) * 1000,
                error = NA_character_
            )
        },
        error = function(e) {
            list(ok = FALSE, latency_ms = NA_real_, error = conditionMessage(e))
        }
    )
}
# }}}

# query_result__reach_url {{{
query_result__reach_url <- function(url, timeout = 5, network_policy = NULL) {
    query_result__reach_check(timeout, network_policy)

    if (is.na(url) || !nzchar(url)) {
        return(query_result__reach_missing("Missing URL."))
    }
    if (query_result__url_local(url)) {
        return(query_result__reach_local(url))
    }
    if (!query_result__url_http(url)) {
        return(query_result__reach_missing("Unsupported URL scheme."))
    }

    head <- query_result__url_try(
        url,
        timeout = timeout,
        network_policy = network_policy,
        nobody = TRUE
    )
    if (isTRUE(head$ok)) {
        return(list(
            reachable = TRUE,
            latency_ms = head$latency_ms,
            error = NA_character_
        ))
    }

    body <- query_result__url_try(
        url,
        timeout = timeout,
        network_policy = network_policy,
        nobody = FALSE,
        range = TRUE
    )
    if (isTRUE(body$ok)) {
        return(list(
            reachable = TRUE,
            latency_ms = body$latency_ms,
            error = NA_character_
        ))
    }

    error <- body$error
    if (
        is.null(error) ||
            !length(error) ||
            is.na(error[[1L]]) ||
            !nzchar(error[[1L]])
    ) {
        error <- head$error
    }
    if (
        is.null(error) ||
            !length(error) ||
            is.na(error[[1L]]) ||
            !nzchar(error[[1L]])
    ) {
        error <- "URL check failed."
    }
    list(
        reachable = FALSE,
        latency_ms = NA_real_,
        error = error
    )
}
# }}}

# Convert an ESGF OPeNDAP data URL to its DAP2 Dataset Descriptor Structure
# endpoint without carrying a selection expression or fragment into the check.
# query_result__opendap_dds_url {{{
query_result__opendap_dds_url <- function(url) {
    url <- sub("[?#].*$", "", as.character(url))
    url <- sub("\\.html$", "", url, ignore.case = TRUE)
    ifelse(grepl("\\.dds$", url, ignore.case = TRUE), url, paste0(url, ".dds"))
}
# }}}

# Confirm that a successful HTTP response is a DAP Dataset Descriptor Structure
# rather than an HTML error page returned with status 200.
# query_result__valid_dds {{{
query_result__valid_dds <- function(content) {
    if (is.null(content) || !length(content)) {
        return(FALSE)
    }
    text <- tryCatch(rawToChar(content), error = function(error) "")
    isTRUE(grepl("^[[:space:]]*Dataset[[:space:]]*\\{", text)) &&
        isTRUE(grepl(
            "\\}[[:space:]]*[^;[:space:]]+[[:space:]]*;[[:space:]]*$",
            text
        ))
}
# }}}

# Check one exact OPeNDAP file endpoint by requesting its DDS metadata. A data
# node homepage or an arbitrary 200 response cannot satisfy this contract.
# query_result__check_opendap_url {{{
query_result__check_opendap_url <- function(
    url,
    timeout = 5,
    network_policy = NULL
) {
    query_result__reach_check(timeout, network_policy)
    if (is.na(url) || !nzchar(url)) {
        return(query_result__reach_missing("Missing URL."))
    }
    if (!query_result__url_http(url)) {
        return(query_result__reach_missing("Unsupported URL scheme."))
    }
    if (is.null(network_policy)) {
        network_policy <- list()
    }
    connect_timeout <- network_policy$connect_timeout
    if (is.null(connect_timeout)) {
        connect_timeout <- min(timeout, 3)
    }
    ssl_verifypeer <- network_policy$ssl_verifypeer
    if (is.null(ssl_verifypeer)) {
        ssl_verifypeer <- TRUE
    }
    started_at <- proc.time()[["elapsed"]]
    tryCatch(
        {
            handle <- downloader__curl_handle(
                timeout = timeout,
                connect_timeout = connect_timeout,
                ssl_verifypeer = ssl_verifypeer,
                proxy = network_policy$proxy,
                useragent = network_policy$useragent,
                nobody = FALSE
            )
            curl::handle_setopt(handle, failonerror = TRUE)
            response <- curl::curl_fetch_memory(
                query_result__opendap_dds_url(url),
                handle = handle
            )
            if (!query_result__valid_dds(response$content)) {
                return(list(
                    reachable = FALSE,
                    latency_ms = NA_real_,
                    error = "OPeNDAP endpoint did not return a valid DDS response."
                ))
            }
            list(
                reachable = TRUE,
                latency_ms = (proc.time()[["elapsed"]] - started_at) * 1000,
                error = NA_character_
            )
        },
        error = function(error) {
            list(
                reachable = FALSE,
                latency_ms = NA_real_,
                error = conditionMessage(error)
            )
        }
    )
}
# }}}

# Check unique OPeNDAP URLs concurrently while retaining the original base URL
# as the result key used by catalog records and cache entries.
# query_result__check_opendap_urls {{{
query_result__check_opendap_urls <- function(
    urls,
    timeout = 5,
    network_policy = NULL,
    concurrency = 1L
) {
    urls <- unique(urls[!is.na(urls) & nzchar(urls)])
    urls <- urls[query_result__url_http(urls)]
    query_result__run_url_checks(
        urls = urls,
        timeout = timeout,
        network_policy = network_policy,
        concurrency = concurrency,
        serial_check = function(url) {
            query_result__check_opendap_url(
                url,
                timeout = timeout,
                network_policy = network_policy
            )
        },
        done_result = function(response, url, started_at) {
            if (!query_result__valid_dds(response$content)) {
                return(list(
                    reachable = FALSE,
                    latency_ms = NA_real_,
                    error = "OPeNDAP endpoint did not return a valid DDS response."
                ))
            }
            list(
                reachable = TRUE,
                latency_ms = (proc.time()[["elapsed"]] - started_at) * 1000,
                error = NA_character_
            )
        },
        failonerror = TRUE,
        nobody = FALSE,
        request_url = query_result__opendap_dds_url,
        retry_failed = FALSE
    )
}
# }}}

# query_result__reach_http_urls {{{
query_result__reach_http_urls <- function(
    urls,
    timeout = 5,
    network_policy = NULL,
    probe_concurrency = 1L
) {
    urls <- unique(urls[!is.na(urls) & nzchar(urls)])
    urls <- urls[query_result__url_http(urls)]
    query_result__run_url_checks(
        urls = urls,
        timeout = timeout,
        network_policy = network_policy,
        concurrency = probe_concurrency,
        # Retain the HEAD-then-Range behavior for failed service URL checks.
        serial_check = function(url) {
            query_result__reach_url(
                url,
                timeout = timeout,
                network_policy = network_policy
            )
        },
        # Successful HTTP service checks retain the reachability result schema.
        done_result = function(response, url, started_at) {
            list(
                reachable = TRUE,
                latency_ms = (proc.time()[["elapsed"]] - started_at) * 1000,
                error = NA_character_
            )
        },
        failonerror = TRUE
    )
}
# }}}

# query_result__reach_urls {{{
query_result__reach_urls <- function(
    urls,
    timeout = 5,
    network_policy = NULL,
    probe_concurrency = 1L
) {
    checkmate::assert_character(urls, any.missing = TRUE)
    query_result__reach_check(timeout, network_policy, probe_concurrency)

    out <- data.table::data.table(
        url = urls,
        reachable = rep(NA, length(urls)),
        latency_ms = rep(NA_real_, length(urls)),
        error = rep(NA_character_, length(urls))
    )
    if (!length(urls)) {
        return(out)
    }

    unique_urls <- unique(urls)
    use_http <- !is.na(unique_urls) &
        nzchar(unique_urls) &
        query_result__url_http(unique_urls)
    probes <- query_result__reach_http_urls(
        unique_urls[use_http],
        timeout = timeout,
        network_policy = network_policy,
        probe_concurrency = probe_concurrency
    )

    for (url in unique_urls[!use_http]) {
        probe <- query_result__reach_url(
            url,
            timeout = timeout,
            network_policy = network_policy
        )
        if (is.na(url)) {
            idx <- is.na(out$url)
        } else {
            idx <- !is.na(out$url) & out$url == url
        }
        out[
            idx,
            `:=`(
                reachable = as.logical(probe$reachable),
                latency_ms = as.numeric(probe$latency_ms),
                error = as.character(probe$error)
            )
        ]
    }
    if (length(probes)) {
        for (url in names(probes)) {
            probe <- probes[[url]]
            target_url <- url
            out[
                !is.na(out[["url"]]) & out[["url"]] == target_url,
                `:=`(
                    reachable = as.logical(probe$reachable),
                    latency_ms = as.numeric(probe$latency_ms),
                    error = as.character(probe$error)
                )
            ]
        }
    }

    out[]
}
# }}}

# Dispatch exact URL checks by ESGF service and cache them independently so a
# generic HTTP response can never be reused as evidence of DAP availability.
# query_result__reach_service_urls {{{
query_result__reach_service_urls <- function(
    urls,
    service,
    timeout = 5,
    network_policy = NULL,
    concurrency = 1L,
    cache_seconds = 3600L,
    cache_failures_seconds = 0L
) {
    checkmate::assert_character(urls, any.missing = TRUE)
    checkmate::assert_string(service)
    query_result__reach_check(timeout, network_policy, concurrency)
    out <- data.table::data.table(
        url = urls,
        reachable = rep(NA, length(urls)),
        latency_ms = rep(NA_real_, length(urls)),
        error = rep(NA_character_, length(urls)),
        probe_cached = rep(FALSE, length(urls))
    )
    if (!length(urls)) {
        return(out)
    }

    unique_urls <- unique(urls)
    pending <- character()
    cache_level <- paste0("url:", toupper(service))
    for (url in unique_urls) {
        target_url <- url
        if (is.na(url) || !nzchar(url) || !query_result__url_http(url)) {
            result <- query_result__reach_url(
                url,
                timeout = timeout,
                network_policy = network_policy
            )
            idx <- if (is.na(url)) is.na(out$url) else out$url == target_url
            out[
                idx,
                `:=`(
                    reachable = as.logical(result$reachable),
                    latency_ms = as.numeric(result$latency_ms),
                    error = as.character(result$error)
                )
            ]
            next
        }
        cached <- query_result__reach_cache_get(
            cache_level,
            url,
            timeout = timeout,
            network_policy = network_policy,
            cache_seconds = cache_seconds,
            cache_failures_seconds = cache_failures_seconds
        )
        if (is.null(cached)) {
            pending <- c(pending, url)
            next
        }
        out[
            !is.na(url) & url == target_url,
            `:=`(
                reachable = as.logical(cached$reachable),
                latency_ms = as.numeric(cached$latency_ms),
                error = as.character(cached$error),
                probe_cached = TRUE
            )
        ]
    }

    if (length(pending)) {
        checked <- if (identical(toupper(service), "OPENDAP")) {
            query_result__check_opendap_urls(
                pending,
                timeout = timeout,
                network_policy = network_policy,
                concurrency = concurrency
            )
        } else {
            query_result__reach_http_urls(
                pending,
                timeout = timeout,
                network_policy = network_policy,
                probe_concurrency = concurrency
            )
        }
        for (url in names(checked)) {
            target_url <- url
            result <- checked[[url]]
            result$probe_url <- url
            query_result__reach_cache_set(
                cache_level,
                url,
                timeout = timeout,
                network_policy = network_policy,
                result = result,
                cache_seconds = cache_seconds,
                cache_failures_seconds = cache_failures_seconds
            )
            out[
                !is.na(url) & url == target_url,
                `:=`(
                    reachable = as.logical(result$reachable),
                    latency_ms = as.numeric(result$latency_ms),
                    error = as.character(result$error),
                    probe_cached = FALSE
                )
            ]
        }
    }
    out[]
}
# }}}

# query_result__reach_nodes {{{
query_result__reach_nodes <- function(
    data_node,
    timeout = 5,
    network_policy = NULL,
    probe_concurrency = 1L,
    cache_seconds = 3600L,
    cache_failures_seconds = 0L
) {
    checkmate::assert_character(data_node, any.missing = TRUE)
    query_result__reach_check(timeout, network_policy, probe_concurrency)
    checkmate::assert_count(cache_seconds, positive = FALSE)
    checkmate::assert_count(cache_failures_seconds, positive = FALSE)

    out <- data.table::data.table(
        data_node = data_node,
        reachable = rep(NA, length(data_node)),
        latency_ms = rep(NA_real_, length(data_node)),
        error = rep(NA_character_, length(data_node)),
        probe_url = rep(NA_character_, length(data_node)),
        probe_cached = rep(FALSE, length(data_node))
    )
    if (!length(data_node)) {
        return(out)
    }

    unique_nodes <- unique(data_node)
    probes <- vector("list", length(unique_nodes))
    network_pos <- integer()
    for (k in seq_along(unique_nodes)) {
        node <- unique_nodes[[k]]
        if (is.na(node) || !nzchar(node)) {
            probe <- query_result__reach_missing("Missing data node.")
            probe$probe_url <- NA_character_
            probe$probe_cached <- FALSE
            probes[[k]] <- probe
            next
        }
        cached <- query_result__reach_cache_get(
            "data_node",
            node,
            timeout = timeout,
            network_policy = network_policy,
            cache_seconds = cache_seconds,
            cache_failures_seconds = cache_failures_seconds
        )
        if (!is.null(cached)) {
            probes[[k]] <- cached
            next
        }

        network_pos <- c(network_pos, k)
    }

    if (length(network_pos)) {
        node_values <- unique_nodes[network_pos]
        node_urls <- lapply(node_values, query_result__node_urls)
        first_urls <- vapply(
            node_urls,
            function(urls) {
                if (length(urls)) urls[[1L]] else NA_character_
            },
            character(1L)
        )
        first_probes <- query_result__reach_node_urls(
            first_urls,
            timeout = timeout,
            network_policy = network_policy,
            probe_concurrency = probe_concurrency
        )

        second_pos <- integer()
        first_errors <- rep(NA_character_, length(network_pos))
        for (j in seq_along(network_pos)) {
            k <- network_pos[[j]]
            urls <- node_urls[[j]]
            if (!length(urls)) {
                probe <- query_result__reach_missing("Missing data node.")
                probe$probe_url <- NA_character_
                probe$probe_cached <- FALSE
                probes[[k]] <- probe
                next
            }

            probe <- first_probes[[first_urls[[j]]]]
            if (is.null(probe)) {
                probe <- query_result__reach_missing(
                    "Unsupported data node URL scheme."
                )
                probe$probe_url <- first_urls[[j]]
            }
            if (isTRUE(probe$reachable)) {
                probe$probe_cached <- FALSE
                probes[[k]] <- probe
                next
            }

            error <- as.character(probe$error)
            if (!length(error) || is.na(error[[1L]]) || !nzchar(error[[1L]])) {
                error <- "Data node probe failed."
            }
            first_errors[[j]] <- sprintf("%s: %s", first_urls[[j]], error[[1L]])
            if (length(urls) > 1L) {
                second_pos <- c(second_pos, j)
            } else {
                probe$probe_cached <- FALSE
                probes[[k]] <- probe
            }
        }

        if (length(second_pos)) {
            second_urls <- vapply(
                node_urls[second_pos],
                `[[`,
                character(1L),
                2L
            )
            second_probes <- query_result__reach_node_urls(
                second_urls,
                timeout = timeout,
                network_policy = network_policy,
                probe_concurrency = probe_concurrency
            )
            for (j in second_pos) {
                k <- network_pos[[j]]
                probe <- second_probes[[node_urls[[j]][[2L]]]]
                if (is.null(probe)) {
                    probe <- query_result__reach_missing(
                        "Unsupported data node URL scheme."
                    )
                    probe$probe_url <- node_urls[[j]][[2L]]
                }
                if (!isTRUE(probe$reachable)) {
                    error <- as.character(probe$error)
                    if (
                        !length(error) ||
                            is.na(error[[1L]]) ||
                            !nzchar(error[[1L]])
                    ) {
                        error <- "Data node probe failed."
                    }
                    probe$error <- paste(
                        c(
                            first_errors[[j]],
                            sprintf("%s: %s", node_urls[[j]][[2L]], error[[1L]])
                        ),
                        collapse = " | "
                    )
                    probe$probe_url <- node_urls[[j]][[1L]]
                }
                probe$probe_cached <- FALSE
                probes[[k]] <- probe
            }
        }

        for (k in network_pos) {
            probe <- probes[[k]]
            query_result__reach_cache_set(
                "data_node",
                unique_nodes[[k]],
                timeout = timeout,
                network_policy = network_policy,
                result = probe,
                cache_seconds = cache_seconds,
                cache_failures_seconds = cache_failures_seconds
            )
        }
    }

    for (k in seq_along(unique_nodes)) {
        node <- unique_nodes[[k]]
        probe <- probes[[k]]
        if (is.na(node)) {
            idx <- is.na(out$data_node)
        } else {
            idx <- !is.na(out$data_node) & out$data_node == node
        }
        out[
            idx,
            `:=`(
                reachable = as.logical(probe$reachable),
                latency_ms = as.numeric(probe$latency_ms),
                error = as.character(probe$error),
                probe_url = as.character(probe$probe_url),
                probe_cached = isTRUE(probe$probe_cached)
            )
        ]
    }

    out[]
}
# }}}

# query_result__reach_url_table {{{
query_result__reach_url_table <- function(probes, urls) {
    if (!"probe_level" %in% names(probes)) {
        probes[,
            probe_level := data.table::fifelse(
                !is.na(url) & nzchar(url) & query_result__url_local(url),
                "local",
                "url"
            )
        ]
    }
    if (!"probe_url" %in% names(probes)) {
        probes[, probe_url := url]
    }
    if (!"probe_cached" %in% names(probes)) {
        probes[, probe_cached := FALSE]
    }
    probes[]
}
# }}}

# query_result__reach_targets {{{
query_result__reach_targets <- function(
    urls,
    data_node = NULL,
    service = "OPENDAP",
    level = c("data_node", "url"),
    timeout = 5,
    network_policy = NULL,
    probe_concurrency = 1L,
    cache_seconds = 3600L,
    cache_failures_seconds = 0L
) {
    level <- match.arg(level)
    checkmate::assert_character(urls, any.missing = TRUE)
    query_result__reach_check(timeout, network_policy, probe_concurrency)
    n <- length(urls)
    if (is.null(data_node)) {
        data_node <- rep(NA_character_, n)
    }
    checkmate::assert_character(data_node, any.missing = TRUE, len = n)

    if (identical(level, "url")) {
        checks <- query_result__reach_service_urls(
            urls,
            service = service,
            timeout = timeout,
            network_policy = network_policy,
            concurrency = probe_concurrency,
            cache_seconds = cache_seconds,
            cache_failures_seconds = cache_failures_seconds
        )
        return(query_result__reach_url_table(checks, urls))
    }

    out <- data.table::data.table(
        url = urls,
        reachable = rep(NA, n),
        latency_ms = rep(NA_real_, n),
        error = rep(NA_character_, n),
        probe_level = rep("data_node", n),
        probe_url = rep(NA_character_, n),
        probe_cached = rep(FALSE, n)
    )
    if (!n) {
        return(out)
    }

    missing <- is.na(urls) | !nzchar(urls)
    if (any(missing)) {
        out[
            missing,
            `:=`(
                reachable = NA,
                latency_ms = NA_real_,
                error = "Missing URL."
            )
        ]
    }

    local <- !missing & query_result__url_local(urls)
    if (any(local)) {
        for (i in which(local)) {
            probe <- query_result__reach_local(urls[[i]])
            out[
                i,
                `:=`(
                    reachable = as.logical(probe$reachable),
                    latency_ms = as.numeric(probe$latency_ms),
                    error = as.character(probe$error),
                    probe_level = "local",
                    probe_url = urls[[i]],
                    probe_cached = FALSE
                )
            ]
        }
    }

    remote <- !missing & !local & query_result__url_http(urls)
    unsupported <- !missing & !local & !remote
    if (any(unsupported)) {
        out[
            unsupported,
            `:=`(
                reachable = NA,
                latency_ms = NA_real_,
                error = "Unsupported URL scheme.",
                probe_url = urls[unsupported]
            )
        ]
    }

    if (any(remote)) {
        nodes <- data_node
        fallback <- is.na(nodes) | !nzchar(nodes)
        nodes[fallback] <- query_result__url_host(urls[fallback])
        node_probes <- query_result__reach_nodes(
            nodes[remote],
            timeout = timeout,
            network_policy = network_policy,
            probe_concurrency = probe_concurrency,
            cache_seconds = cache_seconds,
            cache_failures_seconds = cache_failures_seconds
        )
        idx <- which(remote)
        out[
            idx,
            `:=`(
                reachable = node_probes$reachable,
                latency_ms = node_probes$latency_ms,
                error = node_probes$error,
                probe_url = node_probes$probe_url,
                probe_cached = node_probes$probe_cached
            )
        ]
    }

    out[]
}
# }}}

# query_result__latency_url {{{
query_result__latency_url <- function(url, timeout = 5, network_policy = NULL) {
    if (is.na(url) || !nzchar(url) || startsWith(url, "file://")) {
        return(list(latency = NA_real_, throughput = NA_real_))
    }
    if (is.null(network_policy)) {
        network_policy <- list()
    }
    connect_timeout <- network_policy$connect_timeout
    if (is.null(connect_timeout)) {
        connect_timeout <- min(timeout, 3)
    }
    ssl_verifypeer <- network_policy$ssl_verifypeer
    if (is.null(ssl_verifypeer)) {
        ssl_verifypeer <- TRUE
    }
    start <- Sys.time()
    ok <- tryCatch(
        {
            handle <- downloader__curl_handle(
                timeout = timeout,
                connect_timeout = connect_timeout,
                ssl_verifypeer = ssl_verifypeer,
                proxy = network_policy$proxy,
                useragent = network_policy$useragent,
                nobody = TRUE
            )
            curl::curl_fetch_memory(url, handle = handle)
            TRUE
        },
        error = function(e) FALSE
    )
    if (!ok) {
        start <- Sys.time()
        ok <- tryCatch(
            {
                handle <- downloader__curl_handle(
                    timeout = timeout,
                    connect_timeout = connect_timeout,
                    ssl_verifypeer = ssl_verifypeer,
                    proxy = network_policy$proxy,
                    useragent = network_policy$useragent
                )
                curl::handle_setheaders(handle, Range = "bytes=0-0")
                curl::curl_fetch_memory(url, handle = handle)
                TRUE
            },
            error = function(e) FALSE
        )
    }
    if (!ok) {
        return(list(latency = NA_real_, throughput = NA_real_))
    }
    list(
        latency = as.numeric(difftime(Sys.time(), start, units = "secs")),
        throughput = NA_real_
    )
}
# }}}

# query_result__latency_urls {{{
query_result__latency_urls <- function(
    urls,
    timeout = 5,
    network_policy = NULL,
    probe_concurrency = 1L
) {
    urls <- unique(urls[!is.na(urls) & nzchar(urls)])
    urls <- urls[!startsWith(urls, "file://")]
    query_result__run_url_checks(
        urls = urls,
        timeout = timeout,
        network_policy = network_policy,
        concurrency = probe_concurrency,
        # Retain the existing latency check and its Range fallback.
        serial_check = function(url) {
            query_result__latency_url(
                url,
                timeout = timeout,
                network_policy = network_policy
            )
        },
        # Latency remains measured in seconds with an unavailable throughput.
        done_result = function(response, url, started_at) {
            list(
                latency = as.numeric(difftime(
                    Sys.time(),
                    started_at,
                    units = "secs"
                )),
                throughput = NA_real_
            )
        },
        clock = Sys.time
    )
}
# }}}

# query_result__latency_table {{{
query_result__latency_table <- function(
    urls,
    data_node = NULL,
    service = "HTTPServer",
    timeout = 5,
    network_policy = NULL,
    node_stats = NULL,
    node_policy = NULL,
    probe_concurrency = 1L,
    probe_cache_seconds = 3600L
) {
    checkmate::assert_character(urls, any.missing = TRUE)
    checkmate::assert_count(probe_concurrency, positive = TRUE)
    if (!is.null(probe_cache_seconds)) {
        checkmate::assert_count(probe_cache_seconds, positive = FALSE)
    }
    n <- length(urls)
    out <- data.table::data.table(
        url = urls,
        probe_latency = rep(NA_real_, n),
        probe_throughput = rep(NA_real_, n),
        probe_cached = rep(FALSE, n)
    )
    if (!n) {
        return(out)
    }

    if (
        !is.null(data_node) &&
            !is.null(node_stats) &&
            !is.null(probe_cache_seconds) &&
            probe_cache_seconds > 0L
    ) {
        stats <- query_result__node_stats(
            node_stats,
            service = service,
            node_policy = node_policy
        )
        if (!is.null(stats) && nrow(stats)) {
            data_node <- as.character(data_node)
            stats <- stats[!duplicated(data_node)]
            idx <- match(data_node, stats$data_node)
            has <- !is.na(idx)
            cache_time <- as.POSIXct(
                rep(NA, n),
                origin = "1970-01-01",
                tz = "UTC"
            )
            if (any(has)) {
                last_probe <- stats$node_last_probe_at[idx[has]]
                updated <- stats$node_updated_at[idx[has]]
                cache_time[has] <- last_probe
                missing_time <- is.na(cache_time[has])
                cache_time[which(has)[missing_time]] <- updated[missing_time]
            }
            fresh <- !is.na(cache_time) &
                cache_time >= Sys.time() - probe_cache_seconds
            success <- has &
                (suppressWarnings(as.integer(stats$node_probe_success_count[
                    idx
                ])) >
                    0L |
                    suppressWarnings(as.integer(stats$node_success_count[
                        idx
                    ])) >
                        0L)
            latency <- suppressWarnings(as.numeric(stats$node_avg_latency[idx]))
            use_cache <- fresh & success & !is.na(latency)
            out[
                use_cache,
                `:=`(
                    probe_latency = latency[use_cache],
                    probe_throughput = NA_real_,
                    probe_cached = TRUE
                )
            ]
        }
    }

    probe_urls <- unique(out[!probe_cached & !is.na(url) & nzchar(url), url])
    probes <- query_result__latency_urls(
        probe_urls,
        timeout = timeout,
        network_policy = network_policy,
        probe_concurrency = as.integer(probe_concurrency)
    )
    if (length(probes)) {
        for (url in names(probes)) {
            probe <- probes[[url]]
            target_url <- url
            out[
                out[["url"]] == target_url & !out[["probe_cached"]],
                `:=`(
                    probe_latency = as.numeric(probe$latency),
                    probe_throughput = as.numeric(probe$throughput)
                )
            ]
        }
    }
    out[]
}
# }}}

# vim: fdm=marker :
