#' @include query-result.R
NULL

# Match source identities and repair service addresses using compatible replicas.

# query_result__identity {{{
query_result__identity <- function(docs) {
    instance_id <- as.character(query_result__col(docs, "instance_id"))
    master_id <- as.character(query_result__col(docs, "master_id"))
    version <- as.character(query_result__col(docs, "version"))
    has_instance <- !is.na(instance_id) & nzchar(instance_id)
    has_master_version <- !has_instance &
        !is.na(master_id) &
        nzchar(master_id) &
        !is.na(version) &
        nzchar(version)

    # Distributed File searches sometimes omit provider identity fields while
    # retaining a standard DRS filename that is stable across replica rows.
    logical_id <- tryCatch(
        query_result__file_key(data.table::as.data.table(docs)),
        error = function(error) rep(NA_character_, nrow(docs))
    )
    has_logical <- !has_instance &
        !has_master_version &
        !is.na(logical_id) &
        startsWith(logical_id, "drs:")

    key <- rep(NA_character_, nrow(docs))
    key[has_instance] <- paste(
        "instance_id",
        instance_id[has_instance],
        sep = "\r"
    )
    key[has_master_version] <- paste(
        "master_version",
        master_id[has_master_version],
        version[has_master_version],
        sep = "\r"
    )
    key[has_logical] <- paste(
        "logical_file",
        logical_id[has_logical],
        sep = "\r"
    )

    data.frame(
        key = key,
        instance_id = instance_id,
        master_id = master_id,
        version = version,
        logical_id = logical_id,
        has_instance = has_instance,
        has_master_version = has_master_version,
        has_logical = has_logical,
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
}
# }}}

# query_result__identity_match {{{
query_result__identity_match <- function(target, candidates) {
    if (isTRUE(target$has_instance)) {
        return(which(
            candidates$has_instance &
                candidates$instance_id == target$instance_id
        ))
    }
    if (isTRUE(target$has_master_version)) {
        return(which(
            !is.na(candidates$master_id) &
                !is.na(candidates$version) &
                candidates$master_id == target$master_id &
                candidates$version == target$version
        ))
    }
    if (isTRUE(target$has_logical)) {
        return(which(
            candidates$has_logical &
                candidates$logical_id == target$logical_id
        ))
    }

    integer()
}
# }}}

# Reject replica rows whose available version, checksum, or size metadata
# proves they are not the same file content as the selected catalog record.
# query_result__compatible_content {{{
query_result__compatible_content <- function(target, candidates) {
    if (!nrow(candidates)) {
        return(logical())
    }
    compatible <- rep(TRUE, nrow(candidates))
    target_version <- as.character(query_result__col(target, "version"))[[1L]]
    candidate_version <- as.character(query_result__col(candidates, "version"))
    compare_version <- !is.na(target_version) &
        nzchar(target_version) &
        !is.na(candidate_version) &
        nzchar(candidate_version)
    compatible[compare_version] <-
        candidate_version[compare_version] == target_version

    target_checksum <- tolower(as.character(
        query_result__col(target, "checksum")
    )[[1L]])
    candidate_checksum <- tolower(as.character(
        query_result__col(candidates, "checksum")
    ))
    compare_checksum <- !is.na(target_checksum) &
        nzchar(target_checksum) &
        !is.na(candidate_checksum) &
        nzchar(candidate_checksum)
    compatible[compare_checksum] <- compatible[compare_checksum] &
        candidate_checksum[compare_checksum] == target_checksum

    target_size <- suppressWarnings(as.numeric(
        query_result__col(target, "size")
    )[[1L]])
    candidate_size <- suppressWarnings(as.numeric(
        query_result__col(candidates, "size")
    ))
    compare_size <- is.finite(target_size) & is.finite(candidate_size)
    compatible[compare_size] <- compatible[compare_size] &
        candidate_size[compare_size] == target_size
    compatible
}
# }}}

# Assign File rows to logical-content groups without merging replicas whose
# known version, checksum, or size disagree. Pairwise compatibility prevents a
# metadata-sparse row from bridging two replicas that prove different content.
# query_result__compatible_file_groups {{{
query_result__compatible_file_groups <- function(docs) {
    if (!nrow(docs)) {
        return(integer())
    }
    logical_id <- tryCatch(
        query_result__file_key(data.table::as.data.table(docs)),
        error = function(error) paste0("row:", seq_len(nrow(docs)))
    )
    groups <- integer(nrow(docs))
    group_count <- 0L
    for (i in seq_len(nrow(docs))) {
        assigned <- FALSE
        for (group in seq_len(group_count)) {
            members <- which(groups == group)
            if (
                !length(members) ||
                    !all(logical_id[members] == logical_id[[i]])
            ) {
                next
            }
            forward <- query_result__compatible_content(
                docs[i, , drop = FALSE],
                docs[members, , drop = FALSE]
            )
            reverse <- vapply(
                members,
                function(member) {
                    query_result__compatible_content(
                        docs[member, , drop = FALSE],
                        docs[i, , drop = FALSE]
                    )[[1L]]
                },
                logical(1L)
            )
            if (all(forward) && all(reverse)) {
                groups[[i]] <- group
                assigned <- TRUE
                break
            }
        }
        if (!assigned) {
            group_count <- group_count + 1L
            groups[[i]] <- group_count
        }
    }
    groups
}
# }}}

# query_result__identity_in {{{
query_result__identity_in <- function(candidates, targets) {
    keep <- rep(FALSE, nrow(candidates))
    for (i in seq_len(nrow(targets))) {
        keep[query_result__identity_match(
            targets[i, , drop = FALSE],
            candidates
        )] <- TRUE
    }

    keep
}
# }}}

# query_result__replica_store {{{
query_result__replica_store <- function(type, params) {
    store <- QueryParamStore$new()
    store$project(NULL)
    suppressWarnings(do.call(store$params, params))
    store$replica(NULL)
    store$latest(NULL)
    store$distrib(TRUE)
    store$type(type)
    store$format(QUERY_PARAM__FORMAT_JSON)
    store$fields("*")
    store$limit(this$data_max_limit)
    store$offset(0L)
    store
}
# }}}

# query_result__collect_identity {{{
query_result__collect_identity <- function(
    result,
    identity,
    type,
    index_node = NULL,
    all = TRUE
) {
    type <- query_result__type(type)
    if (is.null(index_node)) {
        index_node <- priv(result)$index_node
    } else {
        checkmate::assert_string(index_node)
        index_node <- query__normalize_node(index_node)
    }

    instance_id <- unique(identity$instance_id[identity$has_instance])
    instance_id <- instance_id[!is.na(instance_id) & nzchar(instance_id)]
    master_id <- unique(identity$master_id[identity$has_master_version])
    master_id <- master_id[!is.na(master_id) & nzchar(master_id)]

    stores <- list()
    if (length(instance_id)) {
        instance_batches <- query_result__query_value_batches(instance_id)
        stores <- c(
            stores,
            lapply(instance_batches, function(batch) {
                query_result__replica_store(type, list(instance_id = batch))
            })
        )
    }
    if (length(master_id)) {
        master_batches <- query_result__query_value_batches(master_id)
        stores <- c(
            stores,
            lapply(master_batches, function(batch) {
                query_result__replica_store(type, list(master_id = batch))
            })
        )
    }
    if (!length(stores)) {
        stores[[1L]] <- query_result__replica_store(type, list())
    }

    collected_parts <- lapply(stores, function(store) {
        query__collect(
            index_node,
            store,
            required_fields = query_result__required(type),
            all = all,
            limit = this$data_max_limit,
            constraints = FALSE
        )
    })
    collected <- query_result__merge_collects(collected_parts, stores[[1L]])
    # The result retains the original identity facet, not only its first shard.
    query_result__merge_params(
        collected$parameter,
        if (length(instance_id)) {
            list(instance_id = instance_id)
        } else {
            list(master_id = master_id)
        }
    )
    response <- collected$response
    response$response$docs <- collected$docs

    query_result__new(
        query_result__generator(type),
        index_node,
        collected$parameter,
        response,
        context = collected$context
    )
}
# }}}

# query_result__collect_master {{{
query_result__collect_master <- function(
    result,
    master_id,
    type,
    index_node = NULL,
    all = TRUE
) {
    type <- query_result__type(type)
    checkmate::assert_character(
        master_id,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE
    )
    if (is.null(index_node)) {
        index_node <- priv(result)$index_node
    } else {
        checkmate::assert_string(index_node)
        index_node <- query__normalize_node(index_node)
    }

    stores <- lapply(
        query_result__query_value_batches(master_id),
        function(batch) {
            query_result__replica_store(type, list(master_id = batch))
        }
    )
    collected_parts <- lapply(stores, function(store) {
        query__collect(
            index_node,
            store,
            required_fields = query_result__required(type),
            all = all,
            limit = this$data_max_limit,
            constraints = FALSE
        )
    })
    collected <- query_result__merge_collects(collected_parts, stores[[1L]])
    query_result__merge_params(collected$parameter, list(master_id = master_id))
    response <- collected$response
    response$response$docs <- collected$docs

    query_result__new(
        query_result__generator(type),
        index_node,
        collected$parameter,
        response,
        context = collected$context
    )
}
# }}}

# query_result__expand_files {{{
query_result__expand_files <- function(
    result,
    service = "HTTPServer",
    all = TRUE
) {
    dt <- result$to_data_table()
    identity <- query_result__identity(dt)
    keys <- unique(identity$key[!is.na(identity$key) & nzchar(identity$key)])
    if (!length(keys)) {
        return(result)
    }

    expanded <- query_result__collect_identity(
        result,
        identity,
        type = "File",
        all = all
    )
    expanded_docs <- priv(expanded)$get_docs()
    expanded_identity <- query_result__identity(expanded_docs)
    keep <- query_result__identity_in(expanded_identity, identity)
    priv(expanded)$result_with_docs(expanded_docs[keep, , drop = FALSE])
}
# }}}

# query_result__expand_datasets {{{
query_result__expand_datasets <- function(
    result,
    by = c("instance_id", "master_id"),
    all = TRUE,
    index_node = NULL
) {
    by <- match.arg(by)
    docs <- priv(result)$get_docs()
    if (!nrow(docs)) {
        return(result)
    }

    if (identical(by, "master_id")) {
        master_id <- as.character(query_result__col(docs, "master_id"))
        master_id <- unique(master_id[!is.na(master_id) & nzchar(master_id)])
        if (!length(master_id)) {
            return(result)
        }
        return(query_result__collect_master(
            result,
            master_id,
            type = "Dataset",
            index_node = index_node,
            all = all
        ))
    }

    identity <- query_result__identity(docs)
    keys <- unique(identity$key[!is.na(identity$key) & nzchar(identity$key)])
    if (!length(keys)) {
        return(result)
    }
    expanded <- query_result__collect_identity(
        result,
        identity,
        type = "Dataset",
        index_node = index_node,
        all = all
    )
    expanded_docs <- priv(expanded)$get_docs()
    expanded_identity <- query_result__identity(expanded_docs)
    keep <- query_result__identity_in(expanded_identity, identity)
    priv(expanded)$result_with_docs(expanded_docs[keep, , drop = FALSE])
}
# }}}

# query_result__repair_config {{{
query_result__repair_config <- function(probe = NULL) {
    query_result__reach_config(
        probe,
        include_level = TRUE,
        default_level = "data_node"
    )
}
# }}}

# query_result__merge_urls {{{
query_result__merge_urls <- function(result, extra_context = NULL) {
    context <- query_result__context(priv(result)$context)
    urls <- unname(priv(result)$get_query_url_context())
    extra <- query_result__context(extra_context)
    if (!is.null(extra$query_url)) {
        urls <- c(urls, unname(extra$query_url))
    }
    context$query_url <- query_result__query_urls(urls, named = FALSE)
    context
}
# }}}

# query_result__align_docs {{{
query_result__align_docs <- function(docs, fields, template = NULL) {
    docs <- as.data.frame(docs, stringsAsFactors = FALSE)
    n <- nrow(docs)
    for (field in setdiff(fields, names(docs))) {
        is_list <- !is.null(template) &&
            field %in% names(template) &&
            is.list(template[[field]])
        docs[[field]] <- if (is_list) I(rep(list(NA), n)) else rep(NA, n)
    }

    docs[, fields, drop = FALSE]
}
# }}}

# query_result__merge_collects {{{
query_result__merge_collects <- function(results, params) {
    if (length(results) == 1L) {
        return(results[[1L]])
    }

    docs_list <- lapply(results, .subset2, "docs")
    fields <- unique(unlist(lapply(docs_list, names), use.names = FALSE))
    docs_list <- lapply(docs_list, query_result__align_docs, fields = fields)
    docs <- data.table::rbindlist(
        lapply(docs_list, data.table::as.data.table),
        fill = TRUE
    )
    docs <- as.data.frame(docs, stringsAsFactors = FALSE)

    response <- results[[length(results)]]$response
    response$response$docs <- docs
    response$response$numFound <- nrow(docs)
    response$response$start <- 0L

    contexts <- lapply(results, function(result) {
        query_result__context(result$context)
    })
    urls <- unlist(lapply(contexts, .subset2, "query_url"), use.names = FALSE)

    list(
        response = response,
        docs = docs,
        parameter = query_param__clone(params),
        context = list(
            query_url = query_result__query_urls(urls, named = FALSE)
        )
    )
}
# }}}

# query_result__repair_urls {{{
query_result__repair_urls <- function(
    result,
    service = c("OPENDAP", "HTTPServer"),
    index_node = NULL,
    probe = NULL
) {
    service <- match.arg(service)
    probe <- query_result__repair_config(probe)
    type <- query_result__type(
        priv(result)$result_type,
        choices = c("File", "Aggregation")
    )

    docs <- priv(result)$get_docs()
    n <- nrow(docs)
    if (!n) {
        return(priv(result)$result_with_docs(docs))
    }

    reach <- result$reachable(
        service = service,
        level = probe$level,
        probe = probe[names(probe) != "level"]
    )
    needs_repair <- !(reach$reachable %in% TRUE)
    if (!any(needs_repair)) {
        return(priv(result)$result_with_docs(docs))
    }

    identity <- query_result__identity(docs)
    targets <- which(needs_repair)
    has_identity <- !is.na(identity$key[targets]) &
        nzchar(identity$key[targets])
    missing_identity <- targets[!has_identity]
    repair_targets <- targets[has_identity]

    if (length(missing_identity)) {
        cli::cli_warn(
            "Cannot repair {length(missing_identity)} {service} URL{?s} because `instance_id` or `master_id` + `version` is missing."
        )
    }
    if (!length(repair_targets)) {
        return(priv(result)$result_with_docs(docs))
    }

    out <- data.table::as.data.table(docs)

    repaired <- rep(FALSE, n)
    for (i in repair_targets) {
        rows <- setdiff(
            query_result__identity_match(identity[i, , drop = FALSE], identity),
            i
        )
        rows <- rows[query_result__compatible_content(
            docs[i, , drop = FALSE],
            docs[rows, , drop = FALSE]
        )]
        rows <- rows[reach$reachable[rows] %in% TRUE]
        if (!length(rows)) {
            next
        }

        latency <- reach$latency_ms[rows]
        latency[is.na(latency)] <- Inf
        chosen <- rows[order(latency, rows)[[1L]]]
        # Repair only the requested service. The logical record and every
        # already-valid endpoint remain anchored to the original selection.
        out$url[i] <- list(query_result__set_service_url(
            out$url[[i]],
            service,
            reach$url[[chosen]]
        ))
        repaired[[i]] <- TRUE
    }

    can_collect <- identity$has_instance | identity$has_master_version
    external_targets <- repair_targets[
        !repaired[repair_targets] & can_collect[repair_targets]
    ]
    context <- query_result__context(priv(result)$context)
    if (length(external_targets)) {
        candidates <- query_result__collect_identity(
            result,
            identity[external_targets, , drop = FALSE],
            type = type,
            index_node = index_node,
            all = TRUE
        )
        candidate_docs <- priv(candidates)$get_docs()
        context <- query_result__merge_urls(result, priv(candidates)$context)
        if (nrow(candidate_docs)) {
            candidate_reach <- candidates$reachable(
                service = service,
                level = probe$level,
                probe = probe[names(probe) != "level"]
            )
            candidate_identity <- query_result__identity(candidate_docs)

            for (i in external_targets) {
                rows <- query_result__identity_match(
                    identity[i, , drop = FALSE],
                    candidate_identity
                )
                rows <- rows[query_result__compatible_content(
                    docs[i, , drop = FALSE],
                    candidate_docs[rows, , drop = FALSE]
                )]
                rows <- rows[candidate_reach$reachable[rows] %in% TRUE]
                if (!length(rows)) {
                    next
                }

                latency <- candidate_reach$latency_ms[rows]
                latency[is.na(latency)] <- Inf
                chosen <- rows[order(latency, rows)[[1L]]]
                # External replicas supply only a compatible service URL; the
                # caller's logical record, other service, and row order remain
                # unchanged.
                out$url[i] <- list(query_result__set_service_url(
                    out$url[[i]],
                    service,
                    candidate_reach$url[[chosen]]
                ))
                repaired[[i]] <- TRUE
            }
        }
    }

    unrepaired <- repair_targets[!repaired[repair_targets]]
    if (length(unrepaired)) {
        cli::cli_warn(
            "No reachable {service} replica found for {length(unrepaired)} record{?s}; keeping original record{?s}."
        )
    }

    priv(result)$result_with_docs(
        as.data.frame(out, stringsAsFactors = FALSE),
        context = context
    )
}
# }}}

# Replace selected service entries in one raw ESGF URL cell while preserving
# unrelated services exactly as returned by the index node.
# query_result__set_service_url {{{
query_result__set_service_url <- function(value, service, url) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    value <- as.character(value)
    parsed <- strsplit(value, "|", fixed = TRUE)
    keep <- !vapply(
        parsed,
        function(parts) {
            length(parts) == 3L && identical(parts[[3L]], service)
        },
        logical(1L)
    )
    value <- value[keep]
    if (!is.na(url) && nzchar(url)) {
        value <- c(value, paste(url, "application/netcdf", service, sep = "|"))
    }
    unique(value)
}
# }}}

# Describe one service without contacting its endpoint. Deferred HTTP rows use
# the same diagnostic shape as checked services so callers can distinguish a
# selected recovery candidate from evidence that it has already succeeded.
# query_result__deferred_service_rows {{{
query_result__deferred_service_rows <- function(result, service) {
    docs <- priv(result)$get_docs()
    n <- nrow(docs)
    data.table::data.table(
        record_index = seq_len(n),
        id = as.character(query_result__col(docs, "id")),
        data_node = as.character(query_result__col(docs, "data_node")),
        service = rep(service, n),
        url = priv(result)$get_url(service, service),
        reachable = rep(NA, n),
        latency_ms = rep(NA_real_, n),
        error = rep(NA_character_, n),
        probe_level = rep("deferred", n),
        probe_url = rep(NA_character_, n),
        probe_cached = rep(FALSE, n)
    )
}
# }}}

# Resolve the preferred OPeNDAP service before extraction and retain a
# compatible HTTPServer recovery candidate without checking it eagerly. The
# HTTP endpoint is checked and repaired only if execution actually falls back.
# query_result__resolve_file_services {{{
query_result__resolve_file_services <- function(
    result,
    index_node = NULL,
    check = NULL
) {
    if (!inherits(result, "EsgResultFile")) {
        cli::cli_abort(
            "File-service resolution requires an EsgResultFile object."
        )
    }
    services <- c("OPENDAP", "HTTPServer")
    original_docs <- priv(result)$get_docs()
    resolved <- stats::setNames(vector("list", length(services)), services)
    resolved_urls <- stats::setNames(vector("list", length(services)), services)
    diagnostics <- vector("list", length(services))
    contexts <- list(priv(result)$context)

    for (i in seq_along(services)) {
        service <- services[[i]]
        if (identical(service, "OPENDAP")) {
            current <- query_result__repair_urls(
                result,
                service = service,
                index_node = index_node,
                probe = check
            )
            diagnostics[[i]] <- data.table::as.data.table(current$reachable(
                service = service,
                level = "url",
                probe = check[names(check) != "level"]
            ))
        } else {
            current <- result
            diagnostics[[i]] <- query_result__deferred_service_rows(
                current,
                service
            )
        }
        resolved[[service]] <- current
        resolved_urls[[service]] <- priv(current)$get_url(service, service)
        contexts[[length(contexts) + 1L]] <- priv(current)$context
    }

    groups <- query_result__compatible_file_groups(original_docs)
    for (i in seq_along(services)) {
        diagnostics[[i]][, `:=`(
            selected = FALSE,
            selected_url = NA_character_
        )]
    }
    docs <- list()
    for (group in unique(groups)) {
        members <- which(groups == group)
        replica <- as.logical(query_result__col(
            original_docs[members, , drop = FALSE],
            "replica"
        ))
        replica[is.na(replica)] <- TRUE
        base <- members[order(replica, members)[[1L]]]
        row <- original_docs[base, , drop = FALSE]
        if (is.null(row$url)) {
            row$url <- I(list(character()))
        }
        has_service <- FALSE
        for (i in seq_along(services)) {
            service <- services[[i]]
            check_rows <- match(
                members,
                diagnostics[[i]]$record_index
            )
            values <- resolved_urls[[service]][members]
            available <- !is.na(check_rows) & !is.na(values) & nzchar(values)
            if (identical(service, "OPENDAP")) {
                available <- available &
                    diagnostics[[i]]$reachable[check_rows] %in% TRUE
            }
            chosen_url <- NA_character_
            if (any(available)) {
                choices <- which(available)
                latency <- diagnostics[[i]]$latency_ms[check_rows[choices]]
                latency[is.na(latency)] <- Inf
                chosen_local <- choices[order(latency, members[choices])[[1L]]]
                chosen <- members[[chosen_local]]
                chosen_url <- values[[chosen_local]]
                diagnostic_row <- match(
                    chosen,
                    diagnostics[[i]]$record_index
                )
                diagnostics[[i]][
                    diagnostic_row,
                    `:=`(
                        selected = TRUE,
                        selected_url = chosen_url
                    )
                ]
                has_service <- TRUE
            }
            row$url[1L] <- list(query_result__set_service_url(
                row$url[[1L]],
                service,
                chosen_url
            ))
        }
        if (has_service) {
            docs[[length(docs) + 1L]] <- row
        }
    }

    docs <- if (length(docs)) {
        as.data.frame(
            data.table::rbindlist(
                lapply(docs, data.table::as.data.table),
                use.names = TRUE,
                fill = TRUE
            ),
            stringsAsFactors = FALSE
        )
    } else {
        original_docs[0L, , drop = FALSE]
    }
    context_urls <- unique(unlist(
        lapply(contexts, function(context) {
            unname(query_result__context(context)$query_url)
        }),
        use.names = FALSE
    ))
    context <- query_result__context(priv(result)$context)
    context$query_url <- query_result__query_urls(context_urls, named = FALSE)
    list(
        result = priv(result)$result_with_docs(
            docs,
            context = context
        ),
        diagnostics = data.table::rbindlist(
            diagnostics,
            use.names = TRUE,
            fill = TRUE
        )
    )
}
# }}}

# query_result__http_fallback {{{
query_result__http_fallback <- function(
    result,
    indices,
    downloader,
    session_label = NULL,
    progress = TRUE,
    missing_message = "HTTPServer download URLs are missing."
) {
    checkmate::assert_integerish(
        indices,
        lower = 1L,
        any.missing = FALSE,
        min.len = 1L
    )
    checkmate::assert_string(missing_message, min.chars = 1L)

    # HTTPServer is deliberately deferred during normal OPeNDAP resolution.
    # Check the exact recovery subset here and search compatible replicas only
    # for files that genuinely require a full download.
    selected <- result$slice(as.integer(indices))
    workers <- tryCatch(as.integer(downloader$n_workers), error = function(e) {
        1L
    })
    if (length(workers) != 1L || is.na(workers) || workers < 1L) {
        workers <- 1L
    }
    network_policy <- tryCatch(downloader$network_policy, error = function(e) {
        NULL
    })
    selected <- query_result__repair_urls(
        selected,
        service = "HTTPServer",
        probe = list(
            level = "url",
            concurrency = min(workers, 8L),
            network_policy = network_policy,
            cache_failures_seconds = 1800L
        )
    )
    plan <- selected$download_plan(
        replica = "current",
        service = "HTTPServer",
        probe = FALSE
    )
    if (!nrow(plan)) {
        cli::cli_abort(
            "HTTPServer download URLs are missing for one or more file records."
        )
    }

    selected_indices <- seq_along(indices)
    missing <- setdiff(selected_indices, unique(plan$record_index))
    if (length(missing)) {
        cli::cli_abort(c(
            missing_message,
            "x" = "Missing HTTPServer URL: {priv(selected)$record_labels(missing)}"
        ))
    }
    if (is.null(downloader)) {
        cli::cli_abort(
            "HTTP fallback requires an explicit `store` or `downloader` so downloaded files are recoverable."
        )
    }

    session_id <- downloader$enqueue(plan, session_label = session_label)
    tasks <- downloader$run(session_id = session_id, progress = progress)
    failed <- tasks[
        !tasks[["status"]] %in% c("done", "skipped"),
        ,
        drop = FALSE
    ]
    if (nrow(failed)) {
        cli::cli_abort(
            "HTTP fallback download failed for {nrow(failed)} task(s). Inspect downloader$status(session_id = {.val {session_id}}) for details."
        )
    }

    by_logical_file <- stats::setNames(tasks$target_path, tasks$logical_file_id)
    paths <- vapply(
        selected_indices,
        function(index) {
            row <- plan[record_index == index][1L]
            path <- by_logical_file[[row$logical_file_id[[1L]]]]
            if (
                is.null(path) ||
                    is.na(path) ||
                    !nzchar(path) ||
                    !file.exists(path)
            ) {
                cli::cli_abort(
                    "HTTP fallback completed but the downloaded file cannot be found for record {index}."
                )
            }
            path
        },
        character(1L)
    )
    unname(paths)
}
# }}}

# query_result__open_dataset
# Shared OPeNDAP open and HTTP fallback implementation for File and Aggregation
# results. Class-specific wrappers provide labels and error messages.
# query_result__open_dataset {{{
query_result__open_dataset <- function(
    result,
    which = NULL,
    fallback = c("ask", "auto", "error"),
    store = NULL,
    downloader = NULL,
    progress = getOption("epwshiftr.progress", interactive()),
    result_label = "File",
    empty_message,
    unavailable_message,
    http_missing_message
) {
    fallback <- match.arg(fallback)
    checkmate::assert_flag(progress)
    private <- priv(result)

    if (!result$count()) {
        cli::cli_abort(empty_message)
    }

    if (is.null(which)) {
        indices <- seq_len(result$count())
    } else if (is.character(which)) {
        checkmate::assert_character(
            which,
            any.missing = FALSE,
            min.len = 1L,
            unique = TRUE
        )
        checkmate::assert_subset(which, result$id, empty.ok = FALSE)
        indices <- match(which, result$id)
    } else {
        checkmate::assert_integerish(
            which,
            lower = 1L,
            upper = result$count(),
            any.missing = FALSE,
            min.len = 1L,
            unique = TRUE
        )
        indices <- as.integer(which)
    }

    # Pre-open OPeNDAP targets one by one so successful NetCDF handles can be
    # adopted by the final EsgDataset without reopening those URLs.
    urls <- result$url_opendap[indices]
    targets <- urls
    nc_handles <- vector("list", length(urls))
    opendap_errors <- vector("list", length(urls))
    failed <- rep(FALSE, length(urls))
    missing <- is.na(urls)

    # If any later validation or fallback step aborts, close handles that were
    # already opened during the preflight loop.
    close_preopened_handles <- function() {
        open_pos <- base::which(!vapply(nc_handles, is.null, logical(1L)))
        if (!length(open_pos)) {
            return(invisible(NULL))
        }

        dataset__close_handles(targets[open_pos], nc_handles[open_pos])
        nc_handles[open_pos] <<- vector("list", length(open_pos))
        invisible(NULL)
    }
    cleanup_preopened <- TRUE
    on.exit(
        if (isTRUE(cleanup_preopened)) {
            close_preopened_handles()
        },
        add = TRUE
    )

    # Close the progress bar explicitly so later fallback/download progress is
    # reported as a separate operation.
    progress_id <- dataset__progress_bar(
        progress,
        sprintf("Opening %s records", result_label),
        length(urls)
    )
    finish_opendap_progress <- function(ok) {
        if (!is.null(progress_id)) {
            dataset__progress_done(progress_id, ok)
            progress_id <<- NULL
        }
        invisible(NULL)
    }
    on.exit(finish_opendap_progress(FALSE), add = TRUE)

    for (j in seq_along(urls)) {
        if (missing[[j]]) {
            dataset__progress_update(progress_id, j)
            next
        }

        d <- NULL
        ok <- tryCatch(
            {
                d <- EsgDataset$new(urls[[j]])
                d$open(progress = FALSE)
                handles <- dataset__detach_handles(d)
                if (!length(handles) || is.null(handles[[1L]])) {
                    stop(
                        "Opened EsgDataset does not expose a transferable NetCDF handle.",
                        call. = FALSE
                    )
                }
                nc_handles[j] <- handles[1L]
                TRUE
            },
            error = function(e) {
                if (!is.null(d) && is.function(d$close)) {
                    d$close()
                }
                opendap_errors[[j]] <<- e
                FALSE
            }
        )
        failed[[j]] <- !ok
        dataset__progress_update(progress_id, j)
    }
    finish_opendap_progress(TRUE)

    fallback_pos <- base::which(missing | failed)
    if (length(fallback_pos)) {
        missing_pos <- base::which(missing)
        failed_pos <- base::which(failed)
        if (length(missing_pos)) {
            cli::cli_alert_warning(
                "OPeNDAP URLs are missing for {private$record_labels(indices[missing_pos])}."
            )
        }
        if (length(failed_pos)) {
            cli::cli_alert_warning(
                "OPeNDAP connection failed for {private$record_labels(indices[failed_pos])}."
            )
        }

        opendap_error <- if (length(failed_pos)) {
            opendap_errors[[failed_pos[[1L]]]]
        } else {
            NULL
        }
        if (fallback == "error") {
            details <- c(
                if (length(missing_pos)) {
                    "x" <- "Missing OPeNDAP URL: {private$record_labels(indices[missing_pos])}"
                },
                if (length(failed_pos)) {
                    "x" <- "Failed OPeNDAP open: {private$record_labels(indices[failed_pos])}"
                }
            )
            cli::cli_abort(
                c(unavailable_message, details),
                parent = opendap_error
            )
        }

        if (fallback == "ask") {
            if (!interactive()) {
                cli::cli_abort(
                    "Cannot ask for fallback in a non-interactive session. Use fallback = 'auto' to download via HTTP."
                )
            } else {
                answer <- utils::menu(
                    choices = c(
                        sprintf(
                            "Download %d file(s) via HTTP",
                            length(fallback_pos)
                        ),
                        "Cancel"
                    ),
                    title = "OPeNDAP is not available. What would you like to do?"
                )
                if (answer != 1L) {
                    cli::cli_abort("Operation cancelled by user.")
                }
            }
        }

        download_urls <- result$url_download[indices[fallback_pos]]
        if (
            any(is.na(download_urls)) &&
                is.null(store) &&
                is.null(downloader)
        ) {
            http_missing_pos <- fallback_pos[is.na(download_urls)]
            cli::cli_abort(c(
                http_missing_message,
                "x" = "Missing HTTPServer URL: {private$record_labels(indices[http_missing_pos])}"
            ))
        }

        # Only require a downloader after HTTP fallback is known to be necessary.
        downloader <- query_result__resolve_downloader(
            downloader,
            store,
            "HTTP fallback requires an explicit `store` or `downloader` so downloaded files are recoverable."
        )
        cli::cli_alert_info(
            "Downloading {length(fallback_pos)} file(s) via HTTP as fallback..."
        )
        targets[fallback_pos] <- query_result__http_fallback(
            result,
            indices[fallback_pos],
            downloader,
            progress = progress,
            missing_message = http_missing_message
        )
    }

    # Reuse pre-opened handles for successful OPeNDAP records and open only the
    # fallback-downloaded targets that still need handles.
    ds <- EsgDataset$new(targets)
    dataset__adopt_handles(ds, nc_handles)
    if (!isTRUE(ds$is_open)) {
        ds$open(progress = FALSE)
    }
    dataset__set_context(ds, private$update_selection_context(indices))
    cleanup_preopened <- FALSE
    ds
}
# }}}

# vim: fdm=marker :
