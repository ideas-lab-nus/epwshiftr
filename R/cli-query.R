# epwshiftr_cli_query {{{
epwshiftr_cli_query <- function(
    store,
    command,
    args,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    opt_id <- function(flags = character(), options = character()) {
        p <- epwshiftr_cli_parse_command(args, flags = flags, options = options)
        list(parsed = p, id = epwshiftr_cli_optional_position(p, "query_id"))
    }
    req_id <- function(flags = character(), options = character()) {
        p <- epwshiftr_cli_parse_command(args, flags = flags, options = options)
        list(parsed = p, id = epwshiftr_cli_required_position(p, "query_id"))
    }
    if (identical(command, "list")) {
        epwshiftr_cli_parse_command(args, no_positionals = TRUE)
        return(store$queries())
    }
    if (identical(command, "search")) {
        p <- epwshiftr_cli_parse_command(
            args,
            flags = c("--all", "--dry-run", "--no-progress"),
            options = c(
                "--index-node",
                "--type",
                "--fields",
                "--columns",
                "--limit"
            )
        )
        query <- epwshiftr_cli_search_query(p)
        type <- shift_stage__coalesce(p$options[["--type"]], "Dataset")
        fields <- epwshiftr_cli_csv(p$options[["--fields"]])
        progress <- !isTRUE(quiet) &&
            !isTRUE(json) &&
            !isTRUE(jsonl) &&
            !isTRUE(p$flags[["--no-progress"]])
        limit <- if (isTRUE(p$flags[["--all"]])) {
            FALSE
        } else if (is.null(p$options[["--limit"]])) {
            TRUE
        } else {
            epwshiftr_cli_count(p$options[["--limit"]], "--limit")
        }
        if (isTRUE(p$flags[["--dry-run"]])) {
            return(list(
                url = query$url(),
                type = type,
                fields = fields,
                all = p$flags[["--all"]],
                limit = limit,
                query = query$state(null = TRUE)
            ))
        }
        return(query$collect(
            type = type,
            fields = fields,
            all = p$flags[["--all"]],
            limit = limit,
            progress = progress
        )$to_data_table(fields = fields))
    }
    if (identical(command, "add")) {
        p <- epwshiftr_cli_parse_command(
            args,
            flags = c("--track", "--dry-run"),
            options = c("--query-file", "--label", "--index-node"),
            multi_options = "--tag"
        )
        file <- p$options[["--query-file"]]
        has_params <- length(p$positionals) > 0L
        if (!is.null(file) && has_params) {
            epwshiftr_cli_usage_abort(
                "query add cannot combine --query-file with key=value query parameters."
            )
        }
        if (!is.null(file) && !is.null(p$options[["--index-node"]])) {
            epwshiftr_cli_usage_abort(
                "query add cannot combine --query-file with --index-node."
            )
        }
        if (is.null(file) && !has_params) {
            epwshiftr_cli_usage_abort(
                "query add requires --query-file or at least one key=value query parameter."
            )
        }
        tags <- epwshiftr_cli_query_tags(p$options[["--tag"]])
        query <- if (is.null(file)) {
            epwshiftr_cli_search_query(p)
        } else {
            esg_query()$load(file)
        }
        if (isTRUE(p$flags[["--dry-run"]])) {
            return(epwshiftr_cli_query_add_preview(
                query,
                label = p$options[["--label"]],
                track = p$flags[["--track"]],
                tags = tags
            ))
        }
        before <- store$queries()$query_id
        query_id <- store$add_query(
            query,
            label = p$options[["--label"]],
            track = p$flags[["--track"]]
        )
        query_id <- epwshiftr_cli_added_query_id(store, query_id, before)
        if (length(tags)) {
            store$tag_query(query_id, tags)
        }
        return(epwshiftr_cli_query_row(store, query_id))
    }
    if (identical(command, "show")) {
        r <- req_id(flags = c("--files", "--updates", "--changes"))
        out <- list(
            query = epwshiftr_cli_query_row(store, r$id),
            tags = store$query_tags(r$id),
            graph = store$query_graph(query_id = r$id, direction = "both"),
            status = store$query_status(r$id)
        )
        if (isTRUE(r$parsed$flags[["--files"]])) {
            out$files <- store$query_files(r$id)
        }
        if (isTRUE(r$parsed$flags[["--updates"]])) {
            out$updates <- store$query_updates(r$id)
        }
        if (isTRUE(r$parsed$flags[["--changes"]])) {
            updates <- store$query_updates(r$id, latest = TRUE)
            out$changes <- if (nrow(updates)) {
                store$query_changes(update_id = updates$update_id)
            } else {
                data.table::data.table()
            }
        }
        return(out)
    }
    if (command %in% c("status", "tags", "update")) {
        method <- c(
            status = "query_status",
            tags = "query_tags",
            update = "update_queries"
        )[[command]]
        return(store[[method]](query_id = opt_id()$id))
    }
    if (identical(command, "files")) {
        r <- req_id(options = "--status")
        return(store$query_files(
            r$id,
            status = epwshiftr_cli_csv(r$parsed$options[["--status"]])
        ))
    }
    if (identical(command, "updates")) {
        r <- opt_id(flags = "--latest")
        return(store$query_updates(
            query_id = r$id,
            latest = r$parsed$flags[["--latest"]]
        ))
    }
    if (identical(command, "changes")) {
        r <- opt_id(flags = "--latest", options = c("--update", "--type"))
        update_id <- r$parsed$options[["--update"]]
        if (is.null(update_id) && isTRUE(r$parsed$flags[["--latest"]])) {
            updates <- store$query_updates(query_id = r$id, latest = TRUE)
            if (!nrow(updates)) {
                return(data.table::data.table())
            }
            update_id <- updates$update_id
        }
        return(store$query_changes(
            update_id = epwshiftr_cli_empty_to_null(update_id),
            query_id = r$id,
            change_type = epwshiftr_cli_csv(r$parsed$options[["--type"]])
        ))
    }
    if (command %in% c("track", "untrack")) {
        query_id <- req_id()$id
        store[[paste0(command, "_query")]](query_id)
        return(epwshiftr_cli_query_row(store, query_id))
    }
    if (identical(command, "tag")) {
        p <- epwshiftr_cli_parse_command(args, flags = "--replace")
        if (length(p$positionals) < 2L) {
            epwshiftr_cli_usage_abort(
                "Missing required arguments: query_id and tag."
            )
        }
        return(store$tag_query(
            p$positionals[[1L]],
            p$positionals[-1L],
            replace = p$flags[["--replace"]]
        ))
    }
    if (identical(command, "untag")) {
        p <- epwshiftr_cli_parse_command(args)
        if (!length(p$positionals)) {
            epwshiftr_cli_usage_abort("Missing required argument: query_id.")
        }
        tags <- if (length(p$positionals) > 1L) p$positionals[-1L]
        return(store$untag_query(p$positionals[[1L]], tags))
    }
    if (identical(command, "remove")) {
        r <- req_id(flags = "--execute", options = "--delete")
        delete <- epwshiftr_cli_choice(
            r$parsed$options[["--delete"]],
            c("none", "orphaned"),
            "--delete",
            default = "none"
        )
        if (isTRUE(r$parsed$flags[["--execute"]])) {
            return(store$remove_query(r$id, delete = delete))
        }
        return(epwshiftr_cli_query_remove_preview(store, r$id, delete))
    }
    if (identical(command, "preview")) {
        r <- opt_id(flags = "--detail")
        return(store$preview_update_queries(
            query_id = r$id,
            detail = r$parsed$flags[["--detail"]]
        ))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown query command: %s", command))
}
# }}}

# epwshiftr_cli_search_query {{{
epwshiftr_cli_search_query <- function(parsed) {
    index_node <- parsed$options[["--index-node"]]
    query <- if (is.null(index_node)) esg_query() else esg_query(index_node)
    params <- epwshiftr_cli_key_value_params(parsed$positionals)
    if (length(params)) {
        query <- epwshiftr_cli_apply_search_params(query, params)
    }
    query
}
# }}}

# epwshiftr_cli_apply_search_params {{{
epwshiftr_cli_apply_search_params <- function(query, params) {
    for (key in names(params)) {
        value <- params[[key]]
        condition_query <- epwshiftr_cli_apply_query_condition_param(
            query,
            key,
            value
        )
        if (!is.null(condition_query)) {
            query <- condition_query
            next
        }
        method <- tryCatch(query[[key]], error = function(e) NULL)
        query <- if (is.function(method)) {
            epwshiftr_cli_apply_query_param(method, value)
        } else {
            epwshiftr_cli_apply_query_param(query$params, value, name = key)
        }
    }
    query
}
# }}}

# epwshiftr_cli_apply_query_condition_param {{{
epwshiftr_cli_apply_query_condition_param <- function(query, key, value) {
    aliases <- c(
        datetime_start = "datetime_start",
        datetime_stop = "datetime_stop",
        datetime_end = "datetime_stop",
        timestamp_from = "timestamp_from",
        timestamp_to = "timestamp_to",
        version_min = "version_min",
        version_max = "version_max"
    )
    if (!key %in% names(aliases)) {
        return(NULL)
    }
    if (epwshiftr_cli_is_negated_param(value)) {
        epwshiftr_cli_usage_abort(sprintf(
            "Query condition cannot be negated: %s",
            key
        ))
    }
    if (is.list(value) && all(c("value", "negate") %in% names(value))) {
        value <- value$value
    }
    if (length(value) != 1L) {
        epwshiftr_cli_usage_abort(sprintf(
            "Query condition expects exactly one value: %s",
            key
        ))
    }
    switch(
        aliases[[key]],
        datetime_start = query$datetime_range(start = value),
        datetime_stop = query$datetime_range(stop = value),
        timestamp_from = query$timestamp_range(from = value),
        timestamp_to = query$timestamp_range(to = value),
        version_min = query$version_range(min = value),
        version_max = query$version_range(max = value)
    )
}
# }}}

# epwshiftr_cli_apply_query_param {{{
epwshiftr_cli_apply_query_param <- function(method, value, name = NULL) {
    if (epwshiftr_cli_is_negated_param(value)) {
        args <- list(as.call(list(as.name("!"), value$value)))
        if (!is.null(name)) {
            args <- stats::setNames(args, name)
        }
        return(eval(as.call(c(list(method), args))))
    }
    if (is.list(value) && all(c("value", "negate") %in% names(value))) {
        value <- value$value
    }
    args <- list(value)
    if (!is.null(name)) {
        args <- stats::setNames(args, name)
    }
    do.call(method, args)
}
# }}}

# epwshiftr_cli_is_negated_param {{{
epwshiftr_cli_is_negated_param <- function(value) {
    is.list(value) &&
        all(c("value", "negate") %in% names(value)) &&
        isTRUE(value$negate)
}
# }}}

# epwshiftr_cli_key_value_params {{{
epwshiftr_cli_key_value_params <- function(values) {
    out <- list()
    for (value in values) {
        if (!grepl("=", value, fixed = TRUE)) {
            epwshiftr_cli_usage_abort(sprintf(
                "Expected key=value or key!=value query parameter, got: %s",
                value
            ))
        }
        first_equal <- regexpr("=", value, fixed = TRUE)[[1L]]
        first_bang_equal <- regexpr("!=", value, fixed = TRUE)[[1L]]
        negate <- first_bang_equal > 0L && first_equal == first_bang_equal + 1L
        sep <- if (negate) "!=" else "="
        parts <- strsplit(value, sep, fixed = TRUE)[[1L]]
        key <- parts[[1L]]
        raw <- paste(parts[-1L], collapse = sep)
        if (!nzchar(key)) {
            epwshiftr_cli_usage_abort(sprintf(
                "Query parameter has an empty key: %s",
                value
            ))
        }
        parsed <- list(
            value = epwshiftr_cli_query_param_value(raw),
            negate = negate
        )
        if (key %in% names(out)) {
            if (!identical(isTRUE(out[[key]]$negate), isTRUE(parsed$negate))) {
                epwshiftr_cli_usage_abort(sprintf(
                    "Cannot combine positive and negated values for query parameter: %s",
                    key
                ))
            }
            out[[key]]$value <- c(out[[key]]$value, parsed$value)
        } else {
            out[[key]] <- parsed
        }
    }
    out
}
# }}}

# epwshiftr_cli_query_param_value {{{
epwshiftr_cli_query_param_value <- function(value) {
    values <- epwshiftr_cli_csv(value)
    if (is.null(values)) {
        return("")
    }
    lowered <- tolower(values)
    if (length(values) == 1L && lowered %in% c("true", "false")) {
        return(identical(lowered, "true"))
    }
    values
}
# }}}

# epwshiftr_cli_query_tags {{{
epwshiftr_cli_query_tags <- function(values) {
    if (is.null(values) || !length(values)) {
        return(character())
    }
    tags <- epwshiftr_cli_csv(as.character(values))
    if (!length(tags)) {
        epwshiftr_cli_usage_abort("--tag requires a non-empty tag.")
    }
    tags
}
# }}}

# epwshiftr_cli_query_add_preview {{{
epwshiftr_cli_query_add_preview <- function(
    query,
    label = NULL,
    track = FALSE,
    tags = character()
) {
    list(
        dry_run = TRUE,
        url = query$url(),
        label = label,
        tracked = isTRUE(track),
        tags = tags,
        query = query$state(null = TRUE)
    )
}
# }}}

# epwshiftr_cli_query_remove_preview {{{
epwshiftr_cli_query_remove_preview <- function(
    store,
    query_id,
    delete = "none"
) {
    epwshiftr_cli_query_row(store, query_id)
    files <- store$query_files(query_id)
    data.table::data.table(
        dry_run = TRUE,
        query_id = query_id,
        delete = delete,
        file_count = as.integer(nrow(files)),
        tag_count = as.integer(nrow(store$query_tags(query_id))),
        dependency_count = as.integer(nrow(store$query_graph(
            query_id = query_id,
            direction = "both"
        ))),
        execute_hint = "rerun with --execute to remove the query"
    )
}
# }}}

# epwshiftr_cli_query_row {{{
epwshiftr_cli_query_row <- function(store, query_id) {
    query_id <- as.character(query_id)
    if (length(query_id) != 1L || is.na(query_id) || !nzchar(query_id)) {
        cli::cli_abort("Expected exactly one query ID.")
    }
    wanted_query_id <- query_id
    query <- store$queries()
    query <- query[query[["query_id"]] == wanted_query_id]
    if (!nrow(query)) {
        cli::cli_abort("Unknown query ID: {.val {query_id}}.")
    }
    query[]
}
# }}}

# epwshiftr_cli_added_query_id {{{
epwshiftr_cli_added_query_id <- function(store, query_id, before) {
    query_id <- as.character(query_id)
    after <- store$queries()$query_id
    if (length(query_id) == 1L && query_id %in% after) {
        return(query_id)
    }
    added <- setdiff(after, before)
    if (length(added) == 1L) {
        return(added)
    }
    if (length(query_id) && utils::tail(query_id, 1L) %in% after) {
        return(utils::tail(query_id, 1L))
    }
    cli::cli_abort("Could not resolve the imported query ID.")
}
# }}}

# vim: fdm=marker :
