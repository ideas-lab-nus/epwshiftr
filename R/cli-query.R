epwshiftr_cli_query <- function(store, command, args) {
    if (identical(command, "list")) {
        parsed <- epwshiftr_cli_parse_command(args)
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$queries())
    }
    if (identical(command, "search")) {
        parsed <- epwshiftr_cli_parse_command(
            args,
            flags = c("--all", "--dry-run"),
            options = c("--index-node", "--type", "--fields", "--limit")
        )
        query <- epwshiftr_cli_search_query(parsed)
        type <- if (is.null(parsed$options[["--type"]])) "Dataset" else parsed$options[["--type"]]
        fields <- epwshiftr_cli_csv(parsed$options[["--fields"]])
        limit <- if (isTRUE(parsed$flags[["--all"]])) {
            FALSE
        } else if (is.null(parsed$options[["--limit"]])) {
            TRUE
        } else {
            epwshiftr_cli_count(parsed$options[["--limit"]], "--limit")
        }
        if (isTRUE(parsed$flags[["--dry-run"]])) {
            return(list(
                url = query$url(),
                type = type,
                fields = fields,
                all = parsed$flags[["--all"]],
                limit = limit,
                query = query$state(null = TRUE)
            ))
        }
        result <- query$collect(type = type, fields = fields, all = parsed$flags[["--all"]], limit = limit)
        return(result$to_data_table(fields = fields))
    }
    if (identical(command, "add")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--track", options = c("--query-file", "--label"))
        epwshiftr_cli_assert_no_positionals(parsed)
        file <- parsed$options[["--query-file"]]
        if (is.null(file)) {
            epwshiftr_cli_usage_abort("query add requires --query-file.")
        }
        query <- esg_query()$load(file)
        before <- store$queries()$query_id
        query_id <- store$add_query(query, label = parsed$options[["--label"]], track = parsed$flags[["--track"]])
        query_id <- epwshiftr_cli_added_query_id(store, query_id, before)
        return(epwshiftr_cli_query_row(store, query_id))
    }
    if (identical(command, "show")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = c("--files", "--updates", "--changes"))
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        wanted_query_id <- query_id
        query <- store$queries()
        query <- query[query[["query_id"]] == wanted_query_id]
        if (!nrow(query)) {
            cli::cli_abort("Unknown query ID: {.val {query_id}}.")
        }
        out <- list(
            query = query[],
            tags = store$query_tags(query_id),
            graph = store$query_graph(query_id = query_id, direction = "both"),
            status = store$query_status(query_id)
        )
        if (isTRUE(parsed$flags[["--files"]])) {
            out$files <- store$query_files(query_id)
        }
        if (isTRUE(parsed$flags[["--updates"]])) {
            out$updates <- store$query_updates(query_id)
        }
        if (isTRUE(parsed$flags[["--changes"]])) {
            updates <- store$query_updates(query_id, latest = TRUE)
            out$changes <- if (nrow(updates)) {
                store$query_changes(update_id = updates$update_id)
            } else {
                data.table::data.table()
            }
        }
        return(out)
    }
    if (identical(command, "status")) {
        parsed <- epwshiftr_cli_parse_command(args)
        query_id <- epwshiftr_cli_optional_position(parsed, "query_id")
        return(store$query_status(query_id))
    }
    if (identical(command, "files")) {
        parsed <- epwshiftr_cli_parse_command(args, options = "--status")
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        return(store$query_files(query_id, status = epwshiftr_cli_csv(parsed$options[["--status"]])))
    }
    if (identical(command, "updates")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--latest")
        query_id <- epwshiftr_cli_optional_position(parsed, "query_id")
        return(store$query_updates(query_id = query_id, latest = parsed$flags[["--latest"]]))
    }
    if (identical(command, "changes")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--latest", options = c("--update", "--type"))
        query_id <- epwshiftr_cli_optional_position(parsed, "query_id")
        update_id <- parsed$options[["--update"]]
        if (is.null(update_id) && isTRUE(parsed$flags[["--latest"]])) {
            updates <- store$query_updates(query_id = query_id, latest = TRUE)
            if (!nrow(updates)) {
                return(data.table::data.table())
            }
            update_id <- if (nrow(updates)) updates$update_id else character()
        }
        return(store$query_changes(
            update_id = epwshiftr_cli_empty_to_null(update_id),
            query_id = query_id,
            change_type = epwshiftr_cli_csv(parsed$options[["--type"]])
        ))
    }
    if (identical(command, "tags")) {
        parsed <- epwshiftr_cli_parse_command(args)
        query_id <- epwshiftr_cli_optional_position(parsed, "query_id")
        return(store$query_tags(query_id))
    }
    if (identical(command, "track")) {
        parsed <- epwshiftr_cli_parse_command(args)
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        store$track_query(query_id)
        return(epwshiftr_cli_query_row(store, query_id))
    }
    if (identical(command, "untrack")) {
        parsed <- epwshiftr_cli_parse_command(args)
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        store$untrack_query(query_id)
        return(epwshiftr_cli_query_row(store, query_id))
    }
    if (identical(command, "tag")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--replace")
        if (length(parsed$positionals) < 2L) {
            epwshiftr_cli_usage_abort("Missing required arguments: query_id and tag.")
        }
        query_id <- parsed$positionals[[1L]]
        tags <- parsed$positionals[-1L]
        return(store$tag_query(query_id, tags, replace = parsed$flags[["--replace"]]))
    }
    if (identical(command, "untag")) {
        parsed <- epwshiftr_cli_parse_command(args)
        if (!length(parsed$positionals)) {
            epwshiftr_cli_usage_abort("Missing required argument: query_id.")
        }
        query_id <- parsed$positionals[[1L]]
        tags <- if (length(parsed$positionals) > 1L) parsed$positionals[-1L] else NULL
        return(store$untag_query(query_id, tags))
    }
    if (identical(command, "remove")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--execute", options = "--delete")
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        delete <- parsed$options[["--delete"]]
        if (is.null(delete)) {
            delete <- "none"
        }
        if (!delete %in% c("none", "orphaned")) {
            epwshiftr_cli_usage_abort("--delete must be one of: none, orphaned.")
        }
        if (isTRUE(parsed$flags[["--execute"]])) {
            return(store$remove_query(query_id, delete = delete))
        }
        return(epwshiftr_cli_query_remove_preview(store, query_id, delete))
    }
    if (identical(command, "preview")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--detail")
        query_id <- epwshiftr_cli_optional_position(parsed, "query_id")
        return(store$preview_update_queries(query_id = query_id, detail = parsed$flags[["--detail"]]))
    }
    if (identical(command, "update")) {
        parsed <- epwshiftr_cli_parse_command(args)
        query_id <- epwshiftr_cli_optional_position(parsed, "query_id")
        return(store$update_queries(query_id = query_id))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown query command: %s", command))
}


epwshiftr_cli_search_query <- function(parsed) {
    index_node <- parsed$options[["--index-node"]]
    query <- if (is.null(index_node)) esg_query() else esg_query(index_node)
    params <- epwshiftr_cli_key_value_params(parsed$positionals)
    if (length(params)) {
        query <- epwshiftr_cli_apply_search_params(query, params)
    }
    query
}


epwshiftr_cli_apply_search_params <- function(query, params) {
    for (key in names(params)) {
        value <- params[[key]]
        method <- tryCatch(query[[key]], error = function(e) NULL)
        if (is.function(method)) {
            query <- method(value)
        } else {
            query <- do.call(query$params, stats::setNames(list(value), key))
        }
    }
    query
}


epwshiftr_cli_key_value_params <- function(values) {
    out <- list()
    for (value in values) {
        if (!grepl("=", value, fixed = TRUE)) {
            epwshiftr_cli_usage_abort(sprintf("Expected key=value query parameter, got: %s", value))
        }
        parts <- strsplit(value, "=", fixed = TRUE)[[1L]]
        key <- parts[[1L]]
        raw <- paste(parts[-1L], collapse = "=")
        if (!nzchar(key)) {
            epwshiftr_cli_usage_abort(sprintf("Query parameter has an empty key: %s", value))
        }
        parsed <- epwshiftr_cli_query_param_value(raw)
        if (key %in% names(out)) {
            out[[key]] <- c(out[[key]], parsed)
        } else {
            out[[key]] <- parsed
        }
    }
    out
}


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


epwshiftr_cli_query_remove_preview <- function(store, query_id, delete = "none") {
    query <- epwshiftr_cli_query_row(store, query_id)
    files <- store$query_files(query_id)
    data.table::data.table(
        dry_run = TRUE,
        query_id = query_id,
        delete = delete,
        file_count = as.integer(nrow(files)),
        tag_count = as.integer(nrow(store$query_tags(query_id))),
        dependency_count = as.integer(nrow(store$query_graph(query_id = query_id, direction = "both"))),
        execute_hint = "rerun with --execute to remove the query"
    )
}


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
    if (length(query_id) && tail(query_id, 1L) %in% after) {
        return(tail(query_id, 1L))
    }
    cli::cli_abort("Could not resolve the imported query ID.")
}
