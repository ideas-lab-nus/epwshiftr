epwshiftr_cli_shift_show <- function(store, args) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = c("--files", "--outputs"),
        options = c("--query", "--plan", "--morph")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    epwshiftr_cli_shift_graph(
        store,
        epwshiftr_cli_selector(parsed),
        files = isTRUE(parsed$flags[["--files"]]),
        outputs = isTRUE(parsed$flags[["--outputs"]])
    )
}


epwshiftr_cli_shift_watch <- function(store, args, jsonl = FALSE, quiet = FALSE) {
    parsed <- epwshiftr_cli_parse_command(
        args,
        flags = "--follow",
        options = c("--query", "--plan", "--morph", "--interval", "--count", "--events")
    )
    epwshiftr_cli_assert_no_positionals(parsed)
    selector <- epwshiftr_cli_selector(parsed)
    event_count <- epwshiftr_cli_count_or_default(parsed$options[["--events"]], "--events", 10L, positive = FALSE)
    if (isTRUE(parsed$flags[["--follow"]])) {
        return(epwshiftr_cli_shift_watch_follow(
            store,
            selector = selector,
            event_count = event_count,
            interval = epwshiftr_cli_download_interval(parsed$options[["--interval"]], 1),
            count = epwshiftr_cli_count_or_default(parsed$options[["--count"]], "--count", Inf, positive = FALSE),
            jsonl = jsonl,
            quiet = quiet
        ))
    }
    epwshiftr_cli_shift_watch_snapshot(store, selector = selector, event_count = event_count)
}


epwshiftr_cli_shift_graph <- function(store, selector = list(type = NULL, ids = NULL),
                                      files = FALSE, outputs = FALSE) {
    query_status <- tryCatch(epwshiftr_cli_query_status(store, NULL), error = function(e) data.table::data.table())
    plans <- tryCatch(store$coverage(), error = function(e) data.table::data.table())
    morphs <- tryCatch(epwshiftr_cli_morph_status_rows(store, NULL), error = function(e) data.table::data.table())
    epw_outputs <- tryCatch(epwshiftr_cli_morph_output_rows(store, NULL), error = function(e) data.table::data.table())
    links <- epwshiftr_cli_shift_summary_links(store)

    ids <- epwshiftr_cli_shift_related_ids(selector, query_status, plans, morphs, links)
    query_rows <- epwshiftr_cli_shift_filter_rows(query_status, "query_id", ids$query_id)
    plan_rows <- epwshiftr_cli_shift_filter_rows(plans, "plan_id", ids$plan_id)
    morph_rows <- epwshiftr_cli_shift_filter_rows(morphs, "morph_id", ids$morph_id)
    output_rows <- epwshiftr_cli_shift_filter_rows(epw_outputs, "morph_id", ids$morph_id)
    link_rows <- links
    if (length(ids$plan_id)) {
        link_rows <- link_rows[link_rows[["plan_id"]] %in% ids$plan_id]
    }
    if (length(ids$morph_id) && nrow(morph_rows) && "summary_id" %in% names(morph_rows)) {
        link_rows <- link_rows[link_rows[["summary_id"]] %in% morph_rows$summary_id]
    }

    file_rows <- if (isTRUE(files)) {
        shift_file_catalog(store, ids$query_id)
    } else {
        data.table::data.table()
    }
    diagnostics <- epwshiftr_cli_shift_graph_diagnostics(store, ids, plan_rows)
    list(
        summary = epwshiftr_cli_shift_graph_summary(query_rows, plan_rows, morph_rows, output_rows, file_rows, diagnostics),
        queries = query_rows,
        plans = plan_rows,
        links = link_rows,
        morphs = morph_rows,
        outputs = output_rows,
        files = file_rows,
        diagnostics = diagnostics,
        include_files = isTRUE(files),
        include_outputs = isTRUE(outputs)
    )
}


epwshiftr_cli_shift_summary_links <- function(store) {
    shift_query_maybe(store, "SELECT DISTINCT summary_id, plan_id FROM epw_climate_summary")
}


epwshiftr_cli_shift_related_ids <- function(selector, query_status, plans, morphs, links) {
    type <- selector$type
    ids <- selector$ids
    query_id <- character()
    plan_id <- character()
    morph_id <- character()
    if (identical(type, "query")) {
        query_id <- ids
        if (nrow(plans) && "query_id" %in% names(plans)) {
            plan_id <- plans$plan_id[plans$query_id %in% query_id]
        }
        if (nrow(links) && length(plan_id) && nrow(morphs)) {
            summary_id <- links$summary_id[links$plan_id %in% plan_id]
            morph_id <- morphs$morph_id[morphs$summary_id %in% summary_id]
        }
    } else if (identical(type, "plan")) {
        plan_id <- ids
        if (nrow(plans) && "query_id" %in% names(plans)) {
            query_id <- plans$query_id[plans$plan_id %in% plan_id]
        }
        if (nrow(links) && nrow(morphs)) {
            summary_id <- links$summary_id[links$plan_id %in% plan_id]
            morph_id <- morphs$morph_id[morphs$summary_id %in% summary_id]
        }
    } else if (identical(type, "morph")) {
        morph_id <- ids
        summary_id <- if (nrow(morphs) && "summary_id" %in% names(morphs)) {
            morphs$summary_id[morphs$morph_id %in% morph_id]
        } else {
            character()
        }
        if (nrow(links)) {
            plan_id <- links$plan_id[links$summary_id %in% summary_id]
        }
        if (nrow(plans) && "query_id" %in% names(plans)) {
            query_id <- plans$query_id[plans$plan_id %in% plan_id]
        }
    } else {
        query_id <- c(
            if (nrow(query_status) && "query_id" %in% names(query_status)) query_status$query_id else character(),
            if (nrow(plans) && "query_id" %in% names(plans)) plans$query_id else character()
        )
        plan_id <- if (nrow(plans) && "plan_id" %in% names(plans)) plans$plan_id else character()
        morph_id <- if (nrow(morphs) && "morph_id" %in% names(morphs)) morphs$morph_id else character()
    }
    list(
        query_id = unique(epwshiftr_cli_non_empty_chr(query_id)),
        plan_id = unique(epwshiftr_cli_non_empty_chr(plan_id)),
        morph_id = unique(epwshiftr_cli_non_empty_chr(morph_id))
    )
}


epwshiftr_cli_shift_filter_rows <- function(x, column, values) {
    x <- data.table::as.data.table(x)
    values <- epwshiftr_cli_non_empty_chr(values)
    if (!length(values)) {
        return(x[0])
    }
    if (!column %in% names(x)) {
        return(x[0])
    }
    x[x[[column]] %in% values]
}


epwshiftr_cli_non_empty_chr <- function(x) {
    x <- as.character(x)
    x[!is.na(x) & nzchar(x)]
}


epwshiftr_cli_shift_graph_diagnostics <- function(store, ids, plans) {
    data.table::rbindlist(
        list(
            if (length(ids$query_id)) epwshiftr_cli_query_diagnostics(store, ids$query_id) else shift_diagnostics_empty(),
            if (nrow(plans)) shift_diagnostics_from_coverage(plans) else shift_diagnostics_empty(),
            if (length(ids$morph_id)) epwshiftr_cli_morph_diagnostics(store, ids$morph_id) else shift_diagnostics_empty()
        ),
        use.names = TRUE,
        fill = TRUE
    )
}


epwshiftr_cli_shift_graph_summary <- function(queries, plans, morphs, outputs, files, diagnostics) {
    data.frame(
        query_count = nrow(data.table::as.data.table(queries)),
        file_count = nrow(data.table::as.data.table(files)),
        plan_count = nrow(data.table::as.data.table(plans)),
        complete_plans = if (nrow(plans) && "complete" %in% names(plans)) sum(plans$complete %in% TRUE) else 0L,
        failed_plans = if (nrow(plans) && "status" %in% names(plans)) sum(plans$status %in% "failed") else 0L,
        morph_count = nrow(data.table::as.data.table(morphs)),
        output_count = nrow(data.table::as.data.table(outputs)),
        diagnostic_count = nrow(data.table::as.data.table(diagnostics))
    )
}


epwshiftr_cli_shift_watch_snapshot <- function(store, selector = list(type = NULL, ids = NULL), event_count = 10L) {
    graph <- epwshiftr_cli_shift_graph(store, selector, files = FALSE, outputs = TRUE)
    query_id <- if (nrow(graph$queries) && "query_id" %in% names(graph$queries)) graph$queries$query_id else character()
    download_error <- NA_character_
    downloader <- tryCatch(epwshiftr_cli_downloader(store), error = function(e) {
        download_error <<- conditionMessage(e)
        NULL
    })
    downloads <- if (is.null(downloader)) {
        data.table::data.table()
    } else {
        tryCatch(epwshiftr_cli_shift_download_rows(store, downloader, query_id), error = function(e) {
            download_error <<- conditionMessage(e)
            data.table::data.table()
        })
    }
    task_id <- if (nrow(downloads) && "task_id" %in% names(downloads)) unique(downloads$task_id) else NULL
    events <- if (is.null(downloader)) {
        data.table::data.table()
    } else {
        tryCatch(epwshiftr_cli_download_logs(downloader, task_id = task_id, tail = event_count), error = function(e) {
            download_error <<- conditionMessage(e)
            data.table::data.table()
        })
    }
    summary <- graph$summary
    summary$download_task_count <- nrow(downloads)
    summary$download_error <- download_error
    if (nrow(downloads) && "status" %in% names(downloads)) {
        summary$download_active <- any(downloads$status %in% c("queued", "downloading"))
    } else {
        summary$download_active <- FALSE
    }
    list(
        summary = summary,
        queries = graph$queries,
        downloads = downloads,
        plans = graph$plans,
        morphs = graph$morphs,
        outputs = graph$outputs,
        diagnostics = graph$diagnostics,
        events = events
    )
}


epwshiftr_cli_shift_download_rows <- function(store, downloader, query_id) {
    query_id <- epwshiftr_cli_non_empty_chr(query_id)
    if (!length(query_id)) {
        return(data.table::as.data.table(downloader$tasks()))
    }
    data.table::rbindlist(lapply(query_id, function(id) {
        tryCatch(store$download_status(query_id = id, downloader = downloader), error = function(e) data.table::data.table())
    }), use.names = TRUE, fill = TRUE)
}


epwshiftr_cli_shift_watch_follow <- function(store, selector = list(type = NULL, ids = NULL),
                                             event_count = 10L, interval = 1, count = Inf,
                                             jsonl = FALSE, quiet = FALSE) {
    i <- 0L
    repeat {
        i <- i + 1L
        snapshot <- epwshiftr_cli_shift_watch_snapshot(store, selector = selector, event_count = event_count)
        if (isTRUE(quiet)) {
            # no output
        } else if (isTRUE(jsonl)) {
            epwshiftr_cli_emit_jsonl(snapshot)
        } else {
            cat("\014")
            epwshiftr_cli_with_theme(epwshiftr_cli_render_shift_watch(snapshot))
        }
        if (!is.infinite(count) && i >= count) {
            break
        }
        if (!epwshiftr_cli_shift_watch_active(snapshot)) {
            break
        }
        Sys.sleep(interval)
    }
    structure(snapshot, class = c("epwshiftr_cli_emitted", class(snapshot)))
}


epwshiftr_cli_shift_watch_active <- function(snapshot) {
    downloads <- snapshot$downloads
    if (nrow(downloads) && "status" %in% names(downloads) && any(downloads$status %in% c("queued", "downloading"))) {
        return(TRUE)
    }
    morphs <- snapshot$morphs
    nrow(morphs) && "status" %in% names(morphs) && any(morphs$status %in% "running")
}
