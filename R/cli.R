#' Run the epwshiftr command line interface
#'
#' @description
#' `epwshiftr_cli()` is the package-level entry point used by the optional
#' `epwshiftr` launcher. It exposes a small ESGF store workflow interface and
#' returns status metadata when `exit = FALSE`, which makes it testable from R.
#'
#' @param args Command line arguments. Defaults to
#'        `commandArgs(trailingOnly = TRUE)`.
#' @param exit Whether to terminate the current R process with the command
#'        status. Default: `FALSE`.
#'
#' @return Invisibly, a list with `status`, `result`, and `error`.
#' @export
epwshiftr_cli <- function(args = commandArgs(trailingOnly = TRUE), exit = FALSE) {
    checkmate::assert_character(args, any.missing = FALSE)
    checkmate::assert_flag(exit)

    parsed <- NULL
    status <- 0L
    result <- NULL
    error <- NULL
    tryCatch(
        {
            parsed <- epwshiftr_cli_parse_globals(args)
            result <- epwshiftr_cli_dispatch(parsed)
            epwshiftr_cli_emit_result(result, json = parsed$json, quiet = parsed$quiet)
        },
        epwshiftr_cli_usage_error = function(e) {
            status <<- 2L
            error <<- conditionMessage(e)
            epwshiftr_cli_emit_error(error, json = epwshiftr_cli_has_flag(args, "--json"), quiet = epwshiftr_cli_has_flag(args, "--quiet"), status = status)
        },
        error = function(e) {
            status <<- 1L
            error <<- conditionMessage(e)
            epwshiftr_cli_emit_error(error, json = epwshiftr_cli_has_flag(args, "--json"), quiet = epwshiftr_cli_has_flag(args, "--quiet"), status = status)
        }
    )
    out <- list(status = status, result = result, error = error)
    if (isTRUE(exit)) {
        quit(status = status, save = "no")
    }
    invisible(out)
}


epwshiftr_cli_parse_globals <- function(args) {
    out <- list(
        store = NULL,
        json = FALSE,
        quiet = FALSE,
        help = FALSE,
        args = character()
    )
    i <- 1L
    while (i <= length(args)) {
        arg <- args[[i]]
        if (identical(arg, "--store")) {
            if (i == length(args)) {
                epwshiftr_cli_usage_abort("--store requires a path.")
            }
            out$store <- args[[i + 1L]]
            i <- i + 2L
            next
        }
        if (startsWith(arg, "--store=")) {
            out$store <- sub("^--store=", "", arg)
            if (!nzchar(out$store)) {
                epwshiftr_cli_usage_abort("--store requires a path.")
            }
            i <- i + 1L
            next
        }
        if (identical(arg, "--json")) {
            out$json <- TRUE
            i <- i + 1L
            next
        }
        if (identical(arg, "--quiet")) {
            out$quiet <- TRUE
            i <- i + 1L
            next
        }
        if (arg %in% c("--help", "-h")) {
            out$help <- TRUE
            i <- i + 1L
            next
        }
        out$args <- c(out$args, arg)
        i <- i + 1L
    }
    out
}


epwshiftr_cli_dispatch <- function(parsed) {
    if (isTRUE(parsed$help)) {
        return(epwshiftr_cli_usage())
    }
    args <- parsed$args
    if (length(args) < 2L) {
        epwshiftr_cli_usage_abort(epwshiftr_cli_usage())
    }

    store <- EsgStore$new(path = parsed$store)
    on.exit(store$close(), add = TRUE)

    group <- args[[1L]]
    command <- args[[2L]]
    rest <- args[-seq_len(2L)]

    if (identical(group, "query")) {
        return(epwshiftr_cli_query(store, command, rest))
    }
    if (identical(group, "download")) {
        return(epwshiftr_cli_download(store, command, rest))
    }
    if (identical(group, "workflow")) {
        return(epwshiftr_cli_workflow(store, command, rest))
    }
    if (identical(group, "storage")) {
        return(epwshiftr_cli_storage(store, command, rest))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown command group: %s", group))
}


epwshiftr_cli_emit_result <- function(result, json = FALSE, quiet = FALSE) {
    if (isTRUE(quiet)) {
        return(invisible(NULL))
    }
    if (isTRUE(json)) {
        cat(jsonlite::toJSON(result, dataframe = "rows", POSIXt = "ISO8601", auto_unbox = TRUE, null = "null", pretty = TRUE))
        cat("\n")
        return(invisible(NULL))
    }
    if (is.character(result)) {
        cli::cli_text(paste(result, collapse = "\n"))
    } else {
        print(result)
    }
    invisible(NULL)
}


epwshiftr_cli_emit_error <- function(message, json = FALSE, quiet = FALSE, status = 1L) {
    if (isTRUE(quiet)) {
        return(invisible(NULL))
    }
    if (isTRUE(json)) {
        cat(jsonlite::toJSON(list(status = status, error = message), auto_unbox = TRUE, null = "null", pretty = TRUE))
        cat("\n")
        return(invisible(NULL))
    }
    cli::cli_alert_danger(message)
    invisible(NULL)
}


epwshiftr_cli_usage <- function() {
    c(
        "Usage: epwshiftr [--store PATH] [--json] [--quiet] <group> <command> [options]",
        "",
        "Commands:",
        "  epwshiftr query list",
        "  epwshiftr query search [--index-node URL] [--type Dataset|File|Aggregation] [--dry-run] key=value ...",
        "  epwshiftr query add --query-file PATH [--label LABEL] [--track]",
        "  epwshiftr query show <query_id> [--files] [--updates] [--changes]",
        "  epwshiftr query status [query_id]",
        "  epwshiftr query files <query_id> [--status STATUS]",
        "  epwshiftr query updates [query_id] [--latest]",
        "  epwshiftr query changes [query_id] [--latest] [--update UPDATE_ID] [--type TYPE]",
        "  epwshiftr query tags [query_id]",
        "  epwshiftr query track <query_id>",
        "  epwshiftr query untrack <query_id>",
        "  epwshiftr query tag <query_id> <tag>... [--replace]",
        "  epwshiftr query untag <query_id> [tag...]",
        "  epwshiftr query remove <query_id> [--delete none|orphaned] [--execute]",
        "  epwshiftr query preview [query_id] [--detail]",
        "  epwshiftr query update [query_id]",
        "  epwshiftr download preflight <query_id> [--replica POLICY] [--strategy STRATEGY] [--no-probe]",
        "  epwshiftr download run <query_id> [--session-label LABEL] [--overwrite] [--no-resume] [--no-progress]",
        "  epwshiftr download status [--query QUERY_ID] [--session SESSION_ID]",
        "  epwshiftr download sessions",
        "  epwshiftr download tasks [--session SESSION_ID] [--status STATUS]",
        "  epwshiftr download events [--session SESSION_ID] [--task TASK_ID]",
        "  epwshiftr download resume [--session SESSION_ID] [--task TASK_ID] [--overwrite] [--no-progress]",
        "  epwshiftr download verify [--session SESSION_ID] [--task TASK_ID]",
        "  epwshiftr download cancel [--session SESSION_ID] [--task TASK_ID]",
        "  epwshiftr download nodes [--service HTTPServer]",
        "  epwshiftr download reset-nodes [--node HOST] [--service HTTPServer] [--execute]",
        "  epwshiftr download retry [--query QUERY_ID] [--session SESSION_ID] [--status STATUS] [--run]",
        "  epwshiftr download config show",
        "  epwshiftr download config set [--workers N] [--timeout SECONDS] [--bandwidth-limit BYTES|none]",
        "  epwshiftr workflow report [--query QUERY_ID]",
        "  epwshiftr storage layout show",
        "  epwshiftr storage layout set --layout flat|dataset|drs|template [--template TEMPLATE]",
        "  epwshiftr storage report [--detail]",
        "  epwshiftr storage validate [--query QUERY_ID] [--checksum]",
        "  epwshiftr storage repair [--execute]",
        "  epwshiftr storage cleanup [--scope SCOPE] [--older-than SECONDS] [--execute]"
    )
}


epwshiftr_cli_usage_abort <- function(message) {
    stop(structure(
        list(message = paste(message, collapse = "\n"), call = NULL),
        class = c("epwshiftr_cli_usage_error", "error", "condition")
    ))
}


epwshiftr_cli_has_flag <- function(args, flag) {
    any(args %in% flag)
}
