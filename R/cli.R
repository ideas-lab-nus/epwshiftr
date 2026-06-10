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

epwshiftr_cli_query <- function(store, command, args) {
    if (identical(command, "list")) {
        parsed <- epwshiftr_cli_parse_command(args)
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$queries())
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

epwshiftr_cli_download <- function(store, command, args) {
    if (identical(command, "preflight")) {
        parsed <- epwshiftr_cli_parse_command(args)
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        return(store$download_preflight(query_id))
    }
    if (identical(command, "run")) {
        parsed <- epwshiftr_cli_parse_command(args)
        query_id <- epwshiftr_cli_required_position(parsed, "query_id")
        return(store$download_query(query_id))
    }
    if (identical(command, "status")) {
        parsed <- epwshiftr_cli_parse_command(args, options = c("--query", "--session"))
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$download_status(
            query_id = parsed$options[["--query"]],
            session_id = parsed$options[["--session"]]
        ))
    }
    if (identical(command, "retry")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--run", options = c("--query", "--session"))
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$retry_downloads(
            query_id = parsed$options[["--query"]],
            session_id = parsed$options[["--session"]],
            run = parsed$flags[["--run"]]
        ))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown download command: %s", command))
}

epwshiftr_cli_workflow <- function(store, command, args) {
    if (identical(command, "report")) {
        parsed <- epwshiftr_cli_parse_command(args, options = "--query")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$workflow_report(query_id = parsed$options[["--query"]]))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown workflow command: %s", command))
}

epwshiftr_cli_storage <- function(store, command, args) {
    if (identical(command, "report")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--detail")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$storage_report(detail = parsed$flags[["--detail"]]))
    }
    if (identical(command, "validate")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--checksum")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$validate_files(checksum = parsed$flags[["--checksum"]]))
    }
    if (identical(command, "repair")) {
        parsed <- epwshiftr_cli_parse_command(args, flags = "--execute")
        epwshiftr_cli_assert_no_positionals(parsed)
        return(store$repair_files(dry_run = !parsed$flags[["--execute"]]))
    }
    epwshiftr_cli_usage_abort(sprintf("Unknown storage command: %s", command))
}

epwshiftr_cli_parse_command <- function(args, flags = character(), options = character()) {
    flag_values <- stats::setNames(rep(FALSE, length(flags)), flags)
    option_values <- stats::setNames(rep(list(NULL), length(options)), options)
    positionals <- character()
    i <- 1L
    while (i <= length(args)) {
        arg <- args[[i]]
        if (arg %in% flags) {
            flag_values[[arg]] <- TRUE
            i <- i + 1L
            next
        }
        option_match <- options[startsWith(arg, paste0(options, "="))]
        if (length(option_match)) {
            option <- option_match[[1L]]
            option_values[[option]] <- sub(sprintf("^%s=", option), "", arg)
            if (!nzchar(option_values[[option]])) {
                epwshiftr_cli_usage_abort(sprintf("%s requires a value.", option))
            }
            i <- i + 1L
            next
        }
        if (arg %in% options) {
            if (i == length(args)) {
                epwshiftr_cli_usage_abort(sprintf("%s requires a value.", arg))
            }
            option_values[[arg]] <- args[[i + 1L]]
            i <- i + 2L
            next
        }
        if (startsWith(arg, "--")) {
            epwshiftr_cli_usage_abort(sprintf("Unknown option: %s", arg))
        }
        positionals <- c(positionals, arg)
        i <- i + 1L
    }
    list(positionals = positionals, flags = flag_values, options = option_values)
}

epwshiftr_cli_optional_position <- function(parsed, name) {
    if (length(parsed$positionals) > 1L) {
        epwshiftr_cli_usage_abort(sprintf("Too many positional arguments for %s.", name))
    }
    if (!length(parsed$positionals)) {
        return(NULL)
    }
    parsed$positionals[[1L]]
}

epwshiftr_cli_required_position <- function(parsed, name) {
    if (!length(parsed$positionals)) {
        epwshiftr_cli_usage_abort(sprintf("Missing required argument: %s", name))
    }
    if (length(parsed$positionals) > 1L) {
        epwshiftr_cli_usage_abort(sprintf("Too many positional arguments for %s.", name))
    }
    parsed$positionals[[1L]]
}

epwshiftr_cli_assert_no_positionals <- function(parsed) {
    if (length(parsed$positionals)) {
        epwshiftr_cli_usage_abort(sprintf("Unexpected argument: %s", parsed$positionals[[1L]]))
    }
    invisible(NULL)
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
        "  epwshiftr query preview [query_id] [--detail]",
        "  epwshiftr query update [query_id]",
        "  epwshiftr download preflight <query_id>",
        "  epwshiftr download run <query_id>",
        "  epwshiftr download status [--query QUERY_ID] [--session SESSION_ID]",
        "  epwshiftr download retry [--query QUERY_ID] [--session SESSION_ID] [--run]",
        "  epwshiftr workflow report [--query QUERY_ID]",
        "  epwshiftr storage report [--detail]",
        "  epwshiftr storage validate [--checksum]",
        "  epwshiftr storage repair [--execute]"
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
