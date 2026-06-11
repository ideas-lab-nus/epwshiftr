#' Run the epwshiftr command line interface
#'
#' @description
#' `epwshiftr_cli()` is the package-level entry point used by the optional
#' `epwshiftr` launcher. It exposes a small ESGF store management interface and
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
        return(epwshiftr_cli_help(parsed$args))
    }
    args <- parsed$args
    if (length(args) && identical(args[[1L]], "help")) {
        return(epwshiftr_cli_help(args[-1L]))
    }
    if (length(args) && identical(args[[1L]], "doctor")) {
        return(epwshiftr_cli_doctor(parsed$store, args[-1L]))
    }
    if (length(args) >= 2L && identical(args[[2L]], "help")) {
        topic <- if (length(args) > 2L) c(args[[1L]], args[-seq_len(2L)]) else args[[1L]]
        return(epwshiftr_cli_help(topic))
    }
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
    if (identical(group, "esgf")) {
        return(epwshiftr_cli_esgf(store, command, rest))
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
    epwshiftr_cli_help()
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
