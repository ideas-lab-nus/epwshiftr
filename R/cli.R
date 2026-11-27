#' Run the epwshiftr command line interface
#'
#' @description
#' `epwshiftr_cli()` is the package-level entry point used by the optional
#' `epwshiftr` launcher. It exposes ESGF store operations, weather-method
#' discovery, and persistent single-run or batch future-weather workflows, and
#' returns status metadata when `exit = FALSE`, which makes it testable from R.
#'
#' @param args Command line arguments. Defaults to
#'        `commandArgs(trailingOnly = TRUE)`.
#' @param exit Whether to terminate the current R process with the command
#'        status. Default: `FALSE`.
#'
#' @return Invisibly, a list with `status`, `result`, and `error`.
#' @export
# epwshiftr_cli {{{
epwshiftr_cli <- function(
    args = commandArgs(trailingOnly = TRUE),
    exit = FALSE
) {
    checkmate::assert_character(args, any.missing = FALSE)
    checkmate::assert_flag(exit)

    parsed <- NULL
    status <- 0L
    result <- NULL
    error <- NULL
    handle_error <- function(code) {
        function(e) {
            status <<- code
            error <<- conditionMessage(e)
            epwshiftr_cli_emit_error(
                error,
                json = epwshiftr_cli_has_flag(args, "--json"),
                jsonl = epwshiftr_cli_has_flag(args, "--jsonl"),
                quiet = epwshiftr_cli_has_flag(args, "--quiet"),
                status = status
            )
        }
    }
    tryCatch(
        {
            parsed <- epwshiftr_cli_parse_globals(args)
            result <- epwshiftr_cli_dispatch(parsed)
            # Operational failures can be returned as inspectable batch or
            # partial-run results. Automation still needs a nonzero exit code.
            if (
                length(parsed$args) >= 2L &&
                    identical(parsed$args[[1L]], "shift") &&
                    parsed$args[[2L]] %in% c("run", "resume") &&
                    is.list(result) &&
                    !is.data.frame(result) &&
                    isTRUE(
                        result$status %in%
                            c("failed", "blocked", "partial", "cancelled")
                    )
            ) {
                status <- 1L
            }
            if (
                is.list(result) &&
                    !is.data.frame(result) &&
                    identical(result$readiness, "blocked")
            ) {
                status <- 1L
            }
            epwshiftr_cli_emit_result(
                result,
                json = parsed$json,
                jsonl = parsed$jsonl,
                quiet = parsed$quiet,
                context = epwshiftr_cli_context(parsed)
            )
        },
        epwshiftr_cli_usage_error = handle_error(2L),
        error = handle_error(1L)
    )
    out <- list(status = status, result = result, error = error)
    if (isTRUE(exit)) {
        quit(status = status, save = "no")
    }
    invisible(out)
}
# }}}

# epwshiftr_cli_parse_globals {{{
epwshiftr_cli_parse_globals <- function(args) {
    out <- list(
        store = NULL,
        json = FALSE,
        jsonl = FALSE,
        quiet = FALSE,
        help = FALSE,
        args = character()
    )
    flag_map <- c(
        "--json" = "json",
        "--jsonl" = "jsonl",
        "--quiet" = "quiet",
        "--help" = "help",
        "-h" = "help"
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
        if (arg %in% names(flag_map)) {
            out[[flag_map[[arg]]]] <- TRUE
            i <- i + 1L
            next
        }
        out$args <- c(out$args, arg)
        i <- i + 1L
    }
    out
}
# }}}

# Translate command flags into the same UI contract used by the R shift APIs.
# Machine-readable and quiet modes always select the null renderer so progress
# events remain durable without contaminating stdout.
# epwshiftr_cli_task_ui {{{
epwshiftr_cli_task_ui <- function(
    parsed,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE
) {
    # Commands expose different flag sets. List lookup treats missing flags as
    # disabled instead of throwing on a named atomic-vector subscript.
    flags <- as.list(shift_stage__coalesce(parsed$flags, list()))
    progress <- if (
        isTRUE(quiet) ||
            isTRUE(json) ||
            isTRUE(jsonl) ||
            isTRUE(flags[["--no-progress"]])
    ) {
        "none"
    } else {
        "auto"
    }
    detail <- if (isTRUE(flags[["--debug"]])) {
        "debug"
    } else if (isTRUE(flags[["--verbose"]])) {
        "detail"
    } else {
        "normal"
    }
    motion <- if (isTRUE(flags[["--reduced-motion"]])) "reduced" else "auto"
    shift_ui(progress = progress, detail = detail, motion = motion)
}
# }}}

# epwshiftr_cli_dispatch {{{
epwshiftr_cli_dispatch <- function(parsed) {
    if (isTRUE(parsed$help)) {
        return(epwshiftr_cli_help(parsed$args))
    }
    args <- parsed$args
    if (length(args) && identical(args[[1L]], "help")) {
        return(epwshiftr_cli_help(args[-1L]))
    }
    if (length(args) && identical(args[[1L]], "doctor")) {
        return(epwshiftr_cli_doctor(
            parsed$store,
            args[-1L],
            ui = if (parsed$json || parsed$jsonl || parsed$quiet) {
                shift_ui("none")
            } else {
                shift_ui()
            }
        ))
    }
    if (length(args) >= 2L && identical(args[[2L]], "help")) {
        topic <- if (length(args) > 2L) {
            c(args[[1L]], args[-seq_len(2L)])
        } else {
            args[[1L]]
        }
        return(epwshiftr_cli_help(topic))
    }
    if (length(args) < 2L) {
        epwshiftr_cli_usage_abort(epwshiftr_cli_usage())
    }

    group <- args[[1L]]
    command <- args[[2L]]
    rest <- args[-seq_len(2L)]
    ui_args <- list(
        json = parsed$json,
        jsonl = parsed$jsonl,
        quiet = parsed$quiet
    )

    if (identical(group, "shift")) {
        # Generating a JSON template needs neither a database nor a writable
        # default store. History also inspects existing stores without creation.
        if (
            identical(command, "config") &&
                length(rest) &&
                identical(rest[[1L]], "example")
        ) {
            return(do.call(
                epwshiftr_cli_shift_config,
                c(list(NULL, rest), ui_args)
            ))
        }
        # Shift status/watch/cancel/log commands must be able to fall back to
        # atomic live sidecars while a detached worker owns DuckDB's process
        # lock. Passing the path keeps that fallback reachable; eagerly opening
        # EsgStore here would fail before the shift command could inspect it.
        store_path <- if (is.null(parsed$store)) {
            store_dir(init = !identical(command, "list"))
        } else {
            store_normalize_path(parsed$store)
        }
        return(do.call(
            epwshiftr_cli_shift,
            c(list(store_path, command, rest), ui_args)
        ))
    }

    # Scientific catalog and option inspection need no writable store.
    if (
        identical(group, "morph") &&
            command %in% c("transforms", "describe", "variables")
    ) {
        return(do.call(
            epwshiftr_cli_morph,
            c(list(NULL, command, rest), ui_args)
        ))
    }

    ui_groups <- list(
        query = epwshiftr_cli_query,
        download = epwshiftr_cli_download,
        extract = epwshiftr_cli_extract,
        morph = epwshiftr_cli_morph
    )
    plain_groups <- list(
        esgf = epwshiftr_cli_esgf,
        storage = epwshiftr_cli_storage
    )
    if (!group %in% c(names(ui_groups), names(plain_groups))) {
        epwshiftr_cli_usage_abort(sprintf("Unknown command group: %s", group))
    }

    store <- EsgStore$new(path = parsed$store)
    on.exit(store$close(), add = TRUE)

    if (group %in% names(ui_groups)) {
        return(do.call(
            ui_groups[[group]],
            c(list(store, command, rest), ui_args)
        ))
    }
    plain_groups[[group]](store, command, rest)
}
# }}}

# cli__write_json {{{
cli__write_json <- function(
    x,
    pretty = FALSE,
    dataframe = "rows",
    flush = FALSE
) {
    cat(
        jsonlite::toJSON(
            x,
            dataframe = dataframe,
            POSIXt = "ISO8601",
            auto_unbox = TRUE,
            null = "null",
            pretty = pretty
        ),
        "\n",
        sep = ""
    )
    if (isTRUE(flush)) {
        flush.console()
    }
    invisible(NULL)
}
# }}}

# epwshiftr_cli_emit_result {{{
epwshiftr_cli_emit_result <- function(
    result,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE,
    context = NULL
) {
    if (inherits(result, "epwshiftr_cli_emitted") || isTRUE(quiet)) {
        return(invisible(NULL))
    }
    if (isTRUE(jsonl) || isTRUE(json)) {
        return(cli__write_json(result, pretty = !isTRUE(jsonl)))
    }
    epwshiftr_cli_with_theme(epwshiftr_cli_render(result, context = context))
    invisible(NULL)
}
# }}}

# epwshiftr_cli_emit_error {{{
epwshiftr_cli_emit_error <- function(
    message,
    json = FALSE,
    jsonl = FALSE,
    quiet = FALSE,
    status = 1L
) {
    if (isTRUE(quiet)) {
        return(invisible(NULL))
    }
    if (isTRUE(jsonl) || isTRUE(json)) {
        return(cli__write_json(
            list(status = status, error = message),
            pretty = !isTRUE(jsonl)
        ))
    }
    cli::cli_alert_danger(message)
    invisible(NULL)
}
# }}}

# epwshiftr_cli_with_theme {{{
epwshiftr_cli_with_theme <- function(expr) {
    div <- cli::cli_div(theme = epwshiftr_cli_theme(), .auto_close = FALSE)
    on.exit(cli::cli_end(div), add = TRUE)
    force(expr)
}
# }}}

# epwshiftr_cli_theme {{{
epwshiftr_cli_theme <- function() {
    list(
        h1 = list(
            "font-weight" = "bold",
            color = "cyan",
            "margin-top" = 0,
            "margin-bottom" = 1,
            fmt = function(x) cli::rule(left = x, line = 2, line_col = "cyan")
        ),
        h2 = list(
            "font-weight" = "bold",
            "margin-top" = 1,
            "margin-bottom" = 0,
            fmt = function(x) cli::rule(left = x, line = 1)
        ),
        par = list(
            "margin-top" = 0,
            "margin-bottom" = 0
        )
    )
}
# }}}

# epwshiftr_cli_usage {{{
epwshiftr_cli_usage <- function() {
    epwshiftr_cli_help()
}
# }}}

# epwshiftr_cli_usage_abort {{{
epwshiftr_cli_usage_abort <- function(message) {
    stop(structure(
        list(message = paste(message, collapse = "\n"), call = NULL),
        class = c("epwshiftr_cli_usage_error", "error", "condition")
    ))
}
# }}}

# epwshiftr_cli_has_flag {{{
epwshiftr_cli_has_flag <- function(args, flag) {
    any(args %in% flag)
}
# }}}

CLI_UI_FLAGS <- c("--no-progress", "--reduced-motion", "--verbose", "--debug")
CLI_RUN_FLAGS <- c("--overwrite", "--no-resume", CLI_UI_FLAGS)

# epwshiftr_cli_parse_command {{{
epwshiftr_cli_parse_command <- function(
    args,
    flags = character(),
    options = character(),
    multi_options = character(),
    no_positionals = FALSE
) {
    flag_values <- stats::setNames(rep(FALSE, length(flags)), flags)
    option_values <- stats::setNames(rep(list(NULL), length(options)), options)
    multi_option_values <- stats::setNames(
        rep(list(character()), length(multi_options)),
        multi_options
    )
    positionals <- character()
    all_options <- c(options, multi_options)
    i <- 1L
    while (i <= length(args)) {
        arg <- args[[i]]
        if (arg %in% flags) {
            flag_values[[arg]] <- TRUE
            i <- i + 1L
            next
        }
        option_match <- all_options[startsWith(arg, paste0(all_options, "="))]
        if (length(option_match)) {
            option <- option_match[[1L]]
            value <- sub(sprintf("^%s=", option), "", arg)
            if (!nzchar(value)) {
                epwshiftr_cli_usage_abort(sprintf(
                    "%s requires a value.",
                    option
                ))
            }
            if (option %in% multi_options) {
                multi_option_values[[option]] <- c(
                    multi_option_values[[option]],
                    value
                )
            } else {
                option_values[[option]] <- value
            }
            i <- i + 1L
            next
        }
        if (arg %in% all_options) {
            if (i == length(args)) {
                epwshiftr_cli_usage_abort(sprintf("%s requires a value.", arg))
            }
            if (arg %in% multi_options) {
                multi_option_values[[arg]] <- c(
                    multi_option_values[[arg]],
                    args[[i + 1L]]
                )
            } else {
                option_values[[arg]] <- args[[i + 1L]]
            }
            i <- i + 2L
            next
        }
        if (startsWith(arg, "--")) {
            epwshiftr_cli_usage_abort(sprintf("Unknown option: %s", arg))
        }
        positionals <- c(positionals, arg)
        i <- i + 1L
    }
    out <- list(
        positionals = positionals,
        flags = flag_values,
        options = c(option_values, multi_option_values)
    )
    if (isTRUE(no_positionals)) {
        epwshiftr_cli_assert_no_positionals(out)
    }
    out
}
# }}}

# epwshiftr_cli_optional_position {{{
epwshiftr_cli_optional_position <- function(parsed, name) {
    if (length(parsed$positionals) > 1L) {
        epwshiftr_cli_usage_abort(sprintf(
            "Too many positional arguments for %s.",
            name
        ))
    }
    if (!length(parsed$positionals)) {
        return(NULL)
    }
    parsed$positionals[[1L]]
}
# }}}

# epwshiftr_cli_required_position {{{
epwshiftr_cli_required_position <- function(parsed, name) {
    if (!length(parsed$positionals)) {
        epwshiftr_cli_usage_abort(sprintf(
            "Missing required argument: %s",
            name
        ))
    }
    if (length(parsed$positionals) > 1L) {
        epwshiftr_cli_usage_abort(sprintf(
            "Too many positional arguments for %s.",
            name
        ))
    }
    parsed$positionals[[1L]]
}
# }}}

# epwshiftr_cli_assert_no_positionals {{{
epwshiftr_cli_assert_no_positionals <- function(parsed) {
    if (length(parsed$positionals)) {
        epwshiftr_cli_usage_abort(sprintf(
            "Unexpected argument: %s",
            parsed$positionals[[1L]]
        ))
    }
    invisible(NULL)
}
# }}}

# epwshiftr_cli_csv {{{
epwshiftr_cli_csv <- function(value) {
    if (is.null(value) || !length(value)) {
        return(NULL)
    }
    value <- unlist(strsplit(value, ",", fixed = TRUE), use.names = FALSE)
    value <- trimws(value)
    value <- value[nzchar(value)]
    if (!length(value)) {
        return(NULL)
    }
    unique(value)
}
# }}}

# epwshiftr_cli_count {{{
epwshiftr_cli_count <- function(value, name, positive = TRUE) {
    if (is.null(value)) {
        return(NULL)
    }
    out <- suppressWarnings(as.numeric(value))
    if (length(out) != 1L || is.na(out)) {
        epwshiftr_cli_usage_abort(sprintf("%s must be a number.", name))
    }
    checkmate::assert_count(out, positive = positive, .var.name = name)
    as.integer(out)
}
# }}}

# epwshiftr_cli_count_or_default {{{
epwshiftr_cli_count_or_default <- function(
    value,
    name,
    default,
    positive = TRUE
) {
    if (is.null(value)) {
        return(default)
    }
    epwshiftr_cli_count(value, name, positive = positive)
}
# }}}

# epwshiftr_cli_bool {{{
epwshiftr_cli_bool <- function(value, name, default = NULL) {
    if (is.null(value)) {
        return(default)
    }
    normalized <- tolower(trimws(value))
    if (normalized %in% c("true", "t", "1", "yes", "y")) {
        return(TRUE)
    }
    if (normalized %in% c("false", "f", "0", "no", "n")) {
        return(FALSE)
    }
    epwshiftr_cli_usage_abort(sprintf("%s must be true or false.", name))
}
# }}}

# epwshiftr_cli_string_or_null {{{
epwshiftr_cli_string_or_null <- function(value) {
    normalized <- tolower(trimws(value))
    if (!nzchar(normalized) || normalized %in% c("none", "null", "na")) {
        return(NULL)
    }
    value
}
# }}}

# epwshiftr_cli_count_or_null {{{
epwshiftr_cli_count_or_null <- function(value, name, positive = TRUE) {
    if (is.null(epwshiftr_cli_string_or_null(value))) {
        return(NULL)
    }
    epwshiftr_cli_count(value, name, positive = positive)
}
# }}}

# epwshiftr_cli_empty_to_null {{{
epwshiftr_cli_empty_to_null <- function(value) {
    if (is.null(value) || !length(value)) {
        return(NULL)
    }
    value
}
# }}}

# Resolve and validate statuses selected for a retry command.
# cli_retry__resolve_statuses {{{
cli_retry__resolve_statuses <- function(value, choices) {
    statuses <- shift_stage__coalesce(epwshiftr_cli_csv(value), "failed")
    if (any(!statuses %in% choices)) {
        epwshiftr_cli_usage_abort(sprintf(
            "--status must be one of: %s.",
            paste(choices, collapse = ", ")
        ))
    }
    statuses
}
# }}}

# Filter retry candidates and decide whether execution should proceed.
# cli_retry__prepare_candidates {{{
cli_retry__prepare_candidates <- function(candidates, statuses, run) {
    if (nrow(candidates)) {
        # The plural selector avoids data.table resolving it as the candidate
        # table's singular status column.
        candidates <- candidates[candidates[["status"]] %in% statuses]
    }

    execute <- isTRUE(run) && nrow(candidates) > 0L
    # Preview rows are explicitly marked, while empty results retain their
    # command-specific schema.
    if (!execute && nrow(candidates)) {
        data.table::set(candidates, j = "dry_run", value = TRUE)
    }

    list(candidates = candidates, execute = execute)
}
# }}}

#' Install the epwshiftr command launcher
#'
#' @description
#' `install_cli()` writes a small platform launcher that runs the current
#' `Rscript` with `epwshiftr::epwshiftr_cli(exit = TRUE)`. It does not modify
#' shell profiles or `PATH`.
#'
#' @param bin_dir Directory where the launcher should be written. Defaults to
#'        `~/.local/bin` on macOS/Linux and
#'        `%LOCALAPPDATA%/epwshiftr/bin` on Windows.
#' @param name Launcher command name. Default: `"epwshiftr"`.
#' @param overwrite Whether to replace an existing launcher. Default: `FALSE`.
#'
#' @return A data.table describing the installed launcher.
#' @export
# install_cli {{{
install_cli <- function(bin_dir = NULL, name = "epwshiftr", overwrite = FALSE) {
    checkmate::assert_string(bin_dir, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_flag(overwrite)
    if (!grepl("^[A-Za-z][A-Za-z0-9._-]*$", name)) {
        cli::cli_abort(
            "{.arg name} must start with a letter and contain only letters, digits, '.', '_', or '-'."
        )
    }

    spec <- epwshiftr_cli_launcher_spec(bin_dir, name)
    if (file.exists(spec$path) && !isTRUE(overwrite)) {
        cli::cli_abort(
            "Launcher already exists at {.path {spec$path}}. Use {.code overwrite = TRUE} to replace it."
        )
    }
    if (!dir.exists(spec$bin_dir)) {
        dir.create(spec$bin_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!dir.exists(spec$bin_dir)) {
        cli::cli_abort(
            "Failed to create launcher directory: {.path {spec$bin_dir}}."
        )
    }
    spec$bin_dir <- normalizePath(spec$bin_dir, mustWork = TRUE, winslash = "/")
    spec$path <- file.path(spec$bin_dir, basename(spec$path))

    writeLines(
        epwshiftr_cli_launcher_content(spec$windows),
        spec$path,
        useBytes = TRUE
    )
    if (!isTRUE(spec$windows)) {
        Sys.chmod(spec$path, mode = "0755")
    }
    in_path <- epwshiftr_cli_bin_in_path(spec$bin_dir)
    if (!isTRUE(in_path)) {
        cli::cli_inform(
            "Launcher installed at {.path {spec$path}}, but {.path {spec$bin_dir}} is not currently on PATH."
        )
    }
    data.table::data.table(
        path = spec$path,
        bin_dir = spec$bin_dir,
        command = spec$command,
        in_path = in_path,
        installed = TRUE
    )
}
# }}}

#' Uninstall the epwshiftr command launcher
#'
#' @description
#' Remove a launcher generated by `install_cli()`.
#'
#' @inheritParams install_cli
#'
#' @return A data.table describing the uninstall result.
#' @export
# uninstall_cli {{{
uninstall_cli <- function(bin_dir = NULL, name = "epwshiftr") {
    checkmate::assert_string(bin_dir, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(name, min.chars = 1L)
    spec <- epwshiftr_cli_launcher_spec(bin_dir, name)
    if (!file.exists(spec$path)) {
        return(data.table::data.table(
            path = spec$path,
            bin_dir = spec$bin_dir,
            command = spec$command,
            removed = FALSE,
            message = "launcher not found"
        ))
    }
    content <- readLines(spec$path, warn = FALSE)
    if (
        !any(grepl("Generated by epwshiftr::install_cli\\(\\)", content)) ||
            !any(grepl(
                "epwshiftr::epwshiftr_cli\\(exit = TRUE\\)",
                content,
                fixed = FALSE
            ))
    ) {
        cli::cli_abort(
            "Refusing to remove {.path {spec$path}} because it was not generated by {.fn epwshiftr::install_cli}."
        )
    }
    removed <- unlink(spec$path, recursive = FALSE, force = TRUE) == 0L
    data.table::data.table(
        path = spec$path,
        bin_dir = spec$bin_dir,
        command = spec$command,
        removed = removed,
        message = if (isTRUE(removed)) {
            "removed launcher"
        } else {
            "failed to remove launcher"
        }
    )
}
# }}}

# epwshiftr_cli_launcher_spec {{{
epwshiftr_cli_launcher_spec <- function(bin_dir = NULL, name = "epwshiftr") {
    windows <- .Platform$OS.type == "windows"
    if (is.null(bin_dir)) {
        bin_dir <- if (isTRUE(windows)) {
            local_app <- Sys.getenv("LOCALAPPDATA", unset = NA_character_)
            if (is.na(local_app) || !nzchar(local_app)) {
                local_app <- file.path(path.expand("~"), "AppData", "Local")
            }
            file.path(local_app, "epwshiftr", "bin")
        } else {
            file.path(path.expand("~"), ".local", "bin")
        }
    }
    bin_dir <- path.expand(bin_dir)
    bin_dir <- normalizePath(
        bin_dir,
        mustWork = dir.exists(bin_dir),
        winslash = "/"
    )
    command <- name
    file_name <- name
    if (isTRUE(windows) && !grepl("[.]cmd$", file_name, ignore.case = TRUE)) {
        file_name <- paste0(file_name, ".cmd")
        command <- file_name
    }
    list(
        windows = windows,
        bin_dir = bin_dir,
        command = command,
        path = file.path(bin_dir, file_name)
    )
}
# }}}

# epwshiftr_cli_launcher_content {{{
epwshiftr_cli_launcher_content <- function(
    windows = .Platform$OS.type == "windows"
) {
    rscript <- file.path(
        R.home("bin"),
        if (isTRUE(windows)) "Rscript.exe" else "Rscript"
    )
    rscript <- normalizePath(
        rscript,
        mustWork = FALSE,
        winslash = if (isTRUE(windows)) "\\" else "/"
    )
    expr <- "epwshiftr::epwshiftr_cli(exit = TRUE)"
    if (isTRUE(windows)) {
        return(c(
            "@echo off",
            "rem Generated by epwshiftr::install_cli()",
            sprintf("\"%s\" --vanilla -e \"%s\" %%*", rscript, expr)
        ))
    }
    c(
        "#!/bin/sh",
        "# Generated by epwshiftr::install_cli()",
        sprintf(
            "exec %s --vanilla -e %s \"$@\"",
            shQuote(rscript),
            shQuote(expr)
        )
    )
}
# }}}

# epwshiftr_cli_bin_in_path {{{
epwshiftr_cli_bin_in_path <- function(bin_dir) {
    path <- Sys.getenv("PATH", unset = "")
    if (!nzchar(path)) {
        return(FALSE)
    }
    dirs <- strsplit(path, .Platform$path.sep, fixed = TRUE)[[1L]]
    dirs <- dirs[nzchar(dirs)]
    if (!length(dirs)) {
        return(FALSE)
    }
    normalized <- vapply(
        dirs,
        normalizePath,
        character(1L),
        mustWork = FALSE,
        winslash = "/"
    )
    normalizePath(bin_dir, mustWork = FALSE, winslash = "/") %in% normalized
}
# }}}

# epwshiftr_cli_help {{{
epwshiftr_cli_help <- function(topic = character()) {
    topic <- topic[nzchar(topic)]
    if (!length(topic)) {
        return(epwshiftr_cli_help_root())
    }
    key <- paste(topic, collapse = " ")
    registry <- epwshiftr_cli_help_registry()
    if (!key %in% names(registry)) {
        epwshiftr_cli_usage_abort(sprintf("Unknown help topic: %s", key))
    }
    registry[[key]]
}
# }}}

# epwshiftr_cli_help_root {{{
epwshiftr_cli_help_root <- function() {
    c(
        "Usage: epwshiftr [--store PATH] [--json|--jsonl] [--quiet] <group> <command> [options]",
        "",
        "Command groups:",
        "  epwshiftr doctor",
        "  epwshiftr query help",
        "  epwshiftr download help",
        "  epwshiftr esgf help",
        "  epwshiftr storage help",
        "  epwshiftr shift help",
        "  epwshiftr extract help",
        "  epwshiftr morph help",
        "",
        "Use `epwshiftr help <group> <command>` for command-specific help."
    )
}
# }}}

# epwshiftr_cli_help_registry {{{
epwshiftr_cli_help_registry <- function() {
    list(
        doctor = c(
            "Usage: epwshiftr doctor [--config PATH] [--network] [--index-node URL] [--timeout SECONDS] [--no-progress]",
            "",
            "Check the local CLI, store, downloader, and optional ESGF network environment.",
            "The default checks are local and read-only; use --network to check the index node.",
            "--config PATH also checks configured ERA5 access; credentials remain outside workflow JSON.",
            "Network checks show human progress; --no-progress, --quiet, --json and --jsonl suppress it."
        ),
        query = c(
            "Usage: epwshiftr query <command> [options]",
            "",
            "Commands:",
            "  epwshiftr query list",
            "  epwshiftr query search [--index-node URL] [--type Dataset|File|Aggregation] [--fields FIELDS] [--columns COLS] [--no-progress] [--dry-run] key=value|key!=value ...",
            "  epwshiftr query add [--index-node URL] [--label LABEL] [--tag TAG]... [--track] [--dry-run] key=value|key!=value ...",
            "  epwshiftr query add --query-file PATH [--label LABEL] [--tag TAG]... [--track] [--dry-run]",
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
            "  epwshiftr query update [query_id]"
        ),
        "query list" = c(
            "Usage: epwshiftr query list",
            "",
            "List stored ESGF queries in the selected store."
        ),
        "query search" = c(
            "Usage: epwshiftr query search [--index-node URL] [--type Dataset|File|Aggregation] [--fields FIELDS] [--columns COLS] [--limit N] [--all] [--no-progress] [--dry-run] key=value|key!=value ...",
            "",
            "Run an ESGF search from command-line key=value constraints.",
            "--fields controls fields requested from ESGF; --columns controls human-readable table columns.",
            "--no-progress disables interactive progress bars.",
            "Use comma-separated values for multi-value query parameters.",
            "Use key!=value to negate a facet constraint.",
            "Use datetime_start=VALUE and datetime_stop=VALUE to constrain data coverage."
        ),
        "query add" = c(
            "Usage: epwshiftr query add [--index-node URL] [--label LABEL] [--tag TAG]... [--track] [--dry-run] key=value|key!=value ...",
            "       epwshiftr query add --query-file PATH [--label LABEL] [--tag TAG]... [--track] [--dry-run]",
            "",
            "Create an EsgQuery from command-line key=value constraints or import an EsgQuery JSON file.",
            "Use key!=value to negate a facet constraint.",
            "Use datetime_start=VALUE and datetime_stop=VALUE to constrain data coverage.",
            "This command saves query definitions only; it does not update or download ESGF files."
        ),
        "query show" = c(
            "Usage: epwshiftr query show <query_id> [--files] [--updates] [--changes]",
            "",
            "Show stored query metadata and optional related records."
        ),
        "query status" = c(
            "Usage: epwshiftr query status [query_id]",
            "",
            "Summarise tracked query, file, and download status."
        ),
        "query files" = c(
            "Usage: epwshiftr query files <query_id> [--status STATUS]",
            "",
            "List files linked to a stored query."
        ),
        "query updates" = c(
            "Usage: epwshiftr query updates [query_id] [--latest]",
            "",
            "List stored update batches for tracked queries."
        ),
        "query changes" = c(
            "Usage: epwshiftr query changes [query_id] [--latest] [--update UPDATE_ID] [--type TYPE]",
            "",
            "List per-file changes recorded by query updates."
        ),
        "query tags" = c(
            "Usage: epwshiftr query tags [query_id]",
            "",
            "List query tags."
        ),
        "query track" = c(
            "Usage: epwshiftr query track <query_id>",
            "",
            "Mark a stored query as tracked."
        ),
        "query untrack" = c(
            "Usage: epwshiftr query untrack <query_id>",
            "",
            "Mark a stored query as untracked."
        ),
        "query tag" = c(
            "Usage: epwshiftr query tag <query_id> <tag>... [--replace]",
            "",
            "Add tags to a stored query."
        ),
        "query untag" = c(
            "Usage: epwshiftr query untag <query_id> [tag...]",
            "",
            "Remove tags from a stored query."
        ),
        "query remove" = c(
            "Usage: epwshiftr query remove <query_id> [--delete none|orphaned] [--execute]",
            "",
            "Preview or remove a stored query."
        ),
        "query preview" = c(
            "Usage: epwshiftr query preview [query_id] [--detail]",
            "",
            "Preview query update changes without writing update records."
        ),
        "query update" = c(
            "Usage: epwshiftr query update [query_id]",
            "",
            "Refresh tracked query records and persist the update diff."
        ),

        shift = c(
            "Usage: epwshiftr shift <command> [options]",
            "",
            "Commands:",
            "  epwshiftr shift list [--type all|run|batch] [--status STATE,STATE] [--limit N]",
            "  epwshiftr shift summary (--run RUN_ID | --batch BATCH_ID) [--weather] [--no-progress]",
            "  epwshiftr shift run --config PATH [--dry-run | --background] [--no-progress] [--reduced-motion] [--verbose | --debug]",
            "  epwshiftr shift show (--run RUN_ID | --batch BATCH_ID) [--verbose | --debug]",
            "  epwshiftr shift config example [--output PATH] [--overwrite]",
            "  epwshiftr shift config validate --config PATH [--network]",
            "  epwshiftr shift watch (--run RUN_ID | --batch BATCH_ID) [--follow] [--interval SECONDS] [--count N] [--events N] [--no-progress] [--reduced-motion] [--verbose | --debug]",
            "  epwshiftr shift cancel (--run RUN_ID | --batch BATCH_ID) [--force]",
            "  epwshiftr shift logs (--run RUN_ID | --batch BATCH_ID) [--tail N]",
            "  epwshiftr shift status (--run RUN_ID | --batch BATCH_ID)",
            "  epwshiftr shift diagnostics (--run RUN_ID | --batch BATCH_ID)",
            "  epwshiftr shift outputs (--run RUN_ID | --batch BATCH_ID)",
            "  epwshiftr shift data (--run RUN_ID | --batch BATCH_ID) [--case CASE_ID] [--columns COLS] [--limit N]",
            "  epwshiftr shift resume (--run RUN_ID | --batch BATCH_ID) [--background] [--no-progress] [--reduced-motion] [--verbose | --debug]"
        ),
        "shift run" = c(
            "Usage: epwshiftr shift run --config PATH [--dry-run | --background] [--no-progress] [--reduced-motion] [--verbose | --debug]",
            "",
            "Run a JSON-configured request -> collect -> optional download -> extract -> morph -> EPW workflow.",
            "The config is validated against inst/extdata/schema/shift-workflow-config.json.",
            "Use methods or an explicit transform array for method/model batches; calibration accepts an ERA5 specification.",
            "climate.model accepts names, a positive model count, or null (all); null frequency lets each method select its required resolution.",
            "--dry-run may query model coverage and saves reopenable batch plans. Completed receipts are reused unless control.refresh is true.",
            "Returned failed, blocked, partial, or cancelled results use exit status 1; invalid command/config syntax uses 2.",
            "--reduced-motion replaces animated frames with a stable active marker.",
            "--verbose shows selection, reuse, and fallback details; --debug also shows full URLs and paths."
        ),
        "shift show" = c(
            "Usage: epwshiftr shift show (--run RUN_ID | --batch BATCH_ID) [--verbose | --debug]",
            "",
            "Show persisted intent, case state, events, diagnostics, and outputs for a workflow run.",
            "Batch --verbose includes every child, live activity, full diagnostics and recovery actions."
        ),
        "shift list" = c(
            "Usage: epwshiftr shift list [--type all|run|batch] [--status STATE,STATE] [--limit N]",
            "",
            "List saved runs, batch parents, and child runs without network access or store creation.",
            "Rows retain their full IDs and exact store paths. The default limit is 20.",
            "Unreadable records have status unavailable and an inspection error."
        ),
        "shift summary" = c(
            "Usage: epwshiftr shift summary (--run RUN_ID | --batch BATCH_ID) [--weather] [--no-progress]",
            "",
            "Summarize method/model/scenario/period groups with separate case and EPW file counts.",
            "--weather reads existing local EPWs for hourly means and valid-hour counts; missing codes are excluded.",
            "Weather summaries preserve output type and years; they do not rank methods or establish comparability."
        ),
        "shift config" = c(
            "Usage: epwshiftr shift config <example|validate> [options]",
            "",
            "Generate or validate JSON workflow configuration files."
        ),
        "shift config example" = c(
            "Usage: epwshiftr shift config example [--output PATH] [--overwrite]",
            "",
            "Print or write an example JSON workflow configuration.",
            "Use --methods NAME,NAME for a batch, or --scale SCALE --method NAME [--reconstruction NAME] [--option KEY=VALUE]... for one transform.",
            "--model accepts a positive count, comma-separated model names, or all. Required observed inputs add an ERA5 calibration example.",
            "For ambiguous methods such as epwshiftr, use explicit transform objects; multiple objects form a JSON transform array."
        ),
        "shift config validate" = c(
            "Usage: epwshiftr shift config validate --config PATH [--network] [--no-progress] [--reduced-motion]",
            "",
            "Validate schema, method options, local EPW, and reference roles without contacting providers.",
            "--network also checks common CMIP6 model coverage and configured reanalysis readiness.",
            "Human checks show current activity; --no-progress, --quiet, --json and --jsonl suppress it."
        ),
        "shift watch" = c(
            "Usage: epwshiftr shift watch (--run RUN_ID | --batch BATCH_ID) [--follow] [--interval SECONDS] [--count N] [--events N] [--no-progress] [--reduced-motion] [--verbose | --debug]",
            "",
            "Return a workflow activity snapshot with query, download, extraction, morphing, output, diagnostic, and event state.",
            "Use --follow for a continuously refreshed view; combine with global --jsonl for machine-readable streaming.",
            "Dynamic terminals show a stage rail, measured progress, the current unit, live resolver state, and recent milestones.",
            "Use --reduced-motion to keep the dashboard with a stable active marker."
        ),
        "shift cancel" = c(
            "Usage: epwshiftr shift cancel (--run RUN_ID | --batch BATCH_ID) [--force]",
            "",
            "Request cancellation at the next safe workflow boundary. Use --force to terminate the recorded worker process immediately."
        ),
        "shift logs" = c(
            "Usage: epwshiftr shift logs (--run RUN_ID | --batch BATCH_ID) [--tail N]",
            "",
            "Return the stdout/stderr tail from the latest workflow job attempt."
        ),
        "shift status" = c(
            "Usage: epwshiftr shift status (--run RUN_ID | --batch BATCH_ID)",
            "",
            "Return the persisted workflow run row."
        ),
        "shift diagnostics" = c(
            "Usage: epwshiftr shift diagnostics (--run RUN_ID | --batch BATCH_ID)",
            "",
            "Return workflow diagnostics reconstructed from store manifest state."
        ),
        "shift outputs" = c(
            "Usage: epwshiftr shift outputs (--run RUN_ID | --batch BATCH_ID)",
            "",
            "List EPW outputs recorded for a morphing plan."
        ),
        "shift data" = c(
            "Usage: epwshiftr shift data (--run RUN_ID | --batch BATCH_ID) [--case CASE_ID] [--columns COLS] [--limit N]",
            "",
            "Preview final EPW data for a persisted run."
        ),
        "shift resume" = c(
            "Usage: epwshiftr shift resume (--run RUN_ID | --batch BATCH_ID) [--background] [--no-progress] [--reduced-motion] [--verbose | --debug]",
            "",
            "Resume a failed or interrupted run using its pinned resolved inputs."
        ),
        extract = c(
            "Usage: epwshiftr extract <command> [options]",
            "",
            "Commands:",
            "  epwshiftr extract plan --query QUERY_ID --site-id ID --lon LON --lat LAT --time START,STOP [--variable VARS] [--method METHOD] [--filter key=value]...",
            "  epwshiftr extract run --plan PLAN_ID[,PLAN_ID...] [--fallback auto|error] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "  epwshiftr extract retry [--plan PLAN_ID[,PLAN_ID...]] [--status failed] [--run] [--fallback auto|error] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "  epwshiftr extract coverage [--plan PLAN_ID]",
            "  epwshiftr extract artifacts --plan PLAN_ID"
        ),
        "extract plan" = c(
            "Usage: epwshiftr extract plan --query QUERY_ID --site-id ID --lon LON --lat LAT --time START,STOP [--variable VARS] [--method METHOD] [--filter key=value]...",
            "",
            "Create extraction plan rows for a stored query and target site."
        ),
        "extract run" = c(
            "Usage: epwshiftr extract run --plan PLAN_ID[,PLAN_ID...] [--fallback auto|error] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "",
            "Run selected extraction plans and write derived Parquet artifacts."
        ),
        "extract retry" = c(
            "Usage: epwshiftr extract retry [--plan PLAN_ID[,PLAN_ID...]] [--status failed] [--run] [--fallback auto|error] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "",
            "Preview or rerun extraction plans with retryable statuses."
        ),
        "extract coverage" = c(
            "Usage: epwshiftr extract coverage [--plan PLAN_ID]",
            "",
            "Inspect extraction coverage and completeness."
        ),
        "extract artifacts" = c(
            "Usage: epwshiftr extract artifacts --plan PLAN_ID",
            "",
            "List store artifact rows created by extraction results."
        ),
        morph = c(
            "Usage: epwshiftr morph <command> [options]",
            "",
            "Commands:",
            "  epwshiftr morph describe [--scale monthly|daily|hourly] [--method NAME] [--reconstruction NAME] [--option KEY=VALUE]...",
            "  epwshiftr morph variables [--scale monthly|daily|hourly] [--method NAME] [--reconstruction NAME] [--option KEY=VALUE]...",
            "  epwshiftr morph transforms [--scale SCALE] [--method NAME] [--status production|experimental]",
            "  epwshiftr morph run --plan PLAN_ID[,PLAN_ID...] --epw PATH --period PERIOD=YEARS[,YEARS]... [--scale monthly|daily|hourly] [--method NAME] [--reconstruction NAME] [--option KEY=VALUE]... [--reference historical|plan] [--reference-plan PLAN_ID[,PLAN_ID...]] [--reference-period PERIOD=YEARS[,YEARS]...] [--reference-filter KEY=VALUE] [--reference-option KEY=VALUE] [--observed-plan PLAN_ID[,PLAN_ID...]] [--observed-period PERIOD=YEARS[,YEARS]...] [--strict true|false] [--by COLS] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "  epwshiftr morph epw --morph MORPH_ID [--dir DIR] [--separate true|false] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "  epwshiftr morph retry [--morph MORPH_ID[,MORPH_ID...]] [--status failed] [--run] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "  epwshiftr morph status [--morph MORPH_ID]",
            "  epwshiftr morph outputs [--morph MORPH_ID]"
        ),
        "morph describe" = c(
            "Usage: epwshiftr morph describe [--scale monthly|daily|hourly] [--method NAME] [--reconstruction NAME] [--option KEY=VALUE]...",
            "",
            "Inspect input roles, reconstruction choices, defaults, evidence, field roles, and output type offline.",
            "Defaults to monthly original_morphing. Candidate options are checked by the actual method constructor.",
            "Use repeated --option KEY=VALUE arguments; quote JSON arrays, for example --option tas.bounds=[-40,60].",
            "--json returns the complete machine-readable contract. No store is opened."
        ),
        "morph variables" = c(
            "Usage: epwshiftr morph variables [--scale monthly|daily|hourly] [--method NAME] [--reconstruction NAME] [--option KEY=VALUE]...",
            "",
            "List CMIP source variables participating in a weather transformation's required alternatives."
        ),
        "morph transforms" = c(
            "Usage: epwshiftr morph transforms [--scale SCALE] [--method NAME] [--status production|experimental]",
            "",
            "List built-in transformations with input contracts, evidence, status, and output type."
        ),
        "morph run" = c(
            "Usage: epwshiftr morph run --plan PLAN_ID[,PLAN_ID...] --epw PATH --period PERIOD=YEARS[,YEARS]... [--scale monthly|daily|hourly] [--method NAME] [--reconstruction NAME] [--option KEY=VALUE]... [--reference historical|plan] [--reference-plan PLAN_ID[,PLAN_ID...]] [--reference-period PERIOD=YEARS[,YEARS]...] [--reference-filter KEY=VALUE] [--reference-option KEY=VALUE] [--observed-plan PLAN_ID[,PLAN_ID...]] [--observed-period PERIOD=YEARS[,YEARS]...] [--strict true|false] [--by COLS] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "",
            "Run a selected weather transformation through hourly Parquet outputs without writing EPW files.",
            "The default is --scale monthly --method original_morphing; repeated --option key=value values configure declared scientific settings.",
            "For multivariable methods, use --option variable.setting=value; JSON arrays can supply vector settings.",
            "Use --reference historical to resolve matching CMIP historical data from the plan's collected File metadata."
        ),
        "morph epw" = c(
            "Usage: epwshiftr morph epw --morph MORPH_ID [--dir DIR] [--separate true|false] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "",
            "Write future EPW files for an existing morphing plan."
        ),
        "morph retry" = c(
            "Usage: epwshiftr morph retry [--morph MORPH_ID[,MORPH_ID...]] [--status failed] [--run] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "",
            "Preview or rerun existing morphing plans without replanning climate summaries or writing EPW files."
        ),
        "morph status" = c(
            "Usage: epwshiftr morph status [--morph MORPH_ID]",
            "",
            "List morphing plan status rows."
        ),
        "morph outputs" = c(
            "Usage: epwshiftr morph outputs [--morph MORPH_ID]",
            "",
            "List EPW output rows recorded for morphing plans."
        ),
        download = c(
            "Usage: epwshiftr download <command> [options]",
            "",
            "Commands:",
            "  epwshiftr download preflight <query_id> [--replica POLICY] [--strategy STRATEGY] [--no-probe]",
            "  epwshiftr download run <query_id> [--session-label LABEL] [--background] [--mode process|daemon] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "  epwshiftr download status [--query QUERY_ID] [--session SESSION_ID]",
            "  epwshiftr download sessions",
            "  epwshiftr download tasks [--session SESSION_ID] [--job JOB_ID] [--status STATUS]",
            "  epwshiftr download events [--session SESSION_ID] [--job JOB_ID] [--task TASK_ID]",
            "  epwshiftr download watch [--query QUERY_ID] [--session SESSION_ID] [--job JOB_ID] [--follow] [--interval SECONDS] [--events N]",
            "  epwshiftr download jobs [--status STATUS]",
            "  epwshiftr download stop --job JOB_ID [--force]",
            "  epwshiftr download logs [--session SESSION_ID] [--job JOB_ID] [--task TASK_ID] [--tail N]",
            "  epwshiftr download daemon start|status|stop",
            "  epwshiftr download resume [--session SESSION_ID] [--task TASK_ID] [--overwrite] [--no-progress]",
            "  epwshiftr download verify [--session SESSION_ID] [--task TASK_ID]",
            "  epwshiftr download cancel [--session SESSION_ID] [--task TASK_ID]",
            "  epwshiftr download nodes [--service HTTPServer]",
            "  epwshiftr download reset-nodes [--node HOST] [--service HTTPServer] [--execute]",
            "  epwshiftr download retry [--query QUERY_ID] [--session SESSION_ID] [--status STATUS] [--run]",
            "  epwshiftr download config show",
            "  epwshiftr download config set [--workers N] [--timeout SECONDS] [--bandwidth-limit BYTES|none]"
        ),
        "download preflight" = c(
            "Usage: epwshiftr download preflight <query_id> [--replica POLICY] [--service SERVICE] [--strategy STRATEGY] [--probe|--no-probe]",
            "",
            "Build a replica-aware download plan without enqueuing tasks."
        ),
        "download run" = c(
            "Usage: epwshiftr download run <query_id> [--session-label LABEL] [--background] [--mode process|daemon] [--overwrite] [--no-resume] [--no-progress] [--reduced-motion] [--verbose|--debug]",
            "",
            "Refresh, plan, enqueue, and run downloads for a stored query.",
            "Use --background to create a persistent downloader job instead of blocking the current R session."
        ),
        "download status" = c(
            "Usage: epwshiftr download status [--query QUERY_ID] [--session SESSION_ID]",
            "",
            "List downloader tasks linked to stored query files."
        ),
        "download sessions" = c(
            "Usage: epwshiftr download sessions",
            "",
            "List persistent download sessions."
        ),
        "download tasks" = c(
            "Usage: epwshiftr download tasks [--session SESSION_ID] [--job JOB_ID] [--status STATUS]",
            "",
            "List persistent download tasks."
        ),
        "download events" = c(
            "Usage: epwshiftr download events [--session SESSION_ID] [--job JOB_ID] [--task TASK_ID]",
            "",
            "List persistent downloader event records."
        ),
        "download watch" = c(
            "Usage: epwshiftr download watch [--query QUERY_ID] [--session SESSION_ID] [--job JOB_ID] [--follow] [--interval SECONDS] [--events N]",
            "",
            "Return a one-shot download activity snapshot with summary, tasks, nodes, and recent events.",
            "Use --follow for a continuously refreshed view; combine with global --jsonl for machine-readable streaming."
        ),
        "download jobs" = c(
            "Usage: epwshiftr download jobs [--status STATUS]",
            "",
            "List background downloader jobs."
        ),
        "download stop" = c(
            "Usage: epwshiftr download stop --job JOB_ID [--force]",
            "",
            "Request cancellation of a background downloader job."
        ),
        "download logs" = c(
            "Usage: epwshiftr download logs [--session SESSION_ID] [--job JOB_ID] [--task TASK_ID] [--tail N]",
            "",
            "Return the latest persistent downloader event records or background job log lines."
        ),
        "download daemon" = c(
            "Usage: epwshiftr download daemon <start|status|stop> [options]",
            "",
            "Manage the persistent downloader daemon."
        ),
        "download daemon start" = c(
            "Usage: epwshiftr download daemon start [--port PORT] [--heartbeat-interval SECONDS]",
            "",
            "Start a persistent downloader daemon for queued background jobs."
        ),
        "download daemon status" = c(
            "Usage: epwshiftr download daemon status",
            "",
            "Show downloader daemon status records."
        ),
        "download daemon stop" = c(
            "Usage: epwshiftr download daemon stop [--force]",
            "",
            "Request the active downloader daemon to stop."
        ),
        "download resume" = c(
            "Usage: epwshiftr download resume [--session SESSION_ID] [--task TASK_ID] [--overwrite] [--no-progress]",
            "",
            "Resume queued or interrupted persistent download tasks."
        ),
        "download verify" = c(
            "Usage: epwshiftr download verify [--session SESSION_ID] [--task TASK_ID]",
            "",
            "Verify checksums for completed persistent download tasks."
        ),
        "download cancel" = c(
            "Usage: epwshiftr download cancel [--session SESSION_ID] [--task TASK_ID]",
            "",
            "Cancel queued or in-progress persistent download tasks."
        ),
        "download nodes" = c(
            "Usage: epwshiftr download nodes [--service HTTPServer]",
            "",
            "List persistent data-node performance records."
        ),
        "download reset-nodes" = c(
            "Usage: epwshiftr download reset-nodes [--node HOST] [--service HTTPServer] [--execute]",
            "",
            "Preview or reset data-node performance records."
        ),
        "download retry" = c(
            "Usage: epwshiftr download retry [--query QUERY_ID] [--session SESSION_ID] [--status STATUS] [--run]",
            "",
            "Requeue failed or cancelled persistent tasks."
        ),
        "download config" = c(
            "Usage: epwshiftr download config <show|set> [options]",
            "",
            "Show or update persistent downloader configuration."
        ),
        "download config show" = c(
            "Usage: epwshiftr download config show",
            "",
            "Show persistent downloader configuration."
        ),
        "download config set" = c(
            "Usage: epwshiftr download config set [--workers N] [--retries N] [--timeout SECONDS] [--bandwidth-limit BYTES|none]",
            "",
            "Update persistent downloader configuration."
        ),
        esgf = c(
            "Usage: epwshiftr esgf <command> [options]",
            "",
            "Commands:",
            "  epwshiftr esgf report [--query QUERY_ID]"
        ),
        "esgf report" = c(
            "Usage: epwshiftr esgf report [--query QUERY_ID]",
            "",
            "Return a compact ESGF query and download health report."
        ),
        storage = c(
            "Usage: epwshiftr storage <command> [options]",
            "",
            "Commands:",
            "  epwshiftr storage layout show",
            "  epwshiftr storage layout set --layout flat|dataset|drs|template [--template TEMPLATE]",
            "  epwshiftr storage report [--detail]",
            "  epwshiftr storage validate [--query QUERY_ID] [--checksum]",
            "  epwshiftr storage repair [--execute]",
            "  epwshiftr storage cleanup [--scope SCOPE] [--older-than SECONDS] [--execute]"
        ),
        "storage layout" = c(
            "Usage: epwshiftr storage layout <show|set> [options]",
            "",
            "Show or update the download file layout policy."
        ),
        "storage layout show" = c(
            "Usage: epwshiftr storage layout show",
            "",
            "Show the current download file layout policy."
        ),
        "storage layout set" = c(
            "Usage: epwshiftr storage layout set --layout flat|dataset|drs|template [--template TEMPLATE]",
            "",
            "Update the download file layout policy."
        ),
        "storage report" = c(
            "Usage: epwshiftr storage report [--detail]",
            "",
            "Summarise download storage and cleanup candidates."
        ),
        "storage validate" = c(
            "Usage: epwshiftr storage validate [--query QUERY_ID] [--checksum]",
            "",
            "Validate store-managed NetCDF downloads against the manifest."
        ),
        "storage repair" = c(
            "Usage: epwshiftr storage repair [--execute]",
            "",
            "Preview or execute safe repairs for download file records."
        ),
        "storage cleanup" = c(
            "Usage: epwshiftr storage cleanup [--scope SCOPE] [--older-than SECONDS] [--execute]",
            "",
            "Preview or clean temporary files and safe orphaned records."
        )
    )
}
# }}}

# vim: fdm=marker :
