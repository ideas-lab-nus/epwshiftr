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


epwshiftr_cli_count_or_default <- function(value, name, default, positive = TRUE) {
    if (is.null(value)) {
        return(default)
    }
    epwshiftr_cli_count(value, name, positive = positive)
}


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


epwshiftr_cli_string_or_null <- function(value) {
    normalized <- tolower(trimws(value))
    if (!nzchar(normalized) || normalized %in% c("none", "null", "na")) {
        return(NULL)
    }
    value
}
epwshiftr_cli_count_or_null <- function(value, name, positive = TRUE) {
    if (is.null(epwshiftr_cli_string_or_null(value))) {
        return(NULL)
    }
    epwshiftr_cli_count(value, name, positive = positive)
}


epwshiftr_cli_empty_to_null <- function(value) {
    if (is.null(value) || !length(value)) {
        return(NULL)
    }
    value
}
