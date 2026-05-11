lpad <- function(x, pad = " ", width = NULL) {
    wid <- nchar(x, "width")
    if (is.null(width)) {
        width <- max(wid)
    }
    paste0(strrep(pad, pmax(width - wid, 0)), x)
}

# a little bit faster
unlst <- function(x) unlist(x, FALSE, FALSE)

trim_ws <- function(x) sub("\\s*$", "", sub("^\\s*", "", x))

to_title_case <- function(x) {
    sub("(.)", "\\U\\1", gsub("_", " ", x, fixed = TRUE), perl = TRUE)
}

priv <- function(x) {
    checkmate::assert_r6(x)
    x$.__enclos_env__[["private"]]
}

`priv<-` <- function(x, value) {
    checkmate::assert_r6(x)
    x$.__enclos_env__[["private"]] <- value
    invisible(x)
}

vmsg <- function(..., sep = "") {
    if (getOption("epwshiftr.verbose", FALSE)) {
        message(paste(..., sep = "\n"))
    }
}

verbose <- function(expr) {
    if (!getOption("epwshiftr.verbose", FALSE)) {
        return()
    }
    force(expr)
}

with_silent <- function(expr) {
    old <- options("epwshiftr.verbose" = FALSE)
    on.exit(options(old), add = TRUE)
    force(expr)
}

with_timeout <- function(secs = 300, expr) {
    old <- options(timeout = secs)
    on.exit(options(old), add = TRUE)
    force(expr)
}

# Convert `checkmate::check_*()` results into the return contract expected by
# S7 property/class validators: `NULL` on success, a single message on failure.
checkmate_result <- function(result, label = NULL) {
    if (isTRUE(result)) {
        return(NULL)
    }

    if (!is.null(label)) {
        return(sprintf("`%s` validation failed: %s", label, result))
    }

    result
}

checkmate_validator <- function(check, ..., label = NULL) {
    checkmate::assert_function(check)

    args <- list(...)
    force(label)

    function(value) {
        checkmate_result(do.call(check, c(list(value), args)), label = label)
    }
}

checkmate_rule <- function(class, check, ..., label = NULL, branch = NULL) {
    checkmate::assert_function(check)
    checkmate::assert_string(label, null.ok = TRUE)
    checkmate::assert_string(branch, null.ok = TRUE)

    if (!isS4(class)) {
        checkmate::assert_multi_class(
            class,
            c("S7_class", "S7_base_class", "S7_S3_class", "S7_missing", "S7_any"),
            null.ok = TRUE
        )
    }

    structure(
        list(
            class = class,
            check = check,
            args = list(...),
            label = label,
            branch = branch
        ),
        class = "CheckmateRule"
    )
}

checkmate_any <- function(...) {
    rules <- list(...)
    checkmate::assert_list(rules, "CheckmateRule", any.missing = FALSE, min.len = 1L, null.ok = FALSE)

    structure(
        list(
            mode = "any",
            rules = rules,
            class = Reduce(`|`, lapply(rules, `[[`, "class"))
        ),
        class = c("CheckmateSpecAny", "CheckmateSpec")
    )
}

checkmate_match_rule <- function(value, rules) {
    for (i in seq_along(rules)) {
        rule_class <- rules[[i]]$class

        if (is.null(rule_class)) {
            if (is.null(value)) {
                return(i)
            }
        } else if (inherits(value, rule_class)) {
            return(i)
        }
    }

    NA_integer_
}

checkmate_validate_rule <- function(value, rule) {
    msg <- checkmate_result(
        do.call(rule$check, c(list(value), rule$args)),
        label = rule$label
    )

    if (!is.null(msg) && !is.null(rule$branch)) {
        msg <- sprintf("[%s] %s", rule$branch, msg)
    }

    msg
}

checkmate_property <- function(
    class = S7::class_any,
    check,
    ...,
    getter = NULL,
    setter = NULL,
    default = NULL,
    name = NULL,
    label = NULL
) {
    if (inherits(class, "CheckmateSpec")) {
        spec <- class
        extra_args <- list(...)

        if (!missing(check)) {
            stop("When `class` is a `CheckmateSpec`, `check` must be omitted.", call. = FALSE)
        }
        if (length(extra_args)) {
            stop(
                "When `class` is a `CheckmateSpec`, checker arguments must be supplied in `checkmate_rule()`.",
                call. = FALSE
            )
        }
        if (!is.null(label)) {
            stop("When `class` is a `CheckmateSpec`, `label` must be supplied in `checkmate_rule()`.", call. = FALSE)
        }

        return(S7::new_property(
            class = spec$class,
            getter = getter,
            setter = setter,
            validator = function(value) {
                idx <- checkmate_match_rule(value, spec$rules)

                if (is.na(idx)) {
                    return("No matching validation branch found.")
                }

                checkmate_validate_rule(value, spec$rules[[idx]])
            },
            default = default,
            name = name
        ))
    }

    if (missing(check)) {
        stop("`check` must be supplied unless `class` is a `CheckmateSpec`.")
    }

    S7::new_property(
        class = class,
        getter = getter,
        setter = setter,
        validator = checkmate_validator(check, ..., label = label),
        default = default,
        name = name
    )
}

fast_hash <- function(x) {
    # FNV-1a hash algorithm
    FNV_PRIME <- 16777619 # 0x01000193
    # have to use double here since integer overflow
    FNV_OFFSET <- 2166136261 # 0x811c9dc5

    # Convert to bytes
    if (is.character(x) && length(x) == 1L) {
        bytes <- as.integer(charToRaw(x))
    } else {
        bytes <- as.integer(serialize(x, connection = NULL, ascii = FALSE))
    }

    # FNV-1a algorithm
    hash <- FNV_OFFSET
    for (byte in bytes) {
        # Split into 16-bit parts for XOR operation
        hash_hi <- hash %/% 65536
        hash_lo <- hash %% 65536

        # XOR with byte (only affects low 16 bits since byte < 256)
        hash_lo <- bitwXor(as.integer(hash_lo), byte)

        # Rebuild and multiply by FNV_PRIME
        hash <- (hash_hi * 65536 + hash_lo) * FNV_PRIME
        hash <- hash %% (2^32)
    }

    # Format as 8-character hex string
    if (hash <= .Machine$integer.max) {
        sprintf("%08x", as.integer(hash))
    } else {
        hash_hi <- as.integer(hash %/% 65536)
        hash_lo <- as.integer(hash %% 65536)
        sprintf("%04x%04x", hash_hi, hash_lo)
    }
}

eval_with_bang <- function(..., .env = parent.frame()) {
    dots <- eval(substitute(alist(...)))

    if (length(dots) == 0L) {
        stop("At least one argument is required.")
    }
    checkmate::assert_list(dots, .var.name = "Input", min.len = 1L)

    lapply(dots, function(expr) {
        negate <- !is.symbol(expr) && !is.null(expr) && is.call(expr) && as.character(expr[[1L]]) %in% c("!", "-")
        if (negate) {
            expr[[1L]] <- as.name("c")
        }

        list(value = eval(expr, .env), negate = negate)
    })
}

now <- function(tz = "UTC") {
    t <- Sys.time()
    attr(t, "tzone") <- tz
    t
}

# nocov start
rd_query_is_facets <- function(x) {
    x %in% c("facets", "fields", "shards", "replica", "latest", "type", "limit", "offset", "distrib")
}

rd_query_method_param <- function(method, type, negate, default, nullable = TRUE) {
    val_quote <- if (grepl("character|string", type)) '"' else ""
    def_quote <- if (!missing(default) && is.null(default)) "" else val_quote
    rd <- c(
        paste(
            paste("The", if (rd_query_is_facets(method)) "facet" else "", "parameter value."),
            if (!missing(default)) {
                sprintf("Default: \\code{%s%s%s}.", def_quote, if (is.null(default)) "NULL" else default, def_quote)
            }
        ),

        "There are two options:",
        "\\itemize{",
        "\\item If \\code{value} is not given, current value is returned.",
        paste(
            sprintf(
                "\\item %s %s%s.",
                if (type == "integer") "An" else "A",
                type,
                if (nullable) " or \\code{NULL}" else ""
            ),
            if (method %in% c("fields", "facets")) {
                sprintf(
                    paste(
                        "The special notation \\code{\"*\"} can be used to indicate that",
                        "all available %s should be considered."
                    ),
                    method
                )
            },
            if (!missing(negate)) {
                sprintf(
                    paste(
                        "Note that you can put a preceding \\code{!} or \\code{-} to negate the facet constraints.",
                        "For example, \\code{$%s(!c(%s))} and \\code{$%s(-c(%s))} search for all \\code{%s}s except for",
                        "%s."
                    ),
                    method,
                    paste0(val_quote, negate, val_quote, collapse = ", "),
                    method,
                    paste0(val_quote, negate, val_quote, collapse = ", "),
                    method,
                    paste0("\\code{", negate, "}", collapse = " and ")
                )
            }
        ),
        "}"
    )

    paste(rd, collapse = "\n")
}

rd_query_method_return <- function() {
    paste(
        "\\itemize{",
        "\\item If \\code{value} is given, the modified \\code{EsgQuery} object.",
        paste(
            "\\item Otherwise, a \\code{QueryParam} object:",
            "\\itemize{",
            "\\item Use \\code{query_param_value()} to read the stored value.",
            "\\item Use \\code{query_param_negate()} to read whether the facet is negated.",
            "\\item Use \\code{query_param_name()} and \\code{query_param_kind()} to inspect metadata.",
            "}"
        ),
        "}"
    )
}
# nocov end

set_size_units <- function(x) {
    if (!length(x)) {
        return(NULL)
    }
    base <- 1024L
    iec <- c("Byte", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
    if (!inherits(x, "units")) {
        x <- units::set_units(x, iec[[1L]], mode = "standard")
    } else {
        bytes <- try(units::set_units(x, iec[[1L]], mode = "standard"), silent = TRUE)
        if (inherits(bytes, "try-error")) {
            warning("Failed to set input units to 'Byte'. Conversion skipped.")
            return(x)
        }
        x <- bytes
    }

    power <- log(units::drop_units(x), base = base)
    power[is.infinite(power)] <- 0
    power <- min(as.integer(power), length(iec))
    units::set_units(x, iec[power + 1L], mode = "standard")
}

#' Get the package data storage directory
#'
#' If option `epwshiftr.dir` is set, use it. Otherwise, get package data storage
#' directory using [rappdirs::user_data_dir()].
#'
#' @param init If TRUE, the directory will be created if not exists.
#' @param force If TRUE, issue an error if the directory does not exist.
#'
#' @return A single string indicating the directory location.
#'
#' @noRd
.data_dir <- function(init = FALSE, force = TRUE) {
    checkmate::assert_flag(init)
    checkmate::assert_flag(force)

    d <- getOption("epwshiftr.dir", NULL)
    if (is.null(d)) {
        # nocov start
        if (.Platform$OS.type == "windows") {
            d <- normalizePath(rappdirs::user_data_dir(appauthor = "epwshiftr"), mustWork = FALSE)
        } else {
            d <- normalizePath(rappdirs::user_data_dir(appname = "epwshiftr"), mustWork = FALSE)
        }
        # nocov end

        if (init && !dir.exists(d)) {
            verbose(sprintf("Creating %s package data storage directory '%s'", "epwshiftr", d))
            dir.create(d, recursive = TRUE)
        }
    } else {
        # make sure user specified directory exists
        d <- normalizePath(d, mustWork = FALSE)
        init <- FALSE
        force <- TRUE
    }

    if ((init || force) && !checkmate::test_directory_exists(d, "rw")) {
        stop(sprintf("%s package data storage directory '%s' does not exists or is not writable.", "epwshiftr", d))
    }

    d
}

# get rid of R CMD check NOTEs on global variables
utils::globalVariables(c(
    ".BY",
    ".I",
    ".N",
    ".SD",
    "J",
    ".GRP",
    "activity_drs",
    "alpha",
    "attribute",
    "country",
    "data_node",
    "datetime",
    "datetime_end",
    "datetime_start",
    "day_of_year",
    "degree_Celsius",
    "delta",
    "dew_point_temperature",
    "diffuse_horizontal_radiation",
    "direct_normal_radiation",
    "dl_percent",
    "dry_bulb_temperature",
    "epw_max",
    "epw_mean",
    "epw_min",
    "experiment_id",
    "file_id",
    "file_path",
    "file_realsize",
    "file_size",
    "global_horizontal_radiation",
    "horizontal_infrared_radiation_intensity_from_sky",
    "hour",
    "i.datetime_end",
    "i.datetime_start",
    "i.diffuse_horizontal_radiation",
    "i.opaque_sky_cover",
    "i.relative_humidity",
    "i.val_max",
    "i.val_mean",
    "i.val_min",
    "i.value",
    "id",
    "index",
    "index_case",
    "institution_id",
    "lat",
    "latitude",
    "location",
    "lon",
    "longitude",
    "member_id",
    "name",
    "natts",
    "opaque_sky_cover",
    "ping",
    "relative_humidity",
    "source_id",
    "source_type",
    "state_province",
    "table_id",
    "title",
    "total_sky_cover",
    "val_max",
    "val_mean",
    "val_min",
    "value",
    "value_max",
    "value_min",
    "variable",
    "wmo_number",
    "file_mtime",
    "i.file_path",
    "i.interval",
    "interval",
    "time_calendar",
    "time_units",
    "overlap",
    "frequency",
    "ind_lon",
    "ind_lat",
    "ord_lon",
    "ord_lat",
    "dist",
    "num_years",
    "type",
    "year",
    "status"
))
