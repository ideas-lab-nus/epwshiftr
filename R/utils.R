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

# store_is_abs_path {{{
store_is_abs_path <- function(path) {
    grepl("^(/|~|[A-Za-z]:[\\\\/]|\\\\\\\\)", path)
}
# }}}

# store_normalize_path {{{
store_normalize_path <- function(path) {
    path <- path.expand(path)
    if (file.exists(path)) {
        return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }

    parent <- dirname(path)
    if (dir.exists(parent)) {
        return(file.path(normalizePath(parent, winslash = "/", mustWork = TRUE), basename(path)))
    }

    normalizePath(path, winslash = "/", mustWork = FALSE)
}
# }}}

#' Get the epwshiftr store directory
#'
#' `store_dir()` returns the root directory used for persistent epwshiftr store
#' artifacts, including query snapshots, dictionaries, sources, downloads,
#' extracted data, generated outputs, and the DuckDB manifest.
#'
#' @param init If `TRUE`, create the directory when it does not exist.
#'
#' @return A single string indicating the directory location.
#'
#' @export
store_dir <- function(init = TRUE) {
    checkmate::assert_flag(init)

    path <- getOption(
        "epwshiftr.dir_store",
        tools::R_user_dir("epwshiftr", "data")
    )
    checkmate::assert_string(path, min.chars = 1L)

    path <- store_normalize_path(path)
    if (isTRUE(init) && !dir.exists(path)) {
        verbose(sprintf("Creating epwshiftr store directory '%s'", path))
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    if (isTRUE(init)) {
        path <- store_normalize_path(path)
    }

    if (isTRUE(init) && !checkmate::test_directory_exists(path, "rw")) {
        stop(sprintf("epwshiftr store directory '%s' does not exist or is not writable.", path), call. = FALSE)
    }

    path
}

# store_path {{{
store_path <- function(..., root = store_dir(init = TRUE)) {
    parts <- c(list(root), list(...))
    do.call(file.path, parts)
}
# }}}

# store_abs_path {{{
store_abs_path <- function(path, root = store_dir(init = TRUE)) {
    checkmate::assert_string(path, min.chars = 1L)
    checkmate::assert_string(root, min.chars = 1L)

    if (store_is_abs_path(path)) {
        return(store_normalize_path(path))
    }
    store_normalize_path(file.path(root, path))
}
# }}}

# store_rel_path {{{
store_rel_path <- function(path, root = store_dir(init = TRUE)) {
    checkmate::assert_string(path, min.chars = 1L)
    checkmate::assert_string(root, min.chars = 1L)

    path <- store_abs_path(path, root)
    root <- store_normalize_path(root)
    root_prefix <- paste0(sub("/+$", "", root), "/")
    if (identical(path, root)) {
        return(".")
    }
    if (!startsWith(path, root_prefix)) {
        stop(sprintf("Path '%s' is outside the epwshiftr store root '%s'.", path, root), call. = FALSE)
    }
    substring(path, nchar(root_prefix) + 1L)
}
# }}}

# checksum helpers {{{
checksum_file <- function(path, algo = "sha256") {
    checkmate::assert_file_exists(path, access = "r")
    checkmate::assert_choice(algo, c("md5", "sha256"))

    out <- if (identical(algo, "sha256")) {
        tools::sha256sum(path)
    } else {
        tools::md5sum(path)
    }
    unname(as.character(out))
}

checksum_bytes <- function(bytes, algo = "sha256") {
    if (!is.raw(bytes)) {
        stop("`bytes` must be a raw vector.", call. = FALSE)
    }
    checkmate::assert_choice(algo, c("md5", "sha256"))

    out <- if (identical(algo, "sha256")) {
        tools::sha256sum(bytes = bytes)
    } else {
        tools::md5sum(bytes = bytes)
    }
    unname(as.character(out))
}
# }}}

# duckdb helpers {{{
ddb_connect <- function(dbdir, read_only = FALSE, ...) {
    duckdb::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = read_only, ...)
}

ddb_disconnect <- function(conn, shutdown = TRUE) {
    duckdb::dbDisconnect(conn, shutdown = shutdown)
}

ddb_is_valid <- function(conn) {
    duckdb::dbIsValid(conn)
}

ddb_exec <- function(conn, sql) {
    duckdb::sql_exec(sql, conn = conn)
}

ddb_query <- function(conn, sql) {
    duckdb::sql_query(sql, conn = conn)
}

ddb_list_tables <- function(conn) {
    duckdb::dbListTables(conn)
}

ddb_read_table <- function(conn, table) {
    ddb_query(conn, sprintf("SELECT * FROM %s", ddb_ident(conn, table)))
}

ddb_write_table <- function(conn, table, rows, ...) {
    duckdb::dbWriteTable(conn, table, rows, ...)
}

ddb_append_table <- function(conn, table, rows, ...) {
    duckdb::dbAppendTable(conn, table, rows, ...)
}

ddb_ident <- function(conn, x) {
    as.character(duckdb::dbQuoteIdentifier(conn, x))
}

ddb_literal <- function(conn, x) {
    as.character(duckdb::dbQuoteLiteral(conn, x))
}
# }}}

# write_parquet_file {{{
write_parquet_file <- function(dt, path) {
    checkmate::assert_data_frame(dt)
    checkmate::assert_string(path, min.chars = 1L)

    path <- normalizePath(path, mustWork = FALSE, winslash = "/")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

    conn <- ddb_connect(":memory:")
    on.exit(ddb_disconnect(conn), add = TRUE)

    tmp_file <- tempfile(tmpdir = dirname(path), fileext = ".parquet")
    tmp_table <- sprintf("tmp_parquet_%s", fast_hash(list(path, Sys.time(), stats::runif(1L))))
    on.exit({
        try(ddb_exec(conn, sprintf("DROP TABLE IF EXISTS %s", ddb_ident(conn, tmp_table))), silent = TRUE)
        if (file.exists(tmp_file)) {
            unlink(tmp_file)
        }
    }, add = TRUE)

    ddb_write_table(conn, tmp_table, as.data.frame(dt), temporary = TRUE, overwrite = TRUE)
    ddb_exec(conn, sprintf(
        "COPY %s TO %s (FORMAT PARQUET)",
        ddb_ident(conn, tmp_table),
        ddb_literal(conn, tmp_file)
    ))

    if (file.exists(path)) {
        unlink(path)
    }
    ok <- file.rename(tmp_file, path)
    if (!ok) {
        stop(sprintf("Failed to write Parquet file '%s'.", path), call. = FALSE)
    }

    invisible(path)
}
# }}}

# mirai helpers {{{
mirai_default_workers <- function(n) {
    if (!n) {
        return(0L)
    }

    workers <- getOption("epwshiftr.mirai_workers", NULL)
    if (is.null(workers)) {
        workers <- parallel::detectCores(logical = FALSE)
        if (is.na(workers) || workers < 1L) {
            workers <- 1L
        }
    }

    max(1L, min(as.integer(workers), n))
}

mirai_error_message <- function(result) {
    if (inherits(result, "miraiError")) {
        message <- attr(result, "message", exact = TRUE)
        if (is.null(message) || !nzchar(message)) {
            message <- as.character(result)
        }
        return(message)
    }

    if (inherits(result, "errorValue")) {
        code <- unclass(result)[[1L]]
        return(sprintf("mirai worker returned error code %s", code))
    }

    ""
}

mirai_worker_bindings <- function(symbols = character()) {
    ns <- asNamespace("epwshiftr")
    symbols <- unique(c(
        "J",
        "copy",
        "rbindlist",
        "set",
        "setcolorder",
        "setnames",
        symbols
    ))
    symbols <- symbols[symbols %in% ls(ns, all.names = TRUE)]

    bindings <- mget(symbols, envir = ns, inherits = FALSE)
    local <- vapply(
        bindings,
        function(x) is.function(x) && identical(environment(x), ns),
        logical(1L)
    )
    attr(bindings, "local_symbols") <- names(bindings)[local]
    bindings
}

mirai_lapply <- function(X, FUN, ..., workers = NULL, symbols = character(), label = "mirai task") {
    checkmate::assert_function(FUN)

    n <- length(X)
    if (!n) {
        return(vector("list", 0L))
    }

    workers <- if (is.null(workers)) mirai_default_workers(n) else max(1L, min(as.integer(workers), n))
    dot_args <- list(...)

    if (workers <= 1L) {
        return(lapply(X, function(x) do.call(FUN, c(list(x), dot_args))))
    }

    compute_profile <- sprintf("epwshiftr-%s-%s", gsub("[^A-Za-z0-9]+", "-", label), fast_hash(list(Sys.getpid(), Sys.time(), stats::runif(1L))))
    mirai::daemons(workers, dispatcher = TRUE, .compute = compute_profile)
    on.exit(mirai::daemons(0, .compute = compute_profile), add = TRUE)

    worker_symbols <- mirai_worker_bindings(symbols)
    tasks <- lapply(X, function(x) {
        mirai::mirai(
            {
                old_env <- environment(FUN)
                worker_env <- new.env(parent = old_env)
                if (length(worker_symbols)) {
                    local_symbols <- attr(worker_symbols, "local_symbols", exact = TRUE)
                    list2env(worker_symbols, envir = worker_env)
                    for (nm in local_symbols) {
                        if (is.function(worker_env[[nm]])) {
                            environment(worker_env[[nm]]) <- worker_env
                        }
                    }
                }
                environment(FUN) <- worker_env

                do.call(FUN, c(list(x), dot_args))
            },
            FUN = FUN,
            x = x,
            dot_args = dot_args,
            worker_symbols = worker_symbols,
            .compute = compute_profile
        )
    })

    results <- lapply(tasks, mirai::collect_mirai)
    errors <- vapply(results, mirai_error_message, character(1L))
    failed <- nzchar(errors)
    if (any(failed)) {
        stop(sprintf(
            "Failed to complete %s for %d task(s). First error: %s",
            label,
            sum(failed),
            errors[failed][[1L]]
        ), call. = FALSE)
    }

    results
}

mirai_map <- function(FUN, ..., workers = NULL, symbols = character(), label = "mirai map") {
    args <- list(...)
    if (!length(args)) {
        stop("At least one mapped input is required.", call. = FALSE)
    }

    lens <- vapply(args, length, integer(1L))
    if (!lens[[1L]]) {
        return(vector("list", 0L))
    }
    if (length(unique(lens)) != 1L) {
        stop("Mapped inputs must have the same length.", call. = FALSE)
    }

    items <- lapply(seq_len(lens[[1L]]), function(i) lapply(args, `[[`, i))
    mirai_lapply(
        items,
        function(item) do.call(FUN, item),
        workers = workers,
        symbols = symbols,
        label = label
    )
}

netcdf_mirai_symbols <- function(extra = character()) {
    ns <- asNamespace("epwshiftr")
    unique(c(
        "CF_TIME_CALENDARS",
        "CF_TIME_UNIT_SECONDS",
        grep("^cf_time_", ls(ns, all.names = TRUE), value = TRUE),
        "get_nc_atts",
        "get_nc_axes",
        "get_nc_data",
        "get_nc_dims",
        "get_nc_meta",
        "get_nc_time",
        "get_nc_time_att",
        "match_location_coord",
        "match_nc_coord",
        "match_nc_time",
        "normalize_cf_calendar",
        "parse_cf_time",
        "summary_database_scan_file",
        "to_radian",
        "tunnel_dist",
        extra
    ))
}
# }}}

# store_hash_file {{{
store_hash_file <- function(path, algo = "sha256") {
    checkmate::assert_file_exists(path, access = "r")
    checkmate::assert_choice(algo, c("md5", "sha256"))

    checksum_file(path, algo)
}
# }}}

# store_write_json_atomic {{{
store_write_json_atomic <- function(x, path, ...) {
    checkmate::assert_string(path, min.chars = 1L)
    path <- store_normalize_path(path)

    dir <- dirname(path)
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    tmp <- tempfile(pattern = paste0(basename(path), "-"), tmpdir = dir)
    on.exit(unlink(tmp, force = TRUE), add = TRUE)
    jsonlite::write_json(x, tmp, ...)
    if (!file.rename(tmp, path)) {
        file.copy(tmp, path, overwrite = TRUE)
        unlink(tmp, force = TRUE)
    }
    path
}
# }}}

# store_cmip6_index_active_key {{{
store_cmip6_index_active_key <- function() {
    "active_cmip6_index_artifact_id"
}
# }}}

# store_cmip6_index_dir {{{
store_cmip6_index_dir <- function(init = TRUE) {
    checkmate::assert_flag(init)
    path <- store_path("queries", "cmip6-index", root = store_dir(init = init))
    if (isTRUE(init)) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    path
}
# }}}

# store_cmip6_index_path {{{
store_cmip6_index_path <- function(index_id, init = TRUE) {
    checkmate::assert_string(index_id, min.chars = 1L)
    file.path(store_cmip6_index_dir(init), sprintf("%s.csv", index_id))
}
# }}}

# store_cmip6_index_save {{{
store_cmip6_index_save <- function(index) {
    checkmate::assert_data_table(index)

    dir <- store_cmip6_index_dir(init = TRUE)
    tmp <- tempfile(pattern = "cmip6-index-", tmpdir = dir, fileext = ".csv")
    on.exit(unlink(tmp, force = TRUE), add = TRUE)
    data.table::fwrite(index, tmp)

    index_id <- store_hash_file(tmp)
    path <- store_cmip6_index_path(index_id, init = TRUE)
    if (!file.exists(path)) {
        if (!file.rename(tmp, path)) {
            ok <- file.copy(tmp, path, overwrite = TRUE)
            if (!isTRUE(ok)) {
                stop(sprintf("Failed to save CMIP6 index artifact to '%s'.", path), call. = FALSE)
            }
        }
    }

    store <- EsgStore$new(store_dir(init = TRUE))
    on.exit(store$close(), add = TRUE)
    artifact_id <- store$register_artifact(
        kind = "cmip6_index",
        path = path,
        role = "input",
        project = "CMIP6",
        metadata = list(
            rows = nrow(index),
            columns = names(index)
        )
    )
    store$set_meta(store_cmip6_index_active_key(), artifact_id)

    path
}
# }}}

# store_cmip6_index_active_path {{{
store_cmip6_index_active_path <- function() {
    root <- store_dir(init = FALSE)
    if (!dir.exists(root)) {
        return(NULL)
    }

    store <- EsgStore$new(root, create = FALSE)
    on.exit(store$close(), add = TRUE)
    artifact_id <- store$get_meta(store_cmip6_index_active_key())
    if (is.null(artifact_id) || is.na(artifact_id) || !nzchar(artifact_id)) {
        return(NULL)
    }

    tryCatch(
        store$artifact_path(artifact_id),
        error = function(e) NULL
    )
}
# }}}

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
