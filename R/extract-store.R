# EsgExtractStore {{{
#' Local ESGF Regional Extraction Store
#'
#' @description
#'
#' `EsgExtractStore` manages a local DuckDB manifest and a fixed directory
#' layout for query result snapshots, downloaded NetCDF files, and Parquet
#' regional extracts.
#'
#' @author Hongyuan Jia
#' @name EsgExtractStore
#' @export
EsgExtractStore <- R6::R6Class(
    "EsgExtractStore",
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # initialize {{{
        #' @description
        #' Create or open a local extraction store.
        #'
        #' @param path Store directory.
        #' @param create If `TRUE`, create the store directory when it does not
        #'        exist. Default: `TRUE`.
        #' @param overwrite If `TRUE`, remove an existing store directory before
        #'        creating a new store. Default: `FALSE`.
        #'
        #' @return An `EsgExtractStore` object.
        initialize = function(path, create = TRUE, overwrite = FALSE) {
            checkmate::assert_string(path)
            checkmate::assert_flag(create)
            checkmate::assert_flag(overwrite)

            path <- path.expand(path)
            if (dir.exists(path) && isTRUE(overwrite)) {
                unlink(path, recursive = TRUE, force = TRUE)
            }
            if (!dir.exists(path)) {
                if (!isTRUE(create)) {
                    cli::cli_abort("Store directory does not exist: {.path {path}}.")
                }
                dir.create(path, recursive = TRUE, showWarnings = FALSE)
            }
            if (!dir.exists(path)) {
                cli::cli_abort("Failed to create store directory: {.path {path}}.")
            }

            private$store_path <- normalizePath(path, mustWork = TRUE)
            private$query_dir <- file.path(private$store_path, "queries")
            private$download_dir <- file.path(private$store_path, "downloads")
            private$data_dir <- file.path(private$store_path, "data")
            dirs <- c(private$query_dir, private$download_dir, private$data_dir)
            for (dir in dirs) {
                if (!dir.exists(dir)) {
                    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
                }
            }

            private$manifest_path <- file.path(private$store_path, "manifest.duckdb")
            private$connect()
            private$init_schema()

            self
        },
        # }}}

        # close {{{
        #' @description
        #' Close the DuckDB connection.
        #'
        #' @return The store object itself, invisibly.
        close = function() {
            private$disconnect()
            invisible(self)
        },
        # }}}

        # add_files {{{
        #' @description
        #' Add File or Aggregation query results to the local file catalog.
        #'
        #' @param files An `EsgResultFile` or `EsgResultAggregation` object.
        #' @param label Optional label for this query run.
        #'
        #' @return The created or updated query ID.
        add_files = function(files, label = NULL) {
            checkmate::assert_string(label, null.ok = TRUE)
            private$check_open()

            result_type <- extract_store_result_type(files)
            dt <- extract_store_file_table(files)
            query_id <- extract_store_hash(
                result_type,
                priv(files)$index_node,
                jsonlite::toJSON(priv(files)$parameter$serialize(null = TRUE), auto_unbox = TRUE),
                jsonlite::toJSON(dt, dataframe = "rows", POSIXt = "ISO8601", auto_unbox = TRUE, null = "null")
            )

            query_file <- file.path(private$query_dir, sprintf("files-%s.json", query_id))
            files$save(query_file)

            time_filter <- files$time_filter
            query_run <- data.frame(
                query_id = query_id,
                label = extract_store_na_character(label),
                result_type = result_type,
                query_file = extract_store_rel_path(query_file, private$store_path),
                index_node = priv(files)$index_node,
                time_filter_start = extract_store_parse_datetime_scalar(extract_store_list_value(time_filter, "start")),
                time_filter_stop = extract_store_parse_datetime_scalar(extract_store_list_value(time_filter, "stop")),
                time_filter_method = extract_store_na_character(extract_store_list_value(time_filter, "method")),
                created_at = extract_store_now(),
                package_version = as.character(utils::packageVersion("epwshiftr")),
                stringsAsFactors = FALSE
            )
            private$replace_rows("query_run", query_run, "query_id")

            if (nrow(dt)) {
                now <- extract_store_now()
                catalog <- data.frame(
                    file_key = extract_store_file_keys(dt),
                    query_id = query_id,
                    esgf_id = dt$id,
                    dataset_id = dt$dataset_id,
                    title = dt$title,
                    filename = dt$filename,
                    tracking_id = dt$tracking_id,
                    checksum = dt$checksum,
                    checksum_type = dt$checksum_type,
                    size = suppressWarnings(as.numeric(dt$size)),
                    data_node = dt$data_node,
                    source_id = dt$source_id,
                    experiment_id = dt$experiment_id,
                    variant_label = dt$variant_label,
                    frequency = dt$frequency,
                    table_id = dt$table_id,
                    variable_id = dt$variable_id,
                    grid_label = dt$grid_label,
                    datetime_start = extract_store_parse_datetime(dt$datetime_start),
                    datetime_end = extract_store_parse_datetime(dt$datetime_end),
                    actual_time_start = extract_store_parse_datetime(rep(NA_character_, nrow(dt))),
                    actual_time_end = extract_store_parse_datetime(rep(NA_character_, nrow(dt))),
                    url_opendap = dt$url_opendap,
                    url_download = dt$url_download,
                    local_path = rep(NA_character_, nrow(dt)),
                    created_at = rep(now, nrow(dt)),
                    stringsAsFactors = FALSE
                )
                private$replace_rows("file_catalog", catalog, "file_key")
            }

            query_id
        }
        # }}}
    ),
    active = list(
        # path {{{
        #' @field path Store directory.
        path = function() {
            private$store_path
        },
        # }}}

        # manifest {{{
        #' @field manifest DuckDB manifest path.
        manifest = function() {
            private$manifest_path
        },
        # }}}

        # is_open {{{
        #' @field is_open Whether the manifest connection is open.
        is_open = function() {
            !is.null(private$conn) && DBI::dbIsValid(private$conn)
        }
        # }}}
    ),
    private = list(
        store_path = NULL,
        manifest_path = NULL,
        query_dir = NULL,
        download_dir = NULL,
        data_dir = NULL,
        conn = NULL,

        # connect {{{
        connect = function() {
            private$conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = private$manifest_path, read_only = FALSE)
            invisible(private$conn)
        },
        # }}}

        # disconnect {{{
        disconnect = function() {
            if (is.null(private$conn)) {
                return(invisible(NULL))
            }
            if (DBI::dbIsValid(private$conn)) {
                tryCatch(
                    DBI::dbDisconnect(private$conn, shutdown = TRUE),
                    error = function(e) DBI::dbDisconnect(private$conn)
                )
            }
            private$conn <- NULL
            invisible(NULL)
        },
        # }}}

        # init_schema {{{
        init_schema = function() {
            private$exec("
                CREATE TABLE IF NOT EXISTS query_run (
                    query_id VARCHAR PRIMARY KEY,
                    label VARCHAR,
                    result_type VARCHAR,
                    query_file VARCHAR,
                    index_node VARCHAR,
                    time_filter_start TIMESTAMP,
                    time_filter_stop TIMESTAMP,
                    time_filter_method VARCHAR,
                    created_at TIMESTAMP,
                    package_version VARCHAR
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS file_catalog (
                    file_key VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    esgf_id VARCHAR,
                    dataset_id VARCHAR,
                    title VARCHAR,
                    filename VARCHAR,
                    tracking_id VARCHAR,
                    checksum VARCHAR,
                    checksum_type VARCHAR,
                    size DOUBLE,
                    data_node VARCHAR,
                    source_id VARCHAR,
                    experiment_id VARCHAR,
                    variant_label VARCHAR,
                    frequency VARCHAR,
                    table_id VARCHAR,
                    variable_id VARCHAR,
                    grid_label VARCHAR,
                    datetime_start TIMESTAMP,
                    datetime_end TIMESTAMP,
                    actual_time_start TIMESTAMP,
                    actual_time_end TIMESTAMP,
                    url_opendap VARCHAR,
                    url_download VARCHAR,
                    local_path VARCHAR,
                    created_at TIMESTAMP
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS extraction_plan (
                    plan_id VARCHAR PRIMARY KEY,
                    query_id VARCHAR,
                    file_key VARCHAR,
                    site_id VARCHAR,
                    variable_id VARCHAR,
                    lon DOUBLE,
                    lat DOUBLE,
                    nearest INTEGER,
                    time_start TIMESTAMP,
                    time_stop TIMESTAMP,
                    status VARCHAR,
                    available_time_count INTEGER,
                    attempt_count INTEGER,
                    last_error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            ")
            private$exec("
                CREATE TABLE IF NOT EXISTS extraction_result (
                    result_id VARCHAR PRIMARY KEY,
                    plan_id VARCHAR,
                    file_key VARCHAR,
                    query_id VARCHAR,
                    output_path VARCHAR,
                    year INTEGER,
                    row_count INTEGER,
                    unique_time_count INTEGER,
                    time_min TIMESTAMP,
                    time_max TIMESTAMP,
                    lon_actual DOUBLE,
                    lat_actual DOUBLE,
                    dist_min DOUBLE,
                    completed_at TIMESTAMP
                )
            ")

            invisible(NULL)
        },
        # }}}

        # exec {{{
        exec = function(sql) {
            DBI::dbExecute(private$conn, sql)
        },
        # }}}

        # check_open {{{
        check_open = function() {
            if (!isTRUE(self$is_open)) {
                cli::cli_abort("The extraction store is closed.")
            }

            invisible(NULL)
        },
        # }}}

        # replace_rows {{{
        replace_rows = function(table, rows, key) {
            checkmate::assert_string(table)
            checkmate::assert_string(key)
            if (!nrow(rows)) {
                return(invisible(NULL))
            }

            keys <- unique(rows[[key]])
            if (!length(keys)) {
                return(invisible(NULL))
            }

            private$delete_by_key(table, key, keys)
            DBI::dbAppendTable(private$conn, table, rows)
            invisible(NULL)
        },
        # }}}

        # delete_by_key {{{
        delete_by_key = function(table, key, values) {
            checkmate::assert_string(table)
            checkmate::assert_string(key)
            values <- unique(as.character(values))
            values <- values[!is.na(values)]
            if (!length(values)) {
                return(invisible(NULL))
            }

            q_table <- DBI::dbQuoteIdentifier(private$conn, table)
            q_key <- DBI::dbQuoteIdentifier(private$conn, key)
            q_values <- paste(DBI::dbQuoteString(private$conn, values), collapse = ", ")
            DBI::dbExecute(private$conn, sprintf("DELETE FROM %s WHERE %s IN (%s)", q_table, q_key, q_values))
            invisible(NULL)
        },
        # }}}

        # finalize {{{
        finalize = function() {
            private$disconnect()
        }
        # }}}
    )
)
# }}}

# extract-store helpers {{{
extract_store_result_type <- function(files) {
    if (inherits(files, "EsgResultFile")) {
        return("File")
    }
    if (inherits(files, "EsgResultAggregation")) {
        return("Aggregation")
    }

    cli::cli_abort("`files` must be an EsgResultFile or EsgResultAggregation object.")
}

extract_store_now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}

extract_store_hash <- function(...) {
    text <- paste(vapply(list(...), extract_store_hash_piece, character(1L)), collapse = "\n")
    paste0(openssl::sha256(charToRaw(text)))
}

extract_store_hash_piece <- function(x) {
    if (is.null(x)) {
        return("<NULL>")
    }

    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    paste(x, collapse = "\r")
}

extract_store_na_character <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) {
        return(NA_character_)
    }

    as.character(x[[1L]])
}

extract_store_list_value <- function(x, name) {
    if (is.null(x) || !is.list(x) || !name %in% names(x)) {
        return(NA_character_)
    }

    x[[name]]
}

extract_store_parse_datetime_scalar <- function(x) {
    extract_store_parse_datetime(extract_store_na_character(x))[[1L]]
}

extract_store_parse_datetime <- function(x) {
    if (inherits(x, "POSIXt")) {
        return(as.POSIXct(x, tz = "UTC"))
    }
    x <- as.character(x)
    out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) {
        out[ok] <- parse_datetime(x[ok], tz = "UTC")
    }

    out
}

extract_store_rel_path <- function(path, root) {
    path <- normalizePath(path, mustWork = FALSE)
    root <- normalizePath(root, mustWork = TRUE)
    prefix <- paste0(root, .Platform$file.sep)
    if (startsWith(path, prefix)) {
        return(substring(path, nchar(prefix) + 1L))
    }

    path
}

extract_store_scalar_column <- function(dt, name, n = nrow(dt)) {
    if (!name %in% names(dt)) {
        return(rep(NA_character_, n))
    }

    value <- dt[[name]]
    if (is.list(value)) {
        value <- vapply(value, extract_store_na_character, character(1L))
    } else {
        value <- as.character(value)
        value[is.na(value)] <- NA_character_
    }

    value
}

extract_store_file_table <- function(files) {
    extract_store_result_type(files)
    dt <- data.table::copy(files$to_data_table())
    n <- nrow(dt)
    columns <- c(
        "id", "dataset_id", "title", "filename", "tracking_id",
        "checksum", "checksum_type", "size", "data_node", "source_id",
        "experiment_id", "variant_label", "frequency", "table_id",
        "variable_id", "grid_label", "datetime_start", "datetime_end",
        "url_opendap", "url_download"
    )

    out <- data.table::as.data.table(
        stats::setNames(rep(list(rep(NA_character_, n)), length(columns)), columns)
    )
    for (name in columns) {
        out[[name]] <- extract_store_scalar_column(dt, name, n)
    }
    if (n && all(is.na(out$filename)) && any(!is.na(out$title))) {
        out$filename <- basename(out$title)
    }

    out[]
}

extract_store_file_keys <- function(dt) {
    if (!nrow(dt)) {
        return(character())
    }

    vapply(seq_len(nrow(dt)), function(i) {
        pieces <- unlist(dt[i, c(
            "id", "tracking_id", "url_opendap", "url_download", "title"
        ), with = FALSE], use.names = FALSE)
        if (all(is.na(pieces) | !nzchar(pieces))) {
            cli::cli_abort("Cannot create a stable file key because file record {i} has no ID, tracking ID, URL, or title.")
        }
        extract_store_hash(pieces)
    }, character(1L))
}
# }}}
