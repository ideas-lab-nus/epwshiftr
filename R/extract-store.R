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

        # finalize {{{
        finalize = function() {
            private$disconnect()
        }
        # }}}
    )
)
# }}}
