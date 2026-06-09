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
        },
        # }}}

        # plan_region {{{
        #' @description
        #' Plan regional extraction jobs from cataloged files.
        #'
        #' @param query_id Query ID returned by `$add_files()`.
        #' @param lon,lat Target longitude and latitude.
        #' @param time Length-2 time range.
        #' @param site_id Site identifier. Default: `"site-1"`.
        #' @param variable_id Optional variable IDs. If `NULL`, all cataloged
        #'        variables in the query are used.
        #' @param filters Named list of exact-match file catalog filters.
        #' @param nearest Number of nearest grid cells to keep. Default: `1L`.
        #'
        #' @return A data.table of extraction plan rows.
        plan_region = function(query_id, lon, lat, time, site_id = "site-1",
                               variable_id = NULL, filters = list(), nearest = 1L) {
            checkmate::assert_string(query_id)
            checkmate::assert_number(lon, lower = -180, upper = 360, finite = TRUE)
            checkmate::assert_number(lat, lower = -90, upper = 90, finite = TRUE)
            checkmate::assert_string(site_id)
            checkmate::assert_character(variable_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_list(filters, names = "unique")
            checkmate::assert_int(nearest, lower = 1L)
            private$check_open()

            time_range <- extract_store_parse_time_range(time)
            catalog <- data.table::as.data.table(DBI::dbReadTable(private$conn, "file_catalog"))
            catalog <- catalog[catalog$query_id == query_id]
            if (!nrow(catalog)) {
                cli::cli_abort("No cataloged file records were found for query ID {.val {query_id}}.")
            }

            filter_names <- names(filters)
            if (length(filters)) {
                if (is.null(filter_names) || any(!nzchar(filter_names))) {
                    cli::cli_abort("All file catalog filters must be named.")
                }
                unknown <- setdiff(filter_names, names(catalog))
                if (length(unknown)) {
                    cli::cli_abort("Unknown file catalog filter column(s): {.val {unknown}}.")
                }

                for (name in filter_names) {
                    value <- filters[[name]]
                    checkmate::assert_atomic_vector(value, any.missing = FALSE, min.len = 1L, .var.name = sprintf("filters$%s", name))
                    catalog <- catalog[as.character(get(name)) %in% as.character(value)]
                }
            }

            if (!nrow(catalog)) {
                cli::cli_abort("No cataloged file records match the requested extraction plan filters.")
            }

            if (!is.null(variable_id)) {
                if (any(!is.na(catalog$variable_id))) {
                    catalog <- catalog[catalog$variable_id %in% variable_id]
                }
            } else {
                variable_id <- unique(catalog$variable_id)
                variable_id <- variable_id[!is.na(variable_id) & nzchar(variable_id)]
                if (!length(variable_id)) {
                    cli::cli_abort("Cannot plan extraction without `variable_id` because the file catalog does not contain variable IDs.")
                }
            }

            plan <- data.table::as.data.table(catalog)
            if (!any(!is.na(plan$variable_id)) && !is.null(variable_id)) {
                plan <- plan[rep(seq_len(nrow(plan)), each = length(variable_id))]
                plan$variable_id <- rep(variable_id, times = nrow(catalog))
            } else {
                plan <- plan[plan$variable_id %in% variable_id]
            }
            if (!nrow(plan)) {
                cli::cli_abort("No cataloged file records match the requested variable IDs.")
            }

            now <- extract_store_now()
            out <- data.frame(
                plan_id = vapply(seq_len(nrow(plan)), function(i) {
                    extract_store_hash(
                        plan$file_key[[i]],
                        site_id,
                        plan$variable_id[[i]],
                        lon,
                        lat,
                        nearest,
                        time_range$start,
                        time_range$stop
                    )
                }, character(1L)),
                query_id = query_id,
                file_key = plan$file_key,
                site_id = site_id,
                variable_id = plan$variable_id,
                lon = lon,
                lat = lat,
                nearest = nearest,
                time_start = time_range$start,
                time_stop = time_range$stop,
                status = "pending",
                available_time_count = NA_integer_,
                attempt_count = 0L,
                last_error = NA_character_,
                created_at = now,
                updated_at = now,
                stringsAsFactors = FALSE
            )
            out <- unique(out)
            private$append_new_rows("extraction_plan", out, "plan_id")

            existing <- data.table::as.data.table(DBI::dbReadTable(private$conn, "extraction_plan"))
            existing[plan_id %in% out$plan_id]
        },
        # }}}

        # extract {{{
        #' @description
        #' Execute pending or failed regional extraction plans.
        #'
        #' @param plan_id Optional plan IDs to run.
        #' @param status Plan statuses to run when `plan_id` is `NULL`.
        #'        Default: `c("pending", "failed")`.
        #' @param fallback What to do when OPeNDAP is unavailable. `"auto"`
        #'        downloads through HTTPServer when possible; `"error"` marks
        #'        the plan failed without downloading. Default: `"auto"`.
        #' @param overwrite If `TRUE`, overwrite existing Parquet outputs.
        #'        Default: `FALSE`.
        #'
        #' @return A data.table of processed extraction plan rows.
        extract = function(plan_id = NULL, status = c("pending", "failed"),
                           fallback = c("auto", "error"), overwrite = FALSE) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_subset(status, c("pending", "failed", "empty", "done"))
            checkmate::assert_flag(overwrite)
            fallback <- match.arg(fallback)
            private$check_open()

            plans <- data.table::as.data.table(DBI::dbReadTable(private$conn, "extraction_plan"))
            if (is.null(plan_id)) {
                plans <- plans[plans$status %in% status]
            } else {
                plans <- plans[plans$plan_id %in% plan_id]
            }
            if (!nrow(plans)) {
                return(plans)
            }

            catalog <- data.table::as.data.table(DBI::dbReadTable(private$conn, "file_catalog"))
            processed <- vector("list", nrow(plans))
            for (i in seq_len(nrow(plans))) {
                plan <- plans[i]
                file <- catalog[catalog$file_key == plan$file_key[[1L]]]
                if (!nrow(file)) {
                    processed[[i]] <- private$mark_plan_failed(plan, "The cataloged file record no longer exists.")
                    next
                }

                processed[[i]] <- tryCatch(
                    private$extract_one(plan, file[1L], fallback = fallback, overwrite = overwrite),
                    error = function(e) private$mark_plan_failed(plan, conditionMessage(e))
                )
            }

            data.table::rbindlist(processed, use.names = TRUE, fill = TRUE)
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

        # append_new_rows {{{
        append_new_rows = function(table, rows, key) {
            checkmate::assert_string(table)
            checkmate::assert_string(key)
            if (!nrow(rows)) {
                return(invisible(NULL))
            }

            q_table <- DBI::dbQuoteIdentifier(private$conn, table)
            q_key <- DBI::dbQuoteIdentifier(private$conn, key)
            current <- DBI::dbGetQuery(private$conn, sprintf("SELECT %s FROM %s", q_key, q_table))[[key]]
            rows <- rows[!rows[[key]] %in% current, , drop = FALSE]
            if (nrow(rows)) {
                DBI::dbAppendTable(private$conn, table, rows)
            }

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

        # extract_one {{{
        extract_one = function(plan, file, fallback = "auto", overwrite = FALSE) {
            opened <- private$open_plan_dataset(file, fallback = fallback, overwrite = overwrite)
            ds <- opened$dataset
            on.exit(if (isTRUE(ds$is_open)) ds$close(), add = TRUE)

            time_axis <- ds$get_time_axis(index = 1L)$values
            valid_time <- time_axis[!is.na(time_axis)]
            if (!length(valid_time)) {
                stop("The NetCDF time axis is empty or unavailable.", call. = FALSE)
            }
            actual_start <- min(valid_time)
            actual_end <- max(valid_time)
            requested_time <- c(plan$time_start[[1L]], plan$time_stop[[1L]])
            available_time_count <- sum(valid_time >= requested_time[[1L]] & valid_time <= requested_time[[2L]])
            private$update_file_actual_time(file, actual_start, actual_end)

            dt <- ds$read_region(
                variable = plan$variable_id[[1L]],
                lon = plan$lon[[1L]],
                lat = plan$lat[[1L]],
                time = requested_time,
                nearest = plan$nearest[[1L]]
            )
            if (!nrow(dt)) {
                return(private$mark_plan_status(
                    plan,
                    status = "empty",
                    available_time_count = available_time_count,
                    last_error = NA_character_
                ))
            }

            private$decorate_extract(dt, plan, file)
            results <- private$write_extract_partitions(dt, plan, file, overwrite = overwrite)
            private$delete_by_key("extraction_result", "plan_id", plan$plan_id)
            if (nrow(results)) {
                DBI::dbAppendTable(private$conn, "extraction_result", results)
            }

            private$mark_plan_status(
                plan,
                status = "done",
                available_time_count = available_time_count,
                last_error = NA_character_
            )
        },
        # }}}

        # open_plan_dataset {{{
        open_plan_dataset = function(file, fallback = "auto", overwrite = FALSE) {
            opendap <- extract_store_na_character(file$url_opendap)
            if (!is.na(opendap) && nzchar(opendap)) {
                ds <- EsgDataset$new(opendap)
                open_error <- NULL
                ok <- tryCatch(
                    {
                        ds$open()
                        TRUE
                    },
                    error = function(e) {
                        open_error <<- e
                        FALSE
                    }
                )
                if (isTRUE(ok)) {
                    return(list(dataset = ds, target = opendap))
                }
                if (identical(fallback, "error")) {
                    stop(open_error)
                }
                if (isTRUE(ds$is_open)) {
                    ds$close()
                }
            }

            if (identical(fallback, "error")) {
                stop("OPeNDAP is not available for this file record.", call. = FALSE)
            }

            local_path <- private$download_plan_file(file, overwrite = overwrite)
            ds <- EsgDataset$new(local_path)
            ds$open()
            list(dataset = ds, target = local_path)
        },
        # }}}

        # download_plan_file {{{
        download_plan_file = function(file, overwrite = FALSE) {
            download <- extract_store_na_character(file$url_download)
            if (is.na(download) || !nzchar(download)) {
                stop("HTTPServer download URL is not available for this file record.", call. = FALSE)
            }

            checksum <- extract_store_na_character(file$checksum)
            checksum_type <- tolower(extract_store_na_character(file$checksum_type))
            if (is.na(checksum_type) || !checksum_type %in% c("md5", "sha256")) {
                checksum <- NULL
                checksum_type <- "sha256"
            }
            if (is.na(checksum)) {
                checksum <- NULL
            }

            downloader <- FileDownloader$new(dest = private$download_dir, n_workers = 0L)
            local_path <- downloader$download(
                url = download,
                progress = FALSE,
                overwrite = overwrite,
                checksum = checksum,
                checksum_type = checksum_type
            )
            file$local_path <- local_path
            private$replace_rows("file_catalog", as.data.frame(file), "file_key")
            local_path
        },
        # }}}

        # decorate_extract {{{
        decorate_extract = function(dt, plan, file) {
            dt[, `:=`(
                plan_id = plan$plan_id[[1L]],
                file_key = plan$file_key[[1L]],
                query_id = plan$query_id[[1L]],
                site_id = plan$site_id[[1L]],
                requested_lon = plan$lon[[1L]],
                requested_lat = plan$lat[[1L]],
                source_id = file$source_id[[1L]],
                experiment_id = file$experiment_id[[1L]],
                variant_label = file$variant_label[[1L]],
                frequency = file$frequency[[1L]],
                table_id = file$table_id[[1L]],
                variable_id = plan$variable_id[[1L]],
                grid_label = file$grid_label[[1L]]
            )]
            data.table::setcolorder(dt, c(
                "plan_id", "file_key", "query_id", "site_id",
                "source_id", "experiment_id", "variant_label", "frequency",
                "table_id", "variable_id", "variable", "grid_label",
                "time", "lon", "lat", "dist", "value",
                setdiff(names(dt), c(
                    "plan_id", "file_key", "query_id", "site_id",
                    "source_id", "experiment_id", "variant_label", "frequency",
                    "table_id", "variable_id", "variable", "grid_label",
                    "time", "lon", "lat", "dist", "value"
                ))
            ))
            invisible(dt)
        },
        # }}}

        # write_extract_partitions {{{
        write_extract_partitions = function(dt, plan, file, overwrite = FALSE) {
            dt[, year := as.integer(format(time, "%Y", tz = "UTC"))]
            years <- sort(unique(dt$year))
            results <- vector("list", length(years))
            for (i in seq_along(years)) {
                year <- years[[i]]
                chunk <- dt[dt$year == year]
                output_path <- private$output_path(plan, file, year)
                private$write_parquet(chunk, output_path, overwrite = overwrite)
                results[[i]] <- private$extract_result_row(plan, chunk, output_path, year)
            }

            data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
        },
        # }}}

        # output_path {{{
        output_path = function(plan, file, year) {
            parts <- c(
                project = "CMIP6",
                source_id = file$source_id[[1L]],
                experiment_id = file$experiment_id[[1L]],
                variant_label = file$variant_label[[1L]],
                frequency = file$frequency[[1L]],
                variable_id = plan$variable_id[[1L]],
                year = year
            )
            dirs <- paste0(names(parts), "=", vapply(parts, extract_store_partition_value, character(1L)))
            file.path(private$data_dir, do.call(file.path, as.list(dirs)), sprintf("part-%s.parquet", plan$plan_id[[1L]]))
        },
        # }}}

        # write_parquet {{{
        write_parquet = function(dt, path, overwrite = FALSE) {
            if (file.exists(path) && !isTRUE(overwrite)) {
                stop(sprintf("Parquet output already exists: %s.", path), call. = FALSE)
            }
            dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
            tmp_file <- tempfile(tmpdir = dirname(path), fileext = ".parquet")
            tmp_table <- sprintf("tmp_extract_%s", substr(extract_store_hash(path, Sys.time(), runif(1L)), 1L, 16L))
            on.exit({
                try(DBI::dbExecute(private$conn, sprintf("DROP TABLE IF EXISTS %s", DBI::dbQuoteIdentifier(private$conn, tmp_table))), silent = TRUE)
                if (file.exists(tmp_file)) {
                    unlink(tmp_file)
                }
            }, add = TRUE)

            DBI::dbWriteTable(private$conn, tmp_table, as.data.frame(dt), temporary = TRUE, overwrite = TRUE)
            DBI::dbExecute(private$conn, sprintf(
                "COPY %s TO %s (FORMAT PARQUET)",
                DBI::dbQuoteIdentifier(private$conn, tmp_table),
                DBI::dbQuoteString(private$conn, tmp_file)
            ))
            if (file.exists(path) && isTRUE(overwrite)) {
                unlink(path)
            }
            ok <- file.rename(tmp_file, path)
            if (!isTRUE(ok)) {
                stop(sprintf("Failed to move Parquet output into place: %s.", path), call. = FALSE)
            }

            invisible(path)
        },
        # }}}

        # extract_result_row {{{
        extract_result_row = function(plan, dt, output_path, year) {
            data.frame(
                result_id = extract_store_hash(plan$plan_id[[1L]], year, output_path),
                plan_id = plan$plan_id[[1L]],
                file_key = plan$file_key[[1L]],
                query_id = plan$query_id[[1L]],
                output_path = extract_store_rel_path(output_path, private$store_path),
                year = as.integer(year),
                row_count = nrow(dt),
                unique_time_count = data.table::uniqueN(dt$time),
                time_min = min(dt$time),
                time_max = max(dt$time),
                lon_actual = dt$lon[[which.min(dt$dist)]],
                lat_actual = dt$lat[[which.min(dt$dist)]],
                dist_min = min(dt$dist, na.rm = TRUE),
                completed_at = extract_store_now(),
                stringsAsFactors = FALSE
            )
        },
        # }}}

        # update_file_actual_time {{{
        update_file_actual_time = function(file, start, end) {
            file$actual_time_start <- start
            file$actual_time_end <- end
            private$replace_rows("file_catalog", as.data.frame(file), "file_key")
            invisible(NULL)
        },
        # }}}

        # mark_plan_failed {{{
        mark_plan_failed = function(plan, message) {
            private$mark_plan_status(
                plan,
                status = "failed",
                available_time_count = plan$available_time_count[[1L]],
                last_error = message,
                increment_attempt = TRUE
            )
        },
        # }}}

        # mark_plan_status {{{
        mark_plan_status = function(plan, status, available_time_count = NULL,
                                    last_error = NA_character_, increment_attempt = FALSE) {
            plan <- as.data.frame(plan)
            plan$status <- status
            if (!is.null(available_time_count)) {
                plan$available_time_count <- as.integer(available_time_count)
            }
            if (isTRUE(increment_attempt)) {
                plan$attempt_count <- as.integer(plan$attempt_count) + 1L
            }
            plan$last_error <- extract_store_na_character(last_error)
            plan$updated_at <- extract_store_now()
            private$replace_rows("extraction_plan", plan, "plan_id")
            data.table::as.data.table(plan)
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

extract_store_parse_time_range <- function(time) {
    checkmate::assert_atomic_vector(time, len = 2L, any.missing = FALSE)
    parsed <- extract_store_parse_datetime(time)
    if (any(is.na(parsed))) {
        cli::cli_abort("`time` must be a length-2 parseable datetime range.")
    }
    if (parsed[[2L]] < parsed[[1L]]) {
        cli::cli_abort("The second `time` value must be greater than or equal to the first.")
    }

    list(start = parsed[[1L]], stop = parsed[[2L]])
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

extract_store_partition_value <- function(x) {
    x <- extract_store_na_character(x)
    if (is.na(x) || !nzchar(x)) {
        return("__missing__")
    }

    x <- gsub("[/\\\\:]+", "_", x)
    x <- gsub("[^A-Za-z0-9_.=-]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- sub("^_+", "", sub("_+$", "", x))
    if (!nzchar(x)) {
        return("__missing__")
    }

    x
}
# }}}
