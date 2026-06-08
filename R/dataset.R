# EsgDataset {{{
# DatasetAsyncTask {{{
dataset_async_error_condition <- function(class, message) {
    structure(list(message = message, call = NULL), class = c(class, "error", "condition"))
}

dataset_async_result_error <- function(operation, result, timeout_ms = NULL) {
    if (inherits(result, "miraiError")) {
        message <- attr(result, "message", exact = TRUE)
        if (is.null(message) || !nzchar(message)) {
            message <- as.character(result)
        }
        return(dataset_async_error_condition(
            "epwshiftr_async_failure",
            sprintf("Failed to %s: %s", operation, message)
        ))
    }

    if (!inherits(result, "errorValue")) {
        return(NULL)
    }

    code <- unclass(result)[[1L]]
    if (identical(code, 5L)) {
        suffix <- if (is.null(timeout_ms)) "" else sprintf(" after %d ms", timeout_ms)
        return(dataset_async_error_condition(
            "epwshiftr_async_timeout",
            sprintf("Failed to %s: async operation timed out%s.", operation, suffix)
        ))
    }

    if (identical(code, 20L)) {
        return(dataset_async_error_condition(
            "epwshiftr_async_cancelled",
            sprintf("Failed to %s: async operation was cancelled (best-effort).", operation)
        ))
    }

    dataset_async_error_condition(
        "epwshiftr_async_failure",
        sprintf("Failed to %s: async operation failed with mirai error code %s.", operation, code)
    )
}

DatasetAsyncTask <- R6::R6Class(
    "DatasetAsyncTask",
    lock_class = TRUE,
    public = list(
        operation = NULL,
        status = "pending",
        timeout_ms = NULL,
        mirai_obj = NULL,
        compute_profile = NULL,
        started_at = NULL,
        completed_at = NULL,
        result = NULL,
        error = NULL,
        cancellation_requested = FALSE,
        backend_released = FALSE,

        initialize = function(operation, mirai_obj, compute_profile, timeout_ms = NULL) {
            self$operation <- operation
            self$mirai_obj <- mirai_obj
            self$compute_profile <- compute_profile
            self$timeout_ms <- timeout_ms
            self$status <- "running"
            self$started_at <- Sys.time()
        },

        release_backend = function() {
            if (!isTRUE(self$backend_released) && !is.null(self$compute_profile)) {
                try(mirai::daemons(0, .compute = self$compute_profile), silent = TRUE)
                self$backend_released <- TRUE
            }
            invisible(self)
        },

        mark_terminal = function(status, result = NULL, error = NULL) {
            self$status <- status
            self$result <- result
            self$error <- error
            self$completed_at <- Sys.time()
            invisible(self)
        },

        collect = function() {
            if (identical(self$status, "completed")) {
                return(self$result)
            }

            if (identical(self$status, "cancelled") || identical(self$status, "failed") || identical(self$status, "timed_out")) {
                stop(self$error)
            }

            result <- mirai::collect_mirai(self$mirai_obj)
            error <- dataset_async_result_error(self$operation, result, self$timeout_ms)
            if (!is.null(error)) {
                status <- if (inherits(error, "epwshiftr_async_timeout")) {
                    "timed_out"
                } else if (inherits(error, "epwshiftr_async_cancelled")) {
                    "cancelled"
                } else {
                    "failed"
                }
                self$mark_terminal(status, error = error)
                self$release_backend()
                stop(error)
            }

            self$mark_terminal("completed", result = result)
            self$release_backend()
            result
        },

        cancel = function() {
            self$cancellation_requested <- TRUE

            if (is.null(self$mirai_obj)) {
                self$mark_terminal(
                    "cancelled",
                    error = dataset_async_error_condition(
                        "epwshiftr_async_cancelled",
                        sprintf("Failed to %s: async operation was cancelled (best-effort).", self$operation)
                    )
                )
                self$release_backend()
                return(FALSE)
            }

            if (!mirai::unresolved(self$mirai_obj)) {
                result <- self$mirai_obj$data
                error <- dataset_async_result_error(self$operation, result, self$timeout_ms)
                if (is.null(error)) {
                    self$mark_terminal("completed", result = result)
                } else {
                    status <- if (inherits(error, "epwshiftr_async_timeout")) {
                        "timed_out"
                    } else if (inherits(error, "epwshiftr_async_cancelled")) {
                        "cancelled"
                    } else {
                        "failed"
                    }
                    self$mark_terminal(status, error = error)
                }
                self$release_backend()
                return(FALSE)
            }

            requested <- isTRUE(mirai::stop_mirai(self$mirai_obj))
            self$mark_terminal(
                "cancelled",
                error = dataset_async_error_condition(
                    "epwshiftr_async_cancelled",
                    sprintf("Failed to %s: async operation was cancelled (best-effort).", self$operation)
                )
            )
            self$release_backend()
            requested
        }
    )
)
# }}}

#' Remote NetCDF Dataset Access via OPeNDAP
#'
#' @description
#'
#' `EsgDataset` provides a unified interface for accessing NetCDF data
#' remotely via OPeNDAP protocol. It wraps RNetCDF functions and provides
#' convenient methods for subsetting, slicing, and reading data without
#' downloading entire files.
#'
#' The class supports three levels of interfaces:
#'
#' - **Basic layer**: Direct wrappers around RNetCDF functions
#' - **Middle layer**: Convenient methods for subsetting by time/space
#' - **High layer**: Data manipulation and format conversion
#'
#' It also supports aggregating multiple files into a single logical dataset,
#' automatically handling time dimension concatenation.
#'
#' @author Hongyuan Jia
#' @name EsgDataset
#' @export
EsgDataset <- R6::R6Class(
    "EsgDataset",
    lock_class = TRUE,
    public = list(
        # initialize {{{
        #' @description
        #' Create a new EsgDataset object
        #'
        #' @param urls A character vector of OPeNDAP URLs. Can be a single URL
        #'        or multiple URLs for a multi-file dataset.
        #' @return An `EsgDataset` object.
        #'
        #' @examples
        #' \dontrun{
        #' # Single file
        #' ds <- EsgDataset$new("https://example.com/data.nc")
        #'
        #' # Multiple files
        #' ds <- EsgDataset$new(c("url1.nc", "url2.nc"))
        #' }
        initialize = function(urls) {
            checkmate::assert_character(urls, any.missing = FALSE, min.len = 1L)

            private$urls <- urls
            private$metadata_cache <- list()
            private$opened <- FALSE
            private$nc_handles <- vector("list", length(urls))

            self
        },
        # }}}

        # open {{{
        #' @description
        #' Open OPeNDAP connection(s)
        #'
        #' @param async If `TRUE`, first validates opening in a one-shot worker,
        #'        then re-opens caller-owned handles before returning so the
        #'        dataset remains opened after `open()` returns. The caller still
        #'        receives the final `EsgDataset` object itself rather than a
        #'        `Mirai`/`Future`-like handle. Default: `FALSE`.
        #' @param timeout Optional positive number of seconds for the async
        #'        worker pre-open phase. Only supported when `async = TRUE`.
        #'        It does not limit the final caller-owned reopen that makes the
        #'        returned dataset stay opened.
        #'
        #' @return The `EsgDataset` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' ds$open()
        #' # Returns the opened dataset directly; no Mirai/Future to collect.
        #' ds$open(async = TRUE, timeout = 10)
        #' }
        open = function(async = FALSE, timeout = NULL) {
            private$validate_async_request(async, timeout)

            if (private$opened) {
                warning("Dataset is already open.")
                return(invisible(self))
            }

            if (!isTRUE(async)) {
                private$open_handles()
                return(invisible(self))
            }

            task <- private$start_async_operation(
                operation = "open OPeNDAP connection",
                handler = function(urls, nc_handles) TRUE,
                timeout = timeout
            )
            private$collect_async_task(task)

            # NOTE: Worker-owned handles cannot be transferred back to the
            # caller, so a successful async probe must be followed by a local
            # handoff reopen to preserve the public `open()` post-condition.
            # The async timeout only applies to the worker probe above; the
            # caller-owned reopen below intentionally runs outside that timeout.
            private$open_handles()

            invisible(self)
        },
        # }}}

        # close {{{
        #' @description
        #' Close OPeNDAP connection(s)
        #'
        #' @return The `EsgDataset` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' ds$close()
        #' }
        close = function() {
            private$cancel_async_task()
            private$close_handles()
            invisible(self)
        },
        # }}}

        # file_inq {{{
        #' @description
        #' Get file information
        #'
        #' @param index File index for multi-file datasets. Default: `1L`.
        #'
        #' @return A list with file information.
        #'
        #' @examples
        #' \dontrun{
        #' info <- ds$file_inq()
        #' }
        file_inq = function(index = 1L) {
            private$check_open()
            private$check_index(index)
            RNetCDF::file.inq.nc(private$nc_handles[[index]])
        },
        # }}}

        # var_inq {{{
        #' @description
        #' Get variable information
        #'
        #' @param var Variable name or ID.
        #' @param index File index for multi-file datasets. Default: `1L`.
        #'
        #' @return A list with variable information.
        #'
        #' @examples
        #' \dontrun{
        #' var_info <- ds$var_inq("tas")
        #' }
        var_inq = function(var, index = 1L) {
            private$check_open()
            private$check_index(index)
            RNetCDF::var.inq.nc(private$nc_handles[[index]], var)
        },
        # }}}

        # dim_inq {{{
        #' @description
        #' Get dimension information
        #'
        #' @param dim Dimension name or ID.
        #' @param index File index for multi-file datasets. Default: `1L`.
        #'
        #' @return A list with dimension information.
        #'
        #' @examples
        #' \dontrun{
        #' dim_info <- ds$dim_inq("time")
        #' }
        dim_inq = function(dim, index = 1L) {
            private$check_open()
            private$check_index(index)
            RNetCDF::dim.inq.nc(private$nc_handles[[index]], dim)
        },
        # }}}

        # att_get {{{
        #' @description
        #' Get attribute value
        #'
        #' @param var Variable name or ID, or "NC_GLOBAL" for global attributes.
        #' @param att Attribute name.
        #' @param index File index for multi-file datasets. Default: `1L`.
        #'
        #' @return The attribute value.
        #'
        #' @examples
        #' \dontrun{
        #' units <- ds$att_get("tas", "units")
        #' }
        att_get = function(var, att, index = 1L) {
            private$check_open()
            private$check_index(index)
            RNetCDF::att.get.nc(private$nc_handles[[index]], var, att)
        },
        # }}}

        # var_get {{{
        #' @description
        #' Read variable data
        #'
        #' @param var Variable name or ID.
        #' @param start Starting indices (1-based). If `NULL`, starts from beginning.
        #' @param count Number of values to read. If `NULL`, reads all.
        #' @param index File index for multi-file datasets. Default: `1L`.
        #' @param collapse Whether to collapse result. Default: `FALSE`.
        #' @param async If `TRUE`, perform the variable read in a one-shot
        #'        worker and return the final array directly once complete.
        #'        No `Mirai`/`Future` object is exposed. Default: `FALSE`.
        #' @param timeout Optional positive number of seconds for the async
        #'        worker phase. Only supported when `async = TRUE`.
        #'
        #' @return An array with variable data.
        #'
        #' @examples
        #' \dontrun{
        #' data <- ds$var_get("tas")
        #' data_subset <- ds$var_get("tas", start = c(1, 1, 1), count = c(10, 10, 1))
        #' # Returns the final array directly; no Mirai/Future handling required.
        #' data_async <- ds$var_get("tas", async = TRUE, timeout = 10)
        #' }
        var_get = function(var, start = NULL, count = NULL, index = 1L, collapse = FALSE,
                           async = FALSE, timeout = NULL) {
            private$validate_async_request(async, timeout)
            private$check_open()
            private$check_index(index)

            if (isTRUE(async)) {
                return(private$async_var_get(
                    var = var,
                    start = start,
                    count = count,
                    index = index,
                    collapse = collapse,
                    timeout = timeout
                ))
            }

            if (is.null(start) && is.null(count)) {
                RNetCDF::var.get.nc(private$nc_handles[[index]], var, collapse = collapse)
            } else {
                RNetCDF::var.get.nc(private$nc_handles[[index]], var, start = start, count = count, collapse = collapse)
            }
        },
        # }}}

        # get_variables {{{
        #' @description
        #' List all variables in the dataset
        #'
        #' @param index File index for multi-file datasets. Default: `1L`.
        #'
        #' @return A character vector of variable names.
        #'
        #' @examples
        #' \dontrun{
        #' vars <- ds$get_variables()
        #' }
        get_variables = function(index = 1L) {
            private$check_open()
            private$check_index(index)

            info <- self$file_inq(index)
            vars <- character(info$nvars)
            for (i in seq_len(info$nvars)) {
                var_info <- RNetCDF::var.inq.nc(private$nc_handles[[index]], i - 1L)
                vars[i] <- var_info$name
            }
            vars
        },
        # }}}

        # get_dimensions {{{
        #' @description
        #' List all dimensions in the dataset
        #'
        #' @param index File index for multi-file datasets. Default: `1L`.
        #'
        #' @return A character vector of dimension names.
        #'
        #' @examples
        #' \dontrun{
        #' dims <- ds$get_dimensions()
        #' }
        get_dimensions = function(index = 1L) {
            private$check_open()
            private$check_index(index)

            info <- self$file_inq(index)
            dims <- character(info$ndims)
            for (i in seq_len(info$ndims)) {
                dim_info <- RNetCDF::dim.inq.nc(private$nc_handles[[index]], i - 1L)
                dims[i] <- dim_info$name
            }
            dims
        },
        # }}}

        # get_time_axis {{{
        #' @description
        #' Get time axis information
        #'
        #' @param index File index for multi-file datasets. Default: `1L`.
        #'
        #' @return A list containing time values, units, and calendar.
        #'
        #' @examples
        #' \dontrun{
        #' time_info <- ds$get_time_axis()
        #' }
        get_time_axis = function(index = 1L) {
            private$check_open()
            private$check_index(index)

            # Check if already cached
            cache_key <- sprintf("time_axis_%d", index)
            if (!is.null(private$metadata_cache[[cache_key]])) {
                return(private$metadata_cache[[cache_key]])
            }

            # Get time dimension info
            time_dim <- self$dim_inq("time", index)

            # Get time attributes
            time_units <- self$att_get("time", "units", index)

            time_calendar <- tryCatch(
                self$att_get("time", "calendar", index),
                error = function(e) "standard"
            )

            # Get and parse time variable data
            time_vals <- parse_cf_time(
                self$var_get("time", index = index),
                time_units,
                time_calendar
            )

            result <- list(
                values = time_vals,
                units = time_units,
                calendar = normalize_cf_calendar(time_calendar),
                length = time_dim$length
            )

            # Cache the result
            private$metadata_cache[[cache_key]] <- result
            result
        },
        # }}}

        # get_spatial_grid {{{
        #' @description
        #' Get spatial grid information (latitude and longitude)
        #'
        #' @param index File index for multi-file datasets. Default: `1L`.
        #'
        #' @return A list containing latitude and longitude values.
        #'
        #' @examples
        #' \dontrun{
        #' grid <- ds$get_spatial_grid()
        #' }
        get_spatial_grid = function(index = 1L) {
            private$check_open()
            private$check_index(index)

            # Check if already cached
            cache_key <- sprintf("spatial_grid_%d", index)
            if (!is.null(private$metadata_cache[[cache_key]])) {
                return(private$metadata_cache[[cache_key]])
            }

            # Get lat/lon data
            lat <- tryCatch(
                self$var_get("lat", index = index),
                error = function(e) NULL
            )

            lon <- tryCatch(
                self$var_get("lon", index = index),
                error = function(e) NULL
            )

            result <- list(
                lat = lat,
                lon = lon
            )

            # Cache the result
            private$metadata_cache[[cache_key]] <- result
            result
        },
        # }}}

        # read_array {{{
        #' @description
        #' Read variable data as a list of arrays (one per file)
        #'
        #' @param variable Variable name.
        #' @param start Starting indices. If `NULL`, starts from beginning.
        #' @param count Number of values to read. If `NULL`, reads all.
        #' @param collapse Whether to collapse result. Default: `FALSE`.
        #' @param async If `TRUE`, read array values in a one-shot worker and
        #'        return the final list directly once complete. No
        #'        `Mirai`/`Future` object is exposed. Default: `FALSE`.
        #' @param timeout Optional positive number of seconds for the async
        #'        worker phase. Only supported when `async = TRUE`.
        #'
        #' @return A list of arrays with variable data. Each element
        #' corresponds to a file in the dataset.
        #'
        #' @examples
        #' \dontrun{
        #' data_list <- ds$read_array("tas")
        #' data <- data_list[[1]]
        #' # Returns the final list directly; no Mirai/Future handling required.
        #' data_list_async <- ds$read_array("tas", async = TRUE, timeout = 10)
        #' }
        read_array = function(variable, start = NULL, count = NULL, collapse = FALSE,
                              async = FALSE, timeout = NULL) {
            private$validate_async_request(async, timeout)
            private$check_open()

            if (isTRUE(async)) {
                return(private$async_read_array(
                    variable = variable,
                    start = start,
                    count = count,
                    collapse = collapse,
                    timeout = timeout
                ))
            }

            lapply(seq_along(private$urls), function(i) {
                self$var_get(variable, start = start, count = count, index = i, collapse = collapse)
            })
        },
        # }}}

        # read_data_table {{{
        #' @description
        #' Read variable data as a list of data.table (one per file)
        #'
        #' @param variable Variable name.
        #' @param start Starting indices. If `NULL`, starts from beginning.
        #' @param count Number of values to read. If `NULL`, reads all.
        #' @param rbind If `TRUE`, return a single data.table by row-binding
        #' the per-file results with `data.table::rbindlist(..., idcol = "file_index")`.
        #' Default: `FALSE`.
        #' @param async If `TRUE`, offload the array read phase to a one-shot
        #'        worker and still return the final data.table result directly.
        #'        No `Mirai`/`Future` object is exposed. Default: `FALSE`.
        #' @param timeout Optional positive number of seconds for the async
        #'        worker phase. Only supported when `async = TRUE`.
        #'
        #' @return If `rbind = FALSE`, a list of data.table (one per file).
        #' If `rbind = TRUE`, a single data.table with an extra `file_index` column.
        #'
        #' @examples
        #' \dontrun{
        #' dt_list <- ds$read_data_table("tas")
        #' dt <- dt_list[[1]]
        #' dt_all <- ds$read_data_table("tas", rbind = TRUE)
        #' # Returns the final data.table directly; no Mirai/Future handling required.
        #' dt_async <- ds$read_data_table("tas", async = TRUE, timeout = 10)
        #' }
        read_data_table = function(variable, start = NULL, count = NULL, rbind = FALSE,
                                   async = FALSE, timeout = NULL) {
            checkmate::assert_flag(rbind)
            private$validate_async_request(async, timeout)
            private$check_open()

            # Always keep dimensions for table output
            arr_list <- self$read_array(
                variable,
                start = start,
                count = count,
                collapse = FALSE,
                async = async,
                timeout = timeout
            )
            dt_list <- lapply(seq_along(arr_list), function(i) {
                private$array_to_data_table(
                    arr_list[[i]],
                    variable,
                    start = start,
                    count = count,
                    index = i
                )
            })

            if (!isTRUE(rbind)) {
                return(dt_list)
            }

            dt <- data.table::rbindlist(dt_list, use.names = TRUE, fill = TRUE, idcol = "file_index")
            if ("file_index" %in% names(dt) && is.character(dt[["file_index"]])) {
                dt[["file_index"]] <- as.integer(dt[["file_index"]])
            }
            dt
        },
        # }}}

        # read_region {{{
        #' @description
        #' Read variable values near a target coordinate and optional time range
        #'
        #' @param variable Character vector of variable names.
        #' @param lon Target longitude.
        #' @param lat Target latitude.
        #' @param time Optional length-2 time range. Character, `Date`, and
        #'        `POSIXt` inputs are accepted and parsed in UTC.
        #' @param nearest Number of nearest grid cells to keep. Default: `1L`.
        #' @param rbind If `TRUE`, return one data.table. If `FALSE`, return a
        #'        list of per-file, per-variable data.tables. Default: `TRUE`.
        #' @param async If `TRUE`, offload each NetCDF variable read to a
        #'        one-shot worker. Default: `FALSE`.
        #' @param timeout Optional positive number of seconds for each async
        #'        read. Only supported when `async = TRUE`.
        #'
        #' @return A data.table or list of data.tables with columns including
        #' `file_index`, `variable`, `time`, `lon`, `lat`, `dist`, and `value`.
        #'
        #' @examples
        #' \dontrun{
        #' dt <- ds$read_region(
        #'     variable = c("tas", "hurs"),
        #'     lon = 103.98,
        #'     lat = 1.37,
        #'     time = c("2050-01-01", "2050-12-31")
        #' )
        #' }
        read_region = function(variable, lon, lat, time = NULL, nearest = 1L,
                               rbind = TRUE, async = FALSE, timeout = NULL) {
            checkmate::assert_character(variable, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_number(lon, lower = -180, upper = 360, finite = TRUE)
            checkmate::assert_number(lat, lower = -90, upper = 90, finite = TRUE)
            checkmate::assert_int(nearest, lower = 1L)
            checkmate::assert_flag(rbind)
            private$validate_async_request(async, timeout)
            private$check_open()

            if (!is.null(time)) {
                if (length(time) != 2L) {
                    stop("`time` must be `NULL` or a length-2 range.", call. = FALSE)
                }
                time <- parse_datetime(time, tz = "UTC")
                if (any(is.na(time))) {
                    stop("`time` contains values that cannot be parsed as datetimes.", call. = FALSE)
                }
                if (time[[2L]] < time[[1L]]) {
                    stop("`time` end must be greater than or equal to `time` start.", call. = FALSE)
                }
            }

            pieces <- list()
            found <- stats::setNames(rep(FALSE, length(variable)), variable)

            for (i in seq_along(private$urls)) {
                for (var in variable) {
                    chunk <- private$read_region_one(
                        variable = var,
                        lon = lon,
                        lat = lat,
                        time = time,
                        nearest = nearest,
                        index = i,
                        async = async,
                        timeout = timeout
                    )
                    if (is.null(chunk)) {
                        next
                    }

                    found[[var]] <- TRUE
                    pieces[[length(pieces) + 1L]] <- chunk
                }
            }

            missing <- names(found)[!found]
            if (length(missing) == length(found)) {
                stop(sprintf(
                    "None of the requested variable(s) were found in the dataset: [%s].",
                    paste(sprintf("'%s'", missing), collapse = ", ")
                ), call. = FALSE)
            }
            if (length(missing)) {
                warning(sprintf(
                    "The following variable(s) were not found in any file and were skipped: [%s].",
                    paste(sprintf("'%s'", missing), collapse = ", ")
                ), call. = FALSE)
            }

            if (!length(pieces)) {
                empty <- private$empty_region_data_table()
                return(if (isTRUE(rbind)) empty else list())
            }
            if (!isTRUE(rbind)) {
                return(pieces)
            }

            data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
        },
        # }}}

        # print {{{
        #' @description
        #' Print dataset summary
        #'
        #' @return The `EsgDataset` object itself, invisibly.
        print = function() {
            cli::cli_h1("ESGF Dataset")
            cli::cli_li("URLs: {length(private$urls)}")
            cli::cli_li("Status: {if (private$opened) 'Open' else 'Closed'}")
            cli::cli_li("Multiple: {length(private$urls) > 1L}")

            if (private$opened) {
                vars <- tryCatch(self$get_variables(1L), error = function(e) character())
                dims <- tryCatch(self$get_dimensions(1L), error = function(e) character())

                cli::cli_h2("Dimensions")
                cli::cli_li("{paste(dims, collapse = ', ')}")

                cli::cli_h2("Variables")
                cli::cli_li("{paste(vars, collapse = ', ')}")
            }

            invisible(self)
        }
        # }}}
    ),

    active = list(
        # url {{{
        #' @field url The OPeNDAP URL(s)
        url = function() {
            private$urls
        },
        # }}}

        # is_open {{{
        #' @field is_open Whether the connection is open
        is_open = function() {
            private$opened
        },
        # }}}

        # is_aggregated {{{
        #' @field is_aggregated Whether the dataset contains multiple files
        is_aggregated = function() {
            length(private$urls) > 1L
        },
        # }}}

        # file_count {{{
        #' @field file_count Number of files in the dataset
        file_count = function() {
            length(private$urls)
        }
        # }}}
    ),

    private = list(
        urls = NULL,
        nc_handles = NULL,
        opened = FALSE,
        metadata_cache = NULL,
        async_task = NULL,
        async_state = "idle",
        async_sequence = 0L,

        # get_var_dim_meta {{
        # NOTE: `start`/`count` are in NetCDF dimension order.
        get_var_dim_meta = function(variable, index = 1L) {
            cache_key <- sprintf("var_dim_meta_%s_%d", variable, index)
            if (!is.null(private$metadata_cache[[cache_key]])) {
                return(private$metadata_cache[[cache_key]])
            }

            vinfo <- self$var_inq(variable, index = index)
            dimids <- vinfo$dimids
            dnames <- vapply(dimids, function(id) self$dim_inq(id, index = index)$name, character(1L))
            dlens <- vapply(dimids, function(id) as.integer(self$dim_inq(id, index = index)$length), integer(1L))

            res <- list(ids = dimids, names = dnames, lengths = dlens)
            private$metadata_cache[[cache_key]] <- res
            res
        },
        # }}}

        # infer_dim_perm {{
        # Infer how NetCDF dimension order maps to the returned R array order.
        # Returns an index vector `perm` such that:
        #   dim_names_in_array_order <- dim_names_in_netcdf_order[perm]
        infer_dim_perm = function(arr_dim, count = NULL, nc_lengths = NULL) {
            nd <- length(arr_dim)
            ref <- NULL
            if (!is.null(count)) {
                ref <- as.integer(count)
            } else if (!is.null(nc_lengths)) {
                ref <- as.integer(nc_lengths)
            }

            if (!is.null(ref) && length(ref) == nd) {
                arr_dim <- as.integer(arr_dim)
                if (identical(arr_dim, ref)) {
                    return(seq_len(nd))
                }
                if (identical(arr_dim, rev(ref))) {
                    return(rev(seq_len(nd)))
                }
            }

            warning(
                "Cannot infer dimension order between NetCDF metadata and returned R array; ",
                "assuming they match NetCDF order."
            )
            seq_len(nd)
        },
        # }}}

        # finalize {{{
        finalize = function() {
            self$close()
        },
        # }}}

        # validate_async_request {{{
        validate_async_request = function(async, timeout) {
            checkmate::assert_flag(async)
            if (!isTRUE(async) && !is.null(timeout)) {
                stop("`timeout` is only supported when `async = TRUE`.")
            }
            invisible(async)
        },
        # }}}

        # open_handles {{{
        open_handles = function() {
            tryCatch(
                {
                    for (i in seq_along(private$urls)) {
                        if (!is.null(private$nc_handles[[i]])) {
                            next
                        }
                        private$nc_handles[[i]] <- RNetCDF::open.nc(private$urls[[i]])
                    }
                    private$opened <- TRUE
                },
                error = function(e) {
                    private$close_handles()
                    stop(sprintf("Failed to open OPeNDAP connection: %s", conditionMessage(e)))
                }
            )

            invisible(NULL)
        },
        # }}}

        # close_handles {{{
        close_handles = function() {
            for (i in seq_along(private$nc_handles)) {
                if (!is.null(private$nc_handles[[i]])) {
                    tryCatch(
                        RNetCDF::close.nc(private$nc_handles[[i]]),
                        error = function(e) NULL
                    )
                    # NOTE: Use single-bracket assignment to avoid shrinking the list.
                    private$nc_handles[i] <- list(NULL)
                }
            }
            private$opened <- FALSE
            invisible(NULL)
        },
        # }}}

        # normalize_async_timeout {{{
        normalize_async_timeout = function(timeout) {
            if (is.null(timeout)) {
                return(NULL)
            }

            timeout <- as.numeric(timeout)
            if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
                stop("`timeout` must be a single positive number of seconds.")
            }

            as.integer(ceiling(timeout * 1000))
        },
        # }}}

        # next_async_compute_profile {{{
        next_async_compute_profile = function() {
            private$async_sequence <- private$async_sequence + 1L
            sprintf("epwshiftr.esgdataset.%d.%d", Sys.getpid(), private$async_sequence)
        },
        # }}}

        # start_async_operation {{{
        start_async_operation = function(operation, handler, handler_args = list(), timeout = NULL) {
            checkmate::assert_string(operation, min.chars = 1L)
            checkmate::assert_function(handler)
            if (!is.list(handler_args)) {
                stop("`handler_args` must be a list.")
            }

            if (!is.null(private$async_task) && identical(private$async_state, "running")) {
                stop("An async task is already running for this dataset.")
            }

            timeout_ms <- private$normalize_async_timeout(timeout)
            compute_profile <- private$next_async_compute_profile()
            mirai::daemons(1L, dispatcher = TRUE, .compute = compute_profile)

            mirai_obj <- mirai::mirai(
                {
                    handles <- vector("list", length(urls))
                    on.exit(
                        {
                            for (i in seq_along(handles)) {
                                if (!is.null(handles[[i]])) {
                                    try(RNetCDF::close.nc(handles[[i]]), silent = TRUE)
                                }
                            }
                        },
                        add = TRUE
                    )

                    for (i in seq_along(urls)) {
                        handles[[i]] <- RNetCDF::open.nc(urls[[i]])
                    }

                    do.call(handler, c(list(urls = urls, nc_handles = handles), handler_args))
                },
                urls = private$urls,
                handler = handler,
                handler_args = handler_args,
                .timeout = timeout_ms,
                .compute = compute_profile
            )

            task <- DatasetAsyncTask$new(
                operation = operation,
                mirai_obj = mirai_obj,
                compute_profile = compute_profile,
                timeout_ms = timeout_ms
            )
            private$async_task <- task
            private$async_state <- "running"
            task
        },
        # }}}

        # collect_async_task {{{
        collect_async_task = function(task = private$async_task, clear = TRUE) {
            if (is.null(task)) {
                stop("No async task is registered for this dataset.")
            }

            on.exit(
                {
                    private$async_state <- task$status
                    if (isTRUE(clear) && identical(private$async_task, task)) {
                        private$async_task <- NULL
                    }
                },
                add = TRUE
            )

            task$collect()
        },
        # }}}

        # cancel_async_task {{{
        cancel_async_task = function(task = private$async_task, clear = TRUE) {
            if (is.null(task)) {
                return(FALSE)
            }

            requested <- task$cancel()
            private$async_state <- task$status
            if (isTRUE(clear) && identical(private$async_task, task)) {
                private$async_task <- NULL
            }
            requested
        },
        # }}}

        # async_var_get {{{
        async_var_get = function(var, start = NULL, count = NULL, index = 1L,
                                 collapse = FALSE, timeout = NULL) {
            task <- private$start_async_operation(
                operation = "read variable data",
                handler = function(urls, nc_handles, var, start, count, index, collapse) {
                    if (is.null(start) && is.null(count)) {
                        return(RNetCDF::var.get.nc(nc_handles[[index]], var, collapse = collapse))
                    }

                    RNetCDF::var.get.nc(
                        nc_handles[[index]],
                        var,
                        start = start,
                        count = count,
                        collapse = collapse
                    )
                },
                handler_args = list(
                    var = var,
                    start = start,
                    count = count,
                    index = index,
                    collapse = collapse
                ),
                timeout = timeout
            )

            private$collect_async_task(task)
        },
        # }}}

        # async_read_array {{{
        async_read_array = function(variable, start = NULL, count = NULL,
                                    collapse = FALSE, timeout = NULL) {
            task <- private$start_async_operation(
                operation = "read variable data",
                handler = function(urls, nc_handles, variable, start, count, collapse) {
                    lapply(seq_along(nc_handles), function(i) {
                        if (is.null(start) && is.null(count)) {
                            return(RNetCDF::var.get.nc(nc_handles[[i]], variable, collapse = collapse))
                        }

                        RNetCDF::var.get.nc(
                            nc_handles[[i]],
                            variable,
                            start = start,
                            count = count,
                            collapse = collapse
                        )
                    })
                },
                handler_args = list(
                    variable = variable,
                    start = start,
                    count = count,
                    collapse = collapse
                ),
                timeout = timeout
            )

            private$collect_async_task(task)
        },
        # }}}

        # check_open {{{
        check_open = function() {
            if (!private$opened) {
                stop("Dataset is not open. Call $open() first.")
            }
        },
        # }}}

        # check_index {{{
        check_index = function(index) {
            if (index < 1L || index > length(private$urls)) {
                stop(sprintf("Invalid index %d. Must be between 1 and %d.", index, length(private$urls)))
            }
        },
        # }}}

        # empty_region_data_table {{{
        empty_region_data_table = function() {
            data.table::data.table(
                file_index = integer(),
                variable = character(),
                time = as.POSIXct(character(), tz = "UTC"),
                lon = numeric(),
                lat = numeric(),
                dist = numeric(),
                value = numeric()
            )
        },
        # }}}

        # normalize_lon_for_grid {{{
        normalize_lon_for_grid = function(lon, grid_lon) {
            grid_lon <- grid_lon[!is.na(grid_lon)]
            if (!length(grid_lon)) {
                return(lon)
            }
            if (min(grid_lon) >= 0 && lon < 0) {
                return((lon + 360) %% 360)
            }
            if (max(grid_lon) <= 180 && lon > 180) {
                return(((lon + 180) %% 360) - 180)
            }
            lon
        },
        # }}}

        # read_region_one {{{
        read_region_one = function(variable, lon, lat, time, nearest, index,
                                   async = FALSE, timeout = NULL) {
            meta <- tryCatch(
                private$get_var_dim_meta(variable, index = index),
                error = function(e) NULL
            )
            if (is.null(meta)) {
                return(NULL)
            }

            required_dims <- c("time", "lat", "lon")
            missing_dims <- setdiff(required_dims, meta$names)
            if (length(missing_dims)) {
                stop(sprintf(
                    "Variable '%s' in file index %d is missing required dimension(s): [%s].",
                    variable,
                    index,
                    paste(sprintf("'%s'", missing_dims), collapse = ", ")
                ), call. = FALSE)
            }

            extra_dims <- setdiff(meta$names, required_dims)
            if (length(extra_dims)) {
                extra_lengths <- meta$lengths[match(extra_dims, meta$names)]
                if (any(extra_lengths != 1L)) {
                    stop(sprintf(
                        "Variable '%s' in file index %d has unsupported non-spatiotemporal dimension(s): [%s].",
                        variable,
                        index,
                        paste(sprintf("'%s'", extra_dims[extra_lengths != 1L]), collapse = ", ")
                    ), call. = FALSE)
                }
            }

            time_axis <- self$get_time_axis(index = index)$values
            time_idx <- if (is.null(time)) {
                seq_along(time_axis)
            } else {
                base::which(time_axis >= time[[1L]] & time_axis <= time[[2L]])
            }
            if (!length(time_idx)) {
                return(private$empty_region_data_table())
            }

            grid <- self$get_spatial_grid(index = index)
            if (is.null(grid$lat) || is.null(grid$lon)) {
                stop(sprintf("File index %d does not expose both 'lat' and 'lon' coordinate variables.", index), call. = FALSE)
            }
            grid_lat <- as.vector(grid$lat)
            grid_lon <- as.vector(grid$lon)
            target_lon <- private$normalize_lon_for_grid(lon, grid_lon)

            coords <- data.table::CJ(
                ind_lat = seq_along(grid_lat),
                ind_lon = seq_along(grid_lon)
            )
            coords[, `:=`(
                lat = grid_lat[ind_lat],
                lon = grid_lon[ind_lon],
                dist = tunnel_dist(grid_lat[ind_lat], grid_lon[ind_lon], lat, target_lon)
            )]
            data.table::setorder(coords, dist)
            coords <- coords[seq_len(min(nearest, .N))]

            dim_index <- function(name) match(name, meta$names)
            selected <- list(
                time = time_idx,
                lat = coords$ind_lat,
                lon = coords$ind_lon
            )

            start <- rep(1L, length(meta$names))
            count <- meta$lengths
            for (name in names(selected)) {
                k <- dim_index(name)
                idx <- selected[[name]]
                start[[k]] <- min(idx)
                count[[k]] <- max(idx) - min(idx) + 1L
            }
            for (name in extra_dims) {
                k <- dim_index(name)
                start[[k]] <- 1L
                count[[k]] <- 1L
            }

            arr <- self$var_get(
                variable,
                start = start,
                count = count,
                index = index,
                collapse = FALSE,
                async = async,
                timeout = timeout
            )
            dt <- private$array_to_data_table(arr, variable, start = start, count = count, index = index)

            if (!nrow(dt)) {
                return(private$empty_region_data_table())
            }

            coords_keep <- coords[, .(lat, lon, dist)]
            dt <- merge(dt, coords_keep, by = c("lat", "lon"), all = FALSE, sort = FALSE)
            dt <- dt[time %in% time_axis[time_idx]]
            if (!nrow(dt)) {
                return(private$empty_region_data_table())
            }

            value_col <- if (variable %in% names(dt)) variable else paste0(variable, "_value")
            if (!value_col %in% names(dt)) {
                stop(sprintf("Cannot identify value column for variable '%s'.", variable), call. = FALSE)
            }
            data.table::setnames(dt, value_col, "value")
            dt[, `:=`(file_index = index, variable = variable)]
            data.table::setcolorder(dt, c(
                "file_index", "variable",
                intersect(c("time", "lon", "lat", "dist"), names(dt)),
                setdiff(names(dt), c("file_index", "variable", "time", "lon", "lat", "dist"))
            ))
            dt[]
        },
        # }}}

        # array_to_data_table {{{
        array_to_data_table = function(arr, variable, start = NULL, count = NULL, index = 1L) {
            meta <- private$get_var_dim_meta(variable, index = index)
            if (length(meta$names) == 0L) {
                dt <- data.table::data.table(val = as.vector(arr))
                data.table::setnames(dt, "val", variable)
                return(dt)
            }
            dim_names_nc <- make.unique(meta$names, sep = "_")

            # Ensure `arr` is treated as an array
            arr_dim <- dim(arr)
            if (is.null(arr_dim)) {
                arr_dim <- length(arr)
                dim(arr) <- arr_dim
            }

            nd <- length(arr_dim)
            if (length(meta$names) != nd) {
                stop("Dimension mismatch between NetCDF metadata and returned R array. Try using `collapse = FALSE`.")
            }

            perm <- private$infer_dim_perm(arr_dim, count = count, nc_lengths = meta$lengths)
            dim_names_arr <- dim_names_nc[perm]

            starts_nc <- if (is.null(start)) rep(1L, length(meta$names)) else as.integer(start)
            if (length(starts_nc) != length(meta$names)) {
                stop("`start` must have the same length as the variable's number of dimensions.")
            }
            starts_arr <- starts_nc[perm]
            count_nc <- if (is.null(count)) NULL else as.integer(count)
            if (!is.null(count_nc) && length(count_nc) != length(meta$names)) {
                stop("`count` must have the same length as the variable's number of dimensions.")
            }

            time_axis <- NULL
            if ("time" %in% meta$names) {
                time_axis <- self$get_time_axis(index = index)$values
            }

            # Build coordinates from linear index without assuming dimension count
            idx_mat <- arrayInd(seq_len(length(arr)), .dim = arr_dim)
            dt <- data.table::as.data.table(idx_mat)
            data.table::setnames(dt, dim_names_arr)

            # Replace indices with actual coordinate values when possible.
            # For each dimension name `dname`, try to read coordinate variable `self$var_get(dname, ...)`.
            for (j in seq_along(dim_names_arr)) {
                col <- dim_names_arr[[j]]
                k <- perm[[j]]
                dname_nc <- meta$names[[k]]

                start_k <- starts_nc[[k]]
                len_k <- as.integer(arr_dim[[j]])
                coord <- NULL

                if (identical(dname_nc, "time") && !is.null(time_axis)) {
                    coord_idx <- seq.int(from = start_k, length.out = len_k)
                    coord <- time_axis[coord_idx]
                } else {
                    coord <- tryCatch(
                        self$var_get(dname_nc, start = start_k, count = len_k, index = index, collapse = TRUE),
                        error = function(e) NULL
                    )
                    if (!is.null(coord)) {
                        coord <- as.vector(coord)
                    }
                }

                if (!is.null(coord) && length(coord) == as.integer(arr_dim[[j]])) {
                    dt[[col]] <- coord[idx_mat[, j]]
                } else {
                    # Fallback to 1-based indices in original NetCDF space
                    dt[[col]] <- as.integer(dt[[col]]) + starts_arr[[j]] - 1L
                }
            }

            value_name <- variable
            if (value_name %in% dim_names_arr) {
                value_name <- paste0(variable, "_value")
                warning(sprintf(
                    "Value column name '%s' conflicts with a dimension name; using '%s'.",
                    variable,
                    value_name
                ))
            }
            dt[[value_name]] <- as.vector(arr)

            # Reorder columns to NetCDF dimension order when possible
            keep_dims <- intersect(dim_names_nc, names(dt))
            data.table::setcolorder(dt, c(keep_dims, value_name))

            dt
        }
        # }}}
    )
)
# }}}

# esg_dataset_private {{{
esg_dataset_private <- function(dataset) {
    private <- tryCatch(dataset$.__enclos_env__$private, error = function(e) NULL)
    if (is.null(private) || !is.environment(private)) {
        stop("`dataset` must be an EsgDataset-like object.", call. = FALSE)
    }
    missing <- setdiff(c("nc_handles", "opened"), names(private))
    if (length(missing)) {
        stop(sprintf(
            "`dataset` is missing required internal field(s): %s.",
            paste(sprintf("`%s`", missing), collapse = ", ")
        ), call. = FALSE)
    }
    if (!is.list(private$nc_handles)) {
        stop("`dataset` internal `nc_handles` field must be a list.", call. = FALSE)
    }

    private
}
# }}}

# esg_dataset_detach_handles {{{
esg_dataset_detach_handles <- function(dataset) {
    private <- esg_dataset_private(dataset)
    handles <- private$nc_handles
    private$nc_handles <- vector("list", length(handles))
    private$opened <- FALSE
    handles
}
# }}}

# esg_dataset_adopt_handles {{{
esg_dataset_adopt_handles <- function(dataset, handles) {
    checkmate::assert_list(handles)

    private <- esg_dataset_private(dataset)
    if (length(handles) != length(private$nc_handles)) {
        stop("`handles` must have the same length as the target dataset.", call. = FALSE)
    }

    private$nc_handles <- handles
    private$opened <- all(!vapply(private$nc_handles, is.null, logical(1L)))

    invisible(dataset)
}
# }}}

# esg_dataset_close_handles {{{
esg_dataset_close_handles <- function(urls, handles) {
    checkmate::assert_character(urls, any.missing = FALSE)
    checkmate::assert_list(handles)
    if (length(urls) != length(handles)) {
        stop("`urls` and `handles` must have the same length.", call. = FALSE)
    }
    if (!length(handles) || all(vapply(handles, is.null, logical(1L)))) {
        return(invisible(NULL))
    }

    holder <- EsgDataset$new(urls)
    esg_dataset_adopt_handles(holder, handles)
    holder$close()
    invisible(NULL)
}
# }}}
