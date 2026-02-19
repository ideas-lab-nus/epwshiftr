# EsgDataset {{{
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
EsgDataset <- R6::R6Class("EsgDataset",
    lock_class = TRUE,
    public = list(
        # initialize {{{
        #' @description
        #' Create a new EsgDataset object
        #'
        #' @param urls A character vector of OPeNDAP URLs. Can be a single URL
        #'        or multiple URLs for aggregation.
        #'
        #' @param aggregate Logical. If `TRUE` and multiple URLs are provided,
        #'        attempt to aggregate files into a single dataset. Default: `TRUE`.
        #'
        #' @return An `EsgDataset` object.
        #'
        #' @examples
        #' \dontrun{
        #' # Single file
        #' ds <- EsgDataset$new("https://example.com/data.nc")
        #'
        #' # Multiple files with aggregation
        #' ds <- EsgDataset$new(c("url1.nc", "url2.nc"), aggregate = TRUE)
        #' }
        initialize = function(urls, aggregate = TRUE) {
            checkmate::assert_character(urls, any.missing = FALSE, min.len = 1L)
            checkmate::assert_flag(aggregate)

            private$urls <- urls
            private$should_aggregate <- aggregate && length(urls) > 1L
            private$opened <- FALSE
            private$metadata_cache <- list()

            # Initialize file handles list
            private$nc_handles <- vector("list", length(urls))

            self
        },
        # }}}

        # open {{{
        #' @description
        #' Open OPeNDAP connection(s)
        #'
        #' @return The `EsgDataset` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' ds$open()
        #' }
        open = function() {
            if (private$opened) {
                cli::cli_warn("Dataset is already open.")
                return(invisible(self))
            }

            tryCatch(
                {
                    for (i in seq_along(private$urls)) {
                        private$nc_handles[[i]] <- RNetCDF::open.nc(private$urls[[i]])
                    }
                    private$opened <- TRUE

                    # If aggregation is requested, validate and prepare
                    if (private$should_aggregate) {
                        private$validate_aggregation()
                    }
                },
                error = function(e) {
                    # Clean up any opened connections
                    self$close()
                    cli::cli_abort("Failed to open OPeNDAP connection: {e$message}")
                }
            )

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
            for (i in seq_along(private$nc_handles)) {
                if (!is.null(private$nc_handles[[i]])) {
                    tryCatch(
                        RNetCDF::close.nc(private$nc_handles[[i]]),
                        error = function(e) NULL
                    )
                    private$nc_handles[[i]] <- NULL
                }
            }
            private$opened <- FALSE
            invisible(self)
        },
        # }}}

        # file_inq {{{
        #' @description
        #' Get file information
        #'
        #' Wraps [RNetCDF::file.inq.nc()].
        #'
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
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
        #' Wraps [RNetCDF::var.inq.nc()].
        #'
        #' @param var Variable name or ID.
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
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
        #' Wraps [RNetCDF::dim.inq.nc()].
        #'
        #' @param dim Dimension name or ID.
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
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
        #' Wraps [RNetCDF::att.get.nc()].
        #'
        #' @param var Variable name or ID, or `"NC_GLOBAL"` for global attributes.
        #' @param att Attribute name.
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
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
        #' Wraps [RNetCDF::var.get.nc()].
        #'
        #' @param var Variable name or ID.
        #' @param start Integer vector. Starting indices (1-based). If `NULL`,
        #'        starts from beginning. Default: `NULL`.
        #' @param count Integer vector. Number of values to read along each
        #'        dimension. If `NULL`, reads all. Default: `NULL`.
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
        #' @param collapse Logical. Whether to collapse degenerate dimensions.
        #'        Default: `TRUE`.
        #'
        #' @return An array with variable data.
        #'
        #' @examples
        #' \dontrun{
        #' data <- ds$var_get("tas")
        #' data_subset <- ds$var_get("tas", start = c(1, 1, 1), count = c(10, 10, 1))
        #' }
        var_get = function(var, start = NULL, count = NULL, index = 1L, collapse = TRUE) {
            private$check_open()
            private$check_index(index)

            if (is.null(start) && is.null(count)) {
                RNetCDF::var.get.nc(private$nc_handles[[index]], var, collapse = collapse)
            } else {
                RNetCDF::var.get.nc(private$nc_handles[[index]], var,
                    start = start, count = count, collapse = collapse
                )
            }
        },
        # }}}

        # get_variables {{{
        #' @description
        #' List all variables in the dataset
        #'
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
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
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
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
        #' Results are cached in an internal metadata cache for subsequent calls.
        #'
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
        #'
        #' @return A list containing:
        #' - `values`: Numeric vector of time values
        #' - `units`: Character string of time units
        #' - `calendar`: Character string of calendar type
        #' - `length`: Integer length of the time dimension
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

            # Get time variable data
            time_vals <- self$var_get("time", index = index)

            # Get time attributes
            time_units <- tryCatch(
                self$att_get("time", "units", index),
                error = function(e) "unknown"
            )

            time_calendar <- tryCatch(
                self$att_get("time", "calendar", index),
                error = function(e) "standard"
            )

            result <- list(
                values = time_vals,
                units = time_units,
                calendar = time_calendar,
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
        #' Results are cached in an internal metadata cache for subsequent calls.
        #'
        #' @param index Integer. File index for aggregated datasets. Default: `1L`.
        #'
        #' @return A list containing:
        #' - `lat`: Numeric vector of latitude values, or `NULL` if not found
        #' - `lon`: Numeric vector of longitude values, or `NULL` if not found
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
        #' Read variable data as an array
        #'
        #' If the dataset is aggregated and `aggregate = TRUE`, data from all
        #' files is concatenated along the time dimension (first dimension)
        #' using [abind::abind()].
        #'
        #' @param variable Character. Variable name.
        #' @param start Integer vector. Starting indices. If `NULL`, starts from
        #'        beginning. Default: `NULL`.
        #' @param count Integer vector. Number of values to read. If `NULL`,
        #'        reads all. Default: `NULL`.
        #' @param aggregate Logical. If `TRUE` and dataset is aggregated,
        #'        concatenate data across files along time dimension.
        #'        Default: `TRUE`.
        #'
        #' @return An array with variable data.
        #'
        #' @examples
        #' \dontrun{
        #' data <- ds$read_array("tas")
        #' }
        read_array = function(variable, start = NULL, count = NULL, aggregate = TRUE) {
            private$check_open()
            checkmate::assert_string(variable)
            checkmate::assert_flag(aggregate)

            if (!private$should_aggregate || !aggregate) {
                # Single file read
                return(self$var_get(variable, start = start, count = count, index = 1L))
            }

            # Aggregated read
            private$read_aggregated(variable, start, count)
        },
        # }}}

        # read_data_table {{{
        #' @description
        #' Read variable data as a [data.table][data.table::data.table()]
        #'
        #' Reads the variable as an array and converts it to long format.
        #' For 3D arrays (time x lat x lon), the result has columns for
        #' each dimension index and the variable value.
        #'
        #' @param variable Character. Variable name.
        #' @param start Integer vector. Starting indices. If `NULL`, starts from
        #'        beginning. Default: `NULL`.
        #' @param count Integer vector. Number of values to read. If `NULL`,
        #'        reads all. Default: `NULL`.
        #' @param aggregate Logical. If `TRUE` and dataset is aggregated,
        #'        concatenate data across files along time dimension.
        #'        Default: `TRUE`.
        #'
        #' @return A [data.table][data.table::data.table()] with variable data
        #'         in long format.
        #'
        #' @examples
        #' \dontrun{
        #' dt <- ds$read_data_table("tas")
        #' }
        read_data_table = function(variable, start = NULL, count = NULL, aggregate = TRUE) {
            arr <- self$read_array(variable, start, count, aggregate)
            private$array_to_data_table(arr, variable)
        },
        # }}}

        # print {{{
        #' @description
        #' Print dataset summary
        #'
        #' @return The `EsgDataset` object itself, invisibly.
        print = function() {
            d <- cli::cli_div(
                theme = list(rule = list("line-type" = "double"))
            )
            cli::cli_rule("EsgDataset")
            cli::cli_end(d)

            cli::cli_li("URLs: {.val {length(private$urls)}}")
            cli::cli_li("Status: {.field {if (private$opened) 'Open' else 'Closed'}}")
            cli::cli_li("Aggregated: {.val {private$should_aggregate}}")

            if (private$opened) {
                vars <- tryCatch(self$get_variables(1L), error = function(e) character())
                dims <- tryCatch(self$get_dimensions(1L), error = function(e) character())

                if (length(dims)) {
                    cli::cli_h2("Dimensions")
                    cli::cli_li("{paste(dims, collapse = ', ')}")
                }

                if (length(vars)) {
                    cli::cli_h2("Variables")
                    cli::cli_li("{paste(vars, collapse = ', ')}")
                }
            }

            invisible(self)
        }
        # }}}
    ),

    active = list(
        # url {{{
        #' @field url The OPeNDAP URL(s).
        url = function() {
            private$urls
        },
        # }}}

        # is_open {{{
        #' @field is_open Logical. Whether the connection is open.
        is_open = function() {
            private$opened
        },
        # }}}

        # is_aggregated {{{
        #' @field is_aggregated Logical. Whether the dataset is aggregated.
        is_aggregated = function() {
            private$should_aggregate
        },
        # }}}

        # file_count {{{
        #' @field file_count Integer. Number of files in the dataset.
        file_count = function() {
            length(private$urls)
        }
        # }}}
    ),

    private = list(
        urls = NULL,
        nc_handles = NULL,
        opened = FALSE,
        should_aggregate = FALSE,
        metadata_cache = NULL,
        aggregation_info = NULL,

        # finalize {{{
        finalize = function() {
            self$close()
        },
        # }}}

        # check_open {{{
        check_open = function() {
            if (!private$opened) {
                cli::cli_abort("Dataset is not open. Call {.code $open()} first.")
            }
        },
        # }}}

        # check_index {{{
        check_index = function(index) {
            checkmate::assert_int(index, lower = 1L, upper = length(private$urls))
        },
        # }}}

        # validate_aggregation {{{
        validate_aggregation = function() {
            # Check that all files have compatible dimensions and variables
            ref_dims <- self$get_dimensions(1L)
            ref_vars <- self$get_variables(1L)

            for (i in 2:length(private$urls)) {
                dims <- self$get_dimensions(i)
                vars <- self$get_variables(i)

                if (!setequal(dims, ref_dims)) {
                    cli::cli_warn(
                        "File {i} has different dimensions than file 1. Aggregation may fail."
                    )
                }

                if (!setequal(vars, ref_vars)) {
                    cli::cli_warn(
                        "File {i} has different variables than file 1. Some variables may be missing."
                    )
                }
            }

            # Get time axis for all files
            time_axes <- lapply(seq_along(private$urls), function(i) {
                self$get_time_axis(i)
            })

            private$aggregation_info <- list(
                time_axes = time_axes,
                validated = TRUE
            )

            invisible(NULL)
        },
        # }}}

        # read_aggregated {{{
        read_aggregated = function(variable, start, count) {
            if (!isTRUE(private$aggregation_info$validated)) {
                cli::cli_abort("Aggregation not validated. This should not happen.")
            }

            # Read data from all files
            data_list <- lapply(seq_along(private$urls), function(i) {
                self$var_get(variable, start = start, count = count, index = i)
            })

            # Concatenate along time dimension (assuming first dimension is time)
            do.call(abind::abind, c(data_list, list(along = 1)))
        },
        # }}}

        # array_to_data_table {{{
        array_to_data_table = function(arr, variable) {
            # Convert array to data.table in long format
            dims <- dim(arr)

            if (length(dims) == 3L) {
                # Assuming dimensions are time, lat, lon
                indices <- expand.grid(
                    time = seq_len(dims[1L]),
                    lat = seq_len(dims[2L]),
                    lon = seq_len(dims[3L])
                )

                dt <- data.table::as.data.table(indices)
                dt[[variable]] <- as.vector(arr)
                dt
            } else if (length(dims) == 2L) {
                indices <- expand.grid(
                    dim1 = seq_len(dims[1L]),
                    dim2 = seq_len(dims[2L])
                )

                dt <- data.table::as.data.table(indices)
                dt[[variable]] <- as.vector(arr)
                dt
            } else {
                # For 1D or other dimensions, just convert to data.table
                data.table::data.table(value = as.vector(arr))
            }
        }
        # }}}
    )
)
# }}}