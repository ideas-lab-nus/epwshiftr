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
        #'
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
                warning("Dataset is already open.")
                return(invisible(self))
            }

            tryCatch(
                {
                    for (i in seq_along(private$urls)) {
                        private$nc_handles[[i]] <- RNetCDF::open.nc(private$urls[[i]])
                    }
                    private$opened <- TRUE
                },
                error = function(e) {
                    # Clean up any opened connections
                    self$close()
                    stop(sprintf("Failed to open OPeNDAP connection: %s", e$message))
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
                    # NOTE: Use single-bracket assignment to avoid shrinking the list.
                    private$nc_handles[i] <- list(NULL)
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
        #'
        #' @return An array with variable data.
        #'
        #' @examples
        #' \dontrun{
        #' data <- ds$var_get("tas")
        #' data_subset <- ds$var_get("tas", start = c(1, 1, 1), count = c(10, 10, 1))
        #' }
        var_get = function(var, start = NULL, count = NULL, index = 1L, collapse = FALSE) {
            private$check_open()
            private$check_index(index)

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
        #'
        #' @return A list of arrays with variable data. Each element
        #' corresponds to a file in the dataset.
        #'
        #' @examples
        #' \dontrun{
        #' data_list <- ds$read_array("tas")
        #' data <- data_list[[1]]
        #' }
        read_array = function(variable, start = NULL, count = NULL, collapse = FALSE) {
            private$check_open()
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
        #'
        #' @return If `rbind = FALSE`, a list of data.table (one per file).
        #' If `rbind = TRUE`, a single data.table with an extra `file_index` column.
        #'
        #' @examples
        #' \dontrun{
        #' dt_list <- ds$read_data_table("tas")
        #' dt <- dt_list[[1]]
        #' dt_all <- ds$read_data_table("tas", rbind = TRUE)
        #' }
        read_data_table = function(variable, start = NULL, count = NULL, rbind = FALSE) {
            checkmate::assert_flag(rbind)
            private$check_open()

            # Always keep dimensions for table output
            arr_list <- self$read_array(variable, start = start, count = count, collapse = FALSE)
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
