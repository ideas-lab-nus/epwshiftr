# EsgResult {{{
#' Base class for results for ESGF query
#'
#' @description
#'
#' `EsgResult` is a base class that represents basic query results from
#' ESGF search RESTful API. It defines common fields and methods for results
#' from all query types, including `Dataset`, `File` and `Aggregation`. Results
#' from the three types are
#'
#' In general, there is no need to create an `EsgResult` manually.
#'
#' @author Hongyuan Jia
#' @name EsgResult
#' @keywords internal
EsgResult <- R6::R6Class(
    "EsgResult",
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # initialize {{{
        #' @description
        #' Create a new EsgResult object
        #'
        #' @param index_node The URL to the ESGF Index Node. It should be the
        #'        same as the `index_node` for an [EsgQuery] object that
        #'        collects the query results.
        #'
        #' @param params A list of query parameters.
        #'
        #' @param response The result of an query response.
        #'
        #' @param context Optional saved-result context. Used internally for
        #'        result-level metadata such as recorded time filters.
        #'
        #' @return An `EsgResult` object.
        #'
        initialize = function(index_node, params, response, context = NULL) {
            private$index_node <- index_node
            private$parameter <- query_param_clone(params)
            private$response <- query_result_normalize_response(response)
            private$context <- query_result_normalize_context(context)
            private$register_dynamic_fields()
            self
        },
        # }}}

        # to_data_table {{{
        #' @description
        #' Convert the results into a [data.table][data.table::data.table()]
        #'
        #' @param fields A non-empty character vector indicating the fields to
        #'        put into the `data.table`. If `NULL`, all fields in the query
        #'        result will be used. Possible field names can be retrieved
        #'        using `$fields`. Default: `NULL`.
        #'
        #' @param formatted Whether to use formatted values for special fields,
        #'        including `url` and `size`. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_data_table = function(fields = NULL, formatted = NULL) {
            checkmate::assert_character(fields, any.missing = FALSE, unique = TRUE, min.len = 1L, null.ok = TRUE)
            checkmate::assert_character(formatted, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

            if (is.null(fields)) {
                fields <- self$fields
            } else {
                checkmate::assert_subset(fields, self$fields)
            }

            res <- stats::setNames(
                lapply(fields, function(field) {
                    private$normalize_output_field(
                        private$get_output_field(field, formatted = field %in% formatted)
                    )
                }),
                fields
            )

            data.table::setDT(res)
        },
        # }}}

        # to_dt {{{
        #' @description
        #' Alias of `$to_data_table()`.
        #'
        #' @param ... Arguments passed to `$to_data_table()`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_dt = function(...) {
            self$to_data_table(...)
        },
        # }}}

        # count {{{
        #' @description
        #' Count the number of matched records in current result
        #'
        #' @return An integer.
        #'
        count = function() {
            length(self$id)
        },
        # }}}

        # save {{{
        #' @description
        #' Save the result into a JSON file
        #'
        #' `$save()` puts main data of an `EsgResult` object into a JSON file
        #' which can be loaded to restore the current state of result using
        #' \href{#method-EsgResult-load}{\code{EsgResult$load()}}.
        #'
        #' @param file A string indicating the JSON file path to save the data
        #'        to.
        #'
        #' @param pretty Whether to add indentation whitespace to JSON output.
        #'        For details, please see [jsonlite::toJSON()]. Default: `TRUE`.
        #'
        #' @return The full path of the output JSON file.
        #'
        save = function(file, pretty = TRUE) {
            args <- list(
                index_node = private$index_node,
                parameter = private$parameter,
                response = private$response,
                file = file,
                pretty = pretty,
                schema = private$result_schema()
            )
            if (length(private$context)) {
                args$context <- private$context
            }

            do.call(query_save, args)
        },
        # }}}

        # load {{{
        #' @description
        #' Restore the result state from an JSON file
        #'
        #' `$load()` reads data of an `EsgResult` object from a JSON file
        #' created using
        #' \href{#method-EsgResult-save}{\code{EsgResult$save()}}. Saved
        #' result fields are restored as read-only active bindings. A saved
        #' file can only be loaded by the matching result type.
        #'
        #' @param file A string indicating the JSON file path to read the data
        #'        from.
        #'
        #' @return The modified `EsgResult` object itself.
        #'
        load = function(file) {
            q <- query_load(file, private$result_schema())
            private$validate_loaded_result(q)

            private$index_node <- q$index_node
            private$parameter <- q$parameter
            private$response <- q$response
            private$context <- query_result_normalize_context(q$context)
            private$register_dynamic_fields()

            self
        },
        # }}}

        # query_url {{{
        #' @description
        #' Return the ESGF search query URL used to create this result.
        #'
        #' @param pages Which recorded query URLs to return. `"first"` returns
        #'        the first request URL. `"all"` returns every recorded request
        #'        URL, including pagination requests. Default: `"first"`.
        #'
        #' @return A named character vector of query URLs.
        #'
        query_url = function(pages = c("first", "all")) {
            pages <- match.arg(pages)
            urls <- private$get_query_url_context()
            if (identical(pages, "first")) {
                return(utils::head(urls, 1L))
            }

            urls
        },
        # }}}

        # reachable {{{
        #' @description
        #' Probe whether result records are reachable through a service URL.
        #'
        #' `$reachable()` performs a lightweight probe of the selected service
        #' URL for each record already held in the result. It returns diagnostic
        #' rows and does not modify result context or saved-result metadata.
        #'
        #' @param service ESGF URL service to probe. Default: `"OPENDAP"`.
        #' @param timeout Timeout for each URL probe in seconds. Default: `5`.
        #' @param probe_concurrency Maximum concurrent URL probes. Default:
        #'        `1`.
        #' @param network_policy Optional list of curl options, including
        #'        `connect_timeout`, `ssl_verifypeer`, `proxy`, and `useragent`.
        #'
        #' @return A [data.table][data.table::data.table()] with columns
        #'        `record_index`, `id`, `data_node`, `service`, `url`,
        #'        `reachable`, `latency_ms`, and `error`.
        reachable = function(service = "OPENDAP", timeout = 5, probe_concurrency = 1L,
                             network_policy = NULL) {
            checkmate::assert_string(service)

            docs <- private$get_docs()
            n <- nrow(docs)
            urls <- private$get_url(service, service)
            probes <- query_result_reachable_probe_urls(
                urls,
                timeout = timeout,
                network_policy = network_policy,
                probe_concurrency = probe_concurrency
            )

            data.table::data.table(
                record_index = seq_len(n),
                id = as.character(query_result_column(docs, "id")),
                data_node = as.character(query_result_column(docs, "data_node")),
                service = rep(service, n),
                url = urls,
                reachable = probes$reachable,
                latency_ms = probes$latency_ms,
                error = probes$error
            )
        },
        # }}}

        # slice {{{
        #' @description
        #' Subset result records by row, logical selector, or record ID.
        #'
        #' `$slice()` filters the records already held in memory. It does not
        #' change the original ESGF query parameters or query URL provenance.
        #'
        #' @param i A positive or negative integer vector, logical vector,
        #'        character vector of record IDs, or `NULL`. `NULL` returns an
        #'        empty result.
        #'
        #' @return A new result object of the same type.
        slice = function(i = NULL) {
            index <- private$normalize_slice_index(i)
            docs <- private$get_docs()
            out <- docs[index, , drop = FALSE]
            private$result_with_docs(
                out,
                context = private$update_selection_context(index)
            )
        },
        # }}}

        # filter {{{
        #' @description
        #' Subset result records with a predicate function.
        #'
        #' @param predicate A function that accepts `self$to_data_table()` and
        #'        returns a logical vector with one value per current record.
        #' @param formatted Whether to use formatted values when creating the
        #'        predicate input data table. Default: `FALSE`.
        #'
        #' @return A new result object of the same type.
        filter = function(predicate, formatted = FALSE) {
            checkmate::assert_function(predicate)
            checkmate::assert_flag(formatted)

            dt <- if (isTRUE(formatted)) {
                self$to_data_table(formatted = TRUE)
            } else {
                self$to_data_table()
            }
            keep <- predicate(dt)
            checkmate::assert_logical(
                keep,
                len = nrow(dt),
                any.missing = FALSE,
                .var.name = "predicate result"
            )

            self$slice(keep)
        },
        # }}}

        # selection {{{
        #' @description
        #' Return local selection provenance for this result.
        #'
        #' `$selection()` maps current rows back to the result object that first
        #' recorded selection provenance. It does not record intermediate filter
        #' steps.
        #'
        #' @return A list with `source_count`, `source_num_found`, and
        #'        `source_indices`.
        selection = function() {
            private$get_selection_context()
        }
        # }}}
    ),

    active = list(
        # id {{{
        #' @field id A character vector indicating globally unique record
        #'        identifiers.
        id = function() {
            private$get_field("id")
        },
        # }}}

        # url {{{
        #' @field url A list of [data.table][data.table::data.table()] with 3
        #'        columns:
        #'
        #' 1. `service` \\[`character`\\]: The service types, e.g. OPENDAP,
        #'    HTTPServer, etc.;
        #' 2. `url` \\[`character`\\]: The actual URLs;
        #' 3. `mime_type` \\[`character`\\]: The MIME types indicating
        #'    the nature and format of the corresponding document of the
        #'    URLs.
        url = function() {
            urls <- private$get_field("url")
            if (!length(urls)) {
                return(NULL)
            }

            lapply(urls, function(url) {
                if (!length(url)) {
                    return(NULL)
                } # nocov

                s <- strsplit(url, "|", fixed = TRUE)
                # nocov start
                if (any(unreco <- lengths(s) != 3L)) {
                    s[unreco] <- NULL
                }
                if (!length(s)) {
                    return(NULL)
                }
                # nocov end

                res <- data.table::setDT(data.table::transpose(s))
                data.table::setcolorder(res, c(3L, 1L, 2L))
                data.table::setnames(res, c("service", "url", "mime_type"))
                res
            })
        },
        # }}}

        # size {{{
        #' @field size A vector of [units][units::as_units()] indicating the
        #'        file sizes.
        size = function() {
            size <- private$get_field("size")
            set_size_units(size)
        },
        # }}}

        # fields {{{
        #' @field fields A character vector indicating all fields in the results,
        #'        preserving the order returned by the response.
        fields = function() {
            names(private$get_docs())
        },
        # }}}

        # time_filter {{{
        #' @field time_filter A list describing the time range used by
        #'        `$filter_time()`, or `NULL` if no result-level time filter
        #'        has been recorded.
        time_filter = function() {
            private$get_time_filter_context()
        }
        # }}}
    ),

    private = list(
        index_node = NULL,
        parameter = NULL,
        response = NULL,
        context = list(),
        dynamic_fields = character(),
        result_type = NULL,

        required_fields = c("id", "size", "url"),
        query_fields = c("dataset_id", "fields", "latest", "distrib", "limit", "type", "format"),
        static_fields = c("id", "url", "size", "fields", "filename", "url_opendap", "url_download"),

        # result_schema {{{
        result_schema = function() {
            type <- private$result_type
            if (!is.character(type) || length(type) != 1L || is.na(type)) {
                cli::cli_abort(
                    "Cannot select a saved-result schema for an untyped ESGF result. Use {.code esg_result('dataset')}, {.code esg_result('file')} or {.code esg_result('aggregation')}."
                )
            }

            schema <- switch(
                type,
                Dataset = SCHEMA_RESULT_DATASET,
                File = SCHEMA_RESULT_FILE,
                Aggregation = SCHEMA_RESULT_AGGREGATION,
                NULL
            )
            if (is.null(schema)) {
                cli::cli_abort("Cannot select a saved-result schema for result type {.val {type}}.")
            }

            schema
        },
        # }}}

        get_docs = function() {
            if (is.null(private$response)) {
                return(data.frame(check.names = FALSE))
            }

            docs <- private$response$response$docs
            if (is.null(docs)) {
                data.frame(check.names = FALSE)
            } else {
                docs
            }
        },

        get_field = function(field) {
            docs <- private$get_docs()
            val <- docs[[field]]
            if (is.null(val)) {
                return(NULL)
            }
            if (all(lengths(val) == 1L)) unlst(val) else val
        },

        # get_time_filter_context {{{
        get_time_filter_context = function() {
            ctx <- private$context$time_filter
            if (is.null(ctx) || !length(ctx)) {
                return(NULL)
            }

            ctx
        },
        # }}}

        # get_query_url_context {{{
        get_query_url_context = function() {
            urls <- private$context$query_url
            if (is.null(urls)) {
                if (is.null(private$index_node) || is.null(private$parameter)) {
                    return(character())
                }
                urls <- query_build(private$index_node, private$parameter)
            }

            query_result_normalize_query_url(urls)
        },
        # }}}

        # get_selection_context {{{
        get_selection_context = function() {
            ctx <- private$context$selection
            if (!is.null(ctx) && length(ctx)) {
                return(query_result_normalize_selection_context(ctx))
            }

            n <- nrow(private$get_docs())
            source_num_found <- if (is.null(private$response)) {
                n
            } else {
                private$response$response$numFound
            }
            if (is.null(source_num_found) || !length(source_num_found) || is.na(source_num_found[[1L]])) {
                source_num_found <- n
            }

            list(
                source_count = as.integer(n),
                source_num_found = as.integer(source_num_found[[1L]]),
                source_indices = seq_len(n)
            )
        },
        # }}}

        # update_time_filter_context {{{
        update_time_filter_context = function(start, stop, method, total, selected, unknown) {
            context <- private$context
            context$time_filter <- list(
                start = query_result_format_iso_datetime(start),
                stop = query_result_format_iso_datetime(stop),
                method = method,
                unknown = "kept",
                total = as.integer(total),
                selected = as.integer(selected),
                unknown_count = as.integer(unknown)
            )

            context
        },
        # }}}

        # update_selection_context {{{
        update_selection_context = function(index, context = private$context) {
            selection <- private$get_selection_context()
            context$selection <- list(
                source_count = selection$source_count,
                source_num_found = selection$source_num_found,
                source_indices = selection$source_indices[index]
            )

            context
        },
        # }}}

        # result_with_docs {{{
        result_with_docs = function(docs, context = private$context) {
            response <- private$response
            response$response$docs <- docs
            response$response$numFound <- nrow(docs)
            response$response$start <- 0L
            generator <- switch(
                private$result_type,
                Dataset = EsgResultDataset,
                File = EsgResultFile,
                Aggregation = EsgResultAggregation,
                NULL
            )
            if (is.null(generator)) {
                cli::cli_abort("Cannot create a filtered result for an untyped ESGF result.")
            }

            new_query_result(
                generator,
                private$index_node,
                private$parameter,
                response,
                context = context
            )
        },
        # }}}

        # filter_time_result {{{
        filter_time_result = function(start, stop, method = c("drs", "opendap"), result_label = "file") {
            method <- match.arg(method)
            window <- query_result_parse_time_window(start, stop)
            docs <- private$get_docs()
            if (!nrow(docs)) {
                context <- private$update_time_filter_context(
                    window$start,
                    window$stop,
                    method = method,
                    total = 0L,
                    selected = 0L,
                    unknown = 0L
                )
                return(private$result_with_docs(
                    docs,
                    context = private$update_selection_context(integer(), context = context)
                ))
            }

            ranges <- switch(
                method,
                drs = private$filter_time_ranges_drs(docs, result_label),
                opendap = private$filter_time_ranges_opendap(result_label)
            )
            known <- !is.na(ranges$datetime_start) & !is.na(ranges$datetime_end)
            keep <- !known | (ranges$datetime_start <= window$stop & ranges$datetime_end >= window$start)
            keep[is.na(keep)] <- TRUE

            docs <- private$add_time_range_fields(docs, ranges)
            index <- which(keep)
            out <- docs[index, , drop = FALSE]
            context <- private$update_time_filter_context(
                window$start,
                window$stop,
                method = method,
                total = nrow(docs),
                selected = nrow(out),
                unknown = sum(!known)
            )
            context <- private$update_selection_context(index, context = context)

            private$result_with_docs(out, context = context)
        },
        # }}}

        # normalize_slice_index {{{
        normalize_slice_index = function(i) {
            n <- nrow(private$get_docs())
            if (is.null(i)) {
                return(integer())
            }

            if (is.logical(i)) {
                checkmate::assert_logical(i, len = n, any.missing = FALSE, .var.name = "i")
                return(which(i))
            }

            if (is.character(i)) {
                checkmate::assert_character(i, any.missing = FALSE, .var.name = "i")
                ids <- self$id
                if (!length(i)) {
                    return(integer())
                }
                if (anyDuplicated(i)) {
                    stop("`i` must not contain duplicate record IDs.", call. = FALSE)
                }
                index <- match(i, ids)
                if (anyNA(index)) {
                    missing <- i[is.na(index)]
                    stop(sprintf(
                        "Unknown record ID(s): [%s].",
                        paste(sprintf("'%s'", missing), collapse = ", ")
                    ), call. = FALSE)
                }
                return(index)
            }

            if (!checkmate::test_integerish(i, any.missing = FALSE)) {
                stop("`i` must be an integer, logical, character, or NULL selector.", call. = FALSE)
            }

            i <- as.integer(i)
            if (!length(i)) {
                return(integer())
            }
            if (any(i == 0L)) {
                stop("`i` must not contain zero.", call. = FALSE)
            }
            if (any(i > 0L) && any(i < 0L)) {
                stop("`i` must not mix positive and negative indices.", call. = FALSE)
            }
            if (anyDuplicated(i)) {
                stop("`i` must not contain duplicate indices.", call. = FALSE)
            }

            if (all(i < 0L)) {
                if (any(abs(i) > n)) {
                    stop(sprintf("Negative indices must be between -%d and -1.", n), call. = FALSE)
                }
                return(setdiff(seq_len(n), abs(i)))
            }

            if (any(i > n)) {
                stop(sprintf("Positive indices must be between 1 and %d.", n), call. = FALSE)
            }

            i
        },
        # }}}

        # filter_time_ranges_drs {{{
        filter_time_ranges_drs = function(docs, result_label = "file") {
            warning(
                sprintf(
                    "Time filtering with method = 'drs' uses DRS filename conventions for %s records. Files whose time range cannot be parsed are kept.",
                    result_label
                ),
                call. = FALSE
            )

            labels <- query_result_drs_labels(docs)
            ranges <- query_result_parse_drs_ranges(labels$value)
            unknown <- is.na(ranges$datetime_start) | is.na(ranges$datetime_end)
            if (any(unknown)) {
                warning(
                    sprintf(
                        "Could not parse a DRS time range for %d %s record(s); keeping those records.",
                        sum(unknown),
                        result_label
                    ),
                    call. = FALSE
                )
            }

            ranges
        },
        # }}}

        # filter_time_ranges_opendap {{{
        filter_time_ranges_opendap = function(result_label = "file") {
            urls <- self$url_opendap
            if (is.null(urls)) {
                urls <- rep(NA_character_, self$count())
            }

            start <- as.POSIXct(rep(NA_real_, length(urls)), origin = "1970-01-01", tz = "UTC")
            end <- start
            failed <- logical(length(urls))

            for (i in seq_along(urls)) {
                url <- urls[[i]]
                if (is.na(url) || !nzchar(url)) {
                    failed[[i]] <- TRUE
                    next
                }

                ds <- NULL
                ok <- tryCatch(
                    {
                        ds <- EsgDataset$new(url)
                        ds$open()
                        time_axis <- ds$get_time_axis()$values
                        if (!length(time_axis) || all(is.na(time_axis))) {
                            stop("The NetCDF time axis is empty or unavailable.", call. = FALSE)
                        }
                        time_axis <- time_axis[!is.na(time_axis)]
                        start[[i]] <- min(time_axis)
                        end[[i]] <- max(time_axis)
                        TRUE
                    },
                    error = function(e) FALSE,
                    finally = {
                        if (!is.null(ds) && isTRUE(ds$is_open)) {
                            ds$close()
                        }
                    }
                )
                failed[[i]] <- !ok
            }

            if (any(failed)) {
                warning(
                    sprintf(
                        "Could not inspect OPeNDAP time axes for %d %s record(s); keeping those records.",
                        sum(failed),
                        result_label
                    ),
                    call. = FALSE
                )
            }

            data.frame(datetime_start = start, datetime_end = end, check.names = FALSE)
        },
        # }}}

        # add_time_range_fields {{{
        add_time_range_fields = function(docs, ranges) {
            docs$datetime_start <- query_result_format_iso_datetime(ranges$datetime_start)
            docs$datetime_end <- query_result_format_iso_datetime(ranges$datetime_end)
            docs
        },
        # }}}

        # validate_loaded_result {{{
        validate_loaded_result = function(q) {
            expected <- private$result_type
            actual <- query_param_value(q$parameter$type())
            if (!identical(actual, expected)) {
                stop(sprintf(
                    "Cannot load %s result into %s object. Use esg_result('%s')$load() instead.",
                    if (is.null(actual)) "NULL" else sprintf("'%s'", actual),
                    class(self)[[1L]],
                    tolower(expected)
                ), call. = FALSE)
            }

            invisible(q)
        },
        # }}}

        # register_dynamic_fields {{{
        register_dynamic_fields = function() {
            if (length(private$dynamic_fields)) {
                for (field in private$dynamic_fields) {
                    if (exists(field, envir = self, inherits = FALSE) && bindingIsActive(field, self)) {
                        rm(list = field, envir = self)
                    }
                }
                private$dynamic_fields <- character()
            }

            docs <- private$get_docs()
            fields <- setdiff(names(docs), private$static_fields)
            if (!length(fields)) {
                return(invisible(character()))
            }

            fields <- fields[!vapply(fields, exists, logical(1L), envir = self, inherits = FALSE)]
            for (field in fields) {
                makeActiveBinding(
                    field,
                    local({
                        field <- field
                        function(value) {
                            if (!missing(value)) {
                                stop("ESGF result fields are read-only.", call. = FALSE)
                            }
                            private$get_field(field)
                        }
                    }),
                    self
                )
            }
            private$dynamic_fields <- fields

            invisible(fields)
        },
        # }}}

        # has_access {{{
        has_access = function(type) {
            n <- self$count()
            if (!n) {
                return(logical())
            }

            access <- private$get_field("access")
            if (is.null(access)) {
                return(rep(FALSE, n))
            }
            if (!is.list(access)) {
                access <- as.list(access)
            }
            if (length(access) < n) {
                access <- c(access, rep(list(character()), n - length(access)))
            }

            vapply(access[seq_len(n)], function(acc) type %in% acc, logical(1L))
        },
        # }}}

        # get_url {{{
        get_url = function(type, name = type) {
            urls <- self$url
            if (is.null(urls)) {
                return(rep(NA_character_, self$count()))
            }

            vapply(
                seq_along(urls),
                function(i) {
                    dt_url <- urls[[i]]
                    # nocov start
                    if (!length(dt_url)) {
                        return(NA_character_)
                    }
                    # nocov end

                    res <- dt_url$url[dt_url$service == type]
                    if (!length(res)) {
                        return(NA_character_)
                    }
                    # nocov start
                    if (length(res) > 1L) {
                        warning(sprintf(
                            "Multiple %s URLs found for record %d%s. Only the first is returned.",
                            name,
                            i,
                            private$record_context(i)
                        ))
                        res <- res[[1L]]
                    }
                    # nocov end

                    res
                },
                character(1L)
            )
        },
        # }}}

        # get_output_field {{{
        get_output_field = function(field, formatted = FALSE) {
            docs <- private$get_docs()
            value <- docs[[field]]
            if (isTRUE(formatted) || is.null(value)) {
                return(self[[field]])
            }

            value
        },
        # }}}

        # normalize_output_field {{{
        normalize_output_field = function(value) {
            if (typeof(value) == "list") {
                len <- lengths(value)
                if (all(len <= 1L)) {
                    value[len == 0L] <- list(NA)
                    value <- unlst(value)
                }
            }

            value
        },
        # }}}

        # record_context {{{
        record_context = function(index) {
            checkmate::assert_int(index, lower = 1L)

            docs <- private$get_docs()
            for (field in c("id", "dataset_id", "instance_id", "title", "filename")) {
                value <- docs[[field]]
                if (is.null(value) || length(value) < index) {
                    next
                }

                value <- value[[index]]
                value <- private$format_record_context_value(value)
                if (is.null(value)) {
                    next
                }

                return(sprintf(" (%s: %s)", field, value))
            }

            ""
        },
        # }}}

        # record_label {{{
        record_label = function(index) {
            checkmate::assert_int(index, lower = 1L)
            sprintf("record %d%s", index, private$record_context(index))
        },
        # }}}

        # record_labels {{{
        record_labels = function(index) {
            checkmate::assert_integerish(index, lower = 1L, any.missing = FALSE, min.len = 1L)
            paste(
                vapply(as.integer(index), private$record_label, character(1L)),
                collapse = ", "
            )
        },
        # }}}

        # format_record_context_value {{{
        format_record_context_value = function(value) {
            if (is.null(value) || !length(value)) {
                return(NULL)
            }

            if (is.data.frame(value) || is.list(value)) {
                value <- unlist(value, recursive = TRUE, use.names = FALSE)
            }
            if (!length(value)) {
                return(NULL)
            }

            value <- tryCatch(as.character(value), error = function(e) character())
            value <- value[!is.na(value) & nzchar(value)]
            if (!length(value)) {
                return(NULL)
            }

            value <- value[[1L]]
            if (nchar(value, type = "width") > 80L) {
                value <- paste0(substr(value, 1L, 77L), "...")
            }

            value
        },
        # }}}

        # print_header {{{
        print_header = function(type = "") {
            d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
            cli::cli_rule("ESGF Query Result [{type}]")
            cli::cli_end(d)
        },
        # }}}

        # print_summary {{{
        print_summary = function(type = "") {
            ts <- format(private$response$timestamp, tz = Sys.timezone(), usetz = TRUE)
            fields <- self$fields
            cli::cli_bullets(c("*" = "Index Node: {private$index_node}"))
            cli::cli_bullets(c("*" = "Collected at: {ts}"))
            cli::cli_bullets(c("*" = "Result count: {self$count()}"))
            if (type == "Aggregation") {
                cli::cli_bullets(c("*" = "Total size: <{.emph Unknown}> [Byte]"))
            } else {
                cli::cli_bullets(c("*" = "Total size: {format(round(set_size_units(sum(self$size)), 2L))}"))
            }
            if (!length(fields)) {
                cli::cli_bullets(c("*" = "Fields: 0"))
            } else {
                cli::cli_bullets(c("*" = "Fields: {length(fields)} | [ {fields} ]"))
            }
        },
        # }}}

        # print_parameters {{{
        print_parameters = function() {
            cli::cli_h1("<Query Parameter>")
            print_query_params(private$parameter)
        },
        # }}}

        # print_contents {{{
        print_contents = function(type, n) {
            checkmate::assert_count(n, positive = TRUE, null.ok = TRUE)

            if (is.null(private$get_field("data_node"))) {
                cli::cli_rule("<{type}>")
            } else {
                cli::cli_rule("<{type}> (From {length(unique(private$get_field('data_node')))} Data Nodes)")
            }

            if (self$count() == 0L) {
                cli::cli_bullets(c(" " = "{.strong <Empty>}"))
                cli::cli_bullets(c(
                    " " = "{.emph NOTE: No matched data found. Please update query parameters and try again.}"
                ))
                return()
            }

            checkmate::assert_count(n, positive = TRUE, null.ok = TRUE)
            n <- if (is.null(n)) self$count() else min(n, self$count())
            ind <- seq_len(n)

            pre <- lpad(ind, "0")
            brief <- sprintf("[%s] %s", pre, self$id[ind])

            spc <- strrep(" ", nchar(pre[1L], "width"))

            if (type == "Dataset") {
                size <- sprintf(
                    "%s   [ %s Files, %s %s | %s ]\n%s   [ Access: <%s> ]",
                    spc,
                    private$get_field("number_of_files")[ind],
                    round(self$size[ind], 2),
                    units(self$size)$numerator,
                    if (is.null(private$get_field("number_of_aggregations"))) {
                        "No Aggregations"
                    } else {
                        agg <- private$get_field("number_of_aggregations")[ind]
                        agg[is.na(agg)] <- 0L
                        paste(agg, vapply(agg, ngettext, "", "Aggregation", "Aggregations"))
                    },
                    spc,
                    if (is.null(private$get_field("access"))) {
                        "NONE"
                    } else {
                        vapply(private$get_field("access")[ind], paste0, "", collapse = ", ")
                    }
                )
            } else {
                size <- sprintf(
                    "%s   [ %s %s | Access: <%s> ]",
                    spc,
                    if (type == "Aggregation") "<Unknown>" else round(self$size[ind], 2),
                    units(self$size)$numerator,
                    if (is.null(self$url)) {
                        "NONE"
                    } else {
                        vapply(self$url[ind], FUN.VALUE = character(1), function(url) {
                            if (is.null(url)) {
                                return("NONE")
                            }
                            paste0(url$service, collapse = ", ")
                        })
                    }
                )
            }

            for (i in ind) {
                cli::cat_line(.subset2(brief, i))
                cli::cat_line(.subset2(size, i))
            }

            query_result_print_trunc(self$id, n)
        }
        # }}}
    )
)
# }}}

# result collection helpers {{{
query_result_print_trunc <- function(x, n, newline_before = is.data.frame(x)) {
    d <- cli::cli_div(theme = list(body = list(`padding-left` = 0L, `margin-left` = 0L)))
    total <- if (is.data.frame(x)) nrow(x) else length(x)
    if (n < total) {
        if (newline_before) cli::cli_text()
        cli::cli_text(cli::col_grey("# ... with {total - n} more item{?s}"))
    }
    cli::cli_end(d)
}

query_result_normalize_context <- function(context = NULL) {
    if (is.null(context) || !length(context)) {
        return(list())
    }
    if (!is.list(context)) {
        stop("Saved result context must be a list.", call. = FALSE)
    }
    if (!is.null(context$query_url)) {
        context$query_url <- query_result_normalize_query_url(context$query_url, named = FALSE)
    }
    if (!is.null(context$selection)) {
        context$selection <- query_result_normalize_selection_context(context$selection)
    }

    context
}

query_result_normalize_selection_context <- function(selection) {
    if (is.null(selection) || !length(selection)) {
        return(NULL)
    }
    if (!is.list(selection)) {
        stop("Saved result selection context must be a list.", call. = FALSE)
    }

    required <- c("source_count", "source_num_found", "source_indices")
    missing <- setdiff(required, names(selection))
    if (length(missing)) {
        stop(sprintf(
            "Saved result selection context is missing required field(s): [%s].",
            paste(sprintf("'%s'", missing), collapse = ", ")
        ), call. = FALSE)
    }

    source_indices <- selection$source_indices
    if (is.list(source_indices) && !length(source_indices)) {
        source_indices <- integer()
    }

    checkmate::assert_integerish(selection$source_count, lower = 0L, len = 1L, any.missing = FALSE)
    checkmate::assert_integerish(selection$source_num_found, lower = 0L, len = 1L, any.missing = FALSE)
    checkmate::assert_integerish(source_indices, lower = 1L, any.missing = FALSE)

    source_count <- as.integer(selection$source_count[[1L]])
    source_num_found <- as.integer(selection$source_num_found[[1L]])
    source_indices <- as.integer(source_indices)
    if (length(source_indices) && any(source_indices > source_count)) {
        stop("Saved result selection source indices must not exceed `source_count`.", call. = FALSE)
    }

    list(
        source_count = source_count,
        source_num_found = source_num_found,
        source_indices = source_indices
    )
}

query_result_normalize_query_url <- function(urls, named = TRUE) {
    if (is.null(urls) || !length(urls)) {
        return(stats::setNames(character(), character()))
    }

    checkmate::assert_character(urls, any.missing = FALSE)
    urls <- unname(as.character(urls))
    if (isTRUE(named)) {
        stats::setNames(urls, paste0("page", seq_along(urls)))
    } else {
        urls
    }
}

query_result_format_iso_datetime <- function(x) {
    if (is.null(x)) {
        return(character())
    }

    x <- as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    out[ok] <- format.POSIXct(x[ok], tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    out
}

query_result_parse_time_window <- function(start, stop) {
    checkmate::assert_scalar(start)
    checkmate::assert_scalar(stop)

    time <- parse_datetime(c(start, stop), tz = "UTC")
    if (any(is.na(time))) {
        stop("`start` and `stop` must be parseable datetimes.", call. = FALSE)
    }
    if (time[[2L]] < time[[1L]]) {
        stop("`stop` must be greater than or equal to `start`.", call. = FALSE)
    }

    list(start = time[[1L]], stop = time[[2L]])
}

query_result_drs_label_from_url <- function(url) {
    if (is.null(url) || !length(url)) {
        return(NA_character_)
    }

    url <- unlist(url, recursive = TRUE, use.names = FALSE)
    url <- as.character(url)
    url <- url[!is.na(url) & nzchar(url)]
    if (!length(url)) {
        return(NA_character_)
    }

    parsed <- vapply(strsplit(url, "|", fixed = TRUE), function(parts) parts[[1L]], character(1L))
    parsed <- sub("[?#].*$", "", parsed)
    parsed <- basename(parsed)
    parsed <- sub("\\.html$", "", parsed)
    parsed <- parsed[grepl("\\.nc$", parsed)]
    if (!length(parsed)) {
        return(NA_character_)
    }

    parsed[[1L]]
}

query_result_drs_label_from_id <- function(id) {
    if (is.null(id) || !length(id) || is.na(id[[1L]])) {
        return(NA_character_)
    }

    id <- as.character(id[[1L]])
    hit <- regmatches(id, regexpr("[^|/]+\\.nc", id, perl = TRUE))
    if (!length(hit) || !nzchar(hit)) {
        return(NA_character_)
    }

    hit
}

query_result_drs_labels <- function(docs) {
    n <- nrow(docs)
    labels <- rep(NA_character_, n)
    source <- rep(NA_character_, n)

    scalar_field <- function(field, i) {
        value <- docs[[field]]
        if (is.null(value) || length(value) < i) {
            return(NA_character_)
        }

        value <- value[[i]]
        value <- unlist(value, recursive = TRUE, use.names = FALSE)
        value <- as.character(value)
        value <- value[!is.na(value) & nzchar(value)]
        if (!length(value)) {
            return(NA_character_)
        }

        value[[1L]]
    }

    for (i in seq_len(n)) {
        title <- scalar_field("title", i)
        if (!is.na(title) && grepl("\\.nc$", title)) {
            labels[[i]] <- title
            source[[i]] <- "title"
            next
        }

        url <- if (!is.null(docs$url) && length(docs$url) >= i) {
            query_result_drs_label_from_url(docs$url[[i]])
        } else {
            NA_character_
        }
        if (!is.na(url)) {
            labels[[i]] <- url
            source[[i]] <- "url"
            next
        }

        id <- scalar_field("id", i)
        id_label <- query_result_drs_label_from_id(id)
        if (!is.na(id_label)) {
            labels[[i]] <- id_label
            source[[i]] <- "id"
        }
    }

    data.frame(value = labels, source = source, check.names = FALSE)
}

query_result_column <- function(dt, name, default = NA_character_) {
    if (name %in% names(dt)) {
        return(dt[[name]])
    }
    rep(default, nrow(dt))
}

query_result_logical_file_id <- function(dt) {
    master_id <- query_result_column(dt, "master_id")
    tracking_id <- query_result_column(dt, "tracking_id")
    checksum <- query_result_column(dt, "checksum")
    size <- as.character(query_result_column(dt, "size"))
    filename <- query_result_column(dt, "filename")
    id <- query_result_column(dt, "id")

    out <- rep(NA_character_, nrow(dt))
    use <- !is.na(master_id) & nzchar(master_id)
    out[use] <- paste0("master:", master_id[use])
    use <- is.na(out) & !is.na(tracking_id) & nzchar(tracking_id)
    out[use] <- paste0("tracking:", tracking_id[use])
    use <- is.na(out) & !is.na(checksum) & nzchar(checksum) & !is.na(filename) & nzchar(filename)
    out[use] <- vapply(which(use), function(i) {
        paste("checksum", checksum[[i]], size[[i]], filename[[i]], sep = ":")
    }, character(1L))
    use <- is.na(out) & !is.na(id) & nzchar(id)
    out[use] <- paste0("id:", id[use])
    out[is.na(out)] <- paste0("row:", which(is.na(out)))
    out
}

query_result_reachable_url_scheme <- function(url) {
    url <- as.character(url)
    out <- rep(NA_character_, length(url))
    ok <- !is.na(url) & nzchar(url)
    has_scheme <- ok & grepl("^[A-Za-z][A-Za-z0-9+.-]*:", url)
    out[has_scheme] <- tolower(sub("^([A-Za-z][A-Za-z0-9+.-]*):.*$", "\\1", url[has_scheme]))
    out
}

query_result_reachable_is_http_url <- function(url) {
    query_result_reachable_url_scheme(url) %in% c("http", "https")
}

query_result_reachable_is_local_url <- function(url) {
    scheme <- query_result_reachable_url_scheme(url)
    missing <- is.na(url) | !nzchar(url)
    !missing & (is.na(scheme) | scheme == "file")
}

query_result_reachable_file_path <- function(url) {
    url <- as.character(url[[1L]])
    scheme <- query_result_reachable_url_scheme(url)
    if (identical(scheme, "file")) {
        path <- sub("^file://", "", url, ignore.case = TRUE)
        path <- sub("^localhost(?=/)", "", path, perl = TRUE)
        return(utils::URLdecode(path))
    }

    url
}

query_result_reachable_url_host <- function(url) {
    url <- as.character(url)
    out <- rep(NA_character_, length(url))
    use <- query_result_reachable_is_http_url(url)
    out[use] <- sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/:?#]+).*$", "\\1", url[use])
    out
}

query_result_reachable_missing_probe <- function(error) {
    list(reachable = NA, latency_ms = NA_real_, error = error)
}

query_result_reachable_local_probe <- function(url) {
    path <- query_result_reachable_file_path(url)
    ok <- file.exists(path)
    list(
        reachable = ok,
        latency_ms = 0,
        error = if (ok) NA_character_ else "File does not exist."
    )
}

query_result_reachable_validate_probe_args <- function(timeout = 5, network_policy = NULL,
                                                       probe_concurrency = NULL) {
    checkmate::assert_number(timeout, lower = 0, finite = TRUE)
    if (timeout <= 0) {
        cli::cli_abort("`timeout` must be greater than zero.")
    }
    if (!is.null(network_policy)) {
        checkmate::assert_list(network_policy, names = "unique")
    }
    if (!is.null(probe_concurrency)) {
        checkmate::assert_count(probe_concurrency, positive = TRUE)
    }

    invisible(NULL)
}

query_result_reachable_http_attempt <- function(url, timeout = 5, network_policy = NULL,
                                                nobody = TRUE, range = FALSE) {
    if (is.null(network_policy)) {
        network_policy <- list()
    }
    connect_timeout <- network_policy$connect_timeout
    if (is.null(connect_timeout)) {
        connect_timeout <- min(timeout, 3)
    }
    ssl_verifypeer <- network_policy$ssl_verifypeer
    if (is.null(ssl_verifypeer)) {
        ssl_verifypeer <- TRUE
    }

    start <- proc.time()[["elapsed"]]
    tryCatch(
        {
            handle <- downloader__curl_handle(
                timeout = timeout,
                connect_timeout = connect_timeout,
                ssl_verifypeer = ssl_verifypeer,
                proxy = network_policy$proxy,
                useragent = network_policy$useragent,
                nobody = nobody
            )
            curl::handle_setopt(handle, failonerror = TRUE)
            if (isTRUE(range)) {
                curl::handle_setheaders(handle, Range = "bytes=0-0")
            }
            curl::curl_fetch_memory(url, handle = handle)
            list(
                ok = TRUE,
                latency_ms = (proc.time()[["elapsed"]] - start) * 1000,
                error = NA_character_
            )
        },
        error = function(e) {
            list(ok = FALSE, latency_ms = NA_real_, error = conditionMessage(e))
        }
    )
}

query_result_reachable_probe_url <- function(url, timeout = 5, network_policy = NULL) {
    query_result_reachable_validate_probe_args(timeout, network_policy)

    if (is.na(url) || !nzchar(url)) {
        return(query_result_reachable_missing_probe("Missing URL."))
    }
    if (query_result_reachable_is_local_url(url)) {
        return(query_result_reachable_local_probe(url))
    }
    if (!query_result_reachable_is_http_url(url)) {
        return(query_result_reachable_missing_probe("Unsupported URL scheme."))
    }

    head <- query_result_reachable_http_attempt(
        url,
        timeout = timeout,
        network_policy = network_policy,
        nobody = TRUE
    )
    if (isTRUE(head$ok)) {
        return(list(reachable = TRUE, latency_ms = head$latency_ms, error = NA_character_))
    }

    body <- query_result_reachable_http_attempt(
        url,
        timeout = timeout,
        network_policy = network_policy,
        nobody = FALSE,
        range = TRUE
    )
    if (isTRUE(body$ok)) {
        return(list(reachable = TRUE, latency_ms = body$latency_ms, error = NA_character_))
    }

    error <- body$error
    if (is.null(error) || !length(error) || is.na(error[[1L]]) || !nzchar(error[[1L]])) {
        error <- head$error
    }
    if (is.null(error) || !length(error) || is.na(error[[1L]]) || !nzchar(error[[1L]])) {
        error <- "URL probe failed."
    }
    list(
        reachable = FALSE,
        latency_ms = NA_real_,
        error = error
    )
}

query_result_reachable_probe_http_urls_network <- function(urls, timeout = 5, network_policy = NULL,
                                                           probe_concurrency = 1L) {
    urls <- unique(urls[!is.na(urls) & nzchar(urls)])
    urls <- urls[query_result_reachable_is_http_url(urls)]
    if (!length(urls)) {
        return(stats::setNames(list(), character()))
    }
    if (probe_concurrency <= 1L || length(urls) <= 1L) {
        return(stats::setNames(
            lapply(urls, query_result_reachable_probe_url, timeout = timeout, network_policy = network_policy),
            urls
        ))
    }

    out <- vector("list", length(urls))
    names(out) <- urls
    failed <- rep(FALSE, length(urls))
    ok <- tryCatch({
        if (is.null(network_policy)) {
            network_policy <- list()
        }
        connect_timeout <- network_policy$connect_timeout
        if (is.null(connect_timeout)) {
            connect_timeout <- min(timeout, 3)
        }
        ssl_verifypeer <- network_policy$ssl_verifypeer
        if (is.null(ssl_verifypeer)) {
            ssl_verifypeer <- TRUE
        }
        pool <- curl::new_pool(total_con = probe_concurrency, host_con = probe_concurrency)
        for (i in seq_along(urls)) {
            local({
                j <- i
                start <- proc.time()[["elapsed"]]
                handle <- downloader__curl_handle(
                    timeout = timeout,
                    connect_timeout = connect_timeout,
                    ssl_verifypeer = ssl_verifypeer,
                    proxy = network_policy$proxy,
                    useragent = network_policy$useragent,
                    nobody = TRUE
                )
                curl::handle_setopt(handle, failonerror = TRUE)
                curl::handle_setopt(handle, url = urls[[j]])
                curl::multi_add(
                    handle,
                    done = function(response) {
                        out[[j]] <<- list(
                            reachable = TRUE,
                            latency_ms = (proc.time()[["elapsed"]] - start) * 1000,
                            error = NA_character_
                        )
                    },
                    fail = function(error) {
                        failed[[j]] <<- TRUE
                    },
                    pool = pool
                )
            })
        }
        curl::multi_run(timeout = max(timeout * length(urls), 1), poll = TRUE, pool = pool)
        TRUE
    }, error = function(e) FALSE)

    if (!isTRUE(ok)) {
        return(stats::setNames(
            lapply(urls, query_result_reachable_probe_url, timeout = timeout, network_policy = network_policy),
            urls
        ))
    }

    missing <- vapply(out, is.null, logical(1L)) | failed
    if (any(missing)) {
        out[missing] <- lapply(
            urls[missing],
            query_result_reachable_probe_url,
            timeout = timeout,
            network_policy = network_policy
        )
    }

    out
}

query_result_reachable_probe_urls <- function(urls, timeout = 5, network_policy = NULL,
                                              probe_concurrency = 1L) {
    checkmate::assert_character(urls, any.missing = TRUE)
    query_result_reachable_validate_probe_args(timeout, network_policy, probe_concurrency)

    out <- data.table::data.table(
        url = urls,
        reachable = rep(NA, length(urls)),
        latency_ms = rep(NA_real_, length(urls)),
        error = rep(NA_character_, length(urls))
    )
    if (!length(urls)) {
        return(out)
    }

    unique_urls <- unique(urls)
    use_http <- !is.na(unique_urls) & nzchar(unique_urls) & query_result_reachable_is_http_url(unique_urls)
    probes <- query_result_reachable_probe_http_urls_network(
        unique_urls[use_http],
        timeout = timeout,
        network_policy = network_policy,
        probe_concurrency = probe_concurrency
    )

    for (url in unique_urls[!use_http]) {
        probe <- query_result_reachable_probe_url(url, timeout = timeout, network_policy = network_policy)
        if (is.na(url)) {
            idx <- is.na(out$url)
        } else {
            idx <- !is.na(out$url) & out$url == url
        }
        out[idx, `:=`(
            reachable = as.logical(probe$reachable),
            latency_ms = as.numeric(probe$latency_ms),
            error = as.character(probe$error)
        )]
    }
    if (length(probes)) {
        for (url in names(probes)) {
            probe <- probes[[url]]
            target_url <- url
            out[!is.na(out[["url"]]) & out[["url"]] == target_url, `:=`(
                reachable = as.logical(probe$reachable),
                latency_ms = as.numeric(probe$latency_ms),
                error = as.character(probe$error)
            )]
        }
    }

    out[]
}

query_result_probe_url <- function(url, timeout = 5, network_policy = NULL) {
    if (is.na(url) || !nzchar(url) || startsWith(url, "file://")) {
        return(list(latency = NA_real_, throughput = NA_real_))
    }
    if (is.null(network_policy)) {
        network_policy <- list()
    }
    connect_timeout <- network_policy$connect_timeout
    if (is.null(connect_timeout)) {
        connect_timeout <- min(timeout, 3)
    }
    ssl_verifypeer <- network_policy$ssl_verifypeer
    if (is.null(ssl_verifypeer)) {
        ssl_verifypeer <- TRUE
    }
    start <- Sys.time()
    ok <- tryCatch(
        {
            handle <- downloader__curl_handle(
                timeout = timeout,
                connect_timeout = connect_timeout,
                ssl_verifypeer = ssl_verifypeer,
                proxy = network_policy$proxy,
                useragent = network_policy$useragent,
                nobody = TRUE
            )
            curl::curl_fetch_memory(url, handle = handle)
            TRUE
        },
        error = function(e) FALSE
    )
    if (!ok) {
        start <- Sys.time()
        ok <- tryCatch(
            {
                handle <- downloader__curl_handle(
                    timeout = timeout,
                    connect_timeout = connect_timeout,
                    ssl_verifypeer = ssl_verifypeer,
                    proxy = network_policy$proxy,
                    useragent = network_policy$useragent
                )
                curl::handle_setheaders(handle, Range = "bytes=0-0")
                curl::curl_fetch_memory(url, handle = handle)
                TRUE
            },
            error = function(e) FALSE
        )
    }
    if (!ok) {
        return(list(latency = NA_real_, throughput = NA_real_))
    }
    list(latency = as.numeric(difftime(Sys.time(), start, units = "secs")), throughput = NA_real_)
}

query_result_probe_urls_network <- function(urls, timeout = 5, network_policy = NULL, probe_concurrency = 1L) {
    urls <- unique(urls[!is.na(urls) & nzchar(urls)])
    urls <- urls[!startsWith(urls, "file://")]
    if (!length(urls)) {
        return(stats::setNames(list(), character()))
    }
    if (probe_concurrency <= 1L || length(urls) <= 1L) {
        return(stats::setNames(lapply(urls, query_result_probe_url, timeout = timeout, network_policy = network_policy), urls))
    }

    out <- vector("list", length(urls))
    names(out) <- urls
    failed <- rep(FALSE, length(urls))
    ok <- tryCatch({
        if (is.null(network_policy)) {
            network_policy <- list()
        }
        connect_timeout <- network_policy$connect_timeout
        if (is.null(connect_timeout)) {
            connect_timeout <- min(timeout, 3)
        }
        ssl_verifypeer <- network_policy$ssl_verifypeer
        if (is.null(ssl_verifypeer)) {
            ssl_verifypeer <- TRUE
        }
        pool <- curl::new_pool(total_con = probe_concurrency, host_con = probe_concurrency)
        for (i in seq_along(urls)) {
            local({
                j <- i
                start <- Sys.time()
                handle <- downloader__curl_handle(
                    timeout = timeout,
                    connect_timeout = connect_timeout,
                    ssl_verifypeer = ssl_verifypeer,
                    proxy = network_policy$proxy,
                    useragent = network_policy$useragent,
                    nobody = TRUE
                )
                curl::handle_setopt(handle, url = urls[[j]])
                curl::multi_add(
                    handle,
                    done = function(response) {
                        out[[j]] <<- list(
                            latency = as.numeric(difftime(Sys.time(), start, units = "secs")),
                            throughput = NA_real_
                        )
                    },
                    fail = function(error) {
                        failed[[j]] <<- TRUE
                    },
                    pool = pool
                )
            })
        }
        curl::multi_run(timeout = max(timeout * length(urls), 1), poll = TRUE, pool = pool)
        TRUE
    }, error = function(e) FALSE)

    if (!isTRUE(ok)) {
        return(stats::setNames(lapply(urls, query_result_probe_url, timeout = timeout, network_policy = network_policy), urls))
    }
    missing <- vapply(out, is.null, logical(1L)) | failed
    if (any(missing)) {
        fallback <- lapply(urls[missing], query_result_probe_url, timeout = timeout, network_policy = network_policy)
        out[missing] <- fallback
    }
    out
}

query_result_probe_urls <- function(urls, data_node = NULL, service = "HTTPServer", timeout = 5,
                                    network_policy = NULL, node_stats = NULL, node_policy = NULL,
                                    probe_concurrency = 1L, probe_cache_seconds = 3600L) {
    checkmate::assert_character(urls, any.missing = TRUE)
    checkmate::assert_count(probe_concurrency, positive = TRUE)
    if (!is.null(probe_cache_seconds)) {
        checkmate::assert_count(probe_cache_seconds, positive = FALSE)
    }
    n <- length(urls)
    out <- data.table::data.table(
        url = urls,
        probe_latency = rep(NA_real_, n),
        probe_throughput = rep(NA_real_, n),
        probe_cached = rep(FALSE, n)
    )
    if (!n) {
        return(out)
    }

    if (!is.null(data_node) && !is.null(node_stats) && !is.null(probe_cache_seconds) && probe_cache_seconds > 0L) {
        stats <- query_result_normalize_node_stats(node_stats, service = service, node_policy = node_policy)
        if (!is.null(stats) && nrow(stats)) {
            data_node <- as.character(data_node)
            stats <- stats[!duplicated(data_node)]
            idx <- match(data_node, stats$data_node)
            has <- !is.na(idx)
            cache_time <- as.POSIXct(rep(NA, n), origin = "1970-01-01", tz = "UTC")
            if (any(has)) {
                last_probe <- stats$node_last_probe_at[idx[has]]
                updated <- stats$node_updated_at[idx[has]]
                cache_time[has] <- last_probe
                missing_time <- is.na(cache_time[has])
                cache_time[which(has)[missing_time]] <- updated[missing_time]
            }
            fresh <- !is.na(cache_time) & cache_time >= Sys.time() - probe_cache_seconds
            success <- has &
                (
                    suppressWarnings(as.integer(stats$node_probe_success_count[idx])) > 0L |
                        suppressWarnings(as.integer(stats$node_success_count[idx])) > 0L
                )
            latency <- suppressWarnings(as.numeric(stats$node_avg_latency[idx]))
            use_cache <- fresh & success & !is.na(latency)
            out[use_cache, `:=`(
                probe_latency = latency[use_cache],
                probe_throughput = NA_real_,
                probe_cached = TRUE
            )]
        }
    }

    probe_urls <- unique(out[!probe_cached & !is.na(url) & nzchar(url), url])
    probes <- query_result_probe_urls_network(
        probe_urls,
        timeout = timeout,
        network_policy = network_policy,
        probe_concurrency = as.integer(probe_concurrency)
    )
    if (length(probes)) {
        for (url in names(probes)) {
            probe <- probes[[url]]
            target_url <- url
            out[out[["url"]] == target_url & !out[["probe_cached"]], `:=`(
                probe_latency = as.numeric(probe$latency),
                probe_throughput = as.numeric(probe$throughput)
            )]
        }
    }
    out[]
}

query_result_normalize_node_policy <- function(node_policy = NULL) {
    if (exists("downloader__node_policy_defaults", mode = "function")) {
        return(downloader__node_policy_defaults(node_policy))
    }
    if (is.null(node_policy)) {
        return(list(history_ttl_seconds = 14L * 24L * 3600L))
    }
    node_policy
}

query_result_normalize_node_stats <- function(node_stats, service = "HTTPServer", node_policy = NULL) {
    if (is.null(node_stats)) {
        return(NULL)
    }
    node_policy <- query_result_normalize_node_policy(node_policy)
    stats <- data.table::as.data.table(node_stats)
    required <- c("data_node", "service", "success_count", "failure_count", "avg_latency")
    if (!all(required %in% names(stats))) {
        return(NULL)
    }
    wanted_service <- service
    stats <- stats[stats[["service"]] == wanted_service]
    if (!nrow(stats)) {
        return(NULL)
    }
    if ("updated_at" %in% names(stats) && !is.null(node_policy$history_ttl_seconds)) {
        updated_at <- as.POSIXct(stats$updated_at, tz = "UTC")
        fresh <- is.na(updated_at) | updated_at >= Sys.time() - node_policy$history_ttl_seconds
        stats <- stats[fresh]
        if (!nrow(stats)) {
            return(NULL)
        }
    }
    stats[, node_success_count := suppressWarnings(as.integer(success_count))]
    stats[, node_failure_count := suppressWarnings(as.integer(failure_count))]
    stats[is.na(node_success_count), node_success_count := 0L]
    stats[is.na(node_failure_count), node_failure_count := 0L]
    stats[, node_attempt_count := node_success_count + node_failure_count]
    stats[, node_success_rate := data.table::fifelse(
        node_attempt_count > 0L,
        node_success_count / node_attempt_count,
        NA_real_
    )]
    stats[, node_avg_latency := suppressWarnings(as.numeric(avg_latency))]
    if ("probe_success_count" %in% names(stats)) {
        stats[, node_probe_success_count := suppressWarnings(as.integer(probe_success_count))]
        stats[is.na(node_probe_success_count), node_probe_success_count := 0L]
    } else {
        stats[, node_probe_success_count := NA_integer_]
    }
    if ("probe_failure_count" %in% names(stats)) {
        stats[, node_probe_failure_count := suppressWarnings(as.integer(probe_failure_count))]
        stats[is.na(node_probe_failure_count), node_probe_failure_count := 0L]
    } else {
        stats[, node_probe_failure_count := NA_integer_]
    }
    if ("cooldown_until" %in% names(stats)) {
        stats[, node_cooldown_until := as.POSIXct(cooldown_until, tz = "UTC")]
        stats[, node_is_cooling_down := !is.na(node_cooldown_until) & node_cooldown_until > Sys.time()]
    } else {
        stats[, node_cooldown_until := as.POSIXct(NA)]
        stats[, node_is_cooling_down := FALSE]
    }
    if ("updated_at" %in% names(stats)) {
        stats[, node_updated_at := as.POSIXct(updated_at, tz = "UTC")]
    } else {
        stats[, node_updated_at := as.POSIXct(NA)]
    }
    if ("last_probe_at" %in% names(stats)) {
        stats[, node_last_probe_at := as.POSIXct(last_probe_at, tz = "UTC")]
    } else {
        stats[, node_last_probe_at := as.POSIXct(NA)]
    }
    stats[, .(
        data_node,
        service,
        node_success_count,
        node_failure_count,
        node_attempt_count,
        node_success_rate,
        node_avg_latency,
        node_probe_success_count,
        node_probe_failure_count,
        node_cooldown_until,
        node_is_cooling_down,
        node_updated_at,
        node_last_probe_at
    )]
}

query_result_apply_node_stats <- function(plan, node_stats, service = "HTTPServer", node_policy = NULL) {
    stats <- query_result_normalize_node_stats(node_stats, service = service, node_policy = node_policy)
    if (is.null(stats) || !nrow(plan)) {
        plan[, `:=`(
            node_success_count = NA_integer_,
            node_failure_count = NA_integer_,
            node_attempt_count = NA_integer_,
            node_success_rate = NA_real_,
            node_avg_latency = NA_real_,
            node_probe_success_count = NA_integer_,
            node_probe_failure_count = NA_integer_,
            node_cooldown_until = as.POSIXct(NA),
            node_is_cooling_down = FALSE,
            node_updated_at = as.POSIXct(NA),
            node_last_probe_at = as.POSIXct(NA),
            node_cooldown_rank = 0L
        )]
        return(plan[])
    }
    out <- merge(plan, stats, by = c("data_node", "service"), all.x = TRUE, sort = FALSE)
    out[is.na(node_is_cooling_down), node_is_cooling_down := FALSE]
    out[, node_cooldown_rank := data.table::fifelse(node_is_cooling_down, 1L, 0L)]
    out[, all_candidates_cooling := all(node_cooldown_rank == 1L), by = "logical_file_id"]
    out[all_candidates_cooling %in% TRUE, node_cooldown_rank := 0L]
    out[, all_candidates_cooling := NULL]
    out[]
}

query_result_download_plan <- function(result, service = "HTTPServer", probe = FALSE,
                                       strategy = c("fastest", "first", "stable"),
                                       node_stats = NULL, network_policy = NULL,
                                       node_policy = NULL, probe_concurrency = 1L,
                                       probe_cache_seconds = 3600L) {
    strategy <- match.arg(strategy)
    checkmate::assert_string(service)
    checkmate::assert_flag(probe)
    checkmate::assert_count(probe_concurrency, positive = TRUE)
    checkmate::assert_count(probe_cache_seconds, positive = FALSE)

    dt <- result$to_data_table()
    n <- nrow(dt)
    if (!n) {
        return(data.table::data.table())
    }
    urls <- priv(result)$get_url(service, service)
    filename <- if ("filename" %in% result$fields) result$filename else query_result_column(dt, "title")
    plan <- data.table::data.table(
        logical_file_id = query_result_logical_file_id(dt),
        record_index = seq_len(n),
        file_key = query_result_column(dt, "file_key"),
        esgf_id = query_result_column(dt, "id"),
        dataset_id = query_result_column(dt, "dataset_id"),
        filename = filename,
        subdir = NA_character_,
        checksum = query_result_column(dt, "checksum"),
        checksum_type = tolower(query_result_column(dt, "checksum_type", "sha256")),
        size = suppressWarnings(as.numeric(query_result_column(dt, "size", NA_real_))),
        url = urls,
        service = service,
        data_node = query_result_column(dt, "data_node"),
        priority = seq_len(n),
        probe_latency = NA_real_,
        probe_throughput = NA_real_,
        probe_cached = FALSE
    )
    plan <- plan[!is.na(url) & nzchar(url)]
    if (!nrow(plan)) {
        return(plan)
    }
    if (probe) {
        probes <- query_result_probe_urls(
            plan$url,
            data_node = plan$data_node,
            service = service,
            network_policy = network_policy,
            node_stats = node_stats,
            node_policy = node_policy,
            probe_concurrency = probe_concurrency,
            probe_cache_seconds = probe_cache_seconds
        )
        plan[, probe_latency := probes$probe_latency]
        plan[, probe_throughput := probes$probe_throughput]
        plan[, probe_cached := probes$probe_cached]
    }
    plan <- query_result_apply_node_stats(plan, node_stats = node_stats, service = service, node_policy = node_policy)
    if (identical(strategy, "fastest")) {
        plan[, probe_missing := is.na(probe_latency)]
        plan[, node_missing := is.na(node_success_rate)]
        data.table::setorderv(
            plan,
            c("logical_file_id", "node_cooldown_rank", "probe_missing", "probe_latency", "node_missing", "node_success_rate", "node_avg_latency", "priority"),
            c(1L, 1L, 1L, 1L, 1L, -1L, 1L, 1L)
        )
        plan[, c("probe_missing", "node_missing") := NULL]
    } else if (identical(strategy, "stable")) {
        plan[, node_missing := is.na(node_success_rate)]
        data.table::setorderv(
            plan,
            c("logical_file_id", "node_cooldown_rank", "node_missing", "node_success_rate", "data_node", "url", "priority"),
            c(1L, 1L, 1L, -1L, 1L, 1L, 1L)
        )
        plan[, node_missing := NULL]
    } else {
        data.table::setorderv(plan, c("logical_file_id", "priority"))
    }
    plan[, priority := seq_len(.N), by = "logical_file_id"]
    plan[]
}

query_result_expand_replicas <- function(result, service = "HTTPServer", all = TRUE) {
    dt <- result$to_data_table()
    master_id <- query_result_column(dt, "master_id")
    master_id <- unique(master_id[!is.na(master_id) & nzchar(master_id)])
    if (!length(master_id)) {
        return(result)
    }

    store <- QueryParamStore$new()
    store$params(master_id = master_id)
    store$replica(NULL)
    store$latest(TRUE)
    store$distrib(TRUE)
    store$type("File")
    store$format(FORMAT_JSON)
    store$fields("*")
    store$limit(this$data_max_limit)
    store$offset(0L)

    collected <- query_collect(
        priv(result)$index_node,
        store,
        required_fields = EsgResultFile$private_fields$required_fields,
        all = all,
        limit = this$data_max_limit,
        constraints = FALSE
    )
    new_query_result(
        EsgResultFile,
        priv(result)$index_node,
        collected$parameter,
        collected$response,
        context = collected$context
    )
}

query_result_run_http_fallback <- function(result, indices, downloader, session_label = NULL, progress = TRUE) {
    checkmate::assert_integerish(indices, lower = 1L, any.missing = FALSE, min.len = 1L)
    if (is.null(downloader)) {
        cli::cli_abort("HTTP fallback requires an explicit `store` or `downloader` so downloaded files are recoverable.")
    }

    plan <- result$download_plan(replica = "current", service = "HTTPServer", probe = FALSE)
    plan <- plan[record_index %in% indices]
    if (!nrow(plan)) {
        cli::cli_abort("HTTPServer download URLs are missing for one or more file records.")
    }

    missing <- setdiff(as.integer(indices), unique(plan$record_index))
    if (length(missing)) {
        cli::cli_abort("HTTPServer download URLs are missing for one or more file records.")
    }

    session_id <- downloader$enqueue(plan, session_label = session_label)
    tasks <- downloader$run(session_id = session_id, progress = progress)
    failed <- tasks[!tasks[["status"]] %in% c("done", "skipped"), , drop = FALSE]
    if (nrow(failed)) {
        cli::cli_abort("HTTP fallback download failed for {nrow(failed)} task(s). Inspect downloader$status(session_id = {.val {session_id}}) for details.")
    }

    by_logical_file <- stats::setNames(tasks$target_path, tasks$logical_file_id)
    paths <- vapply(as.integer(indices), function(index) {
        row <- plan[record_index == index][1L]
        path <- by_logical_file[[row$logical_file_id[[1L]]]]
        if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
            cli::cli_abort("HTTP fallback completed but the downloaded file cannot be found for record {index}.")
        }
        path
    }, character(1L))
    unname(paths)
}

query_result_parse_drs_bound <- function(value, end = FALSE) {
    if (is.na(value) || !nzchar(value)) {
        return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    }

    width <- nchar(value)
    if (!width %in% c(4L, 6L, 8L, 10L, 12L)) {
        return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    }

    start <- switch(
        as.character(width),
        `4` = sprintf("%s-01-01 00:00:00", value),
        `6` = sprintf("%s-%s-01 00:00:00", substr(value, 1L, 4L), substr(value, 5L, 6L)),
        `8` = sprintf(
            "%s-%s-%s 00:00:00",
            substr(value, 1L, 4L),
            substr(value, 5L, 6L),
            substr(value, 7L, 8L)
        ),
        `10` = sprintf(
            "%s-%s-%s %s:00:00",
            substr(value, 1L, 4L),
            substr(value, 5L, 6L),
            substr(value, 7L, 8L),
            substr(value, 9L, 10L)
        ),
        `12` = sprintf(
            "%s-%s-%s %s:%s:00",
            substr(value, 1L, 4L),
            substr(value, 5L, 6L),
            substr(value, 7L, 8L),
            substr(value, 9L, 10L),
            substr(value, 11L, 12L)
        )
    )
    parsed <- as.POSIXct(start, tz = "UTC")
    if (is.na(parsed) || !isTRUE(end)) {
        return(parsed)
    }

    increment <- switch(
        as.character(width),
        `4` = "year",
        `6` = "month",
        `8` = "day",
        `10` = "hour",
        `12` = "min"
    )
    seq(parsed, by = increment, length.out = 2L)[[2L]] - 1
}

query_result_parse_drs_ranges <- function(labels) {
    start <- as.POSIXct(rep(NA_real_, length(labels)), origin = "1970-01-01", tz = "UTC")
    end <- start

    for (i in seq_along(labels)) {
        label <- labels[[i]]
        if (is.na(label) || !nzchar(label)) {
            next
        }

        label <- basename(sub("\\.html$", "", sub("[?#].*$", "", label)))
        matches <- gregexpr("_([0-9]{4}|[0-9]{6}|[0-9]{8}|[0-9]{10}|[0-9]{12})-([0-9]{4}|[0-9]{6}|[0-9]{8}|[0-9]{10}|[0-9]{12})(?=\\.nc$|$)", label, perl = TRUE)
        hit <- regmatches(label, matches)[[1L]]
        if (!length(hit) || identical(hit, -1L)) {
            next
        }

        range <- sub("^_", "", hit[[length(hit)]])
        parts <- strsplit(range, "-", fixed = TRUE)[[1L]]
        if (length(parts) != 2L || nchar(parts[[1L]]) != nchar(parts[[2L]])) {
            next
        }

        start[[i]] <- query_result_parse_drs_bound(parts[[1L]], end = FALSE)
        end[[i]] <- query_result_parse_drs_bound(parts[[2L]], end = TRUE)
    }

    data.frame(datetime_start = start, datetime_end = end, check.names = FALSE)
}

query_result_normalize_type <- function(type, choices = c("Dataset", "File", "Aggregation")) {
    checkmate::assert_string(type)
    type <- tolower(type)
    map <- c(dataset = "Dataset", file = "File", aggregation = "Aggregation")
    if (!type %in% names(map)) {
        stop(sprintf(
            "`type` must be one of %s.",
            paste(sprintf("'%s'", choices), collapse = ", ")
        ), call. = FALSE)
    }

    type <- unname(map[[type]])
    checkmate::assert_choice(type, choices)
    type
}

query_result_merge_params <- function(store, params) {
    if (!length(params)) {
        return(store)
    }

    extra <- query_param_as_store(params)$state(null = FALSE)
    state <- store$state(null = TRUE)
    for (bucket in names(extra)) {
        state[[bucket]][names(extra[[bucket]])] <- extra[[bucket]]
    }

    store$restore(state)
}
# }}}

# EsgResultDataset {{{
#' ESGF Query results for `Dataset` type
#'
#' @description
#'
#' `EsgResultDataset` is a class that represents query results for
#' `Dataset` type from ESGF search RESTful API.
#'
#' In general, there is no need to create an `EsgResultDataset` manually.
#' Usually, it is created by calling
#' \href{#method-EsgQuery-collect}{\code{EsgQuery$collect()}}.
#'
#' @author Hongyuan Jia
#' @name EsgResultDataset
#' @keywords internal
EsgResultDataset <- R6::R6Class(
    "EsgResultDataset",
    inherit = EsgResult,
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # to_data_table {{{
        #' @description
        #' Convert the results into a [data.table][data.table::data.table()]
        #'
        #' @param fields A non-empty character vector indicating the fields to
        #'        put into the `data.table`. If `NULL`, all fields in the query
        #'        result will be used. Default: `NULL`.
        #'
        #' @param formatted Whether to use formatted values for special fields,
        #'        including `url` and `size`. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_data_table = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_data_table(fields, if (formatted) c("url", "size"))
        },
        # }}}

        # has_opendap {{{
        #' @description
        #' Check if there are OPeNDAP support for the datasets
        #'
        #' @return A logical vector.
        #'
        has_opendap = function() {
            private$has_access("OPENDAP")
        },
        # }}}

        # has_download {{{
        #' @description
        #' Check if there are HTTPServer download URL for the datasets
        #'
        #' @return A logical vector.
        #'
        has_download = function() {
            private$has_access("HTTPServer")
        },
        # }}}

        # collect {{{
        #' @description
        #' Collect file or aggregation information for current datasets
        #'
        #' `$collect()` sends a query with **`type=File`** or
        #' **`type=aggregation` (based on the specified `type`) for current
        #' datasets and returns an
        #' [EsgResultFile] or
        #' [EsgResultAggregation] object, respectively.
        #'
        #' The following fields are always included in the results:
        #'
        #' - For `File` query: `r paste0("\\verb{", EsgResultFile$private_fields$required_fields, "}", collapse = ", ")`.
        #'
        #' - For `Aggregation` query: `r paste0("\\verb{", EsgResultAggregation$private_fields$required_fields, "}", collapse = ", ")`.
        #'
        #' @param which A character vector giving the value of dataset ID or an
        #'        integer vector giving the indices of the dataset. If `NULL`,
        #'        all datasets will be sent. Empty dataset results return empty
        #'        child results without sending another ESGF query. Default:
        #'        `NULL`.
        #'
        #' @param fields A character vector indicating the value of `fields`
        #'        parameter when sending the query. If `NULL`, all available
        #'        fields will be included. Default: `NULL`.
        #'
        #' @param all A flag. Whether to collect all results. Default: `FALSE`.
        #'
        #' @param limit If `all = FALSE`, the maximum number of child records
        #'        to collect in this request. If `all = TRUE`, the page size
        #'        used for each paginated request, not a total cap. If `NULL`,
        #'        the allowed maximum limit number `r this$data_max_limit` is
        #'        used. Default: `100L`.
        #'
        #' @param type A string indicating the query type. Should be one of
        #'        `File` or `Aggregation`. Default: `"File"`.
        #'
        #' @param index_node Optional ESGF index node used for the child query.
        #'        If `NULL`, the index node that created the Dataset result is
        #'        used. Default: `NULL`.
        #'
        #' @param ... Additional child-result facet filters, plus the control
        #'        parameters `replica`, `distrib`, `latest`, and `shards`.
        #'        Query-level parameters such as `datetime_start` and
        #'        `datetime_stop` cannot be passed through `...`.
        #'        File/Aggregation collection does not use ESGF datetime search
        #'        parameters; call `$filter_time()` on the returned result for
        #'        time filtering. If control parameters are omitted, they are
        #'        inherited from the dataset query when available, with
        #'        `distrib = TRUE` and `latest = TRUE` as fallbacks. For details
        #'        on possible parameters, please see [esg_query()].
        #'        `Aggregation` collection uses `dataset_id` plus explicit child filters
        #'        in `...`; parent Dataset facet filters are not inherited because
        #'        Aggregation records on standard ESGF search nodes do not necessarily
        #'        expose the same facet fields as Dataset records.
        #'
        #' @return
        #'
        #' - If `type="File"`, an [EsgResultFile] object
        #' - If `type="Aggregation"`, an [EsgResultAggregation] object
        #'
        collect = function(which = NULL, fields = NULL, all = FALSE, limit = 100L, type = "File", index_node = NULL, ...) {
            type <- query_result_normalize_type(type, choices = c("File", "Aggregation"))
            child_index_node <- if (is.null(index_node)) {
                private$index_node
            } else {
                checkmate::assert_string(index_node)
                normalize_index_node(index_node)
            }
            if (!is.null(which)) {
                if (!self$count()) {
                    stop("Cannot select records from an empty Dataset result.", call. = FALSE)
                } else if (is.character(which)) {
                    checkmate::assert_character(which, any.missing = FALSE, min.len = 1L, unique = TRUE)
                    checkmate::assert_subset(which, self$id, empty.ok = FALSE)
                } else {
                    checkmate::assert_integerish(
                        which,
                        lower = 1L,
                        upper = self$count(),
                        any.missing = FALSE,
                        unique = TRUE
                    )
                }

                which <- if (is.character(which)) match(which, self$id) else as.integer(which)
            }

            built <- private$build_params(
                fields = fields,
                limit = limit,
                type = type,
                index = which,
                ...
            )
            params <- built$params
            limit <- built$limit

            req_fld <- if (type == "File") {
                EsgResultFile$private_fields$required_fields
            } else if (type == "Aggregation") {
                EsgResultAggregation$private_fields$required_fields
            }

            if (self$count() == 0L) {
                result <- query_result_empty_response(params)
            } else {
                result <- query_collect(
                    child_index_node,
                    params,
                    required_fields = req_fld,
                    all = all,
                    limit = limit,
                    constraints = FALSE
                )
            }

            # replace docs in the last response
            result$response$response$docs <- result$docs
            result_params <- if (!is.null(result$parameter)) result$parameter else params

            # create new results
            if (type == "File") {
                new_query_result(
                    EsgResultFile,
                    child_index_node,
                    result_params,
                    result$response,
                    context = result$context
                )
            } else if (type == "Aggregation") {
                new_query_result(
                    EsgResultAggregation,
                    child_index_node,
                    result_params,
                    result$response,
                    context = result$context
                )
            }
        },
        # }}}

        # print {{{
        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgResultDataset` object itself, invisibly.
        print = function(n = 10L) {
            private$print_header("Dataset")
            private$print_summary("Dataset")
            private$print_parameters()
            cli::cat_line()
            private$print_contents("Dataset", n)
            invisible(self)
        }
        # }}}
    ),

    private = list(
        result_type = "Dataset",

        required_fields = sort(unique(c(
            EsgResult$private_fields$required_fields,
            "index_node",
            "number_of_files",
            "number_of_aggregations",
            "access"
        ))),

        # build_params {{{
        build_params = function(fields = NULL, limit = 100L, type = "File", index = NULL, ...) {
            type <- query_result_normalize_type(type, choices = c("File", "Aggregation"))

            checkmate::assert_integerish(limit, lower = 1L, upper = this$data_max_limit, len = 1L, null.ok = TRUE)
            if (is.null(limit)) {
                limit <- this$data_max_limit
            }

            overrides <- eval(substitute(alist(...)))
            extra_params <- list()
            if (length(overrides)) {
                names_reserved <- c(
                    "dataset_id", "fields", "facets", "type", "format",
                    "limit", "offset", "query", "_timestamp", "time",
                    query_param_names("query")
                )
                overrides <- eval_with_bang(...)

                # stop if unsupported parameter found
                names_params <- names(overrides)
                checkmate::assert_names(names_params, type = "unique", .var.name = "...")
                if (any(!nzchar(names_params))) {
                    stop("All additional query filters in `...` must be named.", call. = FALSE)
                }
                if (any(invld <- names_params %in% names_reserved)) {
                    stop(sprintf(
                        "The following query parameter(s) are controlled by `$collect()` and cannot be set in `...`: [%s].",
                        paste(sprintf("'%s'", names_params[invld]), collapse = ", ")
                    ), call. = FALSE)
                }

                names_ctrl <- c("replica", "distrib", "latest", "shards")
                extra_params <- overrides[!names_params %in% names_ctrl]
                names_extra <- names(extra_params)
                if (length(names_extra)) {
                    not_found <- setdiff(names_extra, FIELDS_FACETS_ALL)
                    if (length(not_found)) {
                        warning(
                            sprintf(
                                "The following facet(s) are not listed in the built-in ESGF facet dictionary and will be sent as-is: [%s].",
                                paste(sprintf("'%s'", not_found), collapse = ", ")
                            ),
                            call. = FALSE
                        )
                    }
                }
                extra_params <- stats::setNames(
                    lapply(seq_along(extra_params), function(i) {
                        param <- extra_params[[i]]
                        if (is.null(param$value)) {
                            return(NULL)
                        }
                        query_param_meta(
                            QueryParamFacet(
                                param$value,
                                negate = isTRUE(param$negate),
                                encoded = FALSE
                            ),
                            name = names(extra_params)[[i]],
                            kind = "facet"
                        )
                    }),
                    names(extra_params)
                )
                extra_params <- extra_params[!vapply(extra_params, is.null, logical(1L))]

                overrides <- overrides[names_params %in% names_ctrl]
                if (length(overrides) && any(vapply(overrides, function(param) isTRUE(param$negate), logical(1L)))) {
                    stop("Control parameters in `...` do not support negation.", call. = FALSE)
                }
            }

            param_value <- function(param) {
                if (is.null(param)) {
                    return(NULL)
                }
                if (S7::S7_inherits(param, QueryParam)) {
                    return(query_param_value(param))
                }
                param$value
            }
            inherited_value <- function(name) {
                if (name %in% names(overrides)) {
                    return(param_value(overrides[[name]]))
                }
                param_value(.subset2(private$parameter, name)())
            }

            controls <- list(
                shards = inherited_value("shards"),
                replica = inherited_value("replica"),
                latest = inherited_value("latest"),
                distrib = inherited_value("distrib")
            )
            if (is.null(controls$latest)) {
                controls$latest <- TRUE
            }
            if (is.null(controls$distrib)) {
                controls$distrib <- TRUE
            }

            dataset_id <- if (is.null(index)) self$id else self$id[index]
            if (!length(dataset_id)) {
                dataset_id <- NULL
            }

            # create a new query to validate params
            query <- esg_query(private$index_node)
            query$distrib(controls$distrib)

            store <- if (type == "Aggregation") {
                QueryParamStore$new()
            } else {
                private$parameter$copy()
            }
            if (type == "Aggregation") {
                store$project(NULL)
            }
            query_result_merge_params(store, c(extra_params, list(dataset_id = dataset_id)))
            store$fields(query_param_value(query$fields(fields)$fields()))
            store$shards(query_param_value(query$shards(controls$shards)$shards()))
            store$replica(query_param_value(query$replica(controls$replica)$replica()))
            store$latest(query_param_value(query$latest(controls$latest)$latest()))
            store$distrib(query_param_value(query$distrib()))
            store$limit(limit)
            store$offset(0L)
            store$type(type)
            store$format(FORMAT_JSON)
            store$facets(NULL)
            store$datetime_range(start = NULL, stop = NULL)

            list(params = store, limit = limit)
        }
        # }}}
    )
)
# }}}

# EsgResultFile {{{
#' ESGF Query results for `File` type
#'
#' @description
#'
#' `EsgResultFile` is a class that represents query results for
#' `File` type from ESGF search RESTful API.
#'
#' In general, there is no need to create an `EsgResultDataset` manually.
#' Usually, it is created by calling
#' \href{#method-EsgResultDataset-collect}{\code{EsgResultDataset$collect()}}.
#'
#' @author Hongyuan Jia
#' @name EsgResultFile
#' @keywords internal
EsgResultFile <- R6::R6Class(
    "EsgResultFile",
    inherit = EsgResult,
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # to_data_table {{{
        #' @description
        #' Convert the results into a [data.table][data.table::data.table()]
        #'
        #' @param fields A non-empty character vector indicating the fields to
        #'        put into the `data.table`. If `NULL`, all fields in the query
        #'        result will be used. Default: `NULL`.
        #'
        #' @param formatted Whether to use formatted values for special fields,
        #'        including `url` and `size`. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_data_table = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_data_table(fields, if (formatted) c("url", "size"))
        },
        # }}}

        # filter_time {{{
        #' @description
        #' Filter file records by the time range covered by each file
        #'
        #' `method = "drs"` parses the time range from CMIP/DRS-style NetCDF
        #' filenames in `title`, URL basenames, or `id`. This is fast and does
        #' not open files, but it depends on the ESGF filename convention.
        #' Records whose time range cannot be parsed are kept and reported with
        #' a warning.
        #'
        #' `method = "opendap"` opens each OPeNDAP URL and reads the NetCDF
        #' time axis to determine the file range. This is more exact, but much
        #' slower and requires OPeNDAP access.
        #'
        #' The requested time filter is recorded on the returned result and is
        #' carried into `EsgDataset$read_region()` when a dataset is opened from
        #' the filtered result.
        #'
        #' @param start,stop Time range boundaries. Character, `Date`, and
        #'        `POSIXt` inputs are accepted and parsed in UTC.
        #' @param method How to determine file time ranges. One of `"drs"` or
        #'        `"opendap"`. Default: `"drs"`.
        #'
        #' @return A new `EsgResultFile` object.
        filter_time = function(start, stop, method = c("drs", "opendap")) {
            private$filter_time_result(start, stop, method = method, result_label = "file")
        },
        # }}}

        # download_plan {{{
        #' @description
        #' Build a persistent downloader plan for file records.
        #'
        #' @param replica Whether to use current records or expand known replicas.
        #' @param service ESGF URL service to download from. Default: `"HTTPServer"`.
        #' @param probe Whether to lightly probe URLs before ranking them.
        #' @param strategy Candidate ranking strategy.
        #' @param all Whether replica expansion should retrieve all matching records.
        #' @param node_stats Optional data node history from
        #'        `Downloader$data_nodes()`.
        #' @param network_policy Optional network options from
        #'        `Downloader$network_policy`.
        #' @param node_policy Optional data-node cooldown policy from
        #'        `Downloader$node_policy`.
        #' @param probe_concurrency Maximum concurrent URL probes. Default: `1`.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #'
        #' @return A data.table download plan.
        download_plan = function(replica = c("auto", "current"), service = "HTTPServer",
                                 probe = TRUE, strategy = c("fastest", "first", "stable"),
                                 all = TRUE, node_stats = NULL, network_policy = NULL,
                                 node_policy = NULL, probe_concurrency = 1L,
                                 probe_cache_seconds = 3600L) {
            replica <- match.arg(replica)
            strategy <- match.arg(strategy)
            target <- if (identical(replica, "auto")) {
                self$expand_replicas(service = service, all = all)
            } else {
                self
            }
            query_result_download_plan(
                target,
                service = service,
                probe = probe,
                strategy = strategy,
                node_stats = node_stats,
                network_policy = network_policy,
                node_policy = node_policy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds
            )
        },
        # }}}

        # expand_replicas {{{
        #' @description
        #' Query ESGF for master and replica records for known `master_id`s.
        #'
        #' @param service ESGF URL service to keep in the method contract.
        #'        Default: `"HTTPServer"`.
        #' @param all Whether to retrieve all matching records. Default:
        #'        `TRUE`.
        #'
        #' @return A new `EsgResultFile` object with expanded replica records
        #'        when `master_id` is available; otherwise `self`.
        expand_replicas = function(service = "HTTPServer", all = TRUE) {
            query_result_expand_replicas(self, service = service, all = all)
        },
        # }}}

        # select_replica {{{
        #' @description
        #' Select the preferred candidate URL per logical file.
        #'
        #' @param strategy Candidate ranking strategy.
        #' @param probe Whether to lightly probe URLs before ranking them.
        #' @param service ESGF URL service to download from. Default:
        #'        `"HTTPServer"`.
        #' @param node_stats Optional data node history from
        #'        `Downloader$data_nodes()`.
        #' @param network_policy Optional network options from
        #'        `Downloader$network_policy`.
        #' @param node_policy Optional data-node cooldown policy from
        #'        `Downloader$node_policy`.
        #' @param probe_concurrency Maximum concurrent URL probes. Default: `1`.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #'
        #' @return A data.table with one selected candidate per logical file.
        select_replica = function(strategy = c("fastest", "first", "stable"), probe = TRUE,
                                  service = "HTTPServer", node_stats = NULL,
                                  network_policy = NULL, node_policy = NULL,
                                  probe_concurrency = 1L, probe_cache_seconds = 3600L) {
            strategy <- match.arg(strategy)
            plan <- self$download_plan(
                replica = "auto",
                service = service,
                probe = probe,
                strategy = strategy,
                node_stats = node_stats,
                network_policy = network_policy,
                node_policy = node_policy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds
            )
            if (!nrow(plan)) {
                return(plan)
            }
            plan[, .SD[which.min(priority)], by = "logical_file_id"]
        },
        # }}}

        # download {{{
        #' @description
        #' Enqueue and run file downloads using a Downloader.
        #'
        #' @param downloader Optional persistent [Downloader]. If `NULL`,
        #'        `store$downloader()` is used when `store` is supplied.
        #' @param store Optional [EsgStore] providing a bound downloader.
        #' @param replica Whether to use current records or expand known
        #'        replicas before downloading.
        #' @param service ESGF URL service to download from. Default:
        #'        `"HTTPServer"`.
        #' @param probe Whether to lightly probe URLs before ranking them.
        #' @param strategy Candidate ranking strategy.
        #' @param probe_concurrency Maximum concurrent URL probes when
        #'        `probe = TRUE`. Default comes from the downloader worker count.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #' @param session_label Optional download session label.
        #' @param run Whether to run the queued session immediately. Default:
        #'        `TRUE`.
        #' @param ... Additional arguments passed to `Downloader$run()`.
        #'
        #' @return The created downloader session ID.
        download = function(downloader = NULL, store = NULL, replica = c("auto", "current"),
                            service = "HTTPServer", probe = TRUE, strategy = c("fastest", "first", "stable"),
                            probe_concurrency = NULL, probe_cache_seconds = 3600L,
                            session_label = NULL, run = TRUE, ...) {
            replica <- match.arg(replica)
            strategy <- match.arg(strategy)
            if (is.null(downloader)) {
                if (!is.null(store)) {
                    downloader <- store$downloader()
                } else {
                    cli::cli_abort("`download()` requires an explicit `store` or persistent `downloader`.")
                }
            }
            node_stats <- tryCatch(downloader$data_nodes(service = service), error = function(e) NULL)
            network_policy <- tryCatch(downloader$network_policy, error = function(e) NULL)
            node_policy <- tryCatch(downloader$node_policy, error = function(e) NULL)
            if (is.null(probe_concurrency)) {
                probe_concurrency <- min(max(downloader$n_workers, 1L), 8L)
            }
            plan <- self$download_plan(
                replica = replica,
                service = service,
                probe = probe,
                strategy = strategy,
                node_stats = node_stats,
                network_policy = network_policy,
                node_policy = node_policy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds
            )
            tryCatch(downloader$record_probes(plan, probed = probe), error = function(e) NULL)
            session_id <- downloader$enqueue(plan, session_label = session_label)
            if (isTRUE(run)) {
                downloader$run(session_id = session_id, ...)
            }
            session_id
        },
        # }}}

        # print {{{
        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgResultFile` object itself, invisibly.
        print = function(n = 10L) {
            private$print_header("File")
            private$print_summary("File")
            private$print_parameters()
            cli::cat_line()
            private$print_contents("File", n)
            invisible(self)
        },
        # }}}

        # open_dataset {{{
        #' @description
        #' Open a file as an EsgDataset for remote data access via OPeNDAP
        #'
        #' @param which File records to open. Use integer indices or file IDs.
        #'        If `NULL`, all file records are opened when `aggregate = TRUE`
        #'        and the first record is opened when `aggregate = FALSE`.
        #'        Default: `NULL`.
        #' @param aggregate Whether to open multiple selected files as one
        #'        aggregated [EsgDataset]. If `FALSE`, exactly one file record
        #'        must be selected. Default: `TRUE`.
        #' @param fallback What to do if OPeNDAP is unavailable. One of:
        #'   - `"ask"`: Interactively ask the user (default). In a
        #'     non-interactive session this raises an error.
        #'   - `"auto"`: Automatically download the file via HTTPServer.
        #'   - `"error"`: Raise an error.
        #'
        #' @param store Optional [EsgStore] used for recoverable HTTP fallback.
        #' @param downloader Optional persistent [Downloader] used for
        #'        recoverable HTTP fallback.
        #'
        #' @return An `EsgDataset` object with the connection already opened.
        open_dataset = function(which = NULL, aggregate = TRUE, fallback = c("ask", "auto", "error"),
                                store = NULL, downloader = NULL) {
            checkmate::assert_flag(aggregate)
            fallback <- match.arg(fallback)

            if (!self$count()) {
                cli::cli_abort("No file records are available to open.")
            }

            if (is.null(which)) {
                indices <- if (isTRUE(aggregate)) seq_len(self$count()) else 1L
            } else if (is.character(which)) {
                checkmate::assert_character(which, any.missing = FALSE, min.len = 1L, unique = TRUE)
                checkmate::assert_subset(which, self$id, empty.ok = FALSE)
                indices <- match(which, self$id)
            } else {
                checkmate::assert_integerish(
                    which,
                    lower = 1L,
                    upper = self$count(),
                    any.missing = FALSE,
                    min.len = 1L,
                    unique = TRUE
                )
                indices <- as.integer(which)
            }

            if (!isTRUE(aggregate) && length(indices) != 1L) {
                cli::cli_abort("`aggregate = FALSE` can only open one file record.")
            }

            urls <- self$url_opendap[indices]
            targets <- urls
            nc_handles <- vector("list", length(urls))
            opendap_errors <- vector("list", length(urls))
            failed <- rep(FALSE, length(urls))
            missing <- is.na(urls)

            close_preopened_handles <- function() {
                open_pos <- base::which(!vapply(nc_handles, is.null, logical(1L)))
                if (!length(open_pos)) {
                    return(invisible(NULL))
                }

                esg_dataset_close_handles(targets[open_pos], nc_handles[open_pos])
                nc_handles[open_pos] <<- vector("list", length(open_pos))
                invisible(NULL)
            }
            cleanup_preopened <- TRUE
            on.exit(
                if (isTRUE(cleanup_preopened)) {
                    close_preopened_handles()
                },
                add = TRUE
            )

            for (j in base::which(!missing)) {
                d <- NULL
                ok <- tryCatch(
                    {
                        d <- EsgDataset$new(urls[[j]])
                        d$open()
                        handles <- esg_dataset_detach_handles(d)
                        if (!length(handles) || is.null(handles[[1L]])) {
                            stop("Opened EsgDataset does not expose a transferable NetCDF handle.", call. = FALSE)
                        }
                        nc_handles[j] <- handles[1L]
                        TRUE
                    },
                    error = function(e) {
                        if (!is.null(d) && is.function(d$close)) {
                            d$close()
                        }
                        opendap_errors[[j]] <<- e
                        FALSE
                    }
                )
                failed[[j]] <- !ok
            }

            fallback_pos <- base::which(missing | failed)
            if (length(fallback_pos)) {
                missing_pos <- base::which(missing)
                failed_pos <- base::which(failed)
                if (length(missing_pos)) {
                    cli::cli_alert_warning(
                        "OPeNDAP URLs are missing for {private$record_labels(indices[missing_pos])}."
                    )
                }
                if (length(failed_pos)) {
                    cli::cli_alert_warning(
                        "OPeNDAP connection failed for {private$record_labels(indices[failed_pos])}."
                    )
                }

                opendap_error <- if (length(failed_pos)) opendap_errors[[failed_pos[[1L]]]] else NULL
                if (fallback == "error") {
                    details <- c(
                        if (length(missing_pos)) {
                            "x" = "Missing OPeNDAP URL: {private$record_labels(indices[missing_pos])}"
                        },
                        if (length(failed_pos)) {
                            "x" = "Failed OPeNDAP open: {private$record_labels(indices[failed_pos])}"
                        }
                    )
                    cli::cli_abort(
                        c("OPeNDAP is not available for these file records.", details),
                        parent = opendap_error
                    )
                }

                if (fallback == "ask") {
                    if (!interactive()) {
                        cli::cli_abort("Cannot ask for fallback in a non-interactive session. Use fallback = 'auto' to download via HTTP.")
                    } else {
                        answer <- utils::menu(
                            choices = c(
                                sprintf("Download %d file(s) via HTTP", length(fallback_pos)),
                                "Cancel"
                            ),
                            title = "OPeNDAP is not available. What would you like to do?"
                        )
                        if (answer != 1L) {
                            cli::cli_abort("Operation cancelled by user.")
                        }
                    }
                }

                download_urls <- self$url_download[indices[fallback_pos]]
                if (any(is.na(download_urls))) {
                    http_missing_pos <- fallback_pos[is.na(download_urls)]
                    cli::cli_abort(c(
                        "HTTPServer download URLs are missing for one or more file records.",
                        "x" = "Missing HTTPServer URL: {private$record_labels(indices[http_missing_pos])}"
                    ))
                }

                if (is.null(downloader)) {
                    if (!is.null(store)) {
                        downloader <- store$downloader()
                    } else {
                        cli::cli_abort("HTTP fallback requires an explicit `store` or `downloader` so downloaded files are recoverable.")
                    }
                }
                cli::cli_alert_info("Downloading {length(fallback_pos)} file(s) via HTTP as fallback...")
                targets[fallback_pos] <- query_result_run_http_fallback(self, indices[fallback_pos], downloader)
            }

            ds <- EsgDataset$new(targets)
            esg_dataset_adopt_handles(ds, nc_handles)
            if (!isTRUE(ds$is_open)) {
                ds$open()
            }
            esg_dataset_set_context(ds, private$update_selection_context(indices))
            cleanup_preopened <- FALSE
            ds
        }
        # }}}
    ),
    active = list(
        # filename {{{
        #' @field filename A character vector indicating file names on the
        #'        sever.
        filename = function() {
            private$get_field("title")
        },
        # }}}

        # url_opendap {{{
        #' @field url_opendap A character vector of the OPeNDAP URLs of the
        #'        files.
        url_opendap = function() {
            url <- private$get_url("OPENDAP", "OPeNDAP")

            has_html <- !is.na(url) & tools::file_ext(url) == "html"
            if (any(has_html)) {
                url[has_html] <- tools::file_path_sans_ext(url[has_html])
            }
            url
        },
        # }}}

        # url_download {{{
        #' @field url_download A character vector of the download URLs of the
        #'        files.
        url_download = function() {
            private$get_url("HTTPServer")
        },
        # }}}

        # fields {{{
        #' @field fields A character vector indicating all response fields,
        #'        followed by derived fields such as `filename`, `url_opendap`
        #'        and `url_download` when their source fields are available.
        fields = function() {
            fields <- super$fields
            derived <- character()
            if ("title" %in% fields) {
                derived <- c(derived, "filename")
            }
            if ("url" %in% fields) {
                derived <- c(derived, "url_opendap", "url_download")
            }
            unique(c(fields, derived))
        }
        # }}}
    ),
    private = list(
        result_type = "File",

        required_fields = sort(unique(c(
            EsgResult$private_fields$required_fields,
            "dataset_id",
            "checksum",
            "checksum_type",
            "instance_id",
            "master_id",
            "replica",
            "tracking_id",
            "title",
            "version",
            "data_node",
            "activity_id",
            "institution_id"
        )))
    )
)
# }}}

# EsgResultAggregation {{{
#' ESGF Query results for `Aggregation` type
#'
#' @description
#'
#' `EsgResultAggregation` is a class that represents query results for
#' `Aggregation` type from ESGF search RESTful API.
#'
#' In general, there is no need to create an `EsgResultAggregation` manually.
#' Usually, it is created by calling
#' \href{#method-EsgResultDataset-collect}{\code{EsgResultDataset$collect()}}.
#'
#' @author Hongyuan Jia
#' @name EsgResultAggregation
#' @keywords internal
EsgResultAggregation <- R6::R6Class(
    "EsgResultAggregation",
    inherit = EsgResult,
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # to_data_table {{{
        #' @description
        #' Convert the results into a [data.table][data.table::data.table()]
        #'
        #' @param fields A non-empty character vector indicating the fields to
        #'        put into the `data.table`. If `NULL`, all fields in the query
        #'        result will be used. Default: `NULL`.
        #'
        #' @param formatted Whether to use formatted values for special fields,
        #'        including `url` and `size`. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_data_table = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_data_table(fields, if (formatted) c("url", "size"))
        },
        # }}}

        # filter_time {{{
        #' @description
        #' Filter aggregation records by the time range covered by each file
        #'
        #' See `EsgResultFile$filter_time()` for the method semantics.
        #'
        #' @param start,stop Time range boundaries. Character, `Date`, and
        #'        `POSIXt` inputs are accepted and parsed in UTC.
        #' @param method How to determine file time ranges. One of `"drs"` or
        #'        `"opendap"`. Default: `"drs"`.
        #'
        #' @return A new `EsgResultAggregation` object.
        filter_time = function(start, stop, method = c("drs", "opendap")) {
            private$filter_time_result(start, stop, method = method, result_label = "aggregation")
        },
        # }}}

        # download_plan {{{
        #' @description
        #' Build a persistent downloader plan for aggregation records.
        #'
        #' @param replica Replica policy. Aggregation records currently use the
        #'        current records.
        #' @param service ESGF URL service to download from. Default:
        #'        `"HTTPServer"`.
        #' @param probe Whether to lightly probe URLs before ranking them.
        #' @param strategy Candidate ranking strategy.
        #' @param all Reserved for API symmetry with `EsgResultFile`.
        #' @param node_stats Optional data node history from
        #'        `Downloader$data_nodes()`.
        #' @param network_policy Optional network options from
        #'        `Downloader$network_policy`.
        #' @param node_policy Optional data-node cooldown policy from
        #'        `Downloader$node_policy`.
        #' @param probe_concurrency Maximum concurrent URL probes. Default: `1`.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #'
        #' @return A data.table download plan.
        download_plan = function(replica = c("current", "auto"), service = "HTTPServer",
                                 probe = TRUE, strategy = c("fastest", "first", "stable"),
                                 all = TRUE, node_stats = NULL, network_policy = NULL,
                                 node_policy = NULL, probe_concurrency = 1L,
                                 probe_cache_seconds = 3600L) {
            replica <- match.arg(replica)
            strategy <- match.arg(strategy)
            query_result_download_plan(
                self,
                service = service,
                probe = probe,
                strategy = strategy,
                node_stats = node_stats,
                network_policy = network_policy,
                node_policy = node_policy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds
            )
        },
        # }}}

        # download {{{
        #' @description
        #' Enqueue and run aggregation downloads using a Downloader.
        #'
        #' @param downloader Optional persistent [Downloader]. If `NULL`,
        #'        `store$downloader()` is used when `store` is supplied.
        #' @param store Optional [EsgStore] providing a bound downloader.
        #' @param replica Replica policy. Aggregation records currently use the
        #'        current records.
        #' @param service ESGF URL service to download from. Default:
        #'        `"HTTPServer"`.
        #' @param probe Whether to lightly probe URLs before ranking them.
        #' @param strategy Candidate ranking strategy.
        #' @param probe_concurrency Maximum concurrent URL probes when
        #'        `probe = TRUE`. Default comes from the downloader worker count.
        #' @param probe_cache_seconds Seconds to reuse fresh data-node probe
        #'        history before probing a URL again. Default: `3600`.
        #' @param session_label Optional download session label.
        #' @param run Whether to run the queued session immediately. Default:
        #'        `TRUE`.
        #' @param ... Additional arguments passed to `Downloader$run()`.
        #'
        #' @return The created downloader session ID.
        download = function(downloader = NULL, store = NULL, replica = c("current", "auto"),
                            service = "HTTPServer", probe = TRUE, strategy = c("fastest", "first", "stable"),
                            probe_concurrency = NULL, probe_cache_seconds = 3600L,
                            session_label = NULL, run = TRUE, ...) {
            replica <- match.arg(replica)
            strategy <- match.arg(strategy)
            if (is.null(downloader)) {
                if (!is.null(store)) {
                    downloader <- store$downloader()
                } else {
                    cli::cli_abort("`download()` requires an explicit `store` or persistent `downloader`.")
                }
            }
            node_stats <- tryCatch(downloader$data_nodes(service = service), error = function(e) NULL)
            network_policy <- tryCatch(downloader$network_policy, error = function(e) NULL)
            node_policy <- tryCatch(downloader$node_policy, error = function(e) NULL)
            if (is.null(probe_concurrency)) {
                probe_concurrency <- min(max(downloader$n_workers, 1L), 8L)
            }
            plan <- self$download_plan(
                replica = replica,
                service = service,
                probe = probe,
                strategy = strategy,
                node_stats = node_stats,
                network_policy = network_policy,
                node_policy = node_policy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds
            )
            tryCatch(downloader$record_probes(plan, probed = probe), error = function(e) NULL)
            session_id <- downloader$enqueue(plan, session_label = session_label)
            if (isTRUE(run)) {
                downloader$run(session_id = session_id, ...)
            }
            session_id
        },
        # }}}

        # print {{{
        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgResultAggregation` object itself, invisibly.
        print = function(n = 10L) {
            private$print_header("Aggregation")
            private$print_summary("Aggregation")
            private$print_parameters()
            cli::cat_line()
            private$print_contents("Aggregation", n)
            invisible(self)
        },
        # }}}

        # open_dataset {{{
        #' @description
        #' Open aggregation files as an EsgDataset for remote data access via OPeNDAP
        #'
        #' @param aggregate If `TRUE` (default), open all files as a multi-file dataset.
        #'   If `FALSE`, open only the first file.
        #' @param fallback What to do if OPeNDAP is unavailable. One of:
        #'   - `"ask"`: Interactively ask the user (default). In a
        #'     non-interactive session this raises an error.
        #'   - `"auto"`: Automatically download files via HTTPServer.
        #'   - `"error"`: Raise an error.
        #'
        #' @param store Optional [EsgStore] used for recoverable HTTP fallback.
        #' @param downloader Optional persistent [Downloader] used for
        #'        recoverable HTTP fallback.
        #'
        #' @return An `EsgDataset` object with the connection already opened.
        open_dataset = function(aggregate = TRUE, fallback = c("ask", "auto", "error"),
                                store = NULL, downloader = NULL) {
            checkmate::assert_flag(aggregate)
            fallback <- match.arg(fallback)

            urls <- self$url_opendap
            indices <- seq_along(urls)

            if (!aggregate) {
                urls <- urls[1L]
                indices <- 1L
            }
            if (!length(urls)) {
                cli::cli_abort("No aggregation records are available to open.")
            }

            targets <- urls
            nc_handles <- vector("list", length(urls))
            opendap_errors <- vector("list", length(urls))
            failed <- rep(FALSE, length(urls))
            missing <- is.na(urls)

            close_preopened_handles <- function() {
                open_pos <- which(!vapply(nc_handles, is.null, logical(1L)))
                if (!length(open_pos)) {
                    return(invisible(NULL))
                }

                esg_dataset_close_handles(targets[open_pos], nc_handles[open_pos])
                nc_handles[open_pos] <<- vector("list", length(open_pos))
                invisible(NULL)
            }
            cleanup_preopened <- TRUE
            on.exit(
                if (isTRUE(cleanup_preopened)) {
                    close_preopened_handles()
                },
                add = TRUE
            )

            if (length(urls)) {
                for (j in which(!missing)) {
                    d <- NULL
                    ok <- tryCatch(
                        {
                            d <- EsgDataset$new(urls[[j]])
                            d$open()
                            handles <- esg_dataset_detach_handles(d)
                            if (!length(handles) || is.null(handles[[1L]])) {
                                stop("Opened EsgDataset does not expose a transferable NetCDF handle.", call. = FALSE)
                            }
                            nc_handles[j] <- handles[1L]
                            TRUE
                        },
                        error = function(e) {
                            if (!is.null(d) && is.function(d$close)) {
                                d$close()
                            }
                            opendap_errors[[j]] <<- e
                            FALSE
                        }
                    )
                    failed[[j]] <- !ok
                }
            }

            fallback_pos <- which(missing | failed)
            if (length(fallback_pos)) {
                missing_pos <- which(missing)
                failed_pos <- which(failed)
                if (length(missing_pos)) {
                    cli::cli_alert_warning(
                        "OPeNDAP URLs are missing for {private$record_labels(indices[missing_pos])}."
                    )
                }
                if (length(failed_pos)) {
                    cli::cli_alert_warning(
                        "OPeNDAP connection failed for {private$record_labels(indices[failed_pos])}."
                    )
                }

                opendap_error <- if (length(failed_pos)) opendap_errors[[failed_pos[[1L]]]] else NULL
                if (fallback == "error") {
                    details <- c(
                        if (length(missing_pos)) {
                            "x" = "Missing OPeNDAP URL: {private$record_labels(indices[missing_pos])}"
                        },
                        if (length(failed_pos)) {
                            "x" = "Failed OPeNDAP open: {private$record_labels(indices[failed_pos])}"
                        }
                    )
                    cli::cli_abort(
                        c("OPeNDAP is not available for these aggregation records.", details),
                        parent = opendap_error
                    )
                }

                if (fallback == "ask") {
                    if (!interactive()) {
                        cli::cli_abort("Cannot ask for fallback in a non-interactive session. Use fallback = 'auto' to download via HTTP.")
                    } else {
                        answer <- utils::menu(
                            choices = c(
                                sprintf("Download %d file(s) via HTTP", length(fallback_pos)),
                                "Cancel"
                            ),
                            title = "OPeNDAP is not available. What would you like to do?"
                        )
                        if (answer != 1L) {
                            cli::cli_abort("Operation cancelled by user.")
                        }
                    }
                }

                download_urls <- self$url_download[indices[fallback_pos]]
                if (any(is.na(download_urls))) {
                    http_missing_pos <- fallback_pos[is.na(download_urls)]
                    cli::cli_abort(c(
                        "HTTPServer download URLs are missing for one or more aggregation records.",
                        "x" = "Missing HTTPServer URL: {private$record_labels(indices[http_missing_pos])}"
                    ))
                }

                if (is.null(downloader)) {
                    if (!is.null(store)) {
                        downloader <- store$downloader()
                    } else {
                        cli::cli_abort("HTTP fallback requires an explicit `store` or `downloader` so downloaded files are recoverable.")
                    }
                }
                cli::cli_alert_info("Downloading {length(fallback_pos)} file(s) via HTTP as fallback...")
                targets[fallback_pos] <- query_result_run_http_fallback(self, indices[fallback_pos], downloader)
            }

            ds <- EsgDataset$new(targets)
            esg_dataset_adopt_handles(ds, nc_handles)
            if (!isTRUE(ds$is_open)) {
                ds$open()
            }
            esg_dataset_set_context(ds, private$update_selection_context(indices))
            cleanup_preopened <- FALSE
            ds
        }
        # }}}
    ),

    active = list(
        # url_opendap {{{
        #' @field url_opendap A character vector of the OPeNDAP URLs of the
        #'        files.
        url_opendap = function() {
            url <- private$get_url("OPENDAP", "OPeNDAP")
            has_html <- !is.na(url) & tools::file_ext(url) == "html"
            if (any(has_html)) {
                url[has_html] <- tools::file_path_sans_ext(url[has_html])
            }
            url
        },
        # }}}

        # url_download {{{
        #' @field url_download A character vector of the download URLs of the
        #'        files.
        url_download = function() {
            private$get_url("HTTPServer")
        },
        # }}}

        # fields {{{
        #' @field fields A character vector indicating all response fields,
        #'        followed by derived URL fields when their source fields are
        #'        available.
        fields = function() {
            fields <- super$fields
            derived <- character()
            if ("url" %in% fields) {
                derived <- c("url_opendap", "url_download")
            }
            unique(c(fields, derived))
        }
        # }}}
    ),

    private = list(
        result_type = "Aggregation",

        required_fields = sort(unique(c(
            EsgResult$private_fields$required_fields,
            "dataset_id",
            "instance_id",
            "master_id",
            "replica",
            "title",
            "version",
            "data_node",
            "activity_id",
            "institution_id"
        )))
    )
)
# }}}

# query_result_normalize_response {{{
query_result_normalize_response <- function(response) {
    if (is.null(response)) {
        return(response)
    }

    docs <- response$response$docs
    if (is.null(docs) || (is.list(docs) && !is.data.frame(docs) && !length(docs))) {
        response$response$docs <- data.frame(check.names = FALSE)
    }

    response
}
# }}}

# query_result_empty_response {{{
query_result_empty_response <- function(params) {
    force(params)

    response <- list(
        responseHeader = list(
            status = 0L,
            QTime = 0L,
            params = stats::setNames(list(), character())
        ),
        response = list(
            numFound = 0L,
            start = 0L,
            docs = data.frame(check.names = FALSE),
            maxScore = 0
        ),
        facet_counts = list(
            facet_queries = list(),
            facet_fields = list(),
            facet_ranges = list(),
            facet_intervals = list(),
            facet_heatmaps = list()
        ),
        timestamp = Sys.time()
    )

    list(
        response = response,
        docs = response$response$docs,
        parameter = query_param_clone(params),
        context = list(query_url = character())
    )
}
# }}}

# new_query_result {{{
new_query_result <- function(generator, index_node = NULL, params = NULL, result = NULL, ...) {
    generator$new(index_node, params, result, ...)
}
# }}}

# result subset method {{{
#' Subset an ESGF query result
#'
#' `[` is a one-dimensional shortcut for `x$slice(i)`.
#'
#' @param x An [EsgResult] object.
#' @param i A row selector accepted by `x$slice(i)`.
#' @param j,...,drop Unsupported.
#'
#' @return A new result object of the same type, or `x` for `x[]`.
#'
#' @export
`[.EsgResult` <- function(x, i, j, ..., drop = FALSE) {
    if (nargs() > 2L || !missing(j) || length(list(...)) || !identical(drop, FALSE)) {
        stop("EsgResult subsetting only supports one-dimensional `result[i]`.", call. = FALSE)
    }
    if (missing(i)) {
        return(x)
    }

    x$slice(i)
}
# }}}

# esg_result {{{

#' Create empty query result object
#'
#' @description
#' `esg_result()` creates an empty query result object of input type, so that
#' you can load the saved JSON file via `EsgResult$load()`.
#'
#' @param type A string indicating what type of ESGF query result should be
#'        created. Should be one of `"dataset"`, `"file"` or "aggregation"`.
#'
#' @return An empty [EsgResult] object of given type.
#'
#' @export
esg_result <- function(type = c("dataset", "file", "aggregation")) {
    type <- match.arg(type)

    new_query_result(
        switch(type, "dataset" = EsgResultDataset, "file" = EsgResultFile, "aggregation" = EsgResultAggregation),
        index_node = NULL,
        params = NULL,
        result = NULL
    )
}
# }}}

# vim: fdm=marker :
