# EsgResult
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
# EsgResult {{{
EsgResult <- R6::R6Class(
    "EsgResult",
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # initialize
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
        # initialize {{{
        initialize = function(index_node, params, response, context = NULL) {
            private$index_node <- index_node
            private$parameter <- query_param__clone(params)
            private$response <- query_result__response(response)
            private$context <- query_result__context(context)
            private$register_dynamic_fields()
            self
        },
        # }}}
        # to_data_table
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
        # to_data_table {{{
        to_data_table = function(fields = NULL, formatted = NULL) {
            checkmate::assert_character(
                fields,
                any.missing = FALSE,
                unique = TRUE,
                min.len = 1L,
                null.ok = TRUE
            )
            checkmate::assert_character(
                formatted,
                any.missing = FALSE,
                unique = TRUE,
                null.ok = TRUE
            )

            if (is.null(fields)) {
                fields <- self$fields
            } else {
                checkmate::assert_subset(fields, self$fields)
            }

            res <- stats::setNames(
                lapply(fields, function(field) {
                    private$normalize_output_field(
                        private$get_output_field(
                            field,
                            formatted = field %in% formatted
                        )
                    )
                }),
                fields
            )

            data.table::setDT(res)
            res[]
        },
        # }}}
        # to_dt
        #' @description
        #' Alias of `$to_data_table()`.
        #'
        #' @param ... Arguments passed to `$to_data_table()`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        # to_dt {{{
        to_dt = function(...) {
            self$to_data_table(...)
        },
        # }}}
        # count
        #' @description
        #' Count the number of matched records in current result
        #'
        #' @return An integer.
        #'
        # count {{{
        count = function() {
            length(self$id)
        },
        # }}}
        # save
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
        # save {{{
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

            do.call(query__save, args)
        },
        # }}}
        # load
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
        # load {{{
        load = function(file) {
            q <- query__load(file, private$result_schema())
            private$validate_loaded_result(q)

            private$index_node <- q$index_node
            private$parameter <- q$parameter
            private$response <- q$response
            private$context <- query_result__context(q$context)
            private$register_dynamic_fields()

            self
        },
        # }}}
        # query_url
        #' @description
        #' Return the ESGF search query URL used to create this result.
        #'
        #' @param pages Which recorded query URLs to return. `"first"` returns
        #'        the first request URL. `"all"` returns every recorded request
        #'        URL, including pagination requests. Default: `"first"`.
        #'
        #' @return A named character vector of query URLs.
        #'
        # query_url {{{
        query_url = function(pages = c("first", "all")) {
            pages <- match.arg(pages)
            urls <- private$get_query_url_context()
            if (identical(pages, "first")) {
                return(utils::head(urls, 1L))
            }

            urls
        },
        # }}}
        # reachable
        #' @description
        #' Check whether result records are reachable through a service URL.
        #'
        #' `$reachable()` performs a lightweight check of the selected service
        #' URL for each record already held in the result. It returns diagnostic
        #' rows and does not modify result context or saved-result metadata. At
        #' URL level, OPeNDAP must return a valid DDS description; HTTPServer
        #' uses HEAD with a minimal Range fallback.
        #'
        #' @param service ESGF URL service to check. Default: `"OPENDAP"`.
        #' @param level Check level. `"data_node"` checks the root URL of each
        #'        data node; `"url"` checks the actual service URL for each
        #'        record. Default: `"data_node"`.
        #' @param probe Optional named list of probe settings. Supported fields
        #'        are `timeout`, `concurrency`, `network_policy`,
        #'        `cache_seconds`, and `cache_failures_seconds`.
        #'
        #' @return A [data.table][data.table::data.table()] with columns
        #'        `record_index`, `id`, `data_node`, `service`, `url`,
        #'        `reachable`, `latency_ms`, `error`, `probe_level`,
        #'        `probe_url`, and `probe_cached`.
        # reachable {{{
        reachable = function(
            service = "OPENDAP",
            level = c("data_node", "url"),
            probe = NULL
        ) {
            checkmate::assert_string(service)
            level <- match.arg(level)
            probe <- query_result__reach_config(probe)

            docs <- private$get_docs()
            n <- nrow(docs)
            urls <- private$get_url(service, service)
            data_node <- as.character(query_result__col(docs, "data_node"))
            probes <- query_result__reach_targets(
                urls,
                data_node = data_node,
                service = service,
                level = level,
                timeout = probe$timeout,
                network_policy = probe$network_policy,
                probe_concurrency = probe$concurrency,
                cache_seconds = probe$cache_seconds,
                cache_failures_seconds = probe$cache_failures_seconds
            )

            data.table::data.table(
                record_index = seq_len(n),
                id = as.character(query_result__col(docs, "id")),
                data_node = data_node,
                service = rep(service, n),
                url = urls,
                reachable = probes$reachable,
                latency_ms = probes$latency_ms,
                error = probes$error,
                probe_level = probes$probe_level,
                probe_url = probes$probe_url,
                probe_cached = probes$probe_cached
            )
        },
        # }}}
        # slice
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
        # slice {{{
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
        # filter
        #' @description
        #' Subset result records with a predicate function.
        #'
        #' @param predicate A function that accepts `self$to_data_table()` and
        #'        returns a logical vector with one value per current record.
        #' @param formatted Whether to use formatted values when creating the
        #'        predicate input data table. Default: `FALSE`.
        #'
        #' @return A new result object of the same type.
        # filter {{{
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
        # selection
        #' @description
        #' Return local selection provenance for this result.
        #'
        #' `$selection()` maps current rows back to the result object that first
        #' recorded selection provenance. It does not record intermediate filter
        #' steps.
        #'
        #' @return A list with `source_count`, `source_num_found`, and
        #'        `source_indices`.
        # selection {{{
        selection = function() {
            private$get_selection_context()
        }
        # }}}
    ),

    active = list(
        # id
        #' @field id A character vector indicating globally unique record
        #'        identifiers.
        # id {{{
        id = function() {
            private$get_field("id")
        },
        # }}}
        # url
        #' @field url A list of [data.table][data.table::data.table()] with 3
        #'        columns:
        #'
        #' 1. `service` \\[`character`\\]: The service types, e.g. OPENDAP,
        #'    HTTPServer, etc.;
        #' 2. `url` \\[`character`\\]: The actual URLs;
        #' 3. `mime_type` \\[`character`\\]: The MIME types indicating
        #'    the nature and format of the corresponding document of the
        #'    URLs.
        # url {{{
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
        # size
        #' @field size A numeric vector of file sizes in bytes.
        # size {{{
        size = function() {
            size <- private$get_field("size")
            set_size_units(size)
        },
        # }}}
        # fields
        #' @field fields A character vector indicating all fields in the results,
        #'        preserving the order returned by the response.
        # fields {{{
        fields = function() {
            names(private$get_docs())
        },
        # }}}
        # time_filter
        #' @field time_filter A list describing the time range used by
        #'        `$filter_time()`, or `NULL` if no result-level time filter
        #'        has been recorded.
        # time_filter {{{
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
        query_fields = c(
            "dataset_id",
            "fields",
            "latest",
            "distrib",
            "limit",
            "type",
            "format"
        ),
        static_fields = c(
            "id",
            "url",
            "size",
            "fields",
            "filename",
            "url_opendap",
            "url_download"
        ),

        # result_schema
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
                Dataset = schema__compiled("RESULT_DATASET"),
                File = schema__compiled("RESULT_FILE"),
                Aggregation = schema__compiled("RESULT_AGGREGATION"),
                NULL
            )
            if (is.null(schema)) {
                cli::cli_abort(
                    "Cannot select a saved-result schema for result type {.val {type}}."
                )
            }

            schema
        },
        # }}}
        # get_docs {{{
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
        # }}}

        # get_field {{{
        get_field = function(field) {
            docs <- private$get_docs()
            val <- docs[[field]]
            if (is.null(val)) {
                return(NULL)
            }
            if (all(lengths(val) == 1L)) unlst(val) else val
        },
        # }}}

        # get_time_filter_context
        # get_time_filter_context {{{
        get_time_filter_context = function() {
            ctx <- private$context$time_filter
            if (is.null(ctx) || !length(ctx)) {
                return(NULL)
            }

            ctx
        },
        # }}}
        # get_query_url_context
        # get_query_url_context {{{
        get_query_url_context = function() {
            urls <- private$context$query_url
            if (is.null(urls)) {
                if (is.null(private$index_node) || is.null(private$parameter)) {
                    return(character())
                }
                urls <- query__build(private$index_node, private$parameter)
            }

            query_result__query_urls(urls)
        },
        # }}}
        # get_selection_context
        # get_selection_context {{{
        get_selection_context = function() {
            ctx <- private$context$selection
            if (!is.null(ctx) && length(ctx)) {
                return(query_result__selection(ctx))
            }

            n <- nrow(private$get_docs())
            source_num_found <- if (is.null(private$response)) {
                n
            } else {
                private$response$response$numFound
            }
            if (
                is.null(source_num_found) ||
                    !length(source_num_found) ||
                    is.na(source_num_found[[1L]])
            ) {
                source_num_found <- n
            }

            list(
                source_count = as.integer(n),
                source_num_found = as.integer(source_num_found[[1L]]),
                source_indices = seq_len(n)
            )
        },
        # }}}
        # update_time_filter_context
        # update_time_filter_context {{{
        update_time_filter_context = function(
            start,
            stop,
            method,
            total,
            selected,
            unknown
        ) {
            context <- private$context
            context$time_filter <- list(
                start = query_result__time_iso(start),
                stop = query_result__time_iso(stop),
                method = method,
                unknown = "kept",
                total = as.integer(total),
                selected = as.integer(selected),
                unknown_count = as.integer(unknown)
            )

            context
        },
        # }}}
        # update_selection_context
        # update_selection_context {{{
        update_selection_context = function(index, context = private$context) {
            context__update_selection(
                context,
                private$get_selection_context(),
                index
            )
        },
        # }}}
        # result_with_docs
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
                cli::cli_abort(
                    "Cannot create a filtered result for an untyped ESGF result."
                )
            }

            query_result__new(
                generator,
                private$index_node,
                private$parameter,
                response,
                context = context
            )
        },
        # }}}
        # filter_time_result
        # filter_time_result {{{
        filter_time_result = function(
            start,
            stop,
            method = c("drs", "opendap", "auto"),
            result_label = "file"
        ) {
            method <- match.arg(method)
            window <- query_result__time_window(start, stop)
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
                    context = private$update_selection_context(
                        integer(),
                        context = context
                    )
                ))
            }

            ranges <- switch(
                method,
                drs = private$filter_time_ranges_drs(docs, result_label),
                opendap = private$filter_time_ranges_opendap(result_label),
                auto = private$filter_time_ranges_auto(docs, result_label)
            )
            known <- !is.na(ranges$datetime_start) & !is.na(ranges$datetime_end)
            keep <- !known |
                (ranges$datetime_start <= window$stop &
                    ranges$datetime_end >= window$start)
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
            context <- private$update_selection_context(
                index,
                context = context
            )

            private$result_with_docs(out, context = context)
        },
        # }}}
        # normalize_slice_index
        # normalize_slice_index {{{
        normalize_slice_index = function(i) {
            n <- nrow(private$get_docs())
            if (is.null(i)) {
                return(integer())
            }

            if (is.logical(i)) {
                checkmate::assert_logical(
                    i,
                    len = n,
                    any.missing = FALSE,
                    .var.name = "i"
                )
                return(which(i))
            }

            if (is.character(i)) {
                checkmate::assert_character(
                    i,
                    any.missing = FALSE,
                    .var.name = "i"
                )
                ids <- self$id
                if (!length(i)) {
                    return(integer())
                }
                if (anyDuplicated(i)) {
                    stop(
                        "`i` must not contain duplicate record IDs.",
                        call. = FALSE
                    )
                }
                index <- match(i, ids)
                if (anyNA(index)) {
                    missing <- i[is.na(index)]
                    stop(
                        sprintf(
                            "Unknown record ID(s): [%s].",
                            paste(sprintf("'%s'", missing), collapse = ", ")
                        ),
                        call. = FALSE
                    )
                }
                return(index)
            }

            if (!checkmate::test_integerish(i, any.missing = FALSE)) {
                stop(
                    "`i` must be an integer, logical, character, or NULL selector.",
                    call. = FALSE
                )
            }

            i <- as.integer(i)
            if (!length(i)) {
                return(integer())
            }
            if (any(i == 0L)) {
                stop("`i` must not contain zero.", call. = FALSE)
            }
            if (any(i > 0L) && any(i < 0L)) {
                stop(
                    "`i` must not mix positive and negative indices.",
                    call. = FALSE
                )
            }
            if (anyDuplicated(i)) {
                stop("`i` must not contain duplicate indices.", call. = FALSE)
            }

            if (all(i < 0L)) {
                if (any(abs(i) > n)) {
                    stop(
                        sprintf(
                            "Negative indices must be between -%d and -1.",
                            n
                        ),
                        call. = FALSE
                    )
                }
                return(setdiff(seq_len(n), abs(i)))
            }

            if (any(i > n)) {
                stop(
                    sprintf("Positive indices must be between 1 and %d.", n),
                    call. = FALSE
                )
            }

            i
        },
        # }}}
        # filter_time_ranges_drs
        # filter_time_ranges_drs {{{
        filter_time_ranges_drs = function(docs, result_label = "file") {
            # Warn only about records that actually cannot be interpreted;
            # selecting the documented DRS strategy is not itself exceptional.
            labels <- query_result__drs_labels(docs)
            ranges <- query_result__drs_ranges(labels$value)
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
        # filter_time_ranges_auto
        # Prefer authoritative File metadata and fill only absent ranges from
        # CMIP/DRS filenames. ESGF nodes commonly omit the requested datetime
        # fields, while local fixtures and some providers already supply them.
        # filter_time_ranges_auto {{{
        filter_time_ranges_auto = function(docs, result_label = "file") {
            ranges <- query_result__fill_time_ranges(docs, function() {
                query_result__drs_labels(docs)$value
            })
            unknown <- is.na(ranges$datetime_start) |
                is.na(ranges$datetime_end)
            if (any(unknown)) {
                warning(
                    sprintf(
                        paste(
                            "Could not determine a metadata or DRS time range",
                            "for %d %s record(s); keeping those records."
                        ),
                        sum(unknown),
                        result_label
                    ),
                    call. = FALSE
                )
            }
            ranges
        },
        # }}}
        # filter_time_ranges_opendap
        # filter_time_ranges_opendap {{{
        filter_time_ranges_opendap = function(result_label = "file") {
            urls <- self$url_opendap
            if (is.null(urls)) {
                urls <- rep(NA_character_, self$count())
            }

            start <- as.POSIXct(
                rep(NA_real_, length(urls)),
                origin = "1970-01-01",
                tz = "UTC"
            )
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
                            stop(
                                "The NetCDF time axis is empty or unavailable.",
                                call. = FALSE
                            )
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

            data.frame(
                datetime_start = start,
                datetime_end = end,
                check.names = FALSE
            )
        },
        # }}}
        # add_time_range_fields
        # add_time_range_fields {{{
        add_time_range_fields = function(docs, ranges) {
            docs$datetime_start <- query_result__time_iso(ranges$datetime_start)
            docs$datetime_end <- query_result__time_iso(ranges$datetime_end)
            docs
        },
        # }}}
        # validate_loaded_result
        # validate_loaded_result {{{
        validate_loaded_result = function(q) {
            expected <- private$result_type
            actual <- query_param__value(q$parameter$type())
            if (!identical(actual, expected)) {
                stop(
                    sprintf(
                        "Cannot load %s result into %s object. Use esg_result('%s')$load() instead.",
                        if (is.null(actual)) {
                            "NULL"
                        } else {
                            sprintf("'%s'", actual)
                        },
                        class(self)[[1L]],
                        tolower(expected)
                    ),
                    call. = FALSE
                )
            }

            invisible(q)
        },
        # }}}
        # register_dynamic_fields
        # register_dynamic_fields {{{
        register_dynamic_fields = function() {
            if (length(private$dynamic_fields)) {
                for (field in private$dynamic_fields) {
                    if (
                        exists(field, envir = self, inherits = FALSE) &&
                            bindingIsActive(field, self)
                    ) {
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

            fields <- fields[
                !vapply(
                    fields,
                    exists,
                    logical(1L),
                    envir = self,
                    inherits = FALSE
                )
            ]
            for (field in fields) {
                makeActiveBinding(
                    field,
                    local({
                        field <- field
                        function(value) {
                            if (!missing(value)) {
                                stop(
                                    "ESGF result fields are read-only.",
                                    call. = FALSE
                                )
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
        # has_access
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
        # get_url
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
        # get_output_field
        # get_output_field {{{
        get_output_field = function(field, formatted = FALSE) {
            docs <- private$get_docs()
            value <- docs[[field]]
            if (isTRUE(formatted) && identical(field, "size")) {
                # Keep the default table numeric, but make the explicitly
                # formatted table human-readable without a units dependency.
                return(format_size_units(self$size))
            }
            if (isTRUE(formatted) || is.null(value)) {
                return(self[[field]])
            }

            value
        },
        # }}}
        # normalize_output_field
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
        # record_context
        # record_context {{{
        record_context = function(index) {
            checkmate::assert_int(index, lower = 1L)

            docs <- private$get_docs()
            for (field in c(
                "id",
                "dataset_id",
                "instance_id",
                "title",
                "filename"
            )) {
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
        # record_label
        # record_label {{{
        record_label = function(index) {
            checkmate::assert_int(index, lower = 1L)
            sprintf("record %d%s", index, private$record_context(index))
        },
        # }}}
        # record_labels
        # record_labels {{{
        record_labels = function(index) {
            checkmate::assert_integerish(
                index,
                lower = 1L,
                any.missing = FALSE,
                min.len = 1L
            )
            paste(
                vapply(as.integer(index), private$record_label, character(1L)),
                collapse = ", "
            )
        },
        # }}}
        # format_record_context_value
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

            value <- tryCatch(as.character(value), error = function(e) {
                character()
            })
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
        # print_header
        # print_header {{{
        print_header = function(type = "") {
            d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
            cli::cli_rule("ESGF Query Result [{type}]")
            cli::cli_end(d)
        },
        # }}}
        # print_summary
        # print_summary {{{
        print_summary = function(type = "") {
            ts <- format(
                private$response$timestamp,
                tz = Sys.timezone(),
                usetz = TRUE
            )
            fields <- self$fields
            cli::cli_bullets(c("*" = "Index Node: {private$index_node}"))
            cli::cli_bullets(c("*" = "Collected at: {ts}"))
            cli::cli_bullets(c("*" = "Result count: {self$count()}"))
            if (identical(type, "Dataset")) {
                n_files <- private$get_field("number_of_files")
                if (!is.null(n_files)) {
                    n_files <- suppressWarnings(as.numeric(n_files))
                    n_files <- n_files[!is.na(n_files)]
                    if (length(n_files)) {
                        n_files <- format(
                            sum(n_files),
                            big.mark = ",",
                            scientific = FALSE
                        )
                        cli::cli_bullets(c("*" = "Dataset files: {n_files}"))
                    }
                }

                n_aggs <- private$get_field("number_of_aggregations")
                if (!is.null(n_aggs)) {
                    n_aggs <- suppressWarnings(as.numeric(n_aggs))
                    n_aggs <- n_aggs[!is.na(n_aggs)]
                    if (length(n_aggs)) {
                        n_aggs <- format(
                            sum(n_aggs),
                            big.mark = ",",
                            scientific = FALSE
                        )
                        cli::cli_bullets(c(
                            "*" = "Dataset aggregations: {n_aggs}"
                        ))
                    }
                }
            }
            if (type == "Aggregation") {
                cli::cli_bullets(c(
                    "*" = "Total size: <{.emph Unknown}> [Byte]"
                ))
            } else {
                cli::cli_bullets(c(
                    "*" = "Total size: {format_size_units(sum(self$size))}"
                ))
            }
            if (!length(fields)) {
                cli::cli_bullets(c("*" = "Fields: 0"))
            } else {
                cli::cli_bullets(c(
                    "*" = "Fields: {length(fields)} | [ {fields} ]"
                ))
            }
        },
        # }}}
        # print_parameters
        # print_parameters {{{
        print_parameters = function() {
            cli::cli_h1("<Query Parameter>")
            query_param__print(private$parameter)
        },
        # }}}
        # print_contents
        # print_contents {{{
        print_contents = function(type, n) {
            checkmate::assert_count(n, positive = TRUE, null.ok = TRUE)

            if (is.null(private$get_field("data_node"))) {
                cli::cli_rule("<{type}>")
            } else {
                cli::cli_rule(
                    "<{type}> (From {length(unique(private$get_field('data_node')))} Data Nodes)"
                )
            }

            if (self$count() == 0L) {
                cli::cli_bullets(c(" " = "{.strong <Empty>}"))
                result_type <- tolower(type)
                # Include the result type so cli does not coalesce identical
                # empty-result notes from consecutive result objects.
                cli::cli_bullets(c(
                    " " = "{.emph NOTE: No matching {result_type} records. Update the query and try again.}"
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
                number_of_files <- private$get_field("number_of_files")
                number_of_aggregations <- private$get_field(
                    "number_of_aggregations"
                )
                access <- private$get_field("access")

                size <- sprintf(
                    "%s   [ %s Files, %s | %s ]\n%s   [ Access: <%s> ]",
                    spc,
                    number_of_files[ind],
                    format_size_units(self$size[ind]),
                    if (is.null(number_of_aggregations)) {
                        "No Aggregations"
                    } else {
                        agg <- number_of_aggregations[ind]
                        agg[is.na(agg)] <- 0L
                        paste(
                            agg,
                            vapply(
                                agg,
                                ngettext,
                                "",
                                "Aggregation",
                                "Aggregations"
                            )
                        )
                    },
                    spc,
                    if (is.null(access)) {
                        "NONE"
                    } else {
                        vapply(access[ind], paste0, "", collapse = ", ")
                    }
                )
            } else {
                url <- self$url

                size <- sprintf(
                    "%s   [ %s | Access: <%s> ]",
                    spc,
                    if (type == "Aggregation") {
                        "<Unknown>"
                    } else {
                        format_size_units(self$size[ind])
                    },
                    if (is.null(url)) {
                        "NONE"
                    } else {
                        vapply(
                            url[ind],
                            FUN.VALUE = character(1),
                            function(url) {
                                if (is.null(url)) {
                                    return("NONE")
                                }
                                paste0(url$service, collapse = ", ")
                            }
                        )
                    }
                )
            }

            cli::cat_line(c(rbind(brief, size)))

            print__truncation_footer(self$id, n)
        }
        # }}}
    )
)
# }}}
# result collection helpers
# query_result__context {{{
query_result__context <- function(context = NULL) {
    if (is.null(context) || !length(context)) {
        return(list())
    }
    if (!is.list(context)) {
        stop("Saved result context must be a list.", call. = FALSE)
    }
    if (!is.null(context$query_url)) {
        context$query_url <- query_result__query_urls(
            context$query_url,
            named = FALSE
        )
    }
    if (!is.null(context$selection)) {
        context$selection <- query_result__selection(context$selection)
    }

    context
}
# }}}

# query_result__selection {{{
query_result__selection <- function(selection) {
    if (is.null(selection) || !length(selection)) {
        return(NULL)
    }
    if (!is.list(selection)) {
        stop("Saved result selection context must be a list.", call. = FALSE)
    }

    required <- c("source_count", "source_num_found", "source_indices")
    missing <- setdiff(required, names(selection))
    if (length(missing)) {
        stop(
            sprintf(
                "Saved result selection context is missing required field(s): [%s].",
                paste(sprintf("'%s'", missing), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    source_indices <- selection$source_indices
    if (is.list(source_indices) && !length(source_indices)) {
        source_indices <- integer()
    }

    checkmate::assert_integerish(
        selection$source_count,
        lower = 0L,
        len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_integerish(
        selection$source_num_found,
        lower = 0L,
        len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_integerish(
        source_indices,
        lower = 1L,
        any.missing = FALSE
    )

    source_count <- as.integer(selection$source_count[[1L]])
    source_num_found <- as.integer(selection$source_num_found[[1L]])
    source_indices <- as.integer(source_indices)
    if (length(source_indices) && any(source_indices > source_count)) {
        stop(
            "Saved result selection source indices must not exceed `source_count`.",
            call. = FALSE
        )
    }

    list(
        source_count = source_count,
        source_num_found = source_num_found,
        source_indices = source_indices
    )
}
# }}}

# Update a result context with selected source positions.
# context__update_selection {{{
context__update_selection <- function(context, selection, index) {
    # Source totals describe the original result; only its retained positions
    # change when a dataset or query result is sliced again.
    context$selection <- list(
        source_count = selection$source_count,
        source_num_found = selection$source_num_found,
        source_indices = selection$source_indices[index]
    )

    context
}
# }}}

# query_result__query_urls {{{
query_result__query_urls <- function(urls, named = TRUE) {
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
# }}}

# Normalize a result column to the fixed character length required by callers.
# query_result__character_column {{{
query_result__character_column <- function(data, name, size = nrow(data)) {
    value <- data[[name]]
    if (is.null(value)) {
        return(rep(NA_character_, size))
    }

    value <- as.character(value)
    if (length(value) < size) {
        value <- c(value, rep(NA_character_, size - length(value)))
    }
    value[seq_len(size)]
}
# }}}

# query_result__time_iso {{{
query_result__time_iso <- function(x) {
    if (is.null(x)) {
        return(character())
    }

    x <- as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    out[ok] <- format.POSIXct(x[ok], tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    out
}
# }}}

# Fill incomplete metadata time ranges from caller-selected CMIP/DRS labels.
# query_result__fill_time_ranges {{{
query_result__fill_time_ranges <- function(data, labels) {
    size <- nrow(data)
    start <- solrdate__parse(
        query_result__character_column(data, "datetime_start", size),
        tz = "UTC"
    )
    end <- solrdate__parse(
        query_result__character_column(data, "datetime_end", size),
        tz = "UTC"
    )
    incomplete <- is.na(start) | is.na(end)
    if (any(incomplete)) {
        # Resolve labels lazily so complete provider metadata never touches a
        # caller's URL or identity fallback fields.
        if (is.function(labels)) {
            labels <- labels()
        }
        drs <- query_result__drs_ranges(labels)
        # One absent boundary invalidates the metadata pair; replace both
        # boundaries to retain the existing whole-range fallback semantics.
        start[incomplete] <- drs$datetime_start[incomplete]
        end[incomplete] <- drs$datetime_end[incomplete]
    }

    data.frame(
        datetime_start = start,
        datetime_end = end,
        check.names = FALSE
    )
}
# }}}

# query_result__time_window {{{
query_result__time_window <- function(start, stop) {
    checkmate::assert_scalar(start)
    checkmate::assert_scalar(stop)

    time <- solrdate__parse(c(start, stop), tz = "UTC")
    if (any(is.na(time))) {
        stop("`start` and `stop` must be parseable datetimes.", call. = FALSE)
    }
    if (time[[2L]] < time[[1L]]) {
        stop("`stop` must be greater than or equal to `start`.", call. = FALSE)
    }

    list(start = time[[1L]], stop = time[[2L]])
}
# }}}

# query_result__drs_url {{{
query_result__drs_url <- function(url) {
    if (is.null(url) || !length(url)) {
        return(NA_character_)
    }

    url <- unlist(url, recursive = TRUE, use.names = FALSE)
    url <- as.character(url)
    url <- url[!is.na(url) & nzchar(url)]
    if (!length(url)) {
        return(NA_character_)
    }

    parsed <- vapply(
        strsplit(url, "|", fixed = TRUE),
        function(parts) parts[[1L]],
        character(1L)
    )
    parsed <- sub("[?#].*$", "", parsed)
    parsed <- basename(parsed)
    parsed <- sub("\\.html$", "", parsed)
    parsed <- parsed[grepl("\\.nc$", parsed)]
    if (!length(parsed)) {
        return(NA_character_)
    }

    parsed[[1L]]
}
# }}}

# query_result__drs_id {{{
query_result__drs_id <- function(id) {
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
# }}}

# query_result__drs_labels {{{
query_result__drs_labels <- function(docs) {
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
            query_result__drs_url(docs$url[[i]])
        } else {
            NA_character_
        }
        if (!is.na(url)) {
            labels[[i]] <- url
            source[[i]] <- "url"
            next
        }

        id <- scalar_field("id", i)
        id_label <- query_result__drs_id(id)
        if (!is.na(id_label)) {
            labels[[i]] <- id_label
            source[[i]] <- "id"
        }
    }

    data.frame(value = labels, source = source, check.names = FALSE)
}
# }}}

# query_result__col {{{
query_result__col <- function(dt, name, default = NA_character_) {
    if (name %in% names(dt)) {
        return(dt[[name]])
    }
    rep(default, nrow(dt))
}
# }}}

# query_result__file_key {{{
query_result__file_key <- function(dt) {
    # Use the same logical identity as the persistent store so replica
    # candidates cannot split into independent download tasks before they are
    # normalized in the catalog.
    store__logical_file_id(data.table::as.data.table(dt))
}
# }}}
# query_result__generator {{{
query_result__generator <- function(type) {
    type <- query_result__type(type)
    switch(
        type,
        Dataset = EsgResultDataset,
        File = EsgResultFile,
        Aggregation = EsgResultAggregation
    )
}
# }}}

# query_result__required {{{
query_result__required <- function(type) {
    type <- query_result__type(type)
    switch(
        type,
        Dataset = EsgResultDataset$private_fields$required_fields,
        File = EsgResultFile$private_fields$required_fields,
        Aggregation = EsgResultAggregation$private_fields$required_fields
    )
}
# }}}
# query_result__drs_bound {{{
query_result__drs_bound <- function(value, end = FALSE) {
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
        `6` = sprintf(
            "%s-%s-01 00:00:00",
            substr(value, 1L, 4L),
            substr(value, 5L, 6L)
        ),
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
# }}}

# query_result__drs_ranges {{{
query_result__drs_ranges <- function(labels) {
    start <- as.POSIXct(
        rep(NA_real_, length(labels)),
        origin = "1970-01-01",
        tz = "UTC"
    )
    end <- start

    for (i in seq_along(labels)) {
        label <- labels[[i]]
        if (is.na(label) || !nzchar(label)) {
            next
        }

        label <- basename(sub("\\.html$", "", sub("[?#].*$", "", label)))
        matches <- gregexpr(
            "_([0-9]{4}|[0-9]{6}|[0-9]{8}|[0-9]{10}|[0-9]{12})-([0-9]{4}|[0-9]{6}|[0-9]{8}|[0-9]{10}|[0-9]{12})(?=\\.nc$|$)",
            label,
            perl = TRUE
        )
        hit <- regmatches(label, matches)[[1L]]
        if (!length(hit) || identical(hit, -1L)) {
            next
        }

        range <- sub("^_", "", hit[[length(hit)]])
        parts <- strsplit(range, "-", fixed = TRUE)[[1L]]
        if (length(parts) != 2L || nchar(parts[[1L]]) != nchar(parts[[2L]])) {
            next
        }

        start[[i]] <- query_result__drs_bound(parts[[1L]], end = FALSE)
        end[[i]] <- query_result__drs_bound(parts[[2L]], end = TRUE)
    }

    data.frame(datetime_start = start, datetime_end = end, check.names = FALSE)
}
# }}}

# query_result__type {{{
query_result__type <- function(
    type,
    choices = c("Dataset", "File", "Aggregation")
) {
    checkmate::assert_string(type)
    type <- tolower(type)
    map <- c(dataset = "Dataset", file = "File", aggregation = "Aggregation")
    if (!type %in% names(map)) {
        stop(
            sprintf(
                "`type` must be one of %s.",
                paste(sprintf("'%s'", choices), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    type <- unname(map[[type]])
    checkmate::assert_choice(type, choices)
    type
}
# }}}

# query_result__merge_params {{{
query_result__merge_params <- function(store, params) {
    if (!length(params)) {
        return(store)
    }

    checkmate::assert_list(params, names = "named")
    if (any(!nzchar(names(params)))) {
        stop("All query parameters to merge must be named.", call. = FALSE)
    }

    extra_store <- query_param__as_store(params)
    extra_names <- intersect(
        names(params),
        names(extra_store$state(null = TRUE))
    )
    extra <- extra_store$state(extra_names, null = TRUE)
    state <- store$state(null = TRUE)
    state[names(extra)] <- extra

    store$restore(state)
}
# }}}
# This is a soft preflight budget, not a server limit. An oversized singleton
# is still attempted; only an actual HTTP 414 triggers failure or further splits.
QUERY_RESULT_CHILD_COLLECT_BATCH_SIZE <- 50L
QUERY_RESULT_COLLECT_MAX_URL_BYTES <- 1800L

# Pack IDs in input order using the fully rendered URL, including fixed params.
# The current batch determines the next boundary, so preallocate group indices.
# query_result__query_value_batches {{{
query_result__query_value_batches <- function(
    values,
    build_url,
    batch_size = QUERY_RESULT_CHILD_COLLECT_BATCH_SIZE,
    max_url_bytes = QUERY_RESULT_COLLECT_MAX_URL_BYTES
) {
    checkmate::assert_character(values, any.missing = FALSE)
    checkmate::assert_function(build_url)
    checkmate::assert_count(batch_size, positive = TRUE)
    checkmate::assert_count(max_url_bytes, positive = TRUE)
    if (!length(values)) {
        return(list())
    }
    batch_index <- integer(length(values))
    current_batch <- 0L
    first <- 1L
    while (first <= length(values)) {
        last <- min(length(values), first + batch_size - 1L)
        # URL length grows monotonically for these positive identity facets.
        # Find the largest fitting prefix without rendering every added ID.
        if (
            last > first &&
                nchar(build_url(values[first:last]), type = "bytes") >
                    max_url_bytes
        ) {
            lower <- first
            upper <- last - 1L
            while (lower < upper) {
                middle <- ceiling((lower + upper) / 2)
                if (
                    nchar(build_url(values[first:middle]), type = "bytes") <=
                        max_url_bytes
                ) {
                    lower <- middle
                } else {
                    upper <- middle - 1L
                }
            }
            last <- lower
        }
        current_batch <- current_batch + 1L
        batch_index[first:last] <- current_batch
        first <- last + 1L
    }
    split(values, batch_index)
}
# }}}

# Collect disjoint identity groups, retrying only actual HTTP 414 responses.
# Each attempt owns its pages: failed partial pages are discarded before restart.
# query_result__collect_batched {{{
query_result__collect_batched <- function(
    index_node,
    params,
    facet,
    required_fields = NULL,
    all = FALSE,
    limit = TRUE,
    constraints = TRUE,
    ...
) {
    checkmate::assert_choice(facet, c("dataset_id", "instance_id", "master_id"))
    checkmate::assert_flag(all)
    store <- query__collect_params(
        index_node,
        params,
        required_fields,
        all,
        limit,
        constraints
    )
    args <- c(
        list(
            index_node = index_node,
            params = store,
            required_fields = required_fields,
            all = all,
            limit = limit,
            constraints = constraints
        ),
        list(...)
    )
    param <- store$state()[[facet]]
    # Splitting excludes or an explicit offset would change query semantics.
    if (
        is.null(param) ||
            query_param__negate(param) ||
            query_param__value(store$offset()) != 0L
    ) {
        return(do.call(query__collect, args))
    }
    # Repeated exact IDs describe the same union; avoid fetching them twice when
    # a boundary falls between duplicates, while retaining the original params.
    values <- unique(query_param__value(param))
    if (!length(values)) {
        return(do.call(query__collect, args))
    }
    # Render through the same normalized store used by the actual collector.
    build_url <- function(batch) {
        batch_params <- store$copy()
        query_result__merge_params(
            batch_params,
            stats::setNames(list(batch), facet)
        )
        query__build(index_node, batch_params)
    }
    batches <- query_result__query_value_batches(values, build_url)
    results <- vector("list", length(values))
    collected <- 0L
    remaining <- query_param__value(store$limit())
    # A binary retry tree has at most 2n - 1 requests for n distinct IDs.
    receipts <- vector("list", 2L * length(values) - 1L)
    receipt_count <- 0L
    first_attempt <- TRUE

    # Depth is bounded by halving the ID count; completed siblings are retained.
    collect_batch <- function(batch, label = args$progress_label) {
        if (!all && remaining <= 0L) {
            return(invisible(NULL))
        }
        batch_params <- store$copy()
        query_result__merge_params(
            batch_params,
            stats::setNames(list(batch), facet)
        )
        batch_args <- args
        batch_args$params <- batch_params
        if (!is.null(label)) {
            batch_args$progress_label <- label
        }
        if (!all) {
            batch_params$limit(remaining)
            batch_args$limit <- remaining
        }
        if (!is.null(batch_args$dict_check)) {
            batch_args$dict_check <- isTRUE(batch_args$dict_check) &&
                first_attempt
        }
        first_attempt <<- FALSE
        result <- tryCatch(
            do.call(query__collect, batch_args),
            epwshiftr_query_uri_too_long = function(error) error
        )
        if (inherits(result, "epwshiftr_query_uri_too_long")) {
            receipt_count <<- receipt_count + 1L
            receipts[receipt_count] <<- list(result$query_urls)
            if (length(batch) == 1L) {
                cli::cli_abort(
                    c(
                        "The index node rejected a single-ID request (HTTP 414); it cannot be split further.",
                        "i" = "Index node: {index_node}",
                        "i" = "Request URL: {nchar(result$url, type = 'bytes')} bytes; remaining IDs: 1."
                    ),
                    class = "epwshiftr_query_uri_too_long",
                    parent = result
                )
            }
            middle <- length(batch) %/% 2L
            collect_batch(batch[seq_len(middle)], label)
            collect_batch(batch[seq.int(middle + 1L, length(batch))], label)
            return(invisible(NULL))
        }
        collected <<- collected + 1L
        results[[collected]] <<- result
        receipt_count <<- receipt_count + 1L
        receipts[receipt_count] <<- list(result$context$query_url)
        remaining <<- remaining - query__collect_nrow(result$docs)
        invisible(NULL)
    }
    for (i in seq_along(batches)) {
        label <- args$progress_label
        if (!is.null(label) && length(batches) > 1L) {
            label <- sprintf("%s (batch %d/%d)", label, i, length(batches))
        }
        collect_batch(batches[[i]], label)
        if (!all && remaining <= 0L) {
            break
        }
    }
    result <- query_result__merge_child_collects(
        results[seq_len(collected)],
        store,
        all = all,
        limit = query_param__value(store$limit()),
        facet = facet
    )
    result$context$query_url <- query_result__query_urls(
        unlist(receipts[seq_len(receipt_count)], use.names = FALSE),
        named = FALSE
    )
    result
}
# }}}

# query_result__merge_child_collects
# Merge several child query responses into one result object state.
# query_result__merge_child_collects {{{
query_result__merge_child_collects <- function(
    results,
    params,
    all = FALSE,
    limit = NULL,
    facet = "dataset_id"
) {
    if (!length(results)) {
        return(query_result__empty_response(params))
    }
    checkmate::assert_list(results)
    checkmate::assert_flag(all)
    checkmate::assert_count(limit, null.ok = TRUE)

    docs <- data.table::rbindlist(lapply(results, `[[`, "docs"), fill = TRUE)
    if (!isTRUE(all) && !is.null(limit) && query__collect_nrow(docs) > limit) {
        # Query backends should honor the per-batch limit, but trim defensively
        # so the public `limit` remains a global cap across all batches.
        docs <- docs[seq_len(as.integer(limit)), , drop = FALSE]
    }

    response <- results[[length(results)]]$response
    num_found <- vapply(
        results,
        function(result) {
            value <- result$response$response$numFound
            if (is.null(value) || !length(value) || is.na(value[[1L]])) {
                return(as.numeric(query__collect_nrow(result$docs)))
            }
            as.numeric(value[[1L]])
        },
        numeric(1L)
    )

    response$response$docs <- docs
    response$response$numFound <- sum(num_found, na.rm = TRUE)
    response$response$start <- 0L

    query_urls <- unlist(
        lapply(results, function(result) result$context$query_url),
        use.names = FALSE
    )

    # The first effective parameter store contains required field expansion;
    # restore the full identity selection so the final result reflects the caller
    # request rather than the last batch.
    parameter <- query_param__clone(results[[1L]]$parameter)
    query_result__merge_params(
        parameter,
        stats::setNames(
            list(query_param__value(params$state()[[facet]])),
            facet
        )
    )
    parameter$limit(query_param__value(params$limit()))

    list(
        response = response,
        docs = docs,
        parameter = parameter,
        context = list(
            query_url = query_result__query_urls(query_urls, named = FALSE)
        )
    )
}
# }}}
# EsgResultDataset
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
# EsgResultDataset {{{
EsgResultDataset <- R6::R6Class(
    "EsgResultDataset",
    inherit = EsgResult,
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # to_data_table
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
        # to_data_table {{{
        to_data_table = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_data_table(fields, if (formatted) c("url", "size"))
        },
        # }}}
        # has_opendap
        #' @description
        #' Check if there are OPeNDAP support for the datasets
        #'
        #' @return A logical vector.
        #'
        # has_opendap {{{
        has_opendap = function() {
            private$has_access("OPENDAP")
        },
        # }}}
        # has_download
        #' @description
        #' Check if there are HTTPServer download URL for the datasets
        #'
        #' @return A logical vector.
        #'
        # has_download {{{
        has_download = function() {
            private$has_access("HTTPServer")
        },
        # }}}
        # collect
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
        #' @param progress Whether to show a progress bar while collecting ESGF
        #'        child search pages. By default, the value of option
        #'        `epwshiftr.progress` is used, falling back to [interactive()].
        #'
        #' @param ... Optional child-result scope filter `data_node`, plus the
        #'        control parameters `replica`, `distrib`, `latest`, and `shards`.
        #'        Query-level parameters such as `datetime_start` and
        #'        `datetime_stop` cannot be passed through `...`.
        #'        File/Aggregation collection does not use ESGF datetime search
        #'        parameters; call `$filter_time()` on the returned result for
        #'        time filtering. If control parameters are omitted, they are
        #'        inherited from the dataset query when available, with
        #'        `distrib = TRUE` as fallback. If `latest` is omitted and was not
        #'        set on the dataset query, no `latest` constraint is sent. For details
        #'        on possible parameters, please see [esg_query()].
        #'        When a local [EsgDict] is available for the query project, child
        #'        collection performs a warning-only dictionary check before sending
        #'        the query. Missing local dictionaries are ignored and never
        #'        downloaded.
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
        # collect {{{
        collect = function(
            which = NULL,
            fields = NULL,
            all = FALSE,
            limit = 100L,
            type = "File",
            index_node = NULL,
            progress = getOption("epwshiftr.progress", interactive()),
            ...
        ) {
            type <- query_result__type(type, choices = c("File", "Aggregation"))
            checkmate::assert_flag(progress)
            child_index_node <- if (is.null(index_node)) {
                private$index_node
            } else {
                checkmate::assert_string(index_node)
                query__normalize_node(index_node)
            }
            if (!is.null(which)) {
                if (!self$count()) {
                    stop(
                        "Cannot select records from an empty Dataset result.",
                        call. = FALSE
                    )
                } else if (is.character(which)) {
                    checkmate::assert_character(
                        which,
                        any.missing = FALSE,
                        min.len = 1L,
                        unique = TRUE
                    )
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

                which <- if (is.character(which)) {
                    match(which, self$id)
                } else {
                    as.integer(which)
                }
            }

            dots_env <- parent.frame()

            built <- private$build_params(
                fields = fields,
                limit = limit,
                type = type,
                index = which,
                dots_env = dots_env,
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
                result <- query_result__empty_response(params)
            } else {
                collect_args <- list(
                    index_node = child_index_node,
                    params = params,
                    facet = "dataset_id",
                    required_fields = req_fld,
                    all = all,
                    limit = limit,
                    constraints = FALSE,
                    dict_check = TRUE,
                    progress_callback = private$progress_callback
                )
                if (isTRUE(progress)) {
                    collect_args$progress <- TRUE
                    collect_args$progress_label <- sprintf(
                        "Collecting %s records",
                        type
                    )
                }
                result <- do.call(query_result__collect_batched, collect_args)
            }

            # replace docs in the last response
            result$response$response$docs <- result$docs
            result_params <- if (!is.null(result$parameter)) {
                result$parameter
            } else {
                params
            }

            # create new results
            if (type == "File") {
                query_result__new(
                    EsgResultFile,
                    child_index_node,
                    result_params,
                    result$response,
                    context = result$context
                )
            } else if (type == "Aggregation") {
                query_result__new(
                    EsgResultAggregation,
                    child_index_node,
                    result_params,
                    result$response,
                    context = result$context
                )
            }
        },
        # }}}
        # expand_replicas
        #' @description
        #' Query ESGF for Dataset master and replica records.
        #'
        #' @param by Replica identity key. Use `"instance_id"` to retrieve
        #'        same-version Dataset replicas, or `"master_id"` to retrieve all
        #'        versions and replicas for the logical Dataset. Default:
        #'        `"instance_id"`.
        #' @param all Whether to retrieve all matching records. Default:
        #'        `TRUE`.
        #' @param index_node Optional ESGF index node used for the replica query.
        #'        If `NULL`, the index node that created this result is used.
        #'        Default: `NULL`.
        #'
        #' @return A new `EsgResultDataset` object with expanded Dataset records
        #'        when the requested identity key is available; otherwise `self`.
        # expand_replicas {{{
        expand_replicas = function(
            by = c("instance_id", "master_id"),
            all = TRUE,
            index_node = NULL
        ) {
            query_result__expand_datasets(
                self,
                by = by,
                all = all,
                index_node = index_node
            )
        },
        # }}}
        # print
        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgResultDataset` object itself, invisibly.
        # print {{{
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
        # Dataset results can collect child catalogs and report request progress.
        progress_callback = NULL,
        result_type = "Dataset",

        required_fields = sort(unique(c(
            EsgResult$private_fields$required_fields,
            "data_node",
            "index_node",
            "instance_id",
            "latest",
            "master_id",
            "number_of_files",
            "number_of_aggregations",
            "replica",
            "version",
            "access"
        ))),

        # build_params
        # build_params {{{
        build_params = function(
            fields = NULL,
            limit = 100L,
            type = "File",
            index = NULL,
            ...,
            dots_env = parent.frame()
        ) {
            type <- query_result__type(type, choices = c("File", "Aggregation"))

            checkmate::assert_integerish(
                limit,
                lower = 1L,
                upper = this$data_max_limit,
                len = 1L,
                null.ok = TRUE
            )
            if (is.null(limit)) {
                limit <- this$data_max_limit
            }

            overrides <- eval(substitute(alist(...)))
            extra_params <- list()
            if (length(overrides)) {
                names_reserved <- c(
                    "dataset_id",
                    "fields",
                    "facets",
                    "type",
                    "format",
                    "limit",
                    "offset",
                    "query",
                    "_timestamp",
                    "time",
                    query_param__names("date")
                )
                overrides <- eval_with_bang(..., .env = dots_env)

                # stop if unsupported parameter found
                names_params <- names(overrides)
                checkmate::assert_names(
                    names_params,
                    type = "unique",
                    .var.name = "..."
                )
                if (any(!nzchar(names_params))) {
                    stop(
                        "All additional query filters in `...` must be named.",
                        call. = FALSE
                    )
                }
                if (any(invld <- names_params %in% names_reserved)) {
                    stop(
                        sprintf(
                            "The following query parameter(s) are controlled by `$collect()` and cannot be set in `...`: [%s].",
                            paste(
                                sprintf("'%s'", names_params[invld]),
                                collapse = ", "
                            )
                        ),
                        call. = FALSE
                    )
                }

                names_ctrl <- c("replica", "distrib", "latest", "shards")
                extra_params <- overrides[!names_params %in% names_ctrl]
                names_extra <- names(extra_params)
                if (length(names_extra)) {
                    allowed_child_filters <- "data_node"
                    unsupported <- setdiff(names_extra, allowed_child_filters)
                    if (length(unsupported)) {
                        stop(
                            sprintf(
                                "Only `data_node` and control parameters (`replica`, `distrib`, `latest`, `shards`) can be passed through `...` for child File/Aggregation collection; unsupported parameter(s): [%s].",
                                paste(
                                    sprintf("'%s'", unsupported),
                                    collapse = ", "
                                )
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
                        QueryParamFacet(
                            param$value,
                            negate = isTRUE(param$negate),
                            encoded = FALSE
                        )
                    }),
                    names(extra_params)
                )
                extra_params <- extra_params[
                    !vapply(extra_params, is.null, logical(1L))
                ]

                overrides <- overrides[names_params %in% names_ctrl]
                if (
                    length(overrides) &&
                        any(vapply(
                            overrides,
                            function(param) isTRUE(param$negate),
                            logical(1L)
                        ))
                ) {
                    stop(
                        "Control parameters in `...` do not support negation.",
                        call. = FALSE
                    )
                }
            }

            param_value <- function(param) {
                if (is.null(param)) {
                    return(NULL)
                }
                if (S7::S7_inherits(param, QueryParam)) {
                    return(query_param__value(param))
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

            store <- QueryParamStore$new()
            store$project(NULL)
            query_result__merge_params(
                store,
                c(extra_params, list(dataset_id = dataset_id))
            )
            store$fields(query_param__value(query$fields(fields)$fields()))
            store$shards(query_param__value(query$shards(
                controls$shards
            )$shards()))
            store$replica(query_param__value(query$replica(
                controls$replica
            )$replica()))
            store$latest(query_param__value(query$latest(
                controls$latest
            )$latest()))
            store$distrib(query_param__value(query$distrib()))
            store$limit(limit)
            store$offset(0L)
            store$type(type)
            store$format(QUERY_PARAM__FORMAT_JSON)
            store$facets(NULL)
            store$datetime_range(start = NULL, stop = NULL)

            list(params = store, limit = limit)
        }
        # }}}
    )
)
# }}}
# EsgResultFile
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
# EsgResultFile {{{
EsgResultFile <- R6::R6Class(
    "EsgResultFile",
    inherit = EsgResult,
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # to_data_table
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
        # to_data_table {{{
        to_data_table = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_data_table(fields, if (formatted) c("url", "size"))
        },
        # }}}
        # filter_time
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
        #' @param method How to determine file time ranges. `"auto"` prefers
        #'        ESGF metadata and fills absent ranges from DRS filenames;
        #'        `"drs"` always parses filenames, and `"opendap"` reads the
        #'        remote time axis. Default: `"drs"`.
        #'
        #' @return A new `EsgResultFile` object.
        # filter_time {{{
        filter_time = function(
            start,
            stop,
            method = c("drs", "opendap", "auto")
        ) {
            private$filter_time_result(
                start,
                stop,
                method = method,
                result_label = "file"
            )
        },
        # }}}
        # download_plan
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
        # download_plan {{{
        download_plan = function(
            replica = c("auto", "current"),
            service = "HTTPServer",
            probe = TRUE,
            strategy = c("fastest", "first", "stable"),
            all = TRUE,
            node_stats = NULL,
            network_policy = NULL,
            node_policy = NULL,
            probe_concurrency = 1L,
            probe_cache_seconds = 3600L
        ) {
            replica <- match.arg(replica)
            strategy <- match.arg(strategy)
            target <- if (identical(replica, "auto")) {
                self$expand_replicas(service = service, all = all)
            } else {
                self
            }
            query_result__download_plan(
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
        # expand_replicas
        #' @description
        #' Query ESGF for same-version master and replica records.
        #'
        #' @param service ESGF URL service to keep in the method contract.
        #'        Default: `"HTTPServer"`.
        #' @param all Whether to retrieve all matching records. Default:
        #'        `TRUE`.
        #'
        #' @return A new `EsgResultFile` object with expanded replica records
        #'        when `instance_id`, or `master_id` plus `version`, is available;
        #'        otherwise `self`.
        # expand_replicas {{{
        expand_replicas = function(service = "HTTPServer", all = TRUE) {
            query_result__expand_files(self, service = service, all = all)
        },
        # }}}
        # repair_urls
        #' @description
        #' Replace unreachable service URLs from compatible replicas.
        #'
        #' `$repair_urls()` checks the current records for the selected service,
        #' queries compatible ESGF replicas for records whose URL is missing or
        #' unreachable, checks candidate replica URLs, and returns a new result with
        #' only that service URL repaired in the original row order. The logical
        #' record and its other service URLs are retained. The original result is
        #' not modified. The returned result records the original query URL plus
        #' the replica lookup query URL in `$query_url("all")`.
        #'
        #' @param service Service URL to repair. One of `"OPENDAP"` or
        #'        `"HTTPServer"`. Default: `"OPENDAP"`.
        #' @param index_node Optional ESGF search index node used to look up
        #'        replicas. If `NULL`, the current result index node is used.
        #' @param probe Optional named list of probe settings. Supported fields
        #'        are `level`, `timeout`, `concurrency`, `network_policy`,
        #'        `cache_seconds`, and `cache_failures_seconds`. Default
        #'        `level` is `"data_node"`.
        #'
        #' @return A new `EsgResultFile` object.
        # repair_urls {{{
        repair_urls = function(
            service = c("OPENDAP", "HTTPServer"),
            index_node = NULL,
            probe = NULL
        ) {
            query_result__repair_urls(
                self,
                service = service,
                index_node = index_node,
                probe = probe
            )
        },
        # }}}
        # select_replica
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
        # select_replica {{{
        select_replica = function(
            strategy = c("fastest", "first", "stable"),
            probe = TRUE,
            service = "HTTPServer",
            node_stats = NULL,
            network_policy = NULL,
            node_policy = NULL,
            probe_concurrency = 1L,
            probe_cache_seconds = 3600L
        ) {
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
        # download
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
        # download {{{
        download = function(
            downloader = NULL,
            store = NULL,
            replica = c("auto", "current"),
            service = "HTTPServer",
            probe = TRUE,
            strategy = c("fastest", "first", "stable"),
            probe_concurrency = NULL,
            probe_cache_seconds = 3600L,
            session_label = NULL,
            run = TRUE,
            ...
        ) {
            replica <- match.arg(replica)
            query_result__download(
                self,
                downloader = downloader,
                store = store,
                replica = replica,
                service = service,
                probe = probe,
                strategy = strategy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds,
                session_label = session_label,
                run = run,
                ...
            )
        },
        # }}}
        # print
        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgResultFile` object itself, invisibly.
        # print {{{
        print = function(n = 10L) {
            private$print_header("File")
            private$print_summary("File")
            private$print_parameters()
            cli::cat_line()
            private$print_contents("File", n)
            invisible(self)
        },
        # }}}
        # open_dataset
        #' @description
        #' Open a file as an EsgDataset for remote data access via OPeNDAP
        #'
        #' @param which File records to open. Use integer indices or file IDs.
        #'        If `NULL`, all file records are opened. Default: `NULL`.
        #' @param fallback What to do if OPeNDAP is unavailable. One of:
        #'   - `"ask"`: Interactively ask the user (default). In a
        #'     non-interactive session this raises an error.
        #'   - `"auto"`: Automatically download the file via HTTPServer.
        #'   - `"error"`: Raise an error.
        #'
        #' @param store Optional [EsgStore] used for recoverable HTTP fallback.
        #' @param downloader Optional persistent [Downloader] used for
        #'        recoverable HTTP fallback.
        #' @param progress Whether to show progress bars while opening
        #'        OPeNDAP records and any HTTP fallback downloads. By default
        #'        the package option `epwshiftr.progress` is used, falling back
        #'        to [interactive()].
        #'
        #' @return An `EsgDataset` object with the connection already opened.
        # open_dataset {{{
        open_dataset = function(
            which = NULL,
            fallback = c("ask", "auto", "error"),
            store = NULL,
            downloader = NULL,
            progress = getOption("epwshiftr.progress", interactive())
        ) {
            query_result__open_dataset(
                self,
                which = which,
                fallback = fallback,
                store = store,
                downloader = downloader,
                progress = progress,
                result_label = "File",
                empty_message = "No file records are available to open.",
                unavailable_message = "OPeNDAP is not available for these file records.",
                http_missing_message = "HTTPServer download URLs are missing for one or more file records."
            )
        }
        # }}}
    ),
    active = list(
        # filename
        #' @field filename A character vector indicating file names on the
        #'        sever.
        # filename {{{
        filename = function() {
            private$get_field("title")
        },
        # }}}
        # url_opendap
        #' @field url_opendap A character vector of the OPeNDAP URLs of the
        #'        files.
        # url_opendap {{{
        url_opendap = function() {
            url <- private$get_url("OPENDAP", "OPeNDAP")

            has_html <- !is.na(url) & tools::file_ext(url) == "html"
            if (any(has_html)) {
                url[has_html] <- tools::file_path_sans_ext(url[has_html])
            }
            url
        },
        # }}}
        # url_download
        #' @field url_download A character vector of the download URLs of the
        #'        files.
        # url_download {{{
        url_download = function() {
            private$get_url("HTTPServer")
        },
        # }}}
        # fields
        #' @field fields A character vector indicating all response fields,
        #'        followed by derived fields such as `filename`, `url_opendap`
        #'        and `url_download` when their source fields are available.
        # fields {{{
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
# EsgResultAggregation
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
# EsgResultAggregation {{{
EsgResultAggregation <- R6::R6Class(
    "EsgResultAggregation",
    inherit = EsgResult,
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        # to_data_table
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
        # to_data_table {{{
        to_data_table = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_data_table(fields, if (formatted) c("url", "size"))
        },
        # }}}
        # filter_time
        #' @description
        #' Filter aggregation records by the time range covered by each file
        #'
        #' See `EsgResultFile$filter_time()` for the method semantics.
        #'
        #' @param start,stop Time range boundaries. Character, `Date`, and
        #'        `POSIXt` inputs are accepted and parsed in UTC.
        #' @param method How to determine file time ranges. `"auto"` prefers
        #'        ESGF metadata and fills absent ranges from DRS filenames;
        #'        `"drs"` always parses filenames, and `"opendap"` reads the
        #'        remote time axis. Default: `"drs"`.
        #'
        #' @return A new `EsgResultAggregation` object.
        # filter_time {{{
        filter_time = function(
            start,
            stop,
            method = c("drs", "opendap", "auto")
        ) {
            private$filter_time_result(
                start,
                stop,
                method = method,
                result_label = "aggregation"
            )
        },
        # }}}
        # download_plan
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
        # download_plan {{{
        download_plan = function(
            replica = c("current", "auto"),
            service = "HTTPServer",
            probe = TRUE,
            strategy = c("fastest", "first", "stable"),
            all = TRUE,
            node_stats = NULL,
            network_policy = NULL,
            node_policy = NULL,
            probe_concurrency = 1L,
            probe_cache_seconds = 3600L
        ) {
            replica <- match.arg(replica)
            strategy <- match.arg(strategy)
            query_result__download_plan(
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
        # repair_urls
        #' @description
        #' Replace unreachable service URLs from compatible replicas.
        #'
        #' `$repair_urls()` checks the current records for the selected service,
        #' queries compatible ESGF replicas for records whose URL is missing or
        #' unreachable, checks candidate replica URLs, and returns a new result with
        #' only that service URL repaired in the original row order. The logical
        #' record and its other service URLs are retained. The original result is
        #' not modified. Aggregation results are not downloadable as files;
        #' repaired aggregation URLs are intended for service access such as OPeNDAP.
        #' The returned result records the original query URL plus the replica
        #' lookup query URL in `$query_url("all")`.
        #'
        #' @param service Service URL to repair. One of `"OPENDAP"` or
        #'        `"HTTPServer"`. Default: `"OPENDAP"`.
        #' @param index_node Optional ESGF search index node used to look up
        #'        replicas. If `NULL`, the current result index node is used.
        #' @param probe Optional named list of probe settings. Supported fields
        #'        are `level`, `timeout`, `concurrency`, `network_policy`,
        #'        `cache_seconds`, and `cache_failures_seconds`. Default
        #'        `level` is `"data_node"`.
        #'
        #' @return A new `EsgResultAggregation` object.
        # repair_urls {{{
        repair_urls = function(
            service = c("OPENDAP", "HTTPServer"),
            index_node = NULL,
            probe = NULL
        ) {
            query_result__repair_urls(
                self,
                service = service,
                index_node = index_node,
                probe = probe
            )
        },
        # }}}
        # download
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
        # download {{{
        download = function(
            downloader = NULL,
            store = NULL,
            replica = c("current", "auto"),
            service = "HTTPServer",
            probe = TRUE,
            strategy = c("fastest", "first", "stable"),
            probe_concurrency = NULL,
            probe_cache_seconds = 3600L,
            session_label = NULL,
            run = TRUE,
            ...
        ) {
            replica <- match.arg(replica)
            query_result__download(
                self,
                downloader = downloader,
                store = store,
                replica = replica,
                service = service,
                probe = probe,
                strategy = strategy,
                probe_concurrency = probe_concurrency,
                probe_cache_seconds = probe_cache_seconds,
                session_label = session_label,
                run = run,
                ...
            )
        },
        # }}}
        # print
        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgResultAggregation` object itself, invisibly.
        # print {{{
        print = function(n = 10L) {
            private$print_header("Aggregation")
            private$print_summary("Aggregation")
            private$print_parameters()
            cli::cat_line()
            private$print_contents("Aggregation", n)
            invisible(self)
        },
        # }}}
        # open_dataset
        #' @description
        #' Open aggregation files as an EsgDataset for remote data access via OPeNDAP
        #'
        #' @param which Aggregation records to open. Use integer indices or
        #'        aggregation record IDs. If `NULL`, all aggregation records
        #'        are opened. Default: `NULL`.
        #' @param fallback What to do if OPeNDAP is unavailable. One of:
        #'   - `"ask"`: Interactively ask the user (default). In a
        #'     non-interactive session this raises an error.
        #'   - `"auto"`: Automatically download files via HTTPServer.
        #'   - `"error"`: Raise an error.
        #'
        #' @param store Optional [EsgStore] used for recoverable HTTP fallback.
        #' @param downloader Optional persistent [Downloader] used for
        #'        recoverable HTTP fallback.
        #' @param progress Whether to show progress bars while opening
        #'        OPeNDAP records and any HTTP fallback downloads. By default
        #'        the package option `epwshiftr.progress` is used, falling back
        #'        to [interactive()].
        #'
        #' @return An `EsgDataset` object with the connection already opened.
        # open_dataset {{{
        open_dataset = function(
            which = NULL,
            fallback = c("ask", "auto", "error"),
            store = NULL,
            downloader = NULL,
            progress = getOption("epwshiftr.progress", interactive())
        ) {
            query_result__open_dataset(
                self,
                which = which,
                fallback = fallback,
                store = store,
                downloader = downloader,
                progress = progress,
                result_label = "Aggregation",
                empty_message = "No aggregation records are available to open.",
                unavailable_message = "OPeNDAP is not available for these aggregation records.",
                http_missing_message = "HTTPServer download URLs are missing for one or more aggregation records."
            )
        }
        # }}}
    ),

    active = list(
        # url_opendap
        #' @field url_opendap A character vector of the OPeNDAP URLs of the
        #'        files.
        # url_opendap {{{
        url_opendap = function() {
            url <- private$get_url("OPENDAP", "OPeNDAP")
            has_html <- !is.na(url) & tools::file_ext(url) == "html"
            if (any(has_html)) {
                url[has_html] <- tools::file_path_sans_ext(url[has_html])
            }
            url
        },
        # }}}
        # url_download
        #' @field url_download A character vector of the download URLs of the
        #'        files.
        # url_download {{{
        url_download = function() {
            private$get_url("HTTPServer")
        },
        # }}}
        # fields
        #' @field fields A character vector indicating all response fields,
        #'        followed by derived URL fields when their source fields are
        #'        available.
        # fields {{{
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
# query_result__response
# Normalize empty ESGF facet buckets to named empty lists so saved-result schema
# validation sees a JSON object instead of an unnamed array.
# query_result__named_empty_facets {{{
query_result__named_empty_facets <- function(x) {
    if (is.null(x) || (is.list(x) && !length(x))) {
        return(stats::setNames(list(), character()))
    }
    x
}
# }}}

# Normalize the parts of an ESGF response whose JSON shape is ambiguous when
# ESGF returns no records or no facet counts.
# query_result__response_facets {{{
query_result__response_facets <- function(response) {
    if (is.null(response$facet_counts)) {
        response$facet_counts <- list()
    }
    for (name in c(
        "facet_queries",
        "facet_fields",
        "facet_ranges",
        "facet_intervals",
        "facet_heatmaps"
    )) {
        response$facet_counts[[
            name
        ]] <- query_result__named_empty_facets(response$facet_counts[[name]])
    }
    response
}
# }}}

# query_result__response {{{
query_result__response <- function(response) {
    if (is.null(response)) {
        return(response)
    }

    docs <- response$response$docs
    if (
        is.null(docs) ||
            (is.list(docs) && !is.data.frame(docs) && !length(docs))
    ) {
        response$response$docs <- data.frame(check.names = FALSE)
    }

    query_result__response_facets(response)
}
# }}}
# query_result__empty_response
# query_result__empty_response {{{
query_result__empty_response <- function(params) {
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
            facet_queries = stats::setNames(list(), character()),
            facet_fields = stats::setNames(list(), character()),
            facet_ranges = stats::setNames(list(), character()),
            facet_intervals = stats::setNames(list(), character()),
            facet_heatmaps = stats::setNames(list(), character())
        ),
        timestamp = Sys.time()
    )

    list(
        response = response,
        docs = response$response$docs,
        parameter = query_param__clone(params),
        context = list(query_url = character())
    )
}
# }}}
# query_result__new
# query_result__new {{{
query_result__new <- function(
    generator,
    index_node = NULL,
    params = NULL,
    result = NULL,
    ...
) {
    generator$new(index_node, params, result, ...)
}
# }}}
# result subset method
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
# `[.EsgResult` {{{
`[.EsgResult` <- function(x, i, j, ..., drop = FALSE) {
    if (
        nargs() > 2L ||
            !missing(j) ||
            length(list(...)) ||
            !identical(drop, FALSE)
    ) {
        stop(
            "EsgResult subsetting only supports one-dimensional `result[i]`.",
            call. = FALSE
        )
    }
    if (missing(i)) {
        return(x)
    }

    x$slice(i)
}
# }}}
# esg_result
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
# esg_result {{{
esg_result <- function(type = c("dataset", "file", "aggregation")) {
    type <- match.arg(type)

    query_result__new(
        switch(
            type,
            "dataset" = EsgResultDataset,
            "file" = EsgResultFile,
            "aggregation" = EsgResultAggregation
        ),
        index_node = NULL,
        params = NULL,
        result = NULL
    )
}
# }}}

# vim: fdm=marker :
