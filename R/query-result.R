#' Base class for results for ESGF query
#'
#' @description
#'
#' `EsgfQueryResult` is a base class that represents basic query results from
#' ESGF search RESTful API. It defines common fields and methods for results
#' from all query types, including `Dataset`, `File` and `Aggregation`. Results
#' from the three types are
#'
#' In general, there is no need to create an `EsgfQueryResult` manually.
#'
#' @author Hongyuan Jia
#' @name EsgfQueryResult
EsgfQueryResult <- R6::R6Class("EsgfQueryResult",
    lock_class = TRUE,
    public = list(
        #' @description
        #' Create a new EsgfQueryResult object
        #'
        #' @param host The URL to the ESGF Search API service. This should be
        #'        the URL of the ESGF search service excluding the final
        #'        endpoint name. It should be the same as the `host` for an
        #'        [EsgfQuery] object that collects the query results.
        #'
        #' @param params A list of query parameters.
        #'
        #' @param result The result of an query response.
        #'
        #' @return An `EsgfQueryResult` object.
        #'
        initialize = function(host, params, result) {
            private$url_host <- host
            private$parameters <- params
            private$result <- result
            self
        },

        #' @description
        #' Convert the results into a [data.table][data.table::data.table()]
        #'
        #' @param fields A character vector indicating the fields to put into
        #'        the `data.table`. If `NULL`, all fields in the query result
        #'        will be used. Possible field names can be retrieved using
        #'        `$fields`. Default: `NULL`.
        #'
        #' @param formatted Whether to use formatted values for special fields,
        #'        including `url` and `size`. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_dt = function(fields = NULL, formatted = NULL) {
            docs <- private$get_docs()

            checkmate::assert_subset(fields, names(docs))
            checkmate::assert_character(formatted, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

            if (length(fields)) {
                docs <- docs[match(fields, names(docs))]
            }
            if (!is.null(formatted)) {
                for (field in formatted) {
                    # nocov start
                    if (is.null(docs[[field]])) next
                    # nocov end
                    docs[[field]] <- self[[field]]
                }
            }

            res <- data.table::setDT(
                lapply(docs, function(doc) {
                    if (typeof(doc) == "list") {
                        len <- lengths(doc)
                        if (all(len <= 1L)) {
                            doc[len == 0L] <- list(NA)
                            doc <- unlst(doc)
                        }
                    }
                    doc
                })
            )

            res
        },

        #' @description
        #' Count the number of matched records in current result
        #'
        #' @return An integer.
        #'
        count = function() {
            length(self$id)
        }
    ),

    active = list(
        #' @field id A character vector indicating globally unique record
        #'        identifiers.
        id = function() {
            private$get_field("id")
        },

        #' @field url A list of [data.table][data.table::data.table()] with 3
        #'        columns:
        #'        (a) `service` \\[`character`\\]: The service types, e.g.
        #'        OPENDAP, HTTPServer, etc.;
        #'        (b) `url` \\[`character`\\]: The actual URLs;
        #'        (c) `mime_type` \\[`character`\\]: The MIME types indicating
        #'        the nature and format of the corresponding document of the
        #'        URLs.
        url = function() {
            urls <- private$get_field("url")
            # nocov start
            if (!length(urls)) return(NULL)
            # nocov end

            lapply(urls, function(url) {
                if (!length(url)) return(NULL) # nocov

                s <- strsplit(url, "|", fixed = TRUE)
                # nocov start
                if (any(unreco <- lengths(s) != 3L)) s[unreco] <- NULL
                if (!length(s)) return(NULL)
                # nocov end

                res <- data.table::setDT(data.table::transpose(s))
                data.table::setcolorder(res, c(3L, 1L, 2L))
                data.table::setnames(res, c("service", "url", "mime_type"))
                res
            })
        },

        #' @field size A vector of [units][units::as_units()] indicating the
        #'        file sizes.
        size = function() {
            size <- private$get_field("size")
            set_size_units(size)
        },

        #' @field fields A character vector indicating all fields in the results.
        fields = function() {
            # nocov start
            if (!self$count()) return(NULL)
            # nocov end
            sort(names(private$get_docs()))
        }
    ),

    private = list(
        url_host = NULL,
        parameters = NULL,
        result = NULL,
        last_response = NULL,

        required_fields = c("id", "size", "url"),

        get_docs = function() {
            private$result$response$docs
        },

        get_field = function(field) {
            val <- private$result$response$docs[[field]]
            if (all(lengths(val) == 1L)) unlst(val) else val
        },

        get_url = function(type, name = type) {
            vapply(self$url, function(dt_url) {
                # nocov start
                if (!length(dt_url)) return(NA_character_)
                # nocov end

                res <- dt_url$url[dt_url$service == type]
                if (!length(res)) return(NA_character_)
                # nocov start
                if (length(res) > 1L) {
                    warning(sprintf("Multiple %s URL found. Only the first is returned.", name))
                    res <- res[[1L]]
                }
                # nocov end

                res
            }, character(1L))
        },

        print_header = function(type = "") {
            d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
            cli::cli_rule("ESGF Query Result [{type}]")
            cli::cli_end(d)
        },

        print_summary = function(type = "") {
            cli::cli_bullets(c("*" = "Host: {private$url_host}"))
            cli::cli_bullets(c("*" = "Collected at: {private$result$timestamp}"))
            cli::cli_bullets(c("*" = "Result count: {self$count()}"))
            if (type == "Aggregation") {
                cli::cli_bullets(c("*" = "Total size: <{.emph Unknown}> [Byte]"))
            } else {
                cli::cli_bullets(c("*" = "Total size: {format(round(set_size_units(sum(self$size)), 2L))}"))
            }
            if (is.null(self$fields)) {
                cli::cli_bullets(c("*" = "Fields: 0"))
            } else {
                cli::cli_bullets(c("*" = "Fields: {length(self$fields)} | [ {self$fields} ]"))
            }
        },

        print_parameters = function() {
            cli::cli_h1("<Query Parameter>")
            print_query_params(private$parameters)
        },

        print_contents = function(type, n) {
            checkmate::assert_count(n, positive = TRUE, null.ok = TRUE)

            if (is.null(self$data_node)) {
                cli::cli_rule("<{type}>")
            } else {
                cli::cli_rule("<{type}> (From {length(unique(self$data_node))} Data Nodes)")
            }

            if (self$count() == 0L) {
                cli::cli_bullets(c(" " = "{.strong <Empty>}"))
                cli::cli_bullets(c(" " = "{.emph NOTE: No matched data found. Please update query parameters and try again.}"))
                return()
            }

            checkmate::assert_count(n, positive = TRUE, null.ok = TRUE)
            n <- if (is.null(n)) self$count() else min(n, self$count())
            ind <- seq_len(n)

            pre <- lpad(ind, "0")
            brief <- sprintf("[%s] %s", pre, self$id[ind])

            spc <- strrep(" ", nchar(pre[1L], "width"))

            if (type == "Dataset") {
                size <- sprintf("%s   [ %s Files, %s %s | %s ]\n%s   [ Access: <%s> ]",
                    spc, self$number_of_files[ind],
                    round(self$size[ind], 2), units(self$size)$numerator,
                    if (is.null(self$number_of_aggregations)) {
                        "No Aggregations"
                    } else {
                        agg <- self$number_of_aggregations
                        agg[is.na(agg)] <- 0L
                        paste(agg,
                            vapply(agg, ngettext, "", "Aggregation", "Aggregations")
                        )
                    },
                    spc,
                    if (is.null(self$access)) {
                        "NONE"
                    } else {
                        vapply(self$access[ind], paste0, "", collapse = ", ")
                    }
                )
            } else {
                size <- sprintf("%s   [ %s %s | Access: <%s> ]",
                    spc,
                    if (type == "Aggregation") "<Unknown>" else round(self$size[ind], 2),
                    units(self$size)$numerator,
                    if (is.null(self$url)) {
                        "NONE"
                    } else {
                        vapply(self$url[ind], FUN.VALUE = character(1),
                            function(url) paste0(url$service, collapse = ", ")
                        )
                    }
                )
            }

            for (i in ind) {
                cli::cat_line(.subset2(brief, i))
                cli::cat_line(.subset2(size, i))
            }

            print_trunc(self$id, n)
        }
    )
)

#' ESGF Query results for `Dataset` type
#'
#' @description
#'
#' `EsgfQueryResultDataset` is a class that represents query results for
#' `Dataset` type from ESGF search RESTful API.
#'
#' In general, there is no need to create an `EsgfQueryResultDataset` manually.
#' Usually, it is created by calling
#' \href{#method-EsgfQuery-collect}{\code{EsgfQuery$collect()}}.
#'
#' @author Hongyuan Jia
#' @name EsgfQueryResultDataset
EsgfQueryResultDataset <- R6::R6Class("EsgfQueryResultDataset",
    inherit = EsgfQueryResult, lock_class = TRUE,
    public = list(
        #' @description
        #' Convert the results into a [data.table][data.table::data.table()]
        #'
        #' @param fields A character vector indicating the fields to put into
        #'        the `data.table`. If `NULL`, all fields in the query result
        #'        will be used. Default: `NULL`.
        #'
        #' @param formatted Whether to use formatted values for special fields,
        #'        including `url` and `size`. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_dt = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_dt(fields, if (formatted) c("url", "size"))
        },

        #' @description
        #' Check if there are OPeNDAP support for the datasets
        #'
        #' @return A logical vector.
        #'
        has_opendap = function() {
            vapply(self$access, function(acc) "OPENDAP" %in% acc, logical(1L))
        },

        #' @description
        #' Check if there are HTTPServer download URL for the datasets
        #'
        #' @return A logical vector.
        #'
        has_download = function() {
            vapply(self$access, function(acc) "HTTPServer" %in% acc, logical(1L))
        },

        #' @description
        #' Collect file or aggregation information for current datasets
        #'
        #' `$collect()` sends a query with **`type=File`** or
        #' **`type=aggregation` (based on the specified `type`) for current
        #' datasets and returns an
        #' [EsgfQueryResultFile] or
        #' [EsgfQueryResultAggregation] object, respectively.
        #'
        #' The following fields are always included in the results:
        #'
        #' - For `File` query: `r paste0("\\verb{", EsgfQueryResultFile$private_fields$required_fields, "}", collapse = ", ")`.
        #'
        #' - For `Aggregation` query: `r paste0("\\verb{", EsgfQueryResultAggregation$private_fields$required_fields, "}", collapse = ", ")`.
        #'
        #' @param which A character vector giving the value of dataset ID or an
        #'        integer vector giving the indices of the dataset. If `NULL`,
        #'        all datasets will be sent. Default: `NULL`.
        #'
        #' @param fields A character vector indicating the value of `fields`
        #'        parameter when sending the query. If `NULL`, all available
        #'        fields will be included. Default: `NULL`.
        #'
        #' @param all A flag. Whether to collect all results. Default: `FALSE`.
        #'
        #' @param limit A positive integer indicating the number of records to
        #'        fetch per query. If `NULL`, the allowed maximum limit number
        #'        `r this$data_max_limit` is used. Default: `100L`.
        #'
        #' @param type A string indicating the query type. Should be one of
        #'        `File` or `Aggregation`. Default: `"File"`.
        #'
        #' @param ... Other parameters to set. Currently, there are 4 parameters
        #'        supported, including `replica`, `distrib`, `latest`, `shards`.
        #'        For details on possible parameters, please see [query_esgf()].
        #'
        #' @return
        #'
        #' - If `type="File"`, an [EsgfQueryResultFile] object
        #' - If `type="Aggregation"`, an [EsgfQueryResultAggregation] object
        #'
        collect = function(which = NULL, fields = NULL, all = FALSE, limit = 100L, type = "File", ...) {
            if (!is.null(which)) {
                if (!self$count()) {
                    which <- NULL
                } else {
                    checkmate::assert(
                        checkmate::check_subset(which, self$id),
                        checkmate::check_integerish(which,
                            lower = 1L, upper = self$count(), any.missing = FALSE,
                            unique = TRUE
                        )
                    )
                }

                which <- if (is.character(which)) match(which, self$id) else as.integer(which)
            }

            params <- private$build_params(
                fields = fields, limit = limit, type = type, index = which, ...
            )

            req_fld <- if (type == "File") {
                EsgfQueryResultFile$private_fields$required_fields
            } else if (type == "Aggregation") {
                EsgfQueryResultAggregation$private_fields$required_fields
            }

            result <- query_collect(
                private$url_host, params, required_fields = req_fld,
                all = all, limit = limit, constraints = FALSE
            )
            private$last_response <- result$response

            # replace docs in the last response
            result$response$response$docs <- result$docs

            # create new results
            if (type == "File") {
                new_query_result(
                    EsgfQueryResultFile,
                    private$url_host, params, result$response
                )
            } else if (type == "Aggregation") {
                new_query_result(
                    EsgfQueryResultAggregation,
                    private$url_host, params, result$response
                )
            }
        },

        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgfQueryResultDataset` object itself, invisibly.
        print = function(n = 10L) {
            private$print_header("Dataset")
            private$print_summary("Dataset")
            private$print_parameters()
            cli::cat_line()
            private$print_contents("Dataset", n)
            invisible(self)
        }
    ),

    private = list(
        required_fields = sort(unique(c(
            EsgfQueryResult$private_fields$required_fields,
            "index_node", "number_of_files", "number_of_aggregations", "access"
        ))),

        build_params = function(fields = NULL, limit = 100L, type = "File", index = NULL, ...) {
            checkmate::assert_choice(type, c("File", "Aggregation"))

            checkmate::assert_integerish(limit, lower = 1L, upper = this$data_max_limit, len = 1L, null.ok = TRUE)
            if (is.null(limit)) limit <- this$data_max_limit

            params <- eval(substitute(alist(...)))
            if (length(params)) {
                # other parameters
                names_supp <- c("replica", "distrib", "latest", "shards")
                params <- eval_with_bang(...)

                # stop if unsupported parameter found
                names_params <- names(params)
                if (any(invld <- !names_params %in% names_supp)) {
                    stop(sprintf(
                        "Unsupported query parameter found: [%s]. Should be subset of [%s].",
                        names_params[invld], paste(names_supp, collapse = ", ")
                    ))
                }
            }

            # assign default values for distrib and latest
            if (is.null(params$distrib)) params$distrib <- new_query_param("distrib", TRUE)
            if (is.null(params$latest)) params$latest <- new_query_param("latest", TRUE)

            # create a new query to validate params
            query <- query_esgf(private$url_host, build = private$url_host %in% names(this$cache$facet))

            params <- list(
                dataset_id = if (is.null(index)) self$id else self$id[index],

                # use query object to validate params
                fields = query$fields(fields)$fields(),
                shards = query$shards(params$shards$value)$shards(),
                replica = query$replica(params$replica$value)$replica(),
                latest = query$latest(params$latest$value)$latest(),
                distrib = query$distrib(params$distrib$value)$distrib(),

                limit = limit,
                type = type,
                format = "application/solr+json"
            )

            # convert all inputs into query params and remove empty one
            query_param_flat(params)
        }
    )
)

#' ESGF Query results for `File` type
#'
#' @description
#'
#' `EsgfQueryResultFile` is a class that represents query results for
#' `File` type from ESGF search RESTful API.
#'
#' In general, there is no need to create an `EsgfQueryResultDataset` manually.
#' Usually, it is created by calling
#' \href{#method-EsgfQueryResultDataset-collect}{\code{EsgfQueryResultDataset$collect()}}.
#'
#' @author Hongyuan Jia
#' @name EsgfQueryResultFile
EsgfQueryResultFile <- R6::R6Class("EsgfQueryResultFile",
    inherit = EsgfQueryResult, lock_class = TRUE,
    public = list(
        #' @description
        #' Convert the results into a [data.table][data.table::data.table()]
        #'
        #' @param fields A character vector indicating the fields to put into
        #'        the `data.table`. If `NULL`, all fields in the query result
        #'        will be used. Default: `NULL`.
        #'
        #' @param formatted Whether to use formatted values for special fields,
        #'        including `url` and `size`. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_dt = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_dt(fields, if (formatted) c("url", "size"))
        },

        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgfQueryResultFile` object itself, invisibly.
        print = function(n = 10L) {
            private$print_header("File")
            private$print_summary("File")
            private$print_parameters()
            cli::cat_line()
            private$print_contents("File", n)
            invisible(self)
        }
    ),
    active = list(
        #' @field filename A character vector indicating file names on the
        #'        sever.
        filename = function() {
            private$get_field("title")
        },

        #' @field url_opendap A character vector of the OPeNDAP URLs of the
        #'        files.
        url_opendap = function() {
            url <- private$get_url("OPENDAP", "OPeNDAP")
            has_html <- tools::file_ext(url) == "html"
            if (any(has_html)) {
                url[has_html] <- tools::file_path_sans_ext(url[has_html])
            }
            url
        },

        #' @field url_download A character vector of the download URLs of the
        #'        files.
        url_download = function() {
            private$get_url("HTTPServer")
        },

        #' @field fields A character vector indicating all fields in the results.
        fields = function() {
            # nocov start
            if (!self$count()) return(NULL)
            # nocov end
            sort(c("filename", "url_opendap", "url_download", super$fields))
        }
    ),
    private = list(
        required_fields = sort(unique(c(
            EsgfQueryResult$private_fields$required_fields,
            "dataset_id", "checksum", "checksum_type", "tracking_id", "title",
            "data_node"
        )))
    )
)

#' ESGF Query results for `Aggregation` type
#'
#' @description
#'
#' `EsgfQueryResultAggregation` is a class that represents query results for
#' `Aggregation` type from ESGF search RESTful API.
#'
#' In general, there is no need to create an `EsgfQueryResultAggregation` manually.
#' Usually, it is created by calling
#' \href{#method-EsgfQueryResultDataset-collect}{\code{EsgfQueryResultDataset$collect()}}.
#'
#' @author Hongyuan Jia
#' @name EsgfQueryResultAggregation
EsgfQueryResultAggregation <- R6::R6Class("EsgfQueryResultAggregation",
    inherit = EsgfQueryResult, lock_class = TRUE,
    public = list(
        #' @description
        #' Convert the results into a [data.table][data.table::data.table()]
        #'
        #' @param fields A character vector indicating the fields to put into
        #'        the `data.table`. If `NULL`, all fields in the query result
        #'        will be used. Default: `NULL`.
        #'
        #' @param formatted Whether to use formatted values for special fields,
        #'        including `url` and `size`. Default: `FALSE`.
        #'
        #' @return A [data.table][data.table::data.table()].
        #'
        to_dt = function(fields = NULL, formatted = FALSE) {
            checkmate::assert_flag(formatted)
            super$to_dt(fields, if (formatted) c("url", "size"))
        },

        #' @description
        #' Print a summary of the current dataset
        #'
        #' @param n An integer indicating how many items to print. If `NULL`,
        #'        all items will be printed. Default: `10L`.
        #'
        #' @return The `EsgfQueryResultAggregation` object itself, invisibly.
        print = function(n = 10L) {
            private$print_header("Aggregation")
            private$print_summary("Aggregation")
            private$print_parameters()
            cli::cat_line()
            private$print_contents("Aggregation", n)
            invisible(self)
        }
    ),

    active = list(
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

        #' @field url_download A character vector of the download URLs of the
        #'        files.
        url_download = function() {
            private$get_url("HTTPServer")
        },

        #' @field fields A character vector indicating all fields in the results.
        fields = function() {
            # nocov start
            if (!self$count()) return(NULL)
            # nocov end
            sort(c("url_opendap", "url_download", super$fields))
        }
    ),

    private = list(
        required_fields = sort(unique(c(
            EsgfQueryResult$private_fields$required_fields,
            "dataset_id", "title", "data_node"
        )))
    )
)

new_query_result <- function(generator, host, params, result, ..., .env = parent.frame()) {
    if (generator$is_locked()) {
        generator$unlock()
        on.exit(generator$lock(), add = TRUE)
    }

    fld_exist <- names(generator$active)
    # also check super class
    super <- generator$inherit
    while (!is.null(super <- eval(super, .env))) {
        fld_exist <- union(fld_exist, names(super$active))
        super <- super$inherit
    }

    fld_all <- names(result$response$docs)
    fld_miss <- setdiff(fld_all, fld_exist)
    if (length(fld_miss)) {
        for (field in fld_miss) {
            eval(substitute(
                generator$set(
                    "active", field,
                    function() {
                        private$get_field(field)
                    }
                ),
                list(field = field)
            ))
        }

        on.exit({
            for (field in fld_miss) {
                generator$active[[field]] <- NULL
            }
        }, add = TRUE)
    }
    generator$new(host, params, result, ...)
}
}
