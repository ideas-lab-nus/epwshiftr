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
        #' @return An `EsgResult` object.
        #'
        initialize = function(index_node, params, response) {
            private$index_node <- index_node
            private$parameter <- query_param_clone(params)
            private$response <- response
            private$register_dynamic_fields()
            self
        },
        # }}}

        # to_data_table {{{
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
        to_data_table = function(fields = NULL, formatted = NULL) {
            docs <- private$get_docs()

            checkmate::assert_subset(fields, names(docs))
            checkmate::assert_character(formatted, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

            if (length(fields)) {
                docs <- docs[match(fields, names(docs))]
            }
            if (!is.null(formatted)) {
                for (field in formatted) {
                    # nocov start
                    if (is.null(docs[[field]])) {
                        next
                    }
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
            query_save(
                index_node = private$index_node,
                parameter = private$parameter,
                response = private$response,
                file = file,
                pretty = pretty,
                schema = SCHEMA_RESULT_DATASET
            )
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
            q <- query_load(file, SCHEMA_RESULT_DATASET)
            private$validate_loaded_result(q)

            private$index_node <- q$index_node
            private$parameter <- q$parameter
            private$response <- q$response
            private$register_dynamic_fields()

            self
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
        #' @field fields A character vector indicating all fields in the results.
        fields = function() {
            # nocov start
            if (!self$count()) {
                return(NULL)
            }
            # nocov end
            sort(names(private$get_docs()))
        }
        # }}}
    ),

    private = list(
        index_node = NULL,
        parameter = NULL,
        response = NULL,
        dynamic_fields = character(),
        result_type = NULL,

        required_fields = c("id", "size", "url"),
        query_fields = c("dataset_id", "fields", "latest", "distrib", "limit", "type", "format"),
        static_fields = c("id", "url", "size", "fields", "filename", "url_opendap", "url_download"),

        get_docs = function() {
            docs <- private$response$response$docs
            if (is.null(docs)) {
                data.frame()
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

        # validate_loaded_result {{{
        validate_loaded_result = function(q) {
            expected <- private$result_type
            if (is.null(expected)) {
                return(invisible(q))
            }

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
                urls,
                function(dt_url) {
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
                        warning(sprintf("Multiple %s URL found. Only the first is returned.", name))
                        res <- res[[1L]]
                    }
                    # nocov end

                    res
                },
                character(1L)
            )
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
            cli::cli_bullets(c("*" = "Index Node: {private$index_node}"))
            cli::cli_bullets(c("*" = "Collected at: {ts}"))
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

            print_trunc(self$id, n)
        }
        # }}}
    )
)
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
        #' @param fields A character vector indicating the fields to put into
        #'        the `data.table`. If `NULL`, all fields in the query result
        #'        will be used. Default: `NULL`.
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
        #' @param limit A positive integer indicating the number of records to
        #'        fetch per query. If `NULL`, the allowed maximum limit number
        #'        `r this$data_max_limit` is used. Default: `100L`.
        #'
        #' @param type A string indicating the query type. Should be one of
        #'        `File` or `Aggregation`. Default: `"File"`.
        #'
        #' @param ... Other parameters to set. Currently, there are 4 parameters
        #'        supported, including `replica`, `distrib`, `latest`, `shards`.
        #'        If omitted, these controls are inherited from the dataset
        #'        query when available, with `distrib = TRUE` and
        #'        `latest = TRUE` as fallbacks.
        #'        For details on possible parameters, please see [esg_query()].
        #'
        #' @return
        #'
        #' - If `type="File"`, an [EsgResultFile] object
        #' - If `type="Aggregation"`, an [EsgResultAggregation] object
        #'
        collect = function(which = NULL, fields = NULL, all = FALSE, limit = 100L, type = "File", ...) {
            if (!is.null(which)) {
                if (!self$count()) {
                    which <- NULL
                } else {
                    checkmate::assert(
                        checkmate::check_subset(which, self$id),
                        checkmate::check_integerish(
                            which,
                            lower = 1L,
                            upper = self$count(),
                            any.missing = FALSE,
                            unique = TRUE
                        )
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
                    private$index_node,
                    params,
                    required_fields = req_fld,
                    all = all,
                    limit = limit,
                    constraints = FALSE
                )
            }

            # replace docs in the last response
            result$response$response$docs <- result$docs

            # create new results
            if (type == "File") {
                new_query_result(
                    EsgResultFile,
                    private$index_node,
                    params,
                    result$response
                )
            } else if (type == "Aggregation") {
                new_query_result(
                    EsgResultAggregation,
                    private$index_node,
                    params,
                    result$response
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
            checkmate::assert_choice(type, c("File", "Aggregation"))

            checkmate::assert_integerish(limit, lower = 1L, upper = this$data_max_limit, len = 1L, null.ok = TRUE)
            if (is.null(limit)) {
                limit <- this$data_max_limit
            }

            overrides <- eval(substitute(alist(...)))
            if (length(overrides)) {
                # other parameters
                names_supp <- c("replica", "distrib", "latest", "shards")
                overrides <- eval_with_bang(...)

                # stop if unsupported parameter found
                names_params <- names(overrides)
                if (any(invld <- !names_params %in% names_supp)) {
                    stop(sprintf(
                        "Unsupported query parameter found: [%s]. Should be subset of [%s].",
                        names_params[invld],
                        paste(names_supp, collapse = ", ")
                    ))
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

            # create a new query to validate params
            query <- esg_query(private$index_node)
            query$distrib(controls$distrib)
            dataset_id <- if (is.null(index)) self$id else self$id[index]
            if (!length(dataset_id)) {
                dataset_id <- NULL
            }

            params <- list(
                dataset_id = dataset_id,

                # use query object to validate params
                fields = query$fields(fields)$fields(),
                shards = query$shards(controls$shards)$shards(),
                replica = query$replica(controls$replica)$replica(),
                latest = query$latest(controls$latest)$latest(),
                distrib = query$distrib(),

                limit = limit,
                type = type,
                format = FORMAT_JSON
            )

            list(params = query_param_as_store(params), limit = limit)
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
        #' @param fields A character vector indicating the fields to put into
        #'        the `data.table`. If `NULL`, all fields in the query result
        #'        will be used. Default: `NULL`.
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
        #' @param index Integer index of the file to open. Default: `1L`.
        #' @param fallback What to do if OPeNDAP is unavailable. One of:
        #'   - `"ask"`: Interactively ask the user (default). In a
        #'     non-interactive session this raises an error.
        #'   - `"auto"`: Automatically download the file via HTTPServer.
        #'   - `"error"`: Raise an error.
        #'
        #' @return An `EsgDataset` object with the connection already opened.
        open_dataset = function(index = 1L, fallback = c("ask", "auto", "error")) {
            checkmate::assert_int(index, lower = 1L, upper = self$count())
            fallback <- match.arg(fallback)

            url <- self$url_opendap[index]
            opendap_error <- NULL
            ds <- NULL

            if (!is.na(url)) {
                # Try OPeNDAP first
                ds <- tryCatch(
                    {
                        d <- EsgDataset$new(url)
                        d$open()
                        d
                    },
                    error = function(e) {
                        opendap_error <<- e
                        NULL
                    }
                )
            }

            if (!is.null(ds)) {
                return(ds)
            }

            # OPeNDAP failed or is unavailable: handle fallback
            if (is.na(url)) {
                cli::cli_alert_warning("No OPeNDAP URL is available for file index {index}.")
            } else {
                cli::cli_alert_warning("OPeNDAP connection failed for: {.url {url}}")
            }

            if (fallback == "error") {
                cli::cli_abort("OPeNDAP is not available for this file.", parent = opendap_error)
            }

            if (fallback == "ask") {
                if (!interactive()) {
                    cli::cli_abort("Cannot ask for fallback in a non-interactive session. Use fallback = 'auto' to download via HTTP.")
                } else {
                    answer <- utils::menu(
                        choices = c("Download via HTTP", "Cancel"),
                        title = "OPeNDAP is not available. What would you like to do?"
                    )
                    if (answer != 1L) {
                        cli::cli_abort("Operation cancelled by user.")
                    }
                }
            }

            # Download as fallback
            file_url <- self$url_download[index]
            if (is.na(file_url)) {
                cli::cli_abort("No HTTPServer download URL is available for file index {index}.")
            }

            cli::cli_alert_info("Downloading file via HTTP as fallback...")
            dl <- FileDownloader$new()

            # Get file info
            dt <- self$to_data_table()
            checksum <- if ("checksum" %in% names(dt)) dt$checksum[index] else NULL
            checksum_type <- if ("checksum_type" %in% names(dt)) tolower(dt$checksum_type[index]) else NULL

            local_path <- dl$download(
                url = file_url,
                checksum = checksum,
                checksum_type = checksum_type
            )

            ds <- EsgDataset$new(local_path)
            ds$open()
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
        #' @field fields A character vector indicating all fields in the results.
        fields = function() {
            # nocov start
            if (!self$count()) {
                return(NULL)
            }
            # nocov end
            sort(c("filename", "url_opendap", "url_download", super$fields))
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
            "tracking_id",
            "title",
            "data_node"
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
        #' @param fields A character vector indicating the fields to put into
        #'        the `data.table`. If `NULL`, all fields in the query result
        #'        will be used. Default: `NULL`.
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
        #' @return An `EsgDataset` object with the connection already opened.
        open_dataset = function(aggregate = TRUE, fallback = c("ask", "auto", "error")) {
            checkmate::assert_flag(aggregate)
            fallback <- match.arg(fallback)

            urls <- self$url_opendap
            indices <- seq_along(urls)

            if (!aggregate) {
                urls <- urls[1L]
                indices <- 1L
            }

            opendap_error <- NULL
            ds <- NULL
            if (length(urls) && all(!is.na(urls))) {
                # Try OPeNDAP
                ds <- tryCatch(
                    {
                        d <- EsgDataset$new(urls)
                        d$open()
                        d
                    },
                    error = function(e) {
                        opendap_error <<- e
                        NULL
                    }
                )
            }

            if (!is.null(ds)) {
                return(ds)
            }

            # OPeNDAP failed or is unavailable
            if (!length(urls) || all(is.na(urls))) {
                cli::cli_alert_warning("No OPeNDAP URLs are available for these aggregation records.")
            } else if (any(is.na(urls))) {
                cli::cli_alert_warning("OPeNDAP URLs are missing for one or more aggregation records.")
            } else {
                cli::cli_alert_warning("OPeNDAP connection failed.")
            }

            if (fallback == "error") {
                cli::cli_abort("OPeNDAP is not available for these files.", parent = opendap_error)
            }

            if (fallback == "ask") {
                if (!interactive()) {
                    cli::cli_abort("Cannot ask for fallback in a non-interactive session. Use fallback = 'auto' to download via HTTP.")
                } else {
                    answer <- utils::menu(
                        choices = c(
                            sprintf("Download %d file(s) via HTTP", length(indices)),
                            "Cancel"
                        ),
                        title = "OPeNDAP is not available. What would you like to do?"
                    )
                    if (answer != 1L) {
                        cli::cli_abort("Operation cancelled by user.")
                    }
                }
            }

            # Download as fallback
            download_urls <- self$url_download[indices]
            if (any(is.na(download_urls))) {
                cli::cli_abort("HTTPServer download URLs are missing for one or more aggregation records.")
            }

            cli::cli_alert_info("Downloading {length(indices)} file(s) via HTTP as fallback...")
            dl <- FileDownloader$new()
            dt <- self$to_data_table()

            local_paths <- vapply(
                seq_along(indices),
                function(j) {
                    i <- indices[[j]]
                    file_url <- download_urls[[j]]
                    checksum <- if ("checksum" %in% names(dt)) dt$checksum[i] else NULL
                    checksum_type <- if ("checksum_type" %in% names(dt)) tolower(dt$checksum_type[i]) else NULL

                    dl$download(
                        url = file_url,
                        checksum = checksum,
                        checksum_type = checksum_type
                    )
                },
                character(1L)
            )

            ds <- EsgDataset$new(local_paths)
            ds$open()
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
        #' @field fields A character vector indicating all fields in the results.
        fields = function() {
            # nocov start
            if (!self$count()) {
                return(NULL)
            }
            # nocov end
            sort(c("url_opendap", "url_download", super$fields))
        }
        # }}}
    ),

    private = list(
        result_type = "Aggregation",

        required_fields = sort(unique(c(
            EsgResult$private_fields$required_fields,
            "dataset_id",
            "title",
            "data_node"
        )))
    )
)
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

    list(response = response, docs = response$response$docs)
}
# }}}

# new_query_result {{{
new_query_result <- function(generator, index_node = NULL, params = NULL, result = NULL, ...) {
    generator$new(index_node, params, result, ...)
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
