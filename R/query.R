#' @include query-result.R
NULL

# TODO: update doc about query type
# TODO: be able to import query parameters?
# read_json_response {{{
read_json_response <- function(url, strict = TRUE, cache = getOption("epwshiftr.cache", TRUE), ...) {
    checkmate::assert(checkmate::check_flag(cache), checkmate::check_count(cache, positive = TRUE))
    if (cache && url %in% names(this$cache$query)) return(this$cache$query[[url]])

    timestamp <- now()
    res <- tryCatch(jsonlite::fromJSON(url, bigint_as_char = TRUE, ...), warning = function(w) w, error = function(e) e)

    # nocov start
    if (inherits(res, "warning") || inherits(res, "error")) {
        cond_fun <- if (strict) stop else warning
        cond_fun(
            "No matched data.",
            "Please check network connection and the availability of ESGF index/data node. Details: \n",
            conditionMessage(res)
        )
        res <- NULL
    # nocov end
    } else if (res$response$numFound == 0L) {
        vb(cli::cli_alert_warning(
            "No matched data. Please examine your query and the actual response."
        ))
    }

    res$timestamp <- timestamp

    # cache results
    if (cache) {
        if (is.numeric(cache) && !url %in% names(this$cache$query) && length(this$cache$query) >= cache) {
            this$cache$query <- this$cache$query[seq_len(length(this$cache$query))]
        }
        this$cache$query[[url]] <- res
    }

    res
}
# }}}

# new_query_param {{{
new_query_param <- function(name, value) {
    checkmate::assert_string(name)

    if (!is.list(value)) value <- list(value = value, negate = FALSE)
    if (is.null(value$value)) return(NULL)

    checkmate::assert_list(value,
        types = c("logical", "numeric", "character"),
        any.missing = TRUE, names = "unique"
    )
    checkmate::assert_names(names(value), must.include = c("value", "negate"))

    # do not allow NAs
    for (val in value) checkmate::assert_atomic(val, any.missing = FALSE)

    value$name <- name
    structure(value, class = c("EsgfQueryParam", "list"))
}
# }}}

# is_query_param {{{
is_query_param <- function(x) inherits(x, "EsgfQueryParam")
# }}}

#' @export
# print.EsgfQueryParam {{{
print.EsgfQueryParam <- function(x, encode = FALSE, space = TRUE, ...) {
    cat(format.EsgfQueryParam(x, encode = encode, space = space), sep = "\n")
    invisible(x)
}
# }}}

# format.EsgfQueryParam {{{
format.EsgfQueryParam <- function(x, encode = TRUE, space = FALSE, ...) {
    if (is.logical(x$value)) {
        res <- tolower(if (x$negate) !x$value else x$value)
    } else if (is.numeric(x$value)) {
        res <- as.character(x$value)
    } else if (encode) {
        res <- query_param_encode(x$value)
    } else {
        res <- x$value
    }

    spc <- if (space) " " else ""

    if (x$negate && !is.logical(x$value)) {
        paste0(x$name, spc, "!=", spc, res, collapse = paste0(spc, "&", spc))
    } else {
        paste0(x$name, spc, "=", spc, paste0(res, collapse = paste0(",", spc)))
    }
}
# }}}

# query_esgf {{{

#' Query CMIP6 data using ESGF search RESTful API
#'
#' @description
#' The Earth System Grid Federation (ESGF) is an international collaboration for
#' the software that powers most global climate change research, notably
#' assessments by the Intergovernmental Panel on Climate Change (IPCC).
#'
#' The ESGF search service exposes RESTful APIs that can be used by clients to
#' query the contents of the underlying search index, and return results
#' matching the given constraints. The documentation of the APIs can be found
#' using this
#' [link](https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html).
#'
#' `EsgfQuery` is the workhorse for dealing with ESGF search services.
#'
#' # `EsgfQuery` object
#'
#' `query_esgf()` returns an `EsgfQuery` object, which is an [R6][R6::R6Class]
#' object with quite a few methods that can be classified into 3 categories:
#'
#' - **Value listing**: methods to list all possible values of facets, shards,
#'   etc.
#'
#' - **Parameter getter & setter**: methods to get the query parameter values or
#'   set them before sending the actual query to the ESGF search services.
#'
#' - **Query responses**: methods to collect results for the query response.
#'
#' ## Value listing
#'
#' When creating an `EsgfQuery` object, a
#' [facet listing query](https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html#facet-listings)
#' is sent to the index node to get all available facets and shards for the
#' default project (CMIP6).
#' `EsgfQuery` object provides three value-listing methods to extract data from
#' the response of the facet listing query:
#'
#' - \href{#method-EsgfQuery-list_all_facets}{\code{EsgfQuery$list_all_facets()}}:
#'   List all available facet names.
#'
#' - \href{#method-EsgfQuery-list_all_fields}{\code{EsgfQuery$list_all_fields()}}:
#'   List all available field names.
#'
#' - \href{#method-EsgfQuery-list_all_shards}{\code{EsgfQuery$list_all_shards()}}:
#'   List all available shards.
#'
#' - \href{#method-EsgfQuery-list_all_values}{\code{EsgfQuery$list_all_values()}}:
#'   List all available values of a specific facet.
#'
#' ## Parameter getter & setter
#'
#' The ESGF search services support a lot of parameters. The `EsgfQuery`
#' contains dedicated methods to set values for most of them, including:
#'
#' - Most common keywords:
#'   \href{#method-EsgfQuery-facets}{\code{facets}},
#'   \href{#method-EsgfQuery-offset}{\code{offset}},
#'   \href{#method-EsgfQuery-limit}{\code{limit}},
#'   \href{#method-EsgfQuery-fields}{\code{fields}},
#'   \href{#method-EsgfQuery-type}{\code{type}},
#'   \href{#method-EsgfQuery-replica}{\code{replica}},
#'   \href{#method-EsgfQuery-latest}{\code{latest}},
#'   \href{#method-EsgfQuery-distrib}{\code{distrib}}
#'   and
#'   \href{#method-EsgfQuery-shards}{\code{shards}}.
#'
#' - Most common facets:
#'   \href{#method-EsgfQuery-project}{\code{project}},
#'   \href{#method-EsgfQuery-activity_id}{\code{activity_id}},
#'   \href{#method-EsgfQuery-experiment_id}{\code{experiment_id}},
#'   \href{#method-EsgfQuery-source_id}{\code{source_id}},
#'   \href{#method-EsgfQuery-variable_id}{\code{variable_id}},
#'   \href{#method-EsgfQuery-frequency}{\code{frequency}},
#'   \href{#method-EsgfQuery-variant_label}{\code{variant_label}},
#'   \href{#method-EsgfQuery-nominal_resolution}{\code{nominal_resolution}}
#'   and
#'   \href{#method-EsgfQuery-data_node}{\code{data_node}}.
#'
#' All methods act in a similar way:
#'
#' - If input is given, the corresponding parameter is set and the updated
#'   `EsgfQuery` object is returned.
#'
#'   * This makes it possible to chain different parameter setters, e.g.
#'     `EsgfQuery$project("CMIP6")$frequency("day")$limit(1)` sets the parameter
#'     `project`, `frequency` and `limit` sequentially.
#'
#'   * For parameters that want character inputs, you can put a preceding `!` to
#'     negate the constraints, e.g. `EsgfQuery$project(!"CMIP6")` searches for
#'     all projects except for `CMIP6`.
#'
#' - If no input is given, the current parameter value is returned. For example,
#'   directly calling `EsgfQuery$project()` returns the current value of the
#'   `project` parameter. The returned value can be two types:
#'
#'   * `NULL`, i.e. there is no constraint on the corresponding parameter
#'
#'   * An `EsgfQueryParam` object which is essentially a list of three elements:
#'
#'     + `value`: The input values
#'     + `negate`: Whether there is a preceding `!` in the input
#'     + `name`: The parameter name
#'
#' Despite methods for specific keywords and facets, you can specify arbitrary
#' query parameters using
#' \href{#method-EsgfQuery-params}{\code{EsgfQuery$params()}} method. For
#' details on the usage, please see the
#' \href{#method-EsgfQuery-params}{documentation}.
#'
#' ## Query responses
#'
#' The query is not sent unless related methods are called:
#'
#' - \href{#method-EsgfQuery-count}{\code{EsgfQuery$count()}}: Count the total
#'   number of records that match the query.
#'
#'   * You can return only the total number of matched record by calling
#'     `EsgfQuery$count(facets = FALSE)`
#'
#'   * You can also count the matched records for specified facets, e.g.
#'     `EsgfQuery$count(facets = c("source_id", "activity_id"))`
#'
#' - \href{#method-EsgfQuery-collect}{\code{EsgfQuery$collect()}}: Collect the
#'   query results and format it into an [EsgfQueryResultDataset] object.
#'
#' ## Other helpers
#'
#' `EsgfQuery` object also provide several other helper functions:
#'
#' - \href{#method-EsgfQuery-build_listing}{\code{EsgfQuery$build_listing()}}:
#'   By default, `EsgfQuery$build_listing()` is not called when initialize a new
#'   `EsgfQuery` object. Basically, `EsgfQuery$build_listing()` sends a
#'   [facet listing query](https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html#facet-listings)
#'   to the index node and stores the response internally. The response contains
#'   all available facets and shards and is used as a source for validating user
#'   input for parameter setters.
#'
#' - \href{#method-EsgfQuery-url}{\code{EsgfQuery$url()}}: Returns the actual
#'   query URL or the wget script URL which can be used to download all files
#'   matching the given constraints..
#'
#' - \href{#method-EsgfQuery-print}{\code{EsgfQuery$print()}}: Print a summary
#'   of the current `EsgfQuery` object including the host URL, the built time of
#'   facet listing and all query parameters.
#'
#' @author Hongyuan Jia
#'
#' @name EsgfQuery
#'
#' @param host The URL to the ESGF Search API service. This should be the URL of
#'        the ESGF search service excluding the final endpoint name. Usually
#'        this is `http://<hostname>/esg-search`. Default is set to the
#'        [LLNL (Lawrence Livermore National Laboratory) Index Node](http://esgf-node.llnl.gov),
#'        which is `"https://esgf-node.llnl.gov/esg-search"`. Current possible
#'        values could be:
#'
#' - USA, PCMDI/LLNL (California):
#'   `https://esgf-node.llnl.gov/esg-search`. The default value.
#' - IPSL (Institut Pierre-Simon Laplace), France:
#'   `https://esgf-node.ipsl.upmc.fr/esg-search`
#' - CEDA (Centre for Environment Data Analysis), UK:
#'   `https://esgf.ceda.ac.uk/esg-search`
#' - DKRZ (Deutsches Klimarechenzentrum), Germany:
#'   `https://esgf-data.dkrz.de/esg-search`
#'
#' @param listing Whether to send the facet listing query used to validate
#'        inputs. Default: `FALSE`. See `r sQuote("Value listing")` in Details.
#'
#' @export
query_esgf <- function(host = "https://esgf-node.llnl.gov/esg-search", listing = FALSE) {
    EsgfQuery$new(host = host, listing = listing)
}
# }}}

#' @name EsgfQuery
#' @export
# EsgfQuery {{{
EsgfQuery <- R6::R6Class("EsgfQuery",
    lock_class = TRUE,
    public = list(
        # initialize {{{
        #' @description
        #' Create a new EsgfQuery object
        #'
        #' @param host The URL to the ESGF Search API service. This should be
        #'        the URL of the ESGF search service excluding the final
        #'        endpoint name. Usually this is `http://<hostname>/esg-search`.
        #'        Default is to ses the [LLNL (Lawrence Livermore National
        #'        Laboratory)](http://esgf-node.llnl.gov) Index Node, which is
        #'        `"https://esgf-node.llnl.gov/esg-search"`.
        #'        Current possible values could be:
        #'
        #' - USA, PCMDI/LLNL (California):
        #'   `https://esgf-node.llnl.gov/esg-search`. The default value.
        #' - IPSL (Institut Pierre-Simon Laplace), France:
        #'   `https://esgf-node.ipsl.upmc.fr/esg-search`
        #' - CEDA (Centre for Environment Data Analysis), UK:
        #'   `https://esgf.ceda.ac.uk/esg-search`
        #' - DKRZ (Deutsches Klimarechenzentrum), Germany:
        #'   `https://esgf-data.dkrz.de/esg-search`
        #'
        #' @param listing Whether to send the facet listing query.
        #'        A [facet listing query](https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html#facet-listings)
        #'        can be sent to the index node to get all available facets and
        #'        shards. When available, this information will be automatically
        #'        used to validate inputs for `activity_id`, `scource_id` facets
        #'        and etc. NOTE: Not all index nodes support facet listing
        #'        query. You may encounter error if your input host does not
        #'        support it. Default: `FALSE`.
        #'
        #' @return An `EsgfQuery` object.
        #'
        #' @examples
        #' \dontrun{
        #' q <- EsgfQuery$new(host = "https://esgf-node.llnl.gov/esg-search")
        #' q
        #' }
        initialize = function(host = "https://esgf-node.llnl.gov/esg-search", listing = FALSE) {
            checkmate::assert_string(host)
            checkmate::assert_flag(listing)

            # in case of a encoded URL
            private$url_host <- utils::URLdecode(host)
            if (grepl("/$", private$url_host)) {
                private$url_host <- substring(private$url_host, 1L, nchar(private$url_host) - 1L)
            }

            if (listing) self$build_listing()

            # init parameter values
            params <- setdiff(names(private$parameter), "others")
            for (name in params) {
                if (!is.null(.subset2(private$parameter, name))) {
                    private$parameter[[name]] <- new_query_param(
                        name = name, value = .subset2(private$parameter, name)
                    )
                }
            }

            self
        },
        # }}}

        # build_listing {{{
        #' @description
        #' Build facet listing used for input validation
        #'
        #' Facet listing is data that is fetched using a
        #' [facet listing query](https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html#facet-listings)
        #' to the index node. It contains all available facets and shards that
        #' can be used as parameter values within a specific project.
        #'
        #' @param force By default, every facet listing query is cached and
        #'        reused when possible. If `TRUE`, the previous cache is
        #'        abandoned and a new query is re-sent and cached. Default:
        #'        `FALSE`.
        #'
        #' @return The modified `EsgfQuery` object.
        #'
        #' @examples
        #' \dontrun{
        #' q$build_listing()
        #' }
        build_listing = function(force = FALSE) {
            checkmate::assert_flag(force)

            if (!force && !is.null(this$cache$facet[[private$url_host]])) {
                vb(cli::cli_progress_step(paste(
                    "Loaded facet listing cache for host {.var {private$url_host}}",
                    "built at {private$facet_listing$timestamp}."
                )))

                private$facet_listing <- this$cache$facet[[private$url_host]]
                return(self)
            }

            vb(cli::cli_progress_step(
                "Building facet listing for host {.var {private$url_host}}...",
                paste(
                    "Built facet listing for host {.var {private$url_host}}",
                    "built at {private$facet_listing$timestamp}."
                )
            ))

            private$facet_listing <- query_build_facet_listing(
                private$url_host, private$parameter$project
            )
            self
        },
        # }}}

        # has_listing {{{
        #' @description
        #' Check whether facet listing query has been sent
        #'
        #' @return `TRUE` or `FALSE`.
        #'
        #' @examples
        #' \dontrun{
        #' q$has_listing()
        #' }
        has_listing = function() {
            as.logical(private$has_facet_listing())
        },
        # }}}

        # list_all_facets {{{
        #' @description
        #' List all available facet names
        #'
        #' @return A character vector or `NULL` if no facet listing is found.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_all_facets()
        #' }
        list_all_facets = function() {
            if (!private$has_facet_listing()) return(NULL)

            unlst(private$facet_listing$responseHeader$params$facet.field)
        },
        # }}}

        # list_all_fields {{{
        #' @description
        #' List all available field names
        #'
        #' @return A character vector or `NULL` if no facet listing is found.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_all_fields()
        #' }
        list_all_fields = function() {
            if (!private$has_facet_listing()) return(NULL)

            unlst(private$facet_listing$responseHeader$params$fields)
        },
        # }}}

        # list_all_shards {{{
        #' @description
        #' List all available shards
        #'
        #' @return A character vector or `NULL` if no facet listing is found.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_all_shards()
        #' }
        list_all_shards = function() {
            if (!private$has_facet_listing()) return(NULL)

            shards <- private$facet_listing$responseHeader$params$shards
            # nocov start
            if (!length(shards)) return(NULL)
            # nocov end

            shards <- strsplit(shards, ",", fixed = TRUE)[[1L]]
            # fix the host if it refers to the local server

            shards_parts <- regmatches(shards, regexec("(.*):(\\d*)/solr(.*)", shards))

            # nocov start
            if (length(invld <- shards[lengths(shards_parts) != 4L])) {
                warning(sprintf(
                    "Unrecognized Shard specification found: %s.",
                    paste0("'", invld, "'", collapse = ", ")
                ))
            }
            # nocov end

            vapply(shards_parts, FUN.VALUE = "", function(shard) {
                shard <- shard[-1L]
                # replace localhost
                if (tolower(shard[[1L]]) %in% c("localhost", "0.0.0.0", "127.0.0.1")) {
                    shard[1L] <- strsplit(private$url_host, "/", fixed = TRUE)[[1L]][[3L]]
                }
                # exclude suffix
                sprintf("%s:%s/solr%s", shard[[1L]], shard[[2L]], shard[[3L]])
            })
        },
        # }}}

        # list_all_values {{{
        #' @description
        #' List all available values of a specific facet
        #'
        #' @param facet A single string giving the facet name.
        #'
        #' @return A named character vector or `NULL` if no facet listing is found.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_all_values()
        #' }
        list_all_values = function(facet) {
            if (!private$has_facet_listing()) return(NULL)

            checkmate::assert_choice(facet, self$list_all_facets())
            names(private$facet_listing_count(facet))
        },
        # }}}

        # project {{{
        #' @description
        #' Get or set the `project` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("project", "character vector", c("CMIP5", "CMIP6"), "CMIP6")`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$project()
        #'
        #' # set the parameter
        #' q$project("CMIP6")
        #'
        #' # negate the project constraints
        #' q$project(!"CMIP6")
        #'
        #' # remove the parameter
        #' q$project(NULL)
        #' }
        project = function(value = "CMIP6") {
            if (missing(value)) return(private$parameter$project)
            private$parameter$project <- private$new_facet_param("project", value)
            self
        },
        # }}}

        # activity_id {{{
        #' @description
        #' Get or set the `activity_id` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("activity_id", "character vector", c("C4MIP", "GeoMIP"), NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$activity_id()
        #'
        #' # set the parameter
        #' q$activity_id("ScenarioMIP")
        #'
        #' # negate the constraints
        #' q$activity_id(!c("CFMIP", "ScenarioMIP"))
        #'
        #' # remove the parameter
        #' q$activity_id(NULL)
        #' }
        activity_id = function(value) {
            if (missing(value)) return(private$parameter$activity_id)
            private$parameter$activity_id <- private$new_facet_param("activity_id", value)
            self
        },
        # }}}

        # experiment_id {{{
        #' @description
        #' Get or set the `experiment_id` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("experiment_id", "character vector", c("ssp126", "ssp245"), NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$experiment_id()
        #'
        #' # set the parameter
        #' q$experiment_id(c("ssp126", "ssp585"))
        #'
        #' # negate the constraints
        #' q$experiment_id(!c("ssp126", "ssp585"))
        #'
        #' # remove the parameter
        #' q$experiment_id(NULL)
        #' }
        experiment_id = function(value) {
            if (missing(value)) return(private$parameter$experiment_id)
            private$parameter$experiment_id <- private$new_facet_param("experiment_id", value)
            self
        },
        # }}}

        # source_id {{{
        #' @description
        #' Get or set the `source_id` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("source_id", "character vector", c("CESM2", "CESM2-FV2"), NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$source_id()
        #'
        #' # set the parameter
        #' q$source_id(c("BCC-CSM2-MR", "CESM2"))
        #'
        #' # negate the constraints
        #' q$source_id(!c("BCC-CSM2-MR", "CESM2"))
        #'
        #' # remove the parameter
        #' q$source_id(NULL)
        #' }
        source_id = function(value) {
            if (missing(value)) return(private$parameter$source_id)
            private$parameter$source_id <- private$new_facet_param("source_id", value)
            self
        },
        # }}}

        # variable_id {{{
        #' @description
        #' Get or set the `variable_id` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("variable_id", "character vector", c("tas", "pr"), NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$variable_id()
        #'
        #' # set the parameter
        #' q$variable_id(c("tas", "pr"))
        #'
        #' # negate the constraints
        #' q$variable_id(!c("tas", "pr"))
        #'
        #' # remove the parameter
        #' q$variable_id(NULL)
        #' }
        variable_id = function(value) {
            if (missing(value)) return(private$parameter$variable_id)
            private$parameter$variable_id <- private$new_facet_param("variable_id", value)
            self
        },
        # }}}

        # frequency {{{
        #' @description
        #' Get or set the `frequency` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("frequency", "character vector", c("day", "mon"), NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$frequency()
        #'
        #' # set the parameter
        #' q$frequency(c("1hr", "day"))
        #'
        #' # negate the constraints
        #' q$frequency(!c("1hr", "day"))
        #'
        #' # remove the parameter
        #' q$frequency(NULL)
        #' }
        frequency = function(value) {
            if (missing(value)) return(private$parameter$frequency)
            private$parameter$frequency <- private$new_facet_param("frequency", value)
            self
        },
        # }}}

        # variant_label {{{
        #' @description
        #' Get or set the `variant_label` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("variant_label", "character vector", c("r1i1p1f1", "r2i1p1f1"), NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$variant_label()
        #'
        #' # set the parameter
        #' q$variant_label(c("r1i1p1f1", "r1i2p1f1"))
        #'
        #' # negate the constraints
        #' q$variant_label(!c("r1i1p1f1", "r1i2p1f1"))
        #'
        #' # remove the parameter
        #' q$variant_label(NULL)
        #' }
        variant_label = function(value) {
            if (missing(value)) return(private$parameter$variant_label)
            private$parameter$variant_label <- private$new_facet_param("variant_label", value)
            self
        },
        # }}}

        # nominal_resolution {{{
        #' @description
        #' Get or set the `nominal_resolution` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("nominal_resolution", "character vector", c("50 km", "1x1 degree"), NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$nominal_resolution()
        #'
        #' # set the parameter
        #' q$nominal_resolution(c("100 km", "1x1 degree"))
        #'
        #' # negate the constraints
        #' q$nominal_resolution(!c("100 km", "1x1 degree"))
        #'
        #' # remove the parameter
        #' q$nominal_resolution(NULL)
        #' }
        nominal_resolution = function(value) {
            if (missing(value)) return(private$parameter$nominal_resolution)
            param <- private$new_facet_param("nominal_resolution", value)

            if (!is.null(param)) {
                # handle nominal resolution specially
                # there are some GCMs that mistakenly set '100 km' to '100km'
                if ("100 km" %in% param$value && !"100km" %in% param$value) {
                    param$value <- c(param$value, "100km")
                }

                # ESGF uses '+' for spaces in nominal resolution
                param$value <- gsub(" ", "+", param$value, fixed = TRUE)

                # explictly mark it as realdy encoded, otherwise '+' will not be
                # perserved
                attr(param$value, "encoded") <- TRUE
            }

            private$parameter$nominal_resolution <- param
            self
        },
        # }}}

        # data_node {{{
        #' @description
        #' Get or set the `data_node` parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("data_node", "character vector", c("cmip.bcc.cma.cn", "esg.camscma.cn"), NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$data_node()
        #'
        #' # set the parameter
        #' q$data_node("esg.lasg.ac.cn")
        #'
        #' # negate the constraints
        #' q$data_node(!"esg.lasg.ac.cn")
        #'
        #' # remove the parameter
        #' q$data_node(NULL)
        #' }
        data_node = function(value) {
            if (missing(value)) return(private$parameter$data_node)
            private$parameter$data_node <- private$new_facet_param("data_node", value)
            self
        },
        # }}}

        # facets {{{
        #' @description
        #' Get or set the `facets` parameter for facet counting query.
        #'
        #' Note that `$facets()` only affects
        #' \href{#method-EsgfQuery-count}{\code{$count()}}
        #' method when sending a query of facet counting.
        #'
        #' @param value
        #' `r rd_query_method_param("facets", "character vector", default = NULL)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$facets()
        #'
        #' # set the facets
        #' q$facets(c("activity_id", "source_id"))
        #'
        #' # use all available facets
        #' q$facets("*")
        #' }
        facets = function(value) {
            if (missing(value)) return(private$parameter$facets)
            private$parameter$facets <- private$new_facet_param("facets", value, FALSE)
            self
        },
        # }}}

        # fields {{{
        #' @description
        #' Get or set the `fields` parameter.
        #'
        #' By default, all available metadata fields are returned for each
        #' query. `$fields()` can be used to limit the number of fields returned
        #' in the query response. However, the following fields are always
        #' included in the results:
        #' `r paste0("\\verb{", EsgfQueryResultDataset$private_fields$required_fields, "}", collapse = ", ")`.
        #'
        #' @param value
        #' `r rd_query_method_param("fields", "character vector", default = "*")`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$fields()
        #'
        #' # set the fields
        #' q$fields(c("activity_id", "source_id"))
        #'
        #' # use all available fields
        #' q$fields("*")
        #'
        #' # remove the parameter
        #' # act the same as above because the default `fields` in ESGF search
        #' # services is `*` if `fields` is not specified
        #' q$fields(NULL)
        #' }
        fields = function(value = "*") {
            if (missing(value)) return(private$parameter$fields)
            private$parameter$fields <- private$new_facet_param("fields", value, FALSE)
            self
        },
        # }}}

        # shards {{{
        #' @description
        #' Get or set the `shards` parameter.
        #'
        #' By default, a distributed query targets all ESGF Nodes. `$shards()`
        #' can be used to execute a distributed search that targets only one or
        #' more specific nodes.
        #'
        #' All available shards can be retrieved using
        #' \href{#method-EsgfQuery-list_all_shards}{\code{$list_all_shards()}}
        #' method.
        #'
        #' @param value
        #' `r rd_query_method_param("shards", "character vector")`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$shards()
        #'
        #' # set the parameter
        #' q$shards("localhost:8983/solr/datasets")
        #'
        #' # negate the constraints
        #' q$shards(!"localhost:8983/solr/datasets")
        #'
        #' # only applicable for distributed queries
        #' q$distrib(FALSE)$shards("localhost:8983/solr/datasets") # Error
        #'
        #' # remove the parameter
        #' q$shards(NULL)
        #' }
        shards = function(value) {
            if (missing(value)) return(private$parameter$shards)
            if (!private$parameter$distrib$value && !is.null(value)) {
                stop("'$distrib()' returns FALSE. Shard specification is only applicable for distributed queries.")
            }
            private$parameter$shards <- private$new_facet_param("shards", value, FALSE)
            self
        },
        # }}}

        # replica {{{
        #' @description
        #' Get or set the `replica` parameter.
        #'
        #' By default, a query returns all records (masters and replicas)
        #' matching the search criteria, i.e. `$replica(NULL)`.
        #' To return only master records, use `$replica(FALSE)`; to return only
        #' replicas, use `$replica(TRUE)`.
        #'
        #' @param value
        #' `r rd_query_method_param("replica", "flag", default = NULL, nullable = TRUE)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$replica()
        #'
        #' # set the parameter
        #' q$replica(TRUE)
        #'
        #' # remove the parameter
        #' q$replica(NULL)
        #' }
        replica = function(value) {
            if (missing(value)) return(private$parameter$replica)
            checkmate::assert_flag(value, null.ok = TRUE, .var.name = "replica")
            private$parameter$replica <- new_query_param("replica", value)
            self
        },
        # }}}

        # latest {{{
        #' @description
        #' Get or set the `latest` parameter.
        #'
        #' By default, a query to the ESGF search services returns only the very
        #' last, up-to-date version of the matching records, i.e.
        #' `$latest(TRUE)`. You can use `$latest(FALSE)` to return all versions.
        #'
        #' @param value
        #' `r rd_query_method_param("latest", "flag", default = TRUE, nullable = FALSE)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$latest()
        #'
        #' # set the parameter
        #' q$latest(TRUE)
        #' }
        latest = function(value = TRUE) {
            if (missing(value)) return(private$parameter$latest)
            checkmate::assert_flag(value, .var.name = "latest")
            private$parameter$latest <- new_query_param("latest", value)
            self
        },
        # }}}

        # limit {{{
        #' @description
        #' Get or set the `limit` parameter.
        #'
        #' `$limit()` can be used to limit the number of records to return.
        #' Note that the maximum number of records to return per query for ESGF
        #' search services is `r format(this$data_max_limit, big.mark = ',')`.
        #' A warning is issued if input value is greater than that. In this
        #' case, `limit` will be reset to `r format(this$data_max_limit, big.mark = ',')`.
        #'
        #' @param value
        #' `r rd_query_method_param("limit", "integer", default = 10L, nullable = FALSE)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$limit()
        #'
        #' # set the parameter
        #' q$limit(10L)
        #'
        #' # `limit` is reset to the allowed maximum query limit if input is greater than that
        #' q$limit(12000L) # warning
        #' }
        limit = function(value = 10L) {
            if (missing(value)) return(private$parameter$limit)
            checkmate::assert_count(value, .var.name = "limit")
            if (value > this$data_max_limit) {
                warning(sprintf(
                    paste(
                        "ESGF Search API only supports a maximum value of limit <= %s",
                        "'limit' has been reset to '%s'."
                    ),
                    format(this$data_max_limit, big.mark = ","),
                    this$data_max_limit
                ))
                value <- this$data_max_limit
            }
            private$parameter$limit <- new_query_param("limit", value)
            self
        },
        # }}}

        # offset {{{
        #' @description
        #' Get or set the `offset` parameter.
        #'
        #' If the query returns records that exceed the
        #' \href{#method-EsgfQuery-limit}{\code{limit}} number,
        #' `$offset()` can be used to paginate through the available results.
        #'
        #' @param value
        #' `r rd_query_method_param("offset", "integer", default = 0L, nullable = FALSE)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$offset()
        #'
        #' # set the parameter
        #' q$offset(0L)
        #' }
        offset = function(value = 0L) {
            if (missing(value)) return(private$parameter$offset)
            checkmate::assert_count(value, .var.name = "offset")
            private$parameter$offset <- new_query_param("offset", value)
            self
        },
        # }}}

        # distrib {{{
        #' @description
        #' Get or set the `distrib` facet
        #'
        #' By default, the query is sent to all ESGF Nodes, i.e.
        #' `$distrib(TRUE)`.
        #' `$distrib(FALSE)` can be used to execute the query only on the
        #' target node.
        #'
        #' @param value
        #' `r rd_query_method_param("distrib", "flag", default = TRUE, nullable = FALSE)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$distrib()
        #'
        #' # set the parameter
        #' q$distrib(TRUE)
        #' }
        distrib = function(value = TRUE) {
            if (missing(value)) return(private$parameter$distrib)
            checkmate::assert_flag(value, .var.name = "distrib")
            private$parameter$distrib <- new_query_param("distrib", value)
            self
        },
        # }}}

        # params {{{
        #' @description
        #' Get or set other parameters.
        #'
        #' `$params()` can be used to specify other parameters that do not have
        #' a dedicated method, e.g. `version`, `master_id`, etc. It can also be
        #' used to overwrite existing parameter values specified using methods
        #' like \href{#method-EsgfQuery-activity_id}{\code{$activity_id()}}.
        #'
        #' @param ... Parameter values to set. There are three options:
        #'
        #' - If not given, existing parameters that do not have a dedicated
        #'   method are returned.
        #' - If `NULL`, all existing parameters that do not have a dedicated
        #'   method are removed.
        #' - A named vector, e.g. `$params(score = 1, table_id = "day")` will
        #'   set `score` to `1` and `table_id` to `day`.
        #'   The `!` notation can still be used to negate the constraints, e.g.
        #'   `$params(table_id = !c("3hr", "day"))` searches for all `table_id`
        #'   except for `3hr` and `day`.
        #'
        #' @return
        #'
        #' - If parameters are specified, the modified `EsgfQuery` object.
        #'
        #' - Otherwise, an empty list for `$params(NULL)` or a list of
        #'   `EsgfQueryParam` objects.
        #'
        #' @examples
        #' \dontrun{
        #' # get current values
        #' # default is an empty list (`list()`)
        #' q$params()
        #'
        #' # set the parameter
        #' q$params(table_id = c("3hr", "day"), member_id = "00")
        #' q$params()
        #'
        #' # reset existing parameters
        #' q$frequency("day")
        #' q$params(frequency = "mon")
        #' q$frequency() # frequency value has been changed using $params()
        #'
        #' # negating the constraints is also supported
        #' q$params(table_id = !c("3hr", "day"))
        #'
        #' # use NULL to remove all parameters
        #' q$params(NULL)$params()
        #' }
        params = function(...) {
            dots <- eval(substitute(alist(...)))

            if (length(dots) == 0L) return(private$parameter$others)

            if (length(dots) == 1L && is.null(names(dots)) && is.null(dots[[1L]])) {
                private$parameter$others <- list()
                return(self)
            }

            params <- eval_with_bang(...)
            checkmate::assert_list(
                lapply(params, .subset2, "value"),
                # allow NULL
                types = c("logical", "numeric", "character", "null"),
                any.missing = TRUE, names = "unique", .var.name = "params"
            )
            nms <- names(params)

            all_facets <- self$list_all_facets()
            predefined <- private$predefined_facets()

            if ("type" %in% nms && length(type <- params[[nms == "type"]])) {
                checkmate::assert_choice(type$value,
                    c("Dataset", "File", "Aggregation"), .var.name = "type"
                )

                if (type$value != "Dataset") {
                    warning(sprintf(
                        paste(
                            "Only 'Dataset' query is supported.",
                            "But 'type' found in input with value = '%s'.",
                            "It has been reset to 'Dataset'.",
                            "If you want to perform a '%s' query,",
                            "please first run 'EsgfQuery$collect()' to get the 'Dataset'",
                            "result, and then use 'EsgfQueryResultDataset$collect(type = '%s')'."
                        ),
                        type$value, type$value, type$value
                    ))
                    params$type$value <- "Dataset"
                    params$type$negate <- FALSE
                }
            }

            if ("format" %in% nms && length(fmt <- params[[nms == "format"]]) &&
                (is.null(fmt) || fmt$value != "application/solr+json")
            ) {
                warning(sprintf(
                    paste(
                        "Only JSON response format is supported.",
                        "But 'format' found in input with value = '%s'.",
                        "It has been reset to 'application/solr+json'."
                    ),
                    if (is.null(fmt)) "NULL" else fmt$value
                ))
                params <- params[nms != "format"]
                nms <- nms[nms != "format"]
            }

            is_predefined <- nms %in% predefined
            params_base <- params[is_predefined]
            names_base <- nms[is_predefined]
            params_oth <- params[!is_predefined]
            names_oth <- nms[!is_predefined]

            if (length(params_oth)) {
                private$parameter$others <- lapply(
                    seq_along(params_oth),
                    function(i) {
                        if (names_oth[[i]] %in% all_facets) {
                            checkmate::assert_subset(
                                params_oth[[i]]$value,
                                names(private$facet_listing_count(names_oth[[i]])),
                                .var.name = names_oth[[i]]
                            )
                        }
                        new_query_param(names_oth[[i]], params_oth[[i]])
                    }
                )
                names(private$parameter$others) <- names_oth
            }

            if (length(params_base)) {
                tryCatch(
                    {
                        # restore the original parameter values in case of errors
                        params_base_ori <- private$parameter[names_base]

                        for (i in seq_along(params_base)) {
                            name <- names_base[[i]]
                            value <- params_base[[i]]

                            checkmate::assert_atomic(value$value, any.missing = FALSE, .var.name = "param")

                            if (!is.null(value) && isTRUE(value$negate)) {
                                eval(substitute(self[[name]](!value), list(value = value$value)))
                            } else {
                                eval(substitute(self[[name]](value), list(value = value$value)))
                            }
                        }
                    },
                    error = function(e) {
                        for (i in seq_along(names_base)) {
                            private$parameter[[names_base[[i]]]] <- params_base_ori[[i]]
                        }
                        stop(e)
                    }
                )
            }

            self
        },
        # }}}

        # url {{{
        #' @description
        #' Get the URL of actual query or wget script
        #'
        #' @param wget Whether to return the URL of the wget script that can be
        #'        used to download all files matching the given constraints.
        #'        Default: `FALSE`.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' \dontrun{
        #' q$url()
        #'
        #' # get the wget script URL
        #' q$url(wget = TRUE)
        #'
        #' # You can download the wget script using the URL directly. For
        #' # example, the code below downloads the script and save it as
        #' # 'wget.sh' in R's temporary folder:
        #' download.file(q$url(TRUE), file.path(tempdir(), "wget.sh"), mode = "wb")
        #'
        #' }
        url = function(wget = FALSE) {
            checkmate::assert_flag(wget)
            query_build(
                private$url_host,
                private$parameter,
                type = if (wget) "wget" else "search"
            )
        },
        # }}}

        # count {{{
        #' @description
        #' Send a query of facet counting and fetch the results
        #'
        #' @param facets `NULL`, a flag or a character vector. There are three
        #'        options:
        #'
        #' - If `NULL` or `FALSE`, only the total number of matched records is
        #'   returned.
        #' - If `TRUE`, the value of \href{#method-EsgfQuery-facets}{\code{$facets()}}
        #'   is used to limit the facets. This is the default value.
        #' - If a character vector, it is used to limit the facets.
        #'
        #' @return
        #'
        #' - If `facets` equals `NULL` or `FALSE`, or `$facets()` returns `NULL`,
        #'   an integer.
        #' - Otherwise, a named list with the first element always being `total`
        #'   which is the total number of matched records. Other elements have
        #'   the same length as input facets and are all named integer vectors.
        #'
        #' @examples
        #' \dontrun{
        #' # get the total number of matched records
        #' q$count(NULL) # or q$count(facets = FALSE)
        #'
        #' # count records for specific facets
        #' q$facets(c("activity_id", "source_id"))$count()
        #'
        #' # same as above
        #' q$count(facets = c("activity_id", "source_id"))
        #' }
        count = function(facets = TRUE) {
            params <- private$parameter
            params$limit <- new_query_param("limit", 0)

            # reset facets
            if (!checkmate::test_flag(facets)) {
                params$facets <- private$new_facet_param("facets", facets, FALSE)
                facets <- !is.null(facets)
            } else {
                # use current stored facets
                if (facets) {
                    # if current facets is set to empty, returns the total
                    # number
                    if (is.null(params$facets)) facets <- FALSE
                } else {
                    params$facets <- NULL
                }
            }

            url <- query_build(private$url_host, params)
            res <- read_json_response(url, simplifyVector = FALSE)

            if (!facets) return(res$response$numFound)

            counts <- lapply(res$facet_counts$facet_fields, private$format_facet_counts)
            c(list(total = res$response$numFound), counts)
        },
        # }}}

        # collect {{{
        #' @description
        #' Send the actual query and fetch the results
        #'
        #' `$collect()` sends the actual query with **`type=Dataset`** to the
        #' ESGF search services and returns the results as an
        #' [EsgfQueryResultDataset] object.
        #' The fields included depend on `fields` parameter.
        #' However, the following fields are always included in the results:
        #' `r paste0("\\verb{", EsgfQueryResultDataset$private_fields$required_fields, "}", collapse = ", ")`.
        #'
        #' @param all Whether to collect all results despite of the value of
        #'        `offset`. Default: `FALSE`.
        #'
        #' @param limit Only applicable when `all` is set to `TRUE`. Whether to
        #'        respect the current value of `limit` when collecting all
        #'        matched records. If `FALSE`, the allowed maximum limit number
        #'        `r this$data_max_limit` is used. It can also be a positive
        #'        integer which will be used as a temporary limit per query.
        #'        Default: `FALSE`.
        #'
        #' @param params Whether to include facet fields that have parameter
        #'        constraints explicitly set using `EsgfQuery$project()`,
        #'        `EsgfQuery$activity_id()`, `EsgfQuery$params()` and etc.
        #'        Default: `TRUE`.
        #'
        #' @return An [EsgfQueryResultDataset] object.
        #'
        #' @examples
        #' \dontrun{
        #' # by default, all fields with constrains are included in the results
        #' query <- query_esgf()$experiment_id("ssp585")$frequency("1hr")$fields("source_id")
        #' res1 <- query$collect()
        #' res1$fields
        #'
        #' # set `params` to `FALSE` to exclude them
        #' query$collect(params = FALSE)$fields
        #'
        #' # collect all matched records with `query$limit()` records per query
        #' res2 <- query$collect(all = TRUE, limit = TRUE)
        #' identical(query$count(), res2$count())
        #'
        #' # same as above, but collect all matched records with max allowed
        #' # record limit per query
        #' res3 <- query$collect(all = TRUE, limit = FALSE)
        #' identical(res2$count(), res3$count())
        #'
        #' # same as above, but collect all matched records with specified limit
        #' # per query
        #' res4 <- query$collect(all = TRUE, limit = 30)
        #' identical(res2$count(), res4$count())
        #' }
        collect = function(all = FALSE, limit = TRUE, params = TRUE) {
            result <- query_collect(
                private$url_host, private$parameter,
                required_fields = EsgfQueryResultDataset$private_fields$required_fields,
                all = all, limit = limit, constraints = params
            )

            # replace docs in the last response
            result$response$response$docs <- result$docs

            # store last result
            private$last_result <- list(
                url_host = private$url_host,
                parameter = private$parameter,
                response = result$response
            )

            # create new results
            new_query_result(EsgfQueryResultDataset,
                private$url_host, private$parameter, result$response
            )
        },
        # }}}

        # save {{{
        #' @description
        #' Save the query into a JSON file
        #'
        #' `$save()` puts main data of an `EsgfQuery` object into a JSON file
        #' which can be loaded to restore the current state of query using
        #' \href{#method-EsgfQuery-load}{\code{EsgfQuery$load()}}.
        #'
        #' @param file A string indicating the JSON file path to save the data
        #'        to. Default: `query.json`.
        #'
        #' @param pretty Whether to add indentation whitespace to JSON output.
        #'        For details, please see [jsonlite::toJSON()]. Default: `TRUE`.
        #'
        #' @return The full path of the output JSON file.
        #'
        #' @examples
        #' \dontrun{
        #' q$save(tempfile(fileext = ".json"))
        #' }
        save = function(file = "query.json", pretty = TRUE) {
            query_save(
                host = private$url_host,
                parameter = private$parameter,
                last_result = private$last_result,
                facet_listing = private$facet_listing,
                file = file, pretty = pretty
            )
        },
        # }}}

        # load {{{
        #' @description
        #' Restore the query state from an JSON file
        #'
        #' `$load()` reads data of an `EsgfQuery` object from a JSON file
        #' created using
        #' \href{#method-EsgfQuery-save}{\code{EsgfQuery$save()}}.
        #'
        #' @param file A string indicating the JSON file path to read the data
        #'        from. Default: `query.json`.
        #'
        #' @return The modified `EsgfQuery` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' f <- tempfile(fileext = "json")
        #'
        #' q <- query_esgf()
        #' json <- q$save(f)
        #' q$load(f)
        #' }
        load = function(file = "query.json") {
            q <- query_load(file)

            private$url_host <- q$host
            private$parameter <- q$parameter
            private$facet_listing <- q$facet_listing
            private$last_result <- q$last_result

            if (length(q$last_result)) {
                # restore parameters in last result
                q$last_result$parameter <- restore_params(
                    input = q$last_result$parameter,
                    params = EsgfQuery$private_fields$parameter
                )
                private$last_result <- q$last_result
            }

            self
        },
        # }}}

        # print {{{
        #' @description
        #' Print a summary of the current `EsgfQuery` object
        #'
        #' `$print()` gives the summary of current `EsgfQuery` object including
        #' the host URL, the built time of facet listing and all query parameters.
        #'
        #' @return The `EsgfQuery` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' q$print()
        #' }
        print = function() {
            d <- cli::cli_div(
                theme = list(rule = list("line-type" = "double"))
            )
            cli::cli_rule("ESGF Query")
            cli::cli_li("Host: {private$url_host}")
            if (!private$has_facet_listing()) {
                cli::cli_li("Facet listing built at: {.emph <NONE>}")
                cli::cli_bullets(c(" " = "{.emph NOTE: You may run `$build_listing()`} to create the listing"))
            } else {
                ts <- format(private$facet_listing$timestamp, tz = Sys.timezone(), usetz = TRUE)
                cli::cli_li("Facet listing built at: {ts}")
            }

            ts <- if (is.null(private$last_result)) {
                "<NONE>"
            } else {
                private$last_result$response$timestamp
            }
            cli::cli_li("Last query sent at: {ts}")

            cli::cli_h1("<Query Parameter>")
            print_query_params(private$parameter)

            invisible(self)
        }
        # }}}
    ),

    private = list(
        url_host = NULL,
        facet_listing = list(),

        # all query parameters
        parameter = list(
            # facets
            project = "CMIP6",
            activity_id = NULL,
            experiment_id = NULL,
            source_id = NULL,
            variable_id = NULL,
            frequency = NULL,
            variant_label = NULL,
            nominal_resolution = NULL,
            # others
            data_node = NULL,
            facets = NULL,
            fields = "*",
            shards = NULL,
            replica = NULL,
            latest = TRUE,
            type = "Dataset",
            offset = 0L,
            distrib = TRUE,
            limit = 10L,
            format = "application/solr+json",
            others = list()
        ),

        last_result = NULL,

        has_facet_listing = function() length(private$facet_listing),

        facet_listing_count = function(facet) {
            private$format_facet_counts(
                private$facet_listing$facet_counts$facet_fields[[facet]]
            )
        },

        format_facet_counts = function(counts) {
            ind <- seq_along(counts)
            nm <- unlst(.subset(counts, ind[ind %% 2L == 1L]))
            value <- unlst(.subset(counts, ind[ind %% 2L == 0L]))
            names(value) <- nm

            value
        },

        # validate_facet_value {{{
        validate_facet_value = function(facet, value) {
            if (!private$has_facet_listing()) return(value)

            if (facet %in% c("facets", "fields")) {
                if ("*" %in% value) {
                    value <- "*"
                }
                if (facet == "facets") {
                    choices <- c("*", self$list_all_facets())
                } else {
                    choices <- c("*", self$list_all_fields())
                }
            } else if (facet == "shards") {
                # suffix should be excluded when query
                choices <- self$list_all_shards()
                if (length(choices)) {
                    choices <- gsub("(?<=/solr).+", "", choices, perl = TRUE)
                }
            } else {
                choices <- self$list_all_values(facet)
            }

            if (length(choices)) {
                checkmate::assert_subset(value, empty.ok = TRUE, .var.name = facet, choices = choices)
            }
            unique(value)
        },
        # }}}

        new_facet_param = function(facet, value, allow_negate = TRUE, env = parent.frame()) {
            if (allow_negate) {
                value <- eval(substitute(eval_with_bang(value)[[1L]], env))
            } else {
                value <- list(value = value, negate = FALSE)
            }

            value$value <- private$validate_facet_value(facet, value$value)
            new_query_param(facet, value)
        },

        predefined_facets = function() {
            setdiff(names(private$parameter), c("type", "format", "others"))
        }
    )
)
# }}}

# query_param_encode {{{
query_param_encode <- function(param) {
    # nocov start
    # only for character
    if (!is.character(param)) return(param)
    # nocov end

    # escape encoding if needed
    if (!is.null(attr(param, "encoded", TRUE)) && attr(param, "encoded", TRUE)) {
        return(param)
    }

    # '*', '.', ':', '_', '|', '-' are kept
    reg <- "[^a-zA-Z0-9*.:_|-]"

    vapply(param, FUN.VALUE = character(1L), USE.NAMES = FALSE, function(value) {
        s <- strsplit(value, "")[[1L]]
        if (length(ind <- grep(reg, s))) {
            esc <- vapply(
                s[ind],
                function(char) paste0("%", toupper(as.character(charToRaw(char))), collapse = ""),
                character(1L)
            )
            s[ind] <- esc
        }

        paste(s, collapse = "")
    })
}
# }}}

# query_param_flat {{{
query_param_flat <- function(params, exclude = NULL, empty = FALSE, merge = TRUE) {
    checkmate::assert_names(names(params))
    checkmate::assert_character(exclude, null.ok = TRUE, any.missing = FALSE)
    checkmate::assert_flag(empty)
    checkmate::assert_flag(merge)

    params_other <- params[["others"]]
    params_base <- params[names(params) != "others"]

    # standardize params
    for (i in seq_along(params_base)) {
        if (!is.null(params_base[[i]]) && !is_query_param(params_base[[i]])) {
            params_base[[i]] <- new_query_param(names(params_base[i]), params_base[[i]])
        }
    }
    for (i in seq_along(params_other)) {
        if (!is.null(params_other[[i]]) && !is_query_param(params_other[[i]])) {
            params_other[[i]] <- new_query_param(names(params_other[i]), params_other[[i]])
        }
    }

    if (!length(params_other)) {
        params <- params_base
    } else {
        # merge
        params <- utils::modifyList(params_base, params_other)
    }

    if (!is.null(exclude)) {
        params <- params[!names(params) %in% exclude]
    }

    if (!empty) {
        # skip empty parameter
        params <- params[!vapply(params, is.null, logical(1L))]
    }

    params
}
# }}}

# query_build {{{
query_build <- function(host, params, type = "search") {
    checkmate::assert_choice(type, c("search", "wget"))
    params <- query_param_flat(params)

    if (type == "wget") {
        params <- params[!names(params) %in% c("type", "format")]
    }

    if (!length(params)) return(NULL)

    paste0(
        sprintf("%s/%s?", host, type),
        paste(vapply(params, format.EsgfQueryParam, FUN.VALUE = ""), collapse = "&")
    )
}
# }}}

# query_build_facet_listing {{{
query_build_facet_listing <- function(host, project = "CMIP6") {
    # build a query without project to get facet names and values
    url <- query_build(host,
        list(
            project = NULL,
            facets = "*",
            limit = 0,
            distrib = TRUE,
            format = "application/solr+json"
        )
    )
    res <- read_json_response(url, simplifyVector = FALSE)

    # build a query with project to get the Shards and field names
    url <- query_build(host,
        list(
            project = "CMIP6",
            limit = 1,
            distrib = TRUE,
            fields = "*",
            format = "application/solr+json"
        )
    )
    res2 <- read_json_response(url)
    res$responseHeader$params$shards <- res2$responseHeader$params$shards
    res$responseHeader$params$fields <- names(res2$response$docs)

    # add timestamp
    res$timestamp <- now()

    this$cache$facet[[host]] <- res
}
# }}}

# query_collect {{{
query_collect <- function(host, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE) {
    checkmate::assert_flag(all)
    checkmate::assert_flag(constraints)
    checkmate::assert(
        checkmate::check_flag(limit),
        checkmate::check_integerish(limit, lower = 1L, upper = this$data_max_limit, len = 1L)
    )

    params <- query_param_flat(params)
    # include necessary fields
    if (!is.null(params$fields) && !"*" %in% params$fields$value) {
        params$fields <- params$fields$value

        if (!is.null(required_fields)) {
            params$fields <- unique(c(params$fields, required_fields))
        }

        if (constraints) {
            # all non-emtpy param names
            par_nms <- names(params)[!vapply(params, is.null, logical(1L))]
            # exclude keywords
            keywords <- c(
                "facets", "offset", "limit", "fields", "format", "type",
                "replica", "latest", "distrib", "shards", "bbox", "start",
                "end", "from", "to"
            )
            par_nms <- setdiff(par_nms, keywords)
            params$fields <- unique(c(params$fields, par_nms))
        }

        # convert back to a query param in case steps below assume it to be one
        params$fields <- new_query_param("fields", params$fields)
    }

    # reset limit to the allowed maximum number with zero offset
    if (all) {
        # use specified batch
        if (is.numeric(limit)) {
            params$limit <- limit
        # use 'unlimited' batch
        } else if (!limit) {
            params$limit <- this$data_max_limit
        }

        params$offset <- 0L
    }

    response <- read_json_response(query_build(host, params))
    docs <- response$response$docs

    # check if the total number is less that the limit
    total <- response$response$numFound
    if (total > 0L && all) {
        current <- length(response$response$docs[[1]])
        left <- total - current

        while (left > 0L) {
            params$offset <- current

            response <- read_json_response(query_build(host, params))

            # combine results
            docs <- rbind(docs, response$response$docs)
            current <- nrow(docs)

            left <- total - current
        }
    }

    # 'score' is always returned in the response
    # remove if unless explicitly required
    if ("score" %in% names(docs) && !is.null(fields <- params$fields)) {
        in_facets <- "score" %in% fields$value
        if ((in_facets && fields$negate) || (!in_facets && !fields$negate)) {
            docs$score <- NULL
        }
    }

    list(response = response, docs = docs)
}
# }}}

# query_save {{{
query_save <- function(host, parameter, last_result, ..., file = "query.json", pretty = TRUE) {
    checkmate::assert_string(file)
    checkmate::assert_choice(tools::file_ext(file), "json")

    params <- query_param_flat(parameter, empty = TRUE)
    # exclude name
    params <- lapply(params, .subset, c("value", "negate"))

    if (length(last_result)) {
        last_result$parameter <- query_param_flat(
            last_result$parameter, empty = TRUE
        )
        # exclude name
        last_result$parameter <- lapply(last_result$parameter,
            .subset, c("value", "negate"))

        # NOTE: the timestamp may include sub-seconds, but
        # jsonlite::toJSON() will cut that part when converting POSIXct
        # to string we can convert it to a string in advance
        if (length(last_result$response$timestamp)) {
            last_result$response$timestamp <- format.POSIXct(
                last_result$response$timestamp, digits = 6, tz = "UTC",
                format = "%Y-%m-%dT%H:%M:%S:%OS6Z"
            )
        }
    }

    data <- list(
        host = host,
        parameter = params,
        last_result = last_result
    )

    if (length(list(...))) data <- c(data, list(...))

    if (length(data$response) && length(data$response$timestamp)) {
        # NOTE: the timestamp may include sub-seconds, but
        # jsonlite::toJSON() will cut that part when converting POSIXct
        # to string we can convert it to a string in advance
        data$response$timestamp <- format.POSIXct(
            data$response$timestamp, digits = 6, tz = "UTC",
            format = "%Y-%m-%dT%H:%M:%S:%OS6Z"
        )
    }

    if (length(data$facet_listing) && length(data$facet_listing$timestamp)) {
        # NOTE: the timestamp may include sub-seconds, but
        # jsonlite::toJSON() will cut that part when converting POSIXct
        # to string we can convert it to a string in advance
        data$facet_listing$timestamp <- format.POSIXct(
            data$facet_listing$timestamp, digits = 6, tz = "UTC",
            format = "%Y-%m-%dT%H:%M:%S:%OS6Z"
        )
    }

    jsonlite::write_json(data, file, null = "null", digits = 6, pretty = pretty)

    normalizePath(file, mustWork = TRUE)
}
# }}}

# query_load {{{
query_load <- function(file = "query.json", schema = SCHEMA_QUERY) {
    checkmate::assert_file(file, "r", extension = "json")

    # simplifyVector will convert facet counts to characters
    # have to set simplifyMatrix to FALSE
    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)

    # validate using predefined schema
    schema_validate(json, schema)

    if (length(json$parameter)) {
        json$parameter <- restore_params(
            input = json$parameter,
            params = EsgfQuery$private_fields$parameter
        )
    }

    if (length(json$response) && length(json$response$timestamp)) {
        json$response$timestamp <- as.POSIXct(
            json$response$timestamp, tz = "UTC",
            format = "%Y-%m-%dT%H:%M:%S:%OSZ"
        )
    }

    if (length(json$last_result) && length(json$last_result$response$timestamp)) {
        json$last_result$response$timestamp <- as.POSIXct(
            json$last_result$response$timestamp, tz = "UTC",
            format = "%Y-%m-%dT%H:%M:%S:%OSZ"
        )
    }

    if (length(json$facet_listing) && length(json$facet_listing$timestamp)) {
        json$facet_listing$timestamp <- as.POSIXct(
            json$facet_listing$timestamp, tz = "UTC",
            format = "%Y-%m-%dT%H:%M:%S:%OSZ"
        )
    }

    json
}
# }}}

# restore_params {{{
restore_params <- function(input, params = EsgfQuery$private_fields$parameter) {
    for (nm in names(input)) {
        par <- input[[nm]]
        if (!is.null(par)) {
            par <- new_query_param(nm, par[c("value", "negate")])
            if (nm %in% names(params)) {
                params[[nm]] <- new_query_param(nm, par)
            } else {
                params$others[[nm]] <- new_query_param(nm, par)
            }
        }
    }
    params
}
# }}}

# print_query_params {{{
print_query_params <- function(params) {
    fields <- params[names(params) != "others"]
    for (fld in fields) {
        if (!is.null(fld) && !is.null(fld$value)) {
            cli::cli_bullets(c("*" = format(fld, encode = FALSE, space = TRUE)))
        }
    }

    if (length(params$others)) {
        for (param in params$others) {
            if (!is.null(param) && !is.null(param$value)) {
                cli::cli_bullets(c("*" = format(param, encode = FALSE, space = TRUE)))
            }
        }
    }

    invisible(params)
}
# }}}

# vim: fdm=marker :
