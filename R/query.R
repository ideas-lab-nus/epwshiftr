#' @include query-param.R
#' @include query-result.R
NULL

# Federated ESGF Nodes
# https://esgf.github.io/nodes.html
INDEX_NODES <- c(
    # Metagrid Nodes - Europe/AU
    CEDA = "https://esgf.ceda.ac.uk",
    DKRZ = "https://esgf-data.dkrz.de",
    NCI = "https://esgf.nci.org.au",

    # Metagrid Nodes - DOE/US
    ORNL = "https://esgf-node.ornl.gov",
    LLNL = "https://esgf-node.llnl.gov",

    # CoG Nodes - Europe/AU
    IPSL = "https://esgf-node.ipsl.upmc.fr",
    LIU = "https://esg-dn1.nsc.liu.se"
)

FIELDS_FACETS_COMMON <- c(
    "activity",
    "activity_drs",
    "activity_id",
    "cmor_table",
    "dataset_id",
    "driving_model",
    "domain",
    "ensemble",
    "experiment",
    "experiment_id",
    "frequency",
    "index_node",
    "institution",
    "institution_id",
    "member_id",
    "mip_era",
    "model",
    "nominal_resolution",
    "project",
    "rcm_name",
    "realm",
    "source_id",
    "table_id",
    "time_frequency",
    "variable",
    "variable_id",
    "variant_label"
)

# esg_query {{{
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
#' `EsgQuery` is the workhorse for dealing with ESGF search services.
#' For new code, start with `esg_query()` / `EsgQuery`. The older [esgf_query()]
#' wrapper is retained for compatibility with the legacy data.table-oriented API,
#' emits a gentle deprecation warning, and preserves its historical LLNL `host`
#' semantics instead of `esg_query()`'s ORNL `index_node` default.
#'
#' # `EsgQuery` object
#'
#' `esg_query()` returns an `EsgQuery` object, which is an [R6][R6::R6Class]
#' object with quite a few methods that can be classified into 3 categories:
#'
#' - **Value listing**: methods to list all possible values of facets, fields,
#'   shards, and values.
#'
#' - **Parameter getter & setter**: methods to get the query parameter values or
#'   set them before sending the actual query to the ESGF search services.
#'
#' - **Query responses**: methods to collect results for the query response.
#'
#' ## Value listing
#'
#' `EsgQuery` object provides the following value-listing methods to query
#' available facets, fields, shards, and values from the ESGF index node:
#'
#' - \href{#method-EsgQuery-list_facets}{\code{EsgQuery$list_facets()}}:
#'   List all available facet names. When called, a
#'   [facet listing query](https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html#facet-listings)
#'   is sent to the index node to get all available facets for the current
#'   project (default: CMIP6).
#'
#' - \href{#method-EsgQuery-list_fields}{\code{EsgQuery$list_fields()}}:
#'   List all available field names. This is useful for bridge index nodes
#'   where facet listing is not available.
#'
#' - \href{#method-EsgQuery-list_shards}{\code{EsgQuery$list_shards()}}:
#'   List all available shards (ESGF index nodes) that can be queried in
#'   distributed searches.
#'
#' - \href{#method-EsgQuery-list_values}{\code{EsgQuery$list_values()}}:
#'   List all available values of specific facets.
#'
#' ## Parameter getter & setter
#'
#' The ESGF search services support a lot of parameters. The `EsgQuery`
#' contains dedicated methods to set values for most of them, including:
#'
#' - Most common keywords:
#'   \href{#method-EsgQuery-facets}{\code{facets}},
#'   \href{#method-EsgQuery-offset}{\code{offset}},
#'   \href{#method-EsgQuery-limit}{\code{limit}},
#'   \href{#method-EsgQuery-fields}{\code{fields}},
#'   \href{#method-EsgQuery-replica}{\code{replica}},
#'   \href{#method-EsgQuery-latest}{\code{latest}},
#'   \href{#method-EsgQuery-distrib}{\code{distrib}}
#'   and
#'   \href{#method-EsgQuery-shards}{\code{shards}}.
#'
#' - Most common facets:
#'   \href{#method-EsgQuery-project}{\code{project}},
#'   \href{#method-EsgQuery-activity_id}{\code{activity_id}},
#'   \href{#method-EsgQuery-experiment_id}{\code{experiment_id}},
#'   \href{#method-EsgQuery-source_id}{\code{source_id}},
#'   \href{#method-EsgQuery-variable_id}{\code{variable_id}},
#'   \href{#method-EsgQuery-frequency}{\code{frequency}},
#'   \href{#method-EsgQuery-variant_label}{\code{variant_label}},
#'   \href{#method-EsgQuery-nominal_resolution}{\code{nominal_resolution}},
#'   \href{#method-EsgQuery-datetime_range}{\code{datetime_range}},
#'   \href{#method-EsgQuery-timestamp_range}{\code{timestamp_range}},
#'   \href{#method-EsgQuery-version_range}{\code{version_range}}
#'   and
#'   \href{#method-EsgQuery-data_node}{\code{data_node}}.
#'
#' All methods act in a similar way:
#'
#' - If input is given, the corresponding parameter is set and the updated
#'   `EsgQuery` object is returned.
#'
#'   * This makes it possible to chain different parameter setters, e.g.
#'     `EsgQuery$project("CMIP6")$frequency("day")$limit(1)` sets the parameter
#'     `project`, `frequency` and `limit` sequentially.
#'
#'   * For parameters that want character inputs, you can put a preceding `!` to
#'     negate the constraints, e.g. `EsgQuery$project(!"CMIP6")` searches for
#'     all projects except for `CMIP6`.
#'
#' - If no input is given, the current parameter value is returned. For example,
#'   directly calling `EsgQuery$project()` returns the current value of the
#'   `project` parameter. The returned value can be two types:
#'
#'   * `NULL`, i.e. there is no constraint on the corresponding parameter
#'
#'   * A `QueryParam` object. Use `query_param__value()` and
#'     `query_param__negate()` to inspect it.
#'
#' Despite methods for specific keywords and facets, you can specify arbitrary
#' query parameters using
#' \href{#method-EsgQuery-params}{\code{EsgQuery$params()}} method. For
#' details on the usage, please see the
#' \href{#method-EsgQuery-params}{documentation}.
#'
#' ## Query responses
#'
#' The query is not sent unless related methods are called:
#'
#' - \href{#method-EsgQuery-count}{\code{EsgQuery$count()}}: Count the total
#'   number of records that match the query.
#'
#'   * You can return only the total number of matched record by calling
#'     `EsgQuery$count(facets = FALSE)`
#'
#'   * You can also count the matched records for specified facets, e.g.
#'     `EsgQuery$count(facets = c("source_id", "activity_id"))`
#'
#' - \href{#method-EsgQuery-collect}{\code{EsgQuery$collect()}}: Collect the
#'   query results and format it into an [EsgResultDataset] object.
#'
#' ## Bridge Index Nodes
#'
#' Some ESGF index nodes are "bridge" nodes that have certain limitations
#' compared to standard index nodes. When using a bridge index node (e.g.,
#' `https://esgf-node.ornl.gov/esgf-1-5-bridge`), the following restrictions
#' apply:
#'
#' - The `fields` parameter is not supported. All available fields are always
#'   returned.
#' - Only `Dataset` and `File` queries are supported. `Aggregation` queries
#'   should use a standard ESGF search index node, such as
#'   `https://esgf-data.dkrz.de` or `https://esgf.ceda.ac.uk`.
#' - The `retracted` parameter is not supported and will be ignored.
#' - Wget script generation is not supported. Calling `$url(wget = TRUE)` will
#'   result in an error.
#' - Facet listing is not available. `$list_facets()` will return a predefined
#'   set of common facets instead. Use `$list_fields()` to get all available
#'   fields.
#'
#' ## Other helpers
#'
#' `EsgQuery` object also provides several other helper functions:
#'
#' - **Query URL generation**:
#'
#'   * \href{#method-EsgQuery-url}{\code{EsgQuery$url()}}: Returns the actual
#'     query URL or the wget script URL which can be used to download all files
#'     matching the given constraints.
#'
#' - **State persistence**:
#'
#'   * \href{#method-EsgQuery-save}{\code{EsgQuery$save()}}: Save the query
#'     state to a JSON file for later use.
#'
#'   * \href{#method-EsgQuery-load}{\code{EsgQuery$load()}}: Restore the
#'     query state from a JSON file created by `$save()`.
#'
#' - **Display**:
#'
#'   * \href{#method-EsgQuery-print}{\code{EsgQuery$print()}}: Print a
#'     summary of the current `EsgQuery` object including the index node URL
#'     and all query parameters.
#'
#' @author Hongyuan Jia
#'
#' @name EsgQuery
#'
#' @param index_node The URL to the ESGF Index Node. Default is to use the
#'        [ORNL (Oak Ridge National Laboratory) Index Node](https://esgf-node.ornl.gov).
#'        Current possible values could be:
#'
#' - ORNL (Oak Ridge National Laboratory), USA:
#'   `https://esgf-node.ornl.gov`. The default value.
#' - LLNL (Lawrence Livermore National Laboratory), USA:
#'   `https://esgf-node.llnl.gov`
#' - NCI (National Computational Infrastructure), Australia:
#'   `https://esgf.nci.org.au`
#' - IPSL (Institut Pierre-Simon Laplace), France:
#'   `https://esgf-node.ipsl.upmc.fr`
#' - DKRZ (Deutsches Klimarechenzentrum), Germany:
#'   `https://esgf-data.dkrz.de`
#' - LIU (National Academic Infrastructure for Supercomputing), Sweden:
#'   `https://esg-dn1.nsc.liu.se`
#' - CEDA (Centre for Environmental Data Analysis), UK:
#'   `https://esgf.ceda.ac.uk`
#'
#' @export
esg_query <- function(index_node = "https://esgf-node.ornl.gov") {
    EsgQuery$new(index_node = index_node)
}
# }}}

#' @name EsgQuery
#' @export
# EsgQuery {{{
EsgQuery <- R6::R6Class(
    "EsgQuery",
    lock_class = TRUE,
    public = list(
        # initialize {{{
        #' @description
        #' Create a new EsgQuery object
        #'
        #' @param index_node The URL to the ESGF Index Node. Default is to use the
        #'        [ORNL (Oak Ridge National Laboratory)](https://esgf-node.ornl.gov)
        #'        Index Node. Current possible values could be:
        #'
        #' - ORNL (Oak Ridge National Laboratory), USA:
        #'   `https://esgf-node.ornl.gov`. The default value.
        #' - LLNL (Lawrence Livermore National Laboratory), USA:
        #'   `https://esgf-node.llnl.gov`
        #' - NCI (National Computational Infrastructure), Australia:
        #'   `https://esgf.nci.org.au`
        #' - IPSL (Institut Pierre-Simon Laplace), France:
        #'   `https://esgf-node.ipsl.upmc.fr`
        #' - DKRZ (Deutsches Klimarechenzentrum), Germany:
        #'   `https://esgf-data.dkrz.de`
        #' - LIU (National Academic Infrastructure for Supercomputing), Sweden:
        #'   `https://esg-dn1.nsc.liu.se`
        #' - CEDA (Centre for Environmental Data Analysis), UK:
        #'   `https://esgf.ceda.ac.uk`
        #'
        #' @return An `EsgQuery` object.
        #'
        #' @examples
        #' \dontrun{
        #' q <- EsgQuery$new(index_node = "https://esgf-node.ornl.gov")
        #' q
        #' }
        initialize = function(index_node = "https://esgf-node.ornl.gov") {
            checkmate::assert_string(index_node)
            private$index_node_url <- normalize_index_node(index_node)

            private$parameter <- QueryParamStore$new()

            self
        },
        # }}}

        # index_node {{{
        #' @description
        #' Get or set the ESGF index node.
        #'
        #' `$index_node()` returns the current normalized index node URL.
        #' `$index_node(value)` updates the index node after applying the same
        #' normalization used by \href{#method-EsgQuery-new}{\code{EsgQuery$new()}}.
        #' Existing query parameters are kept unchanged.
        #'
        #' @param value A string giving the new index node URL. If omitted, the
        #'        current index node is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a string.
        #'
        #' @examples
        #' \dontrun{
        #' q$index_node()
        #' q$index_node("https://esgf.ceda.ac.uk")
        #' }
        index_node = function(value) {
            if (missing(value)) {
                return(private$index_node_url)
            }
            checkmate::assert_string(value)
            private$index_node_url <- normalize_index_node(value)
            self
        },
        # }}}

        # list_facets {{{
        #' @description
        #' List all available facet names
        #'
        #' @param force By default, every facet listing query is cached and
        #'        reused when possible. If `TRUE`, the previous cache is
        #'        abandoned and a new query is re-sent and cached. Default:
        #'        `FALSE`.
        #'
        #' @return A character vector.
        #'
        #' @note For bridge index nodes, only predefined common facets are returned.
        #'       `$list_fields()` can be used to get all available fields,
        #'       including facets.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_facets()
        #' }
        list_facets = function(force = FALSE) {
            checkmate::assert_flag(force)

            # for bridge nodes, give a hint
            if (is_bridge_index_node(private$index_node_url)) {
                verbose(cli::cli_alert_info(paste(
                    "Current index node is a bridge node. Facet listing is not available.",
                    "Predefined common facets are returned.",
                    "Please use '$list_fields()' instead to get all available fields."
                )))
                return(FIELDS_FACETS_COMMON)
            }

            url <- query_build(
                private$index_node_url,
                list(
                    project = private$parameter$project(),
                    facets = "*",
                    limit = 0,
                    distrib = FALSE,
                    format = QUERY_PARAM__FORMAT_JSON
                )
            )

            res <- private$query_listing_cached(url, force, "facet")

            unlst(res$responseHeader$params$facet.field)
        },
        # }}}

        # list_fields {{{
        #' @description
        #' List all available field names
        #'
        #' @param force By default, every field listing query is cached and
        #'        reused when possible. If `TRUE`, the previous cache is
        #'        abandoned and a new query is re-sent and cached. Default:
        #'        `FALSE`.
        #'
        #' @return A character vector or `NULL` if no facet listing is found.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_fields()
        #' }
        list_fields = function(force = FALSE) {
            checkmate::assert_flag(force)

            url <- query_build(
                private$index_node_url,
                list(
                    project = private$parameter$project(),
                    limit = 1,
                    fields = "*",
                    distrib = FALSE,
                    format = QUERY_PARAM__FORMAT_JSON
                )
            )

            res <- private$query_listing_cached(url, force, "field")
            names(res$response$docs[[1L]])
        },
        # }}}

        # list_shards {{{
        #' @description
        #' List all available shards.
        #'
        #' @param force By default, every shard listing query is cached and
        #'        reused when possible. If `TRUE`, the previous cache is
        #'        abandoned and a new query is re-sent and cached. Default:
        #'        `FALSE`.
        #'
        #' @return A character vector or `NULL` if no shard listing is found.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_shards()
        #' }
        list_shards = function(force = FALSE) {
            checkmate::assert_flag(force)

            url <- query_build(
                private$index_node_url,
                list(
                    project = private$parameter$project(),
                    type = "Dataset",
                    offset = 0,
                    limit = 0,
                    # Shards are not available if distrib is set to FALSE.
                    distrib = TRUE,
                    format = QUERY_PARAM__FORMAT_JSON
                )
            )

            res <- private$query_listing_cached(url, force, "shard")
            shards <- res$responseHeader$params$shards

            if (!length(shards)) {
                return(NULL)
            }

            shards <- strsplit(shards, ",", fixed = TRUE)[[1L]]

            # fix the index_node if it refers to the local server
            shards_parts <- regmatches(shards, regexec("(.*?)(?::(\\d*))?/solr(.*)", shards))

            invld <- shards[lengths(shards_parts) != 4L]
            if (length(invld)) {
                warning(sprintf(
                    "Unrecognized Shard specification found: %s.",
                    paste0("'", invld, "'", collapse = ", ")
                ))
            }

            vapply(shards_parts, FUN.VALUE = "", function(shard) {
                shard <- shard[-1L]
                # replace localhost
                if (tolower(shard[[1L]]) %in% c("localhost", "0.0.0.0", "127.0.0.1")) {
                    shard[1L] <- strsplit(private$index_node_url, "/", fixed = TRUE)[[1L]][[3L]]
                }
                # exclude suffix
                sprintf("%s%s%s/solr%s", shard[[1L]], if (shard[[2L]] != "") ":" else "", shard[[2L]], shard[[3L]])
            })
        },
        # }}}

        # list_values {{{
        #' @description
        #' List all available values of specific facets.
        #'
        #' @param facets A character vector giving the facet names.
        #'
        #' @param force By default, every value listing query is cached and
        #'        reused when possible. If `TRUE`, the previous cache is
        #'        abandoned and a new query is re-sent and cached. Default:
        #'        `FALSE`.
        #'
        #' @return If `length(facets) == 1`, a named integer vector giving
        #'         the facet value counts. Otherwise, a list of named integer
        #'         vectors of the same length as `facets`.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_values(c("activity_id", "experiment_id"))
        #' }
        list_values = function(facets, force = FALSE) {
            checkmate::assert_subset(facets, QUERY_PARAM__FIELDS)
            checkmate::assert_flag(force)

            url <- query_build(
                private$index_node_url,
                list(
                    project = private$parameter$project(),
                    type = "Dataset",
                    facets = paste0(facets, collapse = ","),
                    offset = 0,
                    limit = 0,
                    distrib = private$parameter$distrib(),
                    format = QUERY_PARAM__FORMAT_JSON
                )
            )

            res <- private$query_listing_cached(url, force, "value")

            out <- vector("list", length(facets))
            names(out) <- facets
            for (nm in facets) {
                out[[nm]] <- private$format_facet_counts(
                    res$facet_counts$facet_fields[[nm]]
                )
            }

            if (length(facets) == 1L) {
                out <- out[[1L]]
            }
            out
        },
        # }}}

        # parameter methods {{{
        #' @description
        #' Get or set the `project` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression such as `!"CMIP6"`. If omitted, the current value is
        #'        returned. Default when setting without an explicit value:
        #'        `"CMIP6"`.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        project = function(value = "CMIP6") {
            if (missing(value)) {
                return(private$parameter$project())
            }
            private$eval_param_call(
                substitute(private$parameter$project(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `activity_id` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression such as `!c("CFMIP", "ScenarioMIP")`. If omitted,
        #'        the current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        activity_id = function(value) {
            if (missing(value)) {
                return(private$parameter$activity_id())
            }
            private$eval_param_call(
                substitute(private$parameter$activity_id(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `experiment_id` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression such as `!c("ssp126", "ssp585")`. If omitted, the
        #'        current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        experiment_id = function(value) {
            if (missing(value)) {
                return(private$parameter$experiment_id())
            }
            private$eval_param_call(
                substitute(private$parameter$experiment_id(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `source_id` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression. If omitted, the current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        source_id = function(value) {
            if (missing(value)) {
                return(private$parameter$source_id())
            }
            private$eval_param_call(
                substitute(private$parameter$source_id(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `variable_id` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression. If omitted, the current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        variable_id = function(value) {
            if (missing(value)) {
                return(private$parameter$variable_id())
            }
            private$eval_param_call(
                substitute(private$parameter$variable_id(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `frequency` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression. If omitted, the current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        frequency = function(value) {
            if (missing(value)) {
                return(private$parameter$frequency())
            }
            private$eval_param_call(
                substitute(private$parameter$frequency(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `variant_label` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression. If omitted, the current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        variant_label = function(value) {
            if (missing(value)) {
                return(private$parameter$variant_label())
            }
            private$eval_param_call(
                substitute(private$parameter$variant_label(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `nominal_resolution` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression. If omitted, the current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        nominal_resolution = function(value) {
            if (missing(value)) {
                return(private$parameter$nominal_resolution())
            }
            private$eval_param_call(
                substitute(private$parameter$nominal_resolution(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `data_node` facet parameter.
        #'
        #' @param value A character vector, `NULL`, or a negated character
        #'        expression. If omitted, the current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        data_node = function(value) {
            if (missing(value)) {
                return(private$parameter$data_node())
            }
            private$eval_param_call(
                substitute(private$parameter$data_node(value), list(value = substitute(value))),
                parent.frame()
            )
            self
        },

        #' @description
        #' Get or set the `facets` parameter used by `$count()`.
        #'
        #' @param value A character vector, `"*"`, or `NULL`. If omitted, the
        #'        current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        facets = function(value) {
            if (missing(value)) {
                return(private$parameter$facets())
            }
            private$parameter$facets(value)
            self
        },

        #' @description
        #' Get or set the `fields` parameter.
        #'
        #' @param value A character vector, `"*"`, or `NULL`. If omitted, the
        #'        current value is returned. Default when setting without an
        #'        explicit value: `"*"`.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        fields = function(value = "*") {
            if (missing(value)) {
                return(private$parameter$fields())
            }
            private$parameter$fields(value)
            self
        },

        #' @description
        #' Get or set the `shards` parameter for distributed searches.
        #'
        #' @param value A character vector or `NULL`. If omitted, the current
        #'        value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        shards = function(value) {
            if (missing(value)) {
                return(private$parameter$shards())
            }
            private$parameter$shards(value)
            self
        },

        #' @description
        #' Get or set temporal coverage overlap constraints.
        #'
        #' @param start,stop Temporal boundary strings accepted by `solr_date()`,
        #'        complete Solr range expressions, `"*"`, or `NULL`. If both are
        #'        omitted, the current range state is returned. The helper renders
        #'        Solr constraints for the ESGF REST `start`/`end` temporal
        #'        coverage keyword semantics.
        #'
        #' @return If either boundary is supplied, the modified `EsgQuery`
        #'         object. Otherwise, a list with `start` and `stop` elements.
        datetime_range = function(start, stop) {
            if (missing(start) && missing(stop)) {
                return(private$parameter$datetime_range())
            }
            if (!missing(start)) {
                private$parameter$datetime_range(start = start)
            }
            if (!missing(stop)) {
                private$parameter$datetime_range(stop = stop)
            }
            self
        },

        #' @description
        #' Get or set Solr index timestamp range constraints.
        #'
        #' @param from,to Timestamp boundary strings accepted by `solr_date()`,
        #'        `"*"`, or `NULL`. Complete Solr range expressions are not
        #'        accepted here. If both are omitted, the current range state is
        #'        returned.
        #'
        #' @return If either boundary is supplied, the modified `EsgQuery`
        #'         object. Otherwise, a list with `from` and `to` elements.
        timestamp_range = function(from, to) {
            if (missing(from) && missing(to)) {
                return(private$parameter$timestamp_range())
            }
            if (!missing(from)) {
                private$parameter$timestamp_range(from = from)
            }
            if (!missing(to)) {
                private$parameter$timestamp_range(to = to)
            }
            self
        },

        #' @description
        #' Get or set version range constraints.
        #'
        #' @param min,max Version boundaries such as `20200101`, `"20200101"`,
        #'        simplified dates, `"*"`, or `NULL`. ESGF `version` is queried
        #'        as a numeric field; simplified date inputs are normalized to
        #'        comparable `YYYYMMDD` integer boundaries before rendering.
        #'        Solr Date Math and complete range expressions are not accepted
        #'        here. If both are omitted, the current range state is returned.
        #'
        #' @return If either boundary is supplied, the modified `EsgQuery`
        #'         object. Otherwise, a list with `min` and `max` elements.
        version_range = function(min, max) {
            if (missing(min) && missing(max)) {
                return(private$parameter$version_range())
            }
            if (!missing(min)) {
                private$parameter$version_range(min = min)
            }
            if (!missing(max)) {
                private$parameter$version_range(max = max)
            }
            self
        },

        #' @description
        #' Get or set the `replica` parameter.
        #'
        #' @param value A flag or `NULL`. If omitted, the current value is
        #'        returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        replica = function(value) {
            if (missing(value)) {
                return(private$parameter$replica())
            }
            private$parameter$replica(value)
            self
        },

        #' @description
        #' Get or set the `latest` parameter.
        #'
        #' @param value A flag, or `NULL` to remove the `latest` constraint. If
        #'        omitted, the current value is returned.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        latest = function(value = NULL) {
            if (missing(value)) {
                return(private$parameter$latest())
            }
            private$parameter$latest(value)
            self
        },

        #' @description
        #' Get or set the `limit` parameter.
        #'
        #' @param value A positive integer. If omitted, the current value is
        #'        returned. Default when setting without an explicit value: `10L`.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        limit = function(value = 10L) {
            if (missing(value)) {
                return(private$parameter$limit())
            }
            private$parameter$limit(value)
            self
        },

        #' @description
        #' Get or set the `offset` parameter.
        #'
        #' @param value A non-negative integer. If omitted, the current value is
        #'        returned. Default when setting without an explicit value: `0L`.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        offset = function(value = 0L) {
            if (missing(value)) {
                return(private$parameter$offset())
            }
            private$parameter$offset(value)
            self
        },

        #' @description
        #' Get or set the `distrib` parameter.
        #'
        #' @param value A flag. If omitted, the current value is returned.
        #'        Default when setting without an explicit value: `TRUE`.
        #'
        #' @return If `value` is supplied, the modified `EsgQuery` object.
        #'         Otherwise, a `QueryParam` object or `NULL`.
        distrib = function(value = TRUE) {
            if (missing(value)) {
                return(private$parameter$distrib())
            }
            private$parameter$distrib(value)
            self
        },

        #' @description
        #' Get or set ad hoc query parameters.
        #'
        #' `$params()` handles parameters without dedicated methods and can also
        #' update supported dedicated parameters by name.
        #' The `type` and `format` control parameters cannot be changed here:
        #' `EsgQuery` always performs Dataset queries and always parses JSON
        #' responses. Use `EsgResultDataset$collect()` to collect File or
        #' Aggregation records from Dataset results.
        #'
        #' @param ... Named parameter values. If omitted, existing ad hoc
        #'        parameters are returned. If a single unnamed `NULL` is supplied,
        #'        all ad hoc parameters are removed.
        #'
        #' @return If parameters are supplied, the modified `EsgQuery` object.
        #'         Otherwise, a named list of `QueryParam` objects.
        params = function(...) {
            dots <- eval(substitute(alist(...)))
            if (length(dots) == 0L) {
                return(private$parameter$params())
            }

            nms <- names(dots)
            if (is.null(nms)) {
                nms <- rep("", length(dots))
            }
            reserved <- intersect(nms, c("type", "format"))
            if (length(reserved)) {
                stop(
                    sprintf(
                        paste(
                            "The following parameter(s) cannot be set using 'EsgQuery$params()': [%s].",
                            "'EsgQuery' always uses Dataset queries with JSON responses;",
                            "use 'EsgResultDataset$collect(type = ...)' for File or Aggregation records."
                        ),
                        paste(sprintf("'%s'", reserved), collapse = ", ")
                    ),
                    call. = FALSE
                )
            }

            private$eval_param_call(substitute(private$parameter$params(...)), parent.frame())
            self
        },
        # }}}

        # url {{{
        #' @description
        #' Get the URL of actual query or wget script
        #'
        #' The wget script URL can be used to download a bash script that
        #' contains wget commands for downloading all files matching the query
        #' constraints. This is useful for batch downloading large amounts of
        #' data.
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
                private$index_node_url,
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
        #' - If `TRUE`, the value of \href{#method-EsgQuery-facets}{\code{$facets()}}
        #'   is used to limit the facets. If `$facets()` returns `NULL`, only the
        #'   total count is returned. This is the default value.
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
            params <- private$parameter$copy()
            params$limit(0L)

            # reset facets
            if (!checkmate::test_flag(facets)) {
                params$facets(facets)
                facets <- !is.null(facets)
            } else {
                # use current stored facets
                if (facets) {
                    # if current facets is set to empty, returns the total
                    # number
                    if (is.null(params$facets())) facets <- FALSE
                } else {
                    params$facets(NULL)
                }
            }

            url <- query_build(private$index_node_url, params)
            res <- read_json_response(url, simplifyVector = FALSE)

            if (!facets) {
                return(res$response$numFound)
            }

            counts <- lapply(res$facet_counts$facet_fields, private$format_facet_counts)
            c(list(total = res$response$numFound), counts)
        },
        # }}}

        # collect {{{
        #' @description
        #' Send the actual query and fetch the results
        #'
        #' `$collect()` sends the actual query to the ESGF search services.
        #' By default it collects **`type=Dataset`** results and returns an
        #' [EsgResultDataset] object. If `type` is `"File"` or
        #' `"Aggregation"`, it first collects matching Dataset results and then
        #' collects child File or Aggregation results for those datasets.
        #' The fields included depend on `fields` parameter.
        #' However, the following fields are always included in the results:
        #' `r paste0("\\verb{", EsgResultDataset$private_fields$required_fields, "}", collapse = ", ")`.
        #' When a local [EsgDict] is available for the query project, `$collect()`
        #' also performs a warning-only dictionary check before sending the query.
        #' Missing local dictionaries are ignored and never downloaded.
        #'
        #' @param all Whether to collect all results despite of the value of
        #'        `offset`. Default: `FALSE`.
        #'
        #' @param limit If `all = FALSE`, the maximum number of records to
        #'        collect in this request. If `all = TRUE`, the page size used
        #'        for each paginated request, not a total cap. When `all = TRUE`
        #'        and `limit = TRUE`, the current query `limit` value is used;
        #'        if `limit = FALSE`, the allowed maximum limit number
        #'        `r this$data_max_limit` is used. It can also be a positive
        #'        integer used as a temporary page size. Default: `TRUE`.
        #'
        #' @param params Whether to include facet fields that have parameter
        #'        constraints explicitly set using `EsgQuery$project()`,
        #'        `EsgQuery$activity_id()`, `EsgQuery$params()` and etc. in the
        #'        returned fields. For example, if you set `$experiment_id("ssp585")`,
        #'        the `experiment_id` field will be included in the results when
        #'        `params = TRUE`. Default: `TRUE`.
        #'
        #' @param type Result type to collect. One of `"Dataset"`, `"File"`,
        #'        or `"Aggregation"`. Default: `"Dataset"`.
        #'
        #' @param fields Optional fields used only when `type` is `"File"` or
        #'        `"Aggregation"`. Dataset fields should be configured with
        #'        `$fields()` before collecting.
        #'
        #' @param ... Arguments passed to [EsgResultDataset] child collection
        #'        when `type` is `"File"` or `"Aggregation"`, including the
        #'        `data_node` scope filter, child-query controls, and
        #'        `use_record_index_node`. File/Aggregation collection does not
        #'        use ESGF datetime search parameters; use `$filter_time()` on the
        #'        returned result for time filtering.
        #'
        #' @return An [EsgResultDataset], [EsgResultFile], or
        #' [EsgResultAggregation] object.
        #'
        #' @examples
        #' \dontrun{
        #' # by default, all fields with constrains are included in the results
        #' query <- esg_query()$experiment_id("ssp585")$frequency("1hr")$fields("source_id")
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
        collect = function(all = FALSE, limit = TRUE, params = TRUE, type = "Dataset", fields = NULL, ...) {
            type <- query_result_normalize_type(type)
            dots <- eval(substitute(alist(...)))

            collect_dataset <- function(all, limit, dict_check = TRUE) {
                result <- query_collect(
                    private$index_node_url,
                    private$parameter,
                    required_fields = EsgResultDataset$private_fields$required_fields,
                    all = all,
                    limit = limit,
                    constraints = params,
                    dict_check = dict_check
                )

                # replace docs in the last response
                result$response$response$docs <- result$docs
                result_params <- if (!is.null(result$parameter)) result$parameter else private$parameter

                # create new results
                new_query_result(
                    EsgResultDataset,
                    private$index_node_url,
                    result_params,
                    result$response,
                    context = result$context
                )
            }

            if (identical(type, "Dataset")) {
                if (length(dots)) {
                    stop(
                        "Additional query filters in `...` are only supported when `type` is 'File' or 'Aggregation'.",
                        call. = FALSE
                    )
                }
                if (!is.null(fields)) {
                    stop(
                        "`fields` in `$collect()` is only supported when `type` is 'File' or 'Aggregation'. Use `$fields()` before collecting Dataset results.",
                        call. = FALSE
                    )
                }
                return(collect_dataset(all = all, limit = limit))
            }

            child_limit <- private$collect_child_limit(limit)
            datasets <- collect_dataset(all = TRUE, limit = FALSE, dict_check = FALSE)
            datasets$collect(
                fields = fields,
                all = all,
                limit = child_limit,
                type = type,
                ...
            )
        },
        # }}}

        # state {{{
        #' @description
        #' Get the current query state.
        #'
        #' `$state()` returns a read-only snapshot containing the current index
        #' node and the current parameter state.
        #'
        #' @param name A character vector of parameter names to include, or
        #'        `NULL` to include all parameters.
        #' @param null If `TRUE`, include parameters whose current value is
        #'        `NULL`. Otherwise, omit unset parameters.
        #'
        #' @return A named list with elements `index_node` and `parameter`.
        #'
        #' @examples
        #' \dontrun{
        #' q$state()
        #' q$state(null = TRUE)
        #' }
        state = function(name = NULL, null = FALSE) {
            list(
                index_node = private$index_node_url,
                parameter = private$parameter$state(name = name, null = null)
            )
        },
        # }}}

        # reset {{{
        #' @description
        #' Reset query parameters to their defaults.
        #'
        #' `$reset()` clears the current parameter store and restores the default
        #' query parameters. The current index node is kept unchanged.
        #'
        #' @return The modified `EsgQuery` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' q$experiment_id("ssp585")$reset()
        #' }
        reset = function() {
            private$parameter <- query_param__new_store()
            self
        },
        # }}}

        # save {{{
        #' @description
        #' Save the query into a JSON file
        #'
        #' `$save()` puts main data of an `EsgQuery` object into a JSON file
        #' which can be loaded to restore the current state of query using
        #' \href{#method-EsgQuery-load}{\code{EsgQuery$load()}}.
        #'
        #' @param file A string indicating the JSON file path to save the data
        #'        to.
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
                index_node = private$index_node_url,
                parameter = private$parameter,
                response = NULL,
                file = file,
                pretty = pretty,
                schema = SCHEMA_QUERY
            )
        },
        # }}}

        # load {{{
        #' @description
        #' Restore the query state from an JSON file
        #'
        #' `$load()` reads data of an `EsgQuery` object from a JSON file
        #' created using
        #' \href{#method-EsgQuery-save}{\code{EsgQuery$save()}}.
        #'
        #' @param file A string indicating the JSON file path to read the data
        #'        from.
        #'
        #' @return The modified `EsgQuery` object itself.
        #'
        #' @examples
        #' \dontrun{
        #' f <- tempfile(fileext = "json")
        #'
        #' q <- esg_query()
        #' json <- q$save(f)
        #' q$load(f)
        #' }
        load = function(file) {
            q <- query_load(file, SCHEMA_QUERY)
            private$validate_query_state(q$parameter)

            private$index_node_url <- q$index_node
            private$parameter <- q$parameter

            self
        },
        # }}}

        # print {{{
        #' @description
        #' Print a summary of the current `EsgQuery` object
        #'
        #' `$print()` gives the summary of current `EsgQuery` object including
        #' the index node URL and all query parameters.
        #'
        #' @return The `EsgQuery` object itself, invisibly.
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
            cli::cli_li("Index Node: {private$index_node_url}")

            cli::cli_h1("<Query Parameter>")
            query_param__print(private$parameter)

            invisible(self)
        }
        # }}}
    ),

    private = list(
        index_node_url = NULL,

        parameter = NULL,

        collect_child_limit = function(limit) {
            checkmate::assert(
                checkmate::check_flag(limit),
                checkmate::check_integerish(limit, lower = 1L, upper = this$data_max_limit, len = 1L)
            )

            if (isTRUE(limit)) {
                value <- query_param__value(private$parameter$limit())
                if (is.null(value)) {
                    value <- 10L
                }
                return(as.integer(value))
            }
            if (identical(limit, FALSE)) {
                return(NULL)
            }

            as.integer(limit)
        },

        validate_query_state = function(parameter) {
            type <- parameter$type()
            type_value <- query_param__value(type)
            if (!identical(type_value, "Dataset")) {
                stop(
                    sprintf(
                        paste(
                            "'EsgQuery' only supports Dataset queries.",
                            "Loaded query has 'type' = %s.",
                            "Use 'EsgResultDataset$collect(type = ...)' for File or Aggregation records."
                        ),
                        if (is.null(type_value)) "NULL" else sprintf("'%s'", type_value)
                    ),
                    call. = FALSE
                )
            }

            format <- parameter$format()
            format_value <- query_param__value(format)
            if (!identical(format_value, QUERY_PARAM__FORMAT_JSON)) {
                stop(
                    sprintf(
                        "'EsgQuery' only supports JSON response format '%s'. Loaded query has 'format' = %s.",
                        QUERY_PARAM__FORMAT_JSON,
                        if (is.null(format_value)) "NULL" else sprintf("'%s'", format_value)
                    ),
                    call. = FALSE
                )
            }

            invisible(parameter)
        },

        format_facet_counts = function(counts) {
            ind <- seq_along(counts)
            nm <- unlst(.subset(counts, ind[ind %% 2L == 1L]))
            value <- unlst(.subset(counts, ind[ind %% 2L == 0L]))
            names(value) <- nm

            value
        },

        eval_param_call = function(expr, env) {
            call_env <- new.env(parent = env)
            call_env$private <- private
            eval(expr, envir = call_env)
        },

        query_listing_cached = function(url, force, type) {
            mode <- cache_mode()
            if (mode != "off") {
                cache <- get_cache()
                key <- get_response_cache_key(url)
                cached <- if (force) {
                    cache$remove(key)
                    structure(list(), class = "key_missing")
                } else {
                    cache$get(key)
                }

                if (!is.key_missing(cached)) {
                    verbose(cli::cli_alert_info(paste(
                        "Loaded cached {type} listing for index node {.var {private$index_node_url}}",
                        "built at {format(cached$timestamp, '%F %T %Z')}."
                    )))

                    return(cached)
                }
                if (mode == "offline") {
                    stop("Cache miss in offline mode. Cannot fetch data while offline.", call. = FALSE)
                }
            }

            verbose(cli::cli_progress_step(
                "Retrieving {type} listing for index node {.var {private$index_node_url}}...",
                paste(
                    "Retrieved {type} listing for index node {.var {private$index_node_url}}",
                    "at {format(Sys.time(), '%F %T %Z')}."
                ),
                "Failed to retrieve {type} listing for index node {.var {private$index_node_url}}."
            ))
            with_timeout(300, read_json_response(url, simplifyVector = FALSE))
        }
    )
)
# }}}

# is_bridge_index_node {{{
is_bridge_index_node <- function(index_node) {
    grepl("esgf-1-5-bridge", index_node, fixed = TRUE)
}

assert_bridge_index_node_type <- function(index_node, params) {
    if (!is_bridge_index_node(index_node)) {
        return(invisible(TRUE))
    }

    type <- query_param__value(query_param__as_store(params)$type())
    if (identical(type, "Aggregation")) {
        cli::cli_abort(c(
            "Bridge index nodes do not support {.val Aggregation} queries.",
            "i" = "The ORNL/LLNL bridge accepts only {.val Dataset} and {.val File} query types.",
            "i" = "Use a standard ESGF search index node, such as {.url https://esgf-data.dkrz.de} or {.url https://esgf.ceda.ac.uk}, for Aggregation results."
        ))
    }

    invisible(TRUE)
}
# }}}

# normalize_index_node {{{
normalize_index_node <- function(index_node, raw = FALSE) {
    index_node <- curl::curl_unescape(index_node)
    # curl::curl_parse_url() requires scheme and host to present
    if (!grepl("://", index_node, fixed = TRUE)) {
        index_node <- paste0("https://", index_node)
    }
    if (grepl("/+$", index_node)) {
        index_node <- sub("/+$", "", index_node)
    }
    parsed <- curl::curl_parse_url(index_node)

    if (
        (parsed$host == "esgf-node.ornl.gov" || parsed$host == "esgf-node.llnl.gov") &&
            (is.null(parsed$path) || parsed$path == "/")
    ) {
        # since LLNL will redirect to ORNL bridge, we always use ORNL bridge
        parsed$path <- "/esgf-1-5-bridge"
        parsed$host <- "esgf-node.ornl.gov"
    }

    if (raw) {
        return(parsed)
    }

    url <- do.call(curl::curl_modify_url, parsed)
    # curl::curl_modify_url always appends a '/' at the end of the URL
    if (is.null(parsed$path) || parsed$path == "/") {
        url <- sub("/$", "", url)
    }
    url
}
# }}}

# query_build {{{
query_render_free_text <- function(query) {
    if (!length(query) || !nchar(query)) {
        return(character())
    }

    paste0("query=", query_param__encode(query))
}

query_render_globus_value <- function(value) {
    value <- as.character(value)
    ifelse(grepl("*", value, fixed = TRUE), value, query_param__quote_bound(value))
}

query_build <- function(index_node, params, type = "search") {
    checkmate::assert_choice(type, c("search", "wget"))
    store <- query_param__clone(params)

    if (type == "wget") {
        store$type(NULL)
        store$format(NULL)
    }

    assert_bridge_index_node_type(index_node, store)

    # NOTE: handle special endpoint for bridge
    if (is_bridge_index_node(index_node)) {
        if (type == "wget") {
            stop("Input index node is a bridge. Wget script is not supported.")
        }
        endpoint <- index_node

        # remove 'retracted' since bridge does not support it
        store$params(retracted = NULL)

        # remove 'fields=' since bridge does not support it
        store$fields(NULL)
    } else {
        endpoint <- sprintf("%s/esg-search/%s", index_node, type)
    }

    params <- store$state()
    if (!length(params)) {
        return(NULL)
    }

    # separate query= params from regular facet params
    query_names <- intersect(names(store$state()), query_param__names("date"))
    is_bridge <- is_bridge_index_node(index_node)
    bridge_now <- if (is_bridge) getOption("epwshiftr.solr_date_math_now", Sys.time()) else NULL
    query_clauses <- if (length(query_names)) {
        store$render(
            query_names,
            quote_date = is_bridge,
            datetime_end_alias = is_bridge,
            eval_math = is_bridge,
            now = bridge_now
        )
    } else {
        character()
    }
    query_clauses <- query_clauses[nchar(query_clauses) > 0L]
    params <- params[!names(params) %in% query_names]

    is_negate <- vapply(params, function(param) isTRUE(query_param__negate(param)), logical(1L))
    # facet queries without any negated inputs
    if (!is_bridge || !any(is_negate)) {
        rendered <- c(
            vapply(names(params), function(name) query_param__render(params[[name]], name), FUN.VALUE = ""),
            if (length(query_clauses)) {
                query_render_free_text(paste(query_clauses, collapse = " AND "))
            }
        )
        rendered <- rendered[nchar(rendered) > 0L]
        if (!length(rendered)) {
            return(NULL)
        }
        return(paste0(endpoint, "?", paste(rendered, collapse = "&")))
    }

    # format free text queries for negated inputs for bridge index node
    # For negated facet inputs, free text queries formatted in Apache Lucene
    # query syntax should be used since bridge does not support negate syntax
    # like 'project!=CMIP6'.
    # see: https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html#free-text-queries
    facets <- paste(
        vapply(
            names(params[!is_negate]),
            function(name) query_param__render(params[[name]], name),
            FUN.VALUE = ""
        ),
        collapse = "&"
    )
    negate_query <- paste(
        vapply(
            names(params[is_negate]),
            function(name) {
                param <- params[[name]]
                value <- query_render_globus_value(query_param__value(param))
                if (length(value) == 1L) {
                    value <- value
                } else {
                    value <- sprintf("(%s)", paste(value, collapse = " "))
                }
                sprintf("NOT (%s:%s)", name, value)
            },
            FUN.VALUE = ""
        ),
        collapse = " AND "
    )

    # combine negate query with query= params
    all_query_parts <- c(negate_query, query_clauses)
    query <- paste(all_query_parts[nchar(all_query_parts) > 0L], collapse = " AND ")

    paste0(
        endpoint,
        "?",
        if (nchar(facets)) paste0(facets, "&"),
        query_render_free_text(query)
    )
}
# }}}

# query dict check {{{
query_dict_check_project <- function(store) {
    project <- store$project()
    if (is.null(project) || isTRUE(query_param__negate(project))) {
        return(NULL)
    }

    values <- esgdict__as_character(query_param__value(project))
    values <- unique(values[nzchar(values)])
    if (length(values) != 1L) {
        return(NULL)
    }

    tryCatch(
        {
            project <- esgdict__normalize_project(values[[1L]])
            esgdict__assert_implemented(project)
            project
        },
        error = function(e) NULL
    )
}

query_dict_check_load <- function(project) {
    dict <- esgdict_get_default(project)
    if (!is.null(dict) && dict$has_data()) {
        return(dict)
    }

    tryCatch(
        {
            dict <- EsgDict$new(project = project)
            suppressWarnings(suppressMessages(dict$load()))
            if (dict$has_data()) dict else NULL
        },
        error = function(e) NULL
    )
}

query_dict_check_args <- function(store, dict) {
    params <- store$state()
    if (!length(params)) {
        return(list())
    }

    out <- list()
    for (name in names(params)) {
        param <- params[[name]]
        if (
            is.null(param) ||
                !S7::S7_inherits(param, QueryParamFacet) ||
                isTRUE(query_param__negate(param)) ||
                !query_param__field(name)
        ) {
            next
        }

        field <- tryCatch(esgdict__normalize_field(name, dict), error = function(e) NULL)
        if (is.null(field)) {
            next
        }

        values <- esgdict__as_character(query_param__value(param))
        values <- unique(values[nzchar(values) & !grepl("[*?]", values)])
        if (!length(values)) {
            next
        }

        out[[field]] <- unique(c(out[[field]], values))
    }

    out
}

query_dict_check_warning <- function(invalid, n = 5L) {
    n <- min(n, nrow(invalid))
    lines <- vapply(seq_len(n), function(i) {
        msg <- invalid$message[[i]]
        suggestions <- invalid$suggestions[[i]]
        if (length(suggestions)) {
            msg <- sprintf("%s Suggestions: %s.", msg, paste(utils::head(suggestions, 3L), collapse = ", "))
        }
        sprintf("- %s", msg)
    }, character(1L))

    extra <- nrow(invalid) - n
    if (extra > 0L) {
        lines <- c(lines, sprintf("- ... and %d more.", extra))
    }

    paste(
        c("ESG dictionary check found invalid query constraint(s):", lines),
        collapse = "\n"
    )
}

query_warn_dict_check <- function(params) {
    store <- query_param__as_store(params)
    project <- query_dict_check_project(store)
    if (is.null(project)) {
        return(invisible(NULL))
    }

    dict <- query_dict_check_load(project)
    if (is.null(dict)) {
        return(invisible(NULL))
    }

    args <- query_dict_check_args(store, dict)
    if (!length(args)) {
        return(invisible(NULL))
    }

    result <- tryCatch(
        esgdict__check(dict, args, error = FALSE, suggest = TRUE, relationship = "any"),
        error = function(e) NULL
    )
    if (is.null(result) || !nrow(result)) {
        return(invisible(result))
    }

    invalid <- result[!is.na(result$valid) & !result$valid]
    if (nrow(invalid)) {
        warning(query_dict_check_warning(invalid), call. = FALSE)
    }

    invisible(result)
}
# }}}

# query_collect {{{
query_collect <- function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE, dict_check = FALSE) {
    checkmate::assert_flag(all)
    checkmate::assert_flag(constraints)
    checkmate::assert_flag(dict_check)
    checkmate::assert(
        checkmate::check_flag(limit),
        checkmate::check_integerish(limit, lower = 1L, upper = this$data_max_limit, len = 1L)
    )

    store <- query_param__clone(params)
    params <- store$state()

    # include necessary fields
    if (!is.null(params$fields)) {
        if (is_bridge_index_node(index_node)) {
            # bridge index node does not support 'fields='
            store$fields(NULL)
        } else if (!"*" %in% query_param__value(params$fields)) {
            fields <- query_param__value(params$fields)

            if (!is.null(required_fields)) {
                fields <- unique(c(fields, required_fields))
            }

            if (constraints) {
                fields <- unique(c(fields, names(params)[query_param__field(names(params))]))
            }

            store$fields(fields)
        }
    }

    # reset limit to the allowed maximum number with zero offset
    if (all) {
        # use specified batch
        if (is.numeric(limit)) {
            store$limit(as.integer(limit))
            # use 'unlimited' batch
        } else if (!limit) {
            store$limit(this$data_max_limit)
        }

        store$offset(0L)
    }

    if (dict_check) {
        query_warn_dict_check(store)
    }

    effective_store <- store$copy()
    query_urls <- character()
    url <- query_build(index_node, store)
    query_urls <- c(query_urls, url)
    response <- read_json_response(url)
    docs <- response$response$docs

    # check if the total number is less that the limit
    total <- response$response$numFound
    if (total > 0L && all) {
        current <- length(response$response$docs[[1]])
        left <- total - current

        while (left > 0L) {
            store$offset(current)

            url <- query_build(index_node, store)
            query_urls <- c(query_urls, url)
            response <- read_json_response(url)

            # combine results
            docs <- rbind(docs, response$response$docs)
            current <- nrow(docs)

            left <- total - current
        }
    }

    # 'score' is always returned in the response
    # remove if unless explicitly required
    fields <- store$fields()
    if ("score" %in% names(docs) && !is.null(fields)) {
        in_facets <- "score" %in% query_param__value(fields)
        if ((in_facets && query_param__negate(fields)) || (!in_facets && !query_param__negate(fields))) {
            docs$score <- NULL
        }
    }

    list(
        response = response,
        docs = docs,
        parameter = effective_store,
        context = list(query_url = query_result_normalize_query_url(query_urls, named = FALSE))
    )
}
# }}}

# query_save {{{
query_save <- function(index_node, parameter, response, ..., file = "query.json", pretty = TRUE, schema = NULL) {
    checkmate::assert_string(file)
    checkmate::assert_choice(tools::file_ext(file), "json")

    params <- query_param__as_store(parameter)$serialize(null = TRUE)

    if (length(response)) {
        # NOTE: the timestamp may include sub-seconds, but
        # jsonlite::toJSON() will cut that part when converting POSIXct
        # to string we can convert it to a string in advance
        if (length(response$timestamp)) {
            response$timestamp <- format.POSIXct(
                response$timestamp,
                digits = 6,
                tz = "UTC",
                format = "%Y-%m-%dT%H:%M:%S:%OS6Z"
            )
        }

        # remove the cache key
        if (length(response$cache)) {
            response$cache <- NULL
        }
    }

    data <- list(index_node = index_node, parameter = params)
    if (length(response)) {
        data$response <- response
    }

    if (length(list(...))) {
        data <- c(data, list(...))
    }

    if (!is.null(schema)) {
        schema_validate(schema, data, mode = "assert", name = file)
    }

    jsonlite::write_json(data, file, null = "null", digits = 6, pretty = pretty)

    normalizePath(file, mustWork = TRUE)
}
# }}}

# query_load {{{
query_load <- function(file, schema = NULL) {
    checkmate::assert_file(file, "r", extension = "json")

    # simplifyVector will convert facet counts to characters
    # have to set simplifyMatrix to FALSE
    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)

    if (
        length(json$response) &&
            length(json$response$response) &&
            "docs" %in% names(json$response$response) &&
            is.list(json$response$response$docs) &&
            !inherits(json$response$response$docs, "data.frame") &&
            !length(json$response$response$docs)
    ) {
        json$response$response$docs <- data.frame()
    }
    if (
        length(json$context) &&
            length(json$context$selection) &&
            "source_indices" %in% names(json$context$selection) &&
            is.list(json$context$selection$source_indices) &&
            !length(json$context$selection$source_indices)
    ) {
        json$context$selection$source_indices <- integer()
    }

    if (!is.null(schema)) {
        schema_validate(schema, json, mode = "assert", name = file)
    }

    json$parameter <- if (length(json$parameter)) {
        QueryParamStore$new()$restore(json$parameter)
    } else {
        QueryParamStore$new()
    }

    if (length(json$response) && length(json$response$timestamp)) {
        json$response$timestamp <- as.POSIXct(
            json$response$timestamp,
            tz = "UTC",
            format = "%Y-%m-%dT%H:%M:%S:%OSZ"
        )
        # change the time zone to current time zone
        attr(json$response$timestamp, "tzone") <- NULL
    }

    json
}
# }}}

# restore_params {{{
restore_params <- function(input, params = query_param__new_store()) {
    query_param__as_store(input)
}
# }}}

# vim: fdm=marker :
