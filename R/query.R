#' @include query-result.R
NULL

FORMAT_JSON <- "application/solr+json"

INDEX_NODES <- c(
    ORNL = "https://esgf-node.ornl.gov",
    LLNL = "https://esgf-node.llnl.gov",
    NCI = "https://esgf.nci.org.au",
    IPSL = "https://esgf-node.ipsl.upmc.fr",
    DKRZ = "https://esgf-data.dkrz.de",
    LIU = "https://esg-dn1.nsc.liu.se",
    CEDA = "https://esgf.ceda.ac.uk"
)

FIELDS_FACETS_ALL <- c(
    "Conventions",
    "access",
    "activity",
    "activity_drs",
    "activity_id",
    "branch_method",
    "cf_standard_name",
    "cmor_table",
    "collection",
    "contact",
    "creation_date",
    "data_node",
    "data_specs_version",
    "data_structure",
    "dataset_category",
    "dataset_status",
    "datetime_end",
    "deprecated",
    "directory_format_template_",
    "domain",
    "driving_model",
    "ensemble",
    "experiment",
    "experiment_family",
    "experiment_id",
    "experiment_title",
    "forcing",
    "format",
    "framework",
    "frequency",
    "globus_url",
    "grid",
    "grid_label",
    "index_node",
    "institute",
    "institution",
    "institution_id",
    "master_gateway",
    "member_id",
    "metadata_format",
    "mip_era",
    "model",
    "model_cohort",
    "nominal_resolution",
    "product",
    "product_version",
    "project",
    "rcm_name",
    "rcm_version",
    "realm",
    "region",
    "short_description",
    "source",
    "source_id",
    "source_type",
    "source_version",
    "source_version_number",
    "start_date_string",
    "sub_experiment_id",
    "table_id",
    "target_mip",
    "target_mip_list",
    "time_frequency",
    "variable",
    "variable_id",
    "variable_long_name",
    "variant_label",
    "version",
    "amodell",
    "cera_acronym",
    "data_type",
    "dataset_version_number",
    "doi",
    "doi_author",
    "doi_publication_year",
    "doi_publisher",
    "doi_title",
    "exp",
    "experiment_description",
    "gebiet",
    "grid_resolution",
    "lta",
    "modelvers",
    "pid_prefix",
    "rcm_model",
    "shard",
    "std_citation",
    "std_citation_author",
    "std_citation_publication_year",
    "std_citation_publisher",
    "std_citation_title",
    "ACK",
    "Acknowledgement",
    "bias_adjustment",
    "driving_ensemble",
    "driving_reanalysis",
    "project_name",
    "project_under",
    "reanalysis",
    "reanalysis_ensemble",
    "run_model",
    "metadata_url",
    "processing_level",
    "realization",
    "source_data_id",
    "work_package",
    "downscaling_model_type",
    "driving_model_ensemble_member",
    "driving_model_id",
    "predictand_calibration_dataset",
    "region_id"
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

# read_json_response {{{
read_json_response <- function(url, strict = TRUE, cache = getOption("epwshiftr.cache", TRUE), ...) {
    checkmate::assert_flag(cache)
    if (cache) {
        disk_cache <- get_cache()
        key <- get_response_cache_key(url)

        cached <- disk_cache$get(key)
        if (!is.key_missing(cached)) {
            return(cached)
        }
    }

    res <- tryCatch(jsonlite::fromJSON(url, bigint_as_char = TRUE, ...), warning = function(w) w, error = function(e) e)
    timestamp <- Sys.time()

    # nocov start
    if (inherits(res, "warning") || inherits(res, "error")) {
        cond_fun <- if (strict) stop else warning
        cond_fun(
            "Failed to read the JSON response. Details: \n",
            conditionMessage(res)
        )

        res <- NULL
        # nocov end
    } else if (res$response$numFound == 0L) {
        verbose(warning(
            "No matched data. ",
            "Please examine your query and the actual response."
        ))
    }

    res$timestamp <- timestamp

    # cache results
    if (cache) {
        # record the cache key
        res$cache <- key
        disk_cache$set(key, res)
    }

    res
}
# }}}

# new_query_param {{{
new_query_param <- function(name, value) {
    checkmate::assert_string(name)

    if (!is.list(value)) {
        value <- list(value = value, negate = FALSE)
    }
    if (is.null(value$value)) {
        return(NULL)
    }

    checkmate::assert_list(value, types = c("logical", "numeric", "character"), any.missing = TRUE, names = "unique")
    checkmate::assert_names(names(value), must.include = c("value", "negate"))

    # do not allow NAs
    for (val in value) {
        checkmate::assert_atomic(val, any.missing = FALSE)
    }

    value$name <- name
    structure(value, class = c("EsgQueryParam", "list"))
}
# }}}

# is.query_param {{{
is.query_param <- function(x) inherits(x, "EsgQueryParam")
# }}}

#' @export
# print.EsgQueryParam {{{
print.EsgQueryParam <- function(x, encode = FALSE, space = TRUE, ...) {
    cat(format.EsgQueryParam(x, encode = encode, space = space), sep = "\n")
    invisible(x)
}
# }}}

#' @export
# format.EsgQueryParam {{{
format.EsgQueryParam <- function(x, encode = TRUE, space = FALSE, ...) {
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
#'   \href{#method-EsgQuery-type}{\code{type}},
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
#'   \href{#method-EsgQuery-date_range}{\code{date_range}},
#'   \href{#method-EsgQuery-nominal_resolution}{\code{nominal_resolution}}
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
#'   * An `EsgQueryParam` object which is essentially a list of three elements:
#'
#'     + `value`: The input values
#'     + `negate`: Whether there is a preceding `!` in the input
#'     + `name`: The parameter name
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
#'     summary of the current `EsgQuery` object including the index node URL,
#'     the last query timestamp, and all query parameters.
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
            private$index_node <- normalize_index_node(index_node)

            # init parameter values
            params <- setdiff(names(private$parameter), "others")
            for (name in params) {
                if (!is.null(.subset2(private$parameter, name))) {
                    private$parameter[[name]] <- new_query_param(
                        name = name,
                        value = .subset2(private$parameter, name)
                    )
                }
            }

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
            if (is_bridge_index_node(private$index_node)) {
                verbose(cli::cli_alert_info(paste(
                    "Current index node is a bridge node. Facet listing is not available.",
                    "Predefined common facets are returned.",
                    "Please use '$list_fields()' instead to get all available fields."
                )))
                return(FIELDS_FACETS_COMMON)
            }

            url <- query_build(
                private$index_node,
                list(
                    project = private$parameter$project,
                    facets = "*",
                    limit = 0,
                    distrib = FALSE,
                    format = FORMAT_JSON
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
                private$index_node,
                list(
                    project = private$parameter$project,
                    limit = 1,
                    fields = "*",
                    distrib = FALSE,
                    format = FORMAT_JSON
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
                private$index_node,
                list(
                    project = private$parameter$project,
                    type = "Dataset",
                    offset = 0,
                    limit = 0,
                    # Shards are not available if distrib is set to FALSE.
                    distrib = TRUE,
                    format = FORMAT_JSON
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

            if (length(invld <- shards[lengths(shards_parts) != 4L])) {
                warning(sprintf(
                    "Unrecognized Shard specification found: %s.",
                    paste0("'", invld, "'", collapse = ", ")
                ))
            }

            vapply(shards_parts, FUN.VALUE = "", function(shard) {
                shard <- shard[-1L]
                # replace localhost
                if (tolower(shard[[1L]]) %in% c("localhost", "0.0.0.0", "127.0.0.1")) {
                    shard[1L] <- strsplit(private$index_node, "/", fixed = TRUE)[[1L]][[3L]]
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
        #' @return If `length(facets) == 1`, a named character vector giving
        #'         the facet value counts. Otherwise, a list of named character
        #'         vectors of the same length as `facets`.
        #'
        #' @examples
        #' \dontrun{
        #' q$list_values(c("activity_id", "experiment_id"))
        #' }
        list_values = function(facets, force = FALSE) {
            checkmate::assert_subset(facets, FIELDS_FACETS_ALL)
            checkmate::assert_flag(force)

            url <- query_build(
                private$index_node,
                list(
                    project = private$parameter$project,
                    type = "Dataset",
                    facets = paste0(facets, collapse = ","),
                    offset = 0,
                    limit = 0,
                    distrib = private$parameter$distrib,
                    format = FORMAT_JSON
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

        # project {{{
        #' @description
        #' Get or set the `project` facet parameter.
        #'
        #' @param value
        #' `r rd_query_method_param("project", "character vector", c("CMIP6"), "CMIP6")`
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
        #' # remove the parameter
        #' q$project(NULL)
        #' }
        project = function(value = "CMIP6") {
            if (missing(value)) {
                return(private$parameter$project)
            }
            # See: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            private$parameter$project <- eval(bquote(
                private$new_facet_param("project", .(substitute(value)), env = env)
            ))
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$activity_id)
            }
            # see: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            private$parameter$activity_id <- eval(bquote(
                private$new_facet_param("activity_id", .(substitute(value)), env = env)
            ))
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$experiment_id)
            }
            # see: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            private$parameter$experiment_id <- eval(bquote(
                private$new_facet_param("experiment_id", .(substitute(value)), env = env)
            ))
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$source_id)
            }
            # see: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            private$parameter$source_id <- eval(bquote(
                private$new_facet_param("source_id", .(substitute(value)), env = env)
            ))
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$variable_id)
            }
            # see: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            private$parameter$variable_id <- eval(bquote(
                private$new_facet_param("variable_id", .(substitute(value)), env = env)
            ))
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$frequency)
            }
            # see: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            private$parameter$frequency <- eval(bquote(
                private$new_facet_param("frequency", .(substitute(value)), env = env)
            ))
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$variant_label)
            }
            # see: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            private$parameter$variant_label <- eval(bquote(
                private$new_facet_param("variant_label", .(substitute(value)), env = env)
            ))
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$nominal_resolution)
            }
            # see: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            param <- eval(bquote(
                private$new_facet_param("nominal_resolution", .(substitute(value)), env = env)
            ))

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
            if (missing(value)) {
                return(private$parameter$data_node)
            }
            # See: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
            env <- parent.frame()
            private$parameter$data_node <- eval(bquote(
                private$new_facet_param("data_node", .(substitute(value)), env = env)
            ))
            invisible(self)
        },
        # }}}

        # facets {{{
        #' @description
        #' Get or set the `facets` parameter for facet counting query.
        #'
        #' Note that `$facets()` only affects
        #' \href{#method-EsgQuery-count}{\code{$count()}}
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
            if (missing(value)) {
                return(private$parameter$facets)
            }
            private$parameter$facets <- private$new_facet_param("facets", value, FALSE)
            invisible(self)
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
        #' `r paste0("\\verb{", EsgResultDataset$private_fields$required_fields, "}", collapse = ", ")`.
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
            if (missing(value)) {
                return(private$parameter$fields)
            }
            private$parameter$fields <- private$new_facet_param("fields", value, FALSE)
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$shards)
            }
            if (!private$parameter$distrib$value && !is.null(value)) {
                stop("'$distrib()' returns FALSE. Shard specification is only applicable for distributed queries.")
            }
            private$parameter$shards <- private$new_facet_param("shards", value, FALSE)
            invisible(self)
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
            if (missing(value)) {
                return(private$parameter$replica)
            }
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
            if (missing(value)) {
                return(private$parameter$latest)
            }
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
            if (missing(value)) {
                return(private$parameter$limit)
            }
            checkmate::assert_count(value, .var.name = "limit")
            if (value > this$data_max_limit) {
                warning(sprintf(
                    paste(
                        "ESGF Search API only supports a maximum value of limit <= %s",
                        "'limit' will be reset to '%s'."
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
        #' \href{#method-EsgQuery-limit}{\code{limit}} number,
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
            if (missing(value)) {
                return(private$parameter$offset)
            }
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
            if (missing(value)) {
                return(private$parameter$distrib)
            }
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
        #' like \href{#method-EsgQuery-activity_id}{\code{$activity_id()}}.
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
        #' - If parameters are specified, the modified `EsgQuery` object.
        #'
        #' - Otherwise, an empty list for `$params(NULL)` or a list of
        #'   `EsgQueryParam` objects.
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

            if (length(dots) == 0L) {
                return(private$parameter$others)
            }

            if (length(dots) == 1L && is.null(names(dots)) && is.null(dots[[1L]])) {
                private$parameter$others <- list()
                return(self)
            }

            env <- parent.frame()
            params <- eval_with_bang(..., .env = env)
            checkmate::assert_list(
                lapply(params, .subset2, "value"),
                # allow NULL
                types = c("logical", "numeric", "character", "null"),
                any.missing = TRUE,
                names = "unique",
                .var.name = "params"
            )
            nms <- names(params)

            predefined <- private$predefined_facets()

            if ("type" %in% nms && length(type <- params[[nms == "type"]])) {
                checkmate::assert_choice(type$value, c("Dataset", "File", "Aggregation"), .var.name = "type")

                if (type$value != "Dataset") {
                    warning(sprintf(
                        paste(
                            "Only 'Dataset' query is supported.",
                            "But 'type' found in input with value = '%s'.",
                            "It will be reset to 'Dataset'.",
                            "If you want to perform a '%s' query,",
                            "please first run 'EsgQuery$collect()' to get the 'Dataset'",
                            "result, and then use 'EsgResultDataset$collect(type = '%s')'."
                        ),
                        type$value,
                        type$value,
                        type$value
                    ))
                    params$type$value <- "Dataset"
                    params$type$negate <- FALSE
                }
            }

            if (
                "format" %in%
                    nms &&
                    length(fmt <- params[[nms == "format"]]) &&
                    (is.null(fmt) || fmt$value != FORMAT_JSON)
            ) {
                warning(sprintf(
                    paste(
                        "Only JSON response format is supported.",
                        "But 'format' found in input with value = '%s'.",
                        "It will be reset to '%s'."
                    ),
                    if (is.null(fmt)) "NULL" else fmt$value,
                    FORMAT_JSON
                ))
                params <- params[nms != "format"]
                nms <- nms[nms != "format"]
            }

            if (
                is_bridge_index_node(private$index_node) &&
                    "retracted" %in% nms &&
                    length(val <- params[[nms == "retracted"]]) &&
                    !is.null(val$value)
            ) {
                warning(sprintf(
                    paste(
                        "'retracted' is not supported for bridge index node.",
                        "It will be removed."
                    )
                ))
                params <- params[nms != "retracted"]
                nms <- nms[nms != "retracted"]
            }

            is_predefined <- nms %in% predefined
            params_base <- params[is_predefined]
            names_base <- nms[is_predefined]
            params_oth <- params[!is_predefined]
            names_oth <- nms[!is_predefined]

            if (length(not_found <- setdiff(names_oth, FIELDS_FACETS_ALL))) {
                warning(sprintf(
                    paste(
                        "The following facet(s) seems to be invalid or not supported:",
                        "[%s].",
                        "Unexpected response may be returned."
                    ),
                    paste(sprintf("'%s'", not_found), collapse = ", ")
                ))
            }

            if (length(params_oth)) {
                private$parameter$others <- lapply(
                    seq_along(params_oth),
                    function(i) {
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
                private$index_node,
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

            url <- query_build(private$index_node, params)
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
        #' `$collect()` sends the actual query with **`type=Dataset`** to the
        #' ESGF search services and returns the results as an
        #' [EsgResultDataset] object.
        #' The fields included depend on `fields` parameter.
        #' However, the following fields are always included in the results:
        #' `r paste0("\\verb{", EsgResultDataset$private_fields$required_fields, "}", collapse = ", ")`.
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
        #'        constraints explicitly set using `EsgQuery$project()`,
        #'        `EsgQuery$activity_id()`, `EsgQuery$params()` and etc. in the
        #'        returned fields. For example, if you set `$experiment_id("ssp585")`,
        #'        the `experiment_id` field will be included in the results when
        #'        `params = TRUE`. Default: `TRUE`.
        #'
        #' @return An [EsgResultDataset] object.
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
        collect = function(all = FALSE, limit = TRUE, params = TRUE) {
            result <- query_collect(
                private$index_node,
                private$parameter,
                required_fields = EsgResultDataset$private_fields$required_fields,
                all = all,
                limit = limit,
                constraints = params
            )

            # replace docs in the last response
            result$response$response$docs <- result$docs

            # create new results
            new_query_result(EsgResultDataset, private$index_node, private$parameter, result$response)
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
                index_node = private$index_node,
                parameter = private$parameter,
                response = NULL,
                file = file,
                pretty = pretty
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
            q <- query_load(file)

            private$index_node <- q$index_node
            private$parameter <- q$parameter

            self
        },
        # }}}

        # print {{{
        #' @description
        #' Print a summary of the current `EsgQuery` object
        #'
        #' `$print()` gives the summary of current `EsgQuery` object including
        #' the index node URL, the last query timestamp, and all query parameters.
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
            cli::cli_li("Index Node: {private$index_node}")

            cli::cli_h1("<Query Parameter>")
            print_query_params(private$parameter)

            invisible(self)
        }
        # }}}
    ),

    private = list(
        index_node = NULL,

        # all query parameters {{{
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

        format_facet_counts = function(counts) {
            ind <- seq_along(counts)
            nm <- unlst(.subset(counts, ind[ind %% 2L == 1L]))
            value <- unlst(.subset(counts, ind[ind %% 2L == 0L]))
            names(value) <- nm

            value
        },

        new_facet_param = function(facet, value, allow_negate = TRUE, env = parent.frame()) {
            if (allow_negate) {
                # NOTE: We have to force `env` first otherwise R's lazy
                # evaluation feature will cause lexical scoping error
                # Take a look at the following example:
                # q <- 1
                # f1 <- function(x) {
                #     eval(bquote(f2(.(substitute(x)), parent.frame())))
                # }
                # f2 <- function(y, env = parent.frame()) {
                #     print(eval(substitute(y), env))
                # }
                #
                # f1(q) will return `base::q()` instead of 1
                force(env)

                # see: https://stackoverflow.com/questions/75543796/how-to-use-substitute-and-quote-with-nested-functions-in-r
                value <- eval(bquote(eval_with_bang(.(substitute(value)), .env = env)[[1L]]))
            } else {
                value <- list(value = value, negate = FALSE)
            }

            new_query_param(facet, value)
        },

        predefined_facets = function() {
            setdiff(names(private$parameter), c("type", "format", "others"))
        },

        query_listing_cached = function(url, force, type) {
            if (getOption("epwshiftr.cache", TRUE)) {
                cache <- get_cache()
                key <- get_response_cache_key(url)
                cached <- cache$exists(key)
                if (cached) {
                    if (force) {
                        cache$remove(key)
                    } else {
                        verbose(cli::cli_alert_info(paste(
                            "Loaded cached {type} listing for index node {.var {private$index_node}}",
                            "built at {format(cache$get(key)$timestamp, '%F %T %Z')}."
                        )))

                        return(cache$get(key))
                    }
                }
            }

            verbose(cli::cli_progress_step(
                "Retrieving {type} listing for index node {.var {private$index_node}}...",
                paste(
                    "Retrieved {type} listing for index node {.var {private$index_node}}",
                    "at {format(Sys.time(), '%F %T %Z')}."
                ),
                "Failed to retrieve {type} listing for index node {.var {private$index_node}}."
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
# }}}

# normalize_index_node {{{
normalize_index_node <- function(index_node) {
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

    url <- do.call(curl::curl_modify_url, parsed)
    # curl::curl_modify_url always appends a '/' at the end of the URL
    if (is.null(parsed$path) || parsed$path == "/") {
        url <- sub("/$", "", url)
    }
    url
}
# }}}

# get_response_cache_key {{{
get_response_cache_key <- function(url) {
    paste0("response-", fast_hash(url))
}
# }}}

# query_param_encode {{{
query_param_encode <- function(param) {
    # nocov start
    # only for character
    if (!is.character(param)) {
        return(param)
    }
    # nocov end

    # escape encoding if needed
    if (!is.null(attr(param, "encoded", TRUE)) && attr(param, "encoded", TRUE)) {
        return(param)
    }

    # '*', '.', ':', '_', '|', '-' are kept
    reg <- "[^a-zA-Z0-9*.:_|-]"

    vapply(strsplit(param, ""), FUN.VALUE = character(1L), USE.NAMES = FALSE, function(s) {
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
        if (!is.null(params_base[[i]]) && !is.query_param(params_base[[i]])) {
            params_base[[i]] <- new_query_param(names(params_base[i]), params_base[[i]])
        }
    }
    for (i in seq_along(params_other)) {
        if (!is.null(params_other[[i]]) && !is.query_param(params_other[[i]])) {
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
        params <- params[vapply(params, length, integer(1L)) > 0L]
    }

    params
}
# }}}

# query_build {{{
query_build <- function(index_node, params, type = "search") {
    checkmate::assert_choice(type, c("search", "wget"))
    params <- query_param_flat(params)

    if (type == "wget") {
        params <- params[!names(params) %in% c("type", "format")]
    }

    if (!length(params)) {
        return(NULL)
    }

    # NOTE: handle special endpoint for bridge
    if (is_bridge_index_node(index_node)) {
        if (type == "wget") {
            stop("Input index node is a bridge. Wget script is not supported.")
        }
        endpoint <- index_node

        # remove 'retracted' since bridge does not support it
        params <- params[!names(params) %in% "retracted"]

        # remove 'fields=' since bridge does not support it
        if (!is.null(params$fields)) {
            params$fields <- NULL
        }
    } else {
        endpoint <- sprintf("%s/esg-search/%s", index_node, type)
    }

    is_negate <- vapply(params, function(param) param$negate, logical(1L))
    # facet queries without any negated inputs
    if (!is_bridge_index_node(index_node) || !any(is_negate)) {
        facets <- paste(vapply(params, format.EsgQueryParam, FUN.VALUE = ""), collapse = "&")
        return(paste0(endpoint, "?", facets))
    }

    # format free text queries for negated inputs for bridge index node
    # For negated facet inputs, free text queries formatted in Apache Lucene
    # query syntax should be used since bridge does not support negate syntax
    # like 'project!=CMIP6'.
    # see: https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html#free-text-queries
    facets <- paste(vapply(params[!is_negate], format.EsgQueryParam, FUN.VALUE = ""), collapse = "&")
    query <- paste(
        vapply(
            params[is_negate],
            function(param) {
                if (length(param$value) == 1L) {
                    value <- param$value
                } else {
                    value <- sprintf("(%s)", paste(param$value, collapse = " "))
                }
                sprintf("%s:(NOT %s)", param$name, value)
            },
            FUN.VALUE = ""
        ),
        collapse = " AND "
    )

    paste0(
        endpoint,
        "?",
        if (length(facets)) paste0(facets, "&"),
        paste0("query=", query_param_encode(query))
    )
}
# }}}

# query_collect {{{
query_collect <- function(index_node, params, required_fields = NULL, all = FALSE, limit = TRUE, constraints = TRUE) {
    checkmate::assert_flag(all)
    checkmate::assert_flag(constraints)
    checkmate::assert(
        checkmate::check_flag(limit),
        checkmate::check_integerish(limit, lower = 1L, upper = this$data_max_limit, len = 1L)
    )

    params <- query_param_flat(params)
    # include necessary fields
    if (!is.null(params$fields)) {
        if (is_bridge_index_node(index_node)) {
            # bridge index node does not support 'fields='
            params$fields <- NULL
        } else if (!"*" %in% params$fields$value) {
            params$fields <- params$fields$value

            if (!is.null(required_fields)) {
                params$fields <- unique(c(params$fields, required_fields))
            }

            if (constraints) {
                # all non-emtpy param names
                par_nms <- names(params)[!vapply(params, is.null, logical(1L))]
                # exclude keywords
                keywords <- c(
                    "facets",
                    "offset",
                    "limit",
                    "fields",
                    "format",
                    "type",
                    "replica",
                    "latest",
                    "distrib",
                    "shards",
                    "bbox",
                    "start",
                    "end",
                    "from",
                    "to"
                )
                par_nms <- setdiff(par_nms, keywords)
                params$fields <- unique(c(params$fields, par_nms))
            }

            # convert back to a query param in case steps below assume it to be one
            params$fields <- new_query_param("fields", params$fields)
        }
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

    response <- read_json_response(query_build(index_node, params))
    docs <- response$response$docs

    # check if the total number is less that the limit
    total <- response$response$numFound
    if (total > 0L && all) {
        current <- length(response$response$docs[[1]])
        left <- total - current

        while (left > 0L) {
            params$offset <- current

            response <- read_json_response(query_build(index_node, params))

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
query_save <- function(index_node, parameter, response, ..., file = "query.json", pretty = TRUE) {
    checkmate::assert_string(file)
    checkmate::assert_choice(tools::file_ext(file), "json")

    params <- query_param_flat(parameter, empty = TRUE)
    # exclude name
    params <- lapply(params, .subset, c("value", "negate"))

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

    jsonlite::write_json(data, file, null = "null", digits = 6, pretty = pretty)

    normalizePath(file, mustWork = TRUE)
}
# }}}

# query_load {{{
query_load <- function(file, schema = SCHEMA_QUERY) {
    checkmate::assert_file(file, "r", extension = "json")

    # simplifyVector will convert facet counts to characters
    # have to set simplifyMatrix to FALSE
    json <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)

    # validate using predefined schema
    schema_validate(json, schema, name = normalizePath(file, winslash = "/"))

    if (length(json$parameter)) {
        json$parameter <- restore_params(
            input = json$parameter,
            params = EsgQuery$private_fields$parameter
        )
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
restore_params <- function(input, params = EsgQuery$private_fields$parameter) {
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
