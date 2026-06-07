#' @include solr_date.R
NULL

FORMAT_JSON <- "application/solr+json"

# PARAM_BUCKETS {{{
PARAM_BUCKETS_FACET <- c("facet", "control", "others")
PARAM_BUCKETS_QUERY <- c("query")
PARAM_BUCKETS <- c(
    PARAM_BUCKETS_FACET[PARAM_BUCKETS_FACET == "facet"],
    PARAM_BUCKETS_QUERY,
    PARAM_BUCKETS_FACET[PARAM_BUCKETS_FACET != "facet"]
)
# }}}

# FIELDS_FACETS_ALL {{{
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
# }}}

# QueryParam {{{
QueryParam <- S7::new_class("QueryParam", abstract = TRUE)

prop_single_value <- checkmate_property(
    checkmate_any(
        checkmate_rule(S7::class_logical, checkmate::check_flag, branch = "flag"),
        checkmate_rule(S7::class_double, checkmate::check_number, branch = "double"),
        checkmate_rule(S7::class_integer, checkmate::check_number, branch = "integer"),
        checkmate_rule(S7::class_character, checkmate::check_string, min.chars = 1L, branch = "string")
    )
)

prop_atomic_value <- checkmate_property(
    S7::class_atomic,
    checkmate::check_atomic,
    any.missing = FALSE,
    min.len = 1L
)

QueryParamCtrl <- S7::new_class(
    "QueryParamCtrl",
    parent = QueryParam,
    properties = list(value = prop_atomic_value)
)

QueryParamFacet <- S7::new_class(
    "QueryParamFacet",
    parent = QueryParam,
    properties = list(
        value = prop_atomic_value,
        negate = checkmate_property(S7::class_logical, checkmate::check_flag, default = FALSE),
        encoded = checkmate_property(S7::class_logical, checkmate::check_flag, default = FALSE)
    )
)

QueryParamDate <- S7::new_class(
    "QueryParamDate",
    parent = QueryParam,
    properties = list(value = S7::new_property(SolrDate)),
    constructor = function(value) {
        value <- solr_date(value)
        S7::new_object(S7::S7_object(), value = value)
    }
)

# render {{{
render <- S7::new_generic("render", "x", function(x, name, ...) {
    checkmate::assert_string(name, null.ok = TRUE)
    S7::S7_dispatch()
})
S7::method(render, QueryParamFacet) <- function(x, name, ..., encode = FALSE, space = FALSE) {
    checkmate::assert_flag(encode)
    checkmate::assert_flag(space)
    value <- x@value

    if (is.logical(value)) {
        res <- tolower(if (x@negate) !value else value)
    } else if (is.numeric(value)) {
        res <- as.character(value)
    } else if (encode && !x@encoded) {
        # '*', '.', ':', '_', '|', '-' are kept
        reg <- "[^a-zA-Z0-9*.:_|-]"

        res <- vapply(strsplit(value, ""), FUN.VALUE = character(1L), USE.NAMES = FALSE, function(s) {
            ind <- grep(reg, s)
            if (length(ind)) {
                esc <- vapply(
                    s[ind],
                    function(char) paste0("%", toupper(as.character(charToRaw(char))), collapse = ""),
                    character(1L)
                )
                s[ind] <- esc
            }

            paste(s, collapse = "")
        })
    } else {
        res <- value
    }

    spc <- if (space) " " else ""
    equal <- if (x@negate && !is.logical(value)) "!=" else "="
    equal <- paste0(spc, equal, spc)

    if (x@negate && !is.logical(value)) {
        paste0(name, equal, res, collapse = paste0(spc, "&", spc))
    } else {
        paste0(name, equal, paste0(res, collapse = paste0(",", spc)))
    }
}
S7::method(render, QueryParamDate) <- function(x, name, ..., as = "iso") {
    checkmate::assert_string(as)
    paste0(name, ":", format(x@value, as = as))
}
S7::method(render, QueryParamCtrl) <- function(x, name, ...) {
    value <- x@value
    if (is.logical(value)) {
        res <- tolower(value)
    } else {
        res <- as.character(value)
    }
    paste0(name, "=", paste0(res, collapse = ","))
}
# }}}

# as.list {{{
S7::method(as.list, QueryParamFacet) <- function(x) {
    list(
        value = x@value,
        negate = x@negate,
        encoded = x@encoded
    )
}
S7::method(as.list, QueryParamDate) <- function(x) {
    list(value = format(x@value, as = "iso"))
}
S7::method(as.list, QueryParamCtrl) <- function(x) {
    list(value = x@value)
}
# }}}

# print {{{
S7::method(print, QueryParam) <- function(x) {
    rendered <- render(x, name = NULL)
    if (S7::S7_inherits(x, QueryParamDate)) {
        # remove the leading ":"
        rendered <- substring(rendered, 2L)
    }
    cat(rendered, "\n", sep = "")
}
# }}}
# }}}

# QueryParam helpers {{{
query_param_names <- function(class = c("facet", "query", "control", "all")) {
    class <- match.arg(class)
    facet <- c(
        "project",
        "activity_id",
        "experiment_id",
        "source_id",
        "variable_id",
        "frequency",
        "variant_label",
        "nominal_resolution",
        "data_node",
        "facets",
        "fields",
        "shards"
    )
    query <- c(
        "datetime_start",
        "datetime_stop",
        "timestamp_from",
        "timestamp_to",
        "_timestamp",
        "version_min",
        "version_max"
    )
    control <- c("replica", "latest", "type", "offset", "distrib", "limit", "format")

    switch(
        class,
        facet = facet,
        query = query,
        control = control,
        all = c(facet, query, control)
    )
}

query_param_store <- function() {
    QueryParamStore$new()
}

query_param_meta <- function(x, name = NULL, kind = NULL) {
    if (!is.null(name)) {
        attr(x, "name") <- name
    }
    if (!is.null(kind)) {
        attr(x, "kind") <- kind
    }
    x
}

query_param_spec <- function(name) {
    checkmate::assert_string(name, min.chars = 1L)

    class <- if (name %in% query_param_names("query")) {
        "query"
    } else if (name %in% query_param_names("control")) {
        "control"
    } else {
        "facet"
    }

    list(name = name, class = class)
}

query_param_kind <- function(x) {
    kind <- attr(x, "kind", exact = TRUE)
    if (!is.null(kind)) {
        return(kind)
    }

    name <- query_param_name(x)
    if (!is.null(name)) {
        spec <- query_param_spec(name)
        return(if (identical(spec$class, "query")) name else spec$class)
    }
    if (S7::S7_inherits(x, QueryParamDate)) {
        return("query")
    }
    if (S7::S7_inherits(x, QueryParamCtrl)) {
        return("control")
    }
    "facet"
}

query_param_name <- function(x) {
    attr(x, "name", exact = TRUE)
}

query_param_value <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }

    x@value
}

query_param_negate <- function(x) {
    if (is.null(x) || !S7::S7_inherits(x, QueryParamFacet)) {
        return(FALSE)
    }

    x@negate
}

as_query_param <- function(name, value, negate = FALSE) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_flag(negate)

    if (is.null(value)) {
        return(NULL)
    }
    if (S7::S7_inherits(value, QueryParam)) {
        return(query_param_meta(value, name = name, kind = query_param_kind(value)))
    }
    if (is.list(value) && all(c("value", "negate") %in% names(value))) {
        negate <- isTRUE(value$negate)
        value <- value$value
    }
    if (is.null(value)) {
        return(NULL)
    }

    spec <- query_param_spec(name)
    param <- switch(
        spec$class,
        query = QueryParamDate(value),
        control = QueryParamCtrl(value),
        facet = QueryParamFacet(value, negate = negate)
    )

    query_param_meta(param, name = name, kind = if (spec$class == "query") name else spec$class)
}

query_param_render <- function(x, name = query_param_name(x), ...) {
    kind <- query_param_kind(x)
    if (kind %in% c("version_min", "version_max")) {
        return(render(x, name = "version", as = "num"))
    }
    if (identical(kind, "timestamp_range")) {
        return(render(x, name = "_timestamp", ...))
    }

    if (S7::S7_inherits(x, QueryParamFacet)) {
        return(render(x, name = name, encode = TRUE, ...))
    }

    render(x, name = name, ...)
}

query_param_encode <- function(x) {
    curl::curl_escape(x)
}

query_param_flat <- function(params, null = FALSE) {
    if (inherits(params, "QueryParamStore")) {
        return(params$flat(null = null))
    }

    checkmate::assert_list(params, names = "unique")
    checkmate::assert_flag(null)

    out <- list()
    for (name in names(params)) {
        value <- params[[name]]
        if (identical(name, "others")) {
            if (length(value)) {
                for (other_name in names(value)) {
                    param <- as_query_param(other_name, value[[other_name]])
                    if (is.null(param) && null) {
                        out[other_name] <- list(NULL)
                    } else if (!is.null(param)) {
                        out[[other_name]] <- param
                    }
                }
            }
            next
        }

        param <- as_query_param(name, value)
        if (is.null(param) && null) {
            out[name] <- list(NULL)
        } else if (!is.null(param)) {
            out[[name]] <- param
        }
    }

    out
}

query_params_flat <- query_param_flat

query_param_as_store <- function(params) {
    if (inherits(params, "QueryParamStore")) {
        return(params)
    }

    store <- QueryParamStore$new()
    if (is.null(params)) {
        return(store)
    }

    checkmate::assert_list(params, names = "named")
    buckets <- stats::setNames(rep(list(list()), length(PARAM_BUCKETS)), PARAM_BUCKETS)

    add_param <- function(name, value) {
        if (!name %in% query_param_names("all")) {
            buckets$others[[name]] <<- if (is.null(value) || S7::S7_inherits(value, QueryParam)) {
                value
            } else {
                as_query_param(name, value)
            }
            return(invisible())
        }

        spec <- query_param_spec(name)
        bucket <- spec$class
        if (identical(name, "_timestamp")) {
            bucket <- "query"
        }

        buckets[[bucket]][[name]] <<- if (is.null(value) || S7::S7_inherits(value, QueryParam)) {
            value
        } else {
            as_query_param(name, value)
        }
    }

    for (name in names(params)) {
        if (identical(name, "others")) {
            if (!length(params$others)) {
                next
            }
            for (other_name in names(params$others)) {
                value <- params$others[[other_name]]
                add_param(other_name, value)
            }
            next
        }

        add_param(name, params[[name]])
    }

    store$restore(buckets)
}

query_param_clone <- function(params) {
    query_param_as_store(params)$copy()
}

print_query_params <- function(params) {
    store <- query_param_as_store(params)
    rendered <- store$render()
    if (!length(rendered)) {
        cli::cli_bullets(c(" " = "<Empty>"))
        return(invisible(params))
    }

    for (line in rendered) {
        cli::cli_bullets(c("*" = line))
    }

    invisible(params)
}
# }}}

# QueryParamStore {{{
#' Internal query parameter store
#'
#' @description
#' `QueryParamStore` is the internal [R6][R6::R6Class] used by [EsgQuery] to
#' collect, validate, mutate, serialize, and render ESGF query parameters
#' before requests are sent to the search service.
#'
#' Internally, the store keeps parameters in four buckets:
#'
#' - `facet`: predefined facet-style parameters rendered as standard
#'   `name=value` query components.
#' - `query`: structured Solr query constraints rendered into the `query=`
#'   parameter.
#' - `control`: scalar control parameters such as pagination, distribution,
#'   record type, and response format.
#' - `others`: user-supplied ad hoc parameters that do not have dedicated
#'   setter methods.
#'
#' # `QueryParamStore` object
#'
#' `QueryParamStore$new()` returns an internal `QueryParamStore` object whose
#' methods can be broadly grouped into the following categories:
#'
#' - **Parameter getter & setter**: methods such as `$project()`, `$limit()`,
#'   `$params()`, and related helpers for updating the stored query state.
#' - **Structured query constraints**: helpers such as `$datetime_range()`,
#'   `$timestamp_range()`, and `$version_range()` that build Solr-compatible
#'   range conditions.
#' - **State persistence**: `$state()`, `$serialize()`, and `$restore()` for
#'   inspecting or round-tripping the current parameter state.
#' - **Rendering and display**: `$render()` and `$print()` for converting the
#'   stored state into query fragments or human-readable summaries.
#'
#' The object stores values as `QueryParam` subclasses (`QueryParamFacet`,
#' `QueryParamCtrl`, and `QueryParamDate`) so parameter metadata, negation
#' state, and rendering behavior can be handled consistently.
#'
#' @author Hongyuan Jia
#' @name QueryParamStore
#' @keywords internal
#' @noRd
QueryParamStore <- R6::R6Class(
    "QueryParamStore",

    public = list(
        # initialize {{{
        #' @description
        #' Create a new `QueryParamStore` object.
        #'
        #' @return A new `QueryParamStore` object.
        #'
        #' @examples
        #' \dontrun{
        #' q <- QueryParamStore$new()
        #' }
        initialize = function() {
            private$init_params()
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
                return(private$get_or_set_facet("project"))
            }

            private$get_or_set_facet("project", value, allow_negate = TRUE, env = parent.frame())
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
            private$get_or_set_facet("activity_id", value, allow_negate = TRUE, env = parent.frame())
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
            private$get_or_set_facet("experiment_id", value, allow_negate = TRUE, env = parent.frame())
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
            private$get_or_set_facet("source_id", value, allow_negate = TRUE, env = parent.frame())
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
            private$get_or_set_facet("variable_id", value, allow_negate = TRUE, env = parent.frame())
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
            private$get_or_set_facet("frequency", value, allow_negate = TRUE, env = parent.frame())
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
            private$get_or_set_facet("variant_label", value, allow_negate = TRUE, env = parent.frame())
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
                return(private$get_or_set_facet("nominal_resolution"))
            }

            private$get_or_set_facet("nominal_resolution", value, allow_negate = TRUE, env = parent.frame())
            param <- private$get_or_set_facet("nominal_resolution")
            if (is.null(param)) {
                return(self)
            }

            values <- param@value
            if (!is.null(values)) {
                # handle nominal resolution specially
                # there are some GCMs that mistakenly set '100 km' to '100km'
                if ("100 km" %in% values && !"100km" %in% values) {
                    values <- c(values, "100km")
                }

                # ESGF uses '+' for spaces in nominal resolution
                values <- gsub(" ", "+", values, fixed = TRUE)
                attr(values, "encoded") <- NULL
                param@value <- values

                # explictly mark it as realdy encoded, otherwise '+' will not be
                # perserved
                param@encoded <- TRUE
            }

            private$set_param_value("facet", "nominal_resolution", param)

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
            private$get_or_set_facet("data_node", value, allow_negate = TRUE, env = parent.frame())
        },
        # }}}

        # facets {{{
        #' @description
        #' Get or set the `facets` parameter for facet counting query.
        #'
        #' Note that `$facets()` only affects
        #' \href{#method-QueryParamStore-count}{\code{$count()}}
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
            private$get_or_set_facet("facets", value, allow_negate = FALSE, env = parent.frame())
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
                return(private$get_or_set_facet("fields"))
            }

            private$get_or_set_facet("fields", value, allow_negate = FALSE, env = parent.frame())
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
                return(private$get_or_set_facet("shards"))
            }

            distrib <- private$get_or_set_control("distrib")
            if (!is.null(distrib) && !distrib@value && !is.null(value)) {
                stop("'$distrib()' returns FALSE. Shard specification is only applicable for distributed queries.")
            }
            private$get_or_set_facet("shards", value, allow_negate = FALSE, env = parent.frame())
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
            private$get_or_set_control("replica", value, type = "flag")
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
                return(private$get_or_set_control("latest"))
            }

            private$get_or_set_control("latest", value, type = "flag")
        },
        # }}}

        # type {{{
        #' @description
        #' Get or set the `type` parameter.
        #'
        #' `$type()` controls which ESGF record type the query targets.
        #' Supported values are `"Dataset"`, `"File"`, and `"Aggregation"`.
        #'
        #' @param value
        #' `r rd_query_method_param("type", "string", default = "Dataset", nullable = FALSE)`
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$type()
        #'
        #' # set the parameter
        #' q$type("File")
        #' }
        type = function(value = "Dataset") {
            if (missing(value)) {
                return(private$get_or_set_control("type"))
            }

            private$get_or_set_control(
                "type",
                value,
                type = "choice",
                choices = c("Dataset", "File", "Aggregation")
            )
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
                return(private$get_or_set_control("limit"))
            }

            checkmate::assert_count(value, .var.name = "limit")
            if (value > this$data_max_limit) {
                warning(sprintf(
                    paste(
                        "ESGF Search API only supports a maximum value of 'limit' <= %s.",
                        "'limit' will be reset to %s."
                    ),
                    format(this$data_max_limit, big.mark = ","),
                    format(this$data_max_limit, big.mark = ",")
                ))
                value <- this$data_max_limit
            }
            private$get_or_set_control("limit", value, type = "count")
        },
        # }}}

        # offset {{{
        #' @description
        #' Get or set the `offset` parameter.
        #'
        #' If the query returns records that exceed the
        #' \href{#method-QueryParamStore-limit}{\code{limit}} number,
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
                return(private$get_or_set_control("offset"))
            }

            private$get_or_set_control("offset", value, type = "count")
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
                return(private$get_or_set_control("distrib"))
            }

            private$get_or_set_control("distrib", value, type = "flag")
        },
        # }}}

        # format {{{
        #' @description
        #' Get or set the `format` parameter.
        #'
        #' Only JSON responses are currently supported. If a value other than
        #' `r FORMAT_JSON` is supplied, a warning is issued and the stored value
        #' is reset to `r FORMAT_JSON`.
        #'
        #' @param value
        #' The response format string. If not given, current value is returned.
        #'
        #' @return
        #' `r rd_query_method_return()`
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$format()
        #'
        #' # set the parameter
        #' q$format("application/solr+json")
        #'
        #' # unsupported formats are reset to JSON with a warning
        #' q$format("application/xml")
        #' }
        format = function(value = FORMAT_JSON) {
            if (missing(value)) {
                return(private$get_or_set_control("format"))
            }

            if (!is.null(value) && !identical(value, FORMAT_JSON)) {
                warning(sprintf(
                    paste(
                        "Only JSON response format is supported.",
                        "'format' will be reset to '%s'."
                    ),
                    FORMAT_JSON
                ))
                value <- FORMAT_JSON
            }

            private$get_or_set_control("format", value, type = "string")
        },
        # }}}

        # params {{{
        #' @description
        #' Get or set other parameters.
        #'
        #' `$params()` can be used to specify other parameters that do not have
        #' a dedicated method, e.g. `master_id`, etc. It can also be
        #' used to overwrite existing parameter values specified using methods
        #' like \href{#method-QueryParamStore-activity_id}{\code{$activity_id()}}.
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
        #' - If parameters are specified, the modified `QueryParamStore` object.
        #'
        #' - Otherwise, an empty list for `$params(NULL)` or a list of
        #'   `QueryParam` objects.
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
            private$get_or_set_others(...)
        },
        # }}}

        # datetime_range {{{
        #' @description
        #' Get or set the temporal coverage range for data search.
        #'
        #' `$datetime_range()` constrains the search to datasets whose temporal
        #' coverage overlaps the specified range. It uses an interval-overlap
        #' logic consistent with the ESG search server's `start`/`stop` parameters:
        #'
        #' - `start` constrains `datetime_start:[* TO start]`: datasets whose
        #'   start time is no later than the specified `start`.
        #' - `stop` constrains `datetime_stop:[stop TO *]`: datasets whose stop
        #'   time is no earlier than the specified `stop`.
        #'
        #' Both conditions are placed into the Solr `query=` parameter.
        #'
        #' @param start,stop A single string specifying the temporal boundary.
        #'   Supported inputs include complete ISO 8601 datetimes
        #'   (`yyyy-MM-ddTHH:mm:ssZ`), simplified date strings (e.g. `"2020"`,
        #'   `"2020-06"`, `"2020-06-15"`), Solr Date Math expressions
        #'   (e.g. `NOW-1YEAR`), and complete Solr range expressions
        #'   (e.g. `[2020-01-01T00:00:00Z TO 2025-01-01T00:00:00Z]`).
        #'   Simplified date strings are normalized to full UTC instants before
        #'   being added to the query.
        #'
        #' @return
        #' If `start` and `stop` are omitted, a list with elements `start` and
        #' `stop`, each either `NULL` or a length-1 `SolrDateTime` object.
        #' Otherwise, the modified `QueryParamStore` object, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$datetime_range()
        #'
        #' # datasets overlapping 2017-2025
        #' q$datetime_range(
        #'     start = "2017-01-01T00:00:00Z",
        #'     stop  = "2025-01-01T00:00:00Z"
        #' )
        #'
        #' # using Solr Date Math
        #' q$datetime_range(start = "NOW-10YEARS")
        #'
        #' # using a simplified year input
        #' q$datetime_range(start = "2017")
        #'
        #' # using a complete range expression for start
        #' q$datetime_range(start = "[2017-01-01T00:00:00Z TO 2020-01-01T00:00:00Z]")
        #'
        #' # remove the parameter
        #' q$datetime_range(start = NULL, stop = NULL)
        #' }
        datetime_range = function(start, stop) {
            if (missing(start) && missing(stop)) {
                return(list(
                    start = private$query$datetime_start,
                    stop = private$query$datetime_stop
                ))
            }

            ensure_bound <- function(name, value) {
                if (is.null(value)) {
                    return(NULL)
                }

                bound <- QueryParamDate(value)

                # reset to NULL if "*"
                if (S7::S7_inherits(bound@value, SolrDateUnbounded)) {
                    bound <- NULL
                } else if (S7::S7_inherits(bound@value, SolrDatePoint)) {
                    bound@value <- if (name == "start") {
                        SolrDateRange(start = SolrDateUnbounded(), end = bound@value)
                    } else {
                        SolrDateRange(start = bound@value, end = SolrDateUnbounded())
                    }
                }

                bound
            }

            if (!missing(start)) {
                private$set_param_value("query", "datetime_start", ensure_bound("start", start))
            }

            if (!missing(stop)) {
                private$set_param_value("query", "datetime_stop", ensure_bound("stop", stop))
            }

            self
        },
        # }}}

        # timestamp_range {{{
        #' @description
        #' Get or set the index timestamp range for data search.
        #'
        #' `$timestamp_range()` constrains the search to datasets whose Solr
        #' index timestamp (`_timestamp`) falls within the specified range.
        #' Both `from` and `to` are combined into a single range condition
        #' `_timestamp:[from TO to]` placed in the Solr `query=` parameter.
        #' If only one boundary is given, the other defaults to `*` (unbounded).
        #'
        #' @param from,to A single string specifying the timestamp boundary.
        #'   Supported inputs include complete ISO 8601 datetimes
        #'   (`yyyy-MM-ddTHH:mm:ssZ`), simplified date strings (e.g. `"2020"`,
        #'   `"2020-06"`, `"2020-06-15"`), and Solr Date Math expressions
        #'   (e.g. `NOW-1YEAR`). Simplified date strings are normalized to full
        #'   UTC instants before being added to the query. Solr range syntax is
        #'   not accepted here; use `from` and `to` separately.
        #'
        #' @return
        #' If `from` and `to` are omitted, either `NULL` or a length-1
        #' `SolrDateTime` range object. Otherwise, the modified `QueryParamStore`
        #' object, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$timestamp_range()
        #'
        #' # datasets indexed between 2020 and 2021
        #' q$timestamp_range(
        #'     from = "2020-01-01T00:00:00Z",
        #'     to   = "2021-01-01T00:00:00Z"
        #' )
        #'
        #' # datasets indexed in the past year
        #' q$timestamp_range(from = "NOW-1YEAR")
        #'
        #' # using a simplified year input
        #' q$timestamp_range(from = "2020")
        #'
        #' # remove the parameter
        #' q$timestamp_range(from = NULL, to = NULL)
        #' }
        timestamp_range = function(from, to) {
            if (missing(from) && missing(to)) {
                return(list(
                    from = private$query$timestamp_from,
                    to = private$query$timestamp_to
                ))
            }

            ensure_bound <- function(name, value) {
                if (is.null(value)) {
                    return(NULL)
                }

                bound <- QueryParamDate(value)

                if (S7::S7_inherits(bound@value, SolrDateRange)) {
                    stop(sprintf(
                        paste(
                            "`%s` does not support range syntax like '[... TO ...]'.",
                            "Provide a single datetime value instead."
                        ),
                        name
                    ))
                }

                # reset to NULL if "*"
                if (S7::S7_inherits(bound@value, SolrDateUnbounded)) {
                    bound <- NULL
                }

                bound
            }

            if (!missing(from)) {
                private$set_param_value("query", "timestamp_from", ensure_bound("from", from))
            }

            if (!missing(to)) {
                private$set_param_value("query", "timestamp_to", ensure_bound("to", to))
            }

            self
        },
        # }}}

        # version_range {{{
        #' @description
        #' Get or set the version range for data search.
        #'
        #' `$version_range()` constrains the search to datasets whose `version`
        #' field (a string field using lexicographic ordering) falls within the
        #' specified range. Each boundary generates an independent Solr range
        #' condition placed in the `query=` parameter:
        #'
        #' - `min` → `version:[min TO *]`
        #' - `max` → `version:[* TO max]`
        #'
        #' Because `version` is a **string** field, lexicographic comparison is
        #' used. ESGF stores versions in `YYYYMMDD` format, so lexicographic
        #' order matches chronological order for well-formed values.
        #'
        #' Solr Date Math syntax (e.g. `NOW-1YEAR`) is **not** supported because
        #' `version` is not a date field.
        #'
        #' Range expressions (e.g. `[2020 TO 2025]`) are **not** accepted as
        #' input; use the `min` and `max` parameters separately instead.
        #'
        #' @param min,max A single string specifying the version boundary,
        #'   e.g. `"20200101"`, `"2020"`, or `"2020-06"`. Simplified date
        #'   strings are normalized to `YYYYMMDD`. Inputs must **not** contain
        #'   Solr range syntax (`[... TO ...]`) or Solr Date Math expressions.
        #'
        #' @return
        #' If `min` and `max` are omitted, a list with elements `min` and
        #' `max`, each either `NULL` or a length-1 `SolrDateTime` object.
        #' Otherwise, the modified `QueryParamStore` object, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # get current value
        #' q$version_range()
        #'
        #' # datasets with version >= 20200101
        #' q$version_range(min = "20200101")
        #'
        #' # simplified inputs are normalized to YYYYMMDD
        #' q$version_range(min = "2020", max = "2020-06")
        #'
        #' # datasets with version in [20200101, 20211231]
        #' q$version_range(min = "20200101", max = "20211231")
        #'
        #' # remove the parameter
        #' q$version_range(min = NULL, max = NULL)
        #' }
        version_range = function(min, max) {
            if (missing(min) && missing(max)) {
                return(list(
                    min = private$query$version_min,
                    max = private$query$version_max
                ))
            }

            # helper: parse and validate a version boundary string.
            # Accepts dates (incl. simplified) and converts to YYYYMMDD.
            # Rejects Range syntax and Date Math expressions.
            ensure_bound <- function(name, value) {
                if (is.null(value)) {
                    return(NULL)
                }

                bound <- QueryParamDate(value)

                if (S7::S7_inherits(bound@value, SolrDateRange)) {
                    stop(sprintf(
                        paste(
                            "`%s` must not contain range syntax like '[... TO ...]'.",
                            "Use the 'min' and 'max' parameters separately to specify a range, ",
                            "e.g. version_range(min = '20200101', max = '20211231')."
                        ),
                        name
                    ))
                }

                if (S7::S7_inherits(bound@value, SolrDateMath)) {
                    stop(sprintf(
                        paste(
                            "'%s' does not support Solr Date Math expressions (e.g. 'NOW-1YEAR'). ",
                            "The 'version' field is a string type and does not support Date Math."
                        ),
                        name
                    ))
                }

                if (S7::S7_inherits(bound@value, SolrDateUnbounded)) {
                    bound <- NULL
                } else {
                    bound@value <- if (name == "min") {
                        SolrDateRange(start = bound@value, end = SolrDateUnbounded())
                    } else {
                        SolrDateRange(start = SolrDateUnbounded(), end = bound@value)
                    }
                }

                bound
            }

            if (!missing(min)) {
                private$set_param_value("query", "version_min", ensure_bound("min", min))
            }

            if (!missing(max)) {
                private$set_param_value("query", "version_max", ensure_bound("max", max))
            }

            invisible(self)
        },
        # }}}

        # render {{{
        #' @description
        #' Render current query parameters as URL query components.
        #'
        #' `$render()` converts the currently stored parameters into a character
        #' vector of rendered query fragments such as `"project=CMIP6"`. Use
        #' `name` to render only a subset of parameters.
        #'
        #' For convenience, requesting `"_timestamp"` expands to
        #' `"timestamp_from"` and `"timestamp_to"`, and requesting
        #' `"version"` expands to `"version_min"` and `"version_max"`.
        #'
        #' @param name A character vector of parameter names to render, or
        #'   `NULL` to render all currently stored parameters.
        #'
        #' @return A character vector of rendered query components.
        #'
        #' @examples
        #' \dontrun{
        #' q$project("CMIP6")$limit(5L)
        #'
        #' # render all current parameters
        #' q$render()
        #'
        #' # render only selected parameters
        #' q$render(c("project", "limit"))
        #' }
        render = function(name = NULL) {
            if (!is.null(name)) {
                checkmate::assert_character(name, any.missing = FALSE, unique = TRUE)

                idx_time <- match("_timestamp", name, nomatch = 0L)
                if (idx_time > 0L) {
                    name <- c(
                        name[seq_len(idx_time - 1L)],
                        "timestamp_from",
                        "timestamp_to",
                        name[-seq_len(idx_time)]
                    )
                }

                idx_ver <- match("version", name, nomatch = 0L)
                if (idx_ver > 0L) {
                    name <- c(
                        name[seq_len(idx_ver - 1L)],
                        "version_min",
                        "version_max",
                        name[-seq_len(idx_ver)]
                    )
                }

                # in case that the original 'name' contains `timestamp_*` or `version_*`
                name <- unique(name)
            }

            params <- private$subset_params(name = name, null = TRUE, flat = FALSE)

            bucket_facet <- names(params)[names(params) %in% PARAM_BUCKETS_FACET]
            bucket_query <- names(params)[names(params) %in% PARAM_BUCKETS_QUERY]

            rendered <- c()
            if (length(bucket_facet)) {
                for (bucket in bucket_facet) {
                    rendered <- c(rendered, private$render_facet(names(params[[bucket]])))
                }
            }
            if (length(bucket_query)) {
                for (bucket in bucket_query) {
                    rendered <- c(rendered, private$render_query(names(params[[bucket]])))
                }
            }

            rendered
        },
        # }}}

        # state {{{
        #' @description
        #' Get the current parameter state.
        #'
        #' `$state()` returns the current query parameters grouped by internal
        #' parameter bucket (`facet`, `query`, `control`, and `others`).
        #'
        #' @param name A character vector of parameter names to include, or
        #'   `NULL` to include all parameters.
        #' @param null If `TRUE`, include parameters whose current value is
        #'   `NULL`. Otherwise, omit unset parameters.
        #'
        #' @return A named list of parameter buckets containing `QueryParam`
        #'   objects and, when `null = TRUE`, `NULL` entries.
        #'
        #' @examples
        #' \dontrun{
        #' # get non-null parameters only
        #' q$state()
        #'
        #' # include unset parameters too
        #' q$state(null = TRUE)
        #' }
        state = function(name = NULL, null = FALSE) {
            private$subset_params(name = name, null = null, flat = FALSE)
        },
        # }}}

        # flat {{{
        #' @description
        #' Get the current parameter state as a flat named list.
        #'
        #' `$flat()` is mainly used by request builders that need to inspect or
        #' temporarily adjust individual parameters regardless of their internal
        #' bucket.
        #'
        #' @param name A character vector of parameter names to include, or
        #'   `NULL` to include all parameters.
        #' @param null If `TRUE`, include parameters whose current value is
        #'   `NULL`. Otherwise, omit unset parameters.
        #'
        #' @return A flat named list of `QueryParam` objects.
        flat = function(name = NULL, null = FALSE) {
            private$subset_params(name = name, null = null, flat = TRUE)
        },
        # }}}

        # copy {{{
        #' @description
        #' Create an independent copy of the current parameter store.
        #'
        #' @return A new `QueryParamStore` object with the same parameter state.
        copy = function() {
            QueryParamStore$new()$restore(self$serialize(null = TRUE))
        },
        # }}}

        # serialize {{{
        #' @description
        #' Serialize the current parameter state.
        #'
        #' `$serialize()` converts the current state into either a nested list or
        #' a JSON string that can later be passed to
        #' \\href{#method-QueryParamStore-restore}{\\code{$restore()}}.
        #'
        #' @param name A character vector of parameter names to include, or
        #'   `NULL` to include all parameters.
        #' @param null If `TRUE`, include parameters whose current value is
        #'   `NULL`. Otherwise, omit unset parameters.
        #' @param type Output type. Either `"list"` or `"json"`.
        #'
        #' @return Either a nested list or a JSON string, depending on `type`.
        #'
        #' @examples
        #' \dontrun{
        #' q$serialize()
        #' q$serialize(type = "json")
        #' }
        serialize = function(name = NULL, null = FALSE, type = c("list", "json")) {
            out <- rep(list(list()), length(PARAM_BUCKETS))
            names(out) <- PARAM_BUCKETS

            subsetted <- lapply(
                self$state(name = name, null = null),
                function(bucket) {
                    lapply(
                        bucket,
                        private$serialize_param
                    )
                }
            )
            out <- modifyList(out, subsetted)

            type <- match.arg(type)
            if (type == "json") {
                jsonlite::toJSON(out, pretty = TRUE, null = "null", auto_unbox = TRUE, na = "string")
            } else {
                out
            }
        },
        # }}}

        # restore {{{
        #' @description
        #' Restore a previously saved parameter state.
        #'
        #' `$restore()` resets the current store and rebuilds it from a state
        #' object such as one returned by
        #' \\href{#method-QueryParamStore-state}{\\code{$state(null = TRUE)}} or
        #' \\href{#method-QueryParamStore-serialize}{\\code{$serialize(type = "list")}}.
        #' If restoration fails, the original state is restored before the error
        #' is re-thrown.
        #'
        #' @param state A named list describing the parameter state to restore.
        #'
        #' @return The modified `QueryParamStore` object, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' state <- q$serialize(type = "list")
        #' q$restore(state)
        #' }
        restore = function(state) {
            checkmate::assert_list(state, names = "named")
            # store current state in a temp variable and roll back if restore fails at any point
            original <- self$state(name = NULL, null = TRUE)

            tryCatch(
                {
                    private$init_params()
                    for (bucket_name in intersect(names(state), PARAM_BUCKETS)) {
                        private$restore_bucket(bucket_name, state[[bucket_name]])
                    }
                },
                error = function(e) {
                    for (bucket in PARAM_BUCKETS) {
                        private[[bucket]] <- original[[bucket]]
                    }
                    stop(e)
                }
            )

            invisible(self)
        },
        # }}}

        # print {{{
        #' @description
        #' Print a summary of the current `QueryParamStore` object
        #'
        #' `$print()` gives the summary of current `QueryParamStore` object and
        #' all current query parameters.
        #'
        #' @return The `QueryParamStore` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' q$print()
        #' }
        print = function() {
            d <- cli::cli_div(
                theme = list(rule = list("line-type" = "double"))
            )
            cli::cli_h1("<Query Parameter>")
            for (line in self$render()) {
                cli::cli_bullets(c("*" = line))
            }

            invisible(self)
        }
        # }}}
    ),

    # private {{{
    private = list(
        # params {{{
        # facet: predefined facet-style parameters rendered as standard
        # `name=value` query components.
        facet = list(
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
            shards = NULL
        ),
        # query: structured free-text query constraints rendered into the Solr
        # `query=` parameter rather than plain `name=value` pairs.
        query = list(
            # temporal and version range components
            datetime_start = NULL,
            datetime_stop = NULL,
            timestamp_from = NULL,
            timestamp_to = NULL,
            version_min = NULL,
            version_max = NULL
        ),
        # control: scalar control parameters that affect result shape,
        # pagination, distribution, or transport format.
        control = list(
            replica = NULL,
            latest = TRUE,
            type = "Dataset",
            offset = 0L,
            distrib = TRUE,
            limit = 10L,
            format = "application/solr+json"
        ),
        # others: user-supplied ad hoc parameters without dedicated methods.
        others = list(),
        # }}}

        # init_params {{{
        # Initialize all parameter buckets from class defaults, coercing
        # non-NULL defaults into the corresponding QueryParam subclasses.
        init_params = function() {
            private$facet <- stats::setNames(
                lapply(names(QueryParamStore$private_fields$facet), function(name) {
                    as_query_param(name, QueryParamStore$private_fields$facet[[name]])
                }),
                names(QueryParamStore$private_fields$facet)
            )

            private$query <- stats::setNames(
                lapply(names(QueryParamStore$private_fields$query), function(name) {
                    as_query_param(name, QueryParamStore$private_fields$query[[name]])
                }),
                names(QueryParamStore$private_fields$query)
            )

            private$control <- stats::setNames(
                lapply(names(QueryParamStore$private_fields$control), function(name) {
                    as_query_param(name, QueryParamStore$private_fields$control[[name]])
                }),
                names(QueryParamStore$private_fields$control)
            )

            private$others <- list()
        },
        # }}}

        # subset_params {{{
        # Select parameters from one or more buckets, optionally filtering by
        # parameter name, keeping NULL entries, and/or flattening the result.
        subset_params = function(name = NULL, null = FALSE, flat = TRUE, bucket = NULL) {
            checkmate::assert_character(name, null.ok = TRUE, any.missing = FALSE, unique = TRUE)
            checkmate::assert_character(bucket, null.ok = TRUE, any.missing = FALSE, unique = TRUE)
            checkmate::assert_subset(bucket, PARAM_BUCKETS, empty.ok = TRUE)
            checkmate::assert_flag(null)

            if (is.null(bucket)) {
                bucket <- PARAM_BUCKETS
            }
            buckets <- lapply(bucket, function(b) private[[b]])
            names(buckets) <- bucket

            bucket_of <- stats::setNames(
                rep(names(buckets), lengths(buckets)),
                unlist(lapply(buckets, names), use.names = FALSE)
            )

            if (anyDuplicated(names(bucket_of))) {
                stop("Internal error: Duplicate parameter names found across buckets.")
            }

            if (is.null(name)) {
                selected <- buckets
            } else {
                checkmate::assert_subset(name, names(bucket_of))

                selected_grouped <- split.default(
                    names(bucket_of[name]),
                    factor(bucket_of[name], unique(bucket_of[name]))
                )
                selected <- lapply(
                    stats::setNames(seq_along(selected_grouped), names(selected_grouped)),
                    function(i) {
                        buckets[[names(selected_grouped)[[i]]]][selected_grouped[[i]]]
                    }
                )
            }

            if (!null) {
                selected <- lapply(selected, function(sel) {
                    sel[!vapply(sel, function(x) is.null(x), logical(1L))]
                })
                selected <- selected[lengths(selected) > 0L]
            }

            if (flat) {
                selected <- unlist(unname(selected), recursive = FALSE)
            }

            selected
        },
        # }}}

        # serialize_param {{{
        # Convert a single QueryParam object to a plain list payload suitable
        # for state persistence. NULL parameters are preserved as NULL.
        serialize_param = function(param) {
            if (is.null(param)) {
                NULL
            } else {
                out <- as.list(param)
                out$value <- if (S7::S7_inherits(param, QueryParamDate)) {
                    as <- if (query_param_kind(param) %in% c("version_min", "version_max")) "num" else "iso"
                    format(param@value, as = as)
                } else {
                    param@value
                }
                if (S7::S7_inherits(param, QueryParamFacet) && !isTRUE(out$encoded)) {
                    out$encoded <- NULL
                }
                out
            }
        },
        # }}}

        # restore_bucket {{{
        # Restore one parameter bucket from a serialized state payload by
        # reconstructing each entry with the appropriate QueryParam class.
        restore_bucket = function(bucket, state) {
            checkmate::assert_choice(bucket, PARAM_BUCKETS)
            checkmate::assert_list(state)
            if (length(state)) {
                checkmate::assert_names(names(state), type = "unique")
            }

            if (bucket != "others" && length(state)) {
                checkmate::assert_names(names(state), subset.of = names(private[[bucket]]))
            }
            if (bucket != "others") {
                private[[bucket]] <- QueryParamStore$private_fields[[bucket]]
            } else {
                private[[bucket]] <- list()
            }
            private[[bucket]] <- lapply(private[[bucket]], function(value) NULL)

            for (name in names(state)) {
                payload <- state[[name]]
                if (is.null(payload)) {
                    if (bucket != "others") {
                        private[[bucket]][name] <- list(NULL)
                    }
                    next
                }

                if (S7::S7_inherits(payload, QueryParam)) {
                    param <- payload
                } else if (bucket == "query") {
                    param <- QueryParamDate(payload$value)
                } else if (bucket == "control") {
                    param <- QueryParamCtrl(payload$value)
                } else {
                    param <- QueryParamFacet(
                        payload$value,
                        negate = isTRUE(payload$negate),
                        encoded = isTRUE(payload$encoded)
                    )
                }

                if (bucket == "others") {
                    private$others[[name]] <- query_param_meta(param, name = name, kind = "facet")
                } else {
                    private$set_param_value(bucket, name, param)
                }
            }
        },
        # }}}

        # build_timestamp_param {{{
        # Combine `timestamp_from` and `timestamp_to` into a single synthetic
        # `_timestamp` range parameter used by the rendered Solr query.
        build_timestamp_param = function() {
            from <- private$query$timestamp_from
            to <- private$query$timestamp_to

            from_value <- if (is.null(from)) SolrDateUnbounded() else from@value
            to_value <- if (is.null(to)) SolrDateUnbounded() else to@value

            if (S7::S7_inherits(from_value, SolrDateUnbounded) && S7::S7_inherits(to_value, SolrDateUnbounded)) {
                return(NULL)
            }

            query_param_meta(
                QueryParamDate(SolrDateRange(from_value, to_value)),
                name = "_timestamp",
                kind = "timestamp_range"
            )
        },
        # }}}

        # get_or_set_facet {{{
        # Shared getter/setter for facet-like parameters. Supports `!` / `-`
        # negation syntax by evaluating the original call in the caller's
        # environment when `allow_negate = TRUE`.
        get_or_set_facet = function(name, value, allow_negate = TRUE, env = parent.frame()) {
            if (missing(value)) {
                return(private$facet[[name]])
            }

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
                expr <- substitute(value, parent.frame())
                value <- eval(bquote(eval_with_bang(.(expr), .env = .(env))[[1L]]))
            } else {
                value <- list(value = value, negate = FALSE)
            }

            val <- if (is.null(value$value)) {
                NULL
            } else {
                QueryParamFacet(value$value, negate = isTRUE(value$negate))
            }

            private$set_param_value("facet", name, val)

            self
        },
        # }}}

        # get_or_set_control {{{
        # Shared getter/setter for scalar control parameters such as flags,
        # strings, and counts, with validation delegated to checkmate.
        get_or_set_control = function(name, value, type = c("flag", "string", "count", "choice"), ...) {
            type <- match.arg(type)
            if (missing(value)) {
                return(private$control[[name]])
            }

            val <- if (is.null(value)) {
                NULL
            } else {
                getFromNamespace(paste0("assert_", match.arg(type)), "checkmate")(value, ..., .var.name = name)
                QueryParamCtrl(value = value)
            }

            private$set_param_value("control", name, val)

            self
        },
        # }}}

        # get_or_set_others {{{
        # Handle ad hoc parameters that do not have dedicated methods. This
        # path supports negation syntax, protects reserved names, and routes
        # predefined facets back through their dedicated setters.
        get_or_set_others = function(...) {
            dots <- eval(substitute(alist(...)))

            # directly return existing parameters if no new parameter is given
            if (length(dots) == 0L) {
                return(private$others)
            }

            # remove all existing parameters if `NULL` is given
            if (length(dots) == 1L && is.null(names(dots)) && is.null(dots[[1L]])) {
                private$others <- list()
                return(self)
            }

            # evaluate all input parameters in the parent environment to support
            # `!` notation for negating the constraints
            env <- parent.frame(n = 2L)
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

            # protect 'format'
            fmt <- if ("format" %in% nms) params[["format"]] else NULL
            if ("format" %in% nms && length(fmt) && (is.null(fmt$value) || fmt$value != FORMAT_JSON)) {
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

            # protect 'type'
            # TODO: move this to EsgQuery class
            type <- if ("type" %in% nms) params[["type"]] else NULL
            if ("type" %in% nms && length(type) && !is.null(type$value)) {
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

            predefined <- c(names(private$facet), "type", "format")

            is_predefined <- nms %in% predefined
            params_base <- params[is_predefined]
            names_base <- nms[is_predefined]
            params_oth <- params[!is_predefined]
            names_oth <- nms[!is_predefined]

            if (length(params_oth)) {
                clear <- vapply(params_oth, function(param) is.null(param$value), logical(1L))
                if (any(clear)) {
                    private$others[names_oth[clear]] <- NULL
                    params_oth <- params_oth[!clear]
                    names_oth <- names_oth[!clear]
                }
            }

            # stop for query and control parameters
            reserved_query <- names_oth[names_oth %in% c(names(private$query), "_timestamp")]
            if (length(reserved_query)) {
                stop(sprintf(
                    "The following parameter(s) are reserved for query conditions and cannot be set using '$params()': [%s].",
                    paste(sprintf("'%s'", reserved_query), collapse = ", ")
                ))
            }
            reserved_control <- names_oth[names_oth %in% setdiff(names(private$control), c("type", "format"))]
            if (length(reserved_control)) {
                stop(sprintf(
                    "The following parameter(s) are reserved for control conditions and cannot be set using '$params()': [%s].",
                    paste(sprintf("'%s'", reserved_control), collapse = ", ")
                ))
            }

            not_found <- setdiff(names_oth, FIELDS_FACETS_ALL)
            if (length(not_found)) {
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
                private$others <- stats::setNames(
                    lapply(seq_along(params_oth), function(i) {
                        param <- params_oth[[i]]
                        if (is.null(param$value)) {
                            return(NULL)
                        }

                        query_param_meta(
                            QueryParamFacet(
                                param$value,
                                negate = isTRUE(param$negate),
                                encoded = FALSE
                            ),
                            name = names_oth[[i]],
                            kind = "facet"
                        )
                    }),
                    names_oth
                )
                private$others <- private$others[!vapply(private$others, is.null, logical(1L))]
            }

            if (length(params_base)) {
                tryCatch(
                    {
                        # restore the original parameter values in case of errors
                        params_base_ori <- private$facet[names_base]

                        for (i in seq_along(params_base)) {
                            name <- names_base[[i]]
                            value <- params_base[[i]]

                            if (is.null(value$value)) {
                                eval(substitute(self[[name]](NULL)))
                                next
                            }

                            if (name %in% names(private$control) && isTRUE(value$negate)) {
                                stop(sprintf(
                                    "Control parameter '%s' does not support negation.",
                                    name
                                ))
                            }

                            checkmate::assert_vector(value$value, any.missing = FALSE, .var.name = "param")

                            if (isTRUE(value$negate)) {
                                eval(substitute(self[[name]](!value), list(value = value$value)))
                            } else {
                                eval(substitute(self[[name]](value), list(value = value$value)))
                            }
                        }
                    },
                    error = function(e) {
                        for (i in seq_along(names_base)) {
                            private$facet[[names_base[[i]]]] <- params_base_ori[[i]]
                        }
                        stop(e)
                    }
                )
            }

            self
        },
        # }}}

        # set_param_value {{{
        # Low-level assignment helper that writes a QueryParam (or NULL) into a
        # specific non-`others` bucket after validating the target bucket name.
        set_param_value = function(bucket, name, value) {
            checkmate::assert_choice(bucket, setdiff(PARAM_BUCKETS, "others"))

            if (is.null(value)) {
                private[[bucket]][name] <- list(NULL)
            } else if (S7::S7_inherits(value, QueryParam)) {
                kind <- if (bucket == "query") name else bucket
                private[[bucket]][[name]] <- query_param_meta(value, name = name, kind = kind)
            } else {
                stop("Invalid parameter value. Must be NULL or an instance of QueryParam.")
            }

            value
        },
        # }}}

        # render_facet {{{
        # Render facet/control/other parameters to `name=value` fragments in a
        # stable order for URL/query-string assembly.
        render_facet = function(name = NULL, null = FALSE) {
            params <- private$subset_params(
                name = name,
                bucket = c("facet", "control", "others"),
                null = null,
                flat = TRUE
            )

            rendered <- character(length(params))
            names(rendered) <- names(params)
            for (i in seq_along(params)) {
                rendered[i] <- query_param_render(params[[i]], names(params)[[i]])
            }

            rendered
        },
        # }}}

        # render_param_query {{{
        # Render free-text query constraints. This helper folds timestamp
        # boundaries into `_timestamp` and normalizes version boundary names so
        # downstream consumers see the final Solr query representation.
        render_query = function(name = NULL, null = FALSE) {
            params <- private$subset_params(
                name = name,
                bucket = "query",
                null = null,
                flat = TRUE
            )

            if (any(c("timestamp_from", "timestamp_to") %in% names(params))) {
                timestamp <- private$build_timestamp_param()
                if (!is.null(timestamp)) {
                    params$`_timestamp` <- timestamp
                }
                params$timestamp_from <- NULL
                params$timestamp_to <- NULL
            }

            idx_ver <- match(c("version_min", "version_max"), names(params), 0L)
            if (any(idx_ver > 0L)) {
                names(params)[idx_ver] <- "version"
            }

            rendered <- character(length(params))
            names(rendered) <- names(params)
            for (i in seq_along(params)) {
                rendered[i] <- query_param_render(params[[i]], names(params)[[i]])
            }

            rendered
        }
        # }}}
    )
    # }}}
)
# }}}

# vim: fdm=marker :
