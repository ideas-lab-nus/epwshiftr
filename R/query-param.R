#' @include solr-date.R

# constants {{{
QUERY_PARAM__FORMAT_JSON <- "application/solr+json"

QUERY_PARAM__REST_KEYS <- c("facets", "fields", "shards", "bbox", "start", "end", "from", "to")

# QUERY_PARAM__FIELDS {{{
QUERY_PARAM__FIELDS <- c(
    "ACK",
    "Acknowledgement",
    "Conventions",
    "access",
    "activity",
    "activity_drs",
    "activity_id",
    "amodell",
    "bias_adjustment",
    "branch_method",
    "cera_acronym",
    "cf_standard_name",
    "checksum",
    "checksum_type",
    "cmor_table",
    "collection",
    "contact",
    "creation_date",
    "data_node",
    "data_specs_version",
    "data_structure",
    "data_type",
    "dataset_category",
    "dataset_id",
    "dataset_status",
    "dataset_version_number",
    "datetime_end",
    "datetime_start",
    "datetime_stop",
    "deprecated",
    "directory_format_template_",
    "doi",
    "doi_author",
    "doi_publication_year",
    "doi_publisher",
    "doi_title",
    "domain",
    "downscaling_model_type",
    "driving_ensemble",
    "driving_model",
    "driving_model_ensemble_member",
    "driving_model_id",
    "driving_reanalysis",
    "east_degrees",
    "ensemble",
    "exp",
    "experiment",
    "experiment_description",
    "experiment_family",
    "experiment_id",
    "experiment_title",
    "forcing",
    "format",
    "framework",
    "frequency",
    "gebiet",
    "globus_url",
    "grid",
    "grid_label",
    "grid_resolution",
    "height_bottom",
    "height_top",
    "height_units",
    "id",
    "index_node",
    "instance_id",
    "institute",
    "institution",
    "institution_id",
    "latest",
    "lta",
    "master_gateway",
    "master_id",
    "member_id",
    "metadata_format",
    "metadata_url",
    "mip_era",
    "model",
    "model_cohort",
    "modelvers",
    "nominal_resolution",
    "north_degrees",
    "number_of_aggregations",
    "number_of_files",
    "pid_prefix",
    "predictand_calibration_dataset",
    "processing_level",
    "product",
    "product_version",
    "project",
    "project_name",
    "project_under",
    "rcm_model",
    "rcm_name",
    "rcm_version",
    "realization",
    "realm",
    "reanalysis",
    "reanalysis_ensemble",
    "region",
    "region_id",
    "replica",
    "retracted",
    "run_model",
    "schema",
    "shard",
    "short_description",
    "size",
    "source",
    "source_data_id",
    "source_id",
    "source_type",
    "source_version",
    "source_version_number",
    "south_degrees",
    "start_date_string",
    "std_citation",
    "std_citation_author",
    "std_citation_publication_year",
    "std_citation_publisher",
    "std_citation_title",
    "sub_experiment_id",
    "table_id",
    "target_mip",
    "target_mip_list",
    "time_frequency",
    "timestamp",
    "title",
    "tracking_id",
    "type",
    "url",
    "variable",
    "variable_id",
    "variable_long_name",
    "variable_units",
    "variant_label",
    "version",
    "west_degrees",
    "work_package"
)
# }}}

# QUERY_PARAM__DEF {{{
QUERY_PARAM__DEF <- list(
    project = list(type = "facet", default = "CMIP6"),
    activity_id = list(type = "facet", default = NULL),
    experiment_id = list(type = "facet", default = NULL),
    source_id = list(type = "facet", default = NULL),
    variable_id = list(type = "facet", default = NULL),
    frequency = list(type = "facet", default = NULL),
    variant_label = list(type = "facet", default = NULL),
    nominal_resolution = list(type = "facet", default = NULL),
    data_node = list(type = "facet", default = NULL),
    facets = list(type = "facet", default = NULL),
    fields = list(type = "facet", default = "*"),
    shards = list(type = "facet", default = NULL),
    datetime_start = list(type = "date", default = NULL),
    datetime_stop = list(type = "date", default = NULL),
    timestamp_from = list(type = "date", default = NULL),
    timestamp_to = list(type = "date", default = NULL),
    version_min = list(type = "date", default = NULL),
    version_max = list(type = "date", default = NULL),
    replica = list(type = "control", default = NULL),
    latest = list(type = "control", default = NULL),
    type = list(type = "control", default = "Dataset"),
    offset = list(type = "control", default = 0L),
    distrib = list(type = "control", default = TRUE),
    limit = list(type = "control", default = 10L),
    format = list(type = "control", default = QUERY_PARAM__FORMAT_JSON)
)
# }}}
# }}}

# QueryParam {{{
# classes {{{
# Base S7 class for all typed query parameter values.
# Concrete subclasses define how each parameter kind validates and renders.
QueryParam <- S7::new_class("QueryParam", abstract = TRUE)

# Accept the scalar value types supported by ESGF query parameters.
# The shared property keeps facet and control validation aligned.
query_param__prop_value <- checkmate_property(
    checkmate_any(
        checkmate_rule(S7::class_logical, checkmate::check_flag, branch = "flag"),
        checkmate_rule(S7::class_double, checkmate::check_number, branch = "double"),
        checkmate_rule(S7::class_integer, checkmate::check_number, branch = "integer"),
        checkmate_rule(S7::class_character, checkmate::check_string, min.chars = 1L, branch = "string")
    )
)

# Accept vector values supported by facet-style ESGF parameters.
# Facets can naturally render multiple values as comma-separated constraints.
query_param__prop_values <- checkmate_property(
    checkmate_any(
        checkmate_rule(S7::class_logical, checkmate::check_logical, any.missing = FALSE, branch = "flag"),
        checkmate_rule(S7::class_double, checkmate::check_double, any.missing = FALSE, branch = "double"),
        checkmate_rule(S7::class_integer, checkmate::check_integerish, any.missing = FALSE, branch = "integer"),
        checkmate_rule(
            S7::class_character,
            checkmate::check_character,
            min.chars = 1L,
            any.missing = FALSE,
            branch = "string"
        )
    )
)

# Store control parameters that accept a single scalar value.
# Examples include pagination flags and the response `format`.
QueryParamCtrl <- S7::new_class(
    "QueryParamCtrl",
    parent = QueryParam,
    properties = list(value = query_param__prop_value)
)

# Store facet parameters that may contain multiple values and optional negation.
# The `encoded` flag marks values that should not be escaped again.
QueryParamFacet <- S7::new_class(
    "QueryParamFacet",
    parent = QueryParam,
    properties = list(
        value = query_param__prop_values,
        negate = checkmate_property(S7::class_logical, checkmate::check_flag, default = FALSE),
        encoded = checkmate_property(S7::class_logical, checkmate::check_flag, default = FALSE)
    )
)

# Store structured query constraints backed by `SolrDate` values.
# These render into the free-text Solr `query=` parameter.
QueryParamDate <- S7::new_class(
    "QueryParamDate",
    parent = QueryParam,
    properties = list(value = S7::new_property(SolrDate)),
    # Normalize incoming date-like inputs to the internal Solr date model.
    # This lets query rendering treat strings, ranges, and date math uniformly.
    constructor = function(value) {
        value <- solr_date(value)
        S7::new_object(S7::S7_object(), value = value)
    }
)
# }}}

# render {{{
# Dispatch query parameter objects to their URL/query-string representation.
# Methods choose the syntax for facets, controls, and structured query clauses.
render <- S7::new_generic("render", "x", function(x, name, ...) {
    checkmate::assert_string(name, null.ok = TRUE)
    S7::S7_dispatch()
})

#' Render a facet query parameter.
#'
#' Converts facet values to `name=value` fragments, or `name!=value` fragments
#' when the parameter is negated. Optional encoding mirrors the ESGF URL rules
#' used for facet values.
#'
#' @param x A `QueryParamFacet` object.
#' @param name The query parameter name to render.
#' @param ... Reserved for render method compatibility.
#' @param encode If `TRUE`, URL-encode unencoded character values.
#' @param space If `TRUE`, add spaces around separators for display output.
#'
#' @return A character vector of rendered query fragments.
#'
#' @noRd
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
        # directly return for already encoded values
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

# Quote a single Solr range boundary when bridge-compatible date quoting is needed.
# Wildcards, empty strings, and already quoted values are left unchanged.
query_param__quote_bound <- function(x) {
    # do not quote for empty, wildcard, or already quoted values
    if (!nzchar(x) || identical(x, "*") || grepl('^".*"$', x)) {
        return(x)
    }

    # escape double quotes and wrap the value in double quotes
    sprintf('"%s"', gsub('"', '\\"', x, fixed = TRUE))
}

# Quote each concrete boundary in a Solr range expression.
# Non-range values are treated as a single boundary and quoted directly.
query_param__quote_range <- function(x) {
    vapply(
        x,
        function(value) {
            match <- regexec("^([\\[{])\\s*(.*?)\\s+TO\\s+(.*?)\\s*([\\]}])$", value, perl = TRUE)
            parts <- regmatches(value, match)[[1L]]
            # directly quote the value if not match the expected range format
            if (length(parts) != 5L) {
                return(query_param__quote_bound(value))
            }

            paste0(
                parts[[2L]],
                query_param__quote_bound(parts[[3L]]),
                " TO ",
                query_param__quote_bound(parts[[4L]]),
                parts[[5L]]
            )
        },
        character(1L),
        USE.NAMES = FALSE
    )
}

#' Render a structured Solr date query parameter.
#'
#' Converts a `QueryParamDate` object to a Solr field clause such as
#' `datetime_start:[* TO 2020-01-01T00:00:00Z]`. Bridge rendering can first
#' evaluate Solr date math locally and then quote concrete UTC dates.
#'
#' @param x A `QueryParamDate` object.
#' @param name The Solr field name to render.
#' @param ... Reserved for render method compatibility.
#' @param as Date formatting mode passed to `format()`.
#' @param quote_date If `TRUE`, quote concrete date boundaries.
#' @param eval_math If `TRUE`, evaluate Solr date math before formatting.
#' @param now Optional current time used when evaluating `NOW`.
#'
#' @return A length-one Solr field clause.
#'
#' @noRd
S7::method(render, QueryParamDate) <- function(
    x,
    name,
    ...,
    as = "iso",
    quote_date = FALSE,
    eval_math = FALSE,
    now = NULL
) {
    checkmate::assert_string(as)
    checkmate::assert_flag(quote_date)
    checkmate::assert_flag(eval_math)

    value <- if (eval_math) {
        solrdate__eval(x@value, now = if (is.null(now)) Sys.time() else now)
    } else {
        x@value
    }
    value <- format(value, as = as)
    if (quote_date && !identical(as, "num")) {
        value <- query_param__quote_range(value)
    }

    paste0(name, ":", value)
}

# Render control parameters as plain `name=value` query components.
# Character controls are URL-escaped unless the caller requests display output.
S7::method(render, QueryParamCtrl) <- function(x, name, ..., encode = TRUE) {
    checkmate::assert_flag(encode)

    value <- x@value
    if (is.logical(value)) {
        res <- tolower(value)
    } else if (is.numeric(value)) {
        res <- as.character(value)
    } else if (encode) {
        res <- query_param__encode(as.character(value))
    } else {
        res <- as.character(value)
    }
    paste0(name, "=", paste0(res, collapse = ","))
}
# }}}

# as.list {{{
# Convert a facet parameter to a serializable list payload.
# The payload preserves value, negation, and encoding state.
S7::method(as.list, QueryParamFacet) <- function(x) {
    list(
        value = x@value,
        negate = x@negate,
        encoded = x@encoded
    )
}

# Convert a date parameter to a serializable ISO string payload.
# The date object is formatted without applying bridge-only date math evaluation.
S7::method(as.list, QueryParamDate) <- function(x) {
    list(value = format(x@value, as = "iso"))
}

# Convert a control parameter to a serializable list payload.
# Control parameters only need to persist their scalar value.
S7::method(as.list, QueryParamCtrl) <- function(x) {
    list(value = x@value)
}
# }}}

# print {{{
# Print a compact representation of a single query parameter.
# Date parameters drop the synthetic leading field separator used by `render()`.
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
# Return predefined parameter names grouped by registry type.
# The registry is the only source of truth for dedicated parameter defaults.
query_param__names <- function(type = c("facet", "date", "control", "dedicated", "all")) {
    type <- match.arg(type)
    if (type == "all") {
        return(unique(c(names(QUERY_PARAM__DEF), QUERY_PARAM__REST_KEYS)))
    }
    if (type == "dedicated") {
        return(names(QUERY_PARAM__DEF))
    }

    names(QUERY_PARAM__DEF)[vapply(QUERY_PARAM__DEF, function(def) identical(def$type, type), logical(1L))]
}

# Test whether a parameter name is a result field worth requesting/validating.
# Date, control, and raw REST keywords are not result fields even when the
# underlying ESGF metadata exposes similarly named fields.
query_param__field <- function(name) {
    name %in%
        QUERY_PARAM__FIELDS &
        !name %in% QUERY_PARAM__REST_KEYS &
        !name %in% query_param__names("date") &
        !name %in% query_param__names("control")
}

# Create a fresh query parameter store.
# This wrapper keeps call sites independent of the concrete R6 constructor.
query_param__new_store <- function() {
    QueryParamStore$new()
}

# Return the stored value from a query parameter object.
# `NULL` is propagated to simplify callers that inspect optional parameters.
query_param__value <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }

    x@value
}

# Return whether a facet parameter is negated.
# Non-facet or missing parameters are never treated as negated.
query_param__negate <- function(x) {
    if (is.null(x) || !S7::S7_inherits(x, QueryParamFacet)) {
        return(FALSE)
    }

    x@negate
}

# Coerce an input value into the appropriate `QueryParam` subclass.
# The parameter name determines whether the value becomes a facet, query, or control object.
query_param__as <- function(name, value, negate = FALSE) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_flag(negate)

    if (is.null(value)) {
        return(NULL)
    }
    if (S7::S7_inherits(value, QueryParam)) {
        return(value)
    }
    encoded <- FALSE
    if (is.list(value) && "value" %in% names(value)) {
        negate <- isTRUE(value$negate)
        encoded <- isTRUE(value$encoded)
        value <- value$value
    }
    if (is.null(value)) {
        return(NULL)
    }

    type <- QUERY_PARAM__DEF[[name]]$type
    if (is.null(type)) {
        type <- "facet"
    }

    switch(
        type,
        date = QueryParamDate(value),
        control = QueryParamCtrl(value),
        facet = QueryParamFacet(value, negate = negate, encoded = encoded)
    )
}

# Render one `QueryParam` object using the explicit parameter name.
# Synthetic timestamp and version names are normalized to their Solr field names.
query_param__render <- function(x, name, ..., encode = TRUE) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_flag(encode)

    if (name %in% c("version_min", "version_max")) {
        return(render(x, name = "version", as = "num"))
    }
    if (identical(name, "_timestamp")) {
        return(render(x, name = "_timestamp", ...))
    }

    if (S7::S7_inherits(x, QueryParamFacet)) {
        return(render(x, name = name, encode = encode, ...))
    }
    if (S7::S7_inherits(x, QueryParamCtrl)) {
        return(render(x, name = name, encode = encode, ...))
    }

    render(x, name = name, ...)
}

# URL-escape a query component with libcurl semantics.
# This is used for both control values and the combined free-text `query=` payload.
query_param__encode <- function(x) {
    curl::curl_escape(x)
}

# Coerce a named list or existing store into a `QueryParamStore`.
# Plain values are routed through the flat parameter registry.
query_param__as_store <- function(params) {
    if (inherits(params, "QueryParamStore")) {
        return(params)
    }

    store <- QueryParamStore$new()
    if (is.null(params)) {
        return(store)
    }

    checkmate::assert_list(params, names = "named")
    state <- stats::setNames(
        lapply(seq_along(params), function(i) {
            name <- names(params)[[i]]
            value <- params[[i]]
            if (is.null(value) || S7::S7_inherits(value, QueryParam) || (is.list(value) && "value" %in% names(value))) {
                return(value)
            }
            query_param__as(name, value)
        }),
        names(params)
    )

    store$restore(state)
}

# Create an independent copy of any supported query parameter input.
# Inputs are first normalized to a store so callers can pass lists or stores.
query_param__clone <- function(params) {
    query_param__as_store(params)$copy()
}

# Render all parameters in the same order as query output, for display.
# This folds synthetic timestamp/version query fields without URL encoding values.
query_param__display <- function(params) {
    store <- query_param__as_store(params)
    params <- store$state()
    if (!length(params)) {
        return(character())
    }

    names_all <- names(params)
    selected <- unique(c(
        intersect(query_param__names("facet"), names_all),
        intersect(query_param__names("control"), names_all),
        setdiff(names_all, query_param__names("dedicated")),
        intersect(query_param__names("date"), names_all)
    ))

    query_names <- selected[selected %in% query_param__names("date")]
    facet_names <- selected[!selected %in% query_names]

    rendered <- character()
    if (length(facet_names)) {
        facet_params <- params[facet_names]
        rendered <- c(rendered, stats::setNames(
            vapply(seq_along(facet_params), function(i) {
                query_param__render(facet_params[[i]], names(facet_params)[[i]], encode = FALSE)
            }, character(1L)),
            names(facet_params)
        ))
    }

    if (!length(query_names)) {
        return(rendered)
    }

    query_params <- params[query_names]
    if (any(c("timestamp_from", "timestamp_to") %in% names(query_params))) {
        from <- query_params$timestamp_from
        to <- query_params$timestamp_to
        from_value <- if (is.null(from)) SolrDateUnbounded() else from@value
        to_value <- if (is.null(to)) SolrDateUnbounded() else to@value

        if (!S7::S7_inherits(from_value, SolrDateUnbounded) || !S7::S7_inherits(to_value, SolrDateUnbounded)) {
            query_params$`_timestamp` <- QueryParamDate(SolrDateRange(from_value, to_value))
        }
        query_params$timestamp_from <- NULL
        query_params$timestamp_to <- NULL
    }

    idx_ver <- match(c("version_min", "version_max"), names(query_params), 0L)
    if (any(idx_ver > 0L)) {
        names(query_params)[idx_ver] <- "version"
    }

    c(rendered, stats::setNames(
        vapply(seq_along(query_params), function(i) {
            query_param__render(query_params[[i]], names(query_params)[[i]], encode = FALSE)
        }, character(1L)),
        names(query_params)
    ))
}

# Print all query parameters in a user-facing bullet list.
# Empty stores are rendered explicitly so debugging output is not silent.
query_param__print <- function(params) {
    rendered <- query_param__display(params)
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
#' Internally, the store keeps parameters in one flat named list. Dedicated
#' parameter defaults and value shapes come from `QUERY_PARAM__DEF`, while
#' user-supplied ad hoc parameters are stored under their own names.
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
#' `QueryParamCtrl`, and `QueryParamDate`) so validation, negation state, and
#' rendering behavior can be handled consistently without attaching parameter
#' names to the value objects themselves.
#'
#' @author Hongyuan Jia
#' @name QueryParamStore
#' @keywords internal
#' @noRd
QueryParamStore <- R6::R6Class(
    "QueryParamStore",

    # public {{{
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
            private$init()
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
                return(private$facet("project"))
            }

            private$facet("project", value, allow_negate = TRUE, env = parent.frame())
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
            private$facet("activity_id", value, allow_negate = TRUE, env = parent.frame())
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
            private$facet("experiment_id", value, allow_negate = TRUE, env = parent.frame())
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
            private$facet("source_id", value, allow_negate = TRUE, env = parent.frame())
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
            private$facet("variable_id", value, allow_negate = TRUE, env = parent.frame())
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
            private$facet("frequency", value, allow_negate = TRUE, env = parent.frame())
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
            private$facet("variant_label", value, allow_negate = TRUE, env = parent.frame())
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
                return(private$facet("nominal_resolution"))
            }

            private$facet("nominal_resolution", value, allow_negate = TRUE, env = parent.frame())
            param <- private$facet("nominal_resolution")
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

            private$set("nominal_resolution", param)

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
            private$facet("data_node", value, allow_negate = TRUE, env = parent.frame())
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
            private$facet("facets", value, allow_negate = FALSE, env = parent.frame())
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
                return(private$facet("fields"))
            }

            private$facet("fields", value, allow_negate = FALSE, env = parent.frame())
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
                return(private$facet("shards"))
            }

            distrib <- private$control("distrib")
            if (!is.null(distrib) && !distrib@value && !is.null(value)) {
                stop("'$distrib()' returns FALSE. Shard specification is only applicable for distributed queries.")
            }
            private$facet("shards", value, allow_negate = FALSE, env = parent.frame())
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
            private$control("replica", value, type = "flag")
        },
        # }}}

        # latest {{{
        #' @description
        #' Get or set the `latest` parameter.
        #'
        #' By default, no `latest` constraint is sent, i.e. `$latest(NULL)`,
        #' and ESGF returns all versions. Use `$latest(TRUE)` to return only
        #' the latest version of each record, or `$latest(FALSE)` to return
        #' only records superseded by newer versions.
        #'
        #' @param value
        #' `r rd_query_method_param("latest", "flag", default = NULL, nullable = TRUE)`
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
        #'
        #' # remove the parameter
        #' q$latest(NULL)
        #' }
        latest = function(value = NULL) {
            if (missing(value)) {
                return(private$control("latest"))
            }

            private$control("latest", value, type = "flag")
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
                return(private$control("type"))
            }

            private$control(
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
                return(private$control("limit"))
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
            private$control("limit", value, type = "count")
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
                return(private$control("offset"))
            }

            private$control("offset", value, type = "count")
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
                return(private$control("distrib"))
            }

            private$control("distrib", value, type = "flag")
        },
        # }}}

        # format {{{
        #' @description
        #' Get or set the `format` parameter.
        #'
        #' Only JSON responses are currently supported. If a value other than
        #' `r QUERY_PARAM__FORMAT_JSON` is supplied, an error is raised.
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
        #' # unsupported formats are rejected
        #' q$format("application/xml")
        #' }
        format = function(value = QUERY_PARAM__FORMAT_JSON) {
            if (missing(value)) {
                return(private$control("format"))
            }

            if (!is.null(value) && !identical(value, QUERY_PARAM__FORMAT_JSON)) {
                stop(
                    sprintf(
                        "Only JSON response format '%s' is supported.",
                        QUERY_PARAM__FORMAT_JSON
                    ),
                    call. = FALSE
                )
            }

            private$control("format", value, type = "string")
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
            private$others(...)
        },
        # }}}

        # datetime_range {{{
        #' @description
        #' Get or set the temporal coverage range for data search.
        #'
        #' `$datetime_range()` constrains the search to datasets whose temporal
        #' coverage overlaps the specified range. It uses an interval-overlap
        #' logic consistent with the ESG search server's `start`/`end` keywords:
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
                    start = private$items$datetime_start,
                    stop = private$items$datetime_stop
                ))
            }

            # Convert a datetime boundary into the range form expected by ESGF.
            # Point values become half-open Solr ranges, while `*` clears the bound.
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

            start_bound <- if (!missing(start)) ensure_bound("start", start) else NULL
            stop_bound <- if (!missing(stop)) ensure_bound("stop", stop) else NULL
            if ((!missing(start) && !is.null(start_bound)) || (!missing(stop) && !is.null(stop_bound))) {
                private$clear_raw(c("start", "end"), "$datetime_range()")
            }

            if (!missing(start)) {
                private$set("datetime_start", start_bound)
            }

            if (!missing(stop)) {
                private$set("datetime_stop", stop_bound)
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
                    from = private$items$timestamp_from,
                    to = private$items$timestamp_to
                ))
            }

            # Convert a timestamp boundary into a single `QueryParamDate`.
            # Timestamp helpers accept point-like dates only; full range syntax is rejected.
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

            from_bound <- if (!missing(from)) ensure_bound("from", from) else NULL
            to_bound <- if (!missing(to)) ensure_bound("to", to) else NULL
            if ((!missing(from) && !is.null(from_bound)) || (!missing(to) && !is.null(to_bound))) {
                private$clear_raw(c("from", "to"), "$timestamp_range()")
            }

            if (!missing(from)) {
                private$set("timestamp_from", from_bound)
            }

            if (!missing(to)) {
                private$set("timestamp_to", to_bound)
            }

            self
        },
        # }}}

        # version_range {{{
        #' @description
        #' Get or set the version range for data search.
        #'
        #' `$version_range()` constrains the search to records whose integer
        #' `version` field falls within the specified range. Each boundary
        #' generates an independent numeric Solr range condition placed in the
        #' `query=` parameter:
        #'
        #' - `min` → `version:[min TO *]`
        #' - `max` → `version:[* TO max]`
        #'
        #' ESGF stores most CMIP-style versions in `YYYYMMDD` form, but the
        #' search field is numeric. Inputs such as `"2020"` or `"2020-06"` are
        #' normalized to comparable `YYYYMMDD` integer boundaries before
        #' rendering.
        #'
        #' Solr Date Math syntax (e.g. `NOW-1YEAR`) is **not** supported because
        #' `version` is not a date field.
        #'
        #' Range expressions (e.g. `[2020 TO 2025]`) are **not** accepted as
        #' input; use the `min` and `max` parameters separately instead.
        #'
        #' @param min,max A single string or number specifying the version boundary,
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
                    min = private$items$version_min,
                    max = private$items$version_max
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
                private$set("version_min", ensure_bound("min", min))
            }

            if (!missing(max)) {
                private$set("version_max", ensure_bound("max", max))
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
        render = function(name = NULL, quote_date = FALSE, datetime_end_alias = FALSE, eval_math = FALSE, now = NULL) {
            checkmate::assert_flag(quote_date)
            checkmate::assert_flag(datetime_end_alias)
            checkmate::assert_flag(eval_math)

            # expand synthetic names {{{
            if (!is.null(name)) {
                checkmate::assert_character(name, any.missing = FALSE, unique = TRUE)
                if (!length(name)) {
                    return(character())
                }

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
            # }}}

            # render selected parameters {{{
            names_all <- names(private$items)
            if (!is.null(name)) {
                checkmate::assert_subset(name, names_all)
                selected <- name
            } else {
                selected <- private$order()
            }

            query_names <- selected[selected %in% query_param__names("date")]
            facet_names <- selected[!selected %in% query_names]

            rendered <- c(
                private$render_facet(facet_names),
                private$render_query(
                    query_names,
                    quote_date = quote_date,
                    datetime_end_alias = datetime_end_alias,
                    eval_math = eval_math,
                    now = now
                )
            )
            # }}}

            rendered
        },
        # }}}

        # state {{{
        #' @description
        #' Get the current parameter state.
        #'
        #' `$state()` returns the current query parameters as a flat named list.
        #'
        #' @param name A character vector of parameter names to include, or
        #'   `NULL` to include all parameters.
        #' @param null If `TRUE`, include parameters whose current value is
        #'   `NULL`. Otherwise, omit unset parameters.
        #'
        #' @return A flat named list containing `QueryParam` objects and, when
        #'   `null = TRUE`, `NULL` entries for unset dedicated parameters.
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
            private$subset(name = name, null = null)
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
        #' `$serialize()` converts the current flat state into either a list or a
        #' JSON string that can later be passed to
        #' \\href{#method-QueryParamStore-restore}{\\code{$restore()}}.
        #'
        #' @param name A character vector of parameter names to include, or
        #'   `NULL` to include all parameters.
        #' @param null If `TRUE`, include parameters whose current value is
        #'   `NULL`. Otherwise, omit unset parameters.
        #' @param type Output type. Either `"list"` or `"json"`.
        #'
        #' @return Either a flat named list or a JSON string, depending on `type`.
        #'
        #' @examples
        #' \dontrun{
        #' q$serialize()
        #' q$serialize(type = "json")
        #' }
        serialize = function(name = NULL, null = FALSE, type = c("list", "json")) {
            params <- self$state(name = name, null = null)
            out <- stats::setNames(
                lapply(names(params), function(name) {
                    private$serialize_one(params[[name]], name = name)
                }),
                names(params)
            )

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
            if (length(state)) {
                checkmate::assert_names(names(state), type = "unique")
            }
            if (any(c("facet", "query", "control", "others") %in% names(state))) {
                stop(
                    "Bucketed query parameter states are no longer supported. Use the flat parameter schema.",
                    call. = FALSE
                )
            }
            # store current state in a temp variable and roll back if restore fails at any point
            original <- self$state(name = NULL, null = TRUE)

            tryCatch(
                {
                    private$init()
                    for (name in names(state)) {
                        private$restore_one(name, state[[name]])
                    }
                },
                error = function(e) {
                    private$items <- original
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
            for (line in query_param__display(self)) {
                cli::cli_bullets(c("*" = line))
            }

            invisible(self)
        }
        # }}}
    ),
    # }}}

    # private {{{
    private = list(
        # defaults {{{
        # items: one flat named list containing dedicated and ad hoc parameters.
        items = list(),
        # }}}

        # init {{{
        # Initialize the flat parameter list from registry defaults.
        # Non-NULL defaults are coerced into their QueryParam subclasses.
        init = function() {
            private$items <- stats::setNames(
                lapply(names(QUERY_PARAM__DEF), function(name) {
                    query_param__as(name, QUERY_PARAM__DEF[[name]]$default)
                }),
                names(QUERY_PARAM__DEF)
            )
        },
        # }}}

        # subset {{{
        # Select parameters from the flat list, optionally filtering by name and
        # keeping NULL entries for unset dedicated parameters.
        subset = function(name = NULL, null = FALSE) {
            checkmate::assert_character(name, null.ok = TRUE, any.missing = FALSE, unique = TRUE)
            checkmate::assert_flag(null)

            selected <- if (is.null(name)) {
                private$items
            } else {
                checkmate::assert_subset(name, names(private$items))
                private$items[name]
            }

            if (!null) {
                selected <- selected[!vapply(selected, is.null, logical(1L))]
            }

            selected
        },
        # }}}

        # serialize_one {{{
        # Convert a single QueryParam object to a plain list payload suitable
        # for state persistence. NULL parameters are preserved as NULL.
        serialize_one = function(param, name = NULL) {
            if (is.null(param)) {
                NULL
            } else {
                out <- as.list(param)
                out$value <- if (S7::S7_inherits(param, QueryParamDate)) {
                    as <- if (name %in% c("version_min", "version_max")) "num" else "iso"
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

        # restore_one {{{
        # Restore one flat parameter entry from either a QueryParam object or a
        # serialized payload containing a `value` field.
        restore_one = function(name, payload) {
            checkmate::assert_string(name, min.chars = 1L)

            if (is.null(payload)) {
                private$set(name, NULL)
                return(invisible(NULL))
            }

            param <- if (S7::S7_inherits(payload, QueryParam)) {
                payload
            } else {
                query_param__as(name, payload)
            }

            private$set(name, param)
            invisible(param)
        },
        # }}}

        # timestamp {{{
        # Combine `timestamp_from` and `timestamp_to` into a single synthetic
        # `_timestamp` range parameter used by the rendered Solr query.
        timestamp = function() {
            from <- private$items$timestamp_from
            to <- private$items$timestamp_to

            from_value <- if (is.null(from)) SolrDateUnbounded() else from@value
            to_value <- if (is.null(to)) SolrDateUnbounded() else to@value

            if (S7::S7_inherits(from_value, SolrDateUnbounded) && S7::S7_inherits(to_value, SolrDateUnbounded)) {
                return(NULL)
            }

            QueryParamDate(SolrDateRange(from_value, to_value))
        },
        # }}}

        # extra_names {{{
        # Return names that do not have dedicated setter methods.
        # These are the parameters exposed through `$params()`.
        extra_names = function() {
            setdiff(names(private$items), query_param__names("dedicated"))
        },
        # }}}

        # order {{{
        # Return the default render order for the flat parameter list.
        # Facet-like URL parameters are rendered before structured Solr queries.
        order = function() {
            c(
                intersect(query_param__names("facet"), names(private$items)),
                intersect(query_param__names("control"), names(private$items)),
                private$extra_names(),
                intersect(query_param__names("date"), names(private$items))
            )
        },
        # }}}

        # facet {{{
        # Shared getter/setter for facet-like parameters. Supports `!` / `-`
        # negation syntax by evaluating the original call in the caller's
        # environment when `allow_negate = TRUE`.
        facet = function(name, value, allow_negate = TRUE, env = parent.frame()) {
            if (missing(value)) {
                return(private$items[[name]])
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

            private$set(name, val)

            self
        },
        # }}}

        # control {{{
        # Shared getter/setter for scalar control parameters such as flags,
        # strings, and counts, with validation delegated to checkmate.
        control = function(name, value, type = c("flag", "string", "count", "choice"), ...) {
            type <- match.arg(type)
            if (missing(value)) {
                return(private$items[[name]])
            }

            val <- if (is.null(value)) {
                NULL
            } else {
                getFromNamespace(paste0("assert_", match.arg(type)), "checkmate")(value, ..., .var.name = name)
                QueryParamCtrl(value = value)
            }

            private$set(name, val)

            self
        },
        # }}}

        # others {{{
        # Handle ad hoc parameters that do not have dedicated methods. This
        # path supports negation syntax, protects reserved names, and routes
        # predefined facets back through their dedicated setters.
        others = function(...) {
            # normalize input {{{
            dots <- eval(substitute(alist(...)))

            # directly return existing parameters if no new parameter is given
            if (length(dots) == 0L) {
                extra_names <- private$extra_names()
                if (!length(extra_names)) {
                    return(list())
                }

                return(private$items[extra_names])
            }

            # remove all existing parameters if `NULL` is given
            if (length(dots) == 1L && is.null(names(dots)) && is.null(dots[[1L]])) {
                private$items[private$extra_names()] <- NULL
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
            # }}}

            # split predefined and ad hoc params {{{
            predefined <- c(query_param__names("facet"), "type", "format")

            is_predefined <- nms %in% predefined
            params_base <- params[is_predefined]
            names_base <- nms[is_predefined]
            params_oth <- params[!is_predefined]
            names_oth <- nms[!is_predefined]

            if (length(params_oth)) {
                clear <- vapply(params_oth, function(param) is.null(param$value), logical(1L))
                if (any(clear)) {
                    private$items[names_oth[clear]] <- NULL
                    params_oth <- params_oth[!clear]
                    names_oth <- names_oth[!clear]
                }
            }
            if (length(params_oth)) {
                params_oth <- private$filter_raw(params_oth)
                names_oth <- names(params_oth)
            }
            # }}}

            # validate ad hoc params {{{
            # stop for query and control parameters
            reserved_query <- names_oth[names_oth %in% c(query_param__names("date"), "_timestamp")]
            if (length(reserved_query)) {
                stop(sprintf(
                    "The following parameter(s) are reserved for query conditions and cannot be set using '$params()': [%s].",
                    paste(sprintf("'%s'", reserved_query), collapse = ", ")
                ))
            }
            reserved_control <- names_oth[names_oth %in% setdiff(query_param__names("control"), c("type", "format"))]
            if (length(reserved_control)) {
                stop(sprintf(
                    "The following parameter(s) are reserved for control conditions and cannot be set using '$params()': [%s].",
                    paste(sprintf("'%s'", reserved_control), collapse = ", ")
                ))
            }

            not_found <- setdiff(names_oth, c(QUERY_PARAM__FIELDS, QUERY_PARAM__REST_KEYS))
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
            # }}}

            # apply ad hoc params {{{
            if (length(params_oth)) {
                for (i in seq_along(params_oth)) {
                    param <- params_oth[[i]]
                    private$set(
                        names_oth[[i]],
                        QueryParamFacet(
                            param$value,
                            negate = isTRUE(param$negate),
                            encoded = FALSE
                        )
                    )
                }
            }
            # }}}

            # apply predefined params {{{
            if (length(params_base)) {
                tryCatch(
                    {
                        # restore the original parameter values in case of errors
                        params_base_ori <- lapply(names_base, function(name) {
                            private$items[[name]]
                        })

                        for (i in seq_along(params_base)) {
                            name <- names_base[[i]]
                            value <- params_base[[i]]

                            if (is.null(value$value)) {
                                eval(substitute(self[[name]](NULL)))
                                next
                            }

                            if (name %in% query_param__names("control") && isTRUE(value$negate)) {
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
                            name <- names_base[[i]]
                            private$items[[name]] <- params_base_ori[[i]]
                        }
                        stop(e)
                    }
                )
            }
            # }}}

            self
        },
        # }}}

        # raw REST keyword helpers {{{
        # Warn when a structured helper overrides raw REST temporal keywords.
        # The message keeps the helper name and action explicit for user debugging.
        warn_raw = function(keywords, helper, action) {
            warning(
                sprintf(
                    "%s raw REST keyword(s) %s because structured helper %s takes precedence over raw REST keyword constraints.",
                    action,
                    paste(sprintf("'%s'", keywords), collapse = ", "),
                    helper
                ),
                call. = FALSE
            )
        },

        # Report whether any structured query parameters are currently set.
        # This is used to decide whether raw REST keywords should be ignored.
        structured = function(names) {
            !all(vapply(private$items[names], is.null, logical(1L)))
        },

        # Drop raw REST temporal keywords that conflict with structured helpers.
        # Non-conflicting ad hoc parameters pass through unchanged.
        filter_raw = function(params) {
            nms <- names(params)
            if (private$structured(c("datetime_start", "datetime_stop"))) {
                drop <- intersect(nms, c("start", "end"))
                if (length(drop)) {
                    private$warn_raw(drop, "$datetime_range()", "Ignoring")
                    params <- params[!nms %in% drop]
                    nms <- names(params)
                }
            }
            if (private$structured(c("timestamp_from", "timestamp_to"))) {
                drop <- intersect(nms, c("from", "to"))
                if (length(drop)) {
                    private$warn_raw(drop, "$timestamp_range()", "Ignoring")
                    params <- params[!nms %in% drop]
                }
            }

            params
        },

        # Remove existing raw REST temporal keywords after a structured helper is set.
        # Removed keywords are returned invisibly for callers that need diagnostics.
        clear_raw = function(keywords, helper) {
            existing <- intersect(keywords, private$extra_names())
            existing <- existing[!vapply(private$items[existing], is.null, logical(1L))]
            if (length(existing)) {
                private$items[existing] <- NULL
                private$warn_raw(existing, helper, "Removing")
            }
            invisible(existing)
        },
        # }}}

        # set {{{
        # Low-level assignment helper that writes a QueryParam (or NULL) into
        # the flat parameter list.
        set = function(name, value) {
            checkmate::assert_string(name, min.chars = 1L)

            if (is.null(value)) {
                if (name %in% query_param__names("dedicated")) {
                    private$items[name] <- list(NULL)
                } else {
                    private$items[name] <- NULL
                }
            } else if (S7::S7_inherits(value, QueryParam)) {
                private$items[[name]] <- value
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
            params <- private$subset(
                name = name,
                null = null
            )

            rendered <- character(length(params))
            names(rendered) <- names(params)
            for (i in seq_along(params)) {
                rendered[i] <- query_param__render(params[[i]], names(params)[[i]])
            }

            rendered
        },
        # }}}

        # render_query {{{
        # Render free-text query constraints. This helper folds timestamp
        # boundaries into `_timestamp` and normalizes version boundary names so
        # downstream consumers see the final Solr query representation.
        render_query = function(
            name = NULL,
            null = FALSE,
            quote_date = FALSE,
            datetime_end_alias = FALSE,
            eval_math = FALSE,
            now = NULL
        ) {
            checkmate::assert_flag(quote_date)
            checkmate::assert_flag(datetime_end_alias)
            checkmate::assert_flag(eval_math)

            params <- private$subset(
                name = name,
                null = null
            )

            if (any(c("timestamp_from", "timestamp_to") %in% names(params))) {
                timestamp <- private$timestamp()
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
                name <- names(params)[[i]]
                rendered[i] <- query_param__render(
                    params[[i]],
                    name,
                    quote_date = quote_date,
                    eval_math = eval_math,
                    now = now
                )
                if (datetime_end_alias && identical(name, "datetime_stop")) {
                    rendered[i] <- sprintf(
                        "(%s OR %s)",
                        rendered[i],
                        query_param__render(
                            params[[i]],
                            "datetime_end",
                            quote_date = quote_date,
                            eval_math = eval_math,
                            now = now
                        )
                    )
                }
            }

            rendered
        }
        # }}}
    )
    # }}}
)
# }}}

# vim: fdm=marker :
