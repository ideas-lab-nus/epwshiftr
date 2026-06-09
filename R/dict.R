ESGDICT_FORMAT <- "epwshiftr_esg_dict"
ESGDICT_FORMAT_VERSION <- "2"

CMIP6DICT_VARIANT_PATTERN <- "^r\\d+i\\d+p\\d+f\\d+$"

# Each project has a required vocab source and an optional request source.
# Request data powers variable/table/frequency relationship checks when present.
ESGDICT_PROJECTS <- list(
    CMIP6 = list(
        profile = "cmip6",
        vocab = list(source = "github", repo = "WCRP-CMIP/CMIP6_CVs", reader = "cmip6_cvs", tagged = TRUE),
        request = list(source = "github", repo = "PCMDI/cmip6-cmor-tables", reader = "cmip6_cmor", tagged = TRUE)
    ),
    CMIP6PLUS = list(
        profile = "cmip6plus",
        vocab = list(source = "github", repo = "WCRP-CMIP/CMIP6Plus_CVs", reader = "esgvoc", ref = "esgvoc"),
        request = NULL
    ),
    INPUT4MIP = list(
        profile = "input4mip",
        vocab = list(source = "github", repo = "PCMDI/input4MIPs_CVs", reader = "esgvoc", ref = "esgvoc"),
        request = NULL
    ),
    OBS4REF = list(
        profile = "obs4ref",
        vocab = list(source = "github", repo = "Climate-REF/Obs4REF_CVs", reader = "esgvoc", ref = "main"),
        request = NULL
    ),
    `CORDEX-CMIP6` = list(
        profile = "cordex-cmip6",
        vocab = list(source = "github", repo = "WCRP-CORDEX/cordex-cmip6-cv", reader = "esgvoc", ref = "esgvoc"),
        request = NULL
    ),
    CMIP7 = list(
        profile = "cmip7",
        vocab = list(source = "github", repo = "WCRP-CMIP/CMIP7-CVs", reader = "esgvoc", ref = "esgvoc"),
        request = NULL
    ),
    EMD = list(
        profile = "emd",
        vocab = list(source = "github", repo = "WCRP-CMIP/Essential-Model-Documentation", reader = "esgvoc", ref = "esgvoc"),
        request = NULL
    )
)

CV_TYPES <- c(
    "DRS",
    "activity_id",
    "experiment_id",
    "frequency",
    "grid_label",
    "institution_id",
    "nominal_resolution",
    "realm",
    "required_global_attributes",
    "source_id",
    "source_type",
    "sub_experiment_id",
    "table_id"
)

CMIP6DICT_FIELDS <- c(
    "project",
    "mip_era",
    "activity_id",
    "experiment_id",
    "source_id",
    "variable_id",
    "table_id",
    "frequency",
    "grid_label",
    "nominal_resolution",
    "institution_id",
    "realm",
    "source_type",
    "sub_experiment_id",
    "variant_label"
)

CMIP6DICT_FIELD_ALIASES <- c(
    activity = "activity_id",
    activity_drs = "activity_id",
    experiment = "experiment_id",
    source = "source_id",
    variable = "variable_id",
    variant = "variant_label",
    member_id = "variant_label",
    resolution = "nominal_resolution",
    modeling_realm = "realm"
)

esgdict__normalize_project <- function(project = "CMIP6") {
    checkmate::assert_string(project, min.chars = 1L)
    toupper(project)
}

esgdict__project_spec <- function(project = "CMIP6") {
    project <- esgdict__normalize_project(project)
    spec <- ESGDICT_PROJECTS[[project]]
    if (is.null(spec)) {
        stop(
            sprintf(
                "ESG dictionary project `%s` is not implemented. Supported projects: %s.",
                project,
                paste(names(ESGDICT_PROJECTS), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    spec$project <- project
    spec
}

esgdict__profile <- function(project = "CMIP6") {
    esgdict__project_spec(project)$profile
}

esgdict__default_file <- function(project = "CMIP6") {
    sprintf("%sDICT.json", esgdict__normalize_project(project))
}

esgdict__default_store_path <- function(project = "CMIP6", dict_id = NULL) {
    project <- esgdict__normalize_project(project)
    checkmate::assert_string(dict_id, min.chars = 1L, null.ok = TRUE)

    file <- if (is.null(dict_id)) {
        esgdict__default_file(project)
    } else {
        sprintf("%s.json", dict_id)
    }
    store_path("dicts", tolower(project), file)
}

esgdict__hashable <- function(x) {
    if (inherits(x, "numeric_version")) {
        return(as.character(x))
    }
    if (inherits(x, "POSIXt")) {
        return(format(x, usetz = TRUE))
    }
    if (is.list(x)) {
        return(lapply(x, esgdict__hashable))
    }
    x
}

esgdict__dict_id <- function(project, version, sources, built_time = NULL) {
    extract_store_hash(
        esgdict__normalize_project(project),
        ESGDICT_FORMAT_VERSION,
        jsonlite::toJSON(esgdict__hashable(version), auto_unbox = TRUE, null = "null"),
        jsonlite::toJSON(esgdict__hashable(sources), auto_unbox = TRUE, null = "null"),
        esgdict__hashable(built_time)
    )
}

esgdict__latest_store_path <- function(project = "CMIP6") {
    target_project <- esgdict__normalize_project(project)
    root <- store_dir(init = FALSE)
    if (!dir.exists(root)) {
        return(NULL)
    }

    store <- EsgStore$new(root, create = FALSE)
    on.exit(store$close(), add = TRUE)
    artifacts <- data.table::as.data.table(ddb_read_table(priv(store)$conn, "artifact"))
    artifacts <- artifacts[kind == "dict" & project == target_project & status == "available"]
    if (!nrow(artifacts)) {
        return(NULL)
    }
    data.table::setorder(artifacts, -updated_at)
    store$artifact_path(artifacts$artifact_id[[1L]])
}

esgdict__default_env <- function() {
    if (is.null(this$dicts)) {
        this$dicts <- new.env(parent = emptyenv())
    }
    this$dicts
}

esgdict__assert_implemented <- function(project) {
    esgdict__project_spec(project)
    invisible(TRUE)
}

esgdict__cache_policy <- function(use_cache = TRUE) {
    checkmate::assert_flag(use_cache)

    mode <- cache_mode()
    enabled <- isTRUE(use_cache) && !identical(mode, "off")
    list(
        mode = mode,
        read = enabled,
        write = enabled && identical(mode, "normal"),
        source_read = TRUE,
        source_write = !identical(mode, "offline"),
        offline = identical(mode, "offline")
    )
}

#' ESG Project Dictionary
#'
#' @description
#' `EsgDict` is an [R6][R6::R6Class] class for project-specific ESG
#' controlled vocabulary data. It stores vocabulary tables, optional request
#' tables, normalized query indices, and source metadata used by local option
#' discovery and legality checks.
#'
#' `esgdict()` is a small constructor around `EsgDict$new()`.
#'
#' # Supported projects
#'
#' The dictionary currently supports `"CMIP6"`, `"CMIP6PLUS"`, `"INPUT4MIP"`,
#' `"OBS4REF"`, `"CORDEX-CMIP6"`, `"CMIP7"`, and `"EMD"`. CMIP6 dictionaries
#' include both controlled vocabularies and CMOR request-table data. Other
#' projects use vocabulary data only until a project-specific request source is
#' registered.
#'
#' # Source downloads in examples
#'
#' Building a dictionary may download upstream vocabulary/request sources when
#' the parsed dictionary cache and raw source cache are missing. Most examples
#' load a small installed CMIP6 example dictionary and run without network
#' access. The example that calls `$build()` is guarded so CRAN checks do not
#' perform network downloads.
#'
#' @param project ESG project identifier, such as `"CMIP6"` or `"CMIP6PLUS"`.
#'
#' @return `esgdict()` returns a new [EsgDict] object.
#'   `esgdict_set_default()` returns `dict`, invisibly.
#'   `esgdict_get_default()` returns the current package-level default
#'   dictionary for `project`, or `NULL`.
#'
#' @seealso [esgdict_option()] and [esgdict_check()] for user-facing discovery
#' and validation helpers.
#'
#' @examples
#' example_path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
#' dict <- esgdict(project = "CMIP6")
#' suppressMessages(dict$load(example_path))
#' dict$project()
#'
#' esgdict_set_default(dict)
#' identical(esgdict_get_default("CMIP6"), dict)
#' esgdict_option("experiment_id", activity_id = "CMIP")
#' esgdict_check(activity = "CMIP", experiment = "historical")
#' @author Hongyuan Jia
#'
#' @name EsgDict
#' @export
esgdict <- function(project = "CMIP6") {
    EsgDict$new(project = project)
}

#' @rdname EsgDict
#' @param dict An [EsgDict] object used as the package-level default dictionary
#'   for its project.
#' @export
esgdict_set_default <- function(dict) {
    if (!inherits(dict, "EsgDict")) {
        stop("`dict` must be an `EsgDict` object.", call. = FALSE)
    }

    assign(dict$project(), dict, envir = esgdict__default_env())
    invisible(dict)
}

#' @rdname EsgDict
#' @export
esgdict_get_default <- function(project = "CMIP6") {
    project <- esgdict__normalize_project(project)
    env <- esgdict__default_env()
    if (exists(project, envir = env, inherits = FALSE)) {
        get(project, envir = env, inherits = FALSE)
    } else {
        NULL
    }
}

#' @name EsgDict
#' @export
EsgDict <- R6::R6Class("EsgDict",
    cloneable = FALSE, lock_class = TRUE,
    public = list(
        #' @description
        #' Create a new ESG project dictionary.
        #'
        #' The new dictionary is empty. Use
        #' \href{#method-EsgDict-build}{\code{$build()}} to fetch and parse
        #' upstream sources, or \href{#method-EsgDict-load}{\code{$load()}} to
        #' restore a saved dictionary JSON file.
        #'
        #' @param project ESG project identifier, such as `"CMIP6"` or
        #'   `"CMIP6PLUS"`.
        #'
        #' @return An `EsgDict` object.
        #'
        #' @examples
        #' dict <- EsgDict$new(project = "CMIP6")
        #' dict$status()
        initialize = function(project = "CMIP6") {
            private$m_project <- esgdict__normalize_project(project)
            private$m_profile <- esgdict__profile(private$m_project)
        },

        #' @description
        #' Return the normalized ESG project identifier.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' dict <- EsgDict$new(project = "CMIP6PLUS")
        #' dict$project()
        project = function() {
            private$m_project
        },

        #' @description
        #' Return the internal dictionary profile.
        #'
        #' The profile determines how project-specific vocabulary sources are
        #' parsed and normalized.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' dict <- EsgDict$new(project = "CMIP6")
        #' dict$profile()
        profile = function() {
            private$m_profile
        },

        #' @description
        #' Return vocabulary and request-source versions.
        #'
        #' Empty or partially loaded dictionaries return `NULL`.
        #'
        #' @return A named list with `vocab` and `request` elements, or `NULL`.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$version()
        version = function() {
            private$m_version
        },

        #' @description
        #' Return upstream source metadata.
        #'
        #' Source metadata records repository, tag/ref, commit, and local source
        #' directory information for the data used to build the dictionary.
        #'
        #' @return A named list, or `NULL` for an empty dictionary.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$sources()
        sources = function() {
            private$m_sources
        },

        #' @description
        #' Return source vocabulary timestamps.
        #'
        #' Timestamps are extracted from source vocabulary metadata when
        #' available.
        #'
        #' @return A named list of timestamps, or `NULL`.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$timestamp()
        timestamp = function() {
            private$m_timestamps
        },

        #' @description
        #' Return the time when this dictionary was built.
        #'
        #' @return A `POSIXct` value, or `NULL`.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$built_time()
        built_time = function() {
            private$m_built_time
        },

        #' @description
        #' Return the dictionary lifecycle status.
        #'
        #' Status values are:
        #'
        #' - `"empty"`: no vocabulary/request payload is loaded.
        #' - `"partial"`: some required payload is missing.
        #' - `"built"`: the dictionary was built in this R session.
        #' - `"loaded"`: the dictionary was restored from disk.
        #'
        #' @return A single string.
        #'
        #' @examples
        #' dict <- EsgDict$new(project = "CMIP6")
        #' dict$status()
        status = function() {
            has_vocab <- !is.null(private$m_data$vocab) && length(private$m_data$vocab) > 0L
            has_request <- !is.null(private$m_data$request) && NROW(private$m_data$request) > 0L
            needs_request <- !is.null(esgdict__project_spec(private$m_project)$request)

            if (!has_vocab && !has_request) {
                "empty"
            } else if (has_vocab && (!needs_request || has_request) && !is.null(private$m_version)) {
                if (!is.null(private$m_status)) private$m_status else "loaded"
            } else if (has_vocab && (!needs_request || has_request)) {
                if (!is.null(private$m_status)) private$m_status else "loaded"
            } else {
                "partial"
            }
        },

        #' @description
        #' Check whether the dictionary contains usable data.
        #'
        #' A dictionary has usable data after a complete
        #' \href{#method-EsgDict-build}{\code{$build()}} or
        #' \href{#method-EsgDict-load}{\code{$load()}}.
        #'
        #' @return `TRUE` or `FALSE`.
        #'
        #' @examples
        #' dict <- EsgDict$new(project = "CMIP6")
        #' dict$has_data()
        has_data = function() {
            identical(self$status(), "built") || identical(self$status(), "loaded")
        },

        #' @description
        #' Check whether the dictionary is empty.
        #'
        #' @return `TRUE` or `FALSE`.
        #'
        #' @examples
        #' dict <- EsgDict$new(project = "CMIP6")
        #' dict$is_empty()
        is_empty = function() {
            identical(self$status(), "empty")
        },

        #' @description
        #' Build the dictionary from upstream source data.
        #'
        #' `$build()` resolves the configured project vocabulary source, downloads
        #' or reuses raw source files as needed, parses them into normalized
        #' tables, and builds query indices for option discovery and validation.
        #' If the dictionary already has data and `force = FALSE`, the object is
        #' returned unchanged.
        #'
        #' @param token Optional GitHub token used for source resolution and
        #'   downloads.
        #' @param force If `TRUE`, rebuild even when the dictionary already has
        #'   data and bypass the parsed dictionary cache.
        #' @param cv_tag Optional vocabulary source tag or ref. When `NULL`, the
        #'   project default ref or latest tagged source is used.
        #' @param request_tag Optional request-table source tag. Used by projects
        #'   that define a request source, currently CMIP6.
        #' @param dreq_tag Deprecated alias for `request_tag`.
        #' @param use_cache If `TRUE`, use the parsed dictionary cache when
        #'   available. Raw source files may still be reused from `source_dir`.
        #' @param source_dir Directory used to read and write raw source files.
        #'   The default is the package store source directory for this project.
        #'
        #' @return The modified `EsgDict` object itself.
        #'
        #' @examples
        #' if (identical(Sys.getenv("NOT_CRAN"), "true") && curl::has_internet()) {
        #'     dict <- EsgDict$new(project = "CMIP6")
        #'     dict$build(cv_tag = "6.2.58", request_tag = "01.00.33")
        #'     dict$has_data()
        #' }
        build = function(
            token = NULL,
            force = FALSE,
            cv_tag = NULL,
            request_tag = NULL,
            dreq_tag = NULL,
            use_cache = TRUE,
            source_dir = esgdict__source_dir(project = private$m_project)
        ) {
            esgdict__assert_implemented(private$m_project)
            checkmate::assert_flag(force)
            checkmate::assert_flag(use_cache)
            checkmate::assert_string(cv_tag, null.ok = TRUE)
            checkmate::assert_string(request_tag, null.ok = TRUE)
            checkmate::assert_string(dreq_tag, null.ok = TRUE)
            if (is.null(request_tag) && !is.null(dreq_tag)) {
                request_tag <- dreq_tag
            }

            # Empty dictionaries need data, but only an explicit user `force`
            # should bypass the parsed dictionary cache.
            rebuild <- force || self$is_empty()
            if (!rebuild) return(self)

            dict <- esgdict__build(esgdict__fetch_cached(
                project = private$m_project,
                token = token,
                cv_tag = cv_tag,
                request_tag = request_tag,
                policy = esgdict__cache_policy(use_cache),
                source_dir = source_dir,
                force = force
            ))
            private$replace(dict, status = "built")
            self
        },

        #' @description
        #' Return raw dictionary payload data.
        #'
        #' `$get("vocab")` returns the full vocabulary payload list.
        #' `$get("request")` and `$get("dreq")` return the request table when
        #' available. Any other value is interpreted as a vocabulary field name,
        #' such as `"experiment_id"` or `"source_id"`.
        #'
        #' @param type Data type to retrieve. Use `"vocab"`, `"request"`,
        #'   `"dreq"`, or a project vocabulary field name.
        #'
        #' @return A copy of the requested data.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$get("experiment_id")
        #' dict$get("request")
        get = function(type) {
            private$assert_has_data("get dictionary data")
            checkmate::assert_string(type, min.chars = 1L)
            type <- tolower(type)

            if (identical(type, "dreq")) {
                type <- "request"
            }
            if (identical(type, "vocab")) {
                return(data.table::copy(private$m_data$vocab))
            }
            if (identical(type, "request")) {
                return(data.table::copy(private$m_data$request))
            }

            if (!type %in% names(private$m_data$vocab)) {
                stop(sprintf("Unknown ESG dictionary data type `%s`.", type), call. = FALSE)
            }

            data.table::copy(private$m_data$vocab[[type]])
        },

        #' @description
        #' Return available dictionary capabilities.
        #'
        #' Capabilities describe whether vocabulary data, request data, and
        #' relation indices are currently available.
        #'
        #' @return A named list with `vocab`, `request`, and `relations`.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$capabilities()
        capabilities = function() {
            private$make_capabilities()
        },

        #' @description
        #' Return supported relation-index fields.
        #'
        #' Relation fields describe which field combinations can be used for
        #' constrained option discovery and cross-field legality checks.
        #'
        #' @return A named list of character vectors.
        #'
        #' @examples
        #' dict <- EsgDict$new(project = "CMIP6")
        #' dict$relation_fields()
        relation_fields = function() {
            esgdict__relation_fields(private$m_project)
        },

        #' @description
        #' Return normalized dictionary field names.
        #'
        #' Empty dictionaries return `character()`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$fields()
        fields = function() {
            values <- private$m_indices$values
            if (is.null(values) || !nrow(values)) {
                return(character())
            } else {
                sort(unique(values$field))
            }
        },

        #' @description
        #' Return normalized dictionary indices.
        #'
        #' `$indices()` returns all available indices. `$indices(type)` returns a
        #' single index table, such as `"values"`, `"variable"`,
        #' `"activity_experiment"`, or `"activity_source"`.
        #'
        #' @param type Optional index name.
        #'
        #' @return A named list of indices, or a [data.table::data.table()] when
        #'   `type` is supplied.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' names(dict$indices())
        #' dict$indices("values")
        indices = function(type = NULL) {
            private$assert_has_data("get dictionary indices")
            if (is.null(type)) {
                return(data.table::copy(private$m_indices))
            }

            checkmate::assert_string(type, min.chars = 1L)
            checkmate::assert_choice(type, names(private$m_indices))
            data.table::copy(private$m_indices[[type]])
        },

        #' @description
        #' Discover valid values for a dictionary field.
        #'
        #' Constraints supplied through `...` are used when a matching relation
        #' index exists. For example, CMIP6 `experiment_id` options can be
        #' constrained by `activity_id`.
        #'
        #' @param field ESG dictionary field name or supported alias.
        #' @param ... Optional field constraints.
        #'
        #' @return A [data.table::data.table()] with available values and
        #'   metadata. The `ignored_constraints` attribute records constraints
        #'   that could not be applied.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$options("experiment_id", activity_id = "CMIP")
        options = function(field, ...) {
            private$assert_has_data("discover ESG dictionary options")
            esgdict__options(self, field, list(...))
        },

        #' @description
        #' Check dictionary values and relationships.
        #'
        #' `$check()` validates supplied values against dictionary value indices
        #' and, when possible, validates cross-field combinations using relation
        #' indices.
        #'
        #' @param ... ESG dictionary field values.
        #' @param error If `TRUE`, throw an error when invalid values or
        #'   relationships are found.
        #' @param suggest If `TRUE`, include near-match suggestions for invalid
        #'   values.
        #' @param n_suggestions Maximum number of suggestions for each invalid
        #'   value.
        #' @param relationship Relationship validation mode. `"any"` validates
        #'   ESGF-query style OR semantics. `"all_pairs"` requires every supplied
        #'   combination inside each relation index to exist.
        #'
        #' @return An `esgdict_check_result` [data.table::data.table()].
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$check(activity_id = "CMIP", experiment_id = "historical")
        #' dict$check(variable_id = "tas", table_id = "Amon")
        check = function(
            ...,
            error = FALSE,
            suggest = TRUE,
            n_suggestions = 5L,
            relationship = c("any", "all_pairs")
        ) {
            private$assert_has_data("check ESG dictionary values")
            esgdict__check(
                self,
                list(...),
                error = error,
                suggest = suggest,
                n_suggestions = n_suggestions,
                relationship = relationship
            )
        },

        #' @description
        #' Save the dictionary to JSON.
        #'
        #' If `path = NULL`, the dictionary is saved in the package store and
        #' registered in the store manifest. If `path` is supplied, only that JSON
        #' file is written.
        #'
        #' @param path Optional JSON file path. If `NULL`, use the package store.
        #' @param allow_empty If `TRUE`, allow saving an empty dictionary.
        #'
        #' @return The normalized output path.
        #'
        #' @examples
        #' dict_path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(dict_path))
        #'
        #' path <- tempfile(fileext = ".json")
        #' dict$save(path)
        #' file.exists(path)
        save = function(path = NULL, allow_empty = FALSE) {
            checkmate::assert_flag(allow_empty)
            register <- is.null(path)
            if (self$is_empty() && !allow_empty) {
                stop(
                    "Cannot save an empty ESG Dictionary. Build or load data first, ",
                    "or use `allow_empty = TRUE` explicitly.",
                    call. = FALSE
                )
            }
            dict_id <- esgdict__dict_id(private$m_project, private$m_version, private$m_sources, private$m_built_time)
            if (is.null(path)) path <- esgdict__default_store_path(private$m_project, dict_id = dict_id)

            esgdict__save(
                private$m_project,
                private$m_profile,
                private$m_built_time,
                private$m_data,
                private$m_version,
                private$m_timestamps,
                private$m_sources,
                private$m_indices,
                path = path
            )
            if (isTRUE(register)) {
                store <- EsgStore$new(store_dir(init = TRUE))
                on.exit(store$close(), add = TRUE)
                store$register_artifact(
                    kind = "dict",
                    path = path,
                    role = "input",
                    project = private$m_project,
                    dict_id = dict_id,
                    metadata = list(
                        profile = private$m_profile,
                        version = esgdict__hashable(private$m_version),
                        built_time = if (is.null(private$m_built_time)) NULL else format(private$m_built_time, usetz = TRUE)
                    )
                )
            }
            path
        },

        #' @description
        #' Load a dictionary from JSON.
        #'
        #' If `path = NULL`, the latest stored dictionary for this project is
        #' located through the package store manifest.
        #'
        #' @param path Optional JSON file path. If `NULL`, load the latest stored
        #'   dictionary for this project.
        #'
        #' @return The modified `EsgDict` object itself.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' restored <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(restored$load(path))
        #' restored$has_data()
        load = function(path = NULL) {
            if (is.null(path)) {
                path <- esgdict__latest_store_path(private$m_project)
            }

            if (is.null(path)) {
                cli::cli_alert_info("Failed to find a stored ESG Dictionary for project {.val {private$m_project}}. Skip loading.")
                return(self)
            }

            dict <- esgdict__load(path, project = private$m_project)

            if (is.null(dict)) {
                cli::cli_alert_info("Failed to find ESG Dictionary at {.path {normalizePath(path, mustWork = FALSE)}}. Skip loading.")
                return(self)
            }

            dict <- esgdict__build(dict)
            if (is.null(dict$built_time)) {
                cli::cli_alert_success("Loaded empty ESG Dictionary.")
            } else {
                cli::cli_alert_success("Loaded ESG Dictionary that was built at {format(dict$built_time, usetz = TRUE)}.")
            }

            private$replace(dict, status = "loaded")
            self
        },

        #' @description
        #' Print a dictionary summary.
        #'
        #' @return The `EsgDict` object itself, invisibly.
        #'
        #' @examples
        #' path <- system.file("extdata", "examples", "cmip6-dict.json", package = "epwshiftr")
        #' dict <- EsgDict$new(project = "CMIP6")
        #' suppressMessages(dict$load(path))
        #' dict$print()
        print = function() {
            d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
            cli::cli_rule("ESG Dictionary")
            cli::cli_end(d)

            cli::cli_bullets(c(
                "*" = "{.strong Project}: {.val {private$m_project}}",
                "*" = "{.strong Profile}: {.val {private$m_profile}}",
                "*" = "{.strong Status}: {.val {self$status()}}"
            ))
            if (!is.null(private$m_built_time)) {
                cli::cli_bullets(c("*" = "Built at: {format(private$m_built_time, usetz = TRUE)}"))
            } else {
                cli::cli_bullets(c("*" = "Built at: {.emph <NONE>}"))
            }

            if (!self$has_data()) {
                cli::cli_h1("Vocabulary")
                cli::cli_bullets(c("*" = "{.strong Vocab Version}: {.emph <Empty>}"))
                cli::cli_h1("Request")
                cli::cli_bullets(c("*" = "{.strong Request Version}: {.emph <Empty>}"))
                return(invisible(self))
            }

            cli::cli_h1("Vocabulary")
            cli::cli_bullets(c("*" = "{.strong Vocab Version}: {.var {private$m_version$vocab}}"))

            vocab <- private$m_data$vocab
            cli::cli_bullets(c("*" = "{.strong Vocab Contents} [{length(vocab)} type{?s}]: "))
            fmt <- sprintf("{.strong %s} [%s items]", names(vocab), vapply(vocab, NROW, integer(1)))
            names(fmt) <- rep("*", length(fmt))
            div <- cli::cli_div(theme = list(".bullets .bullet-*" = list("padding-left" = 2)))
            cli::cli_bullets(fmt)
            cli::cli_end(div)

            cli::cli_h1("Request")
            if (is.null(private$m_data$request)) {
                cli::cli_bullets(c("*" = "{.strong Request Version}: {.emph <None>}"))
            } else {
                cli::cli_bullets(c("*" = "{.strong Request Version}: {.var {private$m_version$request}}"))
                request <- private$m_data$request
                meta <- attr(request, "metadata", TRUE)
                cli::cli_bullets(c("*" = "{.strong Request Contents}: {nrow(request)} Variables from {length(unique(meta$table_id))} Tables and {length(unique(meta$realm))} Realms"))
            }

            invisible(self)
        }
    ),

    private = list(
        m_project = "CMIP6",
        m_profile = "cmip6",
        m_version = NULL,
        m_status = NULL,
        m_sources = NULL,
        m_built_time = NULL,
        m_timestamps = NULL,
        m_data = list(vocab = NULL, request = NULL),
        m_indices = list(),

        replace = function(dict, status = NULL) {
            for (nm in names(dict)) {
                private[[paste0("m_", nm)]] <- dict[[nm]]
            }
            private$m_status <- status
            if (is.null(private$m_indices) && self$has_data()) {
                private$m_indices <- esgdict__indices(private$m_project, private$m_data)
            }
            invisible(self)
        },

        make_capabilities = function() {
            caps <- list(
                vocab = !is.null(private$m_data$vocab) && length(private$m_data$vocab) > 0L,
                request = !is.null(private$m_data$request) && NROW(private$m_data$request) > 0L,
                relations = setdiff(names(private$m_indices), "values")
            )
            caps
        },

        assert_has_data = function(action) {
            if (self$has_data()) {
                return(invisible(TRUE))
            }

            stop(
                sprintf(
                    "Cannot %s because the ESG dictionary status is `%s`. Build or load a complete dictionary first.",
                    action,
                    self$status()
                ),
                call. = FALSE
            )
        }
    )
)
