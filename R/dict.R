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
    artifacts <- data.table::as.data.table(DBI::dbReadTable(priv(store)$conn, "artifact"))
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
#' `EsgDict` stores project-specific ESG controlled vocabulary data, query
#' indices, and validation metadata for local option discovery and legality
#' checks.
#'
#' @param project ESG project identifier, such as `"CMIP6"` or `"CMIP6PLUS"`.
#'
#' @seealso [esgdict_option()] and [esgdict_check()] for user-facing discovery
#' and validation helpers.
#'
#' @examples
#' \dontrun{
#' dict <- esgdict(project = "CMIP6")
#' dict$build(cv_tag = "6.2.58", request_tag = "01.00.33")
#' dict$save()
#'
#' esgdict_set_default(dict)
#' esgdict_option("experiment_id", activity_id = "CMIP")
#' esgdict_check(activity = "CMIP", experiment = "historical")
#' }
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
#' @return `esgdict_set_default()` returns `dict`, invisibly.
#'   `esgdict_get_default()` returns the current package-level default
#'   dictionary for `project`, or `NULL`.
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
        initialize = function(project = "CMIP6") {
            private$m_project <- esgdict__normalize_project(project)
            private$m_profile <- esgdict__profile(private$m_project)
        },

        project = function() {
            private$m_project
        },

        profile = function() {
            private$m_profile
        },

        version = function() {
            private$m_version
        },

        sources = function() {
            private$m_sources
        },

        timestamp = function() {
            private$m_timestamps
        },

        built_time = function() {
            private$m_built_time
        },

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

        has_data = function() {
            identical(self$status(), "built") || identical(self$status(), "loaded")
        },

        is_empty = function() {
            identical(self$status(), "empty")
        },

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

        capabilities = function() {
            private$make_capabilities()
        },

        relation_fields = function() {
            esgdict__relation_fields(private$m_project)
        },

        fields = function() {
            values <- private$m_indices$values
            if (is.null(values) || !nrow(values)) {
                return(character())
            } else {
                sort(unique(values$field))
            }
        },

        indices = function(type = NULL) {
            private$assert_has_data("get dictionary indices")
            if (is.null(type)) {
                return(data.table::copy(private$m_indices))
            }

            checkmate::assert_string(type, min.chars = 1L)
            checkmate::assert_choice(type, names(private$m_indices))
            data.table::copy(private$m_indices[[type]])
        },

        options = function(field, ...) {
            private$assert_has_data("discover ESG dictionary options")
            esgdict__options(self, field, list(...))
        },

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
