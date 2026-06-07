ESGDICT_FORMAT <- "epwshiftr_esg_dict"
ESGDICT_FORMAT_VERSION <- "1"
ESGDICT_IMPLEMENTED_PROFILES <- c(CMIP6 = "cmip6")

CMIP6DICT_VARIANT_PATTERN <- "^r\\d+i\\d+p\\d+f\\d+$"

REPO_CV <- "WCRP-CMIP/CMIP6_CVs"
REPO_DREQ <- "PCMDI/cmip6-cmor-tables"

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

esgdict__profile <- function(project = "CMIP6") {
    project <- esgdict__normalize_project(project)
    if (project %in% names(ESGDICT_IMPLEMENTED_PROFILES)) {
        return(unname(ESGDICT_IMPLEMENTED_PROFILES[[project]]))
    }
    tolower(project)
}

esgdict__default_file <- function(project = "CMIP6") {
    sprintf("%sDICT.json", esgdict__normalize_project(project))
}

esgdict__default_env <- function() {
    if (is.null(this$dicts)) {
        this$dicts <- new.env(parent = emptyenv())
    }
    this$dicts
}

esgdict__assert_implemented <- function(project) {
    project <- esgdict__normalize_project(project)
    profile <- esgdict__profile(project)
    if (!identical(profile, "cmip6")) {
        stop(
            sprintf("ESG dictionary project `%s` is not implemented.", project),
            call. = FALSE
        )
    }
    invisible(TRUE)
}

esgdict__cache_policy <- function(use_cache = TRUE) {
    checkmate::assert_flag(use_cache)

    mode <- cache_mode()
    if (identical(mode, "offline") && !isTRUE(use_cache)) {
        stop(
            "Cannot build an ESG dictionary with `use_cache = FALSE` in offline cache mode.",
            call. = FALSE
        )
    }

    # This is the compatibility bridge between the older per-call `use_cache`
    # flag and the package-wide `epwshiftr.cache` mode used by `DiskCache`.
    enabled <- isTRUE(use_cache) && !identical(mode, "off")
    list(
        mode = mode,
        read = enabled,
        write = enabled && identical(mode, "normal"),
        raw_read = enabled,
        raw_write = enabled && identical(mode, "normal"),
        offline = identical(mode, "offline")
    )
}

#' ESG Project Dictionary
#'
#' `EsgDict` stores project-specific ESG controlled vocabulary data, query
#' indices, and validation metadata for local option discovery and legality
#' checks. The current implementation provides a CMIP6 backend only.
#'
#' @param project ESG project identifier. `"CMIP6"` is the only implemented
#'   backend.
#'
#' @seealso [esgdict_option()] and [esgdict_check()] for user-facing discovery
#' and validation helpers.
#'
#' @examples
#' \dontrun{
#' dict <- esgdict(project = "CMIP6")
#' dict$build(cv_tag = "6.2.58", dreq_tag = "01.00.33")
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
            has_cvs <- !is.null(private$m_data$cvs) && length(private$m_data$cvs) > 0L
            has_dreq <- !is.null(private$m_data$dreq) && nrow(private$m_data$dreq) > 0L

            if (!has_cvs && !has_dreq) {
                "empty"
            } else if (has_cvs && has_dreq && !is.null(private$m_version)) {
                if (!is.null(private$m_status)) private$m_status else "loaded"
            } else if (has_cvs && has_dreq) {
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
            dreq_tag = NULL,
            use_cache = TRUE,
            cache_dir = cmip6dict__raw_cache_dir()
        ) {
            esgdict__assert_implemented(private$m_project)
            checkmate::assert_flag(force)
            checkmate::assert_flag(use_cache)
            checkmate::assert_string(cv_tag, null.ok = TRUE)
            checkmate::assert_string(dreq_tag, null.ok = TRUE)

            # Empty dictionaries need data, but only an explicit user `force`
            # should bypass the parsed dictionary cache.
            rebuild <- force || self$is_empty()
            if (!rebuild) return(self)

            dict <- cmip6dict__build(cmip6dict__fetch_cached(
                token = token,
                cv_tag = cv_tag,
                dreq_tag = dreq_tag,
                policy = esgdict__cache_policy(use_cache),
                cache_dir = cache_dir,
                force = force
            ))
            private$replace(dict, status = "built")
            self
        },

        get = function(type) {
            private$assert_has_data("get dictionary data")
            checkmate::assert_string(type, min.chars = 1L)
            type <- tolower(type)
            checkmate::assert_choice(type, c(tolower(CV_TYPES), "dreq"))

            if (type == "dreq") {
                data.table::copy(private$m_data$dreq)
            } else {
                data.table::copy(private$m_data$cvs[[type]])
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
            esgdict__assert_implemented(private$m_project)
            private$assert_has_data("discover ESG dictionary options")
            cmip6dict__options(self, field, list(...))
        },

        check = function(
            ...,
            error = FALSE,
            suggest = TRUE,
            n_suggestions = 5L,
            relationship = c("any", "all_pairs")
        ) {
            esgdict__assert_implemented(private$m_project)
            private$assert_has_data("check ESG dictionary values")
            cmip6dict__check(
                self,
                list(...),
                error = error,
                suggest = suggest,
                n_suggestions = n_suggestions,
                relationship = relationship
            )
        },

        save = function(
            dir = getOption("epwshiftr.dir", "."),
            file = NULL,
            allow_empty = FALSE
        ) {
            checkmate::assert_flag(allow_empty)
            if (is.null(file)) file <- esgdict__default_file(private$m_project)
            if (self$is_empty() && !allow_empty) {
                stop(
                    "Cannot save an empty ESG Dictionary. Build or load data first, ",
                    "or use `allow_empty = TRUE` explicitly.",
                    call. = FALSE
                )
            }

            esgdict__save(
                private$m_project,
                private$m_profile,
                private$m_built_time,
                private$m_data,
                private$m_version,
                private$m_timestamps,
                private$m_sources,
                private$m_indices,
                dir = dir,
                file = file
            )
        },

        load = function(dir = getOption("epwshiftr.dir", "."), file = NULL) {
            if (is.null(file)) file <- esgdict__default_file(private$m_project)
            dict <- esgdict__load(dir, file = file, project = private$m_project)

            if (is.null(dict)) {
                cli::cli_alert_info("Failed to find file {.file {file}} at {.path {normalizePath(dir, mustWork = FALSE)}}. Skip loading.")
                return(self)
            }

            esgdict__assert_implemented(dict$project)
            dict <- cmip6dict__build(dict)
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
                cli::cli_h1("Controlled Vocabularies (CVs)")
                cli::cli_bullets(c("*" = "{.strong CV Version}: {.emph <Empty>}"))
                cli::cli_h1("Data Request (DReq)")
                cli::cli_bullets(c("*" = "{.strong DReq Version}: {.emph <Empty>}"))
                return(invisible(self))
            }

            cli::cli_h1("Controlled Vocabularies (CVs)")
            cli::cli_bullets(c("*" = "{.strong CV Version}: {.var {private$m_version$cvs}}"))

            cvs <- private$m_data$cvs
            cli::cli_bullets(c("*" = "{.strong CV Contents} [{length(cvs)} type{?s}]: "))
            fmt <- sprintf("{.strong %s} [%s items]", names(cvs), vapply(cvs, NROW, integer(1)))
            names(fmt) <- rep("*", length(fmt))
            div <- cli::cli_div(theme = list(".bullets .bullet-*" = list("padding-left" = 2)))
            cli::cli_bullets(fmt)
            cli::cli_end(div)

            cli::cli_h1("Data Request (DReq)")
            cli::cli_bullets(c("*" = "{.strong DReq Version}: {.var {private$m_version$dreq}}"))
            dreq <- private$m_data$dreq
            meta <- attr(dreq, "metadata", TRUE)
            cli::cli_bullets(c("*" = "{.strong DReq Contents}: {nrow(dreq)} Variables from {length(unique(meta$table_id))} Tables and {length(unique(meta$realm))} Realms"))

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
        m_data = list(cvs = NULL, dreq = NULL),
        m_indices = list(),

        replace = function(dict, status = NULL) {
            for (nm in names(dict)) {
                private[[paste0("m_", nm)]] <- dict[[nm]]
            }
            private$m_status <- status
            if (is.null(private$m_indices) && self$has_data()) {
                private$m_indices <- cmip6dict__indices(private$m_data)
            }
            invisible(self)
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
