#' CMIP6 Controlled Vocabularies (CVs) and Data Request Dictionary
#'
#' The `CMIP6Dict` object provides functionalities to fetch the latested CMIP6
#' Controlled Vocabularies (CVs) and Data Request (DReq) information.
#'
#' The CMIP6 CVs gives a well-defined set of global attributes that are recorded
#' in each CMIP6 model output, providing information necessary for interpreting
#' the data. The data of CMIP6 CVs is stored as JSON files in the WCRP-CMIP
#' [GitHub Repo](https://github.com/WCRP-CMIP/CMIP6_CVs).
#'
#' The CMIP6 DReq defines all the quantities from CMIP6 simulations that should
#' be archived. This includes both quantities of general interest needed from
#' most of the CMIP6-endorsed model intercomparison projects (MIPs) and
#' quantities that are more specialized and only of interest to a single
#' endorsed MIP. The raw data of DReq is stored a Microsoft Excel file
#' (`CMIP6_MIP_tables.xlsx`) in a
#' [Subversion repo](http://proj.badc.rl.ac.uk/svn/exarch/CMIP6dreq/trunk).
#' The `CMIP6Dict` object uses the parsed DReq data that is stored in the
#' [GitHub Repo](https://github.com/PCMDI/cmip6-cmor-tables).
#'
#' For more information, please see:
#'
#' - [CMIP6 Global Attributes, DRS, Filenames, Directory Structure, and CV's](https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit)
#' - [CMIP6 Data Request](https://wcrp-cmip.github.io/WGCM_Infrastructure_Panel/CMIP6/data_request.html)
#'
#' @examples
#' \dontrun{
#'
#' # create a new CMIP6Dict object
#' dict <- cmip6_dict()
#'
#' # by default, there is no data when the CMIP6Dict was created
#' dict$is_empty()
#'
#' # fetch and parse all CVs and Data Request data
#' dict$build()
#'
#' # get the version of CVs nand Data Request
#' dict$version()
#'
#' # get the last modified time for each CV and Data Request
#' dict$timestamp()
#'
#' # get the time when the dict was built
#' dict$built_time()
#'
#' # get the data of CVs and DReq
#' dict$get("activity_id")
#' dict$get("experiment_id")
#' dict$get("sub_experiment_id")
#' dict$get("institution_id")
#' dict$get("source_id")
#' dict$get("table_id")
#' dict$get("frequency")
#' dict$get("grid_label")
#' dict$get("realm")
#' dict$get("source_type")
#' dict$get("dreq")
#'
#' # save the dict object for later usage
#' # default location is the value of global option "epwshiftr.dir"
#' dict$save()
#'
#' # the saved dict object can be reloaded
#' new_dict <- cmip6_dict()
#' new_dict$load()
#'
#' # print will show the version summary and the last built time
#' dict$print()
#' }
#' @author Hongyuan Jia
#'
#' @importFrom R6 R6Class
#' @name CMIP6Dict
#' @export
cmip6_dict <- function() {
    EPWSHIFTR_ENV$dict <- CMIP6Dict$new()
    EPWSHIFTR_ENV$dict
}

#' @name CMIP6Dict
#' @export
CMIP6Dict <- R6::R6Class("CMIP6Dict",
    cloneable = FALSE, lock_class = TRUE,
    public = list(
        #' @description
        #' Get the version of CVs and Data Request
        #'
        #' @return A list of two element:
        #'
        #' - `cvs`: A [numeric_version] object giving the version of CVs
        #' - `dreq`: A [numeric_version] object giving the version of Data
        #'   Request
        version = function() {
            private$m_version
        },

        #' @description
        #' Is it an empty CMIP6Dict?
        #'
        #' `$is_empty()` checks if this `CMIP6Dict` is empty, i.e. the `$build()
        #' ` or `$load()` method hasn't been called yet and there is no data of
        #' CVs and Data Request.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`.
        is_empty = function() {
            is.null(private$m_version)
        },

        #' @description
        #' Get the last modified time for CVs
        #'
        #' @return A list of 14 [DateTime][POSIXct]s:
        #' - `"cvs"`: The last modified time for the whole CV collection
        #' - `"drs"`: The last modified time for Data Reference Syntax (DRS)
        #' - `"activity_id"`: The last modified time for Activity ID
        #' - `"experiment_id"`: The last modified time for Experiment ID
        #' - `"frequency"`: The last modified time for Frequency
        #' - `"grid_label"`: The last modified time for Grid Label
        #' - `"institution_id"`: The last modified time for Institution ID
        #' - `"nominal_resolution"`: The last modified time for Nominal Resolution
        #' - `"realm"`: The last modified time for Realm
        #' - `"required_global_attributes"`: The last modified time for Required Global Attributes
        #' - `"source_id"`: The last modified time for Source ID
        #' - `"source_type"`: The last modified time for Source Type
        #' - `"sub_experiment_id"`: The last modified time for Sub-Experiment ID
        #' - `"table_id"`: The last modified time for Table ID
        timestamp = function() {
            private$m_timestamps
        },

        #' @description
        #' Get the time when the dictionary was built
        #'
        #' @return A [DateTime][POSIXct]
        built_time = function() {
            private$m_built_time
        },

        #' @description
        #' Fetch and parse all data of CVs and Data Request
        #'
        #' @param token A string of GitHub token that is used to access GitHub
        #'        REST APIs. If `NULL`, `GITHUB_PAT` or `GITHUB_TOKEN`
        #'        environment variable will be used if exists. Default: `NULL`.
        #'
        #' @return The updated `CMIP6Dict` itself.
        build = function(token = NULL) {
            dict <- cmip6dict_build(cmip6dict_fetch())
            for (nm in names(dict)) private[[paste0("m_", nm)]] <- dict[[nm]]
            self
        },

        #' @description
        #' Get the data for a specific CV or Data Request
        #'
        #' @param type A single string indicating the type of data to list.
        #' Should be one of:
        #'
        #' - `"drs"`: Data Reference Syntax (DRS)
        #' - `"activity_id"`: Activity ID
        #' - `"experiment_id"`: Experiment ID
        #' - `"frequency"`: Frequency
        #' - `"grid_label"`: Grid Label
        #' - `"institution_id"`: Institution ID
        #' - `"nominal_resolution"`: Nominal Resolution
        #' - `"realm"`: Realm
        #' - `"required_global_attributes"`: Required Global Attributes
        #' - `"source_id"`: Source ID
        #' - `"source_type"`: Source Type
        #' - `"sub_experiment_id"`: Sub-Experiment ID
        #' - `"table_id"`: Table ID
        #' - `"dreq"`: Data Request
        #'
        #' @return
        #' For `"drs"`, "activity_id"`, `"frequency"`, `"grid_label"`,
        #' `"institution_id"`, `"source_type"` and `"sub_experiment_id"`, a
        #' [list].
        #'
        #' For `"experiment_id"`, `"source_id"` and `"dreq"`, a [data.table].
        #'
        #' For `"nominal_resolution"`, `"required_global_attributes"` and
        #' `"table_id"`, a [character] vector.
        get = function(type) {
            assert_subset(type, c(tolower(CV_TYPES), "dreq"))

            if (type == "dreq") {
                data.table::copy(private$m_data$dreq)
            } else {
                data.table::copy(private$m_data$cvs[[type]])
            }
        },

        #' @description
        #' Save the CMIP6Dict object
        #'
        #' `$save()` stores all the core data of current `CMIP6Dict` object into
        #' an [RDS][saveRDS()] file named `CMIP6DICT` in the specified folder.
        #' This file can be reloaded via `$load()` method to restore the last
        #' state of current `CMIP6Dict` object.
        #'
        #' @param dir A single string giving the directory to save the RDS file.
        #'        Default is set to the global option `epwshiftr.dir`. The
        #'        directory will be created if not exists. If this global option
        #'        is not set, the current working directory is used.
        #'
        #' @return A single string giving the full path of the RDS file.
        save = function(dir = getOption("epwshiftr.dir", ".")) {
            cmip6dict_save(
                private$m_built_time,
                private$m_data,
                dir = dir
            )
        },

        #' @description
        #' Load the saved CMIP6Dict object from file
        #'
        #' `$load()` loads the RDS file named `CMIP6DICT` that is created using
        #' `$save()` method.
        #'
        #' Please note that the file should be exactly the same as `CMIP6DICT`
        #' without file extension.
        #'
        #' @param dir A single string giving the directory to find the RDS file.
        #'        Default is set to the global option `epwshiftr.dir`. If this
        #'        global option is not set, the current working directory is
        #'        used.
        #'
        #' @return A single string giving the full path of the RDS file.
        load = function(dir = getOption("epwshiftr.dir", ".")) {
            dict <- cmip6dict_load(dir)

            if (is.null(dict)) {
                cli::cli_alert_info("Failed to find file {.file CMIP6DICT} at {normalizePath(dir, mustWork = FALSE)}. Skip loading")
                return(self)
            }

            dict <- cmip6dict_build(dict)
            cli::cli_alert_success("Loaded CMIP6 Dictionary that was built at {dict$built_time}.")

            for (nm in names(dict)) private[[paste0("m_", nm)]] <- dict[[nm]]
            self
        },

        #' @description
        #' Print a summary of the current `CMIP6Dict` object
        #'
        #' `$print()` gives the summary of current `CMIP6Dict` object including
        #' the version of CVs and Data Request, and the last built time.
        #'
        #' @return The `CMIP6Dict` object itself, invisibly.
        print = function() {
            d <- cli::cli_div(
                theme = list(rule = list("line-type" = "double"))
            )
            cli::cli_rule("CMIP6 Dictionary")
            if (length(private$m_version)) {
                cli::cli_li("Built at: {private$m_built_time}")
            } else {
                cli::cli_li("Built at: {.emph <Empty>}")
            }
            cli::cli_end(d)

            d <- cli::cli_div(theme = list(`li` = list(`margin-left` = 0L, `padding-left` = 2L)))
            ul <- cli::cli_ul()

            if (!length(private$m_version)) {
                cli::cli_h1("Controlled Vocabularies (CVs)")
                cli::cli_li("{.strong CV Version}: {.emph <Empty>}")
                cli::cli_h1("Data Request (DReq)")
                cli::cli_li("{.strong DReq Version}: {.emph <Empty>}")
            } else {
                cli::cli_h1("Controlled Vocabularies (CVs)")
                cli::cli_li("{.strong CV Version}: {.var {private$m_version$cvs}}")
                cvs <- private$m_data$cvs
                cli::cli_li("{.strong CV Contents} [{length(cvs)} type{?s}]: ")
                ul2 <- cli::cli_ul()
                for (nm in names(cvs)) {
                    cli::cli_li("{.strong {nm}} [{NROW(cvs[[nm]])} item{?s}]")
                }
                cli::cli_end(ul2)

                cli::cli_h1("Data Request (DReq)")
                cli::cli_li("{.strong DReq Version}: {.var {private$m_version$dreq}}")
                dreq <- private$m_data$dreq
                meta <- attr(dreq, "metadata", TRUE)
                cli::cli_li("{.strong DReq Contents}: {nrow(dreq)} Variables from {length(unique(meta$table_id))} Tables and {length(unique(meta$realm))} Realms")
            }

            cli::cli_end(ul)
            cli::cli_end(d)

            invisible(self)
        }
    ),

    private = list(
        # CV and Data Request versions
        m_version = NULL,
        # the time when the dict was last built
        m_built_time = NULL,
        # modified time for CV and its components
        m_timestamps = NULL,
        # tables for all CV and Data Request
        m_data = NULL
    )
)

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

cmip6dict_fetch <- function() {
    cli::cli_progress_step(
        "Fetching {.strong CMIP6 Dictionary}...",
        "Fetched {.strong CMIP6 Dictionary} successfully at {Sys.time()}",
        "Failed to fetch {.strong CMIP6 Dictionary}.",
        spinner = TRUE
    )

    cvs <- cmip6dict_fetch_cv()
    dreq <- cmip6dict_fetch_dreq()
    built_time <- Sys.time()

    list(cvs = cvs, dreq = dreq, built_time = built_time)
}

cmip6dict_build <- function(dict) {
    res <- list()
    res$built_time <- dict$built_time
    dict$built_time <- NULL

    res$version = list(
        cvs = attr(dict$cvs$drs, "version", TRUE)$CV_collection_version,
        dreq = attr(dict$dreq, "metadata", TRUE)$dreq_version[[1L]]
    )

    res$timestamps <- c(
        list(cvs = dict$cvs$CV_collection_modified),
        lapply(dict$cvs, function(cv) attr(cv, "version", TRUE)$CV_modified)
    )

    res$data <- dict

    res
}

cmip6dict_fetch_cv_tag_latest <- function(token = NULL) {
    cli::cli_progress_step(
        "Fetching latest tag of {.strong CMIP6 CVs}...",
        "Fetched latest tag of {.strong CMIP6 CVs} successfully.",
        "Failed to fetched latest tag of {.strong CMIP6 CVs}.",
        spinner = TRUE
    )
    gh_tags(REPO_CV, token)[[1L]]
}

cmip6dict_download_cv_file <- function(tag, dir = tempdir(), token = NULL) {
    dests <- character(length(CV_TYPES))
    names(dests) <- CV_TYPES

    file <- ""
    cli::cli_progress_step(
        "Downloading data of {.strong CMIP6 CVs} ['{.file {file}}']...",
        "Downloaded data of {.strong CMIP6 CVs} successfully.",
        "Failed to download data of {.strong CMIP6 CVs}.",
        spinner = TRUE
    )
    for (type in CV_TYPES) {
        file <- sprintf("CMIP6_%s.json", type)
        cli::cli_progress_update(1L)
        dests[[type]] <- download_gh_file(REPO_CV, tag, file, dir, token)
    }

    dests
}

cmip6dict_fetch_cv <- function(tag = NULL, token = NULL) {
    if (is.null(tag)) {
        tag <- cmip6dict_fetch_cv_tag_latest(token)$name
    }
    files <- cmip6dict_download_cv_file(tag)

    cvs <- list()
    for (type in names(files)) {
        abbr <- tolower(tools::file_path_sans_ext(type))
        cvs[[abbr]] <- match.fun(sprintf("cmip6dict_parse_cv_%s", abbr))(files[[type]])
    }

    cvs
}

cmip6dict_format_cv_nest <- function(json) {
    transposed <- lapply(names(json[[1L]]), function(nm) lapply(json, "[[", nm))
    setnames(as.data.table(transposed), names(json[[1L]]))
}

cmip6dict_parse_cv_version_metadata <- function(lst) {
    res <- list()

    # make sure the locale for time is set to "C"
    ori <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", ori), add = TRUE)
    Sys.setlocale("LC_TIME", "C")

    res$CV_collection_version <- as.numeric_version(lst$CV_collection_version)
    res$CV_collection_modified <- as.POSIXct(
        lst$CV_collection_modified, format = "%c %z", tz = "UTC"
    )

    # fix malformed timezone spec
    type_modified <- names(lst)[endsWith(names(lst), "CV_modified")]
    res$CV_modified <- gsub(" 0(\\d{4})$", " -\\1", lst[[type_modified]])

    # fix malformed abbr weekday spec
    res$CV_modified <- gsub("Tues", "Tue", res$CV_modified, fixed = TRUE)
    res$CV_modified <- as.POSIXct(res$CV_modified, format = "%c %z", tz = "UTC")

    res$CV_note <- lst[[names(lst)[endsWith(names(lst), "CV_note")]]]

    res
}

cmip6dict_parse_cv_vec <- function(file, subclass = NULL) {
    json <- jsonlite::read_json(file)
    res <- unlst(json[[1L]])
    setattr(res, "version",
        cmip6dict_parse_cv_version_metadata(json$version_metadata)
    )

    structure(res, class = c(subclass, "CMIP6CV", typeof(res)))
}

cmip6dict_parse_cv_list <- function(file, subclass = NULL) {
    json <- jsonlite::read_json(file)
    res <- json[[1L]]
    setattr(res, "version",
        cmip6dict_parse_cv_version_metadata(json$version_metadata)
    )

    structure(res, class = c(subclass, "CMIP6CV", "list"))
}

cmip6dict_parse_cv_drs <- function(file) {
    cmip6dict_parse_cv_list(file, "CMIP6CV_DRS")
}

cmip6dict_parse_cv_activity_id <- function(file) {
    cmip6dict_parse_cv_list(file, "CMIP6CV_ActivityId")
}

cmip6dict_parse_cv_experiment_id <- function(file) {
    json <- jsonlite::read_json(file)
    d <- cmip6dict_format_cv_nest(json[[1L]])

    setcolorder(d, "experiment_id")

    cols_lst <- c("activity_id", "additional_allowed_model_components",
        "parent_activity_id", "parent_experiment_id",
        "required_model_components", "sub_experiment_id")
    for (col in cols_lst) {
        set(d, NULL, col, lapply(d[[col]], unlist, FALSE, FALSE))
    }

    cols_flat <- c("experiment_id", "description", "end_year", "experiment",
        "min_number_yrs_per_sim", "start_year", "tier")
    for (col in cols_flat) {
        set(d, NULL, col, unlist(d[[col]], FALSE, FALSE))
    }

    cols_int <- c("end_year", "min_number_yrs_per_sim", "start_year", "tier")
    for (col in cols_int) {
        set(d, NULL, col, suppressWarnings(as.integer(d[[col]])))
    }

    setcolorder(d, c(
         "experiment_id", "experiment", "description", "tier",
        "start_year", "end_year", "min_number_yrs_per_sim",
        "required_model_components",
        "parent_experiment_id", "sub_experiment_id",
        "activity_id", "parent_activity_id",
        "additional_allowed_model_components"
    ))
    setattr(d, "version", cmip6dict_parse_cv_version_metadata(json$version_metadata))

    structure(d, class = c("CMIP6CV_ExperimentId", "CMIP6CV", class(d)))
}

cmip6dict_parse_cv_frequency <- function(file) {
    cmip6dict_parse_cv_list(file, "CMIP6CV_Frequency")
}

cmip6dict_parse_cv_grid_label <- function(file) {
    cmip6dict_parse_cv_list(file, "CMIP6CV_GridLabel")
}

cmip6dict_parse_cv_institution_id <- function(file) {
    cmip6dict_parse_cv_list(file, "CMIP6CV_InstitutionId")
}

cmip6dict_parse_cv_nominal_resolution <- function(file) {
    cmip6dict_parse_cv_vec(file, "CMIP6CV_Resolution")
}

cmip6dict_parse_cv_realm <- function(file) {
    cmip6dict_parse_cv_list(file, "CMIP6CV_Realm")
}

cmip6dict_parse_cv_required_global_attributes <- function(file) {
    cmip6dict_parse_cv_vec(file, "CMIP6CV_ReqGlobAttr")
}

cmip6dict_parse_cv_source_id <- function(file) {
    json <- jsonlite::read_json(file)
    d <- cmip6dict_format_cv_nest(json[[1L]])

    setcolorder(d, "source_id")

    cols_lst <- c("activity_participation", "institution_id")
    for (col in cols_lst) {
        set(d, NULL, col, lapply(d[[col]], unlst))
    }

    cols_flat <- c("source_id", "release_year", "cohort", "label",
        "label_extended")
    for (col in cols_flat) {
        set(d, NULL, col, unlist(d[[col]], FALSE, FALSE))
    }

    cols_int <- c("release_year")
    for (col in cols_int) {
        set(d, NULL, col, suppressWarnings(as.integer(d[[col]])))
    }

    setcolorder(d, c(
        "source_id", "release_year", "institution_id", "label", "label_extended",
        "cohort", "activity_participation", "model_component", "license_info"
    ))
    setattr(d, "version", cmip6dict_parse_cv_version_metadata(json$version_metadata))

    structure(d, class = c("CMIP6CV_SourceId", "CMIP6CV", class(d)))
}

cmip6dict_parse_cv_source_type <- function(file) {
    cmip6dict_parse_cv_list(file, "CMIP6CV_SourceType")
}

cmip6dict_parse_cv_sub_experiment_id <- function(file) {
    cmip6dict_parse_cv_list(file, "CMIP6CV_SubExperimentId")
}

cmip6dict_parse_cv_table_id <- function(file) {
    cmip6dict_parse_cv_vec(file, "CMIP6CV_TableId")
}

print_trunc <- function(x, n) {
    d <- cli::cli_div(theme = list(
        body = list(`padding-left` = 0L, `margin-left` = 0L)
    ))
    if (!is.data.frame(x)) {
        total <- length(x)
        if (n < total) {
            cli::cli_text(cli::col_grey("# ... with {total - n} more item{?s}"))
        }
    } else {
        total <- nrow(x)
        if (n < total) {
            cli::cli_text()
            cli::cli_text(cli::col_grey("# ... with {total - n} more item{?s}"))
        }
    }
    cli::cli_end(d)
}

print_list <- function(x, elem = "") {
    if (!length(x)) return()

    if (length(x) > 1L) {
        cli::cli_li("{.strong {to_title_case(elem)}}:")
        ul <- cli::cli_ul()
        for (nm in names(x)) {
            print_list(x[[nm]], nm)
        }
        cli::cli_end(ul)
    } else {
        cli::cli_li("{.strong {to_title_case(elem)}}: {.val {unlst(x)}}")
    }
}

cmip6dict_print_cv_rule <- function(name) {
    d <- cli::cli_div(
        theme = list(rule = list("line-type" = "double"))
    )
    cli::cli_rule("{.strong CMIP6CV {name}}", right = "{.strong CMIP6 Dictionary}")
    cli::cli_end(d)
}

cmip6dict_print_cv_version <- function(cv, name = "") {
    ver <- attr(cv, "version", TRUE)
    if (is.null(ver) || !length(ver)) return(invisible(cv))

    cli::cli_h1("<VERSION METADATA>")

    d <- cli::cli_div(theme = list(`li` = list(`margin-left` = 0L, `padding-left` = 2L)))
    ul <- cli::cli_ul()
    cli::cli_li("{.strong CV Version}: {.var {ver$CV_collection_version}}")
    cli::cli_li("{.strong CV Modified}: {format(ver$CV_collection_modified, '%F %T %Z')}")
    cli::cli_li("{.strong {name} Modified}: {format(ver$CV_modified, '%F %T %Z')}")
    cli::cli_li("{.strong {name} Note}: {.val {ver$CV_note}}")
    cli::cli_end(ul)
    cli::cli_end(d)

    invisible(cv)
}

cmip6dict_print_cv_vec <- function(cv, n = 5L) {
    cli::cli_h1("<STORED TYPE>")
    cli::cli_li("{.strong Stored type}: {.cls {typeof(cv)}}")

    cli::cli_h1("<VALUES>")

    n <- min(n, length(cv))
    nms <- to_title_case(names(cv))

    txt <- cli::cli_vec(unclass(cv), list(vec_trunc = n))
    cli::cli_text("{.val {txt}}")

    print_trunc(cv, n)

    invisible(cv)
}

cmip6dict_print_cv_list <- function(cv, n = 5L, to_title = FALSE) {
    cli::cli_h1("<STORED TYPE>")
    cli::cli_li("{.strong Stored type}: {.cls list}")

    cli::cli_h1("<VALUES>")

    n <- min(n, length(cv))
    nms <- names(cv)
    if (to_title) nms <- to_title_case(nms)

    d <- cli::cli_div(theme = list(
        ul = list(`margin-left` = 0L, `padding-left` = 0L),
        li = list(`margin-left` = 0L, `padding-left` = 2L)
    ))
    d <- cli::cli_div()
    ul <- cli::cli_ul()
    for (i in seq.int(n)) {
        cli::cli_li("{.strong {nms[i]}}: {.val {unlst(cv[i])}}")
    }
    cli::cli_end(ul)
    cli::cli_end(d)

    print_trunc(cv, n)

    invisible(cv)
}

cmip6dict_print_cv_table <- function(cv, n = 3L) {
    n <- min(n, nrow(cv))
    cols <- names(cv)

    cli::cli_h1("<STORED TYPE>")
    cli::cli_li("Stored type: {.cls data.table}")

    cli::cli_h1("<VALUES>")
    for (i in seq.int(n)) {
        dt <- cv[i]
        d <- cli::cli_div(theme = list(
            h2 = list("margin-left" = 2L, "margin-bottom" = 0L), li = list("padding-left" = 2L)
        ))
        cli::cli_h2("[{to_title_case(cols[1])}: {.strong {.val {dt[[cols[1]]]}}}]")
        ul <- cli::cli_ul()
        for (col in cols[-1L]) {
            if (is.list(dt[[col]][[1L]])) {
                print_list(dt[[col]][[1L]], col)
            } else {
                cli::cli_li("{.strong {to_title_case(col)}}: {.val {unlst(dt[[col]])}}")
            }
        }
        cli::cli_end(ul)
        cli::cli_end(d)
    }

    print_trunc(cv, n)

    invisible(cv)
}

#' @export
print.CMIP6CV <- function(x, n = NULL, ...) {
    cls <- sub("CMIP6CV_", "", class(x)[[1L]], fixed = TRUE)
    if (is.null(n)) {
        n <- if (is.data.frame(x)) 3L else if (is.list(x)) 5L else 10L
    }
    switch(cls,
        "DRS" = {
            cmip6dict_print_cv_rule("Data Reference Syntax (DRS)")
            cmip6dict_print_cv_version(x, "DRS")
            cmip6dict_print_cv_list(x, n, TRUE)
        },
        "ActivityId" = {
            cmip6dict_print_cv_rule("Activity ID")
            cmip6dict_print_cv_version(x, "ActivityId")
            cmip6dict_print_cv_list(x, n)
        },
        "ExperimentId" = {
            cmip6dict_print_cv_rule("Experiment ID")
            cmip6dict_print_cv_version(x, "ExperimentId")
            cmip6dict_print_cv_table(x, n)
        },
        "Frequency" = {
            cmip6dict_print_cv_rule("Frequency")
            cmip6dict_print_cv_version(x, "Frequency")
            cmip6dict_print_cv_list(x, n)
        },
        "GridLabel" = {
            cmip6dict_print_cv_rule("Grid Label")
            cmip6dict_print_cv_version(x, "GridLabel")
            cmip6dict_print_cv_list(x, n)
        },
        "InstitutionId" = {
            cmip6dict_print_cv_rule("Institution ID")
            cmip6dict_print_cv_version(x, "InstitutionId")
            cmip6dict_print_cv_list(x, n)
        },
        "Resolution" = {
            cmip6dict_print_cv_rule("Nominal Resolution")
            cmip6dict_print_cv_version(x, "NominalResolution")
            cmip6dict_print_cv_vec(x, n)
        },
        "Realm" = {
            cmip6dict_print_cv_rule("Realm")
            cmip6dict_print_cv_version(x, "Realm")
            cmip6dict_print_cv_list(x, n)
        },
        "ReqGlobAttr" = {
            cmip6dict_print_cv_rule("Required Global Attributes")
            cmip6dict_print_cv_version(x, "ReqGlobAttr")
            cmip6dict_print_cv_vec(x, n)
        },
        "SourceId" = {
            cmip6dict_print_cv_rule("Source ID")
            cmip6dict_print_cv_version(x, "SourceId")
            cmip6dict_print_cv_table(x, n)
        },
        "SourceType" = {
            cmip6dict_print_cv_rule("Source Type")
            cmip6dict_print_cv_version(x, "SourceType")
            cmip6dict_print_cv_list(x, n)
        },
        "SubExperimentId" = {
            cmip6dict_print_cv_rule("Sub Experiment ID")
            cmip6dict_print_cv_version(x, "SubExperimentId")
            cmip6dict_print_cv_list(x, n)
        },
        "TableId" = {
            cmip6dict_print_cv_rule("Table ID")
            cmip6dict_print_cv_version(x, "TableId")
            cmip6dict_print_cv_vec(x, n)
        }
    )

    invisible(x)
}

cmip6dict_fetch_dreq_tag_latest <- function(token = NULL) {
    cli::cli_progress_step(
        "Fetching latest tag of {.strong CMIP6 DReq}...",
        "Fetched latest tag of {.strong CMIP6 DReq} successfully.",
        "Failed to fetched latest tag of {.strong CMIP6 DReq}.",
        spinner = TRUE
    )
    gh_tags(REPO_DREQ, token)[[1L]]
}

cmip6dict_download_dreq_file <- function(tag, dir = tempdir(), token = NULL) {
    cli::cli_progress_step(
        "Downloading data of {.strong CMIP6 DReq}...",
        "Downloaded data of {.strong CMIP6 DReq} successfully.",
        "Failed to download data of {.strong CMIP6 DReq}.",
        spinner = TRUE
    )
    zipball <- download_gh_tag(REPO_DREQ, tag, dir, token)

    files <- utils::unzip(zipball, exdir = dir)

    files <- files[basename(dirname(files)) == "Tables"]
    names(files) <- basename(files)

    exclu <- c("CV", "coordinate", "formula_terms", "grids", "input_example")
    files[!names(files) %in% sprintf("CMIP6_%s.json", exclu)]
}

cmip6dict_parse_dreq_header <- function(lst) {
    res <- list()

    # make sure the locale for time is set to "C"
    ori <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", ori), add = TRUE)
    Sys.setlocale("LC_TIME", "C")

    res$dreq_version <- numeric_version(lst$data_specs_version)
    res$cmor_version <- numeric_version(lst$cmor_version)
    res$table_id <- sub("Table ", "", lst$table_id, fixed = TRUE)
    res$table_date <- as.Date(lst$table_date, format = "%d %b %Y")
    res$realm <- lst$realm
    res$dbl_missing_value <- as.double(lst$missing_value)
    res$int_missing_value <- as.integer(lst$int_missing_value)
    res$mip_era <- lst$mip_era
    res$conventions <- lst$Conventions

    res
}

cmip6dict_parse_dreq_file <- function(file) {
    json <- jsonlite::read_json(file)
    header <- cmip6dict_parse_dreq_header(json[["Header"]])

    d <- cmip6dict_format_cv_nest(json[["variable_entry"]])
    set(d, NULL, "variable", names(json[["variable_entry"]]))
    setcolorder(d, "variable")

    cols_flat <- names(d)
    empty_to_na <- function(x) {x[x == ""] <- NA_character_; x}
    for (col in cols_flat) {
        set(d, NULL, col, empty_to_na(unlist(d[[col]], FALSE, FALSE)))
    }

    setattr(d, "metadata", header)

    d
}

cmip6dict_fetch_dreq <- function(tag = NULL, token = NULL) {
    if (is.null(tag)) {
        tag <- cmip6dict_fetch_dreq_tag_latest(token)$name
    }
    files <- cmip6dict_download_dreq_file(tag)

    dreq <- lapply(files, cmip6dict_parse_dreq_file)
    metadata <- lapply(dreq, attr, "metadata", TRUE)

    for (nm in names(dreq)) {
        set(dreq[[nm]], NULL, "table_id", metadata[[nm]][["table_id"]])
    }

    dreq <- rbindlist(dreq, use.names = TRUE)
    setcolorder(dreq, c("variable", "table_id", "modeling_realm", "standard_name", "long_name"))
    structure(dreq,
        metadata = rbindlist(metadata, use.names = TRUE),
        class = c("CMIP6DReq", class(dreq))
    )
}

cmip6dict_print_dreq_rule <- function() {
    d <- cli::cli_div(
        theme = list(rule = list("line-type" = "double"))
    )
    cli::cli_rule("{.strong CMIP6 Data Request}", right = "{.strong CMIP6 Dictionary}")
    cli::cli_end(d)
}

cmip6dict_print_dreq_meta <- function(dreq) {
    meta <- attr(dreq, "metadata", TRUE)
    if (is.null(meta) || !length(meta)) return(invisible(dreq))

    cli::cli_h1("<HEADER METADATA>")

    d <- cli::cli_div(theme = list(`li` = list(`margin-left` = 0L, `padding-left` = 2L)))
    ul <- cli::cli_ul()
    cli::cli_li("{.strong DReq Version}: {.var {meta$dreq_version[[1L]]}}")
    cli::cli_li("{.strong CMOR Version}: {.var {meta$cmor_version[[1L]]}}")
    cli::cli_li("{.strong MIP Era}: {.var {meta$mip_era[[1L]]}}")
    cli::cli_li("{.strong Missing Value}:")
    d2 <- cli::cli_div(theme = list(`li` = list(`margin-left` = 2L, `padding-left` = 2L)))
    ul2 <- cli::cli_ul()
    cli::cli_li("Real: {.var {meta$dbl_missing_value[[1L]]}}")
    cli::cli_li("Int: {.var {meta$int_missing_value[[1L]]}}")
    cli::cli_end(ul2)
    cli::cli_end(d2)
    cli::cli_li("{.strong Conventions}: {.var {meta$conventions[[1L]]}}")
    cli::cli_li("{.var {nrow(dreq)}} Variables from {.var {length(unique(meta$table_id))}} Tables and {.var {length(unique(meta$realm))}} Realms")
    cli::cli_end(ul)
    cli::cli_end(d)

    invisible(dreq)
}

#' @export
print.CMIP6DReq <- function(x, n = 3L, ...) {
    cmip6dict_print_dreq_rule()
    cmip6dict_print_dreq_meta(x)
    cmip6dict_print_cv_table(x, n)
    invisible(x)
}

cmip6dict_save <- function(built_time, data, dir = getOption("epwshiftr.dir", ".")) {
    dict <- list(cvs = data$cvs, dreq = data$dreq, built_time = built_time)

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    f <- normalizePath(file.path(dir, "CMIP6DICT"), mustWork = FALSE)
    saveRDS(dict, f)
    f
}

cmip6dict_load <- function(dir = getOption("epwshiftr.dir", ".")) {
    path <- normalizePath(file.path(dir, "CMIP6DICT"), mustWork = FALSE)
    if (!file.exists(path)) return(NULL)

    val <- readRDS(path)

    if (!identical(names(val), c("cvs", "dreq", "built_time"))) {
        cli::cli_abort("Malformed format of {.file CMIP6DICT} found.")
    }

    for (nm in names(val$cvs)) {
        cls <- class(val$cvs[[nm]])
        if (data.table::is.data.table(val$cvs[[nm]])) setDT(val$cvs[[nm]])
        setattr(val$cvs[[nm]], "class", cls)
    }
    setDT(val$dreq)

    val
}
