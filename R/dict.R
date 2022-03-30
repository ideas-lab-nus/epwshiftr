#' CMIP6 Controlled Vocabularies (CVs) and Data Request Dictionary
#'
#' The `CMIP6Dict` object provides functionalities to fetch the latested CMIP6
#' Controlled Vocabularies (CVs) and Data Request information.
#'
#' The CMIP6 CVs defines a well-defined set of global attributes that are
#' recorded in each CMIP6 model output, providing information necessary for
#' interpreting the data.
#'
#' The CMIP6 Data Request defines all the quantities from CMIP6 simulations that
#' should be archived. This includes both quantities of general interest needed
#' from most of the CMIP6-endorsed model intercomparison projects (MIPs) and
#' quantities that are more specialized and only of interest to a single
#' endorsed MIP.
#'
#' The data of CVs for use in CMIP6 is stored as JSON files in a
#' [GitHub Repo](https://github.com/WCRP-CMIP/CMIP6_CVs), while the Data Request
#' data is stored a Microsoft Excel file (`CMIP6_MIP_tables.xlsx`) in a
#' [Subversion repo](http://proj.badc.rl.ac.uk/svn/exarch/CMIP6dreq/trunk).
#' `CMIP6Dict` object is able to fetch those files and parse them.
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
#' # check if there is updates of CVs and Data Request
#' dict$update()
#'
#' # list the data of CVs
#' dict$list("activity")
#' dict$list("experiment")
#' dict$list("sub_experiment")
#' dict$list("institution")
#' dict$list("source")
#' dict$list("table")
#' dict$list("frequency")
#' dict$list("grid_label")
#' dict$list("realm")
#' dict$list("source_type")
#' dict$list("req_global_atts")
#' dict$list("variable")
#'
#' # save the dict object for later usage
#' # default location is the value of global option "epwshiftr.dir"
#' dict$save()
#'
#' # the saved dict object can be reloaded
#' new_dict <- cmip6_dict()
#' new_dict$load()
#'
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
        #' - `cv`: A [numeric_version] object giving the version of CVs
        #' - `req`: A [numeric_version] object giving the version of Data
        #'   Request
        version = function() {
            private$m_version
        },

        #' @description
        #' Is it an empty CMIP6Dict?
        #'
        #' `$is_empty()` checks if this `CMIP6Dict` is empty, i.e. the
        #' `$build()` or `$load()` method hasn't been called yet and there is no
        #' data of CVs and Data Request.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`.
        is_empty = function() {
            is.null(private$m_version)
        },

        #' @description
        #' Get the last modified time for CVs
        #'
        #' @return A list of 14 [DateTime][POSIXct]s:
        #'
        #' - `cv`: The last modified time for the whole CV collection
        #' - `activity`: The last modified time for the CV `activity_id`
        #' - `experiment`: The last modified time for the CV `experiment_id`
        #' - `sub_experiment`: The last modified time for the CV `sub_experiment_id`
        #' - `institution`: The last modified time for the CV `institution_id`
        #' - `source`: The last modified time for the CV `source_id`
        #' - `table`: The last modified time for the CV `table_id`
        #' - `frequency`: The last modified time for the CV `frequency`
        #' - `grid_label`: The last modified time for the CV `grid_label`
        #' - `realm`: The last modified time for the CV `realm`
        #' - `source_type`: The last modified time for the CV `source_type`
        #' - `req_global_atts`: The last modified time for the CV
        #'   `required_global_attributes`
        timestamp = function() {
            private$m_timestamps[names(private$m_timestamps) != "dict"]
        },

        #' @description
        #' Get the time when the dictionary was built
        #'
        #' @return A [DateTime][POSIXct]
        built_time = function() {
            private$m_timestamps[[names(private$m_timestamps) == "dict"]]
        },

        #' @description
        #' Fetch and parse all data of CVs and Data Request
        #'
        #' @return The updated `CMIP6Dict` itself.
        build = function() {
            dict <- cmip6dict_build()

            private$m_version = list(
                cv = dict$cvs$activity$CV_collection_version,
                req = dict$mip_table$version
            )

            private$m_timestamps <- list(
                cv              = dict$cvs$activity$CV_collection_modified,
                activity        = dict$cvs$activity$activity_id_CV_modified,
                experiment      = dict$cvs$experiment$experiment_id_CV_modified,
                sub_experiment  = dict$cvs$sub_experiment$sub_experiment_id_CV_modified,
                institution     = dict$cvs$institution$institution_id_CV_modified,
                source          = dict$cvs$source$source_id_CV_modified,
                table           = dict$cvs$table$table_id_CV_modified,
                frequency       = dict$cvs$frequency$frequency_CV_modified,
                grid_label      = dict$cvs$grid_label$grid_label_CV_modified,
                realm           = dict$cvs$realm$realm_CV_modified,
                source_type     = dict$cvs$source_type$source_type_CV_modified,
                req_global_atts = dict$cvs$req_global_atts$required_global_attributes_CV_modified,
                resolution      = dict$cvs$resolution$nominal_resolution_CV_modified,
                dict              = dict$built_time
            )

            private$m_tables <- list(
                mip_table       = dict$mip_table$value,
                activity        = dict$cvs$activity$value,
                experiment      = dict$cvs$experiment$value,
                sub_experiment  = dict$cvs$sub_experiment$value,
                institution     = dict$cvs$institution$value,
                source          = dict$cvs$source$value,
                table           = dict$cvs$table$value,
                frequency       = dict$cvs$frequency$value,
                grid_label      = dict$cvs$grid_label$value,
                realm           = dict$cvs$realm$value,
                source_type     = dict$cvs$source_type$value,
                req_global_atts = dict$cvs$req_global_atts$value,
                resolution      = dict$cvs$resolution$value
            )

            private$m_cv_sha <- dict$cv_sha
            private$m_req_tag <- dict$req_tag

            self
        },

        #' @description
        #' List the data for a specific CV or Data Request
        #'
        #' @param type A single string indicating the type of data to list.
        #' Should be one of:
        #'
        #' - `"activity"`: The data of CV `activity_id`
        #' - `"experiment"`: The data of CV `experiment_id`
        #' - `"sub_experiment"`: The data of CV `sub_experiment_id`
        #' - `"institution"`: The data of CV `institution_id`
        #' - `"source"`: The data of CV `source_id`
        #' - `"table"`: The data of CV `table_id`
        #' - `"frequency"`: The data of CV `frequency`
        #' - `"grid_label"`: The data of CV `grid_label`
        #' - `"realm"`: The data of CV `realm`
        #' - `"source_type"`: The data of CV `source_type`
        #' - `"req_global_atts"`: The data of CV `required_global_attributes`
        #' - `"variable"`: The data of Data Request
        #'
        #' @return
        #'
        #' For `"activity"`, `"experiment"`, `"sub_experiment"`, `"institution"`,
        #' `"source"`, `"frequency"`, `"grid_label"`, `"realm"` `"source_type"`,
        #' and `"variable", `a [data.table][data.table::data.table] object
        #' giving the value of the corresponding CV, its description and other
        #' attributes.
        #'
        #' For `"table"` and `"req_global_atts"`, a character vector giving the
        #' value of the corresponding CV.
        list = function(type) {
            assert_subset(type, c(CV_TYPES, "variable"))

            if (type == "variable") type <- "mip_table"
            out <- private$m_tables[[type]]
            if (data.table::is.data.table(out)) out <- copy(out)
            out
        },

        #' @description
        #' Update the data for a specific CV or Data Request if applicable
        #'
        #' For CVs, `$update()` uses the GitHub RESTful API to fetch the SHA1
        #' value of each JSON file that stores the CV and compares it with the
        #' last fetched value. If any SHA1 is unmatched, the corresponding JSON
        #' file will be downloaded and parsed.
        #'
        #' For Data Request, `$update()` fetches the latest tag of the Data
        #' Request Subversion repo and compares it with the last fetched value.
        #' If new tag is found, the corresponding `CMIP6_MIP_tables.xlsx`
        #' Microsoft Excel file will be downloaded and parsed.
        #'
        #' @return The `CMIP6Dict` object itself with updated data.
        update = function() {
            dict <- cmip6dict_update(private$m_cv_sha, private$m_req_tag)

            if (!is.null(dict$cvs)) {
                private$m_cv_sha <- dict$cv_sha

                private$m_version$cv <- dict$cvs[[1L]]$CV_collection_version
                private$m_timestamps$cv <- dict$cvs[[1L]]$CV_collection_modified
                private$m_timestamps$dict <- dict$built_time

                for (type in names(private$m_timestamps)) {
                    if (is.null(dict$cvs[[type]])) next
                    nm <- names(CV_TYPES)[CV_TYPES == type]
                    private$m_timestamps[[type]] <- dict$cvs[[type]][[sprintf("%s_CV_modified", nm)]]
                    private$m_tables[[type]] <- dict$cvs[[type]]$value
                }
            }

            if (!is.null(dict$mip_table)) {
                private$m_req_tag <- dict$req_tag
                private$m_version$req <- dict$mip_table$version
            }

            self
        },

        #' @description
        #' Save the CMIP6Dict object
        #'
        #' `$save()` stores all the core data of current `CMIP6Dict` object into
        #' a [RDS][saveRDS()] file named `CMIP6DICT` in the specified folder.
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
                private$m_version,
                private$m_cv_sha,
                private$m_req_tag,
                private$m_timestamps,
                private$m_tables,
                private$m_index,
                private$m_log,
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
            val <- cmip6dict_load(dir)

            private$m_version <- val$version
            private$m_cv_sha <- val$cv_sha
            private$m_req_tag <- val$req_tag
            private$m_timestamps <- val$timestamps
            private$m_tables <- val$tables
            private$m_index <- val$index
            private$m_log <- val$log
        },

        #' @description
        #' Print a summary of the current `CMIP6Dict` object
        #'
        #' `$print()` gives the summary of current `CMIP6Dict` object including
        #' the version of CVs and Data Request, and the last built time.
        #'
        #' @return The `CMIP6Dict` object itself, invisibly.
        print = function() {
            cat_rule("CMIP6 Dictionary", "-")
            if (!length(private$m_version)) {
                cat("- CV  ver: <Empty>\n")
                cat("- Req ver: <Empty>\n")
            } else {
                cat(sprintf("- CV  ver: '%s'\n", private$m_version$cv))
                cat(sprintf("- Req ver: '%s'\n", private$m_version$req))
                cat(sprintf("- Built at: '%s'\n", private$m_timestamps$dict))
            }
        }
    ),

    private = list(
        # CV and Data Request versions
        m_version = NULL,
        # CV JSON file sha
        m_cv_sha = NULL,
        # Data Request tag
        m_req_tag = NULL,
        # modified time for CV and its components
        m_timestamps = NULL,
        # tables for all CV and Data Request
        m_tables = NULL,
        # output index
        m_index = NULL,
        # temporary variables
        m_log = NULL,

        deep_clone = function(name, value) {
            if (data.table::is.data.table(value)) {
                copy(value)
            } else if (is.list(value)) {
                lapply(value, function(x) if (data.table::is.data.table(x)) copy(x) else x)
            } else {
                value
            }
        }
    )
)

CV_TYPES <- c(
    "activity_id"                = "activity",
    "experiment_id"              = "experiment",
    "sub_experiment_id"          = "sub_experiment",
    "institution_id"             = "institution",
    "source_id"                  = "source",
    "table_id"                   = "table",
    "frequency"                  = "frequency",
    "grid_label"                 = "grid_label",
    "realm"                      = "realm",
    "source_type"                = "source_type",
    "required_global_attributes" = "req_global_atts",
    "nominal_resolution"         = "resolution"
)

type_to_std_name <- function(type) names(CV_TYPES)[CV_TYPES %in% type]

cmip6dict_build <- function() {
    req_tag <- cmip6dict_fetch_mip_table_latest_tag()
    cv_sha <- cmip6dict_fetch_cv_sha()

    cvs <- lapply(CV_TYPES, cmip6dict_fetch_cv)
    names(cvs) <- CV_TYPES

    mip_table <- cmip6dict_fetch_mip_table(req_tag)

    built_time <- Sys.time()
    verbose("CMIP6 Dictionary built successfully at ", format(built_time), ".")
    list(cvs = cvs, cv_sha = cv_sha, req_tag = req_tag, mip_table = mip_table, built_time = built_time)
}

cmip6dict_fetch_cv <- function(type, save = FALSE) {
    assert_subset(type, CV_TYPES, empty.ok = FALSE)

    name <- type_to_std_name(type)
    file_name <- sprintf("CMIP6_%s.json", name)
    URL_CMIP6_CV <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/master"
    url_json <- file.path(URL_CMIP6_CV, file_name, fsep = "/")

    verbose("Fetching Controlled Vocabularies(CVs) ", toupper(name), "...")
    q <- tryCatch(jsonlite::read_json(url_json, TRUE), warning = function (w) w, error = function (e) e)

    # nocov start
    if (inherits(q, "warning") || inherits(q, "error")) {
        message(sprintf("Failed to fetch CV json file '%s'. Please check network connection.", url_json))
        return(data.table())
    }

    d <- switch(type,
        activity = cmip6dict_format_cv_flat(q),
        experiment = cmip6dict_format_cv_experiment(q),
        frequency = cmip6dict_format_cv_flat(q),
        grid_label = cmip6dict_format_cv_flat(q),
        institution = cmip6dict_format_cv_flat(q),
        resolution = cmip6dict_format_cv_simple(q),
        realm = cmip6dict_format_cv_flat(q),
        req_global_atts = cmip6dict_format_cv_simple(q),
        source = cmip6dict_format_cv_source(q),
        source_type = cmip6dict_format_cv_flat(q),
        sub_experiment = cmip6dict_format_cv_flat(q),
        table = cmip6dict_format_cv_simple(q)
    )

    # make sure the locale for time is set to "C"
    ori <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", ori), add = TRUE)
    Sys.setlocale("LC_TIME", "C")

    d$CV_collection_version <- as.numeric_version(d$CV_collection_version)
    d$CV_collection_modified <- as.POSIXct(d$CV_collection_modified, format =  "%c %z")

    # fix malformed timezone spec
    type_modified <- sprintf("%s_CV_modified", name)
    d[[type_modified]] <- gsub(" 0(\\d{4})$", " -\\1", d[[type_modified]])

    # fix malformed abbr weekday spec
    d[[type_modified]] <- gsub("Tues", "Tue", d[[type_modified]])
    d[[type_modified]] <- as.POSIXct(d[[type_modified]], format = "%c %z")

    d
}

cmip6dict_format_cv_simple <- function(json) {
    c(json$version_metadata, list(value = json[[1L]]))
}

cmip6dict_format_cv_flat <- function(json) {
    d <- data.table(names(json[[1L]]), unlist(json[[1L]]))
    setnames(d, c(names(json)[1L], "description"))

    c(json$version_metadata, list(value = as.data.table(d)))
}

transpose_nested_list <- function(x, name = "__X__") {
    transposed <- lapply(names(x[[1L]]), function(nm) lapply(x, "[[", nm))
    val <- c(list(names(x)), transposed)
    names(val) <- c(name, names(x[[1L]]))
    val
}

cmip6dict_format_cv_nest <- function(json) {
    val <- transpose_nested_list(json[[1L]], names(json)[1L])
    c(json$version_metadata, list(value = as.data.table(val)))
}

cmip6dict_format_cv_experiment <- function(json) {
    d <- cmip6dict_format_cv_nest(json)
    setnames(d$value, 1L, "name")
    set(d$value, NULL, "experiment_id", NULL)
    setnames(d$value, 1L, "experiment_id")

    cols_flat <- c("description", "end_year", "experiment",
        "min_number_yrs_per_sim", "start_year", "tier")
    for (col in cols_flat) {
        set(d$value, NULL, col, unlist(d$value[[col]], FALSE, FALSE))
    }

    cols_int <- c("end_year", "min_number_yrs_per_sim", "start_year", "tier")
    for (col in cols_int) {
        set(d$value, NULL, col, suppressWarnings(as.integer(d$value[[col]])))
    }

    d
}

cmip6dict_format_cv_source <- function(json) {
    d <- cmip6dict_format_cv_nest(json)

    setnames(d$value, 1L, "name")
    set(d$value, NULL, "source_id", NULL)
    setnames(d$value, 1L, "source_id")

    cols_flat <- c("cohort", "label", "label_extended", "release_year")
    for (col in cols_flat) {
        set(d$value, NULL, col, unlist(d$value[[col]], FALSE, FALSE))
    }

    cols_int <- c("release_year")
    for (col in cols_int) {
        set(d$value, NULL, col, as.integer(d$value[[col]]))
    }

    null_to_none <- function(x) if (is.null(x)) "none" else x
    get_value <- function(l, name) vapply(l, function(x) null_to_none(x[[name]]), character(1))
    set(d$value, NULL, "model_component",
        lapply(d$value$model_component, function(comp) {
            data.table(
                name = names(comp),
                description = get_value(comp, "description"),
                native_nominal_resolution = get_value(comp, "native_nominal_resolution")
            )
        })
    )

    d
}

cmip6dict_fetch_mip_table_latest_tag <- function() {
    l <- readLines("http://proj.badc.rl.ac.uk/svn/exarch/CMIP6dreq/tags/", warn = FALSE)
    re <- '(?<=href=")\\d{2}\\.\\d{2}\\.\\d{2}(?=/")'
    m <- unlist(regmatches(l, regexec(re, l, perl = TRUE)))

    vers <- do.call(c, lapply(m, as.numeric_version))
    m[max(vers) == vers]
}

#' @importFrom readxl read_excel
cmip6dict_fetch_mip_table <- function(tag, dir = tempdir()) {
    checkmate::assert_directory_exists(dir, "w")

    verbose("Fetching CMIP6 MIP Table...")
    url <- sprintf("http://proj.badc.rl.ac.uk/svn/exarch/CMIP6dreq/tags/%s/dreqPy/docs/CMIP6_MIP_tables.xlsx", tag)
    f <- file.path(dir, basename(url))
    download.file(url, f, mode = "wb", quiet = TRUE)

    # parse version
    notes <- readxl::read_excel(f, "Notes", skip = 1)
    ver <- as.numeric_version(names(notes)[2L])

    # parse table
    sheets <- readxl::excel_sheets(f)[-1L]
    l <- lapply(sheets, function(nm) {
        d <- as.data.table(readxl::read_excel(f, nm))
        set(d, NULL, "MIPs (by experiment)", strsplit(d[["MIPs (by experiment)"]], "\\s*,\\s*"))
        set(d, NULL, "MIPs (requesting)", strsplit(d[["MIPs (requesting)"]], "\\s*,\\s*"))
        setnames(d, gsub(")", "", fixed = TRUE, gsub(" (", "_", tolower(names(d)), fixed = TRUE)))
        setnames(d, gsub(" ", "_", fixed = TRUE, names(d)))

        set(d, NULL, "table_id", nm)
        set(d, NULL, "default_priority", as.integer(d$default_priority))
        setcolorder(d, "table_id")
    })
    d <- rbindlist(l, use.names = TRUE)

    list(tag = tag, version = ver, value = d)
}

cmip6dict_fetch_cv_sha <- function() {
    verbose("Fetching SHA1 of Controlled Vocabularies(CVs)...")

    url_json <- "https://api.github.com/repos/WCRP-CMIP/CMIP6_CVs/git/trees/master"
    q <- tryCatch(jsonlite::read_json(url_json), warning = function (w) w, error = function (e) e)

    # nocov start
    if (inherits(q, "warning") || inherits(q, "error")) {
        message(sprintf("Failed to fetch CV json file '%s'. Please check network connection.", url_json))
        return(data.table())
    }

    res <- transpose_nested_list(q$tree)[-1L]

    d <- data.table(
        file = basename(unlist(res$path, FALSE, FALSE)),
        sha = unlist(res$sha, FALSE, FALSE)
    )

    types <- data.table(type = CV_TYPES, file = sprintf("CMIP6_%s.json", names(CV_TYPES)))

    setcolorder(set(d[types, on = "file"], NULL, "file", NULL), "type")
}

cmip6dict_save <- function(version, cv_sha, req_tag, timestamps, tables, index, log, dir = getOption("epwshiftr.dir")) {
    val <- list(version = version, cv_sha = cv_sha, req_tag = req_tag,
        timestamps = timestamps, tables = tables, index = index, log = log
    )
    f <- normalizePath(file.path(dir, "CMIP6Dict"), mustWork = FALSE)
    saveRDS(val, f)
    f
}

cmip6dict_load <- function(path) {
    checkmate::assert_file_exists(path, "r")
    val <- readRDS(path)

    for (nm in names(val$tables)) {
        if (data.table::is.data.table(val$tables[[nm]])) {
            data.table::setDT(val$tables[[nm]])
        }
    }

    data.table::setDT(val$cv_sha)

    if (!is.null(val$index)) {
        data.table::setDT(val$index)
    }

    val
}

cmip6dict_update <- function(cv_sha, req_tag) {
    new_sha <- no_verbose(cmip6dict_fetch_cv_sha())
    new_tag <- cmip6dict_fetch_mip_table_latest_tag()

    out <- list()

    if (is.null(cv_sha) || length(new_cv <- new_sha[!cv_sha, on = c("type", "sha"), type])) {
        if (is.null(cv_sha)) {
            new_cv <- CV_TYPES

            verbose("Empty local CMIP6 Controlled Vocabularies (CVs) found. Fetching all...")
        } else {
            new_cv <- CV_TYPES[CV_TYPES %in% new_cv]

            verbose(sprintf("New CMIP6 Controlled Vocabularies (CVs) [%s] found. Updating...",
                paste0("'", toupper(names(new_cv)), "'", collapse = ", ")
            ))
        }

        cvs <- lapply(new_cv, cmip6dict_fetch_cv)
        names(cvs) <- new_cv

        out$cvs <- cvs
        out$cv_sha <- new_sha
    }

    if (is.null(req_tag) || new_tag != req_tag) {
        if (is.null(req_tag)) {
            verbose("Empty local CMIP6 MIP Table found. Fetching...")
        } else {
            verbose(sprintf("Online CMIP6 MIP Table tag '%s' is newer than local tag '%s'. Updating...", new_tag, req_tag))
        }
        out$mip_table <- cmip6dict_fetch_mip_table(new_tag)
        out$req_tag <- new_tag
    }

    if (!length(out)) {
        verbose("CMIP6 Controlled Vocabularies (CVs) and MIP Table are all at the latest version. Nothing to udpate.")
        return(NULL)
    }

    out$built_time <- Sys.time()
    verbose("CMIP6 Dictionary updated successfully at ", format(out$built_time), ".")

    out
}

cmip6dict_validate_query_input <- function(
    DICT = NULL,
    activity = NULL, experiment = NULL, sub_experiment = NULL,
    institution = NULL, source = NULL, table = NULL, frequency = NULL,
    grid_label = NULL, realm = NULL, source_type = NULL, resolution = NULL,
    variable = NULL, variant = NULL)
{
    if (is.null(DICT) || !length(DICT)) {
        stop("Current CMIP6Dict is empty. Please first run '$build()' to ",
            "collection all CMIP6 Controlled Vocabularies (CVs) and MIP Table.",
            call. = FALSE)
    }

    validate_input <- function(input) {
        type <- deparse(substitute(input))
        if (type == "variable") {
            name <- "mipt_table"
            colnm <- "variable_name"
        } else {
            name <- type
            colnm <- type_to_std_name(type)
        }

        if (!checkmate::test_subset(input, empty.ok = TRUE, choices = DICT[[name]][[colnm]])) {
            stop(sprintf(
                "Invalid %s found: [%s]. Please run '$list_all(\"%s\")' to see all possible options.",
                type, paste0("'", input, "'", collapse = ","), type
            ))
        }
    }

    validate_input(activity)
    validate_input(experiment)
    validate_input(sub_experiment)
    validate_input(institution)
    validate_input(source)
    validate_input(table)
    validate_input(frequency)
    validate_input(grid_label)
    validate_input(realm)
    validate_input(source_type)
    validate_input(resolution)
    validate_input(variable)
    assert_character(variant, any.missing = FALSE, pattern = "r\\d+i\\d+p\\d+f\\d+", null.ok = TRUE)
}

cmip6dict_esgf_query <- function(
    DICT = NULL,
    activity = NULL, experiment = NULL, sub_experiment = NULL,
    institution = NULL, source = NULL, table = NULL, frequency = NULL,
    grid_label = NULL, realm = NULL, source_type = NULL, resolution = NULL,
    variable = NULL, variant = NULL,
    count = FALSE, replica = FALSE, latest = TRUE, type = "Dataset", limit = 1000L, data_node = NULL)
{
    cmip6dict_validate_query_input(DICT,
        activity = activity, experiment = experiment,
        sub_experiment = sub_experiment, institution = institution,
        source = source, table = table, frequency = frequency,
        grid_label = grid_label, realm = realm, source_type = source_type,
        resolution = resolution, variable = variable, variant = variant
    )
    assert_choice(type, c("Dataset", "File"))
    assert_flag(count)
    assert_flag(replica)
    assert_flag(latest)
    assert_count(limit, positive = TRUE)
    assert_character(data_node, any.missing = FALSE, null.ok = TRUE)

    pair <- function(x, first = FALSE) {
        # get name
        var <- deparse(substitute(x))

        # skip if empty
        if (is.null(x) || !length(x)) return()

        # get key name
        if (var == "variable") {
            key <- "variable_id"
        } else if (var == "variant"){
            key <- "variant_label"
        } else {
            key <- type_to_std_name(var)
        }

        # for logical key
        if (!length(key)) key <- var
        if (is.logical(x)) x <- tolower(x)

        s <- paste0(key, "=", paste0(x, collapse = "%2C")) # %2C = ","
        if (first) s else paste0("&", s)
    }

    `%and%` <- function (lhs, rhs) if (is.null(rhs)) lhs else paste0(lhs, rhs)

    project <- "CMIP6"
    format <- "application%2Fsolr%2Bjson"

    resolution <- c(gsub(" ", "", resolution, fixed = TRUE),
                    gsub(" ", "+", resolution, fixed = TRUE))

    # if only to return the summary counts
    if (count) {
        limit <- 0L
        facets <- c(
            "mip_era",
            "activity_id",
            "institution_id",
            "experiment_id",
            "sub_experiment_id",
            "source_id",
            "member_id",
            "table_id",
            "frequency",
            "grid_label",
            "version",
            "nominal_resolution",
            "variable_id",
            "variable_units",
            "cf_standard_name",
            "variant_label",
            "datetime_start",
            "datetime_end",
            "data_node",
            "realm",
            "number_of_files"
        )
    }

    url_base <- "http://esgf-node.llnl.gov/esg-search/search/?"

    q <- url_base %and%
        pair(project, TRUE) %and%
        pair(activity) %and%
        pair(experiment) %and%
        pair(source) %and%
        pair(variable) %and%
        pair(resolution) %and%
        pair(variant) %and%
        pair(data_node) %and%
        pair(frequency) %and%
        pair(replica) %and%
        pair(latest) %and%
        pair(type) %and%
        pair(limit) %and%
        pair(facets) %and%
        pair(format)

    res <- tryCatch(jsonlite::read_json(q), warning = function (w) w, error = function (e) e)

    # nocov start
    if (inherits(res, "warning") || inherits(res, "error")) {
        message("No matched data. Please check network connection and the availability of LLNL ESGF node.")
        dt <- data.table::data.table()
    # nocov end
    } else if (res$response$numFound == 0L) {
        message("No matched data. Please examine the actual response using 'attr(x, \"response\")'.")
        dt <- data.table::data.table()
    } else if (count) {
        out <- cmip6dict_extract_query_facets(res)
    } else if (type == "Dataset") {
        out <- cmip6dict_extract_query_dataset(res)
    } else if (type == "File") {
        out <- cmip6dict_extract_query_file(res)
    }

    data.table::setattr(out, "response", res)

    out
}

cmip6dict_extract_query_facets <- function(response) {
    if (response$responseHeader$status != 0L) {
        stop("Input query response has a status of not being zero.")
    }

    # extract query fields
    fq <- response$responseHeader$params$fq

    fq <- lapply(response$responseHeader$params$fq, function(q) {
        fld <- unlist(strsplit(strsplit(q, ":", fixed = TRUE)[[1L]], "\\s*\\|\\|\\s*"), FALSE, FALSE)
        c(fld[1L], fld[fld != fld[1L]])
    })
    names(fq) <- vapply(fq, `[[`, character(1), 1L)
    query <- lapply(fq, `[`, -1L)

    # extract facet results
    facets <- response$facet_counts$facet_fields
    out <- lapply(facets, function(f) {
        if (length(f) %% 2 != 0L) {
            stop(sprintf("Invalid length of facet fields query results: '%s'.", length(f)))
        }
        ind <- seq_along(f)
        val <- unlist(f[ind %% 2 == 0L], FALSE, FALSE)
        names(val) <- unlist(f[ind %% 2 == 1L], FALSE, FALSE)
        val
    })

    structure(out, query = query, class = c("CMIP6QueryFacets", "CMIP6Query"))
}

#' Print Results of CMIP6 Query Facets
#'
#' @param x An `CMIP6QueryFacets` object.
#' @param max How many elements to list in each facet field. Default: `10L`.
#' @param ... Further arguments passed to or from other methods.
#' @export
print.CMIP6QueryFacets <- function(x, max = 10L, ...) {
    checkmate::assert_int(max, lower = 0L)

    facet <- names(x)
    query <- attr(x, "query", TRUE)

    cat_rule("CMIP6 Facet Query Summary", "=")

    cat("<QUERY>:\n")
    for (i in seq_along(query)) {
        if (names(query)[i] %in% c("replica", "latest")) {
            cat(sprintf("  * %s = %s", names(query)[i], query[[i]]), sep = "\n")
        } else {
            cat(sprintf("  * %s = [%s]", names(query)[i], paste0(query[[i]], collapse = ", ")), sep = "\n")
        }
    }

    if (query$type == "File") facet <- facet[!facet %in% c("number_of_files", "datetime_start", "datetime_end")]
    for (f in facet) {
        cat("\n")
        cat_rule(sprintf("%s [%i]", f, length(x[[f]])), right = sprintf("[Tot: %i]", sum(x[[f]])))
        if (length(x[[f]]) > max) {
            cat(
                paste0(sprintf("  * %s [%s]", names(x[[f]])[1:max], x[[f]][1:max]), collapse = "\n"),
                sprintf("  [And %i more]...", length(x[[f]]) - max),
                sep = "\n"
            )
        } else {
            cat(paste0(sprintf("  * %s [%s]", names(x[[f]]), x[[f]]), collapse = "\n"), sep = "\n")
        }
    }
}
