cmip6dict__format_cv_nest <- function(json) {
    transposed <- lapply(names(json[[1L]]), function(nm) lapply(json, "[[", nm))
    data.table::setnames(data.table::as.data.table(transposed), names(json[[1L]]))
}

cmip6dict__parse_cv_version_metadata <- function(lst) {
    res <- list()

    ori <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", ori), add = TRUE)
    Sys.setlocale("LC_TIME", "C")

    res$CV_collection_version <- as.numeric_version(lst$CV_collection_version)
    res$CV_collection_modified <- as.POSIXct(
        lst$CV_collection_modified,
        format = "%c %z",
        tz = "UTC"
    )

    type_modified <- names(lst)[endsWith(names(lst), "CV_modified")]
    res$CV_modified <- gsub(" 0(\\d{4})$", " -\\1", lst[[type_modified]])
    res$CV_modified <- gsub("Tues", "Tue", res$CV_modified, fixed = TRUE)
    res$CV_modified <- as.POSIXct(res$CV_modified, format = "%c %z", tz = "UTC")

    res$CV_note <- lst[[names(lst)[endsWith(names(lst), "CV_note")]]]

    res
}

cmip6dict__parse_cv_vec <- function(file, subclass = NULL) {
    json <- jsonlite::read_json(file)
    res <- unlst(json[[1L]])
    data.table::setattr(res, "version",
        cmip6dict__parse_cv_version_metadata(json$version_metadata)
    )

    structure(res, class = c(subclass, "Cmip6CV", typeof(res)))
}

cmip6dict__parse_cv_list <- function(file, subclass = NULL) {
    json <- jsonlite::read_json(file)
    res <- json[[1L]]
    data.table::setattr(res, "version",
        cmip6dict__parse_cv_version_metadata(json$version_metadata)
    )

    structure(res, class = c(subclass, "Cmip6CV", "list"))
}

cmip6dict__parse_cv_drs <- function(file) {
    cmip6dict__parse_cv_list(file, "Cmip6CV_DRS")
}

cmip6dict__parse_cv_activity_id <- function(file) {
    cmip6dict__parse_cv_list(file, "Cmip6CV_ActivityId")
}

cmip6dict__parse_cv_experiment_id <- function(file) {
    json <- jsonlite::read_json(file)
    d <- cmip6dict__format_cv_nest(json[[1L]])

    data.table::setcolorder(d, "experiment_id")

    cols_lst <- c("activity_id", "additional_allowed_model_components",
        "parent_activity_id", "parent_experiment_id",
        "required_model_components", "sub_experiment_id")
    for (col in cols_lst) {
        data.table::set(d, NULL, col, lapply(d[[col]], unlist, FALSE, FALSE))
    }

    cols_flat <- c("experiment_id", "description", "end_year", "experiment",
        "min_number_yrs_per_sim", "start_year", "tier")
    for (col in cols_flat) {
        data.table::set(d, NULL, col, unlist(d[[col]], FALSE, FALSE))
    }

    cols_int <- c("end_year", "min_number_yrs_per_sim", "start_year", "tier")
    for (col in cols_int) {
        data.table::set(d, NULL, col, suppressWarnings(as.integer(d[[col]])))
    }

    data.table::setcolorder(d, c(
        "experiment_id", "experiment", "description", "tier",
        "start_year", "end_year", "min_number_yrs_per_sim",
        "required_model_components",
        "parent_experiment_id", "sub_experiment_id",
        "activity_id", "parent_activity_id",
        "additional_allowed_model_components"
    ))
    data.table::setattr(d, "version", cmip6dict__parse_cv_version_metadata(json$version_metadata))

    structure(d, class = c("Cmip6CV_ExperimentId", "Cmip6CV", class(d)))
}

cmip6dict__parse_cv_frequency <- function(file) {
    cmip6dict__parse_cv_list(file, "Cmip6CV_Frequency")
}

cmip6dict__parse_cv_grid_label <- function(file) {
    cmip6dict__parse_cv_list(file, "Cmip6CV_GridLabel")
}

cmip6dict__parse_cv_institution_id <- function(file) {
    cmip6dict__parse_cv_list(file, "Cmip6CV_InstitutionId")
}

cmip6dict__parse_cv_nominal_resolution <- function(file) {
    cmip6dict__parse_cv_vec(file, "Cmip6CV_Resolution")
}

cmip6dict__parse_cv_realm <- function(file) {
    cmip6dict__parse_cv_list(file, "Cmip6CV_Realm")
}

cmip6dict__parse_cv_required_global_attributes <- function(file) {
    cmip6dict__parse_cv_vec(file, "Cmip6CV_ReqGlobAttr")
}

cmip6dict__parse_cv_source_id <- function(file) {
    json <- jsonlite::read_json(file)
    d <- cmip6dict__format_cv_nest(json[[1L]])

    data.table::setcolorder(d, "source_id")

    cols_lst <- c("activity_participation", "institution_id")
    for (col in cols_lst) {
        data.table::set(d, NULL, col, lapply(d[[col]], unlst))
    }

    cols_flat <- c("source_id", "release_year", "cohort", "label", "label_extended")
    for (col in cols_flat) {
        data.table::set(d, NULL, col, unlist(d[[col]], FALSE, FALSE))
    }

    data.table::set(d, NULL, "release_year", suppressWarnings(as.integer(d$release_year)))

    data.table::setcolorder(d, c(
        "source_id", "release_year", "institution_id", "label", "label_extended",
        "cohort", "activity_participation", "model_component", "license_info"
    ))
    data.table::setattr(d, "version", cmip6dict__parse_cv_version_metadata(json$version_metadata))

    structure(d, class = c("Cmip6CV_SourceId", "Cmip6CV", class(d)))
}

cmip6dict__parse_cv_source_type <- function(file) {
    cmip6dict__parse_cv_list(file, "Cmip6CV_SourceType")
}

cmip6dict__parse_cv_sub_experiment_id <- function(file) {
    cmip6dict__parse_cv_list(file, "Cmip6CV_SubExperimentId")
}

cmip6dict__parse_cv_table_id <- function(file) {
    cmip6dict__parse_cv_vec(file, "Cmip6CV_TableId")
}
