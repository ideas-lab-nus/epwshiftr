cmip6dict__build <- function(dict) {
    res <- list()
    res$built_time <- dict$built_time
    res$sources <- dict$sources
    dict$built_time <- NULL
    dict$sources <- NULL

    if (is.null(dict$cvs) || is.null(dict$dreq)) {
        res$version <- NULL
        res$timestamps <- NULL
        res$data <- list(cvs = dict$cvs, dreq = dict$dreq)
        res$indices <- list()
        return(res)
    }

    res$version <- list(
        cvs = attr(dict$cvs$drs, "version", TRUE)$CV_collection_version,
        dreq = attr(dict$dreq, "metadata", TRUE)$dreq_version[[1L]]
    )

    res$timestamps <- c(
        list(cvs = attr(dict$cvs$drs, "version", TRUE)$CV_collection_modified),
        lapply(dict$cvs, function(cv) attr(cv, "version", TRUE)$CV_modified)
    )

    res$data <- list(cvs = dict$cvs, dreq = dict$dreq)
    res$indices <- if (is.null(dict$indices)) {
        cmip6dict__indices(res$data)
    } else {
        dict$indices
    }

    res
}

esgdict__save <- function(
    project,
    profile,
    built_time,
    data,
    version = NULL,
    timestamps = NULL,
    sources = NULL,
    indices = NULL,
    dir = getOption("epwshiftr.dir", "."),
    file = "CMIP6DICT.json"
) {
    project <- esgdict__normalize_project(project)
    profile <- esgdict__profile(project)
    checkmate::assert_string(file, min.chars = 1L)

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    path <- normalizePath(file.path(dir, file), mustWork = FALSE)
    payload <- esgdict__payload(project, profile, built_time, data, version, timestamps, sources, indices)
    schema_validate(SCHEMA_ESG_DICT, payload, mode = "assert", name = path)
    esgdict__validate_payload(payload, name = path)
    jsonlite::write_json(payload, path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null",
        na = "null"
    )
    path
}

esgdict__load <- function(dir = getOption("epwshiftr.dir", "."), file = "CMIP6DICT.json", project = "CMIP6") {
    project <- esgdict__normalize_project(project)
    checkmate::assert_string(file, min.chars = 1L)

    path <- normalizePath(file.path(dir, file), mustWork = FALSE)
    if (!file.exists(path)) return(NULL)

    payload <- jsonlite::read_json(path, simplifyVector = FALSE)
    schema_validate(SCHEMA_ESG_DICT, payload, mode = "assert", name = path)
    esgdict__validate_payload(payload, name = path)

    loaded_project <- esgdict__normalize_project(payload$project)
    if (!identical(loaded_project, project)) {
        stop(
            sprintf(
                "%s contains project `%s`, not requested project `%s`.",
                path,
                loaded_project,
                project
            ),
            call. = FALSE
        )
    }

    esgdict__from_payload(payload)
}

esgdict__validate_payload <- function(payload, name = "ESGDICT.json") {
    project <- esgdict__normalize_project(payload$project)
    profile <- esgdict__profile(project)
    if (!identical(as.character(payload$profile), profile)) {
        stop(
            sprintf("%s profile `%s` does not match project `%s`.", name, payload$profile, project),
            call. = FALSE
        )
    }

    if (!identical(profile, "cmip6")) {
        if (!is.null(payload$payload)) {
            stop(sprintf("%s has payload for unsupported profile `%s`.", name, profile), call. = FALSE)
        }
        return(invisible(TRUE))
    }

    cmip6dict__validate_payload(payload$payload, name = name)
}

cmip6dict__validate_payload <- function(payload, name = "CMIP6DICT.json") {
    if (is.null(payload) || (is.null(payload$cvs) && is.null(payload$dreq))) {
        return(invisible(TRUE))
    }

    if (is.null(payload$cvs) || is.null(payload$dreq)) {
        stop(sprintf("%s is a partial CMIP6 dictionary payload.", name), call. = FALSE)
    }

    required_cvs <- tolower(CV_TYPES)
    missing_cvs <- setdiff(required_cvs, names(payload$cvs))
    if (length(missing_cvs)) {
        stop(
            sprintf("%s is missing CMIP6 CV payload(s): %s.", name, paste(missing_cvs, collapse = ", ")),
            call. = FALSE
        )
    }

    for (cv_name in names(payload$cvs)) {
        cv <- payload$cvs[[cv_name]]
        kind <- as.character(cv$kind)
        if (!kind %in% c("table", "list", "vector")) {
            stop(sprintf("%s$cvs$%s has invalid kind `%s`.", name, cv_name, kind), call. = FALSE)
        }
        if (identical(kind, "table") && (is.null(cv$columns) || is.null(cv$rows))) {
            stop(sprintf("%s$cvs$%s table payload must contain columns and rows.", name, cv_name), call. = FALSE)
        }
        if (!identical(kind, "table") && is.null(cv$values)) {
            stop(sprintf("%s$cvs$%s %s payload must contain values.", name, cv_name, kind), call. = FALSE)
        }
    }

    dreq_cols <- cmip6dict__payload_column_names(payload$dreq)
    missing_dreq <- setdiff(c("variable", "table_id", "frequency", "modeling_realm"), dreq_cols)
    if (length(missing_dreq)) {
        stop(
            sprintf("%s$dreq is missing required column(s): %s.", name, paste(missing_dreq, collapse = ", ")),
            call. = FALSE
        )
    }

    meta_cols <- cmip6dict__payload_column_names(payload$dreq_metadata)
    missing_meta <- setdiff(c("dreq_version", "cmor_version", "table_id", "mip_era"), meta_cols)
    if (length(missing_meta)) {
        stop(
            sprintf("%s$dreq_metadata is missing required column(s): %s.", name, paste(missing_meta, collapse = ", ")),
            call. = FALSE
        )
    }

    if (!is.null(payload$indices)) {
        missing_indices <- setdiff(c("values", "dreq", "activity_experiment", "activity_source"), names(payload$indices))
        if (length(missing_indices)) {
            stop(
                sprintf("%s$indices is missing required index table(s): %s.", name, paste(missing_indices, collapse = ", ")),
                call. = FALSE
            )
        }
    }

    invisible(TRUE)
}

cmip6dict__payload_column_names <- function(payload) {
    if (is.null(payload) || is.null(payload$columns)) {
        return(character())
    }
    vapply(payload$columns, function(col) as.character(col$name), character(1L), USE.NAMES = FALSE)
}

esgdict__payload <- function(
    project,
    profile,
    built_time,
    data,
    version = NULL,
    timestamps = NULL,
    sources = NULL,
    indices = NULL
) {
    project <- esgdict__normalize_project(project)
    profile <- esgdict__profile(project)
    list(
        format = ESGDICT_FORMAT,
        format_version = ESGDICT_FORMAT_VERSION,
        project = project,
        profile = profile,
        built_time = cmip6dict__encode_time(built_time),
        version = cmip6dict__encode_version(version),
        timestamps = cmip6dict__encode_value(timestamps),
        sources = cmip6dict__encode_value(sources),
        payload = if (identical(profile, "cmip6")) {
            cmip6dict__payload(data, indices)
        } else {
            NULL
        }
    )
}

cmip6dict__payload <- function(data, indices = NULL) {
    list(
        cvs = cmip6dict__encode_cvs(data$cvs),
        dreq = cmip6dict__encode_table(data$dreq),
        dreq_metadata = cmip6dict__encode_table(attr(data$dreq, "metadata", TRUE), include_class = FALSE),
        indices = cmip6dict__encode_indices(indices)
    )
}

esgdict__from_payload <- function(payload) {
    project <- esgdict__normalize_project(payload$project)
    profile <- esgdict__profile(project)
    body <- payload$payload
    list(
        project = project,
        profile = profile,
        built_time = cmip6dict__decode_time(payload$built_time),
        sources = payload$sources,
        cvs = if (identical(profile, "cmip6")) cmip6dict__decode_cvs(body$cvs) else NULL,
        dreq = if (identical(profile, "cmip6")) cmip6dict__decode_dreq(body$dreq, body$dreq_metadata) else NULL,
        indices = if (identical(profile, "cmip6")) cmip6dict__decode_indices(body$indices) else NULL
    )
}

cmip6dict__encode_version <- function(version) {
    if (is.null(version)) return(NULL)

    list(
        cvs = as.character(version$cvs),
        dreq = as.character(version$dreq)
    )
}

cmip6dict__encode_time <- function(x) {
    if (is.null(x)) return(NULL)
    format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

cmip6dict__decode_time <- function(x) {
    if (is.null(x) || !length(x) || is.na(x)) return(NULL)
    as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
}

cmip6dict__encode_value <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "numeric_version")) return(as.character(x))
    if (inherits(x, "POSIXt")) return(vapply(x, cmip6dict__encode_time, character(1L), USE.NAMES = FALSE))
    if (inherits(x, "Date")) return(as.character(x))
    if (is.list(x) && !is.data.frame(x)) return(lapply(x, cmip6dict__encode_value))
    if (is.atomic(x)) return(as.vector(x))
    x
}

cmip6dict__encode_cvs <- function(cvs) {
    if (is.null(cvs)) return(NULL)

    out <- lapply(cvs, cmip6dict__encode_cv)
    out[intersect(tolower(CV_TYPES), names(out))]
}

cmip6dict__encode_cv <- function(cv) {
    payload <- list(
        kind = cmip6dict__cv_kind(cv),
        class = class(cv),
        version = cmip6dict__encode_value(attr(cv, "version", TRUE))
    )

    if (is.data.frame(cv)) {
        payload$columns <- cmip6dict__column_specs(cv)
        payload$rows <- cmip6dict__encode_rows(cv)
    } else {
        payload$values <- cmip6dict__encode_value(unclass(cv))
    }

    payload
}

cmip6dict__cv_kind <- function(cv) {
    if (is.data.frame(cv)) return("table")
    if (is.list(cv)) return("list")
    "vector"
}

cmip6dict__encode_indices <- function(indices) {
    if (is.null(indices) || !length(indices)) return(NULL)
    lapply(indices, cmip6dict__encode_table)
}

cmip6dict__decode_indices <- function(indices) {
    if (is.null(indices)) return(NULL)
    out <- lapply(indices, cmip6dict__decode_table)
    names(out) <- names(indices)
    out
}

cmip6dict__encode_table <- function(x, include_class = TRUE) {
    if (is.null(x)) return(NULL)

    out <- list(
        columns = cmip6dict__column_specs(x),
        rows = cmip6dict__encode_rows(x)
    )
    if (include_class) {
        out <- c(list(class = class(x)), out)
    }
    out
}

cmip6dict__column_specs <- function(x) {
    lapply(names(x), function(col) {
        list(name = col, type = cmip6dict__column_type(x[[col]]))
    })
}

cmip6dict__column_type <- function(x) {
    if (is.list(x) && !inherits(x, c("Date", "POSIXt", "numeric_version"))) return("list")
    if (inherits(x, "numeric_version")) return("numeric_version")
    if (inherits(x, "POSIXt")) return("posixct")
    if (inherits(x, "Date")) return("date")
    if (is.integer(x)) return("integer")
    if (is.double(x)) return("numeric")
    if (is.logical(x)) return("logical")
    "character"
}

cmip6dict__encode_rows <- function(x) {
    if (is.null(x) || !nrow(x)) return(list())

    lapply(seq_len(nrow(x)), function(i) {
        row <- lapply(names(x), function(col) {
            cmip6dict__encode_value(cmip6dict__table_cell(x[[col]], i))
        })
        names(row) <- names(x)
        row
    })
}

cmip6dict__table_cell <- function(col, i) {
    if (is.list(col) && !inherits(col, c("Date", "POSIXt", "numeric_version"))) {
        return(col[[i]])
    }
    col[i]
}

cmip6dict__decode_cvs <- function(cvs) {
    if (is.null(cvs)) return(NULL)

    out <- lapply(names(cvs), function(nm) cmip6dict__decode_cv(nm, cvs[[nm]]))
    names(out) <- names(cvs)
    out
}

cmip6dict__decode_cv <- function(name, payload) {
    cls <- cmip6dict__decode_class(payload$class)
    version <- cmip6dict__decode_cv_version(payload$version)

    if (identical(payload$kind, "table")) {
        x <- cmip6dict__decode_table(payload, class = cls)
    } else if (identical(payload$kind, "list")) {
        x <- payload$values
        class(x) <- cls
    } else {
        x <- as.character(unlst(payload$values))
        class(x) <- cls
    }

    data.table::setattr(x, "version", version)
    x
}

cmip6dict__decode_class <- function(x) {
    if (is.null(x)) return(NULL)
    as.character(unlst(x))
}

cmip6dict__decode_cv_version <- function(x) {
    if (is.null(x)) return(NULL)

    list(
        CV_collection_version = as.numeric_version(x$CV_collection_version),
        CV_collection_modified = cmip6dict__decode_time(x$CV_collection_modified),
        CV_modified = cmip6dict__decode_time(x$CV_modified),
        CV_note = as.character(x$CV_note)
    )
}

cmip6dict__decode_dreq <- function(payload, metadata) {
    if (is.null(payload)) return(NULL)

    dreq <- cmip6dict__decode_table(payload, class = cmip6dict__decode_class(payload$class))
    meta <- cmip6dict__decode_table(metadata)
    data.table::setattr(dreq, "metadata", meta)
    dreq
}

cmip6dict__decode_table <- function(payload, class = NULL) {
    if (is.null(payload)) return(NULL)

    columns <- cmip6dict__decode_columns(payload$columns)
    rows <- payload$rows
    values <- lapply(columns$name, function(col) {
        lapply(rows, function(row) {
            if (col %in% names(row)) row[[col]] else NULL
        })
    })
    names(values) <- columns$name

    for (i in seq_len(nrow(columns))) {
        col <- columns$name[[i]]
        values[[col]] <- cmip6dict__decode_column(values[[col]], columns$type[[i]])
    }

    out <- data.table::as.data.table(values)

    if (!is.null(class)) {
        class(out) <- unique(c(class, class(out)))
    }
    out
}

cmip6dict__decode_columns <- function(columns) {
    if (!length(columns)) {
        return(data.table::data.table(name = character(), type = character()))
    }

    data.table::rbindlist(lapply(columns, function(col) {
        data.table::data.table(
            name = as.character(col$name),
            type = as.character(col$type)
        )
    }))
}

cmip6dict__decode_column <- function(values, type) {
    switch(type,
        list = lapply(values, cmip6dict__decode_list_cell),
        integer = suppressWarnings(as.integer(vapply(values, cmip6dict__decode_scalar_cell, character(1L), USE.NAMES = FALSE))),
        numeric = suppressWarnings(as.numeric(vapply(values, cmip6dict__decode_scalar_cell, character(1L), USE.NAMES = FALSE))),
        logical = as.logical(vapply(values, cmip6dict__decode_scalar_cell, character(1L), USE.NAMES = FALSE)),
        date = as.Date(vapply(values, cmip6dict__decode_scalar_cell, character(1L), USE.NAMES = FALSE)),
        posixct = as.POSIXct(vapply(values, cmip6dict__decode_scalar_cell, character(1L), USE.NAMES = FALSE), tz = "UTC"),
        numeric_version = as.numeric_version(vapply(values, cmip6dict__decode_scalar_cell, character(1L), USE.NAMES = FALSE)),
        vapply(values, cmip6dict__decode_scalar_cell, character(1L), USE.NAMES = FALSE)
    )
}

cmip6dict__decode_list_cell <- function(x) {
    if (is.null(x)) return(character())
    x
}

cmip6dict__decode_scalar_cell <- function(x) {
    if (is.null(x) || !length(x)) return(NA_character_)
    if (is.list(x)) {
        if (!length(x) || is.null(x[[1L]])) return(NA_character_)
        x <- x[[1L]]
    }
    as.character(x[[1L]])
}
