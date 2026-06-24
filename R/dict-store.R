esgdict__build <- function(dict) {
    res <- list()
    res$project <- esgdict__normalize_project(if (is.null(dict$project)) "CMIP6" else dict$project)
    res$profile <- esgdict__profile(res$project)
    res$built_time <- dict$built_time
    res$sources <- dict$sources
    dict$built_time <- NULL
    dict$sources <- NULL

    vocab <- dict$vocab
    request <- dict$request
    if (is.null(vocab) && !is.null(dict$cvs)) vocab <- dict$cvs
    if (is.null(request) && !is.null(dict$dreq)) request <- dict$dreq

    if (is.null(vocab) && is.null(request)) {
        res$version <- NULL
        res$timestamps <- NULL
        res$data <- list(vocab = NULL, request = NULL)
        res$indices <- list()
        return(res)
    }

    if (is.null(vocab) || (!is.null(esgdict__project_spec(res$project)$request) && is.null(request))) {
        res$version <- NULL
        res$timestamps <- NULL
        res$data <- list(vocab = vocab, request = request)
        res$indices <- list()
        return(res)
    }

    res$version <- list(
        vocab = esgdict__vocab_version(vocab),
        request = esgdict__request_version(request)
    )

    res$timestamps <- esgdict__vocab_timestamps(vocab)

    res$data <- list(vocab = vocab, request = request)
    res$indices <- if (is.null(dict$indices)) {
        esgdict__indices(res$project, res$data)
    } else {
        esgdict__normalize_indices(dict$indices)
    }

    res
}

cmip6dict__build <- esgdict__build

esgdict__vocab_version <- function(vocab) {
    if (is.null(vocab)) return(NULL)
    if (!is.null(vocab$drs)) {
        return(attr(vocab$drs, "version", TRUE)$CV_collection_version)
    }
    for (item in vocab) {
        version <- attr(item, "version", TRUE)
        if (!is.null(version$CV_collection_version)) {
            return(version$CV_collection_version)
        }
    }
    NULL
}

esgdict__request_version <- function(request) {
    if (is.null(request)) return(NULL)
    attr(request, "metadata", TRUE)$dreq_version[[1L]]
}

esgdict__vocab_timestamps <- function(vocab) {
    if (is.null(vocab)) return(NULL)

    out <- lapply(vocab, function(cv) attr(cv, "version", TRUE)$CV_modified)
    if (!is.null(vocab$drs)) {
        out <- c(list(vocab = attr(vocab$drs, "version", TRUE)$CV_collection_modified), out)
    }
    out[lengths(out) > 0L]
}

esgdict__normalize_indices <- function(indices) {
    if (is.null(indices)) return(NULL)
    if ("dreq" %in% names(indices) && !"variable" %in% names(indices)) {
        names(indices)[names(indices) == "dreq"] <- "variable"
    }
    indices
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
    path
) {
    project <- esgdict__normalize_project(project)
    profile <- esgdict__profile(project)
    checkmate::assert_string(path, min.chars = 1L)

    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    payload <- esgdict__payload(project, profile, built_time, data, version, timestamps, sources, indices)
    schema_validate(SCHEMA_ESG_DICT, payload, mode = "assert", name = path)
    esgdict__validate_payload(payload, name = path)
    store_write_json_atomic(payload, path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null",
        na = "null"
    )
    path
}

esgdict__load <- function(path, project = "CMIP6") {
    project <- esgdict__normalize_project(project)
    checkmate::assert_string(path, min.chars = 1L)

    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
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

    esgdict__validate_body(payload$payload, project = project, name = name)
}

esgdict__validate_body <- function(payload, project = "CMIP6", name = "ESGDICT.json") {
    project <- esgdict__normalize_project(project)
    if (is.null(payload) || (is.null(payload$vocab) && is.null(payload$request))) {
        return(invisible(TRUE))
    }

    if (is.null(payload$vocab)) {
        stop(sprintf("%s is missing ESG dictionary vocab payload.", name), call. = FALSE)
    }

    for (cv_name in names(payload$vocab)) {
        cv <- payload$vocab[[cv_name]]
        kind <- as.character(cv$kind)
        if (!kind %in% c("table", "list", "vector")) {
            stop(sprintf("%s$vocab$%s has invalid kind `%s`.", name, cv_name, kind), call. = FALSE)
        }
        if (identical(kind, "table") && (is.null(cv$columns) || is.null(cv$rows))) {
            stop(sprintf("%s$vocab$%s table payload must contain columns and rows.", name, cv_name), call. = FALSE)
        }
        if (!identical(kind, "table") && is.null(cv$values)) {
            stop(sprintf("%s$vocab$%s %s payload must contain values.", name, cv_name, kind), call. = FALSE)
        }
    }

    if (identical(project, "CMIP6")) {
        required_cvs <- tolower(CV_TYPES)
        missing_cvs <- setdiff(required_cvs, names(payload$vocab))
        if (length(missing_cvs)) {
            stop(
                sprintf("%s is missing CMIP6 vocab payload(s): %s.", name, paste(missing_cvs, collapse = ", ")),
                call. = FALSE
            )
        }
        if (is.null(payload$request)) {
            stop(sprintf("%s is a partial CMIP6 dictionary payload.", name), call. = FALSE)
        }

        request_cols <- cmip6dict__payload_column_names(payload$request)
        missing_request <- setdiff(c("variable", "table_id", "frequency", "modeling_realm"), request_cols)
        if (length(missing_request)) {
            stop(
                sprintf("%s$request is missing required column(s): %s.", name, paste(missing_request, collapse = ", ")),
                call. = FALSE
            )
        }

        meta_cols <- cmip6dict__payload_column_names(payload$request_metadata)
        missing_meta <- setdiff(c("dreq_version", "cmor_version", "table_id", "mip_era"), meta_cols)
        if (length(missing_meta)) {
            stop(
                sprintf("%s$request_metadata is missing required column(s): %s.", name, paste(missing_meta, collapse = ", ")),
                call. = FALSE
            )
        }
    }

    if (!is.null(payload$indices)) {
        missing_indices <- setdiff("values", names(payload$indices))
        if (length(missing_indices)) {
            stop(
                sprintf("%s$indices is missing required index table(s): %s.", name, paste(missing_indices, collapse = ", ")),
                call. = FALSE
            )
        }
    }

    invisible(TRUE)
}

cmip6dict__validate_payload <- function(payload, name = "CMIP6DICT.json") {
    if (!is.null(payload) && "cvs" %in% names(payload)) {
        payload <- list(
            vocab = payload$cvs,
            request = payload$dreq,
            request_metadata = payload$dreq_metadata,
            indices = esgdict__normalize_indices(payload$indices)
        )
    }
    esgdict__validate_body(payload, project = "CMIP6", name = name)
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
        payload = esgdict__body_payload(data, indices)
    )
}

esgdict__body_payload <- function(data, indices = NULL) {
    list(
        vocab = esgdict__encode_vocab(data$vocab),
        request = cmip6dict__encode_table(data$request),
        request_metadata = cmip6dict__encode_table(attr(data$request, "metadata", TRUE), include_class = FALSE),
        indices = cmip6dict__encode_indices(indices)
    )
}

cmip6dict__payload <- function(data, indices = NULL) {
    if (is.null(data$vocab) && !is.null(data$cvs)) data$vocab <- data$cvs
    if (is.null(data$request) && !is.null(data$dreq)) data$request <- data$dreq
    esgdict__body_payload(data, indices)
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
        vocab = esgdict__decode_vocab(body$vocab),
        request = esgdict__decode_request(body$request, body$request_metadata),
        indices = esgdict__normalize_indices(cmip6dict__decode_indices(body$indices))
    )
}

cmip6dict__encode_version <- function(version) {
    if (is.null(version)) return(NULL)

    list(
        vocab = if (is.null(version$vocab)) NULL else as.character(version$vocab),
        request = if (is.null(version$request)) NULL else as.character(version$request)
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

esgdict__encode_vocab <- function(vocab) {
    if (is.null(vocab)) return(NULL)

    out <- lapply(vocab, cmip6dict__encode_cv)
    order <- unique(c(intersect(tolower(CV_TYPES), names(out)), names(out)))
    out[order]
}

cmip6dict__encode_cvs <- esgdict__encode_vocab

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

esgdict__decode_vocab <- function(vocab) {
    if (is.null(vocab)) return(NULL)

    out <- lapply(names(vocab), function(nm) cmip6dict__decode_cv(nm, vocab[[nm]]))
    names(out) <- names(vocab)
    out
}

cmip6dict__decode_cvs <- esgdict__decode_vocab

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

esgdict__decode_request <- function(payload, metadata) {
    if (is.null(payload)) return(NULL)

    request <- cmip6dict__decode_table(payload, class = cmip6dict__decode_class(payload$class))
    meta <- cmip6dict__decode_table(metadata)
    data.table::setattr(request, "metadata", meta)
    request
}

cmip6dict__decode_dreq <- esgdict__decode_request

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
