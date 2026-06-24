dict__build <- function(dict) {
    res <- list()
    res$project <- dict__project(if (is.null(dict$project)) "CMIP6" else dict$project)
    res$profile <- dict__profile(res$project)
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

    if (is.null(vocab) || (!is.null(dict__spec(res$project)$request) && is.null(request))) {
        res$version <- NULL
        res$timestamps <- NULL
        res$data <- list(vocab = vocab, request = request)
        res$indices <- list()
        return(res)
    }

    res$version <- list(
        vocab = dict__vocab_version(vocab),
        request = dict__request_version(request)
    )

    res$timestamps <- dict__vocab_times(vocab)

    res$data <- list(vocab = vocab, request = request)
    res$indices <- if (is.null(dict$indices)) {
        dict__make_indices(res$project, res$data)
    } else {
        dict__norm_indices(dict$indices)
    }

    res
}

dict__vocab_version <- function(vocab) {
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

dict__request_version <- function(request) {
    if (is.null(request)) return(NULL)
    attr(request, "metadata", TRUE)$dreq_version[[1L]]
}

dict__vocab_times <- function(vocab) {
    if (is.null(vocab)) return(NULL)

    out <- lapply(vocab, function(cv) attr(cv, "version", TRUE)$CV_modified)
    if (!is.null(vocab$drs)) {
        out <- c(list(vocab = attr(vocab$drs, "version", TRUE)$CV_collection_modified), out)
    }
    out[lengths(out) > 0L]
}

dict__norm_indices <- function(indices) {
    if (is.null(indices)) return(NULL)
    if ("dreq" %in% names(indices) && !"variable" %in% names(indices)) {
        names(indices)[names(indices) == "dreq"] <- "variable"
    }
    indices
}

dict__save <- function(
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
    project <- dict__project(project)
    profile <- dict__profile(project)
    checkmate::assert_string(path, min.chars = 1L)

    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    payload <- dict__payload(project, profile, built_time, data, version, timestamps, sources, indices)
    schema_validate(SCHEMA_ESG_DICT, payload, mode = "assert", name = path)
    dict__validate(payload, name = path)
    store_write_json_atomic(payload, path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null",
        na = "null"
    )
    path
}

dict__load <- function(path, project = "CMIP6") {
    project <- dict__project(project)
    checkmate::assert_string(path, min.chars = 1L)

    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (!file.exists(path)) return(NULL)

    payload <- jsonlite::read_json(path, simplifyVector = FALSE)
    schema_validate(SCHEMA_ESG_DICT, payload, mode = "assert", name = path)
    dict__validate(payload, name = path)

    loaded_project <- dict__project(payload$project)
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

    dict__from_payload(payload)
}

dict__validate <- function(payload, name = "ESGDICT.json") {
    project <- dict__project(payload$project)
    profile <- dict__profile(project)
    if (!identical(as.character(payload$profile), profile)) {
        stop(
            sprintf("%s profile `%s` does not match project `%s`.", name, payload$profile, project),
            call. = FALSE
        )
    }

    dict__validate_body(payload$payload, project = project, name = name)
}

dict__validate_body <- function(payload, project = "CMIP6", name = "ESGDICT.json") {
    project <- dict__project(project)
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

        request_cols <- dict__payload_cols(payload$request)
        missing_request <- setdiff(c("variable", "table_id", "frequency", "modeling_realm"), request_cols)
        if (length(missing_request)) {
            stop(
                sprintf("%s$request is missing required column(s): %s.", name, paste(missing_request, collapse = ", ")),
                call. = FALSE
            )
        }

        meta_cols <- dict__payload_cols(payload$request_metadata)
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

dict__payload_cols <- function(payload) {
    if (is.null(payload) || is.null(payload$columns)) {
        return(character())
    }
    vapply(payload$columns, function(col) as.character(col$name), character(1L), USE.NAMES = FALSE)
}

dict__payload <- function(
    project,
    profile,
    built_time,
    data,
    version = NULL,
    timestamps = NULL,
    sources = NULL,
    indices = NULL
) {
    project <- dict__project(project)
    profile <- dict__profile(project)
    list(
        format = ESGDICT_FORMAT,
        format_version = ESGDICT_FORMAT_VERSION,
        project = project,
        profile = profile,
        built_time = dict__enc_time(built_time),
        version = dict__enc_version(version),
        timestamps = dict__enc(timestamps),
        sources = dict__enc(sources),
        payload = dict__body(data, indices)
    )
}

dict__body <- function(data, indices = NULL) {
    list(
        vocab = dict__enc_vocab(data$vocab),
        request = dict__enc_table(data$request),
        request_metadata = dict__enc_table(attr(data$request, "metadata", TRUE), include_class = FALSE),
        indices = dict__enc_indices(indices)
    )
}

dict__from_payload <- function(payload) {
    project <- dict__project(payload$project)
    profile <- dict__profile(project)
    body <- payload$payload
    list(
        project = project,
        profile = profile,
        built_time = dict__dec_time(payload$built_time),
        sources = payload$sources,
        vocab = dict__dec_vocab(body$vocab),
        request = dict__dec_request(body$request, body$request_metadata),
        indices = dict__norm_indices(dict__dec_indices(body$indices))
    )
}

dict__enc_version <- function(version) {
    if (is.null(version)) return(NULL)

    list(
        vocab = if (is.null(version$vocab)) NULL else as.character(version$vocab),
        request = if (is.null(version$request)) NULL else as.character(version$request)
    )
}

dict__enc_time <- function(x) {
    if (is.null(x)) return(NULL)
    format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

dict__dec_time <- function(x) {
    if (is.null(x) || !length(x) || is.na(x)) return(NULL)
    as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
}

dict__enc <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "numeric_version")) return(as.character(x))
    if (inherits(x, "POSIXt")) return(vapply(x, dict__enc_time, character(1L), USE.NAMES = FALSE))
    if (inherits(x, "Date")) return(as.character(x))
    if (is.list(x) && !is.data.frame(x)) return(lapply(x, dict__enc))
    if (is.atomic(x)) return(as.vector(x))
    x
}

dict__enc_vocab <- function(vocab) {
    if (is.null(vocab)) return(NULL)

    out <- lapply(vocab, dict__enc_cv)
    order <- unique(c(intersect(tolower(CV_TYPES), names(out)), names(out)))
    out[order]
}

dict__enc_cv <- function(cv) {
    payload <- list(
        kind = dict__cv_kind(cv),
        class = class(cv),
        version = dict__enc(attr(cv, "version", TRUE))
    )

    if (is.data.frame(cv)) {
        payload$columns <- dict__cols(cv)
        payload$rows <- dict__enc_rows(cv)
    } else {
        payload$values <- dict__enc(unclass(cv))
    }

    payload
}

dict__cv_kind <- function(cv) {
    if (is.data.frame(cv)) return("table")
    if (is.list(cv)) return("list")
    "vector"
}

dict__enc_indices <- function(indices) {
    if (is.null(indices) || !length(indices)) return(NULL)
    lapply(indices, dict__enc_table)
}

dict__dec_indices <- function(indices) {
    if (is.null(indices)) return(NULL)
    out <- lapply(indices, dict__dec_table)
    names(out) <- names(indices)
    out
}

dict__enc_table <- function(x, include_class = TRUE) {
    if (is.null(x)) return(NULL)

    out <- list(
        columns = dict__cols(x),
        rows = dict__enc_rows(x)
    )
    if (include_class) {
        out <- c(list(class = class(x)), out)
    }
    out
}

dict__cols <- function(x) {
    lapply(names(x), function(col) {
        list(name = col, type = dict__col_type(x[[col]]))
    })
}

dict__col_type <- function(x) {
    if (is.list(x) && !inherits(x, c("Date", "POSIXt", "numeric_version"))) return("list")
    if (inherits(x, "numeric_version")) return("numeric_version")
    if (inherits(x, "POSIXt")) return("posixct")
    if (inherits(x, "Date")) return("date")
    if (is.integer(x)) return("integer")
    if (is.double(x)) return("numeric")
    if (is.logical(x)) return("logical")
    "character"
}

dict__enc_rows <- function(x) {
    if (is.null(x) || !nrow(x)) return(list())

    lapply(seq_len(nrow(x)), function(i) {
        row <- lapply(names(x), function(col) {
            dict__enc(dict__cell(x[[col]], i))
        })
        names(row) <- names(x)
        row
    })
}

dict__cell <- function(col, i) {
    if (is.list(col) && !inherits(col, c("Date", "POSIXt", "numeric_version"))) {
        return(col[[i]])
    }
    col[i]
}

dict__dec_vocab <- function(vocab) {
    if (is.null(vocab)) return(NULL)

    out <- lapply(names(vocab), function(nm) dict__dec_cv(nm, vocab[[nm]]))
    names(out) <- names(vocab)
    out
}

dict__dec_cv <- function(name, payload) {
    cls <- dict__dec_class(payload$class)
    version <- dict__dec_cv_version(payload$version)

    if (identical(payload$kind, "table")) {
        x <- dict__dec_table(payload, class = cls)
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

dict__dec_class <- function(x) {
    if (is.null(x)) return(NULL)
    as.character(unlst(x))
}

dict__dec_cv_version <- function(x) {
    if (is.null(x)) return(NULL)

    list(
        CV_collection_version = as.numeric_version(x$CV_collection_version),
        CV_collection_modified = dict__dec_time(x$CV_collection_modified),
        CV_modified = dict__dec_time(x$CV_modified),
        CV_note = as.character(x$CV_note)
    )
}

dict__dec_request <- function(payload, metadata) {
    if (is.null(payload)) return(NULL)

    request <- dict__dec_table(payload, class = dict__dec_class(payload$class))
    meta <- dict__dec_table(metadata)
    data.table::setattr(request, "metadata", meta)
    request
}

dict__dec_table <- function(payload, class = NULL) {
    if (is.null(payload)) return(NULL)

    columns <- dict__dec_cols(payload$columns)
    rows <- payload$rows
    values <- lapply(columns$name, function(col) {
        lapply(rows, function(row) {
            if (col %in% names(row)) row[[col]] else NULL
        })
    })
    names(values) <- columns$name

    for (i in seq_len(nrow(columns))) {
        col <- columns$name[[i]]
        values[[col]] <- dict__dec_col(values[[col]], columns$type[[i]])
    }

    out <- data.table::as.data.table(values)

    if (!is.null(class)) {
        class(out) <- unique(c(class, class(out)))
    }
    out
}

dict__dec_cols <- function(columns) {
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

dict__dec_col <- function(values, type) {
    switch(type,
        list = lapply(values, dict__dec_list_cell),
        integer = suppressWarnings(as.integer(vapply(values, dict__dec_scalar, character(1L), USE.NAMES = FALSE))),
        numeric = suppressWarnings(as.numeric(vapply(values, dict__dec_scalar, character(1L), USE.NAMES = FALSE))),
        logical = as.logical(vapply(values, dict__dec_scalar, character(1L), USE.NAMES = FALSE)),
        date = as.Date(vapply(values, dict__dec_scalar, character(1L), USE.NAMES = FALSE)),
        posixct = as.POSIXct(vapply(values, dict__dec_scalar, character(1L), USE.NAMES = FALSE), tz = "UTC"),
        numeric_version = as.numeric_version(vapply(values, dict__dec_scalar, character(1L), USE.NAMES = FALSE)),
        vapply(values, dict__dec_scalar, character(1L), USE.NAMES = FALSE)
    )
}

dict__dec_list_cell <- function(x) {
    if (is.null(x)) return(character())
    x
}

dict__dec_scalar <- function(x) {
    if (is.null(x) || !length(x)) return(NA_character_)
    if (is.list(x)) {
        if (!length(x) || is.null(x[[1L]])) return(NA_character_)
        x <- x[[1L]]
    }
    as.character(x[[1L]])
}
