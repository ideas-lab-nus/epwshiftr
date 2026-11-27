# dict__source_dir {{{
dict__source_dir <- function(root = store_dir(init = TRUE), project = "CMIP6") {
    file.path(root, "sources", "esg-dict", tolower(dict__project(project)))
}
# }}}

# dict__tmp_source_dir {{{
dict__tmp_source_dir <- function(project = "CMIP6") {
    file.path(
        tempdir(),
        paste0(
            "esg-dict-source-",
            tolower(dict__project(project)),
            "-",
            fast_hash(list(
                Sys.getpid(),
                Sys.time(),
                sample.int(.Machine$integer.max, 1L)
            ))
        )
    )
}
# }}}

# dict__cache_key {{{
dict__cache_key <- function(project, cv_tag_info, request_tag_info, spec) {
    cache__key(
        "esgdict",
        project = dict__project(project),
        profile = dict__profile(project),
        vocab_reader = spec$vocab$reader,
        cv_tag = cv_tag_info$tag,
        cv_commit = cv_tag_info$commit,
        request_reader = spec$request$reader,
        request_tag = request_tag_info$tag,
        request_commit = request_tag_info$commit,
        format_version = ESGDICT_FORMAT_VERSION
    )
}
# }}}

# dict__fetch {{{
dict__fetch <- function(
    project = "CMIP6",
    token = NULL,
    cv_tag = NULL,
    request_tag = NULL,
    policy,
    source_dir = dict__source_dir(project = project),
    force = FALSE
) {
    project <- dict__project(project)
    spec <- dict__spec(project)
    checkmate::assert_string(cv_tag, null.ok = TRUE)
    checkmate::assert_string(request_tag, null.ok = TRUE)
    checkmate::assert_flag(force)

    cv_tag_info <- dict__resolve_ref(spec$vocab, cv_tag, token, policy)
    request_tag_info <- dict__resolve_ref(
        spec$request,
        request_tag,
        token,
        policy
    )
    cache_key <- dict__cache_key(project, cv_tag_info, request_tag_info, spec)

    # Parsed vocab/request payloads are cached separately from source JSON files. A forced
    # build skips this read path, but still writes a fresh parsed value later.
    if (isTRUE(policy$read) && !isTRUE(force)) {
        cached <- cache__get()$get(cache_key)
        if (!cache__missing(cached)) {
            cached$built_time <- Sys.time()
            return(cached)
        }
    }

    source_dir <- if (
        isTRUE(policy$source_read) || isTRUE(policy$source_write)
    ) {
        source_dir
    } else {
        dict__tmp_source_dir(project)
    }

    fetched <- dict__fetch_resolved(
        project = project,
        spec = spec,
        cv_tag_info = cv_tag_info,
        request_tag_info = request_tag_info,
        token = token,
        policy = policy,
        source_dir = source_dir
    )

    if (isTRUE(policy$write)) {
        cache__get()$set(
            cache_key,
            list(
                project = fetched$project,
                profile = fetched$profile,
                vocab = fetched$vocab,
                request = fetched$request,
                sources = fetched$sources
            )
        )
    }

    fetched
}
# }}}

# dict__fetch_resolved {{{
dict__fetch_resolved <- function(
    project,
    spec,
    cv_tag_info,
    request_tag_info,
    token = NULL,
    policy,
    source_dir = dict__source_dir(project = project)
) {
    project <- dict__project(project)
    cli::cli_progress_step(
        "Fetching {.strong {project} ESG Dictionary}...",
        "Fetched {.strong {project} ESG Dictionary} successfully at {Sys.time()}",
        "Failed to fetch {.strong {project} ESG Dictionary}.",
        spinner = TRUE
    )

    vocab <- dict__read_vocab(
        spec$vocab$reader,
        project = project,
        source = spec$vocab,
        tag = cv_tag_info$tag,
        token = token,
        use_source = isTRUE(policy$source_read),
        write_source = isTRUE(policy$source_write),
        offline = isTRUE(policy$offline),
        source_dir = file.path(source_dir, "vocab", cv_tag_info$tag)
    )
    request <- NULL
    if (!is.null(spec$request)) {
        request <- dict__read_request(
            spec$request$reader,
            project = project,
            source = spec$request,
            tag = request_tag_info$tag,
            token = token,
            use_source = isTRUE(policy$source_read),
            write_source = isTRUE(policy$source_write),
            offline = isTRUE(policy$offline),
            source_dir = file.path(source_dir, "request", request_tag_info$tag)
        )
    }

    list(
        project = project,
        profile = dict__profile(project),
        vocab = vocab,
        request = request,
        built_time = Sys.time(),
        sources = list(
            vocab = dict__source_info(
                spec$vocab$repo,
                cv_tag_info,
                file.path(source_dir, "vocab", cv_tag_info$tag)
            ),
            request = if (!is.null(spec$request)) {
                dict__source_info(
                    spec$request$repo,
                    request_tag_info,
                    file.path(source_dir, "request", request_tag_info$tag)
                )
            } else {
                NULL
            }
        )
    )
}
# }}}

# dict__resolve_tag_cache {{{
dict__resolve_tag_cache <- function(repo, tag = NULL, token = NULL, policy) {
    if (!is.null(tag)) {
        return(dict__resolve_tag(repo, tag, token))
    }

    if (!isTRUE(policy$read)) {
        return(dict__resolve_tag(repo, tag, token))
    }

    tryCatch(
        cache__url(
            "esgdict-tag",
            list(repo = repo),
            fn = function() dict__resolve_tag(repo, tag, token),
            validate = function(x) {
                is.list(x) &&
                    is.character(x$tag) &&
                    length(x$tag) == 1L &&
                    !is.na(x$tag) &&
                    nzchar(x$tag)
            }
        ),
        error = function(e) {
            if (isTRUE(policy$offline)) {
                stop(
                    sprintf(
                        paste(
                            "Cannot resolve the latest CMIP6 tag for `%s` in offline mode.",
                            "Supply explicit `cv_tag`/`request_tag`, or build once with `epwshiftr.cache = TRUE`."
                        ),
                        repo
                    ),
                    call. = FALSE
                )
            }
            stop(e)
        }
    )
}
# }}}

# dict__resolve_ref {{{
dict__resolve_ref <- function(source, tag = NULL, token = NULL, policy) {
    if (is.null(source)) {
        return(list(tag = NULL, commit = NULL))
    }
    if (!is.null(tag)) {
        return(list(tag = tag, commit = NA_character_))
    }
    if (!is.null(source$ref) && !isTRUE(source$tagged)) {
        return(list(tag = source$ref, commit = NA_character_))
    }

    dict__resolve_tag_cache(source$repo, tag, token, policy)
}
# }}}

# dict__resolve_tag {{{
dict__resolve_tag <- function(repo, tag = NULL, token = NULL) {
    if (!is.null(tag)) {
        return(list(tag = tag, commit = NA_character_))
    }

    tag_row <- gh_tags(repo, token)[1L, ]
    list(
        tag = dict__tag_value(tag_row, "name"),
        commit = dict__tag_commit(tag_row)
    )
}
# }}}

# dict__read_vocab {{{
dict__read_vocab <- function(
    reader,
    project,
    source,
    tag,
    token = NULL,
    use_source = TRUE,
    source_dir = tempdir(),
    write_source = use_source,
    offline = FALSE
) {
    switch(
        reader,
        cmip6_cvs = dict__fetch_cv(
            tag,
            token,
            use_source,
            source_dir,
            write_source,
            offline,
            repo = source$repo
        ),
        esgvoc = dict__fetch_voc(
            project,
            source,
            tag,
            token,
            use_source,
            source_dir,
            write_source,
            offline
        ),
        stop(
            sprintf("Unknown ESG dictionary vocab reader `%s`.", reader),
            call. = FALSE
        )
    )
}
# }}}

# dict__read_request {{{
dict__read_request <- function(
    reader,
    project,
    source,
    tag,
    token = NULL,
    use_source = TRUE,
    source_dir = tempdir(),
    write_source = use_source,
    offline = FALSE
) {
    switch(
        reader,
        cmip6_cmor = dict__fetch_dreq(
            tag,
            token,
            use_source,
            source_dir,
            write_source,
            offline,
            repo = source$repo
        ),
        stop(
            sprintf("Unknown ESG dictionary request reader `%s`.", reader),
            call. = FALSE
        )
    )
}
# }}}

# dict__tag_value {{{
dict__tag_value <- function(tag_row, name) {
    value <- tag_row[[name]]
    if (is.list(value)) {
        value <- value[[1L]]
    }
    as.character(value[[1L]])
}
# }}}

# dict__tag_commit {{{
dict__tag_commit <- function(tag_row) {
    if ("commit.sha" %in% names(tag_row)) {
        return(dict__tag_value(tag_row, "commit.sha"))
    }
    if ("commit" %in% names(tag_row)) {
        commit <- tag_row[["commit"]]
        if (is.data.frame(commit) && "sha" %in% names(commit)) {
            return(as.character(commit$sha[[1L]]))
        }
        if (is.list(commit) && "sha" %in% names(commit[[1L]])) {
            return(as.character(commit[[1L]]$sha))
        }
    }
    NA_character_
}
# }}}

# dict__source_info {{{
dict__source_info <- function(repo, tag_info, source_dir) {
    list(
        repo = repo,
        tag = tag_info$tag,
        commit = tag_info$commit,
        source_dir = normalizePath(source_dir, winslash = "/", mustWork = FALSE)
    )
}
# }}}

# dict__source_ready {{{
dict__source_ready <- function(files) {
    length(files) && all(file.exists(files))
}
# }}}

# dict__source_miss {{{
dict__source_miss <- function(kind, dir) {
    stop(
        sprintf(
            paste(
                "%s source miss in offline mode at `%s`.",
                "Build the dictionary once online, or pass a `source_dir` containing the required source files."
            ),
            kind,
            normalizePath(dir, winslash = "/", mustWork = FALSE)
        ),
        call. = FALSE
    )
}
# }}}

# dict__fetch_cv {{{
dict__fetch_cv <- function(
    tag,
    token = NULL,
    use_source = TRUE,
    source_dir = tempdir(),
    write_source = use_source,
    offline = FALSE,
    repo = dict__spec("CMIP6")$vocab$repo
) {
    checkmate::assert_string(tag, min.chars = 1L)
    checkmate::assert_string(repo, min.chars = 1L)

    files <- dict__download_cv(
        tag,
        repo = repo,
        dir = source_dir,
        token = token,
        use_source = use_source,
        write_source = write_source,
        offline = offline
    )

    cvs <- list()
    for (type in names(files)) {
        abbr <- tolower(tools::file_path_sans_ext(type))
        cvs[[abbr]] <- match.fun(sprintf("dict__parse_cv_%s", abbr))(files[[
            type
        ]])
    }

    cvs
}
# }}}

# dict__download_cv {{{
dict__download_cv <- function(
    tag,
    repo = dict__spec("CMIP6")$vocab$repo,
    dir = tempdir(),
    token = NULL,
    use_source = TRUE,
    write_source = use_source,
    offline = FALSE
) {
    checkmate::assert_string(repo, min.chars = 1L)
    dests <- file.path(dir, sprintf("CMIP6_%s.json", CV_TYPES))
    names(dests) <- CV_TYPES
    if (use_source && dict__source_ready(dests)) {
        return(dests)
    }
    if (isTRUE(offline)) {
        dict__source_miss("CMIP6 CV", dir)
    }

    # Source CV files stay as normal JSON files so users can inspect exactly what
    # came from the upstream CV repository when source persistence is enabled.
    if (!isTRUE(write_source)) {
        dir <- file.path(dict__tmp_source_dir(), "cvs", tag)
        dests <- file.path(dir, sprintf("CMIP6_%s.json", CV_TYPES))
        names(dests) <- CV_TYPES
    }

    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }

    file <- ""
    cli::cli_progress_step(
        "Downloading data of {.strong CMIP6 CVs} [{.file {file}}]...",
        "Downloaded data of {.strong CMIP6 CVs} successfully.",
        "Failed to download data of {.strong CMIP6 CVs}.",
        spinner = TRUE
    )
    for (type in CV_TYPES) {
        file <- sprintf("CMIP6_%s.json", type)
        cli::cli_progress_update(1L)
        dests[[type]] <- download_gh_file(repo, tag, file, dir, token)
    }

    dests
}
# }}}

# dict__fetch_voc {{{
dict__fetch_voc <- function(
    project,
    source,
    tag,
    token = NULL,
    use_source = TRUE,
    source_dir = tempdir(),
    write_source = use_source,
    offline = FALSE
) {
    checkmate::assert_string(tag, min.chars = 1L)

    files <- dict__download_voc(
        source = source,
        tag = tag,
        dir = source_dir,
        token = token,
        use_source = use_source,
        write_source = write_source,
        offline = offline
    )
    dict__parse_voc(files, project = project)
}
# }}}

# dict__download_voc {{{
dict__download_voc <- function(
    source,
    tag,
    dir = tempdir(),
    token = NULL,
    use_source = TRUE,
    write_source = use_source,
    offline = FALSE
) {
    files <- list.files(
        dir,
        pattern = "[.]json$",
        full.names = TRUE,
        recursive = TRUE
    )
    if (use_source && length(files)) {
        names(files) <- dict__rel_paths(files, dir)
        return(files)
    }
    if (isTRUE(offline)) {
        dict__source_miss("ESG vocabulary", dir)
    }

    if (!isTRUE(write_source)) {
        dir <- file.path(dict__tmp_source_dir(), "vocab", tag)
    }
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }

    cli::cli_progress_step(
        "Downloading data of {.strong ESG vocabulary}...",
        "Downloaded data of {.strong ESG vocabulary} successfully.",
        "Failed to download data of {.strong ESG vocabulary}.",
        spinner = TRUE
    )
    zipball <- download_gh_ref(source$repo, tag, tempdir(), token)
    archive <- utils::unzip(zipball, list = TRUE)
    json_files <- archive$Name[grepl("[.]json$", archive$Name)]
    json_files <- json_files[!grepl("(^|/)[.]", json_files)]
    extracted <- dict__unzip_archive(zipball, "esg-vocab", json_files)
    files <- extracted[grepl("[.]json$", extracted)]
    files <- files[!grepl("(^|/)[.]", files)]

    rel <- dict__archive_paths(files)
    dests <- file.path(dir, rel)
    if (length(dests)) {
        for (dest_dir in unique(dirname(dests))) {
            dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
        }
        ok <- file.copy(files, dests, overwrite = TRUE)
        if (!all(ok)) {
            stop(
                "Failed to cache downloaded ESG vocabulary files.",
                call. = FALSE
            )
        }
    }

    names(dests) <- rel
    dests
}
# }}}

# dict__rel_paths {{{
dict__rel_paths <- function(files, root) {
    sub(
        sprintf(
            "^%s/+",
            gsub(
                "([][{}()+*^$|\\\\?.])",
                "\\\\\\1",
                normalizePath(root, winslash = "/", mustWork = FALSE)
            )
        ),
        "",
        normalizePath(files, winslash = "/", mustWork = FALSE)
    )
}
# }}}

# dict__unzip_archive {{{
dict__unzip_archive <- function(
    zipball,
    prefix = "esg-dict-archive",
    files = NULL
) {
    if (!is.null(files) && !length(files)) {
        return(character())
    }
    exdir <- tempfile(pattern = paste0(prefix, "-"), tmpdir = tempdir())
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(zipball, files = files, exdir = exdir)
}
# }}}

# dict__archive_paths {{{
dict__archive_paths <- function(files) {
    files <- normalizePath(files, winslash = "/", mustWork = FALSE)
    parts <- strsplit(files, "/", fixed = TRUE)
    min_len <- min(lengths(parts))
    common <- 0L
    for (i in seq_len(min_len)) {
        values <- vapply(parts, `[[`, character(1L), i)
        if (length(unique(values)) != 1L) {
            break
        }
        common <- i
    }
    common <- max(common - 1L, 0L)
    vapply(
        parts,
        function(x) paste(x[(common + 1L):length(x)], collapse = "/"),
        character(1L),
        USE.NAMES = FALSE
    )
}
# }}}

# dict__parse_voc {{{
dict__parse_voc <- function(files, project) {
    rows <- lapply(files, dict__parse_voc_file)
    rows <- rows[lengths(rows) > 0L]
    if (!length(rows)) {
        return(list())
    }

    all <- unique(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
    split <- split(all, all$field)
    out <- lapply(names(split), function(field) {
        dt <- data.table::as.data.table(split[[field]])
        dt[, field := NULL]
        if ("value" %in% names(dt)) {
            data.table::setnames(dt, "value", field)
        }
        unique(dt)
    })
    names(out) <- names(split)
    out
}
# }}}

# dict__parse_voc_file {{{
dict__parse_voc_file <- function(file) {
    json <- tryCatch(jsonlite::read_json(file), error = function(e) NULL)
    if (is.null(json) || !length(json)) {
        return(NULL)
    }

    collection <- dict__voc_collection(json, file)
    if (is.null(collection) || !nzchar(collection)) {
        return(NULL)
    }

    if (
        collection %in%
            names(json) &&
            is.list(json[[collection]]) &&
            length(json[[collection]]) > 1L
    ) {
        return(dict__voc_rows(collection, json[[collection]]))
    }

    dict__voc_term_rows(collection, json, file)
}
# }}}

# dict__voc_collection {{{
dict__voc_collection <- function(json, file) {
    nms <- setdiff(names(json), c("version_metadata", "@context", "$schema"))
    if (
        length(nms) == 1L &&
            is.list(json[[nms]]) &&
            !any(c("@id", "id", "drs_name") %in% names(json))
    ) {
        return(dict__voc_field(nms))
    }

    parent <- basename(dirname(file))
    stem <- tools::file_path_sans_ext(basename(file))
    if (
        !identical(parent, ".") &&
            nzchar(parent) &&
            !parent %in% c("vocab", "raw")
    ) {
        return(dict__voc_field(parent))
    }
    dict__voc_field(sub("^[A-Za-z0-9]+_", "", stem))
}
# }}}

# dict__voc_field {{{
dict__voc_field <- function(field) {
    field <- tolower(field)
    # esgvoc collection names are often shorter than ESGF query field names.
    # Normalize them before building the shared value index.
    aliases <- c(
        activity = "activity_id",
        experiment = "experiment_id",
        source = "source_id",
        variable = "variable_id",
        table = "table_id",
        institution = "institution_id",
        sub_experiment = "sub_experiment_id",
        nominal_resolution = "nominal_resolution"
    )
    if (field %in% names(aliases)) {
        unname(aliases[[field]])
    } else {
        field
    }
}
# }}}

# dict__voc_rows {{{
dict__voc_rows <- function(field, values) {
    if (is.atomic(values)) {
        return(dict__value_rows(field, values, NA_character_, "vocab"))
    }

    if (is.list(values) && !is.null(names(values))) {
        return(dict__value_rows(
            field,
            names(values),
            vapply(values, dict__desc, character(1L), USE.NAMES = FALSE),
            "vocab"
        ))
    }

    rows <- lapply(values, function(term) {
        dict__voc_term_rows(field, term, NULL)
    })
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
# }}}

# dict__voc_term_rows {{{
dict__voc_term_rows <- function(field, term, file = NULL) {
    value <- dict__first(term, c("drs_name", "id", "@id", "term", "label"))
    if (is.null(value) && !is.null(file)) {
        value <- tools::file_path_sans_ext(basename(file))
    }
    if (is.null(value) || !nzchar(value)) {
        return(NULL)
    }

    dict__value_rows(
        field,
        value,
        dict__first(
            term,
            c("description", "label_extended", "label", "title", "name")
        ),
        "vocab"
    )
}
# }}}

# dict__first {{{
dict__first <- function(x, names) {
    for (nm in names) {
        value <- x[[nm]]
        if (is.null(value)) {
            next
        }
        value <- unlst(value)
        if (
            length(value) &&
                !is.na(value[[1L]]) &&
                nzchar(as.character(value[[1L]]))
        ) {
            return(as.character(value[[1L]]))
        }
    }
    NULL
}
# }}}

# dict__fetch_dreq {{{
dict__fetch_dreq <- function(
    tag,
    token = NULL,
    use_source = TRUE,
    source_dir = tempdir(),
    write_source = use_source,
    offline = FALSE,
    repo = dict__spec("CMIP6")$request$repo
) {
    checkmate::assert_string(tag, min.chars = 1L)
    checkmate::assert_string(repo, min.chars = 1L)

    files <- dict__download_dreq(
        tag,
        repo = repo,
        dir = source_dir,
        token = token,
        use_source = use_source,
        write_source = write_source,
        offline = offline
    )

    dreq <- lapply(files, dict__parse_dreq)
    metadata <- lapply(dreq, attr, "metadata", TRUE)

    for (nm in names(dreq)) {
        data.table::set(
            dreq[[nm]],
            NULL,
            "table_id",
            metadata[[nm]][["table_id"]]
        )
    }

    dreq <- data.table::rbindlist(dreq, use.names = TRUE)
    data.table::setcolorder(
        dreq,
        c(
            "variable",
            "table_id",
            "modeling_realm",
            "standard_name",
            "long_name"
        )
    )
    structure(
        dreq,
        metadata = data.table::rbindlist(metadata, use.names = TRUE),
        class = c("Cmip6DReq", class(dreq))
    )
}
# }}}

# dict__download_dreq {{{
dict__download_dreq <- function(
    tag,
    repo = dict__spec("CMIP6")$request$repo,
    dir = tempdir(),
    token = NULL,
    use_source = TRUE,
    write_source = use_source,
    offline = FALSE
) {
    checkmate::assert_string(repo, min.chars = 1L)
    files <- list.files(dir, pattern = "^CMIP6_.*[.]json$", full.names = TRUE)
    if (use_source && length(files)) {
        names(files) <- basename(files)
        return(files)
    }
    if (isTRUE(offline)) {
        dict__source_miss("CMIP6 DReq", dir)
    }

    # DReq is downloaded as an archive, but the store keeps only the extracted
    # table JSON files. That keeps the source stable and easy to diff.
    if (!isTRUE(write_source)) {
        dir <- file.path(dict__tmp_source_dir(), "dreq", tag)
    }

    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }

    cli::cli_progress_step(
        "Downloading data of {.strong CMIP6 DReq}...",
        "Downloaded data of {.strong CMIP6 DReq} successfully.",
        "Failed to download data of {.strong CMIP6 DReq}.",
        spinner = TRUE
    )
    zipball <- download_gh_tag(repo, tag, tempdir(), token)
    extracted <- dict__unzip_archive(zipball, "cmip6-dreq")

    files <- extracted[basename(dirname(extracted)) == "Tables"]
    names(files) <- basename(files)

    exclu <- c("CV", "coordinate", "formula_terms", "grids", "input_example")
    files <- files[!names(files) %in% sprintf("CMIP6_%s.json", exclu)]
    dests <- file.path(dir, names(files))
    ok <- file.copy(files, dests, overwrite = TRUE)
    if (!all(ok)) {
        stop("Failed to cache downloaded CMIP6 DReq files.", call. = FALSE)
    }

    names(dests) <- names(files)
    dests
}
# }}}

# gh_token {{{
gh_token <- function(token = NULL, header = TRUE) {
    checkmate::assert_string(token, null.ok = TRUE)

    if (is.null(token)) {
        token <- Sys.getenv("GITHUB_PAT")

        if (token == "") {
            token <- Sys.getenv("GITHUB_TOKEN")
        }

        if (token == "") return(NULL)
    }

    token <- trimws(token)
    if (!nchar(token)) {
        return(NULL)
    }

    if (
        !grepl("^gh[pousr]_[A-Za-z0-9_]{36,251}$", token) &&
            !grepl("[[:xdigit:]]{40}", token)
    ) {
        return(NULL)
    } else {
        if (!header) {
            return(token)
        } else {
            return(c("Authorization" = paste("token", token)))
        }
    }
}
# }}}

# gh {{{
gh <- function(path, token = NULL) {
    base_url <- "https://api.github.com"
    link <- file.path(base_url, path)

    cache__url("gh", link, function() {
        # use GitHub token if possible
        headers <- c("Accept" = "application/vnd.github+json")

        if (!is.null(token <- gh_token(token))) {
            headers <- c(headers, token)
        }

        jsonlite::fromJSON(base::url(link, headers = headers))
    })
}
# }}}

# TODO: pagination for tags
# gh_tags {{{
gh_tags <- function(repo, token = NULL) {
    gh(sprintf("repos/%s/tags", repo), token)
}
# }}}

# gh__download {{{
gh__download <- function(url, dest, token = NULL) {
    cache__download(url, dest, function() {
        utils::download.file(
            url,
            dest,
            mode = "wb",
            headers = gh_token(token),
            quiet = TRUE
        )
        dest
    })
}
# }}}

# download_gh_file {{{
download_gh_file <- function(repo, tag, file, dir = tempdir(), token = NULL) {
    url <- sprintf(
        "https://raw.githubusercontent.com/%s/%s/%s",
        repo,
        tag,
        file
    )
    gh__download(url, file.path(dir, file), token)
}
# }}}

# download_gh_tag {{{
download_gh_tag <- function(repo, tag, dir = tempdir(), token = NULL) {
    url <- sprintf(
        "https://api.github.com/repos/%s/zipball/refs/tags/%s",
        repo,
        tag
    )
    dest <- file.path(dir, sprintf("%s-%s.zip", basename(repo), tag))
    gh__download(url, dest, token)
}
# }}}

# download_gh_ref {{{
download_gh_ref <- function(repo, ref, dir = tempdir(), token = NULL) {
    url <- sprintf("https://api.github.com/repos/%s/zipball/%s", repo, ref)
    ref_file <- gsub("[/\\\\]", "-", ref)
    dest <- file.path(dir, sprintf("%s-%s.zip", basename(repo), ref_file))
    gh__download(url, dest, token)
}
# }}}

# dict__nest {{{
dict__nest <- function(json) {
    transposed <- lapply(names(json[[1L]]), function(nm) lapply(json, "[[", nm))
    data.table::setnames(
        data.table::as.data.table(transposed),
        names(json[[1L]])
    )
}
# }}}

# dict__parse_cv_version {{{
dict__parse_cv_version <- function(lst) {
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
# }}}

# dict__parse_cv_vec {{{
dict__parse_cv_vec <- function(file, subclass = NULL) {
    json <- jsonlite::read_json(file)
    res <- unlst(json[[1L]])
    data.table::setattr(
        res,
        "version",
        dict__parse_cv_version(json$version_metadata)
    )

    structure(res, class = c(subclass, "Cmip6CV", typeof(res)))
}
# }}}

# dict__parse_cv_list {{{
dict__parse_cv_list <- function(file, subclass = NULL) {
    json <- jsonlite::read_json(file)
    res <- json[[1L]]
    data.table::setattr(
        res,
        "version",
        dict__parse_cv_version(json$version_metadata)
    )

    structure(res, class = c(subclass, "Cmip6CV", "list"))
}
# }}}

# dict__parse_cv_drs {{{
dict__parse_cv_drs <- function(file) {
    dict__parse_cv_list(file, "Cmip6CV_DRS")
}
# }}}

# dict__parse_cv_activity_id {{{
dict__parse_cv_activity_id <- function(file) {
    dict__parse_cv_list(file, "Cmip6CV_ActivityId")
}
# }}}

# dict__parse_cv_experiment_id {{{
dict__parse_cv_experiment_id <- function(file) {
    json <- jsonlite::read_json(file)
    d <- dict__nest(json[[1L]])

    data.table::setcolorder(d, "experiment_id")

    cols_lst <- c(
        "activity_id",
        "additional_allowed_model_components",
        "parent_activity_id",
        "parent_experiment_id",
        "required_model_components",
        "sub_experiment_id"
    )
    for (col in cols_lst) {
        data.table::set(d, NULL, col, lapply(d[[col]], unlist, FALSE, FALSE))
    }

    cols_flat <- c(
        "experiment_id",
        "description",
        "end_year",
        "experiment",
        "min_number_yrs_per_sim",
        "start_year",
        "tier"
    )
    for (col in cols_flat) {
        data.table::set(d, NULL, col, unlist(d[[col]], FALSE, FALSE))
    }

    cols_int <- c("end_year", "min_number_yrs_per_sim", "start_year", "tier")
    for (col in cols_int) {
        data.table::set(d, NULL, col, suppressWarnings(as.integer(d[[col]])))
    }

    data.table::setcolorder(
        d,
        c(
            "experiment_id",
            "experiment",
            "description",
            "tier",
            "start_year",
            "end_year",
            "min_number_yrs_per_sim",
            "required_model_components",
            "parent_experiment_id",
            "sub_experiment_id",
            "activity_id",
            "parent_activity_id",
            "additional_allowed_model_components"
        )
    )
    data.table::setattr(
        d,
        "version",
        dict__parse_cv_version(json$version_metadata)
    )

    structure(d, class = c("Cmip6CV_ExperimentId", "Cmip6CV", class(d)))
}
# }}}

# dict__parse_cv_frequency {{{
dict__parse_cv_frequency <- function(file) {
    dict__parse_cv_list(file, "Cmip6CV_Frequency")
}
# }}}

# dict__parse_cv_grid_label {{{
dict__parse_cv_grid_label <- function(file) {
    dict__parse_cv_list(file, "Cmip6CV_GridLabel")
}
# }}}

# dict__parse_cv_institution_id {{{
dict__parse_cv_institution_id <- function(file) {
    dict__parse_cv_list(file, "Cmip6CV_InstitutionId")
}
# }}}

# dict__parse_cv_nominal_resolution {{{
dict__parse_cv_nominal_resolution <- function(file) {
    dict__parse_cv_vec(file, "Cmip6CV_Resolution")
}
# }}}

# dict__parse_cv_realm {{{
dict__parse_cv_realm <- function(file) {
    dict__parse_cv_list(file, "Cmip6CV_Realm")
}
# }}}

# dict__parse_cv_required_global_attributes {{{
dict__parse_cv_required_global_attributes <- function(file) {
    dict__parse_cv_vec(file, "Cmip6CV_ReqGlobAttr")
}
# }}}

# dict__parse_cv_source_id {{{
dict__parse_cv_source_id <- function(file) {
    json <- jsonlite::read_json(file)
    d <- dict__nest(json[[1L]])

    data.table::setcolorder(d, "source_id")

    cols_lst <- c("activity_participation", "institution_id")
    for (col in cols_lst) {
        data.table::set(d, NULL, col, lapply(d[[col]], unlst))
    }

    cols_flat <- c(
        "source_id",
        "release_year",
        "cohort",
        "label",
        "label_extended"
    )
    for (col in cols_flat) {
        data.table::set(d, NULL, col, unlist(d[[col]], FALSE, FALSE))
    }

    data.table::set(
        d,
        NULL,
        "release_year",
        suppressWarnings(as.integer(d$release_year))
    )

    data.table::setcolorder(
        d,
        c(
            "source_id",
            "release_year",
            "institution_id",
            "label",
            "label_extended",
            "cohort",
            "activity_participation",
            "model_component",
            "license_info"
        )
    )
    data.table::setattr(
        d,
        "version",
        dict__parse_cv_version(json$version_metadata)
    )

    structure(d, class = c("Cmip6CV_SourceId", "Cmip6CV", class(d)))
}
# }}}

# dict__parse_cv_source_type {{{
dict__parse_cv_source_type <- function(file) {
    dict__parse_cv_list(file, "Cmip6CV_SourceType")
}
# }}}

# dict__parse_cv_sub_experiment_id {{{
dict__parse_cv_sub_experiment_id <- function(file) {
    dict__parse_cv_list(file, "Cmip6CV_SubExperimentId")
}
# }}}

# dict__parse_cv_table_id {{{
dict__parse_cv_table_id <- function(file) {
    dict__parse_cv_vec(file, "Cmip6CV_TableId")
}
# }}}

# dict__parse_dreq_header {{{
dict__parse_dreq_header <- function(lst) {
    res <- list()

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
# }}}

# dict__parse_dreq {{{
dict__parse_dreq <- function(file) {
    json <- jsonlite::read_json(file)
    header <- dict__parse_dreq_header(json[["Header"]])

    d <- dict__nest(json[["variable_entry"]])
    data.table::set(d, NULL, "variable", names(json[["variable_entry"]]))
    data.table::setcolorder(d, "variable")

    empty_to_na <- function(x) {
        x[x == ""] <- NA_character_
        x
    }
    for (col in names(d)) {
        data.table::set(
            d,
            NULL,
            col,
            empty_to_na(unlist(d[[col]], FALSE, FALSE))
        )
    }

    data.table::setattr(d, "metadata", header)

    d
}
# }}}

# vim: fdm=marker :
