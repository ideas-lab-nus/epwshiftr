cmip6dict__raw_cache_dir <- function(dir = getOption("epwshiftr.dir", ".")) {
    file.path(dir, "cmip6-dict", "raw")
}

cmip6dict__volatile_raw_cache_dir <- function() {
    file.path(
        tempdir(),
        paste0(
            "cmip6-dict-raw-",
            fast_hash(list(Sys.getpid(), Sys.time(), sample.int(.Machine$integer.max, 1L)))
        )
    )
}

cmip6dict__parsed_cache_key <- function(cv_tag_info, dreq_tag_info) {
    make_cache_key(
        "esgdict",
        project = "CMIP6",
        profile = "cmip6",
        cv_tag = cv_tag_info$tag,
        cv_commit = cv_tag_info$commit,
        dreq_tag = dreq_tag_info$tag,
        dreq_commit = dreq_tag_info$commit,
        format_version = ESGDICT_FORMAT_VERSION
    )
}

cmip6dict__cached_fetch_value <- function(fetched) {
    list(
        cvs = fetched$cvs,
        dreq = fetched$dreq,
        sources = fetched$sources
    )
}

cmip6dict__with_built_time <- function(fetched) {
    fetched$built_time <- Sys.time()
    fetched
}

cmip6dict__fetch_cached <- function(
    token = NULL,
    cv_tag = NULL,
    dreq_tag = NULL,
    policy,
    cache_dir = cmip6dict__raw_cache_dir(),
    force = FALSE
) {
    checkmate::assert_string(cv_tag, null.ok = TRUE)
    checkmate::assert_string(dreq_tag, null.ok = TRUE)
    checkmate::assert_flag(force)

    cv_tag_info <- cmip6dict__resolve_tag_cached(REPO_CV, cv_tag, token, policy)
    dreq_tag_info <- cmip6dict__resolve_tag_cached(REPO_DREQ, dreq_tag, token, policy)
    cache_key <- cmip6dict__parsed_cache_key(cv_tag_info, dreq_tag_info)

    # Parsed CV/DReq payloads are cached separately from raw JSON files. A forced
    # build skips this read path, but still writes a fresh parsed value later.
    if (isTRUE(policy$read) && !isTRUE(force)) {
        cached <- get_cache()$get(cache_key)
        if (!is.key_missing(cached)) {
            return(cmip6dict__with_built_time(cached))
        }
    }

    raw_cache_dir <- if (isTRUE(policy$raw_read) || isTRUE(policy$raw_write)) {
        cache_dir
    } else {
        cmip6dict__volatile_raw_cache_dir()
    }

    fetched <- cmip6dict__fetch_resolved(
        cv_tag_info = cv_tag_info,
        dreq_tag_info = dreq_tag_info,
        token = token,
        policy = policy,
        cache_dir = raw_cache_dir
    )

    if (isTRUE(policy$write)) {
        get_cache()$set(cache_key, cmip6dict__cached_fetch_value(fetched))
    }

    fetched
}

cmip6dict__fetch <- function(
    token = NULL,
    cv_tag = NULL,
    dreq_tag = NULL,
    use_cache = TRUE,
    cache_dir = cmip6dict__raw_cache_dir()
) {
    checkmate::assert_flag(use_cache)

    cv_tag_info <- cmip6dict__resolve_tag(REPO_CV, cv_tag, token)
    dreq_tag_info <- cmip6dict__resolve_tag(REPO_DREQ, dreq_tag, token)
    raw_cache_dir <- if (isTRUE(use_cache)) cache_dir else cmip6dict__volatile_raw_cache_dir()

    cmip6dict__fetch_resolved(
        cv_tag_info = cv_tag_info,
        dreq_tag_info = dreq_tag_info,
        token = token,
        policy = list(
            raw_read = isTRUE(use_cache),
            raw_write = isTRUE(use_cache),
            offline = FALSE
        ),
        cache_dir = raw_cache_dir
    )
}

cmip6dict__fetch_resolved <- function(
    cv_tag_info,
    dreq_tag_info,
    token = NULL,
    policy,
    cache_dir = cmip6dict__raw_cache_dir()
) {
    cli::cli_progress_step(
        "Fetching {.strong CMIP6 Dictionary}...",
        "Fetched {.strong CMIP6 Dictionary} successfully at {Sys.time()}",
        "Failed to fetch {.strong CMIP6 Dictionary}.",
        spinner = TRUE
    )

    cvs <- cmip6dict__fetch_cv(
        tag = cv_tag_info$tag,
        token = token,
        use_cache = isTRUE(policy$raw_read),
        write_cache = isTRUE(policy$raw_write),
        offline = isTRUE(policy$offline),
        cache_dir = file.path(cache_dir, "cvs", cv_tag_info$tag)
    )
    dreq <- cmip6dict__fetch_dreq(
        tag = dreq_tag_info$tag,
        token = token,
        use_cache = isTRUE(policy$raw_read),
        write_cache = isTRUE(policy$raw_write),
        offline = isTRUE(policy$offline),
        cache_dir = file.path(cache_dir, "dreq", dreq_tag_info$tag)
    )

    list(
        cvs = cvs,
        dreq = dreq,
        built_time = Sys.time(),
        sources = list(
            cvs = cmip6dict__source_info(REPO_CV, cv_tag_info, file.path(cache_dir, "cvs", cv_tag_info$tag)),
            dreq = cmip6dict__source_info(REPO_DREQ, dreq_tag_info, file.path(cache_dir, "dreq", dreq_tag_info$tag))
        )
    )
}

cmip6dict__resolve_tag_cached <- function(repo, tag = NULL, token = NULL, policy) {
    if (!is.null(tag)) {
        return(cmip6dict__resolve_tag(repo, tag, token))
    }

    if (!isTRUE(policy$read)) {
        return(cmip6dict__resolve_tag(repo, tag, token))
    }

    tryCatch(
        with_url_cache(
            "esgdict-tag",
            list(repo = repo),
            fn = function() cmip6dict__resolve_tag(repo, tag, token),
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
                            "Cannot resolve the latest CMIP6 tag for `%s` in offline cache mode.",
                            "Supply explicit `cv_tag`/`dreq_tag`, or build once with `epwshiftr.cache = TRUE`."
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

cmip6dict__resolve_tag <- function(repo, tag = NULL, token = NULL) {
    if (!is.null(tag)) {
        return(list(tag = tag, commit = NA_character_))
    }

    tag_row <- gh_tags(repo, token)[1L, ]
    list(
        tag = cmip6dict__tag_value(tag_row, "name"),
        commit = cmip6dict__tag_commit(tag_row)
    )
}

cmip6dict__tag_value <- function(tag_row, name) {
    value <- tag_row[[name]]
    if (is.list(value)) value <- value[[1L]]
    as.character(value[[1L]])
}

cmip6dict__tag_commit <- function(tag_row) {
    if ("commit.sha" %in% names(tag_row)) {
        return(cmip6dict__tag_value(tag_row, "commit.sha"))
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

cmip6dict__source_info <- function(repo, tag_info, cache_dir) {
    list(
        repo = repo,
        tag = tag_info$tag,
        commit = tag_info$commit,
        cache_dir = normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
    )
}

cmip6dict__cache_ready <- function(files) {
    length(files) && all(file.exists(files))
}

cmip6dict__raw_cache_miss_error <- function(kind, dir) {
    stop(
        sprintf(
            paste(
                "%s raw cache miss in offline cache mode at `%s`.",
                "Build the dictionary once online, or pass a `cache_dir` containing the required raw files."
            ),
            kind,
            normalizePath(dir, winslash = "/", mustWork = FALSE)
        ),
        call. = FALSE
    )
}

cmip6dict__fetch_cv <- function(
    tag,
    token = NULL,
    use_cache = TRUE,
    cache_dir = tempdir(),
    write_cache = use_cache,
    offline = FALSE
) {
    checkmate::assert_string(tag, min.chars = 1L)

    files <- cmip6dict__download_cv_file(
        tag,
        dir = cache_dir,
        token = token,
        use_cache = use_cache,
        write_cache = write_cache,
        offline = offline
    )

    cvs <- list()
    for (type in names(files)) {
        abbr <- tolower(tools::file_path_sans_ext(type))
        cvs[[abbr]] <- match.fun(sprintf("cmip6dict__parse_cv_%s", abbr))(files[[type]])
    }

    cvs
}

cmip6dict__download_cv_file <- function(
    tag,
    dir = tempdir(),
    token = NULL,
    use_cache = TRUE,
    write_cache = use_cache,
    offline = FALSE
) {
    dests <- file.path(dir, sprintf("CMIP6_%s.json", CV_TYPES))
    names(dests) <- CV_TYPES
    if (use_cache && cmip6dict__cache_ready(dests)) {
        return(dests)
    }
    if (isTRUE(offline)) {
        cmip6dict__raw_cache_miss_error("CMIP6 CV", dir)
    }

    # Raw CV files stay as normal JSON files so users can inspect exactly what
    # came from the upstream CV repository when persistent caching is enabled.
    if (!isTRUE(write_cache)) {
        dir <- file.path(cmip6dict__volatile_raw_cache_dir(), "cvs", tag)
        dests <- file.path(dir, sprintf("CMIP6_%s.json", CV_TYPES))
        names(dests) <- CV_TYPES
    }

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

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
        dests[[type]] <- download_gh_file(REPO_CV, tag, file, dir, token)
    }

    dests
}

cmip6dict__fetch_dreq <- function(
    tag,
    token = NULL,
    use_cache = TRUE,
    cache_dir = tempdir(),
    write_cache = use_cache,
    offline = FALSE
) {
    checkmate::assert_string(tag, min.chars = 1L)

    files <- cmip6dict__download_dreq_file(
        tag,
        dir = cache_dir,
        token = token,
        use_cache = use_cache,
        write_cache = write_cache,
        offline = offline
    )

    dreq <- lapply(files, cmip6dict__parse_dreq_file)
    metadata <- lapply(dreq, attr, "metadata", TRUE)

    for (nm in names(dreq)) {
        data.table::set(dreq[[nm]], NULL, "table_id", metadata[[nm]][["table_id"]])
    }

    dreq <- data.table::rbindlist(dreq, use.names = TRUE)
    data.table::setcolorder(dreq, c("variable", "table_id", "modeling_realm", "standard_name", "long_name"))
    structure(dreq,
        metadata = data.table::rbindlist(metadata, use.names = TRUE),
        class = c("Cmip6DReq", class(dreq))
    )
}

cmip6dict__download_dreq_file <- function(
    tag,
    dir = tempdir(),
    token = NULL,
    use_cache = TRUE,
    write_cache = use_cache,
    offline = FALSE
) {
    files <- list.files(dir, pattern = "^CMIP6_.*[.]json$", full.names = TRUE)
    if (use_cache && length(files)) {
        names(files) <- basename(files)
        return(files)
    }
    if (isTRUE(offline)) {
        cmip6dict__raw_cache_miss_error("CMIP6 DReq", dir)
    }

    # DReq is downloaded as an archive, but we cache only the extracted table
    # JSON files. That keeps the raw cache stable and easy to diff.
    if (!isTRUE(write_cache)) {
        dir <- file.path(cmip6dict__volatile_raw_cache_dir(), "dreq", tag)
    }

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

    cli::cli_progress_step(
        "Downloading data of {.strong CMIP6 DReq}...",
        "Downloaded data of {.strong CMIP6 DReq} successfully.",
        "Failed to download data of {.strong CMIP6 DReq}.",
        spinner = TRUE
    )
    zipball <- download_gh_tag(REPO_DREQ, tag, tempdir(), token)
    extracted <- utils::unzip(zipball, exdir = tempdir())

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
