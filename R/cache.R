DiskCache <- R6::R6Class(
    "DiskCache",
    public = list(
        initialize = function(
            dir,
            max_size = Inf,
            max_age = Inf,
            max_n = Inf,
            prune_rate = 20,
            prune_limit = 5,
            prune_on_init = TRUE
        ) {
            if (!is.character(dir) || length(dir) != 1L || is.na(dir) || nchar(dir) == 0L) {
                stop("'dir' must be a single string.")
            }

            max_size <- private$parse_size(max_size)
            if (!is.numeric(max_size) || length(max_size) != 1L || is.na(max_size) || max_size < 0) {
                stop("'max_size' must be a non-negative number or a string like '100 MB', '10 GB'")
            }

            max_age <- private$parse_age(max_age)
            if (!is.numeric(max_age) || length(max_age) != 1L || is.na(max_age) || max_age < 0) {
                stop("'max_age' must be a non-negative number or a string like '1 hour', '10 mins', '1000 secs'")
            }

            if (!is.numeric(max_n) || length(max_n) != 1L || is.na(max_n) || max_n < 0) {
                stop("'max_n' must be a non-negative number")
            }

            if (!is.numeric(prune_rate) || length(prune_rate) != 1L || is.na(prune_rate) ||
                !is.finite(prune_rate) || prune_rate < 1 || prune_rate != floor(prune_rate)) {
                stop("'prune_rate' must be a positive integer")
            }
            prune_rate <- as.integer(prune_rate)
            prune_limit <- private$parse_age(prune_limit)
            if (!is.numeric(prune_limit) || length(prune_limit) != 1L || is.na(prune_limit) || prune_limit < 0) {
                stop("'prune_limit' must be a non-negative number or a string like '1 hour', '10 mins', '1000 secs'")
            }
            if (!is.logical(prune_on_init) || length(prune_on_init) != 1L || is.na(prune_on_init)) {
                stop("'prune_on_init' must be TRUE or FALSE")
            }

            private$dir <- normalizePath(dir, mustWork = FALSE, winslash = "/")
            if (!dir.exists(private$dir)) {
                success <- dir.create(private$dir, recursive = TRUE)
                if (!success) {
                    stop(sprintf("Failed to create cache directory '%s'.", private$dir))
                }
            }

            private$max_size <- max_size
            private$max_age <- max_age
            private$max_n <- max_n
            private$prune_rate <- prune_rate
            private$prune_limit <- prune_limit

            private$set_count <- sample.int(private$prune_rate, 1L) - 1L
            private$last_prune_time <- Sys.time()

            private$check_config(prune_on_init)

            invisible(self)
        },
        get = function(key) {
            self$is_destroyed(throw = TRUE)

            private$validate_key(key)
            path <- private$get_cache_path(key)

            if (!file.exists(path)) {
                return(private$missing)
            }

            if (!is.infinite(private$max_age) && file.exists(path)) {
                info <- file.info(path, extra_cols = FALSE)
                if (!is.na(info$mtime)) {
                    age <- difftime(Sys.time(), info$mtime, units = "secs")
                    if (age > private$max_age) {
                        unlink(path)
                        return(private$missing)
                    }
                }
            }

            value <- tryCatch(
                readRDS(path),
                error = function(e) private$missing
            )

            if (is.key_missing(value)) {
                return(value)
            }

            tryCatch(Sys.setFileTime(path, Sys.time()), error = function(e) NULL)
            value
        },
        set = function(key, value) {
            self$is_destroyed(throw = TRUE)

            private$validate_key(key)
            path <- private$get_cache_path(key)

            private$atomic_write(value, path)

            private$set_count <- private$set_count + 1L
            should_prune <- private$set_count >= private$prune_rate ||
                difftime(Sys.time(), private$last_prune_time, units = "secs") >= private$prune_limit

            if (should_prune) {
                self$prune()
                private$set_count <- 0L
            }

            invisible(self)
        },
        exists = function(key) {
            self$is_destroyed(throw = TRUE)

            private$validate_key(key)
            file.exists(private$get_cache_path(key))
        },
        keys = function() {
            self$is_destroyed(throw = TRUE)

            tools::file_path_sans_ext(basename(private$list_files()))
        },
        remove = function(key) {
            self$is_destroyed(throw = TRUE)

            private$validate_key(key)
            path <- private$get_cache_path(key)
            if (file.exists(path)) {
                success <- unlink(path)
                invisible(success == 0)
            } else {
                invisible(FALSE)
            }
        },
        reset = function() {
            self$is_destroyed(throw = TRUE)

            files <- private$list_files()
            if (length(files) == 0L) {
                return(invisible(self))
            }
            success <- unlink(files)
            invisible(self)
        },
        size = function() {
            self$is_destroyed(throw = TRUE)

            length(self$keys())
        },
        destroy = function() {
            if (private$destroyed) {
                return(invisible(FALSE))
            }
            sentinel <- file.path(private$dir, "__destroyed__")
            success <- file.create(sentinel)
            if (!success) {
                warning(sprintf("Failed to create sentinel file '%s'.", sentinel))
            }

            files <- private$list_files()
            if (length(files) > 0L) {
                success <- unlink(files)
                if (success != 0) {
                    warning("Failed to remove some cache files.")
                }
            }

            metadata <- file.path(private$dir, private$metadata_file)
            success <- unlink(metadata)
            if (success != 0) {
                warning("Failed to remove cache metadata.")
            }

            success <- unlink(private$dir, recursive = TRUE)
            if (success != 0) {
                warning(sprintf("Failed to remove cache directory '%s'.", private$dir))
            }

            private$destroyed <- TRUE
            invisible(TRUE)
        },
        is_destroyed = function(throw = FALSE) {
            if (
                !dir.exists(private$dir) ||
                    file.exists(file.path(private$dir, "__destroyed__"))
            ) {
                private$destroyed <- TRUE
            }
            if (private$destroyed && throw) {
                stop(sprintf("Cache '%s' has been destroyed.", private$dir))
            }
            private$destroyed
        },
        prune = function() {
            self$is_destroyed(throw = TRUE)

            info <- private$get_file_info()
            if (nrow(info) == 0L) {
                private$last_prune_time <- Sys.time()
                return(invisible(self))
            }

            info_is_sorted <- FALSE

            ensure_sorted <- function() {
                if (info_is_sorted) {
                    return()
                }

                info <<- info[order(info$mtime, decreasing = TRUE), , drop = FALSE]
                info_is_sorted <<- TRUE
            }

            if (!is.infinite(private$max_age)) {
                now <- Sys.time()
                old <- info$path[
                    difftime(now, info$mtime, units = "secs") > private$max_age
                ]

                if (length(old) > 0) {
                    success <- unlink(old)
                    if (success != 0) {
                        warning("Failed to remove some old cache files (max_age).")
                    }
                    info <- info[!info$path %in% old, , drop = FALSE]
                }
            }

            if (!is.infinite(private$max_n) && nrow(info) > private$max_n) {
                ensure_sorted()
                rem <- info$path[seq_len(nrow(info)) > private$max_n]
                success <- unlink(rem)
                if (success != 0) {
                    warning("Failed to remove some cache files (max_n).")
                }
                info <- info[!info$path %in% rem, , drop = FALSE]
            }

            if (!is.infinite(private$max_size)) {
                total <- sum(info$size)
                if (total > private$max_size) {
                    ensure_sorted()
                    cumsize <- cumsum(info$size)
                    rem <- info$path[cumsize > private$max_size]
                    if (length(rem) > 0) {
                        success <- unlink(rem)
                        if (success != 0) {
                            warning("Failed to remove some cache files (max_size).")
                        }
                        info <- info[!info$path %in% rem, , drop = FALSE]
                    }
                }
            }

            private$last_prune_time <- Sys.time()
            invisible(self)
        },
        info = function() {
            list(
                dir = private$dir,
                max_size = private$max_size,
                max_age = private$max_age,
                max_n = private$max_n,
                prune_rate = private$prune_rate,
                prune_limit = private$prune_limit,
                n = self$size(),
                size = private$get_cache_size()
            )
        }
    ),
    private = list(
        dir = NULL,
        max_size = NULL,
        max_age = NULL,
        max_n = NULL,
        last_prune_time = NULL,
        prune_rate = NULL,
        prune_limit = NULL,
        set_count = 0L,
        destroyed = FALSE,
        metadata_file = ".metadata.rds",
        missing = structure(list(), class = "key_missing"),
        list_files = function() {
            files <- list.files(private$dir, pattern = "\\.rds$", full.names = TRUE)
            files <- files[basename(files) != private$metadata_file]
            files
        },

        validate_key = function(key) {
            if (!is.character(key) || length(key) != 1L) {
                stop("Key must be a single string.")
            }
            if (nchar(key) == 0L) {
                stop("Key must not be empty.")
            }
            if (nchar(key) > 80L) {
                stop("Key must be shorter than 80 characters.")
            }
            if (grepl("[<>:\"/\\\\|?*]", key)) {
                stop("Key must not contain any of the following characters: <>:\"/\\|?*")
            }
        },

        atomic_write = function(value, path) {
            temp_file <- paste0(path, "-temp-", format(Sys.time(), "%Y%m%d%H%M%OS6"))
            on.exit(unlink(temp_file), add = TRUE)
            tryCatch(
                {
                    saveRDS(value, temp_file)
                    success <- file.rename(temp_file, path)
                    if (!success) {
                        stop("Failed to rename temporary file.")
                    }
                },
                error = function(e) {
                    stop(sprintf("Failed to write to cache: %s", e$message))
                }
            )
        },

        get_file_info = function() {
            files <- private$list_files()

            if (length(files) == 0L) {
                return(data.frame(
                    path = character(),
                    size = numeric(),
                    mtime = structure(numeric(), class = c("POSIXct", "POSIXt")),
                    stringsAsFactors = FALSE,
                    row.names = NULL
                ))
            }

            info <- file.info(files, extra_cols = FALSE)
            info$path <- rownames(info)
            rownames(info) <- NULL

            info[!info$isdir & !is.na(info$size), , drop = FALSE]
        },

        get_cache_size = function() {
            files <- private$list_files()
            if (length(files) == 0L) {
                return(0L)
            }
            sum(file.info(files)$size, na.rm = TRUE)
        },

        get_cache_path = function(key) {
            file.path(private$dir, paste0(key, ".rds"))
        },

        check_config = function(prune = TRUE) {
            metadata <- file.path(private$dir, private$metadata_file)

            old <- tryCatch(
                {
                    if (file.exists(metadata)) {
                        readRDS(metadata)
                    } else {
                        NULL
                    }
                },
                error = function(e) {
                    warning("Failed to load cache metadata. ", conditionMessage(e))
                    NULL
                }
            )

            current <- list(
                max_size = private$max_size,
                max_age = private$max_age,
                max_n = private$max_n,
                prune_rate = private$prune_rate,
                prune_limit = private$prune_limit
            )

            to_write <- FALSE
            if (is.null(old)) {
                to_write <- TRUE
            } else if (!is.null(old) && !identical(old, current)) {
                message("Cache configuration has changed:")
                for (field in names(current)) {
                    old_val <- old[[field]]
                    new_val <- current[[field]]

                    if (!identical(old_val, new_val)) {
                        old_str <- if (is.infinite(old_val)) "Inf" else as.character(old_val)
                        new_str <- if (is.infinite(new_val)) "Inf" else as.character(new_val)
                        message(sprintf("  - %s: %s -> %s", field, old_str, new_str))
                    }
                }

                if (prune) {
                    message("Triggering prune...")
                    self$prune()
                } else {
                    message("Call `$prune()` to apply.")
                }
                to_write <- TRUE
            }

            if (to_write) {
                tryCatch(
                    saveRDS(current, metadata),
                    error = function(e) {
                        warning("Failed to save cache metadata. ", conditionMessage(e))
                    }
                )
            }

            invisible(self)
        },

        parse_size = function(size) {
            if (is.numeric(size)) {
                return(size)
            }

            if (!is.character(size) || length(size) != 1L || is.na(size)) {
                stop("'max_size' must be a number or a string like '100 MB', '10 GB'")
            }

            # string format: "number unit"
            # regex: optional whitespace, number (integer or decimal), optional whitespace, unit, optional 's'
            re <- "^\\s*([0-9.]+)\\s*([kKmMgGtTpP]?[bB])\\s*$"
            matches <- regmatches(size, regexec(re, size, perl = TRUE))[[1]]

            if (length(matches) != 3L) {
                stop(sprintf(
                    paste(
                        "When 'max_size' is a string, it should be in format 'X (B|kB|MB|GB|TB|PB)'.",
                        "Invalid input found: '%s'"
                    ),
                    size
                ))
            }

            num <- as.numeric(matches[2L])
            unit <- tolower(matches[3L])

            if (is.na(num)) {
                stop(sprintf("Invalid number in 'max_size': '%s'", matches[2]))
            }

            # convert to bytes
            multiplier <- switch(
                unit,
                "b" = 1,
                "kb" = 1024,
                "mb" = 1024^2,
                "gb" = 1024^3,
                "tb" = 1024^4,
                "pb" = 1024^5,
                stop(sprintf("Unknown size unit: '%s'", unit))
            )

            num * multiplier
        },

        parse_age = function(age) {
            if (is.numeric(age)) {
                return(age)
            }

            if (!is.character(age) || length(age) != 1L || is.na(age)) {
                stop("'max_age' must be a number or a string like '1 hour', '10 mins', '1000 secs'")
            }

            # string format: "number unit"
            # regex: optional whitespace, number (integer or decimal), optional whitespace, unit, optional 's'
            re <- "^\\s*([0-9.]+)\\s*(sec|min|hour|day)s?\\s*$"
            matches <- regmatches(age, regexec(re, age, perl = TRUE))[[1]]

            if (length(matches) != 3L) {
                stop(sprintf(
                    paste(
                        "When 'max_age' is a string, it should be in format 'X (sec|min|hour|day)s'.",
                        "Invalid input found: '%s'"
                    ),
                    age
                ))
            }

            num <- as.numeric(matches[2L])
            unit <- matches[3L]

            if (is.na(num)) {
                stop(sprintf("Invalid number in 'max_age': '%s'", matches[2L]))
            }

            # convert to seconds using difftime
            age_difftime <- as.difftime(num, units = paste0(unit, "s"))
            units(age_difftime) <- "secs"
            as.double(age_difftime)
        }
    )
)

is.key_missing <- function(x) {
    inherits(x, "key_missing")
}


#' Get the current cache mode
#'
#' Reads `getOption("epwshiftr.cache", TRUE)` and returns a string:
#' - `"normal"` when `TRUE`
#' - `"off"` when `FALSE`
#' - `"offline"` when `"offline"`
#'
#' @return A single string: `"normal"`, `"off"`, or `"offline"`.
#' @noRd
normalize_cache_mode <- function(val, name = "epwshiftr.cache") {
    if (isTRUE(val)) {
        "normal"
    } else if (isFALSE(val)) {
        "off"
    } else if (identical(val, "offline")) {
        "offline"
    } else {
        warning(sprintf(
            "Unknown value for %s: %s. Treating as TRUE.",
            name,
            deparse(val)
        ))
        "normal"
    }
}

cache_mode <- function() {
    normalize_cache_mode(getOption("epwshiftr.cache", TRUE))
}

#' Check if caching is enabled
#'
#' @return `TRUE` if cache mode is not `"off"`.
#' @noRd
is_cache_enabled <- function() {
    cache_mode() != "off"
}

#' Check if cache is in offline mode
#'
#' @return `TRUE` if cache mode is `"offline"`.
#' @noRd
is_cache_offline <- function() {
    cache_mode() == "offline"
}

#' Temporarily set cache mode
#'
#' @param mode A string: `"normal"`, `"off"`, or `"offline"`.
#'   Alternatively, `TRUE` (for `"normal"`) or `FALSE` (for `"off"`).
#' @param expr An expression to evaluate with the temporary cache mode.
#'
#' @return The result of evaluating `expr`.
#' @noRd
with_cache_mode <- function(mode, expr) {
    if (isTRUE(mode)) {
        opt_val <- TRUE
    } else if (isFALSE(mode)) {
        opt_val <- FALSE
    } else if (is.character(mode) && length(mode) == 1L) {
        opt_val <- switch(
            mode,
            "normal" = TRUE,
            "off" = FALSE,
            "offline" = "offline",
            stop(sprintf("Unknown cache mode: '%s'. Use 'normal', 'off', or 'offline'.", mode))
        )
    } else {
        stop("Unknown cache mode. Use TRUE, FALSE, 'normal', 'off', or 'offline'.", call. = FALSE)
    }
    old <- options(epwshiftr.cache = opt_val)
    on.exit(options(old), add = TRUE)
    force(expr)
}

#' Generate a deterministic cache key
#'
#' Combines a prefix with a hash of the provided arguments.
#'
#' @param prefix A string prefix for the cache key (e.g., `"gh"`, `"esgf"`).
#' @param ... Arguments to hash. Passed to `fast_hash`.
#'
#' @return A string in the format `"prefix-xxxxxxxx"` where `xxxxxxxx` is an
#'   8-character hex hash.
#' @noRd
make_cache_key <- function(prefix, ...) {
    paste0(prefix, "-", fast_hash(list(...)))
}

# get_response_cache_key {{{
get_response_cache_key <- function(url) {
    paste0("response-", fast_hash(url))
}
# }}}

# read_json_response {{{
read_json_response <- function(url, strict = TRUE, cache = getOption("epwshiftr.cache", TRUE), ...) {
    mode <- normalize_cache_mode(cache, name = "`cache`")

    if (mode != "off") {
        disk_cache <- get_cache()
        key <- get_response_cache_key(url)

        cached <- disk_cache$get(key)
        if (!is.key_missing(cached)) {
            return(cached)
        }

        if (mode == "offline") {
            stop("Cache miss in offline mode for URL '", url, "'. Cannot fetch data while offline.", call. = FALSE)
        }
    }

    res <- tryCatch(jsonlite::fromJSON(url, bigint_as_char = TRUE, ...), warning = function(w) w, error = function(e) e)
    timestamp <- Sys.time()

    if (inherits(res, "warning") || inherits(res, "error")) {
        msg <- paste0(
            "Failed to read the JSON response. Details: \n",
            conditionMessage(res)
        )
        if (isTRUE(strict)) {
            stop(msg, call. = FALSE)
        }
        warning(msg, call. = FALSE)
        return(NULL)
    } else if (!is.null(res$response$numFound) && res$response$numFound == 0L) {
        verbose(warning(
            "No matched data. ",
            "Please examine your query and the actual response."
        ))
    }

    res$timestamp <- timestamp

    # cache successful results only
    if (mode != "off" && !is.null(res)) {
        # record the cache key
        res$cache <- key
        disk_cache$set(key, res)
    }

    res
}
# }}}

#' Cache wrapper for URL-based operations
#'
#' Wraps a function that fetches data from a URL, caching the result.
#' Behavior depends on the current cache mode:
#' - `"off"`: calls `fn()` directly, no caching
#' - `"normal"`: checks cache first; on miss, calls `fn()` and caches result
#' - `"offline"`: returns cached value; on miss, stops with error
#'
#' @param key_prefix A string prefix for the cache key.
#' @param key_data Data to include in the cache key hash.
#' @param fn A function (no arguments) that fetches the data.
#' @param validate Optional function taking the result and returning TRUE if
#'   it should be cached. If NULL (default), all results are cached.
#'
#' @return The result of `fn()` (possibly from cache).
#' @noRd
with_url_cache <- function(key_prefix, key_data, fn, validate = NULL) {
    mode <- cache_mode()

    # Off mode: bypass cache entirely
    if (mode == "off") {
        return(fn())
    }

    # Normal or offline: check cache
    cache <- get_cache()
    key <- make_cache_key(key_prefix, key_data)
    value <- cache$get(key)

    if (!is.key_missing(value)) {
        return(value)
    }

    # Cache miss in offline mode: error
    if (mode == "offline") {
        stop(
            "Cache miss in offline mode for key '",
            key,
            "'. ",
            "Cannot fetch data while offline.",
            call. = FALSE
        )
    }

    # Normal mode: fetch, conditionally cache, return
    value <- fn()
    if (is.null(validate) || isTRUE(validate(value))) {
        cache$set(key, value)
    }
    value
}

#' Cache wrapper for file downloads
#'
#' Wraps a function that downloads a file, caching the raw bytes.
#' Behavior depends on the current cache mode:
#' - `"off"`: calls `fn()` directly, no caching
#' - `"normal"`: checks cache first; on hit, writes cached bytes to `destfile`;
#'   on miss, calls `fn()`, reads the downloaded file, and caches the bytes
#' - `"offline"`: returns cached value; on miss, stops with error
#'
#' @param url The URL being downloaded (used for cache key).
#' @param destfile The destination file path.
#' @param fn A function (no arguments) that performs the actual download.
#'   It should write to `destfile` and return `destfile`.
#'
#' @return The `destfile` path.
#' @noRd
with_download_cache <- function(url, destfile, fn) {
    mode <- cache_mode()

    # Off mode: bypass cache entirely
    if (mode == "off") {
        return(fn())
    }

    # Normal or offline: check cache
    cache <- get_cache()
    key <- make_cache_key("dl", url)
    value <- cache$get(key)

    if (!is.key_missing(value)) {
        # Cache hit: write cached bytes to destfile
        writeBin(value, destfile)
        return(destfile)
    }

    # Cache miss in offline mode: error
    if (mode == "offline") {
        stop(
            "Cache miss in offline mode for download '",
            url,
            "'. ",
            "Cannot download files while offline.",
            call. = FALSE
        )
    }

    # Normal mode: download, cache raw bytes, return
    fn()
    raw_bytes <- readBin(destfile, "raw", file.info(destfile)$size)
    cache$set(key, raw_bytes)
    destfile
}
