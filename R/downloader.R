# verify_checksum {{{
verify_checksum <- function(file, checksum, algo = "sha256") {
    checkmate::assert_file_exists(file)
    checkmate::assert_string(checksum)
    checkmate::assert_choice(algo, c("md5", "sha256"))

    actual <- checksum_file(file, algo)

    tolower(actual) == tolower(checksum)
}
# }}}

# format_bytes {{{
format_bytes <- function(bytes) {
    if (is.na(bytes) || bytes < 0) return("? B")

    units <- c("B", "KB", "MB", "GB", "TB")
    unit_idx <- 1L
    size <- as.numeric(bytes)

    while (size >= 1024 && unit_idx < length(units)) {
        size <- size / 1024
        unit_idx <- unit_idx + 1L
    }

    if (unit_idx == 1L) {
        # Bytes
        sprintf("%d %s", as.integer(size), units[unit_idx])
    } else {
        sprintf("%.2f %s", size, units[unit_idx])
    }
}
# }}}

# FileStatus {{{
#' File Download Status
#'
#' Internal enumeration for file download states
#'
#' @keywords internal
#' @noRd
FileStatus <- list(
    Missing = 0L,        # File does not exist
    Downloading = 1L,    # .part file exists (download in progress or interrupted)
    Downloaded = 2L,     # .done file exists (download complete, not verified)
    Verified = 3L        # Final file exists and verified
)
# }}}

# DownloadStatus {{{
#' File Download Status
#'
#' Internal enumeration for file download states
#'
#' @keywords internal
#' @noRd
DownloadStatus <- list(
    Pending = 0L,        # Download task is pending
    Downloading = 1L,    # Download is in progress
    Completed = 2L,      # Download is completed
    Failed = 3L          # Download failed
)
# }}}

# DownloadTask {{{
#' Download Task Tracker
#'
#' Internal class for tracking async download tasks
#'
#' @keywords internal
#' @noRd
DownloadTask <- R6::R6Class("DownloadTask",
    lock_class = TRUE,
    public = list(
        url = NULL,
        filename = NULL,
        status = DownloadStatus$Pending,
        progress = 0L,
        total = 0L,
        error = NULL,
        mirai_obj = NULL,
        started_at = NULL,
        completed_at = NULL,

        initialize = function(url, filename) {
            self$url <- url
            self$filename <- filename
            self$started_at <- Sys.time()
        },

        update_progress = function(current, total = NULL) {
            self$progress <- current
            if (!is.null(total)) self$total <- total
        },

        mark_completed = function() {
            self$status <- DownloadStatus$Completed
            self$completed_at <- Sys.time()
        },

        mark_failed = function(error) {
            self$status <- DownloadStatus$Failed
            self$error <- error
            self$completed_at <- Sys.time()
        }
    )
)
# }}}

# downloader utilities {{{
DOWNLOADER_SCHEMA_VERSION <- "1.0.0"
DOWNLOADER_TASK_STATUS <- c("queued", "downloading", "done", "error", "cancelled", "skipped")
DOWNLOADER_SESSION_STARTED_AT <- Sys.time()

download_now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}

download_hash <- function(...) {
    values <- vapply(list(...), function(x) {
        if (is.null(x) || length(x) == 0L || all(is.na(x))) {
            return("")
        }
        paste(as.character(x), collapse = "\r")
    }, character(1L))
    checksum_bytes(charToRaw(paste(values, collapse = "\n")), "sha256")
}

download_one_chr <- function(x) {
    if (is.null(x) || length(x) == 0L) {
        return(NA_character_)
    }
    out <- as.character(x[[1L]])
    if (is.na(out) || !nzchar(out)) NA_character_ else out
}

download_absolute_path <- function(path) {
    grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

download_resolve_path <- function(path, base = getwd()) {
    if (is.null(path) || is.na(path) || !nzchar(path)) {
        return(NULL)
    }
    path <- path.expand(path)
    if (!download_absolute_path(path)) {
        path <- file.path(base, path)
    }
    normalizePath(path, mustWork = FALSE, winslash = "/")
}

download_checksum_type <- function(type) {
    type <- tolower(download_one_chr(type))
    if (is.na(type) || !type %in% c("md5", "sha256")) "sha256" else type
}

download_config_validate <- function(config, name = "downloader config") {
    schema_validate(SCHEMA_DOWNLOADER_CONFIG, config, mode = "assert", name = name)
    invisible(config)
}

download_config_read <- function(file) {
    checkmate::assert_file(file, access = "r", extension = "json")
    config <- jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE)
    download_config_validate(config, file)

    base <- dirname(normalizePath(file, mustWork = TRUE, winslash = "/"))
    config$dest <- download_resolve_path(config$dest, base)
    config$temp <- download_resolve_path(config$temp, base)
    if (!is.null(config$manifest)) {
        config$manifest <- download_resolve_path(config$manifest, base)
    }
    config
}

download_config_write <- function(config, file, pretty = TRUE) {
    download_config_validate(config, file)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(config, file, null = "null", digits = 6, pretty = pretty, auto_unbox = TRUE)
    normalizePath(file, mustWork = TRUE, winslash = "/")
}
# }}}

# FileDownloader {{{
#' General Purpose File Downloader
#'
#' @description
#'
#' `FileDownloader` provides a general purpose file download system with:
#' - File status management (missing, downloading, downloaded, verified)
#' - Incremental checksum verification during download
#' - Resume capability for interrupted downloads
#' - Async and parallel download using mirai
#' - Progress tracking
#' - Error handling and retry logic
#'
#' @author Hongyuan Jia
#' @name FileDownloader
#' @export
FileDownloader <- R6::R6Class("FileDownloader",
    lock_class = TRUE,
    public = list(
        # initialize {{{
        #' @description
        #' Create a new FileDownloader object
        #'
        #' @param dest A string specifying the directory for final
        #'        downloaded files. If `NULL`, uses a temporary directory. Default: `NULL`.
        #'
        #' @param temp A string specifying the directory for temporary files
        #'        (.part, .done). If `NULL`, uses `dest/.tmp`. Should ideally
        #'        be on the same filesystem as `dest` for atomic rename
        #'        operations. Default: `NULL`.
        #'
        #' @param retries A positive integer specifying the maximum number of
        #'        retry attempts for failed downloads. Default: `3L`.
        #'
        #' @param timeout A positive integer specifying the timeout in seconds for
        #'        each download. Default: `3600L` (1 hour).
        #'
        #' @param cleanup A logical value specifying whether to automatically clean up failed temporary
        #'        files. Default: `TRUE`.
        #'
        #' @param n_workers A non-negative integer specifying the number of parallel workers
        #'        for async downloads. If `0`, async downloads will fallback to synchronous mode.
        #'        Default: `4L`.
        #'
        #' @param manifest Optional DuckDB manifest path for persistent
        #'        sessions, tasks, candidate URLs, and events. If `NULL`, only
        #'        the single-file shortcut API is available. Default: `NULL`.
        #'
        #' @param config Optional downloader config, either a JSON file path
        #'        created by `$save_config()` or a validated config list.
        #'        Explicit constructor arguments override config values.
        #'        Default: `NULL`.
        #'
        #' @return An `FileDownloader` object.
        #'
        #' @examples
        #' \dontrun{
        #' dl <- FileDownloader$new()
        #' dl <- FileDownloader$new(dest = "~/data")
        #' dl <- FileDownloader$new(
        #'     dest = "~/data",
        #'     temp = "~/data/.tmp",
        #'     n_workers = 8
        #' )
        #' }
        initialize = function(dest = NULL, temp = NULL, retries = 3L, timeout = 3600L,
                              cleanup = TRUE, n_workers = 4L, manifest = NULL, config = NULL) {
            if (!is.null(config)) {
                cfg <- if (is.character(config)) download_config_read(config) else config
                download_config_validate(cfg)
                if (missing(dest)) dest <- cfg$dest
                if (missing(temp)) temp <- cfg$temp
                if (missing(retries)) retries <- cfg$retries
                if (missing(timeout)) timeout <- cfg$timeout
                if (missing(cleanup)) cleanup <- cfg$cleanup
                if (missing(n_workers)) n_workers <- cfg$n_workers
                if (missing(manifest)) manifest <- cfg$manifest
            }
            if (is.null(dest)) {
                dest <- tempdir()
            }
            dest <- path.expand(dest)
            if (!dir.exists(dest)) {
                dir.create(dest, recursive = TRUE, showWarnings = FALSE)
            }

            checkmate::assert_directory_exists(dest, access = "rw")
            checkmate::assert_string(temp, null.ok = TRUE)
            checkmate::assert_count(retries, positive = TRUE)
            checkmate::assert_count(timeout, positive = TRUE)
            checkmate::assert_flag(cleanup)
            checkmate::assert_count(n_workers, positive = FALSE)
            checkmate::assert_string(manifest, null.ok = TRUE)

            # Check if in development mode via environment variable or option
            in_dev <- isTRUE(as.logical(Sys.getenv("EPWSHIFTR_DEV_MODE", "FALSE"))) ||
                      isTRUE(getOption("epwshiftr.dev_mode", FALSE))

            private$dest <- normalizePath(dest, mustWork = TRUE)

            # Set up temp
            if (is.null(temp)) {
                private$temp <- file.path(private$dest, ".tmp")
            } else {
                private$temp <- normalizePath(path.expand(temp), mustWork = FALSE, winslash = "/")
            }

            # Create temp if it doesn't exist
            if (!dir.exists(private$temp)) {
                dir.create(private$temp, recursive = TRUE)
            }

            private$retries <- retries
            private$dl_timeout <- timeout
            private$cleanup <- cleanup
            private$worker_count <- n_workers
            private$in_dev <- in_dev
            private$async_tasks <- list()

            if (!is.null(manifest)) {
                private$manifest_path <- normalizePath(path.expand(manifest), mustWork = FALSE, winslash = "/")
                private$config_path <- file.path(dirname(private$manifest_path), "config.json")
                dir.create(dirname(private$manifest_path), recursive = TRUE, showWarnings = FALSE)
                private$connect_manifest()
                private$init_manifest_schema()
                if (!file.exists(private$config_path)) {
                    self$save_config(private$config_path)
                }
            }

            # Start daemon pool if needed
            if (n_workers > 0) {
                mirai::daemons(n_workers)

                # If in development mode, source this file on all daemons to avoid devtools dependency
                if (in_dev) {
                    cli::cli_alert_info(
                        "Development mode detected. Sourcing downloader.R on all daemons..."
                    )
                    # Try to locate R/downloader.R relative to current working directory
                    candidates <- c(
                        file.path(getwd(), "R", "downloader.R"),
                        file.path(getwd(), "..", "R", "downloader.R"),
                        file.path(getwd(), "../..", "R", "downloader.R")
                    )
                    src_path <- NULL
                    for (p in candidates) {
                        if (file.exists(p)) {
                            src_path <- normalizePath(p, mustWork = FALSE)
                            break
                        }
                    }
                    if (!is.null(src_path)) {
                        mirai::everywhere(source(src_path, chdir = TRUE), .compute = "default")
                    } else {
                        cli::cli_alert_warning(
                            "Could not locate R/downloader.R for dev-mode sourcing; async workers may lack FileDownloader."
                        )
                    }
                }
            }

            self
        },
        # }}}

        # download {{{
        #' @description
        #' Download a single file with state management and resume support
        #'
        #' @param url A string specifying the URL to download from.
        #'
        #' @param filename A string specifying the filename for the downloaded file. If `NULL`, uses
        #'        filename from URL. Default: `NULL`.
        #'
        #' @param subdir A string specifying the subdirectory within `dest` to save file.
        #'        Default: `NULL` (save directly in `dest`).
        #'
        #' @param progress A logical value specifying whether to show progress bar. Default: `TRUE`.
        #'
        #' @param overwrite A logical value specifying whether to overwrite existing file. Default: `FALSE`.
        #'
        #' @param checksum A string specifying the expected checksum for verification. If provided,
        #'        enables incremental checksum calculation. Default: `NULL`.
        #'
        #' @param checksum_type A string specifying the checksum type ("sha256" or "md5"). Default: `"sha256"`.
        #'
        #' @param resume A logical value specifying whether to resume interrupted downloads.
        #'        Default: `TRUE`.
        #'
        #' @param block A logical value specifying whether to block until download completes.
        #'        If `FALSE`, downloads asynchronously in background. Default: `TRUE`.
        #'
        #' @param .tmp_id Internal temporary file ID used by persistent
        #'        download tasks. Default: `NULL`.
        #'
        #' @return If `block = TRUE`, returns the path to the downloaded file.
        #'        If `block = FALSE`, returns a task ID for tracking the download.
        #'
        #' @examples
        #' \dontrun{
        #' # Blocking download
        #' path <- dl$download(url = "https://example.com/data.nc")
        #'
        #' # Async download
        #' task_id <- dl$download(
        #'     url = "https://example.com/data.nc",
        #'     block = FALSE
        #' )
        #' dl$wait_for_tasks(task_id)
        #'
        #' # Multiple files (async batch)
        #' urls <- c("https://example.com/file1.nc", "https://example.com/file2.nc")
        #' task_ids <- sapply(urls, function(url) {
        #'     dl$download(url = url, block = FALSE)
        #' })
        #' results <- dl$wait_for_tasks(task_ids)
        #' }
        download = function(url, filename = NULL, subdir = NULL,
                                progress = TRUE, overwrite = FALSE,
                                checksum = NULL, checksum_type = "sha256",
                                resume = TRUE, block = TRUE, .tmp_id = NULL) {
            checkmate::assert_string(url)
            checkmate::assert_string(filename, null.ok = TRUE)
            checkmate::assert_string(subdir, null.ok = TRUE)
            checkmate::assert_flag(progress)
            checkmate::assert_flag(overwrite)
            checkmate::assert_string(checksum, null.ok = TRUE)
            checkmate::assert_choice(checksum_type, c("md5", "sha256"))
            checkmate::assert_flag(resume)
            checkmate::assert_flag(block)
            checkmate::assert_string(.tmp_id, null.ok = TRUE)

            # handle async download
            if (!block) {
                # If n_workers = 0, fallback to sync with warning
                if (private$worker_count == 0L) {
                    cli::cli_alert_warning(
                        "Async download not available (n_workers = 0). Running synchronously."
                    )
                    # Continue with sync execution below
                } else {
                    return(private$download_async(
                        url, filename, subdir, progress, overwrite,
                        checksum, checksum_type, resume
                    ))
                }
            }

            # determine filename
            if (is.null(filename)) {
                filename <- basename(url)
                filename <- sub("\\?.*$", "", filename)  # remove query params
            }
            # TODO: handle redirect in filename

            # determine final destination
            if (is.null(subdir)) {
                dest <- file.path(private$dest, filename)
            } else {
                dest <- file.path(private$dest, subdir, filename)
            }

            # get temporary file path
            tmp_id <- if (!is.null(.tmp_id)) .tmp_id else if (!is.null(checksum)) checksum else checksum_bytes(charToRaw(url), "md5")
            tmp_part <- file.path(private$temp, paste0(tmp_id, ".part"))
            tmp_done <- file.path(private$temp, paste0(tmp_id, ".done"))

            # check current status
            status <- private$check_file_status(dest, tmp_part, tmp_done, checksum, checksum_type)

            # if file is verified and exists, return it
            if (status == FileStatus$Verified && !overwrite) {
                verbose(cli::cli_alert_info("File already verified: {dest}. Skipping..."))
                return(dest)
            }

            # In offline mode, refuse to download files that don't exist locally
            if (is_cache_offline() && status != FileStatus$Downloaded) {
                stop(
                    "Cannot download file in offline mode: ", url,
                    "\nExpected file at: ", dest,
                    call. = FALSE
                )
            }

            # if .done file exists and is valid, just move it
            if (status == FileStatus$Downloaded) {
                verbose(cli::cli_alert_info("Moving completed download to final location..."))
                private$finalize_download(tmp_done, dest)
                return(dest)
            }

            # create destination directory if needed
            dest_dir <- dirname(dest)
            if (!dir.exists(dest_dir)) {
                dir.create(dest_dir, recursive = TRUE)
            }

            # prepare for download
            start_byte <- 0L

            if (resume && status == FileStatus$Downloading) {
                # resume from .part file
                start_byte <- file.info(tmp_part)$size
                verbose(cli::cli_alert_info("Resuming download from byte {start_byte}..."))
            }

            # download with retry logic
            attempt <- 1L
            last_error <- NULL
            chksum_history <- character(0)  # track checksums for intelligent retry

            while (attempt <= private$retries) {
                result <- tryCatch(
                    {
                        private$download_with_streaming(
                            url, tmp_part, tmp_done,
                            progress, start_byte
                        )
                        TRUE
                    },
                    error = function(e) {
                        last_error <<- e
                        FALSE
                    }
                )

                if (result) {
                    # verify checksum if provided
                    if (!is.null(checksum)) {
                        verbose(cli::cli_alert_info("Verifying checksum..."))

                        is_valid <- verify_checksum(tmp_done, checksum, checksum_type)

                        if (!is_valid) {
                            # get actual checksum for debugging
                            actual <- private$get_file_checksum(tmp_done, checksum_type)
                            chksum_history <- c(chksum_history, actual)

                            # check for persistent checksum mismatch (server-side issue)
                            if (length(chksum_history) >= 2 &&
                                all(chksum_history == chksum_history[1])) {
                                unlink(tmp_part)
                                unlink(tmp_done)
                                stop(sprintf(
                                    paste0(
                                        "File consistently returns the same incorrect checksum.\n",
                                        "This indicates a server-side problem.\n",
                                        "Expected: %s\n",
                                        "Got:      %s\n",
                                        "Please contact the data provider to verify the file integrity."
                                    ),
                                    checksum, actual
                                ))
                            }

                            if (attempt < private$retries) {
                                cli::cli_alert_warning(sprintf(
                                    "Checksum verification failed (attempt %d/%d)",
                                    attempt, private$retries
                                ))
                                verbose(cli::cli_alert_info("Expected: {checksum}"))
                                verbose(cli::cli_alert_info("Got:      {actual}"))
                                cli::cli_alert_info("Retrying download...")

                                unlink(tmp_part)
                                unlink(tmp_done)
                                start_byte <- 0L
                                attempt <- attempt + 1L
                                Sys.sleep(2^(attempt - 2))
                                next
                            } else {
                                unlink(tmp_part)
                                unlink(tmp_done)
                                stop(sprintf(
                                    paste0(
                                        "Checksum verification failed after %d attempts\n",
                                        "Expected: %s\n",
                                        "Got:      %s"
                                    ), private$retries, checksum, actual))
                            }
                        }

                        verbose(cli::cli_alert_success("Checksum verified"))
                    }

                    # move to final location
                    private$finalize_download(tmp_done, dest)
                    verbose(cli::cli_alert_success("Downloaded: {.var {dest}}."))
                    return(dest)
                }

                if (attempt < private$retries) {
                    wait_time <- 2^(attempt - 1)
                    warning(sprintf(
                        "Download failed (attempt %d/%d). Waiting %ds before retry...",
                        attempt, private$retries, wait_time
                    ))
                    Sys.sleep(wait_time)
                }

                attempt <- attempt + 1L
            }

            stop(sprintf(
                "Failed to download file after %d attempts. Last error: %s",
                private$retries, conditionMessage(last_error)
            ))
        },
        # }}}

        # save_config {{{
        #' @description
        #' Save downloader construction config as schema-validated JSON.
        #'
        #' @param file Output JSON path. If `NULL`, uses the config path next to
        #'        the persistent manifest.
        #' @param pretty Whether to add indentation whitespace. Default: `TRUE`.
        #'
        #' @return The normalized output path.
        save_config = function(file = NULL, pretty = TRUE) {
            checkmate::assert_string(file, null.ok = TRUE)
            checkmate::assert_flag(pretty)
            if (is.null(file)) {
                if (is.null(private$config_path)) {
                    cli::cli_abort("`file` is required when the downloader has no persistent manifest.")
                }
                file <- private$config_path
            }
            download_config_write(private$config_payload(), file, pretty = pretty)
        },
        # }}}

        # enqueue {{{
        #' @description
        #' Add a download plan to the persistent manifest.
        #'
        #' @param plan A data frame with at least `logical_file_id`, `filename`,
        #'        and `url` columns.
        #' @param session_label Optional label for this download session.
        #'
        #' @return The created session ID.
        enqueue = function(plan, session_label = NULL) {
            private$require_manifest()
            checkmate::assert_string(session_label, null.ok = TRUE)
            plan <- private$normalize_plan(plan)
            if (!nrow(plan)) {
                cli::cli_abort("Download plan does not contain any candidate URL.")
            }

            session_id <- private$new_session_id()
            now <- download_now()
            private$append_rows("download_session", data.frame(
                session_id = session_id,
                label = download_one_chr(session_label),
                status = "queued",
                created_at = now,
                updated_at = now,
                completed_at = as.POSIXct(NA),
                stringsAsFactors = FALSE
            ))

            tasks <- private$plan_tasks(plan, session_id, now)
            candidates <- private$plan_candidates(plan, tasks, now)
            private$append_rows("download_task", tasks)
            private$append_rows("download_candidate", candidates)
            private$log_event(session_id, NA_character_, "enqueue", sprintf("Queued %d download task(s).", nrow(tasks)))
            session_id
        },
        # }}}

        # run {{{
        #' @description
        #' Run queued persistent download tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #' @param block Whether to block until completion. Persistent runs are
        #'        currently synchronous.
        #' @param progress Whether to show per-file progress.
        #' @param overwrite Whether to overwrite existing final files.
        #' @param resume Whether to resume `.part` files.
        #'
        #' @return A data.table of selected task records after the run.
        run = function(session_id = NULL, task_id = NULL, block = TRUE,
                       progress = TRUE, overwrite = FALSE, resume = TRUE) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_character(task_id, any.missing = FALSE, null.ok = TRUE)
            checkmate::assert_flag(block)
            checkmate::assert_flag(progress)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            if (!isTRUE(block)) {
                cli::cli_alert_warning("Persistent downloader runs execute synchronously in this version.")
            }

            private$cancel_stale_downloading(session_id = session_id, task_id = task_id)
            tasks <- private$select_tasks(session_id = session_id, task_id = task_id, status = c("queued", "downloading"))
            if (!nrow(tasks)) {
                return(private$decorate_task_status(private$select_tasks(session_id = session_id, task_id = task_id)))
            }
            for (i in seq_len(nrow(tasks))) {
                private$run_task(tasks[i, , drop = FALSE], progress = progress, overwrite = overwrite, resume = resume)
            }
            private$decorate_task_status(private$select_tasks(session_id = session_id, task_id = task_id))
        },
        # }}}

        # sessions {{{
        #' @description
        #' List persistent download sessions.
        sessions = function() {
            private$require_manifest()
            private$read_table("download_session")
        },
        # }}}

        # tasks {{{
        #' @description
        #' List persistent download tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param status Optional task status filter.
        tasks = function(session_id = NULL, status = NULL) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_subset(status, DOWNLOADER_TASK_STATUS, empty.ok = TRUE)
            private$decorate_task_status(private$select_tasks(session_id = session_id, status = status))
        },
        # }}}

        # status {{{
        #' @description
        #' Return persistent download task status.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #'
        #' @return A data.table of matching task records.
        status = function(session_id = NULL, task_id = NULL) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_character(task_id, any.missing = FALSE, null.ok = TRUE)
            private$decorate_task_status(private$select_tasks(session_id = session_id, task_id = task_id))
        },
        # }}}

        # retry {{{
        #' @description
        #' Requeue failed or cancelled persistent tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #' @param status Task statuses to requeue. Default:
        #'        `c("error", "cancelled")`.
        #'
        #' @return A data.table of requeued task records.
        retry = function(session_id = NULL, task_id = NULL, status = c("error", "cancelled")) {
            private$require_manifest()
            checkmate::assert_subset(status, c("error", "cancelled"), empty.ok = FALSE)
            private$queue_tasks(session_id = session_id, task_id = task_id, status = status)
        },
        # }}}

        # resume {{{
        #' @description
        #' Resume queued or interrupted persistent tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #' @param ... Additional arguments passed to `$run()`.
        #'
        #' @return A data.table of selected task records after the run.
        resume = function(session_id = NULL, task_id = NULL, ...) {
            private$require_manifest()
            private$queue_tasks(session_id = session_id, task_id = task_id, status = c("downloading", "error", "cancelled"))
            self$run(session_id = session_id, task_id = task_id, ...)
        },
        # }}}

        # verify {{{
        #' @description
        #' Verify checksums for completed persistent tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #'
        #' @return A data.table of completed task records with a
        #'        `checksum_ok` column.
        verify = function(session_id = NULL, task_id = NULL) {
            private$require_manifest()
            tasks <- private$select_tasks(session_id = session_id, task_id = task_id, status = c("done", "skipped"))
            tasks$checksum_ok <- if (nrow(tasks)) {
                vapply(seq_len(nrow(tasks)), function(i) {
                    path <- tasks$target_path[[i]]
                    checksum <- download_one_chr(tasks$checksum[[i]])
                    checksum_type <- download_checksum_type(tasks$checksum_type[[i]])
                    if (!file.exists(path)) return(FALSE)
                    if (is.na(checksum)) return(TRUE)
                    private$verify_checksum_internal(path, checksum, checksum_type)
                }, logical(1L))
            } else {
                logical()
            }
            failed <- tasks[!tasks$checksum_ok]
            if (nrow(failed)) {
                failed$status <- "error"
                failed$last_error <- "Checksum verification failed."
                failed$completed_at <- download_now()
                private$replace_rows("download_task", as.data.frame(failed[, setdiff(names(failed), "checksum_ok"), with = FALSE]), "task_id")
                for (i in seq_len(nrow(failed))) {
                    private$log_event(failed$session_id[[i]], failed$task_id[[i]], "verify_error", failed$last_error[[i]])
                }
                for (sid in unique(failed$session_id)) {
                    private$update_session_status(sid)
                }
                tasks$status[!tasks$checksum_ok] <- "error"
                tasks$last_error[!tasks$checksum_ok] <- "Checksum verification failed."
            }
            tasks[]
        },
        # }}}

        # cleanup_tmp {{{
        #' @description
        #' Clean up temporary files (.part and .done files)
        #'
        #' @param all If `TRUE`, removes all temporary files. If `FALSE`,
        #'        only removes orphaned files (no corresponding final file).
        #'        Default: `FALSE`.
        #'
        #' @return Number of files removed.
        #'
        #' @examples
        #' \dontrun{
        #' n_removed <- downloader$cleanup_tmp()
        #' n_removed <- downloader$cleanup_tmp(all = TRUE)
        #' }
        cleanup_tmp = function(all = FALSE) {
            checkmate::assert_flag(all)

            tmp_files <- list.files(private$temp, pattern = "\\.(part|done)$",
                                   full.names = TRUE)

            if (length(tmp_files) == 0) {
                verbose(cli::cli_alert_info("No temporary files to clean up."))
                return(0L)
            }

            if (all) {
                file.remove(tmp_files)
                verbose(cli::cli_alert_success("Removed {.var {length(tmp_files)}} temporary file{?s}."))
                return(length(tmp_files))
            }

            # smart cleanup: check if corresponding final file exists
            to_remove <- character()
            for (tmp_file in tmp_files) {
                # extract checksum from temporary filename
                base_name <- basename(tmp_file)
                checksum <- sub("\\.(part|done)$", "", base_name)

                # try to find corresponding final file
                # search in data_dir for files that might match
                pattern <- "**/*"
                data_files <- list.files(private$dest, recursive = TRUE, full.names = TRUE)

                # check if any final file has this checksum
                found <- FALSE
                for (data_file in data_files) {
                    if (file.exists(data_file)) {
                        # this is a simple check - in production, you might want to verify checksums
                        found <- TRUE
                        break
                    }
                }

                # if no corresponding final file found, mark for removal
                if (!found) {
                    to_remove <- c(to_remove, tmp_file)
                }
            }

            if (length(to_remove) > 0) {
                file.remove(to_remove)
                verbose(cli::cli_alert_success("Removed {.var {length(to_remove)}} orphaned temporary file{?s}."))
                return(length(to_remove))
            } else {
                verbose(cli::cli_alert_info("No orphaned temporary files found."))
                return(0L)
            }
        },
        # }}}

        # get_tasks {{{
        #' @description
        #' Get all async download tasks
        #'
        #' @return A list of DownloadTask objects.
        #'
        #' @examples
        #' \dontrun{
        #' tasks <- downloader$get_tasks()
        #' }
        get_tasks = function() {
            private$async_tasks
        },
        # }}}

        # get_task_status {{{
        #' @description
        #' Get status of an async download task
        #'
        #' @param task_id Task ID returned by `download(block = FALSE)`.
        #'
        #' @return A list with task information including status, progress, etc.
        #'
        #' @examples
        #' \dontrun{
        #' task_id <- dl$download(url, block = FALSE)
        #' status <- dl$get_task_status(task_id)
        #' }
        get_task_status = function(task_id) {
            checkmate::assert_string(task_id)

            task <- private$async_tasks[[task_id]]
            if (is.null(task)) {
                stop(sprintf("Task ID '%s' not found", task_id))
            }

            # update task status if mirai object exists
            if (!is.null(task$mirai_obj)) {
                if (mirai::unresolved(task$mirai_obj)) {
                    task$status <- "downloading"
                } else {
                    # task completed, get result
                    result <- task$mirai_obj$data
                    if (inherits(result, "error")) {
                        task$mark_failed(conditionMessage(result))
                    } else {
                        task$mark_completed()
                    }
                }
            }

            list(
                task_id = task_id,
                url = task$url,
                filename = task$filename,
                status = task$status,
                progress = task$progress,
                total = task$total,
                error = task$error,
                started_at = task$started_at,
                completed_at = task$completed_at
            )
        },
        # }}}

        # wait_for_tasks {{{
        #' @description
        #' Wait for all async download tasks to complete
        #'
        #' @param task_ids Optional vector of task IDs to wait for. If `NULL`,
        #'        waits for all tasks. Default: `NULL`.
        #' @param progress Whether to show progress. Default: `TRUE`.
        #'
        #' @return A list of completed task statuses.
        #'
        #' @examples
        #' \dontrun{
        #' task1 <- dl$download(url1, block = FALSE)
        #' task2 <- dl$download(url2, block = FALSE)
        #' results <- dl$wait_for_tasks()
        #' }
        wait_for_tasks = function(task_ids = NULL, progress = TRUE) {
            checkmate::assert_character(task_ids, null.ok = TRUE)
            checkmate::assert_flag(progress)

            if (is.null(task_ids)) {
                task_ids <- names(private$async_tasks)
            }

            if (length(task_ids) == 0) {
                verbose(cli::cli_alert_info("No tasks to wait for"))
                return(list())
            }

            # wait for all tasks with progress
            progress_id <- NULL
            if (progress) {
                progress_id <- cli::cli_progress_bar(
                    "Waiting for downloads",
                    total = length(task_ids),
                    .auto_close = FALSE
                )
            }

            results <- list()
            for (task_id in task_ids) {
                task <- private$async_tasks[[task_id]]
                file_result <- NULL

                if (!is.null(task) && !is.null(task$mirai_obj)) {
                    # wait for task to complete
                    result <- task$mirai_obj[]

                    if (inherits(result, "error")) {
                        task$mark_failed(conditionMessage(result))
                    } else {
                        task$mark_completed()
                        file_result <- result  # Save the file path
                    }
                }

                # Get task status and add file result
                task_info <- self$get_task_status(task_id)
                task_info$result <- file_result
                results[[task_id]] <- task_info

                if (progress && !is.null(progress_id)) {
                    cli::cli_progress_update(id = progress_id)
                }
            }

            if (progress && !is.null(progress_id)) {
                cli::cli_progress_done(id = progress_id)
            }

            results
        },
        # }}}

        # cancel_task {{{
        #' @description
        #' Cancel an async download task
        #'
        #' @param task_id Task ID returned by `download(block = FALSE)`.
        #'
        #' @return Logical TRUE if cancellation was successful, FALSE otherwise.
        #'
        #' @examples
        #' \dontrun{
        #' task_id <- dl$download(url, block = FALSE)
        #' # Cancel if needed
        #' dl$cancel_task(task_id)
        #' }
        cancel_task = function(task_id) {
            checkmate::assert_string(task_id)

            task <- private$async_tasks[[task_id]]
            if (is.null(task)) {
                stop(sprintf("Task ID '%s' not found", task_id))
            }

            if (is.null(task$mirai_obj)) {
                cli::cli_alert_warning("Task has no mirai object, already completed?")
                return(FALSE)
            }

            result <- mirai::stop_mirai(task$mirai_obj)

            if (result) {
                task$mark_failed("Cancelled by user")
                cli::cli_alert_success("Task {task_id} cancelled")
            } else {
                cli::cli_alert_warning("Task {task_id} could not be cancelled (already completed?)")
            }

            return(result)
        },
        # }}}

        # list_incomplete {{{
        #' @description
        #' List incomplete downloads
        #'
        #' @return A data.frame with information about incomplete downloads.
        #'
        #' @examples
        #' \dontrun{
        #' incomplete <- downloader$list_incomplete()
        #' }
        list_incomplete = function() {
            part_files <- list.files(private$temp, pattern = "\\.part$",
                                    full.names = TRUE)
            done_files <- list.files(private$temp, pattern = "\\.done$",
                                    full.names = TRUE)

            if (length(part_files) == 0 && length(done_files) == 0) {
                return(data.frame(
                    file = character(),
                    status = character(),
                    size_bytes = numeric(),
                    modified = character(),
                    stringsAsFactors = FALSE
                ))
            }

            process_files <- function(files, status_label) {
                if (length(files) == 0) return(NULL)

                info <- file.info(files)
                data.frame(
                    file = basename(files),
                    status = status_label,
                    size_bytes = info$size,
                    modified = as.character(info$mtime),
                    stringsAsFactors = FALSE
                )
            }

            result <- rbind(
                process_files(part_files, "downloading"),
                process_files(done_files, "downloaded")
            )

            result
        },
        # }}}

        # verify_checksum {{{
        #' @description
        #' Verify file checksum
        #'
        #' @param file Path to file to verify.
        #' @param expected Expected checksum value.
        #' @param type Checksum type ("md5" or "sha256"). Default: `"sha256"`.
        #'
        #' @return `TRUE` if checksum matches, `FALSE` otherwise.
        #'
        #' @examples
        #' \dontrun{
        #' valid <- downloader$verify_checksum("data.nc", "abc123", "sha256")
        #' }
        verify_checksum = function(file, expected, type = "sha256") {
            checkmate::assert_file_exists(file)
            checkmate::assert_string(expected)
            checkmate::assert_choice(type, c("md5", "sha256"))

            private$verify_checksum_internal(file, expected, type)
        },
        # }}}

        # print {{{
        #' @description
        #' Print downloader summary
        #'
        #' @return The `FileDownloader` object itself, invisibly.
        print = function() {
            cli::cli_h1("ESGF File Downloader")
            cli::cli_li("Data directory: {private$dest}")
            cli::cli_li("Temporary directory: {private$temp}")
            cli::cli_li("Max retries: {private$retries}")
            cli::cli_li("Timeout: {private$dl_timeout}s")
            cli::cli_li("Auto cleanup: {private$cleanup}")

            # Show incomplete downloads
            incomplete <- self$list_incomplete()
            if (nrow(incomplete) > 0) {
                cli::cli_alert_warning("Incomplete downloads: {nrow(incomplete)}")
            }

            invisible(self)
        }
        # }}}
    ),

    active = list(
        # data_dir {{{
        #' @field data_dir The final data directory
        data_dir = function() {
            private$dest
        },
        # }}}

        # tmp_dir {{{
        #' @field tmp_dir The temporary files directory
        tmp_dir = function() {
            private$temp
        },
        # }}}

        # max_retries {{{
        #' @field max_retries Maximum number of retry attempts
        max_retries = function() {
            private$retries
        },
        # }}}

        # timeout {{{
        #' @field timeout Download timeout in seconds
        timeout = function() {
            private$dl_timeout
        },
        # }}}

        # n_workers {{{
        #' @field n_workers Number of parallel workers
        n_workers = function() {
            private$worker_count
        },
        # }}}

        # manifest {{{
        #' @field manifest Persistent download manifest path, or `NULL`.
        manifest = function() {
            private$manifest_path
        },
        # }}}

        # config_file {{{
        #' @field config_file Downloader config path, or `NULL`.
        config_file = function() {
            private$config_path
        }
        # }}}
    ),

    private = list(
        dest = NULL,
        temp = NULL,
        manifest_path = NULL,
        manifest_conn = NULL,
        config_path = NULL,
        dl_timeout = NULL,
        retries = NULL,
        cleanup = NULL,
        worker_count = NULL,
        in_dev = NULL,
        async_tasks = NULL,  # List of DownloadTask objects for async downloads

        # finalize {{{
        finalize = function() {
            private$disconnect_manifest()
            if (!is.null(private$worker_count) && private$worker_count > 0) {
                mirai::daemons(0)
            }
        },
        # }}}

        # config_payload {{{
        config_payload = function() {
            list(
                schema_version = DOWNLOADER_SCHEMA_VERSION,
                dest = private$dest,
                temp = private$temp,
                manifest = private$manifest_path,
                retries = as.integer(private$retries),
                timeout = as.integer(private$dl_timeout),
                cleanup = isTRUE(private$cleanup),
                n_workers = as.integer(private$worker_count)
            )
        },
        # }}}

        # manifest {{{
        connect_manifest = function() {
            if (is.null(private$manifest_path) || !is.null(private$manifest_conn)) {
                return(invisible(NULL))
            }
            private$manifest_conn <- ddb_connect(private$manifest_path, read_only = FALSE)
            invisible(private$manifest_conn)
        },

        disconnect_manifest = function() {
            if (!is.null(private$manifest_conn)) {
                valid <- tryCatch(ddb_is_valid(private$manifest_conn), error = function(e) FALSE)
                if (isTRUE(valid)) {
                    tryCatch(
                        ddb_disconnect(private$manifest_conn, shutdown = TRUE),
                        error = function(e) ddb_disconnect(private$manifest_conn)
                    )
                }
                private$manifest_conn <- NULL
            }
            invisible(NULL)
        },

        require_manifest = function() {
            if (is.null(private$manifest_path)) {
                cli::cli_abort("This downloader has no persistent manifest. Create it with {.code FileDownloader$new(manifest = ...)}.")
            }
            if (!is.null(private$manifest_conn)) {
                valid <- tryCatch(ddb_is_valid(private$manifest_conn), error = function(e) FALSE)
                if (!isTRUE(valid)) {
                    private$manifest_conn <- NULL
                }
            }
            if (is.null(private$manifest_conn)) {
                private$connect_manifest()
                private$init_manifest_schema()
            }
            invisible(private$manifest_conn)
        },

        exec_manifest = function(sql) {
            ddb_exec(private$manifest_conn, sql)
        },

        init_manifest_schema = function() {
            private$exec_manifest("
                CREATE TABLE IF NOT EXISTS download_session (
                    session_id VARCHAR PRIMARY KEY,
                    label VARCHAR,
                    status VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP,
                    completed_at TIMESTAMP
                )
            ")
            private$exec_manifest("
                CREATE TABLE IF NOT EXISTS download_task (
                    task_id VARCHAR PRIMARY KEY,
                    session_id VARCHAR,
                    logical_file_id VARCHAR,
                    file_key VARCHAR,
                    esgf_id VARCHAR,
                    dataset_id VARCHAR,
                    filename VARCHAR,
                    subdir VARCHAR,
                    target_path VARCHAR,
                    checksum VARCHAR,
                    checksum_type VARCHAR,
                    size DOUBLE,
                    status VARCHAR,
                    attempts INTEGER,
                    bytes_done DOUBLE,
                    selected_url VARCHAR,
                    data_node VARCHAR,
                    last_error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP,
                    completed_at TIMESTAMP
                )
            ")
            private$exec_manifest("
                CREATE TABLE IF NOT EXISTS download_candidate (
                    candidate_id VARCHAR PRIMARY KEY,
                    task_id VARCHAR,
                    url VARCHAR,
                    service VARCHAR,
                    data_node VARCHAR,
                    priority INTEGER,
                    probe_latency DOUBLE,
                    probe_throughput DOUBLE,
                    failed_count INTEGER,
                    last_error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            ")
            private$exec_manifest("
                CREATE TABLE IF NOT EXISTS download_event (
                    event_id VARCHAR PRIMARY KEY,
                    session_id VARCHAR,
                    task_id VARCHAR,
                    event VARCHAR,
                    message VARCHAR,
                    created_at TIMESTAMP
                )
            ")
            private$exec_manifest("
                CREATE TABLE IF NOT EXISTS download_meta (
                    key VARCHAR PRIMARY KEY,
                    value VARCHAR,
                    updated_at TIMESTAMP
                )
            ")
            private$replace_rows("download_meta", data.frame(
                key = "schema_version",
                value = DOWNLOADER_SCHEMA_VERSION,
                updated_at = download_now(),
                stringsAsFactors = FALSE
            ), "key")
            invisible(NULL)
        },

        append_rows = function(table, rows) {
            if (!nrow(rows)) return(invisible(rows))
            ddb_append_table(private$manifest_conn, table, as.data.frame(rows))
            invisible(rows)
        },

        replace_rows = function(table, rows, key) {
            if (!nrow(rows)) return(invisible(rows))
            for (value in rows[[key]]) {
                ddb_exec(private$manifest_conn, sprintf(
                    "DELETE FROM %s WHERE %s = %s",
                    ddb_ident(private$manifest_conn, table),
                    ddb_ident(private$manifest_conn, key),
                    ddb_literal(private$manifest_conn, value)
                ))
            }
            ddb_append_table(private$manifest_conn, table, as.data.frame(rows))
            invisible(rows)
        },

        read_table = function(table) {
            data.table::as.data.table(ddb_read_table(private$manifest_conn, table))
        },

        log_event = function(session_id, task_id, event, message = NA_character_) {
            private$append_rows("download_event", data.frame(
                event_id = download_hash(session_id, task_id, event, message, as.numeric(Sys.time()), stats::runif(1L)),
                session_id = download_one_chr(session_id),
                task_id = download_one_chr(task_id),
                event = event,
                message = download_one_chr(message),
                created_at = download_now(),
                stringsAsFactors = FALSE
            ))
        },
        # }}}

        # plan and task helpers {{{
        new_session_id = function() {
            stamp <- format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")
            paste0(stamp, "-", substr(download_hash(stamp, stats::runif(1L)), 1L, 8L))
        },

        normalize_plan = function(plan) {
            if (inherits(plan, "data.table")) {
                plan <- data.table::copy(plan)
            } else if (is.data.frame(plan)) {
                plan <- data.table::as.data.table(plan)
            } else {
                cli::cli_abort("`plan` must be a data frame returned by {.code download_plan()}.")
            }

            missing <- setdiff(c("logical_file_id", "filename", "url"), names(plan))
            if (length(missing)) {
                cli::cli_abort("Download plan is missing required column(s): {.field {missing}}.")
            }
            defaults <- list(
                file_key = NA_character_,
                esgf_id = NA_character_,
                dataset_id = NA_character_,
                subdir = NA_character_,
                checksum = NA_character_,
                checksum_type = "sha256",
                size = NA_real_,
                service = "HTTPServer",
                data_node = NA_character_,
                priority = NA_integer_,
                probe_latency = NA_real_,
                probe_throughput = NA_real_
            )
            for (name in setdiff(names(defaults), names(plan))) {
                plan[[name]] <- defaults[[name]]
            }
            plan <- plan[!is.na(url) & nzchar(url)]
            if (!nrow(plan)) return(plan)

            plan[, checksum_type := vapply(checksum_type, download_checksum_type, character(1L))]
            plan[, priority := data.table::fifelse(is.na(priority), seq_len(.N), as.integer(priority))]
            plan[, size := suppressWarnings(as.numeric(size))]
            plan[, filename := data.table::fifelse(is.na(filename) | !nzchar(filename), basename(sub("\\?.*$", "", url)), filename)]
            plan[]
        },

        target_path = function(filename, subdir = NA_character_) {
            if (is.na(subdir) || !nzchar(subdir)) {
                file.path(private$dest, filename)
            } else {
                file.path(private$dest, subdir, filename)
            }
        },

        plan_tasks = function(plan, session_id, now) {
            task <- unique(plan[, .(
                logical_file_id,
                file_key,
                esgf_id,
                dataset_id,
                filename,
                subdir,
                checksum,
                checksum_type,
                size
            )])
            task[, target_path := mapply(private$target_path, filename, subdir, USE.NAMES = FALSE)]
            task[, task_id := vapply(seq_len(.N), function(i) {
                download_hash(session_id, logical_file_id[[i]], target_path[[i]])
            }, character(1L))]
            task[, `:=`(
                session_id = session_id,
                status = "queued",
                attempts = 0L,
                bytes_done = 0,
                selected_url = NA_character_,
                data_node = NA_character_,
                last_error = NA_character_,
                created_at = now,
                updated_at = now,
                completed_at = as.POSIXct(NA)
            )]
            data.table::setcolorder(task, c(
                "task_id", "session_id", "logical_file_id", "file_key",
                "esgf_id", "dataset_id", "filename", "subdir", "target_path",
                "checksum", "checksum_type", "size", "status", "attempts",
                "bytes_done", "selected_url", "data_node", "last_error",
                "created_at", "updated_at", "completed_at"
            ))
            task[]
        },

        plan_candidates = function(plan, tasks, now) {
            key <- c("logical_file_id", "filename", "subdir", "checksum", "checksum_type")
            candidate <- merge(
                plan,
                tasks[, c(key, "task_id"), with = FALSE],
                by = key,
                all.x = TRUE,
                sort = FALSE
            )
            candidate[, candidate_id := vapply(seq_len(.N), function(i) {
                download_hash(task_id[[i]], url[[i]])
            }, character(1L))]
            unique(candidate[, .(
                candidate_id,
                task_id,
                url,
                service,
                data_node,
                priority = as.integer(priority),
                probe_latency = as.numeric(probe_latency),
                probe_throughput = as.numeric(probe_throughput),
                failed_count = 0L,
                last_error = NA_character_,
                created_at = now,
                updated_at = now
            )], by = "candidate_id")
        },

        select_tasks = function(session_id = NULL, task_id = NULL, status = NULL) {
            tasks <- private$read_table("download_task")
            if (!nrow(tasks)) return(tasks[])
            sid <- session_id
            tid <- task_id
            task_status <- status
            if (!is.null(sid)) tasks <- tasks[tasks[["session_id"]] == sid]
            if (!is.null(tid)) tasks <- tasks[tasks[["task_id"]] %in% tid]
            if (!is.null(task_status)) tasks <- tasks[tasks[["status"]] %in% task_status]
            tasks[]
        },

        decorate_task_status = function(tasks) {
            tasks <- data.table::as.data.table(tasks)
            if (!nrow(tasks)) {
                tasks$candidate_count <- integer()
                tasks$failed_candidate_count <- integer()
                tasks$selected_data_node <- character()
                return(tasks[])
            }
            candidates <- private$read_table("download_candidate")
            if (!nrow(candidates)) {
                tasks$candidate_count <- 0L
                tasks$failed_candidate_count <- 0L
                tasks$selected_data_node <- tasks$data_node
                return(tasks[])
            }
            summary <- candidates[, .(
                candidate_count = .N,
                failed_candidate_count = sum(as.integer(failed_count) > 0L, na.rm = TRUE)
            ), by = "task_id"]
            tasks <- merge(tasks, summary, by = "task_id", all.x = TRUE, sort = FALSE)
            tasks[is.na(candidate_count), candidate_count := 0L]
            tasks[is.na(failed_candidate_count), failed_candidate_count := 0L]
            tasks[, selected_data_node := data_node]
            tasks[]
        },

        cancel_stale_downloading = function(session_id = NULL, task_id = NULL) {
            tasks <- private$select_tasks(session_id = session_id, task_id = task_id, status = "downloading")
            if (!nrow(tasks)) {
                return(invisible(tasks))
            }
            stale <- tasks[tasks[["updated_at"]] < DOWNLOADER_SESSION_STARTED_AT]
            if (!nrow(stale)) {
                return(invisible(stale))
            }
            stale$status <- "cancelled"
            stale$last_error <- "Download was left in progress by a previous R session."
            stale$updated_at <- download_now()
            stale$completed_at <- download_now()
            private$replace_rows("download_task", as.data.frame(stale), "task_id")
            for (i in seq_len(nrow(stale))) {
                private$log_event(stale$session_id[[i]], stale$task_id[[i]], "cancelled", stale$last_error[[i]])
            }
            for (sid in unique(stale$session_id)) {
                private$update_session_status(sid)
            }
            invisible(stale)
        },

        get_candidates = function(task_id) {
            tid <- task_id
            candidates <- private$read_table("download_candidate")
            candidates <- candidates[candidates[["task_id"]] == tid]
            data.table::setorderv(candidates, c("priority", "failed_count"), c(1L, 1L))
            candidates[]
        },

        update_task = function(task) {
            task$updated_at <- download_now()
            private$replace_rows("download_task", as.data.frame(task), "task_id")
            private$update_session_status(task$session_id[[1L]])
            invisible(task)
        },

        update_candidate = function(candidate) {
            candidate$updated_at <- download_now()
            private$replace_rows("download_candidate", as.data.frame(candidate), "candidate_id")
            invisible(candidate)
        },

        update_session_status = function(session_id) {
            sessions <- private$read_table("download_session")
            idx <- match(session_id, sessions$session_id)
            if (is.na(idx)) return(invisible(NULL))
            tasks <- private$select_tasks(session_id = session_id)
            status <- if (!nrow(tasks)) {
                "queued"
            } else if (all(tasks$status %in% c("done", "skipped"))) {
                "done"
            } else if (any(tasks$status == "error")) {
                "error"
            } else if (any(tasks$status == "cancelled")) {
                "cancelled"
            } else if (any(tasks$status == "downloading")) {
                "downloading"
            } else {
                "queued"
            }
            sessions$status[idx] <- status
            sessions$updated_at[idx] <- download_now()
            if (status %in% c("done", "error", "cancelled")) {
                sessions$completed_at[idx] <- download_now()
            }
            private$replace_rows("download_session", as.data.frame(sessions[idx]), "session_id")
            invisible(status)
        },

        queue_tasks = function(session_id = NULL, task_id = NULL, status = c("error", "cancelled")) {
            tasks <- private$select_tasks(session_id = session_id, task_id = task_id, status = status)
            if (!nrow(tasks)) return(tasks)
            tasks$status <- "queued"
            tasks$last_error <- NA_character_
            tasks$updated_at <- download_now()
            tasks$completed_at <- as.POSIXct(NA)
            private$replace_rows("download_task", as.data.frame(tasks), "task_id")
            for (sid in unique(tasks$session_id)) {
                private$update_session_status(sid)
                private$log_event(sid, NA_character_, "retry", sprintf("Requeued %d task(s).", sum(tasks$session_id == sid)))
            }
            private$select_tasks(session_id = session_id, task_id = task_id)
        },

        run_task = function(task, progress = TRUE, overwrite = FALSE, resume = TRUE) {
            task <- data.table::as.data.table(task)
            task_id <- task$task_id[[1L]]
            session_id <- task$session_id[[1L]]
            checksum <- download_one_chr(task$checksum[[1L]])
            checksum_type <- download_checksum_type(task$checksum_type[[1L]])
            target_path <- task$target_path[[1L]]

            if (file.exists(target_path) && !isTRUE(overwrite)) {
                ok <- if (is.na(checksum)) TRUE else private$verify_checksum_internal(target_path, checksum, checksum_type)
                if (ok) {
                    task$status <- "skipped"
                    task$bytes_done <- file.info(target_path)$size
                    task$last_error <- NA_character_
                    task$completed_at <- download_now()
                    private$update_task(task)
                    private$log_event(session_id, task_id, "skipped", "Final file already exists and passed validation.")
                    return(target_path)
                }
            }

            candidates <- private$get_candidates(task_id)
            if (!nrow(candidates)) {
                task$status <- "error"
                task$last_error <- "No candidate URLs are available."
                task$completed_at <- download_now()
                private$update_task(task)
                private$log_event(session_id, task_id, "error", task$last_error)
                return(NA_character_)
            }

            last_error <- NULL
            for (i in seq_len(nrow(candidates))) {
                candidate <- candidates[i, , drop = FALSE]
                attempts <- suppressWarnings(as.integer(task$attempts[[1L]]))
                if (is.na(attempts)) attempts <- 0L
                task$status <- "downloading"
                task$attempts <- attempts + 1L
                task$selected_url <- candidate$url[[1L]]
                task$data_node <- candidate$data_node[[1L]]
                task$last_error <- NA_character_
                private$update_task(task)
                private$log_event(session_id, task_id, "start", candidate$url[[1L]])

                task_subdir <- download_one_chr(task$subdir[[1L]])
                if (is.na(task_subdir)) task_subdir <- NULL
                path <- tryCatch(
                    self$download(
                        url = candidate$url[[1L]],
                        filename = task$filename[[1L]],
                        subdir = task_subdir,
                        progress = progress,
                        overwrite = overwrite,
                        checksum = if (is.na(checksum)) NULL else checksum,
                        checksum_type = checksum_type,
                        resume = resume,
                        block = TRUE,
                        .tmp_id = task_id
                    ),
                    error = function(e) {
                        last_error <<- conditionMessage(e)
                        NULL
                    }
                )

                if (!is.null(path) && file.exists(path)) {
                    task$status <- "done"
                    task$target_path <- normalizePath(path, mustWork = TRUE, winslash = "/")
                    task$bytes_done <- file.info(path)$size
                    task$last_error <- NA_character_
                    task$completed_at <- download_now()
                    private$update_task(task)
                    private$log_event(session_id, task_id, "done", path)
                    return(path)
                }

                candidate$failed_count <- as.integer(candidate$failed_count) + 1L
                candidate$last_error <- last_error
                private$update_candidate(candidate)
                private$log_event(session_id, task_id, "candidate_error", last_error)
            }

            task$status <- "error"
            task$last_error <- if (is.null(last_error) || is.na(last_error)) "All candidate URLs failed." else last_error
            task$completed_at <- download_now()
            private$update_task(task)
            private$log_event(session_id, task_id, "error", task$last_error)
            NA_character_
        },
        # }}}

        # check_file_status {{{
        check_file_status = function(dest, tmp_part, tmp_done, checksum, checksum_type) {
            # Check final destination
            if (file.exists(dest)) {
                # Verify checksum if provided
                if (!is.null(checksum)) {
                    if (private$verify_checksum_internal(dest, checksum, checksum_type)) {
                        return(FileStatus$Verified)
                    } else {
                        # File exists but checksum doesn't match
                        return(FileStatus$Missing)
                    }
                } else {
                    return(FileStatus$Verified)
                }
            }

            # Check .done file
            if (file.exists(tmp_done)) {
                return(FileStatus$Downloaded)
            }

            # Check .part file
            if (file.exists(tmp_part)) {
                return(FileStatus$Downloading)
            }

            return(FileStatus$Missing)
        },
        # }}}

        # finalize_download {{{
        finalize_download = function(tmp_done, dest) {
            dest_dir <- dirname(dest)
            if (!dir.exists(dest_dir)) {
                dir.create(dest_dir, recursive = TRUE)
            }

            # Try atomic rename first
            renamed <- tryCatch(
                {
                    file.rename(tmp_done, dest)
                    TRUE
                },
                error = function(e) FALSE
            )

            if (!renamed) {
                # Fall back to copy + delete
                warning("Cannot rename across filesystems. Using copy instead (slower).")
                file.copy(tmp_done, dest, overwrite = TRUE)
                file.remove(tmp_done)
            }

            invisible(dest)
        },
        # }}}

        # download_async {{{
        download_async = function(url, filename, subdir, progress,
                                  overwrite, checksum, checksum_type, resume) {
            # Check offline mode before launching async download
            if (is_cache_offline()) {
                stop(
                    "Cannot download file in offline mode: ", url,
                    call. = FALSE
                )
            }

            # Generate task ID
            task_id <- checksum_bytes(charToRaw(paste(url, Sys.time())), "md5")

            # Determine filename
            if (is.null(filename)) {
                filename <- basename(url)
                filename <- sub("\\?.*$", "", filename)
            }

            # Create task
            task <- DownloadTask$new(url, filename)
            private$async_tasks[[task_id]] <- task

            # Start mirai download
            if (!requireNamespace("mirai", quietly = TRUE)) {
                stop("mirai package required for async downloads")
            }

            # Capture self parameters for mirai
            downloader_params <- list(
                dest = private$dest,
                temp = private$temp,
                retries = private$retries,
                timeout = private$dl_timeout,
                cleanup = private$cleanup
            )

            # Launch async download (no need to check daemons, already started)
            task$mirai_obj <- mirai::mirai(
                {
                    # Prefer using FileDownloader from the current environment.
                    # In dev mode, this will be available if downloader.R was sourced via everywhere().
                    # In installed mode, fall back to epwshiftr::FileDownloader.
                    ctor <- NULL
                    if (exists("FileDownloader", mode = "function")) {
                        ctor <- get("FileDownloader")
                    } else if ("epwshiftr" %in% .packages(all.available = TRUE)) {
                        ctor <- epwshiftr::FileDownloader
                    } else {
                        stop("FileDownloader class not available in worker")
                    }

                    dl <- ctor$new(
                        dest = downloader_params$dest,
                        temp = downloader_params$temp,
                        retries = downloader_params$retries,
                        timeout = downloader_params$timeout,
                        cleanup = downloader_params$cleanup,
                        n_workers = 0L  # Async tasks don't need their own workers
                    )

                    dl$download(
                        url = url,
                        filename = filename,
                        subdir = subdir,
                        progress = FALSE,  # No progress in async mode
                        overwrite = overwrite,
                        checksum = checksum,
                        checksum_type = checksum_type,
                        resume = resume,
                        block = TRUE  # Must be blocking in mirai
                    )
                },
                url = url,
                filename = filename,
                subdir = subdir,
                overwrite = overwrite,
                checksum = checksum,
                checksum_type = checksum_type,
                resume = resume,
                downloader_params = downloader_params
            )

            task$status <- "downloading"

            verbose(cli::cli_alert_success("Started async download: {url}"))
            verbose(cli::cli_alert_info("Task ID: {task_id}"))

            invisible(task_id)
        },
        # }}}

        # download_with_streaming {{{
        download_with_streaming = function(url, tmp_part, tmp_done, progress, start_byte) {
            handle <- curl::new_handle()
            curl::handle_setopt(handle,
                timeout = private$dl_timeout,
                followlocation = TRUE,
                ssl_verifypeer = TRUE
            )

            # Add Range header for resume
            if (start_byte > 0) {
                curl::handle_setheaders(handle,
                    Range = sprintf("bytes=%d-", start_byte)
                )
            }

            # Open file for writing
            mode <- if (start_byte > 0) "ab" else "wb"
            con <- file(tmp_part, mode)
            on.exit(close(con), add = TRUE)

            # Track download progress
            bytes_downloaded <- start_byte
            start_time <- Sys.time()
            last_update <- start_time

            progress_id <- NULL
            if (progress) {
                progress_id <- cli::cli_progress_bar(
                    "Downloading",
                    total = NA,
                    .auto_close = FALSE
                )
            }

            # Download with streaming
            curl::curl_fetch_stream(url, function(chunk) {
                # Write chunk to file
                writeBin(chunk, con)
                bytes_downloaded <<- bytes_downloaded + length(chunk)

                # Update progress (limit update frequency)
                if (progress && !is.null(progress_id)) {
                    current_time <- Sys.time()
                    if (difftime(current_time, last_update, units = "secs") > 0.1) {
                        elapsed <- as.numeric(difftime(current_time, start_time, units = "secs"))
                        speed <- if (elapsed > 0) (bytes_downloaded - start_byte) / elapsed else 0

                        cli::cli_progress_update(
                            id = progress_id,
                            status = sprintf(
                                "%s (%s/s)",
                                format_bytes(bytes_downloaded),
                                format_bytes(speed)
                            )
                        )
                        last_update <<- current_time
                    }
                }

                TRUE  # Continue download
            }, handle = handle)

            close(con)
            on.exit(NULL)

            if (progress && !is.null(progress_id)) {
                elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                speed <- if (elapsed > 0) (bytes_downloaded - start_byte) / elapsed else 0
                cli::cli_progress_done(
                    id = progress_id,
                    result = sprintf(
                        "Downloaded %s in %.1fs (avg %s/s)",
                        format_bytes(bytes_downloaded),
                        elapsed,
                        format_bytes(speed)
                    )
                )
            }

            # Move to .done
            file.rename(tmp_part, tmp_done)

            invisible(tmp_done)
        },
        # }}}

        # verify_checksum_internal {{{
        verify_checksum_internal = function(file, expected, type) {
            verify_checksum(file, expected, type)
        },
        # }}}

        # get_file_checksum {{{
        get_file_checksum = function(file, type) {
            checkmate::assert_file_exists(file)
            checkmate::assert_choice(type, c("md5", "sha256"))

            tolower(checksum_file(file, type))
        }
        # }}}
    )
)
# }}}
FileDownloader$load_config <- function(file) {
    FileDownloader$new(config = file)
}
