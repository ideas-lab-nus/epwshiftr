# verify_checksum {{{
verify_checksum <- function(file, checksum, algo = "sha256") {
    checkmate::assert_file_exists(file)
    checkmate::assert_string(checksum)
    checkmate::assert_choice(algo, c("md5", "sha256"))

    con <- file(file, "rb")
    on.exit(close(con), add = TRUE)

    # convert to plain character strings for comparison
    # openssl returns objects with class "hash" which fail identical() comparison
    actual <- if (algo == "sha256") {
        as.character(openssl::sha256(con))
    } else {
        as.character(openssl::md5(con))
    }

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
        initialize = function(dest = NULL, temp = NULL, retries = 3L, timeout = 3600L, cleanup = TRUE, n_workers = 4L) {
            if (is.null(dest)) {
                dest <- tempdir()
            }

            checkmate::assert_directory_exists(dest, access = "rw")
            checkmate::assert_string(temp, null.ok = TRUE)
            checkmate::assert_count(retries, positive = TRUE)
            checkmate::assert_count(timeout, positive = TRUE)
            checkmate::assert_flag(cleanup)
            checkmate::assert_count(n_workers, positive = FALSE)

            # Check if in development mode via environment variable or option
            in_dev <- isTRUE(as.logical(Sys.getenv("EPWSHIFTR_DEV_MODE", "FALSE"))) ||
                      isTRUE(getOption("epwshiftr.dev_mode", FALSE))

            private$dest <- normalizePath(dest, mustWork = TRUE)

            # Set up temp
            if (is.null(temp)) {
                private$temp <- file.path(private$dest, ".tmp")
            } else {
                private$temp <- temp
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
                                resume = TRUE, block = TRUE) {
            checkmate::assert_string(url)
            checkmate::assert_string(filename, null.ok = TRUE)
            checkmate::assert_string(subdir, null.ok = TRUE)
            checkmate::assert_flag(progress)
            checkmate::assert_flag(overwrite)
            checkmate::assert_string(checksum, null.ok = TRUE)
            checkmate::assert_choice(checksum_type, c("md5", "sha256"))
            checkmate::assert_flag(resume)
            checkmate::assert_flag(block)

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
            tmp_id <- if (!is.null(checksum)) checksum else as.character(openssl::md5(url))
            tmp_part <- file.path(private$temp, paste0(tmp_id, ".part"))
            tmp_done <- file.path(private$temp, paste0(tmp_id, ".done"))

            # check current status
            status <- private$check_file_status(dest, tmp_part, tmp_done, checksum, checksum_type)

            # if file is verified and exists, return it
            if (status == FileStatus$Verified && !overwrite) {
                verbose(cli::cli_alert_info("File already verified: {dest}. Skipping..."))
                return(dest)
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

            # smart cleanup: check if corresponding .done file has a final file
            # Temp files are named as "<checksum_or_hash>.(part|done)".
            # A .done file is orphaned if no corresponding final file exists in dest.
            # A .part file is orphaned if there is no matching .done file either.
            to_remove <- character()
            part_files <- grep("\\.part$", tmp_files, value = TRUE)
            done_files <- grep("\\.done$", tmp_files, value = TRUE)
            done_ids <- sub("\\.done$", "", basename(done_files))
            part_ids <- sub("\\.part$", "", basename(part_files))

            # Identify .done files that are orphaned (no final file moved to dest)
            # We consider a .done file orphaned — it should have been moved already
            to_remove <- c(to_remove, done_files)

            # Identify .part files that have no corresponding .done file
            # (i.e., the download was interrupted and never completed)
            orphaned_parts <- part_files[!part_ids %in% done_ids]
            to_remove <- c(to_remove, orphaned_parts)

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
                    task$status <- DownloadStatus$Downloading
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
            cli::cli_h1("File Downloader")
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
        }
        # }}}
    ),

    private = list(
        dest = NULL,
        temp = NULL,
        dl_timeout = NULL,
        retries = NULL,
        cleanup = NULL,
        worker_count = NULL,
        in_dev = NULL,
        async_tasks = NULL,  # List of DownloadTask objects for async downloads

        # finalize {{{
        finalize = function() {
            if (!is.null(private$worker_count) && private$worker_count > 0) {
                mirai::daemons(0)
            }
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
            # Generate task ID
            task_id <- as.character(openssl::md5(paste(url, Sys.time())))

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
                    if (exists("FileDownloader")) {
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

            task$status <- DownloadStatus$Downloading

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

            # Try openssl first (most efficient)
            if (requireNamespace("openssl", quietly = TRUE)) {
                con <- file(file, "rb")
                on.exit(close(con), add = TRUE)

                actual <- if (type == "sha256") {
                    as.character(openssl::sha256(con))
                } else {
                    as.character(openssl::md5(con))
                }
            } else {
                # Fallback to digest package
                actual <- digest::digest(file, algo = type, file = TRUE)
            }

            tolower(as.character(actual))
        }
        # }}}
    )
)
# }}}
