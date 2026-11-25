# ---
# repo: ideas-lab-nus/epwshiftr
# file: standalone-downloader.R
# last-updated: 2026-10-05
# copyright: Copyright (c) 2019-2026 Hongyuan Jia and Adrian Chong
# SPDX-License-Identifier: MIT
# license: MIT
# imports: [checkmate (>= 2.0.0), cli (>= 3.4.0), curl, duckdb, jsonlite, mirai (>= 2.7.3), R6]
# ---
#
# # Standalone Changelog
#
# ## 2026-10-05
# - Own local worker startup, including macOS loopback TCP transport.
# - Publish the independently copyable Downloader and its local helpers.
# - Include checksum verification, persistent manifests, resumable transfers,
#   background jobs and independent asynchronous worker pools.
# - Keep host cache policy and store synchronization optional.
#
# Import with usethis::use_standalone("ideas-lab-nus/epwshiftr", "downloader").
# Package dependencies above are installed by the receiving project. No other
# source files are required. Retain the MIT copyright and license when copying.
#
# This file is a copyable downloader module. It requires its explicitly qualified
# R packages, but no other epwshiftr source file. Optional host integration is
# supplied by the owning package after load; standalone defaults perform downloads.
# Remember a sourced file for detached workers without serializing the caller's
# environment. Installed packages resolve their own namespace at launch time.
DOWNLOADER_RUNTIME <- local({
    runtime <- new.env(parent = emptyenv())
    runtime$source <- NULL
    for (frame in if (isNamespace(topenv())) list() else rev(sys.frames())) {
        file <- get0("ofile", envir = frame, inherits = FALSE)
        if (is.null(file)) {
            file <- get0("file", envir = frame, inherits = FALSE)
        }
        if (is.character(file) && length(file) == 1L && file.exists(file)) {
            runtime$source <- normalizePath(file, winslash = "/")
            break
        }
    }
    runtime
})
DOWNLOADER_RUNTIME$offline <- NULL
DOWNLOADER_RUNTIME$verbose <- NULL
DOWNLOADER_RUNTIME$sync_store <- NULL

# Start an owned local pool without changing its worker count or dispatcher.
# Use mirai's public loopback TCP transport on each platform. This choice
# avoids the default transport implicated in a local macOS shutdown wait; it
# does not establish the cause of that wait or prevent all native shutdown hangs.
downloader__start_pool <- function(
    n,
    dispatcher = TRUE,
    .compute,
    output = FALSE
) {
    checkmate::assert_count(n, positive = TRUE)
    checkmate::assert_flag(output)
    checkmate::assert_string(.compute, min.chars = 1L)
    started <- FALSE
    on.exit(
        {
            if (!started) {
                try(mirai::daemons(0L, .compute = .compute), silent = TRUE)
            }
        },
        add = TRUE
    )
    mirai::daemons(
        n,
        url = mirai::local_url(tcp = TRUE),
        dispatcher = dispatcher,
        output = output,
        .compute = .compute
    )
    # Explicit URLs configure a listener; launch_local() connects workers to
    # its actual bound port. Passing port zero directly to workers is invalid.
    mirai::launch_local(n, .compute = .compute)
    # This bounds the connection polling interval after launch_local() returns,
    # not native startup/status/cleanup calls or task execution.
    deadline <- proc.time()[["elapsed"]] + 30
    repeat {
        connections <- mirai::status(.compute = .compute)$connections
        if (connections >= n) {
            break
        }
        if (proc.time()[["elapsed"]] >= deadline) {
            stop(
                "Local mirai connection wait exceeded 30 seconds for profile '",
                .compute,
                "': expected ",
                n,
                " worker(s), connected ",
                connections,
                ".",
                call. = FALSE
            )
        }
        Sys.sleep(0.01)
    }
    started <- TRUE
    invisible(TRUE)
}

# Keep host cache policy optional; copying this module does not require a cache.
# downloader__offline {{{
downloader__offline <- function() {
    is.function(DOWNLOADER_RUNTIME$offline) &&
        isTRUE(DOWNLOADER_RUNTIME$offline())
}
# }}}

# Evaluate messages only when the host explicitly enables verbose output.
# downloader__verbose {{{
downloader__verbose <- function(expr) {
    if (
        is.function(DOWNLOADER_RUNTIME$verbose) &&
            isTRUE(DOWNLOADER_RUNTIME$verbose())
    ) {
        force(expr)
    }
    invisible(NULL)
}
# }}}

# Resolve the owning installed package or the one sourced module for workers.
# downloader__runtime {{{
downloader__runtime <- function() {
    owner <- topenv(environment(downloader__runtime))
    if (isNamespace(owner)) {
        path <- getNamespaceInfo(owner, "path")
        if (!file.exists(file.path(path, "Meta", "package.rds"))) {
            cli::cli_abort(
                "Downloader workers require an installed package or a sourced downloader file."
            )
        }
        return(list(
            package = getNamespaceName(owner),
            source = NULL,
            libraries = unique(c(dirname(path), .libPaths()))
        ))
    }
    checkmate::assert_file_exists(DOWNLOADER_RUNTIME$source)
    list(
        package = NULL,
        source = DOWNLOADER_RUNTIME$source,
        libraries = .libPaths()
    )
}
# }}}

# Only scalar strings may enter the detached R command expression.
# downloader__string_literal {{{
downloader__string_literal <- function(x) {
    checkmate::assert_string(x, null.ok = TRUE)
    if (is.null(x)) "NULL" else encodeString(x, quote = '"')
}
# }}}

# Normalize both R conditions and mirai transport failures before reading results.
# downloader__worker_error {{{
downloader__worker_error <- function(result) {
    if (inherits(result, "error")) {
        return(conditionMessage(result))
    }
    if (inherits(result, "miraiError")) {
        message <- attr(result, "message", exact = TRUE)
        if (is.null(message) || !nzchar(message)) {
            message <- as.character(result)
        }
        return(message)
    }
    if (inherits(result, "errorValue")) {
        return(sprintf(
            "mirai worker returned error code %s",
            unclass(result)[[1L]]
        ))
    }
    ""
}
# }}}

# Download-local checksum file; kept here so the module can be copied alone.
# downloader__checksum_file {{{
downloader__checksum_file <- function(path, algo = "sha256") {
    checkmate::assert_file_exists(path, access = "r")
    checkmate::assert_choice(algo, c("md5", "sha256"))

    out <- if (identical(algo, "sha256")) {
        tools::sha256sum(path)
    } else {
        tools::md5sum(path)
    }
    unname(as.character(out))
}
# }}}

# Download-local checksum bytes; kept here so the module can be copied alone.
# downloader__checksum_bytes {{{
downloader__checksum_bytes <- function(bytes, algo = "sha256") {
    if (!is.raw(bytes)) {
        stop("`bytes` must be a raw vector.", call. = FALSE)
    }
    checkmate::assert_choice(algo, c("md5", "sha256"))

    out <- if (identical(algo, "sha256")) {
        tools::sha256sum(bytes = bytes)
    } else {
        tools::md5sum(bytes = bytes)
    }
    unname(as.character(out))
}
# }}}

# Download-local ddb connect; kept here so the module can be copied alone.
# downloader__ddb_connect {{{
downloader__ddb_connect <- function(dbdir, read_only = FALSE, ...) {
    duckdb::dbConnect(
        duckdb::duckdb(),
        dbdir = dbdir,
        read_only = read_only,
        ...
    )
}
# }}}

# Download-local ddb disconnect; kept here so the module can be copied alone.
# downloader__ddb_disconnect {{{
downloader__ddb_disconnect <- function(conn, shutdown = TRUE) {
    duckdb::dbDisconnect(conn, shutdown = shutdown)
}
# }}}

# Download-local ddb is valid; kept here so the module can be copied alone.
# downloader__ddb_is_valid {{{
downloader__ddb_is_valid <- function(conn) {
    duckdb::dbIsValid(conn)
}
# }}}

# Download-local ddb exec; kept here so the module can be copied alone.
# downloader__ddb_exec {{{
downloader__ddb_exec <- function(conn, sql) {
    duckdb::sql_exec(sql, conn = conn)
}
# }}}

# Download-local ddb query; kept here so the module can be copied alone.
# downloader__ddb_query {{{
downloader__ddb_query <- function(conn, sql) {
    duckdb::sql_query(sql, conn = conn)
}
# }}}

# Download-local ddb read table; kept here so the module can be copied alone.
# downloader__ddb_read_table {{{
downloader__ddb_read_table <- function(conn, table) {
    downloader__ddb_query(
        conn,
        sprintf("SELECT * FROM %s", downloader__ddb_ident(conn, table))
    )
}
# }}}

# Download-local ddb append table; kept here so the module can be copied alone.
# downloader__ddb_append_table {{{
downloader__ddb_append_table <- function(conn, table, rows, ...) {
    duckdb::dbAppendTable(conn, table, rows, ...)
}
# }}}

# Download-local ddb ident; kept here so the module can be copied alone.
# downloader__ddb_ident {{{
downloader__ddb_ident <- function(conn, x) {
    as.character(duckdb::dbQuoteIdentifier(conn, x))
}
# }}}

# Download-local ddb literal; kept here so the module can be copied alone.
# downloader__ddb_literal {{{
downloader__ddb_literal <- function(conn, x) {
    as.character(duckdb::dbQuoteLiteral(conn, x))
}
# }}}

# Download-local manifest lock path; kept here so the module can be copied alone.
# downloader__manifest_lock_path {{{
downloader__manifest_lock_path <- function(path) {
    paste0(normalizePath(path, mustWork = FALSE, winslash = "/"), ".lock")
}
# }}}

# Download-local manifest lock metadata; kept here so the module can be copied alone.
# downloader__manifest_lock_metadata {{{
downloader__manifest_lock_metadata <- function(lock_dir) {
    file.path(lock_dir, "owner.json")
}
# }}}

# Read only ownership records whose host and PID can be validated.
# downloader__manifest_lock_owner {{{
downloader__manifest_lock_owner <- function(lock_dir) {
    owner <- tryCatch(
        suppressWarnings(jsonlite::read_json(
            downloader__manifest_lock_metadata(lock_dir)
        )),
        error = function(error) NULL
    )
    if (
        !is.list(owner) ||
            length(owner$pid) != 1L ||
            !is.numeric(owner$pid) ||
            is.na(owner$pid) ||
            owner$pid <= 0 ||
            owner$pid > .Machine$integer.max ||
            owner$pid != as.integer(owner$pid) ||
            !is.character(owner$hostname) ||
            length(owner$hostname) != 1L ||
            is.na(owner$hostname) ||
            !nzchar(owner$hostname)
    ) {
        return(NULL)
    }
    owner
}
# }}}

# Prove that an owner is absent on this host. Unknown hosts, malformed PIDs and
# failed process queries are conservative: they never authorize takeover.
# downloader__local_pid_absent {{{
downloader__local_pid_absent <- function(pid, hostname) {
    if (
        length(pid) != 1L ||
            !is.numeric(pid) ||
            is.na(pid) ||
            pid <= 0 ||
            pid > .Machine$integer.max ||
            pid != as.integer(pid) ||
            !is.character(hostname) ||
            length(hostname) != 1L ||
            is.na(hostname)
    ) {
        return(FALSE)
    }
    if (!identical(hostname, unname(Sys.info()[["nodename"]]))) {
        return(FALSE)
    }
    if (pid == Sys.getpid()) {
        return(FALSE)
    }
    pid <- as.integer(pid)
    # An unreaped process may retain its PID after releasing its resources.
    tryCatch(
        ps::ps_status(ps::ps_handle(pid)) %in% c("dead", "zombie"),
        no_such_process = function(error) TRUE,
        os_error = function(error) {
            if (
                !identical(Sys.info()[["sysname"]], "Linux") ||
                    !identical(error$errno, 2L)
            ) {
                return(FALSE)
            }
            # On Linux ps_handle() reports ENOENT for an absent /proc entry.
            # A restricted procfs can hide live processes, so confirm ESRCH
            # with a zero signal, which cannot stop or alter a process.
            output <- tryCatch(
                suppressWarnings(system2(
                    "kill",
                    c("-0", pid),
                    stdout = TRUE,
                    stderr = TRUE,
                    env = "LC_ALL=C"
                )),
                error = function(error) NULL
            )
            identical(attr(output, "status"), 1L) &&
                any(endsWith(output, ": No such process"))
        },
        error = function(error) FALSE
    )
}
# }}}

# A valid local owner outlives any age threshold. Only a positively observed
# absent PID permits immediate recovery; permission/status errors remain locked.
# The former stale_after argument is retained for existing callers.
# downloader__manifest_lock_stale {{{
downloader__manifest_lock_stale <- function(lock_dir, stale_after) {
    if (!dir.exists(lock_dir)) {
        return(FALSE)
    }
    owner <- downloader__manifest_lock_owner(lock_dir)
    !is.null(owner) && downloader__local_pid_absent(owner$pid, owner$hostname)
}
# }}}

# Serialize orphan reclamation, then recheck ownership after winning the guard.
# A second waiter must never remove a lock installed by the first waiter. Keep
# the old ownership record for diagnosis; never touch climate/download payloads.
# downloader__manifest_reclaim_lock {{{
downloader__manifest_reclaim_lock <- function(lock_dir, stale_after) {
    guard <- paste0(lock_dir, ".reclaim")
    if (!dir.create(guard, showWarnings = FALSE)) {
        return(FALSE)
    }
    on.exit(unlink(guard, recursive = TRUE), add = TRUE)
    if (!downloader__manifest_lock_stale(lock_dir, stale_after)) {
        return(FALSE)
    }
    archived <- tempfile(
        pattern = paste0(basename(lock_dir), ".orphan-"),
        tmpdir = dirname(lock_dir)
    )
    file.rename(lock_dir, archived)
}
# }}}

# Download-local manifest acquire lock; kept here so the module can be copied alone.
# downloader__manifest_acquire_lock {{{
downloader__manifest_acquire_lock <- function(
    path,
    timeout = 30,
    stale_after = 24 * 3600
) {
    lock_dir <- downloader__manifest_lock_path(path)
    checkmate::assert_count(timeout, positive = FALSE)
    checkmate::assert_count(stale_after, positive = TRUE)
    started <- Sys.time()

    repeat {
        ok <- dir.create(lock_dir, showWarnings = FALSE)
        if (isTRUE(ok)) {
            meta <- list(
                pid = Sys.getpid(),
                hostname = unname(Sys.info()[["nodename"]]),
                token = basename(tempfile("lock-owner-")),
                created_at = format(
                    Sys.time(),
                    "%Y-%m-%dT%H:%M:%SZ",
                    tz = "UTC"
                )
            )
            # A failed or truncated owner write must not grant the lock. The
            # release closure cannot identify such a directory later, leaving
            # an ownerless lock that must be investigated.
            publication_error <- tryCatch(
                {
                    jsonlite::write_json(
                        meta,
                        downloader__manifest_lock_metadata(lock_dir),
                        auto_unbox = TRUE,
                        pretty = TRUE
                    )
                    current <- downloader__manifest_lock_owner(lock_dir)
                    if (!identical(current, meta)) {
                        stop("Manifest lock owner readback did not match.")
                    }
                    NULL
                },
                error = identity
            )
            if (inherits(publication_error, "error")) {
                owner_path <- downloader__manifest_lock_metadata(lock_dir)
                current <- if (file.exists(owner_path)) {
                    downloader__manifest_lock_owner(lock_dir)
                } else {
                    NULL
                }
                entries <- list.files(lock_dir, all.files = TRUE, no.. = TRUE)
                # This process just created the directory. Remove only its
                # failed owner record; unexpected files or another valid
                # owner remain available for explicit investigation.
                if (
                    (is.null(current) || identical(current, meta)) &&
                        all(entries %in% "owner.json")
                ) {
                    unlink(lock_dir, recursive = TRUE, force = TRUE)
                }
                cli::cli_abort(
                    "Cannot publish manifest lock ownership: {.path {lock_dir}}.",
                    parent = publication_error
                )
            }
            return(function() {
                current <- downloader__manifest_lock_owner(lock_dir)
                if (!is.null(current) && identical(current, meta)) {
                    unlink(lock_dir, recursive = TRUE, force = TRUE)
                }
                invisible(NULL)
            })
        }

        if (
            downloader__manifest_lock_stale(lock_dir, stale_after) &&
                downloader__manifest_reclaim_lock(lock_dir, stale_after)
        ) {
            next
        }

        elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
        if (elapsed >= timeout) {
            cli::cli_abort(
                "Manifest is locked by another process: {.path {lock_dir}}."
            )
        }
        Sys.sleep(min(0.2, max(0.01, timeout - elapsed)))
    }
}
# }}}

# Download-local manifest with lock; kept here so the module can be copied alone.
# downloader__manifest_with_lock {{{
downloader__manifest_with_lock <- function(
    path,
    expr,
    timeout = 30,
    stale_after = 24 * 3600
) {
    release <- downloader__manifest_acquire_lock(
        path,
        timeout = timeout,
        stale_after = stale_after
    )
    on.exit(release(), add = TRUE)
    force(expr)
}
# }}}

# downloader__verify_checksum
# downloader__verify_checksum {{{
downloader__verify_checksum <- function(file, checksum, algo = "sha256") {
    checkmate::assert_file_exists(file)
    checkmate::assert_string(checksum)
    checkmate::assert_choice(algo, c("md5", "sha256"))

    actual <- downloader__checksum_file(file, algo)

    tolower(actual) == tolower(checksum)
}
# }}}
# downloader__format_bytes
# downloader__format_bytes {{{
downloader__format_bytes <- function(bytes) {
    if (is.na(bytes) || bytes < 0) {
        return("? B")
    }

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
# FileStatus
#' File Download Status
#'
#' Internal enumeration for file download states
#'
#' @keywords internal
#' @noRd
FileStatus <- list(
    Missing = 0L, # File does not exist
    Downloading = 1L, # .part file exists (download in progress or interrupted)
    Downloaded = 2L, # .done file exists (download complete, not verified)
    Verified = 3L # Final file exists and verified
)
# DownloadStatus
#' File Download Status
#'
#' Internal enumeration for file download states
#'
#' @keywords internal
#' @noRd
DownloadStatus <- list(
    Pending = 0L, # Download task is pending
    Downloading = 1L, # Download is in progress
    Completed = 2L, # Download is completed
    Failed = 3L # Download failed
)
# DownloadTask
#' Download Task Tracker
#'
#' Internal class for tracking async download tasks
#'
#' @keywords internal
#' @noRd
# DownloadTask {{{
DownloadTask <- R6::R6Class(
    "DownloadTask",
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

        # initialize {{{
        initialize = function(url, filename) {
            self$url <- url
            self$filename <- filename
            self$started_at <- Sys.time()
        },
        # }}}

        # update_progress {{{
        update_progress = function(current, total = NULL) {
            self$progress <- current
            if (!is.null(total)) self$total <- total
        },
        # }}}

        # mark_completed {{{
        mark_completed = function() {
            self$status <- DownloadStatus$Completed
            self$completed_at <- Sys.time()
        },
        # }}}

        # mark_failed {{{
        mark_failed = function(error) {
            self$status <- DownloadStatus$Failed
            self$error <- error
            self$completed_at <- Sys.time()
        }
        # }}}
    )
)
# }}}
# downloader utilities
DOWNLOADER_SCHEMA_VERSION <- "1.2.0"
DOWNLOADER_TASK_STATUS <- c(
    "queued",
    "downloading",
    "done",
    "error",
    "cancelled",
    "skipped"
)
DOWNLOADER_JOB_STATUS <- c(
    "queued",
    "running",
    "done",
    "error",
    "cancelled",
    "stopping",
    "stale"
)
DOWNLOADER_DAEMON_STATUS <- c(
    "starting",
    "running",
    "stopping",
    "stopped",
    "error",
    "stale"
)
DOWNLOADER_CONTROL_STATUS <- c("queued", "handled", "error")
DOWNLOADER_PROGRESS_INTERVAL <- 1
DOWNLOADER_JOB_LEASE_SECONDS <- 60
DOWNLOADER_NODE_POLICY_DEFAULT <- list(
    cooldown_after_failures = 3L,
    cooldown_seconds = 3600L,
    history_ttl_seconds = 14L * 24L * 3600L,
    min_attempts = 2L
)
DOWNLOADER_TRANSFER_POLICY_DEFAULT <- list(
    chunk_size = NULL,
    bandwidth_limit = NULL,
    low_speed_limit = NULL,
    low_speed_time = NULL,
    range_mode = "off",
    piece_size = 16L * 1024L^2L,
    piece_concurrency = 4L,
    max_sources = 4L,
    require_checksum_for_multisource = TRUE,
    range_probe_timeout = 30L
)
DOWNLOADER_RESOURCE_POLICY_DEFAULT <- list(
    host_concurrency = NULL,
    disk_preflight = TRUE,
    min_free_space = 0
)
DOWNLOADER_CALLBACK_EVENTS <- c(
    "session_start",
    "task_start",
    "task_progress",
    "candidate_error",
    "task_done",
    "task_error",
    "task_cancelled",
    "session_done"
)
DOWNLOADER_SESSION_STARTED_AT <- Sys.time()

# downloader__now {{{
downloader__now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}
# }}}

# downloader__hash {{{
downloader__hash <- function(...) {
    values <- vapply(
        list(...),
        function(x) {
            if (is.null(x) || length(x) == 0L || all(is.na(x))) {
                return("")
            }
            paste(as.character(x), collapse = "\r")
        },
        character(1L)
    )
    downloader__checksum_bytes(
        charToRaw(paste(values, collapse = "\n")),
        "sha256"
    )
}
# }}}

# downloader__id {{{
downloader__id <- function(prefix) {
    stamp <- format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")
    paste0(
        prefix,
        "-",
        stamp,
        "-",
        substr(downloader__hash(prefix, stamp, stats::runif(1L)), 1L, 10L)
    )
}
# }}}

# downloader__hostname {{{
downloader__hostname <- function() {
    host <- unname(Sys.info()[["nodename"]])
    if (is.na(host) || !nzchar(host)) "localhost" else host
}
# }}}

# downloader__rscript {{{
downloader__rscript <- function() {
    file.path(R.home("bin"), "Rscript")
}
# }}}

# Check process existence without delivering a signal on Windows: pskill()
# calls TerminateProcess there, even when the caller supplies signal zero.
# downloader__pid_alive {{{
downloader__pid_alive <- function(pid) {
    pid <- suppressWarnings(as.integer(pid[[1L]]))
    if (is.na(pid) || pid <= 0L) {
        return(FALSE)
    }
    if (pid == Sys.getpid()) {
        return(TRUE)
    }
    if (.Platform$OS.type == "windows") {
        rows <- tryCatch(
            suppressWarnings(system2(
                "tasklist",
                c("/FI", shQuote(paste("PID eq", pid)), "/FO", "CSV", "/NH"),
                stdout = TRUE,
                stderr = FALSE
            )),
            error = function(error) character()
        )
        # CSV field positions are stable across Windows display languages.
        return(any(grepl(sprintf('^"[^"]*","%d",', pid), rows)))
    }
    out <- tryCatch(tools::pskill(pid, 0), error = function(e) FALSE)
    isTRUE(out)
}
# }}}

# downloader__pid_kill {{{
downloader__pid_kill <- function(pid) {
    pid <- suppressWarnings(as.integer(pid[[1L]]))
    if (is.na(pid) || pid <= 0L || pid == Sys.getpid()) {
        return(FALSE)
    }
    tryCatch(tools::pskill(pid), error = function(e) FALSE)
}
# }}}

# downloader__bool {{{
downloader__bool <- function(x) {
    isTRUE(x)
}
# }}}

# downloader__one_chr {{{
downloader__one_chr <- function(x) {
    if (is.null(x) || length(x) == 0L) {
        return(NA_character_)
    }
    out <- as.character(x[[1L]])
    if (is.na(out) || !nzchar(out)) NA_character_ else out
}
# }}}

# downloader__absolute_path {{{
downloader__absolute_path <- function(path) {
    grepl("^(/|[A-Za-z]:[/\\\\])", path)
}
# }}}

# downloader__resolve_path {{{
downloader__resolve_path <- function(path, base = getwd()) {
    if (is.null(path) || is.na(path) || !nzchar(path)) {
        return(NULL)
    }
    path <- path.expand(path)
    if (!downloader__absolute_path(path)) {
        path <- file.path(base, path)
    }
    normalizePath(path, mustWork = FALSE, winslash = "/")
}
# }}}

# downloader__checksum_type {{{
downloader__checksum_type <- function(type) {
    type <- tolower(downloader__one_chr(type))
    if (is.na(type) || !type %in% c("md5", "sha256")) "sha256" else type
}
# }}}

# downloader__df {{{
downloader__df <- function(..., check.names = FALSE) {
    data.frame(..., stringsAsFactors = FALSE, check.names = check.names)
}
# }}}

# downloader__as_df {{{
downloader__as_df <- function(x) {
    if (is.null(x)) {
        return(data.frame())
    }
    as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
}
# }}}

# downloader__rbind_fill {{{
downloader__rbind_fill <- function(rows) {
    rows <- rows[vapply(rows, function(x) !is.null(x) && nrow(x), logical(1L))]
    if (!length(rows)) {
        return(data.frame())
    }
    cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
    rows <- lapply(rows, function(row) {
        missing <- setdiff(cols, names(row))
        for (col in missing) {
            row[[col]] <- NA
        }
        row[, cols, drop = FALSE]
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}
# }}}

# downloader__sql_in {{{
downloader__sql_in <- function(conn, column, values) {
    values <- values[!is.na(values)]
    if (!length(values)) {
        return("FALSE")
    }
    sprintf(
        "%s IN (%s)",
        downloader__ddb_ident(conn, column),
        paste(downloader__ddb_literal(conn, values), collapse = ", ")
    )
}
# }}}

# Validate persisted configuration locally so copying the module needs no schema file.
# downloader__config_validate {{{
downloader__config_validate <- function(config, name = "downloader config") {
    checkmate::assert_list(config, names = "unique", .var.name = name)
    checkmate::assert_names(
        names(config),
        must.include = c(
            "schema_version",
            "dest",
            "temp",
            "retries",
            "timeout",
            "cleanup",
            "n_workers"
        ),
        .var.name = name
    )
    for (field in c("schema_version", "dest", "temp")) {
        checkmate::assert_string(
            config[[field]],
            .var.name = paste0(name, "$", field)
        )
    }
    for (field in c("manifest", "proxy", "useragent")) {
        checkmate::assert_string(
            config[[field]],
            null.ok = TRUE,
            .var.name = paste0(name, "$", field)
        )
    }
    for (field in c("retries", "timeout")) {
        checkmate::assert_count(
            config[[field]],
            positive = TRUE,
            .var.name = paste0(name, "$", field)
        )
    }
    checkmate::assert_count(config$n_workers, positive = FALSE)
    checkmate::assert_flag(config$cleanup)
    if ("ssl_verifypeer" %in% names(config)) {
        checkmate::assert_flag(config$ssl_verifypeer)
    }
    if (!is.null(config$connect_timeout)) {
        checkmate::assert_count(config$connect_timeout, positive = TRUE)
    }
    # Policies retain the same persisted field contract without a package schema file.
    for (field in c("node_policy", "transfer_policy", "resource_policy")) {
        if (!field %in% names(config)) {
            next
        }
        policy <- config[[field]]
        defaults <- switch(
            field,
            node_policy = DOWNLOADER_NODE_POLICY_DEFAULT,
            transfer_policy = DOWNLOADER_TRANSFER_POLICY_DEFAULT,
            resource_policy = DOWNLOADER_RESOURCE_POLICY_DEFAULT
        )
        checkmate::assert_list(policy, names = "unique")
        checkmate::assert_names(names(policy), must.include = names(defaults))
        if (field == "node_policy") {
            for (key in names(defaults)) {
                checkmate::assert_count(policy[[key]], positive = TRUE)
            }
        } else if (field == "transfer_policy") {
            for (key in c(
                "chunk_size",
                "bandwidth_limit",
                "low_speed_limit",
                "low_speed_time"
            )) {
                if (!is.null(policy[[key]])) {
                    checkmate::assert_count(policy[[key]], positive = TRUE)
                }
            }
            for (key in c(
                "piece_size",
                "piece_concurrency",
                "max_sources",
                "range_probe_timeout"
            )) {
                checkmate::assert_count(policy[[key]], positive = TRUE)
            }
            checkmate::assert_choice(
                policy$range_mode,
                c("off", "single", "multi", "auto")
            )
            checkmate::assert_flag(policy$require_checksum_for_multisource)
        } else {
            if (!is.null(policy$host_concurrency)) {
                checkmate::assert_count(
                    policy$host_concurrency,
                    positive = TRUE
                )
            }
            checkmate::assert_flag(policy$disk_preflight)
            checkmate::assert_number(
                policy$min_free_space,
                lower = 0,
                finite = TRUE
            )
        }
    }
    invisible(config)
}
# }}}

# downloader__config_defaults {{{
downloader__config_defaults <- function(config) {
    if (!"ssl_verifypeer" %in% names(config)) {
        config$ssl_verifypeer <- TRUE
    }
    if (!"proxy" %in% names(config)) {
        config$proxy <- NULL
    }
    if (!"connect_timeout" %in% names(config)) {
        config$connect_timeout <- NULL
    }
    if (!"useragent" %in% names(config)) {
        config$useragent <- NULL
    }
    if (!"node_policy" %in% names(config)) {
        config$node_policy <- DOWNLOADER_NODE_POLICY_DEFAULT
    }
    if (!"transfer_policy" %in% names(config)) {
        config$transfer_policy <- DOWNLOADER_TRANSFER_POLICY_DEFAULT
    }
    if (!"resource_policy" %in% names(config)) {
        config$resource_policy <- DOWNLOADER_RESOURCE_POLICY_DEFAULT
    }
    config$node_policy <- downloader__node_policy_defaults(config$node_policy)
    config$transfer_policy <- downloader__transfer_policy_defaults(
        config$transfer_policy
    )
    config$resource_policy <- downloader__resource_policy_defaults(
        config$resource_policy
    )
    config
}
# }}}

# downloader__node_policy_defaults {{{
downloader__node_policy_defaults <- function(policy = NULL) {
    if (is.null(policy)) {
        policy <- list()
    }
    checkmate::assert_list(policy, names = "unique")
    defaults <- DOWNLOADER_NODE_POLICY_DEFAULT
    for (name in names(defaults)) {
        if (!name %in% names(policy) || is.null(policy[[name]])) {
            policy[[name]] <- defaults[[name]]
        }
    }
    checkmate::assert_count(policy$cooldown_after_failures, positive = TRUE)
    checkmate::assert_count(policy$cooldown_seconds, positive = TRUE)
    checkmate::assert_count(policy$history_ttl_seconds, positive = TRUE)
    checkmate::assert_count(policy$min_attempts, positive = TRUE)
    policy$cooldown_after_failures <- as.integer(policy$cooldown_after_failures)
    policy$cooldown_seconds <- as.integer(policy$cooldown_seconds)
    policy$history_ttl_seconds <- as.integer(policy$history_ttl_seconds)
    policy$min_attempts <- as.integer(policy$min_attempts)
    policy
}
# }}}

# downloader__nullable_count {{{
downloader__nullable_count <- function(value, positive = TRUE, name = "value") {
    if (is.null(value)) {
        return(NULL)
    }
    checkmate::assert_count(value, positive = positive, .var.name = name)
    as.numeric(value)
}
# }}}

# downloader__transfer_policy_defaults {{{
downloader__transfer_policy_defaults <- function(policy = NULL) {
    if (is.null(policy)) {
        policy <- list()
    }
    checkmate::assert_list(policy, names = "unique")
    defaults <- DOWNLOADER_TRANSFER_POLICY_DEFAULT
    for (name in names(defaults)) {
        if (!name %in% names(policy) || is.null(policy[[name]])) {
            policy[name] <- list(defaults[[name]])
        }
    }
    policy["chunk_size"] <- list(downloader__nullable_count(
        policy$chunk_size,
        name = "transfer_policy$chunk_size"
    ))
    policy["bandwidth_limit"] <- list(downloader__nullable_count(
        policy$bandwidth_limit,
        name = "transfer_policy$bandwidth_limit"
    ))
    policy["low_speed_limit"] <- list(downloader__nullable_count(
        policy$low_speed_limit,
        name = "transfer_policy$low_speed_limit"
    ))
    policy["low_speed_time"] <- list(downloader__nullable_count(
        policy$low_speed_time,
        name = "transfer_policy$low_speed_time"
    ))
    policy$range_mode <- match.arg(
        downloader__one_chr(policy$range_mode),
        c("off", "single", "multi", "auto")
    )
    policy["piece_size"] <- list(downloader__nullable_count(
        policy$piece_size,
        name = "transfer_policy$piece_size"
    ))
    policy["piece_concurrency"] <- list(downloader__nullable_count(
        policy$piece_concurrency,
        name = "transfer_policy$piece_concurrency"
    ))
    policy["max_sources"] <- list(downloader__nullable_count(
        policy$max_sources,
        name = "transfer_policy$max_sources"
    ))
    checkmate::assert_flag(policy$require_checksum_for_multisource)
    policy["range_probe_timeout"] <- list(downloader__nullable_count(
        policy$range_probe_timeout,
        name = "transfer_policy$range_probe_timeout"
    ))
    policy$piece_size <- as.integer(policy$piece_size)
    policy$piece_concurrency <- as.integer(policy$piece_concurrency)
    policy$max_sources <- as.integer(policy$max_sources)
    policy$range_probe_timeout <- as.integer(policy$range_probe_timeout)
    policy
}
# }}}

# downloader__resource_policy_defaults {{{
downloader__resource_policy_defaults <- function(policy = NULL) {
    if (is.null(policy)) {
        policy <- list()
    }
    checkmate::assert_list(policy, names = "unique")
    defaults <- DOWNLOADER_RESOURCE_POLICY_DEFAULT
    for (name in names(defaults)) {
        if (!name %in% names(policy) || is.null(policy[[name]])) {
            policy[name] <- list(defaults[[name]])
        }
    }
    policy["host_concurrency"] <- list(downloader__nullable_count(
        policy$host_concurrency,
        name = "resource_policy$host_concurrency"
    ))
    checkmate::assert_flag(policy$disk_preflight)
    checkmate::assert_number(policy$min_free_space, lower = 0, finite = TRUE)
    policy$min_free_space <- as.numeric(policy$min_free_space)
    policy
}
# }}}

# downloader__config_optional_integer {{{
downloader__config_optional_integer <- function(value) {
    if (is.null(value) || !length(value) || is.na(value[[1L]])) {
        return(NA_integer_)
    }
    as.integer(value[[1L]])
}
# }}}

# downloader__config_optional_number {{{
downloader__config_optional_number <- function(value) {
    if (is.null(value) || !length(value) || is.na(value[[1L]])) {
        return(NA_real_)
    }
    as.numeric(value[[1L]])
}
# }}}

# downloader__config_optional_character {{{
downloader__config_optional_character <- function(value) {
    value <- downloader__one_chr(value)
    if (is.na(value)) NA_character_ else value
}
# }}}

# downloader__config_null_if_na {{{
downloader__config_null_if_na <- function(
    value,
    type = c("character", "integer", "double", "logical")
) {
    type <- match.arg(type)
    if (is.null(value) || !length(value) || is.na(value[[1L]])) {
        return(NULL)
    }
    switch(
        type,
        character = as.character(value[[1L]]),
        integer = as.integer(value[[1L]]),
        double = as.numeric(value[[1L]]),
        logical = as.logical(value[[1L]])
    )
}
# }}}

# downloader__config_flatten {{{
downloader__config_flatten <- function(config) {
    config <- downloader__config_defaults(config)
    downloader__config_validate(config)
    downloader__df(
        config_id = "default",
        schema_version = DOWNLOADER_SCHEMA_VERSION,
        dest = config$dest,
        temp = config$temp,
        retries = as.integer(config$retries),
        timeout = as.integer(config$timeout),
        ssl_verifypeer = isTRUE(config$ssl_verifypeer),
        proxy = downloader__config_optional_character(config$proxy),
        connect_timeout = downloader__config_optional_integer(
            config$connect_timeout
        ),
        useragent = downloader__config_optional_character(config$useragent),
        cleanup = isTRUE(config$cleanup),
        n_workers = as.integer(config$n_workers),
        node_cooldown_after_failures = as.integer(
            config$node_policy$cooldown_after_failures
        ),
        node_cooldown_seconds = as.integer(config$node_policy$cooldown_seconds),
        node_history_ttl_seconds = as.integer(
            config$node_policy$history_ttl_seconds
        ),
        node_min_attempts = as.integer(config$node_policy$min_attempts),
        transfer_chunk_size = downloader__config_optional_number(
            config$transfer_policy$chunk_size
        ),
        transfer_bandwidth_limit = downloader__config_optional_number(
            config$transfer_policy$bandwidth_limit
        ),
        transfer_low_speed_limit = downloader__config_optional_number(
            config$transfer_policy$low_speed_limit
        ),
        transfer_low_speed_time = downloader__config_optional_number(
            config$transfer_policy$low_speed_time
        ),
        transfer_range_mode = as.character(config$transfer_policy$range_mode),
        transfer_piece_size = as.integer(config$transfer_policy$piece_size),
        transfer_piece_concurrency = as.integer(
            config$transfer_policy$piece_concurrency
        ),
        transfer_max_sources = as.integer(config$transfer_policy$max_sources),
        transfer_require_checksum_for_multisource = isTRUE(
            config$transfer_policy$require_checksum_for_multisource
        ),
        transfer_range_probe_timeout = as.integer(
            config$transfer_policy$range_probe_timeout
        ),
        resource_host_concurrency = downloader__config_optional_integer(
            config$resource_policy$host_concurrency
        ),
        resource_disk_preflight = isTRUE(config$resource_policy$disk_preflight),
        resource_min_free_space = as.numeric(
            config$resource_policy$min_free_space
        ),
        updated_at = downloader__now()
    )
}
# }}}

# downloader__config_unflatten {{{
downloader__config_unflatten <- function(row, manifest = NULL) {
    if (is.null(row) || !nrow(row)) {
        return(NULL)
    }
    row <- row[1L, , drop = FALSE]
    config <- list(
        schema_version = as.character(row$schema_version[[1L]]),
        dest = as.character(row$dest[[1L]]),
        temp = as.character(row$temp[[1L]]),
        manifest = manifest,
        retries = as.integer(row$retries[[1L]]),
        timeout = as.integer(row$timeout[[1L]]),
        ssl_verifypeer = isTRUE(row$ssl_verifypeer[[1L]]),
        proxy = downloader__config_null_if_na(row$proxy, "character"),
        connect_timeout = downloader__config_null_if_na(
            row$connect_timeout,
            "integer"
        ),
        useragent = downloader__config_null_if_na(row$useragent, "character"),
        cleanup = isTRUE(row$cleanup[[1L]]),
        n_workers = as.integer(row$n_workers[[1L]]),
        node_policy = list(
            cooldown_after_failures = as.integer(row$node_cooldown_after_failures[[
                1L
            ]]),
            cooldown_seconds = as.integer(row$node_cooldown_seconds[[1L]]),
            history_ttl_seconds = as.integer(row$node_history_ttl_seconds[[
                1L
            ]]),
            min_attempts = as.integer(row$node_min_attempts[[1L]])
        ),
        transfer_policy = list(
            chunk_size = downloader__config_null_if_na(
                row$transfer_chunk_size,
                "double"
            ),
            bandwidth_limit = downloader__config_null_if_na(
                row$transfer_bandwidth_limit,
                "double"
            ),
            low_speed_limit = downloader__config_null_if_na(
                row$transfer_low_speed_limit,
                "double"
            ),
            low_speed_time = downloader__config_null_if_na(
                row$transfer_low_speed_time,
                "double"
            ),
            range_mode = downloader__config_null_if_na(
                row$transfer_range_mode,
                "character"
            ),
            piece_size = downloader__config_null_if_na(
                row$transfer_piece_size,
                "integer"
            ),
            piece_concurrency = downloader__config_null_if_na(
                row$transfer_piece_concurrency,
                "integer"
            ),
            max_sources = downloader__config_null_if_na(
                row$transfer_max_sources,
                "integer"
            ),
            require_checksum_for_multisource = downloader__config_null_if_na(
                row$transfer_require_checksum_for_multisource,
                "logical"
            ),
            range_probe_timeout = downloader__config_null_if_na(
                row$transfer_range_probe_timeout,
                "integer"
            )
        ),
        resource_policy = list(
            host_concurrency = downloader__config_null_if_na(
                row$resource_host_concurrency,
                "integer"
            ),
            disk_preflight = isTRUE(row$resource_disk_preflight[[1L]]),
            min_free_space = as.numeric(row$resource_min_free_space[[1L]])
        )
    )
    config <- downloader__config_defaults(config)
    downloader__config_validate(config)
    config
}
# }}}

# downloader__url_host {{{
downloader__url_host <- function(url) {
    url <- downloader__one_chr(url)
    if (is.na(url) || !grepl("^https?://", url)) {
        return(NA_character_)
    }
    sub("^https?://([^/:?#]+).*$", "\\1", url)
}
# }}}

# downloader__existing_dir {{{
downloader__existing_dir <- function(path) {
    path <- normalizePath(path.expand(path), mustWork = FALSE, winslash = "/")
    while (!dir.exists(path)) {
        parent <- dirname(path)
        if (identical(parent, path)) {
            return(NA_character_)
        }
        path <- parent
    }
    normalizePath(path, mustWork = TRUE, winslash = "/")
}
# }}}

# downloader__disk_free_bytes {{{
downloader__disk_free_bytes <- function(path) {
    root <- downloader__existing_dir(path)
    if (is.na(root)) {
        return(NA_real_)
    }
    out <- tryCatch(
        system2("df", c("-Pk", root), stdout = TRUE, stderr = FALSE),
        error = function(e) character()
    )
    if (length(out) < 2L) {
        return(NA_real_)
    }
    fields <- strsplit(trimws(out[[length(out)]]), "[[:space:]]+")[[1L]]
    if (length(fields) < 4L) {
        return(NA_real_)
    }
    available_kb <- suppressWarnings(as.numeric(fields[[4L]]))
    if (is.na(available_kb)) {
        return(NA_real_)
    }
    available_kb * 1024
}
# }}}

# downloader__null_if_empty {{{
downloader__null_if_empty <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    x <- downloader__one_chr(x)
    if (is.na(x) || !nzchar(x)) {
        return(NULL)
    }
    x
}
# }}}

# downloader__curl_handle {{{
downloader__curl_handle <- function(
    timeout,
    connect_timeout = NULL,
    ssl_verifypeer = TRUE,
    proxy = NULL,
    useragent = NULL,
    nobody = FALSE,
    chunk_size = NULL,
    bandwidth_limit = NULL,
    low_speed_limit = NULL,
    low_speed_time = NULL
) {
    handle <- curl::new_handle()
    opts <- list(
        timeout = timeout,
        followlocation = TRUE,
        ssl_verifypeer = isTRUE(ssl_verifypeer)
    )
    if (!is.null(connect_timeout)) {
        opts$connecttimeout <- connect_timeout
    }
    proxy <- downloader__null_if_empty(proxy)
    if (!is.null(proxy)) {
        opts$proxy <- proxy
    }
    useragent <- downloader__null_if_empty(useragent)
    if (!is.null(useragent)) {
        opts$useragent <- useragent
    }
    if (!is.null(chunk_size)) {
        opts$buffersize <- as.integer(chunk_size)
    }
    if (!is.null(bandwidth_limit)) {
        opts$max_recv_speed_large <- as.numeric(bandwidth_limit)
    }
    if (!is.null(low_speed_limit)) {
        opts$low_speed_limit <- as.integer(low_speed_limit)
    }
    if (!is.null(low_speed_time)) {
        opts$low_speed_time <- as.integer(low_speed_time)
    }
    if (isTRUE(nobody)) {
        opts$nobody <- TRUE
    }
    do.call(curl::handle_setopt, c(list(handle = handle), opts))
    handle
}
# }}}

# downloader__headers_text {{{
downloader__headers_text <- function(headers) {
    if (is.null(headers)) {
        return("")
    }
    if (is.raw(headers)) {
        return(rawToChar(headers))
    }
    paste(as.character(headers), collapse = "\n")
}
# }}}

# downloader__resume_supported {{{
downloader__resume_supported <- function(
    url,
    start_byte,
    timeout,
    connect_timeout = NULL,
    ssl_verifypeer = TRUE,
    proxy = NULL,
    useragent = NULL,
    chunk_size = NULL,
    bandwidth_limit = NULL,
    low_speed_limit = NULL,
    low_speed_time = NULL
) {
    if (start_byte <= 0 || !grepl("^https?://", url)) {
        return(TRUE)
    }
    handle <- downloader__curl_handle(
        timeout = min(timeout, 30L),
        connect_timeout = connect_timeout,
        ssl_verifypeer = ssl_verifypeer,
        proxy = proxy,
        useragent = useragent,
        chunk_size = chunk_size,
        bandwidth_limit = bandwidth_limit,
        low_speed_limit = low_speed_limit,
        low_speed_time = low_speed_time,
        nobody = TRUE
    )
    curl::handle_setheaders(handle, Range = sprintf("bytes=%.0f-", start_byte))
    response <- tryCatch(
        curl::curl_fetch_memory(url, handle = handle),
        error = function(e) NULL
    )
    if (
        is.null(response) || !identical(as.integer(response$status_code), 206L)
    ) {
        return(FALSE)
    }
    headers <- tolower(downloader__headers_text(response$headers))
    pattern <- sprintf(
        "content-range:[[:space:]]*bytes[[:space:]]+%.0f-",
        start_byte
    )
    grepl(pattern, headers, perl = TRUE)
}
# }}}

# downloader__format_byte {{{
downloader__format_byte <- function(x) {
    format(as.numeric(x), scientific = FALSE, trim = TRUE)
}
# }}}

# downloader__header_value {{{
downloader__header_value <- function(headers, name) {
    text <- downloader__headers_text(headers)
    if (!nzchar(text)) {
        return(NA_character_)
    }
    lines <- strsplit(text, "\r?\n")[[1L]]
    pattern <- paste0("^[[:space:]]*", tolower(name), "[[:space:]]*:")
    hit <- grep(pattern, tolower(lines), value = TRUE)
    if (!length(hit)) {
        return(NA_character_)
    }
    trimws(sub("^[^:]+:[[:space:]]*", "", hit[[length(hit)]]))
}
# }}}

# downloader__parse_content_range {{{
downloader__parse_content_range <- function(value) {
    value <- downloader__one_chr(value)
    if (is.na(value)) {
        return(NULL)
    }
    match <- regexec(
        "bytes[[:space:]]+([0-9]+)-([0-9]+)/([0-9]+|[*])",
        value,
        ignore.case = TRUE
    )
    parts <- regmatches(value, match)[[1L]]
    if (length(parts) != 4L || identical(parts[[4L]], "*")) {
        return(NULL)
    }
    start <- suppressWarnings(as.numeric(parts[[2L]]))
    end <- suppressWarnings(as.numeric(parts[[3L]]))
    size <- suppressWarnings(as.numeric(parts[[4L]]))
    if (any(is.na(c(start, end, size))) || end < start || size <= 0) {
        return(NULL)
    }
    list(start = start, end = end, size = size)
}
# }}}

# downloader__range_probe_url {{{
downloader__range_probe_url <- function(
    url,
    timeout = 30L,
    connect_timeout = NULL,
    ssl_verifypeer = TRUE,
    proxy = NULL,
    useragent = NULL,
    transfer_policy = NULL
) {
    url <- downloader__one_chr(url)
    if (is.na(url)) {
        return(list(
            range_supported = FALSE,
            range_size = NA_real_,
            range_etag = NA_character_,
            range_last_modified = NA_character_,
            range_final_url = NA_character_,
            range_probe_error = "Missing URL."
        ))
    }
    if (grepl("^file://", url, ignore.case = TRUE)) {
        path <- sub("^file://", "", url, ignore.case = TRUE)
        path <- utils::URLdecode(path)
        exists <- file.exists(path)
        return(list(
            range_supported = exists,
            range_size = if (exists) {
                as.numeric(file.info(path, extra_cols = FALSE)$size)
            } else {
                NA_real_
            },
            range_etag = NA_character_,
            range_last_modified = NA_character_,
            range_final_url = url,
            range_probe_error = if (exists) {
                NA_character_
            } else {
                "Local file does not exist."
            }
        ))
    }
    if (!grepl("^https?://", url, ignore.case = TRUE)) {
        return(list(
            range_supported = FALSE,
            range_size = NA_real_,
            range_etag = NA_character_,
            range_last_modified = NA_character_,
            range_final_url = url,
            range_probe_error = "Unsupported URL scheme."
        ))
    }
    if (is.null(transfer_policy)) {
        transfer_policy <- list()
    }
    if (is.null(connect_timeout)) {
        connect_timeout <- min(timeout, 10L)
    }
    handle <- downloader__curl_handle(
        timeout = timeout,
        connect_timeout = connect_timeout,
        ssl_verifypeer = ssl_verifypeer,
        proxy = proxy,
        useragent = useragent,
        chunk_size = transfer_policy$chunk_size,
        bandwidth_limit = transfer_policy$bandwidth_limit,
        low_speed_limit = transfer_policy$low_speed_limit,
        low_speed_time = transfer_policy$low_speed_time
    )
    curl::handle_setheaders(handle, Range = "bytes=0-0")
    curl::handle_setopt(handle, failonerror = FALSE)
    response <- tryCatch(
        curl::curl_fetch_memory(url, handle = handle),
        error = function(e) e
    )
    if (inherits(response, "error")) {
        return(list(
            range_supported = FALSE,
            range_size = NA_real_,
            range_etag = NA_character_,
            range_last_modified = NA_character_,
            range_final_url = url,
            range_probe_error = conditionMessage(response)
        ))
    }
    final_url <- downloader__one_chr(response$url)
    if (is.na(final_url)) {
        final_url <- url
    }
    parsed <- downloader__parse_content_range(downloader__header_value(
        response$headers,
        "content-range"
    ))
    ok <- identical(as.integer(response$status_code), 206L) &&
        !is.null(parsed) &&
        identical(parsed$start, 0) &&
        identical(parsed$end, 0)
    list(
        range_supported = isTRUE(ok),
        range_size = if (isTRUE(ok)) parsed$size else NA_real_,
        range_etag = downloader__header_value(response$headers, "etag"),
        range_last_modified = downloader__header_value(
            response$headers,
            "last-modified"
        ),
        range_final_url = final_url,
        range_probe_error = if (isTRUE(ok)) {
            NA_character_
        } else {
            "Server did not return a valid 206 Content-Range response."
        }
    )
}
# }}}

# Build the self-contained helper bundle serialized with downloader worker tasks.
# downloader__worker_dependencies {{{
downloader__worker_dependencies <- function() {
    # Normalize tabular worker inputs without requiring package helpers.
    as_df <- function(x) {
        as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    }

    # Read one usable scalar string from worker input or return a missing value.
    one_chr <- function(x) {
        if (is.null(x) || !length(x)) {
            return(NA_character_)
        }
        value <- as.character(x[[1L]])
        if (is.na(value) || !nzchar(value)) NA_character_ else value
    }

    # Normalize optional numeric transfer settings inside an isolated process.
    normalize_count <- function(x) {
        if (is.null(x) || length(x) == 0L || is.na(x)) {
            return(NULL)
        }
        as.numeric(x)
    }

    # Resolve the common worker transfer settings and optional piece concurrency.
    normalize_transfer_policy <- function(
        policy,
        include_piece_concurrency = FALSE
    ) {
        if (is.null(policy)) {
            policy <- list()
        }
        out <- list(
            chunk_size = normalize_count(policy$chunk_size),
            bandwidth_limit = normalize_count(policy$bandwidth_limit),
            low_speed_limit = normalize_count(policy$low_speed_limit),
            low_speed_time = normalize_count(policy$low_speed_time)
        )
        if (isTRUE(include_piece_concurrency)) {
            piece_concurrency <- normalize_count(policy$piece_concurrency)
            if (is.null(piece_concurrency)) {
                piece_concurrency <- 1L
            }
            out$piece_concurrency <- max(1L, as.integer(piece_concurrency))
        }
        out
    }

    # Calculate the checksum algorithms accepted by downloader workers.
    checksum_file <- function(path, algo = "sha256") {
        out <- if (identical(algo, "sha256")) {
            tools::sha256sum(path)
        } else {
            tools::md5sum(path)
        }
        unname(as.character(out))
    }

    # Treat an absent checksum as valid and otherwise verify the completed file.
    verify_checksum <- function(path, expected, algo = "sha256") {
        if (is.null(expected) || is.na(expected) || !nzchar(expected)) {
            return(TRUE)
        }
        if (!file.exists(path)) {
            return(FALSE)
        }
        identical(
            tolower(checksum_file(path, algo)),
            tolower(expected)
        )
    }

    # Convert empty optional worker settings to NULL before curl configuration.
    null_if_empty <- function(x) {
        if (is.null(x)) {
            return(NULL)
        }
        if (length(x) == 0L || is.na(x) || !nzchar(x)) {
            return(NULL)
        }
        x
    }

    # Create a curl handle from the normalized transfer policy used by either worker.
    curl_handle <- function(
        timeout,
        connect_timeout,
        ssl_verifypeer,
        proxy,
        useragent,
        transfer_policy,
        nobody = FALSE
    ) {
        handle <- curl::new_handle()
        opts <- list(
            timeout = timeout,
            followlocation = TRUE,
            ssl_verifypeer = isTRUE(ssl_verifypeer)
        )
        if (!is.null(connect_timeout)) {
            opts$connecttimeout <- connect_timeout
        }
        proxy <- null_if_empty(proxy)
        if (!is.null(proxy)) {
            opts$proxy <- proxy
        }
        useragent <- null_if_empty(useragent)
        if (!is.null(useragent)) {
            opts$useragent <- useragent
        }
        if (!is.null(transfer_policy$chunk_size)) {
            opts$buffersize <- as.integer(transfer_policy$chunk_size)
        }
        if (!is.null(transfer_policy$bandwidth_limit)) {
            opts$max_recv_speed_large <- as.numeric(
                transfer_policy$bandwidth_limit
            )
        }
        if (!is.null(transfer_policy$low_speed_limit)) {
            opts$low_speed_limit <- as.integer(transfer_policy$low_speed_limit)
        }
        if (!is.null(transfer_policy$low_speed_time)) {
            opts$low_speed_time <- as.integer(transfer_policy$low_speed_time)
        }
        if (isTRUE(nobody)) {
            opts$nobody <- TRUE
        }
        do.call(curl::handle_setopt, c(list(handle = handle), opts))
        handle
    }

    # Format byte offsets without scientific notation for HTTP Range headers.
    format_byte <- function(x) {
        format(as.numeric(x), scientific = FALSE, trim = TRUE)
    }

    # Convert raw or character response headers to searchable text.
    headers_text <- function(headers) {
        if (is.raw(headers)) {
            return(rawToChar(headers))
        }
        paste(headers, collapse = "\n")
    }

    # Confirm that an interrupted HTTP transfer can resume at the requested byte.
    resume_supported <- function(
        url,
        start_byte,
        timeout,
        connect_timeout,
        ssl_verifypeer,
        proxy,
        useragent,
        transfer_policy
    ) {
        if (start_byte <= 0 || !grepl("^https?://", url)) {
            return(TRUE)
        }
        handle <- curl_handle(
            timeout = min(timeout, 30L),
            connect_timeout = connect_timeout,
            ssl_verifypeer = ssl_verifypeer,
            proxy = proxy,
            useragent = useragent,
            transfer_policy = transfer_policy,
            nobody = TRUE
        )
        curl::handle_setheaders(
            handle,
            Range = sprintf("bytes=%.0f-", start_byte)
        )
        response <- tryCatch(
            curl::curl_fetch_memory(url, handle = handle),
            error = function(e) NULL
        )
        if (
            is.null(response) ||
                !identical(as.integer(response$status_code), 206L)
        ) {
            return(FALSE)
        }
        pattern <- sprintf(
            "content-range:[[:space:]]*bytes[[:space:]]+%.0f-",
            start_byte
        )
        grepl(pattern, tolower(headers_text(response$headers)), perl = TRUE)
    }

    # Check the actual GET before appending any resumed bytes. HEAD negotiation
    # alone cannot protect a partial when a gateway changes its response later.
    resume_guard <- function(handle, start_byte) {
        stopifnot(
            length(start_byte) == 1L,
            is.finite(start_byte),
            start_byte >= 0,
            start_byte <= 2^53 - 1,
            start_byte == floor(start_byte)
        )
        received <- 0
        expected <- NULL
        validated <- FALSE
        validate <- function(response) {
            if (start_byte == 0 || validated) {
                return(invisible(NULL))
            }
            lines <- strsplit(headers_text(response$headers), "\r?\n")[[1L]]
            # Redirect/proxy response blocks must not supply missing headers for
            # the final GET. Only that final block describes appended bytes.
            status_lines <- grep("^HTTP/", lines)
            if (length(status_lines)) {
                lines <- lines[seq.int(tail(status_lines, 1L), length(lines))]
            }
            value <- function(name) {
                hit <- grep(paste0("^", name, ":[[:space:]]*"), tolower(lines))
                if (!length(hit)) {
                    return(NA_character_)
                }
                trimws(sub("^[^:]+:", "", lines[[tail(hit, 1L)]]))
            }
            text <- value("content-range")
            parts <- if (!is.na(text)) {
                regmatches(
                    text,
                    regexec("^bytes ([0-9]+)-([0-9]+)/([0-9]+)$", text)
                )[[1L]]
            } else {
                character()
            }
            bounds <- suppressWarnings(as.numeric(parts[-1L]))
            valid <- length(bounds) == 3L &&
                all(is.finite(bounds)) &&
                all(bounds <= 2^53 - 1) &&
                bounds[[1L]] == start_byte &&
                bounds[[2L]] >= start_byte &&
                bounds[[2L]] == bounds[[3L]] - 1
            if (!identical(as.integer(response$status_code), 206L) || !valid) {
                stop(
                    "HTTP resume GET did not return the exact remaining Content-Range.",
                    call. = FALSE
                )
            }
            expected <<- bounds[[2L]] - bounds[[1L]] + 1
            length_header <- value("content-length")
            encoding <- value("content-encoding")
            if (
                (!is.na(length_header) &&
                    !identical(
                        suppressWarnings(as.numeric(length_header)),
                        expected
                    )) ||
                    (!is.na(encoding) && tolower(encoding) != "identity")
            ) {
                stop(
                    "HTTP resume GET has inconsistent length or encoding.",
                    call. = FALSE
                )
            }
            validated <<- TRUE
            invisible(NULL)
        }
        # curl exposes the final response headers before its first body callback.
        accept <- function(chunk) {
            if (start_byte == 0) {
                return(invisible(NULL))
            }
            validate(curl::handle_data(handle))
            if (received + length(chunk) > expected) {
                stop(
                    "HTTP resume GET exceeded its declared byte range.",
                    call. = FALSE
                )
            }
            received <<- received + length(chunk)
            invisible(NULL)
        }
        # Keep an interrupted valid prefix, but never seal an incomplete tail.
        finish <- function(response) {
            if (start_byte == 0) {
                return(invisible(NULL))
            }
            validate(response)
            if (!identical(received, expected)) {
                stop(
                    "HTTP resume GET ended before its declared byte range.",
                    call. = FALSE
                )
            }
            invisible(NULL)
        }
        list(accept = accept, finish = finish)
    }

    # Normalize the final actual response, keeping opaque values unchanged and
    # deriving date validators only when no entity tag was provided.
    transfer_response <- function(response, url) {
        lines <- strsplit(headers_text(response$headers), "\r?\n")[[1L]]
        statuses <- grep("^HTTP/", lines)
        if (length(statuses)) {
            lines <- lines[seq.int(tail(statuses, 1L), length(lines))]
        }
        field <- function(name) {
            hit <- grep(paste0("^", name, ":[[:space:]]*"), tolower(lines))
            if (!length(hit)) {
                return(NA_character_)
            }
            trimws(sub("^[^:]+:", "", lines[[tail(hit, 1L)]]))
        }
        etag <- field("etag")
        modified <- field("last-modified")
        date <- field("date")
        kind <- value <- NA_character_
        if (!is.na(etag) && grepl('^"[^"[:cntrl:]]*"$', etag)) {
            kind <- "etag"
            value <- etag
        } else if (is.na(etag) && !is.na(modified) && !is.na(date)) {
            elapsed <- suppressWarnings(
                as.numeric(curl::parse_date(date)) -
                    as.numeric(curl::parse_date(modified))
            )
            if (length(elapsed) == 1L && is.finite(elapsed) && elapsed >= 60) {
                kind <- "last-modified"
                value <- modified
            }
        }
        final_url <- one_chr(response$url)
        if (is.na(final_url)) {
            final_url <- url
        }
        list(
            request_url = url,
            final_url = final_url,
            etag = etag,
            last_modified = modified,
            date = date,
            validator_kind = kind,
            validator_value = value,
            content_range = field("content-range"),
            content_length = field("content-length"),
            content_encoding = field("content-encoding"),
            status = as.integer(response$status_code)
        )
    }

    # Bounded range pieces preserve their concurrency while validating the same
    # actual response identity used by ordinary downloads before any body write.
    piece_guard <- function(handle, url, row, tmp, bind_evidence) {
        state <- NULL
        full_response <- NULL
        received <- 0
        validate <- function(response) {
            if (!is.null(state)) {
                return(invisible(NULL))
            }
            evidence <- transfer_response(response, url)
            # A complete response cannot enter a piece writer. Capture its
            # actual identity and let the caller retire the entire batch before
            # opening the shared zero-based stream.
            if (evidence$status == 200L) {
                evidence$size <- suppressWarnings(as.numeric(
                    evidence$content_length
                ))
                if (
                    !is.na(evidence$content_encoding) &&
                        tolower(evidence$content_encoding) != "identity"
                ) {
                    stop(
                        "HTTP piece GET has incompatible content encoding.",
                        call. = FALSE
                    )
                }
                bind_evidence(evidence)
                full_response <<- evidence
                stop(
                    "HTTP piece GET returned a complete response; restart required.",
                    call. = FALSE
                )
            }
            text <- evidence$content_range
            parts <- if (!is.na(text)) {
                regmatches(
                    text,
                    regexec("^bytes ([0-9]+)-([0-9]+)/([0-9]+)$", text)
                )[[1L]]
            } else {
                character()
            }
            bounds <- suppressWarnings(as.numeric(parts[-1L]))
            if (
                evidence$status != 206L ||
                    length(bounds) != 3L ||
                    any(!is.finite(bounds)) ||
                    any(bounds > 2^53 - 1) ||
                    bounds[[1L]] != row$start_byte[[1L]] ||
                    bounds[[2L]] != row$end_byte[[1L]] ||
                    bounds[[3L]] <= bounds[[2L]]
            ) {
                stop(
                    "HTTP piece GET has an inconsistent Content-Range.",
                    call. = FALSE
                )
            }
            length_header <- suppressWarnings(as.numeric(
                evidence$content_length
            ))
            if (
                (!is.na(evidence$content_length) &&
                    (!is.finite(length_header) ||
                        length_header != row$byte_count[[1L]])) ||
                    (!is.na(evidence$content_encoding) &&
                        tolower(evidence$content_encoding) != "identity")
            ) {
                stop(
                    "HTTP piece GET has inconsistent length or encoding.",
                    call. = FALSE
                )
            }
            evidence$size <- bounds[[3L]]
            bind_evidence(evidence)
            evidence$validator_bound <- !is.na(evidence$validator_kind)
            evidence$generation <- basename(tempfile("piece-"))
            evidence$path <- tmp
            evidence$bytes <- 0
            evidence$sha256 <- NA_character_
            evidence$complete <- FALSE
            evidence$checksum_verified <- FALSE
            transfer_write(tmp, evidence)
            state <<- evidence
            invisible(NULL)
        }
        accept <- function(chunk) {
            validate(curl::handle_data(handle))
            if (received + length(chunk) > row$byte_count[[1L]]) {
                stop(
                    "HTTP piece GET exceeded its declared range.",
                    call. = FALSE
                )
            }
            received <<- received + length(chunk)
            invisible(NULL)
        }
        finish <- function(response) {
            validate(response)
            if (received != row$byte_count[[1L]]) {
                stop(
                    "HTTP piece GET ended before its declared range.",
                    call. = FALSE
                )
            }
            # Each chunk is flushed before completion. Checkpoint its exact
            # retained bytes before the caller closes and seals the piece.
            state$bytes <<- received
            state$sha256 <<- checksum_file(tmp, "sha256")
            transfer_write(tmp, state)
            invisible(NULL)
        }
        list(accept = accept, finish = finish, full_response = function() {
            full_response
        })
    }

    # A sidecar belongs to one generation of retained bytes. Atomic replacement
    # prevents a torn metadata write from assigning a new validator to old bytes.
    transfer_read <- function(tmp_part) {
        if (!file.exists(sub("[.]part$", ".transfer.json", tmp_part))) {
            return(NULL)
        }
        tryCatch(
            jsonlite::read_json(
                sub("[.]part$", ".transfer.json", tmp_part),
                simplifyVector = TRUE
            ),
            error = function(e) NULL
        )
    }

    # Readback is checked before a body callback is allowed to write any bytes.
    transfer_write <- function(tmp_part, state) {
        path <- sub("[.]part$", ".transfer.json", tmp_part)
        staged <- tempfile(paste0(basename(path), "."), dirname(path))
        on.exit(unlink(staged), add = TRUE)
        jsonlite::write_json(
            state,
            staged,
            auto_unbox = TRUE,
            null = "null",
            na = "null",
            digits = NA
        )
        if (
            is.null(tryCatch(jsonlite::read_json(staged), error = function(e) {
                NULL
            })) ||
                !file.rename(staged, path)
        ) {
            stop("Cannot publish HTTP transfer evidence.", call. = FALSE)
        }
        invisible(state)
    }

    # Keep unsafe or superseded data and its evidence together; a new response
    # may only acquire the canonical pathname after the old generation is moved.
    transfer_preserve <- function(tmp_part) {
        suffix <- paste0(".retained-", basename(tempfile()))
        for (path in c(tmp_part, sub("[.]part$", ".transfer.json", tmp_part))) {
            if (file.exists(path) && !file.rename(path, paste0(path, suffix))) {
                stop(
                    "Cannot preserve the previous HTTP transfer generation.",
                    call. = FALSE
                )
            }
        }
        invisible(NULL)
    }

    # Publish complete-file evidence only after the expected publisher checksum
    # has been checked by the caller. Missing checksums remain explicitly false.
    transfer_seal <- function(tmp_part, path, checksum, checksum_type) {
        state <- transfer_read(tmp_part)
        if (is.null(state)) {
            return(invisible(NULL))
        }
        actual_bytes <- as.numeric(file.info(path, extra_cols = FALSE)$size)
        actual_sha256 <- checksum_file(path, "sha256")
        if (is.null(state$sha256) || is.na(state$sha256)) {
            return(invisible(NULL))
        }
        if (
            state$bytes != actual_bytes ||
                !identical(state$sha256, actual_sha256)
        ) {
            stop(
                "source_changed: Retained file does not match its saved transfer checkpoint.",
                call. = FALSE
            )
        }
        state$path <- normalizePath(path, winslash = "/", mustWork = TRUE)
        state$bytes <- as.numeric(file.info(path, extra_cols = FALSE)$size)
        state$sha256 <- actual_sha256
        if (is.null(state$size) || !is.finite(state$size)) {
            state$size <- state$bytes
        }
        state$complete <- TRUE
        state$checksum_verified <- !is.null(null_if_empty(checksum)) &&
            verify_checksum(path, checksum, checksum_type)
        transfer_write(tmp_part, state)
    }

    # Both ordinary and mirai downloads use this byte boundary. Response headers
    # are checked before opening append mode, and only a validated prefix is kept.
    transfer_stream <- function(
        url,
        tmp_part,
        tmp_done,
        timeout,
        start_byte,
        connect_timeout,
        ssl_verifypeer,
        proxy,
        useragent,
        transfer_policy,
        checksum = NULL,
        resume = TRUE,
        on_progress = function(bytes) NULL,
        representation = NULL
    ) {
        http <- grepl("^https?://", url, ignore.case = TRUE)
        checksum <- null_if_empty(checksum)
        prior <- transfer_read(tmp_part)
        current_size <- if (file.exists(tmp_part)) {
            as.numeric(file.info(tmp_part, extra_cols = FALSE)$size)
        } else {
            0
        }
        start_byte <- if (isTRUE(resume)) current_size else 0
        if (current_size == 0) {
            prior <- NULL
        }
        # A checkpoint hashes the file once at attempt close, rather than once
        # per small curl chunk. A crash tail without that checkpoint is retained
        # separately and never gets certified by the older saved hash.
        if (http && current_size > 0) {
            checkpoint_ok <- !is.null(prior) &&
                !is.null(prior$bytes) &&
                identical(as.numeric(prior$bytes), current_size) &&
                !is.null(prior$sha256) &&
                identical(prior$sha256, checksum_file(tmp_part, "sha256"))
            eligible <- !is.null(checksum) ||
                (checkpoint_ok &&
                    identical(prior$request_url, url) &&
                    isTRUE(prior$validator_bound) &&
                    !is.null(null_if_empty(prior$validator_kind)))
            if (
                !isTRUE(resume) ||
                    !eligible ||
                    (!is.null(prior) && !checkpoint_ok)
            ) {
                transfer_preserve(tmp_part)
                prior <- NULL
                start_byte <- 0
            }
        } else if (!isTRUE(resume) && file.exists(tmp_part)) {
            transfer_preserve(tmp_part)
            prior <- NULL
        }
        handle <- curl_handle(
            timeout,
            connect_timeout,
            ssl_verifypeer,
            proxy,
            useragent,
            normalize_transfer_policy(transfer_policy)
        )
        curl::handle_setopt(handle, failonerror = !http)
        if (http) {
            curl::handle_setopt(handle, http_content_decoding = FALSE)
        }
        headers <- list(`Accept-Encoding` = "identity")
        if (start_byte > 0) {
            headers$Range <- sprintf("bytes=%.0f-", start_byte)
            if (
                !is.null(prior) &&
                    identical(prior$request_url, url) &&
                    !is.null(null_if_empty(prior$validator_value))
            ) {
                headers$`If-Range` <- prior$validator_value
            }
            if (!http) {
                curl::handle_setopt(handle, resume_from_large = start_byte)
            }
        }
        do.call(curl::handle_setheaders, c(list(handle = handle), headers))
        state <- prior
        con <- NULL
        validated <- FALSE
        expected <- NA_real_
        received <- 0
        write_start <- start_byte
        range_complete <- FALSE
        changed <- function(message) {
            stop(paste0("source_changed: ", message), call. = FALSE)
        }
        validate <- function(response) {
            if (validated) {
                return(invisible(NULL))
            }
            evidence <- transfer_response(response, url)
            final_url <- evidence$final_url
            status <- evidence$status
            etag <- evidence$etag
            modified <- evidence$last_modified
            date <- evidence$date
            validator_kind <- evidence$validator_kind
            validator_value <- evidence$validator_value
            encoding <- evidence$content_encoding
            length_text <- evidence$content_length
            length_header <- suppressWarnings(as.numeric(length_text))
            if (http && !is.na(encoding) && tolower(encoding) != "identity") {
                stop(
                    "HTTP GET has incompatible content encoding.",
                    call. = FALSE
                )
            }
            total <- length_header
            if (http && status == 416L) {
                text <- evidence$content_range
                size <- if (!is.na(text) && grepl("^bytes [*]/[0-9]+$", text)) {
                    as.numeric(sub("^bytes [*]/", "", text))
                } else {
                    NA_real_
                }
                complete_hash <- !is.null(checksum) &&
                    verify_checksum(
                        tmp_part,
                        checksum,
                        if (nchar(checksum) == 32L) "md5" else "sha256"
                    )
                complete_saved <- !is.null(prior) &&
                    !is.null(prior$size) &&
                    prior$size == start_byte &&
                    isTRUE(prior$validator_bound) &&
                    identical(prior$request_url, url) &&
                    identical(prior$final_url, final_url) &&
                    identical(prior$validator_kind, validator_kind) &&
                    identical(prior$validator_value, validator_value) &&
                    !is.na(validator_kind) &&
                    identical(prior$sha256, checksum_file(tmp_part, "sha256"))
                if (
                    !is.finite(size) ||
                        size != start_byte ||
                        !(complete_hash || complete_saved)
                ) {
                    stop(
                        "HTTP 416 does not establish a complete local representation.",
                        call. = FALSE
                    )
                }
                if (
                    !is.null(prior) &&
                        !is.null(prior$size) &&
                        prior$size != size
                ) {
                    changed("HTTP total length changed.")
                }
                state <<- if (is.null(prior)) {
                    list(
                        generation = basename(tempfile("transfer-")),
                        request_url = url,
                        final_url = final_url,
                        etag = NA_character_,
                        last_modified = NA_character_,
                        date = NA_character_,
                        validator_kind = NA_character_,
                        validator_value = NA_character_,
                        origin = "checksum-existing",
                        validator_bound = FALSE
                    )
                } else {
                    prior
                }
                state$size <<- size
                state$bytes <<- start_byte
                state$sha256 <<- checksum_file(tmp_part, "sha256")
                state$path <<- tmp_part
                state$complete <<- FALSE
                state$checksum_verified <<- FALSE
                state$completion_response <<- evidence
                transfer_write(tmp_part, state)
                range_complete <<- TRUE
                expected <<- 0
                validated <<- TRUE
                return(invisible(NULL))
            }
            if (http && !status %in% c(200L, 206L)) {
                stop(
                    sprintf("HTTP GET failed with status %d.", status),
                    call. = FALSE
                )
            }
            if (http && status == 206L) {
                text <- evidence$content_range
                parts <- if (!is.na(text)) {
                    regmatches(
                        text,
                        regexec("^bytes ([0-9]+)-([0-9]+)/([0-9]+)$", text)
                    )[[1L]]
                } else {
                    character()
                }
                bounds <- suppressWarnings(as.numeric(parts[-1L]))
                if (
                    length(bounds) != 3L ||
                        any(!is.finite(bounds)) ||
                        any(bounds > 2^53 - 1) ||
                        bounds[[1L]] != start_byte ||
                        bounds[[2L]] < start_byte ||
                        bounds[[2L]] != bounds[[3L]] - 1
                ) {
                    stop(
                        "HTTP GET did not return the exact remaining Content-Range.",
                        call. = FALSE
                    )
                }
                total <- bounds[[3L]]
                if (
                    start_byte > 0 &&
                        !is.null(prior$size) &&
                        prior$size != total
                ) {
                    changed("HTTP total length changed.")
                }
                expected <<- bounds[[2L]] - bounds[[1L]] + 1
                if (
                    !is.na(length_text) &&
                        (!is.finite(length_header) || length_header != expected)
                ) {
                    stop(
                        "HTTP GET has inconsistent Content-Length.",
                        call. = FALSE
                    )
                }
            } else {
                expected <<- if (!http && is.finite(length_header)) {
                    length_header - start_byte
                } else {
                    length_header
                }
            }
            if (start_byte > 0 && !is.null(prior)) {
                if (
                    !is.null(prior$size) &&
                        is.finite(total) &&
                        prior$size != total
                ) {
                    changed("HTTP total length changed.")
                }
                if (is.null(checksum)) {
                    if (
                        !identical(prior$request_url, url) ||
                            !identical(prior$final_url, final_url)
                    ) {
                        changed("HTTP resource changed.")
                    }
                    if (
                        !identical(prior$validator_kind, validator_kind) ||
                            !identical(prior$validator_value, validator_value)
                    ) {
                        changed("HTTP validator changed or is missing.")
                    }
                }
            }
            # Segmented recovery carries the original actual-GET anchors into
            # this common byte path. They constrain a fresh GET even though no
            # earlier piece is appended to the new file.
            if (!is.null(representation)) {
                if (is.finite(total) && total != representation$size) {
                    changed(
                        "HTTP total length changed during complete-response recovery."
                    )
                }
                if (is.null(checksum)) {
                    for (anchor in representation$anchors) {
                        if (
                            !identical(anchor$request_url, url) ||
                                !identical(anchor$final_url, final_url) ||
                                is.null(null_if_empty(anchor$validator_kind)) ||
                                !identical(
                                    anchor$validator_kind,
                                    validator_kind
                                ) ||
                                !identical(
                                    anchor$validator_value,
                                    validator_value
                                )
                        ) {
                            changed(
                                "HTTP validator or resource changed during complete-response recovery."
                            )
                        }
                    }
                }
            }
            # A full response to Range always starts a fresh generation. Save
            # the former bytes before publishing the replacement's metadata.
            if (http && start_byte > 0 && status == 200L) {
                if (is.null(checksum) && is.null(prior)) {
                    changed("Cannot verify the full replacement response.")
                }
                transfer_preserve(tmp_part)
                write_start <<- 0
                prior <<- NULL
            }
            # The full-200 decision resets write_start. A chunked replacement
            # must therefore use the entire anchored total, not the old suffix.
            if (!is.null(representation) && !is.finite(expected)) {
                expected <<- representation$size - write_start
            }
            if (is.null(prior)) {
                if (file.exists(tmp_part) && write_start == 0) {
                    transfer_preserve(tmp_part)
                }
                state <<- list(
                    generation = basename(tempfile("transfer-")),
                    bytes = 0,
                    sha256 = NA_character_
                )
            } else {
                state <<- prior
            }
            state$origin <<- if (http) "get" else "file"
            state$request_url <<- url
            state$final_url <<- final_url
            state$etag <<- etag
            state$last_modified <<- modified
            state$date <<- date
            state$validator_kind <<- validator_kind
            state$validator_value <<- validator_value
            state$size <<- if (is.finite(total)) total else NA_real_
            state$path <<- tmp_part
            state$complete <<- FALSE
            state$checksum_verified <<- FALSE
            state$replaces_prior <<- !is.null(representation) ||
                (http && start_byte > 0 && status == 200L)
            if (!is.null(representation)) {
                state$range_fallback <<- representation
            }
            # A resumed GET describes its suffix. Keep the original producer's
            # identity and record a differing replica response separately; only
            # the expected publisher checksum may certify that mixed transfer.
            same_identity <- !is.null(prior) &&
                identical(prior$request_url, url) &&
                identical(prior$final_url, final_url) &&
                identical(one_chr(prior$validator_kind), validator_kind) &&
                identical(one_chr(prior$validator_value), validator_value)
            state$validator_bound <<- write_start == 0 ||
                (isTRUE(prior$validator_bound) && same_identity)
            if (write_start > 0) {
                state$origin <<- if (isTRUE(state$validator_bound)) {
                    "get"
                } else {
                    "checksum-resume"
                }
                state$append_response <<- evidence
                state$append_response$start_byte <<- write_start
                if (!is.null(prior)) {
                    if (!same_identity) {
                        state$previous_checkpoint <<- prior
                    }
                    for (name in c(
                        "request_url",
                        "final_url",
                        "etag",
                        "last_modified",
                        "date",
                        "validator_kind",
                        "validator_value"
                    )) {
                        state[[name]] <<- one_chr(prior[[name]])
                    }
                }
                old_sources <- if (!is.null(prior$sources)) {
                    tryCatch(
                        jsonlite::fromJSON(prior$sources),
                        error = function(e) character()
                    )
                } else {
                    one_chr(prior$request_url)
                }
                state$sources <<- as.character(jsonlite::toJSON(
                    unique(c(old_sources[!is.na(old_sources)], url)),
                    auto_unbox = FALSE
                ))
                state$source_changed_candidate <<- isTRUE(
                    prior$source_changed_candidate
                ) ||
                    (!is.null(prior) && !same_identity)
            }
            transfer_write(tmp_part, state)
            con <<- file(tmp_part, if (write_start > 0) "ab" else "wb")
            validated <<- TRUE
            invisible(NULL)
        }
        # Closing and checkpointing also run after a short read or timeout.
        # Failed header validation leaves both the old bytes and evidence intact.
        checkpoint <- function() {
            if (!is.null(con)) {
                close(con)
                con <<- NULL
                state$bytes <<- as.numeric(
                    file.info(tmp_part, extra_cols = FALSE)$size
                )
                state$sha256 <<- checksum_file(tmp_part, "sha256")
                transfer_write(tmp_part, state)
            }
        }
        on.exit(checkpoint(), add = TRUE)
        response <- curl::curl_fetch_stream(
            url,
            function(chunk) {
                validate(curl::handle_data(handle))
                if (range_complete) {
                    return(TRUE)
                }
                if (
                    is.finite(expected) && received + length(chunk) > expected
                ) {
                    stop(
                        "HTTP GET exceeded its declared byte range.",
                        call. = FALSE
                    )
                }
                writeBin(chunk, con)
                flush(con)
                received <<- received + length(chunk)
                on_progress(write_start + received)
                TRUE
            },
            handle = handle
        )
        validate(response)
        if (!range_complete && is.finite(expected) && received != expected) {
            stop(
                "HTTP GET ended before its declared byte range.",
                call. = FALSE
            )
        }
        checkpoint()
        if (!file.rename(tmp_part, tmp_done)) {
            stop("Cannot publish completed HTTP bytes.", call. = FALSE)
        }
        invisible(tmp_done)
    }

    # Copy one byte range from a local file URL into an atomic piece file.
    copy_file_range <- function(
        url,
        start_byte,
        byte_count,
        path,
        chunk_size = 1024L^2L
    ) {
        src <- sub("^file://", "", url, ignore.case = TRUE)
        src <- utils::URLdecode(src)
        if (!file.exists(src)) {
            stop(
                sprintf("Local source file does not exist: %s", src),
                call. = FALSE
            )
        }
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        tmp <- paste0(path, ".part")
        in_con <- file(src, "rb")
        on.exit(close(in_con), add = TRUE)
        out_con <- file(tmp, "wb")
        on.exit(close(out_con), add = TRUE)
        seek(in_con, where = start_byte, origin = "start")
        remaining <- as.numeric(byte_count)
        while (remaining > 0) {
            n <- min(chunk_size, remaining)
            chunk <- readBin(in_con, "raw", n = as.integer(n))
            if (!length(chunk)) {
                break
            }
            writeBin(chunk, out_con)
            remaining <- remaining - length(chunk)
        }
        close(out_con)
        close(in_con)
        on.exit(NULL, add = FALSE)
        size <- if (file.exists(tmp)) {
            as.numeric(file.info(tmp, extra_cols = FALSE)$size)
        } else {
            NA_real_
        }
        if (!identical(size, as.numeric(byte_count))) {
            unlink(tmp)
            stop(
                "Local range read produced an unexpected byte count.",
                call. = FALSE
            )
        }
        if (file.exists(path)) {
            unlink(path)
        }
        if (!file.rename(tmp, path)) {
            file.copy(tmp, path, overwrite = TRUE)
            unlink(tmp)
        }
        invisible(path)
    }

    # Merge completed piece files in byte-range order into one atomic result.
    merge_piece_files <- function(pieces, tmp_done, chunk_size = 1024L^2L) {
        pieces <- pieces[order(as.integer(pieces$piece_index)), , drop = FALSE]
        dir.create(dirname(tmp_done), recursive = TRUE, showWarnings = FALSE)
        tmp_merge <- paste0(tmp_done, ".merge")
        out_con <- file(tmp_merge, "wb")
        on.exit(close(out_con), add = TRUE)
        for (i in seq_len(nrow(pieces))) {
            path <- pieces$path[[i]]
            in_con <- file(path, "rb")
            repeat {
                chunk <- readBin(in_con, "raw", n = as.integer(chunk_size))
                if (!length(chunk)) {
                    break
                }
                writeBin(chunk, out_con)
            }
            close(in_con)
        }
        close(out_con)
        on.exit(NULL, add = FALSE)
        if (file.exists(tmp_done)) {
            unlink(tmp_done)
        }
        if (!file.rename(tmp_merge, tmp_done)) {
            file.copy(tmp_merge, tmp_done, overwrite = TRUE)
            unlink(tmp_merge)
        }
        invisible(tmp_done)
    }

    # Resolve the final download target from its optional relative subdirectory.
    target_path <- function(dest, subdir, filename) {
        if (is.null(subdir) || is.na(subdir) || !nzchar(subdir)) {
            file.path(dest, filename)
        } else {
            file.path(dest, subdir, filename)
        }
    }

    # Move a verified temporary file into place with a copy fallback across filesystems.
    finalize <- function(tmp_done, target_path) {
        dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
        renamed <- tryCatch(
            file.rename(tmp_done, target_path),
            error = function(e) FALSE
        )
        if (!isTRUE(renamed)) {
            file.copy(tmp_done, target_path, overwrite = TRUE)
            unlink(tmp_done)
        }
        normalizePath(target_path, mustWork = TRUE, winslash = "/")
    }

    list(
        as_df = as_df,
        one_chr = one_chr,
        normalize_count = normalize_count,
        normalize_transfer_policy = normalize_transfer_policy,
        checksum_file = checksum_file,
        verify_checksum = verify_checksum,
        null_if_empty = null_if_empty,
        curl_handle = curl_handle,
        format_byte = format_byte,
        headers_text = headers_text,
        resume_supported = resume_supported,
        resume_guard = resume_guard,
        transfer_response = transfer_response,
        piece_guard = piece_guard,
        transfer_preserve = transfer_preserve,
        transfer_read = transfer_read,
        transfer_write = transfer_write,
        transfer_seal = transfer_seal,
        transfer_stream = transfer_stream,
        copy_file_range = copy_file_range,
        merge_piece_files = merge_piece_files,
        target_path = target_path,
        finalize = finalize
    )
}
# }}}

# downloader__worker_segmented_download {{{
downloader__worker_segmented_download <- function(
    candidates,
    pieces,
    filename,
    subdir,
    dest,
    temp,
    retries,
    timeout,
    overwrite,
    checksum,
    checksum_type,
    tmp_id,
    ssl_verifypeer = TRUE,
    proxy = NULL,
    connect_timeout = NULL,
    useragent = NULL,
    transfer_policy = NULL,
    mode_used = "single",
    worker_dependencies = NULL
) {
    if (is.null(worker_dependencies)) {
        worker_dependencies <- downloader__worker_dependencies()
    }
    # Bind serialized helpers locally so the worker algorithm stays readable.
    as_df <- worker_dependencies$as_df
    one_chr <- worker_dependencies$one_chr
    normalize_transfer_policy <- worker_dependencies$normalize_transfer_policy
    checksum_file <- worker_dependencies$checksum_file
    verify_checksum <- worker_dependencies$verify_checksum
    curl_handle <- worker_dependencies$curl_handle
    format_byte <- worker_dependencies$format_byte
    copy_file_range <- worker_dependencies$copy_file_range
    merge_piece_files <- worker_dependencies$merge_piece_files

    candidates <- as_df(candidates)
    pieces <- as_df(pieces)
    if (!nrow(candidates) || !nrow(pieces)) {
        return(list(
            ok = FALSE,
            error = "No range candidates or pieces are available."
        ))
    }
    transfer_policy <- normalize_transfer_policy(
        transfer_policy,
        include_piece_concurrency = TRUE
    )
    checksum <- worker_dependencies$null_if_empty(checksum)
    target_path <- worker_dependencies$target_path(dest, subdir, filename)
    tmp_done <- file.path(temp, paste0(tmp_id, ".done"))

    if (
        file.exists(target_path) &&
            !isTRUE(overwrite) &&
            verify_checksum(target_path, checksum, checksum_type)
    ) {
        worker_dependencies$transfer_seal(
            file.path(temp, paste0(tmp_id, ".part")),
            target_path,
            checksum,
            checksum_type
        )
        return(list(
            ok = TRUE,
            path = normalizePath(target_path, mustWork = TRUE, winslash = "/"),
            pieces = pieces,
            mode_used = mode_used
        ))
    }
    if (
        file.exists(tmp_done) &&
            verify_checksum(tmp_done, checksum, checksum_type)
    ) {
        path <- worker_dependencies$finalize(tmp_done, target_path)
        worker_dependencies$transfer_seal(
            file.path(temp, paste0(tmp_id, ".part")),
            path,
            checksum,
            checksum_type
        )
        return(list(
            ok = TRUE,
            path = path,
            pieces = pieces,
            mode_used = mode_used
        ))
    }

    evidence_by_resource <- list()
    # Bind every contributing GET to the fixed representation before writing.
    bind_evidence <- function(evidence) {
        if (
            is.finite(evidence$size) && evidence$size != sum(pieces$byte_count)
        ) {
            stop(
                "source_changed: Actual HTTP piece total differs from the planned representation.",
                call. = FALSE
            )
        }
        key <- evidence$request_url
        previous <- evidence_by_resource[[key]]
        if (is.null(checksum) && grepl("^https?://", key)) {
            if (is.na(evidence$validator_kind)) {
                stop(
                    "HTTP range pieces have insufficient strong representation evidence.",
                    call. = FALSE
                )
            }
            if (length(setdiff(names(evidence_by_resource), key))) {
                stop(
                    "HTTP range pieces from different resources require a publisher checksum.",
                    call. = FALSE
                )
            }
            if (
                !is.null(previous) &&
                    (!identical(previous$final_url, evidence$final_url) ||
                        !identical(
                            previous$validator_kind,
                            evidence$validator_kind
                        ) ||
                        !identical(
                            previous$validator_value,
                            evidence$validator_value
                        ) ||
                        (is.finite(previous$size) &&
                            is.finite(evidence$size) &&
                            previous$size != evidence$size))
            ) {
                stop(
                    "source_changed: HTTP range pieces describe different representations.",
                    call. = FALSE
                )
            }
        }
        if (is.null(previous)) {
            evidence_by_resource[[key]] <<- evidence
        }
        invisible(NULL)
    }
    # A saved length alone cannot authorize reuse of an earlier HTTP piece.
    valid_piece <- function(row) {
        path <- row$path[[1L]]
        if (
            !file.exists(path) ||
                as.numeric(file.info(path, extra_cols = FALSE)$size) !=
                    as.numeric(row$byte_count[[1L]])
        ) {
            return(FALSE)
        }
        state <- worker_dependencies$transfer_read(paste0(path, ".part"))
        !is.null(state) &&
            isTRUE(state$complete) &&
            identical(
                as.numeric(state$bytes),
                as.numeric(row$byte_count[[1L]])
            ) &&
            identical(state$sha256, checksum_file(path, "sha256")) &&
            state$request_url %in% candidates$url &&
            (!is.null(checksum) ||
                !is.null(worker_dependencies$null_if_empty(
                    state$validator_kind
                )))
    }
    for (i in seq_len(nrow(pieces))) {
        path <- pieces$path[[i]]
        if (valid_piece(pieces[i, , drop = FALSE])) {
            state <- worker_dependencies$transfer_read(paste0(path, ".part"))
            error <- tryCatch(
                {
                    bind_evidence(state)
                    NULL
                },
                error = function(e) conditionMessage(e)
            )
            if (!is.null(error)) {
                return(list(
                    ok = FALSE,
                    error = error,
                    pieces = pieces,
                    mode_used = mode_used
                ))
            }
            pieces$status[[i]] <- "done"
            pieces$bytes_done[[i]] <- as.numeric(pieces$byte_count[[i]])
            pieces$last_error[[i]] <- NA_character_
        } else {
            if (file.exists(path)) {
                if (
                    !file.rename(
                        path,
                        paste0(path, ".retained-", basename(tempfile()))
                    )
                ) {
                    stop(
                        "Cannot preserve the previous HTTP range piece.",
                        call. = FALSE
                    )
                }
            }
            if (
                file.exists(sub(
                    "[.]part$",
                    ".transfer.json",
                    paste0(path, ".part")
                ))
            ) {
                worker_dependencies$transfer_preserve(paste0(path, ".part"))
            }
            pieces$status[[i]] <- "pending"
            pieces$bytes_done[[i]] <- 0
            pieces$attempts[[i]] <- 0L
        }
    }

    candidate_by_id <- stats::setNames(
        seq_len(nrow(candidates)),
        candidates$candidate_id
    )
    candidate_ids <- candidates$candidate_id[
        !is.na(candidates$candidate_id) & nzchar(candidates$candidate_id)
    ]
    if (!length(candidate_ids)) {
        return(list(
            ok = FALSE,
            error = "No usable range candidate IDs are available.",
            pieces = pieces,
            mode_used = mode_used
        ))
    }
    choose_candidate <- function(piece_index, attempts) {
        candidate_ids[
            ((as.integer(piece_index) + as.integer(attempts)) %%
                length(candidate_ids)) +
                1L
        ]
    }
    piece_concurrency <- max(1L, as.integer(transfer_policy$piece_concurrency))
    piece_chunk_size <- transfer_policy$chunk_size
    if (is.null(piece_chunk_size) || is.na(piece_chunk_size)) {
        piece_chunk_size <- 1024L^2L
    }
    used_candidate_id <- character()

    download_http_batch <- function(batch) {
        out <- vector("list", nrow(batch))
        pool <- curl::new_pool(
            total_con = piece_concurrency,
            host_con = piece_concurrency
        )
        conns <- list()
        handles <- list()
        batch_error <- "curl multi transfer failed."
        on.exit(
            for (handle in handles) {
                try(curl::multi_cancel(handle), silent = TRUE)
            },
            add = TRUE
        )
        close_conn <- function(key) {
            con <- conns[[key]]
            if (!is.null(con)) {
                try(close(con), silent = TRUE)
                conns[[key]] <<- NULL
            }
        }
        for (i in seq_len(nrow(batch))) {
            local({
                j <- i
                row <- batch[j, , drop = FALSE]
                candidate <- candidates[
                    candidate_by_id[[row$candidate_id[[1L]]]],
                    ,
                    drop = FALSE
                ]
                tmp <- paste0(row$path[[1L]], ".part")
                dir.create(dirname(tmp), recursive = TRUE, showWarnings = FALSE)
                if (file.exists(tmp)) {
                    worker_dependencies$transfer_preserve(tmp)
                }
                con <- file(tmp, "wb")
                conns[[as.character(j)]] <<- con
                handle <- curl_handle(
                    timeout = timeout,
                    connect_timeout = connect_timeout,
                    ssl_verifypeer = ssl_verifypeer,
                    proxy = proxy,
                    useragent = useragent,
                    transfer_policy = transfer_policy
                )
                curl::handle_setheaders(
                    handle,
                    `Accept-Encoding` = "identity",
                    Range = sprintf(
                        "bytes=%s-%s",
                        format_byte(row$start_byte[[1L]]),
                        format_byte(row$end_byte[[1L]])
                    )
                )
                curl::handle_setopt(
                    handle,
                    url = candidate$url[[1L]],
                    failonerror = FALSE,
                    http_content_decoding = FALSE
                )
                guard_error <- NULL
                guard <- worker_dependencies$piece_guard(
                    handle,
                    candidate$url[[1L]],
                    row,
                    tmp,
                    bind_evidence
                )
                handles[[as.character(j)]] <<- handle
                curl::multi_add(
                    handle,
                    data = function(chunk, ...) {
                        guard_error <<- tryCatch(
                            {
                                guard$accept(chunk)
                                writeBin(chunk, con)
                                flush(con)
                                NULL
                            },
                            error = function(e) conditionMessage(e)
                        )
                        is.null(guard_error)
                    },
                    done = function(response, ...) {
                        validation_error <- tryCatch(
                            {
                                guard$finish(response)
                                NULL
                            },
                            error = function(e) conditionMessage(e)
                        )
                        close_conn(as.character(j))
                        size <- if (file.exists(tmp)) {
                            as.numeric(file.info(tmp, extra_cols = FALSE)$size)
                        } else {
                            NA_real_
                        }
                        ok <- is.null(validation_error) &&
                            identical(
                                as.integer(response$status_code),
                                206L
                            ) &&
                            identical(size, as.numeric(row$byte_count[[1L]]))
                        if (ok) {
                            if (file.exists(row$path[[1L]])) {
                                unlink(row$path[[1L]])
                            }
                            if (!file.rename(tmp, row$path[[1L]])) {
                                file.copy(tmp, row$path[[1L]], overwrite = TRUE)
                                unlink(tmp)
                            }
                            worker_dependencies$transfer_seal(
                                tmp,
                                row$path[[1L]],
                                NULL,
                                checksum_type
                            )
                            out[[j]] <<- list(
                                ok = TRUE,
                                bytes_done = size,
                                error = NA_character_
                            )
                        } else {
                            worker_dependencies$transfer_preserve(tmp)
                            out[[j]] <<- list(
                                ok = FALSE,
                                bytes_done = 0,
                                error = if (!is.null(validation_error)) {
                                    validation_error
                                } else {
                                    sprintf(
                                        "HTTP Range request returned status %s or unexpected byte count.",
                                        response$status_code
                                    )
                                },
                                full_response = guard$full_response()
                            )
                        }
                    },
                    fail = function(error, ...) {
                        close_conn(as.character(j))
                        worker_dependencies$transfer_preserve(tmp)
                        out[[j]] <<- list(
                            ok = FALSE,
                            bytes_done = 0,
                            error = if (!is.null(guard_error)) {
                                guard_error
                            } else {
                                as.character(error)
                            },
                            full_response = guard$full_response()
                        )
                    },
                    pool = pool
                )
            })
        }
        ok <- tryCatch(
            {
                deadline <- Sys.time() + max(timeout * nrow(batch), 1)
                repeat {
                    state <- curl::multi_run(
                        timeout = max(
                            as.numeric(deadline - Sys.time(), units = "secs"),
                            0.001
                        ),
                        poll = TRUE,
                        pool = pool
                    )
                    if (
                        is.null(state$pending) ||
                            identical(as.integer(state$pending), 0L)
                    ) {
                        break
                    }
                    if (Sys.time() >= deadline) {
                        stop("curl multi transfer timed out.", call. = FALSE)
                    }
                }
                TRUE
            },
            error = function(e) {
                batch_error <<- conditionMessage(e)
                for (key in names(conns)) {
                    close_conn(key)
                }
                FALSE
            }
        )
        for (key in names(conns)) {
            close_conn(key)
        }
        if (!isTRUE(ok)) {
            for (i in seq_len(nrow(batch))) {
                if (is.null(out[[i]])) {
                    out[[i]] <- list(
                        ok = FALSE,
                        bytes_done = 0,
                        error = batch_error
                    )
                }
            }
        }
        out
    }

    # Whole-file replacement begins only after download_http_batch has returned,
    # cancelled its handles and closed its writers. Persist the original anchors
    # separately from the new bytes so a later process can repeat this recovery.
    recover_full_response <- function(response = NULL) {
        tmp_part <- file.path(temp, paste0(tmp_id, ".part"))
        saved <- worker_dependencies$transfer_read(tmp_part)
        representation <- saved$range_fallback
        if (is.null(representation)) {
            representation <- list(
                size = sum(pieces$byte_count),
                anchors = evidence_by_resource,
                response = response
            )
            if (file.exists(tmp_part) || !is.null(saved)) {
                worker_dependencies$transfer_preserve(tmp_part)
            }
            worker_dependencies$transfer_write(
                tmp_part,
                list(
                    generation = basename(tempfile("transfer-")),
                    bytes = 0,
                    sha256 = NA_character_,
                    complete = FALSE,
                    checksum_verified = FALSE,
                    range_fallback = representation
                )
            )
        }
        # Only known active piece paths move; retained history is never deleted
        # by the ordinary task-piece cleanup after a successful replacement.
        for (i in seq_len(nrow(pieces))) {
            path <- pieces$path[[i]]
            suffix <- paste0(".retained-", basename(tempfile()))
            for (active in c(
                path,
                paste0(path, ".part"),
                paste0(path, ".transfer.json")
            )) {
                if (
                    file.exists(active) &&
                        !file.rename(active, paste0(active, suffix))
                ) {
                    stop(
                        "Cannot preserve the previous HTTP range piece.",
                        call. = FALSE
                    )
                }
            }
            pieces$status[[i]] <<- "pending"
            pieces$bytes_done[[i]] <<- 0
        }
        if (file.exists(tmp_done)) {
            if (
                !file.rename(
                    tmp_done,
                    paste0(tmp_done, ".retained-", basename(tempfile()))
                )
            ) {
                stop(
                    "Cannot preserve the previous complete response.",
                    call. = FALSE
                )
            }
            worker_dependencies$transfer_preserve(tmp_part)
        }
        url <- representation$response$request_url
        error <- tryCatch(
            {
                worker_dependencies$transfer_stream(
                    url = url,
                    tmp_part = tmp_part,
                    tmp_done = tmp_done,
                    timeout = timeout,
                    start_byte = 0,
                    connect_timeout = connect_timeout,
                    ssl_verifypeer = ssl_verifypeer,
                    proxy = proxy,
                    useragent = useragent,
                    transfer_policy = transfer_policy,
                    checksum = checksum,
                    resume = TRUE,
                    representation = representation
                )
                if (!verify_checksum(tmp_done, checksum, checksum_type)) {
                    stop(
                        "source_changed: Replacement content does not match the fixed publisher checksum.",
                        call. = FALSE
                    )
                }
                NULL
            },
            error = function(e) conditionMessage(e)
        )
        if (!is.null(error)) {
            return(list(
                ok = FALSE,
                error = error,
                pieces = pieces,
                mode_used = mode_used
            ))
        }
        path <- worker_dependencies$finalize(tmp_done, target_path)
        worker_dependencies$transfer_seal(
            tmp_part,
            path,
            checksum,
            checksum_type
        )
        list(
            ok = TRUE,
            path = path,
            selected_url = url,
            used_candidate_id = candidates$candidate_id[candidates$url == url],
            pieces = pieces,
            mode_used = mode_used
        )
    }
    saved_transfer <- worker_dependencies$transfer_read(file.path(
        temp,
        paste0(tmp_id, ".part")
    ))
    if (!is.null(saved_transfer$range_fallback)) {
        return(recover_full_response())
    }

    while (any(!pieces$status %in% "done")) {
        pending <- which(!pieces$status %in% "done")
        exhausted <- pending[
            suppressWarnings(as.integer(pieces$attempts[pending])) >= retries
        ]
        if (length(exhausted)) {
            err <- pieces$last_error[[exhausted[[1L]]]]
            if (is.na(err)) {
                err <- "Piece download retries exhausted."
            }
            return(list(
                ok = FALSE,
                error = err,
                pieces = pieces,
                mode_used = mode_used
            ))
        }
        batch_idx <- pending[seq_len(min(piece_concurrency, length(pending)))]
        for (idx in batch_idx) {
            attempts <- suppressWarnings(as.integer(pieces$attempts[[idx]]))
            if (is.na(attempts)) {
                attempts <- 0L
            }
            pieces$attempts[[idx]] <- attempts + 1L
            if (
                !pieces$candidate_id[[idx]] %in% candidate_ids ||
                    identical(mode_used, "multi")
            ) {
                pieces$candidate_id[[idx]] <- choose_candidate(
                    pieces$piece_index[[idx]],
                    attempts
                )
            }
            pieces$status[[idx]] <- "downloading"
            pieces$last_error[[idx]] <- NA_character_
        }
        batch <- pieces[batch_idx, , drop = FALSE]
        file_batch <- grepl(
            "^file://",
            candidates$url[match(batch$candidate_id, candidates$candidate_id)],
            ignore.case = TRUE
        )
        results <- vector("list", nrow(batch))
        if (any(file_batch)) {
            rows <- which(file_batch)
            for (pos in rows) {
                row <- batch[pos, , drop = FALSE]
                candidate <- candidates[
                    candidate_by_id[[row$candidate_id[[1L]]]],
                    ,
                    drop = FALSE
                ]
                results[[pos]] <- tryCatch(
                    {
                        copy_file_range(
                            candidate$url[[1L]],
                            start_byte = row$start_byte[[1L]],
                            byte_count = row$byte_count[[1L]],
                            path = row$path[[1L]],
                            chunk_size = piece_chunk_size
                        )
                        evidence <- list(
                            request_url = candidate$url[[1L]],
                            final_url = candidate$url[[1L]],
                            etag = NA_character_,
                            last_modified = NA_character_,
                            date = NA_character_,
                            validator_kind = NA_character_,
                            validator_value = NA_character_,
                            generation = basename(tempfile("piece-")),
                            size = sum(pieces$byte_count),
                            bytes = row$byte_count[[1L]],
                            sha256 = checksum_file(row$path[[1L]], "sha256"),
                            complete = TRUE,
                            checksum_verified = FALSE
                        )
                        worker_dependencies$transfer_write(
                            paste0(row$path[[1L]], ".part"),
                            evidence
                        )
                        bind_evidence(evidence)
                        list(
                            ok = TRUE,
                            bytes_done = as.numeric(row$byte_count[[1L]]),
                            error = NA_character_
                        )
                    },
                    error = function(e) {
                        list(
                            ok = FALSE,
                            bytes_done = 0,
                            error = conditionMessage(e)
                        )
                    }
                )
            }
        }
        if (any(!file_batch)) {
            http_rows <- which(!file_batch)
            http_results <- download_http_batch(batch[
                http_rows,
                ,
                drop = FALSE
            ])
            for (k in seq_along(http_rows)) {
                result <- if (k <= length(http_results)) {
                    http_results[[k]]
                } else {
                    NULL
                }
                if (is.null(result)) {
                    result <- list(
                        ok = FALSE,
                        bytes_done = 0,
                        error = "HTTP piece download did not return a result."
                    )
                }
                results[[http_rows[[k]]]] <- result
            }
        }
        # A competing response can prove a change while another asks to restart
        # from zero. Preserve that contradiction instead of letting the restart
        # mask it after the entire batch has retired.
        contradictions <- which(vapply(
            results,
            function(result) {
                !isTRUE(result$ok) &&
                    isTRUE(grepl("^source_changed:", one_chr(result$error)))
            },
            logical(1L)
        ))
        if (length(contradictions)) {
            return(list(
                ok = FALSE,
                error = results[[contradictions[[1L]]]]$error,
                pieces = pieces,
                mode_used = mode_used
            ))
        }
        complete_responses <- which(vapply(
            results,
            function(result) {
                !is.null(result$full_response)
            },
            logical(1L)
        ))
        if (length(complete_responses)) {
            return(recover_full_response(
                results[[complete_responses[[1L]]]]$full_response
            ))
        }
        for (pos in seq_along(batch_idx)) {
            idx <- batch_idx[[pos]]
            result <- if (pos <= length(results)) results[[pos]] else NULL
            if (is.null(result)) {
                result <- list(
                    ok = FALSE,
                    bytes_done = 0,
                    error = "Piece download did not return a result."
                )
            }
            if (isTRUE(result$ok)) {
                pieces$status[[idx]] <- "done"
                pieces$bytes_done[[idx]] <- as.numeric(result$bytes_done)
                pieces$last_error[[idx]] <- NA_character_
                used_candidate_id <- unique(c(
                    used_candidate_id,
                    pieces$candidate_id[[idx]]
                ))
            } else {
                pieces$status[[idx]] <- "pending"
                pieces$bytes_done[[idx]] <- 0
                pieces$last_error[[idx]] <- one_chr(result$error)
                if (identical(mode_used, "single")) {
                    pieces$candidate_id[[idx]] <- candidate_ids[[1L]]
                }
            }
        }
    }

    merge_piece_files(pieces, tmp_done, chunk_size = piece_chunk_size)
    if (!verify_checksum(tmp_done, checksum, checksum_type)) {
        actual <- if (is.null(checksum)) {
            NA_character_
        } else {
            checksum_file(tmp_done, checksum_type)
        }
        unlink(tmp_done)
        return(list(
            ok = FALSE,
            error = sprintf(
                "Checksum verification failed for segmented download. Expected: %s; got: %s",
                checksum,
                actual
            ),
            pieces = pieces,
            integrity_error = TRUE,
            mode_used = mode_used
        ))
    }
    piece_evidence <- lapply(pieces$path, function(path) {
        worker_dependencies$transfer_read(paste0(path, ".part"))
    })
    if (any(vapply(piece_evidence, is.null, logical(1L)))) {
        return(list(
            ok = FALSE,
            error = "HTTP range pieces are missing actual transfer evidence.",
            pieces = pieces,
            mode_used = mode_used
        ))
    }
    evidence <- piece_evidence[[1L]]
    evidence$origin <- "get-pieces"
    evidence$generation <- basename(tempfile("transfer-pieces-"))
    evidence$size <- sum(pieces$byte_count)
    evidence$bytes <- evidence$size
    evidence$sha256 <- checksum_file(tmp_done, "sha256")
    evidence$pieces <- piece_evidence
    evidence$sources <- as.character(jsonlite::toJSON(
        unique(vapply(
            piece_evidence,
            function(x) x$request_url,
            character(1L)
        )),
        auto_unbox = FALSE
    ))
    if (
        length(unique(vapply(
            piece_evidence,
            function(x) x$request_url,
            character(1L)
        ))) >
            1L
    ) {
        evidence$etag <- NA_character_
        evidence$last_modified <- NA_character_
        evidence$validator_kind <- NA_character_
        evidence$validator_value <- NA_character_
        evidence$validator_bound <- FALSE
    }
    evidence$complete <- FALSE
    evidence$checksum_verified <- FALSE
    tmp_part <- file.path(temp, paste0(tmp_id, ".part"))
    if (
        file.exists(tmp_part) ||
            !is.null(worker_dependencies$transfer_read(tmp_part))
    ) {
        worker_dependencies$transfer_preserve(tmp_part)
    }
    evidence$path <- tmp_done
    worker_dependencies$transfer_write(tmp_part, evidence)
    path <- worker_dependencies$finalize(tmp_done, target_path)
    worker_dependencies$transfer_seal(tmp_part, path, checksum, checksum_type)
    list(
        ok = TRUE,
        path = path,
        selected_url = piece_evidence[[1L]]$request_url,
        used_candidate_id = unique(pieces$candidate_id),
        pieces = pieces,
        mode_used = mode_used
    )
}
# }}}

# downloader__worker_download {{{
downloader__worker_download <- function(
    url,
    filename,
    subdir,
    dest,
    temp,
    retries,
    timeout,
    overwrite,
    checksum,
    checksum_type,
    resume,
    tmp_id,
    ssl_verifypeer = TRUE,
    proxy = NULL,
    connect_timeout = NULL,
    useragent = NULL,
    transfer_policy = NULL,
    worker_dependencies = NULL
) {
    if (is.null(worker_dependencies)) {
        worker_dependencies <- downloader__worker_dependencies()
    }
    # Bind serialized helpers locally while retaining the ordinary stream algorithm.
    checksum_file <- worker_dependencies$checksum_file
    verify_checksum <- worker_dependencies$verify_checksum
    normalize_transfer_policy <- worker_dependencies$normalize_transfer_policy
    curl_handle <- worker_dependencies$curl_handle
    stream <- worker_dependencies$transfer_stream

    target_path <- worker_dependencies$target_path(dest, subdir, filename)
    tmp_part <- file.path(temp, paste0(tmp_id, ".part"))
    tmp_done <- file.path(temp, paste0(tmp_id, ".done"))
    checksum <- worker_dependencies$null_if_empty(checksum)

    if (
        file.exists(target_path) &&
            !isTRUE(overwrite) &&
            verify_checksum(target_path, checksum, checksum_type)
    ) {
        worker_dependencies$transfer_seal(
            tmp_part,
            target_path,
            checksum,
            checksum_type
        )
        return(list(
            ok = TRUE,
            path = normalizePath(target_path, mustWork = TRUE, winslash = "/")
        ))
    }
    if (
        file.exists(tmp_done) &&
            verify_checksum(tmp_done, checksum, checksum_type)
    ) {
        path <- worker_dependencies$finalize(tmp_done, target_path)
        worker_dependencies$transfer_seal(
            tmp_part,
            path,
            checksum,
            checksum_type
        )
        return(list(ok = TRUE, path = path))
    }

    dir.create(dirname(tmp_part), recursive = TRUE, showWarnings = FALSE)
    start_byte <- if (isTRUE(resume) && file.exists(tmp_part)) {
        file.info(tmp_part, extra_cols = FALSE)$size
    } else {
        0
    }

    attempt <- 1L
    last_error <- NULL
    checksum_history <- character()
    while (attempt <= retries) {
        # The previous interrupted stream may have added valid bytes.
        if (isTRUE(resume) && file.exists(tmp_part)) {
            start_byte <- as.numeric(
                file.info(tmp_part, extra_cols = FALSE)$size
            )
        }
        ok <- tryCatch(
            {
                stream(
                    url,
                    tmp_part,
                    tmp_done,
                    timeout,
                    start_byte,
                    connect_timeout,
                    ssl_verifypeer,
                    proxy,
                    useragent,
                    transfer_policy,
                    checksum = checksum,
                    resume = resume
                )
                TRUE
            },
            error = function(e) {
                last_error <<- conditionMessage(e)
                FALSE
            }
        )
        if (ok) {
            if (verify_checksum(tmp_done, checksum, checksum_type)) {
                path <- worker_dependencies$finalize(tmp_done, target_path)
                worker_dependencies$transfer_seal(
                    tmp_part,
                    path,
                    checksum,
                    checksum_type
                )
                return(list(ok = TRUE, path = path))
            }

            evidence <- worker_dependencies$transfer_read(tmp_part)
            if (
                isTRUE(evidence$replaces_prior) ||
                    isTRUE(evidence$source_changed_candidate)
            ) {
                return(list(
                    ok = FALSE,
                    error = "source_changed: Replacement content does not match the fixed publisher checksum."
                ))
            }
            actual <- checksum_file(tmp_done, checksum_type)
            checksum_history <- c(checksum_history, actual)
            unlink(c(tmp_part, tmp_done))
            if (
                length(checksum_history) >= 2L &&
                    all(checksum_history == checksum_history[[1L]])
            ) {
                return(list(
                    ok = FALSE,
                    error = sprintf(
                        "File consistently returns the same incorrect checksum. Expected: %s; got: %s",
                        checksum,
                        actual
                    )
                ))
            }
            last_error <- sprintf(
                "Checksum verification failed. Expected: %s; got: %s",
                checksum,
                actual
            )
            start_byte <- 0
        }
        if (attempt < retries) {
            Sys.sleep(2^(attempt - 1L))
        }
        attempt <- attempt + 1L
    }

    if (is.null(last_error) || is.na(last_error)) {
        last_error <- "unknown"
    }
    list(
        ok = FALSE,
        error = sprintf(
            "Failed to download file after %d attempts. Last error: %s",
            retries,
            last_error
        )
    )
}
# }}}

# downloader__job_main {{{
downloader__job_main <- function(manifest, job_id) {
    dl <- Downloader$new(manifest = manifest)
    on.exit(
        try(dl$.__enclos_env__$private$disconnect_manifest(), silent = TRUE),
        add = TRUE
    )
    dl$.__enclos_env__$private$run_job(job_id)
    invisible(TRUE)
}
# }}}

# downloader__daemon_main {{{
downloader__daemon_main <- function(manifest, daemon_id) {
    dl <- Downloader$new(manifest = manifest)
    on.exit(
        try(dl$.__enclos_env__$private$disconnect_manifest(), silent = TRUE),
        add = TRUE
    )
    dl$.__enclos_env__$private$run_daemon_loop(daemon_id)
    invisible(TRUE)
}
# }}}
# Downloader
#' General Purpose File Downloader
#'
#' @description
#'
#' The implementation in `R/standalone-downloader.R` can be copied and sourced on its own,
#' with its qualified package dependencies installed. Detached jobs retain the
#' copied source path, so keep that file available until the jobs finish.
#' Integration with a host's cache policy and store is optional and supplied by
#' the host package; ordinary downloads do not require epwshiftr to be loaded.
#'
#' `Downloader` provides a general purpose file download system with:
#' - File status management (missing, downloading, downloaded, verified)
#' - Incremental checksum verification during download
#' - Resume capability for interrupted downloads
#' - Async and parallel download using mirai
#' - Progress tracking
#' - Error handling and retry logic
#'
#' @author Hongyuan Jia
#' @name Downloader
#' @export
# Downloader {{{
Downloader <- R6::R6Class(
    "Downloader",
    lock_class = TRUE,
    public = list(
        # initialize
        #' @description
        #' Create a new Downloader object
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
        #' @param ssl_verifypeer Whether to verify HTTPS certificates. Default:
        #'        `TRUE`.
        #'
        #' @param proxy Optional proxy URL passed to libcurl. Default: `NULL`.
        #'
        #' @param connect_timeout Optional connection timeout in seconds passed
        #'        to libcurl. Default: `NULL`.
        #'
        #' @param useragent Optional HTTP user agent passed to libcurl.
        #'        Default: `NULL`.
        #'
        #' @param cleanup A logical value specifying whether to automatically clean up failed temporary
        #'        files. Default: `TRUE`.
        #'
        #' @param n_workers A non-negative integer specifying the number of parallel workers
        #'        for async downloads. If `0`, async downloads will fallback to synchronous mode.
        #'        Default: `4L`.
        #'
        #' @param node_policy A list controlling historical data-node cooldown
        #'        and ranking. Missing fields use conservative defaults.
        #'
        #' @param transfer_policy A list controlling curl transfer options and
        #'        optional experimental Range-piece downloads. Supported curl
        #'        fields are `chunk_size`, `bandwidth_limit`, `low_speed_limit`,
        #'        and `low_speed_time`. Range fields are `range_mode`
        #'        (`"off"`, `"single"`, `"multi"`, or `"auto"`), `piece_size`,
        #'        `piece_concurrency`, `max_sources`,
        #'        `require_checksum_for_multisource`, and `range_probe_timeout`.
        #'        The default `range_mode = "off"` keeps the existing streaming
        #'        download behavior.
        #'
        #' @param resource_policy A list controlling local resource checks and
        #'        scheduling. Supported fields are `host_concurrency`,
        #'        `disk_preflight`, and `min_free_space`.
        #'
        #' @param manifest Optional DuckDB manifest path for persistent
        #'        sessions, tasks, candidate URLs, and events. If `NULL`, only
        #'        the single-file shortcut API is available. Default: `NULL`.
        #'
        #' @return An `Downloader` object.
        #'
        #' @examples
        #' \dontrun{
        #' dl <- Downloader$new()
        #' dl <- Downloader$new(dest = "~/data")
        #' dl <- Downloader$new(
        #'     dest = "~/data",
        #'     temp = "~/data/.tmp",
        #'     n_workers = 8
        #' )
        #' }
        # initialize {{{
        initialize = function(
            dest = NULL,
            temp = NULL,
            retries = 3L,
            timeout = 3600L,
            ssl_verifypeer = TRUE,
            proxy = NULL,
            connect_timeout = NULL,
            useragent = NULL,
            cleanup = TRUE,
            n_workers = 4L,
            node_policy = NULL,
            transfer_policy = NULL,
            resource_policy = NULL,
            manifest = NULL
        ) {
            checkmate::assert_string(manifest, null.ok = TRUE)
            manifest_config <- NULL
            if (!is.null(manifest)) {
                private$manifest_path <- normalizePath(
                    path.expand(manifest),
                    mustWork = FALSE,
                    winslash = "/"
                )
                dir.create(
                    dirname(private$manifest_path),
                    recursive = TRUE,
                    showWarnings = FALSE
                )
                private$connect_manifest()
                private$init_manifest_schema()
                manifest_config <- private$read_manifest_config()
                if (!is.null(manifest_config)) {
                    if (missing(dest)) {
                        dest <- manifest_config$dest
                    }
                    if (missing(temp)) {
                        temp <- manifest_config$temp
                    }
                    if (missing(retries)) {
                        retries <- manifest_config$retries
                    }
                    if (missing(timeout)) {
                        timeout <- manifest_config$timeout
                    }
                    if (missing(ssl_verifypeer)) {
                        ssl_verifypeer <- manifest_config$ssl_verifypeer
                    }
                    if (missing(proxy)) {
                        proxy <- manifest_config$proxy
                    }
                    if (missing(connect_timeout)) {
                        connect_timeout <- manifest_config$connect_timeout
                    }
                    if (missing(useragent)) {
                        useragent <- manifest_config$useragent
                    }
                    if (missing(cleanup)) {
                        cleanup <- manifest_config$cleanup
                    }
                    if (missing(n_workers)) {
                        n_workers <- manifest_config$n_workers
                    }
                    if (missing(node_policy)) {
                        node_policy <- manifest_config$node_policy
                    } else if (!is.null(node_policy)) {
                        node_policy <- utils::modifyList(
                            manifest_config$node_policy,
                            node_policy,
                            keep.null = TRUE
                        )
                    }
                    if (missing(transfer_policy)) {
                        transfer_policy <- manifest_config$transfer_policy
                    } else if (!is.null(transfer_policy)) {
                        transfer_policy <- utils::modifyList(
                            manifest_config$transfer_policy,
                            transfer_policy,
                            keep.null = TRUE
                        )
                    }
                    if (missing(resource_policy)) {
                        resource_policy <- manifest_config$resource_policy
                    } else if (!is.null(resource_policy)) {
                        resource_policy <- utils::modifyList(
                            manifest_config$resource_policy,
                            resource_policy,
                            keep.null = TRUE
                        )
                    }
                } else if (missing(dest)) {
                    cli::cli_abort(c(
                        "Downloader manifest does not contain typed configuration.",
                        "i" = "Initialize it once with {.code Downloader$new(manifest = ..., dest = ...)}."
                    ))
                }
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
            checkmate::assert_flag(ssl_verifypeer)
            proxy <- downloader__null_if_empty(proxy)
            checkmate::assert_string(proxy, null.ok = TRUE)
            if (!is.null(connect_timeout)) {
                checkmate::assert_count(connect_timeout, positive = TRUE)
            }
            useragent <- downloader__null_if_empty(useragent)
            checkmate::assert_string(useragent, null.ok = TRUE)
            checkmate::assert_flag(cleanup)
            checkmate::assert_count(n_workers, positive = FALSE)
            node_policy <- downloader__node_policy_defaults(node_policy)
            transfer_policy <- downloader__transfer_policy_defaults(
                transfer_policy
            )
            resource_policy <- downloader__resource_policy_defaults(
                resource_policy
            )

            private$dest <- normalizePath(dest, mustWork = TRUE, winslash = "/")

            # Set up temp
            if (is.null(temp)) {
                private$temp <- file.path(private$dest, ".tmp")
            } else {
                private$temp <- normalizePath(
                    path.expand(temp),
                    mustWork = FALSE,
                    winslash = "/"
                )
            }

            # Create temp if it doesn't exist
            if (!dir.exists(private$temp)) {
                dir.create(private$temp, recursive = TRUE)
            }
            private$temp <- normalizePath(
                private$temp,
                mustWork = TRUE,
                winslash = "/"
            )

            private$retries <- retries
            private$dl_timeout <- timeout
            private$ssl_verifypeer <- ssl_verifypeer
            private$proxy <- proxy
            private$connect_timeout <- connect_timeout
            private$useragent <- useragent
            private$cleanup <- cleanup
            private$worker_count <- n_workers
            private$node_policy_config <- node_policy
            private$transfer_policy_config <- transfer_policy
            private$resource_policy_config <- resource_policy
            private$async_tasks <- list()
            private$persistent_tasks <- list()
            private$callbacks <- list()

            if (!is.null(manifest)) {
                private$write_manifest_config()
            }

            self
        },
        # }}}
        # download
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
        # download {{{
        download = function(
            url,
            filename = NULL,
            subdir = NULL,
            progress = TRUE,
            overwrite = FALSE,
            checksum = NULL,
            checksum_type = "sha256",
            resume = TRUE,
            block = TRUE,
            .tmp_id = NULL
        ) {
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
                        url,
                        filename,
                        subdir,
                        progress,
                        overwrite,
                        checksum,
                        checksum_type,
                        resume
                    ))
                }
            }

            # determine filename
            if (is.null(filename)) {
                filename <- basename(url)
                filename <- sub("\\?.*$", "", filename) # remove query params
            }
            # TODO: handle redirect in filename

            # determine final destination
            if (is.null(subdir)) {
                dest <- file.path(private$dest, filename)
            } else {
                dest <- file.path(private$dest, subdir, filename)
            }

            # get temporary file path
            tmp_id <- if (!is.null(.tmp_id)) {
                .tmp_id
            } else if (!is.null(checksum)) {
                checksum
            } else {
                downloader__checksum_bytes(charToRaw(url), "md5")
            }
            tmp_part <- file.path(private$temp, paste0(tmp_id, ".part"))
            tmp_done <- file.path(private$temp, paste0(tmp_id, ".done"))

            # check current status
            status <- private$check_file_status(
                dest,
                tmp_part,
                tmp_done,
                checksum,
                checksum_type
            )

            # if file is verified and exists, return it
            if (status == FileStatus$Verified && !overwrite) {
                downloader__worker_dependencies()$transfer_seal(
                    tmp_part,
                    dest,
                    checksum,
                    checksum_type
                )
                downloader__verbose(cli::cli_alert_info(
                    "File already verified: {dest}. Skipping..."
                ))
                return(dest)
            }

            # In offline mode, refuse to download files that don't exist locally
            if (downloader__offline() && status != FileStatus$Downloaded) {
                stop(
                    "Cannot download file in offline mode: ",
                    url,
                    "\nExpected file at: ",
                    dest,
                    call. = FALSE
                )
            }

            # if .done file exists and is valid, just move it
            if (status == FileStatus$Downloaded) {
                downloader__verbose(cli::cli_alert_info(
                    "Moving completed download to final location..."
                ))
                private$finalize_download(tmp_done, dest)
                downloader__worker_dependencies()$transfer_seal(
                    tmp_part,
                    dest,
                    checksum,
                    checksum_type
                )
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
                downloader__verbose(cli::cli_alert_info(
                    "Resuming download from byte {start_byte}..."
                ))
            }

            # download with retry logic
            attempt <- 1L
            last_error <- NULL
            chksum_history <- character(0) # track checksums for intelligent retry

            while (attempt <= private$retries) {
                # Re-read the partial after each failed GET rather than appending
                # the same tail again from an obsolete starting offset.
                if (isTRUE(resume) && file.exists(tmp_part)) {
                    start_byte <- as.numeric(
                        file.info(tmp_part, extra_cols = FALSE)$size
                    )
                }
                result <- tryCatch(
                    {
                        private$download_with_streaming(
                            url,
                            tmp_part,
                            tmp_done,
                            progress,
                            start_byte,
                            task_id = .tmp_id,
                            checksum = checksum,
                            resume = resume
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
                        downloader__verbose(cli::cli_alert_info(
                            "Verifying checksum..."
                        ))

                        is_valid <- downloader__verify_checksum(
                            tmp_done,
                            checksum,
                            checksum_type
                        )

                        if (!is_valid) {
                            evidence <- downloader__worker_dependencies()$transfer_read(
                                tmp_part
                            )
                            if (
                                (isTRUE(evidence$replaces_prior) ||
                                    isTRUE(evidence$source_changed_candidate))
                            ) {
                                stop(
                                    "source_changed: Replacement content does not match the fixed publisher checksum.",
                                    call. = FALSE
                                )
                            }
                            # get actual checksum for debugging
                            actual <- private$get_file_checksum(
                                tmp_done,
                                checksum_type
                            )
                            chksum_history <- c(chksum_history, actual)

                            # check for persistent checksum mismatch (server-side issue)
                            if (
                                length(chksum_history) >= 2 &&
                                    all(chksum_history == chksum_history[1])
                            ) {
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
                                    checksum,
                                    actual
                                ))
                            }

                            if (attempt < private$retries) {
                                cli::cli_alert_warning(sprintf(
                                    "Checksum verification failed (attempt %d/%d)",
                                    attempt,
                                    private$retries
                                ))
                                downloader__verbose(cli::cli_alert_info(
                                    "Expected: {checksum}"
                                ))
                                downloader__verbose(cli::cli_alert_info(
                                    "Got:      {actual}"
                                ))
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
                                    ),
                                    private$retries,
                                    checksum,
                                    actual
                                ))
                            }
                        }

                        downloader__verbose(cli::cli_alert_success(
                            "Checksum verified"
                        ))
                    }

                    # move to final location
                    private$finalize_download(tmp_done, dest)
                    downloader__worker_dependencies()$transfer_seal(
                        tmp_part,
                        dest,
                        checksum,
                        checksum_type
                    )
                    downloader__verbose(cli::cli_alert_success(
                        "Downloaded: {.var {dest}}."
                    ))
                    return(dest)
                }

                if (attempt < private$retries) {
                    wait_time <- 2^(attempt - 1)
                    warning(sprintf(
                        "Download failed (attempt %d/%d). Waiting %ds before retry...",
                        attempt,
                        private$retries,
                        wait_time
                    ))
                    Sys.sleep(wait_time)
                }

                attempt <- attempt + 1L
            }

            stop(sprintf(
                "Failed to download file after %d attempts. Last error: %s",
                private$retries,
                conditionMessage(last_error)
            ))
        },
        # }}}
        # enqueue
        #' @description
        #' Add a download plan to the persistent manifest.
        #'
        #' @param plan A data frame with at least `logical_file_id`, `filename`,
        #'        and `url` columns.
        #' @param session_label Optional label for this download session.
        #'
        #' @return The created session ID.
        # enqueue {{{
        enqueue = function(plan, session_label = NULL) {
            private$require_manifest()
            checkmate::assert_string(session_label, null.ok = TRUE)
            private$with_manifest_lock({
                plan <- private$normalize_plan(plan)
                if (!nrow(plan)) {
                    cli::cli_abort(
                        "Download plan does not contain any candidate URL."
                    )
                }

                session_id <- private$new_session_id()
                now <- downloader__now()
                private$append_rows(
                    "download_session",
                    data.frame(
                        session_id = session_id,
                        label = downloader__one_chr(session_label),
                        status = "queued",
                        created_at = now,
                        updated_at = now,
                        completed_at = as.POSIXct(NA),
                        stringsAsFactors = FALSE
                    )
                )

                tasks <- private$plan_tasks(plan, session_id, now)
                candidates <- private$plan_candidates(plan, tasks, now)
                private$append_rows("download_task", tasks)
                private$append_rows("download_candidate", candidates)
                private$log_event(
                    session_id,
                    NA_character_,
                    "enqueue",
                    sprintf("Queued %d download task(s).", nrow(tasks))
                )
                session_id
            })
        },
        # }}}
        # preflight
        #' @description
        #' Check local resource requirements before downloading.
        #'
        #' @param plan Optional download plan. If supplied, preflight is
        #'        calculated without writing to the persistent manifest.
        #' @param session_id Optional persistent session ID.
        #' @param task_id Optional persistent task ID vector.
        #' @param overwrite Whether existing final files would be overwritten.
        #'        Default: `FALSE`.
        #'
        #' @return A one-row data frame with byte and disk-space summary.
        # preflight {{{
        preflight = function(
            plan = NULL,
            session_id = NULL,
            task_id = NULL,
            overwrite = FALSE
        ) {
            checkmate::assert_data_frame(plan, null.ok = TRUE)
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_character(
                task_id,
                any.missing = FALSE,
                null.ok = TRUE
            )
            checkmate::assert_flag(overwrite)
            if (!is.null(plan)) {
                plan <- private$normalize_plan(plan)
                tasks <- private$plan_tasks(
                    plan,
                    session_id = "preflight",
                    now = downloader__now()
                )
                return(private$preflight_task_space(
                    tasks,
                    overwrite = overwrite
                ))
            }
            private$require_manifest()
            tasks <- private$select_tasks(
                session_id = session_id,
                task_id = task_id,
                status = c("queued", "downloading")
            )
            private$preflight_task_space(tasks, overwrite = overwrite)
        },
        # }}}
        # run
        #' @description
        #' Run queued persistent download tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #' @param block Whether to block until completion. If `FALSE`, creates
        #'        a detached background job via `$start()`.
        #' @param progress Whether to show per-file progress.
        #' @param overwrite Whether to overwrite existing final files.
        #' @param resume Whether to resume `.part` files.
        #'
        #' @return If `block = TRUE`, a data frame of selected task records
        #'         after the run. If `block = FALSE`, a one-row background job
        #'         record.
        # run {{{
        run = function(
            session_id = NULL,
            task_id = NULL,
            block = TRUE,
            progress = TRUE,
            overwrite = FALSE,
            resume = TRUE
        ) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_character(
                task_id,
                any.missing = FALSE,
                null.ok = TRUE
            )
            checkmate::assert_flag(block)
            checkmate::assert_flag(progress)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            if (!isTRUE(block)) {
                return(self$start(
                    session_id = session_id,
                    task_id = task_id,
                    overwrite = overwrite,
                    resume = resume,
                    mode = "process"
                ))
            }

            private$run_selected_tasks(
                session_id = session_id,
                task_id = task_id,
                progress = progress,
                overwrite = overwrite,
                resume = resume
            )
        },
        # }}}
        # start
        #' @description
        #' Start a persistent download session in the background.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #' @param overwrite Whether to overwrite existing final files.
        #' @param resume Whether to resume `.part` files.
        #' @param mode Background execution mode. `"process"` starts a detached
        #'        `Rscript`; `"daemon"` submits the job to a running downloader
        #'        daemon.
        #' @param store_path Optional [EsgStore] path to sync after completion.
        #'
        #' @return A one-row data frame describing the background job.
        # start {{{
        start = function(
            session_id = NULL,
            task_id = NULL,
            overwrite = FALSE,
            resume = TRUE,
            mode = c("process", "daemon"),
            store_path = NULL
        ) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_character(
                task_id,
                any.missing = FALSE,
                null.ok = TRUE
            )
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            checkmate::assert_string(store_path, null.ok = TRUE)
            mode <- match.arg(mode)

            job <- private$create_job(
                session_id = session_id,
                task_id = task_id,
                overwrite = overwrite,
                resume = resume,
                mode = mode,
                store_path = store_path
            )
            if (identical(mode, "daemon")) {
                if (
                    !private$wake_daemon(
                        command = "job",
                        id = job$job_id[[1L]],
                        attempts = 10L
                    )
                ) {
                    job$status <- "error"
                    job$error <- "No running downloader daemon accepted the job."
                    private$update_job(job)
                    cli::cli_abort(
                        "No running downloader daemon accepted the job. Start one with {.code downloader$daemon_start()}."
                    )
                }
                return(private$select_jobs(job_id = job$job_id[[1L]]))
            }

            ok <- tryCatch(
                {
                    private$launch_process(
                        "job",
                        job$job_id[[1L]],
                        job$log_path[[1L]]
                    )
                    TRUE
                },
                error = function(e) {
                    job$status <- "error"
                    job$error <- conditionMessage(e)
                    private$update_job(job)
                    FALSE
                }
            )
            if (!isTRUE(ok)) {
                cli::cli_abort(
                    "Failed to launch downloader background job: {job$error[[1L]]}"
                )
            }
            private$select_jobs(job_id = job$job_id[[1L]])
        },
        # }}}
        # jobs
        #' @description
        #' List downloader background jobs.
        #'
        #' @param status Optional job status filter.
        # jobs {{{
        jobs = function(status = NULL) {
            private$require_manifest()
            checkmate::assert_subset(
                status,
                DOWNLOADER_JOB_STATUS,
                empty.ok = TRUE
            )
            private$select_jobs(status = status)
        },
        # }}}
        # job_status
        #' @description
        #' Return downloader background job status.
        #'
        #' @param job_id Optional job ID filter.
        # job_status {{{
        job_status = function(job_id = NULL) {
            private$require_manifest()
            checkmate::assert_character(
                job_id,
                any.missing = FALSE,
                null.ok = TRUE
            )
            if (is.null(job_id)) {
                return(private$select_jobs())
            }
            private$select_jobs(job_id = job_id)
        },
        # }}}
        # job_logs
        #' @description
        #' Return downloader background job log lines.
        #'
        #' @param job_id Job ID.
        #' @param tail Number of trailing lines to return.
        # job_logs {{{
        job_logs = function(job_id, tail = 100L) {
            private$require_manifest()
            checkmate::assert_string(job_id)
            checkmate::assert_count(tail, positive = FALSE)
            job <- private$select_jobs(job_id = job_id)
            if (!nrow(job)) {
                cli::cli_abort("Downloader job not found: {.val {job_id}}.")
            }
            path <- downloader__one_chr(job$log_path[[1L]])
            lines <- if (!is.na(path) && file.exists(path)) {
                readLines(path, warn = FALSE)
            } else {
                character()
            }
            if (length(lines) > tail) {
                lines <- utils::tail(lines, tail)
            }
            data.frame(
                job_id = rep(job_id, length(lines)),
                line = seq_along(lines),
                message = lines,
                stringsAsFactors = FALSE
            )
        },
        # }}}
        # stop_job
        #' @description
        #' Request cancellation of a background downloader job.
        #'
        #' @param job_id Job ID.
        #' @param force Whether to kill the recorded process immediately.
        # stop_job {{{
        stop_job = function(job_id, force = FALSE) {
            private$require_manifest()
            checkmate::assert_string(job_id)
            checkmate::assert_flag(force)
            job <- private$select_jobs(job_id = job_id)
            if (!nrow(job)) {
                cli::cli_abort("Downloader job not found: {.val {job_id}}.")
            }
            private$append_control("job", job_id, "stop")
            if (job$status[[1L]] %in% "queued") {
                tasks <- private$select_tasks(
                    job_id = job_id,
                    status = "queued"
                )
                if (nrow(tasks)) {
                    tasks$status <- "cancelled"
                    tasks$last_error <- "Cancelled by user."
                    tasks$completed_at <- downloader__now()
                    private$update_task(tasks)
                }
                private$complete_job(
                    job_id,
                    status = "cancelled",
                    error = "Cancelled by user.",
                    exit_code = 0L
                )
            } else {
                job$status <- "stopping"
                private$update_job(job)
            }
            private$wake_daemon(command = "stop", id = job_id, attempts = 3L)
            if (isTRUE(force)) {
                downloader__pid_kill(job$pid[[1L]])
            }
            private$select_jobs(job_id = job_id)
        },
        # }}}
        # daemon_start
        #' @description
        #' Start a persistent downloader daemon.
        #'
        #' @param port Optional localhost TCP port. If `NULL`, a random high
        #'        port is chosen.
        #' @param heartbeat_interval Seconds between daemon heartbeat checks.
        #' @return A one-row data frame describing the daemon.
        # daemon_start {{{
        daemon_start = function(port = NULL, heartbeat_interval = 5) {
            private$require_manifest()
            checkmate::assert_count(port, positive = TRUE, null.ok = TRUE)
            checkmate::assert_number(
                heartbeat_interval,
                lower = 0.25,
                finite = TRUE
            )
            active <- private$active_daemon()
            if (nrow(active)) {
                return(active)
            }
            if (is.null(port)) {
                port <- sample(30000:60999, 1L)
            }
            daemon_id <- downloader__id("daemon")
            log_dir <- file.path(dirname(private$manifest_path), "logs")
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
            now <- downloader__now()
            daemon <- data.frame(
                daemon_id = daemon_id,
                status = "starting",
                pid = NA_integer_,
                hostname = downloader__hostname(),
                port = as.integer(port),
                token = substr(
                    downloader__hash(daemon_id, stats::runif(1L), now),
                    1L,
                    32L
                ),
                heartbeat_interval = as.numeric(heartbeat_interval),
                started_at = as.POSIXct(NA),
                heartbeat_at = as.POSIXct(NA),
                stopped_at = as.POSIXct(NA),
                log_path = file.path(log_dir, paste0(daemon_id, ".log")),
                error = NA_character_,
                created_at = now,
                updated_at = now,
                stringsAsFactors = FALSE
            )
            private$with_manifest_lock({
                private$append_rows("download_daemon", daemon)
            })
            ok <- tryCatch(
                {
                    private$launch_process(
                        "daemon",
                        daemon_id,
                        daemon$log_path[[1L]]
                    )
                    TRUE
                },
                error = function(e) {
                    daemon$status <- "error"
                    daemon$error <- conditionMessage(e)
                    private$update_daemon(daemon)
                    FALSE
                }
            )
            if (!isTRUE(ok)) {
                cli::cli_abort(
                    "Failed to launch downloader daemon: {daemon$error[[1L]]}"
                )
            }
            private$select_daemons(daemon_id = daemon_id)
        },
        # }}}
        # daemon_status
        #' @description
        #' Return downloader daemon status records.
        # daemon_status {{{
        daemon_status = function() {
            private$require_manifest()
            daemons <- private$select_daemons()
            if (!nrow(daemons)) {
                return(daemons)
            }
            stale <- daemons$status %in%
                c("starting", "running") &
                !is.na(daemons$pid) &
                !vapply(daemons$pid, downloader__pid_alive, logical(1L))
            if (any(stale)) {
                daemons$status[stale] <- "stale"
                daemons$error[stale] <- "Daemon process is not running."
                daemons$updated_at[stale] <- downloader__now()
                private$replace_rows(
                    "download_daemon",
                    daemons[stale, , drop = FALSE],
                    "daemon_id"
                )
            }
            daemons
        },
        # }}}
        # daemon_stop
        #' @description
        #' Request the running downloader daemon to stop.
        #'
        #' @param force Whether to kill the daemon process immediately.
        # daemon_stop {{{
        daemon_stop = function(force = FALSE) {
            private$require_manifest()
            checkmate::assert_flag(force)
            daemon <- private$active_daemon()
            if (!nrow(daemon)) {
                return(self$daemon_status())
            }
            private$append_control("daemon", daemon$daemon_id[[1L]], "stop")
            daemon$status <- "stopping"
            private$update_daemon(daemon)
            private$wake_daemon(
                command = "stop-daemon",
                id = daemon$daemon_id[[1L]],
                attempts = 3L
            )
            if (isTRUE(force)) {
                downloader__pid_kill(daemon$pid[[1L]])
            }
            self$daemon_status()
        },
        # }}}
        # sessions
        #' @description
        #' List persistent download sessions.
        # sessions {{{
        sessions = function() {
            private$require_manifest()
            private$read_table("download_session")
        },
        # }}}
        # tasks
        #' @description
        #' List persistent download tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param job_id Optional background job ID.
        #' @param status Optional task status filter.
        # tasks {{{
        tasks = function(session_id = NULL, job_id = NULL, status = NULL) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_string(job_id, null.ok = TRUE)
            checkmate::assert_subset(
                status,
                DOWNLOADER_TASK_STATUS,
                empty.ok = TRUE
            )
            private$decorate_task_status(private$select_tasks(
                session_id = session_id,
                job_id = job_id,
                status = status
            ))
        },
        # }}}
        # status
        #' @description
        #' Return persistent download task status.
        #'
        #' @param session_id Optional session ID.
        #' @param job_id Optional background job ID.
        #' @param task_id Optional task ID vector.
        #'
        #' @return A data frame of matching task records.
        # status {{{
        status = function(session_id = NULL, job_id = NULL, task_id = NULL) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_string(job_id, null.ok = TRUE)
            checkmate::assert_character(
                task_id,
                any.missing = FALSE,
                null.ok = TRUE
            )
            private$decorate_task_status(private$select_tasks(
                session_id = session_id,
                job_id = job_id,
                task_id = task_id
            ))
        },
        # }}}
        # events
        #' @description
        #' Return persistent downloader event logs.
        #'
        #' @param session_id Optional session ID.
        #' @param job_id Optional background job ID.
        #' @param task_id Optional task ID vector.
        #'
        #' @return A data frame of event records.
        # events {{{
        events = function(session_id = NULL, job_id = NULL, task_id = NULL) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_string(job_id, null.ok = TRUE)
            checkmate::assert_character(
                task_id,
                any.missing = FALSE,
                null.ok = TRUE
            )
            events <- private$read_table("download_event")
            if (!is.null(job_id) && nrow(events)) {
                wanted_job_id <- job_id
                events <- events[
                    events[["job_id"]] %in% wanted_job_id,
                    ,
                    drop = FALSE
                ]
            }
            if (!is.null(session_id) && nrow(events)) {
                wanted_session_id <- session_id
                events <- events[
                    events[["session_id"]] %in% wanted_session_id,
                    ,
                    drop = FALSE
                ]
            }
            if (!is.null(task_id) && nrow(events)) {
                wanted_task_id <- task_id
                events <- events[
                    events[["task_id"]] %in% wanted_task_id,
                    ,
                    drop = FALSE
                ]
            }
            events
        },
        # }}}
        # on
        #' @description
        #' Register an in-session downloader event callback.
        #'
        #' @param event Event name.
        #' @param fun Callback function called with `(event, downloader)`.
        #'
        #' @return A callback token for `$off()`.
        # on {{{
        on = function(event, fun) {
            checkmate::assert_choice(event, DOWNLOADER_CALLBACK_EVENTS)
            checkmate::assert_function(fun)
            token <- downloader__hash(
                event,
                length(private$callbacks) + 1L,
                as.numeric(Sys.time()),
                stats::runif(1L)
            )
            private$callbacks[[token]] <- list(event = event, fun = fun)
            token
        },
        # }}}
        # off
        #' @description
        #' Remove a downloader event callback.
        #'
        #' @param token Callback token returned by `$on()`.
        #'
        #' @return `TRUE` when a callback was removed.
        # off {{{
        off = function(token) {
            checkmate::assert_string(token)
            exists <- token %in% names(private$callbacks)
            if (exists) {
                private$callbacks[[token]] <- NULL
            }
            exists
        },
        # }}}
        # data_nodes
        #' @description
        #' Return historical data node download performance.
        #'
        #' @param service Optional ESGF service filter.
        #'
        #' @return A data frame of data node performance records.
        # data_nodes {{{
        data_nodes = function(service = NULL) {
            private$require_manifest()
            checkmate::assert_string(service, null.ok = TRUE)
            nodes <- private$read_table("download_node")
            if (!is.null(service) && nrow(nodes)) {
                wanted_service <- service
                nodes <- nodes[
                    nodes[["service"]] == wanted_service,
                    ,
                    drop = FALSE
                ]
            }
            nodes
        },
        # }}}
        # reset_data_nodes
        #' @description
        #' Reset historical data-node health records.
        #'
        #' @param data_node Optional data-node host to reset.
        #' @param service Optional ESGF service filter.
        #'
        #' @return The remaining data-node records.
        # reset_data_nodes {{{
        reset_data_nodes = function(data_node = NULL, service = NULL) {
            private$require_manifest()
            checkmate::assert_string(data_node, null.ok = TRUE)
            checkmate::assert_string(service, null.ok = TRUE)
            private$with_manifest_lock({
                nodes <- private$read_table("download_node")
                if (!nrow(nodes)) {
                    return(nodes[])
                }
                remove <- rep(TRUE, nrow(nodes))
                if (!is.null(data_node)) {
                    remove <- remove & nodes[["data_node"]] == data_node
                }
                if (!is.null(service)) {
                    remove <- remove & nodes[["service"]] == service
                }
                remove_rows <- nodes[remove, , drop = FALSE]
                if (nrow(remove_rows)) {
                    for (id in remove_rows$node_id) {
                        downloader__ddb_exec(
                            private$manifest_conn,
                            sprintf(
                                "DELETE FROM %s WHERE %s = %s",
                                downloader__ddb_ident(
                                    private$manifest_conn,
                                    "download_node"
                                ),
                                downloader__ddb_ident(
                                    private$manifest_conn,
                                    "node_id"
                                ),
                                downloader__ddb_literal(
                                    private$manifest_conn,
                                    id
                                )
                            )
                        )
                    }
                }
                private$read_table("download_node")
            })
        },
        # }}}
        # record_probes
        #' @description
        #' Record URL probe outcomes from a download plan into node history.
        #'
        #' @param plan A download plan returned by `$download_plan()`.
        #' @param probed Whether `probe = TRUE` was used to create the plan.
        #'
        #' @return The current data-node records.
        # record_probes {{{
        record_probes = function(plan, probed = TRUE) {
            private$require_manifest()
            checkmate::assert_data_frame(plan)
            checkmate::assert_flag(probed)
            if (!isTRUE(probed) || !nrow(plan)) {
                return(self$data_nodes())
            }
            private$with_manifest_lock({
                private$record_plan_probes(plan)
                private$read_table("download_node")
            })
        },
        # }}}
        # retry
        #' @description
        #' Requeue failed or cancelled persistent tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #' @param status Task statuses to requeue. Default:
        #'        `c("error", "cancelled")`.
        #'
        #' @return A data frame of requeued task records.
        # retry {{{
        retry = function(
            session_id = NULL,
            task_id = NULL,
            status = c("error", "cancelled")
        ) {
            private$require_manifest()
            checkmate::assert_subset(
                status,
                c("error", "cancelled"),
                empty.ok = FALSE
            )
            private$with_manifest_lock({
                private$queue_tasks(
                    session_id = session_id,
                    task_id = task_id,
                    status = status
                )
            })
        },
        # }}}
        # cancel
        #' @description
        #' Cancel queued or in-progress persistent download tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #' @param status Task statuses to cancel. Default:
        #'        `c("queued", "downloading")`.
        #'
        #' @return A data frame of cancelled task records.
        # cancel {{{
        cancel = function(
            session_id = NULL,
            task_id = NULL,
            status = c("queued", "downloading")
        ) {
            private$require_manifest()
            checkmate::assert_string(session_id, null.ok = TRUE)
            checkmate::assert_character(
                task_id,
                any.missing = FALSE,
                null.ok = TRUE
            )
            checkmate::assert_subset(
                status,
                c("queued", "downloading"),
                empty.ok = FALSE
            )

            private$with_manifest_lock({
                tasks <- private$select_tasks(
                    session_id = session_id,
                    task_id = task_id,
                    status = status
                )
                if (!nrow(tasks)) {
                    return(private$decorate_task_status(tasks))
                }
                for (id in tasks$task_id) {
                    active <- private$persistent_tasks[[id]]
                    if (!is.null(active) && !is.null(active$mirai)) {
                        try(mirai::stop_mirai(active$mirai), silent = TRUE)
                        private$persistent_tasks[[id]] <- NULL
                    }
                }

                tasks$status <- "cancelled"
                tasks$last_error <- "Cancelled by user."
                tasks$updated_at <- downloader__now()
                tasks$completed_at <- downloader__now()
                private$replace_rows(
                    "download_task",
                    as.data.frame(tasks),
                    "task_id"
                )
                for (i in seq_len(nrow(tasks))) {
                    private$log_event(
                        tasks$session_id[[i]],
                        tasks$task_id[[i]],
                        "cancelled",
                        tasks$last_error[[i]]
                    )
                }
                for (sid in unique(tasks$session_id)) {
                    private$update_session_status(sid)
                }
                private$decorate_task_status(private$select_tasks(
                    session_id = session_id,
                    task_id = tasks$task_id
                ))
            })
        },
        # }}}
        # resume
        #' @description
        #' Resume queued or interrupted persistent tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #' @param ... Additional arguments passed to `$run()`.
        #'
        #' @return A data frame of selected task records after the run.
        # resume {{{
        resume = function(session_id = NULL, task_id = NULL, ...) {
            private$require_manifest()
            private$with_manifest_lock({
                private$queue_tasks(
                    session_id = session_id,
                    task_id = task_id,
                    status = c("downloading", "error", "cancelled")
                )
            })
            self$run(session_id = session_id, task_id = task_id, ...)
        },
        # }}}
        # verify
        #' @description
        #' Verify checksums for completed persistent tasks.
        #'
        #' @param session_id Optional session ID.
        #' @param task_id Optional task ID vector.
        #'
        #' @return A data frame of completed task records with `checksum_ok` and
        #'        `integrity_evidence` columns. Read both together with task status:
        #'        `"publisher_checksum"` identifies a supplied expected checksum,
        #'        `"saved_transfer"` identifies complete saved byte-producing transfer
        #'        evidence without a publisher checksum, and `"missing"` identifies
        #'        absent historical evidence. Generic `checksum_ok` can be `TRUE` on
        #'        file existence alone when neither evidence type is available. A local
        #'        hash does not establish publisher checksum verification or authorize
        #'        Store source adoption by itself.
        # verify {{{
        verify = function(session_id = NULL, task_id = NULL) {
            private$require_manifest()
            private$with_manifest_lock({
                tasks <- private$select_tasks(
                    session_id = session_id,
                    task_id = task_id,
                    status = c("done", "skipped")
                )
                integrity_errors <- rep(NA_character_, nrow(tasks))
                tasks$checksum_ok <- if (nrow(tasks)) {
                    vapply(
                        seq_len(nrow(tasks)),
                        function(i) {
                            path <- tasks$target_path[[i]]
                            checksum <- downloader__one_chr(tasks$checksum[[i]])
                            checksum_type <- downloader__checksum_type(tasks$checksum_type[[
                                i
                            ]])
                            if (!file.exists(path)) {
                                return(FALSE)
                            }
                            if (
                                !is.na(checksum) &&
                                    !private$verify_checksum_internal(
                                        path,
                                        checksum,
                                        checksum_type
                                    )
                            ) {
                                return(FALSE)
                            }
                            integrity_errors[[
                                i
                            ]] <<- private$task_target_error(tasks[
                                i,
                                ,
                                drop = FALSE
                            ])
                            is.na(integrity_errors[[i]])
                        },
                        logical(1L)
                    )
                } else {
                    logical()
                }
                failed <- tasks[!tasks$checksum_ok, , drop = FALSE]
                if (nrow(failed)) {
                    failed$status <- "error"
                    failed$last_error <- integrity_errors[!tasks$checksum_ok]
                    failed$last_error[is.na(
                        failed$last_error
                    )] <- "Checksum verification failed."
                    failed$completed_at <- downloader__now()
                    private$replace_rows(
                        "download_task",
                        failed[,
                            setdiff(names(failed), "checksum_ok"),
                            drop = FALSE
                        ],
                        "task_id"
                    )
                    for (i in seq_len(nrow(failed))) {
                        private$log_event(
                            failed$session_id[[i]],
                            failed$task_id[[i]],
                            "verify_error",
                            failed$last_error[[i]]
                        )
                    }
                    for (sid in unique(failed$session_id)) {
                        private$update_session_status(sid)
                    }
                    tasks$status[!tasks$checksum_ok] <- "error"
                    tasks$last_error[
                        !tasks$checksum_ok
                    ] <- failed$last_error
                }
                # Classify the original evidence separately from the permissive
                # generic checksum result when no publisher hash was supplied.
                tasks <- private$decorate_task_status(tasks)
                tasks$integrity_evidence <- vapply(
                    seq_len(nrow(tasks)),
                    function(i) {
                        if (!is.na(downloader__one_chr(tasks$checksum[[i]]))) {
                            "publisher_checksum"
                        } else if (
                            isTRUE(tasks$transfer_complete[[i]]) &&
                                !is.na(tasks$transfer_sha256[[i]])
                        ) {
                            "saved_transfer"
                        } else {
                            "missing"
                        }
                    },
                    character(1L)
                )
                tasks
            })
        },
        # }}}
        # cleanup_tmp
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
        # cleanup_tmp {{{
        cleanup_tmp = function(all = FALSE) {
            checkmate::assert_flag(all)

            tmp_files <- list.files(
                private$temp,
                pattern = "\\.(part|done)$",
                full.names = TRUE
            )
            piece_dirs <- list.files(
                private$temp,
                pattern = "\\.pieces$",
                full.names = TRUE
            )

            if (length(tmp_files) == 0 && length(piece_dirs) == 0) {
                downloader__verbose(cli::cli_alert_info(
                    "No temporary files to clean up."
                ))
                return(0L)
            }

            if (all) {
                removed_files <- if (length(tmp_files)) {
                    sum(file.remove(tmp_files), na.rm = TRUE)
                } else {
                    0L
                }
                removed_dirs <- 0L
                for (dir in piece_dirs) {
                    if (dir.exists(dir)) {
                        unlink(dir, recursive = TRUE, force = TRUE)
                        removed_dirs <- removed_dirs + 1L
                    }
                }
                if (!is.null(private$manifest_path)) {
                    private$require_manifest()
                    private$with_manifest_lock({
                        downloader__ddb_exec(
                            private$manifest_conn,
                            sprintf(
                                "DELETE FROM %s",
                                downloader__ddb_ident(
                                    private$manifest_conn,
                                    "download_piece"
                                )
                            )
                        )
                    })
                }
                removed <- as.integer(removed_files + removed_dirs)
                downloader__verbose(cli::cli_alert_success(
                    "Removed {.var {removed}} temporary item{?s}."
                ))
                return(removed)
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
                data_files <- list.files(
                    private$dest,
                    recursive = TRUE,
                    full.names = TRUE
                )

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
                removed_files <- sum(file.remove(to_remove), na.rm = TRUE)
                downloader__verbose(cli::cli_alert_success(
                    "Removed {.var {length(to_remove)}} orphaned temporary file{?s}."
                ))
                return(as.integer(removed_files))
            } else {
                downloader__verbose(cli::cli_alert_info(
                    "No orphaned temporary files found."
                ))
                return(0L)
            }
        },
        # }}}
        # get_tasks
        #' @description
        #' Get all async download tasks
        #'
        #' @return A list of DownloadTask objects.
        #'
        #' @examples
        #' \dontrun{
        #' tasks <- downloader$get_tasks()
        #' }
        # get_tasks {{{
        get_tasks = function() {
            private$async_tasks
        },
        # }}}
        # get_task_status
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
        # get_task_status {{{
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
                    error <- downloader__worker_error(result)
                    if (nzchar(error)) {
                        task$mark_failed(error)
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
        # wait_for_tasks
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
        # wait_for_tasks {{{
        wait_for_tasks = function(task_ids = NULL, progress = TRUE) {
            checkmate::assert_character(task_ids, null.ok = TRUE)
            checkmate::assert_flag(progress)

            if (is.null(task_ids)) {
                task_ids <- names(private$async_tasks)
            }

            if (length(task_ids) == 0) {
                downloader__verbose(cli::cli_alert_info("No tasks to wait for"))
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

                    error <- downloader__worker_error(result)
                    if (nzchar(error)) {
                        task$mark_failed(error)
                    } else {
                        task$mark_completed()
                        file_result <- result # Save the file path
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
        # cancel_task
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
        # cancel_task {{{
        cancel_task = function(task_id) {
            checkmate::assert_string(task_id)

            task <- private$async_tasks[[task_id]]
            if (is.null(task)) {
                stop(sprintf("Task ID '%s' not found", task_id))
            }

            if (is.null(task$mirai_obj)) {
                cli::cli_alert_warning(
                    "Task has no mirai object, already completed?"
                )
                return(FALSE)
            }

            result <- mirai::stop_mirai(task$mirai_obj)

            if (result) {
                task$mark_failed("Cancelled by user")
                cli::cli_alert_success("Task {task_id} cancelled")
            } else {
                cli::cli_alert_warning(
                    "Task {task_id} could not be cancelled (already completed?)"
                )
            }

            return(result)
        },
        # }}}
        # list_incomplete
        #' @description
        #' List incomplete downloads
        #'
        #' @return A data.frame with information about incomplete downloads.
        #'
        #' @examples
        #' \dontrun{
        #' incomplete <- downloader$list_incomplete()
        #' }
        # list_incomplete {{{
        list_incomplete = function() {
            part_files <- list.files(
                private$temp,
                pattern = "\\.part$",
                full.names = TRUE
            )
            done_files <- list.files(
                private$temp,
                pattern = "\\.done$",
                full.names = TRUE
            )

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
                if (length(files) == 0) {
                    return(NULL)
                }

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
        # verify_checksum
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
        # verify_checksum {{{
        verify_checksum = function(file, expected, type = "sha256") {
            checkmate::assert_file_exists(file)
            checkmate::assert_string(expected)
            checkmate::assert_choice(type, c("md5", "sha256"))

            private$verify_checksum_internal(file, expected, type)
        },
        # }}}
        # print
        #' @description
        #' Print downloader summary
        #'
        #' @return The `Downloader` object itself, invisibly.
        # print {{{
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
                cli::cli_alert_warning(
                    "Incomplete downloads: {nrow(incomplete)}"
                )
            }

            invisible(self)
        }
        # }}}
    ),

    active = list(
        # data_dir
        #' @field data_dir The final data directory
        # data_dir {{{
        data_dir = function() {
            private$dest
        },
        # }}}
        # tmp_dir
        #' @field tmp_dir The temporary files directory
        # tmp_dir {{{
        tmp_dir = function() {
            private$temp
        },
        # }}}
        # max_retries
        #' @field max_retries Maximum number of retry attempts
        # max_retries {{{
        max_retries = function() {
            private$retries
        },
        # }}}
        # timeout
        #' @field timeout Download timeout in seconds
        # timeout {{{
        timeout = function() {
            private$dl_timeout
        },
        # }}}
        # network_policy
        #' @field network_policy Network options passed to libcurl.
        # network_policy {{{
        network_policy = function() {
            list(
                ssl_verifypeer = isTRUE(private$ssl_verifypeer),
                proxy = private$proxy,
                connect_timeout = private$connect_timeout,
                useragent = private$useragent
            )
        },
        # }}}
        # node_policy
        #' @field node_policy Data-node cooldown and ranking policy.
        # node_policy {{{
        node_policy = function() {
            private$node_policy_config
        },
        # }}}
        # transfer_policy
        #' @field transfer_policy Curl transfer policy.
        # transfer_policy {{{
        transfer_policy = function() {
            private$transfer_policy_config
        },
        # }}}
        # resource_policy
        #' @field resource_policy Local resource and scheduling policy.
        # resource_policy {{{
        resource_policy = function() {
            private$resource_policy_config
        },
        # }}}
        # n_workers
        #' @field n_workers Number of parallel workers
        # n_workers {{{
        n_workers = function() {
            private$worker_count
        },
        # }}}
        # manifest
        #' @field manifest Persistent download manifest path, or `NULL`.
        # manifest {{{
        manifest = function() {
            private$manifest_path
        },
        # }}}
        # config
        #' @field config Current downloader configuration as a named list. When
        #'        `manifest` is set, the configuration is stored in the
        #'        manifest's `download_config` table.
        # config {{{
        config = function() {
            private$config_payload()
        }
        # }}}
    ),

    private = list(
        dest = NULL,
        temp = NULL,
        manifest_path = NULL,
        manifest_conn = NULL,
        manifest_owner = NULL,
        dl_timeout = NULL,
        ssl_verifypeer = NULL,
        proxy = NULL,
        connect_timeout = NULL,
        useragent = NULL,
        node_policy_config = NULL,
        transfer_policy_config = NULL,
        resource_policy_config = NULL,
        retries = NULL,
        cleanup = NULL,
        worker_count = NULL,
        compute_profile = NULL,
        compute_owner = NULL,
        async_tasks = NULL, # List of DownloadTask objects for async downloads
        persistent_tasks = NULL,
        callbacks = NULL,
        current_job_id = NULL,
        current_owner_id = NULL,
        lock_depth = 0L,

        # finalize
        # finalize {{{
        finalize = function() {
            private$disconnect_manifest()
            # An instance owns only its named pool; collecting an older instance
            # must not stop another downloader or the host application's tasks.
            if (
                !is.null(private$compute_profile) &&
                    identical(private$compute_owner, private)
            ) {
                mirai::daemons(0, .compute = private$compute_profile)
                private$compute_profile <- NULL
            }
        },
        # }}}
        # config_payload
        # config_payload {{{
        config_payload = function() {
            list(
                schema_version = DOWNLOADER_SCHEMA_VERSION,
                dest = private$dest,
                temp = private$temp,
                manifest = private$manifest_path,
                retries = as.integer(private$retries),
                timeout = as.integer(private$dl_timeout),
                ssl_verifypeer = isTRUE(private$ssl_verifypeer),
                proxy = private$proxy,
                connect_timeout = if (is.null(private$connect_timeout)) {
                    NULL
                } else {
                    as.integer(private$connect_timeout)
                },
                useragent = private$useragent,
                cleanup = isTRUE(private$cleanup),
                n_workers = as.integer(private$worker_count),
                node_policy = private$node_policy_config,
                transfer_policy = private$transfer_policy_config,
                resource_policy = private$resource_policy_config
            )
        },
        # }}}
        # mirai daemon helpers
        # ensure_mirai_daemons {{{
        ensure_mirai_daemons = function() {
            if (is.null(private$worker_count) || private$worker_count <= 0L) {
                return(invisible(FALSE))
            }
            if (!requireNamespace("mirai", quietly = TRUE)) {
                stop(
                    "mirai package required for async downloads",
                    call. = FALSE
                )
            }

            # R6 clones borrow existing resources. A new pool belongs to this
            # instance and needs a distinct profile so its finalizer is isolated.
            if (
                is.null(private$compute_profile) ||
                    !identical(private$compute_owner, private)
            ) {
                private$compute_profile <- basename(tempfile("downloader-"))
                private$compute_owner <- private
            }
            is_set <- tryCatch(
                isTRUE(mirai::daemons_set(.compute = private$compute_profile)),
                error = function(e) FALSE
            )
            status <- tryCatch(
                mirai::status(.compute = private$compute_profile),
                error = function(e) NULL
            )
            daemon_count <- if (
                is.list(status) && !is.null(status$connections)
            ) {
                suppressWarnings(as.integer(status$connections[[1L]]))
            } else if (
                is.list(status) &&
                    !is.null(status$daemons) &&
                    is.numeric(status$daemons)
            ) {
                suppressWarnings(as.integer(status$daemons[[1L]]))
            } else {
                NA_integer_
            }
            needs_start <- !is_set ||
                is.na(daemon_count) ||
                daemon_count < private$worker_count

            if (isTRUE(needs_start)) {
                downloader__start_pool(
                    private$worker_count,
                    .compute = private$compute_profile
                )
            }

            invisible(TRUE)
        },
        # }}}

        # manifest config
        # read_manifest_config {{{
        read_manifest_config = function() {
            if (is.null(private$manifest_conn)) {
                return(NULL)
            }
            rows <- tryCatch(
                private$read_table("download_config"),
                error = function(e) data.frame()
            )
            if (!nrow(rows) || !"config_id" %in% names(rows)) {
                return(NULL)
            }
            rows <- rows[rows$config_id == "default", , drop = FALSE]
            downloader__config_unflatten(rows, manifest = private$manifest_path)
        },
        # }}}

        # write_manifest_config {{{
        write_manifest_config = function() {
            private$replace_rows(
                "download_config",
                downloader__config_flatten(private$config_payload()),
                "config_id"
            )
            invisible(private$config_payload())
        },
        # }}}
        # with_manifest_lock {{{
        with_manifest_lock = function(expr) {
            private$require_manifest()
            if (private$lock_depth > 0L) {
                return(force(expr))
            }
            private$lock_depth <- private$lock_depth + 1L
            on.exit(
                {
                    private$lock_depth <- max(0L, private$lock_depth - 1L)
                },
                add = TRUE
            )
            downloader__manifest_with_lock(private$manifest_path, force(expr))
        },
        # }}}

        # manifest
        # connect_manifest {{{
        connect_manifest = function() {
            if (
                is.null(private$manifest_path) ||
                    !is.null(private$manifest_conn)
            ) {
                return(invisible(NULL))
            }
            private$manifest_conn <- downloader__ddb_connect(
                private$manifest_path,
                read_only = FALSE
            )
            # Keep the creator's environment alive while clones borrow its
            # connection; cloning an ordinary environment preserves its identity.
            private$manifest_owner <- private
            invisible(private$manifest_conn)
        },
        # }}}

        # disconnect_manifest {{{
        disconnect_manifest = function() {
            # A collected clone must not invalidate a connection used by another
            # instance between its validity check and the next SQL statement.
            if (!identical(private$manifest_owner, private)) {
                private$manifest_conn <- NULL
                private$manifest_owner <- NULL
                return(invisible(NULL))
            }
            if (!is.null(private$manifest_conn)) {
                valid <- tryCatch(
                    downloader__ddb_is_valid(private$manifest_conn),
                    error = function(e) FALSE
                )
                if (isTRUE(valid)) {
                    tryCatch(
                        downloader__ddb_disconnect(
                            private$manifest_conn,
                            shutdown = TRUE
                        ),
                        error = function(e) {
                            downloader__ddb_disconnect(private$manifest_conn)
                        }
                    )
                }
                private$manifest_conn <- NULL
            }
            private$manifest_owner <- NULL
            invisible(NULL)
        },
        # }}}

        # require_manifest {{{
        require_manifest = function() {
            if (is.null(private$manifest_path)) {
                cli::cli_abort(
                    "This downloader has no persistent manifest. Create it with {.code Downloader$new(manifest = ...)}."
                )
            }
            if (!is.null(private$manifest_conn)) {
                valid <- tryCatch(
                    downloader__ddb_is_valid(private$manifest_conn),
                    error = function(e) FALSE
                )
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
        # }}}

        # exec_manifest {{{
        exec_manifest = function(sql) {
            downloader__ddb_exec(private$manifest_conn, sql)
        },
        # }}}

        # init_manifest_schema {{{
        init_manifest_schema = function() {
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_config (
                    config_id VARCHAR PRIMARY KEY,
                    schema_version VARCHAR,
                    dest VARCHAR,
                    temp VARCHAR,
                    retries INTEGER,
                    timeout INTEGER,
                    ssl_verifypeer BOOLEAN,
                    proxy VARCHAR,
                    connect_timeout INTEGER,
                    useragent VARCHAR,
                    cleanup BOOLEAN,
                    n_workers INTEGER,
                    node_cooldown_after_failures INTEGER,
                    node_cooldown_seconds INTEGER,
                    node_history_ttl_seconds INTEGER,
                    node_min_attempts INTEGER,
                    transfer_chunk_size DOUBLE,
                    transfer_bandwidth_limit DOUBLE,
                    transfer_low_speed_limit DOUBLE,
                    transfer_low_speed_time DOUBLE,
                    transfer_range_mode VARCHAR,
                    transfer_piece_size INTEGER,
                    transfer_piece_concurrency INTEGER,
                    transfer_max_sources INTEGER,
                    transfer_require_checksum_for_multisource BOOLEAN,
                    transfer_range_probe_timeout INTEGER,
                    resource_host_concurrency INTEGER,
                    resource_disk_preflight BOOLEAN,
                    resource_min_free_space DOUBLE,
                    updated_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_session (
                    session_id VARCHAR PRIMARY KEY,
                    label VARCHAR,
                    status VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP,
                    completed_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
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
                    speed_bps DOUBLE,
                    eta_seconds DOUBLE,
                    progress_updated_at TIMESTAMP,
                    current_url VARCHAR,
                    job_id VARCHAR,
                    owner_id VARCHAR,
                    lease_until TIMESTAMP,
                    heartbeat_at TIMESTAMP,
                    selected_url VARCHAR,
                    data_node VARCHAR,
                    last_error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP,
                    completed_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_candidate (
                    candidate_id VARCHAR PRIMARY KEY,
                    task_id VARCHAR,
                    url VARCHAR,
                    service VARCHAR,
                    data_node VARCHAR,
                    priority INTEGER,
                    probe_latency DOUBLE,
                    probe_throughput DOUBLE,
                    range_supported BOOLEAN,
                    range_size DOUBLE,
                    range_etag VARCHAR,
                    range_last_modified VARCHAR,
                    range_final_url VARCHAR,
                    range_probe_error VARCHAR,
                    range_probed_at TIMESTAMP,
                    failed_count INTEGER,
                    last_error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_piece (
                    piece_id VARCHAR PRIMARY KEY,
                    task_id VARCHAR,
                    piece_index INTEGER,
                    start_byte DOUBLE,
                    end_byte DOUBLE,
                    byte_count DOUBLE,
                    path VARCHAR,
                    status VARCHAR,
                    candidate_id VARCHAR,
                    attempts INTEGER,
                    bytes_done DOUBLE,
                    last_error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP,
                    completed_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_event (
                    event_id VARCHAR PRIMARY KEY,
                    job_id VARCHAR,
                    session_id VARCHAR,
                    task_id VARCHAR,
                    event VARCHAR,
                    message VARCHAR,
                    created_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_job (
                    job_id VARCHAR PRIMARY KEY,
                    session_id VARCHAR,
                    status VARCHAR,
                    mode VARCHAR,
                    pid INTEGER,
                    hostname VARCHAR,
                    store_path VARCHAR,
                    log_path VARCHAR,
                    overwrite BOOLEAN,
                    resume BOOLEAN,
                    bytes_done DOUBLE,
                    bytes_total DOUBLE,
                    speed_bps DOUBLE,
                    active_task_count INTEGER,
                    progress_updated_at TIMESTAMP,
                    started_at TIMESTAMP,
                    heartbeat_at TIMESTAMP,
                    completed_at TIMESTAMP,
                    exit_code INTEGER,
                    error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_daemon (
                    daemon_id VARCHAR PRIMARY KEY,
                    status VARCHAR,
                    pid INTEGER,
                    hostname VARCHAR,
                    port INTEGER,
                    token VARCHAR,
                    heartbeat_interval DOUBLE,
                    started_at TIMESTAMP,
                    heartbeat_at TIMESTAMP,
                    stopped_at TIMESTAMP,
                    log_path VARCHAR,
                    error VARCHAR,
                    created_at TIMESTAMP,
                    updated_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_control (
                    control_id VARCHAR PRIMARY KEY,
                    target_type VARCHAR,
                    target_id VARCHAR,
                    command VARCHAR,
                    status VARCHAR,
                    message VARCHAR,
                    created_at TIMESTAMP,
                    handled_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_node (
                    node_id VARCHAR PRIMARY KEY,
                    data_node VARCHAR,
                    service VARCHAR,
                    success_count INTEGER,
                    failure_count INTEGER,
                    bytes_done DOUBLE,
                    avg_latency DOUBLE,
                    probe_success_count INTEGER,
                    probe_failure_count INTEGER,
                    last_success_at TIMESTAMP,
                    last_failure_at TIMESTAMP,
                    last_probe_at TIMESTAMP,
                    cooldown_until TIMESTAMP,
                    updated_at TIMESTAMP
                )
            "
            )
            private$exec_manifest(
                "
                CREATE TABLE IF NOT EXISTS download_meta (
                    key VARCHAR PRIMARY KEY,
                    value VARCHAR,
                    updated_at TIMESTAMP
                )
            "
            )
            private$migrate_manifest_schema()
            invisible(NULL)
        },
        # }}}

        # migrate_manifest_schema {{{
        migrate_manifest_schema = function() {
            current <- private$manifest_schema_version()
            private$migrate_manifest_schema_to_1(current)
            private$set_manifest_schema_version(DOWNLOADER_SCHEMA_VERSION)
            invisible(NULL)
        },
        # }}}

        # manifest_schema_version {{{
        manifest_schema_version = function() {
            meta <- tryCatch(
                private$read_table("download_meta"),
                error = function(e) data.frame()
            )
            if (!nrow(meta)) {
                return(NA_character_)
            }
            row <- meta[meta[["key"]] == "schema_version", , drop = FALSE]
            if (!nrow(row)) {
                return(NA_character_)
            }
            version <- as.character(row$value[[1L]])
            if (is.na(version) || !nzchar(version)) NA_character_ else version
        },
        # }}}

        # set_manifest_schema_version {{{
        set_manifest_schema_version = function(version) {
            private$replace_rows(
                "download_meta",
                data.frame(
                    key = "schema_version",
                    value = version,
                    updated_at = downloader__now(),
                    stringsAsFactors = FALSE
                ),
                "key"
            )
            invisible(NULL)
        },
        # }}}

        # migrate_manifest_schema_to_1 {{{
        migrate_manifest_schema_to_1 = function(current) {
            if (!is.na(current)) {
                cmp <- tryCatch(
                    utils::compareVersion(current, DOWNLOADER_SCHEMA_VERSION),
                    error = function(e) -1L
                )
                if (cmp > 0L) {
                    cli::cli_abort(
                        "Downloader manifest schema version {.val {current}} is newer than this package supports ({.val {DOWNLOADER_SCHEMA_VERSION}})."
                    )
                }
            }
            private$exec_manifest(
                "ALTER TABLE download_node ADD COLUMN IF NOT EXISTS probe_success_count INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_node ADD COLUMN IF NOT EXISTS probe_failure_count INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_node ADD COLUMN IF NOT EXISTS last_probe_at TIMESTAMP"
            )
            private$exec_manifest(
                "ALTER TABLE download_node ADD COLUMN IF NOT EXISTS cooldown_until TIMESTAMP"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS node_cooldown_after_failures INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS node_cooldown_seconds INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS node_history_ttl_seconds INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS node_min_attempts INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_chunk_size DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_bandwidth_limit DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_low_speed_limit DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_low_speed_time DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_range_mode VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_piece_size INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_piece_concurrency INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_max_sources INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_require_checksum_for_multisource BOOLEAN"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS transfer_range_probe_timeout INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS resource_host_concurrency INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS resource_disk_preflight BOOLEAN"
            )
            private$exec_manifest(
                "ALTER TABLE download_config ADD COLUMN IF NOT EXISTS resource_min_free_space DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_task ADD COLUMN IF NOT EXISTS speed_bps DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_task ADD COLUMN IF NOT EXISTS eta_seconds DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_task ADD COLUMN IF NOT EXISTS progress_updated_at TIMESTAMP"
            )
            private$exec_manifest(
                "ALTER TABLE download_task ADD COLUMN IF NOT EXISTS current_url VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_task ADD COLUMN IF NOT EXISTS job_id VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_task ADD COLUMN IF NOT EXISTS owner_id VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_task ADD COLUMN IF NOT EXISTS lease_until TIMESTAMP"
            )
            private$exec_manifest(
                "ALTER TABLE download_task ADD COLUMN IF NOT EXISTS heartbeat_at TIMESTAMP"
            )
            private$exec_manifest(
                "ALTER TABLE download_event ADD COLUMN IF NOT EXISTS job_id VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS mode VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS store_path VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS overwrite BOOLEAN"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS resume BOOLEAN"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS bytes_done DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS bytes_total DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS speed_bps DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS active_task_count INTEGER"
            )
            private$exec_manifest(
                "ALTER TABLE download_job ADD COLUMN IF NOT EXISTS progress_updated_at TIMESTAMP"
            )
            private$exec_manifest(
                "ALTER TABLE download_candidate ADD COLUMN IF NOT EXISTS range_supported BOOLEAN"
            )
            private$exec_manifest(
                "ALTER TABLE download_candidate ADD COLUMN IF NOT EXISTS range_size DOUBLE"
            )
            private$exec_manifest(
                "ALTER TABLE download_candidate ADD COLUMN IF NOT EXISTS range_etag VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_candidate ADD COLUMN IF NOT EXISTS range_last_modified VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_candidate ADD COLUMN IF NOT EXISTS range_final_url VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_candidate ADD COLUMN IF NOT EXISTS range_probe_error VARCHAR"
            )
            private$exec_manifest(
                "ALTER TABLE download_candidate ADD COLUMN IF NOT EXISTS range_probed_at TIMESTAMP"
            )
            invisible(NULL)
        },
        # }}}

        # append_rows {{{
        append_rows = function(table, rows) {
            if (!nrow(rows)) {
                return(invisible(rows))
            }
            downloader__ddb_append_table(
                private$manifest_conn,
                table,
                as.data.frame(rows)
            )
            invisible(rows)
        },
        # }}}

        # Replace the entire set atomically. If a source worker is retired
        # between the delete and append, its prior task still owns the partial bytes.
        # replace_rows {{{
        replace_rows = function(table, rows, key) {
            if (!nrow(rows)) {
                return(invisible(rows))
            }
            conn <- private$manifest_conn
            downloader__ddb_exec(conn, "BEGIN TRANSACTION")
            committed <- FALSE
            on.exit(
                if (!committed) downloader__ddb_exec(conn, "ROLLBACK"),
                add = TRUE
            )
            for (value in rows[[key]]) {
                downloader__ddb_exec(
                    private$manifest_conn,
                    sprintf(
                        "DELETE FROM %s WHERE %s = %s",
                        downloader__ddb_ident(private$manifest_conn, table),
                        downloader__ddb_ident(private$manifest_conn, key),
                        downloader__ddb_literal(private$manifest_conn, value)
                    )
                )
            }
            downloader__ddb_append_table(
                private$manifest_conn,
                table,
                as.data.frame(rows)
            )
            downloader__ddb_exec(conn, "COMMIT")
            committed <- TRUE
            invisible(rows)
        },
        # }}}

        # read_table {{{
        read_table = function(table) {
            downloader__as_df(downloader__ddb_read_table(
                private$manifest_conn,
                table
            ))
        },
        # }}}

        # select_jobs {{{
        select_jobs = function(job_id = NULL, status = NULL) {
            clauses <- character()
            if (!is.null(job_id)) {
                clauses <- c(
                    clauses,
                    downloader__sql_in(private$manifest_conn, "job_id", job_id)
                )
            }
            if (!is.null(status)) {
                clauses <- c(
                    clauses,
                    downloader__sql_in(private$manifest_conn, "status", status)
                )
            }
            sql <- sprintf(
                "SELECT * FROM %s",
                downloader__ddb_ident(private$manifest_conn, "download_job")
            )
            if (length(clauses)) {
                sql <- paste(sql, "WHERE", paste(clauses, collapse = " AND "))
            }
            sql <- paste(sql, "ORDER BY created_at, job_id")
            downloader__as_df(downloader__ddb_query(private$manifest_conn, sql))
        },
        # }}}

        # select_daemons {{{
        select_daemons = function(daemon_id = NULL, status = NULL) {
            clauses <- character()
            if (!is.null(daemon_id)) {
                clauses <- c(
                    clauses,
                    downloader__sql_in(
                        private$manifest_conn,
                        "daemon_id",
                        daemon_id
                    )
                )
            }
            if (!is.null(status)) {
                clauses <- c(
                    clauses,
                    downloader__sql_in(private$manifest_conn, "status", status)
                )
            }
            sql <- sprintf(
                "SELECT * FROM %s",
                downloader__ddb_ident(private$manifest_conn, "download_daemon")
            )
            if (length(clauses)) {
                sql <- paste(sql, "WHERE", paste(clauses, collapse = " AND "))
            }
            sql <- paste(sql, "ORDER BY created_at DESC, daemon_id")
            downloader__as_df(downloader__ddb_query(private$manifest_conn, sql))
        },
        # }}}

        # select_controls {{{
        select_controls = function(
            target_type = NULL,
            target_id = NULL,
            status = "queued"
        ) {
            clauses <- character()
            if (!is.null(target_type)) {
                clauses <- c(
                    clauses,
                    sprintf(
                        "%s = %s",
                        downloader__ddb_ident(
                            private$manifest_conn,
                            "target_type"
                        ),
                        downloader__ddb_literal(
                            private$manifest_conn,
                            target_type
                        )
                    )
                )
            }
            if (!is.null(target_id)) {
                clauses <- c(
                    clauses,
                    sprintf(
                        "%s = %s",
                        downloader__ddb_ident(
                            private$manifest_conn,
                            "target_id"
                        ),
                        downloader__ddb_literal(
                            private$manifest_conn,
                            target_id
                        )
                    )
                )
            }
            if (!is.null(status)) {
                clauses <- c(
                    clauses,
                    downloader__sql_in(private$manifest_conn, "status", status)
                )
            }
            sql <- sprintf(
                "SELECT * FROM %s",
                downloader__ddb_ident(private$manifest_conn, "download_control")
            )
            if (length(clauses)) {
                sql <- paste(sql, "WHERE", paste(clauses, collapse = " AND "))
            }
            sql <- paste(sql, "ORDER BY created_at, control_id")
            downloader__as_df(downloader__ddb_query(private$manifest_conn, sql))
        },
        # }}}

        # update_job {{{
        update_job = function(job) {
            private$with_manifest_lock({
                job <- downloader__as_df(job)
                job$updated_at <- downloader__now()
                private$replace_rows("download_job", job, "job_id")
                invisible(job)
            })
        },
        # }}}

        # update_daemon {{{
        update_daemon = function(daemon) {
            private$with_manifest_lock({
                daemon <- downloader__as_df(daemon)
                daemon$updated_at <- downloader__now()
                private$replace_rows("download_daemon", daemon, "daemon_id")
                invisible(daemon)
            })
        },
        # }}}

        # update_control {{{
        update_control = function(
            control,
            status = "handled",
            message = NA_character_
        ) {
            private$with_manifest_lock({
                control <- downloader__as_df(control)
                control$status <- status
                control$message <- downloader__one_chr(message)
                control$handled_at <- downloader__now()
                private$replace_rows("download_control", control, "control_id")
                invisible(control)
            })
        },
        # }}}

        # append_control {{{
        append_control = function(
            target_type,
            target_id,
            command,
            message = NA_character_
        ) {
            private$with_manifest_lock({
                control <- data.frame(
                    control_id = downloader__id("ctl"),
                    target_type = target_type,
                    target_id = target_id,
                    command = command,
                    status = "queued",
                    message = downloader__one_chr(message),
                    created_at = downloader__now(),
                    handled_at = as.POSIXct(NA),
                    stringsAsFactors = FALSE
                )
                private$append_rows("download_control", control)
                control
            })
        },
        # }}}

        # job_log_path {{{
        job_log_path = function(job_id) {
            log_dir <- file.path(dirname(private$manifest_path), "logs")
            dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
            file.path(log_dir, paste0(job_id, ".log"))
        },
        # }}}

        # create_job {{{
        create_job = function(
            session_id = NULL,
            task_id = NULL,
            overwrite = FALSE,
            resume = TRUE,
            mode = "process",
            store_path = NULL
        ) {
            private$with_manifest_lock({
                private$cancel_stale_downloading(
                    session_id = session_id,
                    task_id = task_id
                )
                tasks <- private$select_tasks(
                    session_id = session_id,
                    task_id = task_id,
                    status = c("queued", "downloading")
                )
                if (!nrow(tasks)) {
                    cli::cli_abort(
                        "No queued or resumable downloader tasks were selected for background execution."
                    )
                }
                private$assert_disk_preflight(tasks, overwrite = overwrite)
                job_id <- downloader__id("job")
                now <- downloader__now()
                job <- data.frame(
                    job_id = job_id,
                    session_id = downloader__one_chr(session_id),
                    status = "queued",
                    mode = mode,
                    pid = NA_integer_,
                    hostname = downloader__hostname(),
                    store_path = downloader__one_chr(store_path),
                    log_path = private$job_log_path(job_id),
                    overwrite = isTRUE(overwrite),
                    resume = isTRUE(resume),
                    bytes_done = 0,
                    bytes_total = NA_real_,
                    speed_bps = NA_real_,
                    active_task_count = 0L,
                    progress_updated_at = as.POSIXct(NA),
                    started_at = as.POSIXct(NA),
                    heartbeat_at = as.POSIXct(NA),
                    completed_at = as.POSIXct(NA),
                    exit_code = NA_integer_,
                    error = NA_character_,
                    created_at = now,
                    updated_at = now,
                    stringsAsFactors = FALSE
                )
                tasks$job_id <- job_id
                tasks$owner_id <- NA_character_
                tasks$lease_until <- as.POSIXct(NA)
                tasks$heartbeat_at <- as.POSIXct(NA)
                tasks$updated_at <- now
                private$append_rows("download_job", job)
                private$replace_rows("download_task", tasks, "task_id")
                private$update_job_progress(job_id)
                private$log_event(
                    downloader__one_chr(session_id),
                    NA_character_,
                    "job_queued",
                    job_id,
                    job_id = job_id
                )
                private$select_jobs(job_id = job_id)
            })
        },
        # }}}

        # launch_process {{{
        launch_process = function(kind, id, log_path) {
            downloader__launch_process(
                kind,
                id,
                private$manifest_path,
                log_path
            )
        },
        # }}}

        # wake_daemon {{{
        wake_daemon = function(
            command = "wake",
            id = NA_character_,
            attempts = 1L,
            interval = 0.2
        ) {
            attempts <- max(1L, as.integer(attempts[[1L]]))
            for (attempt in seq_len(attempts)) {
                daemon <- private$active_daemon()
                if (!nrow(daemon)) {
                    return(FALSE)
                }
                con <- tryCatch(
                    socketConnection(
                        "127.0.0.1",
                        port = as.integer(daemon$port[[1L]]),
                        blocking = TRUE,
                        open = "r+",
                        timeout = 2
                    ),
                    error = function(e) NULL
                )
                if (!is.null(con)) {
                    on.exit(close(con), add = TRUE)
                    writeLines(
                        paste(
                            daemon$token[[1L]],
                            command,
                            downloader__one_chr(id)
                        ),
                        con
                    )
                    flush(con)
                    ack <- tryCatch(
                        readLines(con, n = 1L, warn = FALSE),
                        error = function(e) character()
                    )
                    return(length(ack) && identical(ack[[1L]], "OK"))
                }
                if (attempt < attempts) {
                    Sys.sleep(interval)
                }
            }
            FALSE
        },
        # }}}

        # active_daemon {{{
        active_daemon = function() {
            daemons <- private$select_daemons(status = c("starting", "running"))
            if (!nrow(daemons)) {
                return(daemons)
            }
            alive <- vapply(daemons$pid, downloader__pid_alive, logical(1L))
            # A starting daemon may not have written its PID yet.
            alive <- alive |
                (daemons$status %in% "starting" & is.na(daemons$pid))
            daemons[alive, , drop = FALSE][1L, , drop = FALSE]
        },
        # }}}

        # mark_job_running {{{
        mark_job_running = function(job_id) {
            private$with_manifest_lock({
                job <- private$select_jobs(job_id = job_id)
                if (!nrow(job)) {
                    cli::cli_abort("Downloader job not found: {.val {job_id}}.")
                }
                now <- downloader__now()
                job$status <- "running"
                job$pid <- as.integer(Sys.getpid())
                job$hostname <- downloader__hostname()
                job$started_at <- now
                job$heartbeat_at <- now
                job$error <- NA_character_
                private$update_job(job)
                private$log_event(
                    job$session_id[[1L]],
                    NA_character_,
                    "job_start",
                    job_id,
                    job_id = job_id
                )
                job
            })
        },
        # }}}

        # complete_job {{{
        complete_job = function(
            job_id,
            status = "done",
            error = NA_character_,
            exit_code = 0L
        ) {
            private$with_manifest_lock({
                job <- private$select_jobs(job_id = job_id)
                if (!nrow(job)) {
                    return(invisible(job))
                }
                now <- downloader__now()
                job$status <- status
                job$completed_at <- now
                job$heartbeat_at <- now
                job$exit_code <- as.integer(exit_code)
                job$error <- downloader__one_chr(error)
                private$update_job_progress(job_id)
                job <- private$select_jobs(job_id = job_id)
                job$status <- status
                job$completed_at <- now
                job$heartbeat_at <- now
                job$exit_code <- as.integer(exit_code)
                job$error <- downloader__one_chr(error)
                private$update_job(job)
                private$log_event(
                    job$session_id[[1L]],
                    NA_character_,
                    paste0("job_", status),
                    downloader__one_chr(error),
                    job_id = job_id
                )
                job
            })
        },
        # }}}

        # job_stop_requested {{{
        job_stop_requested = function(job_id) {
            controls <- private$select_controls(
                target_type = "job",
                target_id = job_id,
                status = "queued"
            )
            any(controls$command %in% "stop")
        },
        # }}}

        # handle_job_controls {{{
        handle_job_controls = function(job_id) {
            controls <- private$select_controls(
                target_type = "job",
                target_id = job_id,
                status = "queued"
            )
            if (!nrow(controls)) {
                return(FALSE)
            }
            stop_rows <- controls[controls$command %in% "stop", , drop = FALSE]
            if (!nrow(stop_rows)) {
                for (i in seq_len(nrow(controls))) {
                    private$update_control(
                        controls[i, , drop = FALSE],
                        status = "handled",
                        message = "ignored"
                    )
                }
                return(FALSE)
            }
            for (i in seq_len(nrow(stop_rows))) {
                private$update_control(
                    stop_rows[i, , drop = FALSE],
                    status = "handled",
                    message = "stop requested"
                )
            }
            TRUE
        },
        # }}}

        # run_job {{{
        run_job = function(job_id) {
            private$require_manifest()
            job <- private$mark_job_running(job_id)
            old_job_id <- private$current_job_id
            old_owner_id <- private$current_owner_id
            private$current_job_id <- job_id
            private$current_owner_id <- paste0("pid-", Sys.getpid())
            on.exit(
                {
                    private$current_job_id <- old_job_id
                    private$current_owner_id <- old_owner_id
                },
                add = TRUE
            )
            status <- "done"
            error <- NA_character_
            exit_code <- 0L
            tryCatch(
                {
                    task_ids <- private$select_tasks(job_id = job_id)$task_id
                    if (!length(task_ids)) {
                        cli::cli_abort(
                            "Downloader job has no assigned tasks: {.val {job_id}}."
                        )
                    }
                    if (private$handle_job_controls(job_id)) {
                        status <<- "cancelled"
                    } else {
                        job <- private$select_jobs(job_id = job_id)
                        tasks <- private$run_selected_tasks(
                            session_id = NULL,
                            task_id = task_ids,
                            progress = FALSE,
                            overwrite = isTRUE(job$overwrite[[1L]]),
                            resume = isTRUE(job$resume[[1L]]),
                            job_id = job_id
                        )
                        if (private$handle_job_controls(job_id)) {
                            status <<- "cancelled"
                        } else if (
                            nrow(tasks) && any(tasks$status %in% "error")
                        ) {
                            status <<- "error"
                            error <<- "One or more downloader tasks failed."
                            exit_code <<- 1L
                        } else if (
                            nrow(tasks) && any(tasks$status %in% "cancelled")
                        ) {
                            status <<- "cancelled"
                        }
                        store_path <- downloader__one_chr(job$store_path[[1L]])
                        if (!is.na(store_path) && status %in% "done") {
                            if (!is.function(DOWNLOADER_RUNTIME$sync_store)) {
                                cli::cli_abort(
                                    "No store synchronization adapter is registered for this download job."
                                )
                            }
                            DOWNLOADER_RUNTIME$sync_store(store_path, self)
                        }
                    }
                },
                error = function(e) {
                    status <<- "error"
                    error <<- conditionMessage(e)
                    exit_code <<- 1L
                }
            )
            private$complete_job(
                job_id,
                status = status,
                error = error,
                exit_code = exit_code
            )
            invisible(private$select_jobs(job_id = job_id))
        },
        # }}}

        # run_selected_tasks {{{
        run_selected_tasks = function(
            session_id = NULL,
            task_id = NULL,
            progress = TRUE,
            overwrite = FALSE,
            resume = TRUE,
            job_id = NULL
        ) {
            private$with_manifest_lock({
                private$cancel_stale_downloading(
                    session_id = session_id,
                    task_id = task_id
                )
                tasks <- private$select_tasks(
                    session_id = session_id,
                    task_id = task_id,
                    status = c("queued", "downloading"),
                    job_id = job_id
                )
                if (!nrow(tasks)) {
                    return(private$decorate_task_status(private$select_tasks(
                        session_id = session_id,
                        task_id = task_id,
                        job_id = job_id
                    )))
                }
                private$assert_disk_preflight(tasks, overwrite = overwrite)
            })
            if (private$worker_count > 1L && nrow(tasks) > 1L) {
                private$run_tasks_concurrent(
                    tasks,
                    progress = progress,
                    overwrite = overwrite,
                    resume = resume
                )
            } else {
                for (i in seq_len(nrow(tasks))) {
                    if (
                        !is.null(job_id) && private$handle_job_controls(job_id)
                    ) {
                        remaining <- private$select_tasks(
                            job_id = job_id,
                            status = c("queued", "downloading")
                        )
                        if (nrow(remaining)) {
                            remaining$status <- "cancelled"
                            remaining$last_error <- "Cancelled by user."
                            remaining$completed_at <- downloader__now()
                            private$update_task(remaining)
                        }
                        break
                    }
                    private$run_task(
                        tasks[i, , drop = FALSE],
                        progress = progress,
                        overwrite = overwrite,
                        resume = resume
                    )
                }
            }
            private$decorate_task_status(private$select_tasks(
                session_id = session_id,
                task_id = task_id,
                job_id = job_id
            ))
        },
        # }}}

        # daemon_stop_requested {{{
        daemon_stop_requested = function(daemon_id) {
            controls <- private$select_controls(
                target_type = "daemon",
                target_id = daemon_id,
                status = "queued"
            )
            if (!nrow(controls)) {
                return(FALSE)
            }
            stop_rows <- controls[controls$command %in% "stop", , drop = FALSE]
            if (!nrow(stop_rows)) {
                for (i in seq_len(nrow(controls))) {
                    private$update_control(
                        controls[i, , drop = FALSE],
                        status = "handled",
                        message = "ignored"
                    )
                }
                return(FALSE)
            }
            for (i in seq_len(nrow(stop_rows))) {
                private$update_control(
                    stop_rows[i, , drop = FALSE],
                    status = "handled",
                    message = "stop requested"
                )
            }
            TRUE
        },
        # }}}

        # daemon_next_job {{{
        daemon_next_job = function() {
            jobs <- private$select_jobs(status = "queued")
            if (nrow(jobs) && "mode" %in% names(jobs)) {
                jobs <- jobs[jobs$mode %in% "daemon", , drop = FALSE]
            }
            if (!nrow(jobs)) {
                return(jobs)
            }
            if ("created_at" %in% names(jobs)) {
                jobs <- jobs[order(jobs$created_at), , drop = FALSE]
            }
            jobs[1L, , drop = FALSE]
        },
        # }}}

        # daemon_heartbeat {{{
        daemon_heartbeat = function(daemon_id) {
            private$with_manifest_lock({
                daemon <- private$select_daemons(daemon_id = daemon_id)
                if (!nrow(daemon)) {
                    return(invisible(daemon))
                }
                now <- downloader__now()
                daemon$status <- "running"
                daemon$pid <- as.integer(Sys.getpid())
                daemon$hostname <- downloader__hostname()
                if (is.na(daemon$started_at[[1L]])) {
                    daemon$started_at <- now
                }
                daemon$heartbeat_at <- now
                daemon$updated_at <- now
                daemon$error <- NA_character_
                private$replace_rows("download_daemon", daemon, "daemon_id")
                invisible(daemon)
            })
        },
        # }}}

        # daemon_mark_stopped {{{
        daemon_mark_stopped = function(
            daemon_id,
            status = "stopped",
            error = NA_character_
        ) {
            private$with_manifest_lock({
                daemon <- private$select_daemons(daemon_id = daemon_id)
                if (!nrow(daemon)) {
                    return(invisible(daemon))
                }
                now <- downloader__now()
                daemon$status <- status
                daemon$stopped_at <- now
                daemon$heartbeat_at <- now
                daemon$updated_at <- now
                daemon$error <- downloader__one_chr(error)
                private$replace_rows("download_daemon", daemon, "daemon_id")
                invisible(daemon)
            })
        },
        # }}}

        # daemon_accept_wake {{{
        daemon_accept_wake = function(server, daemon, timeout = 1) {
            ready <- tryCatch(
                socketSelect(list(server), timeout = timeout),
                error = function(e) FALSE
            )
            if (!isTRUE(ready[[1L]])) {
                return(invisible(FALSE))
            }
            con <- tryCatch(
                socketAccept(server, blocking = TRUE, open = "r+"),
                error = function(e) NULL
            )
            if (is.null(con)) {
                return(invisible(FALSE))
            }
            on.exit(close(con), add = TRUE)
            line <- tryCatch(
                readLines(con, n = 1L, warn = FALSE),
                error = function(e) character()
            )
            parts <- strsplit(
                if (length(line)) line[[1L]] else "",
                "[[:space:]]+"
            )[[1L]]
            token <- if (length(parts) >= 1L) parts[[1L]] else ""
            command <- if (length(parts) >= 2L) parts[[2L]] else ""
            target_id <- if (length(parts) >= 3L) parts[[3L]] else NA_character_
            if (!identical(token, daemon$token[[1L]])) {
                writeLines("ERR", con)
                flush(con)
                return(invisible(FALSE))
            }
            if (identical(command, "stop-daemon")) {
                private$append_control(
                    "daemon",
                    daemon$daemon_id[[1L]],
                    "stop",
                    target_id
                )
            }
            writeLines("OK", con)
            flush(con)
            invisible(TRUE)
        },
        # }}}

        # run_daemon_loop {{{
        run_daemon_loop = function(daemon_id) {
            private$require_manifest()
            daemon <- private$select_daemons(daemon_id = daemon_id)
            if (!nrow(daemon)) {
                cli::cli_abort(
                    "Downloader daemon not found: {.val {daemon_id}}."
                )
            }
            server <- NULL
            opened <- FALSE
            tryCatch(
                {
                    server <- serverSocket(port = as.integer(daemon$port[[1L]]))
                    opened <- TRUE
                },
                error = function(e) {
                    private$daemon_mark_stopped(
                        daemon_id,
                        status = "error",
                        error = conditionMessage(e)
                    )
                    stop(e)
                }
            )
            on.exit(
                {
                    if (!is.null(server)) {
                        try(close(server), silent = TRUE)
                    }
                    if (isTRUE(opened)) {
                        private$daemon_mark_stopped(
                            daemon_id,
                            status = "stopped"
                        )
                    }
                },
                add = TRUE
            )

            repeat {
                daemon <- private$daemon_heartbeat(daemon_id)
                if (private$daemon_stop_requested(daemon_id)) {
                    break
                }
                job <- private$daemon_next_job()
                if (nrow(job)) {
                    private$run_job(job$job_id[[1L]])
                    next
                }
                private$daemon_accept_wake(
                    server,
                    daemon,
                    timeout = daemon$heartbeat_interval[[1L]]
                )
            }
            invisible(private$select_daemons(daemon_id = daemon_id))
        },
        # }}}

        # log_event {{{
        log_event = function(
            session_id,
            task_id,
            event,
            message = NA_character_,
            emit = TRUE,
            job_id = NULL
        ) {
            if (is.null(job_id)) {
                job_id <- private$current_job_id
            }
            private$with_manifest_lock({
                private$append_rows(
                    "download_event",
                    data.frame(
                        event_id = downloader__hash(
                            job_id,
                            session_id,
                            task_id,
                            event,
                            message,
                            as.numeric(Sys.time()),
                            stats::runif(1L)
                        ),
                        job_id = downloader__one_chr(job_id),
                        session_id = downloader__one_chr(session_id),
                        task_id = downloader__one_chr(task_id),
                        event = event,
                        message = downloader__one_chr(message),
                        created_at = downloader__now(),
                        stringsAsFactors = FALSE
                    )
                )
            })
            if (isTRUE(emit)) {
                private$emit_callback_event(
                    event,
                    session_id = session_id,
                    task_id = task_id,
                    message = message
                )
            }
        },
        # }}}
        # callbacks
        callback_event_name = function(event) {
            switch(
                event,
                enqueue = "session_start",
                start = "task_start",
                progress = "task_progress",
                candidate_error = "candidate_error",
                done = "task_done",
                error = "task_error",
                cancelled = "task_cancelled",
                session_done = "session_done",
                NA_character_
            )
        },

        emit_callback_event = function(
            event,
            session_id = NA_character_,
            task_id = NA_character_,
            message = NA_character_
        ) {
            callback_event <- private$callback_event_name(event)
            if (is.na(callback_event) || !length(private$callbacks)) {
                return(invisible(NULL))
            }
            payload <- private$callback_payload(
                callback_event,
                session_id = session_id,
                task_id = task_id,
                message = message
            )
            for (token in names(private$callbacks)) {
                callback <- private$callbacks[[token]]
                if (
                    is.null(callback) ||
                        !identical(callback$event, callback_event)
                ) {
                    next
                }
                tryCatch(
                    callback$fun(payload, self),
                    error = function(e) {
                        private$log_event(
                            payload$session_id,
                            payload$task_id,
                            "callback_error",
                            conditionMessage(e),
                            emit = FALSE
                        )
                    }
                )
            }
            invisible(NULL)
        },

        callback_payload = function(
            event,
            session_id = NA_character_,
            task_id = NA_character_,
            message = NA_character_
        ) {
            session_id <- downloader__one_chr(session_id)
            task_id <- downloader__one_chr(task_id)
            message <- downloader__one_chr(message)
            task <- data.frame()
            if (!is.na(task_id) && nzchar(task_id)) {
                task <- tryCatch(
                    private$select_tasks(task_id = task_id),
                    error = function(e) data.frame()
                )
            }
            task_value <- function(column, default = NA_character_) {
                if (!nrow(task) || !column %in% names(task)) {
                    return(default)
                }
                value <- task[[column]][[1L]]
                if (is.null(value) || !length(value) || is.na(value)) {
                    return(default)
                }
                value
            }
            status <- downloader__one_chr(task_value("status", NA_character_))
            if (is.na(status) && identical(event, "session_done")) {
                status <- message
            }
            error <- downloader__one_chr(task_value(
                "last_error",
                NA_character_
            ))
            if (
                is.na(error) &&
                    event %in%
                        c("candidate_error", "task_error", "task_cancelled")
            ) {
                error <- message
            }
            bytes_done <- suppressWarnings(as.numeric(task_value(
                "bytes_done",
                NA_real_
            )))
            if (!length(bytes_done) || is.na(bytes_done)) {
                bytes_done <- NA_real_
            }
            list(
                event = event,
                session_id = session_id,
                task_id = task_id,
                file_key = downloader__one_chr(task_value(
                    "file_key",
                    NA_character_
                )),
                status = status,
                target_path = downloader__one_chr(task_value(
                    "target_path",
                    NA_character_
                )),
                selected_url = downloader__one_chr(task_value(
                    "selected_url",
                    NA_character_
                )),
                data_node = downloader__one_chr(task_value(
                    "data_node",
                    NA_character_
                )),
                filename = downloader__one_chr(task_value(
                    "filename",
                    NA_character_
                )),
                size = suppressWarnings(as.numeric(task_value(
                    "size",
                    NA_real_
                ))),
                bytes_done = bytes_done,
                speed_bps = suppressWarnings(as.numeric(task_value(
                    "speed_bps",
                    NA_real_
                ))),
                eta_seconds = suppressWarnings(as.numeric(task_value(
                    "eta_seconds",
                    NA_real_
                ))),
                error = error,
                created_at = downloader__now(),
                message = message
            )
        },
        # plan and task helpers
        # new_session_id {{{
        new_session_id = function() {
            stamp <- format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")
            paste0(
                stamp,
                "-",
                substr(downloader__hash(stamp, stats::runif(1L)), 1L, 8L)
            )
        },
        # }}}

        # normalize_plan {{{
        normalize_plan = function(plan) {
            if (!is.data.frame(plan)) {
                cli::cli_abort(
                    "`plan` must be a data frame returned by {.code download_plan()}."
                )
            }
            plan <- downloader__as_df(plan)

            missing <- setdiff(
                c("logical_file_id", "filename", "url"),
                names(plan)
            )
            if (length(missing)) {
                cli::cli_abort(
                    "Download plan is missing required column(s): {.field {missing}}."
                )
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
            plan <- plan[!is.na(plan$url) & nzchar(plan$url), , drop = FALSE]
            if (!nrow(plan)) {
                return(plan)
            }

            plan$checksum_type <- vapply(
                plan$checksum_type,
                downloader__checksum_type,
                character(1L)
            )
            priority <- suppressWarnings(as.integer(plan$priority))
            missing_priority <- is.na(priority)
            priority[missing_priority] <- seq_len(nrow(plan))[missing_priority]
            plan$priority <- priority
            plan$size <- suppressWarnings(as.numeric(plan$size))
            filename <- as.character(plan$filename)
            missing_filename <- is.na(filename) | !nzchar(filename)
            filename[missing_filename] <- basename(sub(
                "\\?.*$",
                "",
                plan$url[missing_filename]
            ))
            plan$filename <- filename
            rownames(plan) <- NULL
            plan
        },
        # }}}

        # target_path {{{
        target_path = function(filename, subdir = NA_character_) {
            if (is.na(subdir) || !nzchar(subdir)) {
                file.path(private$dest, filename)
            } else {
                file.path(private$dest, subdir, filename)
            }
        },
        # }}}

        # task_needs_download {{{
        task_needs_download = function(task, overwrite = FALSE) {
            task <- downloader__as_df(task)
            if (!nrow(task)) {
                return(logical())
            }
            if (isTRUE(overwrite)) {
                return(rep(TRUE, nrow(task)))
            }
            vapply(
                seq_len(nrow(task)),
                function(i) {
                    target <- task$target_path[[i]]
                    if (!file.exists(target)) {
                        return(TRUE)
                    }
                    checksum <- downloader__one_chr(task$checksum[[i]])
                    checksum_type <- downloader__checksum_type(task$checksum_type[[
                        i
                    ]])
                    if (is.na(checksum)) {
                        return(FALSE)
                    }
                    !private$verify_checksum_internal(
                        target,
                        checksum,
                        checksum_type
                    )
                },
                logical(1L)
            )
        },
        # }}}

        # preflight_task_space {{{
        preflight_task_space = function(tasks, overwrite = FALSE) {
            tasks <- downloader__as_df(tasks)
            if (!nrow(tasks)) {
                needed <- tasks
                needs_download <- logical()
            } else {
                needs_download <- private$task_needs_download(
                    tasks,
                    overwrite = overwrite
                )
                needed <- tasks[needs_download, , drop = FALSE]
            }
            size <- if (nrow(needed) && "size" %in% names(needed)) {
                suppressWarnings(as.numeric(needed$size))
            } else {
                numeric()
            }
            unknown <- is.na(size) | size < 0
            required_bytes <- sum(size[!unknown], na.rm = TRUE)
            dest_free <- downloader__disk_free_bytes(private$dest)
            tmp_free <- downloader__disk_free_bytes(private$temp)
            min_free <- private$resource_policy_config$min_free_space
            check_one <- function(free) {
                if (is.na(free)) {
                    return(NA)
                }
                (free - required_bytes) >= min_free
            }
            dest_ok <- check_one(dest_free)
            tmp_ok <- check_one(tmp_free)
            disk_preflight <- isTRUE(
                private$resource_policy_config$disk_preflight
            )
            would_block <- isTRUE(disk_preflight) &&
                (identical(dest_ok, FALSE) || identical(tmp_ok, FALSE))
            disk_ok <- if (!isTRUE(disk_preflight)) {
                TRUE
            } else if (isTRUE(would_block)) {
                FALSE
            } else if (is.na(dest_ok) || is.na(tmp_ok) || any(unknown)) {
                NA
            } else {
                TRUE
            }
            downloader__df(
                task_count = as.integer(nrow(tasks)),
                needs_download = as.integer(sum(needs_download)),
                required_bytes = as.numeric(required_bytes),
                size_unknown_count = as.integer(sum(unknown)),
                dest_free_bytes = as.numeric(dest_free),
                tmp_free_bytes = as.numeric(tmp_free),
                min_free_space = as.numeric(min_free),
                dest_disk_ok = dest_ok,
                tmp_disk_ok = tmp_ok,
                disk_ok = disk_ok,
                disk_would_block = would_block,
                disk_preflight = disk_preflight
            )
        },
        # }}}

        # assert_disk_preflight {{{
        assert_disk_preflight = function(tasks, overwrite = FALSE) {
            summary <- private$preflight_task_space(
                tasks,
                overwrite = overwrite
            )
            if (!isTRUE(summary$disk_would_block[[1L]])) {
                return(invisible(summary))
            }
            cli::cli_abort(c(
                "Insufficient disk space for the selected download tasks.",
                "i" = "Required known bytes: {downloader__format_bytes(summary$required_bytes[[1L]])}.",
                "i" = "Free space in destination: {downloader__format_bytes(summary$dest_free_bytes[[1L]])}.",
                "i" = "Free space in temporary directory: {downloader__format_bytes(summary$tmp_free_bytes[[1L]])}.",
                "i" = "Minimum free-space reserve: {downloader__format_bytes(summary$min_free_space[[1L]])}."
            ))
        },
        # }}}

        # plan_tasks {{{
        plan_tasks = function(plan, session_id, now) {
            task_cols <- c(
                "logical_file_id",
                "file_key",
                "esgf_id",
                "dataset_id",
                "filename",
                "subdir",
                "checksum",
                "checksum_type",
                "size"
            )
            task <- unique(plan[, task_cols, drop = FALSE])
            task$target_path <- mapply(
                private$target_path,
                task$filename,
                task$subdir,
                USE.NAMES = FALSE
            )
            task$task_id <- vapply(
                seq_len(nrow(task)),
                function(i) {
                    downloader__hash(
                        session_id,
                        task$logical_file_id[[i]],
                        task$target_path[[i]]
                    )
                },
                character(1L)
            )
            task$session_id <- session_id
            task$status <- "queued"
            task$attempts <- 0L
            task$bytes_done <- 0
            task$speed_bps <- NA_real_
            task$eta_seconds <- NA_real_
            task$progress_updated_at <- as.POSIXct(NA)
            task$current_url <- NA_character_
            task$job_id <- NA_character_
            task$owner_id <- NA_character_
            task$lease_until <- as.POSIXct(NA)
            task$heartbeat_at <- as.POSIXct(NA)
            task$selected_url <- NA_character_
            task$data_node <- NA_character_
            task$last_error <- NA_character_
            task$created_at <- now
            task$updated_at <- now
            task$completed_at <- as.POSIXct(NA)
            task <- task[,
                c(
                    "task_id",
                    "session_id",
                    "logical_file_id",
                    "file_key",
                    "esgf_id",
                    "dataset_id",
                    "filename",
                    "subdir",
                    "target_path",
                    "checksum",
                    "checksum_type",
                    "size",
                    "status",
                    "attempts",
                    "bytes_done",
                    "speed_bps",
                    "eta_seconds",
                    "progress_updated_at",
                    "current_url",
                    "job_id",
                    "owner_id",
                    "lease_until",
                    "heartbeat_at",
                    "selected_url",
                    "data_node",
                    "last_error",
                    "created_at",
                    "updated_at",
                    "completed_at"
                ),
                drop = FALSE
            ]
            rownames(task) <- NULL
            task
        },
        # }}}

        # plan_candidates {{{
        plan_candidates = function(plan, tasks, now) {
            key <- c(
                "logical_file_id",
                "filename",
                "subdir",
                "checksum",
                "checksum_type"
            )
            plan$.downloader_row <- seq_len(nrow(plan))
            candidate <- merge(
                plan,
                tasks[, c(key, "task_id"), drop = FALSE],
                by = key,
                all.x = TRUE,
                sort = FALSE
            )
            candidate <- candidate[
                order(candidate$.downloader_row),
                ,
                drop = FALSE
            ]
            candidate$.downloader_row <- NULL
            candidate$candidate_id <- vapply(
                seq_len(nrow(candidate)),
                function(i) {
                    downloader__hash(candidate$task_id[[i]], candidate$url[[i]])
                },
                character(1L)
            )
            candidate <- downloader__df(
                candidate_id = candidate$candidate_id,
                task_id = candidate$task_id,
                url = candidate$url,
                service = candidate$service,
                data_node = candidate$data_node,
                priority = as.integer(candidate$priority),
                probe_latency = as.numeric(candidate$probe_latency),
                probe_throughput = as.numeric(candidate$probe_throughput),
                range_supported = NA,
                range_size = NA_real_,
                range_etag = NA_character_,
                range_last_modified = NA_character_,
                range_final_url = NA_character_,
                range_probe_error = NA_character_,
                range_probed_at = as.POSIXct(NA),
                failed_count = 0L,
                last_error = NA_character_,
                created_at = now,
                updated_at = now
            )
            candidate <- candidate[
                !duplicated(candidate$candidate_id),
                ,
                drop = FALSE
            ]
            rownames(candidate) <- NULL
            candidate
        },
        # }}}

        # select_tasks {{{
        select_tasks = function(
            session_id = NULL,
            task_id = NULL,
            status = NULL,
            job_id = NULL
        ) {
            clauses <- character()
            if (!is.null(session_id)) {
                clauses <- c(
                    clauses,
                    sprintf(
                        "%s = %s",
                        downloader__ddb_ident(
                            private$manifest_conn,
                            "session_id"
                        ),
                        downloader__ddb_literal(
                            private$manifest_conn,
                            session_id
                        )
                    )
                )
            }
            if (!is.null(job_id)) {
                clauses <- c(
                    clauses,
                    sprintf(
                        "%s = %s",
                        downloader__ddb_ident(private$manifest_conn, "job_id"),
                        downloader__ddb_literal(private$manifest_conn, job_id)
                    )
                )
            }
            if (!is.null(task_id)) {
                clauses <- c(
                    clauses,
                    downloader__sql_in(
                        private$manifest_conn,
                        "task_id",
                        task_id
                    )
                )
            }
            if (!is.null(status)) {
                clauses <- c(
                    clauses,
                    downloader__sql_in(private$manifest_conn, "status", status)
                )
            }
            sql <- sprintf(
                "SELECT * FROM %s",
                downloader__ddb_ident(private$manifest_conn, "download_task")
            )
            if (length(clauses)) {
                sql <- paste(sql, "WHERE", paste(clauses, collapse = " AND "))
            }
            sql <- paste(sql, "ORDER BY created_at, task_id")
            downloader__as_df(downloader__ddb_query(private$manifest_conn, sql))
        },
        # }}}

        # decorate_task_status {{{
        decorate_task_status = function(tasks) {
            tasks <- downloader__as_df(tasks)
            evidence_fields <- c(
                "origin",
                "sources",
                "request_url",
                "final_url",
                "etag",
                "last_modified",
                "date",
                "validator_kind",
                "validator_value",
                "validator_bound",
                "size",
                "bytes",
                "sha256",
                "generation",
                "complete",
                "checksum_verified"
            )
            evidence <- lapply(tasks$task_id, function(id) {
                downloader__worker_dependencies()$transfer_read(file.path(
                    private$temp,
                    paste0(id, ".part")
                ))
            })
            for (field in evidence_fields) {
                missing <- if (field %in% c("size", "bytes")) {
                    NA_real_
                } else if (
                    field %in%
                        c("complete", "checksum_verified", "validator_bound")
                ) {
                    NA
                } else {
                    NA_character_
                }
                tasks[[paste0("transfer_", field)]] <- vapply(
                    evidence,
                    function(state) {
                        if (is.null(state) || is.null(state[[field]])) {
                            missing
                        } else {
                            state[[field]]
                        }
                    },
                    missing
                )
            }
            if (!nrow(tasks)) {
                tasks$candidate_count <- integer()
                tasks$failed_candidate_count <- integer()
                tasks$selected_data_node <- character()
                return(tasks)
            }
            summary <- downloader__as_df(downloader__ddb_query(
                private$manifest_conn,
                sprintf(
                    "SELECT task_id, COUNT(*)::INTEGER AS candidate_count, SUM(CASE WHEN COALESCE(failed_count, 0) > 0 THEN 1 ELSE 0 END)::INTEGER AS failed_candidate_count FROM %s WHERE %s GROUP BY task_id",
                    downloader__ddb_ident(
                        private$manifest_conn,
                        "download_candidate"
                    ),
                    downloader__sql_in(
                        private$manifest_conn,
                        "task_id",
                        tasks$task_id
                    )
                )
            ))
            if (!nrow(summary)) {
                tasks$candidate_count <- 0L
                tasks$failed_candidate_count <- 0L
                tasks$selected_data_node <- tasks$data_node
                return(tasks)
            }
            original_order <- seq_len(nrow(tasks))
            tasks$.downloader_order <- original_order
            tasks <- merge(
                tasks,
                summary,
                by = "task_id",
                all.x = TRUE,
                sort = FALSE
            )
            tasks <- tasks[order(tasks$.downloader_order), , drop = FALSE]
            tasks$.downloader_order <- NULL
            tasks$candidate_count[is.na(tasks$candidate_count)] <- 0L
            tasks$failed_candidate_count[is.na(
                tasks$failed_candidate_count
            )] <- 0L
            tasks$selected_data_node <- tasks$data_node
            rownames(tasks) <- NULL
            tasks
        },
        # }}}

        # cancel_stale_downloading {{{
        cancel_stale_downloading = function(session_id = NULL, task_id = NULL) {
            tasks <- private$select_tasks(
                session_id = session_id,
                task_id = task_id,
                status = "downloading"
            )
            if (!nrow(tasks)) {
                return(invisible(tasks))
            }
            now <- downloader__now()
            lease <- if ("lease_until" %in% names(tasks)) {
                tasks$lease_until
            } else {
                rep(as.POSIXct(NA), nrow(tasks))
            }
            has_lease <- !is.na(lease)
            lease_stale <- has_lease & lease < now
            legacy_stale <- !has_lease &
                tasks[["updated_at"]] < DOWNLOADER_SESSION_STARTED_AT

            requeue <- tasks[lease_stale, , drop = FALSE]
            if (nrow(requeue)) {
                requeue$status <- "queued"
                requeue$last_error <- "Download lease expired; requeued."
                requeue$owner_id <- NA_character_
                requeue$lease_until <- as.POSIXct(NA)
                requeue$heartbeat_at <- as.POSIXct(NA)
                requeue$updated_at <- now
                requeue$completed_at <- as.POSIXct(NA)
                private$replace_rows(
                    "download_task",
                    as.data.frame(requeue),
                    "task_id"
                )
                for (i in seq_len(nrow(requeue))) {
                    private$log_event(
                        requeue$session_id[[i]],
                        requeue$task_id[[i]],
                        "stale_requeue",
                        requeue$last_error[[i]],
                        job_id = requeue$job_id[[i]]
                    )
                }
            }

            stale <- tasks[legacy_stale, , drop = FALSE]
            if (nrow(stale)) {
                stale$status <- "cancelled"
                stale$last_error <- "Download was left in progress by a previous R session."
                stale$updated_at <- now
                stale$completed_at <- now
                private$replace_rows(
                    "download_task",
                    as.data.frame(stale),
                    "task_id"
                )
                for (i in seq_len(nrow(stale))) {
                    private$log_event(
                        stale$session_id[[i]],
                        stale$task_id[[i]],
                        "cancelled",
                        stale$last_error[[i]],
                        job_id = stale$job_id[[i]]
                    )
                }
            }
            for (sid in unique(c(stale$session_id, requeue$session_id))) {
                private$update_session_status(sid)
            }
            invisible(downloader__rbind_fill(list(requeue, stale)))
        },
        # }}}

        # get_candidates {{{
        get_candidates = function(task_id) {
            sql <- sprintf(
                "SELECT * FROM %s WHERE %s = %s ORDER BY priority ASC NULLS LAST, failed_count ASC NULLS LAST, created_at ASC, candidate_id ASC",
                downloader__ddb_ident(
                    private$manifest_conn,
                    "download_candidate"
                ),
                downloader__ddb_ident(private$manifest_conn, "task_id"),
                downloader__ddb_literal(
                    private$manifest_conn,
                    downloader__one_chr(task_id)
                )
            )
            downloader__as_df(downloader__ddb_query(private$manifest_conn, sql))
        },
        # }}}

        # candidate_host {{{
        candidate_host = function(candidate) {
            candidate <- downloader__as_df(candidate)
            if (!nrow(candidate)) {
                return(NA_character_)
            }
            host <- downloader__one_chr(candidate$data_node[[1L]])
            if (is.na(host)) {
                host <- downloader__url_host(candidate$url[[1L]])
            }
            host
        },
        # }}}

        # running_host_counts {{{
        running_host_counts = function(running) {
            if (!length(running)) {
                return(integer())
            }
            hosts <- vapply(
                running,
                function(item) {
                    host <- downloader__one_chr(item$host)
                    if (is.na(host)) "" else host
                },
                character(1L)
            )
            hosts <- hosts[nzchar(hosts)]
            if (!length(hosts)) {
                return(integer())
            }
            table(hosts)
        },
        # }}}

        # host_has_capacity {{{
        host_has_capacity = function(host, active_hosts) {
            limit <- private$resource_policy_config$host_concurrency
            if (is.null(limit) || is.na(limit)) {
                return(TRUE)
            }
            host <- downloader__one_chr(host)
            if (is.na(host)) {
                return(TRUE)
            }
            current <- if (host %in% names(active_hosts)) {
                active_hosts[[host]]
            } else {
                NA_integer_
            }
            if (is.na(current)) {
                current <- 0L
            }
            as.integer(current) < as.integer(limit)
        },
        # }}}

        # update_task {{{
        update_task = function(task) {
            private$with_manifest_lock({
                task <- downloader__as_df(task)
                now <- downloader__now()
                task$updated_at <- now
                if ("status" %in% names(task)) {
                    downloading <- task$status %in% "downloading"
                    terminal <- task$status %in%
                        c("done", "error", "cancelled", "skipped")
                    if (any(downloading)) {
                        if (!is.null(private$current_job_id)) {
                            task$job_id[downloading] <- private$current_job_id
                        }
                        if (!is.null(private$current_owner_id)) {
                            task$owner_id[
                                downloading
                            ] <- private$current_owner_id
                        }
                        task$heartbeat_at[downloading] <- now
                        task$lease_until[downloading] <- now +
                            DOWNLOADER_JOB_LEASE_SECONDS
                    }
                    if (any(terminal)) {
                        task$owner_id[terminal] <- NA_character_
                        task$lease_until[terminal] <- as.POSIXct(NA)
                        task$heartbeat_at[terminal] <- now
                        missing_progress_time <- terminal &
                            is.na(task$progress_updated_at)
                        task$progress_updated_at[missing_progress_time] <- now
                        missing_current_url <- terminal &
                            (is.na(task$current_url) |
                                !nzchar(task$current_url))
                        task$current_url[
                            missing_current_url
                        ] <- task$selected_url[missing_current_url]
                        task$speed_bps[terminal] <- NA_real_
                        task$eta_seconds[terminal] <- NA_real_
                    }
                    queued <- task$status %in% "queued"
                    if (any(queued)) {
                        task$owner_id[queued] <- NA_character_
                        task$lease_until[queued] <- as.POSIXct(NA)
                        task$heartbeat_at[queued] <- as.POSIXct(NA)
                    }
                }
                private$replace_rows(
                    "download_task",
                    as.data.frame(task),
                    "task_id"
                )
                job_ids <- unique(task$job_id[
                    !is.na(task$job_id) & nzchar(task$job_id)
                ])
                for (job_id in job_ids) {
                    private$update_job_progress(job_id)
                }
                private$update_session_status(task$session_id[[1L]])
                invisible(task)
            })
        },
        # }}}

        # update_task_progress {{{
        update_task_progress = function(
            task_id,
            bytes_done,
            speed_bps = NA_real_,
            eta_seconds = NA_real_,
            current_url = NA_character_
        ) {
            updated <- private$with_manifest_lock({
                task <- private$select_tasks(task_id = task_id)
                if (!nrow(task)) {
                    return(invisible(task))
                }
                now <- downloader__now()
                task$bytes_done <- as.numeric(bytes_done)
                task$speed_bps <- as.numeric(speed_bps)
                task$eta_seconds <- as.numeric(eta_seconds)
                task$progress_updated_at <- now
                task$current_url <- downloader__one_chr(current_url)
                task$heartbeat_at <- now
                if (
                    !is.na(task$status[[1L]]) &&
                        identical(task$status[[1L]], "downloading")
                ) {
                    task$lease_until <- now + DOWNLOADER_JOB_LEASE_SECONDS
                    if (!is.null(private$current_job_id)) {
                        task$job_id <- private$current_job_id
                    }
                    if (!is.null(private$current_owner_id)) {
                        task$owner_id <- private$current_owner_id
                    }
                }
                private$replace_rows("download_task", task, "task_id")
                job_id <- downloader__one_chr(task$job_id[[1L]])
                if (!is.na(job_id)) {
                    private$update_job_progress(job_id)
                }
                task
            })
            # Emit after releasing the manifest lock because workflow callbacks
            # may inspect aggregate session tasks to build byte/file progress.
            if (nrow(updated)) {
                private$emit_callback_event(
                    "progress",
                    session_id = updated$session_id[[1L]],
                    task_id = updated$task_id[[1L]]
                )
            }
            invisible(updated)
        },
        # }}}

        # update_job_progress {{{
        update_job_progress = function(job_id) {
            job_id <- downloader__one_chr(job_id)
            if (is.na(job_id)) {
                return(invisible(NULL))
            }
            jobs <- private$read_table("download_job")
            idx <- match(job_id, jobs$job_id)
            if (is.na(idx)) {
                return(invisible(NULL))
            }
            tasks <- private$select_tasks(job_id = job_id)
            bytes_done <- if (nrow(tasks)) {
                sum(
                    suppressWarnings(as.numeric(tasks$bytes_done)),
                    na.rm = TRUE
                )
            } else {
                0
            }
            size <- if (nrow(tasks) && "size" %in% names(tasks)) {
                suppressWarnings(as.numeric(tasks$size))
            } else {
                numeric()
            }
            bytes_total <- if (length(size) && any(!is.na(size) & size >= 0)) {
                sum(size[!is.na(size) & size >= 0], na.rm = TRUE)
            } else {
                NA_real_
            }
            active <- if (nrow(tasks)) {
                tasks$status %in% "downloading"
            } else {
                logical()
            }
            speed <- if (any(active)) {
                sum(
                    suppressWarnings(as.numeric(tasks$speed_bps[active])),
                    na.rm = TRUE
                )
            } else {
                NA_real_
            }
            jobs$bytes_done[[idx]] <- as.numeric(bytes_done)
            jobs$bytes_total[[idx]] <- as.numeric(bytes_total)
            jobs$speed_bps[[idx]] <- as.numeric(speed)
            jobs$active_task_count[[idx]] <- as.integer(sum(active))
            jobs$progress_updated_at[[idx]] <- downloader__now()
            jobs$updated_at[[idx]] <- downloader__now()
            private$replace_rows(
                "download_job",
                jobs[idx, , drop = FALSE],
                "job_id"
            )
            invisible(jobs[idx, , drop = FALSE])
        },
        # }}}

        # update_candidate {{{
        update_candidate = function(candidate) {
            private$with_manifest_lock({
                candidate$updated_at <- downloader__now()
                private$replace_rows(
                    "download_candidate",
                    as.data.frame(candidate),
                    "candidate_id"
                )
                invisible(candidate)
            })
        },
        # }}}

        # update_node_stats {{{
        update_node_stats = function(candidate, ok, bytes_done = 0) {
            candidate <- downloader__as_df(candidate)
            data_node <- downloader__one_chr(candidate$data_node[[1L]])
            if (is.na(data_node)) {
                data_node <- downloader__url_host(candidate$url[[1L]])
            }
            if (is.na(data_node)) {
                return(invisible(NULL))
            }
            service <- downloader__one_chr(candidate$service[[1L]])
            if (is.na(service)) {
                service <- "HTTPServer"
            }
            node_id <- downloader__hash(service, data_node)
            wanted_node_id <- node_id
            now <- downloader__now()
            nodes <- private$read_table("download_node")
            row <- nodes[nodes[["node_id"]] == wanted_node_id, , drop = FALSE]
            if (!nrow(row)) {
                row <- downloader__df(
                    node_id = node_id,
                    data_node = data_node,
                    service = service,
                    success_count = 0L,
                    failure_count = 0L,
                    bytes_done = 0,
                    avg_latency = NA_real_,
                    probe_success_count = 0L,
                    probe_failure_count = 0L,
                    last_success_at = as.POSIXct(NA),
                    last_failure_at = as.POSIXct(NA),
                    last_probe_at = as.POSIXct(NA),
                    cooldown_until = as.POSIXct(NA),
                    updated_at = now
                )
            }
            for (col in c("probe_success_count", "probe_failure_count")) {
                if (!col %in% names(row)) {
                    row[[col]] <- 0L
                }
            }
            for (col in c("last_probe_at", "cooldown_until")) {
                if (!col %in% names(row)) {
                    row[[col]] <- as.POSIXct(NA)
                }
            }

            latency <- suppressWarnings(as.numeric(candidate$probe_latency[[
                1L
            ]]))
            if (isTRUE(ok)) {
                successes <- suppressWarnings(as.integer(row$success_count[[
                    1L
                ]]))
                if (is.na(successes)) {
                    successes <- 0L
                }
                previous_latency <- suppressWarnings(as.numeric(row$avg_latency[[
                    1L
                ]]))
                previous_bytes <- suppressWarnings(as.numeric(row$bytes_done[[
                    1L
                ]]))
                if (is.na(previous_bytes)) {
                    previous_bytes <- 0
                }
                row$success_count <- successes + 1L
                row$bytes_done <- previous_bytes + bytes_done
                if (!is.na(latency)) {
                    row$avg_latency <- if (is.na(previous_latency)) {
                        latency
                    } else {
                        ((previous_latency * successes) + latency) /
                            (successes + 1L)
                    }
                }
                row$last_success_at <- now
                row$cooldown_until <- as.POSIXct(NA)
            } else {
                failures <- suppressWarnings(as.integer(row$failure_count[[
                    1L
                ]]))
                if (is.na(failures)) {
                    failures <- 0L
                }
                row$failure_count <- failures + 1L
                row$last_failure_at <- now
                attempts <- suppressWarnings(as.integer(row$success_count[[
                    1L
                ]])) +
                    row$failure_count[[1L]]
                if (is.na(attempts)) {
                    attempts <- row$failure_count[[1L]]
                }
                if (
                    row$failure_count[[1L]] >=
                        private$node_policy_config$cooldown_after_failures &&
                        attempts >= private$node_policy_config$min_attempts
                ) {
                    row$cooldown_until <- now +
                        private$node_policy_config$cooldown_seconds
                }
            }
            row$updated_at <- now
            private$replace_rows("download_node", as.data.frame(row), "node_id")
            invisible(row)
        },
        # }}}

        # update_node_probe {{{
        update_node_probe = function(plan_row, ok) {
            plan_row <- downloader__as_df(plan_row)
            data_node <- downloader__one_chr(plan_row$data_node[[1L]])
            if (is.na(data_node)) {
                data_node <- downloader__url_host(plan_row$url[[1L]])
            }
            if (is.na(data_node)) {
                return(invisible(NULL))
            }
            service <- downloader__one_chr(plan_row$service[[1L]])
            if (is.na(service)) {
                service <- "HTTPServer"
            }
            node_id <- downloader__hash(service, data_node)
            wanted_node_id <- node_id
            now <- downloader__now()
            nodes <- private$read_table("download_node")
            row <- nodes[nodes[["node_id"]] == wanted_node_id, , drop = FALSE]
            if (!nrow(row)) {
                row <- downloader__df(
                    node_id = node_id,
                    data_node = data_node,
                    service = service,
                    success_count = 0L,
                    failure_count = 0L,
                    bytes_done = 0,
                    avg_latency = NA_real_,
                    probe_success_count = 0L,
                    probe_failure_count = 0L,
                    last_success_at = as.POSIXct(NA),
                    last_failure_at = as.POSIXct(NA),
                    last_probe_at = as.POSIXct(NA),
                    cooldown_until = as.POSIXct(NA),
                    updated_at = now
                )
            }
            for (col in c("probe_success_count", "probe_failure_count")) {
                if (!col %in% names(row)) {
                    row[[col]] <- 0L
                }
                value <- suppressWarnings(as.integer(row[[col]][[1L]]))
                if (is.na(value)) row[[col]] <- 0L
            }
            for (col in c("last_probe_at", "cooldown_until")) {
                if (!col %in% names(row)) {
                    row[[col]] <- as.POSIXct(NA)
                }
            }
            latency <- suppressWarnings(as.numeric(plan_row$probe_latency[[
                1L
            ]]))
            if (isTRUE(ok)) {
                successes <- suppressWarnings(as.integer(row$probe_success_count[[
                    1L
                ]]))
                if (is.na(successes)) {
                    successes <- 0L
                }
                row$probe_success_count <- successes + 1L
                previous_latency <- suppressWarnings(as.numeric(row$avg_latency[[
                    1L
                ]]))
                if (!is.na(latency)) {
                    row$avg_latency <- if (is.na(previous_latency)) {
                        latency
                    } else {
                        ((previous_latency * successes) + latency) /
                            (successes + 1L)
                    }
                }
            } else {
                failures <- suppressWarnings(as.integer(row$probe_failure_count[[
                    1L
                ]]))
                if (is.na(failures)) {
                    failures <- 0L
                }
                row$probe_failure_count <- failures + 1L
                total_failures <- suppressWarnings(as.integer(row$failure_count[[
                    1L
                ]])) +
                    row$probe_failure_count[[1L]]
                total_attempts <- suppressWarnings(as.integer(row$success_count[[
                    1L
                ]])) +
                    suppressWarnings(as.integer(row$failure_count[[1L]])) +
                    suppressWarnings(as.integer(row$probe_success_count[[
                        1L
                    ]])) +
                    row$probe_failure_count[[1L]]
                if (is.na(total_failures)) {
                    total_failures <- row$probe_failure_count[[1L]]
                }
                if (is.na(total_attempts)) {
                    total_attempts <- total_failures
                }
                if (
                    total_failures >=
                        private$node_policy_config$cooldown_after_failures &&
                        total_attempts >=
                            private$node_policy_config$min_attempts
                ) {
                    row$cooldown_until <- now +
                        private$node_policy_config$cooldown_seconds
                }
            }
            row$last_probe_at <- now
            row$updated_at <- now
            private$replace_rows("download_node", as.data.frame(row), "node_id")
            invisible(row)
        },
        # }}}

        # record_plan_probes {{{
        record_plan_probes = function(plan) {
            plan <- downloader__as_df(plan)
            if (!nrow(plan) || !"probe_latency" %in% names(plan)) {
                return(invisible(NULL))
            }
            for (i in seq_len(nrow(plan))) {
                if (
                    "probe_cached" %in%
                        names(plan) &&
                        isTRUE(plan$probe_cached[[i]])
                ) {
                    next
                }
                ok <- !is.na(suppressWarnings(as.numeric(plan$probe_latency[[
                    i
                ]])))
                private$update_node_probe(plan[i, , drop = FALSE], ok = ok)
            }
            invisible(NULL)
        },
        # }}}

        # update_session_status {{{
        update_session_status = function(session_id) {
            private$with_manifest_lock({
                sessions <- private$read_table("download_session")
                idx <- match(session_id, sessions$session_id)
                if (is.na(idx)) {
                    return(invisible(NULL))
                }
                tasks <- private$select_tasks(session_id = session_id)
                was_complete <- !is.na(sessions$completed_at[idx])
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
                sessions$updated_at[idx] <- downloader__now()
                if (status %in% c("done", "error", "cancelled")) {
                    sessions$completed_at[idx] <- downloader__now()
                }
                private$replace_rows(
                    "download_session",
                    sessions[idx, , drop = FALSE],
                    "session_id"
                )
                if (
                    status %in%
                        c("done", "error", "cancelled") &&
                        !isTRUE(was_complete)
                ) {
                    private$log_event(
                        session_id,
                        NA_character_,
                        "session_done",
                        status
                    )
                }
                invisible(status)
            })
        },
        # }}}

        # queue_tasks {{{
        queue_tasks = function(
            session_id = NULL,
            task_id = NULL,
            status = c("error", "cancelled")
        ) {
            tasks <- private$select_tasks(
                session_id = session_id,
                task_id = task_id,
                status = status
            )
            if (!nrow(tasks)) {
                return(tasks)
            }
            tasks$status <- "queued"
            tasks$last_error <- NA_character_
            tasks$updated_at <- downloader__now()
            tasks$completed_at <- as.POSIXct(NA)
            private$replace_rows(
                "download_task",
                as.data.frame(tasks),
                "task_id"
            )
            for (sid in unique(tasks$session_id)) {
                private$update_session_status(sid)
                private$log_event(
                    sid,
                    NA_character_,
                    "retry",
                    sprintf(
                        "Requeued %d task(s).",
                        sum(tasks$session_id == sid)
                    )
                )
            }
            private$select_tasks(session_id = session_id, task_id = task_id)
        },
        # }}}

        # select_pieces {{{
        select_pieces = function(task_id) {
            sql <- sprintf(
                "SELECT * FROM %s WHERE %s = %s ORDER BY piece_index ASC",
                downloader__ddb_ident(private$manifest_conn, "download_piece"),
                downloader__ddb_ident(private$manifest_conn, "task_id"),
                downloader__ddb_literal(
                    private$manifest_conn,
                    downloader__one_chr(task_id)
                )
            )
            downloader__as_df(downloader__ddb_query(private$manifest_conn, sql))
        },
        # }}}

        # delete_task_pieces {{{
        delete_task_pieces = function(task_id, files = FALSE) {
            pieces <- private$select_pieces(task_id)
            if (isTRUE(files) && nrow(pieces) && "path" %in% names(pieces)) {
                paths <- pieces$path[!is.na(pieces$path) & nzchar(pieces$path)]
                unlink(c(
                    paths,
                    paste0(paths, ".part"),
                    paste0(paths, ".transfer.json")
                ))
                for (dir in unique(dirname(paths))) {
                    if (
                        dir.exists(dir) &&
                            !length(list.files(
                                dir,
                                all.files = TRUE,
                                no.. = TRUE
                            ))
                    ) {
                        unlink(dir, recursive = TRUE)
                    }
                }
            }
            downloader__ddb_exec(
                private$manifest_conn,
                sprintf(
                    "DELETE FROM %s WHERE %s = %s",
                    downloader__ddb_ident(
                        private$manifest_conn,
                        "download_piece"
                    ),
                    downloader__ddb_ident(private$manifest_conn, "task_id"),
                    downloader__ddb_literal(
                        private$manifest_conn,
                        downloader__one_chr(task_id)
                    )
                )
            )
            invisible(pieces)
        },
        # }}}

        # update_piece_results {{{
        update_piece_results = function(pieces) {
            private$with_manifest_lock({
                pieces <- downloader__as_df(pieces)
                if (!nrow(pieces)) {
                    return(invisible(pieces))
                }
                now <- downloader__now()
                pieces$updated_at <- now
                if (!"completed_at" %in% names(pieces)) {
                    pieces$completed_at <- as.POSIXct(NA)
                }
                pieces$completed_at[pieces$status %in% "done"] <- now
                private$replace_rows("download_piece", pieces, "piece_id")
                for (task_id in unique(pieces$task_id)) {
                    task <- private$select_tasks(task_id = task_id)
                    if (!nrow(task)) {
                        next
                    }
                    all_pieces <- private$select_pieces(task_id)
                    done_bytes <- sum(
                        suppressWarnings(as.numeric(all_pieces$bytes_done)),
                        na.rm = TRUE
                    )
                    private$update_task_progress(
                        task_id,
                        done_bytes,
                        current_url = task$current_url[[1L]]
                    )
                }
                invisible(pieces)
            })
        },
        # }}}

        # cleanup_task_pieces {{{
        cleanup_task_pieces = function(task_id) {
            private$delete_task_pieces(task_id, files = TRUE)
            invisible(NULL)
        },
        # }}}

        # probe_range_candidates {{{
        probe_range_candidates = function(candidates) {
            candidates <- downloader__as_df(candidates)
            if (!nrow(candidates)) {
                return(candidates)
            }
            now <- downloader__now()
            for (i in seq_len(nrow(candidates))) {
                already <- "range_probed_at" %in%
                    names(candidates) &&
                    !is.na(candidates$range_probed_at[[i]]) &&
                    "range_supported" %in% names(candidates) &&
                    !is.na(candidates$range_supported[[i]])
                if (isTRUE(already)) {
                    next
                }
                probe <- downloader__range_probe_url(
                    candidates$url[[i]],
                    timeout = private$transfer_policy_config$range_probe_timeout,
                    connect_timeout = private$connect_timeout,
                    ssl_verifypeer = private$ssl_verifypeer,
                    proxy = private$proxy,
                    useragent = private$useragent,
                    transfer_policy = private$transfer_policy_config
                )
                candidates$range_supported[[i]] <- isTRUE(probe$range_supported)
                candidates$range_size[[i]] <- as.numeric(probe$range_size)
                candidates$range_etag[[i]] <- downloader__one_chr(
                    probe$range_etag
                )
                candidates$range_last_modified[[i]] <- downloader__one_chr(
                    probe$range_last_modified
                )
                candidates$range_final_url[[i]] <- downloader__one_chr(
                    probe$range_final_url
                )
                candidates$range_probe_error[[i]] <- downloader__one_chr(
                    probe$range_probe_error
                )
                candidates$range_probed_at[[i]] <- now
                private$update_candidate(candidates[i, , drop = FALSE])
            }
            candidates
        },
        # }}}

        # range_supported_candidates {{{
        range_supported_candidates = function(candidates) {
            candidates <- downloader__as_df(candidates)
            if (
                !nrow(candidates) || !"range_supported" %in% names(candidates)
            ) {
                return(candidates[0L, , drop = FALSE])
            }
            size <- suppressWarnings(as.numeric(candidates$range_size))
            candidates[
                candidates$range_supported %in% TRUE & !is.na(size) & size > 0,
                ,
                drop = FALSE
            ]
        },
        # }}}

        # prepare_task_pieces {{{
        prepare_task_pieces = function(task, candidates, size, resume = TRUE) {
            task <- downloader__as_df(task)
            candidates <- downloader__as_df(candidates)
            task_id <- task$task_id[[1L]]
            piece_size <- as.numeric(private$transfer_policy_config$piece_size)
            starts <- seq(0, as.numeric(size) - 1, by = piece_size)
            ends <- pmin(starts + piece_size - 1, as.numeric(size) - 1)
            counts <- ends - starts + 1
            piece_dir <- file.path(private$temp, paste0(task_id, ".pieces"))
            dir.create(piece_dir, recursive = TRUE, showWarnings = FALSE)
            existing <- if (isTRUE(resume)) {
                private$select_pieces(task_id)
            } else {
                data.frame()
            }
            existing_by_index <- if (nrow(existing)) {
                stats::setNames(
                    seq_len(nrow(existing)),
                    as.character(existing$piece_index)
                )
            } else {
                integer()
            }
            candidate_id <- candidates$candidate_id[
                ((seq_along(starts) - 1L) %% nrow(candidates)) + 1L
            ]
            now <- downloader__now()
            pieces <- downloader__df(
                piece_id = vapply(
                    seq_along(starts),
                    function(i) {
                        downloader__hash(task_id, i, starts[[i]], ends[[i]])
                    },
                    character(1L)
                ),
                task_id = task_id,
                piece_index = as.integer(seq_along(starts)),
                start_byte = as.numeric(starts),
                end_byte = as.numeric(ends),
                byte_count = as.numeric(counts),
                path = file.path(
                    piece_dir,
                    sprintf("%06d.done", seq_along(starts))
                ),
                status = "pending",
                candidate_id = candidate_id,
                attempts = 0L,
                bytes_done = 0,
                last_error = NA_character_,
                created_at = now,
                updated_at = now,
                completed_at = as.POSIXct(NA)
            )
            if (nrow(existing)) {
                for (i in seq_len(nrow(pieces))) {
                    old_idx <- existing_by_index[[as.character(pieces$piece_index[[
                        i
                    ]])]]
                    if (is.null(old_idx) || is.na(old_idx)) {
                        next
                    }
                    old <- existing[old_idx, , drop = FALSE]
                    same_range <- identical(
                        as.numeric(old$start_byte[[1L]]),
                        pieces$start_byte[[i]]
                    ) &&
                        identical(
                            as.numeric(old$end_byte[[1L]]),
                            pieces$end_byte[[i]]
                        ) &&
                        identical(
                            as.numeric(old$byte_count[[1L]]),
                            pieces$byte_count[[i]]
                        )
                    valid_file <- same_range &&
                        file.exists(pieces$path[[i]]) &&
                        identical(
                            as.numeric(
                                file.info(
                                    pieces$path[[i]],
                                    extra_cols = FALSE
                                )$size
                            ),
                            pieces$byte_count[[i]]
                        )
                    if (isTRUE(valid_file)) {
                        pieces$status[[i]] <- "done"
                        pieces$attempts[[
                            i
                        ]] <- suppressWarnings(as.integer(old$attempts[[1L]]))
                        if (is.na(pieces$attempts[[i]])) {
                            pieces$attempts[[i]] <- 0L
                        }
                        pieces$bytes_done[[i]] <- pieces$byte_count[[i]]
                        pieces$completed_at[[i]] <- old$completed_at[[1L]]
                    }
                }
            }
            private$delete_task_pieces(task_id, files = !isTRUE(resume))
            private$append_rows("download_piece", pieces)
            pieces
        },
        # }}}

        # prepare_segmented_attempt {{{
        prepare_segmented_attempt = function(
            task,
            candidates,
            preferred_candidate = NULL,
            resume = TRUE
        ) {
            mode <- private$transfer_policy_config$range_mode
            if (identical(mode, "off")) {
                return(NULL)
            }
            candidates <- private$probe_range_candidates(candidates)
            supported <- private$range_supported_candidates(candidates)
            if (!nrow(supported)) {
                return(NULL)
            }
            checksum <- downloader__one_chr(task$checksum[[1L]])
            require_checksum <- isTRUE(
                private$transfer_policy_config$require_checksum_for_multisource
            )
            same_size <- function(x) {
                size <- suppressWarnings(as.numeric(x$range_size))
                all(abs(size - size[[1L]]) < 1)
            }
            if (
                mode %in%
                    c("multi", "auto") &&
                    nrow(supported) >= 2L &&
                    (!require_checksum || !is.na(checksum)) &&
                    same_size(supported)
            ) {
                max_sources <- max(
                    1L,
                    as.integer(private$transfer_policy_config$max_sources)
                )
                use <- supported[
                    seq_len(min(max_sources, nrow(supported))),
                    ,
                    drop = FALSE
                ]
                pieces <- private$prepare_task_pieces(
                    task,
                    use,
                    size = use$range_size[[1L]],
                    resume = resume
                )
                return(list(
                    attempt = TRUE,
                    mode_used = "multi",
                    candidates = use,
                    pieces = pieces
                ))
            }
            if (mode %in% c("single", "multi", "auto")) {
                use <- supported[1L, , drop = FALSE]
                if (!is.null(preferred_candidate)) {
                    preferred_id <- downloader__one_chr(preferred_candidate$candidate_id[[
                        1L
                    ]])
                    hit <- supported[
                        supported$candidate_id == preferred_id,
                        ,
                        drop = FALSE
                    ]
                    if (nrow(hit)) {
                        use <- hit[1L, , drop = FALSE]
                    } else if (identical(mode, "single")) {
                        return(NULL)
                    }
                }
                pieces <- private$prepare_task_pieces(
                    task,
                    use,
                    size = use$range_size[[1L]],
                    resume = resume
                )
                return(list(
                    attempt = TRUE,
                    mode_used = "single",
                    candidates = use,
                    pieces = pieces
                ))
            }
            NULL
        },
        # }}}

        # run_segmented_attempt {{{
        run_segmented_attempt = function(
            task,
            candidates,
            preferred_candidate = NULL,
            overwrite = FALSE,
            resume = TRUE
        ) {
            attempt <- private$prepare_segmented_attempt(
                task,
                candidates = candidates,
                preferred_candidate = preferred_candidate,
                resume = resume
            )
            if (is.null(attempt)) {
                return(list(attempted = FALSE))
            }
            task_subdir <- downloader__one_chr(task$subdir[[1L]])
            if (is.na(task_subdir)) {
                task_subdir <- NULL
            }
            result <- downloader__worker_segmented_download(
                candidates = attempt$candidates,
                pieces = attempt$pieces,
                filename = task$filename[[1L]],
                subdir = task_subdir,
                dest = private$dest,
                temp = private$temp,
                retries = private$retries,
                timeout = private$dl_timeout,
                overwrite = overwrite,
                checksum = downloader__one_chr(task$checksum[[1L]]),
                checksum_type = downloader__checksum_type(task$checksum_type[[
                    1L
                ]]),
                tmp_id = task$task_id[[1L]],
                ssl_verifypeer = private$ssl_verifypeer,
                proxy = private$proxy,
                connect_timeout = private$connect_timeout,
                useragent = private$useragent,
                transfer_policy = private$transfer_policy_config,
                mode_used = attempt$mode_used,
                worker_dependencies = downloader__worker_dependencies()
            )
            if (!is.null(result$pieces)) {
                private$update_piece_results(result$pieces)
            }
            if (isTRUE(result$ok)) {
                private$cleanup_task_pieces(task$task_id[[1L]])
            } else if (isTRUE(result$integrity_error)) {
                private$cleanup_task_pieces(task$task_id[[1L]])
                private$log_event(
                    task$session_id[[1L]],
                    task$task_id[[1L]],
                    "segment_integrity_error",
                    result$error
                )
            }
            result$attempted <- TRUE
            result$attempt <- attempt
            result
        },
        # }}}

        # run_tasks_concurrent {{{
        run_tasks_concurrent = function(
            tasks,
            progress = TRUE,
            overwrite = FALSE,
            resume = TRUE
        ) {
            private$ensure_mirai_daemons()
            tasks <- downloader__as_df(tasks)
            pending <- tasks
            pending$tried_candidate_id <- I(rep(
                list(character()),
                nrow(pending)
            ))
            running <- list()
            active_targets <- character()
            completed <- 0L
            total <- nrow(tasks)

            progress_id <- NULL
            if (isTRUE(progress)) {
                progress_id <- cli::cli_progress_bar(
                    "Downloading files",
                    total = total,
                    .auto_close = FALSE
                )
                on.exit(cli::cli_progress_done(id = progress_id), add = TRUE)
            }

            tick <- function() {
                completed <<- completed + 1L
                if (!is.null(progress_id)) {
                    cli::cli_progress_update(id = progress_id)
                }
            }

            add_running <- function(item) {
                running[[item$task_id]] <<- item
                private$persistent_tasks[[item$task_id]] <- item
                active_targets <<- unique(c(active_targets, item$target_path))
            }

            while (nrow(pending) || length(running)) {
                blocked <- rep(FALSE, nrow(pending))
                while (
                    length(running) < private$worker_count && nrow(pending)
                ) {
                    available <- !pending$target_path %in% active_targets &
                        !blocked
                    if (!any(available)) {
                        break
                    }
                    idx <- which(available)[[1L]]
                    tried <- pending$tried_candidate_id[[idx]]
                    task <- pending[
                        idx,
                        setdiff(names(pending), "tried_candidate_id"),
                        drop = FALSE
                    ]
                    item <- private$launch_concurrent_task(
                        task,
                        tried_candidate_id = tried,
                        overwrite = overwrite,
                        resume = resume,
                        active_hosts = private$running_host_counts(running)
                    )
                    if (identical(item$status, "running")) {
                        pending <- pending[-idx, , drop = FALSE]
                        blocked <- blocked[-idx]
                        add_running(item)
                    } else if (identical(item$status, "deferred")) {
                        blocked[idx] <- TRUE
                    } else {
                        pending <- pending[-idx, , drop = FALSE]
                        blocked <- blocked[-idx]
                        tick()
                    }
                }

                if (!length(running)) {
                    next
                }

                done <- names(running)[
                    !vapply(
                        running,
                        function(item) {
                            mirai::unresolved(item$mirai)
                        },
                        logical(1L)
                    )
                ]
                if (!length(done)) {
                    private$ensure_mirai_daemons()
                    mirai::race_mirai(
                        lapply(running, function(item) {
                            item$mirai
                        }),
                        .compute = private$compute_profile
                    )
                    done <- names(running)[
                        !vapply(
                            running,
                            function(item) {
                                mirai::unresolved(item$mirai)
                            },
                            logical(1L)
                        )
                    ]
                    if (!length(done)) {
                        next
                    }
                }

                for (id in done) {
                    item <- running[[id]]
                    running[[id]] <- NULL
                    private$persistent_tasks[[id]] <- NULL
                    active_targets <- setdiff(active_targets, item$target_path)

                    result <- tryCatch(item$mirai[], error = function(e) e)
                    error <- downloader__worker_error(result)
                    if (nzchar(error)) {
                        result <- list(ok = FALSE, error = error)
                    }

                    task <- private$select_tasks(task_id = item$task_id)
                    if (isTRUE(item$segmented) && !is.null(result$pieces)) {
                        private$update_piece_results(result$pieces)
                    }
                    if (
                        isTRUE(item$segmented) && isTRUE(result$integrity_error)
                    ) {
                        private$cleanup_task_pieces(item$task_id)
                        private$log_event(
                            task$session_id[[1L]],
                            task$task_id[[1L]],
                            "segment_integrity_error",
                            result$error
                        )
                    }
                    if (
                        isTRUE(result$ok) &&
                            !is.null(result$path) &&
                            file.exists(result$path)
                    ) {
                        task$status <- "done"
                        task$target_path <- normalizePath(
                            result$path,
                            mustWork = TRUE,
                            winslash = "/"
                        )
                        task$bytes_done <- file.info(
                            result$path,
                            extra_cols = FALSE
                        )$size
                        if (isTRUE(item$segmented)) {
                            task$selected_url <- downloader__one_chr(
                                result$selected_url
                            )
                            if (is.na(task$selected_url)) {
                                task$selected_url <- item$candidate$url[[1L]]
                            }
                            private$cleanup_task_pieces(item$task_id)
                        }
                        task$last_error <- NA_character_
                        task$completed_at <- downloader__now()
                        if (
                            isTRUE(item$segmented) &&
                                !is.null(result$used_candidate_id) &&
                                length(result$used_candidate_id)
                        ) {
                            used <- item$segmented_candidates[
                                item$segmented_candidates$candidate_id %in%
                                    result$used_candidate_id,
                                ,
                                drop = FALSE
                            ]
                            if (!nrow(used)) {
                                used <- item$candidate
                            }
                            bytes_each <- task$bytes_done[[1L]] / nrow(used)
                            for (j in seq_len(nrow(used))) {
                                private$update_node_stats(
                                    used[j, , drop = FALSE],
                                    ok = TRUE,
                                    bytes_done = bytes_each
                                )
                            }
                        } else {
                            private$update_node_stats(
                                item$candidate,
                                ok = TRUE,
                                bytes_done = task$bytes_done[[1L]]
                            )
                        }
                        private$update_task(task)
                        private$log_event(
                            task$session_id[[1L]],
                            task$task_id[[1L]],
                            "done",
                            result$path
                        )
                        tick()
                        next
                    }

                    last_error <- downloader__one_chr(result$error)
                    if (is.na(last_error)) {
                        last_error <- "Download failed."
                    }
                    candidate <- item$candidate
                    failed <- suppressWarnings(as.integer(candidate$failed_count[[
                        1L
                    ]]))
                    if (is.na(failed)) {
                        failed <- 0L
                    }
                    candidate$failed_count <- failed + 1L
                    candidate$last_error <- last_error
                    private$update_candidate(candidate)
                    private$update_node_stats(candidate, ok = FALSE)
                    private$log_event(
                        task$session_id[[1L]],
                        task$task_id[[1L]],
                        "candidate_error",
                        last_error
                    )

                    tried <- unique(c(
                        item$tried_candidate_id,
                        candidate$candidate_id[[1L]]
                    ))
                    next_item <- private$launch_concurrent_task(
                        task,
                        tried_candidate_id = tried,
                        overwrite = overwrite,
                        resume = resume,
                        active_hosts = private$running_host_counts(running)
                    )
                    if (identical(next_item$status, "running")) {
                        add_running(next_item)
                    } else if (identical(next_item$status, "deferred")) {
                        task$tried_candidate_id <- I(list(tried))
                        pending <- downloader__rbind_fill(list(pending, task))
                    } else {
                        tick()
                    }
                }
            }

            invisible(tasks)
        },
        # }}}

        # Verify an already-present target against the saved byte checkpoint.
        # Missing legacy evidence stays missing for the Store's provenance gate.
        # task_target_error {{{
        task_target_error = function(task) {
            checksum <- downloader__one_chr(task$checksum[[1L]])
            tryCatch(
                {
                    downloader__worker_dependencies()$transfer_seal(
                        file.path(
                            private$temp,
                            paste0(task$task_id[[1L]], ".part")
                        ),
                        task$target_path[[1L]],
                        if (is.na(checksum)) NULL else checksum,
                        downloader__checksum_type(task$checksum_type[[1L]])
                    )
                    NA_character_
                },
                error = function(e) conditionMessage(e)
            )
        },
        # }}}

        # launch_concurrent_task {{{
        launch_concurrent_task = function(
            task,
            tried_candidate_id = character(),
            overwrite = FALSE,
            resume = TRUE,
            active_hosts = integer()
        ) {
            task <- downloader__as_df(task)
            task_id <- task$task_id[[1L]]
            session_id <- task$session_id[[1L]]
            checksum <- downloader__one_chr(task$checksum[[1L]])
            checksum_type <- downloader__checksum_type(task$checksum_type[[1L]])
            target_path <- task$target_path[[1L]]

            if (file.exists(target_path) && !isTRUE(overwrite)) {
                ok <- if (is.na(checksum)) {
                    TRUE
                } else {
                    private$verify_checksum_internal(
                        target_path,
                        checksum,
                        checksum_type
                    )
                }
                if (ok) {
                    integrity_error <- private$task_target_error(task)
                    if (!is.na(integrity_error)) {
                        task$status <- "error"
                        task$last_error <- integrity_error
                        task$completed_at <- downloader__now()
                        private$update_task(task)
                        private$log_event(
                            session_id,
                            task_id,
                            "error",
                            integrity_error
                        )
                        return(list(status = "terminal", task_id = task_id))
                    }
                    task$status <- "skipped"
                    task$bytes_done <- file.info(
                        target_path,
                        extra_cols = FALSE
                    )$size
                    task$last_error <- NA_character_
                    task$completed_at <- downloader__now()
                    private$update_task(task)
                    private$log_event(
                        session_id,
                        task_id,
                        "skipped",
                        "Final file already exists and passed validation."
                    )
                    return(list(status = "terminal", task_id = task_id))
                }
            }

            candidates <- private$get_candidates(task_id)
            if (length(tried_candidate_id)) {
                candidates <- candidates[
                    !candidates[["candidate_id"]] %in% tried_candidate_id,
                    ,
                    drop = FALSE
                ]
            }
            if (!nrow(candidates)) {
                task$status <- "error"
                task$last_error <- "All candidate URLs failed."
                task$completed_at <- downloader__now()
                private$update_task(task)
                private$log_event(session_id, task_id, "error", task$last_error)
                return(list(status = "terminal", task_id = task_id))
            }

            candidate_hosts <- vapply(
                seq_len(nrow(candidates)),
                function(i) {
                    private$candidate_host(candidates[i, , drop = FALSE])
                },
                character(1L)
            )
            has_capacity <- vapply(
                candidate_hosts,
                private$host_has_capacity,
                logical(1L),
                active_hosts = active_hosts
            )
            if (!any(has_capacity)) {
                return(list(status = "deferred", task_id = task_id))
            }
            candidates <- candidates[has_capacity, , drop = FALSE]
            candidate_hosts <- candidate_hosts[has_capacity]
            candidate <- candidates[1L, , drop = FALSE]
            candidate_host <- candidate_hosts[[1L]]
            attempts <- suppressWarnings(as.integer(task$attempts[[1L]]))
            if (is.na(attempts)) {
                attempts <- 0L
            }
            task$status <- "downloading"
            task$attempts <- attempts + 1L
            task$selected_url <- candidate$url[[1L]]
            task$data_node <- candidate$data_node[[1L]]
            task$last_error <- NA_character_
            private$update_task(task)
            private$log_event(session_id, task_id, "start", candidate$url[[1L]])

            task_subdir <- downloader__one_chr(task$subdir[[1L]])
            if (is.na(task_subdir)) {
                task_subdir <- NULL
            }
            segmented_attempt <- NULL
            if (!identical(private$transfer_policy_config$range_mode, "off")) {
                segmented_candidates <- if (
                    private$transfer_policy_config$range_mode %in%
                        c("multi", "auto")
                ) {
                    candidates
                } else {
                    candidate
                }
                segmented_attempt <- private$prepare_segmented_attempt(
                    task,
                    candidates = segmented_candidates,
                    preferred_candidate = candidate,
                    resume = resume
                )
            }
            segmented <- !is.null(segmented_attempt)
            private$ensure_mirai_daemons()
            mirai_obj <- mirai::mirai(
                {
                    tryCatch(
                        if (isTRUE(segmented)) {
                            segmented_worker_fun(
                                candidates = segmented_candidates,
                                pieces = segmented_pieces,
                                filename = filename,
                                subdir = subdir,
                                dest = dest,
                                temp = temp,
                                retries = retries,
                                timeout = timeout,
                                overwrite = overwrite,
                                checksum = checksum,
                                checksum_type = checksum_type,
                                tmp_id = tmp_id,
                                ssl_verifypeer = ssl_verifypeer,
                                proxy = proxy,
                                connect_timeout = connect_timeout,
                                useragent = useragent,
                                transfer_policy = transfer_policy,
                                mode_used = segmented_mode,
                                worker_dependencies = worker_dependencies
                            )
                        } else {
                            worker_fun(
                                url = url,
                                filename = filename,
                                subdir = subdir,
                                dest = dest,
                                temp = temp,
                                retries = retries,
                                timeout = timeout,
                                overwrite = overwrite,
                                checksum = checksum,
                                checksum_type = checksum_type,
                                resume = resume,
                                tmp_id = tmp_id,
                                ssl_verifypeer = ssl_verifypeer,
                                proxy = proxy,
                                connect_timeout = connect_timeout,
                                useragent = useragent,
                                transfer_policy = transfer_policy,
                                worker_dependencies = worker_dependencies
                            )
                        },
                        error = function(e) {
                            list(ok = FALSE, error = conditionMessage(e))
                        }
                    )
                },
                .compute = private$compute_profile,
                worker_fun = downloader__worker_download,
                segmented_worker_fun = downloader__worker_segmented_download,
                worker_dependencies = downloader__worker_dependencies(),
                url = candidate$url[[1L]],
                filename = task$filename[[1L]],
                subdir = task_subdir,
                dest = private$dest,
                temp = private$temp,
                retries = private$retries,
                timeout = private$dl_timeout,
                overwrite = overwrite,
                checksum = if (is.na(checksum)) NULL else checksum,
                checksum_type = checksum_type,
                resume = resume,
                tmp_id = task_id,
                ssl_verifypeer = private$ssl_verifypeer,
                proxy = private$proxy,
                connect_timeout = private$connect_timeout,
                useragent = private$useragent,
                transfer_policy = private$transfer_policy_config,
                segmented = segmented,
                segmented_candidates = if (isTRUE(segmented)) {
                    segmented_attempt$candidates
                } else {
                    data.frame()
                },
                segmented_pieces = if (isTRUE(segmented)) {
                    segmented_attempt$pieces
                } else {
                    data.frame()
                },
                segmented_mode = if (isTRUE(segmented)) {
                    segmented_attempt$mode_used
                } else {
                    NA_character_
                }
            )

            list(
                status = "running",
                task_id = task_id,
                target_path = target_path,
                host = candidate_host,
                candidate = candidate,
                segmented = segmented,
                segmented_candidates = if (isTRUE(segmented)) {
                    segmented_attempt$candidates
                } else {
                    data.frame()
                },
                tried_candidate_id = tried_candidate_id,
                mirai = mirai_obj
            )
        },
        # }}}

        # run_task {{{
        run_task = function(
            task,
            progress = TRUE,
            overwrite = FALSE,
            resume = TRUE
        ) {
            task <- downloader__as_df(task)
            task_id <- task$task_id[[1L]]
            session_id <- task$session_id[[1L]]
            checksum <- downloader__one_chr(task$checksum[[1L]])
            checksum_type <- downloader__checksum_type(task$checksum_type[[1L]])
            target_path <- task$target_path[[1L]]

            if (file.exists(target_path) && !isTRUE(overwrite)) {
                ok <- if (is.na(checksum)) {
                    TRUE
                } else {
                    private$verify_checksum_internal(
                        target_path,
                        checksum,
                        checksum_type
                    )
                }
                if (ok) {
                    integrity_error <- private$task_target_error(task)
                    if (!is.na(integrity_error)) {
                        task$status <- "error"
                        task$last_error <- integrity_error
                        task$completed_at <- downloader__now()
                        private$update_task(task)
                        private$log_event(
                            session_id,
                            task_id,
                            "error",
                            integrity_error
                        )
                        return(NA_character_)
                    }
                    task$status <- "skipped"
                    task$bytes_done <- file.info(target_path)$size
                    task$last_error <- NA_character_
                    task$completed_at <- downloader__now()
                    private$update_task(task)
                    private$log_event(
                        session_id,
                        task_id,
                        "skipped",
                        "Final file already exists and passed validation."
                    )
                    return(target_path)
                }
            }

            candidates <- private$get_candidates(task_id)
            if (!nrow(candidates)) {
                task$status <- "error"
                task$last_error <- "No candidate URLs are available."
                task$completed_at <- downloader__now()
                private$update_task(task)
                private$log_event(session_id, task_id, "error", task$last_error)
                return(NA_character_)
            }

            last_error <- NULL
            if (
                private$transfer_policy_config$range_mode %in%
                    c("multi", "auto") &&
                    nrow(candidates) > 1L
            ) {
                segmented <- private$run_segmented_attempt(
                    task,
                    candidates = candidates,
                    overwrite = overwrite,
                    resume = resume
                )
                if (
                    isTRUE(segmented$ok) &&
                        !is.null(segmented$path) &&
                        file.exists(segmented$path)
                ) {
                    task$status <- "done"
                    task$target_path <- normalizePath(
                        segmented$path,
                        mustWork = TRUE,
                        winslash = "/"
                    )
                    task$bytes_done <- file.info(segmented$path)$size
                    task$selected_url <- downloader__one_chr(
                        segmented$selected_url
                    )
                    if (is.na(task$selected_url)) {
                        task$selected_url <- segmented$attempt$candidates$url[[
                            1L
                        ]]
                    }
                    task$data_node <- downloader__one_chr(segmented$attempt$candidates$data_node[[
                        1L
                    ]])
                    task$last_error <- NA_character_
                    task$completed_at <- downloader__now()
                    used <- segmented$attempt$candidates
                    if (
                        !is.null(segmented$used_candidate_id) &&
                            length(segmented$used_candidate_id)
                    ) {
                        used <- used[
                            used$candidate_id %in% segmented$used_candidate_id,
                            ,
                            drop = FALSE
                        ]
                    }
                    if (!nrow(used)) {
                        used <- segmented$attempt$candidates[1L, , drop = FALSE]
                    }
                    bytes_each <- task$bytes_done[[1L]] / nrow(used)
                    for (j in seq_len(nrow(used))) {
                        private$update_node_stats(
                            used[j, , drop = FALSE],
                            ok = TRUE,
                            bytes_done = bytes_each
                        )
                    }
                    private$update_task(task)
                    private$log_event(
                        session_id,
                        task_id,
                        "done",
                        segmented$path
                    )
                    return(segmented$path)
                }
                if (isTRUE(segmented$attempted)) {
                    last_error <- downloader__one_chr(segmented$error)
                }
            }

            for (i in seq_len(nrow(candidates))) {
                candidate <- candidates[i, , drop = FALSE]
                attempts <- suppressWarnings(as.integer(task$attempts[[1L]]))
                if (is.na(attempts)) {
                    attempts <- 0L
                }
                task$status <- "downloading"
                task$attempts <- attempts + 1L
                task$selected_url <- candidate$url[[1L]]
                task$data_node <- candidate$data_node[[1L]]
                task$last_error <- NA_character_
                private$update_task(task)
                private$log_event(
                    session_id,
                    task_id,
                    "start",
                    candidate$url[[1L]]
                )

                task_subdir <- downloader__one_chr(task$subdir[[1L]])
                if (is.na(task_subdir)) {
                    task_subdir <- NULL
                }
                segmented <- private$run_segmented_attempt(
                    task,
                    candidates = candidate,
                    preferred_candidate = candidate,
                    overwrite = overwrite,
                    resume = resume
                )
                if (isTRUE(segmented$attempted)) {
                    path <- if (isTRUE(segmented$ok)) segmented$path else NULL
                    if (!isTRUE(segmented$ok)) {
                        last_error <- downloader__one_chr(segmented$error)
                    }
                } else {
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
                }

                if (!is.null(path) && file.exists(path)) {
                    task$status <- "done"
                    task$target_path <- normalizePath(
                        path,
                        mustWork = TRUE,
                        winslash = "/"
                    )
                    task$bytes_done <- file.info(path)$size
                    if (isTRUE(segmented$attempted)) {
                        task$selected_url <- downloader__one_chr(
                            segmented$selected_url
                        )
                        if (is.na(task$selected_url)) {
                            task$selected_url <- candidate$url[[1L]]
                        }
                    }
                    task$last_error <- NA_character_
                    task$completed_at <- downloader__now()
                    private$update_node_stats(
                        candidate,
                        ok = TRUE,
                        bytes_done = task$bytes_done[[1L]]
                    )
                    private$update_task(task)
                    private$log_event(session_id, task_id, "done", path)
                    return(path)
                }

                candidate$failed_count <- as.integer(candidate$failed_count) +
                    1L
                candidate$last_error <- last_error
                private$update_candidate(candidate)
                private$update_node_stats(candidate, ok = FALSE)
                private$log_event(
                    session_id,
                    task_id,
                    "candidate_error",
                    last_error
                )
            }

            task$status <- "error"
            task$last_error <- if (is.null(last_error) || is.na(last_error)) {
                "All candidate URLs failed."
            } else {
                last_error
            }
            task$completed_at <- downloader__now()
            private$update_task(task)
            private$log_event(session_id, task_id, "error", task$last_error)
            NA_character_
        },
        # }}}
        # check_file_status
        # check_file_status {{{
        check_file_status = function(
            dest,
            tmp_part,
            tmp_done,
            checksum,
            checksum_type
        ) {
            # Check final destination
            if (file.exists(dest)) {
                # Verify checksum if provided
                if (!is.null(checksum)) {
                    if (
                        private$verify_checksum_internal(
                            dest,
                            checksum,
                            checksum_type
                        )
                    ) {
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
        # finalize_download
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
                warning(
                    "Cannot rename across filesystems. Using copy instead (slower)."
                )
                file.copy(tmp_done, dest, overwrite = TRUE)
                file.remove(tmp_done)
            }

            invisible(dest)
        },
        # }}}
        # download_async
        # download_async {{{
        download_async = function(
            url,
            filename,
            subdir,
            progress,
            overwrite,
            checksum,
            checksum_type,
            resume
        ) {
            # Check offline mode before launching async download
            if (downloader__offline()) {
                stop(
                    "Cannot download file in offline mode: ",
                    url,
                    call. = FALSE
                )
            }

            # Generate task ID
            task_id <- downloader__checksum_bytes(
                charToRaw(paste(url, Sys.time())),
                "md5"
            )

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
            private$ensure_mirai_daemons()

            # Capture self parameters for mirai
            downloader_params <- list(
                dest = private$dest,
                temp = private$temp,
                retries = private$retries,
                timeout = private$dl_timeout,
                ssl_verifypeer = private$ssl_verifypeer,
                proxy = private$proxy,
                connect_timeout = private$connect_timeout,
                useragent = private$useragent,
                cleanup = private$cleanup,
                transfer_policy = private$transfer_policy_config,
                resource_policy = private$resource_policy_config
            )

            private$ensure_mirai_daemons()

            task$mirai_obj <- mirai::mirai(
                {
                    .libPaths(runtime$libraries)
                    module <- if (!is.null(runtime$package)) {
                        asNamespace(runtime$package)
                    } else {
                        env <- new.env(parent = baseenv())
                        sys.source(runtime$source, envir = env)
                        env
                    }
                    ctor <- get("Downloader", envir = module, inherits = FALSE)

                    dl <- ctor$new(
                        dest = downloader_params$dest,
                        temp = downloader_params$temp,
                        retries = downloader_params$retries,
                        timeout = downloader_params$timeout,
                        ssl_verifypeer = downloader_params$ssl_verifypeer,
                        proxy = downloader_params$proxy,
                        connect_timeout = downloader_params$connect_timeout,
                        useragent = downloader_params$useragent,
                        cleanup = downloader_params$cleanup,
                        transfer_policy = downloader_params$transfer_policy,
                        resource_policy = downloader_params$resource_policy,
                        n_workers = 0L # Async tasks don't need their own workers
                    )

                    dl$download(
                        url = url,
                        filename = filename,
                        subdir = subdir,
                        progress = FALSE, # No progress in async mode
                        overwrite = overwrite,
                        checksum = checksum,
                        checksum_type = checksum_type,
                        resume = resume,
                        block = TRUE # Must be blocking in mirai
                    )
                },
                url = url,
                filename = filename,
                subdir = subdir,
                overwrite = overwrite,
                checksum = checksum,
                checksum_type = checksum_type,
                resume = resume,
                downloader_params = downloader_params,
                runtime = downloader__runtime(),
                .compute = private$compute_profile
            )

            task$status <- "downloading"

            downloader__verbose(cli::cli_alert_success(
                "Started async download: {url}"
            ))
            downloader__verbose(cli::cli_alert_info("Task ID: {task_id}"))

            invisible(task_id)
        },
        # }}}
        # progress_total_bytes {{{
        progress_total_bytes = function(task_id) {
            if (is.null(task_id) || is.null(private$manifest_path)) {
                return(NA_real_)
            }
            task <- tryCatch(
                private$select_tasks(task_id = task_id),
                error = function(e) data.frame()
            )
            if (!nrow(task) || !"size" %in% names(task)) {
                return(NA_real_)
            }
            suppressWarnings(as.numeric(task$size[[1L]]))
        },
        # }}}

        # download_with_streaming {{{
        download_with_streaming = function(
            url,
            tmp_part,
            tmp_done,
            progress,
            start_byte,
            task_id = NULL,
            progress_interval = DOWNLOADER_PROGRESS_INTERVAL,
            checksum = NULL,
            resume = TRUE
        ) {
            # Track download progress
            bytes_downloaded <- start_byte
            start_time <- Sys.time()
            last_update <- start_time
            last_manifest_update <- start_time
            total_bytes <- private$progress_total_bytes(task_id)
            manifest_progress <- !is.null(task_id) &&
                !is.null(private$manifest_path)

            write_progress <- function(force = FALSE) {
                if (!isTRUE(manifest_progress)) {
                    return(invisible(NULL))
                }
                current_time <- Sys.time()
                if (
                    !isTRUE(force) &&
                        as.numeric(difftime(
                            current_time,
                            last_manifest_update,
                            units = "secs"
                        )) <
                            progress_interval
                ) {
                    return(invisible(NULL))
                }
                elapsed <- as.numeric(difftime(
                    current_time,
                    start_time,
                    units = "secs"
                ))
                speed <- if (elapsed > 0) {
                    (bytes_downloaded - start_byte) / elapsed
                } else {
                    NA_real_
                }
                eta <- NA_real_
                if (
                    !is.na(total_bytes) &&
                        total_bytes > bytes_downloaded &&
                        !is.na(speed) &&
                        speed > 0
                ) {
                    eta <- (total_bytes - bytes_downloaded) / speed
                }
                private$update_task_progress(
                    task_id = task_id,
                    bytes_done = bytes_downloaded,
                    speed_bps = speed,
                    eta_seconds = eta,
                    current_url = url
                )
                last_manifest_update <<- current_time
                invisible(NULL)
            }

            progress_id <- NULL
            if (progress) {
                progress_id <- cli::cli_progress_bar(
                    "Downloading",
                    total = NA,
                    .auto_close = FALSE
                )
            }

            # Download with streaming
            downloader__worker_dependencies()$transfer_stream(
                url = url,
                tmp_part = tmp_part,
                tmp_done = tmp_done,
                timeout = private$dl_timeout,
                start_byte = start_byte,
                connect_timeout = private$connect_timeout,
                ssl_verifypeer = private$ssl_verifypeer,
                proxy = private$proxy,
                useragent = private$useragent,
                transfer_policy = private$transfer_policy_config,
                checksum = checksum,
                resume = resume,
                on_progress = function(bytes) {
                    bytes_downloaded <<- bytes

                    # Update progress (limit update frequency)
                    current_time <- Sys.time()
                    elapsed <- as.numeric(difftime(
                        current_time,
                        start_time,
                        units = "secs"
                    ))
                    speed <- if (elapsed > 0) {
                        (bytes_downloaded - start_byte) / elapsed
                    } else {
                        0
                    }
                    if (progress && !is.null(progress_id)) {
                        if (
                            difftime(
                                current_time,
                                last_update,
                                units = "secs"
                            ) >
                                0.1
                        ) {
                            cli::cli_progress_update(
                                id = progress_id,
                                status = sprintf(
                                    "%s (%s/s)",
                                    downloader__format_bytes(bytes_downloaded),
                                    downloader__format_bytes(speed)
                                )
                            )
                            last_update <<- current_time
                        }
                    }
                    write_progress()

                    TRUE # Continue download
                }
            )

            if (progress && !is.null(progress_id)) {
                elapsed <- as.numeric(difftime(
                    Sys.time(),
                    start_time,
                    units = "secs"
                ))
                speed <- if (elapsed > 0) {
                    (bytes_downloaded - start_byte) / elapsed
                } else {
                    0
                }
                cli::cli_progress_done(
                    id = progress_id,
                    result = sprintf(
                        "Downloaded %s in %.1fs (avg %s/s)",
                        downloader__format_bytes(bytes_downloaded),
                        elapsed,
                        downloader__format_bytes(speed)
                    )
                )
            }
            write_progress(force = TRUE)

            invisible(tmp_done)
        },
        # }}}
        # verify_checksum_internal
        # verify_checksum_internal {{{
        verify_checksum_internal = function(file, expected, type) {
            downloader__verify_checksum(file, expected, type)
        },
        # }}}
        # get_file_checksum
        # get_file_checksum {{{
        get_file_checksum = function(file, type) {
            checkmate::assert_file_exists(file)
            checkmate::assert_choice(type, c("md5", "sha256"))

            tolower(downloader__checksum_file(file, type))
        }
        # }}}
    )
)
# }}}
# Start a persisted download job or daemon from its installed package or source file.
# downloader__launch_process {{{
downloader__launch_process <- function(kind, id, manifest, log_path) {
    fun <- if (identical(kind, "daemon")) {
        "downloader__daemon_main"
    } else {
        "downloader__job_main"
    }
    arg <- if (identical(kind, "daemon")) "daemon_id" else "job_id"
    runtime <- downloader__runtime()
    args <- stats::setNames(list(manifest, id), c("manifest", arg))
    arguments <- paste(
        paste0(
            names(args),
            " = ",
            vapply(args, downloader__string_literal, character(1L))
        ),
        collapse = ","
    )
    libraries <- paste(
        vapply(runtime$libraries, downloader__string_literal, character(1L)),
        collapse = ","
    )
    load <- if (!is.null(runtime$package)) {
        sprintf(
            "module <- asNamespace(%s)",
            downloader__string_literal(runtime$package)
        )
    } else {
        sprintf(
            "module <- new.env(parent = baseenv()); sys.source(%s, envir = module)",
            downloader__string_literal(runtime$source)
        )
    }
    expr <- sprintf(
        ".libPaths(c(%s)); %s; get(%s, module, inherits = FALSE)(%s)",
        libraries,
        load,
        downloader__string_literal(fun),
        arguments
    )
    status <- system2(
        downloader__rscript(),
        c("--vanilla", "-e", shQuote(expr)),
        stdout = log_path,
        stderr = log_path,
        wait = FALSE
    )
    if (!identical(as.integer(status), 0L)) {
        cli::cli_abort("Could not launch the downloader process.")
    }
    invisible(status)
}
# }}}

# vim: fdm=marker :
