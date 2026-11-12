#' Download planned ERA5 raw assets with durable submission records
#'
#' Execute an independent [shift_plan_reanalysis()] plan serially, retaining a
#' submission ledger and raw assets. This transport stage does not validate
#' weather; use [shift_read_reanalysis()] after acquiring all required files.
#'
#' @param plan An unchanged [shift_plan_reanalysis()] result.
#' @param dir Directory dedicated to this plan and provider origin.
#' @param limits Named list: `max_submissions`, `max_file_bytes` and
#'   `max_total_bytes` default to `Inf`; `wait_timeout` defaults to 3600 seconds,
#'   `download_timeout` to 600 seconds, and `total_timeout` to `Inf`.
#' @param resume Reuse the ledger and verified completed assets. `FALSE` refuses
#'   an existing ledger rather than deleting it or submitting duplicate jobs.
#' @param poll_interval Positive seconds between status requests; default 1.
#' @return A list with transport `status`, named completed `files`, request
#'   receipts, this invocation's `execution` measurements, `cumulative` counters,
#'   `accounting_uncertain` flag, and `receipt` path.
#'   `downloaded` means HTTP assets are complete, not validated weather.
#' @details
#' Credentials use the existing CDS environment/configuration settings. Tokens
#'   and signed asset URLs are never saved. A job ID is durably recorded before
#'   polling. An uncertain submission is `ambiguous` and never automatically
#'   posted again. A directory lock prevents concurrent writers; an orphaned
#'   lock requires verifying its recorded owner has stopped before removal.
#'
#' Submission counts and total byte budgets accumulate across invocations in
#' this directory, including failed attempts and JSON response
#'   bodies. Response-body bytes delivered to the stream reader and bytes written
#'   to assets are counted separately. The first block crossing a limit aborts
#'   the request without being written. Bytes already buffered by the network
#'   cannot be recalled: these are fail-stop thresholds, not an exact wire-byte
#'   ceiling. HTTP/TLS headers and uploads are outside response-body accounting.
#'   A killed process may leave only a lower bound for its last in-flight attempt;
#'   resuming records that uncertainty and stops rather than declaring zero lost
#'   traffic. Resume inherits earlier limits; raising them requires explicit
#'   `limits` values. Timeouts apply per invocation or per individual wait/download.
#'   The file-byte threshold applies to each individual asset response. A fixed
#'   1 MiB response threshold also bounds each JSON control message. Redirects
#'   are rejected; tokens are sent only to the configured API base path.
#'
#' Partial files are never returned as complete. Downloads restart from the
#'   original job's refreshed result URL; they do not resume a byte range.
#'   Asset responses must have HTTP status 200. Completed files are returned
#'   only after size and SHA-256 verification during the current invocation.
#' @export
# shift_download_reanalysis {{{
shift_download_reanalysis <- function(
    plan,
    dir,
    limits = list(),
    resume = TRUE,
    poll_interval = 1
) {
    checkmate::assert_flag(resume)
    checkmate::assert_string(dir, min.chars = 1L)
    checkmate::assert_number(
        poll_interval,
        lower = .Machine$double.eps,
        finite = TRUE
    )
    if (!is.list(plan) || !identical(plan$schema_version, 1L)) {
        cli::cli_abort("Supply an unchanged reanalysis plan.")
    }
    canonical <- shift_plan_reanalysis(
        shift_era5(
            plan$source$years,
            variables = plan$variables$variable_id,
            product = plan$source$product,
            frequency = plan$source$frequency,
            access = plan$source$access
        ),
        lapply(seq_len(nrow(plan$sites)), function(i) {
            shift_site(
                plan$sites$site_id[[i]],
                lat = plan$sites$latitude[[i]],
                lon = plan$sites$longitude[[i]]
            )
        }),
        dates = plan$source$dates
    )
    if (!reanalysis_plan__identical(plan, canonical)) {
        cli::cli_abort("Supply an unchanged reanalysis plan.")
    }
    settings <- reanalysis_transfer__limits(limits)
    config <- cds__config(require_key = FALSE)
    # Store only a plain origin/base path; URL credentials, query strings and
    # fragments are never valid persistent provider identities.
    if (
        !grepl("^https?://[^/@?#]+(/[^?#]*)?$", config$url) ||
            (nzchar(config$key) && grepl(config$key, config$url, fixed = TRUE))
    ) {
        cli::cli_abort(
            "Provider URL must not contain credentials, queries or fragments."
        )
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir <- normalizePath(dir, mustWork = TRUE)
    lock <- file.path(dir, ".transfer-lock")
    if (!dir.create(lock, showWarnings = FALSE)) {
        cli::cli_abort(
            "Transfer directory is locked; verify the recorded owner before recovering it."
        )
    }
    owner <- list(
        pid = Sys.getpid(),
        host = Sys.info()[["nodename"]],
        nonce = basename(tempfile()),
        started_at = reanalysis_transfer__time()
    )
    reanalysis_transfer__write(owner, file.path(lock, "owner.json"))
    on.exit(
        {
            current <- tryCatch(
                jsonlite::read_json(file.path(lock, "owner.json")),
                error = function(e) NULL
            )
            if (identical(current$nonce, owner$nonce)) {
                unlink(lock, recursive = TRUE)
            }
        },
        add = TRUE
    )
    ledger_path <- file.path(dir, "transfer.json")
    if (file.exists(ledger_path) && !resume) {
        cli::cli_abort("A transfer ledger already exists.")
    }
    if (file.exists(ledger_path)) {
        ledger <- jsonlite::read_json(ledger_path, simplifyVector = FALSE)
        if (
            !identical(ledger$schema_version, 1L) ||
                !identical(ledger$provider_origin, config$url) ||
                !identical(
                    unlist(ledger$request_ids, use.names = FALSE),
                    plan$requests$request_id
                ) ||
                length(ledger$requests) != nrow(plan$requests)
        ) {
            cli::cli_abort(
                "Existing ledger has a different plan, provider or schema."
            )
        }
        # A matching list of IDs is insufficient: each saved request body and
        # locator must still describe the public plan that the caller supplied.
        for (i in seq_along(ledger$requests)) {
            row <- ledger$requests[[i]]
            identity <- jsonlite::toJSON(
                list(dataset = row$dataset, request = row$request),
                auto_unbox = TRUE,
                digits = NA
            )
            if (
                !identical(row$request_id, plan$requests$request_id[[i]]) ||
                    !identical(
                        checksum_bytes(charToRaw(identity)),
                        row$request_id
                    ) ||
                    (!is.null(row$locator) &&
                        (length(row$locator) != 1L ||
                            !is.character(row$locator) ||
                            !grepl("^[A-Za-z0-9_-]+$", row$locator)))
            ) {
                cli::cli_abort("Invalid persisted request receipt.")
            }
        }
        for (execution in ledger$executions) {
            for (name in c(
                "submissions",
                "bytes_received",
                "asset_bytes_written"
            )) {
                if (
                    !checkmate::test_number(
                        execution[[name]],
                        lower = 0,
                        finite = TRUE
                    )
                ) {
                    cli::cli_abort("Invalid transfer ledger counters.")
                }
            }
        }
        inherited <- lapply(ledger$limits, function(x) {
            if (identical(x, "unlimited")) Inf else x
        })
        if (!setequal(names(inherited), names(settings))) {
            cli::cli_abort("Invalid persisted limits.")
        }
        inherited[names(limits)] <- limits
        settings <- reanalysis_transfer__limits(inherited)
    } else {
        ledger <- list(
            schema_version = 1L,
            provider_origin = config$url,
            request_ids = plan$requests$request_id,
            requests = lapply(seq_len(nrow(plan$requests)), function(i) {
                list(
                    schema_version = 1L,
                    request_id = plan$requests$request_id[[i]],
                    provider_origin = config$url,
                    updated_at = reanalysis_transfer__time(),
                    dataset = plan$requests$dataset[[i]],
                    request = plan$requests$request[[i]],
                    state = "not_submitted",
                    submission_attempted = FALSE,
                    locator = NULL,
                    raw_asset = NULL,
                    validation = NULL,
                    attempts = list(),
                    error = NULL
                )
            }),
            executions = list()
        )
    }
    dir.create(file.path(dir, "raw"), showWarnings = FALSE)
    # The environment owns one invocation's counters; every state/byte update is
    # published as one atomic ledger replacement before further network work.
    e <- new.env(parent = emptyenv())
    e$ledger <- ledger
    e$path <- ledger_path
    e$config <- config
    e$limits <- settings
    e$ledger$limits <- lapply(settings, function(x) {
        if (is.infinite(x)) "unlimited" else x
    })
    e$prior <- list(
        submissions = sum(vapply(
            ledger$executions,
            function(x) x$submissions,
            numeric(1)
        )),
        bytes_received = sum(vapply(
            ledger$executions,
            function(x) x$bytes_received,
            numeric(1)
        )),
        asset_bytes_written = sum(vapply(
            ledger$executions,
            function(x) x$asset_bytes_written,
            numeric(1)
        ))
    )
    e$start <- proc.time()[["elapsed"]]
    e$run <- length(ledger$executions) + 1L
    e$execution <- list(
        started_at = reanalysis_transfer__time(),
        status = "running",
        submissions = 0,
        bytes_received = 0,
        asset_bytes_written = 0,
        limits = lapply(settings, function(x) {
            if (is.infinite(x)) "unlimited" else x
        })
    )
    # No locator can be inferred after a killed POST. Preserve the uncertainty
    # even if a server might have accepted and completed the request.
    for (i in seq_along(e$ledger$requests)) {
        row <- e$ledger$requests[[i]]
        if (row$state == "submitting") {
            row$state <- "ambiguous"
        }
        if (
            is.null(row$locator) && identical(row$submission_attempted, FALSE)
        ) {
            row$state <- "not_submitted"
        }
        for (j in seq_along(row$attempts)) {
            if (identical(row$attempts[[j]]$outcome, "in_flight")) {
                row$attempts[[
                    j
                ]]$outcome <- "interrupted_accounting_lower_bound"
                e$ledger$accounting_uncertain <- TRUE
            }
        }
        e$ledger$requests[[i]] <- row
    }
    reanalysis_transfer__save(e)
    # A historical downloaded state is not evidence that the file is unchanged
    # now. An early failure must not return later, unvisited cache entries.
    verified <- rep(FALSE, length(e$ledger$requests))
    eligible <- if (isTRUE(e$ledger$accounting_uncertain)) {
        integer()
    } else {
        seq_along(e$ledger$requests)
    }
    for (i in eligible) {
        e$current <- i
        row <- e$ledger$requests[[i]]
        target <- file.path(dir, "raw", paste0(row$request_id, ".asset"))
        if (identical(row$state, "downloaded")) {
            if (
                file.exists(target) &&
                    identical(checksum_file(target), row$raw_asset$sha256) &&
                    file.info(target)$size == row$raw_asset$bytes
            ) {
                verified[[i]] <- TRUE
                next
            }
            e$ledger$requests[[i]]$state <- "failed"
            e$ledger$requests[[i]]$error <- "cached_asset_changed"
            reanalysis_transfer__save(e)
            break
        }
        if (
            row$state == "ambiguous" ||
                (is.null(row$locator) && row$state != "not_submitted")
        ) {
            break
        }
        failure <- tryCatch(
            {
                if (is.null(row$locator)) {
                    if (
                        e$prior$submissions + e$execution$submissions >=
                            settings$max_submissions
                    ) {
                        reanalysis_transfer__abort("submission_budget")
                    }
                    if (!nzchar(config$key)) {
                        reanalysis_transfer__abort("missing_credentials")
                    }
                    reanalysis_transfer__remaining(e)
                    reanalysis_transfer__budget(e)
                    e$execution$submissions <- e$execution$submissions + 1
                    e$ledger$requests[[i]]$submission_attempted <- TRUE
                    e$ledger$requests[[i]]$state <- "submitting"
                    reanalysis_transfer__save(e)
                    submit_url <- paste0(
                        config$url,
                        "/retrieve/v1/processes/",
                        row$dataset,
                        "/execution"
                    )
                    response <- reanalysis_transfer__http(
                        e,
                        i,
                        "submit",
                        "POST",
                        submit_url,
                        body = list(inputs = row$request),
                        timeout = settings$wait_timeout
                    )
                    monitor <- cds__absolute_url(
                        cds__link(response, "monitor"),
                        submit_url
                    )
                    prefix <- paste0(config$url, "/retrieve/v1/jobs/")
                    id <- substring(monitor, nchar(prefix) + 1L)
                    if (
                        !startsWith(monitor, prefix) ||
                            !grepl("^[A-Za-z0-9_-]+$", id)
                    ) {
                        reanalysis_transfer__abort("invalid_locator")
                    }
                    e$ledger$requests[[i]]$locator <- id
                    e$ledger$requests[[i]]$state <- "submitted"
                    reanalysis_transfer__save(e)
                }
                job_url <- paste0(
                    config$url,
                    "/retrieve/v1/jobs/",
                    e$ledger$requests[[i]]$locator
                )
                wait_start <- proc.time()[["elapsed"]]
                repeat {
                    remaining <- settings$wait_timeout -
                        (proc.time()[["elapsed"]] - wait_start)
                    if (remaining <= 0) {
                        reanalysis_transfer__abort("wait_timeout")
                    }
                    status <- reanalysis_transfer__http(
                        e,
                        i,
                        "poll",
                        "GET",
                        job_url,
                        timeout = remaining
                    )
                    if (identical(status$status, "successful")) {
                        break
                    }
                    if (
                        !status$status %in% c("accepted", "running", "queued")
                    ) {
                        reanalysis_transfer__abort("provider_job_failed")
                    }
                    Sys.sleep(min(
                        poll_interval,
                        remaining,
                        reanalysis_transfer__remaining(e)
                    ))
                }
                result_url <- paste0(job_url, "/results")
                result <- reanalysis_transfer__http(
                    e,
                    i,
                    "result",
                    "GET",
                    result_url,
                    timeout = settings$wait_timeout
                )
                asset <- result$asset$value
                url <- cds__absolute_url(asset$href, result_url)
                if (!grepl("^https?://[^/@?#]+([/?#]|$)", url)) {
                    reanalysis_transfer__abort("invalid_asset_url")
                }
                e$ledger$requests[[i]]$state <- "downloading"
                reanalysis_transfer__save(e)
                part <- paste0(target, ".part")
                reanalysis_transfer__http(
                    e,
                    i,
                    "download",
                    "GET",
                    url,
                    target = part,
                    timeout = settings$download_timeout
                )
                size <- file.info(part)$size
                expected <- asset[["file:size"]]
                if (
                    !is.null(expected) &&
                        (!is.numeric(expected) ||
                            length(expected) != 1L ||
                            !is.finite(expected) ||
                            size != expected)
                ) {
                    reanalysis_transfer__abort("asset_size_mismatch")
                }
                hash <- checksum_file(part)
                if (!file.rename(part, target)) {
                    reanalysis_transfer__abort("asset_publish_failed")
                }
                e$ledger$requests[[i]]$raw_asset <- list(
                    bytes = size,
                    sha256 = hash
                )
                e$ledger$requests[[i]]$state <- "downloaded"
                e$ledger$requests[[i]]$error <- NULL
                reanalysis_transfer__save(e)
                verified[[i]] <- TRUE
                NULL
            },
            error = function(error) {
                if (inherits(error, "epwshiftr_transfer_error")) {
                    error$code
                } else {
                    "transport_failed"
                }
            },
            interrupt = function(error) "interrupted"
        )
        if (!is.null(failure)) {
            state <- if (e$ledger$requests[[i]]$state == "submitting") {
                "ambiguous"
            } else if (grepl("budget|timeout", failure)) {
                "budget_exhausted"
            } else {
                "failed"
            }
            e$ledger$requests[[i]]$state <- state
            e$ledger$requests[[i]]$error <- failure
            reanalysis_transfer__save(e)
            break
        }
    }
    complete <- verified &
        vapply(
            e$ledger$requests,
            function(x) identical(x$state, "downloaded"),
            logical(1)
        )
    e$execution$status <- if (all(complete)) "downloaded" else "incomplete"
    e$execution$finished_at <- reanalysis_transfer__time()
    reanalysis_transfer__save(e)
    files <- stats::setNames(character(), character())
    if (any(complete)) {
        files <- file.path(
            dir,
            "raw",
            paste0(plan$requests$request_id[complete], ".asset")
        )
        names(files) <- plan$requests$request_id[complete]
    }
    list(
        status = e$execution$status,
        files = files,
        requests = e$ledger$requests,
        execution = e$execution,
        cumulative = e$ledger$cumulative,
        accounting_uncertain = isTRUE(e$ledger$accounting_uncertain),
        receipt = ledger_path
    )
}
# }}}

# Validate the per-invocation contract independently of any provider calls.
reanalysis_transfer__limits <- function(limits) {
    defaults <- list(
        max_submissions = Inf,
        max_file_bytes = Inf,
        max_total_bytes = Inf,
        wait_timeout = 3600,
        download_timeout = 600,
        total_timeout = Inf
    )
    checkmate::assert_list(limits, names = "unique")
    if (length(setdiff(names(limits), names(defaults)))) {
        cli::cli_abort("Unknown transfer limit.")
    }
    defaults[names(limits)] <- limits
    for (name in names(defaults)) {
        value <- defaults[[name]]
        checkmate::assert_number(
            value,
            lower = .Machine$double.eps,
            finite = FALSE
        )
        if (
            startsWith(name, "max_") &&
                is.finite(value) &&
                value != floor(value)
        ) {
            cli::cli_abort(
                "Byte and submission limits must be integers or Inf."
            )
        }
    }
    defaults
}

# Record UTC timestamps without inheriting the host's local time zone.
reanalysis_transfer__time <- function() {
    format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

# Same-directory temporary writes and rename prevent half-written JSON records.
reanalysis_transfer__write <- function(value, path) {
    temp <- tempfile("ledger-", tmpdir = dirname(path))
    on.exit(unlink(temp), add = TRUE)
    jsonlite::write_json(
        value,
        temp,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null",
        digits = NA
    )
    if (!file.rename(temp, path)) {
        cli::cli_abort("Cannot publish transfer ledger.")
    }
}

# Save counters and request state as one atomic snapshot, not separate files.
reanalysis_transfer__save <- function(e) {
    if (!is.null(e$current)) {
        e$ledger$requests[[e$current]]$updated_at <- reanalysis_transfer__time()
    }
    e$execution$elapsed_seconds <- proc.time()[["elapsed"]] - e$start
    e$ledger$executions[[e$run]] <- e$execution
    e$ledger$cumulative <- list(
        submissions = e$prior$submissions + e$execution$submissions,
        bytes_received = e$prior$bytes_received + e$execution$bytes_received,
        asset_bytes_written = e$prior$asset_bytes_written +
            e$execution$asset_bytes_written
    )
    reanalysis_transfer__write(e$ledger, e$path)
}

# A resumed call cannot refill a previously consumed response-byte allowance.
reanalysis_transfer__budget <- function(e) {
    if (
        e$prior$bytes_received + e$execution$bytes_received >=
            e$limits$max_total_bytes
    ) {
        reanalysis_transfer__abort("total_budget")
    }
}

# Use a stable error category only; provider messages can contain signed URLs.
reanalysis_transfer__abort <- function(code) {
    stop(structure(
        list(message = code, call = NULL, code = code),
        class = c("epwshiftr_transfer_error", "error", "condition")
    ))
}

# Monotonic elapsed time bounds control calls, polling sleeps and downloads.
reanalysis_transfer__remaining <- function(e) {
    remaining <- e$limits$total_timeout - (proc.time()[["elapsed"]] - e$start)
    if (remaining <= 0) {
        reanalysis_transfer__abort("total_timeout")
    }
    remaining
}

# Stream response bodies directly and persist every observed block. Never trust
# Content-Length alone, silently discard failed bytes, or retain signed URLs.
reanalysis_transfer__http <- function(
    e,
    i,
    phase,
    method,
    url,
    body = NULL,
    target = NULL,
    timeout
) {
    timeout <- min(timeout, reanalysis_transfer__remaining(e))
    reanalysis_transfer__budget(e)
    # Authenticate and prepare local resources before declaring a network
    # attempt. Missing credentials or an unwritable target are known preflight
    # failures, not interrupted transfers with an unknown byte count.
    handle <- curl::new_handle(
        timeout_ms = min(.Machine$integer.max, ceiling(timeout * 1000)),
        connecttimeout_ms = min(30000, ceiling(timeout * 1000)),
        followlocation = FALSE,
        customrequest = method,
        accept_encoding = "identity"
    )
    if (startsWith(url, paste0(e$config$url, "/"))) {
        if (!nzchar(e$config$key)) {
            reanalysis_transfer__abort("missing_credentials")
        }
        curl::handle_setheaders(handle, `PRIVATE-TOKEN` = e$config$key)
    }
    if (!is.null(body)) {
        curl::handle_setheaders(
            handle,
            `PRIVATE-TOKEN` = e$config$key,
            `Content-Type` = "application/json"
        )
        curl::handle_setopt(
            handle,
            postfields = as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
        )
    }
    con <- if (is.null(target)) {
        rawConnection(raw(), "wb")
    } else {
        file(target, "wb")
    }
    on.exit(close(con), add = TRUE)
    index <- length(e$ledger$requests[[i]]$attempts) + 1L
    e$ledger$requests[[i]]$attempts[[index]] <- list(
        phase = phase,
        started_at = reanalysis_transfer__time(),
        bytes_received = 0,
        bytes_written = 0,
        outcome = "in_flight",
        execution = e$run
    )
    reanalysis_transfer__save(e)
    code <- NULL
    timeout_warning <- FALSE
    received <- 0
    response <- tryCatch(
        withCallingHandlers(
            curl::curl_fetch_stream(
                url,
                function(chunk) {
                    count <- length(chunk)
                    received <<- received + count
                    e$execution$bytes_received <- e$execution$bytes_received +
                        count
                    e$ledger$requests[[i]]$attempts[[
                        index
                    ]]$bytes_received <- received
                    cap <- if (is.null(target)) {
                        1024^2
                    } else {
                        e$limits$max_file_bytes
                    }
                    total <- e$prior$bytes_received + e$execution$bytes_received
                    if (received > cap || total > e$limits$max_total_bytes) {
                        code <<- if (total > e$limits$max_total_bytes) {
                            "total_budget"
                        } else if (is.null(target)) {
                            "control_response_budget"
                        } else {
                            "file_budget"
                        }
                        reanalysis_transfer__save(e)
                        # curl_fetch_stream ignores callback return values. Raise a
                        # condition so the connection closes before the next read.
                        reanalysis_transfer__abort(code)
                    }
                    writeBin(chunk, con)
                    e$ledger$requests[[i]]$attempts[[
                        index
                    ]]$bytes_written <- received
                    if (!is.null(target)) {
                        e$execution$asset_bytes_written <- e$execution$asset_bytes_written +
                            count
                    }
                    reanalysis_transfer__save(e)
                    TRUE
                },
                handle = handle
            ),
            warning = function(warning) {
                # Connection opening may expose timeout only as a warning followed
                # by a generic R connection error. Classify it transiently, without
                # recording messages that may contain credentials or signed URLs.
                timeout_warning <<- timeout_warning ||
                    grepl(
                        "(Operation|Connection|Resolving) timed out|Timeout was reached",
                        conditionMessage(warning)
                    )
                invokeRestart("muffleWarning")
            }
        ),
        error = function(error) {
            if (inherits(error, "epwshiftr_transfer_error")) {
                code <<- error$code
            } else if (
                inherits(error, "curl_error_operation_timedout") ||
                    timeout_warning
            ) {
                code <<- if (
                    proc.time()[["elapsed"]] - e$start >= e$limits$total_timeout
                ) {
                    "total_timeout"
                } else if (phase == "download") {
                    "download_timeout"
                } else {
                    "wait_timeout"
                }
            }
            NULL
        }
    )
    if (is.null(response) && is.null(code)) {
        code <- "transport_failed"
    }
    if (
        !is.null(response) &&
            (response$status_code < 200L ||
                response$status_code >= 300L ||
                (phase == "download" && response$status_code != 200L))
    ) {
        code <- paste0("HTTP_", response$status_code)
    }
    e$ledger$requests[[i]]$attempts[[index]]$outcome <- if (is.null(code)) {
        "received"
    } else {
        code
    }
    e$ledger$requests[[i]]$attempts[[
        index
    ]]$finished_at <- reanalysis_transfer__time()
    reanalysis_transfer__save(e)
    if (!is.null(code)) {
        reanalysis_transfer__abort(code)
    }
    if (is.null(target)) {
        jsonlite::fromJSON(
            rawToChar(rawConnectionValue(con)),
            simplifyVector = FALSE
        )
    } else {
        invisible(NULL)
    }
}

# vim: fdm=marker :
