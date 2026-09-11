#' @include utils.R
NULL

# Copernicus Data Store transport --------------------------------------------

CDS__DEFAULT_URL <- "https://cds.climate.copernicus.eu/api"
CDS__TERMINAL_STATUSES <- c(
    "successful", "failed", "rejected", "dismissed", "deleted"
)

# Parse the simple key-value configuration format used by both current ECMWF
# and legacy CDS client configuration files without evaluating user content.
cds__read_config_file <- function(path) {
    path <- path.expand(path)
    if (!file.exists(path)) {
        return(list())
    }
    lines <- readLines(path, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
    output <- list()
    for (line in lines) {
        separator <- regexpr(":", line, fixed = TRUE)[[1L]]
        if (separator < 1L) {
            next
        }
        name <- trimws(substr(line, 1L, separator - 1L))
        value <- trimws(substr(line, separator + 1L, nchar(line)))
        if (nzchar(name) && nzchar(value)) {
            output[[name]] <- value
        }
    }
    output
}

# Resolve CDS configuration in the same precedence order as the official
# client while accepting the established `.cdsapirc` file as a fallback.
cds__config <- function(require_key = TRUE) {
    checkmate::assert_flag(require_key)
    modern_path <- Sys.getenv(
        "ECMWF_DATASTORES_RC_FILE",
        unset = "~/.ecmwfdatastoresrc"
    )
    modern <- cds__read_config_file(modern_path)
    legacy <- cds__read_config_file("~/.cdsapirc")
    url <- Sys.getenv(
        "ECMWF_DATASTORES_URL",
        unset = Sys.getenv(
            "CDSAPI_URL",
            unset = shift_coalesce(modern$url, shift_coalesce(
                legacy$url,
                CDS__DEFAULT_URL
            ))
        )
    )
    key <- Sys.getenv(
        "ECMWF_DATASTORES_KEY",
        unset = Sys.getenv(
            "CDSAPI_KEY",
            unset = Sys.getenv(
                "CDS_API_KEY",
                unset = shift_coalesce(modern$key, legacy$key)
            )
        )
    )
    url <- sub("/+$", "", url)
    if (isTRUE(require_key) && (is.null(key) || !nzchar(key))) {
        cli::cli_abort(c(
            "A Copernicus Data Store API key is required to retrieve ERA data.",
            "i" = paste(
                "Set `ECMWF_DATASTORES_KEY` or add `key:` to",
                "`~/.ecmwfdatastoresrc` or `~/.cdsapirc`."
            )
        ), class = "epwshiftr_cds_auth_error")
    }
    if (!is.null(key) && grepl(":", key, fixed = TRUE)) {
        cli::cli_abort(c(
            "The configured CDS key uses the retired `UID:key` form.",
            "i" = "Replace it with the personal access token shown in the current CDS profile."
        ), class = "epwshiftr_cds_auth_error")
    }
    list(url = url, key = key)
}

# Resolve `.` and `..` URL path segments without introducing another HTTP
# dependency into the provider transport.
cds__normalize_url_path <- function(url) {
    parsed <- regmatches(
        url,
        regexec("^(https?://[^/]+)(/[^?#]*)?([?#].*)?$", url)
    )[[1L]]
    if (!length(parsed)) {
        return(url)
    }
    origin <- parsed[[2L]]
    path <- shift_coalesce(parsed[[3L]], "/")
    suffix <- shift_coalesce(parsed[[4L]], "")
    stack <- character()
    for (part in strsplit(path, "/", fixed = TRUE)[[1L]]) {
        if (!nzchar(part) || identical(part, ".")) {
            next
        }
        if (identical(part, "..")) {
            if (length(stack)) {
                stack <- stack[-length(stack)]
            }
        } else {
            stack <- c(stack, part)
        }
    }
    paste0(origin, "/", paste(stack, collapse = "/"), suffix)
}

# Join a relative API link from an OGC job response to its response URL.
cds__absolute_url <- function(url, base) {
    checkmate::assert_string(url, min.chars = 1L)
    checkmate::assert_string(base, min.chars = 1L)
    if (grepl("^https?://", url, ignore.case = TRUE)) {
        return(cds__normalize_url_path(url))
    }
    base <- sub("[#?].*$", "", base)
    base_parts <- strsplit(base, "/", fixed = TRUE)[[1L]]
    origin <- paste(base_parts[seq_len(min(3L, length(base_parts)))],
        collapse = "/")
    if (startsWith(url, "/")) {
        return(cds__normalize_url_path(paste0(origin, url)))
    }
    parent <- sub("/[^/]*$", "/", base)
    cds__normalize_url_path(paste0(parent, url))
}

# Remove a configured secret from provider error text before it reaches logs,
# diagnostics, snapshots, or a persisted run failure.
cds__redact <- function(value, key = NULL) {
    value <- as.character(value)
    if (!is.null(key) && nzchar(key)) {
        value <- gsub(key, "<redacted>", value, fixed = TRUE)
    }
    value
}

# Classify provider HTTP failures so callers can distinguish invalid
# authentication and unaccepted data terms from transient request failures.
cds__http_error_classes <- function(status_code, provider_message) {
    checkmate::assert_count(status_code)
    provider_message <- paste(as.character(provider_message), collapse = " ")
    if (identical(as.integer(status_code), 401L)) {
        return(c(
            "epwshiftr_cds_auth_error",
            "epwshiftr_cds_request_error"
        ))
    }
    licence_error <- identical(as.integer(status_code), 403L) && grepl(
        "licen[cs]e|terms?.*(accept|agree)|(accept|agree).*terms?",
        provider_message,
        ignore.case = TRUE
    )
    if (licence_error) {
        return(c(
            "epwshiftr_cds_license_error",
            "epwshiftr_cds_request_error"
        ))
    }
    "epwshiftr_cds_request_error"
}

# Verify a configured personal access token through the official CDS profile
# endpoint without requesting, staging, or downloading any climate dataset.
cds__check_authentication <- function(config = cds__config(), timeout = 120) {
    checkmate::assert_list(config, names = "unique")
    response <- cds__http(
        "POST",
        paste0(config$url, "/profiles/v1/account/verification/pat"),
        key = config$key,
        timeout = timeout
    )
    invisible(identical(response$status_code, 200L))
}

# Build the public CDS page where a user can inspect and accept the terms for
# one dataset. The package never accepts those terms on the user's behalf.
cds__dataset_license_url <- function(dataset_id) {
    checkmate::assert_string(dataset_id, min.chars = 1L)
    paste0(
        "https://cds.climate.copernicus.eu/datasets/",
        dataset_id,
        "?tab=download#manage-licences"
    )
}

# Execute one JSON CDS request through curl and return status, headers, and a
# parsed body. Keeping this function small makes the full async lifecycle easy
# to exercise against a local mock server.
cds__http <- function(
    method,
    url,
    key,
    body = NULL,
    timeout = 120
) {
    checkmate::assert_choice(method, c("GET", "POST", "DELETE"))
    checkmate::assert_string(url, min.chars = 1L)
    checkmate::assert_string(key, min.chars = 1L)
    checkmate::assert_number(timeout, lower = 1, finite = TRUE)
    handle <- curl::new_handle(
        timeout = timeout,
        connecttimeout = min(timeout, 30),
        failonerror = FALSE
    )
    curl::handle_setheaders(
        handle,
        `PRIVATE-TOKEN` = key,
        Accept = "application/json",
        `User-Agent` = "epwshiftr"
    )
    options <- list(customrequest = method)
    if (!is.null(body)) {
        encoded <- as.character(jsonlite::toJSON(
            body,
            auto_unbox = TRUE,
            null = "null",
            na = "null"
        ))
        curl::handle_setheaders(
            handle,
            `PRIVATE-TOKEN` = key,
            Accept = "application/json",
            `Content-Type` = "application/json",
            `User-Agent` = "epwshiftr"
        )
        options$postfields <- encoded
    }
    do.call(curl::handle_setopt, c(list(handle = handle), options))
    response <- tryCatch(
        curl::curl_fetch_memory(url, handle = handle),
        error = function(error) {
            cli::cli_abort(
                "CDS request failed: {cds__redact(conditionMessage(error), key)}",
                class = "epwshiftr_cds_request_error"
            )
        }
    )
    text <- rawToChar(response$content)
    parsed <- if (nzchar(text)) {
        tryCatch(
            jsonlite::fromJSON(text, simplifyVector = FALSE),
            error = function(error) list(message = text)
        )
    } else {
        list()
    }
    if (response$status_code < 200L || response$status_code >= 300L) {
        provider_message <- shift_coalesce(
            parsed$detail,
            shift_coalesce(parsed$title, shift_coalesce(parsed$message, text))
        )
        provider_message <- paste(
            cds__redact(provider_message, key),
            collapse = " "
        )
        cli::cli_abort(
            c(
                "CDS returned HTTP {response$status_code}.",
                "x" = provider_message
            ),
            class = cds__http_error_classes(
                response$status_code,
                provider_message
            ),
            status_code = response$status_code,
            provider_message = provider_message
        )
    }
    list(
        status_code = response$status_code,
        headers = response$headers,
        body = parsed,
        url = url
    )
}

# Return the unique link matching one OGC relation from a provider response.
cds__link <- function(response, relation, required = TRUE) {
    checkmate::assert_string(relation, min.chars = 1L)
    checkmate::assert_flag(required)
    links <- shift_coalesce(response$links, list())
    matches <- Filter(function(link) {
        identical(as.character(link$rel), relation) &&
            !is.null(link$href) && nzchar(as.character(link$href))
    }, links)
    if (length(matches) == 1L) {
        return(as.character(matches[[1L]]$href))
    }
    if (isTRUE(required)) {
        cli::cli_abort(
            "CDS response does not contain one unique {.val {relation}} link.",
            class = "epwshiftr_cds_response_error"
        )
    }
    NULL
}

# Submit one dataset request and retain only the public job locator and ID.
cds__submit <- function(dataset_id, request, config = cds__config()) {
    checkmate::assert_string(dataset_id, min.chars = 1L)
    checkmate::assert_list(request, names = "unique")
    url <- sprintf(
        "%s/retrieve/v1/processes/%s/execution",
        config$url,
        dataset_id
    )
    response <- tryCatch(
        cds__http(
            "POST",
            url,
            key = config$key,
            body = list(inputs = request)
        ),
        epwshiftr_cds_license_error = function(error) {
            cli::cli_abort(
                c(
                    "CDS access terms have not been accepted for dataset {.val {dataset_id}}.",
                    "i" = "Review and accept them at {cds__dataset_license_url(dataset_id)}.",
                    "x" = error$provider_message
                ),
                class = c(
                    "epwshiftr_cds_license_error",
                    "epwshiftr_cds_request_error"
                ),
                status_code = error$status_code,
                provider_message = error$provider_message
            )
        }
    )
    monitor <- cds__absolute_url(
        cds__link(response$body, "monitor"),
        response$url
    )
    list(
        dataset_id = dataset_id,
        request_id = sub(".*/", "", monitor),
        monitor_url = monitor,
        status = as.character(shift_coalesce(
            response$body$status,
            "accepted"
        ))
    )
}

# Fetch one current job snapshot without retaining request parameters or auth.
cds__status <- function(job, config = cds__config()) {
    checkmate::assert_list(job, names = "unique")
    response <- cds__http(
        "GET",
        job$monitor_url,
        key = config$key
    )
    list(
        dataset_id = job$dataset_id,
        request_id = job$request_id,
        monitor_url = job$monitor_url,
        status = as.character(response$body$status),
        links = shift_coalesce(response$body$links, list()),
        message = shift_coalesce(response$body$message, NULL)
    )
}

# Poll one submitted job with bounded delays and return its successful snapshot.
cds__wait <- function(
    job,
    config = cds__config(),
    timeout = 86400,
    poll_interval = 1,
    reporter = NULL
) {
    checkmate::assert_number(timeout, lower = 1, finite = TRUE)
    checkmate::assert_number(poll_interval, lower = 0, finite = TRUE)
    started <- Sys.time()
    delay <- poll_interval
    repeat {
        current <- cds__status(job, config = config)
        if (length(current$status) != 1L || is.na(current$status) ||
            !nzchar(current$status)) {
            cli::cli_abort(
                "CDS returned a missing or malformed job status.",
                class = "epwshiftr_cds_response_error"
            )
        }
        if (identical(current$status, "successful")) {
            return(current)
        }
        if (current$status %in% setdiff(
            CDS__TERMINAL_STATUSES,
            "successful"
        )) {
            cli::cli_abort(
                c(
                    "CDS request {.val {current$request_id}} ended with status {.val {current$status}}.",
                    "x" = cds__redact(current$message, config$key)
                ),
                class = "epwshiftr_cds_processing_error"
            )
        }
        elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
        if (elapsed >= timeout) {
            cli::cli_abort(
                "CDS request {.val {current$request_id}} did not finish within {timeout} seconds.",
                class = "epwshiftr_cds_timeout"
            )
        }
        if (!is.null(reporter)) {
            reporter$heartbeat(
                sprintf("ERA5 request %s is %s", current$request_id,
                    current$status),
                details = list(
                    unit_type = "reanalysis_request",
                    request_id = current$request_id,
                    status = current$status
                )
            )
            reporter$check_cancel("reanalysis")
        }
        if (delay > 0) {
            Sys.sleep(min(delay, 30))
        }
        delay <- min(max(1, delay * 1.5), 30)
    }
}

# Resolve the downloadable asset returned by one successful OGC job.
cds__result <- function(job, config = cds__config()) {
    result_url <- cds__link(job, "results", required = FALSE)
    if (is.null(result_url)) {
        result_url <- paste0(job$monitor_url, "/results")
    }
    response <- cds__http(
        "GET",
        cds__absolute_url(result_url, job$monitor_url),
        key = config$key
    )
    asset <- response$body$asset$value
    if (is.null(asset$href)) {
        cli::cli_abort(
            "CDS result does not contain a downloadable asset.",
            class = "epwshiftr_cds_response_error"
        )
    }
    list(
        url = cds__absolute_url(as.character(asset$href), response$url),
        size = suppressWarnings(as.numeric(asset[["file:size"]])),
        type = as.character(shift_coalesce(
            asset$type,
            "application/octet-stream"
        ))
    )
}

# Detect ZIP payloads from their signature because the CDS asset media type is
# not consistently specific enough to distinguish an archive from NetCDF.
cds__is_zip_file <- function(path) {
    checkmate::assert_file_exists(path)
    signature <- readBin(path, what = "raw", n = 4L)
    length(signature) >= 4L && identical(
        as.integer(signature[seq_len(4L)]),
        c(0x50L, 0x4bL, 0x03L, 0x04L)
    )
}

# Extract the single NetCDF member returned for one variable-sized CDS request.
# Archive paths are validated before extraction so provider filenames cannot
# escape the temporary directory or silently select an unrelated member.
cds__extract_netcdf_archive <- function(archive, directory) {
    checkmate::assert_file_exists(archive)
    checkmate::assert_directory_exists(directory)
    manifest <- tryCatch(
        utils::unzip(archive, list = TRUE),
        error = function(error) {
            cli::cli_abort(
                "CDS returned an unreadable ZIP archive: {conditionMessage(error)}",
                class = "epwshiftr_cds_response_error"
            )
        }
    )
    members <- manifest$Name[grepl("[.]nc$", manifest$Name,
        ignore.case = TRUE)]
    safe_member <- length(members) == 1L &&
        identical(basename(members), members) &&
        !grepl("[\\\\/]", members)
    if (!safe_member) {
        cli::cli_abort(
            "CDS ZIP results must contain one top-level NetCDF file.",
            class = "epwshiftr_cds_response_error"
        )
    }
    extracted <- tryCatch(
        utils::unzip(
            archive,
            files = members,
            exdir = directory,
            junkpaths = TRUE
        ),
        error = function(error) {
            cli::cli_abort(
                "CDS NetCDF extraction failed: {conditionMessage(error)}",
                class = "epwshiftr_cds_response_error"
            )
        }
    )
    if (length(extracted) != 1L || !file.exists(extracted)) {
        cli::cli_abort(
            "CDS ZIP extraction did not produce one NetCDF file.",
            class = "epwshiftr_cds_response_error"
        )
    }
    normalizePath(extracted, winslash = "/", mustWork = TRUE)
}

# Download one result atomically and verify its declared size when available.
cds__download <- function(asset, target, config = cds__config()) {
    checkmate::assert_list(asset, names = "unique")
    checkmate::assert_string(target, min.chars = 1L)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(tmpdir = dirname(target), fileext = ".part")
    on.exit(unlink(temporary, force = TRUE), add = TRUE)
    handle <- curl::new_handle(timeout = 0, connecttimeout = 30)
    curl::handle_setheaders(
        handle,
        `PRIVATE-TOKEN` = config$key,
        `User-Agent` = "epwshiftr"
    )
    tryCatch(
        curl::curl_download(
            asset$url,
            destfile = temporary,
            quiet = TRUE,
            mode = "wb",
            handle = handle
        ),
        error = function(error) {
            cli::cli_abort(
                "CDS result download failed: {cds__redact(conditionMessage(error), config$key)}",
                class = "epwshiftr_cds_download_error"
            )
        }
    )
    if (length(asset$size) && is.finite(asset$size) &&
        file.info(temporary)$size != asset$size) {
        cli::cli_abort(
            "CDS result size does not match the provider manifest.",
            class = "epwshiftr_cds_download_error"
        )
    }
    completed <- temporary
    if (cds__is_zip_file(temporary)) {
        extraction_directory <- tempfile(
            "cds-netcdf-",
            tmpdir = dirname(target)
        )
        dir.create(extraction_directory)
        on.exit(unlink(extraction_directory, recursive = TRUE,
            force = TRUE), add = TRUE)
        completed <- cds__extract_netcdf_archive(
            temporary,
            extraction_directory
        )
    }
    if (file.exists(target)) {
        unlink(target, force = TRUE)
    }
    if (!file.rename(completed, target)) {
        cli::cli_abort("Could not move the completed CDS result into place.")
    }
    normalizePath(target, winslash = "/", mustWork = TRUE)
}

# Run the complete CDS submit, wait, result, and download sequence for one
# variable-sized request. Splitting ERA fields into separate jobs keeps errors
# attributable and avoids grouped archive outputs.
cds__retrieve <- function(
    dataset_id,
    request,
    target,
    config = cds__config(),
    reporter = NULL,
    timeout = 86400,
    poll_interval = 1,
    overwrite = FALSE
) {
    checkmate::assert_flag(overwrite)
    if (file.exists(target) && !isTRUE(overwrite)) {
        return(list(
            path = normalizePath(target, winslash = "/", mustWork = TRUE),
            job = NULL,
            reused = TRUE
        ))
    }
    submitted <- cds__submit(dataset_id, request, config = config)
    completed <- tryCatch(
        cds__wait(
            submitted,
            config = config,
            timeout = timeout,
            poll_interval = poll_interval,
            reporter = reporter
        ),
        epwshiftr_shift_cancelled = function(error) {
            try(cds__cancel(submitted, config = config), silent = TRUE)
            stop(error)
        }
    )
    asset <- cds__result(completed, config = config)
    path <- cds__download(asset, target, config = config)
    list(path = path, job = completed, reused = FALSE)
}

# Cancel one provider job when a future batch integration has a remote request
# still in progress. Completed jobs are left untouched.
cds__cancel <- function(job, config = cds__config()) {
    response <- cds__http(
        "DELETE",
        job$monitor_url,
        key = config$key
    )
    invisible(response$body)
}
