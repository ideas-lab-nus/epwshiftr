#' @include utils.R
NULL

# Copernicus Data Store transport
CDS__DEFAULT_URL <- "https://cds.climate.copernicus.eu/api"
CDS__TERMINAL_STATUSES <- c(
    "successful",
    "failed",
    "rejected",
    "dismissed",
    "deleted"
)

# Parse the simple key-value configuration format used by both current ECMWF
# and legacy CDS client configuration files without evaluating user content.
# cds__read_config_file {{{
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
# }}}

# Resolve CDS configuration in the same precedence order as the official
# client while accepting the established `.cdsapirc` file as a fallback.
# cds__config {{{
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
            unset = shift_stage__coalesce(
                modern$url,
                shift_stage__coalesce(
                    legacy$url,
                    CDS__DEFAULT_URL
                )
            )
        )
    )
    key <- Sys.getenv(
        "ECMWF_DATASTORES_KEY",
        unset = Sys.getenv(
            "CDSAPI_KEY",
            unset = Sys.getenv(
                "CDS_API_KEY",
                # Sys.getenv evaluates fallback arguments even when a key is
                # present, so absent config files must yield a string, not NULL.
                unset = shift_stage__coalesce(
                    modern$key,
                    shift_stage__coalesce(legacy$key, "")
                )
            )
        )
    )
    url <- sub("/+$", "", url)
    if (isTRUE(require_key) && (is.null(key) || !nzchar(key))) {
        cli::cli_abort(
            c(
                "A Copernicus Data Store API key is required to retrieve ERA data.",
                "i" = paste(
                    "Set `ECMWF_DATASTORES_KEY` or add `key:` to",
                    "`~/.ecmwfdatastoresrc` or `~/.cdsapirc`."
                )
            ),
            class = "epwshiftr_cds_auth_error"
        )
    }
    if (!is.null(key) && grepl(":", key, fixed = TRUE)) {
        cli::cli_abort(
            c(
                "The configured CDS key uses the retired `UID:key` form.",
                "i" = "Replace it with the personal access token shown in the current CDS profile."
            ),
            class = "epwshiftr_cds_auth_error"
        )
    }
    list(url = url, key = key)
}
# }}}

# Resolve `.` and `..` URL path segments without introducing another HTTP
# dependency into the provider transport.
# cds__normalize_url_path {{{
cds__normalize_url_path <- function(url) {
    parsed <- regmatches(
        url,
        regexec("^(https?://[^/]+)(/[^?#]*)?([?#].*)?$", url)
    )[[1L]]
    if (!length(parsed)) {
        return(url)
    }
    origin <- parsed[[2L]]
    path <- shift_stage__coalesce(parsed[[3L]], "/")
    suffix <- shift_stage__coalesce(parsed[[4L]], "")
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
# }}}

# Join a relative API link from an OGC job response to its response URL.
# cds__absolute_url {{{
cds__absolute_url <- function(url, base) {
    checkmate::assert_string(url, min.chars = 1L)
    checkmate::assert_string(base, min.chars = 1L)
    if (grepl("^https?://", url, ignore.case = TRUE)) {
        return(cds__normalize_url_path(url))
    }
    base <- sub("[#?].*$", "", base)
    base_parts <- strsplit(base, "/", fixed = TRUE)[[1L]]
    origin <- paste(
        base_parts[seq_len(min(3L, length(base_parts)))],
        collapse = "/"
    )
    if (startsWith(url, "/")) {
        return(cds__normalize_url_path(paste0(origin, url)))
    }
    parent <- sub("/[^/]*$", "/", base)
    cds__normalize_url_path(paste0(parent, url))
}
# }}}

# Remove a configured secret from provider error text before it reaches logs,
# diagnostics, snapshots, or a persisted run failure.
# cds__redact {{{
cds__redact <- function(value, key = NULL) {
    value <- as.character(value)
    if (!is.null(key) && nzchar(key)) {
        value <- gsub(key, "<redacted>", value, fixed = TRUE)
    }
    value
}
# }}}

# Classify provider HTTP failures so callers can distinguish invalid
# authentication and unaccepted data terms from transient request failures.
# cds__http_error_classes {{{
cds__http_error_classes <- function(status_code, provider_message) {
    checkmate::assert_count(status_code)
    provider_message <- paste(as.character(provider_message), collapse = " ")
    if (identical(as.integer(status_code), 401L)) {
        return(c(
            "epwshiftr_cds_auth_error",
            "epwshiftr_cds_request_error"
        ))
    }
    licence_error <- identical(as.integer(status_code), 403L) &&
        grepl(
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
# }}}

# Verify a configured personal access token through the official CDS profile
# endpoint without requesting, staging, or downloading any climate dataset.
# cds__check_authentication {{{
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
# }}}

# Build the public CDS page where a user can inspect and accept the terms for
# one dataset. The package never accepts those terms on the user's behalf.
# cds__dataset_license_url {{{
cds__dataset_license_url <- function(dataset_id) {
    checkmate::assert_string(dataset_id, min.chars = 1L)
    paste0(
        "https://cds.climate.copernicus.eu/datasets/",
        dataset_id,
        "?tab=download#manage-licences"
    )
}
# }}}

# Execute one JSON CDS request through curl and return status, headers, and a
# parsed body. Keeping this function small makes the full async lifecycle easy
# to exercise against a local mock server.
# cds__http {{{
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
        provider_message <- shift_stage__coalesce(
            parsed$detail,
            shift_stage__coalesce(
                parsed$title,
                shift_stage__coalesce(parsed$message, text)
            )
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
# }}}

# Return the unique link matching one OGC relation from a provider response.
# cds__link {{{
cds__link <- function(response, relation, required = TRUE) {
    checkmate::assert_string(relation, min.chars = 1L)
    checkmate::assert_flag(required)
    links <- shift_stage__coalesce(response$links, list())
    matches <- Filter(
        function(link) {
            identical(as.character(link$rel), relation) &&
                !is.null(link$href) &&
                nzchar(as.character(link$href))
        },
        links
    )
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
# }}}

# Submit one dataset request and retain only the public job locator and ID.
# cds__submit {{{
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
        status = as.character(shift_stage__coalesce(
            response$body$status,
            "accepted"
        ))
    )
}
# }}}

# Fetch one current job snapshot without retaining request parameters or auth.
# cds__status {{{
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
        links = shift_stage__coalesce(response$body$links, list()),
        message = shift_stage__coalesce(response$body$message, NULL)
    )
}
# }}}

# Poll one submitted job with bounded delays and return its successful snapshot.
# cds__wait {{{
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
        if (
            length(current$status) != 1L ||
                is.na(current$status) ||
                !nzchar(current$status)
        ) {
            cli::cli_abort(
                "CDS returned a missing or malformed job status.",
                class = "epwshiftr_cds_response_error"
            )
        }
        if (identical(current$status, "successful")) {
            return(current)
        }
        if (
            current$status %in%
                setdiff(
                    CDS__TERMINAL_STATUSES,
                    "successful"
                )
        ) {
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
                sprintf(
                    "ERA5 request %s is %s",
                    current$request_id,
                    current$status
                ),
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
# }}}

# Resolve the downloadable asset returned by one successful OGC job.
# cds__result {{{
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
        type = as.character(shift_stage__coalesce(
            asset$type,
            "application/octet-stream"
        ))
    )
}
# }}}

# Detect ZIP payloads from their signature because the CDS asset media type is
# not consistently specific enough to distinguish an archive from NetCDF.
# cds__is_zip_file {{{
cds__is_zip_file <- function(path) {
    checkmate::assert_file_exists(path)
    signature <- readBin(path, what = "raw", n = 4L)
    length(signature) >= 4L &&
        identical(
            as.integer(signature[seq_len(4L)]),
            c(0x50L, 0x4bL, 0x03L, 0x04L)
        )
}
# }}}

# Extract the single NetCDF member returned for one variable-sized CDS request.
# Archive paths are validated before extraction so provider filenames cannot
# escape the temporary directory or silently select an unrelated member.
# cds__extract_netcdf_archive {{{
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
    members <- manifest$Name[grepl("[.]nc$", manifest$Name, ignore.case = TRUE)]
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
# }}}

# Download one result atomically and verify its declared size when available.
# cds__download {{{
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
    if (
        length(asset$size) &&
            is.finite(asset$size) &&
            file.info(temporary)$size != asset$size
    ) {
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
        on.exit(
            unlink(extraction_directory, recursive = TRUE, force = TRUE),
            add = TRUE
        )
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
# }}}

# Run the complete CDS submit, wait, result, and download sequence for one
# variable-sized request. Splitting ERA fields into separate jobs keeps errors
# attributable and avoids grouped archive outputs.
# cds__retrieve {{{
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
# }}}

# Cancel one provider job when a future batch integration has a remote request
# still in progress. Completed jobs are left untouched.
# cds__cancel {{{
cds__cancel <- function(job, config = cds__config()) {
    response <- cds__http(
        "DELETE",
        job$monitor_url,
        key = config$key
    )
    invisible(response$body)
}
# }}}

# Reanalysis source contracts
REANALYSIS__FREQUENCIES <- c("hour", "day", "mon")
REANALYSIS__ACCESS <- c("auto", "arco", "cds")

# Normalize equivalent longitude representations before provider requests,
# distance calculations, and persistent identity construction.
# reanalysis__longitude {{{
reanalysis__longitude <- function(longitude) {
    normalized <- (as.numeric(longitude) + 180) %% 360 - 180
    normalized[normalized == -180 & as.numeric(longitude) > 0] <- 180
    normalized
}
# }}}

# ShiftReanalysisSpec records provider-neutral observational source intent.
# Credentials are deliberately excluded so persisted plans and console output
# remain safe to share.
# ShiftReanalysisSpec {{{
ShiftReanalysisSpec <- S7::new_class(
    "ShiftReanalysisSpec",
    properties = list(
        provider = S7::new_property(S7::class_character),
        dataset = S7::new_property(S7::class_character),
        product = S7::new_property(S7::class_character),
        years = S7::new_property(S7::class_integer),
        variables = S7::new_property(S7::class_any, default = NULL),
        frequency = S7::new_property(S7::class_any, default = NULL),
        access = S7::new_property(S7::class_character),
        options = S7::new_property(S7::class_list, default = list())
    ),
    # validator {{{
    validator = function(self) {
        for (property in c("provider", "dataset", "product", "access")) {
            value <- S7::prop(self, property)
            if (!checkmate::test_string(value, min.chars = 1L)) {
                return(sprintf("`%s` must be one non-empty string.", property))
            }
        }
        if (
            !length(self@years) ||
                anyNA(self@years) ||
                anyDuplicated(self@years)
        ) {
            return("`years` must contain unique, non-missing years.")
        }
        if (
            !is.null(self@variables) &&
                (!is.character(self@variables) ||
                    !length(self@variables) ||
                    anyNA(self@variables) ||
                    any(!nzchar(self@variables)) ||
                    anyDuplicated(self@variables))
        ) {
            return(
                "`variables` must be NULL or unique, non-empty CF variable IDs."
            )
        }
        if (!is.null(self@frequency)) {
            frequency <- unlist(self@frequency, use.names = TRUE)
            if (
                !is.character(frequency) ||
                    !length(frequency) ||
                    anyNA(frequency) ||
                    any(!frequency %in% REANALYSIS__FREQUENCIES)
            ) {
                return("`frequency` must use `hour`, `day`, or `mon`.")
            }
            if (
                length(frequency) > 1L &&
                    (is.null(names(frequency)) ||
                        any(!nzchar(names(frequency))) ||
                        anyDuplicated(names(frequency)))
            ) {
                return(
                    "A variable-specific `frequency` must be uniquely named."
                )
            }
        }
        if (!self@access %in% REANALYSIS__ACCESS) {
            return("`access` must be `auto`, `arco`, or `cds`.")
        }
        if (
            length(self@options) &&
                (is.null(names(self@options)) ||
                    any(!nzchar(names(self@options))) ||
                    anyDuplicated(names(self@options)))
        ) {
            return("`options` must be uniquely named.")
        }
        NULL
    }
    # }}}
)
# }}}

# Construct one validated reanalysis source specification for a public
# provider-specific wrapper.
# reanalysis__spec {{{
reanalysis__spec <- function(
    dataset,
    years,
    product,
    variables = NULL,
    frequency = NULL,
    access = "auto",
    options = list()
) {
    years <- sort(unique(as.integer(years)))
    if (!length(years) || anyNA(years) || any(years < 1800L | years > 2300L)) {
        cli::cli_abort("`years` must contain valid calendar years.")
    }
    if (!is.null(frequency)) {
        frequency_names <- names(frequency)
        frequency <- as.character(unlist(frequency, use.names = TRUE))
        names(frequency) <- frequency_names
    }
    access <- match.arg(access, REANALYSIS__ACCESS)
    checkmate::assert_list(options, names = "unique")

    ShiftReanalysisSpec(
        provider = "copernicus",
        dataset = dataset,
        product = product,
        years = years,
        variables = variables,
        frequency = frequency,
        access = access,
        options = options
    )
}
# }}}

# Serialize only reproducible source intent. Authentication is read again in
# the executing process and therefore cannot leak through a plan specification.
# reanalysis__spec_value {{{
reanalysis__spec_value <- function(spec) {
    if (!S7::S7_inherits(spec, ShiftReanalysisSpec)) {
        cli::cli_abort("`spec` must be a {.cls ShiftReanalysisSpec}.")
    }
    list(
        mode = "reanalysis",
        role = "observed_reference",
        dataset = spec@dataset,
        years = as.integer(spec@years),
        product = spec@product,
        variables = spec@variables,
        frequency = if (!is.null(names(spec@frequency))) {
            as.list(spec@frequency)
        } else {
            spec@frequency
        },
        access = spec@access,
        options = spec@options
    )
}
# }}}

# Reconstruct a supported reanalysis source from persisted scientific intent.
# reanalysis__from_spec {{{
reanalysis__from_spec <- function(spec) {
    dataset <- as.character(spec$dataset)
    arguments <- c(
        list(
            years = as.integer(unlist(spec$years, use.names = FALSE)),
            product = as.character(spec$product),
            variables = if (is.null(spec$variables)) {
                NULL
            } else {
                as.character(unlist(spec$variables, use.names = FALSE))
            },
            frequency = if (is.null(spec$frequency)) {
                NULL
            } else {
                unlist(spec$frequency, use.names = TRUE)
            },
            access = as.character(spec$access)
        ),
        shift_stage__coalesce(spec$options, list())
    )
    if (identical(dataset, "era5")) {
        return(do.call(shift_era5, arguments))
    }
    if (identical(dataset, "era6")) {
        return(do.call(reanalysis__era6, arguments))
    }
    cli::cli_abort(
        "Unsupported persisted reanalysis dataset: {.val {dataset}}."
    )
}
# }}}

# Present source intent without printing provider credentials or endpoint
# overrides that could contain user-specific information.
# S7::method(print, ShiftReanalysisSpec) {{{
S7::method(print, ShiftReanalysisSpec) <- function(x, ...) {
    esg__print_header("Reanalysis Source")
    esg__print_facts(list(
        "Dataset" = toupper(x@dataset),
        "Product" = x@product,
        "Years" = sprintf("%d-%d", min(x@years), max(x@years)),
        "Variables" = if (is.null(x@variables)) {
            "inferred from weather method"
        } else {
            paste(x@variables, collapse = ", ")
        },
        "Frequency" = if (is.null(x@frequency)) {
            "inferred from weather method"
        } else {
            paste(x@frequency, collapse = ", ")
        },
        "Access" = x@access
    ))
    invisible(x)
}
# }}}

# ERA5 source
# Return the canonical ERA5 variable manifest. Long CDS names define requests,
# short aliases cover both CDS and time-series NetCDF encodings, and product
# flags prevent an ERA5-Land request from inheriting unsupported ERA5 fields.
# era5__variable_manifest {{{
era5__variable_manifest <- function() {
    manifest <- data.table::data.table(
        variable_id = c(
            "tas",
            "tasmin",
            "tasmax",
            "tdps",
            "ps",
            "psl",
            "uas",
            "vas",
            "sfcWind",
            "rsds",
            "rlds",
            "clt",
            "pr",
            "hurs",
            "huss",
            "snd"
        ),
        era_variable = c(
            "2m_temperature",
            "2m_temperature",
            "2m_temperature",
            "2m_dewpoint_temperature",
            "surface_pressure",
            "mean_sea_level_pressure",
            "10m_u_component_of_wind",
            "10m_v_component_of_wind",
            "10m_u_component_of_wind",
            "surface_solar_radiation_downwards",
            "surface_thermal_radiation_downwards",
            "total_cloud_cover",
            "total_precipitation",
            "2m_dewpoint_temperature",
            "2m_dewpoint_temperature",
            "snow_depth"
        ),
        aliases = I(list(
            c("t2m", "2m_temperature"),
            c("t2m", "2m_temperature"),
            c("t2m", "2m_temperature"),
            c("d2m", "2m_dewpoint_temperature"),
            c("sp", "surface_pressure"),
            c("msl", "mean_sea_level_pressure"),
            c("u10", "10m_u_component_of_wind"),
            c("v10", "10m_v_component_of_wind"),
            c("u10", "10m_u_component_of_wind"),
            c("ssrd", "surface_solar_radiation_downwards"),
            c("strd", "surface_thermal_radiation_downwards"),
            c("tcc", "total_cloud_cover"),
            c("tp", "total_precipitation"),
            c("d2m", "2m_dewpoint_temperature"),
            c("d2m", "2m_dewpoint_temperature"),
            c("sd", "snow_depth")
        )),
        units = c(
            "K",
            "K",
            "K",
            "K",
            "Pa",
            "Pa",
            "m s-1",
            "m s-1",
            "m s-1",
            "W m-2",
            "W m-2",
            "%",
            "kg m-2 s-1",
            "%",
            "1",
            "m"
        ),
        aggregation = c("mean", "min", "max", rep("mean", 13L)),
        single_levels = c(rep(TRUE, 15L), FALSE),
        land = c(
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            FALSE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            FALSE,
            TRUE,
            TRUE,
            TRUE,
            TRUE
        ),
        time_series_single_levels = c(
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            FALSE,
            TRUE,
            TRUE,
            TRUE,
            FALSE
        ),
        time_series_land = c(
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            FALSE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            FALSE,
            FALSE,
            TRUE,
            TRUE,
            TRUE,
            TRUE
        )
    )
    # ECMWF's current single-level ARCO parameter table includes cloud cover
    # and direct horizontal solar radiation. Keep ERA5-Land flags separate:
    # https://confluence.ecmwf.int/pages/viewpage.action?pageId=505390919
    data.table::set(
        manifest,
        i = which(manifest$variable_id == "clt"),
        j = "time_series_single_levels",
        value = TRUE
    )
    # FDIR is a direct horizontal accumulation, not direct-normal radiation.
    # Diffuse radiation still requires matched SSRD and FDIR source intervals.
    radiation <- data.table::copy(manifest[rep(
        which(manifest$variable_id == "rsds"),
        2L
    )])
    data.table::set(radiation, j = "variable_id", value = c("fdir", "rsdsdiff"))
    data.table::set(
        radiation,
        j = "era_variable",
        value = c(
            "total_sky_direct_solar_radiation_at_surface",
            NA_character_
        )
    )
    data.table::set(
        radiation,
        j = "aliases",
        value = I(list(
            c("fdir", "total_sky_direct_solar_radiation_at_surface"),
            character()
        ))
    )
    for (column in c("land", "time_series_land")) {
        data.table::set(radiation, j = column, value = FALSE)
    }
    # ECMWF's 2025-09-25 announcement adds skin temperature to both hourly
    # time-series products; it is an instantaneous surface temperature in K.
    skin <- data.table::copy(manifest[manifest$variable_id == "tas"])
    data.table::set(skin, j = "variable_id", value = "skt")
    data.table::set(skin, j = "era_variable", value = "skin_temperature")
    data.table::set(
        skin,
        j = "aliases",
        value = I(list(c("skt", "skin_temperature")))
    )
    data.table::rbindlist(list(manifest, radiation, skin))
}
# }}}

# Expand supported CF variables to the provider fields needed to calculate
# them, rejecting variables that are unavailable in the selected ERA5 product.
# era5__source_variables {{{
era5__source_variables <- function(variables, product = "single_levels") {
    checkmate::assert_choice(product, c("single_levels", "land"))
    dependencies <- list(
        tas = "tas",
        tasmin = "tas",
        tasmax = "tas",
        tdps = "tdps",
        ps = "ps",
        psl = "psl",
        uas = "uas",
        vas = "vas",
        sfcWind = c("uas", "vas"),
        rsds = "rsds",
        rsdsdiff = c("rsds", "fdir"),
        rlds = "rlds",
        clt = "clt",
        pr = "pr",
        hurs = c("tas", "tdps"),
        huss = c("tas", "tdps", "ps"),
        snd = "snd",
        skt = "skt"
    )
    manifest <- era5__variable_manifest()
    supported <- manifest[get(product) %in% TRUE, variable_id]
    unknown <- setdiff(variables, intersect(names(dependencies), supported))
    if (length(unknown)) {
        cli::cli_abort(
            "ERA5 product {.val {product}} does not provide or derive requested CF variable(s): {.val {unknown}}."
        )
    }
    unique(unlist(dependencies[variables], use.names = FALSE))
}
# }}}

# Select the official dataset entry used by each product and access mode.
# era5__dataset_id {{{
era5__dataset_id <- function(product, access) {
    checkmate::assert_choice(product, c("single_levels", "land"))
    checkmate::assert_choice(access, c("arco", "cds"))
    if (identical(product, "land")) {
        if (identical(access, "arco")) {
            "reanalysis-era5-land-timeseries"
        } else {
            "reanalysis-era5-land"
        }
    } else if (identical(access, "arco")) {
        "reanalysis-era5-single-levels-timeseries"
    } else {
        "reanalysis-era5-single-levels"
    }
}
# }}}

# Prefer the official ARCO-backed point service only when it contains every
# required source field; otherwise use the complete CDS subset service.
# era5__resolve_access {{{
era5__resolve_access <- function(spec, variables) {
    if (!identical(spec@access, "auto")) {
        return(spec@access)
    }
    sources <- era5__source_variables(variables, spec@product)
    manifest <- era5__variable_manifest()
    time_series_column <- paste0("time_series_", spec@product)
    available <- manifest[
        variable_id %in% sources,
        all(get(time_series_column))
    ]
    if (isTRUE(available)) "arco" else "cds"
}
# }}}

# Build the transport request for one ERA5 source variable. Requests include
# one UTC day of padding so conversion to the EPW fixed standard offset cannot
# lose the first or last local calendar day.
# era5__request {{{
era5__request <- function(spec, variable, site, access) {
    manifest <- era5__variable_manifest()
    row <- manifest[variable_id == variable & get(spec@product) %in% TRUE]
    if (nrow(row) != 1L) {
        cli::cli_abort(
            "ERA5 variable mapping is not unique for {.val {variable}}."
        )
    }
    start <- as.Date(sprintf("%04d-01-01", min(spec@years))) - 1L
    stop <- as.Date(sprintf("%04d-12-31", max(spec@years))) + 1L
    longitude <- reanalysis__longitude(site@lon)
    if (identical(access, "arco")) {
        return(list(
            variable = unname(row$era_variable),
            location = list(longitude = longitude, latitude = site@lat),
            date = sprintf("%s/%s", start, stop),
            data_format = "netcdf"
        ))
    }

    dates <- seq(start, stop, by = "day")
    # The full CDS collection accepts independent year/month/day dimensions,
    # so boundary-day padding necessarily includes both adjacent calendar
    # years. Normalization still retains only the requested local years.
    # A narrow enclosing area retains enough cells for deterministic nearest
    # selection even when the requested location is between grid centres.
    resolution <- if (identical(spec@product, "land")) 0.1 else 0.25
    half_cell <- resolution / 2
    request <- list(
        variable = unname(row$era_variable),
        year = sprintf("%04d", sort(unique(as.integer(format(dates, "%Y"))))),
        month = sprintf("%02d", 1:12),
        day = sprintf("%02d", 1:31),
        time = sprintf("%02d:00", 0:23),
        area = c(
            min(90, site@lat + half_cell),
            max(-180, longitude - half_cell),
            max(-90, site@lat - half_cell),
            min(180, longitude + half_cell)
        ),
        data_format = "netcdf",
        download_format = "unarchived"
    )
    if (identical(spec@product, "single_levels")) {
        request$product_type <- "reanalysis"
    }
    attr(request, "requested_years") <- sort(unique(as.integer(
        format(dates, "%Y")
    )))
    request
}
# }}}

#' Use ERA5 as an observed weather reference
#'
#' `shift_era5()` describes an ERA5 calibration source. Variables, temporal
#' frequency, and the target coordinate are normally inferred from the weather
#' method and baseline EPW when [shift_epw_future()] builds the run.
#' Constructing the source neither reads credentials nor contacts CDS. Use
#' `shift_check(source)` to validate local configuration and
#' `shift_check(source, network = TRUE)` to authenticate the configured token
#' without submitting a data request. Dataset-specific terms are checked by CDS
#' when retrieval begins and are never accepted by the package.
#'
#' `rsdsdiff` is derived from matched hourly SSRD minus FDIR accumulations
#' (direct horizontal solar radiation, not DNI). These fields and total cloud
#' cover (`clt`) are available through both single-level CDS and its ARCO-backed
#' time-series service; automatic access prefers the time-series service.
#' `skt` requests skin temperature in kelvin and is available through both
#' full CDS and the hourly point time-series service. For independent local
#' point-file validation, use [shift_read_reanalysis()].
#'
#' @param years Calendar years used as the observed reference period.
#' @param product ERA5 single-level product. `"single_levels"` uses global
#'   ERA5; `"land"` uses ERA5-Land.
#' @param variables Optional CF variable IDs. `NULL` infers them from the
#'   selected weather method.
#' @param frequency Optional `"hour"`, `"day"`, or `"mon"` frequency. A named
#'   vector can assign frequencies by CF variable. `NULL` infers the contract.
#' @param access Data access route. `"auto"` prefers the official ARCO-backed
#'   time-series service for supported point requests and otherwise uses the
#'   complete CDS subset service.
#' @param ... Named provider options retained in the source specification.
#'
#' @return A reusable `ShiftReanalysisSpec` containing no credentials.
#'
#' @references
#' [Copernicus Climate Data Store ERA5 hourly time-series data](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-timeseries)
#' and the [CDS API setup guide](https://cds.climate.copernicus.eu/en/how-to-api).
#'
#' @seealso [shift_epw_future()], [shift_check()]
#' @export
# shift_era5 {{{
shift_era5 <- function(
    years,
    product = c("single_levels", "land"),
    variables = NULL,
    frequency = NULL,
    access = c("auto", "arco", "cds"),
    ...
) {
    product <- match.arg(product)
    access <- match.arg(access)
    options <- list(...)
    unknown <- setdiff(names(options), c("timeout", "poll_interval"))
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown ERA5 provider option(s): {.arg {unknown}}."
        )
    }
    reanalysis__spec(
        dataset = "era5",
        years = years,
        product = product,
        variables = variables,
        frequency = frequency,
        access = access,
        options = options
    )
}
# }}}

# Placeholder for the future ERA6 source boundary. ERA6 production has started,
# but a stable public dataset endpoint is not yet available; fail explicitly so
# ERA5 is never substituted or spliced into a request labelled ERA6.
# reanalysis__era6 {{{
reanalysis__era6 <- function(
    years,
    product = "single_levels",
    variables = NULL,
    frequency = NULL,
    access = c("auto", "arco", "cds"),
    ...
) {
    checkmate::assert_choice(product, "single_levels")
    options <- list(...)
    unknown <- setdiff(names(options), c("timeout", "poll_interval"))
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown ERA6 provider option(s): {.arg {unknown}}."
        )
    }
    # Construct and validate the future provider-neutral contract before the
    # availability error so the eventual ERA6 implementation can keep this API.
    reanalysis__spec(
        dataset = "era6",
        years = years,
        product = product,
        variables = variables,
        frequency = frequency,
        access = match.arg(access),
        options = options
    )
    cli::cli_abort(
        c(
            "ERA6 is registered but is not yet available through a stable public data endpoint.",
            "i" = "Use {.fn shift_era5} until an official ERA6 dataset and variable manifest are published."
        ),
        class = "epwshiftr_reanalysis_unavailable"
    )
}
# }}}

# vim: fdm=marker :
