test_that("CDS configuration parses current files and rejects retired keys", {
    current <- tempfile()
    writeLines(c(
        "url: https://example.test/api",
        "key: current-token"
    ), current)
    withr::local_envvar(c(
        ECMWF_DATASTORES_RC_FILE = current,
        ECMWF_DATASTORES_URL = NA,
        ECMWF_DATASTORES_KEY = NA,
        CDSAPI_URL = NA,
        CDSAPI_KEY = NA,
        CDS_API_KEY = NA
    ))

    expect_identical(cds__config(), list(
        url = "https://example.test/api",
        key = "current-token"
    ))

    writeLines("key: 1234:retired", current)
    expect_error(cds__config(), class = "epwshiftr_cds_auth_error")
})

test_that("CDS HTTP failures distinguish authentication and licences", {
    expect_identical(
        cds__http_error_classes(401L, "invalid personal access token"),
        c("epwshiftr_cds_auth_error", "epwshiftr_cds_request_error")
    )
    expect_identical(
        cds__http_error_classes(
            403L,
            "Not all required licences have been accepted"
        ),
        c("epwshiftr_cds_license_error", "epwshiftr_cds_request_error")
    )
    expect_identical(
        cds__http_error_classes(503L, "service unavailable"),
        "epwshiftr_cds_request_error"
    )
})

test_that("CDS authentication check uses the profile endpoint only", {
    call <- NULL
    testthat::local_mocked_bindings(
        cds__http = function(method, url, key, body = NULL, timeout = 120) {
            call <<- list(
                method = method,
                url = url,
                key = key,
                body = body,
                timeout = timeout
            )
            list(status_code = 200L)
        },
        .package = "epwshiftr"
    )
    config <- list(url = "https://example.test/api", key = "secret")

    expect_true(cds__check_authentication(config, timeout = 30))
    expect_identical(call$method, "POST")
    expect_identical(
        call$url,
        "https://example.test/api/profiles/v1/account/verification/pat"
    )
    expect_null(call$body)
    expect_identical(call$timeout, 30)
})

test_that("CDS submission reports dataset-specific licence recovery", {
    testthat::local_mocked_bindings(
        cds__http = function(...) {
            cli::cli_abort(
                "CDS returned HTTP 403.",
                class = c(
                    "epwshiftr_cds_license_error",
                    "epwshiftr_cds_request_error"
                ),
                status_code = 403L,
                provider_message = "licence not accepted"
            )
        },
        .package = "epwshiftr"
    )
    config <- list(url = "https://example.test/api", key = "secret")
    condition <- tryCatch(
        cds__submit(
            "reanalysis-era5-single-levels-timeseries",
            list(variable = "2m_temperature"),
            config
        ),
        epwshiftr_cds_license_error = identity
    )

    expect_s3_class(condition, "epwshiftr_cds_license_error")
    expect_match(
        conditionMessage(condition),
        "reanalysis-era5-single-levels-timeseries",
        fixed = TRUE
    )
    expect_match(conditionMessage(condition), "manage-licences", fixed = TRUE)
    expect_false(grepl("secret", conditionMessage(condition), fixed = TRUE))
})

test_that("CDS job helpers follow the OGC async link contract", {
    calls <- list()
    replies <- list(
        list(
            status_code = 201L,
            body = list(
                status = "accepted",
                links = list(list(
                    rel = "monitor",
                    href = "../../jobs/job-1"
                ))
            ),
            url = "https://example.test/api/retrieve/v1/processes/data/execution"
        ),
        list(
            status_code = 200L,
            body = list(
                asset = list(value = list(
                    href = "../../files/result.nc",
                    `file:size` = 12,
                    type = "application/x-netcdf"
                ))
            ),
            url = "https://example.test/api/retrieve/v1/jobs/job-1/results"
        )
    )
    fake_http <- function(method, url, key, body = NULL, timeout = 120) {
        calls[[length(calls) + 1L]] <<- list(
            method = method,
            url = url,
            key = key,
            body = body
        )
        replies[[length(calls)]]
    }
    testthat::local_mocked_bindings(
        cds__http = fake_http,
        .package = "epwshiftr"
    )
    config <- list(url = "https://example.test/api", key = "secret")

    job <- cds__submit("data", list(variable = "2m_temperature"), config)
    expect_identical(job$request_id, "job-1")
    expect_identical(
        job$monitor_url,
        "https://example.test/api/retrieve/v1/jobs/job-1"
    )
    expect_identical(
        calls[[1L]]$url,
        "https://example.test/api/retrieve/v1/processes/data/execution"
    )
    expect_identical(
        calls[[1L]]$body,
        list(inputs = list(variable = "2m_temperature"))
    )

    successful <- c(job, list(
        links = list(list(
            rel = "results",
            href = "/api/retrieve/v1/jobs/job-1/results"
        ))
    ))
    asset <- cds__result(successful, config)
    expect_identical(
        asset$url,
        "https://example.test/api/retrieve/v1/files/result.nc"
    )
    expect_identical(asset$size, 12)
})

test_that("CDS polling recognizes success, failure, and secret redaction", {
    index <- 0L
    fake_status <- function(job, config) {
        index <<- index + 1L
        list(
            dataset_id = job$dataset_id,
            request_id = job$request_id,
            monitor_url = job$monitor_url,
            status = c("running", "successful")[[index]],
            links = list(),
            message = NULL
        )
    }
    testthat::local_mocked_bindings(
        cds__status = fake_status,
        .package = "epwshiftr"
    )
    job <- list(
        dataset_id = "data",
        request_id = "job-1",
        monitor_url = "https://example.test/jobs/job-1"
    )
    config <- list(url = "https://example.test/api", key = "private-token")

    result <- cds__wait(job, config, timeout = 5, poll_interval = 0)
    expect_identical(result$status, "successful")
    expect_identical(
        cds__redact("request private-token failed", config$key),
        "request <redacted> failed"
    )

    testthat::local_mocked_bindings(
        cds__status = function(job, config) {
            list(
                dataset_id = job$dataset_id,
                request_id = job$request_id,
                monitor_url = job$monitor_url,
                status = character(),
                links = list(),
                message = NULL
            )
        },
        .package = "epwshiftr"
    )
    expect_error(
        cds__wait(job, config, timeout = 5, poll_interval = 0),
        class = "epwshiftr_cds_response_error"
    )
})

test_that("CDS relative URLs preserve the response path context", {
    expect_identical(
        cds__absolute_url(
            "../files/value.nc",
            "https://example.test/api/jobs/1/results"
        ),
        "https://example.test/api/jobs/files/value.nc"
    )
    expect_identical(
        cds__absolute_url("/files/value.nc", "https://example.test/api"),
        "https://example.test/files/value.nc"
    )
})

test_that("CDS ZIP results expose exactly one safe NetCDF member", {
    skip_if_not(nzchar(Sys.which("zip")), "The zip utility is unavailable.")
    source_directory <- tempfile("cds-archive-source-")
    output_directory <- tempfile("cds-archive-output-")
    dir.create(source_directory)
    dir.create(output_directory)
    netcdf <- file.path(source_directory, "result.nc")
    writeBin(as.raw(1:10), netcdf)
    archive <- tempfile(fileext = ".zip")
    old_directory <- setwd(source_directory)
    on.exit(setwd(old_directory), add = TRUE)
    utils::zip(archive, "result.nc", flags = "-q")

    expect_true(cds__is_zip_file(archive))
    extracted <- cds__extract_netcdf_archive(archive, output_directory)
    expect_identical(readBin(extracted, "raw", n = 10L), as.raw(1:10))
})

test_that("CDS retrieval reuses files unless overwrite is explicit", {
    target <- tempfile(fileext = ".nc")
    writeBin(as.raw(1:3), target)
    config <- list(url = "https://example.test/api", key = "secret")

    reused <- cds__retrieve(
        "data",
        list(variable = "2m_temperature"),
        target,
        config = config
    )
    expect_true(reused$reused)

    downloaded <- 0L
    testthat::local_mocked_bindings(
        cds__submit = function(dataset_id, request, config) {
            list(dataset_id = dataset_id, request_id = "job-1",
                monitor_url = "https://example.test/jobs/job-1")
        },
        cds__wait = function(job, config, timeout, poll_interval, reporter) {
            c(job, list(status = "successful", links = list()))
        },
        cds__result = function(job, config) {
            list(url = "https://example.test/result.nc", size = 4)
        },
        cds__download = function(asset, target, config) {
            downloaded <<- downloaded + 1L
            writeBin(as.raw(1:4), target)
            target
        },
        .package = "epwshiftr"
    )
    replaced <- cds__retrieve(
        "data",
        list(variable = "2m_temperature"),
        target,
        config = config,
        overwrite = TRUE
    )

    expect_false(replaced$reused)
    expect_identical(downloaded, 1L)
    expect_identical(file.info(target)$size, 4)
})
