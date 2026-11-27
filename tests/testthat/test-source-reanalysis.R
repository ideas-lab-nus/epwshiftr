# Isolate configuration files so environment-only authentication is independent
# of the developer's home directory and matches a fresh CI runner.
test_that("CDS configuration accepts environment-only credentials in priority order", {
    testthat::local_mocked_bindings(
        cds__read_config_file = function(path) list(),
        .package = "epwshiftr"
    )
    withr::local_envvar(c(
        ECMWF_DATASTORES_URL = NA,
        CDSAPI_URL = NA,
        ECMWF_DATASTORES_KEY = "modern-token",
        CDSAPI_KEY = "legacy-token",
        CDS_API_KEY = "alternate-token"
    ))

    expect_identical(
        cds__config(),
        list(url = CDS__DEFAULT_URL, key = "modern-token")
    )
    Sys.unsetenv("ECMWF_DATASTORES_KEY")
    expect_identical(cds__config()$key, "legacy-token")
    Sys.unsetenv("CDSAPI_KEY")
    expect_identical(cds__config()$key, "alternate-token")
})

# Missing optional credentials must remain representable, while retrieval must
# report the package authentication error rather than a base R type error.
test_that("CDS configuration handles missing and empty credentials", {
    testthat::local_mocked_bindings(
        cds__read_config_file = function(path) list(),
        .package = "epwshiftr"
    )
    withr::local_envvar(c(
        ECMWF_DATASTORES_KEY = NA,
        CDSAPI_KEY = NA,
        CDS_API_KEY = NA
    ))

    expect_identical(cds__config(require_key = FALSE)$key, "")
    expect_error(cds__config(), class = "epwshiftr_cds_auth_error")
    Sys.setenv(ECMWF_DATASTORES_KEY = "")
    expect_identical(cds__config(require_key = FALSE)$key, "")
    expect_error(cds__config(), class = "epwshiftr_cds_auth_error")
})

test_that("CDS configuration parses current files and rejects retired keys", {
    current <- tempfile()
    writeLines(
        c(
            "url: https://example.test/api",
            "key: current-token"
        ),
        current
    )
    withr::local_envvar(c(
        ECMWF_DATASTORES_RC_FILE = current,
        ECMWF_DATASTORES_URL = NA,
        ECMWF_DATASTORES_KEY = NA,
        CDSAPI_URL = NA,
        CDSAPI_KEY = NA,
        CDS_API_KEY = NA
    ))

    expect_identical(
        cds__config(),
        list(
            url = "https://example.test/api",
            key = "current-token"
        )
    )

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
                asset = list(
                    value = list(
                        href = "../../files/result.nc",
                        `file:size` = 12,
                        type = "application/x-netcdf"
                    )
                )
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

    successful <- c(
        job,
        list(
            links = list(list(
                rel = "results",
                href = "/api/retrieve/v1/jobs/job-1/results"
            ))
        )
    )
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
            list(
                dataset_id = dataset_id,
                request_id = "job-1",
                monitor_url = "https://example.test/jobs/job-1"
            )
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

test_that("ERA source specifications are provider-neutral and credential-free", {
    source <- shift_era5(
        years = 1995:2014,
        product = "single_levels",
        timeout = 60,
        poll_interval = 0.5
    )

    expect_true(S7::S7_inherits(source, ShiftReanalysisSpec))
    expect_identical(source@dataset, "era5")
    expect_identical(source@years, 1995:2014)
    expect_identical(source@options$timeout, 60)
    expect_false(any(grepl("key|token|secret", names(source@options))))

    restored <- reanalysis__from_spec(reanalysis__spec_value(source))
    expect_identical(restored@dataset, source@dataset)
    expect_identical(restored@years, source@years)
    expect_identical(restored@options, source@options)
})

test_that("reanalysis checks keep local and remote authentication explicit", {
    source <- shift_era5(2000)
    calls <- 0L
    testthat::local_mocked_bindings(
        cds__config = function(...) {
            list(url = "https://example.test/api", key = "secret")
        },
        cds__check_authentication = function(config, timeout = 120) {
            calls <<- calls + 1L
            TRUE
        },
        .package = "epwshiftr"
    )

    expect_equal(nrow(shift_check(source)), 0L)
    expect_identical(calls, 0L)
    expect_equal(nrow(shift_check(source, network = TRUE)), 0L)
    expect_identical(calls, 1L)
})

test_that("reanalysis checks diagnose missing and invalid credentials", {
    source <- shift_era5(2000)
    testthat::local_mocked_bindings(
        cds__config = function(...) {
            cli::cli_abort(
                "A Copernicus Data Store API key is required.",
                class = "epwshiftr_cds_auth_error"
            )
        },
        .package = "epwshiftr"
    )
    missing <- shift_check(source)

    expect_identical(missing$code, "cds_auth_missing")
    expect_error(shift_check(source, strict = TRUE))

    testthat::local_mocked_bindings(
        cds__config = function(...) {
            list(url = "https://example.test/api", key = "secret")
        },
        cds__check_authentication = function(...) {
            cli::cli_abort(
                "CDS returned HTTP 401.",
                class = c(
                    "epwshiftr_cds_auth_error",
                    "epwshiftr_cds_request_error"
                )
            )
        },
        .package = "epwshiftr"
    )
    invalid <- shift_check(source, network = TRUE)

    expect_identical(invalid$code, "cds_auth_invalid")
    expect_false(grepl("secret", invalid$message, fixed = TRUE))
})

test_that("ERA6 is explicit and never falls back to ERA5", {
    expect_error(
        reanalysis__era6(2000:2001),
        class = "epwshiftr_reanalysis_unavailable"
    )
})

test_that("ERA5 access and requests reflect variable and time-zone needs", {
    site <- shift_site(
        id = "SFO",
        lon = -122.375,
        lat = 37.619,
        epw = get_cache_epw()
    )
    source <- shift_era5(1995:2014)

    expect_identical(
        era5__resolve_access(source, c("tas", "hurs")),
        "arco"
    )
    expect_identical(era5__resolve_access(source, "clt"), "arco")
    expect_identical(
        era5__resolve_access(shift_era5(2001, access = "cds"), "clt"),
        "cds"
    )

    arco <- era5__request(source, "tas", site, "arco")
    expect_identical(
        arco$date,
        "1994-12-31/2015-01-01"
    )
    expect_identical(arco$location$longitude, -122.375)

    wrapped_site <- shift_site(
        id = "SFO-360",
        lon = 237.625,
        lat = 37.619,
        epw = get_cache_epw()
    )
    wrapped <- era5__request(source, "tas", wrapped_site, "arco")
    expect_identical(wrapped$location$longitude, -122.375)

    cds <- era5__request(source, "tas", site, "cds")
    expect_identical(cds$variable, "2m_temperature")
    expect_identical(attr(cds, "requested_years"), 1994:2015)
    expect_true(all(
        c("area", "year", "month", "day", "time") %in%
            names(cds)
    ))
})

test_that("ERA5 rejects unknown provider options", {
    expect_error(
        shift_era5(2000, unsupported = TRUE),
        "Unknown ERA5 provider option"
    )
})

test_that("ERA5 variables are constrained by product semantics", {
    expect_error(
        era5__source_variables("snd"),
        "single_levels.*does not provide or derive.*snd"
    )
    expect_identical(era5__source_variables("snd", "land"), "snd")
    expect_error(
        era5__source_variables("psl", "land"),
        "land.*does not provide or derive.*psl"
    )

    land <- shift_era5(2000, product = "land")
    expect_identical(era5__resolve_access(land, "snd"), "arco")
    expect_identical(era5__resolve_access(land, "rlds"), "cds")
})

# vim: fdm=marker :
