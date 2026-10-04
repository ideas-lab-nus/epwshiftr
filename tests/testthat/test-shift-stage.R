# Plan against shared local catalogs; no live ESGF request is needed.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("shift diagnostics normalize empty partial tables", {
    partial <- data.table::data.table(
        stage = character(),
        severity = character()
    )
    diagnostics <- shift_stage__diagnostics_normalize(partial)

    expect_named(diagnostics, shift_stage__diagnostic_columns())
    expect_equal(nrow(diagnostics), 0L)
})

test_that("morph case failures update only their matching public case", {
    cases <- data.table::data.table(
        source_id = "IPSL-CM6A-LR",
        experiment_id = c("ssp126", "ssp585"),
        variant_label = "r1i1p1f1",
        grid_label = "gr",
        period = "2050",
        status = "ready",
        missing_reason = NA_character_
    )
    morph_cases <- data.table::copy(cases)[, `:=`(
        status = c("failed", "completed"),
        last_error = c("bounded target failed", NA_character_)
    )]

    updated <- shift_run__apply_morph_case_status(cases, morph_cases)

    expect_identical(updated$status, c("failed", "ready"))
    expect_identical(
        updated$missing_reason,
        c("bounded target failed", NA_character_)
    )
})

test_that("shift_extract() fallback policy is available from collected files", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    docs <- esgf_test__file_docs(
        basename(nc),
        download_url = sprintf("https://example.org/%s", basename(nc)),
        include_opendap = FALSE
    )
    docs$size <- file.info(nc)$size
    docs$checksum <- NA_character_
    docs$checksum_type <- NA_character_

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = "tas",
        frequency = "day"
    )
    site <- shift_site(
        "SIN",
        lon = 103.98,
        lat = 1.37,
        label = "singapore",
        epw = get_cache_epw()
    )
    files <- shift_collect(req, store = tempfile("shift-store-"))
    periods <- epw_morph_periods(`2060s` = 2060L)
    time <- c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")

    remote_only <- shift_extract(
        files,
        site = site,
        periods = periods,
        time = time,
        fallback = "error"
    )
    expect_equal(shift_status(remote_only), "blocked")
    expect_true(any(shift_coverage(remote_only)$status %in% "failed"))
    expect_match(
        paste(shift_diagnostics(remote_only)$message, collapse = "\n"),
        "OPeNDAP is not available"
    )

    queued <- shift_download(files, run = FALSE, probe = FALSE)
    task <- data.table::as.data.table(queued)[1L]
    target <- task$target_path[[1L]]
    if (!shift_test__is_absolute_path(target)) {
        target <- file.path(shift_store(files)$path, target)
    }
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.copy(nc, target, overwrite = TRUE))

    local_fallback <- shift_extract(
        files,
        site = site,
        periods = periods,
        time = time,
        fallback = "auto"
    )
    expect_equal(shift_status(local_fallback), "extracted")
    expect_true(all(shift_coverage(local_fallback)$complete))
})

# vim: fdm=marker :
