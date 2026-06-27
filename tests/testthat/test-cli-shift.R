test_that("shift run validates JSON config and reports usage errors", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)

    dry_run <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", config, "--dry-run"))
    expect_equal(dry_run$status, 0L)
    expect_equal(dry_run$result$status, "dry_run")
    expect_equal(dry_run$result$request$variables, "tas")

    missing_request <- tempfile(fileext = ".json")
    jsonlite::write_json(list(site = list(id = "SIN", lon = 103.98, lat = 1.37)), missing_request, auto_unbox = TRUE)
    bad_request <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", missing_request, "--dry-run"))
    expect_equal(bad_request$status, 2L)
    expect_match(bad_request$error, "request")

    unknown_field <- tempfile(fileext = ".json")
    jsonlite::write_json(list(request = list(), site = list(id = "SIN", lon = 103.98, lat = 1.37), surprise = TRUE), unknown_field, auto_unbox = TRUE)
    bad_unknown <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", unknown_field, "--dry-run"))
    expect_equal(bad_unknown$status, 2L)
    expect_match(bad_unknown$error, "surprise")

    invalid_period <- tempfile(fileext = ".json")
    cli_shift_test_config(invalid_period)
    payload <- jsonlite::read_json(invalid_period, simplifyVector = TRUE)
    payload$extract$periods <- list(`2060s` = "not-a-year")
    jsonlite::write_json(payload, invalid_period, auto_unbox = TRUE)
    bad_period <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", invalid_period, "--dry-run"))
    expect_equal(bad_period$status, 2L)
    expect_match(bad_period$error, "Invalid year")
})

test_that("shift run executes collect, extract, relaxed morph, EPW output, and store-ID inspectors", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    docs <- cli_shift_test_file_docs(basename(nc), opendap_url = nc, download_url = nc)
    calls <- cli_shift_test_mock_collect(docs)
    dir <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)

    run <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", config, "--overwrite"))
    expect_equal(run$status, 0L)
    expect_equal(run$result$status, "written")
    expect_length(run$result$query_id, 1L)
    expect_length(run$result$morph_id, 1L)
    expect_true(length(run$result$plan_id) >= 1L)
    expect_true(nrow(run$result$outputs) >= 1L)
    expect_true(all(file.exists(file.path(dir, run$result$outputs$path))))
    expect_true("File" %in% calls$types)

    status <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "status", "--query", run$result$query_id))
    expect_equal(status$status, 0L)
    expect_equal(status$result$query_id, run$result$query_id)

    diagnostics <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "diagnostics", "--plan", paste(run$result$plan_id, collapse = ",")))
    expect_equal(diagnostics$status, 0L)
    expect_named(diagnostics$result, shift_diagnostic_columns())

    outputs <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "outputs", "--morph", run$result$morph_id))
    expect_equal(outputs$status, 0L)
    expect_equal(outputs$result$morph_id, run$result$morph_id)

    plan_data <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "data", "--plan", paste(run$result$plan_id, collapse = ","), "--columns", "time,value,site_id", "--limit", "2"))
    expect_equal(plan_data$status, 0L)
    expect_equal(nrow(plan_data$result), 2L)
    expect_named(plan_data$result, c("time", "value", "site_id"))

    morph_data <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "data", "--morph", run$result$morph_id, "--columns", "case_id,period,dry_bulb_temperature", "--limit", "2"))
    expect_equal(morph_data$status, 0L)
    expect_equal(nrow(morph_data$result), 2L)
    expect_true(all(c("case_id", "period", "dry_bulb_temperature") %in% names(morph_data$result)))

    json_text <- capture.output(
        json_status <- epwshiftr_cli(c("--store", dir, "--json", "shift", "status", "--query", run$result$query_id))
    )
    expect_equal(json_status$status, 0L)
    expect_equal(jsonlite::fromJSON(paste(json_text, collapse = "\n"))$query_id, run$result$query_id)

    rendered <- capture.output(
        rendered_run <- epwshiftr_cli(c("--store", dir, "shift", "run", "--config", config, "--overwrite")),
        type = "message"
    )
    expect_equal(rendered_run$status, 0L)
    expect_true(any(grepl("Shift workflow", rendered)))
    expect_false(any(grepl("^\\$", rendered)))
})
