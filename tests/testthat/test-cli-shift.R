test_that("shift run validates JSON config and reports usage errors", {
    skip_if_not_installed("duckdb")

    dir <- tempfile("esg-store-")
    config <- tempfile(fileext = ".json")
    cli_shift_test_config(config)

    dry_run <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", config, "--dry-run"))
    expect_equal(dry_run$status, 0L)
    expect_equal(dry_run$result$status, "dry_run")
    expect_equal(dry_run$result$request$variables, "tas")

    validate <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "config", "validate", "--config", config))
    expect_equal(validate$status, 0L)
    expect_equal(validate$result$status, "valid")

    example <- tempfile(fileext = ".json")
    example_result <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "config", "example", "--output", example))
    expect_equal(example_result$status, 0L)
    expect_equal(example_result$result$status, "written")
    expect_true(file.exists(example))
    example_validate <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "config", "validate", "--config", example))
    expect_equal(example_validate$status, 0L)

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

    nested_reference <- tempfile(fileext = ".json")
    cli_shift_test_config(nested_reference)
    payload <- jsonlite::read_json(nested_reference, simplifyVector = TRUE)
    payload$morph$reference <- list(
        mode = "historical",
        periods = list(reference = 1995L),
        filters = list(table_id = "day")
    )
    jsonlite::write_json(payload, nested_reference, auto_unbox = TRUE)
    valid_nested <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", nested_reference, "--dry-run"))
    expect_equal(valid_nested$status, 0L)

    plan_reference <- tempfile(fileext = ".json")
    cli_shift_test_config(plan_reference)
    payload <- jsonlite::read_json(plan_reference, simplifyVector = TRUE)
    payload$morph$reference <- list(
        mode = "plan",
        plan = "REFERENCE_PLAN_ID",
        periods = list(reference = 1995L)
    )
    jsonlite::write_json(payload, plan_reference, auto_unbox = TRUE)
    valid_plan <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", plan_reference, "--dry-run"))
    expect_equal(valid_plan$status, 0L)

    mixed_reference <- tempfile(fileext = ".json")
    cli_shift_test_config(mixed_reference)
    payload <- jsonlite::read_json(mixed_reference, simplifyVector = TRUE)
    payload$morph$reference <- list(
        mode = "historical",
        periods = list(reference = 1995L)
    )
    payload$morph$reference_plan <- "REFERENCE_PLAN_ID"
    payload$morph$reference_periods <- list(reference = 1995L)
    jsonlite::write_json(payload, mixed_reference, auto_unbox = TRUE)
    bad_mixed <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "run", "--config", mixed_reference, "--dry-run"))
    expect_equal(bad_mixed$status, 2L)
    expect_match(bad_mixed$error, "morph.reference")
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
    expect_true(nrow(run$result$next_steps) >= 1L)

    status <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "status", "--query", run$result$query_id))
    expect_equal(status$status, 0L)
    expect_equal(status$result$query_id, run$result$query_id)

    show <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "show", "--query", run$result$query_id, "--files", "--outputs"))
    expect_equal(show$status, 0L)
    expect_named(show$result, c("summary", "queries", "plans", "links", "morphs", "outputs", "files", "diagnostics", "include_files", "include_outputs"))
    expect_equal(show$result$summary$query_count, 1L)
    expect_true(nrow(show$result$plans) >= 1L)
    expect_equal(show$result$morphs$morph_id, run$result$morph_id)

    watch <- epwshiftr_cli(c("--quiet", "--store", dir, "shift", "watch", "--morph", run$result$morph_id, "--events", "1"))
    expect_equal(watch$status, 0L)
    expect_named(watch$result, c("summary", "queries", "downloads", "plans", "morphs", "outputs", "diagnostics", "events"))
    expect_equal(watch$result$morphs$morph_id, run$result$morph_id)

    jsonl_text <- capture.output(
        jsonl_watch <- epwshiftr_cli(c("--store", dir, "--jsonl", "shift", "watch", "--morph", run$result$morph_id, "--follow", "--count", "1", "--events", "1"))
    )
    expect_equal(jsonl_watch$status, 0L)
    expect_equal(jsonlite::fromJSON(jsonl_text[[1L]])$morphs$morph_id, run$result$morph_id)

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
    expect_true(any(grepl("Next steps", rendered)))
    expect_false(any(grepl("^\\$", rendered)))

    rendered_show <- capture.output(
        rendered_show_result <- epwshiftr_cli(c("--store", dir, "shift", "show", "--morph", run$result$morph_id, "--outputs")),
        type = "message"
    )
    expect_equal(rendered_show_result$status, 0L)
    expect_true(any(grepl("Shift workflow graph", rendered_show)))
    expect_true(any(grepl("-> morph", rendered_show, fixed = TRUE)))
    expect_false(any(grepl("^\\$", rendered_show)))
})
