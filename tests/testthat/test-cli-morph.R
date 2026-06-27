test_that("morph CLI lists metadata, runs morphing, writes EPW, and reports outputs", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)
    setup <- cli_shift_test_store_with_extract(nc)

    variables <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "morph", "variables", "--recipe", "minimal"))
    expect_equal(variables$status, 0L)
    expect_true("tas" %in% variables$result$variable_id)

    backends <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "morph", "backends"))
    expect_equal(backends$status, 0L)
    expect_true("belcher" %in% backends$result$backend)

    run <- epwshiftr_cli(c(
        "--quiet", "--store", setup$dir,
        "morph", "run",
        "--plan", paste(setup$plan_id, collapse = ","),
        "--epw", get_cache_epw(),
        "--period", "2060s=2060",
        "--strict", "false",
        "--overwrite"
    ))
    expect_equal(run$status, 0L)
    expect_length(run$result$morph_id, 1L)
    expect_true(nrow(run$result$results) >= 1L)

    status <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "morph", "status", "--morph", run$result$morph_id))
    expect_equal(status$status, 0L)
    expect_equal(status$result$status, "result_done")

    store <- EsgStore$new(setup$dir)
    suppressWarnings(store$query(sprintf(
        "UPDATE epw_morph_plan SET status = 'failed', last_error = 'forced failure' WHERE morph_id = %s",
        shift_sql_string(run$result$morph_id)
    )))
    store$close()

    retry_preview <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "morph", "retry", "--morph", run$result$morph_id))
    expect_equal(retry_preview$status, 0L)
    expect_equal(retry_preview$result$status, "failed")
    expect_true(retry_preview$result$dry_run)

    retry_bad_status <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "morph", "retry", "--status", "bogus"))
    expect_equal(retry_bad_status$status, 2L)
    expect_match(retry_bad_status$error, "--status")

    retry_run <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "morph", "retry", "--morph", run$result$morph_id, "--run"))
    expect_equal(retry_run$status, 0L)
    expect_true(nrow(retry_run$result) >= 1L)

    epw <- epwshiftr_cli(c(
        "--quiet", "--store", setup$dir,
        "morph", "epw",
        "--morph", run$result$morph_id,
        "--dir", "cli-morph-epw",
        "--separate", "false",
        "--overwrite"
    ))
    expect_equal(epw$status, 0L)
    expect_true(nrow(epw$result) >= 1L)
    expect_true(all(file.exists(file.path(setup$dir, epw$result$path))))

    outputs <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "morph", "outputs", "--morph", run$result$morph_id))
    expect_equal(outputs$status, 0L)
    expect_equal(outputs$result$morph_id, run$result$morph_id)

    all_outputs <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "morph", "outputs"))
    expect_equal(all_outputs$status, 0L)
    expect_true(run$result$morph_id %in% all_outputs$result$morph_id)

    rendered <- capture.output(
        rendered_run <- epwshiftr_cli(c(
            "--store", setup$dir,
            "morph", "run",
            "--plan", paste(setup$plan_id, collapse = ","),
            "--epw", get_cache_epw(),
            "--period", "2060s=2060",
            "--strict", "false"
        )),
        type = "message"
    )
    expect_equal(rendered_run$status, 0L)
    expect_true(any(grepl("Morph run", rendered)))
    expect_false(any(grepl("^\\$", rendered)))

    jsonl_text <- capture.output(
        jsonl <- epwshiftr_cli(c("--store", setup$dir, "--jsonl", "morph", "outputs", "--morph", run$result$morph_id))
    )
    expect_equal(jsonl$status, 0L)
    expect_true(nrow(jsonlite::fromJSON(jsonl_text[[1L]])) >= 1L)
})
