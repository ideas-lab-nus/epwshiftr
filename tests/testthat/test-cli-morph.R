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
    expect_true("belcher_absolute" %in% backends$result$backend)

    run <- epwshiftr_cli(c(
        "--quiet", "--store", setup$dir,
        "morph", "run",
        "--plan", paste(setup$plan_id, collapse = ","),
        "--epw", get_cache_epw(),
        "--recipe", "belcher_absolute",
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
            "--recipe", "belcher_absolute",
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
