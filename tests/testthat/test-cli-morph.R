test_that("morph CLI expands variable settings and JSON vector values", {
    options <- cli_morph__transform_options(list(
        tas.grid_points = cli_morph__option_value("128"),
        tas.bounds = cli_morph__option_value("[-40,60]")
    ))

    expect_identical(options$tas$grid_points, 128L)
    expect_equal(options$tas$bounds, c(-40, 60))
    transform <- do.call(
        hourly_transform,
        c(list(method = "kernel_qdm"), options)
    )
    expect_identical(
        transform@options$signal_overrides$tas$grid_points,
        128L
    )

    variables <- epwshiftr_cli_morph_variables(c(
        "--scale",
        "monthly",
        "--method",
        "epwshiftr",
        "--option",
        "humidity_source=huss"
    ))
    expect_true(all(c("huss", "ps") %in% variables$variable_id))
    expect_false("hurs" %in% variables$variable_id)
})

test_that("morph CLI lists metadata, runs morphing, writes EPW, and reports outputs", {
    skip_on_cran()

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)
    setup <- cli_shift_test_store_with_extract(nc)
    plan_ids <- paste(setup$plan_id, collapse = ",")

    variables <- cli_run(
        setup$dir,
        "morph",
        "variables",
        "--scale",
        "daily",
        "--method",
        "epwshiftr"
    )
    expect_equal(variables$status, 0L)
    expect_true("tas" %in% variables$result$variable_id)

    transforms <- cli_run(setup$dir, "morph", "transforms")
    expect_equal(transforms$status, 0L)
    expect_true("original_morphing" %in% transforms$result$method)
    expect_true("epwshiftr" %in% transforms$result$method)
    expect_true("reconstruction_label" %in% names(transforms$result))
    expect_identical(
        transforms$result[method == "original_morphing", reconstruction_label],
        "Original morphing field equations"
    )

    base_run_args <- c(
        "morph",
        "run",
        "--plan",
        plan_ids,
        "--epw",
        get_cache_epw(),
        "--scale",
        "daily",
        "--method",
        "epwshiftr",
        "--period",
        "2060s=2060",
        "--reference",
        "plan",
        "--reference-plan",
        plan_ids,
        "--reference-period",
        "reference=2060",
        "--strict",
        "false"
    )
    run <- cli_run(
        setup$dir,
        base_run_args,
        "--option",
        "window_days=31",
        "--overwrite"
    )
    expect_equal(run$status, 0L, info = run$error)
    morph_id <- run$result$morph_id
    expect_length(morph_id, 1L)
    expect_length(run$result$run_id, 1L)
    expect_length(run$result$step_id, 1L)
    expect_true(nrow(run$result$results) >= 1L)

    status <- cli_run(setup$dir, "morph", "status", "--morph", morph_id)
    expect_equal(status$status, 0L)
    expect_equal(status$result$status, "result_done")

    store <- EsgStore$new(setup$dir)
    persisted <- shift_inspect__morph_plan(store, morph_id)
    persisted_recipe <- cli_shift__recipe_from_json(
        persisted$recipe_json[[1L]]
    )
    expect_identical(
        persisted_recipe$recipe_spec,
        "epwshiftr_daily_power"
    )
    expect_identical(persisted_recipe$options$window_days, 31L)
    suppressWarnings(store$query(sprintf(
        "UPDATE epw_morph_plan SET status = 'failed', last_error = 'forced failure' WHERE morph_id = %s",
        shift_stage__sql_string(morph_id)
    )))
    store$close()

    retry_preview <- cli_run(setup$dir, "morph", "retry", "--morph", morph_id)
    expect_equal(retry_preview$status, 0L)
    expect_equal(retry_preview$result$status, "failed")
    expect_true(retry_preview$result$dry_run)

    retry_other_status <- cli_run(
        setup$dir,
        "morph",
        "retry",
        "--morph",
        morph_id,
        "--status",
        "result_done"
    )
    expect_equal(retry_other_status$status, 0L)
    expect_equal(nrow(retry_other_status$result), 0L)

    retry_bad_status <- cli_run(
        setup$dir,
        "morph",
        "retry",
        "--status",
        "bogus"
    )
    expect_equal(retry_bad_status$status, 2L)
    expect_match(retry_bad_status$error, "--status")

    retry_run <- cli_run(
        setup$dir,
        "morph",
        "retry",
        "--morph",
        morph_id,
        "--run"
    )
    expect_equal(retry_run$status, 0L)
    expect_true(nrow(retry_run$result) >= 1L)

    epw <- cli_run(
        setup$dir,
        "morph",
        "epw",
        "--morph",
        morph_id,
        "--dir",
        "cli-morph-epw",
        "--separate",
        "false",
        "--overwrite"
    )
    expect_equal(epw$status, 0L)
    expect_true(nrow(epw$result) >= 1L)
    expect_true(all(c("run_id", "step_id") %in% names(epw$result)))
    expect_true(all(file.exists(file.path(setup$dir, epw$result$path))))

    outputs <- cli_run(setup$dir, "morph", "outputs", "--morph", morph_id)
    expect_equal(outputs$status, 0L)
    expect_equal(outputs$result$morph_id, morph_id)

    all_outputs <- cli_run(setup$dir, "morph", "outputs")
    expect_equal(all_outputs$status, 0L)
    expect_true(morph_id %in% all_outputs$result$morph_id)

    rendered <- capture.output(
        rendered_run <- cli_run(setup$dir, base_run_args, quiet = FALSE),
        type = "message"
    )
    expect_equal(rendered_run$status, 0L)
    expect_true(any(grepl("Morph run", rendered)))
    expect_false(any(grepl("^\\$", rendered)))

    jsonl_text <- capture.output(
        jsonl <- cli_run(
            setup$dir,
            "--jsonl",
            "morph",
            "outputs",
            "--morph",
            morph_id,
            quiet = FALSE
        )
    )
    expect_equal(jsonl$status, 0L)
    expect_true(nrow(jsonlite::fromJSON(jsonl_text[[1L]])) >= 1L)
})

# vim: fdm=marker :
