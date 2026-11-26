test_that("extract CLI plans, runs, checks coverage, and lists artifacts", {
    skip_on_cran()

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)
    setup <- cli_shift_test_store_with_query(nc)

    plan <- cli_run(
        setup$dir,
        "extract",
        "plan",
        "--query",
        setup$query_id,
        "--site-id",
        "SIN",
        "--lon",
        "103.98",
        "--lat",
        "1.37",
        "--time",
        "2060-01-02T00:00:00Z,2060-01-03T23:59:59Z",
        "--variable",
        "tas",
        "--method",
        "nearest",
        "--filter",
        "source_id=EC-Earth3"
    )
    expect_equal(plan$status, 0L)
    expect_true(length(plan$result$plan_id) >= 1L)
    expect_equal(unique(plan$result$site_id), "SIN")

    plan_ids <- paste(plan$result$plan_id, collapse = ",")
    run <- cli_run(
        setup$dir,
        "extract",
        "run",
        "--plan",
        plan_ids,
        "--fallback",
        "auto"
    )
    expect_equal(run$status, 0L)
    expect_true(all(run$result$status == "done"))
    expect_true(all(c("run_id", "step_id") %in% names(run$result)))

    coverage <- cli_run(setup$dir, "extract", "coverage", "--plan", plan_ids)
    expect_equal(coverage$status, 0L)
    expect_true(all(coverage$result$complete))

    artifacts <- cli_run(setup$dir, "extract", "artifacts", "--plan", plan_ids)
    expect_equal(artifacts$status, 0L)
    expect_true(nrow(artifacts$result) >= 1L)
    expect_true(all(artifacts$result$role %in% "derived"))

    store <- EsgStore$new(setup$dir)
    suppressWarnings(store$query(sprintf(
        "UPDATE extraction_plan SET status = 'failed', last_error = 'forced failure' WHERE plan_id = %s",
        shift_stage__sql_string(plan$result$plan_id[[1L]])
    )))
    store$close()

    first_plan <- plan$result$plan_id[[1L]]
    retry_preview <- cli_run(
        setup$dir,
        "extract",
        "retry",
        "--plan",
        first_plan
    )
    expect_equal(retry_preview$status, 0L)
    expect_equal(retry_preview$result$status, "failed")
    expect_true(retry_preview$result$dry_run)

    retry_other_status <- cli_run(
        setup$dir,
        "extract",
        "retry",
        "--plan",
        first_plan,
        "--status",
        "done"
    )
    expect_equal(retry_other_status$status, 0L)
    expect_equal(nrow(retry_other_status$result), 0L)

    retry_bad_status <- cli_run(
        setup$dir,
        "extract",
        "retry",
        "--status",
        "bogus"
    )
    expect_equal(retry_bad_status$status, 2L)
    expect_match(retry_bad_status$error, "--status")

    retry_run <- cli_run(
        setup$dir,
        "extract",
        "retry",
        "--plan",
        first_plan,
        "--run",
        "--overwrite"
    )
    expect_equal(retry_run$status, 0L)
    expect_equal(retry_run$result$status, "done")

    rendered <- capture.output(
        rendered_cov <- cli_run(
            setup$dir,
            "extract",
            "coverage",
            "--plan",
            plan_ids,
            quiet = FALSE
        ),
        type = "message"
    )
    expect_equal(rendered_cov$status, 0L)
    expect_true(any(grepl("Extraction coverage", rendered)))
    expect_false(any(grepl("^\\$", rendered)))

    missing <- cli_run(
        setup$dir,
        "extract",
        "plan",
        "--query",
        setup$query_id
    )
    expect_equal(missing$status, 2L)
    expect_match(missing$error, "--site-id")
})

# vim: fdm=marker :
