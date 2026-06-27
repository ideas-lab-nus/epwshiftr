test_that("extract CLI plans, runs, checks coverage, and lists artifacts", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)
    setup <- cli_shift_test_store_with_query(nc)

    plan <- epwshiftr_cli(c(
        "--quiet", "--store", setup$dir,
        "extract", "plan",
        "--query", setup$query_id,
        "--site-id", "SIN",
        "--lon", "103.98",
        "--lat", "1.37",
        "--time", "2060-01-02T00:00:00Z,2060-01-03T23:59:59Z",
        "--variable", "tas",
        "--nearest", "1",
        "--filter", "source_id=EC-Earth3"
    ))
    expect_equal(plan$status, 0L)
    expect_true(length(plan$result$plan_id) >= 1L)
    expect_equal(unique(plan$result$site_id), "SIN")

    run <- epwshiftr_cli(c(
        "--quiet", "--store", setup$dir,
        "extract", "run",
        "--plan", paste(plan$result$plan_id, collapse = ","),
        "--fallback", "auto"
    ))
    expect_equal(run$status, 0L)
    expect_true(all(run$result$status == "done"))

    coverage <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "extract", "coverage", "--plan", paste(plan$result$plan_id, collapse = ",")))
    expect_equal(coverage$status, 0L)
    expect_true(all(coverage$result$complete))

    artifacts <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "extract", "artifacts", "--plan", paste(plan$result$plan_id, collapse = ",")))
    expect_equal(artifacts$status, 0L)
    expect_true(nrow(artifacts$result) >= 1L)
    expect_true(all(artifacts$result$role %in% "derived"))

    rendered <- capture.output(
        rendered_cov <- epwshiftr_cli(c("--store", setup$dir, "extract", "coverage", "--plan", paste(plan$result$plan_id, collapse = ","))),
        type = "message"
    )
    expect_equal(rendered_cov$status, 0L)
    expect_true(any(grepl("Extraction coverage", rendered)))
    expect_false(any(grepl("^\\$", rendered)))

    missing <- epwshiftr_cli(c("--quiet", "--store", setup$dir, "extract", "plan", "--query", setup$query_id))
    expect_equal(missing$status, 2L)
    expect_match(missing$error, "--site-id")
})
