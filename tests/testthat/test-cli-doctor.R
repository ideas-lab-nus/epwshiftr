# epwshiftr_cli_doctor() {{{

test_that("epwshiftr_cli_doctor() reports missing and valid stores", {
    skip_if_not_installed("duckdb")

    missing_dir <- tempfile("missing-store-")
    missing_doctor <- epwshiftr_cli(c("--quiet", "--store", missing_dir, "doctor"))
    expect_equal(missing_doctor$status, 0L)
    expect_named(missing_doctor$result, c("summary", "checks"))
    expect_true(any(missing_doctor$result$checks$check == "store_path"))
    expect_false(dir.exists(missing_dir))

    doctor_text <- capture.output(
        doctor_rendered <- epwshiftr_cli(c("--store", missing_dir, "doctor")),
        type = "message"
    )
    expect_equal(doctor_rendered$status, 0L)
    expect_true(any(grepl("epwshiftr doctor", doctor_text)))
    expect_true(any(grepl("Summary", doctor_text)))
    expect_true(any(grepl("Checks", doctor_text)))
    expect_false(any(grepl("^\\$summary", doctor_text)))
    expect_false(dir.exists(missing_dir))

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)
    store$downloader(n_workers = 0L)

    doctor <- epwshiftr_cli(c("--quiet", "--store", dir, "doctor"))
    expect_equal(doctor$status, 0L)
    expect_named(doctor$result, c("summary", "checks"))
    expect_true(all(c("store_manifest", "store_schema", "downloader_manifest", "downloader_schema", "index_node") %in% doctor$result$checks$check))
    expect_equal(doctor$result$checks$status[doctor$result$checks$check == "store_manifest"], "ok")
    expect_equal(doctor$result$checks$status[doctor$result$checks$check == "downloader_manifest"], "ok")
})

# }}}
