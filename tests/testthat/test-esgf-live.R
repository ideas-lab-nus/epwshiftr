test_that("live ESGF query smoke covers Dataset and File contracts", {
    skip_live_esgf()

    q <- esg_query(INDEX_NODES[["DKRZ"]])$
        source_id("CMCC-CM2-SR5")$
        experiment_id("dcppA-hindcast")$
        variable_id("sftgif")$
        frequency("fx")$
        nominal_resolution("100 km")$
        params(institution_id = "CMCC", sub_experiment_id = "s2017")$
        limit(1L)

    datasets <- tryCatch(
        allow_live_esgf_dict_warnings(q$collect()),
        error = function(e) skip(sprintf("Live ESGF Dataset query failed: %s", conditionMessage(e)))
    )
    if (!datasets$count()) {
        skip("Live ESGF Dataset query returned no records.")
    }
    expect_s3_class(datasets, "EsgResultDataset")
    expect_true(all(c("id", "source_id", "experiment_id", "variable_id", "data_node") %in% names(datasets)))

    files <- tryCatch(
        datasets$collect(type = "File", limit = 1L),
        error = function(e) skip(sprintf("Live ESGF File query failed: %s", conditionMessage(e)))
    )
    if (!files$count()) {
        skip("Live ESGF File query returned no records.")
    }
    expect_s3_class(files, "EsgResultFile")
    expect_true(all(c("id", "dataset_id", "url", "tracking_id") %in% names(files)))
})

test_that("live ESGF query smoke keeps empty result behavior stable", {
    skip_live_esgf()

    empty <- tryCatch(
        allow_live_esgf_dict_warnings(esg_query(INDEX_NODES[["DKRZ"]])$source_id("NONSENSE")$limit(1L)$collect()),
        error = function(e) skip(sprintf("Live ESGF empty query failed: %s", conditionMessage(e)))
    )
    expect_s3_class(empty, "EsgResultDataset")
    expect_identical(empty$count(), 0L)
})
