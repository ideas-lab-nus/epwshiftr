# dict fetch/source/cache {{{

test_that("source files can rebuild parsed data without network", {
    source_root <- local_cmip6_source_store(withr::local_tempdir())

    cvs <- dict__fetch_cv("test-cv", use_source = TRUE, source_dir = file.path(source_root, "vocab", "test-cv"))
    dreq <- dict__fetch_dreq("test-request", use_source = TRUE, source_dir = file.path(source_root, "request", "test-request"))

    expect_named(cvs, tolower(CV_TYPES))
    expect_s3_class(cvs$experiment_id, "data.table")
    expect_s3_class(dreq, "data.table")
    expect_true(all(c("tas", "sftlf") %in% dreq$variable))
})

test_that("offline EsgDict build can use source files but rejects source misses", {
    local_esgdict_disk_cache()
    local_cache_mode_for_test("offline")
    source_root <- local_cmip6_source_store(withr::local_tempdir())

    dict <- EsgDict$new()
    expect_s3_class(
        dict$build(cv_tag = "test-cv", request_tag = "test-request", source_dir = source_root),
        "EsgDict"
    )
    expect_true(dict$has_data())
    expect_equal(dict$options("experiment_id", activity_id = "CMIP")$value, "historical")

    empty_source <- withr::local_tempdir()
    expect_error(
        EsgDict$new()$build(cv_tag = "test-cv", request_tag = "test-request", source_dir = empty_source),
        "source miss in offline mode"
    )
})

test_that("parsed EsgDict cache can satisfy offline builds without source files", {
    local_esgdict_disk_cache()
    local_cache_mode_for_test(TRUE)
    source_root <- local_cmip6_source_store(withr::local_tempdir())

    first <- EsgDict$new()
    first$build(cv_tag = "test-cv", request_tag = "test-request", source_dir = source_root)
    expect_true(first$has_data())
    expect_equal(cache__get()$size(), 1L)

    local_cache_mode_for_test("offline")
    empty_source <- withr::local_tempdir()
    second <- EsgDict$new()
    second$build(cv_tag = "test-cv", request_tag = "test-request", source_dir = empty_source)

    expect_true(second$has_data())
    expect_equal(second$options("experiment_id", activity_id = "CMIP")$value, "historical")
})

# }}}
