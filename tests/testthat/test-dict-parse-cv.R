# dict__parse_cv_activity_id() {{{
test_that("dict__parse_cv_activity_id() parses local CMIP6 CV fixtures", {
    source_root <- local_cmip6_source_store(withr::local_tempdir())
    cv_dir <- file.path(source_root, "vocab", "test-cv")

    activity <- dict__parse_cv_activity_id(file.path(cv_dir, "CMIP6_activity_id.json"))
    expect_s3_class(activity, "Cmip6CV_ActivityId")
    expect_equal(names(activity), c("CMIP", "ScenarioMIP"))
    expect_equal(activity$CMIP, "CMIP activity")
    expect_false(is.null(attr(activity, "version")))
})
# }}}
# dict__parse_cv_source_id() {{{
test_that("dict__parse_cv_source_id() parses local CMIP6 CV fixtures", {
    source_root <- local_cmip6_source_store(withr::local_tempdir())
    cv_dir <- file.path(source_root, "vocab", "test-cv")

    source <- dict__parse_cv_source_id(file.path(cv_dir, "CMIP6_source_id.json"))
    expect_s3_class(source, "Cmip6CV_SourceId")
    expect_true(all(c("source_id", "institution_id", "release_year") %in% names(source)))
    expect_equal(source$release_year, as.integer(c(2019, 2020)))
})
# }}}
# dict__parse_cv_table_id() {{{
test_that("dict__parse_cv_table_id() parses local CMIP6 CV fixtures", {
    source_root <- local_cmip6_source_store(withr::local_tempdir())
    cv_dir <- file.path(source_root, "vocab", "test-cv")

    tables <- dict__parse_cv_table_id(file.path(cv_dir, "CMIP6_table_id.json"))
    expect_s3_class(tables, "Cmip6CV_TableId")
    expect_setequal(unclass(tables), c("Amon", "day", "fx"))
})
# }}}
