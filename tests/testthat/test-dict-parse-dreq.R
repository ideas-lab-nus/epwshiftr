# dict DReq parsers {{{
test_that("dict__parse_dreq() parses local request table fixtures", {
    source_root <- local_cmip6_source_store(withr::local_tempdir())
    dreq_file <- file.path(source_root, "request", "test-request", "CMIP6_day.json")

    dreq <- dict__parse_dreq(dreq_file)
    expect_s3_class(dreq, "data.table")
    expect_true(all(c("variable", "modeling_realm", "units") %in% names(dreq)))
    expect_equal(dreq$variable, "tas")
    expect_equal(dreq$modeling_realm, "atmos")
    expect_equal(attr(dreq, "metadata")$table_id, "day")
    expect_equal(attr(dreq, "metadata")$realm, "atmos")
})
# }}}
