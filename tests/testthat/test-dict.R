# esgdict() {{{
test_that("esgdict()", {
    old <- this$dicts
    this$dicts <- new.env(parent = emptyenv())
    withr::defer(this$dicts <- old)

    expect_s3_class(dict <- esgdict(), "EsgDict")
    expect_equal(dict$project(), "CMIP6")
    expect_equal(dict$profile(), "cmip6")
    expect_true(dict$is_empty())
    other <- esgdict(project = "CMIP6PLUS")
    expect_equal(other$project(), "CMIP6PLUS")
    expect_equal(other$profile(), "cmip6plus")
})
# }}}
# esgdict_get_default() / esgdict_set_default() {{{
test_that("esgdict_get_default() / esgdict_set_default()", {
    old <- this$dicts
    this$dicts <- new.env(parent = emptyenv())
    withr::defer(this$dicts <- old)

    dict <- esgdict()
    expect_null(esgdict_get_default())

    esgdict_set_default(dict)
    expect_identical(esgdict_get_default(), dict)

    other <- esgdict(project = "CMIP6PLUS")
    expect_equal(other$project(), "CMIP6PLUS")
    expect_equal(other$profile(), "cmip6plus")
    esgdict_set_default(other)
    expect_identical(esgdict_get_default("CMIP6PLUS"), other)
    expect_identical(esgdict_get_default("CMIP6"), dict)

    expect_error(esgdict_option("activity_id", project = "CMIP5"), "not implemented")
    expect_error(esgdict_check(activity = "CMIP", project = "CMIP5"), "not implemented")
})
# }}}
# EsgDict$status() {{{
test_that("EsgDict$status()", {
    empty <- EsgDict$new()
    expect_equal(empty$status(), "empty")

    dict <- local_test_esgdict()
    expect_equal(dict$status(), "built")
})
# }}}
# EsgDict$has_data() {{{
test_that("EsgDict$has_data()", {
    empty <- EsgDict$new()
    expect_false(empty$has_data())

    dict <- local_test_esgdict()
    expect_true(dict$has_data())
})
# }}}
# EsgDict$is_empty() {{{
test_that("EsgDict$is_empty()", {
    empty <- EsgDict$new()
    expect_true(empty$is_empty())

    dict <- local_test_esgdict()
    expect_false(dict$is_empty())
})
# }}}
# EsgDict$built_time() {{{
test_that("EsgDict$built_time()", {
    dict <- local_test_esgdict()
    expect_s3_class(dict$built_time(), "POSIXct")
})
# }}}
# EsgDict$timestamp() {{{
test_that("EsgDict$timestamp()", {
    dict <- local_test_esgdict()
    expect_named(dict$timestamp(), c("vocab", tolower(CV_TYPES)))
})
# }}}
# EsgDict$sources() {{{
test_that("EsgDict$sources()", {
    dict <- local_test_esgdict()
    expect_named(dict$sources(), c("vocab", "request"))
})
# }}}
# EsgDict$get() {{{
test_that("EsgDict$get()", {
    empty <- EsgDict$new()
    expect_error(empty$get("activity_id"), "status is `empty`")

    dict <- local_test_esgdict()
    expect_s3_class(dict$get("EXPERIMENT_ID"), "data.table")
    expect_s3_class(dict$get("experiment_id"), "data.table")
    expect_s3_class(dict$get("request"), "data.table")
    expect_error(dict$get(c("request", "activity_id")), "Must have length 1")
})
# }}}
# EsgDict$options() {{{
test_that("EsgDict$options()", {
    empty <- EsgDict$new()
    expect_error(empty$options("activity_id"), "status is `empty`")
})
# }}}
# EsgDict$indices() {{{
test_that("EsgDict$indices()", {
    dict <- local_test_esgdict()
    expect_s3_class(dict$indices("variable"), "data.table")
    expect_error(dict$indices(c("variable", "values")), "Must have length 1")
})
# }}}
# EsgDict$print() {{{
test_that("EsgDict$print()", {
    dict <- local_test_esgdict()
    expect_snapshot(dict$print())
})
# }}}
# EsgDict$build() {{{
test_that("EsgDict$build() supports CV-only ESG projects", {
    projects <- c("CMIP6PLUS", "INPUT4MIP", "OBS4REF", "CORDEX-CMIP6", "CMIP7", "EMD")
    for (project in projects) {
        dict <- esgdict(project = project)
        expect_equal(dict$project(), project)
        expect_equal(dict$profile(), tolower(project))
    }

    local_esgdict_disk_cache()
    source_root <- local_esgvoc_source_store(withr::local_tempdir())
    dict <- EsgDict$new("CMIP6PLUS")
    expect_s3_class(
        dict$build(cv_tag = "test-vocab", source_dir = source_root),
        "EsgDict"
    )
    expect_true(dict$has_data())
    expect_true(dict$capabilities()$vocab)
    expect_false(dict$capabilities()$request)

    activity <- esgdict_option("activity_id", dict = dict)
    expect_equal(activity$value, "CMIP")

    ok <- esgdict_check(activity_id = "CMIP", dict = dict)
    expect_true(all(ok$valid))

    unchecked <- esgdict_check(variable_id = "tas", table_id = "day", dict = dict)
    expect_false(any(!is.na(unchecked$valid) & !unchecked$valid))
    expect_true(any(is.na(unchecked$valid) & unchecked$type == "not_checked" & unchecked$rule == "variable"))
})
# }}}
# EsgDict$save() / EsgDict$load() {{{
test_that("EsgDict$save() / EsgDict$load() round-trip CV-only ESG projects", {
    local_esgdict_disk_cache()
    source_root <- local_esgvoc_source_store(withr::local_tempdir())
    dict <- EsgDict$new("CMIP6PLUS")
    expect_s3_class(
        dict$build(cv_tag = "test-vocab", source_dir = source_root),
        "EsgDict"
    )

    dir <- withr::local_tempdir()
    path <- file.path(dir, "CMIP6PLUSDICT.json")
    expect_identical(dict$save(path = path), path)
    payload <- jsonlite::read_json(path, simplifyVector = FALSE)
    expect_true(schema_validate(SCHEMA_ESG_DICT, payload, mode = "test", name = path))
    expect_true(dict__validate(payload, name = path))
    expect_null(payload$payload$request)

    loaded <- EsgDict$new("CMIP6PLUS")
    loaded$load(path = path)
    expect_true(loaded$has_data())
    expect_equal(loaded$options("activity_id")$value, "CMIP")
})
# }}}
# dict__cache() {{{
test_that("dict__cache() follows package cache mode", {
    local_cache_mode_for_test(TRUE)
    normal <- dict__cache(TRUE)
    expect_equal(normal$mode, "normal")
    expect_true(normal$read)
    expect_true(normal$write)
    expect_true(normal$source_read)
    expect_true(normal$source_write)

    no_call_cache <- dict__cache(FALSE)
    expect_false(no_call_cache$read)
    expect_false(no_call_cache$write)
    expect_true(no_call_cache$source_read)
    expect_true(no_call_cache$source_write)

    local_cache_mode_for_test(FALSE)
    off <- dict__cache(TRUE)
    expect_equal(off$mode, "off")
    expect_false(off$read)
    expect_false(off$write)
    expect_true(off$source_read)
    expect_true(off$source_write)

    local_cache_mode_for_test("offline")
    offline <- dict__cache(TRUE)
    expect_equal(offline$mode, "offline")
    expect_true(offline$read)
    expect_false(offline$write)
    expect_true(offline$source_read)
    expect_false(offline$source_write)
    expect_true(offline$offline)
    offline_no_call_cache <- dict__cache(FALSE)
    expect_false(offline_no_call_cache$read)
    expect_false(offline_no_call_cache$write)
    expect_true(offline_no_call_cache$source_read)
    expect_false(offline_no_call_cache$source_write)
    expect_true(offline_no_call_cache$offline)
})
# }}}
