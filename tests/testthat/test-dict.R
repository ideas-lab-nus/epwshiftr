# EsgDict constructor/defaults/lifecycle {{{
test_that("esgdict() and explicit default dictionary helpers", {
    old <- this$dicts
    this$dicts <- new.env(parent = emptyenv())
    withr::defer(this$dicts <- old)

    expect_s3_class(dict <- esgdict(), "EsgDict")
    expect_equal(dict$project(), "CMIP6")
    expect_equal(dict$profile(), "cmip6")
    expect_true(dict$is_empty())
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

test_that("EsgDict state and getters are explicit", {
    empty <- EsgDict$new()
    expect_equal(empty$status(), "empty")
    expect_false(empty$has_data())
    expect_error(empty$get("activity_id"), "status is `empty`")
    expect_error(empty$options("activity_id"), "status is `empty`")

    dict <- local_test_esgdict()
    expect_equal(dict$status(), "built")
    expect_true(dict$has_data())
    expect_false(dict$is_empty())
    expect_s3_class(dict$built_time(), "POSIXct")
    expect_named(dict$timestamp(), c("vocab", tolower(CV_TYPES)))
    expect_named(dict$sources(), c("vocab", "request"))
    expect_s3_class(dict$get("EXPERIMENT_ID"), "data.table")
    expect_s3_class(dict$get("experiment_id"), "data.table")
    expect_s3_class(dict$get("request"), "data.table")
    expect_s3_class(dict$indices("variable"), "data.table")
    expect_error(dict$get(c("request", "activity_id")), "Must have length 1")
    expect_error(dict$indices(c("variable", "values")), "Must have length 1")
})

test_that("EsgDict$print()", {
    dict <- local_test_esgdict()
    expect_snapshot(dict$print())
})
# }}}
# EsgDict build/cache policy {{{
test_that("CV-only ESG projects can build and report unchecked relationships", {
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

test_that("EsgDict cache policy follows package cache mode", {
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
