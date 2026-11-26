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

    expect_error(
        esgdict_option("activity_id", project = "CMIP5"),
        "not implemented"
    )
    expect_error(
        esgdict_check(activity = "CMIP", project = "CMIP5"),
        "not implemented"
    )
})
# }}}
# EsgDict getters {{{
test_that("EsgDict getters inspect empty and built dictionaries", {
    empty <- EsgDict$new()
    expect_equal(empty$status(), "empty")
    expect_false(empty$has_data())
    expect_true(empty$is_empty())
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
    expect_error(dict$get(c("request", "activity_id")), "Must have length 1")
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
    projects <- c(
        "CMIP6PLUS",
        "INPUT4MIP",
        "OBS4REF",
        "CORDEX-CMIP6",
        "CMIP7",
        "EMD"
    )
    for (project in projects) {
        dict <- esgdict(project = project)
        expect_equal(dict$project(), project)
        expect_equal(dict$profile(), tolower(project))
    }

    local_test_cache()
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

    unchecked <- esgdict_check(
        variable_id = "tas",
        table_id = "day",
        dict = dict
    )
    expect_false(any(!is.na(unchecked$valid) & !unchecked$valid))
    expect_true(any(
        is.na(unchecked$valid) &
            unchecked$type == "not_checked" &
            unchecked$rule == "variable"
    ))
})
# }}}
# EsgDict$save() / EsgDict$load() {{{
test_that("EsgDict$save() / EsgDict$load() round-trip CV-only ESG projects", {
    local_test_cache()
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
    expect_true(schema_validate(
        SCHEMA_ESG_DICT,
        payload,
        mode = "test",
        name = path
    ))
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
    local_cache_mode("normal")
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

    local_cache_mode("off")
    off <- dict__cache(TRUE)
    expect_equal(off$mode, "off")
    expect_false(off$read)
    expect_false(off$write)
    expect_true(off$source_read)
    expect_true(off$source_write)

    local_cache_mode("offline")
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
# print.Cmip6CV() / print.Cmip6DReq() {{{
test_that("print.Cmip6CV() / print.Cmip6DReq()", {
    dict <- local_test_esgdict()
    normalize_dreq_indent <- function(x) {
        gsub(
            "(?m)^\\s+(\\* (Table id|Modeling realm|Standard name|Long name|Frequency|Units|Cell methods|Cell measures|Comment|Dimensions|Out name|Type|Positive|Valid min|Valid max|Ok min mean abs|Ok max mean abs):)",
            "\\1",
            x,
            perl = TRUE
        )
    }

    expect_snapshot(print.Cmip6CV(dict$get("activity_id")))
    expect_snapshot(
        print.Cmip6DReq(dict$get("request"), n = 1L),
        transform = normalize_dreq_indent
    )
})
# }}}
# dict__parse_cv_*() / dict__parse_dreq() {{{
test_that("dict__parse_cv_*() and dict__parse_dreq() parse local CMIP6 fixtures", {
    source_root <- local_cmip6_source_store(withr::local_tempdir())
    cv_dir <- file.path(source_root, "vocab", "test-cv")

    activity <- dict__parse_cv_activity_id(file.path(
        cv_dir,
        "CMIP6_activity_id.json"
    ))
    expect_s3_class(activity, "Cmip6CV_ActivityId")
    expect_equal(names(activity), c("CMIP", "ScenarioMIP"))
    expect_equal(activity$CMIP, "CMIP activity")
    expect_false(is.null(attr(activity, "version")))

    source <- dict__parse_cv_source_id(file.path(
        cv_dir,
        "CMIP6_source_id.json"
    ))
    expect_s3_class(source, "Cmip6CV_SourceId")
    expect_true(all(
        c("source_id", "institution_id", "release_year") %in% names(source)
    ))
    expect_equal(source$release_year, as.integer(c(2019, 2020)))

    tables <- dict__parse_cv_table_id(file.path(cv_dir, "CMIP6_table_id.json"))
    expect_s3_class(tables, "Cmip6CV_TableId")
    expect_setequal(unclass(tables), c("Amon", "day", "fx"))

    dreq <- dict__parse_dreq(file.path(
        source_root,
        "request",
        "test-request",
        "CMIP6_day.json"
    ))
    expect_s3_class(dreq, "data.table")
    expect_true(all(c("variable", "modeling_realm", "units") %in% names(dreq)))
    expect_equal(dreq$variable, "tas")
    expect_equal(dreq$modeling_realm, "atmos")
    expect_equal(attr(dreq, "metadata")$table_id, "day")
    expect_equal(attr(dreq, "metadata")$realm, "atmos")
})
# }}}
# dict__fetch_cv() / dict__fetch_dreq() {{{
test_that("dict__fetch_cv() and dict__fetch_dreq() rebuild parsed data from source files", {
    source_root <- local_cmip6_source_store(withr::local_tempdir())

    cvs <- dict__fetch_cv(
        "test-cv",
        use_source = TRUE,
        source_dir = file.path(source_root, "vocab", "test-cv")
    )
    expect_named(cvs, tolower(CV_TYPES))
    expect_s3_class(cvs$experiment_id, "data.table")

    dreq <- dict__fetch_dreq(
        "test-request",
        use_source = TRUE,
        source_dir = file.path(source_root, "request", "test-request")
    )
    expect_s3_class(dreq, "data.table")
    expect_true(all(c("tas", "sftlf") %in% dreq$variable))
})
# }}}
# EsgDict$build() offline & cache {{{
test_that("EsgDict$build() uses source files in offline mode", {
    local_test_cache()
    local_cache_mode("offline")
    source_root <- local_cmip6_source_store(withr::local_tempdir())

    dict <- EsgDict$new()
    expect_s3_class(
        dict$build(
            cv_tag = "test-cv",
            request_tag = "test-request",
            source_dir = source_root
        ),
        "EsgDict"
    )
    expect_true(dict$has_data())
    expect_equal(
        dict$options("experiment_id", activity_id = "CMIP")$value,
        "historical"
    )

    empty_source <- withr::local_tempdir()
    expect_error(
        EsgDict$new()$build(
            cv_tag = "test-cv",
            request_tag = "test-request",
            source_dir = empty_source
        ),
        "source miss in offline mode"
    )
})

test_that("EsgDict$build() uses parsed cache in offline mode", {
    local_test_cache()
    local_cache_mode("normal")
    source_root <- local_cmip6_source_store(withr::local_tempdir())

    first <- EsgDict$new()
    first$build(
        cv_tag = "test-cv",
        request_tag = "test-request",
        source_dir = source_root
    )
    expect_true(first$has_data())
    expect_equal(cache__get()$size(), 1L)

    local_cache_mode("offline")
    empty_source <- withr::local_tempdir()
    second <- EsgDict$new()
    second$build(
        cv_tag = "test-cv",
        request_tag = "test-request",
        source_dir = empty_source
    )

    expect_true(second$has_data())
    expect_equal(
        second$options("experiment_id", activity_id = "CMIP")$value,
        "historical"
    )
})
# }}}
# gh_token() {{{
test_that("gh_token() works", {
    skip_on_cran()

    pat <- Sys.getenv("GITHUB_PAT")
    Sys.setenv("GITHUB_PAT" = "")
    expect_null(gh_token())

    Sys.setenv("GITHUB_PAT" = "   ")
    expect_null(gh_token())

    Sys.setenv("GITHUB_PAT" = "abc")
    expect_null(gh_token())

    Sys.setenv("GITHUB_PAT" = pat)
    expect_null(names(gh_token(header = FALSE)))
})
# }}}

# vim: fdm=marker :
