# EsgDict$save() / dict__validate() {{{
test_that("EsgDict$save() writes typed schema-validated JSON", {
    dict <- local_test_esgdict()
    dir <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_store = dir))

    expect_error(EsgDict$new()$save(), "Cannot save an empty")
    empty_path <- file.path(dir, "empty.json")
    expect_identical(EsgDict$new()$save(path = empty_path, allow_empty = TRUE), empty_path)
    expect_true(file.exists(empty_path))

    path <- dict$save()
    expect_match(store_rel_path(path, root = dir), "^dicts/cmip6/[0-9a-f]{64}[.]json$")
    payload <- jsonlite::read_json(path, simplifyVector = FALSE)
    expect_true(schema_validate(SCHEMA_ESG_DICT, payload, mode = "test", name = path))
    expect_true(dict__validate(payload, name = path))
    expect_equal(payload$format, "epwshiftr_esg_dict")
    expect_equal(payload$format_version, "2")
    expect_equal(payload$project, "CMIP6")
    expect_equal(payload$profile, "cmip6")
    expect_true(all(vapply(payload$payload$request$columns, function(col) all(c("name", "type") %in% names(col)), logical(1L))))

    store <- EsgStore$new(dir, create = FALSE)
    withr::defer(store$close())
    artifacts <- data.table::as.data.table(ddb_read_table(priv(store)$conn, "artifact"))
    expect_equal(nrow(artifacts[kind == "dict" & project == "CMIP6" & status == "available"]), 1L)
})
# }}}
# EsgDict$load() {{{
test_that("EsgDict$load() reads stored dictionary entries", {
    dict <- local_test_esgdict()
    dir <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_store = dir))
    dict$save()

    loaded <- EsgDict$new()
    expect_s3_class(loaded$load(), "EsgDict")
    expect_equal(loaded$status(), "loaded")
    expect_true(loaded$has_data())
    expect_equal(loaded$sources()$vocab$tag, "test-cv")
    expect_equal(loaded$get("source_id")$release_year, as.integer(c(2019, 2020)))
    expect_s3_class(loaded$indices("activity_experiment"), "data.table")
})

test_that("EsgDict$load() handles missing entries and malformed JSON", {
    dir <- withr::local_tempdir()
    withr::local_options(list(epwshiftr.dir_store = dir))

    old <- EsgDict$new()
    expect_message(old$load(), "Failed to find a stored ESG Dictionary")
    expect_true(old$is_empty())

    jsonlite::write_json(
        list(
            format = "epwshiftr_esgdict",
            format_version = "2",
            built_time = NULL,
            version = NULL,
            timestamps = NULL,
            sources = NULL,
            project = "CMIP6",
            profile = "cmip6",
            payload = list(
                vocab = list(activity_id = list(kind = "list", class = c("Cmip6CV", "list"), version = NULL, values = list(CMIP = "CMIP"))),
                request = NULL,
                request_metadata = NULL,
                indices = NULL
            )
        ),
        file.path(dir, "bad.json"),
        auto_unbox = TRUE,
        null = "null"
    )

    bad <- EsgDict$new()
    expect_error(bad$load(path = file.path(dir, "bad.json")))
})
# }}}
