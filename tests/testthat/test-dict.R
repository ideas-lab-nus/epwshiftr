local_cv_timestamp <- function() {
    "Wed Jan 01 00:00:00 2025 +0000"
}

local_cv_version <- function(note, type = note) {
    modified <- sprintf("%s_CV_modified", type)
    note_name <- sprintf("%s_CV_note", type)
    stats::setNames(
        list("6.2.0", local_cv_timestamp(), local_cv_timestamp(), note),
        c("CV_collection_version", "CV_collection_modified", modified, note_name)
    )
}

local_write_cv <- function(dir, type, values, version_type = type) {
    path <- file.path(dir, sprintf("CMIP6_%s.json", type))
    payload <- list(values, version_metadata = local_cv_version(type, version_type))
    names(payload)[[1L]] <- type
    jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    path
}

local_write_dreq <- function(dir, table_id, realm, variables) {
    path <- file.path(dir, sprintf("CMIP6_%s.json", table_id))
    payload <- list(
        Header = list(
            data_specs_version = "1.0.0",
            cmor_version = "3.7.0",
            table_id = sprintf("Table %s", table_id),
            table_date = "03 Jan 2025",
            realm = realm,
            missing_value = "1e20",
            int_missing_value = "-999",
            mip_era = "CMIP6",
            Conventions = "CF-1.7 CMIP-6.2"
        ),
        variable_entry = variables
    )
    jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    path
}

local_cmip6_source_store <- function(root) {
    cv_dir <- file.path(root, "vocab", "test-cv")
    dreq_dir <- file.path(root, "request", "test-request")
    dir.create(cv_dir, recursive = TRUE)
    dir.create(dreq_dir, recursive = TRUE)

    local_write_cv(
        cv_dir,
        "DRS",
        list(
            directory_path_example = "CMIP6/CMIP",
            directory_path_sub_experiment_example = "CMIP6/CMIP/sub",
            directory_path_template = "<mip_era>/<activity_id>",
            filename_example = "tas_day_EC-Earth3_historical_r1i1p1f1_gn.nc",
            filename_sub_experiment_example = "tas_day_EC-Earth3_historical_s1960-r1i1p1f1_gn.nc",
            filename_template = "<variable_id>_<table_id>_<source_id>.nc"
        )
    )
    local_write_cv(cv_dir, "activity_id", list(CMIP = "CMIP activity", ScenarioMIP = "ScenarioMIP activity"))
    local_write_cv(
        cv_dir,
        "experiment_id",
        list(
            historical = list(
                experiment_id = "historical",
                activity_id = "CMIP",
                additional_allowed_model_components = character(),
                parent_activity_id = "CMIP",
                parent_experiment_id = "piControl",
                required_model_components = "AOGCM",
                sub_experiment_id = "none",
                description = "CMIP historical run",
                end_year = "2014",
                experiment = "all-forcing simulation",
                min_number_yrs_per_sim = "165",
                start_year = "1850",
                tier = "1"
            ),
            ssp585 = list(
                experiment_id = "ssp585",
                activity_id = "ScenarioMIP",
                additional_allowed_model_components = character(),
                parent_activity_id = "CMIP",
                parent_experiment_id = "historical",
                required_model_components = "AOGCM",
                sub_experiment_id = "none",
                description = "ScenarioMIP SSP5-8.5 run",
                end_year = "2100",
                experiment = "high-end scenario",
                min_number_yrs_per_sim = "86",
                start_year = "2015",
                tier = "1"
            )
        )
    )
    local_write_cv(cv_dir, "frequency", list(day = "daily mean", fx = "fixed field", mon = "monthly mean"))
    local_write_cv(cv_dir, "grid_label", list(gn = "native grid", gr = "regridded"))
    local_write_cv(cv_dir, "institution_id", list(`EC-Earth-Consortium` = "EC-Earth Consortium", CCCma = "CCCma"))
    local_write_cv(cv_dir, "nominal_resolution", c("100 km", "250 km"), "nominal_resolution")
    local_write_cv(cv_dir, "realm", list(atmos = "atmosphere", land = "land"))
    local_write_cv(cv_dir, "required_global_attributes", c("activity_id", "table_id", "variant_label"))
    local_write_cv(
        cv_dir,
        "source_id",
        list(
            `EC-Earth3` = list(
                source_id = "EC-Earth3",
                activity_participation = c("CMIP", "ScenarioMIP"),
                institution_id = "EC-Earth-Consortium",
                release_year = "2019",
                cohort = "Registered",
                label = "EC-Earth3",
                label_extended = "EC-Earth version 3",
                model_component = list(atmos = list(type = "AOGCM")),
                license_info = list(license = "CC BY 4.0")
            ),
            CanESM5 = list(
                source_id = "CanESM5",
                activity_participation = "CMIP",
                institution_id = "CCCma",
                release_year = "2020",
                cohort = "Registered",
                label = "CanESM5",
                label_extended = "Canadian Earth System Model 5",
                model_component = list(atmos = list(type = "AOGCM")),
                license_info = list(license = "CC BY 4.0")
            )
        )
    )
    local_write_cv(cv_dir, "source_type", list(AOGCM = "coupled model"))
    local_write_cv(cv_dir, "sub_experiment_id", list(none = "no sub experiment", s1960 = "start year 1960"))
    local_write_cv(cv_dir, "table_id", c("Amon", "day", "fx"))

    local_write_dreq(
        dreq_dir,
        "day",
        "atmos",
        list(
            tas = list(
                modeling_realm = "atmos",
                standard_name = "air_temperature",
                long_name = "Near-Surface Air Temperature",
                frequency = "day",
                units = "K",
                cell_methods = "time: mean",
                cell_measures = "area: areacella",
                comment = "",
                dimensions = "longitude latitude time",
                out_name = "tas",
                type = "real",
                positive = "",
                valid_min = "",
                valid_max = "",
                ok_min_mean_abs = "",
                ok_max_mean_abs = ""
            )
        )
    )
    local_write_dreq(
        dreq_dir,
        "Amon",
        "atmos",
        list(
            tas = list(
                modeling_realm = "atmos",
                standard_name = "air_temperature",
                long_name = "Near-Surface Air Temperature",
                frequency = "mon",
                units = "K",
                cell_methods = "time: mean",
                cell_measures = "area: areacella",
                comment = "",
                dimensions = "longitude latitude time",
                out_name = "tas",
                type = "real",
                positive = "",
                valid_min = "",
                valid_max = "",
                ok_min_mean_abs = "",
                ok_max_mean_abs = ""
            )
        )
    )
    local_write_dreq(
        dreq_dir,
        "fx",
        "land",
        list(
            sftlf = list(
                modeling_realm = "land",
                standard_name = "land_area_fraction",
                long_name = "Percentage of the grid cell occupied by land",
                frequency = "fx",
                units = "%",
                cell_methods = "area: mean",
                cell_measures = "area: areacella",
                comment = "",
                dimensions = "longitude latitude",
                out_name = "sftlf",
                type = "real",
                positive = "",
                valid_min = "0",
                valid_max = "100",
                ok_min_mean_abs = "",
                ok_max_mean_abs = ""
            )
        )
    )

    root
}

local_test_esgdict <- function() {
    source_root <- local_cmip6_source_store(withr::local_tempdir())
    spec <- dict__spec("CMIP6")
    fetched <- list(
        project = "CMIP6",
        vocab = dict__fetch_cv("test-cv", use_source = TRUE, source_dir = file.path(source_root, "vocab", "test-cv")),
        request = dict__fetch_dreq("test-request", use_source = TRUE, source_dir = file.path(source_root, "request", "test-request")),
        built_time = as.POSIXct("2025-01-04 00:00:00", tz = "UTC"),
        sources = list(
            vocab = list(repo = spec$vocab$repo, tag = "test-cv", commit = "abc123", source_dir = file.path(source_root, "vocab", "test-cv")),
            request = list(repo = spec$request$repo, tag = "test-request", commit = "def456", source_dir = file.path(source_root, "request", "test-request"))
        )
    )

    built <- dict__build(fetched)
    dict <- EsgDict$new()
    private <- priv(dict)
    private$replace(built, status = "built")
    dict
}

local_esgvoc_source_store <- function(root) {
    vocab_dir <- file.path(root, "vocab", "test-vocab")
    dir.create(file.path(vocab_dir, "activity"), recursive = TRUE)
    dir.create(file.path(vocab_dir, "experiment"), recursive = TRUE)
    dir.create(file.path(vocab_dir, "table"), recursive = TRUE)

    jsonlite::write_json(
        list(id = "CMIP", description = "CMIP activity"),
        file.path(vocab_dir, "activity", "CMIP.json"),
        auto_unbox = TRUE,
        pretty = TRUE
    )
    jsonlite::write_json(
        list(id = "historical", description = "historical experiment"),
        file.path(vocab_dir, "experiment", "historical.json"),
        auto_unbox = TRUE,
        pretty = TRUE
    )
    jsonlite::write_json(
        list(id = "day", description = "daily table"),
        file.path(vocab_dir, "table", "day.json"),
        auto_unbox = TRUE,
        pretty = TRUE
    )

    root
}

local_esgdict_disk_cache <- function(env = parent.frame()) {
    cache_dir <- tempfile("epwshiftr-esgdict-cache-")
    dir.create(cache_dir, recursive = TRUE)
    cache <- DiskCache$new(cache_dir, max_size = Inf, max_age = Inf, max_n = Inf, prune_on_init = FALSE)
    old <- cache__set(cache)

    withr::defer({
        cache__set(old)
        unlink(cache_dir, recursive = TRUE)
    }, envir = env)

    invisible(cache)
}

local_cache_mode_for_test <- function(value, env = parent.frame()) {
    old <- getOption("epwshiftr.cache")
    options(epwshiftr.cache = value)

    withr::defer({
        if (is.null(old)) {
            options(epwshiftr.cache = NULL)
        } else {
            options(epwshiftr.cache = old)
        }
    }, envir = env)
}

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

test_that("EsgDict state, getters, and print are explicit", {
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

    out <- testthat::capture_messages(expect_invisible(dict$print()))
    expect_true(any(grepl("Status", out, fixed = TRUE)))
    expect_true(any(grepl("Vocab Contents [13 types]", out, fixed = TRUE)))
})

test_that("EsgDict save/load uses typed schema-validated JSON", {
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

    loaded <- EsgDict$new()
    expect_s3_class(loaded$load(), "EsgDict")
    expect_equal(loaded$status(), "loaded")
    expect_true(loaded$has_data())
    expect_equal(loaded$sources()$vocab$tag, "test-cv")
    expect_equal(loaded$get("source_id")$release_year, as.integer(c(2019, 2020)))
    expect_s3_class(loaded$indices("activity_experiment"), "data.table")
})

test_that("EsgDict load reports missing store entry and rejects malformed JSON", {
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

test_that("esgdict_option() uses bidirectional relation indices and reports ignored constraints", {
    dict <- local_test_esgdict()

    exp <- esgdict_option("experiment", activity = "CMIP", dict = dict)
    expect_true("historical" %in% exp$value)
    expect_false("ssp585" %in% exp$value)

    activity <- esgdict_option("activity_id", experiment_id = "historical", dict = dict)
    expect_equal(activity$value, "CMIP")

    source <- esgdict_option("source_id", activity_id = "ScenarioMIP", dict = dict)
    expect_equal(source$value, "EC-Earth3")

    institution <- esgdict_option("institution_id", source_id = "EC-Earth3", dict = dict)
    expect_equal(institution$value, "EC-Earth-Consortium")

    variables <- esgdict_option("variable_id", table_id = "day", dict = dict)
    expect_equal(variables$value, "tas")

    tables <- esgdict_option("table_id", variable_id = "tas", dict = dict)
    expect_setequal(tables$value, c("day", "Amon"))

    freq <- esgdict_option("frequency", variable_id = "tas", table_id = "day", dict = dict)
    expect_equal(freq$value, "day")

    expect_warning(esgdict_option("activity_id", variable_id = "tas", dict = dict), "Ignored constraint")
    ignored <- suppressWarnings(esgdict_option("activity_id", variable_id = "tas", dict = dict))
    expect_equal(attr(ignored, "ignored_constraints"), "variable_id")

    variant <- esgdict_option("variant_label", dict = dict)
    expect_equal(variant$pattern, CMIP6DICT_VARIANT_PATTERN)

    expect_equal(dict$options("experiment_id", activity_id = "ScenarioMIP")$value, "ssp585")
})

test_that("esgdict_check() returns rich value and relationship diagnostics", {
    dict <- local_test_esgdict()

    ok <- esgdict_check(
        activity = "CMIP",
        experiment = "historical",
        table_id = "day",
        variable = "tas",
        variant = "r1i1p1f1",
        dict = dict
    )
    expect_s3_class(ok, "esgdict_check_result")
    expect_true(all(ok$valid))
    expect_true(all(c("rule", "source", "constraint_fields", "compatible_values") %in% names(ok)))

    typo <- esgdict_check(experiment = "historial", dict = dict)
    expect_false(typo[field == "experiment_id"]$valid)
    expect_true("historical" %in% typo[field == "experiment_id"]$suggestions[[1L]])

    bad_exp <- esgdict_check(activity = "ScenarioMIP", experiment = "historical", dict = dict)
    expect_true(any(!bad_exp$valid & bad_exp$type == "relationship"))
    expect_true("CMIP" %in% unlist(bad_exp[type == "relationship"]$compatible_values))

    any_ok <- esgdict_check(table_id = c("day", "fx"), variable_id = c("tas", "sftlf"), dict = dict)
    expect_true(all(any_ok$valid))

    all_pairs <- esgdict_check(
        table_id = c("day", "fx"),
        variable_id = c("tas", "sftlf"),
        dict = dict,
        relationship = "all_pairs"
    )
    expect_true(any(!all_pairs$valid & all_pairs$rule == "variable"))

    expect_error(esgdict_check(experiment = "historial", dict = dict, error = TRUE))
    expect_s3_class(dict$check(activity = "CMIP", experiment = "historical"), "esgdict_check_result")
})

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
