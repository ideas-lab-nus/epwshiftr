test_that("cmip6_dict()", {
    expect_s3_class(dict <- cmip6_dict(), "Cmip6Dict")
    expect_s3_class(this$dict, "Cmip6Dict")
})

test_that("$build() and $save()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_warning(dict$save())

    expect_s3_class(dict$build(), "Cmip6Dict")

    expect_true(file.exists(dict$save(test_path())))
    # can create dir if not exists
    expect_true(file.exists(dict$save(file.path(tempdir(), "abc"))))
})

test_that("$load()", {
    dict <- cmip6_dict()
    expect_message(dict$load(file.path(tempdir(), "not_exists")))

    empty_dir <- file.path(tempdir(), "empty")
    if (!dir.exists(empty_dir)) dir.create(empty_dir)
    empty <- file.path(empty_dir, "CMIP6DICT")
    saveRDS(list(), empty)
    expect_error(dict$load(dirname(empty)))
})

test_that("$version()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_null(dict$version())

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    expect_type(dict$version(), "list")
    expect_named(dict$version(), c("cvs", "dreq"))
    expect_s3_class(dict$version()$cvs, "numeric_version")
    expect_s3_class(dict$version()$dreq, "numeric_version")
})

test_that("$is_empty()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_true(dict$is_empty())

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    expect_false(dict$is_empty())
    expect_s3_class(dict$version()$dreq, "numeric_version")
})

test_that("$timestamp()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_null(dict$timestamp())

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    expect_type(dict$timestamp(), "list")
    expect_length(dict$timestamp(), 14L)
    expect_named(dict$timestamp(),
        c(
            "cvs", "drs", "activity_id", "experiment_id", "frequency",
            "grid_label", "institution_id", "nominal_resolution", "realm",
            "required_global_attributes", "source_id", "source_type",
            "sub_experiment_id", "table_id"
        )
    )
})

test_that("$built_time()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_null(dict$built_time())

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    expect_s3_class(dict$built_time(), "POSIXct")
    expect_length(dict$built_time(), 1L)
})

test_that("$get()", {
    skip_on_cran()

    dict <- cmip6_dict()

    expect_null(dict$get("activity_id"))

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")

    expect_s3_class(dict$get("drs"), "list")
    expect_identical(
        unique(vapply(dict$get("drs"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("drs")))

    expect_s3_class(dict$get("activity_id"), "list")
    expect_identical(
        unique(vapply(dict$get("activity_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("activity_id")))

    expect_s3_class(dict$get("experiment_id"), "data.table")
    expect_identical(
        vapply(dict$get("experiment_id"), typeof, ""),
        c(
            experiment_id = "character", experiment = "character", description = "character", 
            tier = "integer", start_year = "integer", end_year = "integer", 
            min_number_yrs_per_sim = "integer", required_model_components = "list", 
            parent_experiment_id = "list", sub_experiment_id = "list",
            activity_id = "list", parent_activity_id = "list",
            additional_allowed_model_components = "list"
        )
    )
    expect_message(print(dict$get("experiment_id")))

    expect_s3_class(dict$get("frequency"), "list")
    expect_identical(
        unique(vapply(dict$get("frequency"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("frequency")))

    expect_s3_class(dict$get("grid_label"), "list")
    expect_identical(
        unique(vapply(dict$get("grid_label"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("grid_label")))

    expect_s3_class(dict$get("institution_id"), "list")
    expect_identical(
        unique(vapply(dict$get("institution_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("institution_id")))

    expect_s3_class(dict$get("nominal_resolution"), "character")
    expect_identical(
        unique(vapply(dict$get("nominal_resolution"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("nominal_resolution")))

    expect_s3_class(dict$get("realm"), "list")
    expect_identical(
        unique(vapply(dict$get("realm"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("realm")))

    expect_s3_class(dict$get("required_global_attributes"), "character")
    expect_message(print(dict$get("required_global_attributes")))

    expect_s3_class(dict$get("source_id"), "data.table")
    expect_identical(
        vapply(dict$get("source_id"), typeof, ""),
        c(
            source_id = "character", release_year = "integer", institution_id = "list",
            label = "character", label_extended = "character", cohort = "list",
            activity_participation = "list", model_component = "list", license_info = "list"
        )
    )
    expect_message(print(dict$get("source_id")))

    expect_s3_class(dict$get("source_type"), "list")
    expect_identical(
        unique(vapply(dict$get("realm"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("source_type")))

    expect_s3_class(dict$get("sub_experiment_id"), "list")
    expect_identical(
        unique(vapply(dict$get("sub_experiment_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_message(print(dict$get("sub_experiment_id")))

    expect_s3_class(dict$get("table_id"), "character")
    expect_message(print(dict$get("table_id")))

    expect_s3_class(dict$get("dreq"), "data.table")
    expect_message(print(dict$get("dreq")))
})

test_that("$print()", {
    dict <- cmip6_dict()
    expect_message(dict$print())

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    expect_message(dict$print())
})

unlink(test_path("CMIP6DICT"))
