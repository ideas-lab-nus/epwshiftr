test_that("cmip6_dict()", {
    expect_is(dict <- cmip6_dict(), "CMIP6Dict")
    expect_is(EPWSHIFTR_ENV$dict, "CMIP6Dict")
})

test_that("$build() and $save()", {
    skip_on_cran()

    dict <- cmip6_dict()

    expect_is(dict$build(), "CMIP6Dict")

    expect_true(file.exists(dict$save(test_path())))
})

test_that("$version()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_null(dict$version())

    expect_is(dict$load(test_path()), "CMIP6Dict")
    expect_is(dict$version(), "list")
    expect_named(dict$version(), c("cvs", "dreq"))
    expect_is(dict$version()$cvs, "numeric_version")
    expect_is(dict$version()$dreq, "numeric_version")
})

test_that("$is_empty()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_true(dict$is_empty())

    expect_is(dict$load(test_path()), "CMIP6Dict")
    expect_false(dict$is_empty())
    expect_is(dict$version()$req, "numeric_version")
})

test_that("$timestamp()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_null(dict$timestamp())

    expect_is(dict$load(test_path()), "CMIP6Dict")
    expect_is(dict$timestamp(), "list")
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

    expect_is(dict$load(test_path()), "CMIP6Dict")
    expect_is(dict$built_time(), "POSIXct")
    expect_length(dict$built_time(), 1L)
})

test_that("$get()", {
    skip_on_cran()

    dict <- cmip6_dict()

    expect_null(dict$get("activity_id"))

    expect_is(dict$load(test_path()), "CMIP6Dict")

    expect_is(dict$get("drs"), "list")
    expect_identical(
        unique(vapply(dict$get("drs"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("activity_id"), "list")
    expect_identical(
        unique(vapply(dict$get("activity_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("experiment_id"), "data.table")
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

    expect_is(dict$get("frequency"), "list")
    expect_identical(
        unique(vapply(dict$get("frequency"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("grid_label"), "list")
    expect_identical(
        unique(vapply(dict$get("grid_label"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("institution_id"), "list")
    expect_identical(
        unique(vapply(dict$get("institution_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("nominal_resolution"), "character")
    expect_identical(
        unique(vapply(dict$get("nominal_resolution"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("realm"), "list")
    expect_identical(
        unique(vapply(dict$get("realm"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("required_global_attributes"), "character")

    expect_is(dict$get("source_id"), "data.table")
    expect_identical(
        vapply(dict$get("source_id"), typeof, ""),
        c(
            source_id = "character", release_year = "integer", institution_id = "list",
            label = "character", label_extended = "character", cohort = "list",
            activity_participation = "list", model_component = "list", license_info = "list"
        )
    )

    expect_is(dict$get("source_type"), "list")
    expect_identical(
        unique(vapply(dict$get("realm"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("sub_experiment_id"), "list")
    expect_identical(
        unique(vapply(dict$get("sub_experiment_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_is(dict$get("table_id"), "character")
})

test_that("$print()", {
    dict <- cmip6_dict()
    expect_message(dict$print())
})

unlink(test_path("CMIP6DICT"))
