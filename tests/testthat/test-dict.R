test_that("cmip6_dict()", {
    expect_is(dict <- cmip6_dict(), "CMIP6Dict")
    expect_is(EPWSHIFTR_ENV$dict, "CMIP6Dict")
})

test_that("$build() and $save()", {
    dict <- cmip6_dict()

    expect_is(dict$build(), "CMIP6Dict")

    expect_true(file.exists(dict$save(test_path())))
})

test_that("$version()", {
    dict <- cmip6_dict()
    expect_null(dict$version())

    expect_is(dict$load(test_path()), "CMIP6Dict")
    expect_is(dict$version(), "list")
    expect_equal(names(dict$version()), c("cv", "req"))
    expect_is(dict$version()$cv, "numeric_version")
    expect_is(dict$version()$req, "numeric_version")
})

test_that("$is_empty()", {
    dict <- cmip6_dict()
    expect_true(dict$is_empty())

    expect_is(dict$load(test_path()), "CMIP6Dict")
    expect_false(dict$is_empty())
    expect_is(dict$version()$req, "numeric_version")
})

test_that("$timestamp()", {
    dict <- cmip6_dict()
    expect_null(dict$timestamp())

    expect_is(dict$load(test_path()), "CMIP6Dict")
    expect_is(dict$timestamp(), "list")
    expect_equal(length(dict$timestamp()), 13L)
    expect_equal(names(dict$timestamp()),
        c("cv", "activity", "experiment", "sub_experiment", "institution",
            "source", "table", "frequency", "grid_label", "realm", "source_type",
            "req_global_atts", "resolution"
        )
    )
})

test_that("$built_time()", {
    dict <- cmip6_dict()
    expect_null(dict$built_time())

    expect_is(dict$load(test_path()), "CMIP6Dict")
    expect_is(dict$built_time(), "POSIXct")
    expect_equal(length(dict$built_time()), 1L)
})

test_that("$list()", {
    dict <- cmip6_dict()
    expect_null(dict$list("activity"))

    expect_is(dict$load(test_path()), "CMIP6Dict")

    expect_is(dict$list("activity"), "data.table")
    expect_equal(
        vapply(dict$list("activity"), typeof, ""),
        c(activity_id = "character", description = "character")
    )

    expect_is(dict$list("experiment"), "data.table")
    expect_equal(
        x <- vapply(dict$list("experiment"), typeof, ""),
        c(experiment_id = "character", activity_id = "list",
          additional_allowed_model_components = "list",
          description = "character", end_year = "integer",
          experiment = "character", min_number_yrs_per_sim = "integer",
          parent_activity_id = "list", parent_experiment_id = "list",
          required_model_components = "list", start_year = "integer",
          sub_experiment_id = "list", tier = "integer"
        )
    )

    expect_is(dict$list("sub_experiment"), "data.table")
    expect_equal(
        vapply(dict$list("sub_experiment"), typeof, ""),
        c(sub_experiment_id = "character", description = "character")
    )

    expect_is(dict$list("institution"), "data.table")
    expect_equal(
        vapply(dict$list("institution"), typeof, ""),
        c(institution_id = "character", description = "character")
    )

    expect_is(dict$list("source"), "data.table")
    expect_equal(
        x <- vapply(dict$list("source"), typeof, ""),
        c(source_id = "character", activity_participation = "list",
          cohort = "character", institution_id = "list",
          label = "character", label_extended = "character",
          model_component = "list", release_year = "integer"
        )
    )

    expect_is(dict$list("table"), "character")

    expect_is(dict$list("frequency"), "data.table")
    expect_equal(
        vapply(dict$list("frequency"), typeof, ""),
        c(frequency = "character", description = "character")
    )

    expect_is(dict$list("grid_label"), "data.table")
    expect_equal(
        vapply(dict$list("grid_label"), typeof, ""),
        c(grid_label = "character", description = "character")
    )

    expect_is(dict$list("realm"), "data.table")
    expect_equal(
        vapply(dict$list("realm"), typeof, ""),
        c(realm = "character", description = "character")
    )

    expect_is(dict$list("source_type"), "data.table")
    expect_equal(
        vapply(dict$list("source_type"), typeof, ""),
        c(source_type = "character", description = "character")
    )

    expect_is(dict$list("req_global_atts"), "character")
    expect_equal(
        x <- vapply(dict$list("variable"), typeof, ""),
        c(table_id = "character", default_priority = "integer",
          long_name = "character", units = "character",
          description = "character", comment = "character",
          variable_name = "character", cf_standard_name = "character",
          cell_methods = "character", positive = "character", type = "character",
          dimensions = "character", cmor_name = "character",
          modeling_realm = "character", frequency = "character",
          cell_measures = "character", prov = "character", provnote = "character",
          rowindex = "character", uid = "character", vid = "character",
          stid = "character", structure_title = "character",
          valid_min = "character", valid_max = "character",
          ok_min_mean_abs = "character", ok_max_mean_abs = "character",
          mips_requesting = "list", mips_by_experiment = "list"
        )
    )
})


test_that("$update()", {
    dict <- cmip6_dict()

    # can work for empty dict
    expect_is(dict$update(), "CMIP6Dict")
    expect_false(dict$is_empty())

    # can skip rebuild if nothing new
    last_built <- dict$built_time()
    expect_equal(
        {
            mockthat::with_mock(
                cmip6dict_fetch_mip_table_latest_tag = function(...) priv_env(dict)$m_req_tag,
                cmip6dict_fetch_cv_sha = function(...) priv_env(dict)$m_cv_sha,
                dict$update()
            )
            dict$built_time()
        },
        last_built
    )

    # can update if new version of CV and MIP Table found
    expect_false(
        {
            dict2 <- cmip6_dict()
            dict2$load(test_path())
            priv_env(dict2, "m_req_tag") <- "01.00.32"
            priv_env(dict2, "m_cv_sha") <- priv_env(dict2)$m_cv_sha[type == "activity", sha := "a"]
            mockthat::with_mock(
                cmip6dict_fetch_mip_table_latest_tag = function(...) priv_env(dict2)$m_req_tag,
                cmip6dict_fetch_cv_sha = function(...) priv_env(dict2)$m_cv_sha,
                dict2$update()
            )
            dict2$built_time() == last_built
        }
    )
})

test_that("$print()", {
    dict <- cmip6_dict()
    expect_output(dict$print())
})

unlink(test_path("CMIP6DICT"))
