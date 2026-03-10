test_that("cmip6_dict()", {
    expect_s3_class(dict <- cmip6_dict(), "Cmip6Dict")
    expect_s3_class(this$dict, "Cmip6Dict")
})

test_that("Cmip6Dict$build() and Cmip6Dict$save()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_warning(dict$save())

    expect_s3_class(dict$build(), "Cmip6Dict")
    expect_s3_class(dict$build(), "Cmip6Dict")

    expect_true(file.exists(dict$save(test_path())))
    # can create dir if not exists
    expect_true(file.exists(dict$save(file.path(tempdir(), "abc"))))
})

test_that("Cmip6Dict$load()", {
    dict <- cmip6_dict()
    expect_message(dict$load(file.path(tempdir(), "not_exists")))

    empty_dir <- file.path(tempdir(), "empty")
    if (!dir.exists(empty_dir)) dir.create(empty_dir)
    empty <- file.path(empty_dir, "CMIP6DICT")
    saveRDS(list(), empty)
    expect_error(dict$load(dirname(empty)))
})

test_that("Cmip6Dict$version()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_null(dict$version())

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    expect_type(dict$version(), "list")
    expect_named(dict$version(), c("cvs", "dreq"))
    expect_s3_class(dict$version()$cvs, "numeric_version")
    expect_s3_class(dict$version()$dreq, "numeric_version")
})

test_that("Cmip6Dict$is_empty()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_true(dict$is_empty())

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    expect_false(dict$is_empty())
    expect_s3_class(dict$version()$dreq, "numeric_version")
})

test_that("Cmip6Dict$timestamp()", {
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

test_that("Cmip6Dict$built_time()", {
    skip_on_cran()

    dict <- cmip6_dict()
    expect_null(dict$built_time())

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    expect_s3_class(dict$built_time(), "POSIXct")
    expect_length(dict$built_time(), 1L)
})

test_that("Cmip6Dict$get()", {
    skip_on_cran()

    dict <- cmip6_dict()

    expect_null(dict$get("activity_id"))

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")

    expect_s3_class(dict$get("drs"), "list")
    expect_named(
        dict$get("drs"),
        c(
            "directory_path_example", "directory_path_sub_experiment_example",
            "directory_path_template", "filename_example",
            "filename_sub_experiment_example", "filename_template"
        )
    )
    expect_identical(
        unique(vapply(dict$get("drs"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )

    expect_s3_class(dict$get("activity_id"), "list")
    expect_identical(
        unique(vapply(dict$get("activity_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_true(all(c("AerChemMIP", "CMIP", "ScenarioMIP") %in% names(dict$get("activity_id"))))

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
    expect_true(all(c("1pctCO2", "historical", "ssp585") %in% dict$get("experiment_id")$experiment_id))

    expect_s3_class(dict$get("frequency"), "list")
    expect_identical(
        unique(vapply(dict$get("frequency"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_true(all(c("day", "fx", "mon") %in% names(dict$get("frequency"))))

    expect_s3_class(dict$get("grid_label"), "list")
    expect_identical(
        unique(vapply(dict$get("grid_label"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_true(all(c("gm", "gn", "gr") %in% names(dict$get("grid_label"))))

    expect_s3_class(dict$get("institution_id"), "list")
    expect_identical(
        unique(vapply(dict$get("institution_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_true(all(c("IPSL", "MOHC", "NASA-GISS") %in% names(dict$get("institution_id"))))

    expect_s3_class(dict$get("nominal_resolution"), "character")
    expect_identical(
        unique(vapply(dict$get("nominal_resolution"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_true(all(c("1 km", "100 km", "250 km") %in% dict$get("nominal_resolution")))

    expect_s3_class(dict$get("realm"), "list")
    expect_identical(
        unique(vapply(dict$get("realm"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_named(
        dict$get("realm"),
        c("aerosol", "atmos", "atmosChem", "land", "landIce", "ocean", "ocnBgchem", "seaIce")
    )

    expect_s3_class(dict$get("required_global_attributes"), "character")
    expect_true(all(c("activity_id", "table_id", "variant_label") %in% dict$get("required_global_attributes")))

    expect_s3_class(dict$get("source_id"), "data.table")
    expect_identical(
        vapply(dict$get("source_id"), typeof, ""),
        c(
            source_id = "character", release_year = "integer", institution_id = "list",
            label = "character", label_extended = "character", cohort = "list",
            activity_participation = "list", model_component = "list", license_info = "list"
        )
    )
    expect_true(all(c("ACCESS-CM2", "ACCESS-ESM1-5", "TaiESM1") %in% dict$get("source_id")$source_id))

    expect_s3_class(dict$get("source_type"), "list")
    expect_identical(
        unique(vapply(dict$get("source_type"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_named(
        dict$get("source_type"),
        c("AER", "AGCM", "AOGCM", "BGC", "CHEM", "ISM", "LAND", "OGCM", "RAD", "SLAB")
    )

    expect_s3_class(dict$get("sub_experiment_id"), "list")
    expect_identical(
        unique(vapply(dict$get("sub_experiment_id"), typeof, "", USE.NAMES = FALSE)),
        "character"
    )
    expect_true(all(c("none", "s1960", "s2015") %in% names(dict$get("sub_experiment_id"))))

    expect_s3_class(dict$get("table_id"), "character")
    expect_true(all(c("3hr", "day", "fx") %in% dict$get("table_id")))

    expect_s3_class(dict$get("dreq"), "data.table")
    expect_named(
        dict$get("dreq"),
        c(
            "variable", "table_id", "modeling_realm", "standard_name", "long_name",
            "frequency", "units", "cell_methods", "cell_measures", "comment",
            "dimensions", "out_name", "type", "positive", "valid_min", "valid_max",
            "ok_min_mean_abs", "ok_max_mean_abs"
        )
    )
    expect_true(all(c("tas", "clt", "hfls") %in% dict$get("dreq")$variable))
})

test_that("Cmip6Dict$print()", {
    dict <- cmip6_dict()
    empty_out <- testthat::capture_messages(expect_invisible(dict$print()))
    expect_true(any(grepl("CMIP6 Dictionary", empty_out, fixed = TRUE)))
    expect_true(any(grepl("Built at: <NONE>", empty_out, fixed = TRUE)))
    expect_true(any(grepl("CV Version: <Empty>", empty_out, fixed = TRUE)))
    expect_true(any(grepl("DReq Version: <Empty>", empty_out, fixed = TRUE)))

    expect_s3_class(dict$load(test_path()), "Cmip6Dict")
    loaded_out <- testthat::capture_messages(expect_invisible(dict$print()))
    loaded_out <- gsub("^.*Built at: .+$", "Built at: [yyyy-mm-dd HH:MM:SS]", loaded_out)
    loaded_out <- gsub("^.*CV Version: .+$", "CV Version: [version]", loaded_out)
    loaded_out <- gsub("^.*DReq Version: .+$", "DReq Version: [version]", loaded_out)
    loaded_out <- gsub("\\[[0-9]+ items\\]", "[XX items]", loaded_out)
    loaded_out <- gsub(
        "^.*DReq Contents: .+$",
        "DReq Contents: XX Variables from XX Tables and XX Realms",
        loaded_out
    )
    expect_true(any(grepl("Controlled Vocabularies (CVs)", loaded_out, fixed = TRUE)))
    expect_true(any(grepl("CV Contents [13 types]:", loaded_out, fixed = TRUE)))
    expect_true(any(grepl("drs [XX items]", loaded_out, fixed = TRUE)))
    expect_true(any(grepl("source_type [XX items]", loaded_out, fixed = TRUE)))
    expect_true(any(grepl("table_id [XX items]", loaded_out, fixed = TRUE)))
    expect_true(any(grepl("DReq Contents: XX Variables from XX Tables and XX Realms", loaded_out, fixed = TRUE)))
})

unlink(test_path("CMIP6DICT"))
