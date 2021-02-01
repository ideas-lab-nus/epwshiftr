# esgf_query {{{
test_that("Query ESGF", {
    options(epwshiftr.verbose = FALSE)
    # Dataset query
    expect_is(
        qd <- esgf_query(variable = "tas", source = "AWI-CM-1-1-MR", frequency = "day", limit = 1),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(qd)) {
        fq_qd <- unlist(attr(qd, "response")$responseHeader$params$fq)
        expect_true(
            all(c("type:Dataset",
              "project:\"CMIP6\"",
              "activity_id:\"ScenarioMIP\"",
              "experiment_id:\"ssp126\" || experiment_id:\"ssp245\" || experiment_id:\"ssp370\" || experiment_id:\"ssp585\"",
              "source_id:\"AWI-CM-1-1-MR\"",
              "variable_id:\"tas\"",
              "nominal_resolution:\"100km\" || nominal_resolution:\"50km\" || nominal_resolution:\"100 km\" || nominal_resolution:\"50 km\"",
              "frequency:\"day\"",
              "replica:false",
              "latest:true",
              "variant_label:\"r1i1p1f1\""
            ) %in% fq_qd)
        )
        expect_equal(names(qd),
            c(
                "dataset_id", "mip_era", "activity_drs", "institution_id", "source_id",
                "experiment_id", "member_id", "table_id", "frequency", "grid_label",
                "version", "nominal_resolution", "variable_id", "variable_long_name",
                "variable_units", "data_node", "dataset_pid"
            )
        )
    }

    # File query
    expect_is(
        qf <- esgf_query(variable = "tas", source = "AWI-CM-1-1-MR", frequency = "day", limit = 1, type = "File"),
        "data.table"
    )

    # only check when LLNL ESGF node works
    if (nrow(qf)) {
        fq_qf <- unlist(attr(qf, "response")$responseHeader$params$fq)
        expect_true(
            all(c("type:File",
              "project:\"CMIP6\"",
              "activity_id:\"ScenarioMIP\"",
              "experiment_id:\"ssp126\" || experiment_id:\"ssp245\" || experiment_id:\"ssp370\" || experiment_id:\"ssp585\"",
              "source_id:\"AWI-CM-1-1-MR\"",
              "variable_id:\"tas\"",
              "nominal_resolution:\"100km\" || nominal_resolution:\"50km\" || nominal_resolution:\"100 km\" || nominal_resolution:\"50 km\"",
              "frequency:\"day\"",
              "replica:false",
              "latest:true",
              "variant_label:\"r1i1p1f1\""
            ) %in% fq_qf)
        )
        expect_equal(names(qf),
            c(
                "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
                "source_id", "experiment_id", "member_id", "table_id", "frequency", 
                "grid_label", "version", "nominal_resolution", "variable_id",
                "variable_long_name", "variable_units", "datetime_start",
                "datetime_end", "file_size", "data_node", "file_url", "tracking_id"
            )
        )
    }

    # empty found
    expect_is(q <- esgf_query(variable = "NONSENSE"), "data.table")
    expect_equivalent(q, data.table())
})
# }}}

# build_smip_index {{{
test_that("Build CMIP6 file index database", {
    options(epwshiftr.dir = tempdir())
    expect_is(idx <- init_cmip6_index(variable = "tas", source = "AWI-CM-1-1-MR", limit = 1, save = TRUE), "data.table")
    # only check when LLNL ESGF node works
    if (nrow(idx)) {
        expect_equal(names(idx),
            c(
                "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
                "source_id", "experiment_id", "member_id", "table_id", "frequency",
                "grid_label", "version", "nominal_resolution", "variable_id",
                "variable_long_name", "variable_units", "datetime_start",
                "datetime_end", "file_size", "data_node", "file_url", "dataset_pid",
                "tracking_id"
            )
        )
        expect_true(file.exists(file.path(.data_dir(), "cmip6_index.csv")))

        expect_silent(idx1 <- load_cmip6_index())
        expect_is(idx1 <- load_cmip6_index(), "data.table")
        expect_equal(idx, idx1)
    }
})
# }}}

# get_data_dir {{{
test_that("Get package data storage directory", {
    options("epwshiftr.dir" = "a")
    expect_error(get_data_dir(), "not exists")
    options("epwshiftr.dir" = NULL)
    skip_on_cran()
    .data_dir(TRUE)
    if (.Platform$OS.type == "windows") {
        expect_equal(get_data_dir(), normalizePath(user_data_dir(appauthor = "epwshiftr")))
    } else {
        expect_equal(get_data_dir(), normalizePath(user_data_dir(appname = "epwshiftr")))
    }
})
# }}}
