test_that("Query ESGF", {
    # Dataset query
    expect_silent(
        qd <- esgf_query(variable = "tas", source = "AWI-CM-1-1-MR", frequency = "day", limit = 1)
    )
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
            "experiment_id", "member_id", "table_id", "grid_label",
            "version", "nominal_resolution", "variable_id", "variable_long_name",
            "variable_units", "data_node"
        )
    )

    # File query
    expect_silent(
        qf <- esgf_query(variable = "tas", source = "AWI-CM-1-1-MR", frequency = "day", limit = 1, type = "File")
    )

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
            "source_id", "experiment_id", "member_id", "table_id", "grid_label",
            "version", "nominal_resolution", "variable_id", "variable_long_name",
            "variable_units", "datetime_start", "datetime_end", "file_size",
            "data_node", "url"
        )
    )

    # empty found
    expect_message(q <- esgf_query(variable = "NONSENSE"), "No matched data")
    expect_equivalent(q, data.table())
})
