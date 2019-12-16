test_that("Query ESGF", {
    expect_silent(
        l <- esgf_query(variable = "tas", source = "AWI-CM-1-1-MR", frequency = "day", limit = 1)
    )

    fq <- unlist(l$responseHeader$params$fq)
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
      ) %in% fq)
    )
})
