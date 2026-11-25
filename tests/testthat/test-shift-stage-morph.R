# Keep high-level planning tests independent of live ESGF catalogs.
test_local_dependencies(list(
    availability = test_cmip6_availability,
    shift_resolve__cmip6_period_coverage = test_cmip6_period_coverage
))

test_that("humidity fallback persists a canonical hurs extraction artifact", {
    skip_on_cran()

    inputs <- c("huss", "tas", "ps")
    variables <- c(inputs, "snd")
    paths <- stats::setNames(
        vapply(
            variables,
            function(variable) {
                path <- tempfile(fileext = ".nc")
                write_local_cmip6_netcdf_fixture(
                    path,
                    2060L,
                    variable_id = variable
                )
                path
            },
            character(1L)
        ),
        variables
    )
    on.exit(unlink(paths), add = TRUE)
    docs <- data.table::rbindlist(
        lapply(variables, function(variable) {
            row <- esgf_test__file_docs(
                basename(paths[[variable]]),
                opendap_url = paths[[variable]],
                download_url = paths[[variable]],
                variable_id = variable
            )
            row$master_id <- sprintf("humidity-%s", variable)
            row$tracking_id <- sprintf("hdl:test/humidity-%s", variable)
            row$id <- sprintf("humidity-%s|dataset", variable)
            row
        }),
        fill = TRUE
    )
    # Model the production layout where optional snow depth is a separate
    # complete identity that has no atmospheric humidity source variables.
    docs[
        variable_id == "snd",
        `:=`(
            frequency = "mon",
            table_id = "LImon"
        )
    ]
    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    calls$file_fields <- list()
    shift_test__mock_collect(docs, calls)

    request <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = variables,
        frequency = "day"
    )
    site <- shift_site("SIN", lon = 103.98, lat = 1.37, epw = get_cache_epw())
    climate <- request |>
        shift_collect(store = tempfile("shift-derived-hurs-store-")) |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            fallback = "error"
        )
    derived <- shift_climate__derive_hurs_climate(
        climate,
        epw_morph_recipe("original_morphing")
    )
    coverage <- shift_coverage(derived)
    hurs <- coverage[variable_id == "hurs"]
    snd <- coverage[variable_id == "snd"]
    data <- shift_data(derived, variables = "hurs")

    expect_equal(nrow(hurs), 1L)
    expect_true(hurs$complete[[1L]])
    expect_equal(nrow(snd), 1L)
    expect_identical(snd$table_id[[1L]], "LImon")
    expect_true(all(data$units == "%"))
    expect_true(all(is.finite(data$value)))
    expect_true(all(data$derived_from == "huss,tas,ps"))
    expect_true(all(data$value > 0 & data$value < 150))

    store <- shift_store(derived)
    artifact_id <- store$query(sprintf(
        "SELECT artifact_id FROM extraction_result WHERE plan_id = %s LIMIT 1",
        ddb_literal(priv(store)$conn, hurs$plan_id[[1L]])
    ))$artifact_id[[1L]]
    artifact <- store$query(sprintf(
        "SELECT metadata_json FROM artifact WHERE artifact_id = %s",
        ddb_literal(priv(store)$conn, artifact_id)
    ))
    expect_match(artifact$metadata_json[[1L]], "huss,tas,ps")

    reused <- shift_climate__derive_hurs_climate(
        derived,
        epw_morph_recipe("original_morphing"),
        resume = TRUE
    )
    expect_equal(shift_ids(reused)$plan_id, shift_ids(derived)$plan_id)
})

test_that("shift_morph() uses complete extraction plans by default", {
    skip_on_cran()

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L, variable_id = "tas")
    on.exit(unlink(nc), add = TRUE)

    good <- esgf_test__file_docs(
        basename(nc),
        opendap_url = nc,
        download_url = nc,
        variable_id = "tas"
    )
    bad <- esgf_test__file_docs(
        "hurs_missing_opendap.nc",
        opendap_url = "https://example.org/hurs_missing_opendap.nc",
        download_url = "https://example.org/hurs_missing_opendap.nc",
        variable_id = "hurs",
        include_opendap = FALSE
    )
    docs <- data.table::rbindlist(list(good, bad), fill = TRUE)

    calls <- new.env(parent = emptyenv())
    calls$values <- character()
    shift_test__mock_collect(docs, calls)

    req <- shift_request(
        project = "CMIP6",
        experiment = "ssp585",
        variables = c("tas", "hurs"),
        frequency = "day"
    )
    site <- shift_site(
        "SIN",
        lon = 103.98,
        lat = 1.37,
        label = "singapore",
        epw = get_cache_epw()
    )
    climate <- req |>
        shift_collect(
            store = tempfile("shift-store-"),
            label = "complete-subset"
        ) |>
        shift_extract(
            site = site,
            periods = epw_morph_periods(`2060s` = 2060L),
            time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
            fallback = "error"
        )

    coverage <- shift_coverage(climate)
    expect_true(any(coverage$complete))
    expect_true(any(!coverage$complete))

    transform <- daily_transform("epwshiftr")
    morphed <- shift_morph(
        climate,
        transform = transform,
        reference = climate,
        strict = FALSE
    )
    blocked <- shift_morph(
        climate,
        transform = transform,
        reference = climate,
        strict = FALSE,
        complete_only = FALSE,
        overwrite = TRUE
    )

    expect_equal(
        shift_ids(morphed)$plan_id,
        coverage$plan_id[coverage$complete]
    )
    expect_true(any(
        shift_diagnostics(morphed)$code %in% "ignored_incomplete_extraction"
    ))
    expect_equal(shift_status(morphed), "morphed")
    expect_equal(shift_status(blocked), "blocked")
    expect_equal(shift_status(shift_complete(morphed)), "partial")
})

# vim: fdm=marker :
