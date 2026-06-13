epw_morpher_test_response <- function(docs) {
    list(
        responseHeader = list(
            status = 0L,
            QTime = 0L,
            params = stats::setNames(list(), character())
        ),
        response = list(
            numFound = nrow(docs),
            start = 0L,
            docs = docs,
            maxScore = 1
        ),
        facet_counts = list(
            facet_queries = stats::setNames(list(), character()),
            facet_fields = stats::setNames(list(), character()),
            facet_ranges = stats::setNames(list(), character()),
            facet_intervals = stats::setNames(list(), character()),
            facet_heatmaps = stats::setNames(list(), character())
        ),
        timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    )
}

epw_morpher_test_params <- function() {
    query_param__as_store(list(
        project = "CMIP6",
        latest = TRUE,
        distrib = TRUE,
        limit = 10L,
        type = "File",
        format = QUERY_PARAM__FORMAT_JSON
    ))
}

epw_morpher_test_result <- function(docs) {
    new_query_result(
        EsgResultFile,
        index_node = "https://example.org",
        params = epw_morpher_test_params(),
        result = epw_morpher_test_response(docs)
    )
}

epw_morpher_test_file_docs <- function(path, opendap_url, download_url, variable_id = "tas") {
    docs <- data.frame(
        id = sprintf("%s|dataset-1", path),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        instance_id = sprintf("%s.instance", path),
        master_id = sprintf("%s.master", path),
        replica = FALSE,
        tracking_id = "hdl:21.14100/local-test-2060",
        title = path,
        version = 20260101L,
        data_node = "example.org",
        activity_id = "ScenarioMIP",
        institution_id = "EC-Earth-Consortium",
        source_id = "EC-Earth3",
        experiment_id = "ssp585",
        variant_label = "r1i1p1f1",
        frequency = "day",
        table_id = "day",
        variable_id = variable_id,
        grid_label = "gr",
        check.names = FALSE
    )
    docs$url <- I(list(c(
        sprintf("%s|application/netcdf|OPENDAP", opendap_url),
        sprintf("%s|application/netcdf|HTTPServer", download_url)
    )))
    docs
}

test_that("EpwMorpher creates relaxed future EPW outputs from store extracts", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L)
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- epw_morpher_test_file_docs(
        path = basename(nc),
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(epw_morpher_test_result(docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        site_id = "SIN",
        nearest = 1L
    )
    processed <- store$extract(plan_id = plan$plan_id)
    expect_equal(processed$status, "done")

    morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        label = "singapore"
    )
    expect_true(inherits(morpher, "EpwMorpher"))
    expect_setequal(morpher$required_variables(), epw_morph_variables("recommended"))

    periods <- epw_morph_periods(`2060s` = 2060L)
    climate <- morpher$summarise_climate(plan$plan_id, periods, strict = TRUE)
    expect_equal(unique(climate$variable_id), "tas")
    expect_equal(unique(climate$units), "K")

    baseline <- morpher$summarise_baseline()
    expect_true(all(c("dry_bulb_temperature", "relative_humidity") %in% baseline$epw_field))

    blocked <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(blocked$status, "blocked")
    expect_true(any(morpher$diagnose(blocked$morph_id)$severity == "error"))
    expect_error(morpher$check(blocked$morph_id), "blocking issues")

    relaxed <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = FALSE
    )
    expect_equal(relaxed$status, "planned")
    expect_true(any(morpher$diagnose(relaxed$morph_id)$severity == "warning"))

    results <- morpher$run(relaxed$morph_id, overwrite = TRUE)
    expect_equal(nrow(results), 1L)
    expect_equal(results$row_count, 8760L)
    result_path <- store_abs_path(results$output_path, root = store$path)
    expect_true(file.exists(result_path))

    result_data <- read_test_parquet(result_path)
    expect_true(all(c("source_id", "experiment_id", "variant_label", "period") %in% names(result_data)))
    expect_equal(unique(result_data$period), "2060s")

    outputs <- morpher$write_epw(
        morph_id = relaxed$morph_id,
        dir = "outputs/future-epw",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_equal(nrow(outputs), 1L)
    output_path <- store_abs_path(outputs$path, root = store$path)
    expect_true(file.exists(output_path))
    expect_gt(file.size(output_path), 0)
    expect_true(inherits(eplusr::read_epw(output_path), "Epw"))

    expect_equal(morpher$status(relaxed$morph_id)$status, "done")
    expect_equal(nrow(morpher$outputs(relaxed$morph_id)), 1L)
})

test_that("EpwMorpher completes strict outputs with recommended variables", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("RNetCDF")

    variables <- epw_morph_variables("recommended")
    nc <- stats::setNames(
        vapply(variables, function(variable_id) {
            path <- tempfile(fileext = ".nc")
            write_local_cmip6_netcdf_fixture(path, 2060L, variable_id = variable_id)
            path
        }, character(1L)),
        variables
    )
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- data.table::rbindlist(lapply(variables, function(variable_id) {
        epw_morpher_test_file_docs(
            path = basename(nc[[variable_id]]),
            opendap_url = nc[[variable_id]],
            download_url = nc[[variable_id]],
            variable_id = variable_id
        )
    }), fill = TRUE)
    query_id <- store$add_files(epw_morpher_test_result(as.data.frame(docs)))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
        site_id = "SIN",
        variable_id = variables,
        nearest = 1L
    )
    expect_setequal(plan$variable_id, variables)

    processed <- store$extract(plan_id = plan$plan_id)
    expect_equal(nrow(processed), length(variables))
    expect_true(all(processed$status == "done"))

    morpher <- epw_morpher(
        store = store,
        epw = get_cache_epw(),
        site_id = "SIN",
        label = "singapore"
    )
    periods <- epw_morph_periods(`2060s` = 2060L)
    climate <- morpher$summarise_climate(plan$plan_id, periods, strict = TRUE)
    expect_setequal(unique(climate$variable_id), variables)

    baseline <- morpher$summarise_baseline()
    strict <- morpher$plan(
        summary_id = unique(climate$summary_id),
        baseline_id = unique(baseline$baseline_id),
        strict = TRUE
    )
    expect_equal(strict$status, "planned")
    expect_equal(nrow(morpher$diagnose(strict$morph_id)), 0L)
    expect_silent(morpher$check(strict$morph_id))

    results <- morpher$run(strict$morph_id, overwrite = TRUE)
    expect_equal(nrow(results), 1L)
    expect_equal(results$row_count, 8760L)
    result_path <- store_abs_path(results$output_path, root = store$path)
    expect_true(file.exists(result_path))

    outputs <- morpher$write_epw(
        morph_id = strict$morph_id,
        dir = "outputs/future-epw-strict",
        separate = FALSE,
        overwrite = TRUE
    )
    expect_equal(nrow(outputs), 1L)
    output_path <- store_abs_path(outputs$path, root = store$path)
    expect_true(file.exists(output_path))
    expect_gt(file.size(output_path), 0)
    expect_true(inherits(eplusr::read_epw(output_path), "Epw"))

    expect_equal(morpher$status(strict$morph_id)$status, "done")
    expect_equal(nrow(morpher$outputs(strict$morph_id)), 1L)
})
