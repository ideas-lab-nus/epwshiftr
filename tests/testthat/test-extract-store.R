extract_store_test_response <- function(docs) {
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

extract_store_test_params <- function(type = "File") {
    query_param_as_store(list(
        project = "CMIP6",
        latest = TRUE,
        distrib = TRUE,
        limit = 10L,
        type = type,
        format = FORMAT_JSON
    ))
}

extract_store_test_result <- function(type = "File", docs, context = NULL) {
    generator <- switch(
        type,
        File = EsgResultFile,
        Aggregation = EsgResultAggregation
    )
    new_query_result(
        generator,
        index_node = "https://example.org",
        params = extract_store_test_params(type),
        result = extract_store_test_response(docs),
        context = context
    )
}

extract_store_test_file_docs <- function(path = "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
                                         source_id = "EC-Earth3",
                                         experiment_id = "ssp585",
                                         variable_id = "tas",
                                         opendap_url = NULL,
                                         download_url = NULL) {
    if (is.null(opendap_url)) {
        opendap_url <- sprintf("https://example.org/dods/%s.html", path)
    }
    if (is.null(download_url)) {
        download_url <- sprintf("https://example.org/fileServer/%s", path)
    }

    docs <- data.frame(
        id = sprintf("%s|dataset-1", path),
        dataset_id = "dataset-1",
        size = 123,
        checksum = "abc",
        checksum_type = "SHA256",
        tracking_id = "hdl:21.14100/local-test-2060",
        title = path,
        data_node = "example.org",
        source_id = source_id,
        experiment_id = experiment_id,
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

test_that("EsgExtractStore creates a DuckDB manifest and store layout", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    dir <- tempfile("esg-store-")
    store <- EsgExtractStore$new(dir)
    on.exit(store$close(), add = TRUE)

    expect_true(dir.exists(dir))
    expect_true(dir.exists(file.path(dir, "queries")))
    expect_true(dir.exists(file.path(dir, "downloads")))
    expect_true(dir.exists(file.path(dir, "data")))
    expect_equal(store$path, normalizePath(dir, mustWork = TRUE))
    expect_true(file.exists(store$manifest))
    expect_true(store$is_open)

    tables <- DBI::dbListTables(epwshiftr:::priv(store)$conn)
    expect_setequal(
        tables,
        c("query_run", "file_catalog", "extraction_plan", "extraction_result")
    )

    store$close()
    expect_false(store$is_open)
})

test_that("EsgExtractStore can reopen an existing store", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    dir <- tempfile("esg-store-")
    EsgExtractStore$new(dir)$close()

    store <- EsgExtractStore$new(dir, create = FALSE)
    on.exit(store$close(), add = TRUE)

    expect_true(store$is_open)
    expect_true(file.exists(file.path(dir, "manifest.duckdb")))
})

test_that("EsgExtractStore catalogs File result records", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    dir <- tempfile("esg-store-")
    store <- EsgExtractStore$new(dir)
    on.exit(store$close(), add = TRUE)

    files <- extract_store_test_result(
        docs = extract_store_test_file_docs(),
        context = list(time_filter = list(
            start = "2060-01-01T00:00:00Z",
            stop = "2060-12-31T23:59:59Z",
            method = "drs"
        ))
    )

    query_id <- store$add_files(files, label = "cmip6 test")
    conn <- epwshiftr:::priv(store)$conn
    runs <- DBI::dbReadTable(conn, "query_run")
    catalog <- DBI::dbReadTable(conn, "file_catalog")

    expect_match(query_id, "^[0-9a-f]{64}$")
    expect_equal(nrow(runs), 1L)
    expect_equal(runs$label, "cmip6 test")
    expect_equal(runs$result_type, "File")
    expect_equal(runs$time_filter_method, "drs")
    expect_true(file.exists(file.path(dir, runs$query_file)))
    expect_equal(nrow(catalog), 1L)
    expect_match(catalog$file_key, "^[0-9a-f]{64}$")
    expect_equal(catalog$query_id, query_id)
    expect_equal(catalog$source_id, "EC-Earth3")
    expect_equal(catalog$experiment_id, "ssp585")
    expect_equal(catalog$url_opendap, "https://example.org/dods/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc")
    expect_equal(catalog$url_download, "https://example.org/fileServer/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc")
})

test_that("EsgExtractStore catalogs Aggregation records and replaces duplicates", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    dir <- tempfile("esg-store-")
    store <- EsgExtractStore$new(dir)
    on.exit(store$close(), add = TRUE)

    aggs <- extract_store_test_result(
        type = "Aggregation",
        docs = extract_store_test_file_docs("tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20610101-20611231.nc")
    )

    query_id_1 <- store$add_files(aggs)
    query_id_2 <- store$add_files(aggs)
    conn <- epwshiftr:::priv(store)$conn

    expect_identical(query_id_2, query_id_1)
    expect_equal(nrow(DBI::dbReadTable(conn, "query_run")), 1L)
    expect_equal(nrow(DBI::dbReadTable(conn, "file_catalog")), 1L)
    expect_equal(DBI::dbReadTable(conn, "query_run")$result_type, "Aggregation")
})

test_that("EsgExtractStore plans regional extraction jobs from catalog filters", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    dir <- tempfile("esg-store-")
    store <- EsgExtractStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- data.table::rbindlist(list(
        extract_store_test_file_docs(
            "tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
            source_id = "EC-Earth3",
            experiment_id = "ssp585",
            variable_id = "tas"
        ),
        extract_store_test_file_docs(
            "hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
            source_id = "EC-Earth3",
            experiment_id = "ssp585",
            variable_id = "hurs"
        ),
        extract_store_test_file_docs(
            "tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gr_20600101-20601231.nc",
            source_id = "AWI-CM-1-1-MR",
            experiment_id = "ssp585",
            variable_id = "tas"
        )
    ), fill = TRUE)
    files <- extract_store_test_result(docs = as.data.frame(docs))
    query_id <- store$add_files(files)

    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-01", "2060-12-31"),
        site_id = "SIN",
        filters = list(source_id = "EC-Earth3"),
        variable_id = c("tas", "hurs"),
        nearest = 2L
    )

    expect_s3_class(plan, "data.table")
    expect_equal(nrow(plan), 2L)
    expect_setequal(plan$variable_id, c("tas", "hurs"))
    expect_equal(unique(plan$site_id), "SIN")
    expect_equal(unique(plan$status), "pending")
    expect_equal(unique(plan$nearest), 2L)
    expect_match(plan$plan_id, "^[0-9a-f]{64}$")

    conn <- epwshiftr:::priv(store)$conn
    DBI::dbExecute(conn, "UPDATE extraction_plan SET status = 'done'")
    plan_again <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-01", "2060-12-31"),
        site_id = "SIN",
        filters = list(source_id = "EC-Earth3"),
        variable_id = c("tas", "hurs"),
        nearest = 2L
    )
    expect_equal(nrow(DBI::dbReadTable(conn, "extraction_plan")), 2L)
    expect_equal(unique(plan_again$status), "done")
})

test_that("EsgExtractStore rejects invalid extraction plans", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    dir <- tempfile("esg-store-")
    store <- EsgExtractStore$new(dir)
    on.exit(store$close(), add = TRUE)

    query_id <- store$add_files(extract_store_test_result(docs = extract_store_test_file_docs()))

    expect_error(
        store$plan_region(
            query_id,
            lon = 103.98,
            lat = 1.37,
            time = c("2060-12-31", "2060-01-01")
        ),
        "greater than or equal"
    )
    expect_error(
        store$plan_region(
            query_id,
            lon = 103.98,
            lat = 1.37,
            time = c("2060-01-01", "2060-12-31"),
            filters = list(unknown = "x")
        ),
        "Unknown file catalog filter"
    )
    expect_error(
        store$plan_region(
            query_id,
            lon = 103.98,
            lat = 1.37,
            time = c("2060-01-01", "2060-12-31"),
            filters = list(source_id = "no-match")
        ),
        "No cataloged file records match"
    )
})

test_that("EsgExtractStore extracts regional data to Parquet", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L)
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgExtractStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- extract_store_test_file_docs(
        path = basename(nc),
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(extract_store_test_result(docs = docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z"),
        site_id = "SIN",
        nearest = 1L
    )

    processed <- store$extract(plan_id = plan$plan_id)
    conn <- epwshiftr:::priv(store)$conn
    plans <- DBI::dbReadTable(conn, "extraction_plan")
    results <- DBI::dbReadTable(conn, "extraction_result")

    expect_equal(processed$status, "done")
    expect_equal(plans$status, "done")
    expect_equal(plans$available_time_count, 2L)
    expect_equal(nrow(results), 1L)
    expect_equal(results$year, 2060L)
    expect_equal(results$row_count, 2L)
    expect_equal(results$unique_time_count, 2L)

    parquet <- file.path(dir, results$output_path)
    expect_true(file.exists(parquet))
    rows <- DBI::dbGetQuery(conn, sprintf(
        "SELECT site_id, source_id, experiment_id, variable_id, COUNT(*) AS n FROM read_parquet(%s) GROUP BY ALL",
        DBI::dbQuoteString(conn, parquet)
    ))
    expect_equal(rows$site_id, "SIN")
    expect_equal(rows$source_id, "EC-Earth3")
    expect_equal(rows$experiment_id, "ssp585")
    expect_equal(rows$variable_id, "tas")
    expect_equal(rows$n, 2)
})

test_that("EsgExtractStore records failed extraction plans", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")

    nc <- tempfile(fileext = ".nc")
    write_local_cmip6_netcdf_fixture(nc, 2060L)
    on.exit(unlink(nc), add = TRUE)

    dir <- tempfile("esg-store-")
    store <- EsgExtractStore$new(dir)
    on.exit(store$close(), add = TRUE)

    docs <- extract_store_test_file_docs(
        path = basename(nc),
        variable_id = "hurs",
        opendap_url = nc,
        download_url = nc
    )
    query_id <- store$add_files(extract_store_test_result(docs = docs))
    plan <- store$plan_region(
        query_id = query_id,
        lon = 103.98,
        lat = 1.37,
        time = c("2060-01-02T00:00:00Z", "2060-01-03T23:59:59Z")
    )

    processed <- store$extract(plan_id = plan$plan_id)
    plans <- DBI::dbReadTable(epwshiftr:::priv(store)$conn, "extraction_plan")

    expect_equal(processed$status, "failed")
    expect_equal(plans$status, "failed")
    expect_equal(plans$attempt_count, 1L)
    expect_match(plans$last_error, "None of the requested variable")
})
