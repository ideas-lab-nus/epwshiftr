# Build a stable Solr response envelope for local morphing integration tests.
epw_morpher_test_response <- function(docs) {
    esgf_test__response(docs)
}

# Return the file-query parameters shared by local morphing fixtures.
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

# Construct a typed ESGF result from local file-document fixtures.
epw_morpher_test_result <- function(docs) {
    query_result__new(
        EsgResultFile,
        index_node = "https://example.org",
        params = epw_morpher_test_params(),
        result = epw_morpher_test_response(docs)
    )
}

# Describe one local NetCDF file with the ESGF fields consumed by EsgStore.
epw_morpher_test_file_docs <- function(
    path,
    opendap_url,
    download_url,
    variable_id = "tas"
) {
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
