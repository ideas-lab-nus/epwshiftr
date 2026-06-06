ESGF_SCHEMA_QUERY_URL <- paste0(
    "https://esgf-node.ornl.gov/esgf-1-5-bridge?",
    "project=CMIP6&offset=0&limit=10&type=Dataset&",
    "format=application%2Fsolr%2Bjson&",
    "facets=activity_id%2C+data_node%2C+source_id%2C+institution_id%2C+",
    "source_type%2C+experiment_id%2C+sub_experiment_id%2C+",
    "nominal_resolution%2C+variant_label%2C+grid_label%2C+table_id%2C+",
    "frequency%2C+realm%2C+variable_id%2C+cf_standard_name&",
    "latest=true&min_version=20231001&max_version=20251001&",
    "query=*&nominal_resolution=100+km"
)

schema_bootstrap <- function() {
    assign("convert", S7::convert, envir = globalenv())

    files <- c(
        "R/utils.R",
        "R/epwshiftr-package.R",
        "R/import-standalone-schema.R",
        "R/solr_date.R",
        "R/query-param.R",
        "R/query-result.R",
        "R/query.R"
    )
    for (file in files) {
        source(file, local = globalenv())
    }
}

schema_direct_child <- function(parent) {
    force(parent)
    function(path, node) {
        prefix <- paste0(parent, "$")
        startsWith(path, prefix) && !grepl("$", substring(path, nchar(prefix) + 1L), fixed = TRUE)
    }
}

schema_replace_children <- function(schema, parent, value) {
    schema_replace_where(
        schema,
        schema_direct_child(parent),
        value,
        missing = "ignore"
    )
}

schema_path_join <- function(parent, child) {
    if (identical(parent, "$")) {
        paste0("$", child)
    } else {
        paste0(parent, "$", child)
    }
}

schema_atomic_or_null <- function() {
    schema_any(
        schema_check("character", any.missing = FALSE),
        schema_check("logical", any.missing = FALSE),
        schema_check("integer", any.missing = FALSE),
        schema_check("numeric", any.missing = FALSE),
        schema_check("null")
    )
}

schema_saved_param <- function() {
    schema_any(
        schema_check("null"),
        list(
            check = list(kind = "list"),
            keys = list(
                type = "named",
                subset.of = c("kind", "value", "negate", "encoded"),
                must.include = "value"
            ),
            fields = list(
                kind = schema_check("string"),
                value = schema_atomic_or_null(),
                negate = schema_check("flag"),
                encoded = schema_check("flag")
            )
        )
    )
}

schema_solr_param <- function() {
    schema_any(
        schema_check("string"),
        schema_check("character", any.missing = FALSE),
        schema_check("logical", any.missing = FALSE),
        schema_check("integer", any.missing = FALSE),
        schema_check("numeric", any.missing = FALSE),
        schema_check("list"),
        schema_check("null")
    )
}

schema_facet_count <- function() {
    schema_check("character", any.missing = FALSE)
}

schema_doc_column <- function() {
    schema_any(
        schema_check("character", any.missing = TRUE),
        schema_check("logical", any.missing = TRUE),
        schema_check("integer", any.missing = TRUE),
        schema_check("numeric", any.missing = TRUE),
        schema_check("list"),
        schema_check("null")
    )
}

schema_doc_fields <- function(response) {
    sort(unique(c(
        FIELDS_FACETS_ALL,
        names(response$response$docs),
        c(
            "_timestamp",
            "_version_",
            "access",
            "checksum",
            "checksum_type",
            "citation_url",
            "dataset_id_template_",
            "filename",
            "geo_units",
            "height_bottom",
            "height_top",
            "id",
            "instance_id",
            "latest",
            "master_id",
            "number_of_aggregations",
            "number_of_files",
            "pid",
            "replica",
            "retracted",
            "score",
            "size",
            "tracking_id",
            "type",
            "url",
            "url_download",
            "url_opendap",
            "version",
            "xlink"
        )
    )))
}

schema_relax_parameters <- function(schema, path = "$parameter") {
    param_schema <- schema_saved_param()
    schema <- schema_set_keys(
        schema,
        path,
        type = "named"
    )
    schema <- schema_replace_children(schema, path, param_schema)
    schema <- tryCatch(
        schema_replace(schema, paste0(path, "$`fields`"), param_schema),
        error = function(e) schema
    )
    schema_set_rest(schema, param_schema, path = path)
}

schema_relax_response <- function(schema, response_path = "$", response) {
    docs_path <- schema_path_join(response_path, "response$docs")
    facet_fields_path <- schema_path_join(response_path, "facet_counts$facet_fields")
    params_path <- schema_path_join(response_path, "responseHeader$params")

    doc_fields <- schema_doc_fields(response)
    solr_param <- schema_solr_param()
    facet_count <- schema_facet_count()
    doc_column <- schema_doc_column()

    schema <- schema_set_keys(schema, docs_path, type = "named", subset.of = doc_fields)
    schema <- schema_replace_children(schema, docs_path, doc_column)
    schema <- schema_set_keys(schema, facet_fields_path, type = "named", subset.of = doc_fields)
    schema <- schema_set_rest(schema, facet_count, path = facet_fields_path)
    schema <- schema_replace_children(schema, facet_fields_path, facet_count)

    schema <- schema_set_keys(schema, params_path, type = "named")
    schema <- schema_set_rest(schema, solr_param, path = params_path)
    schema_replace_children(schema, params_path, solr_param)
}

schema_infer_file <- function(file) {
    schema_compact(schema_infer(
        jsonlite::fromJSON(file, simplifyVector = TRUE, simplifyMatrix = FALSE),
        keys = "required",
        arrays = "rest"
    ))
}

schema_write_all <- function() {
    schema_bootstrap()

    out_dir <- file.path("inst", "extdata", "schema")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    response <- read_json_response(
        ESGF_SCHEMA_QUERY_URL,
        strict = TRUE,
        cache = FALSE,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )

    params <- query_param_store()
    params$activity_id <- as_query_param("activity_id", "HighResMIP")
    params$source_id <- as_query_param("source_id", "CAM-MPAS-LR")
    params$variable_id <- as_query_param("variable_id", "tas")
    params$variant_label <- as_query_param("variant_label", "r1i1p1f1")
    params$nominal_resolution <- as_query_param("nominal_resolution", "100 km")
    params$fields <- as_query_param("fields", c("source_id", "experiment_id", "frequency"))
    params$facets <- as_query_param("facets", c(
        "activity_id",
        "data_node",
        "source_id",
        "institution_id",
        "source_type",
        "experiment_id",
        "sub_experiment_id",
        "nominal_resolution",
        "variant_label",
        "grid_label",
        "table_id",
        "frequency",
        "realm",
        "variable_id",
        "cf_standard_name"
    ))
    params$version_min <- as_query_param("version_min", "20231001")
    params$version_max <- as_query_param("version_max", "20251001")
    params$others$query <- as_query_param("query", "*")

    query_file <- tempfile(fileext = ".json")
    result_file <- tempfile(fileext = ".json")
    on.exit(unlink(c(query_file, result_file)), add = TRUE)

    query_save(
        index_node = "https://esgf-node.ornl.gov/esgf-1-5-bridge",
        parameter = params,
        response = NULL,
        file = query_file
    )
    query_save(
        index_node = "https://esgf-node.ornl.gov/esgf-1-5-bridge",
        parameter = params,
        response = response,
        file = result_file
    )

    query_schema <- schema_relax_parameters(schema_infer_file(query_file))

    saved_response <- jsonlite::fromJSON(
        result_file,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )$response

    response_schema <- schema_compact(schema_infer(saved_response, keys = "required", arrays = "rest"))
    response_schema <- schema_relax_response(response_schema, response = response)

    result_schema <- schema_infer_file(result_file)
    result_schema <- schema_relax_parameters(result_schema)
    result_schema <- schema_relax_response(result_schema, response_path = "$response", response = response)

    schema_write(query_schema, file.path(out_dir, "query.json"), overwrite = TRUE)
    schema_write(response_schema, file.path(out_dir, "response.json"), overwrite = TRUE)
    schema_write(result_schema, file.path(out_dir, "result-dataset.json"), overwrite = TRUE)
}

schema_write_all()
