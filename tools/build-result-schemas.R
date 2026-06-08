#!/usr/bin/env Rscript

requireNamespace("jsonlite")

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
schema_dir <- file.path(repo_root, "inst", "extdata", "schema")
schema_files <- file.path(
    schema_dir,
    c("result-dataset.json", "result-file.json", "result-aggregation.json")
)

read_schema_json <- function(path) {
    jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

write_schema_json <- function(x, path) {
    jsonlite::write_json(
        x,
        path,
        pretty = TRUE,
        auto_unbox = TRUE,
        null = "null",
        na = "null"
    )
}

ref <- function(name) {
    list(`$ref` = paste0("#/$defs/", name))
}

ref_name <- function(node) {
    if (!is.list(node) || is.null(node[["$ref"]])) {
        return(NULL)
    }
    sub("^#/\\$defs/", "", node[["$ref"]])
}

resolve_ref <- function(node, defs) {
    name <- ref_name(node)
    if (is.null(name)) {
        return(node)
    }
    defs[[name]]
}

promote_parameter_defs <- function(parameter, defs) {
    buckets <- c("facet", "query", "control", "others")
    for (bucket in buckets) {
        if (is.null(parameter$fields[[bucket]])) {
            next
        }

        name <- paste0("parameter_", bucket)
        defs[[name]] <- resolve_ref(parameter$fields[[bucket]], defs)
        parameter$fields[[bucket]] <- ref(name)
    }

    defs$parameter <- parameter
    defs
}

promote_response_defs <- function(response, defs) {
    if (!is.null(response$fields$responseHeader)) {
        defs$response_header <- resolve_ref(response$fields$responseHeader, defs)
        response$fields$responseHeader <- ref("response_header")
    }
    if (!is.null(response$fields$response)) {
        body <- resolve_ref(response$fields$response, defs)
        if (!is.null(body$fields$docs)) {
            defs$response_docs <- resolve_ref(body$fields$docs, defs)
            body$fields$docs <- ref("response_docs")
        }
        defs$response_body <- body
        response$fields$response <- ref("response_body")
    }
    if (!is.null(response$fields$facet_counts)) {
        defs$facet_counts <- resolve_ref(response$fields$facet_counts, defs)
        response$fields$facet_counts <- ref("facet_counts")
    }
    if (!is.null(response$fields$timestamp)) {
        defs$timestamp <- resolve_ref(response$fields$timestamp, defs)
        response$fields$timestamp <- ref("timestamp")
    }

    defs$response <- response
    defs
}

order_defs <- function(defs) {
    preferred <- c(
        "index_node",
        "parameter",
        "parameter_facet",
        "parameter_query",
        "parameter_control",
        "parameter_others",
        "response",
        "response_header",
        "response_body",
        "response_docs",
        "facet_counts",
        "timestamp"
    )
    defs[c(intersect(preferred, names(defs)), setdiff(names(defs), preferred))]
}

build_result_schema <- function(schema) {
    defs <- schema[["$defs"]]
    if (is.null(defs)) {
        defs <- list()
    }

    defs$index_node <- resolve_ref(schema$fields$index_node, defs)
    defs <- promote_parameter_defs(resolve_ref(schema$fields$parameter, defs), defs)
    defs <- promote_response_defs(resolve_ref(schema$fields$response, defs), defs)
    defs <- order_defs(defs)

    schema$fields <- list(
        index_node = ref("index_node"),
        parameter = ref("parameter"),
        response = ref("response")
    )
    schema[["$defs"]] <- defs
    schema
}

for (path in schema_files) {
    write_schema_json(build_result_schema(read_schema_json(path)), path)
}
