schema__extdata <- function(file) {
    system.file(
        "extdata",
        "schema",
        file,
        package = "epwshiftr",
        mustWork = TRUE
    )
}

schema__load <- function(file) {
    schema_read(schema__extdata(file))
}

schema__load_lazy <- function(name, file, env = parent.frame()) {
    force(file)
    delayedAssign(
        name,
        schema__load(file),
        assign.env = env,
        eval.env = environment()
    )
    invisible(name)
}

schema__load_lazy("SCHEMA_QUERY", "query.json")
schema__load_lazy("SCHEMA_RESPONSE", "response.json")
schema__load_lazy("SCHEMA_RESULT_DATASET", "result-dataset.json")
schema__load_lazy("SCHEMA_ESG_DICT", "esg-dict.json")

# vim: fdm=marker :
