# schema__extdata {{{
schema__extdata <- function(file) {
    system.file(
        "extdata",
        "schema",
        file,
        package = "epwshiftr",
        mustWork = TRUE
    )
}
# }}}

# schema__load {{{
schema__load <- function(file) {
    schema_read(schema__extdata(file))
}
# }}}

# schema__load_lazy {{{
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
# }}}

schema__load_lazy("SCHEMA_QUERY", "query.json")
schema__load_lazy("SCHEMA_RESPONSE", "response.json")
schema__load_lazy("SCHEMA_RESULT_DATASET", "result-dataset.json")
schema__load_lazy("SCHEMA_RESULT_FILE", "result-file.json")
schema__load_lazy("SCHEMA_RESULT_AGGREGATION", "result-aggregation.json")
schema__load_lazy("SCHEMA_ESG_DICT", "esg-dict.json")
schema__load_lazy("SCHEMA_DOWNLOADER_CONFIG", "downloader-config.json")
schema__load_lazy("SCHEMA_SHIFT_WORKFLOW_CONFIG", "shift-workflow-config.json")

# Map each short schema name to the lazy document constant it compiles. The
# SchemaDoc constants stay available for inspection and editing; only the
# validation path uses the compiled form.
schema__compiled_documents <- c(
    QUERY = "SCHEMA_QUERY",
    RESPONSE = "SCHEMA_RESPONSE",
    RESULT_DATASET = "SCHEMA_RESULT_DATASET",
    RESULT_FILE = "SCHEMA_RESULT_FILE",
    RESULT_AGGREGATION = "SCHEMA_RESULT_AGGREGATION",
    ESG_DICT = "SCHEMA_ESG_DICT",
    DOWNLOADER_CONFIG = "SCHEMA_DOWNLOADER_CONFIG",
    SHIFT_WORKFLOW_CONFIG = "SCHEMA_SHIFT_WORKFLOW_CONFIG"
)

# Per-process store of compiled package schemas. An environment is mutable even
# though namespace bindings are locked, so no binding needs unlocking. Each
# R process (including mirai workers) loads its own namespace and therefore
# fills its own, initially empty, cache.
schema__compiled_cache <- new.env(parent = emptyenv())

# Return the compiled SchemaFlat of one fixed package schema.
#
# `SchemaDoc` validation recompiles the whole schema graph on every call, which
# dominated profiled test time. Compiling once per process and validating many
# payloads against the result keeps every check while removing that repeated
# work. Only a completed compilation is stored, so a failed read or compile is
# retried by the next caller. Callers must keep passing an explicit `name` to
# `schema_validate()`: the compiled and document methods infer different
# labels when it is omitted.
# schema__compiled {{{
schema__compiled <- function(name) {
    checkmate::assert_choice(name, names(schema__compiled_documents))
    compiled <- schema__compiled_cache[[name]]
    if (is.null(compiled)) {
        document <- get(
            schema__compiled_documents[[name]],
            envir = environment(schema__compiled),
            inherits = FALSE
        )
        compiled <- schema_flat__compile(document)
        assign(name, compiled, envir = schema__compiled_cache)
    }
    compiled
}
# }}}

# vim: fdm=marker :
