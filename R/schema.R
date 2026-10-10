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

# Fixed package document names identify the unchanged JSON sources. Store only
# filenames at namespace load: coverage discovery enumerates namespace values,
# so namespace promises would construct every document before any caller needs it.
schema__document_files <- c(
    QUERY = "query.json",
    RESPONSE = "response.json",
    RESULT_DATASET = "result-dataset.json",
    RESULT_FILE = "result-file.json",
    RESULT_AGGREGATION = "result-aggregation.json",
    ESG_DICT = "esg-dict.json",
    DOWNLOADER_CONFIG = "downloader-config.json",
    SHIFT_WORKFLOW_CONFIG = "shift-workflow-config.json"
)

# Replace ordinary lists on publication so copied namespace bindings and
# serialized callbacks do not acquire documents constructed after their copy.
schema__document_cache <- list()

# Read one fixed document on demand, retaining standalone editing and source
# metadata. Failed reads leave no cache entry and can be retried by the caller.
# schema__document {{{
schema__document <- function(name) {
    checkmate::assert_choice(name, names(schema__document_files))
    namespace <- environment(schema__document)
    cache <- get("schema__document_cache", envir = namespace, inherits = FALSE)
    if (!name %in% names(cache)) {
        document <- schema__load(schema__document_files[[name]])
        cache[[name]] <- document
        # Publish only the completed document; restore the original binding lock
        # even on assignment failure. Each worker has its own namespace/cache.
        locked <- bindingIsLocked("schema__document_cache", namespace)
        if (locked) {
            unlockBinding("schema__document_cache", namespace)
            on.exit(
                lockBinding("schema__document_cache", namespace),
                add = TRUE
            )
        }
        assign("schema__document_cache", cache, envir = namespace)
    }
    cache[[name]]
}
# }}}

# Keep compiled package schemas local to this namespace and R process. The
# original documents remain available for inspection and standalone editing.
# Replace this list instead of mutating a shared environment: namespace clones
# used by tests and serialized callbacks must not acquire later compiled graphs.
schema__compiled_cache <- list()

# Compile a fixed package schema on first use, publishing only successful
# results. Arbitrary or edited SchemaDoc objects retain normal validation.
# Callers supply their existing explicit diagnostic names because the standalone
# document and compiled methods infer different labels when name is omitted.
# schema__compiled {{{
schema__compiled <- function(name) {
    checkmate::assert_choice(
        name,
        c(
            "QUERY",
            "RESPONSE",
            "RESULT_DATASET",
            "RESULT_FILE",
            "RESULT_AGGREGATION",
            "ESG_DICT",
            "DOWNLOADER_CONFIG",
            "SHIFT_WORKFLOW_CONFIG"
        )
    )
    namespace <- environment(schema__compiled)
    cache <- get("schema__compiled_cache", envir = namespace, inherits = FALSE)
    if (!name %in% names(cache)) {
        document <- schema__document(name)
        compiled <- schema_flat__compile(document)
        cache[[name]] <- compiled
        # Publish only a completed value to our own namespace, restoring the
        # original binding lock even if assignment fails. R executes this
        # synchronously; separate workers retain their independent caches.
        locked <- bindingIsLocked("schema__compiled_cache", namespace)
        if (locked) {
            unlockBinding("schema__compiled_cache", namespace)
            on.exit(
                lockBinding("schema__compiled_cache", namespace),
                add = TRUE
            )
        }
        assign("schema__compiled_cache", cache, envir = namespace)
    }
    cache[[name]]
}
# }}}

# vim: fdm=marker :
