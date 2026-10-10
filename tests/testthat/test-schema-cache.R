# Namespace scans used by coverage must not construct fixed schema documents.
test_that("fixed documents remain lazy during namespace enumeration", {
    local_mocked_bindings(schema__document_cache = list())
    namespace <- asNamespace("epwshiftr")
    load <- schema__load
    calls <- character()
    local_mocked_bindings(schema__load = function(file) {
        calls <<- c(calls, file)
        load(file)
    })
    invisible(eapply(
        namespace,
        function(value) inherits(value, "R6ClassGenerator"),
        all.names = TRUE
    ))
    expect_length(calls, 0L)
    expect_identical(get("schema__document_cache", namespace), list())
    expect_false(any(
        paste0("SCHEMA_", names(schema__document_files)) %in%
            ls(namespace, all.names = TRUE)
    ))

    document <- schema__document("QUERY")
    expect_identical(calls, "query.json")
    expect_identical(schema__document("QUERY"), document)
    expect_identical(calls, "query.json")
    expect_identical(names(get("schema__document_cache", namespace)), "QUERY")
    expect_true(S7::S7_inherits(document, SchemaDoc))
})

# Reads publish only successful documents and leave locks intact on retry.
test_that("failed fixed document reads can be retried", {
    local_mocked_bindings(schema__document_cache = list())
    namespace <- asNamespace("epwshiftr")
    locked <- bindingIsLocked("schema__document_cache", namespace)
    load <- schema__load
    calls <- 0L
    local_mocked_bindings(schema__load = function(file) {
        calls <<- calls + 1L
        if (calls == 1L) {
            stop("controlled document read failure")
        }
        load(file)
    })
    expect_error(schema__document("QUERY"), "controlled document read failure")
    expect_identical(get("schema__document_cache", namespace), list())
    expect_identical(
        bindingIsLocked("schema__document_cache", namespace),
        locked
    )
    expect_error(schema__document("UNKNOWN"))
    expect_identical(calls, 1L)
    expect_true(S7::S7_inherits(schema__document("QUERY"), SchemaDoc))
    expect_identical(calls, 2L)
    expect_identical(names(get("schema__document_cache", namespace)), "QUERY")
    expect_identical(
        bindingIsLocked("schema__document_cache", namespace),
        locked
    )
})

# The accessor retains every source document's S7 graph, metadata and JSON shape.
test_that("cached documents equal their unchanged JSON sources", {
    for (name in names(schema__document_files)) {
        expected <- schema__load(schema__document_files[[name]])
        document <- schema__document(name)
        expect_true(S7::S7_inherits(document, SchemaDoc))
        expect_identical(document, expected)
        expect_identical(as.list(document), as.list(expected))
        expect_identical(
            document@path,
            schema__extdata(schema__document_files[[name]])
        )
    }
    original <- schema__document("QUERY")
    edited <- original
    edited@path <- "edited-copy"
    expect_identical(schema__document("QUERY"), original)
    expect_false(identical(schema__document("QUERY")@path, edited@path))
})

# Replacing lists preserves copied bindings and serialized callback snapshots.
test_that("document caching preserves copied bindings and callback serialization", {
    local_mocked_bindings(schema__document_cache = list())
    namespace <- asNamespace("epwshiftr")
    callback_env <- new.env(parent = baseenv())
    callback_env$cache <- get("schema__document_cache", namespace)
    callback <- evalq(function(value) value + 1, callback_env)
    before <- serialize(callback, NULL)
    locked <- bindingIsLocked("schema__document_cache", namespace)

    invisible(schema__document("QUERY"))
    expect_identical(callback_env$cache, list())
    expect_identical(serialize(callback, NULL), before)
    expect_identical(unserialize(before)(2), 3)
    expect_identical(names(get("schema__document_cache", namespace)), "QUERY")
    snapshot <- get("schema__document_cache", namespace)
    invisible(schema__document("ESG_DICT"))
    expect_identical(names(snapshot), "QUERY")
    expect_identical(
        names(get("schema__document_cache", namespace)),
        c("QUERY", "ESG_DICT")
    )
    expect_identical(
        bindingIsLocked("schema__document_cache", namespace),
        locked
    )
})

# The cache is limited to package-owned schemas; documents remain editable.
test_that("fixed schemas compile lazily once without replacing documents", {
    local_mocked_bindings(schema__compiled_cache = list())
    # Read the current namespace value rather than a copied test binding.
    cache <- function() get("schema__compiled_cache", asNamespace("epwshiftr"))
    compile <- schema_flat__compile
    calls <- 0L
    local_mocked_bindings(schema_flat__compile = function(schema) {
        calls <<- calls + 1L
        compile(schema)
    })

    expect_length(cache(), 0L)
    first <- schema__compiled("QUERY")
    expect_identical(calls, 1L)
    expect_identical(schema__compiled("QUERY"), first)
    expect_identical(calls, 1L)
    expect_true(S7::S7_inherits(schema__document("QUERY"), SchemaDoc))
    expect_true(S7::S7_inherits(first, SchemaFlat))
    expect_error(schema__compiled("UNKNOWN"))
    expect_identical(names(cache()), "QUERY")
})

# A failed compile must not publish a partially initialized cache entry.
test_that("failed compilation can be retried without retaining partial state", {
    local_mocked_bindings(schema__compiled_cache = list())
    # Failed compilation must leave the namespace value unchanged.
    cache <- function() get("schema__compiled_cache", asNamespace("epwshiftr"))
    compile <- schema_flat__compile
    calls <- 0L
    local_mocked_bindings(schema_flat__compile = function(schema) {
        calls <<- calls + 1L
        if (calls == 1L) {
            stop("controlled compilation failure")
        }
        compile(schema)
    })
    expect_error(schema__compiled("QUERY"), "controlled compilation failure")
    expect_length(cache(), 0L)
    expect_true(S7::S7_inherits(schema__compiled("QUERY"), SchemaFlat))
    expect_identical(calls, 2L)
})

# All fixed schemas keep their compiled structure and diagnostic semantics.
test_that("cached schemas preserve validation and input boundaries", {
    names <- c(
        "QUERY",
        "RESPONSE",
        "RESULT_DATASET",
        "RESULT_FILE",
        "RESULT_AGGREGATION",
        "ESG_DICT",
        "DOWNLOADER_CONFIG",
        "SHIFT_WORKFLOW_CONFIG"
    )
    for (name in names) {
        document <- schema__document(name)
        flat <- schema__compiled(name)
        expect_identical(flat, schema_flat__compile(document))
        expect_identical(
            schema_validate(flat, list(), mode = "check", name = "payload"),
            schema_validate(document, list(), mode = "check", name = "payload")
        )
    }

    # The loop above exercises real compilation and SchemaDoc dispatch for all
    # eight documents. Compile the large reference once for the input matrix:
    # the real document method must still forward every value, mode and label
    # through this seam, without recompiling the unchanged graph 35 times.
    reference <- schema_flat__compile(schema__document("RESULT_DATASET"))
    compile <- schema_flat__compile
    document_dispatches <- 0L
    local_mocked_bindings(schema_flat__compile = function(schema) {
        if (identical(schema, schema__document("RESULT_DATASET"))) {
            document_dispatches <<- document_dispatches + 1L
            return(reference)
        }
        compile(schema)
    })

    payload <- jsonlite::fromJSON(
        test_path("_snaps", "query-result", "dataset.json"),
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
    )
    wrong <- payload
    wrong$parameter <- "invalid"
    missing <- payload
    missing$index_node <- NULL
    extra <- payload
    extra$response$response$docs$not_a_solr_field <- NA_real_
    zero <- payload
    zero$response$response$docs <- zero$response$response$docs[0L, ]
    partial <- payload
    partial$response$response$docs$id[[1L]] <- NA_character_
    duplicate <- payload
    duplicate$response$response$docs <- rbind(
        duplicate$response$response$docs,
        duplicate$response$response$docs[1L, ]
    )
    cases <- list(
        payload,
        wrong,
        missing,
        extra,
        zero,
        partial,
        duplicate,
        NULL,
        NA,
        list()
    )
    for (value in cases) {
        for (mode in c("check", "test", "expect")) {
            expect_identical(
                schema_validate(
                    schema__compiled("RESULT_DATASET"),
                    value,
                    mode = mode,
                    name = "payload"
                ),
                schema_validate(
                    schema__document("RESULT_DATASET"),
                    value,
                    mode = mode,
                    name = "payload"
                )
            )
        }
    }

    # Compare visibility, original return value, error class/message/call and
    # explicit labels used by production callers, not only boolean acceptance.
    # Standalone SchemaDoc and SchemaFlat infer different default names; all
    # cached production paths must retain their existing explicit labels.
    capture <- function(schema, value, name = NULL) {
        tryCatch(
            withVisible(schema_validate(schema, value, name = name)),
            error = function(error) {
                list(
                    class = class(error),
                    message = conditionMessage(error),
                    call = conditionCall(error)
                )
            }
        )
    }
    for (value in list(payload, wrong)) {
        for (name in list("payload", "saved-result")) {
            expect_identical(
                capture(schema__compiled("RESULT_DATASET"), value, name),
                capture(schema__document("RESULT_DATASET"), value, name)
            )
        }
    }

    path <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(wrong, path, null = "null", auto_unbox = TRUE)
    # Exercise the real saved-result loader's diagnostic contract as well.
    capture_load <- function(schema) {
        tryCatch(query__load(path, schema), error = function(error) {
            list(
                class = class(error),
                message = conditionMessage(error),
                call = conditionCall(error)
            )
        })
    }
    expect_identical(
        capture_load(schema__compiled("RESULT_DATASET")),
        capture_load(schema__document("RESULT_DATASET"))
    )
    expect_identical(document_dispatches, 35L)
})

# Editing a returned value or another document must never change fixed schemas.
test_that("compiled cache does not change standalone schema editing", {
    original <- schema__compiled("QUERY")
    edited <- original
    edited@path <- "edited-copy"
    expect_identical(schema__compiled("QUERY"), original)
    document <- schema_replace(
        schema__document("QUERY"),
        "$",
        schema_check("string")
    )
    expect_identical(document@version, schema__document("QUERY")@version)
    expect_true(schema_validate(document, "custom", mode = "test"))
    expect_false(schema_validate(
        schema__document("QUERY"),
        "custom",
        mode = "test"
    ))
    expect_match(
        schema_validate(schema__document("QUERY"), "custom", mode = "check"),
        "^x"
    )
    expect_identical(schema__compiled("QUERY"), original)
    expect_true(S7::S7_inherits(schema__document("QUERY"), SchemaDoc))
})

# Namespace clones must not capture compiled graphs added after their creation.
test_that("compilation preserves copied bindings and callback serialization", {
    local_mocked_bindings(schema__compiled_cache = list())
    namespace <- asNamespace("epwshiftr")
    callback_env <- new.env(parent = baseenv())
    callback_env$cache <- get("schema__compiled_cache", namespace)
    callback <- evalq(function(value) value + 1, callback_env)
    before <- serialize(callback, NULL)
    locked <- bindingIsLocked("schema__compiled_cache", namespace)

    invisible(schema__compiled("QUERY"))
    expect_identical(callback_env$cache, list())
    expect_identical(serialize(callback, NULL), before)
    expect_identical(unserialize(before)(2), 3)
    expect_identical(names(get("schema__compiled_cache", namespace)), "QUERY")
    expect_identical(
        bindingIsLocked("schema__compiled_cache", namespace),
        locked
    )

    # A second publication preserves snapshots of the first compiled value too.
    snapshot <- get("schema__compiled_cache", namespace)
    invisible(schema__compiled("ESG_DICT"))
    expect_identical(names(snapshot), "QUERY")
    expect_identical(
        names(get("schema__compiled_cache", namespace)),
        c("QUERY", "ESG_DICT")
    )
    expect_identical(
        bindingIsLocked("schema__compiled_cache", namespace),
        locked
    )
})

# vim: fdm=marker :
