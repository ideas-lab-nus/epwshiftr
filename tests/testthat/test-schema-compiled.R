test_that("schema__compiled() compiles each package schema once per process", {
    for (name in names(schema__compiled_documents)) {
        first <- schema__compiled(name)
        expect_true(
            S7::S7_inherits(first, SchemaFlat),
            info = name
        )
        # Reuse must return the stored object, not a recompiled copy.
        expect_identical(schema__compiled(name), first, info = name)
    }
})

test_that("schema__compiled() rejects unknown schema names", {
    expect_error(schema__compiled("NOT_A_SCHEMA"))
    expect_error(schema__compiled(NA_character_))
    expect_error(schema__compiled(c("QUERY", "ESG_DICT")))
})

test_that("compiled validation reports the same diagnostics as document validation", {
    # An empty list violates every package schema's required structure; the
    # message must not depend on whether the compiled form was cached.
    for (name in names(schema__compiled_documents)) {
        document <- get(schema__compiled_documents[[name]])
        expected <- schema_validate(
            document,
            list(),
            mode = "check",
            name = "payload"
        )
        actual <- schema_validate(
            schema__compiled(name),
            list(),
            mode = "check",
            name = "payload"
        )
        expect_identical(actual, expected, info = name)
        expect_type(actual, "character")
    }
})

test_that("a failed compilation is not cached and can be retried", {
    saved <- as.list(schema__compiled_cache, all.names = TRUE)
    withr::defer({
        rm(
            list = ls(schema__compiled_cache, all.names = TRUE),
            envir = schema__compiled_cache
        )
        list2env(saved, envir = schema__compiled_cache)
    })
    rm(
        list = ls(schema__compiled_cache, all.names = TRUE),
        envir = schema__compiled_cache
    )

    with_mocked_bindings(
        expect_error(schema__compiled("QUERY"), "compile failed"),
        schema_flat__compile = function(x) stop("compile failed")
    )
    expect_false(exists(
        "QUERY",
        envir = schema__compiled_cache,
        inherits = FALSE
    ))

    # Outside the mock the real compiler runs, so the same name now succeeds and
    # the completed result is stored.
    compiled <- schema__compiled("QUERY")
    expect_true(S7::S7_inherits(compiled, SchemaFlat))
    expect_identical(
        get("QUERY", envir = schema__compiled_cache, inherits = FALSE),
        compiled
    )
})

# vim: fdm=marker :
