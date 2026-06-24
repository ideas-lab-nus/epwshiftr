# Helper function to create a simple schema
simple_schema <- function() {
    structure(
        list(
            type = "list",
            names = list(must.include = c("field1", "field2")),
            fields = list(
                field1 = list(type = "string"),
                field2 = list(type = "int")
            )
        ),
        class = "Schema"
    )
}

# Helper function to create a nested schema
nested_schema <- function() {
    structure(
        list(
            type = list("list", null.ok = TRUE, names = "unique"),
            names = list(must.include = c("header", "data")),
            fields = list(
                header = list(
                    type = "list",
                    names = list(must.include = c("status", "message")),
                    fields = list(
                        status = list(type = "int"),
                        message = list(type = "string")
                    )
                ),
                data = list(
                    type = list("data_frame", null.ok = TRUE, col.names = "unique"),
                    names = list(subset.of = c("id", "name", "value"))
                )
            )
        ),
        class = "Schema"
    )
}

test_that("print.schema: basic functionality", {
    schema <- simple_schema()
    expect_snapshot(print(schema))
})

test_that("print.schema: parameter works", {
    schema <- nested_schema()

    # With max_depth = 0
    expect_snapshot(print(schema, max_depth = 0))

    # With max_depth = 1
    expect_snapshot(print(schema, max_depth = 1))

    # With max_depth = 2
    expect_snapshot(print(schema, max_depth = 2))

    # With show_constraints = TRUE
    expect_snapshot(print(schema, show_constraints = TRUE))

    # With show_constraints = FALSE
    expect_snapshot(print(schema, show_constraints = FALSE))

    # Create schema with many required fields
    schema <- structure(
        list(
            type = "list",
            names = list(must.include = letters[1:10]),
            fields = list()
        ),
        class = "Schema"
    )

    # Non-compact mode
    expect_snapshot(print(schema, compact = FALSE))

    # Compact mode
    expect_snapshot(print(schema, compact = TRUE))
})

test_that("format_type", {
    expect_equal(format_type("string"), "string")
    expect_equal(format_type("int"), "int")

    type <- list("list", null.ok = TRUE, names = "unique")
    result <- format_type(type)

    expect_match(result, "list")
    expect_match(result, "null\\.ok")
    expect_match(result, "names")

    type <- list("choice", c("option1", "option2"))
    result <- format_type(type)

    expect_match(result, "choice")
    expect_match(result, "option1")
    expect_match(result, "option2")

    type <- list("choice", letters[1:10])
    result <- format_type(type)
    expect_match(result, "choice")
    expect_match(result, "10 options")

    type <- list("character", any.missing = FALSE, unique = TRUE)
    result <- format_type(type)
    expect_match(result, "any\\.missing=FALSE")
    expect_match(result, "unique")

    type <- list("list", rules = "S1")
    result <- format_type(type)
    expect_match(result, "rules")

    type <- list("list", rules = c("S1", "I1"))
    result <- format_type(type)
    expect_match(result, "rules")
})

test_that("summary.schema", {
    schema <- simple_schema()

    # Should return invisibly
    expect_snapshot(result <- summary(schema))
    expect_type(result, "list")

    schema <- nested_schema()
    expect_snapshot(stats <- summary(schema))

    expect_equal(stats$root_type, "list")
    expect_equal(stats$required_fields, 2)
    expect_true(stats$total_fields > 0)
    expect_true(stats$max_depth > 0)
})

test_that("str.schema", {
    schema <- simple_schema()

    # Snapshot test
    expect_snapshot(str(schema))
})

test_that("count_fields", {
    schema <- simple_schema()
    expect_equal(count_fields(schema), 2)

    schema_nested <- nested_schema()
    expect_equal(count_fields(schema_nested), 4)  # header, status, message, data

    schema <- structure(
        list(type = "list"),
        class = "Schema"
    )
    expect_equal(count_fields(schema), 0)
})

test_that("get_max_depth", {
    schema <- simple_schema()
    expect_equal(get_max_depth(schema), 1)

    schema_nested <- nested_schema()
    expect_equal(get_max_depth(schema_nested), 2)

    schema <- structure(
        list(type = "list"),
        class = "Schema"
    )
    expect_equal(get_max_depth(schema), 0)
})

test_that("as.data.frame.schema", {
    schema <- simple_schema()
    df <- as.data.frame(schema)

    expect_s3_class(df, "data.frame")
    expect_true("path" %in% names(df))
    expect_true("type" %in% names(df))
    expect_true("constraints" %in% names(df))
    expect_true("$field1" %in% df$path)
    expect_true("$field2" %in% df$path)

    field1_row <- df[df$path == "$field1", ]
    expect_equal(field1_row$type, "string")
    field2_row <- df[df$path == "$field2", ]
    expect_equal(field2_row$type, "int")

    schema <- nested_schema()
    df <- as.data.frame(schema)

    expect_true("$header" %in% df$path)
    expect_true("$header$status" %in% df$path)
    expect_true("$header$message" %in% df$path)
    expect_true("$data" %in% df$path)

    schema <- structure(
        list(
            type = "list",
            fields = list(
                field1 = list(
                    type = list("character", null.ok = TRUE, unique = TRUE),
                    names = list(must.include = c("a", "b"))
                )
            )
        ),
        class = "Schema"
    )

    df <- as.data.frame(schema)
    field1_row <- df[df$path == "$field1", ]
    expect_match(field1_row$constraints, "null\\.ok")
    expect_match(field1_row$constraints, "unique")
    expect_match(field1_row$constraints, "required=2")

    schema <- structure(
        list(type = "list"),
        class = "Schema"
    )
    df <- as.data.frame(schema)
    expect_equal(nrow(df), 0)
    expect_true("path" %in% names(df))
})

test_that("print.schema", {
    schema <- structure(
        list(
            type = "list",
            fields = list(
                "..." = list(type = "string")
            )
        ),
        class = "Schema"
    )

    expect_snapshot(print(schema))

    schema <- structure(
        list(
            type = "list",
            names = list(subset.of = c("field1", "field2", "field3"))
        ),
        class = "Schema"
    )

    expect_snapshot(print(schema))

    schema <- structure(
        list(type = "list"),
        class = "Schema"
    )
    expect_snapshot(print(schema))
})

test_that("SCHEMA_RESPONSE", {
    expect_s3_class(SCHEMA_RESPONSE, "Schema")
    expect_snapshot(print(SCHEMA_RESPONSE))

    expect_snapshot(stats <- summary(SCHEMA_RESPONSE))
    expect_type(stats, "list")
    expect_true(stats$total_fields > 0)
    expect_true(stats$max_depth > 0)

    df <- as.data.frame(SCHEMA_RESPONSE)
    expect_s3_class(df, "data.frame")
    expect_true(nrow(df) > 0)
    expect_true("$responseHeader" %in% df$path)
    expect_true("$response" %in% df$path)
})

test_that("SCHEMA_QUERY", {
    expect_s3_class(SCHEMA_QUERY, "Schema")
    expect_snapshot(print(SCHEMA_QUERY))

    expect_snapshot(stats <- summary(SCHEMA_QUERY))
    expect_type(stats, "list")
    expect_true(stats$total_fields > 0)
})

test_that("SCHEMA_RESULT_DATASET", {
    expect_s3_class(SCHEMA_RESULT_DATASET, "Schema")
    expect_snapshot(print(SCHEMA_RESULT_DATASET))

    expect_snapshot(stats <- summary(SCHEMA_RESULT_DATASET))
    expect_type(stats, "list")
    expect_true(stats$total_fields > 0)
    expect_true(stats$max_depth > 0)

    df <- as.data.frame(SCHEMA_RESULT_DATASET)
    expect_s3_class(df, "data.frame")
    expect_true(nrow(df) > 0)
    expect_true("$index_node" %in% df$path)
    expect_true("$parameter" %in% df$path)
    expect_true("$response" %in% df$path)
})
