test_that("shared truncation footer preserves display behavior", {
    testthat::local_reproducible_output(crayon = FALSE, unicode = TRUE)

    vector_footer <- capture.output(
        print__truncation_footer(letters[1:3], 2L, newline_before = FALSE),
        type = "message"
    )
    expect_identical(vector_footer, "# ... with 1 more item")

    table_footer <- capture.output(
        print__truncation_footer(data.frame(value = 1:4), 2L),
        type = "message"
    )
    expect_identical(table_footer, c("", "# ... with 2 more items"))

    complete_footer <- capture.output(
        print__truncation_footer(letters[1:2], 2L),
        type = "message"
    )
    expect_identical(complete_footer, character())
})
