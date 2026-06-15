# epwshiftr_cli_render_table() {{{
test_that("epwshiftr_cli_render_table() renders stable boxed tables", {
    testthat::local_reproducible_output(crayon = FALSE, unicode = TRUE)
    withr::local_options(cli.num_colors = 1L, width = 96L)

    rows <- data.frame(
        status = c("queued", "error", "done"),
        filename = c("queued.nc", "failed.nc", "done.nc"),
        bytes_done = c(0, 128, 1024),
        size = c(1024, 1024, 1024),
        attempts = c(0L, 2L, 1L),
        last_error = c(NA_character_, "temporary failure", NA_character_),
        stringsAsFactors = FALSE
    )

    text <- capture.output(
        epwshiftr:::epwshiftr_cli_render_table(
            rows,
            title = "Snapshot table",
            columns = c("status", "filename", "bytes_done", "size", "attempts", "last_error")
        ),
        type = "message"
    )

    expect_snapshot(cat(text, sep = "\n"))
})

test_that("epwshiftr_cli_render_table() snapshots narrow table adaptation", {
    testthat::local_reproducible_output(crayon = FALSE, unicode = TRUE)
    withr::local_options(cli.num_colors = 1L, width = 54L)

    rows <- data.frame(
        status = "queued",
        filename = "very-long-climate-file-name-for-cli-output.nc",
        bytes_done = 0,
        size = 1024,
        attempts = 0,
        last_error = "this error message should be hidden",
        session_id = paste(rep("s", 64L), collapse = ""),
        task_id = paste(rep("t", 64L), collapse = ""),
        file_key = paste(rep("f", 64L), collapse = ""),
        stringsAsFactors = FALSE
    )

    text <- capture.output(
        epwshiftr:::epwshiftr_cli_render_table(
            rows,
            title = "Narrow table",
            columns = c("status", "filename", "bytes_done", "size", "attempts", "last_error", "session_id", "task_id", "file_key")
        ),
        type = "message"
    )

    expect_snapshot(cat(text, sep = "\n"))
})
# }}}
# epwshiftr_cli_context() {{{
test_that("epwshiftr_cli_context() captures selected query search columns", {
    context <- epwshiftr:::epwshiftr_cli_context(list(
        help = FALSE,
        args = c("query", "search", "--columns", "source_id,variable_id", "project=CMIP6")
    ))
    expect_equal(context$columns, c("source_id", "variable_id"))
})
# }}}
# epwshiftr_cli_render() {{{
test_that("epwshiftr_cli_render() renders selected query search columns", {
    testthat::local_reproducible_output(crayon = FALSE, unicode = TRUE)
    withr::local_options(cli.num_colors = 1L, width = 96L)

    rows <- data.frame(
        title = "tas_Amon.nc",
        source_id = "EC-Earth3",
        variable_id = "tas",
        size = 1024,
        stringsAsFactors = FALSE
    )

    context <- epwshiftr:::epwshiftr_cli_context(list(
        help = FALSE,
        args = c("query", "search", "--columns", "source_id,variable_id", "project=CMIP6")
    ))

    text <- capture.output(
        epwshiftr:::epwshiftr_cli_render(
            rows,
            context = context
        ),
        type = "message"
    )
    expect_true(any(grepl("Source Id", text, fixed = TRUE)))
    expect_true(any(grepl("Variable Id", text, fixed = TRUE)))
    expect_false(any(grepl("Title", text, fixed = TRUE)))
    expect_false(any(grepl("Size", text, fixed = TRUE)))

    expect_error(
        epwshiftr:::epwshiftr_cli_render(
            rows,
            context = list(group = "query", command = "search", columns = "missing_column")
        ),
        "Unknown display column"
    )
})
# }}}
# epwshiftr_cli_render_table() {{{
test_that("epwshiftr_cli_render_table() adapts table output to console width", {
    withr::local_options(width = 54L)

    rows <- data.frame(
        status = "queued",
        filename = "very-long-climate-file-name-for-cli-output.nc",
        bytes_done = 0,
        size = 1024,
        attempts = 0,
        last_error = "this error message should be hidden before core status columns",
        session_id = paste(rep("s", 64L), collapse = ""),
        task_id = paste(rep("t", 64L), collapse = ""),
        file_key = paste(rep("f", 64L), collapse = ""),
        stringsAsFactors = FALSE
    )

    text <- capture.output(
        epwshiftr:::epwshiftr_cli_render_table(
            rows,
            title = "Narrow table",
            columns = c("status", "filename", "bytes_done", "size", "attempts", "last_error", "session_id", "task_id", "file_key")
        ),
        type = "message"
    )

    table_lines <- text[grepl("^[\u250c\u2502\u251c\u2514+|]", text)]
    expect_true(length(table_lines) > 0L)
    expect_lte(max(cli::ansi_nchar(table_lines, type = "width")), 54L)
    expect_true(any(grepl("Hidden columns for console width", text)))
    expect_true(any(grepl("Status", text)))
})
# }}}
# epwshiftr_cli_color_status() {{{
test_that("epwshiftr_cli_color_status() highlights statuses", {
    withr::local_options(cli.num_colors = 256L, width = 120L)

    danger <- epwshiftr:::epwshiftr_cli_color_status("error")
    success <- epwshiftr:::epwshiftr_cli_color_status("done")
    expect_false(identical(danger, "error"))
    expect_false(identical(success, "done"))
    expect_equal(cli::ansi_nchar(danger, type = "width"), 5L)
    expect_equal(cli::ansi_nchar(success, type = "width"), 4L)
})
# }}}
# epwshiftr_cli_render_table() {{{
test_that("epwshiftr_cli_render_table() renders progress", {
    withr::local_options(cli.num_colors = 256L, width = 120L)

    rows <- data.frame(
        status = c("error", "done"),
        filename = c("failed.nc", "done.nc"),
        bytes_done = c(128, 1024),
        size = c(1024, 1024),
        attempts = c(2L, 1L),
        last_error = c("temporary failure", NA_character_),
        stringsAsFactors = FALSE
    )
    text <- capture.output(
        epwshiftr:::epwshiftr_cli_render_table(
            rows,
            title = "Progress table",
            columns = c("status", "filename", "bytes_done", "size", "attempts", "last_error")
        ),
        type = "message"
    )

    expect_true(any(grepl("Progress", text)))
    expect_true(any(grepl("\\[[#-]+\\]", text)))
})
# }}}
