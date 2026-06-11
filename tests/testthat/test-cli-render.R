test_that("epwshiftr_cli renders stable boxed tables", {
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

test_that("epwshiftr_cli snapshots narrow table adaptation", {
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

test_that("epwshiftr_cli renders selected query search columns", {
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
    expect_equal(context$columns, c("source_id", "variable_id"))

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
