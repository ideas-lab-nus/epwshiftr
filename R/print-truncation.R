# Render the shared footer used when a print method displays only part of an
# object.
print__truncation_footer <- function(x, n, newline_before = is.data.frame(x)) {
    # Remove CLI's default body indentation so the footer aligns with the
    # surrounding dictionary or query-result content.
    div <- cli::cli_div(theme = list(
        body = list(`padding-left` = 0L, `margin-left` = 0L)
    ))
    total <- if (is.data.frame(x)) nrow(x) else length(x)
    if (n < total) {
        if (newline_before) {
            cli::cli_text()
        }
        cli::cli_text(cli::col_grey(
            "# ... with {total - n} more item{?s}"
        ))
    }
    cli::cli_end(div)
}
