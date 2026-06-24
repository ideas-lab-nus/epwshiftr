if (!file.exists("DESCRIPTION")) {
    stop("Run this script from the package root.", call. = FALSE)
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install pkgload before rendering raw vignettes.", call. = FALSE)
}

if (!requireNamespace("rawvignette", quietly = TRUE)) {
    stop(
        "Install rawvignette before rendering raw vignettes:\n",
        "  pak::pak(\"matthewkling/rawvignette\")",
        call. = FALSE
    )
}

pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

rawvignette::precompile_raw_vignettes()

article_paths <- list.files("vignettes/articles", pattern = "[.]Rmd$", full.names = TRUE)
trim_trailing_whitespace <- function(path) {
    lines <- readLines(path, warn = FALSE)
    trimmed <- sub("[ \t]+$", "", lines)
    if (!identical(lines, trimmed)) {
        writeLines(trimmed, path, useBytes = TRUE)
    }
}
invisible(lapply(article_paths, trim_trailing_whitespace))

freshness <- rawvignette::check_raw_vignettes()

if (is.data.frame(freshness) && any(freshness$status != "fresh")) {
    stale <- freshness$name[freshness$status != "fresh"]
    stop(
        "Raw vignette output is stale: ",
        paste(stale, collapse = ", "),
        call. = FALSE
    )
}

render_errors <- lapply(article_paths, function(path) {
    lines <- readLines(path, warn = FALSE)
    index <- grep("^#> Error", lines)
    if (!length(index)) {
        return(NULL)
    }
    data.frame(
        file = path,
        line = index,
        text = lines[index],
        stringsAsFactors = FALSE
    )
})
render_errors <- do.call(rbind, render_errors[lengths(render_errors) > 0L])

if (!is.null(render_errors) && nrow(render_errors)) {
    details <- sprintf(
        "%s:%d: %s",
        render_errors$file,
        render_errors$line,
        render_errors$text
    )
    stop(
        "Rendered raw vignette output contains errors:\n",
        paste(details, collapse = "\n"),
        call. = FALSE
    )
}
