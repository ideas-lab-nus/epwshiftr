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
freshness <- rawvignette::check_raw_vignettes()

if (is.data.frame(freshness) && any(freshness$status != "fresh")) {
    stale <- freshness$name[freshness$status != "fresh"]
    stop(
        "Raw vignette output is stale: ",
        paste(stale, collapse = ", "),
        call. = FALSE
    )
}
