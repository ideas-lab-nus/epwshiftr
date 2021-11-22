get_cache <- function(path = Sys.getenv("EPWSHIFTR_CHECK_CACHE", NULL)) {
    if (is.null(path)) path <- tools::R_user_dir("epwshiftr", "cache")

    cache <- normalizePath(path, mustWork = FALSE)
    if (identical(Sys.getenv("NOT_CRAN"), "true") && !dir.exists(cache)) {
        dir.create(cache, recursive = TRUE)
    }
    cache
}
