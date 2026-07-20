# nocov start
# reference: https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
.onLoad <- function(libname, pkgname) {
    S7::methods_register()
    registerS3method("as.data.table", "epwshiftr::ShiftStage", shift_stage_as_data_table, envir = asNamespace("data.table"))
    cache__configure(pkgname)

    # set package options
    .opts <- list(
        "epwshiftr.verbose" = FALSE,
        "epwshiftr.progress" = interactive(),
        "epwshiftr.threshold_alpha" = 3,
        # TRUE = normal caching, FALSE = no caching, "offline" = cache-only (no network)
        "epwshiftr.cache" = TRUE,
        "epwshiftr.dir_store" = store_normalize_path(tools::R_user_dir("epwshiftr", "data")),
        "epwshiftr.dir_cache" = store_normalize_path(tools::R_user_dir("epwshiftr", "cache"))
    )
    missing <- setdiff(names(.opts), names(options()))
    if (length(missing)) {
        do.call(options, .opts[missing])
    }

    invisible()
}
# nocov end
