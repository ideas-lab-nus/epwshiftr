# nocov start
# reference: https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
.onLoad <- function(libname, pkgname) {
    S7::methods_register()
    registerS3method("as.data.table", "epwshiftr::ShiftStage", shift_stage_as_data_table, envir = asNamespace("data.table"))
    # S7 refreshes the package method table during load. Re-register the
    # dependency-free byte vector methods afterwards so base format()/print()
    # keep the established human-readable EsgResult size contract.
    registerS3method("format", "epwshiftr_bytes",
        format.epwshiftr_bytes, envir = asNamespace("base"))
    registerS3method("print", "epwshiftr_bytes",
        print.epwshiftr_bytes, envir = asNamespace("base"))
    cache__configure(pkgname)
    # Register standalone signal methods at load time so downstream component
    # pipelines can resolve them without constructing a complete EPW recipe.
    bias__register_linear_scaling_component()
    bias__register_delta_change_component()
    qm__register_component()
    qdm__register_component()
    sdm__register_component()
    cdft__register_component()
    edcdf__register_component()
    isimip__register_component()

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
