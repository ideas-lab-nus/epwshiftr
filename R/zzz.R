# nocov start
# reference: https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
.onLoad <- function(libname, pkgname) {
    S7::methods_register()

    # set package options
    .opts <- list(
        "epwshiftr.verbose" = FALSE,
        "epwshiftr.threshold_alpha" = 3,
        # TRUE = normal caching, FALSE = no caching, "offline" = cache-only (no network)
        "epwshiftr.cache" = TRUE,
        "epwshiftr.cache_dir" = tools::R_user_dir("epwshiftr", "cache")
    )
    missing <- setdiff(names(.opts), names(options()))
    if (length(missing)) do.call(options, .opts[missing])

    # install IEC style Byte units
    IEC <- c("Byte", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
    big <- IEC[-1L]
    small <- IEC[-length(IEC)]
    for (i in seq_along(big)) {
        try(units::install_unit(big[[i]], sprintf("1024 %s", small[[i]])), silent = TRUE)
    }

    invisible()
}
# nocov end
