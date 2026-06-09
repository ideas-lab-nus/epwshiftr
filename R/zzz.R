# nocov start
# reference: https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
.onLoad <- function(libname, pkgname) {
    S7::methods_register()

    # set package options
    .opts <- c(
        "epwshiftr.verbose" = "FALSE",
        "epwshiftr.threshold_alpha" = "3",
        # TRUE = normal caching, FALSE = no caching, "offline" = cache-only (no network)
        "epwshiftr.cache" = "TRUE",
        "epwshiftr.dir_store" = sprintf('"%s"', normalizePath(tools::R_user_dir("epwshiftr", "data"), winslash = "/")),
        "epwshiftr.cache_dir" = sprintf('"%s"', normalizePath(tools::R_user_dir("epwshiftr", "cache"), winslash = "/"))
    )
    for (name in setdiff(names(.opts), names(options()))) {
        eval(parse(text = sprintf("options(%s = %s)", name, .opts[name])))
    }

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
