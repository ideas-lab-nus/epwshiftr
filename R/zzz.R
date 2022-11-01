# nocov start
# reference: https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
.onLoad <- function(libname, pkgname) {
    # set package options
    .opts  <-  c(
        "epwshiftr.verbose" = "FALSE",
        "epwshiftr.threshold_alpha" = "3"
    )
    for (name in setdiff(names(.opts), names(options()))) {
        eval(parse(text = sprintf("options(%s = %s)", name, .opts[name])))
    }

    # install IEC style Byte units
    IEC <- c("Byte", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
    big <- IEC[-1L]
    small <- IEC[-length(IEC)]
    for (i in seq_along(big)) {
        try(units::install_unit(big[[i]], sprintf("1024 %s", small[[i]])),
            silent = TRUE
        )
    }

    invisible()
}
# nocov end
