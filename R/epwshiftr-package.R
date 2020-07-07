#' epwshiftr: Create future EnergyPlus Weather files using CMIP6 data
#'
#' Query, download climate change projection data from the [CMIP6 (Coupled Model
#' Intercomparison Project Phase 6) project](https://pcmdi.llnl.gov/CMIP6) in
#' the [ESGF (Earth System Grid Federation) platform](https://esgf.llnl.gov),
#' and create future [EnergyPlus](https://energyplus.net) Weather (EPW) files
#' adjusted from climate changes using data from Global Climate Models (GCM).
#'
#' @section Package options:
#'
#' * `epwshiftr.verbose`: If `TRUE`, more detailed message will be printed.
#'   Default: `FALSE`.
#' * `epwshiftr.dir`: The directory to store package data, including CMIP6
#'   model output file index and etc. If not set, the current user data
#'   directory will be used.
#'
#' @include utils.R
#' @author Hongyuan Jia
"_PACKAGE"

# package internal environment
EPWSHIFTR_ENV <- new.env(parent = emptyenv())
EPWSHIFTR_ENV$index_db <- NULL

# set package options
# reference: https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
.onLoad <- function (libname, pkgname) {
    .opts  <-  c("epwshiftr.verbose" = "FALSE")
    for (i in setdiff(names(.opts), names(options()))) {
        eval(parse(text = paste0("options(",i,"=",.opts[i],")")))
    }
    invisible()
}
