#' epwshiftr: Create future EnergyPlus Weather files using CMIP6 data
#'
#' Query, download climate change projection data from CMIP6 (Coupled Model
#' Intercomparison Project Phase 6) project and create futher EnergyPlus Weather
#' (EPW) files adjusted from climate changes using data from Global Climate
#' Model (GCM).
#'
#' @section Package options:
#'
#' * `epwshiftr.verbose`: If `TRUE`, more detailed message will be printed.
#'   Default: `FALSE`.
#' * `epwshiftr.data_dir`: The directory to store package data, including CMIP6
#'   model output file index database and etc. If not set, the current user data
#'   directory will be used.
#'
#' @include utils.R
#' @author Hongyuan Jia
"_PACKAGE"

# package internal environment
EPWSHIFTR_ENV <- new.env(parent = emptyenv())
EPWSHIFTR_ENV$data_dir <- .data_dir(init = TRUE)
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
