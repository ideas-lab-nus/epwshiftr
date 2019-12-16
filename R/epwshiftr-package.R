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
#'
#' @author Hongyuan Jia
"_PACKAGE"

# set package options
# reference: https://github.com/Rdatatable/data.table/blob/master/R/onLoad.R
.opts  <-  c("epwshiftr.verbose" = "FALSE")
for (i in setdiff(names(.opts), names(options()))) {
    eval(parse(text = paste0("options(",i,"=",.opts[i],")")))
}
