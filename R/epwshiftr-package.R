#' epwshiftr: Create future EnergyPlus Weather files using CMIP6 data
#'
#' Query, download climate change projection data from the [CMIP6 (Coupled Model
#' Intercomparison Project Phase 6) project](https://pcmdi.llnl.gov/CMIP6/) in
#' the [ESGF (Earth System Grid Federation) platform](https://esgf.llnl.gov),
#' and create future [EnergyPlus](https://energyplus.net) Weather (EPW) files
#' adjusted from climate changes using data from Global Climate Models (GCM).
#'
#' @section Package options:
#'
#' * `epwshiftr.verbose`: If `TRUE`, more detailed message will be printed.
#'   Default: `FALSE`.
#' * `epwshiftr.threshold_alpha`: the threshold of the absolute value for alpha,
#'   i.e. monthly-mean fractional change, when performing morphing operations.
#'   The default value is set to `3`. If the morphing methods are set
#'   `"stretch"` or `"combined"`, and the absolute alpha exceeds the threshold
#'   value, warnings are issued and the morphing method fallbacks to
#'   `"shift"` to avoid unrealistic morphed values.
#' * `epwshiftr.dir`: The directory to store package data, including CMIP6
#'   model output file index and etc. If not set, the current user data
#'   directory will be used.
#'
#' @include utils.R
#' @author Hongyuan Jia
## usethis namespace: start
#' @importFrom checkmate assert_string
#' @importFrom cli cli_rule
#' @importFrom jsonlite parse_json
## usethis namespace: end
"_PACKAGE"

# package internal environment
this <- new.env(parent = emptyenv())
this$index_db <- NULL
this$dict <- NULL
this$cache <- list()
this$data_max_limit <- 10000L

# nocov start
attach_cache <- function(cache) this$cache <- cache
# nocov end
