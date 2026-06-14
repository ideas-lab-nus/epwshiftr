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
#' * `epwshiftr.dir_store`: The persistent store directory for query snapshots,
#'   dictionaries, source mirrors, downloads, extraction results, outputs, and
#'   the store manifest. If not set, [tools::R_user_dir()] with type `"data"`
#'   will be used.
#' * `epwshiftr.cache`: Controls caching behavior. `TRUE` enables normal
#'   caching (default), `FALSE` disables caching entirely, and `"offline"`
#'   enables offline mode where only cached data is used and no network
#'   requests are made. Default: `TRUE`
#' * `epwshiftr.dir_cache`: The directory for disposable cache entries. Deleting
#'   this directory can require re-fetching or re-parsing data, but should not
#'   invalidate a persistent store.
#'
#'
#' @include utils.R
#' @author Hongyuan Jia
## usethis namespace: start
#' @importFrom checkmate assert_count
#' @importFrom cli cli_rule
#' @importFrom data.table :=
#' @importFrom data.table data.table
#' @importFrom eplusr read_epw
#' @importFrom jsonlite fromJSON
#' @importFrom mirai daemons mirai
#' @importFrom psychrolib GetTDewPointFromRelHum
#' @importFrom psychrolib SetUnitSystem
#' @importFrom R6 R6Class
#' @importFrom RNetCDF utcal.nc
#' @importFrom units set_units
#' @importFrom utils flush.console head menu tail
#' @importFrom S7 convert
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
## usethis namespace: end
"_PACKAGE"

# package internal environment
this <- new.env(parent = emptyenv())
this$index_db <- NULL
this$dicts <- new.env(parent = emptyenv())
this$data_max_limit <- 10000L
