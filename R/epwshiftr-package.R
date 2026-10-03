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
#' * `epwshiftr.progress`: If `TRUE`, progress bars are shown for long-running
#'   operations that support them. Default: [interactive()].
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
#' * `epwshiftr.mirai_workers`: Maximum concurrent source readers, default `4`.
#'   Set to `1` for serial reads. Single-task and batch workflows share this limit.
#' * `epwshiftr.ui_height`: Optional terminal display height; unset by default.
#' * `epwshiftr.cache_max_size`: Disposable cache capacity in bytes, default 1 GiB.
#' * `epwshiftr.cache_max_age`: Disposable cache lifetime in seconds, default 1800.
#' * `epwshiftr.cache_max_n`: Maximum cache entries, default `Inf`.
#'   Cache limits are applied when the session's cache is first initialized.
#' * `epwshiftr.query.timeout`: Catalog request timeout in seconds, default 300.
#' * `epwshiftr.query.connect_timeout`: Catalog connection timeout in seconds,
#'   default 30.
#'
#' Execution settings are captured when a workflow attempt starts and forwarded
#' to detached processes and source workers. They do not change scientific plan
#' identities. Development and test dependency substitutions are not options.
#'
#'
#' @include utils.R
#' @author Hongyuan Jia
## usethis namespace: start
#' @importFrom checkmate assert_count
#' @importFrom cli cli_rule
#' @importFrom data.table :=
#' @importFrom data.table data.table
#' @importFrom jsonlite fromJSON
#' @importFrom mirai daemons mirai
#' @importFrom R6 R6Class
#' @importFrom RNetCDF utcal.nc
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
