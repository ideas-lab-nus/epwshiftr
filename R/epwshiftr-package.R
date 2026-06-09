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
#' @importFrom fst write_fst
#' @importFrom future.apply future_lapply
#' @importFrom jsonlite fromJSON
#' @importFrom mirai daemons mirai
#' @importFrom progressr with_progress
#' @importFrom psychrolib GetTDewPointFromRelHum
#' @importFrom psychrolib SetUnitSystem
#' @importFrom R6 R6Class
#' @importFrom RNetCDF utcal.nc
#' @importFrom units set_units
#' @importFrom utils menu
#' @importFrom S7 convert
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
## usethis namespace: end
"_PACKAGE"

# package internal environment
this <- new.env(parent = emptyenv())
this$index_db <- NULL
this$dicts <- new.env(parent = emptyenv())
this$cache <- NULL
this$data_max_limit <- 10000L

#' Get the package-level `DiskCache` instance
#'
#' Lazily creates the cache on first access using options for configuration.
#'
#' @return A `DiskCache` instance.
#' @noRd
get_cache <- function() {
    if (is.null(this$cache)) {
        cache_dir <- getOption(
            "epwshiftr.dir_cache",
            tools::R_user_dir("epwshiftr", "cache")
        )
        this$cache <- DiskCache$new(
            dir = cache_dir,
            max_size = getOption("epwshiftr.cache_max_size", 1024^3),
            max_age = getOption("epwshiftr.cache_max_age", 30 * 60),
            max_n = getOption("epwshiftr.cache_max_n", Inf)
        )
    }
    this$cache
}

#' Set or replace the package-level `DiskCache` instance
#'
#' @param cache A `DiskCache` instance to use as the package cache.
#'
#' @return The previous cache instance (invisibly), or `NULL` if none was set.
#' @noRd
set_cache <- function(cache) {
    old <- this$cache
    this$cache <- cache
    invisible(old)
}

#' Reset the package-level cache
#'
#' Sets the internal cache reference to `NULL`, so the next call to
#' `get_cache` will create a fresh `DiskCache` instance.
#'
#' @return `NULL` (invisibly).
#' @noRd
reset_cache <- function() {
    this$cache <- NULL
    invisible(NULL)
}
