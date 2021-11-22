# epwshiftr (development version)

## New features

* A new parameter `full` is added to `future_epw()`. When setting to `TRUE`, 
  a `data.table` containing information about how the
  data are split by the `by` argument and also the generated future EPWs and
  their paths are returned (#18).

## Bug fixes

* Fix the error in `summary_database()` when no NetCDF files are found in the
  input directory (#25).
* Fix the error about overwriting temporary EPW file when `epw` in `match_coord() `
  is a search string (#25).
* Fix the wrong warning messages when `combined` method is used in
  `morphing_epw()` (#25).

# epwshiftr 0.1.3

## Minor changes

* Remove `LazyData` in `DESCRIPTION` (#16).

# epwshiftr 0.1.2

## New features

* The type of morphing methods can be specified using the newly added `methods`
  argument in `morphing_epw()`.

## Bug fixes

* `esgf_query()` can accept `NULL` `resolution` argument (#12).
* `load_cimp6_index()` can be correctly load local CMIP6 output index file when
  `force` is `TRUE` (#8).

# epwshiftr 0.1.1

## Bug fixes

* `esgf_query()` will give an informative message when LLNL ESGF node is not
  available (#3).
* `extract_data()` will automatically detect input NetCDF dimensions (#6).
* `summary_database()` now will proceed when no matched found (#6).

# epwshiftr 0.1.0

* Initial CRAN version
