# epwshiftr (development version)

## Major changes

* `match_coord()` has been refactored to correct the calculation method of
  geographical distance. Previously, epwshiftr assumes that distance on
  longitude and latitude is the same which is not true. Now it uses a spheroid
  formula to calculate the tunnel distance (#39). For details, please see [Tunnel
  Distance](https://en.wikipedia.org/wiki/Geographical_distance#Tunnel_distance)
  . The structure of the returned `epw_cmip6_coord` object has also changed.
  The `coord` column in the `coord` `data.table` is also a `data.table` which
  contains 6 columns describing the matched coordinates:
  * `index`: the indices of matched coordinates
  * `ind_lon`, `ind_lat`: The value indices of longitude or latitude in the
    NetCDF coordinate grids. These values are used to extract the corresponding
    variable values
  * `lon`, `lat`: the actual longitude or latitude in the NetCDF coordinate
    grids
  * `dist`: the distance in km between the coordinate values in NetCDF and input
    EPW

  Besides, the usage of the input `threshold` and `max_num` has been changed a
  little bit:

  - `threshold`: Due to the change in distance calculation, the meaning of the
    `threshold` input has been changed.  Instead of directly being used to get
    the 'closest' grid points in NetCDF, the longitude and latitude threshold
    are only used to help exclude grid points that are definitely too far away
    from the target location. The default threshold, which is 1 degree for both
    longitude and latitude, is still reasonable for common use cases and is kept
    unchanged.  Also `threshold` now can be set to `NULL`. In this case, the
    distances between the target location and all grid points will be
    calculated. But this may be only useful for rare cases.
  - `max_num`: Now the value `max_num` is the key input to control how many grid
    points to be matched. The points will always be ordered in a descending
    order in terms of the distances.
* The `data` in the returned value of `extract_data()` has been updated to
  include a new column `dist` which gives the spherical distance in km between
  EPW location and grid coordinates (#39). Also the document of `extract_data()`
  has been fixed (#29).

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
* Now `morphing_epw()` can correctly fall back to use "Shift" method when any
  missing values are detected in maximum and minimum prediction values of
  climate variables (#25).
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
