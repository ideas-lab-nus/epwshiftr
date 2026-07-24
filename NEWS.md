# epwshiftr (development version)

## Breaking changes

* Standalone `shift_*()` stages now carry their persisted `run_id` and
  `step_id` into the next stage automatically. The public API does not expose a
  workflow session/current-session object. Intermediate runs are `waiting`,
  final EPW exports complete automatically, and `shift_complete()` explicitly
  closes a workflow that intentionally stops at an intermediate artifact.
  Logical `progress` arguments on `shift_datasets()` and `shift_collect()` were
  removed in favour of `ui = shift_ui(progress = ...)` (#126).

* Replaced the decomposed high-level future-EPW arguments with the
  task-oriented `shift_future_epw(epw, climate, periods, method, dir, control,
  store, dry_run)` interface. Future model, scenarios, member, grid, frequency,
  and discovery constraints now form one complete `ShiftCmip6Spec` created by
  `shift_cmip6()`. Morphing methods are explicit `ShiftMorphMethod` objects.
  `belcher(reference = historical_reference(...))` is the recommended path
  when matching historical CMIP6 data are available. `belcher()` remains a
  fallback that uses the baseline EPW climatology; `NULL` never infers a
  historical request.
* Removed the `eplusr`, `psychrolib`, and `units` runtime dependencies. EPW files
  are parsed and written directly, while objects inheriting from `Epw` remain
  accepted through a dependency-free conversion to the internal `EpwFile`.
  The required ASHRAE saturation-pressure and dew-point equations are
  implemented internally, and climate-value conversion uses explicit supported
  unit pairs.
* Replaced the extraction `nearest` count option with explicit grid extraction
  methods: `"nearest"`, `"idw"`, `"bilinear"`, and `"mean"` (#123).
* Replaced the legacy data.table-oriented workflow with the new store-native
  workflow. Users who need `v0.1.4` behavior should use the `legacy` branch on
  GitHub or install epwshiftr `v0.1.4`.
* Removed the legacy `init_cmip6_index()`, `summary_database()`,
  `match_coord()`, `extract_data()`, `morphing_epw()`, `future_epw()`,
  `EsgfQuery`, `esgf_query()`, and related helpers. Use `shift_*`,
  `EsgStore`, `EpwMorpher`, `EsgQuery`, and `data_node_status()` instead.
* Dropped the old `fst`, `future.apply`, `progressr`, `pingr`, `PCICt`, and
  `rappdirs`-based implementation paths; the new implementation uses DuckDB,
  `mirai`, S7, and store-managed manifests.

## New features

* Added explicit future-weather input roles and reusable `preprocess`,
  `calendar`, `signal`, `sequence`, `hourly`, `physics`, and `output`
  component contracts. Registered components declare their input requirements,
  intermediate data kinds, dimensional scope, stochastic behavior, settings,
  provenance, and diagnostics while existing `EpwMorphBackend` runners remain
  compatible (#143).

* Added the built-in `daily_temperature()` method for complete future-EPW
  workflows using daily future and historical CMIP6 temperature data. It
  estimates circular daily `tas` changes, constrains each hourly profile to
  daily mean/minimum/maximum targets when paired `tasmin` and `tasmax` are
  available, and otherwise inherits the baseline daily range explicitly.
  Baseline specific humidity is retained subject to saturation, relative
  humidity and dew point are recomputed, and hourly numerical and physical
  diagnostics remain available in the persisted result (#141).

* Added calendar-neutral daily temperature targets and constrained 24-hour
  temperature projection. Matching future and historical `tas`, `tasmin`, and
  `tasmax` climatologies provide daily mean, minimum, maximum, and DTR changes.
  Feasible target statistics are closed while hourly order and extrema timing
  are preserved; missing extrema and flat template days use explicit, reported
  fallbacks (#139).

* Added calendar-neutral circular daily-climatology primitives. Odd 21-, 31-,
  and 61-day windows operate on `annual_phase` and map 360-, 365-, and 366-day
  inputs onto a common target grid without date pairing (#137).

* Added `shift_cmip6_avail()` to discover all CMIP6
  model/member/grid identities that provide every requested Dataset variable
  across future scenarios and optional historical data. The query accepts
  `frequency`, `table_id`, and `variable_id` constraints, accepts known ESGF
  node names including the ORNL/LLNL Bridge, and reports incomplete identities
  without downloading NetCDF payloads (#127, #133).

* Belcher morphing now defaults to the `"enhanced"` profile. It uses guarded
  combined temperature morphing with cyclic month-boundary smoothing, a
  case-wide specific-humidity state path when complete `huss + tas + ps` data
  are available, integrated EPW solar geometry, RBL diffuse radiation, Perez
  illuminance, optional `snd` scaling, and recalculated ground-temperature and
  typical/extreme-period headers. Use `profile = "legacy"` for compatible
  pre-enhancement numerical results and EPW headers, and configure policies with
  `belcher_options()` (#126).

* `shift_cmip6(table = NULL)` now resolves exact variable/table/grid
  partitions. Atmospheric inputs remain in `Amon`, `snd` is discovered in
  `LImon`, and future/reference optional inputs are retained only when both
  cases provide a matching partition. Scalar tables still pin every variable;
  named vectors override individual variables.

* Added `store_reset()` for deliberately incompatible store schemas. Existing
  stores are moved to timestamped same-filesystem backups by default;
  permanent removal requires `backup = FALSE, force = TRUE`. Reset targets are
  validated before any filesystem change, and schema mismatch errors now show
  a directly runnable recovery command.

* Generalized the Future EPW reporter and run store across standalone Dataset
  inspection, collect, download, extract, morph, EPW-write, and export tasks.
  Each invocation records an ordered step and foreground job; the latest
  returned stage continues the run, while stale or terminal inputs fork a child
  lineage. `shift_datasets()` preserves its `EsgResultDataset` return value while
  carrying run coordinates as lightweight attributes. Background download
  sessions remain `running` and synchronize their completion, cancellation, and
  logs through the same run ID. `shift_result()` rebuilds the latest successful
  stage, and CLI extract/morph/download commands expose `run_id` and `step_id`
  without mixing progress text into quiet, JSON, or JSONL output. Store schema
  2.8 records these steps and deliberately requires a new store instead of
  migrating older manifests.

* `ShiftRequest` now follows `EsgQuery`'s double-rule query receipt and canonical
  parameter renderer. `ShiftFiles` preserves the established
  `EsgResultFile` double-rule and bullet-summary header while adding a compact,
  terminal-width-aware CMIP6 preview. It reads only the requested preview rows
  from the store and accepts `n`, `width`, and `verbose` print controls; the
  complete catalog remains available through `shift_files()` or
  `data.table::as.data.table()`.

* Added a shared semantic print design for every public Shift configuration and
  stage object. Receipts use double-rule titles, bullet facts, bounded
  width-aware tables, compact periods and references, and common `n`, `width`,
  and `verbose` controls. `ShiftRun` prints a refreshed, motion-free snapshot
  through the same dashboard view used by foreground runs and `shift_watch()`;
  an unavailable store now falls back to the cached snapshot with a diagnostic
  instead of failing the print method.

* Added `shift_ui()` and a unified workflow reporter with an in-place plan
  summary; a responsive stage rail, one animated current operation,
  stage-specific measured progress, terse resolver attempts, and a two-item
  activity feed; aggregate download bytes, speed, ETA, and active files; and
  ordered `normal`, `detail`, and `debug` output. Resolver failover no longer
  presents node attempts as a workflow percentage. ANSI terminals paint the
  complete dashboard atomically through a cli-capability-aware framebuffer,
  add a quiet panel border with labelled workflow and activity sections at 60
  columns and above, reserve the final terminal column against autowrap, and
  reflow plan, current operation, status, and diagnosis fields at semantic
  boundaries instead of clipping them. Flow falls back from the complete rail
  to current-plus-next and current-only forms as space contracts. Semantic
  colours remain reserved for state instead of ordinary plan values. Narrow
  terminals retain the same content without decorative chrome. Constrained dynamic IDE
  consoles use one compact cli status row. Failed runs now commit their final
  panel to terminal scrollback and replace repeated resolver exceptions with a
  structured diagnosis: attempt counts, one cause, the closest CMIP6 identity,
  and the first missing requirement. Recovery text distinguishes transient
  retryable failures from scientific coverage failures and omits the default
  store path from copyable commands. Title-like dashboard labels now use a
  consistent bold accent, while failure labels use the danger colour and
  secondary labels remain quiet. `motion` and `refresh` control presentation
  independently from durable job heartbeats, and `shift_watch()` advances
  cached animation frames without increasing store polling frequency. Reporter
  milestones remain structured run events while throttled sidecars carry
  transient state without recording animation-only events. CI, `TERM=dumb`,
  redirected, and captured output use complete append-only logs.
* Added detached Future EPW jobs with live `ShiftRun` refresh,
  `shift_watch()`, `shift_cancel()`, and `shift_logs()`. Background attempts
  retain their PID, heartbeat, log, cancellation, and terminal state, while a
  lock-free live snapshot keeps watch/cancel responsive when the worker owns
  DuckDB's process lock. `shift_logs()` identifies process-log and persisted-
  event rows, and CLI JSONL follow emits typed snapshot, event-delta, gap, and
  terminal records.
* Added persisted `ShiftRun` workflow records, expected-case state and events,
  plus `shift_runs()`, `shift_run_get()`, `shift_cases()`, `shift_missing()`, and
  `shift_resume()` for run-ID-based inspection and cross-session recovery.
* Added typed `shift_cmip6()` and `shift_control()` configuration,
  complete future/reference CMIP6 resolution, strict expected-case coverage,
  and explicit partial-output policy.
* Added the store-native `shift_request()` -> `shift_collect()` ->
  `shift_download()` -> `shift_extract()` -> `shift_morph()` -> `shift_epw()`
  workflow, with inspection helpers such as `shift_status()`,
  `shift_diagnostics()`, `shift_coverage()`, `shift_outputs()`, and
  `shift_data()`.
* Added `EsgStore`, a DuckDB-backed local store for query snapshots,
  dictionaries, source files, downloads, Parquet extracts, and generated EPW
  outputs.
* Reworked ESGF querying around `esg_query()` / `EsgQuery` and typed
  `EsgResult*` objects for Dataset, File, and Aggregation records.
* Added `EsgDataset` for remote OPeNDAP NetCDF access without downloading full
  files.
* Rebuilt ESG dictionaries as project-aware `EsgDict` objects with option
  discovery and legality checks across CMIP6 and related ESG projects.
* Added persistent downloader support, including download planning, resume,
  verification, node health, background jobs, daemon mode, and event logs.
* Added `EpwMorpher`, morphing recipes, backend registration, historical
  reference handling, resumable morphing, and manifest-backed EPW output
  registration (#111, #115).
* Belcher EPW downscaling now supports CMIP6 precipitation (`pr`) and includes it
  in the recommended morphing variable set (#122).
* Added the `epwshiftr` CLI with `doctor`, `query`, `download`, `storage`,
  `shift`, `extract`, `morph`, and `esgf` command groups (#114).

## Bug fixes

* Point extraction now retains canonical CF calendar coordinates and annual
  phase, uses them for non-Gregorian range selection and yearly Parquet
  partitions, and prefers them in morphing summaries with legacy artifact
  fallback. Multi-year extraction partitions no longer duplicate every row
  into every year (#134, #135).

* CMIP6 coverage checks now keep requested variable and grid values distinct
  from same-named convenience columns returned by ESGF providers, preventing
  complete catalog identities from being rejected (#130, #132).

* ESGF result persistence now recognizes the current bridge-provided
  `datetime_stop`, `geo`, and `mod_time` document fields. Nodes that ignore an
  explicit `fields` request no longer fail while saving otherwise valid
  Dataset or File metadata (#129, #131).

* Humidity fallback now skips resolved CMIP table partitions that do not
  contain complete `huss`, `tas`, and `ps` extraction rows, avoiding false
  derivation attempts for optional partitions such as `LImon` `snd` (#126).

* `shift_request()` now preserves provider facet values such as `project` and
  `frequency` exactly for EsgDict validation instead of silently translating
  non-standard aliases. A bare numeric `limit` now caps Dataset results instead
  of silently becoming a pagination size. Empty collections are reported as
  `partial` rather than as ready for extraction, while successful intermediate
  receipts display `READY` instead of the internal state-machine term
  `waiting`.

* Standalone dynamic `shift_*()` stages now translate successful `waiting` and
  detached `running` workflow states to the framebuffer's `done` terminal
  outcome. This prevents a completed `shift_collect()` from failing while its
  final dashboard receipt is committed.

* Belcher CMIP6 resolution now keeps `hurs` as the canonical humidity input but
  can satisfy it from complete `huss`, `tas`, and surface-pressure (`ps`) data
  when direct relative humidity is unavailable. The derived `hurs` values use
  the package's ASHRAE saturation-pressure equation, are persisted as normal
  extraction plans and Parquet artifacts with source-plan provenance, and are
  reused on resume. Mean sea-level pressure (`psl`) is never substituted for
  surface pressure. High-level Future EPW plans also reject equal or nested
  delivery/store paths so internal manifests, catalogs, and logs cannot enter
  the exported-EPW directory.
* Future-EPW collection now preserves ESGF File time metadata and fills missing
  ranges from CMIP/DRS filenames before storing or filtering records. Resolver
  coverage also repairs older cached catalogs defensively, preventing complete
  member/grid combinations from being rejected as if every requested year were
  absent. Closest-identity diagnostics prefer identities present in the future
  catalog, and a mixed set of coverage failures plus one timed-out mirror no
  longer presents an unconditional `Retry` action.
* Historical-reference discovery no longer turns requested calendar years into
  exact ESGF Dataset end timestamps. This keeps complete monthly datasets whose
  metadata ends on a representative December date (for example December 16)
  eligible for resolution; the requested years are still enforced during
  candidate selection, extraction, and coverage checks. Empty reference
  catalogs now report a dedicated diagnostic before member/grid matching.
* Future-EPW progress now degrades to readable line logs when a dynamic Console
  region cannot be recreated, keeps the current business-unit label in sync,
  freezes elapsed time for every terminal run state, and preserves every unseen
  event between R or CLI watch polls. Expected resolver-node rejection no longer
  creates an error diagnostic, and foreground runs expose their durable events
  through `shift_logs()`. Extraction fallback downloads now share the workflow
  reporter instead of opening competing native progress bars; remote NetCDF
  reads remain visibly alive through a polled worker and fall back to synchronous
  I/O when that worker cannot be launched.
* Successful dynamic runs now commit a responsive final receipt to terminal
  scrollback instead of erasing the dashboard and leaving only two summary
  lines. Its Results section retains completion counts, the delivery directory,
  and width-wrapped output filenames; compact and log renderers keep their
  append-only completion summary.
* Fixed precipitation morphing summaries to avoid carrying the removed legacy
  `dist` extraction column after grid extraction methods became explicit (#124).
* `EsgStore` now keeps ESGF query collection and downloader operations outside
  the store manifest lock, reducing lock hold time during query update and
  download workflows (#120).
* `EsgQuery$collect(all = TRUE)` now warns and returns partial results when
  ESGF pagination stops making progress, instead of repeatedly requesting the
  same offset (#116).
* `shift_epw()` and `EpwMorpher$write_epw()` now fill missing, out-of-range,
  and special EPW values before saving generated weather files (#87).

## Documentation

* Rewrote the README around the recommended `shift_*` workflow and the
  store-native migration path.
* Added migration documentation mapping legacy workflow steps to the new
  store-native replacements.
* Split and refreshed vignettes for ESGF query results, dictionaries, stores,
  downloader usage, EPW morphing, CLI usage, troubleshooting, and future EPW
  workflows.
* Expanded the development changelog so the release notes reflect the
  store-native workflow overhaul and migration path (#121).

## Internal changes

* Added regression tests for batched Dataset child collection, including
  global `limit` handling and progress labels (#119).
* Updated GitHub Actions workflow dependencies for current Actions behavior
  (#99).
* Removed obsolete print helpers and the legacy implementation files (#109,
  #110).
* `EsgResultFile` and `EsgResultAggregation` now share their internal download
  and OPeNDAP fallback helpers while keeping the public methods unchanged
  (#118).
* Removed the obsolete testthat start-order override now that tests no longer
  depend on shared cache side effects (#117).

# epwshiftr 0.1.4

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
    is only used to help exclude grid points that are definitely too far away
    from the target location. The default threshold, which is 1 degree for both
    longitude and latitude, is still reasonable for common use cases and is kept
    unchanged.  Also `threshold` now can be set to `NULL`. In this case, the
    distances between the target location and all grid points will be
    calculated. But this may be only useful for rare cases.
  - `max_num`: Now the value `max_num` is the key input to control how many grid
    points are to be matched. The points will always be ordered in descending
    order in terms of the distances.
* The `data` in the returned value of `extract_data()` has been updated to
  include a new column `dist` which gives the spherical distance in km between
  EPW location and grid coordinates (#39).
* The document on the return value structure for `extract_data()` and
  `morphing_epw()` has been fixed (#29). And the column order for all metadata
  in the returned `data.table` from `extract_data()` and `morphing_epw()` are
  not consistent. The columns will always be in the order below (#45):
  - `activity_drs`
  - `institution_id`
  - `source_id`
  - `experiment_id`
  - `member_id`
  - `table_id`
  - `lon`
  - `lat`
  - `dist`

## New features

* A new parameter `full` is added to `future_epw()`. When setting to `TRUE`, 
  a `data.table` containing information about how the
  data are split by the `by` argument and also the generated future EPWs and
  their paths are returned (#18).
* Now `summary_database()` can append results to the previous scan and detect if
  any previously matched NetCDF files do not exist. It stores the metadata of
  those missing files as a new attribute `not_found` in the results. Warnings
  are issued if `warning` is set to `TRUE`. Also, a new parameter `miss` has
  been added to control how to manage those invalid entries. You can set it to
  `"keep"`, which is the default, to do nothing about it or `"overwrite"` to
  overwrite those entries based on the newly matched NetCDF files if possible
  (#40).
* `extract_data()` now supports non-standard calendar, e.g. `360_day` (#32).
* A new parameter `warning` has been added in `morphing_epw()`. If set to `TRUE`,
  warnings will be issued for cases with input data less than a decade (10 years)
  . This is because using data that only covers a short period of time may not
  be able to capture the average of future climate (#41).
* Now epwshiftr is able to download, parse and store CMIP6 Controlled
  Vocabularies (CVs) and Data Request data using the `EsgDict` class. Please
  see `?EsgDict` for details (#53).
* A new option `epwshiftr.threshold_alpha` has been added to set the threshold
  of the absolute value for alpha, i.e. monthly-mean fractional change when
  performing morphing operations. The default value is set to `3`. If the
  morphing methods are set `"stretch"` or `"combined"`, and the absolute alpha
  exceeds the threshold value, warnings are issued and the morphing method is
  fallbacked to `"shift"` to avoid unrealistic morphed values (#54).
* Now HDF5 format is supported (#60).
* Now `replica` can be `NULL` in `esgf_query()` and `init_cmip6_index()`. In
  this case, both the master record and replicas are all returned (#61).
* New class `EsgfQuery` is added to support more flexible APIs for ESGF search
  services. Please see `?EsgfQuery` for details (#63, #69).

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
* Now `get_data_node()` works again (#80)

## Internal refactor

* `fields` parameter is used to directly filter the ESGF query responses (#66).
* Improve URL encoding (#62).
* Update minimal version of depended packages (#75).

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
