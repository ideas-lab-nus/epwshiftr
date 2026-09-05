# epwshiftr (development version)

## Breaking changes

* Renamed reusable future-weather registry keys, intermediate kinds, BTWS
  diagnostics, and method choices by the algorithms they implement rather than
  by a reference paper or software package. The Arima et al. complete workflow
  remains available through `arima_temperature()`, while its recipe key is now
  `monthly_percentile_temperature` (#165).

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

* Added a shared EPW physical policy layer used by all built-in complete weather
  methods. Method adapters submit `EpwPhysicalRequest` objects and retain their
  paper-faithful or harmonized definitions through explicit
  `EpwPhysicalPolicy` values. The registered `epw_hourly_physical_closure`
  component uses the `absolute_model_fields` policy for mapped direct-model
  years, including humidity and wind alternatives, unit conversion, field
  bounds, shortwave closure, inherited template fields, and typed diagnostics
  (#199). Method-neutral helpers also centralize three-role signal contracts,
  adjusted-result validation, future-year fitting blocks, numeric bounds,
  calendar-neutral circular interpolation and target-day mapping, regular
  hourly lattices, temperature conversion, parametric CDF/quantile dispatch,
  and the shared Spencer solar geometry kernels while retaining each published
  method's equations, temporal conventions, diagnostics, and provenance.

* Added the registered `direct_model_epw_calendar_mapping` hourly component.
  It maps complete hourly direct-model years from every supported native CF
  calendar onto the baseline EPW's fixed 365-day, 8760-row lattice while
  preserving each variable's time-of-day position. Already aligned 365-day
  years remain exact; 360- and 366-day point variables use circular seasonal
  interpolation separately at each daily hour, while interval-mean radiation
  uses normalized conservative remapping separately at each daily hour. Typed
  sequence members retain group identity, source calendars, mapping methods,
  annual-mean error diagnostics, and upstream signal provenance for later
  physical closure and output (#197).

* Added the registered `hourly_weather_interpolation` preprocessing component
  for model inputs that combine point-state variables with interval-mean
  shortwave radiation. It dispatches each variable family to its existing
  linear or solar-projection algorithm, optionally inserts matching daily
  `tasmin` and `tasmax` as source-labelled hourly `tas` anchors using observed
  site/month extreme-hour modes, and records deterministic hour and adjacent-
  pair fallbacks. The merged historical and future roles retain variable-level
  diagnostics, sampling phases, source provenance, and validated regular native-
  calendar hourly coordinates (#195).

* Added the registered `hourly_calendar_grouping` calendar component. It turns
  hourly observed, historical-model, and future-model role inputs into
  univariate signal groups while preserving each role's native CF chronology,
  requiring complete hourly calendar years, and rejecting incompatible units,
  variables, sites, or model identities before statistical adjustment. The
  hourly kernel-density QDM and direct-model sequence contracts now also
  accept the actual preprocessed source and sub-daily intermediate kinds
  needed by this component chain (#193).

* Added the registered `kernel_quantile_delta_mapping_hourly` signal component
  for pre-aligned hourly observations, historical model output, and future
  model output. It defaults to Gaussian kernel-density CDFs in centered
  three-month native-calendar windows, transfers additive quantile changes for
  temperature and pressure or multiplicative changes for wind, humidity, and
  radiation, and retains future-model chronology in a
  `SubdailyAdjustedSeries`. Because the source publication does not report its
  KDE kernel, bandwidth, numerical grid, tails, or zero-denominator behavior,
  the corresponding package defaults and supported alternatives are explicitly
  validated, user-overridable, warned about, and recorded separately from the
  published method settings (#191).

* Added the registered `solar_radiation_interpolation` preprocessing component
  for interval-mean three-hourly `rsds` and `rsdsdiff`. It preserves explicit
  CF time bounds through regional extraction, allocates each source interval
  to hourly means using positive solar projection, conserves the source mean,
  and records weights and diagnostics on every supported native CF calendar.
  Regular six-hourly input is supported as an explicit extension of the
  published three-hourly method (#189).

* Added the registered `linear_temporal_interpolation` preprocessing component
  for regular three- and six-hourly `tas`, `huss`, `hurs`, `ps`, `psl`, `uas`,
  and `vas` inputs. It produces an exact hourly native CF-calendar lattice,
  retains source rows, timestamps, weights, grouping, diagnostics, and
  provenance, and rejects extrapolation and variables requiring dedicated
  temporal methods (#187).

* Added the frequency-aware `AdjustedWeatherSeries` signal-result contract.
  Existing daily methods retain their strict `DailyAdjustedSeries` output,
  while regular sub-daily outputs carry explicit frequency, timestep, and
  calendar-native time-of-day metadata. `direct_model_realization` now
  preserves complete daily or sub-daily source-model years without resampling
  (#185).

* Added the deterministic `direct_model_realization` sequence component. It
  preserves corrected daily `model_future` chronology, partitions complete
  native CF-calendar years without selecting or resampling days, and retains
  group identities and signal provenance for later hourly reconstruction
  (#183).

* Future-weather recipes with `future_year` or `multi_year` output types now
  persist a year-addressable sequence manifest and write one independently
  resumable Parquet and EPW member per weather year. Existing
  `representative_year` outputs and identifiers remain unchanged (#181).

* Weather component pipelines now pass optional variable-specific
  `signal_overrides` to the selected signal component. Reusable downstream
  temperature reconstruction components ignore signal-owned settings while
  continuing to validate their own projection options (#179).

* Added the native R `isimip3basd_daily` signal component for the complete
  ISIMIP3BASD 3.0.x marginal bias-adjustment configuration. It transfers
  modeled quantile changes onto historical observations, then maps the
  future-model sequence through Normal, Gamma, Weibull, or empirical
  distributions with variable-specific additive, mixed, or bounded trend
  preservation. Published profiles cover eight direct variables plus
  `prsnratio`, `tasrange`, and `tasskew`; reconstructing `prsn`, `tasmin`, and
  `tasmax`, and applying MBCnSD spatial downscaling, remain explicit later
  stages. Native CF calendars use the package annual-phase adapter, while
  threshold randomization, missing-ratio imputation, detrending, short-wave
  upper-bound scaling, fitting fallbacks, bound frequencies, seeds, and
  clipping remain inspectable in result provenance (#177).

* Added the native R `equidistant_cdf_matching_daily` signal component for
  `tas` and `pr`. It applies the Li et al. additive equidistant equation using
  four-parameter Beta temperature distributions and mixed dry-mass/Gamma
  precipitation distributions. Although the transfer equation is
  mathematically equivalent to absolute Quantile Delta Mapping, this
  implementation retains the distributions and range convention of the
  original method. Because Li et al. applied it to monthly fields, fitting
  separate distributions from daily values in each native CF-calendar month
  is recorded and warned about as an experimental epwshiftr adaptation.
  Results preserve the future-model sequence and record fitted parameters,
  sample coverage, probability clamping, precipitation dry counts and
  non-negative clipping, resolved settings, and frequency provenance (#175).

* Added the native R `cdf_transform_daily` signal component. It estimates the
  future target CDF through the CDF-t chain
  \(F_{obs,hist}(F^{-1}_{model,hist}(F_{model,future}(x)))\), then quantile
  matches the future-model sequence onto that target. The method uses the
  authors' additive-mean range alignment, explicit empirical-grid and
  constant-correction tail conventions. Following the Famien et al. (2018)
  daily Africa application, native calendar months use 17-year fitting
  windows with disjoint central 9-year output blocks. The package-selected
  boundary policy truncates unavailable four-year flanks and records that
  provenance explicitly. Precipitation uses deterministic Singularity
  Stochastic Removal below \(10^{-8}\) kg m-2 s-1 without changing R's global
  RNG state. Results retain future-model CF-calendar coordinates and record
  window coverage, target-CDF ranges, tail extensions, clipping, SSR seeds,
  and resolved settings for the six variables in that application (#173).

* Added the native R `scaled_distribution_mapping_daily` signal component. It
  implements the published absolute temperature branch with linear detrending,
  Normal fits, and two-tailed recurrence intervals, plus the relative
  precipitation branch with a 0.1 mm/day dry threshold, zero-location Gamma
  fits, expected wet-day adjustment, and one-tailed recurrence intervals.
  Calendar-month groups use the published 30-year future fitting window with
  disjoint 10-year retained blocks and explicit edge truncation. The result
  preserves the future-model CF-calendar sequence and records resolved
  settings, distribution fits, window coverage, clipping, and the method's
  explicit limitation that additional future wet days are not invented
  (#171).

* Added the native R `quantile_delta_mapping_daily` signal component. It uses
  each projected value's time-dependent future-model quantile to transfer the
  corresponding historical-to-future model change onto the observed-reference
  quantile: absolutely for interval variables and relatively for precipitation.
  Calendar-neutral 91-day seasonal windows and centered 31-year future windows
  make both sources of variation explicit. Published precipitation censoring
  uses deterministic role-specific randomization below 0.05 mm/day without
  changing R's global RNG state. The future-model sequence, resolved settings,
  window coverage, quantile changes, bounds, censoring, and provenance are
  retained in the `DailyAdjustedSeries` result (#169).

* Added the native R `quantile_mapping_daily` signal component. It maps
  future-model daily values through historical-model and observed-reference
  empirical distributions in calendar-neutral circular windows, using
  explicit tie, interpolation, tail, bound, and minimum-sample conventions.
  Precipitation uses a mixed dry-day/positive-amount hurdle distribution with
  deterministic group-specific randomization that leaves R's global RNG state
  untouched. Results preserve the future-model CF-calendar sequence and record
  resolved settings, sample coverage, tails, clipping, dry-day changes, and
  provenance. Published method-variable profiles are limited to `tas` and
  `pr`; implementation-selected defaults for other supported variables remain
  experimental (#167).

* Added the native R `delta_change_daily` signal component. It transfers
  historical-to-future monthly mean changes onto the observed-reference daily
  sequence: additively for `tas`, `tasmin`, and `tasmax`, and multiplicatively
  for `pr`. The typed result preserves observed calendar coordinates and
  variability while recording resolved settings, modeled monthly changes,
  bounds, clipping, provenance, and explicit diagnostics (#163).

* Added the package-native `DailyAdjustedSeries` signal contract and registered
  native R `linear_scaling_daily` component. The univariate component consumes
  distinct observed-reference, historical-model, and future-model daily
  series, applies monthly additive temperature or multiplicative precipitation
  mean corrections, and returns the corrected future sequence with canonical
  CF-calendar coordinates, resolved settings, correction provenance, bounds,
  and explicit diagnostics (#161).

* Added `arima_temperature()` and registered the temperature-focused
  `monthly_percentile_temperature` recipe. The workflow carries baseline EPW,
  historical daily model `tas`, future daily model `tas`, and multi-year
  observed daily `tas` as four distinct input roles. It builds month-wise
  historical/future inverse-CDF change functions, applies the published
  endpoint-aware nine-point smoothing three times, locates each baseline daily
  mean in the observed monthly CDF, and adds the selected factor to all 24
  hours. Empirical-CDF conventions and endpoint clamping are recorded
  explicitly; `paper_faithful` preserves baseline humidity fields, while
  `harmonized` applies specific-humidity closure (#157).

* Added `ek_daily_temperature()` and registered the temperature-focused
  `ek_daily_factors` recipe. Matching daily CMIP6 `tasmin` and `tasmax` years
  are mapped from their native calendars to the 365-day EPW phase grid before
  daily mean and DTR change factors are calculated and applied through the Ek
  combined shift-and-stretch equation. The result records the selected
  relative-DTR interpretation, zero-historical-DTR fallback, calendar mapping,
  and source ambiguities. The `paper_faithful` policy preserves baseline
  humidity fields, while `harmonized` applies specific-humidity closure
  (#155).

* Added the temperature-only `eames_temperature()` method and registered
  `eames_monthly_temperature` recipe. Matching daily CMIP6 `tas`, `tasmin`, and
  `tasmax` are aggregated into the monthly mean, average daily minimum, and
  average daily maximum changes used by Eames et al. (2024), then applied
  month-by-month through the BTWS hourly reconstruction. Provenance records the
  substitution of daily CMIP6-derived monthly statistics for the paper's
  UKCP18 factors and states that non-temperature transformations are not
  included (#153).

* Added `reconstruction = "btws"` to `daily_temperature()` and registered the
  `epwshiftr_daily_btws` comparison recipe. It combines the existing
  calendar-neutral daily CMIP6
  mean/minimum/maximum signal with the hourly bounded temperature weighted
  stretch from Eames et al. (2024), while reusing the baseline sequence,
  specific-humidity closure, and EPW output components. The implementation
  applies equations (7)--(16), records `S`, `m`, `n`, closure errors and
  fallback reasons, and uses deterministic bisection to retain the largest
  admissible exponent where the paper does not publish solver code. Its
  provenance identifies the complete recipe as a combination rather than the
  paper's monthly UKCP18 workflow (#151).

* Added a `harmonized` policy to `sobie_curry_daily()`. It retains the
  Sobie-Curry calendar-neutral factors, circular 21-day smoothing, baseline
  sequence, and hourly temperature transformation while applying the smoothed
  daily `huss` change through a shared specific-humidity closure. The target is
  bounded at zero and saturation before relative humidity and dew point are
  derived from projected temperature and pressure; closure states and clipped
  targets are retained as diagnostics. The existing `paper_faithful` output
  remains the default (#149).

* Added the registered `sobie_curry_daily()` paper-faithful comparison method.
  Its seven-stage pipeline derives daily thermodynamic factors from matching
  historical and future `tas`, `tasmin`, `tasmax`, `huss`, and `ps`, smooths
  the factors with the published circular 21-day window, preserves the
  baseline EPW sequence, and independently transforms dry-bulb temperature,
  dew point, relative humidity, and pressure. Dew-point standard-deviation
  change is interpreted as `sigma_future / sigma_historical - 1` so zero
  climate change remains an identity; the interpretation, settings,
  fallbacks, closure errors, and physical diagnostics are retained with the
  result (#147).

* Added a versioned registry of complete future-weather recipes. The catalog
  records each method's input roles, calendar policy, seven component stages,
  execution policy, output type, diagnostics, and provenance without
  serializing executable functions. `epw_morph_recipes()` and
  `epw_morph_recipe_spec()` expose the metadata, registered methods validate
  their inputs before execution, and CLI/workflow round trips retain the
  selected definition and policy (#145).

* Added explicit future-weather input roles and reusable `preprocess`,
  `calendar`, `signal`, `sequence`, `hourly`, `physics`, and `output`
  component contracts. Registered components declare their input requirements,
  intermediate data kinds, dimensional scope, stochastic behavior, settings,
  provenance, and diagnostics while existing `EpwMorphBackend` runners remain
  compatible (#143).

* Added the built-in `daily_temperature()` method for complete future-EPW
  workflows using daily future and historical CMIP6 temperature data. Its
  seven-stage component pipeline estimates circular daily `tas` changes,
  preserves the baseline EPW sequence, constrains each hourly profile to daily
  mean/minimum/maximum targets when paired `tasmin` and `tasmax` are available,
  and otherwise inherits the baseline daily range explicitly. Baseline specific
  humidity is retained subject to saturation, relative humidity and dew point
  are recomputed, and hourly numerical, physical, and component diagnostics
  remain available in the persisted result (#141).

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

* Consolidated projected and historical Belcher monthly-extrema aggregation
  and attachment while preserving their distinct scientific case identities,
  missing-input behavior, and alignment diagnostics (#224).

* Consolidated absolute-target and change-factor Belcher runner assembly while
  retaining their separate field builders, no-reference fallback, equations,
  policies, diagnostics, factor metadata, part order, and result values (#222).

* Added complete Belcher runner behavior snapshots covering absolute-target
  and change-factor execution, legacy and enhanced profiles, baseline-EPW
  fallback, production case isolation, every intermediate part, factors, and
  diagnostics (#220).

* Consolidated direct-model hourly calendar traversal while retaining separate
  circular point interpolation and conservative interval-mean kernels,
  identity behavior, row order, time-of-day provenance, and diagnostics (#218).

* Consolidated Shift dynamic-scope nesting and artifact-row iteration while
  preserving per-stage readers, metadata, column selection, diagnostics,
  ordering, and global row limits (#216).

* Consolidated fixed-length ESGF time-field normalization and metadata-first
  DRS range filling while retaining query warnings, label selection, output
  formatting, and workflow catalog behavior (#214).

* Consolidated typed weather-sequence identifier, year, member-class,
  provenance, uniqueness, ordering, and shared-identity validation while
  preserving each sequence type's calendar, variable, row, and physical rules
  (#212).

* Consolidated concurrent data-node reachability, service URL reachability,
  and latency check scheduling into one callback-driven executor while
  preserving each check's HTTP-status, Range fallback, timing, and result
  semantics (#210).

* Consolidated ordinary and segmented downloader worker dependencies into one
  explicitly serialized helper bundle while preserving their separate stream,
  resume, piece-scheduling, and multi-source algorithms (#207).

* Consolidated Store catalog download-plan identity and base-row construction,
  and replaced duplicate plan decorators with one file-row implementation while
  preserving candidate selection and layout behavior (#205).

* Consolidated the shared settings envelope, schema validation, integer and
  bound checks, deterministic seed normalization, and role-wise precipitation
  threshold randomization used by statistical signal methods while preserving
  each method's equations and threshold semantics (#203).

* Consolidated repeated weather input validation, daily temperature
  preprocessing, successful signal-value extraction, native-calendar method
  fixtures, and Solr response fixtures into method-neutral helpers while
  preserving method equations and observable workflow behavior (#201).

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
