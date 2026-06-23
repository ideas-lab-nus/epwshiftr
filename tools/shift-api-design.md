# Store-Native Shift API Design

This document records the proposed next-generation user-facing API for
epwshiftr. It is a design note, not a committed implementation contract.

## Goals

epwshiftr should make the complete future-weather workflow easy to follow:

```r
job$
  collect()$
  download()$
  extract()$
  morph()$
  epw()
```

For most users, the public API should feel more like a small set of plain
functions than a set of engine objects and manifest IDs. Every step should
return an object that is easy to print, inspect, save, and pass into the next
step.

Advanced users should still be able to tune provider queries, download
selection, extraction details, morphing recipes, grouping, strictness, resume,
and overwrite behavior. The lower-level engines and manifest IDs remain
available for debugging and reproducibility, but ordinary workflows should not
need to pass `query_id`, `session_id`, `plan_id`, `summary_id`, `baseline_id`,
or `morph_id` by hand.

## Naming

Use a stable package-level prefix that is not tied to a single data source or
output file format.

This design uses:

```r
shift_request()
shift_site()
shift_collect()
shift_download()
shift_extract()
shift_morph()
shift_epw()
```

Rationale:

- `cmip6_*` is too narrow. ESGF/CMIP6 is only one supported provider family.
- `epw_*` is too narrow. EPW means EnergyPlus Weather File, while the package
  workflow also covers climate data search, download, extraction, diagnostics,
  and intermediate artifacts.
- `shift_*` matches the package idea: shift weather data into a future-weather
  representation, with EPW as the first-class final output.

## Architecture

The new API should be a thin functional facade over typed S7 stage objects.
Existing R6 classes remain as execution engines.

```text
user functions
  -> S7 stage/result objects
    -> EsgQuery, EsgStore, Downloader, EpwMorpher
      -> manifest tables, downloaded files, extracted Parquet, EPW outputs
```

The S7 objects should be small handles. They should store stable references,
metadata, diagnostics, and the internal IDs needed to resume or inspect work.
They should not store large climate arrays or full downloaded datasets in
memory.

## Design Principles

1. One public object per workflow stage.
2. Each public function accepts the previous stage object and returns the next
   stage object.
3. Internal manifest IDs are stored in stage objects, not passed through normal
   user code.
4. All stage objects are inspectable before continuing.
5. Advanced options are function arguments, not separate required APIs.
6. Provider-specific behavior lives in adapters behind provider-neutral
   constructors.
7. Existing R6 classes remain the engines. The new S7 layer should reduce
   surface complexity, not duplicate engine logic.

## Object Model

Use one abstract S7 base class for the public stage objects. This is
illustrative implementation pseudocode; the real code should use the package's
existing `checkmate_property()` helpers where they make validation clearer.

```r
ShiftDiagnostics <- S7::new_S3_class("data.frame")

ShiftStage <- S7::new_class(
  "ShiftStage",
  abstract = TRUE,
  properties = list(
    stage = checkmate_property(S7::class_character, checkmate::check_string),
    store_path = S7::new_property(S7::class_character | NULL, default = NULL),
    ids = S7::new_property(S7::class_list, default = list()),
    meta = S7::new_property(S7::class_list, default = list()),
    diagnostics = S7::new_property(ShiftDiagnostics, default = data.frame())
  )
)
```

Concrete stage classes should add only the state that is specific to that
stage. Keep them small and durable:

```r
ShiftRequest  <- S7::new_class("ShiftRequest",  parent = ShiftStage)
ShiftFiles    <- S7::new_class("ShiftFiles",    parent = ShiftStage)
ShiftDownload <- S7::new_class("ShiftDownload", parent = ShiftStage)
ShiftSite     <- S7::new_class("ShiftSite",     parent = ShiftStage)
ShiftClimate  <- S7::new_class("ShiftClimate",  parent = ShiftStage)
ShiftMorphed  <- S7::new_class("ShiftMorphed",  parent = ShiftStage)
ShiftOutputs  <- S7::new_class("ShiftOutputs",  parent = ShiftStage)
```

The exact implementation may split `meta` into stricter properties once the
first implementation proves which stage-specific metadata deserves a stable
slot. Start conservative: typed stage boundary, flexible metadata inside the
boundary.

`ids` is the only place ordinary stage objects should carry manifest IDs. Use
stable names:

```r
list(
  query_id = NULL,
  session_id = NULL,
  plan_id = NULL,
  summary_id = NULL,
  baseline_id = NULL,
  morph_id = NULL,
  output_id = NULL
)
```

The public workflow must not require users to extract these values manually.

`ShiftSite` is the one special case: it may be created before any store exists,
so `store_path` should normally stay `NULL`.

## Provider Adapter Boundary

`shift_request()` is provider-neutral, but `shift_collect()` needs
provider-specific implementation. Keep that boundary explicit.

Internal adapter functions:

```r
shift_provider(x)
shift_as_query(x)
shift_collect_files(x, query, fields, all, limit, ...)
shift_request_summary(x)
```

Initial supported adapter:

```r
shift_as_query.ShiftRequest <- function(x) {
  switch(
    x@meta$provider,
    esgf = shift_as_esgf_query(x),
    cli::cli_abort("Unsupported shift provider: {.val {x@meta$provider}}.")
  )
}
```

For ESGF:

- create an `EsgQuery`;
- apply provider-neutral aliases such as `project`, `experiment`, `source`,
  `variant`, `variables`, `frequency`, and `time`;
- apply `filters` as provider-specific query parameters;
- keep `Dataset$collect(type = "File")` as the required File collection path.

Provider-neutral aliases should be sugar only. Users can always pass
provider-specific filters:

```r
req <- shift_request(
  provider = "esgf",
  project = "CMIP6",
  filters = list(
    activity_id = "ScenarioMIP",
    table_id = "Amon",
    experiment_id = "ssp585",
    source_id = "MRI-ESM2-0"
  )
)
```

This keeps the API stable if future providers do not use CMIP6 names.

## Core Stage Objects

### `ShiftRequest`

Represents a provider-neutral climate data request.

Suggested properties:

- `provider`: provider key, for example `"esgf"`.
- `project`: provider project, for example `"CMIP6"`.
- `source`: optional source/model selection.
- `experiment`: optional scenario/experiment selection.
- `variant`: optional ensemble/member selection.
- `variables`: requested variable IDs.
- `frequency`: requested temporal frequency.
- `time`: requested date/year range.
- `filters`: provider-specific filters.
- `options`: request-level options.

Constructor:

```r
req <- shift_request(
  provider = "esgf",
  project = "CMIP6",
  experiment = "ssp585",
  variables = epw_morph_variables("recommended"),
  frequency = "mon",
  filters = list(table_id = "Amon")
)
```

### `ShiftFiles`

Represents collected provider file records registered in an `EsgStore`.

Suggested properties:

- `request`: the originating `ShiftRequest`.
- `store_path`: path to the durable store.
- `query_id`: internal manifest query ID.
- `dataset_count`: collected dataset count.
- `file_count`: collected file count.
- `variables`: variables present in collected file records.
- `time_range`: discovered time coverage.
- `diagnostics`: standardized diagnostics table.

Produced by:

```r
files <- shift_collect(req, store = "cache/singapore-store")
```

For ESGF, implementation must collect File records through:

```r
Dataset$collect(type = "File")
```

The public object should hide this detail, but the implementation should keep
that data path.

Implementation outline:

```r
shift_collect.ShiftRequest <- function(
  x,
  store,
  fields = "*",
  all = TRUE,
  limit = FALSE,
  ...
) {
  query <- shift_as_query(x)
  datasets <- query$collect(type = "Dataset", fields = fields, all = all, limit = limit)
  files <- datasets$collect(type = "File", fields = fields, all = TRUE, limit = FALSE, ...)

  store <- shift_open_store(store)
  query_id <- store$add_files(files)

  ShiftFiles(
    stage = "files",
    store_path = store$path,
    ids = list(query_id = query_id),
    meta = list(request = x, dataset_count = nrow(datasets), file_count = nrow(files)),
    diagnostics = shift_diagnostics_empty()
  )
}
```

`shift_as_query()` is provider-specific. For `provider = "esgf"`, it creates
an `EsgQuery` and applies request filters.

### `ShiftDownload`

Represents downloaded local files and download-session state.

Suggested properties:

- `files`: the originating `ShiftFiles`.
- `session_id`: internal downloader session ID.
- `file_count`: target file count.
- `done_count`: completed download count.
- `failed_count`: failed download count.
- `skipped_count`: reused local file count.
- `diagnostics`: standardized diagnostics table.

Produced by:

```r
dl <- shift_download(files, resume = TRUE)
```

Implementation outline:

```r
shift_download.ShiftFiles <- function(
  x,
  dir = NULL,
  resume = TRUE,
  overwrite = FALSE,
  checksum = FALSE,
  ...
) {
  store <- shift_store(x)
  ids <- shift_ids(x)

  session_id <- store$download_files(
    query_id = ids$query_id,
    dir = dir,
    resume = resume,
    overwrite = overwrite,
    checksum = checksum,
    ...
  )

  status <- store$workflow_status(query_id = ids$query_id)

  ShiftDownload(
    stage = "download",
    store_path = S7::prop(x, "store_path"),
    ids = modifyList(ids, list(session_id = session_id)),
    meta = list(files = x, status = status),
    diagnostics = shift_diagnostics_from_status(status)
  )
}
```

### `ShiftSite`

Represents the spatial target for extraction and EPW morphing.

Suggested properties:

- `id`: stable site key, for example `"SIN"`.
- `label`: human-readable label.
- `lon`: longitude, optionally read from `epw$location()`.
- `lat`: latitude, optionally read from `epw$location()`.
- `epw`: optional baseline EPW path or `eplusr::Epw` object.
- `metadata`: provider-neutral site metadata.

Constructor:

```r
epw <- system.file(
  "extdata/vignettes/future-weather/SGP_Singapore.486980_IWEC.epw",
  package = "epwshiftr",
  mustWork = TRUE
)

site <- shift_site(
  id = "SIN",
  label = "singapore",
  epw = epw
)
```

### `ShiftClimate`

Represents extracted site-level climate data.

Suggested properties:

- `download`: originating `ShiftDownload`.
- `site`: `ShiftSite`.
- `plan_id`: internal extraction plan ID.
- `periods`: extraction periods.
- `variables`: extracted variables.
- `coverage`: period/month/variable coverage summary.
- `diagnostics`: standardized diagnostics table.

Produced by:

```r
climate <- shift_extract(
  dl,
  site = site,
  periods = epw_morph_periods(2060),
  method = "nearest"
)
```

Implementation outline:

```r
shift_extract.ShiftDownload <- function(
  x,
  site,
  periods,
  variables = NULL,
  method = "nearest",
  fallback = "auto",
  overwrite = FALSE,
  ...
) {
  store <- shift_store(x)
  ids <- shift_ids(x)

  plan <- store$plan_region(
    query_id = ids$query_id,
    lon = site@lon,
    lat = site@lat,
    site_id = site@id,
    variable_id = variables,
    periods = periods,
    method = method,
    ...
  )

  processed <- store$extract(plan_id = plan$plan_id, fallback = fallback, overwrite = overwrite)
  coverage <- store$coverage(plan_id = plan$plan_id)

  ShiftClimate(
    stage = "climate",
    store_path = S7::prop(x, "store_path"),
    ids = modifyList(ids, list(plan_id = plan$plan_id)),
    meta = list(download = x, site = site, periods = periods, processed = processed, coverage = coverage),
    diagnostics = shift_diagnostics_from_coverage(coverage)
  )
}
```

### `ShiftMorphed`

Represents store-backed morphing results.

Suggested properties:

- `climate`: originating `ShiftClimate`.
- `baseline`: baseline EPW summary reference.
- `recipe`: morphing recipe.
- `summary_id`: internal climate summary ID.
- `baseline_id`: internal baseline summary ID.
- `morph_id`: internal morphing plan/result ID.
- `strict`: strictness used for the morphing plan.
- `diagnostics`: standardized diagnostics table.
- `results`: morphing result references.

Produced by:

```r
morphed <- shift_morph(
  climate,
  baseline = site,
  recipe = epw_morph_recipe("belcher"),
  strict = TRUE,
  resume = TRUE
)
```

Internally this should use `EpwMorpher` through the component steps up to
`run()`. It should not call `write_epw()`; that remains the responsibility of
`shift_epw()`.

```r
morpher <- epw_morpher(store, epw, site_id = site$id, label = site$label)
morpher$preflight(plan_id = plan_id, periods = periods, strict = strict)
morpher$summarise_climate(plan_id = plan_id, periods = periods, strict = strict)
morpher$summarise_baseline()
morpher$plan(summary_id = summary_id, baseline_id = baseline_id, strict = strict)
morpher$run(morph_id, resume = resume)
```

The public API should not require the user to pass `plan_id`,
`summary_id`, `baseline_id`, or `morph_id`.

Implementation outline:

```r
shift_morph.ShiftClimate <- function(
  x,
  baseline = NULL,
  periods = NULL,
  recipe = epw_morph_recipe("belcher"),
  strict = TRUE,
  by = c("source_id", "experiment_id", "variant_label", "period"),
  resume = TRUE,
  overwrite = FALSE,
  ...
) {
  store <- shift_store(x)
  ids <- shift_ids(x)
  site <- shift_target(x)
  epw <- shift_resolve_epw(baseline %||% site)
  periods <- periods %||% S7::prop(x, "meta")$periods

  morpher <- epw_morpher(store, epw, site_id = site@id, label = site@label, recipe = recipe)
  preflight <- morpher$preflight(plan_id = ids$plan_id, periods = periods, by = by, strict = strict)
  climate <- morpher$summarise_climate(plan_id = ids$plan_id, periods = periods, strict = strict, overwrite = overwrite)
  baseline_summary <- morpher$summarise_baseline(overwrite = overwrite)
  preview <- morpher$preview_plan(summary_id = climate$summary_id[[1L]], baseline_id = baseline_summary$baseline_id[[1L]], by = by, strict = strict)
  plan <- morpher$plan(summary_id = climate$summary_id[[1L]], baseline_id = baseline_summary$baseline_id[[1L]], by = by, strict = strict, overwrite = overwrite)
  results <- morpher$run(plan$morph_id[[1L]], overwrite = overwrite, resume = resume)

  ShiftMorphed(
    stage = "morphed",
    store_path = S7::prop(x, "store_path"),
    ids = modifyList(ids, list(
      summary_id = climate$summary_id[[1L]],
      baseline_id = baseline_summary$baseline_id[[1L]],
      morph_id = plan$morph_id[[1L]]
    )),
    meta = list(climate = x, baseline = baseline, recipe = recipe, preview = preview, results = results),
    diagnostics = shift_bind_diagnostics(preflight, preview$diagnostics)
  )
}
```

### `ShiftOutputs`

Represents final written output files.

Suggested properties:

- `morphed`: originating `ShiftMorphed`.
- `format`: output format, initially `"epw"`.
- `paths`: output file paths.
- `output_rows`: manifest output rows or a compact reference to them.
- `diagnostics`: standardized diagnostics table.

Produced by:

```r
out <- shift_epw(morphed, dir = "outputs/future-epw")
```

Implementation outline:

```r
shift_epw.ShiftMorphed <- function(
  x,
  dir,
  separate = TRUE,
  overwrite = FALSE,
  resume = TRUE,
  ...
) {
  store <- shift_store(x)
  ids <- shift_ids(x)
  site <- shift_target(x)
  epw <- shift_resolve_epw(site)

  morpher <- epw_morpher(store, epw, site_id = site@id, recipe = S7::prop(x, "meta")$recipe)
  outputs <- morpher$write_epw(
    morph_id = ids$morph_id,
    dir = dir,
    separate = separate,
    overwrite = overwrite,
    resume = resume,
    ...
  )

  ShiftOutputs(
    stage = "outputs",
    store_path = S7::prop(x, "store_path"),
    ids = modifyList(ids, list(output_id = outputs$output_id %||% NULL)),
    meta = list(morphed = x, format = "epw", outputs = outputs, paths = outputs$path),
    diagnostics = shift_diagnostics_empty()
  )
}
```

## Ordinary User Workflow

The target README-level workflow should read top to bottom without exposed
manifest IDs:

```r
library(epwshiftr)

epw <- system.file(
  "extdata/vignettes/future-weather/SGP_Singapore.486980_IWEC.epw",
  package = "epwshiftr",
  mustWork = TRUE
)

site <- shift_site(
  id = "SIN",
  label = "singapore",
  epw = epw
)

req <- shift_request(
  provider = "esgf",
  project = "CMIP6",
  source = "EC-Earth3",
  experiment = "ssp585",
  variant = "r1i1p1f1",
  variables = epw_morph_variables("recommended"),
  frequency = "mon",
  time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
  filters = list(activity_id = "ScenarioMIP", table_id = "Amon")
)

files <- shift_collect(req, store = "cache/singapore-store")
shift_status(files)
shift_coverage(files)

dl <- shift_download(files)
shift_status(dl)

climate <- shift_extract(
  dl,
  site = site,
  periods = epw_morph_periods(2060)
)
shift_diagnostics(climate)

morphed <- shift_morph(climate, baseline = site, strict = TRUE)
shift_status(morphed)
shift_diagnostics(morphed)

epws <- shift_epw(morphed, dir = "outputs/future-epw")
shift_outputs(epws)
```

## Optional Pipe Workflow

Functions should also support pipe-friendly use:

```r
epws <-
  shift_request(
    provider = "esgf",
    project = "CMIP6",
    source = "EC-Earth3",
    experiment = "ssp585",
    variant = "r1i1p1f1",
    variables = epw_morph_variables("recommended"),
    frequency = "mon",
    time = c("2060-01-01T00:00:00Z", "2060-12-31T23:59:59Z"),
    filters = list(activity_id = "ScenarioMIP", table_id = "Amon")
  ) |>
  shift_collect(store = "cache/singapore-store") |>
  shift_download() |>
  shift_extract(site = site, periods = epw_morph_periods(2060)) |>
  shift_morph(baseline = site, strict = TRUE) |>
  shift_epw(dir = "outputs/future-epw")
```

## Public Function Surface

The first implementation should expose a small set of constructors, workflow
functions, and inspectors.

Constructors:

```r
shift_request(provider = "esgf", project = NULL, ..., filters = list(), options = list())
shift_site(id = NULL, lon = NULL, lat = NULL, label = NULL, epw = NULL, metadata = list())
```

Workflow functions:

```r
shift_collect(x, store, ...)
shift_download(x, ...)
shift_extract(x, site, periods, ...)
shift_morph(x, baseline = NULL, ...)
shift_epw(x, dir, ...)
```

Inspectors:

```r
shift_status(x)
shift_check(x, strict = FALSE)
shift_diagnostics(x, severity = NULL)
shift_ids(x)
shift_store(x)
shift_target(x)
shift_artifacts(x)
shift_coverage(x)
shift_outputs(x)
```

Conversion and display:

```r
as.data.table(x)
summary(x)
print(x)
```

Implementation choice:

- Use S7 generics for user-facing verbs that dispatch on stage class.
- Use ordinary internal helpers for provider adapters, store reopening, and
  diagnostics conversion.
- Export the functions. Keep raw S7 classes internal in the first release unless
  direct class construction becomes genuinely useful.

S7 generic sketch:

```r
shift_collect <- S7::new_generic("shift_collect", "x", function(x, store, ...) {
  S7::S7_dispatch()
})

shift_download <- S7::new_generic("shift_download", "x", function(x, ...) {
  S7::S7_dispatch()
})

shift_extract <- S7::new_generic("shift_extract", "x", function(x, site, periods, ...) {
  S7::S7_dispatch()
})

shift_morph <- S7::new_generic("shift_morph", "x", function(x, baseline = NULL, ...) {
  S7::S7_dispatch()
})

shift_epw <- S7::new_generic("shift_epw", "x", function(x, dir, ...) {
  S7::S7_dispatch()
})
```

## Inspectors

Every stage object should support a consistent inspection surface.

```r
shift_status(x)
shift_check(x)
shift_diagnostics(x)
shift_ids(x)
shift_store(x)
shift_target(x)
shift_artifacts(x)
shift_coverage(x)
shift_outputs(x)
as.data.table(x)
summary(x)
print(x)
```

Design rules:

- `print()` should show where the user is in the workflow, counts, status,
  blocking diagnostics, and the next natural action.
- `summary()` should give a compact human-readable stage report.
- `as.data.table()` should expose the primary table for the current stage.
- `shift_check()` should run the strongest no-side-effect validation available
  for the current stage and return diagnostics. With `strict = TRUE`, error
  diagnostics should abort.
- `shift_ids()` is the escape hatch for advanced users who need internal
  manifest IDs.
- `shift_store()` should reopen the underlying `EsgStore` when needed.
- `shift_target()` should return the `ShiftSite` associated with a later stage
  object.

## Diagnostics

Diagnostics should use one common table shape across stages, aligned with
`EpwMorpher$preflight()` diagnostics:

```text
stage
severity
code
message
query_id
session_id
plan_id
summary_id
baseline_id
morph_id
case_id
variable_id
epw_field
period
month
action
```

Not every column needs to be populated for every stage. Stable columns are more
important than perfectly compact tables.

## Status Model

Each stage should report one compact status through `shift_status()`:

```text
new
collected
downloaded
extracted
morphed
written
blocked
failed
partial
```

Status meanings:

- `new`: request or site object exists, but no remote/local work has run.
- `collected`: file records are registered in the store.
- `downloaded`: all expected files are local and manifest-linked.
- `extracted`: extraction outputs exist for the requested site/periods.
- `morphed`: morphing result tables/files exist.
- `written`: final EPW outputs exist.
- `blocked`: validation found errors that prevent the next step.
- `failed`: the previous execution step errored and stored `last_error` or an
  equivalent diagnostic.
- `partial`: some expected work is complete, but the next step may need resume
  or user review.

The status should be derived from the stage object's own metadata plus the
current store state. It should not rely only on stale metadata saved in the S7
object.

## Validation Contract

Every workflow function should do a cheap local check before running side
effects. Every inspector should be no-side-effect.

```r
shift_check(req)
shift_check(files)
shift_check(dl)
shift_check(climate)
shift_check(morphed)
shift_check(epws)
```

Expected checks:

- `ShiftRequest`: provider exists, required request fields are present, filter
  names are valid for provider when that can be checked locally.
- `ShiftFiles`: store exists, query ID exists, collected file rows are present,
  variables/time coverage are plausible.
- `ShiftDownload`: local files exist, manifest links exist, failed download
  records are exposed.
- `ShiftClimate`: extraction plan exists, output files are readable, requested
  variables/months/periods are covered.
- `ShiftMorphed`: required morphing variables and EPW fields are present,
  factors are complete or diagnostics explain why not.
- `ShiftOutputs`: output files exist, manifest rows point to existing files,
  file paths are returned.

`shift_check(x, strict = TRUE)` should abort if any diagnostic has
`severity = "error"`. With `strict = FALSE`, it should return diagnostics for
inspection.

`shift_morph()` should delegate strict morphing validation to
`EpwMorpher$preflight()` and `EpwMorpher$check()`.

## Advanced User Workflow

Advanced users can still drop down to the existing engine layer:

```r
store <- shift_store(climate)
ids <- shift_ids(climate)

store$extract(plan_id = ids$plan_id, overwrite = TRUE)

morpher <- epw_morpher(store, site@epw, site_id = site@id)
morpher$preflight(plan_id = ids$plan_id)
morpher$preview_plan(summary_id = ids$summary_id)
```

The stage objects should make this possible, but this should not be the normal
path.

## Persistence

S7 objects should be restartable handles. Prefer storing:

- store path
- internal manifest IDs
- compact metadata
- diagnostics
- output references

Avoid storing:

- live R6 objects as the only state
- large climate arrays
- full NetCDF or Parquet data in object slots

This makes it possible to save a stage object and reopen it later:

```r
saveRDS(climate, "climate-stage.rds")
climate <- readRDS("climate-stage.rds")
shift_status(climate)
```

## Resume And Overwrite Rules

The high-level API should inherit the current store/morpher resume semantics:

- default `resume = TRUE`;
- default `overwrite = FALSE`;
- if complete manifest rows and files exist, return a stage object without
  rerunning expensive work;
- if manifest rows exist but some files are missing, resume the missing work
  where the lower-level engine supports it;
- if conflicting files exist without manifest rows, abort unless
  `overwrite = TRUE`;
- if a stage fails, preserve diagnostics and let the error propagate.

The stage object returned after a resumed operation should refresh metadata from
the store instead of appending stale counts from the previous object.

## Implementation Batches

### Batch 1: S7 Stage Skeleton

Add internal S7 classes, constructors, shared diagnostics helpers, and
inspectors:

- `R/shift-stage.R`
- `R/shift-diagnostics.R`
- `shift_status()`
- `shift_diagnostics()`
- `shift_ids()`
- `shift_store()`
- `shift_target()`

No network or heavy workflow behavior is required in this batch.

### Batch 2: Request And Collection

Implement:

- `shift_request()`
- ESGF request adapter
- `shift_collect()`
- tests proving `Dataset$collect(type = "File")` is used for ESGF File rows
- `as.data.table()` and `print()` for `ShiftRequest` and `ShiftFiles`

### Batch 3: Download And Extraction

Implement:

- `shift_download()`
- `shift_extract()`
- `shift_coverage()`
- status refresh from `EsgStore`
- resume/overwrite behavior inherited from `EsgStore`

### Batch 4: Morph And EPW Outputs

Implement:

- `shift_morph()`
- `shift_epw()`
- `shift_outputs()`
- delegation to `EpwMorpher` component methods through `run()`
- diagnostics propagation from `EpwMorpher$preflight()`

### Batch 5: User-Facing Documentation

Update:

- README workflow
- store-native morphing vignette
- pkgdown reference grouping

The old low-level vignettes can remain, but the recommended path should become
the `shift_*` API.

## Test Matrix

Minimum focused tests for the new API:

- constructors validate required fields and print compact stage summaries;
- `ShiftRequest -> ShiftFiles` uses the ESGF Dataset-to-File collection path;
- `ShiftFiles -> ShiftDownload` registers session IDs and reports partial or
  failed downloads;
- `ShiftDownload -> ShiftClimate` creates and executes extraction plans without
  exposing `plan_id` in normal user code;
- `ShiftClimate -> ShiftMorphed` delegates to `EpwMorpher` component methods
  through `run()` and propagates diagnostics;
- `ShiftMorphed -> ShiftOutputs` writes EPW files and returns paths;
- `shift_ids()` exposes internal IDs for advanced use;
- saved and reloaded stage objects can reopen the store and report status;
- no test fixture commits large `.nc` or `.nc4` files.

## Implementation Notes

1. Add S7 stage classes in a new file such as `R/shift-stage.R`.
2. Add public constructors and function-style generics in `R/shift-api.R`.
3. Keep the first implementation thin:
   - `shift_collect()` delegates to `EsgQuery` and `EsgStore$add_files()`.
   - `shift_download()` delegates to `EsgStore$download_files()`.
   - `shift_extract()` delegates to `EsgStore$plan_region()` and
     `EsgStore$extract()`.
   - `shift_morph()` delegates to `EpwMorpher` component methods through `run()`.
   - `shift_epw()` delegates to `EpwMorpher$write_epw()` through the stored
     morphing state.
4. Do not remove low-level APIs in the first batch.
5. Do not add compatibility shims for rejected names unless explicitly needed.
6. Avoid adding a new persistent `workflow_id` until stage objects prove that
   existing manifest IDs are insufficient. If needed later, add it as a grouping
   metadata layer, not as another required user argument.

## Open Decisions

- Whether S7 class constructors are exported directly or only through
  `shift_*()` functions.
- Whether `ShiftSite` should store an `eplusr::Epw` object, an EPW path, or both.
- Whether `shift_request()` should include provider plugins in the first
  release, or start with ESGF only while preserving provider-neutral naming.
- How much provider-specific detail should appear in `print()` versus
  `summary()` versus `as.data.table()`.
