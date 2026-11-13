# Independent hourly ERA5 data

The development API can acquire selected ERA5 point fields, validate their raw
files, and save a normalized RDS table without generating EPWs. This workflow
requires ERA5 single-level, hourly, explicit ARCO or CDS access. Unsupported
routes fail explicitly. It does not generate TMYs or calculate derived variables.
The published v0.1.4 workflow remains on `legacy`; see the
[migration guide](https://ideas-lab-nus.github.io/epwshiftr/articles/legacy-migration.html).

## Plan before acquiring

```r
library(epwshiftr)
source <- shift_era5(
    2001,
    variables = c("tas", "tdps", "ps", "uas", "vas", "rsds"),
    frequency = "hour",
    access = "arco"
)
site <- shift_site("Singapore", lat = 1.25, lon = 104)
plan <- shift_reanalysis_data(source, site, dry_run = TRUE)
# Equivalently: shift_plan_reanalysis(source, site)
plan$requests[, c("request_id", "variable_id", "expected_hours", "status")]
```

Planning reads no credentials, writes no files and makes no network requests.
These six requests cover 2000-12-31 through 2002-01-01 inclusive: 8,808 hours per
field, including a full UTC boundary day on each side. Disjoint year ranges
remain separate. Equivalent points and shared raw dependencies are deduplicated;
associated site IDs remain in the plan. Request IDs identify the exact dataset
and request body. Expected coverage is a request specification, not downloaded
coverage. The returned grid coordinates must still be checked.

## Exact UTC dates and radiation dependencies

Cloud cover (`clt`) and direct horizontal solar radiation are listed in the
[official ERA5 ARCO parameter table](https://confluence.ecmwf.int/pages/viewpage.action?pageId=505390919).
Both `access = "arco"` and explicit `access = "cds"` support these fields.
Public `rsdsdiff` expands to its two raw dependencies, SSRD and FDIR; the saved
data keep both, without calculating diffuse radiation. The CDS route requires
explicit native 0.25-degree coordinates; the following example uses that route.

```r
source <- shift_era5(2001, variables = c("rsdsdiff", "rlds", "clt", "pr"),
    frequency = "hour", access = "cds")
site <- shift_site("grid", lat = -12.5, lon = 131)
dates <- seq(as.Date("2001-06-29"), as.Date("2001-08-02"), by = "day")
plan <- shift_reanalysis_data(source, site, dates = dates, dry_run = TRUE)
```

`dates` specifies exact complete UTC days; it does not add padding. Supply the
boundary days needed by local-time interpolation and accumulated interval
coverage. Dates must fall within the declared years or their immediate boundary
days. With `dates = NULL`, existing full-year ARCO requests remain unchanged.
CDS splits each field by month and contiguous date range, requesting a single
explicit grid centre. Date gaps are never filled silently. This does not certify
provider availability; actual coordinates, units and full hours still require
validation. Clouds use explicit fraction/percent units and must lie in [0,100]%;
FDIR has the same preceding-hour accumulation contract as SSRD.

## Acquire with an explicit budget

Configure your CDS credentials through `ECMWF_DATASTORES_KEY` or the existing
`~/.ecmwfdatastoresrc` / `~/.cdsapirc` configuration, and accept the dataset's
terms in your CDS account. Do not put credentials in research scripts. Existing
`CDSAPI_KEY` settings are also supported. Authentication and provider availability
are only established during actual access.

```r
# This call performs network access. These limits apply to this six-request plan.
result <- shift_reanalysis_data(
    source, site, dir = "era5-singapore-2001",
    limits = list(
        max_submissions = 6,
        max_file_bytes = 16 * 1024^2,
        max_total_bytes = 128 * 1024^2,
        wait_timeout = 900,
        download_timeout = 180,
        total_timeout = 7200
    )
)
result$status
result$requests
result$diagnostics
result$execution
if (identical(result$status, "complete")) {
    weather <- readRDS(result$data$path)
    attr(weather, "reanalysis_schema")
}
```

Submissions run serially. Submission and total response-body budgets accumulate
across calls in the same directory, including failed attempts and control JSON.
Timeouts are seconds. `total_timeout` bounds this invocation's transfer stage;
use a separate process supervisor to limit total execution including local
validation and normalization. Without explicit limits, submission and byte
budgets are unlimited; wait/download defaults are 3,600/600 seconds.

Byte counts measure response bodies delivered to the reader, separately from
bytes written to assets. The first block crossing a threshold is counted and
aborts without being written. Already buffered bytes, HTTP/TLS headers and
uploads prevent interpreting this as an exact wire-traffic cap. Limits must be
chosen for the actual plan; do not increase them merely to bypass a failure.

## Understand the saved files

```text
era5-singapore-2001/
  identity.rds                 exact plan and acquisition origin
  result.rds                   latest execution status and diagnostics
  receipts/<request-id>.rds    individual strict-validation records
  transfer/                   network mode
    transfer.json             jobs, request states and cumulative accounting
    raw/<request-id>.asset    retained provider responses
  raw/                        copied assets in local-replay mode
  normalization.rds           normalized-data identity and descriptor
  normalized-<id>.rds          complete normalized long table
```

Locks exist while an owner is active. Paths in saved descriptors are absolute;
moving a directory does not rewrite them. Retain the original layout for normal
resume and archive the files with a hash manifest when sharing evidence.

CDS may return ZIP even when NetCDF was requested. A single top-level NetCDF
member is extracted temporarily and validated using the same rules as a bare
file. The raw ZIP remains unchanged. Artifacts retain its `raw_sha256`, plus
`netcdf_member` and `netcdf_sha256`; the member name is missing for bare NetCDF.
Multiple or nested NetCDF members are rejected, rather than silently selecting
one. Normalized values and UTC semantics are identical for both containers.

Strict validation checks the complete unique ordered UTC axis, scalar returned
grid, units, finite values, physical bounds and consistent coordinates across
fields. Normalized data retain raw values/units, hashes and interval semantics.
Hourly accumulated radiation uses the preceding hour and converts J/m² to W/m²
by dividing by 3,600. Skin temperature (`skt`) is available explicitly but is not
automatically added to a request or treated as building ground temperature.

Only `complete` exposes `result$data`; `partial` and `failed` have `data = NULL`.
An invalid raw asset remains available with a failure record. The normalized
identity includes the schema/conversion version, plan, origin and raw hashes.
Reopening verifies raw data and normalized-file integrity. This is an acquisition
and local data contract, not evidence of coastal-point suitability, TMY
representativeness or building-response validity. All boundary hours and raw
dependencies remain in the table; downstream study selection must be explicit.

## Resume or replay

```r
# Same source, sites, origin and directory; saved limits are inherited.
result <- shift_reanalysis_data(source, site, dir = "era5-singapore-2001")

# Local replay requires one file for every exact request ID.
files <- setNames(
    file.path("supplied-files", paste0(plan$requests$request_id, ".nc")),
    plan$requests$request_id
)
local <- shift_reanalysis_data(source, site, dir = "era5-local", data = files)
```

Changed plans or origins require a new directory. `resume = FALSE` refuses an
existing execution. Local replay copies the supplied files; the original paths
must remain available on later calls, and their provider origin is unauthenticated.

Completed raw assets are reused only after size/hash verification. Partial
downloads restart from the saved job's refreshed result URL, without byte-range
resume. Ambiguous submissions are never automatically repeated. A killed process
can leave an in-flight attempt with incomplete byte accounting: recovery records
that uncertainty and stops new transfers. Verify the recorded owner has stopped
before manually recovering any orphan lock; removing a lock does not resolve
an ambiguous job or uncertain accounting. Preserve the ledger and raw evidence
for diagnosis. A budget-limited recovery test is distinct from a real process-kill
experiment.

Plans may be saved with `saveRDS()` and restored with `readRDS()` for separate
stages. Request values, types and metadata must remain unchanged. Contract
validation ignores only data.table's process-local allocation pointer and does
not modify the supplied plan.

For separate stages, use `shift_download_reanalysis(plan, dir, limits = ...)`
and then `shift_read_reanalysis(plan, downloaded$files)`. Transport status
`downloaded` only certifies completed assets. Reader status `validated_local`
describes strict in-memory validation. The high-level `complete` status also
requires successful persistent publication of the normalized output.
