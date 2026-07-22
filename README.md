
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epwshiftr <img src="man/figures/logo.svg" align="right" />

<!-- badges: start -->

[![R build
status](https://github.com/ideas-lab-nus/epwshiftr/workflows/R-CMD-check/badge.svg)](https://github.com/ideas-lab-nus/epwshiftr/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/epwshiftr)](https://CRAN.R-project.org/package=epwshiftr)
[![Codecov test
coverage](https://codecov.io/gh/ideas-lab-nus/epwshiftr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ideas-lab-nus/epwshiftr?branch=master)
[![CRAN Download
Badge](https://cranlogs.r-pkg.org/badges/epwshiftr)](https://cran.r-project.org/package=epwshiftr)
<!-- badges: end -->

> Shift weather files with climate projection data and generate future
> EPW files.

epwshiftr helps you request climate projection data, collect file
records in a local store, extract site-level climate variables, morph a
baseline EnergyPlus Weather (EPW) file, and write shifted future EPW
files. The recommended user-facing path is the `shift_*` workflow.

<!-- TOC GFM -->

- [Installation](#installation)
- [Quick start](#quick-start)
- [Inspect a workflow](#inspect-a-workflow)
- [Advanced workflows](#advanced-workflows)
- [Legacy workflow](#legacy-workflow)
- [How to cite](#how-to-cite)
- [Author](#author)
- [License](#license)
- [Disclaimer](#disclaimer)
- [Contribute](#contribute)

<!-- /TOC -->

## Installation

You can install the latest stable release of epwshiftr from
[CRAN](https://CRAN.R-project.org).

``` r
install.packages("epwshiftr")
```

Alternatively, you can install the development version from R-universe.

``` r
install.packages(
    "epwshiftr",
    repos = c(
        ideaslab = "https://ideas-lab-nus.r-universe.dev",
        cran = "https://cran.r-project.org"
    )
)
```

The current development line and the next CRAN release use the new
store-native implementation. If you need the old data.table-oriented
workflow, use the `legacy` branch on GitHub or install epwshiftr
`v0.1.4`.

## Quick start

For the common baseline-to-future EPW workflow, use
`shift_future_epw()`. When matching historical CMIP6 data are available,
the recommended Belcher method supplies them explicitly with
`historical_reference()`. The function resolves files, extracts the site
time series, persists a resumable run, and copies only the final EPWs to
`dir`.

Run the complete workflow with the scientific intent kept in one call:

``` r
library(epwshiftr)

run <- shift_future_epw(
    epw = system.file(
        "extdata/examples/SGP_Singapore.486980_IWEC.epw",
        package = "epwshiftr"
    ),
    climate = shift_cmip6(
        model = "BCC-CSM2-MR",
        scenarios = c("ssp126", "ssp585")
    ),
    periods = list(`2060s` = 2055:2065),
    method = belcher(
        reference = historical_reference(1995:2014)
    ),
    dir = tempdir()
)
```

The representative terminal recording below is generated from
deterministic workflow states, so README builds do not depend on live
ESGF services. A real run uses the same dashboard; the selected node,
timings, and file counts vary.

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/shift-workflow-output-dark.svg">
<img src="man/figures/README/shift-workflow-output.svg" width="100%" />
</picture>

Inspect the persisted run and its delivered files with the same handle:

``` r
shift_status(run)
outputs <- shift_outputs(run)
outputs[, .(
    experiment_id,
    period,
    file = basename(export_path)
)]
shift_missing(run)
shift_diagnostics(run)
```

Dynamic foreground runs start directly inside one atomic live panel; the
plan context is replaced in place instead of leaving a duplicate startup
transcript. On terminals at least 60 columns wide, a quiet border and
labelled `Workflow` and `Activity` rules separate the run identity,
active work, and recent results. Narrow terminals omit that decoration
and preserve the same semantic rows. The dashboard keeps one animated
current operation, a static stage rail, stage-specific measured
progress, and two recent resolver or workflow outcomes. Resolver
failover reports node attempts and elapsed time without presenting the
attempt count as a workflow percentage. The live region is capped at 112
columns, uses short normal-mode diagnostics, and reserves green, yellow,
and red for semantic outcomes. Dynamic IDE consoles without cursor-up
support receive one compact status row, while redirected output uses
complete append-only logs. Downloads add aggregate bytes, speed, ETA,
active-file counts, and filenames at higher detail levels. Successful
and partial runs leave a final `Results` receipt with output counts, the
delivery directory, and exported filenames in terminal scrollback. If a
run fails, its final panel remains in terminal scrollback and changes
`Activity` to `Diagnosis`: repeated mirror failures are collapsed into
attempt counts, one cause, the closest CMIP6 identity, and the first
missing requirement. Transient failures offer `Retry`; incomplete
scientific coverage instead asks you to change the selection or
reference, because resuming the unchanged request would produce the same
result. `shift_ui()` controls only presentation and never changes the
workflow specification or its `spec_hash`:

``` r
run <- shift_future_epw(
    epw = epw,
    climate = shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585")),
    periods = list(`2060s` = 2055:2065),
    method = belcher(reference = historical_reference(1995:2014)),
    dir = "~/Downloads/epwshiftr-test",
    ui = shift_ui(
        progress = "auto",
        detail = "detail",
        motion = "auto"
    )
)
```

Use `detail = "normal"` for task-level progress, `"detail"` for
selection, reuse, fallback, and all output paths, or `"debug"` to
additionally include full URLs and internal paths. Live renderers use
full animation by default; `motion = "reduced"` keeps a stable active
marker, while `motion = "none"` removes motion without disabling the
dashboard or compact status row. Terminal capability, width, Unicode,
styling, and cursor visibility follow `cli`’s public APIs; the
multi-line framebuffer owns only atomic frame painting. `refresh`
controls only visual frames and `heartbeat` controls durable job
liveness. Use `progress = "log"` for screen readers, redirected output,
and stable captured logs. `"auto"` selects log mode automatically in CI,
`TERM=dumb`, and non-dynamic terminals.

For a long run that should survive the current R session, launch a
detached worker. Registration returns immediately; the same `ShiftRun`
handle reads live state from the store:

``` r
run <- shift_future_epw(
    epw = epw,
    climate = shift_cmip6("BCC-CSM2-MR", c("ssp126", "ssp585")),
    periods = list(`2060s` = 2055:2065),
    method = belcher(reference = historical_reference(1995:2014)),
    dir = "~/Downloads/epwshiftr-test",
    background = TRUE
)
shift_watch(run)
shift_logs(run) # `source` distinguishes process logs from persisted events
# shift_cancel(run) # stop at the next safe workflow boundary
```

Use `force = TRUE` only if the worker does not respond to the normal
cancellation request; it terminates the recorded worker process
immediately.

This example uses monthly CMIP6 `Amon` records and the variables
required by the Belcher recipe. The committed asciicast SVG demonstrates
the production dashboard without performing remote data reads during
documentation builds.

If no suitable historical CMIP6 reference is available, use `belcher()`
as a fallback. It then uses the input EPW climatology and does not infer
or query a historical reference.

## Inspect a workflow

Set `dry_run = TRUE` and use `shift_explain()` when you want to inspect
the workflow before touching ESGF services.

``` r
plan <- shift_future_epw(
    epw = epw,
    climate = shift_cmip6(
        model = "BCC-CSM2-MR",
        scenarios = c("ssp126", "ssp585")
    ),
    periods = list(`2060s` = 2055:2065),
    method = belcher(
        reference = historical_reference(1995:2014)
    ),
    dir = "~/Downloads/epwshiftr-test",
    dry_run = TRUE
)

shift_explain(plan)
run <- shift_run(plan, ui = shift_ui("auto"))

shift_status(run)
shift_diagnostics(run)
shift_outputs(run)
shift_data(run)
```

`shift_ids()` exposes underlying manifest IDs for advanced debugging,
but normal workflows should not require users to pass `query_id`,
`plan_id`, `summary_id`, or `morph_id` by hand.

To keep a complete local copy of the source NetCDF files, insert
`shift_download()` after `shift_collect()`. This is optional for the
normal single-site workflow because `shift_extract()` reads through
OPeNDAP first and only falls back to HTTP downloads when
`fallback = "auto"` and remote access is unavailable.

Standalone stages use the same dashboard and persisted run model. No
session object is required: the latest returned stage carries its
`run_id` and `step_id` into the next call automatically.

``` r
files <- shift_collect(request, store = store)
climate <- shift_extract(files, site, periods)
morphed <- shift_morph(climate, baseline = epw)
outputs <- shift_export_epw(morphed, dir = output_dir)

shift_run_get(outputs)
shift_ids(outputs)[c("run_id", "step_id")]
```

Intermediate stages leave the durable run in `waiting`, which means the
work succeeded and can continue; the terminal receipt labels this
user-facing state as `READY`. An empty collection is `partial` instead
because it cannot feed the next stage. `shift_export_epw()` completes a
successful run automatically. If an intermediate artifact is
intentionally the final result, close the run with
`shift_complete(files)` (or the latest stage object). Continuing from an
older or already completed stage creates a child run rather than
rewriting history. For `shift_download(..., background = TRUE)`, the run
remains `running` until the Downloader session finishes;
`shift_watch()`, `shift_cancel()`, and `shift_logs()` follow that
underlying job through the same run ID.

## Advanced workflows

The `shift_*` functions are a thin user-facing facade over lower-level
engines:

- `EsgQuery` builds and collects ESGF queries.
- `EsgStore` manages file records, downloads, extraction plans, and
  artifacts.
- `EpwMorpher` summarises climate data, builds morphing plans, runs
  morphing, and writes EPW files.

Use those lower-level objects when you need to inspect or tune
manifests, download candidate selection, extraction coverage, morphing
factors, or output registration. See [Create Future EPW
Files](vignettes/articles/future-epw-workflow.Rmd) for the expanded
query/download/extract/morph/write path.

## Legacy workflow

The older workflow based on `init_cmip6_index()`, `summary_database()`,
`extract_data()`, `morphing_epw()`, and `future_epw()` belongs to the
legacy implementation. These functions are not exported by the current
package.

Use the `legacy` branch on GitHub or epwshiftr `v0.1.4` if you need that
workflow unchanged while migrating to the store-native API.

## How to cite

To cite epwshiftr in publications use:

    Jia, Hongyuan, Chong, Adrian, Ning, Baisong, 2023.
    Epwshiftr: incorporating open data of climate change prediction into building performance simulation for future adaptation and mitigation,
    in: Proceedings of Building Simulation 2023: 18th Conference of IBPSA, Building Simulation.
    Presented at the Building Simulation 2023, IBPSA, Shanghai, China, pp. 3201-3207.
    https://doi.org/10.26868/25222708.2023.1612

A BibTeX entry for LaTeX users is:

``` bibtex
@inproceedings{jia2023epwshiftr,
  title = {Epwshiftr: Incorporating Open Data of Climate Change Prediction into Building Performance Simulation for Future Adaptation and Mitigation},
  shorttitle = {Epwshiftr},
  booktitle = {Proceedings of {{Building Simulation}} 2023: 18th {{Conference}} of {{IBPSA}}},
  author = {Jia, Hongyuan and Chong, Adrian and Ning, Baisong},
  year = {2023},
  series = {Building {{Simulation}}},
  volume = {18},
  pages = {3201--3207},
  publisher = {{IBPSA}},
  address = {{Shanghai, China}},
  doi = {10.26868/25222708.2023.1612}
}
```

## Author

Hongyuan Jia and Adrian Chong

## License

- **epwshiftr**

  epwshiftr is released under the terms of MIT License.

  Copyright (c) 2019-2024 Hongyuan Jia and Adrian Chong

- **CMIP6 data**

  > To enable modeling groups and others who support CMIP6 to
  > demonstrate its impact (and secure ongoing funding), you are
  > required to cite and acknowledge those who have made CMIP6 possible.
  > You also must abide by any licensing restrictions, which are
  > recorded in each file as a global attribute (named “license”).
  >
  > Please carefully read and adhere to the [CMIP6 Terms of
  > Use](https://pcmdi.llnl.gov/CMIP6/TermsOfUse/).

## Disclaimer

CMIP6 model data is licensed under a [Creative Commons
Attribution-ShareAlike 4.0 International
License](https://creativecommons.org/licenses/). Consult [Terms of
Use](https://pcmdi.llnl.gov/CMIP6/TermsOfUse/) for terms of use
governing CMIP6 output, including citation requirements and proper
acknowledgment. Further information about each GCM output data,
including some limitations, can be found via the `further_info_url`
recorded as a global attribute in the NetCDF file. The data producers
and data providers make no warranty, either express or implied,
including, but not limited to, warranties of merchantability and fitness
for a particular purpose. All liabilities arising from the supply of the
information, including any liability arising in negligence, are excluded
to the fullest extent permitted by law.

## Contribute

If you encounter a clear bug or have questions about the usage, please
file an issue with a minimal reproducible example on
[GitHub](https://github.com/ideas-lab-nus/epwshiftr/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc).

If you have a solution for an existing bug or an implementation for a
missing feature, please send a pull request and let us review.

------------------------------------------------------------------------

Please note that the ‘epwshiftr’ project is released with a [Contributor
Code of
Conduct](https://github.com/ideas-lab-nus/epwshiftr/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
