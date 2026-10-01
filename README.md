
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

epwshiftr is an R package for generating future EnergyPlus Weather (EPW)
files from CMIP6 climate projections and baseline weather, for use in
building simulation.

## Features

- **Discover climate data** from ESGF for the variables, models and
  periods your method needs.
- **Extract site data** through OPeNDAP where available, without
  downloading whole climate files.
- **Choose weather-generation methods** at monthly, daily and hourly
  scales.
- **Run batches** across locations, models, scenarios and methods.
- **Reuse and inspect work** with local caches, resumable workflows and
  source tracking, through R or the CLI.

## Installation

Install the development version used below:

``` r
install.packages(
    "epwshiftr",
    repos = c(
        "https://ideas-lab-nus.r-universe.dev",
        "https://cran.r-project.org"
    )
)
```

For CRAN v0.1.4, see the [migration
guide](https://ideas-lab-nus.github.io/epwshiftr/articles/legacy-migration.html).

## Example

Generate weather for Singapore and San Francisco using two monthly
methods. Place your baseline EPWs in the working directory; this example
uses [Singapore
IWEC](https://github.com/ideas-lab-nus/epwshiftr/blob/master/inst/extdata/examples/SGP_Singapore.486980_IWEC.epw)
and [San Francisco
TMY3](https://energyplus-weather.s3.amazonaws.com/north_and_central_america_wmo_region_4/USA/CA/USA_CA_San.Francisco.Intl.AP.724940_TMY3/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw).
Missing baseline precipitation is retained with `precipitation = "off"`.

``` r
library(epwshiftr)

batch <- shift_future_epw(
    sites = list(
        shift_site("Singapore", epw = "Singapore.epw"),
        shift_site("SanFrancisco", epw = "SanFrancisco.epw")
    ),
    climate = shift_cmip6(model = "IPSL-CM6A-LR", scenarios = "ssp245"),
    periods = list(`2060s` = 2055:2065),
    transform = list(
        monthly_transform("original_morphing", precipitation = "off"),
        monthly_transform("bws_btws")
    ),
    reference = historical_reference(1995:2014),
    dir = "future-epw-batch",
    store = "store",
    control = shift_control(
        download = "never",
        resume = FALSE,
        overwrite = TRUE
    )
)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/example-dark.svg">
<img src="man/figures/README/example.svg" width="100%" alt="Actual terminal recording of two-city future EPW generation.">
</picture>

*Actual terminal recording of `shift_future_epw()`, regenerated from
cached climate inputs. Playback is accelerated 4×, with long waits
shortened.*

Inspect the same batch directly in R. These tables show only the key
columns for the README; `shift_outputs()`, `shift_summary()` and
`shift_diagnostics()` return the full `data.table` results. Temperatures
are in °C.

``` r
shift_status(batch)
#> [1] "completed"
outputs <- shift_outputs(batch)
outputs[, .(site_id, method, model, period)]
#>         site_id            method        model period
#>          <char>            <char>       <char> <char>
#> 1: SanFrancisco original_morphing IPSL-CM6A-LR  2060s
#> 2: SanFrancisco          bws_btws IPSL-CM6A-LR  2060s
#> 3:    Singapore original_morphing IPSL-CM6A-LR  2060s
#> 4:    Singapore          bws_btws IPSL-CM6A-LR  2060s
summary <- shift_summary(batch, weather = TRUE)
summary[, .(site_id, method, weather_hours, mean_temperature_c)]
#>         site_id            method weather_hours mean_temperature_c
#>          <char>            <char>         <int>              <num>
#> 1: SanFrancisco original_morphing          8760           15.42045
#> 2: SanFrancisco          bws_btws          8760           15.42045
#> 3:    Singapore original_morphing          8760           28.28410
#> 4:    Singapore          bws_btws          8760           28.28410
shift_diagnostics(batch)[, .N, by = severity]
#>    severity     N
#>      <char> <int>
#> 1:  warning     1
#> 2:     info     2
```

## Documentation

| Task | Guide |
|----|----|
| Plan, run, inspect, or resume a workflow | [Future EPW workflow](https://ideas-lab-nus.github.io/epwshiftr/articles/future-epw-workflow.html) |
| Choose methods, settings, and required inputs | [Weather transformations](https://ideas-lab-nus.github.io/epwshiftr/articles/epw-morpher.html) |
| Configure terminal output and background jobs | [Live feedback](https://ideas-lab-nus.github.io/epwshiftr/articles/future-epw-workflow.html#live-feedback-and-background-runs) |
| Automate workflows from the shell | [CLI guide](https://ideas-lab-nus.github.io/epwshiftr/articles/cli-esgf-store.html) |
| Diagnose access, coverage, or download failures | [Troubleshooting](https://ideas-lab-nus.github.io/epwshiftr/articles/esgf-troubleshooting.html) |
| Find every function and argument | [API reference](https://ideas-lab-nus.github.io/epwshiftr/reference/index.html) |

## Citation and license

Jia, H., Chong, A., and Ning, B. (2023). *Epwshiftr: Incorporating Open
Data of Climate Change Prediction into Building Performance Simulation
for Future Adaptation and Mitigation.* Building Simulation 2023,
pp. 3201–3207. [DOI:
10.26868/25222708.2023.1612](https://doi.org/10.26868/25222708.2023.1612).
Run `citation("epwshiftr")` for the full citation and BibTeX entry.

Developed by Hongyuan Jia and Adrian Chong. The package is released
under the [MIT
license](https://github.com/ideas-lab-nus/epwshiftr/blob/master/LICENSE.md).
Climate data has separate terms: follow the official [CMIP licensing and
citation
guidance](https://wcrp-cmip.org/cmip-data-citation-and-licenses/) and
the terms of any calibration dataset you use.

[Report an issue](https://github.com/ideas-lab-nus/epwshiftr/issues).
