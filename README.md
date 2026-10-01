
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
methods, then inspect the returned batch. Place your baseline EPWs in
the working directory; this example uses [Singapore
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

shift_status(batch)
outputs <- shift_outputs(batch)
outputs[, .(site_id, method, model, period)]
summary <- shift_summary(batch, weather = TRUE)
summary[, .(site_id, method, weather_hours, mean_temperature_c)]
shift_diagnostics(batch)[, .N, by = severity]
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/example-dark.svg">
<img src="man/figures/README/example.svg" width="100%" alt="Actual two-city EPW generation, followed by status, output, weather-summary and diagnostic queries.">
</picture>

*Actual terminal recording of this example, regenerated from cached
climate inputs. Playback is accelerated 4×, with long waits shortened.
Summary temperatures are in °C.*

## Documentation

See the [workflow
guide](https://ideas-lab-nus.github.io/epwshiftr/articles/future-epw-workflow.html)
for input preparation, reference periods and recovery; [method
guide](https://ideas-lab-nus.github.io/epwshiftr/articles/epw-morpher.html)
for requirements and experimental methods; [CLI
guide](https://ideas-lab-nus.github.io/epwshiftr/articles/cli-esgf-store.html)
for command-line use; and the [API
reference](https://ideas-lab-nus.github.io/epwshiftr/reference/index.html).

Developed by Hongyuan Jia and Adrian Chong. [MIT license](LICENSE.md).
Please cite [Jia, Chong and Ning
(2023)](https://doi.org/10.26868/25222708.2023.1612) using
`citation("epwshiftr")`; climate datasets have their own [citation and
licensing
requirements](https://wcrp-cmip.org/cmip-data-citation-and-licenses/).
[Report an issue](https://github.com/ideas-lab-nus/epwshiftr/issues).
