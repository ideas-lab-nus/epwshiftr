---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'epwshiftr: Create future EnergyPlus Weather files using CMIP6 data'
tags:
  - R
  - CMIP6
  - EnergyPlus
  - EPW
  - Climate change
  - Weather
authors:
  - name: Hongyuan Jia
    orcid: 0000-0002-0075-8183
    affiliation: 1
  - name: Adrian Chong
    orcid: 0000-0002-9486-4728
    affiliation: 2
affiliations:
 - name:  School of Civil Engineering and Architecture, Chongqing University of Science and Technology
   index: 1
 - name:  Department of the Built Environment, National University of Singapore
   index: 2
citation_author: Jia and Chong
date: 20 November 2021
year: 2021
bibliography: paper.bib
output:
  rmarkdown::html_vignette:
    keep_md: TRUE
vignette: >
  %\VignetteEncoding{UTF-8}
  %\VignetteIndexEntry{epwshiftr: Create future EnergyPlus Weather files using CMIP6 data}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  markdown:
    wrap: 80
---

# Summary

[![](logo.png "The epwshiftr package logo"){width="120px"}](https://cran.r-project.org/package=epwshiftr)

Building energy simulation (BES) has become increasingly applied to assess
building performance under climate changes and yield a more sustainable and
resilient design [@yassaghi2019]. Multiple morphing-based weather-file
modification tools have been developed to integrate climate change predictions
[@troup2016]. Most of the widely adopted weather generators, including
CCWorkdWeatherGen [@jentsch2008], Meteonorm [@remund2020], and WeatherShift
[@dickinson2016], use GCM (Global Climate Models) data from the CMIP (Coupled
Model Intercomparison Project) that covers worldwide locations.

Currently, the CMIP project is in its sixth phase (CMIP6) [@eyring2016], which
has developed new emission scenarios that have a similar range as its fifth
phase (CMIP5) but fill critical gaps for intermediate forcing levels
[@oneill2016]. It will be used in the Sixth IPCC (Intergovernmental Panel on
Climate Change) Climate Assessments Reports [@ipcc2021]. However, existing tools
based on the previous CMIP were unable to utilize the data from the latest
climate change research. Currently, there are no tools available that could
process user-defined climate simulations in an automated way and allow further
statistical analysis.

The epwshiftr package bridges these gaps. It is a free, open-source R package
for adapting a whole-building energy simulation EnergyPlus [@crawley2001]
Weather (EPW) files to incorporate climate change predictions using the morphing
statistical downscaling method [@belcher2005]. The primary goal is to
automatically process large amounts of climate change prediction outputs from
the CMIP6 (CMIP Phase 6) GCMs and create future climate data for BES across
worldwide locations in a user-friendly and flexible way.

# Epwshiftr R package

Epwshiftr is capable of processing multiple GCM outputs at various spatial and
temporal resolutions. Additionally, the package is designed in a modular manner
for flexibility and extensibility. There are five modules in total, and the
table below lists their corresponding names and functionalities.

+-------------------+----------------------------------------------------------+
| Module name       | Description                                              |
+===================+==========================================================+
| Query module      | Query and store metadata of online CMIP6 GCM outputs via |
|                   | the ESGF (Earth System Grid Federation) Search RESTful   |
|                   | API. Meta includes the name of GCM, the institution that |
|                   | developed the GCM, emission scenarios, output interval,  |
|                   | nominal resolution, output variable, output unit, etc.   |
+-------------------+----------------------------------------------------------+
| Database module   | Create and manage a local database of GCM outputs using  |
|                   | NetCDF files downloaded in the ESGF portal.              |
+-------------------+----------------------------------------------------------+
| Data Extraction   | Extract climate variable data of desired temporal domain |
| Module            | and specified grid distances to the input baseline EPW   |
|                   | file                                                     |
+-------------------+----------------------------------------------------------+
| Morphing Module   | Calculate future weather data under the latest CMIP6     |
|                   | emission scenarios using the morphing method             |
+-------------------+----------------------------------------------------------+
| EPW Generation    | Create future EPW files using various data aggregation   |
| Module            | strategies using the eplusr package [@jia2021]           |
+-------------------+----------------------------------------------------------+

: The modules designed in the epwshiftr package

Each module stores climate data in a consistent Tidy [@wickham2014] data format,
allowing exploring a considerably broad pool of ready-to-use methods available
in R for customized statistical analysis. Computational-intensive processes have
been designed to run in parallel for speed-up.

The epwshiftr is distributed via CRAN (The Comprehensive R Archive Network). The
source code is available on GitHub at
<https://github.com/ideas-lab-nus/epwshiftr> and released under the MIT license.

# Acknowledgements

This research was funded by the Republic of Singapore's National Research
Foundation through a grant to the Berkeley Education Alliance for Research in
Singapore (BEARS) for the Singapore-Berkeley Building Efficiency and
Sustainability in the Tropics (SinBerBEST) Program. BEARS has been established
by the University of California, Berkeley as a center for intellectual
excellence in research and education in Singapore.

# References
