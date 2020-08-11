
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epwshiftr <img src="man/figures/logo.svg" align="right" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/ideas-lab-nus/epwshiftr.svg?branch=master)](https://travis-ci.com/ideas-lab-nus/epwshiftr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ideas-lab-nus/epwshiftr?branch=master&svg=true)](https://ci.appveyor.com/project/ideas-lab-nus/epwshiftr)
[![CRAN
status](https://www.r-pkg.org/badges/version/epwshiftr)](https://CRAN.R-project.org/package=epwshiftr)
[![Codecov test
coverage](https://codecov.io/gh/ideas-lab-nus/epwshiftr/branch/master/graph/badge.svg)](https://codecov.io/gh/ideas-lab-nus/epwshiftr?branch=master)
<!-- badges: end -->

> Create future EnergyPlus Weather files using CMIP6 data

<!-- TOC GFM -->

  - [Installation](#installation)
  - [Get started](#get-started)
      - [Build CMIP6 output file index](#build-cmip6-output-file-index)
      - [Manage CMIP6 output files](#download-cmip6-output-files)
      - [Extract CMIP6 output data](#extract-cmip6-output-data)
      - [Morphing EPW weather
        variables](#morphing-epw-weather-variables)
      - [Create future EPW files](#create-future-epw-files)
  - [Author](#author)
  - [License](#license)
  - [Contribute](#contribute)

<!-- /TOC -->

## Installation

You can install the latest stable release of epwshiftr from
[CRAN](https://CRAN.R-project.org).

``` r
install.packages("epwshiftr")
```

Alternatively, you can install the development version from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("ideas-lab-nus/epwshiftr")
```

## Get started

### Build CMIP6 output file index

  - The first step is to build CMIP6 experiment output file index based
    on queries using ESGF search RESTful API

<!-- end list -->

``` r
# set directory to store files
options(epwshiftr.dir = tempdir())
options(epwshiftr.verbose = TRUE)

# get CMIP6 data nodes
(nodes <- get_data_node())
#>                          data_node status
#>                             <char> <char>
#>  1:                 aims3.llnl.gov     UP
#>  2:                cmip.bcc.cma.cn     UP
#>  3:      cmip.dess.tsinghua.edu.cn     UP
#>  4:                cmip.fio.org.cn     UP
#>  5:          crd-esgf-drc.ec.gc.ca     UP
#>  6:           data.meteo.unican.es     UP
#>  7:       dataserver.nccs.nasa.gov     UP
#>  8:           dist.nmlab.snu.ac.kr     UP
#>  9:         dpesgf03.nccs.nasa.gov     UP
#> 10:        esg-cccr.tropmet.res.in     UP
#> 11:               esg-dn1.ru.ac.th     UP
#> 12:             esg-dn2.nsc.liu.se     UP
#> 13:                 esg.camscma.cn     UP
#> 14:                 esg.lasg.ac.cn     UP
#> 15:             esg.pik-potsdam.de     UP
#> 16:             esgf-data.ucar.edu     UP
#> 17:          esgf-data1.ceda.ac.uk     UP
#> 18:          esgf-data1.diasjp.net     UP
#> 19:            esgf-data1.llnl.gov     UP
#> 20:          esgf-data2.ceda.ac.uk     UP
#> 21:          esgf-data2.diasjp.net     UP
#> 22:            esgf-data2.llnl.gov     UP
#> 23:          esgf-data3.ceda.ac.uk     UP
#> 24:          esgf-data3.diasjp.net     UP
#> 25:                esgf-dev.bsc.es     UP
#> 26:      esgf-nimscmip6.apcc21.org     UP
#> 27:              esgf-node.cmcc.it     UP
#> 28:             esgf-node2.cmcc.it     UP
#> 29:                   esgf.anl.gov     UP
#> 30:                esgf.apcc21.org     UP
#> 31:                    esgf.dwd.de     UP
#> 32:                esgf.nci.org.au     UP
#> 33:        esgf.rcec.sinica.edu.tw     UP
#> 34:                  esgf2.dkrz.de     UP
#> 35:          noresg.nird.sigma2.no     UP
#> 36:              vesg.ipsl.upmc.fr     UP
#> 37:  145.100.59.180.surf-hosted.nl   DOWN
#> 38: acdisc.gesdisc.eosdis.nasa.gov   DOWN
#> 39:               cordexesg.dmi.dk   DOWN
#> 40:             esg-dn1.nsc.liu.se   DOWN
#> 41:               esg1.umr-cnrm.fr   DOWN
#> 42:          esgdata.gfdl.noaa.gov   DOWN
#> 43:         esgf-cnr.hpc.cineca.it   DOWN
#> 44:        esgf-ictp.hpc.cineca.it   DOWN
#> 45:                    esgf.bsc.es   DOWN
#> 46:                  esgf.ichec.ie   DOWN
#> 47:                  esgf1.dkrz.de   DOWN
#> 48:                  esgf3.dkrz.de   DOWN
#> 49:   gpm1.gesdisc.eosdis.nasa.gov   DOWN
#>                          data_node status

# create a CMIP6 output file index
idx <- init_cmip6_index(
    # only consider ScenarioMIP activity
    activity = "ScenarioMIP",

    # specify dry-bulb temperature and relative humidity
    variable = c("tas", "hurs"),

    # specify report frequent
    frequency = "day",

    # specify experiment name
    experiment = c("ssp585"),

    # specify GCM name
    source = "AWI-CM-1-1-MR",

    # specify variant,
    variant = "r1i1p1f1",

    # specify years of interest
    years = c(2050, 2080),

    # save to data dictionary
    save = TRUE
)
#> Querying CMIP6 Dataset Information
#> Querying CMIP6 File Information [Attempt 1]
#> Checking if data is complete
#> Data file index saved to '/tmp/RtmpDtbJVc/cmip6_index.csv'

# the index has been automatically saved into directory specified using
# `epwshiftr.dir` option and can be reloaded
idx <- load_cmip6_index()

str(head(idx))
#> Classes 'data.table' and 'data.frame':   6 obs. of  23 variables:
#>  $ file_id           : chr  "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.hurs.gn.v20190529.hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f"| __truncated__ "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.hurs.gn.v20190529.hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f"| __truncated__ "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.hurs.gn.v20190529.hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f"| __truncated__ "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529.tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_"| __truncated__ ...
#>  $ dataset_id        : chr  "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.hurs.gn.v20190529|esgf3.dkrz.de" "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.hurs.gn.v20190529|esgf3.dkrz.de" "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.hurs.gn.v20190529|esgf3.dkrz.de" "CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529|esgf3.dkrz.de" ...
#>  $ mip_era           : chr  "CMIP6" "CMIP6" "CMIP6" "CMIP6" ...
#>  $ activity_drs      : chr  "ScenarioMIP" "ScenarioMIP" "ScenarioMIP" "ScenarioMIP" ...
#>  $ institution_id    : chr  "AWI" "AWI" "AWI" "AWI" ...
#>  $ source_id         : chr  "AWI-CM-1-1-MR" "AWI-CM-1-1-MR" "AWI-CM-1-1-MR" "AWI-CM-1-1-MR" ...
#>  $ experiment_id     : chr  "ssp585" "ssp585" "ssp585" "ssp585" ...
#>  $ member_id         : chr  "r1i1p1f1" "r1i1p1f1" "r1i1p1f1" "r1i1p1f1" ...
#>  $ table_id          : chr  "day" "day" "day" "day" ...
#>  $ frequency         : chr  "day" "day" "day" "day" ...
#>  $ grid_label        : chr  "gn" "gn" "gn" "gn" ...
#>  $ version           : chr  "20190529" "20190529" "20190529" "20190529" ...
#>  $ nominal_resolution: chr  "100 km" "100 km" "100 km" "100 km" ...
#>  $ variable_id       : chr  "hurs" "hurs" "hurs" "tas" ...
#>  $ variable_long_name: chr  "Near-Surface Relative Humidity" "Near-Surface Relative Humidity" "Near-Surface Relative Humidity" "Near-Surface Air Temperature" ...
#>  $ variable_units    : chr  "%" "%" "%" "K" ...
#>  $ datetime_start    : POSIXct, format: "2049-01-01" "2050-01-01" ...
#>  $ datetime_end      : POSIXct, format: "2049-12-31" "2050-12-31" ...
#>  $ file_size         : int  91761231 91729347 91727399 82292505 82268546 82149654
#>  $ data_node         : chr  "esgf3.dkrz.de" "esgf3.dkrz.de" "esgf3.dkrz.de" "esgf3.dkrz.de" ...
#>  $ file_url          : chr  "http://esgf3.dkrz.de/thredds/fileServer/cmip6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/hurs/gn/v201905"| __truncated__ "http://esgf3.dkrz.de/thredds/fileServer/cmip6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/hurs/gn/v201905"| __truncated__ "http://esgf3.dkrz.de/thredds/fileServer/cmip6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/hurs/gn/v201905"| __truncated__ "http://esgf3.dkrz.de/thredds/fileServer/cmip6/ScenarioMIP/AWI/AWI-CM-1-1-MR/ssp585/r1i1p1f1/day/tas/gn/v2019052"| __truncated__ ...
#>  $ dataset_pid       : chr  "hdl:21.14100/89501ae0-2fec-307b-bf68-552ea4d504a0" "hdl:21.14100/89501ae0-2fec-307b-bf68-552ea4d504a0" "hdl:21.14100/89501ae0-2fec-307b-bf68-552ea4d504a0" "hdl:21.14100/a336f13f-a4d3-3b57-a45a-8f27f0ba01b8" ...
#>  $ tracking_id       : chr  "hdl:21.14100/f46077ee-ae81-4932-81af-d61394446ea3" "hdl:21.14100/a476933a-0f14-4d4c-b62d-0bf08e3471fd" "hdl:21.14100/3c3c98f8-d56e-4d8d-8ba7-1a9e541e6018" "hdl:21.14100/8503efb4-6509-4728-b95c-7203bd214a77" ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

### Manage CMIP6 output files

  - You have to download CMIP6 output file by yourself using your
    preferable methods or tools. The download url can be found in the
    `file_url` column in the index.

  - After you have downloaded CMIP6 output files of interest, you can
    use `suumary_database()` to get a summary on files downloaded
    against the CMIP6 output file index.

  - This step is necessary as it map the loaded files against index so
    that epwshiftr knows which case is complete and can be used for the
    next step.

<!-- end list -->

``` r
# Summary downloaded file by GCM and variable, use the latest downloaded file if
# multiple matches are detected and save matched information into the index file
sm <- summary_database(tempdir(), by = c("source", "variable"), mult = "latest", update = TRUE)
#> 24 NetCDF files found.
#> Data file index updated and saved to '/tmp/RtmpDtbJVc/cmip6_index.csv'

knitr::kable(sm)
```

| variable\_id | source\_id    | datetime\_start     | datetime\_end       | file\_num |     file\_size | dl\_num | dl\_percent |       dl\_size |
| :----------- | :------------ | :------------------ | :------------------ | --------: | -------------: | ------: | ----------: | -------------: |
| hurs         | AWI-CM-1-1-MR | 2049-01-01 12:00:00 | 2081-12-31 12:00:00 |         6 | 551 \[Mbytes\] |       6 |   100 \[%\] | 548 \[Mbytes\] |
| tas          | AWI-CM-1-1-MR | 2049-01-01 12:00:00 | 2081-12-31 12:00:00 |         6 | 493 \[Mbytes\] |       6 |   100 \[%\] | 484 \[Mbytes\] |

### Extract CMIP6 output data

  - With previous step, now we can match coordinates of an EPW in the
    CMIP6 output file

<!-- end list -->

``` r
epw <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
# match any coordinates with absolute distance less than 1 degree
coord <- match_coord(epw, threshold = list(lon = 1, lat = 1), max_num = 1)
#> Start to match coordinates...

class(coord)
#> [1] "epw_cmip6_coord"

names(coord)
#> [1] "epw"   "meta"  "coord"

coord$meta
#> $city
#> [1] "San Francisco Intl Ap"
#> 
#> $state_province
#> [1] "CA"
#> 
#> $country
#> [1] "USA"
#> 
#> $latitude
#> [1] 37.62
#> 
#> $longitude
#> [1] -122.4

coord$coord[, .(file_path, coord)]
#>                                                                          file_path
#>                                                                             <char>
#>  1: /tmp/RtmpDtbJVc/hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20490101-20491231.nc
#>  2: /tmp/RtmpDtbJVc/hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20500101-20501231.nc
#>  3: /tmp/RtmpDtbJVc/hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20510101-20511231.nc
#>  4:  /tmp/RtmpDtbJVc/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20490101-20491231.nc
#>  5:  /tmp/RtmpDtbJVc/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20500101-20501231.nc
#>  6:  /tmp/RtmpDtbJVc/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20510101-20511231.nc
#>  7: /tmp/RtmpDtbJVc/hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20790101-20791231.nc
#>  8: /tmp/RtmpDtbJVc/hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20800101-20801231.nc
#>  9: /tmp/RtmpDtbJVc/hurs_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20810101-20811231.nc
#> 10:  /tmp/RtmpDtbJVc/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20790101-20791231.nc
#> 11:  /tmp/RtmpDtbJVc/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20800101-20801231.nc
#> 12:  /tmp/RtmpDtbJVc/tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20810101-20811231.nc
#>      coord
#>     <list>
#>  1: <list>
#>  2: <list>
#>  3: <list>
#>  4: <list>
#>  5: <list>
#>  6: <list>
#>  7: <list>
#>  8: <list>
#>  9: <list>
#> 10: <list>
#> 11: <list>
#> 12: <list>

str(coord$coord$coord[[1]])
#> List of 2
#>  $ lat:List of 4
#>   ..$ index: int 1
#>   ..$ value: num 36.9
#>   ..$ dis  : num -0.685
#>   ..$ which: int 136
#>  $ lon:List of 4
#>   ..$ index: int 1
#>   ..$ value: num 302
#>   ..$ dis  : num -0.525
#>   ..$ which: int 323
```

  - Once we get the matched coordinates, we can extract corresponding
    data related to input EPW file using `extract_data()`

<!-- end list -->

``` r
data <- extract_data(coord, years = c(2050, 2080))
#> Start to extract CMIP6 data according to matched coordinates...

class(data)
#> [1] "epw_cmip6_data"
names(data)
#> [1] "epw"  "meta" "data"
knitr::kable(head(data$data))
```

| activity\_drs | institution\_id | source\_id    | experiment\_id | member\_id | table\_id | datetime            |      lat |     lon | variable | description                    | units |    value |
| :------------ | :-------------- | :------------ | :------------- | :--------- | :-------- | :------------------ | -------: | ------: | :------- | :----------------------------- | :---- | -------: |
| ScenarioMIP   | AWI             | AWI-CM-1-1-MR | ssp585         | r1i1p1f1   | day       | 2050-01-01 20:00:00 | 36.93492 | 301.875 | hurs     | Near-Surface Relative Humidity | %     | 57.04578 |
| ScenarioMIP   | AWI             | AWI-CM-1-1-MR | ssp585         | r1i1p1f1   | day       | 2050-01-02 20:00:00 | 36.93492 | 301.875 | hurs     | Near-Surface Relative Humidity | %     | 66.95392 |
| ScenarioMIP   | AWI             | AWI-CM-1-1-MR | ssp585         | r1i1p1f1   | day       | 2050-01-03 20:00:00 | 36.93492 | 301.875 | hurs     | Near-Surface Relative Humidity | %     | 71.37276 |
| ScenarioMIP   | AWI             | AWI-CM-1-1-MR | ssp585         | r1i1p1f1   | day       | 2050-01-04 20:00:00 | 36.93492 | 301.875 | hurs     | Near-Surface Relative Humidity | %     | 82.09089 |
| ScenarioMIP   | AWI             | AWI-CM-1-1-MR | ssp585         | r1i1p1f1   | day       | 2050-01-05 20:00:00 | 36.93492 | 301.875 | hurs     | Near-Surface Relative Humidity | %     | 65.37158 |
| ScenarioMIP   | AWI             | AWI-CM-1-1-MR | ssp585         | r1i1p1f1   | day       | 2050-01-06 20:00:00 | 36.93492 | 301.875 | hurs     | Near-Surface Relative Humidity | %     | 78.18507 |

### Morphing EPW weather variables

  - With all necessary information extracted above, now we can perform
    morphing on out EPW

<!-- end list -->

``` r
morphed <- morphing_epw(data)
#> Morphing 'dry bulb temperature'...
#> Morphing 'relative humidity'...
#> Morphing 'dew point temperature'...
#> Morphing 'atmospheric pressure'...
#> WARNING: Input does not contain any data of 'sea level pressure'. Skip.
#> Morphing 'horizontal infrared radiation from the sky'...
#> WARNING: Input does not contain any data of 'surface downwelling longware radiation'. Skip.
#> Morphing 'global horizontal radiation'...
#> WARNING: Input does not contain any data of 'surface downwelling shortware radiation'. Skip.
#> Morphing 'diffuse horizontal radiation'...
#> WARNING: Input does not contain any data of 'surface downwelling shortware radiation'. Skip.
#> Morphing 'direct normal radiation'...
#> WARNING: Input does not contain any data of 'surface downwelling shortware radiation'. Skip.
#> Morphing 'wind speed'...
#> WARNING: Input does not contain any data of 'near-surface wind speed'. Skip.
#> Morphing 'total sky cover'...
#> WARNING: Input does not contain any data of 'total cloud area fraction for the whole atmospheric column'. Skip.
#> Morphing 'opaque sky cover'...
#> WARNING: Input does not contain any data of 'total cloud area fraction for the whole atmospheric column'. Skip.

class(morphed)
#> [1] "epw_cmip6_morphed"

names(morphed)
#>  [1] "epw"          "tdb"          "tdew"         "rh"          
#>  [5] "p"            "hor_ir"       "glob_rad"     "norm_rad"    
#>  [9] "diff_rad"     "wind"         "total_cover"  "opaque_cover"

knitr::kable(head(morphed$tdb))
```

| activity\_drs | experiment\_id | institution\_id | source\_id    | member\_id | table\_id |     lon |      lat | interval | datetime            | year | month | day | hour | minute | dry\_bulb\_temperature |    delta |    alpha |
| :------------ | :------------- | :-------------- | :------------ | :--------- | :-------- | ------: | -------: | :------- | :------------------ | ---: | ----: | --: | ---: | -----: | ---------------------: | -------: | -------: |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 01:00:00 | 1999 |     1 |   1 |    1 |      0 |              13.056525 | 7.808153 | 1.813406 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 02:00:00 | 1999 |     1 |   1 |    2 |      0 |              13.056525 | 7.808153 | 1.813406 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 03:00:00 | 1999 |     1 |   1 |    3 |      0 |              12.149822 | 7.808153 | 1.813406 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 04:00:00 | 1999 |     1 |   1 |    4 |      0 |              11.061778 | 7.808153 | 1.813406 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 05:00:00 | 1999 |     1 |   1 |    5 |      0 |               7.978987 | 7.808153 | 1.813406 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 06:00:00 | 1999 |     1 |   1 |    6 |      0 |               7.978987 | 7.808153 | 1.813406 |

``` r

knitr::kable(head(morphed$rh))
```

| activity\_drs | experiment\_id | institution\_id | source\_id    | member\_id | table\_id |     lon |      lat | interval | datetime            | year | month | day | hour | minute | relative\_humidity |      delta |     alpha |
| :------------ | :------------- | :-------------- | :------------ | :--------- | :-------- | ------: | -------: | :------- | :------------------ | ---: | ----: | --: | ---: | -----: | -----------------: | ---------: | --------: |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 01:00:00 | 1999 |     1 |   1 |    1 |      0 |           75.94106 | \-12.70029 | 0.8437895 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 02:00:00 | 1999 |     1 |   1 |    2 |      0 |           75.94106 | \-12.70029 | 0.8437895 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 03:00:00 | 1999 |     1 |   1 |    3 |      0 |           75.09727 | \-12.70029 | 0.8437895 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 04:00:00 | 1999 |     1 |   1 |    4 |      0 |           78.47243 | \-12.70029 | 0.8437895 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 05:00:00 | 1999 |     1 |   1 |    5 |      0 |           81.84758 | \-12.70029 | 0.8437895 |
| ScenarioMIP   | ssp585         | AWI             | AWI-CM-1-1-MR | r1i1p1f1   | day       | 301.875 | 36.93492 | 2050     | 2017-01-01 06:00:00 | 1999 |     1 |   1 |    6 |      0 |           81.84758 | \-12.70029 | 0.8437895 |

### Create future EPW files

  - Once we get the morphed data using `morphing_epw()`, we can now
    create future EPW files using `future_epw()`

<!-- end list -->

``` r
# create future EPWs grouped by GCM, experiment ID, interval (year)
epws <- future_epw(morphed, by = c("source", "experiment", "interval"),
    dir = tempdir(), separate = TRUE, overwrite = TRUE
)
#> Warning: Empty morphed data found for variables listed below. Original data from EPW will be used:
#>  [1]: Atmospheric pressure
#>  [2]: Horizontal infrared radiation intensity from sky
#>  [3]: Global horizontal radiation
#>  [4]: Direct normal radiation
#>  [5]: Diffuse horizontal radiation
#>  [6]: Wind speed
#>  [7]: Total sky cover
#>  [8]: Opaque sky cover
#> ── Info ──────────────────────────────────────────────────────────────────
#> Data period #1 has been replaced with input data.
#> 
#>      Name StartDayOfWeek StartDay EndDay
#>  1:  Data         Sunday     1/ 1  12/31
#> ──────────────────────────────────────────────────────────────────────────
#> Replace the existing EPW file located at /tmp/RtmpDtbJVc/AWI-CM-1-1-MR/ssp585/2050/USA_CA_San.Francisco.Intl.AP.724940_TMY3.AWI-CM-1-1-MR.ssp585.2050.epw.
#> ── Info ──────────────────────────────────────────────────────────────────
#> Data period #1 has been replaced with input data.
#> 
#>      Name StartDayOfWeek StartDay EndDay
#>  1:  Data         Sunday     1/ 1  12/31
#> ──────────────────────────────────────────────────────────────────────────
#> Replace the existing EPW file located at /tmp/RtmpDtbJVc/AWI-CM-1-1-MR/ssp585/2080/USA_CA_San.Francisco.Intl.AP.724940_TMY3.AWI-CM-1-1-MR.ssp585.2080.epw.

epws
#> [[1]]
#> ══ EnergyPlus Weather File ═══════════════════════════════════════════════
#> [Location ]: San Francisco Intl Ap, CA, USA
#>              {N 37°37'}, {W 122°24'}, {UTC-08:00}
#> [Elevation]: 2m above see level
#> [Data Src ]: TMY3
#> [WMO Stat ]: 724940
#> [Leap Year]: No
#> [Interval ]: 60 mins
#> 
#> ── Data Periods ──────────────────────────────────────────────────────────
#>    Name StartDayOfWeek StartDay EndDay
#> 1: Data         Sunday     1/ 1  12/31
#> 
#> ──────────────────────────────────────────────────────────────────────────
#> 
#> [[2]]
#> ══ EnergyPlus Weather File ═══════════════════════════════════════════════
#> [Location ]: San Francisco Intl Ap, CA, USA
#>              {N 37°37'}, {W 122°24'}, {UTC-08:00}
#> [Elevation]: 2m above see level
#> [Data Src ]: TMY3
#> [WMO Stat ]: 724940
#> [Leap Year]: No
#> [Interval ]: 60 mins
#> 
#> ── Data Periods ──────────────────────────────────────────────────────────
#>    Name StartDayOfWeek StartDay EndDay
#> 1: Data         Sunday     1/ 1  12/31
#> 
#> ──────────────────────────────────────────────────────────────────────────

sapply(epws, function (epw) epw$path())
#> [1] "/tmp/RtmpDtbJVc/AWI-CM-1-1-MR/ssp585/2050/USA_CA_San.Francisco.Intl.AP.724940_TMY3.AWI-CM-1-1-MR.ssp585.2050.epw"
#> [2] "/tmp/RtmpDtbJVc/AWI-CM-1-1-MR/ssp585/2080/USA_CA_San.Francisco.Intl.AP.724940_TMY3.AWI-CM-1-1-MR.ssp585.2080.epw"
```

## Author

Hongyuan Jia and Adrian Chong

## License

  - **epwshiftr**
    
    epwshiftr is released under the terms of MIT License.
    
    Copyright © 2019-2020 Hongyuan Jia and Adrian Chong

  - **CMIP6 data**
    
    > To enable modeling groups and others who support CMIP6 to
    > demonstrate its impact (and secure ongoing funding), you are
    > required to cite and acknowledge those who have made CMIP6
    > possible. You also must abide by any licensing restrictions, which
    > are recorded in each file as a global attribute (named “license”).
    > 
    > Please carefully read and adhere to the [CMIP6 Terms of
    > Use](https://pcmdi.llnl.gov/CMIP6/TermsOfUse/).

## Contribute

Please note that the ‘epwshiftr’ project is released with a [Contributor
Code of
Conduct](https://github.com/ideas-lab-nus/epwshiftr/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
