
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epwshiftr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/hongyuanjia/epwshiftr.svg?branch=master)](https://travis-ci.com/hongyuanjia/epwshiftr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/hongyuanjia/epwshiftr?branch=master&svg=true)](https://ci.appveyor.com/project/hongyuanjia/epwshiftr)
[![CRAN
status](https://www.r-pkg.org/badges/version/epwshiftr)](https://CRAN.R-project.org/package=epwshiftr)
[![Codecov test
coverage](https://codecov.io/gh/hongyuanjia/epwshiftr/branch/master/graph/badge.svg)](https://codecov.io/gh/hongyuanjia/epwshiftr?branch=master)
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

<!-- You can install the released version of epwshiftr from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("epwshiftr") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("hongyuanjia/epwshiftr")
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
#>                         data_node status
#>  1:                aims3.llnl.gov     UP
#>  2:               cmip.bcc.cma.cn     UP
#>  3:     cmip.dess.tsinghua.edu.cn     UP
#>  4:               cmip.fio.org.cn     UP
#>  5:              cordexesg.dmi.dk     UP
#>  6:         crd-esgf-drc.ec.gc.ca     UP
#>  7:          data.meteo.unican.es     UP
#>  8:      dataserver.nccs.nasa.gov     UP
#>  9:          dist.nmlab.snu.ac.kr     UP
#> 10:       esg-cccr.tropmet.res.in     UP
#> 11:            esg-dn1.nsc.liu.se     UP
#> 12:              esg-dn1.ru.ac.th     UP
#> 13:            esg-dn2.nsc.liu.se     UP
#> 14:                esg.camscma.cn     UP
#> 15:                esg.lasg.ac.cn     UP
#> 16:            esg.pik-potsdam.de     UP
#> 17:              esg1.umr-cnrm.fr     UP
#> 18:        esgf-cnr.hpc.cineca.it     UP
#> 19:            esgf-data.ucar.edu     UP
#> 20:         esgf-data1.ceda.ac.uk     UP
#> 21:           esgf-data1.llnl.gov     UP
#> 22:         esgf-data2.ceda.ac.uk     UP
#> 23:         esgf-data2.diasjp.net     UP
#> 24:         esgf-data3.ceda.ac.uk     UP
#> 25:               esgf-dev.bsc.es     UP
#> 26:       esgf-ictp.hpc.cineca.it     UP
#> 27:             esgf-node.cmcc.it     UP
#> 28:            esgf-node2.cmcc.it     UP
#> 29:                  esgf.anl.gov     UP
#> 30:               esgf.apcc21.org     UP
#> 31:                   esgf.bsc.es     UP
#> 32:                   esgf.dwd.de     UP
#> 33:                 esgf.ichec.ie     UP
#> 34:            esgf.nccs.nasa.gov     UP
#> 35:               esgf.nci.org.au     UP
#> 36:       esgf.rcec.sinica.edu.tw     UP
#> 37:         noresg.nird.sigma2.no     UP
#> 38:             vesg.ipsl.upmc.fr     UP
#> 39: 145.100.59.180.surf-hosted.nl   DOWN
#> 40:         esgdata.gfdl.noaa.gov   DOWN
#> 41:         esgf-data1.diasjp.net   DOWN
#> 42:     esgf-nimscmip6.apcc21.org   DOWN
#> 43:                 esgf1.dkrz.de   DOWN
#> 44:                 esgf2.dkrz.de   DOWN
#> 45:                 esgf3.dkrz.de   DOWN
#> 46:  gpm1.gesdisc.eosdis.nasa.gov   DOWN
#>                         data_node status

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
    source = "EC-Earth3",

    # only consider data nodes that are current working
    data_node = nodes[status == "UP", data_node],

    # specify years of interest
    years = c(2050, 2080)
)
#> Querying CMIP6 Dataset Information
#> Querying CMIP6 File Information [Attempt 1]
#> Checking if data is complete
#> Data file index saved to '/tmp/Rtmp7yrkZW/cmip6_index.csv'

# the index has been automatically saved into directory specified using
# `epwshiftr.dir` option and can be reloaded
idx <- load_cmip6_index()

str(head(idx))
#> Classes 'data.table' and 'data.frame':   6 obs. of  22 variables:
#>  $ file_id           : chr  "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.day.tas.gr.v20200203.tas_day_EC-Earth3_ssp585_r"| __truncated__ "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.day.tas.gr.v20200203.tas_day_EC-Earth3_ssp585_r"| __truncated__ "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.day.tas.gr.v20200203.tas_day_EC-Earth3_ssp585_r"| __truncated__ "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.day.hurs.gr.v20200203.hurs_day_EC-Earth3_ssp585"| __truncated__ ...
#>  $ dataset_id        : chr  "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.day.tas.gr.v20200203|esg-dn2.nsc.liu.se" "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.day.tas.gr.v20200203|esg-dn2.nsc.liu.se" "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.day.tas.gr.v20200203|esg-dn2.nsc.liu.se" "CMIP6.ScenarioMIP.EC-Earth-Consortium.EC-Earth3.ssp585.r1i1p1f1.day.hurs.gr.v20200203|esg-dn2.nsc.liu.se" ...
#>  $ mip_era           : chr  "CMIP6" "CMIP6" "CMIP6" "CMIP6" ...
#>  $ activity_drs      : chr  "ScenarioMIP" "ScenarioMIP" "ScenarioMIP" "ScenarioMIP" ...
#>  $ institution_id    : chr  "EC-Earth-Consortium" "EC-Earth-Consortium" "EC-Earth-Consortium" "EC-Earth-Consortium" ...
#>  $ source_id         : chr  "EC-Earth3" "EC-Earth3" "EC-Earth3" "EC-Earth3" ...
#>  $ experiment_id     : chr  "ssp585" "ssp585" "ssp585" "ssp585" ...
#>  $ member_id         : chr  "r1i1p1f1" "r1i1p1f1" "r1i1p1f1" "r1i1p1f1" ...
#>  $ table_id          : chr  "day" "day" "day" "day" ...
#>  $ grid_label        : chr  "gr" "gr" "gr" "gr" ...
#>  $ version           : chr  "20200203" "20200203" "20200203" "20200203" ...
#>  $ nominal_resolution: chr  "100 km" "100 km" "100 km" "100 km" ...
#>  $ variable_id       : chr  "tas" "tas" "tas" "hurs" ...
#>  $ variable_long_name: chr  "Near-Surface Air Temperature" "Near-Surface Air Temperature" "Near-Surface Air Temperature" "Near-Surface Relative Humidity" ...
#>  $ variable_units    : chr  "K" "K" "K" "%" ...
#>  $ datetime_start    : POSIXct, format: "2049-01-01" "2050-01-01" ...
#>  $ datetime_end      : POSIXct, format: "2049-12-31" "2050-12-31" ...
#>  $ file_size         : int  140957772 141308277 141157930 161752607 161748507 161673372
#>  $ data_node         : chr  "esg-dn2.nsc.liu.se" "esg-dn2.nsc.liu.se" "esg-dn2.nsc.liu.se" "esg-dn2.nsc.liu.se" ...
#>  $ file_url          : chr  "http://esg-dn2.nsc.liu.se/thredds/fileServer/esg_dataroot1/cmip6data/CMIP6/ScenarioMIP/EC-Earth-Consortium/EC-E"| __truncated__ "http://esg-dn2.nsc.liu.se/thredds/fileServer/esg_dataroot1/cmip6data/CMIP6/ScenarioMIP/EC-Earth-Consortium/EC-E"| __truncated__ "http://esg-dn2.nsc.liu.se/thredds/fileServer/esg_dataroot1/cmip6data/CMIP6/ScenarioMIP/EC-Earth-Consortium/EC-E"| __truncated__ "http://esg-dn2.nsc.liu.se/thredds/fileServer/esg_dataroot1/cmip6data/CMIP6/ScenarioMIP/EC-Earth-Consortium/EC-E"| __truncated__ ...
#>  $ dataset_pid       : chr  "hdl:21.14100/d7ffe204-df79-308c-86e6-2b7466a11f9b" "hdl:21.14100/d7ffe204-df79-308c-86e6-2b7466a11f9b" "hdl:21.14100/d7ffe204-df79-308c-86e6-2b7466a11f9b" "hdl:21.14100/7cdf1215-c964-3a06-8ad7-913101e3278b" ...
#>  $ tracking_id       : chr  "hdl:21.14100/b7bbbb87-e3d6-4b0a-90e9-4a747a85df61" "hdl:21.14100/1e8651dc-f3d4-4761-94e0-4fb4227dcf09" "hdl:21.14100/47d24c86-b410-40d0-9d2d-42d7da09434d" "hdl:21.14100/e25ab7e8-5608-4823-ab41-a556f646d7f5" ...
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
#> 12 NetCDF files found.
#> Processing file /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20490...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20490101-20491231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20500...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20500101-20501231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20510...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20510101-20511231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20790...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20790101-20791231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20800...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20800101-20801231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20810...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20810101-20811231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_204901...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20490101-20491231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_205001...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20500101-20501231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_205101...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20510101-20511231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_207901...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20790101-20791231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_208001...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20800101-20801231.nc'...
#> Processing file /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_208101...
#> Parsing meta data of NetCDF file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20810101-20811231.nc'...
#> Data file index updated and saved to '/tmp/Rtmp7yrkZW/cmip6_index.csv'

knitr::kable(sm)
```

| variable\_id | source\_id | datetime\_start     | datetime\_end       | file\_num |     file\_size | dl\_num | dl\_percent |       dl\_size |
| :----------- | :--------- | :------------------ | :------------------ | --------: | -------------: | ------: | ----------: | -------------: |
| tas          | EC-Earth3  | 2049-01-01 12:00:00 | 2081-12-31 12:00:00 |         6 | 846 \[Mbytes\] |       6 |   100 \[%\] | 846 \[Mbytes\] |
| hurs         | EC-Earth3  | 2049-01-01 12:00:00 | 2081-12-31 12:00:00 |         6 | 971 \[Mbytes\] |       6 |   100 \[%\] | 971 \[Mbytes\] |

### Extract CMIP6 output data

  - With previous step, now we can match coordinates of an EPW in the
    CMIP6 output file

<!-- end list -->

``` r
epw <- file.path(eplusr::eplus_config(8.8)$dir, "WeatherData/USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw")
# match any coordinates with absolute distance less than 1 degree
coord <- match_coord(epw, threshold = list(lon = 1, lat = 1), max_num = 1)
#> Start to match coordinates...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20490...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20500...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20510...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2049...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2050...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2051...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20790...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20800...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20810...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2079...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2080...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2081...

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
#>                                                                      file_path
#>  1:  /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20490101-20491231.nc
#>  2:  /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20500101-20501231.nc
#>  3:  /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20510101-20511231.nc
#>  4: /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20490101-20491231.nc
#>  5: /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20500101-20501231.nc
#>  6: /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20510101-20511231.nc
#>  7:  /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20790101-20791231.nc
#>  8:  /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20800101-20801231.nc
#>  9:  /tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20810101-20811231.nc
#> 10: /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20790101-20791231.nc
#> 11: /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20800101-20801231.nc
#> 12: /tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_20810101-20811231.nc
#>      coord
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
#>   ..$ value: num 36.8
#>   ..$ dis  : num -0.778
#>   ..$ which: int 181
#>  $ lon:List of 4
#>   ..$ index: int 1
#>   ..$ value: num 302
#>   ..$ dis  : num -0.759
#>   ..$ which: int 430
```

  - Once we get the matched coordinates, we can extract corresponding
    data related to input EPW file using `extract_data()`

<!-- end list -->

``` r
data <- extract_data(coord, years = c(2050, 2080))
#> Start to extract CMIP6 data according to matched coordinates...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20490...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20500...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20510...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2049...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2050...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2051...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20790...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20800...
#> Processing file '/tmp/Rtmp7yrkZW/tas_day_EC-Earth3_ssp585_r1i1p1f1_gr_20810...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2079...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2080...
#> Processing file '/tmp/Rtmp7yrkZW/hurs_day_EC-Earth3_ssp585_r1i1p1f1_gr_2081...

class(data)
#> [1] "epw_cmip6_data"
names(data)
#> [1] "epw"  "meta" "data"
knitr::kable(head(data$data))
```

| activity\_drs | institution\_id     | source\_id | experiment\_id | member\_id | table\_id | datetime            |      lat |      lon | variable | description                  | units |    value |
| :------------ | :------------------ | :--------- | :------------- | :--------- | :-------: | :------------------ | -------: | -------: | :------- | :--------------------------- | :---- | -------: |
| ScenarioMIP   | EC-Earth-Consortium | EC-Earth3  | ssp585         | r1i1p1f1   |    day    | 2050-01-01 20:00:00 | 36.84202 | 301.6406 | tas      | Near-Surface Air Temperature | K     | 292.4459 |
| ScenarioMIP   | EC-Earth-Consortium | EC-Earth3  | ssp585         | r1i1p1f1   |    day    | 2050-01-02 20:00:00 | 36.84202 | 301.6406 | tas      | Near-Surface Air Temperature | K     | 289.5120 |
| ScenarioMIP   | EC-Earth-Consortium | EC-Earth3  | ssp585         | r1i1p1f1   |    day    | 2050-01-03 20:00:00 | 36.84202 | 301.6406 | tas      | Near-Surface Air Temperature | K     | 286.8790 |
| ScenarioMIP   | EC-Earth-Consortium | EC-Earth3  | ssp585         | r1i1p1f1   |    day    | 2050-01-04 20:00:00 | 36.84202 | 301.6406 | tas      | Near-Surface Air Temperature | K     | 290.4449 |
| ScenarioMIP   | EC-Earth-Consortium | EC-Earth3  | ssp585         | r1i1p1f1   |    day    | 2050-01-05 20:00:00 | 36.84202 | 301.6406 | tas      | Near-Surface Air Temperature | K     | 288.8000 |
| ScenarioMIP   | EC-Earth-Consortium | EC-Earth3  | ssp585         | r1i1p1f1   |    day    | 2050-01-06 20:00:00 | 36.84202 | 301.6406 | tas      | Near-Surface Air Temperature | K     | 288.6677 |

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
#> Morphing 'horizontal infrared radiation from the sky'...
#> Morphing 'global horizontal radiation'...
#> Morphing 'diffuse horizontal radiation'...
#> Morphing 'direct normal radiation'...
#> Morphing 'wind speed'...
#> Morphing 'total sky cover'...
#> Morphing 'opaque sky cover'...

class(morphed)
#> [1] "epw_cmip6_morphed"

names(morphed)
#>  [1] "epw"          "tdb"          "tdew"         "rh"           "p"           
#>  [6] "hor_ir"       "glob_rad"     "norm_rad"     "diff_rad"     "wind"        
#> [11] "total_cover"  "opaque_cover"

knitr::kable(head(morphed$tdb))
```

| activity\_drs | experiment\_id | institution\_id     | source\_id | member\_id | table\_id |      lon |      lat | interval | datetime            | year | month | day | hour | minute | dry\_bulb\_temperature |   delta |    alpha |
| :------------ | :------------- | :------------------ | :--------- | :--------- | :-------: | -------: | -------: | :------- | :------------------ | ---: | ----: | --: | ---: | -----: | ---------------------: | ------: | -------: |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 01:00:00 | 1999 |     1 |   1 |    1 |      0 |              13.050994 | 7.80078 | 1.812638 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 02:00:00 | 1999 |     1 |   1 |    2 |      0 |              13.050994 | 7.80078 | 1.812638 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 03:00:00 | 1999 |     1 |   1 |    3 |      0 |              12.144675 | 7.80078 | 1.812638 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 04:00:00 | 1999 |     1 |   1 |    4 |      0 |              11.057093 | 7.80078 | 1.812638 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 05:00:00 | 1999 |     1 |   1 |    5 |      0 |               7.975608 | 7.80078 | 1.812638 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 06:00:00 | 1999 |     1 |   1 |    6 |      0 |               7.975608 | 7.80078 | 1.812638 |

``` r

knitr::kable(head(morphed$rh))
```

| activity\_drs | experiment\_id | institution\_id     | source\_id | member\_id | table\_id |      lon |      lat | interval | datetime            | year | month | day | hour | minute | relative\_humidity |      delta |     alpha |
| :------------ | :------------- | :------------------ | :--------- | :--------- | :-------: | -------: | -------: | :------- | :------------------ | ---: | ----: | --: | ---: | -----: | -----------------: | ---------: | --------: |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 01:00:00 | 1999 |     1 |   1 |    1 |      0 |           80.54769 | \-8.538842 | 0.8949743 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 02:00:00 | 1999 |     1 |   1 |    2 |      0 |           80.54769 | \-8.538842 | 0.8949743 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 03:00:00 | 1999 |     1 |   1 |    3 |      0 |           79.65271 | \-8.538842 | 0.8949743 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 04:00:00 | 1999 |     1 |   1 |    4 |      0 |           83.23261 | \-8.538842 | 0.8949743 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 05:00:00 | 1999 |     1 |   1 |    5 |      0 |           86.81251 | \-8.538842 | 0.8949743 |
| ScenarioMIP   | ssp585         | EC-Earth-Consortium | EC-Earth3  | r1i1p1f1   |    day    | 301.6406 | 36.84202 | 2050     | 2017-01-01 06:00:00 | 1999 |     1 |   1 |    6 |      0 |           86.81251 | \-8.538842 | 0.8949743 |

### Create future EPW files

  - Once we get the morphed data using `morphing_epw()`, we can now
    create future EPW files using `future_epw()`

<!-- end list -->

``` r
# create future EPWs grouped by GCM, experiment ID, interval (year)
epws <- future_epw(morphed, by = c("source_id", "experiment_id", "interval"),
    dir = tempdir(), separate = TRUE
)
#> ── Info ────────────────────────────────────────────────────────────────────────
#> Data period #1 has been replaced with input data.
#>      Name StartDayOfWeek StartDay EndDay
#>  1:  Data         Sunday     1/ 1  12/31
#> ── Info ────────────────────────────────────────────────────────────────────────
#> Data period #1 has been replaced with input data.
#>      Name StartDayOfWeek StartDay EndDay
#>  1:  Data         Sunday     1/ 1  12/31

epws
#> [[1]]
#> ══ EnergyPlus Weather File ═════════════════════════════════════════════════════
#> [Location ]: San Francisco Intl Ap, CA, USA
#>              {N 37°37'}, {W 122°24'}, {UTC-08:00}
#> [Elevation]: 2m above see level
#> [Data Src ]: TMY3
#> [WMO Stat ]: 724940
#> [Leap Year]: FALSE
#> [Interval ]: 60 mins
#> 
#> ── Data Periods ────────────────────────────────────────────────────────────────
#>    Name StartDayOfWeek StartDay EndDay
#> 1: Data         Sunday     1/ 1  12/31
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> [[2]]
#> ══ EnergyPlus Weather File ═════════════════════════════════════════════════════
#> [Location ]: San Francisco Intl Ap, CA, USA
#>              {N 37°37'}, {W 122°24'}, {UTC-08:00}
#> [Elevation]: 2m above see level
#> [Data Src ]: TMY3
#> [WMO Stat ]: 724940
#> [Leap Year]: FALSE
#> [Interval ]: 60 mins
#> 
#> ── Data Periods ────────────────────────────────────────────────────────────────
#>    Name StartDayOfWeek StartDay EndDay
#> 1: Data         Sunday     1/ 1  12/31
#> 
#> ────────────────────────────────────────────────────────────────────────────────

sapply(epws, function (epw) epw$path())
#> [1] "/tmp/Rtmp7yrkZW/EC-Earth3/ssp585/2050/USA_CA_San.Francisco.Intl.AP.724940_TMY3.EC-Earth3.ssp585.2050.epw"
#> [2] "/tmp/Rtmp7yrkZW/EC-Earth3/ssp585/2080/USA_CA_San.Francisco.Intl.AP.724940_TMY3.EC-Earth3.ssp585.2080.epw"
```

## Author

Hongyuan Jia

## License

  - **epwshiftr**
    
    epwshiftr is released under the terms of MIT License.
    
    Copyright © 2019 Hongyuan Jia

  - **CMIP6 data**
    
    > To enable modeling groups and others who support CMIP6 to
    > demonstrate its impact (and secure ongoing funding), you are
    > required to cite and acknowledge those who have made CMIP6
    > possible. You also must abide by any licensing restrictions, which
    > are recorded in each file as a global attribute (named “license”).
    > 
    > Please carefully read and adhere to the [CMIP6 Terms of
    > Use](https://pcmdi.llnl.gov/CMIP6/TermsOfUse).

## Contribute

Please note that the ‘epwshiftr’ project is released with a [Contributor
Code of Conduct](https://github.com/hongyuanjia/epwshiftr/blob/master/.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
