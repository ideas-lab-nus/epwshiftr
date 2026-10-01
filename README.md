
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

Inspect the same batch directly in R. The output and summary are
`data.table` results; temperatures in the weather summary are in °C.

<details>

<summary>

Show the inspection code and results
</summary>

``` r
shift_status(batch)
#> [1] "completed"
shift_outputs(batch)
#>         site_id
#>          <char>
#> 1: SanFrancisco
#> 2: SanFrancisco
#> 3:    Singapore
#> 4:    Singapore
#>                                                                                               child_key
#>                                                                                                  <char>
#> 1: SanFrancisco-3ca0298aa4f7--monthly-original_morphing-original_morphing_field_equations--IPSL-CM6A-LR
#> 2:                           SanFrancisco-3ca0298aa4f7--monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR
#> 3:    Singapore-c96dd81db196--monthly-original_morphing-original_morphing_field_equations--IPSL-CM6A-LR
#> 4:                              Singapore-c96dd81db196--monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR
#>               method   scale                    reconstruction        model
#>               <char>  <char>                            <char>       <char>
#> 1: original_morphing monthly original_morphing_field_equations IPSL-CM6A-LR
#> 2:          bws_btws monthly                  bws_btws_weather IPSL-CM6A-LR
#> 3: original_morphing monthly original_morphing_field_equations IPSL-CM6A-LR
#> 4:          bws_btws monthly                  bws_btws_weather IPSL-CM6A-LR
#>      member   grid
#>      <char> <char>
#> 1: r1i1p1f1     gr
#> 2: r1i1p1f1     gr
#> 3: r1i1p1f1     gr
#> 4: r1i1p1f1     gr
#>                                                           output_id
#>                                                              <char>
#> 1: 028b7f965e7fbc1915b4958a5859355c2d7044c3f6f424e2f6291494ef661287
#> 2: 2c7ee9db64c14b3ecb407c11c176c26567d0253d972f8d79d7af73c68059f6ac
#> 3: 89e89218580705602d4b5edd44e3128df408649288acd3fd1fe7c8143962318f
#> 4: 0568c8d5464a5c2c98335a3b05df06c527135a1631dc312fc51b18882faf9d80
#>                                                            morph_id
#>                                                              <char>
#> 1: 58e24fc1602f893276c1065c8026972d6bb36ca24fef13f07dd523309ce4ddb7
#> 2: 40a0f3bc71e17f6abf1928ecc4ec65fbd29bba2cdea975ae7d6e3b58b567884b
#> 3: 91c6dc9af796735d20c01df0653b418a4387275eebd41dbc111d02f878d86193
#> 4: 60d591dbbe3056d1a3c54579a653b5843030487f0330aa3b5fb601f6f2508380
#>                                                             case_id
#>                                                              <char>
#> 1: 72b31998a292faee41e2e34fd24dd9b9a2d0c3fe968747944bffc57ed9963b65
#> 2: 52a09968111bd838e40aac8b86d5b771d3ea762e3ff52318a9248035d6c9e05d
#> 3: 3d54d0a7a0de6bf33a2be5a0ff5ab1fdfc8d4e811e6b8f2e875fdcccdf6bb044
#> 4: fc726297177bdfbb46519737245f9475ac8e2b62c7ace9486a072492f85de213
#>                                                           result_id
#>                                                              <char>
#> 1: 42e3b7919e637468641009a157e27a621c282ec061f01d087044d93886e24ace
#> 2: 8d7bc65880557d786c49523ac6141ed1fd9d546544814361c7d07bf0b719e52f
#> 3: 48c1985072bdc22655186697720a6c41a7a0adb9d7ddf88956e59558d7baa04b
#> 4: e9ceec68a7d1d3294d72ba329889b4c882897ec4279e92a23a192840034c54c5
#>                                                         artifact_id
#>                                                              <char>
#> 1: 93796a1e93aba13e9a71febb740c4addbd64de4e923cad58565e68e602c73807
#> 2: 9e62636835b33fa7cf16e57aadab45b7f71aceea0675ee8ebe53c856bbba8f97
#> 3: 04ea4940bef220e25d12529c53b4d9fd1671398e7a3dd23c19155c846429158e
#> 4: 52da383adb94641a4848b5d85c26b4f25f34fbb2d5183908988bfb8d9cb27d53
#>                                                                                                                      path
#>                                                                                                                    <char>
#> 1: outputs/future-epw/IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s/a9635d76da09-SanFrancisco.IPSL-CM6A-LR.ssp245.r1i1p1f1.2060s.epw
#> 2: outputs/future-epw/IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s/a9635d76da09-SanFrancisco.IPSL-CM6A-LR.ssp245.r1i1p1f1.2060s.epw
#> 3:    outputs/future-epw/IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s/2614f7b88616-Singapore.IPSL-CM6A-LR.ssp245.r1i1p1f1.2060s.epw
#> 4:    outputs/future-epw/IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s/2614f7b88616-Singapore.IPSL-CM6A-LR.ssp245.r1i1p1f1.2060s.epw
#>       source_id experiment_id variant_label period         output_type
#>          <char>        <char>        <char> <char>              <char>
#> 1: IPSL-CM6A-LR        ssp245      r1i1p1f1  2060s representative_year
#> 2: IPSL-CM6A-LR        ssp245      r1i1p1f1  2060s representative_year
#> 3: IPSL-CM6A-LR        ssp245      r1i1p1f1  2060s representative_year
#> 4: IPSL-CM6A-LR        ssp245      r1i1p1f1  2060s representative_year
#>    sequence_id weather_year calendar stochastic_seed member_count
#>         <char>        <int>   <char>           <int>        <int>
#> 1:        <NA>           NA     <NA>              NA            1
#> 2:        <NA>           NA     <NA>              NA            1
#> 3:        <NA>           NA     <NA>              NA            1
#> 4:        <NA>           NA     <NA>              NA            1
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            provenance_json
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <char>
#> 1:                                                                                                                                                                                               {"weather_field_roles":{"transformed_fields":["atmospheric_pressure","dry_bulb_temperature","global_horizontal_radiation","horizontal_infrared_radiation_intensity_from_sky","relative_humidity","total_sky_cover","wind_speed"],"derived_fields":["dew_point_temperature","diffuse_horizontal_radiation","direct_normal_radiation","liquid_precip_rate","opaque_sky_cover"],"physically_closed_fields":[],"inherited_fields":["aerosol_optical_depth","albedo","ceiling_height","days_since_last_snow","diffuse_horizontal_illuminance","direct_normal_illuminance","extraterrestrial_direct_normal_radiation","extraterrestrial_horizontal_radiation","global_horizontal_illuminance","liquid_precip_depth","precipitable_water","present_weather_codes","present_weather_observation","snow_depth","visibility","wind_direction","zenith_luminance"]}}
#> 2: {"weather_field_roles":{"transformed_fields":["dry_bulb_temperature","global_horizontal_radiation","total_sky_cover"],"derived_fields":["dew_point_temperature","diffuse_horizontal_radiation","direct_normal_radiation","opaque_sky_cover","relative_humidity"],"physically_closed_fields":["dew_point_temperature","diffuse_horizontal_radiation","direct_normal_radiation","dry_bulb_temperature","global_horizontal_radiation","opaque_sky_cover","relative_humidity","total_sky_cover"],"inherited_fields":["aerosol_optical_depth","albedo","atmospheric_pressure","ceiling_height","days_since_last_snow","diffuse_horizontal_illuminance","direct_normal_illuminance","extraterrestrial_direct_normal_radiation","extraterrestrial_horizontal_radiation","global_horizontal_illuminance","horizontal_infrared_radiation_intensity_from_sky","liquid_precip_depth","liquid_precip_rate","precipitable_water","present_weather_codes","present_weather_observation","snow_depth","visibility","wind_direction","wind_speed","zenith_luminance"]}}
#> 3:                                                                                                                                                                                               {"weather_field_roles":{"transformed_fields":["atmospheric_pressure","dry_bulb_temperature","global_horizontal_radiation","horizontal_infrared_radiation_intensity_from_sky","relative_humidity","total_sky_cover","wind_speed"],"derived_fields":["dew_point_temperature","diffuse_horizontal_radiation","direct_normal_radiation","liquid_precip_rate","opaque_sky_cover"],"physically_closed_fields":[],"inherited_fields":["aerosol_optical_depth","albedo","ceiling_height","days_since_last_snow","diffuse_horizontal_illuminance","direct_normal_illuminance","extraterrestrial_direct_normal_radiation","extraterrestrial_horizontal_radiation","global_horizontal_illuminance","liquid_precip_depth","precipitable_water","present_weather_codes","present_weather_observation","snow_depth","visibility","wind_direction","zenith_luminance"]}}
#> 4: {"weather_field_roles":{"transformed_fields":["dry_bulb_temperature","global_horizontal_radiation","total_sky_cover"],"derived_fields":["dew_point_temperature","diffuse_horizontal_radiation","direct_normal_radiation","opaque_sky_cover","relative_humidity"],"physically_closed_fields":["dew_point_temperature","diffuse_horizontal_radiation","direct_normal_radiation","dry_bulb_temperature","global_horizontal_radiation","opaque_sky_cover","relative_humidity","total_sky_cover"],"inherited_fields":["aerosol_optical_depth","albedo","atmospheric_pressure","ceiling_height","days_since_last_snow","diffuse_horizontal_illuminance","direct_normal_illuminance","extraterrestrial_direct_normal_radiation","extraterrestrial_horizontal_radiation","global_horizontal_illuminance","horizontal_infrared_radiation_intensity_from_sky","liquid_precip_depth","liquid_precip_rate","precipitable_water","present_weather_codes","present_weather_observation","snow_depth","visibility","wind_direction","wind_speed","zenith_luminance"]}}
#>             created_at
#>                 <POSc>
#> 1: 2026-10-01 16:59:21
#> 2: 2026-10-01 16:59:39
#> 3: 2026-10-01 17:00:05
#> 4: 2026-10-01 17:00:24
#>                                                                                                                                                                                                                                                                                      export_path
#>                                                                                                                                                                                                                                                                                           <char>
#> 1: /Users/hongyuanjia/Library/Caches/org.R-project.R/R/epwshiftr/readme/future-epw-batch/SanFrancisco-3ca0298aa4f7/monthly-original_morphing-original_morphing_field_equations--IPSL-CM6A-LR/IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s/a9635d76da09-SanFrancisco.IPSL-CM6A-LR.ssp245.r1i1p1f1.2060s.epw
#> 2:                           /Users/hongyuanjia/Library/Caches/org.R-project.R/R/epwshiftr/readme/future-epw-batch/SanFrancisco-3ca0298aa4f7/monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR/IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s/a9635d76da09-SanFrancisco.IPSL-CM6A-LR.ssp245.r1i1p1f1.2060s.epw
#> 3:       /Users/hongyuanjia/Library/Caches/org.R-project.R/R/epwshiftr/readme/future-epw-batch/Singapore-c96dd81db196/monthly-original_morphing-original_morphing_field_equations--IPSL-CM6A-LR/IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s/2614f7b88616-Singapore.IPSL-CM6A-LR.ssp245.r1i1p1f1.2060s.epw
#> 4:                                 /Users/hongyuanjia/Library/Caches/org.R-project.R/R/epwshiftr/readme/future-epw-batch/Singapore-c96dd81db196/monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR/IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s/2614f7b88616-Singapore.IPSL-CM6A-LR.ssp245.r1i1p1f1.2060s.epw
shift_summary(batch, weather = TRUE)
#>         site_id
#>          <char>
#> 1: SanFrancisco
#> 2: SanFrancisco
#> 3:    Singapore
#> 4:    Singapore
#>                                                                                               child_key
#>                                                                                                  <char>
#> 1: SanFrancisco-3ca0298aa4f7--monthly-original_morphing-original_morphing_field_equations--IPSL-CM6A-LR
#> 2:                           SanFrancisco-3ca0298aa4f7--monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR
#> 3:    Singapore-c96dd81db196--monthly-original_morphing-original_morphing_field_equations--IPSL-CM6A-LR
#> 4:                              Singapore-c96dd81db196--monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR
#>               method   scale                    reconstruction
#>               <char>  <char>                            <char>
#> 1: original_morphing monthly original_morphing_field_equations
#> 2:          bws_btws monthly                  bws_btws_weather
#> 3: original_morphing monthly original_morphing_field_equations
#> 4:          bws_btws monthly                  bws_btws_weather
#>                                                            batch_id
#>                                                              <char>
#> 1: 08a502ca51f59cdba857db31da8c31d2aa8fc3065b2e242cd861aeafec0ca517
#> 2: 08a502ca51f59cdba857db31da8c31d2aa8fc3065b2e242cd861aeafec0ca517
#> 3: 08a502ca51f59cdba857db31da8c31d2aa8fc3065b2e242cd861aeafec0ca517
#> 4: 08a502ca51f59cdba857db31da8c31d2aa8fc3065b2e242cd861aeafec0ca517
#>           model scenario   member   grid period    status cases completed_cases
#>          <char>   <char>   <char> <char> <char>    <char> <int>           <int>
#> 1: IPSL-CM6A-LR   ssp245 r1i1p1f1     gr  2060s completed     1               1
#> 2: IPSL-CM6A-LR   ssp245 r1i1p1f1     gr  2060s completed     1               1
#> 3: IPSL-CM6A-LR   ssp245 r1i1p1f1     gr  2060s completed     1               1
#> 4: IPSL-CM6A-LR   ssp245 r1i1p1f1     gr  2060s completed     1               1
#>    epw_files available_files         output_type weather_years warnings errors
#>        <int>           <int>              <char>        <char>    <int>  <int>
#> 1:         1               1 representative_year                      0      0
#> 2:         1               1 representative_year                      1      0
#> 3:         1               1 representative_year                      0      0
#> 4:         1               1 representative_year                      0      0
#>                                                       field_roles weather_hours
#>                                                            <char>         <int>
#> 1: 7 transformed · 5 derived · 0 physically closed · 17 inherited          8760
#> 2: 3 transformed · 5 derived · 8 physically closed · 21 inherited          8760
#> 3: 7 transformed · 5 derived · 0 physically closed · 17 inherited          8760
#> 4: 3 transformed · 5 derived · 8 physically closed · 21 inherited          8760
#>    unreadable_files mean_temperature_c temperature_hours
#>               <int>              <num>             <num>
#> 1:                0           15.42045              8760
#> 2:                0           15.42045              8760
#> 3:                0           28.28410              8760
#> 4:                0           28.28410              8760
#>    mean_relative_humidity_pct humidity_hours mean_wind_speed_ms wind_hours
#>                         <num>          <num>              <num>      <num>
#> 1:                   73.62764           8760           4.693441       8760
#> 2:                   67.10022           8760           4.670753       8760
#> 3:                   78.79673           8760           2.564993       8760
#> 4:                   72.35275           8760           2.500000       8760
#>    mean_global_horizontal_radiation_wh_m2 radiation_hours weather_error
#>                                     <num>           <num>        <char>
#> 1:                               197.2366            8760          <NA>
#> 2:                               197.1229            8760          <NA>
#> 3:                               247.3633            8760          <NA>
#> 4:                               248.5584            8760          <NA>
shift_diagnostics(batch)
#>      stage severity                      code
#>     <char>   <char>                    <char>
#> 1: runtime  warning       bws_target_adjusted
#> 2: runtime     info bws_btws_shortwave_closed
#> 3: runtime     info bws_btws_shortwave_closed
#>                                                                                                                                                                                                                                                                  message
#>                                                                                                                                                                                                                                                                   <char>
#> 1: BWS adjusted clt month 8 for IPSL-CM6A-LR/ssp245/r1i1p1f1/2060s from 17.301198 to 6.1290323 within the attainable interval [1.4919355, 6.1290323] (physical_upper_bound); GCM historical mean 0.34586296, GCM future mean 1.5648434, and baseline EPW mean 3.8239247.
#> 2:                                                                                                                                                                                                    BWS/BTWS shortwave physical closure adjusted 2 candidate state(s).
#> 3:                                                                                                                                                                                                  BWS/BTWS shortwave physical closure adjusted 198 candidate state(s).
#>    query_id session_id plan_id summary_id baseline_id
#>      <char>     <char>  <char>     <char>      <char>
#> 1:     <NA>       <NA>    <NA>       <NA>        <NA>
#> 2:     <NA>       <NA>    <NA>       <NA>        <NA>
#> 3:     <NA>       <NA>    <NA>       <NA>        <NA>
#>                                                            morph_id
#>                                                              <char>
#> 1: 40a0f3bc71e17f6abf1928ecc4ec65fbd29bba2cdea975ae7d6e3b58b567884b
#> 2: 40a0f3bc71e17f6abf1928ecc4ec65fbd29bba2cdea975ae7d6e3b58b567884b
#> 3: 60d591dbbe3056d1a3c54579a653b5843030487f0330aa3b5fb601f6f2508380
#>                                                             case_id variable_id
#>                                                              <char>      <char>
#> 1: 52a09968111bd838e40aac8b86d5b771d3ea762e3ff52318a9248035d6c9e05d         clt
#> 2: 52a09968111bd838e40aac8b86d5b771d3ea762e3ff52318a9248035d6c9e05d        rsds
#> 3: fc726297177bdfbb46519737245f9475ac8e2b62c7ace9486a072492f85de213        rsds
#>                      epw_field period  month
#>                         <char> <char> <char>
#> 1:             total_sky_cover  2060s      8
#> 2: global_horizontal_radiation  2060s   <NA>
#> 3: global_horizontal_radiation  2060s   <NA>
#>                                                                                                                 action
#>                                                                                                                 <char>
#> 1: Inspect bws_factors for the requested and applied targets; the Historical-to-Scenario signal is retained unchanged.
#> 2:                                                            Inspect the physical-policy corrections and bws_factors.
#> 3:                                                            Inspect the physical-policy corrections and bws_factors.
#>         site_id
#>          <char>
#> 1: SanFrancisco
#> 2: SanFrancisco
#> 3:    Singapore
#>                                                                     child_key
#>                                                                        <char>
#> 1: SanFrancisco-3ca0298aa4f7--monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR
#> 2: SanFrancisco-3ca0298aa4f7--monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR
#> 3:    Singapore-c96dd81db196--monthly-bws_btws-bws_btws_weather--IPSL-CM6A-LR
#>      method   scale   reconstruction        model   member   grid
#>      <char>  <char>           <char>       <char>   <char> <char>
#> 1: bws_btws monthly bws_btws_weather IPSL-CM6A-LR r1i1p1f1     gr
#> 2: bws_btws monthly bws_btws_weather IPSL-CM6A-LR r1i1p1f1     gr
#> 3: bws_btws monthly bws_btws_weather IPSL-CM6A-LR r1i1p1f1     gr
```

</details>

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
