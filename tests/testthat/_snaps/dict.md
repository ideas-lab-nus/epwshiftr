# Cmip6Dict$get()

    Code
      print(dict$get("drs"))
    Message <cliMessage>
      == Cmip6CV Data Reference Syntax (DRS) ===================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * DRS Modified: yyyy-mm-dd HH:MM:SS UTC
      * DRS Note: "Add CMIP6 Data Reference Syntax (DRS) templates"
      
      -- Values <list> ---------------------------------------------------------------
      * Directory path example:
        "CMIP6/CMIP/MOHC/HadGEM3-GC31-MM/historical/r1i1p1f3/Amon/tas/gn/v20191207/"
      * Directory path sub experiment example:
        "CMIP6/DCPP/MOHC/HadGEM3-GC31-MM/dcppA-hindcast/s1960-r1i1p1f2/Amon/tas/gn/v20200417/"
      * Directory path template:
        "<mip_era>/<activity_id>/<institution_id>/<source_id>/<experiment_id>/<member_id>/<table_id>/<variable_id>/<grid_label>/<version>"
      * Filename example:
        "tas_Amon_HadGEM3-GC31-MM_historical_r1i1p1f3_gn_185001-186912.nc"
      * Filename sub experiment example:
        "tas_Amon_HadGEM3-GC31-MM_dcppA-hindcast_s1960-r1i1p1f2_gn_196011-196012.nc"
      # ... with 1 more item

---

    Code
      print(dict$get("activity_id"))
    Message <cliMessage>
      == Cmip6CV Activity ID ===================================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * ActivityId Modified: yyyy-mm-dd HH:MM:SS UTC
      * ActivityId Note: "Update activity_id to include CDRMIP and PAMIP"
      
      -- Values <list> ---------------------------------------------------------------
      * AerChemMIP: "Aerosols and Chemistry Model Intercomparison Project"
      * C4MIP: "Coupled Climate Carbon Cycle Model Intercomparison Project"
      * CDRMIP: "Carbon Dioxide Removal Model Intercomparison Project"
      * CFMIP: "Cloud Feedback Model Intercomparison Project"
      * CMIP: "CMIP DECK: 1pctCO2, abrupt4xCO2, amip, esm-piControl,
        esm-historical, historical, and piControl experiments"
      # ... with 19 more items

---

    Code
      print(dict$get("experiment_id"))
    Message <cliMessage>
      == Cmip6CV Experiment ID =================================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * ExperimentId Modified: yyyy-mm-dd HH:MM:SS UTC
      * ExperimentId Note: "Revise experiment_id historical parent experiments"
      
      -- Values <data.table> ---------------------------------------------------------
      
        -- [Experiment id: "1pctCO2"] --
        * Experiment: "1 percent per year increase in CO2"
        * Description: "DECK: 1pctCO2"
        * Tier: 1
        * Start year: NA
        * End year: NA
        * Min number yrs per sim: 150
        * Required model components: "AOGCM"
        * Parent experiment id: "piControl"
        * Sub experiment id: "none"
        * Activity id: "CMIP"
        * Parent activity id: "CMIP"
        * Additional allowed model components: "AER", "CHEM", and "BGC"
      
        -- [Experiment id: "1pctCO2-4xext"] --
        * Experiment: "extension from year 140 of 1pctCO2 with 4xCO2"
        * Description: "branched from 1pctCO2 run at year 140 and run with CO2
          fixed at 4x pre-industrial concentration"
        * Tier: 1
        * Start year: NA
        * End year: NA
        * Min number yrs per sim: 210
        * Required model components: "AOGCM"
        * Parent experiment id: "1pctCO2"
        * Sub experiment id: "none"
        * Activity id: "ISMIP6"
        * Parent activity id: "CMIP"
        * Additional allowed model components: "AER", "CHEM", and "BGC"
      
        -- [Experiment id: "1pctCO2-bgc"] --
        * Experiment: "biogeochemically-coupled version of 1 percent per year
          increasing CO2 experiment"
        * Description: "Biogeochemically-coupled specified concentration simulation
          in which CO2 increases at a rate of 1% per year until quadrupling"
        * Tier: 1
        * Start year: NA
        * End year: NA
        * Min number yrs per sim: 150
        * Required model components: "AOGCM" and "BGC"
        * Parent experiment id: "piControl"
        * Sub experiment id: "none"
        * Activity id: "C4MIP"
        * Parent activity id: "CMIP"
        * Additional allowed model components: "AER" and "CHEM"
      
      # ... with 319 more items

---

    Code
      print(dict$get("frequency"))
    Message <cliMessage>
      == Cmip6CV Frequency ======================================= CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * Frequency Modified: yyyy-mm-dd HH:MM:SS UTC
      * Frequency Note: "Update description of 3hr and 6hr frequencies"
      
      -- Values <list> ---------------------------------------------------------------
      * 1hr: "sampled hourly"
      * 1hrCM: "monthly-mean diurnal cycle resolving each day into 1-hour means"
      * 1hrPt: "sampled hourly, at specified time point within an hour"
      * 3hr: "3 hourly mean samples"
      * 3hrPt: "sampled 3 hourly, at specified time point within the time period"
      # ... with 11 more items

---

    Code
      print(dict$get("grid_label"))
    Message <cliMessage>
      == Cmip6CV Grid Label ====================================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * GridLabel Modified: yyyy-mm-dd HH:MM:SS UTC
      * GridLabel Note: "Issue395 durack1 augment grid_label with description
        (#401)"
      
      -- Values <list> ---------------------------------------------------------------
      * gm: "global mean data"
      * gn: "data reported on a model's native grid"
      * gna: "data reported on a native grid in the region of Antarctica"
      * gng: "data reported on a native grid in the region of Greenland"
      * gnz: "zonal mean data reported on a model's native latitude grid"
      # ... with 40 more items

---

    Code
      print(dict$get("institution_id"))
    Message <cliMessage>
      == Cmip6CV Institution ID ================================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * InstitutionId Modified: yyyy-mm-dd HH:MM:SS UTC
      * InstitutionId Note: "Register institution_id UCSB for E3SM-1-0"
      
      -- Values <list> ---------------------------------------------------------------
      * AER: "Research and Climate Group, Atmospheric and Environmental Research,
        131 Hartwell Avenue, Lexington, MA 02421, USA"
      * AS-RCEC: "Research Center for Environmental Changes, Academia Sinica,
        Nankang, Taipei 11529, Taiwan"
      * AWI: "Alfred Wegener Institute, Helmholtz Centre for Polar and Marine
        Research, Am Handelshafen 12, 27570 Bremerhaven, Germany"
      * BCC: "Beijing Climate Center, Beijing 100081, China"
      * CAMS: "Chinese Academy of Meteorological Sciences, Beijing 100081, China"
      # ... with 44 more items

---

    Code
      print(dict$get("nominal_resolution"))
    Message <cliMessage>
      == Cmip6CV Nominal Resolution ============================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * NominalResolution Modified: yyyy-mm-dd HH:MM:SS UTC
      * NominalResolution Note: "Issue141 durack1 update grid_resolution to
        nominal_resolution (#143)"
      
      -- Values <character> ----------------------------------------------------------
      "0.5 km", "1 km", "10 km", "100 km", "1000 km", "10000 km", "1x1 degree", "2.5
      km", ..., "500 km", and "5000 km"
      # ... with 5 more items

---

    Code
      print(dict$get("realm"))
    Message <cliMessage>
      == Cmip6CV Realm =========================================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * Realm Modified: yyyy-mm-dd HH:MM:SS UTC
      * Realm Note: "Issue285 durack1 update realm format (#290)"
      
      -- Values <list> ---------------------------------------------------------------
      * aerosol: "Aerosol"
      * atmos: "Atmosphere"
      * atmosChem: "Atmospheric Chemistry"
      * land: "Land Surface"
      * landIce: "Land Ice"
      # ... with 3 more items

---

    Code
      print(dict$get("required_global_attributes"))
    Message <cliMessage>
      == Cmip6CV Required Global Attributes ====================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * ReqGlobAttr Modified: yyyy-mm-dd HH:MM:SS UTC
      * ReqGlobAttr Note: "Reverting addition of external_variables to
        required_global_attributes"
      
      -- Values <character> ----------------------------------------------------------
      "Conventions", "activity_id", "creation_date", "data_specs_version",
      "experiment", "experiment_id", "forcing_index", "frequency", ...,
      "variable_id", and "variant_label"
      # ... with 20 more items

---

    Code
      print(dict$get("source_id"))
    Message <cliMessage>
      == Cmip6CV Source ID ======================================= CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * SourceId Modified: yyyy-mm-dd HH:MM:SS UTC
      * SourceId Note: "Revised E3SM-2-0 source_id license history"
      
      -- Values <data.table> ---------------------------------------------------------
      
        -- [Source id: "4AOP-v1-5"] --
        * Release year: 2019
        * Institution id: "IPSL"
        * Label: "4AOP-v1-5"
        * Label extended: "Line-By-Line Radiative Transfer Model v1.5, Laboratoire
          Meteorologie Dynamique, GEISA spectroscopic database"
        * Cohort: "Published"
        * Activity participation: "RFMIP"
        * Model component:
            * Aerosol:
              * Description: "none"
              * Native nominal resolution: "none"
            * Atmos:
              * Description: "none"
              * Native nominal resolution: "none"
            * AtmosChem:
              * Description: "none"
              * Native nominal resolution: "none"
            * Land:
              * Description: "none"
              * Native nominal resolution: "none"
            * LandIce:
              * Description: "none"
              * Native nominal resolution: "none"
            * Ocean:
              * Description: "none"
              * Native nominal resolution: "none"
            * OcnBgchem:
              * Description: "none"
              * Native nominal resolution: "none"
            * SeaIce:
              * Description: "none"
              * Native nominal resolution: "none"
        * License info:
            * Exceptions contact: "@listes.ipsl.fr <- ipsl-cmip6"
            * History: "2020-06-11: initially published under CC BY-NC-SA 4.0;
              2022-06-03: relaxed to CC BY 4.0"
            * Id: "CC BY 4.0"
            * License: "Creative Commons Attribution 4.0 International (CC BY 4.0;
              https://creativecommons.org/licenses/by/4.0/)"
            * Source specific info: ""
            * Url: "https://creativecommons.org/licenses/by/4.0/"
      
        -- [Source id: "ACCESS-CM2"] --
        * Release year: 2019
        * Institution id: "CSIRO-ARCCSS"
        * Label: "ACCESS-CM2"
        * Label extended: "Australian Community Climate and Earth System Simulator
          Climate Model Version 2"
        * Cohort: "Published"
        * Activity participation: "CMIP", "DAMIP", "FAFMIP", "OMIP", "RFMIP",
          "SIMIP", and "ScenarioMIP"
        * Model component:
            * Aerosol:
              * Description: "UKCA-GLOMAP-mode"
              * Native nominal resolution: "250 km"
            * Atmos:
              * Description: "MetUM-HadGEM3-GA7.1 (N96; 192 x 144
                longitude/latitude; 85 levels; top level 85 km)"
              * Native nominal resolution: "250 km"
            * AtmosChem:
              * Description: "none"
              * Native nominal resolution: "none"
            * Land:
              * Description: "CABLE2.5"
              * Native nominal resolution: "250 km"
            * LandIce:
              * Description: "none"
              * Native nominal resolution: "none"
            * Ocean:
              * Description: "ACCESS-OM2 (GFDL-MOM5, tripolar primarily 1deg; 360 x
                300 longitude/latitude; 50 levels; top grid cell 0-10 m)"
              * Native nominal resolution: "100 km"
            * OcnBgchem:
              * Description: "none"
              * Native nominal resolution: "none"
            * SeaIce:
              * Description: "CICE5.1.2 (same grid as ocean)"
              * Native nominal resolution: "100 km"
        * License info:
            * Exceptions contact: "@csiro.au <- access_csiro"
            * History: "2019-11-08: initially published under CC BY-SA 4.0;
              2022-06-10: relaxed to CC BY 4.0"
            * Id: "CC BY 4.0"
            * License: "Creative Commons Attribution 4.0 International (CC BY 4.0;
              https://creativecommons.org/licenses/by/4.0/)"
            * Source specific info: ""
            * Url: "https://creativecommons.org/licenses/by/4.0/"
      
        -- [Source id: "ACCESS-ESM1-5"] --
        * Release year: 2019
        * Institution id: "CSIRO"
        * Label: "ACCESS-ESM1.5"
        * Label extended: "Australian Community Climate and Earth System Simulator
          Earth System Model Version 1.5"
        * Cohort: "Published"
        * Activity participation: "C4MIP", "CDRMIP", "CMIP", "DAMIP", "LUMIP",
          "OMIP", "PMIP", "RFMIP", and "ScenarioMIP"
        * Model component:
            * Aerosol:
              * Description: "CLASSIC (v1.0)"
              * Native nominal resolution: "250 km"
            * Atmos:
              * Description: "HadGAM2 (r1.1, N96; 192 x 145 longitude/latitude; 38
                levels; top level 39255 m)"
              * Native nominal resolution: "250 km"
            * AtmosChem:
              * Description: "none"
              * Native nominal resolution: "none"
            * Land:
              * Description: "CABLE2.4"
              * Native nominal resolution: "250 km"
            * LandIce:
              * Description: "none"
              * Native nominal resolution: "none"
            * Ocean:
              * Description: "ACCESS-OM2 (MOM5, tripolar primarily 1deg; 360 x 300
                longitude/latitude; 50 levels; top grid cell 0-10 m)"
              * Native nominal resolution: "100 km"
            * OcnBgchem:
              * Description: "WOMBAT (same grid as ocean)"
              * Native nominal resolution: "100 km"
            * SeaIce:
              * Description: "CICE4.1 (same grid as ocean)"
              * Native nominal resolution: "100 km"
        * License info:
            * Exceptions contact: "@csiro.au <- access_csiro"
            * History: "2019-11-12: initially published under CC BY-SA 4.0;
              2022-06-10: relaxed to CC BY 4.0"
            * Id: "CC BY 4.0"
            * License: "Creative Commons Attribution 4.0 International (CC BY 4.0;
              https://creativecommons.org/licenses/by/4.0/)"
            * Source specific info: ""
            * Url: "https://creativecommons.org/licenses/by/4.0/"
      
      # ... with 129 more items

---

    Code
      print(dict$get("source_type"))
    Message <cliMessage>
      == Cmip6CV Source Type ===================================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * SourceType Modified: yyyy-mm-dd HH:MM:SS UTC
      * SourceType Note: "Issue396 durack1 augment source_type with description
        (#399)"
      
      -- Values <list> ---------------------------------------------------------------
      * AER: "aerosol treatment in an atmospheric model where concentrations are
        calculated based on emissions, transformation, and removal processes (rather
        than being prescribed or omitted entirely)"
      * AGCM: "atmospheric general circulation model run with prescribed ocean
        surface conditions and usually a model of the land surface"
      * AOGCM: "coupled atmosphere-ocean global climate model, additionally
        including explicit representation of at least the land and sea ice"
      * BGC: "biogeochemistry model component that at the very least accounts for
        carbon reservoirs and fluxes in the atmosphere, terrestrial biosphere, and
        ocean"
      * CHEM: "chemistry treatment in an atmospheric model that calculates
        atmospheric oxidant concentrations (including at least ozone), rather than
        prescribing them"
      # ... with 5 more items

---

    Code
      print(dict$get("sub_experiment_id"))
    Message <cliMessage>
      == Cmip6CV Sub Experiment ID =============================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * SubExperimentId Modified: yyyy-mm-dd HH:MM:SS UTC
      * SubExperimentId Note: "Revise sub_experiment_id values"
      
      -- Values <list> ---------------------------------------------------------------
      * none: "none"
      * s1910: "initialized near end of year 1910"
      * s1920: "initialized near end of year 1920"
      * s1950: "initialized near end of year 1950"
      * s1960: "initialized near end of year 1960"
      # ... with 69 more items

---

    Code
      print(dict$get("table_id"))
    Message <cliMessage>
      == Cmip6CV Table ID ======================================== CMIP6 Dictionary ==
      * CV Version: `6.2.58.49`
      * CV Modified: yyyy-mm-dd HH:MM:SS UTC
      * TableId Modified: yyyy-mm-dd HH:MM:SS UTC
      * TableId Note: "Issue199 durack1 update table_id to Data Request v1.0
        (#200)"
      
      -- Values <character> ----------------------------------------------------------
      "3hr", "6hrLev", "6hrPlev", "6hrPlevPt", "AERday", "AERhr", "AERmon",
      "AERmonZ", ..., "day", and "fx"
      # ... with 33 more items

---

    Code
      print(dict$get("dreq"))
    Message <cliMessage>
      == CMIP6 Data Request ====================================== CMIP6 Dictionary ==
      
      -- <Header Metadata> -----------------------------------------------------------
      * DReq Version: `1.0.33`
      * CMOR Version: `3.5`
      * MIP Era: `CMIP6`
      * Missing Value:
          * Real: `1e+20`
          * Int: `-999`
      * Conventions: `CF-1.7 CMIP-6.2`
      * `2062` Variables from `43` Tables and `9` Realms
      
      -- Values <data.table> ---------------------------------------------------------
      
        -- [Variable: "clt"] --
        * Table id: "3hr"
        * Modeling realm: "atmos"
        * Standard name: "cloud_area_fraction"
        * Long name: "Total Cloud Cover Percentage"
        * Frequency: "3hr"
        * Units: "%"
        * Cell methods: "area: time: mean"
        * Cell measures: "area: areacella"
        * Comment: "Total cloud area fraction (reported as a percentage) for the
          whole atmospheric column, as seen from the surface or the top of the
          atmosphere. Includes both large-scale and convective cloud."
        * Dimensions: "longitude latitude time"
        * Out name: "clt"
        * Type: "real"
        * Positive: NA
        * Valid min: NA
        * Valid max: NA
        * Ok min mean abs: NA
        * Ok max mean abs: NA
      
        -- [Variable: "hfls"] --
        * Table id: "3hr"
        * Modeling realm: "atmos"
        * Standard name: "surface_upward_latent_heat_flux"
        * Long name: "Surface Upward Latent Heat Flux"
        * Frequency: "3hr"
        * Units: "W m-2"
        * Cell methods: "area: time: mean"
        * Cell measures: "area: areacella"
        * Comment: "The surface called 'surface' means the lower boundary of the
          atmosphere. 'Upward' indicates a vector component which is positive when
          directed upward (negative downward). The surface latent heat flux is the
          exchange of heat between the surface and the air on account of evaporation
          (including sublimation). In accordance with common usage in geophysical
          disciplines, 'flux' implies per unit area, called 'flux density' in
          physics."
        * Dimensions: "longitude latitude time"
        * Out name: "hfls"
        * Type: "real"
        * Positive: "up"
        * Valid min: NA
        * Valid max: NA
        * Ok min mean abs: NA
        * Ok max mean abs: NA
      
        -- [Variable: "hfss"] --
        * Table id: "3hr"
        * Modeling realm: "atmos"
        * Standard name: "surface_upward_sensible_heat_flux"
        * Long name: "Surface Upward Sensible Heat Flux"
        * Frequency: "3hr"
        * Units: "W m-2"
        * Cell methods: "area: time: mean"
        * Cell measures: "area: areacella"
        * Comment: "The surface sensible heat flux, also called turbulent heat
          flux, is the exchange of heat between the surface and the air by motion of
          air."
        * Dimensions: "longitude latitude time"
        * Out name: "hfss"
        * Type: "real"
        * Positive: "up"
        * Valid min: NA
        * Valid max: NA
        * Ok min mean abs: NA
        * Ok max mean abs: NA
      
      # ... with 2059 more items

# Cmip6Dict$print()

    Code
      dict$print()
    Message <cliMessage>
      == CMIP6 Dictionary ============================================================
      * Built at: <NONE>
      
      -- Controlled Vocabularies (CVs) -----------------------------------------------
      * CV Version: <Empty>
      
      -- Data Request (DReq) ---------------------------------------------------------
      * DReq Version: <Empty>

---

    Code
      dict$print()
    Message <cliMessage>
      == CMIP6 Dictionary ============================================================
      * Built at: [yyyy-mm-dd HH:MM:SS]
      
      -- Controlled Vocabularies (CVs) -----------------------------------------------
      * CV Version: `6.2.58.49`
      * CV Contents [13 types]:
        * drs [XX items]
        * activity_id [XX items]
        * experiment_id [XX items]
        * frequency [XX items]
        * grid_label [XX items]
        * institution_id [XX items]
        * nominal_resolution [XX items]
        * realm [XX items]
        * required_global_attributes [XX items]
        * source_id [XX items]
        * source_type [XX items]
        * sub_experiment_id [XX items]
        * table_id [XX items]
      
      -- Data Request (DReq) ---------------------------------------------------------
      * DReq Version: `1.0.33`
      * DReq Contents: XX Variables from XX Tables and XX Realms

