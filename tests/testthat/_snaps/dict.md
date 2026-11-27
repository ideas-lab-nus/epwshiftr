# EsgDict$print()

    Code
      dict$print()
    Message
      == ESG Dictionary ==============================================================
      * Project: "CMIP6"
      * Profile: "cmip6"
      * Status: "built"
      * Built at: 2025-01-04 UTC
      
      -- Vocabulary ------------------------------------------------------------------
      * Vocab Version: `6.2.0`
      * Vocab Contents [13 types]:
        * drs [6 items]
        * activity_id [2 items]
        * experiment_id [2 items]
        * frequency [3 items]
        * grid_label [2 items]
        * institution_id [2 items]
        * nominal_resolution [2 items]
        * realm [2 items]
        * required_global_attributes [3 items]
        * source_id [2 items]
        * source_type [1 items]
        * sub_experiment_id [2 items]
        * table_id [3 items]
      
      -- Request ---------------------------------------------------------------------
      * Request Version: `1.0.0`
      * Request Contents: 3 Variables from 3 Tables and 2 Realms

# print.Cmip6CV() / print.Cmip6DReq()

    Code
      print.Cmip6CV(dict$get("activity_id"))
    Message
      == Cmip6CV Activity ID ===================================== CMIP6 Dictionary ==
      * CV Version: `6.2.0`
      * CV Modified: 2025-01-01 00:00:00 UTC
      * ActivityId Modified: 2025-01-01 00:00:00 UTC
      * ActivityId Note: "activity_id"
      
      -- Values <list> ---------------------------------------------------------------
      * CMIP: "CMIP activity"
      * ScenarioMIP: "ScenarioMIP activity"

---

    Code
      print.Cmip6DReq(dict$get("request"), n = 1L)
    Message
      == CMIP6 Data Request ====================================== CMIP6 Dictionary ==
      
      -- <Header Metadata> -----------------------------------------------------------
      * DReq Version: `1.0.0`
      * CMOR Version: `3.7.0`
      * MIP Era: `CMIP6`
      * Missing Value:
          * Real: `1e+20`
          * Int: `-999`
      * Conventions: `CF-1.7 CMIP-6.2`
      * `3` Variables from `3` Tables and `2` Realms
      
      -- Values <data.table> ---------------------------------------------------------
      
        -- [Variable: "tas"] --
      * Table id: "Amon"
      * Modeling realm: "atmos"
      * Standard name: "air_temperature"
      * Long name: "Near-Surface Air Temperature"
      * Frequency: "mon"
      * Units: "K"
      * Cell methods: "time: mean"
      * Cell measures: "area: areacella"
      * Comment: NA
      * Dimensions: "longitude latitude time"
      * Out name: "tas"
      * Type: "real"
      * Positive: NA
      * Valid min: NA
      * Valid max: NA
      * Ok min mean abs: NA
      * Ok max mean abs: NA
      
      # ... with 2 more items

