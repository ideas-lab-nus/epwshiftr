# ESGF Query Result Dataset works

    Code
      datasets$print()
    Message
      == ESGF Query Result [Dataset] =================================================
      * Index Node: https://esgf-data.dkrz.de
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 2
      * Total size: XX [GiB]
      * Fields: 14 | [ access, activity_id, experiment_id, frequency, id, index_node,
        number_of_aggregations, number_of_files, project, size, source_id, url,
        variable_id, and variant_label ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * project = CMIP6
      * activity_id = ScenarioMIP
      * experiment_id = ssp585
      * source_id = AWI-CM-1-1-MR
      * variable_id = tas
      * frequency = day
      * variant_label = r1i1p1f1
      * fields = source_id, experiment_id, frequency
      * latest = true
      * type = Dataset
      * offset = 0
      * distrib = true
      * limit = 2
      * format = application/solr+json
    Output
      
    Message
      -- <Dataset> -------------------------------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
          [ XX Files, XX GiB | X Aggregations ]
          [ Access: <...> ]
      [2] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
          [ XX Files, XX GiB | X Aggregations ]
          [ Access: <...> ]

# ESGF Query Result File works

    Code
      files$print()
    Message
      == ESGF Query Result [File] ====================================================
      * Index Node: https://esgf-data.dkrz.de
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 1
      * Total size: XX [GiB]
      * Fields: 56 | [ _timestamp, _version_, activity_drs, activity_id,
        cf_standard_name, checksum, checksum_type, citation_url, data_node,
        data_specs_version, dataset_id, dataset_id_template_,
        directory_format_template_, experiment_id, experiment_title, filename,
        frequency, further_info_url, ..., variant_label, and version ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * dataset_id =
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
      * latest = true
      * distrib = true
      * limit = 1
      * type = File
      * format = application/solr+json
    Output
      
    Message
      -- <File> (From 1 Data Nodes) --------------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529.tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20200101-20211231.nc|esgf.data.node
          [ XX MiB | Access: <...> ]

# ESGF Query Result Aggregation works

    Code
      aggs$print()
    Message
      == ESGF Query Result [Aggregation] =============================================
      * Index Node: https://esgf-data.dkrz.de
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 2
      * Total size: <Unknown> [Byte]
      * Fields: 8 | [ data_node, dataset_id, id, size, title, url, url_download, and
        url_opendap ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * dataset_id =
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
      * fields = id
      * latest = true
      * distrib = true
      * limit = 2
      * type = Aggregation
      * format = application/solr+json
    Output
      
    Message
      -- <Aggregation> (From 1 Data Nodes) -------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.tas.20200101.aggregration|esgf.data.node
          [ <Unknown> Byte | Access: <...> ]
      [2] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.tas.20200101.aggregration|esgf.data.node
          [ <Unknown> Byte | Access: <...> ]

# result_esgf() works

    Code
      result_esgf("file")$print()
    Message
      == ESGF Query Result [File] ====================================================
      * Index Node:
      * Collected at: NULL
      * Result count: 0
      * Total size: 0 [Byte]
      * Fields: 0
      
      -- <Query Parameter> -----------------------------------------------------------
    Output
      
    Message
      -- <File> ----------------------------------------------------------------------
        <Empty>
        NOTE: No matched data found. Please update query parameters and try again.

---

    Code
      result_esgf("aggregation")$print()
    Message
      == ESGF Query Result [Aggregation] =============================================
      * Index Node:
      * Collected at: NULL
      * Result count: 0
      * Total size: <Unknown> [Byte]
      * Fields: 0
      
      -- <Query Parameter> -----------------------------------------------------------
    Output
      
    Message
      -- <Aggregation> ---------------------------------------------------------------
        <Empty>
        NOTE: No matched data found. Please update query parameters and try again.

---

    Code
      result_esgf("aggregation")$print()
    Message
      == ESGF Query Result [Aggregation] =============================================
      * Index Node:
      * Collected at: NULL
      * Result count: 0
      * Total size: <Unknown> [Byte]
      * Fields: 0
      
      -- <Query Parameter> -----------------------------------------------------------
    Output
      
    Message
      -- <Aggregation> ---------------------------------------------------------------
        <Empty>
        NOTE: No matched data found. Please update query parameters and try again.

