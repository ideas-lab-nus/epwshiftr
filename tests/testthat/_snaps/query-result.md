# ESGF Query Result Dataset works

    Code
      datasets$print()
    Message <cliMessage>
      == ESGF Query Result [Dataset] =================================================
      * Host: https://esgf-node.llnl.gov/esg-search
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
      
    Message <cliMessage>
      -- <Dataset> -------------------------------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529|esgf-data1.llnl.gov
          [ XX Files, XX GiB | X Aggregations ]
          [ Access: <HTTPServer, GridFTP, OPENDAP, Globus, LAS> ]
      [2] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529|esgf-data04.diasjp.net
          [ XX Files, XX GiB | X Aggregations ]
          [ Access: <HTTPServer, OPENDAP> ]

# ESGF Query Result File works

    Code
      files$print()
    Message <cliMessage>
      == ESGF Query Result [File] ====================================================
      * Host: https://esgf-node.llnl.gov/esg-search
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 1
      * Total size: XX [MiB]
      * Fields: 55 | [ _timestamp, _version_, activity_drs, activity_id,
        cf_standard_name, checksum, checksum_type, citation_url, data_node,
        data_specs_version, dataset_id, dataset_id_template_,
        directory_format_template_, experiment_id, experiment_title, filename,
        frequency, further_info_url, ..., variant_label, and version ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * dataset_id =
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529|esgf-data1.llnl.gov,
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529|esgf-data04.diasjp.net
      * latest = true
      * distrib = true
      * limit = 1
      * type = File
      * format = application/solr+json
    Output
      
    Message <cliMessage>
      -- <File> (From 1 Data Nodes) --------------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529.tas_day_AWI-CM-1-1-MR_ssp585_r1i1p1f1_gn_20150101-20151231.nc|esgf-data04.diasjp.net
          [ XX MiB | Access: <HTTPServer, OPENDAP> ]

# ESGF Query Result Aggregation works

    Code
      aggs$print()
    Message <cliMessage>
      == ESGF Query Result [Aggregation] =============================================
      * Host: https://esgf-node.llnl.gov/esg-search
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 2
      * Total size: <Unknown> [Byte]
      * Fields: 8 | [ data_node, dataset_id, id, size, title, url, url_download, and
        url_opendap ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * dataset_id =
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529|esgf-data1.llnl.gov,
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.v20190529|esgf-data04.diasjp.net
      * fields = id
      * latest = true
      * distrib = true
      * limit = 2
      * type = Aggregation
      * format = application/solr+json
    Output
      
    Message <cliMessage>
      -- <Aggregation> (From 2 Data Nodes) -------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.tas.20190529.aggregation|esgf-data04.diasjp.net
          [ <Unknown> Byte | Access: <OPENDAP> ]
      [2] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.ssp585.r1i1p1f1.day.tas.gn.tas.20190529.aggregation|esgf-data1.llnl.gov
          [ <Unknown> Byte | Access: <LAS> ]

