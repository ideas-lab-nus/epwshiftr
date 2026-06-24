# ESGF Query Result Dataset works

    Code
      datasets$print()
    Message
      == ESGF Query Result [Dataset] =================================================
      * Index Node: https://esgf-data.dkrz.de
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 2
      * Total size: XX [GiB]
      * Fields: XX | [ id, access, activity_id, experiment_id, frequency, index_node,
        number_of_files, project, size, source_id, variable_id, and variant_label ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * project=CMIP6
      * activity_id=ScenarioMIP
      * source_id=AWI-CM-1-1-MR
      * variable_id=tas
      * frequency=day
      * variant_label=r1i1p1f1
      * fields=source_id,experiment_id,frequency,access,id,index_node,number_of_aggregations,number_of_files,size,url,project,activity_id,variable_id,variant_label
      * latest=true
      * type=Dataset
      * offset=0
      * distrib=true
      * limit=2
      * format=application%2Fsolr%2Bjson
      * project=CMIP6
      * activity_id=ScenarioMIP
      * source_id=AWI-CM-1-1-MR
      * variable_id=tas
      * frequency=day
      * variant_label=r1i1p1f1
      * fields=source_id,experiment_id,frequency,access,id,index_node,number_of_aggregations,number_of_files,size,url,project,activity_id,variable_id,variant_label
      * latest=true
      * type=Dataset
      * offset=0
      * distrib=true
      * limit=2
      * format=application%2Fsolr%2Bjson
    Output
      
    Message
      -- <Dataset> -------------------------------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
          [ 86 Files, 6.6 GiB | No Aggregations ]
          [ Access: <...> ]
      [2] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
          [ 86 Files, 6.59 GiB | No Aggregations ]
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
      * Fields: XX | [ id, version, activity_drs, activity_id, cf_standard_name,
        checksum, checksum_type, citation_url, data_node, data_specs_version,
        dataset_id, dataset_id_template_, directory_format_template_, experiment_id,
        experiment_title, frequency, further_info_url, grid, ..., url_opendap, and
        url_download ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * project=CMIP6
      * activity_id=ScenarioMIP
      * experiment_id=sspXXX
      * source_id=AWI-CM-1-1-MR
      * variable_id=tas
      * frequency=day
      * variant_label=r1i1p1f1
      * latest=true
      * type=File
      * offset=0
      * distrib=true
      * limit=1
      * format=application%2Fsolr%2Bjson
      * dataset_id=CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
    Output
      
    Message
      -- <File> (From 1 Data Nodes) --------------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20190529.tas_day_AWI-CM-1-1-MR_sspXXX_r1i1p1f1_gn_20200101-20211231.nc|esgf.data.node
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
      * Fields: XX | [ id, data_node, dataset_id, size, title, url, url_opendap, and
        url_download ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * fields=id,data_node,dataset_id,size,title,url
      * latest=true
      * type=Aggregation
      * offset=0
      * distrib=true
      * limit=2
      * format=application%2Fsolr%2Bjson
      * dataset_id=CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
    Output
      
    Message
      -- <Aggregation> (From 1 Data Nodes) -------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.tas.20200101.aggregration|esgf.data.node
          [ <Unknown> Byte | Access: <...> ]
      [2] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.tas.20200101.aggregration|esgf.data.node
          [ <Unknown> Byte | Access: <...> ]

# esg_result() works

    Code
      esg_result("file")$print()
    Message
      == ESGF Query Result [File] ====================================================
      * Index Node:
      * Collected at: NULL
      * Result count: 0
      * Total size: 0 [Byte]
      * Fields: 0
      
      -- <Query Parameter> -----------------------------------------------------------
      * project=CMIP6
      * fields=*
      * latest=true
      * type=Dataset
      * offset=0
      * distrib=true
      * limit=10
      * format=application%2Fsolr%2Bjson
      * project=CMIP6
      * fields=*
      * latest=true
      * type=Dataset
      * offset=0
      * distrib=true
      * limit=10
      * format=application%2Fsolr%2Bjson
    Output
      
    Message
      -- <File> ----------------------------------------------------------------------
        <Empty>
        NOTE: No matched data found. Please update query parameters and try again.

---

    Code
      esg_result("aggregation")$print()
    Message
      == ESGF Query Result [Aggregation] =============================================
      * Index Node:
      * Collected at: NULL
      * Result count: 0
      * Total size: <Unknown> [Byte]
      * Fields: 0
      
      -- <Query Parameter> -----------------------------------------------------------
      * project=CMIP6
      * fields=*
      * latest=true
      * type=Dataset
      * offset=0
      * distrib=true
      * limit=10
      * format=application%2Fsolr%2Bjson
      * project=CMIP6
      * fields=*
      * latest=true
      * type=Dataset
      * offset=0
      * distrib=true
      * limit=10
      * format=application%2Fsolr%2Bjson
    Output
      
    Message
      -- <Aggregation> ---------------------------------------------------------------
        <Empty>
        NOTE: No matched data found. Please update query parameters and try again.

---

    Code
      esg_result("aggregation")$print()
    Message
      == ESGF Query Result [Aggregation] =============================================
      * Index Node:
      * Collected at: NULL
      * Result count: 0
      * Total size: <Unknown> [Byte]
      * Fields: 0
      
      -- <Query Parameter> -----------------------------------------------------------
      * project=CMIP6
      * fields=*
      * latest=true
      * type=Dataset
      * offset=0
      * distrib=true
      * limit=10
      * format=application%2Fsolr%2Bjson
      * project=CMIP6
      * fields=*
      * latest=true
      * type=Dataset
      * offset=0
      * distrib=true
      * limit=10
      * format=application%2Fsolr%2Bjson
    Output
      
    Message
      -- <Aggregation> ---------------------------------------------------------------
        <Empty>
        NOTE: No matched data found. Please update query parameters and try again.

