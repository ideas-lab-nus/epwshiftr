# EsgResultDataset$print() snapshots offline fixtures

    Code
      datasets$print()
    Message
      == ESGF Query Result [Dataset] =================================================
      * Index Node: https://example.org
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 2
      * Dataset files: 3
      * Dataset aggregations: 2
      * Total size: XX [GiB]
      * Fields: XX | [ id, version, activity_id, data_node, experiment_id, frequency,
        index_node, instance_id, latest, master_id, number_of_files, project,
        replica, size, source_id, variable_id, variant_label, access, and
        number_of_aggregations ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * project = CMIP6
      * activity_id = ScenarioMIP
      * source_id = AWI-CM-1-1-MR
      * variable_id = tas
      * frequency = day
      * variant_label = r1i1p1f1
      * fields = source_id, experiment_id, frequency
      * type = Dataset
      * offset = 0
      * distrib = true
      * limit = 2
      * format = application/solr+json
    Output
      
    Message
      -- <Dataset> (From 1 Data Nodes) -----------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
          [ 2 Files, 1 GiB | 2 Aggregations ]
          [ Access: <...> ]
      [2] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
          [ 1 Files, 2 GiB | 0 Aggregations ]
          [ Access: <...> ]

# EsgResultFile$print() snapshots offline fixtures

    Code
      files$print()
    Message
      == ESGF Query Result [File] ====================================================
      * Index Node: https://example.org
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 1
      * Total size: XX [GiB]
      * Fields: XX | [ id, version, activity_id, institution_id, dataset_id, size,
        checksum, checksum_type, instance_id, master_id, replica, tracking_id, title,
        data_node, url, filename, url_opendap, and url_download ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * project = CMIP6
      * fields = source_id, experiment_id, frequency
      * type = File
      * offset = 0
      * distrib = true
      * limit = 1
      * format = application/solr+json
      * dataset_id =
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
    Output
      
    Message
      -- <File> (From 1 Data Nodes) --------------------------------------------------
    Output
      [1] tas_day_AWI-CM-1-1-MR_sspXXX_r1i1p1f1_gn_20200101-20211231.nc|esgf.data.node
          [ 1 MiB | Access: <OPENDAP, HTTPServer> ]

# EsgResultAggregation$print() snapshots offline fixtures

    Code
      aggs$print()
    Message
      == ESGF Query Result [Aggregation] =============================================
      * Index Node: https://example.org
      * Collected at: yyyy-mm-dd HH:MM:SS
      * Result count: 2
      * Total size: <Unknown> [Byte]
      * Fields: XX | [ id, version, activity_id, institution_id, data_node,
        dataset_id, instance_id, master_id, replica, size, title, url, url_opendap,
        and url_download ]
      
      -- <Query Parameter> -----------------------------------------------------------
      * project = CMIP6
      * fields = id
      * type = Aggregation
      * offset = 0
      * distrib = true
      * limit = 2
      * format = application/solr+json
      * dataset_id =
        CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.v20200202|esgf.data.node
    Output
      
    Message
      -- <Aggregation> (From 1 Data Nodes) -------------------------------------------
    Output
      [1] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.tas.20200101.aggregration|esgf.data.node
          [ <Unknown> Byte | Access: <...> ]
      [2] CMIP6.ScenarioMIP.AWI.AWI-CM-1-1-MR.sspXXX.r1i1p1f1.day.tas.gn.tas.20200101.aggregration|esgf.data.node
          [ <Unknown> Byte | Access: <...> ]

# esg_result() constructs typed empty query results

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
      * project = CMIP6
      * fields = *
      * type = Dataset
      * offset = 0
      * distrib = true
      * limit = 10
      * format = application/solr+json
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
      * project = CMIP6
      * fields = *
      * type = Dataset
      * offset = 0
      * distrib = true
      * limit = 10
      * format = application/solr+json
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
      * project = CMIP6
      * fields = *
      * type = Dataset
      * offset = 0
      * distrib = true
      * limit = 10
      * format = application/solr+json
    Output
      
    Message
      -- <Aggregation> ---------------------------------------------------------------
        <Empty>
        NOTE: No matched data found. Please update query parameters and try again.
