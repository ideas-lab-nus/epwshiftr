# Shift configuration printers use compact semantic receipts

    Code
      shift_test_print_objects(list(climate, control, ui, reference, method, site),
      width = 72L)
    Message
      == CMIP6 Climate =======================================================
      * Model: BCC-CSM2-MR
      * Scenarios: ssp126, ssp585
      * Member: auto
      * Grid: auto
      * Frequency: mon
      * Table: Amon
      * Activity: ScenarioMIP
      * Index nodes: 6-node failover
      * Data node: auto
      == Shift Control =======================================================
      * Strict: TRUE
      * Allow partial: FALSE
      * Download: auto
      * Resume: TRUE
      * Overwrite: FALSE
      * Extraction: nearest
      * Output layout: nested
      == Shift UI ============================================================
      * Progress: auto
      * Detail: normal
      * Motion: auto
      * Refresh: 0.12 s
      * Heartbeat: 10 s
      == Climate Reference ===================================================
      * Mode: historical
      * Periods: reference 1995–2014
      * Experiment: historical
      * Activity: CMIP
      * Match: source_id, variant_label, frequency, table_id, grid_label
      == Morph Method ========================================================
      * Name: belcher
      * Backend: belcher
      * Reference: baseline EPW
      * Requires reference: FALSE
      * Accepts reference: TRUE
      * Variables: tas, hurs, psl, rlds, rsds, sfcWind, clt, ... (8 total)
      == EPW Site ============================================================
      * ID: SIN
      * Label: Singapore
      * Coordinates: 103.980000, 1.370000

---

    Code
      shift_test_print_objects(list(climate, control, ui, reference, method, site),
      width = 100L, n = 3L, verbose = TRUE)
    Message
      == CMIP6 Climate ===================================================================================
      * Model: BCC-CSM2-MR
      * Scenarios: ssp126, ssp585
      * Member: auto
      * Grid: auto
      * Frequency: mon
      * Table: Amon
      * Activity: ScenarioMIP
      * Index nodes: 6-node failover
      * Data node: auto
      -- Discovery ---------------------------------------------------------------------------------------
      +----------+--------------------------+
      | Priority | Index Node               |
      +----------+--------------------------+
      |        1 | https://esgf-data.dkr... |
      |        2 | https://esgf.ceda.ac.uk  |
      |        3 | https://esgf-node.orn... |
      +----------+--------------------------+
      i 3 more rows; increase `n` to show every index node.
      == Shift Control ===================================================================================
      * Strict: TRUE
      * Allow partial: FALSE
      * Download: auto
      * Resume: TRUE
      * Overwrite: FALSE
      * Extraction: nearest
      * Output layout: nested
      == Shift UI ========================================================================================
      * Progress: auto
      * Detail: normal
      * Motion: auto
      * Refresh: 0.12 s
      * Heartbeat: 10 s
      == Climate Reference ===============================================================================
      * Mode: historical
      * Periods: reference 1995–2014
      * Experiment: historical
      * Activity: CMIP
      * Match: source_id, variant_label, frequency, table_id, grid_label
      -- Workflow options --------------------------------------------------------------------------------
      * Extract: fallback=auto
      == Morph Method ====================================================================================
      * Name: belcher
      * Backend: belcher
      * Reference: baseline EPW
      * Requires reference: FALSE
      * Accepts reference: TRUE
      * Variables: tas, hurs, psl, rlds, rsds, sfcWind, clt, ... (8 total)
      -- Method overrides --------------------------------------------------------------------------------
      +-------+--------+
      | Field | Method |
      +-------+--------+
      | tdb   | shift  |
      | rh    | shift  |
      | p     | shift  |
      +-------+--------+
      i 3 more rows; increase `n` to show every method override.
      -- Rules -------------------------------------------------------------------------------------------
      +------+----------------------+-------------+--------+----------+
      | Step | Epw Field            | Variable Id | Method | Required |
      +------+----------------------+-------------+--------+----------+
      | tdb  | dry_bulb_temperature | tas         | shift  | yes      |
      | rh   | relative_humidity    | hurs        | shift  | yes      |
      | p    | atmospheric_pressure | psl         | shift  | yes      |
      +------+----------------------+-------------+--------+----------+
      i 10 more rows; increase `n` to show every backend rule.
      == EPW Site ========================================================================================
      * ID: SIN
      * Label: Singapore
      * Coordinates: 103.980000, 1.370000
      -- Workflow ----------------------------------------------------------------------------------------
      * Status: new

# Shift plan and stage printers use bounded semantic previews

    Code
      shift_test_print_objects(list(plan, download, climate, morphed, outputs),
      width = 80L, n = 3L)
    Message
      == Future EPW Plan =============================================================
      * Status: planned
      * Climate: BCC-CSM2-MR · ssp126, ssp585
      * Periods: 2060s 2055–2065
      * Method: belcher
      * Reference: historical · reference 1995–2014
      * Selection: member auto · grid auto
      * Expected outputs: 2
      * Output directory: <tempdir>/shift-print-output
      -- Expected outputs ------------------------------------------------------------
      +-------------+---------------+---------------+------------+--------+
      | Source Id   | Experiment Id | Variant Label | Grid Label | Period |
      +-------------+---------------+---------------+------------+--------+
      | BCC-CSM2-MR | ssp126        | -             | -          | 2060s  |
      | BCC-CSM2-MR | ssp585        | -             | -          | 2060s  |
      +-------------+---------------+---------------+------------+--------+
      i Hidden columns for console width: years, status, missing_reason. Use the
        corresponding shift_*() inspector for all columns.
      == CMIP6 Download ==============================================================
      * Status: partial
      * Session: session-print
      * Tasks: 2/4 complete · done 2 · error 1 · queued 1
      * Transfer: 320 Byte / 1000 Byte
      -- Tasks -----------------------------------------------------------------------
      +--------+-----------+------------+-------+-----------------+-----------+
      | Status | Filename  | Bytes Done | Size  | Progress        | Speed Bps |
      +--------+-----------+------------+-------+-----------------+-----------+
      | done   | tas_01.nc |      100 B | 100 B | [########] 100% |        10 |
      | done   | tas_02.nc |      200 B | 200 B | [########] 100% |        20 |
      | queued | tas_03.nc |        0 B | 300 B | [--------]   0% |         0 |
      +--------+-----------+------------+-------+-----------------+-----------+
      i 1 more rows; use `shift_data()` or the Downloader inspectors for all tasks.
      i Hidden columns for console width: eta_seconds, data_node, attempts,
        last_error. Use the corresponding shift_*() inspector for all columns.
      == Extracted Climate ===========================================================
      * Status: partial
      * Site: Singapore
      * Periods: 2060s 2055–2065
      * Coverage: 2/3 complete
      * Variables: tas, hurs
      * Rows: 264
      -- Coverage --------------------------------------------------------------------
      +----------+--------+---------------+-------------+---------------+------------+
      | Complete | Status | Experiment Id | Variable Id | Variant Label | Grid Label |
      +----------+--------+---------------+-------------+---------------+------------+
      | yes      | done   | ssp126        | tas         | r1i1p1f1      | gn         |
      | yes      | done   | ssp585        | tas         | r1i1p1f1      | gn         |
      | no       | failed | ssp585        | hurs        | r1i1p1f1      | gn         |
      +----------+--------+---------------+-------------+---------------+------------+
      i Hidden columns for console width: time_start, time_stop, output_time_count,
        output_rows, last_error. Use the corresponding shift_*() inspector for all
        columns.
      == Morphed EPW =================================================================
      * Status: partial
      * Method: belcher
      * Reference: historical · reference 1995–2014
      * Cases: 4
      * Results: 4
      -- Morph results ---------------------------------------------------------------
      +---------+-------------+---------------+---------------+--------+-------------+
      | Case Id | Source Id   | Experiment Id | Variant Label | Period | Status      |
      +---------+-------------+---------------+---------------+--------+-------------+
      | case-1  | BCC-CSM2-MR | ssp126        | r1i1p1f1      | 2060s  | result_done |
      | case-2  | BCC-CSM2-MR | ssp126        | r1i1p1f1      | 2060s  | result_done |
      | case-3  | BCC-CSM2-MR | ssp585        | r1i1p1f1      | 2060s  | result_done |
      +---------+-------------+---------------+---------------+--------+-------------+
      i 1 more rows; use `shift_data()` or `shift_artifacts()` for complete morph
        data.
      i Hidden columns for console width: row_count, output_path. Use the
        corresponding shift_*() inspector for all columns.
      == EPW Outputs =================================================================
      * Status: partial
      * Outputs: 12 registered · 12 paths
      * Export directory: /exports
      -- Outputs ---------------------------------------------------------------------
      +-------------+---------------+---------------+--------+-----------------------+
      | Source Id   | Experiment Id | Variant Label | Period | Path                  |
      +-------------+---------------+---------------+--------+-----------------------+
      | BCC-CSM2-MR | ssp126        | r1i1p1f1      | 2060s  | outputs/future-01.epw |
      | BCC-CSM2-MR | ssp585        | r1i1p1f1      | 2060s  | outputs/future-02.epw |
      | BCC-CSM2-MR | ssp126        | r1i1p1f1      | 2060s  | outputs/future-03.epw |
      +-------------+---------------+---------------+--------+-----------------------+
      i 9 more rows; use `shift_outputs()` for all output records.
      i Hidden columns for console width: export_path, created_at. Use the
        corresponding shift_*() inspector for all columns.

---

    Code
      shift_test_print_objects(list(plan, download, climate, morphed, outputs),
      width = 100L, n = 3L, verbose = TRUE)
    Message
      == Future EPW Plan =================================================================================
      * Status: planned
      * Climate: BCC-CSM2-MR · ssp126, ssp585
      * Periods: 2060s 2055–2065
      * Method: belcher
      * Reference: historical · reference 1995–2014
      * Selection: member auto · grid auto
      * Expected outputs: 2
      * Output directory: <tempdir>/shift-print-output
      -- Discovery ---------------------------------------------------------------------------------------
      * Frequency: mon
      * Table: Amon
      * Index nodes: https://esgf-data.dkrz.de, https://esgf.ceda.ac.uk,
        https://esgf-node.ornl.gov/esgf-1-5-bridge, https://esgf.nci.org.au,
        https://esgf-node.ipsl.upmc.fr, https://esg-dn1.nsc.liu.se
      * Download: auto
      * Partial outputs: FALSE
      * Output layout: nested
      -- Expected outputs --------------------------------------------------------------------------------
      +-------------+---------------+---------------+------------+--------+-----------+---------+
      | Source Id   | Experiment Id | Variant Label | Grid Label | Period | Years     | Status  |
      +-------------+---------------+---------------+------------+--------+-----------+---------+
      | BCC-CSM2-MR | ssp126        | -             | -          | 2060s  | 2055–2065 | planned |
      | BCC-CSM2-MR | ssp585        | -             | -          | 2060s  | 2055–2065 | planned |
      +-------------+---------------+---------------+------------+--------+-----------+---------+
      i Hidden columns for console width: missing_reason. Use the corresponding shift_*() inspector for
        all columns.
      -- Workflow ----------------------------------------------------------------------------------------
      * Status: planned
      * Store: <tempdir>/shift-print-store
      == CMIP6 Download ==================================================================================
      * Status: partial
      * Session: session-print
      * Tasks: 2/4 complete · done 2 · error 1 · queued 1
      * Transfer: 320 Byte / 1000 Byte
      -- Tasks -------------------------------------------------------------------------------------------
      +--------+-----------+------------+-------+-----------------+-----------+-------------+
      | Status | Filename  | Bytes Done | Size  | Progress        | Speed Bps | Eta Seconds |
      +--------+-----------+------------+-------+-----------------+-----------+-------------+
      | done   | tas_01.nc |      100 B | 100 B | [########] 100% |        10 |           0 |
      | done   | tas_02.nc |      200 B | 200 B | [########] 100% |        20 |           0 |
      | queued | tas_03.nc |        0 B | 300 B | [--------]   0% |         0 |          30 |
      +--------+-----------+------------+-------+-----------------+-----------+-------------+
      i 1 more rows; use `shift_data()` or the Downloader inspectors for all tasks.
      i Hidden columns for console width: data_node, attempts, last_error. Use the corresponding
        shift_*() inspector for all columns.
      -- Workflow ----------------------------------------------------------------------------------------
      * Status: partial
      == Extracted Climate ===============================================================================
      * Status: partial
      * Site: Singapore
      * Periods: 2060s 2055–2065
      * Coverage: 2/3 complete
      * Variables: tas, hurs
      * Rows: 264
      -- Coverage ----------------------------------------------------------------------------------------
      +----------+--------+---------------+-------------+---------------+------------+
      | Complete | Status | Experiment Id | Variable Id | Variant Label | Grid Label |
      +----------+--------+---------------+-------------+---------------+------------+
      | yes      | done   | ssp126        | tas         | r1i1p1f1      | gn         |
      | yes      | done   | ssp585        | tas         | r1i1p1f1      | gn         |
      | no       | failed | ssp585        | hurs        | r1i1p1f1      | gn         |
      +----------+--------+---------------+-------------+---------------+------------+
      i Hidden columns for console width: time_start, time_stop, output_time_count, output_rows,
        last_error. Use the corresponding shift_*() inspector for all columns.
      -- Workflow ----------------------------------------------------------------------------------------
      * Status: partial
      == Morphed EPW =====================================================================================
      * Status: partial
      * Method: belcher
      * Reference: historical · reference 1995–2014
      * Cases: 4
      * Results: 4
      -- Morph results -----------------------------------------------------------------------------------
      +---------+-------------+---------------+---------------+--------+-------------+-----------+
      | Case Id | Source Id   | Experiment Id | Variant Label | Period | Status      | Row Count |
      +---------+-------------+---------------+---------------+--------+-------------+-----------+
      | case-1  | BCC-CSM2-MR | ssp126        | r1i1p1f1      | 2060s  | result_done |      8760 |
      | case-2  | BCC-CSM2-MR | ssp126        | r1i1p1f1      | 2060s  | result_done |      8760 |
      | case-3  | BCC-CSM2-MR | ssp585        | r1i1p1f1      | 2060s  | result_done |      8760 |
      +---------+-------------+---------------+---------------+--------+-------------+-----------+
      i 1 more rows; use `shift_data()` or `shift_artifacts()` for complete morph data.
      i Hidden columns for console width: output_path. Use the corresponding shift_*() inspector for all
        columns.
      -- Workflow ----------------------------------------------------------------------------------------
      * Status: partial
      == EPW Outputs =====================================================================================
      * Status: partial
      * Outputs: 12 registered · 12 paths
      * Export directory: /exports
      -- Outputs -----------------------------------------------------------------------------------------
      +-------------+---------------+---------------+--------+-----------------------+
      | Source Id   | Experiment Id | Variant Label | Period | Path                  |
      +-------------+---------------+---------------+--------+-----------------------+
      | BCC-CSM2-MR | ssp126        | r1i1p1f1      | 2060s  | outputs/future-01.epw |
      | BCC-CSM2-MR | ssp585        | r1i1p1f1      | 2060s  | outputs/future-02.epw |
      | BCC-CSM2-MR | ssp126        | r1i1p1f1      | 2060s  | outputs/future-03.epw |
      +-------------+---------------+---------------+--------+-----------------------+
      i 9 more rows; use `shift_outputs()` for all output records.
      i Hidden columns for console width: export_path, created_at. Use the corresponding shift_*()
        inspector for all columns.
      -- Workflow ----------------------------------------------------------------------------------------
      * Status: partial

# ShiftRun print falls back to a cached static snapshot

    Code
      print(run, width = 72L)
    Message
      ╭─ Future EPW  COMPLETED  5s  run 12345678 ───────────────────────────╮
      │ Plan     BCC-CSM2-MR · ssp126 + ssp585 · 2060s (2055–2065)          │
      │          belcher / historical 1995–2014 · 2 EPWs                    │
      ├─ Workflow ──────────────────────────────────────────────────────────┤
      │ Flow     [6/6] ✔ EPW · final stage                                  │
      │ Now      ✔ Workflow completed                                       │
      │ EPWs     ━━━━━━━━ 2/2 · 100% · exported 2/2                         │
      ├─ Results ───────────────────────────────────────────────────────────┤
      │ Summary  2/2 EPWs exported · 0 missing                              │
      │ Output   /exports                                                   │
      │ Files    ssp126.epw                                                 │
      │          ssp585.epw                                                 │
      ╰─────────────────────────────────────────────────────────────────────╯
      Cases
        Scenario  Period  Member  Status                                    
        ssp126    2060s   —       completed                                 
        ssp585    2060s   —       completed                                 
      -- Diagnostics ---------------------------------------------------------
      ! Persisted preview unavailable: No store is associated with this cached run.

---

    Code
      print(run, width = 100L, verbose = TRUE)
    Message
      ╭─ Future EPW  COMPLETED  5s  run 12345678 ───────────────────────────────────────────────────────╮
      │ Plan     BCC-CSM2-MR · ssp126 + ssp585 · 2060s (2055–2065) · belcher / historical 1995–2014     │
      │          2 EPWs                                                                                 │
      ├─ Workflow ──────────────────────────────────────────────────────────────────────────────────────┤
      │ Flow     ✔ Resolve  ›  ✔ Future  ›  ✔ Reference  ›  ✔ Coverage  ›  ✔ Morph  ›  ✔ EPW            │
      │ Now      ✔ Workflow completed                                                                   │
      │ EPWs     ━━━━━━━━━━━━━━ 2/2 · 100% · exported 2/2                                               │
      ├─ Results ───────────────────────────────────────────────────────────────────────────────────────┤
      │ Summary  2/2 EPWs exported · 0 missing                                                          │
      │ Output   /exports                                                                               │
      │ Files    ssp126.epw                                                                             │
      │          ssp585.epw                                                                             │
      ╰─────────────────────────────────────────────────────────────────────────────────────────────────╯
      Cases
        Scenario  Period  Member  Status                                                                
        ssp126    2060s   —       completed                                                             
        ssp585    2060s   —       completed                                                             
      -- Diagnostics -------------------------------------------------------------------------------------
      ! Persisted preview unavailable: No store is associated with this cached run.

