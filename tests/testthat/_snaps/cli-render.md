# epwshiftr_cli_render_table() renders stable boxed tables

    Code
      cat(text, sep = "\n")
    Output
      
      ── Snapshot table ──
      
      ┌────────┬───────────┬────────────┬────────┬─────────────────┬──────────┬───────────────────┐
      │ Status │ Filename  │ Bytes Done │ Size   │ Progress        │ Attempts │ Last Error        │
      │ <char> │ <char>    │ <num>      │ <num>  │ <char>          │ <int>    │ <char>            │
      ├────────┼───────────┼────────────┼────────┼─────────────────┼──────────┼───────────────────┤
      │ queued │ queued.nc │        0 B │ 1.0 KB │ [--------]   0% │        0 │                 - │
      │ error  │ failed.nc │      128 B │ 1.0 KB │ [#-------]  12% │        2 │ temporary failure │
      │ done   │ done.nc   │     1.0 KB │ 1.0 KB │ [########] 100% │        1 │                 - │
      └────────┴───────────┴────────────┴────────┴─────────────────┴──────────┴───────────────────┘

# epwshiftr_cli_render_table() snapshots narrow table adaptation

    Code
      cat(text, sep = "\n")
    Output
      
      ── Narrow table ──
      
      ┌────────┬──────────────────────────────────────────┐
      │ Status │ Filename                                 │
      │ <char> │ <char>                                   │
      ├────────┼──────────────────────────────────────────┤
      │ queued │ very-long-climate-file-name-for-cli-o... │
      └────────┴──────────────────────────────────────────┘
      ℹ Hidden columns for console width: bytes_done, size, progress, attempts, last_error, session_id, task_id, file_key. Use --json for full output.

