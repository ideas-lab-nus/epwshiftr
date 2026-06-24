# DiskCache$print()

    Code
      print(cache)
    Output
      <DiskCache>
        Public:
          clone: function (deep = FALSE) 
          destroy: function () 
          exists: function (key) 
          get: function (key) 
          info: function () 
          initialize: function (dir, max_size = Inf, max_age = Inf, max_n = Inf, prune_rate = 20, 
          is_destroyed: function (throw = FALSE) 
          keys: function () 
          prune: function () 
          remove: function (key) 
          reset: function () 
          set: function (key, value) 
          size: function () 
        Private:
          atomic_write: function (value, path) 
          check_config: function (prune = TRUE) 
          destroyed: FALSE
          dir: <cache-dir>
          get_cache_path: function (key) 
          get_cache_size: function () 
          get_file_info: function () 
          last_prune_time: <time>
          list_files: function () 
          max_age: Inf
          max_n: Inf
          max_size: Inf
          metadata_file: .metadata.rds
          missing: key_missing
          parse_age: function (age) 
          parse_size: function (size) 
          prune_limit: 5
          prune_rate: 20
          set_count: <count>
          validate_key: function (key) 

