local_cv_timestamp <- function() {
    "Wed Jan 01 00:00:00 2025 +0000"
}

local_cv_version <- function(note, type = note) {
    modified <- sprintf("%s_CV_modified", type)
    note_name <- sprintf("%s_CV_note", type)
    stats::setNames(
        list("6.2.0", local_cv_timestamp(), local_cv_timestamp(), note),
        c("CV_collection_version", "CV_collection_modified", modified, note_name)
    )
}

local_write_cv <- function(dir, type, values, version_type = type) {
    path <- file.path(dir, sprintf("CMIP6_%s.json", type))
    payload <- list(values, version_metadata = local_cv_version(type, version_type))
    names(payload)[[1L]] <- type
    jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    path
}

local_write_dreq <- function(dir, table_id, realm, variables) {
    path <- file.path(dir, sprintf("CMIP6_%s.json", table_id))
    payload <- list(
        Header = list(
            data_specs_version = "1.0.0",
            cmor_version = "3.7.0",
            table_id = sprintf("Table %s", table_id),
            table_date = "03 Jan 2025",
            realm = realm,
            missing_value = "1e20",
            int_missing_value = "-999",
            mip_era = "CMIP6",
            Conventions = "CF-1.7 CMIP-6.2"
        ),
        variable_entry = variables
    )
    jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    path
}

local_cmip6_source_store <- function(root) {
    cv_dir <- file.path(root, "vocab", "test-cv")
    dreq_dir <- file.path(root, "request", "test-request")
    dir.create(cv_dir, recursive = TRUE)
    dir.create(dreq_dir, recursive = TRUE)

    local_write_cv(
        cv_dir,
        "DRS",
        list(
            directory_path_example = "CMIP6/CMIP",
            directory_path_sub_experiment_example = "CMIP6/CMIP/sub",
            directory_path_template = "<mip_era>/<activity_id>",
            filename_example = "tas_day_EC-Earth3_historical_r1i1p1f1_gn.nc",
            filename_sub_experiment_example = "tas_day_EC-Earth3_historical_s1960-r1i1p1f1_gn.nc",
            filename_template = "<variable_id>_<table_id>_<source_id>.nc"
        )
    )
    local_write_cv(cv_dir, "activity_id", list(CMIP = "CMIP activity", ScenarioMIP = "ScenarioMIP activity"))
    local_write_cv(
        cv_dir,
        "experiment_id",
        list(
            historical = list(
                experiment_id = "historical",
                activity_id = "CMIP",
                additional_allowed_model_components = character(),
                parent_activity_id = "CMIP",
                parent_experiment_id = "piControl",
                required_model_components = "AOGCM",
                sub_experiment_id = "none",
                description = "CMIP historical run",
                end_year = "2014",
                experiment = "all-forcing simulation",
                min_number_yrs_per_sim = "165",
                start_year = "1850",
                tier = "1"
            ),
            ssp585 = list(
                experiment_id = "ssp585",
                activity_id = "ScenarioMIP",
                additional_allowed_model_components = character(),
                parent_activity_id = "CMIP",
                parent_experiment_id = "historical",
                required_model_components = "AOGCM",
                sub_experiment_id = "none",
                description = "ScenarioMIP SSP5-8.5 run",
                end_year = "2100",
                experiment = "high-end scenario",
                min_number_yrs_per_sim = "86",
                start_year = "2015",
                tier = "1"
            )
        )
    )
    local_write_cv(cv_dir, "frequency", list(day = "daily mean", fx = "fixed field", mon = "monthly mean"))
    local_write_cv(cv_dir, "grid_label", list(gn = "native grid", gr = "regridded"))
    local_write_cv(cv_dir, "institution_id", list(`EC-Earth-Consortium` = "EC-Earth Consortium", CCCma = "CCCma"))
    local_write_cv(cv_dir, "nominal_resolution", c("100 km", "250 km"), "nominal_resolution")
    local_write_cv(cv_dir, "realm", list(atmos = "atmosphere", land = "land"))
    local_write_cv(cv_dir, "required_global_attributes", c("activity_id", "table_id", "variant_label"))
    local_write_cv(
        cv_dir,
        "source_id",
        list(
            `EC-Earth3` = list(
                source_id = "EC-Earth3",
                activity_participation = c("CMIP", "ScenarioMIP"),
                institution_id = "EC-Earth-Consortium",
                release_year = "2019",
                cohort = "Registered",
                label = "EC-Earth3",
                label_extended = "EC-Earth version 3",
                model_component = list(atmos = list(type = "AOGCM")),
                license_info = list(license = "CC BY 4.0")
            ),
            CanESM5 = list(
                source_id = "CanESM5",
                activity_participation = "CMIP",
                institution_id = "CCCma",
                release_year = "2020",
                cohort = "Registered",
                label = "CanESM5",
                label_extended = "Canadian Earth System Model 5",
                model_component = list(atmos = list(type = "AOGCM")),
                license_info = list(license = "CC BY 4.0")
            )
        )
    )
    local_write_cv(cv_dir, "source_type", list(AOGCM = "coupled model"))
    local_write_cv(cv_dir, "sub_experiment_id", list(none = "no sub experiment", s1960 = "start year 1960"))
    local_write_cv(cv_dir, "table_id", c("Amon", "day", "fx"))

    local_write_dreq(
        dreq_dir,
        "day",
        "atmos",
        list(
            tas = list(
                modeling_realm = "atmos",
                standard_name = "air_temperature",
                long_name = "Near-Surface Air Temperature",
                frequency = "day",
                units = "K",
                cell_methods = "time: mean",
                cell_measures = "area: areacella",
                comment = "",
                dimensions = "longitude latitude time",
                out_name = "tas",
                type = "real",
                positive = "",
                valid_min = "",
                valid_max = "",
                ok_min_mean_abs = "",
                ok_max_mean_abs = ""
            )
        )
    )
    local_write_dreq(
        dreq_dir,
        "Amon",
        "atmos",
        list(
            tas = list(
                modeling_realm = "atmos",
                standard_name = "air_temperature",
                long_name = "Near-Surface Air Temperature",
                frequency = "mon",
                units = "K",
                cell_methods = "time: mean",
                cell_measures = "area: areacella",
                comment = "",
                dimensions = "longitude latitude time",
                out_name = "tas",
                type = "real",
                positive = "",
                valid_min = "",
                valid_max = "",
                ok_min_mean_abs = "",
                ok_max_mean_abs = ""
            )
        )
    )
    local_write_dreq(
        dreq_dir,
        "fx",
        "land",
        list(
            sftlf = list(
                modeling_realm = "land",
                standard_name = "land_area_fraction",
                long_name = "Percentage of the grid cell occupied by land",
                frequency = "fx",
                units = "%",
                cell_methods = "area: mean",
                cell_measures = "area: areacella",
                comment = "",
                dimensions = "longitude latitude",
                out_name = "sftlf",
                type = "real",
                positive = "",
                valid_min = "0",
                valid_max = "100",
                ok_min_mean_abs = "",
                ok_max_mean_abs = ""
            )
        )
    )

    root
}

local_test_esgdict <- function() {
    source_root <- local_cmip6_source_store(withr::local_tempdir())
    spec <- dict__spec("CMIP6")
    fetched <- list(
        project = "CMIP6",
        vocab = dict__fetch_cv("test-cv", use_source = TRUE, source_dir = file.path(source_root, "vocab", "test-cv")),
        request = dict__fetch_dreq("test-request", use_source = TRUE, source_dir = file.path(source_root, "request", "test-request")),
        built_time = as.POSIXct("2025-01-04 00:00:00", tz = "UTC"),
        sources = list(
            vocab = list(repo = spec$vocab$repo, tag = "test-cv", commit = "abc123", source_dir = file.path(source_root, "vocab", "test-cv")),
            request = list(repo = spec$request$repo, tag = "test-request", commit = "def456", source_dir = file.path(source_root, "request", "test-request"))
        )
    )

    built <- dict__build(fetched)
    dict <- EsgDict$new()
    private <- priv(dict)
    private$replace(built, status = "built")
    dict
}

local_esgvoc_source_store <- function(root) {
    vocab_dir <- file.path(root, "vocab", "test-vocab")
    dir.create(file.path(vocab_dir, "activity"), recursive = TRUE)
    dir.create(file.path(vocab_dir, "experiment"), recursive = TRUE)
    dir.create(file.path(vocab_dir, "table"), recursive = TRUE)

    jsonlite::write_json(
        list(id = "CMIP", description = "CMIP activity"),
        file.path(vocab_dir, "activity", "CMIP.json"),
        auto_unbox = TRUE,
        pretty = TRUE
    )
    jsonlite::write_json(
        list(id = "historical", description = "historical experiment"),
        file.path(vocab_dir, "experiment", "historical.json"),
        auto_unbox = TRUE,
        pretty = TRUE
    )
    jsonlite::write_json(
        list(id = "day", description = "daily table"),
        file.path(vocab_dir, "table", "day.json"),
        auto_unbox = TRUE,
        pretty = TRUE
    )

    root
}

local_esgdict_disk_cache <- function(env = parent.frame()) {
    cache_dir <- tempfile("epwshiftr-esgdict-cache-")
    dir.create(cache_dir, recursive = TRUE)
    cache <- DiskCache$new(cache_dir, max_size = Inf, max_age = Inf, max_n = Inf, prune_on_init = FALSE)
    old <- cache__set(cache)

    withr::defer({
        cache__set(old)
        unlink(cache_dir, recursive = TRUE)
    }, envir = env)

    invisible(cache)
}

local_cache_mode_for_test <- function(value, env = parent.frame()) {
    old <- getOption("epwshiftr.cache")
    options(epwshiftr.cache = value)

    withr::defer({
        if (is.null(old)) {
            options(epwshiftr.cache = NULL)
        } else {
            options(epwshiftr.cache = old)
        }
    }, envir = env)
}
