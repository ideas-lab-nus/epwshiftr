# Build the minimal CMIP6 ESG dictionary used by EsgDict examples.
#
# Run from the package root:
# source("data-raw/build-example-esgdict.R")

for (file in c(
    "R/utils.R",
    "R/import-standalone-schema.R",
    "R/dict.R",
    "R/dict-parse-cv.R",
    "R/dict-parse-dreq.R",
    "R/dict-fetch.R",
    "R/dict-check.R",
    "R/dict-store.R"
)) {
    source(file, local = globalenv())
}

SCHEMA_ESG_DICT <- schema_read(file.path("inst", "extdata", "schema", "esg-dict.json"))

example_cv_timestamp <- function() {
    "Wed Jan 01 00:00:00 2025 +0000"
}

example_cv_version <- function(note, type = note) {
    modified <- sprintf("%s_CV_modified", type)
    note_name <- sprintf("%s_CV_note", type)
    stats::setNames(
        list("6.2.0", example_cv_timestamp(), example_cv_timestamp(), note),
        c("CV_collection_version", "CV_collection_modified", modified, note_name)
    )
}

example_write_cv <- function(dir, type, values, version_type = type) {
    path <- file.path(dir, sprintf("CMIP6_%s.json", type))
    payload <- list(values, version_metadata = example_cv_version(type, version_type))
    names(payload)[[1L]] <- type
    jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    path
}

example_write_dreq <- function(dir, table_id, realm, variables) {
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

example_cmip6_source_store <- function(root) {
    cv_dir <- file.path(root, "vocab", "test-cv")
    dreq_dir <- file.path(root, "request", "test-request")
    dir.create(cv_dir, recursive = TRUE)
    dir.create(dreq_dir, recursive = TRUE)

    example_write_cv(
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
    example_write_cv(cv_dir, "activity_id", list(CMIP = "CMIP activity", ScenarioMIP = "ScenarioMIP activity"))
    example_write_cv(
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
    example_write_cv(cv_dir, "frequency", list(day = "daily mean", fx = "fixed field", mon = "monthly mean"))
    example_write_cv(cv_dir, "grid_label", list(gn = "native grid", gr = "regridded"))
    example_write_cv(cv_dir, "institution_id", list(`EC-Earth-Consortium` = "EC-Earth Consortium", CCCma = "CCCma"))
    example_write_cv(cv_dir, "nominal_resolution", c("100 km", "250 km"), "nominal_resolution")
    example_write_cv(cv_dir, "realm", list(atmos = "atmosphere", land = "land"))
    example_write_cv(cv_dir, "required_global_attributes", c("activity_id", "table_id", "variant_label"))
    example_write_cv(
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
    example_write_cv(cv_dir, "source_type", list(AOGCM = "coupled model"))
    example_write_cv(cv_dir, "sub_experiment_id", list(none = "no sub experiment", s1960 = "start year 1960"))
    example_write_cv(cv_dir, "table_id", c("Amon", "day", "fx"))

    tas <- list(
        modeling_realm = "atmos",
        standard_name = "air_temperature",
        long_name = "Near-Surface Air Temperature",
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
    example_write_dreq(dreq_dir, "day", "atmos", list(tas = c(tas, frequency = "day")))
    example_write_dreq(dreq_dir, "Amon", "atmos", list(tas = c(tas, frequency = "mon")))
    example_write_dreq(
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

source_root <- tempfile("epwshiftr-example-esgdict-")
dir.create(source_root, recursive = TRUE)
on.exit(unlink(source_root, recursive = TRUE), add = TRUE)
example_cmip6_source_store(source_root)

spec <- esgdict__project_spec("CMIP6")
fetched <- list(
    project = "CMIP6",
    vocab = cmip6dict__fetch_cv(
        "test-cv",
        use_source = TRUE,
        source_dir = file.path(source_root, "vocab", "test-cv"),
        offline = TRUE
    ),
    request = cmip6dict__fetch_dreq(
        "test-request",
        use_source = TRUE,
        source_dir = file.path(source_root, "request", "test-request"),
        offline = TRUE
    ),
    built_time = as.POSIXct("2025-01-04 00:00:00", tz = "UTC"),
    sources = list(
        vocab = list(repo = spec$vocab$repo, tag = "test-cv", commit = "example-vocab", source_dir = "example"),
        request = list(repo = spec$request$repo, tag = "test-request", commit = "example-request", source_dir = "example")
    )
)

built <- esgdict__build(fetched)
out_dir <- file.path("inst", "extdata", "examples")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(out_dir, "cmip6-dict.json")
esgdict__save(
    built$project,
    built$profile,
    built$built_time,
    built$data,
    built$version,
    built$timestamps,
    built$sources,
    built$indices,
    path = out
)

message("Wrote ", normalizePath(out, winslash = "/", mustWork = TRUE))
