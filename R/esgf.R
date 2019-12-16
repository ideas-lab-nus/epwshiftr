# esgf_query {{{
#' Query CMIP6 data using ESGF search RESTful API
#'
#' @details
#' The Earth System Grid Federation (ESGF) is an international collaboration for
#' the software that powers most global climate change research, notably
#' assessments by the Intergovernmental Panel on Climate Change (IPCC).
#'
#' The ESGF search service exposes a RESTful URL that can be used by clients to
#' query the contents of the underlying search index, and return results
#' matching the given constraints. ributed capabilities of the ESGF search, the
#' URL at any Index Node can be used to query that Node only, or all Nodes in
#' the ESGF system. `esgf_query()` uses the [LLNL (Lawrence Livermore National
#' Laboratory)](http://esgf-node.llnl.gov)
#' Index Node.
#'
#' @param activity A character vector indicating activity identifiers. Default:
#'        `"ScenarioMIP"`. Possible values:
#'
#' * `"AerChemMIP"`: Aerosols and Chemistry Model Intercomparison Project,
#' * `"C4MIP"`: Coupled Climate Carbon Cycle Model Intercomparison Project,
#' * `"CDRMIP"`: Carbon Dioxide Removal Model Intercomparison Project,
#' * `"CFMIP"`: Cloud Feedback Model Intercomparison Project,
#' * `"CMIP"`: CMIP DECK: 1pctCO2, abrupt4xCO2, amip, esm-piControl, esm-historical, historical, and piControl experiments,
#' * `"CORDEX"`: Coordinated Regional Climate Downscaling Experiment,
#' * `"DAMIP"`: Detection and Attribution Model Intercomparison Project,
#' * `"DCPP"`: Decadal Climate Prediction Project,
#' * `"DynVarMIP"`: Dynamics and Variability Model Intercomparison Project,
#' * `"FAFMIP"`: Flux-Anomaly-Forced Model Intercomparison Project,
#' * `"GMMIP"`: Global Monsoons Model Intercomparison Project,
#' * `"GeoMIP"`: Geoengineering Model Intercomparison Project,
#' * `"HighResMIP"`: High-Resolution Model Intercomparison Project,
#' * `"ISMIP6"`: Ice Sheet Model Intercomparison Project for CMIP6,
#' * `"LS3MIP"`: Land Surface, Snow and Soil Moisture,
#' * `"LUMIP"`: Land-Use Model Intercomparison Project,
#' * `"OMIP"`: Ocean Model Intercomparison Project,
#' * `"PAMIP"`: Polar Amplification Model Intercomparison Project,
#' * `"PMIP"`: Palaeoclimate Modelling Intercomparison Project,
#' * `"RFMIP"`: Radiative Forcing Model Intercomparison Project,
#' * `"SIMIP"`: Sea Ice Model Intercomparison Project,
#' * `"ScenarioMIP"`: Scenario Model Intercomparison Project,
#' * `"VIACSAB"`: Vulnerability, Impacts, Adaptation and Climate Services Advisory Board,
#' * `"VolMIP"`: Volcanic Forcings Model Intercomparison Project
#'
#' @param variable A character vector indicating variable identifiers. The 12
#'        most related variables for EPW are set as defaults. Default: `c("tas",
#'        "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "psl", "rss", "rls",
#'        "sfcWind", "pr", "clt")`, where:
#'
#' * `tas`: Near-surface (usually, 2 meter) air temperature, units: `K`.
#' * `tasmax`: Maximum near-surface (usually, 2 meter) air temperature, units: `K`.
#' * `tasmin`: Minimum near-surface (usually, 2 meter) air temperature, units: `K`.
#' * `hurs`: Near-surface relative humidity, units: `%`.
#' * `hursmax`: Maximum near-surface relative humidity, units: `%`.
#' * `hursmin`: Minimum near-surface relative humidity, units: `%`.
#' * `psl`: sea level pressure, units: `Pa`.
#' * `rss`: Net downward shortwave radiation at the surface, units: `W m-2`.
#' * `rls`: Net downward longware radiation at the surface, units: `W m-2`.
#' * `sfcWind`: Near-surface (usually, 10 meters) wind speed, units: `m s-1`.
#' * `pr`: Precipitation, units: `kg m-2 s-1`.
#' * `clt`: Total cloud area fraction for the whole atmospheric column, as
#'   seen from the surface or the top of the atmosphere. Units: `%`.
#'
#' @param frequency A character vector of sampling frequency. Default: `"day"`.
#'        Possible values:
#'
#' * `"1hr"`: sampled hourly,
#' * `"1hrCM"`: monthly-mean diurnal cycle resolving each day into 1-hour means,
#' * `"1hrPt"`: sampled hourly, at specified time point within an hour,
#' * `"3hr"`: sampled every 3 hours,
#' * `"3hrPt"`: sampled 3 hourly, at specified time point within the time period,
#' * `"6hr"`: sampled every 6 hours,
#' * `"6hrPt"`: sampled 6 hourly, at specified time point within the time period,
#' * `"day"`: daily mean samples,
#' * `"dec"`: decadal mean samples,
#' * `"fx"`: fixed (time invariant) field,
#' * `"mon"`: monthly mean samples,
#' * `"monC"`: monthly climatology computed from monthly mean samples,
#' * `"monPt"`: sampled monthly, at specified time point within the time period,
#' * `"subhrPt"`: sampled sub-hourly, at specified time point within an hour,
#' * `"yr"`: annual mean samples,
#' * `"yrPt"`: sampled yearly, at specified time point within the time period
#'
#' @param experiment A character vector indicating root experiment identifiers.
#'        The **Tier-1** experiment of activity ScenarioMIP are set as defaults.
#'        Default: `c("ssp126", "ssp245", "ssp370", "ssp585")`.
#'
#' @param source A character vector indicating model identifiers. Defaults are
#'        set to 11 sources which give outputs of all 4 experiment of activity
#'        ScenarioMIP with daily frequency, i.e. `"AWI-CM-1-1-MR"`,
#'        `"BCC-CSM2-MR"`, `"CESM2"`, `"CESM2-WACCM"`, `"EC-Earth3"`,
#'        `"EC-Earth3-Veg"`, `"GFDL-ESM4"`, `"INM-CM4-8"`, `"INM-CM5-0"`,
#'        `"MPI-ESM1-2-HR"` and `"MRI-ESM2-0"`.
#'
#' @param replica Whether the record is the "master" copy, or a replica. Use
#'        `FALSE` to return only originals and `TRUE` to return only replicas.
#'        Default: `FALSE`.
#'
#' @param latest Whether the record is the latest available version, or a
#'        previous version. Use `TRUE` to return only the latest version of all
#'        records and `FALSE` to return previous versions. Default: `FALSE`.
#'
#' @param resolution A character vector indicating approximate horizontal
#'        resolution. Default: `c("50 km", "100 km")`.
#'
#' @param type A single string indicating the intrinsic type of the record.
#'        Should be either `"Dataset"` or `"File"`. Default: `"Dataset"`.
#'
#' @param limit An integer indicating the maximum of matched records to return.
#'        Should be <= 10,000. Default: `10000`.
#'
#' @return A [data.table::data.table] with an attribute named `response` which
#' is a list converted from json response. If no matched data is found, an empty
#' data.table is returned. Otherwise, the columns of returned data varies based
#' on the `type`:
#'
#' * If `"Dataset"`, returned columns are:
#'
#'     | No.  | Column               | Type      | Description                                                          |
#'     | ---: | -----                | -----     | -----                                                                |
#'     | 1    | `id`                 | Character | Dataset universal identifier                                         |
#'     | 2    | `mip_era`            | Character | Activity's associated CMIP cycle. Will always be `"CMIP6"`           |
#'     | 3    | `activity_drs`       | Character | Activity DRS (Data Reference Syntax)                                 |
#'     | 4    | `institution_id`     | Character | Institution identifier                                               |
#'     | 5    | `source_id`          | Character | Model identifier                                                     |
#'     | 6    | `experiment_id`      | Character | Root experiment identifier                                           |
#'     | 7    | `member_id`          | Character | A compound construction from `sub_experiment_id` and `variant_label` |
#'     | 8    | `table_id`           | Character | Table identifier                                                     |
#'     | 9    | `grid_label`         | Character | Grid identifier                                                      |
#'     | 10   | `version`            | Character | Approximate date of model output file                                |
#'     | 11   | `nominal_resolution` | Character | Approximate horizontal resolution                                    |
#'     | 12   | `variable_id`        | Character | Variable identifier                                                  |
#'     | 13   | `variable_long_name` | Character | Variable long name                                                   |
#'     | 14   | `variable_units`     | Character | Units of variable                                                    |
#'     | 15   | `data_node`          | Character | Data node to download the model output file                          |
#'
#' * If `"File"`, returned columns are:
#'
#'     | No.  | Column               | Type      | Description                                                          |
#'     | ---: | -----                | -----     | -----                                                                |
#'     | 1    | `id`                 | Character | Model output file universal identifier                               |
#'     | 2    | `dataset_id`         | Character | Dataset universal identifier                                         |
#'     | 3    | `mip_era`            | Character | Activity's associated CMIP cycle. Will always be `"CMIP6"`           |
#'     | 4    | `activity_drs`       | Character | Activity DRS (Data Reference Syntax)                                 |
#'     | 5    | `institution_id`     | Character | Institution identifier                                               |
#'     | 6    | `source_id`          | Character | Model identifier                                                     |
#'     | 7    | `experiment_id`      | Character | Root experiment identifier                                           |
#'     | 8    | `member_id`          | Character | A compound construction from `sub_experiment_id` and `variant_label` |
#'     | 9    | `table_id`           | Character | Table identifier                                                     |
#'     | 10   | `grid_label`         | Character | Grid identifier                                                      |
#'     | 11   | `version`            | Character | Approximate date of model output file                                |
#'     | 12   | `nominal_resolution` | Character | Approximate horizontal resolution                                    |
#'     | 13   | `variable_id`        | Character | Variable identifier                                                  |
#'     | 14   | `variable_long_name` | Character | Variable long name                                                   |
#'     | 15   | `variable_units`     | Character | Units of variable                                                    |
#'     | 16   | `datetime_start`     | POSIXct   | Start date and time of simulation                                    |
#'     | 17   | `datetime_end`       | POSIXct   | End date and time of simulation                                      |
#'     | 18   | `file_size`          | Character | Model output file size in Bytes                                      |
#'     | 19   | `data_node`          | Character | Data node to download the model output file                          |
#'     | 20   | `url`                | Character | Model output file download url from HTTP server                      |
#'
#' @references
#' https://github.com/ESGF/esgf.github.io/wiki/ESGF_Search_REST_API
#'
#' @examples
#' \dontrun{
#' esgf_query(variable = "rss", experiment = "ssp126", resolution = "100 km", limit = 1)
#'
#' esgf_query(variable = "rss", experiment = "ssp126", type = "File", limit = 1)
#' }
#'
#' @importFrom jsonlite read_json
#' @importFrom checkmate assert_character assert_choice assert_count assert_flag assert_subset
#' @importFrom data.table data.table setattr
#' @export
esgf_query <- function (
    activity = "ScenarioMIP",
    variable = c("tas", "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "pr", "rss", "rls", "psl", "sfcWind", "clt"),
    frequency = "day",
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source = c("AWI-CM-1-1-MR", "BCC-CSM2-MR", "CESM2", "CESM2-WACCM",
               "EC-Earth3", "EC-Earth3-Veg", "GFDL-ESM4", "INM-CM4-8",
               "INM-CM5-0", "MPI-ESM1-2-HR", "MRI-ESM2-0"),
    replica = FALSE,
    latest = TRUE,
    resolution = c("100 km", "50 km"),
    type = "Dataset",
    limit = 10000L
)
{
    assert_subset(activity, empty.ok = FALSE, choices = c(
        "AerChemMIP", "C4MIP", "CDRMIP", "CFMIP", "CMIP", "CORDEX", "DAMIP",
        "DCPP", "DynVarMIP", "FAFMIP", "GMMIP", "GeoMIP", "HighResMIP",
        "ISMIP6", "LS3MIP", "LUMIP", "OMIP", "PAMIP", "PMIP", "RFMIP", "SIMIP",
        "ScenarioMIP", "VIACSAB", "VolMIP"
    ))
    assert_character(variable, any.missing = FALSE)
    assert_subset(frequency, empty.ok = FALSE, choices = c(
        "1hr", "1hrCM", "1hrPt", "3hr", "3hrPt", "6hr", "6hrPt", "day", "dec",
        "fx", "mon", "monC", "monPt", "subhrPt", "yr", "yrPt"
    ))
    assert_character(experiment, any.missing = FALSE)
    assert_character(source, any.missing = FALSE)
    assert_character(resolution, any.missing = FALSE)
    assert_flag(replica)
    assert_flag(latest)
    assert_count(limit, positive = TRUE)
    assert_choice(type, choices = c("Dataset", "File"))

    url_base <- "http://esgf-node.llnl.gov/esg-search/search/?"
    or <- function (...) paste(..., sep = "%2C", collapse = "%2C") # %2C = ","
    resolution <- c(gsub(" ", "", resolution, fixed = TRUE),
                    gsub(" ", "+", resolution, fixed = TRUE))

    project <- "CMIP6"
    variant <- "r1i1p1f1"
    format <- "application%2Fsolr%2Bjson"

    q <- paste0(url_base,
                   "project=" ,          project , "&" ,
               "activity_id=" ,     or(activity) , "&" ,
             "experiment_id=" ,   or(experiment) , "&" ,
                 "source_id=" ,       or(source) , "&" ,
               "variable_id=" ,     or(variable) , "&" ,
        "nominal_resolution=" ,   or(resolution) , "&" ,
                 "frequency=" ,    or(frequency) , "&" ,
                   "replica=" , tolower(replica) , "&" ,
                    "latest=" ,  tolower(latest) , "&" ,
             "variant_label=" ,      or(variant) , "&" ,
                      "type=" ,             type , "&" ,
                     "limit=" ,            limit , "&" ,
                    "format=" ,           format
    )

    q <- jsonlite::read_json(q)

    if (q$response$numFound == 0L) {
        message("No matched data. Please examine the actual response using 'attr(x, \"response\")'.")
        dt <- data.table::data.table()
    } else if (type == "Dataset") {
        dt <- extract_query_dataset(q)
    } else if (type == "File") {
        dt <- extract_query_file(q)
    }

    data.table::setattr(dt, "response", q)

    dt
}
# }}}

# extract_query_dataset {{{
#' @importFrom data.table rbindlist
extract_query_dataset <- function (q) {
    data.table::rbindlist(lapply(q$response$docs, function (l) {
        l <- l[c("id", "mip_era", "activity_drs", "institution_id", "source_id",
            "experiment_id", "member_id", "table_id", "grid_label",
            "version", "nominal_resolution", "variable_id", "variable_long_name",
            "variable_units", "data_node")]
        lapply(l, unlist)
    }))
}
# }}}

# extract_query_file {{{
#' @importFrom data.table rbindlist setcolorder setnames tstrsplit ":="
extract_query_file <- function (q) {
    # to avoid No visible binding for global variable check NOTE
    id <- NULL
    dt_file <- data.table::rbindlist(lapply(q$response$docs, function (l) {
        l <- l[c("id", "dataset_id", "mip_era", "activity_drs", "institution_id",
            "source_id", "experiment_id", "member_id", "table_id", "grid_label",
            "version", "nominal_resolution", "variable_id", "variable_long_name",
            "variable_units", "data_node", "size", "url")]
        l$url <- grep("HTTPServer", unlist(l$url), fixed = TRUE, value = TRUE)
        lapply(l, unlist)
    }))[, c("datetime_start", "datetime_end") := {
        m <- regexpr("([0-9]{8})-([0-9]{8})", id)
        s <- data.table::tstrsplit(regmatches(id, m), "-", fixed = TRUE)
        lapply(s, as.POSIXct, format = "%Y%m%d", tz = "UTC")
    }][, url := gsub("\\|.+$", "", url)]
    data.table::setnames(dt_file, c("size"), c("file_size"))
    data.table::setcolorder(dt_file, c(
        "id", "dataset_id", "mip_era", "activity_drs", "institution_id",
        "source_id", "experiment_id", "member_id", "table_id", "grid_label",
        "version", "nominal_resolution", "variable_id", "variable_long_name",
        "variable_units", "datetime_start", "datetime_end", "file_size",
        "data_node", "url"
    ))

    dt_file
}
# }}}
