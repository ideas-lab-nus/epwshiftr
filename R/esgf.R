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
#'        most related variables for EPW are set as defaults. If `NULL`, all
#'        possible variables are returned. Default: `c("tas",
#'        "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "psl", "rss", "rls",
#'        "sfcWind", "pr", "clt")`, where:
#'
#' * `tas`: Near-surface (usually, 2 meter) air temperature, units: `K`.
#' * `tasmax`: Maximum near-surface (usually, 2 meter) air temperature, units: `K`.
#' * `tasmin`: Minimum near-surface (usually, 2 meter) air temperature, units: `K`.
#' * `hurs`: Near-surface relative humidity, units: `%`.
#' * `hursmax`: Maximum near-surface relative humidity, units: `%`.
#' * `hursmin`: Minimum near-surface relative humidity, units: `%`.
#' * `psl`: Sea level pressure, units: `Pa`.
#' * `rsds`: Surface downwelling shortwave radiation, units: `W m-2`.
#' * `rlds`: Surface downwelling longware radiation, units: `W m-2`.
#' * `sfcWind`: Near-surface (usually, 10 meters) wind speed, units: `m s-1`.
#' * `pr`: Precipitation, units: `kg m-2 s-1`.
#' * `clt`: Total cloud area fraction for the whole atmospheric column, as
#'   seen from the surface or the top of the atmosphere. Units: `%`.
#'
#' @param frequency A character vector of sampling frequency. If `NULL`, all
#'        possible frequencies are returned. Default: `"day"`.
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
#'        If `NULL`, all possible experiment are returned.
#'        Default: `c("ssp126", "ssp245", "ssp370", "ssp585")`.
#'
#' @param source A character vector indicating model identifiers. Defaults are
#'        set to 11 sources which give outputs of all 4 experiment of activity
#'        ScenarioMIP with daily frequency, i.e. `"AWI-CM-1-1-MR"`,
#'        `"BCC-CSM2-MR"`, `"CESM2"`, `"CESM2-WACCM"`, `"EC-Earth3"`,
#'        `"EC-Earth3-Veg"`, `"GFDL-ESM4"`, `"INM-CM4-8"`, `"INM-CM5-0"`,
#'        `"MPI-ESM1-2-HR"` and `"MRI-ESM2-0"`.
#'        If `NULL`, all possible sources are returned.
#'
#' @param variant A character vector indicating label constructed from 4
#'        indices stored as global attributes in format `r<k>i<l>p<m>f<n>`
#'        described below. Default: `"r1i1p1f1"`.
#'        If `NULL`, all possible variants are returned.
#'
#' * `r`: realization_index (<k>) = realization number (integer >0)
#' * `i`: initialization_index (<l>) = index for variant of initialization method (integer >0)
#' * `p`: physics_index (<m>) = index for model physics variant (integer >0)
#' * `f`: forcing_index (<n>) = index for variant of forcing (integer >0)
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
#'        If `NULL`, all possible resolutions are returned.
#'
#' @param type A single string indicating the intrinsic type of the record.
#'        Should be either `"Dataset"` or `"File"`. Default: `"Dataset"`.
#'
#' @param limit An integer indicating the maximum of matched records to return.
#'        Should be <= 10,000. Default: `10000`.
#'
#' @param data_node A character vector indicating data nodes to be queried.
#'        Default to `NULL`, which means all possible data nodes.
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
#'     | 1    | `dataset_id`         | Character | Dataset universal identifier                                         |
#'     | 2    | `mip_era`            | Character | Activity's associated CMIP cycle. Will always be `"CMIP6"`           |
#'     | 3    | `activity_drs`       | Character | Activity DRS (Data Reference Syntax)                                 |
#'     | 4    | `institution_id`     | Character | Institution identifier                                               |
#'     | 5    | `source_id`          | Character | Model identifier                                                     |
#'     | 6    | `experiment_id`      | Character | Root experiment identifier                                           |
#'     | 7    | `member_id`          | Character | A compound construction from `sub_experiment_id` and `variant_label` |
#'     | 8    | `table_id`           | Character | Table identifier, i.e. sampling frequency identifier                 |
#'     | 9    | `frequency`          | Character | Sampling frequency                                                   |
#'     | 10   | `grid_label`         | Character | Grid identifier                                                      |
#'     | 11   | `version`            | Character | Approximate date of model output file                                |
#'     | 12   | `nominal_resolution` | Character | Approximate horizontal resolution                                    |
#'     | 13   | `variable_id`        | Character | Variable identifier                                                  |
#'     | 14   | `variable_long_name` | Character | Variable long name                                                   |
#'     | 15   | `variable_units`     | Character | Units of variable                                                    |
#'     | 16   | `data_node`          | Character | Data node to download the model output file                          |
#'     | 17   | `dataset_pid`        | Character | A unique string that helps identify the dataset                      |
#'
#' * If `"File"`, returned columns are:
#'
#'     | No.  | Column               | Type      | Description                                                          |
#'     | ---: | -----                | -----     | -----                                                                |
#'     | 1    | `file_id`            | Character | Model output file universal identifier                               |
#'     | 2    | `dataset_id`         | Character | Dataset universal identifier                                         |
#'     | 3    | `mip_era`            | Character | Activity's associated CMIP cycle. Will always be `"CMIP6"`           |
#'     | 4    | `activity_drs`       | Character | Activity DRS (Data Reference Syntax)                                 |
#'     | 5    | `institution_id`     | Character | Institution identifier                                               |
#'     | 6    | `source_id`          | Character | Model identifier                                                     |
#'     | 7    | `experiment_id`      | Character | Root experiment identifier                                           |
#'     | 8    | `member_id`          | Character | A compound construction from `sub_experiment_id` and `variant_label` |
#'     | 9    | `table_id`           | Character | Table identifier, i.e. sampling frequency identifier                 |
#'     | 10   | `frequency`          | Character | Sampling frequency                                                   |
#'     | 11   | `grid_label`         | Character | Grid identifier                                                      |
#'     | 12   | `version`            | Character | Approximate date of model output file                                |
#'     | 13   | `nominal_resolution` | Character | Approximate horizontal resolution                                    |
#'     | 14   | `variable_id`        | Character | Variable identifier                                                  |
#'     | 15   | `variable_long_name` | Character | Variable long name                                                   |
#'     | 16   | `variable_units`     | Character | Units of variable                                                    |
#'     | 17   | `datetime_start`     | POSIXct   | Start date and time of simulation                                    |
#'     | 18   | `datetime_end`       | POSIXct   | End date and time of simulation                                      |
#'     | 19   | `file_size`          | Character | Model output file size in Bytes                                      |
#'     | 20   | `data_node`          | Character | Data node to download the model output file                          |
#'     | 21   | `file_url`           | Character | Model output file download url from HTTP server                      |
#'     | 22   | `tracking_id`        | Character | A unique string that helps identify the output file                  |
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
    variable = c("tas", "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "pr", "rsds", "rlds", "psl", "sfcWind", "clt"),
    frequency = "day",
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source = c("AWI-CM-1-1-MR", "BCC-CSM2-MR", "CESM2", "CESM2-WACCM",
               "EC-Earth3", "EC-Earth3-Veg", "GFDL-ESM4", "INM-CM4-8",
               "INM-CM5-0", "MPI-ESM1-2-HR", "MRI-ESM2-0"),
    variant = "r1i1p1f1",
    replica = FALSE,
    latest = TRUE,
    resolution = c("100 km", "50 km"),
    type = "Dataset",
    limit = 10000L,
    data_node = NULL
)
{
    assert_subset(activity, empty.ok = FALSE, choices = c(
        "AerChemMIP", "C4MIP", "CDRMIP", "CFMIP", "CMIP", "CORDEX", "DAMIP",
        "DCPP", "DynVarMIP", "FAFMIP", "GMMIP", "GeoMIP", "HighResMIP",
        "ISMIP6", "LS3MIP", "LUMIP", "OMIP", "PAMIP", "PMIP", "RFMIP", "SIMIP",
        "ScenarioMIP", "VIACSAB", "VolMIP"
    ))
    assert_character(variable, any.missing = FALSE, null.ok = TRUE)
    assert_subset(frequency, empty.ok = TRUE, choices = c(
        "1hr", "1hrCM", "1hrPt", "3hr", "3hrPt", "6hr", "6hrPt", "day", "dec",
        "fx", "mon", "monC", "monPt", "subhrPt", "yr", "yrPt"
    ))
    assert_character(experiment, any.missing = FALSE, null.ok = TRUE)
    assert_character(source, any.missing = FALSE, null.ok = TRUE)
    assert_character(variant, any.missing = FALSE, pattern = "r\\d+i\\d+p\\d+f\\d+", null.ok = TRUE)
    assert_character(resolution, any.missing = FALSE, null.ok = TRUE)
    assert_flag(replica)
    assert_flag(latest)
    assert_count(limit, positive = TRUE)
    assert_choice(type, choices = c("Dataset", "File"))
    assert_character(data_node, any.missing = FALSE, null.ok = TRUE)

    url_base <- "http://esgf-node.llnl.gov/esg-search/search/?"

    dict <- c(activity = "activity_id",
              experiment = "experiment_id",
              source = "source_id",
              variable = "variable_id",
              resolution = "nominal_resolution",
              variant = "variant_label")

    pair <- function (x, first = FALSE) {
        # get name
        var <- deparse(substitute(x))
        # skip if empty
        if (is.null(x)) return()
        # get key name
        key <- dict[names(dict) == var]
        if (!length(key)) key <- var
        if (is.logical(x)) x <- tolower(x)
        s <- paste0(key, "=", paste0(x, collapse = "%2C")) # %2C = ","
        if (first) s else paste0("&", s)
    }

    `%and%` <- function (lhs, rhs) if (is.null(rhs)) lhs else paste0(lhs, rhs)

    project <- "CMIP6"
    format <- "application%2Fsolr%2Bjson"

    resolution <- c(gsub(" ", "", resolution, fixed = TRUE),
                    gsub(" ", "+", resolution, fixed = TRUE))

    q <- url_base %and%
        pair(project, TRUE) %and%
        pair(activity) %and%
        pair(experiment) %and%
        pair(source) %and%
        pair(variable) %and%
        pair(resolution) %and%
        pair(variant) %and%
        pair(data_node) %and%
        pair(frequency) %and%
        pair(replica) %and%
        pair(latest) %and%
        pair(type) %and%
        pair(limit) %and%
        pair(format)

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
    dt <- data.table::rbindlist(lapply(q$response$docs, function (l) {
        l <- l[c("id", "mip_era", "activity_drs", "institution_id", "source_id",
            "experiment_id", "member_id", "table_id", "frequency", "grid_label",
            "version", "nominal_resolution", "variable_id", "variable_long_name",
            "variable_units", "data_node", "pid")]
        lapply(l, unlist)
    }))
    data.table::setnames(dt, c("id", "pid"), c("dataset_id", "dataset_pid"))
}
# }}}

# extract_query_file {{{
#' @importFrom data.table rbindlist setcolorder setnames tstrsplit ":="
extract_query_file <- function (q) {
    # to avoid No visible binding for global variable check NOTE
    id <- NULL
    dt_file <- data.table::rbindlist(lapply(q$response$docs, function (l) {
        l <- l[c("id", "dataset_id", "mip_era", "activity_drs", "institution_id",
            "source_id", "experiment_id", "member_id", "table_id", "frequency",
            "grid_label", "version", "nominal_resolution", "variable_id",
            "variable_long_name", "variable_units", "data_node", "size", "url",
            "tracking_id")]
        l$url <- grep("HTTPServer", unlist(l$url), fixed = TRUE, value = TRUE)
        if (!length(l$url)) {
            warning("Dataset with id '", l$id, "' does not have a HTTPServer download method.")
            l$url <- NA_character_
        }
        lapply(l, unlist)
    }))

    dt_file[, c("datetime_start", "datetime_end") := parse_file_date(id, frequency)]
    dt_file[, url := gsub("\\|.+$", "", url)]
    data.table::setnames(dt_file, c("id", "size", "url"),
        c("file_id", "file_size", "file_url"))
    data.table::setcolorder(dt_file, c(
        "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
        "source_id", "experiment_id", "member_id", "table_id", "frequency",
        "grid_label", "version", "nominal_resolution", "variable_id",
        "variable_long_name", "variable_units", "datetime_start",
        "datetime_end", "file_size", "data_node", "file_url", "tracking_id"
    ))

    dt_file
}
# }}}

# init_cmip6_index {{{
#' Build CMIP6 experiment output file index
#'
#' `init_cmip6_index()` will search the CMIP6 model output file using [esgf_query()]
#' , return a [data.table::data.table()] containing the actual NetCDF file url
#' to download, and store it into user data directory for future use.
#'
#' For details on where the file index is stored, see [rappdirs::user_data_dir()].
#'
#' @note
#' Argument `limit` will only apply to `Dataset` query. `init_cmip6_index()` will
#' try to get all model output files which match the dataset id.
#'
#' @inheritParams esgf_query
#' @param years An integer vector indicating the target years to be include in
#'        the data file. All other years will be excluded. If `NULL`, no
#'        subsetting on years will be performed. Default: `NULL`.
#' @param save If `TRUE`, the results will be saved into user data directory.
#'        Default: `FALSE`.
#'
#' @return A [data.table::data.table] with 22 columns:
#'
#' | No.  | Column               | Type      | Description                                                          |
#' | ---: | -----                | -----     | -----                                                                |
#' | 1    | `file_id`            | Character | Model output file universal identifier                               |
#' | 2    | `dataset_id`         | Character | Dataset universal identifier                                         |
#' | 3    | `mip_era`            | Character | Activity's associated CMIP cycle. Will always be `"CMIP6"`           |
#' | 4    | `activity_drs`       | Character | Activity DRS (Data Reference Syntax)                                 |
#' | 5    | `institution_id`     | Character | Institution identifier                                               |
#' | 6    | `source_id`          | Character | Model identifier                                                     |
#' | 7    | `experiment_id`      | Character | Root experiment identifier                                           |
#' | 8    | `member_id`          | Character | A compound construction from `sub_experiment_id` and `variant_label` |
#' | 9    | `table_id`           | Character | Table identifier                                                     |
#' | 10   | `frequency`          | Character | Sampling frequency                                                   |
#' | 11   | `grid_label`         | Character | Grid identifier                                                      |
#' | 12   | `version`            | Character | Approximate date of model output file                                |
#' | 13   | `nominal_resolution` | Character | Approximate horizontal resolution                                    |
#' | 14   | `variable_id`        | Character | Variable identifier                                                  |
#' | 15   | `variable_long_name` | Character | Variable long name                                                   |
#' | 16   | `variable_units`     | Character | Units of variable                                                    |
#' | 17   | `datetime_start`     | POSIXct   | Start date and time of simulation                                    |
#' | 18   | `datetime_end`       | POSIXct   | End date and time of simulation                                      |
#' | 19   | `file_size`          | Character | Model output file size in Bytes                                      |
#' | 20   | `data_node`          | Character | Data node to download the model output file                          |
#' | 21   | `dataset_pid`        | Character | A unique string that helps identify the dataset                      |
#' | 22   | `tracking_id`        | Character | A unique string that helps identify the output file                  |
#'
#' @examples
#' \dontrun{
#' init_cmip6_index()
#' }
#'
#' @importFrom checkmate assert_directory_exists assert_integerish
#' @importFrom data.table copy fwrite rbindlist set setcolorder
#' @importFrom rappdirs user_data_dir
#' @export
init_cmip6_index <- function (
    activity = "ScenarioMIP",
    variable = c("tas", "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "pr", "rsds", "rlds", "psl", "sfcWind", "clt"),
    frequency = "day",
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source = c("AWI-CM-1-1-MR", "BCC-CSM2-MR", "CESM2", "CESM2-WACCM",
               "EC-Earth3", "EC-Earth3-Veg", "GFDL-ESM4", "INM-CM4-8",
               "INM-CM5-0", "MPI-ESM1-2-HR", "MRI-ESM2-0"),
    variant = "r1i1p1f1",
    replica = FALSE,
    latest = TRUE,
    resolution = c("100 km", "50 km"),
    limit = 10000L,
    data_node = NULL,
    years = NULL,
    save = FALSE
)
{
    assert_integerish(years, lower = 1900, unique = TRUE, sorted = TRUE, any.missing = FALSE, null.ok = TRUE)
    assert_flag(save)

    verbose("Querying CMIP6 Dataset Information")
    qd <- esgf_query(activity = activity, variable = variable, frequency = frequency,
        experiment = experiment, source = source, replica = replica, latest = latest,
        variant = variant, resolution = resolution, limit = limit, type = "Dataset",
        data_node = data_node)

    if (!nrow(qd)) return(qd)

    # give a warning if dataset query response hits the limits
    if (nrow(qd) == 10000L) {
        warning("The dataset query returns 10,000 results which ",
            "hits the maximum record limitation of a single query using ESGF search RESTful API. ",
            "It is possible that the returned Dataset query responses are not complete. ",
            "It is suggested to examine and refine your query."
        )
    }

    dt <- data.table::set(qd, NULL, "file_url", NA_character_)

    # to avoid No visible binding for global variable check NOTE
    file_url <- NULL
    attempt <- 0L
    retry <- 10L
    while (nrow(nf <- dt[is.na(file_url)]) && attempt <= retry) {
        attempt <- attempt + 1L
        verbose("Querying CMIP6 File Information [Attempt ", attempt, "]")

        # to avoid No visible binding for global variable check NOTE
        .SD <- NULL

        # use qd to construction query for files
        q <- unique(nf[, .SD, .SDcols = c("activity_drs", "source_id", "member_id",
            "experiment_id", "nominal_resolution", "table_id", "frequency", "variable_id")])

        qf <- esgf_query(
            activity = unique(q$activity_drs),
            variable = unique(q$variable_id),
            frequency = unique(q$frequency),
            experiment = unique(q$experiment_id),
            source = unique(q$source_id),
            variant = unique(q$member_id),
            resolution = unique(q$nominal_resolution),
            replica = replica,
            latest = latest,
            type = "File",
            data_node = data_node
        )

        # remove all common columns in file query except for "dataset_id"
        data.table::set(qf, NULL, value = NULL,
            setdiff(intersect(names(qd), names(qf)), c("dataset_id", "file_url"))
        )

        # remove all common column in nf except for "dataset_id"
        data.table::set(nf, NULL, value = NULL,
            setdiff(intersect(names(qf), names(nf)), c("dataset_id"))
        )

        dt <- data.table::rbindlist(list(dt[!nf, on = "dataset_id"], qf[nf, on = "dataset_id"]), fill = TRUE)
    }

    verbose("Checking if data is complete")
    if (anyNA(dt$file_url)) {
        warning("There are still ", length(unique(dt$dataset_id[is.na(dt$file_url)])), " Dataset that ",
            "did not find any matched output file after ", retry, " retries."
        )
    }

    data.table::setcolorder(dt, c(
        "file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
        "source_id", "experiment_id", "member_id", "table_id", "frequency",
        "grid_label", "version", "nominal_resolution", "variable_id",
        "variable_long_name", "variable_units", "datetime_start",
        "datetime_end", "file_size", "data_node", "file_url", "dataset_pid",
        "tracking_id"
    ))

    # use non-equi join to extract matched rows
    # NOTE: CMIP6 output uses temporal amounts for saving date and time, e.g.
    # "days since 1900-01-01". In order to make sure corresponding UTC
    # referenced date and time include input years, we include the year before and
    # after also
    if (!is.null(years)) {
        exp <- data.table::data.table(
            expect_start = ISOdatetime(years - 1L, 1, 1, 0, 0, 0, "UTC"),
            expect_end = ISOdatetime(years + 1L, 12, 31, 0, 0, 0, "UTC")
        )

        dt[, `:=`(expect_start = datetime_start, expect_end = datetime_end)]
        dt <- dt[exp, on = c("expect_start<=expect_end", "expect_end>=expect_start")][
            , `:=`(expect_start = NULL, expect_end = NULL)]
    }

    # remove duplications
    dt <- unique(dt, by = "file_id")

    if (save) {
        # save database into the app data directory
        data.table::fwrite(dt, file.path(.data_dir(TRUE), "cmip6_index.csv"))
        verbose("Data file index saved to '", normalizePath(file.path(.data_dir(TRUE), "cmip6_index.csv")), "'")

        EPWSHIFTR_ENV$index_db <- data.table::copy(dt)
    }

    dt
}
# }}}

# load_cmip6_index {{{
#' Load previously stored CMIP6 experiment output file index
#'
#' @param force If `TRUE`, read the index file. Otherwise, return the
#'        cached index if exists. Default: `FALSE`.
#'
#' @return A [data.table::data.table] with 20 columns. For detail description on
#' column, see [init_cmip6_index()].
#'
#' @examples
#' \dontrun{
#' load_cmip6_index()
#' }
#' @importFrom data.table copy fread
#' @export
load_cmip6_index <- function (force = FALSE) {
    if (is.null(EPWSHIFTR_ENV$index_db)) force <- TRUE

    if (!force) {
        idx <- data.table::copy(EPWSHIFTR_ENV$index_db)
    } else {
        f <- normalizePath(file.path(.data_dir(force = FALSE), "cmip6_index.csv"), mustWork = FALSE)
        if (!file.exists(f)) {
            stop(sprintf("CMIP6 experiment output file index does not exists. You may want to create one using 'init_cmip6_index()'."))
        }

        # load file info
        idx <- tryCatch(
            data.table::fread(f, colClasses = c("version" = "character", "file_size" = "double")),
            error = function (e) {
                stop("Failed to parse CMIP6 experiment output file index.\n", conditionMessage(e))
            }
        )
        message("Loading CMIP6 experiment output file index created at ", file.info(f)$mtime, ".")

        # fix column types in case of empty values
        if ("file_path" %in% names(idx)) {
            data.table::set(idx, NULL, "file_path", as.character(idx$file_path))
            idx[J(""), on = "file_path", file_path := NA_character_]
        }
        if ("file_realsize" %in% names(idx)) {
            data.table::set(idx, NULL, "file_realsize", as.numeric(idx$file_realsize))
        }
        if ("file_mtime" %in% names(idx)) {
            # to avoid No visible binding for global variable check NOTE
            file_mtim <- NULL
            idx[J(""), on = "file_mtime", file_mtime := NA]
            idx[, file_mtime := as.POSIXct(file_mtime, origin = "1970-01-01", Sys.timezone())]
        }
        if ("time_units" %in% names(idx)) {
            data.table::set(idx, NULL, "time_units", as.character(idx$time_units))
        }
        if ("time_calendar" %in% names(idx)) {
            data.table::set(idx, NULL, "time_calendar", as.character(idx$time_calendar))
        }
        if ("datetime_start" %in% names(idx)) {
            # to avoid No visible binding for global variable check NOTE
            datetime_start <- NULL
            idx[J(""), on = "datetime_start", datetime_start := NA]
            data.table::set(idx, NULL, "datetime_start", as.POSIXct(idx$datetime_start, "UTC"))
        }
        if ("datetime_end" %in% names(idx)) {
            # to avoid No visible binding for global variable check NOTE
            datetime_end <- NULL
            idx[J(""), on = "datetime_end", datetime_end := NA]
            data.table::set(idx, NULL, "datetime_end", as.POSIXct(idx$datetime_end, "UTC"))
        }
    }

    # udpate package internal stored file index
    EPWSHIFTR_ENV$index_db <- data.table::copy(idx)

    idx[]
}
# }}}

# set_cmip6_index {{{
#' Set CMIP6 index
#'
#' `set_cmip6_index()` takes a [data.table::data.table()] as input and set it as
#' current index.
#'
#' `set_cmip6_index()` is useful when [init_cmip6_index()] may give you too much
#' cases of which only some are of interest.
#'
#' @param index A [data.table::data.table()] containing the same column names
#'        and types as the output of [init_cmip6_index()].
#'
#' @param save If `TRUE`, besides loaded index, the index file saved to data
#'        directory will be also updated. Default: `FALSE`.
#'
#' @return A [data.table::data.table()].
#' @importFrom checkmate assert_data_table
#' @export
set_cmip6_index <- function (index, save = FALSE) {
    checkmate::assert_data_table(index)
    checkmate::assert_subset(names(index),
        c("file_id", "dataset_id", "mip_era", "activity_drs", "institution_id",
        "source_id", "experiment_id", "member_id", "table_id", "frequency",
        "grid_label", "version", "nominal_resolution", "variable_id",
        "variable_long_name", "variable_units", "datetime_start",
        "datetime_end", "file_size", "data_node", "file_url", "dataset_pid",
        "tracking_id", "file_path", "file_realsize", "file_mtime", "time_units",
        "time_calendar")
    )

    # save database into the app data directory
    if (save) {
        data.table::fwrite(index, file.path(.data_dir(TRUE), "cmip6_index.csv"))
        verbose("Data file index saved to '", normalizePath(file.path(.data_dir(TRUE), "cmip6_index.csv")), "'")
    }

    # udpate package internal stored file index
    EPWSHIFTR_ENV$index_db <- data.table::copy(index)

    invisible(index)
}
# }}}

# get_data_dir {{{
#' Get the path of directory where epwshiftr data is stored
#'
#' If option `epwshiftr.dir` is set, use it. Otherwise, get package data storage
#' directory using [rappdirs::user_data_dir()].
#'
#' @return A single string.
#'
#' @examples
#' options(epwshiftr.dir = tempdir())
#' get_data_dir()
#'
#' @export
get_data_dir <- function () {
    .data_dir(force = TRUE)
}
# }}}

# get_data_node {{{
#' Get data nodes which store CMIP6 output
#'
#' @param speed_test If `TRUE`, use [pingr::ping()] to perform connection speed
#'        test on each data node. A `ping` column is appended in returned
#'        data.table which stores each data node response in milliseconds. This
#'        feature needs pingr package already installed. Default: `FALSE`.
#' @param timeout Timeout for a ping response in seconds. Default: `3`.
#'
#' @return A [data.table::data.table()] of 2 or 3 (when `speed_test` is `TRUE`)
#' columns:
#'
#' | Column      | Type      | Description                                                                     |
#' | -----       | -----     | -----                                                                           |
#' | `data_node` | character | Web address of data node                                                        |
#' | `status`    | character | Status of data node. `"UP"` means OK and `"DOWN"` means currently not available |
#' | `ping`      | double    | Data node response in milliseconds during speed test                            |
#'
#' @examples
#' \donttest{
#' get_data_node()
#' }
#'
#' @export
get_data_node <- function (speed_test = FALSE, timeout = 3) {
    # read html page
    f <- tempfile()
    utils::download.file("https://esgf-node.llnl.gov/status/", f, "libcurl", quiet = TRUE)
    l <- readLines(f)

    # locate table
    l_s <- grep("<!--load block main-->", l, fixed = TRUE)
    if (!length(l_s)) stop("Internal Error: Failed to read data node table")
    l <- l[l_s:length(l)]
    l_s <- grep("<table>", l, fixed = TRUE)[1L]
    l_e <- grep("</table>", l, fixed = TRUE)[1L]
    if (!length(l_s) || !length(l_e)) stop("Internal Error: Failed to read data node table")
    l <- l[l_s:l_e]

    # extract nodes
    loc <- regexec("\\t<td>(.+)</td>", l)
    nodes <- vapply(seq_along(l), function (i) {
        if (all(loc[[i]][1] == -1L)) return(NA_character_)
        substr(l[i], loc[[i]][2], loc[[i]][2] + attr(loc[[i]], "match.length")[2] - 1L)
    }, NA_character_)
    nodes <- nodes[!is.na(nodes)]

    # extract status
    loc <- regexec('\\t\\t<font color="#\\S{6}"><b>(UP|DOWN)</b>', l)
    status <- vapply(seq_along(l), function (i) {
        if (all(loc[[i]][1] == -1L)) return(NA_character_)
        substr(l[i], loc[[i]][2], loc[[i]][2] + attr(loc[[i]], "match.length")[2] - 1L)
    }, NA_character_)
    status <- status[!is.na(status)]

    if (length(nodes) != length(status)) stop("Internal Error: Failed to read data node table")
    res <- data.table::data.table(data_node = nodes, status = status)
    data.table::setorderv(res, "status", -1)

    if (!speed_test) return(res)

    if (!requireNamespace("pingr", quietly = TRUE)) {
        stop("'epwshiftr' relies on the package 'pingr' to perform speed test",
            "please add this to your library with install.packages('pingr') and try again."
        )
    }

    if (!length(nodes_up <- res[status == "UP", data_node])) {
        message("No working data nodes available now. Skip speed test")
        return(res)
    }

    # use the pingr package to test the connection speed
    speed <- vapply(nodes_up, function (node) {
        message(sprintf("Testing data node '%s'...", node))
        pingr::ping(node, count = 1, timeout = timeout)
    }, numeric(1))

    res[status == "UP", ping := speed][order(ping)]
}
# }}}

# parse_file_date {{{
#' @importFrom data.table fifelse
parse_file_date <- function (id, frequency) {
    dig <- fifelse(grepl("hr", frequency, fixed = TRUE), 12L,
        fifelse(frequency == "day", 8L,
            fifelse(frequency == "dec", 4L,
                fifelse(grepl("mon", frequency, fixed = TRUE), 6L,
                    fifelse(grepl("yr", frequency, fixed = TRUE), 4L, 0L)))))

    reg <- sprintf("([0-9]{%i})-([0-9]{%i})", dig, dig)
    reg[dig == 0L] <- NA_character_

    suf <- fifelse(dig == 0L, "",
        fifelse(dig == 4L, "0101",
            fifelse(dig == 6L, "01", "")))

    fmt <- fifelse(dig == 0L, NA_character_,
        fifelse(dig == 4L, "%Y%m%d",
            fifelse(dig == 6L, "%Y%m%d",
                fifelse(dig == 8L, "%Y%m%d",
                    fifelse(dig == 12L, "%Y%m%d%H%M%s", NA_character_)))))

    data.table(id, reg, suf, fmt)[
        !J(NA_character_), on = "reg", by = "reg",
        c("datetime_start", "datetime_end") := {
        m <- regexpr(.BY$reg, id)
        s <- data.table::tstrsplit(regmatches(id, m), "-", fixed = TRUE)
        lapply(s, function (x) as.POSIXct(paste0(x, suf), format = fmt[1L], tz = "UTC"))
    }][, .SD, .SDcols = c("datetime_start", "datetime_end")]
}
# }}}
