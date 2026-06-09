RES_DATASET <- c(
    "id",
    "mip_era",
    "activity_drs",
    "institution_id",
    "source_id",
    "experiment_id",
    "member_id",
    "table_id",
    "frequency",
    "grid_label",
    "version",
    "nominal_resolution",
    "variable_id",
    "variable_long_name",
    "variable_units",
    "data_node",
    "pid"
)

RES_FILE <- c(
    "id",
    "dataset_id",
    "mip_era",
    "activity_drs",
    "institution_id",
    "source_id",
    "experiment_id",
    "member_id",
    "table_id",
    "frequency",
    "grid_label",
    "version",
    "nominal_resolution",
    "variable_id",
    "variable_long_name",
    "variable_units",
    "data_node",
    "size",
    "url",
    "tracking_id"
)

esgf_query_deprecate <- function() {
    if (isTRUE(this$esgf_query_deprecated_warned)) {
        return(invisible(FALSE))
    }

    this$esgf_query_deprecated_warned <- TRUE
    warning(
        "`esgf_query()` is deprecated; please use `esg_query()` / `EsgQuery` for new code. Returning legacy-compatible results for now.",
        call. = FALSE
    )

    invisible(TRUE)
}

esgf_host_to_index_node <- function(host) {
    checkmate::assert_string(host)

    host <- curl::curl_unescape(host)
    host <- sub("/search/?$", "", host)
    host <- sub("/esg-search/?$", "", host)
    host <- sub("/+$", "", host)

    normalize_index_node(host)
}

esgf_subset_response_docs <- function(response, fields) {
    docs <- response$response$docs
    if (is.null(docs)) {
        return(response)
    }

    keep <- intersect(fields, names(docs))
    if (is.data.frame(docs)) {
        response$response$docs <- docs[, keep, drop = FALSE]
    } else if (is.list(docs)) {
        response$response$docs <- docs[keep]
    }

    response
}

esgf_query_normalize_legacy_fq <- function(response, replica, latest) {
    params <- response$responseHeader$params
    if (is.null(params) || is.null(params$fq)) {
        return(response)
    }

    fq <- unlist(params$fq, use.names = FALSE)
    set_flag <- function(x, name, value) {
        x <- x[!grepl(sprintf("^%s:", name), x)]
        c(x, sprintf("%s:%s", name, tolower(as.character(value))))
    }

    if (!is.null(replica)) {
        fq <- set_flag(fq, "replica", replica)
    }
    if (!is.null(latest)) {
        fq <- set_flag(fq, "latest", latest)
    }

    response$responseHeader$params$fq <- fq
    response
}

esgf_query_collect <- function(q) {
    withCallingHandlers(
        query_collect(
            priv(q)$index_node_url,
            priv(q)$parameter,
            required_fields = NULL,
            all = FALSE,
            limit = TRUE,
            constraints = FALSE
        ),
        warning = function(w) {
            if (grepl("^No matched data\\.", conditionMessage(w))) {
                invokeRestart("muffleWarning")
            }
        }
    )
}

esgf_query_empty <- function(response) {
    dt <- data.table::data.table()
    data.table::setattr(dt, "response", response)
    dt
}

esgf_query_build <- function(host, type, activity, variable, frequency, experiment, source, variant, replica, latest, resolution, limit, data_node) {
    index_node <- esgf_host_to_index_node(host)
    q <- esg_query(index_node)
    resolution_param <- NULL

    if (!is.null(resolution)) {
        resolution_param <- unique(c(
            gsub(" ", "", resolution, fixed = TRUE),
            gsub(" ", "+", resolution, fixed = TRUE)
        ))
        attr(resolution_param, "encoded") <- TRUE
    }

    # keep the legacy host semantics and only reuse the new query stack for
    # parameter normalization and request execution.
    q$index_node(index_node)

    q$project("CMIP6")
    q$activity_id(activity)
    q$variable_id(variable)
    q$frequency(frequency)
    q$experiment_id(experiment)
    q$source_id(source)
    q$variant_label(variant)
    q$replica(replica)
    q$latest(latest)
    q$data_node(data_node)
    q$nominal_resolution(resolution_param)
    q$limit(as.integer(limit))
    q$fields(RES_DATASET)

    q
}

# esgf_query {{{
#' Legacy-compatible CMIP6 query wrapper using ESGF search RESTful API
#'
#' `esgf_query()` is a compatibility wrapper around [esg_query()] for the legacy
#' data.table-oriented CMIP6 query workflow.
#'
#' For new code, start with [esg_query()] / [EsgQuery]. `esgf_query()` emits a
#' gentle deprecation warning and preserves the historical `host` interface,
#' including the default LLNL search host instead of `esg_query()`'s ORNL
#' `index_node` default.
#'
#' @details
#' The Earth System Grid Federation (ESGF) is an international collaboration for
#' the software that powers most global climate change research, notably
#' assessments by the Intergovernmental Panel on Climate Change (IPCC).
#'
#' The ESGF search service exposes a RESTful URL that can be used by clients to
#' query the contents of the underlying search index, and return results
#' matching the given constraints. With the distributed capabilities of the ESGF
#' search, the URL at any Index Node can be used to query that Node only, or all
#' Nodes in the ESGF system. For compatibility, `esgf_query()` keeps the legacy
#' [LLNL (Lawrence Livermore National Laboratory) Index Node](http://esgf-node.llnl.gov)
#' `host` default instead of `esg_query()`'s ORNL `index_node` default.
#'
#' The core Controlled Vocabularies (CVs) for use in CMIP6, including all
#' activities, experiment, sources (GCMs), frequencies can be found at the
#' [WCRP-CMIP/CMIP6_CVs](https://github.com/WCRP-CMIP/CMIP6_CVs) GitHub repo.
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
#' * `rlds`: Surface downwelling longwave radiation, units: `W m-2`.
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
#' * `r`: realization_index (`<k>`) = realization number (integer >0)
#' * `i`: initialization_index (`<l>`) = index for variant of initialization method (integer >0)
#' * `p`: physics_index (`<m>`) = index for model physics variant (integer >0)
#' * `f`: forcing_index (`<n>`) = index for variant of forcing (integer >0)
#'
#' @param replica Whether the record is the "master" copy, or a replica. Use
#'        `FALSE` to return only originals and `TRUE` to return only replicas.
#'        Use `NULL` to return both the master and the replicas.
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
#' @param host The URL to the ESGF Search API service. This should be the URL of
#'        the ESGF search service excluding the final endpoint name. Usually
#'        this is `http://<hostname>/esg-search`. This legacy `host` argument is
#'        preserved for compatibility and keeps the legacy LLNL Search API
#'        semantics. Default is set to the
#'        [LLNL (Lawrence Livermore National Laboratory) Index Node](http://esgf-node.llnl.gov),
#'        which is `"https://esgf-node.llnl.gov/esg-search"`. For new code,
#'        prefer `esg_query(index_node = ...)`.
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
#' @seealso [esg_query()], [EsgQuery]
#'
#' @examples
#' \dontrun{
#' esgf_query(variable = "rss", experiment = "ssp126", resolution = "100 km", limit = 1)
#'
#' esgf_query(variable = "rss", experiment = "ssp126", type = "File", limit = 1)
#' }
#'
#' @export
esgf_query <- function(
    activity = "ScenarioMIP",
    variable = c(
        "tas",
        "tasmax",
        "tasmin",
        "hurs",
        "hursmax",
        "hursmin",
        "pr",
        "rsds",
        "rlds",
        "psl",
        "sfcWind",
        "clt"
    ),
    frequency = "day",
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source = c(
        "AWI-CM-1-1-MR",
        "BCC-CSM2-MR",
        "CESM2",
        "CESM2-WACCM",
        "EC-Earth3",
        "EC-Earth3-Veg",
        "GFDL-ESM4",
        "INM-CM4-8",
        "INM-CM5-0",
        "MPI-ESM1-2-HR",
        "MRI-ESM2-0"
    ),
    variant = "r1i1p1f1",
    replica = FALSE,
    latest = TRUE,
    resolution = c("100 km", "50 km"),
    type = "Dataset",
    limit = 10000L,
    data_node = NULL,
    host = "http://esgf-node.llnl.gov/esg-search"
) {
    checkmate::assert_subset(
        activity,
        empty.ok = FALSE,
        choices = c(
            "AerChemMIP",
            "C4MIP",
            "CDRMIP",
            "CFMIP",
            "CMIP",
            "CORDEX",
            "DAMIP",
            "DCPP",
            "DynVarMIP",
            "FAFMIP",
            "GMMIP",
            "GeoMIP",
            "HighResMIP",
            "ISMIP6",
            "LS3MIP",
            "LUMIP",
            "OMIP",
            "PAMIP",
            "PMIP",
            "RFMIP",
            "SIMIP",
            "ScenarioMIP",
            "VIACSAB",
            "VolMIP"
        )
    )
    checkmate::assert_character(variable, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_subset(
        frequency,
        empty.ok = TRUE,
        choices = c(
            "1hr",
            "1hrCM",
            "1hrPt",
            "3hr",
            "3hrPt",
            "6hr",
            "6hrPt",
            "day",
            "dec",
            "fx",
            "mon",
            "monC",
            "monPt",
            "subhrPt",
            "yr",
            "yrPt"
        )
    )
    checkmate::assert_character(experiment, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_character(source, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_character(variant, any.missing = FALSE, pattern = "r\\d+i\\d+p\\d+f\\d+", null.ok = TRUE)
    checkmate::assert_character(resolution, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_flag(replica, null.ok = TRUE)
    checkmate::assert_flag(latest)
    checkmate::assert_count(limit, positive = TRUE)
    checkmate::assert_choice(type, choices = c("Dataset", "File"))
    checkmate::assert_character(data_node, any.missing = FALSE, null.ok = TRUE)

    esgf_query_deprecate()

    q <- esgf_query_build(
        host = host,
        type = type,
        activity = activity,
        variable = variable,
        frequency = frequency,
        experiment = experiment,
        source = source,
        variant = variant,
        replica = replica,
        latest = latest,
        resolution = resolution,
        limit = limit,
        data_node = data_node
    )

    response <- tryCatch(
        {
            res <- esgf_query_collect(q)
            res$response$response$docs <- res$docs
            response <- if (type == "Dataset" || res$response$response$numFound == 0L) {
                esgf_subset_response_docs(res$response, RES_DATASET)
            } else {
                datasets <- new_query_result(
                    EsgResultDataset,
                    priv(q)$index_node_url,
                    if (!is.null(res$parameter)) res$parameter else priv(q)$parameter,
                    res$response
                )
                collect_args <- list(
                    fields = RES_FILE,
                    all = FALSE,
                    limit = limit,
                    type = "File",
                    latest = latest
                )
                if (!is.null(replica)) {
                    collect_args$replica <- replica
                }

                files <- do.call(datasets$collect, collect_args)
                esgf_subset_response_docs(priv(files)$response, RES_FILE)
            }

            esgf_query_normalize_legacy_fq(response, replica = replica, latest = latest)
        },
        error = function(e) e
    )

    # nocov start
    if (inherits(response, "warning") || inherits(response, "error")) {
        message("No matched data. Please check network connection and the availability of LLNL ESGF node.")
        return(esgf_query_empty(response))
    }
    # nocov end

    if (response$response$numFound == 0L) {
        message("No matched data. Please examine the actual response using 'attr(x, \"response\")'.")
        return(esgf_query_empty(response))
    }

    dt <- if (type == "Dataset") {
        extract_query_dataset(response)
    } else {
        extract_query_file(response)
    }

    data.table::setattr(dt, "response", response)

    dt
}
# }}}

# extract_query_dataset {{{
#' @importFrom data.table rbindlist
extract_query_dataset <- function(q) {
    dt <- data.table::as.data.table(q$response$docs)
    data.table::set(dt, NULL, setdiff(names(dt), RES_DATASET), NULL)
    data.table::setcolorder(dt, RES_DATASET)
    for (col in names(dt)) {
        if (is.list(.subset2(dt, col))) {
            data.table::set(dt, NULL, col, unlst(.subset2(dt, col)))
        }
    }
    data.table::setnames(dt, c("id", "pid"), c("dataset_id", "dataset_pid"))
    if ("version" %in% names(dt)) {
        data.table::set(dt, NULL, "version", as.character(dt$version))
    }

    dt
}
# }}}

# extract_query_file {{{
#' @importFrom data.table rbindlist setcolorder setnames tstrsplit ":="
extract_query_file <- function(q) {
    # to avoid No visible binding for global variable check NOTE
    id <- NULL

    dt <- data.table::as.data.table(q$response$docs)
    data.table::set(dt, NULL, setdiff(names(dt), RES_FILE), NULL)
    data.table::setcolorder(dt, RES_FILE)

    data.table::set(
        dt,
        NULL,
        "url",
        vapply(seq_len(nrow(dt)), FUN.VALUE = "", function(i) {
            url <- grep("HTTPServer", .subset2(dt, "url")[[i]], fixed = TRUE, value = TRUE)

            # nocov start
            if (!length(url)) {
                warning("Dataset with id '", .subset2(dt, "id")[[i]], "' does not have a HTTPServer download method.")
                url <- NA_character_
            }
            # nocov end

            url
        })
    )

    for (col in names(dt)) {
        if (is.list(.subset2(dt, col))) {
            data.table::set(dt, NULL, col, unlst(.subset2(dt, col)))
        }
    }

    dt[, c("datetime_start", "datetime_end") := parse_file_date(id, frequency)]
    dt[, url := gsub("\\|.+$", "", url)]
    data.table::setnames(
        dt,
        c("id", "size", "url"),
        c("file_id", "file_size", "file_url")
    )
    if ("version" %in% names(dt)) {
        data.table::set(dt, NULL, "version", as.character(dt$version))
    }
    data.table::setcolorder(
        dt,
        c(
            "file_id",
            "dataset_id",
            "mip_era",
            "activity_drs",
            "institution_id",
            "source_id",
            "experiment_id",
            "member_id",
            "table_id",
            "frequency",
            "grid_label",
            "version",
            "nominal_resolution",
            "variable_id",
            "variable_long_name",
            "variable_units",
            "datetime_start",
            "datetime_end",
            "file_size",
            "data_node",
            "file_url",
            "tracking_id"
        )
    )

    dt
}
# }}}

# init_cmip6_index {{{
#' Build CMIP6 experiment output file index
#'
#' `init_cmip6_index()` builds a CMIP6 file index by issuing Dataset/File
#' lookups through the legacy-compatible [esgf_query()] wrapper so existing
#' host-based workflows keep working. For new direct ESGF searches, start with
#' [esg_query()] / [EsgQuery]. It returns a
#' [data.table::data.table()] containing the actual NetCDF file url to
#' download, and can store it in the persistent epwshiftr store for future use.
#'
#' @note
#' Argument `limit` will only apply to `Dataset` query. `init_cmip6_index()` will
#' try to get all model output files which match the dataset id.
#'
#' The inherited `host` argument keeps the legacy LLNL Search API semantics from
#' [esgf_query()], so its default is not the same as `esg_query()`'s ORNL
#' `index_node` default.
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
#' @importFrom data.table copy fwrite rbindlist set setcolorder
#' @export
init_cmip6_index <- function(
    activity = "ScenarioMIP",
    variable = c(
        "tas",
        "tasmax",
        "tasmin",
        "hurs",
        "hursmax",
        "hursmin",
        "pr",
        "rsds",
        "rlds",
        "psl",
        "sfcWind",
        "clt"
    ),
    frequency = "day",
    experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
    source = c(
        "AWI-CM-1-1-MR",
        "BCC-CSM2-MR",
        "CESM2",
        "CESM2-WACCM",
        "EC-Earth3",
        "EC-Earth3-Veg",
        "GFDL-ESM4",
        "INM-CM4-8",
        "INM-CM5-0",
        "MPI-ESM1-2-HR",
        "MRI-ESM2-0"
    ),
    variant = "r1i1p1f1",
    replica = FALSE,
    latest = TRUE,
    resolution = c("100 km", "50 km"),
    limit = 10000L,
    data_node = NULL,
    years = NULL,
    save = FALSE,
    host = "http://esgf-node.llnl.gov/esg-search"
) {
    checkmate::assert_integerish(years, lower = 1900, unique = TRUE, sorted = TRUE, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_flag(save)

    vmsg("Querying CMIP6 Dataset Information")
    qd <- esgf_query(
        activity = activity,
        variable = variable,
        frequency = frequency,
        experiment = experiment,
        source = source,
        replica = replica,
        latest = latest,
        variant = variant,
        resolution = resolution,
        limit = limit,
        type = "Dataset",
        data_node = data_node,
        host = host
    )

    if (!nrow(qd)) {
        return(qd)
    }

    # nocov start
    # give a warning if dataset query response hits the limits
    if (nrow(qd) == 10000L) {
        warning(
            "The dataset query returns 10,000 results which ",
            "hits the maximum record limitation of a single query using ESGF search RESTful API. ",
            "It is possible that the returned Dataset query responses are not complete. ",
            "It is suggested to examine and refine your query."
        )
    }
    # nocov end

    dt <- data.table::set(qd, NULL, "file_url", NA_character_)

    # to avoid No visible binding for global variable check NOTE
    file_url <- NULL
    attempt <- 0L
    retry <- 10L
    while (nrow(nf <- dt[is.na(file_url)]) && attempt <= retry) {
        attempt <- attempt + 1L
        vmsg(sprintf("Querying CMIP6 File Information [Attempt %s]", attempt))

        # to avoid No visible binding for global variable check NOTE
        .SD <- NULL

        # use qd to construction query for files
        q <- unique(nf[,
            .SD,
            .SDcols = c(
                "activity_drs",
                "source_id",
                "member_id",
                "experiment_id",
                "nominal_resolution",
                "table_id",
                "frequency",
                "variable_id"
            )
        ])

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
            data_node = data_node,
            host = host
        )

        required_qf_cols <- list(
            file_id = character(),
            dataset_id = character(),
            datetime_start = as.POSIXct(character(), tz = "UTC"),
            datetime_end = as.POSIXct(character(), tz = "UTC"),
            file_size = character(),
            file_url = character(),
            tracking_id = character()
        )
        for (nm in setdiff(names(required_qf_cols), names(qf))) {
            data.table::set(qf, NULL, nm, required_qf_cols[[nm]])
        }

        # remove all common columns in file query except for "dataset_id"
        data.table::set(qf, NULL, value = NULL, setdiff(intersect(names(qd), names(qf)), c("dataset_id", "file_url")))

        # remove all common column in nf except for "dataset_id"
        data.table::set(nf, NULL, value = NULL, setdiff(intersect(names(qf), names(nf)), c("dataset_id")))

        dt <- data.table::rbindlist(list(dt[!nf, on = "dataset_id"], qf[nf, on = "dataset_id"]), fill = TRUE)
    }

    vmsg("Checking if data is complete")
    # nocov start
    if (anyNA(dt$file_url)) {
        warning(
            "There are still ",
            length(unique(dt$dataset_id[is.na(dt$file_url)])),
            " Dataset that ",
            "did not find any matched output file after ",
            retry,
            " retries."
        )
    }
    # nocov end

    data.table::setcolorder(
        dt,
        c(
            "file_id",
            "dataset_id",
            "mip_era",
            "activity_drs",
            "institution_id",
            "source_id",
            "experiment_id",
            "member_id",
            "table_id",
            "frequency",
            "grid_label",
            "version",
            "nominal_resolution",
            "variable_id",
            "variable_long_name",
            "variable_units",
            "datetime_start",
            "datetime_end",
            "file_size",
            "data_node",
            "file_url",
            "dataset_pid",
            "tracking_id"
        )
    )

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
        dt <- dt[exp, on = c("expect_start<=expect_end", "expect_end>=expect_start")][,
            `:=`(expect_start = NULL, expect_end = NULL)
        ]
    }

    # remove duplications
    dt <- unique(dt, by = "file_id")

    if (save) {
        # save database into the persistent store
        index_path <- store_cmip6_index_path(init = TRUE)
        data.table::fwrite(dt, index_path)
        vmsg(sprintf(
            "Data file index saved to '%s'",
            normalizePath(index_path, winslash = "/", mustWork = FALSE)
        ))

        this$index_db <- data.table::copy(dt)
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
load_cmip6_index <- function(force = FALSE) {
    if (is.null(this$index_db)) {
        force <- TRUE
    }

    if (!force) {
        idx <- data.table::copy(this$index_db)
    } else {
        f <- normalizePath(store_cmip6_index_path(init = FALSE), winslash = "/", mustWork = FALSE)
        if (!file.exists(f)) {
            stop(
                "CMIP6 experiment output file index does not exists. You may want to create one using 'init_cmip6_index()'."
            )
        }

        # nocov start
        # load file info
        idx <- tryCatch(
            data.table::fread(f, colClasses = c("version" = "character", "file_size" = "double")),
            warning = function(w) {
                stop("Failed to parse CMIP6 experiment output file index. ", conditionMessage(w))
            },
            error = function(e) {
                stop("Failed to parse CMIP6 experiment output file index. ", conditionMessage(e))
            }
        )
        # nocov end
        vmsg(sprintf(
            "Loading CMIP6 experiment output file index created at %s.",
            as.character(file.info(f)$mtime)
        ))
    }

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
        file_mtime <- NULL
        if (is.character(idx$file_mtime)) {
            idx[J(""), on = "file_mtime", file_mtime := NA]
        }
        idx[, file_mtime := data.table::setattr(as.POSIXct(file_mtime, origin = "1970-01-01"), "tzone", NULL)]
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
        if (is.character(idx$datetime_start)) {
            idx[J(""), on = "datetime_start", datetime_start := NA]
        }
        data.table::set(idx, NULL, "datetime_start", as.POSIXct(idx$datetime_start, "UTC"))
    }
    if ("datetime_end" %in% names(idx)) {
        # to avoid No visible binding for global variable check NOTE
        datetime_end <- NULL
        if (is.character(idx$datetime_end)) {
            idx[J(""), on = "datetime_end", datetime_end := NA]
        }
        data.table::set(idx, NULL, "datetime_end", as.POSIXct(idx$datetime_end, "UTC"))
    }

    # udpate package internal stored file index
    this$index_db <- data.table::copy(idx)

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
#' @export
set_cmip6_index <- function(index, save = FALSE) {
    checkmate::assert_data_table(index)
    checkmate::assert_subset(
        names(index),
        c(
            "file_id",
            "dataset_id",
            "mip_era",
            "activity_drs",
            "institution_id",
            "source_id",
            "experiment_id",
            "member_id",
            "table_id",
            "frequency",
            "grid_label",
            "version",
            "nominal_resolution",
            "variable_id",
            "variable_long_name",
            "variable_units",
            "datetime_start",
            "datetime_end",
            "file_size",
            "data_node",
            "file_url",
            "dataset_pid",
            "tracking_id",
            "file_path",
            "file_realsize",
            "file_mtime",
            "time_units",
            "time_calendar"
        )
    )

    # save database into the persistent store
    if (save) {
        index_path <- store_cmip6_index_path(init = TRUE)
        data.table::fwrite(index, index_path)
        vmsg(sprintf(
            "Data file index saved to '%s'",
            normalizePath(index_path, winslash = "/", mustWork = FALSE)
        ))
    }

    # udpate package internal stored file index
    this$index_db <- data.table::copy(index)

    invisible(index)
}
# }}}

# data_node_status {{{
#' Get status of data nodes which store CMIP6 output
#'
#' `data_node_status()` is the user-facing replacement for the legacy
#' data-node helper name used in earlier releases.
#'
#' @param speed_test If `TRUE`, use [pingr::ping()] to perform connection speed
#'        test on each data node. A `ping` column is appended in returned
#'        data.table which stores each data node response in milliseconds. This
#'        feature needs pingr package already installed. Default: `FALSE`.
#' @param timeout Timeout for a ping response in seconds. Default: `3`.
#' @param index_node The index node to query the data node status. Default: `INDEX_NODES[["ORNL"]]`.
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
#' \dontrun{
#' data_node_status()
#' }
#'
#' @export
data_node_status <- function(speed_test = FALSE, timeout = 3, index_node = INDEX_NODES[["ORNL"]]) {
    # use the metagrid-backend to get the data node status
    # see: https://github.com/esgf2-us/metagrid/blob/2e90dd10317506a82f120217e39c4a3cde6a7560/backend/.envs/.django#L30
    #      https://github.com/ESGF/esgf-utils/blob/master/node_status/query_prom.py
    path <- "proxy/status"
    parsed <- normalize_index_node(index_node, raw = TRUE)
    if (parsed$path == "/esgf-1-5-bridge") {
        url <- curl::curl_modify_url(parsed$url, path = path)
    } else {
        url <- curl::curl_modify_url(parsed$url, path = paste(parsed$path, path, sep = "/"))
    }

    msg <- NULL
    res <- with_url_cache(
        "datanode",
        url,
        function() {
            tryCatch(
                jsonlite::fromJSON(url),
                warning = function(w) {
                    msg <<- conditionMessage(w)
                    NULL
                },
                error = function(e) {
                    msg <<- conditionMessage(e)
                    NULL
                }
            )
        },
        validate = function(res) !is.null(res)
    )

    # nocov start
    if (is.null(res) || res$status != "success") {
        message("Failed to retrieve the data node status from aims2.llnl.gov. Reason:\n  ", msg)
        return(data.table::data.table())
    }
    # nocov end

    res <- data.table::data.table(
        data_node = res$data$result$metric$instance,
        status = data.table::fifelse(
            vapply(res$data$result$value, .subset2, character(1L), 2L) == "1",
            "UP",
            "DOWN"
        )
    )

    data.table::setorderv(res, "status", -1)

    if (!speed_test) {
        return(res)
    }

    # nocov start
    if (!requireNamespace("pingr", quietly = TRUE)) {
        stop(
            "'epwshiftr' relies on the package 'pingr' to perform speed test",
            "please add this to your library with install.packages('pingr') and try again."
        )
    }
    # nocov end

    # nocov start
    if (!length(nodes_up <- res$data_node[res$status == "UP"])) {
        message("No working data nodes available now. Skip speed test")
        return(res)
    }
    # nocov end

    # use the pingr package to test the connection speed
    speed <- vapply(
        nodes_up,
        function(node) {
            message(sprintf("Testing data node '%s'...", node))
            pingr::ping(node, count = 1, timeout = timeout)
        },
        numeric(1)
    )

    res[status == "UP", ping := speed][order(ping)]
}
# }}}

# parse_file_date {{{
#' @importFrom data.table fifelse
parse_file_date <- function(id, frequency) {
    dig <- fifelse(
        grepl("hr", frequency, fixed = TRUE),
        12L,
        fifelse(
            frequency == "day",
            8L,
            fifelse(
                frequency == "dec",
                4L,
                fifelse(
                    grepl("mon", frequency, fixed = TRUE),
                    6L,
                    fifelse(grepl("yr", frequency, fixed = TRUE), 4L, 0L)
                )
            )
        )
    )

    reg <- sprintf("([0-9]{%i})-([0-9]{%i})", dig, dig)
    reg[dig == 0L] <- NA_character_

    suf <- fifelse(
        dig == 0L,
        "",
        fifelse(
            dig == 4L,
            "0101",
            fifelse(dig == 6L, "01", "")
        )
    )

    fmt <- fifelse(
        dig == 0L,
        NA_character_,
        fifelse(
            dig == 4L,
            "%Y%m%d",
            fifelse(
                dig == 6L,
                "%Y%m%d",
                fifelse(
                    dig == 8L,
                    "%Y%m%d",
                    fifelse(dig == 12L, "%Y%m%d%H%M%s", NA_character_)
                )
            )
        )
    )

    data.table::data.table(id, reg, suf, fmt)[
        !J(NA_character_),
        on = "reg",
        by = "reg",
        c("datetime_start", "datetime_end") := {
            m <- regexpr(.BY$reg, id)
            s <- data.table::tstrsplit(regmatches(id, m), "-", fixed = TRUE)
            lapply(s, function(x) as.POSIXct(paste0(x, suf), format = fmt[1L], tz = "UTC"))
        }
    ][, .SD, .SDcols = c("datetime_start", "datetime_end")]
}
# }}}
