#' @include source-reanalysis.R
NULL

# ERA5 source ---------------------------------------------------------------

# Return the canonical ERA5 variable manifest. Long CDS names define requests,
# short aliases cover both CDS and time-series NetCDF encodings, and product
# flags prevent an ERA5-Land request from inheriting unsupported ERA5 fields.
era5__variable_manifest <- function() {
    data.table::data.table(
        variable_id = c(
            "tas", "tasmin", "tasmax", "tdps", "ps", "psl", "uas", "vas",
            "sfcWind", "rsds", "rlds", "clt", "pr", "hurs", "huss", "snd"
        ),
        era_variable = c(
            "2m_temperature", "2m_temperature", "2m_temperature",
            "2m_dewpoint_temperature", "surface_pressure",
            "mean_sea_level_pressure", "10m_u_component_of_wind",
            "10m_v_component_of_wind", "10m_u_component_of_wind",
            "surface_solar_radiation_downwards",
            "surface_thermal_radiation_downwards", "total_cloud_cover",
            "total_precipitation", "2m_dewpoint_temperature",
            "2m_dewpoint_temperature", "snow_depth"
        ),
        aliases = I(list(
            c("t2m", "2m_temperature"),
            c("t2m", "2m_temperature"),
            c("t2m", "2m_temperature"),
            c("d2m", "2m_dewpoint_temperature"),
            c("sp", "surface_pressure"),
            c("msl", "mean_sea_level_pressure"),
            c("u10", "10m_u_component_of_wind"),
            c("v10", "10m_v_component_of_wind"),
            c("u10", "10m_u_component_of_wind"),
            c("ssrd", "surface_solar_radiation_downwards"),
            c("strd", "surface_thermal_radiation_downwards"),
            c("tcc", "total_cloud_cover"),
            c("tp", "total_precipitation"),
            c("d2m", "2m_dewpoint_temperature"),
            c("d2m", "2m_dewpoint_temperature"),
            c("sd", "snow_depth")
        )),
        units = c(
            "K", "K", "K", "K", "Pa", "Pa", "m s-1", "m s-1",
            "m s-1", "W m-2", "W m-2", "%", "kg m-2 s-1", "%", "1",
            "m"
        ),
        aggregation = c("mean", "min", "max", rep("mean", 13L)),
        single_levels = c(rep(TRUE, 15L), FALSE),
        land = c(
            TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
            TRUE, FALSE, TRUE, TRUE, TRUE, TRUE
        ),
        time_series_single_levels = c(
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            FALSE, TRUE, TRUE, TRUE, FALSE
        ),
        time_series_land = c(
            TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
            FALSE, FALSE, TRUE, TRUE, TRUE, TRUE
        )
    )
}

# Expand supported CF variables to the provider fields needed to calculate
# them, rejecting variables that are unavailable in the selected ERA5 product.
era5__source_variables <- function(variables, product = "single_levels") {
    checkmate::assert_choice(product, c("single_levels", "land"))
    dependencies <- list(
        tas = "tas",
        tasmin = "tas",
        tasmax = "tas",
        tdps = "tdps",
        ps = "ps",
        psl = "psl",
        uas = "uas",
        vas = "vas",
        sfcWind = c("uas", "vas"),
        rsds = "rsds",
        rlds = "rlds",
        clt = "clt",
        pr = "pr",
        hurs = c("tas", "tdps"),
        huss = c("tas", "tdps", "ps"),
        snd = "snd"
    )
    manifest <- era5__variable_manifest()
    supported <- manifest[get(product) %in% TRUE, variable_id]
    unknown <- setdiff(variables, intersect(names(dependencies), supported))
    if (length(unknown)) {
        cli::cli_abort(
            "ERA5 product {.val {product}} does not provide or derive requested CF variable(s): {.val {unknown}}."
        )
    }
    unique(unlist(dependencies[variables], use.names = FALSE))
}

# Select the official dataset entry used by each product and access mode.
era5__dataset_id <- function(product, access) {
    checkmate::assert_choice(product, c("single_levels", "land"))
    checkmate::assert_choice(access, c("arco", "cds"))
    if (identical(product, "land")) {
        if (identical(access, "arco")) {
            "reanalysis-era5-land-timeseries"
        } else {
            "reanalysis-era5-land"
        }
    } else if (identical(access, "arco")) {
        "reanalysis-era5-single-levels-timeseries"
    } else {
        "reanalysis-era5-single-levels"
    }
}

# Prefer the official ARCO-backed point service only when it contains every
# required source field; otherwise use the complete CDS subset service.
era5__resolve_access <- function(spec, variables) {
    if (!identical(spec@access, "auto")) {
        return(spec@access)
    }
    sources <- era5__source_variables(variables, spec@product)
    manifest <- era5__variable_manifest()
    time_series_column <- paste0("time_series_", spec@product)
    available <- manifest[
        variable_id %in% sources,
        all(get(time_series_column))
    ]
    if (isTRUE(available)) "arco" else "cds"
}

# Build the transport request for one ERA5 source variable. Requests include
# one UTC day of padding so conversion to the EPW fixed standard offset cannot
# lose the first or last local calendar day.
era5__request <- function(spec, variable, site, access) {
    manifest <- era5__variable_manifest()
    row <- manifest[variable_id == variable & get(spec@product) %in% TRUE]
    if (nrow(row) != 1L) {
        cli::cli_abort("ERA5 variable mapping is not unique for {.val {variable}}.")
    }
    start <- as.Date(sprintf("%04d-01-01", min(spec@years))) - 1L
    stop <- as.Date(sprintf("%04d-12-31", max(spec@years))) + 1L
    longitude <- reanalysis__longitude(site@lon)
    if (identical(access, "arco")) {
        return(list(
            variable = unname(row$era_variable),
            location = list(longitude = longitude, latitude = site@lat),
            date = sprintf("%s/%s", start, stop),
            data_format = "netcdf"
        ))
    }

    dates <- seq(start, stop, by = "day")
    # The full CDS collection accepts independent year/month/day dimensions,
    # so boundary-day padding necessarily includes both adjacent calendar
    # years. Normalization still retains only the requested local years.
    # A narrow enclosing area retains enough cells for deterministic nearest
    # selection even when the requested location is between grid centres.
    resolution <- if (identical(spec@product, "land")) 0.1 else 0.25
    half_cell <- resolution / 2
    request <- list(
        variable = unname(row$era_variable),
        year = sprintf("%04d", sort(unique(as.integer(format(dates, "%Y"))))),
        month = sprintf("%02d", 1:12),
        day = sprintf("%02d", 1:31),
        time = sprintf("%02d:00", 0:23),
        area = c(
            min(90, site@lat + half_cell),
            max(-180, longitude - half_cell),
            max(-90, site@lat - half_cell),
            min(180, longitude + half_cell)
        ),
        data_format = "netcdf",
        download_format = "unarchived"
    )
    if (identical(spec@product, "single_levels")) {
        request$product_type <- "reanalysis"
    }
    attr(request, "requested_years") <- sort(unique(as.integer(
        format(dates, "%Y")
    )))
    request
}

#' Use ERA5 as an observed weather reference
#'
#' `shift_era5()` describes an ERA5 calibration source. Variables, temporal
#' frequency, and the target coordinate are normally inferred from the weather
#' method and baseline EPW when [shift_future_epw()] builds the run.
#' Constructing the source neither reads credentials nor contacts CDS. Use
#' `shift_check(source)` to validate local configuration and
#' `shift_check(source, network = TRUE)` to authenticate the configured token
#' without submitting a data request. Dataset-specific terms are checked by CDS
#' when retrieval begins and are never accepted by the package.
#'
#' @param years Calendar years used as the observed reference period.
#' @param product ERA5 single-level product. `"single_levels"` uses global
#'   ERA5; `"land"` uses ERA5-Land.
#' @param variables Optional CF variable IDs. `NULL` infers them from the
#'   selected weather method.
#' @param frequency Optional `"hour"`, `"day"`, or `"mon"` frequency. A named
#'   vector can assign frequencies by CF variable. `NULL` infers the contract.
#' @param access Data access route. `"auto"` prefers the official ARCO-backed
#'   time-series service for supported point requests and otherwise uses the
#'   complete CDS subset service.
#' @param ... Named provider options retained in the source specification.
#'
#' @return A reusable `ShiftReanalysisSpec` containing no credentials.
#'
#' @references
#' [Copernicus Climate Data Store ERA5 hourly time-series data](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-timeseries)
#' and the [CDS API setup guide](https://cds.climate.copernicus.eu/en/how-to-api).
#'
#' @seealso [shift_future_epw()], [shift_check()]
#' @export
shift_era5 <- function(
    years,
    product = c("single_levels", "land"),
    variables = NULL,
    frequency = NULL,
    access = c("auto", "arco", "cds"),
    ...
) {
    product <- match.arg(product)
    access <- match.arg(access)
    options <- list(...)
    unknown <- setdiff(names(options), c("timeout", "poll_interval"))
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown ERA5 provider option(s): {.arg {unknown}}."
        )
    }
    reanalysis__spec(
        dataset = "era5",
        years = years,
        product = product,
        variables = variables,
        frequency = frequency,
        access = access,
        options = options
    )
}

#' Describe the future ERA6 source boundary
#'
#' ERA6 production has started, but a stable public dataset endpoint is not yet
#' available. This constructor fails explicitly so ERA5 is never substituted or
#' spliced into a request labelled ERA6.
#'
#' @inheritParams shift_era5
#'
#' @return This function currently raises an unavailable-data error.
#'
#' @references
#' [ECMWF, ERA6 reanalysis production](https://www.ecmwf.int/en/newsletter/188/news/era6-reanalysis-production).
#'
#' @export
shift_era6 <- function(
    years,
    product = "single_levels",
    variables = NULL,
    frequency = NULL,
    access = c("auto", "arco", "cds"),
    ...
) {
    checkmate::assert_choice(product, reanalysis__registry()$era6$products)
    options <- list(...)
    unknown <- setdiff(names(options), c("timeout", "poll_interval"))
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown ERA6 provider option(s): {.arg {unknown}}."
        )
    }
    # Construct and validate the future provider-neutral contract before the
    # availability error so the eventual ERA6 implementation can keep this API.
    candidate <- reanalysis__spec(
        dataset = "era6",
        years = years,
        product = product,
        variables = variables,
        frequency = frequency,
        access = match.arg(access),
        options = options
    )
    invisible(candidate)
    cli::cli_abort(
        c(
            "ERA6 is registered but is not yet available through a stable public data endpoint.",
            "i" = "Use {.fn shift_era5} until an official ERA6 dataset and variable manifest are published."
        ),
        class = "epwshiftr_reanalysis_unavailable"
    )
}
