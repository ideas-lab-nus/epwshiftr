#' @include era5-epw-source.R epw-file.R epw-physics.R
NULL

# Generate an explicit local calendar before selecting source records. ERA5
# instantaneous values are at hour end; accumulations cover the preceding hour.
era_epw__calendar <- function(year, timezone, leap_day) {
    start <- as.POSIXct(sprintf("%d-01-01", year), tz = "UTC")
    end <- as.POSIXct(sprintf("%d-01-01", year + 1L), tz = "UTC")
    local <- seq(start, end - 3600, by = 3600)
    fields <- as.POSIXlt(local, tz = "UTC")
    data.table::data.table(
        year = year,
        month = fields$mon + 1L,
        day = fields$mday,
        hour = fields$hour + 1L,
        minute = 60L,
        utc_time = local + 3600 - timezone * 3600,
        retained = leap_day == "keep" | !(fields$mon == 1L & fields$mday == 29L)
    )
}

# IFS liquid-water saturation pressure (CY41R2, Part IV, equation 7.5).
# ERA5 dew point uses this convention even below freezing.
era_epw__water_pressure <- function(kelvin) {
    611.21 * exp(17.502 * (kelvin - 273.16) / (kelvin - 32.19))
}

# Record every EPW field's source, including missing fields. The source ERA5
# grid and the target site's solar geometry remain separate in the receipt.
era_epw__fields <- function(precipitation) {
    origins <- c(
        year = "local standard calendar",
        month = "local standard calendar",
        day = "local standard calendar",
        hour = "hour ending 1..24",
        minute = "60",
        data_source = "ERA5 reanalysis; see field provenance",
        dry_bulb_temperature = "tas: K to degC",
        dew_point_temperature = "tdps: original liquid-water dew point; K to degC",
        relative_humidity = "100 * IFS water saturation(tdps) / saturation(tas)",
        atmospheric_pressure = "ps: surface pressure in Pa",
        extraterrestrial_horizontal_radiation = "shared solar interval geometry",
        extraterrestrial_direct_normal_radiation = "shared solar interval geometry",
        horizontal_infrared_radiation_intensity_from_sky = "rlds: preceding-hour energy",
        global_horizontal_radiation = "rsds: preceding-hour energy",
        direct_normal_radiation = "fdir / hourly effective solar projection",
        diffuse_horizontal_radiation = "rsds - fdir over the same hour",
        wind_speed = "sqrt(uas^2 + vas^2)",
        wind_direction = "atan2(-uas, -vas): meteorological direction from north",
        total_sky_cover = "clt: fraction rounded to nearest tenth"
    )
    if (precipitation == "total_water_equivalent") {
        origins <- c(
            origins,
            liquid_precip_depth = "pr: total water equivalent; precipitation phase unresolved",
            liquid_precip_rate = "1 hour accumulation interval"
        )
    }
    data.table::data.table(
        field = EPW_FILE_COLUMNS,
        source = data.table::fifelse(
            EPW_FILE_COLUMNS %in% names(origins),
            origins[EPW_FILE_COLUMNS],
            "missing: no ERA5 field supplied"
        ),
        unit = vapply(EPW_FILE_COLUMNS, epw_file_unit, character(1L))
    )
}

# Check the complete requested calendar, including leap hours that may later
# be omitted. Do not repair negative radiation, supersaturation or missing data.
era_epw__annual <- function(input, site, year, leap_day, precipitation) {
    calendar <- era_epw__calendar(year, site$timezone, leap_day)
    index <- match(
        as.numeric(calendar$utc_time),
        as.numeric(input$data$utc_time)
    )
    if (anyNA(index)) {
        cli::cli_abort(
            "Missing {sum(is.na(index))} required UTC hour(s) for local year {year}."
        )
    }
    all <- input$data[index]
    if (any(!is.finite(as.matrix(all[, ERA_EPW_VARIABLES, with = FALSE])))) {
        cli::cli_abort("Required ERA5 hours contain non-finite weather values.")
    }
    if (
        any(all$tdps > all$tas) ||
            any(all$clt < 0 | all$clt > 1) ||
            any(all$pr < 0) ||
            any(all$ps <= 0) ||
            any(
                all$rsds < 0 | all$fdir < 0 | all$rlds < 0 | all$fdir > all$rsds
            )
    ) {
        cli::cli_abort(
            "ERA5 physical inconsistency: check dew point, cloud, precipitation, pressure and radiation."
        )
    }
    omitted <- all[!calendar$retained]
    data <- all[calendar$retained]
    weather <- calendar[
        which(calendar$retained),
        c("year", "month", "day", "hour", "minute"),
        with = FALSE
    ]
    for (field in setdiff(EPW_FILE_COLUMNS, names(weather))) {
        spec <- EPW_FILE_FIELD_SPECS[[field]]
        missing_value <- if (field == "data_source") {
            "?9"
        } else if (field == "present_weather_codes") {
            "999999999"
        } else {
            spec$missing_value
        }
        data.table::set(weather, j = field, value = missing_value)
    }
    data.table::set(
        weather,
        j = "dry_bulb_temperature",
        value = data$tas - 273.15
    )
    data.table::set(
        weather,
        j = "dew_point_temperature",
        value = data$tdps - 273.15
    )
    vapour <- era_epw__water_pressure(data$tdps)
    humidity <- 100 * vapour / era_epw__water_pressure(data$tas)
    data.table::set(weather, j = "relative_humidity", value = humidity)
    data.table::set(weather, j = "atmospheric_pressure", value = data$ps)

    # Integrate the same hour used by ERA5. FDIR is horizontal direct energy.
    # There is no fallback partition or redistribution of beam into diffuse.
    geometry <- solar__epw_interval_geometry(
        weather,
        site$latitude,
        site$longitude,
        site$timezone
    )
    projection <- geometry$effective_solar_projection
    if (any(projection <= 0 & data$fdir > 0)) {
        cli::cli_abort(
            "Positive direct-horizontal radiation in an hour without solar projection."
        )
    }
    dni <- numeric(nrow(data))
    daylight <- projection > 0
    dni[daylight] <- data$fdir[daylight] / projection[daylight]
    if (any(dni > geometry$extraterrestrial_direct_normal_radiation + 1e-6)) {
        cli::cli_abort(
            "Derived DNI exceeds the extraterrestrial interval limit."
        )
    }
    data.table::set(
        weather,
        j = "extraterrestrial_horizontal_radiation",
        value = geometry$extraterrestrial_horizontal_radiation
    )
    data.table::set(
        weather,
        j = "extraterrestrial_direct_normal_radiation",
        value = geometry$extraterrestrial_direct_normal_radiation
    )
    data.table::set(
        weather,
        j = "horizontal_infrared_radiation_intensity_from_sky",
        value = data$rlds
    )
    data.table::set(
        weather,
        j = "global_horizontal_radiation",
        value = data$rsds
    )
    data.table::set(
        weather,
        j = "diffuse_horizontal_radiation",
        value = data$rsds - data$fdir
    )
    data.table::set(weather, j = "direct_normal_radiation", value = dni)
    wind <- epwphys__wind_from_components(data$uas, data$vas)
    data.table::set(weather, j = "wind_speed", value = wind$speed)
    data.table::set(weather, j = "wind_direction", value = wind$direction)
    data.table::set(
        weather,
        j = "total_sky_cover",
        value = as.integer(round(data$clt * 10))
    )
    if (precipitation == "total_water_equivalent") {
        data.table::set(weather, j = "liquid_precip_depth", value = data$pr)
        data.table::set(weather, j = "liquid_precip_rate", value = 1)
    }
    fields <- era_epw__fields(precipitation)
    populated <- fields$field[!startsWith(fields$source, "missing")]
    for (field in intersect(populated, names(EPW_FILE_FIELD_SPECS))) {
        value <- weather[[field]]
        spec <- EPW_FILE_FIELD_SPECS[[field]]
        if (
            any(
                !is.finite(value) |
                    value < spec$minimum |
                    value > spec$maximum |
                    value == spec$missing_value
            )
        ) {
            cli::cli_abort(
                "Derived EPW field {.val {field}} violates its documented range."
            )
        }
    }
    # EnergyPlus uses a different below-freezing saturation relation when it
    # interprets RH. Quantify this difference; do not silently clip/rewrite Td.
    engine_vapour <- humidity /
        100 *
        exp(epwphys__psychro_ln_pws(data$tas - 273.15))
    if (any(vapour >= data$ps | engine_vapour >= data$ps)) {
        cli::cli_abort("Vapour pressure must remain below surface pressure.")
    }
    diagnostics <- data.table::data.table(
        utc_time = data$utc_time,
        source_vapour_pressure_Pa = vapour,
        epw_rh_percent = humidity,
        energyplus_vapour_pressure_Pa = engine_vapour,
        # IFS CY41R2 chapter 12 defines Rd = 287.0597 and Rv = 461.5250.
        # Use their ratio for the ERA5 reference; do not silently substitute
        # the ASHRAE constant or EnergyPlus's legacy ratio used below.
        source_humidity_ratio_kg_kg = (287.0597 / 461.5250) *
            vapour /
            (data$ps - vapour),
        # EnergyPlus 9.6 PsyWFnTdbRhPb uses its legacy molecular ratio,
        # denominator guard and minimum humidity ratio. This is diagnostic
        # only: none of these guards modifies the ERA5 data or EPW humidity.
        energyplus_humidity_ratio_kg_kg = pmax(
            1e-5,
            0.62198 * engine_vapour / pmax(data$ps - engine_vapour, 1000)
        ),
        source_precipitation_water_equivalent_mm = data$pr
    )
    list(
        weather = weather[, EPW_FILE_COLUMNS, with = FALSE],
        diagnostics = diagnostics,
        omitted = omitted,
        fields = era_epw__fields(precipitation)
    )
}

# Write a complete new EPW with explicit missing sentinels, then read it through
# the shared EPW parser before publishing. No historical template is copied.
era_epw__write <- function(annual, site, year, leap_day, start_day, path) {
    days <- c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
    )
    weekday <- if (start_day == "calendar") {
        days[[as.POSIXlt(sprintf("%d-01-01", year), tz = "UTC")$wday + 1L]]
    } else {
        start_day
    }
    header <- c(
        paste(
            c(
                "LOCATION",
                site$label,
                site$state,
                site$country,
                "ERA5",
                "0",
                site$latitude,
                site$longitude,
                site$timezone,
                site$elevation
            ),
            collapse = ","
        ),
        "DESIGN CONDITIONS,0",
        "TYPICAL/EXTREME PERIODS,0",
        "GROUND TEMPERATURES,0",
        paste0(
            "HOLIDAYS/DAYLIGHT SAVINGS,",
            if (leap_day == "keep") "Yes" else "No",
            ",0,0,0"
        ),
        "COMMENTS 1,ERA5 reanalysis annual weather; see accompanying provenance and diagnostics",
        "COMMENTS 2,Water-based RH; low-temperature EnergyPlus humidity interpretation differs; no DST",
        paste("DATA PERIODS,1,1,Data", weekday, "1/1", "12/31", sep = ",")
    )
    temporary <- tempfile("weather-", tmpdir = dirname(path), fileext = ".epw")
    on.exit(unlink(temporary), add = TRUE)
    writeLines(header, temporary, useBytes = TRUE)
    data.table::fwrite(
        annual$weather,
        temporary,
        append = TRUE,
        col.names = FALSE,
        quote = FALSE,
        na = ""
    )
    recovered <- EpwFile$new(temporary)$data()[, EPW_FILE_COLUMNS, with = FALSE]
    if (
        !isTRUE(all.equal(
            as.data.frame(recovered),
            as.data.frame(annual$weather),
            check.attributes = FALSE,
            tolerance = 1e-12
        ))
    ) {
        cli::cli_abort(
            "Reference EPW did not survive a field-for-field readback."
        )
    }
    if (!file.rename(temporary, path)) {
        cli::cli_abort("Cannot publish reference EPW.")
    }
    invisible(path)
}

# Include both the adapter and shared physics in cache identity. Function text
# is stable across sessions, unlike an environment or a data.table pointer.
era_epw__implementation <- function() {
    functions <- c(
        "era_epw__annual",
        "era_epw__calendar",
        "era_epw__fields",
        "era_epw__normalize",
        "era_epw__read_field",
        "era_epw__read_files",
        "era_epw__water_pressure",
        "era_epw__write",
        "solar__epw_interval_geometry",
        "epwphys__psychro_ln_pws",
        "epwphys__wind_from_components",
        "solar__spencer_declination",
        "solar__spencer_equation_of_time",
        "solar__cos_zenith",
        "solar__radians",
        "era_epw__site",
        "epw_file_normalize_weather",
        "reanalysis__longitude"
    )
    env <- environment(era_epw__implementation)
    era_epw__hash(list(
        functions = lapply(functions, function(name) {
            deparse(get(name, envir = env))
        }),
        field_specs = EPW_FILE_FIELD_SPECS,
        columns = EPW_FILE_COLUMNS
    ))
}

# Reuse only a fully published result whose input identity and every artifact
# hash still match. Failed attempts and damaged artifacts remain on disk.
era_epw__reusable <- function(receipt, key) {
    if (
        is.null(receipt) ||
            !identical(receipt$status, "complete") ||
            !identical(receipt$key, key) ||
            !length(receipt$artifacts)
    ) {
        return(FALSE)
    }
    all(vapply(
        receipt$artifacts,
        function(artifact) {
            file.exists(artifact$path) &&
                identical(checksum_file(artifact$path), artifact$sha256)
        },
        logical(1L)
    ))
}

# Persist an isolated attempt before updating the job's current receipt. An
# interrupted write therefore never marks an incomplete EPW as reusable.
era_epw__job <- function(
    input,
    metadata,
    source,
    year,
    options,
    directory,
    resume
) {
    specification <- list(
        site = metadata,
        year = year,
        product = source@product,
        input_sha256 = input$sha256,
        options = options,
        implementation_sha256 = era_epw__implementation()
    )
    key <- era_epw__hash(specification)
    job <- file.path(
        directory,
        "jobs",
        paste0(
            gsub("[^A-Za-z0-9_-]", "_", metadata$id),
            "-",
            year,
            "-",
            key
        )
    )
    dir.create(job, recursive = TRUE, showWarnings = FALSE)
    receipt_path <- file.path(job, "receipt.json")
    previous <- if (file.exists(receipt_path)) {
        jsonlite::read_json(receipt_path)
    } else {
        NULL
    }
    if (resume && era_epw__reusable(previous, key)) {
        return(data.table::data.table(
            site = metadata$id,
            year = year,
            status = "complete",
            reused = TRUE,
            epw = previous$epw,
            receipt = receipt_path,
            error = NA_character_
        ))
    }
    count <- length(list.dirs(job, recursive = FALSE)) + 1L
    attempt <- file.path(job, sprintf("attempt-%03d", count))
    if (!dir.create(attempt)) {
        cli::cli_abort("Cannot create a new reference-weather attempt.")
    }
    started <- Sys.time()
    receipt <- c(
        specification,
        list(
            key = key,
            status = "running",
            started_at = format(started, tz = "UTC", usetz = TRUE),
            provenance = input$provenance,
            grid = input$grid,
            native_units = input$native_units,
            R_version = as.character(getRversion()),
            epwshiftr_version = as.character(utils::packageVersion(
                "epwshiftr"
            )),
            humidity_diagnostic = "EnergyPlus 9.6 PsyWFnTdbRhPb; hourly endpoint before timestep interpolation"
        )
    )
    era_epw__json(receipt, file.path(attempt, "receipt.json"))
    tryCatch(
        {
            annual <- era_epw__annual(
                input,
                metadata,
                year,
                options$leap_day,
                options$precipitation
            )
            path <- file.path(attempt, "weather.epw")
            era_epw__write(
                annual,
                metadata,
                year,
                options$leap_day,
                options$start_day,
                path
            )
            data.table::fwrite(annual$fields, file.path(attempt, "fields.csv"))
            data.table::fwrite(
                annual$diagnostics,
                file.path(attempt, "diagnostics.csv")
            )
            saveRDS(
                annual$omitted,
                file.path(attempt, "omitted-leap-hours.rds"),
                version = 2
            )
            outputs <- file.path(
                attempt,
                c(
                    "weather.epw",
                    "fields.csv",
                    "diagnostics.csv",
                    "omitted-leap-hours.rds"
                )
            )
            receipt$artifacts <- lapply(outputs, function(x) {
                list(path = x, sha256 = checksum_file(x))
            })
            receipt$epw <- path
            receipt$hours <- nrow(annual$weather)
            receipt$omitted_hours <- nrow(annual$omitted)
            receipt$max_humidity_ratio_difference_kg_kg <- max(abs(
                annual$diagnostics$energyplus_humidity_ratio_kg_kg -
                    annual$diagnostics$source_humidity_ratio_kg_kg
            ))
            receipt$status <- "complete"
        },
        error = function(error) {
            receipt$status <<- "failed"
            receipt$error <<- conditionMessage(error)
        }
    )
    receipt$elapsed_seconds <- as.numeric(difftime(
        Sys.time(),
        started,
        units = "secs"
    ))
    era_epw__json(receipt, file.path(attempt, "receipt.json"))
    era_epw__json(receipt, receipt_path)
    data.table::data.table(
        site = metadata$id,
        year = year,
        status = receipt$status,
        reused = FALSE,
        epw = shift_stage__coalesce(receipt[["epw"]], NA_character_),
        receipt = receipt_path,
        error = shift_stage__coalesce(receipt$error, NA_character_)
    )
}

#' Generate annual reference EPW files from ERA5
#'
#' Create independent annual weather files without a baseline EPW or GCM.
#' This first implementation supports hourly ERA5 single-level data and
#' whole-hour standard timezone offsets, without daylight saving time.
#'
#' @param source A [shift_era5()] specification for `product = "single_levels"`.
#'   Leave `variables` and `frequency` unset. Its years select local calendar years.
#' @param sites One [shift_site()] or a list of sites. Each site's `metadata`
#'   must provide `timezone` (standard UTC offset in hours) and `elevation`
#'   (metres). Optional `state` and `country` appear in the EPW header.
#' @param dir Directory for source caches, immutable conversion attempts and receipts.
#' @param data Optional local NetCDF path(s), an explicit data bundle, or a named
#'   list of those inputs keyed by site ID. Local input never contacts CDS.
#'   A bundle contains `data` (a data.frame), `units` (a named character vector),
#'   `grid` (latitude/longitude), `interval_seconds = 3600`, and optional
#'   `provenance`. Its columns are `utc_time` (ordered POSIXct validity times),
#'   `tas`, `tdps`, `ps`, `uas`, `vas`, `rsds`, `fdir`, `rlds`,
#'   `clt`, and `pr`. NetCDF input must already use the hourly ERA5
#'   single-level validity-time convention; forecast-step/ensemble dimensions
#'   must be resolved before calling. No implicit accumulation reset is inferred.
#'   With `NULL`, the existing CDS transport retrieves small monthly requests;
#'   this is not a bulk-download accelerator.
#' @param leap_day Keep February 29 (8784 hours in leap years), or drop it for
#'   a 8760-hour calendar. The omitted source hours remain in an artifact.
#' @param start_day `"calendar"` uses the actual January 1 weekday. Alternatively
#'   supply an English weekday, e.g. `"Monday"`, for a fixed simulation schedule.
#' @param precipitation Default `"missing"` leaves EPW liquid precipitation
#'   missing because ERA5 total precipitation does not resolve its phase.
#'   `"total_water_equivalent"` explicitly writes that total into the liquid
#'   precipitation field, with a provenance warning.
#' @param resume Reuse successful results only when inputs, implementation,
#'   options and all output hashes match.
#'
#' @details
#' Instantaneous variables are assigned at hour end. Radiation and precipitation
#' cover the preceding hour: accumulated radiation in J/m2 is divided by 3600;
#' hourly mean W/m2 and hourly Wh/m2 have the same numeric EPW value.
#' FDIR is direct *horizontal* radiation. Diffuse radiation is SSRD minus FDIR;
#' DNI uses the shared hourly solar projection at the target site's coordinates.
#' Negative energy, inconsistent fields, missing hours and impossible DNI fail
#' that annual job without clipping or substituting weather.
#'
#' Relative humidity uses the ECMWF liquid-water saturation relation, including
#' below freezing. Original dew point is retained. EnergyPlus interprets RH
#' using a different saturation relation below freezing; the accompanying hourly
#' diagnostics quantify the resulting humidity-ratio difference. Thus a complete
#' conversion does not establish moisture equivalence for building simulation.
#' Grid values are not elevation-adjusted to the target site.
#'
#' Unsupported weather fields use documented EPW missing sentinels. Ground
#' temperatures, design conditions, holidays and typical/extreme periods are
#' not fabricated. These and precipitation/snow assumptions must be configured
#' as appropriate in the simulation model. This creates reanalysis annual
#' weather, not observations or a typical meteorological year. Leap-day removal
#' changes calendar continuity; original timestamps and omitted hours remain
#' available. Solar conversion uses each source record's original calendar date.
#'
#' @return A data.table with one row per site/year, status, reuse flag, EPW and
#'   receipt paths, and any error. Partial failures are retained and warned.
#'   `complete` means conversion and EPW readback passed, not building validation.
#' @examples
#' site <- shift_site("example", lon = 113.3, lat = 23.2,
#'     metadata = list(timezone = 8, elevation = 10))
#' \dontrun{
#' shift_epw_reanalysis(shift_era5(1995), site, "reference-weather",
#'     data = c("instantaneous.nc", "accumulated.nc"))
#' }
#' @export
shift_epw_reanalysis <- function(
    source,
    sites,
    dir,
    data = NULL,
    leap_day = c("drop", "keep"),
    start_day = "calendar",
    precipitation = c("missing", "total_water_equivalent"),
    resume = TRUE
) {
    if (
        !S7::S7_inherits(source, ShiftReanalysisSpec) ||
            source@dataset != "era5" ||
            source@product != "single_levels"
    ) {
        cli::cli_abort(
            "Reference EPW requires {.fn shift_era5} with product = 'single_levels'."
        )
    }
    if (length(source@variables) || length(source@frequency)) {
        cli::cli_abort(
            "Leave source variables and frequency unset for reference EPW."
        )
    }
    leap_day <- match.arg(leap_day)
    precipitation <- match.arg(precipitation)
    checkmate::assert_choice(
        start_day,
        c(
            "calendar",
            "Sunday",
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday"
        )
    )
    checkmate::assert_flag(resume)
    checkmate::assert_string(dir, min.chars = 1L)
    if (S7::S7_inherits(sites, ShiftSite)) {
        sites <- list(sites)
    }
    checkmate::assert_list(sites, min.len = 1L)
    metadata <- lapply(sites, era_epw__site)
    ids <- vapply(metadata, "[[", character(1L), "id")
    if (anyDuplicated(ids)) {
        cli::cli_abort("Reference sites must have unique IDs.")
    }
    is_bundle <- is.list(data) && is.data.frame(data$data)
    if (
        !is.null(data) &&
            !is.character(data) &&
            !is_bundle &&
            (!is.list(data) || !all(ids %in% names(data)))
    ) {
        cli::cli_abort(
            "Local inputs must be file paths, a bundle, or a list keyed by site ID."
        )
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    directory <- normalizePath(dir, winslash = "/", mustWork = TRUE)
    lock <- file.path(directory, ".reference-lock")
    if (!dir.create(lock, showWarnings = FALSE)) {
        cli::cli_abort(
            "Reference directory is locked; inspect its owner before recovery: {.path {lock}}."
        )
    }
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
    era_epw__json(
        list(
            pid = Sys.getpid(),
            started_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
        ),
        file.path(lock, "owner.json")
    )
    options <- list(
        leap_day = leap_day,
        start_day = start_day,
        precipitation = precipitation
    )
    rows <- vector("list", length(sites) * length(source@years))
    row_index <- 0L
    for (i in seq_along(sites)) {
        selected <- if (is.null(data)) {
            NULL
        } else if (is.character(data) || is_bundle) {
            data
        } else {
            data[[ids[[i]]]]
        }
        input <- tryCatch(
            {
                if (is.null(selected) && !is.null(data)) {
                    cli::cli_abort(
                        "Local input is missing for site {.val {ids[[i]]}}."
                    )
                }
                if (is.null(selected)) {
                    selected <- era_epw__download(
                        source,
                        sites[[i]],
                        file.path(directory, "source")
                    )
                }
                if (is.character(selected)) {
                    selected <- era_epw__read_files(selected, sites[[i]])
                }
                era_epw__normalize(selected)
            },
            error = identity
        )
        for (year in source@years) {
            if (inherits(input, "error")) {
                failure <- list(
                    site = ids[[i]],
                    year = year,
                    status = "failed",
                    error = conditionMessage(input),
                    stage = "source",
                    time = format(Sys.time(), tz = "UTC", usetz = TRUE)
                )
                receipt <- tempfile(
                    "source-failure-",
                    tmpdir = directory,
                    fileext = ".json"
                )
                era_epw__json(failure, receipt)
                row <- data.table::data.table(
                    site = ids[[i]],
                    year = year,
                    status = "failed",
                    reused = FALSE,
                    epw = NA_character_,
                    receipt = receipt,
                    error = failure$error
                )
            } else {
                row <- era_epw__job(
                    input,
                    metadata[[i]],
                    source,
                    year,
                    options,
                    directory,
                    resume
                )
            }
            row_index <- row_index + 1L
            rows[[row_index]] <- row
        }
    }
    result <- data.table::rbindlist(rows)
    data.table::fwrite(result, file.path(directory, "last-run.csv"))
    if (any(result$status != "complete")) {
        cli::cli_warn(
            "{sum(result$status != 'complete')} reference EPW job(s) failed; inspect returned errors and receipts."
        )
    }
    result[]
}
