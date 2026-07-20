EPW_FILE_COLUMNS <- c(
    "year", "month", "day", "hour", "minute", "data_source",
    "dry_bulb_temperature", "dew_point_temperature", "relative_humidity",
    "atmospheric_pressure", "extraterrestrial_horizontal_radiation",
    "extraterrestrial_direct_normal_radiation",
    "horizontal_infrared_radiation_intensity_from_sky",
    "global_horizontal_radiation", "direct_normal_radiation",
    "diffuse_horizontal_radiation", "global_horizontal_illuminance",
    "direct_normal_illuminance", "diffuse_horizontal_illuminance",
    "zenith_luminance", "wind_direction", "wind_speed", "total_sky_cover",
    "opaque_sky_cover", "visibility", "ceiling_height",
    "present_weather_observation", "present_weather_codes",
    "precipitable_water", "aerosol_optical_depth", "snow_depth",
    "days_since_last_snow", "albedo", "liquid_precip_depth",
    "liquid_precip_rate"
)

EPW_FILE_UNITS <- c(
    dry_bulb_temperature = "degC",
    dew_point_temperature = "degC",
    relative_humidity = "%",
    atmospheric_pressure = "Pa",
    extraterrestrial_horizontal_radiation = "W/m^2",
    extraterrestrial_direct_normal_radiation = "W/m^2",
    horizontal_infrared_radiation_intensity_from_sky = "W/m^2",
    global_horizontal_radiation = "W/m^2",
    direct_normal_radiation = "W/m^2",
    diffuse_horizontal_radiation = "W/m^2",
    global_horizontal_illuminance = "lx",
    direct_normal_illuminance = "lx",
    diffuse_horizontal_illuminance = "lx",
    zenith_luminance = "cd/m^2",
    wind_direction = "degree",
    wind_speed = "m/s",
    visibility = "km",
    ceiling_height = "m",
    precipitable_water = "mm",
    aerosol_optical_depth = "thousandths",
    snow_depth = "cm",
    days_since_last_snow = "d",
    liquid_precip_depth = "mm",
    liquid_precip_rate = "h"
)

# Parse and write the small, fixed EPW text format without depending on a full
# EnergyPlus object model. The methods intentionally cover only operations used
# by the morphing workflow.
EpwFile <- R6::R6Class(
    "EpwFile",
    public = list(
        # Read one EPW header and its fixed 35-column hourly data section.
        initialize = function(path) {
            checkmate::assert_file_exists(path)
            path <- normalizePath(path, winslash = "/", mustWork = TRUE)
            header <- readLines(path, n = 8L, warn = FALSE)
            if (length(header) != 8L || !startsWith(toupper(header[[1L]]), "LOCATION,")) {
                cli::cli_abort("Invalid EPW header in {.path {path}}.")
            }
            weather <- data.table::fread(
                path,
                skip = 8L,
                header = FALSE,
                colClasses = list(character = c(6L, 28L)),
                showProgress = FALSE
            )
            if (ncol(weather) != length(EPW_FILE_COLUMNS) || !nrow(weather)) {
                cli::cli_abort(
                    "EPW data in {.path {path}} must contain {length(EPW_FILE_COLUMNS)} columns and at least one row."
                )
            }
            data.table::setnames(weather, EPW_FILE_COLUMNS)
            private$source_path <- path
            private$header <- header
            private$weather <- epw_file_normalize_weather(weather)
            invisible(self)
        },

        # Return the current source or saved path.
        path = function() {
            private$source_path
        },

        # Parse the LOCATION record into stable site metadata.
        location = function() {
            epw_file_location(private$header[[1L]])
        },

        # Return a copy so data.table reference semantics cannot mutate the EPW
        # outside the explicit set() method.
        data = function() {
            weather <- data.table::copy(private$weather)
            weather[, datetime := epw_file_datetime(year, month, day, hour)]
            data.table::setcolorder(weather, c("datetime", EPW_FILE_COLUMNS))
            weather[]
        },

        # Unit attachment was an implementation detail of the previous object;
        # units now live in explicit metadata and calculations stay numeric.
        add_unit = function() {
            invisible(self)
        },

        # Numeric EPW data need no class-stripping operation.
        drop_unit = function() {
            invisible(self)
        },

        # Replace hourly data while preserving the original EPW header.
        set = function(data) {
            data <- data.table::as.data.table(data.table::copy(data))
            missing <- setdiff(EPW_FILE_COLUMNS, names(data))
            if (length(missing)) {
                cli::cli_abort("Replacement EPW data are missing field(s): {.field {missing}}.")
            }
            if (nrow(data) != nrow(private$weather)) {
                cli::cli_abort("Replacement EPW data must have {nrow(private$weather)} rows, not {nrow(data)}.")
            }
            private$weather <- epw_file_normalize_weather(data[, ..EPW_FILE_COLUMNS])
            invisible(self)
        },

        # Read or replace COMMENTS 1 without touching other header records.
        comment1 = function(value = NULL) {
            if (is.null(value)) {
                return(sub("^[^,]*,", "", private$header[[6L]]))
            }
            checkmate::assert_string(value)
            private$header[[6L]] <- paste("COMMENTS 1", gsub("[\r\n]+", " ", value), sep = ",")
            invisible(self)
        },

        # Replace missing or invalid morph results with EPW missing-value
        # sentinels so a malformed numeric field is never written silently.
        fill_abnormal = function(missing = TRUE, out_of_range = TRUE, special = TRUE) {
            checkmate::assert_flag(missing)
            checkmate::assert_flag(out_of_range)
            checkmate::assert_flag(special)
            private$weather <- epw_file_fill_abnormal(
                private$weather,
                missing = missing,
                out_of_range = out_of_range,
                special = special
            )
            invisible(self)
        },

        # Save the preserved header and fixed data columns through a temporary
        # file so interrupted writes do not leave a partial EPW at the target.
        save = function(path, overwrite = FALSE) {
            checkmate::assert_string(path, min.chars = 1L)
            checkmate::assert_flag(overwrite)
            path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
            if (file.exists(path) && !isTRUE(overwrite)) {
                cli::cli_abort("EPW output already exists: {.path {path}}.")
            }
            dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
            temp <- tempfile("epw-write-", tmpdir = dirname(path), fileext = ".epw")
            on.exit(unlink(temp), add = TRUE)
            writeLines(private$header, temp, useBytes = TRUE)
            data.table::fwrite(
                private$weather[, ..EPW_FILE_COLUMNS],
                temp,
                append = TRUE,
                col.names = FALSE,
                quote = FALSE,
                na = ""
            )
            if (file.exists(path)) {
                unlink(path)
            }
            if (!file.rename(temp, path)) {
                cli::cli_abort("Failed to save EPW output to {.path {path}}.")
            }
            private$source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
            invisible(self)
        }
    ),
    private = list(
        source_path = NULL,
        header = NULL,
        weather = NULL
    )
)

# Create the internal EPW representation used throughout the package.
epw_file_read <- function(path) {
    EpwFile$new(path)
}

# Identify an eplusr-compatible EPW object by its stable public class contract
# without importing, suggesting, or loading the eplusr package.
epw_file_is_external <- function(epw) {
    inherits(epw, "Epw") && !inherits(epw, "EpwFile")
}

# Convert EPW object inputs into the package's internal representation. External
# Epw objects are deep-cloned before saving so conversion never changes the
# caller's object path or unsaved state.
epw_file_coerce <- function(epw, dir = NULL) {
    if (!inherits(epw, "EpwFile") && !epw_file_is_external(epw)) {
        cli::cli_abort("`epw` must be an EPW path or an object inheriting from {.cls Epw} or {.cls EpwFile}.")
    }

    source <- if (inherits(epw, "EpwFile")) {
        epw$path()
    } else {
        snapshot <- tryCatch(
            epw$clone(deep = TRUE),
            error = function(e) {
                cli::cli_abort(
                    "An external {.cls Epw} object must provide `$clone(deep = TRUE)`.",
                    parent = e
                )
            }
        )
        path <- tempfile("epwshiftr-external-epw-", fileext = ".epw")
        tryCatch(
            snapshot$save(path = path, overwrite = TRUE),
            error = function(e) {
                cli::cli_abort(
                    "Failed to save the external {.cls Epw} object as an internal EPW snapshot.",
                    parent = e
                )
            }
        )
        if (!file.exists(path)) {
            cli::cli_abort("The external {.cls Epw} object's `$save()` method did not create an EPW file.")
        }
        path
    }
    checkmate::assert_file_exists(source)

    if (!is.null(dir)) {
        checkmate::assert_string(dir, min.chars = 1L)
        dir <- normalizePath(path.expand(dir), winslash = "/", mustWork = FALSE)
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        checksum <- store_hash_file(source, "sha256")
        target <- file.path(dir, sprintf("epw-%s.epw", substr(checksum, 1L, 16L)))
        if (!identical(
            normalizePath(source, winslash = "/", mustWork = TRUE),
            normalizePath(target, winslash = "/", mustWork = FALSE)
        ) && !file.exists(target)) {
            if (!isTRUE(file.copy(source, target, overwrite = FALSE))) {
                cli::cli_abort("Failed to persist the EPW object snapshot in {.path {dir}}.")
            }
        }
        source <- target
    }

    epw_file_read(source)
}

# Normalize column storage types after reads and replacements.
epw_file_normalize_weather <- function(weather) {
    weather <- data.table::as.data.table(data.table::copy(weather))
    integer_fields <- c(
        "year", "month", "day", "hour", "minute", "total_sky_cover",
        "opaque_sky_cover", "present_weather_observation", "days_since_last_snow"
    )
    character_fields <- c("data_source", "present_weather_codes")
    for (field in intersect(integer_fields, names(weather))) {
        data.table::set(weather, j = field, value = as.integer(weather[[field]]))
    }
    for (field in intersect(character_fields, names(weather))) {
        data.table::set(weather, j = field, value = as.character(weather[[field]]))
    }
    numeric_fields <- setdiff(EPW_FILE_COLUMNS, c(integer_fields, character_fields))
    for (field in intersect(numeric_fields, names(weather))) {
        data.table::set(weather, j = field, value = as.numeric(weather[[field]]))
    }
    weather[]
}

# Construct end-of-hour timestamps from EPW date columns.
epw_file_datetime <- function(year, month, day, hour) {
    safe_year <- as.integer(year)
    safe_year[is.na(safe_year) | safe_year < 1600L | safe_year > 9999L] <- 2001L
    start <- as.POSIXct(
        sprintf("%04d-%02d-%02d 00:00:00", safe_year, as.integer(month), as.integer(day)),
        tz = "UTC"
    )
    start + as.numeric(hour) * 3600
}

# Decode the nine values in the EPW LOCATION header.
epw_file_location <- function(line) {
    fields <- strsplit(line, ",", fixed = TRUE)[[1L]]
    if (length(fields) < 10L) {
        cli::cli_abort("The EPW LOCATION record must contain nine values.")
    }
    list(
        city = fields[[2L]],
        state_province = fields[[3L]],
        country = fields[[4L]],
        data_source = fields[[5L]],
        wmo_number = fields[[6L]],
        latitude = suppressWarnings(as.numeric(fields[[7L]])),
        longitude = suppressWarnings(as.numeric(fields[[8L]])),
        time_zone = suppressWarnings(as.numeric(fields[[9L]])),
        elevation = suppressWarnings(as.numeric(fields[[10L]]))
    )
}

# Return the explicit physical unit associated with an EPW weather field.
epw_file_unit <- function(field) {
    value <- unname(EPW_FILE_UNITS[field])
    if (is.null(value) || !length(value) || is.na(value)) NA_character_ else value
}

# Apply the EPW missing sentinels and principal physical bounds used by fields
# that morphing can change.
epw_file_fill_abnormal <- function(weather, missing = TRUE, out_of_range = TRUE, special = TRUE) {
    weather <- data.table::as.data.table(data.table::copy(weather))
    specs <- list(
        dry_bulb_temperature = c(-70, 70, 99.9),
        dew_point_temperature = c(-70, 70, 99.9),
        relative_humidity = c(0, 110, 999),
        atmospheric_pressure = c(31000, 120000, 999999),
        horizontal_infrared_radiation_intensity_from_sky = c(0, Inf, 9999),
        global_horizontal_radiation = c(0, Inf, 9999),
        direct_normal_radiation = c(0, Inf, 9999),
        diffuse_horizontal_radiation = c(0, Inf, 9999),
        wind_direction = c(0, 360, 999),
        wind_speed = c(0, 40, 999),
        total_sky_cover = c(0, 10, 99),
        opaque_sky_cover = c(0, 10, 99),
        liquid_precip_depth = c(0, Inf, 999),
        liquid_precip_rate = c(0, Inf, 99)
    )
    for (field in intersect(names(specs), names(weather))) {
        spec <- specs[[field]]
        value <- suppressWarnings(as.numeric(weather[[field]]))
        invalid <- if (isTRUE(missing) || isTRUE(special)) !is.finite(value) else rep(FALSE, length(value))
        if (isTRUE(out_of_range)) {
            invalid <- invalid | value < spec[[1L]] | value > spec[[2L]]
        }
        value[invalid] <- spec[[3L]]
        if (field %in% c("total_sky_cover", "opaque_sky_cover")) {
            value <- as.integer(round(value))
        }
        data.table::set(weather, j = field, value = value)
    }
    weather[]
}
