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
    extraterrestrial_horizontal_radiation = "Wh/m^2",
    extraterrestrial_direct_normal_radiation = "Wh/m^2",
    horizontal_infrared_radiation_intensity_from_sky = "Wh/m^2",
    global_horizontal_radiation = "Wh/m^2",
    direct_normal_radiation = "Wh/m^2",
    diffuse_horizontal_radiation = "Wh/m^2",
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

# Principal EPW limits and missing sentinels are defined once so validation,
# physical closure, and file serialization cannot silently diverge.
EPW_FILE_FIELD_SPECS <- list(
    dry_bulb_temperature = c(-70, 70, 99.9),
    dew_point_temperature = c(-70, 70, 99.9),
    relative_humidity = c(0, 110, 999),
    atmospheric_pressure = c(31000, 120000, 999999),
    horizontal_infrared_radiation_intensity_from_sky = c(0, Inf, 9999),
    global_horizontal_radiation = c(0, Inf, 9999),
    direct_normal_radiation = c(0, Inf, 9999),
    diffuse_horizontal_radiation = c(0, Inf, 9999),
    global_horizontal_illuminance = c(0, Inf, 999999),
    direct_normal_illuminance = c(0, Inf, 999999),
    diffuse_horizontal_illuminance = c(0, Inf, 999999),
    zenith_luminance = c(0, Inf, 9999),
    wind_direction = c(0, 360, 999),
    wind_speed = c(0, 40, 999),
    total_sky_cover = c(0, 10, 99),
    opaque_sky_cover = c(0, 10, 99),
    liquid_precip_depth = c(0, Inf, 999),
    liquid_precip_rate = c(0, Inf, 99),
    snow_depth = c(0, Inf, 999)
)

EPW_FILE_HEADER_NAMES <- c(
    "LOCATION",
    "DESIGN CONDITIONS",
    "TYPICAL/EXTREME PERIODS",
    "GROUND TEMPERATURES",
    "HOLIDAYS/DAYLIGHT SAVINGS",
    "COMMENTS 1",
    "COMMENTS 2",
    "DATA PERIODS"
)

# Split a comma record while preserving a final empty field, which base
# strsplit() otherwise discards. Exact untouched text is retained separately
# for field-for-field legacy header round trips independent of platform EOLs.
epw_file__split_header <- function(line) {
    marker <- "\u001f"
    fields <- strsplit(paste0(line, marker), ",", fixed = TRUE)[[1L]]
    fields[[length(fields)]] <- sub(paste0(marker, "$"), "",
        fields[[length(fields)]])
    fields
}

# Normalize a caller-supplied record name against the fixed eight-line EPW
# header sequence.
epw_file__header_name <- function(name) {
    checkmate::assert_string(name, min.chars = 1L)
    key <- toupper(trimws(name))
    if (!key %in% EPW_FILE_HEADER_NAMES) {
        cli::cli_abort("Unknown EPW header record: {.val {name}}.")
    }
    key
}

# Parse all eight records and reject reordered, duplicated, or malformed
# headers. Strict ordering prevents a changed count field from shifting the
# interpretation of later records.
epw_file__parse_headers <- function(lines, path = NULL) {
    if (length(lines) != length(EPW_FILE_HEADER_NAMES)) {
        cli::cli_abort("An EPW file must contain exactly eight header records before hourly data.")
    }
    records <- lapply(lines, function(line) {
        fields <- epw_file__split_header(line)
        if (!length(fields) || !nzchar(trimws(fields[[1L]]))) {
            cli::cli_abort("An EPW header record has an empty name.")
        }
        list(
            name = toupper(trimws(fields[[1L]])),
            label = fields[[1L]],
            fields = fields[-1L],
            raw = line,
            dirty = FALSE
        )
    })
    names_found <- vapply(records, `[[`, character(1L), "name")
    if (!identical(names_found, EPW_FILE_HEADER_NAMES)) {
        location <- if (is.null(path)) "the input" else path
        cli::cli_abort(c(
            "Invalid or reordered EPW header in {.path {location}}.",
            "i" = "Expected: {.val {EPW_FILE_HEADER_NAMES}}.",
            "x" = "Found: {.val {names_found}}."
        ))
    }
    records
}

# Serialize one structured record. Untouched records use their original raw
# bytes for the legacy profile; changed records use canonical names and reject
# commas or line breaks only where they would corrupt field boundaries.
epw_file__serialize_header <- function(record) {
    if (!isTRUE(record$dirty) && length(record$raw) == 1L &&
        !is.na(record$raw)) {
        return(record$raw)
    }
    name <- epw_file__header_name(record$name)
    fields <- as.character(record$fields)
    if (anyNA(fields) || any(grepl("[\r\n]", fields))) {
        cli::cli_abort("EPW header fields cannot be missing or contain line breaks.")
    }
    paste(c(name, fields), collapse = ",")
}

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
            header <- epw_file__parse_headers(
                readLines(path, n = 8L, warn = FALSE), path = path
            )
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
            private$header_records <- header
            private$weather <- epw_file_normalize_weather(weather)
            invisible(self)
        },

        # Return the current source or saved path.
        path = function() {
            private$source_path
        },

        # Parse the LOCATION record into stable site metadata.
        location = function() {
            epw_file_location(epw_file__serialize_header(
                private$header_records[[1L]]
            ))
        },

        # Return a deep copy of all eight structured header records so callers
        # can inspect fields without mutating the EPW through list semantics.
        headers = function() {
            unserialize(serialize(private$header_records, NULL))
        },

        # Read or replace one structured header record. Replacement values are
        # fields after the record name; serialization validates line safety and
        # emits the canonical EPW record label.
        header = function(name, value = NULL) {
            key <- epw_file__header_name(name)
            index <- match(key, EPW_FILE_HEADER_NAMES)
            if (is.null(value)) {
                return(private$header_records[[index]]$fields)
            }
            value <- as.character(value)
            if (anyNA(value) || any(grepl("[\r\n]", value))) {
                cli::cli_abort("EPW header fields cannot be missing or contain line breaks.")
            }
            private$header_records[[index]] <- list(
                name = key,
                label = key,
                fields = value,
                raw = NA_character_,
                dirty = TRUE
            )
            invisible(self)
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
                return(paste(private$header_records[[6L]]$fields, collapse = ","))
            }
            checkmate::assert_string(value)
            self$header("COMMENTS 1", gsub("[\r\n]+", " ", value))
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
            writeLines(vapply(private$header_records, epw_file__serialize_header,
                character(1L)), temp, useBytes = TRUE)
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
        header_records = NULL,
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

# Format recalculated header numbers compactly and deterministically. EPW header
# fields are decimal text rather than fixed-width records.
epw_file__header_number <- function(value, digits = 2L) {
    out <- formatC(as.numeric(value), format = "f", digits = digits)
    out <- sub("0+$", "", out)
    out <- sub("\\.$", "", out)
    out[out %in% c("-0", "")] <- "0"
    out
}

# Parse existing ground-temperature groups. Each depth owns four soil fields
# followed by twelve monthly temperatures; malformed groups are discarded so
# the documented default soil can be generated safely.
epw_file__ground_properties <- function(fields) {
    fields <- as.character(fields)
    count <- suppressWarnings(as.integer(fields[[1L]]))
    width <- 16L
    if (is.na(count) || count <= 0L || length(fields) < 1L + count * width) {
        return(data.table::data.table())
    }
    rows <- lapply(seq_len(count), function(i) {
        start <- 2L + (i - 1L) * width
        values <- suppressWarnings(as.numeric(fields[start:(start + 3L)]))
        if (any(!is.finite(values)) || values[[1L]] < 0 ||
            any(values[-1L] <= 0)) {
            return(NULL)
        }
        data.table::data.table(
            depth = values[[1L]],
            conductivity = values[[2L]],
            density = values[[3L]],
            specific_heat = values[[4L]]
        )
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) data.table::data.table() else
        data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Calculate Kusuda-Achenbach monthly soil temperatures. The implemented
# equation is T(z,t) = Tmean - A exp(-z d) cos(2 pi (t-tmin)/P - z d), where
# d = sqrt(pi/(alpha P)); alpha is converted from m2/s to m2/day and P is one
# year. This produces the expected exponential damping and phase delay.
epw_file__ground_temperatures <- function(weather, properties) {
    weather <- data.table::as.data.table(weather)
    temperature <- suppressWarnings(as.numeric(
        weather$dry_bulb_temperature
    ))
    valid <- is.finite(temperature) & temperature >= -70 & temperature <= 70
    monthly <- data.table::data.table(
        month = as.integer(weather$month[valid]),
        temperature = temperature[valid]
    )[, .(temperature = mean(temperature)), by = month]
    if (!all(seq_len(12L) %in% monthly$month)) {
        return(NULL)
    }
    data.table::setorder(monthly, month)
    annual_mean <- mean(temperature[valid])
    amplitude <- (max(monthly$temperature) - min(monthly$temperature)) / 2
    month_midpoint <- cumsum(c(0, c(31, 28, 31, 30, 31, 30, 31,
        31, 30, 31, 30))) + c(31, 28, 31, 30, 31, 30, 31, 31,
        30, 31, 30, 31) / 2
    t_min <- month_midpoint[[which.min(monthly$temperature)]]
    period_days <- 365
    output <- vector("list", nrow(properties))
    for (i in seq_len(nrow(properties))) {
        property <- properties[i]
        alpha_day <- property$conductivity[[1L]] /
            (property$density[[1L]] * property$specific_heat[[1L]]) * 86400
        if (!is.finite(alpha_day) || alpha_day <= 0) {
            return(NULL)
        }
        damping <- sqrt(pi / (alpha_day * period_days))
        depth <- property$depth[[1L]]
        phase <- 2 * pi * (month_midpoint - t_min) / period_days -
            depth * damping
        soil <- annual_mean - amplitude * exp(-depth * damping) * cos(phase)
        output[[i]] <- c(
            epw_file__header_number(depth, 2L),
            epw_file__header_number(property$conductivity[[1L]], 3L),
            epw_file__header_number(property$density[[1L]], 1L),
            epw_file__header_number(property$specific_heat[[1L]], 1L),
            epw_file__header_number(soil, 2L)
        )
    }
    c(as.character(nrow(properties)), unlist(output, use.names = FALSE))
}

# Convert hourly temperatures to one cyclic calendar year for complete rolling
# seven-day selection. Invalid EPW sentinel temperatures are excluded.
epw_file__daily_temperatures <- function(weather) {
    weather <- data.table::as.data.table(weather)
    value <- suppressWarnings(as.numeric(weather$dry_bulb_temperature))
    valid <- is.finite(value) & value >= -70 & value <= 70
    daily <- data.table::data.table(
        month = as.integer(weather$month[valid]),
        day = as.integer(weather$day[valid]),
        value = value[valid]
    )[, .(temperature = mean(value)), by = .(month, day)]
    leap <- any(daily$month == 2L & daily$day == 29L)
    year <- if (leap) 2000L else 2001L
    daily[, date := as.Date(sprintf("%04d-%02d-%02d", year, month, day))]
    daily <- daily[!is.na(date)]
    data.table::setorder(daily, date)
    expected <- if (leap) 366L else 365L
    if (nrow(daily) != expected ||
        any(diff(as.integer(daily$date)) != 1L)) {
        return(data.table::data.table())
    }
    daily[, index := seq_len(.N)]
    daily[]
}

# Define meteorological seasons by hemisphere. Equatorial locations use the
# northern ordering only as a deterministic header convention.
epw_file__season_months <- function(latitude) {
    if (is.finite(latitude) && latitude < 0) {
        list(
            spring = 9:11,
            summer = c(12L, 1L, 2L),
            autumn = 3:5,
            winter = 6:8
        )
    } else {
        list(
            spring = 3:5,
            summer = 6:8,
            autumn = 9:11,
            winter = c(12L, 1L, 2L)
        )
    }
}

# Select one full rolling seven-day window. Extreme summer/winter windows use
# maximum/minimum weekly mean temperature; typical windows minimize absolute
# distance to the corresponding seasonal daily mean. Ties use the earliest
# calendar start index.
epw_file__select_week <- function(daily, months,
                                   kind = c("typical", "maximum", "minimum")) {
    kind <- match.arg(kind)
    count <- nrow(daily)
    extended <- data.table::rbindlist(list(
        daily,
        data.table::copy(daily[seq_len(6L)])
    ))
    rows <- lapply(seq_len(count), function(start) {
        window <- extended[start:(start + 6L)]
        if (!all(window$month %in% months)) {
            return(NULL)
        }
        data.table::data.table(
            start = start,
            stop = ((start + 5L) %% count) + 1L,
            temperature = mean(window$temperature)
        )
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) {
        return(NULL)
    }
    weeks <- data.table::rbindlist(rows)
    if (identical(kind, "maximum")) {
        data.table::setorder(weeks, -temperature, start)
    } else if (identical(kind, "minimum")) {
        data.table::setorder(weeks, temperature, start)
    } else {
        target <- mean(daily[month %in% months]$temperature)
        weeks[, distance := abs(temperature - target)]
        data.table::setorder(weeks, distance, start)
    }
    weeks[1L]
}

# Generate the six EPW typical/extreme period groups from complete rolling
# windows, using hemisphere-aware meteorological seasons.
epw_file__typical_extreme_periods <- function(weather, latitude) {
    daily <- epw_file__daily_temperatures(weather)
    if (!nrow(daily)) {
        return(NULL)
    }
    seasons <- epw_file__season_months(latitude)
    definitions <- list(
        list("Summer - Extreme Week", "Extreme", "summer", "maximum"),
        list("Summer - Typical Week", "Typical", "summer", "typical"),
        list("Winter - Extreme Week", "Extreme", "winter", "minimum"),
        list("Winter - Typical Week", "Typical", "winter", "typical"),
        list("Spring - Typical Week", "Typical", "spring", "typical"),
        list("Autumn - Typical Week", "Typical", "autumn", "typical")
    )
    fields <- list("6")
    for (definition in definitions) {
        week <- epw_file__select_week(
            daily, seasons[[definition[[3L]]]], definition[[4L]]
        )
        if (is.null(week)) {
            return(NULL)
        }
        start <- daily[week$start[[1L]]]
        stop <- daily[week$stop[[1L]]]
        fields <- c(fields, list(
            definition[[1L]],
            definition[[2L]],
            sprintf("%d/%d", start$month[[1L]], start$day[[1L]]),
            sprintf("%d/%d", stop$month[[1L]], stop$day[[1L]])
        ))
    }
    unlist(fields, use.names = FALSE)
}

# Apply the profile's three header policies to one persisted morphed hourly
# result. Recalculation happens immediately before final EPW serialization, so
# resumed writes derive identical headers from the durable Parquet artifact.
epw_file__apply_morph_headers <- function(epw, weather, options) {
    if (!inherits(epw, "EpwFile") || is.null(options)) {
        return(invisible(epw))
    }
    if (identical(options$design_conditions, "drop")) {
        epw$header("DESIGN CONDITIONS", "0")
    }
    if (identical(options$ground_temperatures, "recalculate")) {
        properties <- epw_file__ground_properties(
            epw$header("GROUND TEMPERATURES")
        )
        if (!nrow(properties)) {
            properties <- data.table::data.table(
                depth = c(0.5, 2, 4),
                conductivity = 1.08,
                density = 962,
                specific_heat = 2576
            )
        }
        fields <- epw_file__ground_temperatures(weather, properties)
        if (!is.null(fields)) {
            epw$header("GROUND TEMPERATURES", fields)
        }
    }
    if (identical(options$typical_extreme_periods, "recalculate")) {
        latitude <- suppressWarnings(as.numeric(epw$location()$latitude))
        fields <- epw_file__typical_extreme_periods(weather, latitude)
        if (!is.null(fields)) {
            epw$header("TYPICAL/EXTREME PERIODS", fields)
        }
    }
    invisible(epw)
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
    for (field in intersect(names(EPW_FILE_FIELD_SPECS), names(weather))) {
        spec <- EPW_FILE_FIELD_SPECS[[field]]
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
