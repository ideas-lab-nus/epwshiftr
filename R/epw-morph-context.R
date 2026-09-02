#' @include epw-file.R weather-input.R epw-physics.R
NULL

# EPW morphing execution context {{{

morpher__units_label <- function(x) {
    out <- attr(x, "epw_unit", exact = TRUE)
    if (length(out) != 1L || is.na(out) || !nzchar(out)) NA_character_ else out
}

morpher__drop_units <- function(x) {
    as.numeric(x)
}

morpher__unit_alias <- function(x) {
    x <- store__chr1(x)
    if (is.na(x) || !nzchar(x)) {
        return(NA_character_)
    }
    switch(
        x,
        "K" = "K",
        "kelvin" = "K",
        "\u00b0C" = "degC",
        "degC" = "degC",
        "degree_Celsius" = "degC",
        "C" = "degC",
        "celsius" = "degC",
        "%" = "%",
        "percent" = "%",
        "Pa" = "Pa",
        "pascal" = "Pa",
        "hPa" = "hPa",
        "hectopascal" = "hPa",
        "W/m2" = "W/m^2",
        "W m-2" = "W/m^2",
        "W h m-2" = "W/m^2",
        "Wh/m2" = "W/m^2",
        "Wh/m^2" = "W/m^2",
        "m s-1" = "m/s",
        "m/s" = "m/s",
        "m" = "m",
        "meter" = "m",
        "metre" = "m",
        "cm" = "cm",
        "centimeter" = "cm",
        "centimetre" = "cm",
        "mm" = "mm",
        "millimeter" = "mm",
        "millimetre" = "mm",
        "kg m-2 s-1" = "kg m-2 s-1",
        "kg m^-2 s^-1" = "kg m-2 s-1",
        "kg kg-1" = "kg/kg",
        "kg kg^-1" = "kg/kg",
        "kg/kg" = "kg/kg",
        "1" = "1",
        x
    )
}

morpher__convert_value <- function(value, from, to) {
    morpher__convert_value_checked(value, from, to)$value
}

# Apply one supported unit conversion to either a scalar or a complete series.
morpher__convert_value_checked <- function(value, from, to) {
    value <- as.numeric(value)
    from <- morpher__unit_alias(from)
    to <- morpher__unit_alias(to)
    if (all(is.na(value)) || is.na(from) || is.na(to) || !nzchar(from) || !nzchar(to) || identical(from, to)) {
        return(list(value = value, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "K") && identical(to, "degC")) {
        return(list(value = value - 273.15, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "degC") && identical(to, "K")) {
        return(list(value = value + 273.15, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "hPa") && identical(to, "Pa")) {
        return(list(value = value * 100, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "Pa") && identical(to, "hPa")) {
        return(list(value = value / 100, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "1") && identical(to, "%")) {
        return(list(value = value * 100, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "%") && identical(to, "1")) {
        return(list(value = value / 100, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "m") && identical(to, "cm")) {
        return(list(value = value * 100, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "cm") && identical(to, "m")) {
        return(list(value = value / 100, ok = TRUE, message = NA_character_))
    }
    list(
        value = value,
        ok = FALSE,
        message = sprintf("Unsupported unit conversion from %s to %s.", from, to)
    )
}

morpher__default_epw_units <- function(field) {
    epw_file_unit(field)
}

morpher__humidity_input_si <- function(value, units, variable_id) {
    units <- vapply(units, morpher__unit_alias, character(1L))
    allowed <- switch(
        variable_id,
        huss = c("1", "kg/kg"),
        tas = c("K", "degC"),
        ps = c("Pa", "hPa")
    )
    unknown <- unique(units[is.na(units) | !units %in% allowed])
    if (length(unknown)) {
        cli::cli_abort(
            "Cannot derive hurs from {.val {variable_id}} with unsupported unit(s): {.val {unknown}}.",
            class = "epwshiftr_hurs_derivation_error"
        )
    }
    out <- as.numeric(value)
    if (identical(variable_id, "tas")) {
        out[units == "degC"] <- out[units == "degC"] + 273.15
    } else if (identical(variable_id, "ps")) {
        out[units == "hPa"] <- out[units == "hPa"] * 100
    }
    out
}

# Build canonical hurs extraction rows from aligned huss, tas, and ps rows.
# Direct hurs rows are handled by the caller and never pass through this helper.
morpher__derive_hurs_rows <- function(climate) {
    climate <- data.table::as.data.table(data.table::copy(climate))
    required_columns <- c("variable_id", "time", "value", "units")
    missing_columns <- setdiff(required_columns, names(climate))
    if (length(missing_columns)) {
        cli::cli_abort(
            "Cannot derive hurs because extraction data lacks column(s): {.val {missing_columns}}.",
            class = "epwshiftr_hurs_derivation_error"
        )
    }
    required_inputs <- c("huss", "tas", "ps")
    absent <- setdiff(required_inputs, unique(climate$variable_id))
    if (length(absent)) {
        present <- unique(as.character(climate$variable_id))
        cli::cli_abort(
            c(
                "Cannot derive hurs because input variable(s) are missing: {.val {absent}}.",
                "i" = "Available aligned input variable(s): {.val {present}}."
            ),
            class = "epwshiftr_hurs_derivation_error"
        )
    }

    key <- intersect(
        c("source_id", "experiment_id", "variant_label", "frequency",
          "table_id", "grid_label", "site_id", "time"),
        names(climate)
    )
    if (!"time" %in% key) {
        cli::cli_abort("Cannot derive hurs without aligned extraction times.")
    }

    # Collapse identical rows from overlapping source files, but fail when two
    # files disagree at the same identity and timestamp.
    prepare <- function(variable_id) {
        target_variable <- variable_id
        rows <- climate[climate[["variable_id"]] == target_variable]
        rows[["value_si"]] <- morpher__humidity_input_si(
            rows[["value"]], rows[["units"]], target_variable
        )
        conflicts <- rows[, list(
            values = data.table::uniqueN(get("value_si"))
        ), by = key]
        conflicts <- conflicts[conflicts[["values"]] > 1L]
        if (nrow(conflicts)) {
            cli::cli_abort(
                "Cannot derive hurs because {.val {variable_id}} has conflicting values at aligned timestamps.",
                class = "epwshiftr_hurs_derivation_error"
            )
        }
        rows[!duplicated(rows, by = key)]
    }
    huss <- prepare("huss")
    tas <- prepare("tas")[, c(key, "value_si"), with = FALSE]
    data.table::setnames(tas, "value_si", "tas_si")
    ps <- prepare("ps")[, c(key, "value_si"), with = FALSE]
    data.table::setnames(ps, "value_si", "ps_si")
    out <- tas[huss, on = key, nomatch = 0L]
    out <- ps[out, on = key, nomatch = 0L]
    if (nrow(out) != nrow(huss)) {
        cli::cli_abort(
            "Cannot derive hurs because huss, tas, and ps timestamps are not fully aligned.",
            class = "epwshiftr_hurs_derivation_error"
        )
    }

    source_plan_ids <- sort(unique(as.character(climate$plan_id)))
    source_plan_ids <- source_plan_ids[!is.na(source_plan_ids) & nzchar(source_plan_ids)]
    out[["variable_id"]] <- "hurs"
    out[["variable"]] <- "hurs"
    out[["value"]] <- epwphys__hurs_from_huss_si(
        out[["value_si"]], out[["tas_si"]], out[["ps_si"]]
    )
    out[["units"]] <- "%"
    out[["derived_from"]] <- "huss,tas,ps"
    out[["derivation"]] <-
        "q-to-vapour-pressure + ASHRAE saturation pressure"
    out[["source_plan_ids"]] <- paste(source_plan_ids, collapse = ",")
    out[, c("value_si", "tas_si", "ps_si") := NULL]
    out[]
}

morpher__diagnostic_columns <- function() {
    c(
        "stage", "severity", "code", "message", "plan_id", "summary_id",
        "baseline_id", "morph_id", "case_id", "variable_id", "epw_field",
        "period", "month", "action"
    )
}

morpher__empty_diagnostics <- function() {
    out <- data.table::data.table(
        stage = character(),
        severity = character(),
        code = character(),
        message = character(),
        plan_id = character(),
        summary_id = character(),
        baseline_id = character(),
        morph_id = character(),
        case_id = character(),
        variable_id = character(),
        epw_field = character(),
        period = character(),
        month = integer(),
        action = character()
    )
    out[, morpher__diagnostic_columns(), with = FALSE]
}

morpher__diagnostic <- function(stage, severity, code, message, plan_id = NA_character_,
                                 summary_id = NA_character_, baseline_id = NA_character_,
                                 morph_id = NA_character_, case_id = NA_character_,
                                 variable_id = NA_character_, epw_field = NA_character_,
                                 period = NA_character_, month = NA_integer_,
                                 action = NA_character_) {
    out <- data.table::data.table(
        stage = stage,
        severity = severity,
        code = code,
        message = message,
        plan_id = store__chr1(plan_id),
        summary_id = store__chr1(summary_id),
        baseline_id = store__chr1(baseline_id),
        morph_id = store__chr1(morph_id),
        case_id = store__chr1(case_id),
        variable_id = store__chr1(variable_id),
        epw_field = store__chr1(epw_field),
        period = store__chr1(period),
        month = as.integer(month),
        action = store__chr1(action)
    )
    out[, morpher__diagnostic_columns(), with = FALSE]
}

morpher__bind_diagnostics <- function(...) {
    args <- list(...)
    parts <- list()
    for (arg in args) {
        if (is.data.frame(arg)) {
            parts[[length(parts) + 1L]] <- arg
        } else if (is.list(arg)) {
            for (part in arg) {
                if (is.data.frame(part)) {
                    parts[[length(parts) + 1L]] <- part
                }
            }
        }
    }
    parts <- parts[vapply(parts, nrow, integer(1L)) > 0L]
    if (!length(parts)) {
        return(morpher__empty_diagnostics())
    }
    out <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
    out[, morpher__diagnostic_columns(), with = FALSE]
}

morpher__abort_diagnostics <- function(diagnostics, message = "EPW morphing preflight has blocking issues.") {
    errors <- diagnostics[diagnostics$severity == "error"]
    if (!nrow(errors)) {
        return(invisible(NULL))
    }
    cli::cli_abort(c(
        message,
        "x" = "{errors$message[[1L]]}"
    ))
}

morpher__engine_by_columns <- function(by) {
    map <- c(
        source_id = "source_id",
        experiment_id = "experiment_id",
        variant_label = "member_id",
        period = "interval",
        table_id = "table_id",
        frequency = "table_id"
    )
    unique(unname(map[intersect(by, names(map))]))
}

morpher__reference_case_by <- function(by) {
    setdiff(by, c("experiment_id", "period"))
}

# Observations are site records rather than members of a future-model case.
# Match a site identifier when one is part of the case key and otherwise make
# the same observed reference available to every model/scenario case.
morpher__observed_case_by <- function(by) {
    intersect(by, "site_id")
}

# Resolve calendar columns with row-level compatibility for old extraction
# artifacts. Canonical CF fields take precedence; missing values fall back to
# existing columns and finally to the surrogate POSIXct timestamp.
morpher__resolve_calendar_columns <- function(climate, month = FALSE, day = FALSE) {
    n <- nrow(climate)
    time <- if ("time" %in% names(climate)) climate$time else NULL

    # Resolve one field without replacing usable legacy values when a mixed
    # collection contains both old and current Parquet schemas.
    resolve <- function(target, canonical, fallback) {
        value <- if (target %in% names(climate)) {
            as.integer(climate[[target]])
        } else {
            rep.int(NA_integer_, n)
        }
        missing <- is.na(value)
        value[missing] <- fallback[missing]
        if (canonical %in% names(climate)) {
            canonical_value <- as.integer(climate[[canonical]])
            present <- !is.na(canonical_value)
            value[present] <- canonical_value[present]
        }
        data.table::set(climate, j = target, value = value)
    }

    if (any(c("year", "cf_year") %in% names(climate)) || !is.null(time)) {
        fallback_year <- if (is.null(time)) {
            rep.int(NA_integer_, n)
        } else {
            as.integer(format(time, "%Y", tz = "UTC"))
        }
        resolve("year", "cf_year", fallback_year)
    }

    if (isTRUE(month) &&
        (any(c("month", "cf_month") %in% names(climate)) || !is.null(time))) {
        fallback_month <- if (is.null(time)) {
            rep.int(NA_integer_, n)
        } else {
            as.integer(format(time, "%m", tz = "UTC"))
        }
        resolve("month", "cf_month", fallback_month)
    }
    if (isTRUE(day) &&
        (any(c("day", "cf_day") %in% names(climate)) || !is.null(time))) {
        fallback_day <- if (is.null(time)) {
            rep.int(NA_integer_, n)
        } else {
            as.integer(format(time, "%d", tz = "UTC"))
        }
        resolve("day", "cf_day", fallback_day)
    }

    climate[]
}

morpher__normalize_context_climate <- function(climate, years = NULL, labels = NULL) {
    climate <- data.table::as.data.table(data.table::copy(climate))
    if (!"time" %in% names(climate) && "datetime" %in% names(climate)) {
        climate[, time := datetime]
    }
    if ("variable" %in% names(climate)) {
        climate[, variable_id := variable]
    }
    if (!"variant_label" %in% names(climate) && "member_id" %in% names(climate)) {
        climate[, variant_label := member_id]
    }
    if (!"period" %in% names(climate) && "interval" %in% names(climate)) {
        climate[, period := as.character(interval)]
    }
    climate <- morpher__resolve_calendar_columns(climate)
    if (!"period" %in% names(climate) && "year" %in% names(climate)) {
        if (!is.null(years) && !is.null(labels)) {
            label_map <- data.table::data.table(year = as.integer(years), period = as.character(labels))
            climate <- label_map[climate, on = "year"]
        } else {
            climate[, period := as.character(year)]
        }
    }
    if (!"units" %in% names(climate)) {
        climate[, units := NA_character_]
    }
    for (col in c("lon", "lat")) {
        if (!col %in% names(climate)) {
            climate[, (col) := NA_real_]
        }
    }
    climate[]
}

morpher__context <- function(epw, climate, recipe = epw_morph_recipe("belcher"),
                              reference_climate = NULL,
                              years = NULL, labels = NULL,
                              reference_years = NULL, reference_labels = NULL,
                              by = character(),
                              case = NULL, strict = TRUE, warning = FALSE,
                              observed_reference = NULL) {
    if (!inherits(epw, "EpwFile")) {
        cli::cli_abort("`epw` must be an internal {.cls EpwFile} object.")
    }
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    climate <- morpher__normalize_context_climate(climate, years = years, labels = labels)
    if (!is.null(reference_climate)) {
        reference_climate <- morpher__normalize_context_climate(
            reference_climate,
            years = reference_years,
            labels = reference_labels
        )
    }
    if (!is.null(observed_reference)) {
        observed_reference <- morpher__normalize_context_climate(
            observed_reference
        )
    }
    checkmate::assert_character(by, any.missing = FALSE, unique = TRUE)
    epw <- epw$clone()
    # The explicit input set is the authoritative semantic view for new
    # components. Legacy fields remain available below so existing backends and
    # external extensions continue to receive the context they already consume.
    inputs <- weather__context_inputs(
        epw = epw,
        model_future = climate,
        model_historical = reference_climate,
        observed_reference = observed_reference
    )
    structure(
        list(
            inputs = inputs,
            epw = epw,
            climate = climate,
            reference_climate = reference_climate,
            observed_reference = observed_reference,
            recipe = recipe,
            years = years,
            labels = labels,
            reference_years = reference_years,
            reference_labels = reference_labels,
            by = by,
            case = case,
            strict = strict,
            warning = warning
        ),
        class = "morpher__context"
    )
}

morpher__context_required_columns <- function() {
    c("variable_id", "time", "period", "year", "lon", "lat", "units", "value")
}

morpher__validate_context <- function(context) {
    checkmate::assert_class(context, "morpher__context")
    climate <- context$climate
    missing <- setdiff(morpher__context_required_columns(), names(climate))
    if (length(missing)) {
        cli::cli_abort("Canonical EPW morphing climate data are missing column(s): {.val {missing}}.")
    }
    invisible(context)
}

morpher__context_variable <- function(context, variable_id) {
    morpher__validate_context(context)
    target_variable_id <- store__chr1(variable_id)
    climate <- context$climate
    climate[climate[["variable_id"]] == target_variable_id]
}

morpher__context_reference_variable <- function(context, variable_id) {
    morpher__validate_context(context)
    if (is.null(context$reference_climate)) {
        return(data.table::data.table())
    }
    missing <- setdiff(morpher__context_required_columns(), names(context$reference_climate))
    if (length(missing)) {
        cli::cli_abort("Canonical EPW morphing reference climate data are missing column(s): {.val {missing}}.")
    }
    target_variable_id <- store__chr1(variable_id)
    climate <- context$reference_climate
    climate[climate[["variable_id"]] == target_variable_id]
}

morpher__context_year_labels <- function(context) {
    if (!is.null(context$years)) {
        return(list(years = context$years, labels = context$labels))
    }
    climate <- context$climate
    if (!all(c("year", "period") %in% names(climate))) {
        return(list(years = NULL, labels = NULL))
    }
    period_years <- unique(climate[, .(year = as.integer(year), period = as.character(period))])
    data.table::setorder(period_years, year, period)
    years <- sort(unique(period_years$year))
    labels <- vapply(years, function(year) {
        period_years$period[match(year, period_years$year)]
    }, character(1L))
    list(years = years, labels = labels)
}

morpher__context_pick_column <- function(data, primary, fallback = NULL, default = NA_character_) {
    if (primary %in% names(data)) {
        return(data[[primary]])
    }
    if (!is.null(fallback) && fallback %in% names(data)) {
        return(data[[fallback]])
    }
    rep(default, nrow(data))
}

morpher__context_identity_rows <- function(data) {
    data.table::data.table(
        activity_drs = store__chr(morpher__context_pick_column(data, "activity_drs", "activity_id")),
        institution_id = store__chr(morpher__context_pick_column(data, "institution_id")),
        source_id = store__chr(morpher__context_pick_column(data, "source_id")),
        experiment_id = store__chr(morpher__context_pick_column(data, "experiment_id")),
        member_id = store__chr(morpher__context_pick_column(data, "member_id", "variant_label")),
        table_id = store__chr(morpher__context_pick_column(data, "table_id", "frequency")),
        lon = as.numeric(morpher__context_pick_column(data, "lon", default = NA_real_)),
        lat = as.numeric(morpher__context_pick_column(data, "lat", default = NA_real_))
    )
}

morpher__monthly_climate <- function(data, years = NULL, labels = NULL, warning = FALSE) {
    data <- data.table::as.data.table(data.table::copy(data))
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    missing <- setdiff(c("variable_id", "time", "year", "period", "units", "value", "lon", "lat"), names(data))
    if (length(missing)) {
        cli::cli_abort("Canonical EPW morphing climate data are missing column(s): {.val {missing}}.")
    }
    data <- morpher__resolve_calendar_columns(data, month = TRUE, day = TRUE)
    data[, year := as.integer(year)]
    data <- data[!(month == 2L & day == 29L)]

    checkmate::assert_integerish(years, lower = 1900, unique = TRUE, sorted = TRUE, any.missing = FALSE, null.ok = TRUE)
    if (is.null(years)) {
        data[, interval := as.factor(period)]
    } else {
        years <- as.integer(years)
        missing_years <- setdiff(years, unique(data$year))
        if (length(missing_years)) {
            stop("Input data does not contain any data of year ", paste0("'", missing_years, "'", collapse = ", "), ".")
        }
        data <- data[year %in% years]
        if (is.null(labels)) {
            data[, interval := as.factor(year)]
        } else {
            if (is.factor(labels)) {
                labels <- as.character(labels)
            }
            checkmate::assert_character(labels, any.missing = FALSE, len = length(years))
            label_map <- data.table::data.table(year = years, interval = as.factor(labels))
            data <- label_map[data, on = "year"]
        }
    }

    if (isTRUE(warning)) {
        by_cols <- intersect(c("variable_id", "table_id", "frequency", "source_id", "experiment_id", "variant_label", "member_id"), names(data))
        if (length(by_cols)) {
            rng_year <- data[, list(years = list(sort(unique(year))), num_years = length(unique(year))), by = by_cols][num_years < 10L]
            if (nrow(rng_year)) {
                data.table::set(rng_year, NULL, "index_case", seq.int(nrow(rng_year)))
                mes <- rng_year[, by = "index_case", {
                    yrs <- paste0("'", years[[1L]], "'", collapse = ", ")
                    list(message = sprintf("#%i | [%i] %s", .BY$index_case, num_years, yrs))
                }]$message
                warning("Case(s) shown below contains CMIP data less than a decade. ",
                    "The morphed data may not be able to capture average weather of the future climate.\n",
                    paste0(mes, collapse = "\n"), call. = FALSE)
            }
        }
    }

    identity <- morpher__context_identity_rows(data)
    data <- data.table::data.table(
        identity,
        units = as.character(data$units),
        value = as.numeric(data$value),
        month = as.integer(data$month),
        interval = data$interval
    )
    group_cols <- c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id", "units", "month", "interval")
    out <- data[, .(
        lon = mean(lon, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        value = mean(value, na.rm = TRUE)
    ), by = group_cols]
    data.table::setcolorder(out, c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id", "lon", "lat", "units", "value", "month", "interval"))
    out[]
}


morpher__factor_case_columns <- function(data) {
    intersect(
        c("activity_drs", "institution_id", "source_id", "experiment_id",
          "member_id", "interval"),
        names(data)
    )
}

morpher__engine_complete_data <- function(epw, parts, by = character()) {
    suppressMessages(epw$drop_unit())
    data_epw <- data.table::as.data.table(epw$data())

    parts <- parts[vapply(parts, nrow, integer(1L)) > 0L]
    if (!length(parts)) {
        return(data.table::data.table())
    }

    parts <- lapply(parts, function(dt) {
        dt <- data.table::copy(dt)
        drop <- intersect(c("delta", "alpha"), names(dt))
        if (length(drop)) {
            data.table::set(dt, j = drop, value = NULL)
        }
        dt
    })

    cols_dt <- c("datetime", "year", "month", "day", "hour", "minute")
    cols_by <- intersect(morpher__engine_by_columns(by), Reduce(intersect, lapply(parts, names)))
    keep_base <- c(cols_by, cols_dt)
    for (i in seq_along(parts)) {
        keep <- c(intersect(names(data_epw), names(parts[[i]])), keep_base)
        drop <- setdiff(names(parts[[i]]), keep)
        if (length(drop)) {
            data.table::set(parts[[i]], j = drop, value = NULL)
        }
    }

    merge_by <- c(cols_by, cols_dt)
    merged <- Reduce(function(x, y) merge(x, y, by = merge_by), parts)
    merged <- merged[, lapply(.SD, mean), by = merge_by]

    if ("total_sky_cover" %in% names(merged)) {
        data.table::set(merged, j = "total_sky_cover", value = as.integer(round(merged$total_sky_cover)))
    }
    if ("opaque_sky_cover" %in% names(merged)) {
        data.table::set(merged, j = "opaque_sky_cover", value = as.integer(round(merged$opaque_sky_cover)))
    }

    value_cols <- setdiff(intersect(names(merged), names(data_epw)), cols_dt)
    complete_base <- data.table::copy(data_epw)
    complete_base[, .epw_order := .I]
    complete <- merge(
        complete_base,
        merged,
        by = cols_dt,
        all.x = TRUE,
        sort = FALSE,
        suffixes = c("", ".morphed")
    )
    for (col in value_cols) {
        morphed_col <- paste0(col, ".morphed")
        if (morphed_col %in% names(complete)) {
            idx <- !is.na(complete[[morphed_col]])
            data.table::set(complete, i = which(idx), j = col, value = complete[[morphed_col]][idx])
            data.table::set(complete, j = morphed_col, value = NULL)
        }
    }
    data.table::setorder(complete, .epw_order)
    data.table::set(complete, j = ".epw_order", value = NULL)
    complete[]
}

#' Create an EPW morphing backend result
#'
#' @description
#' Backend runner functions return `epw_morph_result` objects. Use
#' `epw_morph_result()` in custom backends after producing complete hourly EPW
#' weather data.
#'
#' @param context Canonical EPW morphing context supplied to the backend runner.
#' @param epw EPW object associated with the result.
#' @param data Complete hourly EPW weather data ready for Parquet output or
#'        EPW writing.
#' @param parts Optional named list of intermediate backend result tables.
#' @param diagnostics Optional backend diagnostic rows.
#' @param factors Optional backend factor rows.
#'
#' @return An `epw_morph_result` object.
#' @export
epw_morph_result <- function(context, epw = context$epw, data, parts = list(),
                             diagnostics = morpher__empty_diagnostics(), factors = NULL) {
    checkmate::assert_class(context, "morpher__context")
    if (!inherits(epw, "EpwFile")) {
        cli::cli_abort("`epw` must be an internal {.cls EpwFile} object.")
    }
    if (missing(data)) {
        cli::cli_abort("`data` must be supplied.")
    }
    checkmate::assert_list(parts, names = "named")
    data <- data.table::as.data.table(data.table::copy(data))
    morpher__engine_output(context, epw, parts = parts, data = data, diagnostics = diagnostics, factors = factors)
}

morpher__engine_output <- function(context, epw, parts, data = NULL, diagnostics = morpher__empty_diagnostics(), factors = NULL) {
    if (is.null(data)) {
        data <- morpher__engine_complete_data(epw, parts, by = context$by)
    }
    if (context$recipe$backend %in% c("belcher", "belcher_absolute")) {
        # Monthly backends predate executable pipeline stages. Applying their
        # declared policy by complete case gives them the same physical boundary
        # as pipeline methods while retaining every legacy field value.
        physical <- epwphys__apply_groups(
            data,
            epwphys__recipe_policy(context$recipe),
            group_columns = morpher__factor_case_columns(data)
        )
        data <- physical$weather
    }
    structure(
        list(
            backend = context$recipe$backend,
            recipe = context$recipe,
            epw = epw,
            data = data,
            parts = parts,
            diagnostics = diagnostics,
            factors = factors
        ),
        class = "epw_morph_result"
    )
}

morpher__result_as_morphed <- function(result) {
    out <- c(list(epw = result$epw), result$parts)
    class(out) <- "epw_cmip6_morphed"
    out
}

morpher__run_context <- function(context) {
    checkmate::assert_class(context, "morpher__context")
    recipe_spec <- morpher__recipe_spec(context$recipe)
    if (!is.null(recipe_spec)) {
        recipe__validate_inputs(recipe_spec, context$inputs)
    }
    backend <- epw_morph_backend(context$recipe$backend)
    result <- backend$run(context)
    if (!inherits(result, "epw_morph_result") &&
        !S7::S7_inherits(result, WeatherSequenceResult)) {
        cli::cli_abort(
            "EPW morphing backend {.val {backend$name}} did not return an {.cls epw_morph_result} or {.cls WeatherSequenceResult}."
        )
    }
    if (!is.null(recipe_spec)) {
        actual_output_type <- if (S7::S7_inherits(
            result,
            WeatherSequenceResult
        )) {
            result@output_type
        } else {
            "representative_year"
        }
        if (!identical(recipe_spec@output_type, actual_output_type)) {
            cli::cli_abort(c(
                "Future-weather backend output does not match its registered recipe contract.",
                "x" = "Recipe {.val {recipe_spec@name}} declares {.val {recipe_spec@output_type}} but returned {.val {actual_output_type}}."
            ))
        }
    }
    result
}
# }}}
