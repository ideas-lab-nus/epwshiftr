# daily morphing statistical primitives {{{

# Build a calendar-neutral daily grid at day midpoints. Midpoints avoid making
# phase zero belong preferentially to either side of the circular year boundary.
daily__phase_grid <- function(target_year_days = 365L) {
    checkmate::assert_integerish(
        target_year_days,
        lower = 3L,
        len = 1L,
        any.missing = FALSE
    )
    target_year_days <- as.integer(target_year_days)

    (seq_len(target_year_days) - 0.5) / target_year_days
}

# Compute the shortest circular separation between an observation phase p and
# a target phase c using min(abs(p - c), 1 - abs(p - c)).
daily__phase_distance <- function(annual_phase, center) {
    direct_distance <- abs(annual_phase - center)
    pmin(direct_distance, 1 - direct_distance)
}

# Validate the shared target-grid and odd moving-window definition once, then
# retain its half-width in normalized annual-phase units for downstream use.
daily__window_spec <- function(window_days = 31L, target_year_days = 365L) {
    checkmate::assert_integerish(
        target_year_days,
        lower = 3L,
        len = 1L,
        any.missing = FALSE
    )
    checkmate::assert_integerish(
        window_days,
        lower = 1L,
        len = 1L,
        any.missing = FALSE
    )
    target_year_days <- as.integer(target_year_days)
    window_days <- as.integer(window_days)

    if (window_days > target_year_days) {
        cli::cli_abort(
            "{.arg window_days} must not exceed {.arg target_year_days}."
        )
    }
    if (window_days %% 2L == 0L) {
        cli::cli_abort(
            "{.arg window_days} must be odd so every window has one target-day center."
        )
    }

    list(
        window_days = window_days,
        target_year_days = target_year_days,
        half_width = (window_days %/% 2L) / target_year_days,
        full_cycle = identical(window_days, target_year_days)
    )
}

# Validate canonical annual phases before circular arithmetic. A phase equal to
# one is rejected because it aliases phase zero and would duplicate the boundary.
daily__check_phase <- function(annual_phase, name = "annual_phase") {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_numeric(
        annual_phase,
        lower = 0,
        finite = TRUE,
        any.missing = FALSE,
        .var.name = name
    )
    if (any(annual_phase >= 1)) {
        cli::cli_abort("{.arg {name}} must contain values in the interval [0, 1).")
    }

    as.numeric(annual_phase)
}

# Select observations inside an odd-width circular window expressed in target
# grid days. Calendar-native observations participate through annual_phase, so
# no Gregorian date or raw day-of-year pairing is introduced.
daily__phase_window <- function(annual_phase, center, window_days = 31L,
                                target_year_days = 365L) {
    annual_phase <- daily__check_phase(annual_phase)
    center <- daily__check_phase(center, "center")
    if (length(center) != 1L) {
        cli::cli_abort("{.arg center} must be one annual-phase value.")
    }
    spec <- daily__window_spec(window_days, target_year_days)

    if (isTRUE(spec$full_cycle)) {
        return(rep.int(TRUE, length(annual_phase)))
    }

    # Include mathematically exact boundary points despite floating-point noise.
    tolerance <- 8 * .Machine$double.eps
    daily__phase_distance(annual_phase, center) <= spec$half_width + tolerance
}

# Estimate one group's climatology over the complete target phase grid. Inputs
# are prevalidated by daily__climatology() to keep the 365-window loop lean.
daily__climatology_group <- function(annual_phase, value, target_phase, spec) {
    target_count <- length(target_phase)
    sample_count <- integer(target_count)
    climatology <- rep.int(NA_real_, target_count)
    valid_value <- !is.na(value)

    if (isTRUE(spec$full_cycle)) {
        # A full-cycle window has the same estimate at every target phase.
        n_valid <- sum(valid_value)
        if (n_valid) {
            climatology[] <- mean(value[valid_value])
        }
        sample_count[] <- n_valid
    } else {
        tolerance <- 8 * .Machine$double.eps
        for (i in seq_along(target_phase)) {
            in_window <- daily__phase_distance(
                annual_phase,
                target_phase[[i]]
            ) <= spec$half_width + tolerance
            keep <- in_window & valid_value
            sample_count[[i]] <- sum(keep)
            if (sample_count[[i]]) {
                climatology[[i]] <- mean(value[keep])
            }
        }
    }

    data.table::data.table(
        target_day = seq_len(target_count),
        annual_phase = target_phase,
        climatology = climatology,
        n = sample_count
    )
}

# Estimate a grouped circular daily climatology on a common target grid. The
# returned n column counts only finite, non-missing values used in each mean.
daily__climatology <- function(data, value = "value", by = character(),
                               window_days = 31L, target_year_days = 365L) {
    checkmate::assert_data_frame(data)
    checkmate::assert_string(value, min.chars = 1L)
    checkmate::assert_character(by, any.missing = FALSE, unique = TRUE)

    if (!nrow(data)) {
        cli::cli_abort("{.arg data} must contain at least one observation.")
    }
    if (anyDuplicated(names(data))) {
        cli::cli_abort("{.arg data} must have unique column names.")
    }

    required <- unique(c("annual_phase", value, by))
    missing <- setdiff(required, names(data))
    if (length(missing)) {
        cli::cli_abort(
            "{.arg data} is missing required column{?s}: {.val {missing}}."
        )
    }

    reserved_by <- intersect(
        by,
        c(
            "annual_phase",
            value,
            "target_day",
            "climatology",
            "n",
            ".daily_value"
        )
    )
    if (length(reserved_by)) {
        cli::cli_abort(
            "{.arg by} cannot use value, phase, or output column{?s}: {.val {reserved_by}}."
        )
    }

    annual_phase <- daily__check_phase(data[["annual_phase"]])
    climate_value <- data[[value]]
    checkmate::assert_numeric(
        climate_value,
        finite = TRUE,
        any.missing = TRUE,
        .var.name = sprintf("data[['%s']]", value)
    )
    climate_value <- as.numeric(climate_value)

    target_phase <- daily__phase_grid(target_year_days)
    spec <- daily__window_spec(window_days, target_year_days)

    # Work on a private data.table so callers can safely reuse their source data.
    source <- data.table::as.data.table(data.table::copy(data))
    working <- source[, c(by, "annual_phase"), with = FALSE]
    data.table::set(working, j = "annual_phase", value = annual_phase)
    data.table::set(working, j = ".daily_value", value = climate_value)

    if (!length(by)) {
        return(daily__climatology_group(
            working[["annual_phase"]],
            working[[".daily_value"]],
            target_phase,
            spec
        ))
    }

    working[
        ,
        daily__climatology_group(
            .SD[["annual_phase"]],
            .SD[[".daily_value"]],
            target_phase,
            spec
        ),
        by = by,
        .SDcols = c("annual_phase", ".daily_value")
    ][]
}

# }}}
