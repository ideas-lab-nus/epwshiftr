#' Plan independent hourly ERA5 point requests
#'
#' Build a credential-free request plan without downloading weather, requiring
#' an EPW template, or constructing a future-climate workflow. Supports explicit
#' ERA5 single-level ARCO and CDS hourly sources.
#' Unsupported products and access routes are rejected, never substituted.
#'
#' @param source A [shift_era5()] specification with explicit `variables`,
#'   `frequency = "hour"`, and `access = "arco"` or `"cds"`.
#' @param sites One [shift_site()] or a non-empty list of uniquely named sites
#'   (unique site IDs; list names are optional). Coordinates describe requested
#'   points; a plan cannot establish the coordinates actually returned by CDS.
#'   CDS sites must explicitly identify native 0.25-degree grid points.
#' @param dates Optional vector of UTC dates (Date or ISO YYYY-MM-DD strings).
#'   Request exactly these full days, without additional padding. Dates must be
#'   within `source` years or their immediately adjacent boundary days. `NULL`
#'   retains complete years plus one UTC boundary day on each side. Callers
#'   selecting local source months must include their own UTC boundary coverage.
#' @return A list with `schema_version`, `status = "planned"`, `source`,
#'   `sites`, `variables`, `requests`, and `submitted = 0L`. `requests` is a
#'   data.table containing stable SHA-256 request IDs, dataset, raw variable,
#'   requested coordinates, inclusive UTC dates, expected hourly sample count,
#'   associated site IDs, provider request bodies, and `not_submitted` status.
#'   A full UTC boundary day is included on each side of each contiguous
#'   requested year range; intervening gap years are not silently acquired.
#'   `variables` records the raw dependencies of each requested variable.
#' @details
#' Planning performs no authentication, network access, or file writes. The
#' expected sample count describes the requested UTC interval, not a verified
#' download. Raw requests and normalized weather have separate identities;
#' this function does not read or normalize weather and does not enforce a
#' transfer budget. Independent retrieval, receipts and recovery are separate
#' execution work. No claim of completed data acquisition follows from a plan.
#' Equivalent longitude aliases and derived-variable dependencies share raw
#' requests. All associated site IDs remain recorded. Provider availability,
#' actual file coordinates, units and complete hourly coverage still require
#' validation when data are acquired.
#' CDS requests are split by calendar month and contiguous days, one raw field
#' per request, avoiding year/month/day Cartesian expansion and mixed step types.
#' Use `rsdsdiff` to request its raw dependencies SSRD and FDIR; this interface
#' does not calculate diffuse radiation. Cloud cover is normalized to percent.
#' @examples
#' source <- shift_era5(2001, variables = c("tas", "rsds"),
#'     frequency = "hour", access = "arco")
#' site <- shift_site("point", lon = 104, lat = 1.25)
#' plan <- shift_plan_reanalysis(source, site)
#' plan$requests[, c("variable_id", "expected_hours", "status")]
#' @export
# shift_plan_reanalysis {{{
shift_plan_reanalysis <- function(source, sites, dates = NULL) {
    if (
        !S7::S7_inherits(source, ShiftReanalysisSpec) ||
            !identical(source@provider, "copernicus") ||
            !identical(source@dataset, "era5") ||
            !identical(source@product, "single_levels") ||
            !source@access %in% c("arco", "cds")
    ) {
        cli::cli_abort(
            "Independent planning requires ERA5 single_levels with explicit access = 'arco' or 'cds'."
        )
    }
    if (is.null(source@variables) || !identical(source@frequency, "hour")) {
        cli::cli_abort("Supply explicit variables and frequency = 'hour'.")
    }
    if (S7::S7_inherits(sites, ShiftSite)) {
        sites <- list(sites)
    }
    checkmate::assert_list(sites, min.len = 1L)
    site_rows <- lapply(sites, function(site) {
        if (!S7::S7_inherits(site, ShiftSite)) {
            cli::cli_abort("Each site must be a ShiftSite.")
        }
        checkmate::assert_string(site@id, min.chars = 1L)
        checkmate::assert_number(
            site@lat,
            lower = -90,
            upper = 90,
            finite = TRUE
        )
        checkmate::assert_number(
            site@lon,
            lower = -180,
            upper = 360,
            finite = TRUE
        )
        list(
            site_id = site@id,
            latitude = site@lat,
            longitude = reanalysis__longitude(site@lon)
        )
    })
    site_table <- data.table::rbindlist(site_rows)
    # +180 and -180 denote the same meridian. Canonicalize within this new
    # raw-request contract without changing legacy source normalization.
    data.table::set(
        site_table,
        j = "longitude",
        value = (site_table$longitude + 180) %% 360 - 180
    )
    if (anyDuplicated(site_table$site_id)) {
        cli::cli_abort("Sites must have unique IDs.")
    }
    dependencies <- lapply(
        source@variables,
        era5__source_variables,
        product = source@product
    )
    raw_variables <- sort(unique(unlist(dependencies, use.names = FALSE)))
    manifest <- era5__variable_manifest()
    supported <- manifest$variable_id[manifest$time_series_single_levels]
    if (source@access == "arco" && length(setdiff(raw_variables, supported))) {
        cli::cli_abort(
            "Requested variables are not available through ARCO; no CDS fallback is performed."
        )
    }
    if (
        source@access == "cds" &&
            any(
                abs(
                    c(site_table$latitude, site_table$longitude) *
                        4 -
                        round(c(site_table$latitude, site_table$longitude) * 4)
                ) >
                    1e-8
            )
    ) {
        cli::cli_abort(
            "CDS point planning requires explicit native 0.25-degree grid coordinates."
        )
    }
    dataset <- era5__dataset_id(source@product, source@access)
    years <- sort(source@years)
    ranges <- split(years, cumsum(c(TRUE, diff(years) != 1L)))
    explicit_dates <- !is.null(dates)
    if (explicit_dates || source@access == "cds") {
        days <- reanalysis_plan__dates(dates, ranges)
        group <- cumsum(c(TRUE, diff(days) != 1))
        # CDS dates are also separated by month to avoid Cartesian expansion.
        if (source@access == "cds") {
            group <- paste(format(days, "%Y-%m"), group)
        }
        ranges <- split(days, group)
    }
    rows <- vector(
        "list",
        length(sites) * length(raw_variables) * length(ranges)
    )
    cursor <- 0L
    # Each iteration creates one external request body, rather than processing
    # hourly weather values. Preallocation bounds the small request matrix.
    for (i in seq_along(sites)) {
        for (variable in raw_variables) {
            for (period_values in ranges) {
                # Never pass disjoint years to the existing min/max request builder:
                # each contiguous range gets its own explicitly identified padding.
                period <- shift_era5(
                    if (source@access == "cds") {
                        # Build only this month's base request, never allocate
                        # the full multi-decade calendar for every small job.
                        as.integer(format(period_values[1L], "%Y"))
                    } else if (explicit_dates) {
                        source@years
                    } else {
                        period_values
                    },
                    variables = source@variables,
                    frequency = "hour",
                    access = source@access
                )
                request <- era5__request(
                    period,
                    variable,
                    sites[[i]],
                    source@access
                )
                if (source@access == "arco") {
                    request$location$longitude <- site_table$longitude[[i]]
                    if (explicit_dates) {
                        request$date <- sprintf(
                            "%s/%s",
                            min(period_values),
                            max(period_values)
                        )
                    }
                    bounds <- as.Date(strsplit(
                        request$date,
                        "/",
                        fixed = TRUE
                    )[[1L]])
                } else {
                    attr(request, "requested_years") <- NULL
                    request$year <- format(period_values[1L], "%Y")
                    request$month <- format(period_values[1L], "%m")
                    request$day <- format(period_values, "%d")
                    # Explicit grid centres avoid a hidden nearest-point choice.
                    request$area <- rep(
                        c(site_table$latitude[i], site_table$longitude[i]),
                        2L
                    )
                    bounds <- range(period_values)
                }
                identity <- jsonlite::toJSON(
                    list(dataset = dataset, request = request),
                    auto_unbox = TRUE,
                    digits = NA
                )
                cursor <- cursor + 1L
                rows[[cursor]] <- list(
                    request_id = checksum_bytes(charToRaw(identity)),
                    dataset = dataset,
                    variable_id = variable,
                    latitude = site_table$latitude[[i]],
                    longitude = site_table$longitude[[i]],
                    date_start = bounds[[1L]],
                    date_end = bounds[[2L]],
                    expected_hours = as.integer(
                        bounds[[2L]] - bounds[[1L]] + 1L
                    ) *
                        24L,
                    site_ids = list(sites[[i]]@id),
                    request = list(request),
                    status = "not_submitted"
                )
            }
        }
    }
    requests <- data.table::rbindlist(rows)
    groups <- split(seq_len(nrow(requests)), requests$request_id)
    aliases <- lapply(groups, function(i) {
        sort(unique(unlist(requests$site_ids[i], use.names = FALSE)))
    })
    requests <- requests[match(names(groups), requests$request_id)]
    data.table::set(requests, j = "site_ids", value = unname(aliases))
    result <- list(
        schema_version = 1L,
        status = "planned",
        source = list(
            dataset = "era5",
            product = "single_levels",
            years = source@years,
            frequency = "hour",
            access = source@access
        ),
        sites = site_table,
        variables = data.table::data.table(
            variable_id = source@variables,
            source_variables = dependencies
        ),
        requests = requests,
        submitted = 0L
    )
    if (explicit_dates) {
        result$source$dates <- as.character(days)
    }
    result
}
# }}}

# Validate exact UTC days before splitting requests. ISO roundtrip validation
# prevents silent truncation, rollover and POSIX/local-time interpretation.
reanalysis_plan__dates <- function(dates, ranges) {
    permitted <- sort(unique(do.call(
        c,
        lapply(ranges, function(years) {
            seq(
                as.Date(sprintf("%d-01-01", min(years))) - 1L,
                as.Date(sprintf("%d-12-31", max(years))) + 1L,
                by = "day"
            )
        })
    )))
    if (is.null(dates)) {
        return(permitted)
    }
    if (!inherits(dates, "Date") && !is.character(dates)) {
        cli::cli_abort(
            "Supply UTC dates as Date values or ISO YYYY-MM-DD strings."
        )
    }
    if (is.character(dates)) {
        parsed <- as.Date(dates, format = "%Y-%m-%d")
        if (anyNA(parsed) || any(format(parsed, "%Y-%m-%d") != dates)) {
            cli::cli_abort("Dates must be valid ISO YYYY-MM-DD calendar dates.")
        }
        dates <- parsed
    }
    if (
        !length(dates) ||
            anyNA(dates) ||
            any(!is.finite(as.numeric(dates))) ||
            any(as.numeric(dates) != floor(as.numeric(dates))) ||
            any(!dates %in% permitted)
    ) {
        cli::cli_abort(
            "Dates must be nonempty whole UTC days within source years and boundary days."
        )
    }
    # Date and data.table IDate can carry integer or double day counts. Freeze
    # one representation so reconstruction from saved ISO dates is exact.
    as.Date(sort(unique(as.numeric(dates))), origin = "1970-01-01")
}

# Compare every plan value and type exactly while excluding data.table's
# process-local allocation pointer, which cannot survive an RDS round trip.
# Work on copies so validating a caller's saved plan never modifies its tables.
# reanalysis_plan__identical {{{
reanalysis_plan__identical <- function(plan, canonical) {
    # The fixed three tables carry the allocation pointer; retain all their
    # other attributes rather than using an approximate numeric comparison.
    strip_pointer <- function(value) {
        for (name in c("sites", "variables", "requests")) {
            table <- data.table::copy(value[[name]])
            data.table::setattr(table, ".internal.selfref", NULL)
            value[[name]] <- table
        }
        value
    }
    identical(strip_pointer(plan), strip_pointer(canonical))
}
# }}}

# vim: fdm=marker :
