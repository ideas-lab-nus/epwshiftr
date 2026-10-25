# Offline differential checks against an explicitly supplied, pinned CFtime
# source checkout. No downloads, installation or package dependency changes.
# Run from the package root:
# Rscript --vanilla tools/check-cf-time-differential.R /path/to/CFtime /path/to/report.json

# Compare native fields rather than surrogate POSIX labels. The external parser
# is a second implementation, not an oracle for the documented divergent cases.
cf_diff__main <- function(args) {
    if (length(args) != 2L) {
        stop("Supply the local CFtime checkout and output JSON path.")
    }
    root <- normalizePath(args[[1L]], mustWork = TRUE)
    revision <- system2(
        "git",
        c("-C", shQuote(root), "rev-parse", "HEAD"),
        stdout = TRUE
    )
    expected_revision <- "b766909ecf893b39d84c4912742d19c3c2cfed73"
    stopifnot(identical(revision, expected_revision))
    dirty <- system2(
        "git",
        c("-C", shQuote(root), "status", "--porcelain", "--untracked-files=no"),
        stdout = TRUE
    )
    stopifnot(!length(dirty))
    ours <- new.env(parent = globalenv())
    sys.source(file.path("R", "netcdf.R"), ours)
    external <- new.env(parent = globalenv())
    description <- read.dcf(file.path(root, "DESCRIPTION"))
    files <- scan(
        text = description[1L, "Collate"],
        what = character(),
        quiet = TRUE
    )
    for (file in files) {
        sys.source(file.path(root, "R", file), external)
    }
    external$.onLoad(NULL, NULL)
    results <- list()

    # Record every comparison, preserving the original inputs and both outputs.
    record <- function(
        name,
        units,
        calendar,
        offsets,
        expected_difference = FALSE
    ) {
        decoded <- ours$parse_cf_time(offsets, units, calendar)
        native <- attr(decoded, "cf_coordinates")
        object <- external$CFtime(units, calendar, offsets)
        reference <- object$calendar$offsets2time(object$offsets)
        actual <- native[, c(
            "cf_year",
            "cf_month",
            "cf_day",
            "cf_second_of_day"
        )]
        names(actual) <- c("year", "month", "day", "second_of_day")
        target <- reference[, c("year", "month", "day")]
        target$second_of_day <- reference$hour *
            3600 +
            reference$minute * 60 +
            reference$second
        same <- isTRUE(all.equal(
            actual,
            target,
            check.attributes = FALSE,
            tolerance = 1e-9
        ))
        stopifnot(identical(same, !expected_difference))
        stopifnot(identical(as.double(object$offsets), as.double(offsets)))
        results[[name]] <<- list(
            units = units,
            calendar = calendar,
            offsets = offsets,
            expected_difference = expected_difference,
            ours = actual,
            CFtime = target
        )
        invisible(decoded)
    }
    for (calendar in ours$CF_TIME_CALENDARS) {
        for (unit in c("days", "hours", "minutes", "seconds")) {
            record(
                paste(calendar, unit, sep = "/"),
                paste(unit, "since 2000-02-28 12:34:56"),
                calendar,
                c(-48, -1, 0, 1, 48)
            )
        }
    }
    record("standard/reform", "days since 1582-10-04", "standard", -1:2)
    record(
        "standard/ancient_epoch",
        "days since 0001-01-01",
        "standard",
        c(0, 547557, 730121)
    )
    record("standard/historical_leap", "days since 1500-02-28", "standard", 0:2)
    record(
        "proleptic/signed_epoch",
        "days since -0001-01-01",
        "proleptic_gregorian",
        c(0, 365, 730)
    )

    # These are deliberate, specification-backed differences in the pinned
    # external revision: fixed UDUNITS durations and submillisecond precision.
    record(
        "month/fixed_duration",
        "months since 2000-01-31",
        "standard",
        c(0, 1, 2),
        TRUE
    )
    record(
        "year/fixed_duration",
        "years since 2000-01-01",
        "standard",
        c(0, 1, 2),
        TRUE
    )
    precise <- record(
        "seconds/submillisecond",
        "seconds since 2000-01-01",
        "standard",
        c(0, .0004, .0008),
        TRUE
    )
    stopifnot(
        max(abs(
            attr(precise, "cf_coordinates")$cf_second_of_day -
                c(0, .0004, .0008)
        )) <
            1e-12
    )
    record(
        "seconds/midnight",
        "seconds since 2000-01-01",
        "standard",
        c(86399.9996, 86400.0004),
        TRUE
    )
    for (zone in c("+02:00", "-6")) {
        units <- paste("hours since 2000-01-01 00:00:00", zone)
        decoded <- record(
            paste0("timezone/", zone),
            units,
            "standard",
            0:1,
            TRUE
        )
        shift <- if (zone == "+02:00") -7200 else 21600
        stopifnot(all(as.numeric(decoded) == 946684800 + shift + 0:1 * 3600))
        reference <- external$CFtime(units, "standard", 0:1)$as_timestamp(
            asPOSIX = TRUE
        )
        stopifnot(any(as.numeric(reference) != as.numeric(decoded)))
    }

    # Differential bounds observations never synthesize intervals or mutate
    # source values. Point/mean validation remains the package's own contract.
    offsets <- c(0, .5, 1)
    bounds <- rbind(offsets, offsets)
    object <- external$CFtime("days since 2000-01-01", "standard", offsets)
    object$set_bounds(bounds)
    stopifnot(
        identical(object$get_bounds(), bounds),
        identical(object$offsets, offsets)
    )
    rejected <- tryCatch(
        ours$parse_cf_time(0, "days since 1582-10-10"),
        error = identity
    )
    stopifnot(inherits(rejected, "error"))
    report <- list(
        CFtime_revision = revision,
        CFtime_version = description[1L, "Version"],
        parser_sha256 = unname(tools::sha256sum(file.path("R", "netcdf.R"))),
        standard = "CF 1.7 time coordinates and UDUNITS durations; CF 1.13 section 4.4.2 clarifies zones",
        comparisons = length(results),
        expected_differences = sum(vapply(
            results,
            function(x) x$expected_difference,
            logical(1L)
        )),
        bounds_preserved = TRUE,
        results = results
    )
    jsonlite::write_json(
        report,
        args[[2L]],
        pretty = TRUE,
        auto_unbox = TRUE,
        digits = 16
    )
    cat(sprintf(
        "Offline CFtime differential: %d comparisons, %d documented differences; bounds preserved.\n",
        length(results),
        report$expected_differences
    ))
}

cf_diff__main(commandArgs(trailingOnly = TRUE))
