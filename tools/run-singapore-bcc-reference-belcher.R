# Run the current epwshiftr implementation to generate Singapore future EPWs
# for BCC-CSM2-MR under SSP1-2.6 and SSP5-8.5.

# Load the source checkout when the script is run from the package root;
# otherwise fall back to the installed package.
load_current_epwshiftr <- function() {
    desc <- file.path(getwd(), "DESCRIPTION")
    is_source <- file.exists(desc) &&
        any(grepl("^Package:\\s*epwshiftr\\s*$", readLines(desc, warn = FALSE)))

    if (is_source) {
        if (!requireNamespace("pkgload", quietly = TRUE)) {
            stop(
                "Please install pkgload, or install epwshiftr first.",
                call. = FALSE
            )
        }
        pkgload::load_all(getwd(), quiet = FALSE)
    } else {
        library(epwshiftr)
    }

    invisible(TRUE)
}

# Resolve the packaged Singapore EPW fixture used by examples and smoke
# workflows; the fixture is installed with the package under inst/extdata.
find_singapore_epw <- function() {
    normalizePath(
        system.file(
            "extdata/examples/SGP_Singapore.486980_IWEC.epw",
            package = "epwshiftr",
            mustWork = TRUE
        ),
        winslash = "/",
        mustWork = TRUE
    )
}

options(timeout = max(getOption("timeout"), 1200))
load_current_epwshiftr()

out_root <- path.expand("~/Downloads/epwshiftr-test")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

run <- shift_future_epw(
    epw = system.file(
        "extdata/examples/SGP_Singapore.486980_IWEC.epw",
        package = "epwshiftr"
    ),
    climate = shift_cmip6(
        model = "BCC-CSM2-MR",
        scenarios = c("ssp126", "ssp585")
    ),
    periods = list(`2060s` = 2055:2065),
    method = belcher(
        reference = historical_reference(1995:2014)
    ),
    dir = out_root,
    store = out_root
)

print(shift_outputs(run)[,
    intersect(
        c(
            "source_id",
            "experiment_id",
            "variant_label",
            "period",
            "path",
            "export_path"
        ),
        names(shift_outputs(run))
    ),
    with = FALSE
])
