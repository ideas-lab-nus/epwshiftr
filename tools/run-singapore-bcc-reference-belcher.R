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
            stop("Please install pkgload, or install epwshiftr first.", call. = FALSE)
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
options(epwshiftr.dir_cache = file.path(out_root, "cache"))

outputs <- shift_future_epw(
    baseline = find_singapore_epw(),
    source = "BCC-CSM2-MR",
    scenario = c("ssp126", "ssp585"),
    member = "r1i1p1f1",
    years = 2055:2065,
    period_name = "2060s",
    store = file.path(out_root, "store-ceda"),
    output = out_root,
    recipe = "belcher",
    reference_years = 1995:2014,
    frequency = "mon",
    table_id = "Amon",
    grid_label = "gn",
    index_node = "https://esgf-data.dkrz.de",
    morph = list(strict = FALSE),
    resume = TRUE
)

print(shift_outputs(outputs)[, intersect(
    c("source_id", "experiment_id", "variant_label", "period", "path", "export_path"),
    names(shift_outputs(outputs))
), with = FALSE])
