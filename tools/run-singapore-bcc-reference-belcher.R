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
    epw <- system.file(
        "extdata/examples/SGP_Singapore.486980_IWEC.epw",
        package = "epwshiftr",
        mustWork = TRUE
    )

    normalizePath(epw, winslash = "/", mustWork = TRUE)
}

# Print a timestamped progress message so long ESGF phases are easy to locate.
say <- function(...) {
    message(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...)))
}

options(timeout = max(getOption("timeout"), 1200))
load_current_epwshiftr()

out_root <- path.expand("~/Downloads/epwshiftr-test")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
store_root <- file.path(out_root, "store-ceda")
dir.create(store_root, recursive = TRUE, showWarnings = FALSE)

options(epwshiftr.dir_cache = file.path(out_root, "cache"))

baseline_epw <- find_singapore_epw()
say("Baseline EPW: ", baseline_epw)

model <- "BCC-CSM2-MR"
scenarios <- c("ssp126", "ssp585")
variant <- "r1i1p1f1"
grid_label <- "gn"

future_years <- 2055:2065
reference_years <- 1995:2014

future_time <- c(
    sprintf("%d-01-01T00:00:00Z", min(future_years)),
    sprintf("%d-12-31T23:59:59Z", max(future_years))
)

reference_periods <- epw_morph_periods(reference = reference_years)
future_periods <- epw_morph_periods(`2060s` = future_years)

recipe <- epw_morph_recipe("belcher")
variables <- epw_morph_variables(recipe)

site <- shift_site(
    id = "SIN",
    label = "singapore",
    epw = baseline_epw
)

request <- shift_request(
    provider = "esgf",
    project = "CMIP6",
    source = model,
    experiment = scenarios,
    variant = variant,
    variables = variables,
    frequency = "mon",
    time = future_time,
    filters = list(
        activity_id = "ScenarioMIP",
        table_id = "Amon",
        grid_label = grid_label,
        data_node = "esgf.ceda.ac.uk"
    ),
    options = list(index_node = "https://esgf-data.dkrz.de")
)

say("Collecting future File records")
files <- shift_collect(
    request,
    store = store_root,
    label = "singapore-bcc-csm2-mr-ssp126-ssp585-2055-2065"
)
say("Future collect status: ", shift_status(files))

say("Extracting future climate data")
extracted <- shift_extract(
    files,
    site = site,
    periods = future_periods,
    variables = variables,
    fallback = "auto",
    resume = TRUE
)
say("Future extract status: ", shift_status(extracted))
invisible(shift_check(extracted, strict = TRUE))
future_coverage <- shift_coverage(extracted)
future_plan_id <- unique(future_coverage$plan_id[
    future_coverage$complete %in% TRUE &
        future_coverage$experiment_id %in% scenarios
])
if (!length(future_plan_id)) {
    stop("No complete future extraction plans are available.", call. = FALSE)
}
say("Complete future extraction plans: ", length(future_plan_id))

reference_request <- shift_request(
    provider = "esgf",
    project = "CMIP6",
    source = model,
    experiment = "historical",
    variant = variant,
    variables = variables,
    frequency = "mon",
    filters = list(
        activity_id = "CMIP",
        table_id = "Amon",
        grid_label = grid_label,
        data_node = "esgf.ceda.ac.uk"
    ),
    options = list(index_node = "https://esgf-data.dkrz.de")
)

say("Collecting historical reference File records")
reference_files <- shift_collect(
    reference_request,
    store = store_root,
    label = "singapore-bcc-csm2-mr-historical-reference"
)
say("Reference collect status: ", shift_status(reference_files))

say("Extracting historical reference climate data")
reference <- shift_extract(
    reference_files,
    site = site,
    periods = reference_periods,
    variables = variables,
    fallback = "auto",
    resume = TRUE
)
say("Reference extract status: ", shift_status(reference))
reference_coverage <- shift_coverage(reference)
reference_plan_id <- unique(reference_coverage$plan_id[
    reference_coverage$complete %in% TRUE &
        reference_coverage$experiment_id %in% "historical"
])
if (!length(reference_plan_id)) {
    stop("No complete historical reference extraction plans are available.", call. = FALSE)
}
say("Complete reference extraction plans: ", length(reference_plan_id))

store <- shift_store(store_root, create = FALSE)
morpher <- epw_morpher(
    store = store,
    epw = baseline_epw,
    site_id = site@id,
    recipe = recipe,
    label = site@label
)

say("Morphing and writing EPW files with historical reference")
workflow <- morpher$workflow(
    plan_id = future_plan_id,
    periods = future_periods,
    reference_plan_id = reference_plan_id,
    reference_periods = reference_periods,
    by = c("source_id", "experiment_id", "variant_label", "period"),
    strict = FALSE,
    dir = "future-epw",
    separate = TRUE,
    resume = TRUE
)

outputs <- workflow$outputs
path_col <- intersect(c("path", "output_path", "relative_path"), names(outputs))[[1L]]
outputs$absolute_path <- normalizePath(
    file.path(store_root, outputs[[path_col]]),
    winslash = "/",
    mustWork = TRUE
)

say("Generated EPW files")
print(outputs[, intersect(
    c("source_id", "experiment_id", "variant_label", "period", "absolute_path"),
    names(outputs)
), with = FALSE])
