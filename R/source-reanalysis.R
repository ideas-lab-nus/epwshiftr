#' @include utils.R
NULL

# Reanalysis source contracts -------------------------------------------------

REANALYSIS__FREQUENCIES <- c("hour", "day", "mon")
REANALYSIS__ACCESS <- c("auto", "arco", "cds")

# Normalize equivalent longitude representations before provider requests,
# distance calculations, and persistent identity construction.
reanalysis__longitude <- function(longitude) {
    normalized <- (as.numeric(longitude) + 180) %% 360 - 180
    normalized[normalized == -180 & as.numeric(longitude) > 0] <- 180
    normalized
}

# ShiftReanalysisSpec records provider-neutral observational source intent.
# Credentials are deliberately excluded so persisted plans and console output
# remain safe to share.
ShiftReanalysisSpec <- S7::new_class(
    "ShiftReanalysisSpec",
    properties = list(
        provider = S7::new_property(S7::class_character),
        dataset = S7::new_property(S7::class_character),
        product = S7::new_property(S7::class_character),
        years = S7::new_property(S7::class_integer),
        variables = S7::new_property(S7::class_any, default = NULL),
        frequency = S7::new_property(S7::class_any, default = NULL),
        access = S7::new_property(S7::class_character),
        options = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        for (property in c("provider", "dataset", "product", "access")) {
            value <- S7::prop(self, property)
            if (length(value) != 1L || is.na(value) || !nzchar(value)) {
                return(sprintf("`%s` must be one non-empty string.", property))
            }
        }
        if (!length(self@years) || anyNA(self@years) ||
            anyDuplicated(self@years)) {
            return("`years` must contain unique, non-missing years.")
        }
        if (!is.null(self@variables) &&
            (!is.character(self@variables) || !length(self@variables) ||
                anyNA(self@variables) || any(!nzchar(self@variables)) ||
                anyDuplicated(self@variables))) {
            return("`variables` must be NULL or unique, non-empty CF variable IDs.")
        }
        if (!is.null(self@frequency)) {
            frequency <- unlist(self@frequency, use.names = TRUE)
            if (!is.character(frequency) || !length(frequency) ||
                anyNA(frequency) || any(!frequency %in% REANALYSIS__FREQUENCIES)) {
                return("`frequency` must use `hour`, `day`, or `mon`.")
            }
            if (length(frequency) > 1L &&
                (is.null(names(frequency)) || any(!nzchar(names(frequency))) ||
                    anyDuplicated(names(frequency)))) {
                return("A variable-specific `frequency` must be uniquely named.")
            }
        }
        if (!self@access %in% REANALYSIS__ACCESS) {
            return("`access` must be `auto`, `arco`, or `cds`.")
        }
        if (length(self@options) &&
            (is.null(names(self@options)) || any(!nzchar(names(self@options))) ||
                anyDuplicated(names(self@options)))) {
            return("`options` must be uniquely named.")
        }
        NULL
    }
)

# Return the registered reanalysis products without exposing credentials or
# provider transport details to method code.
reanalysis__registry <- function() {
    list(
        era5 = list(
            provider = "copernicus",
            products = c("single_levels", "land"),
            available = TRUE,
            replacement = NULL
        ),
        era6 = list(
            provider = "copernicus",
            products = "single_levels",
            available = FALSE,
            replacement = NULL
        )
    )
}

# Construct one validated reanalysis source specification for a public
# provider-specific wrapper.
reanalysis__spec <- function(
    dataset,
    years,
    product,
    variables = NULL,
    frequency = NULL,
    access = "auto",
    options = list()
) {
    checkmate::assert_string(dataset, min.chars = 1L)
    years <- sort(unique(as.integer(years)))
    if (!length(years) || anyNA(years) || any(years < 1800L | years > 2300L)) {
        cli::cli_abort("`years` must contain valid calendar years.")
    }
    checkmate::assert_string(product, min.chars = 1L)
    checkmate::assert_character(
        variables,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = TRUE
    )
    if (!is.null(frequency)) {
        frequency_names <- names(frequency)
        frequency <- as.character(unlist(frequency, use.names = TRUE))
        names(frequency) <- frequency_names
    }
    access <- match.arg(access, REANALYSIS__ACCESS)
    checkmate::assert_list(options, names = "unique")

    ShiftReanalysisSpec(
        provider = "copernicus",
        dataset = dataset,
        product = product,
        years = years,
        variables = variables,
        frequency = frequency,
        access = access,
        options = options
    )
}

# Serialize only reproducible source intent. Authentication is read again in
# the executing process and therefore cannot leak through a plan specification.
reanalysis__spec_value <- function(spec) {
    if (!S7::S7_inherits(spec, ShiftReanalysisSpec)) {
        cli::cli_abort("`spec` must be a {.cls ShiftReanalysisSpec}.")
    }
    list(
        mode = "reanalysis",
        role = "observed_reference",
        dataset = spec@dataset,
        years = as.integer(spec@years),
        product = spec@product,
        variables = spec@variables,
        frequency = if (!is.null(names(spec@frequency))) {
            as.list(spec@frequency)
        } else {
            spec@frequency
        },
        access = spec@access,
        options = spec@options
    )
}

# Reconstruct a supported reanalysis source from persisted scientific intent.
reanalysis__from_spec <- function(spec) {
    dataset <- as.character(spec$dataset)
    arguments <- c(list(
        years = as.integer(unlist(spec$years, use.names = FALSE)),
        product = as.character(spec$product),
        variables = if (is.null(spec$variables)) {
            NULL
        } else {
            as.character(unlist(spec$variables, use.names = FALSE))
        },
        frequency = if (is.null(spec$frequency)) {
            NULL
        } else {
            unlist(spec$frequency, use.names = TRUE)
        },
        access = as.character(spec$access)
    ), shift_coalesce(spec$options, list()))
    if (identical(dataset, "era5")) {
        return(do.call(shift_era5, arguments))
    }
    if (identical(dataset, "era6")) {
        return(do.call(shift_era6, arguments))
    }
    cli::cli_abort("Unsupported persisted reanalysis dataset: {.val {dataset}}.")
}

# Present source intent without printing provider credentials or endpoint
# overrides that could contain user-specific information.
S7::method(print, ShiftReanalysisSpec) <- function(x, ...) {
    esg__print_header("Reanalysis Source")
    esg__print_facts(list(
        "Dataset" = toupper(x@dataset),
        "Product" = x@product,
        "Years" = sprintf("%d-%d", min(x@years), max(x@years)),
        "Variables" = if (is.null(x@variables)) {
            "inferred from weather method"
        } else {
            paste(x@variables, collapse = ", ")
        },
        "Frequency" = if (is.null(x@frequency)) {
            "inferred from weather method"
        } else {
            paste(x@frequency, collapse = ", ")
        },
        "Access" = x@access
    ))
    invisible(x)
}
