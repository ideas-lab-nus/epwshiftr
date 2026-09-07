#' @include component-hourly-kqdm-input.R weather-pipeline.R
NULL

# Hourly kernel-QDM backend {{{

# The complete workflow follows the six climate variables used by the
# published hourly KDE-QDM weather generation path. Dew point and direct-normal
# radiation are derived later by the common EPW physical policy.
EPW_MORPH_HOURLY_KQDM_VARIABLES <- c(
    HOURLY_KQDM_SIGNAL_VARIABLES
)

# Model extraction resolves the raw CMIP variables separately from the
# canonical signal variables exposed by the backend rules.
EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES <- HOURLY_KQDM_MODEL_VARIABLES

# Record the ten model-member identities and archive substitutions reported in
# Supplementary Table 4 and Supplementary Method 3 of Wang et al. (2023).
hourly_kqdm__source_manifest <- function() {
    data.table::data.table(
        source_id = c(
            "ACCESS-CM2",
            "BCC-CSM2-MR",
            "CanESM5",
            "CMCC-CM2-SR5",
            "CMCC-ESM2",
            "FGOALS-g3",
            "GISS-E2-1-G",
            "IITM-ESM",
            "KACE-1-0-G",
            "MRI-ESM2-0"
        ),
        variant_label = c(
            "r1i1p1f1",
            "r1i1p1f1",
            "r1i1p2f1",
            "r1i1p1f1",
            "r1i1p1f1",
            "r3i1p1f1",
            "r1i1p1f2",
            "r1i1p1f1",
            "r1i1p1f1",
            "r1i1p1f1"
        ),
        nominal_resolution_km = c(
            250L, 100L, 500L, 100L, 100L,
            250L, 250L, 250L, 250L, 100L
        ),
        published_frequencies = c(
            "3hr,day",
            "3hr,day",
            "3hr,6hr,day",
            "3hr",
            "3hr,day",
            "3hr,6hr,day",
            "3hr,day",
            "3hr,6hr,day",
            "3hr,day",
            "3hr,day"
        ),
        special_treatment = c(
            NA_character_,
            NA_character_,
            paste(
                "Use 6-hourly ps for ssp245, ssp370, and ssp585",
                "when 3-hourly ps is unavailable"
            ),
            NA_character_,
            NA_character_,
            paste(
                "Use 6-hourly sfcWind plus lowest-model-level ua and va",
                "to reconstruct wind direction"
            ),
            paste(
                "Correct rsdsdiff after download using the 3-hourly",
                "cosine of the solar zenith angle"
            ),
            paste(
                "Use psl at the highest available temporal resolution",
                "and convert it to station surface pressure"
            ),
            NA_character_,
            NA_character_
        ),
        reference = rep.int(
            "https://doi.org/10.1038/s41467-023-41458-5",
            10L
        )
    )
}

EPW_MORPH_HOURLY_KQDM_METHODS <- c(
    tdb = "kernel_quantile_delta_mapping",
    pressure = "kernel_quantile_delta_mapping",
    rh = "kernel_quantile_delta_mapping",
    wind_speed = "kernel_quantile_delta_mapping",
    ghi = "kernel_quantile_delta_mapping",
    dhi = "kernel_quantile_delta_mapping"
)

# Backend rules expose the climate variables needed by extraction and retain
# the distinction between corrected source variables and derived EPW fields.
EPW_MORPH_HOURLY_KQDM_RULES <- data.table::data.table(
    step = c(
        names(EPW_MORPH_HOURLY_KQDM_METHODS),
        "tdew",
        "dni"
    ),
    epw_field = c(
        "dry_bulb_temperature",
        "atmospheric_pressure",
        "relative_humidity",
        "wind_speed",
        "global_horizontal_radiation",
        "diffuse_horizontal_radiation",
        "dew_point_temperature",
        "direct_normal_radiation"
    ),
    variable_id = c(
        EPW_MORPH_HOURLY_KQDM_VARIABLES,
        NA_character_,
        NA_character_
    ),
    optional_variable_id = NA_character_,
    method = c(
        unname(EPW_MORPH_HOURLY_KQDM_METHODS),
        "derived",
        "derived"
    ),
    required = c(
        rep.int(TRUE, length(EPW_MORPH_HOURLY_KQDM_VARIABLES)),
        FALSE,
        FALSE
    ),
    derived = c(
        rep.int(FALSE, length(EPW_MORPH_HOURLY_KQDM_VARIABLES)),
        TRUE,
        TRUE
    ),
    method_choices = c(
        as.list(unname(EPW_MORPH_HOURLY_KQDM_METHODS)),
        list("derived", "derived")
    )
)

# Complete and validate the only backend-level option. Variable-specific
# numerical settings remain owned by the registered signal component.
hourly_kqdm__options <- function(options = NULL) {
    if (is.null(options)) {
        return(list(signal_overrides = list()))
    }
    checkmate::assert_list(options, names = "unique")
    unknown <- setdiff(names(options), "signal_overrides")
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown hourly kernel QDM option(s): {.val {unknown}}."
        )
    }
    list(signal_overrides = pipeline__signal_overrides(options))
}

# Register every already-independent component needed by the complete hourly
# workflow while preserving process-local replacements under the same keys.
hourly_kqdm__register_components <- function() {
    hourly_kqdm_input__register_component()
    hourly_calendar__register_component()
    kqdm__register_component()
    sequence__register_direct_model_component()
    hourmap__register_component()
    direct_epw__register_component()
    sequence__register_epw_output_component()
    invisible(NULL)
}

# Compose the implemented interpolation, distribution correction, calendar,
# physical, and output stages into one executable future-weather pipeline.
hourly_kqdm__pipeline <- function() {
    hourly_kqdm__register_components()
    pipeline__spec(list(
        preprocess = "hourly_kernel_qdm_input_preparation",
        calendar = "hourly_calendar_grouping",
        signal = "kernel_quantile_delta_mapping_hourly",
        sequence = "direct_model_realization",
        hourly = "direct_model_epw_calendar_mapping",
        physics = "epw_hourly_physical_closure",
        output = "direct_model_epw_result"
    ))
}

# }}}
