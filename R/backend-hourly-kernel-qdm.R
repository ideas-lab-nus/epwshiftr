#' @include component-hourly-kqdm-input.R weather-pipeline.R
NULL

# Hourly kernel-QDM backend
# The complete workflow follows the six climate variables used by the
# published hourly KDE-QDM weather generation path. Dew point and direct-normal
# radiation are derived later by the common EPW physical policy.
EPW_MORPH_HOURLY_KQDM_VARIABLES <- c(
    HOURLY_KQDM_SIGNAL_VARIABLES
)

# Model extraction resolves the raw CMIP variables separately from the
# canonical signal variables exposed by the backend rules.
EPW_MORPH_HOURLY_KQDM_MODEL_VARIABLES <- HOURLY_KQDM_MODEL_VARIABLES

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

# Validate reconstruction settings and signal-profile overrides. Variable-specific
# numerical settings remain owned by the registered signal component.
# hourly_kqdm__options {{{
hourly_kqdm__options <- function(options = NULL) {
    if (is.null(options)) {
        return(list(signal_overrides = list()))
    }
    checkmate::assert_list(options, names = "unique")
    unknown <- setdiff(
        names(options),
        c("signal_overrides", "include_longwave", "model_utc_offset_hours")
    )
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown hourly kernel QDM option(s): {.val {unknown}}."
        )
    }
    include_longwave <- isTRUE(options$include_longwave)
    if (!is.null(options$include_longwave)) {
        checkmate::assert_flag(options$include_longwave)
    }
    result <- list(signal_overrides = pipeline__signal_overrides(options))
    # Omit the disabled option so existing serialized recipes retain their ID.
    if (include_longwave) {
        result$include_longwave <- TRUE
    }
    if (!is.null(options$model_utc_offset_hours)) {
        checkmate::assert_number(
            options$model_utc_offset_hours,
            lower = -12,
            upper = 14,
            finite = TRUE
        )
        if (options$model_utc_offset_hours != 0) {
            result$model_utc_offset_hours <- options$model_utc_offset_hours
        }
    }
    result
}

# Extend a configured recipe's input contract without mutating the registered
# six-signal publication adapter or affecting another transform in the process.
hourly_kqdm__longwave_spec <- function(spec) {
    requirements <- spec@required_inputs
    for (role in c("observed_reference", "model_historical", "model_future")) {
        requirement <- requirements[[role]]
        requirement@variable_sets <- lapply(
            requirement@variable_sets,
            function(variables) unique(c(variables, "rlds"))
        )
        if (role != "observed_reference") {
            requirement@variable_frequencies$rlds <- "3hr"
        }
        requirements[[role]] <- requirement
    }
    spec@required_inputs <- requirements
    spec@source$longwave_extension <- list(
        variable = "rlds",
        interpolation = "constant_interval_mean",
        correction = "multiplicative_kernel_qdm",
        evidence = "experimental_package_extension"
    )
    spec
}
# }}}

# Register every already-independent component needed by the complete hourly
# workflow while preserving process-local replacements under the same keys.
# hourly_kqdm__register_components {{{
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
# }}}

# Compose the implemented interpolation, distribution correction, calendar,
# physical, and output stages into one executable future-weather pipeline.
# hourly_kqdm__pipeline {{{
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

# vim: fdm=marker :
