#' @include component-temperature-epw.R signal-adjustment.R
NULL

# The eight daily bias-adjustment kernels share one temperature comparison
# boundary. Each backend selects only its signal component; every surrounding
# calendar, reconstruction, physical, and output stage remains identical.
DAILY_ADJUSTMENT_METHOD_COMPONENTS <- c(
    linear_scaling = "linear_scaling_daily",
    delta_change = "delta_change_daily",
    quantile_mapping = "quantile_mapping_daily",
    quantile_delta_mapping = "quantile_delta_mapping_daily",
    scaled_distribution_mapping = "scaled_distribution_mapping_daily",
    cdf_transform = "cdf_transform_daily",
    equidistant_cdf_matching = "equidistant_cdf_matching_daily",
    isimip3basd = "isimip3basd_daily"
)

# Backend identifiers remain stable execution adapters and do not redefine the
# statistical methods named by DAILY_ADJUSTMENT_METHOD_COMPONENTS.
DAILY_ADJUSTMENT_BACKENDS <- stats::setNames(
    paste0("daily_adjustment_", names(DAILY_ADJUSTMENT_METHOD_COMPONENTS)),
    names(DAILY_ADJUSTMENT_METHOD_COMPONENTS)
)

# All daily temperature comparisons require the same three climate roles and
# preserve humidity through the shared physical layer.
EPW_MORPH_DAILY_ADJUSTMENT_RULES <- data.table::data.table(
    step = c("tdb", "rh", "tdew"),
    epw_field = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "dew_point_temperature"
    ),
    variable_id = c("tas", NA_character_, NA_character_),
    optional_variable_id = NA_character_,
    method = c("daily_adjustment", "derived", "derived"),
    required = c(TRUE, FALSE, FALSE),
    derived = c(FALSE, TRUE, TRUE),
    method_choices = list(
        unname(DAILY_ADJUSTMENT_METHOD_COMPONENTS),
        "derived",
        "derived"
    )
)

# Protocol-owned adapter options are deliberately limited to representative-
# year climatology and hourly projection controls. Method settings remain in
# variable-specific signal_overrides.
DAILY_ADJUSTMENT_OPTIONS <- list(
    climatology_window_days = 31L,
    tolerance = 1e-8,
    signal_overrides = list()
)

# Validate daily comparison options without accepting a publication data
# source, study period, physical policy, or output override.
daily_adjustment__options <- function(options = NULL) {
    if (is.null(options)) {
        return(DAILY_ADJUSTMENT_OPTIONS)
    }
    checkmate::assert_list(options, names = "unique")
    unknown <- setdiff(names(options), names(DAILY_ADJUSTMENT_OPTIONS))
    if (length(unknown)) {
        cli::cli_abort(
            "Unknown daily adjustment option(s): {.val {unknown}}."
        )
    }
    resolved <- utils::modifyList(DAILY_ADJUSTMENT_OPTIONS, options)
    checkmate::assert_count(
        resolved$climatology_window_days,
        positive = TRUE
    )
    resolved$climatology_window_days <- as.integer(
        resolved$climatology_window_days
    )
    checkmate::assert_number(
        resolved$tolerance,
        lower = 0,
        finite = TRUE
    )
    resolved$signal_overrides <- pipeline__signal_overrides(resolved)
    resolved
}

# Normalize one role's daily temperature source onto the canonical signal
# table and a common degrees-Celsius unit before any statistical method runs.
daily_adjustment__temperature_table <- function(data, name) {
    climate <- temperature__daily_climate(data, name)
    climate <- climate[climate[["variable_id"]] == "tas"]
    bias__daily_table(climate, name)
}

# Prepare the same observed, historical-model, and future-model inputs for all
# eight signal methods without interpreting their method-owned settings.
daily_adjustment__preprocess_apply <- function(
    inputs,
    context,
    options
) {
    morpher__validate_context(context)
    options <- daily_adjustment__options(options)
    sources <- lapply(SIGNAL_THREE_INPUT_ROLES, function(role) {
        input <- weather__get_input(inputs, role)
        daily_adjustment__temperature_table(input@source, role)
    })
    names(sources) <- SIGNAL_THREE_INPUT_ROLES
    list(sources = sources, options = options)
}

# Preserve native CF dates and form the one aligned univariate signal group
# consumed identically by every daily bias-adjustment component.
daily_adjustment__calendar_apply <- function(
    data,
    inputs,
    context,
    options
) {
    list(signal__group(
        key = list(),
        inputs = data$sources,
        variables = "tas"
    ))
}

# Convert an absolute adjusted daily temperature series into a common 365-day
# representative climatology and express it as targets relative to the same
# baseline EPW daily means.
daily_adjustment__temperature_targets <- function(
    adjusted,
    baseline,
    window_days
) {
    if (!S7::S7_inherits(adjusted, DailyAdjustedSeries)) {
        cli::cli_abort(
            "Daily temperature comparison requires a DailyAdjustedSeries object."
        )
    }
    if (!identical(unique(adjusted@data[["variable_id"]]), "tas")) {
        cli::cli_abort(
            "Daily temperature comparison currently supports only {.val tas}."
        )
    }
    climatology <- daily__climatology(
        data.table::as.data.table(adjusted@data),
        value = "value",
        by = "variable_id",
        window_days = window_days,
        target_year_days = 365L
    )
    if (nrow(climatology) != 365L ||
        !identical(as.integer(climatology[["target_day"]]), 1:365)) {
        cli::cli_abort(
            "Adjusted daily temperature must produce one complete 365-day climatology."
        )
    }
    baseline_daily <- baseline$template[, list(
        baseline_mean = mean(.SD[["dry_bulb_temperature"]])
    ), by = "target_day", .SDcols = "dry_bulb_temperature"]
    targets <- merge(
        climatology,
        baseline_daily,
        by = "target_day",
        all.x = TRUE,
        sort = FALSE
    )
    data.table::setnames(targets, "climatology", "adjusted_mean")
    data.table::set(
        targets,
        j = "mean_delta",
        value = targets[["adjusted_mean"]] - targets[["baseline_mean"]]
    )
    data.table::set(targets, j = "minimum_delta", value = NA_real_)
    data.table::set(targets, j = "maximum_delta", value = NA_real_)
    data.table::set(targets, j = "dtr_delta", value = NA_real_)
    data.table::set(
        targets,
        j = "dtr_status",
        value = rep.int("inherited_missing_extremes", nrow(targets))
    )
    data.table::set(
        targets,
        j = "signal_output_role",
        value = rep.int(adjusted@output_role, nrow(targets))
    )
    data.table::set(
        targets,
        j = "signal_transformation",
        value = rep.int(adjusted@transformation, nrow(targets))
    )
    data.table::setorderv(targets, "target_day")
    targets[]
}

# Adapt a successful method result to the established daily temperature
# sequence contract while preserving the method's output role and provenance.
daily_adjustment__sequence_generate <- function(
    data,
    inputs,
    context,
    options
) {
    adjusted <- signal__single_value(data, "Daily bias adjustment")
    options <- daily_adjustment__options(options)
    template <- weather__get_input(inputs, "weather_template")
    baseline <- temperature__epw_template(template@source)
    list(
        baseline = baseline,
        targets = daily_adjustment__temperature_targets(
            adjusted,
            baseline,
            options$climatology_window_days
        ),
        adjusted = adjusted
    )
}

# Reuse the common POWER reconstruction while carrying the adjusted-series
# record forward for the final result provenance.
daily_adjustment__hourly_reconstruct <- function(
    data,
    inputs,
    context,
    options
) {
    reconstructed <- temperature__hourly_result(
        data,
        options,
        daily__project_temperature
    )
    reconstructed$adjusted <- data$adjusted
    reconstructed
}

# Reuse the unified specific-humidity policy and retain the selected method's
# adjusted series after the physical stage.
daily_adjustment__physics_apply <- function(
    data,
    inputs,
    context,
    options
) {
    physical <- temperature__physics_apply(
        data,
        inputs,
        context,
        options
    )
    physical$adjusted <- data$adjusted
    physical
}

# Assemble a standard representative-year result and expose the original
# adjusted daily values and common comparison protocol in result parts.
daily_adjustment__output_write <- function(
    data,
    inputs,
    context,
    options,
    stages
) {
    protocol <- protocol__get("daily_bias_adjustment_comparison")
    result <- temperature__output_write(
        data,
        inputs,
        context,
        options,
        stages
    )
    result$parts$adjusted_series <- data$adjusted@data
    result$parts$adjusted_series_metadata <- list(
        output_role = data$adjusted@output_role,
        transformation = data$adjusted@transformation,
        settings = data$adjusted@settings,
        provenance = data$adjusted@provenance,
        protocol = protocol@name,
        protocol_version = protocol@version,
        random_seed = protocol@random_seed,
        replicates = protocol@replicates,
        diagnostics = protocol@diagnostics,
        metrics = protocol@metrics
    )
    result
}

# Declare the method-neutral components surrounding all daily adjustment
# signals so complete recipes differ at the signal stage only.
daily_adjustment__component_specs <- function() {
    template <- component__input_requirement(
        "weather_template",
        representations = "epw",
        frequencies = "hour",
        calendars = "gregorian"
    )
    climate <- signal__three_role_requirements(
        "tas",
        frequencies = "day",
        calendars = CF_TIME_CALENDARS
    )
    complete_inputs <- c(
        list(weather_template = template),
        climate
    )
    list(
        preprocess = component__spec(
            name = "daily_adjustment_inputs",
            stage = "preprocess",
            label = "Daily adjustment input normalization",
            required_inputs = complete_inputs,
            input_kinds = "role_inputs",
            output_kinds = "daily_adjustment_preprocessed",
            scopes = "univariate",
            operations = list(apply = daily_adjustment__preprocess_apply)
        ),
        calendar = component__spec(
            name = "daily_adjustment_calendar_grouping",
            stage = "calendar",
            label = "Daily adjustment calendar grouping",
            required_inputs = complete_inputs,
            input_kinds = "daily_adjustment_preprocessed",
            output_kinds = "calendar_indexed_daily_series",
            scopes = "univariate",
            operations = list(apply = daily_adjustment__calendar_apply)
        ),
        sequence = component__spec(
            name = "daily_adjusted_temperature_sequence",
            stage = "sequence",
            label = "Adjusted daily temperature sequence",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_adjusted_series",
            output_kinds = "daily_temperature_sequence",
            scopes = "univariate",
            operations = list(
                generate = daily_adjustment__sequence_generate
            ),
            metadata = list(
                source_contract = "daily_adjusted_series",
                comparison_variable = "tas",
                output_contract = "daily_temperature_sequence"
            )
        ),
        hourly = component__spec(
            name = "daily_adjusted_temperature_projection",
            stage = "hourly",
            label = "Adjusted daily temperature projection",
            required_inputs = list(weather_template = template),
            input_kinds = "daily_temperature_sequence",
            output_kinds = "hourly_temperature_projected",
            scopes = "univariate",
            operations = list(
                reconstruct = daily_adjustment__hourly_reconstruct
            ),
            metadata = list(
                reconstruction = "constrained_daily_temperature",
                output_contract = "hourly_temperature_projected"
            )
        ),
        physics = component__spec(
            name = "daily_adjusted_specific_humidity_closure",
            stage = "physics",
            label = "Adjusted temperature specific-humidity closure",
            required_inputs = list(weather_template = template),
            input_kinds = "hourly_temperature_projected",
            output_kinds = "hourly_weather_closed",
            scopes = "univariate",
            operations = list(apply = daily_adjustment__physics_apply),
            metadata = list(
                physical_policies = "preserve_specific_humidity"
            )
        ),
        output = component__spec(
            name = "daily_adjusted_temperature_epw_result",
            stage = "output",
            label = "Adjusted daily temperature EPW result",
            required_inputs = list(weather_template = template),
            input_kinds = "hourly_weather_closed",
            output_kinds = "epw_morph_result",
            scopes = "univariate",
            operations = list(write = daily_adjustment__output_write),
            metadata = list(target_calendar = "epw_365_day")
        )
    )
}

# Register the shared daily adjusted-series adapter components once.
daily_adjustment__register_components <- function() {
    temperature__register_components()
    component__register_builtins(daily_adjustment__component_specs())
    invisible(NULL)
}

# Compose one complete comparison pipeline by changing only the signal
# component selected from the eight supported daily methods.
daily_adjustment__pipeline <- function(method) {
    checkmate::assert_choice(
        method,
        names(DAILY_ADJUSTMENT_METHOD_COMPONENTS)
    )
    daily_adjustment__register_components()
    component <- DAILY_ADJUSTMENT_METHOD_COMPONENTS[[method]]
    component__get("signal", component)
    pipeline__spec(list(
        preprocess = "daily_adjustment_inputs",
        calendar = "daily_adjustment_calendar_grouping",
        signal = component,
        sequence = "daily_adjusted_temperature_sequence",
        hourly = "daily_adjusted_temperature_projection",
        physics = "daily_adjusted_specific_humidity_closure",
        output = "daily_adjusted_temperature_epw_result"
    ))
}

# Construct the eight thin backends that bind a method component to one common
# comparison protocol without duplicating adapter logic.
daily_adjustment__backend_specs <- function() {
    specs <- lapply(names(DAILY_ADJUSTMENT_METHOD_COMPONENTS), function(method) {
        component <- DAILY_ADJUSTMENT_METHOD_COMPONENTS[[method]]
        EpwMorphBackend$new(
            name = DAILY_ADJUSTMENT_BACKENDS[[method]],
            label = sprintf("Daily %s temperature comparison", method),
            methods = c(tdb = component),
            method_choices = component,
            rules = data.table::copy(EPW_MORPH_DAILY_ADJUSTMENT_RULES),
            requires_reference = TRUE,
            pipeline = daily_adjustment__pipeline(method)
        )
    })
    names(specs) <- unname(DAILY_ADJUSTMENT_BACKENDS)
    specs
}
