#' @include weather-protocol.R
NULL

# Study preset status distinguishes a documented publication configuration
# from one that the package can execute without manual source preparation.
STUDY_PRESET_STATUSES <- c("documented", "executable")

# Publication presets are optional and are never selected implicitly by a
# method or comparison protocol.
STUDY_PRESET_DEFAULTS <- c(
    "belcher_2005",
    "eames_2024",
    "ek_2018",
    "arima_2024",
    "sobie_curry_2025",
    "wang_2023"
)

# Study configuration records live separately from method and protocol
# registries so data-availability substitutions cannot become algorithm rules.
STUDY_PRESET_REGISTRY <- new.env(parent = emptyenv())

# StudyPreset records publication-specific sources, periods, and overrides.
# It contains no executable function and cannot alter a method unless a caller
# explicitly selects and applies it.
StudyPreset <- S7::new_class(
    "StudyPreset",
    properties = list(
        name = S7::new_property(S7::class_character),
        version = S7::new_property(S7::class_integer),
        label = S7::new_property(S7::class_character),
        method = S7::new_property(S7::class_character),
        protocol = S7::new_property(S7::class_character),
        citation = S7::new_property(S7::class_character),
        data_source = S7::new_property(S7::class_list),
        periods = S7::new_property(S7::class_list),
        method_overrides = S7::new_property(
            S7::class_list,
            default = list()
        ),
        protocol_overrides = S7::new_property(
            S7::class_list,
            default = list()
        ),
        status = S7::new_property(S7::class_character),
        notes = S7::new_property(
            S7::class_character,
            default = character()
        )
    ),
    validator = function(self) {
        if (length(self@name) != 1L ||
            is.na(self@name) ||
            !grepl("^[a-z][a-z0-9_]*$", self@name)) {
            return("`name` must be one lower snake_case preset identifier.")
        }
        if (length(self@version) != 1L ||
            is.na(self@version) ||
            self@version < 1L) {
            return("`version` must be one positive integer.")
        }
        for (property in c("label", "method", "protocol", "citation")) {
            value <- S7::prop(self, property)
            if (length(value) != 1L || is.na(value) || !nzchar(value)) {
                return(sprintf("`%s` must be one non-empty string.", property))
            }
        }
        for (property in c(
            "data_source", "periods", "method_overrides",
            "protocol_overrides"
        )) {
            value <- S7::prop(self, property)
            if (length(value) &&
                (is.null(names(value)) || any(!nzchar(names(value))) ||
                    anyDuplicated(names(value)))) {
                return(sprintf("`%s` must be a uniquely named list.", property))
            }
        }
        if (length(self@status) != 1L ||
            is.na(self@status) ||
            !self@status %in% STUDY_PRESET_STATUSES) {
            return("`status` must be `documented` or `executable`.")
        }
        if (anyNA(self@notes) || any(!nzchar(self@notes)) ||
            anyDuplicated(self@notes)) {
            return("`notes` must contain unique, non-empty values.")
        }
        NULL
    }
)

# Construct one optional publication preset after resolving its independent
# method and protocol identifiers.
study__preset <- function(
    name,
    label,
    method,
    protocol,
    citation,
    data_source = list(),
    periods = list(),
    method_overrides = list(),
    protocol_overrides = list(),
    status = "documented",
    notes = character(),
    version = 1L
) {
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_count(version, positive = TRUE)
    checkmate::assert_string(label, min.chars = 1L)
    checkmate::assert_string(method, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_string(protocol, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_string(citation, min.chars = 1L)
    method__get(method)
    protocol__get(protocol)
    for (value in list(
        data_source,
        periods,
        method_overrides,
        protocol_overrides
    )) {
        checkmate::assert_list(value, names = "unique")
    }
    checkmate::assert_choice(status, STUDY_PRESET_STATUSES)
    notes <- weather__descriptor_values(notes, "notes")

    StudyPreset(
        name = name,
        version = as.integer(version),
        label = label,
        method = method,
        protocol = protocol,
        citation = citation,
        data_source = data_source,
        periods = periods,
        method_overrides = method_overrides,
        protocol_overrides = protocol_overrides,
        status = status,
        notes = notes
    )
}

# Retain the exact GCM/member inventory and archive substitutions reported by
# Wang et al. as study metadata rather than hourly KQDM algorithm behavior.
study__wang_source_manifest <- function() {
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
        )
    )
}

# Build publication presets as optional validation records. Presets that still
# require source preparation or unpublished implementation details remain
# explicitly documented rather than being presented as executable workflows.
study__default_presets <- function() {
    list(
        belcher_2005 = study__preset(
            name = "belcher_2005",
            label = "Belcher et al. 2005 configuration",
            method = "belcher_monthly",
            protocol = "monthly_morphing_comparison",
            citation = "https://doi.org/10.1191/0143624405bt112oa",
            data_source = list(source_family = "publication_climate_scenarios"),
            status = "documented",
            notes = "Use for publication-oriented validation, not as the default comparison protocol."
        ),
        eames_2024 = study__preset(
            name = "eames_2024",
            label = "Eames et al. 2024 configuration",
            method = "eames_monthly_temperature",
            protocol = "daily_temperature_comparison",
            citation = "https://doi.org/10.1177/01436244231218861",
            data_source = list(
                product = "UKCP18",
                temporal_information = "monthly change factors"
            ),
            status = "documented",
            notes = c(
                "The publication does not use daily CMIP6 as its future signal.",
                "The package method can be compared under a common protocol without selecting this preset."
            )
        ),
        ek_2018 = study__preset(
            name = "ek_2018",
            label = "Ek et al. 2018 configuration",
            method = "ek_daily_factors",
            protocol = "daily_temperature_comparison",
            citation = paste0(
                "https://dspace.library.uvic.ca/items/",
                "5e8e6684-c704-4d2e-8480-2c81bdbafde9"
            ),
            status = "documented",
            notes = "The unavailable original Matlab implementation prevents byte-for-byte reproduction."
        ),
        arima_2024 = study__preset(
            name = "arima_2024",
            label = "Arima et al. 2024 configuration",
            method = "monthly_percentile_temperature",
            protocol = "daily_temperature_comparison",
            citation = "https://doi.org/10.69357/asim2024.1178",
            status = "documented",
            notes = "The published application uses a model-specific Japanese case."
        ),
        sobie_curry_2025 = study__preset(
            name = "sobie_curry_2025",
            label = "Sobie and Curry 2025 configuration",
            method = "sobie_curry_daily",
            protocol = "daily_temperature_comparison",
            citation = "https://doi.org/10.1016/j.dib.2025.111667",
            data_source = list(project = "CMIP6", region = "Canada"),
            status = "documented"
        ),
        wang_2023 = study__preset(
            name = "wang_2023",
            label = "Wang et al. 2023 configuration",
            method = "kernel_quantile_delta_mapping_hourly",
            protocol = "hourly_direct_model_comparison",
            citation = "https://doi.org/10.1038/s41467-023-41458-5",
            data_source = list(
                project = "CMIP6",
                manifest = study__wang_source_manifest()
            ),
            protocol_overrides = list(
                source_frequency_substitutions = "publication_manifest"
            ),
            status = "documented",
            notes = c(
                "Pressure, wind, and radiation substitutions are publication data-preparation rules.",
                "The substitutions are not part of kernel QDM and are not applied by default."
            )
        )
    )
}

# Register one study preset without allowing publication metadata to replace a
# stable identifier silently.
study__register <- function(
    preset,
    overwrite = FALSE,
    registry = STUDY_PRESET_REGISTRY
) {
    if (!S7::S7_inherits(preset, StudyPreset)) {
        cli::cli_abort("{.arg preset} must be a StudyPreset object.")
    }
    checkmate::assert_flag(overwrite)
    checkmate::assert_environment(registry)
    if (exists(preset@name, envir = registry, inherits = FALSE) &&
        !isTRUE(overwrite)) {
        cli::cli_abort(
            "Study preset {.val {preset@name}} is already registered."
        )
    }
    assign(preset@name, preset, envir = registry)
    invisible(preset)
}

# Populate the optional built-in publication presets once.
study__register_defaults <- function() {
    registered <- ls(envir = STUDY_PRESET_REGISTRY, all.names = FALSE)
    if (all(STUDY_PRESET_DEFAULTS %in% registered)) {
        return(invisible(NULL))
    }
    for (preset in study__default_presets()) {
        if (!exists(
            preset@name,
            envir = STUDY_PRESET_REGISTRY,
            inherits = FALSE
        )) {
            study__register(preset)
        }
    }
    invisible(NULL)
}

# Retrieve one optional publication preset and optionally enforce its catalog
# version.
study__get <- function(
    name,
    version = NULL,
    registry = STUDY_PRESET_REGISTRY
) {
    checkmate::assert_string(name, pattern = "^[a-z][a-z0-9_]*$")
    checkmate::assert_environment(registry)
    if (identical(registry, STUDY_PRESET_REGISTRY)) {
        study__register_defaults()
    }
    name <- tolower(name)
    if (!exists(name, envir = registry, inherits = FALSE)) {
        cli::cli_abort("Unknown future-weather study preset: {.val {name}}.")
    }
    preset <- get(name, envir = registry, inherits = FALSE)
    if (!is.null(version)) {
        checkmate::assert_count(version, positive = TRUE)
        if (!identical(preset@version, as.integer(version))) {
            cli::cli_abort(
                "Study preset {.val {name}} requires definition version {preset@version}; persisted version is {as.integer(version)}."
            )
        }
    }
    preset
}

# Return publication configuration metadata without selecting or executing any
# preset implicitly.
study__list <- function(registry = STUDY_PRESET_REGISTRY) {
    checkmate::assert_environment(registry)
    if (identical(registry, STUDY_PRESET_REGISTRY)) {
        study__register_defaults()
    }
    names <- sort(ls(envir = registry, all.names = FALSE))
    data.table::rbindlist(lapply(names, function(name) {
        preset <- get(name, envir = registry, inherits = FALSE)
        data.table::data.table(
            name = preset@name,
            version = preset@version,
            label = preset@label,
            method = preset@method,
            protocol = preset@protocol,
            citation = preset@citation,
            data_source = list(preset@data_source),
            periods = list(preset@periods),
            method_overrides = list(preset@method_overrides),
            protocol_overrides = list(preset@protocol_overrides),
            status = preset@status,
            notes = list(preset@notes)
        )
    }), use.names = TRUE, fill = TRUE)
}

#' Inspect optional publication study presets
#'
#' Study presets record paper-specific data and settings separately from method
#' definitions and shared comparison protocols. They are never applied by
#' default.
#'
#' @return A data table with one row per registered study preset.
#'
#' @seealso [epw_morph_methods()], [epw_morph_protocols()]
#' @export
epw_morph_study_presets <- function() {
    study__list()
}

#' Get an optional publication study preset
#'
#' @param name Stable preset name returned by [epw_morph_study_presets()].
#'
#' @return A `StudyPreset` object.
#'
#' @seealso [epw_morph_study_presets()]
#' @export
epw_morph_study_preset <- function(name) {
    study__get(name)
}
