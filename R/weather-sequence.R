# Direct model realizations use one stable identifier because ensemble-member
# identity already belongs to the surrounding morphing case.
DIRECT_MODEL_SEQUENCE_ID <- "direct-model"

# Return a deterministic identity for one aligned signal group without
# serializing its input payloads into the downstream sequence.
sequence__direct_group_id <- function(group) {
    if (!S7::S7_inherits(group, SignalGroup)) {
        cli::cli_abort("{.arg group} must be a SignalGroup object.")
    }
    key <- group@key[sort(names(group@key))]
    paste0(
        "group-",
        substr(
            store__hash(
                list(
                    key = key,
                    variables = sort(group@variables)
                )
            ),
            1L,
            16L
        )
    )
}

# Check whether one adjusted variable group contains exactly one complete,
# ordered native-calendar year before it enters an hourly reconstructor.
sequence__direct_year_error <- function(data, weather_year, calendar) {
    if (!identical(unique(as.integer(data[["cf_year"]])), weather_year)) {
        return("Every direct-model row must match `weather_year`.")
    }
    if (!identical(unique(data[["cf_calendar"]]), calendar)) {
        return("Every direct-model row must match the member calendar.")
    }
    expected_order <- order(
        data[["cf_day_of_year"]],
        data[["annual_phase"]],
        data[["variable_id"]],
        method = "radix"
    )
    if (!identical(expected_order, seq_len(nrow(data)))) {
        return("Direct-model rows must use native chronological order.")
    }
    for (variable in unique(data[["variable_id"]])) {
        rows <- data[data[["variable_id"]] == variable, , drop = FALSE]
        year_days <- unique(as.integer(rows[["cf_year_days"]]))
        if (length(year_days) != 1L ||
            nrow(rows) != year_days ||
            !identical(
                as.integer(rows[["cf_day_of_year"]]),
                seq_len(year_days)
            )) {
            return(sprintf(
                "Variable `%s` must cover every native-calendar day in weather year %d.",
                variable,
                weather_year
            ))
        }
    }
    NULL
}

# DirectModelSeries retains one signal group's key, variables, correction
# metadata, and calendar-native daily values after partitioning by source year.
DirectModelSeries <- S7::new_class(
    "DirectModelSeries",
    properties = list(
        group_id = S7::new_property(S7::class_character),
        key = S7::new_property(S7::class_list, default = list()),
        variables = S7::new_property(S7::class_character),
        adjusted = S7::new_property(S7::class_any)
    ),
    validator = function(self) {
        if (length(self@group_id) != 1L ||
            is.na(self@group_id) ||
            !grepl("^[a-z][a-z0-9-]*$", self@group_id)) {
            return("`group_id` must use lower-case letters, numbers, and hyphens.")
        }
        if (length(self@key) &&
            (is.null(names(self@key)) ||
                any(!nzchar(names(self@key))) ||
                anyDuplicated(names(self@key)) ||
                any(vapply(self@key, length, integer(1L)) != 1L) ||
                any(!vapply(self@key, is.atomic, logical(1L))))) {
            return("`key` must be a uniquely named list of atomic scalar values.")
        }
        if (!length(self@variables) ||
            anyNA(self@variables) ||
            any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", self@variables)) ||
            anyDuplicated(self@variables)) {
            return("`variables` must contain unique CMIP-style identifiers.")
        }
        if (!S7::S7_inherits(self@adjusted, DailyAdjustedSeries)) {
            return("`adjusted` must be a DailyAdjustedSeries object.")
        }
        if (!identical(self@adjusted@output_role, "model_future")) {
            return("Direct model realization requires a `model_future` signal output.")
        }
        if (!setequal(
            self@variables,
            unique(self@adjusted@data[["variable_id"]])
        )) {
            return("`variables` must match the adjusted series variables.")
        }
        NULL
    }
)

# DirectModelSequenceMember groups all corrected signal series belonging to
# one complete source-model year without selecting, resampling, or reordering days.
DirectModelSequenceMember <- S7::new_class(
    "DirectModelSequenceMember",
    properties = list(
        sequence_id = S7::new_property(S7::class_character),
        weather_year = S7::new_property(S7::class_integer),
        calendar = S7::new_property(S7::class_character),
        series = S7::new_property(S7::class_list),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (length(self@sequence_id) != 1L ||
            is.na(self@sequence_id) ||
            !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", self@sequence_id)) {
            return("`sequence_id` contains unsupported characters.")
        }
        if (length(self@weather_year) != 1L ||
            is.na(self@weather_year) ||
            self@weather_year < 1L) {
            return("`weather_year` must be one positive integer.")
        }
        if (length(self@calendar) != 1L ||
            is.na(self@calendar) ||
            !self@calendar %in% CF_TIME_CALENDARS) {
            return("`calendar` must identify one supported CF calendar.")
        }
        if (!length(self@series) ||
            !all(vapply(
                self@series,
                S7::S7_inherits,
                logical(1L),
                class = DirectModelSeries
            ))) {
            return("`series` must contain DirectModelSeries objects.")
        }
        group_ids <- vapply(
            self@series,
            function(item) item@group_id,
            character(1L)
        )
        if (anyDuplicated(group_ids)) {
            return("Direct-model group identities must be unique within a year.")
        }
        for (item in self@series) {
            error <- sequence__direct_year_error(
                item@adjusted@data,
                self@weather_year,
                self@calendar
            )
            if (!is.null(error)) {
                return(error)
            }
        }
        if (length(self@provenance) &&
            (is.null(names(self@provenance)) ||
                any(!nzchar(names(self@provenance))) ||
                anyDuplicated(names(self@provenance)))) {
            return("`provenance` must be a uniquely named list.")
        }
        NULL
    }
)

# DirectModelSequence is the typed intermediate exchanged between a future-
# backbone signal and a later daily-to-hourly reconstruction component.
DirectModelSequence <- S7::new_class(
    "DirectModelSequence",
    properties = list(
        members = S7::new_property(S7::class_list),
        frequency = S7::new_property(S7::class_character),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (!length(self@members) ||
            !all(vapply(
                self@members,
                S7::S7_inherits,
                logical(1L),
                class = DirectModelSequenceMember
            ))) {
            return("`members` must contain DirectModelSequenceMember objects.")
        }
        if (!identical(self@frequency, "day")) {
            return("DirectModelSequence currently requires daily values.")
        }
        years <- vapply(
            self@members,
            function(member) member@weather_year,
            integer(1L)
        )
        if (anyDuplicated(years) || !identical(years, sort(years))) {
            return("Direct-model members must use unique ascending weather years.")
        }
        sequence_ids <- vapply(
            self@members,
            function(member) member@sequence_id,
            character(1L)
        )
        calendars <- vapply(
            self@members,
            function(member) member@calendar,
            character(1L)
        )
        if (length(unique(sequence_ids)) != 1L) {
            return("Direct-model members must share one `sequence_id`.")
        }
        if (length(unique(calendars)) != 1L) {
            return("Direct-model members must share one CF calendar.")
        }
        group_ids <- lapply(self@members, function(member) {
            sort(vapply(
                member@series,
                function(item) item@group_id,
                character(1L)
            ))
        })
        if (!all(vapply(
            group_ids[-1L],
            identical,
            logical(1L),
            group_ids[[1L]]
        ))) {
            return("Every direct-model year must contain the same signal groups.")
        }
        if (length(self@provenance) &&
            (is.null(names(self@provenance)) ||
                any(!nzchar(names(self@provenance))) ||
                anyDuplicated(names(self@provenance)))) {
            return("`provenance` must be a uniquely named list.")
        }
        NULL
    }
)

# Rebuild one year slice with the original signal transformation, settings,
# and provenance while retaining only variables present in that source year.
sequence__slice_adjusted <- function(adjusted, year) {
    data <- data.table::as.data.table(
        data.table::copy(adjusted@data)
    )
    data <- data[data[["cf_year"]] == year]
    data.table::setorderv(
        data,
        c("cf_day_of_year", "annual_phase", "variable_id")
    )
    variables <- unique(data[["variable_id"]])
    bias__daily_adjusted_series(
        data,
        output_role = adjusted@output_role,
        transformation = adjusted@transformation,
        variable_metadata = adjusted@variable_metadata[variables],
        settings = adjusted@settings,
        provenance = adjusted@provenance
    )
}

# Construct one typed year member from all aligned signal groups after the
# generator has established common year and calendar coverage.
sequence__direct_member <- function(series, weather_year, calendar) {
    group_ids <- vapply(
        series,
        function(item) item@group_id,
        character(1L)
    )
    DirectModelSequenceMember(
        sequence_id = DIRECT_MODEL_SEQUENCE_ID,
        weather_year = as.integer(weather_year),
        calendar = calendar,
        series = series,
        provenance = list(
            source_role = "model_future",
            source_year = as.integer(weather_year),
            calendar = calendar,
            ordering = "native_cf_chronology",
            selection = "none",
            resampling = "none",
            group_ids = group_ids
        )
    )
}

# Preserve the corrected future-model chronology, partition it by complete CF
# year, and retain group-level signal metadata for later hourly reconstruction.
sequence__direct_model_generate <- function(
    data,
    inputs,
    context,
    options
) {
    if (!S7::S7_inherits(data, SignalExecutionResult) ||
        !length(data@groups) ||
        length(data@groups) != length(data@values)) {
        cli::cli_abort(
            "Direct model realization requires an aligned SignalExecutionResult."
        )
    }
    if (any(data@diagnostics[["status"]] != "ok") ||
        any(vapply(data@values, is.null, logical(1L)))) {
        cli::cli_abort(
            "Direct model realization cannot preserve failed signal groups."
        )
    }
    if (!all(vapply(
        data@values,
        S7::S7_inherits,
        logical(1L),
        class = DailyAdjustedSeries
    ))) {
        cli::cli_abort(
            "Direct model realization currently requires DailyAdjustedSeries values."
        )
    }

    group_ids <- vapply(
        data@groups,
        sequence__direct_group_id,
        character(1L)
    )
    if (anyDuplicated(group_ids)) {
        cli::cli_abort(
            "Direct model realization received duplicate signal-group identities."
        )
    }
    year_sets <- lapply(data@values, function(adjusted) {
        if (!identical(adjusted@output_role, "model_future")) {
            cli::cli_abort(
                "Direct model realization requires every signal output to retain `model_future`."
            )
        }
        calendars <- unique(adjusted@data[["cf_calendar"]])
        if (length(calendars) != 1L) {
            cli::cli_abort(
                "Each direct-model signal group must use one native CF calendar."
            )
        }
        sort(unique(as.integer(adjusted@data[["cf_year"]])))
    })
    if (!all(vapply(
        year_sets[-1L],
        identical,
        logical(1L),
        year_sets[[1L]]
    ))) {
        cli::cli_abort(
            "Every direct-model signal group must cover the same weather years."
        )
    }
    calendars <- vapply(
        data@values,
        function(adjusted) unique(adjusted@data[["cf_calendar"]]),
        character(1L)
    )
    if (length(unique(calendars)) != 1L) {
        cli::cli_abort(
            "Every direct-model signal group must use the same CF calendar."
        )
    }

    years <- year_sets[[1L]]
    members <- lapply(years, function(year) {
        series <- lapply(seq_along(data@values), function(index) {
            DirectModelSeries(
                group_id = group_ids[[index]],
                key = data@groups[[index]]@key,
                variables = data@groups[[index]]@variables,
                adjusted = sequence__slice_adjusted(
                    data@values[[index]],
                    year
                )
            )
        })
        sequence__direct_member(series, year, calendars[[1L]])
    })
    DirectModelSequence(
        members = members,
        frequency = "day",
        provenance = list(
            method = "direct_model_realization",
            source_role = "model_future",
            ordering = "native_cf_chronology",
            selection = "none",
            resampling = "none",
            years = years,
            calendar = calendars[[1L]],
            group_ids = group_ids
        )
    )
}

# Describe the deterministic sequence component independently of any one
# complete weather recipe or bias-adjustment implementation.
sequence__direct_model_component <- function() {
    component__spec(
        name = "direct_model_realization",
        stage = "sequence",
        label = "Direct model realization",
        required_inputs = list(
            model_future = component__input_requirement(
                "model_future",
                representations = "series",
                frequencies = "day",
                calendars = CF_TIME_CALENDARS
            )
        ),
        input_kinds = "daily_adjusted_series",
        output_kinds = "direct_model_sequence",
        scopes = "multivariate",
        stochastic = FALSE,
        operations = list(generate = sequence__direct_model_generate),
        metadata = list(
            sequence_method = "direct_model_realization",
            source_role = "model_future",
            ordering = "native_cf_chronology",
            selection = "none",
            resampling = "none",
            supported_frequencies = "day",
            output_contract = "direct_model_sequence"
        )
    )
}

# Register the reusable sequence implementation once so recipes can refer to
# its stable algorithmic name without embedding executable functions.
sequence__register_direct_model_component <- function() {
    component <- sequence__direct_model_component()
    key <- component__registry_key(component@stage, component@name)
    if (!exists(
        key,
        envir = WEATHER_COMPONENT_REGISTRY,
        inherits = FALSE
    )) {
        component__register(component)
    }
    invisible(NULL)
}

# Future-weather sequence results keep year identity outside the hourly table
# so each member can be persisted, resumed, and written as an independent EPW.
WeatherSequenceMember <- S7::new_class(
    "WeatherSequenceMember",
    properties = list(
        sequence_id = S7::new_property(S7::class_character),
        weather_year = S7::new_property(S7::class_integer),
        calendar = S7::new_property(S7::class_character),
        stochastic_seed = S7::new_property(
            S7::class_integer,
            default = NA_integer_
        ),
        data = S7::new_property(S7::class_any),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (length(self@sequence_id) != 1L ||
            is.na(self@sequence_id) ||
            !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", self@sequence_id)) {
            return(
                "`sequence_id` must contain only letters, numbers, dots, underscores, or hyphens."
            )
        }
        if (length(self@weather_year) != 1L ||
            is.na(self@weather_year) ||
            self@weather_year < 1L) {
            return("`weather_year` must be one positive integer.")
        }
        if (length(self@calendar) != 1L ||
            is.na(self@calendar) ||
            !nzchar(self@calendar)) {
            return("`calendar` must be one non-empty string.")
        }
        if (length(self@stochastic_seed) != 1L) {
            return("`stochastic_seed` must be one integer or `NA_integer_`.")
        }
        if (!is.data.frame(self@data) || !nrow(self@data)) {
            return("`data` must be a non-empty hourly weather table.")
        }
        if (!"year" %in% names(self@data)) {
            return("`data` must contain an EPW `year` column.")
        }
        years <- unique(as.integer(self@data[["year"]]))
        if (anyNA(years) ||
            length(years) != 1L ||
            !identical(years, self@weather_year)) {
            return("Every hourly row must match `weather_year`.")
        }
        if (length(self@provenance) &&
            (is.null(names(self@provenance)) ||
                any(!nzchar(names(self@provenance))) ||
                anyDuplicated(names(self@provenance)))) {
            return("`provenance` must be a uniquely named list.")
        }
        NULL
    }
)

# WeatherSequenceResult is the final backend contract for one or more explicit
# future years; existing representative-year backends keep epw_morph_result().
WeatherSequenceResult <- S7::new_class(
    "WeatherSequenceResult",
    properties = list(
        backend = S7::new_property(S7::class_character),
        recipe = S7::new_property(S7::class_list),
        epw = S7::new_property(S7::class_any),
        output_type = S7::new_property(S7::class_character),
        members = S7::new_property(S7::class_list),
        parts = S7::new_property(S7::class_list, default = list()),
        diagnostics = S7::new_property(S7::class_any, default = NULL),
        factors = S7::new_property(S7::class_any, default = NULL),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (length(self@backend) != 1L ||
            is.na(self@backend) ||
            !nzchar(self@backend)) {
            return("`backend` must be one non-empty string.")
        }
        if (!inherits(self@epw, "EpwFile")) {
            return("`epw` must be an internal EpwFile object.")
        }
        if (length(self@output_type) != 1L ||
            is.na(self@output_type) ||
            !self@output_type %in% c("future_year", "multi_year")) {
            return("`output_type` must be `future_year` or `multi_year`.")
        }
        if (!length(self@members) ||
            !all(vapply(
                self@members,
                S7::S7_inherits,
                logical(1L),
                class = WeatherSequenceMember
            ))) {
            return("`members` must contain WeatherSequenceMember objects.")
        }
        if (identical(self@output_type, "future_year") &&
            length(self@members) != 1L) {
            return("A `future_year` result must contain exactly one member.")
        }
        if (identical(self@output_type, "multi_year") &&
            length(self@members) < 2L) {
            return("A `multi_year` result must contain at least two members.")
        }
        keys <- vapply(self@members, function(member) {
            paste(member@sequence_id, member@weather_year, sep = "\r")
        }, character(1L))
        if (anyDuplicated(keys)) {
            return("Sequence member `sequence_id` and `weather_year` pairs must be unique.")
        }
        template <- self@epw$data()
        required <- setdiff(names(template), "datetime")
        valid_data <- vapply(self@members, function(member) {
            nrow(member@data) == nrow(template) &&
                all(required %in% names(member@data))
        }, logical(1L))
        if (!all(valid_data)) {
            return(
                "Every sequence member must contain a complete baseline-shaped EPW year."
            )
        }
        if (length(self@provenance) &&
            (is.null(names(self@provenance)) ||
                any(!nzchar(names(self@provenance))) ||
                anyDuplicated(names(self@provenance)))) {
            return("`provenance` must be a uniquely named list.")
        }
        NULL
    }
)

# Construct one validated sequence member after hourly reconstruction and
# calendar mapping have produced a complete EPW-compatible weather year.
sequence__member <- function(
    data,
    weather_year,
    sequence_id = "default",
    calendar = "noleap",
    stochastic_seed = NA_integer_,
    provenance = list()
) {
    checkmate::assert_data_frame(data, min.rows = 1L)
    checkmate::assert_int(weather_year, lower = 1L)
    checkmate::assert_string(
        sequence_id,
        pattern = "^[A-Za-z0-9][A-Za-z0-9._-]*$"
    )
    checkmate::assert_string(calendar, min.chars = 1L)
    checkmate::assert_int(stochastic_seed, na.ok = TRUE)
    checkmate::assert_list(provenance, names = "unique")
    data <- data.table::as.data.table(data.table::copy(data))
    if (all(c("year", "month", "day", "hour") %in% names(data))) {
        # The EPW datetime column is derived convenience data. Recompute it
        # after year assignment so Parquet timestamps match member identity.
        data[, datetime := epw_file_datetime(year, month, day, hour)]
        data.table::setcolorder(
            data,
            c("datetime", setdiff(names(data), "datetime"))
        )
    }
    WeatherSequenceMember(
        sequence_id = sequence_id,
        weather_year = as.integer(weather_year),
        calendar = calendar,
        stochastic_seed = as.integer(stochastic_seed),
        data = data,
        provenance = provenance
    )
}

# Construct the final typed sequence returned by an internal future-weather
# backend while retaining the same context metadata as epw_morph_result().
sequence__result <- function(
    context,
    members,
    output_type = c("multi_year", "future_year"),
    parts = list(),
    diagnostics = morpher__empty_diagnostics(),
    factors = NULL,
    provenance = list()
) {
    checkmate::assert_class(context, "morpher__context")
    checkmate::assert_list(members, min.len = 1L)
    output_type <- match.arg(output_type)
    checkmate::assert_list(parts, names = "named")
    checkmate::assert_list(provenance, names = "unique")
    WeatherSequenceResult(
        backend = context$recipe$backend,
        recipe = context$recipe,
        epw = context$epw,
        output_type = output_type,
        members = members,
        parts = parts,
        diagnostics = diagnostics,
        factors = factors,
        provenance = provenance
    )
}

# Normalize both legacy single-year and typed sequence backend results into
# member records consumed uniformly by persistence and EPW output code.
sequence__records <- function(result) {
    if (inherits(result, "epw_morph_result")) {
        return(list(list(
            output_type = "representative_year",
            sequence_id = NA_character_,
            weather_year = NA_integer_,
            calendar = NA_character_,
            stochastic_seed = NA_integer_,
            provenance = list(),
            data = data.table::as.data.table(data.table::copy(result$data))
        )))
    }
    if (!S7::S7_inherits(result, WeatherSequenceResult)) {
        cli::cli_abort(
            "A weather backend result must be an {.cls epw_morph_result} or {.cls WeatherSequenceResult}."
        )
    }
    lapply(result@members, function(member) {
        list(
            output_type = result@output_type,
            sequence_id = member@sequence_id,
            weather_year = member@weather_year,
            calendar = member@calendar,
            stochastic_seed = member@stochastic_seed,
            provenance = utils::modifyList(
                result@provenance,
                member@provenance
            ),
            data = data.table::as.data.table(data.table::copy(member@data))
        )
    })
}
