# Direct model realizations use one stable identifier because ensemble-member
# identity already belongs to the surrounding morphing case.
DIRECT_MODEL_SEQUENCE_ID <- "direct-model"

# Return the first non-NULL validation error string across supplied checks.
# sequence__first_error {{{
sequence__first_error <- function(...) {
    for (item in list(...)) {
        if (!is.null(item)) {
            return(item)
        }
    }
    NULL
}
# }}}

# Return the supplied error when a sequence identifier is not one safe scalar.
# sequence__identifier_error {{{
sequence__identifier_error <- function(value, message) {
    if (
        length(value) != 1L ||
            is.na(value) ||
            !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", value)
    ) {
        return(message)
    }
    NULL
}
# }}}

# Validate optional sequence metadata without imposing a value schema.
# sequence__provenance_error {{{
sequence__provenance_error <- function(value) {
    if (
        length(value) &&
            (is.null(names(value)) ||
                any(!nzchar(names(value))) ||
                anyDuplicated(names(value)))
    ) {
        return("`provenance` must be a uniquely named list.")
    }
    NULL
}
# }}}

# Require a non-empty homogeneous collection of one declared S7 member class.
# sequence__member_class_error {{{
sequence__member_class_error <- function(members, class, message) {
    if (
        !length(members) ||
            !all(vapply(members, S7::S7_inherits, logical(1L), class = class))
    ) {
        return(message)
    }
    NULL
}
# }}}

# Validate shared signal-group identity fields on direct-model and mapped series.
# sequence__validate_group_series {{{
sequence__validate_group_series <- function(
    group_id,
    key,
    variables,
    key_atomic = FALSE
) {
    if (
        length(group_id) != 1L ||
            is.na(group_id) ||
            !grepl("^[a-z][a-z0-9-]*$", group_id)
    ) {
        return("`group_id` must use lower-case letters, numbers, and hyphens.")
    }
    if (length(key)) {
        bad_names <- is.null(names(key)) ||
            any(!nzchar(names(key))) ||
            anyDuplicated(names(key))
        bad_values <- isTRUE(key_atomic) &&
            (any(vapply(key, length, integer(1L)) != 1L) ||
                any(!vapply(key, is.atomic, logical(1L))))
        if (bad_names || bad_values) {
            return(
                if (isTRUE(key_atomic)) {
                    "`key` must be a uniquely named list of atomic scalar values."
                } else {
                    "`key` must be a uniquely named list."
                }
            )
        }
    }
    if (
        !length(variables) ||
            anyNA(variables) ||
            any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", variables)) ||
            anyDuplicated(variables)
    ) {
        return("`variables` must contain unique CMIP-style identifiers.")
    }
    NULL
}
# }}}

# Validate the common header fields shared across all sequence member classes.
# sequence__validate_member_header {{{
sequence__validate_member_header <- function(
    sequence_id,
    weather_year,
    calendar,
    calendar_name = "calendar",
    cf_calendar = TRUE,
    id_message = "`sequence_id` contains unsupported characters.",
    provenance = list()
) {
    sequence__first_error(
        sequence__identifier_error(sequence_id, id_message),
        if (
            length(weather_year) != 1L ||
                is.na(weather_year) ||
                weather_year < 1L
        ) {
            "`weather_year` must be one positive integer."
        },
        if (isTRUE(cf_calendar)) {
            if (
                length(calendar) != 1L ||
                    is.na(calendar) ||
                    !calendar %in% CF_TIME_CALENDARS
            ) {
                sprintf(
                    "`%s` must identify one supported CF calendar.",
                    calendar_name
                )
            }
        } else if (
            length(calendar) != 1L || is.na(calendar) || !nzchar(calendar)
        ) {
            sprintf("`%s` must be one non-empty string.", calendar_name)
        },
        sequence__provenance_error(provenance)
    )
}
# }}}

# Validate homogeneous member classes, unique ascending years, shared sequence
# identity, and provenance across all typed direct-model sequence containers.
# sequence__validate_members {{{
sequence__validate_members <- function(
    members,
    class,
    class_name,
    label,
    year_label = "weather years",
    provenance = list()
) {
    error <- sequence__member_class_error(
        members,
        class,
        sprintf("`members` must contain %s objects.", class_name)
    )
    if (!is.null(error)) {
        return(error)
    }
    years <- vapply(members, function(m) m@weather_year, integer(1L))
    if (anyDuplicated(years) || !identical(years, sort(years))) {
        return(sprintf(
            "%s must use unique ascending %s.",
            label,
            year_label
        ))
    }
    sequence_ids <- vapply(members, function(m) m@sequence_id, character(1L))
    if (length(unique(sequence_ids)) != 1L) {
        return(sprintf("%s must share one `sequence_id`.", label))
    }
    sequence__provenance_error(provenance)
}
# }}}

# Compare normalized identity sets across years without defining their contents.
# sequence__shared_sets_error {{{
sequence__shared_sets_error <- function(sets, message) {
    if (
        length(sets) > 1L &&
            !all(vapply(sets[-1L], identical, logical(1L), sets[[1L]]))
    ) {
        return(message)
    }
    NULL
}
# }}}

# Shared EPW template input requirement and extraction helper for direct-model
# hourly, physics, and output components.
# sequence__epw_template_requirement {{{
sequence__epw_template_requirement <- function() {
    list(
        weather_template = component__input_requirement(
            "weather_template",
            representations = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        )
    )
}
# }}}

# sequence__epw_template_input {{{
sequence__epw_template_input <- function(inputs, require_epw_file = FALSE) {
    template_input <- weather__get_input(inputs, "weather_template")
    if (
        !S7::S7_inherits(template_input, WeatherInput) ||
            !identical(template_input@representation, "epw") ||
            (isTRUE(require_epw_file) &&
                !inherits(template_input@source, "EpwFile"))
    ) {
        cli::cli_abort(
            "Role `weather_template` must contain an EPW WeatherInput."
        )
    }
    template_input@source
}
# }}}

# Return a deterministic identity for one aligned signal group without
# serializing its input payloads into the downstream sequence.
# sequence__direct_group_id {{{
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
# }}}

# Check whether one adjusted variable group contains exactly one complete,
# ordered native-calendar year at its declared regular frequency.
# sequence__direct_year_error {{{
sequence__direct_year_error <- function(adjusted, weather_year, calendar) {
    data <- adjusted@data
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
        if (length(year_days) != 1L) {
            return(sprintf(
                "Variable `%s` must use one native-calendar year length.",
                variable
            ))
        }
        if (identical(adjusted@frequency, "day")) {
            if (
                nrow(rows) == year_days &&
                    identical(
                        as.integer(rows[["cf_day_of_year"]]),
                        seq_len(year_days)
                    )
            ) {
                next
            }
            return(sprintf(
                "Variable `%s` must cover every native-calendar day in weather year %d.",
                variable,
                weather_year
            ))
        }

        # A complete sub-daily year repeats the same regular time-of-day
        # lattice on every native-calendar day, including non-Gregorian years.
        samples_per_day <- as.integer(round(
            86400 / adjusted@time_step_seconds
        ))
        expected_days <- rep(seq_len(year_days), each = samples_per_day)
        if (
            nrow(rows) != length(expected_days) ||
                !identical(
                    as.integer(rows[["cf_day_of_year"]]),
                    expected_days
                )
        ) {
            return(sprintf(
                "Variable `%s` must cover every declared sub-daily timestep in weather year %d.",
                variable,
                weather_year
            ))
        }
        offsets <- rows[["cf_second_of_day"]][seq_len(samples_per_day)]
        wrapped_steps <- diff(c(offsets, offsets[[1L]] + 86400))
        tolerance <- sqrt(.Machine$double.eps)
        if (any(abs(wrapped_steps - adjusted@time_step_seconds) > tolerance)) {
            return(sprintf(
                "Variable `%s` has an irregular within-day timestep lattice.",
                variable
            ))
        }
        observed_offsets <- matrix(
            rows[["cf_second_of_day"]],
            ncol = samples_per_day,
            byrow = TRUE
        )
        expected_offsets <- matrix(
            rep.int(offsets, year_days),
            ncol = samples_per_day,
            byrow = TRUE
        )
        if (any(abs(observed_offsets - expected_offsets) > tolerance)) {
            return(sprintf(
                "Variable `%s` must use the same sub-daily offsets on every day.",
                variable
            ))
        }
    }
    NULL
}
# }}}

# DirectModelSeries retains one signal group's key, variables, correction
# metadata, and calendar-native values after partitioning by source year.
# DirectModelSeries {{{
DirectModelSeries <- S7::new_class(
    "DirectModelSeries",
    properties = list(
        group_id = S7::new_property(S7::class_character),
        key = S7::new_property(S7::class_list, default = list()),
        variables = S7::new_property(S7::class_character),
        adjusted = S7::new_property(S7::class_any)
    ),
    validator = function(self) {
        sequence__first_error(
            sequence__validate_group_series(
                self@group_id,
                self@key,
                self@variables,
                key_atomic = TRUE
            ),
            if (!S7::S7_inherits(self@adjusted, AdjustedWeatherSeries)) {
                "`adjusted` must be an AdjustedWeatherSeries object."
            },
            if (!identical(self@adjusted@output_role, "model_future")) {
                "Direct model realization requires a `model_future` signal output."
            },
            if (
                !setequal(
                    self@variables,
                    unique(self@adjusted@data[["variable_id"]])
                )
            ) {
                "`variables` must match the adjusted series variables."
            }
        )
    }
)
# }}}

# DirectModelSequenceMember groups all corrected signal series belonging to
# one complete source-model year without selecting, resampling, or reordering values.
# DirectModelSequenceMember {{{
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
        error <- sequence__first_error(
            sequence__validate_member_header(
                self@sequence_id,
                self@weather_year,
                self@calendar,
                provenance = self@provenance
            ),
            sequence__member_class_error(
                self@series,
                DirectModelSeries,
                "`series` must contain DirectModelSeries objects."
            )
        )
        if (!is.null(error)) {
            return(error)
        }
        group_ids <- vapply(self@series, function(x) x@group_id, character(1L))
        if (anyDuplicated(group_ids)) {
            return(
                "Direct-model group identities must be unique within a year."
            )
        }
        for (item in self@series) {
            error <- sequence__direct_year_error(
                item@adjusted,
                self@weather_year,
                self@calendar
            )
            if (!is.null(error)) {
                return(error)
            }
        }
        NULL
    }
)
# }}}

# DirectModelSequence is the frequency-aware intermediate exchanged between a
# future-backbone signal and later reconstruction and physical components.
# DirectModelSequence {{{
DirectModelSequence <- S7::new_class(
    "DirectModelSequence",
    properties = list(
        members = S7::new_property(S7::class_list),
        frequency = S7::new_property(S7::class_character),
        time_step_seconds = S7::new_property(S7::class_numeric),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        error <- sequence__first_error(
            sequence__validate_members(
                self@members,
                DirectModelSequenceMember,
                "DirectModelSequenceMember",
                "Direct-model members",
                provenance = self@provenance
            ),
            if (
                length(self@frequency) != 1L ||
                    is.na(self@frequency) ||
                    !nzchar(self@frequency)
            ) {
                "`frequency` must identify one adjusted-series frequency."
            },
            if (
                length(self@time_step_seconds) != 1L ||
                    is.na(self@time_step_seconds) ||
                    !is.finite(self@time_step_seconds) ||
                    self@time_step_seconds <= 0
            ) {
                "`time_step_seconds` must be one positive finite number."
            }
        )
        if (!is.null(error)) {
            return(error)
        }
        calendars <- vapply(self@members, function(m) m@calendar, character(1L))
        if (length(unique(calendars)) != 1L) {
            return("Direct-model members must share one CF calendar.")
        }
        group_ids <- lapply(self@members, function(member) {
            sort(vapply(member@series, function(x) x@group_id, character(1L)))
        })
        error <- sequence__shared_sets_error(
            group_ids,
            "Every direct-model year must contain the same signal groups."
        )
        if (!is.null(error)) {
            return(error)
        }
        for (member in self@members) {
            for (item in member@series) {
                if (
                    !identical(item@adjusted@frequency, self@frequency) ||
                        !isTRUE(all.equal(
                            item@adjusted@time_step_seconds,
                            self@time_step_seconds
                        ))
                ) {
                    return(
                        "Every direct-model series must share the sequence frequency and timestep."
                    )
                }
            }
        }
        NULL
    }
)
# }}}

# Rebuild one year slice with the original signal transformation, settings,
# and provenance while retaining only variables present in that source year.
# sequence__slice_adjusted {{{
sequence__slice_adjusted <- function(adjusted, year) {
    data <- data.table::as.data.table(
        data.table::copy(adjusted@data)
    )
    # Keep the requested scalar outside data.table evaluation so a compatibility
    # alias column named `year` cannot shadow the function argument.
    target_year <- as.integer(year)
    data <- data[get("cf_year") == target_year]
    data.table::setorderv(
        data,
        c("cf_day_of_year", "annual_phase", "variable_id")
    )
    variables <- unique(data[["variable_id"]])
    # Mapping curves and row-level evidence cover the entire signal group.
    # Retain them once in the output parts rather than duplicating them in each
    # year member's JSON provenance or mislabelling full-period rows as a slice.
    provenance <- adjusted@provenance
    if (!is.null(provenance$mapping)) {
        provenance$mapping <- NULL
        provenance$mapping_location <- "parts$signal$groups"
    }
    bias__adjusted_series(
        data = data,
        frequency = adjusted@frequency,
        time_step_seconds = adjusted@time_step_seconds,
        output_role = adjusted@output_role,
        transformation = adjusted@transformation,
        variable_metadata = adjusted@variable_metadata[variables],
        settings = adjusted@settings,
        provenance = provenance
    )
}
# }}}

# Construct one typed year member from all aligned signal groups after the
# generator has established common year and calendar coverage.
# sequence__direct_member {{{
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
# }}}

# Preserve the corrected future-model chronology, partition it by complete CF
# year, and retain group-level signal metadata for later reconstruction.
# sequence__direct_model_generate {{{
sequence__direct_model_generate <- function(
    data,
    inputs,
    context,
    options
) {
    if (
        !S7::S7_inherits(data, SignalExecutionResult) ||
            !length(data@groups) ||
            length(data@groups) != length(data@values)
    ) {
        cli::cli_abort(
            "Direct model realization requires an aligned SignalExecutionResult."
        )
    }
    if (
        any(data@diagnostics[["status"]] != "ok") ||
            any(vapply(data@values, is.null, logical(1L)))
    ) {
        cli::cli_abort(
            "Direct model realization cannot preserve failed signal groups."
        )
    }
    if (
        !all(vapply(
            data@values,
            S7::S7_inherits,
            logical(1L),
            class = AdjustedWeatherSeries
        ))
    ) {
        cli::cli_abort(
            "Direct model realization requires AdjustedWeatherSeries values."
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
    if (
        !all(vapply(
            year_sets[-1L],
            identical,
            logical(1L),
            year_sets[[1L]]
        ))
    ) {
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
    frequencies <- vapply(
        data@values,
        function(adjusted) adjusted@frequency,
        character(1L)
    )
    time_steps <- vapply(
        data@values,
        function(adjusted) adjusted@time_step_seconds,
        numeric(1L)
    )
    if (
        length(unique(frequencies)) != 1L ||
            length(unique(time_steps)) != 1L
    ) {
        cli::cli_abort(
            "Every direct-model signal group must use the same frequency and timestep."
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
        frequency = frequencies[[1L]],
        time_step_seconds = time_steps[[1L]],
        provenance = list(
            method = "direct_model_realization",
            source_role = "model_future",
            ordering = "native_cf_chronology",
            selection = "none",
            resampling = "none",
            years = years,
            calendar = calendars[[1L]],
            frequency = frequencies[[1L]],
            time_step_seconds = time_steps[[1L]],
            group_ids = group_ids
        )
    )
}
# }}}

# Describe the deterministic sequence component independently of any one
# complete weather recipe or bias-adjustment implementation.
# sequence__direct_model_component {{{
sequence__direct_model_component <- function() {
    component__spec(
        name = "direct_model_realization",
        stage = "sequence",
        label = "Direct model realization",
        required_inputs = list(
            model_future = component__input_requirement(
                "model_future",
                representations = "series",
                calendars = CF_TIME_CALENDARS
            )
        ),
        input_kinds = c(
            "daily_adjusted_series",
            "subdaily_adjusted_series",
            "adjusted_weather_series"
        ),
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
            frequency_policy = "single_regular_frequency",
            output_contract = "direct_model_sequence"
        )
    )
}
# }}}

# Register the reusable sequence implementation once so recipes can refer to
# its stable algorithmic name without embedding executable functions.
# sequence__register_direct_model_component {{{
sequence__register_direct_model_component <- function() {
    component__register_builtin(sequence__direct_model_component())
    invisible(NULL)
}
# }}}

# Future-weather sequence results keep year identity outside the hourly table
# so each member can be persisted, resumed, and written as an independent EPW.
# WeatherSequenceMember {{{
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
        error <- sequence__first_error(
            sequence__validate_member_header(
                self@sequence_id,
                self@weather_year,
                self@calendar,
                cf_calendar = FALSE,
                id_message = "`sequence_id` must contain only letters, numbers, dots, underscores, or hyphens.",
                provenance = self@provenance
            ),
            if (length(self@stochastic_seed) != 1L) {
                "`stochastic_seed` must be one integer or `NA_integer_`."
            },
            if (!is.data.frame(self@data) || !nrow(self@data)) {
                "`data` must be a non-empty hourly weather table."
            },
            if (!"year" %in% names(self@data)) {
                "`data` must contain an EPW `year` column."
            }
        )
        if (!is.null(error)) {
            return(error)
        }
        years <- unique(as.integer(self@data[["year"]]))
        if (
            anyNA(years) ||
                length(years) != 1L ||
                !identical(years, self@weather_year)
        ) {
            return("Every hourly row must match `weather_year`.")
        }
        NULL
    }
)
# }}}

# WeatherSequenceResult is the final backend contract for one or more explicit
# future years; existing representative-year backends keep epw_morph_result().
# WeatherSequenceResult {{{
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
        error <- sequence__first_error(
            if (
                length(self@backend) != 1L ||
                    is.na(self@backend) ||
                    !nzchar(self@backend)
            ) {
                "`backend` must be one non-empty string."
            },
            if (!inherits(self@epw, "EpwFile")) {
                "`epw` must be an internal EpwFile object."
            },
            if (
                length(self@output_type) != 1L ||
                    is.na(self@output_type) ||
                    !self@output_type %in% c("future_year", "multi_year")
            ) {
                "`output_type` must be `future_year` or `multi_year`."
            },
            sequence__member_class_error(
                self@members,
                WeatherSequenceMember,
                "`members` must contain WeatherSequenceMember objects."
            ),
            if (
                identical(self@output_type, "future_year") &&
                    length(self@members) != 1L
            ) {
                "A `future_year` result must contain exactly one member."
            },
            if (
                identical(self@output_type, "multi_year") &&
                    length(self@members) < 2L
            ) {
                "A `multi_year` result must contain at least two members."
            },
            sequence__provenance_error(self@provenance)
        )
        if (!is.null(error)) {
            return(error)
        }
        keys <- vapply(
            self@members,
            function(m) paste(m@sequence_id, m@weather_year, sep = "\r"),
            character(1L)
        )
        if (anyDuplicated(keys)) {
            return(
                "Sequence member `sequence_id` and `weather_year` pairs must be unique."
            )
        }
        template <- self@epw$data()
        required <- setdiff(names(template), "datetime")
        valid_data <- vapply(
            self@members,
            function(member) {
                nrow(member@data) == nrow(template) &&
                    all(required %in% names(member@data))
            },
            logical(1L)
        )
        if (!all(valid_data)) {
            return(
                "Every sequence member must contain a complete baseline-shaped EPW year."
            )
        }
        NULL
    }
)
# }}}

# Construct one validated sequence member after hourly reconstruction and
# calendar mapping have produced a complete EPW-compatible weather year.
# sequence__member {{{
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
        data[, datetime := epw_file__datetime(year, month, day, hour)]
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
# }}}

# Construct the final typed sequence returned by an internal future-weather
# backend while retaining the same context metadata as epw_morph_result().
# sequence__result {{{
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
# }}}

# Preserve the signal's scientific values and resolved settings as plain data.
# Group inputs and executable components are deliberately not serialized.
# sequence__signal_products {{{
sequence__signal_products <- function(stages) {
    stage <- stages$signal
    if (is.null(stage)) {
        return(NULL)
    }
    execution <- stage@value
    if (!S7::S7_inherits(execution, SignalExecutionResult)) {
        cli::cli_abort(
            "Direct-model output requires a signal execution record."
        )
    }
    groups <- lapply(seq_along(execution@values), function(i) {
        value <- execution@values[[i]]
        if (!S7::S7_inherits(value, AdjustedWeatherSeries)) {
            cli::cli_abort(
                "Direct-model signal products must be adjusted weather series."
            )
        }
        list(
            group_index = i,
            group_id = sequence__direct_group_id(execution@groups[[i]]),
            key = execution@groups[[i]]@key,
            variables = execution@groups[[i]]@variables,
            data = value@data,
            frequency = value@frequency,
            time_step_seconds = value@time_step_seconds,
            output_role = value@output_role,
            transformation = value@transformation,
            variable_metadata = value@variable_metadata,
            settings = value@settings,
            provenance = value@provenance
        )
    })
    list(
        component = stage@component,
        profiles = execution@profiles,
        diagnostics = execution@diagnostics,
        groups = groups
    )
}
# }}}

# Translate correction counts to the common runtime diagnostic schema while
# retaining the full physical table and row-addressable signal evidence in
# parts. Messages identify the exact group/month or output year to inspect.
# sequence__runtime_diagnostics {{{
sequence__runtime_diagnostics <- function(physical, signal) {
    count_names <- grep(
        "(_clipped|_zeroed|_reallocated)$",
        names(physical),
        value = TRUE
    )
    physical_rows <- lapply(count_names, function(name) {
        indices <- which(!is.na(physical[[name]]) & physical[[name]] > 0)
        lapply(indices, function(i) {
            morpher__diagnostic(
                stage = "physics",
                severity = "warning",
                code = paste0("physical_", name),
                message = sprintf(
                    "%s affected %d hourly state(s) in weather year %d.",
                    name,
                    physical[[name]][[i]],
                    physical$weather_year[[i]]
                ),
                action = "Inspect the matching weather_year in parts$physical_diagnostics. Counts may overlap across correction rules."
            )
        })
    })
    signal_rows <- lapply(signal$groups, function(group) {
        months <- group$provenance$diagnostics$months
        if (!is.data.frame(months) || !"clipped_values" %in% names(months)) {
            return(list())
        }
        lapply(which(months$clipped_values > 0), function(i) {
            morpher__diagnostic(
                stage = "signal",
                severity = "warning",
                code = "signal_bounds_clipped",
                variable_id = paste(group$variables, collapse = ","),
                month = months$center_month[[i]],
                message = sprintf(
                    "Signal group %d bounded %d value(s) in native calendar month %d.",
                    group$group_index,
                    months$clipped_values[[i]],
                    months$center_month[[i]]
                ),
                action = sprintf(
                    "Inspect parts$signal$groups[[%d]]$settings$bounds and provenance$mapping$rows for input, unbounded and adjusted values and native CF dates.",
                    group$group_index
                )
            )
        })
    })
    rows <- c(
        unlist(physical_rows, recursive = FALSE),
        unlist(signal_rows, recursive = FALSE)
    )
    morpher__bind_diagnostics(rows)
}
# }}}

# Convert physically closed direct-model years into the package's persistent
# multi-year result contract. File creation remains owned by EpwMorpher.
# sequence__epw_output_write {{{
sequence__epw_output_write <- function(
    data,
    inputs,
    context,
    options,
    stages
) {
    if (!S7::S7_inherits(data, EpwHourlyWeatherSequence)) {
        cli::cli_abort(
            "Direct-model EPW output requires an EpwHourlyWeatherSequence object."
        )
    }
    if (length(data@members) < 2L) {
        cli::cli_abort(
            "Direct-model multi-year output requires at least two complete weather years."
        )
    }

    # Preserve every source-model year as an independently addressable member
    # while retaining physical and calendar provenance from the earlier stages.
    members <- lapply(data@members, function(member) {
        sequence__member(
            data = member@data,
            weather_year = member@weather_year,
            sequence_id = member@sequence_id,
            calendar = data@target_calendar,
            provenance = member@provenance
        )
    })
    diagnostics <- data.table::rbindlist(
        lapply(data@members, function(member) {
            data.table::as.data.table(member@diagnostics)
        }),
        use.names = TRUE,
        fill = TRUE
    )
    signal <- sequence__signal_products(stages)

    sequence__result(
        context,
        members,
        output_type = "multi_year",
        parts = list(
            physical_diagnostics = diagnostics,
            constructed_fields = data@constructed_fields,
            signal = signal,
            runtime_diagnostics = sequence__runtime_diagnostics(
                diagnostics,
                signal
            )
        ),
        diagnostics = diagnostics,
        provenance = list(
            method = "direct_model_epw_result",
            target_calendar = data@target_calendar,
            constructed_fields = data@constructed_fields,
            physical = data@provenance
        )
    )
}
# }}}

# Describe the common output boundary for direct-model future-weather recipes
# that retain every complete corrected model year.
# sequence__epw_output_component {{{
sequence__epw_output_component <- function() {
    component__spec(
        name = "direct_model_epw_result",
        stage = "output",
        label = "Direct-model multi-year EPW result",
        required_inputs = sequence__epw_template_requirement(),
        input_kinds = "epw_hourly_weather_sequence",
        output_kinds = "weather_sequence_result",
        scopes = "multivariate",
        stochastic = FALSE,
        operations = list(write = sequence__epw_output_write),
        metadata = list(
            output_type = "multi_year",
            target_calendar = "epw_365_day",
            member_policy = "one_member_per_complete_model_year",
            file_writer = "EpwMorpher",
            output_contract = "weather_sequence_result"
        )
    )
}
# }}}

# Register the direct-model output implementation once so complete recipes can
# close the seven-stage component sequence without embedding executable code.
# sequence__register_epw_output_component {{{
sequence__register_epw_output_component <- function() {
    component__register_builtin(sequence__epw_output_component())
    invisible(NULL)
}
# }}}

# Normalize both legacy single-year and typed sequence backend results into
# member records consumed uniformly by persistence and EPW output code.
# sequence__records {{{
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
# }}}

# vim: fdm=marker :
