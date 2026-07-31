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
