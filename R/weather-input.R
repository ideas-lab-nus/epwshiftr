# Future-weather input roles remain independent because bias adjustment can
# require an observational reference and matching historical/future model data
# in addition to the hourly weather template being transformed.
WEATHER_INPUT_ROLES <- c(
    "weather_template",
    "observed_reference",
    "model_historical",
    "model_future"
)

# Representations describe how an input can be consumed without constraining
# the concrete R object used to carry an EPW, table, grid, library, or workflow
# stage.
WEATHER_INPUT_REPRESENTATIONS <- c(
    "epw",
    "series",
    "grid",
    "hourly_library",
    "stage",
    "external"
)

# WeatherInput keeps the semantic role separate from source metadata so a
# component never has to infer whether "reference" means observations or a
# historical model run.
WeatherInput <- S7::new_class(
    "WeatherInput",
    properties = list(
        role = S7::new_property(S7::class_character),
        source = S7::new_property(S7::class_any),
        representation = S7::new_property(S7::class_character),
        variables = S7::new_property(S7::class_character, default = character()),
        frequencies = S7::new_property(S7::class_character, default = character()),
        calendars = S7::new_property(S7::class_character, default = character()),
        provenance = S7::new_property(S7::class_list, default = list()),
        metadata = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (is.null(self@source)) {
            return("`source` cannot be NULL.")
        }
        if (length(self@role) != 1L ||
            is.na(self@role) ||
            !self@role %in% WEATHER_INPUT_ROLES) {
            return(sprintf(
                "`role` must be one of %s.",
                paste(sprintf("`%s`", WEATHER_INPUT_ROLES), collapse = ", ")
            ))
        }
        if (length(self@representation) != 1L ||
            is.na(self@representation) ||
            !self@representation %in% WEATHER_INPUT_REPRESENTATIONS) {
            return(sprintf(
                "`representation` must be one of %s.",
                paste(
                    sprintf("`%s`", WEATHER_INPUT_REPRESENTATIONS),
                    collapse = ", "
                )
            ))
        }
        for (property in c("variables", "frequencies", "calendars")) {
            value <- S7::prop(self, property)
            if (anyNA(value) || any(!nzchar(value)) || anyDuplicated(value)) {
                return(sprintf(
                    "`%s` must contain unique, non-missing, non-empty values.",
                    property
                ))
            }
        }
        NULL
    }
)

# WeatherInputs provides one named slot for every semantic role. Missing roles
# remain explicit NULL values instead of being guessed from another input.
WeatherInputs <- S7::new_class(
    "WeatherInputs",
    properties = list(
        weather_template = S7::new_property(S7::class_any, default = NULL),
        observed_reference = S7::new_property(S7::class_any, default = NULL),
        model_historical = S7::new_property(S7::class_any, default = NULL),
        model_future = S7::new_property(S7::class_any, default = NULL)
    ),
    validator = function(self) {
        present <- character()
        for (role in WEATHER_INPUT_ROLES) {
            value <- S7::prop(self, role)
            if (is.null(value)) {
                next
            }
            present <- c(present, role)
            if (!S7::S7_inherits(value, WeatherInput)) {
                return(sprintf("`%s` must be a WeatherInput or NULL.", role))
            }
            if (!identical(value@role, role)) {
                return(sprintf(
                    "`%s` contains input role `%s`.",
                    role,
                    value@role
                ))
            }
        }
        if (!length(present)) {
            return("At least one future-weather input must be supplied.")
        }
        NULL
    }
)

# Normalize optional descriptor values once so every input has stable,
# serializable metadata even when its source table uses factors or list columns.
weather__descriptor_values <- function(value, name) {
    if (is.null(value)) {
        return(character())
    }
    checkmate::assert_character(
        value,
        any.missing = FALSE,
        unique = TRUE
    )
    value <- as.character(value)
    if (any(!nzchar(value))) {
        cli::cli_abort("{.arg {name}} cannot contain empty values.")
    }
    value
}

# Read one descriptor from a canonical climate table without requiring all
# external or deferred sources to materialize their data at construction time.
weather__source_values <- function(source, columns) {
    if (!is.data.frame(source)) {
        return(character())
    }
    column <- intersect(columns, names(source))
    if (!length(column)) {
        return(character())
    }
    values <- unique(as.character(source[[column[[1L]]]]))
    values[!is.na(values) & nzchar(values)]
}

# Infer only the physical representation of common in-package sources. Unknown
# objects remain explicit external inputs instead of being inspected by class
# name heuristics that optional packages could accidentally satisfy.
weather__representation <- function(source) {
    if (inherits(source, "EpwFile") ||
        (is.character(source) && length(source) == 1L &&
            grepl("\\.epw$", source, ignore.case = TRUE))) {
        return("epw")
    }
    if (is.data.frame(source)) {
        return("series")
    }
    "external"
}

# Construct one role-labelled future-weather input while retaining the source
# object unchanged and deriving only metadata that are already materialized.
weather__new_input <- function(
    role, source, representation = NULL,
    variables = NULL, frequencies = NULL, calendars = NULL,
    provenance = list(), metadata = list()
) {
    checkmate::assert_choice(role, WEATHER_INPUT_ROLES)
    if (missing(source) || is.null(source)) {
        cli::cli_abort("{.arg source} cannot be NULL.")
    }
    if (is.null(representation)) {
        representation <- weather__representation(source)
    }
    checkmate::assert_choice(
        representation,
        WEATHER_INPUT_REPRESENTATIONS
    )
    if (is.null(variables)) {
        variables <- weather__source_values(source, "variable_id")
    }
    if (is.null(frequencies)) {
        frequencies <- weather__source_values(source, "frequency")
    }
    if (is.null(calendars)) {
        calendars <- weather__source_values(
            source,
            c("cf_calendar", "calendar")
        )
    }
    checkmate::assert_list(provenance, names = "unique")
    checkmate::assert_list(metadata, names = "unique")

    WeatherInput(
        role = role,
        source = source,
        representation = representation,
        variables = weather__descriptor_values(variables, "variables"),
        frequencies = weather__descriptor_values(
            frequencies,
            "frequencies"
        ),
        calendars = weather__descriptor_values(calendars, "calendars"),
        provenance = provenance,
        metadata = metadata
    )
}

# Construct the complete role-addressable input set used by component
# validation and execution contexts.
weather__new_inputs <- function(
    weather_template = NULL,
    observed_reference = NULL,
    model_historical = NULL,
    model_future = NULL
) {
    WeatherInputs(
        weather_template = weather_template,
        observed_reference = observed_reference,
        model_historical = model_historical,
        model_future = model_future
    )
}

# Retrieve one input by semantic role without exposing callers to dynamic S7
# property access.
weather__get_input <- function(inputs, role) {
    if (!S7::S7_inherits(inputs, WeatherInputs)) {
        cli::cli_abort("{.arg inputs} must be a WeatherInputs object.")
    }
    checkmate::assert_choice(role, WEATHER_INPUT_ROLES)
    S7::prop(inputs, role)
}

# Build explicit role-labelled inputs for the legacy morphing context. The old
# context fields are retained separately for custom backend compatibility.
weather__context_inputs <- function(
    epw, model_future,
    model_historical = NULL,
    observed_reference = NULL
) {
    weather__new_inputs(
        weather_template = weather__new_input(
            "weather_template",
            epw,
            representation = "epw",
            frequencies = "hour",
            calendars = "gregorian"
        ),
        observed_reference = if (is.null(observed_reference)) {
            NULL
        } else {
            weather__new_input("observed_reference", observed_reference)
        },
        model_historical = if (is.null(model_historical)) {
            NULL
        } else {
            weather__new_input("model_historical", model_historical)
        },
        model_future = weather__new_input("model_future", model_future)
    )
}
