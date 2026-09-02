#' @include weather-pipeline.R
NULL

# EPW morphing backend registry {{{

EPW_MORPH_BACKEND_REGISTRY <- new.env(parent = emptyenv())
EPW_MORPH_BACKEND_WARNINGS <- new.env(parent = emptyenv())

morpher__split_rule_variables <- function(x) {
    if (is.list(x) && length(x) == 1L) {
        x <- x[[1L]]
    }
    x <- unlist(x, use.names = FALSE)
    if (!length(x)) {
        return(character())
    }
    x <- unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE)
    x <- trimws(x)
    x[!is.na(x) & nzchar(x)]
}

morpher__rule_list_column <- function(rules, list_col, scalar_col) {
    lapply(seq_len(nrow(rules)), function(i) {
        if (list_col %in% names(rules)) {
            out <- morpher__split_rule_variables(rules[[list_col]][i])
            if (length(out)) {
                return(out)
            }
        }
        if (scalar_col %in% names(rules)) {
            return(morpher__split_rule_variables(rules[[scalar_col]][i]))
        }
        character()
    })
}

morpher__rule_method_choices <- function(rule, fallback = character()) {
    if (!nrow(rule) || !"method_choices" %in% names(rule)) {
        return(fallback)
    }
    choices <- morpher__split_rule_variables(rule[["method_choices"]][1L])
    if (length(choices)) choices else fallback
}

morpher__rules_required_variables <- function(rules) {
    if (!nrow(rules)) {
        return(character())
    }
    vars <- lapply(seq_len(nrow(rules)), function(i) {
        morpher__split_rule_variables(rules[["required_variables"]][i])
    })
    unique(unlist(vars, use.names = FALSE))
}

# Build user-facing guidance for required CMIP variables that are unavailable
# in the selected extraction or summary input.
morpher__missing_variable_guidance <- function(variable_id, present_variables = character()) {
    present_variables <- unique(as.character(present_variables))
    present_variables <- present_variables[!is.na(present_variables) & nzchar(present_variables)]
    if (identical(variable_id, "hurs")) {
        return(list(
            suffix = paste(
                " Belcher humidity morphing requires near-surface relative humidity (hurs)",
                "or derivable huss + tas + ps inputs."
            ),
            action = paste(
                "Add and extract hurs from a source that provides near-surface relative humidity,",
                "or extract huss, tas, and surface pressure ps so canonical hurs can be derived;",
                "relative humidity and dew point fall back to baseline only in relaxed low-level runs."
            )
        ))
    }

    list(
        suffix = "",
        action = "Add and extract the required variable, or run in relaxed mode."
    )
}

morpher__rule_primary_variable <- function(rule) {
    vars <- if ("required_variables" %in% names(rule)) {
        morpher__split_rule_variables(rule[["required_variables"]][1L])
    } else {
        character()
    }
    if (length(vars)) {
        return(vars[[1L]])
    }
    vars <- morpher__split_rule_variables(rule[["variable_id"]][1L])
    if (length(vars)) vars[[1L]] else NA_character_
}

morpher__normalize_backend_rules <- function(name, rules, method_defaults = NULL, method_choices = NULL) {
    rules <- data.table::as.data.table(rules)
    required_cols <- c("step", "epw_field", "method", "required")
    missing_cols <- setdiff(required_cols, names(rules))
    if (length(missing_cols)) {
        cli::cli_abort("EPW morphing backend {.val {name}} rules are missing column(s): {.val {missing_cols}}.")
    }
    if (!"variable_id" %in% names(rules)) {
        rules[, variable_id := NA_character_]
    }
    if (!"optional_variable_id" %in% names(rules)) {
        rules[, optional_variable_id := NA_character_]
    }
    rules[, `:=`(
        step = as.character(step),
        epw_field = as.character(epw_field),
        variable_id = as.character(variable_id),
        optional_variable_id = as.character(optional_variable_id),
        method = as.character(method),
        required = as.logical(required)
    )]
    required_variables <- morpher__rule_list_column(rules, "required_variables", "variable_id")
    optional_variables <- morpher__rule_list_column(rules, "optional_variables", "optional_variable_id")
    method_defaults <- if (is.null(method_defaults)) stats::setNames(character(), character()) else method_defaults
    method_choices <- if (is.null(method_choices)) character() else method_choices
    rule_method_choices <- lapply(seq_len(nrow(rules)), function(i) {
        if ("method_choices" %in% names(rules)) {
            choices <- morpher__split_rule_variables(rules[["method_choices"]][i])
            if (length(choices)) {
                return(choices)
            }
        }
        if (rules$step[[i]] %in% names(method_defaults)) {
            return(method_choices)
        }
        rules$method[[i]]
    })
    data.table::set(rules, j = "required_variables", value = required_variables)
    data.table::set(rules, j = "optional_variables", value = optional_variables)
    data.table::set(rules, j = "method_choices", value = rule_method_choices)
    if (!"derived" %in% names(rules)) {
        data.table::set(
            rules,
            j = "derived",
            value = vapply(required_variables, function(x) !length(x), logical(1L))
        )
    } else {
        rules[, derived := as.logical(derived)]
    }
    missing_required <- rules[required == TRUE & !derived & lengths(required_variables) == 0L, step]
    if (length(missing_required)) {
        cli::cli_abort("EPW morphing backend {.val {name}} rule(s) lack required variables: {.val {missing_required}}.")
    }
    rules[]
}

#' EPW morphing backend
#'
#' @description
#' `EpwMorphBackend` defines a statistical downscaling backend that can be
#' selected by [epw_morph_recipe()] and executed by [EpwMorpher].
#'
#' @export
EpwMorphBackend <- R6::R6Class(
    "EpwMorphBackend",
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        #' @field name Backend name.
        name = NULL,
        #' @field label Human-readable backend label.
        label = NULL,
        #' @field requires_reference Whether external reference climate data are
        #'   mandatory for this backend.
        requires_reference = FALSE,
        #' @field accepts_reference Whether the backend can consume external
        #'   reference climate data. A backend may accept a reference without
        #'   requiring one.
        accepts_reference = FALSE,

        #' @description
        #' Create an EPW morphing backend.
        #'
        #' @param name Backend name.
        #' @param label Human-readable backend label.
        #' @param methods Named default method vector.
        #' @param method_choices Allowed method values.
        #' @param rules Backend rule table.
        #' @param requires_reference Whether external reference climate data are
        #'   mandatory. This can be `TRUE` only when `accepts_reference` is also
        #'   `TRUE`.
        #' @param accepts_reference Whether external reference climate data can
        #'   be consumed. `TRUE` with `requires_reference = FALSE` defines an
        #'   optional-reference backend such as Belcher.
        #' @param pipeline Optional internal component pipeline specification.
        #' @param runner Optional function taking `(context, backend)` and
        #'        returning an `epw_morph_result`. Exactly one of `pipeline` and
        #'        `runner` must be supplied.
        initialize = function(name, label = NULL, methods = NULL, method_choices = NULL, rules,
                              requires_reference = FALSE,
                              accepts_reference = requires_reference,
                              pipeline = NULL, runner = NULL) {
            checkmate::assert_string(name, min.chars = 1L)
            checkmate::assert_string(label, null.ok = TRUE)
            checkmate::assert_flag(requires_reference)
            checkmate::assert_flag(accepts_reference)
            if (isTRUE(requires_reference) && !isTRUE(accepts_reference)) {
                cli::cli_abort("A backend that requires reference climate must also accept it.")
            }
            if (is.null(methods)) {
                methods <- stats::setNames(character(), character())
            }
            checkmate::assert_character(methods, any.missing = FALSE, names = "named")
            if (is.null(method_choices)) {
                method_choices <- unique(unname(methods))
            }
            checkmate::assert_character(method_choices, any.missing = FALSE)
            if (!is.null(pipeline) &&
                !S7::S7_inherits(pipeline, WeatherPipelineSpec)) {
                cli::cli_abort(
                    "{.arg pipeline} must be a WeatherPipelineSpec object."
                )
            }
            checkmate::assert_function(runner, null.ok = TRUE)
            if (is.null(pipeline) == is.null(runner)) {
                cli::cli_abort(
                    "Supply exactly one backend {.arg pipeline} or {.arg runner}."
                )
            }
            self$name <- tolower(name)
            self$label <- if (is.null(label)) self$name else label
            self$requires_reference <- requires_reference
            self$accepts_reference <- accepts_reference
            private$method_defaults <- methods
            private$allowed_methods <- method_choices
            private$rule_table <- morpher__normalize_backend_rules(
                self$name,
                rules,
                method_defaults = private$method_defaults,
                method_choices = private$allowed_methods
            )
            private$pipeline <- pipeline
            private$runner <- runner
        },

        #' @description
        #' Return default backend methods.
        methods = function() {
            private$method_defaults
        },

        #' @description
        #' Return allowed backend method values.
        method_choices = function() {
            private$allowed_methods
        },

        #' @description
        #' Return backend rules.
        rules = function() {
            data.table::copy(private$rule_table)
        },

        #' @description
        #' Return the optional component pipeline used by this backend.
        component_pipeline = function() {
            private$pipeline
        },

        #' @description
        #' Return required CMIP variable IDs.
        required_variables = function() {
            rules <- private$rule_table
            morpher__rules_required_variables(rules[required == TRUE & !derived])
        },

        #' @description
        #' Validate and complete method overrides.
        #'
        #' @param methods Optional named method override vector.
        validate_methods = function(methods = NULL) {
            defaults <- private$method_defaults
            if (is.null(methods)) {
                return(defaults)
            }
            checkmate::assert_character(methods, any.missing = FALSE, names = "named")
            unknown <- setdiff(names(methods), names(defaults))
            if (length(unknown)) {
                cli::cli_abort("Unknown EPW morphing method override(s): {.val {unknown}}.")
            }
            rules <- private$rule_table
            for (method_name in names(methods)) {
                rule <- rules[step == method_name]
                allowed <- morpher__rule_method_choices(rule, private$allowed_methods)
                if (!methods[[method_name]] %in% allowed) {
                    cli::cli_abort(
                        "Unsupported EPW morphing method value {.val {methods[[method_name]]}} for step {.val {method_name}}. Allowed value(s): {.val {allowed}}."
                    )
                }
            }
            unlist(utils::modifyList(as.list(defaults), as.list(methods)))
        },

        #' @description
        #' Return backend rules with methods applied.
        #'
        #' @param methods Optional named method override vector.
        rules_with_methods = function(methods = NULL) {
            rules <- self$rules()
            methods <- self$validate_methods(methods)
            for (method_name in names(methods)) {
                rules[step == method_name, method := methods[[method_name]]]
            }
            rules[]
        },

        #' @description
        #' Run this backend on a canonical EPW morphing context.
        #'
        #' @param context Canonical EPW morphing context.
        run = function(context) {
            if (!is.null(private$pipeline)) {
                return(pipeline__run(private$pipeline, context))
            }
            private$runner(context, self)
        }
    ),
    private = list(
        method_defaults = NULL,
        allowed_methods = NULL,
        rule_table = NULL,
        pipeline = NULL,
        runner = NULL
    )
)

morpher__default_backend_specs <- function() {
    list(
        belcher = EpwMorphBackend$new(
            name = "belcher",
            label = "Belcher statistical downscaling with optional external reference",
            methods = unlist(EPW_MORPH_BELCHER_PROFILE_METHODS$enhanced, use.names = TRUE),
            method_choices = EPW_MORPH_BELCHER_METHOD_CHOICES,
            rules = EPW_MORPH_BELCHER_RULES,
            accepts_reference = TRUE,
            runner = morpher__belcher_run
        ),
        belcher_absolute = EpwMorphBackend$new(
            name = "belcher_absolute",
            label = "Belcher absolute-target statistical downscaling",
            methods = unlist(EPW_MORPH_BELCHER_ABSOLUTE_PROFILE_METHODS$enhanced, use.names = TRUE),
            method_choices = EPW_MORPH_BELCHER_METHOD_CHOICES,
            rules = EPW_MORPH_BELCHER_RULES,
            runner = morpher__belcher_absolute_run
        ),
        daily_temperature = EpwMorphBackend$new(
            name = "daily_temperature",
            label = "Calendar-neutral constrained daily temperature projection",
            methods = EPW_MORPH_DAILY_TEMPERATURE_METHODS,
            method_choices = "constrained",
            rules = EPW_MORPH_DAILY_TEMPERATURE_RULES,
            requires_reference = TRUE,
            pipeline = daily__temperature_pipeline()
        ),
        daily_temperature_btws = EpwMorphBackend$new(
            name = "daily_temperature_btws",
            label = "Daily temperature signal with BTWS hourly projection",
            methods = EPW_MORPH_DAILY_TEMPERATURE_BTWS_METHODS,
            method_choices = "btws",
            rules = EPW_MORPH_DAILY_TEMPERATURE_BTWS_RULES,
            requires_reference = TRUE,
            pipeline = daily__temperature_pipeline("btws")
        ),
        eames_monthly_temperature = EpwMorphBackend$new(
            name = "eames_monthly_temperature",
            label = "Eames monthly temperature signal with BTWS projection",
            methods = EPW_MORPH_DAILY_TEMPERATURE_BTWS_METHODS,
            method_choices = "btws",
            rules = EPW_MORPH_DAILY_TEMPERATURE_BTWS_RULES,
            requires_reference = TRUE,
            pipeline = eames__monthly_temperature_pipeline()
        ),
        ek_daily_temperature = EpwMorphBackend$new(
            name = "ek_daily_temperature",
            label = "Ek daily temperature change-factor workflow",
            methods = EPW_MORPH_EK_DAILY_TEMPERATURE_METHODS,
            method_choices = "daily_mean_dtr",
            rules = EPW_MORPH_EK_DAILY_TEMPERATURE_RULES,
            requires_reference = TRUE,
            pipeline = ek__pipeline()
        ),
        arima_temperature = EpwMorphBackend$new(
            name = "arima_temperature",
            label = "Arima month-wise quantile-mapping temperature workflow",
            methods = EPW_MORPH_ARIMA_TEMPERATURE_METHODS,
            method_choices = "percentile_additive",
            rules = EPW_MORPH_ARIMA_TEMPERATURE_RULES,
            requires_reference = TRUE,
            pipeline = arima__pipeline()
        ),
        sobie_curry_daily = EpwMorphBackend$new(
            name = "sobie_curry_daily",
            label = "Sobie-Curry daily morphing with selectable closure",
            methods = EPW_MORPH_SOBIE_CURRY_METHODS,
            method_choices = unname(EPW_MORPH_SOBIE_CURRY_METHODS),
            rules = EPW_MORPH_SOBIE_CURRY_RULES,
            requires_reference = TRUE,
            pipeline = sobie__pipeline()
        )
    )
}

morpher__warn_backend <- function(name) {
    if (!identical(name, "belcher_absolute")) {
        return(invisible(NULL))
    }
    if (exists(name, envir = EPW_MORPH_BACKEND_WARNINGS, inherits = FALSE)) {
        return(invisible(NULL))
    }
    assign(name, TRUE, envir = EPW_MORPH_BACKEND_WARNINGS)
    cli::cli_warn(c(
        "!" = "Backend {.val belcher_absolute} uses the legacy absolute-target Belcher implementation.",
        "i" = "Use {.code belcher(reference = historical_reference(...))} when matching historical data are available; omit the reference only as a fallback."
    ))
    invisible(NULL)
}

morpher__register_default_backends <- function() {
    specs <- morpher__default_backend_specs()
    for (name in names(specs)) {
        if (!exists(name, envir = EPW_MORPH_BACKEND_REGISTRY, inherits = FALSE)) {
            assign(name, specs[[name]], envir = EPW_MORPH_BACKEND_REGISTRY)
        }
    }
    invisible(NULL)
}

#' EPW morphing backends
#'
#' @return A character vector of registered backend names.
#' @export
epw_morph_backends <- function() {
    morpher__register_default_backends()
    sort(ls(envir = EPW_MORPH_BACKEND_REGISTRY, all.names = FALSE))
}

#' Get an EPW morphing backend
#'
#' @param name Backend name.
#'
#' @return An [EpwMorphBackend] object.
#' @export
epw_morph_backend <- function(name = "belcher") {
    morpher__register_default_backends()
    checkmate::assert_string(name, min.chars = 1L)
    name <- tolower(name)
    if (!exists(name, envir = EPW_MORPH_BACKEND_REGISTRY, inherits = FALSE)) {
        cli::cli_abort("Unknown EPW morphing backend: {.val {name}}.")
    }
    morpher__warn_backend(name)
    get(name, envir = EPW_MORPH_BACKEND_REGISTRY, inherits = FALSE)
}

#' Register an EPW morphing backend
#'
#' @param name Backend name.
#' @param backend An [EpwMorphBackend] object.
#' @param overwrite Whether to replace an existing backend.
#'
#' @return The backend object, invisibly.
#' @export
epw_morph_register_backend <- function(name, backend, overwrite = FALSE) {
    morpher__register_default_backends()
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_flag(overwrite)
    name <- tolower(name)
    if (exists(name, envir = EPW_MORPH_BACKEND_REGISTRY, inherits = FALSE) && !isTRUE(overwrite)) {
        cli::cli_abort("EPW morphing backend {.val {name}} is already registered.")
    }
    if (!inherits(backend, "EpwMorphBackend")) {
        cli::cli_abort("`backend` must be an {.cls EpwMorphBackend} object.")
    }
    if (!identical(backend$name, name)) {
        cli::cli_abort("Backend object name {.val {backend$name}} does not match registration name {.val {name}}.")
    }
    assign(name, backend, envir = EPW_MORPH_BACKEND_REGISTRY)
    invisible(backend)
}
# }}}
