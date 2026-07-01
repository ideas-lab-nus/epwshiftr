#' @include store.R utils.R
NULL

# epw morph helpers {{{
EPW_MORPH_VARIABLE_LEVELS <- list(
    minimal = c("tas", "hurs"),
    recommended = c("tas", "hurs", "psl", "rlds", "rsds", "sfcWind", "clt", "pr"),
    extended = c("tas", "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "psl", "rlds", "rsds", "sfcWind", "clt", "pr")
)

EPW_MORPH_BACKEND_REGISTRY <- new.env(parent = emptyenv())
EPW_MORPH_BACKEND_WARNINGS <- new.env(parent = emptyenv())

EPW_MORPH_BELCHER_METHOD_DEFAULTS <- c(
    tdb = "stretch",
    rh = "stretch",
    p = "stretch",
    hor_ir = "stretch",
    glob_rad = "stretch",
    wind = "stretch"
)

EPW_MORPH_BELCHER_CHANGE_FACTOR_METHOD_DEFAULTS <- c(
    tdb = "shift",
    rh = "shift",
    p = "shift",
    hor_ir = "stretch",
    glob_rad = "stretch",
    wind = "stretch"
)

EPW_MORPH_BELCHER_METHOD_CHOICES <- c("shift", "stretch", "combined")

EPW_MORPH_BELCHER_RULES <- data.table::data.table(
    step = c(
        "tdb",
        "rh",
        "p",
        "hor_ir",
        "glob_rad",
        "wind",
        "total_cover",
        "precip",
        "tdew",
        "diff_rad",
        "norm_rad",
        "opaque_cover",
        "precip_rate"
    ),
    epw_field = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "atmospheric_pressure",
        "horizontal_infrared_radiation_intensity_from_sky",
        "global_horizontal_radiation",
        "wind_speed",
        "total_sky_cover",
        "liquid_precip_depth",
        "dew_point_temperature",
        "diffuse_horizontal_radiation",
        "direct_normal_radiation",
        "opaque_sky_cover",
        "liquid_precip_rate"
    ),
    variable_id = c("tas", "hurs", "psl", "rlds", "rsds", "sfcWind", "clt", "pr", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    optional_variable_id = c("tasmax,tasmin", "hursmax,hursmin", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    method = c(EPW_MORPH_BELCHER_METHOD_DEFAULTS, "sky_cover", "precipitation", "derived", "derived", "derived", "derived", "derived"),
    required = c(rep(TRUE, 8L), rep(FALSE, 5L)),
    derived = c(rep(FALSE, 8L), rep(TRUE, 5L))
)

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
        #' @field requires_reference Whether the backend requires reference climate data.
        requires_reference = FALSE,

        #' @description
        #' Create an EPW morphing backend.
        #'
        #' @param name Backend name.
        #' @param label Human-readable backend label.
        #' @param methods Named default method vector.
        #' @param method_choices Allowed method values.
        #' @param rules Backend rule table.
        #' @param requires_reference Whether reference climate data are required.
        #' @param runner Function taking `(context, backend)` and returning an
        #'        `epw_morph_result`.
        initialize = function(name, label = NULL, methods = NULL, method_choices = NULL, rules,
                              requires_reference = FALSE, runner) {
            checkmate::assert_string(name, min.chars = 1L)
            checkmate::assert_string(label, null.ok = TRUE)
            checkmate::assert_flag(requires_reference)
            if (is.null(methods)) {
                methods <- stats::setNames(character(), character())
            }
            checkmate::assert_character(methods, any.missing = FALSE, names = "named")
            if (is.null(method_choices)) {
                method_choices <- unique(unname(methods))
            }
            checkmate::assert_character(method_choices, any.missing = FALSE)
            checkmate::assert_function(runner)
            self$name <- tolower(name)
            self$label <- if (is.null(label)) self$name else label
            self$requires_reference <- requires_reference
            private$method_defaults <- methods
            private$allowed_methods <- method_choices
            private$rule_table <- morpher__normalize_backend_rules(
                self$name,
                rules,
                method_defaults = private$method_defaults,
                method_choices = private$allowed_methods
            )
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
            private$runner(context, self)
        }
    ),
    private = list(
        method_defaults = NULL,
        allowed_methods = NULL,
        rule_table = NULL,
        runner = NULL
    )
)

morpher__default_backend_specs <- function() {
    list(
        belcher = EpwMorphBackend$new(
            name = "belcher",
            label = "Belcher change-factor statistical downscaling",
            methods = EPW_MORPH_BELCHER_CHANGE_FACTOR_METHOD_DEFAULTS,
            method_choices = EPW_MORPH_BELCHER_METHOD_CHOICES,
            rules = EPW_MORPH_BELCHER_RULES,
            requires_reference = TRUE,
            runner = morpher__belcher_run
        ),
        belcher_absolute = EpwMorphBackend$new(
            name = "belcher_absolute",
            label = "Belcher absolute-target statistical downscaling",
            methods = EPW_MORPH_BELCHER_METHOD_DEFAULTS,
            method_choices = EPW_MORPH_BELCHER_METHOD_CHOICES,
            rules = EPW_MORPH_BELCHER_RULES,
            runner = morpher__belcher_absolute_run
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
        "i" = "Use {.val belcher} with reference climate data for change-factor morphing."
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

#' EPW morphing variable sets
#'
#' @param level Variable set level, an [EpwMorphBackend] object, or an
#'        [epw_morph_recipe()] object.
#'
#' @return A character vector of CMIP variable IDs.
#' @export
epw_morph_variables <- function(level = c("recommended", "minimal", "extended")) {
    if (inherits(level, "epw_morph_recipe")) {
        return(morpher__rules_required_variables(morpher__recipe_rules(level)[required == TRUE & !derived]))
    }
    if (inherits(level, "EpwMorphBackend")) {
        return(level$required_variables())
    }
    if (is.character(level) && length(level) == 1L && !level %in% names(EPW_MORPH_VARIABLE_LEVELS)) {
        return(epw_morph_backend(level)$required_variables())
    }
    level <- match.arg(level)
    EPW_MORPH_VARIABLE_LEVELS[[level]]
}

#' EPW morphing recipe
#'
#' @param name Recipe name. Defaults to `"belcher"`.
#' @param backend Backend name. Defaults to `name`.
#' @param methods Optional named character vector overriding morphing methods for
#'        backend steps.
#'
#' @return A recipe list.
#' @export
epw_morph_recipe <- function(name = "belcher", backend = name, methods = NULL) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_string(backend, min.chars = 1L)
    name <- tolower(name)
    backend <- tolower(backend)
    backend_spec <- epw_morph_backend(backend)

    methods <- morpher__recipe_methods(methods, backend_spec)
    rules <- backend_spec$rules_with_methods(methods)

    structure(
        list(
            name = name,
            backend = backend,
            methods = methods,
            rules = rules
        ),
        class = "epw_morph_recipe"
    )
}

#' EPW morphing periods
#'
#' @param ... Named integer year vectors.
#'
#' @return A data.table with columns `period` and `year`.
#' @export
epw_morph_periods <- function(...) {
    periods <- list(...)
    if (!length(periods)) {
        cli::cli_abort("At least one named period must be supplied.")
    }
    nms <- names(periods)
    if (is.null(nms) || any(!nzchar(nms))) {
        cli::cli_abort("All EPW morphing periods must be named.")
    }

    rows <- lapply(seq_along(periods), function(i) {
        years <- periods[[i]]
        checkmate::assert_integerish(years, lower = 1900, any.missing = FALSE, min.len = 1L, unique = TRUE)
        data.table::data.table(period = nms[[i]], year = as.integer(sort(years)))
    })
    data.table::rbindlist(rows)
}

#' Create an EPW morpher
#'
#' @param store An [EsgStore] object.
#' @param epw EPW path or an [eplusr::Epw] object.
#' @param site_id Optional site identifier.
#' @param recipe EPW morphing recipe.
#' @param label Optional source label.
#'
#' @return An [EpwMorpher] object.
#' @export
epw_morpher <- function(store, epw, site_id = NULL, recipe = epw_morph_recipe("belcher"), label = NULL) {
    EpwMorpher$new(store = store, epw = epw, site_id = site_id, recipe = recipe, label = label)
}

morpher__now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}

morpher__json <- function(x) {
    if (inherits(x, "epw_morph_recipe")) {
        rules <- data.table::copy(x$rules)
        for (col in intersect(c("required_variables", "optional_variables", "method_choices"), names(rules))) {
            rules[[col]] <- vapply(rules[[col]], paste, character(1L), collapse = ",")
        }
        x <- list(
            name = x$name,
            backend = x$backend,
            methods = x$methods,
            rules = as.data.frame(rules)
        )
    }
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
}

morpher__recipe_rules <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    data.table::as.data.table(recipe$rules)
}

morpher__recipe_methods <- function(methods = NULL, backend = epw_morph_backend("belcher")) {
    if (!inherits(backend, "EpwMorphBackend")) {
        cli::cli_abort("`backend` must be an {.cls EpwMorphBackend} object.")
    }
    backend$validate_methods(methods)
}

morpher__recipe_requires_reference <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    epw_morph_backend(recipe$backend)$requires_reference
}

morpher__recipe_method_overrides <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    backend <- epw_morph_backend(recipe$backend)
    methods <- recipe$methods
    if (is.null(methods)) {
        return(NULL)
    }
    defaults <- backend$methods()
    overrides <- methods[names(methods) %in% names(defaults) & methods != defaults[names(methods)]]
    if (!length(overrides)) NULL else overrides
}

morpher__hash <- function(...) {
    store__hash(...)
}

morpher__hash_rows <- function(...) {
    args <- list(...)
    n <- max(vapply(args, length, integer(1L)), 0L)
    if (!n) {
        return(character())
    }
    args <- lapply(args, function(x) {
        if (length(x) == n) {
            return(x)
        }
        if (length(x) == 1L) {
            return(rep(x, n))
        }
        cli::cli_abort("Cannot recycle morphing hash input of length {length(x)} to {n}.")
    })
    vapply(seq_len(n), function(i) {
        do.call(morpher__hash, lapply(args, `[[`, i))
    }, character(1L))
}

morpher__private_store <- function(store) {
    if (!inherits(store, "EsgStore")) {
        cli::cli_abort("`store` must be an {.cls EsgStore} object.")
    }
    private <- priv(store)
    private$check_open()
    private
}

morpher__replace_rows <- function(store, table, rows, key) {
    morpher__private_store(store)$replace_rows(table, as.data.frame(rows), key)
    invisible(rows)
}

morpher__read_table <- function(store, table) {
    morpher__private_store(store)$read_table(table)
}

morpher__delete_by_key <- function(store, table, key, values) {
    morpher__private_store(store)$delete_by_key(table, key, values)
    invisible(NULL)
}

morpher__units_label <- function(x) {
    if (!inherits(x, "units")) {
        return(NA_character_)
    }
    out <- tryCatch(units::deparse_unit(x), error = function(e) NA_character_)
    if (length(out) != 1L || is.na(out) || !nzchar(out)) NA_character_ else out
}

morpher__drop_units <- function(x) {
    if (inherits(x, "units")) {
        return(as.numeric(units::drop_units(x)))
    }
    as.numeric(x)
}

morpher__unit_alias <- function(x) {
    x <- store__chr1(x)
    if (is.na(x) || !nzchar(x)) {
        return(NA_character_)
    }
    switch(
        x,
        "K" = "K",
        "kelvin" = "K",
        "\u00b0C" = "degC",
        "degC" = "degC",
        "degree_Celsius" = "degC",
        "C" = "degC",
        "celsius" = "degC",
        "%" = "%",
        "percent" = "%",
        "Pa" = "Pa",
        "pascal" = "Pa",
        "W/m2" = "W/m^2",
        "W m-2" = "W/m^2",
        "W h m-2" = "W/m^2",
        "Wh/m2" = "W/m^2",
        "Wh/m^2" = "W/m^2",
        "m s-1" = "m/s",
        "m/s" = "m/s",
        "mm" = "mm",
        "millimeter" = "mm",
        "millimetre" = "mm",
        "kg m-2 s-1" = "kg m-2 s-1",
        "kg m^-2 s^-1" = "kg m-2 s-1",
        x
    )
}

morpher__convert_value <- function(value, from, to) {
    morpher__convert_value_checked(value, from, to)$value
}

morpher__convert_value_checked <- function(value, from, to) {
    from <- morpher__unit_alias(from)
    to <- morpher__unit_alias(to)
    if (is.na(value) || is.na(from) || is.na(to) || !nzchar(from) || !nzchar(to) || identical(from, to)) {
        return(list(value = as.numeric(value), ok = TRUE, message = NA_character_))
    }
    if (identical(from, "K") && identical(to, "degC")) {
        return(list(value = as.numeric(value) - 273.15, ok = TRUE, message = NA_character_))
    }
    if (identical(from, "degC") && identical(to, "K")) {
        return(list(value = as.numeric(value) + 273.15, ok = TRUE, message = NA_character_))
    }
    tryCatch(
        list(
            value = as.numeric(units::drop_units(units::set_units(units::set_units(value, from, mode = "standard"), to, mode = "standard"))),
            ok = TRUE,
            message = NA_character_
        ),
        error = function(e) list(
            value = as.numeric(value),
            ok = FALSE,
            message = conditionMessage(e)
        )
    )
}

morpher__default_epw_units <- function(field) {
    switch(
        field,
        dry_bulb_temperature = "degC",
        relative_humidity = "%",
        atmospheric_pressure = "Pa",
        horizontal_infrared_radiation_intensity_from_sky = "W/m^2",
        global_horizontal_radiation = "W/m^2",
        wind_speed = "m/s",
        total_sky_cover = NA_character_,
        liquid_precip_depth = "mm",
        liquid_precip_rate = "h",
        NA_character_
    )
}

morpher__case_columns <- function() {
    c("source_id", "experiment_id", "variant_label", "period")
}

morpher__safe_path <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "unknown"
    gsub("[^A-Za-z0-9_.=-]+", "-", x)
}

morpher__parquet_read <- function(store, path) {
    conn <- morpher__private_store(store)$conn
    data.table::as.data.table(ddb_query(conn, sprintf(
        "SELECT * FROM read_parquet(%s)",
        ddb_literal(conn, path)
    )))
}

morpher__monthly_long <- function(data, id_cols, value_cols, units_map) {
    rows <- list()
    for (field in value_cols) {
        if (!field %in% names(data)) {
            next
        }
        value <- morpher__drop_units(data[[field]])
        units <- units_map[[field]]
        dt <- data.table::data.table(month = data$month, value = value)
        summary <- dt[, .(
            mean = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE)
        ), by = "month"]
        summary <- data.table::melt(
            summary,
            id.vars = "month",
            variable.name = "stat",
            value.name = "value",
            variable.factor = FALSE
        )
        summary[, `:=`(
            epw_field = field,
            units = if (is.null(units)) NA_character_ else units
        )]
        rows[[length(rows) + 1L]] <- summary
    }
    if (!length(rows)) {
        return(data.table::data.table())
    }
    out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    data.table::setcolorder(out, c("epw_field", "month", "stat", "value", "units"))
    out
}

morpher__stat_rows <- function(dt) {
    mean_rows <- dt[, .(
        value = mean(value, na.rm = TRUE),
        lon = if ("lon" %in% names(.SD)) mean(lon, na.rm = TRUE) else NA_real_,
        lat = if ("lat" %in% names(.SD)) mean(lat, na.rm = TRUE) else NA_real_,
        n_records = .N
    ), by = .(plan_id, site_id, source_id, experiment_id, variant_label, frequency, table_id, variable_id, period, month, units)]
    mean_rows[, stat := "mean"]

    min_rows <- dt[, .(
        value = min(value, na.rm = TRUE),
        lon = if ("lon" %in% names(.SD)) mean(lon, na.rm = TRUE) else NA_real_,
        lat = if ("lat" %in% names(.SD)) mean(lat, na.rm = TRUE) else NA_real_,
        n_records = .N
    ), by = .(plan_id, site_id, source_id, experiment_id, variant_label, frequency, table_id, variable_id, period, month, units)]
    min_rows[, stat := "min"]

    max_rows <- dt[, .(
        value = max(value, na.rm = TRUE),
        lon = if ("lon" %in% names(.SD)) mean(lon, na.rm = TRUE) else NA_real_,
        lat = if ("lat" %in% names(.SD)) mean(lat, na.rm = TRUE) else NA_real_,
        n_records = .N
    ), by = .(plan_id, site_id, source_id, experiment_id, variant_label, frequency, table_id, variable_id, period, month, units)]
    max_rows[, stat := "max"]

    data.table::rbindlist(list(mean_rows, min_rows, max_rows), use.names = TRUE, fill = TRUE)
}

morpher__field_units <- function(data, fields) {
    stats::setNames(lapply(fields, function(field) morpher__units_label(data[[field]])), fields)
}

morpher__get_epw_path <- function(epw) {
    path <- tryCatch(epw$path(), error = function(e) NULL)
    if (is.null(path) || !length(path) || is.na(path[[1L]]) || !nzchar(path[[1L]])) {
        cli::cli_abort("An {.cls eplusr::Epw} object used by {.cls EpwMorpher} must have a file path.")
    }
    path[[1L]]
}

morpher__diagnostic_columns <- function() {
    c(
        "stage", "severity", "code", "message", "plan_id", "summary_id",
        "baseline_id", "morph_id", "case_id", "variable_id", "epw_field",
        "period", "month", "action"
    )
}

morpher__empty_diagnostics <- function() {
    out <- data.table::data.table(
        stage = character(),
        severity = character(),
        code = character(),
        message = character(),
        plan_id = character(),
        summary_id = character(),
        baseline_id = character(),
        morph_id = character(),
        case_id = character(),
        variable_id = character(),
        epw_field = character(),
        period = character(),
        month = integer(),
        action = character()
    )
    out[, morpher__diagnostic_columns(), with = FALSE]
}

morpher__diagnostic <- function(stage, severity, code, message, plan_id = NA_character_,
                                 summary_id = NA_character_, baseline_id = NA_character_,
                                 morph_id = NA_character_, case_id = NA_character_,
                                 variable_id = NA_character_, epw_field = NA_character_,
                                 period = NA_character_, month = NA_integer_,
                                 action = NA_character_) {
    out <- data.table::data.table(
        stage = stage,
        severity = severity,
        code = code,
        message = message,
        plan_id = store__chr1(plan_id),
        summary_id = store__chr1(summary_id),
        baseline_id = store__chr1(baseline_id),
        morph_id = store__chr1(morph_id),
        case_id = store__chr1(case_id),
        variable_id = store__chr1(variable_id),
        epw_field = store__chr1(epw_field),
        period = store__chr1(period),
        month = as.integer(month),
        action = store__chr1(action)
    )
    out[, morpher__diagnostic_columns(), with = FALSE]
}

morpher__bind_diagnostics <- function(...) {
    args <- list(...)
    parts <- list()
    for (arg in args) {
        if (is.data.frame(arg)) {
            parts[[length(parts) + 1L]] <- arg
        } else if (is.list(arg)) {
            for (part in arg) {
                if (is.data.frame(part)) {
                    parts[[length(parts) + 1L]] <- part
                }
            }
        }
    }
    parts <- parts[vapply(parts, nrow, integer(1L)) > 0L]
    if (!length(parts)) {
        return(morpher__empty_diagnostics())
    }
    out <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
    out[, morpher__diagnostic_columns(), with = FALSE]
}

morpher__abort_diagnostics <- function(diagnostics, message = "EPW morphing preflight has blocking issues.") {
    errors <- diagnostics[diagnostics$severity == "error"]
    if (!nrow(errors)) {
        return(invisible(NULL))
    }
    cli::cli_abort(c(
        message,
        "x" = "{errors$message[[1L]]}"
    ))
}

morpher__json_int_vector <- function(x) {
    as.integer(jsonlite::fromJSON(x))
}

morpher__engine_by_columns <- function(by) {
    map <- c(
        source_id = "source_id",
        experiment_id = "experiment_id",
        variant_label = "member_id",
        period = "interval",
        table_id = "table_id",
        frequency = "table_id"
    )
    unique(unname(map[intersect(by, names(map))]))
}

morpher__reference_case_by <- function(by) {
    setdiff(by, c("experiment_id", "period"))
}

morpher__normalize_context_climate <- function(climate, years = NULL, labels = NULL) {
    climate <- data.table::as.data.table(data.table::copy(climate))
    if (!"time" %in% names(climate) && "datetime" %in% names(climate)) {
        climate[, time := datetime]
    }
    if ("variable" %in% names(climate)) {
        climate[, variable_id := variable]
    }
    if (!"variant_label" %in% names(climate) && "member_id" %in% names(climate)) {
        climate[, variant_label := member_id]
    }
    if (!"period" %in% names(climate) && "interval" %in% names(climate)) {
        climate[, period := as.character(interval)]
    }
    if (!"year" %in% names(climate) && "time" %in% names(climate)) {
        climate[, year := as.integer(format(time, "%Y", tz = "UTC"))]
    }
    if (!"period" %in% names(climate) && "year" %in% names(climate)) {
        if (!is.null(years) && !is.null(labels)) {
            label_map <- data.table::data.table(year = as.integer(years), period = as.character(labels))
            climate <- label_map[climate, on = "year"]
        } else {
            climate[, period := as.character(year)]
        }
    }
    if (!"units" %in% names(climate)) {
        climate[, units := NA_character_]
    }
    for (col in c("lon", "lat")) {
        if (!col %in% names(climate)) {
            climate[, (col) := NA_real_]
        }
    }
    climate[]
}

morpher__context <- function(epw, climate, recipe = epw_morph_recipe("belcher"),
                              reference_climate = NULL, years = NULL, labels = NULL,
                              reference_years = NULL, reference_labels = NULL,
                              by = character(),
                              case = NULL, strict = TRUE, warning = FALSE) {
    if (!inherits(epw, "Epw")) {
        cli::cli_abort("`epw` must be an {.cls eplusr::Epw} object.")
    }
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    climate <- morpher__normalize_context_climate(climate, years = years, labels = labels)
    if (!is.null(reference_climate)) {
        reference_climate <- morpher__normalize_context_climate(
            reference_climate,
            years = reference_years,
            labels = reference_labels
        )
    }
    checkmate::assert_character(by, any.missing = FALSE, unique = TRUE)
    structure(
        list(
            epw = epw$clone(),
            climate = climate,
            reference_climate = reference_climate,
            recipe = recipe,
            years = years,
            labels = labels,
            reference_years = reference_years,
            reference_labels = reference_labels,
            by = by,
            case = case,
            strict = strict,
            warning = warning
        ),
        class = "morpher__context"
    )
}

morpher__context_required_columns <- function() {
    c("variable_id", "time", "period", "year", "lon", "lat", "units", "value")
}

morpher__validate_context <- function(context) {
    checkmate::assert_class(context, "morpher__context")
    climate <- context$climate
    missing <- setdiff(morpher__context_required_columns(), names(climate))
    if (length(missing)) {
        cli::cli_abort("Canonical EPW morphing climate data are missing column(s): {.val {missing}}.")
    }
    invisible(context)
}

morpher__context_variable <- function(context, variable_id) {
    morpher__validate_context(context)
    target_variable_id <- store__chr1(variable_id)
    climate <- context$climate
    climate[climate[["variable_id"]] == target_variable_id]
}

morpher__context_reference_variable <- function(context, variable_id) {
    morpher__validate_context(context)
    if (is.null(context$reference_climate)) {
        return(data.table::data.table())
    }
    missing <- setdiff(morpher__context_required_columns(), names(context$reference_climate))
    if (length(missing)) {
        cli::cli_abort("Canonical EPW morphing reference climate data are missing column(s): {.val {missing}}.")
    }
    target_variable_id <- store__chr1(variable_id)
    climate <- context$reference_climate
    climate[climate[["variable_id"]] == target_variable_id]
}

morpher__context_year_labels <- function(context) {
    if (!is.null(context$years)) {
        return(list(years = context$years, labels = context$labels))
    }
    climate <- context$climate
    if (!all(c("year", "period") %in% names(climate))) {
        return(list(years = NULL, labels = NULL))
    }
    period_years <- unique(climate[, .(year = as.integer(year), period = as.character(period))])
    data.table::setorder(period_years, year, period)
    years <- sort(unique(period_years$year))
    labels <- vapply(years, function(year) {
        period_years$period[match(year, period_years$year)]
    }, character(1L))
    list(years = years, labels = labels)
}

morpher__context_pick_column <- function(data, primary, fallback = NULL, default = NA_character_) {
    if (primary %in% names(data)) {
        return(data[[primary]])
    }
    if (!is.null(fallback) && fallback %in% names(data)) {
        return(data[[fallback]])
    }
    rep(default, nrow(data))
}

morpher__context_identity_rows <- function(data) {
    data.table::data.table(
        activity_drs = store__chr(morpher__context_pick_column(data, "activity_drs", "activity_id")),
        institution_id = store__chr(morpher__context_pick_column(data, "institution_id")),
        source_id = store__chr(morpher__context_pick_column(data, "source_id")),
        experiment_id = store__chr(morpher__context_pick_column(data, "experiment_id")),
        member_id = store__chr(morpher__context_pick_column(data, "member_id", "variant_label")),
        table_id = store__chr(morpher__context_pick_column(data, "table_id", "frequency")),
        lon = as.numeric(morpher__context_pick_column(data, "lon", default = NA_real_)),
        lat = as.numeric(morpher__context_pick_column(data, "lat", default = NA_real_))
    )
}

morpher__monthly_climate <- function(data, years = NULL, labels = NULL, warning = FALSE) {
    data <- data.table::as.data.table(data.table::copy(data))
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    missing <- setdiff(c("variable_id", "time", "year", "period", "units", "value", "lon", "lat"), names(data))
    if (length(missing)) {
        cli::cli_abort("Canonical EPW morphing climate data are missing column(s): {.val {missing}}.")
    }
    data[, `:=`(
        year = as.integer(year),
        month = data.table::month(time),
        day = data.table::mday(time)
    )]
    data <- data[!(month == 2L & day == 29L)]

    checkmate::assert_integerish(years, lower = 1900, unique = TRUE, sorted = TRUE, any.missing = FALSE, null.ok = TRUE)
    if (is.null(years)) {
        data[, interval := as.factor(period)]
    } else {
        years <- as.integer(years)
        missing_years <- setdiff(years, unique(data$year))
        if (length(missing_years)) {
            stop("Input data does not contain any data of year ", paste0("'", missing_years, "'", collapse = ", "), ".")
        }
        data <- data[year %in% years]
        if (is.null(labels)) {
            data[, interval := as.factor(year)]
        } else {
            if (is.factor(labels)) {
                labels <- as.character(labels)
            }
            checkmate::assert_character(labels, any.missing = FALSE, len = length(years))
            label_map <- data.table::data.table(year = years, interval = as.factor(labels))
            data <- label_map[data, on = "year"]
        }
    }

    if (isTRUE(warning)) {
        by_cols <- intersect(c("variable_id", "table_id", "frequency", "source_id", "experiment_id", "variant_label", "member_id"), names(data))
        if (length(by_cols)) {
            rng_year <- data[, list(years = list(sort(unique(year))), num_years = length(unique(year))), by = by_cols][num_years < 10L]
            if (nrow(rng_year)) {
                data.table::set(rng_year, NULL, "index_case", seq.int(nrow(rng_year)))
                mes <- rng_year[, by = "index_case", {
                    yrs <- paste0("'", years[[1L]], "'", collapse = ", ")
                    list(message = sprintf("#%i | [%i] %s", .BY$index_case, num_years, yrs))
                }]$message
                warning("Case(s) shown below contains CMIP data less than a decade. ",
                    "The morphed data may not be able to capture average weather of the future climate.\n",
                    paste0(mes, collapse = "\n"), call. = FALSE)
            }
        }
    }

    identity <- morpher__context_identity_rows(data)
    data <- data.table::data.table(
        identity,
        units = as.character(data$units),
        value = as.numeric(data$value),
        month = as.integer(data$month),
        interval = data$interval
    )
    group_cols <- c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id", "units", "month", "interval")
    out <- data[, .(
        lon = mean(lon, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        value = mean(value, na.rm = TRUE)
    ), by = group_cols]
    unit <- out$units[!is.na(out$units) & nzchar(out$units)][1L]
    if (length(unit) && !is.na(unit)) {
        data.table::set(out, NULL, "value", units::set_units(out$value, unit, mode = "standard"))
    }
    data.table::setcolorder(out, c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id", "lon", "lat", "units", "value", "month", "interval"))
    out[]
}

morpher__engine_complete_data <- function(epw, parts, by = character()) {
    suppressMessages(epw$drop_unit())
    data_epw <- data.table::as.data.table(epw$data())

    parts <- parts[vapply(parts, nrow, integer(1L)) > 0L]
    if (!length(parts)) {
        return(data.table::data.table())
    }

    parts <- lapply(parts, function(dt) {
        dt <- data.table::copy(dt)
        drop <- intersect(c("delta", "alpha"), names(dt))
        if (length(drop)) {
            data.table::set(dt, j = drop, value = NULL)
        }
        dt
    })

    cols_dt <- c("datetime", "year", "month", "day", "hour", "minute")
    cols_by <- intersect(morpher__engine_by_columns(by), Reduce(intersect, lapply(parts, names)))
    keep_base <- c(cols_by, cols_dt)
    for (i in seq_along(parts)) {
        keep <- c(intersect(names(data_epw), names(parts[[i]])), keep_base)
        drop <- setdiff(names(parts[[i]]), keep)
        if (length(drop)) {
            data.table::set(parts[[i]], j = drop, value = NULL)
        }
    }

    merge_by <- c(cols_by, cols_dt)
    merged <- Reduce(function(x, y) merge(x, y, by = merge_by), parts)
    merged <- merged[, lapply(.SD, mean), by = merge_by]

    if ("total_sky_cover" %in% names(merged)) {
        data.table::set(merged, j = "total_sky_cover", value = as.integer(round(merged$total_sky_cover)))
    }
    if ("opaque_sky_cover" %in% names(merged)) {
        data.table::set(merged, j = "opaque_sky_cover", value = as.integer(round(merged$opaque_sky_cover)))
    }

    value_cols <- setdiff(intersect(names(merged), names(data_epw)), cols_dt)
    complete_base <- data.table::copy(data_epw)
    complete_base[, .epw_order := .I]
    complete <- merge(
        complete_base,
        merged,
        by = cols_dt,
        all.x = TRUE,
        sort = FALSE,
        suffixes = c("", ".morphed")
    )
    for (col in value_cols) {
        morphed_col <- paste0(col, ".morphed")
        if (morphed_col %in% names(complete)) {
            idx <- !is.na(complete[[morphed_col]])
            data.table::set(complete, i = which(idx), j = col, value = complete[[morphed_col]][idx])
            data.table::set(complete, j = morphed_col, value = NULL)
        }
    }
    data.table::setorder(complete, .epw_order)
    data.table::set(complete, j = ".epw_order", value = NULL)
    complete[]
}

#' Create an EPW morphing backend result
#'
#' @description
#' Backend runner functions return `epw_morph_result` objects. Use
#' `epw_morph_result()` in custom backends after producing complete hourly EPW
#' weather data.
#'
#' @param context Canonical EPW morphing context supplied to the backend runner.
#' @param epw EPW object associated with the result.
#' @param data Complete hourly EPW weather data ready for Parquet output or
#'        EPW writing.
#' @param parts Optional named list of intermediate backend result tables.
#' @param diagnostics Optional backend diagnostic rows.
#' @param factors Optional backend factor rows.
#'
#' @return An `epw_morph_result` object.
#' @export
epw_morph_result <- function(context, epw = context$epw, data, parts = list(),
                             diagnostics = morpher__empty_diagnostics(), factors = NULL) {
    checkmate::assert_class(context, "morpher__context")
    if (!inherits(epw, "Epw")) {
        cli::cli_abort("`epw` must be an {.cls eplusr::Epw} object.")
    }
    if (missing(data)) {
        cli::cli_abort("`data` must be supplied.")
    }
    checkmate::assert_list(parts, names = "named")
    data <- data.table::as.data.table(data.table::copy(data))
    morpher__engine_output(context, epw, parts = parts, data = data, diagnostics = diagnostics, factors = factors)
}

morpher__engine_output <- function(context, epw, parts, data = NULL, diagnostics = morpher__empty_diagnostics(), factors = NULL) {
    if (is.null(data)) {
        data <- morpher__engine_complete_data(epw, parts, by = context$by)
    }
    structure(
        list(
            backend = context$recipe$backend,
            recipe = context$recipe,
            epw = epw,
            data = data,
            parts = parts,
            diagnostics = diagnostics,
            factors = factors
        ),
        class = "epw_morph_result"
    )
}

morpher__result_as_morphed <- function(result) {
    out <- c(list(epw = result$epw), result$parts)
    class(out) <- "epw_cmip6_morphed"
    out
}

morpher__run_context <- function(context) {
    checkmate::assert_class(context, "morpher__context")
    backend <- epw_morph_backend(context$recipe$backend)
    result <- backend$run(context)
    if (!inherits(result, "epw_morph_result")) {
        cli::cli_abort("EPW morphing backend {.val {backend$name}} did not return an {.cls epw_morph_result}.")
    }
    result
}

morpher__belcher_monthly_variable <- function(context, variable_id) {
    data <- morpher__context_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    year_labels <- morpher__context_year_labels(context)
    morpher__monthly_climate(
        data,
        years = year_labels$years,
        labels = year_labels$labels,
        warning = context$warning
    )
}

morpher__belcher_monthly_reference_variable <- function(context, variable_id) {
    data <- morpher__context_reference_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    morpher__monthly_climate(
        data,
        years = context$reference_years,
        labels = context$reference_labels,
        warning = context$warning
    )
}

morpher__belcher_epw_monthly <- function(data_epw, var, keep_units = TRUE) {
    monthly <- data_epw[,
        list(val_mean = mean(get(var)), val_max = max(get(var)), val_min = min(get(var))),
        by = "month"
    ]

    if (keep_units && inherits(data_epw[[var]], "units")) {
        u <- units::deparse_unit(data_epw[[var]])
        monthly[, `:=`(
            val_mean = units::set_units(val_mean, u, mode = "standard"),
            val_max = units::set_units(val_max, u, mode = "standard"),
            val_min = units::set_units(val_min, u, mode = "standard")
        )]
    }

    monthly
}

morpher__belcher_align_units <- function(data, target_units) {
    data.table::set(data, NULL, "value", units::set_units(data$value, target_units, mode = "standard"))
    data
}

morpher__belcher_drop_units <- function(data, vars) {
    for (var in c(vars, "delta", "alpha")) {
        if (var %in% names(data) && inherits(data[[var]], "units")) {
            data.table::set(data, NULL, var, units::drop_units(data[[var]]))
        }
    }
    data
}

morpher__belcher_to_radian <- function(degree) {
    degree * pi / 180
}

morpher__belcher_day_angle <- function(day_of_year) {
    2.0 * pi * (day_of_year - 1.0) / 365.0
}

morpher__belcher_equation_of_time <- function(day_of_year) {
    d <- morpher__belcher_day_angle(day_of_year)
    (-7.659 * sin(d) + 9.863 * sin(2.0 * d + 3.5932)) / 60.0
}

morpher__belcher_solar_time <- function(longitude, day_of_year, hour, timezone) {
    local_standard_time <- (hour - 0.5) %% 24.0
    local_standard_time + (longitude - timezone * 15.0) / 15.0 +
        morpher__belcher_equation_of_time(day_of_year)
}

morpher__belcher_hour_angle <- function(longitude, day_of_year, hour, timezone) {
    solar_time <- morpher__belcher_solar_time(longitude, day_of_year, hour, timezone)
    360 / 24 * (solar_time - 12)
}

morpher__belcher_declination <- function(day_of_year) {
    d <- morpher__belcher_day_angle(day_of_year)
    0.006918 -
        0.399912 * cos(d) +
        0.070257 * sin(d) -
        0.006758 * cos(2.0 * d) +
        0.000907 * sin(2.0 * d) -
        0.002697 * cos(3.0 * d) +
        0.00148 * sin(3.0 * d)
}

morpher__belcher_solar_angle <- function(latitude, longitude, day_of_year, hour, timezone) {
    declination <- morpher__belcher_declination(day_of_year)
    hour_angle <- morpher__belcher_hour_angle(longitude, day_of_year, hour, timezone)
    sin(morpher__belcher_to_radian(latitude)) * sin(declination) +
        cos(morpher__belcher_to_radian(latitude)) * cos(declination) *
        cos(morpher__belcher_to_radian(hour_angle))
}

morpher__belcher_tdew <- function(tdb, rh) {
    psychrolib::SetUnitSystem("SI")

    tdew <- data.table::copy(tdb)[
        rh, on = c(setdiff(names(tdb), c("dry_bulb_temperature", "delta", "alpha"))),
        relative_humidity := i.relative_humidity
    ]

    tdew[!is.na(dry_bulb_temperature) & !is.na(relative_humidity),
        dew_point_temperature := units::set_units(
            psychrolib::GetTDewPointFromRelHum(
                units::drop_units(dry_bulb_temperature),
                units::drop_units(relative_humidity) / 100
            ),
            "degree_Celsius",
            mode = "standard"
        )
    ]

    data.table::set(tdew, NULL, c("delta", "alpha"), NA_real_)
    data.table::set(tdew, NULL, c("dry_bulb_temperature", "relative_humidity"), NULL)

    data.table::setcolorder(tdew,
        c(setdiff(names(tdew), c("dew_point_temperature", "delta", "alpha")),
          "dew_point_temperature", "delta", "alpha")
    )

    tdew
}

morpher__belcher_diffuse_radiation <- function(data_epw, glob_rad) {
    diff_rad <- data.table::copy(glob_rad)
    if (!nrow(diff_rad)) {
        return(data.table::data.table())
    }
    diff_rad[data_epw[, .SD, .SDcols = c("month", "day", "hour", "diffuse_horizontal_radiation")],
        on = c("month", "day", "hour"),
        diffuse_horizontal_radiation := i.diffuse_horizontal_radiation * alpha
    ]
    diff_rad[, global_horizontal_radiation := NULL]
    diff_rad[, diffuse_horizontal_radiation := units::set_units(units::drop_units(
        diffuse_horizontal_radiation
    ), "W/m^2")][]
}

morpher__epw_location_numeric <- function(epw, names, default = NA_real_) {
    loc <- tryCatch(epw$location(), error = function(e) NULL)
    if (is.null(loc)) {
        return(default)
    }
    for (name in names) {
        if (name %in% names(loc)) {
            value <- suppressWarnings(as.numeric(loc[[name]][[1L]]))
            if (length(value) && !is.na(value) && is.finite(value)) {
                return(value)
            }
        }
    }
    default
}

morpher__belcher_direct_normal_radiation <- function(glob_rad, diff_rad, latitude = NULL,
                                                      longitude = NULL, timezone = NULL) {
    norm_rad <- data.table::copy(glob_rad)
    if (!nrow(glob_rad) || !nrow(diff_rad)) {
        return(data.table::data.table())
    }
    norm_rad[, diffuse_horizontal_radiation := diff_rad$diffuse_horizontal_radiation]
    norm_rad[, day_of_year := data.table::yday(datetime)]
    if (!is.null(latitude) && !is.na(latitude)) {
        norm_rad[, lat_calc := latitude]
    } else {
        norm_rad[, lat_calc := lat]
    }
    if (!is.null(longitude) && !is.na(longitude)) {
        norm_rad[, lon_calc := longitude]
    } else {
        norm_rad[, lon_calc := lon]
    }
    if (is.null(timezone) || is.na(timezone)) {
        timezone <- 0
    }
    norm_rad[, solar_angle := morpher__belcher_solar_angle(lat_calc, lon_calc, day_of_year, hour, timezone)]
    ghi <- morpher__drop_units(norm_rad$global_horizontal_radiation)
    dhi <- morpher__drop_units(norm_rad$diffuse_horizontal_radiation)
    sin_altitude <- norm_rad$solar_angle
    dni <- ifelse(
        is.finite(sin_altitude) & sin_altitude > 0 & is.finite(ghi) & ghi > 0,
        pmax(0, (ghi - dhi) / sin_altitude),
        0
    )
    dni <- pmin(dni, pmax(0, ghi) * 3)
    norm_rad[, direct_normal_radiation := units::set_units(dni, "W/m^2")]
    norm_rad[, c("global_horizontal_radiation", "diffuse_horizontal_radiation", "day_of_year", "solar_angle") := NULL]
    norm_rad[, c("lat_calc", "lon_calc") := NULL]
}

morpher__belcher_opaque_sky_cover <- function(data_epw, total_sky_cover) {
    if (!nrow(total_sky_cover)) {
        return(data.table::data.table())
    }
    data <- data.table::copy(total_sky_cover)[
        data_epw[, .SD, .SDcols = c("month", "day", "hour", "opaque_sky_cover", "total_sky_cover")],
        on = c("month", "day", "hour"),
        `:=`(
            baseline_opaque_sky_cover = i.opaque_sky_cover,
            baseline_total_sky_cover = i.total_sky_cover
        )
    ]
    data[, opaque_sky_cover := ifelse(
        baseline_total_sky_cover == 0,
        as.integer(round(total_sky_cover / 2.0)),
        as.integer(round(total_sky_cover * baseline_opaque_sky_cover / baseline_total_sky_cover))
    )]
    data[opaque_sky_cover > total_sky_cover, opaque_sky_cover := total_sky_cover]
    data[opaque_sky_cover < 0, opaque_sky_cover := 0L]
    data[, c("total_sky_cover", "baseline_opaque_sky_cover", "baseline_total_sky_cover") := NULL]

    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        "opaque_sky_cover", "delta", "alpha"
    )]
}

morpher__belcher_from_monthly <- function(var, data_epw, data_mean, data_max = NULL, data_min = NULL,
                                           type = c("shift", "stretch", "combined")) {
    type <- match.arg(type)
    if (!nrow(data_mean)) {
        return(data.table::data.table())
    }

    monthly <- morpher__belcher_epw_monthly(data_epw, var)
    u <- units::deparse_unit(data_epw[[var]])
    data_mean <- morpher__belcher_align_units(data.table::copy(data_mean), u)

    case_fallback <- data.table::data.table()
    if (identical(type, "combined") && !is.null(data_max) && !is.null(data_min)) {
        data_max <- morpher__belcher_align_units(data.table::copy(data_max), u)
        data_min <- morpher__belcher_align_units(data.table::copy(data_min), u)
        join_cols <- c(
            "activity_drs", "institution_id", "source_id", "experiment_id",
            "member_id", "table_id", "lat", "lon", "units", "month",
            "interval"
        )
        data_mean[data_max, on = join_cols, value_max := i.value]
        data_mean[data_min, on = join_cols, value_min := i.value]

        i_max <- data_mean[J(NA_real_), on = "value_max", which = TRUE, nomatch = NULL]
        i_min <- data_mean[J(NA_real_), on = "value_min", which = TRUE, nomatch = NULL]
        i <- unique(c(i_min, i_max))
        if (length(i)) {
            cols <- c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id")
            case_fallback <- unique(data_mean[i], by = cols)
            data.table::set(case_fallback, NULL, setdiff(names(case_fallback), cols), NULL)
            cases <- case_fallback[, unique(sprintf(
                "CMIP6.%s.%s.%s.%s.%s.%s",
                activity_drs, institution_id, source_id, experiment_id, member_id, table_id
            ))]
            cases <- sprintf("[%i] '%s'", seq_along(cases), sort(cases))
            warning(sprintf(
                "Case(s) below contains missing values of max or min of '%s' data. ",
                gsub("_", " ", var)
            ),
            "'Shift' method will be used for it.\n", paste0(cases, collapse = "\n"),
            call. = FALSE
            )
        }
    }

    data_mean[monthly, on = "month", `:=`(
        delta = value - val_mean,
        alpha = value / val_mean,
        epw_mean = i.val_mean,
        epw_max = i.val_max,
        epw_min = i.val_min
    )]

    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE
    ]

    if (identical(type, "combined") && all(c("value_min", "value_max") %in% names(data))) {
        data[, alpha := ((value_max - epw_max) - (value_min - epw_min)) / (epw_max - epw_min)]
        if (nrow(case_fallback)) {
            data[case_fallback, on = names(case_fallback), alpha := 0.0]
        }
    } else {
        data[, alpha := value / epw_mean]
    }

    thres_alpha <- getOption("epwshiftr.threshold_alpha")
    if (!checkmate::test_number(thres_alpha, lower = 0)) {
        warning(paste0(
            "The threshold value for the monthly-mean fractional change (Alpha) ",
            "should be a positive number, but '",
            if (is.null(thres_alpha)) "NULL" else thres_alpha,
            "' is found."
        ))
    }
    if (type %in% c("stretch", "combined") && nrow(abnorm_alpha <- data_mean[abs(morpher__drop_units(alpha)) > thres_alpha])) {
        warning(sprintf(
            paste(
                "The absolute values of monthly-mean fractional change (Alpha) below",
                "for '%s' has exceeded the threshold (%s) set by the option",
                "'epwshiftr.threshold_alpha'. 'Shift' morphing method will be utilized",
                "instead of '%s' method to avoid unrealistic values. It is highly",
                "suggested to further investigate the input data.\n%s",
                collapse = " "
            ),
            gsub("_", " ", var, fixed = TRUE), thres_alpha, type,
            paste0(sprintf(
                "Month = %s | Monthly-mean: EPW = %s, GCM = %s --> Alpha = %s",
                format(abnorm_alpha$month),
                format(abnorm_alpha$epw_mean, digits = 3),
                format(abnorm_alpha$value, digits = 3),
                format(morpher__drop_units(abnorm_alpha$alpha), digits = 3)
            ), collapse = "\n")
        ))
        type <- "shift"
    }

    if (identical(type, "shift")) {
        data[, c(var) := units::set_units(get(var) + delta, u, mode = "standard")]
    } else if (identical(type, "stretch")) {
        data[, c(var) := units::set_units(get(var) * alpha, u, mode = "standard")]
    } else if (identical(type, "combined")) {
        if (all(c("value_min", "value_max") %in% names(data))) {
            data[, c(var) := units::set_units(get(var) + delta + alpha * (get(var) - epw_mean), u, mode = "standard")]
        } else {
            data[, c(var) := units::set_units(get(var) + delta + alpha * get(var), u, mode = "standard")]
        }
    }

    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        var, "delta", "alpha"
    )]
}

morpher__belcher_reference_join_cols <- function(target, reference) {
    cols <- c("institution_id", "source_id", "member_id", "table_id", "month")
    cols <- intersect(cols, intersect(names(target), names(reference)))
    if (!"month" %in% cols && "month" %in% names(target) && "month" %in% names(reference)) {
        cols <- c(cols, "month")
    }
    cols
}

morpher__belcher_attach_reference <- function(target, reference, value_name = "reference_value") {
    if (!nrow(target) || !nrow(reference)) {
        target[, (value_name) := NA_real_]
        return(target)
    }
    reference <- data.table::copy(reference)
    data.table::setnames(reference, "value", value_name)
    join_cols <- morpher__belcher_reference_join_cols(target, reference)
    if (!length(join_cols)) {
        cli::cli_abort("Cannot align target and reference climate data without shared identity columns.")
    }
    reference[, value_reference_tmp := get(value_name)]
    reference[, (value_name) := NULL]
    reference <- reference[, .(
        value_reference_tmp = mean(value_reference_tmp, na.rm = TRUE)
    ), by = join_cols]
    target[reference, on = join_cols, (value_name) := i.value_reference_tmp]
    target[]
}

morpher__belcher_handle_missing_reference <- function(data, var, strict = TRUE) {
    missing <- data[is.na(reference_value)]
    if (!nrow(missing)) {
        return(data)
    }
    message <- sprintf("Reference climate data are missing for %s in one or more morphing months.", var)
    if (isTRUE(strict)) {
        cli::cli_abort(message)
    }
    warning(message, call. = FALSE)
    data[is.na(reference_value), reference_value := value]
    data
}

morpher__belcher_from_monthly_change <- function(var, data_epw, data_mean, reference_mean,
                                                  data_max = NULL, data_min = NULL,
                                                  reference_max = NULL, reference_min = NULL,
                                                  type = c("shift", "stretch", "combined"),
                                                  strict = TRUE) {
    type <- match.arg(type)
    if (!nrow(data_mean)) {
        return(data.table::data.table())
    }
    if (!nrow(reference_mean)) {
        if (isTRUE(strict)) {
            cli::cli_abort("Change-factor morphing requires reference climate data for {.val {var}}.")
        }
        return(data.table::data.table())
    }

    monthly <- morpher__belcher_epw_monthly(data_epw, var)
    u <- units::deparse_unit(data_epw[[var]])
    data_mean <- morpher__belcher_align_units(data.table::copy(data_mean), u)
    reference_mean <- morpher__belcher_align_units(data.table::copy(reference_mean), u)
    data_mean <- morpher__belcher_attach_reference(data_mean, reference_mean, "reference_value")
    data_mean <- morpher__belcher_handle_missing_reference(data_mean, var, strict = strict)

    case_fallback <- data.table::data.table()
    if (identical(type, "combined") && !is.null(data_max) && !is.null(data_min) &&
        !is.null(reference_max) && !is.null(reference_min)) {
        data_max <- morpher__belcher_align_units(data.table::copy(data_max), u)
        data_min <- morpher__belcher_align_units(data.table::copy(data_min), u)
        reference_max <- morpher__belcher_align_units(data.table::copy(reference_max), u)
        reference_min <- morpher__belcher_align_units(data.table::copy(reference_min), u)
        join_cols <- intersect(c(
            "activity_drs", "institution_id", "source_id", "experiment_id",
            "member_id", "table_id", "lat", "lon", "units", "month",
            "interval"
        ), names(data_mean))
        data_mean[data_max, on = join_cols, value_max := i.value]
        data_mean[data_min, on = join_cols, value_min := i.value]
        data_mean <- morpher__belcher_attach_reference(data_mean, reference_max, "reference_max")
        data_mean <- morpher__belcher_attach_reference(data_mean, reference_min, "reference_min")

        missing_extreme <- data_mean[
            is.na(value_max) | is.na(value_min) | is.na(reference_max) | is.na(reference_min)
        ]
        if (nrow(missing_extreme)) {
            cols <- c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id")
            case_fallback <- unique(missing_extreme[, ..cols], by = cols)
            cases <- case_fallback[, unique(sprintf(
                "CMIP6.%s.%s.%s.%s.%s.%s",
                activity_drs, institution_id, source_id, experiment_id, member_id, table_id
            ))]
            cases <- sprintf("[%i] '%s'", seq_along(cases), sort(cases))
            warning(sprintf(
                "Case(s) below contains missing target or reference max/min values of '%s'. ",
                gsub("_", " ", var)
            ),
            "'Shift' method will be used for it.\n", paste0(cases, collapse = "\n"),
            call. = FALSE
            )
        }
    }

    data_mean[monthly, on = "month", `:=`(
        epw_mean = i.val_mean,
        epw_max = i.val_max,
        epw_min = i.val_min
    )]
    data_mean[, delta := value - reference_value]

    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE
    ]

    if (identical(type, "combined") && all(c("value_min", "value_max", "reference_min", "reference_max") %in% names(data))) {
        data[, alpha := ((value_max - reference_max) - (value_min - reference_min)) / (epw_max - epw_min)]
        if (nrow(case_fallback)) {
            data[case_fallback, on = names(case_fallback), alpha := 0.0]
        }
        data[is.na(alpha) | !is.finite(morpher__drop_units(alpha)), alpha := 0.0]
    } else if (identical(type, "stretch")) {
        ref <- morpher__drop_units(data$reference_value)
        val <- morpher__drop_units(data$value)
        data[, alpha := ifelse(is.finite(ref) & abs(ref) > .Machine$double.eps, val / ref, NA_real_)]
        missing_alpha <- data[is.na(alpha) | !is.finite(alpha)]
        if (nrow(missing_alpha)) {
            message <- sprintf("Reference climate data include zero values for %s; cannot compute stretch factors.", var)
            if (isTRUE(strict)) {
                cli::cli_abort(message)
            }
            warning(message, call. = FALSE)
            data[is.na(alpha) | !is.finite(alpha), alpha := 1.0]
        }
    } else {
        data[, alpha := 0.0]
    }

    thres_alpha <- getOption("epwshiftr.threshold_alpha")
    if (!checkmate::test_number(thres_alpha, lower = 0)) {
        warning(paste0(
            "The threshold value for the monthly-mean fractional change (Alpha) ",
            "should be a positive number, but '",
            if (is.null(thres_alpha)) "NULL" else thres_alpha,
            "' is found."
        ))
    }
    if (type %in% c("stretch", "combined") && nrow(abnorm_alpha <- data[abs(morpher__drop_units(alpha)) > thres_alpha])) {
        warning(sprintf(
            paste(
                "The absolute values of monthly-mean fractional change (Alpha) below",
                "for '%s' has exceeded the threshold (%s) set by the option",
                "'epwshiftr.threshold_alpha'. 'Shift' morphing method will be utilized",
                "instead of '%s' method to avoid unrealistic values. It is highly",
                "suggested to further investigate the input data.\n%s",
                collapse = " "
            ),
            gsub("_", " ", var, fixed = TRUE), thres_alpha, type,
            paste0(sprintf(
                "Month = %s | Future = %s, Reference = %s --> Alpha = %s",
                format(abnorm_alpha$month),
                format(abnorm_alpha$value, digits = 3),
                format(abnorm_alpha$reference_value, digits = 3),
                format(morpher__drop_units(abnorm_alpha$alpha), digits = 3)
            ), collapse = "\n")
        ))
        type <- "shift"
    }

    if (identical(type, "shift")) {
        data[, c(var) := units::set_units(get(var) + delta, u, mode = "standard")]
    } else if (identical(type, "stretch")) {
        data[, c(var) := units::set_units(get(var) * alpha, u, mode = "standard")]
    } else if (identical(type, "combined")) {
        data[, c(var) := units::set_units(get(var) + delta + alpha * (get(var) - epw_mean), u, mode = "standard")]
    }

    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        var, "delta", "alpha"
    )]
}

morpher__belcher_tdb <- function(data_epw, context, type) {
    tas <- morpher__belcher_monthly_variable(context, "tas")
    if (!nrow(tas)) {
        return(data.table::data.table())
    }
    tasmax <- morpher__belcher_monthly_variable(context, "tasmax")
    tasmin <- morpher__belcher_monthly_variable(context, "tasmin")
    morpher__belcher_from_monthly(
        "dry_bulb_temperature", data_epw, tas,
        if (nrow(tasmax)) tasmax else NULL,
        if (nrow(tasmin)) tasmin else NULL,
        type = type
    )
}

morpher__belcher_rh <- function(data_epw, context, type) {
    hurs <- morpher__belcher_monthly_variable(context, "hurs")
    if (!nrow(hurs)) {
        return(data.table::data.table())
    }
    hursmax <- morpher__belcher_monthly_variable(context, "hursmax")
    hursmin <- morpher__belcher_monthly_variable(context, "hursmin")
    rh <- morpher__belcher_from_monthly(
        "relative_humidity", data_epw, hurs,
        if (nrow(hursmax)) hursmax else NULL,
        if (nrow(hursmin)) hursmin else NULL,
        type = type
    )
    rh[relative_humidity > units::set_units(100, "%"), relative_humidity := units::set_units(100, "%")]
    rh[relative_humidity < units::set_units(0, "%"), relative_humidity := units::set_units(0, "%")]
    rh
}

morpher__belcher_change_tdb <- function(data_epw, context, type) {
    tas <- morpher__belcher_monthly_variable(context, "tas")
    tas_ref <- morpher__belcher_monthly_reference_variable(context, "tas")
    if (!nrow(tas)) {
        return(data.table::data.table())
    }
    tasmax <- morpher__belcher_monthly_variable(context, "tasmax")
    tasmin <- morpher__belcher_monthly_variable(context, "tasmin")
    tasmax_ref <- morpher__belcher_monthly_reference_variable(context, "tasmax")
    tasmin_ref <- morpher__belcher_monthly_reference_variable(context, "tasmin")
    morpher__belcher_from_monthly_change(
        "dry_bulb_temperature", data_epw, tas, tas_ref,
        if (nrow(tasmax)) tasmax else NULL,
        if (nrow(tasmin)) tasmin else NULL,
        if (nrow(tasmax_ref)) tasmax_ref else NULL,
        if (nrow(tasmin_ref)) tasmin_ref else NULL,
        type = type,
        strict = context$strict
    )
}

morpher__belcher_change_rh <- function(data_epw, context, type) {
    hurs <- morpher__belcher_monthly_variable(context, "hurs")
    hurs_ref <- morpher__belcher_monthly_reference_variable(context, "hurs")
    if (!nrow(hurs)) {
        return(data.table::data.table())
    }
    hursmax <- morpher__belcher_monthly_variable(context, "hursmax")
    hursmin <- morpher__belcher_monthly_variable(context, "hursmin")
    hursmax_ref <- morpher__belcher_monthly_reference_variable(context, "hursmax")
    hursmin_ref <- morpher__belcher_monthly_reference_variable(context, "hursmin")
    rh <- morpher__belcher_from_monthly_change(
        "relative_humidity", data_epw, hurs, hurs_ref,
        if (nrow(hursmax)) hursmax else NULL,
        if (nrow(hursmin)) hursmin else NULL,
        if (nrow(hursmax_ref)) hursmax_ref else NULL,
        if (nrow(hursmin_ref)) hursmin_ref else NULL,
        type = type,
        strict = context$strict
    )
    rh[relative_humidity > units::set_units(100, "%"), relative_humidity := units::set_units(100, "%")]
    rh[relative_humidity < units::set_units(0, "%"), relative_humidity := units::set_units(0, "%")]
    rh
}

morpher__belcher_monthly_field <- function(data_epw, context, variable_id, epw_field, type) {
    data <- morpher__belcher_monthly_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    morpher__belcher_from_monthly(epw_field, data_epw, data, type = type)
}

morpher__belcher_change_monthly_field <- function(data_epw, context, variable_id, epw_field, type) {
    data <- morpher__belcher_monthly_variable(context, variable_id)
    reference <- morpher__belcher_monthly_reference_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    morpher__belcher_from_monthly_change(
        epw_field,
        data_epw,
        data,
        reference,
        type = type,
        strict = context$strict
    )
}

morpher__belcher_monthly_change_variable <- function(context, variable_id, target_units = NULL) {
    data <- morpher__belcher_monthly_variable(context, variable_id)
    reference <- morpher__belcher_monthly_reference_variable(context, variable_id)
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    if (!nrow(reference)) {
        if (isTRUE(context$strict)) {
            cli::cli_abort("Change-factor morphing requires reference climate data for {.val {variable_id}}.")
        }
        return(data.table::data.table())
    }
    if (!is.null(target_units)) {
        data <- morpher__belcher_align_units(data.table::copy(data), target_units)
        reference <- morpher__belcher_align_units(data.table::copy(reference), target_units)
    }
    data <- morpher__belcher_attach_reference(data, reference, "reference_value")
    data <- morpher__belcher_handle_missing_reference(data, variable_id, strict = context$strict)
    data[, value := value - reference_value]
    data[, reference_value := NULL]
    data[]
}

morpher__belcher_total_sky_cover <- function(data_epw, context, data_mean = NULL, change_factor = FALSE) {
    var <- "total_sky_cover"
    if (is.null(data_mean)) {
        data_mean <- morpher__belcher_monthly_variable(context, "clt")
    }
    if (!nrow(data_mean)) {
        return(data.table::data.table())
    }
    monthly <- unique(data_epw[, .SD, .SDcols = "month"])
    data_mean <- data_mean[monthly, on = "month"]
    data <- data_epw[, .SD, .SDcols = c("datetime", "year", "month", "day", "hour", "minute", var)][
        data_mean, on = "month", allow.cartesian = TRUE
    ]
    data.table::set(data, NULL, "value", morpher__drop_units(data$value))
    if (isTRUE(change_factor)) {
        data[, target_total_sky_cover := as.integer(round(pmax(0, pmin(10, total_sky_cover + value / 10))))]
    } else {
        data[, target_total_sky_cover := as.integer(round(pmax(0, pmin(10, value / 10))))]
    }
    data[, `:=`(
        delta = target_total_sky_cover - total_sky_cover,
        alpha = ifelse(total_sky_cover == 0, NA_real_, target_total_sky_cover / total_sky_cover),
        total_sky_cover = target_total_sky_cover
    )]
    data[, target_total_sky_cover := NULL]
    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        var, "delta", "alpha"
    )]
}

morpher__belcher_change_total_sky_cover <- function(data_epw, context) {
    data_mean <- morpher__belcher_monthly_change_variable(context, "clt", target_units = "%")
    morpher__belcher_total_sky_cover(data_epw, context, data_mean = data_mean, change_factor = TRUE)
}

# Normalize precipitation units before manually converting fluxes to monthly
# water-equivalent depth; udunits does not know the density convention.
morpher__precip_unit_kind <- function(units) {
    units <- morpher__unit_alias(units)
    if (is.na(units) || !nzchar(units)) {
        return(NA_character_)
    }
    key <- tolower(trimws(units))
    key <- gsub("\\s+", "", key)
    key <- gsub("\\^", "", key)
    switch(
        key,
        "kgm-2s-1" = "kg_m2_s",
        "kgm**-2s**-1" = "kg_m2_s",
        "kg/m2/s" = "kg_m2_s",
        "kgm-2sec-1" = "kg_m2_s",
        "mmday-1" = "mm_day",
        "mm/day" = "mm_day",
        "mmd-1" = "mm_day",
        "mm/d" = "mm_day",
        "mm" = "mm",
        NA_character_
    )
}

# Return Gregorian month lengths for the period years used by morphing.
morpher__precip_month_days <- function(year, month) {
    year <- as.integer(year)
    month <- as.integer(month)
    if (!length(year) || !length(month)) {
        return(integer())
    }
    mapply(function(y, m) {
        start <- as.Date(sprintf("%04d-%02d-01", y, m))
        next_year <- y + as.integer(m == 12L)
        next_month <- if (m == 12L) 1L else m + 1L
        as.integer(as.Date(sprintf("%04d-%02d-01", next_year, next_month)) - start)
    }, year, month)
}

# Convert a precipitation rate or monthly depth into water-equivalent millimetres.
morpher__precip_depth_checked <- function(value, units, seconds) {
    kind <- morpher__precip_unit_kind(units)
    value <- morpher__drop_units(value)
    if (!length(value) || is.na(value[[1L]]) || is.na(kind)) {
        return(list(value = as.numeric(value), ok = !is.na(kind), message = "Unsupported precipitation units."))
    }
    value <- as.numeric(value[[1L]])
    seconds <- as.numeric(seconds[[1L]])
    if (!is.finite(value) || !is.finite(seconds)) {
        return(list(value = value, ok = TRUE, message = NA_character_))
    }
    out <- switch(
        kind,
        kg_m2_s = value * seconds,
        mm_day = value * seconds / 86400,
        mm = value
    )
    list(value = out, ok = TRUE, message = NA_character_)
}

# Convert climate summary rows for `pr` from monthly mean rate to monthly depth.
morpher__precip_summary_depth_checked <- function(value, units, years_json, month) {
    years <- tryCatch(morpher__json_int_vector(years_json), error = function(e) integer())
    if (!length(years)) {
        years <- 2001L
    }
    days <- morpher__precip_month_days(years, month)
    morpher__precip_depth_checked(value, units, mean(days, na.rm = TRUE) * 86400)
}

# Convert a baseline EPW monthly mean precipitation depth into a monthly total.
morpher__baseline_precip_depth_checked <- function(value, units, month) {
    if (is.na(units) || !nzchar(units)) {
        units <- "mm"
    }
    converted <- morpher__convert_value_checked(value, units, "mm")
    converted$value <- converted$value * morpher__precip_month_days(2001L, month) * 24
    converted
}

# Summarise raw `pr` climate data to monthly water-equivalent depths.
morpher__belcher_monthly_precip_variable <- function(context, variable_id, reference = FALSE) {
    data <- if (isTRUE(reference)) {
        morpher__context_reference_variable(context, variable_id)
    } else {
        morpher__context_variable(context, variable_id)
    }
    if (!nrow(data)) {
        return(data.table::data.table())
    }
    data <- data.table::as.data.table(data.table::copy(data))
    data[, `:=`(
        year = as.integer(year),
        month = data.table::month(time),
        day = data.table::mday(time)
    )]
    data <- data[!(month == 2L & day == 29L)]

    identity <- morpher__context_identity_rows(data)
    data <- data.table::data.table(
        identity,
        units = as.character(data$units),
        value = as.numeric(data$value),
        year = as.integer(data$year),
        month = as.integer(data$month),
        interval = as.factor(data$period)
    )
    group_cols <- c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id", "units", "month", "interval")
    out <- data[, .(
        lon = mean(lon, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        dist = mean(dist, na.rm = TRUE),
        value = mean(value, na.rm = TRUE),
        years = list(sort(unique(year)))
    ), by = group_cols]

    values <- vapply(seq_len(nrow(out)), function(i) {
        days <- morpher__precip_month_days(out$years[[i]], out$month[[i]])
        converted <- morpher__precip_depth_checked(out$value[[i]], out$units[[i]], mean(days, na.rm = TRUE) * 86400)
        if (!isTRUE(converted$ok)) {
            cli::cli_abort("Unsupported precipitation units for {.val {variable_id}}: {.val {out$units[[i]]}}.")
        }
        converted$value
    }, numeric(1L))
    out[, `:=`(
        value = units::set_units(values, "mm", mode = "standard"),
        units = "mm",
        years = NULL
    )]
    data.table::setcolorder(out, c("activity_drs", "institution_id", "source_id", "experiment_id", "member_id", "table_id", "lon", "lat", "dist", "units", "value", "month", "interval"))
    out[]
}

# Report conservative precipitation fallbacks consistently across strict modes.
morpher__belcher_precip_guard <- function(rows, message, strict = TRUE) {
    if (!nrow(rows)) {
        return(invisible(NULL))
    }
    months <- paste(sort(unique(rows$month)), collapse = ", ")
    message <- sprintf("%s Month(s): %s.", message, months)
    if (isTRUE(strict)) {
        cli::cli_abort(message)
    }
    warning(message, call. = FALSE)
    invisible(NULL)
}

# Apply monthly precipitation targets while preserving baseline wet-hour timing.
morpher__belcher_precip_from_monthly <- function(data_epw, data_mean, strict = TRUE,
                                                 change_factor = FALSE) {
    if (!nrow(data_mean) || !"liquid_precip_depth" %in% names(data_epw)) {
        return(data.table::data.table())
    }
    rate_col <- "liquid_precip_rate"
    keep <- c("datetime", "year", "month", "day", "hour", "minute", "liquid_precip_depth")
    if (rate_col %in% names(data_epw)) {
        keep <- c(keep, rate_col)
    }
    baseline <- data_epw[, .SD, .SDcols = keep]
    if (!rate_col %in% names(baseline)) {
        baseline[, (rate_col) := units::set_units(0, "h", mode = "standard")]
    }
    baseline[, .baseline_precip_depth := pmax(0, morpher__drop_units(liquid_precip_depth))]
    monthly <- baseline[, .(baseline_total = sum(.baseline_precip_depth, na.rm = TRUE)), by = "month"]

    data_mean <- data.table::copy(data_mean)
    data_mean[monthly, on = "month", baseline_total := i.baseline_total]
    data_mean[is.na(baseline_total), baseline_total := 0]
    data_mean[, future_total := morpher__drop_units(value)]

    if (isTRUE(change_factor)) {
        data_mean[, reference_total := morpher__drop_units(reference_value)]
        zero_reference <- data_mean[reference_total <= .Machine$double.eps & future_total > .Machine$double.eps]
        morpher__belcher_precip_guard(
            zero_reference,
            "Reference climate precipitation is zero while future precipitation is positive; preserving baseline precipitation in relaxed mode.",
            strict = strict
        )
        # Relaxed mode cannot infer new storm frequency from zero reference rain,
        # so it keeps the baseline precipitation magnitude unchanged.
        data_mean[, alpha := data.table::fifelse(
            reference_total > .Machine$double.eps,
            future_total / reference_total,
            data.table::fifelse(future_total <= .Machine$double.eps, 0, 1)
        )]
        data_mean[, target_total := baseline_total * alpha]
        data_mean[, delta := future_total - reference_total]
    } else {
        data_mean[, target_total := future_total]
        data_mean[, alpha := data.table::fifelse(
            baseline_total > .Machine$double.eps,
            target_total / baseline_total,
            NA_real_
        )]
        data_mean[, delta := target_total - baseline_total]
    }

    dry_target <- data_mean[baseline_total <= .Machine$double.eps & future_total > .Machine$double.eps]
    morpher__belcher_precip_guard(
        dry_target,
        "Baseline EPW has no wet hours for positive target precipitation; keeping the month dry in relaxed mode.",
        strict = strict
    )
    # Without baseline wet hours, v1 deliberately refuses to synthesize event
    # timing and therefore leaves precipitation at zero for that month.
    data_mean[baseline_total <= .Machine$double.eps, `:=`(
        target_total = 0,
        alpha = NA_real_
    )]

    data <- baseline[data_mean, on = "month", allow.cartesian = TRUE]
    scale <- ifelse(is.na(data$alpha), 0, data$alpha)
    # CMIP6 `pr` only supplies precipitation amount after time integration; the
    # EPW liquid precipitation duration/rate field is derived from wet hours.
    depth <- data$.baseline_precip_depth * scale
    data[, liquid_precip_depth := units::set_units(depth, "mm", mode = "standard")]
    data[, liquid_precip_rate := units::set_units(as.numeric(depth > .Machine$double.eps), "h", mode = "standard")]
    data[, .baseline_precip_depth := NULL]
    data[, .SD, .SDcols = c(
        "activity_drs", "institution_id", "source_id", "experiment_id", "member_id",
        "table_id", "lon", "lat", "dist", "interval",
        "datetime", "year", "month", "day", "hour", "minute",
        "liquid_precip_depth", "liquid_precip_rate", "delta", "alpha"
    )]
}

# Build absolute-target Belcher precipitation from future climate monthly totals.
morpher__belcher_precip <- function(data_epw, context) {
    pr <- morpher__belcher_monthly_precip_variable(context, "pr")
    morpher__belcher_precip_from_monthly(data_epw, pr, strict = context$strict)
}

# Build change-factor Belcher precipitation from future/reference monthly totals.
morpher__belcher_change_precip <- function(data_epw, context) {
    pr <- morpher__belcher_monthly_precip_variable(context, "pr")
    pr_ref <- morpher__belcher_monthly_precip_variable(context, "pr", reference = TRUE)
    if (!nrow(pr)) {
        return(data.table::data.table())
    }
    if (!nrow(pr_ref)) {
        if (isTRUE(context$strict)) {
            cli::cli_abort("Change-factor morphing requires reference climate data for {.val pr}.")
        }
        warning("Reference climate data are missing for pr; precipitation is left unchanged.", call. = FALSE)
        return(data.table::data.table())
    }
    pr <- morpher__belcher_attach_reference(pr, pr_ref, "reference_value")
    pr <- morpher__belcher_handle_missing_reference(pr, "pr", strict = context$strict)
    morpher__belcher_precip_from_monthly(data_epw, pr, strict = context$strict, change_factor = TRUE)
}

morpher__belcher_absolute_run <- function(context, backend = NULL) {
    methods <- context$recipe$methods
    epw <- context$epw$clone()
    data_epw <- suppressMessages(epw$add_unit()$data())

    tdb <- morpher__belcher_tdb(data_epw, context, methods[["tdb"]])
    rh <- morpher__belcher_rh(data_epw, context, methods[["rh"]])
    tdew <- if (!nrow(tdb) || !nrow(rh)) data.table::data.table() else morpher__belcher_tdew(tdb, rh)

    p <- morpher__belcher_monthly_field(data_epw, context, "psl", "atmospheric_pressure", methods[["p"]])

    data_epw[, horizontal_infrared_radiation_intensity_from_sky :=
        units::set_units(units::drop_units(horizontal_infrared_radiation_intensity_from_sky), "W/m^2"
    )]
    hor_ir <- morpher__belcher_monthly_field(data_epw, context, "rlds", "horizontal_infrared_radiation_intensity_from_sky", methods[["hor_ir"]])

    data_epw[, global_horizontal_radiation :=
        units::set_units(units::drop_units(global_horizontal_radiation), "W/m^2"
    )]
    glob_rad <- morpher__belcher_monthly_field(data_epw, context, "rsds", "global_horizontal_radiation", methods[["glob_rad"]])
    diff_rad <- if (!nrow(glob_rad)) data.table::data.table() else morpher__belcher_diffuse_radiation(data_epw, glob_rad)
    epw_lat <- morpher__epw_location_numeric(epw, c("latitude", "lat", "N2_latitude"))
    epw_lon <- morpher__epw_location_numeric(epw, c("longitude", "lon", "N3_longitude"))
    epw_tz <- morpher__epw_location_numeric(epw, c("time_zone", "timezone", "N4_time_zone"), default = 0)
    norm_rad <- if (!nrow(glob_rad) || !nrow(diff_rad)) {
        data.table::data.table()
    } else {
        morpher__belcher_direct_normal_radiation(
            glob_rad,
            diff_rad,
            latitude = epw_lat,
            longitude = epw_lon,
            timezone = epw_tz
        )
    }

    wind <- morpher__belcher_monthly_field(data_epw, context, "sfcWind", "wind_speed", methods[["wind"]])

    total_cover <- morpher__belcher_total_sky_cover(data_epw, context)
    opaque_cover <- if (!nrow(total_cover)) data.table::data.table() else morpher__belcher_opaque_sky_cover(data_epw, total_cover)
    precip <- morpher__belcher_precip(data_epw, context)

    parts <- list(
        tdb = tdb,
        tdew = tdew,
        rh = rh,
        p = p,
        hor_ir = hor_ir,
        glob_rad = glob_rad,
        norm_rad = norm_rad,
        diff_rad = diff_rad,
        wind = wind,
        total_cover = total_cover,
        opaque_cover = opaque_cover,
        precip = precip
    )
    suppressMessages(epw$drop_unit())
    for (name in names(parts)) {
        parts[[name]] <- morpher__belcher_drop_units(parts[[name]], intersect(names(parts[[name]]), names(data_epw)))
    }
    morpher__engine_output(context, epw, parts)
}

morpher__belcher_run <- function(context, backend = NULL) {
    if (is.null(context$reference_climate)) {
        cli::cli_abort("Backend {.val belcher} requires reference climate data.")
    }

    methods <- context$recipe$methods
    epw <- context$epw$clone()
    data_epw <- suppressMessages(epw$add_unit()$data())

    tdb <- morpher__belcher_change_tdb(data_epw, context, methods[["tdb"]])
    rh <- morpher__belcher_change_rh(data_epw, context, methods[["rh"]])
    tdew <- if (!nrow(tdb) || !nrow(rh)) data.table::data.table() else morpher__belcher_tdew(tdb, rh)

    p <- morpher__belcher_change_monthly_field(data_epw, context, "psl", "atmospheric_pressure", methods[["p"]])

    data_epw[, horizontal_infrared_radiation_intensity_from_sky :=
        units::set_units(units::drop_units(horizontal_infrared_radiation_intensity_from_sky), "W/m^2"
    )]
    hor_ir <- morpher__belcher_change_monthly_field(data_epw, context, "rlds", "horizontal_infrared_radiation_intensity_from_sky", methods[["hor_ir"]])

    data_epw[, global_horizontal_radiation :=
        units::set_units(units::drop_units(global_horizontal_radiation), "W/m^2"
    )]
    glob_rad <- morpher__belcher_change_monthly_field(data_epw, context, "rsds", "global_horizontal_radiation", methods[["glob_rad"]])
    diff_rad <- if (!nrow(glob_rad)) data.table::data.table() else morpher__belcher_diffuse_radiation(data_epw, glob_rad)
    epw_lat <- morpher__epw_location_numeric(epw, c("latitude", "lat", "N2_latitude"))
    epw_lon <- morpher__epw_location_numeric(epw, c("longitude", "lon", "N3_longitude"))
    epw_tz <- morpher__epw_location_numeric(epw, c("time_zone", "timezone", "N4_time_zone"), default = 0)
    norm_rad <- if (!nrow(glob_rad) || !nrow(diff_rad)) {
        data.table::data.table()
    } else {
        morpher__belcher_direct_normal_radiation(
            glob_rad,
            diff_rad,
            latitude = epw_lat,
            longitude = epw_lon,
            timezone = epw_tz
        )
    }

    wind <- morpher__belcher_change_monthly_field(data_epw, context, "sfcWind", "wind_speed", methods[["wind"]])

    total_cover <- morpher__belcher_change_total_sky_cover(data_epw, context)
    opaque_cover <- if (!nrow(total_cover)) data.table::data.table() else morpher__belcher_opaque_sky_cover(data_epw, total_cover)
    precip <- morpher__belcher_change_precip(data_epw, context)

    parts <- list(
        tdb = tdb,
        tdew = tdew,
        rh = rh,
        p = p,
        hor_ir = hor_ir,
        glob_rad = glob_rad,
        norm_rad = norm_rad,
        diff_rad = diff_rad,
        wind = wind,
        total_cover = total_cover,
        opaque_cover = opaque_cover,
        precip = precip
    )
    suppressMessages(epw$drop_unit())
    for (name in names(parts)) {
        parts[[name]] <- morpher__belcher_drop_units(parts[[name]], intersect(names(parts[[name]]), names(data_epw)))
    }
    morpher__engine_output(context, epw, parts)
}
# }}}

# EpwMorpher {{{
#' Store-native EPW morpher
#'
#' @description
#' `EpwMorpher` consumes completed [EsgStore] extraction outputs and creates
#' future EPW files through a store-backed morphing workflow.
#'
#' @author Hongyuan Jia
#' @name EpwMorpher
#' @export
EpwMorpher <- R6::R6Class(
    "EpwMorpher",
    lock_class = TRUE,
    lock_objects = FALSE,
    public = list(
        #' @description
        #' Create an EPW morpher.
        #'
        #' @param store An [EsgStore] object.
        #' @param epw EPW path or an [eplusr::Epw] object.
        #' @param site_id Optional site identifier.
        #' @param recipe EPW morphing recipe.
        #' @param label Optional source label.
        initialize = function(store, epw, site_id = NULL, recipe = epw_morph_recipe("belcher"), label = NULL) {
            private$store <- store
            private$store_private <- morpher__private_store(store)
            private$recipe <- recipe
            checkmate::assert_string(site_id, null.ok = TRUE)
            checkmate::assert_string(label, null.ok = TRUE)
            private$site_id <- site_id
            private$label <- label

            private$register_epw(epw)
            self
        },

        #' @description
        #' Return recipe-required CMIP variable IDs.
        required_variables = function() {
            epw_morph_variables(private$recipe)
        },

        #' @description
        #' Preflight EPW morphing inputs without writing store state.
        #'
        #' @param plan_id Optional extraction plan IDs.
        #' @param periods Optional period table from [epw_morph_periods()].
        #' @param reference_plan_id Optional reference extraction plan IDs for
        #'   change-factor backends.
        #' @param reference_periods Optional reference period table from
        #'   [epw_morph_periods()].
        #' @param summary_id Optional climate summary ID.
        #' @param reference_summary_id Optional reference climate summary ID for
        #'   change-factor backends.
        #' @param baseline_id Optional baseline summary ID.
        #' @param by Climate grouping columns.
        #' @param strict Whether required-data issues are errors.
        preflight = function(plan_id = NULL, periods = NULL, reference_plan_id = NULL,
                             reference_periods = NULL, summary_id = NULL,
                             reference_summary_id = NULL, baseline_id = NULL,
                             by = c("source_id", "experiment_id", "variant_label", "period"), strict = TRUE) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_character(reference_plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            if (!is.null(periods)) {
                checkmate::assert_data_frame(periods)
                checkmate::assert_names(names(periods), must.include = c("period", "year"))
            }
            if (!is.null(reference_periods)) {
                checkmate::assert_data_frame(reference_periods)
                checkmate::assert_names(names(reference_periods), must.include = c("period", "year"))
            }
            checkmate::assert_string(summary_id, null.ok = TRUE)
            checkmate::assert_string(reference_summary_id, null.ok = TRUE)
            checkmate::assert_string(baseline_id, null.ok = TRUE)
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            if (is.null(plan_id) && is.null(summary_id)) {
                cli::cli_abort("Either `plan_id` or `summary_id` must be supplied.")
            }
            if (!is.null(plan_id) && is.null(periods)) {
                cli::cli_abort("`periods` must be supplied when `plan_id` is supplied.")
            }
            if (!is.null(reference_plan_id) && is.null(reference_periods)) {
                cli::cli_abort("`reference_periods` must be supplied when `reference_plan_id` is supplied.")
            }
            reference_required <- morpher__recipe_requires_reference(private$recipe)
            reference_missing <- isTRUE(reference_required) &&
                is.null(reference_plan_id) && is.null(reference_summary_id)

            morpher__bind_diagnostics(
                if (!is.null(plan_id)) private$preflight_extraction(plan_id, periods, strict = strict) else morpher__empty_diagnostics(),
                if (!is.null(reference_plan_id)) private$preflight_extraction(reference_plan_id, reference_periods, strict = strict) else morpher__empty_diagnostics(),
                if (!is.null(summary_id)) private$preflight_summary(summary_id, by, strict = strict) else morpher__empty_diagnostics(),
                if (!is.null(reference_summary_id)) private$preflight_summary(reference_summary_id, morpher__reference_case_by(by), strict = strict) else morpher__empty_diagnostics(),
                if (reference_missing) {
                    morpher__diagnostic(
                        stage = "reference",
                        severity = if (isTRUE(strict)) "error" else "warning",
                        code = "missing_reference_climate",
                        message = "The selected morphing backend requires reference climate data.",
                        action = "Supply `reference_plan_id` and `reference_periods`, or use a backend that does not require reference climate."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                private$preflight_baseline(baseline_id, strict = strict)
            )
        },

        #' @description
        #' Summarise extracted climate data by period and month.
        #'
        #' @param plan_id Extraction plan IDs.
        #' @param periods Period table from [epw_morph_periods()].
        #' @param strict Whether incomplete extraction coverage is an error.
        #' @param overwrite Whether to replace existing rows for this summary.
        summarise_climate = function(plan_id, periods, strict = TRUE, overwrite = FALSE) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_data_frame(periods)
            checkmate::assert_names(names(periods), must.include = c("period", "year"))
            checkmate::assert_flag(strict)
            checkmate::assert_flag(overwrite)

            diagnostics <- private$preflight_extraction(plan_id, periods, strict = strict)
            if (isTRUE(strict)) {
                morpher__abort_diagnostics(
                    diagnostics,
                    "Cannot summarise climate data because selected extraction plans have blocking issues."
                )
            }

            summary_id <- private$summary_id(plan_id, periods)
            current <- morpher__read_table(private$store, "epw_climate_summary")
            target_summary_id <- summary_id
            current_summary <- current[current[["summary_id"]] == target_summary_id]
            current_usable <- nrow(current_summary) &&
                all(c("years_json", "lon", "lat") %in% names(current_summary)) &&
                all(!is.na(current_summary$years_json) & nzchar(current_summary$years_json))
            if (!isTRUE(overwrite) && isTRUE(current_usable)) {
                return(current_summary)
            }

            result <- private$extraction_rows(plan_id)
            if (!nrow(result)) {
                cli::cli_abort("No extraction results were found for the selected plan IDs.")
            }

            pieces <- vector("list", nrow(result))
            for (i in seq_len(nrow(result))) {
                path <- store_abs_path(result$output_path[[i]], root = private$store$path)
                dt <- morpher__parquet_read(private$store, path)
                if (!"units" %in% names(dt)) {
                    dt[, units := NA_character_]
                }
                dt[, plan_id := result$plan_id[[i]]]
                pieces[[i]] <- dt
            }
            climate <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
            climate[, year := as.integer(format(time, "%Y", tz = "UTC"))]
            climate[, month := as.integer(format(time, "%m", tz = "UTC"))]
            periods <- data.table::as.data.table(periods)
            periods[, year := as.integer(year)]
            period_years <- periods[, .(
                years_json = morpher__json(as.integer(sort(unique(year))))
            ), by = "period"]
            climate <- climate[periods, on = "year", nomatch = 0L]
            if (!nrow(climate)) {
                cli::cli_abort("No extracted climate rows matched the supplied EPW morphing periods.")
            }

            rows <- morpher__stat_rows(climate)
            rows <- period_years[rows, on = "period"]
            rows[, `:=`(
                summary_id = summary_id,
                coverage = 1,
                created_at = morpher__now()
            )]
            rows[, summary_row_id := morpher__hash_rows(summary_id, plan_id, variable_id, period, month, stat)]
            data.table::setcolorder(rows, c(
                "summary_row_id", "summary_id", "plan_id", "site_id", "source_id",
                "experiment_id", "variant_label", "frequency", "table_id",
                "variable_id", "period", "month", "stat", "value", "units",
                "lon", "lat", "years_json", "coverage", "n_records", "created_at"
            ))
            morpher__delete_by_key(private$store, "epw_climate_summary", "summary_id", summary_id)
            morpher__replace_rows(private$store, "epw_climate_summary", rows, "summary_row_id")
            rows[]
        },

        #' @description
        #' Summarise baseline EPW weather by month.
        #'
        #' @param overwrite Whether to replace existing rows.
        summarise_baseline = function(overwrite = FALSE) {
            checkmate::assert_flag(overwrite)
            baseline_id <- private$baseline_id()
            current <- morpher__read_table(private$store, "epw_baseline_summary")
            target_baseline_id <- baseline_id
            current_baseline <- current[current[["baseline_id"]] == target_baseline_id]
            if (!isTRUE(overwrite) && nrow(current_baseline)) {
                return(current_baseline)
            }

            epw <- private$epw$clone()
            suppressMessages(epw$add_unit())
            data <- data.table::as.data.table(epw$data())
            rules <- morpher__recipe_rules(private$recipe)
            fields <- unique(rules[required == TRUE & !derived, epw_field])
            fields <- intersect(fields, names(data))
            units_map <- morpher__field_units(data, fields)
            rows <- morpher__monthly_long(data, character(), fields, units_map)
            if (!nrow(rows)) {
                cli::cli_abort("No recipe EPW fields were found in the baseline EPW.")
            }
            rows[, `:=`(
                baseline_id = baseline_id,
                epw_id = private$epw_id,
                created_at = morpher__now()
            )]
            rows[, baseline_row_id := morpher__hash_rows(baseline_id, epw_field, month, stat)]
            data.table::setcolorder(rows, c(
                "baseline_row_id", "baseline_id", "epw_id", "epw_field",
                "month", "stat", "value", "units", "created_at"
            ))
            morpher__delete_by_key(private$store, "epw_baseline_summary", "baseline_id", baseline_id)
            morpher__replace_rows(private$store, "epw_baseline_summary", rows, "baseline_row_id")
            rows[]
        },

        #' @description
        #' Create a morphing plan and monthly factors.
        #'
        #' @param summary_id Climate summary ID.
        #' @param reference_summary_id Optional reference climate summary ID for
        #'   change-factor backends.
        #' @param baseline_id Baseline summary ID. If `NULL`, baseline summary is created.
        #' @param by Climate grouping columns.
        #' @param strict Whether missing required variables are blocking errors.
        #' @param overwrite Whether to replace an existing plan.
        plan = function(summary_id, reference_summary_id = NULL, baseline_id = NULL,
                        by = c("source_id", "experiment_id", "variant_label", "period"),
                        strict = TRUE, overwrite = FALSE) {
            checkmate::assert_string(summary_id, min.chars = 1L)
            checkmate::assert_string(reference_summary_id, null.ok = TRUE)
            checkmate::assert_string(baseline_id, null.ok = TRUE)
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            checkmate::assert_flag(overwrite)

            preview <- self$preview_plan(
                summary_id = summary_id,
                reference_summary_id = reference_summary_id,
                baseline_id = baseline_id,
                by = by,
                strict = strict
            )
            morph_id <- preview$plan$morph_id[[1L]]
            current <- morpher__read_table(private$store, "epw_morph_plan")
            target_morph_id <- morph_id
            current_plan <- current[current[["morph_id"]] == target_morph_id]
            if (!isTRUE(overwrite) && nrow(current_plan)) {
                return(current_plan)
            }

            morpher__delete_by_key(private$store, "epw_morph_factor", "morph_id", morph_id)
            morpher__replace_rows(private$store, "epw_morph_plan", preview$plan, "morph_id")
            morpher__replace_rows(private$store, "epw_morph_factor", preview$factors, "factor_id")
            data.table::as.data.table(preview$plan)
        },

        #' @description
        #' Preview a morphing plan and monthly factors without writing store state.
        #'
        #' @param summary_id Climate summary ID.
        #' @param reference_summary_id Optional reference climate summary ID for
        #'   change-factor backends.
        #' @param baseline_id Baseline summary ID. If `NULL`, baseline summary is created.
        #' @param by Climate grouping columns.
        #' @param strict Whether missing required variables are blocking errors.
        preview_plan = function(summary_id, reference_summary_id = NULL, baseline_id = NULL,
                                by = c("source_id", "experiment_id", "variant_label", "period"),
                                strict = TRUE) {
            checkmate::assert_string(summary_id, min.chars = 1L)
            checkmate::assert_string(reference_summary_id, null.ok = TRUE)
            checkmate::assert_string(baseline_id, null.ok = TRUE)
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            if (is.null(baseline_id)) {
                baseline_id <- unique(self$summarise_baseline()$baseline_id)[[1L]]
            }

            climate <- morpher__read_table(private$store, "epw_climate_summary")
            target_summary_id <- summary_id
            climate <- climate[climate[["summary_id"]] == target_summary_id & climate[["stat"]] == "mean"]
            if (!nrow(climate)) {
                cli::cli_abort("No climate summary rows were found for summary ID {.val {summary_id}}.")
            }
            reference <- NULL
            if (!is.null(reference_summary_id)) {
                reference <- morpher__read_table(private$store, "epw_climate_summary")
                target_reference_summary_id <- reference_summary_id
                reference <- reference[reference[["summary_id"]] == target_reference_summary_id & reference[["stat"]] == "mean"]
                if (!nrow(reference)) {
                    cli::cli_abort("No reference climate summary rows were found for summary ID {.val {reference_summary_id}}.")
                }
            }
            baseline <- morpher__read_table(private$store, "epw_baseline_summary")
            target_baseline_id <- baseline_id
            baseline <- baseline[baseline[["baseline_id"]] == target_baseline_id & baseline[["stat"]] == "mean"]
            if (!nrow(baseline)) {
                cli::cli_abort("No baseline summary rows were found for baseline ID {.val {baseline_id}}.")
            }

            morph_id <- private$morph_id(summary_id, reference_summary_id, baseline_id, by, strict)
            factors <- private$factor_rows(morph_id, climate, baseline, by, strict = strict, reference = reference)
            reference_required <- morpher__recipe_requires_reference(private$recipe)
            reference_missing <- isTRUE(reference_required) && is.null(reference_summary_id)
            diagnostics <- morpher__bind_diagnostics(
                private$preflight_summary(summary_id, by, strict = strict),
                if (!is.null(reference_summary_id)) {
                    private$preflight_summary(reference_summary_id, morpher__reference_case_by(by), strict = strict)
                } else {
                    morpher__empty_diagnostics()
                },
                if (reference_missing) {
                    morpher__diagnostic(
                        stage = "reference",
                        severity = if (isTRUE(strict)) "error" else "warning",
                        code = "missing_reference_climate",
                        message = "The selected morphing backend requires reference climate data.",
                        morph_id = morph_id,
                        action = "Supply `reference_summary_id`, or use a backend that does not require reference climate."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                private$preflight_baseline(baseline_id, strict = strict),
                private$factor_diagnostics(factors, strict = strict, morph_id = morph_id)
            )
            status <- if (any(diagnostics$severity == "error") && isTRUE(strict)) "blocked" else "planned"
            now <- morpher__now()
            plan <- data.table::data.table(
                morph_id = morph_id,
                epw_id = private$epw_id,
                summary_id = summary_id,
                reference_summary_id = store__chr1(reference_summary_id),
                baseline_id = baseline_id,
                label = store__chr1(private$label),
                by_json = morpher__json(by),
                recipe_json = morpher__json(private$recipe),
                strict = strict,
                status = status,
                created_at = now,
                updated_at = now,
                last_error = NA_character_
            )
            list(plan = plan, factors = factors, diagnostics = diagnostics)
        },

        #' @description
        #' Diagnose a morphing plan.
        #'
        #' @param morph_id Morphing plan ID.
        diagnose = function(morph_id) {
            checkmate::assert_string(morph_id, min.chars = 1L)
            plan <- private$get_plan(morph_id)
            factors <- morpher__read_table(private$store, "epw_morph_factor")
            target_morph_id <- morph_id
            factors <- factors[factors[["morph_id"]] == target_morph_id]
            if (!nrow(factors)) {
                return(morpher__diagnostic(
                    stage = "plan",
                    severity = "error",
                    code = "no_factors",
                    message = "No morphing factors were found.",
                    morph_id = morph_id,
                    action = "Run EpwMorpher$plan() again."
                ))
            }
            private$factor_diagnostics(factors, strict = isTRUE(plan$strict[[1L]]), morph_id = morph_id)
        },

        #' @description
        #' Abort if a morphing plan has blocking diagnostics.
        #'
        #' @param morph_id Morphing plan ID.
        check = function(morph_id) {
            diag <- self$diagnose(morph_id)
            bad <- diag[diag$severity == "error"]
            if (nrow(bad)) {
                cli::cli_abort(c(
                    "EPW morphing plan has blocking issues.",
                    "x" = "{bad$message[[1L]]}"
                ))
            }
            invisible(self)
        },

        #' @description
        #' Execute a morphing plan and write hourly result Parquet files.
        #'
        #' @param morph_id Morphing plan ID.
        #' @param overwrite Whether to overwrite existing result files.
        #' @param resume Whether to reuse complete existing results.
        run = function(morph_id, overwrite = FALSE, resume = TRUE) {
            checkmate::assert_string(morph_id, min.chars = 1L)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            plan <- private$get_plan(morph_id)
            if (isTRUE(plan$strict[[1L]])) {
                self$check(morph_id)
            }

            plan_cases <- private$case_rows(plan)
            cases <- plan_cases$case_id
            if (!length(cases)) {
                cli::cli_abort("No morphing cases were found for morph ID {.val {morph_id}}.")
            }

            existing <- morpher__read_table(private$store, "epw_morph_result")
            target_morph_id <- morph_id
            existing <- existing[existing[["morph_id"]] == target_morph_id]
            existing_paths <- if (nrow(existing)) {
                vapply(existing[["output_path"]], store_abs_path, character(1L), root = private$store$path)
            } else {
                character()
            }
            complete_existing <- existing[
                existing[["case_id"]] %in% cases &
                    vapply(existing_paths, file.exists, logical(1L))
            ]
            if (!isTRUE(overwrite) && isTRUE(resume) && length(unique(complete_existing$case_id)) == length(cases)) {
                private$set_plan_status(morph_id, "result_done")
                return(complete_existing[match(cases, complete_existing$case_id)])
            }

            private$set_plan_status(morph_id, "running")
            tryCatch(
                {
                    climate <- private$engine_climate_data(plan$summary_id[[1L]])
                    reference_summary_id <- if ("reference_summary_id" %in% names(plan)) {
                        store__chr1(plan$reference_summary_id[[1L]])
                    } else {
                        NA_character_
                    }
                    if (is.na(reference_summary_id) || !nzchar(reference_summary_id)) {
                        reference_summary_id <- NULL
                    }
                    if (isTRUE(morpher__recipe_requires_reference(private$recipe)) && is.null(reference_summary_id)) {
                        cli::cli_abort("Backend {.val {private$recipe$backend}} requires reference climate data.")
                    }
                    reference_climate <- if (is.null(reference_summary_id)) {
                        NULL
                    } else {
                        private$engine_climate_data(reference_summary_id)
                    }
                    by <- private$plan_by(plan)
                    reference_by <- morpher__reference_case_by(by)
                    result_rows <- list()
                    for (case_index in seq_along(cases)) {
                        case_id <- cases[[case_index]]
                        path <- private$morph_result_path(morph_id, case_id)
                        target_case_id <- case_id
                        existing_case <- complete_existing[complete_existing[["case_id"]] == target_case_id]
                        if (!isTRUE(overwrite) && isTRUE(resume) && nrow(existing_case)) {
                            result_rows[[length(result_rows) + 1L]] <- existing_case[1L]
                            next
                        }
                        if (file.exists(path) && !isTRUE(overwrite)) {
                            cli::cli_abort("Morph result already exists without a complete manifest row: {.path {path}}.")
                        }
                        case <- plan_cases[case_index]
                        case_climate <- private$filter_case_climate(climate, case, by)
                        if (!nrow(case_climate)) {
                            cli::cli_abort("No extracted climate rows matched morphing case {.val {target_case_id}}.")
                        }
                        reference_case_climate <- NULL
                        if (!is.null(reference_climate)) {
                            reference_case_climate <- private$filter_case_climate(reference_climate, case, reference_by)
                            if (isTRUE(morpher__recipe_requires_reference(private$recipe)) && !nrow(reference_case_climate)) {
                                cli::cli_abort("No reference climate rows matched morphing case {.val {target_case_id}}.")
                            }
                        }
                        context <- morpher__context(
                            epw = private$epw,
                            climate = case_climate,
                            reference_climate = reference_case_climate,
                            recipe = private$recipe,
                            by = by,
                            case = case,
                            strict = isTRUE(plan$strict[[1L]]),
                            warning = FALSE
                        )
                        case_result <- morpher__run_context(context)
                        case_data <- case_result$data
                        if (!nrow(case_data)) {
                            cli::cli_abort("No morphed data were produced for morphing case {.val {target_case_id}}.")
                        }
                        case_meta <- private$case_metadata_from_case(case, case_data)
                        for (name in names(case_meta)) {
                            case_data[, (name) := case_meta[[name]]]
                        }
                        write_parquet_file(case_data, path)
                        artifact_id <- private$store$register_artifact(
                            kind = "output",
                            path = path,
                            role = "derived",
                            project = "CMIP6",
                            metadata = list(morph_id = morph_id, case_id = case_id)
                        )
                        result_rows[[length(result_rows) + 1L]] <- data.frame(
                            result_id = morpher__hash(morph_id, case_id, path),
                            morph_id = morph_id,
                            case_id = case_id,
                            artifact_id = artifact_id,
                            output_path = store_rel_path(path, root = private$store$path),
                            row_count = nrow(case_data),
                            created_at = morpher__now(),
                            stringsAsFactors = FALSE
                        )
                    }
                    results <- data.table::rbindlist(result_rows, use.names = TRUE, fill = TRUE)
                    morpher__delete_by_key(private$store, "epw_morph_result", "morph_id", morph_id)
                    morpher__replace_rows(private$store, "epw_morph_result", results, "result_id")
                    private$set_plan_status(morph_id, "result_done")
                    results[]
                },
                error = function(e) {
                    private$set_plan_status(morph_id, "failed", conditionMessage(e))
                    stop(e)
                }
            )
        },

        #' @description
        #' Write future EPW files from morphing results.
        #'
        #' @param morph_id Morphing plan ID.
        #' @param dir Output directory. Relative paths are resolved under the store
        #'        root. If `NULL`, the workflow stops after writing morph result
        #'        Parquet files and does not write EPW outputs.
        #' @param separate Whether to create case subdirectories.
        #' @param overwrite Whether to overwrite existing EPW files.
        #' @param resume Whether to reuse complete existing EPW outputs.
        write_epw = function(morph_id, dir, separate = TRUE, overwrite = FALSE, resume = TRUE) {
            checkmate::assert_string(morph_id, min.chars = 1L)
            checkmate::assert_string(dir, min.chars = 1L)
            checkmate::assert_flag(separate)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            private$get_plan(morph_id)
            results <- morpher__read_table(private$store, "epw_morph_result")
            target_morph_id <- morph_id
            results <- results[results[["morph_id"]] == target_morph_id]
            if (!nrow(results)) {
                cli::cli_abort("No morphing results were found. Run {.code EpwMorpher$run()} first.")
            }

            tryCatch(
                {
                    root <- store_abs_path(dir, root = private$store$path)
                    dir.create(root, recursive = TRUE, showWarnings = FALSE)
                    base_epw <- private$epw$clone()
                    suppressMessages(base_epw$drop_unit())
                    base_cols <- names(base_epw$data())
                    current_outputs <- morpher__read_table(private$store, "epw_output")
                    target_morph_id <- morph_id
                    current_outputs <- current_outputs[current_outputs[["morph_id"]] == target_morph_id]
                    output_rows <- vector("list", nrow(results))
                    for (i in seq_len(nrow(results))) {
                        result <- results[i]
                        result_path <- store_abs_path(result$output_path[[1L]], root = private$store$path)
                        dt <- morpher__parquet_read(private$store, result_path)
                        meta <- private$case_metadata_from_result(dt)
                        label <- paste(morpher__safe_path(unlist(meta, use.names = FALSE)), collapse = ".")
                        filename <- paste(tools::file_path_sans_ext(basename(morpher__get_epw_path(private$epw))), label, "epw", sep = ".")
                        output_path <- if (isTRUE(separate)) {
                            file.path(root, do.call(file.path, as.list(morpher__safe_path(unlist(meta, use.names = FALSE)))), filename)
                        } else {
                            file.path(root, filename)
                        }
                        output_rel <- store_rel_path(output_path, root = private$store$path)
                        existing_output <- current_outputs[
                            current_outputs[["case_id"]] == result$case_id[[1L]] &
                                current_outputs[["path"]] == output_rel
                        ]
                        if (!isTRUE(overwrite) && isTRUE(resume) && nrow(existing_output) && file.exists(output_path)) {
                            output_rows[[i]] <- existing_output[1L]
                            next
                        }
                        if (file.exists(output_path) && !isTRUE(overwrite)) {
                            cli::cli_abort("EPW output already exists without a complete manifest row: {.path {output_path}}.")
                        }
                        new_epw <- private$epw$clone()
                        set_data <- data.table::copy(dt[, intersect(base_cols, names(dt)), with = FALSE])
                        data.table::setcolorder(set_data, base_cols)
                        suppressMessages(new_epw$drop_unit())
                        suppressMessages(new_epw$set(set_data))
                        case_label <- paste(unlist(meta, use.names = FALSE), collapse = "-")
                        new_epw$comment1(disclaimer_comment(case_label))
                        new_epw$fill_abnormal(missing = TRUE, out_of_range = TRUE, special = TRUE)
                        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
                        new_epw$save(output_path, overwrite = overwrite)
                        artifact_id <- private$store$register_artifact(
                            kind = "output",
                            path = output_path,
                            role = "output",
                            project = "CMIP6",
                            metadata = c(list(morph_id = morph_id, case_id = result$case_id[[1L]]), meta)
                        )
                        output_rows[[i]] <- data.frame(
                            output_id = morpher__hash(morph_id, result$case_id[[1L]], output_path),
                            morph_id = morph_id,
                            case_id = result$case_id[[1L]],
                            artifact_id = artifact_id,
                            path = output_rel,
                            source_id = store__chr1(meta$source_id),
                            experiment_id = store__chr1(meta$experiment_id),
                            variant_label = store__chr1(meta$variant_label),
                            period = store__chr1(meta$period),
                            created_at = morpher__now(),
                            stringsAsFactors = FALSE
                        )
                    }
                    outputs <- data.table::rbindlist(output_rows, use.names = TRUE, fill = TRUE)
                    morpher__delete_by_key(private$store, "epw_output", "morph_id", morph_id)
                    morpher__replace_rows(private$store, "epw_output", outputs, "output_id")
                    private$set_plan_status(morph_id, "epw_written")
                    outputs[]
                },
                error = function(e) {
                    private$set_plan_status(morph_id, "failed", conditionMessage(e))
                    stop(e)
                }
            )
        },

        #' @description
        #' Run the store-native EPW morphing workflow.
        #'
        #' @param plan_id Extraction plan IDs.
        #' @param periods Period table from [epw_morph_periods()].
        #' @param reference_plan_id Optional reference extraction plan IDs for
        #'   change-factor backends.
        #' @param reference_periods Optional reference period table from
        #'   [epw_morph_periods()].
        #' @param by Climate grouping columns.
        #' @param strict Whether blocking diagnostics should abort the workflow.
        #' @param dir Output directory. Relative paths are resolved under the store root.
        #' @param separate Whether to create case subdirectories.
        #' @param overwrite Whether to overwrite existing plan, result, and EPW outputs.
        #' @param resume Whether to reuse complete existing result and EPW outputs.
        workflow = function(plan_id, periods, reference_plan_id = NULL, reference_periods = NULL,
                            by = c("source_id", "experiment_id", "variant_label", "period"),
                            strict = TRUE, dir = "outputs/future-epw", separate = TRUE,
                            overwrite = FALSE, resume = TRUE) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_character(reference_plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_data_frame(periods)
            checkmate::assert_names(names(periods), must.include = c("period", "year"))
            if (!is.null(reference_periods)) {
                checkmate::assert_data_frame(reference_periods)
                checkmate::assert_names(names(reference_periods), must.include = c("period", "year"))
            }
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            checkmate::assert_string(dir, min.chars = 1L, null.ok = TRUE)
            checkmate::assert_flag(separate)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)

            preflight <- self$preflight(
                plan_id = plan_id,
                periods = periods,
                reference_plan_id = reference_plan_id,
                reference_periods = reference_periods,
                by = by,
                strict = strict
            )
            if (isTRUE(strict)) {
                morpher__abort_diagnostics(preflight)
            }
            climate <- self$summarise_climate(plan_id = plan_id, periods = periods, strict = strict, overwrite = overwrite)
            reference_climate <- if (is.null(reference_plan_id)) {
                NULL
            } else {
                self$summarise_climate(
                    plan_id = reference_plan_id,
                    periods = reference_periods,
                    strict = strict,
                    overwrite = overwrite
                )
            }
            reference_summary_id <- if (is.null(reference_climate)) {
                NULL
            } else {
                unique(reference_climate$summary_id)[[1L]]
            }
            baseline <- self$summarise_baseline(overwrite = overwrite)
            preview <- self$preview_plan(
                summary_id = unique(climate$summary_id)[[1L]],
                reference_summary_id = reference_summary_id,
                baseline_id = unique(baseline$baseline_id)[[1L]],
                by = by,
                strict = strict
            )
            plan <- self$plan(
                summary_id = unique(climate$summary_id)[[1L]],
                reference_summary_id = reference_summary_id,
                baseline_id = unique(baseline$baseline_id)[[1L]],
                by = by,
                strict = strict,
                overwrite = overwrite
            )
            diagnostics <- morpher__bind_diagnostics(preflight, preview$diagnostics)
            if (isTRUE(strict)) {
                self$check(plan$morph_id[[1L]])
            }
            results <- self$run(plan$morph_id[[1L]], overwrite = overwrite, resume = resume)
            outputs <- if (is.null(dir)) {
                NULL
            } else {
                self$write_epw(plan$morph_id[[1L]], dir = dir, separate = separate, overwrite = overwrite, resume = resume)
            }
            list(
                preflight = preflight,
                climate = climate,
                baseline = baseline,
                preview = preview,
                plan = plan,
                diagnostics = diagnostics,
                results = results,
                outputs = outputs
            )
        },

        #' @description
        #' Return morphing plan status rows.
        #'
        #' @param morph_id Optional morphing plan IDs.
        status = function(morph_id = NULL) {
            checkmate::assert_character(morph_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            plans <- morpher__read_table(private$store, "epw_morph_plan")
            if (!is.null(morph_id)) {
                target_morph_id <- morph_id
                plans <- plans[plans[["morph_id"]] %in% target_morph_id]
            }
            plans[]
        },

        #' @description
        #' Return future EPW output rows.
        #'
        #' @param morph_id Optional morphing plan IDs.
        outputs = function(morph_id = NULL) {
            checkmate::assert_character(morph_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            outputs <- morpher__read_table(private$store, "epw_output")
            if (!is.null(morph_id)) {
                target_morph_id <- morph_id
                outputs <- outputs[outputs[["morph_id"]] %in% target_morph_id]
            }
            outputs[]
        }
    ),

    private = list(
        store = NULL,
        store_private = NULL,
        epw = NULL,
        epw_id = NULL,
        site_id = NULL,
        label = NULL,
        recipe = NULL,

        register_epw = function(epw) {
            if (inherits(epw, "Epw")) {
                epw_path <- morpher__get_epw_path(epw)
                epw_obj <- epw$clone()
            } else {
                checkmate::assert_string(epw, min.chars = 1L)
                checkmate::assert_file_exists(epw)
                epw_path <- epw
                epw_obj <- eplusr::read_epw(epw_path)
            }
            checksum <- store_hash_file(epw_path, "sha256")
            epw_id <- morpher__hash("epw", checksum, private$site_id, private$label)
            target_dir <- file.path(private$store$path, "sources", "epw")
            dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
            target <- file.path(target_dir, sprintf("%s-%s", substr(epw_id, 1L, 12L), basename(epw_path)))
            if (!file.exists(target)) {
                ok <- file.copy(epw_path, target, overwrite = TRUE)
                if (!isTRUE(ok)) {
                    cli::cli_abort("Failed to copy baseline EPW into the store.")
                }
            }
            artifact_id <- private$store$register_artifact(
                kind = "source",
                path = target,
                role = "source",
                project = "CMIP6",
                checksum = checksum,
                checksum_type = "sha256",
                metadata = list(site_id = private$site_id, label = private$label)
            )
            now <- morpher__now()
            row <- data.frame(
                epw_id = epw_id,
                artifact_id = artifact_id,
                label = store__chr1(private$label),
                site_id = store__chr1(private$site_id),
                path = store_rel_path(target, root = private$store$path),
                checksum = checksum,
                created_at = now,
                updated_at = now,
                stringsAsFactors = FALSE
            )
            morpher__replace_rows(private$store, "epw_source", row, "epw_id")
            private$epw_id <- epw_id
            private$epw <- eplusr::read_epw(target)
            invisible(NULL)
        },

        baseline_id = function() {
            morpher__hash("baseline", private$epw_id, morpher__json(private$recipe))
        },

        summary_id = function(plan_id, periods) {
            morpher__hash("summary", private$epw_id, paste(sort(plan_id), collapse = "\r"), morpher__json(periods))
        },

        morph_id = function(summary_id, reference_summary_id, baseline_id, by, strict) {
            strict_token <- if (isTRUE(strict)) "strict=true" else "strict=false"
            morpher__hash(
                "morph", private$epw_id, summary_id, store__chr1(reference_summary_id),
                baseline_id, paste(by, collapse = "\r"), morpher__json(private$recipe),
                strict_token
            )
        },

        plan_by = function(plan) {
            by <- jsonlite::fromJSON(plan$by_json[[1L]])
            as.character(by)
        },

        extraction_rows = function(plan_id) {
            conn <- private$store_private$conn
            sql <- sprintf(
                paste(
                    "SELECT plan_id, output_path",
                    "FROM extraction_result",
                    "WHERE %s",
                    "ORDER BY plan_id, year"
                ),
                downloader__sql_in(conn, "plan_id", plan_id)
            )
            data.table::as.data.table(ddb_query(conn, sql))
        },

        summary_rows = function(summary_id, stat = NULL) {
            climate <- morpher__read_table(private$store, "epw_climate_summary")
            target_summary_id <- summary_id
            climate <- climate[climate[["summary_id"]] == target_summary_id]
            if (!is.null(stat)) {
                climate <- climate[climate[["stat"]] %in% stat]
            }
            climate[]
        },

        summary_period_years = function(summary_id) {
            climate <- private$summary_rows(summary_id)
            if (!nrow(climate)) {
                cli::cli_abort("No climate summary rows were found for summary ID {.val {summary_id}}.")
            }
            if (!"years_json" %in% names(climate) || any(is.na(climate$years_json) | !nzchar(climate$years_json))) {
                cli::cli_abort("Climate summary lacks period-year metadata. Re-run {.code EpwMorpher$summarise_climate(..., overwrite = TRUE)}.")
            }
            period_rows <- unique(climate[, .(period, years_json)])
            rows <- lapply(seq_len(nrow(period_rows)), function(i) {
                data.table::data.table(
                    period = period_rows$period[[i]],
                    year = morpher__json_int_vector(period_rows$years_json[[i]])
                )
            })
            data.table::rbindlist(rows, use.names = TRUE)
        },

        engine_climate_data = function(summary_id) {
            climate_summary <- private$summary_rows(summary_id)
            if (!nrow(climate_summary)) {
                cli::cli_abort("No climate summary rows were found for summary ID {.val {summary_id}}.")
            }
            plan_id <- unique(climate_summary$plan_id)
            result <- private$extraction_rows(plan_id)
            if (!nrow(result)) {
                cli::cli_abort("No extraction result files were found for climate summary ID {.val {summary_id}}.")
            }

            pieces <- vector("list", nrow(result))
            for (i in seq_len(nrow(result))) {
                path <- store_abs_path(result$output_path[[i]], root = private$store$path)
                dt <- morpher__parquet_read(private$store, path)
                dt[, plan_id := result$plan_id[[i]]]
                pieces[[i]] <- dt
            }
            climate <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
            if (!nrow(climate)) {
                cli::cli_abort("No extracted climate rows were found for climate summary ID {.val {summary_id}}.")
            }
            climate[, year := as.integer(format(time, "%Y", tz = "UTC"))]
            period_years <- private$summary_period_years(summary_id)
            climate <- climate[period_years, on = "year", nomatch = 0L, allow.cartesian = TRUE]
            if (!nrow(climate)) {
                cli::cli_abort("No extracted climate rows matched the stored EPW morphing periods.")
            }

            catalog <- morpher__read_table(private$store, "file_catalog")
            catalog_cols <- intersect(c("file_key", "activity_id", "institution_id", "variable_long_name"), names(catalog))
            if ("file_key" %in% catalog_cols) {
                catalog <- unique(catalog[, catalog_cols, with = FALSE])
                climate <- merge(climate, catalog, by = "file_key", all.x = TRUE, sort = FALSE)
            }

            if (!"units" %in% names(climate)) {
                climate[, units := NA_character_]
            }
            climate[]
        },

        case_rows = function(plan) {
            by <- private$plan_by(plan)
            climate <- private$summary_rows(plan$summary_id[[1L]], stat = "mean")
            missing_by <- setdiff(by, names(climate))
            if (length(missing_by)) {
                cli::cli_abort("Climate summary is missing grouping column(s): {.val {missing_by}}.")
            }
            cases <- unique(climate[, by, with = FALSE])
            if (!nrow(cases)) {
                cli::cli_abort("No morphing cases were found for morph ID {.val {plan$morph_id[[1L]]}}.")
            }
            case_ids <- vapply(seq_len(nrow(cases)), function(i) {
                morpher__hash(plan$morph_id[[1L]], morpher__json(as.list(cases[i])))
            }, character(1L))
            cases[, case_id := case_ids]
            cases[]
        },

        filter_case_climate = function(climate, case, by) {
            case_values <- as.list(case[1L])
            keep <- rep(TRUE, nrow(climate))
            for (name in intersect(by, intersect(names(case), names(climate)))) {
                value <- store__chr1(case_values[[name]])
                keep <- keep & morpher__identical_match(climate[[name]], value)
            }
            climate[keep][]
        },

        preflight_extraction = function(plan_id, periods, strict = TRUE) {
            severity <- if (isTRUE(strict)) "error" else "warning"
            diagnostics <- list()
            coverage <- private$store$coverage(plan_id = plan_id)
            if (!nrow(coverage)) {
                return(morpher__diagnostic(
                    stage = "extraction",
                    severity = "error",
                    code = "no_extraction_plan",
                    message = "No extraction plans were found for the selected plan IDs.",
                    plan_id = paste(plan_id, collapse = ", "),
                    action = "Run EsgStore$plan_region() and EsgStore$extract() first."
                ))
            }
            missing_plan <- setdiff(plan_id, coverage$plan_id)
            for (id in missing_plan) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "extraction",
                    severity = "error",
                    code = "missing_extraction_plan",
                    message = sprintf("Extraction plan %s was not found.", id),
                    plan_id = id,
                    action = "Check the supplied plan IDs."
                )
            }
            incomplete <- coverage[!coverage$complete]
            for (i in seq_len(nrow(incomplete))) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "extraction",
                    severity = "error",
                    code = "incomplete_extraction",
                    message = sprintf("Extraction plan %s is incomplete.", incomplete$plan_id[[i]]),
                    plan_id = incomplete$plan_id[[i]],
                    variable_id = store__chr1(incomplete$variable_id[[i]]),
                    action = "Complete extraction before morphing."
                )
            }
            missing_variables <- setdiff(self$required_variables(), unique(coverage$variable_id))
            for (variable_id in missing_variables) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "extraction",
                    severity = severity,
                    code = "missing_required_variable",
                    message = sprintf("Required CMIP variable %s is missing from selected extraction plans.", variable_id),
                    variable_id = variable_id,
                    action = "Add and extract the required variable, or run in relaxed mode."
                )
            }

            result <- private$extraction_rows(plan_id)
            if (!nrow(result)) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "extraction",
                    severity = "error",
                    code = "no_extraction_result",
                    message = "No extraction result files were found for selected plan IDs.",
                    plan_id = paste(plan_id, collapse = ", "),
                    action = "Run EsgStore$extract() first."
                )
                return(morpher__bind_diagnostics(diagnostics))
            }

            pieces <- list()
            for (i in seq_len(nrow(result))) {
                path <- store_abs_path(result$output_path[[i]], root = private$store$path)
                dt <- tryCatch(morpher__parquet_read(private$store, path), error = function(e) e)
                if (inherits(dt, "error")) {
                    diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                        stage = "extraction",
                        severity = "error",
                        code = "parquet_unreadable",
                        message = sprintf("Extraction result cannot be read: %s.", conditionMessage(dt)),
                        plan_id = result$plan_id[[i]],
                        action = "Re-run extraction for this plan."
                    )
                    next
                }
                if (!all(c("time", "variable_id", "value") %in% names(dt))) {
                    diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                        stage = "extraction",
                        severity = "error",
                        code = "invalid_extraction_schema",
                        message = "Extraction result is missing required columns.",
                        plan_id = result$plan_id[[i]],
                        action = "Re-run extraction with the current package version."
                    )
                    next
                }
                dt[, plan_id := result$plan_id[[i]]]
                pieces[[length(pieces) + 1L]] <- dt
            }
            if (!length(pieces)) {
                return(morpher__bind_diagnostics(diagnostics))
            }

            climate <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
            climate[, year := as.integer(format(time, "%Y", tz = "UTC"))]
            climate[, month := as.integer(format(time, "%m", tz = "UTC"))]
            periods <- data.table::as.data.table(periods)
            periods[, year := as.integer(year)]
            climate <- climate[periods, on = "year", nomatch = 0L]
            if (!nrow(climate)) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "extraction",
                    severity = "error",
                    code = "no_period_rows",
                    message = "No extracted climate rows matched the supplied EPW morphing periods.",
                    action = "Check the supplied periods against extracted years."
                )
                return(morpher__bind_diagnostics(diagnostics))
            }
            present <- unique(climate[, .(variable_id, period, month)])
            expected <- data.table::CJ(
                variable_id = self$required_variables(),
                period = unique(periods$period),
                month = 1:12,
                unique = TRUE
            )
            missing <- expected[!present, on = c("variable_id", "period", "month")]
            for (i in seq_len(nrow(missing))) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "extraction",
                    severity = severity,
                    code = "missing_month",
                    message = sprintf(
                        "Required CMIP variable %s has no rows for period %s month %s.",
                        missing$variable_id[[i]], missing$period[[i]], missing$month[[i]]
                    ),
                    variable_id = missing$variable_id[[i]],
                    period = missing$period[[i]],
                    month = missing$month[[i]],
                    action = "Extract a complete morphing period, or run in relaxed mode."
                )
            }
            morpher__bind_diagnostics(diagnostics)
        },

        preflight_summary = function(summary_id, by, strict = TRUE) {
            severity <- if (isTRUE(strict)) "error" else "warning"
            diagnostics <- list()
            climate <- morpher__read_table(private$store, "epw_climate_summary")
            target_summary_id <- summary_id
            climate <- climate[climate[["summary_id"]] == target_summary_id & climate[["stat"]] == "mean"]
            if (!nrow(climate)) {
                return(morpher__diagnostic(
                    stage = "climate_summary",
                    severity = "error",
                    code = "missing_climate_summary",
                    message = sprintf("No climate summary rows were found for summary ID %s.", summary_id),
                    summary_id = summary_id,
                    action = "Run EpwMorpher$summarise_climate() first."
                ))
            }
            missing_by <- setdiff(by, names(climate))
            for (name in missing_by) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "climate_summary",
                    severity = "error",
                    code = "missing_group_column",
                    message = sprintf("Climate summary is missing grouping column %s.", name),
                    summary_id = summary_id,
                    action = "Use grouping columns available in the climate summary."
                )
            }
            if (length(missing_by)) {
                return(morpher__bind_diagnostics(diagnostics))
            }
            missing_variables <- setdiff(self$required_variables(), unique(climate$variable_id))
            for (variable_id in missing_variables) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "climate_summary",
                    severity = severity,
                    code = "missing_required_variable",
                    message = sprintf("Required CMIP variable %s is missing from climate summary.", variable_id),
                    summary_id = summary_id,
                    variable_id = variable_id,
                    action = "Summarise climate data with all required variables, or run in relaxed mode."
                )
            }
            cases <- unique(climate[, by, with = FALSE])
            for (i in seq_len(nrow(cases))) {
                case <- cases[i]
                case_filter <- rep(TRUE, nrow(climate))
                for (name in by) {
                    case_filter <- case_filter & morpher__identical_match(climate[[name]], case[[name]][[1L]])
                }
                case_climate <- climate[case_filter]
                case_id <- morpher__hash(summary_id, morpher__json(as.list(case)))
                present <- unique(case_climate[, .(variable_id, period, month)])
                expected <- data.table::CJ(
                    variable_id = self$required_variables(),
                    period = unique(case_climate$period),
                    month = 1:12,
                    unique = TRUE
                )
                missing <- expected[!present, on = c("variable_id", "period", "month")]
                for (j in seq_len(nrow(missing))) {
                    diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                        stage = "climate_summary",
                        severity = severity,
                        code = "missing_month",
                        message = sprintf(
                            "Climate summary lacks %s for period %s month %s.",
                            missing$variable_id[[j]], missing$period[[j]], missing$month[[j]]
                        ),
                        summary_id = summary_id,
                        case_id = case_id,
                        variable_id = missing$variable_id[[j]],
                        period = missing$period[[j]],
                        month = missing$month[[j]],
                        action = "Rebuild climate summary from complete extraction results."
                    )
                }
            }
            morpher__bind_diagnostics(diagnostics)
        },

        preflight_baseline = function(baseline_id = NULL, strict = TRUE) {
            severity <- if (isTRUE(strict)) "error" else "warning"
            rules <- morpher__recipe_rules(private$recipe)
            fields <- unique(rules[required == TRUE & !derived, epw_field])
            diagnostics <- list()
            if (!is.null(baseline_id)) {
                baseline <- morpher__read_table(private$store, "epw_baseline_summary")
                target_baseline_id <- baseline_id
                baseline <- baseline[baseline[["baseline_id"]] == target_baseline_id & baseline[["stat"]] == "mean"]
                if (!nrow(baseline)) {
                    return(morpher__diagnostic(
                        stage = "baseline",
                        severity = "error",
                        code = "missing_baseline_summary",
                        message = sprintf("No baseline summary rows were found for baseline ID %s.", baseline_id),
                        baseline_id = baseline_id,
                        action = "Run EpwMorpher$summarise_baseline() first."
                    ))
                }
                missing_fields <- setdiff(fields, unique(baseline$epw_field))
                for (field in missing_fields) {
                    diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                        stage = "baseline",
                        severity = severity,
                        code = "missing_epw_field",
                        message = sprintf("Baseline summary is missing EPW field %s.", field),
                        baseline_id = baseline_id,
                        epw_field = field,
                        action = "Use a baseline EPW containing recipe fields, or run in relaxed mode."
                    )
                }
                present <- unique(baseline[, .(epw_field, month)])
                expected <- data.table::CJ(epw_field = fields, month = 1:12, unique = TRUE)
                missing <- expected[!present, on = c("epw_field", "month")]
                for (i in seq_len(nrow(missing))) {
                    diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                        stage = "baseline",
                        severity = severity,
                        code = "missing_baseline_month",
                        message = sprintf("Baseline summary lacks %s for month %s.", missing$epw_field[[i]], missing$month[[i]]),
                        baseline_id = baseline_id,
                        epw_field = missing$epw_field[[i]],
                        month = missing$month[[i]],
                        action = "Rebuild the baseline summary from a complete EPW."
                    )
                }
                return(morpher__bind_diagnostics(diagnostics))
            }

            epw <- private$epw$clone()
            suppressMessages(epw$add_unit())
            data <- data.table::as.data.table(epw$data())
            missing_fields <- setdiff(fields, names(data))
            for (field in missing_fields) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "baseline",
                    severity = severity,
                    code = "missing_epw_field",
                    message = sprintf("Baseline EPW is missing recipe field %s.", field),
                    epw_field = field,
                    action = "Use a baseline EPW containing recipe fields, or run in relaxed mode."
                )
            }
            morpher__bind_diagnostics(diagnostics)
        },

        factor_diagnostics = function(factors, strict = TRUE, morph_id = NA_character_) {
            bad <- factors[factors[["status"]] != "ok"]
            if (!nrow(bad)) {
                return(morpher__empty_diagnostics())
            }
            severity <- if (isTRUE(strict)) "error" else "warning"
            rows <- vector("list", nrow(bad))
            for (i in seq_len(nrow(bad))) {
                message <- switch(
                    bad$status[[i]],
                    dry_baseline_precip = sprintf(
                        "Baseline EPW has no wet hours for positive target precipitation in month %s.",
                        bad$month[[i]]
                    ),
                    zero_reference_precip = sprintf(
                        "Reference precipitation is zero while future precipitation is positive in month %s.",
                        bad$month[[i]]
                    ),
                    unit_conversion_failed = sprintf(
                        "Morphing factor unit conversion failed for %s from %s.",
                        bad$epw_field[[i]], bad$variable_id[[i]]
                    ),
                    sprintf("Morphing factor is not available for %s from %s.", bad$epw_field[[i]], bad$variable_id[[i]])
                )
                action <- switch(
                    bad$status[[i]],
                    dry_baseline_precip = "Use a baseline EPW with wet hours for that month, or run in relaxed mode to keep it dry.",
                    zero_reference_precip = "Provide non-zero historical precipitation for that month, or run in relaxed mode to keep baseline precipitation unchanged.",
                    unit_conversion_failed = if (identical(bad$variable_id[[i]], "pr")) {
                        "Use supported precipitation units such as kg m-2 s-1, or run in relaxed mode after correcting inputs."
                    } else {
                        "Use climate and baseline units that can be converted, or run in relaxed mode after correcting inputs."
                    },
                    "Provide the missing climate or baseline input, or run in relaxed mode."
                )
                rows[[i]] <- morpher__diagnostic(
                    stage = "plan",
                    severity = severity,
                    code = bad$status[[i]],
                    message = message,
                    morph_id = morph_id,
                    case_id = bad$case_id[[i]],
                    variable_id = bad$variable_id[[i]],
                    epw_field = bad$epw_field[[i]],
                    period = bad$period[[i]],
                    month = bad$month[[i]],
                    action = action
                )
            }
            morpher__bind_diagnostics(rows)
        },

        factor_rows = function(morph_id, climate, baseline, by, strict = TRUE, reference = NULL) {
            rules <- morpher__recipe_rules(private$recipe)
            rules <- rules[required == TRUE & !derived]
            cases <- unique(climate[, by, with = FALSE])
            if (!nrow(cases)) {
                cli::cli_abort("No climate summary cases were found.")
            }
            reference_required <- morpher__recipe_requires_reference(private$recipe)
            reference_by <- morpher__reference_case_by(by)
            if (is.null(reference)) {
                reference <- data.table::data.table()
            }
            rows <- list()
            for (i in seq_len(nrow(cases))) {
                case <- cases[i]
                case_filter <- rep(TRUE, nrow(climate))
                for (name in by) {
                    case_filter <- case_filter & morpher__identical_match(climate[[name]], case[[name]][[1L]])
                }
                case_climate <- climate[case_filter]
                case_reference <- reference
                if (nrow(case_reference) && length(reference_by)) {
                    reference_filter <- rep(TRUE, nrow(case_reference))
                    for (name in intersect(reference_by, intersect(names(case), names(case_reference)))) {
                        reference_filter <- reference_filter & morpher__identical_match(case_reference[[name]], case[[name]][[1L]])
                    }
                    case_reference <- case_reference[reference_filter]
                }
                case_id <- morpher__hash(morph_id, morpher__json(as.list(case)))
                for (j in seq_len(nrow(rules))) {
                    rule <- rules[j]
                    target_variable_id <- morpher__rule_primary_variable(rule)
                    for (m in 1:12) {
                        future <- case_climate[case_climate[["variable_id"]] == target_variable_id & case_climate[["month"]] == m]
                        ref <- case_reference[case_reference[["variable_id"]] == target_variable_id & case_reference[["month"]] == m]
                        base <- baseline[epw_field == rule$epw_field[[1L]] & month == m]
                        status <- "ok"
                        if (!nrow(future)) {
                            status <- "missing_climate"
                        } else if (isTRUE(reference_required) && !nrow(ref)) {
                            status <- "missing_reference"
                        } else if (!nrow(base)) {
                            status <- "missing_baseline"
                        }
                        is_precip <- identical(rule$epw_field[[1L]], "liquid_precip_depth") &&
                            identical(target_variable_id, "pr")
                        future_value <- if (nrow(future)) future$value[[1L]] else NA_real_
                        future_units <- if (nrow(future)) store__chr1(future$units[[1L]]) else NA_character_
                        reference_value <- if (nrow(ref)) mean(ref$value, na.rm = TRUE) else NA_real_
                        reference_units <- if (nrow(ref)) store__chr1(ref$units[[1L]]) else NA_character_
                        base_value <- if (nrow(base)) base$value[[1L]] else NA_real_
                        base_units <- if (nrow(base)) store__chr1(base$units[[1L]]) else NA_character_
                        if (is.na(base_units) || !nzchar(base_units)) {
                            base_units <- morpher__default_epw_units(rule$epw_field[[1L]])
                        }
                        if (isTRUE(is_precip)) {
                            base_units <- "mm"
                            if (identical(status, "ok")) {
                                converted <- morpher__precip_summary_depth_checked(
                                    future_value,
                                    future_units,
                                    future$years_json[[1L]],
                                    m
                                )
                                future_value <- converted$value
                                if (!isTRUE(converted$ok)) {
                                    status <- "unit_conversion_failed"
                                }
                            }
                            if (identical(status, "ok") && isTRUE(reference_required)) {
                                converted <- morpher__precip_summary_depth_checked(
                                    ref$value[[1L]],
                                    reference_units,
                                    ref$years_json[[1L]],
                                    m
                                )
                                reference_value <- converted$value
                                if (!isTRUE(converted$ok)) {
                                    status <- "unit_conversion_failed"
                                }
                            }
                            if (identical(status, "ok")) {
                                converted <- morpher__baseline_precip_depth_checked(base_value, store__chr1(base$units[[1L]]), m)
                                base_value <- converted$value
                                if (!isTRUE(converted$ok)) {
                                    status <- "unit_conversion_failed"
                                }
                            }
                            # A positive target cannot be allocated when the
                            # baseline EPW has no wet hours for that month.
                            if (identical(status, "ok") &&
                                !is.na(base_value) && base_value <= .Machine$double.eps &&
                                !is.na(future_value) && future_value > .Machine$double.eps) {
                                status <- "dry_baseline_precip"
                            }
                            if (identical(status, "ok") && isTRUE(reference_required) &&
                                !is.na(reference_value) && reference_value <= .Machine$double.eps &&
                                !is.na(future_value) && future_value > .Machine$double.eps) {
                                status <- "zero_reference_precip"
                            }
                        } else if (identical(status, "ok")) {
                            converted <- morpher__convert_value_checked(future_value, future_units, base_units)
                            future_value <- converted$value
                            if (!isTRUE(converted$ok)) {
                                status <- "unit_conversion_failed"
                            }
                        }
                        if (identical(status, "ok") && isTRUE(reference_required) && !isTRUE(is_precip)) {
                            converted <- morpher__convert_value_checked(reference_value, reference_units, base_units)
                            reference_value <- converted$value
                            if (!isTRUE(converted$ok)) {
                                status <- "unit_conversion_failed"
                            }
                        }
                        comparison_value <- if (isTRUE(reference_required)) reference_value else base_value
                        delta <- if (!is.na(future_value) && !is.na(comparison_value)) future_value - comparison_value else NA_real_
                        alpha <- if (identical(status, "ok") && !is.na(comparison_value) && !isTRUE(all.equal(comparison_value, 0))) {
                            future_value / comparison_value
                        } else {
                            NA_real_
                        }
                        row_case <- as.list(case)
                        rows[[length(rows) + 1L]] <- data.frame(
                            factor_id = morpher__hash(morph_id, case_id, rule$epw_field[[1L]], target_variable_id, m),
                            morph_id = morph_id,
                            case_id = case_id,
                            epw_field = rule$epw_field[[1L]],
                            variable_id = target_variable_id,
                            source_id = store__chr1(row_case$source_id),
                            experiment_id = store__chr1(row_case$experiment_id),
                            variant_label = store__chr1(row_case$variant_label),
                            period = store__chr1(row_case$period),
                            month = as.integer(m),
                            method = rule$method[[1L]],
                            baseline = base_value,
                            reference = reference_value,
                            future = future_value,
                            delta = delta,
                            alpha = alpha,
                            units = base_units,
                            status = status,
                            stringsAsFactors = FALSE
                        )
                    }
                }
            }
            data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
        },

        get_plan = function(morph_id) {
            plans <- morpher__read_table(private$store, "epw_morph_plan")
            target_morph_id <- morph_id
            plan <- plans[plans[["morph_id"]] == target_morph_id]
            if (!nrow(plan)) {
                cli::cli_abort("Morphing plan ID {.val {morph_id}} was not found.")
            }
            plan[1L]
        },

        set_plan_status = function(morph_id, status, error = NA_character_) {
            plan <- private$get_plan(morph_id)
            plan$status <- status
            plan$updated_at <- morpher__now()
            plan$last_error <- store__chr1(error)
            morpher__replace_rows(private$store, "epw_morph_plan", plan, "morph_id")
            invisible(NULL)
        },

        case_metadata_from_case = function(case, data) {
            pick <- function(case_name, data_name = case_name) {
                if (case_name %in% names(case)) {
                    return(store__chr1(case[[case_name]][[1L]]))
                }
                if (data_name %in% names(data) && length(data[[data_name]])) {
                    return(store__chr1(as.character(data[[data_name]][[1L]])))
                }
                NA_character_
            }
            list(
                source_id = pick("source_id"),
                experiment_id = pick("experiment_id"),
                variant_label = pick("variant_label", "member_id"),
                period = pick("period", "interval"),
                site_id = pick("site_id"),
                frequency = pick("frequency"),
                table_id = pick("table_id")
            )
        },

        case_metadata_from_result = function(dt) {
            list(
                source_id = if ("source_id" %in% names(dt)) store__chr1(dt$source_id[[1L]]) else NA_character_,
                experiment_id = if ("experiment_id" %in% names(dt)) store__chr1(dt$experiment_id[[1L]]) else NA_character_,
                variant_label = if ("variant_label" %in% names(dt)) store__chr1(dt$variant_label[[1L]]) else NA_character_,
                period = if ("period" %in% names(dt)) store__chr1(dt$period[[1L]]) else NA_character_
            )
        },

        morph_result_path = function(morph_id, case_id) {
            file.path(private$store$path, "outputs", "epw-morph", morph_id, sprintf("case=%s.parquet", morpher__safe_path(case_id)))
        }
    )
)

morpher__identical_match <- function(x, value) {
    value <- store__chr1(value)
    if (is.na(value)) {
        return(is.na(x))
    }
    as.character(x) == value
}
# }}}
