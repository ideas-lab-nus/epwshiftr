#' @include store.R utils.R
NULL

# epw morph helpers {{{
EPW_MORPH_VARIABLE_LEVELS <- list(
    minimal = c("tas", "hurs"),
    recommended = c("tas", "hurs", "psl", "rlds", "rsds", "sfcWind", "clt"),
    extended = c("tas", "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "psl", "rlds", "rsds", "sfcWind", "clt")
)

EPW_MORPH_BACKEND_REGISTRY <- new.env(parent = emptyenv())

EPW_MORPH_BELCHER_METHOD_DEFAULTS <- c(
    tdb = "stretch",
    rh = "stretch",
    p = "stretch",
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
        "tdew",
        "diff_rad",
        "norm_rad",
        "opaque_cover"
    ),
    epw_field = c(
        "dry_bulb_temperature",
        "relative_humidity",
        "atmospheric_pressure",
        "horizontal_infrared_radiation_intensity_from_sky",
        "global_horizontal_radiation",
        "wind_speed",
        "total_sky_cover",
        "dew_point_temperature",
        "diffuse_horizontal_radiation",
        "direct_normal_radiation",
        "opaque_sky_cover"
    ),
    variable_id = c("tas", "hurs", "psl", "rlds", "rsds", "sfcWind", "clt", NA_character_, NA_character_, NA_character_, NA_character_),
    optional_variable_id = c("tasmax,tasmin", "hursmax,hursmin", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    method = c(EPW_MORPH_BELCHER_METHOD_DEFAULTS, "sky_cover", "derived", "derived", "derived", "derived"),
    required = c(rep(TRUE, 7L), rep(FALSE, 4L)),
    derived = c(rep(FALSE, 7L), rep(TRUE, 4L))
)

epw_morph_normalize_backend_rules <- function(name, rules) {
    rules <- data.table::as.data.table(rules)
    required_cols <- c("step", "epw_field", "variable_id", "method", "required")
    missing_cols <- setdiff(required_cols, names(rules))
    if (length(missing_cols)) {
        cli::cli_abort("EPW morphing backend {.val {name}} rules are missing column(s): {.val {missing_cols}}.")
    }
    if (!"optional_variable_id" %in% names(rules)) {
        rules[, optional_variable_id := NA_character_]
    }
    if (!"derived" %in% names(rules)) {
        rules[, derived := is.na(variable_id) | !nzchar(variable_id)]
    }
    rules[, `:=`(
        step = as.character(step),
        epw_field = as.character(epw_field),
        variable_id = as.character(variable_id),
        optional_variable_id = as.character(optional_variable_id),
        method = as.character(method),
        required = as.logical(required),
        derived = as.logical(derived)
    )]
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

        #' @description
        #' Create an EPW morphing backend.
        #'
        #' @param name Backend name.
        #' @param label Human-readable backend label.
        #' @param methods Named default method vector.
        #' @param method_choices Allowed method values.
        #' @param rules Backend rule table.
        #' @param runner Function taking `(context, backend)` and returning an
        #'        `epw_morph_result`.
        initialize = function(name, label = NULL, methods = NULL, method_choices = NULL, rules, runner) {
            checkmate::assert_string(name, min.chars = 1L)
            checkmate::assert_string(label, null.ok = TRUE)
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
            private$method_defaults <- methods
            private$allowed_methods <- method_choices
            private$rule_table <- epw_morph_normalize_backend_rules(self$name, rules)
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
            unique(rules[required == TRUE & !derived & !is.na(variable_id) & nzchar(variable_id), variable_id])
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
            bad <- setdiff(unname(methods), private$allowed_methods)
            if (length(bad)) {
                cli::cli_abort("Unsupported EPW morphing method value(s): {.val {bad}}.")
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

epw_morph_default_backend_specs <- function() {
    list(
        belcher = EpwMorphBackend$new(
            name = "belcher",
            label = "Belcher statistical downscaling",
            methods = EPW_MORPH_BELCHER_METHOD_DEFAULTS,
            method_choices = EPW_MORPH_BELCHER_METHOD_CHOICES,
            rules = EPW_MORPH_BELCHER_RULES,
            runner = epw_morph_belcher_run
        )
    )
}

epw_morph_register_default_backends <- function() {
    specs <- epw_morph_default_backend_specs()
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
    epw_morph_register_default_backends()
    sort(ls(envir = EPW_MORPH_BACKEND_REGISTRY, all.names = FALSE))
}

#' Get an EPW morphing backend
#'
#' @param name Backend name.
#'
#' @return An [EpwMorphBackend] object.
#' @export
epw_morph_backend <- function(name = "belcher") {
    epw_morph_register_default_backends()
    checkmate::assert_string(name, min.chars = 1L)
    name <- tolower(name)
    if (!exists(name, envir = EPW_MORPH_BACKEND_REGISTRY, inherits = FALSE)) {
        cli::cli_abort("Unknown EPW morphing backend: {.val {name}}.")
    }
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
    epw_morph_register_default_backends()
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
#' @param level Variable set level.
#'
#' @return A character vector of CMIP variable IDs.
#' @export
epw_morph_variables <- function(level = c("recommended", "minimal", "extended")) {
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

    methods <- epw_morph_recipe_methods(methods, backend_spec)
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

epw_morph_now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}

epw_morph_json <- function(x) {
    if (inherits(x, "epw_morph_recipe")) {
        x <- list(
            name = x$name,
            backend = x$backend,
            methods = x$methods,
            rules = as.data.frame(x$rules)
        )
    }
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
}

epw_morph_recipe_rules <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    data.table::as.data.table(recipe$rules)
}

epw_morph_recipe_methods <- function(methods = NULL, backend = epw_morph_backend("belcher")) {
    if (!inherits(backend, "EpwMorphBackend")) {
        cli::cli_abort("`backend` must be an {.cls EpwMorphBackend} object.")
    }
    backend$validate_methods(methods)
}

epw_morph_recipe_method_overrides <- function(recipe) {
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

epw_morph_hash <- function(...) {
    store__hash(...)
}

epw_morph_hash_rows <- function(...) {
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
        do.call(epw_morph_hash, lapply(args, `[[`, i))
    }, character(1L))
}

epw_morph_private_store <- function(store) {
    if (!inherits(store, "EsgStore")) {
        cli::cli_abort("`store` must be an {.cls EsgStore} object.")
    }
    private <- priv(store)
    private$check_open()
    private
}

epw_morph_replace_rows <- function(store, table, rows, key) {
    epw_morph_private_store(store)$replace_rows(table, as.data.frame(rows), key)
    invisible(rows)
}

epw_morph_read_table <- function(store, table) {
    epw_morph_private_store(store)$read_table(table)
}

epw_morph_delete_by_key <- function(store, table, key, values) {
    epw_morph_private_store(store)$delete_by_key(table, key, values)
    invisible(NULL)
}

epw_morph_units_label <- function(x) {
    if (!inherits(x, "units")) {
        return(NA_character_)
    }
    out <- tryCatch(units::deparse_unit(x), error = function(e) NA_character_)
    if (length(out) != 1L || is.na(out) || !nzchar(out)) NA_character_ else out
}

epw_morph_drop_units <- function(x) {
    if (inherits(x, "units")) {
        return(as.numeric(units::drop_units(x)))
    }
    as.numeric(x)
}

epw_morph_unit_alias <- function(x) {
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
        x
    )
}

epw_morph_convert_value <- function(value, from, to) {
    epw_morph_convert_value_checked(value, from, to)$value
}

epw_morph_convert_value_checked <- function(value, from, to) {
    from <- epw_morph_unit_alias(from)
    to <- epw_morph_unit_alias(to)
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

epw_morph_default_epw_units <- function(field) {
    switch(
        field,
        dry_bulb_temperature = "degC",
        relative_humidity = "%",
        atmospheric_pressure = "Pa",
        horizontal_infrared_radiation_intensity_from_sky = "W/m^2",
        global_horizontal_radiation = "W/m^2",
        wind_speed = "m/s",
        total_sky_cover = NA_character_,
        NA_character_
    )
}

epw_morph_case_columns <- function() {
    c("source_id", "experiment_id", "variant_label", "period")
}

epw_morph_safe_path <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "unknown"
    gsub("[^A-Za-z0-9_.=-]+", "-", x)
}

epw_morph_parquet_read <- function(store, path) {
    conn <- epw_morph_private_store(store)$conn
    data.table::as.data.table(ddb_query(conn, sprintf(
        "SELECT * FROM read_parquet(%s)",
        ddb_literal(conn, path)
    )))
}

epw_morph_monthly_long <- function(data, id_cols, value_cols, units_map) {
    rows <- list()
    for (field in value_cols) {
        if (!field %in% names(data)) {
            next
        }
        value <- epw_morph_drop_units(data[[field]])
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

epw_morph_stat_rows <- function(dt) {
    mean_rows <- dt[, .(
        value = mean(value, na.rm = TRUE),
        lon = if ("lon" %in% names(.SD)) mean(lon, na.rm = TRUE) else NA_real_,
        lat = if ("lat" %in% names(.SD)) mean(lat, na.rm = TRUE) else NA_real_,
        dist = if ("dist" %in% names(.SD)) mean(dist, na.rm = TRUE) else NA_real_,
        n_records = .N
    ), by = .(plan_id, site_id, source_id, experiment_id, variant_label, frequency, table_id, variable_id, period, month, units)]
    mean_rows[, stat := "mean"]

    min_rows <- dt[, .(
        value = min(value, na.rm = TRUE),
        lon = if ("lon" %in% names(.SD)) mean(lon, na.rm = TRUE) else NA_real_,
        lat = if ("lat" %in% names(.SD)) mean(lat, na.rm = TRUE) else NA_real_,
        dist = if ("dist" %in% names(.SD)) mean(dist, na.rm = TRUE) else NA_real_,
        n_records = .N
    ), by = .(plan_id, site_id, source_id, experiment_id, variant_label, frequency, table_id, variable_id, period, month, units)]
    min_rows[, stat := "min"]

    max_rows <- dt[, .(
        value = max(value, na.rm = TRUE),
        lon = if ("lon" %in% names(.SD)) mean(lon, na.rm = TRUE) else NA_real_,
        lat = if ("lat" %in% names(.SD)) mean(lat, na.rm = TRUE) else NA_real_,
        dist = if ("dist" %in% names(.SD)) mean(dist, na.rm = TRUE) else NA_real_,
        n_records = .N
    ), by = .(plan_id, site_id, source_id, experiment_id, variant_label, frequency, table_id, variable_id, period, month, units)]
    max_rows[, stat := "max"]

    data.table::rbindlist(list(mean_rows, min_rows, max_rows), use.names = TRUE, fill = TRUE)
}

epw_morph_field_units <- function(data, fields) {
    stats::setNames(lapply(fields, function(field) epw_morph_units_label(data[[field]])), fields)
}

epw_morph_get_epw_path <- function(epw) {
    path <- tryCatch(epw$path(), error = function(e) NULL)
    if (is.null(path) || !length(path) || is.na(path[[1L]]) || !nzchar(path[[1L]])) {
        cli::cli_abort("An {.cls eplusr::Epw} object used by {.cls EpwMorpher} must have a file path.")
    }
    path[[1L]]
}

epw_morph_diagnostic_columns <- function() {
    c(
        "stage", "severity", "code", "message", "plan_id", "summary_id",
        "baseline_id", "morph_id", "case_id", "variable_id", "epw_field",
        "period", "month", "action"
    )
}

epw_morph_empty_diagnostics <- function() {
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
    out[, epw_morph_diagnostic_columns(), with = FALSE]
}

epw_morph_diagnostic <- function(stage, severity, code, message, plan_id = NA_character_,
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
    out[, epw_morph_diagnostic_columns(), with = FALSE]
}

epw_morph_bind_diagnostics <- function(...) {
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
        return(epw_morph_empty_diagnostics())
    }
    out <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
    out[, epw_morph_diagnostic_columns(), with = FALSE]
}

epw_morph_abort_diagnostics <- function(diagnostics, message = "EPW morphing preflight has blocking issues.") {
    errors <- diagnostics[diagnostics$severity == "error"]
    if (!nrow(errors)) {
        return(invisible(NULL))
    }
    cli::cli_abort(c(
        message,
        "x" = "{errors$message[[1L]]}"
    ))
}

epw_morph_json_int_vector <- function(x) {
    as.integer(jsonlite::fromJSON(x))
}

epw_morph_engine_by_columns <- function(by) {
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

epw_morph_context <- function(epw, climate, recipe = epw_morph_recipe("belcher"),
                              years = NULL, labels = NULL, by = character(),
                              case = NULL, strict = TRUE, warning = FALSE) {
    if (!inherits(epw, "Epw")) {
        cli::cli_abort("`epw` must be an {.cls eplusr::Epw} object.")
    }
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    climate <- data.table::as.data.table(data.table::copy(climate))
    if (!"time" %in% names(climate) && "datetime" %in% names(climate)) {
        climate[, time := datetime]
    }
    if (!"variable_id" %in% names(climate) && "variable" %in% names(climate)) {
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
    checkmate::assert_character(by, any.missing = FALSE, unique = TRUE)
    structure(
        list(
            epw = epw$clone(),
            climate = climate,
            recipe = recipe,
            years = years,
            labels = labels,
            by = by,
            case = case,
            strict = strict,
            warning = warning
        ),
        class = "epw_morph_context"
    )
}

epw_morph_context_year_labels <- function(context) {
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

epw_morph_context_algorithm_climate <- function(context) {
    climate <- data.table::copy(context$climate)
    if (!"time" %in% names(climate)) {
        cli::cli_abort("Canonical EPW morphing climate data must contain `time`.")
    }
    if (!"variable_id" %in% names(climate)) {
        cli::cli_abort("Canonical EPW morphing climate data must contain `variable_id`.")
    }
    if (!"value" %in% names(climate)) {
        cli::cli_abort("Canonical EPW morphing climate data must contain `value`.")
    }
    fill_col <- function(name, value) {
        if (!name %in% names(climate)) {
            if (!length(value)) {
                value <- rep(NA, nrow(climate))
            } else if (length(value) == 1L) {
                value <- rep(value, nrow(climate))
            }
            data.table::set(climate, j = name, value = value)
        }
    }
    fill_col("activity_id", if ("activity_drs" %in% names(climate)) climate$activity_drs else NA_character_)
    fill_col("institution_id", NA_character_)
    fill_col("source_id", NA_character_)
    fill_col("experiment_id", NA_character_)
    fill_col("variant_label", if ("member_id" %in% names(climate)) climate$member_id else NA_character_)
    fill_col("table_id", if ("frequency" %in% names(climate)) climate$frequency else NA_character_)
    fill_col("lon", NA_real_)
    fill_col("lat", NA_real_)
    fill_col("dist", NA_real_)
    fill_col("variable_long_name", if ("description" %in% names(climate)) climate$description else climate$variable_id)
    fill_col("units", NA_character_)
    out <- data.table::data.table(
        activity_drs = store__chr(climate$activity_id),
        institution_id = store__chr(climate$institution_id),
        source_id = store__chr(climate$source_id),
        experiment_id = store__chr(climate$experiment_id),
        member_id = store__chr(climate$variant_label),
        table_id = store__chr(climate$table_id),
        lon = as.numeric(climate$lon),
        lat = as.numeric(climate$lat),
        dist = as.numeric(climate$dist),
        datetime = climate$time,
        variable = as.character(climate$variable_id),
        description = ifelse(is.na(climate$variable_long_name) | !nzchar(climate$variable_long_name), as.character(climate$variable_id), as.character(climate$variable_long_name)),
        units = as.character(climate$units),
        value = as.numeric(climate$value)
    )
    out[]
}

epw_morph_engine_complete_data <- function(epw, parts, by = character()) {
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
    cols_by <- intersect(epw_morph_engine_by_columns(by), Reduce(intersect, lapply(parts, names)))
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
                             diagnostics = epw_morph_empty_diagnostics(), factors = NULL) {
    checkmate::assert_class(context, "epw_morph_context")
    if (!inherits(epw, "Epw")) {
        cli::cli_abort("`epw` must be an {.cls eplusr::Epw} object.")
    }
    if (missing(data)) {
        cli::cli_abort("`data` must be supplied.")
    }
    checkmate::assert_list(parts, names = "named")
    data <- data.table::as.data.table(data.table::copy(data))
    epw_morph_engine_output(context, epw, parts = parts, data = data, diagnostics = diagnostics, factors = factors)
}

epw_morph_engine_output <- function(context, epw, parts, data = NULL, diagnostics = epw_morph_empty_diagnostics(), factors = NULL) {
    if (is.null(data)) {
        data <- epw_morph_engine_complete_data(epw, parts, by = context$by)
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

epw_morph_result_as_morphed <- function(result) {
    out <- c(list(epw = result$epw), result$parts)
    class(out) <- "epw_cmip6_morphed"
    out
}

epw_morph_run_context <- function(context) {
    checkmate::assert_class(context, "epw_morph_context")
    backend <- epw_morph_backend(context$recipe$backend)
    result <- backend$run(context)
    if (!inherits(result, "epw_morph_result")) {
        cli::cli_abort("EPW morphing backend {.val {backend$name}} did not return an {.cls epw_morph_result}.")
    }
    result
}

epw_morph_belcher_run <- function(context, backend = NULL) {
    methods <- context$recipe$methods
    data_cmip <- epw_morph_context_algorithm_climate(context)
    epw <- context$epw$clone()
    data_epw <- suppressMessages(epw$add_unit()$data())
    year_labels <- epw_morph_context_year_labels(context)
    years <- year_labels$years
    labels <- year_labels$labels

    tas <- data_cmip[J("tas"), on = "variable", nomatch = NULL]
    if (!nrow(tas)) {
        tdb <- data.table::data.table()
    } else {
        tasmax <- data_cmip[J("tasmax"), on = "variable", nomatch = NULL]
        tasmin <- data_cmip[J("tasmin"), on = "variable", nomatch = NULL]
        if (!nrow(tasmax)) tasmax <- NULL
        if (!nrow(tasmin)) tasmin <- NULL
        tdb <- morphing_tdb(data_epw, tas, tasmax, tasmin, years, labels = labels, type = methods[["tdb"]], warning = context$warning)
    }

    hurs <- data_cmip[J("hurs"), on = "variable", nomatch = NULL]
    if (!nrow(hurs)) {
        rh <- data.table::data.table()
    } else {
        hursmax <- data_cmip[J("hursmax"), on = "variable", nomatch = NULL]
        hursmin <- data_cmip[J("hursmin"), on = "variable", nomatch = NULL]
        if (!nrow(hursmax)) hursmax <- NULL
        if (!nrow(hursmin)) hursmin <- NULL
        rh <- morphing_rh(data_epw, hurs, hursmax, hursmin, years, labels = labels, type = methods[["rh"]], warning = context$warning)
    }

    tdew <- if (!nrow(tdb) || !nrow(rh)) data.table::data.table() else morphing_tdew(tdb, rh)

    psl <- data_cmip[J("psl"), on = "variable", nomatch = NULL]
    p <- if (!nrow(psl)) {
        data.table::data.table()
    } else {
        morphing_pa(data_epw, psl, years, labels = labels, type = methods[["p"]], warning = context$warning)
    }

    data_epw[, horizontal_infrared_radiation_intensity_from_sky :=
        units::set_units(units::drop_units(horizontal_infrared_radiation_intensity_from_sky), "W/m^2"
    )]
    rlds <- data_cmip[J("rlds"), on = "variable", nomatch = NULL]
    hor_ir <- if (!nrow(rlds)) {
        data.table::data.table()
    } else {
        morphing_hor_ir(data_epw, rlds, years, labels = labels, type = methods[["hor_ir"]], warning = context$warning)
    }

    data_epw[, global_horizontal_radiation :=
        units::set_units(units::drop_units(global_horizontal_radiation), "W/m^2"
    )]
    rsds <- data_cmip[J("rsds"), on = "variable", nomatch = NULL]
    glob_rad <- if (!nrow(rsds)) {
        data.table::data.table()
    } else {
        morphing_glob_rad(data_epw, rsds, years, labels = labels, type = methods[["glob_rad"]], warning = context$warning)
    }
    diff_rad <- if (!nrow(glob_rad)) data.table::data.table() else morphing_diff_rad(data_epw, glob_rad)
    norm_rad <- if (!nrow(glob_rad) || !nrow(diff_rad)) data.table::data.table() else morphing_norm_rad(glob_rad, diff_rad)

    sfcWind <- data_cmip[J("sfcWind"), on = "variable", nomatch = NULL]
    wind <- if (!nrow(sfcWind)) {
        data.table::data.table()
    } else {
        morphing_wind_speed(data_epw, sfcWind, years, labels = labels, type = methods[["wind"]], warning = context$warning)
    }

    clt <- data_cmip[J("clt"), on = "variable", nomatch = NULL]
    total_cover <- if (!nrow(clt)) {
        data.table::data.table()
    } else {
        morphing_total_sky_cover(data_epw, clt, years, labels = labels, warning = context$warning)
    }
    opaque_cover <- if (!nrow(total_cover)) data.table::data.table() else morphing_opaque_sky_cover(data_epw, total_cover)

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
        opaque_cover = opaque_cover
    )
    suppressMessages(epw$drop_unit())
    for (name in names(parts)) {
        remove_units(parts[[name]], intersect(names(parts[[name]]), names(data_epw)))
    }
    epw_morph_engine_output(context, epw, parts)
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
            private$store_private <- epw_morph_private_store(store)
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
            rules <- epw_morph_recipe_rules(private$recipe)
            unique(rules[required == TRUE & !derived & !is.na(variable_id) & nzchar(variable_id), variable_id])
        },

        #' @description
        #' Preflight EPW morphing inputs without writing store state.
        #'
        #' @param plan_id Optional extraction plan IDs.
        #' @param periods Optional period table from [epw_morph_periods()].
        #' @param summary_id Optional climate summary ID.
        #' @param baseline_id Optional baseline summary ID.
        #' @param by Climate grouping columns.
        #' @param strict Whether required-data issues are errors.
        preflight = function(plan_id = NULL, periods = NULL, summary_id = NULL, baseline_id = NULL,
                             by = c("source_id", "experiment_id", "variant_label", "period"), strict = TRUE) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            if (!is.null(periods)) {
                checkmate::assert_data_frame(periods)
                checkmate::assert_names(names(periods), must.include = c("period", "year"))
            }
            checkmate::assert_string(summary_id, null.ok = TRUE)
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

            epw_morph_bind_diagnostics(
                if (!is.null(plan_id)) private$preflight_extraction(plan_id, periods, strict = strict) else epw_morph_empty_diagnostics(),
                if (!is.null(summary_id)) private$preflight_summary(summary_id, by, strict = strict) else epw_morph_empty_diagnostics(),
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

            diagnostics <- self$preflight(plan_id = plan_id, periods = periods, strict = strict)
            if (isTRUE(strict)) {
                epw_morph_abort_diagnostics(
                    diagnostics,
                    "Cannot summarise climate data because selected extraction plans have blocking issues."
                )
            }

            summary_id <- private$summary_id(plan_id, periods)
            current <- epw_morph_read_table(private$store, "epw_climate_summary")
            target_summary_id <- summary_id
            current_summary <- current[current[["summary_id"]] == target_summary_id]
            current_usable <- nrow(current_summary) &&
                all(c("years_json", "lon", "lat", "dist") %in% names(current_summary)) &&
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
                dt <- epw_morph_parquet_read(private$store, path)
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
                years_json = epw_morph_json(as.integer(sort(unique(year))))
            ), by = "period"]
            climate <- climate[periods, on = "year", nomatch = 0L]
            if (!nrow(climate)) {
                cli::cli_abort("No extracted climate rows matched the supplied EPW morphing periods.")
            }

            rows <- epw_morph_stat_rows(climate)
            rows <- period_years[rows, on = "period"]
            rows[, `:=`(
                summary_id = summary_id,
                coverage = 1,
                created_at = epw_morph_now()
            )]
            rows[, summary_row_id := epw_morph_hash_rows(summary_id, plan_id, variable_id, period, month, stat)]
            data.table::setcolorder(rows, c(
                "summary_row_id", "summary_id", "plan_id", "site_id", "source_id",
                "experiment_id", "variant_label", "frequency", "table_id",
                "variable_id", "period", "month", "stat", "value", "units",
                "lon", "lat", "dist", "years_json", "coverage", "n_records", "created_at"
            ))
            epw_morph_delete_by_key(private$store, "epw_climate_summary", "summary_id", summary_id)
            epw_morph_replace_rows(private$store, "epw_climate_summary", rows, "summary_row_id")
            rows[]
        },

        #' @description
        #' Summarise baseline EPW weather by month.
        #'
        #' @param overwrite Whether to replace existing rows.
        summarise_baseline = function(overwrite = FALSE) {
            checkmate::assert_flag(overwrite)
            baseline_id <- private$baseline_id()
            current <- epw_morph_read_table(private$store, "epw_baseline_summary")
            target_baseline_id <- baseline_id
            current_baseline <- current[current[["baseline_id"]] == target_baseline_id]
            if (!isTRUE(overwrite) && nrow(current_baseline)) {
                return(current_baseline)
            }

            epw <- private$epw$clone()
            suppressMessages(epw$add_unit())
            data <- data.table::as.data.table(epw$data())
            rules <- epw_morph_recipe_rules(private$recipe)
            fields <- unique(rules[required == TRUE & !derived, epw_field])
            fields <- intersect(fields, names(data))
            units_map <- epw_morph_field_units(data, fields)
            rows <- epw_morph_monthly_long(data, character(), fields, units_map)
            if (!nrow(rows)) {
                cli::cli_abort("No recipe EPW fields were found in the baseline EPW.")
            }
            rows[, `:=`(
                baseline_id = baseline_id,
                epw_id = private$epw_id,
                created_at = epw_morph_now()
            )]
            rows[, baseline_row_id := epw_morph_hash_rows(baseline_id, epw_field, month, stat)]
            data.table::setcolorder(rows, c(
                "baseline_row_id", "baseline_id", "epw_id", "epw_field",
                "month", "stat", "value", "units", "created_at"
            ))
            epw_morph_delete_by_key(private$store, "epw_baseline_summary", "baseline_id", baseline_id)
            epw_morph_replace_rows(private$store, "epw_baseline_summary", rows, "baseline_row_id")
            rows[]
        },

        #' @description
        #' Create a morphing plan and monthly factors.
        #'
        #' @param summary_id Climate summary ID.
        #' @param baseline_id Baseline summary ID. If `NULL`, baseline summary is created.
        #' @param by Climate grouping columns.
        #' @param strict Whether missing required variables are blocking errors.
        #' @param overwrite Whether to replace an existing plan.
        plan = function(summary_id, baseline_id = NULL, by = c("source_id", "experiment_id", "variant_label", "period"),
                        strict = TRUE, overwrite = FALSE) {
            checkmate::assert_string(summary_id, min.chars = 1L)
            checkmate::assert_string(baseline_id, null.ok = TRUE)
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            checkmate::assert_flag(overwrite)

            preview <- self$preview_plan(summary_id = summary_id, baseline_id = baseline_id, by = by, strict = strict)
            morph_id <- preview$plan$morph_id[[1L]]
            current <- epw_morph_read_table(private$store, "epw_morph_plan")
            target_morph_id <- morph_id
            current_plan <- current[current[["morph_id"]] == target_morph_id]
            if (!isTRUE(overwrite) && nrow(current_plan)) {
                return(current_plan)
            }

            epw_morph_delete_by_key(private$store, "epw_morph_factor", "morph_id", morph_id)
            epw_morph_replace_rows(private$store, "epw_morph_plan", preview$plan, "morph_id")
            epw_morph_replace_rows(private$store, "epw_morph_factor", preview$factors, "factor_id")
            data.table::as.data.table(preview$plan)
        },

        #' @description
        #' Preview a morphing plan and monthly factors without writing store state.
        #'
        #' @param summary_id Climate summary ID.
        #' @param baseline_id Baseline summary ID. If `NULL`, baseline summary is created.
        #' @param by Climate grouping columns.
        #' @param strict Whether missing required variables are blocking errors.
        preview_plan = function(summary_id, baseline_id = NULL, by = c("source_id", "experiment_id", "variant_label", "period"),
                                strict = TRUE) {
            checkmate::assert_string(summary_id, min.chars = 1L)
            checkmate::assert_string(baseline_id, null.ok = TRUE)
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            if (is.null(baseline_id)) {
                baseline_id <- unique(self$summarise_baseline()$baseline_id)[[1L]]
            }

            climate <- epw_morph_read_table(private$store, "epw_climate_summary")
            target_summary_id <- summary_id
            climate <- climate[climate[["summary_id"]] == target_summary_id & climate[["stat"]] == "mean"]
            if (!nrow(climate)) {
                cli::cli_abort("No climate summary rows were found for summary ID {.val {summary_id}}.")
            }
            baseline <- epw_morph_read_table(private$store, "epw_baseline_summary")
            target_baseline_id <- baseline_id
            baseline <- baseline[baseline[["baseline_id"]] == target_baseline_id & baseline[["stat"]] == "mean"]
            if (!nrow(baseline)) {
                cli::cli_abort("No baseline summary rows were found for baseline ID {.val {baseline_id}}.")
            }

            morph_id <- private$morph_id(summary_id, baseline_id, by, strict)
            factors <- private$factor_rows(morph_id, climate, baseline, by, strict = strict)
            diagnostics <- epw_morph_bind_diagnostics(
                private$preflight_summary(summary_id, by, strict = strict),
                private$preflight_baseline(baseline_id, strict = strict),
                private$factor_diagnostics(factors, strict = strict, morph_id = morph_id)
            )
            status <- if (any(diagnostics$severity == "error") && isTRUE(strict)) "blocked" else "planned"
            now <- epw_morph_now()
            plan <- data.table::data.table(
                morph_id = morph_id,
                epw_id = private$epw_id,
                summary_id = summary_id,
                baseline_id = baseline_id,
                label = store__chr1(private$label),
                by_json = epw_morph_json(by),
                recipe_json = epw_morph_json(private$recipe),
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
            factors <- epw_morph_read_table(private$store, "epw_morph_factor")
            target_morph_id <- morph_id
            factors <- factors[factors[["morph_id"]] == target_morph_id]
            if (!nrow(factors)) {
                return(epw_morph_diagnostic(
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

            existing <- epw_morph_read_table(private$store, "epw_morph_result")
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
                    by <- private$plan_by(plan)
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
                        context <- epw_morph_context(
                            epw = private$epw,
                            climate = case_climate,
                            recipe = private$recipe,
                            by = by,
                            case = case,
                            strict = isTRUE(plan$strict[[1L]]),
                            warning = FALSE
                        )
                        case_result <- epw_morph_run_context(context)
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
                            result_id = epw_morph_hash(morph_id, case_id, path),
                            morph_id = morph_id,
                            case_id = case_id,
                            artifact_id = artifact_id,
                            output_path = store_rel_path(path, root = private$store$path),
                            row_count = nrow(case_data),
                            created_at = epw_morph_now(),
                            stringsAsFactors = FALSE
                        )
                    }
                    results <- data.table::rbindlist(result_rows, use.names = TRUE, fill = TRUE)
                    epw_morph_delete_by_key(private$store, "epw_morph_result", "morph_id", morph_id)
                    epw_morph_replace_rows(private$store, "epw_morph_result", results, "result_id")
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
            results <- epw_morph_read_table(private$store, "epw_morph_result")
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
                    current_outputs <- epw_morph_read_table(private$store, "epw_output")
                    target_morph_id <- morph_id
                    current_outputs <- current_outputs[current_outputs[["morph_id"]] == target_morph_id]
                    output_rows <- vector("list", nrow(results))
                    for (i in seq_len(nrow(results))) {
                        result <- results[i]
                        result_path <- store_abs_path(result$output_path[[1L]], root = private$store$path)
                        dt <- epw_morph_parquet_read(private$store, result_path)
                        meta <- private$case_metadata_from_result(dt)
                        label <- paste(epw_morph_safe_path(unlist(meta, use.names = FALSE)), collapse = ".")
                        filename <- paste(tools::file_path_sans_ext(basename(epw_morph_get_epw_path(private$epw))), label, "epw", sep = ".")
                        output_path <- if (isTRUE(separate)) {
                            file.path(root, do.call(file.path, as.list(epw_morph_safe_path(unlist(meta, use.names = FALSE)))), filename)
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
                            output_id = epw_morph_hash(morph_id, result$case_id[[1L]], output_path),
                            morph_id = morph_id,
                            case_id = result$case_id[[1L]],
                            artifact_id = artifact_id,
                            path = output_rel,
                            source_id = store__chr1(meta$source_id),
                            experiment_id = store__chr1(meta$experiment_id),
                            variant_label = store__chr1(meta$variant_label),
                            period = store__chr1(meta$period),
                            created_at = epw_morph_now(),
                            stringsAsFactors = FALSE
                        )
                    }
                    outputs <- data.table::rbindlist(output_rows, use.names = TRUE, fill = TRUE)
                    epw_morph_delete_by_key(private$store, "epw_output", "morph_id", morph_id)
                    epw_morph_replace_rows(private$store, "epw_output", outputs, "output_id")
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
        #' @param by Climate grouping columns.
        #' @param strict Whether blocking diagnostics should abort the workflow.
        #' @param dir Output directory. Relative paths are resolved under the store root.
        #' @param separate Whether to create case subdirectories.
        #' @param overwrite Whether to overwrite existing plan, result, and EPW outputs.
        #' @param resume Whether to reuse complete existing result and EPW outputs.
        workflow = function(plan_id, periods, by = c("source_id", "experiment_id", "variant_label", "period"),
                            strict = TRUE, dir = "outputs/future-epw", separate = TRUE,
                            overwrite = FALSE, resume = TRUE) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_data_frame(periods)
            checkmate::assert_names(names(periods), must.include = c("period", "year"))
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            checkmate::assert_string(dir, min.chars = 1L, null.ok = TRUE)
            checkmate::assert_flag(separate)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)

            preflight <- self$preflight(plan_id = plan_id, periods = periods, by = by, strict = strict)
            if (isTRUE(strict)) {
                epw_morph_abort_diagnostics(preflight)
            }
            climate <- self$summarise_climate(plan_id = plan_id, periods = periods, strict = strict, overwrite = overwrite)
            baseline <- self$summarise_baseline(overwrite = overwrite)
            preview <- self$preview_plan(
                summary_id = unique(climate$summary_id)[[1L]],
                baseline_id = unique(baseline$baseline_id)[[1L]],
                by = by,
                strict = strict
            )
            plan <- self$plan(
                summary_id = unique(climate$summary_id)[[1L]],
                baseline_id = unique(baseline$baseline_id)[[1L]],
                by = by,
                strict = strict,
                overwrite = overwrite
            )
            diagnostics <- epw_morph_bind_diagnostics(preflight, preview$diagnostics)
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
            plans <- epw_morph_read_table(private$store, "epw_morph_plan")
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
            outputs <- epw_morph_read_table(private$store, "epw_output")
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
                epw_path <- epw_morph_get_epw_path(epw)
                epw_obj <- epw$clone()
            } else {
                checkmate::assert_string(epw, min.chars = 1L)
                checkmate::assert_file_exists(epw)
                epw_path <- epw
                epw_obj <- eplusr::read_epw(epw_path)
            }
            checksum <- store_hash_file(epw_path, "sha256")
            epw_id <- epw_morph_hash("epw", checksum, private$site_id, private$label)
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
            now <- epw_morph_now()
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
            epw_morph_replace_rows(private$store, "epw_source", row, "epw_id")
            private$epw_id <- epw_id
            private$epw <- eplusr::read_epw(target)
            invisible(NULL)
        },

        baseline_id = function() {
            epw_morph_hash("baseline", private$epw_id, epw_morph_json(private$recipe))
        },

        summary_id = function(plan_id, periods) {
            epw_morph_hash("summary", private$epw_id, paste(sort(plan_id), collapse = "\r"), epw_morph_json(periods))
        },

        morph_id = function(summary_id, baseline_id, by, strict) {
            strict_token <- if (isTRUE(strict)) "strict=true" else "strict=false"
            epw_morph_hash("morph", private$epw_id, summary_id, baseline_id, paste(by, collapse = "\r"), epw_morph_json(private$recipe), strict_token)
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
            climate <- epw_morph_read_table(private$store, "epw_climate_summary")
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
                    year = epw_morph_json_int_vector(period_rows$years_json[[i]])
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
                dt <- epw_morph_parquet_read(private$store, path)
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

            catalog <- epw_morph_read_table(private$store, "file_catalog")
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
                epw_morph_hash(plan$morph_id[[1L]], epw_morph_json(as.list(cases[i])))
            }, character(1L))
            cases[, case_id := case_ids]
            cases[]
        },

        filter_case_climate = function(climate, case, by) {
            case_values <- as.list(case[1L])
            keep <- rep(TRUE, nrow(climate))
            for (name in intersect(by, intersect(names(case), names(climate)))) {
                value <- store__chr1(case_values[[name]])
                keep <- keep & identical_match(climate[[name]], value)
            }
            climate[keep][]
        },

        preflight_extraction = function(plan_id, periods, strict = TRUE) {
            severity <- if (isTRUE(strict)) "error" else "warning"
            diagnostics <- list()
            coverage <- private$store$coverage(plan_id = plan_id)
            if (!nrow(coverage)) {
                return(epw_morph_diagnostic(
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
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
                    stage = "extraction",
                    severity = "error",
                    code = "no_extraction_result",
                    message = "No extraction result files were found for selected plan IDs.",
                    plan_id = paste(plan_id, collapse = ", "),
                    action = "Run EsgStore$extract() first."
                )
                return(epw_morph_bind_diagnostics(diagnostics))
            }

            pieces <- list()
            for (i in seq_len(nrow(result))) {
                path <- store_abs_path(result$output_path[[i]], root = private$store$path)
                dt <- tryCatch(epw_morph_parquet_read(private$store, path), error = function(e) e)
                if (inherits(dt, "error")) {
                    diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
                    diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
                return(epw_morph_bind_diagnostics(diagnostics))
            }

            climate <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
            climate[, year := as.integer(format(time, "%Y", tz = "UTC"))]
            climate[, month := as.integer(format(time, "%m", tz = "UTC"))]
            periods <- data.table::as.data.table(periods)
            periods[, year := as.integer(year)]
            climate <- climate[periods, on = "year", nomatch = 0L]
            if (!nrow(climate)) {
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
                    stage = "extraction",
                    severity = "error",
                    code = "no_period_rows",
                    message = "No extracted climate rows matched the supplied EPW morphing periods.",
                    action = "Check the supplied periods against extracted years."
                )
                return(epw_morph_bind_diagnostics(diagnostics))
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
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
            epw_morph_bind_diagnostics(diagnostics)
        },

        preflight_summary = function(summary_id, by, strict = TRUE) {
            severity <- if (isTRUE(strict)) "error" else "warning"
            diagnostics <- list()
            climate <- epw_morph_read_table(private$store, "epw_climate_summary")
            target_summary_id <- summary_id
            climate <- climate[climate[["summary_id"]] == target_summary_id & climate[["stat"]] == "mean"]
            if (!nrow(climate)) {
                return(epw_morph_diagnostic(
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
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
                    stage = "climate_summary",
                    severity = "error",
                    code = "missing_group_column",
                    message = sprintf("Climate summary is missing grouping column %s.", name),
                    summary_id = summary_id,
                    action = "Use grouping columns available in the climate summary."
                )
            }
            if (length(missing_by)) {
                return(epw_morph_bind_diagnostics(diagnostics))
            }
            missing_variables <- setdiff(self$required_variables(), unique(climate$variable_id))
            for (variable_id in missing_variables) {
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
                    case_filter <- case_filter & identical_match(climate[[name]], case[[name]][[1L]])
                }
                case_climate <- climate[case_filter]
                case_id <- epw_morph_hash(summary_id, epw_morph_json(as.list(case)))
                present <- unique(case_climate[, .(variable_id, period, month)])
                expected <- data.table::CJ(
                    variable_id = self$required_variables(),
                    period = unique(case_climate$period),
                    month = 1:12,
                    unique = TRUE
                )
                missing <- expected[!present, on = c("variable_id", "period", "month")]
                for (j in seq_len(nrow(missing))) {
                    diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
            epw_morph_bind_diagnostics(diagnostics)
        },

        preflight_baseline = function(baseline_id = NULL, strict = TRUE) {
            severity <- if (isTRUE(strict)) "error" else "warning"
            rules <- epw_morph_recipe_rules(private$recipe)
            fields <- unique(rules[required == TRUE & !derived, epw_field])
            diagnostics <- list()
            if (!is.null(baseline_id)) {
                baseline <- epw_morph_read_table(private$store, "epw_baseline_summary")
                target_baseline_id <- baseline_id
                baseline <- baseline[baseline[["baseline_id"]] == target_baseline_id & baseline[["stat"]] == "mean"]
                if (!nrow(baseline)) {
                    return(epw_morph_diagnostic(
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
                    diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
                    diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
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
                return(epw_morph_bind_diagnostics(diagnostics))
            }

            epw <- private$epw$clone()
            suppressMessages(epw$add_unit())
            data <- data.table::as.data.table(epw$data())
            missing_fields <- setdiff(fields, names(data))
            for (field in missing_fields) {
                diagnostics[[length(diagnostics) + 1L]] <- epw_morph_diagnostic(
                    stage = "baseline",
                    severity = severity,
                    code = "missing_epw_field",
                    message = sprintf("Baseline EPW is missing recipe field %s.", field),
                    epw_field = field,
                    action = "Use a baseline EPW containing recipe fields, or run in relaxed mode."
                )
            }
            epw_morph_bind_diagnostics(diagnostics)
        },

        factor_diagnostics = function(factors, strict = TRUE, morph_id = NA_character_) {
            bad <- factors[factors[["status"]] != "ok"]
            if (!nrow(bad)) {
                return(epw_morph_empty_diagnostics())
            }
            severity <- if (isTRUE(strict)) "error" else "warning"
            rows <- vector("list", nrow(bad))
            for (i in seq_len(nrow(bad))) {
                rows[[i]] <- epw_morph_diagnostic(
                    stage = "plan",
                    severity = severity,
                    code = bad$status[[i]],
                    message = sprintf("Morphing factor is not available for %s from %s.", bad$epw_field[[i]], bad$variable_id[[i]]),
                    morph_id = morph_id,
                    case_id = bad$case_id[[i]],
                    variable_id = bad$variable_id[[i]],
                    epw_field = bad$epw_field[[i]],
                    period = bad$period[[i]],
                    month = bad$month[[i]],
                    action = "Provide the missing climate or baseline input, or run in relaxed mode."
                )
            }
            epw_morph_bind_diagnostics(rows)
        },

        factor_rows = function(morph_id, climate, baseline, by, strict = TRUE) {
            rules <- epw_morph_recipe_rules(private$recipe)
            rules <- rules[required == TRUE & !derived]
            cases <- unique(climate[, by, with = FALSE])
            if (!nrow(cases)) {
                cli::cli_abort("No climate summary cases were found.")
            }
            rows <- list()
            for (i in seq_len(nrow(cases))) {
                case <- cases[i]
                case_filter <- rep(TRUE, nrow(climate))
                for (name in by) {
                    case_filter <- case_filter & identical_match(climate[[name]], case[[name]][[1L]])
                }
                case_climate <- climate[case_filter]
                case_id <- epw_morph_hash(morph_id, epw_morph_json(as.list(case)))
                for (j in seq_len(nrow(rules))) {
                    rule <- rules[j]
                    for (m in 1:12) {
                        future <- case_climate[variable_id == rule$variable_id[[1L]] & month == m]
                        base <- baseline[epw_field == rule$epw_field[[1L]] & month == m]
                        status <- "ok"
                        if (!nrow(future)) {
                            status <- "missing_climate"
                        } else if (!nrow(base)) {
                            status <- "missing_baseline"
                        }
                        future_value <- if (nrow(future)) future$value[[1L]] else NA_real_
                        future_units <- if (nrow(future)) store__chr1(future$units[[1L]]) else NA_character_
                        base_value <- if (nrow(base)) base$value[[1L]] else NA_real_
                        base_units <- if (nrow(base)) store__chr1(base$units[[1L]]) else NA_character_
                        if (is.na(base_units) || !nzchar(base_units)) {
                            base_units <- epw_morph_default_epw_units(rule$epw_field[[1L]])
                        }
                        if (identical(status, "ok")) {
                            converted <- epw_morph_convert_value_checked(future_value, future_units, base_units)
                            future_value <- converted$value
                            if (!isTRUE(converted$ok)) {
                                status <- "unit_conversion_failed"
                            }
                        }
                        delta <- if (status %in% c("ok", "unit_conversion_failed")) future_value - base_value else NA_real_
                        alpha <- if (status %in% c("ok", "unit_conversion_failed") && !is.na(base_value) && !isTRUE(all.equal(base_value, 0))) {
                            future_value / base_value
                        } else {
                            NA_real_
                        }
                        row_case <- as.list(case)
                        rows[[length(rows) + 1L]] <- data.frame(
                            factor_id = epw_morph_hash(morph_id, case_id, rule$epw_field[[1L]], rule$variable_id[[1L]], m),
                            morph_id = morph_id,
                            case_id = case_id,
                            epw_field = rule$epw_field[[1L]],
                            variable_id = rule$variable_id[[1L]],
                            source_id = store__chr1(row_case$source_id),
                            experiment_id = store__chr1(row_case$experiment_id),
                            variant_label = store__chr1(row_case$variant_label),
                            period = store__chr1(row_case$period),
                            month = as.integer(m),
                            method = rule$method[[1L]],
                            baseline = base_value,
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
            plans <- epw_morph_read_table(private$store, "epw_morph_plan")
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
            plan$updated_at <- epw_morph_now()
            plan$last_error <- store__chr1(error)
            epw_morph_replace_rows(private$store, "epw_morph_plan", plan, "morph_id")
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
            file.path(private$store$path, "outputs", "epw-morph", morph_id, sprintf("case=%s.parquet", epw_morph_safe_path(case_id)))
        }
    )
)

identical_match <- function(x, value) {
    value <- store__chr1(value)
    if (is.na(value)) {
        return(is.na(x))
    }
    as.character(x) == value
}
# }}}
