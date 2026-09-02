#' @include store.R epw-morph-recipe.R epw-morph-context.R backend-belcher.R utils.R
NULL

# Store-native EPW morpher {{{

#' Create an EPW morpher
#'
#' @param store An [EsgStore] object.
#' @param epw Baseline EPW path, internal `EpwFile`, or an external object
#'   inheriting from `"Epw"`.
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
            profile = x$profile,
            options = unclass(x$options),
            # Named atomic vectors become JSON arrays and lose their step
            # identity. A named list deliberately serializes as an object so
            # queued and resumed jobs retain every method override.
            methods = as.list(x$methods),
            rules = as.data.frame(rules),
            recipe_spec = x$recipe_spec,
            recipe_version = x$recipe_version,
            policy = x$policy,
            components = x$components
        )
    }
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
}

# Resolve an EPW output directory and fail before any filesystem writes when
# the manifest cannot safely store the path relative to the EsgStore root.
morpher__epw_output_root <- function(dir, store_path) {
    root <- store_abs_path(dir, root = store_path)
    tryCatch(
        {
            store_rel_path(root, root = store_path)
            root
        },
        error = function(e) {
            cli::cli_abort(c(
                "EPW output directory must be inside the epwshiftr store root.",
                "x" = "Requested directory: {.path {root}}",
                "i" = "Use a store under your desired output root, or pass a store-relative `dir` such as {.val outputs/future-epw}."
            ))
        }
    )
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
        cli::cli_abort("An internal EPW object used by {.cls EpwMorpher} must have a file path.")
    }
    path[[1L]]
}

morpher__json_int_vector <- function(x) {
    as.integer(jsonlite::fromJSON(x))
}

morpher__normalize_result_manifest <- function(rows) {
    rows <- data.table::as.data.table(data.table::copy(rows))
    defaults <- list(
        output_type = "representative_year",
        sequence_id = NA_character_,
        weather_year = NA_integer_,
        calendar = NA_character_,
        stochastic_seed = NA_integer_,
        member_count = 1L,
        provenance_json = "[]"
    )
    for (name in names(defaults)) {
        if (!name %in% names(rows)) {
            rows[, (name) := defaults[[name]]]
            next
        }
        missing <- is.na(rows[[name]])
        if (is.character(rows[[name]])) {
            missing <- missing | !nzchar(rows[[name]])
        }
        if (name %in% c(
            "sequence_id",
            "weather_year",
            "calendar",
            "stochastic_seed"
        )) {
            next
        }
        data.table::set(
            rows,
            i = which(missing),
            j = name,
            value = defaults[[name]]
        )
    }
    rows[]
}

# A resumed case is complete only when every member promised by its manifest
# still exists; one surviving year must not hide a missing sibling year.
morpher__result_case_complete <- function(rows) {
    if (!nrow(rows)) {
        return(FALSE)
    }
    expected <- unique(as.integer(rows$member_count))
    member_keys <- paste(
        rows$output_type,
        rows$sequence_id,
        rows$weather_year,
        sep = "\r"
    )
    length(expected) == 1L &&
        !is.na(expected) &&
        expected > 0L &&
        nrow(rows) == expected &&
        !anyDuplicated(member_keys) &&
        !anyDuplicated(rows$output_path)
}

# Restore deterministic case and member order after reading rows from DuckDB,
# whose physical row order is not a persistence contract.
morpher__order_result_rows <- function(rows, cases) {
    rows <- data.table::as.data.table(data.table::copy(rows))
    if (!nrow(rows)) {
        return(rows[])
    }
    rows[, (".case_order") := match(rows[["case_id"]], cases)]
    data.table::setorderv(
        rows,
        c(".case_order", "sequence_id", "weather_year", "output_path"),
        na.last = TRUE
    )
    rows[, (".case_order") := NULL]
    rows[]
}

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
        #' @param epw Baseline EPW path, internal `EpwFile`, or an external
        #'   object inheriting from `"Epw"`.
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
        #' @param observed_plan_id Optional observed-weather extraction plan
        #'   IDs.
        #' @param observed_periods Optional observed-weather period table.
        #' @param observed_summary_id Optional observed-weather summary ID.
        #' @param baseline_id Optional baseline summary ID.
        #' @param by Climate grouping columns.
        #' @param strict Whether required-data issues are errors.
        preflight = function(plan_id = NULL, periods = NULL, reference_plan_id = NULL,
                             reference_periods = NULL, summary_id = NULL,
                             reference_summary_id = NULL,
                             observed_plan_id = NULL,
                             observed_periods = NULL,
                             observed_summary_id = NULL,
                             baseline_id = NULL,
                             by = c("source_id", "experiment_id", "variant_label", "period"), strict = TRUE) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_character(reference_plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_character(observed_plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            if (!is.null(periods)) {
                checkmate::assert_data_frame(periods)
                checkmate::assert_names(names(periods), must.include = c("period", "year"))
            }
            if (!is.null(reference_periods)) {
                checkmate::assert_data_frame(reference_periods)
                checkmate::assert_names(names(reference_periods), must.include = c("period", "year"))
            }
            if (!is.null(observed_periods)) {
                checkmate::assert_data_frame(observed_periods)
                checkmate::assert_names(
                    names(observed_periods),
                    must.include = c("period", "year")
                )
            }
            checkmate::assert_string(summary_id, null.ok = TRUE)
            checkmate::assert_string(reference_summary_id, null.ok = TRUE)
            checkmate::assert_string(observed_summary_id, null.ok = TRUE)
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
            if (!is.null(observed_plan_id) && is.null(observed_periods)) {
                cli::cli_abort(
                    "`observed_periods` must be supplied when `observed_plan_id` is supplied."
                )
            }
            reference_required <- morpher__recipe_requires_reference(private$recipe)
            reference_accepted <- morpher__recipe_accepts_reference(private$recipe)
            reference_supplied <- !is.null(reference_plan_id) || !is.null(reference_summary_id)
            reference_missing <- isTRUE(reference_required) &&
                is.null(reference_plan_id) && is.null(reference_summary_id)
            observed_required <-
                morpher__recipe_requires_observed_reference(private$recipe)
            observed_accepted <-
                morpher__recipe_accepts_observed_reference(private$recipe)
            observed_supplied <- !is.null(observed_plan_id) ||
                !is.null(observed_summary_id)
            observed_missing <- isTRUE(observed_required) &&
                !isTRUE(observed_supplied)

            morpher__bind_diagnostics(
                if (!is.null(plan_id)) private$preflight_extraction(plan_id, periods, strict = strict) else morpher__empty_diagnostics(),
                if (!is.null(reference_plan_id)) private$preflight_extraction(reference_plan_id, reference_periods, strict = strict) else morpher__empty_diagnostics(),
                if (!is.null(observed_plan_id)) private$preflight_extraction(observed_plan_id, observed_periods, strict = strict) else morpher__empty_diagnostics(),
                if (!is.null(summary_id)) private$preflight_summary(summary_id, by, strict = strict) else morpher__empty_diagnostics(),
                if (!is.null(reference_summary_id)) private$preflight_summary(reference_summary_id, morpher__reference_case_by(by), strict = strict) else morpher__empty_diagnostics(),
                if (!is.null(observed_summary_id)) private$preflight_summary(observed_summary_id, morpher__observed_case_by(by), strict = strict) else morpher__empty_diagnostics(),
                if (reference_missing) {
                    morpher__diagnostic(
                        stage = "reference",
                        # A required reference is structural input, so relaxed
                        # scientific diagnostics must never downgrade it.
                        severity = "error",
                        code = "missing_reference_climate",
                        message = "The selected morphing backend requires reference climate data.",
                        action = "Supply `reference_plan_id` and `reference_periods`, or use a backend that does not require reference climate."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                if (isTRUE(reference_supplied) && !isTRUE(reference_accepted)) {
                    morpher__diagnostic(
                        stage = "reference",
                        severity = "error",
                        code = "unexpected_reference_climate",
                        message = "The selected morphing backend does not accept reference climate data.",
                        action = "Remove the reference input or select a backend that accepts it."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                if (observed_missing) {
                    morpher__diagnostic(
                        stage = "observed_reference",
                        severity = "error",
                        code = "missing_observed_reference",
                        message = "The selected morphing backend requires observed daily weather.",
                        action = "Supply `observed_plan_id` and `observed_periods`."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                if (isTRUE(observed_supplied) &&
                    !isTRUE(observed_accepted)) {
                    morpher__diagnostic(
                        stage = "observed_reference",
                        severity = "error",
                        code = "unexpected_observed_reference",
                        message = "The selected morphing backend does not accept observed daily weather.",
                        action = "Remove the observed reference or select a backend that accepts it."
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
            climate <- morpher__resolve_calendar_columns(climate, month = TRUE)
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
            units_map <- stats::setNames(lapply(fields, morpher__default_epw_units), fields)
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
        #' @param observed_summary_id Optional observed-weather summary ID.
        #' @param baseline_id Baseline summary ID. If `NULL`, baseline summary is created.
        #' @param by Climate grouping columns.
        #' @param strict Whether missing required variables are blocking errors.
        #' @param overwrite Whether to replace an existing plan.
        plan = function(summary_id, reference_summary_id = NULL,
                        observed_summary_id = NULL, baseline_id = NULL,
                        by = c("source_id", "experiment_id", "variant_label", "period"),
                        strict = TRUE, overwrite = FALSE) {
            checkmate::assert_string(summary_id, min.chars = 1L)
            checkmate::assert_string(reference_summary_id, null.ok = TRUE)
            checkmate::assert_string(observed_summary_id, null.ok = TRUE)
            checkmate::assert_string(baseline_id, null.ok = TRUE)
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            checkmate::assert_flag(overwrite)

            preview <- self$preview_plan(
                summary_id = summary_id,
                reference_summary_id = reference_summary_id,
                observed_summary_id = observed_summary_id,
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
            morpher__delete_by_key(
                private$store,
                "epw_morph_observed_reference",
                "morph_id",
                morph_id
            )
            morpher__replace_rows(private$store, "epw_morph_plan", preview$plan, "morph_id")
            morpher__replace_rows(private$store, "epw_morph_factor", preview$factors, "factor_id")
            if (!is.null(observed_summary_id)) {
                observed_row <- data.table::data.table(
                    morph_id = morph_id,
                    observed_summary_id = observed_summary_id,
                    created_at = morpher__now()
                )
                morpher__replace_rows(
                    private$store,
                    "epw_morph_observed_reference",
                    observed_row,
                    "morph_id"
                )
            }
            data.table::as.data.table(preview$plan)
        },

        #' @description
        #' Preview a morphing plan and monthly factors without writing store state.
        #'
        #' @param summary_id Climate summary ID.
        #' @param reference_summary_id Optional reference climate summary ID for
        #'   change-factor backends.
        #' @param observed_summary_id Optional observed-weather summary ID.
        #' @param baseline_id Baseline summary ID. If `NULL`, baseline summary is created.
        #' @param by Climate grouping columns.
        #' @param strict Whether missing required variables are blocking errors.
        preview_plan = function(summary_id, reference_summary_id = NULL,
                                observed_summary_id = NULL,
                                baseline_id = NULL,
                                by = c("source_id", "experiment_id", "variant_label", "period"),
                                strict = TRUE) {
            checkmate::assert_string(summary_id, min.chars = 1L)
            checkmate::assert_string(reference_summary_id, null.ok = TRUE)
            checkmate::assert_string(observed_summary_id, null.ok = TRUE)
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
            if (!is.null(observed_summary_id)) {
                observed <- morpher__read_table(
                    private$store,
                    "epw_climate_summary"
                )
                target_observed_summary_id <- observed_summary_id
                observed <- observed[
                    observed[["summary_id"]] ==
                        target_observed_summary_id &
                        observed[["stat"]] == "mean"
                ]
                if (!nrow(observed)) {
                    cli::cli_abort(
                        "No observed climate summary rows were found for summary ID {.val {observed_summary_id}}."
                    )
                }
            }
            baseline <- morpher__read_table(private$store, "epw_baseline_summary")
            target_baseline_id <- baseline_id
            baseline <- baseline[baseline[["baseline_id"]] == target_baseline_id & baseline[["stat"]] == "mean"]
            if (!nrow(baseline)) {
                cli::cli_abort("No baseline summary rows were found for baseline ID {.val {baseline_id}}.")
            }

            morph_id <- private$morph_id(
                summary_id,
                reference_summary_id,
                observed_summary_id,
                baseline_id,
                by,
                strict
            )
            factors <- private$factor_rows(morph_id, climate, baseline, by, strict = strict, reference = reference)
            reference_required <- morpher__recipe_requires_reference(private$recipe)
            reference_missing <- isTRUE(reference_required) && is.null(reference_summary_id)
            reference_rejected <- !is.null(reference_summary_id) &&
                !isTRUE(morpher__recipe_accepts_reference(private$recipe))
            observed_required <-
                morpher__recipe_requires_observed_reference(private$recipe)
            observed_missing <- isTRUE(observed_required) &&
                is.null(observed_summary_id)
            observed_rejected <- !is.null(observed_summary_id) &&
                !isTRUE(
                    morpher__recipe_accepts_observed_reference(
                        private$recipe
                    )
                )
            diagnostics <- morpher__bind_diagnostics(
                private$preflight_summary(summary_id, by, strict = strict),
                if (!is.null(reference_summary_id)) {
                    private$preflight_summary(reference_summary_id, morpher__reference_case_by(by), strict = strict)
                } else {
                    morpher__empty_diagnostics()
                },
                if (!is.null(observed_summary_id)) {
                    private$preflight_summary(
                        observed_summary_id,
                        morpher__observed_case_by(by),
                        strict = strict
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                if (reference_missing) {
                    morpher__diagnostic(
                        stage = "reference",
                        # Missing required reference data is a structural
                        # error, independent of scientific strictness.
                        severity = "error",
                        code = "missing_reference_climate",
                        message = "The selected morphing backend requires reference climate data.",
                        morph_id = morph_id,
                        action = "Supply `reference_summary_id`, or use a backend that does not require reference climate."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                if (reference_rejected) {
                    morpher__diagnostic(
                        stage = "reference",
                        severity = "error",
                        code = "unexpected_reference_climate",
                        message = "The selected morphing backend does not accept reference climate data.",
                        morph_id = morph_id,
                        action = "Remove `reference_summary_id` or select a backend that accepts it."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                if (observed_missing) {
                    morpher__diagnostic(
                        stage = "observed_reference",
                        severity = "error",
                        code = "missing_observed_reference",
                        message = "The selected morphing backend requires observed daily weather.",
                        morph_id = morph_id,
                        action = "Supply `observed_summary_id`."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                if (observed_rejected) {
                    morpher__diagnostic(
                        stage = "observed_reference",
                        severity = "error",
                        code = "unexpected_observed_reference",
                        message = "The selected morphing backend does not accept observed daily weather.",
                        morph_id = morph_id,
                        action = "Remove `observed_summary_id` or select a backend that accepts it."
                    )
                } else {
                    morpher__empty_diagnostics()
                },
                private$preflight_baseline(baseline_id, strict = strict),
                private$factor_diagnostics(factors, strict = strict, morph_id = morph_id)
            )
            structural_reference_error <- isTRUE(reference_missing) ||
                isTRUE(reference_rejected) ||
                isTRUE(observed_missing) ||
                isTRUE(observed_rejected)
            status <- if (structural_reference_error ||
                (any(diagnostics$severity == "error") && isTRUE(strict))) "blocked" else "planned"
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
        #' Execute a morphing plan and write one hourly Parquet file per result
        #' member and weather year.
        #'
        #' @param morph_id Morphing plan ID.
        #' @param overwrite Whether to overwrite existing result files.
        #' @param resume Whether to reuse complete existing results.
        #' @param reporter Optional workflow reporter used by task-level runs.
        run = function(morph_id, overwrite = FALSE, resume = TRUE,
                       reporter = NULL) {
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

            existing <- morpher__normalize_result_manifest(
                morpher__read_table(private$store, "epw_morph_result")
            )
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
            complete_cases <- cases[vapply(cases, function(case_id) {
                rows <- complete_existing[
                    complete_existing[["case_id"]] == case_id
                ]
                morpher__result_case_complete(rows)
            }, logical(1L))]
            if (!isTRUE(overwrite) && isTRUE(resume) &&
                length(complete_cases) == length(cases)) {
                private$set_plan_status(morph_id, "result_done")
                if (!is.null(reporter)) {
                    for (case_index in seq_along(cases)) {
                        case <- plan_cases[case_index]
                        label <- private$report_case_label(case)
                        reporter$unit_started(label, current = case_index, total = length(cases),
                            details = private$report_case_details(case, "morph_case"))
                        reporter$unit_skipped(sprintf("Reused %s", label),
                            current = case_index, total = length(cases))
                    }
                }
                return(morpher__order_result_rows(complete_existing, cases))
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
                    observed_rows <- morpher__read_table(
                        private$store,
                        "epw_morph_observed_reference"
                    )
                    observed_rows <- observed_rows[
                        observed_rows[["morph_id"]] == morph_id
                    ]
                    observed_summary_id <- if (nrow(observed_rows)) {
                        store__chr1(observed_rows$observed_summary_id)
                    } else {
                        NULL
                    }
                    if (!is.null(observed_summary_id) &&
                        (is.na(observed_summary_id) ||
                            !nzchar(observed_summary_id))) {
                        observed_summary_id <- NULL
                    }
                    if (isTRUE(
                        morpher__recipe_requires_observed_reference(
                            private$recipe
                        )
                    ) && is.null(observed_summary_id)) {
                        cli::cli_abort(
                            "Backend {.val {private$recipe$backend}} requires observed daily weather."
                        )
                    }
                    observed_climate <- if (is.null(observed_summary_id)) {
                        NULL
                    } else {
                        private$engine_climate_data(observed_summary_id)
                    }
                    by <- private$plan_by(plan)
                    reference_by <- morpher__reference_case_by(by)
                    observed_by <- morpher__observed_case_by(by)
                    result_rows <- list()
                    for (case_index in seq_along(cases)) {
                        if (!is.null(reporter)) {
                            reporter$check_cancel("morph")
                        }
                        case_id <- cases[[case_index]]
                        case <- plan_cases[case_index]
                        label <- private$report_case_label(case)
                        if (!is.null(reporter)) {
                            reporter$unit_started(label, current = case_index, total = length(cases),
                                details = private$report_case_details(case, "morph_case"))
                        }
                        target_case_id <- case_id
                        existing_case <- complete_existing[complete_existing[["case_id"]] == target_case_id]
                        if (!isTRUE(overwrite) && isTRUE(resume) &&
                            morpher__result_case_complete(existing_case)) {
                            result_rows[[length(result_rows) + 1L]] <- existing_case
                            if (!is.null(reporter)) {
                                reporter$unit_skipped(sprintf("Reused %s", label),
                                    current = case_index, total = length(cases))
                            }
                            next
                        }
                        case_climate <- private$filter_case_climate(climate, case, by)
                        if (!nrow(case_climate)) {
                            cli::cli_abort("No extracted climate rows matched morphing case {.val {target_case_id}}.")
                        }
                        reference_case_climate <- NULL
                        if (!is.null(reference_climate)) {
                            reference_case_climate <- private$filter_case_climate(reference_climate, case, reference_by)
                            # Supplying an external reference selects
                            # change-factor mode for every case; never fall
                            # back to the baseline EPW for an unmatched case.
                            if (!nrow(reference_case_climate)) {
                                cli::cli_abort("No reference climate rows matched morphing case {.val {target_case_id}}.")
                            }
                        }
                        observed_case_climate <- NULL
                        if (!is.null(observed_climate)) {
                            observed_case_climate <- private$filter_case_climate(
                                observed_climate,
                                case,
                                observed_by
                            )
                            # Observations are shared across future-model cases,
                            # but an explicit site key must still match.
                            if (!nrow(observed_case_climate)) {
                                cli::cli_abort(
                                    "No observed climate rows matched morphing case {.val {target_case_id}}."
                                )
                            }
                        }
                        context <- morpher__context(
                            epw = private$epw,
                            climate = case_climate,
                            reference_climate = reference_case_climate,
                            observed_reference = observed_case_climate,
                            recipe = private$recipe,
                            by = by,
                            case = case,
                            strict = isTRUE(plan$strict[[1L]]),
                            warning = FALSE
                        )
                        case_result <- morpher__run_context(context)
                        member_records <- sequence__records(case_result)
                        member_count <- length(member_records)
                        for (member in member_records) {
                            case_data <- member$data
                            if (!nrow(case_data)) {
                                cli::cli_abort(
                                    "No morphed data were produced for morphing case {.val {target_case_id}}."
                                )
                            }
                            path <- private$morph_result_path(
                                morph_id,
                                case_id,
                                output_type = member$output_type,
                                sequence_id = member$sequence_id,
                                weather_year = member$weather_year
                            )
                            path_rel <- store_rel_path(
                                path,
                                root = private$store$path
                            )
                            existing_member <- existing[
                                existing[["case_id"]] == case_id &
                                    existing[["output_path"]] == path_rel
                            ]
                            if (!isTRUE(overwrite) && isTRUE(resume) &&
                                nrow(existing_member) == 1L &&
                                file.exists(path)) {
                                # Partial sequence recovery reuses intact
                                # siblings and regenerates only missing years.
                                result_rows[[length(result_rows) + 1L]] <-
                                    existing_member
                                next
                            }
                            if (file.exists(path) && !isTRUE(overwrite)) {
                                cli::cli_abort(
                                    "Morph result already exists without a complete manifest row: {.path {path}}."
                                )
                            }
                            case_meta <- private$case_metadata_from_case(
                                case,
                                case_data
                            )
                            for (name in names(case_meta)) {
                                case_data[, (name) := case_meta[[name]]]
                            }
                            write_parquet_file(case_data, path)
                            provenance_json <- as.character(morpher__json(
                                member$provenance
                            ))
                            identity <- list(
                                output_type = member$output_type,
                                sequence_id = member$sequence_id,
                                weather_year = member$weather_year,
                                calendar = member$calendar,
                                stochastic_seed = member$stochastic_seed,
                                member_count = member_count,
                                provenance_json = provenance_json
                            )
                            artifact_id <- private$store$register_artifact(
                                kind = "output",
                                path = path,
                                role = "derived",
                                project = "CMIP6",
                                metadata = c(
                                    list(
                                        morph_id = morph_id,
                                        case_id = case_id
                                    ),
                                    identity
                                )
                            )
                            result_id <- if (identical(
                                member$output_type,
                                "representative_year"
                            )) {
                                morpher__hash(morph_id, case_id, path)
                            } else {
                                morpher__hash(
                                    morph_id,
                                    case_id,
                                    member$output_type,
                                    member$sequence_id,
                                    member$weather_year,
                                    path
                                )
                            }
                            result_rows[[length(result_rows) + 1L]] <- data.frame(
                                result_id = result_id,
                                morph_id = morph_id,
                                case_id = case_id,
                                artifact_id = artifact_id,
                                output_path = path_rel,
                                row_count = nrow(case_data),
                                output_type = member$output_type,
                                sequence_id = member$sequence_id,
                                weather_year = member$weather_year,
                                calendar = member$calendar,
                                stochastic_seed = member$stochastic_seed,
                                member_count = member_count,
                                provenance_json = provenance_json,
                                created_at = morpher__now(),
                                stringsAsFactors = FALSE
                            )
                        }
                        if (!is.null(reporter)) {
                            reporter$unit_completed(sprintf("Morphed %s", label),
                                current = case_index, total = length(cases), outcome = "completed")
                        }
                    }
                    results <- data.table::rbindlist(result_rows, use.names = TRUE, fill = TRUE)
                    morpher__delete_by_key(private$store, "epw_morph_result", "morph_id", morph_id)
                    morpher__replace_rows(private$store, "epw_morph_result", results, "result_id")
                    private$set_plan_status(morph_id, "result_done")
                    morpher__order_result_rows(results, cases)
                },
                error = function(e) {
                    private$set_plan_status(morph_id, "failed", conditionMessage(e))
                    stop(e)
                }
            )
        },

        #' @description
        #' Write one future EPW file for every persisted result member.
        #'
        #' @param morph_id Morphing plan ID.
        #' @param dir Output directory inside the store root. Relative paths are
        #'        resolved under the store root. If `NULL`, the workflow stops
        #'        after writing morph result Parquet files and does not write EPW
        #'        outputs.
        #' @param separate Whether to create case subdirectories.
        #' @param overwrite Whether to overwrite existing EPW files.
        #' @param resume Whether to reuse complete existing EPW outputs.
        #' @param reporter Optional workflow reporter used by task-level runs.
        write_epw = function(morph_id, dir, separate = TRUE, overwrite = FALSE,
                             resume = TRUE, reporter = NULL) {
            checkmate::assert_string(morph_id, min.chars = 1L)
            checkmate::assert_string(dir, min.chars = 1L)
            checkmate::assert_flag(separate)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            plan <- private$get_plan(morph_id)
            results <- morpher__normalize_result_manifest(
                morpher__read_table(private$store, "epw_morph_result")
            )
            target_morph_id <- morph_id
            results <- results[results[["morph_id"]] == target_morph_id]
            if (!nrow(results)) {
                cli::cli_abort("No morphing results were found. Run {.code EpwMorpher$run()} first.")
            }
            results <- morpher__order_result_rows(
                results,
                private$case_rows(plan)$case_id
            )

            tryCatch(
                {
                    root <- morpher__epw_output_root(dir, private$store$path)
                    dir.create(root, recursive = TRUE, showWarnings = FALSE)
                    base_epw <- private$epw$clone()
                    suppressMessages(base_epw$drop_unit())
                    base_cols <- names(base_epw$data())
                    current_outputs <- morpher__normalize_result_manifest(
                        morpher__read_table(private$store, "epw_output")
                    )
                    target_morph_id <- morph_id
                    current_outputs <- current_outputs[current_outputs[["morph_id"]] == target_morph_id]
                    output_rows <- vector("list", nrow(results))
                    for (i in seq_len(nrow(results))) {
                        if (!is.null(reporter)) {
                            reporter$check_cancel("write_epw")
                        }
                        result <- results[i]
                        result_path <- store_abs_path(result$output_path[[1L]], root = private$store$path)
                        dt <- morpher__parquet_read(private$store, result_path)
                        meta <- private$case_metadata_from_result(dt)
                        sequence_meta <- if (identical(
                            result$output_type[[1L]],
                            "representative_year"
                        )) {
                            list()
                        } else {
                            list(
                                sequence_id = result$sequence_id[[1L]],
                                weather_year = result$weather_year[[1L]]
                            )
                        }
                        display_meta <- c(
                            meta[c(
                                "experiment_id",
                                "variant_label",
                                "period"
                            )],
                            sequence_meta
                        )
                        label <- paste(
                            unlist(display_meta, use.names = FALSE),
                            collapse = " | "
                        )
                        if (!is.null(reporter)) {
                            reporter$unit_started(label, current = i, total = nrow(results),
                                details = list(
                                    unit_type = "epw_case",
                                    scenario = meta$experiment_id,
                                    period = meta$period
                                ))
                        }
                        path_meta <- c(meta, sequence_meta)
                        path_values <- unlist(path_meta, use.names = FALSE)
                        label <- paste(
                            morpher__safe_path(path_values),
                            collapse = "."
                        )
                        filename <- paste(
                            tools::file_path_sans_ext(basename(
                                morpher__get_epw_path(private$epw)
                            )),
                            label,
                            "epw",
                            sep = "."
                        )
                        output_path <- if (isTRUE(separate)) {
                            file.path(
                                root,
                                do.call(
                                    file.path,
                                    as.list(morpher__safe_path(path_values))
                                ),
                                filename
                            )
                        } else {
                            file.path(root, filename)
                        }
                        output_rel <- store_rel_path(output_path, root = private$store$path)
                        same_result <- current_outputs[["result_id"]] ==
                            result$result_id[[1L]]
                        same_result[is.na(same_result)] <-
                            current_outputs[["case_id"]][is.na(same_result)] ==
                                result$case_id[[1L]]
                        existing_output <- current_outputs[
                            same_result &
                                current_outputs[["path"]] == output_rel
                        ]
                        if (!isTRUE(overwrite) && isTRUE(resume) && nrow(existing_output) && file.exists(output_path)) {
                            output_rows[[i]] <- existing_output[1L]
                            if (!is.null(reporter)) {
                                reporter$unit_skipped(sprintf("Reused EPW %s", label),
                                    current = i, total = nrow(results))
                            }
                            next
                        }
                        if (file.exists(output_path) && !isTRUE(overwrite)) {
                            cli::cli_abort("EPW output already exists without a complete manifest row: {.path {output_path}}.")
                        }
                        new_epw <- private$epw$clone()
                        set_data <- data.table::copy(dt[, intersect(base_cols, names(dt)), with = FALSE])
                        if (nrow(set_data) != nrow(base_epw$data())) {
                            cli::cli_abort(c(
                                "Future-weather member cannot be written with the selected EPW template.",
                                "x" = "Member {.val {result$sequence_id[[1L]]}} year {.val {result$weather_year[[1L]]}} has {nrow(set_data)} rows; the template has {nrow(base_epw$data())}.",
                                "i" = "Map the member to the template calendar before the output stage."
                            ))
                        }
                        data.table::setcolorder(set_data, base_cols)
                        suppressMessages(new_epw$drop_unit())
                        suppressMessages(new_epw$set(set_data))
                        # Header policies are evaluated from the persisted
                        # hourly result just read above, keeping fresh and
                        # resumed EPW writes scientifically identical.
                        epw_file__apply_morph_headers(
                            new_epw, set_data, private$recipe$options
                        )
                        case_label <- paste(
                            unlist(path_meta, use.names = FALSE),
                            collapse = "-"
                        )
                        new_epw$comment1(disclaimer_comment(case_label))
                        new_epw$fill_abnormal(missing = TRUE, out_of_range = TRUE, special = TRUE)
                        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
                        new_epw$save(output_path, overwrite = overwrite)
                        artifact_id <- private$store$register_artifact(
                            kind = "output",
                            path = output_path,
                            role = "output",
                            project = "CMIP6",
                            metadata = c(
                                list(
                                    morph_id = morph_id,
                                    case_id = result$case_id[[1L]],
                                    result_id = result$result_id[[1L]],
                                    output_type = result$output_type[[1L]],
                                    sequence_id = result$sequence_id[[1L]],
                                    weather_year = result$weather_year[[1L]],
                                    calendar = result$calendar[[1L]],
                                    stochastic_seed = result$stochastic_seed[[1L]],
                                    member_count = result$member_count[[1L]],
                                    provenance_json = result$provenance_json[[1L]]
                                ),
                                meta
                            )
                        )
                        output_id <- if (identical(
                            result$output_type[[1L]],
                            "representative_year"
                        )) {
                            morpher__hash(
                                morph_id,
                                result$case_id[[1L]],
                                output_path
                            )
                        } else {
                            morpher__hash(
                                morph_id,
                                result$result_id[[1L]],
                                output_path
                            )
                        }
                        output_rows[[i]] <- data.frame(
                            output_id = output_id,
                            morph_id = morph_id,
                            case_id = result$case_id[[1L]],
                            result_id = result$result_id[[1L]],
                            artifact_id = artifact_id,
                            path = output_rel,
                            source_id = store__chr1(meta$source_id),
                            experiment_id = store__chr1(meta$experiment_id),
                            variant_label = store__chr1(meta$variant_label),
                            period = store__chr1(meta$period),
                            output_type = result$output_type[[1L]],
                            sequence_id = result$sequence_id[[1L]],
                            weather_year = result$weather_year[[1L]],
                            calendar = result$calendar[[1L]],
                            stochastic_seed = result$stochastic_seed[[1L]],
                            member_count = result$member_count[[1L]],
                            provenance_json = result$provenance_json[[1L]],
                            created_at = morpher__now(),
                            stringsAsFactors = FALSE
                        )
                        if (!is.null(reporter)) {
                            reporter$unit_completed(sprintf("Wrote EPW %s", label),
                                current = i, total = nrow(results), outcome = "completed")
                        }
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
        #' @param observed_plan_id Optional observed-weather extraction plan
        #'   IDs.
        #' @param observed_periods Optional observed-weather period table.
        #' @param by Climate grouping columns.
        #' @param strict Whether blocking diagnostics should abort the workflow.
        #' @param dir Output directory. Relative paths are resolved under the store root.
        #' @param separate Whether to create case subdirectories.
        #' @param overwrite Whether to overwrite existing plan, result, and EPW outputs.
        #' @param resume Whether to reuse complete existing result and EPW outputs.
        #' @param reporter Optional workflow reporter used by task-level runs.
        workflow = function(plan_id, periods, reference_plan_id = NULL,
                            reference_periods = NULL,
                            observed_plan_id = NULL,
                            observed_periods = NULL,
                            by = c("source_id", "experiment_id", "variant_label", "period"),
                            strict = TRUE, dir = "outputs/future-epw", separate = TRUE,
                            overwrite = FALSE, resume = TRUE, reporter = NULL) {
            checkmate::assert_character(plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_character(reference_plan_id, any.missing = FALSE, min.len = 1L, unique = TRUE, null.ok = TRUE)
            checkmate::assert_character(
                observed_plan_id,
                any.missing = FALSE,
                min.len = 1L,
                unique = TRUE,
                null.ok = TRUE
            )
            checkmate::assert_data_frame(periods)
            checkmate::assert_names(names(periods), must.include = c("period", "year"))
            if (!is.null(reference_periods)) {
                checkmate::assert_data_frame(reference_periods)
                checkmate::assert_names(names(reference_periods), must.include = c("period", "year"))
            }
            if (!is.null(observed_periods)) {
                checkmate::assert_data_frame(observed_periods)
                checkmate::assert_names(
                    names(observed_periods),
                    must.include = c("period", "year")
                )
            }
            checkmate::assert_character(by, any.missing = FALSE, min.len = 1L, unique = TRUE)
            checkmate::assert_subset(by, c("site_id", "source_id", "experiment_id", "variant_label", "frequency", "table_id", "period"))
            checkmate::assert_flag(strict)
            checkmate::assert_string(dir, min.chars = 1L, null.ok = TRUE)
            checkmate::assert_flag(separate)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)

            # Stop before summaries or store writes when reference structure is
            # incompatible with the selected backend.
            if (isTRUE(morpher__recipe_requires_reference(private$recipe)) && is.null(reference_plan_id)) {
                cli::cli_abort(c(
                    "The selected morphing backend requires explicit reference climate data.",
                    "i" = "Supply `reference_plan_id` and `reference_periods`."
                ))
            }
            if (!is.null(reference_plan_id) && !isTRUE(morpher__recipe_accepts_reference(private$recipe))) {
                cli::cli_abort("The selected morphing backend does not accept reference climate data.")
            }
            if (isTRUE(
                morpher__recipe_requires_observed_reference(private$recipe)
            ) && is.null(observed_plan_id)) {
                cli::cli_abort(c(
                    "The selected morphing backend requires explicit observed daily weather.",
                    "i" = "Supply `observed_plan_id` and `observed_periods`."
                ))
            }
            if (!is.null(observed_plan_id) &&
                !isTRUE(
                    morpher__recipe_accepts_observed_reference(
                        private$recipe
                    )
                )) {
                cli::cli_abort(
                    "The selected morphing backend does not accept observed daily weather."
                )
            }

            preflight <- self$preflight(
                plan_id = plan_id,
                periods = periods,
                reference_plan_id = reference_plan_id,
                reference_periods = reference_periods,
                observed_plan_id = observed_plan_id,
                observed_periods = observed_periods,
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
            observed_climate <- if (is.null(observed_plan_id)) {
                NULL
            } else {
                self$summarise_climate(
                    plan_id = observed_plan_id,
                    periods = observed_periods,
                    strict = strict,
                    overwrite = overwrite
                )
            }
            observed_summary_id <- if (is.null(observed_climate)) {
                NULL
            } else {
                unique(observed_climate$summary_id)[[1L]]
            }
            baseline <- self$summarise_baseline(overwrite = overwrite)
            preview <- self$preview_plan(
                summary_id = unique(climate$summary_id)[[1L]],
                reference_summary_id = reference_summary_id,
                observed_summary_id = observed_summary_id,
                baseline_id = unique(baseline$baseline_id)[[1L]],
                by = by,
                strict = strict
            )
            plan <- self$plan(
                summary_id = unique(climate$summary_id)[[1L]],
                reference_summary_id = reference_summary_id,
                observed_summary_id = observed_summary_id,
                baseline_id = unique(baseline$baseline_id)[[1L]],
                by = by,
                strict = strict,
                overwrite = overwrite
            )
            diagnostics <- morpher__bind_diagnostics(preflight, preview$diagnostics)
            if (isTRUE(strict)) {
                self$check(plan$morph_id[[1L]])
            }
            results <- self$run(plan$morph_id[[1L]], overwrite = overwrite,
                resume = resume, reporter = reporter)
            outputs <- if (is.null(dir)) {
                NULL
            } else {
                self$write_epw(plan$morph_id[[1L]], dir = dir, separate = separate,
                    overwrite = overwrite, resume = resume, reporter = reporter)
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

        # Build one stable user-facing label from the scientific case identity,
        # avoiding opaque internal case hashes in progress displays.
        report_case_label = function(case) {
            fields <- intersect(c("experiment_id", "variant_label", "period"), names(case))
            values <- unlist(case[, ..fields], use.names = FALSE)
            values <- values[!is.na(values) & nzchar(as.character(values))]
            paste(as.character(values), collapse = " | ")
        },

        # Preserve structured case identity alongside the concise label so CLI
        # watch can render progress without reparsing human-readable messages.
        report_case_details = function(case, unit_type) {
            list(
                unit_type = unit_type,
                scenario = if ("experiment_id" %in% names(case)) case$experiment_id[[1L]] else NULL,
                period = if ("period" %in% names(case)) case$period[[1L]] else NULL
            )
        },

        register_epw = function(epw) {
            if (inherits(epw, "EpwFile") || epw_file_is_external(epw)) {
                # Normalize every object input at the engine boundary; external
                # Epw implementations never leak into morphing internals.
                epw_obj <- epw_file_coerce(epw)
                epw_path <- morpher__get_epw_path(epw_obj)
            } else {
                checkmate::assert_string(epw, min.chars = 1L)
                checkmate::assert_file_exists(epw)
                epw_path <- epw
                epw_obj <- epw_file_read(epw_path)
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
            private$epw <- epw_file_read(target)
            invisible(NULL)
        },

        baseline_id = function() {
            morpher__hash("baseline", private$epw_id, morpher__json(private$recipe))
        },

        summary_id = function(plan_id, periods) {
            morpher__hash("summary", private$epw_id, paste(sort(plan_id), collapse = "\r"), morpher__json(periods))
        },

        morph_id = function(summary_id, reference_summary_id,
                            observed_summary_id, baseline_id, by, strict) {
            strict_token <- if (isTRUE(strict)) "strict=true" else "strict=false"
            pieces <- list(
                "morph", private$epw_id, summary_id, store__chr1(reference_summary_id),
                baseline_id, paste(by, collapse = "\r"),
                morpher__json(private$recipe),
                strict_token
            )
            # Preserve existing morph IDs for every recipe without observations;
            # append a tagged role only when the new fourth input is present.
            if (!is.null(observed_summary_id)) {
                pieces <- c(
                    pieces,
                    list(paste0(
                        "observed_reference=",
                        observed_summary_id
                    ))
                )
            }
            do.call(morpher__hash, pieces)
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
            climate <- morpher__resolve_calendar_columns(climate)
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
            present_variables <- unique(coverage$variable_id)
            diagnostics[[length(diagnostics) + 1L]] <-
                morpher__frequency_diagnostic(
                    private$recipe,
                    if ("frequency" %in% names(coverage)) {
                        coverage$frequency
                    } else {
                        character()
                    },
                    stage = "extraction",
                    plan_id = paste(plan_id, collapse = ", ")
                )
            missing_variables <- setdiff(self$required_variables(), present_variables)
            for (variable_id in missing_variables) {
                guidance <- morpher__missing_variable_guidance(variable_id, present_variables)
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "extraction",
                    severity = severity,
                    code = "missing_required_variable",
                    message = sprintf(
                        "Required CMIP variable %s is missing from selected extraction plans.%s",
                        variable_id,
                        guidance$suffix
                    ),
                    variable_id = variable_id,
                    action = guidance$action
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
            climate <- morpher__resolve_calendar_columns(climate, month = TRUE)
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
            diagnostics[[length(diagnostics) + 1L]] <-
                morpher__frequency_diagnostic(
                    private$recipe,
                    if ("frequency" %in% names(climate)) {
                        climate$frequency
                    } else {
                        character()
                    },
                    stage = "climate_summary",
                    summary_id = summary_id
                )
            present_variables <- unique(climate$variable_id)
            missing_variables <- setdiff(self$required_variables(), present_variables)
            for (variable_id in missing_variables) {
                guidance <- morpher__missing_variable_guidance(variable_id, present_variables)
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "climate_summary",
                    severity = severity,
                    code = "missing_required_variable",
                    message = sprintf(
                        "Required CMIP variable %s is missing from climate summary.%s",
                        variable_id,
                        guidance$suffix
                    ),
                    summary_id = summary_id,
                    variable_id = variable_id,
                    action = guidance$action
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
            reference_by <- morpher__reference_case_by(by)
            if (is.null(reference)) {
                reference <- data.table::data.table()
            }
            # A supplied reference summary selects CMIP6 change-factor mode;
            # otherwise each EPW monthly baseline value is the comparison.
            external_reference <- nrow(reference) > 0L
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
                        } else if (isTRUE(external_reference) && !nrow(ref)) {
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
                            if (identical(status, "ok") && isTRUE(external_reference)) {
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
                            if (identical(status, "ok") && isTRUE(external_reference) &&
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
                        if (identical(status, "ok") && isTRUE(external_reference) && !isTRUE(is_precip)) {
                            converted <- morpher__convert_value_checked(reference_value, reference_units, base_units)
                            reference_value <- converted$value
                            if (!isTRUE(converted$ok)) {
                                status <- "unit_conversion_failed"
                            }
                        }
                        # Persist the effective reference even in EPW-baseline
                        # mode so factors remain inspectable and reproducible.
                        if (!isTRUE(external_reference)) {
                            reference_value <- base_value
                        }
                        comparison_value <- reference_value
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

        # Keep legacy representative-year paths stable while sequence results
        # receive collision-free member and year partitions.
        morph_result_path = function(
            morph_id,
            case_id,
            output_type = "representative_year",
            sequence_id = NA_character_,
            weather_year = NA_integer_
        ) {
            root <- file.path(
                private$store$path,
                "outputs",
                "epw-morph",
                morph_id
            )
            if (identical(output_type, "representative_year")) {
                return(file.path(
                    root,
                    sprintf(
                        "case=%s.parquet",
                        morpher__safe_path(case_id)
                    )
                ))
            }
            file.path(
                root,
                sprintf("case=%s", morpher__safe_path(case_id)),
                sprintf("sequence=%s", morpher__safe_path(sequence_id)),
                sprintf("year=%d.parquet", as.integer(weather_year))
            )
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
