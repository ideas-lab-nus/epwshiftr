#' @include store.R weather-transform.R epw-morph-context.R backend-original-morphing.R utils.R
NULL

# Store-native EPW morpher
#' Create an EPW morpher
#'
#' @param store An [EsgStore] object.
#' @param epw Baseline EPW path, internal `EpwFile`, or an external object
#'   inheriting from `"Epw"`.
#' @param site_id Optional site identifier.
#' @param transform A reusable weather transformation created by
#'   [monthly_transform()], [daily_transform()], or [hourly_transform()].
#' @param label Optional source label.
#'
#' @return An [EpwMorpher] object.
#' @export
# epw_morpher {{{
epw_morpher <- function(
    store,
    epw,
    site_id = NULL,
    transform = monthly_transform("original_morphing"),
    label = NULL
) {
    EpwMorpher$new(
        store = store,
        epw = epw,
        site_id = site_id,
        transform = transform,
        label = label
    )
}
# }}}

# morpher__now {{{
morpher__now <- function() {
    as.POSIXct(Sys.time(), tz = "UTC")
}
# }}}

# morpher__json {{{
morpher__json <- function(x) {
    if (inherits(x, "epw_morph_recipe")) {
        rules <- data.table::copy(x$rules)
        for (col in intersect(
            c("required_variables", "optional_variables", "method_choices"),
            names(rules)
        )) {
            rules[[col]] <- vapply(
                rules[[col]],
                paste,
                character(1L),
                collapse = ","
            )
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
# }}}

# Resolve an EPW output directory and fail before any filesystem writes when
# the manifest cannot safely store the path relative to the EsgStore root.
# morpher__epw_output_root {{{
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
# }}}

# morpher__hash {{{
morpher__hash <- function(...) {
    store__hash(...)
}
# }}}

# morpher__hash_rows {{{
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
        cli::cli_abort(
            "Cannot recycle morphing hash input of length {length(x)} to {n}."
        )
    })
    vapply(
        seq_len(n),
        function(i) {
            do.call(morpher__hash, lapply(args, `[[`, i))
        },
        character(1L)
    )
}
# }}}

# morpher__private_store {{{
morpher__private_store <- function(store) {
    if (!inherits(store, "EsgStore")) {
        cli::cli_abort("`store` must be an {.cls EsgStore} object.")
    }
    private <- priv(store)
    private$check_open()
    private
}
# }}}

# morpher__replace_rows {{{
morpher__replace_rows <- function(store, table, rows, key) {
    morpher__private_store(store)$replace_rows(table, as.data.frame(rows), key)
    invisible(rows)
}
# }}}

# morpher__read_table {{{
morpher__read_table <- function(store, table) {
    morpher__private_store(store)$read_table(table)
}
# }}}

# Recover every extraction plan contributing to a climate summary. Current
# summaries use the normalized lineage table, while legacy summaries retain
# their source plan IDs directly on each statistic row.
# morpher__summary_plan_ids {{{
morpher__summary_plan_ids <- function(store, summary) {
    summary <- data.table::as.data.table(data.table::copy(summary))
    if (!nrow(summary) || !"summary_id" %in% names(summary)) {
        return(character())
    }

    summary_ids <- unique(as.character(summary$summary_id))
    summary_ids <- summary_ids[!is.na(summary_ids) & nzchar(summary_ids)]
    lineage <- morpher__read_table(store, "epw_climate_summary_plan")
    if (nrow(lineage) && length(summary_ids)) {
        plan_ids <- unique(as.character(
            lineage[lineage$summary_id %in% summary_ids][["plan_id"]]
        ))
        plan_ids <- plan_ids[!is.na(plan_ids) & nzchar(plan_ids)]
        if (length(plan_ids)) {
            return(sort(plan_ids))
        }
    }

    if (!"plan_id" %in% names(summary)) {
        return(character())
    }
    plan_ids <- unique(as.character(summary$plan_id))
    sort(plan_ids[!is.na(plan_ids) & nzchar(plan_ids)])
}
# }}}

# morpher__delete_by_key {{{
morpher__delete_by_key <- function(store, table, key, values) {
    morpher__private_store(store)$delete_by_key(table, key, values)
    invisible(NULL)
}
# }}}

# Delete current diagnostics for exactly one morphing case. A case hash can be
# shared by different method plans, so filtering by both identities prevents a
# retry in one method from erasing another method's evidence.
# morpher__delete_case_diagnostics {{{
morpher__delete_case_diagnostics <- function(store, morph_id, case_id) {
    diagnostics <- morpher__read_table(store, "epw_morph_diagnostic")
    # Resolve scalar identifiers outside data.table's column lookup so other
    # plans and cases keep their diagnostic evidence when this case is retried.
    selected <- which(
        diagnostics[["morph_id"]] == morph_id &
            diagnostics[["case_id"]] == case_id
    )
    diagnostics <- diagnostics[selected]
    if (nrow(diagnostics)) {
        morpher__delete_by_key(
            store,
            "epw_morph_diagnostic",
            "diagnostic_id",
            diagnostics[["diagnostic_id"]]
        )
    }
    invisible(NULL)
}
# }}}

# morpher__safe_path {{{
morpher__safe_path <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "unknown"
    gsub("[^A-Za-z0-9_.=-]+", "-", x)
}
# }}}

# morpher__parquet_read {{{
morpher__parquet_read <- function(store, path) {
    conn <- morpher__private_store(store)$conn
    data.table::as.data.table(ddb_query(
        conn,
        sprintf(
            "SELECT * FROM read_parquet(%s)",
            ddb_literal(conn, path)
        )
    ))
}
# }}}

# morpher__monthly_long {{{
morpher__monthly_long <- function(data, id_cols, value_cols, units_map) {
    rows <- list()
    for (field in value_cols) {
        if (!field %in% names(data)) {
            next
        }
        value <- morpher__drop_units(data[[field]])
        units <- units_map[[field]]
        dt <- data.table::data.table(month = data$month, value = value)
        summary <- dt[,
            {
                valid <- value[is.finite(value)]
                if (!length(valid)) {
                    list(mean = NA_real_, max = NA_real_, min = NA_real_)
                } else {
                    list(
                        mean = mean(valid),
                        max = max(valid),
                        min = min(valid)
                    )
                }
            },
            by = "month"
        ]
        # An all-missing month has no baseline statistic. Omitting it lets the
        # existing baseline coverage diagnostics identify the exact gap.
        summary <- summary[is.finite(mean) & is.finite(max) & is.finite(min)]
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
    data.table::setcolorder(
        out,
        c("epw_field", "month", "stat", "value", "units")
    )
    out
}
# }}}

# Recover one pooled mean from legacy fragment-level summary rows. New
# summaries aggregate raw records first and therefore normally contain one
# monthly mean row; this helper exists for compatible reading of old stores.
# morpher__pooled_mean {{{
morpher__pooled_mean <- function(rows) {
    if (is.null(rows) || !nrow(rows) || !"value" %in% names(rows)) {
        return(NA_real_)
    }
    if ("stat" %in% names(rows)) {
        stats <- unique(as.character(rows$stat))
        stats <- stats[!is.na(stats) & nzchar(stats)]
        if (length(stats) && !all(stats == "mean")) {
            cli::cli_abort("Only monthly mean summary rows can be pooled.")
        }
    }
    if ("units" %in% names(rows)) {
        units <- unique(as.character(rows$units))
        units <- units[!is.na(units) & nzchar(units)]
        if (length(units) > 1L) {
            cli::cli_abort(
                "Cannot pool climate summary rows with different units."
            )
        }
    }

    values <- as.numeric(rows$value)
    valid <- is.finite(values)
    if (!any(valid)) {
        return(NA_real_)
    }
    values <- values[valid]
    # Valid-value counts are the exact weights for fragment means. Total row
    # counts are accepted only for stores created before `n_valid` existed.
    for (weight_column in c("n_valid", "n_records")) {
        if (!weight_column %in% names(rows)) {
            next
        }
        weights <- as.numeric(rows[[weight_column]])[valid]
        if (
            length(weights) == length(values) &&
                all(is.finite(weights) & weights > 0)
        ) {
            return(stats::weighted.mean(values, weights))
        }
    }
    mean(values)
}
# }}}

# Calculate one statistic from finite values and preserve an empty group as a
# missing value instead of allowing `min()` or `max()` to emit infinities.
# morpher__finite_stat_value {{{
morpher__finite_stat_value <- function(values, fun) {
    values <- as.numeric(values)
    values <- values[is.finite(values)]
    if (!length(values)) {
        return(NA_real_)
    }
    as.numeric(fun(values))
}
# }}}

# Assign extracted climate records to every requested period that contains
# their year. Overlapping future windows intentionally create a one-to-many
# mapping, so the cartesian expansion is allowed only at this explicit join.
# morpher__assign_periods {{{
morpher__assign_periods <- function(climate, periods) {
    climate[
        periods,
        on = "year",
        nomatch = 0L,
        allow.cartesian = TRUE
    ]
}
# }}}

# morpher__stat_rows {{{
morpher__stat_rows <- function(dt) {
    group_columns <- c(
        "site_id",
        "source_id",
        "experiment_id",
        "variant_label",
        "frequency",
        "table_id",
        "variable_id",
        "period",
        "month",
        "units"
    )
    dt <- data.table::as.data.table(data.table::copy(dt))
    has_lon <- "lon" %in% names(dt)
    has_lat <- "lat" %in% names(dt)
    # File identity is deliberately absent from the grouping columns. Compute
    # all statistics in one pass over the concatenated raw records so neither
    # file length nor missingness can change the scientific grouping.
    wide <- dt[,
        .(
            mean = morpher__finite_stat_value(value, mean),
            min = morpher__finite_stat_value(value, min),
            max = morpher__finite_stat_value(value, max),
            lon = if (has_lon) {
                morpher__finite_stat_value(lon, mean)
            } else {
                NA_real_
            },
            lat = if (has_lat) {
                morpher__finite_stat_value(lat, mean)
            } else {
                NA_real_
            },
            n_records = .N,
            n_valid = sum(is.finite(as.numeric(value)))
        ),
        by = group_columns
    ]
    rows <- data.table::melt(
        wide,
        id.vars = c(
            group_columns,
            "lon",
            "lat",
            "n_records",
            "n_valid"
        ),
        measure.vars = c("mean", "min", "max"),
        variable.name = "stat",
        value.name = "value",
        variable.factor = FALSE
    )
    # Source plan lineage belongs to the summary as a whole and is persisted
    # separately so file fragments do not split scientific statistic rows.
    rows[, plan_id := NA_character_]
    data.table::setcolorder(rows, c("plan_id", setdiff(names(rows), "plan_id")))
    rows[]
}
# }}}

# morpher__get_epw_path {{{
morpher__get_epw_path <- function(epw) {
    path <- tryCatch(epw$path(), error = function(e) NULL)
    if (
        is.null(path) ||
            !length(path) ||
            is.na(path[[1L]]) ||
            !nzchar(path[[1L]])
    ) {
        cli::cli_abort(
            "An internal EPW object used by {.cls EpwMorpher} must have a file path."
        )
    }
    path[[1L]]
}
# }}}

# morpher__json_int_vector {{{
morpher__json_int_vector <- function(x) {
    as.integer(jsonlite::fromJSON(x))
}
# }}}

# Persist only backend-owned scientific products, without serializing the EPW
# object, component functions or store connection. Content-addressed files keep
# earlier result references valid if a later execution stops partway through.
# morpher__process_write {{{
morpher__process_write <- function(store, result, plan, case, diagnostics) {
    sequence <- S7::S7_inherits(result, WeatherSequenceResult)
    plan <- data.table::copy(plan)
    data.table::set(
        plan,
        j = intersect(
            c("status", "updated_at", "last_error"),
            names(plan)
        ),
        value = NULL
    )
    payload <- list(
        schema_version = 1L,
        morph_id = plan$morph_id[[1L]],
        case_id = case$case_id[[1L]],
        plan = as.data.frame(plan),
        case = as.data.frame(case),
        package_version = as.character(utils::packageVersion("epwshiftr")),
        runtime_options = list(
            epwshiftr.threshold_alpha = getOption("epwshiftr.threshold_alpha")
        ),
        factors = if (sequence) result@factors else result$factors,
        parts = if (sequence) result@parts else result$parts,
        diagnostics = unique(diagnostics),
        provenance = if (sequence) result@provenance else list()
    )
    root <- file.path(
        store$path,
        "outputs",
        "epw-morph",
        payload$morph_id,
        paste0("case=", morpher__safe_path(payload$case_id))
    )
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile("process-", tmpdir = root)
    on.exit(unlink(temporary), add = TRUE)
    saveRDS(payload, temporary, version = 3L)
    sha256 <- checksum_file(temporary)
    path <- file.path(root, paste0("process-", sha256, ".rds"))
    if (!file.exists(path) || !identical(checksum_file(path), sha256)) {
        if (!file.rename(temporary, path)) {
            cli::cli_abort("Could not save morphing process data.")
        }
    }
    artifact_id <- store$register_artifact(
        kind = "output",
        path = path,
        role = "derived",
        checksum = sha256,
        metadata = list(
            morph_id = payload$morph_id,
            case_id = payload$case_id,
            content = "morph_process_data",
            schema_version = 1L
        )
    )
    list(
        path = store_rel_path(path, root = store$path),
        sha256 = sha256,
        artifact_id = artifact_id,
        schema_version = 1L
    )
}
# }}}

# Resolve a case's shared process reference and verify its content before either
# resuming results or exposing data. Old results remain usable as weather files
# but their missing scientific products must never be reconstructed as actuals.
# morpher__process_reference {{{
morpher__process_reference <- function(rows, store_root, required = TRUE) {
    references <- lapply(rows$provenance_json, function(value) {
        jsonlite::fromJSON(value, simplifyVector = FALSE)$process_data
    })
    absent <- vapply(references, is.null, logical(1L))
    if (all(absent) && !required) {
        return(NULL)
    }
    if (any(absent)) {
        cli::cli_abort(
            "Process data were not saved for this case. Rerun with `overwrite = TRUE` to capture actual execution data."
        )
    }
    reference <- references[[1L]]
    if (!all(vapply(references, identical, logical(1L), reference))) {
        cli::cli_abort("Result members reference different process data.")
    }
    path <- store_abs_path(reference$path, root = store_root)
    if (
        !file.exists(path) ||
            !identical(checksum_file(path), reference$sha256)
    ) {
        cli::cli_abort(
            "Morphing process data are missing or damaged. Restore the saved artifact or rerun with `overwrite = TRUE`."
        )
    }
    reference
}
# }}}

# morpher__normalize_result_manifest {{{
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
        if (
            name %in%
                c(
                    "sequence_id",
                    "weather_year",
                    "calendar",
                    "stochastic_seed"
                )
        ) {
            next
        }
        data.table::set(
            rows,
            i = which(missing),
            j = name,
            value = defaults[[name]]
        )
    }
    # Representative-year rows written before member manifests were added can
    # contain a non-positive count. Their one-file contract is unambiguous and
    # can be upgraded without rerunning the scientific method kernel.
    representative <- rows[["output_type"]] == "representative_year" &
        (is.na(rows[["member_count"]]) | rows[["member_count"]] < 1L)
    data.table::set(
        rows,
        i = which(representative),
        j = "member_count",
        value = 1L
    )
    rows[]
}
# }}}

# A resumed case is complete only when every member promised by its manifest
# still exists; one surviving year must not hide a missing sibling year.
# morpher__result_case_complete {{{
morpher__result_case_complete <- function(rows, store_root = NULL) {
    if (!nrow(rows)) {
        return(FALSE)
    }
    if (!is.null(store_root)) {
        paths <- vapply(
            rows[["output_path"]],
            store_abs_path,
            character(1L),
            root = store_root
        )
        if (any(!file.exists(paths))) {
            return(FALSE)
        }
        # New results promise durable process data as well as hourly weather.
        morpher__process_reference(rows, store_root, required = FALSE)
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
# }}}

# Restore deterministic case and member order after reading rows from DuckDB,
# whose physical row order is not a persistence contract.
# morpher__order_result_rows {{{
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
# }}}

MORPHER_GROUP_COLS <- c(
    "site_id",
    "source_id",
    "experiment_id",
    "variant_label",
    "frequency",
    "table_id",
    "period"
)

# Validate optional or required character identifier vectors consistently.
# morpher__assert_id_vec {{{
morpher__assert_id_vec <- function(x, null_ok = TRUE) {
    checkmate::assert_character(
        x,
        any.missing = FALSE,
        min.len = 1L,
        unique = TRUE,
        null.ok = null_ok
    )
}
# }}}

# Validate an epw_morph_periods() data frame when supplied or required.
# morpher__assert_periods {{{
morpher__assert_periods <- function(periods, null_ok = FALSE) {
    if (is.null(periods) && isTRUE(null_ok)) {
        return(invisible(NULL))
    }
    checkmate::assert_data_frame(periods)
    checkmate::assert_names(names(periods), must.include = c("period", "year"))
    invisible(NULL)
}
# }}}

# Shared argument validation for preflight(), plan(), preview_plan(), and workflow().
# morpher__assert_plan_args {{{
morpher__assert_plan_args <- function(
    plan_id = NULL,
    periods = NULL,
    reference_plan_id = NULL,
    reference_periods = NULL,
    observed_plan_id = NULL,
    observed_periods = NULL,
    summary_id = NULL,
    reference_summary_id = NULL,
    observed_summary_id = NULL,
    baseline_id = NULL,
    by = NULL,
    strict = TRUE,
    require_plan = FALSE,
    require_summary = FALSE,
    require_any = FALSE
) {
    morpher__assert_id_vec(plan_id, null_ok = !isTRUE(require_plan))
    morpher__assert_id_vec(reference_plan_id, null_ok = TRUE)
    morpher__assert_id_vec(observed_plan_id, null_ok = TRUE)
    morpher__assert_periods(periods, null_ok = !isTRUE(require_plan))
    morpher__assert_periods(reference_periods, null_ok = TRUE)
    morpher__assert_periods(observed_periods, null_ok = TRUE)
    checkmate::assert_string(
        summary_id,
        min.chars = if (isTRUE(require_summary)) 1L else 0L,
        null.ok = !isTRUE(require_summary)
    )
    checkmate::assert_string(reference_summary_id, null.ok = TRUE)
    checkmate::assert_string(observed_summary_id, null.ok = TRUE)
    checkmate::assert_string(baseline_id, null.ok = TRUE)
    morpher__assert_id_vec(by, null_ok = FALSE)
    checkmate::assert_subset(by, MORPHER_GROUP_COLS)
    checkmate::assert_flag(strict)
    if (isTRUE(require_any) && is.null(plan_id) && is.null(summary_id)) {
        cli::cli_abort("Either `plan_id` or `summary_id` must be supplied.")
    }
    if (!is.null(plan_id) && is.null(periods)) {
        cli::cli_abort("`periods` must be supplied when `plan_id` is supplied.")
    }
    if (!is.null(reference_plan_id) && is.null(reference_periods)) {
        cli::cli_abort(
            "`reference_periods` must be supplied when `reference_plan_id` is supplied."
        )
    }
    if (!is.null(observed_plan_id) && is.null(observed_periods)) {
        cli::cli_abort(
            "`observed_periods` must be supplied when `observed_plan_id` is supplied."
        )
    }
    invisible(NULL)
}
# }}}

# Compute structural requirement/rejection flags for historical and observed
# reference inputs against the active recipe.
# morpher__reference_role_state {{{
morpher__reference_role_state <- function(
    recipe,
    reference_supplied,
    observed_supplied
) {
    reference_supplied <- isTRUE(reference_supplied)
    observed_supplied <- isTRUE(observed_supplied)
    list(
        reference_missing = isTRUE(morpher__recipe_requires_reference(
            recipe
        )) &&
            !reference_supplied,
        reference_rejected = reference_supplied &&
            !isTRUE(morpher__recipe_accepts_reference(recipe)),
        observed_missing = isTRUE(
            morpher__recipe_requires_observed_reference(recipe)
        ) &&
            !observed_supplied,
        observed_rejected = observed_supplied &&
            !isTRUE(morpher__recipe_accepts_observed_reference(recipe))
    )
}
# }}}

# Abort before summaries or store writes when workflow reference arguments
# violate the selected backend's structural input contract.
# morpher__check_reference_roles {{{
morpher__check_reference_roles <- function(
    recipe,
    reference_supplied,
    observed_supplied
) {
    state <- morpher__reference_role_state(
        recipe,
        reference_supplied,
        observed_supplied
    )
    if (state$reference_missing) {
        cli::cli_abort(c(
            "The selected morphing backend requires explicit reference climate data.",
            "i" = "Supply `reference_plan_id` and `reference_periods`."
        ))
    }
    if (state$reference_rejected) {
        cli::cli_abort(
            "The selected morphing backend does not accept reference climate data."
        )
    }
    if (state$observed_missing) {
        cli::cli_abort(c(
            "The selected morphing backend requires explicit observed reference weather.",
            "i" = "Supply `observed_plan_id` and `observed_periods`."
        ))
    }
    if (state$observed_rejected) {
        cli::cli_abort(
            "The selected morphing backend does not accept observed reference weather."
        )
    }
    invisible(state)
}
# }}}

# Convert future, reference, and baseline summary values to common factor units
# and flag precipitation-specific dry-baseline or zero-reference guards.
# morpher__convert_factor_values {{{
morpher__convert_factor_values <- function(
    is_precip,
    status,
    external_reference,
    future_value,
    future_units,
    future_years_json,
    reference_value,
    reference_units,
    reference_years_json,
    base_value,
    base_units,
    raw_base_units,
    month
) {
    convert_step <- function(res) {
        if (!isTRUE(res$ok)) {
            status <<- "unit_conversion_failed"
        }
        res$value
    }
    if (isTRUE(is_precip)) {
        base_units <- "mm"
        if (identical(status, "ok")) {
            future_value <- convert_step(morpher__precip_summary_depth_checked(
                future_value,
                future_units,
                future_years_json,
                month
            ))
        }
        if (identical(status, "ok") && isTRUE(external_reference)) {
            reference_value <- convert_step(
                morpher__precip_summary_depth_checked(
                    reference_value,
                    reference_units,
                    reference_years_json,
                    month
                )
            )
        }
        if (identical(status, "ok")) {
            base_value <- convert_step(morpher__baseline_precip_depth_checked(
                base_value,
                raw_base_units,
                month
            ))
        }
        # A positive target cannot be allocated when the baseline EPW has no
        # wet hours for that month.
        if (
            identical(status, "ok") &&
                !is.na(base_value) &&
                base_value <= .Machine$double.eps &&
                !is.na(future_value) &&
                future_value > .Machine$double.eps
        ) {
            status <- "dry_baseline_precip"
        }
        if (
            identical(status, "ok") &&
                isTRUE(external_reference) &&
                !is.na(reference_value) &&
                reference_value <= .Machine$double.eps &&
                !is.na(future_value) &&
                future_value > .Machine$double.eps
        ) {
            status <- "zero_reference_precip"
        }
    } else if (identical(status, "ok")) {
        future_value <- convert_step(morpher__convert_value_checked(
            future_value,
            future_units,
            base_units
        ))
        if (identical(status, "ok") && isTRUE(external_reference)) {
            reference_value <- convert_step(morpher__convert_value_checked(
                reference_value,
                reference_units,
                base_units
            ))
        }
    }
    list(
        status = status,
        future_value = future_value,
        reference_value = reference_value,
        base_value = base_value,
        base_units = base_units
    )
}
# }}}

# Forward reporter events when an optional workflow reporter is attached.
# morpher__report {{{
morpher__report <- function(reporter, method, ...) {
    if (!is.null(reporter)) {
        do.call(reporter[[method]], list(...))
    }
    invisible(NULL)
}
# }}}

# EpwMorpher
#' Store-native EPW morpher
#'
#' @description
#' `EpwMorpher` consumes completed [EsgStore] extraction outputs and creates
#' future EPW files through a store-backed morphing workflow.
#'
#' @author Hongyuan Jia
#' @name EpwMorpher
#' @export
# EpwMorpher {{{
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
        #' @param transform A reusable weather transformation created by a
        #'   scale-specific transform constructor.
        #' @param label Optional source label.
        # initialize {{{
        initialize = function(
            store,
            epw,
            site_id = NULL,
            transform = monthly_transform("original_morphing"),
            label = NULL
        ) {
            if (!S7::S7_inherits(transform, WeatherTransformSpec)) {
                cli::cli_abort(
                    "{.arg transform} must be a {.cls WeatherTransformSpec}."
                )
            }
            private$store <- store
            private$store_private <- morpher__private_store(store)
            private$transform <- transform
            private$recipe <- transform__recipe(transform)
            checkmate::assert_string(site_id, null.ok = TRUE)
            checkmate::assert_string(label, null.ok = TRUE)
            private$site_id <- site_id
            private$label <- label

            private$register_epw(epw)
            self
        },
        # }}}

        #' @description
        #' Return transform-required CMIP variable IDs.
        # required_variables {{{
        required_variables = function() {
            epw_morph_variables(private$recipe)
        },
        # }}}

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
        # preflight {{{
        preflight = function(
            plan_id = NULL,
            periods = NULL,
            reference_plan_id = NULL,
            reference_periods = NULL,
            summary_id = NULL,
            reference_summary_id = NULL,
            observed_plan_id = NULL,
            observed_periods = NULL,
            observed_summary_id = NULL,
            baseline_id = NULL,
            by = c("source_id", "experiment_id", "variant_label", "period"),
            strict = TRUE
        ) {
            morpher__assert_plan_args(
                plan_id = plan_id,
                periods = periods,
                reference_plan_id = reference_plan_id,
                reference_periods = reference_periods,
                observed_plan_id = observed_plan_id,
                observed_periods = observed_periods,
                summary_id = summary_id,
                reference_summary_id = reference_summary_id,
                observed_summary_id = observed_summary_id,
                baseline_id = baseline_id,
                by = by,
                strict = strict,
                require_any = TRUE
            )
            reference_supplied <- !is.null(reference_plan_id) ||
                !is.null(reference_summary_id)
            observed_supplied <- !is.null(observed_plan_id) ||
                !is.null(observed_summary_id)
            extraction_roles <- list(
                list(plan_id, periods, "model_future"),
                list(reference_plan_id, reference_periods, "model_historical"),
                list(observed_plan_id, observed_periods, "observed_reference")
            )
            extraction_diags <- lapply(extraction_roles, function(role) {
                if (is.null(role[[1L]])) {
                    return(morpher__empty_diagnostics())
                }
                private$preflight_extraction(
                    role[[1L]],
                    role[[2L]],
                    strict = strict,
                    input_role = role[[3L]]
                )
            })
            morpher__bind_diagnostics(
                extraction_diags,
                private$preflight_role_summaries(
                    summary_id,
                    reference_summary_id,
                    observed_summary_id,
                    by,
                    strict = strict
                ),
                private$reference_role_diagnostics(
                    reference_supplied,
                    observed_supplied,
                    mode = "plan"
                ),
                private$preflight_baseline(baseline_id, strict = strict)
            )
        },
        # }}}

        #' @description
        #' Summarise extracted climate data by period and month.
        #'
        #' @param input_role Source role used to validate raw variables and
        #'   frequencies, one of `model_future`, `model_historical`, or
        #'   `observed_reference`. Workflow calls select this automatically.
        #'
        #' @param plan_id Extraction plan IDs.
        #' @param periods Period table from [epw_morph_periods()].
        #' @param strict Whether incomplete extraction coverage is an error.
        #' @param overwrite Whether to replace existing rows for this summary.
        # summarise_climate {{{
        summarise_climate = function(
            plan_id,
            periods,
            strict = TRUE,
            overwrite = FALSE,
            input_role = "model_future"
        ) {
            morpher__assert_id_vec(plan_id, null_ok = FALSE)
            morpher__assert_periods(periods)
            checkmate::assert_flag(strict)
            checkmate::assert_flag(overwrite)
            checkmate::assert_choice(
                input_role,
                c("model_future", "model_historical", "observed_reference")
            )
            diagnostics <- private$preflight_extraction(
                plan_id,
                periods,
                strict = strict,
                input_role = input_role
            )
            if (isTRUE(strict)) {
                morpher__abort_diagnostics(
                    diagnostics,
                    "Cannot summarise climate data because selected extraction plans have blocking issues."
                )
            }

            summary_id <- private$summary_id(plan_id, periods)
            current_summary <- private$summary_rows(summary_id)
            current_usable <- nrow(current_summary) &&
                all(
                    c("years_json", "lon", "lat") %in% names(current_summary)
                ) &&
                all(
                    !is.na(current_summary$years_json) &
                        nzchar(current_summary$years_json)
                )
            if (!isTRUE(overwrite) && isTRUE(current_usable)) {
                return(current_summary)
            }

            climate <- private$read_extraction_parquet(
                plan_id,
                empty_message = "No extraction results were found for the selected plan IDs.",
                ensure_units = TRUE
            )
            climate <- morpher__resolve_calendar_columns(climate, month = TRUE)
            periods <- data.table::as.data.table(periods)
            periods[, year := as.integer(year)]
            period_years <- periods[,
                .(
                    years_json = morpher__json(as.integer(sort(unique(year))))
                ),
                by = "period"
            ]
            climate <- morpher__assign_periods(climate, periods)
            if (!nrow(climate)) {
                cli::cli_abort(
                    "No extracted climate rows matched the supplied EPW morphing periods."
                )
            }
            source_plan_ids <- sort(unique(as.character(climate$plan_id)))
            source_plan_ids <- source_plan_ids[
                !is.na(source_plan_ids) & nzchar(source_plan_ids)
            ]

            rows <- morpher__stat_rows(climate)
            rows <- period_years[rows, on = "period"]
            created_at <- morpher__now()
            rows[, `:=`(
                summary_id = summary_id,
                coverage = 1,
                created_at = created_at
            )]
            rows[,
                summary_row_id := morpher__hash_rows(
                    summary_id,
                    site_id,
                    source_id,
                    experiment_id,
                    variant_label,
                    frequency,
                    table_id,
                    variable_id,
                    period,
                    month,
                    stat,
                    units
                )
            ]
            data.table::setcolorder(
                rows,
                c(
                    "summary_row_id",
                    "summary_id",
                    "plan_id",
                    "site_id",
                    "source_id",
                    "experiment_id",
                    "variant_label",
                    "frequency",
                    "table_id",
                    "variable_id",
                    "period",
                    "month",
                    "stat",
                    "value",
                    "units",
                    "lon",
                    "lat",
                    "years_json",
                    "coverage",
                    "n_records",
                    "n_valid",
                    "created_at"
                )
            )
            lineage <- data.table::data.table(
                summary_id = summary_id,
                plan_id = source_plan_ids,
                created_at = created_at
            )
            lineage[,
                summary_plan_id := morpher__hash_rows(summary_id, plan_id)
            ]
            data.table::setcolorder(
                lineage,
                c(
                    "summary_plan_id",
                    "summary_id",
                    "plan_id",
                    "created_at"
                )
            )
            morpher__delete_by_key(
                private$store,
                "epw_climate_summary",
                "summary_id",
                summary_id
            )
            morpher__delete_by_key(
                private$store,
                "epw_climate_summary_plan",
                "summary_id",
                summary_id
            )
            morpher__replace_rows(
                private$store,
                "epw_climate_summary",
                rows,
                "summary_row_id"
            )
            if (nrow(lineage)) {
                morpher__replace_rows(
                    private$store,
                    "epw_climate_summary_plan",
                    lineage,
                    "summary_plan_id"
                )
            }
            rows[]
        },
        # }}}

        #' @description
        #' Summarise baseline EPW weather by month.
        #'
        #' @param overwrite Whether to replace existing rows.
        # summarise_baseline {{{
        summarise_baseline = function(overwrite = FALSE) {
            checkmate::assert_flag(overwrite)
            baseline_id <- private$baseline_id()
            current <- morpher__read_table(
                private$store,
                "epw_baseline_summary"
            )
            target_baseline_id <- baseline_id
            current_baseline <- current[
                current[["baseline_id"]] == target_baseline_id
            ]
            if (!isTRUE(overwrite) && nrow(current_baseline)) {
                return(current_baseline)
            }

            epw <- private$epw$clone()
            suppressMessages(epw$add_unit())
            rules <- morpher__recipe_rules(private$recipe)
            fields <- unique(rules[required == TRUE & !derived, epw_field])
            data <- epw_file__calculation_weather(
                epw$data(),
                intersect(fields, names(EPW_FILE_FIELD_SPECS))
            )
            fields <- intersect(fields, names(data))
            units_map <- stats::setNames(
                lapply(fields, morpher__default_epw_units),
                fields
            )
            rows <- morpher__monthly_long(data, character(), fields, units_map)
            if (!nrow(rows)) {
                cli::cli_abort(
                    "No recipe EPW fields were found in the baseline EPW."
                )
            }
            rows[, `:=`(
                baseline_id = baseline_id,
                epw_id = private$epw_id,
                created_at = morpher__now()
            )]
            rows[,
                baseline_row_id := morpher__hash_rows(
                    baseline_id,
                    epw_field,
                    month,
                    stat
                )
            ]
            data.table::setcolorder(
                rows,
                c(
                    "baseline_row_id",
                    "baseline_id",
                    "epw_id",
                    "epw_field",
                    "month",
                    "stat",
                    "value",
                    "units",
                    "created_at"
                )
            )
            morpher__delete_by_key(
                private$store,
                "epw_baseline_summary",
                "baseline_id",
                baseline_id
            )
            morpher__replace_rows(
                private$store,
                "epw_baseline_summary",
                rows,
                "baseline_row_id"
            )
            rows[]
        },
        # }}}

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
        # plan {{{
        plan = function(
            summary_id,
            reference_summary_id = NULL,
            observed_summary_id = NULL,
            baseline_id = NULL,
            by = c("source_id", "experiment_id", "variant_label", "period"),
            strict = TRUE,
            overwrite = FALSE
        ) {
            morpher__assert_plan_args(
                summary_id = summary_id,
                reference_summary_id = reference_summary_id,
                observed_summary_id = observed_summary_id,
                baseline_id = baseline_id,
                by = by,
                strict = strict,
                require_summary = TRUE
            )
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
            current_plan <- private$read_morph_rows("epw_morph_plan", morph_id)
            if (!isTRUE(overwrite) && nrow(current_plan)) {
                return(current_plan)
            }

            morpher__delete_by_key(
                private$store,
                "epw_morph_factor",
                "morph_id",
                morph_id
            )
            morpher__delete_by_key(
                private$store,
                "epw_morph_observed_reference",
                "morph_id",
                morph_id
            )
            morpher__replace_rows(
                private$store,
                "epw_morph_plan",
                preview$plan,
                "morph_id"
            )
            morpher__replace_rows(
                private$store,
                "epw_morph_factor",
                preview$factors,
                "factor_id"
            )
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
        # }}}

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
        # preview_plan {{{
        preview_plan = function(
            summary_id,
            reference_summary_id = NULL,
            observed_summary_id = NULL,
            baseline_id = NULL,
            by = c("source_id", "experiment_id", "variant_label", "period"),
            strict = TRUE
        ) {
            morpher__assert_plan_args(
                summary_id = summary_id,
                reference_summary_id = reference_summary_id,
                observed_summary_id = observed_summary_id,
                baseline_id = baseline_id,
                by = by,
                strict = strict,
                require_summary = TRUE
            )
            if (is.null(baseline_id)) {
                baseline_id <- unique(self$summarise_baseline()$baseline_id)[[
                    1L
                ]]
            }

            climate <- private$read_summary_mean(
                "epw_climate_summary",
                "summary_id",
                summary_id,
                "climate summary",
                "summary ID"
            )
            reference <- if (!is.null(reference_summary_id)) {
                private$read_summary_mean(
                    "epw_climate_summary",
                    "summary_id",
                    reference_summary_id,
                    "reference climate summary",
                    "summary ID"
                )
            }
            if (!is.null(observed_summary_id)) {
                private$read_summary_mean(
                    "epw_climate_summary",
                    "summary_id",
                    observed_summary_id,
                    "observed climate summary",
                    "summary ID"
                )
            }
            baseline <- private$read_summary_mean(
                "epw_baseline_summary",
                "baseline_id",
                baseline_id,
                "baseline summary",
                "baseline ID"
            )

            morph_id <- private$morph_id(
                summary_id,
                reference_summary_id,
                observed_summary_id,
                baseline_id,
                by,
                strict
            )
            factors <- private$factor_rows(
                morph_id,
                climate,
                baseline,
                by,
                strict = strict,
                reference = reference
            )
            role_state <- morpher__reference_role_state(
                private$recipe,
                !is.null(reference_summary_id),
                !is.null(observed_summary_id)
            )
            diagnostics <- morpher__bind_diagnostics(
                private$preflight_role_summaries(
                    summary_id,
                    reference_summary_id,
                    observed_summary_id,
                    by,
                    strict = strict
                ),
                private$reference_role_diagnostics(
                    !is.null(reference_summary_id),
                    !is.null(observed_summary_id),
                    morph_id = morph_id,
                    mode = "summary"
                ),
                private$preflight_baseline(baseline_id, strict = strict),
                private$factor_diagnostics(
                    factors,
                    strict = strict,
                    morph_id = morph_id
                )
            )
            structural_reference_error <- any(unlist(role_state))
            status <- if (
                structural_reference_error ||
                    (any(diagnostics$severity == "error") && isTRUE(strict))
            ) {
                "blocked"
            } else {
                "planned"
            }
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
        # }}}

        #' @description
        #' Diagnose a morphing plan.
        #'
        #' @param morph_id Morphing plan ID.
        # diagnose {{{
        diagnose = function(morph_id) {
            checkmate::assert_string(morph_id, min.chars = 1L)
            plan <- private$get_plan(morph_id)
            if (identical(private$recipe$backend, "hourly_kernel_qdm")) {
                # Revalidate role-specific inputs for a saved distribution-based
                # plan without requiring inapplicable monthly factors.
                return(self$preflight(
                    summary_id = plan$summary_id[[1L]],
                    reference_summary_id = plan$reference_summary_id[[1L]],
                    observed_summary_id = private$observed_summary_id(morph_id),
                    baseline_id = plan$baseline_id[[1L]],
                    by = private$plan_by(plan),
                    strict = isTRUE(plan$strict[[1L]])
                ))
            }
            factors <- private$read_morph_rows("epw_morph_factor", morph_id)
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
            private$factor_diagnostics(
                factors,
                strict = isTRUE(plan$strict[[1L]]),
                morph_id = morph_id
            )
        },
        # }}}

        #' @description
        #' Abort if a morphing plan has blocking diagnostics.
        #'
        #' @param morph_id Morphing plan ID.
        # check {{{
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
        # }}}

        #' @description
        #' Execute a morphing plan and write one hourly Parquet file per result
        #' member and weather year.
        #'
        #' @param morph_id Morphing plan ID.
        #' @param overwrite Whether to overwrite existing result files.
        #' @param resume Whether to reuse complete existing results.
        #' @param reporter Optional workflow reporter used by task-level runs.
        # run {{{
        run = function(
            morph_id,
            overwrite = FALSE,
            resume = TRUE,
            reporter = NULL
        ) {
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
                cli::cli_abort(
                    "No morphing cases were found for morph ID {.val {morph_id}}."
                )
            }

            existing <- private$read_morph_rows(
                "epw_morph_result",
                morph_id,
                normalize = TRUE
            )
            complete_existing <- existing[existing[["case_id"]] %in% cases]
            complete_cases <- cases[vapply(
                cases,
                function(case_id) {
                    if (isTRUE(overwrite)) {
                        return(FALSE)
                    }
                    rows <- complete_existing[
                        which(complete_existing[["case_id"]] == case_id)
                    ]
                    morpher__result_case_complete(
                        rows,
                        store_root = private$store$path
                    )
                },
                logical(1L)
            )]
            private$reset_case_statuses(
                morph_id,
                plan_cases,
                complete_cases
            )
            if (
                !isTRUE(overwrite) &&
                    isTRUE(resume) &&
                    length(complete_cases) == length(cases)
            ) {
                private$set_plan_status(morph_id, "result_done")
                if (!is.null(reporter)) {
                    for (case_index in seq_along(cases)) {
                        case <- plan_cases[case_index]
                        label <- private$report_case_label(case)
                        morpher__report(
                            reporter,
                            "unit_started",
                            label,
                            current = case_index,
                            total = length(cases),
                            details = private$report_case_details(
                                case,
                                "morph_case"
                            )
                        )
                        morpher__report(
                            reporter,
                            "unit_skipped",
                            sprintf("Reused %s", label),
                            current = case_index,
                            total = length(cases)
                        )
                    }
                }
                return(morpher__order_result_rows(complete_existing, cases))
            }

            private$set_plan_status(morph_id, "running")
            tryCatch(
                {
                    climate <- private$engine_climate_data(plan$summary_id[[
                        1L
                    ]])
                    reference_summary_id <- if (
                        "reference_summary_id" %in% names(plan)
                    ) {
                        store__chr1(plan$reference_summary_id[[1L]])
                    } else {
                        NA_character_
                    }
                    reference_climate <- private$load_role_climate(
                        reference_summary_id,
                        required = morpher__recipe_requires_reference(
                            private$recipe
                        ),
                        label = "reference climate data"
                    )
                    observed_climate <- private$load_role_climate(
                        private$observed_summary_id(morph_id),
                        required = morpher__recipe_requires_observed_reference(
                            private$recipe
                        ),
                        label = "observed reference weather"
                    )
                    by <- private$plan_by(plan)
                    reference_by <- morpher__reference_case_by(by)
                    observed_by <- morpher__observed_case_by(by)
                    result_rows <- list()
                    case_errors <- list()
                    case_diagnostics <- list()
                    for (case_index in seq_along(cases)) {
                        morpher__report(reporter, "check_cancel", "morph")
                        case_id <- cases[[case_index]]
                        case <- plan_cases[case_index]
                        label <- private$report_case_label(case)
                        morpher__report(
                            reporter,
                            "unit_started",
                            label,
                            current = case_index,
                            total = length(cases),
                            details = private$report_case_details(
                                case,
                                "morph_case"
                            )
                        )
                        attempt <- tryCatch(
                            private$execute_case(
                                morph_id,
                                case,
                                climate,
                                reference_climate,
                                observed_climate,
                                by,
                                reference_by,
                                observed_by,
                                existing,
                                overwrite,
                                resume
                            ),
                            error = identity
                        )
                        if (inherits(attempt, "error")) {
                            morpher__delete_case_diagnostics(
                                private$store,
                                morph_id,
                                case_id
                            )
                            private$set_case_status(
                                morph_id,
                                case,
                                "failed",
                                error = attempt
                            )
                            case_diagnostics[[length(case_diagnostics) + 1L]] <-
                                morpher__case_error_diagnostic(
                                    attempt,
                                    morph_id,
                                    case_id,
                                    case
                                )
                            case_errors[[length(case_errors) + 1L]] <- attempt
                            morpher__report(
                                reporter,
                                "unit_completed",
                                sprintf("Failed %s", label),
                                current = case_index,
                                total = length(cases),
                                outcome = "failed",
                                details = c(
                                    private$report_case_details(
                                        case,
                                        "morph_case"
                                    ),
                                    list(
                                        error_class = class(attempt)[[1L]],
                                        error = conditionMessage(attempt)
                                    )
                                )
                            )
                            next
                        }
                        if (!isTRUE(attempt$reused)) {
                            morpher__delete_case_diagnostics(
                                private$store,
                                morph_id,
                                case_id
                            )
                        }
                        result_rows[[length(result_rows) + 1L]] <- attempt$rows
                        private$set_case_status(morph_id, case, "completed")
                        case_diagnostics[[length(case_diagnostics) + 1L]] <-
                            attempt$diagnostics
                        if (isTRUE(attempt$reused)) {
                            morpher__report(
                                reporter,
                                "unit_skipped",
                                sprintf("Reused %s", label),
                                current = case_index,
                                total = length(cases)
                            )
                        } else {
                            morpher__report(
                                reporter,
                                "unit_completed",
                                sprintf("Morphed %s", label),
                                current = case_index,
                                total = length(cases),
                                outcome = "completed"
                            )
                        }
                    }
                    # Publish one coherent diagnostic set after all cases have
                    # attempted. Per-case deletion above preserves diagnostics
                    # for resumed siblings, while this batched write makes the
                    # result independent of successful/failed case order.
                    private$persist_case_diagnostics(
                        morpher__bind_diagnostics(case_diagnostics)
                    )
                    results <- data.table::rbindlist(
                        result_rows,
                        use.names = TRUE,
                        fill = TRUE
                    )
                    if (!nrow(results)) {
                        messages <- unique(vapply(
                            case_errors,
                            conditionMessage,
                            character(1L)
                        ))
                        cli::cli_abort(c(
                            "Every morphing case failed.",
                            "x" = messages
                        ))
                    }
                    morpher__delete_by_key(
                        private$store,
                        "epw_morph_result",
                        "morph_id",
                        morph_id
                    )
                    morpher__replace_rows(
                        private$store,
                        "epw_morph_result",
                        results,
                        "result_id"
                    )
                    private$set_plan_status(
                        morph_id,
                        if (length(case_errors)) {
                            "result_partial"
                        } else {
                            "result_done"
                        },
                        if (length(case_errors)) {
                            sprintf(
                                "%d of %d morphing cases failed.",
                                length(case_errors),
                                length(cases)
                            )
                        } else {
                            NA_character_
                        }
                    )
                    morpher__order_result_rows(results, cases)
                },
                error = function(e) {
                    private$set_plan_status(
                        morph_id,
                        "failed",
                        conditionMessage(e)
                    )
                    stop(e)
                }
            )
        },
        # }}}

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
        # write_epw {{{
        write_epw = function(
            morph_id,
            dir,
            separate = TRUE,
            overwrite = FALSE,
            resume = TRUE,
            reporter = NULL
        ) {
            checkmate::assert_string(morph_id, min.chars = 1L)
            checkmate::assert_string(dir, min.chars = 1L)
            checkmate::assert_flag(separate)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)
            plan <- private$get_plan(morph_id)
            results <- private$read_morph_rows(
                "epw_morph_result",
                morph_id,
                normalize = TRUE
            )
            if (!nrow(results)) {
                cli::cli_abort(
                    "No morphing results were found. Run {.code EpwMorpher$run()} first."
                )
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
                    current_outputs <- private$read_morph_rows(
                        "epw_output",
                        morph_id,
                        normalize = TRUE
                    )
                    output_rows <- vector("list", nrow(results))
                    for (i in seq_len(nrow(results))) {
                        morpher__report(reporter, "check_cancel", "write_epw")
                        result <- results[i]
                        result_path <- store_abs_path(
                            result$output_path[[1L]],
                            root = private$store$path
                        )
                        dt <- morpher__parquet_read(private$store, result_path)
                        meta <- private$case_metadata_from_result(dt)
                        sequence_meta <- if (
                            identical(
                                result$output_type[[1L]],
                                "representative_year"
                            )
                        ) {
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
                        morpher__report(
                            reporter,
                            "unit_started",
                            label,
                            current = i,
                            total = nrow(results),
                            details = list(
                                unit_type = "epw_case",
                                scenario = meta$experiment_id,
                                period = meta$period
                            )
                        )
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
                        output_rel <- store_rel_path(
                            output_path,
                            root = private$store$path
                        )
                        same_result <- current_outputs[["result_id"]] ==
                            result$result_id[[1L]]
                        same_result[is.na(same_result)] <-
                            current_outputs[["case_id"]][is.na(same_result)] ==
                                result$case_id[[1L]]
                        existing_output <- current_outputs[
                            same_result &
                                current_outputs[["path"]] == output_rel
                        ]
                        if (
                            !isTRUE(overwrite) &&
                                isTRUE(resume) &&
                                nrow(existing_output) &&
                                file.exists(output_path)
                        ) {
                            output_rows[[i]] <- existing_output[1L]
                            morpher__report(
                                reporter,
                                "unit_skipped",
                                sprintf("Reused EPW %s", label),
                                current = i,
                                total = nrow(results)
                            )
                            next
                        }
                        if (file.exists(output_path) && !isTRUE(overwrite)) {
                            cli::cli_abort(
                                "EPW output already exists without a complete manifest row: {.path {output_path}}."
                            )
                        }
                        new_epw <- private$epw$clone()
                        set_data <- data.table::copy(dt[,
                            intersect(base_cols, names(dt)),
                            with = FALSE
                        ])
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
                            new_epw,
                            set_data,
                            private$recipe$options
                        )
                        case_label <- paste(
                            unlist(path_meta, use.names = FALSE),
                            collapse = "-"
                        )
                        new_epw$comment1(disclaimer_comment(case_label))
                        new_epw$fill_abnormal(
                            missing = TRUE,
                            out_of_range = TRUE,
                            special = TRUE
                        )
                        dir.create(
                            dirname(output_path),
                            recursive = TRUE,
                            showWarnings = FALSE
                        )
                        new_epw$save(output_path, overwrite = overwrite)
                        member_fields <- list(
                            output_type = result$output_type[[1L]],
                            sequence_id = result$sequence_id[[1L]],
                            weather_year = result$weather_year[[1L]],
                            calendar = result$calendar[[1L]],
                            stochastic_seed = result$stochastic_seed[[1L]],
                            member_count = result$member_count[[1L]],
                            provenance_json = result$provenance_json[[1L]]
                        )
                        artifact_id <- private$store$register_artifact(
                            kind = "output",
                            path = output_path,
                            role = "output",
                            project = "CMIP6",
                            metadata = c(
                                list(
                                    morph_id = morph_id,
                                    case_id = result$case_id[[1L]],
                                    result_id = result$result_id[[1L]]
                                ),
                                member_fields,
                                meta
                            )
                        )
                        output_id <- morpher__hash(
                            morph_id,
                            if (
                                identical(
                                    member_fields$output_type,
                                    "representative_year"
                                )
                            ) {
                                result$case_id[[1L]]
                            } else {
                                result$result_id[[1L]]
                            },
                            output_path
                        )
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
                            member_fields,
                            created_at = morpher__now(),
                            stringsAsFactors = FALSE
                        )
                        morpher__report(
                            reporter,
                            "unit_completed",
                            sprintf("Wrote EPW %s", label),
                            current = i,
                            total = nrow(results),
                            outcome = "completed"
                        )
                    }
                    outputs <- data.table::rbindlist(
                        output_rows,
                        use.names = TRUE,
                        fill = TRUE
                    )
                    morpher__delete_by_key(
                        private$store,
                        "epw_output",
                        "morph_id",
                        morph_id
                    )
                    morpher__replace_rows(
                        private$store,
                        "epw_output",
                        outputs,
                        "output_id"
                    )
                    morph_cases <- private$read_morph_rows(
                        "epw_morph_case",
                        morph_id
                    )
                    private$set_plan_status(
                        morph_id,
                        if (
                            nrow(morph_cases) &&
                                any(morph_cases$status == "failed")
                        ) {
                            "epw_partial"
                        } else {
                            "epw_written"
                        }
                    )
                    outputs[]
                },
                error = function(e) {
                    private$set_plan_status(
                        morph_id,
                        "failed",
                        conditionMessage(e)
                    )
                    stop(e)
                }
            )
        },
        # }}}

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
        # workflow {{{
        workflow = function(
            plan_id,
            periods,
            reference_plan_id = NULL,
            reference_periods = NULL,
            observed_plan_id = NULL,
            observed_periods = NULL,
            by = c("source_id", "experiment_id", "variant_label", "period"),
            strict = TRUE,
            dir = "outputs/future-epw",
            separate = TRUE,
            overwrite = FALSE,
            resume = TRUE,
            reporter = NULL
        ) {
            morpher__assert_plan_args(
                plan_id = plan_id,
                periods = periods,
                reference_plan_id = reference_plan_id,
                reference_periods = reference_periods,
                observed_plan_id = observed_plan_id,
                observed_periods = observed_periods,
                by = by,
                strict = strict,
                require_plan = TRUE
            )
            checkmate::assert_string(dir, min.chars = 1L, null.ok = TRUE)
            checkmate::assert_flag(separate)
            checkmate::assert_flag(overwrite)
            checkmate::assert_flag(resume)

            # Stop before summaries or store writes when reference structure is
            # incompatible with the selected backend.
            morpher__check_reference_roles(
                private$recipe,
                !is.null(reference_plan_id),
                !is.null(observed_plan_id)
            )

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
            climate <- self$summarise_climate(
                plan_id = plan_id,
                periods = periods,
                strict = strict,
                overwrite = overwrite
            )
            reference_summary_id <- private$summarise_optional_role(
                reference_plan_id,
                reference_periods,
                strict,
                overwrite,
                "model_historical"
            )
            observed_summary_id <- private$summarise_optional_role(
                observed_plan_id,
                observed_periods,
                strict,
                overwrite,
                "observed_reference"
            )
            baseline <- self$summarise_baseline(overwrite = overwrite)
            summary_id <- unique(climate$summary_id)[[1L]]
            baseline_id <- unique(baseline$baseline_id)[[1L]]
            preview <- self$preview_plan(
                summary_id = summary_id,
                reference_summary_id = reference_summary_id,
                observed_summary_id = observed_summary_id,
                baseline_id = baseline_id,
                by = by,
                strict = strict
            )
            plan <- self$plan(
                summary_id = summary_id,
                reference_summary_id = reference_summary_id,
                observed_summary_id = observed_summary_id,
                baseline_id = baseline_id,
                by = by,
                strict = strict,
                overwrite = overwrite
            )
            morph_id <- plan$morph_id[[1L]]
            diagnostics <- morpher__bind_diagnostics(
                preflight,
                preview$diagnostics
            )
            if (isTRUE(strict)) {
                self$check(morph_id)
            }
            results <- self$run(
                morph_id,
                overwrite = overwrite,
                resume = resume,
                reporter = reporter
            )
            runtime_diagnostics <- private$read_morph_rows(
                "epw_morph_diagnostic",
                morph_id
            )
            if ("diagnostic_id" %in% names(runtime_diagnostics)) {
                runtime_diagnostics[, diagnostic_id := NULL]
            }
            diagnostics <- morpher__bind_diagnostics(
                diagnostics,
                runtime_diagnostics
            )
            case_status <- private$read_morph_rows("epw_morph_case", morph_id)
            outputs <- if (is.null(dir)) {
                NULL
            } else {
                self$write_epw(
                    morph_id,
                    dir = dir,
                    separate = separate,
                    overwrite = overwrite,
                    resume = resume,
                    reporter = reporter
                )
            }
            list(
                preflight = preflight,
                climate = climate,
                baseline = baseline,
                preview = preview,
                plan = plan,
                diagnostics = diagnostics,
                cases = case_status,
                results = results,
                outputs = outputs
            )
        },
        # }}}

        #' @description
        #' Return morphing plan status rows.
        #'
        #' @param morph_id Optional morphing plan IDs.
        # status {{{
        status = function(morph_id = NULL) {
            morpher__assert_id_vec(morph_id, null_ok = TRUE)
            private$read_morph_rows("epw_morph_plan", morph_id)
        },
        # }}}

        #' @description
        #' Read saved execution products for one morphing plan. The result is a
        #' list named by case ID. Each entry contains the saved plan and case,
        #' runtime options, backend `factors`, method-specific `parts`, runtime
        #' diagnostics and provenance, plus the associated result manifest rows
        #' in `results`. These are execution products, not preview factors.
        #'
        #' Factor tables may summarize hourly values. Use method-specific parts
        #' for hourly application parameters where supplied by the backend.
        #' Empty or NULL products mean that the backend did not supply them.
        #' This interface does not reconstruct unrecorded pipeline stages.
        #' Multi-year members share one case payload. Reading checks its SHA256.
        #' Older runs without saved process data require an explicit rerun with
        #' `overwrite = TRUE`. No climate data are fetched by this method.
        #'
        #' @param morph_id Morphing plan ID.
        #' @param case_id Optional case ID to read.
        # process_data {{{
        process_data = function(morph_id, case_id = NULL) {
            checkmate::assert_string(morph_id, min.chars = 1L)
            checkmate::assert_string(case_id, min.chars = 1L, null.ok = TRUE)
            private$get_plan(morph_id)
            rows <- private$read_morph_rows(
                "epw_morph_result",
                morph_id,
                normalize = TRUE
            )
            if (!is.null(case_id)) {
                selected <- which(rows[["case_id"]] == case_id)
                rows <- rows[selected]
            }
            if (!nrow(rows)) {
                cli::cli_abort(
                    "No saved results match the requested morphing plan and case."
                )
            }
            cases <- unique(rows[["case_id"]])
            stats::setNames(
                lapply(cases, function(id) {
                    members <- rows[rows[["case_id"]] == id]
                    reference <- morpher__process_reference(
                        members,
                        private$store$path
                    )
                    value <- readRDS(store_abs_path(
                        reference$path,
                        private$store$path
                    ))
                    if (
                        !identical(value$schema_version, 1L) ||
                            !identical(value$morph_id, morph_id) ||
                            !identical(value$case_id, id)
                    ) {
                        cli::cli_abort(
                            "Saved process data have an unsupported schema or mismatched case identity."
                        )
                    }
                    value$results <- members
                    value
                }),
                cases
            )
        },
        # }}}

        #' @description
        #' Return future EPW output rows.
        #'
        #' @param morph_id Optional morphing plan IDs.
        # outputs {{{
        outputs = function(morph_id = NULL) {
            morpher__assert_id_vec(morph_id, null_ok = TRUE)
            private$read_morph_rows("epw_output", morph_id)
        }
        # }}}
    ),

    private = list(
        store = NULL,
        store_private = NULL,
        epw = NULL,
        epw_id = NULL,
        site_id = NULL,
        label = NULL,
        transform = NULL,
        recipe = NULL,

        # Build one stable user-facing label from the scientific case identity,
        # avoiding opaque internal case hashes in progress displays.
        # report_case_label {{{
        report_case_label = function(case) {
            fields <- intersect(
                c("experiment_id", "variant_label", "period"),
                names(case)
            )
            values <- unlist(case[, ..fields], use.names = FALSE)
            values <- values[!is.na(values) & nzchar(as.character(values))]
            paste(as.character(values), collapse = " | ")
        },
        # }}}

        # Preserve structured case identity alongside the concise label so CLI
        # watch can render progress without reparsing human-readable messages.
        # report_case_details {{{
        report_case_details = function(case, unit_type) {
            list(
                unit_type = unit_type,
                scenario = if ("experiment_id" %in% names(case)) {
                    case$experiment_id[[1L]]
                } else {
                    NULL
                },
                period = if ("period" %in% names(case)) {
                    case$period[[1L]]
                } else {
                    NULL
                }
            )
        },
        # }}}

        # register_epw {{{
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
            epw_id <- morpher__hash(
                "epw",
                checksum,
                private$site_id,
                private$label
            )
            target_dir <- file.path(private$store$path, "sources", "epw")
            dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
            target <- file.path(
                target_dir,
                sprintf("%s-%s", substr(epw_id, 1L, 12L), basename(epw_path))
            )
            if (!file.exists(target)) {
                ok <- file.copy(epw_path, target, overwrite = TRUE)
                if (!isTRUE(ok)) {
                    cli::cli_abort(
                        "Failed to copy baseline EPW into the store."
                    )
                }
            }
            artifact_id <- private$store$register_artifact(
                kind = "source",
                path = target,
                role = "source",
                project = "CMIP6",
                checksum = checksum,
                checksum_type = "sha256",
                metadata = list(
                    site_id = private$site_id,
                    label = private$label
                )
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
        # }}}

        # baseline_id {{{
        baseline_id = function() {
            morpher__hash(
                "baseline",
                private$epw_id,
                morpher__json(private$recipe)
            )
        },
        # }}}

        # summary_id {{{
        summary_id = function(plan_id, periods) {
            morpher__hash(
                "summary",
                private$epw_id,
                paste(sort(plan_id), collapse = "\r"),
                morpher__json(periods)
            )
        },
        # }}}

        # morph_id {{{
        morph_id = function(
            summary_id,
            reference_summary_id,
            observed_summary_id,
            baseline_id,
            by,
            strict
        ) {
            strict_token <- if (isTRUE(strict)) {
                "strict=true"
            } else {
                "strict=false"
            }
            pieces <- list(
                "morph",
                private$epw_id,
                summary_id,
                store__chr1(reference_summary_id),
                baseline_id,
                paste(by, collapse = "\r"),
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
        # }}}

        # plan_by {{{
        plan_by = function(plan) {
            by <- jsonlite::fromJSON(plan$by_json[[1L]])
            as.character(by)
        },
        # }}}

        # extraction_rows {{{
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
        # }}}

        # summary_rows {{{
        summary_rows = function(summary_id, stat = NULL) {
            climate <- morpher__read_table(private$store, "epw_climate_summary")
            target_summary_id <- summary_id
            climate <- climate[climate[["summary_id"]] == target_summary_id]
            if (!is.null(stat)) {
                climate <- climate[climate[["stat"]] %in% stat]
            }
            climate[]
        },
        # }}}

        # summary_period_years {{{
        summary_period_years = function(summary_id) {
            climate <- private$summary_rows(summary_id)
            if (!nrow(climate)) {
                cli::cli_abort(
                    "No climate summary rows were found for summary ID {.val {summary_id}}."
                )
            }
            if (
                !"years_json" %in% names(climate) ||
                    any(is.na(climate$years_json) | !nzchar(climate$years_json))
            ) {
                cli::cli_abort(
                    "Climate summary lacks period-year metadata. Re-run {.code EpwMorpher$summarise_climate(..., overwrite = TRUE)}."
                )
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
        # }}}

        # Read store rows filtered by morph_id outside DT[] column scoping.
        # read_morph_rows {{{
        read_morph_rows = function(table, morph_id = NULL, normalize = FALSE) {
            rows <- morpher__read_table(private$store, table)
            if (isTRUE(normalize)) {
                rows <- morpher__normalize_result_manifest(rows)
            }
            if (!is.null(morph_id)) {
                selected <- which(rows[["morph_id"]] %in% morph_id)
                rows <- rows[selected]
            }
            rows[]
        },
        # }}}

        # Read monthly mean rows for one summary or baseline ID and abort if empty.
        # read_summary_mean {{{
        read_summary_mean = function(table, id_col, id, label, id_label) {
            rows <- morpher__read_table(private$store, table)
            selected <- which(rows[[id_col]] == id & rows[["stat"]] == "mean")
            rows <- rows[selected]
            if (!nrow(rows)) {
                cli::cli_abort(
                    "No {label} rows were found for {id_label} {.val {id}}."
                )
            }
            rows[]
        },
        # }}}

        # Return the observed-reference summary ID registered for one morph plan.
        # observed_summary_id {{{
        observed_summary_id = function(morph_id) {
            rows <- private$read_morph_rows(
                "epw_morph_observed_reference",
                morph_id
            )
            if (!nrow(rows)) {
                return(NULL)
            }
            value <- store__chr1(rows$observed_summary_id[[1L]])
            if (is.na(value) || !nzchar(value)) NULL else value
        },
        # }}}

        # Build structural reference-role diagnostics shared by preflight() and
        # preview_plan() without downgrading them in relaxed mode.
        # reference_role_diagnostics {{{
        reference_role_diagnostics = function(
            reference_supplied,
            observed_supplied,
            morph_id = NA_character_,
            mode = c("plan", "summary")
        ) {
            mode <- match.arg(mode)
            state <- morpher__reference_role_state(
                private$recipe,
                reference_supplied,
                observed_supplied
            )
            is_plan <- identical(mode, "plan")
            emit <- function(flag, stage, code, message, action) {
                if (!isTRUE(flag)) {
                    return(morpher__empty_diagnostics())
                }
                morpher__diagnostic(
                    stage = stage,
                    severity = "error",
                    code = code,
                    message = message,
                    morph_id = morph_id,
                    action = action
                )
            }
            morpher__bind_diagnostics(
                emit(
                    state$reference_missing,
                    "reference",
                    "missing_reference_climate",
                    "The selected morphing backend requires reference climate data.",
                    if (is_plan) {
                        "Supply `reference_plan_id` and `reference_periods`, or use a backend that does not require reference climate."
                    } else {
                        "Supply `reference_summary_id`, or use a backend that does not require reference climate."
                    }
                ),
                emit(
                    state$reference_rejected,
                    "reference",
                    "unexpected_reference_climate",
                    "The selected morphing backend does not accept reference climate data.",
                    if (is_plan) {
                        "Remove the reference input or select a backend that accepts it."
                    } else {
                        "Remove `reference_summary_id` or select a backend that accepts it."
                    }
                ),
                emit(
                    state$observed_missing,
                    "observed_reference",
                    "missing_observed_reference",
                    "The selected morphing backend requires observed reference weather.",
                    if (is_plan) {
                        "Supply `observed_plan_id` and `observed_periods`."
                    } else {
                        "Supply `observed_summary_id`."
                    }
                ),
                emit(
                    state$observed_rejected,
                    "observed_reference",
                    "unexpected_observed_reference",
                    "The selected morphing backend does not accept observed reference weather.",
                    if (is_plan) {
                        "Remove the observed reference or select a backend that accepts it."
                    } else {
                        "Remove `observed_summary_id` or select a backend that accepts it."
                    }
                )
            )
        },
        # }}}

        # Check required variable presence and frequency diagnostics shared by
        # preflight_extraction() and preflight_summary().
        # preflight_variable_diagnostics {{{
        preflight_variable_diagnostics = function(
            table,
            required_variables,
            stage,
            severity,
            input_role,
            message_fmt,
            plan_id = NA_character_,
            summary_id = NA_character_
        ) {
            present_variables <- unique(table$variable_id)
            out <- list(morpher__frequency_diagnostic(
                private$recipe,
                if ("frequency" %in% names(table)) {
                    table$frequency
                } else {
                    character()
                },
                variable_id = table$variable_id,
                stage = stage,
                plan_id = plan_id,
                summary_id = summary_id,
                input_role = input_role
            ))
            for (variable_id in setdiff(
                required_variables,
                present_variables
            )) {
                guidance <- morpher__missing_variable_guidance(
                    variable_id,
                    present_variables
                )
                out[[length(out) + 1L]] <- morpher__diagnostic(
                    stage = stage,
                    severity = severity,
                    code = "missing_required_variable",
                    message = sprintf(
                        message_fmt,
                        variable_id,
                        guidance$suffix
                    ),
                    summary_id = summary_id,
                    variable_id = variable_id,
                    action = guidance$action
                )
            }
            out
        },
        # }}}

        # Emit missing-month diagnostics for all required (variable, period, month)
        # combinations absent from one extraction or climate-summary slice.
        # preflight_missing_month_diagnostics {{{
        preflight_missing_month_diagnostics = function(
            data,
            required_variables,
            periods,
            stage,
            severity,
            message_fmt,
            action,
            summary_id = NA_character_,
            case_id = NA_character_
        ) {
            present <- unique(data[, .(variable_id, period, month)])
            expected <- data.table::CJ(
                variable_id = required_variables,
                period = unique(periods),
                month = 1:12,
                unique = TRUE
            )
            missing <- expected[
                !present,
                on = c("variable_id", "period", "month")
            ]
            lapply(seq_len(nrow(missing)), function(i) {
                morpher__diagnostic(
                    stage = stage,
                    severity = severity,
                    code = "missing_month",
                    message = sprintf(
                        message_fmt,
                        missing$variable_id[[i]],
                        missing$period[[i]],
                        missing$month[[i]]
                    ),
                    summary_id = summary_id,
                    case_id = case_id,
                    variable_id = missing$variable_id[[i]],
                    period = missing$period[[i]],
                    month = missing$month[[i]],
                    action = action
                )
            })
        },
        # }}}

        # Preflight future, historical, and observed climate summaries together.
        # preflight_role_summaries {{{
        preflight_role_summaries = function(
            summary_id,
            reference_summary_id,
            observed_summary_id,
            by,
            strict = TRUE
        ) {
            roles <- list(
                list(summary_id, by, "model_future"),
                list(
                    reference_summary_id,
                    morpher__reference_case_by(by),
                    "model_historical"
                ),
                list(
                    observed_summary_id,
                    morpher__observed_case_by(by),
                    "observed_reference"
                )
            )
            morpher__bind_diagnostics(lapply(roles, function(role) {
                if (is.null(role[[1L]])) {
                    return(morpher__empty_diagnostics())
                }
                private$preflight_summary(
                    role[[1L]],
                    role[[2L]],
                    strict = strict,
                    input_role = role[[3L]]
                )
            }))
        },
        # }}}

        # Summarise an optional role in workflow() and return its summary ID.
        # summarise_optional_role {{{
        summarise_optional_role = function(
            plan_id,
            periods,
            strict,
            overwrite,
            input_role
        ) {
            if (is.null(plan_id)) {
                return(NULL)
            }
            summary <- self$summarise_climate(
                plan_id = plan_id,
                periods = periods,
                strict = strict,
                overwrite = overwrite,
                input_role = input_role
            )
            unique(summary$summary_id)[[1L]]
        },
        # }}}

        # Load optional historical or observed climate for run(), aborting when
        # the backend requires that role and none was persisted.
        # load_role_climate {{{
        load_role_climate = function(summary_id, required, label) {
            if (
                is.null(summary_id) ||
                    is.na(summary_id) ||
                    !nzchar(summary_id)
            ) {
                if (isTRUE(required)) {
                    cli::cli_abort(
                        "Backend {.val {private$recipe$backend}} requires {label}."
                    )
                }
                return(NULL)
            }
            private$engine_climate_data(summary_id)
        },
        # }}}

        # Read and combine extracted Parquet files for one set of plan IDs.
        # read_extraction_parquet {{{
        read_extraction_parquet = function(
            plan_id,
            empty_message,
            ensure_units = FALSE
        ) {
            result <- private$extraction_rows(plan_id)
            if (!nrow(result)) {
                cli::cli_abort(empty_message)
            }
            pieces <- vector("list", nrow(result))
            for (i in seq_len(nrow(result))) {
                path <- store_abs_path(
                    result$output_path[[i]],
                    root = private$store$path
                )
                dt <- morpher__parquet_read(private$store, path)
                if (isTRUE(ensure_units) && !"units" %in% names(dt)) {
                    dt[, units := NA_character_]
                }
                dt[, plan_id := result$plan_id[[i]]]
                pieces[[i]] <- dt
            }
            data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
        },
        # }}}

        # engine_climate_data {{{
        engine_climate_data = function(summary_id) {
            climate_summary <- private$summary_rows(summary_id)
            if (!nrow(climate_summary)) {
                cli::cli_abort(
                    "No climate summary rows were found for summary ID {.val {summary_id}}."
                )
            }
            plan_id <- morpher__summary_plan_ids(private$store, climate_summary)
            if (!length(plan_id)) {
                cli::cli_abort(
                    "Climate summary ID {.val {summary_id}} has no source extraction-plan lineage."
                )
            }
            climate <- private$read_extraction_parquet(
                plan_id,
                empty_message = sprintf(
                    "No extraction result files were found for climate summary ID '%s'.",
                    summary_id
                )
            )
            if (!nrow(climate)) {
                cli::cli_abort(
                    "No extracted climate rows were found for climate summary ID {.val {summary_id}}."
                )
            }
            climate <- morpher__resolve_calendar_columns(climate)
            period_years <- private$summary_period_years(summary_id)
            climate <- morpher__period_climate(
                climate,
                period_years,
                retain_padding = identical(
                    private$recipe$backend,
                    "hourly_kernel_qdm"
                )
            )
            if (!nrow(climate)) {
                cli::cli_abort(
                    "No extracted climate rows matched the stored EPW morphing periods."
                )
            }

            catalog <- morpher__read_table(private$store, "file_catalog")
            catalog_cols <- intersect(
                c(
                    "file_key",
                    "activity_id",
                    "institution_id",
                    "variable_long_name"
                ),
                names(catalog)
            )
            if ("file_key" %in% catalog_cols) {
                catalog <- unique(catalog[, catalog_cols, with = FALSE])
                climate <- merge(
                    climate,
                    catalog,
                    by = "file_key",
                    all.x = TRUE,
                    sort = FALSE
                )
            }

            if (!"units" %in% names(climate)) {
                climate[, units := NA_character_]
            }
            climate[]
        },
        # }}}

        # case_rows {{{
        case_rows = function(plan) {
            by <- private$plan_by(plan)
            climate <- private$summary_rows(
                plan$summary_id[[1L]],
                stat = "mean"
            )
            missing_by <- setdiff(by, names(climate))
            if (length(missing_by)) {
                cli::cli_abort(
                    "Climate summary is missing grouping column(s): {.val {missing_by}}."
                )
            }
            cases <- unique(climate[, by, with = FALSE])
            if (!nrow(cases)) {
                cli::cli_abort(
                    "No morphing cases were found for morph ID {.val {plan$morph_id[[1L]]}}."
                )
            }
            case_ids <- vapply(
                seq_len(nrow(cases)),
                function(i) {
                    morpher__hash(
                        plan$morph_id[[1L]],
                        morpher__json(as.list(cases[i]))
                    )
                },
                character(1L)
            )
            cases[, case_id := case_ids]
            cases[]
        },
        # }}}

        # filter_case_climate {{{
        filter_case_climate = function(climate, case, by) {
            case_values <- as.list(case[1L])
            keep <- rep(TRUE, nrow(climate))
            for (name in intersect(
                by,
                intersect(names(case), names(climate))
            )) {
                value <- store__chr1(case_values[[name]])
                keep <- keep & morpher__identical_match(climate[[name]], value)
            }
            climate[keep][]
        },
        # }}}

        # preflight_extraction {{{
        preflight_extraction = function(
            plan_id,
            periods,
            strict = TRUE,
            input_role = "model_future"
        ) {
            required_variables <- morpher__preflight_variables(
                private$recipe,
                input_role
            )
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
                    message = sprintf(
                        "Extraction plan %s is incomplete.",
                        incomplete$plan_id[[i]]
                    ),
                    plan_id = incomplete$plan_id[[i]],
                    variable_id = store__chr1(incomplete$variable_id[[i]]),
                    action = "Complete extraction before morphing."
                )
            }
            diagnostics <- c(
                diagnostics,
                private$preflight_variable_diagnostics(
                    coverage,
                    required_variables,
                    stage = "extraction",
                    severity = severity,
                    input_role = input_role,
                    message_fmt = "Required CMIP variable %s is missing from selected extraction plans.%s",
                    plan_id = paste(plan_id, collapse = ", ")
                )
            )

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
                path <- store_abs_path(
                    result$output_path[[i]],
                    root = private$store$path
                )
                dt <- tryCatch(
                    morpher__parquet_read(private$store, path),
                    error = function(e) e
                )
                if (inherits(dt, "error")) {
                    diagnostics[[
                        length(diagnostics) + 1L
                    ]] <- morpher__diagnostic(
                        stage = "extraction",
                        severity = "error",
                        code = "parquet_unreadable",
                        message = sprintf(
                            "Extraction result cannot be read: %s.",
                            conditionMessage(dt)
                        ),
                        plan_id = result$plan_id[[i]],
                        action = "Re-run extraction for this plan."
                    )
                    next
                }
                if (!all(c("time", "variable_id", "value") %in% names(dt))) {
                    diagnostics[[
                        length(diagnostics) + 1L
                    ]] <- morpher__diagnostic(
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

            climate <- data.table::rbindlist(
                pieces,
                use.names = TRUE,
                fill = TRUE
            )
            climate <- morpher__resolve_calendar_columns(climate, month = TRUE)
            periods <- data.table::as.data.table(periods)
            periods[, year := as.integer(year)]
            climate <- morpher__assign_periods(climate, periods)
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
            morpher__bind_diagnostics(
                diagnostics,
                private$preflight_missing_month_diagnostics(
                    climate,
                    required_variables,
                    periods$period,
                    stage = "extraction",
                    severity = severity,
                    message_fmt = "Required CMIP variable %s has no rows for period %s month %s.",
                    action = "Extract a complete morphing period, or run in relaxed mode."
                )
            )
        },
        # }}}

        # preflight_summary {{{
        preflight_summary = function(
            summary_id,
            by,
            strict = TRUE,
            input_role = "model_future"
        ) {
            required_variables <- morpher__preflight_variables(
                private$recipe,
                input_role
            )
            severity <- if (isTRUE(strict)) "error" else "warning"
            diagnostics <- list()
            climate <- private$summary_rows(summary_id, stat = "mean")
            if (!nrow(climate)) {
                return(morpher__diagnostic(
                    stage = "climate_summary",
                    severity = "error",
                    code = "missing_climate_summary",
                    message = sprintf(
                        "No climate summary rows were found for summary ID %s.",
                        summary_id
                    ),
                    summary_id = summary_id,
                    action = "Run EpwMorpher$summarise_climate() first."
                ))
            }
            missing_by <- setdiff(by, names(climate))
            if (length(missing_by)) {
                return(morpher__bind_diagnostics(lapply(
                    missing_by,
                    function(name) {
                        morpher__diagnostic(
                            stage = "climate_summary",
                            severity = "error",
                            code = "missing_group_column",
                            message = sprintf(
                                "Climate summary is missing grouping column %s.",
                                name
                            ),
                            summary_id = summary_id,
                            action = "Use grouping columns available in the climate summary."
                        )
                    }
                )))
            }
            diagnostics <- c(
                diagnostics,
                private$preflight_variable_diagnostics(
                    climate,
                    required_variables,
                    stage = "climate_summary",
                    severity = severity,
                    input_role = input_role,
                    message_fmt = "Required CMIP variable %s is missing from climate summary.%s",
                    summary_id = summary_id
                )
            )
            cases <- unique(climate[, by, with = FALSE])
            for (i in seq_len(nrow(cases))) {
                case <- cases[i]
                case_climate <- private$filter_case_climate(climate, case, by)
                case_id <- morpher__hash(
                    summary_id,
                    morpher__json(as.list(case))
                )
                diagnostics <- c(
                    diagnostics,
                    private$preflight_missing_month_diagnostics(
                        case_climate,
                        required_variables,
                        case_climate$period,
                        stage = "climate_summary",
                        severity = severity,
                        message_fmt = "Climate summary lacks %s for period %s month %s.",
                        action = "Rebuild climate summary from complete extraction results.",
                        summary_id = summary_id,
                        case_id = case_id
                    )
                )
            }
            morpher__bind_diagnostics(diagnostics)
        },
        # }}}

        # preflight_baseline {{{
        preflight_baseline = function(baseline_id = NULL, strict = TRUE) {
            severity <- if (isTRUE(strict)) "error" else "warning"
            rules <- morpher__recipe_rules(private$recipe)
            fields <- unique(rules[required == TRUE & !derived, epw_field])
            diagnostics <- list()

            # Inspect the raw EPW before its documented numeric sentinels are
            # converted to NA, retaining exact hourly and monthly evidence.
            epw <- private$epw$clone()
            raw_weather <- data.table::as.data.table(epw$data())
            missing_values <- epw_file__missing_summary(
                raw_weather,
                intersect(fields, names(EPW_FILE_FIELD_SPECS))
            )[missing_hours > 0L]
            for (i in seq_len(nrow(missing_values))) {
                row <- missing_values[i]
                complete_month <- row$missing_hours[[1L]] ==
                    row$total_hours[[1L]]
                message <- if (complete_month) {
                    sprintf(
                        "Baseline EPW field %s is missing for all %s hours in month %s.",
                        row$epw_field[[1L]],
                        row$total_hours[[1L]],
                        row$month[[1L]]
                    )
                } else {
                    sprintf(
                        "Baseline EPW field %s is missing for %s of %s hours in month %s.",
                        row$epw_field[[1L]],
                        row$missing_hours[[1L]],
                        row$total_hours[[1L]],
                        row$month[[1L]]
                    )
                }
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "baseline",
                    severity = severity,
                    code = "missing_epw_values",
                    message = message,
                    baseline_id = baseline_id,
                    epw_field = row$epw_field[[1L]],
                    month = row$month[[1L]],
                    action = paste(
                        "Use a baseline EPW with valid observations for this",
                        "field before morphing."
                    )
                )
            }
            if (!is.null(baseline_id)) {
                baseline <- morpher__read_table(
                    private$store,
                    "epw_baseline_summary"
                )
                target_baseline_id <- baseline_id
                baseline <- baseline[
                    baseline[["baseline_id"]] == target_baseline_id &
                        baseline[["stat"]] == "mean"
                ]
                if (!nrow(baseline)) {
                    return(morpher__diagnostic(
                        stage = "baseline",
                        severity = "error",
                        code = "missing_baseline_summary",
                        message = sprintf(
                            "No baseline summary rows were found for baseline ID %s.",
                            baseline_id
                        ),
                        baseline_id = baseline_id,
                        action = "Run EpwMorpher$summarise_baseline() first."
                    ))
                }
                missing_fields <- setdiff(fields, unique(baseline$epw_field))
                for (field in missing_fields) {
                    diagnostics[[
                        length(diagnostics) + 1L
                    ]] <- morpher__diagnostic(
                        stage = "baseline",
                        severity = severity,
                        code = "missing_epw_field",
                        message = sprintf(
                            "Baseline summary is missing EPW field %s.",
                            field
                        ),
                        baseline_id = baseline_id,
                        epw_field = field,
                        action = "Use a baseline EPW containing recipe fields, or run in relaxed mode."
                    )
                }
                present <- unique(baseline[, .(epw_field, month)])
                expected <- data.table::CJ(
                    epw_field = fields,
                    month = 1:12,
                    unique = TRUE
                )
                missing <- expected[!present, on = c("epw_field", "month")]
                # A documented all-month EPW sentinel already has a more
                # precise hourly diagnostic above; do not report the same gap
                # again as a generic missing summary month.
                sentinel_months <- missing_values[
                    missing_hours == total_hours,
                    .(epw_field, month)
                ]
                if (nrow(sentinel_months)) {
                    missing <- missing[
                        !sentinel_months,
                        on = c("epw_field", "month")
                    ]
                }
                for (i in seq_len(nrow(missing))) {
                    diagnostics[[
                        length(diagnostics) + 1L
                    ]] <- morpher__diagnostic(
                        stage = "baseline",
                        severity = severity,
                        code = "missing_baseline_month",
                        message = sprintf(
                            "Baseline summary lacks %s for month %s.",
                            missing$epw_field[[i]],
                            missing$month[[i]]
                        ),
                        baseline_id = baseline_id,
                        epw_field = missing$epw_field[[i]],
                        month = missing$month[[i]],
                        action = "Rebuild the baseline summary from a complete EPW."
                    )
                }
                return(morpher__bind_diagnostics(diagnostics))
            }

            suppressMessages(epw$add_unit())
            data <- data.table::as.data.table(epw$data())
            missing_fields <- setdiff(fields, names(data))
            for (field in missing_fields) {
                diagnostics[[length(diagnostics) + 1L]] <- morpher__diagnostic(
                    stage = "baseline",
                    severity = severity,
                    code = "missing_epw_field",
                    message = sprintf(
                        "Baseline EPW is missing recipe field %s.",
                        field
                    ),
                    epw_field = field,
                    action = "Use a baseline EPW containing recipe fields, or run in relaxed mode."
                )
            }
            morpher__bind_diagnostics(diagnostics)
        },
        # }}}

        # factor_diagnostics {{{
        factor_diagnostics = function(
            factors,
            strict = TRUE,
            morph_id = NA_character_
        ) {
            bad <- factors[factors[["status"]] != "ok"]
            if (!nrow(bad)) {
                return(morpher__empty_diagnostics())
            }
            severity <- if (isTRUE(strict)) "error" else "warning"
            rows <- vector("list", nrow(bad))
            for (i in seq_len(nrow(bad))) {
                # Name the failed input boundary explicitly. A missing EPW
                # monthly summary must not be reported as a missing CMIP6
                # variable merely because both prevent factor calculation.
                message <- switch(
                    bad$status[[i]],
                    missing_climate = sprintf(
                        "Future climate input %s is missing for month %s.",
                        bad$variable_id[[i]],
                        bad$month[[i]]
                    ),
                    missing_reference = sprintf(
                        "Historical climate reference %s is missing for month %s.",
                        bad$variable_id[[i]],
                        bad$month[[i]]
                    ),
                    missing_baseline = sprintf(
                        "Baseline EPW has no valid monthly value for %s in month %s.",
                        bad$epw_field[[i]],
                        bad$month[[i]]
                    ),
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
                        bad$epw_field[[i]],
                        bad$variable_id[[i]]
                    ),
                    sprintf(
                        "Morphing factor is not available for %s from %s.",
                        bad$epw_field[[i]],
                        bad$variable_id[[i]]
                    )
                )
                action <- switch(
                    bad$status[[i]],
                    missing_climate = sprintf(
                        "Provide complete future climate input for %s.",
                        bad$variable_id[[i]]
                    ),
                    missing_reference = sprintf(
                        "Provide complete Historical climate input for %s.",
                        bad$variable_id[[i]]
                    ),
                    missing_baseline = sprintf(
                        paste(
                            "Use a baseline EPW with valid %s observations",
                            "for this month."
                        ),
                        bad$epw_field[[i]]
                    ),
                    dry_baseline_precip = "Use a baseline EPW with wet hours for that month, or run in relaxed mode to keep it dry.",
                    zero_reference_precip = "Provide non-zero historical precipitation for that month, or run in relaxed mode to keep baseline precipitation unchanged.",
                    unit_conversion_failed = if (
                        identical(bad$variable_id[[i]], "pr")
                    ) {
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
        # }}}

        # factor_rows {{{
        factor_rows = function(
            morph_id,
            climate,
            baseline,
            by,
            strict = TRUE,
            reference = NULL
        ) {
            if (identical(private$recipe$backend, "hourly_kernel_qdm")) {
                # Distribution mappings are computed from full hourly series.
                # A monthly factor table would misrepresent this method.
                return(morpher__read_table(private$store, "epw_morph_factor")[
                    0L
                ])
            }
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
                case_climate <- private$filter_case_climate(climate, case, by)
                case_reference <- if (nrow(reference) && length(reference_by)) {
                    private$filter_case_climate(reference, case, reference_by)
                } else {
                    reference
                }
                case_id <- morpher__hash(morph_id, morpher__json(as.list(case)))
                for (j in seq_len(nrow(rules))) {
                    rule <- rules[j]
                    target_epw_field <- rule$epw_field[[1L]]
                    target_variable_id <- morpher__rule_primary_variable(rule)
                    is_precip <- identical(
                        target_epw_field,
                        "liquid_precip_depth"
                    ) &&
                        identical(target_variable_id, "pr")
                    for (m in 1:12) {
                        future <- case_climate[which(
                            case_climate[["variable_id"]] ==
                                target_variable_id &
                                case_climate[["month"]] == m
                        )]
                        ref <- case_reference[which(
                            case_reference[["variable_id"]] ==
                                target_variable_id &
                                case_reference[["month"]] == m
                        )]
                        base <- baseline[which(
                            baseline[["epw_field"]] == target_epw_field &
                                baseline[["month"]] == m
                        )]
                        status <- if (!nrow(future)) {
                            "missing_climate"
                        } else if (isTRUE(external_reference) && !nrow(ref)) {
                            "missing_reference"
                        } else if (!nrow(base)) {
                            "missing_baseline"
                        } else {
                            "ok"
                        }
                        first_chr <- function(dt, col) {
                            if (nrow(dt)) {
                                store__chr1(dt[[col]][[1L]])
                            } else {
                                NA_character_
                            }
                        }
                        raw_base_units <- first_chr(base, "units")
                        base_units <- if (
                            is.na(raw_base_units) || !nzchar(raw_base_units)
                        ) {
                            morpher__default_epw_units(target_epw_field)
                        } else {
                            raw_base_units
                        }
                        converted <- morpher__convert_factor_values(
                            is_precip = is_precip,
                            status = status,
                            external_reference = external_reference,
                            future_value = morpher__pooled_mean(future),
                            future_units = first_chr(future, "units"),
                            future_years_json = if (nrow(future)) {
                                future$years_json[[1L]]
                            } else {
                                NA_character_
                            },
                            reference_value = morpher__pooled_mean(ref),
                            reference_units = first_chr(ref, "units"),
                            reference_years_json = if (nrow(ref)) {
                                ref$years_json[[1L]]
                            } else {
                                NA_character_
                            },
                            base_value = if (nrow(base)) {
                                base$value[[1L]]
                            } else {
                                NA_real_
                            },
                            base_units = base_units,
                            raw_base_units = raw_base_units,
                            month = m
                        )
                        status <- converted$status
                        future_value <- converted$future_value
                        base_value <- converted$base_value
                        base_units <- converted$base_units
                        # Persist the effective reference even in EPW-baseline
                        # mode so factors remain inspectable and reproducible.
                        reference_value <- if (isTRUE(external_reference)) {
                            converted$reference_value
                        } else {
                            base_value
                        }
                        delta <- if (
                            !is.na(future_value) && !is.na(reference_value)
                        ) {
                            future_value - reference_value
                        } else {
                            NA_real_
                        }
                        alpha <- if (
                            identical(status, "ok") &&
                                !is.na(reference_value) &&
                                !isTRUE(all.equal(reference_value, 0))
                        ) {
                            future_value / reference_value
                        } else {
                            NA_real_
                        }
                        row_case <- as.list(case)
                        rows[[length(rows) + 1L]] <- data.frame(
                            factor_id = morpher__hash(
                                morph_id,
                                case_id,
                                target_epw_field,
                                target_variable_id,
                                m
                            ),
                            morph_id = morph_id,
                            case_id = case_id,
                            epw_field = target_epw_field,
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
        # }}}

        # get_plan {{{
        get_plan = function(morph_id) {
            plan <- private$read_morph_rows("epw_morph_plan", morph_id)
            if (!nrow(plan)) {
                cli::cli_abort(
                    "Morphing plan ID {.val {morph_id}} was not found."
                )
            }
            plan[1L]
        },
        # }}}

        # set_plan_status {{{
        set_plan_status = function(morph_id, status, error = NA_character_) {
            plan <- private$get_plan(morph_id)
            plan$status <- status
            plan$updated_at <- morpher__now()
            plan$last_error <- store__chr1(error)
            morpher__replace_rows(
                private$store,
                "epw_morph_plan",
                plan,
                "morph_id"
            )
            invisible(NULL)
        },
        # }}}

        # Initialize one durable state row per morphing case. Completed result
        # manifests are authoritative during resume; every other case starts as
        # ready and is updated independently after its own attempt.
        # reset_case_statuses {{{
        reset_case_statuses = function(morph_id, cases, complete_cases) {
            cases <- data.table::as.data.table(data.table::copy(cases))
            rows <- vector("list", nrow(cases))
            for (i in seq_len(nrow(cases))) {
                case <- cases[i]
                case_id <- case$case_id[[1L]]
                rows[[i]] <- private$case_status_row(
                    morph_id,
                    case,
                    status = if (case_id %in% complete_cases) {
                        "completed"
                    } else {
                        "ready"
                    }
                )
            }
            morpher__delete_by_key(
                private$store,
                "epw_morph_case",
                "morph_id",
                morph_id
            )
            if (length(rows)) {
                morpher__replace_rows(
                    private$store,
                    "epw_morph_case",
                    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE),
                    "morph_case_id"
                )
            }
            invisible(NULL)
        },
        # }}}

        # Build one normalized case-state row from the scientific identity used
        # by the current morphing plan.
        # case_status_row {{{
        case_status_row = function(
            morph_id,
            case,
            status,
            error = NULL
        ) {
            case <- data.table::as.data.table(case)
            case_id <- case$case_id[[1L]]
            pick <- function(name) {
                if (name %in% names(case)) {
                    store__chr1(case[[name]][[1L]])
                } else {
                    NA_character_
                }
            }
            data.frame(
                morph_case_id = morpher__hash(morph_id, case_id),
                morph_id = morph_id,
                case_id = case_id,
                source_id = pick("source_id"),
                experiment_id = pick("experiment_id"),
                variant_label = pick("variant_label"),
                period = pick("period"),
                status = status,
                error_class = if (is.null(error)) {
                    NA_character_
                } else {
                    class(error)[[1L]]
                },
                last_error = if (is.null(error)) {
                    NA_character_
                } else {
                    conditionMessage(error)
                },
                updated_at = morpher__now(),
                stringsAsFactors = FALSE
            )
        },
        # }}}

        # Replace only the selected case row so a failure cannot overwrite the
        # state of completed or not-yet-attempted siblings.
        # set_case_status {{{
        set_case_status = function(morph_id, case, status, error = NULL) {
            row <- private$case_status_row(
                morph_id,
                case,
                status,
                error = error
            )
            morpher__replace_rows(
                private$store,
                "epw_morph_case",
                row,
                "morph_case_id"
            )
            invisible(row)
        },
        # }}}

        # Keep current morph diagnostics in a dedicated table. Shift run events
        # retain attempt history, while this table is replaced on each resumed
        # morph so inspectors do not confuse superseded failures with current
        # case state.
        # persist_case_diagnostics {{{
        persist_case_diagnostics = function(diagnostics) {
            diagnostics <- morpher__bind_diagnostics(diagnostics)
            if (!nrow(diagnostics)) {
                return(invisible(diagnostics))
            }
            # Components can report the same case-level warning more than once.
            # Its content-derived identity denotes one diagnostic, so retain a
            # single exact row before inserting primary keys. Distinct actions,
            # variables, months and cases remain separate records.
            diagnostics <- unique(diagnostics)
            rows <- data.table::copy(diagnostics)
            rows[,
                diagnostic_id := vapply(
                    seq_len(.N),
                    function(i) morpher__hash(as.list(rows[i])),
                    character(1L)
                )
            ]
            data.table::setcolorder(
                rows,
                c("diagnostic_id", morpher__diagnostic_columns())
            )
            morpher__replace_rows(
                private$store,
                "epw_morph_diagnostic",
                rows,
                "diagnostic_id"
            )
            invisible(diagnostics)
        },
        # }}}

        # Execute one scientific case and return its complete manifest rows.
        # Keeping this boundary inside the R6 engine gives run() a safe place to
        # catch one case failure without weakening validation inside backends.
        # execute_case {{{
        execute_case = function(
            morph_id,
            case,
            climate,
            reference_climate,
            observed_climate,
            by,
            reference_by,
            observed_by,
            existing,
            overwrite,
            resume
        ) {
            case_id <- case$case_id[[1L]]
            selected <- which(existing[["case_id"]] == case_id)
            existing_case <- existing[selected]
            reusable <- existing_case
            if (
                !isTRUE(overwrite) &&
                    isTRUE(resume) &&
                    morpher__result_case_complete(
                        reusable,
                        store_root = private$store$path
                    )
            ) {
                return(list(
                    rows = reusable,
                    diagnostics = morpher__empty_diagnostics(),
                    reused = TRUE
                ))
            }

            filter_role <- function(role_data, role_by, label) {
                if (is.null(role_data)) {
                    return(NULL)
                }
                filtered <- private$filter_case_climate(
                    role_data,
                    case,
                    role_by
                )
                if (!nrow(filtered)) {
                    cli::cli_abort(
                        "No {label} rows matched morphing case {.val {case_id}}."
                    )
                }
                filtered
            }
            case_climate <- filter_role(climate, by, "extracted climate")
            reference_case_climate <- filter_role(
                reference_climate,
                reference_by,
                "reference climate"
            )
            observed_case_climate <- filter_role(
                observed_climate,
                observed_by,
                "observed climate"
            )
            context <- morpher__context(
                epw = private$epw,
                climate = case_climate,
                reference_climate = reference_case_climate,
                observed_reference = observed_case_climate,
                recipe = private$recipe,
                by = by,
                case = case,
                strict = isTRUE(private$get_plan(morph_id)$strict[[1L]]),
                warning = FALSE
            )
            case_result <- morpher__run_context(context)
            diagnostics <- morpher__decorate_case_diagnostics(
                morpher__result_diagnostics(case_result),
                morph_id,
                case_id,
                case
            )
            member_records <- sequence__records(case_result)
            member_count <- length(member_records)
            process_reference <- morpher__process_write(
                private$store,
                case_result,
                private$get_plan(morph_id),
                case,
                diagnostics
            )
            # Partial recovery may retain intact weather members only when the
            # rerun produced the same scientific products and configuration.
            if (!overwrite && resume && nrow(existing_case)) {
                previous_reference <- morpher__process_reference(
                    existing_case,
                    private$store$path
                )
                if (
                    !identical(
                        previous_reference$sha256,
                        process_reference$sha256
                    )
                ) {
                    cli::cli_abort(
                        "Process data changed during partial recovery. Rerun with `overwrite = TRUE` to keep all members consistent."
                    )
                }
            }
            rows <- list()
            for (member in member_records) {
                case_data <- member$data
                if (!nrow(case_data)) {
                    cli::cli_abort(
                        "No morphed data were produced for morphing case {.val {case_id}}."
                    )
                }
                path <- private$morph_result_path(
                    morph_id,
                    case_id,
                    output_type = member$output_type,
                    sequence_id = member$sequence_id,
                    weather_year = member$weather_year
                )
                path_rel <- store_rel_path(path, root = private$store$path)
                selected <- which(
                    existing[["case_id"]] == case_id &
                        existing[["output_path"]] == path_rel
                )
                existing_member <- existing[selected]
                if (
                    !isTRUE(overwrite) &&
                        isTRUE(resume) &&
                        nrow(existing_member) == 1L &&
                        file.exists(path)
                ) {
                    rows[[length(rows) + 1L]] <- existing_member
                    next
                }
                if (file.exists(path) && !isTRUE(overwrite)) {
                    cli::cli_abort(
                        "Morph result already exists without a complete manifest row: {.path {path}}."
                    )
                }
                case_meta <- private$case_metadata_from_case(case, case_data)
                for (name in names(case_meta)) {
                    case_data[, (name) := case_meta[[name]]]
                }
                write_parquet_file(case_data, path)
                # Standard field roles supplement method-owned provenance so
                # every output exposes the same comparison boundary.
                provenance <- utils::modifyList(
                    member$provenance,
                    list(
                        process_data = process_reference,
                        weather_field_roles = morpher__weather_field_roles(
                            private$recipe
                        )
                    )
                )
                provenance_json <- as.character(morpher__json(provenance))
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
                        list(morph_id = morph_id, case_id = case_id),
                        identity
                    )
                )
                result_id <- if (
                    identical(
                        member$output_type,
                        "representative_year"
                    )
                ) {
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
                rows[[length(rows) + 1L]] <- data.frame(
                    result_id = result_id,
                    morph_id = morph_id,
                    case_id = case_id,
                    artifact_id = artifact_id,
                    output_path = path_rel,
                    row_count = nrow(case_data),
                    identity,
                    created_at = morpher__now(),
                    stringsAsFactors = FALSE
                )
            }
            list(
                rows = data.table::rbindlist(
                    rows,
                    use.names = TRUE,
                    fill = TRUE
                ),
                diagnostics = diagnostics,
                reused = FALSE
            )
        },
        # }}}

        # case_metadata_from_case {{{
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
        # }}}

        # case_metadata_from_result {{{
        case_metadata_from_result = function(dt) {
            fields <- c(
                "source_id",
                "experiment_id",
                "variant_label",
                "period"
            )
            stats::setNames(
                lapply(fields, function(name) {
                    if (name %in% names(dt)) {
                        store__chr1(dt[[name]][[1L]])
                    } else {
                        NA_character_
                    }
                }),
                fields
            )
        },
        # }}}

        # Keep legacy representative-year paths stable while sequence results
        # receive collision-free member and year partitions.
        # morph_result_path {{{
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
        # }}}
    )
)
# }}}

# morpher__identical_match {{{
morpher__identical_match <- function(x, value) {
    value <- store__chr1(value)
    if (is.na(value)) {
        return(is.na(x))
    }
    as.character(x) == value
}
# }}}

# vim: fdm=marker :
