#' @include shift-stage.R
NULL

# Match extraction rows to one resolved CMIP identity without relying on
# data.table's NA comparison behaviour. The same helper is used for manifest
# coverage and Parquet data so a derived artifact cannot cross scenarios,
# members, grids, or sites.
# shift_climate__humidity_identity_match {{{
shift_climate__humidity_identity_match <- function(rows, identity, columns) {
    keep <- rep(TRUE, nrow(rows))
    for (column in intersect(columns, names(rows))) {
        keep <- keep &
            shift_resolve__catalog_match(
                rows[[column]],
                identity[[column]][[1L]]
            )
    }
    keep
}
# }}}

# Persist canonical hurs extraction plans and Parquet artifacts when a resolved
# identity has no direct hurs but has complete huss, tas, and ps inputs. This
# occurs before task-level coverage, so strict coverage and EpwMorpher consume
# the same durable canonical evidence on initial and resumed runs.
# shift_climate__derive_hurs_climate {{{
shift_climate__derive_hurs_climate <- function(
    climate,
    recipe,
    overwrite = FALSE,
    resume = TRUE,
    reporter = NULL
) {
    if (!S7::S7_inherits(climate, ShiftClimate)) {
        cli::cli_abort("`climate` must be a {.cls ShiftClimate} stage.")
    }
    checkmate::assert_flag(overwrite)
    checkmate::assert_flag(resume)
    if (
        inherits(recipe, "epw_morph_recipe") &&
            identical(recipe$backend, "hourly_kernel_qdm")
    ) {
        # This workflow must derive HURS after HUSS, TAS, and PS have been
        # reconstructed to the common hourly lattice.
        return(climate)
    }
    requirements <- morpher__variable_requirements(recipe)
    humidity_alternatives <- requirements[["hurs"]]
    if (
        is.null(humidity_alternatives) ||
            !any(vapply(
                humidity_alternatives,
                function(value) {
                    identical(as.character(value), c("huss", "tas", "ps"))
                },
                logical(1L)
            ))
    ) {
        return(climate)
    }

    store <- shift_store(climate)
    private <- priv(store)
    coverage <- store$coverage(plan_id = climate@ids$plan_id)
    coverage <- coverage[complete %in% TRUE]
    if (!nrow(coverage)) {
        return(climate)
    }
    identity_columns <- intersect(
        c(
            "source_id",
            "experiment_id",
            "variant_label",
            "grid_label",
            "frequency",
            "table_id",
            "site_id"
        ),
        names(coverage)
    )
    identities <- unique(coverage[, identity_columns, with = FALSE])
    raw <- NULL
    derived_ids <- character()
    provenance <- list()

    for (i in seq_len(nrow(identities))) {
        identity <- identities[i]
        rows <- coverage[
            shift_climate__humidity_identity_match(
                coverage,
                identity,
                identity_columns
            )
        ]
        # Direct hurs is always preferred, even when the alternative source
        # variables were returned by the broad capability query.
        if (any(rows$variable_id == "hurs" & rows$complete %in% TRUE)) {
            next
        }
        inputs <- c("huss", "tas", "ps")
        source_rows <- lapply(inputs, function(variable) {
            rows[variable_id == variable & complete %in% TRUE]
        })
        # A zero-row data.table still has a non-zero length because `length()`
        # counts columns. Check rows so optional table partitions without the
        # three humidity inputs are skipped instead of being derived.
        if (!all(vapply(source_rows, nrow, integer(1L)) > 0L)) {
            next
        }
        source_plan_ids <- sort(unique(unlist(
            lapply(source_rows, function(value) value$plan_id),
            use.names = FALSE
        )))
        derived_plan_id <- store__hash(
            "derived-hurs-v1",
            paste(source_plan_ids, collapse = "\r")
        )
        existing <- tryCatch(
            store$coverage(plan_id = derived_plan_id),
            error = function(e) data.table::data.table()
        )
        if (
            !isTRUE(overwrite) &&
                isTRUE(resume) &&
                nrow(existing) &&
                all(existing$complete %in% TRUE)
        ) {
            derived_ids <- c(derived_ids, derived_plan_id)
            provenance[[length(provenance) + 1L]] <- list(
                plan_id = derived_plan_id,
                derived_from = inputs,
                source_plan_ids = source_plan_ids,
                reused = TRUE
            )
            if (!is.null(reporter)) {
                reporter$notice(
                    "Reused derived hurs from huss + tas + ps",
                    outcome = "skipped",
                    details = list(
                        unit_type = "derived_variable",
                        variable = "hurs"
                    )
                )
            }
            next
        }

        if (is.null(raw)) {
            # Derivation must read every source partition. `shift_data()` is a
            # preview API by default and would otherwise stop after 100 rows,
            # often before tas and ps partitions are reached.
            raw <- shift_data(climate, n = Inf, variables = inputs)
        }
        data_rows <- raw[
            shift_climate__humidity_identity_match(
                raw,
                identity,
                identity_columns
            )
        ]
        data_rows <- data_rows[plan_id %in% source_plan_ids]
        derived <- morpher__derive_hurs_rows(data_rows)
        if (!nrow(derived)) {
            cli::cli_abort(
                "Derived hurs produced no rows for the resolved CMIP identity.",
                class = "epwshiftr_hurs_derivation_error"
            )
        }

        huss_row <- source_rows[[1L]][1L]
        now <- store__now()
        plan <- data.frame(
            plan_id = derived_plan_id,
            query_id = huss_row$query_id[[1L]],
            file_key = huss_row$file_key[[1L]],
            site_id = huss_row$site_id[[1L]],
            variable_id = "hurs",
            lon = huss_row$lon[[1L]],
            lat = huss_row$lat[[1L]],
            method = huss_row$method[[1L]],
            time_start = min(derived$time, na.rm = TRUE),
            time_stop = max(derived$time, na.rm = TRUE),
            status = "done",
            available_time_count = data.table::uniqueN(derived$time),
            attempt_count = 1L,
            last_error = NA_character_,
            created_at = now,
            updated_at = now,
            stringsAsFactors = FALSE
        )
        file_catalog <- data.table::as.data.table(
            private$read_table("file_catalog")
        )
        file <- file_catalog[
            file_catalog[["file_key"]] == plan$file_key[[1L]]
        ][1L]
        if (!nrow(file)) {
            cli::cli_abort(
                "Cannot persist derived hurs because its source file catalog row is missing."
            )
        }
        derived[, `:=`(
            plan_id = derived_plan_id,
            file_key = plan$file_key[[1L]],
            query_id = plan$query_id[[1L]],
            method = plan$method[[1L]]
        )]

        # Write the plan before its result rows so a crash leaves an explicit,
        # resumable incomplete plan instead of an orphaned Parquet artifact.
        private$replace_rows("extraction_plan", plan, "plan_id")
        private$delete_by_key("extraction_result", "plan_id", derived_plan_id)
        results <- private$write_extract_partitions(
            derived,
            data.table::as.data.table(plan),
            file,
            overwrite = overwrite
        )
        private$replace_rows(
            "extraction_result",
            as.data.frame(results),
            "result_id"
        )
        derived_ids <- c(derived_ids, derived_plan_id)
        provenance[[length(provenance) + 1L]] <- list(
            plan_id = derived_plan_id,
            derived_from = inputs,
            source_plan_ids = source_plan_ids,
            equation = "e=q*p/(epsilon+(1-epsilon)*q); hurs=100*e/pws(tas)",
            reused = FALSE
        )
        if (!is.null(reporter)) {
            reporter$notice(
                "Derived hurs from huss + tas + ps",
                outcome = "completed",
                details = list(
                    unit_type = "derived_variable",
                    variable = "hurs",
                    rows = nrow(derived)
                )
            )
        }
    }

    if (!length(derived_ids)) {
        return(climate)
    }
    climate@ids$plan_id <- unique(c(climate@ids$plan_id, derived_ids))
    climate@meta$coverage <- store$coverage(plan_id = climate@ids$plan_id)
    climate@meta$variables <- unique(c(climate@meta$variables, "hurs"))
    climate@meta$derived_variables <- provenance
    climate
}
# }}}

# vim: fdm=marker :
