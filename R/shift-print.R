#' @include shift-stage.R
NULL

# Render bounded workflow previews without changing execution state.

# display and conversion ------------------------------------------------------

# Parse the shared console controls accepted by modern Shift object printers.
# Unknown arguments fail early so misspelled display options are not ignored.
shift__print_options <- function(dots, default_n = 10L) {
    if (is.null(names(dots))) {
        names(dots) <- rep("", length(dots))
    }
    unknown <- setdiff(names(dots), c("n", "width", "verbose"))
    unknown <- unknown[nzchar(unknown)]
    if (any(!nzchar(names(dots))) || length(unknown)) {
        supplied <- c(names(dots)[!nzchar(names(dots))], unknown)
        supplied[!nzchar(supplied)] <- "<unnamed>"
        cli::cli_abort(
            "Unsupported print argument(s): {paste(supplied, collapse = ', ')}."
        )
    }

    n <- if ("n" %in% names(dots)) dots$n else default_n
    if (is.null(n)) {
        n <- Inf
    }
    checkmate::assert_number(n, lower = 1, finite = FALSE)
    if (!is.infinite(n)) {
        n <- as.integer(n)
    }
    width <- dots$width
    checkmate::assert_integerish(width, lower = 40L, len = 1L, null.ok = TRUE)
    verbose <- shift_coalesce(dots$verbose, FALSE)
    checkmate::assert_flag(verbose)
    list(
        n = n,
        width = if (is.null(width)) NULL else as.integer(width),
        verbose = verbose
    )
}

# Apply an explicit print width only for the duration of one object receipt.
shift__print_use_width <- function(width, env = parent.frame()) {
    if (is.null(width)) {
        return(invisible(NULL))
    }
    # `cli.width` takes precedence over base `width` in snapshot and redirected
    # output. Set both so an explicit print width remains authoritative in every
    # renderer, then restore the caller's complete option state on exit.
    old <- options(width = width, cli.width = width)
    withr::defer(options(old), envir = env)
    invisible(NULL)
}

# Format persisted timestamps whether DuckDB returns POSIXct or an ISO string.
shift__print_time <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1L]])) {
        return(NULL)
    }
    if (inherits(x[[1L]], "POSIXt")) {
        return(format(x[[1L]], tz = Sys.timezone(), usetz = TRUE))
    }
    as.character(x[[1L]])
}

# Apply the shared Shift receipt vocabulary on top of the established ESGF
# header renderer without changing the lower-level query/result presentation.
shift__print_header <- function(title) {
    esg__print_header(title)
}

# Render semantic Shift facts with the same bullet rhythm as ESGF receipts.
# Values are formatted by callers so scientific concepts remain class-aware.
shift__print_facts <- function(x) {
    esg__print_facts(x)
}

# Compress integer years into readable consecutive ranges so period specs do
# not expand into one console row per year.
shift__format_years <- function(years) {
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    if (!length(years)) {
        return(NULL)
    }
    groups <- cumsum(c(TRUE, diff(years) != 1L))
    ranges <- split(years, groups)
    paste(
        vapply(
            ranges,
            function(value) {
                if (length(value) == 1L) {
                    as.character(value)
                } else {
                    sprintf("%d\u2013%d", value[[1L]], value[[length(value)]])
                }
            },
            character(1L)
        ),
        collapse = ", "
    )
}

# Format normalized period tables and named year lists through one compact
# representation shared by plans, references, and extracted climate stages.
shift__format_periods <- function(periods) {
    if (is.null(periods)) {
        return(NULL)
    }
    if (is.list(periods) && !is.data.frame(periods)) {
        if (is.null(names(periods))) {
            return(shift__format_years(unlist(periods, use.names = FALSE)))
        }
        return(paste(
            vapply(
                names(periods),
                function(name) {
                    sprintf("%s %s", name, shift__format_years(periods[[name]]))
                },
                character(1L)
            ),
            collapse = " \u00b7 "
        ))
    }
    periods <- data.table::as.data.table(periods)
    if (!all(c("period", "year") %in% names(periods)) || !nrow(periods)) {
        return(NULL)
    }
    labels <- unique(as.character(periods$period))
    paste(
        vapply(
            labels,
            function(label) {
                sprintf(
                    "%s %s",
                    label,
                    shift__format_years(periods[period == label, year])
                )
            },
            character(1L)
        ),
        collapse = " \u00b7 "
    )
}

# Describe an optional workflow reference without exposing its full S7 object,
# extraction metadata, or one-row-per-year period table.
shift__format_reference <- function(reference, recipe = NULL) {
    if (is.null(reference)) {
        if (
            !is.null(recipe) &&
                isTRUE(morpher__recipe_accepts_reference(recipe))
        ) {
            return("baseline EPW")
        }
        return("none")
    }
    if (S7::S7_inherits(reference, ShiftReferenceSpec)) {
        periods <- shift__format_periods(reference@periods)
        parts <- c(reference@role, reference@mode, periods)
        parts <- parts[!is.na(parts) & nzchar(parts)]
        return(paste(parts, collapse = " \u00b7 "))
    }
    if (S7::S7_inherits(reference, ShiftReanalysisSpec)) {
        return(sprintf(
            "%s \u00b7 %s \u00b7 %d\u2013%d",
            toupper(reference@dataset),
            reference@product,
            min(reference@years),
            max(reference@years)
        ))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return("supplied ShiftClimate")
    }
    class(reference)[[1L]]
}

# Display unresolved workflow selections explicitly instead of letting NULL
# disappear from a compact receipt.
shift__format_auto <- function(x) {
    shift_coalesce(shift__display_values(x), "auto")
}

# Format the public method name together with its persisted compatibility
# profile. Earlier original-morphing specs did not carry a profile and remain
# visibly legacy when rendered without first reconstructing the recipe.
shift__format_morph_method <- function(
    name,
    recipe = NULL,
    missing_original_morphing_profile = NULL
) {
    name <- as.character(shift_coalesce(name, "method"))[[1L]]
    backend <- as.character(shift_coalesce(recipe$backend, name))[[1L]]
    profile <- recipe$profile
    if (
        (is.null(profile) || !length(profile)) &&
            backend %in% c("original_morphing", "original_morphing_absolute")
    ) {
        profile <- missing_original_morphing_profile
    }
    if (
        is.null(profile) ||
            !length(profile) ||
            is.na(profile[[1L]]) ||
            !nzchar(as.character(profile[[1L]]))
    ) {
        return(name)
    }
    sprintf("%s [%s]", name, as.character(profile[[1L]]))
}

# Describe scalar table forcing and named per-variable overrides distinctly.
# This makes the automatic Amon/LImon routing visible without expanding the
# complete recipe variable map in normal receipts.
shift__format_cmip6_tables <- function(table) {
    if (is.null(table) || !length(table)) {
        return("auto by variable")
    }
    if (is.list(table) && !is.data.frame(table)) {
        table <- unlist(table, use.names = TRUE)
    }
    table_names <- names(table)
    table <- as.character(table)
    names(table) <- table_names
    named <- !is.null(names(table)) && any(nzchar(names(table)))
    if (!named) {
        return(sprintf("%s (forced)", shift__display_values(table, max = Inf)))
    }
    overrides <- paste(
        sprintf("%s=%s", names(table), table),
        collapse = " \u00b7 "
    )
    sprintf("auto by variable \u00b7 %s", overrides)
}

# Render scalar and variable-specific frequency specifications without losing
# the distinction between CMIP6 interval means and point samples.
shift__format_cmip6_frequencies <- function(frequency) {
    if (is.null(frequency) || !length(frequency)) {
        return(NULL)
    }
    if (is.list(frequency) && !is.data.frame(frequency)) {
        frequency <- unlist(frequency, use.names = TRUE)
    }
    frequency_names <- names(frequency)
    frequency <- as.character(frequency)
    names(frequency) <- frequency_names
    if (is.null(frequency_names) || !any(nzchar(frequency_names))) {
        return(shift__display_values(frequency, max = Inf))
    }
    paste(
        sprintf("%s=%s", frequency_names, frequency),
        collapse = " | "
    )
}

# Render the exact table/grid partitions selected for download and extraction.
# `grid_label` remains a compatibility summary, while `partition_key` is the
# authoritative multi-table identity persisted by the resolver.
shift__format_cmip6_partitions <- function(selection) {
    selection <- data.table::as.data.table(shift_coalesce(
        selection,
        data.table::data.table()
    ))
    if (!nrow(selection)) {
        return(NULL)
    }
    keys <- if ("partition_key" %in% names(selection)) {
        as.character(selection$partition_key)
    } else {
        character()
    }
    keys <- unique(keys[!is.na(keys) & nzchar(keys)])
    if (
        !length(keys) &&
            all(
                c("table_id", "grid_label") %in%
                    names(selection)
            )
    ) {
        rows <- unique(selection[, .(table_id, grid_label)])
        rows <- rows[
            !is.na(table_id) &
                nzchar(table_id) &
                !is.na(grid_label) &
                nzchar(grid_label)
        ]
        if (nrow(rows)) {
            data.table::setorderv(rows, c("table_id", "grid_label"))
            keys <- paste(
                paste(rows$table_id, rows$grid_label, sep = "="),
                collapse = ";"
            )
        }
    }
    if (!length(keys)) {
        return(NULL)
    }
    paste(gsub(";", " \u00b7 ", keys, fixed = TRUE), collapse = " / ")
}

# Format named provider or workflow option lists without printing nested
# environments or arbitrary objects by structure.
shift__format_options <- function(x) {
    if (is.null(x) || !length(x)) {
        return(NULL)
    }
    values <- vapply(
        names(x),
        function(name) {
            value <- x[[name]]
            if (is.atomic(value)) {
                sprintf(
                    "%s=%s",
                    name,
                    shift_coalesce(shift__display_values(value), "<empty>")
                )
            } else {
                sprintf("%s=<%s>", name, class(value)[[1L]])
            }
        },
        character(1L)
    )
    paste(values, collapse = " \u00b7 ")
}

# Read optional persisted data for a receipt and return a printable diagnostic
# rather than making print() fail when a store is temporarily unavailable.
shift__print_store_read <- function(x, reader) {
    opened <- tryCatch(shift_store(x), error = identity)
    if (inherits(opened, "condition")) {
        return(list(
            data = data.table::data.table(),
            error = conditionMessage(opened)
        ))
    }
    on.exit(try(opened$close(), silent = TRUE), add = TRUE)
    value <- tryCatch(reader(opened), error = identity)
    if (inherits(value, "condition")) {
        return(list(
            data = data.table::data.table(),
            error = conditionMessage(value)
        ))
    }
    list(data = data.table::as.data.table(value), error = NULL)
}

# Render a bounded, width-aware table preview and preserve the total row count
# in the continuation hint even when only the requested rows were materialized.
shift__print_table <- function(
    x,
    title,
    columns,
    n = 10L,
    total_rows = NULL,
    empty = "No rows.",
    more_hint = "use the corresponding shift_*() inspector for all rows."
) {
    checkmate::assert_string(title, min.chars = 1L)
    x <- data.table::as.data.table(shift_coalesce(x, data.table::data.table()))
    if (is.null(total_rows)) {
        total_rows <- nrow(x)
    }
    cli::cli_rule(title)
    if (!nrow(x)) {
        cli::cli_alert_info(empty)
        return(invisible(NULL))
    }
    shown <- if (is.infinite(n)) x else utils::head(x, n)
    epwshiftr_cli_render_table(
        shown,
        columns = columns,
        max_rows = if (is.infinite(n)) nrow(shown) else n,
        show_types = FALSE,
        more_hint = more_hint,
        hidden_hint = "Use the corresponding shift_*() inspector for all columns.",
        total_rows = as.integer(total_rows)
    )
    invisible(NULL)
}

# Print a consistent stage heading and status fact before class-specific
# scientific context is added.
shift__print_stage_intro <- function(x, title, facts = list()) {
    shift__print_header(title)
    shift__print_facts(c(
        list(
            "Status" = tryCatch(shift_status(x), error = function(e) "unknown")
        ),
        facts
    ))
    invisible(NULL)
}

# Render optional workflow provenance after the scientific query/result view.
shift__print_workflow <- function(x, verbose = FALSE) {
    ids <- shift_ids(x)
    diagnostics <- shift_diagnostics(x)
    if (isTRUE(verbose)) {
        cli::cli_rule("Workflow")
        esg__print_facts(list(
            "Status" = tryCatch(shift_status(x), error = function(e) "unknown"),
            "Store" = shift__display_path(x@store_path),
            "Query ID" = ids$query_id,
            "Run ID" = ids$run_id,
            "Step ID" = ids$step_id
        ))
    }
    if (nrow(diagnostics)) {
        counts <- table(diagnostics$severity)
        cli::cli_rule("Diagnostics")
        esg__print_facts(list(
            "Counts" = paste(
                sprintf("%s %s", counts, names(counts)),
                collapse = " \u00b7 "
            )
        ))
    }
    invisible(NULL)
}

# Print a ShiftRequest through the same canonical parameter renderer as
# EsgQuery while retaining the workflow's explicit auto-node semantics.
shift__print_request <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    query <- shift_as_query(x)
    state <- query$state()
    pinned_node <- x@meta$options$index_node
    node <- if (is.null(pinned_node)) "auto" else query$index_node()
    esg__print_query(node, state$parameter, title = "ESGF request")
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Print a persisted ShiftFiles catalog as an ESGF result receipt plus a
# width-aware table preview, without reading the complete catalog into R.
shift__print_files <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    ids <- shift_ids(x)
    result_fields <- unique(as.character(x@meta$result_fields))
    result_fields <- result_fields[
        !is.na(result_fields) & nzchar(result_fields)
    ]
    store <- tryCatch(shift_store(x), error = identity)
    if (inherits(store, "condition")) {
        # A detached or temporarily unavailable store must not make the object
        # itself unprintable. Preserve the established result header and expose
        # only metadata already cached on the ShiftFiles handle.
        request <- shift_stage_root(x)
        node <- if (!is.null(request)) {
            shift_coalesce(request@meta$options$index_node, "auto")
        } else {
            "unavailable"
        }
        fields <- if (length(result_fields)) {
            cli::format_inline("{length(result_fields)} | [ {result_fields} ]")
        } else {
            "unavailable"
        }
        esg__print_header("ESGF Query Result [File]")
        esg__print_facts(list(
            "Index Node" = node,
            "Result count" = shift_coalesce(x@meta$file_count, "unavailable"),
            "Fields" = fields
        ))
        if (!is.null(request)) {
            query <- shift_as_query(request)
            esg__print_parameters(query$state()$parameter)
        }
        cli::cli_rule("Files")
        cli::cli_alert_info(
            "Cached File rows are not available on this handle."
        )
        shift__print_store_notice(conditionMessage(store))
        shift__print_workflow(x, verbose = verbose)
        return(invisible(x))
    }
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    summary <- shift__file_catalog_summary(store, ids$query_id)
    if (!nrow(summary)) {
        summary <- data.table::data.table(
            file_count = 0L,
            total_size = 0
        )
    }
    summary <- summary[1L]
    runs <- shift_query_run(store, ids$query_id)
    run <- if (nrow(runs)) runs[1L] else data.table::data.table()
    file_count <- as.integer(summary$file_count[[1L]])
    created <- if (nrow(run)) shift__print_time(run$created_at) else NULL
    node <- if (nrow(run)) run$index_node[[1L]] else NULL
    if (!length(result_fields)) {
        # Stages created before response fields were persisted fall back to
        # the stable catalog preview schema rather than reading every record.
        result_fields <- names(shift__file_catalog_preview(
            store,
            ids$query_id,
            n = 1L
        ))
    }
    fields <- if (length(result_fields)) {
        # cli's vector interpolation matches the established EsgResultFile
        # punctuation and wrapping, including the final conjunction.
        cli::format_inline("{length(result_fields)} | [ {result_fields} ]")
    } else {
        "0"
    }

    esg__print_header("ESGF Query Result [File]")
    facts <- list(
        "Index Node" = node,
        "Collected at" = created,
        "Result count" = format(file_count, big.mark = ",", scientific = FALSE),
        "Total size" = format_size_units(summary$total_size[[1L]]),
        "Fields" = fields
    )
    esg__print_facts(facts)

    request <- shift_stage_root(x)
    if (!is.null(request)) {
        query <- shift_as_query(request)
        esg__print_parameters(query$state()$parameter)
    }

    cli::cli_rule("Files")
    if (file_count < 1L) {
        cli::cli_alert_info(
            "No matching file records. Review the ESGF query constraints and collect again."
        )
    } else {
        preview <- shift__file_catalog_preview(store, ids$query_id, n = n)
        epwshiftr_cli_render_table(
            preview,
            columns = c(
                "source_id",
                "experiment_id",
                "variable_id",
                "variant_label",
                "grid_label",
                "table_id",
                "datetime_start",
                "datetime_end",
                "size",
                "filename",
                "data_node"
            ),
            max_rows = if (is.infinite(n)) nrow(preview) else n,
            show_types = FALSE,
            more_hint = "use `shift_files()` for all records.",
            hidden_hint = "Use `shift_files()` for all columns.",
            total_rows = file_count
        )
    }
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Describe an EPW input by its stable path when available, falling back to the
# adapter class rather than dumping an R6 or external Epw object.
shift__format_epw <- function(epw, full = FALSE) {
    if (is.null(epw)) {
        return(NULL)
    }
    path <- if (is.character(epw) && length(epw) == 1L) {
        epw
    } else {
        tryCatch(epw_file_coerce(epw)$path(), error = function(e) NULL)
    }
    if (is.null(path)) {
        return(class(epw)[[1L]])
    }
    if (isTRUE(full)) {
        normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
    } else {
        basename(path)
    }
}

# Add a non-fatal store-read notice after a cached object summary so temporary
# filesystem problems remain visible without masking the object itself.
shift__print_store_notice <- function(error) {
    if (is.null(error) || !nzchar(error)) {
        return(invisible(NULL))
    }
    cli::cli_rule("Diagnostics")
    cli::cli_alert_warning("Persisted preview unavailable: {error}")
    invisible(NULL)
}

# Render the deferred Future EPW intent and expected case matrix without
# resolving ESGF nodes or mutating the plan.
shift__print_plan <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    meta <- x@meta
    climate <- meta$climate
    request <- meta$request@meta
    transform <- meta$transform
    model <- if (!is.null(climate)) climate@model else request$source
    scenarios <- if (!is.null(climate)) {
        climate@scenarios
    } else {
        request$experiment
    }
    member <- if (!is.null(climate)) climate@member else request$variant
    grid <- if (!is.null(climate)) climate@grid else request$filters$grid_label
    climate_parts <- c(
        shift__display_values(model),
        shift__display_values(scenarios)
    )
    climate_parts <- climate_parts[
        !is.na(climate_parts) &
            nzchar(climate_parts)
    ]
    cases <- data.table::copy(data.table::as.data.table(meta$expected_cases))
    if ("years" %in% names(cases)) {
        cases[, years := vapply(years, shift__format_years, character(1L))]
    }

    shift__print_stage_intro(
        x,
        "Future EPW Plan",
        list(
            "Climate" = paste(climate_parts, collapse = " \u00b7 "),
            "Periods" = shift__format_periods(meta$periods),
            "Transform" = transform@label,
            "Reference" = shift__format_reference(
                meta$reference,
                meta$recipe
            ),
            "Observed reference" = shift__format_reference(
                meta$observed_reference
            ),
            "Selection" = sprintf(
                "member %s \u00b7 grid %s \u00b7 tables %s",
                shift__format_auto(member),
                shift__format_auto(grid),
                shift__format_cmip6_tables(
                    if (!is.null(climate)) {
                        climate@table
                    } else {
                        request$filters$table_id
                    }
                )
            ),
            "Expected outputs" = nrow(cases),
            "Output directory" = shift__display_path(meta$epw$export_dir)
        )
    )
    if (isTRUE(verbose)) {
        nodes <- if (!is.null(climate)) {
            climate@index_nodes
        } else {
            request$options$index_node
        }
        control <- meta$control
        cli::cli_rule("Discovery")
        shift__print_facts(list(
            "Frequency" = shift__format_cmip6_frequencies(
                if (!is.null(climate) && !is.null(climate@frequency)) {
                    climate@frequency
                } else {
                    request$frequency
                }
            ),
            "Table" = shift__format_cmip6_tables(
                if (!is.null(climate)) {
                    climate@table
                } else {
                    request$filters$table_id
                }
            ),
            "Index nodes" = shift__display_values(nodes, max = Inf),
            "Download" = control@download,
            "Remote refresh" = control@refresh,
            "Partial outputs" = control@allow_partial,
            "Output layout" = control@output_layout
        ))
    }
    shift__print_table(
        cases,
        "Expected outputs",
        columns = c(
            "source_id",
            "experiment_id",
            "variant_label",
            "grid_label",
            "period",
            "years",
            "status",
            "missing_reason"
        ),
        n = n,
        empty = "No expected output cases.",
        more_hint = "use `shift_cases()` for all expected cases."
    )
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Summarize persistent download task state and expose only a bounded task table
# in the default console receipt.
shift__print_download <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    ids <- shift_ids(x)
    cached <- if (is.data.frame(x@meta$session)) {
        data.table::as.data.table(x@meta$session)
    } else {
        data.table::data.table()
    }
    read <- if (nrow(cached)) {
        list(data = cached, error = NULL)
    } else {
        shift__print_store_read(x, function(store) {
            if (is.null(ids$session_id) || is.na(ids$session_id)) {
                return(data.table::data.table())
            }
            store$download_status(session_id = ids$session_id)
        })
    }
    tasks <- read$data
    counts <- if (nrow(tasks) && "status" %in% names(tasks)) {
        table(tasks$status)
    } else {
        integer()
    }
    complete <- if (nrow(tasks) && "status" %in% names(tasks)) {
        sum(tasks$status %in% c("done", "skipped", "verified"))
    } else {
        0L
    }
    bytes_done <- if ("bytes_done" %in% names(tasks)) {
        sum(tasks$bytes_done, na.rm = TRUE)
    } else {
        0
    }
    bytes_total <- if ("size" %in% names(tasks)) {
        sum(tasks$size, na.rm = TRUE)
    } else {
        0
    }

    shift__print_stage_intro(
        x,
        "CMIP6 Download",
        list(
            "Session" = ids$session_id,
            "Tasks" = if (nrow(tasks)) {
                sprintf(
                    "%d/%d complete%s",
                    complete,
                    nrow(tasks),
                    if (length(counts)) {
                        sprintf(
                            " \u00b7 %s",
                            paste(
                                sprintf("%s %d", names(counts), counts),
                                collapse = " \u00b7 "
                            )
                        )
                    } else {
                        ""
                    }
                )
            } else {
                "none"
            },
            "Transfer" = if (bytes_total > 0) {
                sprintf(
                    "%s / %s",
                    format_size_units(bytes_done),
                    format_size_units(bytes_total)
                )
            } else {
                NULL
            }
        )
    )
    shift__print_table(
        tasks,
        "Tasks",
        columns = c(
            "status",
            "filename",
            "bytes_done",
            "size",
            "speed_bps",
            "eta_seconds",
            "data_node",
            "attempts",
            "last_error"
        ),
        n = n,
        empty = "No download tasks are registered.",
        more_hint = "use `shift_data()` or the Downloader inspectors for all tasks."
    )
    shift__print_store_notice(read$error)
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Summarize extraction coverage by scientific identity while keeping the full
# plan and extracted time-series data behind their dedicated inspectors.
shift__print_climate <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    cached <- data.table::as.data.table(shift_coalesce(
        x@meta$coverage,
        data.table::data.table()
    ))
    read <- if (nrow(cached)) {
        list(data = cached, error = NULL)
    } else {
        ids <- shift_ids(x)
        shift__print_store_read(x, function(store) {
            store$coverage(plan_id = ids$plan_id)
        })
    }
    coverage <- read$data
    site <- tryCatch(shift_target(x), error = function(e) NULL)
    complete <- if (nrow(coverage) && "complete" %in% names(coverage)) {
        sum(coverage$complete %in% TRUE)
    } else {
        0L
    }
    rows <- if ("output_rows" %in% names(coverage)) {
        sum(coverage$output_rows, na.rm = TRUE)
    } else {
        0
    }

    shift__print_stage_intro(
        x,
        "Extracted Climate",
        list(
            "Site" = if (!is.null(site)) {
                shift_coalesce(site@label, site@id)
            } else {
                NULL
            },
            "Periods" = shift__format_periods(x@meta$periods),
            "Coverage" = sprintf("%d/%d complete", complete, nrow(coverage)),
            "Variables" = if ("variable_id" %in% names(coverage)) {
                shift__display_values(unique(coverage$variable_id))
            } else {
                NULL
            },
            "Rows" = if (rows > 0) {
                format(rows, big.mark = ",", scientific = FALSE)
            } else {
                NULL
            }
        )
    )
    shift__print_table(
        coverage,
        "Coverage",
        columns = c(
            "complete",
            "status",
            "experiment_id",
            "variable_id",
            "variant_label",
            "grid_label",
            "time_start",
            "time_stop",
            "output_time_count",
            "output_rows",
            "last_error"
        ),
        n = n,
        empty = "No extraction coverage is available.",
        more_hint = "use `shift_coverage()` for all extraction plans."
    )
    shift__print_store_notice(read$error)
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Select the most informative available morph result source in a deterministic
# order so old and resumed stages remain printable across process boundaries.
shift__morph_print_rows <- function(x) {
    cached <- data.table::as.data.table(shift_coalesce(
        x@meta$results,
        data.table::data.table()
    ))
    if (nrow(cached)) {
        return(list(data = cached, error = NULL))
    }
    ids <- shift_ids(x)
    persisted <- shift__print_store_read(x, function(store) {
        shift_morph_result_rows(store, ids$morph_id)
    })
    if (nrow(persisted$data)) {
        return(persisted)
    }
    plan <- data.table::as.data.table(shift_coalesce(
        x@meta$plan,
        data.table::data.table()
    ))
    if (nrow(plan)) {
        persisted$data <- plan
    }
    persisted
}

# Render weather-transform/reference identity and a bounded result/case preview
# without printing hourly morphed weather data.
shift__print_morphed <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    recipe <- x@meta$recipe
    transform <- x@meta$transform
    read <- shift__morph_print_rows(x)
    rows <- read$data
    case_count <- if ("case_id" %in% names(rows)) {
        data.table::uniqueN(rows$case_id)
    } else {
        nrow(rows)
    }
    reference <- shift_coalesce(x@meta$reference_spec, x@meta$reference)
    transform_label <- if (S7::S7_inherits(transform, WeatherTransformSpec)) {
        transform@label
    } else {
        # Older persisted stages may not contain the public transform record.
        shift__format_morph_method(
            shift_coalesce(recipe$name, recipe$backend),
            recipe,
            missing_original_morphing_profile = "legacy"
        )
    }

    shift__print_stage_intro(
        x,
        "Morphed EPW",
        list(
            "Transform" = transform_label,
            "Reference" = shift__format_reference(reference, recipe),
            "Cases" = case_count,
            "Results" = nrow(rows)
        )
    )
    shift__print_table(
        rows,
        "Morph results",
        columns = c(
            "case_id",
            "source_id",
            "experiment_id",
            "variant_label",
            "period",
            "status",
            "row_count",
            "output_path",
            "last_error"
        ),
        n = n,
        empty = "No morph results are available.",
        more_hint = "use `shift_data()` or `shift_artifacts()` for complete morph data."
    )
    shift__print_store_notice(read$error)
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Render generated and exported EPW paths by user case while keeping weather
# rows behind shift_data().
shift__print_outputs_stage <- function(
    x,
    n = 10L,
    width = NULL,
    verbose = FALSE
) {
    shift__print_use_width(width)
    outputs <- data.table::as.data.table(shift_coalesce(
        x@meta$outputs,
        data.table::data.table()
    ))
    read_error <- NULL
    if (!nrow(outputs)) {
        read <- shift__print_store_read(x, function(store) {
            shift_epw_output_rows(store, shift_ids(x)$morph_id)
        })
        outputs <- read$data
        read_error <- read$error
    }
    paths <- intersect(c("export_path", "path", "output_path"), names(outputs))
    existing <- if (length(paths) && nrow(outputs)) {
        path <- outputs[[paths[[1L]]]]
        sum(!is.na(path) & nzchar(path))
    } else {
        0L
    }

    shift__print_stage_intro(
        x,
        "EPW Outputs",
        list(
            "Outputs" = sprintf(
                "%d registered \u00b7 %d path%s",
                nrow(outputs),
                existing,
                if (existing == 1L) "" else "s"
            ),
            "Export directory" = if (!is.null(x@meta$export_dir)) {
                shift__display_path(x@meta$export_dir)
            } else {
                NULL
            }
        )
    )
    shift__print_table(
        outputs,
        "Outputs",
        columns = c(
            "source_id",
            "experiment_id",
            "variant_label",
            "period",
            "path",
            "export_path",
            "created_at"
        ),
        n = n,
        empty = "No EPW outputs are registered.",
        more_hint = "use `shift_outputs()` for all output records."
    )
    shift__print_store_notice(read_error)
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Render a site target as scientific context rather than exposing its inherited
# ShiftStage storage fields.
shift__print_site <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("EPW Site")
    shift__print_facts(list(
        "ID" = x@id,
        "Label" = x@label,
        "Coordinates" = sprintf("%.6f, %.6f", x@lon, x@lat),
        "EPW" = shift__format_epw(x@epw, full = verbose)
    ))
    if (isTRUE(verbose) && length(x@metadata)) {
        cli::cli_rule("Metadata")
        shift__print_facts(list("Values" = shift__format_options(x@metadata)))
    }
    shift__print_workflow(x, verbose = verbose)
    invisible(x)
}

# Render a complete CMIP6 scientific specification without listing every
# failover URL unless verbose output was explicitly requested.
shift__print_cmip6 <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("CMIP6 Climate")
    shift__print_facts(list(
        "Model" = if (is.null(x@model)) {
            if (is.null(x@n_models)) {
                "auto (all compatible models)"
            } else {
                sprintf("auto (%d models)", x@n_models)
            }
        } else {
            shift__display_values(x@model)
        },
        "Scenarios" = shift__display_values(x@scenarios),
        "Member" = shift__format_auto(x@member),
        "Grid" = shift__format_auto(x@grid),
        "Frequency" = if (is.null(x@frequency)) {
            "inferred by weather method"
        } else {
            shift__format_cmip6_frequencies(x@frequency)
        },
        "Table" = shift__format_cmip6_tables(x@table),
        "Activity" = x@activity,
        "Index nodes" = sprintf("%d-node failover", length(x@index_nodes)),
        "Data node" = shift__format_auto(x@data_node)
    ))
    if (isTRUE(verbose)) {
        nodes <- data.table::data.table(
            priority = seq_along(x@index_nodes),
            index_node = x@index_nodes
        )
        shift__print_table(
            nodes,
            "Discovery",
            c("priority", "index_node"),
            n = n,
            more_hint = "increase `n` to show every index node."
        )
        if (length(x@filters)) {
            cli::cli_rule("Filters")
            shift__print_facts(list(
                "Values" = shift__format_options(x@filters)
            ))
        }
    }
    invisible(x)
}

# Render workflow control policy as explicit semantic choices instead of a raw
# S7 property dump.
shift__print_control <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("Shift Control")
    shift__print_facts(list(
        "Strict" = x@strict,
        "Allow partial" = x@allow_partial,
        "Download" = x@download,
        "Resume" = x@resume,
        "Overwrite" = x@overwrite,
        "Remote refresh" = x@refresh,
        "Extraction" = x@extraction_method,
        "Output layout" = x@output_layout
    ))
    invisible(x)
}

# Render a reference specification with compact periods and keep provider and
# stage option detail behind verbose output.
shift__print_reference <- function(x, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    shift__print_header("Climate Reference")
    shift__print_facts(list(
        "Mode" = x@mode,
        "Role" = x@role,
        "Periods" = shift__format_periods(x@periods),
        "Plan IDs" = shift__display_values(x@plan_id),
        "Experiment" = x@experiment,
        "Activity" = x@activity,
        "Match" = shift__display_values(x@match)
    ))
    if (isTRUE(verbose)) {
        details <- list(
            "Filters" = shift__format_options(x@filters),
            "Options" = shift__format_options(x@options),
            "Collect" = shift__format_options(x@collect),
            "Extract" = shift__format_options(x@extract)
        )
        if (
            any(vapply(
                details,
                function(value) {
                    !is.null(value) &&
                        nzchar(value)
                },
                logical(1L)
            ))
        ) {
            cli::cli_rule("Workflow options")
            shift__print_facts(details)
        }
    }
    invisible(x)
}

# Bound a dashboard table after it has been rendered so ShiftRun can honour the
# same `n` contract without duplicating the watch renderer's table semantics.
shift__print_view_rows <- function(lines, n, width, label) {
    if (!length(lines) || is.infinite(n)) {
        return(lines)
    }
    # Resolver and case views both own a title plus one header row. Preserve
    # those rows and limit only the underlying business records.
    prefix <- min(2L, length(lines))
    records <- max(0L, length(lines) - prefix)
    if (records <= n) {
        return(lines)
    }
    hint <- shift__ui_fit(
        sprintf("  \u2026 %d more %s", records - n, label),
        width
    )
    c(lines[seq_len(prefix + n)], cli::style_dim(hint))
}

# Print one non-animated snapshot through the same state/view pipeline used by
# foreground completion receipts and shift_watch(). A failed refresh falls back
# to the handle's cached snapshot and is reported after the dashboard.
shift__print_run <- function(x, n = 10L, width = NULL, verbose = FALSE) {
    shift__print_use_width(width)
    refresh_error <- NULL
    run <- x
    # A cached cross-session handle without store identity cannot be refreshed.
    # Do not silently fall back to the user's default store, which may refer to
    # an unrelated run or produce an environment-specific filesystem error.
    if (is.null(x@store_path) || !nzchar(x@store_path)) {
        refresh_error <- "No store is associated with this cached run."
    } else {
        refreshed <- tryCatch(shift_refresh(x), error = identity)
        if (inherits(refreshed, "condition")) {
            refresh_error <- conditionMessage(refreshed)
        } else {
            run <- refreshed
        }
    }
    view <- tryCatch(
        shift__ui_run_view(
            run,
            width = shift__ui_width(width),
            detail = if (isTRUE(verbose)) "detail" else "normal",
            motion = "none"
        ),
        error = identity
    )
    if (inherits(view, "condition")) {
        # A failed preview must not retry the unavailable store to obtain an
        # identifier that is already present on the cached handle.
        shift__print_stage_intro(
            run,
            "Shift Run",
            list(
                "Run" = run@ids$run_id,
                "Stage" = run@meta$run$current_stage,
                "Snapshot" = "cached metadata only"
            )
        )
        refresh_error <- paste(
            c(refresh_error, conditionMessage(view)),
            collapse = "; "
        )
    } else {
        view$nodes <- shift__print_view_rows(
            view$nodes,
            n,
            shift__ui_width(width),
            "resolver attempt(s)"
        )
        view$cases <- shift__print_view_rows(
            view$cases,
            n,
            shift__ui_width(width),
            "case(s)"
        )
        shift__ui_print_view(view, include_tables = TRUE)
    }
    shift__print_store_notice(refresh_error)
    invisible(x)
}

# ShiftRequest has a query-oriented static receipt rather than the generic
# internal stage dump used by data-processing stages.
S7::method(print, ShiftRequest) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_request(x, width = opts$width, verbose = opts$verbose)
}

# ShiftFiles combines the shared ESGF result hierarchy with a semantic CMIP6
# catalog preview whose row count and terminal width are user-controllable.
S7::method(print, ShiftFiles) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_files(
        x,
        n = opts$n,
        width = opts$width,
        verbose = opts$verbose
    )
}

# ShiftPlan prints immutable scientific intent and its expected case contract;
# it never invokes the resolver or touches remote services.
S7::method(print, ShiftPlan) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_plan(x, n = opts$n, width = opts$width, verbose = opts$verbose)
}

# ShiftRun reuses the static dashboard view so print, watch, and foreground
# completion receipts cannot drift in status or diagnostic wording.
S7::method(print, ShiftRun) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_run(x, n = opts$n, width = opts$width, verbose = opts$verbose)
}

# ShiftDownload prints persistent transfer state without starting or resuming a
# Downloader job.
S7::method(print, ShiftDownload) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_download(
        x,
        n = opts$n,
        width = opts$width,
        verbose = opts$verbose
    )
}

# ShiftClimate prints coverage plans rather than materializing extracted
# Parquet weather rows.
S7::method(print, ShiftClimate) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_climate(
        x,
        n = opts$n,
        width = opts$width,
        verbose = opts$verbose
    )
}

# ShiftMorphed prints result identity and artifacts, leaving hourly weather
# values behind shift_data().
S7::method(print, ShiftMorphed) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_morphed(
        x,
        n = opts$n,
        width = opts$width,
        verbose = opts$verbose
    )
}

# ShiftOutputs prints generated/exported paths by user case.
S7::method(print, ShiftOutputs) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_outputs_stage(
        x,
        n = opts$n,
        width = opts$width,
        verbose = opts$verbose
    )
}

# ShiftCmip6Spec prints the complete future climate identity while collapsing
# failover nodes until verbose output is requested.
S7::method(print, ShiftCmip6Spec) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_cmip6(
        x,
        n = opts$n,
        width = opts$width,
        verbose = opts$verbose
    )
}

# ShiftControl prints the workflow-wide policy choices that cannot be
# overridden by individual stage option lists.
S7::method(print, ShiftControl) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_control(x, width = opts$width, verbose = opts$verbose)
}

# ShiftReferenceSpec prints compact reference periods and identity rather than
# its raw S7 properties.
S7::method(print, ShiftReferenceSpec) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_reference(x, width = opts$width, verbose = opts$verbose)
}

# Extension ShiftStage classes without a dedicated print method still receive
# the shared receipt hierarchy instead of the historical angle-bracket dump.
S7::method(print, ShiftStage) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_use_width(opts$width)
    shift__print_stage_intro(
        x,
        "Shift Stage",
        list(
            "Class" = class(x)[[1L]],
            "Stage" = x@stage
        )
    )
    shift__print_workflow(x, verbose = opts$verbose)
    invisible(x)
}

# ShiftSite prints user-facing geographic and EPW identity.
S7::method(print, ShiftSite) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_site(x, width = opts$width, verbose = opts$verbose)
}
