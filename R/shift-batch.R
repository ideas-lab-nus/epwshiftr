#' @include shift-stage.R cmip6-availability.R
NULL

# Ergonomic future-weather batches ------------------------------------------

# ShiftBatch groups independently resumable future-weather plans or runs. It
# adds no comparison statistics and retains each method's ordinary ShiftStage
# result as the authoritative execution record.
ShiftBatch <- S7::new_class("ShiftBatch", parent = ShiftStage)

# Convert a registry row to the same immutable transform returned by the
# scale-specific public constructors.
shift_batch__transform_from_row <- function(row) {
    reconstruction <- if (is.na(row$reconstruction[[1L]])) {
        NULL
    } else {
        row$reconstruction[[1L]]
    }
    transform__new(
        row$scale[[1L]],
        row$method[[1L]],
        reconstruction = reconstruction
    )
}

# Resolve concise method keys through the public registry. Keys with multiple
# valid configurations remain explicit errors instead of silently selecting a
# scale or reconstruction that the caller did not request.
shift_batch__transforms <- function(methods = NULL, transform = NULL) {
    if (!is.null(methods) && !is.null(transform)) {
        cli::cli_abort("Supply either `methods` or `transform`, not both.")
    }
    if (!is.null(transform)) {
        explicit <- if (S7::S7_inherits(transform, WeatherTransformSpec)) {
            list(transform)
        } else if (is.list(transform) && length(transform) &&
            all(vapply(transform, function(value) {
                S7::S7_inherits(value, WeatherTransformSpec)
            }, logical(1L)))) {
            transform
        } else {
            cli::cli_abort(
                "`transform` must contain one or more WeatherTransformSpec objects. Use `methods` for method keys."
            )
        }
        keys <- vapply(explicit, shift_batch__transform_key, character(1L))
        if (anyDuplicated(keys)) {
            cli::cli_abort("Every selected weather transform must be unique.")
        }
        return(stats::setNames(explicit, keys))
    }
    values <- shift_coalesce(methods, transform)
    if (is.null(values)) {
        cli::cli_abort(
            "Supply `methods` or one explicit weather `transform`."
        )
    }
    if (is.character(values)) {
        if (!length(values) || anyNA(values) || any(!nzchar(values)) ||
            anyDuplicated(values)) {
            cli::cli_abort(
                "`methods` must contain unique, non-empty method keys."
            )
        }
        catalog <- weather_transforms()
        values <- lapply(values, function(method) {
            target_method <- method
            rows <- catalog[catalog[["method"]] == target_method]
            if (!nrow(rows)) {
                cli::cli_abort(c(
                    "Unknown weather method {.val {method}}.",
                    "i" = "Use {.fn weather_transforms} to list method keys."
                ))
            }
            if (nrow(rows) != 1L) {
                cli::cli_abort(c(
                    "Weather method {.val {method}} has more than one configuration.",
                    "i" = paste(
                        "Use the applicable scale-specific constructor and",
                        "pass its result through `transform`."
                    )
                ))
            }
            shift_batch__transform_from_row(rows)
        })
    } else {
        cli::cli_abort(
            "`methods` must contain one or more method keys."
        )
    }
    keys <- vapply(values, shift_batch__transform_key, character(1L))
    if (anyDuplicated(keys)) {
        cli::cli_abort("Every selected weather method must be unique.")
    }
    stats::setNames(values, keys)
}

# Build a stable, path-safe identity that distinguishes scale and optional
# reconstruction while retaining the concise published method key in outputs.
shift_batch__transform_key <- function(transform) {
    parts <- c(transform@scale, transform@method)
    if (!is.na(transform@reconstruction) &&
        nzchar(transform@reconstruction)) {
        parts <- c(parts, transform@reconstruction)
    }
    paste(parts, collapse = "-")
}

# Keep user-facing output and store paths portable across supported platforms.
shift_batch__path_component <- function(value) {
    value <- gsub("[^A-Za-z0-9._-]+", "-", as.character(value))
    value <- gsub("^-+|-+$", "", value)
    ifelse(nzchar(value), value, "item")
}

# Extract the model-future role and fail early when a registered recipe cannot
# describe the source variables needed for model discovery.
shift_batch__future_requirement <- function(transform) {
    requirement <- transform@required_inputs[["model_future"]]
    if (is.null(requirement)) {
        requirement <- transform@optional_inputs[["model_future"]]
    }
    if (is.null(requirement) || !length(requirement@variable_sets)) {
        cli::cli_abort(
            "Weather method {.val {transform@method}} has no model-future variable contract."
        )
    }
    requirement
}

# Subset a named table override to one variable alternative while retaining a
# scalar table pin unchanged.
shift_batch__table_spec <- function(table, variables) {
    if (is.null(table) || is.null(names(table))) {
        return(table)
    }
    table[intersect(names(table), variables)]
}

# Query one variable alternative with index-node failover and return complete
# model/member/grid identities together with the selected frequency and table.
shift_batch__available_alternative <- function(
    climate,
    transform,
    variables,
    include_historical,
    store,
    ui
) {
    frequency <- shift__transform_cmip6_frequencies(
        transform,
        variables,
        climate@frequency
    )
    table <- shift_batch__table_spec(climate@table, variables)
    member <- if (is.null(climate@member)) "r1i1p1f1" else climate@member
    if (is.null(climate@model) &&
        !identical(as.character(member), "r1i1p1f1")) {
        cli::cli_abort(
            "Automatic model discovery requires member `r1i1p1f1`."
        )
    }
    collect <- getOption(
        "epwshiftr.cmip6.availability",
        shift_cmip6_avail
    )
    if (!is.function(collect)) {
        cli::cli_abort("Configured CMIP6 availability adapter must be a function.")
    }
    attempts <- list()
    errors <- character()
    for (node in climate@index_nodes) {
        current <- tryCatch(
            collect(
                variables = variables,
                scenarios = climate@scenarios,
                include_historical = include_historical,
                source = climate@model,
                member = member,
                grid = climate@grid,
                frequency = frequency,
                table = table,
                activity = climate@activity,
                index_node = node,
                data_node = climate@data_node,
                filters = climate@filters,
                store = store,
                ui = ui
            ),
            error = function(error) error
        )
        if (inherits(current, "error")) {
            errors <- c(errors, conditionMessage(current))
            next
        }
        current <- data.table::as.data.table(current)
        current <- current[complete %in% TRUE]
        if (nrow(current)) {
            attempts[[length(attempts) + 1L]] <- current
            break
        }
    }
    if (!length(attempts)) {
        if (length(errors) == length(climate@index_nodes)) {
            cli::cli_abort(c(
                "CMIP6 availability discovery failed at every configured index node.",
                "x" = unique(errors)
            ))
        }
        return(data.table::data.table())
    }
    result <- attempts[[1L]]
    result[, identity := paste(
        source_id,
        variant_label,
        grid_label,
        sep = "\r"
    )]
    result[]
}

# Resolve every method independently across its declared variable alternatives,
# then intersect identities so all selected methods use the same model member
# and grid for a fair downstream comparison performed by the user.
shift_batch__discover_models <- function(
    climate,
    transforms,
    store,
    ui,
    reference = NULL
) {
    by_transform <- lapply(transforms, function(transform) {
        # Historical availability is a property of this method's actual input
        # roles. A historical requirement in another selected method must not
        # constrain variables that this method uses only in the future period.
        include_historical <- transform__requires_input(
            transform,
            "model_historical"
        ) || (!is.null(reference) && transform__accepts_input(
            transform,
            "model_historical"
        ))
        requirement <- shift_batch__future_requirement(transform)
        alternatives <- lapply(
            seq_along(requirement@variable_sets),
            function(index) {
                variables <- as.character(requirement@variable_sets[[index]])
                result <- shift_batch__available_alternative(
                    climate,
                    transform,
                    variables,
                    include_historical = include_historical,
                    store = store,
                    ui = ui
                )
                if (nrow(result)) {
                    result[, `:=`(
                        alternative = index,
                        selected_variables = list(variables)
                    )]
                }
                result
            }
        )
        candidates <- data.table::rbindlist(
            alternatives,
            use.names = TRUE,
            fill = TRUE
        )
        if (!nrow(candidates)) {
            return(candidates)
        }
        data.table::setorderv(
            candidates,
            c("alternative", "source_id", "variant_label", "grid_label")
        )
        candidates[!duplicated(identity)]
    })
    empty <- names(by_transform)[!vapply(by_transform, nrow, integer(1L))]
    if (length(empty)) {
        cli::cli_abort(
            "No complete CMIP6 identity satisfies weather method(s) {.val {empty}}."
        )
    }
    common <- Reduce(intersect, lapply(by_transform, `[[`, "identity"))
    if (!length(common)) {
        cli::cli_abort(c(
            "No common CMIP6 model/member/grid identity satisfies every selected weather method.",
            "i" = "Inspect each method's variable and frequency contract with weather_transforms()."
        ))
    }
    identities <- by_transform[[1L]][identity %in% common]
    data.table::setorderv(
        identities,
        c("source_id", "variant_label", "grid_label")
    )
    identities <- identities[!duplicated(source_id)]
    if (is.null(climate@model)) {
        # A NULL internal count is the explicit public `model = NULL` request
        # for every compatible identity; a numeric count keeps batch expansion
        # bounded and fails instead of silently returning fewer models.
        if (!is.null(climate@n_models)) {
            if (nrow(identities) < climate@n_models) {
                cli::cli_abort(
                    "Only {nrow(identities)} complete common CMIP6 model(s) are available; {climate@n_models} were requested."
                )
            }
            identities <- utils::head(identities, climate@n_models)
        }
    } else {
        missing <- setdiff(climate@model, identities$source_id)
        if (length(missing)) {
            cli::cli_abort(
                "Explicit CMIP6 model(s) lack a common complete identity: {.val {missing}}."
            )
        }
        identities <- identities[match(climate@model, source_id)]
    }
    if (!nrow(identities)) {
        cli::cli_abort("CMIP6 model discovery did not select any complete model.")
    }
    list(identities = identities[], candidates = by_transform)
}

# Resolve the filesystem root without retaining an open EsgStore connection in
# a batch object whose children may run in separate background processes.
shift_batch__store_root <- function(store) {
    path <- if (inherits(store, "EsgStore")) {
        store$path
    } else {
        shift_coalesce(store, store_dir(init = FALSE))
    }
    normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

# Construct one method/model-specific climate spec pinned to the identity
# selected across all methods. The selected table mapping remains specific to
# that method while source, member, and grid are shared.
shift_batch__child_climate <- function(
    climate,
    transform_key,
    identity,
    candidates
) {
    target_identity <- identity[["identity"]]
    candidate <- candidates[[transform_key]][
        identity == target_identity
    ][1L]
    table <- candidate$table[[1L]]
    shift_cmip6(
        model = identity$source_id[[1L]],
        scenarios = climate@scenarios,
        member = identity$variant_label[[1L]],
        grid = identity$grid_label[[1L]],
        frequency = climate@frequency,
        table = table,
        activity = climate@activity,
        index_nodes = climate@index_nodes,
        data_node = climate@data_node,
        filters = climate@filters
    )
}

# Route references only to methods that declare the corresponding semantic
# role, creating a conventional historical model period when one is required.
shift_batch__references <- function(
    transform,
    reference,
    calibration
) {
    model_reference <- if (transform__accepts_input(
        transform,
        "model_historical"
    )) {
        reference
    } else {
        NULL
    }
    if (is.null(model_reference) && transform__requires_input(
        transform,
        "model_historical"
    )) {
        years <- if (S7::S7_inherits(calibration, ShiftReanalysisSpec)) {
            calibration@years
        } else {
            1995:2014
        }
        model_reference <- historical_reference(years)
    }
    observed_reference <- if (transform__accepts_input(
        transform,
        "observed_reference"
    )) {
        calibration
    } else {
        NULL
    }
    transform__validate_execution_inputs(
        transform,
        model_reference,
        observed_reference
    )
    list(
        reference = model_reference,
        observed_reference = observed_reference
    )
}

# Build or execute one child through the existing single-method workflow. This
# function is intentionally a thin adapter so scientific computation continues
# to have one implementation in shift__future_epw_one().
shift_batch__child <- function(
    epw,
    climate,
    periods,
    transform,
    dir,
    reference,
    observed_reference,
    control,
    ui,
    store,
    dry_run,
    background
) {
    shift__future_epw_one(
        epw = epw,
        climate = climate,
        periods = periods,
        transform = transform,
        dir = dir,
        reference = reference,
        observed_reference = observed_reference,
        control = control,
        ui = ui,
        store = store,
        dry_run = dry_run,
        background = background
    )
}

# Create the high-level method-by-model matrix and keep all child plans/runs
# addressable without adding package-owned method comparison calculations.
shift_batch__future_epw <- function(
    epw,
    climate,
    periods,
    transforms,
    dir,
    reference,
    calibration,
    control,
    ui,
    store,
    dry_run,
    background
) {
    if (!S7::S7_inherits(climate, ShiftCmip6Spec)) {
        cli::cli_abort("`climate` must be created by {.fn shift_cmip6}.")
    }
    periods <- shift__periods_from_input(periods)
    output_root <- normalizePath(
        path.expand(dir),
        winslash = "/",
        mustWork = FALSE
    )
    store_root <- shift_batch__store_root(store)
    site_identity <- shift__epw_identity(epw)
    references <- lapply(transforms, function(transform) {
        shift_batch__references(transform, reference, calibration)
    })
    reference_intent <- lapply(references, function(value) {
        list(
            model_historical = shift__reference_spec_value(
                value$reference,
                "model_historical"
            ),
            observed_reference = shift__reference_spec_value(
                value$observed_reference,
                "observed_reference"
            )
        )
    })
    batch_id <- store__hash(
        "shift-batch-v1",
        site_identity$checksum,
        shift__climate_spec_value(climate),
        lapply(transforms, transform__spec_value),
        split(periods$year, periods$period),
        reference_intent
    )
    batch_root <- file.path(store_root, "batches", batch_id)
    discovery <- shift_batch__discover_models(
        climate,
        transforms,
        store = file.path(batch_root, "discovery"),
        ui = ui,
        reference = reference
    )
    children <- list()
    manifest <- list()
    index <- 0L
    for (model_index in seq_len(nrow(discovery$identities))) {
        identity <- discovery$identities[model_index]
        for (transform_key in names(transforms)) {
            index <- index + 1L
            transform <- transforms[[transform_key]]
            child_climate <- shift_batch__child_climate(
                climate,
                transform_key,
                identity,
                discovery$candidates
            )
            child_references <- references[[transform_key]]
            child_key <- paste(
                transform_key,
                identity$source_id[[1L]],
                sep = "--"
            )
            child_component <- shift_batch__path_component(child_key)
            child <- shift_batch__child(
                epw = epw,
                climate = child_climate,
                periods = periods,
                transform = transform,
                dir = file.path(output_root, child_component),
                reference = child_references$reference,
                observed_reference = child_references$observed_reference,
                control = control,
                ui = ui,
                store = file.path(batch_root, child_component),
                dry_run = dry_run,
                background = background
            )
            children[[child_key]] <- child
            manifest[[index]] <- data.table::data.table(
                child_key = child_key,
                method = transform@method,
                scale = transform@scale,
                reconstruction = transform@reconstruction,
                model = identity$source_id[[1L]],
                member = identity$variant_label[[1L]],
                grid = identity$grid_label[[1L]],
                calibration_used = !is.null(
                    child_references$observed_reference
                ),
                store = child@store_path,
                output_dir = file.path(output_root, child_component)
            )
        }
    }
    manifest <- data.table::rbindlist(
        manifest,
        use.names = TRUE,
        fill = TRUE
    )
    shift_stage_new(
        ShiftBatch,
        "batch",
        store_path = batch_root,
        ids = list(
            batch_id = batch_id,
            child_ids = lapply(children, function(child) child@ids)
        ),
        meta = list(
            children = children,
            manifest = manifest,
            periods = periods,
            climate = climate,
            selected_models = discovery$identities,
            output_dir = output_root,
            dry_run = dry_run
        ),
        diagnostics = shift_batch__diagnostics(
            children,
            manifest,
            refresh = FALSE
        )
    )
}

# Add stable child identity columns to an inspector result without altering the
# child's native columns or ordering.
shift_batch__decorate <- function(data, row) {
    data <- data.table::as.data.table(data.table::copy(data))
    if (!nrow(data)) {
        return(data)
    }
    data[, `:=`(
        child_key = row$child_key[[1L]],
        method = row$method[[1L]],
        scale = row$scale[[1L]],
        reconstruction = row$reconstruction[[1L]],
        model = row$model[[1L]],
        member = row$member[[1L]],
        grid = row$grid[[1L]]
    )]
    front <- c(
        "child_key", "method", "scale", "reconstruction", "model",
        "member", "grid"
    )
    data.table::setcolorder(data, c(front, setdiff(names(data), front)))
    data[]
}

# Run one inspector for every child and combine only non-empty tabular results.
shift_batch__inspect <- function(children, manifest, fun) {
    rows <- lapply(seq_along(children), function(index) {
        value <- fun(children[[index]])
        shift_batch__decorate(value, manifest[index])
    })
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Collect child diagnostics while preserving their canonical diagnostic fields.
shift_batch__diagnostics <- function(
    children,
    manifest,
    severity = NULL,
    refresh = TRUE
) {
    rows <- lapply(seq_along(children), function(index) {
        diagnostics <- tryCatch(
            shift_diagnostics(
                children[[index]],
                severity = severity,
                refresh = refresh
            ),
            error = function(error) {
                shift_diagnostic(
                    "batch",
                    "error",
                    "batch_child_inspection_failed",
                    conditionMessage(error),
                    action = "Inspect or resume the affected child run."
                )
            }
        )
        shift_batch__decorate(diagnostics, manifest[index])
    })
    diagnostics <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
    if (!is.null(severity) && nrow(diagnostics)) {
        diagnostics <- diagnostics[severity %in% ..severity]
    }
    diagnostics[]
}

# Refresh each persisted child independently; dry-run plans remain unchanged.
shift_batch__refresh <- function(x) {
    children <- lapply(seq_along(x@meta$children), function(index) {
        child <- x@meta$children[[index]]
        tryCatch(
            shift_refresh(child),
            error = function(error) {
                row <- x@meta$manifest[index]
                child@diagnostics <- shift_bind_diagnostics(
                    child@diagnostics,
                    shift_diagnostic(
                        "batch",
                        "error",
                        "batch_child_refresh_failed",
                        sprintf(
                            "Could not refresh child %s: %s",
                            row$child_key[[1L]],
                            conditionMessage(error)
                        ),
                        action = "Inspect or resume the affected child run."
                    )
                )
                child
            }
        )
    })
    names(children) <- names(x@meta$children)
    x@meta$children <- children
    x@diagnostics <- shift_batch__diagnostics(
        children,
        x@meta$manifest,
        refresh = FALSE
    )
    x@ids$child_ids <- lapply(children, function(child) child@ids)
    x
}

# Aggregate child statuses without hiding a failed, blocked, or active run.
shift_batch__status <- function(x, refresh = TRUE) {
    if (isTRUE(refresh)) {
        x <- shift_batch__refresh(x)
    }
    statuses <- vapply(x@meta$children, function(child) {
        shift_status(child, refresh = FALSE)
    }, character(1L))
    if (!length(statuses)) {
        return("empty")
    }
    if (length(unique(statuses)) == 1L) {
        return(statuses[[1L]])
    }
    precedence <- c(
        "failed", "blocked", "stopping", "running", "queued", "waiting",
        "planned", "partial", "cancelled", "completed"
    )
    selected <- precedence[precedence %in% statuses]
    if (length(selected)) selected[[1L]] else "partial"
}

# Resume plans and interrupted runs independently, leaving active and completed
# child runs untouched.
shift_batch__resume <- function(x, background = FALSE, ui = shift_ui()) {
    children <- lapply(x@meta$children, function(child) {
        status <- shift_status(child, refresh = TRUE)
        if (S7::S7_inherits(child, ShiftPlan)) {
            return(shift_run(child, background = background, ui = ui))
        }
        if (status %in% c("completed", "queued", "running", "stopping",
            "waiting")) {
            return(child)
        }
        shift_resume(child, background = background, ui = ui)
    })
    x@meta$children <- children
    shift_batch__refresh(x)
}

# Request cancellation only for children that are not already terminal.
shift_batch__cancel <- function(x, force = FALSE) {
    children <- lapply(x@meta$children, function(child) {
        if (!S7::S7_inherits(child, ShiftRun)) {
            return(child)
        }
        status <- shift_status(child, refresh = TRUE)
        if (status %in% c("completed", "partial", "failed", "cancelled")) {
            return(child)
        }
        shift_cancel(child, force = force)
    })
    x@meta$children <- children
    shift_batch__refresh(x)
}

# Render a compact receipt for the selected method-by-model matrix.
S7::method(print, ShiftBatch) <- function(x, ...) {
    opts <- shift__print_options(list(...))
    shift__print_use_width(opts$width)
    manifest <- data.table::copy(x@meta$manifest)
    manifest[, status := vapply(x@meta$children, function(child) {
        shift_status(child, refresh = FALSE)
    }, character(1L))]
    shift__print_stage_intro(x, "Future EPW Batch", list(
        "Batch" = x@ids$batch_id,
        "Methods" = data.table::uniqueN(manifest$method),
        "Models" = data.table::uniqueN(manifest$model),
        "Children" = nrow(manifest),
        "Output directory" = shift__display_path(x@meta$output_dir)
    ))
    shift__print_table(
        manifest,
        "Method and model runs",
        c("method", "scale", "model", "member", "grid", "status"),
        n = opts$n,
        more_hint = "use `shift_cases()` for all child cases."
    )
    invisible(x)
}

# Validate every child and combine diagnostics under the batch identity.
S7::method(shift_check, ShiftBatch) <- function(
    x,
    strict = FALSE,
    network = FALSE,
    ...
) {
    checkmate::assert_flag(strict)
    checkmate::assert_flag(network)
    diagnostics <- shift_batch__diagnostics(
        x@meta$children,
        x@meta$manifest,
        refresh = FALSE
    )
    if (isTRUE(strict) && any(diagnostics$severity %in% "error")) {
        shift_abort_diagnostics(diagnostics)
    }
    diagnostics[]
}
