#' @include shift-stage.R cmip6-availability.R
NULL

# Ergonomic future-weather batches ------------------------------------------

# ShiftBatch groups independently resumable future-weather plans or runs. It
# adds no comparison statistics and retains each method's ordinary ShiftStage
# result as the authoritative execution record.
ShiftBatch <- S7::new_class("ShiftBatch", parent = ShiftStage)

# Resolve one unqualified method key from lightweight static records and build
# only its selected transform. Counting every reconstruction preserves the
# existing ambiguity rule across both scales and reconstruction choices.
shift_batch__transform_from_method <- function(method, records) {
    matches <- Filter(
        function(record) identical(record$method, method),
        records
    )
    configurations <- sum(vapply(
        matches,
        function(record) length(record$reconstructions),
        integer(1L)
    ))
    if (!configurations) {
        cli::cli_abort(c(
            "Unknown weather method {.val {method}}.",
            "i" = "Use {.fn weather_transforms} to list method keys."
        ))
    }
    if (configurations != 1L) {
        cli::cli_abort(c(
            "Weather method {.val {method}} has more than one configuration.",
            "i" = paste(
                "Use the applicable scale-specific constructor and",
                "pass its result through `transform`."
            )
        ))
    }

    record <- matches[[1L]]
    transform__new(record$scale, record$method)
}

# Resolve concise method keys through the canonical transform records. Keys
# with multiple valid configurations remain explicit errors instead of
# silently selecting a scale or reconstruction that the caller did not request.
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
        records <- transform__records()
        values <- lapply(values, function(method) {
            shift_batch__transform_from_method(method, records)
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

# Keep batch selection and child-run identities in one small durable receipt.
# Scientific and weather artifacts remain authoritative in each child store;
# this sidecar only avoids repeating remote discovery to find those stores.
shift_batch__receipt_path <- function(batch_root) {
    file.path(batch_root, "batch-receipt.rds")
}

# Read only receipts created for the requested stable batch identity. A stale,
# interrupted, or older-format sidecar is a cache miss rather than a workflow
# failure because child stores still contain the durable run records.
shift_batch__receipt_read <- function(batch_root, batch_id) {
    path <- shift_batch__receipt_path(batch_root)
    if (!file.exists(path)) {
        return(NULL)
    }
    receipt <- tryCatch(readRDS(path), error = identity)
    required <- c(
        "version", "batch_id", "discovery", "manifest", "children",
        "output_dir", "status"
    )
    if (inherits(receipt, "error") || !is.list(receipt) ||
        !all(required %in% names(receipt)) ||
        !identical(receipt$version, 1L) ||
        !identical(receipt$batch_id, batch_id) ||
        !is.list(receipt$discovery) ||
        !all(c("identities", "candidates") %in% names(receipt$discovery))) {
        return(NULL)
    }
    receipt
}

# Publish the batch sidecar after planning and after every resume. Temporary
# files are removed on interruption without touching child stores or any
# user-selected output directory.
shift_batch__receipt_write <- function(x) {
    path <- shift_batch__receipt_path(x@store_path)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    children <- lapply(names(x@meta$children), function(child_key) {
        child <- x@meta$children[[child_key]]
        list(
            child_key = child_key,
            store_path = child@store_path,
            run_id = if (S7::S7_inherits(child, ShiftRun)) {
                store__chr1(child@ids$run_id)
            } else {
                NA_character_
            },
            plan = if (S7::S7_inherits(child, ShiftPlan)) {
                shift__plan_spec(child)
            } else {
                NULL
            },
            status = shift_status(child, refresh = FALSE)
        )
    })
    receipt <- list(
        version = 1L,
        batch_id = x@ids$batch_id,
        discovery = x@meta$discovery,
        manifest = data.table::copy(x@meta$manifest),
        children = children,
        output_dir = x@meta$output_dir,
        status = shift_batch__status(x, refresh = FALSE),
        climate = shift__climate_spec_value(x@meta$climate),
        periods = data.table::copy(x@meta$periods),
        updated_at = Sys.time()
    )
    temporary <- tempfile(
        pattern = paste0(basename(path), "-"),
        tmpdir = dirname(path)
    )
    on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
    saveRDS(receipt, temporary, version = 3L, compress = FALSE)
    if (file.exists(path)) {
        unlink(path)
    }
    if (!file.rename(temporary, path)) {
        cli::cli_abort("Could not publish the future-weather batch receipt.")
    }
    invisible(path)
}

#' Read a persisted future-weather batch
#'
#' @description
#' Reopen a batch and its independent child plans or runs without querying
#' climate catalogs. New dry-run batches retain their child plans; completed
#' receipts from earlier versions remain readable through their child runs.
#' @param batch_id Batch identifier returned by [shift_ids()].
#' @param store Root store used for [shift_future_epw()], or the batch directory.
#' @return A `ShiftBatch` accepted by the ordinary `shift_*()` inspectors,
#'   [shift_run()], [shift_resume()], [shift_cancel()], and [shift_watch()].
#' @export
shift_batch_get <- function(batch_id, store = NULL) {
    checkmate::assert_string(batch_id, min.chars = 1L,
        pattern = "^[A-Za-z0-9_-]+$")
    root <- shift_batch__store_root(store)
    batch_root <- if (identical(basename(root), batch_id)) {
        root
    } else {
        file.path(root, "batches", batch_id)
    }
    receipt <- shift_batch__receipt_read(batch_root, batch_id)
    if (is.null(receipt)) {
        cli::cli_abort("No readable batch receipt for {.val {batch_id}} in {.path {root}}.")
    }
    children <- lapply(receipt$children, function(child) {
        if (!is.na(store__chr1(child$run_id))) {
            return(shift_run_get(child$run_id, store = child$store_path))
        }
        if (!is.null(child$plan)) {
            return(shift__plan_from_spec(child$plan, store = child$store_path))
        }
        cli::cli_abort(c(
            "This older dry-run batch did not persist its child plans.",
            "i" = "Create the batch again from its original configuration."
        ))
    })
    names(children) <- vapply(receipt$children, `[[`, character(1L), "child_key")
    if (!length(children)) {
        cli::cli_abort("The batch receipt contains no children.")
    }
    # Earlier receipts store scientific intent in the authoritative child run.
    # Recover it there instead of requiring a new remote discovery request.
    first <- children[[1L]]
    spec <- if (S7::S7_inherits(first, ShiftPlan)) {
        shift__plan_spec(first)
    } else {
        jsonlite::fromJSON(first@meta$run$spec_json[[1L]], simplifyVector = TRUE)
    }
    manifest <- data.table::as.data.table(data.table::copy(receipt$manifest))
    shift_stage_new(ShiftBatch, "batch", store_path = batch_root,
        ids = list(batch_id = batch_id,
            child_ids = lapply(children, function(child) child@ids)),
        meta = list(
            children = children,
            manifest = manifest,
            periods = shift_coalesce(receipt$periods,
                shift__periods_from_input(spec$periods)),
            climate = shift__climate_from_spec(shift_coalesce(
                receipt$climate, spec$climate)),
            discovery = receipt$discovery,
            selected_models = receipt$discovery$identities,
            output_dir = receipt$output_dir,
            dry_run = all(vapply(children, function(child) {
                S7::S7_inherits(child, ShiftPlan)
            }, logical(1L)))
        ),
        diagnostics = shift_batch__diagnostics(children, manifest, refresh = FALSE)
    )
}

# Restore one child without querying ESGF. Completed children must still own
# all required artifacts; otherwise ordinary planning repairs the missing
# output instead of trusting a stale terminal status.
shift_batch__restore_child <- function(reference) {
    run_id <- store__chr1(reference$run_id)
    store_path <- store__chr1(reference$store_path)
    if (is.na(run_id) || !nzchar(run_id) || is.na(store_path) ||
        !nzchar(store_path)) {
        return(NULL)
    }

    # A completed receipt is only a hint that makes one authoritative store
    # connection safe to attempt. The run status is still read from DuckDB and
    # the same connection verifies every required persisted and exported file.
    if (identical(store__chr1(reference$status), "completed")) {
        child_store <- tryCatch(
            shift_store(store_path, create = FALSE),
            error = identity
        )
        if (!inherits(child_store, "error")) {
            on.exit(try(child_store$close(), silent = TRUE), add = TRUE)
            run <- tryCatch(
                shift_run_get(run_id, store = child_store),
                error = function(error) NULL
            )
            if (is.null(run)) {
                return(NULL)
            }
            if (identical(shift_status(run, refresh = FALSE), "completed") &&
                !shift__run_artifacts_complete(child_store, run_id)) {
                return(NULL)
            }
            return(run)
        }
    }

    # Non-terminal receipts and stores locked by a background worker retain the
    # existing path-based lookup, including its live-snapshot coordination.
    run <- tryCatch(
        shift_run_get(run_id, store = store_path),
        error = function(error) NULL
    )
    if (is.null(run)) {
        return(NULL)
    }
    if (identical(shift_status(run, refresh = FALSE), "completed")) {
        child_store <- shift_store(run)
        on.exit(try(child_store$close(), silent = TRUE), add = TRUE)
        if (!shift__run_artifacts_complete(child_store, run_id)) {
            return(NULL)
        }
    }
    run
}

# Restore every child independently so one invalid receipt entry turns the
# complete batch into a cache miss without weakening other child checks.
shift_batch__restore <- function(
    receipt,
    batch_root,
    climate,
    periods,
    output_dir
) {
    if (!identical(
        normalizePath(receipt$output_dir, winslash = "/", mustWork = FALSE),
        output_dir
    ) || !length(receipt$children)) {
        return(NULL)
    }
    children <- lapply(receipt$children, shift_batch__restore_child)
    if (any(vapply(children, is.null, logical(1L)))) {
        return(NULL)
    }
    names(children) <- vapply(
        receipt$children,
        `[[`,
        character(1L),
        "child_key"
    )
    manifest <- data.table::as.data.table(data.table::copy(receipt$manifest))
    shift_stage_new(
        ShiftBatch,
        "batch",
        store_path = batch_root,
        ids = list(
            batch_id = receipt$batch_id,
            child_ids = lapply(children, function(child) child@ids)
        ),
        meta = list(
            children = children,
            manifest = manifest,
            periods = periods,
            climate = climate,
            discovery = receipt$discovery,
            selected_models = data.table::as.data.table(
                data.table::copy(receipt$discovery$identities)
            ),
            output_dir = output_dir,
            dry_run = FALSE
        ),
        diagnostics = shift_batch__diagnostics(
            children,
            manifest,
            refresh = FALSE
        )
    )
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
# model/member/grid identities whose files cover the requested periods.
shift_batch__available_alternative <- function(
    climate,
    transform,
    variables,
    periods,
    reference,
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
    coverage <- getOption(
        "epwshiftr.cmip6.period_coverage",
        shift__cmip6_period_coverage
    )
    if (!is.function(coverage)) {
        cli::cli_abort("Configured CMIP6 period-coverage adapter must be a function.")
    }
    include_historical <- S7::S7_inherits(reference, ShiftReferenceSpec) &&
        identical(reference@mode, "historical")
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
            current <- tryCatch(
                coverage(
                    candidates = current,
                    climate = climate,
                    transform = transform,
                    variables = variables,
                    frequency = frequency,
                    periods = periods,
                    reference = reference,
                    node = node,
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
        }
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
    periods,
    references,
    store,
    ui
) {
    if (is.null(references)) {
        references <- stats::setNames(
            rep(list(list(reference = NULL)), length(transforms)),
            names(transforms)
        )
    }
    by_transform <- lapply(names(transforms), function(transform_key) {
        transform <- transforms[[transform_key]]
        reference <- references[[transform_key]]$reference
        requirement <- shift_batch__future_requirement(transform)
        alternatives <- lapply(
            seq_along(requirement@variable_sets),
            function(index) {
                variables <- as.character(requirement@variable_sets[[index]])
                result <- shift_batch__available_alternative(
                    climate,
                    transform,
                    variables,
                    periods = periods,
                    reference = reference,
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
        if (!"source_file_count" %in% names(candidates)) {
            data.table::set(
                candidates,
                j = "source_file_count",
                value = rep(NA_real_, nrow(candidates))
            )
        } else {
            data.table::set(
                candidates,
                j = "source_file_count",
                value = as.numeric(candidates[["source_file_count"]])
            )
        }
        data.table::setorderv(
            candidates,
            c("alternative", "source_id", "variant_label", "grid_label")
        )
        candidates[!duplicated(identity)]
    })
    names(by_transform) <- names(transforms)
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
    costs <- data.table::rbindlist(lapply(by_transform, function(candidates) {
        candidates[identity %in% common, {
            values <- .SD[["source_file_count"]]
            list(
                source_file_count = if (all(is.finite(values))) {
                    min(values)
                } else {
                    Inf
                }
            )
        }, by = "identity", .SDcols = "source_file_count"]
    }), use.names = TRUE, fill = TRUE)
    costs <- costs[, {
        values <- .SD[["source_file_count"]]
        list(
            source_file_count = if (all(is.finite(values))) {
                sum(values)
            } else {
                Inf
            }
        )
    }, by = "identity", .SDcols = "source_file_count"]
    data.table::set(
        identities,
        j = "source_file_count",
        value = costs$source_file_count[match(identities$identity, costs$identity)]
    )
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
            # Prefer complete candidates with fewer physical source files so a
            # numeric model request avoids needlessly fragmented archives.
            data.table::setorderv(
                identities,
                c(
                    "source_file_count",
                    "source_id",
                    "variant_label",
                    "grid_label"
                )
            )
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
    path <- path.expand(path)
    # Resolve an existing parent before the new store is created so macOS
    # aliases such as /var and /private/var cannot change persisted identities
    # between the first and second invocation.
    parent <- normalizePath(
        dirname(path),
        winslash = "/",
        mustWork = FALSE
    )
    file.path(parent, basename(path))
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
    call_started <- Sys.time()
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
    reuse_persisted <- isTRUE(control@resume) &&
        !isTRUE(control@overwrite) && !isTRUE(control@refresh)
    receipt <- if (isTRUE(reuse_persisted)) {
        shift_batch__receipt_read(batch_root, batch_id)
    } else {
        NULL
    }
    if (!is.null(receipt) && !isTRUE(dry_run)) {
        restored <- shift_batch__restore(
            receipt,
            batch_root = batch_root,
            climate = climate,
            periods = periods,
            output_dir = output_root
        )
        if (!is.null(restored)) {
            statuses <- vapply(restored@meta$children, function(child) {
                shift_status(child, refresh = FALSE)
            }, character(1L))
            if (all(statuses %in% c(
                "completed", "queued", "running", "stopping", "waiting"
            ))) {
                restored@meta$execution <- data.table::data.table(
                    child_key = names(restored@meta$children),
                    action = "reused",
                    elapsed_seconds = NA_real_
                )
                restored@meta$call_elapsed_seconds <- as.numeric(difftime(
                    Sys.time(), call_started, units = "secs"))
                shift_batch__report(restored, ui)
                return(restored)
            }
            return(shift_batch__resume(
                restored,
                background = background,
                ui = ui
            ))
        }
    }
    discovery <- if (!is.null(receipt)) {
        receipt$discovery
    } else {
        shift_batch__discover_models(
            climate,
            transforms,
            periods = periods,
            references = references,
            store = file.path(batch_root, "discovery"),
            ui = ui
        )
    }
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
                # Build the complete matrix before starting any child. This
                # makes direct execution use the same isolated batch runner as
                # a dry-run plan passed later to shift_run().
                dry_run = TRUE,
                background = FALSE
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
    batch <- shift_stage_new(
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
            discovery = discovery,
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
    shift_batch__receipt_write(batch)
    if (isTRUE(dry_run)) {
        return(batch)
    }
    shift_batch__run(batch, background = background, ui = ui)
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

# Start every child of a dry-run batch through the ordinary ShiftPlan runner.
# Mixed plan/run batches remain the responsibility of shift_resume().
shift_batch__run <- function(x, background = FALSE, ui = shift_ui()) {
    checkmate::assert_flag(background)
    if (!S7::S7_inherits(ui, ShiftUiOptions)) {
        cli::cli_abort("`ui` must be created by {.fn shift_ui}.")
    }
    plans <- vapply(x@meta$children, function(child) {
        S7::S7_inherits(child, ShiftPlan)
    }, logical(1L))
    if (!all(plans)) {
        cli::cli_abort(c(
            "{.fn shift_run} can start only a dry-run {.cls ShiftBatch} whose children are all planned.",
            "i" = "Use {.fn shift_resume} for a batch containing existing runs."
        ))
    }
    x@meta$dry_run <- FALSE
    shift_batch__resume(x, background = background, ui = ui)
}

# Recover the durable failed-run handle emitted by an ordinary child workflow.
# Expected operational failures remain isolated; programming and validation
# errors without a registered run continue to abort the batch immediately.
shift_batch__run_child <- function(expr) {
    tryCatch(
        force(expr),
        epwshiftr_shift_error = function(error) {
            if (is.null(error$run_id) || is.null(error$store)) {
                stop(error)
            }
            shift_run_get(error$run_id, store = error$store)
        }
    )
}

# Resume plans and interrupted runs independently, leaving active and completed
# child runs untouched.
shift_batch__resume <- function(x, background = FALSE, ui = shift_ui()) {
    call_started <- Sys.time()
    execution <- list()
    # Update the shared matrix after each child so subsequent foreground frames
    # show batch progress while the ordinary child reporter owns the terminal.
    for (index in seq_along(x@meta$children)) {
        child <- x@meta$children[[index]]
        started <- Sys.time()
        statuses <- vapply(x@meta$children, function(value) {
            shift_status(value, refresh = FALSE)
        }, character(1L))
        child_ui <- ui
        child_ui@batch_context <- list(id = x@ids$batch_id,
            current = index, total = length(x@meta$children),
            completed = sum(statuses == "completed"),
            failed = sum(statuses %in% c("failed", "blocked")))
        status <- shift_status(child, refresh = TRUE)
        if (identical(status, "completed")) {
            # A completed database row alone does not prove its exported files
            # still exist. Reuse the same artifact check as receipt restoration.
            restored <- shift_batch__restore_child(list(
                run_id = child@ids$run_id, store_path = child@store_path,
                status = status))
            if (is.null(restored)) {
                spec <- jsonlite::fromJSON(child@meta$run$spec_json[[1L]],
                    simplifyVector = TRUE)
                child <- shift__plan_from_spec(spec, store = child@store_path)
                status <- "planned"
            }
        }
        action <- "reused"
        if (S7::S7_inherits(child, ShiftPlan)) {
            action <- "started"
            child <- shift_batch__run_child(
                shift_run(child, background = background, ui = child_ui)
            )
        } else if (!status %in% c("completed", "queued", "running", "stopping",
            "waiting")) {
            action <- "resumed"
            child <- shift_batch__run_child(
                shift_resume(child, background = background, ui = child_ui)
            )
        }
        x@meta$children[[index]] <- child
        execution[[index]] <- data.table::data.table(
            child_key = names(x@meta$children)[[index]], action = action,
            elapsed_seconds = as.numeric(difftime(
                Sys.time(), started, units = "secs"))
        )
        # Persist every completed launch so an interruption retains the latest
        # child run IDs rather than only the original dry-run plans.
        shift_batch__receipt_write(x)
    }
    x@meta$execution <- data.table::rbindlist(execution)
    x@meta$call_elapsed_seconds <- as.numeric(difftime(
        Sys.time(), call_started, units = "secs"))
    x <- shift_batch__refresh(x)
    shift_batch__receipt_write(x)
    shift_batch__report(x, ui)
    x
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
