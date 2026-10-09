#' @include shift-stage.R
NULL

# Serialize an explicit workflow reference with the role assigned by its
# execution argument. ShiftClimate stages do not otherwise carry enough
# provenance to distinguish model output from observations.
# shift_persist__reference_spec_value {{{
shift_persist__reference_spec_value <- function(reference, role) {
    if (is.null(reference)) {
        return(NULL)
    }
    checkmate::assert_choice(role, SHIFT_REFERENCE_ROLES)
    if (S7::S7_inherits(reference, ShiftReanalysisSpec)) {
        if (!identical(role, "observed_reference")) {
            cli::cli_abort(
                "A reanalysis source cannot be persisted as {.val {role}}."
            )
        }
        return(reanalysis__spec_value(reference))
    }
    if (S7::S7_inherits(reference, ShiftClimate)) {
        return(list(
            mode = "plan",
            role = role,
            plan_id = shift_ids(reference)$plan_id,
            periods = split(
                as.integer(reference@meta$periods$year),
                reference@meta$periods$period
            )
        ))
    }
    if (!S7::S7_inherits(reference, ShiftReferenceSpec)) {
        cli::cli_abort("Cannot persist an unsupported shift reference object.")
    }
    if (!identical(reference@role, role)) {
        cli::cli_abort(
            "Cannot persist reference role {.val {reference@role}} as {.val {role}}."
        )
    }
    list(
        mode = reference@mode,
        role = reference@role,
        plan_id = reference@plan_id,
        periods = split(
            as.integer(reference@periods$year),
            reference@periods$period
        ),
        experiment = reference@experiment,
        activity = reference@activity,
        match = reference@match,
        filters = reference@filters,
        options = reference@options,
        collect = reference@collect,
        extract = reference@extract
    )
}
# }}}

# Rebuild only the reference mode that was serialized; a missing value remains
# missing and is never converted into a historical reference.
# shift_persist__reference_from_spec {{{
shift_persist__reference_from_spec <- function(spec) {
    if (is.null(spec)) {
        return(NULL)
    }
    if (is.null(spec$role)) {
        cli::cli_abort(
            "Persisted reference is missing its semantic input role."
        )
    }
    if (identical(spec$mode, "reanalysis")) {
        if (!identical(as.character(spec$role), "observed_reference")) {
            cli::cli_abort(
                "Persisted reanalysis input has an invalid semantic role."
            )
        }
        return(reanalysis__from_spec(spec))
    }
    periods <- shift_spec__periods_from_input(
        spec$periods,
        arg = "reference$periods"
    )
    if (identical(spec$mode, "plan")) {
        return(shift_reference_from_plan(
            as.character(spec$plan_id),
            periods,
            role = as.character(spec$role)
        ))
    }
    if (identical(spec$mode, "historical")) {
        if (
            !identical(
                as.character(spec$role),
                "model_historical"
            )
        ) {
            cli::cli_abort(
                "Persisted automatic historical reference has an invalid semantic role."
            )
        }
        return(shift_reference_historical(
            periods = periods,
            experiment = as.character(spec$experiment),
            activity = as.character(spec$activity),
            match = as.character(spec$match),
            filters = shift_stage__coalesce(spec$filters, list()),
            options = shift_stage__coalesce(spec$options, list()),
            collect = shift_stage__coalesce(spec$collect, list()),
            extract = shift_stage__coalesce(spec$extract, list())
        ))
    }
    cli::cli_abort("Unsupported persisted reference mode: {.val {spec$mode}}.")
}
# }}}

# Serialize the complete CMIP6 identity as the sole scientific source of truth;
# the lower-level request is derived from this value when a run is resumed.
# shift_persist__climate_spec_value {{{
shift_persist__climate_spec_value <- function(climate) {
    if (is.null(climate)) {
        return(NULL)
    }
    spec <- list(
        provider = "cmip6",
        model = climate@model,
        n_models = if (is.null(climate@model)) climate@n_models else NULL,
        scenarios = climate@scenarios,
        member = climate@member,
        grid = climate@grid,
        # Preserve variable names across JSON round-trips for mixed-frequency
        # climate specifications.
        frequency = if (!is.null(names(climate@frequency))) {
            as.list(climate@frequency)
        } else {
            climate@frequency
        },
        # JSON objects preserve variable names; named atomic vectors do not
        # when `auto_unbox = TRUE`, so overrides are persisted as a named list.
        table = if (!is.null(names(climate@table))) {
            as.list(climate@table)
        } else {
            climate@table
        },
        activity = climate@activity,
        index_nodes = climate@index_nodes,
        data_node = climate@data_node,
        filters = climate@filters
    )
    # Omit the historical default so existing common-pool task hashes and
    # receipts remain valid. Only a different selection policy changes intent.
    if (!climate@common) {
        spec$common <- climate@common
    }
    spec
}
# }}}

# Rebuild only explicitly supported climate specifications from persisted task
# intent instead of inferring provider or model fields from request artifacts.
# shift_persist__climate_from_spec {{{
shift_persist__climate_from_spec <- function(spec) {
    if (is.null(spec)) {
        return(NULL)
    }
    if (!identical(as.character(spec$provider), "cmip6")) {
        cli::cli_abort(
            "Unsupported persisted climate provider: {.val {spec$provider}}."
        )
    }
    # Persisted specifications retain a private count field so plans created by
    # earlier development builds can be resumed through the public `model`
    # argument without reintroducing `n_models` into the user API.
    arguments <- spec[setdiff(names(spec), c("provider", "n_models"))]
    model <- if (!is.null(spec$model)) {
        as.character(unlist(spec$model, use.names = FALSE))
    } else if (!is.null(spec$n_models)) {
        as.integer(unlist(spec$n_models, use.names = FALSE))
    } else {
        NULL
    }
    # Single-bracket assignment preserves an explicit NULL list element;
    # `$<- NULL` would delete it and accidentally restore the default count.
    arguments["model"] <- list(model)
    do.call(shift_cmip6, arguments)
}
# }}}

# Preserve variable names on request frequency mappings because jsonlite
# serializes named atomic vectors as arrays when automatic unboxing is enabled.
# shift_persist__request_spec_value {{{
shift_persist__request_spec_value <- function(request) {
    if (is.null(request)) {
        return(NULL)
    }
    out <- request@meta
    if (!is.null(names(out$frequency))) {
        out$frequency <- as.list(out$frequency)
    }
    out
}
# }}}

# Restore request frequencies without allowing character coercion to discard
# names from a JSON object that represents a variable-specific mapping.
# shift_persist__request_frequency_from_spec {{{
shift_persist__request_frequency_from_spec <- function(value) {
    if (is.null(value)) {
        return(NULL)
    }
    value <- unlist(value, use.names = TRUE)
    value_names <- names(value)
    value <- as.character(value)
    names(value) <- value_names
    value
}
# }}}

# Convert a plan into a canonical, JSON-safe task specification. Identical
# resumable intent resolves to the original run ID, while explicit refresh or
# overwrite requests remain distinct executions.
# shift_persist__plan_spec {{{
shift_persist__plan_spec <- function(x) {
    meta <- x@meta
    request <- meta$request@meta
    transform <- meta$transform
    control <- meta$control
    climate <- meta$climate
    epw_path <- if (
        is.character(meta$site@epw) && length(meta$site@epw) == 1L
    ) {
        normalizePath(
            path.expand(meta$site@epw),
            winslash = "/",
            mustWork = FALSE
        )
    } else {
        shift_stage__coalesce(meta$epw_identity$path, NULL)
    }
    spec <- list(
        version = 2L,
        task = "future_epw",
        request = if (is.null(climate)) {
            shift_persist__request_spec_value(meta$request)
        } else {
            NULL
        },
        site = list(
            id = meta$site@id,
            lon = meta$site@lon,
            lat = meta$site@lat,
            label = meta$site@label,
            epw = epw_path,
            metadata = meta$site@metadata,
            identity = meta$epw_identity
        ),
        periods = split(as.integer(meta$periods$year), meta$periods$period),
        transform = transform__spec_value(transform),
        reference = shift_persist__reference_spec_value(
            meta$reference,
            role = "model_historical"
        ),
        observed_reference = shift_persist__reference_spec_value(
            meta$observed_reference,
            role = "observed_reference"
        ),
        climate = shift_persist__climate_spec_value(climate),
        control = list(
            strict = control@strict,
            allow_partial = control@allow_partial,
            download = control@download,
            resume = control@resume,
            overwrite = control@overwrite,
            refresh = control@refresh,
            extraction_method = control@extraction_method,
            output_layout = control@output_layout
        ),
        store = x@store_path,
        stages = list(
            collect = meta$collect,
            download = meta$download,
            extract = meta$extract,
            morph = meta$morph,
            epw = meta$epw
        )
    )
    # Only resolved batch children carry shared inputs; ordinary task identity
    # stays independent of batch scheduling.
    spec$stages$shared_inputs <- meta$shared_inputs
    # Candidate explanations are execution evidence, not scientific intent.
    # Child registration persists them separately before a worker is launched.
    if (!is.null(spec$stages$shared_inputs)) {
        spec$stages$shared_inputs$selection_records <- NULL
    }
    spec
}
# }}}

# Encode workflow specs with stable key order inherited from the constructor
# lists so identical scientific intent produces the same hash.
# shift_persist__spec_json {{{
shift_persist__spec_json <- function(spec) {
    as.character(jsonlite::toJSON(
        spec,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        digits = 15,
        POSIXt = "ISO8601"
    ))
}
# }}}

# Convert one site into the JSON-safe identity required by later extraction and
# morph steps. EPW objects are persisted through their backing path only.
# shift_persist__site_ref {{{
shift_persist__site_ref <- function(site) {
    if (is.null(site)) {
        return(NULL)
    }
    if (!S7::S7_inherits(site, ShiftSite)) {
        cli::cli_abort("Cannot persist a non-ShiftSite task target.")
    }
    epw <- site@epw
    epw_path <- if (shift_spec__is_epw_path(epw)) {
        normalizePath(path.expand(epw), winslash = "/", mustWork = FALSE)
    } else if (shift_spec__is_epw_object(epw)) {
        epw_file_coerce(epw)$path()
    } else {
        NULL
    }
    list(
        id = site@id,
        lon = site@lon,
        lat = site@lat,
        label = site@label,
        epw = epw_path,
        metadata = site@metadata
    )
}
# }}}

# Rebuild a persisted site without inferring or replacing a missing EPW path.
# shift_persist__site_from_ref {{{
shift_persist__site_from_ref <- function(ref) {
    if (is.null(ref)) {
        return(NULL)
    }
    shift_site(
        id = as.character(ref$id),
        lon = as.numeric(ref$lon),
        lat = as.numeric(ref$lat),
        label = if (is.null(ref$label)) NULL else as.character(ref$label),
        epw = if (is.null(ref$epw)) NULL else as.character(ref$epw),
        metadata = shift_stage__coalesce(ref$metadata, list())
    )
}
# }}}

# Reduce a stage to stable store IDs plus the minimum scientific metadata
# required to continue the normal collect-to-export chain in another session.
# shift_persist__stage_ref {{{
shift_persist__stage_ref <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    shift_stage__assert_stage(x)
    base <- list(
        version = 1L,
        class = class(x)[[1L]],
        stage = x@stage,
        store_path = x@store_path,
        ids = x@ids
    )
    meta <- if (S7::S7_inherits(x, ShiftRequest)) {
        x@meta
    } else if (S7::S7_inherits(x, ShiftDatasets)) {
        list(
            request = shift_persist__stage_ref(x@meta$request),
            dataset_count = x@meta$dataset_count,
            result_path = x@meta$result_path
        )
    } else if (S7::S7_inherits(x, ShiftFiles)) {
        list(
            request = shift_persist__stage_ref(x@meta$request),
            dataset_count = x@meta$dataset_count,
            file_count = x@meta$file_count,
            variables = x@meta$variables,
            fields = x@meta$fields
        )
    } else if (S7::S7_inherits(x, ShiftDownload)) {
        list(files = shift_persist__stage_ref(x@meta$files))
    } else if (S7::S7_inherits(x, ShiftClimate)) {
        upstream <- shift_stage__coalesce(x@meta$download, x@meta$files)
        list(
            upstream = shift_persist__stage_ref(upstream),
            site = shift_persist__site_ref(x@meta$site),
            periods = split(
                as.integer(x@meta$periods$year),
                x@meta$periods$period
            ),
            variables = x@meta$variables
        )
    } else if (S7::S7_inherits(x, ShiftMorphed)) {
        baseline <- x@meta$baseline
        list(
            climate = shift_persist__stage_ref(x@meta$climate),
            baseline = if (S7::S7_inherits(baseline, ShiftSite)) {
                list(type = "site", value = shift_persist__site_ref(baseline))
            } else if (is.character(baseline) && length(baseline) == 1L) {
                list(
                    type = "path",
                    value = normalizePath(
                        path.expand(baseline),
                        winslash = "/",
                        mustWork = FALSE
                    )
                )
            } else {
                NULL
            },
            transform = transform__spec_value(x@meta$transform),
            reference = shift_persist__reference_spec_value(
                shift_stage__coalesce(
                    x@meta$reference_spec,
                    x@meta$reference
                ),
                role = "model_historical"
            ),
            observed_reference = shift_persist__reference_spec_value(
                shift_stage__coalesce(
                    x@meta$observed_reference_spec,
                    x@meta$observed_reference
                ),
                role = "observed_reference"
            ),
            reference_plan_id = x@meta$reference_plan_id,
            reference_periods = if (is.null(x@meta$reference_periods)) {
                NULL
            } else {
                split(
                    as.integer(x@meta$reference_periods$year),
                    x@meta$reference_periods$period
                )
            },
            observed_plan_id = x@meta$observed_plan_id,
            observed_periods = if (is.null(x@meta$observed_periods)) {
                NULL
            } else {
                split(
                    as.integer(x@meta$observed_periods$year),
                    x@meta$observed_periods$period
                )
            }
        )
    } else if (S7::S7_inherits(x, ShiftOutputs)) {
        outputs <- data.table::as.data.table(shift_stage__coalesce(
            x@meta$outputs,
            data.table::data.table()
        ))
        exports <- if (all(c("output_id", "export_path") %in% names(outputs))) {
            list(
                output_id = outputs$output_id,
                export_path = outputs$export_path
            )
        } else {
            NULL
        }
        list(
            morphed = shift_persist__stage_ref(x@meta$morphed),
            format = x@meta$format,
            paths = x@meta$paths,
            export_dir = x@meta$export_dir,
            exports = exports
        )
    } else {
        list()
    }
    base$meta <- meta
    base
}
# }}}

# Reconstruct a lightweight but actionable stage from persisted IDs. Large
# datasets and workflow objects are queried from the store instead of being
# embedded in JSON step rows.
# shift_persist__stage_from_ref {{{
shift_persist__stage_from_ref <- function(ref) {
    if (is.null(ref)) {
        return(NULL)
    }
    stage <- as.character(ref$stage)
    store_path <- if (is.null(ref$store_path)) {
        NULL
    } else {
        as.character(ref$store_path)
    }
    ids <- lapply(shift_stage__coalesce(ref$ids, list()), function(value) {
        unlist(value, use.names = FALSE)
    })
    meta <- shift_stage__coalesce(ref$meta, list())
    if (identical(stage, "request")) {
        return(do.call(shift_request, meta))
    }
    if (identical(stage, "datasets")) {
        request <- shift_persist__stage_from_ref(meta$request)
        return(shift_stage__new(
            ShiftDatasets,
            "datasets",
            store_path = store_path,
            ids = ids,
            meta = list(
                request = request,
                dataset_count = as.integer(meta$dataset_count),
                result_path = as.character(meta$result_path)
            )
        ))
    }
    if (identical(stage, "files")) {
        request <- shift_persist__stage_from_ref(meta$request)
        return(shift_stage__new(
            ShiftFiles,
            "files",
            store_path = store_path,
            ids = ids,
            meta = list(
                request = request,
                dataset_count = as.integer(meta$dataset_count),
                file_count = as.integer(meta$file_count),
                variables = as.character(unlist(
                    meta$variables,
                    use.names = FALSE
                )),
                fields = as.character(unlist(meta$fields, use.names = FALSE))
            )
        ))
    }
    if (identical(stage, "download")) {
        files <- shift_persist__stage_from_ref(meta$files)
        return(shift_stage__new(
            ShiftDownload,
            "download",
            store_path = store_path,
            ids = ids,
            meta = list(files = files, session = NULL)
        ))
    }
    if (identical(stage, "climate")) {
        upstream <- shift_persist__stage_from_ref(meta$upstream)
        site <- shift_persist__site_from_ref(meta$site)
        periods <- shift_spec__periods_from_input(meta$periods)
        upstream_name <- if (S7::S7_inherits(upstream, ShiftDownload)) {
            "download"
        } else {
            "files"
        }
        store <- shift_store(store_path, create = FALSE)
        on.exit(try(store$close(), silent = TRUE), add = TRUE)
        # Coverage is a computed store view rather than a persisted table. Use
        # the public store boundary so stage restoration stays aligned with the
        # extraction schema.
        coverage <- store$coverage(plan_id = ids$plan_id)
        return(shift_stage__new(
            ShiftClimate,
            "climate",
            store_path = store_path,
            ids = ids,
            meta = c(
                stats::setNames(list(upstream), upstream_name),
                list(
                    site = site,
                    periods = periods,
                    variables = as.character(unlist(
                        meta$variables,
                        use.names = FALSE
                    )),
                    coverage = coverage
                )
            )
        ))
    }
    if (identical(stage, "morphed")) {
        climate <- shift_persist__stage_from_ref(meta$climate)
        transform <- transform__from_spec(meta$transform)
        baseline <- if (is.null(meta$baseline)) {
            shift_target(climate)
        } else if (identical(as.character(meta$baseline$type), "site")) {
            shift_persist__site_from_ref(meta$baseline$value)
        } else {
            as.character(meta$baseline$value)
        }
        return(shift_stage__new(
            ShiftMorphed,
            "morphed",
            store_path = store_path,
            ids = ids,
            meta = list(
                climate = climate,
                baseline = baseline,
                transform = transform,
                recipe = transform__recipe(transform),
                reference = shift_persist__reference_from_spec(meta$reference),
                observed_reference = shift_persist__reference_from_spec(
                    meta$observed_reference
                ),
                reference_plan_id = unlist(
                    meta$reference_plan_id,
                    use.names = FALSE
                ),
                reference_periods = if (is.null(meta$reference_periods)) {
                    NULL
                } else {
                    shift_spec__periods_from_input(meta$reference_periods)
                },
                observed_plan_id = unlist(
                    meta$observed_plan_id,
                    use.names = FALSE
                ),
                observed_periods = if (is.null(meta$observed_periods)) {
                    NULL
                } else {
                    shift_spec__periods_from_input(meta$observed_periods)
                }
            )
        ))
    }
    if (identical(stage, "outputs")) {
        morphed <- shift_persist__stage_from_ref(meta$morphed)
        store <- shift_store(store_path, create = FALSE)
        on.exit(try(store$close(), silent = TRUE), add = TRUE)
        outputs <- shift_inspect__epw_output_rows(store, ids$morph_id)
        if (!is.null(meta$exports)) {
            exports <- data.table::data.table(
                output_id = as.character(unlist(
                    meta$exports$output_id,
                    use.names = FALSE
                )),
                export_path = as.character(unlist(
                    meta$exports$export_path,
                    use.names = FALSE
                ))
            )
            outputs <- merge(
                outputs,
                exports,
                by = "output_id",
                all.x = TRUE,
                sort = FALSE
            )
        }
        return(shift_stage__new(
            ShiftOutputs,
            "outputs",
            store_path = store_path,
            ids = ids,
            meta = list(
                morphed = morphed,
                format = as.character(shift_stage__coalesce(
                    meta$format,
                    "epw"
                )),
                outputs = outputs,
                paths = as.character(unlist(meta$paths, use.names = FALSE)),
                export_dir = if (is.null(meta$export_dir)) {
                    NULL
                } else {
                    as.character(meta$export_dir)
                }
            )
        ))
    }
    cli::cli_abort("Unsupported persisted shift stage: {.val {stage}}.")
}
# }}}

# Reconstruct a persisted plan for cross-session resume. A baseline EPW object
# without a path cannot be recovered and therefore fails with a targeted error.
# shift_persist__plan_from_spec {{{
shift_persist__plan_from_spec <- function(spec, store = NULL) {
    version <- as.integer(shift_stage__coalesce(spec$version, 1L))
    if (!identical(version, 2L)) {
        cli::cli_abort(c(
            "Persisted future-weather plan uses unsupported schema version {.val {version}}.",
            "i" = "Create a new plan with the weather transform API."
        ))
    }
    site_spec <- spec$site
    if (is.null(site_spec$epw) || !nzchar(as.character(site_spec$epw))) {
        cli::cli_abort(
            "This run cannot be resumed across sessions because its baseline EPW was not persisted as a file path."
        )
    }
    site <- shift_site(
        id = as.character(site_spec$id),
        lon = as.numeric(site_spec$lon),
        lat = as.numeric(site_spec$lat),
        label = if (is.null(site_spec$label)) {
            NULL
        } else {
            as.character(site_spec$label)
        },
        epw = as.character(site_spec$epw),
        metadata = shift_stage__coalesce(site_spec$metadata, list())
    )
    transform <- transform__from_spec(spec$transform)
    reference <- shift_persist__reference_from_spec(spec$reference)
    observed_reference <- shift_persist__reference_from_spec(
        spec$observed_reference
    )
    control <- do.call(shift_control, spec$control)
    climate <- shift_persist__climate_from_spec(spec$climate)
    if (is.null(climate)) {
        request_spec <- spec$request
        request <- do.call(
            shift_request,
            list(
                provider = as.character(request_spec$provider),
                project = if (is.null(request_spec$project)) {
                    NULL
                } else {
                    as.character(request_spec$project)
                },
                source = if (is.null(request_spec$source)) {
                    NULL
                } else {
                    as.character(request_spec$source)
                },
                experiment = if (is.null(request_spec$experiment)) {
                    NULL
                } else {
                    as.character(request_spec$experiment)
                },
                variant = if (is.null(request_spec$variant)) {
                    NULL
                } else {
                    as.character(request_spec$variant)
                },
                variables = if (is.null(request_spec$variables)) {
                    NULL
                } else {
                    as.character(request_spec$variables)
                },
                frequency = shift_persist__request_frequency_from_spec(
                    request_spec$frequency
                ),
                time = request_spec$time,
                filters = shift_stage__coalesce(request_spec$filters, list()),
                options = shift_stage__coalesce(request_spec$options, list())
            )
        )
    } else {
        # The persisted climate spec is authoritative; regenerate request fields
        # so model/scenario/member constraints cannot diverge during resume.
        request <- shift_spec__request_from_cmip6(
            climate,
            shift_spec__periods_from_input(spec$periods),
            transform
        )
    }
    stage <- shift_stage__coalesce(spec$stages, list())
    plan <- shift_plan(
        request = request,
        site = site,
        periods = spec$periods,
        store = shift_stage__coalesce(store, spec$store),
        transform = transform,
        reference = reference,
        observed_reference = observed_reference,
        control = control,
        collect = shift_stage__coalesce(stage$collect, list()),
        download = shift_stage__coalesce(stage$download, list()),
        extract = shift_stage__coalesce(stage$extract, list()),
        morph = shift_stage__coalesce(stage$morph, list()),
        epw = shift_stage__coalesce(stage$epw, list())
    )
    if (!is.null(climate)) {
        plan@meta$climate <- climate
    }
    plan@meta$epw_identity <- site_spec$identity
    plan@meta$shared_inputs <- stage$shared_inputs
    plan
}
# }}}

# vim: fdm=marker :
