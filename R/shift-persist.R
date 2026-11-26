#' @include shift-stage.R
NULL

# Primitive persistence helpers shared across plan and stage envelopes.
# shift_persist__periods_value {{{
shift_persist__periods_value <- function(periods) {
    if (is.null(periods)) {
        return(NULL)
    }
    split(as.integer(periods$year), periods$period)
}
# }}}

# shift_persist__periods_or_null {{{
shift_persist__periods_or_null <- function(x) {
    if (is.null(x)) NULL else shift_spec__periods_from_input(x)
}
# }}}

# shift_persist__chr_or_null {{{
shift_persist__chr_or_null <- function(x) {
    if (is.null(x)) NULL else as.character(x)
}
# }}}

# shift_persist__chr_vec {{{
shift_persist__chr_vec <- function(x) {
    as.character(unlist(x, use.names = FALSE))
}
# }}}

# Preserve variable names across JSON round-trips because jsonlite serializes
# named atomic vectors as arrays when auto_unbox = TRUE.
# shift_persist__named_value {{{
shift_persist__named_value <- function(x) {
    if (!is.null(names(x))) as.list(x) else x
}
# }}}

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
            periods = shift_persist__periods_value(reference@meta$periods)
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
        periods = shift_persist__periods_value(reference@periods),
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
        frequency = shift_persist__named_value(climate@frequency),
        table = shift_persist__named_value(climate@table),
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
        shift_persist__chr_vec(spec$model)
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
    out$frequency <- shift_persist__named_value(out$frequency)
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
        # Earlier completed runs may have omitted a failed mandatory fragment.
        # Invalidate their run receipts while retaining reusable source extracts.
        coverage_version = 2L,
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
        periods = shift_persist__periods_value(meta$periods),
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
        label = shift_persist__chr_or_null(ref$label),
        epw = shift_persist__chr_or_null(ref$epw),
        metadata = shift_stage__coalesce(ref$metadata, list())
    )
}
# }}}

# Encode extraction arguments once for both ordinary stage execution and
# existing-plan receipts. Named NULL fields and their order remain part of the
# saved task specification; only the site and period representations change.
# shift_persist__extract_spec {{{
shift_persist__extract_spec <- function(args) {
    args[c("site", "periods")] <- list(
        shift_persist__site_ref(args$site),
        shift_persist__periods_value(args$periods)
    )
    args
}
# }}}

# Restore saved extraction arguments for the public method. Resume reuses the
# original input and plans while public validation still owns illegal values.
# shift_persist__extract_args {{{
shift_persist__extract_args <- function(spec) {
    list(
        site = shift_persist__site_from_ref(spec$site),
        periods = shift_spec__periods_from_input(spec$periods),
        variables = shift_persist__chr_or_null(spec$variables),
        time = spec$time,
        filters = shift_stage__coalesce(spec$filters, list()),
        method = as.character(spec$method),
        fallback = as.character(spec$fallback),
        overwrite = isTRUE(spec$overwrite),
        resume = TRUE
    )
}
# }}}

# Keep the executable morph arguments separate from their persisted form.
# Session-local baselines stay usable for this call but have no saved path;
# the caller uses that distinction to mark the task as non-resumable.
# shift_persist__morph_spec {{{
shift_persist__morph_spec <- function(args) {
    args[c("baseline", "transform", "reference", "observed_reference")] <- list(
        if (shift_spec__is_epw_path(args$baseline)) args$baseline else NULL,
        transform__spec_value(args$transform),
        shift_persist__reference_spec_value(
            args$reference,
            role = "model_historical"
        ),
        shift_persist__reference_spec_value(
            args$observed_reference,
            role = "observed_reference"
        )
    )
    args
}
# }}}

# Decode the same morph task representation without borrowing the broader
# baseline representation used by completed artifact references.
# shift_persist__morph_args {{{
shift_persist__morph_args <- function(spec) {
    list(
        baseline = shift_persist__chr_or_null(spec$baseline),
        transform = transform__from_spec(spec$transform),
        reference = shift_persist__reference_from_spec(spec$reference),
        observed_reference = shift_persist__reference_from_spec(
            spec$observed_reference
        ),
        strict = isTRUE(spec$strict),
        complete_only = isTRUE(spec$complete_only),
        by = as.character(spec$by),
        overwrite = isTRUE(spec$overwrite),
        resume = TRUE
    )
}
# }}}

# Pack and unpack the common stage persistence envelope shared by every stage
# reference round-trip.
# shift_persist__stage_envelope {{{
shift_persist__stage_envelope <- function(x, meta = list()) {
    list(
        version = 1L,
        class = class(x)[[1L]],
        stage = x@stage,
        store_path = x@store_path,
        ids = x@ids,
        meta = meta
    )
}
# }}}

# shift_persist__stage_unpack {{{
shift_persist__stage_unpack <- function(ref) {
    list(
        stage = as.character(ref$stage),
        store_path = shift_persist__chr_or_null(ref$store_path),
        ids = lapply(shift_stage__coalesce(ref$ids, list()), function(value) {
            unlist(value, use.names = FALSE)
        }),
        meta = shift_stage__coalesce(ref$meta, list())
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
            periods = shift_persist__periods_value(x@meta$periods),
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
            reference_periods = shift_persist__periods_value(
                x@meta$reference_periods
            ),
            observed_plan_id = x@meta$observed_plan_id,
            observed_periods = shift_persist__periods_value(
                x@meta$observed_periods
            )
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
    shift_persist__stage_envelope(x, meta)
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
    env <- shift_persist__stage_unpack(ref)
    meta <- env$meta
    build <- function(class, stage_meta) {
        shift_stage__new(
            class,
            env$stage,
            store_path = env$store_path,
            ids = env$ids,
            meta = stage_meta
        )
    }
    switch(
        env$stage,
        request = {
            # Standalone resume intentionally decodes nested stage JSON without
            # simplification. Restore the request's atomic-vector fields before
            # calling its public validator, retaining NULL and any mapping names.
            for (field in c(
                "source",
                "experiment",
                "variant",
                "variables",
                "frequency",
                "time"
            )) {
                if (is.list(meta[[field]])) {
                    meta[[field]] <- unlist(meta[[field]], recursive = FALSE)
                }
            }
            do.call(shift_request, meta)
        },
        datasets = build(
            ShiftDatasets,
            list(
                request = shift_persist__stage_from_ref(meta$request),
                dataset_count = as.integer(meta$dataset_count),
                result_path = as.character(meta$result_path)
            )
        ),
        files = build(
            ShiftFiles,
            list(
                request = shift_persist__stage_from_ref(meta$request),
                dataset_count = as.integer(meta$dataset_count),
                file_count = as.integer(meta$file_count),
                variables = shift_persist__chr_vec(meta$variables),
                fields = shift_persist__chr_vec(meta$fields)
            )
        ),
        download = build(
            ShiftDownload,
            list(
                files = shift_persist__stage_from_ref(meta$files),
                session = NULL
            )
        ),
        climate = {
            upstream <- shift_persist__stage_from_ref(meta$upstream)
            upstream_name <- if (S7::S7_inherits(upstream, ShiftDownload)) {
                "download"
            } else {
                "files"
            }
            # Coverage is a computed store view rather than a persisted table.
            # Use the public store boundary so stage restoration stays aligned
            # with the extraction schema.
            coverage <- shift_inspect__with_store(
                env$store_path,
                function(store) store$coverage(plan_id = env$ids$plan_id)
            )
            build(
                ShiftClimate,
                c(
                    stats::setNames(list(upstream), upstream_name),
                    list(
                        site = shift_persist__site_from_ref(meta$site),
                        periods = shift_spec__periods_from_input(meta$periods),
                        variables = shift_persist__chr_vec(meta$variables),
                        coverage = coverage
                    )
                )
            )
        },
        morphed = {
            climate <- shift_persist__stage_from_ref(meta$climate)
            transform <- transform__from_spec(meta$transform)
            baseline <- if (is.null(meta$baseline)) {
                shift_target(climate)
            } else if (identical(as.character(meta$baseline$type), "site")) {
                shift_persist__site_from_ref(meta$baseline$value)
            } else {
                as.character(meta$baseline$value)
            }
            build(
                ShiftMorphed,
                list(
                    climate = climate,
                    baseline = baseline,
                    transform = transform,
                    recipe = transform__recipe(transform),
                    reference = shift_persist__reference_from_spec(
                        meta$reference
                    ),
                    observed_reference = shift_persist__reference_from_spec(
                        meta$observed_reference
                    ),
                    reference_plan_id = unlist(
                        meta$reference_plan_id,
                        use.names = FALSE
                    ),
                    reference_periods = shift_persist__periods_or_null(
                        meta$reference_periods
                    ),
                    observed_plan_id = unlist(
                        meta$observed_plan_id,
                        use.names = FALSE
                    ),
                    observed_periods = shift_persist__periods_or_null(
                        meta$observed_periods
                    )
                )
            )
        },
        outputs = {
            outputs <- shift_inspect__with_store(
                env$store_path,
                function(store) {
                    shift_inspect__epw_output_rows(store, env$ids$morph_id)
                }
            )
            if (!is.null(meta$exports)) {
                exports <- data.table::data.table(
                    output_id = shift_persist__chr_vec(meta$exports$output_id),
                    export_path = shift_persist__chr_vec(
                        meta$exports$export_path
                    )
                )
                outputs <- merge(
                    outputs,
                    exports,
                    by = "output_id",
                    all.x = TRUE,
                    sort = FALSE
                )
            }
            build(
                ShiftOutputs,
                list(
                    morphed = shift_persist__stage_from_ref(meta$morphed),
                    format = as.character(shift_stage__coalesce(
                        meta$format,
                        "epw"
                    )),
                    outputs = outputs,
                    paths = shift_persist__chr_vec(meta$paths),
                    export_dir = shift_persist__chr_or_null(meta$export_dir)
                )
            )
        },
        cli::cli_abort("Unsupported persisted shift stage: {.val {env$stage}}.")
    )
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
    site <- shift_persist__site_from_ref(site_spec)
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
                project = shift_persist__chr_or_null(request_spec$project),
                source = shift_persist__chr_or_null(request_spec$source),
                experiment = shift_persist__chr_or_null(
                    request_spec$experiment
                ),
                variant = shift_persist__chr_or_null(request_spec$variant),
                variables = shift_persist__chr_or_null(request_spec$variables),
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
